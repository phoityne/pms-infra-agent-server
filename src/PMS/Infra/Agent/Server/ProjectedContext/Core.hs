{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module PMS.Infra.Agent.Server.ProjectedContext.Core where

import Control.Concurrent.Async (async, cancel)
import Control.Lens ((^.))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ask)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (encode)
import Data.IP (AddrRange, IPv4, isMatchedTo, makeAddrRange)
import Data.Maybe (fromMaybe, isJust)
import Network.Socket (Socket)
import System.IO (hPutStrLn, stderr)
import Text.Read (readMaybe)
import qualified Control.Concurrent.STM as STM
import qualified Control.Exception.Safe as E
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map.Strict as Map
import qualified Data.Text.Encoding as T
import qualified Data.Text as T
import qualified Network.Socket.ByteString as NS
import qualified PMS.Domain.Model.DM.Type as DM
import qualified PMS.Domain.Model.DS.Utility as DU

import PMS.Infra.Agent.Server.CoreModel.Constant
import PMS.Infra.Agent.Server.CoreModel.Type

-- | The boundary between AppStateContext and IO.
-- ProjectedContext functions have the type @AppData -> IO a@.
-- They know nothing about AppStateContext, JsonRpcRequest, or response queues.
type ProjectedContext a = AppData -> IO a

-- | Lift a ProjectedContext action into AppStateContext.
-- This is the single call boundary between ApplicationBase and ProjectedContext.
runProjectedContext :: ProjectedContext a -> AppStateContext a
runProjectedContext pc = do
  appDat <- lift (lift ask)   -- ReaderT AppData layer
  liftIO (pc appDat)

-- | Lift a ProjectedContext action that also needs the domain command queue.
runProjectedContextWithCmdQ
  :: (STM.TQueue DM.AgentServerCommand -> ProjectedContext a)
  -> AppStateContext a
runProjectedContextWithCmdQ pc = do
  appDat <- lift (lift ask)
  domDat <- lift (lift (lift ask))
  let cmdQ = domDat^.DM.agentServerQueueDomainData
  liftIO (pc cmdQ appDat)

-- | Check a remote address string (e.g. "127.0.0.1:12345") against a list of
-- CIDR ranges (sandboxNetworks).
-- Returns Right () if the IP matches any range, Left error message otherwise.
-- An empty CIDR list means deny all connections (default deny-all policy).
-- IPv6 addresses are always rejected; only IPv4 CIDR ranges are supported.
checkRemoteAddr :: [String] -> String -> Either String ()
checkRemoteAddr [] _ =
  Left "connection refused: sandboxNetworks is empty."
checkRemoteAddr cidrs addr
  | isIPv6Addr addr =
      Left "connection refused: IPv6 is not supported."
  | otherwise =
      case parseIPv4 addr of
        Nothing -> Left "connection refused: failed to parse remote address."
        Just ip ->
          case parseCidrs cidrs of
            Left err -> Left err
            Right ranges ->
              if any (isMatchedTo ip) ranges
                then Right ()
                else Left $ "connection refused: " ++ show ip ++ " is outside sandboxNetworks."

-- | Return True if the address string looks like an IPv6 address (starts with '[').
isIPv6Addr :: String -> Bool
isIPv6Addr ('[':_) = True
isIPv6Addr _       = False

-- | Extract the IPv4 host part from a "host:port" string and parse it.
-- Returns Nothing if the host cannot be parsed as an IPv4 address.
parseIPv4 :: String -> Maybe IPv4
parseIPv4 addr =
  let host = reverse . drop 1 . dropWhile (/= ':') . reverse $ addr
  in if null host then Nothing else readMaybe host

-- | Parse a list of CIDR strings into AddrRange IPv4 values.
-- Returns Left with the first unparsable CIDR, or Right with all ranges.
parseCidrs :: [String] -> Either String [AddrRange IPv4]
parseCidrs = mapM parseCidr
  where
    parseCidr :: String -> Either String (AddrRange IPv4)
    parseCidr cidr = case readMaybe cidr of
      Just r  -> Right r
      Nothing ->
        -- Try "a.b.c.d/n" by splitting on '/'
        case break (== '/') cidr of
          (ipStr, '/':lenStr) ->
            case (readMaybe ipStr :: Maybe IPv4, readMaybe lenStr :: Maybe Int) of
              (Just ip, Just len) -> Right (makeAddrRange ip len)
              _ -> Left $ "connection refused: failed to parse CIDR: " ++ cidr
          _ -> Left $ "connection refused: failed to parse CIDR: " ++ cidr

-- | Bind and listen on the address derived from ServerListenToolParams,
-- then start the acceptLoop in a background thread.
-- sandboxNets is the list of allowed CIDR ranges passed to acceptLoop.
-- Returns Either String ListenerCtx; Left on IO failure.
serverListen
  :: ServerListenToolParams
  -> [String]                          -- sandboxNetworks
  -> STM.TQueue DM.AgentServerCommand
  -> ProjectedContext (Either String ListenerCtx)
serverListen params sandboxNets cmdQ appDat = E.tryAny go >>= \case
  Left  ex  -> return $ Left (show ex)
  Right ctx -> return $ Right ctx
  where
    go = do
      let portFunc = appDat^.portFuncAppData
          host     = fromMaybe "0.0.0.0" (params^.hostServerListenToolParams)
          port     = params^.portServerListenToolParams
      sock <- _bindListenServerPortFunc portFunc host port
      evQ  <- STM.newTQueueIO
      thd  <- async (acceptLoop sandboxNets portFunc sock evQ cmdQ appDat)
      return $ ListenerCtx sock thd evQ

-- | Loop accepting connections on the listening socket.
-- On each accepted connection, checks the remote address against sandboxNets.
-- Rejected connections are closed at TCP level and an ErrorOccurred event is enqueued.
-- Enforces single-connection policy using the _DEFAULT_HANDLE_NAME key.
-- If "default" key is already present in the ConnCtx map, sends "busy" and closes.
-- Otherwise starts recvLoop as a background thread and enqueues InternalClientConnected.
-- On any IO exception (including socket close), enqueues ListenerStopped and exits.
acceptLoop
  :: [String]
  -> ServerPortFunc
  -> Socket
  -> STM.TQueue ServerEvent
  -> STM.TQueue DM.AgentServerCommand
  -> AppData
  -> IO ()
acceptLoop sandboxNets portFunc sock evQ cmdQ appDat = E.catchAny go onErr
  where
    go = do
      (connSock, addr) <- _acceptConnServerPortFunc portFunc sock
      case checkRemoteAddr sandboxNets addr of
        Left err -> do
          -- Reject connection at TCP level; no MCP response is sent.
          hPutStrLn stderr $ "[WARN] acceptLoop: rejected " ++ addr ++ ": " ++ err
          _closeConnServerPortFunc portFunc connSock
          STM.atomically $ STM.writeTQueue evQ (ErrorOccurred err)
        Right () -> do
          connMap <- STM.atomically $ STM.readTMVar (appDat^.connCtxAppData)
          if Map.member _DEFAULT_HANDLE_NAME connMap
            then do
              -- Reject second connection: send busy message and close.
              hPutStrLn stderr $ "[WARN] acceptLoop: busy, rejecting " ++ addr
              NS.sendAll connSock "busy\r\n"
              _closeConnServerPortFunc portFunc connSock
            else do
              rQ  <- STM.newTQueueIO
              thd <- async (recvLoop connSock evQ rQ cmdQ)
              let connCtx = ConnCtx connSock thd rQ
              STM.atomically $ do
                m <- STM.takeTMVar (appDat^.connCtxAppData)
                STM.putTMVar (appDat^.connCtxAppData) (Map.insert _DEFAULT_HANDLE_NAME connCtx m)
              -- Notify ApplicationBase via the domain command queue.
              STM.atomically $ STM.writeTQueue cmdQ
                $ DM.AgentServerInternalClientConnectedCommand
                $ DM.AgentServerInternalClientConnectedCommandData _DEFAULT_HANDLE_NAME addr
      acceptLoop sandboxNets portFunc sock evQ cmdQ appDat
    onErr ex = do
      hPutStrLn stderr $ "[INFO] acceptLoop: stopped. " ++ show ex
      STM.atomically $ STM.writeTQueue evQ ListenerStopped

-- | Receive bytes from the accepted connection socket.
-- Enqueues BytesReceived to the server event queue and the per-connection
-- receive queue (rQ), and InternalClientDisconnected to the domain command
-- queue on disconnect.
recvLoop :: Socket -> STM.TQueue ServerEvent -> STM.TQueue String -> STM.TQueue DM.AgentServerCommand -> IO ()
recvLoop sock evQ rQ cmdQ = E.catchAny go onErr
  where
    go = do
      bs <- NS.recv sock _RECV_BUFFER_SIZE
      if BS.null bs
        then notify
        else do
          let hex = DU.bytesToHex bs
          STM.atomically $ do
            STM.writeTQueue evQ (BytesReceived _DEFAULT_HANDLE_NAME hex)
            STM.writeTQueue rQ hex
          recvLoop sock evQ rQ cmdQ
    onErr _ = notify
    notify = STM.atomically $ do
      STM.writeTQueue evQ (ClientDisconnected _DEFAULT_HANDLE_NAME)
      STM.writeTQueue cmdQ
        $ DM.AgentServerInternalClientDisconnectedCommand
        $ DM.AgentServerInternalClientDisconnectedCommandData _DEFAULT_HANDLE_NAME

-- | Query the current listener and connection status.
serverStatus :: ProjectedContext (Either String ServerStatusResult)
serverStatus appDat = E.tryAny go >>= \case
  Left ex -> return $ Left (show ex)
  Right r -> return $ Right r
  where
    go = do
      mCtx    <- STM.atomically $ STM.readTMVar (appDat^.listenerCtxAppData)
      connMap <- STM.atomically $ STM.readTMVar (appDat^.connCtxAppData)
      return $ ServerStatusResult
        { _isListeningServerStatusResult = isJust mCtx
        , _isConnectedServerStatusResult = Map.member _DEFAULT_HANDLE_NAME connMap
        , _listenAddrServerStatusResult  = Nothing
        }

-- | Drain and return queued ServerEvents (up to optional max count).
serverEvents :: ServerEventsToolParams -> ProjectedContext (Either String [ServerEvent])
serverEvents params appDat = E.tryAny go >>= \case
  Left ex -> return $ Left (show ex)
  Right xs -> return $ Right xs
  where
    go = do
      mCtx <- STM.atomically $ STM.readTMVar (appDat^.listenerCtxAppData)
      case mCtx of
        Nothing  -> return []
        Just ctx -> do
          evs <- STM.atomically $ STM.flushTQueue (ctx^.eventQueueListenerCtx)
          let n = fromMaybe maxBound (params^.maxEventsServerEventsToolParams)
          return (take n evs)

-- | Cancel the accept thread, close the listen socket, and clear the ListenerCtx.
serverCloseListener :: ProjectedContext (Either String ())
serverCloseListener appDat = E.tryAny go >>= \case
  Left ex -> return $ Left (show ex)
  Right r -> return r
  where
    go = do
      hPutStrLn stderr "[INFO] serverCloseListener: start."
      mCtx <- STM.atomically $ STM.readTMVar (appDat^.listenerCtxAppData)
      case mCtx of
        Nothing  -> do
          hPutStrLn stderr "[INFO] serverCloseListener: not listening."
          return $ Left "not listening"
        Just ctx -> do
          hPutStrLn stderr "[INFO] serverCloseListener: closing listen socket."
          _closeSocketServerPortFunc (appDat^.portFuncAppData) (ctx^.listenSocketListenerCtx)
          hPutStrLn stderr "[INFO] serverCloseListener: cancelling accept thread."
          cancel (ctx^.acceptThreadListenerCtx)
          hPutStrLn stderr "[INFO] serverCloseListener: clearing ListenerCtx."
          STM.atomically $ do
            _ <- STM.takeTMVar (appDat^.listenerCtxAppData)
            STM.putTMVar (appDat^.listenerCtxAppData) Nothing
          hPutStrLn stderr "[INFO] serverCloseListener: done."
          return $ Right ()

-- | Close the accepted default connection while keeping the listener alive.
-- Socket close errors are ignored (client may have already disconnected);
-- the ConnCtx map entry is always deleted to allow re-connection.
serverCloseConnection :: ProjectedContext (Either String ())
serverCloseConnection appDat = E.tryAny go >>= \case
  Left ex -> return $ Left (show ex)
  Right r -> return r
  where
    go = do
      hPutStrLn stderr "[INFO] serverCloseConnection: start."
      connMap <- STM.atomically $ STM.readTMVar (appDat^.connCtxAppData)
      case Map.lookup _DEFAULT_HANDLE_NAME connMap of
        Nothing  -> do
          hPutStrLn stderr "[INFO] serverCloseConnection: not connected."
          return $ Left "not connected"
        Just ctx -> do
          hPutStrLn stderr "[INFO] serverCloseConnection: closing socket."
          E.tryAny (_closeConnServerPortFunc (appDat^.portFuncAppData) (ctx^.socketConnCtx))
            >>= \r -> case r of
              Left ex -> hPutStrLn stderr $ "[WARN] serverCloseConnection: close socket: " ++ show ex
              Right _  -> return ()
          hPutStrLn stderr "[INFO] serverCloseConnection: cancelling recv thread."
          cancel (ctx^.recvThreadConnCtx)
          hPutStrLn stderr "[INFO] serverCloseConnection: clearing ConnCtx."
          STM.atomically $ do
            m <- STM.takeTMVar (appDat^.connCtxAppData)
            STM.putTMVar (appDat^.connCtxAppData) (Map.delete _DEFAULT_HANDLE_NAME m)
          hPutStrLn stderr "[INFO] serverCloseConnection: done."
          return $ Right ()

-- | Read text from the per-connection receive queue (populated by recvLoop).
-- Takes exactly 1 chunk from rQ, decodes it as UTF-8, and returns it.
-- Waits up to _READ_WAIT_MSEC for data; returns empty string on timeout.
serverRead :: ProjectedContext (Either String String)
serverRead appDat = E.tryAny go >>= \case
  Left ex -> return $ Left (show ex)
  Right r -> return r
  where
    go = do
      connMap <- STM.atomically $ STM.readTMVar (appDat^.connCtxAppData)
      case Map.lookup _DEFAULT_HANDLE_NAME connMap of
        Nothing  -> return $ Left "not connected"
        Just ctx -> do
          bs <- readFromRecvQueue (ctx^.recvQueueConnCtx) _READ_WAIT_MSEC
          return $ Right (DU.bs2strUTF8 bs)

-- | Write text to the accepted default connection socket.
serverWrite :: ServerWriteToolParams -> ProjectedContext (Either String ())
serverWrite params appDat = E.tryAny go >>= \case
  Left ex -> return $ Left (show ex)
  Right r -> return r
  where
    str = params^.dataServerWriteToolParams
    appendNewline = params^.appendNewlineServerWriteToolParams
    payload = if appendNewline == Just False then str else DU.appendCRLF str
    go = do
      connMap <- STM.atomically $ STM.readTMVar (appDat^.connCtxAppData)
      case Map.lookup _DEFAULT_HANDLE_NAME connMap of
        Nothing  -> return $ Left "not connected"
        Just ctx -> do
          NS.sendAll (ctx^.socketConnCtx) (T.encodeUtf8 (T.pack payload))
          return $ Right ()

-- | Read bytes from the per-connection receive queue as uppercase hex.
-- Takes exactly 1 chunk from rQ and returns it as hex.
-- Waits up to _READ_WAIT_MSEC for data; returns empty string on timeout.
serverReadByte :: ProjectedContext (Either String String)
serverReadByte appDat = E.tryAny go >>= \case
  Left ex -> return $ Left (show ex)
  Right r -> return r
  where
    go = do
      connMap <- STM.atomically $ STM.readTMVar (appDat^.connCtxAppData)
      case Map.lookup _DEFAULT_HANDLE_NAME connMap of
        Nothing  -> return $ Left "not connected"
        Just ctx -> do
          bs <- readFromRecvQueue (ctx^.recvQueueConnCtx) _READ_WAIT_MSEC
          return $ Right (DU.bytesToHex bs)

-- | Write uppercase/lowercase hex bytes to the accepted default connection socket.
serverWriteByte :: ServerWriteByteToolParams -> ProjectedContext (Either String ())
serverWriteByte params appDat = E.tryAny go >>= \case
  Left ex -> return $ Left (show ex)
  Right r -> return r
  where
    hex = params^.dataServerWriteByteToolParams
    go = do
      connMap <- STM.atomically $ STM.readTMVar (appDat^.connCtxAppData)
      case Map.lookup _DEFAULT_HANDLE_NAME connMap of
        Nothing  -> return $ Left "not connected"
        Just ctx ->
          case DU.hexToBytes hex of
            Left err -> return $ Left err
            Right bs -> do
              NS.sendAll (ctx^.socketConnCtx) bs
              return $ Right ()

-- | Take exactly 1 hex-encoded chunk from the per-connection receive queue
-- and decode it to a ByteString.
-- Waits up to `tout` milliseconds for data; returns empty ByteString on timeout.
readFromRecvQueue :: STM.TQueue String -> Int -> IO BS.ByteString
readFromRecvQueue rQ tout = do
  tvar <- STM.registerDelay (tout * 1000)
  mHex <- STM.atomically $
    (Just <$> STM.readTQueue rQ)
    `STM.orElse`
    (STM.readTVar tvar >>= \expired ->
      if expired then return Nothing else STM.retry)
  case mHex of
    Nothing  -> return BS.empty
    Just hex -> case DU.hexToBytes hex of
      Right bs -> return bs
      Left  _  -> return BS.empty

-- | Serialize a ServerStatusResult to a JSON string.
formatStatus :: ServerStatusResult -> String
formatStatus = T.unpack . T.decodeUtf8 . BSL.toStrict . encode

-- | Serialize a list of ServerEvents to a JSON string.
formatEvents :: [ServerEvent] -> String
formatEvents = T.unpack . T.decodeUtf8 . BSL.toStrict . encode
