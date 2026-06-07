{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module PMS.Infra.Agent.Server.ProjectedContext.CoreSpec (spec) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, wait)
import Control.Exception.Safe (tryAny)
import Data.IORef
import Network.Socket
  ( ShutdownCmd(ShutdownSend)
  , Family(AF_INET)
  , Socket
  , SocketOption(ReuseAddr)
  , SocketType(Stream)
  , SockAddr(SockAddrInet)
  , accept
  , bind
  , close
  , connect
  , defaultProtocol
  , getSocketName
  , listen
  , setSocketOption
  , shutdown
  , socket
  , tupleToHostAddress
  )
import Test.Hspec
import qualified Control.Concurrent.STM as STM
import qualified Data.ByteString as BS
import qualified Network.Socket.ByteString as NS
import qualified PMS.Domain.Model.DM.Type as DM

import PMS.Infra.Agent.Server.ApplicationBase.Control (defaultAppData)
import PMS.Infra.Agent.Server.CoreModel.Constant (_DEFAULT_HANDLE_NAME)
import PMS.Infra.Agent.Server.CoreModel.Type
import PMS.Infra.Agent.Server.ProjectedContext.Core
  ( acceptLoop
  , checkRemoteAddr
  , recvLoop
  )

spec :: Spec
spec = describe "ProjectedContext.Core" $ do

  -- ----------------------------------------------------------------
  -- recvLoop tests (existing)
  -- ----------------------------------------------------------------

  it "T-34: recvLoop enqueues BytesReceived with uppercase hex" $ do
    (serverSock, clientSock, evQ, cmdQ) <- startRecvLoopWithBytes (BS.pack [0x00, 0x0A, 0x1B, 0xFF])
    ev <- STM.atomically $ STM.readTQueue evQ
    ev `shouldBe` BytesReceived _DEFAULT_HANDLE_NAME "000A1BFF"
    _ <- STM.atomically $ STM.readTQueue cmdQ
    close serverSock
    close clientSock

  it "T-35: recvLoop enqueues ClientDisconnected and internal disconnect command on EOF" $ do
    (serverSock, clientSock, evQ, cmdQ) <- startRecvLoopWithBytes BS.empty
    ev <- STM.atomically $ STM.readTQueue evQ
    ev `shouldBe` ClientDisconnected _DEFAULT_HANDLE_NAME
    cmd <- STM.atomically $ STM.readTQueue cmdQ
    cmd `shouldSatisfy` isInternalDisconnectedCommand
    close serverSock
    close clientSock

  -- ----------------------------------------------------------------
  -- checkRemoteAddr unit tests (T-40 to T-47)
  -- ----------------------------------------------------------------

  it "T-40: empty sandboxNetworks denies all connections" $
    checkRemoteAddr [] "127.0.0.1:12345"
      `shouldBe` Left "connection refused: sandboxNetworks is empty."

  it "T-41: IPv6 address is rejected" $
    checkRemoteAddr ["127.0.0.0/8"] "[::1]:12345"
      `shouldBe` Left "connection refused: IPv6 is not supported."

  it "T-42: unparseable address is rejected" $
    checkRemoteAddr ["127.0.0.0/8"] "not-an-ip"
      `shouldBe` Left "connection refused: failed to parse remote address."

  it "T-43: IP outside CIDR range is rejected" $
    checkRemoteAddr ["192.168.0.0/16"] "10.0.0.1:9999"
      `shouldBe` Left "connection refused: 10.0.0.1 is outside sandboxNetworks."

  it "T-44: IP inside CIDR range is allowed" $
    checkRemoteAddr ["192.168.0.0/16"] "192.168.1.100:8080"
      `shouldBe` Right ()

  it "T-45: IP matching one of multiple CIDRs is allowed" $
    checkRemoteAddr ["10.0.0.0/8", "192.168.0.0/16"] "192.168.2.3:1234"
      `shouldBe` Right ()

  it "T-46: loopback 127.0.0.1 matches /8 range" $
    checkRemoteAddr ["127.0.0.0/8"] "127.0.0.1:9000"
      `shouldBe` Right ()

  it "T-47: address without port is rejected (parse failure)" $
    checkRemoteAddr ["127.0.0.0/8"] "127.0.0.1"
      `shouldBe` Left "connection refused: failed to parse remote address."

  -- ----------------------------------------------------------------
  -- acceptLoop integration tests (T-48 to T-50)
  -- ----------------------------------------------------------------

  it "T-48: acceptLoop enqueues ErrorOccurred when remote IP is outside sandboxNetworks" $ do
    evQ  <- STM.newTQueueIO
    cmdQ <- STM.newTQueueIO
    -- accept returns a disallowed address once, then throws to stop the loop.
    portFunc <- mkSingleShotPortFunc "10.0.0.1:9999"
    appDat   <- defaultAppData portFunc
    _ <- tryAny (runAcceptLoopOnce ["192.168.0.0/16"] portFunc evQ cmdQ appDat)
    ev <- STM.atomically $ STM.readTQueue evQ
    ev `shouldSatisfy` isErrorOccurred

  it "T-49: acceptLoop enqueues InternalClientConnected when IP is inside sandboxNetworks" $ do
    evQ  <- STM.newTQueueIO
    cmdQ <- STM.newTQueueIO
    -- accept returns an allowed address once, then throws to stop the loop.
    portFunc <- mkSingleShotPortFunc "127.0.0.1:12345"
    appDat   <- defaultAppData portFunc
    _ <- tryAny (runAcceptLoopOnce ["127.0.0.0/8"] portFunc evQ cmdQ appDat)
    -- Give recvLoop time to start before checking the command queue.
    threadDelay 30000
    cmd <- STM.atomically $ STM.readTQueue cmdQ
    cmd `shouldSatisfy` isInternalConnectedCommand

  it "T-50: acceptLoop rejects all connections when sandboxNetworks is empty" $ do
    evQ  <- STM.newTQueueIO
    cmdQ <- STM.newTQueueIO
    portFunc <- mkSingleShotPortFunc "127.0.0.1:12345"
    appDat   <- defaultAppData portFunc
    _ <- tryAny (runAcceptLoopOnce [] portFunc evQ cmdQ appDat)
    ev <- STM.atomically $ STM.readTQueue evQ
    ev `shouldSatisfy` isErrorOccurred

-- ----------------------------------------------------------------
-- Helpers
-- ----------------------------------------------------------------

startRecvLoopWithBytes
  :: BS.ByteString
  -> IO (Socket, Socket, STM.TQueue ServerEvent, STM.TQueue DM.AgentServerCommand)
startRecvLoopWithBytes bs = do
  (serverSock, clientSock) <- createTcpPair
  NS.sendAll clientSock bs
  shutdown clientSock ShutdownSend
  evQ  <- STM.newTQueueIO
  rQ   <- STM.newTQueueIO
  cmdQ <- STM.newTQueueIO
  thd <- async (recvLoop serverSock evQ rQ cmdQ)
  wait thd
  return (serverSock, clientSock, evQ, cmdQ)

-- | Build a ServerPortFunc whose accept returns the given address exactly once,
-- then raises an exception on the second call (to terminate the acceptLoop).
mkSingleShotPortFunc :: String -> IO ServerPortFunc
mkSingleShotPortFunc addr = do
  counter <- newIORef (0 :: Int)
  return ServerPortFunc
    { _bindListenServerPortFunc  = \_ _ -> error "mkSingleShotPortFunc: bind not expected"
    , _acceptConnServerPortFunc  = \_ -> do
        n <- readIORef counter
        writeIORef counter (n + 1)
        if n == 0
          then do
            (serverSock, clientSock) <- createTcpPair
            -- Pre-send EOF so recvLoop terminates quickly.
            shutdown clientSock ShutdownSend
            close clientSock
            return (serverSock, addr)
          else ioError (userError "single-shot: no more connections")
    , _closeSocketServerPortFunc = \_ -> return ()
    , _closeConnServerPortFunc   = \s -> tryAny (close s) >> return ()
    }

createTcpPair :: IO (Socket, Socket)
createTcpPair = do
  listenSock <- socket AF_INET Stream defaultProtocol
  setSocketOption listenSock ReuseAddr 1
  bind listenSock (SockAddrInet 0 (tupleToHostAddress (127, 0, 0, 1)))
  listen listenSock 1
  SockAddrInet port _ <- getSocketName listenSock
  clientSock <- socket AF_INET Stream defaultProtocol
  acceptAsync <- async (accept listenSock)
  connect clientSock (SockAddrInet port (tupleToHostAddress (127, 0, 0, 1)))
  (serverSock, _) <- wait acceptAsync
  close listenSock
  return (serverSock, clientSock)

-- | A dummy Socket value. The socket is never actually used because the
-- acceptConn implementation in our mock portFunc ignores it.
dummySocket :: Socket
dummySocket = error "dummySocket: should never be evaluated"

-- | Run acceptLoop in a background thread and wait for it to finish.
-- The loop terminates when the single-shot portFunc raises after one accept.
runAcceptLoopOnce
  :: [String]
  -> ServerPortFunc
  -> STM.TQueue ServerEvent
  -> STM.TQueue DM.AgentServerCommand
  -> AppData
  -> IO ()
runAcceptLoopOnce sandboxNets portFunc evQ cmdQ appDat = do
  thd <- async (acceptLoop sandboxNets portFunc dummySocket evQ cmdQ appDat)
  -- Wait long enough for the loop to process one accept and stop.
  threadDelay 100000
  _ <- tryAny (wait thd)
  return ()

isInternalDisconnectedCommand :: DM.AgentServerCommand -> Bool
isInternalDisconnectedCommand = \case
  DM.AgentServerInternalClientDisconnectedCommand _ -> True
  _ -> False

isInternalConnectedCommand :: DM.AgentServerCommand -> Bool
isInternalConnectedCommand = \case
  DM.AgentServerInternalClientConnectedCommand _ -> True
  _ -> False

isErrorOccurred :: ServerEvent -> Bool
isErrorOccurred (ErrorOccurred _) = True
isErrorOccurred _                 = False
