{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module PMS.Infra.Agent.Server.ApplicationBase.FakeInterfaceSpec (spec) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, wait)
import Control.Lens ((^.))
import Network.Socket
  ( Family(AF_INET)
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
  , socket
  , tupleToHostAddress
  )
import System.Timeout (timeout)
import Test.Hspec
import qualified Control.Concurrent.STM as STM
import qualified Data.ByteString as BS
import qualified Network.Socket.ByteString as NS
import qualified PMS.Domain.Model.DM.Type as DM

import PMS.Infra.Agent.Server.ApplicationBase.Control (defaultAppData)
import PMS.Infra.Agent.Server.CoreModel.Type
import PMS.Infra.Agent.Server.TestSupport

spec :: Spec
spec = describe "ApplicationBase with fake ServerPortFunc" $
  it "covers listen, fake accept, events, connected close, listener close, and re-listen" $ do
    acceptQ <- STM.newTQueueIO
    closeSocketQ <- STM.newTQueueIO
    closeHandleQ <- STM.newTQueueIO
    domDat <- mkDomainDataWithNetworks ["127.0.0.0/8"]
    appDat <- defaultAppData (fakePortFunc acceptQ closeSocketQ closeHandleQ)

    (serverSock, clientSock) <- createTcpPair
    STM.atomically $ STM.writeTQueue acceptQ serverSock

    let listenEv = EventW $ ServerListenEvent
          $ ServerListenEventData dummyJsonRpc
          $ ServerListenToolParams (Just "127.0.0.1") "0"
    (listenResp, listeningState) <- runAction domDat appDat (AppStateW IdleState)
      (actionSW (AppStateW IdleState) listenEv)
    shouldBeSuccess listenResp
    isListeningState listeningState `shouldBe` True

    connectedCmd <- waitInternalConnected domDat
    let connectEv = EventW $ commandToInternalConnectedEvent connectedCmd
    (connectResp, connectedState) <- runAction domDat appDat listeningState
      (actionSW listeningState connectEv)
    connectResp `shouldBe` Nothing
    isConnectedState connectedState `shouldBe` True

    NS.sendAll clientSock "A"
    eventsResp <- waitUntilEventsContain domDat appDat connectedState "BytesReceived"
    respContent eventsResp `shouldContain` "41"

    close clientSock
    let closeConnEv = EventW $ ServerCloseEvent $ ServerCloseEventData dummyJsonRpc
    (closeConnResp, backToListeningState) <- runAction domDat appDat connectedState
      (actionSW connectedState closeConnEv)
    shouldBeSuccess closeConnResp
    isListeningState backToListeningState `shouldBe` True
    _ <- STM.atomically $ STM.readTQueue closeHandleQ

    let closeListenEv = EventW $ ServerCloseEvent $ ServerCloseEventData dummyJsonRpc
    (closeListenResp, idleState) <- runAction domDat appDat backToListeningState
      (actionSW backToListeningState closeListenEv)
    shouldBeSuccess closeListenResp
    isIdleState idleState `shouldBe` True
    _ <- STM.atomically $ STM.readTQueue closeSocketQ

    let relistenEv = EventW $ ServerListenEvent
          $ ServerListenEventData dummyJsonRpc
          $ ServerListenToolParams (Just "127.0.0.1") "0"
    (relistenResp, relisteningState) <- runAction domDat appDat idleState
      (actionSW idleState relistenEv)
    shouldBeSuccess relistenResp
    isListeningState relisteningState `shouldBe` True

    let closeRelistenEv = EventW $ ServerCloseEvent $ ServerCloseEventData dummyJsonRpc
    (closeRelistenResp, finalState) <- runAction domDat appDat relisteningState
      (actionSW relisteningState closeRelistenEv)
    shouldBeSuccess closeRelistenResp
    isIdleState finalState `shouldBe` True
    _ <- STM.atomically $ STM.readTQueue closeSocketQ
    return ()

fakePortFunc :: STM.TQueue Socket -> STM.TQueue () -> STM.TQueue Socket -> ServerPortFunc
fakePortFunc acceptQ closeSocketQ closeHandleQ = ServerPortFunc
  { _bindListenServerPortFunc  = \_ _ -> return (error "fake listen socket")
  , _acceptConnServerPortFunc  = \_ -> do
      sock <- STM.atomically $ STM.readTQueue acceptQ
      return (sock, "127.0.0.1:0")
  , _closeSocketServerPortFunc = \_ -> STM.atomically $ STM.writeTQueue closeSocketQ ()
  , _closeConnServerPortFunc   = \sock -> do
      STM.atomically $ STM.writeTQueue closeHandleQ sock
      close sock
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

waitInternalConnected :: DM.DomainData -> IO DM.AgentServerCommand
waitInternalConnected domDat =
  timeout 3000000 (STM.atomically $ STM.readTQueue (domDat^.DM.agentServerQueueDomainData)) >>= \case
    Nothing -> fail "timed out waiting for internal client connected command"
    Just cmd@(DM.AgentServerInternalClientConnectedCommand _) -> return cmd
    Just other -> fail $ "unexpected command: " ++ show other

commandToInternalConnectedEvent :: DM.AgentServerCommand -> Event InternalClientConnectedEventData
commandToInternalConnectedEvent = \case
  DM.AgentServerInternalClientConnectedCommand dat ->
    InternalClientConnectedEvent $
      InternalClientConnectedEventData
        (dat^.DM.handleNameAgentServerInternalClientConnectedCommandData)
        (dat^.DM.remoteAddrAgentServerInternalClientConnectedCommandData)
  other -> error $ "commandToInternalConnectedEvent: unexpected command " ++ show other

waitUntilEventsContain :: DM.DomainData -> AppData -> AppStateW -> String -> IO DM.McpResponse
waitUntilEventsContain domDat appDat st expected = go (20 :: Int)
  where
    go 0 = fail $ "timed out waiting for events containing " ++ expected
    go n = do
      let ev = EventW $ ServerEventsEvent
            $ ServerEventsEventData dummyJsonRpc
            $ ServerEventsToolParams (Just 10)
      (mResp, _) <- runAction domDat appDat st (actionSW st ev)
      case mResp of
        Just resp | expected `contains` respContent resp -> return resp
        _ -> threadDelay 50000 >> go (n - 1)

contains :: String -> String -> Bool
contains needle haystack = BS.isInfixOf (fromString needle) (fromString haystack)

fromString :: String -> BS.ByteString
fromString = BS.pack . map (fromIntegral . fromEnum)
