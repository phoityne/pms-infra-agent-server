{-# LANGUAGE OverloadedStrings #-}

module PMS.Infra.Agent.Server.ApplicationBase.State.ConnectedSpec (spec) where

import Control.Lens ((^.))
import Test.Hspec
import qualified Control.Concurrent.STM as STM
import qualified Data.Map.Strict as Map

import PMS.Infra.Agent.Server.ApplicationBase.State.Connected ()
import PMS.Infra.Agent.Server.CoreModel.Constant (_DEFAULT_HANDLE_NAME)
import PMS.Infra.Agent.Server.CoreModel.Type
import PMS.Infra.Agent.Server.TestSupport

spec :: Spec
spec = describe "Connected state interfaces" $ do
  it "T-23: listen returns invalid-state error" $ do
    domDat <- mkDomainData
    appDat <- mkConnectedAppData
    let ev = EventW $ ServerListenEvent
               $ ServerListenEventData dummyJsonRpc
               $ ServerListenToolParams (Just "127.0.0.1") "19999"
    (mResp, nst) <- runAction domDat appDat (AppStateW ConnectedState)
                      (actionSW (AppStateW ConnectedState) ev)
    isConnectedState nst `shouldBe` True
    shouldBeError mResp

  it "T-24: close closes the default connection and returns to Listening" $ do
    domDat <- mkDomainData
    appDat <- mkConnectedAppData
    let ev = EventW $ ServerCloseEvent $ ServerCloseEventData dummyJsonRpc
    (mResp, nst) <- runAction domDat appDat (AppStateW ConnectedState)
                      (actionSW (AppStateW ConnectedState) ev)
    connMap <- STM.atomically $ STM.readTMVar (appDat^.connCtxAppData)
    isListeningState nst `shouldBe` True
    shouldBeSuccess mResp
    Map.member _DEFAULT_HANDLE_NAME connMap `shouldBe` False

  it "T-25: status succeeds" $ do
    domDat <- mkDomainData
    appDat <- mkConnectedAppData
    let ev = EventW $ ServerStatusEvent $ ServerStatusEventData dummyJsonRpc
    (mResp, nst) <- runAction domDat appDat (AppStateW ConnectedState)
                      (actionSW (AppStateW ConnectedState) ev)
    isConnectedState nst `shouldBe` True
    shouldBeSuccess mResp

  it "T-26: events succeeds with empty queue" $ do
    domDat <- mkDomainData
    appDat <- mkConnectedAppData
    let ev = EventW $ ServerEventsEvent
               $ ServerEventsEventData dummyJsonRpc (ServerEventsToolParams Nothing)
    (mResp, nst) <- runAction domDat appDat (AppStateW ConnectedState)
                      (actionSW (AppStateW ConnectedState) ev)
    isConnectedState nst `shouldBe` True
    shouldBeSuccess mResp

  let ioErrorCases =
        [ ("T-27: read", EventW $ ServerReadEvent
            $ ServerReadEventData dummyJsonRpc)
        , ("T-29: read-byte", EventW $ ServerReadByteEvent
            $ ServerReadByteEventData dummyJsonRpc)
        ]
  mapM_ (\(label, ev) ->
    it (label ++ " returns success with empty result on timeout") $ do
      domDat <- mkDomainData
      appDat <- mkConnectedAppData
      (mResp, nst) <- runAction domDat appDat (AppStateW ConnectedState)
                        (actionSW (AppStateW ConnectedState) ev)
      isConnectedState nst `shouldBe` True
      shouldBeSuccess mResp
    ) ioErrorCases

  let writeErrorCases =
        [ ("T-28: write", EventW $ ServerWriteEvent
            $ ServerWriteEventData dummyJsonRpc (ServerWriteToolParams "" Nothing))
        , ("T-30: write-byte", EventW $ ServerWriteByteEvent
            $ ServerWriteByteEventData dummyJsonRpc (ServerWriteByteToolParams "00"))
        ]
  mapM_ (\(label, ev) ->
    it (label ++ " returns IO error response with mock socket") $ do
      domDat <- mkDomainData
      appDat <- mkConnectedAppData
      (mResp, nst) <- runAction domDat appDat (AppStateW ConnectedState)
                        (actionSW (AppStateW ConnectedState) ev)
      isConnectedState nst `shouldBe` True
      shouldBeError mResp
    ) writeErrorCases

  it "T-31: internal connect has no transition and no response" $ do
    domDat <- mkDomainData
    appDat <- mkConnectedAppData
    let ev = EventW $ InternalClientConnectedEvent
               $ InternalClientConnectedEventData _DEFAULT_HANDLE_NAME "127.0.0.1:9999"
    (mResp, nst) <- runAction domDat appDat (AppStateW ConnectedState)
                      (actionSW (AppStateW ConnectedState) ev)
    isConnectedState nst `shouldBe` True
    mResp `shouldBe` Nothing

  it "T-32: internal disconnect transitions to Listening without response" $ do
    domDat <- mkDomainData
    appDat <- mkConnectedAppData
    let ev = EventW $ InternalClientDisconnectedEvent InternalClientDisconnectedEventData
    (mResp, nst) <- runAction domDat appDat (AppStateW ConnectedState)
                      (actionSW (AppStateW ConnectedState) ev)
    isListeningState nst `shouldBe` True
    mResp `shouldBe` Nothing

  it "T-33: write-byte with invalid hex returns error before touching handle" $ do
    domDat <- mkDomainData
    appDat <- mkConnectedAppData
    let ev = EventW $ ServerWriteByteEvent
               $ ServerWriteByteEventData dummyJsonRpc (ServerWriteByteToolParams "xx")
    (mResp, nst) <- runAction domDat appDat (AppStateW ConnectedState)
                      (actionSW (AppStateW ConnectedState) ev)
    isConnectedState nst `shouldBe` True
    shouldBeError mResp
