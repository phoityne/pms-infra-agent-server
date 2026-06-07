{-# LANGUAGE OverloadedStrings #-}

module PMS.Infra.Agent.Server.ApplicationBase.State.ListeningSpec (spec) where

import Test.Hspec
import qualified Control.Concurrent.STM as STM

import PMS.Infra.Agent.Server.ApplicationBase.State.Listening ()
import PMS.Infra.Agent.Server.CoreModel.Constant (_DEFAULT_HANDLE_NAME)
import PMS.Infra.Agent.Server.CoreModel.Type
import PMS.Infra.Agent.Server.TestSupport

spec :: Spec
spec = describe "Listening state interfaces" $ do
  it "T-12: listen returns invalid-state error" $ do
    domDat <- mkDomainData
    (appDat, _) <- mkListeningAppData
    let ev = EventW $ ServerListenEvent
               $ ServerListenEventData dummyJsonRpc
               $ ServerListenToolParams (Just "127.0.0.1") "19999"
    (mResp, nst) <- runAction domDat appDat (AppStateW ListeningState)
                      (actionSW (AppStateW ListeningState) ev)
    isListeningState nst `shouldBe` True
    shouldBeError mResp

  it "T-13: close succeeds and transitions to Idle" $ do
    domDat <- mkDomainData
    (appDat, _) <- mkListeningAppData
    let ev = EventW $ ServerCloseEvent $ ServerCloseEventData dummyJsonRpc
    (mResp, nst) <- runAction domDat appDat (AppStateW ListeningState)
                      (actionSW (AppStateW ListeningState) ev)
    isIdleState nst `shouldBe` True
    shouldBeSuccess mResp

  it "T-14: status succeeds" $ do
    domDat <- mkDomainData
    (appDat, _) <- mkListeningAppData
    let ev = EventW $ ServerStatusEvent $ ServerStatusEventData dummyJsonRpc
    (mResp, nst) <- runAction domDat appDat (AppStateW ListeningState)
                      (actionSW (AppStateW ListeningState) ev)
    isListeningState nst `shouldBe` True
    shouldBeSuccess mResp

  it "T-15: events succeeds with empty queue" $ do
    domDat <- mkDomainData
    (appDat, _) <- mkListeningAppData
    let ev = EventW $ ServerEventsEvent
               $ ServerEventsEventData dummyJsonRpc (ServerEventsToolParams Nothing)
    (mResp, nst) <- runAction domDat appDat (AppStateW ListeningState)
                      (actionSW (AppStateW ListeningState) ev)
    isListeningState nst `shouldBe` True
    shouldBeSuccess mResp

  it "T-16: events succeeds with queued data" $ do
    domDat <- mkDomainData
    (appDat, evQ) <- mkListeningAppData
    STM.atomically $ STM.writeTQueue evQ
      (ClientConnected _DEFAULT_HANDLE_NAME "127.0.0.1:9999")
    let ev = EventW $ ServerEventsEvent
               $ ServerEventsEventData dummyJsonRpc (ServerEventsToolParams Nothing)
    (mResp, nst) <- runAction domDat appDat (AppStateW ListeningState)
                      (actionSW (AppStateW ListeningState) ev)
    isListeningState nst `shouldBe` True
    shouldBeSuccess mResp
    fmap respContent mResp `shouldNotBe` Just ""

  let invalidCases =
        [ ("T-17: read", EventW $ ServerReadEvent
            $ ServerReadEventData dummyJsonRpc)
        , ("T-18: write", EventW $ ServerWriteEvent
            $ ServerWriteEventData dummyJsonRpc (ServerWriteToolParams "" Nothing))
        , ("T-19: read-byte", EventW $ ServerReadByteEvent
            $ ServerReadByteEventData dummyJsonRpc)
        , ("T-20: write-byte", EventW $ ServerWriteByteEvent
            $ ServerWriteByteEventData dummyJsonRpc (ServerWriteByteToolParams ""))
        ]
  mapM_ (\(label, ev) ->
    it (label ++ " returns invalid-state error") $ do
      domDat <- mkDomainData
      (appDat, _) <- mkListeningAppData
      (mResp, nst) <- runAction domDat appDat (AppStateW ListeningState)
                        (actionSW (AppStateW ListeningState) ev)
      isListeningState nst `shouldBe` True
      shouldBeError mResp
    ) invalidCases

  it "T-21: internal connect transitions to Connected without response" $ do
    domDat <- mkDomainData
    (appDat, _) <- mkListeningAppData
    let ev = EventW $ InternalClientConnectedEvent
               $ InternalClientConnectedEventData _DEFAULT_HANDLE_NAME "127.0.0.1:9999"
    (mResp, nst) <- runAction domDat appDat (AppStateW ListeningState)
                      (actionSW (AppStateW ListeningState) ev)
    isConnectedState nst `shouldBe` True
    mResp `shouldBe` Nothing

  it "T-22: internal disconnect has no transition and no response" $ do
    domDat <- mkDomainData
    (appDat, _) <- mkListeningAppData
    let ev = EventW $ InternalClientDisconnectedEvent InternalClientDisconnectedEventData
    (mResp, nst) <- runAction domDat appDat (AppStateW ListeningState)
                      (actionSW (AppStateW ListeningState) ev)
    isListeningState nst `shouldBe` True
    mResp `shouldBe` Nothing
