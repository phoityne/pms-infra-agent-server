{-# LANGUAGE OverloadedStrings #-}

module PMS.Infra.Agent.Server.ApplicationBase.State.IdleSpec (spec) where

import Test.Hspec

import PMS.Infra.Agent.Server.ApplicationBase.Control (defaultAppData)
import PMS.Infra.Agent.Server.ApplicationBase.State.Idle ()
import PMS.Infra.Agent.Server.CoreModel.Constant (_DEFAULT_HANDLE_NAME)
import PMS.Infra.Agent.Server.CoreModel.Type
import PMS.Infra.Agent.Server.TestSupport

spec :: Spec
spec = describe "Idle state interfaces" $ do
  it "T-01: listen succeeds and transitions to Listening" $ do
    domDat <- mkDomainData
    let pf = mockPortFunc
               { _bindListenServerPortFunc = \_ _ -> return (error "mock socket ok") }
    appDat <- defaultAppData pf
    let ev = EventW $ ServerListenEvent
               $ ServerListenEventData dummyJsonRpc
               $ ServerListenToolParams (Just "127.0.0.1") "19999"
    (mResp, nst) <- runAction domDat appDat (AppStateW IdleState)
                      (actionSW (AppStateW IdleState) ev)
    isListeningState nst `shouldBe` True
    shouldBeSuccess mResp

  it "T-02: listen while listener exists returns error" $ do
    domDat <- mkDomainData
    (appDat, _) <- mkListeningAppData
    let ev = EventW $ ServerListenEvent
               $ ServerListenEventData dummyJsonRpc
               $ ServerListenToolParams (Just "127.0.0.1") "19999"
    (mResp, nst) <- runAction domDat appDat (AppStateW IdleState)
                      (actionSW (AppStateW IdleState) ev)
    isIdleState nst `shouldBe` True
    shouldBeError mResp

  it "T-03: close returns invalid-state error" $ do
    domDat <- mkDomainData
    appDat <- defaultAppData mockPortFunc
    let ev = EventW $ ServerCloseEvent $ ServerCloseEventData dummyJsonRpc
    (mResp, nst) <- runAction domDat appDat (AppStateW IdleState)
                      (actionSW (AppStateW IdleState) ev)
    isIdleState nst `shouldBe` True
    shouldBeError mResp

  it "T-04: status succeeds" $ do
    domDat <- mkDomainData
    appDat <- defaultAppData mockPortFunc
    let ev = EventW $ ServerStatusEvent $ ServerStatusEventData dummyJsonRpc
    (mResp, nst) <- runAction domDat appDat (AppStateW IdleState)
                      (actionSW (AppStateW IdleState) ev)
    isIdleState nst `shouldBe` True
    shouldBeSuccess mResp

  it "T-05: events succeeds with empty list" $ do
    domDat <- mkDomainData
    appDat <- defaultAppData mockPortFunc
    let ev = EventW $ ServerEventsEvent
               $ ServerEventsEventData dummyJsonRpc (ServerEventsToolParams Nothing)
    (mResp, nst) <- runAction domDat appDat (AppStateW IdleState)
                      (actionSW (AppStateW IdleState) ev)
    isIdleState nst `shouldBe` True
    shouldBeSuccess mResp

  let invalidCases =
        [ ("T-06: read", EventW $ ServerReadEvent
            $ ServerReadEventData dummyJsonRpc)
        , ("T-07: write", EventW $ ServerWriteEvent
            $ ServerWriteEventData dummyJsonRpc (ServerWriteToolParams "" Nothing))
        , ("T-08: read-byte", EventW $ ServerReadByteEvent
            $ ServerReadByteEventData dummyJsonRpc)
        , ("T-09: write-byte", EventW $ ServerWriteByteEvent
            $ ServerWriteByteEventData dummyJsonRpc (ServerWriteByteToolParams ""))
        ]
  mapM_ (\(label, ev) ->
    it (label ++ " returns invalid-state error") $ do
      domDat <- mkDomainData
      appDat <- defaultAppData mockPortFunc
      (mResp, nst) <- runAction domDat appDat (AppStateW IdleState)
                        (actionSW (AppStateW IdleState) ev)
      isIdleState nst `shouldBe` True
      shouldBeError mResp
    ) invalidCases

  let internalCases =
        [ ("T-10: internal connect", EventW $ InternalClientConnectedEvent
            $ InternalClientConnectedEventData _DEFAULT_HANDLE_NAME "127.0.0.1:9999")
        , ("T-11: internal disconnect", EventW
            $ InternalClientDisconnectedEvent InternalClientDisconnectedEventData)
        ]
  mapM_ (\(label, ev) ->
    it (label ++ " has no transition and no response") $ do
      domDat <- mkDomainData
      appDat <- defaultAppData mockPortFunc
      (mResp, nst) <- runAction domDat appDat (AppStateW IdleState)
                        (actionSW (AppStateW IdleState) ev)
      isIdleState nst `shouldBe` True
      mResp `shouldBe` Nothing
    ) internalCases
