{-# LANGUAGE OverloadedStrings #-}

module PMS.Infra.Agent.Server.ApplicationBase.ControlSpec (spec) where

import Test.Hspec

import PMS.Infra.Agent.Server.ApplicationBase.Control (defaultAppData)
import PMS.Infra.Agent.Server.ApplicationBase.State.Connected ()
import PMS.Infra.Agent.Server.ApplicationBase.State.Idle ()
import PMS.Infra.Agent.Server.ApplicationBase.State.Listening ()
import PMS.Infra.Agent.Server.CoreModel.Constant (_DEFAULT_HANDLE_NAME)
import PMS.Infra.Agent.Server.CoreModel.Type
import PMS.Infra.Agent.Server.TestSupport

spec :: Spec
spec = describe "ApplicationBase.Control state transitions" $ do
  it "transitions Idle -> Listening after listen succeeds" $ do
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

  it "transitions Listening -> Idle after close succeeds" $ do
    domDat <- mkDomainData
    (appDat, _) <- mkListeningAppData
    let ev = EventW $ ServerCloseEvent $ ServerCloseEventData dummyJsonRpc
    (mResp, nst) <- runAction domDat appDat (AppStateW ListeningState)
                      (actionSW (AppStateW ListeningState) ev)
    isIdleState nst `shouldBe` True
    shouldBeSuccess mResp

  it "transitions Listening -> Connected after internal connect event" $ do
    domDat <- mkDomainData
    (appDat, _) <- mkListeningAppData
    let ev = EventW $ InternalClientConnectedEvent
               $ InternalClientConnectedEventData _DEFAULT_HANDLE_NAME "127.0.0.1:9999"
    (mResp, nst) <- runAction domDat appDat (AppStateW ListeningState)
                      (actionSW (AppStateW ListeningState) ev)
    isConnectedState nst `shouldBe` True
    mResp `shouldBe` Nothing

  it "transitions Connected -> Listening after internal disconnect event" $ do
    domDat <- mkDomainData
    appDat <- mkConnectedAppData
    let ev = EventW $ InternalClientDisconnectedEvent InternalClientDisconnectedEventData
    (mResp, nst) <- runAction domDat appDat (AppStateW ConnectedState)
                      (actionSW (AppStateW ConnectedState) ev)
    isListeningState nst `shouldBe` True
    mResp `shouldBe` Nothing
