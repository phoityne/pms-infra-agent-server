{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module PMS.Infra.Agent.Server.TestSupport where

import Control.Concurrent.Async (async)
import Control.Lens ((^.), (&), (.~))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Lazy (get)
import System.Log.FastLogger (LogType'(LogNone), newTimeCache, newTimedFastLogger)
import Test.Hspec
import qualified Control.Concurrent.STM as STM
import qualified Data.Map.Strict as Map
import qualified PMS.Domain.Model.DM.Type as DM

import PMS.Infra.Agent.Server.ApplicationBase.Control (defaultAppData, runAppStateContext)
import PMS.Infra.Agent.Server.CoreModel.Constant (_DEFAULT_HANDLE_NAME)
import PMS.Infra.Agent.Server.CoreModel.Type
import PMS.Infra.Agent.Server.CoreModel.Utility (changeTo)

mockPortFunc :: ServerPortFunc
mockPortFunc = ServerPortFunc
  { _bindListenServerPortFunc  = \_ _ -> error "mockPortFunc: bind not expected"
  , _acceptConnServerPortFunc  = \_   -> error "mockPortFunc: accept not expected"
  , _closeSocketServerPortFunc = \_   -> return ()
  , _closeConnServerPortFunc   = \_   -> return ()
  }

dummyJsonRpc :: DM.JsonRpcRequest
dummyJsonRpc = DM.JsonRpcRequest
  { DM._jsonrpcJsonRpcRequest = "2.0"
  , DM._idJsonRpcRequest      = Just 1
  , DM._methodJsonRpcRequest  = "tools/call"
  , DM._paramsJsonRpcRequest  = Nothing
  }

mkDomainData :: IO DM.DomainData
mkDomainData = DM.defaultDomainData

-- | Create DomainData with a specific sandboxNetworks list.
-- Use this in tests that exercise acceptLoop to allow specific addresses.
mkDomainDataWithNetworks :: [String] -> IO DM.DomainData
mkDomainDataWithNetworks nets = do
  domDat <- DM.defaultDomainData
  return $ domDat & DM.sandboxNetworksDomainData .~ nets

mkListeningAppData :: IO (AppData, STM.TQueue ServerEvent)
mkListeningAppData = do
  evQ <- STM.newTQueueIO
  thd <- async (return ())
  let ctx = ListenerCtx
              { _listenSocketListenerCtx = error "mock socket"
              , _acceptThreadListenerCtx = thd
              , _eventQueueListenerCtx   = evQ
              }
  appDat <- defaultAppData mockPortFunc
  STM.atomically $ STM.swapTMVar (appDat^.listenerCtxAppData) (Just ctx)
  return (appDat, evQ)

mkConnectedAppData :: IO AppData
mkConnectedAppData = do
  (appDat, _) <- mkListeningAppData
  recvThd <- async (return ())
  recvQ <- STM.newTQueueIO
  let connCtx = ConnCtx
                  { _socketConnCtx     = error "mock socket"
                  , _recvThreadConnCtx = recvThd
                  , _recvQueueConnCtx  = recvQ
                  }
  STM.atomically $ do
    m <- STM.takeTMVar (appDat^.connCtxAppData)
    STM.putTMVar (appDat^.connCtxAppData) (Map.insert _DEFAULT_HANDLE_NAME connCtx m)
  return appDat

applyMaybeTransition :: Maybe StateTransition -> AppStateContext ()
applyMaybeTransition Nothing  = return ()
applyMaybeTransition (Just t) = do
  nextSt <- lift get >>= \case
    AppStateW IdleState      | t == IdleToListening      -> return $ AppStateW ListeningState
    AppStateW ListeningState | t == ListeningToConnected -> return $ AppStateW ConnectedState
    AppStateW ListeningState | t == ListeningToIdle      -> return $ AppStateW IdleState
    AppStateW ConnectedState | t == ConnectedToListening -> return $ AppStateW ListeningState
    AppStateW ConnectedState | t == ConnectedToIdle      -> return $ AppStateW IdleState
    _ -> error $ "applyMaybeTransition: invalid transition " ++ show t
  changeTo nextSt

runAction :: DM.DomainData
          -> AppData
          -> AppStateW
          -> AppStateContext (Maybe StateTransition)
          -> IO (Maybe DM.McpResponse, AppStateW)
runAction domDat appDat initSt ctx = do
  tcache <- newTimeCache "%Y"
  (logger, cleanup) <- newTimedFastLogger tcache LogNone
  result <- runAppStateContext domDat appDat initSt logger $ do
    mTrans <- ctx
    applyMaybeTransition mTrans
  cleanup
  case result of
    Left _          -> error "runAction: AppStateContext threw ErrorData"
    Right ((), nst) -> do
      mResp <- STM.atomically $ STM.tryReadTQueue (domDat^.DM.responseQueueDomainData)
      return (mResp, nst)

isIdleState :: AppStateW -> Bool
isIdleState (AppStateW IdleState) = True
isIdleState _                     = False

isListeningState :: AppStateW -> Bool
isListeningState (AppStateW ListeningState) = True
isListeningState _                          = False

isConnectedState :: AppStateW -> Bool
isConnectedState (AppStateW ConnectedState) = True
isConnectedState _                          = False

respIsError :: DM.McpResponse -> Bool
respIsError (DM.McpToolsCallResponse d) =
  d ^. DM.resultMcpToolsCallResponseData
    ^. DM.isErrorMcpToolsCallResponseResult
respIsError _ = True

respContent :: DM.McpResponse -> String
respContent (DM.McpToolsCallResponse d) =
  case d ^. DM.resultMcpToolsCallResponseData
         ^. DM.contentMcpToolsCallResponseResult of
    (c:_) -> c ^. DM.textMcpToolsCallResponseResultContent
    []    -> ""
respContent _ = ""

shouldBeSuccess :: Maybe DM.McpResponse -> Expectation
shouldBeSuccess = (`shouldSatisfy` maybe False (not . respIsError))

shouldBeError :: Maybe DM.McpResponse -> Expectation
shouldBeError = (`shouldSatisfy` maybe False respIsError)
