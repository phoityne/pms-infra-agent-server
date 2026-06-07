{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module PMS.Infra.Agent.Server.ApplicationBase.State.Idle where

import Control.Lens ((^.))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ask)
import System.Exit (ExitCode(..))
import qualified Control.Concurrent.STM as STM
import qualified PMS.Domain.Model.DM.Type as DM
import qualified PMS.Domain.Model.DS.Utility as DM

import PMS.Infra.Agent.Server.CoreModel.TH
import PMS.Infra.Agent.Server.CoreModel.Type
import PMS.Infra.Agent.Server.ProjectedContext.Core
  ( runProjectedContext
  , runProjectedContextWithCmdQ
  , serverListen
  , serverStatus
  , serverEvents
  , formatStatus
  , formatEvents
  )

instanceTH_IAppState ''IdleStateData

instance IStateActivity IdleStateData EntryEventData
instance IStateActivity IdleStateData ExitEventData
instance IStateActivity IdleStateData TransitEventData

instance IStateActivity IdleStateData ServerListenEventData where
  action = idleServerListen

instance IStateActivity IdleStateData ServerCloseEventData where
  action = idleServerClose

instance IStateActivity IdleStateData ServerStatusEventData where
  action = idleServerStatus

instance IStateActivity IdleStateData ServerEventsEventData where
  action = idleServerEvents

instance IStateActivity IdleStateData ServerReadEventData where
  action = idleServerRead

instance IStateActivity IdleStateData ServerWriteEventData where
  action = idleServerWrite

instance IStateActivity IdleStateData ServerReadByteEventData where
  action = idleServerReadByte

instance IStateActivity IdleStateData ServerWriteByteEventData where
  action = idleServerWriteByte

instance IStateActivity IdleStateData InternalClientConnectedEventData
instance IStateActivity IdleStateData InternalClientDisconnectedEventData

-- | Handle agent-server-status in the Idle state.
-- Returns JSON-encoded ServerStatusResult (isListening=false, isConnected=false).
idleServerStatus :: AppState IdleStateData
                 -> Event ServerStatusEventData
                 -> AppStateContext (Maybe StateTransition)
idleServerStatus _ (ServerStatusEvent (ServerStatusEventData jsonRpc)) = do
  result <- runProjectedContext serverStatus
  case result of
    Left err -> do
      sendResponse jsonRpc (ExitFailure 1) "" err
      return noStateTransition
    Right res -> do
      sendResponse jsonRpc ExitSuccess (formatStatus res) ""
      return noStateTransition

-- | Handle agent-server-events in the Idle state.
-- No listener is active so the event queue is always empty; returns [].
idleServerEvents :: AppState IdleStateData
                 -> Event ServerEventsEventData
                 -> AppStateContext (Maybe StateTransition)
idleServerEvents _ (ServerEventsEvent (ServerEventsEventData jsonRpc params)) = do
  result <- runProjectedContext (serverEvents params)
  case result of
    Left err -> do
      sendResponse jsonRpc (ExitFailure 1) "" err
      return noStateTransition
    Right evs -> do
      sendResponse jsonRpc ExitSuccess (formatEvents evs) ""
      return noStateTransition

-- | Handle agent-server-listen in the Idle state.
-- Delegates to ProjectedContext.serverListen via runProjectedContextWithCmdQ.
-- Retrieves sandboxNetworks from DomainData and passes it to serverListen.
-- On success: stores ListenerCtx in AppData and transitions to Listening.
-- On failure or already-listening: sends an MCP error response and stays in Idle.
idleServerListen :: AppState IdleStateData
                 -> Event ServerListenEventData
                 -> AppStateContext (Maybe StateTransition)
idleServerListen _ (ServerListenEvent (ServerListenEventData jsonRpc params)) = do
  appDat <- lift (lift ask)
  -- Check whether a listener is already active.
  mCtx <- liftIO $ STM.atomically $ STM.readTMVar (appDat^.listenerCtxAppData)
  case mCtx of
    Just _ -> do
      sendResponse jsonRpc (ExitFailure 1) "" "already listening."
      return noStateTransition
    Nothing -> do
      -- Retrieve sandboxNetworks from DomainData at the ApplicationBase boundary.
      -- Only [String] is passed to ProjectedContext; DomainData is not leaked inward.
      domDat <- lift (lift (lift ask))
      let sandboxNets = domDat^.DM.sandboxNetworksDomainData
      result <- runProjectedContextWithCmdQ (serverListen params sandboxNets)
      case result of
        Left err -> do
          sendResponse jsonRpc (ExitFailure 1) "" err
          return noStateTransition
        Right listenerCtx -> do
          _ <- liftIO $ STM.atomically $
            STM.swapTMVar (appDat^.listenerCtxAppData) (Just listenerCtx)
          sendResponse jsonRpc ExitSuccess "listening." ""
          return (Just IdleToListening)

idleServerClose :: AppState IdleStateData
                -> Event ServerCloseEventData
                -> AppStateContext (Maybe StateTransition)
idleServerClose _ (ServerCloseEvent (ServerCloseEventData jsonRpc)) =
  invalidState jsonRpc "invalid state: close requires Listening or Connected"

idleServerRead :: AppState IdleStateData
               -> Event ServerReadEventData
               -> AppStateContext (Maybe StateTransition)
idleServerRead _ (ServerReadEvent (ServerReadEventData jsonRpc)) =
  invalidState jsonRpc "invalid state: not connected"

idleServerWrite :: AppState IdleStateData
                -> Event ServerWriteEventData
                -> AppStateContext (Maybe StateTransition)
idleServerWrite _ (ServerWriteEvent (ServerWriteEventData jsonRpc _)) =
  invalidState jsonRpc "invalid state: not connected"

idleServerReadByte :: AppState IdleStateData
                   -> Event ServerReadByteEventData
                   -> AppStateContext (Maybe StateTransition)
idleServerReadByte _ (ServerReadByteEvent (ServerReadByteEventData jsonRpc)) =
  invalidState jsonRpc "invalid state: not connected"

idleServerWriteByte :: AppState IdleStateData
                    -> Event ServerWriteByteEventData
                    -> AppStateContext (Maybe StateTransition)
idleServerWriteByte _ (ServerWriteByteEvent (ServerWriteByteEventData jsonRpc _)) =
  invalidState jsonRpc "invalid state: not connected"

invalidState :: DM.JsonRpcRequest -> String -> AppStateContext (Maybe StateTransition)
invalidState jsonRpc msg = do
  sendResponse jsonRpc (ExitFailure 1) "" msg
  return noStateTransition

-- | Send an MCP tools-call response to the domain response queue.
sendResponse :: DM.JsonRpcRequest -> ExitCode -> String -> String -> AppStateContext ()
sendResponse jsonRpc code outStr errStr = do
  domDat <- lift (lift (lift ask))
  let resQ = domDat^.DM.responseQueueDomainData
  liftIO $ DM.toolsCallResponse resQ jsonRpc code outStr errStr
