{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module PMS.Infra.Agent.Server.ApplicationBase.State.Connected.ServerEvents where

import Control.Lens ((^.))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ask)
import System.Exit (ExitCode(..))
import qualified PMS.Domain.Model.DM.Type as DM
import qualified PMS.Domain.Model.DS.Utility as DM

import PMS.Infra.Agent.Server.CoreModel.Type
import PMS.Infra.Agent.Server.ProjectedContext.Core
  ( runProjectedContext
  , serverEvents
  , formatEvents
  )

instance IStateActivity ConnectedStateData ServerEventsEventData where
  action = connectedServerEvents

-- | Handle agent-server-events in the Connected state.
connectedServerEvents :: AppState ConnectedStateData -> Event ServerEventsEventData -> AppStateContext (Maybe StateTransition)
connectedServerEvents _ (ServerEventsEvent (ServerEventsEventData jsonRpc params)) = do
  result <- runProjectedContext (serverEvents params)
  case result of
    Left err -> do
      sendConnectedEventsResponse jsonRpc (ExitFailure 1) "" err
      return noStateTransition
    Right evs -> do
      sendConnectedEventsResponse jsonRpc ExitSuccess (formatEvents evs) ""
      return noStateTransition

sendConnectedEventsResponse :: DM.JsonRpcRequest -> ExitCode -> String -> String -> AppStateContext ()
sendConnectedEventsResponse jsonRpc code outStr errStr = do
  domDat <- lift (lift (lift ask))
  let resQ = domDat^.DM.responseQueueDomainData
  liftIO $ DM.toolsCallResponse resQ jsonRpc code outStr errStr
