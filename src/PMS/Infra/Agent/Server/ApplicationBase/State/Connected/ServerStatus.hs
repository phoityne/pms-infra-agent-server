{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module PMS.Infra.Agent.Server.ApplicationBase.State.Connected.ServerStatus where

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
  , serverStatus
  , formatStatus
  )

instance IStateActivity ConnectedStateData ServerStatusEventData where
  action = connectedServerStatus

-- | Handle agent-server-status in the Connected state.
connectedServerStatus :: AppState ConnectedStateData -> Event ServerStatusEventData -> AppStateContext (Maybe StateTransition)
connectedServerStatus _ (ServerStatusEvent (ServerStatusEventData jsonRpc)) = do
  result <- runProjectedContext serverStatus
  case result of
    Left err -> do
      sendConnectedResponse jsonRpc (ExitFailure 1) "" err
      return noStateTransition
    Right res -> do
      sendConnectedResponse jsonRpc ExitSuccess (formatStatus res) ""
      return noStateTransition

sendConnectedResponse :: DM.JsonRpcRequest -> ExitCode -> String -> String -> AppStateContext ()
sendConnectedResponse jsonRpc code outStr errStr = do
  domDat <- lift (lift (lift ask))
  let resQ = domDat^.DM.responseQueueDomainData
  liftIO $ DM.toolsCallResponse resQ jsonRpc code outStr errStr
