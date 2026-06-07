{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module PMS.Infra.Agent.Server.ApplicationBase.State.Connected.ServerWrite where

import Control.Lens ((^.))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ask)
import System.Exit (ExitCode(..))
import qualified PMS.Domain.Model.DM.Type as DM
import qualified PMS.Domain.Model.DS.Utility as DM

import PMS.Infra.Agent.Server.CoreModel.Type
import qualified PMS.Infra.Agent.Server.ProjectedContext.Core as PC

instance IStateActivity ConnectedStateData ServerWriteEventData where
  action = serverWrite

serverWrite :: AppState ConnectedStateData -> Event ServerWriteEventData -> AppStateContext (Maybe StateTransition)
serverWrite _ (ServerWriteEvent (ServerWriteEventData jsonRpc params)) = do
  result <- PC.runProjectedContext (PC.serverWrite params)
  case result of
    Left err -> sendServerWriteResponse jsonRpc (ExitFailure 1) "" err
    Right () -> sendServerWriteResponse jsonRpc ExitSuccess "" ""
  return noStateTransition

sendServerWriteResponse :: DM.JsonRpcRequest -> ExitCode -> String -> String -> AppStateContext ()
sendServerWriteResponse jsonRpc code outStr errStr = do
  domDat <- lift (lift (lift ask))
  let resQ = domDat^.DM.responseQueueDomainData
  liftIO $ DM.toolsCallResponse resQ jsonRpc code outStr errStr
