{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module PMS.Infra.Agent.Server.ApplicationBase.State.Connected.ServerRead where

import Control.Lens ((^.))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ask)
import System.Exit (ExitCode(..))
import qualified PMS.Domain.Model.DM.Type as DM
import qualified PMS.Domain.Model.DS.Utility as DM

import PMS.Infra.Agent.Server.CoreModel.Type
import qualified PMS.Infra.Agent.Server.ProjectedContext.Core as PC

instance IStateActivity ConnectedStateData ServerReadEventData where
  action = serverRead

serverRead :: AppState ConnectedStateData -> Event ServerReadEventData -> AppStateContext (Maybe StateTransition)
serverRead _ (ServerReadEvent (ServerReadEventData jsonRpc)) = do
  result <- PC.runProjectedContext PC.serverRead
  case result of
    Left err -> sendServerReadResponse jsonRpc (ExitFailure 1) "" err
    Right outStr -> sendServerReadResponse jsonRpc ExitSuccess outStr ""
  return noStateTransition

sendServerReadResponse :: DM.JsonRpcRequest -> ExitCode -> String -> String -> AppStateContext ()
sendServerReadResponse jsonRpc code outStr errStr = do
  domDat <- lift (lift (lift ask))
  let resQ = domDat^.DM.responseQueueDomainData
  liftIO $ DM.toolsCallResponse resQ jsonRpc code outStr errStr
