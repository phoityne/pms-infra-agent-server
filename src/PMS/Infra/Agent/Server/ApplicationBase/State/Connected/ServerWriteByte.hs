{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module PMS.Infra.Agent.Server.ApplicationBase.State.Connected.ServerWriteByte where

import Control.Lens ((^.))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ask)
import System.Exit (ExitCode(..))
import qualified PMS.Domain.Model.DM.Type as DM
import qualified PMS.Domain.Model.DS.Utility as DM

import PMS.Infra.Agent.Server.CoreModel.Type
import qualified PMS.Infra.Agent.Server.ProjectedContext.Core as PC

instance IStateActivity ConnectedStateData ServerWriteByteEventData where
  action = serverWriteByte

serverWriteByte :: AppState ConnectedStateData -> Event ServerWriteByteEventData -> AppStateContext (Maybe StateTransition)
serverWriteByte _ (ServerWriteByteEvent (ServerWriteByteEventData jsonRpc params)) = do
  result <- PC.runProjectedContext (PC.serverWriteByte params)
  case result of
    Left err -> sendServerWriteByteResponse jsonRpc (ExitFailure 1) "" err
    Right () -> sendServerWriteByteResponse jsonRpc ExitSuccess "" ""
  return noStateTransition

sendServerWriteByteResponse :: DM.JsonRpcRequest -> ExitCode -> String -> String -> AppStateContext ()
sendServerWriteByteResponse jsonRpc code outStr errStr = do
  domDat <- lift (lift (lift ask))
  let resQ = domDat^.DM.responseQueueDomainData
  liftIO $ DM.toolsCallResponse resQ jsonRpc code outStr errStr
