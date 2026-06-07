{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module PMS.Infra.Agent.Server.ApplicationBase.State.Connected.ServerReadByte where

import Control.Lens ((^.))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ask)
import System.Exit (ExitCode(..))
import qualified PMS.Domain.Model.DM.Type as DM
import qualified PMS.Domain.Model.DS.Utility as DM

import PMS.Infra.Agent.Server.CoreModel.Type
import qualified PMS.Infra.Agent.Server.ProjectedContext.Core as PC

instance IStateActivity ConnectedStateData ServerReadByteEventData where
  action = serverReadByte

serverReadByte :: AppState ConnectedStateData -> Event ServerReadByteEventData -> AppStateContext (Maybe StateTransition)
serverReadByte _ (ServerReadByteEvent (ServerReadByteEventData jsonRpc)) = do
  result <- PC.runProjectedContext PC.serverReadByte
  case result of
    Left err -> sendServerReadByteResponse jsonRpc (ExitFailure 1) "" err
    Right outStr -> sendServerReadByteResponse jsonRpc ExitSuccess outStr ""
  return noStateTransition

sendServerReadByteResponse :: DM.JsonRpcRequest -> ExitCode -> String -> String -> AppStateContext ()
sendServerReadByteResponse jsonRpc code outStr errStr = do
  domDat <- lift (lift (lift ask))
  let resQ = domDat^.DM.responseQueueDomainData
  liftIO $ DM.toolsCallResponse resQ jsonRpc code outStr errStr
