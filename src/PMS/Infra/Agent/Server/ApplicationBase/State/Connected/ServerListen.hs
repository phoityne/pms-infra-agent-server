{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module PMS.Infra.Agent.Server.ApplicationBase.State.Connected.ServerListen where

import Control.Lens ((^.))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ask)
import System.Exit (ExitCode(..))
import qualified PMS.Domain.Model.DM.Type as DM
import qualified PMS.Domain.Model.DS.Utility as DM

import PMS.Infra.Agent.Server.CoreModel.Type

connectedServerListen :: AppState ConnectedStateData
                      -> Event ServerListenEventData
                      -> AppStateContext (Maybe StateTransition)
connectedServerListen _ (ServerListenEvent (ServerListenEventData jsonRpc _)) =
  invalidState jsonRpc "invalid state: already listening"

invalidState :: DM.JsonRpcRequest -> String -> AppStateContext (Maybe StateTransition)
invalidState jsonRpc msg = do
  sendInvalidStateResponse jsonRpc msg
  return noStateTransition

sendInvalidStateResponse :: DM.JsonRpcRequest -> String -> AppStateContext ()
sendInvalidStateResponse jsonRpc msg = do
  domDat <- lift (lift (lift ask))
  let resQ = domDat^.DM.responseQueueDomainData
  liftIO $ DM.toolsCallResponse resQ jsonRpc (ExitFailure 1) "" msg
