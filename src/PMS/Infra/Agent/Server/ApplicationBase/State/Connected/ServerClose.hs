{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module PMS.Infra.Agent.Server.ApplicationBase.State.Connected.ServerClose where

import Control.Lens ((^.))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (logInfoS)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ask)
import System.Exit (ExitCode(..))
import qualified Data.Text as T
import qualified PMS.Domain.Model.DM.Type as DM
import qualified PMS.Domain.Model.DS.Utility as DM
import qualified PMS.Domain.Model.DM.Constant as DM

import PMS.Infra.Agent.Server.CoreModel.Type
import PMS.Infra.Agent.Server.ProjectedContext.Core
  ( runProjectedContext
  , serverCloseConnection
  )

instance IStateActivity ConnectedStateData ServerCloseEventData where
  action = serverClose

serverClose :: AppState ConnectedStateData -> Event ServerCloseEventData -> AppStateContext (Maybe StateTransition)
serverClose _ (ServerCloseEvent (ServerCloseEventData jsonRpc)) = do
  $logInfoS DM._LOGTAG "serverClose: called."
  result <- runProjectedContext serverCloseConnection
  case result of
    Left err -> do
      $logInfoS DM._LOGTAG $ T.pack $ "serverClose: failed. " ++ err
      sendServerCloseResponse jsonRpc (ExitFailure 1) "" err
      return noStateTransition
    Right () -> do
      $logInfoS DM._LOGTAG "serverClose: connection closed. transitioning to Listening."
      sendServerCloseResponse jsonRpc ExitSuccess "connection closed." ""
      return (Just ConnectedToListening)

sendServerCloseResponse :: DM.JsonRpcRequest -> ExitCode -> String -> String -> AppStateContext ()
sendServerCloseResponse jsonRpc code outStr errStr = do
  domDat <- lift (lift (lift ask))
  let resQ = domDat^.DM.responseQueueDomainData
  liftIO $ DM.toolsCallResponse resQ jsonRpc code outStr errStr
