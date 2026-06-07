{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module PMS.Infra.Agent.Server.ApplicationBase.State.Connected.InternalClientDisconnected where

import Control.Monad.IO.Class (liftIO)
import System.IO (hPutStrLn, stderr)

import PMS.Infra.Agent.Server.CoreModel.Type
import PMS.Infra.Agent.Server.ProjectedContext.Core
  ( runProjectedContext
  , serverCloseConnection
  )

instance IStateActivity ConnectedStateData InternalClientDisconnectedEventData where
  action = connectedInternalClientDisconnected

-- | Handle InternalClientDisconnected in the Connected state.
-- Clears the ConnCtx map entry and transitions back to Listening.
connectedInternalClientDisconnected :: AppState ConnectedStateData -> Event InternalClientDisconnectedEventData -> AppStateContext (Maybe StateTransition)
connectedInternalClientDisconnected _ (InternalClientDisconnectedEvent InternalClientDisconnectedEventData) = do
  liftIO $ hPutStrLn stderr "[INFO] client disconnected, clearing ConnCtx and returning to Listening state."
  -- Clear the ConnCtx map entry so serverStatus reflects the correct state.
  _ <- runProjectedContext serverCloseConnection
  return (Just ConnectedToListening)
