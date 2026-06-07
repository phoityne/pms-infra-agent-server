{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE OverloadedStrings #-}


module PMS.Infra.Agent.Server.ApplicationBase.State.Listening where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (logInfoS)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ask)
import System.Exit (ExitCode(..))
import System.IO (hPutStrLn, stderr)
import qualified Data.Text as T
import qualified PMS.Domain.Model.DM.Type as DM
import qualified PMS.Domain.Model.DS.Utility as DM
import qualified PMS.Domain.Model.DM.Constant as DM
import Control.Lens ((^.))

import PMS.Infra.Agent.Server.CoreModel.TH
import PMS.Infra.Agent.Server.CoreModel.Type
import PMS.Infra.Agent.Server.ProjectedContext.Core
  ( runProjectedContext
  , serverStatus
  , serverEvents
  , serverCloseListener
  , formatStatus
  , formatEvents
  )

instanceTH_IAppState ''ListeningStateData

instance IStateActivity ListeningStateData EntryEventData
instance IStateActivity ListeningStateData ExitEventData
instance IStateActivity ListeningStateData TransitEventData
instance IStateActivity ListeningStateData ServerListenEventData where
  action = listeningServerListen

instance IStateActivity ListeningStateData ServerCloseEventData where
  action = listeningServerClose

instance IStateActivity ListeningStateData ServerStatusEventData where
  action = listeningServerStatus

instance IStateActivity ListeningStateData ServerEventsEventData where
  action = listeningServerEvents

instance IStateActivity ListeningStateData ServerReadEventData where
  action = listeningServerRead

instance IStateActivity ListeningStateData ServerWriteEventData where
  action = listeningServerWrite

instance IStateActivity ListeningStateData ServerReadByteEventData where
  action = listeningServerReadByte

instance IStateActivity ListeningStateData ServerWriteByteEventData where
  action = listeningServerWriteByte

instance IStateActivity ListeningStateData InternalClientConnectedEventData where
  action = listeningInternalClientConnected

instance IStateActivity ListeningStateData InternalClientDisconnectedEventData

-- | Handle agent-server-close in the Listening state.
-- Cancels the accept thread, closes the listen socket, and transitions to Idle.
listeningServerClose :: AppState ListeningStateData -> Event ServerCloseEventData -> AppStateContext (Maybe StateTransition)
listeningServerClose _ (ServerCloseEvent (ServerCloseEventData jsonRpc)) = do
  $logInfoS DM._LOGTAG "listeningServerClose: called."
  result <- runProjectedContext serverCloseListener
  case result of
    Left err -> do
      $logInfoS DM._LOGTAG $ T.pack $ "listeningServerClose: failed. " ++ err
      sendListeningResponse jsonRpc (ExitFailure 1) "" err
      return noStateTransition
    Right () -> do
      $logInfoS DM._LOGTAG "listeningServerClose: listener closed. transitioning to Idle."
      sendListeningResponse jsonRpc ExitSuccess "listener closed." ""
      return (Just ListeningToIdle)

-- | Handle agent-server-status in the Listening state.
-- Returns JSON-encoded ServerStatusResult via MCP response.
listeningServerStatus :: AppState ListeningStateData -> Event ServerStatusEventData -> AppStateContext (Maybe StateTransition)
listeningServerStatus _ (ServerStatusEvent (ServerStatusEventData jsonRpc)) = do
  result <- runProjectedContext serverStatus
  case result of
    Left err -> do
      sendListeningResponse jsonRpc (ExitFailure 1) "" err
      return noStateTransition
    Right res -> do
      sendListeningResponse jsonRpc ExitSuccess (formatStatus res) ""
      return noStateTransition

-- | Handle agent-server-events in the Listening state.
-- Drains the event queue and returns the events as a JSON array.
listeningServerEvents :: AppState ListeningStateData -> Event ServerEventsEventData -> AppStateContext (Maybe StateTransition)
listeningServerEvents _ (ServerEventsEvent (ServerEventsEventData jsonRpc params)) = do
  result <- runProjectedContext (serverEvents params)
  case result of
    Left err -> do
      sendListeningResponse jsonRpc (ExitFailure 1) "" err
      return noStateTransition
    Right evs -> do
      sendListeningResponse jsonRpc ExitSuccess (formatEvents evs) ""
      return noStateTransition

-- | Handle InternalClientConnected in the Listening state.
-- Logs the event and transitions to Connected.
listeningInternalClientConnected :: AppState ListeningStateData -> Event InternalClientConnectedEventData -> AppStateContext (Maybe StateTransition)
listeningInternalClientConnected _ (InternalClientConnectedEvent (InternalClientConnectedEventData hdlName addr)) = do
  liftIO $ hPutStrLn stderr $ "[INFO] client connected: " ++ hdlName ++ " from " ++ addr
  return (Just ListeningToConnected)

listeningServerListen :: AppState ListeningStateData
                      -> Event ServerListenEventData
                      -> AppStateContext (Maybe StateTransition)
listeningServerListen _ (ServerListenEvent (ServerListenEventData jsonRpc _)) =
  invalidState jsonRpc "invalid state: already listening"

listeningServerRead :: AppState ListeningStateData
                    -> Event ServerReadEventData
                    -> AppStateContext (Maybe StateTransition)
listeningServerRead _ (ServerReadEvent (ServerReadEventData jsonRpc)) =
  invalidState jsonRpc "invalid state: not connected"

listeningServerWrite :: AppState ListeningStateData
                     -> Event ServerWriteEventData
                     -> AppStateContext (Maybe StateTransition)
listeningServerWrite _ (ServerWriteEvent (ServerWriteEventData jsonRpc _)) =
  invalidState jsonRpc "invalid state: not connected"

listeningServerReadByte :: AppState ListeningStateData
                        -> Event ServerReadByteEventData
                        -> AppStateContext (Maybe StateTransition)
listeningServerReadByte _ (ServerReadByteEvent (ServerReadByteEventData jsonRpc)) =
  invalidState jsonRpc "invalid state: not connected"

listeningServerWriteByte :: AppState ListeningStateData
                         -> Event ServerWriteByteEventData
                         -> AppStateContext (Maybe StateTransition)
listeningServerWriteByte _ (ServerWriteByteEvent (ServerWriteByteEventData jsonRpc _)) =
  invalidState jsonRpc "invalid state: not connected"

invalidState :: DM.JsonRpcRequest -> String -> AppStateContext (Maybe StateTransition)
invalidState jsonRpc msg = do
  sendListeningResponse jsonRpc (ExitFailure 1) "" msg
  return noStateTransition

-- | Send an MCP tools-call response to the domain response queue.
sendListeningResponse :: DM.JsonRpcRequest -> ExitCode -> String -> String -> AppStateContext ()
sendListeningResponse jsonRpc code outStr errStr = do
  domDat <- lift (lift (lift ask))
  let resQ = domDat^.DM.responseQueueDomainData
  liftIO $ DM.toolsCallResponse resQ jsonRpc code outStr errStr
