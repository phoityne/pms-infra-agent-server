{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

module PMS.Infra.Agent.Server.ApplicationBase.Control where

import Control.Lens ((^.))
import Control.Monad (void)
import Control.Monad.Except (catchError, liftEither, runExceptT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (runReaderT)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ask)
import Control.Monad.Trans.State.Lazy (get, runStateT)
import Data.Aeson (eitherDecode)
import Data.Conduit (ConduitT, await, awaitForever, runConduit, yield, (.|))
import Data.Void (Void)
import System.Exit (ExitCode(..))
import System.IO (hPutStrLn, stderr)
import System.Log.FastLogger (TimedFastLogger)
import qualified Control.Concurrent.STM as STM
import qualified Control.Exception.Safe as E
import qualified Data.Map.Strict as Map
import qualified PMS.Domain.Model.DM.Type as DM
import qualified PMS.Domain.Model.DS.Utility as DM

import PMS.Infra.Agent.Server.CoreModel.Constant
import PMS.Infra.Agent.Server.CoreModel.TH
import PMS.Infra.Agent.Server.CoreModel.Type
import PMS.Infra.Agent.Server.ApplicationBase.State.Connected ()
import PMS.Infra.Agent.Server.ApplicationBase.State.Idle ()
import PMS.Infra.Agent.Server.ApplicationBase.State.Listening ()
import PMS.Infra.Agent.Server.Interface.Network

$(funcTH_transit)

runAppStateContext
  :: DM.DomainData
  -> AppData
  -> AppStateW
  -> TimedFastLogger
  -> AppStateContext a
  -> IO (Either DM.ErrorData (a, AppStateW))
runAppStateContext domDat appDat initSt logger ctx = do
  (result, nextSt) <-
    DM.runFastLoggerT domDat logger
      $ flip runReaderT domDat
      $ flip runReaderT appDat
      $ runStateT (runExceptT ctx) initSt
  return $ case result of
    Left e -> Left e
    Right a -> Right (a, nextSt)

run :: DM.DomainContext ()
run domDat = do
  hPutStrLn stderr "[INFO] PMS.Infra.Agent.Server.ApplicationBase.Control.run called."
  appDat <- defaultAppData defaultServerPortFunc
  runWithAppData appDat domDat

defaultAppData :: ServerPortFunc -> IO AppData
defaultAppData portFunc = AppData portFunc
  <$> STM.newTMVarIO Nothing
  <*> STM.newTMVarIO Map.empty

runWithAppData :: AppData -> DM.DomainContext ()
runWithAppData appDat domDat = do
  logDat <- DM.createLogger domDat _LOG_FILE_NAME
  runWithLogger logDat appDat domDat

runWithLogger :: (TimedFastLogger, IO ()) -> AppData -> DM.DomainContext ()
runWithLogger (logger, finalizeLogger) appDat domDat =
  flip E.catchAny exception
    $ flip E.finally finalize
    $ runAppStateContext domDat appDat (AppStateW IdleState) logger app
    >>= \case
      Right (x, _) -> return x
      Left e -> errorEnd e
  where
    finalize = do
      hPutStrLn stderr "-----------------------------------------------------------------------------"
      hPutStrLn stderr "[INFO] PMS.Infra.Agent.Server.ApplicationBase.Control.run finalize called."
      finalizeLogger
      hPutStrLn stderr "-----------------------------------------------------------------------------"

    exception e = do
      hPutStrLn stderr "-----------------------------------------------------------------------------"
      hPutStrLn stderr "[ERROR] PMS.Infra.Agent.Server.ApplicationBase.Control.run exception occurred."
      hPutStrLn stderr $ show e
      hPutStrLn stderr "-----------------------------------------------------------------------------"
      E.throwIO e

    errorEnd e = do
      hPutStrLn stderr "-----------------------------------------------------------------------------"
      hPutStrLn stderr "[ERROR] PMS.Infra.Agent.Server.ApplicationBase.Control.run end with error."
      hPutStrLn stderr $ show e
      hPutStrLn stderr "-----------------------------------------------------------------------------"

app :: AppStateContext ()
app = do
  void $ actionSW (AppStateW IdleState) (EventW EntryEvent)
  runConduit pipeline
  st <- lift get
  void $ actionSW st (EventW ExitEvent)

pipeline :: ConduitT () Void AppStateContext ()
pipeline = src .| work .| sink

src :: ConduitT () DM.AgentServerCommand AppStateContext ()
src = do
  domDat <- lift (lift (lift (lift ask)))
  let queue = domDat^.DM.agentServerQueueDomainData
  cmd <- liftIO $ STM.atomically $ STM.readTQueue queue
  yield cmd
  src

work :: ConduitT DM.AgentServerCommand EventW AppStateContext ()
work = awaitForever $ \cmd ->
  flip catchError (errHdl cmd) $ do
    ev <- lift (toEventW cmd)
    yield ev
  where
    errHdl :: DM.AgentServerCommand
           -> DM.ErrorData
           -> ConduitT DM.AgentServerCommand EventW AppStateContext ()
    errHdl cmd msg = do
      let jsonRpc = DM.getJsonRpcAgentServerCommand cmd
      lift $ errorToolsCallResponse jsonRpc msg

toEventW :: DM.AgentServerCommand -> AppStateContext EventW
toEventW (DM.AgentServerListenCommand dat) = do
  params <- liftEither $ eitherDecode $ DM.unRawJsonByteString
              $ dat^.DM.argumentsAgentServerListenCommandData
  return $ EventW $ ServerListenEvent
    $ ServerListenEventData (dat^.DM.jsonrpcAgentServerListenCommandData) params
toEventW (DM.AgentServerCloseCommand dat) =
  return $ EventW $ ServerCloseEvent
    $ ServerCloseEventData (dat^.DM.jsonrpcAgentServerCloseCommandData)
toEventW (DM.AgentServerStatusCommand dat) =
  return $ EventW $ ServerStatusEvent
    $ ServerStatusEventData (dat^.DM.jsonrpcAgentServerStatusCommandData)
toEventW (DM.AgentServerEventsCommand dat) = do
  params <- liftEither $ eitherDecode $ DM.unRawJsonByteString
              $ dat^.DM.argumentsAgentServerEventsCommandData
  return $ EventW $ ServerEventsEvent
    $ ServerEventsEventData (dat^.DM.jsonrpcAgentServerEventsCommandData) params
toEventW (DM.AgentServerReadCommand dat) =
  return $ EventW $ ServerReadEvent
    $ ServerReadEventData (dat^.DM.jsonrpcAgentServerReadCommandData)
toEventW (DM.AgentServerWriteCommand dat) = do
  params <- liftEither $ eitherDecode $ DM.unRawJsonByteString
              $ dat^.DM.argumentsAgentServerWriteCommandData
  return $ EventW $ ServerWriteEvent
    $ ServerWriteEventData (dat^.DM.jsonrpcAgentServerWriteCommandData) params
toEventW (DM.AgentServerReadByteCommand dat) =
  return $ EventW $ ServerReadByteEvent
    $ ServerReadByteEventData (dat^.DM.jsonrpcAgentServerReadByteCommandData)
toEventW (DM.AgentServerWriteByteCommand dat) = do
  params <- liftEither $ eitherDecode $ DM.unRawJsonByteString
              $ dat^.DM.argumentsAgentServerWriteByteCommandData
  return $ EventW $ ServerWriteByteEvent
    $ ServerWriteByteEventData (dat^.DM.jsonrpcAgentServerWriteByteCommandData) params
toEventW (DM.AgentServerInternalClientConnectedCommand dat) =
  return $ EventW $ InternalClientConnectedEvent
    $ InternalClientConnectedEventData
        (dat^.DM.handleNameAgentServerInternalClientConnectedCommandData)
        (dat^.DM.remoteAddrAgentServerInternalClientConnectedCommandData)
toEventW (DM.AgentServerInternalClientDisconnectedCommand _) =
  return $ EventW $ InternalClientDisconnectedEvent
    $ InternalClientDisconnectedEventData

errorToolsCallResponse :: DM.JsonRpcRequest -> DM.ErrorData -> AppStateContext ()
errorToolsCallResponse jsonRpc msg = do
  domDat <- lift (lift (lift ask))
  let resQ = domDat^.DM.responseQueueDomainData
  liftIO $ DM.toolsCallResponse resQ jsonRpc (ExitFailure 1) "" msg

sink :: ConduitT EventW Void AppStateContext ()
sink = await >>= \case
  Nothing -> return ()
  Just ev -> flip catchError errHdl $ do
    lift (go ev)
    sink
  where
    errHdl :: DM.ErrorData -> ConduitT EventW Void AppStateContext ()
    errHdl msg = do
      msg `seq` return ()
      sink

    go :: EventW -> AppStateContext ()
    go ev = do
      st <- lift get
      result <- actionSW st ev
      case result of
        Nothing -> return ()
        Just t -> transit t
