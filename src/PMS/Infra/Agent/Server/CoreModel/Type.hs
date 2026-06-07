{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module PMS.Infra.Agent.Server.CoreModel.Type where

import Control.Concurrent.Async (Async)
import Control.Lens (makeLenses)
import Control.Monad.Except (ExceptT)
import Control.Monad.Logger (LoggingT)
import Control.Monad.Reader (ReaderT)
import Control.Monad.Trans.State.Lazy (StateT)
import Data.Aeson.TH (Options(..), defaultOptions, deriveJSON)
import Data.Default (Default(..))
import Data.Map.Strict (Map)
import Network.Socket (Socket)
import qualified Control.Concurrent.STM as STM
import qualified PMS.Domain.Model.DM.TH as DM
import qualified PMS.Domain.Model.DM.Type as DM

data ServerPortFunc = ServerPortFunc
  { _bindListenServerPortFunc  :: String -> String -> IO Socket
  , _acceptConnServerPortFunc  :: Socket -> IO (Socket, String)
  , _closeSocketServerPortFunc :: Socket -> IO ()
  , _closeConnServerPortFunc   :: Socket -> IO ()
  }
makeLenses ''ServerPortFunc

data ServerEvent
  = ClientConnected
      { _handleNameServerEvent :: String
      , _remoteAddrServerEvent :: String
      }
  | ClientDisconnected
      { _handleNameServerEvent :: String
      }
  | BytesReceived
      { _handleNameServerEvent :: String
      , _bytesServerEvent      :: String
      }
  | ListenerStopped
  | ErrorOccurred
      { _messageServerEvent :: String
      }
  deriving (Show, Eq)
$(deriveJSON defaultOptions {fieldLabelModifier = DM.dropDataName "ServerEvent"} ''ServerEvent)
makeLenses ''ServerEvent

data ListenerCtx = ListenerCtx
  { _listenSocketListenerCtx :: Socket
  , _acceptThreadListenerCtx :: Async ()
  , _eventQueueListenerCtx   :: STM.TQueue ServerEvent
  }
makeLenses ''ListenerCtx

data ConnCtx = ConnCtx
  { _socketConnCtx     :: Socket
  , _recvThreadConnCtx :: Async ()
  , _recvQueueConnCtx  :: STM.TQueue String
  }
makeLenses ''ConnCtx

-- | '_connCtxAppData' is managed as a 'Map' to accommodate multiple connections in the future.
-- Currently, only the "default" key is used.
data AppData = AppData
  { _portFuncAppData    :: ServerPortFunc
  , _listenerCtxAppData :: STM.TMVar (Maybe ListenerCtx)
  , _connCtxAppData     :: STM.TMVar (Map String ConnCtx)
  }
makeLenses ''AppData

data ServerStatusResult = ServerStatusResult
  { _isListeningServerStatusResult :: Bool
  , _isConnectedServerStatusResult :: Bool
  , _listenAddrServerStatusResult  :: Maybe String
  } deriving (Show, Eq)
$(deriveJSON defaultOptions {fieldLabelModifier = DM.dropDataName "ServerStatusResult", omitNothingFields = True} ''ServerStatusResult)
makeLenses ''ServerStatusResult

data StateTransition
  = IdleToListening
  | ListeningToConnected
  | ConnectedToListening
  | ListeningToIdle
  | ConnectedToIdle
  deriving (Show, Eq)

noStateTransition :: Maybe StateTransition
noStateTransition = Nothing

data ServerListenToolParams = ServerListenToolParams
  { _hostServerListenToolParams :: Maybe String
  , _portServerListenToolParams :: String
  } deriving (Show, Read, Eq)
$(deriveJSON defaultOptions {fieldLabelModifier = DM.dropDataName "ServerListenToolParams", omitNothingFields = True} ''ServerListenToolParams)
makeLenses ''ServerListenToolParams

instance Default ServerListenToolParams where
  def = ServerListenToolParams Nothing ""

data ServerCloseToolParams = ServerCloseToolParams
  { _targetServerCloseToolParams :: Maybe String
  } deriving (Show, Read, Eq)
$(deriveJSON defaultOptions {fieldLabelModifier = DM.dropDataName "ServerCloseToolParams", omitNothingFields = True} ''ServerCloseToolParams)
makeLenses ''ServerCloseToolParams

instance Default ServerCloseToolParams where
  def = ServerCloseToolParams Nothing

data ServerStatusToolParams = ServerStatusToolParams
  deriving (Show, Read, Eq)
$(deriveJSON defaultOptions {fieldLabelModifier = DM.dropDataName "ServerStatusToolParams", omitNothingFields = True} ''ServerStatusToolParams)

instance Default ServerStatusToolParams where
  def = ServerStatusToolParams

data ServerEventsToolParams = ServerEventsToolParams
  { _maxEventsServerEventsToolParams :: Maybe Int
  } deriving (Show, Read, Eq)
$(deriveJSON defaultOptions {fieldLabelModifier = DM.dropDataName "ServerEventsToolParams", omitNothingFields = True} ''ServerEventsToolParams)
makeLenses ''ServerEventsToolParams

instance Default ServerEventsToolParams where
  def = ServerEventsToolParams Nothing


data ServerWriteToolParams = ServerWriteToolParams
  { _dataServerWriteToolParams :: String
  , _appendNewlineServerWriteToolParams :: Maybe Bool
  } deriving (Show, Read, Eq)
$(deriveJSON defaultOptions {fieldLabelModifier = DM.dropDataName "ServerWriteToolParams", omitNothingFields = True} ''ServerWriteToolParams)
makeLenses ''ServerWriteToolParams

instance Default ServerWriteToolParams where
  def = ServerWriteToolParams "" Nothing


data ServerWriteByteToolParams = ServerWriteByteToolParams
  { _dataServerWriteByteToolParams :: String
  } deriving (Show, Read, Eq)
$(deriveJSON defaultOptions {fieldLabelModifier = DM.dropDataName "ServerWriteByteToolParams", omitNothingFields = True} ''ServerWriteByteToolParams)
makeLenses ''ServerWriteByteToolParams

instance Default ServerWriteByteToolParams where
  def = ServerWriteByteToolParams ""

data EntryEventData = EntryEventData deriving (Show, Eq)
data ExitEventData = ExitEventData deriving (Show, Eq)
data TransitEventData = TransitEventData StateTransition deriving (Show, Eq)

data ServerListenEventData    = ServerListenEventData    DM.JsonRpcRequest ServerListenToolParams    deriving (Show, Eq)
data ServerCloseEventData     = ServerCloseEventData     DM.JsonRpcRequest                           deriving (Show, Eq)
data ServerStatusEventData    = ServerStatusEventData    DM.JsonRpcRequest                           deriving (Show, Eq)
data ServerEventsEventData    = ServerEventsEventData    DM.JsonRpcRequest ServerEventsToolParams    deriving (Show, Eq)
data ServerReadEventData      = ServerReadEventData      DM.JsonRpcRequest                           deriving (Show, Eq)
data ServerWriteEventData     = ServerWriteEventData     DM.JsonRpcRequest ServerWriteToolParams     deriving (Show, Eq)
data ServerReadByteEventData  = ServerReadByteEventData  DM.JsonRpcRequest                           deriving (Show, Eq)
data ServerWriteByteEventData = ServerWriteByteEventData DM.JsonRpcRequest ServerWriteByteToolParams deriving (Show, Eq)

-- | Internal event: contains both hdlName and remoteAddr
data InternalClientConnectedEventData    = InternalClientConnectedEventData    String String deriving (Show, Eq)
data InternalClientDisconnectedEventData = InternalClientDisconnectedEventData               deriving (Show, Eq)

data Event r where
  EntryEvent :: Event EntryEventData
  ExitEvent :: Event ExitEventData
  TransitEvent :: TransitEventData -> Event TransitEventData
  ServerListenEvent :: ServerListenEventData -> Event ServerListenEventData
  ServerCloseEvent :: ServerCloseEventData -> Event ServerCloseEventData
  ServerStatusEvent :: ServerStatusEventData -> Event ServerStatusEventData
  ServerEventsEvent :: ServerEventsEventData -> Event ServerEventsEventData
  ServerReadEvent :: ServerReadEventData -> Event ServerReadEventData
  ServerWriteEvent :: ServerWriteEventData -> Event ServerWriteEventData
  ServerReadByteEvent :: ServerReadByteEventData -> Event ServerReadByteEventData
  ServerWriteByteEvent :: ServerWriteByteEventData -> Event ServerWriteByteEventData
  InternalClientConnectedEvent :: InternalClientConnectedEventData -> Event InternalClientConnectedEventData
  InternalClientDisconnectedEvent :: InternalClientDisconnectedEventData -> Event InternalClientDisconnectedEventData

deriving instance Show r => Show (Event r)

data EventW = forall r. EventW (Event r)

data IdleStateData = IdleStateData deriving (Show)
data ListeningStateData = ListeningStateData deriving (Show)
data ConnectedStateData = ConnectedStateData deriving (Show)

data AppState s where
  IdleState :: AppState IdleStateData
  ListeningState :: AppState ListeningStateData
  ConnectedState :: AppState ConnectedStateData

deriving instance Show (AppState s)

data AppStateW = forall s. (IAppState s, Show s) => AppStateW (AppState s)

type AppStateContext =
  ExceptT DM.ErrorData
    (StateT AppStateW
      (ReaderT AppData
        (ReaderT DM.DomainData
          (LoggingT IO))))

type AppContext =
  ReaderT AppData
    (ReaderT DM.DomainData
      (ExceptT DM.ErrorData
        (LoggingT IO)))

class (Show s, Show r) => IStateActivity s r where
  action :: AppState s -> Event r -> AppStateContext (Maybe StateTransition)
  action _ (TransitEvent (TransitEventData t)) = return (Just t)
  action _ _ = return noStateTransition

class IAppState s where
  actionS :: AppState s -> EventW -> AppStateContext (Maybe StateTransition)

class IAppStateW s where
  actionSW :: s -> EventW -> AppStateContext (Maybe StateTransition)

instance IAppStateW AppStateW where
  actionSW (AppStateW a) r = actionS a r