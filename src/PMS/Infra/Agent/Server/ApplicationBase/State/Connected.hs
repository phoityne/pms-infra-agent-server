{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module PMS.Infra.Agent.Server.ApplicationBase.State.Connected where

import PMS.Infra.Agent.Server.ApplicationBase.State.Connected.InternalClientDisconnected ()
import PMS.Infra.Agent.Server.ApplicationBase.State.Connected.ServerClose ()
import PMS.Infra.Agent.Server.ApplicationBase.State.Connected.ServerEvents ()
import PMS.Infra.Agent.Server.ApplicationBase.State.Connected.ServerListen (connectedServerListen)
import PMS.Infra.Agent.Server.ApplicationBase.State.Connected.ServerRead ()
import PMS.Infra.Agent.Server.ApplicationBase.State.Connected.ServerReadByte ()
import PMS.Infra.Agent.Server.ApplicationBase.State.Connected.ServerStatus ()
import PMS.Infra.Agent.Server.ApplicationBase.State.Connected.ServerWrite ()
import PMS.Infra.Agent.Server.ApplicationBase.State.Connected.ServerWriteByte ()
import PMS.Infra.Agent.Server.CoreModel.TH
import PMS.Infra.Agent.Server.CoreModel.Type

instanceTH_IAppState ''ConnectedStateData

instance IStateActivity ConnectedStateData EntryEventData
instance IStateActivity ConnectedStateData ExitEventData
instance IStateActivity ConnectedStateData TransitEventData
instance IStateActivity ConnectedStateData ServerListenEventData where
  action = connectedServerListen

instance IStateActivity ConnectedStateData InternalClientConnectedEventData
