module PMS.Infra.Agent.Server.CoreModel.Utility where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Lazy (get, modify)

import PMS.Infra.Agent.Server.CoreModel.Type

changeTo :: AppStateW -> AppStateContext ()
changeTo nextSt = do
  curSt <- lift get
  _ <- actionSW curSt (EventW ExitEvent)
  _ <- actionSW nextSt (EventW EntryEvent)
  lift $ modify (\_ -> nextSt)
