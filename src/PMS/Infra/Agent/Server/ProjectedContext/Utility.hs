module PMS.Infra.Agent.Server.ProjectedContext.Utility where

import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import System.Exit (ExitCode(..))
import qualified PMS.Domain.Model.DM.Type as DM

import PMS.Infra.Agent.Server.CoreModel.Type

liftIOE :: IO a -> AppContext a
liftIOE = liftIO

errorToolsCallResponse :: DM.JsonRpcRequest -> String -> AppContext ()
errorToolsCallResponse jsonRpc msg = do
  _ <- jsonRpc `seq` msg `seq` return ()
  throwError msg

toolsCallError :: DM.JsonRpcRequest -> String -> IO ()
toolsCallError jsonRpc msg =
  jsonRpc `seq` msg `seq` return ()

exitFailureCode :: ExitCode
exitFailureCode = ExitFailure 1
