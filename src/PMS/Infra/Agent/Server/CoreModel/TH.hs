{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module PMS.Infra.Agent.Server.CoreModel.TH
  ( funcTH_transit
  , instanceTH_IAppState
  ) where

import Control.Monad (when)
import Control.Monad.Except (throwError)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Lazy (get)
import Language.Haskell.TH
import qualified Data.Text as T
import qualified GHC.Base
import qualified GHC.Show

import PMS.Infra.Agent.Server.CoreModel.Type
import PMS.Infra.Agent.Server.CoreModel.Utility

instanceTH_IAppState :: Name -> Q [Dec]
instanceTH_IAppState stName = do
  ns <- getGadtsContNames ''Event
  clauseList <- mapM go ns
  return [InstanceD Nothing [] (AppT (ConT ''IAppState) (ConT stName)) [FunD 'actionS clauseList]]
  where
    go n = do
      s <- newName "s"
      r <- newName "r"
      return $
        Clause
          [VarP s, ConP 'EventW [] [AsP r (RecP n [])]]
          (NormalB (AppE (AppE (VarE 'action) (VarE s)) (VarE r)))
          []

    getGadtsContNames :: Name -> Q [Name]
    getGadtsContNames n = reify n >>= \case
      TyConI (DataD _ _ _ _ cs _) -> mapM go' cs
      x -> fail $ "[ERROR] can not get data constructor. " ++ show x

    go' (GadtC [name] _ _) = return name
    go' x = fail $ "[ERROR] can not get gadts data constructor. " ++ show x

funcTH_transit :: Q [Dec]
funcTH_transit = do
  fname <- newName "transit"
  cons <- getContNames ''StateTransition
  clauses <- mapM makeClause cons

  return
    [ SigD fname (AppT (AppT ArrowT (ConT ''StateTransition)) (AppT (ConT ''AppStateContext) (TupleT 0)))
    , FunD fname clauses
    ]
  where
    getContNames :: Name -> Q [Name]
    getContNames n = reify n >>= \case
      TyConI (DataD _ _ _ _ cs _) -> mapM go cs
      x -> fail $ "[ERROR] can not get data constructor. " ++ show x

    go (NormalC x _) = return x
    go x = fail $ "[ERROR] can not get data constructor. " ++ show x

    makeClause :: Name -> Q Clause
    makeClause n = do
      x <- newName "x"
      (curSt, nextSt) <- getStName n
      let liftGet = AppE (VarE 'lift) (VarE 'get)
      return $
        Clause
          [ConP n [] []]
          ( NormalB
              ( InfixE
                  (Just liftGet)
                  (VarE '(GHC.Base.>>=))
                  ( Just
                      ( LamCaseE
                          [ Match
                              (ConP 'AppStateW [] [ConP curSt [] []])
                              ( NormalB
                                  ( InfixE
                                      (Just (UnboundVarE 'changeTo))
                                      (VarE '(GHC.Base.$))
                                      (Just (AppE (ConE 'AppStateW) (ConE nextSt)))
                                  )
                              )
                              []
                          , Match
                              (ConP 'AppStateW [] [VarP x])
                              ( NormalB
                                  ( AppE
                                      (VarE 'throwError)
                                      ( InfixE
                                          (Just (LitE (StringL "invalid state transition. trans:")))
                                          (VarE '(GHC.Base.++))
                                          ( Just
                                              ( InfixE
                                                  (Just (AppE (VarE 'GHC.Show.show) (ConE n)))
                                                  (VarE '(GHC.Base.++))
                                                  ( Just
                                                      ( InfixE
                                                          (Just (LitE (StringL ", curSt:")))
                                                          (VarE '(GHC.Base.++))
                                                          (Just (AppE (VarE 'GHC.Show.show) (VarE x)))
                                                      )
                                                  )
                                              )
                                          )
                                      )
                                  )
                              )
                              []
                          ]
                      )
                  )
              )
          )
          []

    getStName :: Name -> Q (Name, Name)
    getStName n = do
      let modName = "PMS.Infra.Agent.Server.CoreModel.Type."
          stStrs = T.splitOn "To" $ T.replace (T.pack modName) "" $ T.pack $ show n
      when (2 /= length stStrs) $
        fail $ "[ERROR] invalid StateTransition constructor. " ++ show n

      let curSt = mkName $ T.unpack $ T.pack modName `T.append` head stStrs `T.append` "State"
          nextSt = mkName $ T.unpack $ T.pack modName `T.append` last stStrs `T.append` "State"

      return (curSt, nextSt)
