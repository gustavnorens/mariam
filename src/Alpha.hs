{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Alpha (alpha) where

import Core

import qualified Data.Map as Map
import Data.Map (Map)
import Control.Monad.State
import Prelude hiding (exp)

type Context = Map Var Var


alpha :: CoreProg -> CoreProg
alpha coreProg = coreProg { core_defs = map alpha_def (core_defs coreProg) }
    where
        alpha_def :: (String, Core) -> (String, Core)
        alpha_def (name, core) = (name, alpha_core core)

alpha_core :: Core -> Core
alpha_core core = evalState (go Map.empty core) 0

go :: Context -> Core -> State Int Core
go ctx = \case
    EInt n -> return (EInt n)
    EVar v -> case Map.lookup v ctx of
        Just v' -> return (EVar v')
        Nothing -> return (EVar v)
    EAtom v -> return (EAtom v)
    EBinOp op e1 e2 -> do
        e1' <- go ctx e1
        e2' <- go ctx e2
        return $ EBinOp op e1' e2'
    ECons name args -> do
        args' <- mapM (go ctx) args
        return $ ECons name args'
    EApp e1 e2 -> do
        e1' <- go ctx e1
        e2' <- go ctx e2
        return $ EApp e1' e2'
    EIf cond e1 e2 -> do
        cond' <- go ctx cond
        e1' <- go ctx e1
        e2' <- go ctx e2
        return $ EIf cond' e1' e2'
    EAbs v t e -> do
        fresh' <- fresh
        modify (+1)
        let ctx' = Map.insert v fresh' ctx
        e' <- go ctx' e
        return $ EAbs fresh' t e'
    ECase exp alts -> do
        exp' <- go ctx exp
        alts' <- mapM (go_alt ctx) alts
        return $ ECase exp' alts'

go_alt :: Context -> EAlt -> State Int EAlt
go_alt ctx ((name, vars), e) = do
    fresh_vars <- mapM (const fresh) vars
    modify (+ length vars)
    let ctx' = foldl (\acc (v, f) -> Map.insert v f acc) ctx (zip vars fresh_vars)
    e' <- go ctx' e
    return ((name, fresh_vars), e')

fresh :: State Int String
fresh = do
    n <- get
    modify (+1)
    return $ "v" ++ show n
