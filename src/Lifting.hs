{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use mapAndUnzipM" #-}
module Lifting (lift) where

import Typecheck
import Control.Monad.State hiding (lift)
import Core (Var)
import Prelude hiding (exp)

type LiftProg = [LiftDef]
type LiftDef = (String, Var, TCore)

lift :: TProg -> LiftProg
lift prog = evalState go_all 1
  where
    go_all :: State Int [LiftDef]
    go_all = concat <$> mapM go_def prog

    go_def :: (String, TCore) -> State Int [LiftDef]
    go_def (_, body) = do
      (_, defs) <- go body
      return defs

go :: TCore -> State Int (TCore, [LiftDef])
go = \case
    TInt n t -> return (TInt n t, [])
    TVar v t -> return (TVar v t, [])
    TAtom a t -> return (TAtom a t, [])
    TArith op e1 e2 t -> do
        (e1', ds1) <- go e1
        (e2', ds2) <- go e2
        return (TArith op e1' e2' t, ds1 ++ ds2)
    TCons header exps t   -> do
        (exps', defs) <- unzip <$> mapM go exps
        return (TCons header exps' t, concat defs)
    TApp e1 e2 t     -> do
        (e1', defs1) <- go e1
        (e2', defs2) <- go e2
        return (TApp e1' e2' t, defs1 ++ defs2)
    TIf cond e1 e2 t  -> do
        (cond',  dsc)  <- go cond
        (e1', ds1)  <- go e1
        (e2', ds2)  <- go e2
        return (TIf cond' e1' e2' t, dsc ++ ds1 ++ ds2)
    TCase exp alts t -> do
        (exp',  defs)   <- go exp
        (alts', defss)      <- unzip <$> mapM go_alt alts
        return (TCase exp' alts' t, defs ++ concat defss)
    TAbs v exp t -> do
        (exp', defs) <- go exp
        fname <- lifted
        return (TVar fname t, (fname, v, exp') : defs)
    where
        go_alt :: TAlt -> State Int (TAlt, [LiftDef])
        go_alt (pat, exp) = do
            (exp', defs) <- go exp
            return ((pat, exp'), defs)

lifted :: State Int String
lifted = do
  i <- get
  put (i+1)
  return ("lifted" ++ show i)