{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use mapAndUnzipM" #-}
module Lifting (lift, FreeMap) where

import Typecheck
import Control.Monad.State hiding (fix, lift)

import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Set (Set)

import Core (Type(..))

import Prelude hiding (exp)

type LiftProg = [LiftDef]
type LiftDef = (String, Maybe String, TCore)
type FreeMap = Map String (Set (String, Type))

lift :: TProg -> (LiftProg, FreeMap)
lift prog = evalState go_all 1
  where
    go_all :: State Int (LiftProg, FreeMap)
    go_all =  both . unzip <$> mapM go_top prog
        where
            both :: (Monoid a, Monoid b) => ([a], [b]) -> (a, b)
            both (xs, ys) = (mconcat xs, mconcat ys)

    go_top :: (String, TCore) -> State Int ([LiftDef], FreeMap)
    go_top (name, body) = case body of 
        TAbs v exp _ -> do
            (body', defs, free') <- go (Map.singleton name (free_vars body))  exp 
            return ((name, Just v, body') : defs, free')
        _ -> do
            (exp, defs, free) <- go (Map.singleton name (free_vars body)) body 
            return ((name, Nothing, exp): defs, free)

    go :: FreeMap -> TCore -> State Int (TCore, [LiftDef], FreeMap)
    go free = \case
        TInt n t -> return (TInt n t, [], free)
        TVar v t -> return (TVar v t, [], free)
        TAtom a t -> return (TAtom a t, [], free)
        TArith op e1 e2 t -> do
            (e1', ds1, free1) <- go free e1
            (e2', ds2, free2) <- go free e2
            return (TArith op e1' e2' t, ds1 ++ ds2, free1 <> free2)
        TCons header exps t -> do
            (exps', defs, free') <- unzip3 <$> mapM (go free) exps
            return (TCons header exps' t, concat defs, mconcat free')
        TApp e1 e2 t     -> do
            (e1', defs1, free1) <- go free e1
            (e2', defs2, free2) <- go free e2
            return (TApp e1' e2' t, defs1 ++ defs2, free1 <> free2)
        TIf cond e1 e2 t  -> do
            (cond',  dsc, freec)  <- go free cond
            (e1', ds1, free1)  <- go free e1
            (e2', ds2, free2)  <- go free e2
            return (TIf cond' e1' e2' t, dsc ++ ds1 ++ ds2, freec <> free1 <> free2)
        TCase exp alts t -> do
            (exp',  defs, freem) <- go free exp
            (alts', defss, free') <- unzip3 <$> mapM (go_alt free) alts
            return (TCase exp' alts' t, defs ++ concat defss, freem <> mconcat free')
        TAbs v exp t -> do
            (exp', defs, free') <- go free exp
            fname <- lifted
            let vars = free_vars (TAbs v exp t)
            let free'' = Map.insert fname vars free'
            return (TVar fname t, (fname, Just v, exp') : defs, free'')
        where
            go_alt :: FreeMap -> TAlt -> State Int (TAlt, [LiftDef], FreeMap)
            go_alt free' (pat, exp) = do
                (exp', defs, free'') <- go free' exp
                return ((pat, exp'), defs, free'')

lifted :: State Int String
lifted = do
  i <- get
  put (i+1)
  return ("lifted" ++ show i)

free_vars :: TCore -> Set (String, Type)
free_vars = go mempty
    where
        go :: Set (String, Type) -> TCore -> Set (String, Type)
        go bound = \case
            TInt {} -> mempty
            TVar v t -> if Set.member (v, t) bound || head v /= 'v' then mempty else Set.singleton (v, t)
            TAtom {} -> mempty
            TArith _ e1 e2 _ -> go bound e1 <> go bound e2
            TCons _ exps _ -> mconcat $ map (go bound) exps
            TApp e1 e2 _ -> go bound e1 <> go bound e2
            TIf {} -> error "'if' not implemented yet"
            TCase {} -> error "'case' not implemented yet"
            TAbs v exp (Fun t1 _) -> go (Set.insert (v, t1) bound) exp
            TAbs _ _ t -> error $ "Unexpected abstraction type: " ++ show t