{-# LANGUAGE LambdaCase #-}
module Typecheck (typecheck, TProg, TCore(..), TAlt, TPattern, get_type) where

import Core

import Prelude hiding (exp)

import Control.Monad (zipWithM)

import qualified Data.Map as Map
import Data.Map (Map)

data Context = Context
    { vars :: Map Var Type
    , tl_defs :: Map String Type
    , cons_map :: Map String (Type, [Type], Integer)
    }

data TCore
    = TInt Integer Type
    | TVar Var Type
    | TAtom (Integer, String) Type
    | TArith ArithOp TCore TCore Type
    | TCons (Integer, String) [TCore] Type
    | TApp TCore TCore Type
    | TAbs Var TCore Type
    | TCase TCore [TAlt] Type
    deriving Show

type TProg = [(String, TCore)]

type TAlt = (TPattern, TCore)
type TPattern = (Integer, String, [(Var, Type)])

get_type :: TCore -> Type
get_type = \case
    TVar _ t -> t
    TAtom _ t -> t
    TInt _ t -> t
    TCons _ _ t -> t
    TArith _ _ _ t -> t
    TApp _ _ t -> t
    TAbs _ _ t -> t
    TCase _ _ t -> t


typecheck :: CoreProg -> Either String TProg
typecheck prog = mapM (\(name, core) -> do
    case Map.lookup name tls of
        Just t -> check (Context Map.empty tls ctrm) t core >>= \tcore -> return (name, tcore)
        Nothing -> Left $ "no such type in the current scope: " ++ name
    ) defs
    where
        defs = core_defs prog
        tls = core_def_type_map prog
        ctrm = core_cons_map prog

check :: Context -> Type -> Core -> Either String TCore
check ctx t e = do
    e' <- infer ctx e
    if t == get_type e'
        then return e'
        else Left $ "expected type: " ++ show t ++ ", got: " ++ show (get_type e')

infer :: Context -> Core -> Either String TCore
infer ctx = \case
    EVar x -> case Map.lookup x (vars ctx) of
        Just t -> return $ TVar x t
        Nothing -> case Map.lookup x (tl_defs ctx) of
            Just t -> return $ TVar x t
            Nothing -> Left $ "no such variable in the current scope: " ++ x
    EInt v -> return $ TInt v (Defined "Int")
    EAtom name -> case Map.lookup name (cons_map ctx) of
        Just (t, _, tag) -> return $ TAtom (tag, name) t
        Nothing -> Left $ "no such atom in the current scope: " ++ name
    EArith op x y -> do
        case op of
            Less -> do
                x' <- check ctx (Defined "Int") x
                y' <- check ctx (Defined "Int") y
                return $ TArith op x' y' (Defined "Bool")
            _ -> do
                x' <- check ctx (Defined "Int") x
                y' <- check ctx (Defined "Int") y
                return $ TArith op x' y' (Defined "Int")
    EApp f x -> do
        tf <- infer ctx f
        case get_type tf of
            Fun t1 t2 -> do
                x' <- check ctx t1 x
                return $ TApp tf x' t2
            _ -> Left $ "expected function type, got: " ++ show (get_type tf)
    EAbs var t exp -> do
        let ctx' = (ctx {vars = Map.insert var t (vars ctx)})
        exp' <- infer ctx' exp
        return $ TAbs var exp' (Fun t (get_type exp'))
    EIf cond e1 e2 -> do
        cond' <- check ctx (Defined "Bool") cond
        e1' <- infer ctx e1
        e2' <- check ctx (get_type e1') e2
        return $ TCase cond'  [((0, "False", []), e2'), ((1, "True", []), e1')] (get_type e1')
    ECons cons_name exps -> let types = Map.lookup cons_name (cons_map ctx) in
        case types of
            Just (t, ts, tag) -> 
                if length exps /= length ts
                    then Left $ "number of expressions does not match the constructor: " ++ cons_name
                else do
                    exps' <- zipWithM (check ctx) ts exps
                    return $ TCons (tag, cons_name) exps' t
            Nothing -> Left $ "no such constructor in the current scope: " ++ cons_name
    ECase exp alts -> do
        exp' <- infer ctx exp
        let t = get_type exp'
        alts' <- mapM (infer_alt ctx t) alts
        let types = map (get_type . snd) alts'
        if and $ zipWith (==) types (tail types)
            then return $ TCase exp' alts' (head types)
            else Left $ "types of alternatives do not match: " ++ show types

infer_alt :: Context -> Type -> EAlt -> Either String TAlt
infer_alt ctx match_t ((name, binders), exp) = do
    case Map.lookup name (cons_map ctx) of
        Just (cons_t, ts, tag) -> do
            if cons_t == match_t
                then do
                    if length binders /= length ts
                        then Left $ "number of binders does not match the constructor: " ++ name
                    else do
                        let binders' = zip binders ts
                        let ctx' = ctx {vars = foldr (\(v, t) acc -> Map.insert v t acc) (vars ctx) binders'}
                        exp' <- infer ctx' exp
                        return ((tag, name, binders'), exp')
                else Left $ "expected type: " ++ show match_t ++ ", got: " ++ show cons_t
        Nothing -> Left $ "no such constructor in the current scope: " ++ name