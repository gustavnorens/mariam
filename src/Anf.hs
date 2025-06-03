{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid lambda using `infix`" #-}
{-# LANGUAGE LambdaCase #-}
{-# HLINT ignore "Use tuple-section" #-}
module Anf (anf, pretty, AFunction(..), Body(..), Exp(..)) where

import Prelude hiding (exp)

import Core(Type(..), ArithOp(..))
import Typecheck (TCore(..))
import Lifting (FreeMap)

import Control.Monad.State

import qualified Data.Map as Map

import Data.Set (Set)
import qualified Data.Set as Set

type Var = (String, Type)

data AFunction = AFunction String (Maybe Var) Body deriving Show

data Context = Context
    {
        counter :: Int,
        free_map :: FreeMap,
        top :: Set String
    } deriving Show

data Body
    = Ret Var
    | Let Var Exp Body
    | Case Var [Body]
    deriving Show

data Exp
    = AInt Integer
    | AATom (Integer, String)
    | ACons (Integer, String) [Var]
    | AArith ArithOp Var Var
    | AApp Var Var
    | AProject Integer Var
    | AClosure String [Var]
    deriving Show

anf :: FreeMap -> [(String, Maybe String, TCore)] -> [AFunction]
anf free defs = map (go_def free tops) defs
    where 
        tops :: Set String
        tops = Set.fromList $ map (\(f,_,_) -> f) defs

go_def :: FreeMap -> Set String -> (String, Maybe String, TCore) -> AFunction
go_def free ss (name, arg, body) = AFunction name ((\s -> (s,Defined "Int")) <$> arg) body''
    where
        body' :: Body
        body' = evalState (go_body body (return . Ret)) (Context 0 free ss)

        body'' :: Body 
        body'' = case Map.lookup name free of 
            Just vs -> foldr (\(i, v) b -> Let v (AProject i ("clos", Defined "clos")) b) body' (zip [1..] (Set.toList vs))
            Nothing -> error $ "Anf: function " ++ name ++ " not found in the free map"

go_body :: TCore -> (Var -> State Context Body) -> State Context Body
go_body exp k = case exp of
    TVar v t -> do
        tops <- gets top 
        if Set.member v tops 
            then do
                var <- fresh t
                b <- k (fst var, t)
                m <- gets free_map
                case Map.lookup v m of
                    Just vs -> do
                        let vs' = Set.toList vs
                        return $ Let var (AClosure v vs') b
                    Nothing -> error $ "Anf: variable " ++ v ++ " not found in the free map"
            else k (v, t)
    TInt n t -> do
        v <- fresh t
        b <- k v
        return $ Let v (AInt n) b
    TAtom a t -> do
        v <- fresh t
        b <- k v
        return $ Let v (AATom a) b
    TArith op e1 e2 t -> do
        go_body e1 $ \v1 -> go_body e2 $ \v2 -> do
            v <- fresh t
            b <- k v
            return $ Let v (AArith op v1 v2) b
    TCons header exps t -> go_list exps $ \vs -> do
        v <- fresh t
        b <- k v
        return $ Let v (ACons header vs) b
    TApp e1 e2 t -> go_body e1 $ \v1 -> go_body e2 $ \v2 -> do
        v <- fresh t
        b <- k v
        return $ Let v (AApp v1 v2) b
    TAbs {} -> error "Anf: abstractions should not be present in the input"
    TIf {} -> error "Anf: 'if' not implemented yet"
    TCase {} -> error "Anf: 'case' not implemented yet"


go_list :: [TCore] -> ([Var] -> State Context Body) -> State Context Body
go_list [] k = k []
go_list (x:xs) k = go_body x $ \v -> go_list xs $ \vs -> k (v:vs)

pretty :: AFunction -> String
pretty (AFunction name arg body) = case arg of
    Just a ->  name ++ " " ++ (show . fst) a ++ " =\n" ++ pretty_body 2 body
    Nothing -> name ++ " =\n" ++ pretty_body 2 body

pretty_body :: Int -> Body -> String
pretty_body indent (Ret v) = replicate indent ' ' ++ "ret " ++ (show . fst) v ++ "\n"
pretty_body indent (Let v exp body) =
    replicate indent ' ' ++ "let " ++ (show . fst) v ++ " = " ++ pretty_exp exp ++ "\n" ++
    pretty_body indent body
pretty_body _ _ = error "Anf: 'case' not implemented yet"

pretty_exp :: Exp -> String
pretty_exp = \case
    AInt n -> show n
    AATom (_, name) -> name
    ACons (_, name) vs -> "constr " ++ show name ++ " [" ++ unwords (map (show . fst) vs) ++ "]"
    AArith op v1 v2 -> (show . fst) v1  ++ " " ++ show op ++ " " ++ (show . fst) v2
    AApp v1 v2 -> (show . fst) v1 ++ " " ++ (show . fst) v2
    AProject i v -> "project " ++ show i ++ (show . fst) v
    AClosure f vars -> "closure " ++ f ++ " [" ++ unwords (map (show . fst) vars) ++ "]"

fresh :: Type -> State Context Var
fresh t = do
    i <- gets counter
    modify $ \s -> s { counter = i + 1 }
    return ("anf" ++ show i, t)