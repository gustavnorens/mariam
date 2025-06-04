{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid lambda using `infix`" #-}
{-# LANGUAGE LambdaCase #-}
{-# HLINT ignore "Use tuple-section" #-}
module Anf (anf, pretty, AFunction(..), Body(..), Exp(..)) where

import Prelude hiding (exp)

import Core(Type(..), BinOp(..))
import Typecheck (TCore(..), TAlt)
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
        top_levels :: Set String
    } deriving Show

data Body
    = Ret Var
    | Let Var Exp Body
    | Case Var [(Integer, Body)]
    deriving Show

data Exp
    = AInt Integer
    | AATom (Integer, String)
    | ACons (Integer, String) [Var]
    | ABinOp BinOp Var Var
    | ACall Var
    | AApp Var Var
    | AProject Integer Var
    | AClosure String [Var]
    deriving Show

anf :: FreeMap -> [(String, Maybe Var, TCore)] -> [AFunction]
anf free defs = map (go_def free tops) defs
    where
        tops :: Set String
        tops = Set.fromList $ map (\(f,_,_) -> f) defs

go_def :: FreeMap -> Set String -> (String, Maybe Var, TCore) -> AFunction
go_def free ss (name, arg, body) = AFunction name arg body''
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
        tops <- gets top_levels
        if Set.member v tops
            then do
                var <- fresh t
                b <- k (fst var, t)
                m <- gets free_map
                case Map.lookup v m of
                    Just vs -> do
                        let vs' = Set.toList vs
                        case t of 
                            Fun {} -> return $ Let var (AClosure v vs') b
                            _ -> do 
                                clos_var <- fresh t
                                return $ Let clos_var (AClosure v vs') (Let var (ACall clos_var) b)
                        
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
    TBinOp op e1 e2 t -> do
        go_body e1 $ \v1 -> go_body e2 $ \v2 -> do
            v <- fresh t
            b <- k v
            return $ Let v (ABinOp op v1 v2) b
    TCons header exps t -> go_list exps $ \vs -> do
        v <- fresh t
        b <- k v
        return $ Let v (ACons header vs) b
    TApp e1 e2 t -> go_body e1 $ \v1 -> go_body e2 $ \v2 -> do
        v <- fresh t
        b <- k v
        return $ Let v (AApp v1 v2) b
    TAbs {} -> error "Anf: abstractions should not be present in the input"
    TCase e alts _ -> go_body e $ \v -> do
        alts' <- mapM (go_alt v) alts
        return $ Case v alts' 
        where 
            go_alt :: Var -> TAlt -> State Context (Integer, Body)
            go_alt v ((i, _, vs), exp') = do
                body <- go_body exp' k
                return (i, foldl (\acc (field, var) -> Let var (AProject field v) acc) body (zip [0..] vs))
        


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
pretty_body indent (Case v bodies) = replicate indent ' ' ++ "case " ++ (show . fst) v ++ " of\n" ++
    concatMap (\(i, body) -> replicate (indent + 2) ' ' ++ show i ++ " ->\n" ++ pretty_body (indent + 4) body) bodies
pretty_exp :: Exp -> String
pretty_exp = \case
    AInt n -> show n
    AATom (_, name) -> name
    ACons (_, name) vs -> "constr " ++ show name ++ " [" ++ unwords (map (show . fst) vs) ++ "]"
    ABinOp op v1 v2 -> (show . fst) v1  ++ " " ++ show op ++ " " ++ (show . fst) v2
    AApp v1 v2 -> (show . fst) v1 ++ " " ++ (show . fst) v2
    ACall v -> "call " ++ fst v
    AProject i v -> "project " ++ show i ++ " " ++ (show . fst) v
    AClosure f vars -> "closure " ++ f ++ " [" ++ unwords (map (show . fst) vars) ++ "]"

fresh :: Type -> State Context Var
fresh t = do
    i <- gets counter
    modify $ \s -> s { counter = i + 1 }
    return ("anf" ++ show i, t)