{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
module Core (
    Core(..),
    CoreProg(..),
    BinOp(..),
    Type(..),
    Var,
    translate,
    EAlt,
    EPattern,
) where

import qualified AbsGrammar as Abs
import Prelude hiding (exp)

import qualified Data.Map as Map
import Data.Map (Map)

type Var = String

data CoreProg = CoreProg
    { core_defs :: [(String, Core)]
    , core_def_type_map :: Map String Type
    , core_cons_map :: Map String (Type, [Type], Integer)
    , core_types :: [Type]
    } deriving (Show)

data Type
    = Integer
    | Defined String
    | Fun Type Type
    deriving (Eq, Show, Ord)

data Core
    = EInt Integer
    | EVar Var
    | EAtom String
    | EBinOp BinOp Core Core
    | ECons String [Core]
    | EApp Core Core
    | EIf Core Core Core
    | ECase Core [EAlt]
    | EAbs Var Type Core
    deriving (Show)

type EAlt = (EPattern, Core)
type EPattern = (String, [Var])

data BinOp
    = Plus
    | Minus
    | Times
    | Div
    | Less
    | Greater
    | EQLess
    | EQGreater
    | Equal
    | Nequal
    deriving Eq

instance Show BinOp where
    show Plus = "+"
    show Minus = "-"
    show Times = "*"
    show Div = "/"
    show Less = "<"
    show Greater = ">"
    show EQLess = "<="
    show EQGreater = ">="
    show Equal = "=="
    show Nequal = "!="

translate :: Abs.Program -> CoreProg
translate (Abs.Program types defs) = CoreProg {
    core_defs = map translate_def defs,
    core_def_type_map = get_tl_map defs,
    core_cons_map = get_cons_map types, 
    core_types = get_types types}

translate_def :: Abs.Def -> (String, Core)
translate_def (Abs.DDef (Abs.Ident name) t _ args exp) = (name,  build_exp args [1..] (translate_type t) (translate_exp exp))
    where
        ident :: Abs.Ident -> String
        ident (Abs.Ident v) = v
        
        build_exp :: [Abs.Ident] -> [Integer] -> Type -> Core -> Core
        build_exp [] nums ty e = case ty of 
            Fun t1 t2 -> let arg = "arg" ++ show (head nums) in EAbs arg t1 (EApp (build_exp [] (tail nums) t2 e) (EVar arg))
            _ -> e
        build_exp (s : ss) nums vt e = case vt of
            Fun t1 t2 -> EAbs (ident s) t1 (build_exp ss nums t2 e)
            _ -> error $ "type for function is not correct: " ++ show t

translate_type :: Abs.Type -> Type
translate_type = \case
    Abs.TFun t1 t2 -> Fun (translate_type t1) (translate_type t2)
    Abs.TBuilt (Abs.UIdent s) -> Defined s

translate_exp :: Abs.Exp -> Core
translate_exp = \case
    Abs.EInt i -> EInt i
    Abs.EVar (Abs.Ident v) -> EVar v
    Abs.EAdd e1 e2 -> EBinOp Plus (translate_exp e1) (translate_exp e2)
    Abs.EMul e1 e2 -> EBinOp Times (translate_exp e1) (translate_exp e2)
    Abs.EDiv e1 e2 -> EBinOp Div (translate_exp e1) (translate_exp e2)
    Abs.ESub e1 e2 -> EBinOp Minus (translate_exp e1) (translate_exp e2)
    Abs.ELt e1 e2 -> EBinOp Less (translate_exp e1) (translate_exp e2)
    Abs.EGt e1 e2 -> EBinOp Greater (translate_exp e1) (translate_exp e2)
    Abs.EQLt e1 e2 -> EBinOp EQLess (translate_exp e1) (translate_exp e2)
    Abs.EQGt e1 e2 -> EBinOp EQGreater (translate_exp e1) (translate_exp e2)
    Abs.Eq e1 e2 -> EBinOp Equal (translate_exp e1) (translate_exp e2)
    Abs.Neq e1 e2 -> EBinOp Nequal (translate_exp e1) (translate_exp e2)
    Abs.EApp e1 e2 -> EApp (translate_exp e1) (translate_cons_exp e2)
    Abs.EAbs (Abs.Ident v) t e -> EAbs v (translate_type t) (translate_exp e)
    Abs.EIf e1 e2 e3 -> EIf (translate_exp e1) (translate_exp e2) (translate_exp e3)
    Abs.ECons (Abs.UIdent s) args -> ECons s (map translate_cons_exp args)
    Abs.EMatch exp alts -> ECase (translate_exp exp) (map translate_alt alts)        
    where
        translate_cons_exp :: Abs.ConsExp -> Core
        translate_cons_exp = \case
            Abs.EAtom (Abs.UIdent s) -> EAtom s
            Abs.EConsExp e -> translate_exp e
        translate_alt :: Abs.Alt -> EAlt
        translate_alt (Abs.CAlt  (Abs.PCons (Abs.UIdent s) args) exp) = ((s, map ident args), translate_exp exp)
            where
                ident :: Abs.Ident -> String
                ident (Abs.Ident v) = v

get_tl_map :: [Abs.Def] -> Map String Type
get_tl_map = foldl (\acc (Abs.DDef (Abs.Ident name) t _ _ _) -> Map.insert name (translate_type t) acc) Map.empty


get_cons_map :: [Abs.Type_declaration] -> Map String (Type, [Type], Integer)
get_cons_map = foldr go Map.empty
    where
        go :: Abs.Type_declaration -> Map String (Type, [Type], Integer) -> Map String (Type, [Type], Integer)
        go (Abs.TDecl (Abs.UIdent type_name) constructors) m = foldr go' m (zip [0..] constructors)
            where
                go' :: (Integer, Abs.Type_constructor) -> Map String (Type, [Type], Integer) -> Map String (Type, [Type], Integer)
                go' (tag, Abs.TCntr (Abs.UIdent cons_name) types) = Map.insert cons_name (Defined type_name, map translate_type types, tag)

get_types :: [Abs.Type_declaration] -> [Type]
get_types = foldr go [] 
    where 
        go :: Abs.Type_declaration -> [Type] -> [Type]
        go (Abs.TDecl (Abs.UIdent type_name) _) acc = Defined type_name : acc
