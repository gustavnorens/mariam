{-# LANGUAGE LambdaCase #-}
module Cast (cast, emit) where

import Core(Var, ArithOp(..))
import Anf (AFunction(..), Body(..), Exp(..))
import Prelude hiding (exp)

type FName = String
type Indent = Int

data CFunction = CFunction
    { cName :: FName
    , cArgs  :: [Var]
    , cBody :: [Cast]
    } deriving Show

data Cast
    = CInt Var Integer
    | CArith ArithOp Var Var Var
    | CSwitch Var [[Cast]]
    | CCall Var Var Var
    | CReturn Var
    | CProject Var Integer Var
    | CInject Var Integer Var
    | CMalloc Var Integer
    deriving Show

cast :: [AFunction] -> [CFunction]
cast = map cast_function

cast_function :: AFunction -> CFunction
cast_function (AFunction name arg body) = case name of 
    "main" -> CFunction name [] (castBody body)
    _ -> case arg of 
        Nothing -> CFunction name ["clos"] (castBody body)
        Just (v, _) -> CFunction name ["clos", v] (castBody body)

castBody :: Body -> [Cast]
castBody = \case
    Ret v -> [CReturn (fst v)]
    Let (v, _) exp body -> case exp of
        AInt i -> CInt v i : castBody body
        AATom a -> CInt v (fst a) : castBody body
        ACons (tag, _) vs ->
            [
                CMalloc v (3 + toInteger (length vs)),
                CInject v 0 (show (tag * 2 + 1)),
                CInject v 1 (show (length vs)),
                CInject v 2 "1"
            ] ++ zipWith (CInject v) [3..] (map fst  vs) ++ castBody body
        AArith op (v1, _) (v2, _) -> CArith op v v1 v2 : castBody body
        AApp (v1, _) (v2, _) -> CCall v v1 v2 : castBody body
        AProject i (r, _) -> CProject v i r : castBody body
        AClosure fname vs -> 
            [
                CMalloc v (4 + toInteger (length vs)),
                CInject v 0 "NULL",
                CInject v 1 (show (length vs)),
                CInject v 2 "1",
                CInject v 3 ("&" ++ fname)
            ] ++ zipWith (CInject v) [4..] (map fst vs) ++ castBody body
    Case _ _ -> error "Case not supported in cast"


emit :: [CFunction] -> String
emit funs = unlines
    [
        "#include <stdio.h>",
        "#include <stdlib.h>",
        "",
        "typedef __int64_t Value;",
        "typedef Value (*Func)(Value, Value);",
        "",
        "Value call(Value clos, Value arg) {",
        "  Func f = ((Value *) clos)[3];",
        "  return f(clos, arg);",
        "}",
        ""
    ] ++ join "\n" (map emit_decl funs) ++ "\n" ++ join "\n" (map emit_function funs) ++ "\n"

emit_decl :: CFunction -> String
emit_decl (CFunction name args _) =
    "Value " ++ name ++ "(" ++ join ", " (map ("Value " ++) args) ++ ");"

emit_function :: CFunction -> String
emit_function (CFunction name args body) =
    "Value " ++ name ++ "(" ++ join ", " (map ("Value " ++) args) ++ ") {\n" ++
    unlines (map (emit_cast 2) body) ++ "}"


emit_cast :: Indent -> Cast -> String
emit_cast indent = \case
    CInt v i -> dent "Value " ++ v ++ " = " ++ show (i * 2 + 1) ++ ";"
    CArith op v1 v2 v3 -> dent $ case op of
        Plus -> "Value " ++ v1 ++ " = " ++ v2 ++ " + " ++ v3 ++ " - 1 " ++ ";"
        Minus -> "Value " ++ v1 ++ " = " ++ v2 ++ " - " ++ v3 ++ " | 1 " ++ ";"
        Times -> "Value " ++ v1 ++ " = ((" ++ v2 ++ " - 1) * (" ++ v3 ++ " >> 1) | 1);"
        Div -> "Value " ++ v1 ++ " = (" ++ v2 ++ "/" ++ "(" ++ v3 ++ " - 1)) << 1 | 1;"
        Less -> "Value " ++ v1 ++ " = " ++ v2 ++ " < " ++ v3 ++ ";"
    CSwitch _ _ -> undefined
    CCall v clos arg -> dent "Value " ++ v ++ " = call(" ++ clos ++ ", " ++ arg ++ ");"
    CReturn v -> dent "return " ++ v ++ ";"
    CProject v i r -> dent "Value " ++ v ++ " = ((Value*) " ++ r ++ ")[" ++ show (i + 3) ++ "];"
    CInject r i v -> dent "((Value*) " ++ r ++ ")[" ++ show i ++ "] = " ++ v ++ ";"
    CMalloc v size -> dent "Value " ++ v ++ " = (Value) malloc(" ++ show size ++ " *" ++ " sizeof(Value));"
    where
        dent :: String -> String
        dent = (++) (replicate indent ' ')

join :: [a] -> [[a]] -> [a]
join sep = foldr (\acc ss -> acc ++ if null ss then ss else sep ++ ss) []