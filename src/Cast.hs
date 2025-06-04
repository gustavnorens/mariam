{-# LANGUAGE LambdaCase #-}
module Cast (cast, emit) where

import Core(Var, BinOp(..))
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
    | CArith BinOp Var Var Var
    | CSwitch Var [(Integer, [Cast])]
    | CCall Var Var (Maybe Var)
    | CReturn Var
    | CPrint Var
    | CProject Var Integer Var
    | CInject Var Integer Var
    | CMalloc Var Integer
    deriving Show

cast :: [AFunction] -> [CFunction]
cast = map cast_function

cast_function :: AFunction -> CFunction
cast_function (AFunction name arg body) = case name of
    "main" -> CFunction name [] (cast_body name body)
    _ -> case arg of
        Nothing -> CFunction name ["clos"] (cast_body name body)
        Just (v, _) -> CFunction name ["clos", v] (cast_body name body)

cast_body :: String -> Body -> [Cast]
cast_body name = \case
    Ret v -> case name of
        "main" -> [CPrint (fst v)]
        _ -> [CReturn (fst v)]
    Let (v, _) exp body -> case exp of
        AInt i -> CInt v i : cast_body name body
        AATom a -> CInt v (fst a) : cast_body name body
        ACons (tag, _) vs ->
            [
                CMalloc v (3 + toInteger (length vs)),
                CInject v 0 (show (tag * 2 + 1)),
                CInject v 1 (show (length vs)),
                CInject v 2 "1"
            ] ++ zipWith (CInject v) [3..] (map fst  vs) ++ cast_body name body
        ABinOp op (v1, _) (v2, _) -> CArith op v v1 v2 : cast_body name body
        AApp (v1, _) (v2, _) -> CCall v v1 (Just v2) : cast_body name body
        ACall (v1, _) -> CCall v v1 Nothing : cast_body name body
        AProject i (r, _) -> CProject v i r : cast_body name body
        AClosure fname vs ->
            [
                CMalloc v (4 + toInteger (length vs)),
                CInject v 0 "NULL",
                CInject v 1 (show (length vs)),
                CInject v 2 "1",
                CInject v 3 ("&" ++ fname)
            ] ++ zipWith (CInject v) [4..] (map fst vs) ++ cast_body name body
    Case v bs -> [CSwitch (fst v) (map (\(i, body) -> (i, cast_body name body)) bs)]


emit :: [CFunction] -> String
emit funs = unlines
    [
        "#include <stdio.h>",
        "#include <stdlib.h>",
        "",
        "typedef __int64_t Value;",
        "typedef Value (*Func1)(Value);",
        "typedef Value (*Func2)(Value, Value);",
        "",
        "Value apply(Value clos, Value arg) {",
        "  Func2 f = ((Value *) clos)[3];",
        "  return f(clos, arg);",
        "}",
        "",
        "Value call(Value clos) {",
        "  Func1 f = ((Value *) clos)[3];",
        "  return f(clos);",
        "}",
        ""
    ] ++ join "\n" (map emit_decl funs) ++ "\n\n" ++ join "\n\n" (map emit_function funs) ++ "\n"

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
        _ -> "Value " ++ v1 ++ " = (" ++ v2 ++ " " ++ show op ++ " " ++ v3 ++ ") << 1 | 1;"
    CCall v clos (Just arg) -> dent "Value " ++ v ++ " = apply(" ++ clos ++ ", " ++ arg ++ ");"
    CCall v clos Nothing -> dent "Value " ++ v ++ " = call(" ++ clos ++ ");"
    CReturn v -> dent "return " ++ v ++ ";"
    CPrint v -> dent "printf(\"%lld\\n\", " ++ v ++ " >> 1);"
    CProject v i r -> dent "Value " ++ v ++ " = ((Value*) " ++ r ++ ")[" ++ show (i + 3) ++ "];"
    CInject r i v -> dent "((Value*) " ++ r ++ ")[" ++ show i ++ "] = " ++ v ++ ";"
    CMalloc v size -> dent "Value " ++ v ++ " = (Value) malloc(" ++ show size ++ " *" ++ " sizeof(Value));"
    CSwitch v bodies -> 
        dent "Value tag_" ++ v ++ " = ((" 
            ++ v 
            ++ " & 1) ? " 
            ++ v 
            ++ " : ((Value*) "
            ++ v 
            ++ ")[0]);\n" 
            ++ dent "switch (tag_" ++ v ++ ") {\n"
            ++ join "\n" (map (emit_case (indent + 2)) bodies)
            ++ "\n"
            ++ dent "}"
    where
        dent :: String -> String
        dent = (++) (replicate indent ' ')

        emit_case :: Indent -> (Integer, [Cast]) -> String
        emit_case indent' (i, ss) = replicate indent' ' ' 
            ++ "case " 
            ++ show (i * 2 + 1)
            ++ ":\n"
            ++ join "\n" (map (emit_cast (indent' + 2)) ss)
            ++ "\n"
            ++ replicate (indent' + 2) ' '
            ++ "break;"

join :: [a] -> [[a]] -> [a]
join sep = foldr (\acc ss -> acc ++ if null ss then ss else sep ++ ss) []