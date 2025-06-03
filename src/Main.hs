module Main where


import ParGrammar (pProgram, myLexer)
import LayoutGrammar (resolveLayout)

import qualified Data.Map as Map

import Core(translate)
import Typecheck(typecheck)
import Alpha(alpha)
import Lifting(lift)
import Anf(anf, pretty)
import Cast(cast, emit)

main :: IO ()
main = do 
    content <- getContents
    let ts = resolveLayout False $ myLexer content
    let prog = pProgram ts
        in case prog of 
            Left err -> putStrLn $ "Parse error: " ++ err
            Right ast -> do
                let program = (alpha . translate) ast
                putStrLn "Type checking..."
                case typecheck program of
                    Left err -> putStrLn $ "Type error: " ++ err
                    Right checked -> do
                        print checked
                        putStrLn "Type checking successful!"
                        putStrLn "Lifting..."
                        let (lifted, free) = lift checked
                        putStrLn "Lifting successful!"
                        print free
                        print lifted
                        putStrLn "ANF conversion..."
                        let anfProg = anf free lifted
                        putStrLn "ANF conversion successful!"
                        putStrLn "Pretty printing ANF program..."
                        mapM_ (putStrLn . pretty) anfProg
                        putStrLn "Free variables in lifted program:"
                        mapM_ (\(name, vars) -> putStrLn $ name ++ ": " ++ show vars) (Map.toList free)
                        putStrLn "Casting to C..."
                        let casted = cast anfProg
                        putStrLn "Casting successful!"
                        putStrLn "Emitting C code..."
                        let cCode = emit casted
                        writeFile "output.c" cCode
                        

        
                