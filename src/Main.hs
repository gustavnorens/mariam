module Main where


import ParGrammar (pProgram, myLexer)
import LayoutGrammar (resolveLayout)

import Core(translate)
import Typecheck(typecheck)
import Alpha(alpha)
import Lifting(lift)
import Anf(anf)
import System.FilePath (takeBaseName)
import Cast(cast, emit)
import System.Environment (getArgs)
import System.Exit (exitSuccess)
import System.Directory (createDirectoryIfMissing, removeDirectoryRecursive)
import System.Process (runCommand, waitForProcess)
import RefCount (rc)

main :: IO ()
main = do 
    args <- getArgs
    case args of 
        ["--run", file] -> do 
            compile file
            wait1 <- runCommand $ "gcc build/" ++ takeBaseName file ++ ".c -Wno-int-conversion -o build/" ++ takeBaseName file ++ ".out"
            _ <- waitForProcess wait1
            wait2 <- runCommand $ "build/" ++ takeBaseName file ++ ".out"
            _ <- waitForProcess wait2
            removeDirectoryRecursive "build"
            exitSuccess
        ["--compile", file] -> do 
            compile file
            exitSuccess
        _ -> putStrLn "Usage: mariam <filename>" >> exitSuccess


compile :: String -> IO ()
compile file = do
    content <- readFile file
    let ts = resolveLayout False $ myLexer content
    let prog = pProgram ts
    case prog of 
        Left err -> putStrLn $ "Parse error: " ++ err
        Right ast -> do
            let program = (alpha . translate) ast
            case typecheck program of
                Left err -> putStrLn $ "Type error: " ++ err
                Right checked -> do
                    let (lifted, free) = lift checked
                    let anfProg = anf free lifted
                    let scoped = rc anfProg
                    let casted = cast scoped
                    let cCode = emit casted
                    createDirectoryIfMissing False "build"
                    writeFile ("build/" ++ takeBaseName file ++ ".c") cCode
        
                