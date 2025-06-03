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
import System.Environment (getArgs)
import System.Exit (exitSuccess)
import System.Directory (createDirectoryIfMissing, removeDirectoryRecursive)
import System.Process (runCommand, waitForProcess)


main :: IO ()
main = do 
    args <- getArgs
    case args of 
        [file] -> do 
            compile file
            wait1 <- runCommand $ "gcc build/" ++ remove_file_suffix file ++ ".c -Wno-int-conversion -o build/" ++ remove_file_suffix file ++ ".out"
            _ <- waitForProcess wait1
            wait2 <- runCommand $ "build/" ++ remove_file_suffix file ++ ".out"
            _ <- waitForProcess wait2
            removeDirectoryRecursive "build"
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
                    let casted = cast anfProg
                    let cCode = emit casted
                    createDirectoryIfMissing False "build"
                    writeFile ("build/" ++ remove_file_suffix file ++ ".c") cCode

remove_file_suffix :: String -> String
remove_file_suffix s = take (length s - 4) s
        
                