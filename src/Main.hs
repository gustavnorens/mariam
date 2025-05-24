module Main where


import ParGrammar (pProgram, myLexer)
import LayoutGrammar (resolveLayout)


import Core(translate)
import Typecheck(typecheck)
import Alpha(alpha)
import Lifting(lift)

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
                        putStrLn "Type checking successful!"
                        print $ lift checked

        
                