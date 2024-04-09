import Lexer
import Parser
import GqlEval

import System.Environment ( getArgs )
import Control.Exception
import System.IO


-- main' = do
    -- (fileName : _ ) <- getArgs
    -- sourceText <- readFile fileName
    -- putStrLn ("Parsing : \n" ++ sourceText ++ "\n\nEND OF PARSING \n\n\n")
    -- let lexedProg = alexScanTokens sourceText
    -- putStrLn ("lexed as " ++ show lexedProg)
    -- let parsedProg = inputParser lexedProg
    -- let printedProg = printFile parsedProg
    -- putStr printedProg
    -- putStrLn ("Parsed as " ++ show parsedProg)
    -- let result1 = returnNodeRecord sample "com11"
    -- let result2 = returnNodeRecord sample "com1"
    -- let result3 = returnNodeRecord sample "as4"
    -- let result4 = returnNodeRecord sample "jj23"
    -- let result5 = returnNodeRecord sample "ab23"
    -- let result6 = returnNodeRecord sample "com9"
    -- let result7 = returnIDValues sample
    -- print result1
    -- print result2
    -- print result3
    -- print result4
    -- print result5
    -- print result6
    -- print result7
    -- print $ xy sample (head result7)
    -- -- print result8
    -- -- print result9

main :: IO ()
main = do 
    (fileName : _ ) <- getArgs
    sourceText <- readFile fileName
    putStrLn ("Input File: \n" ++ sourceText ++ "\n -----------------------------------------")
    catch (main' sourceText) noLex


main' :: String -> IO ()
main' sourceText = do 
    let lexedProg = alexScanTokens sourceText 
    catch (main'' lexedProg) noParse


main'' :: [Token] -> IO ()
main'' lexedProg = do
    let parsedProg = inputParser lexedProg
    main''' parsedProg

main''' :: File -> IO ()
main''' parsedProg = do
    putStr $ printFile parsedProg

noParse :: ErrorCall -> IO ()
noParse e = do let err =  show e
               hPutStr stderr ("Problem with parsing: " ++ err)
               return ()

noLex :: ErrorCall -> IO ()
noLex e = do let err =  show e
             hPutStr stderr ("Problem with lexing: " ++ err)
             return ()

