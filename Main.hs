import Lexer
import Parser
import System.Environment ( getArgs )
import Control.Exception
import System.IO


main :: IO ()
main = catch main' noParse


main' = do 
    (fileName : _ ) <- getArgs 
    sourceText <- readFile fileName
    putStrLn ("Parsing : " ++ sourceText)
    let lexedProg = alexScanTokens sourceText
    -- putStrLn ("lexed as " ++ show lexedProg)
    let parsedProg = inputParser lexedProg 
    putStrLn ("Parsed as " ++ show parsedProg)

noParse :: ErrorCall -> IO ()
noParse e = do let err =  show e
               hPutStr stderr err
               return ()

noLex :: ErrorCall -> IO ()
noLex e = do let err =  show e
             hPutStr stderr ("Problem with lexing: " ++ err)
             return ()