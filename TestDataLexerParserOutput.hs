import DataLexer
import DataParser
import System.Environment
import Control.Exception
import System.IO

main :: IO ()
main = catch main' noParse

main' = do 
    (fileName : _ ) <- getArgs 
    sourceText <- readFile fileName
    putStrLn ("\n=================\n\nPARSING : \n\n=================\n\n" ++ sourceText)
    let lexedProg = alexScanTokens sourceText
    -- putStrLn ("lexed as " ++ show lexedProg)
    let parsedProg = inputParser lexedProg 
    putStrLn ("=================\n\nPARSED AS : \n\n=================\n\n" ++ show parsedProg ++ "\n")

noParse :: ErrorCall -> IO ()
noParse e = do let err =  show e
               hPutStr stderr err
               return ()

noLex :: ErrorCall -> IO ()
noLex e = do let err =  show e
             hPutStr stderr ("Problem with lexing: " ++ err)
             return ()