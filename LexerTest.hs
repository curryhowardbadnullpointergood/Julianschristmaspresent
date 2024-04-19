import GqlLexer
import Control.Exception
import System.IO
import System.Environment ( getArgs )



main :: IO ()
main = do
    (fileName : _ ) <- getArgs
    sourceText <- readFile fileName
    putStrLn ("Input File: \n" ++ replicate 50 '-'  ++ "\n" ++ sourceText ++ "\n" ++ replicate 50 '-')
    catch (main' sourceText) noLex

main' :: String -> IO ()
main' sourceText = do
    let lexedProg = alexScanTokens sourceText
    putStrLn ("Lexed As: \n" ++ replicate 50 '-'  ++ "\n" ++ show lexedProg ++ "\n" ++ replicate 50 '-')


noLex :: ErrorCall -> IO ()
noLex e = do let err =  show e
             hPutStr stderr ("Problem with lexing: " ++ err)
             return ()
