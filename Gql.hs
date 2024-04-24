import LangLexer
import LangParser
import InputLexer
import InputParser
import GqlEvaluator
import System.Environment ( getArgs )
import Control.Exception
import System.IO


main :: IO ()
main = do
    (programFileName : _ ) <- getArgs
    sourceText <- readFile programFileName
    putStrLn ("Input File: \n" ++ replicate 50 '-'  ++ "\n" ++ sourceText ++ "\n" ++ replicate 50 '-')
    catch (lexProgram sourceText) noLex

lexProgram :: String -> IO ()
lexProgram programSourceText = do
    let lexedProg = LangLexer.alexScanTokens programSourceText
    -- putStrLn ("Lexed As: \n" ++ replicate 50 '-'  ++ "\n" ++ show lexedProg ++ "\n" ++ replicate 50 '-')
    catch (parseProgram lexedProg) noParse

parseProgram :: [LangToken] -> IO ()
parseProgram lexedProg = do
    let parsedProg = langParser lexedProg
    putStrLn ("Parsed As: \n" ++ replicate 50 '-'  ++ "\n" ++ show parsedProg ++ "\n" ++ replicate 50 '-')
    getInputFile parsedProg

getInputFile :: Query -> IO ()
getInputFile (Query read match) = do 
    let inputFileName   = evalReadFile read
    inputFileText       <- readFile inputFileName
    let inputFileLexed  = InputLexer.alexScanTokens inputFileText
    let inputFileParse  = inputParser inputFileLexed
    evalQuery match inputFileParse

evalQuery :: Match -> File -> IO()
evalQuery  match file = do
    let matchEval = evalMatch [] match file
    putStrLn $ printTables matchEval

printTables :: [[String]] -> String
printTables (table:tables) = printTable table ++ "\n" ++ printTables tables

printTable :: [String] -> String
printTable [] = "\n"
printTable (line:lines) = line ++ "\n" ++ printTable lines




noParse :: ErrorCall -> IO ()
noParse e = do let err =  show e
               hPutStr stderr ("Problem with parsing: " ++ err)
               return ()

noLex :: ErrorCall -> IO ()
noLex e = do let err =  show e
             hPutStr stderr ("Problem with lexing: " ++ err)
             return ()