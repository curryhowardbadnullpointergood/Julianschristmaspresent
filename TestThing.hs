import System.Environment 
import Control.Exception
import System.IO 
import DataLexer
import DataParser
import GqlEval

handleIOException :: IOException -> IO ()
handleIOException e = hPutStrLn stderr ("IO Error: " ++ show e)

main :: IO ()
main = do
    args <- getArgs
    case args of
        (fileName : _) -> processFile fileName `catch` handleIOException
        _ -> putStrLn "Usage: TestThing <filename>"

processFile :: FilePath -> IO ()
processFile fileName = do
    content <- readFile fileName
    putStrLn "File content read. Processing..."
    let tokens = alexScanTokens content
    putStrLn "Tokenization complete. Parsing..."
    let parsedFile = inputParser tokens
    putStrLn "Parsing complete. Evaluating..."
    evaluateAndPrintResults parsedFile

evaluateAndPrintResults :: File -> IO ()
evaluateAndPrintResults file = do
    putStrLn "Printing all ID values from the file:"
    print $ GqlEval.returnIDValues file
    putStrLn "\nEvaluating and printing node records for specific IDs:"
    mapM_ (printNodeRecord file) ["rw5", "jj23", "nonexistentID"]

printNodeRecord :: File -> String -> IO ()
printNodeRecord file nodeId = do
    let result = GqlEval.returnNodeRecord file nodeId
    case result of
        Just node -> putStrLn $ "Record for " ++ nodeId ++ ": " ++ GqlEval.printNodeEntry node
        Nothing -> putStrLn $ "No record found for ID: " ++ nodeId
