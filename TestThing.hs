import System.Environment 
import Control.Exception
import System.IO 
import DataLexer
import DataParser
import GqlEval
import Data.List

-- THIS WHOLE FUNCTION IS JSUT TO HELP ME UNDERSTAND HOW EVERYTHING THUS FAR WORKS.

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
    print $ returnIDValues file

    putStrLn "\nIDs eligible based on age <= 25 or label 'Visitor':"
    let eligibleIDs = filterEligibleNodes file
    print eligibleIDs

    putStrLn "\nEvaluating and printing node records for specific IDs:"
    mapM_ (printNodeRecord file) ["rw5", "jj23", "nonexistentID"]

printNodeRecord :: File -> String -> IO ()
printNodeRecord file nodeId = do
    let result = returnNodeRecord file nodeId
    case result of
        Just node -> putStrLn $ "Record for " ++ nodeId ++ ": " ++ printNodeEntry node
        Nothing -> putStrLn $ "No record found for ID: " ++ nodeId

filterEligibleNodes :: File -> [String]
filterEligibleNodes (File nodeSets _) = concatMap filterNodeSet nodeSets

filterNodeSet :: NodeSet -> [String]
filterNodeSet (NodeSet _ nodeEntries) = map getNodeID $ filter isEligible nodeEntries

isEligible :: NodeEntry -> Bool
isEligible (NodeEntry _ literals labels) =
    let hasEligibleAge = any (\lit -> case lit of
                                LiteralInt age -> age <= 25
                                _ -> False) literals
        isVisitor = any (\label -> case label of
                                Label "Visitor" -> True
                                _ -> False) labels
    in hasEligibleAge || isVisitor

getNodeID :: NodeEntry -> String
getNodeID (NodeEntry id _ _) = id