import System.Environment 
import Control.Exception
import System.IO 
import DataLexer
import DataParser
import GqlEval
import Data.List

-- THIS WHOLE FUNCTION IS JSUT TO HELP ME UNDERSTAND HOW EVERYTHING THUS FAR WORKS.

-- LIKE SERIOUSLY THIS IS PURELY JUST YAPPING FOR FERDI IGNORE IT

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
    putStrLn ":ID, age:integer, :LABEL"
    let ageData = getField file "age"
    let labelData = getLabels file
    let eligibleByAge = filterIntField ageData (<= 25) False
    let eligibleByVisitorLabel = filterLabel labelData (== "Visitor")
    let eligibleIDs = nub (eligibleByAge ++ eligibleByVisitorLabel)
    mapM_ (printFormattedNodeRecord file) eligibleIDs

printFormattedNodeRecord :: File -> String -> IO ()
printFormattedNodeRecord file nodeId = do
    let maybeNode = returnNodeRecord file nodeId
    case maybeNode of
        Just node -> putStrLn $ formatNode node
        Nothing -> return ()

formatNode :: NodeEntry -> String
formatNode (NodeEntry id literals labels) =
    let age = case find isAgeLiteral literals of
                Just (LiteralInt a) -> show a
                _ -> "null"
        labelStr = extractLabel (head labels)
    in intercalate ", " [id, age, labelStr]

isAgeLiteral :: Literal -> Bool
isAgeLiteral (LiteralInt _) = True
isAgeLiteral _ = False