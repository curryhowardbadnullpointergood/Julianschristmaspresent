import InputLexer
import InputParser
import GqlLibrary

import Control.Exception
import System.IO
import System.Environment ( getArgs )

-- THIS IS FERDI'S TESTGQLLIBARY THING, SO JOSH AND ASH CAN USE

main :: IO ()
main = do
    args <- getArgs
    case args of
        (fileName : _) -> processFile fileName `catch` handleIOException
        _ -> putStrLn "Usage: TestThing <filename>"

handleIOException :: IOException -> IO ()
handleIOException e = hPutStrLn stderr ("IO Error: " ++ show e)

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

    -- let nodes = getNodes file

    -- let filteredAgeNodes = filterIntNodes nodes "age" (< 25)

    -- let extractedID1 = extractNodeFields "firstname" filteredAgeNodes
    -- print extractedID1

    -- let filteredVisitorNodes = filterByLabelNodes nodes (== "Visitor")

    -- let extractedID1 = extractNodeIDs filteredAgeNodes
    -- print extractedID1

    -- let extractedAgeNodes1 = extractNodeFields "age" filteredAgeNodes
    -- print extractedAgeNodes1

    -- let extractedLabel1 = extractNodeLabels filteredAgeNodes

    -- let final = convertToNodes "age:integer" True combined
    -- print final
 
    -- let extractedID2 = extractNodeIDs filteredVisitorNodes
    -- print extractedID2
    -- let extractedAgeNodes2 = extractNodeFields "age" filteredVisitorNodes
    -- print extractedAgeNodes2
    -- let extractedLabel2= extractNodeLabels filteredVisitorNodes
    -- print extractedLabel2

    -- let extracted = extractNodeIDs nodes
    -- print extracted

    -- let extracted = extractRelationStartIDs relations
    -- print extracted

    -- let extracted = extractRelationEndIDs relations
    -- print extracted

    -- let extracted = extractRelationTypes relations
    -- print extracted



    