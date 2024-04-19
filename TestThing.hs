import System.Environment 
import Control.Exception
import System.IO 
import DataLexer
import DataParser
import GqlEval
import Data.List
import Control.Monad

-- THIS WHOLE FUNCTION IS JSUT TO HELP ME UNDERSTAND HOW EVERYTHING THUS FAR WORKS.

-- LIKE SERIOUSLY THIS IS PURELY JUST YAPPING FOR FERDI IGNORE IT

handleIOException :: IOException -> IO ()
handleIOException e = hPutStrLn stderr ("IO Error: " ++ show e)

-- Assuming the function printTables is defined as follows:
printTables :: [String] -> IO ()
printTables tables = forM_ tables $ \table -> do
    putStrLn table

-- Function to print the formatted CSV tables
printCSVTables :: ([String], [String]) -> IO ()
printCSVTables (nodeTables, relationshipTables) = do
    mapM_ putStrLn nodeTables
    mapM_ putStrLn relationshipTables

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
    --let ageData = getField file "age"
    --print ageData
    --let labelData = getLabels file
    --let eligibleByAge = filterIntField ageData (<= 25) False
    --print eligibleByAge
    --let eligibleByVisitorLabel = filterLabel labelData (== "Visitor")
    --let eligibleIDs = nub (eligibleByAge ++ eligibleByVisitorLabel)



    -- Working Q1 (age:TEMPFIXLATER)

    let temp = getNodes file
    let part1 = filterIntNodes temp "age" (<= 25)
    let part2 = filterLabelNodes temp (== "Visitor")
    let combinedParts = unionNode part1 part2
    let output3 = formatNodeTable combinedParts ["age"]
    print output3

    putStrLn output3



{-

    -- Weird

    let nodes = getNodes file
    let youngPeople = filterIntNodes nodes "age" (<= 25)
    print youngPeople

    let visitors = filterLabelNodes nodes (== "Visitor")
    print visitors

    let idsAsString = unionLists (extractNodeIDs youngPeople) (extractNodeIDs visitors)
    print idsAsString

    let idsAsString2 = extractNodeLabels youngPeople
    print idsAsString2

    -- Extract ages as strings directly in the main, handling null values
    let agesAsString = map (\node -> maybe "null" literalToString (extractFieldFromNodeEntry "age" node)) nodes
    print agesAsString

    --let output3 = formatNodeTable relevantNodes ["age"]
    --putStrLn output3
-}

    

    -- Q2

{-
    let wholeThing = getTables file

    -- Relationships filtering
    let temp1 = getRelationships file
    let availTrue = filterBoolRelations temp1 "available" (== True)

    -- Nodes filtering
    let temp2 = getNodes file
    let isStaff = filterLabelNodes temp2 (== "Staff")

    -- Extract valid IDs
    let staffNodeIds = extractNodeIDs isStaff
    let availableStartIds = extractStartIDs availTrue
    let validStartIds = interSectionLists staffNodeIds availableStartIds

    let validEndIds = extractEndIDs $ filterIntRelations temp1 "priority" (>= 8)

    -- Generate new relationships
    let headerSpecs = []
    let newEntries = generatePossiblyAllocatedEntries validStartIds validEndIds "PossiblyAllocated"
    let updatedData = addNewRelationships wholeThing headerSpecs newEntries

    let finale = generateCSVTables updatedData
    printCSVTables finale



-}