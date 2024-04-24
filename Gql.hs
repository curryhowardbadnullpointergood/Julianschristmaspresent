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
    putStrLn $ printFile matchEval


printFile :: File -> String
printFile (File nodeSets relationshipSets) = printNodeSets nodeSets ++ "\n" ++ printRelationshipSets relationshipSets

printNodeSets :: [NodeSet] -> String
printNodeSets [] = ""
printNodeSets (nodeSet : []) = printNodeSet nodeSet
printNodeSets (nodeSet : nodeSets) = printNodeSet nodeSet ++ "\n" ++ printNodeSets nodeSets

printNodeSet :: NodeSet -> String
printNodeSet (NodeSet nodeHeader nodeEntries) = printNodeHeader nodeHeader ++ "\n" ++ printNodeEntries nodeEntries

printNodeHeader :: NodeHeader -> String
printNodeHeader (NodeHeader [] True) = ":ID" ++ ", "  ++  ":LABEL"
printNodeHeader (NodeHeader fields True) = ":ID" ++ ", " ++ printFields fields ++ ", " ++  ":LABEL"
printNodeHeader (NodeHeader [] False) = ":ID"
printNodeHeader (NodeHeader fields False) = ":ID" ++ ", " ++  printFields fields

printNodeEntries :: [NodeEntry] -> String
printNodeEntries [] = ""
printNodeEntries (nodeEntry : []) = printNodeEntry nodeEntry  ++ "\n"
printNodeEntries (nodeEntry : nodeEntries) = printNodeEntry nodeEntry ++ "\n" ++ printNodeEntries nodeEntries

printNodeEntry :: NodeEntry -> String
printNodeEntry (NodeEntry str [] []) = str
printNodeEntry (NodeEntry str [] labels) = str ++  ", " ++ printLabels labels
printNodeEntry (NodeEntry str literals []) = str ++ ", " ++ printLiterals literals
printNodeEntry (NodeEntry str literals labels) = str ++ ", " ++ printLiterals literals ++ ", " ++ printLabels labels

printRelationshipSets :: [RelationshipSet] -> String
printRelationshipSets [] = ""
printRelationshipSets (relationshipSet : []) = printRelationshipSet relationshipSet
printRelationshipSets (relationshipSet : relationShipSets) = printRelationshipSet relationshipSet ++ "\n" ++ printRelationshipSets relationShipSets

printRelationshipSet :: RelationshipSet -> String
printRelationshipSet (RelationshipSet relationshipHeader relationshipEntries) = printRelationshipHeader relationshipHeader ++ "\n" ++ printRelationshipEntries relationshipEntries

printRelationshipHeader :: RelationshipHeader -> String
printRelationshipHeader (RelationshipHeader []) = ":START_ID" ++ ", " ++ ":END_ID" ++ ", " ++ ":TYPE"
printRelationshipHeader (RelationshipHeader fields) = ":START_ID" ++ ", " ++ printFields fields ++ ", " ++ ":END_ID" ++ ", " ++ ":TYPE"

printRelationshipEntries :: [RelationshipEntry] -> String
printRelationshipEntries [] = ""
printRelationshipEntries (relationshipentry : []) = printRelationshipEntry relationshipentry ++ "\n"
printRelationshipEntries (relationshipentry : relationshipEntries) = printRelationshipEntry relationshipentry ++ "\n" ++ printRelationshipEntries relationshipEntries

printRelationshipEntry :: RelationshipEntry -> String
printRelationshipEntry (RelationshipEntry str1 [] str2 str3) = str1 ++ ", " ++ str2 ++ ", " ++ str3
printRelationshipEntry (RelationshipEntry str1 literals str2 str3) = str1 ++ ", " ++ printLiterals literals ++ ", " ++ str2 ++ ", " ++ str3

printType :: FieldType -> String
printType TypeString = "string"
printType TypeBoolean = "boolean"
printType TypeInteger = "integer"

printFields :: [Field] -> String
printFields [] = ""
printFields (field : []) = printField field
printFields (field : fields) = printField field ++ ", " ++ printFields fields

printField :: Field -> String
printField (Field str fieldType) = str ++ ":" ++ printType fieldType

printLiterals :: [Literal] -> String
printLiterals [] = ""
printLiterals (literal : []) = printLiteral literal
printLiterals (literal : literals) = printLiteral literal ++ ", " ++ printLiterals literals

printLiteral :: Literal -> String
printLiteral (LiteralStr str) = "\"" ++ str ++ "\""
printLiteral (LiteralInt int) = show int
printLiteral (LiteralBool True) = "true"
printLiteral (LiteralBool False) = "false"
printLiteral (LiteralNull) = "null"

printLabels :: [Label] -> String
printLabels [] = ""
printLabels (label : []) = printLabel label
printLabels (label : labels) = printLabel label ++ ";" ++ printLabels labels

printLabel :: Label -> String
printLabel (Label str) = str

noParse :: ErrorCall -> IO ()
noParse e = do let err =  show e
               hPutStr stderr ("Problem with parsing: " ++ err)
               return ()

noLex :: ErrorCall -> IO ()
noLex e = do let err =  show e
             hPutStr stderr ("Problem with lexing: " ++ err)
             return ()