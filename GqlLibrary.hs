module GqlLibrary where
import InputLexer
import InputParser

import Data.Maybe
import Control.Monad
import Data.List
import System.Environment ( getArgs )
import Control.Exception
import System.IO

---------------------------------------------------------------------------------------------------
-- Alias Definitions
---------------------------------------------------------------------------------------------------
type Nodes = [(NodeHeader, NodeEntry)]
type Node = (NodeHeader, NodeEntry)
type Relations = [(RelationshipHeader, RelationshipEntry)]
type Relation = (RelationshipHeader, RelationshipEntry)
---------------------------------------------------------------------------------------------------
extractLiteralStr :: Literal -> Maybe String
extractLiteralStr (LiteralStr s) = Just s
extractLiteralStr LiteralNull = Nothing
extractLiteralInt :: Literal -> Maybe Int
extractLiteralInt (LiteralInt i) = Just i
extractLiteralInt LiteralNull = Nothing
extractLiteralBool :: Literal -> Maybe Bool
extractLiteralBool (LiteralBool b) = Just b
extractLiteralBool LiteralNull = Nothing
---------------------------------------------------------------------------------------------------
-- Combining Lists
---------------------------------------------------------------------------------------------------
removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates [] = []
removeDuplicates (a:as) = a : removeDuplicates (filter (\b -> b /= a) as) 

unionLists :: Eq a => [a] -> [a] -> [a]
unionLists a b = removeDuplicates $ a ++ b

interSectionLists :: Eq a => [a] -> [a] -> [a]
interSectionLists [] _ = []
interSectionLists _ [] = []
interSectionLists a b = removeDuplicates $ filter (\c -> elem c a) b
---------------------------------------------------------------------------------------------------
-- Retrieval Functions
---------------------------------------------------------------------------------------------------
getNodes :: File -> Nodes
getNodes (File nodeSets _) = concatMap getNodesFromSet nodeSets
getNodesFromSet :: NodeSet -> [(NodeHeader, NodeEntry)]
getNodesFromSet (NodeSet header entries) = map (\entry -> (header, entry)) entries

getRelations :: File -> Relations
getRelations (File _ relationshipSets) = concatMap getRelationshipsFromSet relationshipSets
getRelationshipsFromSet :: RelationshipSet -> [(RelationshipHeader, RelationshipEntry)]
getRelationshipsFromSet (RelationshipSet header entries) = map (\entry -> (header, entry)) entries
---------------------------------------------------------------------------------------------------
-- Filtering Nodes
---------------------------------------------------------------------------------------------------
findLiteralByFieldNameNodes :: Fields -> Literals -> String -> Maybe Literal
findLiteralByFieldNameNodes fields literals fieldName = do
    fieldIndex <- findIndex (\(Field fName _) -> fName == fieldName) fields
    guard (fieldIndex < length literals)
    return (literals !! fieldIndex)

filterIntNodes :: [(NodeHeader, NodeEntry)] -> String -> (Int -> Bool) -> [(NodeHeader, NodeEntry)]
filterIntNodes nodeEntries fieldName predicate =
    filter matchesIntCondition nodeEntries
  where
    matchesIntCondition :: (NodeHeader, NodeEntry) -> Bool
    matchesIntCondition (NodeHeader fields _, NodeEntry _ literals _) =
      case findLiteralByFieldNameNodes fields literals fieldName of
        Just (LiteralInt value) -> predicate value
        _ -> False

filterBoolNodes :: [(NodeHeader, NodeEntry)] -> String -> (Bool -> Bool) -> [(NodeHeader, NodeEntry)]
filterBoolNodes nodeEntries fieldName predicate =
    filter matchesBoolCondition nodeEntries
  where
    matchesBoolCondition :: (NodeHeader, NodeEntry) -> Bool
    matchesBoolCondition (NodeHeader fields _, NodeEntry _ literals _) =
      case findLiteralByFieldNameNodes fields literals fieldName of
        Just (LiteralBool value) -> predicate value
        _ -> False

filterStringNodes :: [(NodeHeader, NodeEntry)] -> String -> (String -> Bool) -> [(NodeHeader, NodeEntry)]
filterStringNodes nodeEntries fieldName predicate =
    filter matchesStringCondition nodeEntries
  where
    matchesStringCondition :: (NodeHeader, NodeEntry) -> Bool
    matchesStringCondition (NodeHeader fields _, NodeEntry _ literals _) =
      case findLiteralByFieldNameNodes fields literals fieldName of
        Just (LiteralStr value) -> predicate value
        _ -> False

---------------------------------------------------------------------------------------------------
-- Filtering Relations
---------------------------------------------------------------------------------------------------

findLiteralByFieldNameRelations :: Fields -> Literals -> String -> Maybe Literal
findLiteralByFieldNameRelations fields literals fieldName = do
    fieldIndex <- findIndex (\(Field fName _) -> fName == fieldName) fields
    guard (fieldIndex < length literals)
    return (literals !! fieldIndex)

filterIntRelations :: [(RelationshipHeader, RelationshipEntry)] -> String -> (Int -> Bool) -> [(RelationshipHeader, RelationshipEntry)]
filterIntRelations relationships fieldName predicate =
    filter matchesIntCondition relationships
  where
    matchesIntCondition :: (RelationshipHeader, RelationshipEntry) -> Bool
    matchesIntCondition (RelationshipHeader fields, RelationshipEntry _ literals _ _) =
      case findLiteralByFieldNameRelations fields literals fieldName of
        Just (LiteralInt value) -> predicate value
        _ -> False

filterBoolRelations :: [(RelationshipHeader, RelationshipEntry)] -> String -> (Bool -> Bool) -> [(RelationshipHeader, RelationshipEntry)]
filterBoolRelations relationships fieldName predicate =
    filter matchesBoolCondition relationships
  where
    matchesBoolCondition :: (RelationshipHeader, RelationshipEntry) -> Bool
    matchesBoolCondition (RelationshipHeader fields, RelationshipEntry _ literals _ _) =
      case findLiteralByFieldNameRelations fields literals fieldName of
        Just (LiteralBool value) -> predicate value
        _ -> False

filterStringRelations :: [(RelationshipHeader, RelationshipEntry)] -> String -> (String -> Bool) -> [(RelationshipHeader, RelationshipEntry)]
filterStringRelations relationships fieldName predicate =
    filter matchesStringCondition relationships
  where
    matchesStringCondition :: (RelationshipHeader, RelationshipEntry) -> Bool
    matchesStringCondition (RelationshipHeader fields, RelationshipEntry _ literals _ _) =
      case findLiteralByFieldNameRelations fields literals fieldName of
        Just (LiteralStr value) -> predicate value
        _ -> False

---------------------------------------------------------------------------------------------------
-- Printing
---------------------------------------------------------------------------------------------------
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