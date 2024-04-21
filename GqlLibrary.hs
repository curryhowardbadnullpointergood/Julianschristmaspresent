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
-- Being Currently Tested
---------------------------------------------------------------------------------------------------

extractNodeIDs :: Nodes -> [String]
extractNodeIDs nodes = map (\(_, NodeEntry id _ _) -> id) nodes

extractNodeLabels :: Nodes -> [String]
extractNodeLabels nodes = concatMap extractLabelsAsString nodes
  where
    extractLabelsAsString (_, NodeEntry _ _ labels) = map (\(Label s) -> s) labels

extractRelationStartIDs :: Relations -> [String]
extractRelationStartIDs relationships = map (\(_, RelationshipEntry startId _ _ _) -> startId) relationships

extractRelationEndIDs :: Relations -> [String]
extractRelationEndIDs relationships = map (\(_, RelationshipEntry _ _ endId _) -> endId) relationships

extractRelationTypes :: Relations -> [String]
extractRelationTypes relationships = map (\(_, RelationshipEntry _ _ _ typeLabel) -> typeLabel) relationships

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
getAll :: File -> (Nodes, Relations)
getAll file = (getNodes file, getRelations file)

getNodes :: File -> Nodes
getNodes (File nodeSets _) = concatMap getNodesFromSet nodeSets
getNodesFromSet :: NodeSet -> [(NodeHeader, NodeEntry)]
getNodesFromSet (NodeSet header entries) = map (\entry -> (header, entry)) entries

getRelations :: File -> Relations
getRelations (File _ relationshipSets) = concatMap getRelationsFromSet relationshipSets
getRelationsFromSet :: RelationshipSet -> [(RelationshipHeader, RelationshipEntry)]
getRelationsFromSet (RelationshipSet header entries) = map (\entry -> (header, entry)) entries
---------------------------------------------------------------------------------------------------
-- Filtering Nodes
---------------------------------------------------------------------------------------------------
findLiteralByFieldNameNodes :: Fields -> Literals -> String -> Maybe Literal
findLiteralByFieldNameNodes fields literals fieldName = do
    fieldIndex <- findIndex (\(Field fName _) -> fName == fieldName) fields
    guard (fieldIndex < length literals)
    return (literals !! fieldIndex)

filterIntNodes :: Nodes -> String -> (Int -> Bool) -> Nodes
filterIntNodes nodeEntries fieldName predicate =
    filter matchesIntCondition nodeEntries
  where
    matchesIntCondition :: Node -> Bool
    matchesIntCondition (NodeHeader fields _, NodeEntry _ literals _) =
      case findLiteralByFieldNameNodes fields literals fieldName of
        Just (LiteralInt value) -> predicate value
        _ -> False

filterBoolNodes :: Nodes -> String -> (Bool -> Bool) -> Nodes
filterBoolNodes nodeEntries fieldName predicate =
    filter matchesBoolCondition nodeEntries
  where
    matchesBoolCondition :: Node -> Bool
    matchesBoolCondition (NodeHeader fields _, NodeEntry _ literals _) =
      case findLiteralByFieldNameNodes fields literals fieldName of
        Just (LiteralBool value) -> predicate value
        _ -> False

filterStringNodes :: Nodes -> String -> (String -> Bool) -> Nodes
filterStringNodes nodeEntries fieldName predicate =
    filter matchesStringCondition nodeEntries
  where
    matchesStringCondition :: Node -> Bool
    matchesStringCondition (NodeHeader fields _, NodeEntry _ literals _) =
      case findLiteralByFieldNameNodes fields literals fieldName of
        Just (LiteralStr value) -> predicate value
        _ -> False

filterByIDNodes :: Nodes -> (String -> Bool) -> Nodes
filterByIDNodes nodeEntries predicate =
    filter matchesIDCondition nodeEntries
  where
    matchesIDCondition (_, NodeEntry id _ _) = predicate id

filterByLabelNodes :: Nodes -> (String -> Bool) -> Nodes
filterByLabelNodes nodeEntries predicate =
    filter matchesLabelCondition nodeEntries
  where
    matchesLabelCondition (_, NodeEntry _ _ labels) =
        any (predicate . labelToString) labels
labelToString :: Label -> String
labelToString (Label s) = s

---------------------------------------------------------------------------------------------------
-- Filtering Relations
---------------------------------------------------------------------------------------------------

findLiteralByFieldNameRelations :: Fields -> Literals -> String -> Maybe Literal
findLiteralByFieldNameRelations fields literals fieldName = do
    fieldIndex <- findIndex (\(Field fName _) -> fName == fieldName) fields
    guard (fieldIndex < length literals)
    return (literals !! fieldIndex)

filterIntRelations :: Relations -> String -> (Int -> Bool) -> Relations
filterIntRelations relationships fieldName predicate =
    filter matchesIntCondition relationships
  where
    matchesIntCondition :: Relation -> Bool
    matchesIntCondition (RelationshipHeader fields, RelationshipEntry _ literals _ _) =
      case findLiteralByFieldNameRelations fields literals fieldName of
        Just (LiteralInt value) -> predicate value
        _ -> False

filterBoolRelations :: Relations -> String -> (Bool -> Bool) -> Relations
filterBoolRelations relationships fieldName predicate =
    filter matchesBoolCondition relationships
  where
    matchesBoolCondition :: Relation -> Bool
    matchesBoolCondition (RelationshipHeader fields, RelationshipEntry _ literals _ _) =
      case findLiteralByFieldNameRelations fields literals fieldName of
        Just (LiteralBool value) -> predicate value
        _ -> False

filterStringRelations :: Relations -> String -> (String -> Bool) -> Relations
filterStringRelations relationships fieldName predicate =
    filter matchesStringCondition relationships
  where
    matchesStringCondition :: Relation -> Bool
    matchesStringCondition (RelationshipHeader fields, RelationshipEntry _ literals _ _) =
      case findLiteralByFieldNameRelations fields literals fieldName of
        Just (LiteralStr value) -> predicate value
        _ -> False

filterByStartIDRelations :: Relations -> (String -> Bool) -> Relations
filterByStartIDRelations relationships predicate =
    filter matchesStartIDCondition relationships
  where
    matchesStartIDCondition (_, RelationshipEntry startId _ _ _) = predicate startId

filterByEndIDRelations :: Relations -> (String -> Bool) -> Relations
filterByEndIDRelations relationships predicate =
    filter matchesEndIDCondition relationships
  where
    matchesEndIDCondition (_, RelationshipEntry _ _ endId _) = predicate endId

filterByTypeRelations :: Relations -> (String -> Bool) -> Relations
filterByTypeRelations relationships predicate =
    filter matchesTypeCondition relationships
  where
    matchesTypeCondition (_, RelationshipEntry _ _ _ typeLabel) = predicate typeLabel

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