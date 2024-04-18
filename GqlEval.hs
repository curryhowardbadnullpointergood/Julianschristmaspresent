module GqlEval where
import DataLexer ( FieldType(..) )
import DataParser
    ( Field(..),
      File(..),
      Label(..),
      Literal(..),
      NodeEntries,
      NodeEntry(..),
      NodeHeader(..),
      NodeSet(..),
      NodeSets,
      RelationshipEntry(..),
      RelationshipHeader(..),
      RelationshipSet(..) )
import System.Environment ( getArgs )
import Control.Exception
import System.IO

xy :: File -> [String]  -> [Maybe NodeEntry]
xy f ss = map (returnNodeRecord f) ss

extractJust :: Maybe a -> a
extractJust (Just i) = i

extractLiteralStr :: Literal -> String
extractLiteralStr (LiteralStr s) = s

extractLiteralInt :: Literal -> Int
extractLiteralInt (LiteralInt i) = i

extractLiteralBool :: Literal -> Bool
extractLiteralBool (LiteralBool b) = b
extractLabel (Label s) = s

isLiteralNull :: Literal -> Bool
isLiteralNull (LiteralNull) = True
isLiteralNull (LiteralStr s) = False
isLiteralNull (LiteralInt i) = False
isLiteralNull (LiteralBool b) = False

-- Filter functions work by taking a list of ids and a literal value for a certain field, 
-- a predicate to filter on aswell as a boolean to decide what to do with null literal values 
-- (true for keep false for reject)

filterIntField :: [(String, Literal)] -> (Int -> Bool) -> Bool -> [String]
filterIntField input predicate keepNulls = map fst $ filterIntField' input predicate keepNulls

filterIntField' :: [(String, Literal)] -> (Int -> Bool) -> Bool -> [(String, Literal)]
filterIntField' [] _ _ = []
filterIntField' (pair@(string, LiteralInt x):literalPairs) f keepNulls
    | f x = pair : filterIntField' literalPairs f keepNulls
    | otherwise = filterIntField' literalPairs f keepNulls
filterIntField' (pair@(string, LiteralNull):literalPairs) f keepNulls
    | keepNulls = pair : filterIntField' literalPairs f keepNulls
    | otherwise = filterIntField' literalPairs f keepNulls

filterBoolField :: [(String, Literal)] -> (Bool -> Bool) -> Bool -> [String]
filterBoolField input predicate keepNulls = map fst $ filterBoolField' input predicate keepNulls

filterBoolField' :: [(String, Literal)] -> (Bool -> Bool) -> Bool -> [(String, Literal)]
filterBoolField' [] _ _ = []
filterBoolField' (pair@(string, LiteralBool b):literalPairs) f keepNulls
    | f b = pair : filterBoolField' literalPairs f keepNulls
    | otherwise = filterBoolField' literalPairs f keepNulls
filterBoolField' (pair@(string, LiteralNull):literalPairs) f keepNulls
    | keepNulls = pair : filterBoolField' literalPairs f keepNulls
    | otherwise = filterBoolField' literalPairs f keepNulls

filterStringField :: [(String, Literal)] -> (String -> Bool) -> Bool -> [String]
filterStringField input predicate keepNulls = map fst $ filterStringField' input predicate keepNulls

filterStringField' :: [(String, Literal)] -> (String -> Bool) -> Bool -> [(String, Literal)]
filterStringField' [] _ _ = []
filterStringField' (pair@(string, LiteralStr s):literalPairs) f keepNulls
    | f s = pair : filterStringField' literalPairs f keepNulls
    | otherwise = filterStringField' literalPairs f keepNulls
filterStringField' (pair@(string, LiteralNull):literalPairs) f keepNulls
    | keepNulls = pair : filterStringField' literalPairs f keepNulls
    | otherwise = filterStringField' literalPairs f keepNulls

-- function for filtering all the nodes in a field out that arent null values 

filterNullFieldValues :: [(String, Literal)] -> [String]
filterNullFieldValues input = map fst $ filterNullFieldValues' input

filterNullFieldValues' :: [(String, Literal)] -> [(String, Literal)]
filterNullFieldValues' [] = []
filterNullFieldValues' (pair@(string, literal):literalPairs)
    | isLiteralNull literal = pair : filterNullFieldValues' literalPairs
    | otherwise = filterNullFieldValues' literalPairs 

-- Function for retrieving all the nodes with a certain label

filterLabel :: [(String, [Label])] -> (String -> Bool) -> [String]
filterLabel input predicate = map fst $ filterLabel' input predicate

filterLabel' :: [(String, [Label])] -> (String -> Bool) -> [(String, [Label])]
filterLabel' [] _  = []
filterLabel' (pair@(string, labels):literalPairs) f
    | contains labels f = pair : filterLabel' literalPairs f
    | otherwise = filterLabel' literalPairs f
    where 
        contains :: [Label] -> (String -> Bool) -> Bool
        contains [] _ = False
        contains (a : b) f 
            | f (extractLabel a) = True
            | otherwise = contains b f    

-- Returns a list of tuples containing the ID and the labels for each Node
-- eg getLabels file = [("id1",[]),("id2",[Label "label1"]),("id3",[Label "label1",Label "label3"])]

getLabels :: File -> [(String,[Label])]
getLabels (File nodeSets _) = getLabelsNodeSets nodeSets

getLabelsNodeSets :: [NodeSet] -> [(String, [Label])]
getLabelsNodeSets [] = []
getLabelsNodeSets (nodeSet:nodeSets) = getLabelsNodeSet nodeSet ++ getLabelsNodeSets nodeSets

getLabelsNodeSet :: NodeSet -> [(String, [Label])]
getLabelsNodeSet (NodeSet _ []) = [] 
getLabelsNodeSet (NodeSet _ entries) = getLabelsEntries entries

getLabelsEntries :: [NodeEntry] -> [(String, [Label])]
getLabelsEntries [] = []
getLabelsEntries ((NodeEntry nodeID _ labels):nodeEntries) = (nodeID, labels) : getLabelsEntries nodeEntries

-- Returns a list of tuples containing the ID and the value of the literal for each Node of a certain field
-- eg getField file "age" = [("user1",LiteralInt 23), ("user2",LiteralInt 35), ("user3",LiteralNull)]

getField :: File -> String -> [(String, Literal)]
getField (File nodeSets _) s = getFieldNodeSets nodeSets s

getFieldNodeSets :: NodeSets -> String -> [(String, Literal)]
getFieldNodeSets [] _ = []
getFieldNodeSets (nodeSet:nodeSets) s = getFieldNodeSet nodeSet s ++ getFieldNodeSets nodeSets s

getFieldNodeSet :: NodeSet -> String -> [(String, Literal)]
getFieldNodeSet (NodeSet (NodeHeader fields _) nodeEntries) s
    | col == Nothing = []
    | otherwise = getFieldNodeEntries nodeEntries (extractJust col)
    where
        col = getFieldColumn fields s 0

getFieldNodeEntries :: NodeEntries -> Int -> [(String, Literal)]
getFieldNodeEntries [] _ = []
getFieldNodeEntries ((NodeEntry nodeID literals _):entries) col = (nodeID, literals !! col) : getFieldNodeEntries entries col

-- gets the index of a field name in the header

getFieldColumn :: [Field] -> String -> Int -> Maybe Int
getFieldColumn [] _ _ = Nothing
getFieldColumn (field@(Field string _):fields) s i
    | string == s = Just i
    | otherwise = getFieldColumn fields s (i+1)

-- Return

returnIDValues :: File -> [[String]]
returnIDValues (File nodeSets _) = returnIDValues' nodeSets

returnIDValues' :: NodeSets -> [[String]]
returnIDValues' [] = []
returnIDValues' (nodeSet : nodeSets) = returnIDValues'' nodeSet : returnIDValues' nodeSets

returnIDValues'' :: NodeSet -> [String]
returnIDValues'' (NodeSet _ nodeEntries) = returnIDValues''' nodeEntries

returnIDValues''' :: NodeEntries -> [String]
returnIDValues''' [] = []
returnIDValues''' (nodeEntry : nodeEntries) = returnIDValues'''' nodeEntry : returnIDValues''' nodeEntries

returnIDValues'''' :: NodeEntry -> String
returnIDValues'''' (NodeEntry s _ _) = s

returnNodeRecord :: File -> String -> Maybe NodeEntry
returnNodeRecord (File nodeSets _) s = returnNodeRecord' nodeSets s

returnNodeRecord' :: NodeSets -> String -> Maybe NodeEntry
returnNodeRecord' [] _ = Nothing
returnNodeRecord' (nodeSet : nodeSets) s
    | returnNodeRecord'' nodeSet s /= Nothing = returnNodeRecord'' nodeSet s
    | otherwise = returnNodeRecord' nodeSets s

returnNodeRecord'' :: NodeSet -> String -> Maybe NodeEntry
returnNodeRecord'' (NodeSet _ nodeEntries) s
    | returnNodeRecord''' nodeEntries s /= Nothing = returnNodeRecord''' nodeEntries s
    | otherwise = returnNodeRecord''' nodeEntries s

returnNodeRecord''' :: NodeEntries -> String -> Maybe NodeEntry
returnNodeRecord''' [] _ = Nothing
returnNodeRecord''' (nodeEntry : nodeEntries) s
    | returnNodeRecord'''' nodeEntry s /= Nothing = returnNodeRecord'''' nodeEntry s
    | otherwise = returnNodeRecord''' nodeEntries s

returnNodeRecord'''' :: NodeEntry -> String -> Maybe NodeEntry
returnNodeRecord'''' nodeEntry@(NodeEntry s' _ _ ) s
    | s' == s = Just nodeEntry
    | otherwise = Nothing

-- Print

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