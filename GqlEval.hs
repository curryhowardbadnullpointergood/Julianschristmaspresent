module GqlEval where
import InputLexer
import InputParser
import System.Environment ( getArgs )
import Control.Exception
import System.IO

sample = File
            [NodeSet
                (NodeHeader [Field "bonus" TypeInteger,Field "business" TypeString] True)
                    [NodeEntry "com9" [LiteralInt 25,LiteralStr "TheLaughingOnion"] [Label "Restaurant"]
                    ,NodeEntry "com8" [LiteralInt 30,LiteralStr "TheAngryOnion"] [Label "Restaurant"]
                    ,NodeEntry "com7" [LiteralInt 10,LiteralStr "BaaBaaBlackSheep"] [Label "Barber"]
                    ,NodeEntry "com6" [LiteralInt 25,LiteralStr "DoughReMe"] [Label "Pizzeria"]
                    ,NodeEntry "com5" [LiteralInt 15,LiteralStr "CrustInUs"] [Label "Pizzeria"]
                    ,NodeEntry "com4" [LiteralInt 10,LiteralStr "TreatyEats"] [Label "Delicatessen"]
                    ,NodeEntry "com3" [LiteralInt 20,LiteralStr "CoffeeNumberTwo"] [Label "Cafe"]
                    ,NodeEntry "com2" [LiteralInt 15,LiteralStr "NaffeCero"] [Label "Cafe"]
                    ,NodeEntry "com1" [LiteralInt 10,LiteralStr "BarStucks"] [Label "Cafe"]
                    ]
            ,NodeSet
                (NodeHeader [Field "age" TypeInteger,Field "familyname" TypeString,Field "firstname" TypeString] False)
                    [NodeEntry "cr2" [LiteralInt 15,LiteralStr "Flaherty",LiteralStr "Connor"] []
                    ,NodeEntry "as4" [LiteralInt 40,LiteralStr "Sharma",LiteralStr "Anika"] []
                    ,NodeEntry "mw3" [LiteralInt 47,LiteralStr "Wu",LiteralStr "Mei"] []
                    ,NodeEntry "bk21" [LiteralInt 70,LiteralStr "King",LiteralStr "Barbara"] []
                    ,NodeEntry "rw11" [LiteralInt 66,LiteralStr "Wise",LiteralStr "Ray"] []
                    ,NodeEntry "jd6" [LiteralInt 16,LiteralStr "Ding",LiteralStr "Jing"] []
                    ,NodeEntry "pp8" [LiteralInt 17,LiteralStr "Potter",LiteralStr "Peter"] []
                    ,NodeEntry "rw5" [LiteralInt 15,LiteralStr "Watson",LiteralStr "Rebecca"] []
                    ,NodeEntry "nj10" [LiteralInt 16,LiteralStr "Jackson",LiteralStr "Nigel"] []
                    ,NodeEntry "ab23" [LiteralInt 22,LiteralStr "Baker",LiteralStr "Adam"] []
                    ,NodeEntry "jv9" [LiteralInt 49,LiteralStr "Villeneuve",LiteralStr "Jennifer"] []
                    ,NodeEntry "gt2" [LiteralInt 23,LiteralStr "Truffaut",LiteralStr "Guillaume"] []
                    ,NodeEntry "uh12" [LiteralInt 55,LiteralStr "Habib",LiteralStr "Umar"] []
                    ,NodeEntry "jj23" [LiteralInt 43,LiteralStr "Jones",LiteralStr "John"] []
                    ]
            ]
            [RelationshipSet
                (RelationshipHeader [])
                    [RelationshipEntry "cr2" [] "pp8" "Recommended"
                    ,RelationshipEntry "cr2" [] "gt2" "Recommended"
                    ,RelationshipEntry "as4" [] "mw3" "Recommended"
                    ,RelationshipEntry "bk21" [] "rw11" "Recommended"
                    ,RelationshipEntry "rw5" [] "mw3" "Recommended"
                    ,RelationshipEntry "nj10" [] "jd6" "Recommended"
                    ,RelationshipEntry "ab23" [] "cr2" "Recommended"
                    ,RelationshipEntry "jv9" [] "as4" "Recommended"
                    ,RelationshipEntry "gt2" [] "jd6" "Recommended"
                    ,RelationshipEntry "uh12" [] "jv9" "Recommended"
                    ,RelationshipEntry "uh12" [] "as4" "Recommended"
                    ,RelationshipEntry "jj23" [] "bk21" "Recommended"
                    ,RelationshipEntry "jj23" [] "rw5" "Recommended"
                    ]
            ,RelationshipSet
                (RelationshipHeader [Field "reward" TypeInteger])
                    [RelationshipEntry "cr2" [LiteralInt 50] "com6" "CustomerOf"
                    ,RelationshipEntry "as4" [LiteralInt 63] "com1" "CustomerOf"
                    ,RelationshipEntry "mw3" [LiteralInt 3] "com2" "CustomerOf"
                    ,RelationshipEntry "bk21" [LiteralInt 62] "com4" "CustomerOf"
                    ,RelationshipEntry "rw11" [LiteralInt 43] "com7" "CustomerOf"
                    ,RelationshipEntry "pp8" [LiteralInt 86] "com6" "CustomerOf"
                    ,RelationshipEntry "rw5" [LiteralInt 22] "com3" "CustomerOf"
                    ,RelationshipEntry "rw5" [LiteralInt 2] "com4" "CustomerOf"
                    ,RelationshipEntry "nj10" [LiteralInt 26] "com3" "CustomerOf"
                    ,RelationshipEntry "ab23" [LiteralInt 23] "com8" "CustomerOf"
                    ,RelationshipEntry "jd6" [LiteralInt 11] "com1" "CustomerOf"
                    ,RelationshipEntry "jv9" [LiteralInt 0] "com5" "CustomerOf"
                    ,RelationshipEntry "jv9" [LiteralInt 22] "com9" "CustomerOf"
                    ,RelationshipEntry "gt2" [LiteralInt 12] "com1" "CustomerOf"
                    ,RelationshipEntry "uh12" [LiteralInt 24] "com5" "CustomerOf"
                    ,RelationshipEntry "uh12" [LiteralInt 24] "com2" "CustomerOf"
                    ,RelationshipEntry "uh12" [LiteralInt 24] "com8" "CustomerOf"
                    ,RelationshipEntry "jj23" [LiteralInt 5] "com3" "CustomerOf"
                    ,RelationshipEntry "jj23" [LiteralInt 12] "com2" "CustomerOf"
                    ,RelationshipEntry "jj23" [LiteralInt 72] "com5" "CustomerOf"
                    ]
            ]


extractJust :: Maybe a -> a
extractJust (Just i) = i

extractLiteralStr :: Literal -> String
extractLiteralStr (LiteralStr s) = s
extractLiteralInt :: Literal -> Int
extractLiteralInt (LiteralInt i) = i
extractLiteralBool :: Literal -> Bool
extractLiteralBool (LiteralBool b) = b
extractLabel :: Label -> String
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
filterLabel :: [(String, Labels)] -> (String -> Bool) -> [String]
filterLabel input predicate = map fst $ filterLabel' input predicate
filterLabel' :: [(String, Labels)] -> (String -> Bool) -> [(String, Labels)]
filterLabel' [] _  = []
filterLabel' (pair@(string, labels):literalPairs) f
    | contains labels f = pair : filterLabel' literalPairs f
    | otherwise = filterLabel' literalPairs f
    where 
        contains :: Labels -> (String -> Bool) -> Bool
        contains [] _ = False
        contains (a : b) f 
            | f (extractLabel a) = True
            | otherwise = contains b f    



-- Returns a list of tuples containing the ID and the labels for each Node
-- eg getLabels file = [("id1",[]),("id2",[Label "label1"]),("id3",[Label "label1",Label "label3"])]
getLabels :: File -> [(String,Labels)]
getLabels (File nodeSets _) = getLabelsNodeSets nodeSets
getLabelsNodeSets :: [NodeSet] -> [(String, Labels)]
getLabelsNodeSets [] = []
getLabelsNodeSets (nodeSet:nodeSets) = getLabelsNodeSet nodeSet ++ getLabelsNodeSets nodeSets
getLabelsNodeSet :: NodeSet -> [(String, Labels)]
getLabelsNodeSet (NodeSet _ []) = [] 
getLabelsNodeSet (NodeSet _ entries) = getLabelsEntries entries
getLabelsEntries :: [NodeEntry] -> [(String, Labels)]
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
getFieldColumn :: Fields -> String -> Int -> Maybe Int
getFieldColumn [] _ _ = Nothing
getFieldColumn (field@(Field string _):fields) s i
    | string == s = Just i
    | otherwise = getFieldColumn fields s (i+1)



-- returns a list containing all of the id values of each table, each table of ID is in its own list of values
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



-- -- Allows searching for a single record in a node set table by using an ID
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



removeDuplicates [] = []
removeDuplicates (a:as) = a : removeDuplicates (filter (\x -> x /= a) as) 


unionLists a b = removeDuplicates $ a ++ b

interSectionLists [] _ = []
interSectionLists _ [] = []
interSectionLists a b = removeDuplicates $ filter (\x -> elem x a) b

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