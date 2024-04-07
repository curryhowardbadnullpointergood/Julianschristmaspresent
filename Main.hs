import Lexer
import Parser
import System.Environment ( getArgs )
import Control.Exception
import System.IO


main :: IO ()
main = catch main' noParse


main' = do
    -- (fileName : _ ) <- getArgs
    -- sourceText <- readFile fileName
    -- putStrLn ("Parsing : " ++ sourceText)
    -- let lexedProg = alexScanTokens sourceText
    -- -- putStrLn ("lexed as " ++ show lexedProg)
    -- let parsedProg = inputParser lexedProg
    -- putStrLn ("Parsed as " ++ show parsedProg)
    let result1 = returnNodeRecord sample "com11"
    let result2 = returnNodeRecord sample "com1"
    let result3 = returnNodeRecord sample "as4"
    let result4 = returnNodeRecord sample "jj23"
    let result5 = returnNodeRecord sample "ab23"
    let result6 = returnNodeRecord sample "com9"
    let result7 = returnIDValues sample
    print result1
    print result2
    print result3
    print result4
    print result5
    print result6
    print result7
    print $ xy sample (head result7)
    -- print result8
    -- print result9

sample  :: File
sample  = File
    (NodeSets
        (NodeSet
            (NodeHeader
                (Fields (Field "bonus" TypeInteger) (Fields (Field "business" TypeString) EmptyField)) True)
            (NodeEntries (NodeEntryLabel "com9" (Literals (LiteralInt 25) (Literals (LiteralStr "TheLaughingOnion") EmptyLiteral)) (Labels (Label "Restaurant") EmptyLabel))
            (NodeEntries (NodeEntryLabel "com8" (Literals (LiteralInt 30) (Literals (LiteralStr "TheAngryOnion") EmptyLiteral)) (Labels (Label "Restaurant") EmptyLabel))
            (NodeEntries (NodeEntryLabel "com7" (Literals (LiteralInt 10) (Literals (LiteralStr "BaaBaaBlackSheep") EmptyLiteral)) (Labels (Label "Barber") EmptyLabel))
            (NodeEntries (NodeEntryLabel "com6" (Literals (LiteralInt 25) (Literals (LiteralStr "DoughReMe") EmptyLiteral)) (Labels (Label "Pizzeria") EmptyLabel))
            (NodeEntries (NodeEntryLabel "com5" (Literals (LiteralInt 15) (Literals (LiteralStr "CrustInUs") EmptyLiteral)) (Labels (Label "Pizzeria") EmptyLabel))
            (NodeEntries (NodeEntryLabel "com4" (Literals (LiteralInt 10) (Literals (LiteralStr "TreatyEats") EmptyLiteral)) (Labels (Label "Delicatessen") EmptyLabel))
            (NodeEntries (NodeEntryLabel "com3" (Literals (LiteralInt 20) (Literals (LiteralStr "CoffeeNumberTwo") EmptyLiteral)) (Labels (Label "Cafe") EmptyLabel))
            (NodeEntries (NodeEntryLabel "com2" (Literals (LiteralInt 15) (Literals (LiteralStr "NaffeCero") EmptyLiteral)) (Labels (Label "Cafe") EmptyLabel))
            (NodeEntries (NodeEntryLabel "com1" (Literals (LiteralInt 10) (Literals (LiteralStr "BarStucks") EmptyLiteral)) (Labels (Label "Cafe") EmptyLabel))
        EmptyNodeEntry))))))))))
    (NodeSets
        (NodeSet
            (NodeHeader
                (Fields (Field "age" TypeInteger) (Fields (Field "familyname" TypeString) (Fields (Field "firstname" TypeString) EmptyField))) False) 
            (NodeEntries (NodeEntry "cr2" (Literals (LiteralInt 15) (Literals (LiteralStr "Flaherty") (Literals (LiteralStr "Connor") EmptyLiteral))))
            (NodeEntries (NodeEntry "as4" (Literals (LiteralInt 40) (Literals (LiteralStr "Sharma") (Literals (LiteralStr "Anika") EmptyLiteral)))) 
            (NodeEntries (NodeEntry "mw3" (Literals (LiteralInt 47) (Literals (LiteralStr "Wu") (Literals (LiteralStr "Mei") EmptyLiteral))))
            (NodeEntries (NodeEntry "bk21" (Literals (LiteralInt 70) (Literals (LiteralStr "King") (Literals (LiteralStr "Barbara") EmptyLiteral)))) 
            (NodeEntries (NodeEntry "rw11" (Literals (LiteralInt 66) (Literals (LiteralStr "Wise") (Literals (LiteralStr "Ray") EmptyLiteral))))
            (NodeEntries (NodeEntry "jd6" (Literals (LiteralInt 16) (Literals (LiteralStr "Ding") (Literals (LiteralStr "Jing") EmptyLiteral)))) 
            (NodeEntries (NodeEntry "pp8" (Literals (LiteralInt 17) (Literals (LiteralStr "Potter") (Literals (LiteralStr "Peter") EmptyLiteral))))
            (NodeEntries (NodeEntry "rw5" (Literals (LiteralInt 15) (Literals (LiteralStr "Watson") (Literals (LiteralStr "Rebecca") EmptyLiteral)))) 
            (NodeEntries (NodeEntry "nj10" (Literals (LiteralInt 16) (Literals (LiteralStr "Jackson") (Literals (LiteralStr "Nigel") EmptyLiteral))))
            (NodeEntries (NodeEntry "ab23" (Literals (LiteralInt 22) (Literals (LiteralStr "Baker") (Literals (LiteralStr "Adam") EmptyLiteral)))) 
            (NodeEntries (NodeEntry "jv9" (Literals (LiteralInt 49) (Literals (LiteralStr "Villeneuve") (Literals (LiteralStr "Jennifer") EmptyLiteral))))
            (NodeEntries (NodeEntry "gt2" (Literals (LiteralInt 23) (Literals (LiteralStr "Truffaut") (Literals (LiteralStr "Guillaume") EmptyLiteral)))) 
            (NodeEntries (NodeEntry "uh12" (Literals (LiteralInt 55) (Literals (LiteralStr "Habib") (Literals (LiteralStr "Umar") EmptyLiteral))))
            (NodeEntries (NodeEntry "jj23" (Literals (LiteralInt 43) (Literals (LiteralStr "Jones") (Literals (LiteralStr "John") EmptyLiteral))))
        EmptyNodeEntry)))))))))))))))
    EmptyNodeSet))
    (RelationshipSets
        (RelationshipSet
            (RelationshipHeader
                EmptyField)
            (RelationshipEntries (RelationshipEntry "cr2" EmptyLiteral "pp8" "Recommended")
            (RelationshipEntries (RelationshipEntry "cr2" EmptyLiteral "gt2" "Recommended")
            (RelationshipEntries (RelationshipEntry "as4" EmptyLiteral "mw3" "Recommended")
            (RelationshipEntries (RelationshipEntry "bk21" EmptyLiteral "rw11" "Recommended")
            (RelationshipEntries (RelationshipEntry "rw5" EmptyLiteral "mw3" "Recommended")
            (RelationshipEntries (RelationshipEntry "nj10" EmptyLiteral "jd6" "Recommended")
            (RelationshipEntries (RelationshipEntry "ab23" EmptyLiteral "cr2" "Recommended")
            (RelationshipEntries (RelationshipEntry "jv9" EmptyLiteral "as4" "Recommended")
            (RelationshipEntries (RelationshipEntry "gt2" EmptyLiteral "jd6" "Recommended")
            (RelationshipEntries (RelationshipEntry "uh12" EmptyLiteral "jv9" "Recommended")
            (RelationshipEntries (RelationshipEntry "uh12" EmptyLiteral "as4" "Recommended")
            (RelationshipEntries (RelationshipEntry "jj23" EmptyLiteral "bk21" "Recommended")
            (RelationshipEntries (RelationshipEntry "jj23" EmptyLiteral "rw5" "Recommended")
        EmptyRelationshipEntry))))))))))))))
    (RelationshipSets
        (RelationshipSet
            (RelationshipHeader
                (Fields (Field "reward" TypeInteger) EmptyField))
            (RelationshipEntries (RelationshipEntry "cr2" (Literals (LiteralInt 50) EmptyLiteral) "com6" "CustomerOf")
            (RelationshipEntries (RelationshipEntry "as4" (Literals (LiteralInt 63) EmptyLiteral) "com1" "CustomerOf")
            (RelationshipEntries (RelationshipEntry "mw3" (Literals (LiteralInt 3) EmptyLiteral) "com2" "CustomerOf")
            (RelationshipEntries (RelationshipEntry "bk21" (Literals (LiteralInt 62) EmptyLiteral) "com4" "CustomerOf")
            (RelationshipEntries (RelationshipEntry "rw11" (Literals (LiteralInt 43) EmptyLiteral) "com7" "CustomerOf")
            (RelationshipEntries (RelationshipEntry "pp8" (Literals (LiteralInt 86) EmptyLiteral) "com6" "CustomerOf")
            (RelationshipEntries (RelationshipEntry "rw5" (Literals (LiteralInt 22) EmptyLiteral) "com3" "CustomerOf")
            (RelationshipEntries (RelationshipEntry "rw5" (Literals (LiteralInt 2) EmptyLiteral) "com4" "CustomerOf")
            (RelationshipEntries (RelationshipEntry "nj10" (Literals (LiteralInt 26) EmptyLiteral) "com3" "CustomerOf")
            (RelationshipEntries (RelationshipEntry "ab23" (Literals (LiteralInt 23) EmptyLiteral) "com8" "CustomerOf")
            (RelationshipEntries (RelationshipEntry "jd6" (Literals (LiteralInt 11) EmptyLiteral) "com1" "CustomerOf")
            (RelationshipEntries (RelationshipEntry "jv9" (Literals (LiteralInt 0) EmptyLiteral) "com5" "CustomerOf")
            (RelationshipEntries (RelationshipEntry "jv9" (Literals (LiteralInt 22) EmptyLiteral) "com9" "CustomerOf")
            (RelationshipEntries (RelationshipEntry "gt2" (Literals (LiteralInt 12) EmptyLiteral) "com1" "CustomerOf")
            (RelationshipEntries (RelationshipEntry "uh12" (Literals (LiteralInt 24) EmptyLiteral) "com5" "CustomerOf")
            (RelationshipEntries (RelationshipEntry "uh12" (Literals (LiteralInt 24) EmptyLiteral) "com2" "CustomerOf")
            (RelationshipEntries (RelationshipEntry "uh12" (Literals (LiteralInt 24) EmptyLiteral) "com8" "CustomerOf")
            (RelationshipEntries (RelationshipEntry "jj23" (Literals (LiteralInt 5) EmptyLiteral) "com3" "CustomerOf")
            (RelationshipEntries (RelationshipEntry "jj23" (Literals (LiteralInt 12) EmptyLiteral) "com2" "CustomerOf")
            (RelationshipEntries (RelationshipEntry "jj23" (Literals (LiteralInt 72) EmptyLiteral) "com5" "CustomerOf")
        EmptyRelationshipEntry)))))))))))))))))))))
    EmptyRelationshipSet))


xy :: File -> [String]  -> [Maybe NodeEntry]
xy f ss = map (returnNodeRecord f) ss



-- returns a list containing all of the id values of each table, each table of ID is in its own list of values
returnIDValues :: File -> [[String]]
returnIDValues (File nodeSets _) = returnIDValues' nodeSets

returnIDValues' :: NodeSets -> [[String]]
returnIDValues' EmptyNodeSet = []
returnIDValues' (NodeSets nodeSet nodeSets) = returnIDValues'' nodeSet : returnIDValues' nodeSets

returnIDValues'' :: NodeSet -> [String]
returnIDValues'' (NodeSet _ nodeEntries) = returnIDValues''' nodeEntries

returnIDValues''' :: NodeEntries -> [String]
returnIDValues''' EmptyNodeEntry = []
returnIDValues''' (NodeEntries nodeEntry nodeEntries) = returnIDValues'''' nodeEntry : returnIDValues''' nodeEntries

returnIDValues'''' :: NodeEntry -> String
returnIDValues'''' (NodeEntry s _) = s
returnIDValues'''' (NodeEntryLabel s _ _) = s



-- -- Allows searching for a single record in a node set table by using an ID
returnNodeRecord :: File -> String -> Maybe NodeEntry
returnNodeRecord (File nodeSets _) s = returnNodeRecord' nodeSets s

returnNodeRecord' :: NodeSets -> String -> Maybe NodeEntry
returnNodeRecord' EmptyNodeSet _ = Nothing
returnNodeRecord' (NodeSets nodeSet nodeSets) s
    | returnNodeRecord'' nodeSet s /= Nothing = returnNodeRecord'' nodeSet s
    | otherwise = returnNodeRecord' nodeSets s

returnNodeRecord'' :: NodeSet -> String -> Maybe NodeEntry
returnNodeRecord'' (NodeSet _ nodeEntries) s
    | returnNodeRecord''' nodeEntries s /= Nothing = returnNodeRecord''' nodeEntries s
    | otherwise = returnNodeRecord''' nodeEntries s

returnNodeRecord''' :: NodeEntries -> String -> Maybe NodeEntry
returnNodeRecord''' EmptyNodeEntry _ = Nothing
returnNodeRecord''' (NodeEntries nodeEntry nodeEntries) s
    | returnNodeRecord'''' nodeEntry s /= Nothing = returnNodeRecord'''' nodeEntry s
    | otherwise = returnNodeRecord''' nodeEntries s

returnNodeRecord'''' :: NodeEntry -> String -> Maybe NodeEntry
returnNodeRecord'''' nodeEntry@(NodeEntryLabel s' _ _ ) s
    | s' == s = Just nodeEntry
    | otherwise = Nothing
returnNodeRecord'''' nodeEntry@(NodeEntry s' _) s
    | s' == s = Just nodeEntry
    | otherwise = Nothing

noParse :: ErrorCall -> IO ()
noParse e = do let err =  show e
               hPutStr stderr err
               return ()

noLex :: ErrorCall -> IO ()
noLex e = do let err =  show e
             hPutStr stderr ("Problem with lexing: " ++ err)
             return ()

