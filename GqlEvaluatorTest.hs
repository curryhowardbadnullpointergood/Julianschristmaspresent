module GqlEvaluatorTest where

import InputParser
import InputLexer
import LangParser
import LangLexer
import GqlEvaluator (evalMatch, VariableValue (TypeNodes))

parsedFile = File [NodeSet (NodeHeader [Field "points" TypeInteger,Field "team" TypeString] False) [NodeEntry "t1" [LiteralInt 9,LiteralStr "Winchester"] [],NodeEntry "t2" [LiteralNull,LiteralStr "Romsey"] [],NodeEntry "t3" [LiteralInt 7,LiteralStr "Eastleigh"] [],NodeEntry "t4" [LiteralInt 4,LiteralStr "FairOak"] [],NodeEntry "t5" [LiteralNull,LiteralStr "Totton"] [],NodeEntry "t6" [LiteralInt 6,LiteralStr "Weston"] [],NodeEntry "t7" [LiteralInt 7,LiteralStr "Hamble"] [],NodeEntry "t8" [LiteralNull,LiteralStr "Fareham"] [],NodeEntry "t9" [LiteralNull,LiteralStr "Ringwood"] [],NodeEntry "t10" [LiteralInt 3,LiteralStr "Hythe"] [],NodeEntry "t11" [LiteralInt 3,LiteralStr "Shirley"] [],NodeEntry "t12" [LiteralInt 3,LiteralStr "Southampton"] [],NodeEntry "t13" [LiteralInt 4,LiteralStr "Ashurst"] [],NodeEntry "t14" [LiteralInt 6,LiteralStr "Lyndhurst"] []]] [RelationshipSet (RelationshipHeader [Field "week" TypeInteger,Field "ga" TypeInteger,Field "gf" TypeInteger]) [RelationshipEntry "t1" [LiteralInt 3,LiteralInt 2,LiteralInt 3] "t10" "Beat",RelationshipEntry "t11" [LiteralInt 3,LiteralInt 0,LiteralInt 1] "t2" "Beat",RelationshipEntry "t3" [LiteralInt 3,LiteralInt 1,LiteralInt 1] "t12" "DrewWith",RelationshipEntry "t13" [LiteralInt 3,LiteralInt 1,LiteralInt 2] "t4" "Beat",RelationshipEntry "t14" [LiteralInt 3,LiteralInt 2,LiteralInt 3] "t5" "Beat",RelationshipEntry "t6" [LiteralInt 3,LiteralInt 0,LiteralInt 1] "t8" "Beat",RelationshipEntry "t7" [LiteralInt 3,LiteralInt 0,LiteralInt 1] "t9" "Beat",RelationshipEntry "t1" [LiteralInt 2,LiteralInt 0,LiteralInt 1] "t9" "Beat",RelationshipEntry "t10" [LiteralInt 2,LiteralInt 0,LiteralInt 1] "t2" "Beat",RelationshipEntry "t3" [LiteralInt 2,LiteralInt 0,LiteralInt 4] "t11" "Beat",RelationshipEntry "t4" [LiteralInt 2,LiteralInt 3,LiteralInt 3] "t12" "DrewWith",RelationshipEntry "t5" [LiteralInt 2,LiteralInt 0,LiteralInt 0] "t13" "DrewWith",RelationshipEntry "t14" [LiteralInt 2,LiteralInt 0,LiteralInt 1] "t6" "Beat",RelationshipEntry "t7" [LiteralInt 2,LiteralInt 1,LiteralInt 1] "t8" "DrewWith",RelationshipEntry "t1" [LiteralInt 1,LiteralInt 2,LiteralInt 4] "t8" "Beat",RelationshipEntry "t2" [LiteralInt 1,LiteralInt 1,LiteralInt 1] "t9" "DrewWith",RelationshipEntry "t3" [LiteralInt 1,LiteralInt 0,LiteralInt 1] "t10" "Beat",RelationshipEntry "t4" [LiteralInt 1,LiteralInt 1,LiteralInt 3] "t11" "Beat",RelationshipEntry "t5" [LiteralInt 1,LiteralInt 2,LiteralInt 2] "t12" "DrewWith",RelationshipEntry "t6" [LiteralInt 1,LiteralInt 0,LiteralInt 1] "t13" "Beat",RelationshipEntry "t7" [LiteralInt 1,LiteralInt 0,LiteralInt 2] "t14" "Beat"]]

parsedInputMatch = Match (PatternFinal "n") (Return [StrOutput "n" "age" "age"])


main :: IO()
main = do
    putStrLn "\n------------------------------------------------\n------------------------------------------------\n"
    putStrLn "Starting Tests:"
    evalMatchTest
    putStrLn "\n------------------------------------------------\n------------------------------------------------\n"

evalMatchTest :: IO ()
evalMatchTest = do
    putStrLn "\n------------------------------------------------\n------------------------------------------------\n"
    putStrLn "EVAL MATCH Tests:"
    assertEqual "Test 1: " (evalMatch [] parsedInputMatch parsedFile ) ([("n", TypeNodes ([(NodeHeader [Field "points" TypeInteger,Field "team" TypeString] False,NodeEntry "t1" [LiteralInt 9,LiteralStr "Winchester"] []),(NodeHeader [Field "points" TypeInteger,Field "team" TypeString] False,NodeEntry "t2" [LiteralNull,LiteralStr "Romsey"] []),(NodeHeader [Field "points" TypeInteger,Field "team" TypeString] False,NodeEntry "t3" [LiteralInt 7,LiteralStr "Eastleigh"] []),(NodeHeader [Field "points" TypeInteger,Field "team" TypeString] False,NodeEntry "t4" [LiteralInt 4,LiteralStr "FairOak"] []),(NodeHeader [Field "points" TypeInteger,Field "team" TypeString] False,NodeEntry "t5" [LiteralNull,LiteralStr "Totton"] []),(NodeHeader [Field "points" TypeInteger,Field "team" TypeString] False,NodeEntry "t6" [LiteralInt 6,LiteralStr "Weston"] []),(NodeHeader [Field "points" TypeInteger,Field "team" TypeString] False,NodeEntry "t7" [LiteralInt 7,LiteralStr "Hamble"] []),(NodeHeader [Field "points" TypeInteger,Field "team" TypeString] False,NodeEntry "t8" [LiteralNull,LiteralStr "Fareham"] []),(NodeHeader [Field "points" TypeInteger,Field "team" TypeString] False,NodeEntry "t9" [LiteralNull,LiteralStr "Ringwood"] []),(NodeHeader [Field "points" TypeInteger,Field "team" TypeString] False,NodeEntry "t10" [LiteralInt 3,LiteralStr "Hythe"] []),(NodeHeader [Field "points" TypeInteger,Field "team" TypeString] False,NodeEntry "t11" [LiteralInt 3,LiteralStr "Shirley"] []),(NodeHeader [Field "points" TypeInteger,Field "team" TypeString] False,NodeEntry "t12" [LiteralInt 3,LiteralStr "Southampton"] []),(NodeHeader [Field "points" TypeInteger,Field "team" TypeString] False,NodeEntry "t13" [LiteralInt 4,LiteralStr "Ashurst"] []),(NodeHeader [Field "points" TypeInteger,Field "team" TypeString] False,NodeEntry "t14" [LiteralInt 6,LiteralStr "Lyndhurst"] [])]))])


assertEqual :: (Eq a, Show a) => String -> a -> a -> IO ()
assertEqual testName actual expected =
    if actual == expected
        then putStrLn $ testName ++ " passed."
        else putStrLn $ testName ++ " failed. Expected: " ++ show expected ++ ", but got: " ++ show actual
