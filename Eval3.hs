module Eval3 where
import InputLexer ( FieldType(..) )
import InputParser
import Data.Maybe
import Control.Monad
import Data.List
import System.Environment ( getArgs )
import Control.Exception
import System.IO
import Text.Read (Lexeme(String))
import Data.Map.Internal.Debug (node)
import Data.List (elemIndex)






-- Filters the type of the relation table 
filterTypeRelations ::[(RelationshipHeader, RelationshipEntry)] -> (String -> Bool) -> [(RelationshipHeader,RelationshipEntry)]
filterTypeRelations relationshipEnteries predicate = filter matchesConditiont relationshipEnteries
    where 
        matchesConditiont :: (RelationshipHeader, RelationshipEntry) -> Bool 
        matchesConditiont l = matchesCondition l predicate


getRE :: (a, RelationshipEntry) -> String
getRE (rH, rE) = getLastString rE 

getLastString :: RelationshipEntry -> String
getLastString (RelationshipEntry _ _ _ lastStr) = lastStr


-- matchesCondition :: (RelationshipHeader, RelationshipEntries) -> (String -> Bool) -> Bool 
matchesCondition :: (a, RelationshipEntry) -> (String -> Bool) -> Bool
matchesCondition rE predicate = predicate (getRE rE)



-- getStartEndId :: [(relationshipHeader, relationshipEntery)] -> String 
getStartId :: [(RelationshipHeader,RelationshipEntry)] -> [String]
getStartId list = [ getStartEndId' rE | (_,rE) <- list]           



-- getStartEndId :: [(relationshipHeader, relationshipEntery)] -> String 
getEndId :: [(RelationshipHeader,RelationshipEntry)] -> [String]
getEndId list = [ getStartEndId'' rE | (_,rE) <- list]          

getStartEndId' (RelationshipEntry s _ e _) = s

getStartEndId'' (RelationshipEntry s _ e _) = e


getPairStartEnd :: [(RelationshipHeader,RelationshipEntry)] -> [(String,String)]
getPairStartEnd list = [ getPairStartEnd' rE | (_,rE) <- list]          

getPairStartEnd' :: RelationshipEntry -> (String, String)
getPairStartEnd' (RelationshipEntry s _ e _) = (s,e)


helperfindn''2 :: String -> [(String,String)] -> [(String,String)] -> [(String,String,String,String,String,String)]
helperfindn''2 str pairlist pairb = [ ("n", b2,"n'", i1, "n''" ,i2) | (i1,i2) <- pairlist, i1 == str, (b1,b2) <- pairb , i1 == b1]

helperfindn'' :: String -> [(String,String)] -> [(String,String)] -> [(String,String,String,String,String,String)] 
helperfindn'' str pairlist pairb = [ ("n", b2, "n'",i2,"n''",i1) | (i1, i2) <- pairlist , i2 == str, (b1,b2) <- pairb , i2 == b1 ] 

findn'' :: [String] -> [(String,String)] -> [(String,String)] -> [[(String,String,String,String,String,String)]]
findn'' strlist pairlist pairb = (filter (not.null)[ helperfindn'' str pairlist pairb | str <- strlist]) 
                        ++ (filter (not.null)[ helperfindn''2 str pairlist pairb | str <- strlist])







getNodeString:: NodeEntry -> String 
getNodeString (NodeEntry s l _ ) = s


getNE :: (a, NodeEntry) -> String
getNE (nH, nE) = getNodeString nE 


matchesConditionN :: (a, NodeEntry) -> (String -> Bool) -> Bool
matchesConditionN rE predicate = predicate (getNE rE)


filterTypeRelationsN ::[(NodeHeader, NodeEntry)] -> (String -> Bool) -> [(NodeHeader,NodeEntry)]
filterTypeRelationsN nodeEnteries predicate = filter matchesConditiont nodeEnteries
    where 
        matchesConditiont :: (NodeHeader, NodeEntry) -> Bool
        matchesConditiont l = matchesConditionN l predicate

nihow :: (String,String,String,String,String,String) -> [(NodeHeader, NodeEntry)] -> [(NodeHeader,NodeEntry)]
nihow (s1,s2,s3,s4,s5,s6) nodeE  = filterTypeRelationsN nodeE (\s -> s == s2) ++ filterTypeRelationsN nodeE (\s -> s == s6)
    
passitin :: [[(String, String, String, String, String, String)]] -> [(NodeHeader, NodeEntry)] -> [[(NodeHeader, NodeEntry)]]
passitin stri nodeE = [(nihow t nodeE) | gh <- stri, t <- gh]

passincompare :: [[(NodeHeader,NodeEntry)]] -> String -> [(NodeHeader,NodeEntry)]
passincompare list str = nub (concat (filter (not.null)[ (comparepoint1 f str)| f <- list]))

comparepoint1 :: [(NodeHeader,NodeEntry)] -> String -> [(NodeHeader,NodeEntry)] 
comparepoint1 [(nH,nE),(nH1, nH2)] str 
                | (getLiteralsInt (nH, nE) str) == LiteralNull = []
                | (getLiteralsInt (nH, nE) str) == ((getLiteralsInt (nH1, nH2) str)) = [(nH, nE)]
                | otherwise = [] 

getLiteralsInt :: (NodeHeader, NodeEntry) -> String -> Literal 
getLiteralsInt ((NodeHeader list _ ), (NodeEntry _ list1 _)) str = list1 !! (findIndexc (Field str TypeInteger) list )


findIndexc :: Eq a => a -> [a] -> Int
findIndexc _ [] = -1
findIndexc item (x:xs)
    | item == x = 0
    | otherwise = 1 + findIndexc item xs


reverseList :: [a] -> [a]
reverseList [] = []
reverseList (x:xs) = reverseList xs ++ [x]

listpp list nump = [ (passitinnagain g nump) | g <- list]
passitinnagain hg nump = [ arepointsequal g nump  | h <- hg , g <- h]
arepointsequal (nH, nE) nump = [getpointsss nE nump]

getpointsss (NodeEntry s list _) nump = specifyindeee s list nump 

specifyindeee s list nuu = (s,list !! nuu) 

comparepoints (x,xs) _ | xs == LiteralNull = []
comparepoints (x,xs) listtt = [ (r,t) | (r,t) <- listtt, t == xs  ]



concattt :: Foldable t => t [a] -> [a]
concattt input = concat input 

turntotuple :: [(a, b)] -> (a, b)
turntotuple [(a,b)] = (a,b)

parsedFile :: File
parsedFile = File [NodeSet (NodeHeader [Field "points" TypeInteger,Field "team" TypeString] False) [NodeEntry "t1" [LiteralInt 9,LiteralStr "Winchester"] [],NodeEntry "t2" [LiteralNull,LiteralStr "Romsey"] [],NodeEntry "t3" [LiteralInt 7,LiteralStr "Eastleigh"] [],NodeEntry "t4" [LiteralInt 4,LiteralStr "FairOak"] [],NodeEntry "t5" [LiteralNull,LiteralStr "Totton"] [],NodeEntry "t6" [LiteralInt 6,LiteralStr "Weston"] [],NodeEntry "t7" [LiteralInt 7,LiteralStr "Hamble"] [],NodeEntry "t8" [LiteralNull,LiteralStr "Fareham"] [],NodeEntry "t9" [LiteralNull,LiteralStr "Ringwood"] [],NodeEntry "t10" [LiteralInt 3,LiteralStr "Hythe"] [],NodeEntry "t11" [LiteralInt 3,LiteralStr "Shirley"] [],NodeEntry "t12" [LiteralInt 3,LiteralStr "Southampton"] [],NodeEntry "t13" [LiteralInt 4,LiteralStr "Ashurst"] [],NodeEntry "t14" [LiteralInt 6,LiteralStr "Lyndhurst"] []]] [RelationshipSet (RelationshipHeader [Field "week" TypeInteger,Field "ga" TypeInteger,Field "gf" TypeInteger]) [RelationshipEntry "t1" [LiteralInt 3,LiteralInt 2,LiteralInt 3] "t10" "Beat",RelationshipEntry "t11" [LiteralInt 3,LiteralInt 0,LiteralInt 1] "t2" "Beat",RelationshipEntry "t3" [LiteralInt 3,LiteralInt 1,LiteralInt 1] "t12" "DrewWith",RelationshipEntry "t13" [LiteralInt 3,LiteralInt 1,LiteralInt 2] "t4" "Beat",RelationshipEntry "t14" [LiteralInt 3,LiteralInt 2,LiteralInt 3] "t5" "Beat",RelationshipEntry "t6" [LiteralInt 3,LiteralInt 0,LiteralInt 1] "t8" "Beat",RelationshipEntry "t7" [LiteralInt 3,LiteralInt 0,LiteralInt 1] "t9" "Beat",RelationshipEntry "t1" [LiteralInt 2,LiteralInt 0,LiteralInt 1] "t9" "Beat",RelationshipEntry "t10" [LiteralInt 2,LiteralInt 0,LiteralInt 1] "t2" "Beat",RelationshipEntry "t3" [LiteralInt 2,LiteralInt 0,LiteralInt 4] "t11" "Beat",RelationshipEntry "t4" [LiteralInt 2,LiteralInt 3,LiteralInt 3] "t12" "DrewWith",RelationshipEntry "t5" [LiteralInt 2,LiteralInt 0,LiteralInt 0] "t13" "DrewWith",RelationshipEntry "t14" [LiteralInt 2,LiteralInt 0,LiteralInt 1] "t6" "Beat",RelationshipEntry "t7" [LiteralInt 2,LiteralInt 1,LiteralInt 1] "t8" "DrewWith",RelationshipEntry "t1" [LiteralInt 1,LiteralInt 2,LiteralInt 4] "t8" "Beat",RelationshipEntry "t2" [LiteralInt 1,LiteralInt 1,LiteralInt 1] "t9" "DrewWith",RelationshipEntry "t3" [LiteralInt 1,LiteralInt 0,LiteralInt 1] "t10" "Beat",RelationshipEntry "t4" [LiteralInt 1,LiteralInt 1,LiteralInt 3] "t11" "Beat",RelationshipEntry "t5" [LiteralInt 1,LiteralInt 2,LiteralInt 2] "t12" "DrewWith",RelationshipEntry "t6" [LiteralInt 1,LiteralInt 0,LiteralInt 1] "t13" "Beat",RelationshipEntry "t7" [LiteralInt 1,LiteralInt 0,LiteralInt 2] "t14" "Beat"]]

main :: IO ()
main = do
    let relationships = getRelationships parsedFile
    putStrLn "Relationships:"
    mapM_ print relationships








-- Relationships:
-- (RelationshipHeader [Field "week" TypeInteger,Field "ga" TypeInteger,Field "gf" TypeInteger],RelationshipEntry "t1" [LiteralInt 3,LiteralInt 2,LiteralInt 3] "t10" "Beat")
-- (RelationshipHeader [Field "week" TypeInteger,Field "ga" TypeInteger,Field "gf" TypeInteger],RelationshipEntry "t11" [LiteralInt 3,LiteralInt 0,LiteralInt 1] "t2" "Beat")
-- (RelationshipHeader [Field "week" TypeInteger,Field "ga" TypeInteger,Field "gf" TypeInteger],RelationshipEntry "t3" [LiteralInt 3,LiteralInt 1,LiteralInt 1] "t12" "DrewWith")
-- (RelationshipHeader [Field "week" TypeInteger,Field "ga" TypeInteger,Field "gf" TypeInteger],RelationshipEntry "t13" [LiteralInt 3,LiteralInt 1,LiteralInt 2] "t4" "Beat")
-- (RelationshipHeader [Field "week" TypeInteger,Field "ga" TypeInteger,Field "gf" TypeInteger],RelationshipEntry "t14" [LiteralInt 3,LiteralInt 2,LiteralInt 3] "t5" "Beat")
-- (RelationshipHeader [Field "week" TypeInteger,Field "ga" TypeInteger,Field "gf" TypeInteger],RelationshipEntry "t6" [LiteralInt 3,LiteralInt 0,LiteralInt 1] "t8" "Beat")
-- (RelationshipHeader [Field "week" TypeInteger,Field "ga" TypeInteger,Field "gf" TypeInteger],RelationshipEntry "t7" [LiteralInt 3,LiteralInt 0,LiteralInt 1] "t9" "Beat")
-- (RelationshipHeader [Field "week" TypeInteger,Field "ga" TypeInteger,Field "gf" TypeInteger],RelationshipEntry "t1" [LiteralInt 2,LiteralInt 0,LiteralInt 1] "t9" "Beat")
-- (RelationshipHeader [Field "week" TypeInteger,Field "ga" TypeInteger,Field "gf" TypeInteger],RelationshipEntry "t10" [LiteralInt 2,LiteralInt 0,LiteralInt 1] "t2" "Beat")
-- (RelationshipHeader [Field "week" TypeInteger,Field "ga" TypeInteger,Field "gf" TypeInteger],RelationshipEntry "t3" [LiteralInt 2,LiteralInt 0,LiteralInt 4] "t11" "Beat")
-- (RelationshipHeader [Field "week" TypeInteger,Field "ga" TypeInteger,Field "gf" TypeInteger],RelationshipEntry "t4" [LiteralInt 2,LiteralInt 3,LiteralInt 3] "t12" "DrewWith")
-- (RelationshipHeader [Field "week" TypeInteger,Field "ga" TypeInteger,Field "gf" TypeInteger],RelationshipEntry "t5" [LiteralInt 2,LiteralInt 0,LiteralInt 0] "t13" "DrewWith")
-- (RelationshipHeader [Field "week" TypeInteger,Field "ga" TypeInteger,Field "gf" TypeInteger],RelationshipEntry "t14" [LiteralInt 2,LiteralInt 0,LiteralInt 1] "t6" "Beat")
-- (RelationshipHeader [Field "week" TypeInteger,Field "ga" TypeInteger,Field "gf" TypeInteger],RelationshipEntry "t7" [LiteralInt 2,LiteralInt 1,LiteralInt 1] "t8" "DrewWith")
-- (RelationshipHeader [Field "week" TypeInteger,Field "ga" TypeInteger,Field "gf" TypeInteger],RelationshipEntry "t1" [LiteralInt 1,LiteralInt 2,LiteralInt 4] "t8" "Beat")
-- (RelationshipHeader [Field "week" TypeInteger,Field "ga" TypeInteger,Field "gf" TypeInteger],RelationshipEntry "t2" [LiteralInt 1,LiteralInt 1,LiteralInt 1] "t9" "DrewWith")
-- (RelationshipHeader [Field "week" TypeInteger,Field "ga" TypeInteger,Field "gf" TypeInteger],RelationshipEntry "t3" [LiteralInt 1,LiteralInt 0,LiteralInt 1] "t10" "Beat")
-- (RelationshipHeader [Field "week" TypeInteger,Field "ga" TypeInteger,Field "gf" TypeInteger],RelationshipEntry "t4" [LiteralInt 1,LiteralInt 1,LiteralInt 3] "t11" "Beat")
-- (RelationshipHeader [Field "week" TypeInteger,Field "ga" TypeInteger,Field "gf" TypeInteger],RelationshipEntry "t5" [LiteralInt 1,LiteralInt 2,LiteralInt 2] "t12" "DrewWith")
-- (RelationshipHeader [Field "week" TypeInteger,Field "ga" TypeInteger,Field "gf" TypeInteger],RelationshipEntry "t6" [LiteralInt 1,LiteralInt 0,LiteralInt 1] "t13" "Beat")
-- (RelationshipHeader [Field "week" TypeInteger,Field "ga" TypeInteger,Field "gf" TypeInteger],RelationshipEntry "t7" [LiteralInt 1,LiteralInt 0,LiteralInt 2] "t14" "Beat")

--[(RelationshipHeader [Field "week" TypeInteger,Field "ga" TypeInteger,Field "gf" TypeInteger],RelationshipEntry "t1" [LiteralInt 3,LiteralInt 2,LiteralInt 3] "t10" "Beat"),(RelationshipHeader [Field "week" TypeInteger,Field "ga" TypeInteger,Field "gf" TypeInteger],RelationshipEntry "t11" [LiteralInt 3,LiteralInt 0,LiteralInt 1] "t2" "Beat")]



--[(RelationshipHeader [Field "week" TypeInteger,Field "ga" TypeInteger,Field "gf" TypeInteger],RelationshipEntry "t1" [LiteralInt 3,LiteralInt 2,LiteralInt 3] "t10" "Beat"),(RelationshipHeader [Field "week" TypeInteger,Field "ga" TypeInteger,Field "gf" TypeInteger],RelationshipEntry "t11" [LiteralInt 3,LiteralInt 0,LiteralInt 1] "t2" "Beat"),(RelationshipHeader [Field "week" TypeInteger,Field "ga" TypeInteger,Field "gf" TypeInteger],RelationshipEntry "t3" [LiteralInt 3,LiteralInt 1,LiteralInt 1] "t12" "DrewWith"),(RelationshipHeader [Field "week" TypeInteger,Field "ga" TypeInteger,Field "gf" TypeInteger],RelationshipEntry "t13" [LiteralInt 3,LiteralInt 1,LiteralInt 2] "t4" "Beat"),(RelationshipHeader [Field "week" TypeInteger,Field "ga" TypeInteger,Field "gf" TypeInteger],RelationshipEntry "t14" [LiteralInt 3,LiteralInt 2,LiteralInt 3] "t5" "Beat"),(RelationshipHeader [Field "week" TypeInteger,Field "ga" TypeInteger,Field "gf" TypeInteger],RelationshipEntry "t6" [LiteralInt 3,LiteralInt 0,LiteralInt 1] "t8" "Beat"),(RelationshipHeader [Field "week" TypeInteger,Field "ga" TypeInteger,Field "gf" TypeInteger],RelationshipEntry "t7" [LiteralInt 3,LiteralInt 0,LiteralInt 1] "t9" "Beat"),(RelationshipHeader [Field "week" TypeInteger,Field "ga" TypeInteger,Field "gf" TypeInteger],RelationshipEntry "t1" [LiteralInt 2,LiteralInt 0,LiteralInt 1] "t9" "Beat"),(RelationshipHeader [Field "week" TypeInteger,Field "ga" TypeInteger,Field "gf" TypeInteger],RelationshipEntry "t10" [LiteralInt 2,LiteralInt 0,LiteralInt 1] "t2" "Beat"),(RelationshipHeader [Field "week" TypeInteger,Field "ga" TypeInteger,Field "gf" TypeInteger],RelationshipEntry "t3" [LiteralInt 2,LiteralInt 0,LiteralInt 4] "t11" "Beat"),(RelationshipHeader [Field "week" TypeInteger,Field "ga" TypeInteger,Field "gf" TypeInteger],RelationshipEntry "t4" [LiteralInt 2,LiteralInt 3,LiteralInt 3] "t12" "DrewWith"),(RelationshipHeader [Field "week" TypeInteger,Field "ga" TypeInteger,Field "gf" TypeInteger],RelationshipEntry "t5" [LiteralInt 2,LiteralInt 0,LiteralInt 0] "t13" "DrewWith"),(RelationshipHeader [Field "week" TypeInteger,Field "ga" TypeInteger,Field "gf" TypeInteger],RelationshipEntry "t14" [LiteralInt 2,LiteralInt 0,LiteralInt 1] "t6" "Beat"),(RelationshipHeader [Field "week" TypeInteger,Field "ga" TypeInteger,Field "gf" TypeInteger],RelationshipEntry "t7" [LiteralInt 2,LiteralInt 1,LiteralInt 1] "t8" "DrewWith"),(RelationshipHeader [Field "week" TypeInteger,Field "ga" TypeInteger,Field "gf" TypeInteger],RelationshipEntry "t1" [LiteralInt 1,LiteralInt 2,LiteralInt 4] "t8" "Beat"),(RelationshipHeader [Field "week" TypeInteger,Field "ga" TypeInteger,Field "gf" TypeInteger],RelationshipEntry "t2" [LiteralInt 1,LiteralInt 1,LiteralInt 1] "t9" "DrewWith"),(RelationshipHeader [Field "week" TypeInteger,Field "ga" TypeInteger,Field "gf" TypeInteger],RelationshipEntry "t3" [LiteralInt 1,LiteralInt 0,LiteralInt 1] "t10" "Beat"),(RelationshipHeader [Field "week" TypeInteger,Field "ga" TypeInteger,Field "gf" TypeInteger],RelationshipEntry "t4" [LiteralInt 1,LiteralInt 1,LiteralInt 3] "t11" "Beat"),(RelationshipHeader [Field "week" TypeInteger,Field "ga" TypeInteger,Field "gf" TypeInteger],RelationshipEntry "t5" [LiteralInt 1,LiteralInt 2,LiteralInt 2] "t12" "DrewWith"),(RelationshipHeader [Field "week" TypeInteger,Field "ga" TypeInteger,Field "gf" TypeInteger],RelationshipEntry "t6" [LiteralInt 1,LiteralInt 0,LiteralInt 1] "t13" "Beat"),(RelationshipHeader [Field "week" TypeInteger,Field "ga" TypeInteger,Field "gf" TypeInteger],RelationshipEntry "t7" [LiteralInt 1,LiteralInt 0,LiteralInt 2] "t14" "Beat")]


-- [(NodeHeader [Field "points" TypeInteger,Field "team" TypeString] False,NodeEntry "t1" [LiteralInt 9,LiteralStr "Winchester"] []),(NodeHeader [Field "points" TypeInteger,Field "team" TypeString] False,NodeEntry "t2" [LiteralNull,LiteralStr "Romsey"] []),(NodeHeader [Field "points" TypeInteger,Field "team" TypeString] False,NodeEntry "t3" [LiteralInt 7,LiteralStr "Eastleigh"] []),(NodeHeader [Field "points" TypeInteger,Field "team" TypeString] False,NodeEntry "t4" [LiteralInt 4,LiteralStr "FairOak"] []),(NodeHeader [Field "points" TypeInteger,Field "team" TypeString] False,NodeEntry "t5" [LiteralNull,LiteralStr "Totton"] []),(NodeHeader [Field "points" TypeInteger,Field "team" TypeString] False,NodeEntry "t6" [LiteralInt 6,LiteralStr "Weston"] []),(NodeHeader [Field "points" TypeInteger,Field "team" TypeString] False,NodeEntry "t7" [LiteralInt 7,LiteralStr "Hamble"] []),(NodeHeader [Field "points" TypeInteger,Field "team" TypeString] False,NodeEntry "t8" [LiteralNull,LiteralStr "Fareham"] []),(NodeHeader [Field "points" TypeInteger,Field "team" TypeString] False,NodeEntry "t9" [LiteralNull,LiteralStr "Ringwood"] []),(NodeHeader [Field "points" TypeInteger,Field "team" TypeString] False,NodeEntry "t10" [LiteralInt 3,LiteralStr "Hythe"] []),(NodeHeader [Field "points" TypeInteger,Field "team" TypeString] False,NodeEntry "t11" [LiteralInt 3,LiteralStr "Shirley"] []),(NodeHeader [Field "points" TypeInteger,Field "team" TypeString] False,NodeEntry "t12" [LiteralInt 3,LiteralStr "Southampton"] []),(NodeHeader [Field "points" TypeInteger,Field "team" TypeString] False,NodeEntry "t13" [LiteralInt 4,LiteralStr "Ashurst"] []),(NodeHeader [Field "points" TypeInteger,Field "team" TypeString] False,NodeEntry "t14" [LiteralInt 6,LiteralStr "Lyndhurst"] [])]
-- ghci> let node = [(NodeHeader [Field "points" TypeInteger,Field "team" TypeString] False,NodeEntry "t1" [LiteralInt 9,LiteralStr "Winchester"] []),(NodeHeader [Field "points" TypeInteger,Field "team" TypeString] False,NodeEntry "t2" [LiteralNull,LiteralStr "Romsey"] []),(NodeHeader [Field "points" TypeInteger,Field "team" TypeString] False,NodeEntry "t3" [LiteralInt 7,LiteralStr "Eastleigh"] []),(NodeHeader [Field "points" TypeInteger,Field "team" TypeString] False,NodeEntry "t4" [LiteralInt 4,LiteralStr "FairOak"] []),(NodeHeader [Field "points" TypeInteger,Field "team" TypeString] False,NodeEntry "t5" [LiteralNull,LiteralStr "Totton"] []),(NodeHeader [Field "points" TypeInteger,Field "team" TypeString] False,NodeEntry "t6" [LiteralInt 6,LiteralStr "Weston"] []),(NodeHeader [Field "points" TypeInteger,Field "team" TypeString] False,NodeEntry "t7" [LiteralInt 7,LiteralStr "Hamble"] []),(NodeHeader [Field "points" TypeInteger,Field "team" TypeString] False,NodeEntry "t8" [LiteralNull,LiteralStr "Fareham"] []),(NodeHeader [Field "points" TypeInteger,Field "team" TypeString] False,NodeEntry "t9" [LiteralNull,LiteralStr "Ringwood"] []),(NodeHeader [Field "points" TypeInteger,Field "team" TypeString] False,NodeEntry "t10" [LiteralInt 3,LiteralStr "Hythe"] []),(NodeHeader [Field "points" TypeInteger,Field "team" TypeString] False,NodeEntry "t11" [LiteralInt 3,LiteralStr "Shirley"] []),(NodeHeader [Field "points" TypeInteger,Field "team" TypeString] False,NodeEntry "t12" [LiteralInt 3,LiteralStr "Southampton"] []),(NodeHeader [Field "points" TypeInteger,Field "team" TypeString] False,NodeEntry "t13" [LiteralInt 4,LiteralStr "Ashurst"] []),(NodeHeader [Field "points" TypeInteger,Field "team" TypeString] False,NodeEntry "t14" [LiteralInt 6,LiteralStr "Lyndhurst"] [])]
-- ghci> node
--[(NodeHeader [Field "points" TypeInteger,Field "team" TypeString] False,NodeEntry "t1" [LiteralInt 9,LiteralStr "Winchester"] []),(NodeHeader [Field "points" TypeInteger,Field "team" TypeString] False,NodeEntry "t2" [LiteralNull,LiteralStr "Romsey"] []),(NodeHeader [Field "points" TypeInteger,Field "team" TypeString] False,NodeEntry "t3" [LiteralInt 7,LiteralStr "Eastleigh"] []),(NodeHeader [Field "points" TypeInteger,Field "team" TypeString] False,NodeEntry "t4" [LiteralInt 4,LiteralStr "FairOak"] []),(NodeHeader [Field "points" TypeInteger,Field "team" TypeString] False,NodeEntry "t5" [LiteralNull,LiteralStr "Totton"] []),(NodeHeader [Field "points" TypeInteger,Field "team" TypeString] False,NodeEntry "t6" [LiteralInt 6,LiteralStr "Weston"] []),(NodeHeader [Field "points" TypeInteger,Field "team" TypeString] False,NodeEntry "t7" [LiteralInt 7,LiteralStr "Hamble"] []),(NodeHeader [Field "points" TypeInteger,Field "team" TypeString] False,NodeEntry "t8" [LiteralNull,LiteralStr "Fareham"] []),(NodeHeader [Field "points" TypeInteger,Field "team" TypeString] False,NodeEntry "t9" [LiteralNull,LiteralStr "Ringwood"] []),(NodeHeader [Field "points" TypeInteger,Field "team" TypeString] False,NodeEntry "t10" [LiteralInt 3,LiteralStr "Hythe"] []),(NodeHeader [Field "points" TypeInteger,Field "team" TypeString] False,NodeEntry "t11" [LiteralInt 3,LiteralStr "Shirley"] []),(NodeHeader [Field "points" TypeInteger,Field "team" TypeString] False,NodeEntry "t12" [LiteralInt 3,LiteralStr "Southampton"] []),(NodeHeader [Field "points" TypeInteger,Field "team" TypeString] False,NodeEntry "t13" [LiteralInt 4,LiteralStr "Ashurst"] []),(NodeHeader [Field "points" TypeInteger,Field "team" TypeString] False,NodeEntry "t14" [LiteralInt 6,LiteralStr "Lyndhurst"] [])]



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

removeDuplicates [] = []
removeDuplicates (a:as) = a : removeDuplicates (filter (\x -> x /= a) as) 

unionLists a b = removeDuplicates $ a ++ b

interSectionLists [] _ = []
interSectionLists _ [] = []
interSectionLists a b = removeDuplicates $ filter (\x -> elem x a) b



-- Ferdi's Stuff



areEntriesEqual :: (NodeHeader, NodeEntry) -> (NodeHeader, NodeEntry) -> Bool
areEntriesEqual (_, NodeEntry id1 _ _) (_, NodeEntry id2 _ _) = id1 == id2

unionNode :: [(NodeHeader, NodeEntry)] -> [(NodeHeader, NodeEntry)] -> [(NodeHeader, NodeEntry)]
unionNode part1 part2 = nubBy areEntriesEqual (part1 ++ part2)


-- EXTRACTORS


extractNodeIDs :: [(NodeHeader, NodeEntry)] -> [String]
extractNodeIDs nodes = map (\(_, NodeEntry nodeId _ _) -> nodeId) nodes

extractNodeLabels :: [(NodeHeader, NodeEntry)] -> [String]
extractNodeLabels nodes = nub $ concatMap (\(_, NodeEntry _ _ labels) -> map (\(Label l) -> l) labels) nodes

extractStartIDs :: [(RelationshipHeader, RelationshipEntry)] -> [String]
extractStartIDs relationships = nub $ map (\(_, RelationshipEntry startId _ _ _) -> startId) relationships

extractEndIDs :: [(RelationshipHeader, RelationshipEntry)] -> [String]
extractEndIDs relationships = nub $ map (\(_, RelationshipEntry _ _ endId _) -> endId) relationships

extractRelationshipTypes :: [(RelationshipHeader, RelationshipEntry)] -> [String]
extractRelationshipTypes relationships = nub $ map (\(_, RelationshipEntry _ _ _ typeLabel) -> typeLabel) relationships

extractFieldFromNodes :: String -> [(NodeHeader, NodeEntry)] -> [Maybe Literal]
extractFieldFromNodes fieldName nodes = map (extractFieldFromNodeEntry fieldName) nodes

extractFieldFromNodeEntry :: String -> (NodeHeader, NodeEntry) -> Maybe Literal
extractFieldFromNodeEntry fieldName (NodeHeader fields _, NodeEntry _ literals _) =
    findFieldAndExtractLiteral fieldName fields literals

extractFieldFromRelationships :: String -> [(RelationshipHeader, RelationshipEntry)] -> [Maybe Literal]
extractFieldFromRelationships fieldName relationships = map (extractFieldFromRelationshipEntry fieldName) relationships

extractFieldFromRelationshipEntry :: String -> (RelationshipHeader, RelationshipEntry) -> Maybe Literal
extractFieldFromRelationshipEntry fieldName (RelationshipHeader fields, RelationshipEntry _ literals _ _) =
    findFieldAndExtractLiteral fieldName fields literals

findFieldAndExtractLiteral :: String -> [Field] -> [Literal] -> Maybe Literal
findFieldAndExtractLiteral fieldName fields literals = do
    fieldIndex <- findIndex (\(Field name _) -> name == fieldName) fields
    if fieldIndex < length literals then Just (literals !! fieldIndex) else Nothing


-- GENERATES [RELATIONSHIPENTRY]


generateRelationshipEntries :: [String] -> [String] -> [String] -> [RelationshipEntry]
generateRelationshipEntries startIds endIds types =
    [RelationshipEntry start [] end typ | (start, end, typ) <- zip3 startIds endIds types]

generatePossiblyAllocatedEntries :: [String] -> [String] -> String -> [RelationshipEntry]
generatePossiblyAllocatedEntries startIds endIds relType =
    [RelationshipEntry start [] end relType | start <- startIds, end <- endIds]

generateNodeEntries :: [String] -> Literals -> Labels -> [NodeEntry]
generateNodeEntries nodeIds literals labels =
    [NodeEntry nodeId literals labels | nodeId <- nodeIds]

generateCustomNodeEntries :: [String] -> [Literals] -> [Labels] -> [NodeEntry]
generateCustomNodeEntries nodeIds literalsList labelsList =
    [NodeEntry nodeId literals labels | (nodeId, literals, labels) <- zip3 nodeIds literalsList labelsList]


-- CREATING NEW NODES & CREATING NEW RELATIONSHIPS


addNewNodes :: ([(NodeHeader, NodeEntry)], [(RelationshipHeader, RelationshipEntry)]) 
            -> [(String, FieldType)] 
            -> [NodeEntry]         
            -> ([(NodeHeader, NodeEntry)], [(RelationshipHeader, RelationshipEntry)])
addNewNodes (nodes, relationships) headerSpecs newEntries =
    let newHeader = createCustomNodeHeader headerSpecs True 
        newNodes = map ((,) newHeader) newEntries
    in (nodes ++ newNodes, relationships)
createCustomNodeHeader :: [(String, FieldType)] -> Bool -> NodeHeader
createCustomNodeHeader fieldsSpecs hasLabel = NodeHeader (map (\(name, typ) -> Field name typ) fieldsSpecs) hasLabel

addNewRelationships :: ([(NodeHeader, NodeEntry)], [(RelationshipHeader, RelationshipEntry)]) 
                    -> [(String, FieldType)]
                    -> [RelationshipEntry]  
                    -> ([(NodeHeader, NodeEntry)], [(RelationshipHeader, RelationshipEntry)])
addNewRelationships (nodes, relationships) headerSpecs newEntries =
    let newHeader = createCustomHeader headerSpecs
        newRelationships = map ((,) newHeader) newEntries
    in (nodes, relationships ++ newRelationships)
createCustomHeader :: [(String, FieldType)] -> RelationshipHeader
createCustomHeader fieldsSpecs = RelationshipHeader $ map (uncurry Field) fieldsSpecs


-- GETTING NODES & RELATIONSHIPS


generateNodeTables :: [(NodeHeader, NodeEntry)] -> [String]
generateNodeTables entries = map formatTable $ groupBy1 equalNodeHeaders entries
  where
    formatTable groupedEntries@((NodeHeader fields _, _):_) =
      let header = generateNodeTableHeader fields
          rows = map (\(hdr, entry) -> formatNodeEntry (map (\(Field name _) -> name) fields) hdr entry) groupedEntries
      in unlines (header : rows)

generateRelationshipTables :: [(RelationshipHeader, RelationshipEntry)] -> [String]
generateRelationshipTables entries = map formatTable $ groupBy1 equalRelHeaders entries
  where
    formatTable groupedEntries@((RelationshipHeader fields, _):_) =
      let header = generateRelationshipTableHeader fields
          rows = map (\(hdr, entry) -> formatRelationshipEntry (map (\(Field name _) -> name) fields) hdr entry) groupedEntries
      in unlines (header : rows)

getTables :: File -> ([(NodeHeader, NodeEntry)], [(RelationshipHeader, RelationshipEntry)])
getTables file = (getNodes file, getRelationships file)

generateCSVTables :: ([(NodeHeader, NodeEntry)], [(RelationshipHeader, RelationshipEntry)]) -> ([String], [String])
generateCSVTables (nodeData, relationshipData) = 
  (generateNodeTables nodeData, generateRelationshipTables relationshipData)

groupBy1 :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy1 _ [] = []
groupBy1 eq (x:xs) = (x : filter (eq x) xs) : groupBy1 eq (filter (not . eq x) xs)

fieldTypeToString :: FieldType -> String
fieldTypeToString TypeString = "string"
fieldTypeToString TypeInteger = "integer"
fieldTypeToString TypeBoolean = "boolean"

equalNodeHeaders :: (NodeHeader, NodeEntry) -> (NodeHeader, NodeEntry) -> Bool
equalNodeHeaders (NodeHeader fields1 _, _) (NodeHeader fields2 _, _) = fields1 == fields2

equalRelHeaders :: (RelationshipHeader, RelationshipEntry) -> (RelationshipHeader, RelationshipEntry) -> Bool
equalRelHeaders (RelationshipHeader fields1, _) (RelationshipHeader fields2, _) = fields1 == fields2


-- NODES


getNodes :: File -> [(NodeHeader, NodeEntry)]
getNodes (File nodeSets _) = concatMap getNodesFromSet nodeSets
getNodesFromSet :: NodeSet -> [(NodeHeader, NodeEntry)]
getNodesFromSet (NodeSet header entries) = map (\entry -> (header, entry)) entries

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

filterLabelNodes :: [(NodeHeader, NodeEntry)] -> (String -> Bool) -> [(NodeHeader, NodeEntry)]
filterLabelNodes nodeEntries predicate =
    filter matchesLabelCondition nodeEntries
  where
    matchesLabelCondition :: (NodeHeader, NodeEntry) -> Bool
    matchesLabelCondition (_, NodeEntry _ _ labels) =
      any (labelMatchesPredicate predicate) labels
labelMatchesPredicate :: (String -> Bool) -> Label -> Bool
labelMatchesPredicate predicate (Label label) = predicate label


-- NODE TABLE GENERATION


formatNodeTable :: [(NodeHeader, NodeEntry)] -> [String] -> String
formatNodeTable entries fieldNames =
    let headers = case entries of
                     ((NodeHeader fields _, _):_) -> generateNodeTableHeader fields
                     _ -> ":ID, :LABEL"
        formattedEntries = map (\(NodeHeader fields _, entry) -> formatNodeEntry fieldNames (NodeHeader fields False) entry) entries
    in unlines (headers : formattedEntries)

generateNodeTableHeader :: [Field] -> String
generateNodeTableHeader fields = 
    ":ID, " ++ intercalate ", " (map (\(Field name fType) -> name ++ ":" ++ fieldTypeToString fType) fields) ++ ", :LABEL"

formatNodeEntry :: [String] -> NodeHeader -> NodeEntry -> String
formatNodeEntry fieldNames (NodeHeader fields _) (NodeEntry id literals labels) =
  let fieldValues = map (\name -> fromMaybe "null" (lookupFieldValue name fields literals)) fieldNames
      labelStr = case labels of
                  (Label l : _) -> l
                  _ -> ""
  in intercalate ", " (id : fieldValues ++ [labelStr])

lookupFieldValue :: String -> [Field] -> [Literal] -> Maybe String
lookupFieldValue name fields literals = do
  index <- findIndex (\(Field fName _) -> fName == name) fields
  guard (index < length literals)
  let literal = literals !! index
  return $ literalToString literal

literalToString :: Literal -> String
literalToString (LiteralStr s) = s
literalToString (LiteralInt i) = show i
literalToString (LiteralBool b) = show b
literalToString LiteralNull = "null"


-- RELATIONSHIPS


getRelationships :: File -> [(RelationshipHeader, RelationshipEntry)]
getRelationships (File _ relationshipSets) = concatMap getRelationshipsFromSet relationshipSets
getRelationshipsFromSet :: RelationshipSet -> [(RelationshipHeader, RelationshipEntry)]
getRelationshipsFromSet (RelationshipSet header entries) = map (\entry -> (header, entry)) entries

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


-- RELATIONSHIP TABLE GENERATION


formatRelationshipTable :: [(RelationshipHeader, RelationshipEntry)] -> [String] -> String
formatRelationshipTable entries fieldNames =
    let headers = case entries of
                     ((RelationshipHeader fields, _):_) -> generateRelationshipTableHeader fields
                     _ -> ":START_ID, :END_ID, :TYPE"
        formattedEntries = map (\(RelationshipHeader fields, entry) -> formatRelationshipEntry fieldNames (RelationshipHeader fields) entry) entries
    in unlines (headers : formattedEntries)

generateRelationshipTableHeader :: [Field] -> String
generateRelationshipTableHeader fields =
    let fieldsStr = intercalate ", " (map (\(Field name fType) -> name ++ ":" ++ fieldTypeToString fType) fields)
    in ":START_ID" ++ (if null fieldsStr then "" else ", " ++ fieldsStr) ++ ", :END_ID, :TYPE"

lookupLiteralValue :: String -> [Field] -> [Literal] -> Maybe String
lookupLiteralValue name fields literals = do
    index <- findIndex (\(Field fName _) -> fName == name) fields
    guard (index < length literals)
    let literal = literals !! index
    return $ literalToString literal

formatRelationshipEntry :: [String] -> RelationshipHeader -> RelationshipEntry -> String
formatRelationshipEntry fieldNames (RelationshipHeader fields) (RelationshipEntry startId literals endId relType) =
    let fieldValues = map (\name -> fromMaybe "null" (lookupLiteralValue name fields literals)) fieldNames
    in intercalate ", " ([startId] ++ fieldValues ++ [endId, relType])












-- Josh's Stuff




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