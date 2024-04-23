module GqlEvaluator where
import LangParser
import LangLexer
import InputLexer
import InputParser

import System.Console.Terminfo (Color(Magenta))
import Data.List (nub)

data VariableValue
    = TypeFile File
    | TypeNodes Nodes
    | TypeRelations Relations
    deriving (Show, Eq)

type Nodes = [(NodeHeader,NodeEntry)]
type Relations = [(RelationshipHeader, RelationshipEntry)]
type Variables = [(String, VariableValue)]
type Variable = (String, VariableValue)

getVarFromName :: String -> Variables -> VariableValue
getVarFromName name [] = error ("No variable found for " ++ name)
getVarFromName name ((varName, varValue) : vars)
    | name == varName = varValue
    | otherwise = getVarFromName name vars


addVariable :: Variables -> String -> VariableValue ->  Variables
addVariable variables name val = (name, val) : variables

-- evalProgram :: Variables -> Query -> Variables
-- evalProgram (Program [] finalAssignment) = 

-- evalQuery :: Query -> Variables -> 
-- evalQuery (Query read matches )


evalMatch :: Variables -> Match -> File -> Variables
evalMatch vars (MatchWhere patterns conditions return) file = undefined
evalMatch vars (Match (PatternFinal str) return) datafile = addVariable vars str (TypeNodes (getNodes datafile))
evalMatch vars (Match (PatternRelatedTo str (PatternFinal str1)) return) datafile = output
        where 
        vars' = addVariable vars str (TypeNodes (getNodesfromString (getStartId (getRelationships datafile)) (getNodes datafile) []))
        vars'' = addVariable vars' str1 (TypeNodes (getNodesfromString (getEndId (getRelationships datafile)) (getNodes datafile) []))
        output = reverseList vars'' 
evalMatch vars (Match (PatternRelatedToVar str str1 (PatternFinal str2)) return ) datafile = output
        where 
        vars' = addVariable vars str (TypeNodes (getNodesfromString (getStartId (getRelationships datafile)) (getNodes datafile) []))
        vars'' = addVariable vars' str2 (TypeNodes (getNodesfromString (getEndId (getRelationships datafile)) (getNodes datafile) []))
        vars''' = addVariable vars'' str1 (TypeRelations (getRelationshipsfromString (getStartId (getRelationships datafile)) (getRelationships datafile) [] ))
        output = reverseList vars'''  
evalMatch vars (Match (PatternRelatedBy str (PatternFinal str1)) return) datafile 
    = reverseList (evalMatch vars (Match (PatternRelatedTo str1 (PatternFinal str)) return) datafile)
evalMatch vars (Match (PatternRelatedByVar str str1 (PatternFinal str2)) return ) datafile = output 
    where 
    x =  rearrange (evalMatch vars (Match (PatternRelatedToVar str2 str1 (PatternFinal str)) return ) datafile) 
    output = x 
evalMatch vars (Match (PatternRelated str (PatternFinal str1)) return) datafile = output 
    where 
    vals = nub (getStartId (getRelationships datafile) ++ getEndId (getRelationships datafile))
    vars' = addVariable vars str (TypeNodes ((getNodesfromString (vals) (getNodes datafile) [])))
    vars'' = addVariable vars' str1 (TypeNodes ((getNodesfromString (vals) (getNodes datafile) [])))
    output = reverseList vars''


rearrange  (x:s:y)= (s:x:y) 



-- getStartEndId :: [(relationshipHeader, relationshipEntery)] -> String 
getStartId :: [(RelationshipHeader,RelationshipEntry)] -> [String]
getStartId list = nub [ getStartEndId' rE | (_,rE) <- list]



-- getStartEndId :: [(relationshipHeader, relationshipEntery)] -> String 
getEndId :: [(RelationshipHeader,RelationshipEntry)] -> [String]
getEndId list = nub [ getStartEndId'' rE | (_,rE) <- list]

getStartEndId' (RelationshipEntry s _ e _) = s
getStartEndId'' (RelationshipEntry s _ e _) = e

-- gets a list of relationships from a list of strings , filters by start id 
getRelationshipsfromString :: [String] -> [(RelationshipHeader, RelationshipEntry)] -> [[(RelationshipHeader,RelationshipEntry)]] -> [(RelationshipHeader,RelationshipEntry)]
getRelationshipsfromString [] _ acc = concat (nub acc)
getRelationshipsfromString (x:xs) relationships acc = getRelationshipsfromString xs relationships ((filterTypeRelations relationships (\s -> s == x)):acc)

-- Filters the type of the relation table 
filterTypeRelations ::[(RelationshipHeader, RelationshipEntry)] -> (String -> Bool) -> [(RelationshipHeader,RelationshipEntry)]
filterTypeRelations relationshipEnteries predicate = filter matchesConditiont relationshipEnteries
    where 
        matchesConditiont :: (RelationshipHeader, RelationshipEntry) -> Bool 
        matchesConditiont l = matchesCondition l predicate


getRE :: (a, RelationshipEntry) -> String
getRE (rH, rE) = getLastString rE 

getLastString :: RelationshipEntry -> String
getLastString (RelationshipEntry startid _ _ _) = startid


-- matchesCondition :: (RelationshipHeader, RelationshipEntries) -> (String -> Bool) -> Bool 
matchesCondition :: (a, RelationshipEntry) -> (String -> Bool) -> Bool
matchesCondition rE predicate = predicate (getRE rE)







getRelationships :: File -> [(RelationshipHeader, RelationshipEntry)]
getRelationships (File _ relationshipSets) = concatMap getRelationshipsFromSet relationshipSets

getRelationshipsFromSet :: RelationshipSet -> [(RelationshipHeader, RelationshipEntry)]
getRelationshipsFromSet (RelationshipSet header entries) = map (\entry -> (header, entry)) entries


getNodes :: File -> [(NodeHeader, NodeEntry)]
getNodes (File nodeSets _) = concatMap getNodesFromSet nodeSets

getNodesFromSet :: NodeSet -> [(NodeHeader, NodeEntry)]
getNodesFromSet (NodeSet header entries) = map (\entry -> (header, entry)) entries



getNodeString:: NodeEntry -> String
getNodeString (NodeEntry s l _ ) = s


getNE :: (a, NodeEntry) -> String
getNE (nH, nE) = getNodeString nE


matchesConditionN :: (a, NodeEntry) -> (String -> Bool) -> Bool
matchesConditionN rE predicate = predicate (getNE rE)

-- filters node based on predicate 
filterTypeRelationsN ::[(NodeHeader, NodeEntry)] -> (String -> Bool) -> [(NodeHeader,NodeEntry)]
filterTypeRelationsN nodeEnteries predicate = filter matchesConditiont nodeEnteries
    where
        matchesConditiont :: (NodeHeader, NodeEntry) -> Bool
        matchesConditiont l = matchesConditionN l predicate


getNodesfromString :: [String] -> [(NodeHeader, NodeEntry)] -> [[(NodeHeader,NodeEntry)]] -> [(NodeHeader,NodeEntry)]
getNodesfromString [] _ acc = concat acc
getNodesfromString (x:xs) nodes acc = getNodesfromString xs nodes ((filterTypeRelationsN nodes (\s -> s == x)):acc)

reverseList :: [a] -> [a]
reverseList [] = []
reverseList (x:xs) = reverseList xs ++ [x]


-- printResult :: Variables -> String
-- printResult ((bindingName, value): vars)
--     | bindingName == "output" = printOutput value
--     | otherwise = printResult env

-- evalRead :: ReadFile -> File
-- evalRead (ReadFile filename) = readInputFile filename 


-- -- printOutput :: VariableValue -> String
-- -- printOutput (TypeFile file) = printFile file

-- readInputFile fileName = do 
--     sourceText <- readFile fileName
--     let lexedProg = InputLexer.alexScanTokens sourceText
--     let parsedProg = inputParser lexedProg
--     let temp = tempfile parsedProg
--     return parsedProg

-- tempfile :: File -> IO()
-- tempfile file = do 
--     let t = tempfile' file 
--     putStrLn ""

-- tempfile' :: File -> File 
-- tempfile' file = file  

evalQuery :: Variables -> Query -> Variables
evalQuery vars (Query readFile matches) = []

evalReadFile :: Variables -> ReadFile -> Variables
evalReadFile  vars (ReadFile fileName) = []

evalMatches :: Variables -> Matches -> Variables
evalMatches  vars (match:matches) = []

evalPatterns :: Variables -> Patterns -> Variables
evalPatterns vars (PatternFinal nodesVarName) = []
evalPatterns vars (PatternRelated nodesVarName patterns) = []
evalPatterns vars (PatternRelatedVar nodesVarName relationVarName patterns) = []
evalPatterns vars (PatternRelatedTo nodesVarName patterns) = []
evalPatterns vars (PatternRelatedToVar nodesVarName relationVarName patterns) = []
evalPatterns vars (PatternRelatedBy nodesVarName patterns) = []
evalPatterns vars (PatternRelatedByVar nodesVarName relationVarName patterns) = []


evalWhereConditions :: Variables -> WhereConditions -> Variables
evalWhereConditions vars (WhereConditionOr whereCondition whereConditions) = []
evalWhereConditions vars (WhereConditionAnd whereCondition whereConditions) = []
evalWhereConditions vars (WhereConditionNot whereConditions) = []
evalWhereConditions vars (WhereCondition whereCondition) = []


evalWhereCondition :: Variables -> WhereCondition -> Variables
evalWhereCondition vars (IntWhereCondition varName field intCondition int) = []
evalWhereCondition vars (StrWhereCondition varName field strCondition string) = []
evalWhereCondition vars (BoolWhereCondition varName field boolCondition bool) = []
evalWhereCondition vars (LabelWhereCondition varName trCondition tring) = []
evalWhereCondition vars (TypeWhereCondition varName trCondition tring) = []

evalIntCondition :: Variables -> IntCondition -> Variables
evalIntCondition vars (Greater) = []
evalIntCondition vars (Less) = []
evalIntCondition vars (GreaterOrEqual) = []
evalIntCondition vars (LessOrEqual) = []
evalIntCondition vars (IntEqual) = []
evalIntCondition vars (IntNotEqual) = []
evalIntCondition vars (IntIsNull) = []
evalIntCondition vars (IntNotNull) = []

evalStrCondition :: Variables -> StrCondition -> Variables
evalStrCondition vars (StringStarts) = []
evalStrCondition vars (StrEqual) = []
evalStrCondition vars (StrNotEqual) = []
evalStrCondition vars (StrIsNull) = []
evalStrCondition vars (StrNotNull) = []

evalBoolCondition :: Variables -> BoolCondition -> Variables
evalBoolCondition vars (BoolEqual) = []
evalBoolCondition vars (BoolNotEqual) = []
evalBoolCondition vars (BoolIsNull) = []
evalBoolCondition vars (BoolNotNull) = []

evalReturn :: Variables -> Return -> Variables
evalReturn vars (Return outputs) = []

evalOutputs :: Variables -> Outputs -> Variables
evalOutputs vars (output:outputs) = []

evalOutput :: Variables ->  Output -> Variables
evalOutput  vars (StrOutput varName fieldName asName) = []
evalOutput  vars (IntOutput varName fieldName asName) = []
evalOutput  vars (BoolOutput varName fieldName asName) = []
evalOutput  vars (IdOutput varName) = []
evalOutput  vars (StartOutput varName) = []
evalOutput  vars (EndOutput varName) = []
evalOutput  vars (LabelOutput varName) = []
