module GqlEvaluator where 

import LangParser
import LangLexer

import InputParser

import Data.List (nub, elemIndex, transpose,groupBy, sort)
import Data.Function (on)

data VariableValue
    = TypeFile ([[FieldEntry]],[[FieldEntry]])
    | TypeNodes [[FieldEntry]]
    | TypeRelations [[FieldEntry]]
    deriving (Show, Eq)

type Variable = (String, VariableValue)
type InputData = ([[FieldEntry]],[[FieldEntry]])
type FieldEntry = (String, String, DataType)

data DataType
    = TypeString 
    | TypeInt 
    | TypeBool 
    | TypeNull
    deriving (Show, Eq)


-- evalQuery :: InputData -> Query -> String 
-- evalQuery inputData (Query _ match) = evalMatch [] inputData match 


-- evalMatch :: [Variable] -> InputData -> Match -> [Variable]
-- evalMatch vars file (Match patterns w r) = undefined 

    
{-----------------------------------------------------------------
----------------------------PATTERNS-----------------------------
------------------------------------------------------------------
-}

-- Evaluates the pattern 
evalPatterns :: [Variable] -> Patterns -> InputData -> [Variable]
evalPatterns vars [] _ = vars 
evalPatterns vars (p:ps) inputdata = evalPatterns vars' ps inputdata 
    where 
        vars' = evalPatterns' vars p inputdata 

-- Function that evaluates individual patterns 
evalPatterns' :: [Variable] -> Pattern -> InputData -> [Variable]
evalPatterns' vars (PatternFinal str1) (nodes,relation) = addVariable vars str1 (TypeNodes nodes) 
evalPatterns' vars (PatternRelatedTo str1 str2) (nodes,relation) = output
    where 
        startIDRelations = getNodesValFromString relation ":START_ID"
        endIDRelations = getNodesValFromString relation "END_ID"
        -- ! list reversed 
        vars'   = addVariable vars      str1 (TypeNodes (reverseList (getNodesbyString startIDRelations nodes ":ID" [])))
        vars''  = addVariable vars'     str2 (TypeNodes (reverseList(getNodesbyString endIDRelations nodes ":ID" [])))
        output  = reverseList vars''
evalPatterns' vars (PatternRelatedToVar str1 str2 str3) (nodes,relation) = output 
    where
        startIDRelations = getNodesValFromString relation ":START_ID"
        endIDRelations = getNodesValFromString relation ":END_ID"
        vars'   = addVariable vars      str1 (TypeNodes (getNodesbyString startIDRelations nodes ":ID" []))
        vars''  = addVariable vars'     str3 (TypeNodes (getNodesbyString endIDRelations nodes ":ID" []))
        vars''' = addVariable vars''    str2 (TypeRelations (getNodesbyString startIDRelations relation ":START_ID" [] ))
        output  = reverseList vars'''
evalPatterns' vars (PatternRelatedBy str1 str2) (nodes,relation)= 
    reverseList (evalPatterns' vars (PatternRelatedTo str2 str1) (nodes,relation))
evalPatterns' vars (PatternRelatedByVar str1 str2 str3) (nodes,relation) = 
    rearrange (evalPatterns' vars (PatternRelatedToVar str3 str2 str1) (nodes,relation))
evalPatterns' vars (PatternRelated str1 str2) (nodes,relation) = output 
    where
        startIDRelations = getNodesValFromString relation ":ID"
        endIDRelations = getNodesValFromString relation ":END_ID"
        vals    = nub ( startIDRelations ++ endIDRelations)
        vars'   = addVariable vars  str1 (TypeNodes ((getNodesbyString (vals) nodes ":ID" [])))
        vars''  = addVariable vars' str2 (TypeNodes ((getNodesbyString (vals) nodes ":ID" [])))
        output  = reverseList vars''

rearrange :: [a] -> [a]
rearrange  (x:s:y) = (s:x:y)

{-----------------------------------------------------------------
----------------------------VARIABLES-----------------------------
------------------------------------------------------------------
-}

-- Add variables to current variable list 
addVariable :: [Variable] -> String -> VariableValue -> [Variable]
addVariable vars name value
    | varPresent vars name = error ("Binding already made for: " ++ name)
    | otherwise            = (name, value) : vars


varPresent :: [Variable] -> String -> Bool
varPresent [] _ = False
varPresent ((varName, varValue):vars) name
    | varName == name   = True
    | otherwise         = varPresent vars name



{-----------------------------------------------------------------
----------------------------FILTERS-----------------------------
------------------------------------------------------------------
-}


-- filters based on predicate on value , can be used on all types of tables 
filterTables ::[[FieldEntry]] -> String -> (String -> Bool) -> [[FieldEntry]] -> [[FieldEntry]]
filterTables [] _ _ acc = filter (not . null) acc
filterTables (n:ns) fieldName predicate acc = filterTables ns fieldName predicate (line:acc)
    where 
        line = (filterFieldEntry n n fieldName predicate)



-- filters based on value 
filterFieldEntry :: [FieldEntry] -> [FieldEntry] -> String-> (String -> Bool)  -> [FieldEntry]
filterFieldEntry _ [] _ _ = [] 
filterFieldEntry line ((field,value,t):xs) str p 
    | str == field = if filter p [value] == [] then filterFieldEntry line xs str p else line 
    | otherwise = filterFieldEntry line xs str p

-- gets values from a nodes that match anything inside a list of strings 
getNodesbyString :: [String] -> [[FieldEntry]] -> String -> [[[FieldEntry]]] -> [[FieldEntry]]
getNodesbyString [] _ _ acc = concat acc
getNodesbyString (x:xs) nodes str acc = getNodesbyString xs nodes str ((filterTables nodes str (\s -> s == x) []):acc)


getNodesValFromString :: [[FieldEntry]] -> String -> [String]
getNodesValFromString [] _ = []
getNodesValFromString (n:ns) str = (getFieldVal n n str) : getNodesValFromString ns str 

-- filters values and produces a list of values 
getFieldVal :: [FieldEntry] -> [FieldEntry] -> String  -> String
getFieldVal _ [] _  = [] 
getFieldVal line ((field,value,t):xs) str 
    | str == field = value  
    | otherwise = getFieldVal line xs str 


reverseList :: [a] -> [a]
reverseList [] = []
reverseList (x:xs) = reverseList xs ++ [x]