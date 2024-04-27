module GqlEvaluator where 

import LangParser
import LangLexer

import InputParser

import Data.List (nub, elemIndex, transpose,groupBy, sort)
import Data.Function (on)

data VariableValue
    = TypeNodes [[FieldEntry]]
    | TypeRelations [[FieldEntry]]
    deriving (Show, Eq)

type Variables = [Variable]
type Variable = (String, VariableValue)
type InputData = ([[FieldEntry]],[[FieldEntry]])




-- evalQuery :: InputData -> Query -> String 
-- evalQuery inputData (Query _ match) = evalMatch [] inputData match 


-- evalMatch :: [Variable] -> InputData -> Match -> [Variable]
-- evalMatch vars file (Match patterns w r) = undefined 

---------------------------------------------------------------------------------------------------
-- Helper Functions
---------------------------------------------------------------------------------------------------
reverseList :: [a] -> [a]
reverseList [] = []
reverseList (x:xs) = reverseList xs ++ [x]
---------------------------------------------------------------------------------------------------
-- Variable Management Functions
---------------------------------------------------------------------------------------------------
varPresent :: Variables -> String -> Bool
varPresent [] _ = False
varPresent ((varName, varValue):vars) name
    | varName == name   = True
    | otherwise         = varPresent vars name

getVarValueFromName :: Variables -> String -> VariableValue
getVarValueFromName [] name = error ("No binding found for: " ++ name)
getVarValueFromName ((varName, varValue) : vars) name
    | name == varName   = varValue
    | otherwise         = getVarValueFromName vars name


addVariable :: Variables -> String -> VariableValue -> Variables
addVariable vars name value
    | varPresent vars name = error ("Binding already made for: " ++ name)
    | otherwise            = (name, value) : vars

updateVariable :: Variables -> String -> VariableValue -> Variables
updateVariable [] name _    = error ("No binding found for: " ++ name)
updateVariable (var@(varName, varValue) : vars) name value
    | name == varName       = (name, value) : vars
    | otherwise             = var : updateVariable vars name value

extractVariableNodes :: VariableValue -> Nodes
extractVariableNodes (TypeNodes nodes) = nodes
extractVariableRelations :: VariableValue -> Relations
extractVariableRelations (TypeRelations relations) = relations
---------------------------------------------------------------------------------------------------
-- Evaluating ReadFile
---------------------------------------------------------------------------------------------------
evalReadFile :: ReadFile -> String
evalReadFile (ReadFile fileName) = fileName
---------------------------------------------------------------------------------------------------
-- Evaluating Match
---------------------------------------------------------------------------------------------------




---------------------------------------------------------------------------------------------------
-- Evaluating Patterns 
---------------------------------------------------------------------------------------------------
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
        endIDRelations = getNodesValFromString relation ":END_ID"
        -- ! list reversed 
        vars'   = addVariable vars      str1 (TypeNodes (reverseList (getNodesbyString startIDRelations nodes ":ID" [])))
        vars''  = addVariable vars'     str2 (TypeNodes (reverseList(getNodesbyString endIDRelations nodes ":ID" [])))
        output  = reverseList vars''
evalPatterns' vars (PatternRelatedToVar str1 str2 str3) (nodes,relation) = output 
    where
        startIDRelations = getNodesValFromString relation ":START_ID"
        endIDRelations = getNodesValFromString relation ":END_ID"
        -- ! list reversed 
        vars'   = addVariable vars      str1 (TypeNodes (reverseList (getNodesbyString startIDRelations nodes ":ID" [])))
        vars''  = addVariable vars'     str3 (TypeNodes (reverseList(getNodesbyString endIDRelations nodes ":ID" [])))
        vars''' = addVariable vars''    str2 (TypeRelations (reverseList (getNodesbyString startIDRelations relation ":START_ID" [] )))
        output  = reverseList vars'''
evalPatterns' vars (PatternRelatedBy str1 str2) (nodes,relation)= 
    reverseList (evalPatterns' vars (PatternRelatedTo str2 str1) (nodes,relation))
evalPatterns' vars (PatternRelatedByVar str1 str2 str3) (nodes,relation) = 
    rearrange (evalPatterns' vars (PatternRelatedToVar str3 str2 str1) (nodes,relation))
evalPatterns' vars (PatternRelated str1 str2) (nodes,relation) = output 
    where
        startIDRelations = getNodesValFromString relation ":START_ID"
        endIDRelations = getNodesValFromString relation ":END_ID"
        vals    = ( startIDRelations ++ endIDRelations)
        -- ! list reversed 
        vars'   = addVariable vars  str1 (TypeNodes (reverseList ((getNodesbyString (vals) nodes ":ID" []))))
        vars''  = addVariable vars' str2 (TypeNodes (reverseList ((getNodesbyString (vals) nodes ":ID" []))))
        output  = reverseList vars''

rearrange :: [a] -> [a]
rearrange (x:s:y) = (s:x:y)



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



-- filters based on value or fieldname 
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


getNodeVal :: [[FieldEntry]] -> String -> [FieldEntry]
getNodeVal [] _ = [] 
getNodeVal (f:fs) str  = (getNodeVal' f f str) : getNodeVal fs str 

getNodeVal' :: [FieldEntry] -> [FieldEntry] -> String  -> FieldEntry
getNodeVal' _ [] str  = (str,"null", TypeNull)
getNodeVal' line ((field,value,t):xs) str 
    | str == field = (field,value,t)  
    | otherwise = getNodeVal' line xs str 


multiZipL :: [[a]] -> [[a]]
multiZipL = transpose


extractVarVal :: VariableValue -> [[FieldEntry]]
extractVarVal (TypeNodes node ) = node 
extractVarVal (TypeRelations relation ) = relation 


getHeader:: [[FieldEntry]] -> [[String]]
getHeader [] = [] 
getHeader (n:ns) = [getHeader'' result  result]
    where 
        result =  (nub ((getHeader' n) : (getHeader ns)))

getHeader' :: [FieldEntry] -> [String]
getHeader' [] = [] 
getHeader' ((str,val,t):fs) = (str ++ showDataType t) : getHeader' fs 

getHeader'' :: [[String]] -> [[String]] -> [String]
getHeader'' [] (str1:xs) = str1  -- process the plus out of it 
getHeader'' (str:strlist) str1
    | allTrue (getHeader''' str) = str 
    | otherwise = getHeader'' strlist str1

getHeader''' :: [String] -> [Bool] 
getHeader''' [] = [] 
getHeader''' (x:xs) 
    | '+' `elem` x = False : getHeader''' xs 
    | otherwise = True : getHeader''' xs 

allTrue :: [Bool] -> Bool
allTrue = and

showDataType:: DataType -> String 
showDataType (TypeString) = ":string"
showDataType (TypeInt) = ":integer"
showDataType (TypeBool) = ":boolean"
showDataType (TypeNull) = ":null"

getEntryVal:: [[FieldEntry]] -> [[String]]
getEntryVal [] = [] 
getEntryVal (x:xs) = getValF x : getEntryVal xs 

getValF:: [FieldEntry] -> [String]
getValF [] = [] 
getValF ((s,v,t):xs) = v : getValF xs

removeHeader :: [[[String]]] -> [[[String]]]
removeHeader [] = [] 
removeHeader (x:xs) = removeHeader' x : removeHeader xs

removeHeader' :: [[String]] -> [[String]]
removeHeader' [] = [] 
removeHeader' (x:xs) = xs 



sameHeader:: [[[String]]] ->[[[String]]]
sameHeader [] = []
sameHeader ((h:rest):xs) = result ++ sameHeader notsameheader
    where 
        boollist = sameHeader'' h xs 
        mapped = (map snd $ filter fst $ zip boollist xs )
        mapped1 = removeHeader mapped 
        result = (h:rest) : mapped1
        notsameheader = map snd $ filter (not . fst) $ zip boollist xs

sameHeader' :: [String] -> [[String]] -> Bool 
sameHeader' header list 
    | header `elem` list = True 
    | otherwise = False 

sameHeader'' :: [String] -> [[[String]]] -> [Bool]
sameHeader'' _ [] = [] 
sameHeader'' header (x:xs) = (sameHeader' header x ) : sameHeader'' header xs 


{--------------------------------------------------------------------------------------------------
------------------------------------------RETURN---------------------------------------------------
---------------------------------------------------------------------------------------------------}

-- checks if any two outputs have the same header, and if so merges them to be a part of the same output
evalOutputChecker :: [[[String]]] -> [[[String]]]
evalOutputChecker outlist = undefined


evalOutputs :: [Variable] -> Outputs -> [[String]] 
evalOutputs vars out = concat (getHeader (result)) : resultval
    where
        result = evalOutputs'' vars out 
        resultval = getEntryVal result 


evalOutputs'' :: [Variable] -> Outputs -> [[FieldEntry]] 
evalOutputs'' vars outs = multiZipL (evalOutputs''' vars outs)

evalOutputs''' :: [Variable] -> Outputs -> [[FieldEntry]] 
evalOutputs''' vars [] = [] 
evalOutputs''' vars (out:outs) = evalOutput vars out : evalOutputs''' vars outs 

evalOutput :: [Variable] -> Output -> [FieldEntry]
evalOutput vars (Output str1 str2 str3) = output 
    where 
        node = (evalOutputHelper vars (Output str1 str2 str3))
        filteredval = getNodeVal node str2 
        output = filteredval 


evalOutputHelper :: [Variable] -> Output -> [[FieldEntry]]
evalOutputHelper vars (Output str1 str2 str3) = extractVarVal (getVarValueFromName vars str1)




