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

removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates [] = []
removeDuplicates (a:as) = a : removeDuplicates (filter (\x -> x /= a) as)

unionLists :: Eq a => [a] -> [a] -> [a]
unionLists a b = removeDuplicates $ a ++ b

interSectionLists :: Eq a => [a] -> [a] -> [a]
interSectionLists [] _ = []
interSectionLists _ [] = []
interSectionLists a b = removeDuplicates $ filter (\x -> elem x a) b

complementLists :: (Eq a) => [a] -> [a] -> [a]
complementLists a b = filter (\x -> notElem x b) a

extractVariableData :: VariableValue -> [[FieldEntry]]
extractVariableData (TypeNodes nodes) = nodes
extractVariableData (TypeRelations relations) = relations

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


unionVars :: Variables -> Variables -> Variables
unionVars [] _ = []
unionVars ((var1Name, (TypeNodes var1Nodes)):vars1) vars2 = outputVar : unionVars vars1 vars2
    where
        var2Value = getVarValueFromName vars2 var1Name
        outputVar = (var1Name, TypeNodes $ unionLists var1Nodes (extractVariableData var2Value))
unionVars ((var1Name, (TypeRelations var1Relations)):vars1) vars2 = outputVar : unionVars vars1 vars2
    where
        var2Value = getVarValueFromName vars2 var1Name
        outputVar = (var1Name, TypeRelations $ unionLists var1Relations (extractVariableData var2Value))

complementVars :: Variables -> Variables -> Variables
complementVars [] _ = []
complementVars ((var1Name, (TypeNodes var1Nodes)):vars1) vars2 = outputVar : complementVars vars1 vars2
    where
        var2Value = getVarValueFromName vars2 var1Name
        outputVar = (var1Name, TypeNodes $ complementLists var1Nodes (extractVariableData var2Value))
complementVars ((var1Name, (TypeRelations var1Relations)):vars1) vars2 = outputVar : complementVars vars1 vars2
    where
        var2Value = getVarValueFromName vars2 var1Name
        outputVar = (var1Name, TypeRelations $ complementLists var1Relations (extractVariableData var2Value))
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
evalPatterns' vars (PatternRelatedToVar str1 str2 str3) (nodes,relation)= output 
    where
        startIDRelations = getNodesValFromString relation ":ID"
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

-- filters based on value 

filterFieldEntry :: [FieldEntry] -> [FieldEntry] -> String -> (String -> Bool)  -> [FieldEntry]
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
getNodesValFromString (n:ns) str = (getFieldVal n str) : getNodesValFromString ns str 

-- filters values and produces a list of values 
getFieldVal :: [FieldEntry] -> String  -> String
getFieldVal [] _  = [] 
getFieldVal ((field,value,_):xs) str 
    | str == field = value  
    | otherwise = getFieldVal xs str 

-------------------------------------------------------------------
-- Evaluating Where 
-------------------------------------------------------------------

varTypeNodes :: VariableValue -> Bool
varTypeNodes (TypeNodes _) = True
varTypeNodes _ = False 

varTypeRelations :: VariableValue -> Bool
varTypeRelations (TypeRelations _) = True
varTypeRelations _ = False 

getFieldType :: [FieldEntry] -> String -> DataType 
getFieldType ((field,_,t):xs) str 
    | str == field = t  
    | otherwise = getFieldType xs str 


evalWhere :: Variables -> Where -> Variables
evalWhere vars (Where whereExp) = evalWhereExp vars  whereExp 

evalWhereExp :: Variables -> WhereExp -> Variables
evalWhereExp vars (WAnd whereFunc whereExp) = evalWhereExp (evalWhereFunc vars whereFunc) whereExp
evalWhereExp vars (WOr whereFunc whereExp) = unionVars (evalWhereFunc vars whereFunc) (evalWhereExp vars whereExp) 
evalWhereExp vars (WNot whereExp) = complementVars (evalWhereExp vars whereExp) vars
evalWhereExp vars (WFinal whereFunc) = evalWhereFunc vars whereFunc


evalWhereFunc :: Variables -> WhereFunc -> Variables
evalWhereFunc vars (WEqual wdot@(WDot _ _) wlit) = evalWhereEqual vars wdot wlit (==)


evalGeneralFunc 

-- evalWhereEqual :: Variables -> WhereDot -> WhereLit -> (String -> String -> Bool) -> Variables
evalWhereEqual vars (WDot str1 str2) (WStr s) predicate
    | varTypeNodes var = updateVariable vars str1 $ TypeNodes (x nodes str1 str2 predicate)
    | otherwise = updateVariable vars str1 $ TypeRelations (x nodes str1 str2 predicate)
    where
        var = getVarValueFromName vars str1
        nodes = extractVariableData var


-- x :: [[FieldEntry]] -> String -> String -> (String -> String -> Bool) -> [[FieldEntry]] 
x [] _ _ _ = []
x (node:nodes) fieldName comp predicate
    | predicate fieldVal comp = node : x nodes fieldName comp predicate
    | otherwise = x nodes fieldName comp predicate
    where
        fieldVal = getFieldVal node fieldName




-- evalWhereEqual :: Variables -> WhereDot -> WhereLit -> Variables
-- evalWhereEqual vars (WDot str1 str2) (WStr s) = filter (\n -> filterWhereFunc n (==) str1 str2) nodes  
--     where
--         nodes = extractVariableData $ getVarValueFromName vars str1
        

-- filterWhereFunc :: [FieldEntry] -> (String -> String -> Bool) -> String -> String -> Bool
-- filterWhereFunc entrys pred field compareTo = pred fieldVal compareTo 
--     where
--         fieldVal = getFieldVal entrys field 
        
