module GqlEvaluator where

import LangParser
import InputParser

import Data.List (nub, elemIndex, transpose,groupBy, sort, isInfixOf)

type VariableValue  = [FieldEntry]
type Variable       = (String, VariableValue)
type Instance       = [Variable]
type Environment    = [Instance]



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

complementLists :: (Eq a) => [a] -> [a] -> [a]
complementLists a b = filter (\x -> notElem x b) a

unionLists :: [a] -> [a] -> [a]
unionLists a b = a ++ b

startsWith :: Eq a => [a] -> [a] -> Bool
startsWith [] _  = False
startsWith _  [] = True
startsWith (x:xs) (y:ys) = x == y && startsWith xs ys
---------------------------------------------------------------------------------------------------
-- Variable Management Functions
---------------------------------------------------------------------------------------------------

varPresent :: Instance -> String -> Bool
varPresent [] _ = False
varPresent ((varName, _):vars) name
    | varName == name   = True
    | otherwise         = varPresent vars name

getVar :: Instance -> String -> VariableValue
getVar [] name = error ("No binding found for: " ++ name)
getVar ((name, value):vars) varName
    | name == varName = value
    | otherwise       = getVar vars varName 

getField :: [FieldEntry] -> String  -> FieldEntry
getField []                                         field = (field,"null",TypeNull)
getField ((fieldName,fieldValue,fieldType):entries) field
    | fieldName == field = (fieldName,fieldValue,fieldType)
    | otherwise         = getField entries field

getVarField :: Instance -> String -> String -> FieldEntry
getVarField inst varName fieldName = getField (getVar inst varName) fieldName 

-- addVariable :: Variables -> String -> VariableValue -> Variables
-- addVariable vars name value
--     | varPresent vars name = error ("Binding already made for: " ++ name)
--     | otherwise            = (name, value) : vars

unionEnv :: Environment -> Environment -> Environment
unionEnv env1 env2 = nub $ unionLists env1 env2

complementEnv :: Environment -> Environment -> Environment
complementEnv env1 env2 = complementLists env1 env2 

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
-- evalPatterns :: [Variable] -> Patterns -> InputData -> [Variable]
-- evalPatterns vars [] _ = vars
-- evalPatterns vars (p:ps) inputdata = evalPatterns vars' ps inputdata
--     where
--         vars' = evalPatterns' vars p inputdata

-- -- Function that evaluates individual patterns 
-- evalPatterns' :: [Variable] -> Pattern -> InputData -> [Variable]
-- evalPatterns' vars (PatternFinal str1) (nodes,relation) = addVariable vars str1 (TypeNodes nodes)
-- evalPatterns' vars (PatternRelatedTo str1 str2) (nodes,relation) = output
--     where
--         startIDRelations = getNodesValFromString relation ":START_ID"
--         endIDRelations = getNodesValFromString relation ":END_ID"
--         -- ! list reversed 
--         vars'   = addVariable vars      str1 (TypeNodes (reverseList (getNodesbyString startIDRelations nodes ":ID" [])))
--         vars''  = addVariable vars'     str2 (TypeNodes (reverseList (getNodesbyString endIDRelations nodes ":ID" [])))
--         output  = reverseList vars''
-- evalPatterns' vars (PatternRelatedToVar str1 str2 str3) (nodes,relation)= output
--     where
--         startIDRelations = getNodesValFromString relation ":ID"
--         endIDRelations = getNodesValFromString relation ":END_ID"
--         -- ! list reversed 
--         vars'   = addVariable vars      str1 (TypeNodes (reverseList (getNodesbyString startIDRelations nodes ":ID" [])))
--         vars''  = addVariable vars'     str3 (TypeNodes (reverseList (getNodesbyString endIDRelations nodes ":ID" [])))
--         vars''' = addVariable vars''    str2 (TypeRelations (reverseList (getNodesbyString startIDRelations relation ":START_ID" [] )))
--         output  = reverseList vars'''
-- evalPatterns' vars (PatternRelatedBy str1 str2) (nodes,relation)=
--     reverseList (evalPatterns' vars (PatternRelatedTo str2 str1) (nodes,relation))
-- evalPatterns' vars (PatternRelatedByVar str1 str2 str3) (nodes,relation) =
--     rearrange (evalPatterns' vars (PatternRelatedToVar str3 str2 str1) (nodes,relation))
-- evalPatterns' vars (PatternRelated str1 str2) (nodes,relation) = output
--     where
--         startIDRelations = getNodesValFromString relation ":START_ID"
--         endIDRelations = getNodesValFromString relation ":END_ID"
--         vals    = ( startIDRelations ++ endIDRelations)
--         -- ! list reversed 
--         vars'   = addVariable vars  str1 (TypeNodes (reverseList ((getNodesbyString (vals) nodes ":ID" []))))
--         vars''  = addVariable vars' str2 (TypeNodes (reverseList ((getNodesbyString (vals) nodes ":ID" []))))
--         output  = reverseList vars''

-- rearrange :: [a] -> [a]
-- rearrange (x:s:y) = (s:x:y)



{-----------------------------------------------------------------
----------------------------FILTERS-----------------------------
------------------------------------------------------------------
-}


-- -- filters based on predicate on value , can be used on all types of tables 
-- filterTables ::[[FieldEntry]] -> String -> (String -> Bool) -> [[FieldEntry]] -> [[FieldEntry]]
-- filterTables [] _ _ acc = filter (not . null) acc
-- filterTables (n:ns) fieldName predicate acc = filterTables ns fieldName predicate (line:acc)
--     where
--         line = (filterFieldEntry n n fieldName predicate)



-- filterFieldEntry :: [FieldEntry] -> [FieldEntry] -> String -> (String -> Bool)  -> [FieldEntry]
-- filterFieldEntry _ [] _ _ = []
-- filterFieldEntry line ((field,value,t):xs) str p
--     | str == field = if filter p [value] == [] then filterFieldEntry line xs str p else line
--     | otherwise = filterFieldEntry line xs str p

-- -- gets values from a nodes that match anything inside a list of strings 
-- getNodesbyString :: [String] -> [[FieldEntry]] -> String -> [[[FieldEntry]]] -> [[FieldEntry]]
-- getNodesbyString [] _ _ acc = concat acc
-- getNodesbyString (x:xs) nodes str acc = getNodesbyString xs nodes str ((filterTables nodes str (\s -> s == x) []):acc)


-- getNodesValFromString :: [[FieldEntry]] -> String -> [String]
-- getNodesValFromString [] _ = []
-- getNodesValFromString (n:ns) str = (getFieldVal n str) : getNodesValFromString ns str

-- -- filters values and produces a list of values 
-- getFieldVal :: [FieldEntry] -> String  -> String
-- getFieldVal [] _  = []
-- getFieldVal ((field,value,_):xs) str
--     | str == field = value
--     | otherwise = getFieldVal xs str


-- getNodeVal :: [[FieldEntry]] -> String -> [FieldEntry]
-- getNodeVal [] _ = [] 
-- getNodeVal (f:fs) str  = (getNodeVal' f f str) : getNodeVal fs str 

-- getNodeVal' :: [FieldEntry] -> [FieldEntry] -> String  -> FieldEntry
-- getNodeVal' _ [] str  = (str,"null", TypeNull)
-- getNodeVal' line ((field,value,t):xs) str 
--     | str == field = (field,value,t)  
--     | otherwise = getNodeVal' line xs str 


-- multiZipL :: [[a]] -> [[a]]
-- multiZipL = transpose


-- extractVarVal :: VariableValue -> [[FieldEntry]]
-- extractVarVal (TypeNodes node ) = node 
-- extractVarVal (TypeRelations relation ) = relation 


-- getHeader:: [[FieldEntry]] -> [[String]]
-- getHeader [] = [] 
-- getHeader (n:ns) = [getHeader'' result  result]
--     where 
--         result =  (nub ((getHeader' n) : (getHeader ns)))

-- getHeader' :: [FieldEntry] -> [String]
-- getHeader' [] = [] 
-- getHeader' ((str,val,t):fs) = (str ++ showDataType t) : getHeader' fs 

-- getHeader'' :: [[String]] -> [[String]] -> [String]
-- getHeader'' [] (str1:xs) = str1  -- process the plus out of it 
-- getHeader'' (str:strlist) str1
--     | allTrue (getHeader''' str) = str 
--     | otherwise = getHeader'' strlist str1

-- getHeader''' :: [String] -> [Bool] 
-- getHeader''' [] = [] 
-- getHeader''' (x:xs) 
--     | isInfixOf ":null" x = False : getHeader''' xs 
--     | otherwise = True : getHeader''' xs 

-- allTrue :: [Bool] -> Bool
-- allTrue = and

-- showDataType:: DataType -> String 
-- showDataType (TypeString) = ":string"
-- showDataType (TypeInt) = ":integer"
-- showDataType (TypeBool) = ":boolean"
-- showDataType (TypeNull) = ":null"

-- getEntryVal:: [[FieldEntry]] -> [[String]]
-- getEntryVal [] = [] 
-- getEntryVal (x:xs) = getValF x : getEntryVal xs 

-- getValF:: [FieldEntry] -> [String]
-- getValF [] = [] 
-- getValF ((s,v,t):xs) = v : getValF xs

-- removeHeader :: [[[String]]] -> [[[String]]]
-- removeHeader [] = [] 
-- removeHeader (x:xs) = removeHeader' x : removeHeader xs

-- removeHeader' :: [[String]] -> [[String]]
-- removeHeader' [] = [] 
-- removeHeader' (x:xs) = xs 



-- sameHeader:: [[[String]]] ->[[[String]]]
-- sameHeader [] = []
-- sameHeader ((h:rest):xs) = result ++ sameHeader notsameheader
--     where 
--         boollist = sameHeader'' h xs 
--         mapped = (map snd $ filter fst $ zip boollist xs )
--         mapped1 = removeHeader mapped 
--         result = (h:rest) : mapped1
--         notsameheader = map snd $ filter (not . fst) $ zip boollist xs

-- sameHeader' :: [String] -> [[String]] -> Bool 
-- sameHeader' header list 
--     | header `elem` list = True 
--     | otherwise = False 

-- sameHeader'' :: [String] -> [[[String]]] -> [Bool]
-- sameHeader'' _ [] = [] 
-- sameHeader'' header (x:xs) = (sameHeader' header x ) : sameHeader'' header xs 


{--------------------------------------------------------------------------------------------------
------------------------------------------RETURN---------------------------------------------------
---------------------------------------------------------------------------------------------------}

-- -- checks if any two outputs have the same header, and if so merges them to be a part of the same output
-- evalOutputChecker :: [[[String]]] -> [[[String]]]
-- evalOutputChecker outlist = undefined


-- evalOutputs :: [Variable] -> Outputs -> [[String]] 
-- evalOutputs vars out =  (concat (getHeader (result)) : (resultval))
--     where
--         result = evalOutputs'' vars out 
--         resultval = getEntryVal result 
    


-- evalOutputs'' :: [Variable] -> Outputs -> [[FieldEntry]] 
-- evalOutputs'' vars outs = (multiZipL (evalOutputs''' vars outs))

-- evalOutputs''' :: [Variable] -> Outputs -> [[FieldEntry]] 
-- evalOutputs''' vars [] = [] 
-- evalOutputs''' vars (out:outs) =  (evalOutput vars out : evalOutputs''' vars outs )

-- evalOutput :: [Variable] -> Output -> [FieldEntry]
-- evalOutput vars (Output str1 str2 str3) =  output 
--     where 
--         node = (evalOutputHelper vars (Output str1 str2 str3))
--         filteredval = getNodeVal node str2 
--         output = filteredval 


-- evalOutputHelper :: [Variable] -> Output -> [[FieldEntry]]
-- evalOutputHelper vars (Output str1 str2 str3) =  (extractVarVal (getVar vars str1))



-------------------------------------------------------------------
-- Evaluating Where 
-------------------------------------------------------------------

evalWhere :: Environment -> Where -> Environment
evalWhere env (Where whereExp) = evalWhereExp env whereExp

evalWhereExp :: Environment -> WhereExp -> Environment
evalWhereExp env (WNot whereExp)           = complementEnv (evalWhereExp env whereExp) env
evalWhereExp env (WAnd whereFunc whereExp) = evalWhereExp  (filter (\x -> evalWhereFunc x whereFunc) env) whereExp
evalWhereExp env (WOr whereFunc whereExp)  = unionEnv      (filter (\x -> evalWhereFunc x whereFunc) env) (evalWhereExp env whereExp)
evalWhereExp env (WFinal whereFunc)        =               (filter (\x -> evalWhereFunc x whereFunc) env)

evalWhereFunc :: Instance -> WhereFunc -> Bool
evalWhereFunc inst (WEqualDot              (WDot v1 f1) (WDot v2 f2)) = evalWhereEqual          (getVarField inst v1 f1) (getVarField inst v2 f2)
evalWhereFunc inst (WNotEqualDot           (WDot v1 f1) (WDot v2 f2)) = evalWhereNotEqual       (getVarField inst v1 f1) (getVarField inst v2 f2)
evalWhereFunc inst (WLessThanDot           (WDot v1 f1) (WDot v2 f2)) = evalWhereLess           (getVarField inst v1 f1) (getVarField inst v2 f2)
evalWhereFunc inst (WGreaterThanDot        (WDot v1 f1) (WDot v2 f2)) = evalWhereGreater        (getVarField inst v1 f1) (getVarField inst v2 f2)
evalWhereFunc inst (WLessOrEqualThanDot    (WDot v1 f1) (WDot v2 f2)) = evalWhereLessOrEqual    (getVarField inst v1 f1) (getVarField inst v2 f2)
evalWhereFunc inst (WGreaterOrEqualThanDot (WDot v1 f1) (WDot v2 f2)) = evalWhereGreaterOrEqual (getVarField inst v1 f1) (getVarField inst v2 f2)
evalWhereFunc inst (WStartsWithDot         (WDot v1 f1) (WDot v2 f2)) = evalWhereStarts         (getVarField inst v1 f1) (getVarField inst v2 f2)
evalWhereFunc inst (WEndsWithDot           (WDot v1 f1) (WDot v2 f2)) = evalWhereEnds           (getVarField inst v1 f1) (getVarField inst v2 f2)

evalWhereFunc inst (WEqual                 (WDot v1 f1) wlit)         = evalWhereEqual          (getVarField inst v1 f1) (wLitToFieldEntry wlit)
evalWhereFunc inst (WNotEqual              (WDot v1 f1) wlit)         = evalWhereNotEqual       (getVarField inst v1 f1) (wLitToFieldEntry wlit)
evalWhereFunc inst (WLessThan              (WDot v1 f1) wlit)         = evalWhereLess           (getVarField inst v1 f1) (wLitToFieldEntry wlit)
evalWhereFunc inst (WGreaterThan           (WDot v1 f1) wlit)         = evalWhereGreater        (getVarField inst v1 f1) (wLitToFieldEntry wlit)
evalWhereFunc inst (WLessOrEqualThan       (WDot v1 f1) wlit)         = evalWhereLessOrEqual    (getVarField inst v1 f1) (wLitToFieldEntry wlit)
evalWhereFunc inst (WGreaterOrEqualThan    (WDot v1 f1) wlit)         = evalWhereGreaterOrEqual (getVarField inst v1 f1) (wLitToFieldEntry wlit)
evalWhereFunc inst (WStartsWith            (WDot v1 f1) wlit)         = evalWhereStarts         (getVarField inst v1 f1) (wLitToFieldEntry wlit)
evalWhereFunc inst (WEndsWith              (WDot v1 f1) wlit)         = evalWhereEnds           (getVarField inst v1 f1) (wLitToFieldEntry wlit)

wLitToFieldEntry :: WhereLit -> FieldEntry
wLitToFieldEntry (WStr s)  = ("", s,      TypeString)
wLitToFieldEntry (WInt i)  = ("", show i, TypeInt)
wLitToFieldEntry (WBool b) = ("", show b, TypeBool)
wLitToFieldEntry (WNull)   = ("", "null", TypeNull)

evalWhereEqual    :: FieldEntry -> FieldEntry -> Bool
evalWhereEqual (_,v1,_) (_,v2,_) = v1 == v2

evalWhereNotEqual :: FieldEntry -> FieldEntry -> Bool
evalWhereNotEqual f1 f2 = not $ evalWhereEqual f1 f2

evalWhereLess :: FieldEntry -> FieldEntry -> Bool
evalWhereLess (f1,"null",t1) (f2,"null",t2)   = False
evalWhereLess (f1,v1,TypeInt) (f2,v2,TypeInt) = (read v1 :: Int) < (read v2)
evalWhereLess (f1,v1,t1) (f2,v2,t2)           = False

evalWhereGreater :: FieldEntry -> FieldEntry -> Bool
evalWhereGreater (f1,"null",t1) (f2,"null",t2)   = False
evalWhereGreater (f1,v1,TypeInt) (f2,v2,TypeInt) = (read v1 :: Int) > (read v2)
evalWhereGreater (f1,v1,t1) (f2,v2,t2)           = False

evalWhereLessOrEqual :: FieldEntry -> FieldEntry -> Bool
evalWhereLessOrEqual (f1,"null",t1) (f2,"null",t2)   = False
evalWhereLessOrEqual (f1,v1,TypeInt) (f2,v2,TypeInt) = (read v1 :: Int) <= (read v2)
evalWhereLessOrEqual (f1,v1,t1) (f2,v2,t2)           = False

evalWhereGreaterOrEqual :: FieldEntry -> FieldEntry -> Bool
evalWhereGreaterOrEqual (f1,"null",t1) (f2,"null",t2)   = False
evalWhereGreaterOrEqual (f1,v1,TypeInt) (f2,v2,TypeInt) = (read v1 :: Int) >= (read v2)
evalWhereGreaterOrEqual (f1,v1,t1) (f2,v2,t2)           = False

evalWhereStarts :: FieldEntry -> FieldEntry -> Bool
evalWhereStarts (f1,"null",t1) (f2,"null",t2)         = False
evalWhereStarts (f1,v1,TypeString) (f2,v2,TypeString) = startsWith v1 v2
evalWhereStarts (f1,v1,t1) (f2,v2,t2)                 = False

evalWhereEnds :: FieldEntry -> FieldEntry -> Bool 
evalWhereEnds (f1,"null",t1) (f2,"null",t2)         = False
evalWhereEnds (f1,v1,TypeString) (f2,v2,TypeString) = startsWith (reverse v1) (reverse v2)
evalWhereEnds (f1,v1,t1) (f2,v2,t2)                 = False