module GqlEvaluator where

import LangParser
import InputParser

import Data.List (nub, elemIndex, transpose,groupBy, sort, isInfixOf, intercalate)
import Distribution.Fields.Field (fieldName, Field)
import Text.Read (readMaybe)

type VariableValue  = [FieldEntry]
type Variable       = (String, VariableValue)
type Instance       = [Variable]
type Environment    = [Instance]

type InputData      = ([Node],[Relation])

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

removeEmptyLists :: Eq a => [[a]] -> [[a]]
removeEmptyLists [] = []
removeEmptyLists (a:as)
    | a == []   = removeEmptyLists as
    | otherwise = a : removeEmptyLists as
---------------------------------------------------------------------------------------------------
-- Variable Management Functions
---------------------------------------------------------------------------------------------------
varPresent :: Instance -> String -> Bool
varPresent [] _ = False
varPresent ((varName, _):vars) name
    | varName == name   = True
    | otherwise         = varPresent vars name

getVar :: Instance -> String -> VariableValue
getVar [] name = [] 
getVar ((name, value):vars) varName
    | name == varName = value
    | otherwise       = getVar vars varName

getVars :: Environment -> String -> [VariableValue]
getVars [] _ = []
getVars (e:es) name = filter(not . null) (getVar e name : getVars es name )

getValues :: FieldEntry -> String 
getValues (s,ss,sss) = ss 

getFields :: [[FieldEntry]] -> String -> [FieldEntry]
getFields [] _ = [] 
getFields (x:xs) str = getField x str : getFields xs str

getField :: [FieldEntry] -> String  -> FieldEntry
getField []                                         field = (field,"null",TypeNull)
getField ((fieldName,fieldValue,fieldType):entries) field
    | fieldName == field = (fieldName,fieldValue,fieldType)
    | otherwise          = getField entries field

getVarField :: Instance -> String -> String -> FieldEntry
getVarField inst varName fieldName = getField (getVar inst varName) fieldName

addVariable :: Instance -> String -> VariableValue -> Instance
addVariable inst name value
    | varPresent inst name = error ("Binding already made for: " ++ name)
    | otherwise            = (name, value) : inst

unionEnv :: Environment -> Environment -> Environment
unionEnv env1 env2 = nub $ unionLists env1 env2

complementEnv :: Environment -> Environment -> Environment
complementEnv env1 env2 = complementLists env1 env2 
---------------------------------------------------------------------------------------------------
-- Evaluating Query
---------------------------------------------------------------------------------------------------
evalQuery :: InputData -> Query -> [[String]] 
evalQuery inputData (Query _ match wher print) = output
    where 
        env1 = evalMatch match inputData 
        env2 = evalWhere env1 wher
        output = evalPrint env2 print inputData
---------------------------------------------------------------------------------------------------
-- Evaluating ReadFile
---------------------------------------------------------------------------------------------------
evalReadFile :: ReadFile -> String
evalReadFile (ReadFile fileName) = fileName
---------------------------------------------------------------------------------------------------
-- Evaluating Match
---------------------------------------------------------------------------------------------------
evalMatch :: Match -> InputData -> Environment
evalMatch (Match patterns) inputData = evalPatterns [] patterns inputData
---------------------------------------------------------------------------------------------------
-- Evaluating Patterns 
---------------------------------------------------------------------------------------------------
evalPatterns :: Environment -> Patterns -> InputData -> Environment
evalPatterns env [] _ = env
evalPatterns []  (pattern:patterns) inputData = evalPatterns env1 patterns inputData
    where
        env1 = evalPattern [] pattern inputData
evalPatterns env (pattern:patterns) inputData = evalPatterns env1 patterns inputData   
    where
        env1 = removeEmptyLists $ concat $ map (\x -> evalPattern x pattern inputData) env

evalPattern :: Instance -> Pattern -> InputData -> Environment
evalPattern inst (Pattern str1) (nodes,relations) = env1 ++ env2
    where 
        env1 = map (\x -> evalPatternSimple inst str1 x) nodes
        env2 = map (\x -> evalPatternSimple inst str1 x) relations 
evalPattern inst (PatternRelatedTo str1 str2 str3) (nodes,relations) = env3 
    where
        env1 =                                        map (\x -> evalPatternSimple inst   str2 x) relations
        env2 = removeEmptyLists $ concat $ map (\y -> map (\x -> evalPatternStart  y str1 str2 x) nodes) env1
        env3 = removeEmptyLists $ concat $ map (\y -> map (\x -> evalPatternEnd    y str3 str2 x) nodes) env2
evalPattern inst (PatternRelatedBy str1 str2 str3) (nodes,relations) = env3 
    where
        env1 =                                        map (\x -> evalPatternSimple inst str2 x) relations
        env2 = removeEmptyLists $ concat $ map (\y -> map (\x -> evalPatternStart  y str3 str2 x) nodes) env1
        env3 = removeEmptyLists $ concat $ map (\y -> map (\x -> evalPatternEnd    y str1 str2 x) nodes) env2


evalPatternSimple :: Instance -> String -> VariableValue -> Instance
evalPatternSimple inst var1 line = addVariable inst var1 line

evalPatternStart :: Instance -> String -> String -> VariableValue -> Instance
evalPatternStart inst var1 var2 line
    | start == id = addVariable inst var1 line
    | otherwise = []
    where
        (_, id,    _) = getField line ":ID"
        (_, start, _) = getVarField inst var2 ":START_ID" 

evalPatternEnd :: Instance -> String -> String -> VariableValue -> Instance
evalPatternEnd inst var1 var2 line
    | end == id = addVariable inst var1 line
    | otherwise = []
    where
        (_, id,    _) = getField line ":ID"
        (_, end, _) = getVarField inst var2 ":END_ID" 
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

evalWhereEqual :: FieldEntry -> FieldEntry -> Bool
evalWhereEqual (_,"null",_) (_,"null",_) = False
evalWhereEqual (_,v1,_) (_,v2,_)         = v1 == v2

evalWhereNotEqual :: FieldEntry -> FieldEntry -> Bool
evalWhereNotEqual (_,"null",_) (_,"null",_) = False
evalWhereNotEqual f1 f2                     = not $ evalWhereEqual f1 f2

evalWhereLess :: FieldEntry -> FieldEntry -> Bool
evalWhereLess (_,"null",_) (_,"null",_)       = False
evalWhereLess (f1,v1,TypeInt) (f2,v2,TypeInt) = (read v1 :: Int) < (read v2)
evalWhereLess (f1,v1,t1) (f2,v2,t2)           = False

evalWhereGreater :: FieldEntry -> FieldEntry -> Bool
evalWhereGreater (_,"null",_) (_,"null",_)       = False
evalWhereGreater (f1,v1,TypeInt) (f2,v2,TypeInt) = (read v1 :: Int) > (read v2)
evalWhereGreater (f1,v1,t1) (f2,v2,t2)           = False

evalWhereLessOrEqual :: FieldEntry -> FieldEntry -> Bool
evalWhereLessOrEqual (_,"null",_) (_,"null",_)       = False
evalWhereLessOrEqual (f1,v1,TypeInt) (f2,v2,TypeInt) = (read v1 :: Int) <= (read v2)
evalWhereLessOrEqual (f1,v1,t1) (f2,v2,t2)           = False

evalWhereGreaterOrEqual :: FieldEntry -> FieldEntry -> Bool
evalWhereGreaterOrEqual (_,"null",_) (_,"null",_)       = False
evalWhereGreaterOrEqual (f1,v1,TypeInt) (f2,v2,TypeInt) = (read v1 :: Int) >= (read v2)
evalWhereGreaterOrEqual (f1,v1,t1) (f2,v2,t2)           = False

evalWhereStarts :: FieldEntry -> FieldEntry -> Bool
evalWhereStarts (_,"null",_) (_,"null",_)             = False
evalWhereStarts (f1,v1,TypeString) (f2,v2,TypeString) = startsWith v1 v2
evalWhereStarts (f1,v1,t1) (f2,v2,t2)                 = False

evalWhereEnds :: FieldEntry -> FieldEntry -> Bool 
evalWhereEnds (_,"null",_) (_,"null",_)             = False
evalWhereEnds (f1,v1,TypeString) (f2,v2,TypeString) = startsWith (reverse v1) (reverse v2)
evalWhereEnds (f1,v1,t1) (f2,v2,t2)                 = False


-- ASHES DISGUSTING HELPER FUNCTIONS 
getFieldVals :: [[FieldEntry]] -> String -> [String]
getFieldVals [] _ = [] 
getFieldVals (f:fs) str = getFieldVal f str : getFieldVals fs str 

-- -- filters values and produces a list of values 
getFieldVal :: [FieldEntry] -> String  -> String
getFieldVal [] _  = []
getFieldVal ((field,value,_):xs) str
    | str == field = value
    | otherwise = getFieldVal xs str



multiZipL :: [[a]] -> [[a]]
multiZipL = transpose


getHeader:: [[FieldEntry]] -> [[String]]
getHeader [] = [] 
getHeader (n:ns) = [getHeader'' result  result]
    where 
        result =  (nub ((getHeader' n) : (getHeader ns)))

getHeader' :: [FieldEntry] -> [String]
getHeader' [] = [] 
getHeader' ((str,val,t):fs) 
    | str == ":ID" = (str) : getHeader' fs 
    | str == ":LABEL" = (str) : getHeader' fs 
    | str == ":TYPE" = (str) : getHeader' fs 
    | otherwise = (str ++ showDataType t) : getHeader' fs 

getHeader'' :: [[String]] -> [[String]] -> [String]
getHeader'' [] (str1:xs) = str1  -- process the plus out of it 
getHeader'' (str:strlist) str1
    | allTrue (getHeader''' str) = str 
    | otherwise = getHeader'' strlist str1

getHeader''' :: [String] -> [Bool] 
getHeader''' [] = [] 
getHeader''' (x:xs) 
    | isInfixOf ":null" x = False : getHeader''' xs 
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
sameHeader ((h:rest):xs) = [result] ++ sameHeader notsameheader
    where 
        boollist = sameHeader'' h xs 
        mapped = (map snd $ filter fst $ zip boollist xs )
        mapped1 = removeHeader mapped 
        result = mergeNodes (h:rest) mapped1 
        notsameheader = map snd $ filter (not . fst) $ zip boollist xs


mergeNodes :: [[String]] -> [[[String]]] -> [[String]] 
mergeNodes ori nodes = ori ++ concat nodes 

sameHeader' :: [String] -> [[String]] -> Bool 
sameHeader' header list 
    | header `elem` list = True 
    | otherwise = False 

sameHeader'' :: [String] -> [[[String]]] -> [Bool]
sameHeader'' _ [] = [] 
sameHeader'' header (x:xs) = (sameHeader' header x ) : sameHeader'' header xs 
-------------------------------------------------------------------
-- Evaluating Print 
-------------------------------------------------------------------
-- evalPrint :: Environment -> Print -> InputData -> [[String]]
-- a
evalPrint :: Environment -> Print -> InputData -> [[String]]
evalPrint env (Print1 update delete return) (nodes,relations) = evalReturn (evalDelete (evalUpdate env update) delete) return
evalPrint env (Print2 update delete append) (nodes,relations) = evalAppend (nodes,relations) (evalDelete (evalUpdate env update) delete) append
evalPrint env (Print3 update return)        (nodes,relations) = evalReturn (evalUpdate env update) return
evalPrint env (Print4 update append)        (nodes,relations) = evalAppend (nodes,relations) (evalUpdate env update) append
evalPrint env (Print5 delete return)        (nodes,relations) = evalReturn (evalDelete env delete) return
evalPrint env (Print6 delete append)        (nodes,relations) = evalAppend (nodes,relations) (evalDelete env delete) append
evalPrint env (Print7 return)               (nodes,relations) = evalReturn env return
evalPrint env (Print8 append)               (nodes,relations) = evalAppend (nodes,relations) env append

-- evalAppend :: InputData -> Environment -> Return -> [[String]]
{--------------------------------------------------------------------------------------------------
------------------------------------------RETURN---------------------------------------------------
---------------------------------------------------------------------------------------------------}

-- -- checks if any two outputs have the same header, and if so merges them to be a part of the same output
-- evalOutputChecker :: [[[String]]] -> [[[String]]]
-- evalOutputChecker outlist = undefined


evalReturn1 :: Environment -> Return -> [[[String]]]
evalReturn1 env ret = (sameHeader result)
    where 
        result = (evalReturn' env ret)


evalReturn :: Environment -> Return -> [[String]]
evalReturn env ret = output
    where 
        result = sameHeader (evalReturn' env ret)
        headers = gethe result 
        nodewithouth = withouth result
        output = expectedHeader headers nodewithouth

expectedHeader:: [[String]] -> [[[String]]] -> [[String]]
expectedHeader [] [] = [] 
expectedHeader (h:hs) (n:ns) = (h :  n) ++ expectedHeader hs ns  


gethe :: [[[String]]] -> [[String]]
gethe [] = [] 
gethe ((h:rest):rs) = h : gethe rs 

withouth :: [[[String]]] -> [[[String]]]
withouth [] = [] 
withouth ((h:rest):rs) = rest : withouth rs

evalReturn' :: Environment -> Return -> [[[String]]]
evalReturn' env (Return []) = [] 
evalReturn' env (Return (out:outs)) = resultN ++ resultNR
    where 
        outN = printN out 
        outNR = printNR out 
        resultN = ((evalOutput env outN) : evalReturn1 env (Return outs))
        resultNR = evalNewRelation env outNR 


-- evalNewRelation' env (NewRelation varName1 fieldName1 varName2 fieldName2 typeName)


evalOutput :: Environment -> PrintExps -> [[String]]
evalOutput env out = concat (getHeader result ) : (resultval)
    where 
        outN = printN out 
        outNR = printNR out 
        result = evalOutputshelp env outN
        resultval = getEntryVal result 
   



printNR:: PrintExps -> [PrintExp]
printNR [] = [] 
printNR (p:ps) 
    | isNR p = printNR ps 
    | otherwise = p : printNR ps 
    where 
        isNR :: PrintExp -> Bool 
        isNR (Output _ _ _) = True 
        isNR _ = False 


printN:: PrintExps -> [PrintExp]
printN [] = [] 
printN (p:ps) 
    | isNR p = p : printN ps 
    | otherwise = printN ps 
    where 
        isNR :: PrintExp -> Bool 
        isNR (Output _ _ _) = True 
        isNR _ = False 

evalOutputshelp :: Environment -> PrintExps -> [[FieldEntry]]
evalOutputshelp env out = result 
    where 
        output = evalOutputs''' env out 
        result = multiZipL output 

evalOutputs' :: Instance -> PrintExp -> FieldEntry
evalOutputs' inst (Output varName fieldName asName) = val
    where
        val
            | varPresent inst varName = getVarField inst varName fieldName
            | otherwise = (fieldName,"null",TypeNull)

-- evalNewRelation' env (NewRelation varName1 fieldName1 varName2 fieldName2 typeName)

evalOutputs'' :: Environment -> PrintExp -> [FieldEntry]
evalOutputs'' env (Output varName fieldName asName) = map (\i -> evalOutputs' i (Output varName fieldName asName)) env   
-- evalOutputs'' env (NewRelation varName1 fieldName1 varName2 fieldName2 typeName) = evalNewRelation' env (NewRelation varName1 fieldName1 varName2 fieldName2 typeName)


evalOutputs''' :: Environment -> [PrintExp] -> [[FieldEntry]]
evalOutputs''' env  [] = []
evalOutputs''' env (out:os) = ((evalOutputs'' env out) : evalOutputs''' env os )



{--------------------------------------------------------------------------------------------------
------------------------------------------PRINT----------------------------------------------------
---------------------------------------------------------------------------------------------------}


-- evalPrint :: [[[String]]] -> IO()
-- evalPrint nodes = printTable result 
--     where 
--         result = evalPrint'' nodes 

evalPrint'' :: [[[String]]] -> [[String]]
evalPrint'' [] = [] 
evalPrint'' (node:nodes) = evalPrint' node : evalPrint'' nodes

evalPrint' :: [[String]] -> [String] 
evalPrint' node = concat node 


--printTable :: [[String]] -> [String]
printTable [] = [] 
printTable (str:strs) = rows 
    where
        rows = printRows (str:strs)


printRows :: [[String]] -> [String]
printRows [] = [] 
printRows (row:rows) = printLine row : printRows rows  


-- multiple string in header edge case needs to be fixed 
printLineStr :: [String] -> [String] -> Int -> String -> String
printLineStr line [] _ acc = acc 
printLineStr line (x:xs) nu acc
    | elemIndexInt x line 0 == nu = printLineStr line xs nu ( "\"" ++ x ++ "\"" ++ acc) 
    | otherwise = printLineStr line xs nu (acc) 

printLine :: [String] -> String
printLine = intercalate ", " 


elemIndexInt :: String -> [String] -> Int  -> Int
elemIndexInt _ [] nu = -1
elemIndexInt x (str:strs) nu
    | isInfixOf x str == True = nu -- : elemIndexInt x strs (nu+1)
    | otherwise = elemIndexInt x strs (nu+1)
    
elemIndexInt' :: String -> [String] -> Int  -> [Int]
elemIndexInt' _ [] nu = []
elemIndexInt' x (str:strs) nu
    | isInfixOf x str == True = [nu] ++ [elemIndexInt x strs (nu+1)]
    | otherwise = elemIndexInt' x strs (nu+1)

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

{--------------------------------------------------------------------------------------------------
------------------------------------------APPEND----------------------------------------------------
---------------------------------------------------------------------------------------------------}

evalAppend :: InputData -> Environment -> Append -> [[String]]
evalAppend input env (Append x) =  relationout 
    -- ((evalPrint' nodes) : evalPrint'' returnn) ++ ((evalPrint' relation) : evalPrint'' returnr)
    where 
        nodes = turnInputtoStringN input 
        relation = turnInputtoStringR input 
        returnnodes = evalReturn1 env (Return x) 
        returnn = isNodeN returnnodes
        nodeheader = gethe returnn 
        nodeNoH = withouth returnn 
        nodeout = expectedHeader nodeheader nodeNoH
        returnr = isNodeR returnnodes
        relationheader = gethe returnr
        relationNoH = withouth returnr 
        relationout = expectedHeader relationheader relationNoH
         
        



isNodeN :: [[[String]]] -> [[[String]]]
isNodeN [] = [] 
isNodeN (n:ns) 
    | isNodeN' n == [] = isNodeN ns 
    | otherwise = n : isNodeN ns 


isNodeN' :: [[String]] -> [[String]]
isNodeN' (x:xs) 
    | isNodeN'' x = (x:xs)
    | otherwise = [] 

isNodeN'' :: [String] -> Bool 
isNodeN'' [] = False
isNodeN'' (id:r) 
    | id == ":ID" = True 
    | otherwise = False 


isNodeR :: [[[String]]] -> [[[String]]]
isNodeR [] = [] 
isNodeR (n:ns) 
    | isNodeR' n == [] = isNodeR ns 
    | otherwise = n : isNodeR ns 


isNodeR' :: [[String]] -> [[String]]
isNodeR' (x:xs) 
    | isNodeR'' x = (x:xs)
    | otherwise = [] 

isNodeR'' :: [String] -> Bool 
isNodeR'' [] = False
isNodeR'' (id:r) 
    | id == ":ID" = True 
    | otherwise = False 




turnInputtoStringN :: InputData -> [[String]]
turnInputtoStringN (nodes, relation) = (headern ++ nVal) -- ++ (headerr ++ rVal)
    where 
        headern = getHeader nodes 
        headerr = getHeader relation 
        nVal = getEntryVal nodes 
        rVal = getEntryVal relation 


turnInputtoStringR :: InputData -> [[String]]
turnInputtoStringR (nodes, relation) = (headerr ++ rVal)
    where 
        headern = getHeader nodes 
        headerr = getHeader relation 
        nVal = getEntryVal nodes 
        rVal = getEntryVal relation 



{--------------------------------------------------------------------------------------------------
------------------------------------------NEWRELATIONSHIP----------------------------------------------------
---------------------------------------------------------------------------------------------------}


evalNewRelation:: Environment -> [PrintExp] -> [[[String]]]
evalNewRelation _ [] = [] 
evalNewRelation env (out:outs) = evalNewRelation' env out : evalNewRelation env outs 

--evalNewRelation' :: Environment -> Output -> [[String]]
evalNewRelation' :: [Instance] -> PrintExp -> [[String]]
evalNewRelation' env (NewRelation varName1 fieldName1 varName2 fieldName2 typeName) = newhead : vals
    where 
        val1 = getVars env varName1 
        val2 = getVars env varName2  
        fieldval = (getFields val1 fieldName1) 
        fieldval2 = getFields val2 fieldName2 
        typelist = replicate (length fieldval) (":TYPE",typeName,TypeString)
        relation = multiZipL [fieldval, fieldval2, typelist]
        headerrelation = getHeader relation 
        newhead = checkID (concat headerrelation) 0 
        vals = getEntryVal relation



--evalNewRelationt' :: [Instance] -> PrintExp -> [[String]]
evalNewRelationt' env (NewRelation varName1 fieldName1 varName2 fieldName2 typeName) = val2 
    --newhead : vals
    where 
        val1 = getVars env varName1 
        val2 = getVars env varName2  
        fieldval = (getFields val1 fieldName1) 
        fieldval2 = getFields val2 fieldName2 
        typelist = replicate (length fieldval) (":TYPE",typeName,TypeString)
        relation = multiZipL [fieldval, fieldval2, typelist]
        headerrelation = getHeader relation 
        newhead = checkID (concat headerrelation) 0 
        vals = getEntryVal relation
 

x [] _ _ = []
x (field@(fName,fValue,fType):fields) name val
    | fName == name = (fName,(show $ (read fValue) + val),fType) : x fields name val 
    | otherwise = field : x fields name val 


checkID :: [String] -> Int -> [String]
checkID [] _ = []
checkID (x:xs) nu 
    | x == ":ID" && nu == 0 = ":START_ID" : checkID xs (nu+1)
    | x == ":ID" && nu == 1 = ":END_ID" : checkID xs (nu+1)
    | otherwise =  x : checkID xs nu 


{--------------------------------------------------------------------------------------------------
------------------------------------------UPDATE----------------------------------------------------
---------------------------------------------------------------------------------------------------}


evalUpdate :: Environment -> Update -> Environment
evalUpdate env (Update uplist) = evalUpdate'' env uplist 

evalUpdate'' :: Environment -> [UpdateExp] -> Environment
evalUpdate'' env [] = env 
evalUpdate'' env (u:us) = evalUpdate'' newenv us 
    where
        newenv =  evalUpdate' env u 

--evalUpdate :: Environment -> Update -> [[String]]
evalUpdate' :: [Instance] -> UpdateExp -> Environment
evalUpdate' env (UAdd varName fieldName valueToAdd storeVarName storeFieldName) = upf
    where 
        valVarName = getVars env varName 
        valVarField = (getFields valVarName fieldName) 
        updatedvalField = updateValFieldInt valVarField valueToAdd 
        upf = updateFields env storeVarName storeFieldName updatedvalField []
evalUpdate' env (UAddDot varName fieldName varName2 fieldName2 storeVarName storeFieldName) = reverseList updatedF
    where 
        valVarName = (getVars env varName)
        valVarField = (getFields valVarName fieldName)
        val2VarName = (getVars env varName2) 
        valVarField2 = (getFields val2VarName fieldName2)
        updatedintval = updateAddHelp valVarField valVarField2 storeFieldName
        updatedF = updateFields env storeVarName storeFieldName updatedintval [] 





updateAddHelp :: [FieldEntry] -> [FieldEntry] -> String-> [FieldEntry]
updateAddHelp [] _ _ = [] 
updateAddHelp _ [] _ = [] 
updateAddHelp ((varName, val, types):rest ) ((varName2, val2, tpes2):rest2) fieldNameNew 
    | extractValue (stringToInt val)  == -1 && extractValue (stringToInt val2)== -1 = (fieldNameNew,"null",TypeInt) : updateAddHelp rest rest2 fieldNameNew
    | extractValue (stringToInt val) == -1 = (fieldNameNew,val2,TypeInt) : updateAddHelp rest rest2 fieldNameNew 
    | extractValue (stringToInt val2) == -1 = (fieldNameNew,val,TypeInt) : updateAddHelp rest rest2 fieldNameNew 
    | otherwise = (fieldNameNew,(intToString (extractValue  (stringToInt val) + extractValue (stringToInt val2))),TypeInt) : updateAddHelp rest rest2 fieldNameNew





updateFields :: Environment -> String -> String -> [FieldEntry] -> Environment -> Environment
updateFields [] _ _ _ acc = acc
updateFields (inst:insts) varName fieldName fe acc = updateFields insts varName fieldName newfeildentry (currentinst :acc )
    where 
        ins :: Instance -> String -> String -> [FieldEntry] -> Instance 
        ins [] _ _ _ = [] 
        ins inst _ _ [] = inst 
        ins ((s,fl):vs) varName fieldName  (fe:fes)
            | s == varName = (s, updateAtIndex (elemIndexI (getField fl fieldName) fl 0 ) fe fl ) : ins vs varName fieldName fes 
            | otherwise = (s,fl) : ins vs varName fieldName (fe:fes)
              
        fel :: Instance -> String -> String -> [FieldEntry] -> [Int]
        fel [] _ _ _ = [] 
        fel _ _ _ [] = [] 
        fel ((s,fl):vs) varName fieldName  (fe:fes)
            | s == varName = 1 : fel vs varName fieldName fes  
            | otherwise = fel vs varName fieldName (fe:fes)
              

        currentinst = ins inst varName fieldName fe 
        newfeildentry = drop (length (fel inst varName fieldName fe)) fe 


elemIndexI :: (String,String,DataType) -> [FieldEntry] -> Int  -> Int
elemIndexI _ [] nu = -1
elemIndexI x (str:strs) nu
    | x == str = nu -- : elemIndexInt x strs (nu+1)
    | otherwise = elemIndexI x strs (nu+1)


updateAtIndex :: Int -> a -> [a] -> [a]
updateAtIndex (-1) _ a = a  
updateAtIndex _ _ [] = []
updateAtIndex i newVal (x:xs)
    | i == 0    = newVal : xs    -- Replace element at index 0
    | otherwise = x : updateAtIndex (i - 1) newVal xs


updateValFieldInt:: [FieldEntry] -> Int -> [FieldEntry]
updateValFieldInt [] _ = []
updateValFieldInt ((s,v,t):fs) numVal = (s,strint,t) : updateValFieldInt fs numVal
    where 
        intt = extractValue (stringToInt(v))
        int2 = intt + numVal 
        strint 
            | intt == -1 = "null"
            | otherwise = intToString int2 


stringToInt :: String -> Maybe Int
stringToInt str 
    | str == "null" = Just (-1) 
    | otherwise = readMaybe str 


intToString :: Int -> String
intToString = show 

extractValue :: Maybe a -> a
extractValue (Just x) = x
extractValue Nothing  = error "Cannot extract value from Nothing"


{--------------------------------------------------------------------------------------------------
------------------------------------------DELETE----------------------------------------------------
---------------------------------------------------------------------------------------------------}



evalDelete :: Environment -> Delete -> Environment
evalDelete env (Delete dlist) = evalDelete'' env dlist 

evalDelete'' :: Environment -> [DeleteExp] -> Environment
evalDelete'' env [] = env  
evalDelete'' env (d:ds) = evalDelete'' newenv ds 
    where 
        newenv = (evalDelete' env d)

evalDelete' :: Environment -> DeleteExp -> Environment
evalDelete' env (Del varName fieldName deleteType) = reverseList updatedNodes
    where 
        varvalName = getVars env varName 
        varFieldVal = getFields varvalName fieldName
        updatedNodes = filter (not . null ) (updateFieldsD env varName fieldName deleteType varFieldVal [])



updateFieldsD :: Environment -> String -> String -> String -> [FieldEntry] -> Environment -> Environment
updateFieldsD [] _ _ _ _ acc = acc
updateFieldsD (inst:insts) varName fieldName valD fe acc = updateFieldsD insts varName fieldName valD fe (currentinst :acc )
    where 
        ins :: Instance -> String -> String -> String -> [FieldEntry] -> Instance 
        ins [] _ _ _ _ = [] 
        ins inst _ _ _ [] = inst 
        ins ((s,fl):vs) varName fieldName  valD (fe:fes)
            | s == varName && getValues (getField fl fieldName) == valD = ins vs varName fieldName valD (fe:fes) 
            | otherwise = (s,fl) : ins vs varName fieldName valD (fe:fes)
              
        fel :: Instance -> String -> String-> String -> [FieldEntry] -> [Int]
        fel [] _ _ _ _ = [] 
        fel _ _ _ _ [] = [] 
        fel ((s,fl):vs) varName fieldName valD (fe:fes)
            | s == varName && getValues (getField fl fieldName) == valD = 1 : fel vs varName fieldName valD (fe:fes)
            | s == varName = 1 : fel vs varName fieldName valD fes  
            | otherwise = fel vs varName fieldName valD (fe:fes)
              

        currentinst = filter (not. null) (ins inst varName fieldName valD fe) 
        newfeildentry = drop (length (fel inst varName fieldName valD fe)) fe 

