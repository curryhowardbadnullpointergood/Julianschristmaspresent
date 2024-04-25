module GqlEvaluator where
import LangParser
import LangLexer
import InputLexer
import InputParser

import Data.List (nub, elemIndex, transpose,groupBy, sort)
import Data.Function (on)

data VariableValue
    = TypeFile File
    | TypeNodes Nodes
    | TypeRelations Relations
    deriving (Show, Eq)

type Variables = [(String, VariableValue)]
type Variable = (String, VariableValue)

type Nodes = [(NodeHeader, NodeEntry)]
type Relations = [(RelationshipHeader, RelationshipEntry)]



---------------------------------------------------------------------------------------------------
-- Helper Functions
---------------------------------------------------------------------------------------------------
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
-- evalMatch :: Variables -> Match -> File -> File
-- evalMatch vars (Match patterns return) file = output
--     where
--         matchVars = evalPatterns vars patterns file
--         output = evalReturn matchVars return
evalMatch vars (Match patterns whereConditions return) file = output
    where
        matchVars = evalPatterns vars patterns file 
        whereVars = evalWhereConditions matchVars whereConditions
        output = evalReturn whereVars return
---------------------------------------------------------------------------------------------------
-- Evaluating Patterns 
---------------------------------------------------------------------------------------------------
evalPatterns :: Variables -> Patterns -> File -> Variables
evalPatterns vars [] _ = vars
evalPatterns vars (pattern:patterns) file = evalPatterns vars' patterns file
    where
        vars' = evalPattern vars pattern file
---------------------------------------------------------------------------------------------------
-- Evaluating Pattern 
---------------------------------------------------------------------------------------------------
evalPattern :: Variables -> Pattern -> File -> Variables
evalPattern vars (PatternFinal str1) datafile = addVariable vars str1 (TypeNodes (getNodes datafile))
evalPattern vars (PatternRelatedTo str1 str2) datafile = output
    where
        vars'   = addVariable vars      str1 (TypeNodes (getNodesfromString (getStartId (getRelationships datafile)) (getNodes datafile) []))
        vars''  = addVariable vars'     str2 (TypeNodes (getNodesfromString (getEndId (getRelationships datafile)) (getNodes datafile) []))
        output  = reverseList vars''
evalPattern vars (PatternRelatedToVar str1 str2 str3) datafile = output
    where
        vars'   = addVariable vars      str1 (TypeNodes (getNodesfromString (getStartId (getRelationships datafile)) (getNodes datafile) []))
        vars''  = addVariable vars'     str3 (TypeNodes (getNodesfromString (getEndId (getRelationships datafile)) (getNodes datafile) []))
        vars''' = addVariable vars''    str2 (TypeRelations (getRelationshipsfromString (getStartId (getRelationships datafile)) (getRelationships datafile) [] ))
        output  = reverseList vars'''
evalPattern vars (PatternRelatedBy str1 str2) datafile
    = reverseList (evalPattern vars (PatternRelatedTo str2 str1) datafile)
evalPattern vars (PatternRelatedByVar str1 str2 str3) datafile = output
    where
        x       = rearrange (evalPattern vars (PatternRelatedToVar str3 str2 str1) datafile)
        output  = x
evalPattern vars (PatternRelated str1 str2) datafile = output
    where
        vals    = nub (getStartId (getRelationships datafile) ++ getEndId (getRelationships datafile))
        vars'   = addVariable vars  str1 (TypeNodes ((getNodesfromString (vals) (getNodes datafile) [])))
        vars''  = addVariable vars' str2 (TypeNodes ((getNodesfromString (vals) (getNodes datafile) [])))
        output  = reverseList vars''

rearrange :: [a] -> [a]
rearrange  (x:s:y) = (s:x:y)

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



---------------------------------------------------------------------------------------------------
-- Evaluating WhereConditions
---------------------------------------------------------------------------------------------------
evalWhereConditions :: Variables -> WhereExp -> Variables
evalWhereConditions vars (WOr whereCondition whereConditions) = unionVars (evalWhereCondition vars whereCondition) (evalWhereConditions vars whereConditions) 
-- evalWhereConditions vars (WhereConditionAnd whereCondition whereConditions) = evalWhereConditions (evalWhereCondition vars whereCondition) whereConditions
-- evalWhereConditions vars (WhereConditionNot whereConditions) = complementVars (evalWhereConditions vars whereConditions) vars
evalWhereConditions vars (WFinal whereCondition) = evalWhereCondition vars whereCondition

unionVars :: Variables -> Variables -> Variables
unionVars [] _ = []
unionVars ((var1Name, (TypeNodes var1Nodes)):vars1) vars2 = outputVar : unionVars vars1 vars2
    where
        var2Value = getVarValueFromName vars2 var1Name
        outputVar = (var1Name, TypeNodes $ unionLists var1Nodes (extractVariableNodes var2Value))
unionVars ((var1Name, (TypeRelations var1Relations)):vars1) vars2 = outputVar : unionVars vars1 vars2
    where
        var2Value = getVarValueFromName vars2 var1Name
        outputVar = (var1Name, TypeRelations $ unionLists var1Relations (extractVariableRelations var2Value))

complementVars :: Variables -> Variables -> Variables
complementVars [] _ = []
complementVars ((var1Name, (TypeNodes var1Nodes)):vars1) vars2 = outputVar : complementVars vars1 vars2
    where
        var2Value = getVarValueFromName vars2 var1Name
        outputVar = (var1Name, TypeNodes $ complementLists var1Nodes (extractVariableNodes var2Value))
complementVars ((var1Name, (TypeRelations var1Relations)):vars1) vars2 = outputVar : complementVars vars1 vars2
    where
        var2Value = getVarValueFromName vars2 var1Name
        outputVar = (var1Name, TypeRelations $ complementLists var1Relations (extractVariableRelations var2Value))

-- evalWhereCondition vars cond@(IntWhereCondition varName field intCondition) = vars'
--     where
--         varValue = getVarValueFromName vars varName
--         updatedVarValue = coordinator varValue cond
--         vars' = updateVariable vars varName updatedVarValue

-- filterIntField :: VariableValue -> WhereCondition -> Variables
-- filterIntField (TypeNodes nodes) (IntWhereCondition varName field intCondition) = 
--     TypeNodes $ filterIntNodes nodes field intCondition
-- filterIntField (TypeRelations relations) (IntWhereCondition varName field intCondition) = 
--     TypeRelations $ 


-- evalWhereCondition :: Variables -> WhereCondition -> Variables
-- evalWhereCondition vars (IntWhereCondition varName field intCondition) = []

-- evalWhereCondition vars (StrWhereCondition varName field strCondition) = []
-- evalWhereCondition vars (BoolWhereCondition varName field boolCondition) = []
-- evalWhereCondition vars (LabelWhereCondition varName strCondition) = []
-- evalWhereCondition vars (TypeWhereCondition varName strCondition) = []
---------------------------------------------------------------------------------------------------
-- Evaluating Conditions
---------------------------------------------------------------------------------------------------
-- evalIntCondition :: IntCondition -> Literal -> Bool
-- evalIntCondition (Greater int2)        (LiteralInt int1)   = int1 > int2
-- evalIntCondition (Less int2)           (LiteralInt int1)   = int1 < int2
-- evalIntCondition (GreaterOrEqual int2) (LiteralInt int1)   = int1 >= int2
-- evalIntCondition (LessOrEqual int2)    (LiteralInt int1)   = int1 <= int2
-- evalIntCondition (IntEqual int2)       (LiteralInt int1)   = int1 == int2
-- evalIntCondition (IntNotEqual int2)    (LiteralInt int1)   = int1 /= int2
-- evalIntCondition IntIsNull             LiteralNull         = True
-- evalIntCondition IntNotNull            LiteralNull         = False
-- evalIntCondition _                     LiteralNull         = False

-- evalStrCondition :: StrCondition -> Literal -> Bool
-- evalStrCondition (StringStarts string2)    (LiteralStr string1)    = startsWith string1 string2
-- evalStrCondition (StrEqual string2)        (LiteralStr string1)    = string1 == string2
-- evalStrCondition (StrNotEqual string2)     (LiteralStr string1)    = string1 == string2 
-- evalStrCondition StrIsNull                 LiteralNull             = True
-- evalStrCondition StrNotNull                LiteralNull             = False
-- evalStrCondition _                         LiteralNull             = False

-- evalBoolCondition :: BoolCondition -> Literal -> Bool
-- evalBoolCondition (BoolEqual bool2)    (LiteralBool bool1) = bool1 == bool2
-- evalBoolCondition (BoolNotEqual bool2) (LiteralBool bool1) = bool1 /= bool2
-- evalBoolCondition BoolIsNull           LiteralNull         = True
-- evalBoolCondition BoolNotNull          LiteralNull         = False
-- evalBoolCondition _ LiteralNull                            = False

-- startsWith :: Eq a => [a] -> [a] -> Bool
-- startsWith [] _ = True
-- startsWith _  [] = False
-- startsWith (x:xs) (y:ys) = x == y && startsWith xs ys

-- endsWith :: Eq a => [a] -> [a] -> Bool
-- endsWith xs ys = startsWith (reverse xs) (reverse ys)
---------------------------------------------------------------------------------------------------
-- Evaluating Return
---------------------------------------------------------------------------------------------------
-- evalReturn :: Variables -> Return -> File
-- evalReturn vars (ReturnNode (outputs:outputss) b) = File [] []
-- evalReturn vars (ReturnNodeRelation (nodeoutput:nodeouts) (relationoutput: relaouts)) = File [] []
-- evalReturn vars (ReturnRelation (outputs:outputss)) = File [] []
---------------------------------------------------------------------------------------------------
-- Evaluating Outputs
---------------------------------------------------------------------------------------------------
evalOutputsN' :: Variables -> Outputs -> [(String,[Literal])]-> [(String,[Literal])]
evalOutputsN' vars [] acc = acc
evalOutputsN' vars (output:outputs) acc = evalOutputsN' vars outputs (evalOutputN vars output:acc)


evalOutputsR' :: Variables -> Outputs -> [(String,[Literal])]-> [(String,[Literal])]
evalOutputsR' vars [] acc = acc
evalOutputsR' vars (output:outputs) acc = evalOutputsR' vars outputs (evalOutputR vars output:acc)


evalGetVarName:: Outputs -> [String] -> [String]
evalGetVarName (x:xs) acc = evalGetVarName xs (evalGetVarName' x :acc)


evalGetFieldsV:: Outputs -> [String] -> [String]
evalGetFieldsV (x:xs) acc = evalGetFieldsV xs (evalGetFieldsV' x :acc)



combineLists :: [a] -> [b] -> [[(a, b)]]
combineLists xs ys = map (\x -> map (\y -> (x, y)) ys) xs


-- getLitFVN :: [(a,b)] -> Variables -> [(String,[Literal])]
-- getLitFVN (x:xs) vars = ((fst x) , (getValN val) fieldName)
--     where 
--         let val = getVarValueFromName vars (fst x) 



-- getTypeFromFN :: String -> Outputs -> FieldType
-- getTypeFromFN fieldName out =  

evalGetVarName' :: Output -> String 
evalGetVarName' (StrOutput varName fieldName asName) = varName 
evalGetVarName' (IntOutput varName fieldName asName) = varName 
evalGetVarName' (BoolOutput varName fieldName asName) = varName 


evalGetFieldsV' :: Output -> String 
evalGetFieldsV' (StrOutput varName fieldName asName) = fieldName 
evalGetFieldsV' (IntOutput varName fieldName asName) = fieldName 
evalGetFieldsV' (BoolOutput varName fieldName asName) = fieldName 


-- for label 
--evalOutputN' :: Variables -> Outputs -> [([NodeEntry])]
--evalOutputN' :: Variables -> Outputs -> Bool -> [NodeSet]
--evalOutputN' :: Variables -> Outputs -> Bool -> NodeSet 
evalOutputN' :: Variables -> Outputs -> Bool -> NodeSet
evalOutputN' vars out bool = concatNodeS output 
    where 
        evalOut = evalOutputsN' vars out [] 
        multi = multiOutputN evalOut []  
        multiOutNhelper = multiOutputNhelper multi 
        info 
            | bool == True =  nodes
            | otherwise = [ removeLabels n | n <- nodes]
                where
                    nodes =  getInfoN vars multiOutNhelper []

        h = [ getVName' jj | jj <- multiOutNhelper]
        header 
            | bool == True =  reverseList [ (NodeHeader (reverseList (getFields out gg)) bool)| gg <- h]
            | otherwise = reverseList [ (NodeHeader (reverseList (getFields out gg)) bool) | gg <- h]
        g = zip header info 
        ggg = [  NodeSet nH nEl | (nH, nEl) <- g]
        output = ggg


concatNodeS :: [NodeSet] -> NodeSet 
concatNodeS [x] = x 
concatNodeS _ = NodeSet (NodeHeader [] True) [(NodeEntry "" [] [])] 

concatNodeR :: [RelationshipSet] -> RelationshipSet 
concatNodeR [x] = x 
concatNodeR _ = RelationshipSet (RelationshipHeader []) [(RelationshipEntry "" [] "" "")]
evalOutputR' vars out = concatNodeR output 
    where 
        evalOut = evalOutputsR' vars out [] 
        multi = multiOutputN evalOut []  
        multiOutNhelper = multiOutputNhelper multi 
        info = getInfoR vars multiOutNhelper []
        h = [ getVName' jj | jj <- multiOutNhelper]
        header = reverseList [ (RelationshipHeader (reverseList (getFields out gg)) )| gg <- h]
        g = zip header info 
        ggg = [  RelationshipSet rH rEl | (rH, rEl) <- g]
        output = ggg


outputCoord vars (ReturnNode outputs ) = File ([evalOutputN' vars outputsi True | outputsi <- outputs]) []
outputCoord vars (ReturnRelation outputs ) = File [] [(evalOutputR' vars o) | o <- outputs]
outputCoord vars (ReturnNodeRelation outputs1 outputs2 ) = File [(evalOutputN' vars o1 True) | o1 <- outputs1] [(evalOutputR' vars (o2)) | o2 <- outputs2]




removeLabels :: [NodeEntry] -> [NodeEntry]
removeLabels [] = []
removeLabels ((NodeEntry id literals _):entries) = (NodeEntry id literals []) : removeLabels entries



getInfoN :: Variables -> [(String,[[Literal]])] -> [[NodeEntry]] -> [[NodeEntry]]
getInfoN vars [] acc = acc 
getInfoN vars ((str, lit):rest) acc = getInfoN vars rest (generateNodeEntries (getNodeId (getValN (getVarValueFromName vars str)) []) (lit) (getNodeLabel (getValN (getVarValueFromName vars str)) []) [] :acc ) 


getInfoR :: Variables -> [(String,[[Literal]])] -> [[RelationshipEntry]] -> [[RelationshipEntry]]
getInfoR vars [] acc = acc 
getInfoR vars ((str, lit):rest) acc = getInfoR vars rest (generateRelationshipsEntries (getRelationshipStartId (getValR (getVarValueFromName vars str)) []) (lit) (getRelationshipEndId (getValR (getVarValueFromName vars str)) []) (getRelationshipTypeId (getValR (getVarValueFromName vars str)) []) [] :acc ) 
-- generateNodeEntries :: [String] -> [[Literal]] -> [Labels] -> [NodeEntry]-> [NodeEntry]

multiOutputN :: [(String,[Literal])] -> [[(String,[Literal])]] -> [[(String,[Literal])]]
multiOutputN [] acc = acc
multiOutputN list acc = nub [ filter (\(s,l) -> s == str) list | (str,lit) <- list]


multiOutputNhelper :: [[(String,[Literal])]] -> [(String,[[Literal]])]
multiOutputNhelper litlist = [ (getVName lit , multiZipL (multihelper lit [])) | lit <- litlist ]
    --  multiOutputN1 xs ((filter (\(str,_) -> str == s) xs):acc)

multihelper :: [(String,[Literal])] -> [[Literal]] -> [[Literal]]
multihelper [] acc =  reverseList acc 
multihelper ((str, lit):xs) acc = multihelper xs (lit:acc)

getVName:: [(String,b)] -> String 
getVName ((str,lit):xs) = str 

getVName':: (String,[[Literal]]) -> String 
getVName' (str,lit) = str 

--getVHeader :: Variables -> String -> Outputs -> NodeHeader
getVHeaderN vars varName outs = output 
    where 
        nodes = getValN (getVarValueFromName vars varName)
        startid = getNodeId nodes 
        labelid = getNodeLabel nodes 
        output = [] 

-- evalOutputsN :: Variables -> Outputs -> [NodeEntry] 
-- evalOutputsN vars output = output 
--     where 
--         literals = evalOutputsN'
--         startid = 
--         output = literals

getFields :: Outputs -> String -> [Field]
getFields outlist varName = [ whatTypeLit out | out <- outlist ,getNameO out == varName]



getNameO :: Output -> String 
getNameO (BoolOutput varName _ _) = varName  
getNameO (IntOutput varName _ _) = varName 
getNameO (StrOutput varName _ _) = varName 

whatTypeLit (BoolOutput _ _ s) = Field s TypeBoolean
whatTypeLit (StrOutput _ _ s) = Field s TypeString 
whatTypeLit (IntOutput _ _ s) = Field s TypeInteger  


-- evalOutputsN :: Variables -> Outputs -> [NodeEntry]
-- evalOutputsN vars output =  (evalOutputs vars output)

-- generateNodeEntries :: [String] -> [[Literal]] -> [Labels] -> [[NodeEntries]]
-- generateNodeEntries strlist litlist lablist = [ | str <- strlist, lit <- litlist, lab <- lablist]

generateNodeEntries :: [String] -> [[Literal]] -> [Labels] -> [NodeEntry]-> [NodeEntry]
generateNodeEntries [] [] [] acc = acc
generateNodeEntries (strlist:sxs) (litlist:lxs) (lablist:laxs) acc = generateNodeEntries sxs lxs laxs ((generateNodeEntry strlist litlist lablist):acc)

generateNodeEntry :: String -> Literals -> Labels -> NodeEntry
generateNodeEntry str lits labs = NodeEntry str lits labs


generateRelationshipsEntries :: [String] -> [[Literal]] -> [String] -> [String] -> [RelationshipEntry]-> [RelationshipEntry]
generateRelationshipsEntries [] [] [] [] acc = acc
generateRelationshipsEntries (startidlist:sxs) (litlist:lxs) (endidlist:laxs) (typelist:exs) acc = generateRelationshipsEntries sxs lxs laxs exs ((generateRelationshipsEntry startidlist litlist endidlist typelist):acc)

generateRelationshipsEntry :: String -> Literals-> String -> String -> RelationshipEntry
generateRelationshipsEntry startid lits endid types = RelationshipEntry startid lits endid types
---------------------------------------------------------------------------------------------------
-- Evaluating Output
---------------------------------------------------------------------------------------------------
evalOutputN :: Variables ->  Output -> (String,[Literal])
evalOutputN  vars (StrOutput varName fieldName asName) = (varName,getNodeLiteralsStr (getValN val) fieldName)
    -- | otherwise = getRelationLiteralsString (getValR val) fieldName 
    -- | otherwise = [] 
        where
            val = (getVarValueFromName vars varName)
            nodes = (getValN val)


evalOutputN  vars (IntOutput varName fieldName asName) = (varName,getNodeLiteralsInt (getValN val) fieldName)
    -- | otherwise = []
    -- | otherwise = getRelationLiteralsInt (getValR val) fieldName 
        where
            val = (getVarValueFromName vars varName)

evalOutputN vars (BoolOutput varName fieldName asName) = (varName,getNodeLiteralsBool (getValN val) fieldName)
    -- | otherwise = []
    -- | otherwise = getRelationLiteralsBool  (getValR val) fieldName 
        where
            val = (getVarValueFromName vars varName)


-- evalOutputN  vars (IdOutput varName) = []
-- evalOutputN vars (StartOutput varName) = []
-- evalOutputN  vars (EndOutput varName) = []
evalOutputN  vars (LabelOutput varName) = ("",[])



evalOutputR :: Variables ->  Output -> (String,[Literal])
evalOutputR  vars (StrOutput varName fieldName asName) =  (varName,getRelationLiteralsString relations fieldName)
    -- | otherwise = getRelationLiteralsString (getValR val) fieldName 
    -- | otherwise = [] 
        where
            val = (getVarValueFromName vars varName)
            relations = (getValR val)


evalOutputR  vars (IntOutput varName fieldName asName) = (varName,getRelationLiteralsInt relations fieldName )
    -- | otherwise = []
    -- | otherwise = getRelationLiteralsInt (getValR val) fieldName 
        where
            val = (getVarValueFromName vars varName)
            relations = (getValR val)

evalOutputR vars (BoolOutput varName fieldName asName) = (varName,getRelationLiteralsBool  relations fieldName )
    -- | otherwise = []
    -- | otherwise = getRelationLiteralsBool  (getValR val) fieldName 
        where
            val = (getVarValueFromName vars varName)
            relations = (getValR val)


-- evalOutputR  vars (IdOutput varName) = []
-- evalOutputR vars (StartOutput varName) = []
-- evalOutputR  vars (EndOutput varName) = []
evalOutputR  vars (LabelOutput varName) = ("",[])

isTypeNodes :: VariableValue -> Bool
isTypeNodes val = case val of 
    TypeNodes nodes -> True 
    TypeRelations relations -> False 

getValN :: VariableValue -> Nodes
getValN val = case val of
    TypeNodes nodes -> nodes
    TypeRelations relations -> []

getValR :: VariableValue -> Relations
getValR val = case val of
    TypeNodes nodes -> []
    TypeRelations relations -> relations

getNodeId :: Nodes -> [String] -> [String]
getNodeId [] acc = reverseList acc
getNodeId ((nH,NodeEntry str _ _): rest) acc = getNodeId rest (str:acc)

getNodeLabel :: Nodes -> [Labels] -> [Labels]
getNodeLabel [] acc = reverseList acc
getNodeLabel ((nH,NodeEntry _ _ label): rest) acc = getNodeLabel rest (label:acc)

getRelationshipStartId :: Relations -> [String] -> [String]
getRelationshipStartId [] acc = reverseList acc
getRelationshipStartId ((rH, RelationshipEntry startid _ _ _): rest) acc = getRelationshipStartId  rest (startid:acc)

getRelationshipEndId :: Relations -> [String] -> [String]
getRelationshipEndId [] acc = reverseList acc
getRelationshipEndId ((rH, RelationshipEntry _ _ endid _): rest) acc = getRelationshipEndId  rest (endid:acc)

getRelationshipTypeId :: Relations -> [String] -> [String]
getRelationshipTypeId [] acc = reverseList acc
getRelationshipTypeId ((rH, RelationshipEntry _ _ _ typeid): rest) acc = getRelationshipTypeId  rest (typeid:acc)

---------------------------------------------------------------------------------------------------
-- Getting fileName
---------------------------------------------------------------------------------------------------
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

-- evalWhereConditions :: Variables -> WhereConditions -> Variables
-- evalWhereConditions vars (WhereConditionOr whereCondition whereConditions) = unionVars (evalWhereCondition vars whereCondition) (evalWhereConditions vars whereConditions) 
-- evalWhereConditions vars (WhereConditionAnd whereCondition whereConditions) = evalWhereConditions (evalWhereCondition vars whereCondition) whereConditions
-- evalWhereConditions vars (WhereConditionNot whereConditions) = complementVars (evalWhereConditions vars whereConditions) vars
-- evalWhereConditions vars (WhereCondition whereCondition) = evalWhereCondition vars whereCondition



-- evalWhere :: Variables -> Where -> Variables
-- evalWhere vars (Where whereExp) = evalWhereExp vars  whereExp 

-- evalWhereExp :: Variables -> WhereExp -> Variables
-- evalWhereExp vars (WAnd whereExp1 whereExp2) = evalWhereExp (evalWhereExp vars whereExp1) whereExp2 
-- evalWhereExp vars (WOr whereExp1 whereExp2) = unionVars (evalWhereExp vars whereExp1) (evalWhereExp vars whereExp2) 
-- evalWhereExp vars (WNot whereExp1) = complementVars (evalWhereExp vars whereExp1) vars
-- evalWhereExp vars (WEqual (WDot ) whereExp2) =
-- evalWhereExp vars (WNotEqual whereExp1 whereExp2) =
-- evalWhereExp vars (WLessThan whereExp1 whereExp2) =
-- evalWhereExp vars (WGreaterThan whereExp1 whereExp2) =
-- evalWhereExp vars (WLessOrEqualThan whereExp1 whereExp2) =
-- evalWhereExp vars (WGreaterOrEqualThan whereExp1 whereExp2) =
-- evalWhereExp vars (WStartsWith whereExp1 whereExp2) =
-- evalWhereExp _ _ = error ("invalid where expression")

-- unionVars :: Variables -> Variables -> Variables
-- unionVars [] _ = []
-- unionVars ((var1Name, (TypeNodes var1Nodes)):vars1) vars2 = outputVar : unionVars vars1 vars2 
--     where 
--         var2Value = getVarValueFromName vars2 var1Name
--         outputVar = (var1Name, TypeNodes $ unionLists var1Nodes (extractVariableNodes var2Value))
-- unionVars ((var1Name, (TypeRelations var1Relations)):vars1) vars2 = outputVar : unionVars vars1 vars2 
--     where 
--         var2Value = getVarValueFromName vars2 var1Name
--         outputVar = (var1Name, TypeRelations $ unionLists var1Relations (extractVariableRelations var2Value))   

-- complementVars :: Variables -> Variables -> Variables
-- complementVars [] _ = []
-- complementVars ((var1Name, (TypeNodes var1Nodes)):vars1) vars2 = outputVar : complementVars vars1 vars2 
--     where 
--         var2Value = getVarValueFromName vars2 var1Name
--         outputVar = (var1Name, TypeNodes $ complementLists var1Nodes (extractVariableNodes var2Value))   
-- complementVars ((var1Name, (TypeRelations var1Relations)):vars1) vars2 = outputVar : complementVars vars1 vars2 
--     where 
--         var2Value = getVarValueFromName vars2 var1Name
--         outputVar = (var1Name, TypeRelations $ complementLists var1Relations (extractVariableRelations var2Value))  
-- evalWhereConditions :: Variables -> WhereConditions -> Variables
-- evalWhereConditions vars (WhereConditionOr whereCondition whereConditions) = unionVars (evalWhereCondition vars whereCondition) (evalWhereConditions vars whereConditions) 
-- evalWhereConditions vars (WhereConditionAnd whereCondition whereConditions) = evalWhereConditions (evalWhereCondition vars whereCondition) whereConditions
-- evalWhereConditions vars (WhereConditionNot whereConditions) = complementVars (evalWhereConditions vars whereConditions) vars
-- evalWhereConditions vars (WhereCondition whereCondition) = evalWhereCondition vars whereCondition



-- evalWhere :: Variables -> Where -> Variables
-- evalWhere vars (Where whereExp) = evalWhereExp vars  whereExp 

-- evalWhereExp :: Variables -> WhereExp -> Variables
-- evalWhereExp vars (WAnd whereExp1 whereExp2) = evalWhereExp (evalWhereExp vars whereExp1) whereExp2 
-- evalWhereExp vars (WOr whereExp1 whereExp2) = unionVars (evalWhereExp vars whereExp1) (evalWhereExp vars whereExp2) 
-- evalWhereExp vars (WNot whereExp1) = complementVars (evalWhereExp vars whereExp1) vars
-- evalWhereExp vars (WEqual (WDot ) whereExp2) =
-- evalWhereExp vars (WNotEqual whereExp1 whereExp2) =
-- evalWhereExp vars (WLessThan whereExp1 whereExp2) =
-- evalWhereExp vars (WGreaterThan whereExp1 whereExp2) =
-- evalWhereExp vars (WLessOrEqualThan whereExp1 whereExp2) =
-- evalWhereExp vars (WGreaterOrEqualThan whereExp1 whereExp2) =
-- evalWhereExp vars (WStartsWith whereExp1 whereExp2) =
-- evalWhereExp _ _ = error ("invalid where expression")

-- unionVars :: Variables -> Variables -> Variables
-- unionVars [] _ = []
-- unionVars ((var1Name, (TypeNodes var1Nodes)):vars1) vars2 = outputVar : unionVars vars1 vars2 
--     where 
--         var2Value = getVarValueFromName vars2 var1Name
--         outputVar = (var1Name, TypeNodes $ unionLists var1Nodes (extractVariableNodes var2Value))
-- unionVars ((var1Name, (TypeRelations var1Relations)):vars1) vars2 = outputVar : unionVars vars1 vars2 
--     where 
--         var2Value = getVarValueFromName vars2 var1Name
--         outputVar = (var1Name, TypeRelations $ unionLists var1Relations (extractVariableRelations var2Value))   

-- complementVars :: Variables -> Variables -> Variables
-- complementVars [] _ = []
-- complementVars ((var1Name, (TypeNodes var1Nodes)):vars1) vars2 = outputVar : complementVars vars1 vars2 
--     where 
--         var2Value = getVarValueFromName vars2 var1Name
--         outputVar = (var1Name, TypeNodes $ complementLists var1Nodes (extractVariableNodes var2Value))   
-- complementVars ((var1Name, (TypeRelations var1Relations)):vars1) vars2 = outputVar : complementVars vars1 vars2 
--     where 
--         var2Value = getVarValueFromName vars2 var1Name
--         outputVar = (var1Name, TypeRelations $ complementLists var1Relations (extractVariableRelations var2Value))  





getRelationLiteralsInt :: Relations -> String -> [Literal]
getRelationLiteralsInt relations str = [ getLiteralsIntR n str | n <- relations]

getRelationLiteralsString :: Relations -> String -> [Literal]
getRelationLiteralsString relations str = [ getLiteralsStringR n str | n <- relations]

getRelationLiteralsBool :: Relations -> String -> [Literal]
getRelationLiteralsBool relations str = [ getLiteralsBoolR n str | n <- relations]

-- type int 
getNodeLiteralsInt :: Nodes -> String -> [Literal]
getNodeLiteralsInt nodes str = [ getLiteralsInt n str | n <- nodes  ]

-- type String 
getNodeLiteralsStr :: Nodes -> String -> [Literal]
getNodeLiteralsStr nodes str = [ getLiteralsString n str | n <- nodes  ]

-- type Boolean
getNodeLiteralsBool :: Nodes -> String -> [Literal]
getNodeLiteralsBool nodes str = [ getLiteralsBool n str | n <- nodes  ]


getLiteralsInt :: (NodeHeader, NodeEntry) -> String -> Literal
getLiteralsInt ((NodeHeader list _ ), (NodeEntry _ list1 _)) str = output
    where
        index = (findIndexc (Field str TypeInteger) list )
        literal = if index == -1 then LiteralNull else  list1 !! (findIndexc (Field str TypeInteger) list )
        output = literal

getLiteralsIntR :: (RelationshipHeader, RelationshipEntry) -> String -> Literal
getLiteralsIntR ((RelationshipHeader list), (RelationshipEntry _ list1 _ _)) str = output
    where
        index = (findIndexc (Field str TypeInteger) list )
        literal = if index == -1 then LiteralNull else  list1 !! (findIndexc (Field str TypeInteger) list )
        output = literal

getLiteralsString :: (NodeHeader, NodeEntry) -> String -> Literal
getLiteralsString ((NodeHeader list _ ), (NodeEntry _ list1 _)) str = output
    where
        index = (findIndexc (Field str TypeString) list )
        literal = if index == -1 then LiteralNull else  list1 !! (findIndexc (Field str TypeString) list )
        output = literal
getLiteralsStringR :: (RelationshipHeader, RelationshipEntry) -> String -> Literal
getLiteralsStringR ((RelationshipHeader list), (RelationshipEntry _ list1 _ _)) str = output
    where
        index = (findIndexc (Field str TypeString) list )
        literal = if index == -1 then LiteralNull else  list1 !! (findIndexc (Field str TypeString) list )
        output = literal


getLiteralsBool :: (NodeHeader, NodeEntry) -> String -> Literal
getLiteralsBool ((NodeHeader list _ ), (NodeEntry _ list1 _)) str = output
    where
        index = (findIndexc (Field str TypeBoolean) list )
        literal = if index == -1 then LiteralNull else  list1 !! (findIndexc (Field str TypeBoolean) list )
        output = literal

getLiteralsBoolR :: (RelationshipHeader, RelationshipEntry) -> String -> Literal
getLiteralsBoolR ((RelationshipHeader list), (RelationshipEntry _ list1 _ _)) str = output
    where
        index = (findIndexc (Field str TypeBoolean) list )
        literal = if index == -1 then LiteralNull else  list1 !! (findIndexc (Field str TypeBoolean) list )
        output = literal


findIndexc :: Eq a => a -> [a] -> Int
findIndexc val list = case elemIndex val list of
    Just index -> index
    Nothing    -> -1



multiZipL :: [[a]] -> [[a]]
multiZipL = transpose









------------------------------------------------------------------
{-- 
  WHERE:::::::::::::::::::::::::::::::::
--}
-------------------------------------------------------------------





y [] _ = []
y (x:xs) s 
    | x == s = x : y xs s
    | otherwise = y xs s

evalWhere :: Variables -> Where -> Variables
evalWhere vars (Where whereExp) = evalWhereExp vars  whereExp 

evalWhereExp :: Variables -> WhereExp -> Variables
evalWhereExp vars (WAnd whereFunc whereExp) = evalWhereExp (evalWhereFunc vars whereFunc) whereExp
evalWhereExp vars (WOr whereFunc whereExp) = unionVars (evalWhereFunc vars whereFunc) (evalWhereExp vars whereExp) 
evalWhereExp vars (WNot whereExp) = complementVars (evalWhereExp vars whereExp) vars
evalWhereExp vars (WFinal whereFunc) = evalWhereFunc vars whereFunc

-- evalWhereFunc :: Variables -> WhereFunc -> Variables
-- evalWhereFunc vars (WEqual wdot wlit) = [evalWhereEqual vars wdot wlit]
-- evalWhereFunc vars (WNotEqual wdot wlit) = evalWhereCompG vars wdot wlit (/=)
-- evalWhereFunc vars (WLessThan wdot wlit) = evalWhereCompI vars wdot wlit (<)
-- evalWhereFunc vars (WGreaterThan wdot wlit) = evalWhereCompI vars wdot wlit (>)
-- evalWhereFunc vars (WLessOrEqualThan wdot wlit) = evalWhereCompI vars wdot wlit (<=)
-- evalWhereFunc vars (WGreaterOrEqualThan wdot wlit) = evalWhereCompI vars wdot wlit (>=)
-- evalWhereFunc vars (WStartsWith wdot wlit) = evalWhereCompS vars wdot wlit (startsWith)

evalWhereFunc vars (WEqualDot wdot1 wdot2) = undefined
evalWhereFunc vars (WNotEqualDot wdot1 wdot2) = undefined
evalWhereFunc vars (WLessThanDot wdot1 wdot2) = undefined
evalWhereFunc vars (WGreaterThanDot wdot1 wdot2) = undefined
evalWhereFunc vars (WLessOrEqualThanDot wdot1 wdot2) = undefined
evalWhereFunc vars (WGreaterOrEqualThanDot wdot1 wdot2) = undefined
evalWhereFunc vars (WStartsWithDot wdot1 wdot2) = undefined


--evalWhereEqual :: Variables -> WhereDot -> WhereLit -> Bool 
evalWhereEqual :: Variables -> WhereDot -> WhereLit -> (String,VariableValue)
evalWhereEqual vars (WDot varName (WLabelField)) wlit = (varName , TypeNodes (truelist (getValN val) (bWlit (getNodeLabel (getValN val) []) wlit )))
    where 
            val = getVarValueFromName vars varName
evalWhereEqual vars (WDot varName (WFieldName fieldName)) wlit 
    | isTypeNodes val == True =  (varName,TypeNodes (truelist (getValN val) (boolWlit (getFTN (getValN val) fieldName []) wlit) ))
    | otherwise = (varName,TypeRelations (truelist (getValR val) (boolWlit (getFTR (getValR val) fieldName [] ) wlit))) -- truelist (getValR val) (boolWlit (getFTR (getValR val) fieldName [] ) wlit)  
    where 
        val = getVarValueFromName vars varName 



-- getNodeLabel :: Nodes -> [Labels] -> [Labels]
-- getNodeLabel [] acc = reverseList acc
-- getNodeLabel ((nH,NodeEntry _ _ label): rest) acc = getNodeLabel rest (label:acc)

bWlit :: [Labels] -> WhereLit -> [Bool]
bWlit lanel wlit = [ anyTrue (bWlit' l wlit) | l <- lanel] 

anyTrue :: [Bool] -> Bool
anyTrue = any id


bWlit' :: [Label] -> WhereLit -> [Bool]
bWlit' ((Label str):xs) (WStr str2) 
    | str == str2 = True: bWlit' xs (WStr str2) 
    | otherwise = False : bWlit' xs (WStr str2)  


boolWlit :: [Literal] -> WhereLit -> [Bool]
boolWlit (x:xs) wlit 
    | exWhLi wlit == x = True : boolWlit xs wlit 
    | otherwise = False : boolWlit xs wlit 


truelist :: [(a,b)] -> [Bool] -> [(a,b)]
truelist (x:xs) (blist:bxs) 
    | blist == True = x : truelist xs bxs 
    | otherwise = truelist xs bxs 



exWhLi :: WhereLit -> Literal 
exWhLi (WStr s) = LiteralStr s 
exWhLi (WInt i) = LiteralInt i
exWhLi (WBool b) = LiteralBool b 
exWhLi (WNull) = LiteralNull 

getFTN :: Nodes -> String -> [Literal] -> [Literal]
getFTN [] str acc = acc  
getFTN ((NodeHeader flist b,NodeEntry s litlist l):rest) str acc  
    | (getFTN' flist str 0) == -1 = getFTN rest str acc
    | otherwise = getFTN rest str ((litlist !!  (getFTN' flist str 0)):acc) 


getFTR :: Relations -> String -> [Literal] -> [Literal]
getFTR [] str acc = acc
getFTR ((RelationshipHeader flist ,RelationshipEntry s litlist e t):rest) str acc
    | (getFTN' flist str 0) == -1 = getFTR rest str acc
    | otherwise = getFTR rest str ((litlist !!  (getFTN' flist str 0)):acc) 


getFTN' :: [Field] -> String -> Int -> Int 
getFTN' [] _ i = -1  
getFTN' ((Field stri TypeString):rest) str i 
    | stri == str = i
    | otherwise = getFTN' rest str (i+1)
getFTN' ((Field sint TypeBoolean ):rest) str i
    | sint == str = i
    | otherwise = getFTN' rest str (i+1)
getFTN' ((Field  sbool TypeInteger):rest) str i
    | sbool == str = i 
    | otherwise = getFTN' rest str (i+1)


-- getFTN' ((LiteralNull):rest) str = LiteralNull 

getNodeFieldValue :: (NodeHeader, NodeEntry) -> String -> Maybe Literal
getNodeFieldValue (NodeHeader fields _, (NodeEntry _ literals _)) fieldName
    | col == Nothing = Nothing
    | otherwise = Just (literals !! fromJust col)
    where
        fieldNames = getFieldNames fields
        col = elemIndex fieldName fieldNames 

getFieldNames :: Fields -> [String]
getFieldNames []                   = []
getFieldNames ((Field str _):fields) = str:getFieldNames fields 

fromJust :: Maybe a -> a
fromJust (Just a) = a

evalWhereCompI :: Variables -> WhereDot -> WhereLit -> (Int -> Int -> Bool) -> Variables
evalWhereCompI vars wdot wlit pred = vars

evalWhereCompS :: Variables -> WhereDot -> WhereLit -> (String -> String -> Bool) -> Variables
evalWhereCompS vars wdot wlit pred = vars

evalWhereCompG :: Variables -> WhereDot -> WhereLit -> (String -> String -> Bool) -> Variables
evalWhereCompG vars wdot wlit pred = vars

evalWhereDotCompG :: Variables -> WhereDot -> WhereDot -> (a0 -> a0 -> Bool) -> Variables
evalWhereDotCompG vars wdot wlit pred = vars

evalWhereDotCompI :: Variables -> WhereDot -> WhereDot -> (Int -> Int -> Bool) -> Variables
evalWhereDotCompI vars wdot wlit pred = vars

evalWhereDotCompS :: Variables -> WhereDot -> WhereDot -> (String -> String -> Bool) -> Variables
evalWhereDotCompS vars wdot wlit pred = vars


-- evalWhereDotComp :: Variables -> WhereDot -> WhereDot -> Variables
evalWhereDotComp = undefined

startsWith :: Eq a => [a] -> [a] -> Bool
startsWith [] _ = True
startsWith _  [] = False
startsWith (x:xs) (y:ys) = x == y && startsWith xs ys

endsWith :: Eq a => [a] -> [a] -> Bool
endsWith xs ys = startsWith (reverse xs) (reverse ys)

-- unionVars :: Variables -> Variables -> Variables
-- unionVars [] _ = []
-- unionVars ((var1Name, (TypeNodes var1Nodes)):vars1) vars2 = outputVar : unionVars vars1 vars2 
--     where 
--         var2Value = getVarValueFromName vars2 var1Name
--         outputVar = (var1Name, TypeNodes $ unionLists var1Nodes (extractVariableNodes var2Value))
-- unionVars ((var1Name, (TypeRelations var1Relations)):vars1) vars2 = outputVar : unionVars vars1 vars2 
--     where 
--         var2Value = getVarValueFromName vars2 var1Name
--         outputVar = (var1Name, TypeRelations $ unionLists var1Relations (extractVariableRelations var2Value))   

-- complementVars :: Variables -> Variables -> Variables
-- complementVars [] _ = []
-- complementVars ((var1Name, (TypeNodes var1Nodes)):vars1) vars2 = outputVar : complementVars vars1 vars2 
--     where 
--         var2Value = getVarValueFromName vars2 var1Name
--         outputVar = (var1Name, TypeNodes $ complementLists var1Nodes (extractVariableNodes var2Value))
-- complementVars ((var1Name, (TypeRelations var1Relations)):vars1) vars2 = outputVar : complementVars vars1 vars2 
--     where 
--         var2Value = getVarValueFromName vars2 var1Name
--         outputVar = (var1Name, TypeRelations $ complementLists var1Relations (extractVariableRelations var2Value))
















