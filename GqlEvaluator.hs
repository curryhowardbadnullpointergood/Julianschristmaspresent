module GqlEvaluator where
import LangParser
import LangLexer
import InputLexer
import InputParser

import Data.List (nub, elemIndex, transpose)

 
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
evalMatch :: Variables -> Match -> File -> File
evalMatch vars (Match patterns return) file = output
    where
        matchVars = evalPatterns vars patterns file
        output = evalReturn matchVars return
evalMatch vars (MatchWhere patterns whereConditions return) file = output
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
evalReturn :: Variables -> Return -> File
evalReturn vars (Return (outputs:outputss)) = File [] []
---------------------------------------------------------------------------------------------------
-- Evaluating Outputs
---------------------------------------------------------------------------------------------------
evalOutputs :: Variables -> Outputs -> [[Literal]]-> [[Literal]]
evalOutputs vars [] acc = multiZipL acc 
evalOutputs vars (output:outputs) acc = evalOutputs vars outputs (evalOutput vars output:acc)




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
evalOutput :: Variables ->  Output -> [Literal]
evalOutput  vars (StrOutput varName fieldName asName)
    | isTypeNodes val == True = (getNodeLiteralsStr (getValN val) fieldName) 
    | otherwise = getRelationLiteralsString (getValR val) fieldName 
        where
            val = (getVarValueFromName vars varName)
            nodes = (getValN val)
            

evalOutput  vars (IntOutput varName fieldName asName) 
    | isTypeNodes (getVarValueFromName vars varName) == True = getNodeLiteralsInt (getValN val) fieldName
    | otherwise = getRelationLiteralsInt (getValR val) fieldName 
        where
            val = (getVarValueFromName vars varName)
        
evalOutput vars (BoolOutput varName fieldName asName)
    | isTypeNodes (getVarValueFromName vars varName) == True = getNodeLiteralsBool (getValN val) fieldName
    | otherwise = getRelationLiteralsBool  (getValR val) fieldName 
        where
            val = (getVarValueFromName vars varName)
             
            
evalOutput  vars (IdOutput varName) = []
evalOutput  vars (StartOutput varName) = []
evalOutput  vars (EndOutput varName) = []
evalOutput  vars (LabelOutput varName) = []

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

 

evalWhere :: Variables -> Where -> Variables
evalWhere vars (Where whereExp) = evalWhereExp vars  whereExp 

evalWhereExp :: Variables -> WhereExp -> Variables

evalWhereExp vars (WAnd whereFunc whereExp) = evalWhereExp (evalWhereFunc vars whereFunc) whereExp
evalWhereExp vars (WOr whereFunc whereExp) = unionVars (evalWhereFunc vars whereFunc) (evalWhereExp vars whereExp) 
evalWhereExp vars (WNot whereExp) = complementVars (evalWhereExp vars whereExp) vars
evalWhereExp vars (WFinal whereFunc) = evalWhereFunc vars whereFunc



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
-- evalWhereConditions :: Variables -> WhereConditions -> Variables
-- evalWhereConditions vars (WhereConditionOr whereCondition whereConditions) = unionVars (evalWhereCondition vars whereCondition) (evalWhereConditions vars whereConditions) 
-- evalWhereConditions vars (WhereConditionAnd whereCondition whereConditions) = evalWhereConditions (evalWhereCondition vars whereCondition) whereConditions
-- evalWhereConditions vars (WhereConditionNot whereConditions) = complementVars (evalWhereConditions vars whereConditions) vars
-- evalWhereConditions vars (WhereCondition whereCondition) = evalWhereCondition vars whereCondition
