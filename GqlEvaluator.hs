module GqlEvaluator where
import LangParser
import LangLexer
import InputLexer
import InputParser

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
-- Evaluating Query
---------------------------------------------------------------------------------------------------
evalQuery :: Variables -> Query -> Variables
evalQuery vars (Query readFile matches) = []
     
evalReadFile :: Variables -> ReadFile -> Variables
evalReadFile  vars (ReadFile fileName) = []
---------------------------------------------------------------------------------------------------
-- Evaluating Matches 
---------------------------------------------------------------------------------------------------
-- evalMatches :: Variables -> Matches -> Variables
-- evalMatches vars [] = []
-- evalMatches vars (match:matches) = evalMatch vars

-- evalMatch 
     
evalPatterns :: Variables -> Patterns -> Variables
evalPatterns vars (PatternFinal nodesVarName) = []
evalPatterns vars (PatternRelated nodesVarName patterns) = []
evalPatterns vars (PatternRelatedVar nodesVarName relationVarName patterns) = []
evalPatterns vars (PatternRelatedTo nodesVarName patterns) = []
evalPatterns vars (PatternRelatedToVar nodesVarName relationVarName patterns) = []
evalPatterns vars (PatternRelatedBy nodesVarName patterns) = []
evalPatterns vars (PatternRelatedByVar nodesVarName relationVarName patterns) = []
---------------------------------------------------------------------------------------------------
-- Evaluating Where 
---------------------------------------------------------------------------------------------------
evalWhereConditions :: Variables -> WhereConditions -> Variables
evalWhereConditions vars (WhereConditionOr whereCondition whereConditions) = unionVars (evalWhereCondition vars whereCondition) (evalWhereConditions vars whereConditions) 
evalWhereConditions vars (WhereConditionAnd whereCondition whereConditions) = evalWhereConditions (evalWhereCondition vars whereCondition) whereConditions
evalWhereConditions vars (WhereConditionNot whereConditions) = complementVars (evalWhereConditions vars whereConditions) vars
evalWhereConditions vars (WhereCondition whereCondition) = evalWhereCondition vars whereCondition

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

evalWhereCondition :: Variables -> WhereCondition -> Variables
evalWhereCondition vars cond@(IntWhereCondition varName field intCondition) = vars'
    where
        varValue = getVarValueFromName vars varName
        updatedVarValue = coordinator varValue cond
        vars' = updateVariable vars varName updatedVarValue

filterIntField :: VariableValue -> WhereCondition -> Variables
filterIntField (TypeNodes nodes) (IntWhereCondition varName field intCondition) = 
    TypeNodes $ filterIntNodes nodes field intCondition
filterIntField (TypeRelations relations) (IntWhereCondition varName field intCondition) = 
    TypeRelations $ filter 




evalWhereCondition vars (StrWhereCondition varName field strCondition) = []
evalWhereCondition vars (BoolWhereCondition varName field boolCondition) = []
evalWhereCondition vars (LabelWhereCondition varName strCondition) = []
evalWhereCondition vars (TypeWhereCondition varName strCondition) = []
---------------------------------------------------------------------------------------------------
-- Where Conditions
---------------------------------------------------------------------------------------------------
evalIntCondition :: IntCondition -> Literal -> Bool
evalIntCondition (Greater int2)        (LiteralInt int1)   = int1 > int2
evalIntCondition (Less int2)           (LiteralInt int1)   = int1 < int2
evalIntCondition (GreaterOrEqual int2) (LiteralInt int1)   = int1 >= int2
evalIntCondition (LessOrEqual int2)    (LiteralInt int1)   = int1 <= int2
evalIntCondition (IntEqual int2)       (LiteralInt int1)   = int1 == int2
evalIntCondition (IntNotEqual int2)    (LiteralInt int1)   = int1 /= int2
evalIntCondition IntIsNull             LiteralNull         = True
evalIntCondition IntNotNull            LiteralNull         = False
evalIntCondition _                     LiteralNull         = False

evalStrCondition :: StrCondition -> Literal -> Bool
evalStrCondition (StringStarts string2)    (LiteralStr string1)    = startsWith string1 string2
evalStrCondition (StrEqual string2)        (LiteralStr string1)    = string1 == string2
evalStrCondition (StrNotEqual string2)     (LiteralStr string1)    = string1 == string2 
evalStrCondition StrIsNull                 LiteralNull             = True
evalStrCondition StrNotNull                LiteralNull             = False
evalStrCondition _                         LiteralNull             = False

evalBoolCondition :: BoolCondition -> Literal -> Bool
evalBoolCondition (BoolEqual bool2)    (LiteralBool bool1) = bool1 == bool2
evalBoolCondition (BoolNotEqual bool2) (LiteralBool bool1) = bool1 /= bool2
evalBoolCondition BoolIsNull           LiteralNull         = True
evalBoolCondition BoolNotNull          LiteralNull         = False
evalBoolCondition _ LiteralNull                            = False

startsWith :: Eq a => [a] -> [a] -> Bool
startsWith [] _ = True
startsWith _  [] = False
startsWith (x:xs) (y:ys) = x == y && startsWith xs ys

endsWith :: Eq a => [a] -> [a] -> Bool
endsWith xs ys = startsWith (reverse xs) (reverse ys)
---------------------------------------------------------------------------------------------------
-- Evaluating Return
---------------------------------------------------------------------------------------------------
evalReturn :: Variables -> Return -> Variables
evalReturn vars (Return outputs) = []
     
evalOutputs :: Variables -> Outputs -> Variables
evalOutputs vars (output:outputs) = evalOutput output : evalOutputs 
     
evalOutput :: Variables ->  Output -> Variables
evalOutput  vars (StrOutput varName fieldName asName) = []
evalOutput  vars (IntOutput varName fieldName asName) = []
evalOutput  vars (BoolOutput varName fieldName asName) = []
evalOutput  vars (IdOutput varName) = []
evalOutput  vars (StartOutput varName) = []
evalOutput  vars (EndOutput varName) = []
evalOutput  vars (LabelOutput varName) = []
---------------------------------------------------------------------------------------------------

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
