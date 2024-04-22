module GqlEvaluator where
import LangParser
import LangLexer
import InputLexer
import InputParser

data VariableValue 
    = TypeFile File
    | TypeNodes Nodes
    | TypeRelations Relations 

type Nodes = [(NodeHeader, NodeEntry)]
type Relations = [(RelationshipHeader, RelationshipEntry)]

type Variables = [(String, VariableValue)]
type Variable = (String, VariableValue)

getVarFromName :: String -> Variables -> VariableValue 
getVarFromName name [] = error ("No variable found for " ++ name) 
getVarFromName name ((varName, varValue) : vars) 
    | name == varName = varValue
    | otherwise = getVarFromName name vars


addVariable :: String -> VariableValue -> Variables -> Variables
addVariable name val variables = (name, val) : variables 


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
