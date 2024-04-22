module GqlEvaluator where
import LangParser
import LangLexer
import InputLexer
import InputParser

data VariableValue 
    = TypeFile File
    | TypeNodes Nodes
    | TypeRelations Relations 


type Variables = [(String, VariableValue)]
type Variable = (String, VariableValue)

getVarFromName :: String -> Variables -> VariableValue 
getVarFromName name [] = error ("No variable found for " ++ name) 
getVarFromName name ((varName, varValue) : vars) 
    | name == varName = varValue
    | otherwise = getVarFromName name vars


addVariable :: String -> VariableValue -> Variables -> Variables
addVariable name val variables = variables : (name, val)

-- evalProgram :: Variables -> Query -> Variables
-- evalProgram (Program [] finalAssignment) = 

-- evalQuery :: Query -> Variables -> 
evalQuery (Query read matches )









-- printResult :: Variables -> String
-- printResult ((bindingName, value): vars)
--     | bindingName == "output" = printOutput value
--     | otherwise = printResult env

evalRead :: Read -> File
evalRead (Read filename) = readInputFile filename 


printOutput :: VariableValue -> String
printOutput (FileType file) = printFile file

readInputFile :: String -> File
readInputFile fileName = parse
    where
        input = getInputFile fileName
        lexed = lexInputFile input
        parse = parseInputFile lexed

getInputFile :: String -> String
getInputFile fileName = readFile fileName

lexIndexFile :: String -> [InputToken]
lexIndexFile fileName = alexScanTokens

parseInputFile :: [InputToken] -> File
parseInputFile tokens = inputParser tokens