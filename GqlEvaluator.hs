module GqlEvaluator where
import GqlParser
import GqlLexer
import GqlLibrary
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


evalProgram :: Variables -> Program -> Variables
evalProgram (Program [] finalAssignment) = 













printResult :: Variables -> String
printResult ((bindingName, value): vars)
    | bindingName == "output" = printOutput value
    | otherwise = printResult env


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