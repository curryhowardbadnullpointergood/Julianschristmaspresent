module GqlEvaluator where
import LangParser
import LangLexer
import InputLexer
import InputParser

import System.Console.Terminfo (Color(Magenta))

data VariableValue 
    = TypeFile File
    | TypeNodes Nodes
    | TypeRelations Relations 
    deriving (Show, Eq)

type Nodes = [(NodeHeader,NodeEntry)]
type Relations = [(RelationshipHeader, RelationshipEntry)]
type Variables = [(String, VariableValue)]
type Variable = (String, VariableValue)

getVarFromName :: String -> Variables -> VariableValue 
getVarFromName name [] = error ("No variable found for " ++ name) 
getVarFromName name ((varName, varValue) : vars) 
    | name == varName = varValue
    | otherwise = getVarFromName name vars


addVariable :: Variables -> String -> VariableValue ->  Variables
addVariable variables name val = (name, val) : variables 

-- evalProgram :: Variables -> Query -> Variables
-- evalProgram (Program [] finalAssignment) = 

-- evalQuery :: Query -> Variables -> 
-- evalQuery (Query read matches )

evalMatch :: Variables -> Match -> File -> Variables
evalMatch vars (MatchWhere patterns conditions return) file = undefined 
evalMatch vars (Match (PatternFinal str) return) datafile = addVariable vars str (TypeNodes (getNodes datafile))  





getNodes :: File -> [(NodeHeader, NodeEntry)]
getNodes (File nodeSets _) = concatMap getNodesFromSet nodeSets

getNodesFromSet :: NodeSet -> [(NodeHeader, NodeEntry)]
getNodesFromSet (NodeSet header entries) = map (\entry -> (header, entry)) entries







-- printResult :: Variables -> String
-- printResult ((bindingName, value): vars)
--     | bindingName == "output" = printOutput value
--     | otherwise = printResult env

-- evalRead :: Read -> File
-- evalRead (Read filename) = readInputFile filename 


-- printOutput :: VariableValue -> String
-- printOutput (FileType file) = printFile file

-- readInputFile :: String -> File
-- readInputFile fileName = parse
--     where
--         input = getInputFile fileName
--         lexed = lexInputFile input
--         parse = parseInputFile lexed

-- getInputFile :: String -> String
-- getInputFile fileName = readFile fileName

-- lexIndexFile :: String -> [InputToken]
-- lexIndexFile fileName = alexScanTokens

-- parseInputFile :: [InputToken] -> File
-- parseInputFile tokens = inputParser tokens