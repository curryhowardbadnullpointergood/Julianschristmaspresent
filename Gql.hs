import LangLexer
import LangParser
import InputLexer
import InputParser
import GqlEvaluator
import System.Environment ( getArgs )
import Control.Exception
import System.IO


main :: IO ()
main = do
    putStrLn (show $ evalWhereFunc ("n", (TypeNodes,([[(":ID","jj23",TypeString),("firstname","John",TypeString),("familyname","Jones",TypeString),("role","Caretaker",TypeString),("age","43",TypeInt),(":LABEL","Staff",TypeString)],[(":ID","uh12",TypeString),("firstname","Umar",TypeString),("familyname","Habib",TypeString),("role","Headmaster",TypeString),("age","55",TypeInt),(":LABEL","Staff",TypeString)],[(":ID","gt2",TypeString),("firstname","Guillaume",TypeString),("familyname","Truffaut",TypeString),("role","Teacher",TypeString),("age","23",TypeInt),(":LABEL","Staff",TypeString)],[(":ID","jv9",TypeString),("firstname","Jennifer",TypeString),("familyname","Villeneuve",TypeString),("role","DeputyHead",TypeString),("age","49",TypeInt),(":LABEL","Staff",TypeString)],[(":ID","ab23",TypeString),("firstname","Adam",TypeString),("familyname","Baker",TypeString),("role","Teacher",TypeString),("age","22",TypeInt),(":LABEL","Staff",TypeString)],[(":ID","nj10",TypeString),("firstname","Nigel",TypeString),("familyname","Jackson",TypeString),("age","16",TypeInt),(":LABEL","Student",TypeString)],[(":ID","rw5",TypeString),("firstname","Rebecca",TypeString),("familyname","Watson",TypeString),("age","15",TypeInt),(":LABEL","Student",TypeString)],[(":ID","pp8",TypeString),("firstname","Peter",TypeString),("familyname","Potter",TypeString),("age","17",TypeInt),(":LABEL","Student",TypeString)],[(":ID","jd6",TypeString),("firstname","Jing",TypeString),("familyname","Ding",TypeString),("age","16",TypeInt),(":LABEL","Student",TypeString)],[(":ID","cr2",TypeString),("firstname","Connor",TypeString),("familyname","Flaherty",TypeString),("age","15",TypeInt),(":LABEL","Student",TypeString)],[(":ID","v1",TypeString),("firstname","Ray",TypeString),("familyname","Wise",TypeString),(":LABEL","Visitor",TypeString)],[(":ID","v2",TypeString),("firstname","Barbara",TypeString),("familyname","King",TypeString),(":LABEL","Visitor",TypeString)],[(":ID","v3",TypeString),("firstname","Mei",TypeString),("familyname","Wu",TypeString),(":LABEL","Visitor",TypeString)],[(":ID","v4",TypeString),("firstname","Anika",TypeString),("familyname","Sharma",TypeString),(":LABEL","Visitor",TypeString)]],[[(":START_ID","v1",TypeString),("END_ID","uh12",TypeString),("TYPE","IsVisiting",TypeString)],[(":START_ID","v2",TypeString),("END_ID","nj10",TypeString),("TYPE","IsVisiting",TypeString)],[(":START_ID","v3",TypeString),("END_ID","jd6",TypeString),("TYPE","IsVisiting",TypeString)],[(":START_ID","v4",TypeString),("END_ID","uh12",TypeString),("TYPE","IsVisiting",TypeString)]]))) (WLessThan (WDot "n" "age") (WInt 25)) )
    (programFileName : _ ) <- getArgs
    sourceText <- readFile programFileName
    --putStrLn ("Input File: \n" ++ replicate 50 '-'  ++ "\n" ++ sourceText ++ "\n" ++ replicate 50 '-')
    catch (lexProgram sourceText) noLex

lexProgram :: String -> IO ()
lexProgram programSourceText = do
    let lexedProg = LangLexer.alexScanTokens programSourceText
    -- putStrLn ("Lexed As: \n" ++ replicate 50 '-'  ++ "\n" ++ show lexedProg ++ "\n" ++ replicate 50 '-')
    catch (parseProgram lexedProg) noParse

parseProgram :: [LangToken] -> IO ()
parseProgram lexedProg = do
    let parsedProg = langParser lexedProg
    --putStrLn ("Parsed As: \n" ++ replicate 50 '-'  ++ "\n" ++ show parsedProg ++ "\n" ++ replicate 50 '-')
    getInputFile parsedProg

getInputFile :: Query -> IO ()
getInputFile (Query read match) = do 
    let inputFileName   = evalReadFile read
    inputFileText       <- readFile inputFileName
    let inputFileLexed  = InputLexer.alexScanTokens inputFileText
    let inputFileParse  = inputParser inputFileLexed
    evalQuery match inputFileParse

-- THIS NEEDS FIXING

evalQuery :: Match -> File -> IO()
evalQuery  match file = undefined

-- evalQuery :: Match -> File -> IO()
-- evalQuery  match file = do
--     let matchEval = evalMatch [] match file
--     putStrLn $ printFile matchEval


noParse :: ErrorCall -> IO ()
noParse e = do let err =  show e
               hPutStr stderr ("Problem with parsing: " ++ err)
               return ()

noLex :: ErrorCall -> IO ()
noLex e = do let err =  show e
             hPutStr stderr ("Problem with lexing: " ++ err)
             return ()