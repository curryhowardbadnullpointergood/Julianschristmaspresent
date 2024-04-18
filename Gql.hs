import Lexer
import Parser
import GqlEval
import System.Environment ( getArgs )
import Control.Exception
import System.IO

main :: IO ()
main = do
    (fileName : _ ) <- getArgs
    sourceText <- readFile fileName
    putStrLn ("Input File: \n" ++ replicate 50 '-'  ++ "\n" ++ sourceText ++ "\n" ++ replicate 50 '-')
    catch (main' sourceText) noLex


main' :: String -> IO ()
main' sourceText = do
    let lexedProg = alexScanTokens sourceText
    catch (main'' lexedProg) noParse


main'' :: [Token] -> IO ()
main'' lexedProg = do
    let parsedProg = inputParser lexedProg
    main''' parsedProg

main''' :: File -> IO ()
main''' parsedProg = do
    putStrLn (printFile parsedProg)

    print $ getField parsedProg "stringField"
    print $ getField parsedProg "intField"
    print $ getField parsedProg "boolField"
    print $ getLabels parsedProg

    let intField = getField parsedProg "intField"
    let less30 = filterIntField intField (\z -> z < 30) False
    let null = filterNullFieldValues intField
    putStrLn ("Nodes with intField value of < 30: " ++ show less30)
    putStrLn ("Nodes with value of 'null' in intField: " ++ show null)

    let labels = getLabels parsedProg
    let label1 = filterLabel labels (\s -> s == "label1")
    putStrLn ("Nodes with a label with value of 'label1': " ++ show label1)

    putStrLn (show $ problem1 parsedProg)


-- haskell version of what problem 1 would look like
problem1 :: File -> [String]
problem1 file = output
    where
        ageField = getField file "age"
        labels = getLabels file

        nodesLess30 = filterIntField ageField (\x -> x <= 30) False
        nodesLabelVisitor = filterLabel labels (\s -> s == "Visitor")

        output = unionLists nodesLess30 nodesLabelVisitor

noParse :: ErrorCall -> IO ()
noParse e = do let err =  show e
               hPutStr stderr ("Problem with parsing: " ++ err)
               return ()

noLex :: ErrorCall -> IO ()
noLex e = do let err =  show e
             hPutStr stderr ("Problem with lexing: " ++ err)
             return ()

