import LangLexer
import LangParser
import InputLexer
import InputParser
import GqlEvaluator
import System.Environment ( getArgs )
import Control.Exception
import System.IO

main = do
    -- let env = [[("n",[(":ID","jj23",TypeString),("firstname","John",TypeString),("familyname","Jones",TypeString),("role","Caretaker",TypeString),("age","43",TypeInt),(":LABEL","Staff",TypeString),(":LABEL","Student",TypeString)])],[("n",[(":ID","uh12",TypeString),("firstname","Umar",TypeString),("familyname","Habib",TypeString),("role","Headmaster",TypeString),("age","55",TypeInt),(":LABEL","Student",TypeString),(":LABEL","Staff",TypeString)])],[("n",[(":ID","gt2",TypeString),("firstname","Guillaume",TypeString),("familyname","Truffaut",TypeString),("role","Teacher",TypeString),("age","23",TypeInt),(":LABEL","Staff",TypeString)])],[("n",[(":ID","jv9",TypeString),("firstname","Jennifer",TypeString),("familyname","Villeneuve",TypeString),("role","DeputyHead",TypeString),("age","49",TypeInt),(":LABEL","Staff",TypeString)])],[("n",[(":ID","ab23",TypeString),("firstname","Adam",TypeString),("familyname","Baker",TypeString),("role","Teacher",TypeString),("age","22",TypeInt),(":LABEL","Staff",TypeString)])],[("n",[(":ID","nj10",TypeString),("firstname","Nigel",TypeString),("familyname","Jackson",TypeString),("age","16",TypeInt),(":LABEL","Student",TypeString)])],[("n",[(":ID","rw5",TypeString),("firstname","Rebecca",TypeString),("familyname","Watson",TypeString),("age","15",TypeInt),(":LABEL","Student",TypeString)])],[("n",[(":ID","pp8",TypeString),("firstname","Peter",TypeString),("familyname","Potter",TypeString),("age","17",TypeInt),(":LABEL","Student",TypeString)])],[("n",[(":ID","jd6",TypeString),("firstname","Jing",TypeString),("familyname","Ding",TypeString),("age","16",TypeInt),(":LABEL","Student",TypeString)])],[("n",[(":ID","cr2",TypeString),("firstname","Connor",TypeString),("familyname","Flaherty",TypeString),("age","15",TypeInt),(":LABEL","Student",TypeString)])],[("n",[(":ID","v1",TypeString),("firstname","Ray",TypeString),("familyname","Wise",TypeString),(":LABEL","Visitor",TypeString)])],[("n",[(":ID","v2",TypeString),("firstname","Barbara",TypeString),("familyname","King",TypeString),(":LABEL","Visitor",TypeString)])],[("n",[(":ID","v3",TypeString),("firstname","Mei",TypeString),("familyname","Wu",TypeString),(":LABEL","Visitor",TypeString)])],[("n",[(":ID","v4",TypeString),("firstname","Anika",TypeString),("familyname","Sharma",TypeString),(":LABEL","Visitor",TypeString)])]]
    -- let whereFunc1 = WFinal $ WEqual           (WDot "n" ":LABEL")    (WStr "Visitor")
    -- let whereFunc2 = WFinal $ WNotEqual        (WDot "n" "firstname") (WStr "John")
    -- let whereFunc3 = WFinal $ WLessOrEqualThan (WDot "n" "age")       (WInt 25)
    -- let whereFunc4 = WFinal $ WEqual           (WDot "n" "age")       (WInt 16)
    -- let whereFunc5 = WFinal $ WEqual           (WDot "n" "notAfield")       (WNull)
    -- print (evalWhereExp env whereFunc1)
    -- putStrLn "\n"
    -- print (evalWhereExp env whereFunc2)
    -- putStrLn "\n"
    -- print (evalWhereExp env whereFunc3)
    -- putStrLn "\n"
    -- print (evalWhereExp env whereFunc4)
    -- putStrLn "\n"
    -- print (evalWhereExp env whereFunc5)
    -- putStrLn "\n"
    
    -- let whereExp1 = WOr whereFunc1 (WFinal whereFunc3)
    -- print (evalWhereExp inst whereExp1)

-- [[(String, [(String, String, DataType)])]]
