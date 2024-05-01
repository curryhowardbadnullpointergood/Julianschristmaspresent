import LangLexer
import LangParser
import InputLexer
import InputParser
import GqlEvaluator
import System.Environment ( getArgs )
import Control.Exception
import Data.List (nub, elemIndex, transpose,groupBy, sort, isInfixOf)
-- import Eval3 (removeDuplicates)

main = do
    let inputData = ([[(":ID","jj23",TypeString),("firstname","John",TypeString),("familyname","Jones",TypeString),("role","Caretaker",TypeString),("age","43",TypeInt),(":LABEL","Staff",TypeString)],[(":ID","uh12",TypeString),("firstname","Umar",TypeString),("familyname","Habib",TypeString),("role","Headmaster",TypeString),("age","55",TypeInt),(":LABEL","Staff",TypeString)],[(":ID","gt2",TypeString),("firstname","Guillaume",TypeString),("familyname","Truffaut",TypeString),("role","Teacher",TypeString),("age","23",TypeInt),(":LABEL","Staff",TypeString)],[(":ID","jv9",TypeString),("firstname","Jennifer",TypeString),("familyname","Villeneuve",TypeString),("role","DeputyHead",TypeString),("age","49",TypeInt),(":LABEL","Staff",TypeString)],[(":ID","ab23",TypeString),("firstname","Adam",TypeString),("familyname","Baker",TypeString),("role","Teacher",TypeString),("age","22",TypeInt),(":LABEL","Staff",TypeString)],[(":ID","nj10",TypeString),("firstname","Nigel",TypeString),("familyname","Jackson",TypeString),("age","16",TypeInt),(":LABEL","Student",TypeString)],[(":ID","rw5",TypeString),("firstname","Rebecca",TypeString),("familyname","Watson",TypeString),("age","15",TypeInt),(":LABEL","Student",TypeString)],[(":ID","pp8",TypeString),("firstname","Peter",TypeString),("familyname","Potter",TypeString),("age","17",TypeInt),(":LABEL","Student",TypeString)],[(":ID","jd6",TypeString),("firstname","Jing",TypeString),("familyname","Ding",TypeString),("age","16",TypeInt),(":LABEL","Student",TypeString)],[(":ID","cr2",TypeString),("firstname","Connor",TypeString),("familyname","Flaherty",TypeString),("age","15",TypeInt),(":LABEL","Student",TypeString)],[(":ID","v1",TypeString),("firstname","Ray",TypeString),("familyname","Wise",TypeString),(":LABEL","Visitor",TypeString)],[(":ID","v2",TypeString),("firstname","Barbara",TypeString),("familyname","King",TypeString),(":LABEL","Visitor",TypeString)],[(":ID","v3",TypeString),("firstname","Mei",TypeString),("familyname","Wu",TypeString),(":LABEL","Visitor",TypeString)],[(":ID","v4",TypeString),("firstname","Anika",TypeString),("familyname","Sharma",TypeString),(":LABEL","Visitor",TypeString)]],[[(":START_ID","v1",TypeString),(":END_ID","uh12",TypeString),("TYPE","IsVisiting",TypeString)],[(":START_ID","v2",TypeString),(":END_ID","nj10",TypeString),("TYPE","IsVisiting",TypeString)],[(":START_ID","v3",TypeString),(":END_ID","jd6",TypeString),("TYPE","IsVisiting",TypeString)],[(":START_ID","v4",TypeString),(":END_ID","uh12",TypeString),("TYPE","IsVisiting",TypeString)]])
    let p1 = (PatternRelatedTo "x" "y" "z")
    let p2 = (PatternRelatedTo "a" "b" "c")
    let w = WFinal (WEqualDot (WDot "z" ":ID") (WDot "a" ":ID")) 


    let p3 = (Pattern "a")
    let p4 = (Pattern "b")
    let p5 = (Pattern "c")
    let env2 = evalMatch (Match [p3, p4, p5]) inputData
    let w2 = WAnd (WEqualDot (WDot "a" ":ID") (WDot "b" ":START_ID")) (WFinal (WEqualDot (WDot "c" ":ID") (WDot "b" ":END_ID")))
    let env3 = evalWhere env2 (Where w2) 
    print (env3)