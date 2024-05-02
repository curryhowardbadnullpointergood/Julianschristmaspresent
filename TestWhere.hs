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
    -- let inputData = ([[(":ID","jj23",TypeString),("firstname","John",TypeString),("familyname","Jones",TypeString),("role","Caretaker",TypeString),("age","43",TypeInt),(":LABEL","Staff",TypeString)],[(":ID","uh12",TypeString),("firstname","Umar",TypeString),("familyname","Habib",TypeString),("role","Headmaster",TypeString),("age","55",TypeInt),(":LABEL","Staff",TypeString)],[(":ID","gt2",TypeString),("firstname","Guillaume",TypeString),("familyname","Truffaut",TypeString),("role","Teacher",TypeString),("age","23",TypeInt),(":LABEL","Staff",TypeString)],[(":ID","jv9",TypeString),("firstname","Jennifer",TypeString),("familyname","Villeneuve",TypeString),("role","DeputyHead",TypeString),("age","49",TypeInt),(":LABEL","Staff",TypeString)],[(":ID","ab23",TypeString),("firstname","Adam",TypeString),("familyname","Baker",TypeString),("role","Teacher",TypeString),("age","22",TypeInt),(":LABEL","Staff",TypeString)],[(":ID","nj10",TypeString),("firstname","Nigel",TypeString),("familyname","Jackson",TypeString),("age","16",TypeInt),(":LABEL","Student",TypeString)],[(":ID","rw5",TypeString),("firstname","Rebecca",TypeString),("familyname","Watson",TypeString),("age","15",TypeInt),(":LABEL","Student",TypeString)],[(":ID","pp8",TypeString),("firstname","Peter",TypeString),("familyname","Potter",TypeString),("age","17",TypeInt),(":LABEL","Student",TypeString)],[(":ID","jd6",TypeString),("firstname","Jing",TypeString),("familyname","Ding",TypeString),("age","16",TypeInt),(":LABEL","Student",TypeString)],[(":ID","cr2",TypeString),("firstname","Connor",TypeString),("familyname","Flaherty",TypeString),("age","15",TypeInt),(":LABEL","Student",TypeString)],[(":ID","v1",TypeString),("firstname","Ray",TypeString),("familyname","Wise",TypeString),(":LABEL","Visitor",TypeString)],[(":ID","v2",TypeString),("firstname","Barbara",TypeString),("familyname","King",TypeString),(":LABEL","Visitor",TypeString)],[(":ID","v3",TypeString),("firstname","Mei",TypeString),("familyname","Wu",TypeString),(":LABEL","Visitor",TypeString)],[(":ID","v4",TypeString),("firstname","Anika",TypeString),("familyname","Sharma",TypeString),(":LABEL","Visitor",TypeString)]],[[(":START_ID","v1",TypeString),(":END_ID","uh12",TypeString),("TYPE","IsVisiting",TypeString)],[(":START_ID","v2",TypeString),(":END_ID","nj10",TypeString),("TYPE","IsVisiting",TypeString)],[(":START_ID","v3",TypeString),(":END_ID","jd6",TypeString),("TYPE","IsVisiting",TypeString)],[(":START_ID","v4",TypeString),(":END_ID","uh12",TypeString),("TYPE","IsVisiting",TypeString)]])
    -- let p1 = (PatternRelatedTo "x" "y" "z")
    -- let p2 = (PatternRelatedTo "a" "b" "c")
    -- let w = WFinal (WEqualDot (WDot "z" ":ID") (WDot "a" ":ID")) 


    -- let p3 = (Pattern "a")
    -- let p4 = (Pattern "b")
    -- let p5 = (Pattern "c")
    -- let env2 = evalMatch (Match [p3, p4, p5]) inputData
    -- let w2 = WAnd (WEqualDot (WDot "a" ":ID") (WDot "b" ":START_ID")) (WFinal (WEqualDot (WDot "c" ":ID") (WDot "b" ":END_ID")))
    -- let env3 = evalWhere env2 (Where w2) 

    let inputData = ([[(":ID","loc1",TypeString),("site","Garden",TypeString),(":LABEL","Location",TypeString)],[(":ID","loc2",TypeString),("site","FrontRoom",TypeString),(":LABEL","Location",TypeString)],[(":ID","loc3",TypeString),("site","Kitchen",TypeString),(":LABEL","Location",TypeString)],[(":ID","loc4",TypeString),("site","MainBedroom",TypeString),(":LABEL","Location",TypeString)],[(":ID","task1",TypeString),("description","Paving",TypeString),("duration","8",TypeInt),(":LABEL","Job",TypeString)],[(":ID","task2",TypeString),("description","Fencing",TypeString),("duration","12",TypeInt),(":LABEL","Job",TypeString)],[(":ID","task3",TypeString),("description","Wiring",TypeString),("duration","4",TypeInt),(":LABEL","Job",TypeString)],[(":ID","task4",TypeString),("description","Plumbing",TypeString),("duration","12",TypeInt),(":LABEL","Job",TypeString)],[(":ID","task5",TypeString),("description","Painting",TypeString),("duration","4",TypeInt),(":LABEL","Job",TypeString)],[(":ID","emp1",TypeString),("name","Jane",TypeString),(":LABEL","Staff",TypeString)],[(":ID","emp2",TypeString),("name","Bill",TypeString),(":LABEL","Staff",TypeString)],[(":ID","emp3",TypeString),("name","Winona",TypeString),(":LABEL","Staff",TypeString)],[(":ID","emp4",TypeString),("name","Rajesh",TypeString),(":LABEL","Staff",TypeString)],[(":ID","emp5",TypeString),("name","Jakub",TypeString),(":LABEL","Staff",TypeString)]],[[(":START_ID","loc1",TypeString),("priority","2",TypeInt),(":END_ID","task1",TypeString),(":TYPE","ToComplete",TypeString)],[(":START_ID","loc1",TypeString),("priority","4",TypeInt),(":END_ID","task2",TypeString),(":TYPE","ToComplete",TypeString)],[(":START_ID","loc2",TypeString),("priority","9",TypeInt),(":END_ID","task3",TypeString),(":TYPE","ToComplete",TypeString)],[(":START_ID","loc2",TypeString),("priority","1",TypeInt),(":END_ID","task5",TypeString),(":TYPE","ToComplete",TypeString)],[(":START_ID","loc3",TypeString),("priority","8",TypeInt),(":END_ID","task3",TypeString),(":TYPE","ToComplete",TypeString)],[(":START_ID","loc3",TypeString),("priority","10",TypeInt),(":END_ID","task4",TypeString),(":TYPE","ToComplete",TypeString)],[(":START_ID","loc4",TypeString),("priority","9",TypeInt),(":END_ID","task3",TypeString),(":TYPE","ToComplete",TypeString)],[(":START_ID","loc4",TypeString),("priority","1",TypeInt),(":END_ID","task5",TypeString),(":TYPE","ToComplete",TypeString)],[(":START_ID","emp1",TypeString),("available","True",TypeBool),(":END_ID","task1",TypeString),(":TYPE","CanDo",TypeString)],[(":START_ID","emp1",TypeString),("available","True",TypeBool),(":END_ID","task2",TypeString),(":TYPE","CanDo",TypeString)],[(":START_ID","emp2",TypeString),("available","False",TypeBool),(":END_ID","task1",TypeString),(":TYPE","CanDo",TypeString)],[(":START_ID","emp3",TypeString),("available","True",TypeBool),(":END_ID","task3",TypeString),(":TYPE","CanDo",TypeString)],[(":START_ID","emp4",TypeString),("available","True",TypeBool),(":END_ID","task2",TypeString),(":TYPE","CanDo",TypeString)],[(":START_ID","emp4",TypeString),("available","True",TypeBool),(":END_ID","task4",TypeString),(":TYPE","CanDo",TypeString)],[(":START_ID","emp5",TypeString),("available","False",TypeBool),(":END_ID","task4",TypeString),(":TYPE","CanDo",TypeString)]])
    let m = (Match [PatternRelatedTo "a" "r1" "b",PatternRelatedTo "c" "r2" "d"])
    let w = (Where (WAnd (WGreaterOrEqualThan (WDot "r1" "priority") (WInt 8)) (WAnd (WEqual (WDot "d" ":LABEL") (WStr "Staff")) (WFinal (WEqual (WDot "r2" "available") (WBool True))))))
    let p = (Print8 (Append [[NewRelation "b" ":ID" "d" ":ID" "PossibleAllocated"]]))
    let w1 = WFinal $ WGreaterOrEqualThan (WDot "r1" "priority") (WInt 8) 
    let w2 = WFinal $ WEqual (WDot "d" ":LABEL") (WStr "Staff")
    let w3 = WFinal $ WEqual (WDot "r2" "available") (WBool True)
    let env1 = evalMatch m inputData
    -- let env2 = evalWhere env1 w
    -- print env1
    -- print env2

    -- print (evalWhereExp env1 w1)
    print (evalWhereExp env1 w2)
    -- print (evalWhereExp env1 w3)
