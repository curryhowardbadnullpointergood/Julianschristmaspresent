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

    let inputdata = ([[(":ID","t1",TypeString),("team","Winchester",TypeString),("points","9",TypeInt)],[(":ID","t2",TypeString),("team","Romsey",TypeString),("points","null",TypeInt)],[(":ID","t3",TypeString),("team","Eastleigh",TypeString),("points","7",TypeInt)],[(":ID","t4",TypeString),("team","FairOak",TypeString),("points","4",TypeInt)],[(":ID","t5",TypeString),("team","Totton",TypeString),("points","null",TypeInt)],[(":ID","t6",TypeString),("team","Weston",TypeString),("points","6",TypeInt)],[(":ID","t7",TypeString),("team","Hamble",TypeString),("points","7",TypeInt)],[(":ID","t8",TypeString),("team","Fareham",TypeString),("points","null",TypeInt)],[(":ID","t9",TypeString),("team","Ringwood",TypeString),("points","null",TypeInt)],[(":ID","t10",TypeString),("team","Hythe",TypeString),("points","3",TypeInt)],[(":ID","t11",TypeString),("team","Shirley",TypeString),("points","3",TypeInt)],[(":ID","t12",TypeString),("team","Southampton",TypeString),("points","3",TypeInt)],[(":ID","t13",TypeString),("team","Ashurst",TypeString),("points","4",TypeInt)],[(":ID","t14",TypeString),("team","Lyndhurst",TypeString),("points","6",TypeInt)]],[[(":START_ID","t1",TypeString),("gf","3",TypeInt),("ga","2",TypeInt),("week","3",TypeInt),(":END_ID","t10",TypeString),(":TYPE","Beat",TypeString)],[(":START_ID","t11",TypeString),("gf","3",TypeInt),("ga","0",TypeInt),("week","1",TypeInt),(":END_ID","t2",TypeString),(":TYPE","Beat",TypeString)],[(":START_ID","t3",TypeString),("gf","3",TypeInt),("ga","1",TypeInt),("week","1",TypeInt),(":END_ID","t12",TypeString),(":TYPE","DrewWith",TypeString)],[(":START_ID","t13",TypeString),("gf","3",TypeInt),("ga","1",TypeInt),("week","2",TypeInt),(":END_ID","t4",TypeString),(":TYPE","Beat",TypeString)],[(":START_ID","t14",TypeString),("gf","3",TypeInt),("ga","2",TypeInt),("week","3",TypeInt),(":END_ID","t5",TypeString),(":TYPE","Beat",TypeString)],[(":START_ID","t6",TypeString),("gf","3",TypeInt),("ga","0",TypeInt),("week","1",TypeInt),(":END_ID","t8",TypeString),(":TYPE","Beat",TypeString)],[(":START_ID","t7",TypeString),("gf","3",TypeInt),("ga","0",TypeInt),("week","1",TypeInt),(":END_ID","t9",TypeString),(":TYPE","Beat",TypeString)],[(":START_ID","t1",TypeString),("gf","2",TypeInt),("ga","0",TypeInt),("week","1",TypeInt),(":END_ID","t9",TypeString),(":TYPE","Beat",TypeString)],[(":START_ID","t10",TypeString),("gf","2",TypeInt),("ga","0",TypeInt),("week","1",TypeInt),(":END_ID","t2",TypeString),(":TYPE","Beat",TypeString)],[(":START_ID","t3",TypeString),("gf","2",TypeInt),("ga","0",TypeInt),("week","4",TypeInt),(":END_ID","t11",TypeString),(":TYPE","Beat",TypeString)],[(":START_ID","t4",TypeString),("gf","2",TypeInt),("ga","3",TypeInt),("week","3",TypeInt),(":END_ID","t12",TypeString),(":TYPE","DrewWith",TypeString)],[(":START_ID","t5",TypeString),("gf","2",TypeInt),("ga","0",TypeInt),("week","0",TypeInt),(":END_ID","t13",TypeString),(":TYPE","DrewWith",TypeString)],[(":START_ID","t14",TypeString),("gf","2",TypeInt),("ga","0",TypeInt),("week","1",TypeInt),(":END_ID","t6",TypeString),(":TYPE","Beat",TypeString)],[(":START_ID","t7",TypeString),("gf","2",TypeInt),("ga","1",TypeInt),("week","1",TypeInt),(":END_ID","t8",TypeString),(":TYPE","DrewWith",TypeString)],[(":START_ID","t1",TypeString),("gf","1",TypeInt),("ga","2",TypeInt),("week","4",TypeInt),(":END_ID","t8",TypeString),(":TYPE","Beat",TypeString)],[(":START_ID","t2",TypeString),("gf","1",TypeInt),("ga","1",TypeInt),("week","1",TypeInt),(":END_ID","t9",TypeString),(":TYPE","DrewWith",TypeString)],[(":START_ID","t3",TypeString),("gf","1",TypeInt),("ga","0",TypeInt),("week","1",TypeInt),(":END_ID","t10",TypeString),(":TYPE","Beat",TypeString)],[(":START_ID","t4",TypeString),("gf","1",TypeInt),("ga","1",TypeInt),("week","3",TypeInt),(":END_ID","t11",TypeString),(":TYPE","Beat",TypeString)],[(":START_ID","t5",TypeString),("gf","1",TypeInt),("ga","2",TypeInt),("week","2",TypeInt),(":END_ID","t12",TypeString),(":TYPE","DrewWith",TypeString)],[(":START_ID","t6",TypeString),("gf","1",TypeInt),("ga","0",TypeInt),("week","1",TypeInt),(":END_ID","t13",TypeString),(":TYPE","Beat",TypeString)],[(":START_ID","t7",TypeString),("gf","1",TypeInt),("ga","0",TypeInt),("week","2",TypeInt),(":END_ID","t14",TypeString),(":TYPE","Beat",TypeString)]])
    let query = Query (ReadFile "table.n4j") (Match [PatternRelatedTo "n'" "n'Beatn" "n",Pattern "n''",Pattern "n''Drewn'",Pattern "n'Drewn''",Pattern "n''Beatn"]) (Where (WAnd (WEqual (WDot "n'Beatn" ":TYPE") (WStr "Beat")) (WOr (WAnd (WEqualDot (WDot "n''" ":ID") (WDot "n''Drewn'" ":START_ID")) (WAnd (WEqualDot (WDot "n'" ":ID") (WDot "n''Drewn'" ":END_ID")) (WEqual (WDot "n''Drewn'" ":TYPE") (WStr "DrewWith")))) (WAnd (WAnd (WEqualDot (WDot "n'" ":ID") (WDot "n'Drewn''" ":START_ID")) (WAnd (WEqualDot (WDot "n''" ":ID") (WDot "n'Drewn''" ":END_ID")) (WAnd (WEqual (WDot "n'Drewn''" ":TYPE") (WStr "DrewWith")) (WAnd (WEqualDot (WDot "n''" ":ID") (WDot "n''Beatn" ":START_ID")) (WAnd (WEqualDot (WDot "n" ":ID") (WDot "n''Beatn" ":END_ID")) (WEqual (WDot "n''Beatn" ":TYPE") (WStr "Beat"))))))) (WEqualDot (WDot "n" "points") (WDot "n''" "points")))))) (Print7 (Return [[Output "n" ":ID" ":ID",Output "n" "points" "points"]]))
    let m = Match [PatternRelatedTo "n'" "n'Beatn" "n",Pattern "n''",Pattern "n''Drewn'",Pattern "n'Drewn''",Pattern "n''Beatn"]
    let w = Where (WAnd (WEqual (WDot "n'Beatn" ":TYPE") (WStr "Beat")) (WOr (WAnd (WEqualDot (WDot "n''" ":ID") (WDot "n''Drewn'" ":START_ID")) (WAnd (WEqualDot (WDot "n'" ":ID") (WDot "n''Drewn'" ":END_ID")) (WEqual (WDot "n''Drewn'" ":TYPE") (WStr "DrewWith")))) (WAnd (WAnd (WEqualDot (WDot "n'" ":ID") (WDot "n'Drewn''" ":START_ID")) (WAnd (WEqualDot (WDot "n''" ":ID") (WDot "n'Drewn''" ":END_ID")) (WAnd (WEqual (WDot "n'Drewn''" ":TYPE") (WStr "DrewWith")) (WAnd (WEqualDot (WDot "n''" ":ID") (WDot "n''Beatn" ":START_ID")) (WAnd (WEqualDot (WDot "n" ":ID") (WDot "n''Beatn" ":END_ID")) (WEqual (WDot "n''Beatn" ":TYPE") (WStr "Beat"))))))) (WEqualDot (WDot "n" "points") (WDot "n''" "points")))))
    let env = evalMatch m inputdata
    let w1 = Where $ WEqual (WDot "n'Beatn" ":TYPE") (WStr "Beat")
    -- let w2 = WAnd w1
    print $ evalWhere env w1 