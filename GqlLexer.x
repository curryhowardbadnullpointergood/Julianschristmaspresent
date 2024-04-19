{ 
module GqlLexer where 
}

%wrapper "posn" 
$integer = [0-9]
$Lchar = [a-z]
$Uchar = [A-Z]
$char = [a-zA-Z]
$special = [\.]
tokens :-
    $white+                 ;
    "//" .*                 ; -- single line comment
    "/*" [a[^a]\n]* "*/"    ; -- multi line comment

    ";"                     {\p s -> Tok p TokenSemiColon}
    "="                     {\p s -> Tok p TokenAssignment}

    -- Comparisons
    "<="                    {\p s -> Tok p TokenLessThanEqual}
    ">="                    {\p s -> Tok p TokenGreaterThanEqual}
    "<"                     {\p s -> Tok p TokenLessThan}
    ">"                     {\p s -> Tok p TokenGreaterThan}
    "=="                    {\p s -> Tok p TokenEquals}
    "!="                    {\p s -> Tok p TokenNotEquals}
    
    -- String predicates
    "["integer+"]"          {\p s -> Tok p (TokenIndex (read (tail $ init s) :: Int))} 
    "Length"                {\p s -> Tok p TokenLength}

    -- Boolean conditions
    "&&"                    {\p s -> Tok p TokenAnd}
    "||"                    {\p s -> Tok p TokenOr}
    "True"                  {\p s -> Tok p TokenTrue}
    "False"                 {\p s -> Tok p TokenFalse}

    -- Lambda
    "->"                    {\p s -> Tok p TokenLambdaArrow}
    \\                      {\p s -> Tok p TokenLamdaBackSlash}


    -- Brackets   
    "("                     {\p s -> Tok p TokenLParen}
    ")"                     {\p s -> Tok p TokenRParen}


    -- Functions
    ":" $white* "int"       {\p s -> Tok p TokenIntFieldType}
    ":" $white* "string"    {\p s -> Tok p TokenStrFieldType}
    ":" $white* "boolean"   {\p s -> Tok p TokenBoolFieldType}

    "Read"                  {\p s -> Tok p TokenRead}

    -- "GetIntField"           {\p s -> Tok p TokenGetIntField}
    -- "GetStrField"           {\p s -> Tok p TokenGetStrField}
    -- "GetBoolField"          {\p s -> Tok p TokenGetBoolField}
    "GetField"              {\p s -> Tok p TokenGetField}

    "GetLabels"             {\p s -> Tok p TokenGetLabels}         
    "GetRelations"          {\p s -> Tok p TokenGetRelations}

    "FilterLabel"           {\p s -> Tok p TokenFilterRelation}
    "FilterField"           {\p s -> Tok p TokenFilterField} 

    "FilterRelations"       {\p s -> Tok p TokenFilterRelations}

    -- Types
    "File"                  {\p s -> Tok p TokenTypeFile}
    "Nodes"                 {\p s -> Tok p TokenTypeNodes}
    "Relations"             {\p s -> Tok p TokenTypeRelations}



    \"[$char$integer$special]*\"        {\p s -> Tok p (TokenString (tail $ init s))}
    $integer+                   {\p s -> Tok p (TokenInt (read s :: Int))}
    $Lchar [$char $integer \']* {\p s -> Tok p (TokenVar s)}




{


data LangToken = 
    Tok AlexPosn LangTokenClass
    deriving (Eq, Show)

data LangTokenClass =
    TokenSemiColon          |
    TokenAssignment         |

    TokenLessThanEqual      |
    TokenGreaterThanEqual   |
    TokenLessThan           |
    TokenGreaterThan        |
    TokenEquals             |
    TokenNotEquals          |

    TokenIndex Int          |
    TokenLength             |

    TokenAnd                |       
    TokenOr                 |       
    TokenTrue               |       
    TokenFalse              |      

    TokenLambdaArrow        |
    TokenLamdaBackSlash     |

    TokenLParen             |
    TokenRParen             |

    TokenIntFieldType        |
    TokenStrFieldType        |
    TokenBoolFieldType       |

    TokenRead               |
    TokenGetField           |
    -- TokenGetIntField        |
    -- TokenGetStrField        |
    -- TokenGetBoolField       |
    TokenGetLabels          |
    TokenGetRelations       |
    TokenFilterRelation     |
    TokenFilterField        |
    TokenFilterRelations    |

    TokenTypeFile           |
    TokenTypeNodes          |
    TokenTypeRelations      |
                    
    TokenString String      |
    TokenInt Int            |
    TokenVar String
    deriving (Eq, Show)



}