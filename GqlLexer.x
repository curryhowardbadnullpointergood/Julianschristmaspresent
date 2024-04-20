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
    "output"                {\p s -> LTok p TokenOutputVar}

    $white+                 ;
    "//" .*                 ; -- single line comment
    "/*" [a[^a]\n]* "*/"    ; -- multi line comment

    ";"                     {\p s -> LTok p TokenSemiColon}
    "="                     {\p s -> LTok p TokenAssignment}

    -- Comparisons
    "<="                    {\p s -> LTok p TokenLessThanEqual}
    ">="                    {\p s -> LTok p TokenGreaterThanEqual}
    "<"                     {\p s -> LTok p TokenLessThan}
    ">"                     {\p s -> LTok p TokenGreaterThan}
    "=="                    {\p s -> LTok p TokenEquals}
    "!="                    {\p s -> LTok p TokenNotEquals}
    
    -- String predicates
    "["integer+"]"          {\p s -> LTok p (TokenIndex (read (tail $ init s) :: Int))} 
    "Length"                {\p s -> LTok p TokenLength}

    -- Union Intersection
    "&&"                    {\p s -> LTok p TokenAnd}
    "||"                    {\p s -> LTok p TokenOr}

    -- Brackets   
    "("                     {\p s -> LTok p TokenLParen}
    ")"                     {\p s -> LTok p TokenRParen}


    -- Functions
    ":" $white* "int"       {\p s -> LTok p TokenIntFieldType}
    ":" $white* "string"    {\p s -> LTok p TokenStrFieldType}
    ":" $white* "boolean"   {\p s -> LTok p TokenBoolFieldType}
    ":Label"                {\p s -> LTok p TokenLabelField}

    "Read"                  {\p s -> LTok p TokenRead}

    -- "GetIntField"           {\p s -> LTok p TokenGetIntField}
    -- "GetStrField"           {\p s -> LTok p TokenGetStrField}
    -- "GetBoolField"          {\p s -> LTok p TokenGetBoolField}
    -- "GetField"              {\p s -> LTok p TokenGetField}

    -- "GetLabels"             {\p s -> LTok p TokenGetLabels}     
    "GetNodes"              {\p s -> LTok p TokenGetNodes}    
    "GetRelations"          {\p s -> LTok p TokenGetRelations}

    "FilterField"           {\p s -> LTok p TokenFilterField} 
    "FilterRelations"       {\p s -> LTok p TokenFilterRelations}

    -- Types
    "File"                  {\p s -> LTok p TokenTypeFile}
    "Nodes"                 {\p s -> LTok p TokenTypeNodes}
    "Relations"             {\p s -> LTok p TokenTypeRelations}



    "True"                          {\p s -> LTok p TokenTrue}
    "False"                         {\p s -> LTok p TokenFalse}
    \"[$char$integer$special]*\"    {\p s -> LTok p (TokenString (tail $ init s))}
    $integer+                       {\p s -> LTok p (TokenInt (read s :: Int))}
    $Lchar [$char $integer \']*     {\p s -> LTok p (TokenVar s)}




{



data LangToken 
    = LTok AlexPosn LangTokenClass
    deriving (Eq, Show)

data LangTokenClass 
    = TokenOutputVar          
    | TokenSemiColon          
    | TokenAssignment         
         
    | TokenAnd                     
    | TokenOr      

    | TokenEquals             
    | TokenNotEquals 

    | TokenLessThanEqual      
    | TokenGreaterThanEqual   
    | TokenLessThan           
    | TokenGreaterThan        

    | TokenIndex Int          
    | TokenLength   
    | TokenTrue                    
    | TokenFalse                   

    | TokenLParen             
    | TokenRParen

    | TokenIntFieldType        
    | TokenStrFieldType        
    | TokenBoolFieldType       
    | TokenLabelField
    | TokenRead               
    -- | TokenGetField           
    -- | TokenGetIntField        
    -- | TokenGetStrField        
    -- | TokenGetBoolField
    -- | TokenGetLabels

    | TokenGetNodes
    | TokenGetRelations

    | TokenFilterLabel        
    | TokenFilterField        
    | TokenFilterRelations    

    | TokenTypeFile           
    | TokenTypeNodes          
    | TokenTypeRelations
    | TokenString String      
    | TokenInt Int            
    | TokenVar String
    deriving (Eq, Show)



}