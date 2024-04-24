{ 
module LangLexer where 
}

%wrapper "posn" 
$integer = [0-9]
$Lchar = [a-z]
$Uchar = [A-Z]
$char = [a-zA-Z]
$special = [\.\:]
tokens :-
    $white+                 ;
    "//" .*                 ; -- single line comment
    "/*" [a[^a]\n]* "*/"    ; -- multi line comment

    "."                     {\p s -> LTok p LTokenFullStop}
    ","                     {\p s -> LTok p LTokenComma}
    ":"                     {\p s -> LTok p LTokenColon}
    "|"                     {\p s -> LTok p LTokenBar}

    "-"                     {\p s -> LTok p LTokenRelated}
    "->"                    {\p s -> LTok p LTokenRelatedRight}
    "<-"                    {\p s -> LTok p LTokenRelatedLeft}                   

    ":integer"              {\p s -> LTok p LTokenIntField}
    ":string"               {\p s -> LTok p LTokenStrField}
    ":boolean"              {\p s -> LTok p LTokenBoolField}
    ":LABEL"                {\p s -> LTok p LTokenLabelField}
    ":ID"                   {\p s -> LTok p LTokenIdField}
    ":STARTID"              {\p s -> LTok p LTokenStartField}
    ":ENDID"                {\p s -> LTok p LTokenEndField}
    ":TYPE"                 {\p s -> LTok p LTokenTypeField}

    "OR"                    {\p s -> LTok p LTokenOr}                 
    "AND"                   {\p s -> LTok p LTokenAnd}
    "NOT"                   {\p s -> LTok p LTokenNot}

    "READ"                  {\p s -> LTok p LTokenRead}

    "MATCH"                 {\p s -> LTok p LTokenMatch}
    "WHERE"                 {\p s -> LTok p LTokenWhere}
    -- "RETURN"                {\p s -> LTok p LTokenReturn}
    "GETNODES"              {\p s -> LTok p LTokenGetNode}
    "GETRELATIONS"          {\p s -> LTok p LTokenGetRelation}
    "AS"                    {\p s -> LTok p LTokenAs}
    
    "STARTS WITH"           {\p s -> LTok p LTokenStartWith}
    
    "("                     {\p s -> LTok p LTokenLParen}
    ")"                     {\p s -> LTok p LTokenRParen}
    "["                     {\p s -> LTok p LTokenLBrack}
    "]"                     {\p s -> LTok p LTokenRBrack}



    "<="                    {\p s -> LTok p LTokenLessThanEqual}
    ">="                    {\p s -> LTok p LTokenGreaterThanEqual}
    "<"                     {\p s -> LTok p LTokenLessThan}
    ">"                     {\p s -> LTok p LTokenGreaterThan}
    "=="                    {\p s -> LTok p LTokenEquals}
    "!="                    {\p s -> LTok p LTokenNotEquals}

    "NULL"                          {\p s -> LTok p LTokenNull}
    "TRUE"                          {\p s -> LTok p LTokenTrue}
    "FALSE"                         {\p s -> LTok p LTokenFalse}
    \"[$char$integer$special]*\"    {\p s -> LTok p (LTokenString (tail $ init s))}
    $integer+                       {\p s -> LTok p (LTokenInt (read s :: Int))}
    $char [$char $integer \_ \']*   {\p s -> LTok p (LTokenName s)}

{



data LangToken 
    = LTok AlexPosn LangTokenClass
    deriving (Eq, Show)

data LangTokenClass 
    = LTokenFullStop
    | LTokenComma
    | LTokenColon
    | LTokenSemiColon
    | LTokenBar

    | LTokenRelated
    | LTokenRelatedRight
    | LTokenRelatedLeft

    | LTokenIntField
    | LTokenStrField
    | LTokenBoolField
    | LTokenLabelField
    | LTokenIdField
    | LTokenStartField
    | LTokenEndField
    | LTokenTypeField

    | LTokenOr
    | LTokenAnd
    | LTokenNot

    | LTokenRead
    | LTokenMatch
    | LTokenWhere
    -- | LTokenReturn
    | LTokenGetNode
    | LTokenGetRelation
    | LTokenAs
    | LTokenStartWith

    | LTokenLParen
    | LTokenRParen
    | LTokenLBrack
    | LTokenRBrack


    | LTokenLessThanEqual
    | LTokenGreaterThanEqual
    | LTokenLessThan
    | LTokenGreaterThan
    | LTokenEquals
    | LTokenNotEquals

    | LTokenNull
    | LTokenTrue
    | LTokenFalse
    | LTokenString String      
    | LTokenInt Int            
    | LTokenName String
    deriving (Eq, Show)



}