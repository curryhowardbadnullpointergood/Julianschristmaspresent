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


    -- General Stuff
    "="                     {\p s -> LTok p LTokenAssignment}


    -- Read Stuff
    "READ"                  {\p s -> LTok p LTokenRead}

    -- Match Stuff
    "MATCH"                 {\p s -> LTok p LTokenMatch}
    "-"                     {\p s -> LTok p LTokenRelated}
    "->"                    {\p s -> LTok p LTokenRelatedRight}
    "<-"                    {\p s -> LTok p LTokenRelatedLeft}                   
    "--"                    {\p s -> LTok p LTokenNewRelation}

    -- Where Stuff
    "WHERE"                 {\p s -> LTok p LTokenWhere}

    "OR"                    {\p s -> LTok p LTokenOr}                 
    "AND"                   {\p s -> LTok p LTokenAnd}
    "NOT"                   {\p s -> LTok p LTokenNot}

    "IS"                    {\p s -> LTok p LTokenIs}

    "STARTS WITH"           {\p s -> LTok p LTokenStartWith}
    "ENDS WITH"             {\p s -> LTok p LTokenEndWith}
    "("                     {\p s -> LTok p LTokenLParen}
    ")"                     {\p s -> LTok p LTokenRParen}

    "<="                    {\p s -> LTok p LTokenLessThanEqual}
    ">="                    {\p s -> LTok p LTokenGreaterThanEqual}
    "<"                     {\p s -> LTok p LTokenLessThan}
    ">"                     {\p s -> LTok p LTokenGreaterThan}
    "=="                    {\p s -> LTok p LTokenEquals}
    "!="                    {\p s -> LTok p LTokenNotEquals}

    -- Outputting Stuff
    "RETURN"                {\p s -> LTok p LTokenReturn}
    "APPEND"                {\p s -> LTok p LTokenAppend}
    "DELETE"                {\p s -> LTok p LTokenDelete}
    "UPDATE"                {\p s -> LTok p LTokenUpdate}

    "."                     {\p s -> LTok p LTokenFullStop}
    ","                     {\p s -> LTok p LTokenComma}
    "|"                     {\p s -> LTok p LTokenBar}
    "AS"                    {\p s -> LTok p LTokenAs}

    "+"                     {\p s -> LTok p LTokenAdd}
    "-"                     {\p s -> LTok p LTokenMinus}

    -- Values 
    ":LABEL"                        {\p s -> LTok p LTokenLabelField}
    ":ID"                           {\p s -> LTok p LTokenIdField}
    ":STARTID"                      {\p s -> LTok p LTokenStartField}
    ":ENDID"                        {\p s -> LTok p LTokenEndField}
    ":TYPE"                         {\p s -> LTok p LTokenTypeField}

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
    -- General Stuff
    = LTokenAssignment
    | LTokenFullStop

    -- Read Stuff
    | LTokenRead

    -- Match Stuff
    | LTokenMatch
    | LTokenRelated
    | LTokenRelatedRight
    | LTokenRelatedLeft

    -- Where Stuff
    | LTokenWhere

    | LTokenOr
    | LTokenAnd
    | LTokenNot

    | LTokenIs

    | LTokenStartWith
    | LTokenEndWith

    | LTokenLParen
    | LTokenRParen

    | LTokenLessThanEqual
    | LTokenGreaterThanEqual
    | LTokenLessThan
    | LTokenGreaterThan
    | LTokenEquals
    | LTokenNotEquals

    -- Outputting Stuff
    | LTokenReturn
    | LTokenAppend
    | LTokenDelete
    | LTokenUpdate

    | LTokenComma
    | LTokenBar
    | LTokenAs

    | LTokenAdd
    | LTokenMinus

    | LTokenNewRelation

    -- Values 
    | LTokenLabelField
    | LTokenIdField
    | LTokenStartField
    | LTokenEndField
    | LTokenTypeField

    | LTokenNull
    | LTokenTrue
    | LTokenFalse
    | LTokenString String      
    | LTokenInt Int            
    | LTokenName String
    deriving (Eq, Show)



}