{ 
module InputLexer where 
}

%wrapper "posn" 
$integer = [0-9]
$string = [a-zA-Z]
$alphaNumeric = [a-zA-Z0-9]


tokens :-
    \,              { \p s -> Tok p TokenComma }
    \:              { \p s -> Tok p TokenColon } 
    \;              { \p s -> Tok p TokenSemiColon }
    $white          ;

    -- HEADER LEXEMES
    :ID             { \p s -> Tok p TokenID }
    :START_ID       { \p s -> Tok p TokenStartID }

    string          { \p s -> Tok p (TokenFieldType TypeString) }
    integer         { \p s -> Tok p (TokenFieldType TypeInteger) }
    boolean         { \p s -> Tok p (TokenFieldType TypeBoolean) } 

    :LABEL          { \p s -> Tok p TokenLabel }
    :END_ID         { \p s -> Tok p TokenEndID }
    :TYPE           { \p s -> Tok p TokenType }


    -- VALUE LEXEMES
    \"$string+\"    { \p s -> Tok p (TokenStrVal (tail $ init s)) }
    $integer+       { \p s -> Tok p (TokenIntVal (read s :: Int)) }
    true            { \p s -> Tok p (TokenBoolVal True) }
    false           { \p s -> Tok p (TokenBoolVal False) }
    null            { \p s -> Tok p TokenNullVal }

    $alphaNumeric+  { \p s -> Tok p (TokenString s) }


{

data FieldType = 
    TypeString  |
    TypeBoolean |
    TypeInteger
    deriving (Show, Eq)


data InputToken = 
    Tok AlexPosn TokenClass 
    deriving (Eq, Show)


data TokenClass =
    -- Connection tokens
    TokenComma               |
    TokenColon               |
    TokenSemiColon           |
    TokenNewLine             |

    -- The field value tokens
    TokenStrVal    String    |
    TokenIntVal    Int       |
    TokenBoolVal   Bool      |
    TokenNullVal             |

    -- Token for representing unquoted strings and alphanumeric values
    TokenString    String    |

    -- The header tokens
    TokenID                  |
    TokenFieldType FieldType |
    TokenLabel               |
    TokenStartID             |
    TokenEndID               |
    TokenType
    deriving (Eq, Show)
}