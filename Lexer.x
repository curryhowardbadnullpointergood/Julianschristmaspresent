{ 
module Lexer where 
}

%wrapper "posn" 
$integer = [0-9]
-- digits
$string = [a-zA-Z]
-- alphabetic characters
-- $alphaNumeric=($string$string*$integer | $integer$integer*$string)($integer | $string)*

tokens :-
    $white+ ;
    \, ; 

    -- HEADER LEXEMES
    :ID   { \p s -> Tok p TokenID }
    :START_ID { \p s -> Tok p TokenStartID }
    -- ($string$string*$integer | $integer$integer*$string)($integer | $string)*$white?: { \p s -> Tok p (TokenFieldName s) }
    $string [$string $integer]*((\ )*)?: { \p s -> Tok p (TokenFieldName (init s))}

    string { \p s -> Tok p (TokenFieldType TypeString) }
    integer { \p s -> Tok p (TokenFieldType TypeInteger) }
    bool { \p s -> Tok p (TokenFieldType TypeBoolean) } 

    :LABEL { \p s -> Tok p TokenLabel }
    :END_ID { \p s -> Tok p TokenEndID }
    :TYPE { \p s -> Tok p TokenType }


    -- VALUE LEXEMES
    ($string$string*$integer | $integer$integer*$string)($integer | $string)* { \p s -> Tok p (TokenIDVal s) }

    $string+ { \p s -> Tok p (TokenLabelVal s)}
    \;$string+ { \p s -> Tok p (TokenLabelVal (tail s))}

    \"$string+\" { \p s -> Tok p (TokenStr (tail $ init s)) }
    $integer+ { \p s -> Tok p (TokenInt (read s :: Int)) }
    true { \p s -> Tok p (TokenBool True) }
    false { \p s -> Tok p (TokenBool False) }
    null { \p s -> Tok p TokenNull }



{

data FieldType = 
    TypeString  |
    TypeBoolean |
    TypeInteger
    deriving (Show, Eq)


data Token = 
    Tok AlexPosn TokenClass 
    deriving (Eq, Show)


data TokenClass =
    -- The field value tokens
    TokenStr       String    |
    TokenInt       Int       |
    TokenBool      Bool      |
    TokenNull                |

    -- The ID and Label value tokens
    TokenIDVal     String    |
    TokenLabelVal  String    |
    
    -- The header tokens
    TokenID                  |
    TokenFieldName String    |
    TokenFieldType FieldType |
    TokenLabel               |

    TokenStartID             |
    TokenEndID               |
    TokenType
    deriving (Eq, Show)


-- tokenPosn :: Token -> String
-- tokenPosn (TokenStr     (AlexPn a l c) s)   = show(l) ++ ":" ++ show(c)
-- tokenPosn (TokenInt     (AlexPn a l c) x)   = show(l) ++ ":" ++ show(c)
-- tokenPosn (TokenBool    (AlexPn a l c) b)   = show(l) ++ ":" ++ show(c)
-- tokenPosn (TokenAlNu    (AlexPn a l c) s)   = show(l) ++ ":" ++ show(c)
-- tokenPosn (TokenID      (AlexPn a l c))     = show(l) ++ ":" ++ show(c)
-- tokenPosn (TokenField   (AlexPn a l c) s t) = show(l) ++ ":" ++ show(c)
-- tokenPosn (TokenLabel   (AlexPn a l c))     = show(l) ++ ":" ++ show(c)
-- tokenPosn (TokenStartID (AlexPn a l c))     = show(l) ++ ":" ++ show(c)
-- tokenPosn (TokenEndID   (AlexPn a l c))     = show(l) ++ ":" ++ show(c)
-- tokenPosn (TokenType    (AlexPn a l c))     = show(l) ++ ":" ++ show(c)


}