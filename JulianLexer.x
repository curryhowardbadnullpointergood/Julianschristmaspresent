{
    module JulianLexer where 
}

%wrapper "posn"
%token "Token"


$digit = 0-9			-- digits
$alpha = [a-zA-Z]		-- alphabetic characters
$alphaNumeric = [a-zA-Z0-9] 


tokens :-
$white+        ;
"//".*         ;   -- comments

\=             { tok (\p s -> TokenAssignment p) }
\,             { tok (\p s -> TokenComma p) }
\;             { tok (\p s -> TokenEndStmt p) }

-- Strings 

\"             { tok (\p s -> TokenQuotation p) }

-- Integers 
\+             { tok (\p s -> TokenPlus p) }
\-             { tok (\p s -> TokenMinus p) }
\*             { tok (\p s -> TokenMultiply p) }
\/             { tok (\p s -> TokenDivide p) }
\+             { tok (\p s -> TokenPlus p) }


-- Boolean 
"||"             { tok (\p s -> TokenSOr p) }
"&&"             { tok (\p s -> TokenSAnd p) }
true             { tok (\p s -> TokenTrue p) }
false            { tok (\p s -> TokenFalse p) }

-- Types

Int            { tok (\p s -> TokenTypeInt p) }
String         { tok (\p s -> TokenTypeString p) }
Bool           { tok (\p s -> TokenTypeBool p) }
Float          { tok (\p s -> TokenTypeFloat p) }
Node           { tok (\p s -> TokenTypeNode p) }
RNode          { tok (\p s -> TokenTypeRNode p) }


-- Brackets 
\(             { tok (\p s -> TokenLParen p) }
\)             { tok (\p s -> TokenRParen p) }
\{             { tok (\p s -> TokenLCBracket p) }
\}             { tok (\p s -> TokenRCBracket p) }



$digit+                                 { tok (\p s -> TokenInt p (read s)) }
$alpha [$alpha $digit \_ \’ \-]*        { tok (\p s -> TokenVar p s) }
$alpha [$alpha $digit \_ \’ \- \.]*     { tok (\p s -> TokenString p s) }

-- Language In Built Functions
CONCURRENT             { tok (\p s -> TokenConcurrent p) }
PRINT                  { tok (\p s -> TokenPrint p) }
char_length            { tok (\p s -> TokenCharLength p) }
and                    { tok (\p s -> TokenAnd p) }
not                    { tok (\p s -> TokenNot p) }
or                     { tok (\p s -> TokenOr p) }
coalesce               { tok (\p s -> TokenCoalesce p) }
elementId              { tok (\p s -> TokenElementId p) }
MATCH                  { tok (\p s -> TokenMatch p) }
RETURN                 { tok (\p s -> TokenReturn p) }
endNode                { tok (\p s -> TokenEndNode p) }
head                   { tok (\p s -> TokenHead p) }
ID                     { tok (\p s -> TokenID p) }
last                   { tok (\p s -> TokenLast p) }
length                 { tok (\p s -> TokenLength p) }
nullIf                 { tok (\p s -> TokenNullIf p) }
properties             { tok (\p s -> TokenProperties p) }
randomUUID             { tok (\p s -> TokenRandomUUID p) }
size                   { tok (\p s -> TokenSize p) }
startNode              { tok (\p s -> TokenStartNode p) }
timestamp              { tok (\p s -> TokenTimeStamp p) }
toBoolean              { tok (\p s -> TokenToBoolean p) }
toBooleanOrNull        { tok (\p s -> TokenToBooleanOrNull p) }
toFloat                { tok (\p s -> TokenToFloat p) }
toFloatOrNull          { tok (\p s -> TokenToFloatOrNull p) }
toInteger              { tok (\p s -> TokenToInteger p) }
toIntegerOrNull        { tok (\p s -> TokenToIntegerOrNull p) }
type                   { tok (\p s -> TokenType p) }
valuesType             { tok (\p s -> TokenValuesType p) }
all                    { tok (\p s -> TokenAll p) }
any                    { tok (\p s -> TokenAny p) }
exists                 { tok (\p s -> TokenExists p) }
isEmpty                { tok (\p s -> TokenIsEmpty p) }
none                   { tok (\p s -> TokenNone p) }
single                 { tok (\p s -> TokenSingle p) }
avg                    { tok (\p s -> TokenAvg p) }
collect                { tok (\p s -> TokenCollect p) }
count                  { tok (\p s -> TokenCount p) }
max                    { tok (\p s -> TokenMax p) }
min                    { tok (\p s -> TokenMin p) }
percentileCount        { tok (\p s -> TokenPercentileCount p) }
percentileDisc         { tok (\p s -> TokenPercentileDisc p) }
stdev                  { tok (\p s -> TokenStdev p) }
stdevp                 { tok (\p s -> TokenStdevp p) }
sum                    { tok (\p s -> TokenSum p) }
keys                   { tok (\p s -> TokenKeys p) }
labels                 { tok (\p s -> TokenLables p) }
range                  { tok (\p s -> TokenRange p) }
reduce                 { tok (\p s -> TokenReduce p) }
relationships          { tok (\p s -> TokenRelationships p) }

{
    data TokenClass =

    TokenInt Int                |
    TokenAssignment             |
    TokenComma                  |
    TokenQuotation              |
    TokenPlus                   |
    TokenMinus                  |              
    TokenMultiply               | 
    TokenDivide                 |
    TokenPlus                   |
    TokenSOr                    |
    TokenSAnd                   |
    TokenTrue                   |
    TokenFalse                  |


TokenTypeInt
TokenTypeString 
TokenTypeBool 
TokenTypeFloat 
TokenTypeNode 
TokenTypeRNode 



TokenLParen 
TokenRParen 
TokenLCBracket
TokenRCBracket


TokenInt 
TokenVar 
TokenString 


TokenConcurrent p) }
TokenPrint p) }
TokenCharLength p) }
TokenAnd p) }
TokenNot p) }
TokenOr p) }
TokenCoalesce p) }
TokenElementId p) }
TokenMatch p) }
TokenReturn p) }
TokenEndNode p) }
TokenHead p) }
TokenID p) }
TokenLast p) }
TokenLength p) }
TokenNullIf p) }
TokenProperties p) }
TokenRandomUUID p) }
TokenSize p) }
TokenStartNode p) }
TokenTimeStamp p) }
TokenToBoolean p) }
TokenToBooleanOrNull p) }
TokenToFloat p) }
TokenToFloatOrNull p) }
TokenToInteger p) }
TokenToIntegerOrNull p) }
TokenType p) }
TokenValuesType p) }
TokenAll p) }
TokenAny p) }
TokenExists p) }
TokenIsEmpty p) }
TokenNone p) }
TokenSingle p) }
TokenAvg p) }
TokenCollect p) }
TokenCount p) }
TokenMax p) }
TokenMin p) }
TokenPercentileCount p) }
percentileDisc         { tok (\p s -> TokenPercentileDisc p) }
stdev                  { tok (\p s -> TokenStdev p) }
stdevp                 { tok (\p s -> TokenStdevp p) }
sum                    { tok (\p s -> TokenSum p) }
keys                   { tok (\p s -> TokenKeys p) }
labels                 { tok (\p s -> TokenLables p) }
range                  { tok (\p s -> TokenRange p) }
reduce                 { tok (\p s -> TokenReduce p) }
relationships          { tok (\p s -> TokenRelationships p) }


}

