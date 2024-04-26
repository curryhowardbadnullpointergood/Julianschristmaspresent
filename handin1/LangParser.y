{
module LangParser where
import LangLexer
import InputParser (Literal(..))
}

%name langParser
%tokentype {LangToken}
%error {parseError}


%token
"."         {LTok _ LTokenFullStop}
","         {LTok _ LTokenComma}
-- ":"         {LTok _ LTokenColon}
"|"         {LTok _ LTokenBar}
"="         {LTok _ LTokenAssignment}
"-"         {LTok _ LTokenRelated}
"->"        {LTok _ LTokenRelatedRight}
"<-"        {LTok _ LTokenRelatedLeft}
intField    {LTok _ LTokenIntField}
strField    {LTok _ LTokenStrField}
boolField   {LTok _ LTokenBoolField}
labelField  {LTok _ LTokenLabelField}
idField     {LTok _ LTokenIdField}
startField  {LTok _ LTokenStartField}
endField    {LTok _ LTokenEndField}
typeField   {LTok _ LTokenTypeField}

or          {LTok _ LTokenOr}
and         {LTok _ LTokenAnd}
not         {LTok _ LTokenNot}
read        {LTok _ LTokenRead}
match       {LTok _ LTokenMatch}
where       {LTok _ LTokenWhere}
getNode     {LTok _ LTokenGetNode}
getRelation {LTok _ LTokenGetRelation}

-- return      {LTok _ LTokenReturn}
as          {LTok _ LTokenAs}
starts      {LTok _ LTokenStartWith}

"("         {LTok _ LTokenLParen}
")"         {LTok _ LTokenRParen}
-- "["         {LTok _ LTokenLBrack}
-- "]"         {LTok _ LTokenRBrack}

"<="        {LTok _ LTokenLessThanEqual}
">="        {LTok _ LTokenGreaterThanEqual}
"<"         {LTok _ LTokenLessThan}
">"         {LTok _ LTokenGreaterThan}
"=="        {LTok _ LTokenEquals}
"/="        {LTok _ LTokenNotEquals}

null        {LTok _ LTokenNull}
true        {LTok _ LTokenTrue}
false       {LTok _ LTokenFalse}
string      {LTok _ (LTokenString $$)}
int         {LTok _ (LTokenInt $$)}
name        {LTok _ (LTokenName $$)}


%left "."
%left "starts" "<" ">" "<=" ">=" "==" "/=" 
%left and or 
%nonassoc "(" ")"
%nonassoc not 

                        
%%
Query
    : Read Match {Query $1 $2}

Read
    : read "=" string {ReadFile $3}
    

Match
    : match "=" Patterns Where Return   {Match $3 $4 $5}

Patterns
    : Pattern "|" Patterns  {$1 : $3}
    | Pattern           {[$1]}

Pattern
    : name "-" "->" name        {PatternRelatedTo $1 $4}
    | name "-" name "->" name   {PatternRelatedToVar $1 $3 $5}
    | name "<-" "-" name        {PatternRelatedBy $1 $4}
    | name "<-" name "-" name   {PatternRelatedByVar $1 $3 $5}
    | name "-" "-" name         {PatternRelated $1 $4}
    | name "-" name "-" name    {PatternRelatedVar $1 $3 $5}
    | name                      {PatternFinal $1} 

Return
    : getNode "=" Return1 getRelation "=" Return1   {ReturnNodeRelation $3 $6}
    | getNode "=" Return1                       {ReturnNode $3}
    | getRelation "=" Return1                   {ReturnRelation $3}

Return1
    : Outputs "|" Return1   {$1 : $3}
    | Outputs               {[$1]}

Outputs
    : Output "," Outputs    {$1 : $3}
    | Output                {[$1]}

Output
    : name "." name intField as string    {IntOutput $1 $3 $6}  
    | name "." name strField as string    {StrOutput $1 $3 $6}  
    | name "." name boolField as string   {BoolOutput $1 $3 $6}
    | name labelField                     {LabelOutput $1} 



Where
    : where "=" WhereExp1    {Where $3}


WhereExp1
    : WhereFunc and WhereExp1   {WAnd $1 $3}
    | WhereFunc or WhereExp1    {WOr $1 $3}
    | not WhereExp1             {WNot $2}
    | "(" WhereExp1 ")"         {$2}
    | WhereFunc                 {WFinal $1}

WhereFunc
    : WhereDot "==" WhereLit      {WEqual $1 $3}
    | WhereDot "/=" WhereLit      {WNotEqual $1 $3}
    | WhereDot "<"  WhereLit      {WLessThan $1 $3}
    | WhereDot ">"  WhereLit      {WGreaterThan $1 $3}
    | WhereDot "<=" WhereLit      {WLessOrEqualThan $1 $3}
    | WhereDot ">=" WhereLit      {WGreaterOrEqualThan $1 $3}
    | WhereDot starts WhereLit    {WStartsWith $1 $3}

    | WhereDot "==" WhereDot      {WEqualDot $1 $3}
    | WhereDot "/=" WhereDot      {WNotEqualDot $1 $3}
    | WhereDot "<"  WhereDot      {WLessThanDot $1 $3}
    | WhereDot ">"  WhereDot      {WGreaterThanDot $1 $3}
    | WhereDot "<=" WhereDot      {WLessOrEqualThanDot $1 $3}
    | WhereDot ">=" WhereDot      {WGreaterOrEqualThanDot $1 $3}
    | WhereDot starts WhereDot    {WStartsWithDot $1 $3}

WhereDot 
    : name idField          {WDot $1 WId}
    | name typeField        {WDot $1 WType}
    | name startField       {WDot $1 WStartField}
    | name endField         {WDot $1 WEndField}
    | name labelField       {WDot $1 WLabelField}
    | name "." name intField    {WDot $1 (WFieldName $3)}
    | name "." name strField    {WDot $1 (WFieldName $3)}
    | name "." name boolField   {WDot $1 (WFieldName $3)}

WhereLit
    : string                    {LiteralStr $1}
    | null                      {LiteralNull}
    | int                       {LiteralInt $1}
    | true                      {LiteralBool True}
    | false                     {LiteralBool False}

{

parseError :: [LangToken] -> a
parseError [] = error "uknown error"
parseError ts = error $ "Parse error at line " ++ (show ln) ++ " column " ++ (show col) ++ " previous character " ++ (show ch) ++ " tokens " ++ (show ts)
    where LTok p cl = head ts
          AlexPn ch ln col = p

data Query 
    = Query ReadFile Match
    deriving (Eq, Show)

data ReadFile
    = ReadFile String
    deriving (Show, Eq)

data Match
    = Match Patterns Where Return
    deriving (Eq, Show)

type Patterns
    = [Pattern]

data Pattern 
    = PatternFinal String
    | PatternRelated String String
    | PatternRelatedVar String String String
    | PatternRelatedTo String String
    | PatternRelatedToVar String String String
    | PatternRelatedBy String String
    | PatternRelatedByVar String String String
    deriving (Eq, Show)


data Where
    = Where WhereExp
    deriving (Show, Eq)

data WhereExp
    = WAnd WhereFunc WhereExp 
    | WOr WhereFunc WhereExp 
    | WNot WhereExp
    | WFinal WhereFunc
    deriving (Show, Eq)

data WhereFunc
    = WEqual WhereDot Literal
    | WNotEqual WhereDot Literal
    | WLessThan WhereDot Literal
    | WGreaterThan WhereDot Literal
    | WLessOrEqualThan WhereDot Literal
    | WGreaterOrEqualThan WhereDot Literal
    | WStartsWith WhereDot Literal
    
    | WEqualDot WhereDot WhereDot
    | WNotEqualDot WhereDot WhereDot
    | WLessThanDot WhereDot WhereDot
    | WGreaterThanDot WhereDot WhereDot
    | WLessOrEqualThanDot WhereDot WhereDot
    | WGreaterOrEqualThanDot WhereDot WhereDot
    | WStartsWithDot WhereDot WhereDot
    deriving (Show, Eq)

data WhereDot
    = WDot String WhereDotOptions
    deriving (Show, Eq)

data WhereDotOptions
    = WFieldName String
    | WId
    | WLabelField
    | WStartField
    | WEndField
    | WType
    deriving (Show, Eq)

data WhereLit
    = WStr String
    | WInt Int
    | WBool Bool
    | WNull
    deriving (Show, Eq)

data Return
    = ReturnNode [Outputs]
    | ReturnNodeRelation [Outputs] [Outputs]
    | ReturnRelation [Outputs]
    deriving (Eq, Show)

type Outputs
    = [Output]

data Output
    = StrOutput String String String 
    | IntOutput String String String 
    | BoolOutput String String String 
    -- | IdOutput String
    -- | StartOutput String
    -- | EndOutput String
    | LabelOutput String
    deriving (Eq, Show)

}