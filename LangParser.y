{
module GqlParser where
import GqlLexer
}

%name gqlParser
%tokentype {LangToken}
%error {parseError}

%left or and
%nonassoc not 

%token
"."         {LTok _ LTokenFullStop}
","         {LTok _ LTokenComma}
-- ":"         {LTok _ LTokenColon}
"-"         {LTok _ LTokenRelated}
"->"        {LTok _ LTokenRelatedRight}
"<-"        {LTok _ LTokenRelatedLeft}
intField    {LTok _ LTokenIntField}
strField    {LTok _ LTokenStrField}
boolField   {LTok _ LTokenBoolField}
labelField  {LTok _ LTokenLabelField}
-- ""    {LTok _ LTokenStartIDField}
-- ""    {LTok _ LTokenEndIDField}
typeField   {LTok _ LTokenTypeField}

or          {LTok _ LTokenOr}
and         {LTok _ LTokenAnd}
not         {LTok _ LTokenNot}
read        {LTok _ LTokenRead}
match       {LTok _ LTokenMatch}
where       {LTok _ LTokenWhere}
return      {LTok _ LTokenReturn}
as          {LTok _ LTokenAs}
starts      {LTok _ LTokenStartWith}

"("         {LTok _ LTokenLParen}
")"         {LTok _ LTokenRParen}
"["         {LTok _ LTokenLBrack}
"]"         {LTok _ LTokenRBrack}

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


                        
%%
-- Query
--     : read string Matches Return {Query $2 (reverse $3) $4}

-- Matches
--     : Matches Match {}
--     | Match {}

Query
    : Read Match Return {Query $1 $2 $3}

Read
    : read string {Read $2}

Match
    : match Patterns Where  {}
    | match Patterns        {}

Patterns
    : "(" name ")" "-" "->" Patterns                {}
    | "(" name ")" "-" "[" name "]" "->" Patterns   {}
    | "(" name ")" "<-" "-"              Patterns   {}
    | "(" name ")" "<-" "[" name "]" "-" Patterns   {}
    | "(" name ")" "-" "-"               Patterns   {}
    | "(" name ")" "-" "(" name ")" "-"  Patterns   {}
    | "(" name ")"                                  {} 

Where
    : where Conditions {}

Conditions
    : Conditions or Condition   {}
    | Conditions and Condition  {}
    | not Condition             {}
    | Condition                 {}


Condition
    : name "." name intField IntCondition int       {} 
    | name "." name strField StrCondition string    {} 
    | name "." name boolField BoolCondition true    {} 
    | name "." name boolField BoolCondition false   {} 
    | name labelField StrCondition string           {} 
    | name typeField StrCondition string            {} 

IntCondition
    : "<"       {Greater} 
    | ">"       {Less} 
    | "<="      {GreaterOrEqual} 
    | ">="      {LessOrEqual} 
    | "=="      {IntEqual} 
    | "==" null {IntNotEqual} 
    | "/="      {IntIsNull} 
    | "/=" null {IntNotNull} 

StrCondition
    : starts    {StringStarts} 
    | "=="      {StrEqual} 
    | "==" null {StrIsNull} 
    ||"/="      {StrNotEqual} 
    | "/=" null {StrNotNull} 

BoolCondition
    : "=="      {BoolEqual} 
    | "==" null {BoolIsNull} 
    | "/="      {BoolNotEqual} 
    | "/=" null {BoolNotNull} 

Return
    : return Outputs {}

Outputs
    : Outputs "," Output    {}
    | Output                {}

Output
    : name "." string intField as string    {}  
    | name "." string strField as string    {}  
    | name "." string boolField as string   {}  
    | name labelField                       {} 



{

parseError :: [LangToken] -> a
parseError [] = error "uknown error"
parseError ts = error $ "Parse error at line " ++ (show ln) ++ " column " ++ (show col) ++ " previous character " ++ (show ch) ++ " tokens " ++ (show ts)
    where LTok p cl = head ts
          AlexPn ch ln col = p

data Query 
    = Query Read Match Return
    deriving (Eq, Show)

data Read
    = Read String
    deriving (Show, Eq)

type Match
    = [Patterns]

data Pattern 
    = Pattern String
    | PatternRelation String
    | PatternRelationVar String String


data IntCondition
    | Greater
    | Less
    | GreaterOrEqual
    | LessOrEqual
    | IntEqual
    | IntNotEqual
    | IntIsNull
    | IntNotNull
    deriving (Eq, Show)

data StrCondition
    = StringStarts
    | StrEqual
    | StrNotEqual
    | StrIsNull
    | StrNotNull
    deriving (Eq, Show)

data BoolCondition
    = BoolEqual
    | BoolNotEqual
    | BoolIsNull
    | BoolNotNull
    deriving (Eq, Show)

}