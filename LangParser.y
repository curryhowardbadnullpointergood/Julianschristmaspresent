{
module LangParser where
import LangLexer
}

%name langParser
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
Query
    : Read Matches {Query $1 (reverse $2)}

Matches
    : Match Matches {$1 : $2}
    | Match         {[$1]}


Read
    : read string {ReadFile $2}

Match
    : match Patterns where Conditions Return    {MatchWhere $2 $4 $5}
    | match Patterns Return                     {Match $2 $3}

Patterns
    : name "-" "->" Patterns        {PatternRelatedTo $1 $4}
    | name "-" name "->" Patterns   {PatternRelatedToVar $1 $3 $5}
    | name "<-" "-" Patterns        {PatternRelatedBy $1 $4}
    | name "<-" name "-" Patterns   {PatternRelatedByVar $1 $3 $5}
    | name "-" "-" Patterns         {PatternRelated $1 $4}
    | name "-" name "-" Patterns    {PatternRelatedVar $1 $3 $5}
    | name                          {PatternFinal $1} 

-- Where
--     : where Conditions {}

Conditions
    : Condition or Conditions   {WhereConditionOr $1 $3}
    | Condition and Conditions  {WhereConditionAnd $1 $3}
    | not Conditions            {WhereConditionNot $2}
    | Condition                 {WhereCondition $1}


Condition
    : name "." string intField IntCondition     {IntWhereCondition $1 $3 $5} 
    | name "." string strField StrCondition     {StrWhereCondition $1 $3 $5} 
    | name "." string boolField BoolCondition   {BoolWhereCondition $1 $3 $5} 
    | name labelField StrCondition              {LabelWhereCondition $1 $3} 
    | name typeField StrCondition               {TypeWhereCondition $1 $3} 

IntCondition
    : "<" int   {Greater ( $2)} 
    | ">" int   {Less ( $2)} 
    | "<=" int  {LessOrEqual ( $2)} 
    | ">=" int  {GreaterOrEqual ( $2)} 
    | "==" int  {IntEqual ( $2)} 
    | "/=" int  {IntNotEqual ($2)} 
    | "==" null {IntIsNull} 
    | "/=" null {IntNotNull} 


StrCondition
    : starts string {StringStarts ( $2)} 
    | "==" string   {StrEqual ( $2)} 
    | "/=" string   {StrNotEqual ( $2)} 
    | "==" null     {StrIsNull} 
    | "/=" null     {StrNotNull} 

BoolCondition
    : "==" true     {BoolEqual ( True)} 
    | "==" false    {BoolEqual ( False)} 
    | "/=" true     {BoolNotEqual ( True)}
    | "/=" false    {BoolNotEqual ( False)}
    | "==" null     {BoolIsNull} 
    | "/=" null     {BoolNotNull} 

Return
    : return Outputs {Return $2}

Outputs
    : Output "," Outputs    {$1 : $3}
    | Output                {[$1]}

Output
    : name "." string intField as string    {IntOutput $1 $3 $6}  
    | name "." string strField as string    {StrOutput $1 $3 $6}  
    | name "." string boolField as string   {BoolOutput $1 $3 $6}
    | name idField                          {IdOutput $1}
    | name startField                       {StartOutput $1}
    | name endField                         {EndOutput $1}
    | name labelField                       {LabelOutput $1} 



{

parseError :: [LangToken] -> a
parseError [] = error "uknown error"
parseError ts = error $ "Parse error at line " ++ (show ln) ++ " column " ++ (show col) ++ " previous character " ++ (show ch) ++ " tokens " ++ (show ts)
    where LTok p cl = head ts
          AlexPn ch ln col = p

data Query 
    = Query ReadFile Matches
    deriving (Eq, Show)

data ReadFile
    = ReadFile String
    deriving (Show, Eq)

type Matches
    = [Match]

data Match
    = MatchWhere Patterns WhereConditions Return
    | Match Patterns Return     
    deriving (Eq, Show)

data Patterns 
    = PatternFinal String
    | PatternRelated String Patterns
    | PatternRelatedVar String String Patterns
    | PatternRelatedTo String Patterns
    | PatternRelatedToVar String String Patterns
    | PatternRelatedBy String Patterns
    | PatternRelatedByVar String String Patterns
    deriving (Eq, Show)


data WhereConditions
    = WhereConditionOr WhereCondition WhereConditions
    | WhereConditionAnd WhereCondition WhereConditions 
    | WhereConditionNot WhereConditions      
    | WhereCondition WhereCondition 
    deriving (Eq, Show)

data WhereCondition
    = IntWhereCondition String String IntCondition
    | StrWhereCondition String String StrCondition
    | BoolWhereCondition String String BoolCondition
    | LabelWhereCondition String StrCondition
    | TypeWhereCondition String StrCondition
    deriving (Eq, Show)

data IntCondition
    = Greater ( Int)
    | Less ( Int)
    | GreaterOrEqual ( Int)
    | LessOrEqual ( Int)
    | IntEqual ( Int)
    | IntNotEqual ( Int)
    | IntIsNull
    | IntNotNull
    deriving (Eq, Show)

data StrCondition
    = StringStarts ( String)
    | StrEqual ( String)
    | StrNotEqual ( String)
    | StrIsNull
    | StrNotNull
    deriving (Eq, Show)

data BoolCondition
    = BoolEqual ( Bool)
    | BoolNotEqual ( Bool)
    | BoolIsNull
    | BoolNotNull
    deriving (Eq, Show)

data Return
    = Return Outputs
    deriving (Eq, Show)

type Outputs
    = [Output]

data Output
    = StrOutput String String String 
    | IntOutput String String String 
    | BoolOutput String String String 
    | IdOutput String
    | StartOutput String
    | EndOutput String
    | LabelOutput String
    deriving (Eq, Show)

}