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
    : Read string Matches {Query $2 (reverse $3)}

Matches
    : Match Matches {$1 : $2}
    | Match         {[$1]}


Read
    : read string {ReadFile $2}

Match
    : match Patterns Where Return   {MatchWhere $2 $3 $4}
    | match Patterns Return         {Match $2 $3}

Patterns
    : "(" name ")" "-" "->" Patterns                {PatternRelatedTo $1 $4}
    | "(" name ")" "-" "[" name "]" "->" Patterns   {PatternRelatedToVar $1 $3 $5}
    | "(" name ")" "<-" "-"              Patterns   {PatternRelatedBy $1 $4}
    | "(" name ")" "<-" "[" name "]" "-" Patterns   {PatternRelatedByVar $1 $3 $5}
    | "(" name ")" "-" "-"               Patterns   {PatternRelated $1 $4}
    | "(" name ")" "-" "(" name ")" "-"  Patterns   {PatternRelatedVar $1 $3 $5}
    | "(" name ")"                                  {PatternFinal $1} 

Where
    : where Conditions {}

Conditions
    : Condition or Conditions   {WhereConditionOr $1 $3}
    | Condition and Conditions  {WhereConditionAnd $1 $3}
    | not Conditions            {WhereConditionNot $2}
    | Condition                 {WhereCondition $1}


Condition
    : name "." name intField IntCondition int       {IntWhereCondition $1 $3 $4 $5 $6} 
    | name "." name strField StrCondition string    {StrWhereCondition $1 $3 $4 $5 $6} 
    | name "." name boolField BoolCondition true    {BoolWhereCondition $1 $3 $4 $5 $6} 
    | name "." name boolField BoolCondition false   {BoolWhereCondition $1 $3 $4 $5 $6} 
    | name labelField StrCondition string           {LabelWhereCondition $1 $3 $4} 
    | name typeField StrCondition string            {TypeWhereCondition $1 $3 $4} 

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
    | "/="      {StrNotEqual} 
    | "/=" null {StrNotNull} 

BoolCondition
    : "=="      {BoolEqual} 
    | "==" null {BoolIsNull} 
    | "/="      {BoolNotEqual} 
    | "/=" null {BoolNotNull} 

Return
    : return Outputs {Return $2}

Outputs
    : Output "," Outputs    {$1 : $3}
    | Output                {[$1]}

Output
    : name "." string intField as string    {StrOutput $1 $3 $5}  
    | name "." string strField as string    {IntOutput $1 $3 $5}  
    | name "." string boolField as string   {BoolOutput $1 $3 $5}
    | name idField                          {IdOutput $1 $2}
    | name startField                       {StartOutput $1 $2}
    | name endField                         {EndOutput $1 $2}
    | name labelField                       {LabelOutput $1 $2} 



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
    = MatchWhere Patterns Where Return
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

data Where
    = Where WhereConditions

data WhereConditions
    = WhereConditionOr WhereCondition WhereConditions
    | WhereConditionAnd WhereCondition WhereConditions 
    | WhereConditionNot WhereCondition WhereConditions      
    | WhereCondition WhereCondition 
    deriving (Eq, Show)

data WhereCondition
    = IntWhereCondition String String IntCondition Int
    | StrWhereCondition String String StrCondition String
    | BoolWhereConditon String String BoolCondition Bool
    | LabelWhereCondition String StrCondition String
    | TypeWhereCondition String StrCondition String
    deriving (Eq, Show)

data IntCondition
    = Greater
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

data Return
    = Return Outputs
    deriving (Eq, Show)

type Outputs
    = [Output]

data Output
    = StrOutput String String String 
    | IntOutput String String String 
    | BoolOutput String String String 
    | IdOutput String String 
    | StartOutput String String
    | EndOutput String String
    | LabelOutput String String
    deriving (Eq, Show)

}