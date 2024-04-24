{
module LangParser where
import LangLexer
}

%name langParser
%tokentype {LangToken}
%error {parseError}


%token
"."         {LTok _ LTokenFullStop}
","         {LTok _ LTokenComma}
-- ":"         {LTok _ LTokenColon}
"|"         {LTok _ LTokenBar}

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


%left "."
%left "starts" "<" ">" "<=" ">=" "==" "/=" 
%left and or 
%nonassoc "(" ")"
%nonassoc not 

                        
%%
Query
    : Read Match {Query $1 $2}

Read
    : read string {ReadFile $2}
    

-- Matches
--     : Match Matches {$1 : $2}
--     | Match         {[$1]}


Match
    : match Patterns Where Return   {MatchWhere $2 $3 $4}
    | match Patterns Return         {Match $2 $3}

Patterns
    : Pattern Patterns  {$1 : $2}
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
    : getNode Return1 labelField getRelation Return1    {ReturnNodeRelation $2 $5 True}
    | getNode Return1 labelField                        {ReturnNode $2 True}
    | getNode Return1 getRelation Return1               {ReturnNodeRelation $2 $4 False}
    | getNode Return1                                   {ReturnNode $2 False}


Return1
    : Outputs "|" Return1   {$1 : $3}
    | Outputs               {[$1]}

Outputs
    : Output "," Outputs    {$1 : $3}
    | Output                {[$1]}

Output
    : name "." string intField as string    {IntOutput $1 $3 $6}  
    | name "." string strField as string    {StrOutput $1 $3 $6}  
    | name "." string boolField as string   {BoolOutput $1 $3 $6}
    -- | name idField                          {IdOutput $1}
    -- | name startField                       {StartOutput $1}
    -- | name endField                         {EndOutput $1}
    | name labelField                       {LabelOutput $1} 



Where
    : where WhereExp0    {Where $2}

WhereExp0
    : "(" WhereExp1 ")"         {$2}
    | WhereExp1                 {$1}

WhereExp1
    : WhereExp1 and WhereFunc   {WAnd $1 $3}
    | WhereExp1 or WhereFunc    {WOr $1 $3}
    | not WhereExp1             {WNot $2}
    | WhereFunc                 {$1}

WhereFunc
    : WhereDot "==" WhereLit      {WEqual $1 $3}
    | WhereDot "/=" WhereLit      {WNotEqual $1 $3}
    | WhereDot "<"  WhereLit      {WLessThan $1 $3}
    | WhereDot ">"  WhereLit      {WGreaterThan $1 $3}
    | WhereDot "<=" WhereLit      {WLessOrEqualThan $1 $3}
    | WhereDot ">=" WhereLit      {WGreaterOrEqualThan $1 $3}
    | WhereDot starts WhereLit    {WStartsWith $1 $3}
    | WhereDot                     {$1}

WhereDot 
    : name "." idField          {WDot $1 WId}
    | name "." typeField        {WDot $1 WType}
    | name "." startField       {WDot $1 WStartField}
    | name "." endField         {WDot $1 WEndField}
    | name "." labelField       {WDot $1 WLabelField}
    | name "." name             {WDot $1 (WFieldName $3)}
    | WhereLit                  {$1}

WhereLit
    : string                    {WStr $1}
    | null                      {WNull}
    | int                       {WInt $1}
    | true                      {WBool True}
    | false                     {WBool False}

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

-- type Matches
--     = [Match]

data Match
    = MatchWhere Patterns Where Return
    | Match Patterns Return     
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
    = WAnd WhereExp WhereExp
    | WOr WhereExp WhereExp
    | WNot WhereExp

data WhereFunc
    = WEqual WhereDot WhereVal
    | WNotEqual WhereDot WhereVal
    | WLessThan WhereDot WhereVal
    | WGreaterThan WhereDot WhereVal
    | WLessOrEqualThan WhereDot WhereVal
    | WGreaterOrEqualThan WhereDot WhereVal
    | WStartsWith WhereDot WhereVal
    
    | WEqualDot WhereDot WhereDot
    | WNotEqualDot WhereDot WhereDot
    | WLessThanDot WhereDot WhereDot
    | WGreaterThanDot WhereDot WhereDot
    | WLessOrEqualThanDot WhereDot WhereDot
    | WGreaterOrEqualThanDot WhereDot WhereDot
    | WStartsWithDot WhereDot WhereDot

data WhereDot
    = WDot String WhereDotOptions

data WhereDotOptions
    = WFieldName String
    | WId
    | WLabelField
    | WStartField
    | WEndField
    | WType

data WhereVal
    | WStr String
    | WInt Int
    | WBool Bool
    | WNull
    deriving (Show, Eq)

data Return
    = ReturnNode [Outputs] Bool
    | ReturnNodeRelation [Outputs] [Outputs] Bool
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