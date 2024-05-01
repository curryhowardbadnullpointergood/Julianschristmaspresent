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
"--"        {LTok _ LTokenNewRelation}
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
return      {LTok _ LTokenReturn}
append      {LTok _ LTokenAppend}

as          {LTok _ LTokenAs}
starts      {LTok _ LTokenStartWith}
ends        {LTok _ LTokenEndWith}
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
    : Read Match Where Return{Query $1 $2 $3 $4}

Read
    : read "=" string {ReadFile $3}
    

Match
    : match "=" Patterns {Match $3}

Patterns
    : Pattern "|" Patterns  {$1 : $3}
    | Pattern               {[$1]}

Pattern
    : name "-" "->" name        {PatternRelatedTo $1 $4}
    | name "-" name "->" name   {PatternRelatedToVar $1 $3 $5}
    | name "<-" "-" name        {PatternRelatedBy $1 $4}
    | name "<-" name "-" name   {PatternRelatedByVar $1 $3 $5}
    | name "-" "-" name         {PatternRelated $1 $4}
    | name "-" name "-" name    {PatternRelatedVar $1 $3 $5}
    | name                      {PatternFinal $1} 




Where
    : where "=" WhereExp1   {Where $3}


WhereExp1
    : WhereFunc and WhereExp1   {WAnd $1 $3}
    | WhereFunc or WhereExp1    {WOr $1 $3}
    | not WhereExp1             {WNot $2}
    | "(" WhereExp1 ")"         {$2}
    | WhereFunc                 {WFinal $1}

WhereFunc
    : WhereDot "=="   WhereLit    {WEqual $1 $3}
    | WhereDot "/="   WhereLit    {WNotEqual $1 $3}
    | WhereDot "<"    WhereLit    {WLessThan $1 $3}
    | WhereDot ">"    WhereLit    {WGreaterThan $1 $3}
    | WhereDot "<="   WhereLit    {WLessOrEqualThan $1 $3}
    | WhereDot ">="   WhereLit    {WGreaterOrEqualThan $1 $3}
    | WhereDot starts WhereLit    {WStartsWith $1 $3}
    | WhereDot ends   WhereLit    {WEndsWith $1 $3}

    | WhereDot "=="   WhereDot    {WEqualDot $1 $3}
    | WhereDot "/="   WhereDot    {WNotEqualDot $1 $3}
    | WhereDot "<"    WhereDot    {WLessThanDot $1 $3}
    | WhereDot ">"    WhereDot    {WGreaterThanDot $1 $3}
    | WhereDot "<="   WhereDot    {WLessOrEqualThanDot $1 $3}
    | WhereDot ">="   WhereDot    {WGreaterOrEqualThanDot $1 $3}
    | WhereDot starts WhereDot    {WStartsWithDot $1 $3}
    | WhereDot ends   WhereDot    {WEndsWithDot $1 $3}

WhereDot 
    : name idField              {WDot $1 ":ID"}
    | name typeField            {WDot $1 ":TYPE"}
    | name startField           {WDot $1 ":START_ID"}
    | name endField             {WDot $1 ":END_ID"}
    | name labelField           {WDot $1 ":LABEL"}
    | name "." name             {WDot $1 $3}


WhereLit
    : string                    {WStr $1}
    | int                       {WInt $1}
    | true                      {WBool True}
    | false                     {WBool False}
    | null                      {WNull}


Return
    : getNode "=" Return1 getRelation "=" Return1   {ReturnNodeRelation $3 $6}
    | getNode "=" Return1                           {ReturnNode $3}
    | getRelation "=" Return1                       {ReturnRelation $3}

Return1
    : Outputs "|" Return1   {$1 : $3}
    | Outputs               {[$1]}

Outputs
    : Output "," Outputs    {$1 : $3}
    | Output                {[$1]}

Output
    : name "." name as string  {Output $1 $3 $5}  
    | name labelField          {Output $1 ":LABEL"    ":LABEL"}
    | name typeField           {Output $1 ":TYPE"     ":TYPE"} 
    | name startField          {Output $1 ":START_ID" ":START_ID"} 
    | name endField            {Output $1 ":END_ID"   ":END_ID" } 
    | name idField             {Output $1 ":ID"        ":ID"}
-- Output
--     : name "." name intField as string  {Output $1 $3 $6}  
--     | name "." name strField as string  {Output $1 $3 $6}  
--     | name "." name boolField as string {Output $1 $3 $6}
--     | name labelField                   {Output ":LABEL" $1} 
--     | name type                         {Output ":TYPE" $1} 
--     | name startField                   {Output ":START_ID" $1} 
--     | name endField                     {Output ":END_ID" $1} 

{

parseError :: [LangToken] -> a
parseError [] = error "uknown error"
parseError ts = error $ "Parse error at line " ++ (show ln) ++ " column " ++ (show col) ++ " previous character " ++ (show ch) ++ " tokens " ++ (show ts)
    where LTok p cl = head ts
          AlexPn ch ln col = p

data Query 
    = Query ReadFile Match Where Return
    deriving (Eq, Show)

data ReadFile
    = ReadFile String
    deriving (Show, Eq)

data Match
    = Match Patterns
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
    = WEqual WhereDot WhereLit
    | WNotEqual WhereDot WhereLit
    | WLessThan WhereDot WhereLit
    | WGreaterThan WhereDot WhereLit
    | WLessOrEqualThan WhereDot WhereLit
    | WGreaterOrEqualThan WhereDot WhereLit
    | WStartsWith WhereDot WhereLit
    | WEndsWith WhereDot WhereLit

    | WEqualDot WhereDot WhereDot
    | WNotEqualDot WhereDot WhereDot
    | WLessThanDot WhereDot WhereDot
    | WGreaterThanDot WhereDot WhereDot
    | WLessOrEqualThanDot WhereDot WhereDot
    | WGreaterOrEqualThanDot WhereDot WhereDot
    | WStartsWithDot WhereDot WhereDot
    | WEndsWithDot WhereDot WhereDot
    deriving (Show, Eq)

data WhereDot
    = WDot String String
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
    = Output String String String
    | NewRelation String String String String String
    deriving (Eq, Show)
}