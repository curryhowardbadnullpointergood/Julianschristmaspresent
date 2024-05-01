{
module LangParser where
import LangLexer
import InputParser (Literal(..))
}

%name langParser
%tokentype {LangToken}
%error {parseError}


%token    
-- General Stuff
"="         {LTok _ LTokenAssignment}
"."         {LTok _ LTokenFullStop}
    
-- Read Stuff
read        {LTok _ LTokenRead}
    
-- Match Stuff
match       {LTok _ LTokenMatch}
"-"         {LTok _ LTokenRelated}
"->"        {LTok _ LTokenRelatedRight}
"<-"        {LTok _ LTokenRelatedLeft}
    
-- Where Stuff
where       {LTok _ LTokenWhere}

or          {LTok _ LTokenOr}
and         {LTok _ LTokenAnd}
not         {LTok _ LTokenNot}

is          {LTok _ LTokenIs}

starts      {LTok _ LTokenStartWith}
ends        {LTok _ LTokenEndWith}

"("         {LTok _ LTokenLParen}
")"         {LTok _ LTokenRParen}

"<="        {LTok _ LTokenLessThanEqual}
">="        {LTok _ LTokenGreaterThanEqual}
"<"         {LTok _ LTokenLessThan}
">"         {LTok _ LTokenGreaterThan}
"=="        {LTok _ LTokenEquals}
"/="        {LTok _ LTokenNotEquals}
    
-- Outputting Stuff
return      {LTok _ LTokenReturn}
append      {LTok _ LTokenAppend}
delete      {LTok _ LTokenDelete}
update      {LTok _ LTokenUpdate}

","         {LTok _ LTokenComma}
"|"         {LTok _ LTokenBar}
as          {LTok _ LTokenAs}

add         {LTok _ LTokenAdd}
minus       {LTok _ LTokenMinus}

"--"        {LTok _ LTokenNewRelation}

    -- Values 
labelField  {LTok _ LTokenLabelField}
idField     {LTok _ LTokenIdField}
startField  {LTok _ LTokenStartField}
endField    {LTok _ LTokenEndField}
typeField   {LTok _ LTokenTypeField}

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
    : Read Match Where Print {Query $1 $2 $3 $4}
---------------------------------------------------------------------------------------------------
-- Read
---------------------------------------------------------------------------------------------------
Read
    : read "=" string {ReadFile $3}
---------------------------------------------------------------------------------------------------
-- Match
---------------------------------------------------------------------------------------------------
Match
    : match "=" Patterns {Match $3}

Patterns
    : Pattern "|" Patterns  {$1 : $3}
    | Pattern               {[$1]}

Pattern
    : name "-"  name "->" name      {PatternRelatedTo $1 $3 $5}
    | name "<-" name "-"  name      {PatternRelatedBy $1 $3 $5}
    | name                          {Pattern $1} 
---------------------------------------------------------------------------------------------------
-- Where
---------------------------------------------------------------------------------------------------
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
    : name idField          {WDot $1 ":ID"}
    | name typeField        {WDot $1 ":TYPE"}
    | name startField       {WDot $1 ":START_ID"}
    | name endField         {WDot $1 ":END_ID"}
    | name labelField       {WDot $1 ":LABEL"}
    | name "." name         {WDot $1 $3}


WhereLit
    : string                    {WStr $1}
    | int                       {WInt $1}
    | true                      {WBool True}
    | false                     {WBool False}
    | null                      {WNull}
---------------------------------------------------------------------------------------------------
-- Print
---------------------------------------------------------------------------------------------------
Print
    : Update Delete Return      {Print1 $1 $2 $3}
    | Update Delete Append      {Print2 $1 $2 $3}
    | Update Return             {Print3 $1 $2}
    | Update Append             {Print4 $1 $2}
    | Delete Return             {Print5 $1 $2}
    | Delete Append             {Print6 $1 $2}
    | Return                    {Print7 $1}
    | Append                    {Print8 $1}

---------------------------------------------------------------------------------------------------
-- Return + Append
---------------------------------------------------------------------------------------------------
Return
    : return "=" Print1             {Return $3}

Append
    : append "=" Print1             {Append $3}

Print1
    : PrintExps "|" Print1          {$1 : $3}
    | PrintExps                     {[$1]}

PrintExps
    : PrintExp "," PrintExps        {$1 : $3}
    | PrintExp                      {[$1]}

PrintExp
    : name "." name as string       {Output $1 $3          $5}  
    | name labelField               {Output $1 ":LABEL"    ":LABEL"}
    | name typeField                {Output $1 ":TYPE"     ":TYPE"} 
    | name startField               {Output $1 ":START_ID" ":START_ID"} 
    | name endField                 {Output $1 ":END_ID"   ":END_ID" } 
    | name idField                  {Output $1 ":ID"        ":ID"}
    | PDot "--" string "--" PDot    {NewRelation (fst $1) (snd $1) $3 (fst $5) (snd $5)} 

PDot
    : name labelField   {($1,":LABEL")}
    | name typeField    {($1,":TYPE")}
    | name startField   {($1,":START_ID")}
    | name endField     {($1,":END_ID")}
    | name idField      {($1,":ID")}

---------------------------------------------------------------------------------------------------
-- Update
---------------------------------------------------------------------------------------------------
Update
    : update "=" Update1         {Update $3}

Update1 
    : UpdateExp "|" Update1         {$1 : $3}
    | UpdateExp                     {[$1]}

UpdateExp
    : name "." name add   int           "=" name "." name     {UAdd      $1 $3 $5 $7 $9}
    | name "." name add   name "." name "=" name "." name     {UAddDot   $1 $3 $5 $7 $9 $11}


    | name "." name minus int           "=" name "." name     {UMinus    $1 $3 $5 $7 $9}
    | name "." name minus name "." name "=" name "." name     {UMinusDot $1 $3 $5 $7 $9 $11}
---------------------------------------------------------------------------------------------------
-- Delete
---------------------------------------------------------------------------------------------------
Delete
    : delete "=" Delete1            {Delete $3}

Delete1
    : DeleteExp "|" Delete1         {$1 : $3}
    | DeleteExp                     {[$1]}

DeleteExp
    : name "." name   int           {Del $1 $3  (show $4)}
    | name "." name   string        {Del $1 $3  $4}
    | name "." name   true          {Del $1 $3  "True"}
    | name "." name   false         {Del $1 $3  "False"}
    | name "." name   null          {Del $1 $3  "null"}
    
    | name idField    string        {Del $1 ":ID"       $3}
    | name typeField  string        {Del $1 ":TYPE"     $3}
    | name startField string        {Del $1 ":START_ID" $3}
    | name endField   string        {Del $1 ":END_ID"   $3}
    | name labelField string        {Del $1 ":LABEL"    $3}

{

parseError :: [LangToken] -> a
parseError [] = error "uknown error"
parseError ts = error $ "Parse error at line " ++ (show ln) ++ " column " ++ (show col) ++ " previous character " ++ (show ch) ++ " tokens " ++ (show ts)
    where LTok p cl = head ts
          AlexPn ch ln col = p

data Query 
    = Query ReadFile Match Where Print
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
    = Pattern String
    | PatternRelatedTo String String String
    | PatternRelatedBy String String String
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


data Print
    = Print1 Update Delete Return
    | Print2 Update Delete Append
    | Print3 Update Return
    | Print4 Update Append
    | Print5 Delete Return
    | Print6 Delete Append
    | Print7 Return
    | Print8 Append
    deriving (Eq, Show)

data Return 
    = Return [PrintExps]
    deriving (Eq, Show)

data Append
    = Append [PrintExps]
    deriving (Eq, Show)

type PrintExps
    = [PrintExp]

data PrintExp
    = Output String String String
    | NewRelation String String String String String
    deriving (Eq, Show)

data Update
    = Update [UpdateExp]
    deriving (Eq, Show)

data UpdateExp
    = UAdd String String Int String String 
    | UAddDot String String String String String String
    | UMinus String String Int String String 
    | UMinusDot String String String String String String
    deriving (Eq, Show)

data Delete
    = Delete [DeleteExp]
    deriving (Eq, Show)

data DeleteExp
    = Del String String String
    deriving (Eq, Show)

}