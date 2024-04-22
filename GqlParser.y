{
module GqlParser where
import GqlLexer
}

%name gqlParser
%tokentype {LangToken}
%error {parseError}


%token
    ";"             {LTok _ (LTokenSemiColon)}
    "="             {LTok _ (LTokenAssignment)}
    "&&"            {LTok _ (LTokenAnd)}       
    "||"            {LTok _ (LTokenOr)}   

    "=="            {LTok _ (LTokenEquals)}
    "/="            {LTok _ (LTokenNotEquals)}

    "<="            {LTok _ (LTokenLessThanEqual)}
    ">="            {LTok _ (LTokenGreaterThanEqual)}
    "<"             {LTok _ (LTokenLessThan)}
    ">"             {LTok _ (LTokenGreaterThan)}

    index           {LTok _ (LTokenIndex $$)}  

    "("             {LTok _ (LTokenLParen)}
    ")"             {LTok _ (LTokenRParen)}

    ":integer"      {LTok _ (LTokenIntFieldType)}
    ":string"       {LTok _ (LTokenStrFieldType)}
    ":bool"         {LTok _ (LTokenBoolFieldType)}
    ":LABEL"        {LTok _ (LTokenLabelField)}

    read            {LTok _ (LTokenRead)}
    output          {LTok _ (LTokenOutputVar)}

    getNodes        {LTok _ (LTokenGetNodes)}
    getRelations    {LTok _ (LTokenGetRelations)}

    filterField     {LTok _ (LTokenFilterField)}
    filterRelations {LTok _ (LTokenFilterRelations)}

    file            {LTok _ (LTokenTypeFile)}
    nodes           {LTok _ (LTokenTypeNodes)}
    relations       {LTok _ (LTokenTypeRelations)}
                    
    str             {LTok _ (LTokenString $$)}
    int             {LTok _ (LTokenInt $$)}
    true            {LTok _ (LTokenTrue)}       
    false           {LTok _ (LTokenFalse)}

    var             {LTok _ (LTokenVar $$)}


                        
%%
Program 		: Assignments FinalAssignment {Program (reverse $1) $2}
                | FinalAssignment {Program [] $1}

Assignments     : Assignments Assignment  {$2 : $1}
                | Assignment {[$1]}


Assignment 	: Type var "=" Operation ";" {Assignment $1 $2 $4}
FinalAssignment : Type output "=" Operation ";" {FinalAssignment $1 $4}


Operation 		: read str {OpRead $2}
                | filterField var str ":integer"    "(" IntCondition int")"     {OpFilterIntField $2 $3 ($6, $7)}
                | filterField var str ":string" "(" StrCondition str")"     {OpFilterStringField $2 $3 ($6, $7)}
                | filterField var str ":bool"   "(" BoolCondition true")"   {OpFilterBoolField $2 $3 ($6, True)}
                | filterField var str ":bool"   "(" BoolCondition false")"  {OpFilterBoolField $2 $3 ($6, False)}
				| filterField var     ":LABEL"  "(" StrCondition str")"     {OpFilterLabel $2 ($5, $6)}
                | getNodes var {OpGetNodes $2}
                | getRelations var {OpGetRelations $2}
                -- | filterRelations {} 

				| var "&&" var  {OpIntersection $1 $3}
				| var "||" var {OpUnion $1 $3}
				| var {OpVar $1}


IntCondition    : "<" {Greater}
                | ">" {Less}
                | "<=" {GreaterOrEqual}
                | ">=" {LessOrEqual}
				| "==" {IntEqual}
				| "/=" {IntNotEqual}
StrCondition    : index int {Index $2}
                | "==" {StrEqual}
				| "/=" {StrNotEqual}
BoolCondition   : "==" {BoolEqual}
				| "/=" {BoolNotEqual}

Type 			: file {TypeFile}
				| nodes {TypeNodes}
				| relations {TypeRelations}


{
parseError :: [LangToken] -> a
parseError [] = error "uknown error"
parseError ts = error $ "Parse error at line " ++ (show ln) ++ " column " ++ (show col) ++ " previous character " ++ (show ch) ++ " tokens " ++ (show ts)
    where LTok p cl = head ts
          AlexPn ch ln col = p

data Program 
    = Program Assignments FinalAssignment
    deriving (Eq, Show)

type Assignments 
    = [Assignment]

-- Type varname operation
data Assignment 
    = Assignment Type String Operation            
    deriving (Eq, Show)

data FinalAssignment 
    = FinalAssignment Type Operation
    deriving (Eq, Show)

-- First String in each operation is a variable name
data Operation  
    = OpVar String
    | OpUnion String String
    | OpIntersection String String
    | OpRead String

    | OpGetNodes String
    | OpGetRelations String
    | OpFilterIntField String String (IntCondition, Int) 
    | OpFilterStringField String String (StrCondition, String)
    | OpFilterBoolField String String (BoolCondition, Bool)
    | OpFilterLabel String (StrCondition, String)
    deriving (Eq, Show)

data IntCondition   
    = Greater
    | Less
    | GreaterOrEqual
    | LessOrEqual
    | IntEqual
    | IntNotEqual
    deriving (Eq, Show)

data StrCondition   
    = Index Int 
    | StrEqual
    | StrNotEqual
    deriving (Eq, Show)

data BoolCondition  
    = BoolEqual
    | BoolNotEqual
    deriving (Eq, Show)

data Type   
    = TypeFile
    | TypeNodes
    | TypeRelations
    deriving (Eq, Show)

}