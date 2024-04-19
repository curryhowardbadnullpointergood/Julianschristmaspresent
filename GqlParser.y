{
module GqlParser where
import GqlLexer
}

%name gqlParser
%tokentype {LangToken}
%error {parseError}


%nonassoc '<' '>' '<=' '>=' '==' '/='
%nonassoc '=' ';' 
%nonassoc '(' ')'

%left '&&' '||'
%left filterField filterLabel filterRelation
%left getRelations getField getLabels

%token

    ";"             {TokenSemiColon}
    "="             {TokenAssignment}
    "<="            {TokenLessThanEqual}
    ">="            {TokenGreaterThanEqual}
    "<"             {TokenLessThan}
    ">"             {TokenGreaterThan}
    "=="            {TokenEquals}
    "/="            {TokenNotEquals}

                -- {TokenIndex Int}
                -- {TokenLength}

    "&&"            {TokenAnd}       
    "||"            {TokenOr}       
    true            {TokenTrue}       
    false           {TokenFalse}      

    -- { TokenLambdaArrow}
    -- { TokenLamdaBackSlash}

    "("             {TokenLParen}
    ")"             {TokenRParen}

    ":int"          {TokenIntFieldType}
    ":string"       {TokenStrFieldType}
    ":bool"         {TokenBoolFieldType}

    read            {TokenRead}
    -- { TokenGetIntField}
    -- { TokenGetStrField}
    -- { TokenGetBoolField}
    getField        {TokenGetField}
    getLabels       {TokenGetLabels}
    getRelations    {TokenGetRelations}
    
    filterLabel     {TokenFilterRelation}
    filterField     {TokenFilterField}
    filterRelations {TokenFilterRelations}

    file            {TokenTypeFile}
    nodes           {TokenTypeNodes}
    relations       {TokenTypeRelations}
                    
    str             {TokenString $$}
    int             {TokenInt $$}
    var             {TokenVar $$}

    output          {TokenOutputVar}


%%
Program 		: Assignments Type output "=" Operation ";" {}
				| Type output ";" {}

Assignments 	: Type var "=" Operation ";" {}
				| Assignments Type var "=" Operation ";" {}

Operation 		: read str {}
                | filterField var str ":int" Lambda {}
                | filterField var str ":string" Lambda {}
                | filterField var str ":bool" Lambda {}
				| filterLabel var Lambda {}
                -- | filterRelations {} 
                | getField var str {}
                | getLabels var {}
                -- | getRelations var {}
				| var "&&" var  {}
				| var "||" var {}
				| var {}

Lambda 			: "(" Lambda1 ")" {}
Lambda1			: Condition str {}
				| Condition int {}
				| Condition true {}
                | Condition false {}


Condition 		: "<" {}
				| ">" {}
				| "<=" {}
				| ">=" {}
				| "==" {}
				| "/=" {}


Type 			: file {}
				| nodes {}
				| relations {}


{
parseError :: [LangToken] -> a
parseError [] = error "uknown error"
parseError ts = error $ "Parse error at line " ++ (show ln) ++ " column " ++ (show col) ++ " previous character " ++ (show ch) ++ " tokens " ++ (show ts)
    where Tok p cl = head ts
          AlexPn ch ln col = p


}