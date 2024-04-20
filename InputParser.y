{
module InputParser where
import InputLexer
}

%name inputParser
%tokentype {InputToken}
%error {parseError}

%token
    ","         {Tok _ TokenComma}
    ":"         {Tok _ TokenColon}    
    ";"         {Tok _ TokenSemiColon}
    strVal      {Tok _ (TokenStrVal $$)}
    intVal      {Tok _ (TokenIntVal $$)}    
    boolVal     {Tok _ (TokenBoolVal $$)}
    nullVal     {Tok _ TokenNullVal}
    string      {Tok _ (TokenString $$)}
    ":ID"       {Tok _ TokenID}    
    type        {Tok _ (TokenFieldType $$)}
    ":LABEL"    {Tok _ TokenLabel}
    ":START_ID" {Tok _ TokenStartID}
    ":END_ID"   {Tok _ TokenEndID}    
    ":TYPE"     {Tok _ TokenType}


%%
File                    : NodeSets RelationshipSets                         {File (reverse $1) (reverse $2)}


NodeSets                : NodeSets NodeSet                                  {$2 : $1}             
                        | NodeSet                                           {[$1]}

NodeSet                 : NodeHeader NodeEntries                            {NodeSet $1 (reverse $2)}


NodeHeader              : ":ID" "," Fields                                  {NodeHeader (reverse $3) False}
                        | ":ID" "," Fields "," ":LABEL"                     {NodeHeader (reverse $3) True}
                        | ":ID"                                             {NodeHeader [] False}
                        | ":ID" "," ":LABEL"                                {NodeHeader [] True}


Fields                  : Fields "," Field                                  {$3 : $1}            
                        | Field                                             {[$1]}

Field                   : string ":" type                                   {Field $1 $3}


NodeEntries             : NodeEntries NodeEntry                             {$2 : $1}                
                        | NodeEntry                                         {[$1]}



NodeEntry               : string "," Literals "," Labels                    {NodeEntry $1 (reverse $3) (reverse $5)}
                        | string "," Literals                               {NodeEntry $1 (reverse $3) []}
                        | string "," Labels                                 {NodeEntry $1 [] (reverse $3)}
                        | string                                            {NodeEntry $1 [] []}


Literals                : Literals "," Literal                              {$3 : $1}
                        | Literal                                           {[$1]}

Literal                 : strVal                                            {LiteralStr $1}
                        | intVal                                            {LiteralInt $1}
                        | boolVal                                           {LiteralBool $1}
                        | nullVal                                           {LiteralNull}

Labels                  : Labels ";" Label                                  {$3 : $1}           
                        | Label                                             {[$1]}
Label                   : string                                            {Label $1}

RelationshipSets        : RelationshipSets RelationshipSet                  {$2 : $1}  
                        | RelationshipSet                                   {[$1]}

RelationshipSet         : RelationshipHeader RelationshipEntries            {RelationshipSet $1 (reverse $2)}

RelationshipHeader      : ":START_ID" "," Fields "," ":END_ID" "," ":TYPE"  {RelationshipHeader (reverse $3)}
                        | ":START_ID" "," ":END_ID" "," ":TYPE"             {RelationshipHeader []}

RelationshipEntries     : RelationshipEntries RelationshipEntry             {$2 : $1}
                        | RelationshipEntry                                 {[$1]}


RelationshipEntry       : string "," Literals "," string "," string         {RelationshipEntry $1 $3 $5 $7}
                        | string "," string "," string                      {RelationshipEntry $1 [] $3 $5}


{
parseError :: [InputToken] -> a
parseError [] = error "uknown error"
parseError ts = error $ "Parse error at line " ++ (show ln) ++ " column " ++ (show col) ++ " previous character " ++ (show ch) ++ " tokens " ++ (show ts)
    where Tok p cl = head ts
          AlexPn ch ln col = p

data File       = File NodeSets RelationshipSets
                deriving (Show,Eq)

type NodeSets   = [NodeSet]

data NodeSet    = NodeSet NodeHeader NodeEntries
                deriving (Show,Eq)
-- Bool in nodeheader describes whether there is a Label field
data NodeHeader = NodeHeader Fields Bool
                deriving (Show,Eq)

type Fields     = [Field]
data Field      = Field String FieldType
                deriving (Show,Eq)


type NodeEntries    = [NodeEntry]
data NodeEntry      = NodeEntry String Literals Labels
                    deriving (Show,Eq)
type Literals       = [Literal]
data Literal        = LiteralStr String
                    | LiteralInt Int
                    | LiteralBool Bool
                    | LiteralNull
                    deriving (Show,Eq)
type Labels         = [Label] 
data Label          = Label String
                    deriving (Show,Eq)

type RelationshipSets       = [RelationshipSet]
data RelationshipSet        = RelationshipSet RelationshipHeader RelationshipEntries
                            deriving (Show,Eq)
data RelationshipHeader     = RelationshipHeader Fields
                            deriving (Show,Eq)
type RelationshipEntries    = [RelationshipEntry] 
-- Relationship entry is IDs, (fields)*, StartID, EndID
data RelationshipEntry      = RelationshipEntry String Literals String String
                            deriving (Show,Eq)


}

