{
module Parser where
import Lexer
}

%name inputParser
%tokentype { Token }
%error { parseError }

%token
    ","         { Tok _ TokenComma }
    ":"         { Tok _ TokenColon }    
    ";"         { Tok _ TokenSemiColon }
    -- "\n"        { Tok _ TokenNewLine }
    strVal      { Tok _ (TokenStrVal $$) }
    intVal      { Tok _ (TokenIntVal $$) }    
    boolVal     { Tok _ (TokenBoolVal $$) }
    nullVal     { Tok _ TokenNullVal }
    string      { Tok _ (TokenString $$) }
    ":ID"       { Tok _ TokenID }    
    type        { Tok _ (TokenFieldType $$) }
    ":LABEL"    { Tok _ TokenLabel }
    ":START_ID" { Tok _ TokenStartID }
    ":END_ID"   { Tok _ TokenEndID }    
    ":TYPE"     { Tok _ TokenType }


%%

-- File                : NodeSets "\n" RelationshipSets                {File $1 $3}

-- NodeSets            : NodeSets "\n" NodeSet                         {NodeSets $3 $1}             
-- -- NodeSets            : NodeSet "\n" NodeSets                         {NodeSets $1 $3}             
--                     | NodeSet                                       {NodeSets $1 EmptyNodeSet}

-- NodeSet             : NodeHeader "\n" NodeEntries                   {NodeSet $1 $3}


-- NodeHeader          : ":ID" "," Fields                              {NodeHeader $3 False}
--                     | ":ID" "," Fields "," ":LABEL"                 {NodeHeader $3 True}


-- Fields              : Fields "," Field                              {Fields $3 $1}            
-- -- Fields              : Field "," Fields                              {Fields $1 $3}            
--                     | Field                                         {Fields $1 EmptyField}
-- Field               : string ":" type                               {Field $1 $3}


-- NodeEntries         : NodeEntries "\n" NodeEntry                    {NodeEntries $3 $1}                
-- -- NodeEntries         : NodeEntry "\n" NodeEntries                    {NodeEntries $1 $3}                
--                     | NodeEntry                                     {NodeEntries $1 EmptyNodeEntry}


-- NodeEntry           : string "," Literals "," Labels                {NodeEntryLabel $1 $3 $5}
--                     | string "," Literals                           {NodeEntry $1 $3}


-- Literals            : Literals "," Literal                          {Literals $3 $1}
-- -- Literals            : Literal "," Literals                          {Literals $1 $3}
--                     | Literal                                       {Literals $1 EmptyLiteral}


-- Literal             : strVal                                        {LiteralStr $1}
--                     | intVal                                        {LiteralInt $1}
--                     | boolVal                                       {LiteralBool $1}
--                     | nullVal                                       {LiteralNull}

-- Labels              : Labels ";" Label                              {Labels $3 $1}           
-- -- Labels              : Label ";" Labels                              {Labels $1 $3}           
--                     | Label                                         {Labels $1 EmptyLabel}
-- Label               : string                                        {Label $1}

-- -- RelationshipSets    : RelationshipSet "\n" RelationshipSets         {RelationshipSets $1 $3}        
-- RelationshipSets    : RelationshipSets "\n" RelationshipSet         {RelationshipSets $3 $1}  
--                     | RelationshipSet                               {RelationshipSets $1 EmptyRelationshipSet}

-- RelationshipSet     : RelationshipHeader "\n" RelationshipEntries   {RelationshipSet $1 $3}
-- RelationshipHeader  : ":START_ID" "," Fields "," ":END_ID" "," ":TYPE" {RelationshipHeader $3}


-- RelationshipEntries : RelationshipEntries "\n" RelationshipEntry    {RelationshipEntries $3 $1}
-- -- RelationshipEntries : RelationshipEntry "\n" RelationshipEntries    {RelationshipEntries $1 $3}
--                     | RelationshipEntry                             {RelationshipEntries $1 EmptyRelationshipEntry}


-- RelationshipEntry : string "," Literals "," string "," string       {RelationshipEntry $1 $3 $5 $7}



File                : NodeSets RelationshipSets                {File $1 $2}

NodeSets            : NodeSets NodeSet                         {NodeSets $2 $1}             
                    | NodeSet                                       {NodeSets $1 EmptyNodeSet}

NodeSet             : NodeHeader NodeEntries                   {NodeSet $1 $2}


NodeHeader          : ":ID" "," Fields                              {NodeHeader $3 False}
                    | ":ID" "," Fields "," ":LABEL"                 {NodeHeader $3 True}
                    | ":ID"                                         {NodeHeader EmptyField False}
                    | ":ID" "," ":LABEL"                            {NodeHeader EmptyField True}


Fields              : Fields "," Field                              {Fields $3 $1}            
                    | Field                                         {Fields $1 EmptyField}
Field               : string ":" type                               {Field $1 $3}


NodeEntries         : NodeEntries NodeEntry                         {NodeEntries $2 $1}                
                    | NodeEntry                                     {NodeEntries $1 EmptyNodeEntry}


NodeEntry           : string "," Literals "," Labels                {NodeEntryLabel $1 $3 $5}
                    | string "," Literals                           {NodeEntry $1 $3}
                    | string "," Labels                             {NodeEntryLabel $1 EmptyLiteral $3}
                    | string                                        {NodeEntry $1 EmptyLiteral}


Literals            : Literals "," Literal                          {Literals $3 $1}
                    | Literal                                       {Literals $1 EmptyLiteral}

Literal             : strVal                                        {LiteralStr $1}
                    | intVal                                        {LiteralInt $1}
                    | boolVal                                       {LiteralBool $1}
                    | nullVal                                       {LiteralNull}

Labels              : Labels ";" Label                              {Labels $3 $1}           
                    | Label                                         {Labels $1 EmptyLabel}
Label               : string                                        {Label $1}

RelationshipSets    : RelationshipSets RelationshipSet                  {RelationshipSets $2 $1}  
                    | RelationshipSet                                   {RelationshipSets $1 EmptyRelationshipSet}

RelationshipSet     : RelationshipHeader RelationshipEntries            {RelationshipSet $1 $2}

RelationshipHeader  : ":START_ID" "," Fields "," ":END_ID" "," ":TYPE"  {RelationshipHeader $3}
                    | ":START_ID" "," ":END_ID" "," ":TYPE"             {RelationshipHeader EmptyField}

RelationshipEntries : RelationshipEntries RelationshipEntry             {RelationshipEntries $2 $1}
                    | RelationshipEntry                                 {RelationshipEntries $1 EmptyRelationshipEntry}


RelationshipEntry   : string "," Literals "," string "," string         {RelationshipEntry $1 $3 $5 $7}
                    | string "," string "," string                      {RelationshipEntry $1 EmptyLiteral $3 $5}


{
parseError :: [Token] -> a
parseError [] = error "uknown error"
parseError ts = error $ "Parse error at line " ++ (show ln) ++ " column " ++ (show col) ++ " previous character " ++ (show ch) ++ " tokens " ++ (show ts)
    where Tok p cl = head ts
          AlexPn ch ln col = p



data File   = File NodeSets RelationshipSets
            deriving Show

data NodeSets   = NodeSets NodeSet NodeSets
                | EmptyNodeSet
                deriving Show
data NodeSet    = NodeSet NodeHeader NodeEntries
                deriving Show
-- Bool in nodeheader describes whether there is a Label field
data NodeHeader = NodeHeader Fields Bool
                deriving Show
data Fields     = Fields Field Fields
                | EmptyField
                deriving Show
data Field      = Field String FieldType
                deriving Show


data NodeEntries    = NodeEntries NodeEntry NodeEntries 
                    | EmptyNodeEntry
                    deriving Show
data NodeEntry      = NodeEntryLabel String Literals Labels
                    | NodeEntry String Literals
                    deriving Show
data Literals       = Literals Literal Literals
                    | EmptyLiteral
                    deriving Show
data Literal        = LiteralStr String
                    | LiteralInt Int
                    | LiteralBool Bool
                    | LiteralNull
                    deriving Show
data Labels         = Labels Label Labels
                    | EmptyLabel
                    deriving Show
data Label          = Label String
                    deriving Show

data RelationshipSets       = RelationshipSets RelationshipSet RelationshipSets
                            | EmptyRelationshipSet
                            deriving Show
data RelationshipSet        = RelationshipSet RelationshipHeader RelationshipEntries
                            deriving Show
data RelationshipHeader     = RelationshipHeader Fields
                            deriving Show
data RelationshipEntries    = RelationshipEntries RelationshipEntry RelationshipEntries
                            | EmptyRelationshipEntry
                            deriving Show
-- Relationship entry is IDs, (fields)*, StartID, EndID
data RelationshipEntry      = RelationshipEntry String Literals String String
                            deriving Show

}


