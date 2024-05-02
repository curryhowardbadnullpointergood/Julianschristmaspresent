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
File                    : NodeSets RelationshipSets                         {postComputateFile $ File (reverse $1) (reverse $2)}
                        | NodeSets                                          {postComputateFile $ File (reverse $1) []}
                        | RelationshipSets                                  {postComputateFile $ File [] (reverse $1)}
                        |                                                   {postComputateFile $ File [] []}


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


NodeEntries             : NodeEntries INodeEntry                             {$2 : $1}                
                        | INodeEntry                                         {[$1]}



INodeEntry               : string "," Literals "," Labels                   {INodeEntry $1 (reverse $3) (reverse $5)}
                        | string "," Literals                               {INodeEntry $1 (reverse $3) []}
                        | string "," Labels                                 {INodeEntry $1 [] (reverse $3)}
                        | string                                            {INodeEntry $1 [] []}


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

RelationshipEntries     : RelationshipEntries IRelationshipEntry             {$2 : $1}
                        | IRelationshipEntry                                 {[$1]}


IRelationshipEntry      : string "," Literals "," string "," string        {IRelationshipEntry $1 $3 $5 $7}
                        | string "," string "," string                      {IRelationshipEntry $1 [] $3 $5}


{
parseError :: [InputToken] -> a
parseError [] = error "unknown error"
parseError ts = error $ "Parse error at line " ++ (show ln) ++ " column " ++ (show col) ++ " previous character " ++ (show ch) ++ " tokens " ++ (show ts)
    where Tok p cl = head ts
          AlexPn ch ln col = p

data File       = File NodeSets RelationshipSets
                deriving (Show,Eq)

data NodeSet    = NodeSet NodeHeader NodeEntries
                deriving (Show,Eq)

type NodeSets   = [NodeSet]


-- Bool in nodeheader describes whether there is a Label field
data NodeHeader = NodeHeader Fields Bool
                deriving (Show,Eq)

type Fields     = [Field]
data Field      = Field String TokenFieldType
                deriving (Show,Eq)


type NodeEntries    = [INodeEntry]
data INodeEntry      = INodeEntry String Literals Labels
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
type RelationshipEntries    = [IRelationshipEntry] 
-- Relationship entry is IDs, (fields)*, StartID, EndID
data IRelationshipEntry      = IRelationshipEntry String Literals String String
                            deriving (Show,Eq)


data DataType
    = TypeString
    | TypeInt
    | TypeBool
    | TypeNull
    deriving (Show, Eq)

type FieldEntry    = (String, String, DataType)
type NodeEntry     = (String, String, DataType)
type RelationEntry = (String, String, DataType)
type Node          = [NodeEntry]
type Relation      = [RelationEntry]
type Nodes         = [Node]
type Relations     = [Relation]
-- First list is node second is relation 
postComputateFile :: File -> (Nodes,Relations)
postComputateFile (File nodeSets relationshipSets) 
    = (postComputateNodeSets nodeSets,postComputateRelationshipSets relationshipSets)

postComputateNodeSets :: [NodeSet] -> Nodes
postComputateNodeSets []                   = []
postComputateNodeSets (nodeSet:nodeSets)   = postComputateNodeSet nodeSet ++ postComputateNodeSets nodeSets


postComputateNodeSet :: NodeSet -> Nodes
postComputateNodeSet (NodeSet (NodeHeader fields _) entries) 
    = postComputateNodeEntries fields entries

postComputateNodeEntries :: Fields -> NodeEntries -> Nodes
postComputateNodeEntries _ [] = []
postComputateNodeEntries fields (nodeEntry:nodeEntries) 
    = postComputateNodeEntry fields nodeEntry : postComputateNodeEntries fields nodeEntries

postComputateNodeEntry :: Fields -> INodeEntry -> Node
postComputateNodeEntry fields (INodeEntry id literals labels) 
    = [(":ID", id, TypeString)] ++ postComputateLiterals fields literals ++ postComputateLabels labels 

postComputateLabels :: Labels -> [NodeEntry]
postComputateLabels []                   = []
postComputateLabels ((Label str) : labels) 
    = (":LABEL",str,TypeString) : postComputateLabels labels

postComputateRelationshipSets :: [RelationshipSet] -> Relations
postComputateRelationshipSets [] = []
postComputateRelationshipSets (relationshipSet : relationshipSets)  
    = postComputateRelationshipSet relationshipSet ++ postComputateRelationshipSets relationshipSets

postComputateRelationshipSet :: RelationshipSet -> Relations
postComputateRelationshipSet (RelationshipSet (RelationshipHeader fields) relationshipEntries)
    = postComputateRelationshipEntries fields relationshipEntries

postComputateRelationshipEntries :: Fields -> RelationshipEntries -> Relations
postComputateRelationshipEntries _ [] = []
postComputateRelationshipEntries fields (relationshipEntry : relationshipEntries) 
    = postComputateRelationshipEntry fields relationshipEntry : postComputateRelationshipEntries fields relationshipEntries

postComputateRelationshipEntry :: Fields -> IRelationshipEntry -> Relation
postComputateRelationshipEntry fields (IRelationshipEntry start literals end t)
    = [(":START_ID", start, TypeString)] ++ postComputateLiterals fields literals ++ [(":END_ID", end, TypeString)] ++ [(":TYPE", t, TypeString)]

-- postComputateLiterals :: Fields -> Literals -> [FieldEntry]
-- postComputateLiterals []                                        []                              = []
-- postComputateLiterals ((Field fieldName TTypeString)  : fields) ((LiteralStr str)   : literals) = (fieldName, str,       TypeString) : postComputateLiterals fields literals
-- postComputateLiterals ((Field fieldName TTypeInteger) : fields) ((LiteralInt int)   : literals) = (fieldName, show int,  TypeInt)    : postComputateLiterals fields literals
-- postComputateLiterals ((Field fieldName TTypeBoolean) : fields) ((LiteralBool bool) : literals) = (fieldName, show bool, TypeBool)   : postComputateLiterals fields literals
-- postComputateLiterals ((Field fieldName _)            : fields) (LiteralNull        : literals) = (fieldName, "null",    TypeNull)   : postComputateLiterals fields literals
-- postComputateLiterals _ _ = error "Invalid n4j input"

postComputateLiterals :: Fields -> Literals -> [FieldEntry]
postComputateLiterals []                                        []                              = []
postComputateLiterals ((Field fieldName TTypeString)  : fields) ((LiteralStr str)   : literals) = (fieldName, str,       TypeString) : postComputateLiterals fields literals
postComputateLiterals ((Field fieldName TTypeInteger) : fields) ((LiteralInt int)   : literals) = (fieldName, show int,  TypeInt)    : postComputateLiterals fields literals
postComputateLiterals ((Field fieldName TTypeBoolean) : fields) ((LiteralBool bool) : literals) = (fieldName, show bool, TypeBool)   : postComputateLiterals fields literals

postComputateLiterals ((Field fieldName TTypeString)  : fields) ((LiteralNull)      : literals) = (fieldName, "null",    TypeString)   : postComputateLiterals fields literals
postComputateLiterals ((Field fieldName TTypeInteger) : fields) ((LiteralNull)      : literals) = (fieldName, "null",    TypeInt)      : postComputateLiterals fields literals
postComputateLiterals ((Field fieldName TTypeBoolean) : fields) ((LiteralNull)      : literals) = (fieldName, "null",    TypeBool)     : postComputateLiterals fields literals

postComputateLiterals _ _ = error "Invalid n4j input"


}





