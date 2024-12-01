type identifier = string

type datatype = 
  | Int 
  | Float 
  | Char
  | Bool
  | Void
  | Pointer of datatype
[@@deriving compare]

type parameter = datatype * identifier

type bin_operator = 
  | BOP_Plus | BOP_Minus | BOP_Mult | BOP_Div 
  | BOP_Equal | BOP_Lt | BOP_Gt | BOP_Lte | BOP_Gte
  | BOP_LAnd | BOP_LOr

type una_operator = 
  | UOP_Not | UOP_Neg | UOP_And

type variable = 
  | VarIden of identifier 
  | VarAccess of variable * expression

and expression = 
  | StringValue of string
  | IntValue of int
  | FloatValue of float
  | VariableExpr of variable 
  | BinOpExpr of expression * bin_operator * expression
  | PlusPlusExpr of variable
  | UnaOpExpr of una_operator * expression
  | FuncCallExpr of identifier * expression list

and statement = 
  | DeclarationStmt of datatype * variable list
  | AssignmentStmt of variable * expression
  | ExpressionStmt of expression
  | IfStmt of expression * scope
  | ForStmt of statement * expression * statement * scope
  | ReturnStmt of expression option

and scope = statement list

type function_decl = datatype * identifier * parameter list * scope

type program = function_decl list
