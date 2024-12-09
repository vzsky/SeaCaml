type compound_operators = 
  | Com_PlusEq | Com_MinusEq 

type variable_rev = 
  | VarIden of CoreAst.identifier 
  | VarAccess of variable_rev * expression

and expression = 
  | StringValue of string
  | IntValue of int
  | FloatValue of float
  | VariableExpr of variable_rev 
  | BinOpExpr of expression * CoreAst.bin_operator * expression
  | PlusPlusExpr of variable_rev
  | UnaOpExpr of CoreAst.una_operator * expression
  | FuncCallExpr of CoreAst.identifier * expression list
(* more expressions! *)
  | CompoundExpr of variable_rev * compound_operators * expression

and statement = 
  | DeclarationStmt of CoreAst.datatype * variable_rev list
  | AssignmentStmt of variable_rev * expression
  | ExpressionStmt of expression
  | IfStmt of expression * scope
  | ReturnStmt of expression option
  | WhileStmt of expression * scope
(* more statements *)
  | DeclareAssignStmt of CoreAst.datatype * (variable_rev * expression) list 
  | ForStmt of statement * expression * statement * scope

and scope = statement list

type function_decl = CoreAst.datatype * CoreAst.identifier * CoreAst.parameter list * scope

type program = function_decl list
