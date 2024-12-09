open CoreAst
open Utils

let rec string_of_datatype dt = 
  match dt with 
  | Int -> "int"
  | Float -> "float"
  | Bool -> "bool"
  | Char -> "char"
  | Void -> "void"
  | Pointer t -> "*" ^ string_of_datatype t
;;

let string_of_param p = 
  let (dt, id) = p in 
  unwords " " [string_of_datatype dt; id]

let string_of_params ps = unwords ", " (List.map string_of_param ps);; 

let string_of_expression e = 
  match e with 
  | StringValue s -> "expr_string " ^ s 
  | IntValue _ -> "expr_int"
  | FloatValue _ -> "expr_float"
  | VariableExpr _ -> "expr_var"
  | BinOpExpr _ -> "expr_binop"
  | PlusPlusExpr _ -> "expr_++"
  | UnaOpExpr _ -> "expr_unaop"
  | FuncCallExpr _ -> "expr_funccall"
  | StmtsExpr _ -> "expr_statements"

let rec string_of_variable v = 
  match v with 
  | VarIden id -> id 
  | VarAccess (v, e) -> (string_of_variable v) ^ "[" ^ string_of_expression e ^ "]"
;;

let string_of_variables vs = unwords ", " (List.map string_of_variable vs)

let rec string_of_statement s = 
  match s with 
  | DeclarationStmt (dt, vs)  ->"(declare) " ^ (string_of_datatype dt) ^ " " ^ (string_of_variables vs)
  | AssignmentStmt (v, e)     -> "(assign) " ^ (string_of_variable v) ^ " = " ^ (string_of_expression e)
  | ExpressionStmt e          -> "(expression) " ^ (string_of_expression e)
  | IfStmt _                  -> "(if) "
  | WhileStmt _               -> "(while) "
  | ReturnStmt e              -> "(return) " ^ (string_of_option string_of_expression e)
  | StmtsStmt s               -> "(stmts) " ^ (unwords " " (List.map string_of_statement s))
;;

let string_of_scope sc = "{\n" ^ unwords "\n" (List.map string_of_statement sc) ^ "\n}";;

let string_of_funcdecl f = 
  let (dt, id, ps, sc) = f in
  unwords " " [(string_of_datatype dt); id; "("; (string_of_params ps); ")"; (string_of_scope sc)]
;;

let rec stringify program = 
  match program with 
  | []   -> "EOF"
  | h::t -> (string_of_funcdecl h) ^ "\n\n" ^ stringify t
;;
