open Ast

let unwords s l = String.concat s l;;

let rec string_of_charlist l =
  match l with
    [] -> ""
  | h::t -> String.make 1 h ^ string_of_charlist t;;

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
  unwords " " [string_of_datatype dt; string_of_charlist id]

let string_of_params ps = unwords ", " (List.map string_of_param ps);; 

let string_of_expression _ = "expression" ;;

let rec string_of_variable v = 
  match v with 
  | VarIden id -> string_of_charlist id 
  | VarAccess (v, e) -> (string_of_variable v) ^ "[" ^ string_of_expression e ^ "]"
;;

let string_of_variables vs = unwords ", " (List.map string_of_variable vs)

let string_of_statement s = 
  match s with 
  | DeclarationStmt (dt, vs)  ->"(declare) " ^ (string_of_datatype dt) ^ " " ^ (string_of_variables vs)
  | AssignmentStmt (v, e)     -> "(assign) " ^ (string_of_variable v) ^ " = " ^ (string_of_expression e)
  | ExpressionStmt _          -> "(expression)"
  | IfStmt _                  -> "(if) "
  | ForStmt _                 -> "(for) "
  | ReturnStmt e              -> "(return) " ^ (string_of_expression e)
;;

let string_of_scope sc = "{\n" ^ unwords "\n" (List.map string_of_statement sc) ^ "\n}";;

let string_of_funcdecl f = 
  let (dt, id, ps, sc) = f in
  unwords " " [(string_of_datatype dt); (string_of_charlist id); "("; (string_of_params ps); ")"; (string_of_scope sc)]
;;

let rec stringify program = 
  match program with 
  | []   -> "EOF"
  | h::t -> (string_of_funcdecl h) ^ "\n\n" ^ stringify t
;;
