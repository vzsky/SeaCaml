open Ast

(* for now, it maps CoreAst -> CoreAst *)

(*******************************)
(*********** DESUGAR ***********)
(*******************************)

let rec variable_to_list var = 
  match var with 
  | VarIden id -> ([], id)
  | VarAccess (v, e) -> 
      let ls, id = variable_to_list v in (e::ls, id)

let rec list_to_variable ls id = 
  match ls with 
  | [] -> VarIden id
  | h::t -> VarAccess ((list_to_variable t id), h)

(* This is used for declaration of variable *)
let reverse_variable_order var = 
  let ls, id = variable_to_list var in list_to_variable (List.rev ls) id

let desugar_variable = reverse_variable_order

let rec desugar_expr expr = 
  match expr with 
  | VariableExpr v -> VariableExpr (desugar_variable v)
  | BinOpExpr (a, o, b) -> BinOpExpr (desugar_expr a, o, desugar_expr b)
  | PlusPlusExpr v -> PlusPlusExpr (desugar_variable v)
  | UnaOpExpr (o, e) -> UnaOpExpr (o, desugar_expr e)
  | FuncCallExpr (i, el) -> FuncCallExpr (i, List.map desugar_expr el)
  | e -> e

let rec desugar_stmt stmt = 
  match stmt with
  | DeclarationStmt (dt, vl) -> DeclarationStmt (dt, List.map reverse_variable_order vl)
  | AssignmentStmt (v, e) -> AssignmentStmt (reverse_variable_order v, desugar_expr e)
  | ExpressionStmt e -> ExpressionStmt (desugar_expr e)
  | IfStmt (e, s) -> IfStmt (desugar_expr e, List.map desugar_stmt s)
  | ForStmt (st, e, st2, s) -> ForStmt (desugar_stmt st, desugar_expr e, desugar_stmt st2, List.map desugar_stmt s)
  | ReturnStmt e -> ReturnStmt (Option.map desugar_expr e)

let desugar_func func = 
  let (dt, id, p, scope) = func in 
  (dt, id, p, List.map desugar_stmt scope)
let desugar_program (program:program) = List.map desugar_func program
