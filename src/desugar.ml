open CoreAst

(*******************************)
(*********** DESUGAR ***********)
(*******************************)

let rec variable_to_list var = 
  match var with 
  | Ast.VarIden id -> ([], id)
  | Ast.VarAccess (v, e) -> 
      let ls, id = variable_to_list v in (e::ls, id)

let desugar_compound v o e = 
  let binop = match o with 
    | Ast.Com_PlusEq -> BOP_Plus 
    | Ast.Com_MinusEq -> BOP_Minus in 
  let expr = BinOpExpr (VariableExpr v, binop, e) in 
      StmtsExpr ([AssignmentStmt (v, expr)], VariableExpr v)
  
let rec list_to_core_variable ls id = 
  match ls with 
  | [] -> VarIden id
  | h::t -> VarAccess ((list_to_core_variable t id), h)

let rec reverse_variable_order varrev = 
  let ls, id = variable_to_list varrev in 
  let ls = ls |> List.rev |> List.map desugar_expr in
  list_to_core_variable ls id

and desugar_variable v = reverse_variable_order v

and desugar_expr expr = 
  match expr with 
  | Ast.VariableExpr v -> VariableExpr (desugar_variable v)
  | Ast.BinOpExpr (a, o, b) -> BinOpExpr (desugar_expr a, o, desugar_expr b)
  | Ast.PlusPlusExpr v -> PlusPlusExpr (desugar_variable v)
  | Ast.UnaOpExpr (o, e) -> UnaOpExpr (o, desugar_expr e)
  | Ast.FuncCallExpr (i, el) -> FuncCallExpr (i, List.map desugar_expr el)
  | Ast.StringValue v -> StringValue v 
  | Ast.FloatValue v -> FloatValue v 
  | Ast.IntValue v -> IntValue v 
  (* actual desugaring *)
  | Ast.CompoundExpr (v, o, e) -> desugar_compound (desugar_variable v) o (desugar_expr e)

let rec desugar_stmt stmt = 
  match stmt with
  | Ast.DeclarationStmt (dt, vl) -> DeclarationStmt (dt, List.map reverse_variable_order vl)
  | Ast.AssignmentStmt (v, e) -> AssignmentStmt (desugar_variable v, desugar_expr e)
  | Ast.ExpressionStmt e -> ExpressionStmt (desugar_expr e)
  | Ast.IfStmt (e, s) -> IfStmt (desugar_expr e, List.map desugar_stmt s)
  | Ast.ReturnStmt e -> ReturnStmt (Option.map desugar_expr e)
  | Ast.WhileStmt (e, s) -> WhileStmt (desugar_expr e, List.map desugar_stmt s)
  (* actual desugaring *)
  | Ast.ForStmt (st, e, st2, s) -> 
      let scope = List.map desugar_stmt (s @ [st2]) in
      StmtsStmt [desugar_stmt st; WhileStmt(desugar_expr e, scope)] 
  | Ast.DeclareAssignStmt (dt, ves) -> 
      let fs = List.map (fun x -> fst x |> desugar_variable) ves in
      let assigns = List.map (fun (v, e) -> AssignmentStmt (desugar_variable v, desugar_expr e)) ves in 
      StmtsStmt ([DeclarationStmt(dt, fs)] @ assigns)


let desugar_func func = 
  let (dt, id, p, scope) = func in 
  (dt, id, p, List.map desugar_stmt scope)

let desugar_program (program) = List.map desugar_func program
