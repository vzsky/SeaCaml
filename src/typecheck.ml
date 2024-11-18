open Ast
open Utils

type context_var = {
  id: string;
  scope: int;
  var_type: datatype;
}

type context_func = {
  id: string; 
  return: datatype; 
  params: datatype list;
}

type context = {
  scope: int;
  vars : context_var list;
  funcs: context_func list;
  return: datatype;
}

let empty_ctx = {scope=0; vars=[]; funcs=[]; return=Void};;

exception TypeError

let paramtype p = let (dt, _) = p in dt;;
let paramstype ps = List.map paramtype ps ;;

let ctx_add_func f ctx = 
  let (dt, id, ps, _) = f in
  { ctx with
    funcs= { id=string_of_charlist id; return=dt; params=paramstype ps } ::ctx.funcs
  }
;;

let ctx_find_func id ctx : context_func = 
  List.find (fun (v: context_func) -> String.equal id v.id) ctx.funcs
;;

let ctx_find_var id ctx : context_var = 
  List.find (fun (v: context_var) -> String.equal id v.id) ctx.vars
;;
  
let rec remove_outscope_vars s (vs: context_var list) = 
  match vs with 
  | [] -> []
  | h::t -> 
      if h.scope > s then remove_outscope_vars s t 
      else h::(remove_outscope_vars s t)
;;

let ctx_scope ret ctx = 
  match ret with 
  | Some ret -> {ctx with scope=ctx.scope+1; return=ret} 
  | None -> {ctx with scope=ctx.scope+1}
;;
let ctx_descope ctx = {ctx with scope=ctx.scope-1; vars=(remove_outscope_vars (ctx.scope-1) ctx.vars)} ;;

let (>==) cat g =   (* chain context *)
  let (c, _) = cat in g c
;;

let (>>=) cat g =   (* map context *)
  let (c, t) = cat in (g c, t)
;;

(* Typechecking *)

let rec typecheck_expr e ctx = 
  match e with 
  | StringValue _ -> (ctx, Pointer Char)
  | IntValue _ -> (ctx, Int)
  | FloatValue _ -> (ctx, Float)
  | VariableExpr v -> (ctx, type_of_var v ctx)
  | PlusPlusExpr v -> typeassert_var v Int ctx; (ctx, Int)
  | BinOpExpr (a, o, b) -> typecheck_binop a o b ctx
  | UnaOpExpr (o, e) -> typecheck_unaop o e ctx
  | FuncCallExpr _ -> (ctx, Void) (* TODO *)

and typeassert_var v dt ctx = 
  let t = type_of_var v ctx in 
  if (compare_datatype dt t) != 0 then
    raise TypeError
  else ()

and type_of_var v ctx = 
  match v with 
  | VarIden id -> 
      let ctxv = ctx_find_var (string_of_charlist id) ctx in ctxv.var_type
  | VarAccess (v, e) -> 
      typeassert_expr e Int ctx;
      match type_of_var v ctx with 
      | Pointer x -> x
      | _ -> raise TypeError 

and typeassert_expr e dt ctx = 
  let (_, t) = typecheck_expr e ctx in 
  if (compare_datatype dt t) != 0 then
    raise TypeError
  else ()

and typecheck_binop a o b ctx =
  let (ctx, ta) = typecheck_expr a ctx in
  let (ctx, tb) = typecheck_expr b ctx in
  match (ta, o, tb) with 
  | (Int, BOP_Plus, Int) -> (ctx, Int)
  | (Int, BOP_Minus, Int) -> (ctx, Int)
  | (Int, BOP_Mult, Int) -> (ctx, Int)
  | (Int, BOP_Div, Int) -> (ctx, Int)
  | (Float, BOP_Plus, Int) -> (ctx, Float)
  | (Float, BOP_Plus, Float) -> (ctx, Float)
  | (Int, BOP_Plus, Float) -> (ctx, Float)
  | (Float, BOP_Minus, Int) -> (ctx, Float)
  | (Float, BOP_Minus, Float) -> (ctx, Float)
  | (Int, BOP_Minus, Float) -> (ctx, Float)
  | (Float, BOP_Mult, Int) -> (ctx, Float)
  | (Float, BOP_Mult, Float) -> (ctx, Float)
  | (Int, BOP_Mult, Float) -> (ctx, Float)
  | (Float, BOP_Div, Int) -> (ctx, Float)
  | (Float, BOP_Div, Float) -> (ctx, Float)
  | (Int, BOP_Div, Float) -> (ctx, Float)
  | (Bool, BOP_LAnd, Bool) -> (ctx, Bool) 
  | (Bool, BOP_LOr, Bool) -> (ctx, Bool) 
  | _ ->
    if (compare_datatype ta tb) != 0 then raise TypeError
    else match o with 
    | BOP_Equal -> (ctx, Bool)
    | BOP_Lt -> (ctx, Bool)
    | BOP_Gt -> (ctx, Bool)
    | BOP_Lte -> (ctx, Bool)
    | BOP_Gte -> (ctx, Bool)
    | _ -> raise TypeError

and typecheck_unaop o e ctx =
  let (ctx, te) = typecheck_expr e ctx in
  match (o, te) with 
  | (UOP_Not, Bool) -> (ctx, Bool)
  | (UOP_Neg, Int) -> (ctx, Int)
  | (UOP_Neg, Float) -> (ctx, Float)
  | (UOP_And, x) -> (ctx, Pointer x)
  | _ -> raise TypeError
;;

let rec declare_var dt v ctx = 
  match v with 
  | VarIden id -> (dt, id)
  | VarAccess (v, e) -> 
      typeassert_expr e Int ctx;
      let (dt, id) = (declare_var dt v ctx) in
      (Pointer dt, id)
;;

let ctx_add_var dt v ctx = 
  let (dt, id) = declare_var dt v ctx in
  {ctx with vars={id=string_of_charlist id;scope=ctx.scope;var_type=dt}::ctx.vars}
;;

let rec ctx_add_vars dt vs ctx = 
  match vs with 
  | []   -> ctx
  | h::t -> ctx |> ctx_add_var dt h |> ctx_add_vars dt t
;;

let typecheck_stmt st ctx = 
  match st with 
  | DeclarationStmt (dt, vs) -> 
      (ctx, Void) >>= ctx_add_vars dt vs
  | AssignmentStmt (v, e) -> 
      let dt = type_of_var v ctx in
      typeassert_expr e dt ctx; 
      (ctx, Void)
  | ExpressionStmt e -> typecheck_expr e ctx
  | IfStmt _ -> (ctx, Void) (* TODO *)
  | ForStmt _ -> (ctx, Void) (* TODO *)
  | ReturnStmt _ -> (ctx, Void) (* TODO *)
;;

let rec typecheck_stmts sts ctx = 
  match sts with 
  | [] ->   (ctx, Void)
  | h::t -> (ctx, Void)
      >== typecheck_stmt h
      >== typecheck_stmts t
;;

let typecheck_scope sc ret ctx =
  (ctx, Void)
    >>= ctx_scope ret
    >== typecheck_stmts sc 
    >>= ctx_descope
;;

let typecheck_func f ctx = 
  let ctx = ctx_add_func f ctx in
  let (ret, _, _, sc) = f in 
  typecheck_scope sc (Some ret) ctx
;;

let rec typecheck_prog program ctx = 
  match program with
  | []   -> (ctx, Void)
  | h::t -> (ctx, Void)
      >== typecheck_func h
      >== typecheck_prog t
;;

let typecheck program = let _ = typecheck_prog program empty_ctx in ();;
