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

(* We use void here as a wildcard *)
let printf: context_func = { id="printf"; return=Void; params=[Pointer Char; Void] }
let empty_ctx = {scope=0; vars=[]; funcs=[printf]; return=Void};;

exception TypeError of string

let soft_compare_datatype a b : int = 
  if (compare_datatype a Int) == 0 && (compare_datatype b Float) == 0 then 0 
  else if (compare_datatype a Float) == 0 && (compare_datatype b Int) == 0 then 0
  else compare_datatype a b
;; 

let paramtype p = let (dt, _) = p in dt;;
let paramstype ps = List.map paramtype ps ;;

let ctx_add_func f ctx = 
  let (dt, id, ps, _) = f in
  { ctx with
    funcs= { id=string_of_charlist id; return=dt; params=paramstype ps } ::ctx.funcs
  }
;;

let ctx_find_func id ctx : context_func = 
  match List.find_opt (fun (v: context_func) -> String.equal id v.id) ctx.funcs with 
  | Some v -> v
  | None -> raise (TypeError "cannot find function")
;;

let ctx_find_var id ctx : context_var = 
  match List.find_opt (fun (v: context_var) -> String.equal id v.id) ctx.vars with 
  | Some v -> v
  | None -> raise (TypeError "cannot find variables")
;;
  
let rec remove_outscope_vars s (vs: context_var list) = 
  match vs with 
  | [] -> []
  | h::t -> 
      if h.scope > s then remove_outscope_vars s t 
      else h::(remove_outscope_vars s t)
;;

let ctx_add_param p ctx = 
  let (dt, id) = p in 
  { ctx with vars={id=string_of_charlist id;var_type=dt;scope=ctx.scope}::ctx.vars }
;;

let rec ctx_add_params ps ctx = 
  match ps with 
  | [] -> ctx 
  | h::t -> ctx |> ctx_add_param h |> ctx_add_params t
;;

let ctx_scope ctx = {ctx with scope=ctx.scope+1; } ;;
let ctx_descope ctx = {ctx with scope=ctx.scope-1; vars=(remove_outscope_vars (ctx.scope-1) ctx.vars)} ;;
let ctx_sink_func ret params ctx = 
  ctx_add_params params {ctx with return=ret;}

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
  | FuncCallExpr (id, es) -> 
      let ctxf = ctx_find_func (string_of_charlist id) ctx in
    typeassert_params es ctxf.params ctx;
    (ctx, ctxf.return) 

and typeassert_var v dt ctx = 
  let t = type_of_var v ctx in 
  if (compare_datatype dt t) != 0 then
    raise (TypeError "fail var type assertion")
  else ()

and type_of_var v ctx = 
  match v with 
  | VarIden id -> 
      let ctxv = ctx_find_var (string_of_charlist id) ctx in ctxv.var_type
  | VarAccess (v, e) -> 
      typeassert_expr e Int ctx;
      match type_of_var v ctx with 
      | Pointer x -> x
      | _ -> raise (TypeError "cannot access a non-pointer")

and typeassert_expr e dt ctx = 
  let (_, t) = typecheck_expr e ctx in 
  if (compare_datatype dt t) != 0 then
    raise (TypeError "fail expr type assertion")
  else ()

and typeassert_params es dts ctx = 
  match (es, dts) with 
  | ([], []) -> ()
  | (_::t, Void::tdt) ->
      typeassert_params t tdt ctx
  | (h::t, hdt::tdt) -> 
      typeassert_expr h hdt ctx; typeassert_params t tdt ctx
  | _ -> raise (TypeError "wrong arity")

and typecheck_binop a o b ctx =
  let (ctx, ta) = typecheck_expr a ctx in
  let (ctx, tb) = typecheck_expr b ctx in
  match (ta, o, tb) with 
  | (Int, BOP_Plus, Int) -> (ctx, Int)
  | (Int, BOP_Minus, Int) -> (ctx, Int)
  | (Int, BOP_Mult, Int) -> (ctx, Int)
  | (Int, BOP_Div, Int) -> (ctx, Int)
  | _ ->
    if (soft_compare_datatype ta tb) != 0 then raise (TypeError "wrong binary operation")
    else match o with 
    | BOP_Plus -> (ctx, Float)
    | BOP_Minus -> (ctx, Float)
    | BOP_Mult -> (ctx, Float)
    | BOP_Div -> (ctx, Float)
    | BOP_Equal -> (ctx, Bool)
    | BOP_Lt -> (ctx, Bool)
    | BOP_Gt -> (ctx, Bool)
    | BOP_Lte -> (ctx, Bool)
    | BOP_Gte -> (ctx, Bool)
    | _ -> raise (TypeError "wrong binary operation")

and typecheck_unaop o e ctx =
  let (ctx, te) = typecheck_expr e ctx in
  match (o, te) with 
  | (UOP_Not, Bool) -> (ctx, Bool)
  | (UOP_Neg, Int) -> (ctx, Int)
  | (UOP_Neg, Float) -> (ctx, Float)
  | (UOP_And, x) -> (ctx, Pointer x)
  | _ -> raise (TypeError "wrong unary operation")
;;

let soft_typeassert_expr e dt ctx = 
  let (_, t) = typecheck_expr e ctx in 
  if (compare_datatype dt Int) == 0 then (
    if (compare_datatype Int t) == 0 then ()
    else if (compare_datatype Float t) == 0 then ()
    else raise (TypeError "fail soft type assertion (int)"))
  else if (compare_datatype dt Float) == 0 then (
    if (compare_datatype Int t) == 0 then ()
    else if (compare_datatype Float t) == 0 then ()
    else raise (TypeError "fail soft type assertion (float)"))
  else if (compare_datatype dt t) == 0 then ()
  else raise (TypeError "fail soft type assertion")
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

let rec typecheck_stmt st ctx = 
  match st with 
  | DeclarationStmt (dt, vs) -> 
      (ctx, Void) >>= ctx_add_vars dt vs
  | AssignmentStmt (v, e) -> 
      let dt = type_of_var v ctx in
      soft_typeassert_expr e dt ctx; 
      (ctx, Void)
  | ExpressionStmt e -> typecheck_expr e ctx
  | IfStmt (e, s) -> 
      typeassert_expr e Bool ctx; 
      typecheck_scope s ctx
  | ForStmt (s1, e, s2, sc) -> 
      let (ctx, _) = typecheck_stmt s1 ctx in 
      typeassert_expr e Bool ctx; 
      let (ctx, _) = typecheck_stmt s2 ctx in 
      typecheck_scope sc ctx
  | ReturnStmt e -> 
      typeassert_expr e ctx.return ctx; (ctx, Void)

and typecheck_stmts sts ctx = 
  match sts with 
  | [] ->   (ctx, Void)
  | h::t -> (ctx, Void)
      >== typecheck_stmt h
      >== typecheck_stmts t

and typecheck_scope sc ctx =
  (ctx, Void)
    >>= ctx_scope 
    >== typecheck_stmts sc 
    >>= ctx_descope
;;

let typecheck_func f ctx = 
  let nctx = ctx_add_func f ctx in
  let (ret, _, params, sc) = f in 
  (nctx, Void)
    >>= ctx_scope 
    >>= ctx_sink_func ret params
    >== typecheck_stmts sc 
    >>= ctx_descope
;;

let rec typecheck_prog program ctx = 
  match program with
  | []   -> (ctx, Void)
  | h::t -> (ctx, Void)
      >== typecheck_func h
      >== typecheck_prog t
;;

let typecheck program = let _ = typecheck_prog program empty_ctx in ();;
