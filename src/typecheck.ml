open CoreAst

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

open Utils.ContextMonad (struct type t = context end)

(* We use void here as a wildcard *)
let printf: context_func = { id="printf"; return=Void; params=[Pointer Char; Void] }
let debug_print_context: context_func  = { id="debug_print_context"; return=Void; params=[] }
let println: context_func  = { id="println"; return=Void; params=[Void] }
let debug_println: context_func  = { id="debug_println"; return=Void; params=[Void] }

let empty_ctx = {scope=0; vars=[]; funcs=[printf; debug_print_context; println; debug_println]; return=Void};;

exception TypeError of string

let soft_compare_datatype a b : int = 
  if (compare_datatype a Int) == 0 && (compare_datatype b Float) == 0 then 0 
  else if (compare_datatype a Float) == 0 && (compare_datatype b Int) == 0 then 0
  else compare_datatype a b
;; 

let paramtype p = let (dt, _) = p in dt;;
let paramstype ps = List.map paramtype ps ;;

let ctx_get_return_dt = fun ctx -> ctx, ctx.return

let ctx_add_func f = 
  let (dt, id, ps, _) = f in
  fun ctx -> { ctx with
    funcs= { id=id; return=dt; params=paramstype ps } ::ctx.funcs
  }, ()

let ctx_find_func id = 
  fun ctx -> (ctx, match List.find_opt (fun (v: context_func) -> String.equal id v.id) ctx.funcs with 
  | Some v -> v
  | None -> raise (TypeError "cannot find function"))

let ctx_find_var id = 
  fun ctx -> (ctx, match List.find_opt (fun (v: context_var) -> String.equal id v.id) ctx.vars with 
  | Some v -> v
  | None -> raise (TypeError "cannot find variables"))
  
let rec remove_outscope_vars s (vs: context_var list) = 
  match vs with 
  | [] -> []
  | h::t -> 
      if h.scope > s then remove_outscope_vars s t 
      else h::(remove_outscope_vars s t)

let ctx_add_param p = 
  let (dt, id) = p in 
  fun ctx -> { ctx with vars={id=id;var_type=dt;scope=ctx.scope}::ctx.vars }, ()

let rec ctx_add_params ps = 
  match ps with 
  | [] -> return () 
  | h::t -> 
    let* () = ctx_add_param h in 
    ctx_add_params t

let ctx_scope = fun ctx -> { ctx with scope=ctx.scope+1 }, ()
let ctx_descope = fun ctx -> { ctx with scope=ctx.scope-1; vars=(remove_outscope_vars (ctx.scope-1) ctx.vars) }, ()
let ctx_sink_func ret params = 
  fun ctx -> ctx_add_params params { ctx with return=ret }

let (>==) cat g =   (* chain context *)
  let (c, _) = cat in g c
;;

let (>>=) cat g =   (* map context *)
  let (c, t) = cat in (g c, t)
;;

(* Typechecking *)

let rec typecheck_expr e = 
  match e with 
  | StringValue _ -> return (Pointer Char)
  | IntValue _ -> return Int
  | FloatValue _ -> return Float
  | VariableExpr v -> type_of_var v
  | PlusPlusExpr v -> fun ctx -> let _ = typeassert_var v Int ctx in (ctx, Int)
  | BinOpExpr (a, o, b) -> typecheck_binop a o b
  | UnaOpExpr (o, e) -> typecheck_unaop o e
  | FuncCallExpr (id, es) ->
      let* ctxf = ctx_find_func id in
      let* () = typeassert_params es ctxf.params in
      return ctxf.return
  | StmtsExpr (s, e) ->
      let* _ = typecheck_stmts s in 
      typecheck_expr e 
  | ScopeExpr (s, e) ->
      let* _ = typecheck_scope s in 
      typecheck_expr e

and typeassert_var v dt = 
  let* t = type_of_var v in
  if (compare_datatype dt t) != 0 then
    raise (TypeError "fail var type assertion")
  else return ()

and type_of_var v = 
  match v with 
  | VarIden id -> 
      let* ctxv = ctx_find_var id in 
      return ctxv.var_type
  | VarAccess (v, e) -> 
      let* () = typeassert_expr e Int in
      let* dt = type_of_var v in 
      match dt with 
      | Pointer x -> return x
      | _ -> raise (TypeError "cannot access a non-pointer")

and typeassert_expr e dt = 
  let* t = typecheck_expr e in 
  if (compare_datatype dt t) != 0 then
    raise (TypeError "fail expr type assertion")
  else return ()

and typeassert_params es dts = 
  match (es, dts) with 
  | ([], []) -> return ()
  | (h::t, Void::tdt) ->
      typecheck_expr h >>> typeassert_params t tdt
  | (h::t, hdt::tdt) -> 
      typeassert_expr h hdt >>> typeassert_params t tdt
  | _ -> raise (TypeError "wrong arity")

and typecheck_binop a o b =
  let* ta = typecheck_expr a in
  let* tb = typecheck_expr b in
  match (ta, o, tb) with 
  | (Int, BOP_Plus, Int) -> return Int
  | (Int, BOP_Minus, Int) -> return Int
  | (Int, BOP_Mult, Int) -> return Int
  | (Int, BOP_Div, Int) -> return Int
  | _ ->
    if (soft_compare_datatype ta tb) != 0 then raise (TypeError "wrong binary operation")
    else match o with 
    | BOP_Plus -> return Float
    | BOP_Minus -> return Float
    | BOP_Mult -> return Float
    | BOP_Div -> return Float
    | BOP_Equal -> return Bool
    | BOP_Lt -> return Bool
    | BOP_Gt -> return Bool
    | BOP_Lte -> return Bool
    | BOP_Gte -> return Bool
    | _ -> raise (TypeError "wrong binary operation")

and typecheck_unaop o e =
  let* te = typecheck_expr e in
  match (o, te) with 
  | (UOP_Not, Bool) -> return Bool
  | (UOP_Neg, Int) -> return Int
  | (UOP_Neg, Float) -> return Float
  | (UOP_And, x) -> return (Pointer x)
  | _ -> raise (TypeError "wrong unary operation")

and soft_typeassert_expr e dt = 
  let* t = typecheck_expr e in 
  if (compare_datatype dt Int) == 0 then (
    if (compare_datatype Int t) == 0 then return ()
    else if (compare_datatype Float t) == 0 then return ()
    else raise (TypeError "fail soft type assertion (int)"))
  else if (compare_datatype dt Float) == 0 then (
    if (compare_datatype Int t) == 0 then return ()
    else if (compare_datatype Float t) == 0 then return ()
    else raise (TypeError "fail soft type assertion (float)"))
  else if (compare_datatype dt t) == 0 then return ()
  else raise (TypeError "fail soft type assertion")

and declare_var dt v = 
  match v with 
  | VarIden id -> return (dt, id)
  | VarAccess (v, e) -> 
      let* () = typeassert_expr e Int in
      let* (dt, id) = declare_var dt v in
      return (Pointer dt, id)

and ctx_add_var dt v = 
  let* (dt, id) = declare_var dt v in
  fun ctx -> {ctx with vars={id=id;scope=ctx.scope;var_type=dt}::ctx.vars}, ()

and ctx_add_vars dt vs = 
  match vs with 
  | []   -> return ()
  | h::t -> ctx_add_var dt h >>> ctx_add_vars dt t

and typecheck_stmt st = 
  match st with 
  | DeclarationStmt (dt, vs) -> ctx_add_vars dt vs >>> return Void
  | AssignmentStmt (v, e) -> 
      let* dt = type_of_var v in
      let* () = soft_typeassert_expr e dt in 
      return Void
  | ExpressionStmt e -> typecheck_expr e
  | IfStmt (e, s) -> 
      let* () = typeassert_expr e Bool in
      typecheck_scope s
  | WhileStmt (e, sc) -> 
      let* () = typeassert_expr e Bool in
      typecheck_scope sc
  | ReturnStmt expr -> ( 
      let* dt = ctx_get_return_dt in 
      match expr with 
      | Some e -> 
          let* () = typeassert_expr e dt in 
          return Void
      | None -> (match dt with 
          | Void -> return Void
          | _ -> raise (TypeError "typed return in a void function")))
  | StmtsStmt s -> typecheck_stmts s 
  | ScopeStmt s -> typecheck_scope s

and typecheck_stmts sts = 
  match sts with 
  | [] ->   return Void
  | h::t -> typecheck_stmt h >>> typecheck_stmts t

and typecheck_scope sc =
  let* () = ctx_scope in
  let* dt = typecheck_stmts sc in
  let* () = ctx_descope in 
  return dt
;;

let typecheck_func f = 
  let* () = ctx_add_func f in
  let (ret, _, params, sc) = f in 
  let* () = ctx_scope in 
  let* () = ctx_sink_func ret params in 
  let* dt = typecheck_stmts sc in 
  let* () = ctx_descope in 
  return dt
;;

let rec typecheck_prog program = 
  match program with
  | []   -> return Void
  | h::t -> typecheck_func h >>>typecheck_prog t
;;

let typecheck program = let _ = typecheck_prog program empty_ctx in ();;
