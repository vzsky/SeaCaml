open Ast
open Utils

let tape_size = 100

type context_func = {
  decl: function_decl 
}

type value = 
  | IntValue of int option
  | FloatValue of float option
  | CharValue of char option 
  | BoolValue of bool option
  | PointerValue of int option
  | Null (* \0 *)
type memory = value array

type symbol_entry = {
  name: string; 
  addr: int;
  scope: int
}

type context = {
  memory: memory; 
  memory_pos: int;
  symbol_table: symbol_entry list;
  funcs: context_func list; 
  scope: int; 
  last_func_scope: int;
}

let empty_context = {
  memory=(Array.make tape_size Null); 
  memory_pos=0; 
  symbol_table=[]; 
  funcs=[]; 
  scope=0; 
  last_func_scope=0
}

exception RuntimeError of string

module WithContext = struct
  type 'a t = context -> context * 'a

  let return (x : 'a) : 'a t =
    fun ctx -> (ctx, x)

  let bind (m : 'a t) (f : 'a -> 'b t) : 'b t =
    fun ctx ->
      let (ctx', a) = m ctx in
      f a ctx'

  let ( let* ) = bind
  let (>>=) = bind
  let (>>>) (f: 'a t) (g: 'b t) = let* _ = f in g

  let rec repeat (n: int) (m: 'a t) : ('a list) t =
    if n == 0 then return [] else 
    let* x = m in
    let* y = repeat (n-1) m in 
    return (x::y)

end

(*******************************************************)
(**************** Visualization Tools ******************)
(*******************************************************)

let string_of_value v = 
  match v with 
  | IntValue i -> "i " ^ (string_of_option string_of_int i)
  | FloatValue f -> "f " ^ (string_of_option string_of_float f)
  | CharValue c -> "c " ^ (string_of_option string_of_char c)
  | BoolValue b -> "b " ^ (string_of_option string_of_bool b)
  | PointerValue p -> "p " ^ (string_of_option string_of_int p)
  | Null -> "0"

let string_of_symbol_entry s = s.name ^ "@" ^ string_of_int s.addr

let show_memory context = 
  Array.map string_of_value context.memory |> Array.to_list |> unwords "::"

let show_symbol_table context = 
  List.map (string_of_symbol_entry) (List.rev context.symbol_table) |> unwords "::"
  

(*******************************************************)
(********************* Interp **************************)
(*******************************************************)

open WithContext

let descope_once context = 
  let symbol_table = List.filter (fun (ent:symbol_entry) -> ent.scope < context.scope) context.symbol_table in 
  {context with symbol_table}, ()

let rec fold_context (f: 'a -> 'b WithContext.t) (l: 'a list) =
  match l with 
  | [] -> return ()
  | h::t -> f h >>> fold_context f t

let assert_option m =
  match m with 
  | Some a -> return a
  | None -> raise (RuntimeError "result assertion failed")

let assert_int value =
  match value  with
  | IntValue Some i -> return i
  | _ -> raise (RuntimeError "int assertion failed")

let assert_bool value = 
  match value with 
  | BoolValue Some b -> return b 
  | _ -> raise (RuntimeError ("bool assertion failed"))

let allocate_null context = 
  let pos = context.memory_pos in 
  context.memory.(pos) <- Null;
  {context with memory_pos=pos+1}, pos

let allocate_pointer addr context = 
  let pos = context.memory_pos in 
  context.memory.(pos) <- PointerValue (Some addr);
  {context with memory_pos=pos+1}, pos

let allocate_pointers addrs = 
  match addrs with 
  | [] -> raise (RuntimeError "allocating nil list of pointers")
  | h::t -> 
      let* head = allocate_pointer h in 
      let* _ = fold_context allocate_pointer t in
      let* _ = allocate_null in 
      return head

let allocate_mem datatype context = 
  let cell = 
    match datatype with 
    | Int -> IntValue None 
    | Float -> FloatValue None
    | Char -> CharValue None
    | Bool -> BoolValue None
    | Void -> raise (RuntimeError "don't allocate for void")
    | Pointer _ -> PointerValue None
    in 
  let pos = context.memory_pos in 
  context.memory.(pos) <- cell;
  {context with memory_pos=pos+1}, pos

(* It's like we did typecheck again here *)
let interp_binop a o b = 
  match (a, o, b) with 
  | (IntValue Some av, BOP_Plus, IntValue Some bv) -> return (IntValue (Some (av + bv)))
  | (IntValue Some av, BOP_Minus, IntValue Some bv) -> return (IntValue (Some (av - bv)))
  | (IntValue Some av, BOP_Mult, IntValue Some bv) -> return (IntValue (Some (av * bv)))
  | (IntValue Some av, BOP_Div, IntValue Some bv) -> return (IntValue (Some (av / bv)))
  | (IntValue Some av, BOP_Plus, FloatValue Some bv) -> return (FloatValue (Some (float_of_int av +. bv)))
  | (IntValue Some av, BOP_Minus, FloatValue Some bv) -> return (FloatValue (Some (float_of_int av -. bv)))
  | (IntValue Some av, BOP_Mult, FloatValue Some bv) -> return (FloatValue (Some (float_of_int av *. bv)))
  | (IntValue Some av, BOP_Div, FloatValue Some bv) -> return (FloatValue (Some (float_of_int av /. bv)))
  | (FloatValue Some av, BOP_Plus, IntValue Some bv) -> return (FloatValue (Some (av +. float_of_int bv)))
  | (FloatValue Some av, BOP_Minus, IntValue Some bv) -> return (FloatValue (Some (av -. float_of_int bv)))
  | (FloatValue Some av, BOP_Mult, IntValue Some bv) -> return (FloatValue (Some (av *. float_of_int bv)))
  | (FloatValue Some av, BOP_Div, IntValue Some bv) -> return (FloatValue (Some (av /. float_of_int bv)))

  | (IntValue Some av, BOP_Equal, IntValue Some bv) -> return (BoolValue (Some (av == bv)))
  | (IntValue Some av, BOP_Lt, IntValue Some bv) -> return (BoolValue (Some (av < bv)))
  | (IntValue Some av, BOP_Gt, IntValue Some bv) -> return (BoolValue (Some (av > bv)))
  | (IntValue Some av, BOP_Lte, IntValue Some bv) -> return (BoolValue (Some (av <= bv)))
  | (IntValue Some av, BOP_Gte, IntValue Some bv) -> return (BoolValue (Some (av >= bv)))

  | (IntValue Some av, BOP_Equal, FloatValue Some bv) -> return (BoolValue (Some (float_of_int av == bv)))
  | (IntValue Some av, BOP_Lt, FloatValue Some bv) -> return (BoolValue (Some (float_of_int av < bv)))
  | (IntValue Some av, BOP_Gt, FloatValue Some bv) -> return (BoolValue (Some (float_of_int av > bv)))
  | (IntValue Some av, BOP_Lte, FloatValue Some bv) -> return (BoolValue (Some (float_of_int av <= bv)))
  | (IntValue Some av, BOP_Gte, FloatValue Some bv) -> return (BoolValue (Some (float_of_int av >= bv)))

  | (FloatValue Some av, BOP_Equal, IntValue Some bv) -> return (BoolValue (Some (av == float_of_int bv)))
  | (FloatValue Some av, BOP_Lt, IntValue Some bv) -> return (BoolValue (Some (av < float_of_int bv)))
  | (FloatValue Some av, BOP_Gt, IntValue Some bv) -> return (BoolValue (Some (av > float_of_int bv)))
  | (FloatValue Some av, BOP_Lte, IntValue Some bv) -> return (BoolValue (Some (av <= float_of_int bv)))
  | (FloatValue Some av, BOP_Gte, IntValue Some bv) -> return (BoolValue (Some (av >= float_of_int bv)))

  | (CharValue Some av, BOP_Equal, CharValue Some bv) -> return (BoolValue (Some (av == bv)))
  | (PointerValue Some av, BOP_Equal, PointerValue Some bv) -> return (BoolValue (Some (av == bv)))

  | _ -> raise (RuntimeError "invalid binary operation")

let rec get_var_id var = 
  match var with 
  | VarIden id -> id 
  | VarAccess (v, _) -> get_var_id v

let write_symbol_table var addr context = 
  let id = get_var_id var in
  {context with symbol_table={name=id; addr; scope=context.scope}::context.symbol_table}, ()

let look_symbol_table var context = 
  let id = get_var_id var in 
  context, List.find (fun ent -> 
    String.compare ent.name id == 0 
  ) context.symbol_table

(* TODO: Implicit type casting *)
let write_at_addr addr value context = 
  let () = match context.memory.(addr), value with 
  | IntValue _, IntValue _ -> ()
  | FloatValue _, FloatValue _ -> ()
  | CharValue _, CharValue _ -> () 
  | BoolValue _, BoolValue _ -> ()
  | PointerValue _, PointerValue _ -> ()
  | Null, Null -> ()
  | _ -> raise (RuntimeError ("writing to a memory with different type" )) in
  context.memory.(addr) <- value;
  context, ()

let dereference_pointer addr context = 
  match context.memory.(addr) with 
  | PointerValue (Some p) -> context, p
  | PointerValue None -> raise (RuntimeError "dereferencing a null-pointer") 
  | _ -> raise (RuntimeError "dereferencing a non-pointer")

let read_at_addr addr context = context, context.memory.(addr)

let rec read_variable variable = 
  let* entry = look_symbol_table variable in 
  read_pointer entry.addr variable

and read_pointer addr var = 
  match var with 
  | VarIden _ -> read_at_addr addr
  | VarAccess (v, e) -> 
      let* n = interp_expr e in 
      let* n = assert_int n in 
      match v with 
      | VarIden _ -> read_at_addr (addr+n) 
      | VarAccess _ -> 
          let* pos = dereference_pointer (addr+n) in 
          read_pointer pos v

and allocate_mems datatype var = 
  match var with 
  | VarIden _ -> allocate_mem datatype  
  | VarAccess (v, e) -> 
      let* n = interp_expr e in 
      let* n = assert_int n in 
      if n == 0 then raise (RuntimeError "allocating 0 elements array") else
      let* addrs = repeat n (allocate_mems datatype v) in

      match v with 
      | VarIden _ -> allocate_null >>> return (List.hd addrs)
      | VarAccess _ -> allocate_pointers addrs 

and interp_expr e = 
  match e with 
  | IntValue i -> return (IntValue (Some i))
  | FloatValue f -> return (FloatValue (Some f))
  | BinOpExpr (a, o, b) -> 
    let* av = interp_expr a in 
    let* bv = interp_expr b in 
    interp_binop av o bv
  | VariableExpr variable -> read_variable variable
  (* | StringValue of string *)
  (* | PlusPlusExpr of variable *)
  (* | UnaOpExpr of una_operator * expression *)
  (* | FuncCallExpr of identifier * expression list *)
  | _ -> return (IntValue (Some 0)) (* TODO *)
  
let declare_var datatype var = 
  let* head = allocate_mems datatype var in
  write_symbol_table var head

let declare_vars datatype = fold_context (declare_var datatype)

let rec write_pointer addr var value = 
  match var with 
  | VarIden _ -> write_at_addr addr value
  | VarAccess (v, e) -> 
      let* n = interp_expr e in 
      let* n = assert_int n in 
      match v with 
      | VarIden _ -> write_at_addr (addr+n) value 
      | VarAccess _ -> 
          let* pos = dereference_pointer (addr + n) in 
          write_pointer pos v value

let assign_var var value = 
  let* entry = look_symbol_table var in
  match var with 
  | VarIden _ -> write_at_addr entry.addr value
  | VarAccess _ -> write_pointer entry.addr var value

let rec interp_statement statement = 
  match statement with 
  | DeclarationStmt (datatype, var_list) -> declare_vars datatype var_list
  | AssignmentStmt (variable, expression) -> interp_expr expression >>= assign_var variable
  | ExpressionStmt expression -> interp_expr expression >>> return ()
  | IfStmt (expression, scope) -> 
      let* b = interp_expr expression in 
      let* b = assert_bool b in 
      if b then interp_scope scope else return ()
  (* | ForStmt (statement, expression, statement, scope) *)
  (* | ReturnStmt expression *)
  | _ -> return () 

and interp_scope scope context =
  {context with scope=context.scope+1} |>
  let* () = fold_context interp_statement scope in 
  let* () = descope_once in 
  return ()
  

let interp_func func context = 
  let (_, id, _, scope) = func in
  if String.compare id "main" == 0 then interp_scope scope context
  else {context with funcs = {decl=func}::context.funcs}, ()

let interp_program program = fold_context interp_func program 
let interpret program = interp_program program empty_context


