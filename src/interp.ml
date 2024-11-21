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
}

type context = {
  memory: memory; 
  memory_pos: int;
  symbol_table: symbol_entry list;
  funcs: context_func list 
}

let empty_context = {memory=(Array.make tape_size Null); memory_pos=0; symbol_table=[]; funcs=[]}

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
(********************* Interp **************************)
(*******************************************************)

open WithContext

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

let rec allocate_mems datatype var = 
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
  | _ -> return (IntValue (Some 0)) (* TODO *)

let rec get_var_id var = 
  match var with 
  | VarIden id -> id 
  | VarAccess (v, _) -> get_var_id v

let write_symbol_table var addr context = 
  let id = get_var_id var in
  {context with symbol_table={name=id; addr}::context.symbol_table}, ()

let declare_var datatype var = 
  let* head = allocate_mems datatype var in
  write_symbol_table var head

let declare_vars datatype = fold_context (declare_var datatype)

let interp_statement statement = 
  match statement with 
  | DeclarationStmt (datatype, var_list) -> declare_vars datatype var_list
  | _ -> return () 
  (* | AssignmentStmt (variable, expression) *)
  (* | ExpressionStmt expression *)
  (* | IfStmt (expression, scope) *)
  (* | ForStmt (statement, expression, statement, scope) *)
  (* | ReturnStmt expression *)

let interp_scope scope = fold_context interp_statement scope

let interp_func func context = 
  let (_, id, _, scope) = func in
  if String.compare id "main" == 0 then interp_scope scope context
  else {context with funcs = {decl=func}::context.funcs}, ()

let interp_program program = fold_context interp_func program 
let interpret program = interp_program program empty_context

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
  

