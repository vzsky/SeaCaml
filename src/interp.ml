open Ast
open Utils

let tape_size = 100

exception RuntimeError of string

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

type stackOrHeap = Stack | Heap

type symbol_entry = {
  name: string; 
  addr: int;
  scope: int; 
  stack_or_heap: stackOrHeap;
}

type context = {
  memory: memory; 
  stack_pos: int; (* points to the next free space *)
  heap_pos: int;
  symbol_table: symbol_entry list;
  funcs: context_func list; 
  scope: int; 
  last_func_scope: (int * int) list; (* the scope and stack addr *)
  return_value: value option;
}

let empty_context = {
  memory=(Array.make tape_size Null); 
  stack_pos=tape_size-1; 
  heap_pos=0;
  symbol_table=[]; 
  funcs=[]; 
  scope=0; 
  last_func_scope=[];
  return_value=None;
}

let which_memory_is addr context = 
  if addr <= context.heap_pos then Heap 
  else if addr >= context.stack_pos then Stack
  else raise (RuntimeError "looking at unallocated memory")

let get_memory_characteristic stack_or_heap = 
  (* getter, setter, direction *)
  match stack_or_heap with 
  | Stack -> (fun ctx -> ctx.stack_pos), (fun ctx -> fun n -> {ctx with stack_pos=n}), -1
  | Heap -> (fun ctx -> ctx.heap_pos), (fun ctx -> fun n -> {ctx with heap_pos=n}), +1

module ContextMonad = struct
  type 'a ctxMonad = context -> context * 'a

  let return (x : 'a) : 'a ctxMonad =
    fun ctx -> (ctx, x)

  let bind (m : 'a ctxMonad) (f : 'a -> 'b ctxMonad) : 'b ctxMonad =
    fun ctx ->
      let (ctx', a) = m ctx in
      f a ctx'

  let ( let* ) = bind
  let (>>=) = bind

  let (>>>) (f: 'a ctxMonad) (g: 'b ctxMonad) = let* _ = f in g

end

open ContextMonad

let rec map_with_context (f: 'a -> 'b ctxMonad) (l: 'a list) = 
  match l with 
  | [] -> return []
  | h::t -> 
      let* head = f h in 
      let* tail = map_with_context f t in 
      return (head::tail)

let fold_context (f: 'a -> 'b ctxMonad) (l: 'a list) =
  map_with_context f l >>> return ()

let repeat_with_context (n: int) (m: 'a ctxMonad) : ('a list) ctxMonad =
  map_with_context (fun _ -> m) (iota n)

(*******************************************************)
(**************** Visualization Tools ******************)
(*******************************************************)

let printf_with_values format values =
  let rec process fmt vals =
    match fmt, vals with
    | "", [] -> ""
    | "", _ -> failwith "Too many arguments provided"
    | _, [] -> failwith "Not enough arguments provided"
    | _ ->
        let next_percent = String.index_opt fmt '%' in
        (match next_percent with
        | None -> fmt
        | Some idx ->
            let before = String.sub fmt 0 idx in
            let specifier = String.get fmt (idx + 1) in
            let rest_fmt = String.sub fmt (idx + 2) (String.length fmt - idx - 2) in
            let formatted_value =
              match specifier, (List.hd vals) with
              | 'd', IntValue Some i -> string_of_int i
              | 'f', FloatValue Some f -> string_of_float f
              | 'c', CharValue Some c -> string_of_char c
              | _, _ -> failwith "Mismatched format specifier and value"
            in
            before ^ formatted_value ^ process rest_fmt (List.tl vals))
  in process format values

let string_of_value v = 
  match v with 
  | IntValue i -> "i " ^ (string_of_option string_of_int i)
  | FloatValue f -> "f " ^ (string_of_option string_of_float f)
  | CharValue c -> "c " ^ (string_of_option string_of_char c)
  | BoolValue b -> "b " ^ (string_of_option string_of_bool b)
  | PointerValue p -> "p " ^ (string_of_option string_of_int p)
  | Null -> "N"

let string_of_symbol_entry s = s.name ^ "@" ^ string_of_int s.addr

let string_of_func_def f = let (_, id, _, _) = f.decl in id

let show_memory context = 
  Array.map string_of_value context.memory |> Array.to_list |> unwords "::"

let show_symbol_table context = 
  List.map (string_of_symbol_entry) (List.rev context.symbol_table) |> unwords "::"

let show_context_funcs context = 
  List.map (string_of_func_def) (List.rev context.funcs) |> unwords "::"
  
let show_context context = 
  "CONTEXT...........\n" ^
  "..symbols...........\n" ^
  (show_symbol_table context) ^ "\n" ^ 
  "..memory...........\n" ^
  (show_memory context) ^ "\n" ^
  "..funcs...........\n" ^
  (show_context_funcs context) ^ "\n" ^
  "....................\n" ^ 
  "\tscope=" ^ (string_of_int context.scope) ^ "\n" ^
  "\tlast_func_scope=" ^ (unwords "::" (List.map string_of_iint (List.rev context.last_func_scope))) ^ "\n" ^
  "\treturn_value=" ^ (string_of_option string_of_value context.return_value) ^ "\n" ^
  "...................." 

(* for the sake of completing assingment *)
let rec pointer_all_chars addr context = 
  let stack_or_heap = which_memory_is addr context in
  let (_, _, direction) = get_memory_characteristic stack_or_heap in
  let value = context.memory.(addr) in
  match value with 
  | Null -> context, true 
  | CharValue Some _ -> pointer_all_chars (addr + direction) context
  | _ -> context, false

let rec show_value value  = 
  match value with 
  | IntValue i -> return (string_of_option string_of_int i)
  | FloatValue f -> return (string_of_option string_of_float f)
  | CharValue c -> return (string_of_option string_of_char c)
  | BoolValue b -> return (string_of_option string_of_bool b)
  | PointerValue o -> (match o with 
      | Some addr -> show_pointer addr
      | None      -> return "#")
  | Null -> return "#"

and pointer_to_list addr acc context = 
  let stack_or_heap = which_memory_is addr context in
  let (_, _, direction) = get_memory_characteristic stack_or_heap in
  let value = context.memory.(addr) in
  match value with 
  | Null -> context, (List.rev acc) 
  | _ -> 
      (let* val_str = show_value value  in 
      pointer_to_list (addr+direction) (val_str::acc)) context

and show_pointer addr = 
  let* c = pointer_all_chars addr in
  let* ls = pointer_to_list addr [] in
  if c then return (unwords "" ls) 
  else return ("{" ^ (unwords "," ls) ^ "}")

let print_func_call id args = 
  let* str_args = map_with_context show_value args in
  return (id ^ "(" ^ (unwords ", " (str_args) ) ^ ")")

let print_return_call opt_val = 
  match opt_val with 
  | None -> return "return"
  | Some v -> let* s = show_value v in return ("return " ^ s)

(*******************************************************)
(********************* Interp **************************)
(*******************************************************)

let assert_int value =
  match value  with
  | IntValue Some i -> i
  | _ -> raise (RuntimeError "int assertion failed")

let assert_nat value =
  match value  with
  | IntValue Some i -> if i < 0 then raise (RuntimeError "nat assertion failed") else i
  | _ -> raise (RuntimeError "nat assertion failed")

let assert_bool value = 
  match value with 
  | BoolValue Some b -> b 
  | _ -> raise (RuntimeError ("bool assertion failed"))

let assert_string value = 
  match value with 
  | PointerValue Some p -> 
      let* c = pointer_all_chars p in 
      if c then show_pointer p else raise (RuntimeError ("string assertion failed"))
  | _ -> raise (RuntimeError ("string assertion failed"))

and show_pointer addr = 
  let* c = pointer_all_chars addr in
  let* ls = pointer_to_list addr [] in
  if c then return (unwords "" ls) 
  else return ("{" ^ (unwords "," ls) ^ "}")

let get_return_value context = context, context.return_value
let reset_return_value context = {context with return_value=None}, ()

let free_stack_mem addr context = 
  for i = context.stack_pos to addr do
    context.memory.(i) <- Null
  done;
  {context with stack_pos=addr}, ()

let descope_to scope context = 
  if context.scope <= scope then context, () else
  let symbol_table = List.filter (fun (ent:symbol_entry) -> ent.scope <= scope) context.symbol_table in 
  {context with symbol_table; scope}, ()

let descope_func context = context |> (
  let* () = descope_to (fst (List.hd context.last_func_scope) - 1) in 
  let* () = free_stack_mem (snd (List.hd context.last_func_scope)) in 
  let* () = reset_return_value in
  (fun ctx -> {ctx with last_func_scope=List.tl ctx.last_func_scope}, ())) 

let allocate_null stack_or_heap context = 
  let (get_pos, update, direction) = get_memory_characteristic stack_or_heap in 
  let pos = get_pos context in 
  context.memory.(pos) <- Null;
  update context (pos+direction), pos

let allocate_pointer stack_or_heap addr context = 
  let (get_pos, update, direction) = get_memory_characteristic stack_or_heap in 
  let pos = get_pos context in 
  context.memory.(pos) <- PointerValue (Some addr);
  update context (pos+direction), pos

let allocate_pointers stack_or_heap addrs = 
  match addrs with 
  | [] -> raise (RuntimeError "allocating nil list of pointers")
  | h::t -> 
      let* head = allocate_pointer stack_or_heap h in 
      let* _ = fold_context (allocate_pointer stack_or_heap) t in
      let* _ = allocate_null Stack in 
      return head

let allocate_mem stack_or_heap datatype context = 
  let (get_pos, update, direction) = get_memory_characteristic stack_or_heap in 
  let cell = 
    match datatype with 
    | Int -> IntValue None 
    | Float -> FloatValue None
    | Char -> CharValue None
    | Bool -> BoolValue None
    | Void -> raise (RuntimeError "don't allocate for void")
    | Pointer _ -> PointerValue None
    in 
  let pos = get_pos context in 
  context.memory.(pos) <- cell;
  update context (pos+direction), pos

(* It's like we did typecheck again here *)
let interp_binop a o b = 
  return (match (a, o, b) with 
  | (IntValue Some av, BOP_Plus, IntValue Some bv) -> IntValue (Some (av + bv))
  | (IntValue Some av, BOP_Minus, IntValue Some bv) -> IntValue (Some (av - bv))
  | (IntValue Some av, BOP_Mult, IntValue Some bv) -> IntValue (Some (av * bv))
  | (IntValue Some av, BOP_Div, IntValue Some bv) -> IntValue (Some (av / bv))
  | (IntValue Some av, BOP_Plus, FloatValue Some bv) -> FloatValue (Some (float_of_int av +. bv))
  | (IntValue Some av, BOP_Minus, FloatValue Some bv) -> FloatValue (Some (float_of_int av -. bv))
  | (IntValue Some av, BOP_Mult, FloatValue Some bv) -> FloatValue (Some (float_of_int av *. bv))
  | (IntValue Some av, BOP_Div, FloatValue Some bv) -> FloatValue (Some (float_of_int av /. bv))
  | (FloatValue Some av, BOP_Plus, IntValue Some bv) -> FloatValue (Some (av +. float_of_int bv))
  | (FloatValue Some av, BOP_Minus, IntValue Some bv) -> FloatValue (Some (av -. float_of_int bv))
  | (FloatValue Some av, BOP_Mult, IntValue Some bv) -> FloatValue (Some (av *. float_of_int bv))
  | (FloatValue Some av, BOP_Div, IntValue Some bv) -> FloatValue (Some (av /. float_of_int bv))

  | (IntValue Some av, BOP_Equal, IntValue Some bv) -> BoolValue (Some (av == bv))
  | (IntValue Some av, BOP_Lt, IntValue Some bv) -> BoolValue (Some (av < bv))
  | (IntValue Some av, BOP_Gt, IntValue Some bv) -> BoolValue (Some (av > bv))
  | (IntValue Some av, BOP_Lte, IntValue Some bv) -> BoolValue (Some (av <= bv))
  | (IntValue Some av, BOP_Gte, IntValue Some bv) -> BoolValue (Some (av >= bv))

  | (IntValue Some av, BOP_Equal, FloatValue Some bv) -> BoolValue (Some (float_of_int av == bv))
  | (IntValue Some av, BOP_Lt, FloatValue Some bv) -> BoolValue (Some (float_of_int av < bv))
  | (IntValue Some av, BOP_Gt, FloatValue Some bv) -> BoolValue (Some (float_of_int av > bv))
  | (IntValue Some av, BOP_Lte, FloatValue Some bv) -> BoolValue (Some (float_of_int av <= bv))
  | (IntValue Some av, BOP_Gte, FloatValue Some bv) -> BoolValue (Some (float_of_int av >= bv))

  | (FloatValue Some av, BOP_Equal, IntValue Some bv) -> BoolValue (Some (av == float_of_int bv))
  | (FloatValue Some av, BOP_Lt, IntValue Some bv) -> BoolValue (Some (av < float_of_int bv))
  | (FloatValue Some av, BOP_Gt, IntValue Some bv) -> BoolValue (Some (av > float_of_int bv))
  | (FloatValue Some av, BOP_Lte, IntValue Some bv) -> BoolValue (Some (av <= float_of_int bv))
  | (FloatValue Some av, BOP_Gte, IntValue Some bv) -> BoolValue (Some (av >= float_of_int bv))

  | (CharValue Some av, BOP_Equal, CharValue Some bv) -> BoolValue (Some (av == bv))
  | (PointerValue Some av, BOP_Equal, PointerValue Some bv) -> BoolValue (Some (av == bv))

  | _ -> raise (RuntimeError "invalid binary operation"))


let interp_builtin id args = 
  match id with 
  | "debug_print_context" -> 
      (fun context -> print_endline (show_context context);
         context, (Some Null))
  | "debug_println" -> 
      print_endline ("println " ^ string_of_value (List.hd args));
      return (Some Null)
  | "println" -> 
      let* s = show_value (List.hd args) in 
      print_endline s; return (Some Null)
  | "printf" -> 
      let* format = assert_string (List.hd args) in
      let s = printf_with_values format (List.tl args) in 
      print_endline s; return (Some Null)
  | _ -> return None

let interp_unaop o v = 
  return (match (o, v) with 
  | (UOP_Not, BoolValue Some x) -> BoolValue (Some (not x))
  | (UOP_Not, IntValue Some x) -> IntValue (Some (-x))
  | (UOP_Not, FloatValue Some x) -> FloatValue (Some (-. x))
  | _ -> raise (RuntimeError "invalid unary operation"))

let rec get_var_id var = 
  match var with 
  | VarIden id -> id 
  | VarAccess (v, _) -> get_var_id v

let write_symbol_table stack_or_heap var addr context = 
  let id = get_var_id var in
  {context with symbol_table={name=id; addr; scope=context.scope; stack_or_heap}::context.symbol_table}, ()

let look_symbol_table var context = 
  let id = get_var_id var in 
  context, List.find (fun ent -> String.compare ent.name id == 0 ) context.symbol_table

(* TODO: Implicit type casting *)
let write_at_addr addr value context = 
  let v = match context.memory.(addr), value with 
  | IntValue _, IntValue _ -> value
  | FloatValue _, IntValue (Some i) -> FloatValue (Some (float_of_int i))
  | FloatValue _, FloatValue _ -> value
  | CharValue _, CharValue _ -> value
  | BoolValue _, BoolValue _ -> value
  | PointerValue _, PointerValue _ -> value
  | Null, Null -> value
  | _ -> raise (RuntimeError ("writing to a memory with different type" )) in
  context.memory.(addr) <- v;
  context, ()

let read_at_addr addr context = context, context.memory.(addr)

let write_str_to_addr addrs string = 
  let charlist = charlist_of_string string in 
  let writer (addr, value) = write_at_addr addr value in 
  (zip addrs charlist) 
  |> List.map (fun (i, c) -> i, CharValue (Some c)) 
  |> fold_context writer 

let dereference_pointer addr context = 
  match context.memory.(addr) with 
  | PointerValue (Some p) -> context, p
  | PointerValue None -> raise (RuntimeError "dereferencing a null-pointer") 
  | _ -> raise (RuntimeError "dereferencing a non-pointer")

let rec read_variable variable = 
  let* entry = look_symbol_table variable in 
  read_memory entry.stack_or_heap entry.addr variable

and read_memory stack_or_heap addr var = 
  let (_, _, direction) = get_memory_characteristic stack_or_heap in  
  match var with 
  | VarIden _ -> read_at_addr addr
  | VarAccess (v, e) -> 
      let* pos = dereference_pointer (addr) in
      let* n = interp_expr e in 
      let n = assert_nat n in 
      read_memory stack_or_heap (pos + (direction * n)) v

and assign_var var value = 
  let* entry = look_symbol_table var in
  write_to_memory entry.stack_or_heap entry.addr var value

and write_to_memory stack_or_heap addr var value = 
  let (_, _, direction) = get_memory_characteristic stack_or_heap in 
  match var with 
  | VarIden _ -> write_at_addr addr value
  | VarAccess (v, e) -> 
      let* pos = dereference_pointer (addr) in
      let* n = interp_expr e in 
      let n = assert_nat n in 
      write_to_memory stack_or_heap (pos + direction * n) v value

and declare_var datatype var = 
  let* head = allocate_mems Stack datatype var in
  match var with 
  | VarIden _ -> write_symbol_table Stack var head
  | VarAccess _-> allocate_pointer Stack head >>= write_symbol_table Stack var

and declare_vars datatype = fold_context (declare_var datatype)

and allocate_mems stack_or_heap datatype var = 
  match var with 
  | VarIden _ -> allocate_mem stack_or_heap datatype  
  | VarAccess (v, e) -> 
      let* n = interp_expr e in 
      let n = assert_nat n in 
      if n == 0 then raise (RuntimeError "allocating 0 elements array") else
      let* addrs = repeat_with_context n (allocate_mems stack_or_heap datatype v) in
      match v with 
      | VarIden _ -> allocate_null stack_or_heap >>> return (List.hd addrs)
      | VarAccess _ -> allocate_pointers stack_or_heap addrs 

and interp_expr e = 
  match e with 
  | IntValue i -> return (IntValue (Some i))
  | FloatValue f -> return (FloatValue (Some f))
  | BinOpExpr (a, o, b) -> 
      let* av = interp_expr a in 
      let* bv = interp_expr b in 
      interp_binop av o bv
  | VariableExpr variable -> read_variable variable
  | StringValue string -> 
      let n = String.length string in 
      let* addrs = repeat_with_context n (allocate_mem Heap Char) in
      let* _ = allocate_null Heap in
      let* () = write_str_to_addr addrs string in
      return (PointerValue (Some (List.hd (addrs))))
  | PlusPlusExpr variable -> 
      let* value = read_variable variable in
      (match value with 
      | IntValue Some x -> 
          assign_var variable (IntValue (Some (x+1))) >>> return value
      | _ -> raise (RuntimeError "incrementing non int type"))
  | UnaOpExpr (o, e) -> 
      let* v = interp_expr e in interp_unaop o v
  | FuncCallExpr (id, arglist) -> 
      let* args = map_with_context interp_expr arglist in
      let* assignment_print = print_func_call id args in
      print_endline ("function call: " ^ assignment_print);
      let* builtin = interp_builtin id args in
      match builtin with 
      | Some v -> return v
      | None -> interp_funccall id args
  
and interp_funccall id args context = 
  let func = List.find (fun func -> 
    let (_, fid, _, _) = func.decl in String.compare fid id == 0
  ) context.funcs in
  let (_, _, params, scope) = func.decl in 
  {context with 
    scope=context.scope+1; 
    last_func_scope=(context.scope+1, context.stack_pos)::context.last_func_scope
  } |>
  let* () = fold_context assign_param (zip params args) in
  let* ret_val = until_return scope in
  return (match ret_val with 
    | Some x -> x
    | None -> Null) 

and interp_statement statement = 
  match statement with 
  | DeclarationStmt (datatype, var_list) -> declare_vars datatype var_list
  | AssignmentStmt (variable, expression) -> interp_expr expression >>= assign_var variable
  | ExpressionStmt expression -> interp_expr expression >>> return ()
  | IfStmt (expression, scope) -> 
      let* b = interp_expr expression in 
      let b = assert_bool b in 
      if b then interp_scope scope else return ()
  | ReturnStmt expression -> 
      let* opt_val = (match expression with 
        | Some expr -> let* v = interp_expr expr in return (Some v)
        | None -> return None) in
      let* assignment_print = print_return_call opt_val in
      print_endline ("return call: " ^ assignment_print);
      (match opt_val with 
      | Some v -> (fun context -> { context with return_value=Some v }, ())
      | None   -> (fun context -> {context with return_value=Some Null}, ()))
  | ForStmt (init_statement, expression, statement, scope) -> 
      let* _ = interp_statement init_statement in
      interp_while expression (scope @ [statement])

and interp_while expression scope = 
  let* v = interp_expr expression in
  let b = assert_bool v in
  if b then 
    interp_scope scope >>> interp_while expression scope
  else
    return ()

and interp_scope scope context =
  {context with scope=context.scope+1} |>
  let* () = fold_context interp_statement scope in 
  descope_to context.scope

and assign_param (parameter, value) = 
  let (datatype, iden) = parameter in 
  let* addr = allocate_mem Stack datatype in 
  let* () = write_symbol_table Stack (VarIden iden) addr in
  write_at_addr addr value

and until_return scope = 
  let* () = reset_return_value in
  match scope with 
  | [] -> descope_func >>> return None
  | h::t -> 
      let* _ = interp_statement h in 
      let* return_value = get_return_value in
      match return_value with 
      | None -> until_return t
      | Some _ -> descope_func >>> return return_value
 
let interp_func func context = {context with funcs = {decl=func}::context.funcs}, ()
let interp_program program = fold_context interp_func program 
let interpret program = 
  ( interp_program program >>> interp_expr (FuncCallExpr ("main", [])) ) 
  empty_context
