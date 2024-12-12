exception Exception of string

let remove_quotes s =
  let len = String.length s in
  if len >= 2 && s.[0] = '"' && s.[len - 1] = '"' then
    String.sub s 1 (len - 2)
  else s

let string_of_option f opt = 
  match opt with 
  | None -> "_"
  | Some x -> f x

let string_of_char = String.make 1

let string_of_iint (a, b) = 
  (string_of_int a) ^ ", " ^ (string_of_int b)

let string_of_bool b = if b then "T" else "F"

let rec string_of_charlist l =
  match l with
    [] -> ""
  | h::t -> string_of_char h ^ string_of_charlist t;;

let charlist_of_string s = 
  let rec trav l i =
    if i = l then [] else s.[i]::trav l (i+1)
  in
  trav (String.length s) 0;;

let rec last l = 
  match l with 
  | [] -> raise (Exception "no last in empty list")
  | [a] -> a
  | _::t -> last t
  
let unwords s l = String.concat s l;;

let zip lst1 lst2 =
  List.map2 (fun x y -> (x, y)) lst1 lst2

let rec iota n = if n <= 0 then [] else iota (n - 1) @ [n]


module ContextMonad (C: sig type t end) = struct
  type context = C.t
  type 'a ctxMonad = context -> context * 'a

  let return (x : 'a) : 'a ctxMonad =
    fun ctx -> (ctx, x)

  let bind (m : 'a ctxMonad) (f : 'a -> 'b ctxMonad) : 'b ctxMonad =
    fun ctx ->
      let (ctx', a) = m ctx in f a ctx'

  let ( let* ) = bind
  let (>>=) = bind

  let (>>>) (f: 'a ctxMonad) (g: 'b ctxMonad) = let* _ = f in g

end
