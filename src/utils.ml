exception Exception of string

let string_of_option f opt = 
  match opt with 
  | None -> "_"
  | Some x -> f x

let string_of_char = String.make 1

let string_of_bool b = if b then "T" else "F"

let rec string_of_charlist l =
  match l with
    [] -> ""
  | h::t -> string_of_char h ^ string_of_charlist t;;

let rec last l = 
  match l with 
  | [] -> raise (Exception "no last in empty list")
  | [a] -> a
  | _::t -> last t
  
let unwords s l = String.concat s l;;
