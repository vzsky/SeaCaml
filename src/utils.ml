let rec string_of_charlist l =
  match l with
    [] -> ""
  | h::t -> String.make 1 h ^ string_of_charlist t;;
