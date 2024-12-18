open SeaCaml

let () =
  let usage_msg = "Usage: ./parser <filename>" in
  let filename = ref "" in

  let set_file s = filename := s in
  Arg.parse [] set_file usage_msg;

  if !filename = "" then
    print_endline usage_msg
  else
    let in_channel = open_in !filename in
    let lexbuf = Lexing.from_channel in_channel in

    try
      let program = Parser.program Lexer.token lexbuf in
      close_in in_channel;
      print_endline "Parsing successful!";
      let program = Desugar.desugar_program program in
      print_endline (Stringify.stringify program);
      print_endline "\n";
      Typecheck.typecheck program;
      print_endline "Typecheck successful!";
      print_endline "Interpreting..."; 
      print_endline ""; 
      let (_, _) = Interp.interpret program in
      print_endline ""; 
      print_endline "Done!"; 
    with
    | Parser.Error ->
        close_in in_channel;
        let pos = lexbuf.lex_curr_p in
        Printf.eprintf "Syntax error at line %d, column %d\n" pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

