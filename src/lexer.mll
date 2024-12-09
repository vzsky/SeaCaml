{
  open Parser 
  open Utils
}

let lineComment = "//" [^'\r' '\n']*
let blockComment = "/*" ([^'*']* ('*'[^'/'])*)* "*/"

rule token = parse
  | [' ' '\t' '\n' '\r']      { token lexbuf }      (* Skip whitespace *)
  | "int"                     { INT }
  | "float"                   { FLOAT }
  | "bool"                    { BOOL }
  | "void"                    { VOID }
  | "if"                      { IF }
  | "for"                     { FOR }
  | "return"                  { RETURN }
  | "&&"                      { LAND }
  | "||"                      { LOR }
  | "=="                      { EQ }
  | "<="                      { LTE }
  | ">="                      { GTE }
  | "<"                       { LT }
  | ">"                       { GT }
  | "+"                       { PLUS }
  | "-"                       { MINUS }
  | "*"                       { STAR }
  | "/"                       { DIV }
  | "!"                       { NOT }
  | "&"                       { AND }
  | "++"                      { INC }
  | "="                       { ASSIGN }
  | ";"                       { SEMICOLON }
  | "("                       { LPAREN }
  | ")"                       { RPAREN }
  | "["                       { LBRACKET }
  | "]"                       { RBRACKET }
  | "{"                       { LBRACE }
  | "}"                       { RBRACE }
  | ","                       { COMMA }

  | "+="                      { PLUS_EQ }
  | "-="                      { MINUS_EQ }
  | "while"                       { WHILE }

  | ['a'-'z' 'A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9' '_']* as id
                              { IDENTIFIER (id |> String.to_seq |> List.of_seq |> string_of_charlist) }
  | ['0'-'9']+ '.' ['0'-'9']+ as float
                              { FLOAT_LITERAL (float_of_string float) }
  | ['0'-'9']+ as int         { INT_LITERAL (int_of_string int) }
  | '"' [^ '"']* '"' as str   { STRING_LITERAL str }
  | lineComment               { token lexbuf }
  | blockComment              { token lexbuf }
  | eof                       { EOF }
  | _                         { failwith "Unrecognized character" }

