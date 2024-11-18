%{
  open Ast
%}

%token <int> INT_LITERAL 
%token <float> FLOAT_LITERAL
%token <string> STRING_LITERAL
%token <Ast.identifier> IDENTIFIER
%token INT FLOAT BOOL VOID IF FOR RETURN
%token PLUS MINUS STAR DIV EQ LT GT LTE GTE LAND LOR NOT AND INC ASSIGN
%token SEMICOLON LPAREN RPAREN LBRACKET RBRACKET LBRACE RBRACE COMMA EOF
%start <program> program
%type <datatype> datatype
%type <statement> statement
%type <expression> expression
%type <scope> scope

%%

program:
  | function_decl* EOF { $1 }

function_decl:
  datatype IDENTIFIER LPAREN parameters RPAREN scope
    { ($1, $2, $4, $6) }

datatype:
  | INT                      { Int }
  | FLOAT                    { Float }
  | BOOL                     { Bool }
  | VOID                     { Void }
  | datatype STAR            { Pointer($1) }

parameters:
  | /* empty */              { [] }
  | VOID                     { [] } 
  | parameter comma_params*  { $1 :: $2 }
comma_params: 
  | COMMA parameter          { $2 }

parameter:
  datatype IDENTIFIER        { ($1, $2) }

scope:
  | LBRACE statement* RBRACE { $2 }


statement: 
  | datatype variable_list SEMICOLON          { DeclarationStmt($1, $2) }
  | action SEMICOLON                          { $1 }
  | IF LPAREN expression RPAREN statement     { IfStmt($3, [$5]) }
  | IF LPAREN expression RPAREN scope         { IfStmt($3, $5) }
  | FOR LPAREN action SEMICOLON expression SEMICOLON action RPAREN statement 
                                              { ForStmt($3, $5, $7, [$9]) }
  | FOR LPAREN action SEMICOLON expression SEMICOLON action RPAREN scope
                                              { ForStmt($3, $5, $7, $9) }
  | RETURN expression SEMICOLON               { ReturnStmt($2) }

action:
  | variable ASSIGN expression                { AssignmentStmt($1, $3) }
  | expression                                { ExpressionStmt($1) }

variable:
  | IDENTIFIER                                { VarIden ($1) }
  | variable LBRACKET expression RBRACKET     { VarAccess ($1, $3) }

variable_list:
  | variable comma_variable*  { $1 :: $2 }
comma_variable: 
  | COMMA variable            { $2 }

bin_op: 
  | PLUS      {BOP_Plus}
  | MINUS     {BOP_Minus}
  | STAR      {BOP_Mult}
  | DIV       {BOP_Div}
  | EQ        {BOP_Equal}
  | LT        {BOP_Lt}
  | GT        {BOP_Gt}
  | LTE       {BOP_Lte}
  | GTE       {BOP_Gte}
  | LAND      {BOP_LAnd}
  | LOR       {BOP_LOr}

una_op: 
  | NOT       {UOP_Not}
  | MINUS     {UOP_Neg}
  | AND       {UOP_And}

expression:
  | STRING_LITERAL                          { StringValue($1) }
  | INT_LITERAL                             { IntValue($1) }
  | FLOAT_LITERAL                           { FloatValue($1) }
  | variable                                { VariableExpr($1) }
  | variable INC                            { PlusPlusExpr($1) }
  | expression bin_op expression            { BinOpExpr($1, $2, $3) }
  | una_op expression                       { UnaOpExpr($1, $2) }
  | IDENTIFIER LPAREN expressions RPAREN    { FuncCallExpr($1, $3) }
  | LPAREN expression RPAREN                { $2 }

expressions:
  | /* empty */                   { [] }
  | expression comma_expr*        { $1 :: $2 }
comma_expr: 
  | COMMA expression                    { $2 }

