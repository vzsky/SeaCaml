%{
  open Ast
  open Utils
%}

%token <int> INT_LITERAL 
%token <float> FLOAT_LITERAL
%token <string> STRING_LITERAL
%token <CoreAst.identifier> IDENTIFIER
%token INT FLOAT BOOL VOID IF FOR RETURN
%token PLUS MINUS STAR DIV EQ LT GT LTE GTE LAND LOR NOT AND INC ASSIGN
%token SEMICOLON LPAREN RPAREN LBRACKET RBRACKET LBRACE RBRACE COMMA EOF
%token PLUS_EQ MINUS_EQ WHILE ELSE
%start <program> program
%type <CoreAst.datatype> datatype
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
  LBRACE statement* RBRACE { $2 }

var_eq_exp: 
  | variable ASSIGN expression                  {($1, Some $3)}
  | variable                                    {($1, None)}
var_eq_exp_list: 
  var_eq_exp comma_var_eq_exp*                  {$1 :: $2}
comma_var_eq_exp:
  COMMA var_eq_exp                              {$2}

statement_or_scope: 
  | statement     { [$1] }
  | scope         {  $1  } 

statement: 
  | action SEMICOLON                                    { $1 }
  | IF LPAREN expression RPAREN statement_or_scope      { IfStmt($3, $5, []) }
  | RETURN expression? SEMICOLON                        { ReturnStmt($2) }
  | WHILE LPAREN expression RPAREN statement_or_scope   { WhileStmt($3, $5) }

  | FOR LPAREN action SEMICOLON expression SEMICOLON 
    action RPAREN statement_or_scope                    { ForStmt($3, $5, $7, $9) }
  | IF LPAREN expression RPAREN statement_or_scope 
    ELSE statement_or_scope                             { IfStmt($3, $5, $7) }

action:
  | variable ASSIGN expression                          { AssignmentStmt($1, $3) }
  | expression                                          { ExpressionStmt($1) }
  | datatype var_eq_exp_list                            { DeclareAssignStmt ($1, $2) }

variable:
  | IDENTIFIER                                          { VarIden ($1) }
  | variable LBRACKET expression RBRACKET               { VarAccess ($1, $3) }

variable_list:
  | variable comma_variable*  { $1 :: $2 }
comma_variable: 
  | COMMA variable            { $2 }

bin_op: 
  | PLUS      {CoreAst.BOP_Plus}
  | MINUS     {CoreAst.BOP_Minus}
  | STAR      {CoreAst.BOP_Mult}
  | DIV       {CoreAst.BOP_Div}
  | EQ        {CoreAst.BOP_Equal}
  | LT        {CoreAst.BOP_Lt}
  | GT        {CoreAst.BOP_Gt}
  | LTE       {CoreAst.BOP_Lte}
  | GTE       {CoreAst.BOP_Gte}
  | LAND      {CoreAst.BOP_LAnd}
  | LOR       {CoreAst.BOP_LOr}

una_op: 
  | NOT       {CoreAst.UOP_Not}
  | MINUS     {CoreAst.UOP_Neg}
  | AND       {CoreAst.UOP_And}

compound_op: 
  | PLUS_EQ   {Com_PlusEq}
  | MINUS_EQ  {Com_MinusEq}

expression:
  | STRING_LITERAL                          { StringValue(remove_quotes $1) }
  | INT_LITERAL                             { IntValue($1) }
  | FLOAT_LITERAL                           { FloatValue($1) }
  | variable                                { VariableExpr($1) }
  | variable INC                            { PlusPlusExpr($1) }
  | variable compound_op expression         { CompoundExpr($1, $2, $3) }
  | expression bin_op expression            { BinOpExpr($1, $2, $3) }
  | una_op expression                       { UnaOpExpr($1, $2) }
  | IDENTIFIER LPAREN expressions RPAREN    { FuncCallExpr($1, $3) }
  | LPAREN expression RPAREN                { $2 }


expressions:
  | /* empty */                   { [] }
  | expression comma_expr*        { $1 :: $2 }
comma_expr: 
  | COMMA expression                    { $2 }

