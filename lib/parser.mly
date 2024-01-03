%{
  open Absyn
%}

%token <string> ID
%token <string> STRING
%token <int> INT
%token TYPE
%token VAR
%token FUNCTION
%token BREAK
%token OF
%token END
%token IN
%token NIL
%token LET
%token DO
%token TO
%token FOR
%token WHILE
%token ELSE
%token THEN
%token IF
%token ARRAY
%token ASSIGN
%token OR
%token AND
%token GE
%token GT
%token LE
%token LT
%token NEQ
%token EQ
%token DIVIDE
%token TIMES
%token MINUS
%token PLUS
%token DOT
%token RBRACE
%token LBRACE
%token RBRACK
%token LBRACK
%token RPAREN
%token LPAREN
%token SEMICOLON
%token COLON
%token COMMA
%token EOF

%nonassoc OF
%nonassoc THEN
%nonassoc ELSE
%nonassoc DO
%nonassoc ASSIGN
%left OR
%left AND
%nonassoc EQ NEQ GT GE LT LE
%left PLUS MINUS
%left TIMES DIVIDE

%start program

%type <exp> program

%%

let bracketed(x) == delimited(LBRACK, x, RBRACK)
let braced(x)    == delimited(LBRACE, x, RBRACE)
let parened(x)   == delimited(LPAREN, x, RPAREN)

program:
  | exp EOF { $1 }

exp:
  | primitive   { $1 }
  | nil         { $1 }
  | break       { $1 }
  | create_rec  { $1 }
  | create_arr  { $1 }
  | var         { $1 }
  | assignment  { $1 }
  | local       { $1 }
  | conditional { $1 }
  | loop        { $1 }
  | call        { $1 }
  | unary       { $1 }
  | binary      { $1 }
  | bool        { $1 }
  | seq         { $1 }

primitive:
  | STRING  { StringExp ($1, $startpos) }
  | INT     { IntExp ($1) }

nil:
  | NIL     { NilExp }

break:
  | BREAK   { BreakExp $startpos }

symbol:
  | ID      { Symbol.symbol $1 }

create_rec:
  | symbol braced(create_rec_fields)
    { RecordExp { fields = $2; typ = $1 ; pos = $startpos } }

create_rec_fields:
  | separated_list(COMMA, create_rec_field) { $1 }

create_rec_field:
  | symbol EQ exp { ($1, $3, $startpos) }

create_arr:
  | symbol bracketed(exp) OF exp
    { ArrayExp { typ = $1; size = $2; init = $4; pos = $startpos } }

var:
  | lvalue { VarExp $1 }

lvalue:
  | symbol          { SimpleVar ($1, $startpos) }
  | lvalue_complex  { $1 }

lvalue_complex:
  | symbol DOT symbol
    { FieldVar (SimpleVar ($1, $startpos), $3, $startpos) }
  | lvalue_complex DOT symbol
    { FieldVar ($1, $3, $startpos) }
  | symbol bracketed(exp)
    { SubscriptVar (SimpleVar ($1, $startpos), $2, $startpos) }
  | lvalue_complex bracketed(exp)
    { SubscriptVar ($1, $2, $startpos) }

assignment:
  | lvalue ASSIGN exp
    { AssignExp { var = $1; exp = $3; pos = $startpos } }

local:
  | LET decs IN exp_pos_seq END
    { LetExp { decs = $2; body = SeqExp $4; pos = $startpos } }

decs:
  | list(dec) { $1 }

dec:
  | var_dec     { $1 }
  | dec_ty_fun  { $1 }

var_dec:
  | VAR symbol ASSIGN exp
    { VarDec { name = $2; escape = ref true; typ = None; init = $4; pos = $startpos } }
  | VAR symbol COLON symbol ASSIGN exp
    { VarDec { name = $2; escape = ref true; typ = Some ($4, $startpos); init = $6; pos = $startpos } }

dec_ty_fun:
  | nonempty_list(ty_dec)   { TypeDec $1 }
  | nonempty_list(fun_dec)  { FunctionDec $1 }

ty_dec:
  | TYPE symbol EQ ty
    { { tydec_name = $2; tydec_ty = $4; tydec_pos = $startpos } }

ty:
  | braced(ty_fields)   { RecordTy $1 }
  | ARRAY OF symbol     { ArrayTy ($3, $startpos) }
  | symbol              { NameTy ($1, $startpos) }

ty_fields:
  | separated_list(COMMA, ty_field) { $1 }

ty_field:
  | symbol COLON symbol
    { { field_name = $1; field_escape = ref true; field_typ = $3; field_pos = $startpos } }

fun_dec:
  | FUNCTION symbol fun_params EQ exp
    {
      {
        fundec_name = $2;
        fundec_params = $3;
        fundec_result = None;
        fundec_body = $5;
        fundec_pos = $startpos
      }
    }
  | FUNCTION symbol fun_params COLON symbol EQ exp
    {
      {
        fundec_name = $2;
        fundec_params = $3;
        fundec_result = Some ($5, $startpos);
        fundec_body = $7;
        fundec_pos = $startpos
      }
    }

fun_params:
  | parened(ty_fields) { $1 }

conditional:
  | IF exp THEN exp ELSE exp
    { IfExp { test = $2; then' = $4; else' = Some $6; pos = $startpos } }
  | IF exp THEN exp
    { IfExp { test = $2; then' = $4; else' = None; pos = $startpos } }

loop:
  | while_loop  { $1 }
  | for_loop    { $1 }

while_loop:
  | WHILE exp DO exp
    { WhileExp { test = $2; body = $4; pos = $startpos } }

for_loop:
  | FOR symbol ASSIGN exp TO exp DO exp
    { ForExp { var = $2; escape = ref true; lo = $4; hi = $6; body = $8; pos = $startpos } }

call:
  | symbol parened(fun_args)
    { CallExp { func = $1; args = $2; pos = $startpos } }

fun_args:
  | separated_list(COMMA, exp) { $1 }

unary:
  | MINUS exp
    { OpExp { left = IntExp 0; oper = MinusOp; right = $2; pos = $startpos } }

binary:
  | exp binop exp
    { OpExp { left = $1; oper = $2; right = $3; pos = $startpos } }

%inline binop:
  | PLUS { PlusOp }
  | MINUS { MinusOp }
  | TIMES { TimesOp }
  | DIVIDE { DivideOp }
  | EQ { EqOp }
  | NEQ { NeqOp }
  | LT { LtOp }
  | LE { LeOp }
  | GT { GtOp }
  | GE { GeOp }

bool:
  | exp AND exp
    { IfExp { test = $1; then' = $3; else' = Some (IntExp 0); pos = $startpos } }
  | exp OR exp
    { IfExp { test = $1; then' = IntExp 1; else' = Some ($3); pos = $startpos } }

exp_pos:
  | exp { ($1, $startpos) }

exp_pos_seq:
  | separated_list(SEMICOLON, exp_pos) { $1 }

seq:
  | parened(exp_pos_seq) { SeqExp $1 }
