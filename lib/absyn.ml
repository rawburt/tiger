type pos = Lexing.position
type symbol = Symbol.symbol

type var =
  | SimpleVar of symbol * pos
  | FieldVar of var * symbol * pos
  | SubscriptVar of var * exp * pos

and exp =
  | VarExp of var
  | NilExp
  | IntExp of int
  | StringExp of string * pos
  | CallExp of { func: symbol; args: exp list; pos: pos }
  | OpExp of { left: exp; oper: oper; right: exp; pos: pos }
  | RecordExp of { fields: (symbol * exp * pos) list; typ: symbol; pos: pos }
  | SeqExp of (exp * pos) list
  | AssignExp of { var: var; exp: exp; pos: pos }
  | IfExp of { test: exp; then': exp; else': exp option; pos: pos }
  | WhileExp of { test: exp; body: exp; pos: pos }
  | ForExp of { var: symbol; escape: bool ref; lo: exp; hi: exp; body: exp; pos: pos }
  | BreakExp of pos
  | LetExp of { decs: dec list; body: exp; pos: pos }
  | ArrayExp of { typ: symbol; size: exp; init: exp; pos: pos }

and dec =
  | FunctionDec of fundec list
  | VarDec of { name: symbol; escape: bool ref; typ: (symbol * pos) option; init: exp; pos: pos }
  | TypeDec of tydec list

and tydec = {
  tydec_name: symbol;
  tydec_ty: ty;
  tydec_pos: pos
}

and field = {
  field_name: symbol;
  field_escape: bool ref;
  field_typ: symbol;
  field_pos: pos
}

and fundec = {
  fundec_name: symbol;
  fundec_params: field list;
  fundec_result: (symbol * pos) option;
  fundec_body: exp;
  fundec_pos: pos
}

and oper =
  | PlusOp | MinusOp | TimesOp | DivideOp
  | EqOp | NeqOp | LtOp | LeOp | GtOp | GeOp

and ty =
  | NameTy of symbol * pos
  | RecordTy of field list
  | ArrayTy of symbol * pos

let rec string_of_var = function
| SimpleVar (symbol, _) -> Symbol.name symbol
| FieldVar (var, symbol, _) ->
  let parent = string_of_var var in
  parent ^ "." ^ Symbol.name symbol
| SubscriptVar (var, _, _) ->
  let parent = string_of_var var in
  parent ^ "[]"

let string_of_oper = function
  | PlusOp -> "+"
  | MinusOp -> "-"
  | TimesOp -> "*"
  | DivideOp -> "/"
  | EqOp -> "="
  | NeqOp -> "<>"
  | LtOp -> "<"
  | LeOp -> "<="
  | GtOp -> ">"
  | GeOp -> ">="
