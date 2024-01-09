open Types

type enventry =
  | VarEntry of {
    access: Translate.access;
    ty: ty
  }
  | FunEntry of {
    level: Translate.level;
    label: Temp.label;
    formals: ty list;
    result: ty
  }

type tenv = ty Symbol.table
type venv = enventry Symbol.table

(* the default tenv with standard types added *)
let base_tenv : tenv =
  let types =
    [
      ("int", INT);
      ("string", STRING);
    ]
  in
  let add_type tenv (name, ty) =
    Symbol.enter tenv (Symbol.symbol name) ty
  in
  List.fold_left add_type Symbol.empty types

(* the default venv with standard library functions added *)
let base_venv : venv =
  let functions =
    [
      ("print", [STRING], UNIT);
      ("flush", [], UNIT);
      ("getchar", [], STRING);
      ("ord", [STRING], INT);
      ("chr", [INT], STRING);
      ("size", [STRING], INT);
      ("substring", [STRING; INT; INT], STRING);
      ("concat", [STRING; STRING], STRING);
      ("not", [INT], INT);
      ("exit", [INT], UNIT);
    ]
  in
  let add_function venv (name, formals, result) =
    let entry =
      FunEntry {
        formals;
        result;
        level = Translate.outermost;
        label = Temp.newlabel ();
      }
    in
    Symbol.enter venv (Symbol.symbol name) entry
  in
  List.fold_left add_function Symbol.empty functions

(* for debugging *)
let string_of_tenv (tenv:tenv) : string =
  Symbol.string_of_table tenv string_of_ty
