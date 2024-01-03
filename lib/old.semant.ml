open Absyn
open Types
open Env

(* ============ ERRORS *)

exception SemantError of string * pos

let error msg pos = raise (SemantError (msg, pos))

let type_not_found symbol pos =
  error
    (Printf.sprintf "type not found '%s'" (Symbol.name symbol))
    pos

let symbol_not_found symbol pos =
  error
    (Printf.sprintf "symbol not found '%s'" (Symbol.name symbol))
    pos

let field_not_found fields symbol pos =
  error
    (Printf.sprintf "field '%s' not found in %s" (Symbol.name symbol) (string_of_ty (RECORD (fields, 0))))
    pos

let type_mismatch expected got pos =
  error
    (Printf.sprintf "expected type '%s' but got '%s'" expected (string_of_ty got))
    pos

let type_mismatch_ty expected got pos =
  error
    (Printf.sprintf "expected type '%s' but got '%s'" (string_of_ty expected) (string_of_ty got))
    pos

let not_a_function func pos =
  error
    (Printf.sprintf "symbol not a function '%s'" (Symbol.name func))
    pos

let argument_mismatch func pos =
  error
    (Printf.sprintf "wrong number of arguments for function '%s'" (Symbol.name func))
    pos

let fields_missing expected given pos =
  let given = List.map (fun (s, t, _) -> (s, t)) given in
  error
    (Printf.sprintf "record fields missing.\ngiven: %s\nexpected: %s"
      (string_of_ty (RECORD (given, 0)))
      (string_of_ty (RECORD (expected, 0))))
    pos

let cant_be_nil name_str ty pos =
  error
    (Printf.sprintf "only RECORD's can be NIL but '%s' is type '%s'" name_str (string_of_ty ty))
    pos

let record_mismatch name pos =
  error
    (Printf.sprintf "RECORD mismatch for '%s" name)
    pos

let array_mismatch name pos =
  error
    (Printf.sprintf "ARRAY mismatch for '%s" name)
    pos

(* ============ HELPERS *)

let assert_types expected got pos =
  if expected <> got then type_mismatch_ty expected got pos

let assert_int ty pos = assert_types INT ty pos

let is_record = function
  | RECORD _ -> true
  | _ -> false

(* ============ TYPES *)

type expty = { t_exp: Translate.t_exp; ty: ty }

type transenv = { venv: Env.venv; tenv: Env.tenv }

(* ============ FUNCTIONS *)

(*
   function args are order dependent.
   compare arg types. fails if arg length is mismatched.
*)
let rec compare_function_types func pos = function
  | ([], []) -> ()
  | ([], _) | (_, []) -> argument_mismatch func pos
  | (arg :: args, formal :: formals) ->
    if arg <> formal
      then type_mismatch_ty arg formal pos
      else compare_function_types func pos (args, formals)

(*
   record fields are not order dependent.
   for each given field, find the stored field and compare.
   compare lengths of both field lists.
*)
let compare_record_fields
    (stored : (symbol * ty) list)
    (given : (symbol * ty * pos) list)
    (pos : pos)
    : unit =
  let find_field sym =
    List.find_opt (fun (s, _) -> s = sym) stored
  in
  let comparef storedf givenf =
    let (_, stored_ty) = storedf in
    let (_, given_ty, given_pos) = givenf in
      assert_types stored_ty given_ty given_pos;
  in
  let find_and_compare givenf =
    let (given_sym, _, given_pos) = givenf in
    match find_field given_sym with
    | None -> field_not_found stored given_sym given_pos
    | Some storedf -> comparef storedf givenf
  in
  List.iter find_and_compare given;
  if List.length stored <> List.length given
    then fields_missing stored given pos

(* translate a Absyn.var into Types.ty *)
let rec trans_var venv tenv var : expty =

  let trans_simple_var symbol pos =
      match Symbol.look venv symbol with
      | None -> symbol_not_found symbol pos
      | Some (VarEntry { ty }) -> { t_exp = (); ty }
      | Some (FunEntry { result; _ }) -> { t_exp = (); ty = result }
  in

  let trans_field_var parent symbol pos =
    let trans_var_parent = trans_var venv tenv parent in
    match trans_var_parent.ty with
    | RECORD (field_list, _) ->
      begin
        match List.find_opt (fun (s, _) -> Symbol.eq s symbol) field_list with
        | None -> field_not_found field_list symbol pos
        | Some (_, ty) -> { t_exp = (); ty }
      end
    | ty -> type_mismatch "RECORD" ty pos
  in

  let trans_subscript_var parent exp pos =
    let trans_var_parent = trans_var venv tenv parent in
    match trans_var_parent.ty with
    | ARRAY (ty, _) ->
      let subty = (trans_exp venv tenv exp).ty in
      assert_int subty pos;
      { t_exp = (); ty }
    | ty -> type_mismatch "ARRAY" ty pos
  in

  match var with
  | SimpleVar (symbol, pos) -> trans_simple_var symbol pos
  | FieldVar (parent, symbol, pos) -> trans_field_var parent symbol pos
  | SubscriptVar (parent, exp, pos) -> trans_subscript_var parent exp pos

(* translate a Absyn.exp into an `expty` *)
and trans_exp
    (venv : venv)
    (tenv : tenv)
    (exp : exp)
    : expty =

  (*
    `func` should be a function.
    translate all args and compare to stored `formals`.
  *)
  let trans_call_exp func args pos =
    match Symbol.look venv func with
    | Some (FunEntry { formals; result }) ->
      let trans_arg exp = (trans_exp venv tenv exp).ty in
      let args_tys = List.map trans_arg args in
      compare_function_types func pos (args_tys, formals);
      { t_exp = (); ty = result }
    | Some _ -> not_a_function func pos
    | None -> symbol_not_found func pos
  in

  (*
     `=` and `<>` compare general types.
     all other binops require INT for `left` and `right`.
  *)
  let trans_op_exp left oper right pos =
    let left_ty = (trans_exp venv tenv left).ty in
    let right_ty = (trans_exp venv tenv right).ty in
    match oper with
    | PlusOp | MinusOp | TimesOp | DivideOp
    | LtOp | LeOp | GtOp | GeOp ->
      assert_int left_ty pos;
      assert_int right_ty pos;
      { t_exp = (); ty = INT }
    | EqOp | NeqOp ->
      match (left_ty, right_ty) with
      (* RECORD can compare against NIL *)
      | RECORD _, NIL | NIL, RECORD _ -> { t_exp = (); ty = INT }
      | _, _ ->
        if left_ty = right_ty
          then { t_exp = (); ty = INT }
          else type_mismatch_ty left_ty right_ty pos
  in

  (*
     `typ` should be RECORD.
     all field names and types should match RECORD definition.
  *)
  let trans_record_exp field_exps typ pos =
    match actual_ty tenv typ pos with
    | RECORD (field_tys, _) as r ->
      let trans_field (sym, exp, p) =
        let t = trans_exp venv tenv exp in
        let ty = t.ty in
        (sym, ty, p)
      in
      let fexps = List.map trans_field field_exps in
      compare_record_fields field_tys fexps pos;
      { t_exp = (); ty = r }
    | t -> type_mismatch "RECORD" t pos
  in

  (* type check all exp's in the sequence *)
  let rec trans_seq_exp = function
    | [] -> { t_exp = (); ty = UNIT }
    | [(e, _)] ->
      let trans = trans_exp venv tenv e in
      { t_exp = (); ty = trans.ty }
    | (e, _) :: exps ->
      let _ = trans_exp venv tenv e in
      trans_seq_exp exps
  in

  let trans_assign_exp var exp pos =
    let var_ty = (trans_var venv tenv var).ty in
    let exp_ty = (trans_exp venv tenv exp).ty in
    (* only RECORDS can be NIL *)
    if exp_ty = NIL
      then if is_record var_ty
        then ()
        else cant_be_nil (string_of_var var) var_ty pos
      else assert_types var_ty exp_ty pos;
    (* assignment produces no value *)
    { t_exp = (); ty = UNIT }
  in

  (*
     `test` should be INT.
     `then` and `else` should be same type.
  *)
  let trans_if_exp test then' else' pos =
    let test_ty = (trans_exp venv tenv test).ty in
    assert_int test_ty pos;
    let then_ty = (trans_exp venv tenv then').ty in
    match else' with
    | None ->
      (* without else the `then` branch should be UNIT *)
      assert_types UNIT then_ty pos;
      { t_exp = (); ty = UNIT }
    | Some else_exp ->
      let else_ty = (trans_exp venv tenv else_exp).ty in
      assert_types then_ty else_ty pos;
      { t_exp = (); ty = then_ty }
  in

  (*
     `test` should be INT.
     `body` should type check.
     `body` should be UNIT.
  *)
  let trans_while_exp test body pos =
    let test_ty = (trans_exp venv tenv test).ty in
    assert_int test_ty pos;
    let body_ty = (trans_exp venv tenv body).ty in
    assert_types UNIT body_ty pos;
    { t_exp = (); ty = UNIT }
  in

  (*
     `lo` should be INT.
     `hi` should be INT.
     set `var` to INT in type environment.
     `body` should type check.
  *)
  let trans_for_exp var lo hi body pos =
    let lo_ty = (trans_exp venv tenv lo).ty in
    assert_int lo_ty pos;
    let hi_ty = (trans_exp venv tenv hi).ty in
    assert_int hi_ty pos;
    let venv' = Symbol.enter venv var (VarEntry { ty = INT }) in
    let _ = trans_exp venv' tenv body in
    { t_exp = (); ty = UNIT }
  in

  (* update environments based on `decs` and then translate body *)
  let trans_let_exp decs body =
    let foldfn accm dec =
      let (v, t) = accm in
      let envs = trans_dec v t dec in
      (envs.venv, envs.tenv)
    in
    let (venv', tenv') = List.fold_left foldfn (venv, tenv) decs in
    let body_ty = (trans_exp venv' tenv' body).ty in
    { t_exp = (); ty = body_ty }
  in

  (*
     `typ` should be an ARRAY (t, _).
     `size` should be an INT.
     `init` should be `t` from ARRAY definition.
  *)
  let trans_array_exp typ size init pos =
    match actual_ty tenv typ pos with
    | ARRAY (ty, _) as t ->
      let size_ty = (trans_exp venv tenv size).ty in
      assert_int size_ty pos;
      let init_ty = (trans_exp venv tenv init).ty in
      assert_types ty init_ty pos;
      { t_exp = (); ty = t }
    | t -> type_mismatch "ARRAY" t pos
  in

  (* one match to rule them all *)
  match exp with
  | VarExp var -> trans_var venv tenv var
  | NilExp ->  { t_exp = (); ty = NIL }
  | IntExp _ -> { t_exp = (); ty = INT }
  | StringExp _ -> { t_exp = (); ty = STRING }
  | CallExp { func; args; pos } -> trans_call_exp func args pos
  | OpExp { left; oper; right; pos } -> trans_op_exp left oper right pos
  | RecordExp { fields; typ; pos } -> trans_record_exp fields typ pos
  | SeqExp exp_list -> trans_seq_exp exp_list
  | AssignExp { var; exp; pos } -> trans_assign_exp var exp pos
  | IfExp { test; then'; else'; pos } -> trans_if_exp test then' else' pos
  | WhileExp { test; body; pos } -> trans_while_exp test body pos
  | ForExp { var; lo; hi; body; pos; _ } -> trans_for_exp var lo hi body pos
  (* TODO: check that break is inside `while` or `for` *)
  | BreakExp _ -> { t_exp = (); ty = UNIT }
  | LetExp { decs; body; _ } -> trans_let_exp decs body
  | ArrayExp { typ; size; init; pos } -> trans_array_exp typ size init pos

(* translate a Absyn.dec into a `transenv` *)
and trans_dec venv tenv dec : transenv =

  let trans_var_dec_typed name ty init pos : transenv =
    let symname = Symbol.name name in
    (* ty can't be NIL *)
    if ty = NIL then cant_be_nil symname NIL pos;
    let init_ty = (trans_exp venv tenv init).ty in
    (* records and arrays must match unique *)
    begin
      match (ty, init_ty) with
      | RECORD (_, u1), RECORD (_, u2) -> if u1 <> u2 then record_mismatch symname pos
      (* RECORD can initialize to NIL *)
      | RECORD _, NIL -> ()
      | ARRAY (_, u1), ARRAY (_, u2) -> if u1 <> u2 then array_mismatch symname pos
      | _ -> assert_types ty init_ty pos
    end;
    (* use `ty` and not `init_ty` in order to defined as RECORD if RECORD is initialized to NIL *)
    let venv' = Symbol.enter venv name (VarEntry { ty }) in
  { venv = venv'; tenv }
  in

  let trans_var_dec_infer name init pos : transenv =
    let init_ty = (trans_exp venv tenv init).ty in
    if init_ty = NIL then cant_be_nil (Symbol.name name) NIL pos;
    let venv' = Symbol.enter venv name (VarEntry { ty = init_ty }) in
    { venv = venv'; tenv }
  in

  (*
    compute type of `init`.
    if `typ` given, compare the types.
    add `name` to `venv` as VarEnty with computed type.
  *)
  let trans_var_dec name typ init pos : transenv =
    match typ with
    | Some (symbol, pos) ->
      let ty = actual_ty tenv symbol pos in
      trans_var_dec_typed name ty init pos
    | None -> trans_var_dec_infer name init pos
  in

  (*
    bind `dec.tydec_name` to `dec.tydec_ty`
    TODO: recursive types
  *)
  let trans_type_dec venv tenv dec : (venv * tenv) =
    (* TODO *)
    let ty = trans_ty tenv dec.tydec_ty in
    let tenv' = Symbol.enter tenv dec.tydec_name ty in
    (venv, tenv')
  in

  (* fold over the list of tydecs *)
  let trans_type_decs tydecs : transenv =
    let foldf (v, t) dec =
      trans_type_dec v t dec
    in
    let (venv', tenv') = List.fold_left foldf (venv, tenv) tydecs in
    { venv = venv'; tenv = tenv' }
  in

  (*
    translate param types
    translate body type
    compare return type if given
  *)
  let trans_function_dec (venv: venv) tenv fdec : (venv * tenv) =
    (* determine param types *)
    let trans_field field =
      let ty = tenv_lookup tenv field.field_typ field.field_pos in
      (field.field_name, ty)
    in
    let typed_fields =
      List.map trans_field fdec.fundec_params
    in
    let add_field_ty venv (sym, ty) =
      Symbol.enter venv sym (VarEntry { ty = ty})
    in
    (* add params to venv before translating body *)
    let venv' =
      List.fold_left add_field_ty venv typed_fields
    in
    (* compare body and return type *)
    (* add function to venv *)
    let trans_body_ty v = (trans_exp v tenv fdec.fundec_body).ty in
    let add_fun_to_venv v name formals result =
      Symbol.enter v name (FunEntry { formals; result })
    in
    let formals = List.map snd typed_fields in
    match fdec.fundec_result with
    | Some (result, pos) ->
      let result_ty = tenv_lookup tenv result pos in
      (* add function to venv before typecheck for recursive functions *)
      let venv'' = add_fun_to_venv venv' fdec.fundec_name formals result_ty in
      assert_types result_ty (trans_body_ty venv'') fdec.fundec_pos;
      (venv'', tenv)
    | None ->
      (* add function to venv before typecheck for recursive functions *)
      let venv'' = add_fun_to_venv venv' fdec.fundec_name formals UNIT in
      assert_types UNIT (trans_body_ty venv'') fdec.fundec_pos;
      (venv'', tenv)
  in

  (* fold over the list of fundecs *)
  let trans_function_decs fundecs : transenv =
    (* TODO: add function signatures to tenv before evaluating body *)
    let foldf (v, t) fdec =
      trans_function_dec v t fdec
    in
    let (venv', tenv') = List.fold_left foldf (venv, tenv) fundecs in
    { venv = venv'; tenv = tenv' }
  in

  match dec with
  | FunctionDec fundecs -> trans_function_decs fundecs
  | TypeDec tydecs -> trans_type_decs tydecs
  | VarDec { name; typ; init; pos; _ } -> trans_var_dec name typ init pos

(* translate a Absyn.ty into a Types.ty allowing NAME with NONE types *)
and trans_ty tenv ty =
  let tenv_lookup (tenv : tenv) symbol pos =
    match Symbol.look tenv symbol with
      | None -> type_not_found symbol pos
      | Some ty -> ty
  in
  match ty with
  | NameTy (symbol, pos) -> tenv_lookup tenv symbol pos
  | RecordTy fields ->
    let field_ty field =
      let ty =
        tenv_lookup tenv field.field_typ field.field_pos
      in
      (field.field_name, ty)
    in
    RECORD ((List.map field_ty fields), (mk_unique ()))
  | ArrayTy (symbol, pos) ->
    let ty = tenv_lookup tenv symbol pos in
    ARRAY (ty, (mk_unique ()))

and actual_ty tenv absyn_ty pos =
  let rec deep_find = function
    | NAME (name, refty) ->
      begin
        match !refty with
        | Some ty -> deep_find ty
        | None -> type_not_found name pos
      end
    | ty -> ty
  in
  match trans_ty tenv absyn_ty with
  | NAME _ as nty -> deep_find nty
  | ty -> ty
