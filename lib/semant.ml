module A = Absyn
module E = Env
module S = Symbol
module T = Types

let sprintf = Printf.sprintf

exception Error of string * A.pos option

let error ?pos msg = raise (Error (msg, pos))

type env = {
  tenv: E.tenv;
  venv: E.venv;
  level: Translate.level;
  looping: bool
}
type expty = { exp: Translate.exp; ty: Types.ty }

let mk_expty ty = { exp = (); ty = ty }

let get_var venv symbol pos =
  match S.look venv symbol with
  | Some var -> var
  | None -> error ~pos (sprintf "undefined variable %s" (S.name symbol))

let rec actual_ty ty pos =
  match ty with
  (* follow through NAME's until true type is found *)
  | T.NAME (symbol, ty_ref) -> begin
    match !ty_ref with
    | Some resolved_ty -> actual_ty resolved_ty pos
    | None -> error ~pos (sprintf "unresolved type: %s" (S.name symbol))
  end
  | _ -> ty

let get_ty tenv symbol pos =
  match S.look tenv symbol with
  | Some ty -> ty
  | None -> error ~pos (sprintf "type not found: %s" (S.name symbol))

let get_actual_ty tenv symbol pos =
  actual_ty (get_ty tenv symbol pos) pos

let check_int ty pos = actual_ty ty pos = INT

let check_ty expected got =
  (* deep eq `==` to compare `ref ()` fields for equality `(ref () == ref ()) = false` but `(ref () = ref ()) = true` *)
  match expected with
  (* records can be NIL *)
  | T.RECORD _ -> expected == got || got = NIL
  | _ -> expected == got

let is_rec_array_oper = function
  | A.EqOp | NeqOp -> true
  | _ -> false

let is_string_oper = function
  | A.EqOp | NeqOp | LeOp | LtOp
  | GeOp | GtOp -> true
  | _ -> false

let is_int_only_oper = function
  | A.PlusOp | MinusOp | TimesOp | DivideOp -> true
  | _ -> false

let rec trans_exp env exp : expty =
  let rec trexp = function
    | A.VarExp var -> trans_var env var
    | NilExp -> mk_expty NIL
    | IntExp _ -> mk_expty INT
    | StringExp _ -> mk_expty STRING
    | BreakExp pos ->
      if not env.looping
        then error ~pos "break used outside of a loop";
      mk_expty UNIT
    | CallExp { func; args; pos } ->
      let formals, result =
        match get_var env.venv func pos with
        | FunEntry {formals; result; _} -> (formals, result)
        | _ -> error ~pos (sprintf "%s is not a function" (S.name func))
      in
      let rec compare_args = function
        | [], [] -> ()
        | (formal :: formals), (arg :: args) ->
          let {ty=arg_ty; _} = trexp arg in
          let actual_arg_ty = actual_ty arg_ty pos in
          if not (check_ty formal actual_arg_ty)
            then error ~pos (sprintf "function %s expected argument type %s but got %s"
                              (S.name func) (T.string_of_ty formal) (T.string_of_ty actual_arg_ty));
          compare_args (formals, args)
        | _, _ -> error ~pos (sprintf "wrong number of arguments to function %s" (S.name func))
      in
      compare_args (formals, args);
      mk_expty (actual_ty result pos)
    | OpExp { left; oper; right; pos } ->
      let {ty=left_ty; _} = trexp left in
      let {ty=right_ty; _} = trexp right in
      if is_int_only_oper oper then begin
        match (check_int left_ty pos, check_int right_ty pos) with
        | true, true -> ()
        | true, false -> error ~pos (sprintf "right operand to %s must be INT" (A.string_of_oper oper))
        | false, true -> error ~pos (sprintf "left operand to %s must be INT" (A.string_of_oper oper))
        | false, false -> error ~pos (sprintf "both operand to %s must be INT" (A.string_of_oper oper))
      end else if (not (check_ty left_ty right_ty)) && (not (check_ty right_ty left_ty)) then
        error ~pos (sprintf "operands to %s must be same type" (A.string_of_oper oper))
      else begin
        match left_ty with
        | INT -> ()
        | STRING ->
          if not (is_string_oper oper)
            then error ~pos (sprintf "operator %s can't be used on STRING" (A.string_of_oper oper))
        | UNIT | NAME _ -> error ~pos "can't compare UNIT or NAME"
        | _ ->
          if not (is_rec_array_oper oper)
            then error ~pos (sprintf "operator %s can't be used on RECORD or ARRAY" (A.string_of_oper oper))
      end;
      mk_expty INT
    | RecordExp { fields; typ; pos } ->
      let ty, formals =
        match get_actual_ty env.tenv typ pos with
        | RECORD (formals, _) as ty -> (ty, formals)
        | t -> error ~pos (sprintf "expected %s to be RECORD but was %s" (S.name typ) (T.string_of_ty t))
      in
      (* TODO: this is sus... *)
      let rec check_fields formals = function
        | [] -> if not (List.is_empty formals)
                  then error ~pos (sprintf "not all fields initialized in RECORD %s" (S.name typ))
        | (symbol, exp, pos) :: fields ->
          match List.assoc_opt symbol formals with
          | None -> error ~pos (sprintf "field %s does not exist in RECORD %s" (S.name symbol) (S.name typ))
          | Some formal_ty ->
            let actual_formal_ty = actual_ty formal_ty pos in
            let {ty=exp_ty; _} = trexp exp in
            if not (check_ty (actual_ty actual_formal_ty pos) exp_ty)
              then error ~pos (sprintf "expected %s to be type %s but was %s"
                                (S.name symbol) (T.string_of_ty actual_formal_ty) (T.string_of_ty exp_ty));
            check_fields (List.remove_assoc symbol formals) fields
      in
      check_fields formals fields;
      mk_expty ty
    | SeqExp exps ->
      let rec seq = function
        | [] -> mk_expty UNIT
        | [(e, _)] -> trexp e
        | (e, _) :: es ->
          let _ = trexp e in
          seq es
        in
        seq exps
    | AssignExp { var; exp; pos } ->
      let {ty=var_ty; _} = trans_var env var in
      let {ty=exp_ty; _} = trexp exp in
      if not (check_ty var_ty exp_ty)
        then error ~pos (sprintf "can't assign %s to var type %s" (T.string_of_ty exp_ty) (T.string_of_ty var_ty));
      mk_expty UNIT
    | IfExp { test; then'; else'; pos } ->
      let {ty=test_ty; _} = trexp test in
      let {ty=then_ty; _} = trexp then' in
      if not (check_int test_ty pos)
        then error ~pos (sprintf "if statement test should be INT but was %s" (T.string_of_ty test_ty));
      begin
        match else' with
        | Some else_exp ->
          let {ty=else_ty; _} = trexp else_exp in
          if not (check_ty then_ty else_ty)
            then error ~pos "if statement branches do not have the same type";
        | None -> if then_ty <> UNIT then error ~pos "if statement with no else-branch must be type UNIT"
      end;
      mk_expty then_ty
    | WhileExp { test; body; pos } ->
      let {ty=test_ty; _} = trexp test in
      if not (check_int test_ty pos)
        then error ~pos "while loop test must be INT";
      (* update env to track looping *)
      let {ty=body_ty; _} = trans_exp { env with looping = true } body in
      if body_ty <> UNIT
        then error ~pos (sprintf "while loop body must be UNIT but was %s" (T.string_of_ty body_ty));
      mk_expty UNIT
    | ForExp { var; lo; hi; body; pos; _ } ->
      let {ty=lo_ty; _} = trexp lo in
      let {ty=hi_ty; _} = trexp hi in
      (* update env to bind var to INT *)
      (* TODO: check escapes *)
      let varentry =
        E.VarEntry {ty=INT; access=Translate.all_local env.level true}
      in
      let venv' = S.enter env.venv var varentry in
      (* update env to track looping *)
      let env' = { env with venv = venv' } in
      let {ty=body_ty; _} = trans_exp { env' with looping = true } body in
      if not (check_int lo_ty pos)
        then error ~pos "for loop lo range must be INT";
      if not (check_int hi_ty pos)
        then error ~pos "for loop hi range must be INT";
      if body_ty <> UNIT
        then error ~pos (sprintf "for loop body must be UNIT but was %s" (T.string_of_ty body_ty));
      mk_expty UNIT
    | LetExp { decs; body; _ } ->
      let env' = List.fold_left trans_dec env decs in
      trans_exp env' body
    | ArrayExp { typ; size; init; pos } ->
      let ty, array_ty =
        match get_actual_ty env.tenv typ pos with
        | ARRAY (array_ty, _) as ty -> (ty, actual_ty array_ty pos)
        | ty -> error ~pos (sprintf "expected %s to be an ARRAY but was %s" (S.name typ) (T.string_of_ty ty))
      in
      let {ty=size_ty; _} = trexp size in
      let {ty=init_ty; _} = trexp init in
      if not (check_int size_ty pos)
        then error ~pos
              (sprintf "expected ARRAY initialization to be an INT but was %s" (T.string_of_ty size_ty));
      if not (check_ty array_ty init_ty)
        then error ~pos
              (sprintf "ARRAY initialization mismatch. expected: %s, given: %s"
                (T.string_of_ty array_ty)
                (T.string_of_ty init_ty));
      mk_expty ty
  in
  trexp exp

and trans_var env = function
  | SimpleVar (symbol, pos) ->
    let ty =
      match get_var env.venv symbol pos with
      | VarEntry {ty; _} -> ty
      | _ -> error ~pos (sprintf "function %s is not a variable" (S.name symbol))
    in
    mk_expty ty
  | FieldVar (var, symbol, pos) ->
    let {ty=var_ty; _} = trans_var env var in
    let fields =
      match var_ty with
      | RECORD (fields, _) -> fields
      | t -> error ~pos (sprintf "expected %s to be RECORD but was %s" (S.name symbol) (T.string_of_ty t))
    in
    begin
      match List.assoc_opt symbol fields with
      | None -> error ~pos (sprintf "field %s not found" (S.name symbol))
      | Some field_ty -> mk_expty (actual_ty field_ty pos)
    end
  | SubscriptVar (var, exp, pos) ->
    let {ty=array_ty; _} = trans_var env var in
    let result_ty =
      match array_ty with
      | ARRAY (ty, _) -> actual_ty ty pos
      | t -> error ~pos (sprintf "subscripts are for ARRAY but found %s" (T.string_of_ty t))
    in
    let {ty=exp_ty; _} = trans_exp env exp in
    if not (check_int exp_ty pos)
      then error ~pos (sprintf "subscript of ARRAY expects INT but got %s" (T.string_of_ty exp_ty));
    mk_expty result_ty

and trans_dec env = function
  | VarDec { name; typ; init; pos; _ } ->
    let {ty = init_ty; _} = trans_exp env init in
    begin
      match typ with
      | Some (symbol, pos) ->
        let defined_ty = get_actual_ty env.tenv symbol pos in
        if not (check_ty defined_ty init_ty)
          then error ~pos (sprintf "expected %s initialization type to be %s but was %s"
                            (S.name name)
                            (T.string_of_ty defined_ty)
                            (T.string_of_ty init_ty))
      | None ->
        if init_ty = NIL then
          error ~pos "can't infer a variable to be type NIL"
    end;
    (* TODO: check esca[es] *)
    let varentry =
      E.VarEntry {ty = init_ty; access = Translate.all_local env.level true}
    in
    let venv' = S.enter env.venv name varentry in
    { env with venv = venv' }
  | TypeDec tydecs ->
    (* load type headers *)
    let load_type_header env (t:A.tydec) =
      let tenv' =
        S.enter env.tenv t.tydec_name (NAME (t.tydec_name, ref None))
      in
      { env with tenv = tenv'}
    in
    let env' = List.fold_left load_type_header env tydecs in
    (* resolve type headers *)
    let resolve_type (t:A.tydec) =
      let resolve = trans_ty env' t.tydec_ty in
      let reference =
        match get_ty env'.tenv t.tydec_name t.tydec_pos with
        | NAME (_, reference) -> reference
        | ty -> error ~pos:t.tydec_pos
                  (sprintf "expected NAME for %s but got %s" (S.name t.tydec_name) (T.string_of_ty ty))
      in
      reference := Some resolve
    in
    List.iter resolve_type tydecs;
    (* check for cycles *)
    let check_cycles (t:A.tydec) =
      let ty = get_ty env'.tenv t.tydec_name t.tydec_pos in
      let rec detect seen ty =
        match ty with
        | T.NAME (symbol, tyref) -> begin
          match List.find_opt ((=) symbol) seen with
          | Some _ -> error ~pos:t.tydec_pos "cyclic type definition detected"
          | None ->
            match !tyref with
            | Some t -> detect (symbol :: seen) t
            | None -> error ~pos:t.tydec_pos (sprintf "unresolved type %s" (S.name symbol))
        end
        |_ -> ()
      in
      detect [] ty
    in
    List.iter check_cycles tydecs;
    env'
  | FunctionDec fundecs ->
    (* load function headers *)
    let get_field_ty (f:A.field) =
      get_actual_ty env.tenv f.field_typ f.field_pos
    in
    let load_function_header env (fn:A.fundec) =
      let formals = List.map get_field_ty fn.fundec_params in
      let result =
        match fn.fundec_result with
        | Some (symbol, pos) -> get_actual_ty env.tenv symbol pos
        | None -> UNIT
      in
      let name = Temp.newlabel () in
      let funentry = E.FunEntry
        {
          formals;
          result;
          label = name;
          level = Translate.new_level {
            parent = env.level;
            name;
            (* TODO: escapes *)
            formals = List.map (fun _ -> true) fn.fundec_params
          }
        }
      in
      let venv' = S.enter env.venv fn.fundec_name funentry in
      { env with venv = venv' }
    in
    let env' = List.fold_left load_function_header env fundecs in
    (* typecheck functions *)
    let typecheck_function (fn:A.fundec) =
      let result_ty, level =
        match get_var env'.venv fn.fundec_name fn.fundec_pos with
        | FunEntry {result; level; _} -> result, level
        | _ -> error ~pos:fn.fundec_pos
                (sprintf "function %s not loaded into environment" (S.name fn.fundec_name))
      in
      (* load params into env for type checking function body *)
      let insert_param venv (f:A.field) =
        let param_ty =
          get_actual_ty env'.tenv f.field_typ f.field_pos
        in
        (* TODO: find escapes *)
        let varentry =
          E.VarEntry {ty = param_ty; access = Translate.all_local env.level true}
        in
        S.enter venv f.field_name varentry
      in
      let venv' =
        List.fold_left insert_param env'.venv fn.fundec_params
      in
      let {ty=body_ty; _} = trans_exp { env' with venv = venv'; level } fn.fundec_body in
      if not (check_ty result_ty body_ty)
        then error ~pos:fn.fundec_pos
              (sprintf "function %s expected type %s but was %s"
                (S.name fn.fundec_name) (T.string_of_ty result_ty) (T.string_of_ty body_ty));
    in
    List.iter typecheck_function fundecs;
    env'

and trans_ty env = function
  (* do not look through NAME's *)
  | NameTy (symbol, pos) -> get_ty env.tenv symbol pos
  | RecordTy (fields) ->
    let find_field_ty accm (f:A.field) =
      let ty = get_ty env.tenv f.field_typ f.field_pos in
      (f.field_name, ty) :: accm
    in
    let field_list = List.fold_left find_field_ty [] fields in
    RECORD (field_list, ref ())
  | ArrayTy (symbol, pos) ->
    let ty = get_ty env.tenv symbol pos in
    ARRAY (ty, ref ())

(* main entry point *)
let trans_prog exp =
  let env =
    {
      tenv = E.base_tenv;
      venv = E.base_venv;
      looping = false;
      level = Translate.outermost
    }
  in
  let _ = trans_exp env exp in
  ()
