module A = Absyn
module S = Symbol

exception Error of string * A.pos

let error pos msg = raise (Error (msg, pos))

type depth = int
type esc_env = (depth * bool ref) S.table

let rec trans_var env depth = function
  | A.SimpleVar (symbol, pos) -> begin
    match S.look env symbol with
    | Some (d, escapes) ->
      if depth > d then escapes := true
    | None -> error pos ("symbol not found: " ^ S.name symbol)
  end
  | A.FieldVar (var, _, _) -> trans_var env depth var
  | A.SubscriptVar (var, _, _) -> trans_var env depth var

and trans_exp env depth = function
  | A.VarExp var -> trans_var env depth var
  | CallExp { args; _} ->
    List.iter (trans_exp env depth) args
  | OpExp { left; right; _ } ->
    trans_exp env depth left;
    trans_exp env depth right;
  | RecordExp { fields; _ } ->
    List.iter (fun (_, e, _) -> trans_exp env depth e) fields
  | SeqExp exps ->
    List.iter (fun (e, _ ) -> trans_exp env depth e) exps
  | AssignExp { exp; _ } -> trans_exp env depth exp
  | IfExp { test; then'; else'; _ } ->
    trans_exp env depth test;
    trans_exp env depth then';
    begin
      match else' with
      | Some e -> trans_exp env depth e
      | _ -> ()
    end
  | WhileExp{ test; body; _ } ->
    trans_exp env depth test;
    trans_exp env depth body
  | ForExp { var; escape; lo; hi; body; _ } ->
    trans_exp env depth lo;
    trans_exp env depth hi;
    let env' = S.enter env var (depth, escape) in
    trans_exp env' depth body
  | LetExp { decs; body; _ } ->
    let env' = trans_decs env depth decs in
    trans_exp env' depth body
  | ArrayExp { size; init; _ } ->
    trans_exp env depth size;
    trans_exp env depth init
  | _ -> ()

and trans_decs env depth decs : esc_env =
  let trans_dec env = function
    | A.FunctionDec fundecs ->
      let function_dec (fn:A.fundec) =
        let d = depth + 1 in
        let add_param e (f:A.field) =
          S.enter e f.field_name (d, f.field_escape)
        in
        let env' = List.fold_left add_param env fn.fundec_params in
        trans_exp env' d fn.fundec_body
      in
      List.iter function_dec fundecs;
      env
    | VarDec { name; escape; init; _ } ->
      trans_exp env depth init;
      S.enter env name (depth, escape)
    | TypeDec _ -> env
  in
  List.fold_left trans_dec env decs

let find_escape program =
  trans_exp S.empty 0 program
