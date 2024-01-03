type unique = unit ref

type ty =
  | INT
  | STRING
  | RECORD of (Symbol.symbol * ty) list * unique
  | ARRAY of ty * unique
  | NIL
  | UNIT
  | NAME of Symbol.symbol * ty option ref

let rec string_of_ty = function
  | INT -> "INT"
  | STRING -> "STRING"
  | RECORD (field_list, r) ->
    let string_of_field (symbol, ty) =
      Symbol.name symbol ^ ": " ^ string_of_ty ty
    in
    let fields =
      List.map string_of_field field_list
      |> String.concat ", "
    in
    let pnt = Printf.sprintf "%d" (Obj.magic r) in
    "RECORD: (" ^ fields ^ ")(ref#" ^ pnt ^ ")"
  | ARRAY (ty, r) ->
    let pnt = Printf.sprintf "%d" (Obj.magic r) in
    "ARRAY[" ^ string_of_ty ty ^ "](ref#" ^ pnt ^ ")"
  | NIL -> "NIL"
  | UNIT -> "UNIT"
  | NAME (symbol, refopt) ->
    match !refopt with
    | None -> "NAME " ^ Symbol.name symbol ^ ", ref None"
    | Some _ -> "NAME " ^ Symbol.name symbol ^ ", ref Some"
