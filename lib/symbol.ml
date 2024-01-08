type symbol = string * int

let nextsym = ref 0

let hashtable : (string, int) Hashtbl.t = Hashtbl.create 17

let symbol (name : string) =
  match Hashtbl.find_opt hashtable name with
  | Some i -> (name, i)
  | None ->
    let i = !nextsym in
    nextsym := i + 1;
    Hashtbl.add hashtable name i;
    (name, i)

let name (s, _) = s

module Tbl = Map.Make(
  struct
    type t = symbol
    let compare (_, n1) (_, n2) = compare n1 n2
  end)

type 'a table = 'a Tbl.t

let empty = Tbl.empty
let enter t s a = Tbl.add s a t
let look t s = Tbl.find_opt s t

let string_of_table t string_of_entry =
  let show_entry ((n, entry):(symbol * 'a)) =
    Printf.sprintf "[%s] %s" (name n) (string_of_entry entry)
  in
  Tbl.to_seq t
  |> List.of_seq
  |> List.map show_entry
  |> String.concat "\n"
