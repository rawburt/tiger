type symbol = string * int

exception Symbol

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

let eq (_, n1) (_, n2) = n1 = n2

module Tbl = Map.Make(
  struct
    type t = symbol
    let compare (_, n1) (_, n2) = compare n1 n2
  end)

type 'a table = 'a Tbl.t

let empty = Tbl.empty
let enter t s a = Tbl.add s a t
let look t s = Tbl.find_opt s t
