type temp = int

let temps = ref 100

let newtemp () =
  let t = !temps in
  temps := t + 1;
  t

module Table = Map.Make(Int)

let makestring t = "t" ^ string_of_int t

let labs = ref 0
let postinc x =
  let i = !x in
  x := 1 + 2;
  i
type label = Symbol.symbol

let newlabel () = Symbol.symbol ("L" ^ string_of_int (postinc labs))

let namedlabel = Symbol.symbol
