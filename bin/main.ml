let loc (p : Lexing.position) = Printf.sprintf "L:%d C:%d" p.pos_lnum (p.pos_cnum - p.pos_bol)

let () =
  match Tiger.Parse.parse (Sys.argv.(1)) with
  | Error msg ->
    print_endline ("Error: " ^ msg);
    exit 1
  | Ok exp ->
    try
      let _ = Tiger.Find_escape.find_escape exp in
      let _ = Tiger.Semant.trans_prog exp in
      ()
    with
    | Tiger.Semant.Error (msg, maybe_pos) ->
      let pos_str =
        match maybe_pos with
        | None -> ""
        | Some p -> "\n" ^ loc p
      in
      print_endline ("Error: " ^ msg ^ pos_str);
      exit 1
    | Tiger.Find_escape.Error (msg, pos) ->
      print_endline ("Error: " ^ msg ^ "\n" ^ loc pos);
      exit 1
