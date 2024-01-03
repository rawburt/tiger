let print_error_position (lexbuf : Lexing.lexbuf) =
  let startp = (Lexing.lexeme_start_p lexbuf) in
  let endp = (Lexing.lexeme_end_p lexbuf) in
  let loc (p : Lexing.position) =
    Printf.sprintf "L:%d C:%d" p.pos_lnum (p.pos_cnum - p.pos_bol)
  in
  Printf.sprintf "Start:(%s) End:(%s) Token:'%s'"
    (loc startp)
    (loc endp)
    (Lexing.lexeme lexbuf)

let parse file =
  let lexbuf = Lexing.from_channel (open_in file) ~with_positions:true in
  Lexing.set_filename lexbuf file;
  try
    Ok (Parser.program Lexer.tokenize lexbuf)
  with
  | Parser.Error ->
    let error_msg = Printf.sprintf "parse error\n%s" (print_error_position lexbuf) in
    Error (error_msg)
