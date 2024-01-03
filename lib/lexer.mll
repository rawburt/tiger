{
  open Parser

  exception Syntax_error of string * string * Lexing.position

  let error msg lexbuf =
    raise (Syntax_error (msg, Lexing.lexeme lexbuf, Lexing.lexeme_start_p lexbuf))
}

let int = ['0'-'9']+
let id = ['a'-'z' 'A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*
let whitespace = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"

rule tokenize = parse
  | "type"      { TYPE }
  | "var"       { VAR }
  | "function"  { FUNCTION }
  | "break"     { BREAK }
  | "of"        { OF }
  | "end"       { END }
  | "in"        { IN }
  | "nil"       { NIL }
  | "let"       { LET }
  | "do"        { DO }
  | "to"        { TO }
  | "for"       { FOR }
  | "while"     { WHILE }
  | "else"      { ELSE }
  | "then"      { THEN }
  | "if"        { IF }
  | "array"     { ARRAY }
  | ":="        { ASSIGN }
  | "|"         { OR }
  | "&"         { AND }
  | ">="        { GE }
  | ">"         { GT }
  | "<="        { LE }
  | "<"         { LT }
  | "<>"        { NEQ }
  | "="         { EQ }
  | "/"         { DIVIDE }
  | "*"         { TIMES }
  | "-"         { MINUS }
  | "+"         { PLUS }
  | "."         { DOT }
  | "}"         { RBRACE }
  | "{"         { LBRACE }
  | "]"         { RBRACK }
  | "["         { LBRACK }
  | ")"         { RPAREN }
  | "("         { LPAREN }
  | ";"         { SEMICOLON }
  | ":"         { COLON }
  | ","         { COMMA }
  | int as i    { INT (int_of_string i) }
  | id as i     { ID i }
  | '"'         { read_string (Buffer.create 17) lexbuf }
  | "/*"        { read_comment 0 lexbuf }
  | whitespace  { tokenize lexbuf }
  | newline     { Lexing.new_line lexbuf; tokenize lexbuf }
  | eof         { EOF }
  | _           { error "unexpected token" lexbuf }

and read_string buf = parse
  | '"'         { STRING (Buffer.contents buf) }
  | '\\' '/'    { Buffer.add_char buf '/'; read_string buf lexbuf }
  | '\\' '\\'   { Buffer.add_char buf '\\'; read_string buf lexbuf }
  | '\\' 'b'    { Buffer.add_char buf '\b'; read_string buf lexbuf }
  | '\\' 'f'    { Buffer.add_char buf '\012'; read_string buf lexbuf }
  | '\\' 'n'    { Buffer.add_char buf '\n'; read_string buf lexbuf }
  | '\\' 'r'    { Buffer.add_char buf '\r'; read_string buf lexbuf }
  | '\\' 't'    { Buffer.add_char buf '\t'; read_string buf lexbuf }
  | [^ '"' '\\']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_string buf lexbuf
    }
  | eof         { error "string not terminated" lexbuf }
  | _           { error "unexpected string item" lexbuf }

and read_comment level = parse
  | "*/"        { if level = 0 then (tokenize lexbuf) else (read_comment (level - 1) lexbuf) }
  | "/*"        { read_comment (level + 1) lexbuf }
  | "\n"        { Lexing.new_line lexbuf; read_comment level lexbuf }
  | eof         { error "file ended without closing comment" lexbuf }
  | _           { read_comment level lexbuf }
