{
open Parser

exception LexingError of char
}

let newline = '\n' | '\r' | "\r\n"
let whitespace = [' ' '\t'] | newline

rule token = parse
  (* Keywords *)
  | "end" { END }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | "let" { LET }
  | "rec" { REC }
  | "in" { IN }
  | "true" { TRUE }
  | "false" { FALSE }
  | "unit" { UNIT }
  | "int" { INT }
  | "bool" { BOOL }
  | "match" { MATCH }
  | "with" { WITH }
  | "type" { TYPE }
  | "qtype" { QTYPE }
  | "of" { OF }
  | "private" { PRIVATE }
  (* Operators and symbols *)
  | '+' { PLUS }
  | '-' { MINUS }
  | '*' { STAR }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | '~' { BNOT }
  | "||" { BOR }
  | "&&" { BAND }
  | '=' { ASSIGN }
  | "==" { EQUATE }
  | ">" { GT }
  | ">=" { GTEQ }
  | "<" { LT }
  | "<=" { LTEQ }
  | "->" { ARROW }
  | "=>" { BIG_ARROW }
  | ":" { COLON }
  | "," { COMMA }
  | "|" { PIPE }
  | "|/" { QUOTIENT }
  | "()" { UNIT_VAL }
  | "#" { single_line_comment lexbuf }
  | "(*" { multi_line_comment lexbuf }
  (* Literals and names *)
  | ['0'-'9']+ as n { INTLIT (int_of_string n) }
  | ['a'-'z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as name { LNAME name }
  | ['A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as name { UNAME name }
  (* Misc *)
  | eof { EOF }
  | whitespace { token lexbuf }
  | _ as c { raise (LexingError c) }
and single_line_comment = parse
  | newline { token lexbuf }
  | eof { EOF }
  | _ { single_line_comment lexbuf }
and multi_line_comment = parse
  | "*)" { token lexbuf }
  | eof { EOF }
  | _ { multi_line_comment lexbuf }
