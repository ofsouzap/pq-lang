{
open Parser

exception LexingError of char
}

let whitespace = [' ' '\t' '\n']

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
  | "fun" { FUN }
  | "unit" { UNIT }
  | "int" { INT }
  | "bool" { BOOL }
  | "match" { MATCH }
  | "with" { WITH }
  | "type" { TYPE }
  | "of" { OF }
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
  | ":" { COLON }
  | "," { COMMA }
  | "|" { PIPE }
  | "()" { UNIT_VAL }
  (* Literals and names *)
  | ['0'-'9']+ as n { INTLIT (int_of_string n) }
  | ['a'-'z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as name { LNAME name }
  | ['A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as name { UNAME name }
  (* Misc *)
  | eof { EOF }
  | whitespace { token lexbuf }
  | _ as c { raise (LexingError c) }
