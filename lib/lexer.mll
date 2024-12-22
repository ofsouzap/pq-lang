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
  | "==" { EQ }
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
  | ['a'-'z' 'A'-'Z' '_']['a'-'z' 'A'-'Z' '0'-'9' '_']* as name { NAME name }
  (* Misc *)
  | eof { EOF }
  | whitespace { token lexbuf }
  | _ as c { raise (LexingError c) }
