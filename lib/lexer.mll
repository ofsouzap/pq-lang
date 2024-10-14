{
open Parser
}

let whitespace = [' ' '\t' '\n']

rule token = parse
  (* Keywords *)
  | "end" { END }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | "let" { LET }
  | "in" { IN }
  | "true" { TRUE }
  | "false" { FALSE }
  | "int" { INT }
  | "bool" { BOOL }
  | "fun" { FUN }
  (* Operators and symbols *)
  | ':' { COLON }
  | '+' { PLUS }
  | '-' { MINUS }
  | '*' { TIMES }
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
  (* Literals and names *)
  | ['0'-'9']+ as n { INTLIT (int_of_string n) }
  | ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9']* as name { NAME name }
  (* Misc *)
  | eof { EOF }
  | whitespace { token lexbuf }
  | _ { failwith "Unexpected character" } (* TODO - use a proper exception *)
