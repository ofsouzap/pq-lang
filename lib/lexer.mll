{
open Parser
}

let whitespace = [' ' '\t' '\n']

rule token = parse
  | whitespace { token lexbuf }
  | ['0'-'9']+ as n { INT (int_of_string n) }
  | '+' { PLUS }
  | '*' { TIMES }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | eof { EOF }
  | _ { failwith "Unexpected character" }
