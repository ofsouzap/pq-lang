type run_frontend_res = Res of Ast.expr | LexingError of char | ParsingError

let run_frontend_string (input : string) : run_frontend_res =
  try
    let lexbuf = Lexing.from_string input in
    Res (Parser.prog Lexer.token lexbuf)
  with
  | Lexer.LexingError c -> LexingError c
  | Parser.Error -> ParsingError
