type run_frontend_res = Res of Ast.expr | LexingError of char | ParsingError

let lex_parse_from_lexbuf (lexbuf : Lexing.lexbuf) : run_frontend_res =
  try Res (Parser.prog Lexer.token lexbuf) with
  | Lexer.LexingError c -> LexingError c
  | Parser.Error | Parsing_errors.CustomError -> ParsingError

let run_frontend_channel (input : in_channel) : run_frontend_res =
  lex_parse_from_lexbuf (Lexing.from_channel input)

let run_frontend_string (input : string) : run_frontend_res =
  lex_parse_from_lexbuf (Lexing.from_string input)
