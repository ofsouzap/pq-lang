open Core

type frontend_error = LexingError of char | ParsingError
type run_frontend_res = (Ast.plain_expr, frontend_error) Result.t

let lex_parse_from_lexbuf (lexbuf : Lexing.lexbuf) : run_frontend_res =
  try Ok (Parser.prog Lexer.token lexbuf |> snd) with
  | Lexer.LexingError c -> Error (LexingError c)
  | Parser.Error | Parsing_errors.CustomError -> Error ParsingError

let run_frontend_channel (input : In_channel.t) : run_frontend_res =
  lex_parse_from_lexbuf (Lexing.from_channel input)

let run_frontend_string (input : string) : run_frontend_res =
  lex_parse_from_lexbuf (Lexing.from_string input)
