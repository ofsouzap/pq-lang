open Core
module Program = Program.StdProgram

type frontend_error = LexingError of char | ParsingError
[@@deriving sexp, equal]

type source_position = { lnum : int; cnum : int } [@@deriving sexp, equal]

type run_frontend_res =
  ((source_position, source_position) Program.t, frontend_error) Result.t
[@@deriving sexp, equal]

let lex_parse_from_lexbuf (lexbuf : Lexing.lexbuf) : run_frontend_res =
  let open Result in
  (try Ok (Parser.prog Lexer.token lexbuf) with
  | Lexer.LexingError c -> Error (LexingError c)
  | Parser.Error | Parsing_errors.CustomError -> Error ParsingError)
  >>| fun prog ->
  prog
  |> Program.fmap_expr ~f:(fun lex_pos ->
         Lexing.{ lnum = lex_pos.pos_lnum; cnum = lex_pos.pos_cnum })
  |> Program.fmap_pattern ~f:(fun lex_pos ->
         Lexing.{ lnum = lex_pos.pos_lnum; cnum = lex_pos.pos_cnum })

let run_frontend_channel (input : In_channel.t) : run_frontend_res =
  let lexbuf = Lexing.from_channel ~with_positions:true input in
  lex_parse_from_lexbuf lexbuf

let run_frontend_string (input : string) : run_frontend_res =
  let lexbuf = Lexing.from_string ~with_positions:true input in
  lex_parse_from_lexbuf lexbuf
