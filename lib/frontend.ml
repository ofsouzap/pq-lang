open Core
module Program = Program.StdProgram

type frontend_error = LexingError of char | ParsingError
[@@deriving sexp, equal]

type source_position = { fname : string option; lnum : int; cnum : int }
[@@deriving sexp, equal]

type run_frontend_res =
  ((source_position, source_position) Program.t, frontend_error) Result.t
[@@deriving sexp, equal]

let string_to_non_empty_string_option (x : string) : string option =
  if String.is_empty x then None else Some x

let lex_parse_from_lexbuf (lexbuf : Lexing.lexbuf) : run_frontend_res =
  let open Result in
  (try Ok (Parser.prog Lexer.token lexbuf) with
  | Lexer.LexingError c -> Error (LexingError c)
  | Parser.Error | Parsing_errors.CustomError -> Error ParsingError)
  >>| fun prog ->
  prog
  |> Program.fmap_expr ~f:(fun lex_pos ->
         Lexing.
           {
             fname = lex_pos.pos_fname |> string_to_non_empty_string_option;
             lnum = lex_pos.pos_lnum;
             cnum = lex_pos.pos_cnum;
           })
  |> Program.fmap_pattern ~f:(fun lex_pos ->
         Lexing.
           {
             fname = lex_pos.pos_fname |> string_to_non_empty_string_option;
             lnum = lex_pos.pos_lnum;
             cnum = lex_pos.pos_cnum;
           })

let add_filename_to_lexbuf (filename : string option) (lexbuf : Lexing.lexbuf) :
    unit =
  match filename with
  | None -> ()
  | Some filename -> Lexing.set_filename lexbuf filename

let run_frontend_channel ?(filename : string option) (input : In_channel.t) :
    run_frontend_res =
  let lexbuf = Lexing.from_channel ~with_positions:true input in
  add_filename_to_lexbuf filename lexbuf;
  lex_parse_from_lexbuf lexbuf

let run_frontend_string ?(filename : string option) (input : string) :
    run_frontend_res =
  let lexbuf = Lexing.from_string ~with_positions:true input in
  add_filename_to_lexbuf filename lexbuf;
  lex_parse_from_lexbuf lexbuf
