(** A result from trying to run the lexer and parser on an input *)
type run_frontend_res =
  | Res of Ast.expr  (** A successful lex and parse, with the resulting AST *)
  | LexingError of char
      (** A lexing error occured on the specified character *)
  | ParsingError  (** A parsing error occured on the token stream *)

(** Run the lexer and parser on an input channel *)
val run_frontend_channel : in_channel -> run_frontend_res

(** Run the lexer and parser on a string input *)
val run_frontend_string : string -> run_frontend_res
