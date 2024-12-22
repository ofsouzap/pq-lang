open Core

type frontend_error =
  | LexingError of char
      (** A lexing error occured on the specified character *)
  | ParsingError  (** A parsing error occured on the token stream *)

(** A result from trying to run the lexer and parser on an input *)
type run_frontend_res = (Ast.plain_expr, frontend_error) Result.t

(* TODO - custom type definitions *)

(** Run the lexer and parser on an input channel *)
val run_frontend_channel : In_channel.t -> run_frontend_res

(** Run the lexer and parser on a string input *)
val run_frontend_string : string -> run_frontend_res
