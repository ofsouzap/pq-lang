open Custom_types
open Ast

(** A program, consisting of any number of custom type definitions and an expression to evaluate *)
type 'a program = { custom_types : custom_type list; e : 'a expr }
[@@deriving sexp, equal]

(** A program with no tagging *)
type plain_program = unit program [@@deriving sexp, equal]

(** Convert a program into source code.  *)
val program_to_source_code : 'a program -> string
