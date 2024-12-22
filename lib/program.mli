open Custom_types
open Ast

(** A program, consisting of any number of custom type definitions and an expression to evaluate *)
type 'a program = custom_type list * 'a expr [@@deriving sexp, equal]

(** A program with no tagging *)
type plain_program = unit program [@@deriving sexp, equal]
