open Core
open Custom_types
open Ast

type 'a program = custom_type list * 'a expr [@@deriving sexp, equal]
type plain_program = unit program [@@deriving sexp, equal]
