open Core
open Custom_types
open Ast

type 'a program = { custom_types : custom_type list; e : 'a expr }
[@@deriving sexp, equal]

type plain_program = unit program [@@deriving sexp, equal]
