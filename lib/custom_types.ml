open Core
open Vtype

type custom_type_constructor = string * vtype [@@deriving sexp, equal]

type custom_type = string * custom_type_constructor list
[@@deriving sexp, equal]
