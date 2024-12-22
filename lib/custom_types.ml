open Core
open Vtype

type custom_type_constructor = string * vtype [@@deriving sexp, equal]

let custom_type_constructor_to_source_code
    ((c_name, c_type) : custom_type_constructor) : string =
  sprintf "%s of %s" c_name (vtype_to_source_code c_type)

let custom_type_constructors_to_source_code (cs : custom_type_constructor list)
    : string =
  List.map ~f:custom_type_constructor_to_source_code cs
  |> String.concat ~sep:" | "

type custom_type = string * custom_type_constructor list
[@@deriving sexp, equal]

let custom_type_to_source_code ((ct_name, ct_cs) : custom_type) : string =
  sprintf "type %s = %s" ct_name (custom_type_constructors_to_source_code ct_cs)
