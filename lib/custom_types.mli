open Vtype

(** A single constructor for a custom data type, with a name and mandatory attached data type *)
type custom_type_constructor = string * vtype [@@deriving sexp, equal]

(** A custom data type with a name and list of constructors *)
type custom_type = string * custom_type_constructor list
[@@deriving sexp, equal]
