open Vtype

(** A single constructor for a custom data type, with a name and mandatory attached data type *)
type custom_type_constructor = string * vtype [@@deriving sexp, equal]

(** Convert a custom type constuctor definition into source code.  *)
val custom_type_constructor_to_source_code : custom_type_constructor -> string

(** A custom data type with a name and list of constructors *)
type custom_type = string * custom_type_constructor list
[@@deriving sexp, equal]

(** Convert a custom type definition into source code.  *)
val custom_type_to_source_code : custom_type -> string
