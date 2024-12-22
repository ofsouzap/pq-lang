(** The type of a value or expression *)
type vtype =
  | VTypeUnit
  | VTypeInt
  | VTypeBool
  | VTypePair of vtype * vtype
  | VTypeFun of vtype * vtype
[@@deriving sexp, equal]

(* TODO - custom type definitions *)

(** Convert a vtype to a string representation compatible with the source code *)
val vtype_to_source_code : vtype -> string
