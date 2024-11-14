(** The type of a value or expression *)
type vtype = VTypeInt | VTypeBool | VTypeFun of vtype * vtype
[@@deriving sexp, equal]

(** Convert a vtype to a string representation compatible with the source code *)
val vtype_to_source_code : vtype -> string
