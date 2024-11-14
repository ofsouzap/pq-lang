(** The type of a value or expression *)
type vtype = VTypeInt | VTypeBool | VTypeFun of vtype * vtype
[@@deriving sexp, equal]
