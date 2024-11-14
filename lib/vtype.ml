type vtype = VTypeInt | VTypeBool | VTypeFun of vtype * vtype
[@@deriving sexp, equal]
