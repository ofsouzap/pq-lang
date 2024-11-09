open Ast

type vtype = VTypeInt | VTypeBool | VTypeFun of vtype * vtype
[@@deriving sexp, equal]

val type_expr : 'a expr -> ((vtype * 'a) expr, unit) Result.t
