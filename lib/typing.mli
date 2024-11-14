open Ast
open Vtype

val type_expr : 'a expr -> ((vtype * 'a) expr, unit) Result.t
