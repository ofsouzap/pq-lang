open Vtype
open Ast

val type_expr : 'a expr -> ((vtype * 'a) expr, unit) Result.t
