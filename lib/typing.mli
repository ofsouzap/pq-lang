open Vtype
open Ast

val type_expr_no_ctx : 'a expr -> ((vtype * 'a) expr, unit) Result.t
