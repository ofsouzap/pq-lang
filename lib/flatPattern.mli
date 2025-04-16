module StdPattern = Pattern.StdPattern
module StdExpr = Expr.StdExpr
module StdProgram = Program.StdProgram

type 'a flat_pattern =
  | FlatPatName of 'a * string * Vtype.t
  | FlatPatPair of 'a * ('a * string * Vtype.t) * ('a * string * Vtype.t)
  | FlatPatConstructor of 'a * string * ('a * string * Vtype.t)
[@@deriving sexp, equal]

module M : Pattern.S with type 'a t = 'a flat_pattern
module FlatExpr : Expr.S with module Pattern = M

module FlatProgram :
  Program.S
    with module Pattern = M
     and module Expr = FlatExpr
     and module QuotientType = QuotientType.StdQuotientType
     and module CustomType = CustomType.StdCustomType

val to_std_pattern : 'a flat_pattern -> 'a StdPattern.t
val to_std_expr : ('tag_e, 'tag_p) FlatExpr.t -> ('tag_e, 'tag_p) StdExpr.t
