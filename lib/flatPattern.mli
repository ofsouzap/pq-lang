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

type flattening_error =
  | UnknownVariantConstructor of string
  | NoDefaultCaseForMatchBranch of string option
[@@deriving sexp, equal]

val to_std_pattern : 'a flat_pattern -> 'a StdPattern.t

val of_expr :
  existing_names:Utils.StringSet.t ->
  ('tag_e, 'tag_p) StdExpr.t ->
  (Utils.StringSet.t * ('tag_e, 'tag_p) FlatExpr.t, flattening_error) result

val to_std_expr : ('tag_e, 'tag_p) FlatExpr.t -> ('tag_e, 'tag_p) StdExpr.t

val of_program :
  existing_names:Utils.StringSet.t ->
  ('tag_e, 'tag_p) StdProgram.t ->
  (Utils.StringSet.t * ('tag_e, 'tag_p) FlatProgram.t, flattening_error) result
