open Core
open Utils

type flattening_error =
  | UnknownVariantConstructor of string
  | NoDefaultCaseForMatchBranch of string option
[@@deriving sexp, equal]

module type S = sig
  module TypeChecker :
    TypeChecker.S
      with module Pattern = Pattern.StdPattern
       and module Expr = Expr.StdExpr
       and module Program = Program.StdProgram

  val flatten_expr :
    existing_names:StringSet.t ->
    type_ctx:TypeChecker.TypeCtx.t ->
    ('tag, 'tag) Expr.StdExpr.typed_t ->
    ( StringSet.t * ('tag, 'tag) FlatPattern.FlatExpr.typed_t,
      flattening_error )
    Result.t

  val flatten_program :
    existing_names:StringSet.t ->
    type_ctx:TypeChecker.TypeCtx.t ->
    ('tag, 'tag) Program.StdProgram.typed_t ->
    ( StringSet.t * ('tag, 'tag) FlatPattern.FlatProgram.typed_t,
      flattening_error )
    Result.t

  val flatten_typed_program :
    existing_names:StringSet.t ->
    ('tag, 'tag) TypeChecker.typed_program ->
    ( StringSet.t * ('tag, 'tag) FlatPattern.FlatProgram.typed_t,
      flattening_error )
    Result.t
end

module Make
    (TypeChecker :
      TypeChecker.S
        with module Pattern = Pattern.StdPattern
         and module Expr = Expr.StdExpr
         and module Program = Program.StdProgram) :
  S with module TypeChecker = TypeChecker
