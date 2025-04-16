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

  val flatten_program :
    existing_names:StringSet.t ->
    (Vtype.t * 'tag_e, Vtype.t * 'tag_p) TypeChecker.typed_program ->
    ( StringSet.t * (unit, unit) FlatPattern.FlatProgram.typed_t,
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
