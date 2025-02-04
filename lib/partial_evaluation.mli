open Core
open Utils
open Vtype
open Varname
open Ast

type partial_evaluation_error [@@deriving sexp, equal]

module PartialEvaluator : functor
  (TagExpr : sig
     type t [@@deriving sexp, equal]
   end)
  (TagPat : sig
     type t [@@deriving sexp, equal]
   end)
  -> sig
  type closure = {
    param : varname * vtype;
    out_type : vtype;
    body : (TagExpr.t, TagPat.t) typed_expr;
    store : store;
    recursive : [ `Recursive of varname | `NonRecursive ];
  }
  [@@deriving sexp, equal]

  and store_val = ((TagExpr.t, TagPat.t) typed_expr, closure) Either.t
  [@@deriving sexp, equal]

  and store = store_val StringMap.t [@@deriving sexp, equal]

  type state = { store : store; e : (TagExpr.t, TagPat.t) typed_expr }
  [@@deriving sexp, equal]

  (** Evaluate an expression as much as possible, or until the maximum recursion
      depth is reached *)
  val eval :
    mrd:int ->
    state ->
    ((TagExpr.t, TagPat.t) typed_expr, partial_evaluation_error) Result.t
end
