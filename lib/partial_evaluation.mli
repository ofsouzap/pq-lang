open Core
open Utils
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
    param : varname;
    body : (TagExpr.t, TagPat.t) expr;
    store : store;
    recursive : [ `Recursive of varname | `NonRecursive ];
  }
  [@@deriving sexp, equal]

  and store_val = ((TagExpr.t, TagPat.t) expr, closure) Either.t
  [@@deriving sexp, equal]

  and store = store_val StringMap.t [@@deriving sexp, equal]

  type state = { store : store; e : (TagExpr.t, TagPat.t) expr }
  [@@deriving sexp, equal]

  (** Evaluate an expression as much as possible, or until the maximum recursion
      depth is reached *)
  val eval :
    mrd:int ->
    state ->
    ((TagExpr.t, TagPat.t) expr, partial_evaluation_error) Result.t
end
