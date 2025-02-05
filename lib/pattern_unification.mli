(** Provides unification algorithm between patterns, and applying these unifiers
    to expressions and patterns *)

open Core
open Utils
open Pattern

type 'tag_p unifier = 'tag_p pattern StringMap.t [@@deriving sexp, equal]

(** Create a unifier from one given pattern to the other given pattern, if one
    is possible *)
val find_unifier :
  from_pattern:'tag_p pattern ->
  to_pattern:'tag_p pattern ->
  ('tag_p unifier, unit) Result.t

(** Apply a unifier to a pattern *)
val apply_to_pattern :
  unifier:'tag_p unifier -> 'tag_p pattern -> 'tag_p pattern

(** Convert a pattern to a corresponding expression *)
val pattern_to_expr :
  convert_tag:('tag_p -> 'tag_e) -> 'tag_p pattern -> ('tag_e, 'tag_p) Ast.expr

(** Apply a unifier to an expression *)
val apply_to_expr :
  convert_tag:('tag_p -> 'tag_e) ->
  unifier:'tag_p unifier ->
  ('tag_e, 'tag_p) Ast.expr ->
  ('tag_e, 'tag_p) Ast.expr
