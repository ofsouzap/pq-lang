(** Provides unification algorithm between patterns, and applying these unifiers
    to expressions and patterns *)

open Core
open Utils
open Vtype
open Varname
open Pattern

type 'tag_p unifier = 'tag_p pattern StringMap.t [@@deriving sexp, equal]

(** Create a unifier from one given pattern to the other given pattern, if one
    is possible *)
val find_unifier :
  from_pattern:'tag_p pattern ->
  to_pattern:'tag_p pattern ->
  ('tag_p unifier, unit) Result.t

(** Create a unifier from a given expression to a given pattern, if one is
    possible. Will only allow unifying where the expression looks like a pattern
    (ie. without literals, using mostly pairing, constructors, and variable
    names) *)
val find_expr_unifier :
  convert_tag:('tag_e -> 'tag_p) ->
  get_type:(('tag_e, 'tag_p) Ast.expr -> vtype) ->
  from_expr:('tag_e, 'tag_p) Ast.expr ->
  to_pattern:'tag_p pattern ->
  ('tag_p unifier, unit) Result.t

(** Rename a variable in the body of a unifier's substitutions. This doesn't
    affect the names that are substituted *)
val rename_var_in_body :
  old_name:varname -> new_name:varname -> 'tag_p unifier -> 'tag_p unifier

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
