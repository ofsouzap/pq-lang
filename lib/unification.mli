open Core
open Utils
open Varname
open Pattern
open Ast

type ('tag_e, 'tag_p) unifier = ('tag_e, 'tag_p) expr StringMap.t
[@@deriving sexp, equal]

(** Create a unifier from the given pattern to the given expression, if one is
    possible *)
val find_unifier :
  from_pattern:'a pattern ->
  to_expr:('tag_e, 'tag_p) expr ->
  (('tag_e, 'tag_p) unifier, unit) Result.t

(** Create an expression from the given pattern. Useful for finding unifiers
    between patterns by first converting the target pattern to an expression *)
val pattern_to_expr :
  convert_tag:('tag_p -> 'tag_e) -> 'tag_p pattern -> ('tag_e, 'tag_p) expr

(** Rename a variable in the body of a unifier's substitutions. This doesn't
    affect the names that are substituted *)
val rename_var_in_body :
  old_name:varname ->
  new_name:varname ->
  ('tag_e, 'tag_p) unifier ->
  ('tag_e, 'tag_p) unifier

(** Apply a unifier to an expression *)
val apply_to_expr :
  unifier:('tag_e, 'tag_p) unifier ->
  ('tag_e, 'tag_p) Ast.expr ->
  ('tag_e, 'tag_p) Ast.expr
