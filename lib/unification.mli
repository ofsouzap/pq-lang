open Core
open Utils
open Varname
open Pattern
open Ast

type ('tag_e, 'tag_p) unifier = ('tag_e, 'tag_p) expr StringMap.t
[@@deriving sexp, equal]

(** Find a simple unifier from one expression to another. This doesn't try
    unifying branching code or let-bindings *)
val simply_find_unifier :
  bound_names:StringSet.t ->
  from_expr:('a, 'b) expr ->
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
