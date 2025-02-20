open Core
open Utils

type ('tag_e, 'tag_p) unifier = ('tag_e, 'tag_p) Expr.t StringMap.t
[@@deriving sexp, equal]

(** Find a simple unifier from one expression to another. This doesn't try
    unifying branching code or let-bindings *)
val simply_find_unifier :
  bound_names_in_from:StringSet.t ->
  from_expr:('a, 'b) Expr.t ->
  to_expr:('tag_e, 'tag_p) Expr.t ->
  (('tag_e, 'tag_p) unifier, unit) Result.t

(** Rename a variable in the body of a unifier's substitutions. This doesn't
    affect the names that are substituted *)
val rename_var_in_body :
  old_name:Varname.t ->
  new_name:Varname.t ->
  ('tag_e, 'tag_p) unifier ->
  ('tag_e, 'tag_p) unifier

(** Apply a unifier to an expression *)
val apply_to_expr :
  unifier:('tag_e, 'tag_p) unifier ->
  ('tag_e, 'tag_p) Expr.t ->
  ('tag_e, 'tag_p) Expr.t
