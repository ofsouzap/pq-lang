open Utils
open Vtype
open Variant_types

(** A user-defined type. Either a variant type definition or a quotient type
    definition *)
type ('tag_e, 'tag_p) custom_type =
  | VariantType of variant_type
  | QuotientType of ('tag_e, 'tag_p) QuotientType.t
[@@deriving sexp, equal]

type plain_custom_type = (unit, unit) custom_type [@@deriving sexp, equal]

type ('tag_e, 'tag_p) typed_custom_type =
  (vtype * 'tag_e, vtype * 'tag_p) custom_type
[@@deriving sexp, equal]

(** Get the name of a custom type *)
val custom_type_name : ('tag_e, 'tag_p) custom_type -> string

(** Get all the names used (defined or referenced) in a custom type *)
val existing_names : ('tag_e, 'tag_p) custom_type -> StringSet.t

val fmap_expr :
  f:('tag_e1 -> 'tag_e2) ->
  ('tag_e1, 'tag_p) custom_type ->
  ('tag_e2, 'tag_p) custom_type

val fmap_pattern :
  f:('tag_p1 -> 'tag_p2) ->
  ('tag_e, 'tag_p1) custom_type ->
  ('tag_e, 'tag_p2) custom_type

val to_plain_custom_type : ('tag_e, 'tag_p) custom_type -> plain_custom_type

(* TODO - QCheck_testing submodule *)
