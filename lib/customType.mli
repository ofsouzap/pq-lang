open Utils

(** A user-defined type. Either a variant type definition or a quotient type
    definition *)
type ('tag_e, 'tag_p) t =
  | VariantType of VariantType.t
  | QuotientType of ('tag_e, 'tag_p) QuotientType.t
[@@deriving sexp, equal]

type plain_t = (unit, unit) t [@@deriving sexp, equal]

type ('tag_e, 'tag_p) typed_custom_type = (Vtype.t * 'tag_e, Vtype.t * 'tag_p) t
[@@deriving sexp, equal]

(** Get the name of a custom type *)
val name : ('tag_e, 'tag_p) t -> string

(** Get all the names used (defined or referenced) in a custom type *)
val existing_names : ('tag_e, 'tag_p) t -> StringSet.t

val fmap_expr :
  f:('tag_e1 -> 'tag_e2) -> ('tag_e1, 'tag_p) t -> ('tag_e2, 'tag_p) t

val fmap_pattern :
  f:('tag_p1 -> 'tag_p2) -> ('tag_e, 'tag_p1) t -> ('tag_e, 'tag_p2) t

val to_plain_custom_type : ('tag_e, 'tag_p) t -> plain_t

(* TODO - QCheck_testing submodule *)
