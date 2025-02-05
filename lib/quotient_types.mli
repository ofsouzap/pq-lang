open Utils
open Varname
open Vtype
open Pattern
open Ast

(** A single equality constructor on a quotient type *)
type ('tag_e, 'tag_p) quotient_type_eqcons = {
  bindings : (varname * vtype) list;
      (** The variable bindings for the equality constructor of the equality
          constructor *)
  body : 'tag_p pattern * ('tag_e, 'tag_p) expr;
      (** The equality definition of the equality constructor *)
      (* TODO - instead of having "body" which is the pattern and the expression,
      have something like body_pattern and body_expr, to make it nicer to use *)
}
[@@deriving sexp, equal]

type plain_quotient_type_eqcons = (unit, unit) quotient_type_eqcons
[@@deriving sexp, equal]

type ('tag_e, 'tag_p) typed_quotient_type_eqcons =
  (vtype * 'tag_e, vtype * 'tag_p) quotient_type_eqcons
[@@deriving sexp, equal]

(** Get all names used in a quotient type equality constructor *)
val eqcons_existing_names : ('tag_e, 'tag_p) quotient_type_eqcons -> StringSet.t

val eqcons_fmap_expr :
  f:('tag_e1 -> 'tag_e2) ->
  ('tag_e1, 'tag_p) quotient_type_eqcons ->
  ('tag_e2, 'tag_p) quotient_type_eqcons

val eqcons_fmap_pattern :
  f:('tag_p1 -> 'tag_p2) ->
  ('tag_e, 'tag_p1) quotient_type_eqcons ->
  ('tag_e, 'tag_p2) quotient_type_eqcons

val eqcons_to_plain_eqcons :
  ('tag_e, 'tag_p) quotient_type_eqcons -> plain_quotient_type_eqcons

(** Convert a quotient type equality constructor definition into source code. *)
val quotient_type_eqcons_to_source_code :
  ?use_newlines:bool -> ('tag_e, 'tag_p) quotient_type_eqcons -> string

(* TODO - QCheck_testing submodule for quotient_type_eqcons *)

(** A quotient type: a variant type with a list of quotients *)
type ('tag_e, 'tag_p) quotient_type = {
  name : string;  (** The name of the quotient type *)
  base_type_name : string;
      (** The name of the variant/quotient type that the quotient type is based
          on *)
  eqconss : ('tag_e, 'tag_p) quotient_type_eqcons list;
      (** The list of equality constructors for the quotient type *)
}
[@@deriving sexp, equal]

type plain_quotient_type = (unit, unit) quotient_type [@@deriving sexp, equal]

type ('tag_e, 'tag_p) typed_quotient_type =
  (vtype * 'tag_e, vtype * 'tag_p) quotient_type
[@@deriving sexp, equal]

(** Get all names used in a quotient type definition *)
val existing_names : ('tag_e, 'tag_p) quotient_type -> StringSet.t

val fmap_expr :
  f:('tag_e1 -> 'tag_e2) ->
  ('tag_e1, 'tag_p) quotient_type ->
  ('tag_e2, 'tag_p) quotient_type

val fmap_pattern :
  f:('tag_p1 -> 'tag_p2) ->
  ('tag_e, 'tag_p1) quotient_type ->
  ('tag_e, 'tag_p2) quotient_type

val to_plain_quotient_type :
  ('tag_e, 'tag_p) quotient_type -> plain_quotient_type

(** Convert a quotient type definition into source code. *)
val quotient_type_to_source_code :
  ?use_newlines:bool -> ('tag_e, 'tag_p) quotient_type -> string

(* TODO - QCheck_testing submodule for quotient_type *)
