open Utils
open Vtype
open Varname
open Expr
open Custom_types

(** A top-level function definition *)
type ('tag_e, 'tag_p) top_level_defn = {
  recursive : bool;
  name : string;
  param : varname * vtype;
  return_t : vtype;
  body : ('tag_e, 'tag_p) expr;
}
[@@deriving sexp, equal]

type plain_top_level_defn = (unit, unit) top_level_defn

(** Convert a program into source code *)
val top_level_defn_to_source_code :
  use_newlines:bool -> ('tag_e, 'tag_p) top_level_defn -> string

(** A program, consisting of any number of custom type definitions, top-level
    definitions and an expression to evaluate *)
type ('tag_e, 'tag_p) program = {
  custom_types : ('tag_e, 'tag_p) custom_type list;
  top_level_defns : ('tag_e, 'tag_p) top_level_defn list;
  e : ('tag_e, 'tag_p) expr;
}
[@@deriving sexp, equal]

(** A program with typing information *)
type ('tag_e, 'tag_p) typed_program = (vtype * 'tag_e, vtype * 'tag_p) program
[@@deriving sexp, equal]

(** A program with no tagging *)
type plain_program = (unit, unit) program [@@deriving sexp, equal]

(** Map a function onto the tags of expression nodes in a program *)
val fmap_expr :
  f:('tag_e1 -> 'tag_e2) ->
  ('tag_e1, 'tag_p) program ->
  ('tag_e2, 'tag_p) program

(** Map a function onto the tags of pattern nodes in a program *)
val fmap_pattern :
  f:('tag_p1 -> 'tag_p2) ->
  ('tag_e, 'tag_p1) program ->
  ('tag_e, 'tag_p2) program

(** Get all the names used (defined or referenced) in a program. Includes
    variable names, defined type names, variant type constructor names, etc. *)
val existing_names : ('tag_e, 'tag_p) program -> StringSet.t

(** Convert a program into source code *)
val program_to_source_code :
  ?use_newlines:bool -> ('tag_e, 'tag_p) program -> string

module QCheck_testing : functor
  (TagExpr : sig
     type t
   end)
  (TagPat : sig
     type t
   end)
  -> sig
  type gen_options = {
    mrd : int;
    max_variant_types : int;
    max_variant_type_constructors : int;
    max_top_level_defns : int;
    allow_fun_types : bool;
    body_type : vtype option;
    expr_v_gen : TagExpr.t QCheck.Gen.t;
    pat_v_gen : TagPat.t QCheck.Gen.t;
  }

  val gen_top_level_defn :
    expr_v_gen:TagExpr.t QCheck.Gen.t ->
    pat_v_gen:TagPat.t QCheck.Gen.t ->
    top_level_defns:(varname * (vtype * vtype)) list ->
    variant_types:VariantType.t list ->
    mrd:int ->
    (TagExpr.t, TagPat.t) top_level_defn QCheck.Gen.t

  type arb_options = {
    gen : gen_options;
    print : Expr.QCheck_testing(TagExpr)(TagPat).expr_print_method;
    shrink : Expr.QCheck_testing(TagExpr)(TagPat).shrink_options;
  }

  include
    QCheck_testing_sig
      with type t = (TagExpr.t, TagPat.t) program
       and type gen_options := gen_options
       and type print_options =
        Expr.QCheck_testing(TagExpr)(TagPat).expr_print_method
       and type shrink_options =
        Expr.QCheck_testing(TagExpr)(TagPat).shrink_options
       and type arb_options := arb_options
end
