open Utils
open Vtype
open Variant_types
open Varname
open Ast
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

(** A program, consisting of any number of custom type definitions, top-level
    definitions and an expression to evaluate *)
type ('tag_e, 'tag_p) program = {
  custom_types : custom_type list;
  top_level_defns : ('tag_e, 'tag_p) top_level_defn list;
  e : ('tag_e, 'tag_p) expr;
}
[@@deriving sexp, equal]

(** A program with no tagging *)
type plain_program = (unit, unit) program [@@deriving sexp, equal]

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
    ast_type : vtype option;
    expr_v_gen : TagExpr.t QCheck.Gen.t;
    pat_v_gen : TagPat.t QCheck.Gen.t;
  }

  val gen_top_level_defn :
    expr_v_gen:TagExpr.t QCheck.Gen.t ->
    pat_v_gen:TagPat.t QCheck.Gen.t ->
    top_level_defns:(varname * (vtype * vtype)) list ->
    variant_types:variant_type list ->
    mrd:int ->
    (TagExpr.t, TagPat.t) top_level_defn QCheck.Gen.t

  type arb_options = {
    gen : gen_options;
    print : Ast.QCheck_testing(TagExpr)(TagPat).ast_print_method;
    shrink : Ast.QCheck_testing(TagExpr)(TagPat).shrink_options;
  }

  include
    QCheck_testing_sig
      with type t = (TagExpr.t, TagPat.t) program
       and type gen_options := gen_options
       and type print_options =
        Ast.QCheck_testing(TagExpr)(TagPat).ast_print_method
       and type shrink_options =
        Ast.QCheck_testing(TagExpr)(TagPat).shrink_options
       and type arb_options := arb_options
end
