open Utils
open Vtype
open Ast
open Custom_types

(** A program, consisting of any number of custom type definitions and an
    expression to evaluate *)
type ('tag_e, 'tag_p) program = {
  custom_types : custom_type list;
  e : ('tag_e, 'tag_p) expr;
}
[@@deriving sexp, equal]

(** A program with no tagging *)
type plain_program = (unit, unit) program [@@deriving sexp, equal]

(** Convert a program into source code *)
val program_to_source_code :
  ?use_newlines:bool -> ('tag_e, 'tag_p) program -> string

(** Get the set of all names used in a program, including variable names (in any
    scope), type names, and variant type constructors *)
val program_existing_names : ('tag_e, 'tag_p) program -> StringSet.t

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
    ast_type : vtype option;
    expr_v_gen : TagExpr.t QCheck.Gen.t;
    pat_v_gen : TagPat.t QCheck.Gen.t;
  }

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
