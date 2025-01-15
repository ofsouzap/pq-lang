open Utils
open Vtype
open Variant_types
open Ast
open Quotient_types

(** A type definition in a program. Either a variant type definition or a quotient type definition *)
type type_defn = VariantType of variant_type | QuotientType of quotient_type
[@@deriving sexp, equal]

(** Get the name of a defined type *)
val type_defn_name : type_defn -> string

(** A program, consisting of any number of variant type definitions and an expression to evaluate *)
type 'a program = { type_defns : type_defn list; e : 'a expr }
[@@deriving sexp, equal]

(** A program with no tagging *)
type plain_program = unit program [@@deriving sexp, equal]

(** Convert a program into source code *)
val program_to_source_code : ?use_newlines:bool -> 'a program -> string

module QCheck_testing : functor
  (Tag : sig
     type t
   end)
  -> sig
  type gen_options = {
    mrd : int;
    max_variant_types : int;
    max_variant_type_constructors : int;
    ast_type : vtype option;
    v_gen : Tag.t QCheck.Gen.t;
  }

  type arb_options = {
    gen : gen_options;
    print : Ast.QCheck_testing(Tag).ast_print_method;
    shrink : Ast.QCheck_testing(Tag).shrink_options;
  }

  include
    QCheck_testing_sig
      with type t = Tag.t program
       and type gen_options := gen_options
       and type print_options = Ast.QCheck_testing(Tag).ast_print_method
       and type shrink_options = Ast.QCheck_testing(Tag).shrink_options
       and type arb_options := arb_options
end
