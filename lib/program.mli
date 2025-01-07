open Utils
open Custom_types
open Ast

(** A program, consisting of any number of custom type definitions and an expression to evaluate *)
type 'a program = { custom_types : custom_type list; e : 'a expr }
[@@deriving sexp, equal]

(** A program with no tagging *)
type plain_program = unit program [@@deriving sexp, equal]

(** Convert a program into source code.  *)
val program_to_source_code : 'a program -> string

module QCheck_testing : functor
  (Tag : sig
     type t
   end)
  -> sig
  type gen_options = {
    max_custom_types : int;
    max_custom_type_constructors : int;
    ast_gen_options : Ast.QCheck_testing(Tag).gen_options;
  }

  type arb_options = {
    max_custom_types : int;
    max_custom_type_constructors : int;
    ast_arb_options : Ast.QCheck_testing(Tag).arb_options;
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
