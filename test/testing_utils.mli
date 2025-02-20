open Core
open Pq_lang
open Utils
open Vtype
open Variant_types
open Quotient_types
open Custom_types
open Typing

(** A default maximum number of defined variant types *)
val default_max_variant_type_count : int

(** A default maximum number of constructors for a variant type *)
val default_max_variant_type_constructor_count : int

(** A default maximum number of defined quotient types *)
val default_max_quotient_type_count : int

(** A default maximum number of top-level definitions *)
val default_max_top_level_defns_count : int

(** A default maximum recursion depth for generation *)
val default_max_gen_rec_depth : int

(** A printing method for token lists *)
val token_printer : Parser.token list -> string

(** A variation of the default `exec_err` equality function, that considers all
    typing errors equal *)
val override_equal_exec_err :
  Program_executor.exec_err -> Program_executor.exec_err -> bool

(** A variation of the default `exec_res` equality function, that considers all
    typing errors equal *)
val override_equal_exec_res :
  Program_executor.exec_res -> Program_executor.exec_res -> bool

(** A variation of the default typing error equality function, that ignores
    error messages *)
val override_equal_typing_error :
  Typing.typing_error -> Typing.typing_error -> bool

(** Implementation of a type context useful for tests *)
module TestingTypeCtx : sig
  include Typing.TypingTypeContext

  (** Add a variant type to the type context *)
  val add_variant : t -> variant_type -> t

  (** Add a quotient type to the type context *)
  val add_quotient : t -> ('tag_e, 'tag_p) quotient_type -> t

  (** Creates a type from a list *)
  val from_list : ('tag_e, 'tag_p) custom_type list -> t

  (** If there are any defined variant types, get a generator for a random one
      of them *)
  val variant_gen_opt : t -> variant_type QCheck.Gen.t option

  (** Get the type context as a sexp *)
  val sexp_of_t : t -> Sexp.t

  module QCheck_testing : sig
    type gen_options = {
      max_variant_types : int;
      max_constructors : int;
      max_quotient_types : int;
      mrd : int;
    }

    include
      QCheck_testing_sig
        with type t = t
         and type gen_options := gen_options
         and type print_options = unit
         and type shrink_options = unit
         and type arb_options := gen_options
  end
end

(** Generator for testing type context wwith default settings *)
val default_testing_type_ctx_gen : TestingTypeCtx.t QCheck.Gen.t

(** Arbitrary generator for testing type context wwith default settings *)
val default_testing_type_ctx_arb : TestingTypeCtx.t QCheck.arbitrary

(** Implementation of a variable context useful for tests *)
module TestingVarCtx : sig
  include Typing.TypingVarContext

  (** Get a list of all the variables who have the given type *)
  val varnames_of_type : vtype -> t -> string list

  (** Get the variable context as a list *)
  val to_list : t -> (string * vtype) list

  (** Creates a context from a list *)
  val from_list : (string * vtype) list -> t

  module QCheck_testing : sig
    include
      QCheck_testing_sig
        with type t = t
         and type gen_options = TestingTypeCtx.t
         and type print_options = unit
         and type shrink_options = unit
         and type arb_options = TestingTypeCtx.t
  end
end

module TestingTypeChecker : sig
  include module type of TypeChecker (TestingTypeCtx) (TestingVarCtx)
end

module UnitTag : sig
  type t = unit [@@deriving sexp, equal]

  val sexp_of_t : t -> Sexp.t
  val t_of_sexp : Sexp.t -> t
  val equal : t -> t -> bool
end

module Unit_expr_qcheck_testing : sig
  include module type of Expr.QCheck_testing (UnitTag) (UnitTag)
end

module Unit_program_qcheck_testing : sig
  include module type of Program.QCheck_testing (UnitTag) (UnitTag)
end

val unit_program_arbitrary_with_default_options :
  Unit_program_qcheck_testing.t QCheck.arbitrary
