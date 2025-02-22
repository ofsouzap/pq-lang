open Core
open Pq_lang
open Utils
module ProgramExecutor = ProgramExecutor.SimpleExecutor
module Program = ProgramExecutor.Program
module TypeChecker = ProgramExecutor.TypeChecker
module CustomType = Program.CustomType
module QuotientType = Program.CustomType.QuotientType
module Expr = Program.Expr
module Pattern = Program.Pattern

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
  ProgramExecutor.exec_err -> ProgramExecutor.exec_err -> bool

(** A variation of the default `exec_res` equality function, that considers all
    typing errors equal *)
val override_equal_exec_res :
  ProgramExecutor.exec_res -> ProgramExecutor.exec_res -> bool

(** A variation of the default typing error equality function, that ignores
    error messages *)
val override_equal_typing_error :
  TypeChecker.TypingError.t -> TypeChecker.TypingError.t -> bool

(** Implementation of a type context useful for tests *)
module TestingTypeCtx : sig
  include
    Pq_lang.TypeChecker.TypeContext.S
      with module CustomType = CustomType
       and module TypingError = TypeChecker.TypingError

  (** Add a variant type to the type context *)
  val add_variant : t -> VariantType.t -> t

  (** Add a quotient type to the type context *)
  val add_quotient : t -> ('tag_e, 'tag_p) QuotientType.t -> t

  (** Creates a type from a list *)
  val from_list : ('tag_e, 'tag_p) CustomType.t list -> t

  (** If there are any defined variant types, get a generator for a random one
      of them *)
  val variant_gen_opt : t -> VariantType.t QCheck.Gen.t option

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
  include Pq_lang.TypeChecker.VarContext.S

  (** Get a list of all the variables who have the given type *)
  val varnames_of_type : Vtype.t -> t -> string list

  (** Get the variable context as a list *)
  val to_list : t -> (string * Vtype.t) list

  (** Creates a context from a list *)
  val from_list : (string * Vtype.t) list -> t

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
  module Pattern = Pq_lang.Pattern.StdPattern
  module Expr = Pq_lang.Expr.StdExpr
  module Program = Pq_lang.Program.StdProgram

  module TypingError :
    Pq_lang.TypeChecker.TypingError.S
      with module Pattern = Program.Expr.Pattern
       and module Expr = Program.Expr

  module TypeCtx = TestingTypeCtx
  module VarCtx = TestingVarCtx

  include
    Pq_lang.TypeChecker.S
      with module Pattern := Program.Expr.Pattern
       and module Expr := Program.Expr
       and module Program := Program
       and module TypingError := TypingError
       and module TypeCtx := TypeCtx
       and module VarCtx := VarCtx
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
