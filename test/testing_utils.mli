open Core
open Pq_lang
open Vtype
open Custom_types
open Typing

(** A default maximum number of defined custom types *)
val default_max_custom_type_count : int

(** A default maximum number of constructors for a custom type *)
val default_max_custom_type_constructor_count : int

(** A default maximum recursion depth for generation *)
val default_max_gen_rec_depth : int

(** A printing method for token lists *)
val token_printer : Parser.token list -> string

(** A variation of the default `exec_res` comparison function, that considers all typing errors equal *)
val override_equal_exec_res :
  Ast_executor.exec_res -> Ast_executor.exec_res -> bool

(** Implementation of a type context useful for tests *)
module TestingTypeCtx : sig
  include Typing.TypingTypeContext

  (** Add a custom type to the type context *)
  val add_custom : t -> custom_type -> t

  (** Creates a type from a list *)
  val from_list : custom_type list -> t

  (** If there are any defined custom types, get a generator for a random one of them *)
  val custom_gen_opt : t -> custom_type QCheck.Gen.t option

  (** Get the type context as a sexp *)
  val sexp_of_t : t -> Sexp.t
end

(** Generator for testing type context module context instances *)
val testing_type_ctx_gen :
  max_custom_types:int ->
  max_constructors:int ->
  mrd:int ->
  TestingTypeCtx.t QCheck.Gen.t

(** Arbitrary generator for testing type context module context instances *)
val testing_type_ctx_arb :
  max_custom_types:int ->
  max_constructors:int ->
  mrd:int ->
  TestingTypeCtx.t QCheck.arbitrary

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
end

(** Arbitrary generator for testing variable context module context instances *)
val testing_var_ctx_arb :
  type_ctx:TestingTypeCtx.t -> TestingVarCtx.t QCheck.arbitrary

module TestingTypeChecker : sig
  include module type of TypeChecker (TestingTypeCtx) (TestingVarCtx)
end
