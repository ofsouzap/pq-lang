open Core
open Pq_lang
open Utils
open Vtype
open Custom_types
open Pattern
open Ast
open Typing

(* TODO - move the generators and arbitrary instances for different things into the main lib/ things, and have them as submodules with a consistent name relating to QCheck *)

(* TODO - make maximum recursion depth parameters labelled (e.g. as "mrd") *)

(** The printing method for an AST representation of a program *)
type 'a ast_print_method =
  | NoPrint  (** Don't print the AST *)
  | PrintSexp of ('a -> Sexp.t)
      (** Print the sexp of the AST, using the provided sexp_of_ function for the values *)
  | PrintExprSource
      (** Print the source code representation of the AST, ignoring the tagging values *)

(** Take an AST printing method and return a function that implements the printing method.
    Returns None if no printing is specified. *)
val get_asp_printer_opt : 'a ast_print_method -> ('a expr -> string) option

(** Take an AST printing method and return a function that implements the printing method.
    Returns a function always returning the empty string if no printing is specified. *)
val get_asp_printer : 'a ast_print_method -> 'a expr -> string

(** A default AST printing method *)
val default_ast_print_method : 'a ast_print_method

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

(** Keywords for the lexer, to not be used in variable name generation *)
val lexer_keywords : string list

(** Generator for a small-length, non-empty string of lowercase characters *)
val varname_gen : string QCheck.Gen.t

(** Generator for a small-length, non-empty string of lowercase characters *)
val custom_type_name_gen : string QCheck.Gen.t

(** Generator for a small-length, non-empty string starting with uppercase character, then with only lowercase characters *)
val custom_type_constructor_name_gen : string QCheck.Gen.t

(** Implementation of a type context useful for tests *)
module TestingTypeCtx : sig
  include Typing.TypingTypeContext

  (** Get the type context as a list *)
  val to_list : t -> custom_type list

  (** Creates a type from a list *)
  val from_list : custom_type list -> t
end

(** Generator for variable types, taking a maximum recursion depth parameter *)
val vtype_gen : type_ctx:TestingTypeCtx.t -> int -> vtype QCheck.Gen.t

(** Arbitrary generator for variable types, taking a maximum recursion depth parameter *)
val vtype_arb : type_ctx:TestingTypeCtx.t -> int -> vtype QCheck.arbitrary

(** Generator for a pair of a variable name and a type for it *)
val typed_var_gen :
  type_ctx:TestingTypeCtx.t -> int -> (string * vtype) QCheck.Gen.t

(** Generator for a custom data type, taking a maximum number of constructors and a maximum recursion depth *)
val custom_type_gen :
  type_ctx:TestingTypeCtx.t ->
  max_constructors:int ->
  mrd:int ->
  custom_type QCheck.Gen.t

(** Arbitrary generator for a custom data type, taking a maximum number of constructors and a maximum recursion depth *)
val custom_type_arb :
  type_ctx:TestingTypeCtx.t ->
  max_constructors:int ->
  mrd:int ->
  custom_type QCheck.arbitrary

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

(** Arbitrary pattern generator that generates a pattern of the specified type as well as a list of the variables it defines *)
val pattern_arb :
  type_ctx:TestingTypeCtx.t ->
  t:vtype ->
  (pattern * (string * vtype) list) QCheck.arbitrary

(** Generator for an AST expression, possibly of a specified type.
    @param t (optional) The type that the generated output should type to *)
val ast_expr_gen :
  ?t:vtype ->
  type_ctx:TestingTypeCtx.t ->
  'a QCheck.Gen.t ->
  'a Ast.expr QCheck.Gen.t

(** Arbitrary generator for an AST expression, possibly of a specified type.
    Must be provided with a printing method for the AST and a generator for the tagging values.
    @param t (optional) The type that the generated output should type to *)
val ast_expr_arb :
  ?t:vtype ->
  type_ctx:TestingTypeCtx.t ->
  'a ast_print_method ->
  'a QCheck.Gen.t ->
  'a Ast.expr QCheck.arbitrary

(** Arbitrary generator for an AST expression of any type *)
val ast_expr_arb_any :
  type_ctx:TestingTypeCtx.t ->
  'a ast_print_method ->
  'a QCheck.Gen.t ->
  'a expr QCheck.arbitrary

(** Arbitrary expression and type context using a type context generated with the default parameters *)
val ast_expr_arb_default_type_ctx_params :
  ?t:vtype ->
  'a ast_print_method ->
  'a QCheck.Gen.t ->
  (TestingTypeCtx.t * 'a expr) QCheck.arbitrary

(** Arbitrary generator for an untagged AST expression of any type *)
val plain_ast_expr_arb_any :
  type_ctx:TestingTypeCtx.t -> unit expr QCheck.arbitrary

(** Arbitrary generator for an untagged AST expression of any type with default type context generation parameters *)
val plain_ast_expr_arb_any_default_type_ctx_params :
  (TestingTypeCtx.t * unit expr) QCheck.arbitrary

(** Arbitrary generator for a non-empty list *)
val nonempty_list_arb :
  'a QCheck.arbitrary -> 'a Nonempty_list.t QCheck.arbitrary

(** Arbitrary generator for the result type *)
val result_arb :
  'a QCheck.arbitrary ->
  'b QCheck.arbitrary ->
  ('a, 'b) Result.t QCheck.arbitrary
