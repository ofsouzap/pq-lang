open Core
open Pq_lang
open Utils
open Vtype
open Pattern
open Ast

(* TODO - move the generators and arbitrary instances for different things into the main lib/ things, and have them as submodules with a consistent name relating to QCheck *)

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

(** Generator for variable types, taking a maximum recursion depth parameter *)
val vtype_gen : int -> vtype QCheck.Gen.t

(** Arbitrary generator for variable types, taking a maximum recursion depth parameter *)
val vtype_arb : int -> vtype QCheck.arbitrary

(** Generator for a pair of a variable name and a type for it *)
val typed_var_gen : int -> (string * vtype) QCheck.Gen.t

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
val testing_var_ctx_arb : TestingVarCtx.t QCheck.arbitrary

(** Arbitrary pattern generator that generates a pattern of the specified type as well as a list of the variables it defines *)
val pattern_arb : t:vtype -> (pattern * (string * vtype) list) QCheck.arbitrary

(* TODO - custom type definitions (arbitrary generator + integrate with ast arbitrary generator) *)

(** Arbitrary generator for an AST expression, possibly of a specified type.
    Must be provided with a printing method for the AST and a generator for the tagging values.
    @param t (optional) The type that the generated output should type to *)
val ast_expr_arb :
  ?t:vtype ->
  'a ast_print_method ->
  'a QCheck.Gen.t ->
  'a Ast.expr QCheck.arbitrary

(** Arbitrary generator for an AST expression of any type *)
val ast_expr_arb_any :
  'a ast_print_method -> 'a QCheck.Gen.t -> 'a expr QCheck.arbitrary

(** Arbitrary generator for an untagged AST expression of any type *)
val plain_ast_expr_arb_any : unit expr QCheck.arbitrary

(** Arbitrary generator for a non-empty list *)
val nonempty_list_arb :
  'a QCheck.arbitrary -> 'a Nonempty_list.t QCheck.arbitrary

(** Arbitrary generator for the result type *)
val result_arb :
  'a QCheck.arbitrary ->
  'b QCheck.arbitrary ->
  ('a, 'b) Result.t QCheck.arbitrary
