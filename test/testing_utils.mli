open Core
open Pq_lang
open Utils
open Vtype
open Pattern
open Ast

(* TODO - doc comments *)

type 'a ast_print_method =
  | NoPrint
  | PrintSexp of ('a -> Sexp.t)
  | PrintExprSource

val get_asp_printer_opt : 'a ast_print_method -> ('a expr -> string) option
val get_asp_printer : 'a ast_print_method -> 'a expr -> string
val default_ast_print_method : 'a ast_print_method
val default_max_gen_rec_depth : int
val token_printer : Parser.token list -> string

val override_equal_exec_res :
  Ast_executor.exec_res -> Ast_executor.exec_res -> bool

val lexer_keywords : string list
val varname_gen : string QCheck.Gen.t
val vtype_gen : int -> vtype QCheck.Gen.t
val vtype_arb : int -> vtype QCheck.arbitrary
val typed_var_gen : int -> (string * vtype) QCheck.Gen.t

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

val ast_expr_arb :
  ?t:vtype ->
  'a ast_print_method ->
  'a QCheck.Gen.t ->
  'a Ast.expr QCheck.arbitrary

val ast_expr_arb_any :
  'a ast_print_method -> 'a QCheck.Gen.t -> 'a expr QCheck.arbitrary

val plain_ast_expr_arb_any : unit expr QCheck.arbitrary

val nonempty_list_arb :
  'a QCheck.arbitrary -> 'a Nonempty_list.t QCheck.arbitrary

val result_arb :
  'a QCheck.arbitrary ->
  'b QCheck.arbitrary ->
  ('a, 'b) Result.t QCheck.arbitrary
