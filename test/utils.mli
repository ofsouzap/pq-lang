open Core
open Pq_lang
open Vtype
open Ast

(* TODO - doc comments *)

type 'a ast_print_method =
  | NoPrint
  | PrintSexp of ('a -> Sexp.t)
  | PrintExprSource

val default_ast_print_method : 'a ast_print_method
val default_max_gen_rec_depth : int
val token_printer : Parser.token list -> string
val ast_sexp_printer : Ast.plain_expr -> string
val show_ast_sexp : Ast.plain_expr -> string

val override_compare_exec_res :
  Ast_executor.exec_res -> Ast_executor.exec_res -> bool

val show_plain_ast : Ast.plain_expr -> string
val show_tagged_ast : ('a -> Sexp.t) -> 'a Ast.expr -> string
val varname_gen : string QCheck.Gen.t
val vtype_gen : int -> Vtype.vtype QCheck.Gen.t
val typed_var_gen : int -> (string * Vtype.vtype) QCheck.Gen.t

val ast_expr_arb :
  ?t:vtype ->
  'a ast_print_method ->
  'a QCheck.Gen.t ->
  'a Ast.expr QCheck.arbitrary

val ast_expr_arb_any :
  'a ast_print_method -> 'a QCheck.Gen.t -> 'a expr QCheck.arbitrary

val plain_ast_expr_arb_any : unit expr QCheck.arbitrary
