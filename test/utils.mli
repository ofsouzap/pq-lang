open Core
open Pq_lang
open Vtype

val token_printer : Parser.token list -> string
val ast_printer : Ast.plain_expr -> string
val show_ast : Ast.plain_expr -> string

val override_compare_exec_res :
  Ast_executor.exec_res -> Ast_executor.exec_res -> bool

val show_plain_ast : Ast.plain_expr -> string
val show_tagged_ast : ('a -> Sexp.t) -> 'a Ast.expr -> string
val varname_gen : string QCheck.Gen.t
val vtype_gen : Vtype.vtype QCheck.Gen.t
val typed_var_gen : (string * Vtype.vtype) QCheck.Gen.t

val ast_expr_arb_any :
  ?val_sexp:('a -> Sexp.t) -> 'a QCheck.Gen.t -> 'a Ast.expr QCheck.arbitrary

val plain_ast_expr_arb_any : Ast.plain_expr QCheck.arbitrary

val ast_expr_arb_of_type :
  ?val_sexp:('a -> Sexp.t) ->
  vtype ->
  'a QCheck.Gen.t ->
  'a Ast.expr QCheck.arbitrary
