type typing_error = unit

(** Typing contexts of variables *)
module type TypingVarContext = sig
  type t

  (** Creates an empty typing context *)
  val empty : t

  (** Adds a new variable with its type to the context, overwriting any existing values *)
  val add : t -> string -> Vtype.vtype -> t

  (** Looks up a variable's type in the context *)
  val find : t -> string -> Vtype.vtype option
end

(** Typing context of variables using a simple list-based approach *)
module ListTypingVarContext : TypingVarContext

(** Provides type-checking functionality *)
module TypeExpr : functor (Ctx : TypingVarContext) -> sig
  (** Type checks an expression in the given context, returning either
      a typed expression or a typing error *)
  val type_expr :
    Ctx.t -> 'a Ast.expr -> ((Vtype.vtype * 'a) Ast.expr, typing_error) result
end

(** Type an AST expression using the default context implementation with an empty typing context *)
val type_expr :
  'a Ast.expr -> ((Vtype.vtype * 'a) Ast.expr, typing_error) result
