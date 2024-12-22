open Vtype
open Pattern

(** Typing errors for typing patterns *)
type pattern_typing_error =
  | MultipleVariableDefinitions of string
      (** Multiple definitions of some variable name in the pattern have been found *)
[@@deriving sexp, equal]

val equal_pattern_typing_error_variant :
  pattern_typing_error -> pattern_typing_error -> bool

val print_pattern_typing_error : pattern_typing_error -> string

(** Typing errors *)
type typing_error =
  | UndefinedVariable of string
      (** A variable was referenced that isn't defined in the scope *)
  | TypeMismatch of vtype * vtype
      (** An expression was expected to have the first type but had the second *)
  | PatternTypeMismatch of pattern * vtype * vtype
      (** A pattern was expected to have the first type but had the second *)
  | EqualOperatorTypeMistmatch of vtype * vtype
      (** An application of the equality operation had a type mismatch as the operands had the specified types instead of compatible ones *)
  | ExpectedFunctionOf of vtype
      (** A value is used as a function but isn't a function. The expected input type of the function is the value *)
  | PatternTypingError of pattern_typing_error
      (** There was an error when typing a pattern *)
[@@deriving sexp, equal]

val equal_typing_error_variant : typing_error -> typing_error -> bool
val print_typing_error : typing_error -> string

(** Typing contexts of variables *)
module type TypingVarContext = sig
  type t

  (** Creates an empty typing context *)
  val empty : t

  (** Adds a new variable with its type to the context, overwriting any existing values *)
  val add : t -> string -> Vtype.vtype -> t

  (** Looks up a variable's type in the context *)
  val find : t -> string -> Vtype.vtype option

  (** Create a context with a single entry *)
  val singleton : string -> vtype -> t

  (** Appends two variable contexts. When overwriting is necessary, the second argument overwrites the first *)
  val append : t -> t -> t

  (** Check whether a variable exists in the context, by name *)
  val exists : t -> string -> bool

  (** Get a list representation of the variable typing context *)
  val to_list : t -> (string * vtype) list
end

(** Typing context of variables using a simple list-based approach *)
module ListTypingVarContext : TypingVarContext

(** Provides type-checking functionality *)
module TypeChecker : functor (Ctx : TypingVarContext) -> sig
  (** Type checks a pattern in the given context, returning either the pattern's type and declared variables, or a pattern typing error *)
  val type_pattern :
    Ctx.t -> pattern -> (vtype * Ctx.t, pattern_typing_error) Result.t

  (** Type checks an expression in the given context, returning either
      a typed expression or a typing error *)
  val type_expr :
    Ctx.t -> 'a Ast.expr -> ((Vtype.vtype * 'a) Ast.expr, typing_error) result
end

(** An implementation of a type checker using a list typing variable context *)
module ListTypeChecker : sig
  include module type of TypeChecker (ListTypingVarContext)
end

(** Type an AST expression using the default context implementation with an empty typing context *)
val type_expr :
  'a Ast.expr -> ((Vtype.vtype * 'a) Ast.expr, typing_error) result
