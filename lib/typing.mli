open Custom_types
open Vtype
open Pattern

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
  | UndefinedCustomTypeConstructor of string
      (** The specified type constructor was used but hasn't been defined *)
  | PatternMultipleVariableDefinitions of string
      (** In a pattern, there are multiple definitions of some variable name *)
[@@deriving sexp, equal]

val equal_typing_error_variant : typing_error -> typing_error -> bool
val print_typing_error : typing_error -> string

(** Typing context for types (e.g. if a type of a certain name exists) *)
module type TypingTypeContext = sig
  type t

  (** Creates an empty typing context *)
  val empty : t

  (** Creates a typing context using the provided values *)
  val create : custom_types:custom_type list -> t

  (** Looks up a custom type, by name, in the context *)
  val find_custom : t -> string -> custom_type option

  (** Check whether a custom type exists in the context, by name *)
  val custom_exists : t -> string -> bool

  (** Find a custom type in the type context with a custom of the specified name. If multiple exist, only one is returned *)
  val find_custom_with_constructor :
    t -> string -> (custom_type * custom_type_constructor) option
end

(** Typing context of types using a simple set-based approach *)
module SetTypingTypeContext : TypingTypeContext

(** Typing contexts of variables *)
module type TypingVarContext = sig
  type t

  (** Creates an empty typing context *)
  val empty : t

  (** Adds a new variable with its type to the context, overwriting any existing values *)
  val add : t -> string -> vtype -> t

  (** Looks up a variable's type in the context *)
  val find : t -> string -> vtype option

  (** Create a context with a single entry *)
  val singleton : string -> vtype -> t

  (** Appends two variable contexts. When overwriting is necessary, the second argument overwrites the first *)
  val append : t -> t -> t

  (** Check whether a variable exists in the context, by name *)
  val exists : t -> string -> bool
end

(** Typing context of variables using a simple list-based approach *)
module ListTypingVarContext : TypingVarContext

(** Signature for module that provides type-checking functionality *)
module type TypeCheckerSig = functor
  (TypeCtx : TypingTypeContext)
  (VarCtx : TypingVarContext)
  -> sig
  (** The type of a type context that has been checked to be valid *)
  type checked_type_ctx

  (** Check a type context is valid *)
  val check_type_ctx : TypeCtx.t -> (checked_type_ctx, typing_error) Result.t

  (** The type of a program's expression that has passed type checking *)
  type 'a typed_program_expression

  (** Get the type context from a typed program expression *)
  val typed_program_expression_get_type_ctx :
    'a typed_program_expression -> TypeCtx.t

  (** Get the expression from a typed program expression *)
  val typed_program_expression_get_expression :
    'a typed_program_expression -> (vtype * 'a) Ast.expr

  (** Type checks a pattern in the given context, returning either the pattern's type and declared variables, or a pattern typing error *)
  val type_pattern :
    checked_type_ctx * VarCtx.t ->
    pattern ->
    (vtype * VarCtx.t, typing_error) Result.t

  (** Type checks an expression in the given context, returning either
      a typed expression or a typing error *)
  val type_expr :
    checked_type_ctx * VarCtx.t ->
    'a Ast.expr ->
    ('a typed_program_expression, typing_error) result
end

(** Functor for creating modules providing type-checking functionality *)
module TypeChecker : TypeCheckerSig

(** An implementation of a type checker using a list typing variable context *)
module SimpleTypeChecker : sig
  include module type of
      TypeChecker (SetTypingTypeContext) (ListTypingVarContext)
end

(** Type an AST expression using the default context implementations with the provided typing context *)
val type_expr :
  type_ctx:SetTypingTypeContext.t ->
  'a Ast.expr ->
  ('a SimpleTypeChecker.typed_program_expression, typing_error) result
