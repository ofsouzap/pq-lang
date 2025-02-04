open Variant_types
open Vtype
open Pattern
open Ast
open Quotient_types
open Custom_types
open Program

(** Typing errors *)
type typing_error =
  | UndefinedVariable of string
      (** A variable was referenced that isn't defined in the scope *)
  | TypeMismatch of vtype * vtype
      (** An expression was expected to have the first type but had the second
      *)
  | PatternTypeMismatch of plain_pattern * vtype * vtype
      (** A pattern was expected to have the first type but had the second *)
  | EqConsBodyTypeMismatch of quotient_type_eqcons * vtype * vtype
      (** The body of an equivalence constructor was expected to have the first
          type but had the second *)
  | EqualOperatorTypeMistmatch of vtype * vtype
      (** An application of the equality operation had a type mismatch as the
          operands had the specified types instead of compatible ones *)
  | ExpectedFunctionOf of vtype
      (** A value is used as a function but isn't a function. The expected input
          type of the function is the value *)
  | UndefinedVariantTypeConstructor of string
      (** The specified type constructor was used but hasn't been defined *)
  | PatternMultipleVariableDefinitions of string
      (** In a pattern, there are multiple definitions of some variable name *)
  | MultipleTopLevelNameDefinitions of string
      (** There are multiple definitions of the same top-level name *)
  | DuplicateTypeNameDefinition of string
      (** The specified type name has been defined multiple times *)
  | UndefinedTypeName of string
      (** The specified type name has been referenced but not defined *)
  | MultipleVariantTypeConstructorDefinitions of string
      (** The specified variant type constructor name has been defined multiple
          times *)
[@@deriving sexp, equal]

val equal_typing_error_variant : typing_error -> typing_error -> bool
val print_typing_error : typing_error -> string

(** Typing context for types (e.g. if a type of a certain name exists) *)
module type TypingTypeContext = sig
  type t

  (** Creates an empty typing context *)
  val empty : t

  (** Creates a typing context using the provided values *)
  val create : custom_types:custom_type list -> (t, typing_error) Result.t

  (** Looks up a type definition, by name, in the context *)
  val find_type_defn_by_name : t -> string -> custom_type option

  (** Check whether a type definition exists in the context, by name *)
  val type_defn_exists : t -> string -> bool

  (** Find a variant type in the type context with a variant of the specified
      name. If multiple exist, only one is returned *)
  val find_variant_type_with_constructor :
    t -> string -> (variant_type * variant_type_constructor) option

  (** Get a list of the variant types defined in the context *)
  val type_defns_to_list : t -> custom_type list
end

(** Typing context of types using a simple set-based approach *)
module SetTypingTypeContext : TypingTypeContext

(** Typing contexts of variables *)
module type TypingVarContext = sig
  type t

  (** Creates an empty typing context *)
  val empty : t

  (** Adds a new variable with its type to the context, overwriting any existing
      values *)
  val add : t -> string -> vtype -> t

  (** Looks up a variable's type in the context *)
  val find : t -> string -> vtype option

  (** Create a context with a single entry *)
  val singleton : string -> vtype -> t

  (** Appends two variable contexts. When overwriting is necessary, the second
      argument overwrites the first *)
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

  (** A checked version of the empty type context *)
  val checked_empty_type_ctx : checked_type_ctx

  (** The type of a program that has passed type checking *)
  type ('tag_e, 'tag_p) typed_program

  (** Get the expression from a typed program expression *)
  val typed_program_get_program :
    ('tag_e, 'tag_p) typed_program ->
    (vtype * 'tag_e, vtype * 'tag_p) Program.program

  (** Check that a vtype is valid in the given context *)
  val check_vtype : checked_type_ctx -> vtype -> (unit, typing_error) Result.t

  (** Type checks a pattern in the given context, returning either the pattern's
      type and declared variables, or a pattern typing error *)
  val type_pattern :
    checked_type_ctx * VarCtx.t ->
    'tag_p pattern ->
    ((vtype * 'tag_p) pattern * VarCtx.t, typing_error) Result.t

  (** Type checks a single expression in the given context *)
  val type_expr :
    checked_type_ctx * VarCtx.t ->
    ('tag_e, 'tag_p) expr ->
    (('tag_e, 'tag_p) typed_expr, typing_error) Result.t

  (** Check a type context is valid *)
  val check_type_ctx : TypeCtx.t -> (checked_type_ctx, typing_error) Result.t

  (** Type checks a program in the given context, returning either a typed
      program or a typing error *)
  val type_program :
    ('tag_e, 'tag_p) program ->
    (('tag_e, 'tag_p) typed_program, typing_error) Result.t
end

(** Functor for creating modules providing type-checking functionality *)
module TypeChecker : TypeCheckerSig

(** An implementation of a type checker using a list typing variable context *)
module SimpleTypeChecker : sig
  include module type of
      TypeChecker (SetTypingTypeContext) (ListTypingVarContext)
end

(** Type program using the default context implementation *)
val type_expr :
  type_ctx:SetTypingTypeContext.t ->
  ('tag_e, 'tag_p) expr ->
  (('tag_e, 'tag_p) typed_expr, typing_error) result

(** Type program using the default context implementations *)
val type_program :
  ('tag_e, 'tag_p) program ->
  (('tag_e, 'tag_p) SimpleTypeChecker.typed_program, typing_error) result
