(** This module provides functionality for directly executing an Expr of a
    program. *)

(** The data that the executor tags the interpreted Expr with *)
type expr_tag = unit [@@deriving sexp, equal]

(** The data that the executor tags the interpreted Expr patterns with *)
type pattern_tag = unit [@@deriving sexp, equal]

(** Properties of a closure *)
type closure_props = {
  param : Varname.t * Vtype.t;  (** The function's parameter's name and type *)
  out_type : Vtype.t;  (** The output type of the function *)
  body : (expr_tag, pattern_tag) Expr.typed_t;  (** The body of the function *)
  store : store;  (** The store to use when executing the function *)
  recursive : [ `Recursive of Varname.t | `NonRecursive ];
      (** Whether the function is recursive or not. If so, the name of the
          function itself, that is made re-accessible when executing the
          function *)
}
[@@deriving sexp, equal]

(** A resulting value from executing an Expr *)
and value =
  | Unit  (** The unit value *)
  | Int of int  (** An integer value *)
  | Bool of bool  (** A boolean value *)
  | Closure of closure_props  (** A function closure *)
  | Pair of value * value  (** A pair value *)
  | VariantTypeValue of VariantType.t * string * value
      (** A value of a variant type, with the type itself and the constructor
          name specified *)
[@@deriving sexp, equal]

(** A store, containing the values of variables under the current context *)
and store [@@deriving sexp, equal]

(** The empty store *)
val empty_store : store

(** Get a named variable's value from a store *)
val store_get : store -> Varname.t -> value option

(** Set a named variable's value to the provided value in the provided store and
    return the resulting store *)
val store_set : store -> key:Varname.t -> value:value -> store

(** Check if two stores are equal. This is, they have the exact same set of
    keys, and each key maps to the same value in both stores *)
val store_compare : store -> store -> bool

(** Traverse the entire store's variable names and values in arbitrary order
    into a list *)
val store_traverse : store -> (Varname.t * value) list

(** Details of a typing error. Fields are optional in case they can't be
    provided *)
type typing_error = {
  expected_type : string option;
  actual_type : string option;
  variable_name : Varname.t option;
  custom_message : string option;
}
[@@deriving sexp, equal]

(** Details for a typing error that are the default empty values *)
val empty_typing_error : typing_error

type exec_err =
  | TypeContextCreationError of TypeChecker.typing_error
      (** Error when forming a type context from a program *)
  | TypingError of typing_error
      (** Execution was halted due to a typing error *)
  | UndefinedVarError of Varname.t
      (** Execution was halted due to usage of an undefined variable of the
          provided name *)
  | MisplacedFixError  (** Fix node was inappropriately used in the Expr *)
  | FixApplicationError
      (** Application of the Fix node was done on an invalid target *)
  | MaxRecursionDepthExceeded
      (** The maximum recursion depth of the execution has been exceeded so the
          program was terminated *)
  | IncompleteMatchError
      (** No cases could be found that match the provided value in a match
          statement *)
  | UnknownVariantTypeConstructor of string
      (** No variant type could be found with a constructor of the specified
          name *)
[@@deriving sexp, equal]

(** Print human-readable string representation of an execution error *)
val print_exec_err : exec_err -> string

(** The result of executing an Expr *)
type exec_res = (value, exec_err) Result.t [@@deriving sexp, equal]

(** String representation of an execution result *)
val show_exec_res : exec_res -> string

(** Provides Expr execution functionality, given a typing context and variable
    context used for a type checker implementation *)
module Executor : functor
  (TypeCtx : TypeChecker.TypingTypeContext)
  (VarCtx : TypeChecker.TypingVarContext)
  -> sig
  (** Execute a typed program using the type checker constructed from TypeCtx
      and VarCtx *)
  val execute_program :
    ('tag_e, 'tag_p) TypeChecker.TypeChecker(TypeCtx)(VarCtx).typed_program ->
    exec_res
end

(** An implementation of the Expr executor using the simple type checker
    implementation *)
module SimpleExecutor : sig
  include module type of
      Executor
        (TypeChecker.SetTypingTypeContext)
        (TypeChecker.ListTypingVarContext)
end
