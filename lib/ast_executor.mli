(** This module provides functionality for directly executing an AST of a program. *)

(** The type of a variable's name in the store *)
type varname = string

(** Properties of a closure *)
type closure_props = varname * Ast.expr * store [@@deriving equal]

(** A resulting value from executing an AST *)
and value =
  | Int of int  (** An integer values *)
  | Bool of bool  (** A boolean value *)
  | Closure of closure_props  (** A function closure *)
[@@deriving equal]

(** A store, containing the values of variables under the current context *)
and store [@@deriving equal]

(** The empty store *)
val empty_store : store

(** Get a named variable's value from a store *)
val store_get : store -> varname -> value option

(** Set a named variable's value to the provided value in the provided store and return the resulting store *)
val store_set : store -> key:varname -> value:value -> store

(** Check if two stores are equal. This is, they have the exact same set of keys, and each key maps to the same value in both stores *)
val store_compare : store -> store -> bool

(** Traverse the entire store's variable names and values in arbitrary order into a list *)
val store_traverse : store -> (varname * value) list

(** String representation of a store, for debugging purposes *)
val show_store : store -> string

(** Details of a typing error. Fields are optional in case they can't be provided *)
type typing_error = {
  expected_type : string option;
  actual_type : string option;
  variable_name : varname option;
  custom_message : string option;
}

(** Details for a typing error that are the default empty values *)
val empty_typing_error : typing_error

type exec_err =
  | TypingError of typing_error
      (** Execution was halted due to a typing error *)
  | UndefinedVarError of string
      (** Execution was halted due to usage of an undefined variable of the provided name *)
  | MisplacedFixError  (** Fix node was inappropriately used in the AST *)
  | FixApplicationError
      (** Application of the Fix node was done on an invalid target *)
  | MaxRecursionDepthExceeded
      (** The maximum recursion depth of the execution has been exceeded so the program was terminated *)

(** The result of executing an AST *)
type exec_res =
  | Res of value  (** The execution terminated with the provided value *)
  | Err of exec_err  (** The execution terminated with an error *)

(** Check if two execution results are equal *)
val exec_res_compare : exec_res -> exec_res -> bool

(** String representation of a value *)
val show_value : value -> string

(** String representation of an execution result *)
val show_exec_res : exec_res -> string

(** Execute an AST representation of a program *)
val execute : Ast.expr -> exec_res
