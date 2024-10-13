(** This module provides functionality for directly executing an AST of a program. *)

(** A resulting value from executing an AST *)
type value =
  | Int of int  (** An integer values *)
  | Bool of bool  (** A boolean value *)

(** The type of a variable's name in the store *)
type varname = string

(** A store, containing the values of variables under the current context *)
type store

(** The result of executing an AST *)
type exec_res =
  | Res of store * value
      (** The execution terminated with the provided value *)
  | TypingError  (** Execution was halted due to a typing error *)
  | UndefinedVarError of string
      (** Execution was halted due to usage of an undefined variable of the provided name *)

(* TODO - have exec_res just have two options, result or error, then error case can have an attached error, instead of the error type being part of exec_res *)

(** The empty store *)
val store_empty : store

(** Get a named variable's value from a store *)
val store_get : varname -> store -> value option

(** Set a named variable's value to the provided value in the provided store and return the resulting store *)
val store_set : varname -> value -> store -> store

(** String representation of a value *)
val show_value : value -> string

(** String representation of an execution result *)
val show_exec_res : exec_res -> string

(** Execute an AST representation of a program *)
val execute : Ast.expr -> exec_res
