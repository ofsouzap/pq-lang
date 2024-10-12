(** This module provides functionality for directly executing an AST of a program. *)

(** A resulting value from executing an AST *)
type value =
  | Int of int  (** An integer values *)
  | Bool of bool  (** A boolean value *)

(** The result of executing an AST *)
type exec_res =
  | Value of value  (** The execution terminated with the provided value *)
  | TypingError  (** Execution was halted due to a typing error *)

(** String representation of a value *)
val show_value : value -> string

(** String representation of an execution result *)
val show_exec_res : exec_res -> string

(** Execute an AST representation of a program *)
val execute : Ast.expr -> exec_res
