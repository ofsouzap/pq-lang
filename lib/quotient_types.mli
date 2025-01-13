open Varname
open Vtype
open Pattern
open Ast

(** A single equality constructor on a quotient type *)
type quotient_type_eqcons = {
  binidings : (varname * vtype) list;
      (** The variable bindings for the equality constructor of the equality constructor *)
  eq : pattern * plain_expr;
      (** The equality definition of the equality constructor *)
}
[@@deriving sexp, equal]

(** Convert a quotient type equality constructor definition into source code. *)
val quotient_type_eqcons_to_source_code :
  ?use_newlines:bool -> quotient_type_eqcons -> string

(* TODO - QCheck_testing submodule for quotient_type_eqcons *)

(** A quotient type: a custom type with a list of quotients *)
type quotient_type = string * quotient_type_eqcons list [@@deriving sexp, equal]

(** Convert a quotient type definition into source code. *)
val quotient_type_to_source_code : ?use_newlines:bool -> quotient_type -> string

(* TODO - QCheck_testing submodule for quotient_type *)
