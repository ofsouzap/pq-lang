open Utils
open Varname
open Vtype
open Pattern
open Ast

(** A single equality constructor on a quotient type *)
type quotient_type_eqcons = {
  bindings : (varname * vtype) list;
      (** The variable bindings for the equality constructor of the equality
          constructor *)
  body : plain_pattern * plain_expr;
      (** The equality definition of the equality constructor *)
}
[@@deriving sexp, equal]

(** Get all names used in a quotient type equality constructor *)
val eqcons_existing_names : quotient_type_eqcons -> StringSet.t

(** Convert a quotient type equality constructor definition into source code. *)
val quotient_type_eqcons_to_source_code :
  ?use_newlines:bool -> quotient_type_eqcons -> string

(* TODO - QCheck_testing submodule for quotient_type_eqcons *)

(** A quotient type: a variant type with a list of quotients *)
type quotient_type = {
  name : string;  (** The name of the quotient type *)
  base_type_name : string;
      (** The name of the variant/quotient type that the quotient type is based
          on *)
  eqconss : quotient_type_eqcons list;
      (** The list of equality constructors for the quotient type *)
}
[@@deriving sexp, equal]

(** Get all names used in a quotient type definition *)
val existing_names : quotient_type -> StringSet.t

(** Convert a quotient type definition into source code. *)
val quotient_type_to_source_code : ?use_newlines:bool -> quotient_type -> string

(* TODO - QCheck_testing submodule for quotient_type *)
