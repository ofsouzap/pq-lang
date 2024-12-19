open Core
open Vtype

(** A pattern in the language *)
type pattern =
  | PatName of string * vtype  (** A named and typed variable in a pattern *)
  | PatPair of pattern * pattern  (** A pair pattern *)
[@@deriving sexp, equal]

(** Get the type of a pattern *)
val pattern_vtype : pattern -> vtype
