open Core
open Vtype

(** A pattern in the language *)
type pattern =
  | PatName of string * vtype  (** A named and typed variable in a pattern *)
  | PatPair of pattern * pattern  (** A pair pattern *)
[@@deriving sexp, equal]

(** Convert a pattern to a source code representation *)
val pattern_to_source_code : pattern -> string
