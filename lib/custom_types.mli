open Variant_types
open Quotient_types

(** A user-defined type. Either a variant type definition or a quotient type definition *)
type custom_type = VariantType of variant_type | QuotientType of quotient_type
[@@deriving sexp, equal]

(** Get the name of a custom type *)
val custom_type_name : custom_type -> string

(* TODO - QCheck_testing submodule *)
