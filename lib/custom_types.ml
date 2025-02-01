open Utils
open Variant_types
open Quotient_types

type custom_type = VariantType of variant_type | QuotientType of quotient_type
[@@deriving sexp, equal]

let custom_type_name : custom_type -> string = function
  | VariantType (vt_name, _) -> vt_name
  | QuotientType qt -> qt.name

let existing_names : custom_type -> StringSet.t = function
  | VariantType vt -> Variant_types.existing_names vt
  | QuotientType qt -> Quotient_types.existing_names qt
