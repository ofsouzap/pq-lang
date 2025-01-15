open Variant_types
open Quotient_types

type custom_type = VariantType of variant_type | QuotientType of quotient_type
[@@deriving sexp, equal]

let custom_type_name : custom_type -> string = function
  | VariantType (vt_name, _) -> vt_name
  | QuotientType qt -> qt.name
