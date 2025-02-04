open Core
open Utils
open Vtype
open Variant_types
open Quotient_types

type ('tag_e, 'tag_p) custom_type =
  | VariantType of variant_type
  | QuotientType of ('tag_e, 'tag_p) quotient_type
[@@deriving sexp, equal]

type plain_custom_type = (unit, unit) custom_type [@@deriving sexp, equal]

type ('tag_e, 'tag_p) typed_custom_type =
  (vtype * 'tag_e, vtype * 'tag_p) custom_type
[@@deriving sexp, equal]

let custom_type_name : ('tag_e, 'tag_p) custom_type -> string = function
  | VariantType (vt_name, _) -> vt_name
  | QuotientType qt -> qt.name

let existing_names : ('tag_e, 'tag_p) custom_type -> StringSet.t = function
  | VariantType vt -> Variant_types.existing_names vt
  | QuotientType qt -> Quotient_types.existing_names qt

let fmap_expr ~(f : 'tag_e1 -> 'tag_e2) :
    ('tag_e1, 'tag_p) custom_type -> ('tag_e2, 'tag_p) custom_type = function
  | VariantType vt -> VariantType vt
  | QuotientType qt -> QuotientType (Quotient_types.fmap_expr ~f qt)

let fmap_pattern ~(f : 'tag_p1 -> 'tag_p2) :
    ('tag_e, 'tag_p1) custom_type -> ('tag_e, 'tag_p2) custom_type = function
  | VariantType vt -> VariantType vt
  | QuotientType qt -> QuotientType (Quotient_types.fmap_pattern ~f qt)

let to_plain_custom_type : ('tag_e, 'tag_p) custom_type -> plain_custom_type =
  function
  | VariantType vt -> VariantType vt
  | QuotientType qt -> QuotientType (Quotient_types.to_plain_quotient_type qt)
