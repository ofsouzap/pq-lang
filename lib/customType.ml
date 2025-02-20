open Core
open Utils

type ('tag_e, 'tag_p) t =
  | VariantType of VariantType.t
  | QuotientType of ('tag_e, 'tag_p) QuotientType.t
[@@deriving sexp, equal]

type plain_t = (unit, unit) t [@@deriving sexp, equal]

type ('tag_e, 'tag_p) typed_custom_type = (Vtype.t * 'tag_e, Vtype.t * 'tag_p) t
[@@deriving sexp, equal]

let name : ('tag_e, 'tag_p) t -> string = function
  | VariantType (vt_name, _) -> vt_name
  | QuotientType qt -> qt.name

let existing_names : ('tag_e, 'tag_p) t -> StringSet.t = function
  | VariantType vt -> VariantType.existing_names vt
  | QuotientType qt -> QuotientType.existing_names qt

let fmap_expr ~(f : 'tag_e1 -> 'tag_e2) :
    ('tag_e1, 'tag_p) t -> ('tag_e2, 'tag_p) t = function
  | VariantType vt -> VariantType vt
  | QuotientType qt -> QuotientType (QuotientType.fmap_expr ~f qt)

let fmap_pattern ~(f : 'tag_p1 -> 'tag_p2) :
    ('tag_e, 'tag_p1) t -> ('tag_e, 'tag_p2) t = function
  | VariantType vt -> VariantType vt
  | QuotientType qt -> QuotientType (QuotientType.fmap_pattern ~f qt)

let to_plain_custom_type : ('tag_e, 'tag_p) t -> plain_t = function
  | VariantType vt -> VariantType vt
  | QuotientType qt -> QuotientType (QuotientType.to_plain_quotient_type qt)
