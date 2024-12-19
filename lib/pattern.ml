open Vtype

type pattern = PatName of string * vtype | PatPair of pattern * pattern

let rec pattern_vtype = function
  | PatName (_, t) -> t
  | PatPair (p1, p2) -> VTypePair (pattern_vtype p1, pattern_vtype p2)
