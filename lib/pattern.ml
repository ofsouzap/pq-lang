open Core
open Vtype

type pattern = PatName of string * vtype | PatPair of pattern * pattern
[@@deriving sexp, equal]

let rec pattern_to_source_code = function
  | PatName (xname, t) -> sprintf "%s : (%s)" xname (vtype_to_source_code t)
  | PatPair (p1, p2) ->
      sprintf "(%s), (%s)"
        (pattern_to_source_code p1)
        (pattern_to_source_code p2)
