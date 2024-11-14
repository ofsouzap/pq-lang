type vtype = VTypeInt | VTypeBool | VTypeFun of vtype * vtype
[@@deriving sexp, equal]

let rec vtype_to_source_code = function
  | VTypeInt -> "int"
  | VTypeBool -> "bool"
  | VTypeFun (t1, t2) ->
      Printf.sprintf "(%s) -> (%s)" (vtype_to_source_code t1)
        (vtype_to_source_code t2)
