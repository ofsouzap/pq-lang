open Core
open Utils

type vtype =
  | VTypeUnit
  | VTypeInt
  | VTypeBool
  | VTypePair of vtype * vtype
  | VTypeFun of vtype * vtype
  | VTypeCustom of string
[@@deriving sexp, equal]

let rec vtype_to_source_code = function
  | VTypeUnit -> "unit"
  | VTypeInt -> "int"
  | VTypeBool -> "bool"
  | VTypePair (t1, t2) ->
      Printf.sprintf "(%s) * (%s)" (vtype_to_source_code t1)
        (vtype_to_source_code t2)
  | VTypeFun (t1, t2) ->
      Printf.sprintf "(%s) -> (%s)" (vtype_to_source_code t1)
        (vtype_to_source_code t2)
  | VTypeCustom tname -> tname

module QCheck_testing : sig
  type gen_options = { variant_types : StringSet.t; mrd : int }

  include
    Utils.QCheck_testing_sig
      with type t = vtype
       and type gen_options := gen_options
       and type print_options = unit
       and type shrink_options = unit
       and type arb_options := gen_options
end = struct
  type t = vtype
  type gen_options = { variant_types : StringSet.t; mrd : int }
  type print_options = unit
  type shrink_options = unit
  type arb_options = gen_options

  let gen =
    let open QCheck.Gen in
    fix (fun self opts ->
        let self' = self { opts with mrd = opts.mrd - 1 } in
        let variant_type_exists = not (Set.is_empty opts.variant_types) in
        let base_cases =
          [ return VTypeUnit; return VTypeInt; return VTypeBool ]
          @ Option.to_list
              (if variant_type_exists then
                 Some
                   ( opts.variant_types |> Set.to_list |> oneofl
                   >|= fun ct_name -> VTypeCustom ct_name )
               else None)
        in
        let rec_cases =
          [
            (pair self' self' >|= fun (t1, t2) -> VTypeFun (t1, t2));
            (pair self' self' >|= fun (t1, t2) -> VTypePair (t1, t2));
          ]
        in
        if opts.mrd > 0 then oneof (base_cases @ rec_cases)
        else oneof base_cases)

  let print () = vtype_to_source_code

  let rec shrink () =
    let open QCheck.Iter in
    function
    | VTypeUnit | VTypeInt | VTypeBool | VTypeCustom _ -> empty
    | VTypePair (t1, t2) ->
        return t1 <+> return t2
        <+> (shrink () t1 >|= fun t1' -> VTypePair (t1', t2))
        <+> (shrink () t2 >|= fun t2' -> VTypePair (t1, t2'))
    | VTypeFun (t1, t2) ->
        return t1 <+> return t2
        <+> (shrink () t1 >|= fun t1' -> VTypeFun (t1', t2))
        <+> (shrink () t2 >|= fun t2' -> VTypeFun (t1, t2'))

  let arbitrary (opts : arb_options) =
    QCheck.make ~print:(print ()) ~shrink:(shrink ()) (gen opts)
end
