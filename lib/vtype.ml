open Core
open Utils

type t =
  | VTypeUnit
  | VTypeInt
  | VTypeBool
  | VTypePair of t * t
  | VTypeFun of t * t
  | VTypeCustom of string
[@@deriving sexp, equal, compare]

let rec to_source_code = function
  | VTypeUnit -> "unit"
  | VTypeInt -> "int"
  | VTypeBool -> "bool"
  | VTypePair (t1, t2) ->
      Printf.sprintf "(%s) * (%s)" (to_source_code t1) (to_source_code t2)
  | VTypeFun (t1, t2) ->
      Printf.sprintf "(%s) -> (%s)" (to_source_code t1) (to_source_code t2)
  | VTypeCustom tname -> tname

type vtype = t

module QCheck_testing : sig
  type gen_options = {
    variant_types : StringSet.t;
    allow_fun_types : bool;
    mrd : int;
  }

  include
    Utils.QCheck_testing_sig
      with type t = vtype
       and type gen_options := gen_options
       and type print_options = unit
       and type shrink_options = unit
       and type arb_options := gen_options
end = struct
  type t = vtype

  type gen_options = {
    variant_types : StringSet.t;
    allow_fun_types : bool;
    mrd : int;
  }

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
                   >|= fun vt_name -> VTypeCustom vt_name )
               else None)
        in
        let rec_cases =
          (if
             (* Have a case for the function case if it is allowed *)
             opts.allow_fun_types
           then [ (pair self' self' >|= fun (t1, t2) -> VTypeFun (t1, t2)) ]
           else [])
          @ [ (pair self' self' >|= fun (t1, t2) -> VTypePair (t1, t2)) ]
        in
        if opts.mrd > 0 then oneof (base_cases @ rec_cases)
        else oneof base_cases)

  let print () = to_source_code

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
