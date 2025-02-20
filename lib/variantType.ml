open Core
open Utils
open Utils.QCheck_utils
open Vtype

type constructor = string * vtype [@@deriving sexp, equal]

let constructor_existing_names ((c_name, _) : constructor) : StringSet.t =
  StringSet.singleton c_name

let constructor_to_source_code ((c_name, c_type) : constructor) : string =
  sprintf "%s of %s" c_name (vtype_to_source_code c_type)

let constructors_to_source_code (cs : constructor list) : string =
  List.map ~f:constructor_to_source_code cs |> String.concat ~sep:" | "

module QCheck_testing_constructors : sig
  type gen_options = {
    used_variant_type_names : StringSet.t;
    used_variant_type_constructor_names : StringSet.t;
    allow_fun_types : bool;
    mrd : int;
  }

  include
    QCheck_testing_sig
      with type t = constructor
       and type gen_options := gen_options
       and type print_options = unit
       and type shrink_options = unit
       and type arb_options := gen_options
end = struct
  type t = constructor

  type gen_options = {
    used_variant_type_names : StringSet.t;
    used_variant_type_constructor_names : StringSet.t;
    allow_fun_types : bool;
    mrd : int;
  }

  type print_options = unit
  type shrink_options = unit
  type arb_options = gen_options

  let variant_type_constructor_name_gen : string QCheck.Gen.t =
    let open QCheck.Gen in
    filter_gen ~max_attempts:10000
      ~f:(fun name -> not (List.mem lexer_keywords name ~equal:String.equal))
      ( int_range 0 5 >>= fun n ->
        pair (char_range 'A' 'Z') (list_repeat n (char_range 'a' 'z'))
        >|= fun (h, ts) -> String.of_char h ^ String.of_char_list ts )

  let gen (opts : gen_options) : constructor QCheck.Gen.t =
    (* Filtered so that the constructors don't exist already in the type context *)
    let open QCheck.Gen in
    filter_gen ~max_attempts:10000
      (pair variant_type_constructor_name_gen
         (Vtype.QCheck_testing.gen
            {
              variant_types = opts.used_variant_type_names;
              allow_fun_types = opts.allow_fun_types;
              mrd = opts.mrd;
            }))
      ~f:(fun (c_name, _) ->
        not (Set.mem opts.used_variant_type_constructor_names c_name))

  let print () : t QCheck.Print.t = constructor_to_source_code

  let shrink () : t QCheck.Shrink.t =
    QCheck.Shrink.(pair nil (Vtype.QCheck_testing.shrink ()))

  let arbitrary (opts : arb_options) : t QCheck.arbitrary =
    QCheck.make ~print:(print ()) ~shrink:(shrink ()) (gen opts)
end

type t = string * constructor list [@@deriving sexp, equal]

let existing_names ((vt_name, vt_cs) : t) : StringSet.t =
  Set.union
    (StringSet.singleton vt_name)
    (List.fold ~init:StringSet.empty
       ~f:(fun acc c -> Set.union (constructor_existing_names c) acc)
       vt_cs)

let to_source_code ((vt_name, vt_cs) : t) : string =
  sprintf "type %s = %s" vt_name (constructors_to_source_code vt_cs)

type variant_type = t

module QCheck_testing : sig
  type gen_options = {
    used_variant_type_names : StringSet.t;
    used_variant_type_constructor_names : StringSet.t;
    allow_fun_types : bool;
    max_constructors : int;
    mrd : int;
  }

  include
    QCheck_testing_sig
      with type t = variant_type
       and type gen_options := gen_options
       and type print_options = unit
       and type shrink_options = unit
       and type arb_options := gen_options
end = struct
  type t = variant_type

  type gen_options = {
    used_variant_type_names : StringSet.t;
    used_variant_type_constructor_names : StringSet.t;
    allow_fun_types : bool;
    max_constructors : int;
    mrd : int;
  }

  type print_options = unit
  type shrink_options = unit
  type arb_options = gen_options

  let variant_type_name_gen : string QCheck.Gen.t =
    Varname.QCheck_testing.gen ()

  let gen (opts : gen_options) : t QCheck.Gen.t =
    let open QCheck.Gen in
    let used_constructors_with_acc ~(acc : constructor list) =
      Set.union opts.used_variant_type_constructor_names
        (acc |> List.map ~f:(fun (c_name, _) -> c_name) |> StringSet.of_list)
    in
    let gen_constructors =
      (* Iteratively build up list of constructors, making sure to not have multiply-defined constructors in the same variant type *)
      (* TODO - include the possibility of recursive data types *)
      int_range 1 opts.max_constructors >>= fun constructor_count ->
      fix
        (fun self ((n : int), (acc : constructor list)) ->
          if n <= 0 then return acc
          else
            QCheck_testing_constructors.gen
              {
                used_variant_type_names = opts.used_variant_type_names;
                used_variant_type_constructor_names =
                  used_constructors_with_acc ~acc;
                allow_fun_types = opts.allow_fun_types;
                mrd = opts.mrd;
              }
            >>= fun c -> self (n - 1, c :: acc))
        (constructor_count, [])
    in
    pair
      ((* Filter so that the variant type name doesn't already exist *)
       filter_gen ~max_attempts:10000 variant_type_name_gen ~f:(fun vt_name ->
           not (Set.mem opts.used_variant_type_names vt_name)))
      gen_constructors

  let print () : t QCheck.Print.t = to_source_code

  let shrink () =
    QCheck.Shrink.(
      pair nil (list ~shrink:(QCheck_testing_constructors.shrink ())))

  let arbitrary (opts : arb_options) =
    QCheck.make ~print:(print ()) ~shrink:(shrink ()) (gen opts)
end
