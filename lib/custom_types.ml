open Core
open Utils
open Utils.QCheck_utils
open Vtype

type custom_type_constructor = string * vtype [@@deriving sexp, equal]

let custom_type_constructor_to_source_code
    ((c_name, c_type) : custom_type_constructor) : string =
  sprintf "%s of %s" c_name (vtype_to_source_code c_type)

let custom_type_constructors_to_source_code (cs : custom_type_constructor list)
    : string =
  List.map ~f:custom_type_constructor_to_source_code cs
  |> String.concat ~sep:" | "

module QCheck_testing_constructors : sig
  type gen_options = {
    used_custom_type_names : StringSet.t;
    used_custom_type_constructor_names : StringSet.t;
    mrd : int;
  }

  include
    QCheck_testing_sig
      with type t = custom_type_constructor
       and type gen_options := gen_options
       and type print_options = unit
       and type shrink_options = unit
       and type arb_options := gen_options
end = struct
  type t = custom_type_constructor

  type gen_options = {
    used_custom_type_names : StringSet.t;
    used_custom_type_constructor_names : StringSet.t;
    mrd : int;
  }

  type print_options = unit
  type shrink_options = unit
  type arb_options = gen_options

  let custom_type_constructor_name_gen : string QCheck.Gen.t =
    let open QCheck.Gen in
    fix
      (fun self _ ->
        int_range 0 5 >>= fun n ->
        ( pair (char_range 'A' 'Z') (list_repeat n (char_range 'a' 'z'))
        >|= fun (h, ts) -> String.of_char h ^ String.of_char_list ts )
        >>= fun s ->
        if List.mem lexer_keywords s ~equal:String.equal then self ()
        else return s)
      ()

  let gen (opts : gen_options) : custom_type_constructor QCheck.Gen.t =
    (* Filtered so that the constructors don't exist already in the type context *)
    let open QCheck.Gen in
    filter_gen ~max_attempts:10000
      (pair custom_type_constructor_name_gen
         (Vtype.QCheck_utils.gen
            { custom_types = opts.used_custom_type_names; mrd = opts.mrd }))
      ~f:(fun (c_name, _) ->
        not (Set.mem opts.used_custom_type_constructor_names c_name))

  let print _ = failwith "TODO"
  let shrink _ = failwith "TODO"
  let arbitrary _ = failwith "TODO"
end

type custom_type = string * custom_type_constructor list
[@@deriving sexp, equal]

let custom_type_to_source_code ((ct_name, ct_cs) : custom_type) : string =
  sprintf "type %s = %s" ct_name (custom_type_constructors_to_source_code ct_cs)

module QCheck_testing : sig
  type gen_options = {
    used_custom_type_names : StringSet.t;
    used_custom_type_constructor_names : StringSet.t;
    max_constructors : int;
    mrd : int;
  }

  include
    QCheck_testing_sig
      with type t = custom_type
       and type gen_options := gen_options
       and type print_options = unit
       and type shrink_options = unit
       and type arb_options := gen_options
end = struct
  type t = custom_type

  type gen_options = {
    used_custom_type_names : StringSet.t;
    used_custom_type_constructor_names : StringSet.t;
    max_constructors : int;
    mrd : int;
  }

  type print_options = unit
  type shrink_options = unit
  type arb_options = gen_options

  let custom_type_name_gen : string QCheck.Gen.t = Varname.QCheck_testing.gen ()

  let gen (opts : gen_options) : t QCheck.Gen.t =
    let open QCheck.Gen in
    let gen_constructors =
      (* Iteratively build up list of constructors, making sure to not have multiply-defined constructors in the same custom type *)
      (* TODO - include the possibility of recursive data types *)
      int_range 1 opts.max_constructors >>= fun constructor_count ->
      fix
        (fun self ((n : int), (acc : custom_type_constructor list)) ->
          if n <= 0 then return acc
          else
            QCheck_testing_constructors.gen
              {
                used_custom_type_names = opts.used_custom_type_names;
                used_custom_type_constructor_names =
                  List.map acc ~f:(fun (c_name, _) -> c_name)
                  |> StringSet.of_list;
                mrd = opts.mrd;
              }
            >>= fun c -> self (n - 1, c :: acc))
        (constructor_count, [])
    in
    pair
      ((* Filter so that the custom type name doesn't already exist *)
       filter_gen ~max_attempts:10000 custom_type_name_gen ~f:(fun ct_name ->
           not (Set.mem opts.used_custom_type_names ct_name)))
      gen_constructors

  let print (_ : print_options) : t QCheck.Print.t = failwith "TODO"
  let shrink () = QCheck.Shrink.nil
  let arbitrary (_ : arb_options) = failwith "TODO"
end
