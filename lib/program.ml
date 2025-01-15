open Core
open Utils
open Vtype
open Custom_types
open Ast
open Quotient_types

type type_defn = CustomType of custom_type | QuotientType of quotient_type
[@@deriving sexp, equal]

let type_defn_name : type_defn -> string = function
  | CustomType (ct_name, _) -> ct_name
  | QuotientType qt -> qt.name

type 'a program = { type_defns : type_defn list; e : 'a expr }
[@@deriving sexp, equal]

type plain_program = unit program [@@deriving sexp, equal]

let program_to_source_code ?(use_newlines : bool option) (prog : 'a program) :
    string =
  let type_defns_str : string list =
    List.map
      ~f:(function
        | CustomType ct -> custom_type_to_source_code ct
        | QuotientType qt -> quotient_type_to_source_code ?use_newlines qt)
      prog.type_defns
  in
  let e_str : string = ast_to_source_code ?use_newlines prog.e in
  let str_parts : string list = type_defns_str @ [ e_str ] in
  String.concat
    ~sep:
      (if (equal_option equal_bool) use_newlines (Some true) then "\n" else " ")
    str_parts

module QCheck_testing (Tag : sig
  type t
end) : sig
  type gen_options = {
    mrd : int;
    max_custom_types : int;
    max_custom_type_constructors : int;
    ast_type : vtype option;
    v_gen : Tag.t QCheck.Gen.t;
  }

  type arb_options = {
    gen : gen_options;
    print : Ast.QCheck_testing(Tag).ast_print_method;
    shrink : Ast.QCheck_testing(Tag).shrink_options;
  }

  include
    QCheck_testing_sig
      with type t = Tag.t program
       and type gen_options := gen_options
       and type print_options = Ast.QCheck_testing(Tag).ast_print_method
       and type shrink_options = Ast.QCheck_testing(Tag).shrink_options
       and type arb_options := arb_options
end = struct
  type t = Tag.t program

  module Ast_qcheck_testing = Ast.QCheck_testing (Tag)

  type gen_options = {
    mrd : int;
    max_custom_types : int;
    max_custom_type_constructors : int;
    ast_type : vtype option;
    v_gen : Tag.t QCheck.Gen.t;
  }

  type print_options = Ast_qcheck_testing.ast_print_method
  type shrink_options = Ast_qcheck_testing.shrink_options

  type arb_options = {
    gen : gen_options;
    print : print_options;
    shrink : shrink_options;
  }

  type gen_custom_types_list_acc = {
    custom_types : custom_type list;
    custom_type_names : StringSet.t;
    constructor_names : StringSet.t;
  }

  let gen_type_defns_list ~(max_custom_types : int)
      ~(max_custom_type_constructors : int) ~(mrd : int) :
      type_defn list QCheck.Gen.t =
    (* TODO - allow this to generate quotient types too *)
    let open QCheck.Gen in
    int_range 0 max_custom_types >>= fun (n : int) ->
    fix
      (fun self ((n : int), (acc : gen_custom_types_list_acc)) ->
        if n <= 0 then
          return (List.map ~f:(fun ct -> CustomType ct) acc.custom_types)
        else
          Custom_types.QCheck_testing.gen
            {
              used_custom_type_names = acc.custom_type_names;
              used_custom_type_constructor_names = acc.constructor_names;
              max_constructors = max_custom_type_constructors;
              mrd;
            }
          >>= fun ((ct_name, cs) as ct) ->
          self
            ( n - 1,
              {
                custom_types = ct :: acc.custom_types;
                custom_type_names = Set.add acc.custom_type_names ct_name;
                constructor_names =
                  List.fold cs ~init:acc.constructor_names
                    ~f:(fun acc (c_name, _) -> Set.add acc c_name);
              } ))
      ( n,
        {
          custom_types = [];
          custom_type_names = StringSet.empty;
          constructor_names = StringSet.empty;
        } )

  let gen (opts : gen_options) : t QCheck.Gen.t =
    let open QCheck.Gen in
    gen_type_defns_list ~max_custom_types:opts.max_custom_types
      ~max_custom_type_constructors:opts.max_custom_type_constructors
      ~mrd:opts.mrd
    >>= fun type_defns ->
    Ast_qcheck_testing.gen
      {
        t = opts.ast_type;
        custom_types =
          List.filter_map
            ~f:(function CustomType ct -> Some ct | QuotientType _ -> None)
            type_defns;
        v_gen = opts.v_gen;
        mrd = opts.mrd;
      }
    >|= fun e -> { type_defns; e }

  let print (print_method : print_options) : t QCheck.Print.t =
   fun prog ->
    sprintf "[type definitions: %s]\n%s"
      QCheck.Print.(
        list
          (fun qt -> qt |> sexp_of_type_defn |> Sexp.to_string)
          prog.type_defns)
      (Ast_qcheck_testing.print print_method prog.e)

  let shrink (opts : shrink_options) : t QCheck.Shrink.t =
    let open QCheck.Iter in
    fun prog ->
      Ast_qcheck_testing.shrink opts prog.e >|= fun e' -> { prog with e = e' }

  let arbitrary (opts : arb_options) : t QCheck.arbitrary =
    QCheck.make ~print:(print opts.print) ~shrink:(shrink opts.shrink)
      (gen opts.gen)
end
