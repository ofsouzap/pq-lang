open Core
open Utils
open Vtype
open Custom_types
open Ast

type 'a program = { custom_types : custom_type list; e : 'a expr }
[@@deriving sexp, equal]

type plain_program = unit program [@@deriving sexp, equal]

let program_to_source_code (prog : 'a program) : string =
  let custom_types_strs : string list =
    List.map ~f:custom_type_to_source_code prog.custom_types
  in
  let e_str : string = ast_to_source_code prog.e in
  let str_parts : string list = custom_types_strs @ [ e_str ] in
  String.concat ~sep:"\n" str_parts

module QCheck_testing (Tag : sig
  type t
end) : sig
  type gen_options = {
    max_custom_types : int;
    max_custom_type_constructors : int;
    ast_gen_options : Ast.QCheck_testing(Tag).gen_options;
    v_gen : Tag.t QCheck.Gen.t;
  }

  include
    QCheck_testing_sig
      with type t = Tag.t program
       and type gen_options := gen_options
end = struct
  type t = Tag.t program

  module Ast_qcheck_testing = Ast.QCheck_testing (Tag)

  type gen_options = {
    max_custom_types : int;
    max_custom_type_constructors : int;
    ast_gen_options : Ast_qcheck_testing.gen_options;
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

  let gen_custom_types_list ~(max_custom_types : int)
      ~(max_custom_type_constructors : int) ~(mrd : int) :
      custom_type list QCheck.Gen.t =
    let open QCheck.Gen in
    int_range 0 max_custom_types >>= fun (n : int) ->
    fix
      (fun self ((acc : gen_custom_types_list_acc), (n : int)) ->
        if n <= 0 then return acc.custom_types
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
            ( {
                custom_types = ct :: acc.custom_types;
                custom_type_names = Set.add acc.custom_type_names ct_name;
                constructor_names =
                  List.fold cs ~init:acc.constructor_names
                    ~f:(fun acc (c_name, _) -> Set.add acc c_name);
              },
              n - 1 ))
      ( {
          custom_types = [];
          custom_type_names = StringSet.empty;
          constructor_names = StringSet.empty;
        },
        n )

  let gen (opts : gen_options) : t QCheck.Gen.t =
    let open QCheck.Gen in
    gen_custom_types_list ~max_custom_types:opts.max_custom_types
      ~max_custom_type_constructors:opts.max_custom_type_constructors
      ~mrd:opts.ast_gen_options.mrd
    >>= fun custom_types ->
    Ast_qcheck_testing.gen opts.ast_gen_options >|= fun e -> { custom_types; e }

  let print (print_method : print_options) : t QCheck.Print.t =
   fun prog ->
    sprintf "[custom types: %s]\n%s"
      QCheck.Print.(
        list
          (fun ct -> ct |> sexp_of_custom_type |> Sexp.to_string)
          prog.custom_types)
      (Ast_qcheck_testing.print print_method prog.e)

  let shrink (opts : shrink_options) : t QCheck.Shrink.t =
    let open QCheck.Iter in
    fun prog ->
      Ast_qcheck_testing.shrink opts prog.e >|= fun e' -> { prog with e = e' }

  let arbitrary (opts : arb_options) : t QCheck.arbitrary =
    QCheck.make ~print:(print opts.print) ~shrink:(shrink opts.shrink)
      (gen opts.gen)
end
