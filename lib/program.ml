open Core
open Utils
open Vtype
open Variant_types
open Ast
open Quotient_types
open Custom_types

type ('tag_e, 'tag_p) program = {
  custom_types : custom_type list;
  e : ('tag_e, 'tag_p) expr;
}
[@@deriving sexp, equal]

type plain_program = (unit, unit) program [@@deriving sexp, equal]

let program_to_source_code ?(use_newlines : bool option)
    (prog : ('tag_e, 'tag_p) program) : string =
  let type_defns_str : string list =
    List.map
      ~f:(function
        | VariantType vt -> variant_type_to_source_code vt
        | QuotientType qt -> quotient_type_to_source_code ?use_newlines qt)
      prog.custom_types
  in
  let e_str : string = ast_to_source_code ?use_newlines prog.e in
  let str_parts : string list = type_defns_str @ [ e_str ] in
  String.concat
    ~sep:
      (if (equal_option equal_bool) use_newlines (Some true) then "\n" else " ")
    str_parts

module QCheck_testing (TagExpr : sig
  type t
end) (TagPat : sig
  type t
end) : sig
  type gen_options = {
    mrd : int;
    max_variant_types : int;
    max_variant_type_constructors : int;
    ast_type : vtype option;
    expr_v_gen : TagExpr.t QCheck.Gen.t;
    pat_v_gen : TagPat.t QCheck.Gen.t;
  }

  type arb_options = {
    gen : gen_options;
    print : Ast.QCheck_testing(TagExpr)(TagPat).ast_print_method;
    shrink : Ast.QCheck_testing(TagExpr)(TagPat).shrink_options;
  }

  include
    QCheck_testing_sig
      with type t = (TagExpr.t, TagPat.t) program
       and type gen_options := gen_options
       and type print_options =
        Ast.QCheck_testing(TagExpr)(TagPat).ast_print_method
       and type shrink_options =
        Ast.QCheck_testing(TagExpr)(TagPat).shrink_options
       and type arb_options := arb_options
end = struct
  type t = (TagExpr.t, TagPat.t) program

  module Ast_qcheck_testing = Ast.QCheck_testing (TagExpr) (TagPat)

  type gen_options = {
    mrd : int;
    max_variant_types : int;
    max_variant_type_constructors : int;
    ast_type : vtype option;
    expr_v_gen : TagExpr.t QCheck.Gen.t;
    pat_v_gen : TagPat.t QCheck.Gen.t;
  }

  type print_options = Ast_qcheck_testing.ast_print_method
  type shrink_options = Ast_qcheck_testing.shrink_options

  type arb_options = {
    gen : gen_options;
    print : print_options;
    shrink : shrink_options;
  }

  type gen_variant_types_list_acc = {
    variant_types : variant_type list;
    variant_type_names : StringSet.t;
    constructor_names : StringSet.t;
  }

  let gen_type_defns_list ~(max_variant_types : int)
      ~(max_variant_type_constructors : int) ~(mrd : int) :
      custom_type list QCheck.Gen.t =
    (* TODO - allow this to generate quotient types too *)
    let open QCheck.Gen in
    int_range 0 max_variant_types >>= fun (n : int) ->
    fix
      (fun self ((n : int), (acc : gen_variant_types_list_acc)) ->
        if n <= 0 then
          return (List.map ~f:(fun vt -> VariantType vt) acc.variant_types)
        else
          Variant_types.QCheck_testing.gen
            {
              used_variant_type_names = acc.variant_type_names;
              used_variant_type_constructor_names = acc.constructor_names;
              max_constructors = max_variant_type_constructors;
              mrd;
            }
          >>= fun ((vt_name, cs) as vt) ->
          self
            ( n - 1,
              {
                variant_types = vt :: acc.variant_types;
                variant_type_names = Set.add acc.variant_type_names vt_name;
                constructor_names =
                  List.fold cs ~init:acc.constructor_names
                    ~f:(fun acc (c_name, _) -> Set.add acc c_name);
              } ))
      ( n,
        {
          variant_types = [];
          variant_type_names = StringSet.empty;
          constructor_names = StringSet.empty;
        } )

  let gen (opts : gen_options) : t QCheck.Gen.t =
    let open QCheck.Gen in
    gen_type_defns_list ~max_variant_types:opts.max_variant_types
      ~max_variant_type_constructors:opts.max_variant_type_constructors
      ~mrd:opts.mrd
    >>= fun custom_types ->
    Ast_qcheck_testing.gen
      {
        t = opts.ast_type;
        variant_types =
          List.filter_map
            ~f:(function VariantType vt -> Some vt | QuotientType _ -> None)
            custom_types;
        v_gen = opts.expr_v_gen;
        pat_v_gen = opts.pat_v_gen;
        mrd = opts.mrd;
      }
    >|= fun e -> { custom_types; e }

  let print (print_method : print_options) : t QCheck.Print.t =
   fun prog ->
    sprintf "[type definitions: %s]\n%s"
      QCheck.Print.(
        list
          (fun qt -> qt |> sexp_of_custom_type |> Sexp.to_string)
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
