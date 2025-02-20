open Core
open Utils
open Vtype
open Varname
open Variant_types
open Expr
open Custom_types

(** A top-level function definition *)
type ('tag_e, 'tag_p) top_level_defn = {
  recursive : bool;
  name : string;
  param : varname * vtype;
  return_t : vtype;
  body : ('tag_e, 'tag_p) expr;
}
[@@deriving sexp, equal]

type plain_top_level_defn = (unit, unit) top_level_defn

let top_level_defn_to_source_code ~(use_newlines : bool) :
    ('tag_e, 'tag_p) top_level_defn -> string =
  let open SourceCodeBuilder in
  let convert (defn : ('tag_e, 'tag_p) top_level_defn) : state -> state =
    write "let "
    |.> (if defn.recursive then write "rec " else nothing)
    |.> write defn.name |.> write " ("
    |.> write (fst defn.param)
    |.> write " : "
    |.> write (vtype_to_source_code (snd defn.param))
    |.> write ") : "
    |.> write (vtype_to_source_code defn.return_t)
    |.> write " ="
    |.> block (write (Expr.to_source_code ~use_newlines defn.body))
    |.> write "end"
  in
  SourceCodeBuilder.from_converter ~converter:convert ~use_newlines

type ('tag_e, 'tag_p) program = {
  custom_types : ('tag_e, 'tag_p) custom_type list;
  top_level_defns : ('tag_e, 'tag_p) top_level_defn list;
  e : ('tag_e, 'tag_p) expr;
}
[@@deriving sexp, equal]

type ('tag_e, 'tag_p) typed_program = (vtype * 'tag_e, vtype * 'tag_p) program
[@@deriving sexp, equal]

type plain_program = (unit, unit) program [@@deriving sexp, equal]

let fmap_expr ~(f : 'tag_e1 -> 'tag_e2) (prog : ('tag_e1, 'tag_p) program) :
    ('tag_e2, 'tag_p) program =
  {
    custom_types = List.map ~f:(Custom_types.fmap_expr ~f) prog.custom_types;
    top_level_defns =
      List.map
        ~f:(fun defn ->
          { defn with body = Expr.fmap ~f defn.body; return_t = defn.return_t })
        prog.top_level_defns;
    e = Expr.fmap ~f prog.e;
  }

let fmap_pattern ~(f : 'tag_p1 -> 'tag_p2) (prog : ('tag_e, 'tag_p1) program) :
    ('tag_e, 'tag_p2) program =
  {
    custom_types = List.map ~f:(Custom_types.fmap_pattern ~f) prog.custom_types;
    top_level_defns =
      List.map
        ~f:(fun defn ->
          {
            defn with
            body = Expr.fmap_pattern ~f defn.body;
            return_t = defn.return_t;
          })
        prog.top_level_defns;
    e = Expr.fmap_pattern ~f prog.e;
  }

let existing_names (prog : ('tag_e, 'tag_p) program) : StringSet.t =
  Set.union
    (List.fold ~init:StringSet.empty
       ~f:(fun acc -> function
         | VariantType vt -> Variant_types.existing_names vt |> Set.union acc
         | QuotientType qt -> QuotientType.existing_names qt |> Set.union acc)
       prog.custom_types)
    (Set.union
       (List.fold ~init:StringSet.empty
          ~f:(fun acc defn ->
            Set.union (Set.add acc defn.name)
              (Set.union
                 (StringSet.singleton (fst defn.param))
                 (Expr.existing_names defn.body)))
          prog.top_level_defns)
       (Expr.existing_names prog.e))

let program_to_source_code ?(use_newlines : bool option)
    (prog : ('tag_e, 'tag_p) program) : string =
  let use_newlines = Option.value ~default:true use_newlines in
  let type_defns_str : string list =
    List.map
      ~f:(function
        | VariantType vt -> variant_type_to_source_code vt
        | QuotientType qt -> QuotientType.to_source_code ~use_newlines qt)
      prog.custom_types
  in
  let top_level_defns_str : string list =
    List.map
      ~f:(top_level_defn_to_source_code ~use_newlines)
      prog.top_level_defns
  in
  let e_str : string = Expr.to_source_code ~use_newlines prog.e in
  let str_parts : string list =
    type_defns_str @ top_level_defns_str @ [ e_str ]
  in
  String.concat ~sep:(if use_newlines then "\n" else " ") str_parts

module QCheck_testing (TagExpr : sig
  type t
end) (TagPat : sig
  type t
end) : sig
  type gen_options = {
    mrd : int;
    max_variant_types : int;
    max_variant_type_constructors : int;
    max_top_level_defns : int;
    allow_fun_types : bool;
    body_type : vtype option;
    expr_v_gen : TagExpr.t QCheck.Gen.t;
    pat_v_gen : TagPat.t QCheck.Gen.t;
  }

  type arb_options = {
    gen : gen_options;
    print : Expr.QCheck_testing(TagExpr)(TagPat).expr_print_method;
    shrink : Expr.QCheck_testing(TagExpr)(TagPat).shrink_options;
  }

  val gen_top_level_defn :
    expr_v_gen:TagExpr.t QCheck.Gen.t ->
    pat_v_gen:TagPat.t QCheck.Gen.t ->
    top_level_defns:(varname * (vtype * vtype)) list ->
    variant_types:variant_type list ->
    mrd:int ->
    (TagExpr.t, TagPat.t) top_level_defn QCheck.Gen.t

  include
    QCheck_testing_sig
      with type t = (TagExpr.t, TagPat.t) program
       and type gen_options := gen_options
       and type print_options =
        Expr.QCheck_testing(TagExpr)(TagPat).expr_print_method
       and type shrink_options =
        Expr.QCheck_testing(TagExpr)(TagPat).shrink_options
       and type arb_options := arb_options
end = struct
  type t = (TagExpr.t, TagPat.t) program

  module Expr_qcheck_testing = Expr.QCheck_testing (TagExpr) (TagPat)

  type gen_options = {
    mrd : int;
    max_variant_types : int;
    max_variant_type_constructors : int;
    max_top_level_defns : int;
    allow_fun_types : bool;
    body_type : vtype option;
    expr_v_gen : TagExpr.t QCheck.Gen.t;
    pat_v_gen : TagPat.t QCheck.Gen.t;
  }

  type print_options = Expr_qcheck_testing.expr_print_method
  type shrink_options = Expr_qcheck_testing.shrink_options

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
      ~(max_variant_type_constructors : int) ~(allow_fun_types : bool)
      ~(mrd : int) : ('tag_e, 'tag_p) custom_type list QCheck.Gen.t =
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
              allow_fun_types;
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

  let gen_top_level_defn ~(expr_v_gen : TagExpr.t QCheck.Gen.t)
      ~(pat_v_gen : TagPat.t QCheck.Gen.t)
      ~(top_level_defns : (varname * (vtype * vtype)) list)
      ~(variant_types : variant_type list) ~(mrd : int) :
      (TagExpr.t, TagPat.t) top_level_defn QCheck.Gen.t =
    let open QCheck.Gen in
    let top_level_names =
      List.map ~f:(fun (name, _) -> name) top_level_defns |> StringSet.of_list
    in
    let variant_type_names =
      List.map ~f:(fun (vt_name, _) -> vt_name) variant_types
      |> StringSet.of_list
    in
    bool >>= fun recursive ->
    QCheck_utils.gen_unique_pair ~equal:equal_string
      (QCheck_utils.filter_gen (Varname.QCheck_testing.gen ()) ~f:(fun name ->
           Set.mem top_level_names name |> not))
    >>= fun (name, param_name) ->
    (* TODO - maybe consider allowing functions to return functions.
    This is only possible when there is a previously-defined function that aids this *)
    let vtype_gen =
      Vtype.QCheck_testing.gen
        { variant_types = variant_type_names; allow_fun_types = false; mrd }
    in
    pair vtype_gen vtype_gen >>= fun (param_t, return_t) ->
    Expr_qcheck_testing.gen
      {
        t = Some (Expr_qcheck_testing.vtype_to_gen_vtype_unsafe return_t);
        variant_types;
        top_level_defns;
        v_gen = expr_v_gen;
        pat_v_gen;
        mrd;
      }
    >|= fun body ->
    { recursive; name; param = (param_name, param_t); return_t; body }

  type gen_top_level_defns_acc = {
    top_level_defns : (TagExpr.t, TagPat.t) top_level_defn list;
    used_names : StringSet.t;
  }

  let gen_top_level_defns_list ~(expr_v_gen : TagExpr.t QCheck.Gen.t)
      ~(pat_v_gen : TagPat.t QCheck.Gen.t) ~(variant_types : variant_type list)
      ~(max_top_level_defns : int) ~(mrd : int) :
      (TagExpr.t, TagPat.t) top_level_defn list QCheck.Gen.t =
    let open QCheck.Gen in
    int_range 0 max_top_level_defns >>= fun (n : int) ->
    fix
      (fun self ((n : int), (acc : gen_top_level_defns_acc)) ->
        if n <= 0 then return acc.top_level_defns
        else
          gen_top_level_defn ~expr_v_gen ~pat_v_gen
            ~top_level_defns:
              (List.map
                 ~f:(fun defn -> (defn.name, (snd defn.param, defn.return_t)))
                 acc.top_level_defns)
            ~variant_types ~mrd
          >>= fun defn ->
          self
            ( n - 1,
              {
                top_level_defns = defn :: acc.top_level_defns;
                used_names = Set.add acc.used_names defn.name;
              } ))
      (n, { top_level_defns = []; used_names = StringSet.empty })

  let gen (opts : gen_options) : t QCheck.Gen.t =
    let open QCheck.Gen in
    gen_type_defns_list ~max_variant_types:opts.max_variant_types
      ~max_variant_type_constructors:opts.max_variant_type_constructors
      ~allow_fun_types:opts.allow_fun_types ~mrd:opts.mrd
    >>= fun custom_types ->
    gen_top_level_defns_list ~expr_v_gen:opts.expr_v_gen
      ~pat_v_gen:opts.pat_v_gen
      ~variant_types:
        (List.filter_map
           ~f:(function VariantType vt -> Some vt | QuotientType _ -> None)
           custom_types)
      ~max_top_level_defns:opts.max_top_level_defns ~mrd:opts.mrd
    >>= fun top_level_defns ->
    Expr_qcheck_testing.gen
      {
        t =
          Option.(
            opts.body_type >>| Expr_qcheck_testing.vtype_to_gen_vtype_unsafe);
        variant_types =
          List.filter_map
            ~f:(function VariantType vt -> Some vt | QuotientType _ -> None)
            custom_types;
        top_level_defns =
          List.map
            ~f:(fun defn -> (defn.name, (snd defn.param, defn.return_t)))
            top_level_defns;
        v_gen = opts.expr_v_gen;
        pat_v_gen = opts.pat_v_gen;
        mrd = opts.mrd;
      }
    >|= fun e -> { custom_types; top_level_defns; e }

  let print (print_method : print_options) : t QCheck.Print.t =
   fun prog ->
    sprintf "[type definitions: %s]\n%s"
      QCheck.Print.(
        let open Expr_qcheck_testing in
        let ct_to_string ct =
          match print_method with
          | NoPrint -> ""
          | PrintSexp (sexp_of_expr_tag, sexp_of_pat_tag) ->
              ct
              |> sexp_of_custom_type sexp_of_expr_tag sexp_of_pat_tag
              |> Sexp.to_string_hum
          | PrintExprSource -> custom_type_name ct
        in
        list (fun ct -> ct |> ct_to_string) prog.custom_types)
      (Expr_qcheck_testing.print print_method prog.e)

  let shrink (opts : shrink_options) : t QCheck.Shrink.t =
    let open QCheck.Iter in
    fun prog ->
      Expr_qcheck_testing.shrink opts prog.e >|= fun e' -> { prog with e = e' }

  let arbitrary (opts : arb_options) : t QCheck.arbitrary =
    QCheck.make ~print:(print opts.print) ~shrink:(shrink opts.shrink)
      (gen opts.gen)
end
