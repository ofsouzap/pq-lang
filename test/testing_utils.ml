open Core
open Pq_lang
open Utils
open Vtype
open Variant_types
open Pattern
open Ast
open Quotient_types
open Custom_types
open Typing
open Parser
open Ast_executor

let default_max_gen_rec_depth : int = 10
let default_max_variant_type_count : int = 5
let default_max_variant_type_constructor_count : int = 5
let default_max_quotient_type_count : int = 5
let default_max_top_level_defns_count : int = 5

let sexp_of_token = function
  | END -> Sexp.Atom "END"
  | IF -> Sexp.Atom "IF"
  | THEN -> Sexp.Atom "THEN"
  | ELSE -> Sexp.Atom "ELSE"
  | LET -> Sexp.Atom "LET"
  | REC -> Sexp.Atom "REC"
  | IN -> Sexp.Atom "IN"
  | TRUE -> Sexp.Atom "TRUE"
  | FALSE -> Sexp.Atom "FALSE"
  | UNIT -> Sexp.Atom "UNIT"
  | INT -> Sexp.Atom "INT"
  | BOOL -> Sexp.Atom "BOOL"
  | MATCH -> Sexp.Atom "MATCH"
  | WITH -> Sexp.Atom "WITH"
  | TYPE -> Sexp.Atom "TYPE"
  | QTYPE -> Sexp.Atom "QTYPE"
  | OF -> Sexp.Atom "OF"
  | PLUS -> Sexp.Atom "PLUS"
  | MINUS -> Sexp.Atom "MINUS"
  | STAR -> Sexp.Atom "STAR"
  | LPAREN -> Sexp.Atom "LPAREN"
  | RPAREN -> Sexp.Atom "RPAREN"
  | BNOT -> Sexp.Atom "BNOT"
  | BOR -> Sexp.Atom "BOR"
  | BAND -> Sexp.Atom "BAND"
  | ASSIGN -> Sexp.Atom "ASSIGN"
  | EQUATE -> Sexp.Atom "EQUATE"
  | GT -> Sexp.Atom "GT"
  | GTEQ -> Sexp.Atom "GTEQ"
  | LT -> Sexp.Atom "LT"
  | LTEQ -> Sexp.Atom "LTEQ"
  | ARROW -> Sexp.Atom "ARROW"
  | BIG_ARROW -> Sexp.Atom "BIG_ARROW"
  | COLON -> Sexp.Atom "COLON"
  | COMMA -> Sexp.Atom "COMMA"
  | PIPE -> Sexp.Atom "PIPE"
  | QUOTIENT -> Sexp.Atom "QUOTIENT"
  | UNIT_VAL -> Sexp.Atom "UNIT_VAL"
  | INTLIT i -> Sexp.List [ Sexp.Atom "INTLIT"; Sexp.Atom (string_of_int i) ]
  | LNAME n -> Sexp.List [ Sexp.Atom "LNAME"; Sexp.Atom n ]
  | UNAME n -> Sexp.List [ Sexp.Atom "UNAME"; Sexp.Atom n ]
  | EOF -> Sexp.Atom "EOF"

let token_printer tokens =
  String.concat ~sep:", "
    (List.map ~f:(Fn.compose Sexp.to_string_hum sexp_of_token) tokens)

let override_equal_exec_err (a : exec_err) (b : exec_err) : bool =
  match (a, b) with
  | TypingError _, TypingError _ -> true
  | _ -> equal_exec_err a b

let override_equal_exec_res (a : exec_res) (b : exec_res) : bool =
  match (a, b) with
  | Error e1, Error e2 -> override_equal_exec_err e1 e2
  | _ -> equal_exec_res a b

let override_equal_typing_error (a : Typing.typing_error)
    (b : Typing.typing_error) : bool =
  match (a, b) with
  | TypeMismatch (ta1, ta2, _), TypeMismatch (tb1, tb2, _) ->
      equal_vtype ta1 tb1 && equal_vtype ta2 tb2
  | _ -> Typing.equal_typing_error a b

module TestingTypeCtx : sig
  include Typing.TypingTypeContext

  val add_variant : t -> variant_type -> t
  val add_quotient : t -> ('tag_e, 'tag_p) quotient_type -> t
  val from_list : ('tag_e, 'tag_p) custom_type list -> t
  val variant_gen_opt : t -> variant_type QCheck.Gen.t option
  val sexp_of_t : t -> Sexp.t

  module QCheck_testing : sig
    type gen_options = {
      max_variant_types : int;
      max_constructors : int;
      max_quotient_types : int;
      mrd : int;
    }

    include
      QCheck_testing_sig
        with type t = t
         and type gen_options := gen_options
         and type print_options = unit
         and type shrink_options = unit
         and type arb_options := gen_options
  end
end = struct
  open Typing

  type t = plain_custom_type list
  type this_t = t

  let empty = []

  let create ~(custom_types : ('tag_e, 'tag_p) custom_type list) :
      (t, typing_error) Result.t =
    Ok (List.map ~f:to_plain_custom_type custom_types)

  let find_type_defn_by_name ctx td_name =
    List.find ctx ~f:(fun x_td -> equal_string (custom_type_name x_td) td_name)

  let type_defn_exists ctx vt_name =
    Option.is_some (find_type_defn_by_name ctx vt_name)

  let find_variant_type_with_constructor (ctx : t) (c_name : string) :
      (variant_type * variant_type_constructor) option =
    let open Option in
    List.find_map ctx ~f:(function
      | VariantType ((_, cs) as vt) ->
          List.find_map cs ~f:(fun ((x_c_name, _) as c) ->
              if equal_string c_name x_c_name then Some c else None)
          >>| fun c -> (vt, c)
      | QuotientType _ -> None)

  let rec is_quotient_descendant (ctx : t) (t1 : vtype) (t2 : vtype) :
      (bool, typing_error) Result.t =
    let open Result in
    match (t1, t2) with
    | VTypeCustom ct1_name, VTypeCustom ct2_name -> (
        find_type_defn_by_name ctx ct1_name
        |> Result.of_option ~error:(UndefinedTypeName ct1_name)
        >>= fun ct1 ->
        find_type_defn_by_name ctx ct2_name
        |> Result.of_option ~error:(UndefinedTypeName ct2_name)
        >>= fun ct2 ->
        match (ct1, ct2) with
        | VariantType _, QuotientType _ -> Ok false
        | VariantType vt1, VariantType vt2 -> Ok (equal_variant_type vt1 vt2)
        | QuotientType qt1, VariantType vt2 ->
            is_quotient_descendant ctx (VTypeCustom qt1.base_type_name)
              (VTypeCustom (fst vt2))
        | QuotientType qt1, QuotientType qt2 ->
            if equal_quotient_type equal_unit equal_unit qt1 qt2 then Ok true
            else
              is_quotient_descendant ctx (VTypeCustom qt1.base_type_name)
                (VTypeCustom qt2.name))
    | VTypePair (t11, t12), VTypePair (t21, t22) ->
        is_quotient_descendant ctx t11 t21 >>= fun first ->
        is_quotient_descendant ctx t12 t22 >>| fun second -> first && second
    | VTypeFun (t11, t12), VTypeFun (t21, t22) ->
        is_quotient_descendant ctx t11 t21 >>= fun first ->
        (* Note that this part is the other way round to usual *)
        is_quotient_descendant ctx t22 t12 >>| fun second -> first && second
    | _ -> Ok (equal_vtype t1 t2)

  let rec find_common_root_type (ctx : t) (t1 : vtype) (t2 : vtype) :
      (vtype option, typing_error) Result.t =
    let open Result in
    match (t1, t2) with
    | VTypeCustom ct1_name, VTypeCustom ct2_name -> (
        find_type_defn_by_name ctx ct1_name
        |> Result.of_option ~error:(UndefinedTypeName ct1_name)
        >>= fun ct1 ->
        find_type_defn_by_name ctx ct2_name
        |> Result.of_option ~error:(UndefinedTypeName ct2_name)
        >>= fun ct2 ->
        match (ct1, ct2) with
        | VariantType vt1, VariantType vt2 ->
            if equal_variant_type vt1 vt2 then Ok (Some t1) else Ok None
        | QuotientType qt1, QuotientType qt2 ->
            if equal_quotient_type equal_unit equal_unit qt1 qt2 then
              Ok (Some t1)
            else
              find_common_root_type ctx (VTypeCustom qt1.base_type_name)
                (VTypeCustom qt2.name)
        | VariantType vt, QuotientType qt | QuotientType qt, VariantType vt ->
            find_common_root_type ctx (VTypeCustom qt.base_type_name)
              (VTypeCustom (fst vt)))
    | VTypePair (t11, t12), VTypePair (t21, t22) ->
        find_common_root_type ctx t11 t21 >>= fun t1 ->
        find_common_root_type ctx t12 t22 >>= fun t2 ->
        Ok
          Option.(
            t1 >>= fun t1 ->
            t2 >>| fun t2 -> VTypePair (t1, t2))
    | ( VTypeFun _,
        VTypeFun _
        (* TODO - for now, I won't try figure out how to implement this for function types. this is for another time *)
      )
    | _ ->
        if equal_vtype t1 t2 then Ok (Some t1) else Ok None

  let add_variant (ctx : t) (vt : variant_type) : t = VariantType vt :: ctx

  let add_quotient (ctx : t) (qt : ('tag_e, 'tag_p) quotient_type) : t =
    QuotientType (to_plain_quotient_type qt) :: ctx

  let type_defns_to_ordered_list = Fn.id
  let from_list = List.map ~f:to_plain_custom_type

  let variant_gen_opt (ctx : t) : variant_type QCheck.Gen.t option =
    let open QCheck.Gen in
    let choices =
      List.filter_map
        ~f:(function
          | VariantType vt -> Some (return vt) | QuotientType _ -> None)
        ctx
    in
    match choices with [] -> None | _ :: _ -> Some (oneof choices)

  let sexp_of_t : t -> Sexp.t =
    Fn.compose
      (sexp_of_list sexp_of_plain_custom_type)
      type_defns_to_ordered_list

  module QCheck_testing = struct
    type t = this_t

    type gen_options = {
      max_variant_types : int;
      max_constructors : int;
      max_quotient_types : int;
      mrd : int;
    }

    type print_options = unit
    type shrink_options = unit
    type arb_options = gen_options

    let gen (opts : gen_options) : t QCheck.Gen.t =
      let open QCheck.Gen in
      int_range 0 opts.max_variant_types >>= fun variant_type_count ->
      int_range 0 opts.max_quotient_types >>= fun quotient_type_count ->
      fix
        (fun self
             ( (rem_variant_types, rem_quotient_types),
               (type_ctx : plain_custom_type list) ) ->
          let variant_type_gen : variant_type QCheck.Gen.t =
            Variant_types.QCheck_testing.gen
              {
                mrd = opts.mrd;
                used_variant_type_names =
                  type_ctx |> type_defns_to_ordered_list
                  |> List.filter_map ~f:(function
                       | VariantType (vt_name, _) -> Some vt_name
                       | QuotientType _ -> None)
                  |> StringSet.of_list;
                used_variant_type_constructor_names =
                  type_ctx |> type_defns_to_ordered_list
                  |> List.filter_map ~f:(function
                       | VariantType (_, cs) -> Some cs
                       | QuotientType _ -> None)
                  |> List.concat_map ~f:(fun cs ->
                         List.map ~f:(fun (c_name, _) -> c_name) cs)
                  |> StringSet.of_list;
                max_constructors = opts.max_constructors;
                allow_fun_types =
                  (* TODO - until function-typed expression generation works better *)
                  false;
              }
          in
          let choices : this_t QCheck.Gen.t list =
            Option.to_list
              (if rem_variant_types >= 0 then
                 Some
                   ( variant_type_gen >>= fun vt ->
                     self
                       ( (rem_variant_types - 1, rem_quotient_types),
                         VariantType vt :: type_ctx ) )
               else None)
            @ Option.to_list
                (if rem_quotient_types >= 0 then None
                   (* TODO - generator for quotient types *)
                 else None)
          in
          match choices with [] -> return type_ctx | _ :: _ -> oneof choices)
        ((variant_type_count, quotient_type_count), empty)

    let print () : t QCheck.Print.t =
      let print_variant_type_constructor :
          variant_type_constructor QCheck.Print.t =
        QCheck.Print.(pair string vtype_to_source_code)
      in
      let print_variant_type : variant_type QCheck.Print.t =
        QCheck.Print.(pair Fn.id (list print_variant_type_constructor))
      in
      let print_quotient_type_eqcons : plain_quotient_type_eqcons QCheck.Print.t
          =
        let open QCheck.Print in
        fun eqcons ->
          sprintf "{bindings=%s; body=%s}"
            (list (pair string vtype_to_source_code) eqcons.bindings)
            (pair pattern_to_source_code
               (ast_to_source_code ~use_newlines:false)
               eqcons.body)
      in
      let print_quotient_type : plain_quotient_type QCheck.Print.t =
       fun qt ->
        sprintf "{name=%s; base_type_name=%s; eqconss=%s}" qt.name
          qt.base_type_name
          ((QCheck.Print.list print_quotient_type_eqcons) qt.eqconss)
      in
      let print_type_defn : plain_custom_type QCheck.Print.t = function
        | VariantType vt -> sprintf "VariantType(%s)" (print_variant_type vt)
        | QuotientType qt -> sprintf "QuotientType(%s)" (print_quotient_type qt)
      in
      QCheck.Print.(
        Fn.compose (list print_type_defn) type_defns_to_ordered_list)

    let shrink () : t QCheck.Shrink.t =
      let open QCheck.Iter in
      QCheck.Shrink.(
        list ~shrink:(function
          | VariantType vt ->
              Variant_types.QCheck_testing.shrink () vt >|= fun vt ->
              VariantType vt
          | QuotientType qt -> nil qt >|= fun qt -> QuotientType qt))

    let arbitrary (opts : arb_options) : t QCheck.arbitrary =
      QCheck.make ~print:(print ()) ~shrink:(shrink ()) (gen opts)
  end
end

let default_testing_type_ctx_gen =
  TestingTypeCtx.QCheck_testing.gen
    {
      max_variant_types = default_max_variant_type_count;
      max_constructors = default_max_variant_type_constructor_count;
      max_quotient_types = default_max_quotient_type_count;
      mrd = default_max_gen_rec_depth;
    }

let default_testing_type_ctx_arb =
  TestingTypeCtx.QCheck_testing.arbitrary
    {
      max_variant_types = default_max_variant_type_count;
      max_constructors = default_max_variant_type_constructor_count;
      max_quotient_types = default_max_quotient_type_count;
      mrd = default_max_gen_rec_depth;
    }

module TestingVarCtx : sig
  include Typing.TypingVarContext

  val varnames_of_type : vtype -> t -> string list
  val to_list : t -> (string * vtype) list
  val from_list : (string * vtype) list -> t

  module QCheck_testing : sig
    include
      QCheck_testing_sig
        with type t = t
         and type gen_options = TestingTypeCtx.t
         and type print_options = unit
         and type shrink_options = unit
         and type arb_options = TestingTypeCtx.t
  end
end = struct
  type t = (Varname.varname * vtype) list
  type this_t = t

  let empty = []
  let add ctx x t = List.Assoc.add ctx x t ~equal:String.equal
  let find ctx x = List.Assoc.find ctx x ~equal:String.equal
  let singleton x t = add empty x t

  let append ctx1 =
    List.fold ~init:ctx1 ~f:(fun ctx_acc (x, t) -> add ctx_acc x t)

  let exists ctx x = match find ctx x with None -> false | Some _ -> true

  let varnames_of_type (t : vtype) (ctx : t) : string list =
    List.filter_map
      ~f:(fun (vname, vtype) ->
        if equal_vtype vtype t then Some vname else None)
      ctx

  let to_list = Fn.id
  let from_list = List.fold ~init:empty ~f:(fun acc (x, t) -> add acc x t)

  module QCheck_testing = struct
    type t = this_t
    type gen_options = TestingTypeCtx.t
    type print_options = unit
    type shrink_options = unit
    type arb_options = TestingTypeCtx.t

    let gen (type_ctx : TestingTypeCtx.t) : t QCheck.Gen.t =
      let open QCheck.Gen in
      list
        (pair
           (Varname.QCheck_testing.gen ())
           (Vtype.QCheck_testing.gen
              {
                mrd = default_max_gen_rec_depth;
                variant_types =
                  TestingTypeCtx.type_defns_to_ordered_list type_ctx
                  |> List.filter_map ~f:(function
                       | VariantType (vt_name, _) -> Some vt_name
                       | QuotientType _ -> None)
                  |> StringSet.of_list;
                allow_fun_types =
                  (* TODO - until funtion-typed expression generation works better *)
                  false;
              }))
      >|= List.fold ~init:empty ~f:(fun acc (x, t) -> add acc x t)

    let print () : t QCheck.Print.t =
      Core.Fn.compose
        QCheck.Print.(list (pair string vtype_to_source_code))
        to_list

    let shrink () : t QCheck.Shrink.t =
      QCheck.Shrink.(
        list
          ~shrink:
            (pair
               (Varname.QCheck_testing.shrink ())
               (Vtype.QCheck_testing.shrink ())))

    let arbitrary (type_ctx : TestingTypeCtx.t) =
      QCheck.make ~print:(print ()) ~shrink:(shrink ()) (gen type_ctx)
  end
end

module TestingTypeChecker = TypeChecker (TestingTypeCtx) (TestingVarCtx)

module UnitTag = struct
  type t = unit [@@deriving sexp, equal]

  let sexp_of_t = sexp_of_unit
  let t_of_sexp = unit_of_sexp
  let equal = equal_unit
end

module Unit_ast_qcheck_testing = Ast.QCheck_testing (UnitTag) (UnitTag)
module Unit_program_qcheck_testing = Program.QCheck_testing (UnitTag) (UnitTag)

let unit_program_arbitrary_with_default_options =
  Unit_program_qcheck_testing.arbitrary
    {
      gen =
        {
          mrd = default_max_gen_rec_depth;
          max_variant_types = default_max_variant_type_count;
          max_variant_type_constructors =
            default_max_variant_type_constructor_count;
          max_top_level_defns = default_max_top_level_defns_count;
          allow_fun_types =
            (* TODO - until function-typed expression works better *) false;
          ast_type = None;
          expr_v_gen = QCheck.Gen.unit;
          pat_v_gen = QCheck.Gen.unit;
        };
      print = Unit_ast_qcheck_testing.PrintExprSource;
      shrink = { preserve_type = false };
    }
