open Core
open Pq_lang
open Utils
open Parser
module ProgramExecutor = ProgramExecutor.SimpleExecutor
module Program = ProgramExecutor.Program
module TypeChecker = ProgramExecutor.TypeChecker
module CustomType = Program.CustomType
module QuotientType = CustomType.QuotientType
module Expr = Program.Expr
module Pattern = Program.Pattern

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
  | PRIVATE -> Sexp.Atom "PRIVATE"
  | INTLIT i -> Sexp.List [ Sexp.Atom "INTLIT"; Sexp.Atom (string_of_int i) ]
  | LNAME n -> Sexp.List [ Sexp.Atom "LNAME"; Sexp.Atom n ]
  | UNAME n -> Sexp.List [ Sexp.Atom "UNAME"; Sexp.Atom n ]
  | EOF -> Sexp.Atom "EOF"

let token_printer tokens =
  String.concat ~sep:", "
    (List.map ~f:(Fn.compose Sexp.to_string_hum sexp_of_token) tokens)

let pp_from_sexp sexp_of_t ppf x =
  Fmt.string ppf (Sexp.to_string_hum (sexp_of_t x))

let label_tests (label : string) =
  List.map ~f:(fun ((name, speed, f) : 'a Alcotest.test_case) ->
      (sprintf "%s-%s" label name, speed, f))

let override_equal_exec_err (a : ProgramExecutor.exec_err)
    (b : ProgramExecutor.exec_err) : bool =
  match (a, b) with
  | TypingError _, TypingError _ -> true
  | _ -> ProgramExecutor.equal_exec_err a b

let override_equal_exec_res (a : ProgramExecutor.exec_res)
    (b : ProgramExecutor.exec_res) : bool =
  match (a, b) with
  | Error e1, Error e2 -> override_equal_exec_err e1 e2
  | _ -> ProgramExecutor.equal_exec_res a b

let override_equal_typing_error (a : TypeChecker.TypingError.t)
    (b : TypeChecker.TypingError.t) : bool =
  match (a, b) with
  | TypeMismatch (ta1, ta2, _), TypeMismatch (tb1, tb2, _) ->
      Vtype.equal ta1 tb1 && Vtype.equal ta2 tb2
  | _ -> TypeChecker.TypingError.equal a b

let vtype_testable : Vtype.t Alcotest.testable =
  Alcotest.testable (Fmt.of_to_string Vtype.to_source_code) Vtype.equal

let std_expr_testable
    (print :
      [ `PrintSexp of ('tag_e -> Sexp.t) * ('tag_p -> Sexp.t) | `PrintSource ])
    (eq_tag_e : 'tag_e -> 'tag_e -> bool) (eq_tag_p : 'tag_p -> 'tag_p -> bool)
    : ('tag_e, 'tag_p) Expr.t Alcotest.testable =
  let pp =
    match print with
    | `PrintSexp (sexp_of_tag_e, sexp_of_tag_p) ->
        pp_from_sexp (Expr.sexp_of_t sexp_of_tag_e sexp_of_tag_p)
    | `PrintSource -> Fmt.of_to_string Expr.to_source_code
  in
  Alcotest.testable pp (Expr.equal eq_tag_e eq_tag_p)

let plain_std_expr_testable (print_method : [ `PrintSexp | `PrintSource ]) =
  std_expr_testable
    (match print_method with
    | `PrintSexp -> `PrintSexp (sexp_of_unit, sexp_of_unit)
    | `PrintSource -> `PrintSource)
    equal_unit equal_unit

let std_program_testable (eq_tag_e : 'tag_e -> 'tag_e -> 'bool)
    (eq_tag_p : 'tag_p -> 'tag_p -> bool) :
    ('tag_e, 'tag_p) Program.t Alcotest.testable =
  let pp = Fmt.of_to_string (Program.to_source_code ~use_newlines:true) in
  Alcotest.testable pp (Program.equal eq_tag_e eq_tag_p)

let std_executor_store_value_testable :
    ProgramExecutor.Store.value Alcotest.testable =
  Alcotest.testable
    (pp_from_sexp ProgramExecutor.Store.sexp_of_value)
    ProgramExecutor.Store.equal_value

let std_executor_error_testable : ProgramExecutor.exec_err Alcotest.testable =
  Alcotest.testable
    (pp_from_sexp ProgramExecutor.sexp_of_exec_err)
    override_equal_exec_err

let std_executor_res_testable : ProgramExecutor.exec_res Alcotest.testable =
  Alcotest.testable
    (Fmt.of_to_string ProgramExecutor.show_exec_res)
    override_equal_exec_res

let std_typing_error_testable : TypeChecker.TypingError.t Alcotest.testable =
  Alcotest.testable
    (Fmt.of_to_string TypeChecker.TypingError.print)
    override_equal_typing_error

(** Implementation of a type context useful for tests *)
module TestingTypeCtx : sig
  include
    Pq_lang.TypeChecker.TypeContext.S
      with module CustomType = CustomType
       and module TypingError = TypeChecker.TypingError

  (** Add a variant type to the type context *)
  val add_variant : t -> VariantType.t -> t

  (** Add a quotient type to the type context *)
  val add_quotient : t -> ('tag_e, 'tag_p) QuotientType.t -> t

  (** Creates a type from a list *)
  val from_list : ('tag_e, 'tag_p) CustomType.t list -> t

  (** If there are any defined variant types, get a generator for a random one
      of them *)
  val variant_gen_opt : t -> VariantType.t QCheck.Gen.t option

  (** Get the type context as a sexp *)
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
  module CustomType = CustomType
  module TypingError = TypeChecker.TypingError

  type t = CustomType.plain_t list
  type this_t = t

  let empty = []

  let create ~(custom_types : ('tag_e, 'tag_p) CustomType.t list) :
      (t, TypeChecker.TypingError.t) Result.t =
    Ok (List.map ~f:CustomType.to_plain_t custom_types)

  let find_type_defn_by_name ctx td_name =
    List.find ctx ~f:(fun x_td -> equal_string (CustomType.name x_td) td_name)

  let type_defn_exists ctx vt_name =
    Option.is_some (find_type_defn_by_name ctx vt_name)

  let find_variant_type_with_constructor (ctx : t) (c_name : string) :
      (VariantType.t * VariantType.constructor) option =
    let open Option in
    List.find_map ctx ~f:(function
      | VariantType ((_, cs) as vt) ->
          List.find_map cs ~f:(fun ((x_c_name, _) as c) ->
              if equal_string c_name x_c_name then Some c else None)
          >>| fun c -> (vt, c)
      | CustomType.QuotientType _ -> None)

  let rec subtype (ctx : t) (t1 : Vtype.t) (t2 : Vtype.t) :
      (bool, TypeChecker.TypingError.t) Result.t =
    let open Result in
    match (t1, t2) with
    | VTypeInt, VTypeInt | VTypeBool, VTypeBool | VTypeUnit, VTypeUnit ->
        Ok true
    | VTypeInt, _ | VTypeBool, _ | VTypeUnit, _ -> Ok false
    | VTypePair (t1a, t1b), VTypePair (t2a, t2b) ->
        subtype ctx t1a t2a >>= fun t1a_subtype ->
        subtype ctx t1b t2b >>= fun t1b_subtype ->
        Ok (t1a_subtype && t1b_subtype)
    | VTypePair _, _ -> Ok false
    | VTypeFun (t1a, t1b), VTypeFun (t2a, t2b) ->
        subtype ctx t2a t1a >>= fun t2a_subtype ->
        subtype ctx t1b t2b >>= fun t1b_subtype ->
        Ok (t2a_subtype && t1b_subtype)
    | VTypeFun _, _ -> Ok false
    | VTypeCustom c1_name, VTypeCustom c2_name -> (
        find_type_defn_by_name ctx c1_name
        |> Result.of_option
             ~error:(TypeChecker.TypingError.UndefinedTypeName c1_name)
        >>= fun ct1 ->
        find_type_defn_by_name ctx c2_name
        |> Result.of_option
             ~error:(TypeChecker.TypingError.UndefinedTypeName c2_name)
        >>= fun ct2 ->
        match (ct1, ct2) with
        | VariantType (vt1_name, _), VariantType (vt2_name, _) ->
            Ok (equal_string vt1_name vt2_name)
        | VariantType _, QuotientType qt2 ->
            subtype ctx t1 (VTypeCustom qt2.base_type_name)
        | CustomType.QuotientType qt1, QuotientType qt2 ->
            if equal_string qt1.name qt2.name then Ok true
            else subtype ctx t1 (VTypeCustom qt2.base_type_name)
        | CustomType.QuotientType _, VariantType _ -> Ok false)
    | VTypeCustom _, _ -> Ok false

  let add_variant (ctx : t) (vt : VariantType.t) : t = VariantType vt :: ctx

  let add_quotient (ctx : t) (qt : ('tag_e, 'tag_p) QuotientType.t) : t =
    QuotientType (QuotientType.to_plain_t qt) :: ctx

  let type_defns_to_ordered_list = Fn.id
  let from_list = List.map ~f:CustomType.to_plain_t

  let variant_gen_opt (ctx : t) : VariantType.t QCheck.Gen.t option =
    let open QCheck.Gen in
    let choices =
      List.filter_map
        ~f:(function
          | VariantType vt -> Some (return vt)
          | CustomType.QuotientType _ -> None)
        ctx
    in
    match choices with [] -> None | _ :: _ -> Some (oneof choices)

  let sexp_of_t : t -> Sexp.t =
    Fn.compose
      (sexp_of_list CustomType.sexp_of_plain_t)
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
               (type_ctx : CustomType.plain_t list) ) ->
          let variant_type_gen : VariantType.t QCheck.Gen.t =
            VariantType.QCheck_testing.gen
              {
                mrd = opts.mrd;
                used_variant_type_names =
                  type_ctx |> type_defns_to_ordered_list
                  |> List.filter_map ~f:(function
                       | CustomType.VariantType (vt_name, _) -> Some vt_name
                       | CustomType.QuotientType _ -> None)
                  |> StringSet.of_list;
                used_variant_type_constructor_names =
                  type_ctx |> type_defns_to_ordered_list
                  |> List.filter_map ~f:(function
                       | CustomType.VariantType (_, cs) -> Some cs
                       | CustomType.QuotientType _ -> None)
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
          VariantType.constructor QCheck.Print.t =
        QCheck.Print.(pair string Vtype.to_source_code)
      in
      let print_variant_type : VariantType.t QCheck.Print.t =
        QCheck.Print.(pair Fn.id (list print_variant_type_constructor))
      in
      let print_quotient_type_eqcons : QuotientType.plain_eqcons QCheck.Print.t
          =
        let open QCheck.Print in
        fun eqcons ->
          sprintf "{bindings=%s; body=%s}"
            (list (pair string Vtype.to_source_code) eqcons.bindings)
            (pair Pattern.to_source_code
               (Expr.to_source_code ~use_newlines:false)
               eqcons.body)
      in
      let print_quotient_type : QuotientType.plain_t QCheck.Print.t =
       fun qt ->
        sprintf "{name=%s; base_type_name=%s; eqconss=%s}" qt.name
          qt.base_type_name
          ((QCheck.Print.list print_quotient_type_eqcons) qt.eqconss)
      in
      let print_type_defn : CustomType.plain_t QCheck.Print.t = function
        | VariantType vt -> sprintf "VariantType(%s)" (print_variant_type vt)
        | CustomType.QuotientType qt ->
            sprintf "QuotientType(%s)" (print_quotient_type qt)
      in
      QCheck.Print.(
        Fn.compose (list print_type_defn) type_defns_to_ordered_list)

    let shrink () : t QCheck.Shrink.t =
      let open QCheck.Iter in
      QCheck.Shrink.(
        list ~shrink:(function
          | CustomType.VariantType vt ->
              VariantType.QCheck_testing.shrink () vt >|= fun vt ->
              CustomType.VariantType vt
          | CustomType.QuotientType qt ->
              nil qt >|= fun qt -> CustomType.QuotientType qt))

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
  include Pq_lang.TypeChecker.VarContext.S

  val varnames_of_type : Vtype.t -> t -> string list
  val to_list : t -> (string * Vtype.t) list
  val from_list : (string * Vtype.t) list -> t

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
  type t = (Varname.t * Vtype.t) list
  type this_t = t

  let empty = []
  let add ctx x t = List.Assoc.add ctx x t ~equal:String.equal
  let find ctx x = List.Assoc.find ctx x ~equal:String.equal
  let singleton x t = add empty x t

  let append ctx1 =
    List.fold ~init:ctx1 ~f:(fun ctx_acc (x, t) -> add ctx_acc x t)

  let exists ctx x = match find ctx x with None -> false | Some _ -> true

  let varnames_of_type (t : Vtype.t) (ctx : t) : string list =
    List.filter_map
      ~f:(fun (vname, vtype) ->
        if Vtype.equal vtype t then Some vname else None)
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
                       | CustomType.VariantType (vt_name, _) -> Some vt_name
                       | CustomType.QuotientType _ -> None)
                  |> StringSet.of_list;
                allow_fun_types =
                  (* TODO - until funtion-typed expression generation works better *)
                  false;
              }))
      >|= List.fold ~init:empty ~f:(fun acc (x, t) -> add acc x t)

    let print () : t QCheck.Print.t =
      Core.Fn.compose
        QCheck.Print.(list (pair string Vtype.to_source_code))
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

module TestingTypeChecker :
  Pq_lang.TypeChecker.S
    with module Pattern = Pq_lang.Pattern.StdPattern
     and module Expr = Pq_lang.Expr.StdExpr
     and module Program = Pq_lang.Program.StdProgram
     and module TypingError = Pq_lang.TypeChecker.TypingError.StdTypingError =
  Pq_lang.TypeChecker.MakeStd (TestingTypeCtx) (TestingVarCtx)

module UnitTag = struct
  type t = unit [@@deriving sexp, equal]

  let sexp_of_t = sexp_of_unit
  let t_of_sexp = unit_of_sexp
  let equal = equal_unit
end

module Unit_expr_qcheck_testing = Expr.QCheck_testing (UnitTag) (UnitTag)
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
          body_type = None;
          expr_v_gen = QCheck.Gen.unit;
          pat_v_gen = QCheck.Gen.unit;
        };
      print = Unit_expr_qcheck_testing.PrintExprSource;
      shrink = { preserve_type = false };
    }
