open Core
open Pq_lang
open Utils
open Vtype
open Variant_types
open Pattern
open Ast
open Quotient_types
open Typing
open Parser
open Ast_executor
open Program

let default_max_gen_rec_depth : int = 10
let default_max_variant_type_count : int = 5
let default_max_variant_type_constructor_count : int = 5
let default_max_quotient_type_count : int = 5

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
  | FUN -> Sexp.Atom "FUN"
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
    (List.map ~f:(Fn.compose Sexp.to_string sexp_of_token) tokens)

let override_equal_exec_err (a : exec_err) (b : exec_err) : bool =
  match (a, b) with
  | TypingError _, TypingError _ -> true
  | _ -> equal_exec_err a b

let override_equal_exec_res (a : exec_res) (b : exec_res) : bool =
  match (a, b) with
  | Error e1, Error e2 -> override_equal_exec_err e1 e2
  | _ -> equal_exec_res a b

module TestingTypeCtx : sig
  include Typing.TypingTypeContext

  val add_variant : t -> variant_type -> t
  val add_quotient : t -> quotient_type -> t
  val type_defns_to_list : t -> type_defn list
  val from_list : type_defn list -> t
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

  type t = type_defn list
  type this_t = t

  let empty = []

  let create ~(type_defns : type_defn list) : (t, typing_error) Result.t =
    Ok type_defns

  let find_type_defn_by_name ctx td_name =
    List.find ctx ~f:(fun x_td -> equal_string (type_defn_name x_td) td_name)

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

  let add_variant (ctx : t) (vt : variant_type) : t = VariantType vt :: ctx
  let add_quotient (ctx : t) (qt : quotient_type) : t = QuotientType qt :: ctx
  let type_defns_to_list = Fn.id
  let from_list cts = cts

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
    Fn.compose (sexp_of_list sexp_of_type_defn) type_defns_to_list

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
               (type_ctx : type_defn list) ) ->
          let variant_type_gen : variant_type QCheck.Gen.t =
            Variant_types.QCheck_testing.gen
              {
                mrd = opts.mrd;
                used_variant_type_names =
                  type_ctx |> type_defns_to_list
                  |> List.filter_map ~f:(function
                       | VariantType (vt_name, _) -> Some vt_name
                       | QuotientType _ -> None)
                  |> StringSet.of_list;
                used_variant_type_constructor_names =
                  type_ctx |> type_defns_to_list
                  |> List.filter_map ~f:(function
                       | VariantType (_, cs) -> Some cs
                       | QuotientType _ -> None)
                  |> List.concat_map ~f:(fun cs ->
                         List.map ~f:(fun (c_name, _) -> c_name) cs)
                  |> StringSet.of_list;
                max_constructors = opts.max_constructors;
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
      let print_quotient_type_eqcons : quotient_type_eqcons QCheck.Print.t =
        let open QCheck.Print in
        fun eqcons ->
          sprintf "{bindings=%s; body=%s}"
            (list (pair string vtype_to_source_code) eqcons.bindings)
            (pair pattern_to_source_code
               (ast_to_source_code ~use_newlines:false)
               eqcons.body)
      in
      let print_quotient_type : quotient_type QCheck.Print.t =
       fun qt ->
        sprintf "{name=%s; base_type_name=%s; eqconss=%s}" qt.name
          qt.base_type_name
          ((QCheck.Print.list print_quotient_type_eqcons) qt.eqconss)
      in
      let print_type_defn : type_defn QCheck.Print.t = function
        | VariantType vt -> sprintf "VariantType(%s)" (print_variant_type vt)
        | QuotientType qt -> sprintf "QuotientType(%s)" (print_quotient_type qt)
      in
      QCheck.Print.(Fn.compose (list print_type_defn) type_defns_to_list)

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
                  TestingTypeCtx.type_defns_to_list type_ctx
                  |> List.filter_map ~f:(function
                       | VariantType (vt_name, _) -> Some vt_name
                       | QuotientType _ -> None)
                  |> StringSet.of_list;
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
  type t = unit
end

module Unit_ast_qcheck_testing = Ast.QCheck_testing (UnitTag)
module Unit_program_qcheck_testing = Program.QCheck_testing (UnitTag)

let unit_program_arbitrary_with_default_options =
  Unit_program_qcheck_testing.arbitrary
    {
      gen =
        {
          mrd = default_max_gen_rec_depth;
          max_variant_types = default_max_variant_type_count;
          max_variant_type_constructors =
            default_max_variant_type_constructor_count;
          ast_type = None;
          v_gen = QCheck.Gen.unit;
        };
      print = Unit_ast_qcheck_testing.PrintExprSource;
      shrink = { preserve_type = false };
    }
