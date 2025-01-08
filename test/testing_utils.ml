open Core
open Pq_lang
open Utils
open Vtype
open Custom_types
open Typing
open Parser
open Ast_executor

let default_max_gen_rec_depth : int = 10
let default_max_custom_type_count : int = 5
let default_max_custom_type_constructor_count : int = 5

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
  | COLON -> Sexp.Atom "COLON"
  | COMMA -> Sexp.Atom "COMMA"
  | PIPE -> Sexp.Atom "PIPE"
  | UNIT_VAL -> Sexp.Atom "UNIT_VAL"
  | INTLIT i -> Sexp.List [ Sexp.Atom "INTLIT"; Sexp.Atom (string_of_int i) ]
  | LNAME n -> Sexp.List [ Sexp.Atom "LNAME"; Sexp.Atom n ]
  | UNAME n -> Sexp.List [ Sexp.Atom "UNAME"; Sexp.Atom n ]
  | EOF -> Sexp.Atom "EOF"

let token_printer tokens =
  String.concat ~sep:", "
    (List.map ~f:(Fn.compose Sexp.to_string sexp_of_token) tokens)

let override_equal_exec_res (a : exec_res) (b : exec_res) : bool =
  match (a, b) with
  | Error e1, Error e2 -> (
      match (e1, e2) with
      | TypingError _, TypingError _ -> true
      | _ -> equal_exec_res a b)
  | _ -> equal_exec_res a b

module TestingTypeCtx : sig
  include Typing.TypingTypeContext

  val add_custom : t -> custom_type -> t
  val customs_to_list : t -> custom_type list
  val from_list : custom_type list -> t
  val custom_gen_opt : t -> custom_type QCheck.Gen.t option
  val sexp_of_t : t -> Sexp.t

  module QCheck_testing : sig
    type gen_options = {
      max_custom_types : int;
      max_constructors : int;
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
  type t = custom_type list
  type this_t = t

  let empty = []
  let create ~(custom_types : custom_type list) : t = custom_types

  let find_custom ctx ct_name =
    List.find ctx ~f:(fun (x_ct_name, _) -> equal_string x_ct_name ct_name)

  let custom_exists ctx ct_name = Option.is_some (find_custom ctx ct_name)

  let find_custom_with_constructor (ctx : t) (c_name : string) :
      (custom_type * custom_type_constructor) option =
    let open Option in
    List.find_map ctx ~f:(fun ((_, cs) as ct) ->
        List.find_map cs ~f:(fun ((x_c_name, _) as c) ->
            if equal_string c_name x_c_name then Some c else None)
        >>| fun c -> (ct, c))

  let add_custom (ctx : t) (ct : custom_type) : t = ct :: ctx
  let customs_to_list = Fn.id
  let from_list cts = cts

  let custom_gen_opt (ctx : t) : custom_type QCheck.Gen.t option =
    if List.is_empty ctx then None
    else
      let open QCheck.Gen in
      Some (oneof (List.map ~f:return ctx))

  let sexp_of_t : t -> Sexp.t =
    Fn.compose (sexp_of_list sexp_of_custom_type) customs_to_list

  module QCheck_testing = struct
    type t = this_t

    type gen_options = {
      max_custom_types : int;
      max_constructors : int;
      mrd : int;
    }

    type print_options = unit
    type shrink_options = unit
    type arb_options = gen_options

    let gen (opts : gen_options) : t QCheck.Gen.t =
      let open QCheck.Gen in
      int_range 0 opts.max_custom_types >>= fun custom_type_count ->
      fix
        (fun self (n, type_ctx) ->
          if n <= 0 then return type_ctx
          else
            Custom_types.QCheck_testing.gen
              {
                mrd = opts.mrd;
                used_custom_type_names =
                  type_ctx |> customs_to_list |> List.map ~f:fst
                  |> StringSet.of_list;
                used_custom_type_constructor_names =
                  type_ctx |> customs_to_list
                  |> List.concat_map ~f:(fun (_, cs) ->
                         List.map ~f:(fun (c_name, _) -> c_name) cs)
                  |> StringSet.of_list;
                max_constructors = opts.max_constructors;
              }
            >>= fun new_ct -> self (n - 1, add_custom type_ctx new_ct))
        (custom_type_count, empty)

    let print () =
      let print_custom_type_constructor : custom_type_constructor QCheck.Print.t
          =
        QCheck.Print.(pair string vtype_to_source_code)
      in
      let print_custom_type : custom_type QCheck.Print.t =
        QCheck.Print.(pair Fn.id (list print_custom_type_constructor))
      in
      QCheck.Print.(Fn.compose (list print_custom_type) customs_to_list)

    let shrink () =
      QCheck.Shrink.(list ~shrink:(Custom_types.QCheck_testing.shrink ()))

    let arbitrary (opts : arb_options) : t QCheck.arbitrary =
      QCheck.make ~print:(print ()) ~shrink:(shrink ()) (gen opts)
  end
end

let default_testing_type_ctx_gen =
  TestingTypeCtx.QCheck_testing.gen
    {
      max_custom_types = default_max_custom_type_count;
      max_constructors = default_max_custom_type_constructor_count;
      mrd = default_max_gen_rec_depth;
    }

let default_testing_type_ctx_arb =
  TestingTypeCtx.QCheck_testing.arbitrary
    {
      max_custom_types = default_max_custom_type_count;
      max_constructors = default_max_custom_type_constructor_count;
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
                custom_types =
                  TestingTypeCtx.customs_to_list type_ctx
                  |> List.map ~f:fst |> StringSet.of_list;
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
          max_custom_types = default_max_custom_type_count;
          max_custom_type_constructors =
            default_max_custom_type_constructor_count;
          ast_type = None;
          v_gen = QCheck.Gen.unit;
        };
      print = Unit_ast_qcheck_testing.PrintExprSource;
      shrink = { preserve_type = false };
    }
