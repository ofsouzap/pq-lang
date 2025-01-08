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
end = struct
  type t = custom_type list

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
end

let testing_type_ctx_gen ~(max_custom_types : int) ~(max_constructors : int)
    ~(mrd : int) : TestingTypeCtx.t QCheck.Gen.t =
  let open QCheck.Gen in
  int_range 0 max_custom_types >>= fun custom_type_count ->
  fix
    (fun self (n, type_ctx) ->
      if n <= 0 then return type_ctx
      else
        Custom_types.QCheck_testing.gen
          {
            mrd;
            used_custom_type_names =
              type_ctx |> TestingTypeCtx.customs_to_list |> List.map ~f:fst
              |> StringSet.of_list;
            used_custom_type_constructor_names =
              type_ctx |> TestingTypeCtx.customs_to_list
              |> List.concat_map ~f:(Core.Fn.compose (List.map ~f:fst) snd)
              |> StringSet.of_list;
            max_constructors;
          }
        >>= fun new_ct -> self (n - 1, TestingTypeCtx.add_custom type_ctx new_ct))
    (custom_type_count, TestingTypeCtx.empty)

let testing_type_ctx_arb ~(max_custom_types : int) ~(max_constructors : int)
    ~(mrd : int) : TestingTypeCtx.t QCheck.arbitrary =
  let print_custom_type_constructor : custom_type_constructor QCheck.Print.t =
    QCheck.Print.(pair string vtype_to_source_code)
  in
  let print_custom_type : custom_type QCheck.Print.t =
    QCheck.Print.(pair Fn.id (list print_custom_type_constructor))
  in
  QCheck.make
    ~print:
      QCheck.Print.(
        Fn.compose (list print_custom_type) TestingTypeCtx.customs_to_list)
    (testing_type_ctx_gen ~max_custom_types ~max_constructors ~mrd)

let default_testing_type_ctx_gen =
  testing_type_ctx_gen ~max_custom_types:default_max_custom_type_count
    ~max_constructors:default_max_custom_type_constructor_count
    ~mrd:default_max_gen_rec_depth

let default_testing_type_ctx_arb =
  testing_type_ctx_arb ~max_custom_types:default_max_custom_type_count
    ~max_constructors:default_max_custom_type_constructor_count
    ~mrd:default_max_gen_rec_depth

module TestingVarCtx : sig
  include Typing.TypingVarContext

  val varnames_of_type : vtype -> t -> string list
  val to_list : t -> (string * vtype) list
  val from_list : (string * vtype) list -> t
end = struct
  type t = (string * vtype) list

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
end

let testing_var_ctx_arb ~(type_ctx : TestingTypeCtx.t) :
    TestingVarCtx.t QCheck.arbitrary =
  let gen =
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
    >|= TestingVarCtx.from_list
  in
  QCheck.make
    ~print:
      (Core.Fn.compose
         QCheck.Print.(list (pair string vtype_to_source_code))
         TestingVarCtx.to_list)
    gen

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
