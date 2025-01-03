open Core
open Pq_lang
open Utils
open Vtype
open Custom_types
open Pattern
open Ast
open Typing
open Parser
open Ast_executor

let nonempty_list_arb (v_arb : 'a QCheck.arbitrary) :
    'a Nonempty_list.t QCheck.arbitrary =
  QCheck.map
    ~rev:(fun xs -> Nonempty_list.(head xs, tail xs))
    (fun (h, ts) -> Nonempty_list.make (h, ts))
    QCheck.(pair v_arb (list v_arb))

let result_arb (x_arb : 'a QCheck.arbitrary) (y_arb : 'b QCheck.arbitrary) :
    ('a, 'b) Result.t QCheck.arbitrary =
  QCheck.map
    (fun (b, x, y) -> if b then Ok x else Error y)
    (QCheck.triple QCheck.bool x_arb y_arb)

let filter_gen ?(max_attempts : int option) (x_gen : 'a QCheck.Gen.t)
    ~(f : 'a -> bool) : 'a QCheck.Gen.t =
  let open QCheck.Gen in
  fix
    (fun self n_opt ->
      let _ =
        (* Termination check *)
        match n_opt with
        | None -> ()
        | Some n -> if n < 0 then failwith "Filter ran out of attempts"
      in
      x_gen >>= fun x ->
      if f x then return x else self (Option.map n_opt ~f:(fun n -> n - 1)))
    max_attempts

type 'a ast_print_method =
  | NoPrint
  | PrintSexp of ('a -> Sexp.t)
  | PrintExprSource

let get_ast_printer_opt : 'a ast_print_method -> ('a expr -> string) option =
  function
  | NoPrint -> None
  | PrintSexp f -> Some (fun e -> Ast.sexp_of_expr f e |> Sexp.to_string)
  | PrintExprSource -> Some (ast_to_source_code ~use_newlines:true)

let get_ast_printer (p : 'a ast_print_method) (e : 'a expr) : string =
  match get_ast_printer_opt p with None -> "" | Some f -> f e

(* TODO - change these values back to larger ones *)
let default_ast_print_method : 'a ast_print_method = PrintExprSource
let max_gen_rec_depth : int = 3
let default_max_gen_rec_depth : int = max_gen_rec_depth
let default_max_custom_type_count : int = 2
let default_max_custom_type_constructor_count : int = 2

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

let rec expr_shrink ~(preserve_type : bool) : 'a expr QCheck.Shrink.t =
  let open QCheck.Iter in
  let unop_shrink (v, e1) (recomb : 'a * 'a expr -> 'a expr) :
      'a expr QCheck.Iter.t =
    expr_shrink ~preserve_type e1 >|= fun e1' -> recomb (v, e1')
  in
  let binop_shrink ?(allow_return_subexpr : bool option) (v, e1, e2)
      (recomb : 'a * 'a expr * 'a expr -> 'a expr) : 'a expr QCheck.Iter.t =
    let allow_return_subexpr =
      Option.value ~default:true allow_return_subexpr
    in
    (if allow_return_subexpr then return e1 <+> return e2 else empty)
    <+> (expr_shrink ~preserve_type e1 >|= fun e1' -> recomb (v, e1', e2))
    <+> (expr_shrink ~preserve_type e2 >|= fun e2' -> recomb (v, e1, e2'))
  in
  function
  | UnitLit _ -> empty
  | IntLit _ -> empty
  | Add (v, e1, e2) ->
      binop_shrink (v, e1, e2) (fun (v', e1', e2') -> Add (v', e1', e2'))
  | Neg (v, e1) -> unop_shrink (v, e1) (fun (v', e1') -> Neg (v', e1'))
  | Subtr (v, e1, e2) ->
      binop_shrink (v, e1, e2) (fun (v', e1', e2') -> Subtr (v', e1', e2'))
  | Mult (v, e1, e2) ->
      binop_shrink (v, e1, e2) (fun (v', e1', e2') -> Mult (v', e1', e2'))
  | BoolLit _ -> empty
  | BNot (v, e1) -> unop_shrink (v, e1) (fun (v', e1') -> BNot (v', e1'))
  | BOr (v, e1, e2) ->
      binop_shrink (v, e1, e2) (fun (v', e1', e2') -> BOr (v', e1', e2'))
  | BAnd (v, e1, e2) ->
      binop_shrink (v, e1, e2) (fun (v', e1', e2') -> BAnd (v', e1', e2'))
  | Pair (v, e1, e2) ->
      binop_shrink (v, e1, e2) (fun (v', e1', e2') -> Pair (v', e1', e2'))
  | Eq (v, e1, e2) ->
      binop_shrink ~allow_return_subexpr:(not preserve_type) (v, e1, e2)
        (fun (v', e1', e2') -> Eq (v', e1', e2'))
  | Gt (v, e1, e2) ->
      binop_shrink ~allow_return_subexpr:(not preserve_type) (v, e1, e2)
        (fun (v', e1', e2') -> Gt (v', e1', e2'))
  | GtEq (v, e1, e2) ->
      binop_shrink ~allow_return_subexpr:(not preserve_type) (v, e1, e2)
        (fun (v', e1', e2') -> GtEq (v', e1', e2'))
  | Lt (v, e1, e2) ->
      binop_shrink ~allow_return_subexpr:(not preserve_type) (v, e1, e2)
        (fun (v', e1', e2') -> Lt (v', e1', e2'))
  | LtEq (v, e1, e2) ->
      binop_shrink ~allow_return_subexpr:(not preserve_type) (v, e1, e2)
        (fun (v', e1', e2') -> LtEq (v', e1', e2'))
  | If (v, e1, e2, e3) ->
      (if not preserve_type then return e1 <+> return e2 <+> return e3
       else empty)
      <+> (expr_shrink ~preserve_type e1 >|= fun e1' -> If (v, e1', e2, e3))
      <+> (expr_shrink ~preserve_type e2 >|= fun e2' -> If (v, e1, e2', e3))
      <+> (expr_shrink ~preserve_type e3 >|= fun e3' -> If (v, e1, e2, e3'))
  | Var _ -> empty
  | Let _ ->
      (* Because of let-rec definitions needing a specific form when using AST to source code,
         this would need a filtered shrink function which I can't be bothered to write at the moment *)
      empty
  | Fun (v, (x, t), e1) ->
      expr_shrink ~preserve_type e1 >|= fun e1' -> Fun (v, (x, t), e1')
  | App (v, e1, e2) ->
      binop_shrink ~allow_return_subexpr:(not preserve_type) (v, e1, e2)
        (fun (v', e1', e2') -> App (v', e1', e2'))
  | Fix _ ->
      (* I won't try have these shrunk, since it's more complicated than most, since let-rec definitions need a specific form *)
      empty
  | Match (v, e1, ps) ->
      (if preserve_type then empty else return e1)
      <+> ( (* Try shrinking each case expression *)
            QCheck.Shrink.list_elems
              (fun (c_p, c_e) ->
                expr_shrink ~preserve_type c_e >|= fun c_e' -> (c_p, c_e'))
              (Nonempty_list.to_list ps)
          >|= fun ps' -> Match (v, e1, Nonempty_list.from_list_unsafe ps') )
  | Constructor (v, c, e1) ->
      unop_shrink (v, e1) (fun (v', e1') -> Constructor (v', c, e1'))

let override_equal_exec_res (a : exec_res) (b : exec_res) : bool =
  match (a, b) with
  | Error e1, Error e2 -> (
      match (e1, e2) with
      | TypingError _, TypingError _ -> true
      | _ -> equal_exec_res a b)
  | _ -> equal_exec_res a b

let lexer_keywords : string list =
  [
    "end";
    "if";
    "then";
    "else";
    "let";
    "rec";
    "in";
    "true";
    "false";
    "fun";
    "unit";
    "int";
    "bool";
    "match";
    "with";
    "type";
    "of";
  ]

let varname_gen : string QCheck.Gen.t =
  let open QCheck.Gen in
  fix
    (fun self _ ->
      int_range 1 5 >>= fun n ->
      list_repeat n (char_range 'a' 'z') >|= String.of_char_list >>= fun s ->
      if List.mem lexer_keywords s ~equal:String.equal then self ()
      else return s)
    ()

let custom_type_name_gen : string QCheck.Gen.t = varname_gen

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

let gen_unique_pair ~(equal : 'a -> 'a -> bool) (g : 'a QCheck.Gen.t) :
    ('a * 'a) QCheck.Gen.t =
  let open QCheck.Gen in
  fix
    (fun self _ ->
      g >>= fun x ->
      g >>= fun y -> if equal x y then self () else return (x, y))
    ()

module TestingTypeCtx : sig
  include Typing.TypingTypeContext

  val add_custom : t -> custom_type -> t
  val to_list : t -> custom_type list
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
  let to_list = Fn.id
  let from_list cts = cts

  let custom_gen_opt (ctx : t) : custom_type QCheck.Gen.t option =
    if List.is_empty ctx then None
    else
      let open QCheck.Gen in
      Some (oneof (List.map ~f:return ctx))

  let sexp_of_t : t -> Sexp.t =
    Fn.compose (sexp_of_list sexp_of_custom_type) to_list
end

let vtype_gen ~(type_ctx : TestingTypeCtx.t) (d : int) : vtype QCheck.Gen.t =
  let open QCheck.Gen in
  let gen =
    fix (fun self d ->
        let self' = self (d - 1) in
        let base_cases =
          [ return VTypeUnit; return VTypeInt; return VTypeBool ]
          @ Option.(
              to_list
                ( TestingTypeCtx.custom_gen_opt type_ctx >>| fun custom_gen ->
                  custom_gen >|= fun (ct_name, _) -> VTypeCustom ct_name ))
        in
        let rec_cases =
          [
            (pair self' self' >|= fun (t1, t2) -> VTypeFun (t1, t2));
            (pair self' self' >|= fun (t1, t2) -> VTypePair (t1, t2));
          ]
        in
        if d > 0 then oneof (base_cases @ rec_cases) else oneof base_cases)
  in
  gen d

let vtype_arb ~(type_ctx : TestingTypeCtx.t) (d : int) : vtype QCheck.arbitrary
    =
  QCheck.make ~print:vtype_to_source_code (vtype_gen ~type_ctx d)

let typed_var_gen ~(type_ctx : TestingTypeCtx.t) (d : int) :
    (string * vtype) QCheck.Gen.t =
  let open QCheck.Gen in
  pair varname_gen (vtype_gen ~type_ctx d)

let testing_type_ctx_gen ~(max_custom_types : int) ~(max_constructors : int)
    ~(mrd : int) : TestingTypeCtx.t QCheck.Gen.t =
  let open QCheck.Gen in
  let custom_type_gen ~(type_ctx : TestingTypeCtx.t) ~(max_constructors : int)
      ~(mrd : int) =
    let gen_constructor (used_constructor_names : string list) :
        custom_type_constructor QCheck.Gen.t =
      (* Filtered so that the constructors don't exist already in the type context *)
      filter_gen ~max_attempts:10000
        (pair custom_type_constructor_name_gen (vtype_gen ~type_ctx mrd))
        ~f:(fun (c_name, _) ->
          not
            (List.exists (TestingTypeCtx.to_list type_ctx) ~f:(fun (_, cs) ->
                 List.exists cs ~f:(fun (x_c_name, _) ->
                     equal_string x_c_name c_name))
            || List.mem used_constructor_names c_name ~equal:equal_string))
    in
    let gen_constructors =
      (* Iteratively build up list of constructors, making sure to not have multiply-defined constructors in the same custom type *)
      int_range 1 max_constructors >>= fun constructor_count ->
      fix
        (fun self (n, acc) ->
          if n <= 0 then return acc
          else
            gen_constructor (List.map acc ~f:(fun (c_name, _) -> c_name))
            >>= fun c -> self (n - 1, c :: acc))
        (constructor_count, [])
    in
    pair
      ((* Filter so that the custom type name doesn't already exist *)
       filter_gen ~max_attempts:10000 custom_type_name_gen ~f:(fun ct_name ->
           not (TestingTypeCtx.custom_exists type_ctx ct_name)))
      gen_constructors
  in
  int_range 0 max_custom_types >>= fun custom_type_count ->
  fix
    (fun self (n, type_ctx) ->
      if n <= 0 then return type_ctx
      else
        custom_type_gen ~type_ctx ~max_constructors ~mrd >>= fun new_ct ->
        self (n - 1, TestingTypeCtx.add_custom type_ctx new_ct))
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
      QCheck.Print.(Fn.compose (list print_custom_type) TestingTypeCtx.to_list)
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
    list (pair varname_gen (vtype_gen ~type_ctx default_max_gen_rec_depth))
    >|= TestingVarCtx.from_list
  in
  QCheck.make
    ~print:
      (Core.Fn.compose
         QCheck.Print.(list (pair string vtype_to_source_code))
         TestingVarCtx.to_list)
    gen

module TestingTypeChecker = TypeChecker (TestingTypeCtx) (TestingVarCtx)

let pattern_arb ~(type_ctx : TestingTypeCtx.t) ~(t : vtype) :
    (pattern * (string * vtype) list) QCheck.arbitrary =
  let open QCheck in
  let open QCheck.Gen in
  let rec gen_new_varname (ctx : TestingVarCtx.t) : string Gen.t =
    (* Generate a new variable name that is not already in the context *)
    varname_gen >>= fun vname ->
    if TestingVarCtx.exists ctx vname then gen_new_varname ctx else return vname
  in
  let named_var (ctx : TestingVarCtx.t) (t : vtype) :
      (pattern * TestingVarCtx.t) Gen.t =
    gen_new_varname ctx >|= fun vname ->
    (PatName (vname, t), TestingVarCtx.singleton vname t)
  in
  let rec gen_unit (ctx : TestingVarCtx.t) : (pattern * TestingVarCtx.t) Gen.t =
    (* Generate a pattern that types as unit *)
    named_var ctx VTypeUnit
  and gen_int (ctx : TestingVarCtx.t) : (pattern * TestingVarCtx.t) Gen.t =
    (* Generate a pattern that types as integer *)
    named_var ctx VTypeInt
  and gen_bool (ctx : TestingVarCtx.t) : (pattern * TestingVarCtx.t) Gen.t =
    (* Generate a pattern that types as boolean *)
    named_var ctx VTypeBool
  and gen_fun ((t1 : vtype), (t2 : vtype)) (ctx : TestingVarCtx.t) :
      (pattern * TestingVarCtx.t) Gen.t =
    (* Generate a pattern that types as a function *)
    named_var ctx (VTypeFun (t1, t2))
  and gen_pair ((t1 : vtype), (t2 : vtype)) (ctx : TestingVarCtx.t) :
      (pattern * TestingVarCtx.t) Gen.t =
    (* Generate a pattern that types as a pair *)
    oneof
      [
        named_var ctx (VTypePair (t1, t2));
        ( gen t1 ctx >>= fun (p1, ctx1) ->
          gen t2 ctx1 >|= fun (p2, ctx2) -> (PatPair (p1, p2), ctx2) );
      ]
  and gen_custom ((_, cs) : custom_type) (ctx : TestingVarCtx.t) :
      (* Generate a pattern that types as the specified custom type *)
      (pattern * TestingVarCtx.t) Gen.t =
    oneof (List.map ~f:return cs) >>= fun (c_name, c_t) ->
    gen c_t ctx >>= fun (p, ctx') -> return (PatConstructor (c_name, p), ctx')
  and gen (t : vtype) : TestingVarCtx.t -> (pattern * TestingVarCtx.t) Gen.t =
    (* Generate a pattern of a specified type *)
    match t with
    | VTypeUnit -> gen_unit
    | VTypeInt -> gen_int
    | VTypeBool -> gen_bool
    | VTypeFun (t1, t2) -> gen_fun (t1, t2)
    | VTypePair (t1, t2) -> gen_pair (t1, t2)
    | VTypeCustom ct_name ->
        Option.value_exn
          ~message:
            (sprintf
               "The custom type specified (%s) doesn't exist in the context"
               ct_name)
          (TestingTypeCtx.find_custom type_ctx ct_name)
        |> gen_custom
  in
  QCheck.make
    ~print:
      (Print.pair pattern_to_source_code
         (Print.list (Print.pair Print.string vtype_to_source_code)))
    ( gen t TestingVarCtx.empty >|= fun (p, ctx) ->
      (p, TestingVarCtx.to_list ctx) )

let ast_expr_gen ?(t : vtype option) ~(type_ctx : TestingTypeCtx.t)
    (v_gen : 'a QCheck.Gen.t) : 'a expr QCheck.Gen.t =
  let open QCheck in
  let open QCheck.Gen in
  let rec gen_e_var_of_type
      ( (_ : int * TestingVarCtx.t -> 'a expr Gen.t),
        ((_ : int), (ctx : TestingVarCtx.t)),
        (v : 'a) ) (t : vtype) : 'a expr Gen.t option =
    (* Shorthand for generating a variable node for an existing variable with a given type in the context *)
    match TestingVarCtx.varnames_of_type t ctx with
    | [] -> None
    | _ :: _ as varnames ->
        Some (varnames |> List.map ~f:return |> oneof >|= fun x -> Var (v, x))
  and gen_e_if
      ( (self : int * TestingVarCtx.t -> 'a expr Gen.t),
        ((d : int), (ctx : TestingVarCtx.t)),
        (v : 'a) ) : 'a expr Gen.t =
    (* Shorthand for generating an if-then-else expression *)
    triple (gen_bool (d - 1, ctx)) (self (d - 1, ctx)) (self (d - 1, ctx))
    >|= fun (e1, e2, e3) -> If (v, e1, e2, e3)
  and gen_e_let_in
      ( (self : int * TestingVarCtx.t -> 'a expr Gen.t),
        ((d : int), (ctx : TestingVarCtx.t)),
        (v : 'a) ) : 'a expr Gen.t =
    (* Shorthand for generating a let-in expression *)
    pair varname_gen (gen_any_of_type (d - 1, ctx))
    >>= fun (vname, (e1t, e1)) ->
    self (d - 1, TestingVarCtx.add ctx vname e1t) >|= fun e2 ->
    Let (v, vname, e1, e2)
  and gen_e_app
      ( (_ : int * TestingVarCtx.t -> 'a expr Gen.t),
        ((d : int), (ctx : TestingVarCtx.t)),
        (v : 'a) ) (t2 : vtype) : 'a expr Gen.t =
    (* Shorthand for generating a function application expression *)
    vtype_gen ~type_ctx d >>= fun t1 ->
    pair (gen_fun (t1, t2) (d - 1, ctx)) (gen (d - 1, ctx) t1)
    >|= fun (e1, e2) -> App (v, e1, e2)
  and gen_e_let_rec
      ( (self : int * TestingVarCtx.t -> 'a expr Gen.t),
        ((d : int), (ctx : TestingVarCtx.t)),
        (v : 'a) ) : 'a expr Gen.t =
    (* Shorthand for generating a let-rec expression *)
    pair
      (gen_unique_pair ~equal:equal_string varname_gen)
      (pair (vtype_gen ~type_ctx d) (vtype_gen ~type_ctx d))
    >>= fun ((fname, xname), (ftype1, ftype2)) ->
    let ctx_with_f = TestingVarCtx.add ctx fname (VTypeFun (ftype1, ftype2)) in
    let ctx_with_fx = TestingVarCtx.add ctx_with_f xname ftype1 in
    pair (gen (d - 1, ctx_with_fx) ftype2) (self (d - 1, ctx_with_f))
    >|= fun (e1, e2) ->
    Let (v, fname, Fix (v, (fname, ftype1, ftype2), (xname, ftype1), e1), e2)
  and gen_e_match
      ( (self : int * TestingVarCtx.t -> 'a expr Gen.t),
        ((d : int), (ctx : TestingVarCtx.t)),
        (v : 'a) ) : 'a expr Gen.t =
    (* Shorthand for generating a match expression *)
    gen_any_of_type (d - 1, ctx) >>= fun (e1_t, e1) ->
    let pat_gen = pattern_arb ~type_ctx ~t:e1_t |> QCheck.get_gen in
    let case_and_pat_gen =
      pat_gen >>= fun (p, p_ctx_list) ->
      let case_ctx =
        TestingVarCtx.append ctx (TestingVarCtx.from_list p_ctx_list)
      in
      self (d - 1, case_ctx) >|= fun e -> (p, e)
    in
    list_size (int_range 1 4) case_and_pat_gen
    >|= Nonempty_list.from_list_unsafe
    >|= fun cs -> Match (v, e1, cs)
  and standard_rec_gen_cases
      ( (self : int * TestingVarCtx.t -> 'a expr Gen.t),
        ((d : int), (ctx : TestingVarCtx.t)),
        (v : 'a) ) (t : vtype) : 'a expr Gen.t list =
    (* The standard recursive generator cases for some provided type *)
    [
      gen_e_if (self, (d, ctx), v) (* If-then-else *);
      gen_e_let_in (self, (d, ctx), v) (* Let-in *);
      gen_e_app (self, (d, ctx), v) t (* Function application *);
      gen_e_let_rec (self, (d, ctx), v) (* Let-rec *);
      gen_e_match (self, (d, ctx), v) (* Match *);
    ]
  and gen_unit (param : int * TestingVarCtx.t) : 'a expr Gen.t =
    (* Generate an expression that types as unit *)
    fix
      (fun self (d, ctx) ->
        v_gen >>= fun v ->
        let base_cases =
          [ return (UnitLit v) ]
          @ Option.to_list (gen_e_var_of_type (self, (d, ctx), v) VTypeUnit)
        in
        let rec_cases = standard_rec_gen_cases (self, (d, ctx), v) VTypeUnit in
        if d > 0 then oneof (base_cases @ rec_cases) else oneof base_cases)
      param
  and gen_int (param : int * TestingVarCtx.t) : 'a expr Gen.t =
    (* Generate an expression that types as integer *)
    fix
      (fun self (d, ctx) ->
        let self' = self (d - 1, ctx) in
        v_gen >>= fun v ->
        let base_cases =
          [ (nat >|= fun n -> IntLit (v, n)) ]
          @ Option.to_list (gen_e_var_of_type (self, (d, ctx), v) VTypeInt)
        in
        let rec_cases =
          [
            (pair self' self' >|= fun (e1, e2) -> Add (v, e1, e2) (* Addition *));
            (self' >|= fun e -> Neg (v, e) (* Negation *));
            ( pair self' self' >|= fun (e1, e2) -> Subtr (v, e1, e2)
              (* Subtraction *) );
            ( pair self' self' >|= fun (e1, e2) -> Mult (v, e1, e2)
              (* Multiplication *) );
          ]
          @ standard_rec_gen_cases (self, (d, ctx), v) VTypeInt
        in
        if d > 0 then oneof (base_cases @ rec_cases) else oneof base_cases)
      param
  and gen_bool (param : int * TestingVarCtx.t) : 'a expr Gen.t =
    (* Generate an expression that types as boolean *)
    fix
      (fun self (d, ctx) ->
        let self' = self (d - 1, ctx) in
        v_gen >>= fun v ->
        let base_cases =
          [ (bool >|= fun n -> BoolLit (v, n)) ]
          @ Option.to_list (gen_e_var_of_type (self, (d, ctx), v) VTypeBool)
        in
        let rec_cases =
          [
            (self' >|= fun e -> BNot (v, e) (* Negation *));
            ( pair self' self' >|= fun (e1, e2) -> BOr (v, e1, e2)
              (* Disjunction *) );
            ( pair self' self' >|= fun (e1, e2) -> BAnd (v, e1, e2)
              (* Conjunction *) );
            ( pair (gen_int (d - 1, ctx)) (gen_int (d - 1, ctx))
            >|= fun (e1, e2) -> Eq (v, e1, e2) (* Integer equality *) );
            ( pair self' self' >|= fun (e1, e2) -> Eq (v, e1, e2)
              (* Boolean equality *) );
            ( pair (gen_int (d - 1, ctx)) (gen_int (d - 1, ctx))
            >|= fun (e1, e2) -> Gt (v, e1, e2) (* GT *) );
            ( pair (gen_int (d - 1, ctx)) (gen_int (d - 1, ctx))
            >|= fun (e1, e2) -> GtEq (v, e1, e2) (* GTEQ *) );
            ( pair (gen_int (d - 1, ctx)) (gen_int (d - 1, ctx))
            >|= fun (e1, e2) -> Lt (v, e1, e2) (* LT *) );
            ( pair (gen_int (d - 1, ctx)) (gen_int (d - 1, ctx))
            >|= fun (e1, e2) -> LtEq (v, e1, e2) (* LTEQ *) );
          ]
          @ standard_rec_gen_cases (self, (d, ctx), v) VTypeBool
        in
        if d > 0 then oneof (base_cases @ rec_cases) else oneof base_cases)
      param
  and gen_fun ((t1 : vtype), (t2 : vtype)) (param : int * TestingVarCtx.t) :
      'a expr Gen.t =
    (* Generate an expression that has type of t1 -> t2
       Note, functions have no base cases so recursion depth cannot be used to terminate it safely *)
    let t = VTypeFun (t1, t2) in
    fix
      (fun self (d, ctx) ->
        v_gen >>= fun v ->
        let base_cases =
          [
            ( varname_gen >>= fun vname ->
              gen (d - 1, TestingVarCtx.add ctx vname t1) t2 >|= fun e ->
              Ast.Fun (v, (vname, t1), e) (* Function value *) );
          ]
          @ Option.to_list (gen_e_var_of_type (self, (d, ctx), v) t)
        in
        let rec_cases = standard_rec_gen_cases (self, (d, ctx), v) t in
        if d > 0 then oneof (base_cases @ rec_cases) else oneof base_cases)
      param
  and gen_pair ((t1 : vtype), (t2 : vtype)) (param : int * TestingVarCtx.t) :
      'a expr Gen.t =
    (* Generate an expression that types as a pair of the provided types *)
    let t = VTypePair (t1, t2) in
    fix
      (fun self (d, ctx) ->
        v_gen >>= fun v ->
        let base_cases =
          [
            ( pair (gen (d - 1, ctx) t1) (gen (d - 1, ctx) t2)
            >|= fun (e1, e2) -> Ast.Pair (v, e1, e2) );
          ]
          @ Option.to_list (gen_e_var_of_type (self, (d, ctx), v) t)
        in
        let rec_cases = standard_rec_gen_cases (self, (d, ctx), v) t in
        if d > 0 then oneof (base_cases @ rec_cases) else oneof base_cases)
      param
  and gen_custom ((ct_name, cs) : custom_type) (param : int * TestingVarCtx.t) :
      'a expr Gen.t =
    (* Generate an expression that types as the provided custom type *)
    let t = VTypeCustom ct_name in
    fix
      (fun self (d, ctx) ->
        v_gen >>= fun v ->
        let base_cases =
          [
            ( oneof (List.map ~f:return cs) >>= fun (c_name, c_t) ->
              gen (d - 1, ctx) c_t >|= fun e' -> Constructor (v, c_name, e') );
          ]
          @ Option.to_list (gen_e_var_of_type (self, (d, ctx), v) t)
        in
        let rec_cases = standard_rec_gen_cases (self, (d, ctx), v) t in
        if d > 0 then oneof (base_cases @ rec_cases) else oneof base_cases)
      param
  and gen ((d : int), (ctx : TestingVarCtx.t)) (t : vtype) : 'a expr Gen.t =
    match t with
    | VTypeUnit -> gen_unit (d, ctx)
    | VTypeInt -> gen_int (d, ctx)
    | VTypeBool -> gen_bool (d, ctx)
    | VTypeFun (t1, t2) -> gen_fun (t1, t2) (d, ctx)
    | VTypePair (t1, t2) -> gen_pair (t1, t2) (d, ctx)
    | VTypeCustom ct_name ->
        (Option.value_exn
           ~message:
             (sprintf
                "The custom type specified (%s) doesn't exist in the context"
                ct_name)
           (TestingTypeCtx.find_custom type_ctx ct_name)
        |> gen_custom)
          (d, ctx)
  and gen_any_of_type ((d : int), (ctx : TestingVarCtx.t)) :
      (vtype * 'a expr) Gen.t =
    vtype_gen ~type_ctx d >>= fun t ->
    gen (d, ctx) t >|= fun e -> (t, e)
  in
  match t with
  | Some t -> gen (max_gen_rec_depth, TestingVarCtx.empty) t
  | None -> gen_any_of_type (max_gen_rec_depth, TestingVarCtx.empty) >|= snd

let ast_expr_arb ?(t : vtype option) ~(type_ctx : TestingTypeCtx.t)
    (print : 'a ast_print_method) (v_gen : 'a QCheck.Gen.t) :
    'a expr QCheck.arbitrary =
  QCheck.make
    ?print:(get_ast_printer_opt print)
    ~shrink:(expr_shrink ~preserve_type:(Option.is_some t))
    (ast_expr_gen ?t ~type_ctx v_gen)

let ast_expr_arb_any ~(type_ctx : TestingTypeCtx.t) print v_gen =
  ast_expr_arb ~type_ctx print v_gen

let ast_expr_arb_default_type_ctx_params ?(t : vtype option)
    (print : 'a ast_print_method) (v_gen : 'a QCheck.Gen.t) :
    (TestingTypeCtx.t * 'a expr) QCheck.arbitrary =
  let gen : (TestingTypeCtx.t * 'a expr) QCheck.Gen.t =
    let open QCheck.Gen in
    default_testing_type_ctx_gen >>= fun type_ctx ->
    pair (return type_ctx) (ast_expr_gen ?t ~type_ctx v_gen)
  in
  QCheck.make
    ?print:Option.(get_ast_printer_opt print >>| fun p (_, e) -> p e)
    ~shrink:
      QCheck.Shrink.(pair nil (expr_shrink ~preserve_type:(Option.is_some t)))
    gen

let plain_ast_expr_arb_any ~(type_ctx : TestingTypeCtx.t) :
    unit expr QCheck.arbitrary =
  ast_expr_arb_any ~type_ctx PrintExprSource QCheck.Gen.unit

let plain_ast_expr_arb_any_default_type_ctx_params :
    (TestingTypeCtx.t * unit expr) QCheck.arbitrary =
  let gen : (TestingTypeCtx.t * unit expr) QCheck.Gen.t =
    let open QCheck.Gen in
    default_testing_type_ctx_gen >>= fun type_ctx ->
    pair (return type_ctx)
      (QCheck.get_gen
         (ast_expr_arb_any ~type_ctx PrintExprSource QCheck.Gen.unit))
  in
  QCheck.make
    ~print:(fun (type_ctx, e) ->
      sprintf "[type ctx: %s]\n%s"
        (type_ctx |> TestingTypeCtx.sexp_of_t |> Sexp.to_string)
        (ast_to_source_code ~use_newlines:true e))
    ~shrink:QCheck.Shrink.(pair nil (expr_shrink ~preserve_type:false))
    gen
