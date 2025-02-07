open OUnit2
open Core
open Pq_lang
open Utils
open Vtype
open Pattern
open Ast
open Custom_types
open Typing
open Testing_utils

(* TODO - tests for type context checking *)

module Vtype_ast_qcheck_testing =
  Ast.QCheck_testing
    (struct
      type t = vtype
    end)
    (struct
      type t = vtype
    end)

let varname_gen = Varname.QCheck_testing.gen ()

let vtype_gen type_ctx =
  Vtype.QCheck_testing.gen
    {
      variant_types =
        TestingTypeCtx.type_defns_to_ordered_list type_ctx
        |> List.filter_map ~f:(function
             | VariantType (vt_name, _) -> Some vt_name
             | QuotientType _ -> None)
        |> StringSet.of_list;
      mrd = default_max_gen_rec_depth;
      allow_fun_types = false;
    }

let test_cases_expr_typing : test list =
  let open Result in
  let create_test
      ( (type_ctx :
          (SetTypingTypeContext.t, Typing.typing_error) Result.t option),
        (e : plain_expr),
        (t : (vtype, typing_error) Result.t) ) : test =
    ast_to_source_code e >:: fun _ ->
    let type_ctx =
      Option.value ~default:(Ok SetTypingTypeContext.empty) type_ctx |> function
      | Ok type_ctx -> type_ctx
      | Error err ->
          failwith
            (sprintf "Error creating type context: %s" (print_typing_error err))
    in
    let out = Typing.type_expr ~type_ctx e in
    match (out, t) with
    | Ok e_typed, Ok exp_t ->
        let out_t = e_typed |> expr_node_val |> fst in
        assert_equal ~cmp:equal_vtype ~printer:vtype_to_source_code exp_t out_t
    | Ok _, Error _ -> assert_failure "Expected typing error but got type"
    | Error _, Ok _ -> assert_failure "Expected type but got typing error"
    | Error t_err, Error exp_err ->
        assert_equal ~cmp:equal_typing_error ~printer:print_typing_error exp_err
          t_err
  in
  List.map ~f:create_test
    [
      (None, UnitLit (), Ok VTypeUnit);
      (None, IntLit ((), 1), Ok VTypeInt);
      (None, Add ((), IntLit ((), 3), IntLit ((), 0)), Ok VTypeInt);
      ( None,
        Add ((), BoolLit ((), true), IntLit ((), 2)),
        Error (TypeMismatch (VTypeInt, VTypeBool)) );
      (None, Neg ((), IntLit ((), 3)), Ok VTypeInt);
      ( None,
        Neg ((), BoolLit ((), false)),
        Error (TypeMismatch (VTypeInt, VTypeBool)) );
      (None, Subtr ((), IntLit ((), 3), IntLit ((), 0)), Ok VTypeInt);
      ( None,
        Subtr ((), BoolLit ((), true), IntLit ((), 2)),
        Error (TypeMismatch (VTypeInt, VTypeBool)) );
      (None, Mult ((), IntLit ((), 3), IntLit ((), 0)), Ok VTypeInt);
      ( None,
        Mult ((), BoolLit ((), true), IntLit ((), 2)),
        Error (TypeMismatch (VTypeInt, VTypeBool)) );
      (None, BoolLit ((), true), Ok VTypeBool);
      (None, BNot ((), BoolLit ((), false)), Ok VTypeBool);
      ( None,
        BNot ((), IntLit ((), 3)),
        Error (TypeMismatch (VTypeBool, VTypeInt)) );
      (None, BAnd ((), BoolLit ((), false), BoolLit ((), true)), Ok VTypeBool);
      ( None,
        BAnd ((), BoolLit ((), true), IntLit ((), 2)),
        Error (TypeMismatch (VTypeBool, VTypeInt)) );
      (None, BOr ((), BoolLit ((), false), BoolLit ((), true)), Ok VTypeBool);
      ( None,
        BOr ((), BoolLit ((), true), IntLit ((), 2)),
        Error (TypeMismatch (VTypeBool, VTypeInt)) );
      (None, Eq ((), IntLit ((), 3), IntLit ((), 0)), Ok VTypeBool);
      ( None,
        Eq ((), BoolLit ((), true), IntLit ((), 2)),
        Error (EqualOperatorTypeMistmatch (VTypeBool, VTypeInt)) );
      ( None,
        Eq ((), IntLit ((), 3), BoolLit ((), true)),
        Error (EqualOperatorTypeMistmatch (VTypeInt, VTypeBool)) );
      (None, Eq ((), BoolLit ((), true), BoolLit ((), true)), Ok VTypeBool);
      (None, GtEq ((), IntLit ((), 3), IntLit ((), 0)), Ok VTypeBool);
      ( None,
        GtEq ((), BoolLit ((), true), IntLit ((), 2)),
        Error (TypeMismatch (VTypeInt, VTypeBool)) );
      (None, Gt ((), IntLit ((), 3), IntLit ((), 0)), Ok VTypeBool);
      ( None,
        Gt ((), BoolLit ((), true), IntLit ((), 2)),
        Error (TypeMismatch (VTypeInt, VTypeBool)) );
      (None, LtEq ((), IntLit ((), 3), IntLit ((), 0)), Ok VTypeBool);
      ( None,
        LtEq ((), BoolLit ((), true), IntLit ((), 2)),
        Error (TypeMismatch (VTypeInt, VTypeBool)) );
      (None, Lt ((), IntLit ((), 3), IntLit ((), 0)), Ok VTypeBool);
      ( None,
        Lt ((), BoolLit ((), true), IntLit ((), 2)),
        Error (TypeMismatch (VTypeInt, VTypeBool)) );
      ( None,
        If ((), BoolLit ((), true), IntLit ((), 3), IntLit ((), 0)),
        Ok VTypeInt );
      ( None,
        If ((), BoolLit ((), true), IntLit ((), 3), BoolLit ((), false)),
        Error (TypeMismatch (VTypeInt, VTypeBool)) );
      ( None,
        If ((), BoolLit ((), true), IntLit ((), 3), BoolLit ((), false)),
        Error (TypeMismatch (VTypeInt, VTypeBool)) );
      ( None,
        If ((), BoolLit ((), true), IntLit ((), 3), IntLit ((), 0)),
        Ok VTypeInt );
      (None, Var ((), "x"), Error (UndefinedVariable "x"));
      (None, Let ((), "x", IntLit ((), 3), Var ((), "x")), Ok VTypeInt);
      (None, Let ((), "x", BoolLit ((), true), Var ((), "x")), Ok VTypeBool);
      (None, Let ((), "x", IntLit ((), 3), BoolLit ((), true)), Ok VTypeBool);
      ( None,
        Match
          ( (),
            IntLit ((), 3),
            Nonempty_list.from_list_unsafe
              [ (PatName ((), "x", VTypeInt), BoolLit ((), true)) ] ),
        Ok VTypeBool );
      ( None,
        Match
          ( (),
            IntLit ((), 3),
            Nonempty_list.from_list_unsafe
              [ (PatName ((), "x", VTypeInt), Var ((), "x")) ] ),
        Ok VTypeInt );
      ( None,
        Match
          ( (),
            IntLit ((), 3),
            Nonempty_list.from_list_unsafe
              [
                (PatName ((), "x", VTypeInt), BoolLit ((), true));
                (PatName ((), "y", VTypeInt), BoolLit ((), true));
              ] ),
        Ok VTypeBool );
      ( None,
        Match
          ( (),
            IntLit ((), 3),
            Nonempty_list.from_list_unsafe
              [
                (PatName ((), "x", VTypeInt), BoolLit ((), true));
                (PatName ((), "y", VTypeBool), BoolLit ((), true));
              ] ),
        Error
          (PatternTypeMismatch
             (PatName ((), "y", VTypeBool), VTypeInt, VTypeBool)) );
      ( None,
        Match
          ( (),
            IntLit ((), 3),
            Nonempty_list.from_list_unsafe
              [
                (PatName ((), "x", VTypeInt), BoolLit ((), true));
                (PatName ((), "y", VTypeInt), IntLit ((), 5));
              ] ),
        Error (TypeMismatch (VTypeBool, VTypeInt)) );
      ( (* Valid for constructor pattern *)
        Some
          (SetTypingTypeContext.create
             ~custom_types:
               [
                 VariantType ("empty", []);
                 VariantType
                   ( "int_list",
                     [
                       ("Nil", VTypeUnit);
                       ("Cons", VTypePair (VTypeInt, VTypeCustom "int_list"));
                     ] );
               ]),
        Match
          ( (),
            Constructor
              ( (),
                "Cons",
                Pair ((), IntLit ((), 3), Constructor ((), "Nil", UnitLit ()))
              ),
            Nonempty_list.from_list_unsafe
              [
                ( PatConstructor ((), "Nil", PatName ((), "z", VTypeUnit)),
                  IntLit ((), 0) );
                ( PatConstructor
                    ( (),
                      "Cons",
                      PatName
                        ((), "x", VTypePair (VTypeInt, VTypeCustom "int_list"))
                    ),
                  IntLit ((), 1) );
              ] ),
        Ok VTypeInt );
      ( (* Non-existant variant type *)
        Some (SetTypingTypeContext.create ~custom_types:[]),
        Match
          ( (),
            Constructor
              ( (),
                "Cons",
                Pair ((), IntLit ((), 3), Constructor ((), "Nil", UnitLit ()))
              ),
            Nonempty_list.from_list_unsafe
              [
                ( PatConstructor ((), "Nil", PatName ((), "z", VTypeUnit)),
                  IntLit ((), 0) );
                ( PatConstructor
                    ( (),
                      "Cons",
                      PatName
                        ((), "x", VTypePair (VTypeInt, VTypeCustom "int_list"))
                    ),
                  IntLit ((), 1) );
              ] ),
        Error (UndefinedVariantTypeConstructor "Nil") );
      ( None,
        Constructor ((), "Nil", UnitLit ()),
        Error (UndefinedVariantTypeConstructor "Nil") );
      ( Some
          (SetTypingTypeContext.create
             ~custom_types:[ VariantType ("empty", []) ]),
        Constructor ((), "Nil", UnitLit ()),
        Error (UndefinedVariantTypeConstructor "Nil") );
      ( Some
          (SetTypingTypeContext.create
             ~custom_types:
               [
                 VariantType
                   ( "list",
                     [
                       ("Leaf", VTypeInt);
                       ("Cons", VTypePair (VTypeInt, VTypeCustom "list"));
                     ] );
               ]),
        Constructor ((), "Leaf", UnitLit ()),
        Error (TypeMismatch (VTypeInt, VTypeUnit)) );
      ( Some
          (SetTypingTypeContext.create
             ~custom_types:
               [
                 VariantType
                   ( "list",
                     [
                       ("Nil", VTypeUnit);
                       ("Cons", VTypePair (VTypeInt, VTypeCustom "list"));
                     ] );
               ]),
        Constructor ((), "Nil", UnitLit ()),
        Ok (VTypeCustom "list") );
    ]

let test_cases_expr_typing_full_check : test list =
  let create_test
      ( (name : string),
        (type_ctx : SetTypingTypeContext.t option),
        (e : plain_expr),
        (exp : (vtype, vtype) expr) ) : test =
    name >:: fun _ ->
    let open Result in
    let out =
      Typing.type_expr
        ~type_ctx:(Option.value ~default:SetTypingTypeContext.empty type_ctx)
        e
    in
    match out with
    | Ok e_typed ->
        let typed_out = e_typed |> Ast.fmap ~f:fst |> Ast.fmap_pattern ~f:fst in
        assert_equal
          ~cmp:(equal_expr equal_vtype equal_vtype)
          ~printer:
            (Vtype_ast_qcheck_testing.print
               (PrintSexp (sexp_of_vtype, sexp_of_vtype)))
          exp typed_out
    | Error _ -> assert_failure "Failed to type"
  in
  List.map ~f:create_test
    [
      ( "Plain Arithmetic 1",
        None,
        Add
          ( (),
            Subtr ((), IntLit ((), 3), IntLit ((), 2)),
            Mult ((), IntLit ((), 2), Neg ((), IntLit ((), 7))) ),
        Add
          ( VTypeInt,
            Subtr (VTypeInt, IntLit (VTypeInt, 3), IntLit (VTypeInt, 2)),
            Mult
              ( VTypeInt,
                IntLit (VTypeInt, 2),
                Neg (VTypeInt, IntLit (VTypeInt, 7)) ) ) );
      ( "Mixed Typing 1",
        None,
        Add
          ( (),
            IntLit ((), 2),
            If ((), BoolLit ((), true), IntLit ((), 3), IntLit ((), 0)) ),
        Add
          ( VTypeInt,
            IntLit (VTypeInt, 2),
            If
              ( VTypeInt,
                BoolLit (VTypeBool, true),
                IntLit (VTypeInt, 3),
                IntLit (VTypeInt, 0) ) ) );
    ]

let test_cases_typing_with_var_ctx : test list =
  let open Result in
  let create_test
      ( (ctx_list : (string * vtype) list),
        (e : plain_expr),
        (t : (vtype, typing_error) Result.t) ) : test =
    let ctx =
      List.fold ~init:ListTypingVarContext.empty
        ~f:(fun acc (xname, xtype) -> ListTypingVarContext.add acc xname xtype)
        ctx_list
    in
    let name =
      sprintf "[with var context] %s"
        (e |> ast_to_source_code ~use_newlines:true)
    in
    name >:: fun _ ->
    let out =
      SimpleTypeChecker.type_expr
        (SimpleTypeChecker.checked_empty_type_ctx, ctx)
        e
    in
    match (out, t) with
    | Ok e_typed, Ok exp_t ->
        let out_t = e_typed |> expr_node_val |> fst in
        assert_equal ~cmp:equal_vtype ~printer:vtype_to_source_code exp_t out_t
    | Ok _, Error _ -> assert_failure "Expected typing error but got type"
    | Error _, Ok _ -> assert_failure "Expected type but got typing error"
    | Error t_err, Error exp_err ->
        assert_equal ~cmp:equal_typing_error ~printer:print_typing_error exp_err
          t_err
  in
  List.map ~f:create_test
    [
      ([], Var ((), "x"), Error (UndefinedVariable "x"));
      ([ ("x", VTypeInt) ], Var ((), "x"), Ok VTypeInt);
      ([ ("x", VTypeBool) ], Var ((), "x"), Ok VTypeBool);
      ([ ("x", VTypeBool) ], Var ((), "y"), Error (UndefinedVariable "y"));
    ]

(* TODO - type context tester module and implementations *)

module MakeVariableContextTester (VarCtx : TypingVarContext) = struct
  let create_test_add_then_get
      ((ctx : VarCtx.t), (xname : string), (xtype : vtype)) : bool =
    let ctx' = VarCtx.add ctx xname xtype in
    (equal_option equal_vtype) (Some xtype) (VarCtx.find ctx' xname)

  let create_test_overwrite
      ((ctx : VarCtx.t), (xname : string), (xtype1 : vtype), (xtype2 : vtype)) :
      bool =
    let ctx' = VarCtx.add (VarCtx.add ctx xname xtype1) xname xtype2 in
    (equal_option equal_vtype) (Some xtype2) (VarCtx.find ctx' xname)

  let create_test_append_overwriting
      ( ((ctx1_ : VarCtx.t), (ctx2_ : VarCtx.t)),
        ((xname : string), (xtype1 : vtype), (xtype2 : vtype)) ) : bool =
    let ctx1 = VarCtx.add ctx1_ xname xtype1 in
    let ctx2 = VarCtx.add ctx2_ xname xtype2 in
    let ctx' = VarCtx.append ctx1 ctx2 in
    (equal_option equal_vtype) (VarCtx.find ctx2 xname) (VarCtx.find ctx' xname)

  let all_test_cases : test list =
    let open QCheck in
    let var_ctx_from_list : (string * vtype) list -> VarCtx.t =
      List.fold ~init:VarCtx.empty ~f:(fun acc (xname, xtype) ->
          VarCtx.add acc xname xtype)
    in
    let ctx_gen : (TestingTypeCtx.t * VarCtx.t) QCheck.Gen.t =
      let open QCheck.Gen in
      default_testing_type_ctx_gen >>= fun type_ctx ->
      list (pair string (vtype_gen type_ctx)) >|= fun ctx_list ->
      (type_ctx, var_ctx_from_list ctx_list)
    in
    let ctx_two_vtypes_gen :
        ((TestingTypeCtx.t * VarCtx.t) * (vtype * vtype)) QCheck.Gen.t =
      let open QCheck.Gen in
      ctx_gen >>= fun (type_ctx, var_ctx) ->
      pair
        (pair (return type_ctx) (return var_ctx))
        (pair (vtype_gen type_ctx) (vtype_gen type_ctx))
    in
    let ctx_two_vtypes_arb = QCheck.make ctx_two_vtypes_gen in
    List.map ~f:QCheck_runner.to_ounit2_test
      [
        Test.make ~name:"Add then get" ~count:100
          (pair ctx_two_vtypes_arb string)
          (fun (((_, ctx), (xtype, _)), xname) ->
            create_test_add_then_get (ctx, xname, xtype));
        Test.make ~name:"Overwrite" ~count:100 (pair ctx_two_vtypes_arb string)
          (fun (((_, ctx), (xtype1, xtype2)), xname) ->
            create_test_overwrite (ctx, xname, xtype1, xtype2));
        Test.make ~name:"Append overwriting" ~count:100
          (triple string ctx_two_vtypes_arb ctx_two_vtypes_arb)
          (fun (xname, ((_, ctx1), (xtype1, _)), ((_, ctx2), (xtype2, _))) ->
            create_test_append_overwriting
              ((ctx1, ctx2), (xname, xtype1, xtype2)));
      ]
end

(** An implementation of a variable context using functions. It isn't efficient,
    but is just meant to be used as a ground truth to test against the
    actually-used implementations *)
module FunctionTypingVarContext : TypingVarContext = struct
  type t = string -> vtype option

  let empty : t = Fn.const None

  let add (f' : t) (xname : string) (xtype : vtype) : t =
   fun x -> if equal_string xname x then Some xtype else f' x

  let find = Fn.id
  let singleton xname xtype = add empty xname xtype

  let append (f1 : t) (f2 : t) x =
    match f2 x with None -> f1 x | Some v -> Some v

  let exists (f : t) xname = match f xname with None -> false | Some _ -> true
end

module FunctionVariableContextTester =
  MakeVariableContextTester (FunctionTypingVarContext)

module ListVariableContextTester =
  MakeVariableContextTester (ListTypingVarContext)

module TestingVariableContextTester = MakeVariableContextTester (TestingVarCtx)

let test_cases_arb_compound_expr_typing : test list =
  let open QCheck in
  let open QCheck.Gen in
  let expr_gen ~(type_ctx : TestingTypeCtx.t) (t : vtype) :
      (unit, unit) expr Gen.t =
    Unit_ast_qcheck_testing.gen
      {
        t = Some (Unit_ast_qcheck_testing.vtype_to_gen_vtype_unsafe t);
        variant_types =
          TestingTypeCtx.type_defns_to_ordered_list type_ctx
          |> List.filter_map ~f:(function
               | VariantType vt -> Some vt
               | QuotientType _ -> None);
        top_level_defns = (* TODO - have arbitrary TLDs *) [];
        v_gen = QCheck.Gen.unit;
        pat_v_gen = QCheck.Gen.unit;
        mrd = default_max_gen_rec_depth;
      }
  in
  let create_test
      ( (name : string),
        (type_ctx : (TestingTypeCtx.t, Typing.typing_error) Result.t option),
        (e_gen : TestingTypeCtx.t -> (vtype * plain_expr) Gen.t) ) : test =
    let type_ctx : TestingTypeCtx.t =
      Option.value ~default:(Ok TestingTypeCtx.empty) type_ctx |> function
      | Ok type_ctx -> type_ctx
      | Error err ->
          failwith
            (sprintf "Error creating type context: %s" (print_typing_error err))
    in
    let e_arb =
      QCheck.make
        ~print:
          QCheck.Print.(
            pair vtype_to_source_code (ast_to_source_code ~use_newlines:true))
        (e_gen type_ctx)
    in
    QCheck_ounit.to_ounit2_test
      (Test.make ~name ~count:100 e_arb (fun (exp_t, e) ->
           match TestingTypeChecker.check_type_ctx type_ctx with
           | Error err ->
               Test.fail_reportf "Failed to check type ctx, with error: %s"
                 (print_typing_error err)
           | Ok type_ctx -> (
               match
                 TestingTypeChecker.type_expr (type_ctx, TestingVarCtx.empty) e
               with
               | Ok e_typed ->
                   let t = e_typed |> expr_node_val |> fst in
                   equal_vtype exp_t t
               | Error _ -> false)))
  in
  List.map ~f:create_test
    [
      ( "Add",
        None,
        fun type_ctx ->
          pair (return VTypeInt)
            ( pair (expr_gen ~type_ctx VTypeInt) (expr_gen ~type_ctx VTypeInt)
            >|= fun (e1, e2) -> Add ((), e1, e2) ) );
      ( "Neg",
        None,
        fun type_ctx ->
          pair (return VTypeInt)
            (expr_gen ~type_ctx VTypeInt >|= fun e -> Neg ((), e)) );
      ( "Subtr",
        None,
        fun type_ctx ->
          pair (return VTypeInt)
            ( pair (expr_gen ~type_ctx VTypeInt) (expr_gen ~type_ctx VTypeInt)
            >|= fun (e1, e2) -> Subtr ((), e1, e2) ) );
      ( "Mult",
        None,
        fun type_ctx ->
          pair (return VTypeInt)
            ( pair (expr_gen ~type_ctx VTypeInt) (expr_gen ~type_ctx VTypeInt)
            >|= fun (e1, e2) -> Mult ((), e1, e2) ) );
      ( "BNot",
        None,
        fun type_ctx ->
          pair (return VTypeBool)
            (expr_gen ~type_ctx VTypeBool >|= fun e -> BNot ((), e)) );
      ( "BOr",
        None,
        fun type_ctx ->
          pair (return VTypeBool)
            ( pair (expr_gen ~type_ctx VTypeBool) (expr_gen ~type_ctx VTypeBool)
            >|= fun (e1, e2) -> BOr ((), e1, e2) ) );
      ( "BAnd",
        None,
        fun type_ctx ->
          pair (return VTypeBool)
            ( pair (expr_gen ~type_ctx VTypeBool) (expr_gen ~type_ctx VTypeBool)
            >|= fun (e1, e2) -> BAnd ((), e1, e2) ) );
      ( "Eq - int",
        None,
        fun type_ctx ->
          pair (return VTypeBool)
            ( pair (expr_gen ~type_ctx VTypeInt) (expr_gen ~type_ctx VTypeInt)
            >|= fun (e1, e2) -> Eq ((), e1, e2) ) );
      ( "Eq - bool",
        None,
        fun type_ctx ->
          pair (return VTypeBool)
            ( pair (expr_gen ~type_ctx VTypeBool) (expr_gen ~type_ctx VTypeBool)
            >|= fun (e1, e2) -> Eq ((), e1, e2) ) );
      ( "Gt",
        None,
        fun type_ctx ->
          pair (return VTypeBool)
            ( pair (expr_gen ~type_ctx VTypeInt) (expr_gen ~type_ctx VTypeInt)
            >|= fun (e1, e2) -> Gt ((), e1, e2) ) );
      ( "GtEq",
        None,
        fun type_ctx ->
          pair (return VTypeBool)
            ( pair (expr_gen ~type_ctx VTypeInt) (expr_gen ~type_ctx VTypeInt)
            >|= fun (e1, e2) -> GtEq ((), e1, e2) ) );
      ( "Lt",
        None,
        fun type_ctx ->
          pair (return VTypeBool)
            ( pair (expr_gen ~type_ctx VTypeInt) (expr_gen ~type_ctx VTypeInt)
            >|= fun (e1, e2) -> Lt ((), e1, e2) ) );
      ( "LtEq",
        None,
        fun type_ctx ->
          pair (return VTypeBool)
            ( pair (expr_gen ~type_ctx VTypeInt) (expr_gen ~type_ctx VTypeInt)
            >|= fun (e1, e2) -> LtEq ((), e1, e2) ) );
      ( "If",
        None,
        fun type_ctx ->
          vtype_gen type_ctx >>= fun t ->
          triple
            (expr_gen ~type_ctx VTypeBool)
            (expr_gen ~type_ctx t) (expr_gen ~type_ctx t)
          >|= fun (e1, e2, e3) -> (t, If ((), e1, e2, e3)) );
      ( "Let",
        None,
        fun type_ctx ->
          vtype_gen type_ctx >>= fun t ->
          pair varname_gen (vtype_gen type_ctx) >>= fun (xname, xtype) ->
          pair (expr_gen ~type_ctx xtype) (expr_gen ~type_ctx t)
          >|= fun (e1, e2) -> (t, Let ((), xname, e1, e2))
        (* Note, the tests here (and for the Fun and Fix cases) don't work with changing variable contexts. But this should be fine *)
      );
      ( "Constructor - list Nil",
        Some
          (TestingTypeCtx.create
             ~custom_types:
               [
                 VariantType
                   ( "list",
                     [
                       ("Nil", VTypeUnit);
                       ("Cons", VTypePair (VTypeInt, VTypeCustom "list"));
                     ] );
                 VariantType ("int_box", [ ("IntBox", VTypeInt) ]);
               ]),
        fun type_ctx ->
          pair
            (return (VTypeCustom "list"))
            ( expr_gen ~type_ctx VTypeUnit >|= fun e1 ->
              Constructor ((), "Nil", e1) ) );
      ( "Constructor - list Cons",
        Some
          (TestingTypeCtx.create
             ~custom_types:
               [
                 VariantType
                   ( "list",
                     [
                       ("Nil", VTypeUnit);
                       ("Cons", VTypePair (VTypeInt, VTypeCustom "list"));
                     ] );
                 VariantType ("int_box", [ ("IntBox", VTypeInt) ]);
               ]),
        fun type_ctx ->
          pair
            (return (VTypeCustom "list"))
            ( expr_gen ~type_ctx (VTypePair (VTypeInt, VTypeCustom "list"))
            >|= fun e1 -> Constructor ((), "Cons", e1) ) );
      ( "Constructor - int_box",
        Some
          (TestingTypeCtx.create
             ~custom_types:
               [
                 VariantType
                   ( "list",
                     [
                       ("Nil", VTypeUnit);
                       ("Cons", VTypePair (VTypeInt, VTypeCustom "list"));
                     ] );
                 VariantType ("int_box", [ ("IntBox", VTypeInt) ]);
               ]),
        fun type_ctx ->
          pair
            (return (VTypeCustom "int_box"))
            ( expr_gen ~type_ctx VTypeInt >|= fun e1 ->
              Constructor ((), "IntBox", e1) ) );
    ]

let test_cases_typing_maintains_structure : test =
  let open QCheck in
  QCheck_ounit.to_ounit2_test
    (Test.make ~name:"Typing maintains structure" ~count:100
       unit_program_arbitrary_with_default_options (fun prog ->
         let type_ctx = prog.custom_types |> TestingTypeCtx.from_list in
         let e = prog.e in
         match TestingTypeChecker.check_type_ctx type_ctx with
         | Error err ->
             Test.fail_reportf "Failed to check type ctx, with error: %s"
               (print_typing_error err)
         | Ok type_ctx -> (
             match
               TestingTypeChecker.type_expr (type_ctx, TestingVarCtx.empty) e
             with
             | Ok e_typed ->
                 let plain_e = expr_to_plain_expr e in
                 let plain_typed_e = e_typed |> expr_to_plain_expr in
                 equal_plain_expr plain_e plain_typed_e
             | Error _ -> false)))

let suite =
  "Typing"
  >::: [
         "Expression typing" >::: test_cases_expr_typing;
         "Expression typing full hierarchy"
         >::: test_cases_expr_typing_full_check;
         "Expression typing with context" >::: test_cases_typing_with_var_ctx;
         "Arbitrary expression typing" >::: test_cases_arb_compound_expr_typing;
         "Typing maintains structure"
         >::: [ test_cases_typing_maintains_structure ];
         "Variable contexts"
         >::: [
                "Function context"
                >::: FunctionVariableContextTester.all_test_cases;
                "List context" >::: ListVariableContextTester.all_test_cases;
                "Testing context"
                >::: TestingVariableContextTester.all_test_cases;
              ];
       ]
