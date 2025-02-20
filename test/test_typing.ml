open OUnit2
open Core
open Pq_lang
open Utils
open Expr
open Typing
open Testing_utils

(* TODO - tests for type context checking *)

module Vtype_ast_qcheck_testing =
  Expr.QCheck_testing
    (struct
      type t = Vtype.t
    end)
    (struct
      type t = Vtype.t
    end)

let varname_gen = Varname.QCheck_testing.gen ()

let vtype_gen type_ctx =
  Vtype.QCheck_testing.gen
    {
      variant_types =
        TestingTypeCtx.type_defns_to_ordered_list type_ctx
        |> List.filter_map ~f:(function
             | CustomType.VariantType (vt_name, _) -> Some vt_name
             | CustomType.QuotientType _ -> None)
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
        (t : (Vtype.t, typing_error) Result.t) ) : test =
    Expr.to_source_code e >:: fun _ ->
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
        assert_equal ~cmp:Vtype.equal ~printer:Vtype.to_source_code exp_t out_t
    | Ok _, Error _ -> assert_failure "Expected typing error but got type"
    | Error _, Ok _ -> assert_failure "Expected type but got typing error"
    | Error t_err, Error exp_err ->
        assert_equal ~cmp:override_equal_typing_error
          ~printer:print_typing_error exp_err t_err
  in
  List.map ~f:create_test
    [
      (None, UnitLit (), Ok Vtype.VTypeUnit);
      (None, IntLit ((), 1), Ok Vtype.VTypeInt);
      (None, Add ((), IntLit ((), 3), IntLit ((), 0)), Ok Vtype.VTypeInt);
      ( None,
        Add ((), BoolLit ((), true), IntLit ((), 2)),
        Error (TypeMismatch (Vtype.VTypeInt, Vtype.VTypeBool, None)) );
      (None, Neg ((), IntLit ((), 3)), Ok Vtype.VTypeInt);
      ( None,
        Neg ((), BoolLit ((), false)),
        Error (TypeMismatch (Vtype.VTypeInt, Vtype.VTypeBool, None)) );
      (None, Subtr ((), IntLit ((), 3), IntLit ((), 0)), Ok Vtype.VTypeInt);
      ( None,
        Subtr ((), BoolLit ((), true), IntLit ((), 2)),
        Error (TypeMismatch (Vtype.VTypeInt, Vtype.VTypeBool, None)) );
      (None, Mult ((), IntLit ((), 3), IntLit ((), 0)), Ok Vtype.VTypeInt);
      ( None,
        Mult ((), BoolLit ((), true), IntLit ((), 2)),
        Error (TypeMismatch (Vtype.VTypeInt, Vtype.VTypeBool, None)) );
      (None, BoolLit ((), true), Ok Vtype.VTypeBool);
      (None, BNot ((), BoolLit ((), false)), Ok Vtype.VTypeBool);
      ( None,
        BNot ((), IntLit ((), 3)),
        Error (TypeMismatch (Vtype.VTypeBool, Vtype.VTypeInt, None)) );
      ( None,
        BAnd ((), BoolLit ((), false), BoolLit ((), true)),
        Ok Vtype.VTypeBool );
      ( None,
        BAnd ((), BoolLit ((), true), IntLit ((), 2)),
        Error (TypeMismatch (Vtype.VTypeBool, Vtype.VTypeInt, None)) );
      ( None,
        BOr ((), BoolLit ((), false), BoolLit ((), true)),
        Ok Vtype.VTypeBool );
      ( None,
        BOr ((), BoolLit ((), true), IntLit ((), 2)),
        Error (TypeMismatch (Vtype.VTypeBool, Vtype.VTypeInt, None)) );
      (None, Eq ((), IntLit ((), 3), IntLit ((), 0)), Ok Vtype.VTypeBool);
      ( None,
        Eq ((), BoolLit ((), true), IntLit ((), 2)),
        Error (EqualOperatorTypeMistmatch (Vtype.VTypeBool, Vtype.VTypeInt)) );
      ( None,
        Eq ((), IntLit ((), 3), BoolLit ((), true)),
        Error (EqualOperatorTypeMistmatch (Vtype.VTypeInt, Vtype.VTypeBool)) );
      (None, Eq ((), BoolLit ((), true), BoolLit ((), true)), Ok Vtype.VTypeBool);
      (None, GtEq ((), IntLit ((), 3), IntLit ((), 0)), Ok Vtype.VTypeBool);
      ( None,
        GtEq ((), BoolLit ((), true), IntLit ((), 2)),
        Error (TypeMismatch (Vtype.VTypeInt, Vtype.VTypeBool, None)) );
      (None, Gt ((), IntLit ((), 3), IntLit ((), 0)), Ok Vtype.VTypeBool);
      ( None,
        Gt ((), BoolLit ((), true), IntLit ((), 2)),
        Error (TypeMismatch (Vtype.VTypeInt, Vtype.VTypeBool, None)) );
      (None, LtEq ((), IntLit ((), 3), IntLit ((), 0)), Ok Vtype.VTypeBool);
      ( None,
        LtEq ((), BoolLit ((), true), IntLit ((), 2)),
        Error (TypeMismatch (Vtype.VTypeInt, Vtype.VTypeBool, None)) );
      (None, Lt ((), IntLit ((), 3), IntLit ((), 0)), Ok Vtype.VTypeBool);
      ( None,
        Lt ((), BoolLit ((), true), IntLit ((), 2)),
        Error (TypeMismatch (Vtype.VTypeInt, Vtype.VTypeBool, None)) );
      ( None,
        If ((), BoolLit ((), true), IntLit ((), 3), IntLit ((), 0)),
        Ok Vtype.VTypeInt );
      ( None,
        If ((), BoolLit ((), true), IntLit ((), 3), BoolLit ((), false)),
        Error (NoCommonRootType (Vtype.VTypeInt, Vtype.VTypeBool)) );
      ( None,
        If ((), BoolLit ((), true), IntLit ((), 3), IntLit ((), 0)),
        Ok Vtype.VTypeInt );
      (None, Var ((), "x"), Error (UndefinedVariable "x"));
      (None, Let ((), "x", IntLit ((), 3), Var ((), "x")), Ok Vtype.VTypeInt);
      ( None,
        Let ((), "x", BoolLit ((), true), Var ((), "x")),
        Ok Vtype.VTypeBool );
      ( None,
        Let ((), "x", IntLit ((), 3), BoolLit ((), true)),
        Ok Vtype.VTypeBool );
      ( None,
        Match
          ( (),
            IntLit ((), 3),
            Vtype.VTypeBool,
            Nonempty_list.from_list_unsafe
              [
                (Pattern.PatName ((), "x", Vtype.VTypeInt), BoolLit ((), true));
              ] ),
        Ok Vtype.VTypeBool );
      ( None,
        Match
          ( (),
            IntLit ((), 3),
            Vtype.VTypeInt,
            Nonempty_list.from_list_unsafe
              [ (Pattern.PatName ((), "x", Vtype.VTypeInt), Var ((), "x")) ] ),
        Ok Vtype.VTypeInt );
      ( None,
        Match
          ( (),
            IntLit ((), 3),
            Vtype.VTypeBool,
            Nonempty_list.from_list_unsafe
              [
                (Pattern.PatName ((), "x", Vtype.VTypeInt), BoolLit ((), true));
                (Pattern.PatName ((), "y", Vtype.VTypeInt), BoolLit ((), true));
              ] ),
        Ok Vtype.VTypeBool );
      ( None,
        Match
          ( (),
            IntLit ((), 3),
            Vtype.VTypeBool,
            Nonempty_list.from_list_unsafe
              [
                (Pattern.PatName ((), "x", Vtype.VTypeInt), BoolLit ((), true));
                (Pattern.PatName ((), "y", Vtype.VTypeBool), BoolLit ((), true));
              ] ),
        Error
          (PatternTypeMismatch
             ( Pattern.PatName ((), "y", Vtype.VTypeBool),
               Vtype.VTypeInt,
               Vtype.VTypeBool )) );
      ( (* Incorrect return type annotation *) None,
        Match
          ( (),
            IntLit ((), 3),
            Vtype.VTypeInt,
            Nonempty_list.from_list_unsafe
              [
                (Pattern.PatName ((), "x", Vtype.VTypeInt), BoolLit ((), true));
                (Pattern.PatName ((), "y", Vtype.VTypeBool), BoolLit ((), true));
              ] ),
        Error (TypeMismatch (Vtype.VTypeInt, Vtype.VTypeBool, None)) );
      ( None,
        Match
          ( (),
            IntLit ((), 3),
            Vtype.VTypeBool,
            Nonempty_list.from_list_unsafe
              [
                (Pattern.PatName ((), "x", Vtype.VTypeInt), BoolLit ((), true));
                (Pattern.PatName ((), "y", Vtype.VTypeInt), IntLit ((), 5));
              ] ),
        Error (TypeMismatch (Vtype.VTypeBool, Vtype.VTypeInt, None)) );
      ( (* Valid for constructor pattern *)
        Some
          (SetTypingTypeContext.create
             ~custom_types:
               [
                 VariantType ("empty", []);
                 VariantType
                   ( "int_list",
                     [
                       ("Nil", Vtype.VTypeUnit);
                       ( "Cons",
                         Vtype.VTypePair
                           (Vtype.VTypeInt, Vtype.VTypeCustom "int_list") );
                     ] );
               ]),
        Match
          ( (),
            Constructor
              ( (),
                "Cons",
                Pair ((), IntLit ((), 3), Constructor ((), "Nil", UnitLit ()))
              ),
            Vtype.VTypeInt,
            Nonempty_list.from_list_unsafe
              [
                ( Pattern.PatConstructor
                    ((), "Nil", Pattern.PatName ((), "z", Vtype.VTypeUnit)),
                  IntLit ((), 0) );
                ( Pattern.PatConstructor
                    ( (),
                      "Cons",
                      Pattern.PatName
                        ( (),
                          "x",
                          Vtype.VTypePair
                            (Vtype.VTypeInt, Vtype.VTypeCustom "int_list") ) ),
                  IntLit ((), 1) );
              ] ),
        Ok Vtype.VTypeInt );
      ( (* Non-existant variant type *)
        Some (SetTypingTypeContext.create ~custom_types:[]),
        Match
          ( (),
            Constructor
              ( (),
                "Cons",
                Pair ((), IntLit ((), 3), Constructor ((), "Nil", UnitLit ()))
              ),
            Vtype.VTypeInt,
            Nonempty_list.from_list_unsafe
              [
                ( Pattern.PatConstructor
                    ((), "Nil", Pattern.PatName ((), "z", Vtype.VTypeUnit)),
                  IntLit ((), 0) );
                ( Pattern.PatConstructor
                    ( (),
                      "Cons",
                      Pattern.PatName
                        ( (),
                          "x",
                          Vtype.VTypePair
                            (Vtype.VTypeInt, Vtype.VTypeCustom "int_list") ) ),
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
                       ("Leaf", Vtype.VTypeInt);
                       ( "Cons",
                         Vtype.VTypePair
                           (Vtype.VTypeInt, Vtype.VTypeCustom "list") );
                     ] );
               ]),
        Constructor ((), "Leaf", UnitLit ()),
        Error (TypeMismatch (Vtype.VTypeInt, Vtype.VTypeUnit, None)) );
      ( Some
          (SetTypingTypeContext.create
             ~custom_types:
               [
                 VariantType
                   ( "list",
                     [
                       ("Nil", Vtype.VTypeUnit);
                       ( "Cons",
                         Vtype.VTypePair
                           (Vtype.VTypeInt, Vtype.VTypeCustom "list") );
                     ] );
               ]),
        Constructor ((), "Nil", UnitLit ()),
        Ok (Vtype.VTypeCustom "list") );
    ]

let test_cases_expr_typing_full_check : test list =
  let create_test
      ( (name : string),
        (type_ctx : SetTypingTypeContext.t option),
        (e : plain_expr),
        (exp : (Vtype.t, Vtype.t) expr) ) : test =
    name >:: fun _ ->
    let open Result in
    let out =
      Typing.type_expr
        ~type_ctx:(Option.value ~default:SetTypingTypeContext.empty type_ctx)
        e
    in
    match out with
    | Ok e_typed ->
        let typed_out =
          e_typed |> Expr.fmap ~f:fst |> Expr.fmap_pattern ~f:fst
        in
        assert_equal
          ~cmp:(equal_expr Vtype.equal Vtype.equal)
          ~printer:
            (Vtype_ast_qcheck_testing.print
               (PrintSexp (Vtype.sexp_of_t, Vtype.sexp_of_t)))
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
          ( Vtype.VTypeInt,
            Subtr
              ( Vtype.VTypeInt,
                IntLit (Vtype.VTypeInt, 3),
                IntLit (Vtype.VTypeInt, 2) ),
            Mult
              ( Vtype.VTypeInt,
                IntLit (Vtype.VTypeInt, 2),
                Neg (Vtype.VTypeInt, IntLit (Vtype.VTypeInt, 7)) ) ) );
      ( "Mixed Typing 1",
        None,
        Add
          ( (),
            IntLit ((), 2),
            If ((), BoolLit ((), true), IntLit ((), 3), IntLit ((), 0)) ),
        Add
          ( Vtype.VTypeInt,
            IntLit (Vtype.VTypeInt, 2),
            If
              ( Vtype.VTypeInt,
                BoolLit (Vtype.VTypeBool, true),
                IntLit (Vtype.VTypeInt, 3),
                IntLit (Vtype.VTypeInt, 0) ) ) );
    ]

let test_cases_typing_with_var_ctx : test list =
  let open Result in
  let create_test
      ( (ctx_list : (string * Vtype.t) list),
        (e : plain_expr),
        (t : (Vtype.t, typing_error) Result.t) ) : test =
    let ctx =
      List.fold ~init:ListTypingVarContext.empty
        ~f:(fun acc (xname, xtype) -> ListTypingVarContext.add acc xname xtype)
        ctx_list
    in
    let name =
      sprintf "[with var context] %s"
        (e |> Expr.to_source_code ~use_newlines:true)
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
        assert_equal ~cmp:Vtype.equal ~printer:Vtype.to_source_code exp_t out_t
    | Ok _, Error _ -> assert_failure "Expected typing error but got type"
    | Error _, Ok _ -> assert_failure "Expected type but got typing error"
    | Error t_err, Error exp_err ->
        assert_equal ~cmp:override_equal_typing_error
          ~printer:print_typing_error exp_err t_err
  in
  List.map ~f:create_test
    [
      ([], Var ((), "x"), Error (UndefinedVariable "x"));
      ([ ("x", Vtype.VTypeInt) ], Var ((), "x"), Ok Vtype.VTypeInt);
      ([ ("x", Vtype.VTypeBool) ], Var ((), "x"), Ok Vtype.VTypeBool);
      ([ ("x", Vtype.VTypeBool) ], Var ((), "y"), Error (UndefinedVariable "y"));
    ]

(* TODO - type context tester module and implementations *)

module MakeVariableContextTester (VarCtx : TypingVarContext) = struct
  let create_test_add_then_get
      ((ctx : VarCtx.t), (xname : string), (xtype : Vtype.t)) : bool =
    let ctx' = VarCtx.add ctx xname xtype in
    (equal_option Vtype.equal) (Some xtype) (VarCtx.find ctx' xname)

  let create_test_overwrite
      ( (ctx : VarCtx.t),
        (xname : string),
        (xtype1 : Vtype.t),
        (xtype2 : Vtype.t) ) : bool =
    let ctx' = VarCtx.add (VarCtx.add ctx xname xtype1) xname xtype2 in
    (equal_option Vtype.equal) (Some xtype2) (VarCtx.find ctx' xname)

  let create_test_append_overwriting
      ( ((ctx1_ : VarCtx.t), (ctx2_ : VarCtx.t)),
        ((xname : string), (xtype1 : Vtype.t), (xtype2 : Vtype.t)) ) : bool =
    let ctx1 = VarCtx.add ctx1_ xname xtype1 in
    let ctx2 = VarCtx.add ctx2_ xname xtype2 in
    let ctx' = VarCtx.append ctx1 ctx2 in
    (equal_option Vtype.equal) (VarCtx.find ctx2 xname) (VarCtx.find ctx' xname)

  let all_test_cases : test list =
    let open QCheck in
    let var_ctx_from_list : (string * Vtype.t) list -> VarCtx.t =
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
        ((TestingTypeCtx.t * VarCtx.t) * (Vtype.t * Vtype.t)) QCheck.Gen.t =
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
  type t = (string -> Vtype.t option) * string list

  let empty : t = (Fn.const None, [])

  let add ((f', xs') : t) (xname : string) (xtype : Vtype.t) : t =
    ((fun x -> if equal_string xname x then Some xtype else f' x), xname :: xs')

  let find ((f, _) : t) = f
  let singleton xname xtype = add empty xname xtype

  let append ((f1, xs1) : t) ((f2, xs2) : t) : t =
    ((fun x -> match f2 x with None -> f1 x | Some v -> Some v), xs2 @ xs1)

  let exists ((_, xs) : t) xname = List.mem ~equal:equal_string xs xname

  let to_list ((f, xs) : t) : (string * Vtype.t) list =
    List.filter_map xs ~f:(fun x ->
        match f x with None -> None | Some v -> Some (x, v))
end

module FunctionVariableContextTester =
  MakeVariableContextTester (FunctionTypingVarContext)

module ListVariableContextTester =
  MakeVariableContextTester (ListTypingVarContext)

module TestingVariableContextTester = MakeVariableContextTester (TestingVarCtx)

let test_cases_arb_compound_expr_typing : test list =
  let open QCheck in
  let open QCheck.Gen in
  let expr_gen ~(type_ctx : TestingTypeCtx.t) (t : Vtype.t) :
      (unit, unit) expr Gen.t =
    Unit_expr_qcheck_testing.gen
      {
        t = Some (Unit_expr_qcheck_testing.vtype_to_gen_vtype_unsafe t);
        variant_types =
          TestingTypeCtx.type_defns_to_ordered_list type_ctx
          |> List.filter_map ~f:(function
               | CustomType.VariantType vt -> Some vt
               | CustomType.QuotientType _ -> None);
        top_level_defns = (* TODO - have arbitrary TLDs *) [];
        v_gen = QCheck.Gen.unit;
        pat_v_gen = QCheck.Gen.unit;
        mrd = default_max_gen_rec_depth;
      }
  in
  let create_test
      ( (name : string),
        (type_ctx : (TestingTypeCtx.t, Typing.typing_error) Result.t option),
        (e_gen : TestingTypeCtx.t -> (Vtype.t * plain_expr) Gen.t) ) : test =
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
            pair Vtype.to_source_code (Expr.to_source_code ~use_newlines:true))
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
                   Vtype.equal exp_t t
               | Error _ -> false)))
  in
  List.map ~f:create_test
    [
      ( "Add",
        None,
        fun type_ctx ->
          pair (return Vtype.VTypeInt)
            ( pair
                (expr_gen ~type_ctx Vtype.VTypeInt)
                (expr_gen ~type_ctx Vtype.VTypeInt)
            >|= fun (e1, e2) -> Add ((), e1, e2) ) );
      ( "Neg",
        None,
        fun type_ctx ->
          pair (return Vtype.VTypeInt)
            (expr_gen ~type_ctx Vtype.VTypeInt >|= fun e -> Neg ((), e)) );
      ( "Subtr",
        None,
        fun type_ctx ->
          pair (return Vtype.VTypeInt)
            ( pair
                (expr_gen ~type_ctx Vtype.VTypeInt)
                (expr_gen ~type_ctx Vtype.VTypeInt)
            >|= fun (e1, e2) -> Subtr ((), e1, e2) ) );
      ( "Mult",
        None,
        fun type_ctx ->
          pair (return Vtype.VTypeInt)
            ( pair
                (expr_gen ~type_ctx Vtype.VTypeInt)
                (expr_gen ~type_ctx Vtype.VTypeInt)
            >|= fun (e1, e2) -> Mult ((), e1, e2) ) );
      ( "BNot",
        None,
        fun type_ctx ->
          pair (return Vtype.VTypeBool)
            (expr_gen ~type_ctx Vtype.VTypeBool >|= fun e -> BNot ((), e)) );
      ( "BOr",
        None,
        fun type_ctx ->
          pair (return Vtype.VTypeBool)
            ( pair
                (expr_gen ~type_ctx Vtype.VTypeBool)
                (expr_gen ~type_ctx Vtype.VTypeBool)
            >|= fun (e1, e2) -> BOr ((), e1, e2) ) );
      ( "BAnd",
        None,
        fun type_ctx ->
          pair (return Vtype.VTypeBool)
            ( pair
                (expr_gen ~type_ctx Vtype.VTypeBool)
                (expr_gen ~type_ctx Vtype.VTypeBool)
            >|= fun (e1, e2) -> BAnd ((), e1, e2) ) );
      ( "Eq - int",
        None,
        fun type_ctx ->
          pair (return Vtype.VTypeBool)
            ( pair
                (expr_gen ~type_ctx Vtype.VTypeInt)
                (expr_gen ~type_ctx Vtype.VTypeInt)
            >|= fun (e1, e2) -> Eq ((), e1, e2) ) );
      ( "Eq - bool",
        None,
        fun type_ctx ->
          pair (return Vtype.VTypeBool)
            ( pair
                (expr_gen ~type_ctx Vtype.VTypeBool)
                (expr_gen ~type_ctx Vtype.VTypeBool)
            >|= fun (e1, e2) -> Eq ((), e1, e2) ) );
      ( "Gt",
        None,
        fun type_ctx ->
          pair (return Vtype.VTypeBool)
            ( pair
                (expr_gen ~type_ctx Vtype.VTypeInt)
                (expr_gen ~type_ctx Vtype.VTypeInt)
            >|= fun (e1, e2) -> Gt ((), e1, e2) ) );
      ( "GtEq",
        None,
        fun type_ctx ->
          pair (return Vtype.VTypeBool)
            ( pair
                (expr_gen ~type_ctx Vtype.VTypeInt)
                (expr_gen ~type_ctx Vtype.VTypeInt)
            >|= fun (e1, e2) -> GtEq ((), e1, e2) ) );
      ( "Lt",
        None,
        fun type_ctx ->
          pair (return Vtype.VTypeBool)
            ( pair
                (expr_gen ~type_ctx Vtype.VTypeInt)
                (expr_gen ~type_ctx Vtype.VTypeInt)
            >|= fun (e1, e2) -> Lt ((), e1, e2) ) );
      ( "LtEq",
        None,
        fun type_ctx ->
          pair (return Vtype.VTypeBool)
            ( pair
                (expr_gen ~type_ctx Vtype.VTypeInt)
                (expr_gen ~type_ctx Vtype.VTypeInt)
            >|= fun (e1, e2) -> LtEq ((), e1, e2) ) );
      ( "If",
        None,
        fun type_ctx ->
          vtype_gen type_ctx >>= fun t ->
          triple
            (expr_gen ~type_ctx Vtype.VTypeBool)
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
                       ("Nil", Vtype.VTypeUnit);
                       ( "Cons",
                         Vtype.VTypePair
                           (Vtype.VTypeInt, Vtype.VTypeCustom "list") );
                     ] );
                 VariantType ("int_box", [ ("IntBox", Vtype.VTypeInt) ]);
               ]),
        fun type_ctx ->
          pair
            (return (Vtype.VTypeCustom "list"))
            ( expr_gen ~type_ctx Vtype.VTypeUnit >|= fun e1 ->
              Constructor ((), "Nil", e1) ) );
      ( "Constructor - list Cons",
        Some
          (TestingTypeCtx.create
             ~custom_types:
               [
                 VariantType
                   ( "list",
                     [
                       ("Nil", Vtype.VTypeUnit);
                       ( "Cons",
                         Vtype.VTypePair
                           (Vtype.VTypeInt, Vtype.VTypeCustom "list") );
                     ] );
                 VariantType ("int_box", [ ("IntBox", Vtype.VTypeInt) ]);
               ]),
        fun type_ctx ->
          pair
            (return (Vtype.VTypeCustom "list"))
            ( expr_gen ~type_ctx
                (Vtype.VTypePair (Vtype.VTypeInt, Vtype.VTypeCustom "list"))
            >|= fun e1 -> Constructor ((), "Cons", e1) ) );
      ( "Constructor - int_box",
        Some
          (TestingTypeCtx.create
             ~custom_types:
               [
                 VariantType
                   ( "list",
                     [
                       ("Nil", Vtype.VTypeUnit);
                       ( "Cons",
                         Vtype.VTypePair
                           (Vtype.VTypeInt, Vtype.VTypeCustom "list") );
                     ] );
                 VariantType ("int_box", [ ("IntBox", Vtype.VTypeInt) ]);
               ]),
        fun type_ctx ->
          pair
            (return (Vtype.VTypeCustom "int_box"))
            ( expr_gen ~type_ctx Vtype.VTypeInt >|= fun e1 ->
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
