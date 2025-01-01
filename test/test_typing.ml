open OUnit2
open Core
open Pq_lang
open Utils
open Vtype
open Pattern
open Ast
open Typing
open Testing_utils

let test_cases_expr_typing : test list =
  let open Result in
  let create_test
      ( (type_ctx : SetTypingTypeContext.t option),
        (e : plain_expr),
        (t : (vtype, typing_error) Result.t) ) : test =
    ast_to_source_code e >:: fun _ ->
    let out =
      Typing.type_expr
        ~type_ctx:(Option.value ~default:SetTypingTypeContext.empty type_ctx)
        e
    in
    match (out, t) with
    | Ok tpe, Ok exp_t ->
        let out_t =
          tpe |> SimpleTypeChecker.typed_program_expression_get_expression
          |> expr_node_val |> fst
        in
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
      ( None,
        Fun ((), ("x", VTypeInt), Var ((), "x")),
        Ok (VTypeFun (VTypeInt, VTypeInt)) );
      (None, Let ((), "x", IntLit ((), 3), Var ((), "x")), Ok VTypeInt);
      (None, Let ((), "x", BoolLit ((), true), Var ((), "x")), Ok VTypeBool);
      (None, Let ((), "x", IntLit ((), 3), BoolLit ((), true)), Ok VTypeBool);
      ( None,
        Let
          ( (),
            "f",
            Fun ((), ("x", VTypeInt), Add ((), Var ((), "x"), IntLit ((), 1))),
            App ((), Var ((), "f"), IntLit ((), 3)) ),
        Ok VTypeInt );
      ( None,
        Fun ((), ("x", VTypeInt), Add ((), Var ((), "x"), IntLit ((), 1))),
        Ok (VTypeFun (VTypeInt, VTypeInt)) );
      ( None,
        Let
          ( (),
            "f",
            Fun ((), ("x", VTypeInt), Var ((), "x")),
            App ((), Var ((), "f"), IntLit ((), 3)) ),
        Ok VTypeInt );
      ( None,
        Let
          ( (),
            "f",
            Fix ((), ("f", VTypeInt, VTypeInt), ("x", VTypeInt), Var ((), "x")),
            App ((), Var ((), "f"), IntLit ((), 3)) ),
        Ok VTypeInt );
      ( None,
        Let
          ( (),
            "f",
            Fix
              ( (),
                ("f", VTypeInt, VTypeInt),
                ("x", VTypeInt),
                BoolLit ((), false) ),
            App ((), Var ((), "f"), IntLit ((), 3)) ),
        Error (TypeMismatch (VTypeInt, VTypeBool)) );
      ( None,
        Let
          ( (),
            "f",
            Fix ((), ("f", VTypeInt, VTypeInt), ("x", VTypeBool), Var ((), "x")),
            App ((), Var ((), "f"), IntLit ((), 3)) ),
        Error (TypeMismatch (VTypeInt, VTypeBool)) );
      ( None,
        Let
          ( (),
            "f",
            Fix ((), ("f", VTypeInt, VTypeInt), ("x", VTypeBool), Var ((), "x")),
            App ((), Var ((), "f"), BoolLit ((), false)) ),
        Error (TypeMismatch (VTypeInt, VTypeBool)) );
      ( None,
        Let
          ( (),
            "f",
            Fix
              ( (),
                ("f", VTypeInt, VTypeBool),
                ("x", VTypeInt),
                BoolLit ((), false) ),
            App ((), Var ((), "f"), IntLit ((), 3)) ),
        Ok VTypeBool );
      ( None,
        Match
          ( (),
            IntLit ((), 3),
            Nonempty_list.from_list_unsafe
              [ (PatName ("x", VTypeInt), BoolLit ((), true)) ] ),
        Ok VTypeBool );
      ( None,
        Match
          ( (),
            IntLit ((), 3),
            Nonempty_list.from_list_unsafe
              [ (PatName ("x", VTypeInt), Var ((), "x")) ] ),
        Ok VTypeInt );
      ( None,
        Match
          ( (),
            IntLit ((), 3),
            Nonempty_list.from_list_unsafe
              [
                (PatName ("x", VTypeInt), BoolLit ((), true));
                (PatName ("y", VTypeInt), BoolLit ((), true));
              ] ),
        Ok VTypeBool );
      ( None,
        Match
          ( (),
            IntLit ((), 3),
            Nonempty_list.from_list_unsafe
              [
                (PatName ("x", VTypeInt), BoolLit ((), true));
                (PatName ("y", VTypeBool), BoolLit ((), true));
              ] ),
        Error
          (PatternTypeMismatch (PatName ("y", VTypeBool), VTypeInt, VTypeBool))
      );
      ( None,
        Match
          ( (),
            IntLit ((), 3),
            Nonempty_list.from_list_unsafe
              [
                (PatName ("x", VTypeInt), BoolLit ((), true));
                (PatName ("y", VTypeInt), IntLit ((), 5));
              ] ),
        Error (TypeMismatch (VTypeBool, VTypeInt)) );
      ( None,
        Constructor ((), "Nil", UnitLit ()),
        Error (UndefinedCustomTypeConstructor "Nil") );
      ( Some (SetTypingTypeContext.create ~custom_types:[ ("Empty", []) ]),
        Constructor ((), "Nil", UnitLit ()),
        Error (UndefinedCustomTypeConstructor "Nil") );
      ( Some
          (SetTypingTypeContext.create
             ~custom_types:
               [
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
        (exp : vtype expr) ) : test =
    name >:: fun _ ->
    let open Result in
    let out =
      Typing.type_expr
        ~type_ctx:(Option.value ~default:SetTypingTypeContext.empty type_ctx)
        e
    in
    match out with
    | Ok tpe ->
        let typed_out =
          tpe |> SimpleTypeChecker.typed_program_expression_get_expression
          >|= fst
        in
        assert_equal ~cmp:(equal_expr equal_vtype)
          ~printer:(get_asp_printer (PrintSexp sexp_of_vtype))
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
    let name = sprintf "[with var context] %s" (e |> ast_to_source_code) in
    name >:: fun _ ->
    let out = SimpleTypeChecker.type_expr (SetTypingTypeContext.empty, ctx) e in
    match (out, t) with
    | Ok tpe, Ok exp_t ->
        let out_t =
          tpe |> SimpleTypeChecker.typed_program_expression_get_expression
          |> expr_node_val |> fst
        in
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
      ( [ ("x", VTypeBool) ],
        Fun ((), ("y", VTypeInt), IntLit ((), 2)),
        Ok (VTypeFun (VTypeInt, VTypeInt)) );
      ( [ ("x", VTypeBool) ],
        Fun ((), ("x", VTypeInt), IntLit ((), 2)),
        Ok (VTypeFun (VTypeInt, VTypeInt)) );
    ]

(* TODO - type context tester module and implementations *)

module MakeVariableContextTester (Ctx : TypingVarContext) = struct
  let create_test_add_then_get ((ctx : Ctx.t), (xname : string), (xtype : vtype))
      : bool =
    let ctx' = Ctx.add ctx xname xtype in
    (equal_option equal_vtype) (Some xtype) (Ctx.find ctx' xname)

  let create_test_overwrite
      ((ctx : Ctx.t), (xname : string), (xtype1 : vtype), (xtype2 : vtype)) :
      bool =
    let ctx' = Ctx.add (Ctx.add ctx xname xtype1) xname xtype2 in
    (equal_option equal_vtype) (Some xtype2) (Ctx.find ctx' xname)

  let create_test_append_overwriting
      ( ((ctx1_ : Ctx.t), (ctx2_ : Ctx.t)),
        ((xname : string), (xtype1 : vtype), (xtype2 : vtype)) ) : bool =
    let ctx1 = Ctx.add ctx1_ xname xtype1 in
    let ctx2 = Ctx.add ctx2_ xname xtype2 in
    let ctx' = Ctx.append ctx1 ctx2 in
    (equal_option equal_vtype) (Ctx.find ctx2 xname) (Ctx.find ctx' xname)

  let all_test_cases : test list =
    let open QCheck in
    let ctx_from_list : (string * vtype) list -> Ctx.t =
      List.fold ~init:Ctx.empty ~f:(fun acc (xname, xtype) ->
          Ctx.add acc xname xtype)
    in
    let ctx_arb : Ctx.t QCheck.arbitrary =
      QCheck.make
        QCheck.Gen.(
          list (pair string (vtype_gen default_max_gen_rec_depth))
          >|= ctx_from_list)
    in
    List.map ~f:QCheck_runner.to_ounit2_test
      [
        Test.make ~name:"Add then get" ~count:100
          (triple ctx_arb string (vtype_arb default_max_gen_rec_depth))
          create_test_add_then_get;
        Test.make ~name:"Overwrite" ~count:100
          (quad ctx_arb string
             (vtype_arb default_max_gen_rec_depth)
             (vtype_arb default_max_gen_rec_depth))
          create_test_overwrite;
        Test.make ~name:"Append overwriting" ~count:100
          (pair (pair ctx_arb ctx_arb)
             (triple string
                (vtype_arb default_max_gen_rec_depth)
                (vtype_arb default_max_gen_rec_depth)))
          create_test_append_overwriting;
      ]
end

(** An implementation of a variable context using functions.
    It isn't efficient, but is just meant to be used
    as a ground truth to test against the actually-used implementations *)
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
  let expr_gen (t : vtype) : unit expr Gen.t =
    ast_expr_arb ~t NoPrint Gen.unit |> QCheck.gen
  in
  let create_test
      ( (name : string),
        (type_ctx : SetTypingTypeContext.t option),
        (e_gen : (vtype * plain_expr) Gen.t) ) : test =
    let e_arb =
      QCheck.make
        ~print:QCheck.Print.(pair vtype_to_source_code ast_to_source_code)
        e_gen
    in
    QCheck_ounit.to_ounit2_test
      (Test.make ~name ~count:100 e_arb (fun (exp_t, e) ->
           let open Result in
           let out =
             Typing.type_expr
               ~type_ctx:
                 (Option.value ~default:SetTypingTypeContext.empty type_ctx)
               e
           in
           match out with
           | Ok tpe ->
               let t =
                 tpe
                 |> SimpleTypeChecker.typed_program_expression_get_expression
                 |> expr_node_val |> fst
               in
               equal_vtype exp_t t
           | Error _ -> false))
  in
  List.map ~f:create_test
    [
      ( "Add",
        None,
        pair (return VTypeInt)
          ( pair (expr_gen VTypeInt) (expr_gen VTypeInt) >|= fun (e1, e2) ->
            Add ((), e1, e2) ) );
      ( "Neg",
        None,
        pair (return VTypeInt) (expr_gen VTypeInt >|= fun e -> Neg ((), e)) );
      ( "Subtr",
        None,
        pair (return VTypeInt)
          ( pair (expr_gen VTypeInt) (expr_gen VTypeInt) >|= fun (e1, e2) ->
            Subtr ((), e1, e2) ) );
      ( "Mult",
        None,
        pair (return VTypeInt)
          ( pair (expr_gen VTypeInt) (expr_gen VTypeInt) >|= fun (e1, e2) ->
            Mult ((), e1, e2) ) );
      ( "BNot",
        None,
        pair (return VTypeBool) (expr_gen VTypeBool >|= fun e -> BNot ((), e))
      );
      ( "BOr",
        None,
        pair (return VTypeBool)
          ( pair (expr_gen VTypeBool) (expr_gen VTypeBool) >|= fun (e1, e2) ->
            BOr ((), e1, e2) ) );
      ( "BAnd",
        None,
        pair (return VTypeBool)
          ( pair (expr_gen VTypeBool) (expr_gen VTypeBool) >|= fun (e1, e2) ->
            BAnd ((), e1, e2) ) );
      ( "Eq - int",
        None,
        pair (return VTypeBool)
          ( pair (expr_gen VTypeInt) (expr_gen VTypeInt) >|= fun (e1, e2) ->
            Eq ((), e1, e2) ) );
      ( "Eq - bool",
        None,
        pair (return VTypeBool)
          ( pair (expr_gen VTypeBool) (expr_gen VTypeBool) >|= fun (e1, e2) ->
            Eq ((), e1, e2) ) );
      ( "Gt",
        None,
        pair (return VTypeBool)
          ( pair (expr_gen VTypeInt) (expr_gen VTypeInt) >|= fun (e1, e2) ->
            Gt ((), e1, e2) ) );
      ( "GtEq",
        None,
        pair (return VTypeBool)
          ( pair (expr_gen VTypeInt) (expr_gen VTypeInt) >|= fun (e1, e2) ->
            GtEq ((), e1, e2) ) );
      ( "Lt",
        None,
        pair (return VTypeBool)
          ( pair (expr_gen VTypeInt) (expr_gen VTypeInt) >|= fun (e1, e2) ->
            Lt ((), e1, e2) ) );
      ( "LtEq",
        None,
        pair (return VTypeBool)
          ( pair (expr_gen VTypeInt) (expr_gen VTypeInt) >|= fun (e1, e2) ->
            LtEq ((), e1, e2) ) );
      ( "If",
        None,
        vtype_gen default_max_gen_rec_depth >>= fun t ->
        triple (expr_gen VTypeBool) (expr_gen t) (expr_gen t)
        >|= fun (e1, e2, e3) -> (t, If ((), e1, e2, e3)) );
      ( "Let",
        None,
        vtype_gen default_max_gen_rec_depth >>= fun t ->
        pair varname_gen (vtype_gen default_max_gen_rec_depth)
        >>= fun (xname, xtype) ->
        pair (expr_gen xtype) (expr_gen t) >|= fun (e1, e2) ->
        (t, Let ((), xname, e1, e2))
        (* Note, the tests here (and for the Fun and Fix cases) don't work with changing variable contexts. But this should be fine *)
      );
      ( "Fun",
        None,
        vtype_gen default_max_gen_rec_depth >>= fun t2 ->
        pair varname_gen (vtype_gen default_max_gen_rec_depth)
        >>= fun (xname, t1) ->
        expr_gen t2 >|= fun e ->
        (VTypeFun (t1, t2), Ast.Fun ((), (xname, t1), e)) );
      ( "App",
        None,
        pair
          (vtype_gen default_max_gen_rec_depth)
          (vtype_gen default_max_gen_rec_depth)
        >>= fun (t1, t2) ->
        pair (expr_gen (VTypeFun (t1, t2))) (expr_gen t1) >|= fun (e1, e2) ->
        (t2, App ((), e1, e2)) );
      ( "Fix",
        None,
        pair varname_gen (vtype_gen default_max_gen_rec_depth)
        >>= fun (fname, t1) ->
        pair varname_gen (vtype_gen default_max_gen_rec_depth)
        >>= fun (xname, t2) ->
        expr_gen t2 >|= fun e ->
        (VTypeFun (t1, t2), Fix ((), (fname, t1, t2), (xname, t1), e)) );
      (* TODO - test cases for custom type constructors *)
    ]

let test_cases_typing_maintains_structure : test =
  let open QCheck in
  QCheck_ounit.to_ounit2_test
    (Test.make ~name:"Typing maintains structure" ~count:100
       (ast_expr_arb PrintExprSource Gen.unit) (fun e ->
         let open Result in
         let out = Typing.type_expr ~type_ctx:SetTypingTypeContext.empty e in
         (* TODO - have an arbitrary type context and use this in test *)
         match out with
         | Ok tpe ->
             let plain_e = expr_to_plain_expr e in
             let plain_typed_e =
               tpe |> SimpleTypeChecker.typed_program_expression_get_expression
               |> expr_to_plain_expr
             in
             equal_plain_expr plain_e plain_typed_e
         | Error _ -> false))

(* TODO - custom type definitions (typing custom type definitions) *)

(* TODO - custom type definitions (typing when custom types have been defined) *)

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
