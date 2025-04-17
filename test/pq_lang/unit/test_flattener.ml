open Core
open Pq_lang
open Utils
open Pq_lang.Pattern
open Testing_utils
module Flattener = Flattener.Make (TestingTypeChecker)
module StdPattern = Pq_lang.Pattern.StdPattern
module StdExpr = Pq_lang.Expr.StdExpr
module StdProgram = Pq_lang.Program.StdProgram

type ('tag_e, 'tag_p) test_case = {
  name : string;
  custom_types : (Vtype.t * 'tag_e, Vtype.t * 'tag_p) CustomType.t list;
  arg_t : Vtype.t;
  cases : (Vtype.t * unit) StdPattern.t Nonempty_list.t;
}

let sample_test_cases : (unit, unit) test_case list =
  let open Vtype in
  List.map
    ~f:(fun (w, x, y, z) ->
      {
        name = w;
        custom_types = x;
        arg_t = y;
        cases = Nonempty_list.from_list_unsafe z;
      })
    [
      ( "(T u, x) | (F u, x)",
        CustomType.
          [ VariantType ("my_bool", [ ("T", VTypeUnit); ("F", VTypeUnit) ]) ],
        VTypePair (VTypeCustom "my_bool", VTypeInt),
        [
          PatPair
            ( (VTypePair (VTypeCustom "my_bool", VTypeInt), ()),
              PatConstructor
                ( (VTypeCustom "my_bool", ()),
                  "T",
                  PatName ((VTypeUnit, ()), "u", VTypeUnit) ),
              PatName ((VTypeInt, ()), "x", VTypeInt) );
          PatPair
            ( (VTypePair (VTypeCustom "my_bool", VTypeInt), ()),
              PatConstructor
                ( (VTypeCustom "my_bool", ()),
                  "F",
                  PatName ((VTypeUnit, ()), "u", VTypeUnit) ),
              PatName ((VTypeInt, ()), "x", VTypeInt) );
        ] );
      ( "(x, T u) | (x, F u)",
        CustomType.
          [ VariantType ("my_bool", [ ("T", VTypeUnit); ("F", VTypeUnit) ]) ],
        VTypePair (VTypeInt, VTypeCustom "my_bool"),
        [
          PatPair
            ( (VTypePair (VTypeInt, VTypeCustom "my_bool"), ()),
              PatName ((VTypeInt, ()), "x", VTypeInt),
              PatConstructor
                ( (VTypeCustom "my_bool", ()),
                  "T",
                  PatName ((VTypeUnit, ()), "u", VTypeUnit) ) );
          PatPair
            ( (VTypePair (VTypeInt, VTypeCustom "my_bool"), ()),
              PatName ((VTypeInt, ()), "x", VTypeInt),
              PatConstructor
                ( (VTypeCustom "my_bool", ()),
                  "F",
                  PatName ((VTypeUnit, ()), "u", VTypeUnit) ) );
        ] );
      ( "A x | B x | C x",
        CustomType.
          [
            VariantType
              ("t", [ ("A", VTypeInt); ("B", VTypeInt); ("C", VTypeInt) ]);
          ],
        VTypeCustom "t",
        [
          PatConstructor
            ((VTypeCustom "t", ()), "A", PatName ((VTypeInt, ()), "x", VTypeInt));
          PatConstructor
            ((VTypeCustom "t", ()), "B", PatName ((VTypeInt, ()), "x", VTypeInt));
          PatConstructor
            ((VTypeCustom "t", ()), "C", PatName ((VTypeInt, ()), "x", VTypeInt));
        ] );
      ( "(x, y)",
        [],
        VTypePair (VTypeInt, VTypeInt),
        [
          PatPair
            ( (VTypePair (VTypeInt, VTypeInt), ()),
              PatName ((VTypeInt, ()), "x", VTypeInt),
              PatName ((VTypeInt, ()), "y", VTypeInt) );
        ] );
      ( "((x, y), z)",
        [],
        VTypePair (VTypePair (VTypeInt, VTypeInt), VTypeInt),
        [
          PatPair
            ( (VTypePair (VTypePair (VTypeInt, VTypeInt), VTypeInt), ()),
              PatPair
                ( (VTypePair (VTypeInt, VTypeInt), ()),
                  PatName ((VTypeInt, ()), "x", VTypeInt),
                  PatName ((VTypeInt, ()), "y", VTypeInt) ),
              PatName ((VTypeInt, ()), "z", VTypeInt) );
        ] );
      ("x", [], VTypeInt, [ PatName ((VTypeInt, ()), "x", VTypeInt) ]);
      ( "A x | B (A x, C y) | B (C x, C y) | C z | B (x, C y) | B (x, A y) | x",
        CustomType.
          [
            VariantType
              ("t", [ ("A", VTypeInt); ("B", VTypeCustom "t"); ("C", VTypeInt) ]);
          ],
        VTypeCustom "t",
        [
          PatConstructor
            ((VTypeCustom "t", ()), "A", PatName ((VTypeInt, ()), "x", VTypeInt));
          PatConstructor
            ( (VTypeCustom "t", ()),
              "B",
              PatPair
                ( (VTypePair (VTypeCustom "t", VTypeCustom "t"), ()),
                  PatConstructor
                    ( (VTypeCustom "t", ()),
                      "A",
                      PatName ((VTypeInt, ()), "x", VTypeInt) ),
                  PatConstructor
                    ( (VTypeCustom "t", ()),
                      "C",
                      PatName ((VTypeInt, ()), "y", VTypeInt) ) ) );
          PatConstructor
            ( (VTypeCustom "t", ()),
              "B",
              PatPair
                ( (VTypePair (VTypeCustom "t", VTypeCustom "t"), ()),
                  PatConstructor
                    ( (VTypeCustom "t", ()),
                      "C",
                      PatName ((VTypeInt, ()), "x", VTypeInt) ),
                  PatConstructor
                    ( (VTypeCustom "t", ()),
                      "C",
                      PatName ((VTypeInt, ()), "y", VTypeInt) ) ) );
          PatConstructor
            ((VTypeCustom "t", ()), "C", PatName ((VTypeInt, ()), "z", VTypeInt));
          PatConstructor
            ( (VTypeCustom "t", ()),
              "B",
              PatPair
                ( (VTypePair (VTypeCustom "t", VTypeCustom "t"), ()),
                  PatName ((VTypeInt, ()), "x", VTypeInt),
                  PatConstructor
                    ( (VTypeCustom "t", ()),
                      "C",
                      PatName ((VTypeInt, ()), "y", VTypeInt) ) ) );
          PatConstructor
            ( (VTypeCustom "t", ()),
              "B",
              PatPair
                ( (VTypePair (VTypeCustom "t", VTypeCustom "t"), ()),
                  PatName ((VTypeInt, ()), "x", VTypeInt),
                  PatConstructor
                    ( (VTypeCustom "t", ()),
                      "A",
                      PatName ((VTypeInt, ()), "y", VTypeInt) ) ) );
          PatName ((VTypeCustom "t", ()), "x", VTypeCustom "t");
        ] );
    ]

let create_test_expr_eval
    ({ name; custom_types; arg_t; cases } : (unit, unit) test_case) :
    unit Alcotest.test_case =
  let module TypeChecker = TestingTypeChecker in
  let module TypeCtx = TypeChecker.TypeCtx in
  let module VarCtx = TypeChecker.VarCtx in
  (* Create the type context *)
  ( TypeCtx.create ~custom_types |> function
    | Error err ->
        Alcotest.failf "Typing error creating type context: %s"
          (TypeChecker.TypingError.print err)
    | Ok type_ctx -> type_ctx )
  |> fun type_ctx ->
  ( TypeChecker.check_type_ctx type_ctx |> function
    | Error err ->
        Alcotest.failf "Typing error checking type context: %s"
          (TypeChecker.TypingError.print err)
    | Ok x -> x )
  |> fun checked_type_ctx ->
  (* Helper *)
  let expr_to_prog (type tag_e tag_p)
      ~(map_ct_val_e : Vtype.t * unit -> tag_e * tag_p)
      (e : (tag_e, tag_p) StdExpr.t) : (tag_e, tag_p) StdProgram.t =
    {
      custom_types =
        List.map custom_types ~f:(fun ct ->
            Program.
              {
                private_flag = Public;
                ct =
                  ct
                  |> CustomType.fmap_expr ~f:(Fn.compose fst map_ct_val_e)
                  |> CustomType.fmap_pattern ~f:(Fn.compose snd map_ct_val_e);
              });
      top_level_defns = [];
      body = Some e;
    }
  in
  (* Argument expression generator *)
  let arg_expr_gen =
    let open QCheck.Gen in
    Unit_expr_qcheck_testing.gen
      {
        t =
          Unit_expr_qcheck_testing.vtype_to_gen_vtype_unsafe arg_t
          |> Option.some;
        variant_types =
          type_ctx |> TypeCtx.type_defns_to_ordered_list
          |> List.filter_map
               ~f:
                 CustomType.(
                   function QuotientType _ -> None | VariantType vt -> Some vt);
        top_level_defns = [];
        v_gen = QCheck.Gen.unit;
        pat_v_gen = QCheck.Gen.unit;
        mrd = default_max_gen_rec_depth;
      }
    >>= fun e ->
    TypeChecker.type_expr ~get_source_position:(Fn.const None)
      (checked_type_ctx, VarCtx.empty)
      e
    |> ( function
    | Error err ->
        Alcotest.failf "Typing error when typing generated expression: %s"
          (TypeChecker.TypingError.print err)
    | Ok x -> x )
    |> return
  in

  (* The arbitrary generator for the input program pairs *)
  let arb_inp : StdExpr.plain_typed_t QCheck.arbitrary =
    QCheck.make
      ~print:(Plain_typed_expr_qcheck_testing.print PrintExprSource)
      ~shrink:(Plain_typed_expr_qcheck_testing.shrink { preserve_type = true })
      arg_expr_gen
  in
  QCheck_alcotest.to_alcotest
    (QCheck.Test.make
       ~count:(* TODO - increase this count once debugging done *) 10 ~name
       arb_inp (fun match_arg ->
         let module Executor = Pq_lang.ProgramExecutor.MakeStd (TypeChecker) in
         let match_return_t = Vtype.VTypeInt in
         let cases =
           Nonempty_list.mapi cases ~f:(fun i case_p ->
               (case_p, StdExpr.IntLit ((Vtype.VTypeInt, ()), i)))
         in
         let orig_match_expr =
           StdExpr.Match ((match_return_t, ()), match_arg, match_return_t, cases)
         in
      let existing_names = StdExpr.existing_names orig_match_expr in
      let existing_names, flattened_match_expr_std =
        ( orig_match_expr |> Flattener.flatten_expr ~existing_names ~type_ctx
        |> function
          | Error err ->
              Alcotest.failf "Failed to flatten expression: %s"
                (Pq_lang.Flattener.print_flattening_error err)
          | Ok flattened_expr -> flattened_expr )
        |> fun (existing_names, flat_e) ->
        (existing_names, FlatPattern.to_std_expr flat_e)
      in
      let get_typed_prog ~name e =
        expr_to_prog
          ~map_ct_val_e:(Fn.const ((), ()))
          (e
          |> StdExpr.fmap ~f:(Fn.const ())
          |> StdExpr.fmap_pattern ~f:(Fn.const ()))
        |> TypeChecker.type_program ~get_source_position:(Fn.const None)
        |> function
        | Error err ->
            Alcotest.failf "Typing error when typing %s program: %s" name
              (TypeChecker.TypingError.print err)
        | Ok x -> x
      in
      let orig_typed_prog : (unit, unit) TestingTypeChecker.typed_program =
        get_typed_prog ~name:"original" orig_match_expr
      in
         let flattened_typed_prog :
             (unit, unit) TestingTypeChecker.typed_program =
        get_typed_prog ~name:"flattened" flattened_match_expr_std
      in
      let _ = existing_names in
         let orig_res = Executor.execute_program orig_typed_prog in
         let flattened_res = Executor.execute_program flattened_typed_prog in

         if Executor.equal_exec_res orig_res flattened_res then true
         else
           QCheck.Test.fail_reportf
             "Got different execution results:\n\nExpected: %s\n\nActual: %s"
             (Executor.show_exec_res orig_res)
             (Executor.show_exec_res flattened_res)))

let suite : unit Alcotest.test_case list =
  label_tests "Result of evaluating flattened version"
    (List.map ~f:create_test_expr_eval sample_test_cases)
