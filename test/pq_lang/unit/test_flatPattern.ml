open Core
open Pq_lang
open Utils
open Testing_utils
open Pq_lang.Expr.StdExpr
open Pq_lang.Pattern
open Pq_lang.TypeChecker.StdSimpleTypeChecker
open Pq_lang.TypeChecker.TypingError.StdTypingError

let test_cases :
    ((unit, unit) CustomType.t list * unit Pattern.typed_t list) list =
  Pattern.
    [
      ( (* (T u, x) | (F u, x) *)
        CustomType.
          [ VariantType ("my_bool", [ ("T", VTypeUnit); ("F", VTypeUnit) ]) ],
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
      ( (* (x, T u) | (x, F u) *)
        CustomType.
          [ VariantType ("my_bool", [ ("T", VTypeUnit); ("F", VTypeUnit) ]) ],
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
      ( (* A x | B x | C x *)
        CustomType.
          [
            VariantType
              ("t", [ ("A", VTypeInt); ("B", VTypeInt); ("C", VTypeInt) ]);
          ],
        [
          PatConstructor
            ((VTypeCustom "t", ()), "A", PatName ((VTypeInt, ()), "x", VTypeInt));
          PatConstructor
            ((VTypeCustom "t", ()), "B", PatName ((VTypeInt, ()), "x", VTypeInt));
          PatConstructor
            ((VTypeCustom "t", ()), "C", PatName ((VTypeInt, ()), "x", VTypeInt));
        ] );
      ( (* (x, y) *)
        [],
        [
          PatPair
            ( (VTypePair (VTypeInt, VTypeInt), ()),
              PatName ((VTypeInt, ()), "x", VTypeInt),
              PatName ((VTypeInt, ()), "y", VTypeInt) );
        ] );
      ( (* A x | B (A x, C y) | B (C x, C y) | C z | B (x, C y) | B (x, A y) | x *)
        CustomType.
          [
            VariantType
              ("t", [ ("A", VTypeInt); ("B", VTypeCustom "t"); ("C", VTypeInt) ]);
          ],
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

(* TODO - create tests and add to suite *)

let suite : unit Alcotest.test_case list = []
