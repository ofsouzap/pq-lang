open Core
open OUnit2
open Pq_lang
open Utils
open Pattern
open Ast
open Quotient_types
open Program
open Parser
open Frontend
open Testing_utils

type test_case_no_custom_types =
  string * string * token list * (plain_expr, frontend_error) Result.t

type test_case_full_prog =
  string * string * token list * (plain_program, frontend_error) Result.t

type test_case_precedence = string * string * Ast.plain_expr

let test_cases_unit_value : test_case_no_custom_types list =
  List.map
    ~f:(fun (x, y, z) -> (x, x, y, z))
    [
      ("()", [ UNIT_VAL ], Ok (UnitLit ()));
      ( "() + 5",
        [ UNIT_VAL; PLUS; INTLIT 5 ],
        Ok (Add ((), UnitLit (), IntLit ((), 5))) );
    ]

let test_cases_arithmetic : test_case_no_custom_types list =
  List.map
    ~f:(fun (x, y, z) -> (x, x, y, z))
    [
      ("1", [ INTLIT 1 ], Ok (IntLit ((), 1)));
      ("0", [ INTLIT 0 ], Ok (IntLit ((), 0)));
      ("-5", [ MINUS; INTLIT 5 ], Ok (Neg ((), IntLit ((), 5))));
      ( "1 + 2",
        [ INTLIT 1; PLUS; INTLIT 2 ],
        Ok (Add ((), IntLit ((), 1), IntLit ((), 2))) );
      ( "1 - 2",
        [ INTLIT 1; MINUS; INTLIT 2 ],
        Ok (Subtr ((), IntLit ((), 1), IntLit ((), 2))) );
      ( "1 + -2",
        [ INTLIT 1; PLUS; MINUS; INTLIT 2 ],
        Ok (Add ((), IntLit ((), 1), Neg ((), IntLit ((), 2)))) );
      ( "-1 + 2",
        [ MINUS; INTLIT 1; PLUS; INTLIT 2 ],
        Ok (Add ((), Neg ((), IntLit ((), 1)), IntLit ((), 2))) );
      ( "-1 + -2",
        [ MINUS; INTLIT 1; PLUS; MINUS; INTLIT 2 ],
        Ok (Add ((), Neg ((), IntLit ((), 1)), Neg ((), IntLit ((), 2)))) );
      ( "1 * 2",
        [ INTLIT 1; STAR; INTLIT 2 ],
        Ok (Mult ((), IntLit ((), 1), IntLit ((), 2))) );
      ( "1 * -2",
        [ INTLIT 1; STAR; MINUS; INTLIT 2 ],
        Ok (Mult ((), IntLit ((), 1), Neg ((), IntLit ((), 2)))) );
      ( "1    + 4 * (1+2 )",
        [
          INTLIT 1;
          PLUS;
          INTLIT 4;
          STAR;
          LPAREN;
          INTLIT 1;
          PLUS;
          INTLIT 2;
          RPAREN;
        ],
        Ok
          (Add
             ( (),
               IntLit ((), 1),
               Mult
                 ((), IntLit ((), 4), Add ((), IntLit ((), 1), IntLit ((), 2)))
             )) );
      ( "(1 + 2) * 3",
        [ LPAREN; INTLIT 1; PLUS; INTLIT 2; RPAREN; STAR; INTLIT 3 ],
        Ok (Mult ((), Add ((), IntLit ((), 1), IntLit ((), 2)), IntLit ((), 3)))
      );
      ( "(1 + 2) * (3 + 4)",
        [
          LPAREN;
          INTLIT 1;
          PLUS;
          INTLIT 2;
          RPAREN;
          STAR;
          LPAREN;
          INTLIT 3;
          PLUS;
          INTLIT 4;
          RPAREN;
        ],
        Ok
          (Mult
             ( (),
               Add ((), IntLit ((), 1), IntLit ((), 2)),
               Add ((), IntLit ((), 3), IntLit ((), 4)) )) );
      ("+**+", [ PLUS; STAR; STAR; PLUS ], Error ParsingError);
    ]

let test_cases_booleans : test_case_no_custom_types list =
  List.map
    ~f:(fun (x, y, z) -> (x, x, y, z))
    [
      ("true", [ TRUE ], Ok (BoolLit ((), true)));
      ("false", [ FALSE ], Ok (BoolLit ((), false)));
      ("~true", [ BNOT; TRUE ], Ok (BNot ((), BoolLit ((), true))));
      ( "true && false",
        [ TRUE; BAND; FALSE ],
        Ok (BAnd ((), BoolLit ((), true), BoolLit ((), false))) );
      ( "true || false",
        [ TRUE; BOR; FALSE ],
        Ok (BOr ((), BoolLit ((), true), BoolLit ((), false))) );
      ( "true == false",
        [ TRUE; EQUATE; FALSE ],
        Ok (Eq ((), BoolLit ((), true), BoolLit ((), false))) );
      ( "true == false || true",
        [ TRUE; EQUATE; FALSE; BOR; TRUE ],
        Ok
          (BOr
             ( (),
               Eq ((), BoolLit ((), true), BoolLit ((), false)),
               BoolLit ((), true) )) );
    ]

let test_cases_pairs : test_case_no_custom_types list =
  List.map
    ~f:(fun (x, y, z) -> (x, x, y, z))
    [
      ( "(1,2)",
        [ LPAREN; INTLIT 1; COMMA; INTLIT 2; RPAREN ],
        Ok (Pair ((), IntLit ((), 1), IntLit ((), 2))) );
      ( "(true, 1)",
        [ LPAREN; TRUE; COMMA; INTLIT 1; RPAREN ],
        Ok (Pair ((), BoolLit ((), true), IntLit ((), 1))) );
      ( "(3, false)",
        [ LPAREN; INTLIT 3; COMMA; FALSE; RPAREN ],
        Ok (Pair ((), IntLit ((), 3), BoolLit ((), false))) );
      ( "((3, 4), 5)",
        [
          LPAREN;
          LPAREN;
          INTLIT 3;
          COMMA;
          INTLIT 4;
          RPAREN;
          COMMA;
          INTLIT 5;
          RPAREN;
        ],
        Ok
          (Pair ((), Pair ((), IntLit ((), 3), IntLit ((), 4)), IntLit ((), 5)))
      );
    ]

let test_cases_integer_comparisons : test_case_no_custom_types list =
  List.map
    ~f:(fun (x, y, z) -> (x, x, y, z))
    [
      ( "0 == 0",
        [ INTLIT 0; EQUATE; INTLIT 0 ],
        Ok (Eq ((), IntLit ((), 0), IntLit ((), 0))) );
      ( "1 > 0",
        [ INTLIT 1; GT; INTLIT 0 ],
        Ok (Gt ((), IntLit ((), 1), IntLit ((), 0))) );
      ( "0 >= 0",
        [ INTLIT 0; GTEQ; INTLIT 0 ],
        Ok (GtEq ((), IntLit ((), 0), IntLit ((), 0))) );
      ( "0 < 0",
        [ INTLIT 0; LT; INTLIT 0 ],
        Ok (Lt ((), IntLit ((), 0), IntLit ((), 0))) );
      ( "0 <= 1",
        [ INTLIT 0; LTEQ; INTLIT 1 ],
        Ok (LtEq ((), IntLit ((), 0), IntLit ((), 1))) );
      ("== <=", [ EQUATE; LTEQ ], Error ParsingError);
    ]

let test_cases_if_then_else : test_case_no_custom_types list =
  List.map
    ~f:(fun (x, y, z) -> (x, x, y, z))
    [
      ( "if true then 1 else 2 end",
        [ IF; TRUE; THEN; INTLIT 1; ELSE; INTLIT 2; END ],
        Ok (If ((), BoolLit ((), true), IntLit ((), 1), IntLit ((), 2))) );
      ( "if true then 1 else if false then 2 else 3 end end",
        [
          IF;
          TRUE;
          THEN;
          INTLIT 1;
          ELSE;
          IF;
          FALSE;
          THEN;
          INTLIT 2;
          ELSE;
          INTLIT 3;
          END;
          END;
        ],
        Ok
          (If
             ( (),
               BoolLit ((), true),
               IntLit ((), 1),
               If ((), BoolLit ((), false), IntLit ((), 2), IntLit ((), 3)) ))
      );
      ( "if true then (if false then 1 else 2 end) else 3 end",
        [
          IF;
          TRUE;
          THEN;
          LPAREN;
          IF;
          FALSE;
          THEN;
          INTLIT 1;
          ELSE;
          INTLIT 2;
          END;
          RPAREN;
          ELSE;
          INTLIT 3;
          END;
        ],
        Ok
          (If
             ( (),
               BoolLit ((), true),
               If ((), BoolLit ((), false), IntLit ((), 1), IntLit ((), 2)),
               IntLit ((), 3) )) );
      ( "if true then if false then 1 else 2 end else 3 end",
        [
          IF;
          TRUE;
          THEN;
          IF;
          FALSE;
          THEN;
          INTLIT 1;
          ELSE;
          INTLIT 2;
          END;
          ELSE;
          INTLIT 3;
          END;
        ],
        Ok
          (If
             ( (),
               BoolLit ((), true),
               If ((), BoolLit ((), false), IntLit ((), 1), IntLit ((), 2)),
               IntLit ((), 3) )) );
    ]

let test_cases_variables : test_case_no_custom_types list =
  List.map
    ~f:(fun (x, y, z) -> (x, x, y, z))
    [
      ("x", [ LNAME "x" ], Ok (Var ((), "x")));
      ("one_thing", [ LNAME "one_thing" ], Ok (Var ((), "one_thing")));
      ( "let x = 1 in x end",
        [ LET; LNAME "x"; ASSIGN; INTLIT 1; IN; LNAME "x"; END ],
        Ok (Let ((), "x", IntLit ((), 1), Var ((), "x"))) );
      ( "let x = 1 in x end",
        [ LET; LNAME "x"; ASSIGN; INTLIT 1; IN; LNAME "x"; END ],
        Ok (Let ((), "x", IntLit ((), 1), Var ((), "x"))) );
      ( "let x = 1 in let y in x end",
        [
          LET;
          LNAME "x";
          ASSIGN;
          INTLIT 1;
          IN;
          LET;
          LNAME "y";
          IN;
          LNAME "x";
          END;
        ],
        Error ParsingError );
      ( "let f = fun (x : int) -> true end in f 1 end",
        [
          LET;
          LNAME "f";
          ASSIGN;
          FUN;
          LPAREN;
          LNAME "x";
          COLON;
          INT;
          RPAREN;
          ARROW;
          TRUE;
          END;
          IN;
          LNAME "f";
          INTLIT 1;
          END;
        ],
        Ok
          (Let
             ( (),
               "f",
               Fun ((), ("x", VTypeInt), BoolLit ((), true)),
               App ((), Var ((), "f"), IntLit ((), 1)) )) );
    ]

let test_cases_functions : test_case_no_custom_types list =
  List.map
    ~f:(fun (x, y, z) -> (x, x, y, z))
    [
      ( "fun (x : int) -> x end",
        [ FUN; LPAREN; LNAME "x"; COLON; INT; RPAREN; ARROW; LNAME "x"; END ],
        Ok (Fun ((), ("x", VTypeInt), Var ((), "x"))) );
      ( "fun (x : int) -> x + 1 end",
        [
          FUN;
          LPAREN;
          LNAME "x";
          COLON;
          INT;
          RPAREN;
          ARROW;
          LNAME "x";
          PLUS;
          INTLIT 1;
          END;
        ],
        Ok (Fun ((), ("x", VTypeInt), Add ((), Var ((), "x"), IntLit ((), 1))))
      );
      ( "fun (x : int) -> fun (y : int) -> x + y end end",
        [
          FUN;
          LPAREN;
          LNAME "x";
          COLON;
          INT;
          RPAREN;
          ARROW;
          FUN;
          LPAREN;
          LNAME "y";
          COLON;
          INT;
          RPAREN;
          ARROW;
          LNAME "x";
          PLUS;
          LNAME "y";
          END;
          END;
        ],
        Ok
          (Fun
             ( (),
               ("x", VTypeInt),
               Fun ((), ("y", VTypeInt), Add ((), Var ((), "x"), Var ((), "y")))
             )) );
      ( "(fun (x : int) -> x + 1 end) 4",
        [
          LPAREN;
          FUN;
          LPAREN;
          LNAME "x";
          COLON;
          INT;
          RPAREN;
          ARROW;
          LNAME "x";
          PLUS;
          INTLIT 1;
          END;
          RPAREN;
          INTLIT 4;
        ],
        Ok
          (App
             ( (),
               Fun ((), ("x", VTypeInt), Add ((), Var ((), "x"), IntLit ((), 1))),
               IntLit ((), 4) )) );
      ( "x 5",
        [ LNAME "x"; INTLIT 5 ],
        Ok (App ((), Var ((), "x"), IntLit ((), 5))) );
      ( "x y",
        [ LNAME "x"; LNAME "y" ],
        Ok (App ((), Var ((), "x"), Var ((), "y"))) );
      ( "(fun (b : bool) -> fun (x : int) -> fun (y : int) -> if b then x else \
         y end end end end ) true 1 2",
        [
          LPAREN;
          FUN;
          LPAREN;
          LNAME "b";
          COLON;
          BOOL;
          RPAREN;
          ARROW;
          FUN;
          LPAREN;
          LNAME "x";
          COLON;
          INT;
          RPAREN;
          ARROW;
          FUN;
          LPAREN;
          LNAME "y";
          COLON;
          INT;
          RPAREN;
          ARROW;
          IF;
          LNAME "b";
          THEN;
          LNAME "x";
          ELSE;
          LNAME "y";
          END;
          END;
          END;
          END;
          RPAREN;
          TRUE;
          INTLIT 1;
          INTLIT 2;
        ],
        Ok
          (App
             ( (),
               App
                 ( (),
                   App
                     ( (),
                       Fun
                         ( (),
                           ("b", VTypeBool),
                           Fun
                             ( (),
                               ("x", VTypeInt),
                               Fun
                                 ( (),
                                   ("y", VTypeInt),
                                   If
                                     ( (),
                                       Var ((), "b"),
                                       Var ((), "x"),
                                       Var ((), "y") ) ) ) ),
                       BoolLit ((), true) ),
                   IntLit ((), 1) ),
               IntLit ((), 2) )) );
      ( "f1 f2 f3",
        [ LNAME "f1"; LNAME "f2"; LNAME "f3" ],
        Ok (App ((), App ((), Var ((), "f1"), Var ((), "f2")), Var ((), "f3")))
      );
    ]

let test_cases_recursion : test_case_no_custom_types list =
  List.map
    ~f:(fun (x, y, z) -> (x, x, y, z))
    [
      ( "let rec (f : int -> int) = fun (x : int) -> if x == 0 then 0 else x + \
         f (x - 1) end end in f 5 end",
        [
          LET;
          REC;
          LPAREN;
          LNAME "f";
          COLON;
          INT;
          ARROW;
          INT;
          RPAREN;
          ASSIGN;
          FUN;
          LPAREN;
          LNAME "x";
          COLON;
          INT;
          RPAREN;
          ARROW;
          IF;
          LNAME "x";
          EQUATE;
          INTLIT 0;
          THEN;
          INTLIT 0;
          ELSE;
          LNAME "x";
          PLUS;
          LNAME "f";
          LPAREN;
          LNAME "x";
          MINUS;
          INTLIT 1;
          RPAREN;
          END;
          END;
          IN;
          LNAME "f";
          INTLIT 5;
          END;
        ],
        Ok
          (Let
             ( (),
               "f",
               Fix
                 ( (),
                   ("f", VTypeInt, VTypeInt),
                   ("x", VTypeInt),
                   If
                     ( (),
                       Eq ((), Var ((), "x"), IntLit ((), 0)),
                       IntLit ((), 0),
                       Add
                         ( (),
                           Var ((), "x"),
                           App
                             ( (),
                               Var ((), "f"),
                               Subtr ((), Var ((), "x"), IntLit ((), 1)) ) ) )
                 ),
               App ((), Var ((), "f"), IntLit ((), 5)) )) );
      ( "let rec (f : int -> int) = 2 in f end",
        [
          LET;
          REC;
          LPAREN;
          LNAME "f";
          COLON;
          INT;
          ARROW;
          INT;
          RPAREN;
          ASSIGN;
          INTLIT 2;
          IN;
          LNAME "f";
          END;
        ],
        Error ParsingError );
      ( "let rec (f : int -> int) = fun (y : int) -> y end in f 5 end",
        [
          LET;
          REC;
          LPAREN;
          LNAME "f";
          COLON;
          INT;
          ARROW;
          INT;
          RPAREN;
          ASSIGN;
          FUN;
          LPAREN;
          LNAME "y";
          COLON;
          INT;
          RPAREN;
          ARROW;
          LNAME "y";
          END;
          IN;
          LNAME "f";
          INTLIT 5;
          END;
        ],
        Ok
          (Let
             ( (),
               "f",
               Fix
                 ((), ("f", VTypeInt, VTypeInt), ("y", VTypeInt), Var ((), "y")),
               App ((), Var ((), "f"), IntLit ((), 5)) )) );
    ]

let test_cases_match : test_case_no_custom_types list =
  List.map
    ~f:(fun (x, y, z) -> (x, x, y, z))
    [
      ( (* Simple match without leading pipe *)
        "match x with (y : int) -> y end",
        [
          MATCH;
          LNAME "x";
          WITH;
          LPAREN;
          LNAME "y";
          COLON;
          INT;
          RPAREN;
          ARROW;
          LNAME "y";
          END;
        ],
        Ok
          (Match
             ( (),
               Var ((), "x"),
               Nonempty_list.from_list_unsafe
                 [ (PatName ("y", VTypeInt), Var ((), "y")) ] )) );
      ( (* Simple match with leading pipe *)
        "match x with | (y : int) -> y end",
        [
          MATCH;
          LNAME "x";
          WITH;
          PIPE;
          LPAREN;
          LNAME "y";
          COLON;
          INT;
          RPAREN;
          ARROW;
          LNAME "y";
          END;
        ],
        Ok
          (Match
             ( (),
               Var ((), "x"),
               Nonempty_list.from_list_unsafe
                 [ (PatName ("y", VTypeInt), Var ((), "y")) ] )) );
      ( (* Match with compound inner expression *)
        "match (1 + (if true then x else 4 end)) with (y : int) -> y end",
        [
          MATCH;
          LPAREN;
          INTLIT 1;
          PLUS;
          LPAREN;
          IF;
          TRUE;
          THEN;
          LNAME "x";
          ELSE;
          INTLIT 4;
          END;
          RPAREN;
          RPAREN;
          WITH;
          LPAREN;
          LNAME "y";
          COLON;
          INT;
          RPAREN;
          ARROW;
          LNAME "y";
          END;
        ],
        Ok
          (Match
             ( (),
               Add
                 ( (),
                   IntLit ((), 1),
                   If ((), BoolLit ((), true), Var ((), "x"), IntLit ((), 4)) ),
               Nonempty_list.from_list_unsafe
                 [ (PatName ("y", VTypeInt), Var ((), "y")) ] )) );
      ( (* Match with multiple patterns *)
        "match x with (y : int) -> y | (z : int) -> z end",
        [
          MATCH;
          LNAME "x";
          WITH;
          LPAREN;
          LNAME "y";
          COLON;
          INT;
          RPAREN;
          ARROW;
          LNAME "y";
          PIPE;
          LPAREN;
          LNAME "z";
          COLON;
          INT;
          RPAREN;
          ARROW;
          LNAME "z";
          END;
        ],
        Ok
          (Match
             ( (),
               Var ((), "x"),
               Nonempty_list.from_list_unsafe
                 [
                   (PatName ("y", VTypeInt), Var ((), "y"));
                   (PatName ("z", VTypeInt), Var ((), "z"));
                 ] )) );
      ( (* Matching pair *)
        "match x with ((y : int), (z : bool)) -> y end",
        [
          MATCH;
          LNAME "x";
          WITH;
          LPAREN;
          LPAREN;
          LNAME "y";
          COLON;
          INT;
          RPAREN;
          COMMA;
          LPAREN;
          LNAME "z";
          COLON;
          BOOL;
          RPAREN;
          RPAREN;
          ARROW;
          LNAME "y";
          END;
        ],
        Ok
          (Match
             ( (),
               Var ((), "x"),
               Nonempty_list.from_list_unsafe
                 [
                   ( PatPair (PatName ("y", VTypeInt), PatName ("z", VTypeBool)),
                     Var ((), "y") );
                 ] )) );
      ( (* Matching nested pair *)
        "match x with ((y : bool), ((z1 : bool), (z2 : bool))) -> if y then z1 \
         else z2 end end",
        [
          MATCH;
          LNAME "x";
          WITH;
          LPAREN;
          LPAREN;
          LNAME "y";
          COLON;
          BOOL;
          RPAREN;
          COMMA;
          LPAREN;
          LPAREN;
          LNAME "z1";
          COLON;
          BOOL;
          RPAREN;
          COMMA;
          LPAREN;
          LNAME "z2";
          COLON;
          BOOL;
          RPAREN;
          RPAREN;
          RPAREN;
          ARROW;
          IF;
          LNAME "y";
          THEN;
          LNAME "z1";
          ELSE;
          LNAME "z2";
          END;
          END;
        ],
        Ok
          (Match
             ( (),
               Var ((), "x"),
               Nonempty_list.from_list_unsafe
                 [
                   ( PatPair
                       ( PatName ("y", VTypeBool),
                         PatPair
                           (PatName ("z1", VTypeBool), PatName ("z2", VTypeBool))
                       ),
                     If ((), Var ((), "y"), Var ((), "z1"), Var ((), "z2")) );
                 ] )) );
      ( (* Matching custom data type constructor *)
        "match x with (Nil (z : int)) -> 0 | (Cons ((h : int), (ts : \
         int_list))) -> 1 end",
        [
          MATCH;
          LNAME "x";
          WITH;
          LPAREN;
          UNAME "Nil";
          LPAREN;
          LNAME "z";
          COLON;
          INT;
          RPAREN;
          RPAREN;
          ARROW;
          INTLIT 0;
          PIPE;
          LPAREN;
          UNAME "Cons";
          LPAREN;
          LPAREN;
          LNAME "h";
          COLON;
          INT;
          RPAREN;
          COMMA;
          LPAREN;
          LNAME "ts";
          COLON;
          LNAME "int_list";
          RPAREN;
          RPAREN;
          RPAREN;
          ARROW;
          INTLIT 1;
          END;
        ],
        Ok
          (Match
             ( (),
               Var ((), "x"),
               Nonempty_list.from_list_unsafe
                 [
                   ( PatConstructor ("Nil", PatName ("z", VTypeInt)),
                     IntLit ((), 0) );
                   ( PatConstructor
                       ( "Cons",
                         PatPair
                           ( PatName ("h", VTypeInt),
                             PatName ("ts", VTypeCustom "int_list") ) ),
                     IntLit ((), 1) );
                 ] )) );
    ]

let test_cases_custom_type_defn : test_case_full_prog list =
  List.map
    ~f:(fun (x, y, z) -> (x, x, y, z))
    [
      ( (* No type definition *)
        {|
1
|},
        [ INTLIT 1 ],
        Ok { type_defns = []; e = IntLit ((), 1) } );
      ( (* Simple type definition *)
        {|
        type int_or_bool = Int of int | Bool of bool

        1
        |},
        [
          TYPE;
          LNAME "int_or_bool";
          ASSIGN;
          UNAME "Int";
          OF;
          INT;
          PIPE;
          UNAME "Bool";
          OF;
          BOOL;
          INTLIT 1;
        ],
        Ok
          {
            type_defns =
              [
                CustomType
                  ("int_or_bool", [ ("Int", VTypeInt); ("Bool", VTypeBool) ]);
              ];
            e = IntLit ((), 1);
          } );
      ( (* Simple type definition (with leading pipe) *)
        {|
        type int_or_bool = | Int of int | Bool of bool

        1
        |},
        [
          TYPE;
          LNAME "int_or_bool";
          ASSIGN;
          PIPE;
          UNAME "Int";
          OF;
          INT;
          PIPE;
          UNAME "Bool";
          OF;
          BOOL;
          INTLIT 1;
        ],
        Ok
          {
            type_defns =
              [
                CustomType
                  ("int_or_bool", [ ("Int", VTypeInt); ("Bool", VTypeBool) ]);
              ];
            e = IntLit ((), 1);
          } );
      ( (* Incorrectly using upper-name for type name *)
        {|
        type Int_or_bool = Int of int | Bool of bool

        1
        |},
        [
          TYPE;
          UNAME "Int_or_bool";
          ASSIGN;
          UNAME "Int";
          OF;
          INT;
          PIPE;
          UNAME "Bool";
          OF;
          BOOL;
          INTLIT 1;
        ],
        Error ParsingError );
      ( (* Incorrectly using lower-name for constructor name *)
        {|
        type int_or_bool = Int of int | thisisabool of bool

        1
        |},
        [
          TYPE;
          LNAME "int_or_bool";
          ASSIGN;
          UNAME "Int";
          OF;
          INT;
          PIPE;
          LNAME "thisisabool";
          OF;
          BOOL;
          INTLIT 1;
        ],
        Error ParsingError );
      ( (* Recursive type definition *)
        {|
        type int_list = Nil of unit | Cons of int * int_list

        1
        |},
        [
          TYPE;
          LNAME "int_list";
          ASSIGN;
          UNAME "Nil";
          OF;
          UNIT;
          PIPE;
          UNAME "Cons";
          OF;
          INT;
          STAR;
          LNAME "int_list";
          INTLIT 1;
        ],
        Ok
          {
            type_defns =
              [
                CustomType
                  ( "int_list",
                    [
                      ("Nil", VTypeUnit);
                      ("Cons", VTypePair (VTypeInt, VTypeCustom "int_list"));
                    ] );
              ];
            e = IntLit ((), 1);
          } );
    ]

let test_cases_quotient_type_defn : test_case_full_prog list =
  [
    ( "Single variable binding",
      {|
        type int_box = Int of int

qtype int_boxed
  = int_box
  |/ (x : int) => Int (x : int) == (x)

1
|},
      [
        TYPE;
        LNAME "int_box";
        ASSIGN;
        UNAME "Int";
        OF;
        INT;
        QTYPE;
        LNAME "int_boxed";
        ASSIGN;
        LNAME "int_box";
        QUOTIENT;
        LPAREN;
        LNAME "x";
        COLON;
        INT;
        RPAREN;
        BIG_ARROW;
        UNAME "Int";
        LPAREN;
        LNAME "x";
        COLON;
        INT;
        RPAREN;
        EQUATE;
        LPAREN;
        LNAME "x";
        RPAREN;
        INTLIT 1;
      ],
      Ok
        {
          type_defns =
            [
              CustomType ("int_box", [ ("Int", VTypeInt) ]);
              QuotientType
                {
                  name = "int_boxed";
                  base_type_name = "int_box";
                  eqconss =
                    [
                      {
                        bindings = [ ("x", VTypeInt) ];
                        body =
                          ( PatConstructor ("Int", PatName ("x", VTypeInt)),
                            Var ((), "x") );
                      };
                    ];
                };
            ];
          e = IntLit ((), 1);
        } );
    ( "Multiple variable bindings",
      {|
        type tree = Leaf of int | Node of tree * tree

qtype mobile
  = tree
  |/ (l : tree) -> (r : tree) => Node ((l : tree), (r : tree)) == (Node (r, l))

1
|},
      [
        TYPE;
        LNAME "tree";
        ASSIGN;
        UNAME "Leaf";
        OF;
        INT;
        PIPE;
        UNAME "Node";
        OF;
        LNAME "tree";
        STAR;
        LNAME "tree";
        QTYPE;
        LNAME "mobile";
        ASSIGN;
        LNAME "tree";
        QUOTIENT;
        LPAREN;
        LNAME "l";
        COLON;
        LNAME "tree";
        RPAREN;
        ARROW;
        LPAREN;
        LNAME "r";
        COLON;
        LNAME "tree";
        RPAREN;
        BIG_ARROW;
        UNAME "Node";
        LPAREN;
        LPAREN;
        LNAME "l";
        COLON;
        LNAME "tree";
        RPAREN;
        COMMA;
        LPAREN;
        LNAME "r";
        COLON;
        LNAME "tree";
        RPAREN;
        RPAREN;
        EQUATE;
        LPAREN;
        UNAME "Node";
        LPAREN;
        LNAME "r";
        COMMA;
        LNAME "l";
        RPAREN;
        RPAREN;
        INTLIT 1;
      ],
      Ok
        {
          type_defns =
            [
              CustomType
                ( "tree",
                  [
                    ("Leaf", VTypeInt);
                    ("Node", VTypePair (VTypeCustom "tree", VTypeCustom "tree"));
                  ] );
              QuotientType
                {
                  name = "mobile";
                  base_type_name = "tree";
                  eqconss =
                    [
                      {
                        bindings =
                          [
                            ("l", VTypeCustom "tree"); ("r", VTypeCustom "tree");
                          ];
                        body =
                          ( PatConstructor
                              ( "Node",
                                PatPair
                                  ( PatName ("l", VTypeCustom "tree"),
                                    PatName ("r", VTypeCustom "tree") ) ),
                            Constructor
                              ( (),
                                "Node",
                                Pair ((), Var ((), "r"), Var ((), "l")) ) );
                      };
                    ];
                };
            ];
          e = IntLit ((), 1);
        } );
    ( "Incorrectly using upper-name for type name",
      {|
        type int_box = Int of int

qtype Int_boxed
  = int_box
  |/ (x : int) => Int x == x

1
|},
      [
        TYPE;
        LNAME "int_box";
        ASSIGN;
        UNAME "Int";
        OF;
        INT;
        QTYPE;
        UNAME "Int_boxed";
        ASSIGN;
        LNAME "int_box";
        QUOTIENT;
        LPAREN;
        LNAME "x";
        COLON;
        INT;
        RPAREN;
        BIG_ARROW;
        UNAME "Int";
        LNAME "x";
        EQUATE;
        LNAME "x";
        INTLIT 1;
      ],
      Error ParsingError );
  ]

let test_cases_custom_type_referencing : test_case_no_custom_types list =
  List.map
    ~f:(fun (x, y, z) -> (x, x, y, z))
    [
      ( "fun (x : int_or_bool) -> x end",
        [
          FUN;
          LPAREN;
          LNAME "x";
          COLON;
          LNAME "int_or_bool";
          RPAREN;
          ARROW;
          LNAME "x";
          END;
        ],
        Ok (Fun ((), ("x", VTypeCustom "int_or_bool"), Var ((), "x"))) );
      ( "fun (x : Int_or_bool) -> x end",
        [
          FUN;
          LPAREN;
          LNAME "x";
          COLON;
          UNAME "Int_or_bool";
          RPAREN;
          ARROW;
          LNAME "x";
          END;
        ],
        Error ParsingError );
    ]

let test_cases_custom_type_construction : test_case_no_custom_types list =
  List.map
    ~f:(fun (x, y, z) -> (x, x, y, z))
    [
      ( "Nil ()",
        [ UNAME "Nil"; UNIT_VAL ],
        Ok (Constructor ((), "Nil", UnitLit ())) );
      ( "Meters 6",
        [ UNAME "Meters"; INTLIT 6 ],
        Ok (Constructor ((), "Meters", IntLit ((), 6))) );
      ( "Cons (1, Nil ())",
        [ UNAME "Cons"; LPAREN; INTLIT 1; COMMA; UNAME "Nil"; UNIT_VAL; RPAREN ],
        Ok
          (Constructor
             ( (),
               "Cons",
               Pair ((), IntLit ((), 1), Constructor ((), "Nil", UnitLit ())) ))
      );
    ]

let test_cases_precedence : test_case_precedence list =
  List.map
    ~f:(fun (x, z) -> (x, x, z))
    [
      ( "x + y * z",
        Add ((), Var ((), "x"), Mult ((), Var ((), "y"), Var ((), "z"))) );
      ( "x + y < z",
        Lt ((), Add ((), Var ((), "x"), Var ((), "y")), Var ((), "z")) );
      ( "x >= y - z",
        GtEq ((), Var ((), "x"), Subtr ((), Var ((), "y"), Var ((), "z"))) );
      ( "x + f y",
        Add ((), Var ((), "x"), App ((), Var ((), "f"), Var ((), "y"))) );
      ( "f x + y",
        Add ((), App ((), Var ((), "f"), Var ((), "x")), Var ((), "y")) );
    ]

let create_lexer_test ((name, inp, exp, _) : test_case_full_prog) =
  name >:: fun _ ->
  let lexbuf = Lexing.from_string inp in
  let rec collect_tokens acc =
    match Lexer.token lexbuf with
    | Parser.EOF -> List.rev acc
    | token -> collect_tokens (token :: acc)
  in
  let out = collect_tokens [] in
  assert_equal ~cmp:(equal_list Stdlib.( = )) exp out ~printer:token_printer

let create_frontend_test ((name, inp, _, exp) : test_case_full_prog) =
  name >:: fun _ ->
  let out = run_frontend_string inp in
  assert_equal
    ~cmp:(equal_result (equal_program equal_unit) equal_frontend_error)
    exp out
    ~printer:(fun x ->
      match x with
      | Ok prog -> program_to_source_code prog
      | Error (LexingError c) -> sprintf "LexingError %c" c
      | Error ParsingError -> "ParsingError")

let create_precedence_test ((name, inp, exp) : test_case_precedence) =
  name >:: fun _ ->
  let out = run_frontend_string inp in
  match out with
  | Ok prog ->
      assert_equal exp prog.e ~printer:(ast_to_source_code ~use_newlines:true)
  | Error (LexingError c) -> assert_failure (sprintf "LexingError %c" c)
  | Error ParsingError -> assert_failure "ParsingError"

let tests_no_custom_types (test_create_func : test_case_full_prog -> test) :
    test list =
  let f =
    Fn.compose test_create_func (fun (name, inp, tokens, ast) ->
        (name, inp, tokens, Result.(ast >>| fun e -> { type_defns = []; e })))
  in
  [
    "Unit Value" >::: List.map ~f test_cases_unit_value;
    "Arithmetic" >::: List.map ~f test_cases_arithmetic;
    "Booleans" >::: List.map ~f test_cases_booleans;
    "Pairs" >::: List.map ~f test_cases_pairs;
    "Integer Comparisons" >::: List.map ~f test_cases_integer_comparisons;
    "If-Then-Else" >::: List.map ~f test_cases_if_then_else;
    "Variables" >::: List.map ~f test_cases_variables;
    "Functions" >::: List.map ~f test_cases_functions;
    "Recursion" >::: List.map ~f test_cases_recursion;
    "Match" >::: List.map ~f test_cases_match;
    "Custom type referencing"
    >::: List.map ~f test_cases_custom_type_referencing;
    "Custom type construction"
    >::: List.map ~f test_cases_custom_type_construction;
  ]

let tests_full_prog (test_create_func : test_case_full_prog -> test) : test list
    =
  let f = test_create_func in
  [
    "Custom type definition" >::: List.map ~f test_cases_custom_type_defn;
    "Quotient type definition" >::: List.map ~f test_cases_quotient_type_defn;
  ]

let suite =
  "Frontend Tests"
  >::: [
         "Lexer"
         >::: tests_no_custom_types create_lexer_test
              @ tests_full_prog create_lexer_test;
         "Frontend"
         >::: tests_no_custom_types create_frontend_test
              @ tests_full_prog create_frontend_test
              @ [
                  "Precedence"
                  >::: List.map ~f:create_precedence_test test_cases_precedence;
                ];
       ]
