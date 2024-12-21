open Core
open OUnit2
open Pq_lang
open Utils
open Pattern
open Ast
open Parser
open Frontend
open Testing_utils

type test_case = string * string * token list * run_frontend_res
type test_case_precedence = string * string * Ast.plain_expr

let test_cases_arithmetic : test_case list =
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

let test_cases_booleans : test_case list =
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
        [ TRUE; EQ; FALSE ],
        Ok (Eq ((), BoolLit ((), true), BoolLit ((), false))) );
      ( "true == false || true",
        [ TRUE; EQ; FALSE; BOR; TRUE ],
        Ok
          (BOr
             ( (),
               Eq ((), BoolLit ((), true), BoolLit ((), false)),
               BoolLit ((), true) )) );
    ]

let test_cases_pairs : test_case list =
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

let test_cases_integer_comparisons : test_case list =
  List.map
    ~f:(fun (x, y, z) -> (x, x, y, z))
    [
      ( "0 == 0",
        [ INTLIT 0; EQ; INTLIT 0 ],
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
      ("== <=", [ EQ; LTEQ ], Error ParsingError);
    ]

let test_cases_if_then_else : test_case list =
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

let test_cases_variables : test_case list =
  List.map
    ~f:(fun (x, y, z) -> (x, x, y, z))
    [
      ("x", [ NAME "x" ], Ok (Var ((), "x")));
      ( "let x = 1 in x end",
        [ LET; NAME "x"; ASSIGN; INTLIT 1; IN; NAME "x"; END ],
        Ok (Let ((), "x", IntLit ((), 1), Var ((), "x"))) );
      ( "let x = 1 in x end",
        [ LET; NAME "x"; ASSIGN; INTLIT 1; IN; NAME "x"; END ],
        Ok (Let ((), "x", IntLit ((), 1), Var ((), "x"))) );
      ( "let x = 1 in let y in x end",
        [
          LET; NAME "x"; ASSIGN; INTLIT 1; IN; LET; NAME "y"; IN; NAME "x"; END;
        ],
        Error ParsingError );
      ( "let f = fun (x : int) -> true end in f 1 end",
        [
          LET;
          NAME "f";
          ASSIGN;
          FUN;
          LPAREN;
          NAME "x";
          COLON;
          INT;
          RPAREN;
          ARROW;
          TRUE;
          END;
          IN;
          NAME "f";
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

let test_cases_functions : test_case list =
  List.map
    ~f:(fun (x, y, z) -> (x, x, y, z))
    [
      ( "fun (x : int) -> x end",
        [ FUN; LPAREN; NAME "x"; COLON; INT; RPAREN; ARROW; NAME "x"; END ],
        Ok (Fun ((), ("x", VTypeInt), Var ((), "x"))) );
      ( "fun (x : int) -> x + 1 end",
        [
          FUN;
          LPAREN;
          NAME "x";
          COLON;
          INT;
          RPAREN;
          ARROW;
          NAME "x";
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
          NAME "x";
          COLON;
          INT;
          RPAREN;
          ARROW;
          FUN;
          LPAREN;
          NAME "y";
          COLON;
          INT;
          RPAREN;
          ARROW;
          NAME "x";
          PLUS;
          NAME "y";
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
          NAME "x";
          COLON;
          INT;
          RPAREN;
          ARROW;
          NAME "x";
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
        [ NAME "x"; INTLIT 5 ],
        Ok (App ((), Var ((), "x"), IntLit ((), 5))) );
      ( "x y",
        [ NAME "x"; NAME "y" ],
        Ok (App ((), Var ((), "x"), Var ((), "y"))) );
      ( "(fun (b : bool) -> fun (x : int) -> fun (y : int) -> if b then x else \
         y end end end end ) true 1 2",
        [
          LPAREN;
          FUN;
          LPAREN;
          NAME "b";
          COLON;
          BOOL;
          RPAREN;
          ARROW;
          FUN;
          LPAREN;
          NAME "x";
          COLON;
          INT;
          RPAREN;
          ARROW;
          FUN;
          LPAREN;
          NAME "y";
          COLON;
          INT;
          RPAREN;
          ARROW;
          IF;
          NAME "b";
          THEN;
          NAME "x";
          ELSE;
          NAME "y";
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
        [ NAME "f1"; NAME "f2"; NAME "f3" ],
        Ok (App ((), App ((), Var ((), "f1"), Var ((), "f2")), Var ((), "f3")))
      );
    ]

let test_cases_recursion : test_case list =
  List.map
    ~f:(fun (x, y, z) -> (x, x, y, z))
    [
      ( "let rec (f : int -> int) = fun (x : int) -> if x == 0 then 0 else x + \
         f (x - 1) end end in f 5 end",
        [
          LET;
          REC;
          LPAREN;
          NAME "f";
          COLON;
          INT;
          ARROW;
          INT;
          RPAREN;
          ASSIGN;
          FUN;
          LPAREN;
          NAME "x";
          COLON;
          INT;
          RPAREN;
          ARROW;
          IF;
          NAME "x";
          EQ;
          INTLIT 0;
          THEN;
          INTLIT 0;
          ELSE;
          NAME "x";
          PLUS;
          NAME "f";
          LPAREN;
          NAME "x";
          MINUS;
          INTLIT 1;
          RPAREN;
          END;
          END;
          IN;
          NAME "f";
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
          NAME "f";
          COLON;
          INT;
          ARROW;
          INT;
          RPAREN;
          ASSIGN;
          INTLIT 2;
          IN;
          NAME "f";
          END;
        ],
        Error ParsingError );
      ( "let rec (f : int -> int) = fun (y : int) -> y end in f 5 end",
        [
          LET;
          REC;
          LPAREN;
          NAME "f";
          COLON;
          INT;
          ARROW;
          INT;
          RPAREN;
          ASSIGN;
          FUN;
          LPAREN;
          NAME "y";
          COLON;
          INT;
          RPAREN;
          ARROW;
          NAME "y";
          END;
          IN;
          NAME "f";
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

let test_cases_match : test_case list =
  List.map
    ~f:(fun (x, y, z) -> (x, x, y, z))
    [
      ( (* Simple match without leading pipe *)
        "match x with (y : int) -> y end",
        [
          MATCH;
          NAME "x";
          WITH;
          LPAREN;
          NAME "y";
          COLON;
          INT;
          RPAREN;
          ARROW;
          NAME "y";
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
          NAME "x";
          WITH;
          PIPE;
          LPAREN;
          NAME "y";
          COLON;
          INT;
          RPAREN;
          ARROW;
          NAME "y";
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
          NAME "x";
          ELSE;
          INTLIT 4;
          END;
          RPAREN;
          RPAREN;
          WITH;
          LPAREN;
          NAME "y";
          COLON;
          INT;
          RPAREN;
          ARROW;
          NAME "y";
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
          NAME "x";
          WITH;
          LPAREN;
          NAME "y";
          COLON;
          INT;
          RPAREN;
          ARROW;
          NAME "y";
          PIPE;
          LPAREN;
          NAME "z";
          COLON;
          INT;
          RPAREN;
          ARROW;
          NAME "z";
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
          NAME "x";
          WITH;
          LPAREN;
          LPAREN;
          NAME "y";
          COLON;
          INT;
          RPAREN;
          COMMA;
          LPAREN;
          NAME "z";
          COLON;
          BOOL;
          RPAREN;
          RPAREN;
          ARROW;
          NAME "y";
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
          NAME "x";
          WITH;
          LPAREN;
          LPAREN;
          NAME "y";
          COLON;
          BOOL;
          RPAREN;
          COMMA;
          LPAREN;
          LPAREN;
          NAME "z1";
          COLON;
          BOOL;
          RPAREN;
          COMMA;
          LPAREN;
          NAME "z2";
          COLON;
          BOOL;
          RPAREN;
          RPAREN;
          RPAREN;
          ARROW;
          IF;
          NAME "y";
          THEN;
          NAME "z1";
          ELSE;
          NAME "z2";
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

let create_lexer_test ((name, inp, exp, _) : test_case) =
  name >:: fun _ ->
  let lexbuf = Lexing.from_string inp in
  let rec collect_tokens acc =
    match Lexer.token lexbuf with
    | Parser.EOF -> List.rev acc
    | token -> collect_tokens (token :: acc)
  in
  let out = collect_tokens [] in
  assert_equal exp out ~printer:token_printer

let create_frontend_test ((name, inp, _, exp) : test_case) =
  name >:: fun _ ->
  let out = run_frontend_string inp in
  assert_equal exp out ~printer:(fun x ->
      match x with
      | Ok e -> ast_to_source_code e
      | Error (LexingError c) -> sprintf "LexingError %c" c
      | Error ParsingError -> "ParsingError")

let create_precedence_test ((name, inp, exp) : test_case_precedence) =
  name >:: fun _ ->
  let out = run_frontend_string inp in
  match out with
  | Ok e -> assert_equal exp e ~printer:ast_to_source_code
  | Error (LexingError c) -> assert_failure (sprintf "LexingError %c" c)
  | Error ParsingError -> assert_failure "ParsingError"

let test_suites test_create_func =
  [
    "Arithmetic" >::: List.map ~f:test_create_func test_cases_arithmetic;
    "Booleans" >::: List.map ~f:test_create_func test_cases_booleans;
    "Pairs" >::: List.map ~f:test_create_func test_cases_pairs;
    "Integer Comparisons"
    >::: List.map ~f:test_create_func test_cases_integer_comparisons;
    "If-Then-Else" >::: List.map ~f:test_create_func test_cases_if_then_else;
    "Variables" >::: List.map ~f:test_create_func test_cases_variables;
    "Functions" >::: List.map ~f:test_create_func test_cases_functions;
    "Recursion" >::: List.map ~f:test_create_func test_cases_recursion;
    "Match" >::: List.map ~f:test_create_func test_cases_match;
  ]

let suite =
  "Frontend Tests"
  >::: [
         "Lexer" >::: test_suites create_lexer_test;
         "Frontend"
         >::: test_suites create_frontend_test
              @ [
                  "Precedence"
                  >::: List.map ~f:create_precedence_test test_cases_precedence;
                ];
       ]
