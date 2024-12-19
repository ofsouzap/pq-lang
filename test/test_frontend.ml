open Core
open OUnit2
open Pq_lang
open Ast
open Parser
open Frontend
open Utils

type test_case = string * string * token list * run_frontend_res
type test_case_precedence = string * string * Ast.plain_expr

let test_cases_arithmetic : test_case list =
  List.map
    ~f:(fun (x, y, z) -> (x, x, y, z))
    [
      ("1", [ INTLIT 1 ], Res (IntLit ((), 1)));
      ("0", [ INTLIT 0 ], Res (IntLit ((), 0)));
      ("-5", [ MINUS; INTLIT 5 ], Res (Neg ((), IntLit ((), 5))));
      ( "1 + 2",
        [ INTLIT 1; PLUS; INTLIT 2 ],
        Res (Add ((), IntLit ((), 1), IntLit ((), 2))) );
      ( "1 - 2",
        [ INTLIT 1; MINUS; INTLIT 2 ],
        Res (Subtr ((), IntLit ((), 1), IntLit ((), 2))) );
      ( "1 + -2",
        [ INTLIT 1; PLUS; MINUS; INTLIT 2 ],
        Res (Add ((), IntLit ((), 1), Neg ((), IntLit ((), 2)))) );
      ( "-1 + 2",
        [ MINUS; INTLIT 1; PLUS; INTLIT 2 ],
        Res (Add ((), Neg ((), IntLit ((), 1)), IntLit ((), 2))) );
      ( "-1 + -2",
        [ MINUS; INTLIT 1; PLUS; MINUS; INTLIT 2 ],
        Res (Add ((), Neg ((), IntLit ((), 1)), Neg ((), IntLit ((), 2)))) );
      ( "1 * 2",
        [ INTLIT 1; STAR; INTLIT 2 ],
        Res (Mult ((), IntLit ((), 1), IntLit ((), 2))) );
      ( "1 * -2",
        [ INTLIT 1; STAR; MINUS; INTLIT 2 ],
        Res (Mult ((), IntLit ((), 1), Neg ((), IntLit ((), 2)))) );
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
        Res
          (Add
             ( (),
               IntLit ((), 1),
               Mult
                 ((), IntLit ((), 4), Add ((), IntLit ((), 1), IntLit ((), 2)))
             )) );
      ( "(1 + 2) * 3",
        [ LPAREN; INTLIT 1; PLUS; INTLIT 2; RPAREN; STAR; INTLIT 3 ],
        Res
          (Mult ((), Add ((), IntLit ((), 1), IntLit ((), 2)), IntLit ((), 3)))
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
        Res
          (Mult
             ( (),
               Add ((), IntLit ((), 1), IntLit ((), 2)),
               Add ((), IntLit ((), 3), IntLit ((), 4)) )) );
      ("+**+", [ PLUS; STAR; STAR; PLUS ], ParsingError);
    ]

let test_cases_booleans : test_case list =
  List.map
    ~f:(fun (x, y, z) -> (x, x, y, z))
    [
      ("true", [ TRUE ], Res (BoolLit ((), true)));
      ("false", [ FALSE ], Res (BoolLit ((), false)));
      ("~true", [ BNOT; TRUE ], Res (BNot ((), BoolLit ((), true))));
      ( "true && false",
        [ TRUE; BAND; FALSE ],
        Res (BAnd ((), BoolLit ((), true), BoolLit ((), false))) );
      ( "true || false",
        [ TRUE; BOR; FALSE ],
        Res (BOr ((), BoolLit ((), true), BoolLit ((), false))) );
      ( "true == false",
        [ TRUE; EQ; FALSE ],
        Res (Eq ((), BoolLit ((), true), BoolLit ((), false))) );
      ( "true == false || true",
        [ TRUE; EQ; FALSE; BOR; TRUE ],
        Res
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
        Res (Pair ((), IntLit ((), 1), IntLit ((), 2))) );
      ( "(true, 1)",
        [ LPAREN; TRUE; COMMA; INTLIT 1; RPAREN ],
        Res (Pair ((), BoolLit ((), true), IntLit ((), 1))) );
      ( "(3, false)",
        [ LPAREN; INTLIT 3; COMMA; FALSE; RPAREN ],
        Res (Pair ((), IntLit ((), 3), BoolLit ((), false))) );
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
        Res
          (Pair ((), Pair ((), IntLit ((), 3), IntLit ((), 4)), IntLit ((), 5)))
      );
    ]

let test_cases_integer_comparisons : test_case list =
  List.map
    ~f:(fun (x, y, z) -> (x, x, y, z))
    [
      ( "0 == 0",
        [ INTLIT 0; EQ; INTLIT 0 ],
        Res (Eq ((), IntLit ((), 0), IntLit ((), 0))) );
      ( "1 > 0",
        [ INTLIT 1; GT; INTLIT 0 ],
        Res (Gt ((), IntLit ((), 1), IntLit ((), 0))) );
      ( "0 >= 0",
        [ INTLIT 0; GTEQ; INTLIT 0 ],
        Res (GtEq ((), IntLit ((), 0), IntLit ((), 0))) );
      ( "0 < 0",
        [ INTLIT 0; LT; INTLIT 0 ],
        Res (Lt ((), IntLit ((), 0), IntLit ((), 0))) );
      ( "0 <= 1",
        [ INTLIT 0; LTEQ; INTLIT 1 ],
        Res (LtEq ((), IntLit ((), 0), IntLit ((), 1))) );
      ("== <=", [ EQ; LTEQ ], ParsingError);
    ]

let test_cases_if_then_else : test_case list =
  List.map
    ~f:(fun (x, y, z) -> (x, x, y, z))
    [
      ( "if true then 1 else 2 end",
        [ IF; TRUE; THEN; INTLIT 1; ELSE; INTLIT 2; END ],
        Res (If ((), BoolLit ((), true), IntLit ((), 1), IntLit ((), 2))) );
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
        Res
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
        Res
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
        Res
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
      ("x", [ NAME "x" ], Res (Var ((), "x")));
      ( "let x = 1 in x end",
        [ LET; NAME "x"; ASSIGN; INTLIT 1; IN; NAME "x"; END ],
        Res (Let ((), "x", IntLit ((), 1), Var ((), "x"))) );
      ( "let x = 1 in x end",
        [ LET; NAME "x"; ASSIGN; INTLIT 1; IN; NAME "x"; END ],
        Res (Let ((), "x", IntLit ((), 1), Var ((), "x"))) );
      ( "let x = 1 in let y in x end",
        [
          LET; NAME "x"; ASSIGN; INTLIT 1; IN; LET; NAME "y"; IN; NAME "x"; END;
        ],
        ParsingError );
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
        Res
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
        Res (Fun ((), ("x", VTypeInt), Var ((), "x"))) );
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
        Res (Fun ((), ("x", VTypeInt), Add ((), Var ((), "x"), IntLit ((), 1))))
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
        Res
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
        Res
          (App
             ( (),
               Fun ((), ("x", VTypeInt), Add ((), Var ((), "x"), IntLit ((), 1))),
               IntLit ((), 4) )) );
      ( "x 5",
        [ NAME "x"; INTLIT 5 ],
        Res (App ((), Var ((), "x"), IntLit ((), 5))) );
      ( "x y",
        [ NAME "x"; NAME "y" ],
        Res (App ((), Var ((), "x"), Var ((), "y"))) );
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
        Res
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
        Res (App ((), App ((), Var ((), "f1"), Var ((), "f2")), Var ((), "f3")))
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
        Res
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
        ParsingError );
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
        Res
          (Let
             ( (),
               "f",
               Fix
                 ((), ("f", VTypeInt, VTypeInt), ("y", VTypeInt), Var ((), "y")),
               App ((), Var ((), "f"), IntLit ((), 5)) )) );
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
      | Res e -> ast_to_source_code e
      | LexingError c -> sprintf "LexingError %c" c
      | ParsingError -> "ParsingError")

let create_precedence_test ((name, inp, exp) : test_case_precedence) =
  name >:: fun _ ->
  let out = run_frontend_string inp in
  match out with
  | Res e -> assert_equal exp e ~printer:ast_to_source_code
  | LexingError c -> assert_failure (sprintf "LexingError %c" c)
  | ParsingError -> assert_failure "ParsingError"

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
