open OUnit2
open Pq_lang
open Ast
open Parser
open Test_utils

type test_case = string * string * token list * Ast.expr option

let test_cases_arithmetic : test_case list =
  List.map
    (fun (x, y, z) -> (x, x, y, z))
    [
      ("1", [ INT 1 ], Some (IntLit 1));
      (* ("-3", [ MINUS, INT 3 ], Some (Neg (IntLit 3))); (* TODO - change parser to allow negation with unary minus sign *) *)
      ("1 + 2", [ INT 1; PLUS; INT 2 ], Some (Add (IntLit 1, IntLit 2)));
      ("1 * 2", [ INT 1; TIMES; INT 2 ], Some (Mult (IntLit 1, IntLit 2)));
      ( "1    + 4 * (1+2 )",
        [ INT 1; PLUS; INT 4; TIMES; LPAREN; INT 1; PLUS; INT 2; RPAREN ],
        Some (Add (IntLit 1, Mult (IntLit 4, Add (IntLit 1, IntLit 2)))) );
      ( "(1 + 2) * 3",
        [ LPAREN; INT 1; PLUS; INT 2; RPAREN; TIMES; INT 3 ],
        Some (Mult (Add (IntLit 1, IntLit 2), IntLit 3)) );
      ( "(1 + 2) * (3 + 4)",
        [
          LPAREN;
          INT 1;
          PLUS;
          INT 2;
          RPAREN;
          TIMES;
          LPAREN;
          INT 3;
          PLUS;
          INT 4;
          RPAREN;
        ],
        Some (Mult (Add (IntLit 1, IntLit 2), Add (IntLit 3, IntLit 4))) );
      ("+**+", [ PLUS; TIMES; TIMES; PLUS ], None);
    ]

let test_cases_booleans : test_case list =
  List.map
    (fun (x, y, z) -> (x, x, y, z))
    [
      ("true", [ BOOL true ], Some (BoolLit true));
      ("false", [ BOOL false ], Some (BoolLit false));
      ("~true", [ BNOT; BOOL true ], Some (BNot (BoolLit true)));
      ( "true && false",
        [ BOOL true; BAND; BOOL false ],
        Some (BAnd (BoolLit true, BoolLit false)) );
      ( "true || false",
        [ BOOL true; BOR; BOOL false ],
        Some (BOr (BoolLit true, BoolLit false)) );
      ( "true == false",
        [ BOOL true; EQ; BOOL false ],
        Some (Eq (BoolLit true, BoolLit false)) );
      ( "true == false || true",
        [ BOOL true; EQ; BOOL false; BOR; BOOL true ],
        Some (BOr (Eq (BoolLit true, BoolLit false), BoolLit true)) );
    ]

let test_cases_integer_comparisons : test_case list =
  List.map
    (fun (x, y, z) -> (x, x, y, z))
    [
      ("0 == 0", [ INT 0; EQ; INT 0 ], Some (Eq (IntLit 0, IntLit 0)));
      ("1 > 0", [ INT 1; GT; INT 0 ], Some (Gt (IntLit 1, IntLit 0)));
      ("0 >= 0", [ INT 0; GTEQ; INT 0 ], Some (GtEq (IntLit 0, IntLit 0)));
      ("0 < 0", [ INT 0; LT; INT 0 ], Some (Lt (IntLit 0, IntLit 0)));
      ("0 <= 1", [ INT 0; LTEQ; INT 1 ], Some (LtEq (IntLit 0, IntLit 1)));
      ("== <=", [ EQ; LTEQ ], None);
    ]

let test_cases_if_then_else : test_case list =
  List.map
    (fun (x, y, z) -> (x, x, y, z))
    [
      ( "if true then 1 else 2 end",
        [ IF; BOOL true; THEN; INT 1; ELSE; INT 2; END ],
        Some (If (BoolLit true, IntLit 1, IntLit 2)) );
      ( "if true then 1 else if false then 2 else 3 end end",
        [
          IF;
          BOOL true;
          THEN;
          INT 1;
          ELSE;
          IF;
          BOOL false;
          THEN;
          INT 2;
          ELSE;
          INT 3;
          END;
          END;
        ],
        Some
          (If (BoolLit true, IntLit 1, If (BoolLit false, IntLit 2, IntLit 3)))
      );
    ]

let test_cases_variables : test_case list =
  List.map
    (fun (x, y, z) -> (x, x, y, z))
    [
      ("x", [ NAME "x" ], Some (Var "x"));
      ( "let x = 1 in x end",
        [ LET; NAME "x"; ASSIGN; INT 1; IN; NAME "x"; END ],
        Some (Let ("x", IntLit 1, Var "x")) );
      ( "let x = 1 in let y in x end",
        [ LET; NAME "x"; ASSIGN; INT 1; IN; LET; NAME "y"; IN; NAME "x"; END ],
        None );
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

let create_parser_test ((name, inp, _, exp) : test_case) =
  name >:: fun _ ->
  let lexbuf = Lexing.from_string inp in
  let out =
    try Some (Parser.prog Lexer.token lexbuf)
    with _ ->
      Printf.printf "FAILURE\n";
      None
  in
  assert_equal exp out ~printer:(fun x ->
      match x with Some x -> ast_printer x | None -> "None")

let test_suites test_create_func =
  [
    "Arithmetic" >::: List.map test_create_func test_cases_arithmetic;
    "Booleans" >::: List.map test_create_func test_cases_booleans;
    "Integer Comparisons"
    >::: List.map test_create_func test_cases_integer_comparisons;
    "If-Then-Else" >::: List.map test_create_func test_cases_if_then_else;
    "Variables" >::: List.map test_create_func test_cases_variables;
  ]

let suite =
  "Frontend Tests"
  >::: [
         "Lexer" >::: test_suites create_lexer_test;
         "Parser" >::: test_suites create_parser_test;
       ]
