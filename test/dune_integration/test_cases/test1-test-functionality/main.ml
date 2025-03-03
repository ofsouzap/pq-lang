open Core

let rec ocaml_list_to_list : int list -> ListSet.list = function
  | [] -> ListSet.Nil ()
  | x :: xs -> Cons (x, ocaml_list_to_list xs)

let test_cases_of_list : unit Alcotest.test_case list =
  [
    ( "create empty",
      `Quick,
      fun () ->
        let _ = [] |> ocaml_list_to_list |> ListSet.set_of_list in
        () );
    ( "create singleton",
      `Quick,
      fun () ->
        let _ = [ 0 ] |> ocaml_list_to_list |> ListSet.set_of_list in
        () );
    ( "create multi",
      `Quick,
      fun () ->
        let _ = [ 1; 2; 3; 4 ] |> ocaml_list_to_list |> ListSet.set_of_list in
        () );
  ]

let test_cases_contains : unit Alcotest.test_case list =
  [
    ( "contains positive",
      `Quick,
      fun () ->
        let set = [ 1; 2; 3; 4 ] |> ocaml_list_to_list |> ListSet.set_of_list in
        Alcotest.(check bool) "contains 3" true (ListSet.contains (3, set)) );
    ( "contains positive with duplicate",
      `Quick,
      fun () ->
        let set =
          [ 1; 2; 3; 4; 7; 2 ] |> ocaml_list_to_list |> ListSet.set_of_list
        in
        Alcotest.(check bool) "contains 2" true (ListSet.contains (2, set)) );
    ( "contains negative",
      `Quick,
      fun () ->
        let set = [ 1; 2; 3; 4 ] |> ocaml_list_to_list |> ListSet.set_of_list in
        Alcotest.(check bool) "contains 6" false (ListSet.contains (6, set)) );
  ]

let test_cases_add : unit Alcotest.test_case list =
  [
    ( "add to empty",
      `Quick,
      fun () ->
        let set = [] |> ocaml_list_to_list |> ListSet.set_of_list in
        let set = ListSet.add (7, set) in
        Alcotest.(check bool) "contains 7" true (ListSet.contains (7, set)) );
    ( "add non-existing",
      `Quick,
      fun () ->
        let set = [ 1; 2; 3; 4 ] |> ocaml_list_to_list |> ListSet.set_of_list in
        let set = ListSet.add (7, set) in
        Alcotest.(check bool) "contains 7" true (ListSet.contains (7, set)) );
    ( "add existing",
      `Quick,
      fun () ->
        let set =
          [ 1; 2; 3; 4; 7; 7 ] |> ocaml_list_to_list |> ListSet.set_of_list
        in
        let set = ListSet.add (7, set) in
        Alcotest.(check bool) "contains 7" true (ListSet.contains (7, set)) );
  ]

let () =
  Alcotest.run "dune integration [1] testing functionality"
    [
      ("set_of_list", test_cases_of_list);
      ("contains", test_cases_contains);
      ("add", test_cases_add);
    ]
