open Core
open OUnit2
open Pq_lang
open Program
open Quotient_type_checking
open Testing_utils

let manual_tests : test list =
  let create_test
      ((name : string), (inp : string), (exp : [ `Valid | `Invalid ])) =
    name >:: fun _ ->
    let open Result in
    let res =
      Frontend.run_frontend_string inp
      |> Result.map_error ~f:(fun err ->
             sprintf "Frontend error: %s\n"
               (Frontend.sexp_of_frontend_error err |> Sexp.to_string_hum))
      >>= fun inp_prog ->
      TestingTypeChecker.type_program inp_prog
      |> Result.map_error ~f:(fun err ->
             sprintf "Typing error: %s\n" (Typing.print_typing_error err))
      >>= fun inp_typed_program ->
      inp_typed_program |> TestingTypeChecker.typed_program_get_program
      |> fmap_expr ~f:(fun (t, ()) -> ({ t } : Quotient_type_checking.ast_tag))
      |> fmap_pattern ~f:(fun (t, ()) ->
             ({ t } : Quotient_type_checking.pattern_tag))
      |> fun inp_for_quotient_type_checking ->
      match
        ( Quotient_type_checking.check_program inp_for_quotient_type_checking,
          exp )
      with
      | Ok (), `Valid -> Ok ()
      | Ok (), `Invalid ->
          Error "Expected failure but passed quotient type checking"
      | Error QuotientConstraintCheckFailed, `Invalid -> Ok ()
      | Error QuotientConstraintCheckFailed, `Valid ->
          Error "Expected valid input but got failed quotient type check"
      | Error err, _ ->
          Error
            (sprintf "Unexpected quotient type checking failure: %s"
               (err |> sexp_of_quotient_typing_error |> Sexp.to_string_hum))
    in
    match res with Ok () -> () | Error err_msg -> assert_failure err_msg
  in
  List.map ~f:create_test
    [
      ("Trivial program", {|
1
|}, `Valid);
      ( "Boom hierarchy list (empty count)",
        {|
type tree =
  | Empty of unit
  | Leaf of int
  | Node of tree * tree

qtype list
  = tree
  |/ (u : unit) -> (x : list) => Node (Empty (u : unit), (x : list)) == (x)
  |/ (u : unit) -> (x : list) => Node ((x : list), Empty (u : unit)) == (x)
  |/ (x : list) -> (y : list) -> (z : list) =>
    Node (
      Node (
        (x : list),
        (y : list)
      ),
      (z : list)
    )
    ==
    (Node (
      x,
      Node (
        y,
        z
      )
    ))

let rec empty_count (t : list) : int =
  match t with
  | Empty (u : unit) -> 1
  | Leaf (x : int) -> 0
  | Node ((l : list), (r : list)) ->
    empty_count l + empty_count r
  end
end

empty_count (Node (Empty (), (Node (Empty (), (Leaf 1)))))
|},
        `Invalid );
    ]

let suite = "Quotient type checking" >::: [ "Manual tests" >::: manual_tests ]
