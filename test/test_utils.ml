open Core
open OUnit2
open Pq_lang
open Utils

module Int_nonempty_list_qcheck_testing = Nonempty_list.QCheck_testing (struct
  type t = int

  let gen = QCheck.Gen.int
  let print = QCheck.Print.int
  let shrink = QCheck.Shrink.int
end)

let nonempty_list_test_head =
  let open QCheck in
  QCheck_runner.to_ounit2_test
    (Test.make ~name:"Head" ~count:1000
       (pair int (list int))
       (fun (h, ts) ->
         let xs = Nonempty_list.make (h, ts) in
         equal_int (Nonempty_list.head xs) h))

let nonempty_list_test_tail =
  let open QCheck in
  QCheck_runner.to_ounit2_test
    (Test.make ~name:"Tail" ~count:1000
       (pair int (list int))
       (fun (h, ts) ->
         let xs = Nonempty_list.make (h, ts) in
         (equal_list equal_int) (Nonempty_list.tail xs) ts))

let nonempty_list_test_singleton =
  let open QCheck in
  QCheck_runner.to_ounit2_test
    (Test.make ~name:"Singleton" ~count:1000 int (fun h ->
         let xs = Nonempty_list.singleton h in
         (equal_list equal_int) (Nonempty_list.to_list xs) [ h ]))

let nonempty_list_test_cons =
  let open QCheck in
  QCheck_runner.to_ounit2_test
    (Test.make ~name:"Cons" ~count:1000
       (pair int (Int_nonempty_list_qcheck_testing.arbitrary ()))
       (fun (h, ts) ->
         let xs = Nonempty_list.cons h ts in
         (equal_list equal_int) (Nonempty_list.to_list xs)
           (h :: Nonempty_list.to_list ts)))

let nonempty_list_test_map =
  let open QCheck in
  QCheck_runner.to_ounit2_test
    (Test.make ~name:"Map" ~count:1000
       (pair
          (fun1 QCheck.Observable.int int)
          (Int_nonempty_list_qcheck_testing.arbitrary ()))
       (fun (f_, xs) ->
         let fn_then_list =
           xs
           |> Nonempty_list.map ~f:(QCheck.Fn.apply f_)
           |> Nonempty_list.to_list
         in
         let list_then_fn =
           xs |> Nonempty_list.to_list |> List.map ~f:(QCheck.Fn.apply f_)
         in
         (equal_list equal_int) fn_then_list list_then_fn))

let nonempty_list_test_fold =
  let open QCheck in
  QCheck_runner.to_ounit2_test
    (Test.make ~name:"Fold" ~count:1000
       (triple int
          (fun2 QCheck.Observable.int QCheck.Observable.int int)
          (Int_nonempty_list_qcheck_testing.arbitrary ()))
       (fun (init, f_, xs) ->
         let out = xs |> Nonempty_list.fold ~init ~f:(QCheck.Fn.apply f_) in
         let exp =
           xs |> Nonempty_list.to_list
           |> List.fold ~init ~f:(QCheck.Fn.apply f_)
         in
         equal_int exp out))

let nonempty_list_test_fold_result =
  let open QCheck in
  QCheck_runner.to_ounit2_test
    (Test.make ~name:"Fold result" ~count:1000
       (triple int
          (fun2 QCheck.Observable.int QCheck.Observable.int
             (QCheck_utils.result_arb QCheck.int QCheck.int))
          (Int_nonempty_list_qcheck_testing.arbitrary ()))
       (fun (init, f_, xs) ->
         let out =
           xs |> Nonempty_list.fold_result ~init ~f:(QCheck.Fn.apply f_)
         in
         let exp =
           xs |> Nonempty_list.to_list
           |> List.fold_result ~init ~f:(QCheck.Fn.apply f_)
         in
         (Result.equal equal_int equal_int) exp out))

let nonempty_list_test_fold_result_consume_init =
  let open QCheck in
  QCheck_runner.to_ounit2_test
    (Test.make ~name:"Fold result consume init" ~count:1000
       (triple int
          (fun2 QCheck.Observable.int QCheck.Observable.int
             (QCheck_utils.result_arb QCheck.int QCheck.int))
          (Int_nonempty_list_qcheck_testing.arbitrary ()))
       (fun (init, f_, xs) ->
         let out =
           xs
           |> Nonempty_list.fold_result_consume_init ~init ~f:(fun x ->
                  match x with First x | Second x -> (QCheck.Fn.apply f_) x)
         in
         let exp =
           xs |> Nonempty_list.to_list
           |> List.fold_result ~init ~f:(QCheck.Fn.apply f_)
         in
         (Result.equal equal_int equal_int) exp out))

let nonempty_list_tests =
  [
    nonempty_list_test_head;
    nonempty_list_test_tail;
    nonempty_list_test_singleton;
    nonempty_list_test_cons;
    nonempty_list_test_map;
    nonempty_list_test_fold;
    nonempty_list_test_fold_result;
    nonempty_list_test_fold_result_consume_init;
  ]

let suite = "Utils" >::: [ "Non-Empty List" >::: nonempty_list_tests ]
