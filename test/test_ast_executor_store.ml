open OUnit2
open Pq_lang
open Ast_executor

let key_arb = QCheck.string_printable

let int_value_arb =
  let open QCheck in
  map (fun x -> Int x) int

let bool_value_arb =
  let open QCheck in
  map (fun x -> Bool x) bool

let value_arb =
  let open QCheck in
  oneof [ int_value_arb; bool_value_arb ]

let non_empty_store_arb =
  let open QCheck in
  let store_gen =
    (* This generates a non-empty list of key-value pairs *)
    list_of_size Gen.(map (( + ) 1) nat) (pair key_arb value_arb)
  in
  map
    (fun kvs ->
      List.fold_left
        (fun store (key, value) -> store_set key value store)
        store_empty kvs)
    store_gen

(** Test that getting any key from an empty store returns no value *)
let test_store_get_from_empty =
  let open QCheck in
  Test.make ~count:100 ~name:"store_get_from_empty" key_arb (fun key ->
      let store = store_empty in
      let res = store_get key store in
      res = None)

(** Take an empty store, set a value in it, get it back and check that the value is as expected *)
let test_store_empty_set_then_get =
  let open QCheck in
  Test.make ~count:100 ~name:"store_empty_set_then_get" (pair key_arb value_arb)
    (fun (key, value) ->
      let store = store_empty in
      let store' = store_set key value store in
      let res = store_get key store' in
      res = Some value)

(** Populate a store, set a value in it, get it back and check that the value is as expected *)
let test_store_populated_set_then_get =
  let open QCheck in
  Test.make ~count:100 ~name:"store_populated_set_then_get"
    (triple non_empty_store_arb key_arb value_arb) (fun (store, key, value) ->
      let store' = store_set key value store in
      let res = store_get key store' in
      res = Some value)

(** Overwrite a value in a populated store and check that the new value is reflected *)
let test_store_populated_overwrite =
  let open QCheck in
  Test.make ~count:100 ~name:"store_populated_overwrite"
    (quad non_empty_store_arb key_arb value_arb value_arb)
    (fun (store, key, old_value, new_value) ->
      let store' = store_set key old_value store in
      let store'' = store_set key new_value store' in
      let res = store_get key store'' in
      res = Some new_value)

let suite =
  "AST Executor Store"
  >::: List.map QCheck_runner.to_ounit2_test
         [
           test_store_get_from_empty;
           test_store_empty_set_then_get;
           test_store_populated_set_then_get;
         ]
