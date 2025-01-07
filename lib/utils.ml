open Core
module StringSet = Set.Make (String)

let lexer_keywords : string list =
  [
    "end";
    "if";
    "then";
    "else";
    "let";
    "rec";
    "in";
    "true";
    "false";
    "fun";
    "unit";
    "int";
    "bool";
    "match";
    "with";
    "type";
    "of";
  ]

module type QCheck_utils_sig = sig
  val result_arb :
    'a QCheck.arbitrary ->
    'b QCheck.arbitrary ->
    ('a, 'b) Result.t QCheck.arbitrary

  val filter_gen :
    ?max_attempts:int -> 'a QCheck.Gen.t -> f:('a -> bool) -> 'a QCheck.Gen.t
end

module QCheck_utils : QCheck_utils_sig = struct
  let result_arb (x_arb : 'a QCheck.arbitrary) (y_arb : 'b QCheck.arbitrary) :
      ('a, 'b) Result.t QCheck.arbitrary =
    QCheck.map
      (fun (b, x, y) -> if b then Ok x else Error y)
      (QCheck.triple QCheck.bool x_arb y_arb)

  let filter_gen ?(max_attempts : int option) (x_gen : 'a QCheck.Gen.t)
      ~(f : 'a -> bool) : 'a QCheck.Gen.t =
    let open QCheck.Gen in
    fix
      (fun self n_opt ->
        let _ =
          (* Termination check *)
          match n_opt with
          | None -> ()
          | Some n -> if n < 0 then failwith "Filter ran out of attempts"
        in
        x_gen >>= fun x ->
        if f x then return x else self (Option.map n_opt ~f:(fun n -> n - 1)))
      max_attempts
end

module type QCheck_testing_sig = sig
  open QCheck

  type t
  type gen_options
  type print_options
  type shrink_options
  type arb_options

  val gen : gen_options -> t Gen.t
  val print : print_options -> t Print.t
  val shrink : shrink_options -> t Shrink.t
  val arbitrary : arb_options -> t arbitrary
end

module type Nonempty_list_sig = sig
  type 'a t = 'a * 'a list [@@deriving sexp, equal]

  val make : 'a * 'a list -> 'a t
  val to_list : 'a t -> 'a list
  val from_list_unsafe : 'a list -> 'a t
  val head : 'a t -> 'a
  val tail : 'a t -> 'a list
  val singleton : 'a -> 'a t
  val cons : 'a -> 'a t -> 'a t
  val map : f:('a -> 'b) -> 'a t -> 'b t
  val fold : 'a t -> init:'b -> f:('b -> 'a -> 'b) -> 'b
  val rev : 'a t -> 'a t

  val fold_result :
    'a t -> init:'b -> f:('b -> 'a -> ('b, 'c) Result.t) -> ('b, 'c) Result.t

  val fold_result_consume_init :
    'a t ->
    init:'init ->
    f:(('init, 'acc) Either.t -> 'a -> ('acc, 'err) Result.t) ->
    ('acc, 'err) Result.t

  val nonempty_list_arb : 'a QCheck.arbitrary -> 'a t QCheck.arbitrary
end

module Nonempty_list : Nonempty_list_sig = struct
  type 'a t = 'a * 'a list [@@deriving sexp, equal]

  let make = Fn.id
  let to_list ((h, ts) : 'a t) = h :: ts

  let from_list_unsafe (xs : 'a list) =
    match xs with [] -> failwith "Empty list" | h :: ts -> (h, ts)

  let head (h, _) = h
  let tail (_, ts) = ts
  let singleton (h : 'a) = (h, [])
  let cons (h : 'a) (ts : 'a t) = (h, to_list ts)
  let map ~(f : 'a -> 'b) ((h, ts) : 'a t) : 'b t = (f h, List.map ~f ts)
  let fold (xs : 'a t) = List.fold (to_list xs)

  let rev (xs : 'a t) =
    match tail xs with
    | [] -> head xs |> singleton
    | ts_h :: ts_ts ->
        fold
          ~init:(singleton (head xs))
          ~f:(fun acc x -> cons x acc)
          (make (ts_h, ts_ts))

  let fold_result (xs : 'a t) = List.fold_result (to_list xs)

  let fold_result_consume_init ((h, ts) : 'a t) ~(init : 'init)
      ~(f : ('init, 'acc) Either.t -> 'a -> ('acc, 'err) Result.t) :
      ('acc, 'err) Result.t =
    match f (First init) h with
    | Ok h_y -> List.fold_result ts ~init:h_y ~f:(fun x -> f (Second x))
    | Error err -> Error err

  let nonempty_list_arb (v_arb : 'a QCheck.arbitrary) : 'a t QCheck.arbitrary =
    QCheck.map
      ~rev:(fun xs -> (head xs, tail xs))
      (fun (h, ts) -> make (h, ts))
      QCheck.(pair v_arb (list v_arb))
end
