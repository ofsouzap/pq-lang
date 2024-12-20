open Core

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
  val to_string : f:('a -> string) -> 'a t -> string
  val rev : 'a t -> 'a t

  val fold_result :
    'a t -> init:'b -> f:('b -> 'a -> ('b, 'c) Result.t) -> ('b, 'c) Result.t

  val fold_result_consume_init :
    'a t ->
    init:'init ->
    f:(('init, 'acc) Either.t -> 'a -> ('acc, 'err) Result.t) ->
    ('acc, 'err) Result.t
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
  let to_string ~(f : 'a -> string) (xs : 'a t) = List.to_string ~f (to_list xs)

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
end
