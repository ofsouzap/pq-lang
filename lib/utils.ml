open Core

module type Nonempty_list_sig = sig
  type 'a t = 'a * 'a list [@@deriving sexp, equal]

  val make : 'a * 'a list -> 'a t
  val to_list : 'a t -> 'a list
  val head : 'a t -> 'a
  val tail : 'a t -> 'a list
  val cons : 'a -> 'a t -> 'a t
  val map : f:('a -> 'b) -> 'a t -> 'b t
  val fold : 'a t -> init:'b -> f:('b -> 'a -> 'b) -> 'b
  val to_string : f:('a -> string) -> 'a t -> string

  val fold_result :
    'a t -> init:'b -> f:('b -> 'a -> ('b, 'c) result) -> ('b, 'c) result
end

module Nonempty_list : Nonempty_list_sig = struct
  type 'a t = 'a * 'a list [@@deriving sexp, equal]

  let make = Fn.id
  let to_list ((h, ts) : 'a t) = h :: ts
  let head (h, _) = h
  let tail (_, ts) = ts
  let cons (h : 'a) (ts : 'a t) = (h, to_list ts)
  let map ~(f : 'a -> 'b) ((h, ts) : 'a t) : 'b t = (f h, List.map ~f ts)
  let fold (xs : 'a t) = List.fold (to_list xs)
  let to_string ~(f : 'a -> string) (xs : 'a t) = List.to_string ~f (to_list xs)
  let fold_result (xs : 'a t) = List.fold_result (to_list xs)
end
