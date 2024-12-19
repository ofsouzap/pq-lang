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

module Nonempty_list : Nonempty_list_sig
