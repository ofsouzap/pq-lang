type list =
  | Nil of unit
  | Cons of int * list

qtype set =
  list
  |/ (x : int) -> (y : int) -> (zs : set)
    => Cons ((x : int), Cons ((y : int), (zs : set))) == (Cons (y, Cons (x, zs)))

let rec filter (arg : ((int -> bool) * set)) : set =
  match arg -> set with
  | ((p : int -> bool), (t : set)) ->
    match t -> set with
    | Nil (u : unit) -> Nil u
    | Cons (x : (int * set)) ->
      match x -> set with
      | ((xh : int), (xts : set)) ->
        if p xh
        then Cons (xh, filter (p, xts))
        else filter (p, xts)
        end
      end
    end
  end
end

1