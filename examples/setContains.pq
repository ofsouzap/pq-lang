type list =
  | Nil of unit
  | Cons of int * list

qtype set =
  list
  |/ (x : int) -> (y : int) -> (zs : set)
    => Cons ((x : int), Cons ((y : int), (zs : set))) == (Cons (y, Cons (x, zs)))

let rec contains (arg : (int * set)) : bool =
  match arg -> bool with
  | ((q : int), (xs : set)) ->
    match xs -> bool with
    | Nil (u : unit) -> false
    | Cons (p : int * set) ->
      match p -> bool with
      | ((h : int), (ts : set)) ->
        if q == h
        then true
        else contains (q, ts)
        end
      end
    end
  end
end

contains (2, (Cons (1, Nil ())))