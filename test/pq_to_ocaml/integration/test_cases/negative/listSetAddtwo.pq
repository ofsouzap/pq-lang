type list =
  | Nil of unit
  | Cons of int * list

qtype set =
  list
  |/ (x : int) -> (y : int) -> (zs : set)
    => Cons ((x : int), Cons ((y : int), (zs : set))) == (Cons (y, Cons (x, zs)))
  |/ (h : int) -> (ts : set)
    => Cons ((h : int), (ts : set)) == (Cons (h, Cons (h, ts)))
  |/ (h1 : int) -> (h2 : int) -> (ts : set)
    => Cons ((h1 : int), Cons ((h2 : int), (ts : set))) == (
      if h1 == h2
      then
        Cons (h1, ts)
      else
        Cons (h1, Cons (h2, ts))
      end
    )

let rec addtwo (arg : (set * set)) : set =
  match arg -> set with
  | ((xs : set), (ys : set)) ->
    match xs -> set with
    | Nil (u : unit) -> Nil u
    | Cons (x : int * set) ->
      match x -> set with
      | ((xh : int), (xts : set)) ->
        match ys -> set with
        | Nil (u : unit) -> Nil u
        | Cons (y : int * set) ->
          match y -> set with
          | ((yh : int), (yts : set)) ->
            Cons (xh + yh, addtwo (xts, yts))
          end
        end
      end
    end
  end
end
