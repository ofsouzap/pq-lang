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
  | (Nil (_ : unit), (_2 : set)) -> Nil ()
  | ((_ : set), Nil (_2 : unit)) -> Nil ()
  | (Cons ((xh : int), (xts : set)), Cons ((yh : int), (yts : set))) ->
    Cons (xh + yh, addtwo (xts, yts))
  end
end
