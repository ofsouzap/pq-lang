type tree =
  | Empty of unit
  | Leaf of int
  | Node of tree * tree

qtype list
  = tree
  |/ (u : unit) -> (x : list) => Node (Empty (u : unit), (x : list)) == (x)
  |/ (u : unit) -> (x : list) => Node ((x : list), Empty (u : unit)) == (x)
  |/ (x : list) -> (y : list) -> (z : list) =>
    Node (
      Node (
        (x : list),
        (y : list)
      ),
      (z : list)
    )
    ==
    (Node (
      x,
      Node (
        y,
        z
      )
    ))

let rec f (x : int) : int =
  if x == 0
  then 0
  else x + f (x - 1)
  end
end

let rec incr (xs : list) : list =
  match xs -> list with
  | Empty (u : unit) -> Empty u
  | Leaf (x : int) -> Leaf (f x)
  | Node (p : list * list) -> (
    let p2 =
      match p -> list * list with
      | ((l : list), (r : list)) ->
        (incr l, incr r)
      end
    in
      Node p2
    end )
  end
end
