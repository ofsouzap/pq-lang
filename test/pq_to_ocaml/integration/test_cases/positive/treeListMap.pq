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

let rec map (arg : (int -> int) * list) : list =
  match arg -> list with
  | ((f : int -> int), (xs : list)) ->
    match xs -> list with
    | Empty (_ : unit) -> Empty ()
    | Leaf (x : int) -> Leaf (f x)
    | Node (p : list * list) ->
      match p -> list with
      | ((l : list), (r : list)) ->
        Node (map (f, l), map (f, r))
      end
    end
  end
end
