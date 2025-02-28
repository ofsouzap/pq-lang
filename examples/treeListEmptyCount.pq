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

let rec empty_count (t : list) : int =
  match t -> int with
  | Empty (u : unit) -> 1
  | Leaf (x : int) -> 0
  | Node (p : list * list) ->
    match p -> int with
    | ((l : list), (r : list)) ->
      empty_count l + empty_count r
    end
  end
end

empty_count (Node (Empty (), (Node (Empty (), (Leaf 1)))))