type int_option =
  | None of unit
  | Some of int

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

let list_of_tree (xs : tree) : list = xs end

let cons (arg : (int * list)) : list =
  match arg -> list with
  | ((h : int), (ts : list)) ->
    Node (Leaf h, ts)
  end
end

let rec incr (xs : list) : list =
  match xs -> list with
  | Empty (u : unit) -> Empty u
  | Leaf (x : int) -> Leaf (x + 1)
  | Node ((l : list), (r : list)) ->
    Node (incr l, incr r)
  end
end

let append (arg : (list * list)) : list =
  match arg -> list with
  | ((xs : list), (ys : list)) ->
    Node (xs, ys)
  end
end

let rec head (xs : list) : int_option =
  match xs -> int_option with
  | Empty (_ : unit) -> None ()
  | Leaf (x : int) -> Some x
  | Node ((l : list), (r : list)) ->
    let l_head = head l in
      match l_head -> int_option with
      | None (_ : unit) -> head r
      | Some (_ : int) -> l_head
      end
    end
  end
end
