# The basic binary tree variant type
type tree =
  | Leaf of unit
  | Node of (int * (tree * tree))

# A "mobile": a binary tree where the ordering of the children doesn't matter
qtype mobile
  = tree
  |/ (x : int) -> (l : mobile) -> (r : mobile)
    => Node ((x : int), ((l : mobile), (r : mobile))) == (Node (x, (r, l)))

let rec sum (m : mobile) : int =
  match m -> int with
  | Leaf (_ : unit) -> 0
  | Node ((x : int), ((l : mobile), (r : mobile))) ->
    x + sum l + sum r
  end
end

let rec map (p : (int -> int) * mobile) : mobile =
  match p -> mobile with
  | ((f : int -> int), (m : mobile)) ->
    match m -> mobile with
    | Leaf (_ : unit) -> Leaf ()
    | Node ((x : int), ((l : mobile), (r : mobile))) ->
        Node (x, (map (f, l), map (f, r)))
    end
  end
end
