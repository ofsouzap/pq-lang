# Definition of the base variant type for lists
type list =
  | Nil of unit
  | Cons of int * list

# Definition of the quotient type for sets, quotienting the list type
qtype set =
  list
  |/ (x : int) -> (y : int) -> (zs : set)
    => Cons ((x : int), Cons ((y : int), (zs : set))) == (Cons (y, Cons (x, zs)))

# Create a set
let set_of_list (xs : list) : set = xs end

# Add a single element to a set
let add (arg : (int * set)) : set = Cons arg end

(* Filter a set by the predicate provided.

   Input: ((p : int -> bool), (xs : set))
     p - the predicate to filter on
     xs - the set to filter

    Output: (ys : set)
      ys - the set with all the elements of xs that satisfy p *)
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

# Check if a set contains a value
let rec contains (arg : (int * set)) : bool =
  match arg -> bool with
  | ((q : int), (xs : set)) ->
    match xs -> bool with
    | Nil (_ : unit) -> false
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

# Union of two sets
let rec union (arg: (set * set)) : set =
  match arg -> set with
  | ((xs : set), (ys : set)) ->
    match xs -> set with
    | Nil (_ : unit) -> ys
    | Cons (xv : (int * set)) ->
      match xv -> set with
      | ((xh : int), (xts : set)) ->
        Cons (xh, union (xts, ys))
      end
    end
  end
end

1
