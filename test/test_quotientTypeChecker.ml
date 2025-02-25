open Core
open OUnit2
open Pq_lang
module QuotientTypeChecker = Pq_lang.QuotientTypeChecker.Make
open Testing_utils

let manual_tests : test list =
  let create_test
      ((name : string), (inp : string), (exp : [ `Valid | `Invalid ])) =
    name >:: fun _ ->
    let open Result in
    let res =
      Frontend.run_frontend_string inp
      |> Result.map_error ~f:(fun err ->
             sprintf "Frontend error: %s\n"
               (Frontend.sexp_of_frontend_error err |> Sexp.to_string_hum))
      >>= fun inp_prog ->
      TestingTypeChecker.type_program inp_prog
      |> Result.map_error ~f:(fun err ->
             sprintf "Typing error: %s\n" (TypeChecker.TypingError.print err))
      >>= fun inp_typed_program ->
      inp_typed_program |> TestingTypeChecker.typed_program_get_program
      |> Program.fmap_expr ~f:(fun (t, ()) ->
             ({ t } : QuotientTypeChecker.expr_tag))
      |> Program.fmap_pattern ~f:(fun (t, ()) ->
             ({ t } : QuotientTypeChecker.pattern_tag))
      |> fun inp_for_quotient_type_checking ->
      match
        (QuotientTypeChecker.check_program inp_for_quotient_type_checking, exp)
      with
      | Ok (), `Valid -> Ok ()
      | Ok (), `Invalid ->
          Error "Expected failure but passed quotient type checking"
      | Error QuotientConstraintCheckFailed, `Invalid -> Ok ()
      | Error QuotientConstraintCheckFailed, `Valid ->
          Error "Expected valid input but got failed quotient type check"
      | Error err, _ ->
          Error
            (sprintf "Unexpected quotient type checking failure: %s"
               (err |> QuotientTypeChecker.sexp_of_quotient_typing_error
              |> Sexp.to_string_hum))
    in
    match res with Ok () -> () | Error err_msg -> assert_failure err_msg
  in
  List.map ~f:create_test
    [
      ("Trivial program", {|
1
|}, `Valid);
      ( "Boom hierarchy list (empty count)",
        {|
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
|},
        `Invalid );
      ( "Boom hierarchy list (map)",
        {|
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

let rec map (arg : ((int -> int) * list)) : list =
  match arg -> list with
  | ((f : int -> int), (t : list)) ->
    match t -> list with
    | Empty (u : unit) -> Empty u
    | Leaf (x : int) -> Leaf (f x)
    | Node (p : list * list) ->
      match p -> list with
      | ((l : list), (r : list)) ->
        Node (
          map (f, l),
          map (f, r)
        )
      end
    end
  end
end

let incr (x : int) : int =
  x + 1
end

map (
  incr,
  Node (
    Empty (),
    Node (
      Node (
        Leaf 5,
        Leaf 6
      ),
      Empty ()
    )
  )
)
|},
        `Valid );
      ( "Boom hierarchy list (incr)",
        {|
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

1

|},
        `Valid );
      ( "List set incr",
        {|
type list =
  | Nil of unit
  | Cons of int * list

qtype set =
  list
  |/ (x : int) -> (y : int) -> (zs : set)
    => Cons ((x : int), Cons ((y : int), (zs : set))) == (Cons (y, Cons (x, zs)))

let rec incr (xs : set) : set =
  match xs -> set with
  | Nil (u : unit) -> Nil u
  | Cons (v : int * set) ->
    match v -> set with
    | ((h : int), (ts : set)) ->
      Cons (1 + h, incr ts)
    end
  end
end

incr (Cons (1, Nil ()))
|},
        `Valid );
      ( "List set contains",
        {|
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
|},
        `Valid );
      ( "List set addtwo (flat patterns)",
        {|
type list =
  | Nil of unit
  | Cons of int * list

qtype set =
  list
  |/ (x : int) -> (y : int) -> (zs : set)
    => Cons ((x : int), Cons ((y : int), (zs : set))) == (Cons (y, Cons (x, zs)))

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

1
|},
        `Invalid );
      ( "Set filter",
        {|
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
|},
        `Valid );
    ]

let suite = "Quotient type checking" >::: [ "Manual tests" >::: manual_tests ]
