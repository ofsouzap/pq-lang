type arith_expr_base =
  | Zero of unit
  | One of unit
  | Neg of arith_expr_base
  | Add of arith_expr_base * arith_expr_base

qtype arith_expr
  = arith_expr_base
  |/ (* Associative *)
    (e1 : arith_expr) -> (e2 : arith_expr) -> (e3 : arith_expr)
    => Add ((e1 : arith_expr), Add ((e2 : arith_expr), (e3 : arith_expr))) == (Add (Add (e1, e2), e3))
  |/ (* Commutative *)
    (e1 : arith_expr) -> (e2 : arith_expr)
    => Add ((e1 : arith_expr), (e2 : arith_expr)) == (Add (e2, e1))
  |/ (* Neutral Zero (L) *)
    (_ : unit) -> (e : arith_expr)
    => Add ((Zero (_ : unit)), (e : arith_expr)) == (e)
  |/ (* Neutral Zero (R) *)
    (_ : unit) -> (e : arith_expr)
    => Add ((e : arith_expr), (Zero (_ : unit))) == (e)

let rec eval (e : arith_expr) : int =
  match e -> int with
  | Zero (_ : unit) -> 0
  | One (_ : unit) -> 1
  | Neg (e : arith_expr) -> (-1) * eval e
  | Add ((e1 : arith_expr), (e2 : arith_expr)) -> eval e1 + eval e2
  end
end
