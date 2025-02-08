open Core
open Utils
open Varname
open Pattern
open Ast

type partial_evaluation_error [@@deriving sexp, equal]

module PartialEvaluator (TagExpr : sig
  type t [@@deriving sexp, equal]
end) (TagPat : sig
  type t [@@deriving sexp, equal]
end) : sig
  type closure = {
    param : varname;
    body : (TagExpr.t, TagPat.t) expr;
    store : store;
    recursive : [ `Recursive of varname | `NonRecursive ];
  }
  [@@deriving sexp, equal]

  and store_val = ((TagExpr.t, TagPat.t) expr, closure) Either.t
  [@@deriving sexp, equal]

  and store = store_val StringMap.t [@@deriving sexp, equal]

  type state = { store : store; e : (TagExpr.t, TagPat.t) expr }
  [@@deriving sexp, equal]

  val eval :
    mrd:int ->
    state ->
    ((TagExpr.t, TagPat.t) expr, partial_evaluation_error) Result.t
end = struct
  type tag_expr = (TagExpr.t, TagPat.t) expr [@@deriving sexp, equal]

  type closure = {
    param : varname;
    body : tag_expr;
    store : store;
    recursive : [ `Recursive of varname | `NonRecursive ];
  }
  [@@deriving sexp, equal]

  and store_val = (tag_expr, closure) Either.t [@@deriving sexp, equal]
  and store = store_val StringMap.t [@@deriving sexp, equal]

  type state = { store : store; e : tag_expr } [@@deriving sexp, equal]

  let rec match_pattern (pat : 'tag_p pattern) (e : tag_expr) :
      (store_val StringMap.t, unit) Result.t =
    let open Result in
    match (pat, e) with
    | PatName (_, xname, _), _ -> Ok (StringMap.singleton xname (First e))
    | PatPair (_, p1, p2), Pair (_, e1, e2) ->
        match_pattern p1 e1 >>= fun p1_bindings ->
        match_pattern p2 e2 >>| fun p2_bindings ->
        Map.merge p1_bindings p2_bindings ~f:(fun ~key:_ -> function
          | `Left e
          | `Right e
          | `Both (_, e) (* Shouldn't occur, I think, but p2 overwrites p1 *) ->
              Some e)
    | PatPair _, _ -> Error ()
    | PatConstructor (_, c_name, p1), Constructor (_, c_name', e1)
      when equal_string c_name c_name' ->
        match_pattern p1 e1
    | PatConstructor _, _ -> Error ()

  let rec eval ~(mrd : int) { store; e = orig_e } :
      (tag_expr, partial_evaluation_error) Result.t =
    let open Result in
    if mrd <= 0 then (* Don't exceed maximum recursion depth *) Ok orig_e
    else
      let eval ?(new_store : store option) (e : tag_expr) =
        eval ~mrd:(mrd - 1) { store = Option.value ~default:store new_store; e }
      in
      match orig_e with
      | UnitLit v -> Ok (UnitLit v)
      | IntLit (v, x) -> Ok (IntLit (v, x))
      | Add (v, e1, e2) -> (
          eval e1 >>= fun e1' ->
          eval e2 >>| fun e2' ->
          match (e1', e2') with
          | IntLit (_, x1), IntLit (_, x2) -> IntLit (v, x1 + x2)
          | _ -> Add (v, e1', e2'))
      | Neg (v, e1) -> (
          eval e1 >>| fun e1' ->
          match e1' with IntLit (_, x) -> IntLit (v, -x) | _ -> Neg (v, e1'))
      | Subtr (v, e1, e2) -> (
          eval e1 >>= fun e1' ->
          eval e2 >>| fun e2' ->
          match (e1', e2') with
          | IntLit (_, x1), IntLit (_, x2) -> IntLit (v, x1 - x2)
          | _ -> Subtr (v, e1', e2'))
      | Mult (v, e1, e2) -> (
          eval e1 >>= fun e1' ->
          eval e2 >>| fun e2' ->
          match (e1', e2') with
          | IntLit (_, x1), IntLit (_, x2) -> IntLit (v, x1 * x2)
          | _ -> Mult (v, e1', e2'))
      | BoolLit (v, x) -> Ok (BoolLit (v, x))
      | BNot (v, e1) -> (
          eval e1 >>| fun e1' ->
          match e1' with
          | BoolLit (_, x) -> BoolLit (v, not x)
          | _ -> BNot (v, e1'))
      | BOr (v, e1, e2) -> (
          eval e1 >>= fun e1' ->
          eval e2 >>| fun e2' ->
          match (e1', e2') with
          | BoolLit (_, x1), BoolLit (_, x2) -> BoolLit (v, x1 || x2)
          | _ -> BOr (v, e1', e2'))
      | BAnd (v, e1, e2) -> (
          eval e1 >>= fun e1' ->
          eval e2 >>| fun e2' ->
          match (e1', e2') with
          | BoolLit (_, x1), BoolLit (_, x2) -> BoolLit (v, x1 && x2)
          | _ -> BAnd (v, e1', e2'))
      | Pair (v, e1, e2) ->
          eval e1 >>= fun e1' ->
          eval e2 >>| fun e2' -> Pair (v, e1', e2')
      | Eq (v, e1, e2) -> (
          eval e1 >>= fun e1' ->
          eval e2 >>| fun e2' ->
          match (e1', e2') with
          | IntLit (_, x1), IntLit (_, x2) -> BoolLit (v, equal_int x1 x2)
          | BoolLit (_, b1), BoolLit (_, b2) -> BoolLit (v, equal_bool b1 b2)
          | _ -> Eq (v, e1', e2'))
      | Gt (v, e1, e2) -> (
          eval e1 >>= fun e1' ->
          eval e2 >>| fun e2' ->
          match (e1', e2') with
          | IntLit (_, x1), IntLit (_, x2) -> BoolLit (v, x1 > x2)
          | _ -> Gt (v, e1', e2'))
      | GtEq (v, e1, e2) -> (
          eval e1 >>= fun e1' ->
          eval e2 >>| fun e2' ->
          match (e1', e2') with
          | IntLit (_, x1), IntLit (_, x2) -> BoolLit (v, x1 >= x2)
          | _ -> GtEq (v, e1', e2'))
      | Lt (v, e1, e2) -> (
          eval e1 >>= fun e1' ->
          eval e2 >>| fun e2' ->
          match (e1', e2') with
          | IntLit (_, x1), IntLit (_, x2) -> BoolLit (v, x1 < x2)
          | _ -> Lt (v, e1', e2'))
      | LtEq (v, e1, e2) -> (
          eval e1 >>= fun e1' ->
          eval e2 >>| fun e2' ->
          match (e1', e2') with
          | IntLit (_, x1), IntLit (_, x2) -> BoolLit (v, x1 <= x2)
          | _ -> LtEq (v, e1', e2'))
      | If (v, e1, e2, e3) -> (
          eval e1 >>= fun e1' ->
          match e1' with
          | BoolLit (_, true) -> eval e2
          | BoolLit (_, false) -> eval e3
          | _ ->
              eval e2 >>= fun e2' ->
              eval e3 >>| fun e3' -> If (v, e1', e2', e3'))
      | Var (v, xname) -> (
          match Map.find store xname with
          | Some (First e) -> eval e
          | Some (Second _) | None -> Ok (Var (v, xname)))
      | Let (_, xname, e1, e2) ->
          eval e1 >>= fun e1' ->
          eval ~new_store:(Map.set store ~key:xname ~data:(First e1')) e2
      | App (v, e1, e2) -> (
          eval e1 >>= fun e1' ->
          eval e2 >>= fun e2' ->
          let get_default_result () = Ok (App (v, e1', e2')) in
          match e1' with
          | Var (_, xname) -> (
              match Map.find store xname with
              | Some (Second closure) ->
                  let new_store =
                    (* Add the parameter to the store to use *)
                    Map.set closure.store ~key:closure.param ~data:(First e2')
                  in
                  let new_store =
                    (* If the function is recursive, make it re-accessible in the execution of the function *)
                    match closure.recursive with
                    | `Recursive fname ->
                        Map.set new_store ~key:fname ~data:(Second closure)
                    | `NonRecursive -> new_store
                  in
                  eval ~new_store closure.body
              | _ -> get_default_result ())
          | _ -> get_default_result ())
      | Match (v, e1, cases) -> (
          eval e1 >>= fun e1' ->
          Nonempty_list.fold_result
          (* In this fold, an Ok value means we haven't found a matching case and have instead partially evaluated each case,
        and an Error value means we've found a matching case and create a function that will evaluate it with the updated store *)
            ~init:()
            ~f:(fun () (case_p, case_e) ->
              match match_pattern case_p e1' with
              | Ok var_bindings ->
                  Error
                    (let new_store =
                       Map.merge store var_bindings ~f:(fun ~key:_ -> function
                         | `Left e | `Right e | `Both (_, e) -> Some e)
                     in
                     fun () -> eval ~new_store case_e)
              | Error () -> Ok ())
            cases
          |> function
          | Ok () -> Ok (Match (v, e1', cases))
          | Error case_eval_f -> case_eval_f ())
      | Constructor (v, c_name, e1) ->
          eval e1 >>| fun e1' -> Constructor (v, c_name, e1')
end
