open Core
open Ast
open Vtype

let rec type_expr (e : 'a expr) : ((vtype * 'a) expr, unit) Result.t =
  let open Result in
  let add_type (t : vtype) (e : 'a expr) : (vtype * 'a) expr =
    e >|= fun x -> (t, x)
  in
  let be_of_type (t : vtype) (e : (vtype * 'a) expr) :
      ((vtype * 'a) expr, unit) Result.t =
    let t_e = e |> expr_node_val |> fst in
    if equal_vtype t_e t then Ok e else Error ()
  in
  let type_binop (e : 'a expr) (e1 : 'a expr) (e2 : 'a expr) :
      ((vtype * 'a) expr, unit) Result.t =
    type_expr e1 >>= fun e1' ->
    type_expr e2 >>= fun e2' ->
    let t1 = e1' |> expr_node_val |> fst in
    let t2 = e2' |> expr_node_val |> fst in
    match (t1, t2) with
    | VTypeInt, VTypeInt -> Ok (add_type VTypeInt e)
    | VTypeInt, _ -> Error ()
    | VTypeBool, VTypeBool -> Ok (add_type VTypeBool e)
    | VTypeBool, _ -> Error ()
    | VTypeFun (t1, t2), VTypeFun (t3, t4) ->
        if equal_vtype t1 t3 && equal_vtype t2 t4 then
          Ok (add_type (VTypeFun (t1, t2)) e)
        else Error ()
    | VTypeFun _, _ -> Error ()
  in
  match e with
  | IntLit _ as e -> Ok (add_type VTypeInt e)
  | (Add (_, e1, e2) as e) | (Subtr (_, e1, e2) as e) | (Mult (_, e1, e2) as e)
    ->
      type_binop e e1 e2 >>= be_of_type VTypeInt
  | Neg (_, e) -> type_expr e >>= be_of_type VTypeBool
  | BoolLit _ -> Ok (add_type VTypeBool e)
  | BNot (_, e) -> type_expr e >>= be_of_type VTypeBool
  | (BOr (_, e1, e2) as e) | (BAnd (_, e1, e2) as e) ->
      type_binop e e1 e2 >>= be_of_type VTypeBool
  | Eq (_, e1, e2) -> (
      type_expr e1 >>= fun e1' ->
      type_expr e2 >>= fun e2' ->
      let t1 = e1' |> expr_node_val |> fst in
      let t2 = e2' |> expr_node_val |> fst in
      match (t1, t2) with
      | VTypeInt, VTypeInt | VTypeBool, VTypeBool -> Ok (add_type VTypeBool e)
      | _, _ -> Error ())
  | (Gt (_, e1, e2) as e)
  | (GtEq (_, e1, e2) as e)
  | (Lt (_, e1, e2) as e)
  | (LtEq (_, e1, e2) as e) ->
      type_binop e e1 e2 >>= be_of_type VTypeBool
  | If (_, e1, e2, e3) ->
      type_expr e1 >>= be_of_type VTypeBool >>= fun _ ->
      type_expr e2 >>= fun e2' ->
      type_expr e3 >>= fun e3' ->
      let t2 = e2' |> expr_node_val |> fst in
      let t3 = e3' |> expr_node_val |> fst in
      if equal_vtype t2 t3 then Ok (add_type t2 e) else Error ()
  | Var _ -> failwith "TODO - variable context"
  | Let _ -> failwith "TODO - alter variable context"
  | Fun _ ->
      failwith "TODO - need to alter AST to have parameter type annotations"
  | App (_, e1, e2) as e -> (
      type_expr e1 >>= fun e1' ->
      type_expr e2 >>= fun e2' ->
      let t1 = e1' |> expr_node_val |> fst in
      let t2 = e2' |> expr_node_val |> fst in
      match t1 with
      | VTypeFun (t11, t12) ->
          if equal_vtype t11 t2 then Ok (add_type t12 e) else Error ()
      | _ -> Error ())
  | Fix _ -> failwith "TODO"
