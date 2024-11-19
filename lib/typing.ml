open Core
open Vtype
open Ast

type typing_error = unit
(* TODO - provide descriptive typing errors *)

module type TypingVarContext = sig
  type t

  val empty : t
  val add : t -> string -> vtype -> t
  val find : t -> string -> vtype option
end

module ListTypingVarContext : TypingVarContext = struct
  type t = (string * vtype) list

  let empty = []
  let add ctx x t = (x, t) :: ctx
  let find ctx x = List.Assoc.find ctx x ~equal:String.equal
end

module TypeExpr (Ctx : TypingVarContext) = struct
  let rec type_expr (ctx : Ctx.t) (e : 'a expr) :
      ((vtype * 'a) expr, typing_error) Result.t =
    let open Result in
    let add_type (t : vtype) (e : 'a expr) : (vtype * 'a) expr =
      e >|= fun x -> (t, x)
    in
    let node_type = Fn.(compose fst expr_node_val) in
    let be_of_type (t : vtype) (e : (vtype * 'a) expr) :
        ((vtype * 'a) expr, unit) Result.t =
      let t_e = node_type e in
      if equal_vtype t_e t then Ok e else Error ()
    in
    let type_binop ?(out_type_f : (vtype -> vtype) option) (e : 'a expr)
        (e1 : 'a expr) (e2 : 'a expr) : ((vtype * 'a) expr, unit) Result.t =
      let out_type_f = match out_type_f with Some f -> f | None -> Fn.id in
      type_expr ctx e1 >>= fun e1' ->
      type_expr ctx e2 >>= fun e2' ->
      let t1 = e1' |> expr_node_val |> fst in
      let t2 = e2' |> expr_node_val |> fst in
      match (t1, t2) with
      | VTypeInt, VTypeInt -> Ok (add_type (out_type_f VTypeInt) e)
      | VTypeInt, _ -> Error ()
      | VTypeBool, VTypeBool -> Ok (add_type (out_type_f VTypeBool) e)
      | VTypeBool, _ -> Error ()
      | VTypeFun (t1, t2), VTypeFun (t3, t4) ->
          if equal_vtype t1 t3 && equal_vtype t2 t4 then
            Ok (add_type (out_type_f (VTypeFun (t1, t2))) e)
          else Error ()
      | VTypeFun _, _ -> Error ()
    in
    match e with
    | IntLit _ as e -> Ok (add_type VTypeInt e)
    | (Add (_, e1, e2) as e)
    | (Subtr (_, e1, e2) as e)
    | (Mult (_, e1, e2) as e) ->
        type_binop e e1 e2 >>= be_of_type VTypeInt
    | Neg (_, e) -> type_expr ctx e >>= be_of_type VTypeInt
    | BoolLit _ -> Ok (add_type VTypeBool e)
    | BNot (_, e) -> type_expr ctx e >>= be_of_type VTypeBool
    | (BOr (_, e1, e2) as e) | (BAnd (_, e1, e2) as e) ->
        type_binop e e1 e2 >>= be_of_type VTypeBool
    | Eq (_, e1, e2) -> (
        type_expr ctx e1 >>= fun e1' ->
        type_expr ctx e2 >>= fun e2' ->
        let t1 = e1' |> expr_node_val |> fst in
        let t2 = e2' |> expr_node_val |> fst in
        match (t1, t2) with
        | VTypeInt, VTypeInt | VTypeBool, VTypeBool -> Ok (add_type VTypeBool e)
        | _, _ -> Error ())
    | (Gt (_, e1, e2) as e)
    | (GtEq (_, e1, e2) as e)
    | (Lt (_, e1, e2) as e)
    | (LtEq (_, e1, e2) as e) ->
        type_binop e e1 e2 ~out_type_f:(fun t ->
            match t with VTypeInt | VTypeBool -> VTypeBool | t -> t)
        >>= be_of_type VTypeBool
    | If (_, e1, e2, e3) ->
        type_expr ctx e1 >>= be_of_type VTypeBool >>= fun _ ->
        type_expr ctx e2 >>= fun e2' ->
        type_expr ctx e3 >>= fun e3' ->
        let t2 = e2' |> expr_node_val |> fst in
        let t3 = e3' |> expr_node_val |> fst in
        if equal_vtype t2 t3 then Ok (add_type t2 e) else Error ()
    | Var (_, xname) as e -> (
        match Ctx.find ctx xname with
        | Some t -> Ok (add_type t e)
        | None -> Error ())
    | Let (_, xname, e1, e2) as e ->
        type_expr ctx e1 >>= fun e1' ->
        let t1 = e1' |> expr_node_val |> fst in
        type_expr (Ctx.add ctx xname t1) e2 >>= fun e2' ->
        let t2 = e2' |> expr_node_val |> fst in
        Ok (add_type t2 e)
    | Fun (_, (xname, xtype), e') as e ->
        type_expr (Ctx.add ctx xname xtype) e' >>= fun e' ->
        let t = e' |> expr_node_val |> fst in
        Ok (add_type (VTypeFun (xtype, t)) e)
    | App (_, e1, e2) as e -> (
        type_expr ctx e1 >>= fun e1' ->
        type_expr ctx e2 >>= fun e2' ->
        let t1 = e1' |> expr_node_val |> fst in
        let t2 = e2' |> expr_node_val |> fst in
        match t1 with
        | VTypeFun (t11, t12) ->
            if equal_vtype t11 t2 then Ok (add_type t12 e) else Error ()
        | _ -> Error ())
    | Fix _ -> failwith "TODO"
end

module ListTypeExpr = TypeExpr (ListTypingVarContext)

let type_expr (e : 'a Ast.expr) :
    ((Vtype.vtype * 'a) Ast.expr, typing_error) result =
  ListTypeExpr.type_expr ListTypingVarContext.empty e
