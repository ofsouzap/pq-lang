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
  let add ctx x t = List.Assoc.add ctx x t ~equal:String.equal
  let find ctx x = List.Assoc.find ctx x ~equal:String.equal
end

module TypeExpr (Ctx : TypingVarContext) = struct
  let rec type_expr (ctx : Ctx.t) (orig_e : 'a expr) :
      ((vtype * 'a) expr, typing_error) Result.t =
    let open Result in
    let e_type (e : (vtype * 'a) expr) : vtype = e |> expr_node_val |> fst in
    let type_binop
        (recomp :
          (vtype * 'a) expr -> (vtype * 'a) expr -> vtype -> (vtype * 'a) expr)
        (e1 : 'a expr) (e2 : 'a expr) (req_t : vtype) :
        ((vtype * 'a) expr, unit) Result.t =
      type_expr ctx e1 >>= fun e1' ->
      type_expr ctx e2 >>= fun e2' ->
      let t1 = e_type e1' in
      let t2 = e_type e2' in
      match (t1, t2, req_t) with
      | VTypeInt, VTypeInt, VTypeInt -> Ok (recomp e1' e2' VTypeInt)
      | VTypeInt, _, _ -> Error ()
      | VTypeBool, VTypeBool, VTypeBool -> Ok (recomp e1' e2' VTypeBool)
      | VTypeBool, _, _ -> Error ()
      | VTypeFun (t11, t12), VTypeFun (t21, t22), VTypeFun (t31, t32) ->
          if
            equal_vtype t11 t21 && equal_vtype t21 t31 && equal_vtype t12 t22
            && equal_vtype t22 t32
          then Ok (recomp e1' e2' (VTypeFun (t11, t12)))
          else Error ()
      | VTypeFun _, _, _ -> Error ()
    in
    let type_unop (recomp : (vtype * 'a) expr -> vtype -> (vtype * 'a) expr)
        (e' : 'a expr) (req_t : vtype) : ((vtype * 'a) expr, unit) Result.t =
      type_expr ctx e' >>= fun typed_e' ->
      let t = e_type typed_e' in
      if equal_vtype t req_t then Ok (recomp typed_e' t) else Error ()
    in
    let type_int_compare
        (recomp : (vtype * 'a) expr -> (vtype * 'a) expr -> (vtype * 'a) expr)
        (e1 : 'a expr) (e2 : 'a expr) =
      type_expr ctx e1 >>= fun e1' ->
      type_expr ctx e2 >>= fun e2' ->
      let t1 = e_type e1' in
      let t2 = e_type e2' in
      match (t1, t2) with
      | VTypeInt, VTypeInt -> Ok (recomp e1' e2')
      | _, _ -> Error ()
    in
    match orig_e with
    | IntLit (v, x) -> Ok (IntLit ((VTypeInt, v), x))
    | Add (v, e1, e2) ->
        type_binop (fun e1' e2' t -> Add ((t, v), e1', e2')) e1 e2 VTypeInt
    | Subtr (v, e1, e2) ->
        type_binop (fun e1' e2' t -> Subtr ((t, v), e1', e2')) e1 e2 VTypeInt
    | Mult (v, e1, e2) ->
        type_binop (fun e1' e2' t -> Mult ((t, v), e1', e2')) e1 e2 VTypeInt
    | Neg (v, e1) -> type_unop (fun e1' t -> Neg ((t, v), e1')) e1 VTypeInt
    | BoolLit (v, x) -> Ok (BoolLit ((VTypeBool, v), x))
    | BNot (v, e1) -> type_unop (fun e1' t -> BNot ((t, v), e1')) e1 VTypeBool
    | BOr (v, e1, e2) ->
        type_binop (fun e1' e2' t -> BOr ((t, v), e1', e2')) e1 e2 VTypeBool
    | BAnd (v, e1, e2) ->
        type_binop (fun e1' e2' t -> BAnd ((t, v), e1', e2')) e1 e2 VTypeBool
    | Eq (v, e1, e2) -> (
        type_expr ctx e1 >>= fun e1' ->
        type_expr ctx e2 >>= fun e2' ->
        let t1 = e_type e1' in
        let t2 = e_type e2' in
        match (t1, t2) with
        | VTypeInt, VTypeInt | VTypeBool, VTypeBool ->
            Ok (Eq ((VTypeBool, v), e1', e2'))
        | _, _ -> Error ())
    | Gt (v, e1, e2) ->
        type_int_compare (fun e1' e2' -> Gt ((VTypeBool, v), e1', e2')) e1 e2
    | GtEq (v, e1, e2) ->
        type_int_compare (fun e1' e2' -> GtEq ((VTypeBool, v), e1', e2')) e1 e2
    | Lt (v, e1, e2) ->
        type_int_compare (fun e1' e2' -> Lt ((VTypeBool, v), e1', e2')) e1 e2
    | LtEq (v, e1, e2) ->
        type_int_compare (fun e1' e2' -> LtEq ((VTypeBool, v), e1', e2')) e1 e2
    | If (v, e1, e2, e3) ->
        type_expr ctx e1 >>= fun e1' ->
        type_expr ctx e2 >>= fun e2' ->
        type_expr ctx e3 >>= fun e3' ->
        let t1 = e_type e1' in
        let t2 = e_type e2' in
        let t3 = e_type e3' in
        if equal_vtype t1 VTypeBool && equal_vtype t2 t3 then
          Ok (If ((t2, v), e1', e2', e3'))
        else Error ()
    | Var (v, xname) -> (
        match Ctx.find ctx xname with
        | Some t -> Ok (Var ((t, v), xname))
        | None -> Error ())
    | Let (v, xname, e1, e2) ->
        type_expr ctx e1 >>= fun e1' ->
        let t1 = e_type e1' in
        type_expr (Ctx.add ctx xname t1) e2 >>= fun e2' ->
        let t2 = e_type e2' in
        Ok (Let ((t2, v), xname, e1', e2'))
    | Fun (v, (xname, xtype), e') ->
        type_expr (Ctx.add ctx xname xtype) e' >>= fun e' ->
        let t = e_type e' in
        Ok (Fun ((VTypeFun (xtype, t), v), (xname, xtype), e'))
    | App (v, e1, e2) -> (
        type_expr ctx e1 >>= fun e1' ->
        type_expr ctx e2 >>= fun e2' ->
        let t1 = e_type e1' in
        let t2 = e_type e2' in
        match t1 with
        | VTypeFun (t11, t12) ->
            if equal_vtype t11 t2 then Ok (App ((t12, v), e1', e2'))
            else Error ()
        | _ -> Error ())
    | Fix (v, ((fname, ftype1, ftype2) as fvals), ((xname, xtype) as xvals), e)
      ->
        let ftype = VTypeFun (ftype1, ftype2) in
        if equal_vtype ftype1 xtype then
          type_expr (Ctx.add (Ctx.add ctx fname ftype) xname xtype) e
          >>= fun e' ->
          let t = e_type e' in
          if equal_vtype t ftype2 then Ok (Fix ((ftype, v), fvals, xvals, e'))
          else Error ()
        else Error ()
end

module ListTypeExpr = TypeExpr (ListTypingVarContext)

let type_expr (e : 'a Ast.expr) :
    ((Vtype.vtype * 'a) Ast.expr, typing_error) result =
  ListTypeExpr.type_expr ListTypingVarContext.empty e
