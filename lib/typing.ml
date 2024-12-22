open Core
open Utils
open Vtype
open Pattern
open Ast

type pattern_typing_error = MultipleVariableDefinitions of string
[@@deriving sexp, equal]

let equal_pattern_typing_error_variant x y =
  match (x, y) with
  | MultipleVariableDefinitions _, MultipleVariableDefinitions _ -> true

let print_pattern_typing_error = function
  | MultipleVariableDefinitions xname ->
      sprintf "Variable named \"%s\" has been defined twice in the pattern"
        xname

type typing_error =
  | UndefinedVariable of string
  | TypeMismatch of vtype * vtype
  | PatternTypeMismatch of pattern * vtype * vtype
  | EqualOperatorTypeMistmatch of vtype * vtype
  | ExpectedFunctionOf of vtype
  | PatternTypingError of pattern_typing_error
[@@deriving sexp, equal]

let equal_typing_error_variant x y =
  match (x, y) with
  | UndefinedVariable _, UndefinedVariable _
  | TypeMismatch _, TypeMismatch _
  | EqualOperatorTypeMistmatch _, EqualOperatorTypeMistmatch _
  | ExpectedFunctionOf _, ExpectedFunctionOf _ ->
      true
  | _, _ -> false

let print_typing_error = function
  | UndefinedVariable x -> "Undefined variable: " ^ x
  | TypeMismatch (t1, t2) ->
      sprintf "Type mismatch: expected %s but got %s" (vtype_to_source_code t1)
        (vtype_to_source_code t2)
  | PatternTypeMismatch (p, t1, t2) ->
      sprintf "Type mismatch in pattern \"%s\": expected %s but got %s"
        (pattern_to_source_code p) (vtype_to_source_code t1)
        (vtype_to_source_code t2)
  | EqualOperatorTypeMistmatch (t1, t2) ->
      sprintf "Trying to apply equality operator to %s and %s"
        (vtype_to_source_code t1) (vtype_to_source_code t2)
  | ExpectedFunctionOf t ->
      "Expected a function taking input of " ^ vtype_to_source_code t
  | PatternTypingError err ->
      sprintf "Error typing pattern: %s" (print_pattern_typing_error err)

module type TypingVarContext = sig
  type t

  val empty : t
  val add : t -> string -> vtype -> t
  val find : t -> string -> vtype option
  val singleton : string -> vtype -> t
  val append : t -> t -> t
  val exists : t -> string -> bool
end

module ListTypingVarContext : TypingVarContext = struct
  type t = (string * vtype) list

  let empty = []
  let add ctx x t = List.Assoc.add ctx x t ~equal:String.equal
  let find ctx x = List.Assoc.find ctx x ~equal:String.equal
  let singleton = add empty

  let append ctx1 =
    List.fold ~init:ctx1 ~f:(fun ctx_acc (x, t) -> add ctx_acc x t)

  let exists ctx x = match find ctx x with None -> false | Some _ -> true
end

module TypeChecker (Ctx : TypingVarContext) = struct
  let rec type_pattern (ctx : Ctx.t) (orig_p : pattern) :
      (vtype * Ctx.t, pattern_typing_error) Result.t =
    let open Result in
    match orig_p with
    | PatName (x_name, x_t) ->
        if Ctx.exists ctx x_name then Error (MultipleVariableDefinitions x_name)
        else Ok (x_t, Ctx.add ctx x_name x_t)
    | PatPair (p1, p2) ->
        type_pattern ctx p1 >>= fun (p1_t, ctx_from_p1) ->
        type_pattern ctx_from_p1 p2 >>= fun (p2_t, ctx_final) ->
        Ok (VTypePair (p1_t, p2_t), ctx_final)

  let rec type_expr (ctx : Ctx.t) (orig_e : 'a expr) :
      ((vtype * 'a) expr, typing_error) Result.t =
    let open Result in
    let e_type (e : (vtype * 'a) expr) : vtype = e |> expr_node_val |> fst in
    let be_of_type (exp : vtype) (e : (vtype * 'a) expr) :
        ((vtype * 'a) expr, typing_error) Result.t =
      if equal_vtype exp (e_type e) then Ok e
      else Error (TypeMismatch (exp, e_type e))
    in
    let type_binop
        (recomp :
          (vtype * 'a) expr -> (vtype * 'a) expr -> vtype -> (vtype * 'a) expr)
        (e1 : 'a expr) (e2 : 'a expr) (req_t : vtype) :
        ((vtype * 'a) expr, typing_error) Result.t =
      type_expr ctx e1 >>= fun e1' ->
      type_expr ctx e2 >>= fun e2' ->
      be_of_type req_t e1' >>= fun _ ->
      be_of_type req_t e2' >>= fun _ -> Ok (recomp e1' e2' (e_type e1'))
    in
    let type_unop (recomp : (vtype * 'a) expr -> vtype -> (vtype * 'a) expr)
        (e1 : 'a expr) (req_t : vtype) :
        ((vtype * 'a) expr, typing_error) Result.t =
      type_expr ctx e1 >>= fun e1' ->
      be_of_type req_t e1' >>= fun _ -> Ok (recomp e1' (e_type e1'))
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
      | VTypeInt, _ -> Error (TypeMismatch (VTypeInt, t2))
      | _, _ -> Error (TypeMismatch (VTypeInt, t1))
    in
    match orig_e with
    | UnitLit v -> Ok (UnitLit (VTypeUnit, v))
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
    | Pair (v, e1, e2) ->
        type_expr ctx e1 >>= fun e1' ->
        type_expr ctx e2 >>= fun e2' ->
        let t1 = e_type e1' in
        let t2 = e_type e2' in
        Ok (Pair ((VTypePair (t1, t2), v), e1', e2'))
    | Eq (v, e1, e2) -> (
        type_expr ctx e1 >>= fun e1' ->
        type_expr ctx e2 >>= fun e2' ->
        let t1 = e_type e1' in
        let t2 = e_type e2' in
        match (t1, t2) with
        | VTypeInt, VTypeInt | VTypeBool, VTypeBool ->
            Ok (Eq ((VTypeBool, v), e1', e2'))
        | _, _ -> Error (EqualOperatorTypeMistmatch (t1, t2)))
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
        be_of_type VTypeBool e1' >>= fun _ ->
        type_expr ctx e2 >>= fun e2' ->
        type_expr ctx e3 >>= fun e3' ->
        let t2 = e_type e2' in
        be_of_type t2 e3' >>= fun _ -> Ok (If ((t2, v), e1', e2', e3'))
    | Var (v, xname) -> (
        match Ctx.find ctx xname with
        | Some t -> Ok (Var ((t, v), xname))
        | None -> Error (UndefinedVariable xname))
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
            else Error (TypeMismatch (t11, t2))
        | _ -> Error (ExpectedFunctionOf t1))
    | Fix (v, ((fname, ftype1, ftype2) as fvals), ((xname, xtype) as xvals), e)
      ->
        let ftype = VTypeFun (ftype1, ftype2) in
        if equal_vtype ftype1 xtype then
          type_expr (Ctx.add (Ctx.add ctx fname ftype) xname xtype) e
          >>= fun e' ->
          be_of_type ftype2 e' >>= fun _ ->
          Ok (Fix ((ftype, v), fvals, xvals, e'))
        else Error (TypeMismatch (ftype1, xtype))
    | Match (v, e, cs) ->
        type_expr ctx e >>= fun e' ->
        let t_in = e_type e' in
        (* Type the cases and check them against each other, as well as determining the type of the output *)
        Nonempty_list.fold_result_consume_init ~init:()
          ~f:(fun
              (acc :
                ( unit,
                  vtype * (pattern * (vtype * 'a) expr) Nonempty_list.t )
                Either.t)
              ((p : pattern), (c_e : 'a expr))
            ->
            (* First, try type the pattern *)
            match type_pattern Ctx.empty p with
            | Ok (p_t, p_ctx) ->
                (* Check the pattern's type *)
                if equal_vtype t_in p_t then
                  let case_ctx = Ctx.append ctx p_ctx in
                  (* Then, type the case's expression using the extended context *)
                  type_expr case_ctx c_e >>= fun c_e' ->
                  let t_c_e = e_type c_e' in
                  match acc with
                  | First _ ->
                      (* If this is the first case, use this as the output type *)
                      Ok (t_c_e, Nonempty_list.singleton (p, c_e'))
                  | Second (t_out, cs_prev_rev) ->
                      (* If this isn't the first case, check the case's expression's type *)
                      if equal_vtype t_out t_c_e then
                        Ok (t_out, Nonempty_list.cons (p, c_e') cs_prev_rev)
                      else Error (TypeMismatch (t_out, t_c_e))
                else Error (PatternTypeMismatch (p, t_in, p_t))
            | Error err -> Error (PatternTypingError err))
          cs
        >>| fun ( (t_out : vtype),
                  (cs_typed_rev : (pattern * (vtype * 'a) expr) Nonempty_list.t)
                ) -> Match ((t_out, v), e', Nonempty_list.rev cs_typed_rev)
    | Constructor _ -> failwith "TODO"
end

module ListTypeChecker = TypeChecker (ListTypingVarContext)

let type_expr (e : 'a Ast.expr) :
    ((Vtype.vtype * 'a) Ast.expr, typing_error) result =
  ListTypeChecker.type_expr ListTypingVarContext.empty e
