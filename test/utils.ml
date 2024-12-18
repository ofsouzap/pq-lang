open Core
open Pq_lang
open Vtype
open Ast
open Parser
open Ast_executor

type 'a ast_print_method =
  | NoPrint
  | PrintSexp of ('a -> Sexp.t)
  | PrintExprSource

let get_asp_printer_opt : 'a ast_print_method -> ('a expr -> string) option =
  function
  | NoPrint -> None
  | PrintSexp f -> Some (fun e -> Ast.sexp_of_expr f e |> Sexp.to_string)
  | PrintExprSource -> Some ast_to_source_code

let get_asp_printer (p : 'a ast_print_method) (e : 'a expr) : string =
  match get_asp_printer_opt p with None -> "" | Some f -> f e

let default_ast_print_method : 'a ast_print_method = PrintExprSource
let max_gen_rec_depth : int = 10
let default_max_gen_rec_depth : int = max_gen_rec_depth

let sexp_of_token = function
  | END -> Sexp.Atom "END"
  | IF -> Sexp.Atom "IF"
  | THEN -> Sexp.Atom "THEN"
  | ELSE -> Sexp.Atom "ELSE"
  | LET -> Sexp.Atom "LET"
  | REC -> Sexp.Atom "REC"
  | IN -> Sexp.Atom "IN"
  | TRUE -> Sexp.Atom "TRUE"
  | FALSE -> Sexp.Atom "FALSE"
  | FUN -> Sexp.Atom "FUN"
  | INT -> Sexp.Atom "INT"
  | BOOL -> Sexp.Atom "BOOL"
  | PLUS -> Sexp.Atom "PLUS"
  | MINUS -> Sexp.Atom "MINUS"
  | STAR -> Sexp.Atom "STAR"
  | LPAREN -> Sexp.Atom "LPAREN"
  | RPAREN -> Sexp.Atom "RPAREN"
  | BNOT -> Sexp.Atom "BNOT"
  | BOR -> Sexp.Atom "BOR"
  | BAND -> Sexp.Atom "BAND"
  | ASSIGN -> Sexp.Atom "ASSIGN"
  | EQ -> Sexp.Atom "EQ"
  | GT -> Sexp.Atom "GT"
  | GTEQ -> Sexp.Atom "GTEQ"
  | LT -> Sexp.Atom "LT"
  | LTEQ -> Sexp.Atom "LTEQ"
  | ARROW -> Sexp.Atom "ARROW"
  | COLON -> Sexp.Atom "COLON"
  | COMMA -> Sexp.Atom "COMMA"
  | INTLIT i -> Sexp.List [ Sexp.Atom "INTLIT"; Sexp.Atom (string_of_int i) ]
  | NAME n -> Sexp.List [ Sexp.Atom "NAME"; Sexp.Atom n ]
  | EOF -> Sexp.Atom "EOF"

let token_printer tokens =
  String.concat ~sep:", "
    (List.map ~f:(Fn.compose Sexp.to_string sexp_of_token) tokens)

let override_compare_exec_res (a : exec_res) (b : exec_res) : bool =
  match (a, b) with
  | Err e1, Err e2 -> (
      match (e1, e2) with
      | TypingError _, TypingError _ -> true
      | _ -> exec_res_compare a b)
  | _ -> exec_res_compare a b

let lexer_keywords : string list =
  [
    "end";
    "if";
    "then";
    "else";
    "let";
    "rec";
    "in";
    "true";
    "false";
    "fun";
    "int";
    "bool";
  ]

(** Generator for a small-length, non-empty string of lowercase characters *)
let varname_gen : string QCheck.Gen.t =
  let open QCheck.Gen in
  fix
    (fun self _ ->
      int_range 1 5 >>= fun n ->
      list_repeat n (char_range 'a' 'z') >|= String.of_char_list >>= fun s ->
      if List.mem lexer_keywords s ~equal:String.equal then self ()
      else return s)
    ()

let vtype_gen (d : int) : vtype QCheck.Gen.t =
  let open QCheck.Gen in
  let gen =
    fix (fun self d ->
        let self' = self (d - 1) in
        let base_cases = [ return VTypeInt; return VTypeBool ] in
        let rec_cases =
          [ (pair self' self' >|= fun (t1, t2) -> VTypeFun (t1, t2)) ]
        in
        if d > 0 then oneof (base_cases @ rec_cases) else oneof base_cases)
  in
  gen d

let typed_var_gen (d : int) : (string * vtype) QCheck.Gen.t =
  let open QCheck.Gen in
  pair varname_gen (vtype_gen d)

let gen_unique_pair ~(equal : 'a -> 'a -> bool) (g : 'a QCheck.Gen.t) :
    ('a * 'a) QCheck.Gen.t =
  let open QCheck.Gen in
  fix
    (fun self _ ->
      g >>= fun x ->
      g >>= fun y -> if equal x y then self () else return (x, y))
    ()

module TestingVarCtx : sig
  include Typing.TypingVarContext

  (** Get a list of all the variables who have the given type *)
  val varnames_of_type : vtype -> t -> string list
end = struct
  type t = (string * vtype) list

  let empty = []
  let add ctx x t = List.Assoc.add ctx x t ~equal:String.equal
  let find ctx x = List.Assoc.find ctx x ~equal:String.equal

  let varnames_of_type (t : vtype) (ctx : t) : string list =
    List.filter_map
      ~f:(fun (vname, vtype) ->
        if equal_vtype vtype t then Some vname else None)
      ctx
end

let ast_expr_arb ?(t : vtype option) (print : 'a ast_print_method)
    (v_gen : 'a QCheck.Gen.t) : 'a expr QCheck.arbitrary =
  let open QCheck in
  let open QCheck.Gen in
  let rec gen_e_var_of_type
      ( (_ : int * TestingVarCtx.t -> 'a expr Gen.t),
        ((_ : int), (ctx : TestingVarCtx.t)),
        (v : 'a) ) (t : vtype) : 'a expr Gen.t option =
    (* Shorthand for generating a variable node for an existing variable with a given type in the context *)
    match TestingVarCtx.varnames_of_type t ctx with
    | [] -> None
    | _ :: _ as varnames ->
        Some (varnames |> List.map ~f:return |> oneof >|= fun x -> Var (v, x))
  and gen_e_if
      ( (self : int * TestingVarCtx.t -> 'a expr Gen.t),
        ((d : int), (ctx : TestingVarCtx.t)),
        (v : 'a) ) : 'a expr Gen.t =
    (* Shorthand for generating an if-then-else expression *)
    triple (gen_bool (d - 1, ctx)) (self (d - 1, ctx)) (self (d - 1, ctx))
    >|= fun (e1, e2, e3) -> If (v, e1, e2, e3)
  and gen_e_let_in
      ( (self : int * TestingVarCtx.t -> 'a expr Gen.t),
        ((d : int), (ctx : TestingVarCtx.t)),
        (v : 'a) ) : 'a expr Gen.t =
    (* Shorthand for generating a let-in expression *)
    pair varname_gen (gen_any_of_type (d - 1, ctx))
    >>= fun (vname, (e1t, e1)) ->
    self (d - 1, TestingVarCtx.add ctx vname e1t) >|= fun e2 ->
    Let (v, vname, e1, e2)
  and gen_e_app
      ( (_ : int * TestingVarCtx.t -> 'a expr Gen.t),
        ((d : int), (ctx : TestingVarCtx.t)),
        (v : 'a) ) (t2 : vtype) : 'a expr Gen.t =
    (* Shorthand for generating a function application expression *)
    vtype_gen d >>= fun t1 ->
    pair (gen_fun (t1, t2) (d - 1, ctx)) (gen (d - 1, ctx) t1)
    >|= fun (e1, e2) -> App (v, e1, e2)
  and gen_e_let_rec
      ( (self : int * TestingVarCtx.t -> 'a expr Gen.t),
        ((d : int), (ctx : TestingVarCtx.t)),
        (v : 'a) ) : 'a expr Gen.t =
    (* Shorthand for generating a let-rec expression *)
    pair
      (gen_unique_pair ~equal:equal_string varname_gen)
      (pair (vtype_gen d) (vtype_gen d))
    >>= fun ((fname, xname), (ftype1, ftype2)) ->
    let ctx_with_f = TestingVarCtx.add ctx fname (VTypeFun (ftype1, ftype2)) in
    let ctx_with_fx = TestingVarCtx.add ctx_with_f xname ftype1 in
    pair (gen (d - 1, ctx_with_fx) ftype2) (self (d - 1, ctx_with_f))
    >|= fun (e1, e2) ->
    Let (v, fname, Fix (v, (fname, ftype1, ftype2), (xname, ftype1), e1), e2)
  and gen_int (param : int * TestingVarCtx.t) : 'a expr Gen.t =
    (* Generate an expression that types as integer *)
    fix
      (fun self (d, ctx) ->
        let self' = self (d - 1, ctx) in
        v_gen >>= fun v ->
        let base_cases =
          [ (nat >|= fun n -> IntLit (v, n)) ]
          @ Option.to_list (gen_e_var_of_type (self, (d, ctx), v) VTypeInt)
        in
        let rec_cases =
          [
            (pair self' self' >|= fun (e1, e2) -> Add (v, e1, e2) (* Addition *));
            (self' >|= fun e -> Neg (v, e) (* Negation *));
            ( pair self' self' >|= fun (e1, e2) -> Subtr (v, e1, e2)
              (* Subtraction *) );
            ( pair self' self' >|= fun (e1, e2) -> Mult (v, e1, e2)
              (* Multiplication *) );
            gen_e_if (self, (d, ctx), v) (* If-then-else *);
            gen_e_let_in (self, (d, ctx), v) (* Let-in *);
            gen_e_app (self, (d, ctx), v) VTypeInt (* Function application *);
            gen_e_let_rec (self, (d, ctx), v) (* Let-rec *);
          ]
        in
        if d > 0 then oneof (base_cases @ rec_cases) else oneof base_cases)
      param
  and gen_bool (param : int * TestingVarCtx.t) : 'a expr Gen.t =
    (* Generate an expression that types as boolean *)
    fix
      (fun self (d, ctx) ->
        let self' = self (d - 1, ctx) in
        v_gen >>= fun v ->
        let base_cases =
          [ (bool >|= fun n -> BoolLit (v, n)) ]
          @ Option.to_list (gen_e_var_of_type (self, (d, ctx), v) VTypeBool)
        in
        let rec_cases =
          [
            (self' >|= fun e -> BNot (v, e) (* Negation *));
            ( pair self' self' >|= fun (e1, e2) -> BOr (v, e1, e2)
              (* Disjunction *) );
            ( pair self' self' >|= fun (e1, e2) -> BAnd (v, e1, e2)
              (* Conjunction *) );
            ( pair (gen_int (d - 1, ctx)) (gen_int (d - 1, ctx))
            >|= fun (e1, e2) -> Eq (v, e1, e2) (* Integer equality *) );
            ( pair self' self' >|= fun (e1, e2) -> Eq (v, e1, e2)
              (* Boolean equality *) );
            ( pair (gen_int (d - 1, ctx)) (gen_int (d - 1, ctx))
            >|= fun (e1, e2) -> Gt (v, e1, e2) (* GT *) );
            ( pair (gen_int (d - 1, ctx)) (gen_int (d - 1, ctx))
            >|= fun (e1, e2) -> GtEq (v, e1, e2) (* GTEQ *) );
            ( pair (gen_int (d - 1, ctx)) (gen_int (d - 1, ctx))
            >|= fun (e1, e2) -> Lt (v, e1, e2) (* LT *) );
            ( pair (gen_int (d - 1, ctx)) (gen_int (d - 1, ctx))
            >|= fun (e1, e2) -> LtEq (v, e1, e2) (* LTEQ *) );
            gen_e_if (self, (d, ctx), v) (* If-then-else *);
            gen_e_let_in (self, (d, ctx), v) (* Let-in *);
            gen_e_app (self, (d, ctx), v) VTypeBool (* Function application *);
            gen_e_let_rec (self, (d, ctx), v) (* Let-rec *);
          ]
        in
        if d > 0 then oneof (base_cases @ rec_cases) else oneof base_cases)
      param
  and gen_fun ((t1 : vtype), (t2 : vtype)) (param : int * TestingVarCtx.t) :
      'a expr Gen.t =
    (* Generate an expression that has type of t1 -> t2
       Note, functions have no base cases so recursion depth cannot be used to terminate it safely *)
    let t = VTypeFun (t1, t2) in
    fix
      (fun self (d, ctx) ->
        v_gen >>= fun v ->
        let base_cases =
          [
            ( varname_gen >>= fun vname ->
              gen (d - 1, TestingVarCtx.add ctx vname t1) t2 >|= fun e ->
              Ast.Fun (v, (vname, t1), e) (* Function value *) );
          ]
          @ Option.to_list (gen_e_var_of_type (self, (d, ctx), v) t)
        in
        let rec_cases =
          [
            gen_e_if (self, (d, ctx), v) (* If-then-else *);
            gen_e_let_in (self, (d, ctx), v) (* Let-in *);
            gen_e_app (self, (d, ctx), v) t (* Function application *);
            gen_e_let_rec (self, (d, ctx), v) (* Let-rec *);
          ]
        in
        if d > 0 then oneof (base_cases @ rec_cases) else oneof base_cases)
      param
  and gen_any_of_type ((d : int), (ctx : TestingVarCtx.t)) :
      (vtype * 'a expr) Gen.t =
    vtype_gen d >>= fun t ->
    (match t with
    | VTypeInt -> gen_int (d, ctx)
    | VTypeBool -> gen_bool (d, ctx)
    | VTypeFun (t1, t2) -> gen_fun (t1, t2) (d, ctx)
    | VTypePair _ -> failwith "TODO")
    >|= fun e -> (t, e)
  and gen ((d : int), (ctx : TestingVarCtx.t)) (t : vtype) : 'a expr Gen.t =
    match t with
    | VTypeInt -> gen_int (d, ctx)
    | VTypeBool -> gen_bool (d, ctx)
    | VTypeFun (t1, t2) -> gen_fun (t1, t2) (d, ctx)
    | VTypePair _ -> failwith "TODO"
  in
  let make_fn g =
    match get_asp_printer_opt print with
    | None -> make g
    | Some printer -> make ~print:printer g
  in
  make_fn
    (match t with
    | Some t -> gen (max_gen_rec_depth, TestingVarCtx.empty) t
    | None -> gen_any_of_type (max_gen_rec_depth, TestingVarCtx.empty) >|= snd)

let ast_expr_arb_any print v_gen = ast_expr_arb print v_gen

let plain_ast_expr_arb_any : unit expr QCheck.arbitrary =
  ast_expr_arb_any PrintExprSource QCheck.Gen.unit
