open Core
open Utils
open Vtype
open Varname
open Variant_types
open Pattern

type ('tag_e, 'tag_p) expr =
  | UnitLit of 'tag_e
  | IntLit of 'tag_e * int
  | Add of 'tag_e * ('tag_e, 'tag_p) expr * ('tag_e, 'tag_p) expr
  | Neg of 'tag_e * ('tag_e, 'tag_p) expr
  | Subtr of 'tag_e * ('tag_e, 'tag_p) expr * ('tag_e, 'tag_p) expr
  | Mult of 'tag_e * ('tag_e, 'tag_p) expr * ('tag_e, 'tag_p) expr
  | BoolLit of 'tag_e * bool
  | BNot of 'tag_e * ('tag_e, 'tag_p) expr
  | BOr of 'tag_e * ('tag_e, 'tag_p) expr * ('tag_e, 'tag_p) expr
  | BAnd of 'tag_e * ('tag_e, 'tag_p) expr * ('tag_e, 'tag_p) expr
  | Pair of 'tag_e * ('tag_e, 'tag_p) expr * ('tag_e, 'tag_p) expr
  | Eq of 'tag_e * ('tag_e, 'tag_p) expr * ('tag_e, 'tag_p) expr
  | Gt of 'tag_e * ('tag_e, 'tag_p) expr * ('tag_e, 'tag_p) expr
  | GtEq of 'tag_e * ('tag_e, 'tag_p) expr * ('tag_e, 'tag_p) expr
  | Lt of 'tag_e * ('tag_e, 'tag_p) expr * ('tag_e, 'tag_p) expr
  | LtEq of 'tag_e * ('tag_e, 'tag_p) expr * ('tag_e, 'tag_p) expr
  | If of
      'tag_e
      * ('tag_e, 'tag_p) expr
      * ('tag_e, 'tag_p) expr
      * ('tag_e, 'tag_p) expr
  | Var of 'tag_e * string
  | Let of 'tag_e * string * ('tag_e, 'tag_p) expr * ('tag_e, 'tag_p) expr
  | App of 'tag_e * ('tag_e, 'tag_p) expr * ('tag_e, 'tag_p) expr
  | Match of
      'tag_e
      * ('tag_e, 'tag_p) expr
      * ('tag_p pattern * ('tag_e, 'tag_p) expr) Nonempty_list.t
  | Constructor of 'tag_e * string * ('tag_e, 'tag_p) expr
[@@deriving sexp, equal]

let expr_node_map_val_with_result ~(f : 'tag_e -> 'tag_e) :
    ('tag_e, 'tag_p) expr -> 'tag_e * ('tag_e, 'tag_p) expr = function
  | UnitLit v -> (f v, UnitLit (f v))
  | IntLit (v, x) -> (f v, IntLit (f v, x))
  | Add (v, e1, e2) -> (f v, Add (f v, e1, e2))
  | Neg (v, e1) -> (f v, Neg (f v, e1))
  | Subtr (v, e1, e2) -> (f v, Subtr (f v, e1, e2))
  | Mult (v, e1, e2) -> (f v, Mult (f v, e1, e2))
  | BoolLit (v, b) -> (f v, BoolLit (f v, b))
  | BNot (v, e1) -> (f v, BNot (f v, e1))
  | BOr (v, e1, e2) -> (f v, BOr (f v, e1, e2))
  | BAnd (v, e1, e2) -> (f v, BAnd (f v, e1, e2))
  | Pair (v, e1, e2) -> (f v, Pair (f v, e1, e2))
  | Eq (v, e1, e2) -> (f v, Eq (f v, e1, e2))
  | Gt (v, e1, e2) -> (f v, Gt (f v, e1, e2))
  | GtEq (v, e1, e2) -> (f v, GtEq (f v, e1, e2))
  | Lt (v, e1, e2) -> (f v, Lt (f v, e1, e2))
  | LtEq (v, e1, e2) -> (f v, LtEq (f v, e1, e2))
  | If (v, e1, e2, e3) -> (f v, If (f v, e1, e2, e3))
  | Var (v, xname) -> (f v, Var (f v, xname))
  | Let (v, e1, e2, e3) -> (f v, Let (f v, e1, e2, e3))
  | App (v, e1, e2) -> (f v, App (f v, e1, e2))
  | Match (v, e1, cs) -> (f v, Match (f v, e1, cs))
  | Constructor (v, cname, e1) -> (f v, Constructor (f v, cname, e1))

let expr_node_val (e : ('tag_e, 'tag_p) expr) : 'tag_e =
  expr_node_map_val_with_result ~f:Fn.id e |> fst

let expr_node_map_val ~(f : 'tag_e -> 'tag_e) :
    ('tag_e, 'tag_p) expr -> ('tag_e, 'tag_p) expr =
  Fn.compose snd (expr_node_map_val_with_result ~f)

let rec fmap ~(f : 'tag_e1 -> 'tag_e2) (e : ('tag_e1, 'tag_p) expr) :
    ('tag_e2, 'tag_p) expr =
  match e with
  | UnitLit a -> UnitLit (f a)
  | IntLit (a, i) -> IntLit (f a, i)
  | Add (a, e1, e2) -> Add (f a, fmap ~f e1, fmap ~f e2)
  | Neg (a, e) -> Neg (f a, fmap ~f e)
  | Subtr (a, e1, e2) -> Subtr (f a, fmap ~f e1, fmap ~f e2)
  | Mult (a, e1, e2) -> Mult (f a, fmap ~f e1, fmap ~f e2)
  | BoolLit (a, b) -> BoolLit (f a, b)
  | BNot (a, e) -> BNot (f a, fmap ~f e)
  | BOr (a, e1, e2) -> BOr (f a, fmap ~f e1, fmap ~f e2)
  | BAnd (a, e1, e2) -> BAnd (f a, fmap ~f e1, fmap ~f e2)
  | Pair (a, e1, e2) -> Pair (f a, fmap ~f e1, fmap ~f e2)
  | Eq (a, e1, e2) -> Eq (f a, fmap ~f e1, fmap ~f e2)
  | Gt (a, e1, e2) -> Gt (f a, fmap ~f e1, fmap ~f e2)
  | GtEq (a, e1, e2) -> GtEq (f a, fmap ~f e1, fmap ~f e2)
  | Lt (a, e1, e2) -> Lt (f a, fmap ~f e1, fmap ~f e2)
  | LtEq (a, e1, e2) -> LtEq (f a, fmap ~f e1, fmap ~f e2)
  | If (a, e1, e2, e3) -> If (f a, fmap ~f e1, fmap ~f e2, fmap ~f e3)
  | Var (a, vname) -> Var (f a, vname)
  | Let (a, xname, e1, e2) -> Let (f a, xname, fmap ~f e1, fmap ~f e2)
  | App (a, e1, e2) -> App (f a, fmap ~f e1, fmap ~f e2)
  | Match (a, e, cs) ->
      Match
        ( f a,
          fmap ~f e,
          Nonempty_list.map ~f:(fun (p, c_e) -> (p, fmap ~f c_e)) cs )
  | Constructor (a, cname, e) -> Constructor (f a, cname, fmap ~f e)

let rec fmap_pattern ~(f : 'tag_p1 -> 'tag_p2) (e : ('tag_e, 'tag_p1) expr) :
    ('tag_e, 'tag_p2) expr =
  match e with
  | UnitLit v -> UnitLit v
  | IntLit (v, i) -> IntLit (v, i)
  | Add (v, e1, e2) -> Add (v, fmap_pattern ~f e1, fmap_pattern ~f e2)
  | Neg (v, e) -> Neg (v, fmap_pattern ~f e)
  | Subtr (v, e1, e2) -> Subtr (v, fmap_pattern ~f e1, fmap_pattern ~f e2)
  | Mult (v, e1, e2) -> Mult (v, fmap_pattern ~f e1, fmap_pattern ~f e2)
  | BoolLit (v, b) -> BoolLit (v, b)
  | BNot (v, e) -> BNot (v, fmap_pattern ~f e)
  | BOr (v, e1, e2) -> BOr (v, fmap_pattern ~f e1, fmap_pattern ~f e2)
  | BAnd (v, e1, e2) -> BAnd (v, fmap_pattern ~f e1, fmap_pattern ~f e2)
  | Pair (v, e1, e2) -> Pair (v, fmap_pattern ~f e1, fmap_pattern ~f e2)
  | Eq (v, e1, e2) -> Eq (v, fmap_pattern ~f e1, fmap_pattern ~f e2)
  | Gt (v, e1, e2) -> Gt (v, fmap_pattern ~f e1, fmap_pattern ~f e2)
  | GtEq (v, e1, e2) -> GtEq (v, fmap_pattern ~f e1, fmap_pattern ~f e2)
  | Lt (v, e1, e2) -> Lt (v, fmap_pattern ~f e1, fmap_pattern ~f e2)
  | LtEq (v, e1, e2) -> LtEq (v, fmap_pattern ~f e1, fmap_pattern ~f e2)
  | If (v, e1, e2, e3) ->
      If (v, fmap_pattern ~f e1, fmap_pattern ~f e2, fmap_pattern ~f e3)
  | Var (v, name) -> Var (v, name)
  | Let (v, name, e1, e2) ->
      Let (v, name, fmap_pattern ~f e1, fmap_pattern ~f e2)
  | App (v, e1, e2) -> App (v, fmap_pattern ~f e1, fmap_pattern ~f e2)
  | Match (v, e, cs) ->
      Match
        ( v,
          fmap_pattern ~f e,
          Nonempty_list.map
            ~f:(fun (p, c_e) -> (Pattern.fmap ~f p, fmap_pattern ~f c_e))
            cs )
  | Constructor (v, name, e) -> Constructor (v, name, fmap_pattern ~f e)

type plain_expr = (unit, unit) expr [@@deriving sexp, equal]

type ('a, 'b) typed_expr = (vtype * 'a, vtype * 'b) expr
[@@deriving sexp, equal]

type plain_typed_expr = (unit, unit) typed_expr [@@deriving sexp, equal]

let rec expr_to_plain_expr (e : ('tag_e, 'tag_p) expr) : plain_expr =
  match e with
  | UnitLit _ -> UnitLit ()
  | IntLit (_, i) -> IntLit ((), i)
  | Add (_, e1, e2) -> Add ((), expr_to_plain_expr e1, expr_to_plain_expr e2)
  | Neg (_, e) -> Neg ((), expr_to_plain_expr e)
  | Subtr (_, e1, e2) -> Subtr ((), expr_to_plain_expr e1, expr_to_plain_expr e2)
  | Mult (_, e1, e2) -> Mult ((), expr_to_plain_expr e1, expr_to_plain_expr e2)
  | BoolLit (_, b) -> BoolLit ((), b)
  | BNot (_, e) -> BNot ((), expr_to_plain_expr e)
  | BOr (_, e1, e2) -> BOr ((), expr_to_plain_expr e1, expr_to_plain_expr e2)
  | BAnd (_, e1, e2) -> BAnd ((), expr_to_plain_expr e1, expr_to_plain_expr e2)
  | Pair (_, e1, e2) -> Pair ((), expr_to_plain_expr e1, expr_to_plain_expr e2)
  | Eq (_, e1, e2) -> Eq ((), expr_to_plain_expr e1, expr_to_plain_expr e2)
  | Gt (_, e1, e2) -> Gt ((), expr_to_plain_expr e1, expr_to_plain_expr e2)
  | GtEq (_, e1, e2) -> GtEq ((), expr_to_plain_expr e1, expr_to_plain_expr e2)
  | Lt (_, e1, e2) -> Lt ((), expr_to_plain_expr e1, expr_to_plain_expr e2)
  | LtEq (_, e1, e2) -> LtEq ((), expr_to_plain_expr e1, expr_to_plain_expr e2)
  | If (_, e1, e2, e3) ->
      If
        ((), expr_to_plain_expr e1, expr_to_plain_expr e2, expr_to_plain_expr e3)
  | Var (_, vname) -> Var ((), vname)
  | Let (_, xname, e1, e2) ->
      Let ((), xname, expr_to_plain_expr e1, expr_to_plain_expr e2)
  | App (_, e1, e2) -> App ((), expr_to_plain_expr e1, expr_to_plain_expr e2)
  | Match (_, e, cs) ->
      Match
        ( (),
          expr_to_plain_expr e,
          Nonempty_list.map
            ~f:(fun (p, c_e) ->
              (Pattern.fmap ~f:(fun _ -> ()) p, expr_to_plain_expr c_e))
            cs )
  | Constructor (_, cname, e) -> Constructor ((), cname, expr_to_plain_expr e)

exception AstConverionFixError

let ast_to_source_code ?(use_newlines : bool option) :
    ('tag_e, 'tag_p) expr -> string =
  let rec convert ?(bracketed : bool option) (orig_e : ('tag_e, 'tag_p) expr)
      (p : SourceCodeBuilder.state) : SourceCodeBuilder.state =
    let open SourceCodeBuilder in
    let bracketed = Option.value bracketed ~default:true in
    p
    |> (if bracketed then write "(" else Fn.id)
    |> (match orig_e with
       | UnitLit _ -> write "()"
       | IntLit (_, i) -> write (string_of_int i)
       | Add (_, e1, e2) -> convert e1 |.> write " + " |.> convert e2
       | Neg (_, e1) -> write "-" |.> convert e1
       | Subtr (_, e1, e2) -> convert e1 |.> write " - " |.> convert e2
       | Mult (_, e1, e2) -> convert e1 |.> write " * " |.> convert e2
       | BoolLit (_, b) -> write (string_of_bool b)
       | BNot (_, e1) -> write "~" |.> convert e1
       | BOr (_, e1, e2) -> convert e1 |.> write " || " |.> convert e2
       | BAnd (_, e1, e2) -> convert e1 |.> write " && " |.> convert e2
       | Pair (_, e1, e2) ->
           write "(" |.> convert e1 |.> write ", " |.> convert e2 |.> write ")"
       | Eq (_, e1, e2) -> convert e1 |.> write " == " |.> convert e2
       | Gt (_, e1, e2) -> convert e1 |.> write " > " |.> convert e2
       | GtEq (_, e1, e2) -> convert e1 |.> write " >= " |.> convert e2
       | Lt (_, e1, e2) -> convert e1 |.> write " < " |.> convert e2
       | LtEq (_, e1, e2) -> convert e1 |.> write " <= " |.> convert e2
       | If (_, e1, e2, e3) ->
           write "if " |.> convert e1 |.> endline |.> write "then"
           |.> block (convert e2)
           |.> write "else"
           |.> block (convert e3)
           |.> write "end"
       | Var (_, vname) -> write vname
       | Let (_, xname, e1, e2) -> (
           let default_repr : state -> state =
             write "let " |.> write xname |.> write " = "
             |.> block (convert e1)
             |.> write "in"
             |.> block (convert e2)
             |.> write "end"
           in
           match e1 with _ -> default_repr)
       | App (_, e1, e2) -> convert e1 |.> write " " |.> convert e2
       | Match (_, e, cs) ->
           let convert_case ((p : 'tag_p pattern), (c_e : ('tag_e, 'tag_p) expr))
               : state -> state =
             write "| ("
             |.> write (pattern_to_source_code p)
             |.> write ") ->"
             |.> block (convert c_e)
           in
           let blocked_converted_cases : (state -> state) Nonempty_list.t =
             Nonempty_list.map ~f:(convert_case |.> block) cs
           in
           let cases_converter : state -> state =
             Nonempty_list.fold ~init:Fn.id ~f:( |.> ) blocked_converted_cases
           in
           write "match"
           |.> block (convert e)
           |.> write "with" |.> block cases_converter |.> write "end"
       | Constructor (_, cname, e) -> write cname |.> convert e)
    |> if bracketed then write ")" else Fn.id
  in
  SourceCodeBuilder.from_converter ~converter:(convert ~bracketed:false)
    ~use_newlines:(Option.value ~default:true use_newlines)

module QCheck_testing (TagExpr : sig
  type t
end) (TagPat : sig
  type t
end) : sig
  type ast_print_method =
    | NoPrint
    | PrintSexp of (TagExpr.t -> Sexp.t) * (TagPat.t -> Sexp.t)
    | PrintExprSource

  val get_ast_printer_opt :
    ast_print_method -> ((TagExpr.t, TagPat.t) expr -> string) option

  val get_ast_printer : ast_print_method -> (TagExpr.t, TagPat.t) expr -> string
  val default_ast_print_method : ast_print_method

  type gen_vtype =
    | GenVTypeUnit
    | GenVTypeInt
    | GenVTypeBool
    | GenVTypePair of gen_vtype * gen_vtype
    | GenVTypeCustom of string

  val vtype_to_gen_vtype_unsafe : vtype -> gen_vtype

  type gen_options = {
    t : gen_vtype option;
    variant_types : variant_type list;
    top_level_defns : (varname * (vtype * vtype)) list;
    v_gen : TagExpr.t QCheck.Gen.t;
    pat_v_gen : TagPat.t QCheck.Gen.t;
    mrd : int;
  }

  type shrink_options = { preserve_type : bool }

  type arb_options = {
    gen : gen_options;
    print : ast_print_method;
    shrink : shrink_options;
  }

  include
    QCheck_testing_sig
      with type t = (TagExpr.t, TagPat.t) expr
       and type gen_options := gen_options
       and type print_options = ast_print_method
       and type shrink_options := shrink_options
       and type arb_options := arb_options
end = struct
  module TagPatternQCheckTesting = Pattern.QCheck_testing (TagPat)

  type ast_print_method =
    | NoPrint
    | PrintSexp of (TagExpr.t -> Sexp.t) * (TagPat.t -> Sexp.t)
    | PrintExprSource

  let get_ast_printer_opt :
      ast_print_method -> ((TagExpr.t, TagPat.t) expr -> string) option =
    function
    | NoPrint -> None
    | PrintSexp (f_e, f_p) ->
        Some (fun e -> sexp_of_expr f_e f_p e |> Sexp.to_string)
    | PrintExprSource -> Some (ast_to_source_code ~use_newlines:true)

  let get_ast_printer (p : ast_print_method) (e : (TagExpr.t, TagPat.t) expr) :
      string =
    match get_ast_printer_opt p with None -> "" | Some f -> f e

  let default_ast_print_method : ast_print_method = PrintExprSource

  type t = (TagExpr.t, TagPat.t) expr

  type gen_vtype =
    | GenVTypeUnit
    | GenVTypeInt
    | GenVTypeBool
    | GenVTypePair of gen_vtype * gen_vtype
    | GenVTypeCustom of string

  let rec gen_vtype_to_vtype = function
    | GenVTypeUnit -> VTypeUnit
    | GenVTypeInt -> VTypeInt
    | GenVTypeBool -> VTypeBool
    | GenVTypePair (t1, t2) ->
        VTypePair (gen_vtype_to_vtype t1, gen_vtype_to_vtype t2)
    | GenVTypeCustom ct_name -> VTypeCustom ct_name

  let rec vtype_to_gen_vtype_unsafe = function
    | VTypeUnit -> GenVTypeUnit
    | VTypeInt -> GenVTypeInt
    | VTypeBool -> GenVTypeBool
    | VTypePair (t1, t2) ->
        GenVTypePair (vtype_to_gen_vtype_unsafe t1, vtype_to_gen_vtype_unsafe t2)
    | VTypeCustom ct_name -> GenVTypeCustom ct_name
    | VTypeFun _ -> failwith "Can't generate gen_vtype for function type"

  type gen_options = {
    t : gen_vtype option;
    variant_types : variant_type list;
    top_level_defns : (varname * (vtype * vtype)) list;
    v_gen : TagExpr.t QCheck.Gen.t;
    pat_v_gen : TagPat.t QCheck.Gen.t;
    mrd : int;
  }

  type print_options = ast_print_method
  type shrink_options = { preserve_type : bool }

  type arb_options = {
    gen : gen_options;
    print : print_options;
    shrink : shrink_options;
  }

  let gen (initial_opts : gen_options) : t QCheck.Gen.t =
    let open QCheck in
    let open QCheck.Gen in
    let variant_types = initial_opts.variant_types in
    let variant_types_set =
      initial_opts.variant_types |> List.map ~f:fst |> StringSet.of_list
    in
    let v_gen = initial_opts.v_gen in
    let pat_v_gen = initial_opts.pat_v_gen in

    let varname_gen = Varname.QCheck_testing.gen () in
    let rec varname_filter_type
        ( (_ : int * (string * vtype) list -> (TagExpr.t, TagPat.t) expr Gen.t),
          ((_ : int), (ctx : (string * vtype) list)),
          (_ : TagExpr.t) ) (tf : vtype -> bool) :
        (varname * vtype) Gen.t option =
      (* Shorthand for generating a variable node for an existing variable with a given type in the context, filtered by a function of its type *)
      match
        List.filter_map
          ~f:(fun (xname, xt) -> if tf xt then Some (xname, xt) else None)
          ctx
      with
      | [] -> None
      | _ :: _ as varnames -> Some (varnames |> List.map ~f:return |> oneof)
    and gen_e_var_of_type
        (( (_ : int * (string * vtype) list -> (TagExpr.t, TagPat.t) expr Gen.t),
           ((_ : int), (_ : (string * vtype) list)),
           (v : TagExpr.t) ) as param) (t : vtype) :
        (TagExpr.t, TagPat.t) expr Gen.t option =
      (* Shorthand for generating a variable node for an existing variable with a given type in the context *)
      varname_filter_type param (equal_vtype t)
      |> Option.map ~f:(Gen.map (fun (xname, _) -> Var (v, xname)))
    and gen_e_if
        ( (self :
            int * (string * vtype) list -> (TagExpr.t, TagPat.t) expr Gen.t),
          ((d : int), (ctx : (string * vtype) list)),
          (v : TagExpr.t) ) : (TagExpr.t, TagPat.t) expr Gen.t =
      (* Shorthand for generating an if-then-else expression *)
      triple (gen_bool (d - 1, ctx)) (self (d - 1, ctx)) (self (d - 1, ctx))
      >|= fun (e1, e2, e3) -> If (v, e1, e2, e3)
    and gen_e_let_in
        ( (self :
            int * (string * vtype) list -> (TagExpr.t, TagPat.t) expr Gen.t),
          ((d : int), (ctx : (string * vtype) list)),
          (v : TagExpr.t) ) : (TagExpr.t, TagPat.t) expr Gen.t =
      (* Shorthand for generating a let-in expression *)
      pair varname_gen (gen_any_of_type (d - 1, ctx))
      >>= fun (vname, (e1t, e1)) ->
      self (d - 1, List.Assoc.add ~equal:equal_string ctx vname e1t)
      >|= fun e2 -> Let (v, vname, e1, e2)
    and gen_e_app
        ( (_ : int * (string * vtype) list -> (TagExpr.t, TagPat.t) expr Gen.t),
          ((_ : int), (_ : (string * vtype) list)),
          (_ : TagExpr.t) ) (_ : vtype) :
        (TagExpr.t, TagPat.t) expr Gen.t option =
      (* Shorthand for generating a function application expression *)
      (* varname_filter_type
        (self, (d - 1, ctx), v)
        (function VTypeFun (_, x_t2) -> equal_vtype t2 x_t2 | _ -> false)
      |> Option.map ~f:(fun x_gen ->
             x_gen >>= fun (xname, xt) ->
             v_gen >>= fun v2 ->
             match xt with
             | VTypeFun (x_t1, _) ->
                 gen (d - 1, ctx) x_t1 >|= fun e2 -> App (v, Var (v2, xname), e2)
             | _ -> failwith "Variable wasn't of function type, as was expected") *)
      (* TODO - the above code is partially complete, but I need to have a way to only return this
      if the call to `gen` will be able to find something of the return type, which may be a function type *)
      None
    and gen_e_match
        ( (self :
            int * (string * vtype) list -> (TagExpr.t, TagPat.t) expr Gen.t),
          ((d : int), (ctx : (string * vtype) list)),
          (v : TagExpr.t) ) : (TagExpr.t, TagPat.t) expr Gen.t =
      (* Shorthand for generating a match expression *)
      gen_any_of_type (d - 1, ctx) >>= fun (e1_t, e1) ->
      let pat_gen =
        TagPatternQCheckTesting.gen
          {
            t = e1_t;
            get_variant_type_constructors =
              (fun (vt_name : string) ->
                List.find_map_exn
                  ~f:(fun (x_vt_name, cs) ->
                    if equal_string vt_name x_vt_name then Some cs else None)
                  variant_types);
            v_gen = pat_v_gen;
          }
      in
      let case_and_pat_gen =
        pat_gen >>= fun (p, p_ctx_list) ->
        let case_ctx : (string * vtype) list =
          List.fold ~init:ctx
            ~f:(fun acc (x, t) -> List.Assoc.add ~equal:equal_string acc x t)
            p_ctx_list
        in
        self (d - 1, case_ctx) >|= fun e -> (p, e)
      in
      list_size (int_range 1 4) case_and_pat_gen
      >|= Nonempty_list.from_list_unsafe
      >|= fun cs -> Match (v, e1, cs)
    and standard_rec_gen_cases
        ( (self :
            int * (string * vtype) list -> (TagExpr.t, TagPat.t) expr Gen.t),
          ((d : int), (ctx : (string * vtype) list)),
          (v : TagExpr.t) ) (t : vtype) : (TagExpr.t, TagPat.t) expr Gen.t list
        =
      (* The standard recursive generator cases for some provided type *)
      [
        gen_e_if (self, (d, ctx), v) (* If-then-else *);
        gen_e_let_in (self, (d, ctx), v) (* Let-in *);
        gen_e_match (self, (d, ctx), v) (* Match *);
      ]
      @ Option.to_list
          (gen_e_app (self, (d, ctx), v) t (* Function application *))
    and gen_unit (param : int * (string * vtype) list) :
        (TagExpr.t, TagPat.t) expr Gen.t =
      (* Generate an expression that types as unit *)
      fix
        (fun self (d, ctx) ->
          v_gen >>= fun v ->
          let base_cases =
            [ return (UnitLit v) ]
            @ Option.to_list (gen_e_var_of_type (self, (d, ctx), v) VTypeUnit)
          in
          let rec_cases =
            standard_rec_gen_cases (self, (d, ctx), v) VTypeUnit
          in
          if d > 0 then oneof (base_cases @ rec_cases) else oneof base_cases)
        param
    and gen_int (param : int * (string * vtype) list) :
        (TagExpr.t, TagPat.t) expr Gen.t =
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
              ( pair self' self' >|= fun (e1, e2) -> Add (v, e1, e2)
              (* Addition *) );
              (self' >|= fun e -> Neg (v, e) (* Negation *));
              ( pair self' self' >|= fun (e1, e2) -> Subtr (v, e1, e2)
              (* Subtraction *) );
              ( pair self' self' >|= fun (e1, e2) -> Mult (v, e1, e2)
              (* Multiplication *) );
            ]
            @ standard_rec_gen_cases (self, (d, ctx), v) VTypeInt
          in
          if d > 0 then oneof (base_cases @ rec_cases) else oneof base_cases)
        param
    and gen_bool (param : int * (string * vtype) list) :
        (TagExpr.t, TagPat.t) expr Gen.t =
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
              >|= fun (e1, e2) -> Eq (v, e1, e2)
              (* Integer equality *) );
              ( pair self' self' >|= fun (e1, e2) -> Eq (v, e1, e2)
              (* Boolean equality *) );
              ( pair (gen_int (d - 1, ctx)) (gen_int (d - 1, ctx))
              >|= fun (e1, e2) -> Gt (v, e1, e2)
              (* GT *) );
              ( pair (gen_int (d - 1, ctx)) (gen_int (d - 1, ctx))
              >|= fun (e1, e2) -> GtEq (v, e1, e2)
              (* GTEQ *) );
              ( pair (gen_int (d - 1, ctx)) (gen_int (d - 1, ctx))
              >|= fun (e1, e2) -> Lt (v, e1, e2)
              (* LT *) );
              ( pair (gen_int (d - 1, ctx)) (gen_int (d - 1, ctx))
              >|= fun (e1, e2) -> LtEq (v, e1, e2)
              (* LTEQ *) );
            ]
            @ standard_rec_gen_cases (self, (d, ctx), v) VTypeBool
          in
          if d > 0 then oneof (base_cases @ rec_cases) else oneof base_cases)
        param
    and gen_pair ((t1 : vtype), (t2 : vtype))
        (param : int * (string * vtype) list) : (TagExpr.t, TagPat.t) expr Gen.t
        =
      (* Generate an expression that types as a pair of the provided types *)
      let t = VTypePair (t1, t2) in
      fix
        (fun self (d, ctx) ->
          v_gen >>= fun v ->
          let base_cases =
            [
              ( pair (gen (d - 1, ctx) t1) (gen (d - 1, ctx) t2)
              >|= fun (e1, e2) -> Pair (v, e1, e2) );
            ]
            @ Option.to_list (gen_e_var_of_type (self, (d, ctx), v) t)
          in
          let rec_cases = standard_rec_gen_cases (self, (d, ctx), v) t in
          if d > 0 then oneof (base_cases @ rec_cases) else oneof base_cases)
        param
    and gen_variant ((vt_name, cs) : variant_type)
        (param : int * (string * vtype) list) : (TagExpr.t, TagPat.t) expr Gen.t
        =
      (* Generate an expression that types as the provided variant type *)
      let t = VTypeCustom vt_name in
      fix
        (fun self (d, ctx) ->
          v_gen >>= fun v ->
          let base_cases =
            [
              ( oneof (List.map ~f:return cs) >>= fun (c_name, c_t) ->
                gen (d - 1, ctx) c_t >|= fun e' -> Constructor (v, c_name, e')
              );
            ]
            @ Option.to_list (gen_e_var_of_type (self, (d, ctx), v) t)
          in
          let rec_cases = standard_rec_gen_cases (self, (d, ctx), v) t in
          if d > 0 then oneof (base_cases @ rec_cases) else oneof base_cases)
        param
    and gen ((d : int), (ctx : (string * vtype) list)) (t : vtype) :
        (TagExpr.t, TagPat.t) expr Gen.t =
      match t with
      | VTypeUnit -> gen_unit (d, ctx)
      | VTypeInt -> gen_int (d, ctx)
      | VTypeBool -> gen_bool (d, ctx)
      | VTypeFun _ ->
          failwith "Can't generate arbitrary expression of function type"
      | VTypePair (t1, t2) -> gen_pair (t1, t2) (d, ctx)
      | VTypeCustom vt_name ->
          (Option.value_exn
             ~message:
               (sprintf
                  "The variant type specified (%s) doesn't exist in the context"
                  vt_name)
             (List.find
                ~f:(fun (x_vt_name, _) -> equal_string x_vt_name vt_name)
                variant_types)
          |> gen_variant)
            (d, ctx)
    and gen_any_of_type ((d : int), (ctx : (string * vtype) list)) :
        (vtype * (TagExpr.t, TagPat.t) expr) Gen.t =
      Vtype.QCheck_testing.gen { variant_types = variant_types_set; mrd = d }
      >>= fun t ->
      gen (d, ctx) t >|= fun e -> (t, e)
    in
    match initial_opts.t with
    | Some t -> gen (initial_opts.mrd, []) (gen_vtype_to_vtype t)
    | None -> gen_any_of_type (initial_opts.mrd, []) >|= snd

  let print : print_options -> t QCheck.Print.t = get_ast_printer

  let rec shrink (opts : shrink_options) : t QCheck.Shrink.t =
    let open QCheck.Iter in
    let preserve_type = opts.preserve_type in
    let unop_shrink (v, e1)
        (recomb : 'tag_e * ('tag_e, 'tag_p) expr -> ('tag_e, 'tag_p) expr) :
        ('tag_e, 'tag_p) expr QCheck.Iter.t =
      shrink opts e1 >|= fun e1' -> recomb (v, e1')
    in
    let binop_shrink ?(allow_return_subexpr : bool option) (v, e1, e2)
        (recomb :
          'tag_e * ('tag_e, 'tag_p) expr * ('tag_e, 'tag_p) expr ->
          ('tag_e, 'tag_p) expr) : ('tag_e, 'tag_p) expr QCheck.Iter.t =
      let allow_return_subexpr =
        Option.value ~default:true allow_return_subexpr
      in
      (if allow_return_subexpr then return e1 <+> return e2 else empty)
      <+> (shrink opts e1 >|= fun e1' -> recomb (v, e1', e2))
      <+> (shrink opts e2 >|= fun e2' -> recomb (v, e1, e2'))
    in
    function
    | UnitLit _ -> empty
    | IntLit _ -> empty
    | Add (v, e1, e2) ->
        binop_shrink (v, e1, e2) (fun (v', e1', e2') -> Add (v', e1', e2'))
    | Neg (v, e1) -> unop_shrink (v, e1) (fun (v', e1') -> Neg (v', e1'))
    | Subtr (v, e1, e2) ->
        binop_shrink (v, e1, e2) (fun (v', e1', e2') -> Subtr (v', e1', e2'))
    | Mult (v, e1, e2) ->
        binop_shrink (v, e1, e2) (fun (v', e1', e2') -> Mult (v', e1', e2'))
    | BoolLit _ -> empty
    | BNot (v, e1) -> unop_shrink (v, e1) (fun (v', e1') -> BNot (v', e1'))
    | BOr (v, e1, e2) ->
        binop_shrink (v, e1, e2) (fun (v', e1', e2') -> BOr (v', e1', e2'))
    | BAnd (v, e1, e2) ->
        binop_shrink (v, e1, e2) (fun (v', e1', e2') -> BAnd (v', e1', e2'))
    | Pair (v, e1, e2) ->
        binop_shrink (v, e1, e2) (fun (v', e1', e2') -> Pair (v', e1', e2'))
    | Eq (v, e1, e2) ->
        binop_shrink ~allow_return_subexpr:(not preserve_type) (v, e1, e2)
          (fun (v', e1', e2') -> Eq (v', e1', e2'))
    | Gt (v, e1, e2) ->
        binop_shrink ~allow_return_subexpr:(not preserve_type) (v, e1, e2)
          (fun (v', e1', e2') -> Gt (v', e1', e2'))
    | GtEq (v, e1, e2) ->
        binop_shrink ~allow_return_subexpr:(not preserve_type) (v, e1, e2)
          (fun (v', e1', e2') -> GtEq (v', e1', e2'))
    | Lt (v, e1, e2) ->
        binop_shrink ~allow_return_subexpr:(not preserve_type) (v, e1, e2)
          (fun (v', e1', e2') -> Lt (v', e1', e2'))
    | LtEq (v, e1, e2) ->
        binop_shrink ~allow_return_subexpr:(not preserve_type) (v, e1, e2)
          (fun (v', e1', e2') -> LtEq (v', e1', e2'))
    | If (v, e1, e2, e3) ->
        (if not preserve_type then return e1 <+> return e2 <+> return e3
         else empty)
        <+> (shrink opts e1 >|= fun e1' -> If (v, e1', e2, e3))
        <+> (shrink opts e2 >|= fun e2' -> If (v, e1, e2', e3))
        <+> (shrink opts e3 >|= fun e3' -> If (v, e1, e2, e3'))
    | Var _ -> empty
    | Let _ ->
        (* Because of let-rec definitions needing a specific form when using AST to source code,
           this would need a filtered shrink function which I can't be bothered to write at the moment *)
        empty
    | App (v, e1, e2) ->
        binop_shrink ~allow_return_subexpr:(not preserve_type) (v, e1, e2)
          (fun (v', e1', e2') -> App (v', e1', e2'))
    | Match (v, e1, ps) ->
        (if preserve_type then empty else return e1)
        <+> ( (* Try shrinking each case expression *)
              QCheck.Shrink.list_elems
                (fun (c_p, c_e) -> shrink opts c_e >|= fun c_e' -> (c_p, c_e'))
                (Nonempty_list.to_list ps)
            >|= fun ps' -> Match (v, e1, Nonempty_list.from_list_unsafe ps') )
    | Constructor (v, c, e1) ->
        unop_shrink (v, e1) (fun (v', e1') -> Constructor (v', c, e1'))

  let arbitrary (opts : arb_options) : t QCheck.arbitrary =
    QCheck.make ~print:(print opts.print) ~shrink:(shrink opts.shrink)
      (gen opts.gen)
end
