open Core
open Utils

module type S = sig
  module Pattern : Pattern.S
  module Expr : Expr.S with module Pattern = Pattern

  (** A single equality constructor on a quotient type *)
  type ('tag_e, 'tag_p) eqcons = {
    bindings : (Varname.t * Vtype.t) list;
        (** The variable bindings for the equality constructor of the equality
            constructor *)
    body : 'tag_p Pattern.t * ('tag_e, 'tag_p) Expr.t;
        (** The equality definition of the equality constructor *)
        (* TODO - instead of having "body" which is the pattern and the expression,
      have something like body_pattern and body_expr, to make it nicer to use *)
  }
  [@@deriving sexp, equal]

  type plain_eqcons = (unit, unit) eqcons [@@deriving sexp, equal]

  type ('tag_e, 'tag_p) typed_eqcons =
    (Vtype.t * 'tag_e, Vtype.t * 'tag_p) eqcons
  [@@deriving sexp, equal]

  (** Get all names used in a quotient type equality constructor *)
  val eqcons_existing_names : ('tag_e, 'tag_p) eqcons -> StringSet.t

  val eqcons_fmap_expr :
    f:('tag_e1 -> 'tag_e2) ->
    ('tag_e1, 'tag_p) eqcons ->
    ('tag_e2, 'tag_p) eqcons

  val eqcons_fmap_pattern :
    f:('tag_p1 -> 'tag_p2) ->
    ('tag_e, 'tag_p1) eqcons ->
    ('tag_e, 'tag_p2) eqcons

  val eqcons_to_plain_eqcons : ('tag_e, 'tag_p) eqcons -> plain_eqcons

  (** Convert a quotient type equality constructor definition into source code.
  *)
  val eqcons_to_source_code :
    ?use_newlines:bool -> ('tag_e, 'tag_p) eqcons -> string

  (* TODO - QCheck_testing submodule for eqcons *)

  (** A quotient type: a variant type with a list of quotients *)
  type ('tag_e, 'tag_p) t = {
    name : string;  (** The name of the quotient type *)
    base_type_name : string;
        (** The name of the variant/quotient type that the quotient type is
            based on *)
    eqconss : ('tag_e, 'tag_p) eqcons list;
        (** The list of equality constructors for the quotient type *)
  }
  [@@deriving sexp, equal]

  type plain_t = (unit, unit) t [@@deriving sexp, equal]

  type ('tag_e, 'tag_p) typed_t = (Vtype.t * 'tag_e, Vtype.t * 'tag_p) t
  [@@deriving sexp, equal]

  (** Get all names used in a quotient type definition *)
  val existing_names : ('tag_e, 'tag_p) t -> StringSet.t

  val fmap_expr :
    f:('tag_e1 -> 'tag_e2) -> ('tag_e1, 'tag_p) t -> ('tag_e2, 'tag_p) t

  val fmap_pattern :
    f:('tag_p1 -> 'tag_p2) -> ('tag_e, 'tag_p1) t -> ('tag_e, 'tag_p2) t

  val to_plain_t : ('tag_e, 'tag_p) t -> plain_t

  (** Convert a quotient type definition into source code. *)
  val to_source_code : ?use_newlines:bool -> ('tag_e, 'tag_p) t -> string

  (* TODO - QCheck_testing submodule for t *)
end

module Make (Expr : Expr.S) :
  S with module Pattern = Expr.Pattern and module Expr = Expr = struct
  module Pattern = Expr.Pattern
  module Expr = Expr

  type ('tag_e, 'tag_p) eqcons = {
    bindings : (Varname.t * Vtype.t) list;
    body : 'tag_p Pattern.t * ('tag_e, 'tag_p) Expr.t;
  }
  [@@deriving sexp, equal]

  type plain_eqcons = (unit, unit) eqcons [@@deriving sexp, equal]

  type ('tag_e, 'tag_p) typed_eqcons =
    (Vtype.t * 'tag_e, Vtype.t * 'tag_p) eqcons
  [@@deriving sexp, equal]

  let eqcons_existing_names (eqcons : ('tag_e, 'tag_p) eqcons) : StringSet.t =
    let bindings_names =
      eqcons.bindings |> List.map ~f:(fun (v, _) -> v) |> StringSet.of_list
    in
    let p, e = eqcons.body in
    Set.union bindings_names
      (Set.union (Pattern.existing_names p) (Expr.existing_names e))

  let eqcons_fmap_expr ~(f : 'tag_e1 -> 'tag_e2)
      (eqcons : ('tag_e1, 'tag_p) eqcons) : ('tag_e2, 'tag_p) eqcons =
    { eqcons with body = (fst eqcons.body, snd eqcons.body |> Expr.fmap ~f) }

  let eqcons_fmap_pattern ~(f : 'tag_p1 -> 'tag_p2)
      (eqcons : ('tag_e, 'tag_p1) eqcons) : ('tag_e, 'tag_p2) eqcons =
    {
      eqcons with
      body =
        ( fst eqcons.body |> Pattern.fmap ~f,
          snd eqcons.body |> Expr.fmap_pattern ~f );
    }

  let eqcons_to_plain_eqcons (eqcons : ('tag_e, 'tag_p) eqcons) : plain_eqcons =
    eqcons
    |> eqcons_fmap_expr ~f:(fun _ -> ())
    |> eqcons_fmap_pattern ~f:(fun _ -> ())

  let eqcons_to_source_code ?(use_newlines : bool option)
      (eqcons : ('tag_e, 'tag_p) eqcons) : string =
    let bindings_str : string =
      match eqcons.bindings with
      | [] -> "()"
      | h :: ts ->
          Nonempty_list.(
            map
              ~f:(fun (v, vt) ->
                sprintf "(%s : %s)" v (Vtype.to_source_code vt))
              (make (h, ts)))
          |> Nonempty_list.to_list |> String.concat ~sep:" -> "
          |> fun str -> String.append str " => "
    in
    let p, e = eqcons.body in
    sprintf "%s(%s) == (%s)" bindings_str (Pattern.to_source_code p)
      (Expr.to_source_code ?use_newlines e)

  type ('tag_e, 'tag_p) t = {
    name : string;
    base_type_name : string;
    eqconss : ('tag_e, 'tag_p) eqcons list;
  }
  [@@deriving sexp, equal]

  type plain_t = (unit, unit) t [@@deriving sexp, equal]

  type ('tag_e, 'tag_p) typed_t = (Vtype.t * 'tag_e, Vtype.t * 'tag_p) t
  [@@deriving sexp, equal]

  let existing_names (qt : ('tag_e, 'tag_p) t) : StringSet.t =
    let eqcons_names =
      List.fold ~init:StringSet.empty
        ~f:(fun acc eqcons -> Set.union acc (eqcons_existing_names eqcons))
        qt.eqconss
    in
    Set.union
      (StringSet.singleton qt.name)
      (Set.union (StringSet.singleton qt.base_type_name) eqcons_names)

  let fmap_expr ~(f : 'tag_e1 -> 'tag_e2) :
      ('tag_e1, 'tag_p) t -> ('tag_e2, 'tag_p) t =
   fun qt -> { qt with eqconss = List.map ~f:(eqcons_fmap_expr ~f) qt.eqconss }

  let fmap_pattern ~(f : 'tag_p1 -> 'tag_p2) :
      ('tag_e, 'tag_p1) t -> ('tag_e, 'tag_p2) t =
   fun qt ->
    { qt with eqconss = List.map ~f:(eqcons_fmap_pattern ~f) qt.eqconss }

  let to_plain_t (qt : ('tag_e, 'tag_p) t) : plain_t =
    qt |> fmap_expr ~f:(fun _ -> ()) |> fmap_pattern ~f:(fun _ -> ())

  let to_source_code ?(use_newlines : bool option) (qt : ('tag_e, 'tag_p) t) :
      string =
    let open SourceCodeBuilder in
    let converter (qt : ('tag_e, 'tag_p) t) : state -> state =
      write (sprintf "qtype %s = %s" qt.name qt.base_type_name)
      |.> block
            (let converted_eqconss : (state -> state) list =
               List.map
                 ~f:
                   (eqcons_to_source_code ?use_newlines
                   |.> (fun (s : string) -> "|/ " ^ s)
                   |.> write)
                 qt.eqconss
             in
             let blocked_converted_eqconss : (state -> state) list =
               List.map ~f:block converted_eqconss
             in
             let eqconss_converter : state -> state =
               List.fold ~init:Fn.id ~f:( |.> ) blocked_converted_eqconss
             in
             eqconss_converter)
    in
    from_converter ~converter
      ~use_newlines:(Option.value ~default:true use_newlines)
      qt
end

module StdQuotientType :
  S with module Pattern = Pattern.StdPattern and module Expr = Expr.StdExpr =
  Make (Expr.StdExpr)
