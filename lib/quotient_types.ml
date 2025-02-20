open Core
open Utils
open Varname
open Vtype
open Pattern
open Expr

type ('tag_e, 'tag_p) quotient_type_eqcons = {
  bindings : (varname * vtype) list;
  body : 'tag_p pattern * ('tag_e, 'tag_p) expr;
}
[@@deriving sexp, equal]

type plain_quotient_type_eqcons = (unit, unit) quotient_type_eqcons
[@@deriving sexp, equal]

type ('tag_e, 'tag_p) typed_quotient_type_eqcons =
  (vtype * 'tag_e, vtype * 'tag_p) quotient_type_eqcons
[@@deriving sexp, equal]

let eqcons_existing_names (eqcons : ('tag_e, 'tag_p) quotient_type_eqcons) :
    StringSet.t =
  let bindings_names =
    eqcons.bindings |> List.map ~f:(fun (v, _) -> v) |> StringSet.of_list
  in
  let p, e = eqcons.body in
  Set.union bindings_names
    (Set.union (Pattern.existing_names p) (Expr.existing_names e))

let eqcons_fmap_expr ~(f : 'tag_e1 -> 'tag_e2)
    (eqcons : ('tag_e1, 'tag_p) quotient_type_eqcons) :
    ('tag_e2, 'tag_p) quotient_type_eqcons =
  { eqcons with body = (fst eqcons.body, snd eqcons.body |> Expr.fmap ~f) }

let eqcons_fmap_pattern ~(f : 'tag_p1 -> 'tag_p2)
    (eqcons : ('tag_e, 'tag_p1) quotient_type_eqcons) :
    ('tag_e, 'tag_p2) quotient_type_eqcons =
  {
    eqcons with
    body =
      ( fst eqcons.body |> Pattern.fmap ~f,
        snd eqcons.body |> Expr.fmap_pattern ~f );
  }

let eqcons_to_plain_eqcons (eqcons : ('tag_e, 'tag_p) quotient_type_eqcons) :
    plain_quotient_type_eqcons =
  eqcons
  |> eqcons_fmap_expr ~f:(fun _ -> ())
  |> eqcons_fmap_pattern ~f:(fun _ -> ())

let quotient_type_eqcons_to_source_code ?(use_newlines : bool option)
    (eqcons : ('tag_e, 'tag_p) quotient_type_eqcons) : string =
  let bindings_str : string =
    match eqcons.bindings with
    | [] -> "()"
    | h :: ts ->
        Nonempty_list.(
          map
            ~f:(fun (v, vt) -> sprintf "(%s : %s)" v (vtype_to_source_code vt))
            (make (h, ts)))
        |> Nonempty_list.to_list |> String.concat ~sep:" -> "
        |> fun str -> String.append str " => "
  in
  let p, e = eqcons.body in
  sprintf "%s(%s) == (%s)" bindings_str (pattern_to_source_code p)
    (Expr.to_source_code ?use_newlines e)

type ('tag_e, 'tag_p) quotient_type = {
  name : string;
  base_type_name : string;
  eqconss : ('tag_e, 'tag_p) quotient_type_eqcons list;
}
[@@deriving sexp, equal]

type plain_quotient_type = (unit, unit) quotient_type [@@deriving sexp, equal]

type ('tag_e, 'tag_p) typed_quotient_type =
  (vtype * 'tag_e, vtype * 'tag_p) quotient_type
[@@deriving sexp, equal]

let existing_names (qt : ('tag_e, 'tag_p) quotient_type) : StringSet.t =
  let eqcons_names =
    List.fold ~init:StringSet.empty
      ~f:(fun acc eqcons -> Set.union acc (eqcons_existing_names eqcons))
      qt.eqconss
  in
  Set.union
    (StringSet.singleton qt.name)
    (Set.union (StringSet.singleton qt.base_type_name) eqcons_names)

let fmap_expr ~(f : 'tag_e1 -> 'tag_e2) :
    ('tag_e1, 'tag_p) quotient_type -> ('tag_e2, 'tag_p) quotient_type =
 fun qt -> { qt with eqconss = List.map ~f:(eqcons_fmap_expr ~f) qt.eqconss }

let fmap_pattern ~(f : 'tag_p1 -> 'tag_p2) :
    ('tag_e, 'tag_p1) quotient_type -> ('tag_e, 'tag_p2) quotient_type =
 fun qt -> { qt with eqconss = List.map ~f:(eqcons_fmap_pattern ~f) qt.eqconss }

let to_plain_quotient_type (qt : ('tag_e, 'tag_p) quotient_type) :
    plain_quotient_type =
  qt |> fmap_expr ~f:(fun _ -> ()) |> fmap_pattern ~f:(fun _ -> ())

let quotient_type_to_source_code ?(use_newlines : bool option)
    (qt : ('tag_e, 'tag_p) quotient_type) : string =
  let open SourceCodeBuilder in
  let converter (qt : ('tag_e, 'tag_p) quotient_type) : state -> state =
    write (sprintf "qtype %s = %s" qt.name qt.base_type_name)
    |.> block
          (let converted_eqconss : (state -> state) list =
             List.map
               ~f:
                 (quotient_type_eqcons_to_source_code ?use_newlines
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
