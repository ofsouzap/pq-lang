open Core
open Utils
open Varname
open Vtype
open Pattern
open Ast

type quotient_type_eqcons = {
  bindings : (varname * vtype) list;
  body : plain_pattern * plain_expr;
}
[@@deriving sexp, equal]

let eqcons_existing_names (eqcons : quotient_type_eqcons) : StringSet.t =
  let bindings_names =
    eqcons.bindings |> List.map ~f:(fun (v, _) -> v) |> StringSet.of_list
  in
  let p, e = eqcons.body in
  Set.union bindings_names
    (Set.union (Pattern.existing_names p) (Ast.existing_names e))

let quotient_type_eqcons_to_source_code ?(use_newlines : bool option)
    (eqcons : quotient_type_eqcons) : string =
  let bindings_str =
    eqcons.bindings
    |> List.map ~f:(fun (v, vt) ->
           sprintf "(%s : %s)" v (vtype_to_source_code vt))
    |> String.concat ~sep:" -> " |> String.append " -> "
  in
  let p, e = eqcons.body in
  sprintf "%s(%s) == (%s)" bindings_str (pattern_to_source_code p)
    (ast_to_source_code ?use_newlines e)

type quotient_type = {
  name : string;
  base_type_name : string;
  eqconss : quotient_type_eqcons list;
}
[@@deriving sexp, equal]

let existing_names (qt : quotient_type) : StringSet.t =
  let eqcons_names =
    List.fold ~init:StringSet.empty
      ~f:(fun acc eqcons -> Set.union acc (eqcons_existing_names eqcons))
      qt.eqconss
  in
  Set.union
    (StringSet.singleton qt.name)
    (Set.union (StringSet.singleton qt.base_type_name) eqcons_names)

let quotient_type_to_source_code ?(use_newlines : bool option)
    (qt : quotient_type) : string =
  let open SourceCodeBuilder in
  let converter (qt : quotient_type) : state -> state =
    write (sprintf "qtype %s = %s" qt.name qt.base_type_name)
    |.> block
          (let blocked_converted_eqconss : (state -> state) list =
             List.map
               ~f:
                 (quotient_type_eqcons_to_source_code ?use_newlines
                 |.> (fun (s : string) -> "|/ " ^ s)
                 |.> write)
               qt.eqconss
           in
           let eqconss_converter : state -> state =
             List.fold ~init:Fn.id ~f:( |.> ) blocked_converted_eqconss
           in
           eqconss_converter)
  in
  from_converter ~converter
    ~use_newlines:(Option.value ~default:true use_newlines)
    qt
