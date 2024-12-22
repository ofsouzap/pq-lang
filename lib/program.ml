open Core
open Custom_types
open Ast

type 'a program = { custom_types : custom_type list; e : 'a expr }
[@@deriving sexp, equal]

type plain_program = unit program [@@deriving sexp, equal]

let program_to_source_code (prog : 'a program) : string =
  let custom_types_strs : string list =
    List.map ~f:custom_type_to_source_code prog.custom_types
  in
  let e_str : string = ast_to_source_code prog.e in
  let str_parts : string list = custom_types_strs @ [ e_str ] in
  String.concat ~sep:"\n" str_parts
