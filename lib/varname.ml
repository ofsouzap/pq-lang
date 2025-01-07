open Core
open Utils

type varname = string [@@deriving sexp, equal]

module Varname = String
module VarnameMap = Map.Make_using_comparator (Varname)

module QCheck_testing :
  QCheck_testing_sig
    with type t = varname
     and type gen_options = unit
     and type print_options = unit
     and type shrink_options = unit
     and type arb_options = unit = struct
  type t = varname
  type gen_options = unit
  type print_options = unit
  type shrink_options = unit
  type arb_options = unit

  let gen () : t QCheck.Gen.t =
    let open QCheck.Gen in
    fix
      (fun self _ ->
        int_range 1 5 >>= fun n ->
        list_repeat n (char_range 'a' 'z') >|= String.of_char_list >>= fun s ->
        if List.mem Utils.lexer_keywords s ~equal:String.equal then self ()
        else return s)
      ()

  let print () = Fn.id
  let shrink () = QCheck.Shrink.nil
  let arbitrary () = QCheck.make ~print:(print ()) ~shrink:(shrink ()) (gen ())
end
