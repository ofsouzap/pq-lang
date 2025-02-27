module type S = sig
  module Program : Program.S

  type output = { ml_source : string; mli_source : string }

  (* TODO - have an opaque type created by quotient type checker
  so that only quotient type-checked programs can be converted to OCaml *)

  (** Convert a PQ program to OCaml source code *)
  val program_to_ocaml : ('tag_e, 'tag_p) Program.t -> output
end

module StdM : S with module Program = Program.StdProgram
