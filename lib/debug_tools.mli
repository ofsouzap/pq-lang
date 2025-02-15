open Core

module Logger : sig
  val configure : ?out_channel:Out_channel.t -> ?flush:bool -> unit -> unit
  val debug : string -> unit
  val debugf : ('a, unit, string, unit) format4 -> 'a
  val debug_sexp : Sexp.t -> unit
end

val pause : unit -> unit
