open Core

module Logger : sig
  val configure : ?out_channel:Out_channel.t -> ?flush:bool -> unit -> unit
  val debug : string -> unit
  val debug_sexp : Sexp.t -> unit
end

val pause : unit -> unit
