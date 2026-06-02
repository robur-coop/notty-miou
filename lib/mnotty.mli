open Notty

module Stop : sig
  type t

  val create : unit -> t
  val switch : t -> unit
  val wait : t -> unit
end

module Signal : sig
  type t
  type dim = int * int

  val create : unit -> t
  val wait : t -> dim
  val signal : t -> dim -> unit
end

module Term : sig
  type t
  type dim = int * int

  val refresh : t -> unit
  val image : t -> Notty.image -> unit
  val cursor : t -> dim option -> unit
  val size : t -> dim
  val release : t -> unit

  val create :
       ?bpaste:bool
    -> ?mouse:bool
    -> ?stop:Stop.t
    -> dim * Signal.t
    -> (unit -> string option)
    -> (string -> unit)
    -> t

  val events : t -> [ Unescape.event | `Resize of dim ] option
  val source : t -> [ Unescape.event | `Resize of dim ] Flux.source
end
