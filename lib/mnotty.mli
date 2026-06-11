open Notty

(** {1 Miou-compatible switches.} *)
module Stop : sig
  type t
  (** Type of switches. *)

  val create : unit -> t
  (** [create ()] creates a new switch. *)

  val switch : t -> unit
  (** [switch stop] signals an associated task to terminate properly. *)

  val wait : t -> unit
  (** [wait stop] waits a {i switch-off} signal (via {!val:switch}) from a
      possibly concurrent task. *)
end

(** {1 OCaml signals.} *)
module Signal : sig
  type t
  type dim = int * int

  val create : unit -> t
  val wait : t -> dim
  val signal : t -> dim -> unit
end

(** {1 Terminal IO with concurrency.} *)
module Term : sig
  type t
  type dim = int * int

  (** {2 Commands.} *)

  val refresh : t -> unit
  val image : t -> Notty.image -> unit
  val cursor : t -> dim option -> unit
  val size : t -> dim
  val release : t -> unit

  (** {2 Construction.} *)

  val create :
       ?bpaste:bool
    -> ?mouse:bool
    -> ?stop:Stop.t
    -> dim * Signal.t
    -> (unit -> string option)
    -> (string -> unit)
    -> t
  (** [create (dim, sig) oc ic] creates a virtual terminal from an input stream
      of bytes [ic] and fill the given output stream of bytes [oc]. The user
      must give the initial dimensions of the virtual terminal
      [(cols, rows) as dim] and a signal that is triggered as soon as the size
      of the virtual terminal changes. Then, few optional arguments can be set
      by the user:
      - [mouse] activates mouse reporting (defaults to [true])
      - [bpaste] activates bracketed paste reporting (default to [true])
      - [stop] is a value to stop the virtual terminal (and underlying tasks) *)

  (** {2 Events.} *)

  val events : t -> [ Unescape.event | `Resize of dim ] option
  (** [resize term] is the stream of incoming events. Events are:
      - [#Unescape.event], an {{!Notty.Unescape.event} event} from the input
        stream or
      - [`Resize (cols, rows)] whenever the terminal size changes.

      {b Note}: This stream is unique; for the same [term], [events term] always
      returns the same stream. *)

  val source : t -> [ Unescape.event | `Resize of dim ] Flux.source
  (** [source term] is a helper which transforms the {!val:events} as a
      {!type:Flux.source}. *)
end
