open Notty

module Stream : sig
  type 'a t

  val get : 'a t -> 'a
end

(** {1:fullscreen Fullscreen input and output}. *)

(** Terminal IO with concurrency.

    For more info, see {!Notty_unix.Term}. *)
module Term : sig
  type t

  (** {1 Construction and destruction} *)

  val create :
    ?nosig:bool ->
    ?mouse:bool ->
    ?bpaste:bool ->
    ?input:Miou_unix.file_descr ->
    ?output:Miou_unix.file_descr ->
    unit ->
    t
  (** [create ~nosig ~mouse ~input ~output ()] creates a new {{!t} terminal}.

      See {!Notty_unix.Term.create}. *)

  val release : t -> unit

  (** {1 Commands} *)

  val image : t -> image -> unit
  val refresh : t -> unit
  val cursor : t -> (int * int) option -> unit

  (** {1 Events} *)

  val events : t -> [ Unescape.event | `Resize of int * int ] Stream.t
  (** [events t] is the stream of incoming events.

      Invoking {{!release} release} will terminate this stream.

      Events are:
      - [#Unescape.event], an {{!Notty.Unescape.event} event} from the input fd;
        or
      - [`Resize (cols, rows)] whenever the terminal size changes.

      {b Note} This stream is unique; for the same [t], [events t] always
      returns the same stream. *)

  (** {1 Properties} *)

  val size : t -> int * int
  val fds : t -> Miou_unix.file_descr * Miou_unix.file_descr

  (** {1 Window size change notifications}

      {{!create} Creating} a terminal will install a [SIGWINCH] handler. The
      handler should not be replaced directly. This API allows the user to
      monitor deliveries of the signal.

      See {!Notty_unix.Term.Winch}. *)

  val winch : unit -> unit
  (** [winch ()] is a thread completing after the next [SIGWINCH]. A single
      signal delivery will cause the completion of all waiting [winch] threads. *)
end

(** {1:inline Inline output} *)

val winsize : Miou_unix.file_descr -> (int * int) option
val eol : image -> image
val output_image : ?cap:Cap.t -> ?fd:Miou_unix.file_descr -> image -> unit

val output_image_size :
  ?cap:Cap.t -> ?fd:Miou_unix.file_descr -> (int * int -> image) -> unit

val show_cursor : ?cap:Cap.t -> ?fd:Miou_unix.file_descr -> bool -> unit

val move_cursor :
  ?cap:Cap.t ->
  ?fd:Miou_unix.file_descr ->
  [ `Home | `By of int * int | `To of int * int ] ->
  unit
