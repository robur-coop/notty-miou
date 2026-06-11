type dim = int * int

val run :
     ?stop:Mnotty.Stop.t
  -> ?cursor:dim Lwd.var
  -> dim * Mnotty.Signal.t
  -> Nottui.Ui.t Lwd.t
  -> (unit -> string option)
  -> (string -> unit)
  -> unit
(** [run ?stop ?cursor (dim, sig) ui oc ic] creates a new task which repeatedly
    run steps to render something into [oc] according the given [ui] and events
    from [ic]. It requires the initial dimensions of the virtual terminal and a
    signal (which should represents [SIGWINCH]). *)
