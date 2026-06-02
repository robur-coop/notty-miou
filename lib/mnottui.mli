type dim = int * int

val run :
     ?stop:Mnotty.Stop.t
  -> ?cursor:dim Lwd.var
  -> dim * Mnotty.Signal.t
  -> Nottui.Ui.t Lwd.t
  -> (unit -> string option)
  -> (string -> unit)
  -> unit
