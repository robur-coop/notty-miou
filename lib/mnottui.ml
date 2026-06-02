(* Render-loop state. Rendering is synchronous, driven by Lwd's [on_invalidate]:
   any change to the observed document triggers a render. But a sensor
   ([size_sensor], [transient_sensor]) fires *during* [Renderer.update] and may
   re-invalidate the document. In that case, we re-enter into [refresh]: where
   rendering would use a stale, in-progress layout. So a re-entrant call doesn't
   render; it requests one more pass, and [refresh] loops until it reaches a
   fixed point. *)
type state =
  | Idle (* not rendering *)
  | Rendering (* rendering, no extra pass requested *)
  | Rendering_pending (* rendering, one more pass requested *)

let render ~dim ~cursor ~root term =
  let renderer = Nottui.Renderer.make () in
  let dim = ref dim in
  let refresh_cursor () =
    let cursor = Lwd.quick_sample cursor in
    Mnotty.Term.cursor term (Some cursor)
  in
  let render () =
    let ui = Lwd.quick_sample root in
    Nottui.Renderer.update renderer !dim ui;
    let img = Nottui.Renderer.image renderer in
    Mnotty.Term.image term img; refresh_cursor ()
  in
  (* Render to a fixed point, coalescing re-entrant calls (see [state]). *)
  let state = ref Idle in
  let refresh () =
    match !state with
    | Rendering | Rendering_pending -> state := Rendering_pending
    | Idle ->
        state := Rendering;
        let rec go () =
          render ();
          match !state with
          | Rendering_pending ->
              state := Rendering;
              go ()
          | Idle | Rendering -> state := Idle
        in
        go ()
  in
  Lwd.set_on_invalidate root (fun _ -> refresh ());
  let init () = ()
  and push () = function
    | #Notty.Unescape.event as event ->
        let event = (event :> Nottui.Ui.event) in
        ignore (Nottui.Renderer.dispatch_event renderer event)
    | `Resize dim' ->
        dim := dim';
        refresh ()
  and full = Fun.const false
  and stop = Fun.const () in
  refresh ();
  let from = Mnotty.Term.source term in
  let via = Flux.Flow.identity in
  let into = Flux.Sink { init; push; full; stop } in
  let (), src = Flux.Stream.run ~from ~via ~into in
  Option.iter Flux.Source.dispose src

type dim = int * int

let run ?stop ?cursor (dim, sigwinch) document ic oc =
  let cursor = Option.value ~default:(Lwd.var (0, 0)) cursor in
  let term = Mnotty.Term.create ?stop (dim, sigwinch) ic oc in
  let root = Lwd.observe document in
  let cursor = Lwd.observe (Lwd.get cursor) in
  let finally () =
    Mnotty.Term.release term; Lwd.quick_release root; Lwd.quick_release cursor
  in
  let res = Miou.Ownership.create ~finally () in
  Miou.Ownership.own res;
  render ~dim ~cursor ~root term;
  Miou.Ownership.release res
