open Notty
open Nottui
open Notty_miou

let render ~size ~stop ~events v =
  let renderer = Renderer.make () in
  let image = Stream.create () and refresh = Stream.create () in
  let size = ref size in
  let root =
    Lwd.observe
      ~on_invalidate:(fun _ ->
        if Stop.stopped stop = false then Stream.put refresh ())
      v
  in
  let rec do_refresh () =
    Stream.get refresh;
    let ui = Lwd.quick_sample root in
    Renderer.update renderer !size ui;
    Stream.put image (Renderer.image renderer);
    do_refresh ()
  in
  let rec do_event () =
    let () =
      match
        (Stream.get events
          : [ `Resize of _ | Unescape.event ]
          :> [ `Resize of _ | Ui.event ])
      with
      | `Key (`Escape, []) -> Stop.stop stop
      | #Ui.event as event -> ignore (Renderer.dispatch_event renderer event)
      | `Resize size' ->
          size := size';
          let ui = Lwd.quick_sample root in
          Renderer.update renderer !size ui;
          Stream.put image (Renderer.image renderer)
    in
    do_event ()
  in
  let prm0 = Miou.async do_refresh in
  let prm1 = Miou.async do_event in
  Stream.put refresh ();
  (image, prm0, prm1)

let or_raise = function Ok v -> v | Error exn -> raise exn
let ( $ ) f x = f x

let run v =
  let term = Term.create () in
  let stop = Term.stop term in
  let stream, prm0, prm1 =
    render ~size:(Term.size term) ~stop ~events:(Term.events term) v
  in
  let rec do_print () =
    let image = Stream.get stream in
    Term.image term image;
    do_print ()
  in
  let prm2 = Miou.async do_print in
  let prm3 = Miou.async @@ fun () -> Stop.wait stop in
  or_raise $ Miou.await_first [ prm0; prm1; prm2; prm3 ];
  Term.release term
