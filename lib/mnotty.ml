module Stop = struct
  type t = {
      mutex: Miou.Mutex.t
    ; condition: Miou.Condition.t
    ; mutable value: bool
  }

  let create () =
    let mutex = Miou.Mutex.create () and condition = Miou.Condition.create () in
    { mutex; condition; value= false }

  let switch t =
    Miou.Mutex.protect t.mutex @@ fun () ->
    t.value <- true;
    Miou.Condition.broadcast t.condition

  let wait t =
    Miou.Mutex.protect t.mutex @@ fun () ->
    while not t.value do
      Miou.Condition.wait t.condition t.mutex
    done
end

module Signal = struct
  type dim = int * int
  type t = (dim, dim) Flux.Bqueue.t

  let create () = Flux.Bqueue.(create infinite 0x7ff)
  let wait t = Flux.Bqueue.get t
  let signal t dim = Flux.Bqueue.put t dim

  (* NOTE(dinosaure): [Signal] is really poor in the sense that if several
     processes are waiting for the signal and it appears only once, only **one**
     of the processes will be released (this is not a [Condition.broadcast]!).
     In this case, there is only one process attempting to "consume"/wait (see
     [resize]) for the signal, so it doesn't matter, but it's important to bear
     this in mind. *)
end

open Notty

module Term = struct
  type t = {
      oc: string -> unit
    ; trm: Notty.Tmachine.t
    ; buf: Buffer.t
    ; mutable src: [ Unescape.event | `Resize of dim ] Flux.source
    ; stop: Stop.t
  }

  and dim = int * int

  exception Stop
  exception Timeout

  let consume_and_put ic stop q =
    let flt = Unescape.create () in
    let buf = Bytes.create 0x7ff in
    let rec next () =
      match Unescape.next flt with
      | #Unescape.event as ev -> Flux.Bqueue.put q ev; next ()
      | `End -> Flux.Bqueue.close q
      | `Await -> begin
          let prm0 = Miou.async ic in
          let prm1 = Miou.async @@ fun () -> Stop.wait stop; raise Stop in
          match Miou.await_first [ prm0; prm1 ] with
          | Error Stop -> Flux.Bqueue.close q
          | Error _exn -> Flux.Bqueue.close q
          | Ok None -> Unescape.input flt buf 0 0; next ()
          | Ok (Some str) ->
              let rec go (str, str_off, str_len) =
                if str_len > 0 then begin
                  let len = Int.min (Bytes.length buf) str_len in
                  Bytes.blit_string str str_off buf 0 len;
                  Unescape.input flt buf 0 len;
                  go (str, str_off + len, str_len - len)
                end
              in
              go (str, 0, String.length str);
              next ()
        end
    in
    next

  let write t =
    Tmachine.output t.trm t.buf;
    let out = Buffer.contents t.buf in
    Buffer.clear t.buf; t.oc out

  let refresh t = Tmachine.refresh t.trm; write t
  let image t image = Tmachine.image t.trm image; write t
  let cursor t curs = Tmachine.cursor t.trm curs; write t
  let set_size t dim = Tmachine.set_size t.trm dim
  let size t = Tmachine.size t.trm

  let release t =
    if Tmachine.release t.trm then begin
      Stop.switch t.stop; write t
    end

  let resize sigwinch stop on_resize =
    (* NOTE(dinosaure): The aim here is to "debounce" the SIGWINCH signal,
       which can occur several times in succession. When a user manually
       resizes a window, a multitude of SIGWINCH events are triggered (100x80,
       100x79, 100x78, 100x77, ... 100x50).

       Instead of resizing each time, we wait 1000 ns; if no further SIGWINCH
       signals have occurred after these 1000 ns, it means the user has
       finished resizing and is therefore reporting the new window size. *)
    let rec delay dim =
      let prm0 = Miou.async @@ fun () -> Signal.wait sigwinch in
      let prm1 = Miou.async @@ fun () -> Mkernel.sleep 1000; raise Timeout in
      let prm2 = Miou.async @@ fun () -> Stop.wait stop; raise Stop in
      match Miou.await_first [ prm0; prm1; prm2 ] with
      | Ok dim -> delay dim
      | Error Timeout -> on_resize dim; start ()
      | Error Stop -> ()
      | Error _exn -> ()
    and start () =
      let prm0 = Miou.async @@ fun () -> Signal.wait sigwinch in
      let prm1 = Miou.async @@ fun () -> Stop.wait stop; raise Stop in
      match Miou.await_first [ prm0; prm1 ] with
      | Ok dim -> delay dim
      | Error Stop -> ()
      | Error _exn -> ()
    in
    Miou.async start

  let create ?(bpaste = true) ?(mouse = true) ?stop (size, sigwinch) ic oc =
    let stop = match stop with Some stop -> stop | None -> Stop.create () in
    let rec t =
      lazy
        {
          trm= Tmachine.create ~mouse ~bpaste Cap.ansi
        ; oc
        ; buf= Buffer.create 0x7ff
        ; stop
        ; src= Flux.Source.with_task ~size:0x7ff task
        }
    and task q =
      let on_resize dim =
        let t = Lazy.force t in
        Buffer.reset t.buf;
        set_size t dim;
        Flux.Bqueue.put q (`Resize dim)
      in
      let prm = resize sigwinch stop on_resize in
      consume_and_put ic stop q ();
      Miou.cancel prm
    in
    let t = Lazy.force t in
    set_size t size; write t; t

  let events t =
    match Flux.Source.next t.src with
    | Some (ev, src) ->
        t.src <- src;
        Some ev
    | None -> None

  let source { src; _ } = src
end
