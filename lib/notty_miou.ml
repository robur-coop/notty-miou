open Notty

external winsize : Unix.file_descr -> int = "caml_motty_winsize" [@@noalloc]
external winch_number : unit -> int = "caml_motty_winch_number" [@@noalloc]
external reraise : exn -> unit = "%reraise"

let winsize fd =
  match winsize fd with
  | 0 -> None
  | wh -> Some (wh lsr 16, (wh lsr 1) land 0x7fff)

module Private = struct
  let once fn =
    let v = Miou.Lazy.from_fun fn in
    fun () -> Miou.Lazy.force v

  let cap_for_fd =
    let open Cap in
    match Sys.getenv "TERM" with
    | exception Not_found -> fun _ -> dumb
    | "" | "dumb" -> fun _ -> dumb
    | _ -> fun fd -> if Unix.isatty fd then ansi else dumb

  let setup_tcattr ~nosig fd =
    let open Unix in
    try
      let tc = Unix.tcgetattr fd in
      let tc1 = { tc with Unix.c_icanon = false; c_echo = false } in
      let tc2 =
        if nosig then { tc1 with Unix.c_isig = false; c_ixon = false } else tc1
      in
      Unix.tcsetattr fd Unix.TCSANOW tc2;
      `Revert (once @@ fun _ -> Unix.tcsetattr fd Unix.TCSANOW tc)
    with Unix_error (ENOTTY, _, _) -> `Revert ignore

  let winch_signal = winch_number ()

  let set_winch_handler fn =
    let new_behavior = Sys.Signal_handle (fun _ -> fn ()) in
    let old_behavior = Miou.sys_signal winch_signal new_behavior in
    `Revert
      (once @@ fun () -> ignore (Miou.sys_signal winch_signal old_behavior))

  module Gen_output (O : sig
    type fd
    type k

    val def : fd
    val to_fd : fd -> Unix.file_descr
    val write : fd -> Buffer.t -> k
  end) =
  struct
    let scratch = Buffer.create 4096

    let output ?cap ?(fd = O.def) fn =
      let cap = Option.value ~default:(cap_for_fd (O.to_fd fd)) cap in
      let buf = scratch in
      Buffer.reset buf;
      fn buf cap fd;
      O.write fd buf

    let output_image_size ?cap ?fd f =
      output ?cap ?fd @@ fun buf cap fd ->
      let size = winsize (O.to_fd fd) in
      let i = f (Option.value ~default:(80, 24) size) in
      let dim =
        match size with
        | Some (w, _) -> I.(w, height i)
        | None -> I.(width i, height i)
      in
      Render.to_buffer buf cap (0, 0) dim i

    let show_cursor ?cap ?fd x =
      output ?cap ?fd @@ fun buf cap _ -> Direct.show_cursor buf cap x

    let move_cursor ?cap ?fd x =
      output ?cap ?fd @@ fun buf cap _ -> Direct.move_cursor buf cap x

    let output_image ?cap ?fd i = output_image_size ?cap ?fd (fun _ -> i)
    let eol i = I.(i <-> void 0 1)
  end
end

module Stream = struct
  type 'a t = {
    mutex : Miou.Mutex.t;
    condition : Miou.Condition.t;
    queue : 'a Queue.t;
  }

  let create () =
    let mutex = Miou.Mutex.create () in
    let condition = Miou.Condition.create () in
    let queue = Queue.create () in
    { mutex; condition; queue }

  let put t v =
    Miou.Mutex.protect t.mutex @@ fun () ->
    Queue.push v t.queue;
    Miou.Condition.signal t.condition

  let get t =
    Miou.Mutex.protect t.mutex @@ fun () ->
    while Queue.is_empty t.queue do
      Miou.Condition.wait t.condition t.mutex
    done;
    Queue.pop t.queue
end

module Stop = struct
  type t = {
    mutex : Miou.Mutex.t;
    condition : Miou.Condition.t;
    mutable stop : bool;
  }

  let create () =
    let mutex = Miou.Mutex.create () and condition = Miou.Condition.create () in
    { mutex; condition; stop = false }

  let stop t =
    Miou.Mutex.protect t.mutex @@ fun () ->
    t.stop <- true;
    Miou.Condition.broadcast t.condition

  let stopped t = Miou.Mutex.protect t.mutex @@ fun () -> t.stop

  let wait t =
    Miou.Mutex.protect t.mutex @@ fun () ->
    while t.stop = false do
      Miou.Condition.wait t.condition t.mutex
    done
end

module Term = struct
  let winches =
    let condition = Miou.Condition.create () and mutex = Miou.Mutex.create () in
    let (`Revert _) =
      Private.set_winch_handler @@ fun () ->
      Miou.Mutex.protect mutex @@ fun () -> Miou.Condition.broadcast condition
    in
    (mutex, condition)

  let winch () =
    let mutex, condition = winches in
    Miou.Mutex.protect mutex @@ fun () -> Miou.Condition.wait condition mutex

  let fill_from_input ~orphans ~nosig stop input stream =
    let (`Revert fn) =
      Private.setup_tcattr ~nosig (Miou_unix.to_file_descr input)
    in
    let flt = Unescape.create () and buf = Bytes.create 1024 in
    let rec fill () =
      match Unescape.next flt with
      | #Unescape.event as r ->
          Stream.put stream r;
          fill ()
      | `End -> fn ()
      | `Await -> (
          let read =
            Miou.async @@ fun () ->
            `Read (Miou_unix.read input buf)
          and interrupt =
            Miou.async @@ fun () ->
            Stop.wait stop;
            `Stop
          in
          match Miou.await_first [ read; interrupt ] with
          | Ok (`Read n) ->
              Unescape.input flt buf 0 n;
              fill ()
          | Ok `Stop -> fn ()
          | Error exn ->
              fn ();
              Stop.stop stop;
              reraise exn)
    in
    ignore (Miou.async ~orphans fill)

  type t = {
    oc : Miou_unix.file_descr;
    trm : Tmachine.t;
    buf : Buffer.t;
    fds : Miou_unix.file_descr * Miou_unix.file_descr;
    events : [ Unescape.event | `Resize of int * int ] Stream.t;
    orphans : unit Miou.orphans;
    stop : Stop.t;
  }

  let write t =
    Tmachine.output t.trm t.buf;
    let str = Buffer.contents t.buf in
    Buffer.clear t.buf;
    Miou_unix.write t.oc str

  let refresh t =
    Tmachine.refresh t.trm;
    write t

  let image t image =
    Tmachine.image t.trm image;
    write t

  let cursor t v =
    Tmachine.cursor t.trm v;
    write t

  let set_size t dim = Tmachine.set_size t.trm dim
  let size t = Tmachine.size t.trm

  let rec terminate orphans =
    match Miou.care orphans with
    | Some None ->
        Miou.yield ();
        terminate orphans
    | None -> ()
    | Some (Some prm) -> (
        match Miou.await prm with
        | Ok () -> terminate orphans
        | Error exn ->
            Format.eprintf ">>> %s\n%!" (Printexc.to_string exn);
            terminate orphans)

  let release t =
    if Tmachine.release t.trm then (
      Stop.stop t.stop;
      write t);
    terminate t.orphans

  let fill_from_output ~orphans ~on_resize:fn output stop stream =
    let rec fill () =
      let wait =
        Miou.async @@ fun () ->
        Miou_unix.sleep 0.1;
        `Continue
      and interrupt =
        Miou.async @@ fun () ->
        Stop.wait stop;
        `Stop
      and resize =
        Miou.async @@ fun () ->
        winch ();
        `Resize (winsize output)
      in
      match Miou.await_first [ wait; interrupt; resize ] with
      | Ok (`Resize (Some dim)) ->
          fn dim;
          Stream.put stream (`Resize dim);
          fill ()
      | Ok (`Resize None) | Ok `Continue -> fill ()
      | Ok `Stop -> ()
      | Error exn ->
          Stop.stop stop;
          reraise exn
    in
    ignore (Miou.async ~orphans fill)

  let create ?(nosig = true) ?(mouse = true) ?(bpaste = true)
      ?(input = Miou_unix.of_file_descr Unix.stdin)
      ?(output = Miou_unix.of_file_descr Unix.stdout) () =
    let stop = Stop.create () in
    let fd = Miou_unix.to_file_descr output in
    let trm = Tmachine.create ~mouse ~bpaste (Private.cap_for_fd fd) in
    let buf = Buffer.create 4096 in
    let on_resize dim =
      Buffer.reset buf;
      Tmachine.set_size trm dim
    in
    let orphans = Miou.orphans () in
    let stream = Stream.create () in
    fill_from_input ~orphans ~nosig stop input stream;
    fill_from_output ~orphans ~on_resize fd stop stream;
    let t =
      {
        trm;
        oc = output;
        buf = Buffer.create 4096;
        fds = (input, output);
        stop;
        events = stream;
        orphans;
      }
    in
    Option.iter (set_size t) (winsize fd);
    write t;
    t

  let events t = t.events
  let fds t = t.fds
  let stop t = t.stop
end

let winsize fd = winsize (Miou_unix.to_file_descr fd)

include Private.Gen_output (struct
  type fd = Miou_unix.file_descr
  and k = unit

  let def, to_fd = Miou_unix.(of_file_descr Unix.stdout, to_file_descr)

  let write fd buf =
    let str = Buffer.contents buf in
    Miou_unix.write fd str
end)
