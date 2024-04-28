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
      Unix.tcsetattr fd Unix.TCSANOW tc2 ;
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
      Buffer.reset buf ;
      fn buf cap fd ;
      O.write fd buf

    let output_image_size ?cap ?fd f =
      output ?cap ?fd @@ fun buf cap fd ->
      let size = winsize (O.to_fd fd) in
      let i = f (Option.value ~default:(80, 24) size) in
      let dim =
        match size with
        | Some (w, _) -> I.(w, height i)
        | None -> I.(width i, height i) in
      Render.to_buffer buf cap (0, 0) dim i

    let show_cursor ?cap ?fd x =
      output ?cap ?fd @@ fun buf cap _ -> Direct.show_cursor buf cap x

    let move_cursor ?cap ?fd x =
      output ?cap ?fd @@ fun buf cap _ -> Direct.move_cursor buf cap x

    let output_image ?cap ?fd i = output_image_size ?cap ?fd (fun _ -> i)
    let eol i = I.(i <-> void 0 1)
  end
end

(* NOTE(dinosaure): this stream can only work concurrently! *)
module Local_stream = struct
  [@@@warning "-32"]

  type 'a t = {
    next : unit -> 'a option;
    mutable closed : bool;
    runner : Stdlib.Domain.id;
  }

  let from next = { next; closed = false; runner = Stdlib.Domain.self () }

  let closed t =
    if Stdlib.Domain.self () <> t.runner
    then invalid_arg "A stream can only be used locally to a domain" ;
    t.closed

  let get t =
    if Stdlib.Domain.self () <> t.runner
    then invalid_arg "A stream can only be used locally to a domain" ;
    if t.closed
    then None
    else
      match t.next () with
      | Some value -> Some value
      | None ->
          t.closed <- true ;
          None

  let a v = `A v
  let b v = `B v
  let destr = function `A v | `B v -> v
  let if_none v fn = match v with Some v -> v | None -> fn ()
  let ( % ) f g x = f (g x)

  let compose (type v) (x : v t) (y : v t) : v t =
    let open Effect.Deep in
    let retc = Fun.id in
    let exnc = raise in
    let module M = struct
      type _ Effect.t +=
        | Spawn : (unit -> [ `A of v option | `B of v option ]) -> unit Effect.t
    end in
    let orphans = Miou.orphans () in
    let effc :
        type c. c Effect.t -> ((c, v option) continuation -> v option) option =
      function
      | M.Spawn fn ->
          let _ = Miou.call_cc ~orphans fn in
          Some (fun k -> continue k ())
      | _ -> None in
    let rec next orphans =
      match Miou.care orphans with
      | None -> None
      | Some None ->
          Miou.yield () ;
          next orphans
      | Some (Some prm) ->
      match Miou.await prm with
      | Ok (`A (Some v)) ->
          Effect.perform (M.Spawn (a % x.next)) ;
          Some v
      | Ok (`B (Some v)) ->
          Effect.perform (M.Spawn (b % y.next)) ;
          Some v
      | Ok (`A None | `B None) | Error _ -> next orphans in
    let orphans = Miou.orphans () in
    let _ = Miou.call_cc ~orphans (a % x.next) in
    let _ = Miou.call_cc ~orphans (b % y.next) in
    let next () = match_with next orphans { retc; exnc; effc } in
    from next

  (*
  let compose x y =
    let old_a = ref None in
    let old_b = ref None in
    let finish_with v = match !old_a, !old_b, v with
      | Some prm, b, `A -> Option.iter Miou.cancel b; (destr % Miou.await_exn) prm
      | a, Some prm, `B -> Option.iter Miou.cancel a; (destr % Miou.await_exn) prm
      | None, b, `A -> Option.iter Miou.cancel b; x.next ()
      | a, None, `B -> Option.iter Miou.cancel a; y.next () in
    let next () =
      Format.eprintf ">>> compose (a:%b, b:%b)\n%!" (closed x) (closed y);
      if closed x && closed y
      then None
      else if closed x
      then finish_with `B
      else if closed y
      then finish_with `A
      else
        let prm0 = if_none !old_a @@ fun () -> Miou.call_cc (a % x.next) in
        let prm1 = if_none !old_b @@ fun () -> Miou.call_cc (b % y.next) in
        Format.eprintf ">>> compose: wait %a and %a\n%!"
          Miou.Promise.Uid.pp (Miou.Promise.uid prm0)
          Miou.Promise.Uid.pp (Miou.Promise.uid prm1);
        match Miou.await_one [ prm0; prm1 ] with
        | Ok (`A None) ->
            Format.eprintf ">>> compose A closed\n%!";
            old_a := None ;
            old_b := None ;
            (destr % Miou.await_exn) prm1
        | Ok (`B None) ->
            Format.eprintf ">>> compose B closed\n%!";
            old_a := None ;
            old_b := None ;
            (destr % Miou.await_exn) prm0
        | Ok (`A (Some v)) ->
            Format.eprintf ">>> compose A emitted\n%!";
            old_a := None ;
            old_b := Some prm1 ;
            Some v
        | Ok (`B (Some v)) ->
            Format.eprintf ">>> compose B emitted\n%!";
            old_a := Some prm0 ;
            old_b := None ;
            Some v
        | Error exn ->
          Format.eprintf ">>> unexpected error: %S" (Printexc.to_string exn);
          raise exn in
    from next
  *)
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
    Queue.push v t.queue ;
    Miou.Condition.signal t.condition

  let get t =
    Miou.Mutex.protect t.mutex @@ fun () ->
    while Queue.is_empty t.queue do
      Miou.Condition.wait t.condition t.mutex
    done ;
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
    t.stop <- true ;
    Miou.Condition.broadcast t.condition

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
      Private.setup_tcattr ~nosig (Miou_unix.to_file_descr input) in
    let flt = Unescape.create () and buf = Bytes.create 1024 in
    let rec fill () =
      match Unescape.next flt with
      | #Unescape.event as r ->
          Stream.put stream r ;
          fill ()
      | `End -> fn ()
      | `Await -> (
          let read =
            Miou.call_cc @@ fun () ->
            `Read (Miou_unix.read input buf 0 (Bytes.length buf))
          and interrupt =
            Miou.call_cc @@ fun () ->
            Stop.wait stop ;
            `Stop in
          match Miou.await_first [ read; interrupt ] with
          | Ok (`Read n) ->
              Unescape.input flt buf 0 n ;
              fill ()
          | Ok `Stop -> fn ()
          | Error exn ->
              fn () ;
              Stop.stop stop ;
              reraise exn) in
    ignore (Miou.call_cc ~orphans fill)

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
    Tmachine.output t.trm t.buf ;
    let str = Buffer.contents t.buf in
    Buffer.clear t.buf ;
    Miou_unix.write t.oc str 0 (String.length str)

  let refresh t =
    Tmachine.refresh t.trm ;
    write t

  let image t image =
    Tmachine.image t.trm image ;
    write t

  let cursor t v =
    Tmachine.cursor t.trm v ;
    write t

  let set_size t dim = Tmachine.set_size t.trm dim
  let size t = Tmachine.size t.trm

  let rec terminate orphans =
    match Miou.care orphans with
    | Some None ->
        Miou.yield () ;
        terminate orphans
    | None -> ()
    | Some (Some prm) ->
    match Miou.await prm with
    | Ok () -> terminate orphans
    | Error exn ->
        Format.eprintf ">>> %s\n%!" (Printexc.to_string exn) ;
        terminate orphans

  let release t =
    if Tmachine.release t.trm
    then (
      Stop.stop t.stop ;
      write t) ;
    terminate t.orphans

  let fill_from_output ~orphans ~on_resize:fn output stop stream =
    let rec fill () =
      let wait =
        Miou.call_cc @@ fun () ->
        Miou_unix.sleep 0.1 ;
        `Continue
      and interrupt =
        Miou.call_cc @@ fun () ->
        Stop.wait stop ;
        `Stop
      and resize =
        Miou.call_cc @@ fun () ->
        winch () ;
        `Resize (winsize output) in
      match Miou.await_first [ wait; interrupt; resize ] with
      | Ok (`Resize (Some dim)) ->
          fn dim ;
          Stream.put stream (`Resize dim) ;
          fill ()
      | Ok (`Resize None) | Ok `Continue -> fill ()
      | Ok `Stop -> ()
      | Error exn ->
          Stop.stop stop ;
          reraise exn in
    ignore (Miou.call_cc ~orphans fill)

  let create ?(nosig = true) ?(mouse = true) ?(bpaste = true)
      ?(input = Miou_unix.of_file_descr Unix.stdin)
      ?(output = Miou_unix.of_file_descr Unix.stdout) () =
    let stop = Stop.create () in
    let fd = Miou_unix.to_file_descr output in
    let trm = Tmachine.create ~mouse ~bpaste (Private.cap_for_fd fd) in
    let buf = Buffer.create 4096 in
    let on_resize dim =
      Buffer.reset buf ;
      Tmachine.set_size trm dim in
    let orphans = Miou.orphans () in
    let stream = Stream.create () in
    fill_from_input ~orphans ~nosig stop input stream ;
    fill_from_output ~orphans ~on_resize fd stop stream ;
    let t =
      {
        trm;
        oc = output;
        buf = Buffer.create 4096;
        fds = (input, output);
        stop;
        events = stream;
        orphans;
      } in
    Option.iter (set_size t) (winsize fd) ;
    write t ;
    t

  let events t = t.events
  let fds t = t.fds
end

let winsize fd = winsize (Miou_unix.to_file_descr fd)

include Private.Gen_output (struct
  type fd = Miou_unix.file_descr
  and k = unit

  let def, to_fd = Miou_unix.(of_file_descr Unix.stdout, to_file_descr)

  let write fd buf =
    let str = Buffer.contents buf in
    Miou_unix.write fd str 0 (String.length str)
end)
