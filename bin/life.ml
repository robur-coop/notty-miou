(* Copyright (c) 2016-2017 David Kaloper Meršinjak. All rights reserved.
   See LICENSE.md. *)

(*
 * Game of Life with some ZX spectrum kitsch.
 *)

let flip f a b = f b a

(** Live, **)

module Coord = struct
  type t = int * int

  let compare ((a, b) : t) (c, d) =
    match compare a c with 0 -> compare b d | r -> r

  let equal ((a, b) : t) (c, d) = a = c && b = d
end

module CSet = struct
  include Set.Make (Coord)

  let of_list = List.fold_left (flip add) empty
  let map f s = fold (fun x s -> add (f x) s) s empty
end

module CMap = struct
  include Map.Make (Coord)

  let preimg p m =
    fold (fun k v s -> if p v then CSet.add k s else s) m CSet.empty
end

let erem x y = ((x mod y) + y) mod y

let square (w, h) ((a, b) as ab) =
  if a < 0 || a >= w || b < 0 || b >= h then (-1, -1) else ab

let torus (w, h) (a, b) = (erem a w, erem b h)

let moebius (w, h) ((a, b) as ab) =
  if a < 0 || a >= w then (erem a w, h - b - 1) else ab

let neigh topo (a, b) =
  [
    (a - 1, b);
    (a + 1, b);
    (a - 1, b - 1);
    (a - 1, b + 1);
    (a, b - 1);
    (a, b + 1);
    (a + 1, b - 1);
    (a + 1, b + 1);
  ]
  |> List.map topo

let step topo life =
  let nlive pt = List.(neigh topo pt |> filter (flip CSet.mem life) |> length) in
  let f1 pt acc =
    pt :: neigh topo pt
    |> List.fold_left
         (fun acc -> function
           | -1, -1 -> acc
           | pt when CMap.mem pt acc -> acc
           | pt ->
               let n = nlive pt in
               acc
               |> CMap.add pt
                    (if n = 3 || (n = 2 && CSet.mem pt life) then 0 else 1))
         acc in
  CSet.fold f1 life CMap.empty |> CMap.preimg (( = ) 0)

let glider = CSet.of_list [ (2, 1); (3, 2); (1, 3); (2, 3); (3, 3) ]

(** ...render, **)

open Notty
open Notty.Infix

let dot = I.string A.(fg lightred) "●"

let background step (n, m) =
  let k = 24. *. sin (float (step + m + n) /. 10.) |> truncate in
  if k > 0 then I.string A.(fg (gray k)) "." else I.void 1 1

let render (w, h) step life =
  I.tabulate w (h - 1) (fun x y ->
      let pt = (x, y) in
      if CSet.mem pt life then dot else background step pt)
  <-> I.(
        strf ~attr:A.(fg lightblack) "[generation %04d]" step
        |> hsnap ~align:`Right w)

(** ...and interact. **)

open Notty_miou

let ( % ) f g x = f (g x)

let timer () =
  Miou_unix.sleep 0.1 ;
  `Timer

let event term () = Stream.get (Term.events term)

let open_event = function
  | #Unescape.event as value -> value
  | `Resize _ as value -> value

let rec loop (e, t) term ((dim, n, life) as st) =
  match Miou.await_one [ e; t ] with
  | Error _exn -> ()
  | Ok (`Key (`Escape, []) | `Key (`ASCII 'C', [ `Ctrl ])) ->
      Miou.cancel t ;
      Term.release term
  | Ok `Timer ->
      Term.image term (render dim n life) ;
      let t = Miou.async timer in
      loop (e, t) term (dim, n + 1, step (torus dim) life)
  | Ok (`Mouse ((`Press `Left | `Drag), (x, y), _)) ->
      let e = Miou.async (open_event % event term) in
      loop (e, t) term (dim, n, CSet.add (torus dim (x, y)) life)
  | Ok (`Resize dim) ->
      let life = CSet.map (torus dim) life in
      Term.image term (render dim n life) ;
      let e = Miou.async (open_event % event term) in
      loop (e, t) term (dim, n, life)
  | Ok _ ->
      let e = Miou.async (open_event % event term) in
      loop (e, t) term st

let main () =
  let term = Term.create () in
  let e = Miou.async (open_event % event term) in
  let t = Miou.async timer in
  loop (e, t) term (Term.size term, 0, glider)

let () = Miou_unix.run main
