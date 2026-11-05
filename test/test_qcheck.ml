(** Copyright 2026, Phill Clarke *)
(** SPDX-License-Identifier: AGPL-3.0-or-later *)

(** OMazing - property-based structure invariants (QCheck over the
    room-lattice contract in docs/spec.md).

    The lattice checker lives in test_structure.ml and is reused here; this
    module only adds generated inputs. Each generator is reseeded from the
    generated seed immediately before the call and the dimension globals are
    restored after it, so no property leaks state into the next case. *)

open Maze_core

(* Cases per property per algorithm. *)
let qcheck_count = 200

(* Small positive integers; QCheck's own seed capture makes a failure
   reproducible. *)
let seedgen = QCheck2.Gen.int_range 1 4096

(* Odd dimension in 5..81. A sample is a (dimensions, seed) pair; the shrinker
   stays inside the same contract: each axis moves independently toward 5 and
   the seed toward 1. *)
let dimgen = QCheck2.Gen.map (fun n -> (2 * n) + 1) (QCheck2.Gen.int_range 2 40)

let dimension_gen = QCheck2.Gen.pair dimgen dimgen

let odd_shrink v =
  let seq =
    QCheck2.Shrink.number_towards
      (module struct
        type t = int

        let equal = ( = )
        let div = ( / )
        let add = ( + )
        let sub = ( - )
        let of_int n = n
      end)
      ~destination:5 v
  in
  Seq.filter (fun n -> n >= 5 && n mod 2 = 1) seq

let dimension_shrink ((w, h), seed) : ((int * int) * int) Seq.t =
  let axes =
    Seq.map (fun w -> (w, h)) (odd_shrink w)
    |> fun s -> Seq.append s (Seq.map (fun h -> (w, h)) (odd_shrink h))
  in
  Seq.map (fun d -> (d, seed)) axes

let input_gen =
  QCheck2.Gen.set_shrink dimension_shrink (QCheck2.Gen.pair dimension_gen seedgen)

let is_open = function Wall -> false | Path | Start | Exit -> true

let rooms_of maze =
  let h = Array.length maze in
  let w = Array.length maze.(0) in
  ((w - 1) / 2, (h - 1) / 2)

let room_cell maze rx ry = maze.((2 * ry) + 1).((2 * rx) + 1)

(* Carved lattice edges; the same account as Test_structure.carved_edges. *)
let carved_edges maze =
  let rooms_x, rooms_y = rooms_of maze in
  let edges = ref [] in
  for ry = 0 to rooms_y - 1 do
    for rx = 0 to rooms_x - 1 do
      if rx + 1 < rooms_x then begin
        let wall = maze.((2 * ry) + 1).((2 * rx) + 2) in
        if
          is_open wall
          && is_open (room_cell maze rx ry)
          && is_open (room_cell maze (rx + 1) ry)
        then edges := ((rx, ry), (rx + 1, ry)) :: !edges
      end;
      if ry + 1 < rooms_y then begin
        let wall = maze.((2 * ry) + 2).((2 * rx) + 1) in
        if
          is_open wall
          && is_open (room_cell maze rx ry)
          && is_open (room_cell maze rx (ry + 1))
        then edges := ((rx, ry), (rx, ry + 1)) :: !edges
      end
    done
  done;
  !edges

let make_uf n = Array.init n (fun i -> i)

let rec find uf i =
  let p = uf.(i) in
  if p = i then i
  else begin
    let r = find uf p in
    uf.(i) <- r;
    r
  end

let closed_rooms maze =
  let rooms_x, rooms_y = rooms_of maze in
  let closed = ref 0 in
  for ry = 0 to rooms_y - 1 do
    for rx = 0 to rooms_x - 1 do
      if not (is_open (room_cell maze rx ry)) then incr closed
    done
  done;
  !closed

(* A lattice cell is a room (odd, odd) or a passage between two rooms: (even,
   odd) between horizontal neighbours, (odd, even) between vertical ones. Every
   other cell, the even-even lattice interiors, must stay Wall. *)
let is_room x y = x mod 2 = 1 && y mod 2 = 1

let on_lattice x y = is_room x y || is_room (x - 1) y || is_room (x + 1) y || is_room x (y - 1)
                  || is_room x (y + 1)

let off_lattice_open maze =
  let h = Array.length maze in
  let w = Array.length maze.(0) in
  let count = ref 0 in
  for y = 0 to h - 1 do
    for x = 0 to w - 1 do
      if (not (on_lattice x y)) && is_open maze.(y).(x) then incr count
    done
  done;
  !count

let open_at maze x y =
  let h = Array.length maze in
  let w = Array.length maze.(0) in
  x >= 0 && x < w && y >= 0 && y < h && is_open maze.(y).(x)

let open_neighbours maze x y =
  List.fold_left
    (fun acc (nx, ny) -> if open_at maze nx ny then acc + 1 else acc)
    0
    [ (x + 1, y); (x - 1, y); (x, y + 1); (x, y - 1) ]

(* An off-lattice open cell is a false door: a one-cell alcove with exactly one
   open neighbour, the passage it hangs from. *)
let bad_stubs maze =
  let h = Array.length maze in
  let w = Array.length maze.(0) in
  let count = ref 0 in
  for y = 0 to h - 1 do
    for x = 0 to w - 1 do
      if
        (not (on_lattice x y)) && open_at maze x y
        && open_neighbours maze x y <> 1
      then incr count
    done
  done;
  !count

let cycle_count maze =
  let rooms_x, rooms_y = rooms_of maze in
  let uf = make_uf (rooms_x * rooms_y) in
  List.fold_left
    (fun cycles ((ax, ay), (bx, by)) ->
      let a = (ay * rooms_x) + ax and b = (by * rooms_x) + bx in
      let ra = find uf a and rb = find uf b in
      if ra = rb then cycles + 1
      else begin
        uf.(ra) <- rb;
        cycles
      end)
    0 (carved_edges maze)

let count_open maze =
  Array.fold_left
    (Array.fold_left (fun acc c -> if c = Wall then acc else acc + 1))
    0 maze

let reachable_from maze sx sy =
  let h = Array.length maze in
  let w = Array.length maze.(0) in
  let seen = Array.make_matrix h w false in
  let q = Queue.create () in
  Queue.push (sx, sy) q;
  seen.(sy).(sx) <- true;
  let count = ref 1 in
  while not (Queue.is_empty q) do
    let x, y = Queue.pop q in
    List.iter
      (fun (nx, ny) ->
        if
          nx >= 0 && nx < w && ny >= 0 && ny < h && (not seen.(ny).(nx))
          && maze.(ny).(nx) <> Wall
        then begin
          seen.(ny).(nx) <- true;
          incr count;
          Queue.push (nx, ny) q
        end)
      [ (x + 1, y); (x - 1, y); (x, y + 1); (x, y - 1) ]
  done;
  !count

type sample = { w : int; h : int; seed : int; maze : cell array array; sx : int; sy : int }

let old_dims = lazy (!width, !height)

let generate (w, h) seed alg =
  Random.init seed;
  set_dimensions_exn w h;
  let maze, sx, sy, _, _, _ = generate_maze alg in
  let ow, oh = Lazy.force old_dims in
  width := ow;
  height := oh;
  { w; h; seed; maze; sx; sy }

let sample_gen alg =
  QCheck2.Gen.map (fun (d, seed) -> generate d seed alg) input_gen

let sample_print s = Printf.sprintf "%dx%d seed=%d" s.w s.h s.seed

(* Invariant 1: every room open, edges = rooms - 1, acyclic, and no cell off
   the room lattice is open (the spanning-tree algorithms carve nothing else).
*)
let perfect_ok maze =
  let rooms_x, rooms_y = rooms_of maze in
  let rooms = rooms_x * rooms_y in
  List.length (carved_edges maze) = rooms - 1
  && closed_rooms maze = 0
  && cycle_count maze = 0
  && off_lattice_open maze = 0

let perfect_prop s = perfect_ok s.maze

(* Invariant 2: every open cell reachable from Start. *)
let reachable_prop s = reachable_from s.maze s.sx s.sy = count_open s.maze

(* Invariant 3: False Door is the Backtracker tree plus stub cells only, and
   every stub is a one-neighbour alcove off the lattice. The edge-count half is
   compared on the Backtracker base alone: reported edges stop counting a wall
   the moment a neighbouring room is closed, so a shared conjunct would let a
   closed room slip past this check. *)
let false_door_prop s =
  Random.init s.seed;
  set_dimensions_exn s.w s.h;
  let base, _, _, _, _, _ = generate_maze Backtracker in
  let ow, oh = Lazy.force old_dims in
  width := ow;
  height := oh;
  let base_edges = List.length (carved_edges base) in
  perfect_ok base
  && List.length (carved_edges s.maze) = base_edges
  && List.length (carved_edges s.maze) = (fst (rooms_of s.maze) * snd (rooms_of s.maze)) - 1
  && bad_stubs s.maze = 0

let perfect_tests =
  List.map
    (fun alg ->
      QCheck_alcotest.to_alcotest
        ~speed_level:`Quick
        (QCheck2.Test.make ~count:qcheck_count
           ~name:(Printf.sprintf "Perfect maze - %s" (algorithm_name alg))
           ~print:sample_print
           (sample_gen alg) perfect_prop))
    Test_structure.perfect_algorithms

let reachable_tests =
  List.map
    (fun alg ->
      QCheck_alcotest.to_alcotest
        ~speed_level:`Quick
        (QCheck2.Test.make ~count:qcheck_count
           ~name:(Printf.sprintf "All open cells reachable - %s" (algorithm_name alg))
           ~print:sample_print
           (sample_gen alg) reachable_prop))
    Test_structure.all_algorithms

let false_door_reach_tests =
  [
    QCheck_alcotest.to_alcotest
      ~speed_level:`Quick
      (QCheck2.Test.make ~count:qcheck_count ~name:"All open cells reachable - False Door"
         ~print:sample_print
         (sample_gen FalseDoor) reachable_prop);
  ]

let false_door_tests =
  [
    QCheck_alcotest.to_alcotest
      ~speed_level:`Quick
      (QCheck2.Test.make ~count:qcheck_count
         ~name:"False Door stub contract - no lattice edge added"
         ~print:sample_print
         (sample_gen FalseDoor) false_door_prop);
  ]

let property_tests =
  [
    ("Properties: perfect mazes", perfect_tests);
    ("Properties: reachability", reachable_tests @ false_door_reach_tests);
    ("Properties: false door", false_door_tests);
  ]
