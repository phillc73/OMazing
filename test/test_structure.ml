(** Copyright 2026, Phill Clarke *)
(** SPDX-License-Identifier: AGPL-3.0-or-later *)

(** OMazing - maze structure invariant tests *)

(* These tests read the maze as a room lattice. Room (rx, ry) is grid cell
   (2*rx+1, 2*ry+1). A carved wall between two lattice-adjacent rooms is a
   lattice edge. A perfect maze has every room open, edges equal to
   rooms - 1, and no cycle (acyclic plus connected implies a tree). *)

open Maze_core

let all_algorithms =
  [
    Backtracker;
    Prims;
    GrowingTree;
    HuntAndKill;
    Eller;
    Wilson;
    FalseDoor;
    BlindSwarm;
    RandomDFS;
    RecursiveDivision;
  ]

(* Algorithms expected to produce a perfect maze (spanning tree). *)
let perfect_algorithms =
  [
    Backtracker;
    Prims;
    GrowingTree;
    HuntAndKill;
    Eller;
    Wilson;
    BlindSwarm;
    RandomDFS;
    RecursiveDivision;
  ]

let alg_name = algorithm_name
let sizes = [ Easy; Medium; Hard ]

let difficulty_name = function
  | Easy -> "Easy"
  | Medium -> "Medium"
  | Hard -> "Hard"

(* Number of rooms per side for the current dimensions. Maze dimensions are
   odd, so (w - 1) / 2 is exact. *)
let rooms_of maze =
  let h = Array.length maze in
  let w = Array.length maze.(0) in
  ((w - 1) / 2, (h - 1) / 2)

let room_cell maze rx ry = maze.((2 * ry) + 1).((2 * rx) + 1)

let is_open = function Wall -> false | Path | Start | Exit -> true

(* Count carved lattice edges. Room (rx, ry) sits at grid (2rx+1, 2ry+1). A
   horizontal edge is the open cell (2rx+2, 2ry+1) with both end rooms open; a
   vertical edge is (2rx+1, 2ry+2) with both end rooms open. This is the only
   account of the lattice the tests use. *)
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

(* Union-find over room indices. *)
let make_uf n = Array.init n (fun i -> i)

let rec find uf i =
  let p = uf.(i) in
  if p = i then i
  else begin
    let r = find uf p in
    uf.(i) <- r;
    r
  end

let check_perfect test_name maze =
  let rooms_x, rooms_y = rooms_of maze in
  let closed = ref [] in
  for ry = 0 to rooms_y - 1 do
    for rx = 0 to rooms_x - 1 do
      if not (is_open (room_cell maze rx ry)) then closed := (rx, ry) :: !closed
    done
  done;
  Alcotest.(check int) (test_name ^ ": closed room count") 0 (List.length !closed);
  let edges = carved_edges maze in
  let rooms = rooms_x * rooms_y in
  Alcotest.(check int) (test_name ^ ": carved edges = rooms - 1") (rooms - 1)
    (List.length edges);
  let uf = make_uf rooms in
  let cycles = ref 0 in
  List.iter
    (fun ((ax, ay), (bx, by)) ->
      let a = (ay * rooms_x) + ax and b = (by * rooms_x) + bx in
      let ra = find uf a and rb = find uf b in
      if ra = rb then incr cycles else uf.(ra) <- rb)
    edges;
  Alcotest.(check int) (test_name ^ ": no cycles") 0 !cycles

let with_difficulty d alg =
  let old_w = !width in
  let old_h = !height in
  set_dimensions d;
  let result = generate_maze alg in
  width := old_w;
  height := old_h;
  result

let perfect_maze_tests =
  List.map
    (fun alg ->
      Alcotest.test_case (Printf.sprintf "Perfect maze - %s" (alg_name alg)) `Quick
        (fun () ->
          List.iter
            (fun d ->
              Random.init 42;
              let maze, _, _, _, _, _ = with_difficulty d alg in
              check_perfect (difficulty_name d) maze)
            sizes))
    perfect_algorithms

(* Every non-wall cell must be reachable from the start cell. *)
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
          nx >= 0 && nx < w && ny >= 0 && ny < h
          && (not seen.(ny).(nx))
          && maze.(ny).(nx) <> Wall
        then begin
          seen.(ny).(nx) <- true;
          incr count;
          Queue.push (nx, ny) q
        end)
      [ (x + 1, y); (x - 1, y); (x, y + 1); (x, y - 1) ]
  done;
  !count

let count_open maze =
  Array.fold_left
    (Array.fold_left (fun acc c -> if c = Wall then acc else acc + 1))
    0 maze

let full_reachability_tests =
  List.map
    (fun alg ->
      Alcotest.test_case (Printf.sprintf "All open cells reachable - %s" (alg_name alg)) `Quick
        (fun () ->
          List.iter
            (fun d ->
              Random.init 7;
              let maze, sx, sy, _, _, _ = with_difficulty d alg in
              Alcotest.(check int)
                (Printf.sprintf "%s: reachable = open" (difficulty_name d))
                (count_open maze) (reachable_from maze sx sy))
            sizes))
    all_algorithms

(* False Door adds stubs only: the maze stays a perfect tree (no lattice edge
   added), and every open cell that is not on the lattice (not a room, not a
   passage between two rooms) is a dead end with exactly one open neighbour -
   the passage it hangs from. *)
let false_door_stub_test =
  Alcotest.test_case "False Door opens stub cells only" `Quick (fun () ->
      List.iter
        (fun d ->
          Random.init 11;
          let maze, _, _, _, _, _ = with_difficulty d FalseDoor in
          check_perfect (difficulty_name d) maze;
          let h = Array.length maze in
          let w = Array.length maze.(0) in
          let is_room x y = x mod 2 = 1 && y mod 2 = 1 in
          let open_at x y =
            x >= 0 && x < w && y >= 0 && y < h && maze.(y).(x) <> Wall
          in
          let on_lattice x y =
            is_room x y
            || (is_room (x - 1) y && is_room (x + 1) y)
            || (is_room x (y - 1) && is_room x (y + 1))
          in
          let stubs = ref 0 in
          for y = 0 to h - 1 do
            for x = 0 to w - 1 do
              if open_at x y && not (on_lattice x y) then begin
                incr stubs;
                let open_neighbours =
                  List.fold_left
                    (fun acc (nx, ny) -> if open_at nx ny then acc + 1 else acc)
                    0
                    [ (x + 1, y); (x - 1, y); (x, y + 1); (x, y - 1) ]
                in
                Alcotest.(check int)
                  (Printf.sprintf "%s: stub at (%d,%d) has one open neighbour"
                     (difficulty_name d) x y)
                  1 open_neighbours
              end
            done
          done;
          Alcotest.(check bool) (difficulty_name d ^ ": stubs were generated") true
            (!stubs > 0))
        sizes)

let structure_tests =
  [ ("Perfect mazes", perfect_maze_tests @ full_reachability_tests @ [ false_door_stub_test ]) ]
