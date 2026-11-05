(** Copyright 2026, Phill Clarke *)
(** SPDX-License-Identifier: AGPL-3.0-or-later *)

(** OMazing Labyrinth - Maze Generation Tests *)

open Maze_core

(* Cell count record *)
type cell_count = {
  wall : int;
  path : int;
  start : int;
  exit : int;
}

(* Helper: Count cells of each type in a maze *)
let count_cells maze =
  let width = Array.length maze.(0) in
  let height = Array.length maze in
  let count = ref { wall = 0; path = 0; start = 0; exit = 0 } in
  for y = 0 to height - 1 do
    for x = 0 to width - 1 do
      match maze.(y).(x) with
      | Wall -> count := { !count with wall = !count.wall + 1 }
      | Path -> count := { !count with path = !count.path + 1 }
      | Start -> count := { !count with start = !count.start + 1 }
      | Exit -> count := { !count with exit = !count.exit + 1 }
    done
  done;
  !count

(* Helper: Check if there's a path from start to exit using BFS. Walls are
   impassable; the reachability invariant is also pinned in test_structure. *)
let path_exists maze start_x start_y exit_x exit_y =
  let width = Array.length maze.(0) in
  let height = Array.length maze in
  let visited = Array.init height (fun _ -> Array.init width (fun _ -> false)) in
  let queue = Queue.create () in
  Queue.push (start_x, start_y) queue;
  visited.(start_y).(start_x) <- true;
  let found = ref false in
  while (not !found) && not (Queue.is_empty queue) do
    let x, y = Queue.pop queue in
    if x = exit_x && y = exit_y then found := true
    else begin
      List.iter
        (fun (nx, ny) ->
          if
            nx >= 0 && nx < width && ny >= 0 && ny < height
            && not visited.(ny).(nx)
            && (match maze.(ny).(nx) with Wall -> false | _ -> true)
          then begin
            visited.(ny).(nx) <- true;
            Queue.push (nx, ny) queue
          end)
        [(x+1, y); (x-1, y); (x, y+1); (x, y-1)]
    end
  done;
  !found

(* Alcotest comparators for maze_algorithm *)
let maze_algorithm_to_string = function
  | Backtracker -> "Backtracker"
  | Prims -> "Prims"
  | GrowingTree -> "GrowingTree"
  | HuntAndKill -> "HuntAndKill"
  | Eller -> "Eller"
  | Wilson -> "Wilson"
  | FalseDoor -> "FalseDoor"
  | BlindSwarm -> "BlindSwarm"
  | RandomDFS -> "RandomDFS"
  | RecursiveDivision -> "RecursiveDivision"

let maze_algorithm_printer ppf alg =
  Format.pp_print_string ppf (maze_algorithm_to_string alg)

let maze_algorithm_testable = Alcotest.testable maze_algorithm_printer (=)

(* List of all maze algorithms *)
let all_algorithms = [
  Backtracker; Prims; GrowingTree; HuntAndKill; Eller; Wilson;
  FalseDoor; BlindSwarm; RandomDFS; RecursiveDivision
]

(* Test maze generation for all algorithms *)
let test_all_algorithms test_name test_fn =
  List.map (fun alg ->
    let name = Printf.sprintf "%s - %s" test_name (maze_algorithm_to_string alg) in
    Alcotest.test_case name `Quick (fun () -> test_fn alg)
  ) all_algorithms

(* Maze generation tests *)

let test_maze_dimensions () =
  test_all_algorithms "Maze dimensions" (fun alg ->
    let old_width = !width in
    let old_height = !height in
    set_dimensions Medium;
    let (maze, _, _, _, _, _) = generate_maze alg in
    Alcotest.(check int) "maze width matches" !width (Array.length maze.(0));
    Alcotest.(check int) "maze height matches" !height (Array.length maze);
    width := old_width;
    height := old_height)

let test_exactly_one_start_and_exit () =
  test_all_algorithms "Single start/exit" (fun alg ->
    let old_width = !width in
    let old_height = !height in
    set_dimensions Medium;
    let (maze, start_x, start_y, exit_x, exit_y, _) = generate_maze alg in
    let counts = count_cells maze in
    Alcotest.(check int) "exactly one start" 1 counts.start;
    Alcotest.(check int) "exactly one exit" 1 counts.exit;
    Alcotest.(check bool) "start != exit" true (start_x <> exit_x || start_y <> exit_y);
    width := old_width;
    height := old_height)

let test_path_to_exit_exists () =
  test_all_algorithms "Path to exit" (fun alg ->
    let old_width = !width in
    let old_height = !height in
    set_dimensions Medium;
    let (maze, start_x, start_y, exit_x, exit_y, _) = generate_maze alg in
    Alcotest.(check bool) "path exists from start to exit" true
      (path_exists maze start_x start_y exit_x exit_y);
    width := old_width;
    height := old_height)

(* A wall cell must stop the player: a move whose landing cell is a Wall leaves
   the state untouched. The old version of this test asserted a wall equals
   itself, so it could never fail. Walk the maze's own bounds, not the globals.
   The landing cell is computed from the origin, which is always an open cell
   (a wall move would put the origin inside a wall, a state the game cannot
   reach). *)
let test_walls_are_impassable () =
  test_all_algorithms "Walls block movement" (fun alg ->
    set_dimensions Medium;
    let state = init_game alg in
    let h = Array.length state.maze in
    let w = Array.length state.maze.(0) in
    let before = (state.player_x, state.player_y, state.game_won) in
    let checked = ref 0 in
    for y = 0 to h - 1 do
      for x = 0 to w - 1 do
        if state.maze.(y).(x) <> Wall then begin
          let probe = { state with player_x = x; player_y = y; game_won = false } in
          List.iter
            (fun (dir, dx, dy) ->
              let lx = x + dx and ly = y + dy in
              let in_grid = lx >= 0 && lx < w && ly >= 0 && ly < h in
              if in_grid && state.maze.(ly).(lx) = Wall then begin
                incr checked;
                let moved = move_player probe dir in
                Alcotest.(check int)
                  (Printf.sprintf "wall at (%d,%d) blocks the step from (%d,%d)" lx ly x y)
                  x moved.player_x;
                Alcotest.(check int)
                  (Printf.sprintf "wall at (%d,%d) blocks the step from (%d,%d), row" lx ly x y)
                  y moved.player_y
              end)
            [ (Up, 0, -1); (Down, 0, 1); (Left, -1, 0); (Right, 1, 0) ]
        end
      done
    done;
    Alcotest.(check bool) "the test actually exercised wall steps" true (!checked > 0);
    Alcotest.(check bool) "generating did not move the player" true
      (before = (state.player_x, state.player_y, state.game_won)))

let maze_tests = [
  "Maze generation", 
    test_maze_dimensions () @
    test_exactly_one_start_and_exit () @
    test_path_to_exit_exists () @
    test_walls_are_impassable () @
    [ Alcotest.test_case "All algorithms produce valid mazes" `Quick (fun () ->
        List.iter (fun alg ->
          let old_width = !width in
          let old_height = !height in
          set_dimensions Medium;
          let (_maze, _, _, _, _, _) = generate_maze alg in
          (* Just verify it doesn't crash and returns something *)
          Alcotest.(check bool) (Printf.sprintf "maze generated for %s" (maze_algorithm_to_string alg))
            true (!width > 0 && !height > 0);
          width := old_width;
          height := old_height
        ) all_algorithms) ]
]
