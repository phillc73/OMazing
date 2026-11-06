(** Copyright 2026, Phill Clarke *)
(** SPDX-License-Identifier: AGPL-3.0-or-later *)

(* Maze core: generation and game state, with no Graphics dependency.
   Mutability is confined to array carving, which is the normal OCaml idiom for
   generation and cannot be expressed as a fold without leaving generation cost
   on the table. *)

(* Difficulty levels *)
type difficulty = Easy | Medium | Hard

(* Menu dimensions (fixed) and game dimensions (configurable) *)
let menu_width = 41
let menu_height = 38
let cell_size = 25

(* Screen resolution handling - fit maze to screen *)
let max_screen_width = 1920  (* Typical max screen width *)
let max_screen_height = 1080  (* Typical max screen height *)

(* Calculate window dimensions that fit on screen *)
let calc_window_dimensions maze_width maze_height =
  let desired_width = maze_width * cell_size in
  let desired_height = maze_height * cell_size in
  let w = min desired_width max_screen_width in
  let h = min desired_height max_screen_height in
  (w, h)

(* Game dimensions - will be set based on selected difficulty *)
let width = ref 41
let height = ref 41

(* The generators carve a room lattice on odd coordinates and assume at least
   one room on each axis, so a dimension below 3 has no valid lattice. Sizes
   must also be odd: the lattice is (n - 1) / 2 rooms, and an even size would
   silently lose the last row or column. *)
let set_dimensions_exn w h =
  if w < 3 || h < 3 then
    invalid_arg
      (Printf.sprintf "maze dimensions must be at least 3x3, got %dx%d" w h);
  if w mod 2 = 0 || h mod 2 = 0 then
    invalid_arg
      (Printf.sprintf "maze dimensions must be odd, got %dx%d" w h);
  width := w;
  height := h

let set_dimensions = function
  | Easy -> set_dimensions_exn 21 21
  | Medium -> set_dimensions_exn 41 41
  | Hard -> set_dimensions_exn 81 81

(* Maze generation algorithms *)
type maze_algorithm = 
  | Backtracker
  | Prims
  | GrowingTree
  | HuntAndKill
  | Eller
  | Wilson
  | FalseDoor
  | BlindSwarm
  | RandomDFS
  | RecursiveDivision

(* Menu result types *)
type menu_result = Algorithm of maze_algorithm | Back | Quit

(* What the difficulty menu returns. A difficulty is settled; Quit is the
   user leaving, which must reach main rather than read as "no key". *)
type difficulty_result = Difficulty of difficulty | Difficulty_quit

let algorithm_name = function
  | Backtracker -> "Recursive Backtracker"
  | Prims -> "Prim's Algorithm"
  | GrowingTree -> "Growing Tree"
  | HuntAndKill -> "Hunt-and-Kill"
  | Eller -> "Eller's Algorithm"
  | Wilson -> "Wilson's Algorithm"
  | FalseDoor -> "False Door"
  | BlindSwarm -> "Blind Swarm"
  | RandomDFS -> "Randomised DFS"
  | RecursiveDivision -> "Recursive Division"

let algorithm_description = function
  | Backtracker -> 
      ["Creates long, winding passages with a depth-first exploration pattern.";
       "Tends to have longer corridors with fewer branches.";
       "Starts from top-left, resulting in mazes with a slight diagonal bias."]
  | Prims -> 
      ["Grows the maze like a spanning tree from a random starting point.";
       "Produces more uniform mazes with balanced corridors and open spaces.";
       "Passages are generally shorter and more evenly distributed."]
  | GrowingTree ->
      ["Generalises Prim's and Backtracker - uses both newest and random selection.";
       "With 50% probability: extends newest path (like Backtracker) or random cell.";
       "Produces mazes with a natural mix of long corridors and open areas."]
  | HuntAndKill ->
      ["Alternates between random walk ('hunt') and scanning for unvisited cells ('kill').";
       "Creates mazes with long meandering passages and sudden turns.";
       "Efficient with a good balance of speed and passage quality."]
  | Eller ->
      ["Single-pass algorithm that processes one row at a time.";
       "Very efficient for large mazes with O(n) complexity.";
       "Tends to create mazes with a horizontal bias and long corridors."]
  | Wilson ->
      ["Uses loop-erased random walks to create perfectly uniform mazes.";
       "Slowest algorithm but produces the most uniform, unbiased results.";
       "Every cell is equally likely to be connected to any other."]
  | FalseDoor ->
      ["Standard maze with added 'false doors' - one-cell alcoves.";
       "Each door is a dead end that joins nothing, so no route changes.";
       "Adds visual complexity without creating alternative paths."]
  | BlindSwarm ->
      ["Multiple simultaneous random walks that explore and carve the maze.";
       "A walker joins the maze only when its step is accepted, never by luck.";
       "Walkers merge when they meet, forming a swarm-like pattern."]
  | RandomDFS ->
      ["Depth-first search with randomised neighbor selection.";
       "Similar to Backtracker but implemented iteratively with explicit stack.";
       "Creates mazes with long, winding passages and deep recursion patterns."]
  | RecursiveDivision ->
      ["Top-down divide-and-conquer approach with walls and passages.";
       "Creates maze by recursively splitting regions with dividing walls.";
       "Produces mazes with long straight passages and grid-like structure."]

(* Game state *)
type direction = Up | Down | Left | Right

type cell = 
  | Wall
  | Path
  | Start
  | Exit

type game_state = {
  maze : cell array array;
  player_x : int;
  player_y : int;
  exit_x : int;
  exit_y : int;
  game_won : bool;
  algorithm : maze_algorithm;
}

(* Prim's algorithm wall entry for frontier tracking *)
type wall_entry = { wx : int; wy : int; rx : int; ry : int; drx : int; dry : int }

(* Initialise an empty maze filled with walls *)
let init_maze () : cell array array = 
  Array.init !height (fun _ -> Array.init !width (fun _ -> Wall))

(* Check if coordinates are within bounds *)
let in_bounds x y = 
  x >= 0 && x < !width && y >= 0 && y < !height

(* Mark the start and exit cells. Both must already be carved: a generator that
   hands back an exit sitting in a wall is a generator bug, and silently
   overwriting a corridor would destroy a passage. *)
let mark_start_exit maze (start_x, start_y) (exit_x, exit_y) =
  let is_carved x y =
    match maze.(y).(x) with Path | Start | Exit -> true | Wall -> false
  in
  if not (is_carved start_x start_y) then
    invalid_arg "mark_start_exit: start cell is not carved";
  if not (is_carved exit_x exit_y) then
    invalid_arg "mark_start_exit: exit cell is not carved";
  maze.(start_y).(start_x) <- Start;
  maze.(exit_y).(exit_x) <- Exit

(* Start room draw: the top-left half of the room lattice (rooms 0 .. n/2 - 1 on
   each axis), so the player begins far from the bottom-right exit. Rounded up so
   a single-room lattice still yields room 0 rather than an empty range. *)
let pick_start rooms_x rooms_y =
  let half n = max 1 ((n + 1) / 2) in
  (Random.int (half rooms_x), Random.int (half rooms_y))

(* Generate a maze using the standard recursive backtracker algorithm *)
let generate_maze_backtracker () = 
  let maze = init_maze () in
  
  (* Number of rooms in each direction (20x20 rooms in a 41x41 grid) *)
  let rooms_x = (!width - 1) / 2 in
  let rooms_y = (!height - 1) / 2 in
  
  (* Step 1: Mark all room positions as Path *)
  for ry = 0 to rooms_y - 1 do
    for rx = 0 to rooms_x - 1 do
      let y = 2 * ry + 1 in
      let x = 2 * rx + 1 in
      maze.(y).(x) <- Path
    done
  done;
  
  (* Step 2: Recursive backtracker to connect rooms *)
  let visited = Array.init rooms_y (fun _ -> Array.init rooms_x (fun _ -> false)) in
  
  let rec dfs rx ry = 
    visited.(ry).(rx) <- true;
    
    (* Possible directions: up, down, left, right in room coordinates *)
    let neighbors = [
      (rx, ry - 1, 0, -1); (* up *)
      (rx, ry + 1, 0, 1);  (* down *)
      (rx - 1, ry, -1, 0); (* left *)
      (rx + 1, ry, 1, 0)   (* right *)
    ] in
    
    (* Shuffle neighbors randomly *)
    let len = List.length neighbors in
    let arr = Array.of_list neighbors in
    for i = 0 to len - 1 do
      let j = Random.int (len - i) + i in
      let tmp = arr.(i) in
      arr.(i) <- arr.(j);
      arr.(j) <- tmp
    done;
    let shuffled = Array.to_list arr in
    
    List.iter (fun (nrx, nry, drx, dry) ->
      if nrx >= 0 && nrx < rooms_x && nry >= 0 && nry < rooms_y && not visited.(nry).(nrx) then begin
        (* Carve the wall between the two rooms *)
        let wx = 2 * rx + 1 + drx in
        let wy = 2 * ry + 1 + dry in
        maze.(wy).(wx) <- Path;
        
        visited.(nry).(nrx) <- true;
        dfs nrx nry
      end
    ) shuffled
  in
  
  (* Start from top-left room (0,0) which is grid (1,1) *)
  dfs 0 0;
  
  (* Start at grid position (1,1) *)
  let start_x = 1 in
  let start_y = 1 in
  
  (* Exit at bottom-right room *)
  let exit_x = 2 * (rooms_x - 1) + 1 in
  let exit_y = 2 * (rooms_y - 1) + 1 in
  
  (* Mark start and exit *)
  mark_start_exit maze (start_x, start_y) (exit_x, exit_y);
  
  (maze, start_x, start_y, exit_x, exit_y, Backtracker)

(* Generate a maze using Prim's algorithm *)
let generate_maze_prims () = 
  let maze = init_maze () in
  
  (* Number of rooms in each direction (20x20 rooms in a 41x41 grid) *)
  let rooms_x = (!width - 1) / 2 in
  let rooms_y = (!height - 1) / 2 in
  
  (* Track which rooms are in the maze (part of the spanning tree) *)
  let in_maze = Array.init rooms_y (fun _ -> Array.init rooms_x (fun _ -> false)) in
  
  (* Frontier: list of wall_entry records *)
  (* Add walls of a room to the frontier *)
  let add_room_walls rx ry frontier = 
    (* Check each direction: up, down, left, right *)
    (* For each direction (drx, dry) to neighboring room, 
       the wall between rooms is at (2*rx+1+wx_off, 2*ry+1+wy_off) *)
    let directions = [
      (0, -1, 0, -1); (* up: wall is at (2*rx+1, 2*ry) *)
      (0, 1, 0, 1);  (* down: wall is at (2*rx+1, 2*ry+2) *)
      (-1, 0, -1, 0); (* left: wall is at (2*rx, 2*ry+1) *)
      (1, 0, 1, 0)    (* right: wall is at (2*rx+2, 2*ry+1) *)
    ] in
    
    List.fold_left (fun acc (drx, dry, wx_off, wy_off) ->
      let nrx = rx + drx in
      let nry = ry + dry in
      if nrx >= 0 && nrx < rooms_x && nry >= 0 && nry < rooms_y && not in_maze.(nry).(nrx) then begin
        let wx = 2 * rx + 1 + wx_off in
        let wy = 2 * ry + 1 + wy_off in
        {wx; wy; rx; ry; drx; dry} :: acc
      end else
        acc
    ) frontier directions
  in
  
  (* Pick a random start room from the top-left half, far from the exit *)
  let start_rx, start_ry = pick_start rooms_x rooms_y in
  
  (* Mark start room as in maze *)
  in_maze.(start_ry).(start_rx) <- true;
  
  (* Mark the room cell as Path *)
  let start_grid_x = 2 * start_rx + 1 in
  let start_grid_y = 2 * start_ry + 1 in
  maze.(start_grid_y).(start_grid_x) <- Path;
  
  (* Initialise frontier with walls of start room *)
  let frontier = add_room_walls start_rx start_ry [] in
  
  (* Prim's algorithm main loop. The frontier is a growable array: a random
     index is swapped with the last entry and popped, so each step is O(1) and
     no value has to be found and filtered out. *)
  let frontier = ref (Array.of_list frontier) in
  let frontier_len = ref (Array.length !frontier) in
  while !frontier_len > 0 do
    (* Pick a random wall from the frontier and pop it *)
    let idx = Random.int !frontier_len in
    let wall = (!frontier).(idx) in
    decr frontier_len;
    (!frontier).(idx) <- (!frontier).(!frontier_len);
    
    (* Get the room on the other side of this wall *)
    let other_rx = wall.rx + wall.drx in
    let other_ry = wall.ry + wall.dry in
    
    (* Check if that room is not yet in the maze *)
    if other_rx >= 0 && other_rx < rooms_x && other_ry >= 0 && other_ry < rooms_y 
       && not in_maze.(other_ry).(other_rx) then begin
      (* Carve this wall - connect the rooms *)
      maze.(wall.wy).(wall.wx) <- Path;
      
      (* Mark the other room as in maze *)
      in_maze.(other_ry).(other_rx) <- true;
      
      (* Mark the other room's cell as Path *)
      let other_x = 2 * other_rx + 1 in
      let other_y = 2 * other_ry + 1 in
      maze.(other_y).(other_x) <- Path;
      
      (* Add walls of the other room to the frontier *)
      let new_walls = add_room_walls other_rx other_ry [] in
      List.iter
        (fun w ->
          if !frontier_len = Array.length !frontier then begin
            let bigger = Array.make (max 8 (2 * Array.length !frontier)) wall in
            Array.blit !frontier 0 bigger 0 !frontier_len;
            frontier := bigger
          end;
          (!frontier).(!frontier_len) <- w;
          incr frontier_len)
        new_walls
    end
  done;
  
  (* Start at grid position of the start room *)
  let start_x = start_grid_x in
  let start_y = start_grid_y in
  
  (* Exit at bottom-right room *)
  let exit_rx = rooms_x - 1 in
  let exit_ry = rooms_y - 1 in
  let exit_x = 2 * exit_rx + 1 in
  let exit_y = 2 * exit_ry + 1 in
  
  (* Ensure exit is Path *)
  maze.(exit_y).(exit_x) <- Path;
  
  (* Mark start and exit *)
  mark_start_exit maze (start_x, start_y) (exit_x, exit_y);
  
  (maze, start_x, start_y, exit_x, exit_y, Prims)

(* Generate a maze using the Growing Tree algorithm *)
(* Growing Tree generalises both Prim's and Backtracker:
   - With probability p, pick the newest cell (like Backtracker/DFS)
   - With probability 1-p, pick a random cell from the tree (like Prim's)
   - Here we use p=0.5 for a balanced result *)
let generate_maze_growing_tree () = 
  let maze = init_maze () in
  
  (* Number of rooms in each direction (20x20 rooms in a 41x41 grid) *)
  let rooms_x = (!width - 1) / 2 in
  let rooms_y = (!height - 1) / 2 in
  
  (* Track which rooms have been visited *)
  let visited = Array.init rooms_y (fun _ -> Array.init rooms_x (fun _ -> false)) in
  
  (* The "tree" - list of active cells that have unvisited neighbors *)
  let tree = ref [] in
  
  (* Pick a random start room far from the exit (bottom-right) *)
  (* Start from top-left half to ensure good distance from bottom-right exit *)
  let exit_rx = rooms_x - 1 in
  let exit_ry = rooms_y - 1 in
  
  (* Pick start from the top-left half of the grid, far from the exit *)
  let start_rx, start_ry = pick_start rooms_x rooms_y in
  
  (* Mark start room as visited *)
  visited.(start_ry).(start_rx) <- true;
  
  (* Mark the room cell as Path *)
  let start_grid_x = 2 * start_rx + 1 in
  let start_grid_y = 2 * start_ry + 1 in
  maze.(start_grid_y).(start_grid_x) <- Path;
  
  (* Add start room to the tree *)
  tree := [(start_rx, start_ry)];
  
  (* Growing Tree main loop *)
  while !tree <> [] do
    (* Choose a cell from the tree: 50% newest (last), 50% random *)
    let idx = 
      if Random.bool () then
        List.length !tree - 1  (* newest - last element *)
      else
        Random.int (List.length !tree)  (* random *)
    in
    let (cx, cy) = List.nth !tree idx in
    
    (* Find all unvisited neighbors of this cell *)
    let neighbors = [
      (cx, cy - 1, 0, -1); (* up *)
      (cx, cy + 1, 0, 1);  (* down *)
      (cx - 1, cy, -1, 0); (* left *)
      (cx + 1, cy, 1, 0)   (* right *)
    ] in
    
    (* Filter to get valid unvisited neighbors *)
    let valid_neighbors = 
      List.filter (fun (nrx, nry, _, _) ->
        nrx >= 0 && nrx < rooms_x && nry >= 0 && nry < rooms_y && not visited.(nry).(nrx)
      ) neighbors
    in
    
    if valid_neighbors <> [] then begin
      (* Pick a random unvisited neighbor *)
      let nidx = Random.int (List.length valid_neighbors) in
      let (nrx, nry, drx, dry) = List.nth valid_neighbors nidx in
      
      (* Carve the wall between current cell and neighbor *)
      let wx = 2 * cx + 1 + drx in
      let wy = 2 * cy + 1 + dry in
      maze.(wy).(wx) <- Path;
      
      (* Mark neighbor as visited *)
      visited.(nry).(nrx) <- true;
      
      (* Mark neighbor room cell as Path *)
      let nx = 2 * nrx + 1 in
      let ny = 2 * nry + 1 in
      maze.(ny).(nx) <- Path;
      
      (* Add neighbor to the tree *)
      tree := !tree @ [(nrx, nry)]
    end else begin
      (* No unvisited neighbors - remove this cell from the tree *)
      tree := List.filter (fun (x, y) -> x <> cx || y <> cy) !tree
    end
  done;
  
  (* Start at grid position of the start room *)
  let start_x = start_grid_x in
  let start_y = start_grid_y in
  
  (* Exit at bottom-right room - already defined earlier *)
  let exit_x = 2 * exit_rx + 1 in
  let exit_y = 2 * exit_ry + 1 in
  
  (* Ensure exit is Path *)
  maze.(exit_y).(exit_x) <- Path;
  
  (* Mark start and exit *)
  mark_start_exit maze (start_x, start_y) (exit_x, exit_y);
  
  (maze, start_x, start_y, exit_x, exit_y, GrowingTree)

(* Generate a maze using the Hunt-and-Kill algorithm *)
(* Hunt-and-Kill alternates between random walk and systematic scan:
   1. Start from a cell, perform random walk carving passages
   2. When stuck, scan the grid for a cell with unvisited neighbors
   3. Continue until all rooms are visited *)
let generate_maze_hunt_and_kill () = 
  let maze = init_maze () in
  
  (* Number of rooms in each direction (20x20 rooms in a 41x41 grid) *)
  let rooms_x = (!width - 1) / 2 in
  let rooms_y = (!height - 1) / 2 in
  
  (* Track which rooms have been visited *)
  let visited = Array.init rooms_y (fun _ -> Array.init rooms_x (fun _ -> false)) in
  
  (* Pick start from top-left half of the grid *)
  let start_rx, start_ry = pick_start rooms_x rooms_y in
  
  (* Mark all room cells as Path first *)
  for ry = 0 to rooms_y - 1 do
    for rx = 0 to rooms_x - 1 do
      let y = 2 * ry + 1 in
      let x = 2 * rx + 1 in
      maze.(y).(x) <- Path
    done
  done;
  
  (* Mark start as visited *)
  visited.(start_ry).(start_rx) <- true;
  
  (* Hunt-and-Kill main loop *)
  let running = ref true in
  let current_rx = ref start_rx in
  let current_ry = ref start_ry in
  
  while !running do
    (* Hunt phase: loop the random walk from the current cell. A loop rather
       than recursion so the walk cannot grow the stack on a large grid. *)
    let walking = ref true in
    while !walking do
      let rx = !current_rx and ry = !current_ry in
      let directions = [
        (rx, ry - 1, 0, -1); (* up *)
        (rx, ry + 1, 0, 1);  (* down *)
        (rx - 1, ry, -1, 0); (* left *)
        (rx + 1, ry, 1, 0)   (* right *)
      ] in
      
      let unvisited_neighbors = 
        List.filter (fun (nrx, nry, _, _) ->
          nrx >= 0 && nrx < rooms_x && nry >= 0 && nry < rooms_y && not visited.(nry).(nrx)
        ) directions
      in
      
      match unvisited_neighbors with
      | [] -> walking := false
      | _ ->
        (* Pick random unvisited neighbor *)
        let nidx = Random.int (List.length unvisited_neighbors) in
        let (nrx, nry, drx, dry) = List.nth unvisited_neighbors nidx in
        
        (* Carve wall between current and neighbor *)
        let wx = 2 * rx + 1 + drx in
        let wy = 2 * ry + 1 + dry in
        maze.(wy).(wx) <- Path;
        
        (* Mark neighbor as visited and continue from it *)
        visited.(nry).(nrx) <- true;
        current_rx := nrx;
        current_ry := nry
    done;
    
    (* Kill phase: hunt from a random visited cell that still has an unvisited
       neighbour. A random start keeps the reprise from always resuming at the
       top-left cell. *)
    let found = ref false in
    let start_cell = Random.int (rooms_x * rooms_y) in
    let attempts = ref 0 in
    while (not !found) && !attempts < rooms_x * rooms_y do
      let idx = (start_cell + !attempts) mod (rooms_x * rooms_y) in
      let rx = idx mod rooms_x and ry = idx / rooms_x in
      incr attempts;
      if visited.(ry).(rx) then begin
        let directions = [
          (rx, ry - 1, 0, -1); (* up *)
          (rx, ry + 1, 0, 1);  (* down *)
          (rx - 1, ry, -1, 0); (* left *)
          (rx + 1, ry, 1, 0)   (* right *)
        ] in
        let unvisited_neighbors = 
          List.filter (fun (nrx, nry, _, _) ->
            nrx >= 0 && nrx < rooms_x && nry >= 0 && nry < rooms_y && not visited.(nry).(nrx)
          ) directions
        in
        if unvisited_neighbors <> [] then begin
          (* Pick random unvisited neighbor *)
          let nidx = Random.int (List.length unvisited_neighbors) in
          let (nrx, nry, drx, dry) = List.nth unvisited_neighbors nidx in
          
          (* Carve wall between current and neighbor *)
          let wx = 2 * rx + 1 + drx in
          let wy = 2 * ry + 1 + dry in
          maze.(wy).(wx) <- Path;
          
          (* Mark neighbor as visited *)
          visited.(nry).(nrx) <- true;
          
          (* Resume the walk from the neighbour *)
          current_rx := nrx;
          current_ry := nry;
          found := true
        end
      end
    done;
    
    (* If we didn't find any cell with unvisited neighbors, we're done *)
    if not !found then running := false
  done;
  
  (* Start at grid position of the start room *)
  let start_x = 2 * start_rx + 1 in
  let start_y = 2 * start_ry + 1 in
  
  (* Exit at bottom-right room *)
  let exit_rx = rooms_x - 1 in
  let exit_ry = rooms_y - 1 in
  let exit_x = 2 * exit_rx + 1 in
  let exit_y = 2 * exit_ry + 1 in
  
  (* Ensure exit is Path *)
  maze.(exit_y).(exit_x) <- Path;
  
  (* Mark start and exit *)
  mark_start_exit maze (start_x, start_y) (exit_x, exit_y);
  
  (maze, start_x, start_y, exit_x, exit_y, HuntAndKill)

(* Generate a maze using Eller's algorithm *)
(* Eller's is a single-pass algorithm that processes one row at a time:
   1. Each row starts with each cell in its own set
   2. Randomly merge adjacent cells in the same row (horizontal connections)
   3. For each set, randomly carve a vertical passage down to the next row
   4. Repeat for all rows
   5. Last row: merge all remaining cells *)
let generate_maze_eller () = 
  let maze = init_maze () in
  
  (* Number of rooms in each direction (20x20 rooms in a 41x41 grid) *)
  let rooms_x = (!width - 1) / 2 in
  let rooms_y = (!height - 1) / 2 in
  
  (* Mark all room cells as Path *)
  for ry = 0 to rooms_y - 1 do
    for rx = 0 to rooms_x - 1 do
      let y = 2 * ry + 1 in
      let x = 2 * rx + 1 in
      maze.(y).(x) <- Path
    done
  done;
  
  (* For Eller's, we track sets of rooms per row.
     Each set is identified by a unique integer. *)
  
  (* Process each row *)
  let next_set_id = ref 0 in
  
  (* For each row, track which set each cell belongs to *)
  let row_sets = Array.init rooms_y (fun _ -> Array.init rooms_x (fun _ -> -1)) in
  
  for ry = 0 to rooms_y - 1 do
    (* Step 1: Initialise each cell in this row with its own set *)
    if ry = 0 then begin
      for rx = 0 to rooms_x - 1 do
        row_sets.(ry).(rx) <- !next_set_id;
        next_set_id := !next_set_id + 1
      done
    end else begin
      (* For rows after the first, sets come from vertical connections above *)
      (* We'll initialise unassigned cells with new sets *)
      for rx = 0 to rooms_x - 1 do
        if row_sets.(ry).(rx) = -1 then begin
          row_sets.(ry).(rx) <- !next_set_id;
          next_set_id := !next_set_id + 1
        end
      done
    end;
    
    (* Step 2: Randomly merge adjacent cells in this row *)
    for rx = 0 to rooms_x - 2 do
      if row_sets.(ry).(rx) <> row_sets.(ry).(rx + 1) && Random.bool () then begin
        (* Merge the two sets - set all of the right set to the left set's ID *)
        let left_set = row_sets.(ry).(rx) in
        let right_set = row_sets.(ry).(rx + 1) in
        (* Merge: change all cells with right_set to left_set in this row *)
        for rx2 = 0 to rooms_x - 1 do
          if row_sets.(ry).(rx2) = right_set then
            row_sets.(ry).(rx2) <- left_set
        done;
        (* Carve the wall between them *)
        let wx = 2 * rx + 2 in
        let wy = 2 * ry + 1 in
        maze.(wy).(wx) <- Path
      end
    done;
    
    (* Step 3: For each set in this row, randomly carve down to next row *)
    if ry < rooms_y - 1 then begin
      (* Collect unique sets in this row *)
      let sets_in_row = Hashtbl.create 20 in
      for rx = 0 to rooms_x - 1 do
        Hashtbl.replace sets_in_row row_sets.(ry).(rx) ()
      done;
      let set_list = Hashtbl.fold (fun k _ acc -> k :: acc) sets_in_row [] in
      
      List.iter (fun set_id ->
        (* Find all cells in this set *)
        let cells = ref [] in
        for rx = 0 to rooms_x - 1 do
          if row_sets.(ry).(rx) = set_id then
            cells := (rx, ry) :: !cells
        done;
        
        (* Pick a random cell from this set to carve down *)
        if !cells <> [] then begin
          let idx = Random.int (List.length !cells) in
          let (cx, _) = List.nth !cells idx in
          (* Carve vertical wall down from this cell *)
          let wx = 2 * cx + 1 in
          let wy = 2 * ry + 2 in
          maze.(wy).(wx) <- Path;
          (* Set the cell below to be in the same set *)
          row_sets.(ry + 1).(cx) <- set_id
        end
      ) set_list
    end
  done;
  
  (* Step 4: For the last row, merge any remaining separate cells *)
  let ry = rooms_y - 1 in
  for rx = 0 to rooms_x - 2 do
    if row_sets.(ry).(rx) <> row_sets.(ry).(rx + 1) then begin
      (* Carve the wall between them *)
      let wx = 2 * rx + 2 in
      let wy = 2 * ry + 1 in
      maze.(wy).(wx) <- Path;
      (* Merge sets *)
      let left_set = row_sets.(ry).(rx) in
      let right_set = row_sets.(ry).(rx + 1) in
      for rx2 = 0 to rooms_x - 1 do
        if row_sets.(ry).(rx2) = right_set then
          row_sets.(ry).(rx2) <- left_set
      done
    end
  done;
  
  (* Pick start from top-left half of the grid *)
  let start_rx, start_ry = pick_start rooms_x rooms_y in
  let start_x = 2 * start_rx + 1 in
  let start_y = 2 * start_ry + 1 in
  
  (* Exit at bottom-right room *)
  let exit_rx = rooms_x - 1 in
  let exit_ry = rooms_y - 1 in
  let exit_x = 2 * exit_rx + 1 in
  let exit_y = 2 * exit_ry + 1 in
  
  (* Mark start and exit *)
  mark_start_exit maze (start_x, start_y) (exit_x, exit_y);
  
  (maze, start_x, start_y, exit_x, exit_y, Eller)

(* Generate a maze using Wilson's algorithm *)
(* Wilson's algorithm uses loop-erased random walks:
   1. Start with one visited cell (the start)
   2. Pick a random unvisited cell
   3. Perform a random walk from it until hitting a visited cell
   4. Erase loops from the path (remove cells visited twice)
   5. Carve the remaining path into the maze
   6. Mark all cells on the path as visited
   7. Repeat until all cells are visited *)
let generate_maze_wilson () = 
  let maze = init_maze () in
  
  (* Number of rooms in each direction (20x20 rooms in a 41x41 grid) *)
  let rooms_x = (!width - 1) / 2 in
  let rooms_y = (!height - 1) / 2 in
  
  (* Track which rooms have been visited *)
  let visited = Array.init rooms_y (fun _ -> Array.init rooms_x (fun _ -> false)) in
  
  (* Mark all room cells as Path *)
  for ry = 0 to rooms_y - 1 do
    for rx = 0 to rooms_x - 1 do
      let y = 2 * ry + 1 in
      let x = 2 * rx + 1 in
      maze.(y).(x) <- Path
    done
  done;
  
  (* Pick start from top-left half of the grid *)
  let start_rx, start_ry = pick_start rooms_x rooms_y in
  visited.(start_ry).(start_rx) <- true;
  
  (* Total rooms and count of visited *)
  let total_rooms = rooms_x * rooms_y in
  let visited_count = ref 1 in
  
  (* Wilson's main loop: continue until all rooms are visited *)
  while !visited_count < total_rooms do
    (* Find a random unvisited room *)
    let rec find_unvisited () =
      let rx = Random.int rooms_x in
      let ry = Random.int rooms_y in
      if not visited.(ry).(rx) then (rx, ry)
      else find_unvisited ()
    in
    
    let (start_walk_rx, start_walk_ry) = find_unvisited () in
    
    (* Perform loop-erased random walk from this cell *)
    (* The path is stored as a list of (rx, ry) visited, in order *)
    (* When we hit a cell already in the path, we remove the loop *)
    let path = ref [(start_walk_rx, start_walk_ry)] in
    let current_rx = ref start_walk_rx in
    let current_ry = ref start_walk_ry in
    let walking = ref true in
    
    while !walking do
      (* Get unvisited neighbors of current cell *)
      let directions = [
        (!current_rx, !current_ry - 1); (* up *)
        (!current_rx, !current_ry + 1); (* down *)
        (!current_rx - 1, !current_ry); (* left *)
        (!current_rx + 1, !current_ry)  (* right *)
      ] in
      
      (* Filter to valid room coordinates *)
      let neighbors = 
        List.filter (fun (rx, ry) ->
          rx >= 0 && rx < rooms_x && ry >= 0 && ry < rooms_y
        ) directions
      in
      
      if neighbors <> [] then begin
        (* Pick random neighbor *)
        let nidx = Random.int (List.length neighbors) in
        let (nrx, nry) = List.nth neighbors nidx in
        
        (* Check if this neighbor is already in our path *)
        let rec in_path rx ry = function
          | [] -> false
          | (prx, pry) :: _ when prx = rx && pry = ry -> true
          | _ :: tl -> in_path rx ry tl
        in
        
        if in_path nrx nry !path then begin
          (* Find where this cell appears in the path *)
          let rec find_index rx ry idx = function
            | [] -> -1
            | (prx, pry) :: _ when prx = rx && pry = ry -> idx
            | _ :: tl -> find_index rx ry (idx + 1) tl
          in
          let idx = find_index nrx nry 0 !path in
          (* Remove the loop: keep path up to and including this cell *)
          let new_path = List.filteri (fun i _ -> i <= idx) !path in
          path := new_path;
          current_rx := nrx;
          current_ry := nry;
          
          (* Check if we've reached a visited cell *)
          if visited.(nry).(nrx) then
            walking := false
        end else begin
          (* Add to path and continue *)
          path := !path @ [(nrx, nry)];
          current_rx := nrx;
          current_ry := nry;
          
          (* Check if we've reached a visited cell *)
          if visited.(nry).(nrx) then
            walking := false
        end
      end else begin
        (* No neighbors - shouldn't happen in a grid, but just in case *)
        walking := false
      end
    done;
    
    (* Carve the path into the maze and mark as visited *)
    (* The path connects an unvisited cell to a visited cell *)
    (* We carve walls between consecutive cells in the path *)
    for i = 0 to List.length !path - 2 do
      let (rx1, ry1) = List.nth !path i in
      let (rx2, ry2) = List.nth !path (i + 1) in
      
      (* Carve wall between these two rooms *)
      let wx = 2 * rx1 + 1 + (rx2 - rx1) in
      let wy = 2 * ry1 + 1 + (ry2 - ry1) in
      maze.(wy).(wx) <- Path;
      
      (* Mark as visited *)
      if not visited.(ry2).(rx2) then begin
        visited.(ry2).(rx2) <- true;
        visited_count := !visited_count + 1
      end
    done;
    
    (* Also mark the start of the walk as visited *)
    if not visited.(start_walk_ry).(start_walk_rx) then begin
      visited.(start_walk_ry).(start_walk_rx) <- true;
      visited_count := !visited_count + 1
    end
  done;
  
  (* Start at grid position of the start room *)
  let start_x = 2 * start_rx + 1 in
  let start_y = 2 * start_ry + 1 in
  
  (* Exit at bottom-right room *)
  let exit_rx = rooms_x - 1 in
  let exit_ry = rooms_y - 1 in
  let exit_x = 2 * exit_rx + 1 in
  let exit_y = 2 * exit_ry + 1 in
  
  (* Mark start and exit *)
  mark_start_exit maze (start_x, start_y) (exit_x, exit_y);
  
  (maze, start_x, start_y, exit_x, exit_y, Wilson)

(* Generate a maze using False Door algorithm *)
(* False Door: generate a standard maze, then hang false doors off it. A false
   door is an alcove one cell deep, carved sideways off a corridor cell. The
   corridor cell stays a corridor; the win choice is that the opened alcove cell
   has no room adjacent to it (every room-centred neighbour is still a wall), so
   no second route opens and the maze stays a tree. The player sees a side
   opening that is a dead end, which is what the menu promises. *)
let generate_maze_false_door () = 
  (* First generate a base maze using Backtracker *)
  let (maze, start_x, start_y, exit_x, exit_y, _) = generate_maze_backtracker () in
  
  (* Number of rooms in each direction (20x20 rooms in a 41x41 grid) *)
  let rooms_x = (!width - 1) / 2 in
  let rooms_y = (!height - 1) / 2 in
  
  (* The carved lattice is the block of cells at odd coordinates: cell (i, j) is
     grid (2i+1, 2j+1), and no other cell is ever carved. A false door is an
     alcove at an even-even position hanging off a passage, which is the cell
     diagonally beside it: (2i+2, 2j+1) is a horizontal passage and (2i+2, 2j+2)
     sits below-right of it. Even-even cells are never passages, so the alcove
     touches nothing but that passage: a dead end that adds no route. Border
     cells and the start/exit are excluded. *)
  let is_open x y =
    x >= 0 && y >= 0 && x < !width && y < !height
    && (match maze.(y).(x) with Wall -> false | _ -> true)
  in
  (* How many of the four neighbours are open. *)
  let open_neighbour_count x y =
    List.fold_left
      (fun acc (nx, ny) -> acc + Bool.to_int (is_open nx ny))
      0
      [ (x + 1, y); (x - 1, y); (x, y + 1); (x, y - 1) ]
  in
  (* Try to open a stub at (x, y): open it and keep it only when the cell ends
     up a dead end with exactly one open neighbour. *)
  let try_alcove x y =
    let eligible =
      x >= 1 && y >= 1 && x < !width - 1 && y < !height - 1
      && maze.(y).(x) = Wall
      && not (x = exit_x && y = exit_y)
      && not (x = start_x && y = start_y)
    in
    if eligible then begin
      maze.(y).(x) <- Path;
      let stub = open_neighbour_count x y = 1 in
      let reopen = Wall in
      let current = if stub then Path else reopen in
      maze.(y).(x) <- current
    end
  in
  for ry = 0 to rooms_y do
    for rx = 0 to rooms_x do
      let px = (2 * rx) + 1 and py = (2 * ry) + 1 in
      let candidates =
        [ (px + 1, py + 1); (px + 1, py - 1); (px - 1, py + 1); (px - 1, py - 1) ]
      in
      let coin = Random.bool () in
      if coin && is_open px py then begin
        (* Corner cells around a lattice cell are all off-lattice. Try each in
           turn until one opens as a stub. *)
        let rec try_all = function
          | [] -> ()
          | (ax, ay) :: rest ->
              try_alcove ax ay;
              if maze.(ay).(ax) = Wall then try_all rest
        in
        try_all candidates
      end
    done
  done;
  
  (maze, start_x, start_y, exit_x, exit_y, FalseDoor)

(* Generate a maze using Blind Swarm algorithm *)
(* Blind Swarm uses multiple simultaneous random walkers:
   1. Start with a random cell as visited and active
   2. Each iteration, each active walker takes a random step
   3. If step lands on unvisited cell: carve connection, mark visited and active
   4. If step lands on visited active cell: merge the walkers
   5. If step lands on visited inactive cell: walker stops
   6. Repeat until all cells are visited *)
let generate_maze_blind_swarm () = 
  let maze = init_maze () in
  
  (* Number of rooms in each direction (20x20 rooms in a 41x41 grid) *)
  let rooms_x = (!width - 1) / 2 in
  let rooms_y = (!height - 1) / 2 in
  
  (* Mark all room cells as Path *)
  for ry = 0 to rooms_y - 1 do
    for rx = 0 to rooms_x - 1 do
      let y = 2 * ry + 1 in
      let x = 2 * rx + 1 in
      maze.(y).(x) <- Path
    done
  done;
  
  (* Track visited rooms *)
  let visited = Array.init rooms_y (fun _ -> Array.init rooms_x (fun _ -> false)) in
  
  (* Track active walkers (cells that are currently being explored from) *)
  let active = ref [] in
  
  (* Pick start from top-left half of the grid *)
  let start_rx, start_ry = pick_start rooms_x rooms_y in
  visited.(start_ry).(start_rx) <- true;
  active := [(start_rx, start_ry)];
  
  (* Total rooms *)
  let total_rooms = rooms_x * rooms_y in
  let visited_count = ref 1 in
  
  (* Blind Swarm main loop. A walker carves only after the step is accepted:
     into an unvisited room (which becomes visited and continues), or into a
     room that is still active (the walkers merge). A step into a room whose
     walker has stopped ends that walker without carving, so a dead end in the
     swarm never becomes an unreachable room. *)
  while !visited_count < total_rooms && !active <> [] do
    (* For each active walker, take a step *)
    let new_active = ref [] in
    
    List.iter (fun (cx, cy) ->
      (* Get neighbors *)
      let directions = [
        (cx, cy - 1, 0, -1); (* up *)
        (cx, cy + 1, 0, 1);  (* down *)
        (cx - 1, cy, -1, 0); (* left *)
        (cx + 1, cy, 1, 0)   (* right *)
      ] in
      
      (* Filter to valid room coordinates *)
      let valid_neighbors = 
        List.filter (fun (rx, ry, _, _) ->
          rx >= 0 && rx < rooms_x && ry >= 0 && ry < rooms_y
        ) directions
      in
      
      if valid_neighbors <> [] then begin
        (* Pick random neighbor *)
        let nidx = Random.int (List.length valid_neighbors) in
        let (nrx, nry, drx, dry) = List.nth valid_neighbors nidx in
        
        let wx = 2 * cx + 1 + drx in
        let wy = 2 * cy + 1 + dry in
        
        if visited.(nry).(nrx) then begin
          (* A live neighbour merges the walkers and both continue; a neighbour
             whose walker has stopped ends this walker without carving, so no
             room is sealed behind an unused wall. *)
          if List.mem (nrx, nry) !active then begin
            maze.(wy).(wx) <- Path;
            new_active := (cx, cy) :: !new_active
          end
        end else begin
          (* Fresh room: carve, then continue the walk from it *)
          maze.(wy).(wx) <- Path;
          visited.(nry).(nrx) <- true;
          visited_count := !visited_count + 1;
          new_active := (nrx, nry) :: !new_active
        end
      end
    ) !active;
    
    (* If every walker stopped and rooms remain, hunt: take the row-major
       first unvisited room with a visited neighbour, join the two, and
       restart the swarm from it. This always makes progress. *)
    if !new_active = [] && !visited_count < total_rooms then begin
      let found = ref false in
      for ry = 0 to rooms_y - 1 do
        for rx = 0 to rooms_x - 1 do
          if (not !found) && not visited.(ry).(rx) then begin
            let directions = [
              (rx - 1, ry, -1, 0); (* left *)
              (rx + 1, ry, 1, 0);  (* right *)
              (rx, ry - 1, 0, -1); (* up *)
              (rx, ry + 1, 0, 1)   (* down *)
            ] in
            let visited_neighbors = 
              List.filter (fun (nrx, nry, _, _) ->
                nrx >= 0 && nrx < rooms_x && nry >= 0 && nry < rooms_y && visited.(nry).(nrx)
              ) directions
            in
            if visited_neighbors <> [] then begin
              let nidx = Random.int (List.length visited_neighbors) in
              let (_ , _, drx, dry) = List.nth visited_neighbors nidx in
              
              (* Carve wall between them *)
              let wx = 2 * rx + 1 + drx in
              let wy = 2 * ry + 1 + dry in
              maze.(wy).(wx) <- Path;
              
              visited.(ry).(rx) <- true;
              visited_count := !visited_count + 1;
              new_active := (rx, ry) :: !new_active;
              found := true
            end
          end
        done
      done
    end;
    
    active := !new_active
  done;
  
  (* Start at grid position of the start room *)
  let start_x = 2 * start_rx + 1 in
  let start_y = 2 * start_ry + 1 in
  
  (* Exit at bottom-right room *)
  let exit_rx = rooms_x - 1 in
  let exit_ry = rooms_y - 1 in
  let exit_x = 2 * exit_rx + 1 in
  let exit_y = 2 * exit_ry + 1 in
  
  (* Mark start and exit *)
  mark_start_exit maze (start_x, start_y) (exit_x, exit_y);
  
  (maze, start_x, start_y, exit_x, exit_y, BlindSwarm)

(* Generate a maze using Randomised DFS algorithm *)
(* DFS uses a stack (LIFO) with random neighbor ordering:
   1. Start with a cell, mark as visited
   2. Push its neighbors onto stack in random order
   3. Pop the newest cell, carve to one unvisited neighbor
   4. Repeat until stack is empty *)
let generate_maze_random_dfs () = 
  let maze = init_maze () in
  
  (* Number of rooms in each direction (20x20 rooms in a 41x41 grid) *)
  let rooms_x = (!width - 1) / 2 in
  let rooms_y = (!height - 1) / 2 in
  
  (* Mark all room cells as Path *)
  for ry = 0 to rooms_y - 1 do
    for rx = 0 to rooms_x - 1 do
      let y = 2 * ry + 1 in
      let x = 2 * rx + 1 in
      maze.(y).(x) <- Path
    done
  done;
  
  (* Track visited rooms *)
  let visited = Array.init rooms_y (fun _ -> Array.init rooms_x (fun _ -> false)) in
  
  (* Pick start from top-left half of the grid *)
  let start_rx, start_ry = pick_start rooms_x rooms_y in
  visited.(start_ry).(start_rx) <- true;
  
  (* DFS stack: list of (rx, ry) cells to process - newest at head *)
  let stack = ref [(start_rx, start_ry)] in
  
  while !stack <> [] do
    (* Pop the newest cell (head of list) *)
    let (cx, cy) = List.hd !stack in
    stack := List.tl !stack;
    
    (* Get unvisited neighbors *)
    let directions = [
      (cx - 1, cy, -1, 0); (* left *)
      (cx + 1, cy, 1, 0);  (* right *)
      (cx, cy - 1, 0, -1); (* up *)
      (cx, cy + 1, 0, 1)   (* down *)
    ] in
    
    (* Filter to valid unvisited neighbors *)
    let unvisited_neighbors = 
      List.filter (fun (nrx, nry, _, _) ->
        nrx >= 0 && nrx < rooms_x && nry >= 0 && nry < rooms_y && not visited.(nry).(nrx)
      ) directions
    in
    
    if unvisited_neighbors <> [] then begin
      (* Shuffle the neighbors for randomness *)
      let shuffled = 
        let arr = Array.of_list unvisited_neighbors in
        for i = 0 to Array.length arr - 1 do
          let j = Random.int (Array.length arr - i) + i in
          let tmp = arr.(i) in
          arr.(i) <- arr.(j);
          arr.(j) <- tmp
        done;
        Array.to_list arr
      in
      
      (* Carve to the first shuffled neighbor and push onto stack *)
      let (nrx, nry, drx, dry) = List.hd shuffled in
      
      (* Carve wall between current and neighbor *)
      let wx = 2 * cx + 1 + drx in
      let wy = 2 * cy + 1 + dry in
      maze.(wy).(wx) <- Path;
      
      (* Mark as visited *)
      visited.(nry).(nrx) <- true;
      
      (* Push the neighbor onto stack *)
      stack := (nrx, nry) :: !stack;
      
      (* Push remaining neighbors onto stack (in shuffled order) *)
      List.iter (fun (nrx2, nry2, drx2, dry2) ->
        if not (nrx2 = nrx && nry2 = nry) then begin
          visited.(nry2).(nrx2) <- true;
          let wx2 = 2 * cx + 1 + drx2 in
          let wy2 = 2 * cy + 1 + dry2 in
          maze.(wy2).(wx2) <- Path;
          stack := (nrx2, nry2) :: !stack
        end
      ) (List.tl shuffled)
    end
  done;
  
  (* Start at grid position of the start room *)
  let start_x = 2 * start_rx + 1 in
  let start_y = 2 * start_ry + 1 in
  
  (* Exit at bottom-right room *)
  let exit_rx = rooms_x - 1 in
  let exit_ry = rooms_y - 1 in
  let exit_x = 2 * exit_rx + 1 in
  let exit_y = 2 * exit_ry + 1 in
  
  (* Mark start and exit *)
  mark_start_exit maze (start_x, start_y) (exit_x, exit_y);
  
  (maze, start_x, start_y, exit_x, exit_y, RandomDFS)

(* Generate a maze using Recursive Division algorithm *)
(* Recursive Division works top-down:
   1. Start with the full grid as a single region
   2. Divide the region with a wall (horizontal or vertical)
   3. Carve a random passage through the wall
   4. Recurse on each sub-region
   This creates mazes with long straight passages and grid-like structure *)
let generate_maze_recursive_division () = 
  let maze = init_maze () in
  
  (* Number of rooms in each direction (20x20 rooms in a 41x41 grid) *)
  let rooms_x = (!width - 1) / 2 in
  let rooms_y = (!height - 1) / 2 in
  
  (* Mark all room cells as Path *)
  for ry = 0 to rooms_y - 1 do
    for rx = 0 to rooms_x - 1 do
      let y = 2 * ry + 1 in
      let x = 2 * rx + 1 in
      maze.(y).(x) <- Path
    done
  done;
  
  (* Iterative Recursive Division using a stack *)
  (* Rooms are at odd grid coordinates: room (rx, ry) -> grid (2*rx+1, 2*ry+1) *)
  (* Walls between rooms are at even grid coordinates *)
  
  (* Stack of regions to divide, each is (min_rx, min_ry, max_rx, max_ry) *)
  let regions = Stack.create () in
  Stack.push (0, 0, rooms_x - 1, rooms_y - 1) regions;
  
  while not (Stack.is_empty regions) do
    let (min_rx, min_ry, max_rx, max_ry) = Stack.pop regions in
    
    (* Calculate region dimensions *)
    let region_width = max_rx - min_rx + 1 in
    let region_height = max_ry - min_ry + 1 in
    
    (* Skip if region is too small to divide *)
    if region_width < 2 && region_height < 2 then () else begin
      (* Decide whether to divide horizontally or vertically *)
      (* Prefer to divide along the longer dimension *)
      if region_width >= region_height && region_width >= 2 then begin
        (* Divide vertically - pick a column to divide at *)
        let split_positions = max_rx - min_rx - 1 in
        let div_col = 
          if split_positions <= 0 then min_rx + 1
          else min_rx + 1 + Random.int split_positions
        in
        
        (* Carve a wall along the dividing line (grid x = 2*div_col) *)
        (* Pick a random row to leave as passage *)
        let passage_row = min_ry + Random.int (max_ry - min_ry + 1) in
        
        (* Carve walls at all rows *)
        for ry = min_ry to max_ry do
          let wall_x = 2 * div_col in
          let y = 2 * ry + 1 in
          maze.(y).(wall_x) <- Wall
        done;
        (* Carve the passage *)
        let wall_x = 2 * div_col in
        let y = 2 * passage_row + 1 in
        maze.(y).(wall_x) <- Path;
        
        (* Push sub-regions onto stack - only if they're valid *)
        if div_col <= max_rx then
          Stack.push (div_col, min_ry, max_rx, max_ry) regions;
        if min_rx <= div_col - 1 then
          Stack.push (min_rx, min_ry, div_col - 1, max_ry) regions
      end else if region_height >= 2 then begin
        (* Divide horizontally - pick a row to divide at *)
        let split_positions = max_ry - min_ry - 1 in
        let div_row = 
          if split_positions <= 0 then min_ry + 1
          else min_ry + 1 + Random.int split_positions
        in
        
        (* Pick a random column to leave as passage *)
        let passage_col = min_rx + Random.int (max_rx - min_rx + 1) in
        
        (* Carve walls at all columns *)
        for rx = min_rx to max_rx do
          let wall_y = 2 * div_row in
          let x = 2 * rx + 1 in
          maze.(wall_y).(x) <- Wall
        done;
        (* Carve the passage *)
        let wall_y = 2 * div_row in
        let x = 2 * passage_col + 1 in
        maze.(wall_y).(x) <- Path;
        
        (* Push sub-regions onto stack - only if they're valid *)
        if div_row <= max_ry then
          Stack.push (min_rx, div_row, max_rx, max_ry) regions;
        if min_ry <= div_row - 1 then
          Stack.push (min_rx, min_ry, max_rx, div_row - 1) regions
      end
    end
  done;
  
  (* Pick start from top-left half of the grid *)
  let start_rx, start_ry = pick_start rooms_x rooms_y in
  let start_x = 2 * start_rx + 1 in
  let start_y = 2 * start_ry + 1 in
  
  (* Exit at bottom-right room *)
  let exit_rx = rooms_x - 1 in
  let exit_ry = rooms_y - 1 in
  let exit_x = 2 * exit_rx + 1 in
  let exit_y = 2 * exit_ry + 1 in
  
  (* Mark start and exit *)
  mark_start_exit maze (start_x, start_y) (exit_x, exit_y);
  
  (maze, start_x, start_y, exit_x, exit_y, RecursiveDivision)

(* Generate a maze using the selected algorithm *)
let generate_maze = function
  | Backtracker -> generate_maze_backtracker ()
  | Prims -> generate_maze_prims ()
  | GrowingTree -> generate_maze_growing_tree ()
  | HuntAndKill -> generate_maze_hunt_and_kill ()
  | Eller -> generate_maze_eller ()
  | Wilson -> generate_maze_wilson ()
  | FalseDoor -> generate_maze_false_door ()
  | BlindSwarm -> generate_maze_blind_swarm ()
  | RandomDFS -> generate_maze_random_dfs ()
  | RecursiveDivision -> generate_maze_recursive_division ()

(* Create initial game state with specified algorithm *)
let init_game alg = 
  Random.self_init ();
  let (maze, start_x, start_y, exit_x, exit_y, _returned_alg) = generate_maze alg in
  {
    maze = maze;
    player_x = start_x;
    player_y = start_y;
    exit_x = exit_x;
    exit_y = exit_y;
    game_won = false;
    algorithm = alg;
  }

(* Check if a cell is a wall. The maze carried by the state defines the bounds;
   an off-grid coordinate is not a wall, it is simply outside the board. *)
let is_wall state x y = 
  x >= 0 && y >= 0 && x < Array.length state.maze.(0) && y < Array.length state.maze
  && (match state.maze.(y).(x) with
     | Wall -> true
     | _ -> false)

(* Movement bounds come from the state's own maze, never from the global
   dimensions, so a state can never be walked off its own board. *)
let state_in_bounds state x y =
  x >= 0 && y >= 0 && x < Array.length state.maze.(0) && y < Array.length state.maze

(* Move player in a direction *)
let move_player state dir = 
  if state.game_won then state
  else
    let new_x = 
      match dir with
      | Up -> state.player_x
      | Down -> state.player_x
      | Left -> state.player_x - 1
      | Right -> state.player_x + 1
    in
    let new_y = 
      match dir with
      | Up -> state.player_y - 1
      | Down -> state.player_y + 1
      | Left -> state.player_y
      | Right -> state.player_y
    in
    
    if state_in_bounds state new_x new_y && not (is_wall state new_x new_y) then
      let game_won = new_x = state.exit_x && new_y = state.exit_y in
      {
        state with
        player_x = new_x;
        player_y = new_y;
        game_won = game_won;
      }
    else
      state
