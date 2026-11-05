(** Copyright 2026, Phill Clarke *)
(** SPDX-License-Identifier: AGPL-3.0-or-later *)

(* Graphics front end: rendering, menus and the game loop. All maze generation
   and game-state logic lives in Maze_core, which has no Graphics dependency.
   Zanuda suppression: the Graphics module is mutable by design; the menus and
   the redraw path drive it through top-level calls. *)
open Maze_core

(* Window placement: the config file may carry `window-position = +X+Y`, which
   Graphics passes straight to X11 as part of the open_graph geometry string.
   The key is off unless present - without it the window opens where the server
   puts it (0,0), which is the behaviour every earlier build had. Placement is
   manual: the app cannot see which screen the launching command came from
   through Graphics, so it cannot choose a position on its own.

   The config file is $OMAZING_CONFIG, else $XDG_CONFIG_HOME/omazing/config,
   else ~/.config/omazing/config, so test runs can point at their own file.

   The value is validated by Window_position: an accepted value places the
   window, a rejected one is dropped with a warning on stderr and the window
   opens at the default position, exactly as if the key were absent. The
   rejection matters: an unvalidated value that X11 wraps or that lands
   wholly off-screen never gets its first Expose, and Graphics.open_graph
   then blocks forever with no window and no error. *)
let window_position = ref ""

(* A rejected value, from either check, is reported once on stderr and never
   reaches the geometry string: the absent-key state is what the window gets.
   The syntax/range contract is checked when the file is read; the on-screen
   check needs the screen and runs in open_window. *)
module Placement = struct
  let reject value =
    Printf.eprintf
      "omazing: warning: window-position %S rejected (%s); using the default \
       position\n"
      value
      (Printf.sprintf "+X+Y or -X-Y with |x|, |y| <= %d"
         Window_position.limit);
    (* The window opens next and the app then sits in a blocking event wait,
       so the line must not stay in stderr's buffer. *)
    flush stderr
end

let config_path () =
  match Sys.getenv_opt "OMAZING_CONFIG" with
  | Some path -> Some path
  | None ->
      let home = try Some (Sys.getenv "HOME") with Not_found -> None in
      match Sys.getenv_opt "XDG_CONFIG_HOME" with
      | Some dir -> Some (Filename.concat (Filename.concat dir "omazing") "config")
      | None ->
          Option.map (fun h -> Filename.concat (Filename.concat (Filename.concat h ".config") "omazing") "config") home

(* A syntax-rejected value is reported once, on stderr, and leaves the
   placement unset: the same state an absent key produces. A value that passes
   the syntax but lands off this screen is caught at open_window, where the
   screen is known. *)
let apply_window_position value =
  match Window_position.placement value with
  | Some _ -> window_position := value
  | None ->
      window_position := "";
      if value <> "" then Placement.reject value

let read_config file =
  try
    let ic = open_in file in
    Fun.protect
      ~finally:(fun () -> close_in_noerr ic)
      (fun () ->
        let rec loop () =
          match input_line ic with
          | line ->
              let line =
                match String.index_opt line '#' with
                | Some i -> String.sub line 0 i
                | None -> line
              in
              (match String.index_opt line '=' with
               | Some i ->
                   let key = String.trim (String.sub line 0 i) in
                   let value = String.trim (String.sub line (i + 1) (String.length line - i - 1)) in
                   if key = "window-position" then
                     apply_window_position value;
                   loop ()
               | None -> loop ())
          | exception End_of_file -> ()
        in
        loop ())
  with Sys_error _ -> ()

let load_config () =
  window_position := "";
  match config_path () with
  | Some file -> if Sys.file_exists file then read_config file
  | None -> ()

(* The geometry string every open_graph call uses. *)
let window_geometry w h = Window_position.geometry w h !window_position

(* Open the window. A placement that passes the syntax contract but puts the
   window wholly outside the documented screen is dropped here, before the
   geometry string reaches Graphics, and the window opens at the default
   position. *)
let open_window w h =
  let screen_w, screen_h = Window_position.assumed_screen in
  let placed =
    Window_position.on_screen !window_position ~screen_w ~screen_h ~win_w:w ~win_h:h
  in
  if (not placed) && !window_position <> "" then Placement.reject !window_position;
  Graphics.open_graph (window_geometry w h);
  Graphics.auto_synchronize false

(* Draw the game *)
(* Note: Graphics module has (0,0) at lower left, but our maze array 
   has (0,0) at upper left. So we need to flip the Y coordinate. *)
(* Draw the game *)
(* Note: Graphics module has (0,0) at lower left, but our maze array 
   has (0,0) at upper left. So we need to flip the Y coordinate. *)
let draw_game state = 
  Graphics.clear_graph ();
  
  (* Calculate scaling to fit maze in window *)
  let maze_width_px = !width * cell_size in
  let maze_height_px = !height * cell_size in
  let window_w = Graphics.size_x () in
  let window_h = Graphics.size_y () in
  let scale = min (float_of_int window_w /. float_of_int maze_width_px) 
                     (float_of_int window_h /. float_of_int maze_height_px) in
  let scale = if scale < 1.0 then scale else 1.0 in  (* Only scale down, not up *)
  let scaled_cell_size = int_of_float (float_of_int cell_size *. scale) in
  let scaled_cell_size = max 1 scaled_cell_size in  (* Ensure at least 1 pixel *)
  let offset_x = (window_w - !width * scaled_cell_size) / 2 in
  let offset_y = (window_h - !height * scaled_cell_size) / 2 in
  
  (* Draw maze with scaling *)
  for y = 0 to !height - 1 do
    for x = 0 to !width - 1 do
      let screen_y = (!height - 1 - y) * scaled_cell_size + offset_y in
      match state.maze.(y).(x) with
      | Wall -> 
          Graphics.set_color Graphics.black;
          Graphics.fill_rect (x * scaled_cell_size + offset_x) screen_y scaled_cell_size scaled_cell_size
      | Path -> 
          Graphics.set_color Graphics.white;
          Graphics.fill_rect (x * scaled_cell_size + offset_x) screen_y scaled_cell_size scaled_cell_size
      | Start -> 
          Graphics.set_color Graphics.green;
          Graphics.fill_rect (x * scaled_cell_size + offset_x) screen_y scaled_cell_size scaled_cell_size
      | Exit -> 
          Graphics.set_color Graphics.red;
          Graphics.fill_rect (x * scaled_cell_size + offset_x) screen_y scaled_cell_size scaled_cell_size
    done
  done;
  
  (* Draw player with scaling - ensure minimum visible radius *)
  let player_screen_y = (!height - 1 - state.player_y) * scaled_cell_size + scaled_cell_size / 2 + offset_y in
  Graphics.set_color Graphics.blue;
  let player_radius = max 3 (scaled_cell_size / 2 - 5) in
  Graphics.fill_circle (state.player_x * scaled_cell_size + scaled_cell_size / 2 + offset_x) 
                     player_screen_y 
                     player_radius;
  
  (* Draw win modal with scaling *)
  if state.game_won then begin
    (* Modal background - white rectangle centered in maze *)
    Graphics.set_color Graphics.white;
    let modal_width = 520 in
    let modal_height = 120 in
    let modal_x = offset_x + (!width * scaled_cell_size - modal_width) / 2 in
    let modal_y = offset_y + (!height * scaled_cell_size - modal_height) / 2 in
    Graphics.fill_rect modal_x modal_y modal_width modal_height;
    
    (* Modal border *)
    Graphics.set_color Graphics.black;
    Graphics.draw_rect modal_x modal_y modal_width modal_height;
    
    (* Congratulations text - 4 lines with equal padding top and bottom *)
    Graphics.set_color Graphics.black;
    let line1_y = modal_y + 80 in
    let line2_y = modal_y + 60 in
    let line3_y = modal_y + 40 in
    let line4_y = modal_y + 20 in
    Graphics.moveto (modal_x + 10) line1_y;
    Graphics.draw_string "Congratulations you escaped the OMazing Labyrinth!";
    Graphics.moveto (modal_x + 10) line2_y;
    Graphics.draw_string "Press R to load a different maze.";
    Graphics.moveto (modal_x + 10) line3_y;
    Graphics.draw_string "Press M to return to the menu.";
    Graphics.moveto (modal_x + 10) line4_y;
    Graphics.draw_string "Press Q to quit.";
  end;
  
  Graphics.synchronize ()

(* Menu constants *)
let bg_color = Graphics.rgb 240 240 240  (* Light grey *)
let table_x = 30
let table_y_start = menu_height * cell_size - 80

(* Difficulty selection menu *)
let draw_difficulty_menu () = 
  Graphics.clear_graph ();
  (* Fill with light grey background *)
  Graphics.set_color bg_color;
  Graphics.fill_rect 0 0 (menu_width * cell_size) (menu_height * cell_size);
  
  (* Title - Graphics: y=0 is bottom, y=height is top *)
  Graphics.set_color Graphics.black;
  Graphics.set_font "-*-fixed-*-r-normal--20-*-*-*-*-*-*-*";
  Graphics.moveto (menu_width * cell_size / 2 - 170) (menu_height * cell_size - 40);
  Graphics.draw_string "OMazing Labyrinth - Select Difficulty";
  
  (* Instructions *)
  Graphics.set_font "-*-fixed-*-r-normal--14-*-*-*-*-*-*-*";
  Graphics.moveto (menu_width * cell_size / 2 - 150) (menu_height * cell_size - 80);
  Graphics.draw_string "Press 1-3 to select difficulty, Q to quit";
  
  (* Difficulty options *)
  Graphics.moveto (menu_width * cell_size / 2 - 100) (menu_height * cell_size - 120);
  Graphics.draw_string "1. Easy (21x21)";
  Graphics.moveto (menu_width * cell_size / 2 - 100) (menu_height * cell_size - 160);
  Graphics.draw_string "2. Medium (41x41)";
  Graphics.moveto (menu_width * cell_size / 2 - 100) (menu_height * cell_size - 200);
  Graphics.draw_string "3. Hard (81x81)";
  
  (* Quit option *)
  Graphics.moveto (menu_width * cell_size / 2 - 100) (menu_height * cell_size - 240);
  Graphics.draw_string "Q. Quit";
  
  Graphics.synchronize ()

(* Handle difficulty menu input - returns difficulty-or-quit, so 'Q' reaches
   main instead of reading as no key *)
let handle_difficulty_input () = 
  if Graphics.key_pressed () then
    let key = Graphics.read_key () in
    match key with
    | '1' -> Some (Difficulty Easy)
    | '2' -> Some (Difficulty Medium)
    | '3' -> Some (Difficulty Hard)
    | 'q' | 'Q' -> Some Difficulty_quit
    | _ -> None
  else
    None

(* Menu constants continuation *)
let col1_width = 280  (* Algorithm name column *)
let col2_x = table_x + col1_width + 20  (* 20px gap between columns *)
let desc_line_spacing = 14
let cell_padding = 6
let header_height = 25

(* Algorithm data for menu with their description line counts *)
let menu_algorithms = [
  (Backtracker, '1');
  (Prims, '2');
  (GrowingTree, '3');
  (HuntAndKill, '4');
  (Eller, '5');
  (Wilson, '6');
  (FalseDoor, '7');
  (BlindSwarm, '8');
  (RandomDFS, '9');
  (RecursiveDivision, '0');
]

(* Calculate row !height based on number of description lines *)
let row_height alg = 
  let num_lines = List.length (algorithm_description alg) in
  (* Base !height for algorithm name + padding *)
  let base = 18 in
  (* Add space for each description line *)
  base + num_lines * desc_line_spacing + cell_padding

(* Draw the algorithm selection menu as a two-column table. Rows are laid out
   top-down from just under the instructions and every row is clipped to the
   window, so the table cannot run off the top edge. *)
let draw_menu () = 
  Graphics.clear_graph ();
  (* Fill with light grey background *)
  Graphics.set_color bg_color;
  Graphics.fill_rect 0 0 (menu_width * cell_size) (menu_height * cell_size);
  
  (* Title - Graphics: y=0 is bottom, y=height is top *)
  Graphics.set_color Graphics.black;
  Graphics.set_font "-*-fixed-*-r-normal--20-*-*-*-*-*-*-*";
  Graphics.moveto (menu_width * cell_size / 2 - 180) (menu_height * cell_size - 30);
  Graphics.draw_string "OMazing Labyrinth - Maze Algorithm Selector";
  
  (* Instructions *)
  Graphics.set_font "-*-fixed-*-r-normal--14-*-*-*-*-*-*-*";
  Graphics.moveto (menu_width * cell_size / 2 - 180) (menu_height * cell_size - 60);
  Graphics.draw_string "Press 0-9 to select, M to go back, Q to quit";
  
  (* Draw table *)
  Graphics.set_font "-*-fixed-*-r-normal--14-*-*-*-*-*-*-*";
  
  (* Column headers sit just under the instructions *)
  let top = menu_height * cell_size - 90 in
  let in_window y = y >= 10 && y <= menu_height * cell_size - 10 in
  
  Graphics.moveto table_x top;
  Graphics.draw_string "Algorithm";
  Graphics.moveto col2_x top;
  Graphics.draw_string "Description";
  
  (* Draw each algorithm as a table row top-down *)
  let rec draw_rows y = function
    | [] -> ()
    | (alg, key) :: rest ->
        let h = row_height alg in
        let row_y = y - 24 in
        if in_window row_y then begin
          (* Algorithm name in first column - black text *)
          Graphics.moveto (table_x + cell_padding) row_y;
          Graphics.draw_string (Printf.sprintf "%c. %s" key (algorithm_name alg));
          
          (* Description in second column *)
          Graphics.set_font "-*-fixed-*-r-normal--12-*-*-*-*-*-*-*";
          List.iteri (fun j line ->
            let ly = row_y - (j * desc_line_spacing) in
            if in_window ly then begin
              Graphics.moveto (col2_x + cell_padding) ly;
              Graphics.draw_string line
            end
          ) (algorithm_description alg);
          Graphics.set_font "-*-fixed-*-r-normal--14-*-*-*-*-*-*-*"
        end;
        
        (* Draw next row below *)
        draw_rows (y - 24 - h) rest
  in
  
  draw_rows top menu_algorithms;
  
  Graphics.synchronize ()

(* Handle menu input - returns menu_result *)
let handle_menu_input () = 
  if Graphics.key_pressed () then
    let key = Graphics.read_key () in
    match key with
    | '1' -> Some (Algorithm Backtracker)
    | '2' -> Some (Algorithm Prims)
    | '3' -> Some (Algorithm GrowingTree)
    | '4' -> Some (Algorithm HuntAndKill)
    | '5' -> Some (Algorithm Eller)
    | '6' -> Some (Algorithm Wilson)
    | '7' -> Some (Algorithm FalseDoor)
    | '8' -> Some (Algorithm BlindSwarm)
    | '9' -> Some (Algorithm RandomDFS)
    | '0' -> Some (Algorithm RecursiveDivision)
    | 'm' | 'M' -> Some Back
    | 'q' | 'Q' -> Some Quit
    | _ -> None
  else
    None

(* The WM discards window contents on resize and Graphics has no expose
   callback, so a resized window stays blank until the app repaints. The
   poll loops therefore redraw whenever the surface size differs from the
   size the current screen was painted for. *)
let redraw_if_resized painted_w painted_h draw_fn =
  if Graphics.size_x () <> painted_w || Graphics.size_y () <> painted_h
  then draw_fn ()

(* Park briefly between polls. Graphics has no sleep of its own and Unix is
   already linked, so an idle select does it. Without this the menu and game
   loops spin at full tilt on a keyless wait. EINTR (a signal interrupting the
   select - window-manager events, resize, X11 traffic) is caught and retried:
   an unhandled EINTR here killed the game on its first real-display run. *)
let idle () =
  try
    ignore (Unix.select [] [] [] 0.016 : Unix.file_descr list * Unix.file_descr list * Unix.file_descr list)
  with Unix.Unix_error (Unix.EINTR, "select", "") -> ()

(* Wait for algorithm selection - returns menu_result *)
let select_algorithm () = 
  draw_menu ();
  let rec wait_for_selection () = 
    match handle_menu_input () with
    | Some result -> result
    | None -> 
        idle ();
        wait_for_selection ()
  in
  wait_for_selection ()

(* Wait for difficulty and algorithm selection. None means the user quit from
   either menu; main turns that into a normal close. *)
let select_difficulty_and_algorithm () = 
  let rec wait_for_difficulty () = 
    draw_difficulty_menu ();
    match handle_difficulty_input () with
    | Some (Difficulty diff) -> 
        set_dimensions diff;
        (match select_algorithm () with
         | Algorithm alg -> Some alg
         | Back -> wait_for_difficulty ()
         | Quit -> None)
    | Some Difficulty_quit -> None
    | None -> 
        idle ();
        wait_for_difficulty ()
  in
  wait_for_difficulty ()

(* Handle keyboard input - read one key at a time *)
let handle_input state = 
  if Graphics.key_pressed () then
    let key = Graphics.read_key () in
    match key with
    | 'q' | 'Q' -> (false, state) (* Quit *)
    | 'r' | 'R' -> (* Reset game with same algorithm *)
        let new_state = init_game state.algorithm in
        draw_game new_state;
        (true, new_state)
    | 'm' | 'M' -> (* Return to menu - restart from difficulty selection *)
        set_dimensions Medium;
        open_window (menu_width * cell_size) (menu_height * cell_size);
        (match select_difficulty_and_algorithm () with
         | None -> (false, state) (* Quit from either menu - unwind to main *)
         | Some alg ->
        (* Resize window to selected difficulty dimensions, capped at screen size *)
        let (window_w, window_h) = calc_window_dimensions !width !height in
        open_window window_w window_h;
        let new_state = init_game alg in
        Graphics.set_window_title (Printf.sprintf "OMazing Labyrinth - %s (WASD to move, R reset, M menu, Q quit)" (algorithm_name alg));
        draw_game new_state;
        (true, new_state))
    | 'w' | 'W' -> 
        let new_state = move_player state Up in
        if new_state != state then draw_game new_state;
        (true, new_state)
    | 's' | 'S' -> 
        let new_state = move_player state Down in
        if new_state != state then draw_game new_state;
        (true, new_state)
    | 'a' | 'A' -> 
        let new_state = move_player state Left in
        if new_state != state then draw_game new_state;
        (true, new_state)
    | 'd' | 'D' -> 
        let new_state = move_player state Right in
        if new_state != state then draw_game new_state;
        (true, new_state)
    | _ -> (true, state) (* Ignore other keys *)
  else
    (true, state)

(* Main game loop *)
let main () = 
  (* Open window with menu dimensions initially *)
  load_config ();
  open_window (menu_width * cell_size) (menu_height * cell_size);
  Graphics.set_window_title "OMazing Labyrinth - Escape the Maze! (WASD to move, R reset, M menu, Q quit)";
  
  (* Show difficulty and algorithm selection menus - Quit from either ends the
     program here, through close_graph like any other exit *)
  match select_difficulty_and_algorithm () with
  | None -> Graphics.close_graph ()
  | Some alg ->
  (* Resize window to selected difficulty dimensions, capped at screen size *)
  let (window_w, window_h) = calc_window_dimensions !width !height in
  open_window window_w window_h;
  let state = init_game alg in
  draw_game state;
  
  (* Show current algorithm in title briefly *)
  Graphics.set_window_title (Printf.sprintf "Labyrinth - %s (WASD to move, R reset, M menu, Q quit)" (algorithm_name alg));
  
  let rec loop state = 
    let (continue, new_state) = handle_input state in
    if continue then begin
      idle ();
      loop new_state
    end
  in
  
  loop state;
  Graphics.close_graph ()
