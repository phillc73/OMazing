(** Copyright 2026, Phill Clarke *)
(** SPDX-License-Identifier: AGPL-3.0-or-later *)

(** Maze generation and game state. This module has no Graphics dependency;
    the front end in Labyrinth_ui renders what it computes. *)

type difficulty = Easy | Medium | Hard

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

type menu_result = Algorithm of maze_algorithm | Back | Quit

(** Difficulty menu outcome. Quit must reach main: the handler's "no key"
    value is a different thing and the printed prompt promises this one. *)
type difficulty_result = Difficulty of difficulty | Difficulty_quit

type direction = Up | Down | Left | Right

type cell = Wall | Path | Start | Exit

type game_state = {
  maze : cell array array;
  player_x : int;
  player_y : int;
  exit_x : int;
  exit_y : int;
  game_won : bool;
  algorithm : maze_algorithm;
}

val menu_width : int
val menu_height : int
val cell_size : int
val max_screen_width : int
val max_screen_height : int
val calc_window_dimensions : int -> int -> int * int

(** Current generation dimensions. Always odd and at least 3 on each axis. *)
val width : int ref

val height : int ref

(** [set_dimensions_exn w h] sets the generation dimensions. Raises
    [Invalid_argument] unless both are odd and at least 3. *)
val set_dimensions_exn : int -> int -> unit

val set_dimensions : difficulty -> unit
val algorithm_name : maze_algorithm -> string
val algorithm_description : maze_algorithm -> string list
val generate_maze : maze_algorithm -> cell array array * int * int * int * int * maze_algorithm

(** Individual generators, same result tuple as {!generate_maze}. *)
val generate_maze_backtracker : unit -> cell array array * int * int * int * int * maze_algorithm

val init_game : maze_algorithm -> game_state
val in_bounds : int -> int -> bool
val is_wall : game_state -> int -> int -> bool
val move_player : game_state -> direction -> game_state
