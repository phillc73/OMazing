(** Copyright 2026, Phill Clarke *)
(** SPDX-License-Identifier: AGPL-3.0-or-later *)

(** OMazing Labyrinth - Test Suite Entry Point *)

let () =
  Alcotest.run "OMazing Labyrinth"
    (Test_maze.maze_tests @
     Test_structure.structure_tests @
     Test_qcheck.property_tests @
     Test_game.bounds_tests @
     Test_game.wall_tests @
     Test_game.state_tests @
     Test_game.win_tests @
     Test_game.pathfinding_tests @
     Test_window_position.tests)
