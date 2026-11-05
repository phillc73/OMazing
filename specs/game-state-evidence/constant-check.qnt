// OMazing game-state - constant check (P0): the model's maze constants.
// Licence: AGPL-3.0-or-later.
//
// Adjudication 1 of the dispatch: the qnt constants are verified by a
// Quint-native run script, in-toolchain, against the reviewed grid text in
// the platform grids declared in this module.

//
// Each assertion below is a ladder rung: a renamed copy of specs/game-state.qnt
// constants is NOT used - the module is imported so the checked values are
// exactly the ones the model uses.
//
// Run (explicit --match: a bare `quint test` runs ZERO tests, trap 1):
//   ~/.local/bin/quint test specs/game-state-evidence/constant-check.qnt \
//     --main=constant_check \
//     --match='(p0|c0|c1)' 2>&1 | tee specs/game-state-evidence/constant-check.txt

module constant_check {
  import game_state.* from "../game-state"

  // C0 - cardinalities from the reviewed grid.
  //   5x5 mazeId 0: S + 5 P + E = 7 open cells.
  //   5x5 mazeId 1: corridor (3,2) closed -> 6.
  //   7x7 mazeId 0: 9 rooms + 9 corridors + S + E = 20.
  //   7x7 mazeId 1: corridor (5,4) closed -> 19.
  run p0_cardinalities = all {
    assert(p5_open_m0.size() == 7),
    assert(p5_open_m1.size() == 6),
    assert(p7_open_m0.size() == 20),
    assert(p7_open_m1.size() == 19),
  }

  // C1 - dimensions: the two platforms are 5x5 and 7x7.
  run p0_dimensions = all {
    assert(extent5 == 5),
    assert(extent7 == 7),
  }

  // C2 - start and exit positions (both mazes of a platform share them).
  run p0_start_exit = all {
    assert(start5.x == 1),
    assert(start5.y == 1),
    assert(exit5.x == 3),
    assert(exit5.y == 3),
    assert(start7.x == 1),
    assert(start7.y == 1),
    assert(exit7.x == 5),
    assert(exit7.y == 5),
  }

  // C3 - corner cells sit on the border for each platform extent, and the
  // border cells are wall (not in the open set) in every maze.
  run p0_corners_are_wall = all {
    assert(not(p5_open_m0.exists(c => c.x == 0 or c.y == 0 or c.x == 4 or c.y == 4))),
    assert(not(p5_open_m1.exists(c => c.x == 0 or c.y == 0 or c.x == 4 or c.y == 4))),
    assert(not(p7_open_m0.exists(c => c.x == 0 or c.y == 0 or c.x == 6 or c.y == 6))),
    assert(not(p7_open_m1.exists(c => c.x == 0 or c.y == 0 or c.x == 6 or c.y == 6))),
  }

  // C4 - start and exit are open (I5's "non-wall" half), and distinct.
  run p0_start_exit_open = all {
    assert(p5_open_m0.exists(c => c.x == 1 and c.y == 1)),
    assert(p5_open_m0.exists(c => c.x == 3 and c.y == 3)),
    assert(p7_open_m0.exists(c => c.x == 1 and c.y == 1)),
    assert(p7_open_m0.exists(c => c.x == 5 and c.y == 5)),
    assert(start5 != exit5),
    assert(start7 != exit7),
  }

  // C5 - P3's premise, the counterfactual that catches a mistranscribed cell:
  // every mazeId-0 maze connects Start to Exit, and every mazeId-1 maze does
  // NOT (one corridor closed). If a single cell were dropped or added, one of
  // these two flips.
  run p0_path_true_on_m0 = all {
    assert(reachSet(p5_open_m0, start5)
      .exists(c => c.x == exit5.x and c.y == exit5.y)),
    assert(reachSet(p7_open_m0, start7)
      .exists(c => c.x == exit7.x and c.y == exit7.y)),
  }

  // p0_path_false_on_m1 asserts platform5 only. On the reviewed grid,
  // platform7 mazeId 1 closes just corridor (5,4), and room (5,5) still
  // reaches Start via (4,5)-(3,5)-(3,4)-(3,3) - so the reviewed file's
  // comment "cutting room (5,5) off the rest" does not hold for its own
  // grid. The model records the real grid; the file's comment is not edited. platform5's (3,2)
  // pinch does disconnect, and that is the P3-false-premise case.
  run p0_path_false_on_m1 = all {
    assert(not(reachSet(p5_open_m1, start5)
      .exists(c => c.x == exit5.x and c.y == exit5.y))),
  }

  // C6 - I1 on the model grids: every room (odd,odd) cell is open. Room
  // (rx,ry) sits at (2rx+1, 2ry+1) per docs/spec.md "Data model".
  run p0_every_room_open = all {
    assert(p5_open_m0.exists(c => c.x == 1 and c.y == 1)),
    assert(p5_open_m0.exists(c => c.x == 3 and c.y == 1)),
    assert(p5_open_m0.exists(c => c.x == 1 and c.y == 3)),
    assert(p5_open_m0.exists(c => c.x == 3 and c.y == 3)),
    assert(p7_open_m0.exists(c => c.x == 1 and c.y == 1)),
    assert(p7_open_m0.exists(c => c.x == 3 and c.y == 1)),
    assert(p7_open_m0.exists(c => c.x == 5 and c.y == 1)),
    assert(p7_open_m0.exists(c => c.x == 1 and c.y == 3)),
    assert(p7_open_m0.exists(c => c.x == 3 and c.y == 3)),
    assert(p7_open_m0.exists(c => c.x == 5 and c.y == 3)),
    assert(p7_open_m0.exists(c => c.x == 1 and c.y == 5)),
    assert(p7_open_m0.exists(c => c.x == 3 and c.y == 5)),
    assert(p7_open_m0.exists(c => c.x == 5 and c.y == 5)),
  }

  // C7 - I2 on the model grids: no (even,even) cell is open.
  run p0_no_off_lattice_open = all {
    assert(not(p5_open_m0.exists(c => c.x % 2 == 0 and c.y % 2 == 0))),
    assert(not(p5_open_m1.exists(c => c.x % 2 == 0 and c.y % 2 == 0))),
    assert(not(p7_open_m0.exists(c => c.x % 2 == 0 and c.y % 2 == 0))),
    assert(not(p7_open_m1.exists(c => c.x % 2 == 0 and c.y % 2 == 0))),
  }
}
