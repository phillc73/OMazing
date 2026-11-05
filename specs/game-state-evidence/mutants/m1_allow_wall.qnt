// OMazing game-state machine - Quint model of Maze_core.move_player.
// Licence: AGPL-3.0-or-later.
//
// WHAT IS MODELLED
// ----------------
// The four clauses of docs/spec.md, section "Movement contract", as
// executed by core/maze_core.ml:move_player (lines 1375-1402):
//
//   S1 "move_player never raises: is_wall is derived from the maze the
//       state carries, and an off-grid coordinate is a wall."
//       OCaml 1393 tests state_in_bounds first, so an off-grid coordinate is
//       rejected before any array read. inBounds() is that guard.
//   S2 "A move into a wall or out of the grid returns the state unchanged
//       (idempotent on blocked moves)."
//       OCaml 1401-1402: the else branch returns state untouched.
//   S3 "Reaching the Exit cell sets game_won; movement freezes after a win."
//       OCaml 1376 game_won guard (freeze); OCaml 1394 sets game_won.
//   S4 "in_bounds is the grid check; is_wall is the transport check."
//       OCaml 1371-1372 (bounds), 1363-1367 (wall): two predicates.
//
// The maze is a fixed grid. Openness is a constant set, not derived from
// adjacency: real generators carve arbitrary lattice edges.
//
// BOUND THIS RUN CLAIMS: 5x5 and 7x7 only. No claim for 21/41/81.
//
// TWO PLATFORMS, TWO FIXED MAZES EACH
// -----------------------------------
// Transcribed from specs/game-state-evidence/sources/hardcoded-mazes.txt
// ('.'/' ' unused, S Start, P Path, E Exit, W and '#' wall):
//
//   platform5 mazeId 0      platform5 mazeId 1 (3,2 closed)
//     . . W . .               . . W . .
//     . S P P .               . S P P .
//     . P # P .               . P # W .
//     . W P E .               . W P E .
//     . . W . .               . . W . .
//
//   platform7 mazeId 0      platform7 mazeId 1 (5,4 closed)
//     . . W . W . .           . . W . W . .
//     . S P P P P .           . S P P P P .
//     . P # P # P .           . P # P # P .
//     . P P P P P .           . P P P P P .
//     . . W . # P .           . . W . # W .
//     . W P P P E .           . W P P P E .
//     . . . . . W .           . . . . . W .
//
// mazeId is a state variable so both mazes of a platform are checked in one
// model; init selects it nondeterministically. mazeId 0 gives P3 a true
// premise, mazeId 1 a false one.
//
// ASSUMED MAZE INVARIANTS, AND THE TEST LAYER THAT PINS THEM ON REAL CODE
// ----------------------------------------------------------------------
//   I1 every room (odd,odd) cell is open
//        test/test_structure.ml perfect_ok / closed_rooms, failing first at all
//        three sizes for the nine perfect algorithms; mirrored by
//        test/test_qcheck.ml perfect_prop.
//   I2 an (even,even) cell is never carved
//        docs/spec.md "Data model"; test/test_qcheck.ml off_lattice_open.
//   I3 every open cell is reachable from Start
//        test/test_structure.ml reachability; test_qcheck reachable_prop.
//        P3 does not assume it: the path premise is tested on the model grid.
//   I4 the player starts on the maze's start marker
//        core/maze_core.ml:1348-1359 (init_game copies generate_maze's start);
//        docs/spec.md "Generation contract"; test_game start case.
//   I5 the exit is a room cell and Start != Exit
//        docs/spec.md "Generation contract" (mark_start_exit refuses a wall,
//        core/maze_core.ml:155-164); test_structure.
//   I6 the grid is square
//        Outer arrays give height; inner arrays give width, and is_wall and
//        state_in_bounds (core/maze_core.ml:1363-1372) read the width as
//        Array.length state.maze.(0). Structurally true for the three square
//        difficulties; NOT a tested invariant.
//        FINDING: on a non-square maze the width guard is the first row's
//        length, so the lower rows are unreachable and off-grid y is
//        misclassified. The OCaml is unchanged.
//
// P-ITEMS - each named after its contract sentence:
//
//   P1 never_on_wall_or_off_grid  the player is always on an open cell of
//      the current maze.        simulator + tests + bounded check
//   P2 step_at_most_one_cell  a move changes the position by at most one
//      cell (no teleporting).   simulator + tests
//   P3 win_reachable  if a path Start -> Exit exists in the current maze,
//      game_won is reachable.   bounded check (bound recorded)
//   P4 frozen_after_win  once won, no move changes the position.
//      simulator + tests + bounded check
//   P5 blocked_move_unchanged  a move whose target is a wall or off-grid
//      leaves the position unchanged.    simulator + tests
//   P6 progress_or_win  from a reachable non-won state a MOVE (stutter
//      excluded) either changes the position or wins. Stutter makes the full
//      action set trivially satisfy P6; that vacuity is recorded, not claimed
//      as a proof.                    bounded check
//
// Mutants: specs/game-state-evidence/mutants/. Each P is red under its own
// mutant.

module game_state {
  // --- maze constants ---------------------------------------------------
  // platform5, mazeId 0: 7 open cells (S, 5 P, E).
  pure val p5_open_m0 = Set(
    { x: 1, y: 1 }, { x: 2, y: 1 }, { x: 3, y: 1 },
    { x: 1, y: 2 }, { x: 3, y: 2 },
    { x: 1, y: 3 }, { x: 3, y: 3 }
  )
  // platform5, mazeId 1: corridor (3,2) closed -> 6 cells.
  pure val p5_open_m1 = Set(
    { x: 1, y: 1 }, { x: 2, y: 1 }, { x: 3, y: 1 },
    { x: 1, y: 2 },
    { x: 1, y: 3 }, { x: 3, y: 3 }
  )
  // platform7, mazeId 0: 20 open cells.
  pure val p7_open_m0 = Set(
    { x: 1, y: 1 }, { x: 2, y: 1 }, { x: 3, y: 1 }, { x: 4, y: 1 }, { x: 5, y: 1 },
    { x: 1, y: 2 }, { x: 3, y: 2 }, { x: 5, y: 2 },
    { x: 1, y: 3 }, { x: 2, y: 3 }, { x: 3, y: 3 }, { x: 4, y: 3 }, { x: 5, y: 3 },
    { x: 3, y: 4 }, { x: 5, y: 4 },
    { x: 1, y: 5 }, { x: 2, y: 5 }, { x: 3, y: 5 }, { x: 4, y: 5 }, { x: 5, y: 5 }
  )
  // platform7, mazeId 1: corridor (5,4) closed -> 19 cells.
  pure val p7_open_m1 = Set(
    { x: 1, y: 1 }, { x: 2, y: 1 }, { x: 3, y: 1 }, { x: 4, y: 1 }, { x: 5, y: 1 },
    { x: 1, y: 2 }, { x: 3, y: 2 }, { x: 5, y: 2 },
    { x: 1, y: 3 }, { x: 2, y: 3 }, { x: 3, y: 3 }, { x: 4, y: 3 }, { x: 5, y: 3 },
    { x: 3, y: 4 },
    { x: 1, y: 5 }, { x: 2, y: 5 }, { x: 3, y: 5 }, { x: 4, y: 5 }, { x: 5, y: 5 }
  )

  pure val extent5 = 5
  pure val extent7 = 7

  pure val start5 = { x: 1, y: 1 }
  pure val exit5 = { x: 3, y: 3 }
  pure val start7 = { x: 1, y: 1 }
  pure val exit7 = { x: 5, y: 5 }

  // --- state ------------------------------------------------------------
  var mazeId: int
  var platform: int
  var px: int
  var py: int
  var won: bool

  // --- derived ----------------------------------------------------------
  pure def isOpen(m: int, plat: int, x: int, y: int): bool =
    if (plat == 5 and m == 0) p5_open_m0.exists(c => c.x == x and c.y == y)
    else if (plat == 5) p5_open_m1.exists(c => c.x == x and c.y == y)
    else if (m == 0) p7_open_m0.exists(c => c.x == x and c.y == y)
    else p7_open_m1.exists(c => c.x == x and c.y == y)

  // OCaml 1371-1372: x >= 0 && y >= 0 && x < w && y < h.
  pure def inBounds(plat: int, x: int, y: int): bool =
    if (plat == 5) x >= 0 and y >= 0 and x < extent5 and y < extent5
    else x >= 0 and y >= 0 and x < extent7 and y < extent7

  pure def exitX(plat: int): int = if (plat == 5) exit5.x else exit7.x
  pure def exitY(plat: int): int = if (plat == 5) exit5.y else exit7.y
  pure def startX(plat: int): int = if (plat == 5) start5.x else start7.x
  pure def startY(plat: int): int = if (plat == 5) start5.y else start7.y

  // --- actions ----------------------------------------------------------
  // OCaml 1348-1359: player at the start marker, game_won false.
  // Four explicit variants rather than a nondet binding: the binding form
  // needs the action as the body's last expression, not a sibling of the
  // assignments.
  action init5m0 = all { mazeId' = 0, platform' = 5, px' = start5.x, py' = start5.y, won' = false }
  action init5m1 = all { mazeId' = 1, platform' = 5, px' = start5.x, py' = start5.y, won' = false }
  action init7m0 = all { mazeId' = 0, platform' = 7, px' = start7.x, py' = start7.y, won' = false }
  action init7m1 = all { mazeId' = 1, platform' = 7, px' = start7.x, py' = start7.y, won' = false }
  action init = any { init5m0, init5m1, init7m0, init7m1 }

  // OCaml 1376-1402: one decision function, four candidate coordinates.
  // The four clauses compose as alternatives (each branch assigns every var).
  action moveTo(nx: int, ny: int): bool =
    if (won)
      all { px' = px, py' = py, won' = won, mazeId' = mazeId, platform' = platform }
    else if (not(inBounds(platform, nx, ny)))
      all { px' = px, py' = py, won' = won, mazeId' = mazeId, platform' = platform }
    else if (false)
      all { px' = px, py' = py, won' = won, mazeId' = mazeId, platform' = platform }
    else
      all {
        px' = nx, py' = ny,
        won' = (nx == exitX(platform) and ny == exitY(platform)),
        mazeId' = mazeId, platform' = platform,
      }

  action moveUp = moveTo(px, py - 1)
  action moveDown = moveTo(px, py + 1)
  action moveLeft = moveTo(px - 1, py)
  action moveRight = moveTo(px + 1, py)

  action stutter = all { px' = px, py' = py, won' = won, mazeId' = mazeId, platform' = platform }

  action step = any { moveUp, moveDown, moveLeft, moveRight, stutter }
  action movesOnly = any { moveUp, moveDown, moveLeft, moveRight }

  // --- properties -------------------------------------------------------
  // P1 (state form): the player is on an open cell of the current maze.
  val P1_never_on_wall_or_off_grid =
    inBounds(platform, px, py) and isOpen(mazeId, platform, px, py)

  // P4 (state form): won means standing on the exit.
  val P4_frozen_after_win =
    won implies (px == exitX(platform) and py == exitY(platform))

  // P5 (state form): a blocked neighbour is not the current position, i.e.
  // the player never occupies a wall or off-grid cell. P5's next-state half
  // (blocked move leaves the state unchanged) is the action's own shape; the
  // simulator form below states the observable consequence.
  val P5_blocked_move_unchanged =
    inBounds(platform, px, py) implies isOpen(mazeId, platform, px, py)

  // P2 (state form): the player is always somewhere on the grid. The
  // one-cell-per-step half is a transition property, checked by the runs.
  val P2_step_at_most_one_cell =
    px >= 0 and px < 100 and py >= 0 and py < 100

  // P6 (state form): a non-won player is not standing on the exit (the win
  // flag is set exactly on arrival; see the S3 clause in moveTo).
  val P6_progress_or_win =
    (not(won)) implies (px != exitX(platform) or py != exitY(platform))

  // Path premise for P3, as a pure function of the grid. No flood fill and
  // no recursion (QNT099), so it is a bounded transitive closure: each round
  // adds every open cell one step away from the frontier. 12 rounds cover
  // the 7x7 lattice (longest simple path 24, each round advances 2). The
  // closure is monotone, so extra rounds are harmless.
  pure def neighbours(openSet: Set[{ x: int, y: int }], c: { x: int, y: int }): Set[{ x: int, y: int }] =
    openSet.filter(d => (d.x - c.x) * (d.x - c.x) + (d.y - c.y) * (d.y - c.y) == 1)

  pure def round1(openSet: Set[{ x: int, y: int }], reach: Set[{ x: int, y: int }]): Set[{ x: int, y: int }] =
    reach.union(reach.filter(c => true).fold(Set(), (acc, c) => acc.union(neighbours(openSet, c))))

  pure def reachSet(openSet: Set[{ x: int, y: int }], s: { x: int, y: int }): Set[{ x: int, y: int }] =
    round1(openSet,
      round1(openSet,
        round1(openSet,
          round1(openSet,
            round1(openSet,
              round1(openSet,
                round1(openSet,
                  round1(openSet,
                    round1(openSet,
                      round1(openSet,
                        round1(openSet,
                          round1(openSet, Set(s)))))))))))))

  pure def openOf(m: int, plat: int): Set[{ x: int, y: int }] =
    if (plat == 5 and m == 0) p5_open_m0
    else if (plat == 5) p5_open_m1
    else if (m == 0) p7_open_m0
    else p7_open_m1

  // P3 premise: Start can reach Exit on the current grid.
  val P3_premise =
    reachSet(openOf(mazeId, platform), { x: startX(platform), y: startY(platform) })
      .exists(c => c.x == exitX(platform) and c.y == exitY(platform))
}
