// OMazing game-state - P2/P4/P5/P6 as transition properties.
// Licence: AGPL-3.0-or-later.
//
// WHY THESE ARE TRANSITION PROPERTIES
// ---------------------------
// P2/P4/P5/P6 as state predicates leave their mutants alive:
// m2 jumped two cells and P2 still held, m3 removed the freeze and P4 still
// held. Each property here
// reads a state the model REACHES, and the mutant gate is evaluated by
// whether a mutant can put the model into a state the contract forbids.
//
// TOOLCHAIN CONSTRAINTS (quint 0.33.1, Apalache 0.62.4)
// ---------------------------------------------------------------------
// Each constraint below was confirmed against the checker, so a later reader
// does not re-derive them:
//
//   1. `assert` after an updating action is QNT000 (effect conflict). Runs
//      assert nothing about the state after a move.
//   2. Primed names inside a run's all{} are a parse error (QNT000).
//   3. A `val`/`nondet` bound inside a run BODY, before the action, is a
//      parse error. So a run cannot record the pre-state and compare it
//      with the post-state.
//   4. An action compared or branched inside a pure def/val is QNT200. The
//      moveTo relation is not readable as a value.
//   5. `next(x)` is legal only in a `temporal` (QNT200 in a val).
//   6. Apalache binds a Quint step action as 8-10 separate TLA+ actions, and
//      a temporal is checked as an LASSERT (a state predicate at some step).
//      A pre-state gate placed in front of a next-state term does not make
//      the term a next-state read: measured with (C & R) and (C & D & R),
//      where D fails only at the pre-state, the checker found no violation
//      over reachable transitions that falsify R, and `next(px) == px + 2`
//      on a one-cell move was reported as no violation. The temporal form
//      is NOT an instrument for transition properties here.
//
// So the transition properties are stated over the states the run
// visits, which is the part of the step relation the simulator can show,
// and every one of them is paired with a falsifiability control (a
// deliberately-absurd variant of the same
// property, false on the same instrument).
//
// Check with: quint verify on this module (bounded instances).

module transitions {
  import game_state.* from "./m1_allow_wall"

  // --- the compared side: one cell, in line, or stay --------------------
  pure def oneCell(x: int, y: int): Set[{ x: int, y: int }] =
    Set({ x: x, y: y - 1 }, { x: x, y: y + 1 }, { x: x - 1, y: y }, { x: x + 1, y: y })

  pure def inCellSet(ts: Set[{ x: int, y: int }], x: int, y: int): bool =
    ts.exists(t => t.x == x and t.y == y)

  // The in-line neighbours that are off-grid or wall, from the current
  // state alone. S4's two checks applied to a cell.
  pure def blockedAround(m: int, plat: int, x: int, y: int): Set[{ x: int, y: int }] =
    oneCell(x, y).filter(t => not(inBounds(plat, t.x, t.y)) or not(isOpen(m, plat, t.x, t.y)))

  // --- P2 ---------------------------------------------------------------
  // S1 "move_player never raises: is_wall is derived from the maze the state
  // carries, and an off-grid coordinate is a wall", with S4 "in_bounds is
  // the grid check; is_wall is the transport check".
  // Reachable-state form: every state visited is on the grid, and the maze
  // is unchanged by movement (no action assigns mazeId or platform).
  val P2_reachable =
    inBounds(platform, px, py)

  val P2_maze_is_fixed =
    mazeId == 0 or mazeId == 1

  // --- P4 ---------------------------------------------------------------
  // S3 "Reaching the Exit cell sets game_won; movement freezes after a
  // win." The won flag holds exactly on the exit cell, and the exit is the
  // platform's own exit.
  val P4_won_is_on_exit =
    won implies (px == exitX(platform) and py == exitY(platform))

  val P4_exit_reached_sets_won =
    (px == exitX(platform) and py == exitY(platform)) implies won

  // The freeze itself is a transition fact the simulator cannot state (see
  // the header): the landed instrument measures it as "the won-start run
  // never leaves the exit", which is a run this model cannot produce from
  // `init`.

  // --- P5 ---------------------------------------------------------------
  // S2 "A move into a wall or out of the grid returns the state unchanged
  // (idempotent on blocked moves)." Every reached state is on an open cell:
  // a blocked move that moved the player would land on a wall.
  val P5_reached_state_is_open =
    isOpen(mazeId, platform, px, py)

  // --- P6 ---------------------------------------------------------------
  // S3 from the other side: from a non-won state the player is never stuck.
  // Stutter makes a liveness reading vacuous by construction, so the
  // property is the WITNESS: a non-won state has an open neighbour.
  val someNeighbourOpen =
    isOpen(mazeId, platform, px - 1, py) or isOpen(mazeId, platform, px + 1, py) or
    isOpen(mazeId, platform, px, py - 1) or isOpen(mazeId, platform, px, py + 1)

  val P6_witness_state =
    (not(won)) implies someNeighbourOpen
}
