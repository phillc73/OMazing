// OMazing game-state - P3 premise checks, effect-free.
// Licence: AGPL-3.0-or-later.
//
// MEASURED LIMIT (quint 0.33.1): `assert(e)` has stateless effect `[]`, so
// `.then(assert(...))` after an updating action is a QNT000 effect conflict,
// with or without an `all{}` wrapper and regardless of the expression. Runs
// therefore cannot assert post-action state. Transition properties (P2, P4,
// P5, P6) are checked by the simulator over reachable states (invariant
// defs in game-state.qnt) and by bounded Apalache; this file holds only the
// effect-free premise facts.
//
// Explicit --match is mandatory: a bare `quint test` selects ZERO runs and
// exits 0 (quint trap 1).
//
// Run: ~/.local/bin/quint test specs/game-state-evidence/properties.qnt \
//        --main=properties --match='p3_.*'

module properties {
  import game_state.* from "../game-state"

  // P3's premise, stated on the grid model directly (no state read): does
  // Start reach Exit on that grid?
  run p3_premise_p5m0_true = all {
    assert(reachSet(p5_open_m0, start5).exists(c => c.x == exit5.x and c.y == exit5.y)),
  }

  run p3_premise_p5m1_false = all {
    assert(not(reachSet(p5_open_m1, start5).exists(c => c.x == exit5.x and c.y == exit5.y))),
  }

  run p3_premise_p7m0_true = all {
    assert(reachSet(p7_open_m0, start7).exists(c => c.x == exit7.x and c.y == exit7.y)),
  }

  // The reviewed grid's platform7 mazeId 1 closes only corridor (5,4), and
  // (5,5) still reaches Start via (4,5)-(3,5)-(3,4)-(3,3): the premise stays
  // TRUE. That contradicts the reviewed file's own comment ("cutting room
  // (5,5) off the rest"). The model keeps the reviewed grid as it is, not
  // silently "fixed".
  run p3_premise_p7m1_true = all {
    assert(reachSet(p7_open_m1, start7).exists(c => c.x == exit7.x and c.y == exit7.y)),
  }
}
