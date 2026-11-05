// Probe: 0.33.1 transition-property forms on the SHIPPED model.
// Licence: AGPL-3.0-or-later.
//
// Purpose: decide whether any form is checked as a TRANSITION. Two properties
// whose truth over transitions of this model is known by hand:
//
//   Q1_orKeep_px      next(px) - px <= 1   must hold on every transition
//   Q2a_bad_orKeep    next(px) - px <= 0   FALSE on every move (control)
//   Q2c_bad_orKeep    next(py) - py > 1    FALSE on every move (control)
//
// If a form reports "no violation" for Q1 and also for a control, the form is
// not reading the next state. If a control goes RED and Q1 stays green, the
// form reads transitions.
module probe_forms {
  import game_state.* from "../game-state"

  temporal Q1_orKeep_px = always((next(px) - px <= 1 and px - next(px) <= 1).orKeep(px))

  temporal Q4_mustChange_px = always((next(px) - px <= 1 and px - next(px) <= 1).mustChange(px))

  temporal Q2a_bad_orKeep = always((next(px) - px <= 0).orKeep(px))

  temporal Q2c_bad_orKeep = always((next(py) - py > 1).orKeep(py))

  // Plain temporal, no orKeep: next() with no stutter allowance.
  temporal Q5_plain_next = always(next(px) - px <= 1)
}
