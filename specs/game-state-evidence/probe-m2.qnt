// Probe overlay: transition-form properties over ./mutants/m2_teleport.
// Licence: AGPL-3.0-or-later.
//
// Q2c is FALSE on every move by hand (a move changes py by at most one): if it
// stays green, the form is not reading the next state. Q1 is TRUE by hand.
module tl_probe {
  import game_state.* from "./mutants/m2_teleport"

  temporal Q2c_bad_orKeep = always((next(py) - py > 1).orKeep(py))

  temporal Q1_orKeep_px = always((next(px) - px <= 1 and px - next(px) <= 1).orKeep(px))
}
