// Licence: AGPL-3.0-or-later.
//
// TLC backend probe - the falsifiability control (step 2 of the dispatch).
// Every property here has a truth value known BY HAND over the shipped model's
// reachable transitions. A form that stays green on C_* is not reading the
// transition, and no verdict from that backend counts.
//
//   C_px_not_left    next(px) == px - 1        FALSE by hand: negative on the
//                    reachable step (2,1) -> (1,1) of mazeId 0 / platform 5
//                    (platform 7 has the same step on its start row).
//                    Reachable from init in one moveLeft.
//   C_px_jump_two    next(px) == px + 2        FALSE by hand: no action of the
//                    shipped model moves two cells. This is the same SHAPE as
//                    the m2 discriminator (X4) - a control for the FORM.
//
// Controls are stated in the exact orKeep shape the gate uses. Per the R2c
// lesson (quint skill trap 9) a literal `false` limb is constant-folded away;
// these are reachable-false expressions over real model state instead.
module tlc_control {
  import game_state.* from "../game-state"

  temporal C_px_not_left = always((not(next(px) == px - 1)).orKeep(px))

  temporal C_px_jump_two = always((not(next(px) == px + 2)).orKeep(px))
}
