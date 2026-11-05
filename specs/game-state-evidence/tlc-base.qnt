// Licence: AGPL-3.0-or-later.
//
// TLC backend probe - the m2 discriminator (X4 in the gate's shape) plus its
// hand-false partner. One module per model lives in specs/game-state-evidence
// (tlc-<model>.qnt): a property name resolves in the MAIN module only (QNT404),
// and an import path resolves relative to the importing file.
//
//   X4_orKeep   a move changes the position by at most one cell, in line.
//               TRUE by hand on the shipped model; FALSE on m2_teleport.
//   K4_orKeep   same shape, absurd: a move changes the position by exactly
//               two cells. FALSE by hand on the shipped model (the control).
module tlc_base {
  import game_state.* from "../game-state"

  temporal X4_orKeep = always(
    (((next(px) == px + 1 or next(px) == px - 1) and next(py) == py) or
     ((next(py) == py + 1 or next(py) == py - 1) and next(px) == px))
       .orKeep((px, py)))

  temporal X1_orKeep = always(
    ((next(px) != px + 1) or isOpen(mazeId, platform, next(px), py)).orKeep(px))

  temporal K1_orKeep = always(
    ((next(px) != px + 1) or not(isOpen(mazeId, platform, next(px), py))).orKeep(px))

  temporal K4_orKeep = always(
    (((next(px) == px + 2 or next(px) == px - 2) and next(py) == py) or
     ((next(py) == py + 2 or next(py) == py - 2) and next(px) == px))
       .orKeep((px, py)))
}
