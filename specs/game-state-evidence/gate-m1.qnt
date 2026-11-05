  // Closed mutant gate - transition properties for OMazing's game state.
  // Licence: AGPL-3.0-or-later.
  //
  // TEMPLATE. One file per model is produced by substituting
  // replacing the tokens "./mutants/m1_allow_wall" (quoted import path) and gate_m1 (the
  // module name gate_<model>, which `quint verify --main=` must match).
  //
  // WHAT THIS REPLACES
  // ------------------
  // P2/P4/P5/P6 were state predicates, and their mutants survived because the
  // defect lives in the pair (pre-state, post-state) of one edge. The ceiling
  // round recorded that as a toolchain wall; the transition probe falsified it
  // for the 0.33.0 operator `always(<next-term>.orKeep(x))`. This file applies
  // that operator to every clause it can carry, with a paired control each.
  //
  // THE OPERATOR'S REAL SHAPE
  // ---------------------------------------------------------
  // 1. orKeep takes EXACTLY ONE updated variable - a state name OR a tuple of
  //    them. Two positional arguments -> "Expected 2 arguments, got 3". The
  //    tuple form is what makes a two-coordinate freeze expressible.
  // 2. A guard reading next(v) for a v the property does not update breaks
  //    effect inference. The PRE-state is not readable: P4's freeze gate reads
  //    the post-state `next(won)`, and what that can see follows.
  // 3. orKeep SKIPS a step that leaves the updated expression unchanged (probe
  //    3: "26 transitions" along a walk with blocked moves). A property that
  //    only bites on a BLOCKED move cannot use the blocked variable as its kept
  //    variable.
  // 4. The model's init cannot be extended (product variables stay unset:
  //    QNT502) and its name cannot be redefined by an importer (QNT101).
  //
  // PAIRED CONTROLS: every property's hand-false partner sits beside it, in the
  // same form. A control that stays green marks its instrument dead.

module gate_m1 {
  import game_state.* from "./mutants/m1_allow_wall"

    // --- X1 / K1: the TARGET is an open cell (the m5 kill) -----------------
    // If the post-x is one step right, the target cell must be open. Openness
    // is the maze constant's openness, and out-of-grid cells are absent from
    // that set, so this is S4's two checks (grid, then wall) in one boolean at
    // next(px). m5 (grid guard widened to x >= -2) walks onto an out-of-grid
    // cell and must falsify it - and it does, because the widened guard lets a
    // MOVE land there (the step is a move, not a stay).
  temporal X1_right_target_open = always(
    ((next(px) != px + 1) or isOpen(mazeId, platform, next(px), py)).orKeep(px))
    // K1: same shape, the target must be a WALL. False on every right-move into
    // an open cell.
  temporal K1_right_target_wall = always(
    ((next(px) != px + 1) or not(isOpen(mazeId, platform, next(px), py))).orKeep(px))

    // --- X2 / K2: the same claim going down (py's variable) ----------------
  temporal X2_down_target_open = always(
    ((next(py) != py + 1) or isOpen(mazeId, platform, px, next(py))).orKeep(py))
  temporal K2_down_target_wall = always(
    ((next(py) != py + 1) or not(isOpen(mazeId, platform, px, next(py)))).orKeep(py))

    // --- X3 / K3: P4, the freeze after a win, on the position tuple --------
    // The position tuple is ONE updated variable, so both coordinates are
    // compared on one step. The gate reads the POST win flag. What that
    // buys and what it does not: it catches a step that ends won AND moved, and
    // it is blind to a move out of a PREVIOUSLY won state, because the post flag
    // is false again there. Checked against the m3 mutant.
  temporal X3_won_freeze = always(
    ((not(next(won)) or (next(px) == px and next(py) == py)))
       .orKeep((px, py)))
    // K3: same shape, absurd - a step that ends won MUST have moved.
  temporal K3_won_moves = always(
    ((not(next(won)) or (next(px) != px or next(py) != py)))
       .orKeep((px, py)))
  // EMPTY-CONTROL: the K3 pair, same `orKeep((px, py))` shape.
  temporal K3b_won_moves = always(
    ((not(next(won)) or (next(px) != px and next(py) != py)))
       .orKeep((px, py)))


    // --- X4 / K4: P2, one cell per move, on the position tuple -------------
  temporal X4_step_one_cell = always(
    (((next(px) == px + 1 or next(px) == px - 1) and next(py) == py) or
     ((next(py) == py + 1 or next(py) == py - 1) and next(px) == px))
       .orKeep((px, py)))
  temporal K4_step_two_cells = always(
    (((next(px) == px + 2 or next(px) == px - 2) and next(py) == py) or
     ((next(py) == py + 2 or next(py) == py - 2) and next(px) == px))
       .orKeep((px, py)))

    // --- X5 / K5: P5's blocked-move half, as far as it can be stated ------
    // A MOVE that does not end won must land on an open cell: if the position
    // changes and the step does not end won, the post-cell is open. This is the
    // MOVE case of S2; the pure BLOCKED case (no move at all) leaves the
    // position unchanged and is a keep step the operator skips.
  temporal X5_moved_cell_open = always(
    (((next(px) == px and next(py) == py) or next(won) or
      isOpen(mazeId, platform, next(px), next(py))))
       .orKeep((px, py)))
    // K5: same shape, absurd - a move must land on a WALL cell.
  temporal K5_moved_cell_wall = always(
    (((next(px) == px and next(py) == py) or next(won) or
      not(isOpen(mazeId, platform, next(px), next(py))))
       .orKeep((px, py))))
}
