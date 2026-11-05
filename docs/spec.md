# OMazing specification

Behaviour contracts for the maze core, the generators and the game
state. The OCaml code is the implementation; where this document and
the code disagree, the code and its tests govern and this document is
wrong.

## Data model

The maze is a `cell array array`, row-major, indexed `y` then `x`.

```ocaml
type cell = Wall | Path | Start | Exit
```

The maze is a **room lattice**: room `(rx, ry)` is grid cell
`(2*rx + 1, 2*ry + 1)`. A carved wall between two lattice-adjacent
rooms is a **lattice edge**. Corridor cells sit at (even, odd) and
(odd, even) coordinates; (even, even) cells are lattice interiors and
are never carved by a spanning-tree algorithm. A generator marks
exactly one `Start` and one `Exit` cell.

## Dimension contract

Generation dimensions are the globals `Maze_core.width` and
`Maze_core.height`, set by `set_dimensions_exn`, which raises
`Invalid_argument` unless both are odd and at least 3. The three
difficulties are Easy 21x21, Medium 41x41, Hard 81x81. A generator is
a pure function of (algorithm, dimensions, RNG state); no generator
takes a seed parameter and the process-global `Random` is the only
entropy source, reseeded by `init_game` with `Random.self_init`.

## Generation contract

Every generator has type
`unit -> cell array array * int * int * int * int * maze_algorithm`:
the maze, start `(x, y)`, exit `(x, y)`, and the algorithm echoed back.

- Every room of the lattice is open (`Path`, `Start` or `Exit`) after
  generation for all ten algorithms.
- `Start` and `Exit` are distinct, non-wall cells, installed by
  `mark_start_exit`, which refuses to overwrite a cell that is still
  `Wall`. The exit is the bottom-right room; the start is drawn from
  the top-left half of the lattice (`(n + 1) / 2` rooms per axis),
  except Backtracker and False Door, which fix the start at room
  `(0, 0)`.
- Generators return freshly allocated arrays; no generator mutates a
  maze returned by another call.

## Invariants

Pinned by `test/test_structure.ml` for each algorithm at all three
sizes:

1. **Perfect maze** (nine algorithms: Backtracker, Prim's, Growing
   Tree, Hunt-and-Kill, Eller, Wilson, Blind Swarm, Randomized DFS,
   Recursive Division): every room open, carved lattice edges equal
   rooms - 1, and the edge set is acyclic. Acyclic plus connected
   implies a spanning tree; the count plus every-room-open implies
   connected.
2. **Reachability** (all ten): every open cell is reachable from the
   `Start` cell by a flood fill through open cells.
3. **False Door stub contract**: a False Door result is the Backtracker
   result plus zero lattice edges. Each false door is a one-cell alcove
   with exactly one open neighbour; a candidate that would end with
   more than one open neighbour is closed again. Stub counts grow with
   size (12 at Easy, 53 at Medium, 253 at Hard, seed 11).

## Algorithm catalogue

| Algorithm | Family | Properties |
|---|---|---|
| Recursive Backtracker | depth-first search, recursive | longest corridors of the ten; start fixed at room `(0, 0)` |
| Prim's | randomized Prim's, frontier-walls variant | many short branches; frontier is a growable array with swap-and-pop |
| Growing Tree | Jamis Buck's Growing Tree, p = 0.5 newest/random | mix of long corridors and open areas |
| Hunt-and-Kill | Hunt-and-Kill | random-walk carve plus row scan reprise starting from a random cell |
| Eller's | Eller's, row-by-row union with set relabelling | one downward link per set per row; horizontal bias from the merge order |
| Wilson's | Wilson's, loop-erased random walk | uniform spanning tree; slowest of the ten |
| False Door | Backtracker plus a decoration | stub alcoves only, no route change (invariant 3) |
| Blind Swarm | multi-walker random growth | walkers merge on meeting; a walker carves only after its step is accepted |
| Randomized DFS | iterative randomized depth-first search | textbook iterative form; explicit stack |
| Recursive Division | recursive division, wall-adding | one passage per dividing wall; walls split the longer axis first |

## Movement contract

```ocaml
val move_player : game_state -> direction -> game_state
```

- `move_player` never raises: `is_wall` is derived from the maze the
  state carries, and an off-grid coordinate is a wall.
- A move into a wall or out of the grid returns the state unchanged
  (idempotent on blocked moves).
- Reaching the `Exit` cell sets `game_won`; movement freezes after a
  win.
- `in_bounds` is the grid check; `is_wall` is the transport check.

## Window and rendering

`calc_window_dimensions` caps the window at 1920x1080 and scales the
cell size to fit; `draw_game` maps the maze onto the window with the
y-axis flipped. The menu is a fixed 41x41-cell surface with rows laid
out top-down under the instructions, clipped so they cannot run off
the top edge.

## Testing

`dune test` runs 113 cases in five groups: maze structure (the
invariants above), lattice properties (randomised checks over the
invariants), game state (movement, bounds, win behaviour), generation
(dimensions, start/exit contract, path to exit) and window-position
parsing. All generators are seeded with `Random.init` before each
structural case, so failures are reproducible.

## Build

```bash
dune build                              # core and tests, no X11 needed
dune test
OMAZING_GRAPHICS=true dune build main.exe   # the playable front end
```

The core library has no Graphics dependency. The UI library and the
executable are gated on `OMAZING_GRAPHICS=true`. The menu layout
follows from the stated cell arithmetic rather than from measured
pixels.

## Known limits

- Generators draw from the process-global `Random`; there is no seed
  input, so a player cannot replay a specific maze.
- Mouse hover for menu descriptions is not implemented; the menu is
  keyboard-only.
