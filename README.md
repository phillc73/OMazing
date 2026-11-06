# OMazing Labyrinth

A maze navigation game with multiple generation algorithms and difficulty levels, built in OCaml.

## Features

- **10 Maze Generation Algorithms:**
  - Recursive Backtracker
  - Prim's Algorithm
  - Growing Tree
  - Hunt-and-Kill
  - Eller's Algorithm
  - Wilson's Algorithm
  - False Door
  - Blind Swarm
  - Randomized DFS
  - Recursive Division

- **3 Difficulty Levels:**
  - Easy: 21x21 maze
  - Medium: 41x41 maze
  - Hard: 81x81 maze (automatically scales to fit screen)

## Maze Algorithms

| Algorithm | Description |
|-----------|-------------|
| **Recursive Backtracker** | Creates long, winding passages with a depth-first exploration pattern. Tends to have longer corridors with fewer branches. Starts from top-left, resulting in mazes with a slight diagonal bias. |
| **Prim's** | Grows the maze like a spanning tree from a random starting point. Produces more uniform mazes with balanced corridors and open spaces. Passages are generally shorter and more evenly distributed. |
| **Growing Tree** | Generalizes Prim's and Backtracker - uses both newest and random selection. With 50% probability: extends newest path (like Backtracker) or random cell. Produces mazes with a natural mix of long corridors and open areas. |
| **Hunt-and-Kill** | Alternates between random walk ('hunt') and scanning for unvisited cells ('kill'). Creates mazes with long meandering passages and sudden turns. Efficient with a good balance of speed and passage quality. |
| **Eller's** | Single-pass algorithm that processes one row at a time. Very efficient for large mazes with O(n) complexity. Tends to create mazes with a horizontal bias and long corridors. |
| **Wilson's** | Uses loop-erased random walks to create perfectly uniform mazes. Slowest algorithm but produces the most uniform, unbiased results. Every cell is equally likely to be connected to any other. |
| **False Door** | Standard maze with added 'false doors' - one-cell alcoves. Each door is a dead end that joins nothing, so no route changes. Adds visual complexity without creating alternative paths. |
| **Blind Swarm** | Multiple simultaneous random walks that explore and carve the maze. A walker joins the maze only when a step is accepted, never by luck. Walkers merge when they meet, forming a swarm-like pattern. |
| **Randomized DFS** | Depth-first search with randomized neighbor selection. Similar to Backtracker but implemented iteratively with explicit stack. Creates mazes with long, winding passages and deep recursion patterns. |
| **Recursive Division** | Top-down divide-and-conquer approach with walls and passages. Creates maze by recursively splitting regions with dividing walls. Produces mazes with long straight passages and grid-like structure. |

- **Controls:**
  - WASD / Arrow Keys: Move
  - R: Reset with new maze
  - M: Return to menu
  - Q: Quit
  - 1-3: Select difficulty
  - 0-9: Select algorithm

## Requirements

- OCaml 4.08.0 or later
- Dune build system
- Graphics library (X11-based)
- opam package manager

## Installation

```bash
# Clone the repository
git clone https://github.com/phillc73/OMazing
cd OMazing

# Install dependencies
opam install . --deps-only --with-test
```

## Building

Generation and game state live in a core library with no Graphics
dependency, so the core and the test suite build on any machine:

```bash
dune build        # core and tests, no X11 needed
dune test
```

The playable front end needs the Graphics library (X11). Build it with
the flag that enables it:

```bash
OMAZING_GRAPHICS=true dune build main.exe
```

Or use the Makefile, where `make build` sets the flag for you:

```bash
make build
```

### Run the tests

```bash
dune runtest
```

Or:

```bash
make test
```

## Running

### Run the game:

```bash
./_build/default/main.exe
```

Or:

```bash
make run
```

## Project Structure

```
.
├── LICENSE                    # GNU AGPL v3, verbatim
├── Makefile                  # Build automation
├── README.md                 # This file
├── RELEASE-NOTES.md          # Release notes
├── dune                      # Executable stanza (gated on OMAZING_GRAPHICS)
├── dune-project              # Project metadata
├── main.ml                   # Entry point for executable
├── .zanuda                   # Zanuda linter configuration
├── core/
│   ├── maze_core.ml          # Generators and game state, no Graphics
│   └── maze_core.mli         # Public interface
├── ui/
│   └── labyrinth_ui.ml       # Rendering, menus and the game loop
├── window_position/
│   └── window_position.ml    # window-position syntax, range and screen check
└── test/
    ├── dune                 # Test build configuration
    ├── test.ml              # Test suite entry point
    ├── test_maze.ml         # Maze generation tests
    ├── test_structure.ml    # Perfect-maze and reachability invariants
    ├── test_game.ml         # Game state tests
    ├── test_qcheck.ml       # Randomised property tests
    └── test_window_position.ml  # window-position parsing and screen check
```

## Test Suite

The project includes 113 Alcotest unit tests covering:

- **Maze Generation:**
  - All 10 algorithms tested for correct dimensions
  - Exactly one start and one exit per maze
  - Path existence from start to exit verified via BFS
  - Every open cell reachable from the start, at Easy, Medium and Hard
  - The nine perfect-maze algorithms produce a spanning tree on the room
    lattice: every room open, edges equal to rooms - 1, no cycles
  - False Door adds dead-end alcoves only, never a lattice edge
  - Walls block movement: a step into a wall leaves the player in place

- **Game State Logic:**
  - Bounds checking
  - Wall detection
  - Player movement (valid, out-of-bounds, into walls)
  - Win condition verification
  - Pathfinding and reachability

Run tests with:

```bash
make test
# or
dune test
```

## Linting

The project uses [zanuda](https://github.com/Kakadu/zanuda) for OCaml linting.

To run the linter:

```bash
zanuda -dir .
```

Some lints are intentionally disabled (see `.zanuda` file):
- `mutability_check`: Mutability is used for Graphics library integration and game state
- `exc_error_swallowing`: Wildcard pattern matching is used idiomatically

## Specification

The behaviour contracts for the maze core, the generators and the game
state are in [docs/spec.md](docs/spec.md).

## Licence

This project is licensed under the GNU Affero General Public License version 3 or later (AGPL-3.0-or-later).

See [LICENSE](LICENSE) for the full licence text.

## Contributing

Contributions are welcome! Please ensure:
- All tests pass (`make test`)
- Linting passes (`zanuda -dir .`)
- Code follows OCaml best practices

## Acknowledgments

- Built with [OCaml](https://ocaml.org/)
- Uses [Dune](https://dune.build/) build system
- Tests use [Alcotest](https://github.com/mirage/alcotest)
- Randomised tests use [QCheck](https://github.com/c-cube/qcheck)
- Linting with [Zanuda](https://github.com/Kakadu/zanuda)
- Behaviour models in [`specs/`](specs/) use [Quint](https://quint.sh) from [Informal Systems](https://informal.systems/)

## AI assistance

The code in this repository was written with the assistance of a large language model and reviewed by its maintainer.
