# Hex tactics: handoff

A hex-based tactical war game of the owner's own design, started on 2026-11-23. `tactics` is its working name (owner, 2026-11-23), kept until the game has a
name of its own. Built in steps.

## State

- Step 1, data models and hex grid math (2026-11-23).
- Step 2, movement and pathfinding over an immutable game state (2026-11-23).
- No meta, UI, rendering, combat rules or dice yet.
- Not in `build.sbt` yet, so the site's build doesn't include it. Add
  `Compile / unmanagedSourceDirectories += baseDirectory.value / "tactics"` to `common.sbt` and
  `"hex-check.scala"` to `build.sbt`'s `excludeFilter` when the meta is added.

## Files

- `hex.scala`: `Hex(q, r)` axial coordinates (`s` worked out), the six directions, distance, cube rounding
  and `Hex.line` (the hexes on a straight line, nudged to pick a side of an edge).
- `model.scala`: `MoveKind` (Infantry, Vehicle), `Terrain` (sight blocking and the movement cost for each kind),
  `Cover` (None, Light, Heavy, ordered), `HexTile` (q, r, terrain, cover, optional resources and VP) and
  `CombatUnit` (id, owner, health, movement, armor, q, r, kind, and `spent`, the movement used this turn;
  not `Unit`, which would hide Scala's `Unit` in the package).
- `board.scala`: `Board`, immutable (`add` gives a new board; it changed the board in place in Step 1), the
  tiles by coordinate (any shape), `distance`, `neighbors` (tiles on the board) and `lineOfSight`;
  `Board.of(tiles)`, and `Board.hexagon(radius)` for a test board.
- `state.scala`: `GameState(board, units)`, immutable, with `occupants` (who is where), `place`, `reachable`,
  `path`, `move` and `newTurn`. Making a state with two units in a hex, or a unit off the board, throws.
- `movement.scala`: `Movement`, the pure functions (`enterCost`, `canEnd`, `reachable`, `path`, `move`, by Dijkstra),
  and `Path` (the hexes, start and end included, and the cost).
- `hex-check.scala`: `tactics.HexCheck`, the checks.

## Line of sight

Blocked when a hex strictly between the two has blocking terrain (Mountain, Building). The two ends never
block, hexes missing from the board are open, and a line running exactly along a hex edge is traced on both
sides of it and is blocked only if both sides are. An end off the board throws `NoSuchElementException`.

## Movement

| Terrain  | Infantry | Vehicle |
|----------|----------|---------|
| Plains   | 1        | 1       |
| Forest   | 2        | 3       |
| Hills    | 2        | 2       |
| Building | 2        | can't   |
| Water    | can't    | can't   |
| Mountain | can't    | can't   |

- Entering a hex costs its terrain's cost for the unit's kind; leaving costs nothing.
- A unit passes through its own owner's units but can't end its move on one, and can't enter another
  owner's unit's hex at all. A placed unit must be on an empty hex its kind can enter.
- A move takes the cheapest path, spends its cost (`spent`), and is refused when that is more than the unit's
  movement points left (`movesLeft`). `newTurn` sets every unit's `spent` back to 0. A unit at 0 health
  (`alive` false) doesn't move.
- `reachable` gives every hex the unit can end in this turn and its cost. `path` gives the cheapest path whatever
  the movement left, for planning ahead.
- Ties between equal-cost paths are broken by cost, then q, then r, so every client and replay finds the same one.
- Errors come back as `Left(reason)`.

The costs and the vehicle rules were picked on 2026-11-23 (vehicles 3 in forest, never in buildings); they are the
`Terrain` case objects' two last arguments.

## Testing

The package has no HRF dependencies, so it compiles on its own with the Scala 2.13.16 compiler
(`scala-compiler`, `scala-library`, `scala-reflect` and `jline` jars from Maven Central):

```
java -cp <jars> scala.tools.nsc.Main -usejavacp -d out tactics/*.scala
java -cp scala-library-2.13.16.jar:out tactics.HexCheck
```

On 2026-11-23: 54 passed, 0 failed.
