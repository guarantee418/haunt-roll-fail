# Hex tactics: handoff

A hex-based tactical war game, started on 2026-11-23. `tactics` is a working name, kept until the game has a
name of its own. Built in steps; this is Step 1.

## State

- Step 1, data models and hex grid math only: no meta, UI, rendering or combat rules yet.
- Not in `build.sbt` yet, so the site's build doesn't include it. Add
  `Compile / unmanagedSourceDirectories += baseDirectory.value / "tactics"` to `common.sbt` and
  `"hex-check.scala"` to `build.sbt`'s `excludeFilter` when the meta is added.

## Files

- `hex.scala`: `Hex(q, r)` axial coordinates (`s` worked out), the six directions, distance, cube rounding
  and `Hex.line` (the hexes on a straight line, nudged to pick a side of an edge).
- `model.scala`: `Terrain` (Mountain and Building block sight), `Cover` (None, Light, Heavy, ordered),
  `HexTile` (q, r, terrain, cover, optional resources and VP) and `CombatUnit` (id, owner, health,
  movement, armor, q, r; not `Unit`, which would hide Scala's `Unit` in the package).
- `board.scala`: `Board`, the tiles by coordinate (any shape), `distance`, `neighbors` (tiles on the board)
  and `lineOfSight`; `Board.hexagon(radius)` makes a test board.
- `hex-check.scala`: `tactics.HexCheck`, the checks of distance, neighbors, lines and line of sight.

## Line of sight

Blocked when a hex strictly between the two has blocking terrain. The two ends never block, hexes missing
from the board are open, and a line running exactly along a hex edge is traced on both sides of it and is
blocked only if both sides are. An end off the board throws `NoSuchElementException`.

## Testing

The package has no HRF dependencies, so it compiles on its own with the Scala 2.13.16 compiler
(`scala-compiler`, `scala-library`, `scala-reflect` and `jline` jars from Maven Central):

```
java -cp <jars> scala.tools.nsc.Main -usejavacp -d out tactics/*.scala
java -cp scala-library-2.13.16.jar:out tactics.HexCheck
```

On 2026-11-23: 23 passed, 0 failed.
