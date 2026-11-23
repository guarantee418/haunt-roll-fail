# Hex tactics: handoff

A hex-based tactical war game of the owner's own design, started on 2026-11-23. `tactics` is its working name (owner, 2026-11-23), kept until the game has a
name of its own. Built in steps.

## State

- Step 1, data models and hex grid math (2026-11-23).
- Step 2, movement and pathfinding over an immutable game state (2026-11-23).
- Step 3, combat and damage, from dice rolls passed in (2026-11-23).
- Step 4, turns, phases, the economy, sector control and the VP win, through one action reducer (2026-11-23).
- No dice rolling, meta, UI or rendering yet.
- Not in `build.sbt` yet, so the site's build doesn't include it. Add
  `Compile / unmanagedSourceDirectories += baseDirectory.value / "tactics"` to `common.sbt` and
  `"hex-check.scala"` to `build.sbt`'s `excludeFilter` when the meta is added.

## Files

- `hex.scala`: `Hex(q, r)` axial coordinates (`s` worked out), the six directions, distance, cube rounding
  and `Hex.line` (the hexes on a straight line, nudged to pick a side of an edge).
- `model.scala`: `MoveKind` (Infantry, Vehicle), `Terrain` (sight blocking and the movement cost for each kind),
  `Cover` (None, Light, Heavy, ordered), `HexTile` (q, r, terrain, cover, `income` (Resources; a single
  optional number until Step 4) and optional VP; a tile with either is a sector) and
  `CombatUnit` (id, owner, health, movement, armor, q, r, kind, `spent`, the movement used this turn,
  `weapon`, `suppressed` and `attacked`; not `Unit`, which would hide Scala's `Unit` in the package).
- `board.scala`: `Board`, immutable (`add` gives a new board; it changed the board in place in Step 1), the
  tiles by coordinate (any shape), `distance`, `neighbors` (tiles on the board) and `lineOfSight`;
  `Board.of(tiles)`, and `Board.hexagon(radius)` for a test board.
- `state.scala`: `GameState`, immutable: the board and units, and for a match the players in turn order, their
  `economy` (`PlayerState`: resources and VP), `control` (who holds each sector), deployment `zones`, `rules`, `turn`,
  `active` player, `phase`, `deployed` (for unit ids), `winner` and `lastReport`. The match fields have defaults, so
  a state of a board and units works for movement and combat alone. `GameState.start` makes a match. Making a state
  with two units in a hex, or a unit off the board, throws.
- `movement.scala`: `Movement`, the pure functions (`enterCost`, `canEnd`, `reachable`, `path`, `move`, by Dijkstra),
  and `Path` (the hexes, start and end included, and the cost).
- `combat.scala`: `Weapon` (range, minimum range, dice, accuracy, damage against infantry and vehicles,
  penetration), sample `Weapons` (rifle, machine gun, anti-tank gun, mortar), `CombatReport`, and `Combat`,
  the pure functions (`validate`, `canAttack`, `targets`, `needed`, `damagePerHit`, `attack`).
- `economy.scala`: `Resources` (manpower, munitions, fuel), `PlayerState`, `Phase` (`IncomePhase`, `ActionPhase`,
  `EndPhase`), `UnitType` (a deployable unit and its cost), sample `Roster`, and `Rules` (VP target, starting
  resources, base income each turn, roster).
- `turns.scala`: `TacticsAction` (`DeployUnit`, `MoveUnit`, `AttackUnit`, `EndPhaseAction`; not `EndPhase`, the
  phase's name) and `Turns`, the reducer (`applyAction`, `applyAll`) and its parts (`capture`, `income`,
  `beginTurn`, `checkWinner`).
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

## Combat

`attack(state, attacker, target, rolls)` gives `Either[String, (GameState, CombatReport)]`. The dice are rolled
outside and passed in, one roll (1 to 6) for each of the weapon's dice, so an attack comes out the same in every
client, replay and check; in HRF they will be rolled by a random action and recorded.

- `canAttack`: both alive, different owners, the attacker has a weapon, the distance within the weapon's
  `minRange` to `range`, and `Board.lineOfSight` (units don't block it). `validate` gives the reason it can't.
- To hit, each die needs `needed` or more: the weapon's accuracy, +1 for light cover and +2 for heavy cover on the
  target's hex (the tile's `cover`, not its terrain), +1 if the attacker is suppressed; a 1 always misses, a 6 always hits.
- Each hit deals the weapon's damage against the target's kind, less the target's armor beyond the weapon's
  penetration (`max(0, armor - penetration)`), never below 0. Damage past the target's health is lost.
- A target at 0 health is destroyed and taken off the board, freeing its hex.
- Infantry hit at least once (even for 0 damage) and left standing is suppressed until `newTurn`; vehicles never are.
  A suppressed unit's own attacks need 1 more; suppression doesn't change movement yet.
- The attacker isn't marked as having attacked: how often a unit may attack is for the turn rules.

These rules (d6 hits, the cover and suppression numbers, the sample weapons) were picked on 2026-11-23.

## Turns and the economy

`Turns.applyAction(state, action)` (or `state.applyAction(action)`) gives `Either[String, GameState]`: the new state,
or why the action is refused, changing nothing. Every action names its player, who must be the current one.

- A turn is every player's turn once, in `players` order. Each player's turn has three phases, left with `EndPhaseAction`:
  - Income: on entering it, the player's units get their movement and attack back, and the player gets the rules'
    base income plus each held sector's income, and scores each held sector's VP. `DeployUnit` puts a unit of a
    roster type on an empty hex of the player's zone it can stand on, paying its cost; its id is `<player>-<n>`.
  - Action: `MoveUnit` and `AttackUnit`. Each unit attacks once a turn, and can move before it (in several moves,
    up to its movement points) but not after. A refused attack doesn't use it up.
  - End: on leaving it, the player's units stop being suppressed, so suppression lasts through the suppressed
    side's next turn (its attacks need 1 more then).
- A unit ending a move on a sector, deployed on one or starting the match on one, captures it for its owner; it stays
  theirs, even with nobody on it, until another player's unit ends a move there. A destroyed unit's sector stays held.
- A player at `rules.vpTarget` VP wins (checked after every action, so at the start of their turn, when VP come in);
  after that every action is refused. There is no other way to win yet.
- `lastReport` holds the combat report of the last action if it was an attack, for the log to come.
- In HRF each action will be a recorded action, the dice rolled by a random action before `AttackUnit`; replaying the
  actions through `applyAction` rebuilds the same game.

The phases, the income and scoring at the start of a player's turn, the attack and move limits, suppression lasting
through the next turn, and the sample roster and its costs were picked on 2026-11-23.

## Testing

The package has no HRF dependencies, so it compiles on its own with the Scala 2.13.16 compiler
(`scala-compiler`, `scala-library`, `scala-reflect` and `jline` jars from Maven Central):

```
java -cp <jars> scala.tools.nsc.Main -usejavacp -d out tactics/*.scala
java -cp scala-library-2.13.16.jar:out tactics.HexCheck
```

On 2026-11-23: 115 passed, 0 failed, a full match among them.
