# Hex tactics: handoff

A hex-based tactical war game of the owner's own design, started on 2026-11-23. `tactics` is its working name (owner, 2026-11-23), kept until the game has a
name of its own. Built in steps.

## State

- Step 1, data models and hex grid math (2026-11-23).
- Step 2, movement and pathfinding over an immutable game state (2026-11-23).
- Step 3, combat and damage, from dice rolls passed in (2026-11-23).
- Step 4, turns, phases, the economy, sector control and the VP win, through one action reducer (2026-11-23).
- Step 5, the HRF game: meta `tactics`, URL `/play/tactics`, the starter two-player skirmish map, recorded actions with
  the attack dice rolled by a `Roll`, an Easy bot, headless bot games and a map drawn in code (2026-11-23). Playable
  as Local (hotseat), Quick Game (against the bot) and Play Online.
- In the site's build (`common.sbt`; `hex-check.scala` is in `build.sbt`'s `excludeFilter`) and in `HRF.metaUIs`
  (`hrf.scala`), but not on the main `/play` menu: add `tactics.Meta` to `HRF.menuMetas` to list it. No ranking.

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
- `scenario.scala`: `Skirmish`, the starter map (engine types only, so `HexCheck` tests it): 9 rows of 13 and 12
  pointy-top hexes, the first rows and the middle row up to the middle hex drawn as text and the rest made by turning
  them half round the middle hex (`turned`), so both sides get the same ground; the hex names (`C4`: column letter,
  row number; `axial` and `column` convert from and to offset coordinates, odd rows shifted half a hex right); the
  sectors (the Town in the middle, 2 VP and 1 manpower; on each side a Supply depot, 2 munitions, a Fuel dump, 2 fuel,
  a Farm, 1 manpower, and a Ridge, 1 VP); the zones (each side's two edge columns, on ground infantry can stand on),
  the rules (10 VP, starting 6 manpower and 2 munitions, 2 manpower and 1 munition a turn) and one squad of riflemen
  each at the start (`<player>-A`).
- `hex-check.scala`: `tactics.HexCheck`, the checks.

HRF (these need the rest of the site to compile):

- `package.scala`: the package object (`hrf.base.Gaming`, `BotGaming`, `GreyMapUI`), `F` = `Faction`, `G` = `Game`.
- `game.scala`: `Faction` (`Red`, `Blue`; their names are the engine's player names), `Cell` (a hex in an action,
  as the engine's `Hex` isn't a `Record` and can't be written to a saved game), the actions, and `Game`, which holds
  the engine's `GameState` (`state`) and each deployed unit's type (`types`, for names on the screen). Every recorded
  action goes through `Turns.applyAction` (`apply`, which throws on a refusal, as the choices offered are only the
  allowed ones), so a replay rebuilds the same game.
- `meta.scala`: `Meta`, 2 players, the Easy bot, no options and no image assets.
- `bot.scala`: `BotXX`, the Easy bot: deploys while it can, attacks what it can hurt, moves toward the sectors it
  doesn't hold, with some chance.
- `host.scala`: `tactics.Host`, headless bot games.
- `ui.scala`: the screen, drawn in code with no images: the board into one canvas once (terrain and its marks, a
  shield with a bar per cover level, the sectors' names and what they give, the zones as dashed borders in the
  side's color, the hex names), then the sector holders' rings, the choices (gold outlines, red around attack
  targets, white around the unit picked) and the units' tokens (a disc, or a rounded square for vehicles, in the
  owner's color with the type's letters, a health bar and an orange S when suppressed; dimmed when done for the
  turn), each canvas kept by its look. The player panels (phase, VP, stock, income each turn, units, sectors), the
  turn panel, and "Rules and units" (the rules as played and every unit type's numbers).
- `styles.scala`: the styles.
- `serialize.scala`: `Serialize` (prefix `tactics.`).

## The HRF game

- `StartAction` makes the match (`Skirmish.start`, the first seat on the west side), then each player's turn starts
  with a `Milestone` (`PlayerTurnAction`, which logs the income the engine gave) and `TurnAction` offers what the phase
  allows:
  - Income: `DeployAction` for each unit type the player can afford on each free hex of its zone (tapped on the map;
    a hex with several types offers them over the map), and `ToActionPhaseAction`.
  - Action: `UnitMenuAction` (soft) for each unit with a move or an attack left, offering its `MoveAction`s
    (`Movement.reachable`; a unit that has attacked has none) and `AttackAction`s (`Combat.targets`), and
    `EndActionsAction`.
  - End: `EndTurnAction`, the only choice, so the runner takes it at once.
  A phase with only its "end" choice is skipped the same way.
- `AttackAction` returns a `Roll` of the weapon's dice (d6, `Die.range`), and the recorded `AttackRolledAction` gives the
  rolls to the engine's `AttackUnit`, then logs the `CombatReport`.
- A sector captured by a move or a deployment is logged. The engine's winner ends the game (`GameOverAction`).
- The engine's player names are the factions' names (`Red`, `Blue`), unit ids `Red-1` (deployed, numbered across both
  sides) and `Red-A` (starting). Actions name units by id and unit types by name, so they write and parse back.

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

On 2026-11-23: 123 passed, 0 failed, a full match among them, and the skirmish map (its symmetry, names, sectors,
zones, that every sector can be reached from both zones, and an opening).

The HRF game: headless bot games on the JVM like `pincer.Host` (see `pincer/HANDOFF.md`, Testing: a project with
`common.sbt` with `%%` and only `tactics` as a game directory, `host.xsbt` as its `build.sbt`, the engine's sources
linked, `base.scala` without its `arcs` line, and without `convert-images.scala`, `live-check.scala` and
`lookup-index.scala`): `TACTICS_GAMES=10 sbt "runMain tactics.Host"`. On 2026-11-23 ten games of two Easy bots each
ended with a winner in 200 to 280 steps, every action written and parsed back.

In a browser: serve `/play/tactics` from `index.html` (see "Browser driving" in `nort/HANDOFF.md`) and start a
Quick Game or a Local Game.

## Next

- The owner's own rules for what the sample units, weapons, costs and map are standing in for.
- A better bot.
- Art, if wanted, in place of the shapes drawn in code.
