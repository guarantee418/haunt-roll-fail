# Company of Heroes ("Tactics"): handoff

Company of Heroes: The Board Game, 2nd Edition (Bad Crow Games), meta `tactics`, URL `/play/tactics`, started on
2026-11-23 from the core rulebook (RulesPal) and the Tabletop Simulator mod "Company of Heroes - 2nd Edition" (Steam
workshop 2314664989), which has the Building Boards, the maps and the four books (Rulebook, Faction Guide, Mission
Booklet, Solo Rulebook as PDFs in its save). The owner's team playtests it for Bad Crow Games. `RULES.md` has the rules as
played and the choices made.

## State

- Playable: two players, any two of the five core factions (US Forces, Wehrmacht, British Forces, Soviet Forces, Oberkommando West;
  thirty core Commanders, six a faction), on the Mission Booklet's six 2-panel core missions (the setup's
  Map choice, `MapChoice`, Moro River by default: Moro River, Across the Don, Between Two Rivers, Ardennes Forest, Frozen Lake,
  Road to Moscow, with Ice, the Cathedral and multi-hex Buildings on the Ardennes panels); Local (hotseat), Quick Game and Play Online, with
  an Easy bot. The setup's Rules choice: the Full Game Rules (every new game since 2026-11-24: Operations deck, Retreats,
  Assaults, Overwatch, firing arcs and Heavy Vehicle facing, multi-target Heavy Vehicles, HE Splash, Frontal Armor,
  destructible Buildings, Moro River's tank traps) or the First Game Rules (and every game made before the choice). With the
  Full Game Rules, the twelve core Commanders (the Commanders choice, `DrawCommanders`, every new game since 2026-11-24): three drawn
  a player, tiers unlocked with XP, their call-ins (Priest, Easy Eight, Tiger, Assault Engineers, Osttruppen, Stormtroopers),
  Emplacements (MG Bunker, Pak 43), opening abilities and strikes, Smoke, drops and upgrades.
- On the main `/play` menu since 2026-11-24 (`HRF.menuMetas`, its card in `topMenu` in `hrf.scala`). Its images are in
  `haunt-roll-fail/webp2/tactics/images/`, which the server serves as `/hrf/webp2/`; they were first committed to the repository root's
  `webp2/`, where the live site couldn't find them (404).
  No ranking.

## Files

- `board.scala`: `MapData`, a map's grid (its rows, one line a hex: terrain, the six edges' lines, tank trap, Control
  Point), neighbors, distance, line of sight, the edges a line of fire crosses, the Spawn Areas; `MoroRiverFirst`, the rows of
  the games made before the Map choice. `Board` answers for the game's map (`Board.current`, set by `Game`).
- `missions.scala`: the six missions' maps (`MoroRiver` ... `RoadToMoscow`), made by `tools/missions.py`.
- `game.scala`: factions, resources, damage dice, the units and their upgrades (`UnitType`: the five factions' boards, then the
  Commander units; new types go at the end of `UnitType.all`, which orders the units), actions and the rules (`Game`). The British,
  Soviet and OKW rules are unit traits (`builtBy`, `tierBuilding`, `strafing`, `captureMode`, `innate`) and the helpers `emplacements`,
  `conversions`, `buildersOf`, `carries`, `captures`, `evacuate`, `tierBuildingOf`.
  The Full Game Rules are `game.full` (the option `FullGameRules`): their state (`opsDeck`, `facing`, `assaults`, `retreating`,
  `splashes`, `shots`, `ruined`, ...), helpers (the "FULL GAME RULES" section: `inArc`, `heavyOptions`, `retreatChoices`,
  `overwatch`, `splashHexes`, `resolveFull`) and their own actions and `case ... if full` branches; the First Game Rules
  paths are untouched, so older games replay the same (checked: games recorded with the code before replay to the same
  final state). New First Game actions keep their old shape; full-only choices are new actions (`SetUpFacingAction`,
  `HeavyMoveAction`, `AssaultAction`, ...). The US Mortar's XP Splash upgrade is a second slot after the Munitions one,
  so recorded slot numbers keep their meaning.
- `commanders.scala`: the Commander cards (`Commander`, their `Tier`s and `Ability`s), the free units, the strikes and the setup
  choice; their rules are in `game.scala`'s "COMMANDERS" section (`unlocked`, `freeEffects`, `commanderSlots`, `commanderPurchases`,
  `openings`, `queueFree`) and the cases after the "COMMANDER PURCHASES" comment.
- `ui.scala`: the map (tokens with health and status, Control Points and their flags, Sandbags, Spawn Areas), panels,
  the Building Boards and unit overlays.
- `bot.scala`: the Easy bot. `host.scala`: headless bot games (`tactics.Host`) and `tactics.Dump`.
- `tools/commander-art.py`: the Commander cards (`commander/`) and the Commander units' tokens, from the Faction Guide's pages.
- `tools/tactics-art.py`: every image in `webp2/tactics/images/` from the mod (`fetch`, then `make`), the Operations cards
  (`ops/`) and the Retreat and HE Splash status icons too; `factions <dir> <out> british soviet okw` makes only those factions'
  boards, tokens (the OKW Tier Buildings and the Flak 37 from `EXTRA`), emblems, flags and Spawn hexes.
- `tools/map-edges.py`, `tools/map-data.py`: read the cover and impassable lines off the full-size panels and write the
  rows of `MoroRiverFirst` (with the corrections made by eye, the water, Buildings, tank traps and Control Points);
  `tools/ardennes-data.py` the same for the Ardennes panels (AF2 | AF1, the features found by their outline colours and checked by eye).
- `tools/missions.py <dir> <out>`: the missions' maps (`map-mr2mr1`, `map-af1af2`, `map-af2af1`; Moro River and Across the Don use
  `map`) and their rows: the panels joined as each mission's bag in the mod's save lays them, the cover lines read by `map-edges.py`,
  the features and corrections of the two hand-read layouts carried over with their panels, the Control Points from the bag's
  resource tokens. Reproduces `MoroRiverFirst` (but the bridge point) and the Frozen Lake rows exactly.

## Map data

The two panels are 6000x9000 each; joined (12000x9000) the hex centers are at `x = 926 + 923 * col + 461.5 * (odd row)`,
`y = 741 + 751.5 * row` in the original grid (`map-edges.py`), a fifth of that on `map.webp`. In `Board`, row 0 is the
original row -1 (the half row at the top) and column 0 the original column -1. When AF1 or MR2 is the left panel, its hexes
sit half a hex further left: `X0` 464.5 (`map-edges.py`'s fourth argument), `x0` 92.9 in `MapData`, and the rows with an even
number keep their half-hex shift. The missions' Control Points are the bags' resource tokens: a bag's panel (a `Custom_Tile`
26.35 units wide, 39.5 tall) is 6000x9000 pixels, and each pair of tokens is a Control Point at their midpoint; they match the
booklet's pictures (pages 9 and 10). Moro River's first rows had the bridge point a hex off (I4, not I5).

## Testing

- JVM: the same project as Pincer's (see `pincer/HANDOFF.md`), with `tactics/` linked in.
  `SBT_OPTS="-Xmx6G -Xss4M" sbt "runMain tactics.Host"` (`TACTICS_GAMES=n`): on 2026-11-23 two Easy bots finished games in
  about seven rounds, every action written and parsed back. `tactics.Dump <steps> [round]` prints a game's moves (as the
  browser records them, a Milestone's action included) and its final state, stopping at that round's damage assignment if
  given; `tactics.Replay <file>` replays such moves as a browser loading the game does, validating each one, and prints the
  final state as `Dump` does, to compare. `TACTICS_RULES=first` makes all three play the First Game Rules (else Full).
  On 2026-11-24 Full Game bot games finished in 6 to 8 rounds with every action written and parsed back, their dumps
  replayed valid, and four First Game games recorded with the code before the Full Game Rules replayed to the same final
  state; with the Commanders, every pairing of the two factions' cards played through (each card's tiers unlocked), and the
  games recorded before them still replay
  state. With the missions (2026-11-24): bot games finished on all six maps (`TACTICS_MAP=<id>`), the old games replayed to the same
  state, and a scenario run checked each map's stockpile, 12 Control Points (6 VP), Spawn Areas, that every Control Point and the
  other Spawn Area can be reached, Ice and the Cathedral. With the British, Soviet and OKW (2026-11-24): bot games finished for each
  kind of pairing (`TACTICS_FACTIONS=BR,OKW`; US, WM, BR, SU, OKW), with trucks converted, Emplacements and Flak Guns built,
  flamethrower Carriers, Strafing Fire, the T-70's mode and Heroic Sacrifice taken; scenario checks for each; the US and Wehrmacht
  games recorded before replay unchanged. With their eighteen Commanders: every ability unlocked in test games (`LogDump` with `PAIR=<n>`
  forcing each card and `XP=25`), scenario checks for the rarer ones (Ramming, Thorough Salvage, Hold the Line, Crocodile, Thompsons,
  the Emplacements, Assault Tactics' income), the older games still replaying unchanged. sbt can't fetch from Maven Central here at times (HTTP 429): `~/.sbt/repositories` pointing at Google's mirror
  (`https://maven-central.storage-download.googleapis.com/maven2/`) with `-Dsbt.override.build.repos=true` works.
- Browser: see "Browser driving" in `nort/HANDOFF.md`. A dump goes into `localStorage` as `tactics.hotseat.game.<time>`
  ("version ...", "title ...", "seating US WM", "options ") and `tactics.hotseat.moves.<time>`, then Local Game opens it.
  On 2026-11-24 a Quick Game (setup, moves, purchases) and a dumped game at a round 4 Damage Phase (assignments, the rolls,
  a unit destroyed, Sandbags lost, capture) played through in Chromium with no errors.

## Next

- The Lost Brigades and the expansions' Commanders (ECC, StuG, ERP: the Faction Guide lists them), and the buildable barbed wire and
  tank traps.
- Full Game readings to confirm with the team (RULES.md, Interpretations 7 to 12): the Firing Arc's width, Retreat paths,
  Overwatch on a hex passed through.
- 2 vs 2 team games and the Mission Booklet's other missions; the Lost Brigades and terrain packs' missions (4.1 ...)
  need the packs' panels and Bunkers.
- A better bot.
