# Company of Heroes ("Tactics"): handoff

Company of Heroes: The Board Game, 2nd Edition (Bad Crow Games), meta `tactics`, URL `/play/tactics`, started on
2026-11-23 from the core rulebook (RulesPal) and the Tabletop Simulator mod "Company of Heroes - 2nd Edition" (Steam
workshop 2314664989), which has the Building Boards, the maps and the four books (Rulebook, Faction Guide, Mission
Booklet, Solo Rulebook as PDFs in its save). The owner's team playtests it for Bad Crow Games. `RULES.md` has the rules as
played and the choices made.

## State

- Playable: two players, US Forces against the Wehrmacht, First Game Rules, on Moro River; Local (hotseat), Quick Game
  and Play Online, with an Easy bot.
- On the main `/play` menu since 2026-11-24 (`HRF.menuMetas`, its card in `topMenu` in `hrf.scala`). Its images are in
  `haunt-roll-fail/webp2/tactics/images/`, which the server serves as `/hrf/webp2/`; they were first committed to the repository root's
  `webp2/`, where the live site couldn't find them (404).
  No ranking.

## Files

- `board.scala`: the Moro River grid (`Board.data`, one line a hex: terrain, the six edges' lines, tank trap, Control
  Point), neighbors, distance, line of sight, the edges a line of fire crosses.
- `game.scala`: factions, resources, damage dice, the 18 units and their upgrades (`UnitType`), actions and the rules (`Game`).
- `ui.scala`: the map (tokens with health and status, Control Points and their flags, Sandbags, Spawn Areas), panels,
  the Building Boards and unit overlays.
- `bot.scala`: the Easy bot. `host.scala`: headless bot games (`tactics.Host`) and `tactics.Dump`.
- `tools/tactics-art.py`: every image in `webp2/tactics/images/` from the mod (`fetch`, then `make`).
- `tools/map-edges.py`, `tools/map-data.py`: read the cover and impassable lines off the full-size panels and write the
  rows of `Board.data` (with the corrections made by eye, the water, Buildings, tank traps and Control Points).

## Map data

The two panels are 6000x9000 each; joined (12000x9000) the hex centers are at `x = 926 + 923 * col + 461.5 * (odd row)`,
`y = 741 + 751.5 * row` in the original grid (`map-edges.py`), a fifth of that on `map.webp`. In `Board`, row 0 is the
original row -1 (the half row at the top) and column 0 the original column -1. Control Point positions were read off the
Mission Booklet's Moro River picture (page 9).

## Testing

- JVM: the same project as Pincer's (see `pincer/HANDOFF.md`), with `tactics/` linked in.
  `SBT_OPTS="-Xmx6G -Xss4M" sbt "runMain tactics.Host"` (`TACTICS_GAMES=n`): on 2026-11-23 two Easy bots finished games in
  about seven rounds, every action written and parsed back. `tactics.Dump <steps> [round]` prints a game's moves (as the
  browser records them, a Milestone's action included) and its final state, stopping at that round's damage assignment if
  given; `tactics.Replay <file>` replays such moves as a browser loading the game does, validating each one.
- Browser: see "Browser driving" in `nort/HANDOFF.md`. A dump goes into `localStorage` as `tactics.hotseat.game.<time>`
  ("version ...", "title ...", "seating US WM", "options ") and `tactics.hotseat.moves.<time>`, then Local Game opens it.
  On 2026-11-24 a Quick Game (setup, moves, purchases) and a dumped game at a round 4 Damage Phase (assignments, the rolls,
  a unit destroyed, Sandbags lost, capture) played through in Chromium with no errors.

## Next

- The Full Game Rules: Operations deck, firing arcs and Heavy Vehicle facing, Overwatch, Retreat, Assault, HE Splash,
  Frontal Armor, destructible Buildings, permanent tank traps and foot bridges on Moro River, barbed wire and tank traps,
  then Commanders (their cards are in the mod) and the other factions (British, Soviet, OKW) and maps.
- 2 vs 2 team games and the other missions of the Mission Booklet.
- A better bot.
