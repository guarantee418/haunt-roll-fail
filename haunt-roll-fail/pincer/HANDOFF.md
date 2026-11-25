# Pincer: handoff

Pincer: A Gentleman's War (Julian Birke), meta `pincer`, URL `/play/pincer`, started on 2026-11-23 from
the rulebook's second draft and the Tabletop Simulator mod. It is a work in progress that the owner's team
playtests; `RULES.md` has the rules as played and the open questions.

## State

- Playable: 2 to 4 players, Local (hotseat), Quick Game and Play Online, with an Easy bot.
- Done: the board (149 hexes, terrain read off the mod's board image), setup, harvest with the two dice
  (and Founder's Oath), moves (roads, bridges, embarking and disembarking frigates), attacks, captures,
  injuries and revives, Counter-Actions, forts, annexation, building, recruiting, trade (and trading
  companies), war and peace, gifts and returning prisoners, the six Maneuver cards, the 14 Royal Seals
  with their bonuses and steals, surrender, and the 8-seal win.
- Tapping a player panel (not a seal on it) opens the mod's Player Reference card (`card/reference.webp`, `"reference"` in
  `onClick` in `ui.scala`), and the Build menu lists every building's cost above its choices (`BuildCostInfoAction`, from
  `Game.preinfo`, shown by `UI.preinfo`); asked for on 2026-11-24.
- On the main `/play` menu since 2026-11-24 (`HRF.menuMetas`, its card in `topMenu` in `hrf.scala`). No ranking.

## Files

- `board.scala`: the hex grid (`Board`), terrain rows, neighbors, distances, `Hex` (named like `D4`:
  column letter, row number).
- `game.scala`: cards, pieces, seals, actions and the rules (`Game`).
- `ui.scala`: the map (pieces placed by hand in each hex, choices highlighted, taps on hexes and units),
  player panels, the Royal Seals overlay.
- `bot.scala`: the Easy bot (rules of thumb and chance).
- `host.scala`: headless bot games (`pincer.Host`) and `pincer.Dump` (prints one bot game's moves, to load a
  game in mid-play in the browser).
- `tools/pincer-art.py`: every image in `webp2/pincer/images/` from the mod (see its header): board, cards,
  seals, war cards, die faces, the pieces rendered from the mod's 3D meshes, road planks, hex overlays.

## Board data

The board image is 1376x1376; hex centers are `x = 85.5 + 119.5 * (col - (row odd ? 0.5 : 0))`,
`y = 69 + 104 * row`, fitted by a search for the most uniform hexes. The terrain of each hex was read
from the image's median color around the center and checked by eye on an overlay (two cut edge hexes,
A6 and L10, are mountains the colors missed).

## Testing

- JVM: a project with `common.sbt` (`%%%` to `%%`, no game directories) and `host.xsbt`'s exclusions,
  symlinking the engine's sources and `pincer/` (base.scala needs its `arcs` line removed, as that game
  isn't included). `SBT_OPTS="-Xmx6G -Xss4M" sbt "runMain pincer.Host"` (`PINCER_GAMES=n`): on
  2026-11-23 four games of two Easy bots ended with a winner in 1,700 to 3,600 steps, every action
  written and parsed back. With the default 1 GB heap the bots' choice lists run out of memory.
- Browser: see "Browser driving" in `nort/HANDOFF.md`. `pincer.Dump <steps> <players>` prints moves that
  can be put in `localStorage` as `pincer.hotseat.game.<time>` (header: version, title, seating) and
  `pincer.hotseat.moves.<time>` to open a game at that point from Local Game.

## Rulings

The playtest questions answered on 2026-11-23 changed: one unit a hex, moves one step at a time (capturing
along the way, passing through the player's own units), one Counter-Action per unit's action and only
against the active player, cut-off roads, colonies only on hexes without roads at their edges, peace
terms (cards and prisoners) as the only way cards change hands outside trading companies, deposits and
withdrawals on a company's stockpile, fort control and automatic removal of damaged forts, surrender
with too few colony pieces, Retreat and Fortify always asked. Saved games from before don't load.

## Next

- Ask the designer about the open questions in `RULES.md` (Boar's War penalty first).
- The Easy bot plays for the nearest seal since 2026-11-24 (`goals` in `bot.scala`); it finishes its games, but does not plan wars.
- Rulebook diagrams the draft refers to ("fig. x", "pg. X") may change rules when they appear.
