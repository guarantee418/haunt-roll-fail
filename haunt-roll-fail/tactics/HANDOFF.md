# Company of Heroes ("Tactics"): handoff

Company of Heroes: The Board Game, 2nd Edition (Bad Crow Games), meta `tactics`, URL `/play/tactics`, started on
2026-11-23 from the core rulebook (RulesPal) and the Tabletop Simulator mod "Company of Heroes - 2nd Edition" (Steam
workshop 2314664989), which has the Building Boards, the maps and the four books (Rulebook, Faction Guide, Mission
Booklet, Solo Rulebook as PDFs in its save). The owner's team playtests it for Bad Crow Games. `RULES.md` has the rules as
played and the choices made.

## State

- Playable: two players, any two of the five core factions, or team games of three to six (3v3 on maps of three panels or more, a faction taking a second seat, "Wehrmacht II"), Allies against Axis, or three on the 3 Way Free-for-all (RULES.md, Team games) (US Forces, Wehrmacht, British Forces, Soviet Forces, Oberkommando West;
  thirty core Commanders, six a faction, and the 44 expansion Commanders of the five factions with the Expansion Commanders option, and the Lost Brigades with theirs), on the Mission Booklet's six 2-panel core missions (the setup's
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
- Tutorial (2026-11-25, "some new players need a tutorial"): "Tutorial" on the Company of Heroes menu (URL `/play/tactics/tutorial`,
  the core `tutorialMenu` Northgard's uses) teaches as the US Forces in two parts (`Tutorial.lessons` in `tutorial.scala`, all the text in
  one place; `Tutorial.groupOf`, the core `tutorialLessonGroup`). First Game Rules: how you win, the map and Control Points, a round,
  Maneuver and CP, Damage, Supply, playing on this site, and what the Full Game Rules add; then a practice game, US Forces against the
  Wehrmacht's Easy bot on Moro River with the First Game Rules, no Commanders, Short (`Tutorial.options`, the hidden `TutorialOption`),
  saved in the browser like the local games (kind `tutorial`). Full Game Rules (asked for the same day): Operations, Retreats, Assaults,
  facing, arcs and Overwatch, HE Splash, Buildings and defenses, Commanders, and where to go next; then its own practice game with the
  Full Game Rules and the Commanders (`Tutorial.fullOptions`, kind `tutorial-full`, URL `/play/tactics/tutorial/tutorial-full`). Each
  part's game is offered after its last lesson (`Tutorial.games`, the core `tutorialGames`, which by default is Northgard's one
  `tutorialGame`). A "Tutorial tip" above the choices explains each kind of choice (`Tutorial.tip`, `preinfo` in `ui.scala`),
  "Hide tips" turns them off (`tactics.tutorial-tips-hidden`), "Tutorial Lessons" under the action pane opens the lessons over the game
  (`TutorialLessonView`). The lessons' resource icons are in `Meta.menuImages` under their asset names (`res:<id>`), so they show in the
  menu as in the game.

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
- `brigades.scala`: the Lost Brigades (`Brigade`, 65 cards, the setup's `LostBrigades` option); their own abilities are in
  `commanders.scala`, their units (Elite Grenadiers, the Command Panthers) in `game.scala`, the cards cut by `tools/commander-art.py`.
  `TACTICS_BRIGADES=1` plays them in `tactics.Host`.
- `commanders.scala`: the Commander cards (`Commander`, their `Tier`s and `Ability`s), the free units, the strikes and the setup
  choice; their rules are in `game.scala`'s "COMMANDERS" section (`unlocked`, `freeEffects`, `commanderSlots`, `commanderPurchases`,
  `openings`, `queueFree`) and the cases after the "COMMANDER PURCHASES" comment.
- `ui.scala`: the map (tokens with health and status, Control Points and their flags, Sandbags, Spawn Areas), panels,
  the Building Boards and unit overlays.
- `tutorial.scala`: the Tutorial (`TutorialOption`, `Tutorial`: the lessons, the practice game, the tips).
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
- Tutorial: `TACTICS_TUTORIAL=1` (`full` for the Full Game Rules one) plays the practice game's seating and options and prints each kind of choice the US Forces were
  offered with a tip ("TIP") or without ("NO TIP"); the bot never sees the soft menus (a unit's moves, the spawn hexes), so those were
  checked in Chromium (2026-11-25: the lessons in the menu, the practice game's setup, Maneuver and unit tips, the lessons overlay).
- Browser: see "Browser driving" in `nort/HANDOFF.md`. A dump goes into `localStorage` as `tactics.hotseat.game.<time>`
  ("version ...", "title ...", "seating US WM", "options ") and `tactics.hotseat.moves.<time>`, then Local Game opens it.
  On 2026-11-24 a Quick Game (setup, moves, purchases) and a dumped game at a round 4 Damage Phase (assignments, the rolls,
  a unit destroyed, Sandbags lost, capture) played through in Chromium with no errors.

## Next

- Full Game readings to confirm with the team (RULES.md, Interpretations 7 to 12): the Firing Arc's width, Retreat paths,
  Overwatch on a hex passed through.
- Missions: every mission of the Mission Booklet is in (`tools/missions.py`, `missions-panels.py`, `missions-lmp.py`,
  `missions-bags.py`, `missions-grid.py`; 2026-11-25), with the optional Modular Bunkers as a setup toggle. A new booklet mission
  goes in `missions-bags.py` (it has a bag) or `missions-grid.py` (read off the booklet's picture with `booklet-tokens.py`: check its
  points against the picture and correct any in `FIX`).
- Setup's map picker (2026-11-25): the Map choices sit in one folder, in sections by the booklet's chapters (`MapChoice.sectionOf`),
  each a tile with a thumbnail (`Meta.optionTile`, `optionTileWide`). After adding or changing a map, run `sbt "runMain tactics.MapDump"`
  into a file and `tools/map-thumbs.py <file> webp2/tactics/images` for its thumbnail.
- Desert Warfare (asked for on 2026-11-27): the source is Bad Crow's "3rd printing balance changes" sheet, a Google Doc the owner shared
  (doc id `1djH2anwCN1RWDKUn64P5bt_xWIY_CyMDHOX1L2gP6v0`). It is public: `https://docs.google.com/document/d/<id>/export?format=txt` gives
  its text and `export?format=zip` its HTML with all 324 pictures; the art scripts number the pictures in document order from 1. Done:
  - the DAK faction (its boards, units, art) and its 12 Commanders (`commanders.scala`, readings in RULES.md);
  - its 17 Lost Brigades (`brigades.scala`, 2026-11-28; cards and tokens by `tools/dak-brigade-art.py`; `TACTICS_BRIGADE=<id>` makes
    `tactics.Host` draw, pick and buy one);
  - setup toggles under "3rd printing" (RULES.md, 3rd printing): the Barricade (`Barricades`), Blitz Cubes (`BlitzCubes`), Planes with
    anti-air (`Planes`, tokens by `tools/plane-art.py`) and the first new Commanders (`ThirdPrintingCommanders`: Breakthrough Offensive,
    Shock Army; and with the DAK's expansion Commanders its StuG Assault Doctrine and StuG Tactics; cards and tokens by
    `tools/third-printing-art.py`, whose `ALL` names every Commander picture). `tactics.Host` takes `TACTICS_BARRICADES=1`,
    `TACTICS_BLITZ=1`, `TACTICS_PLANES=1`, `TACTICS_PRINTING=1`, `TACTICS_COMMANDER=<id>` (draws only that card) and
    `TACTICS_FORCE=<action kinds>` (also `Aircraft`, `Unit:<id>`).
  To do, all on the sheet: the other new Commanders (RULES.md, 3rd printing, lists them) and the core factions' new Lost Brigades (pictures
  69 to 94: US 69-72, Wehrmacht 73-76, Soviet 77-80, OKW 81-85, British 86-94); Troop Transports (boats, cards 133-138); the Black Gold
  and Twin Beaches
  panels (Fortifications, Anchors, Shorelines, permanent barbed wire) and 17 1v1 scenarios. The sheet's 15
  new boards for the five core factions are, compared pixel by pixel on 2026-11-27, the very boards the site already plays (the mod
  had them), and the code follows them, so its "board changes" list needs nothing. The owner's rule (2026-11-27): never replace what
  is played; a printing's changes and new cards go in as setup options, like Northgard's Card edition (so the sheet's new Commanders
  and Lost Brigades for the core factions go behind a "3rd printing" setup toggle, off for older games). 4v4 isn't on the sheet. Keep the
  second seats (`Faction.seconds`) so games made with them load. The Discord LFG (`/coh`) stops at 5 seats and offers no second seats.
- A better bot.
