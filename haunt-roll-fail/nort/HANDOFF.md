# Northgard handoff notes

Notes for the next agent working on Northgard: Uncharted Lands (package
`nort`). Read this file, then `RULES.md` (the rules summary and status table),
then the Northgard paragraph in the top-level `CLAUDE.md`.

State on 2026-10-03 (third session): the base game is playable end to end,
with every card effect and all seven clan powers, and was checked against the
English core rulebook (the owner shared the rulebook PDFs in a Dropbox
folder; `RULES.md` lists what that check fixed). The Creatures module and its
More Creatures variant are done (`creatures.scala`), on the branch
`claude/nort-creatures`.

## What exists

| File | What it holds |
|---|---|
| `meta.scala` | Clans (picked by name, with their three clan cards shown), 2–5 players, which options go on which setup page, the asset lists (cards, tiles, units, buildings, `ui-` markers), `underConstruction = true` |
| `options.scala` | Setup options: `ColorOption` per clan, `YearsOption` (5–10, default 7), `FameOnly`, `FirstSeatStarts`; `Module` and `ModuleOption` for modules and expansions |
| `game.scala` | Factions, player colors by seat, resources, `FactionState`, `Game` (map state and helpers), `CommonExpansion` (setup, decks, the year loop, harvest, winter, end of year, scoring), `Debug.summary` |
| `cards.scala` | Every core card (name, fame, Flash, text, image) and its `Effect`; `MoveSpecial` / `BuildSpecial` mark Move and Build cards with extra rules |
| `effects.scala` | `CardsExpansion`: the card effects that aren't basic actions (recruit per resource, removing enemy units, copying cards, looking at hands, Defensive Strategy, ...) |
| `tiles.scala` | All 35 core tiles as data: areas, the sides each area owns, resources, lairs, building spaces, and borders (regular or rough) |
| `board.scala` | `Board`: placements, joining areas into territories, adjacency, closed/open, legal placements (`consistent`), drawing positions |
| `map.scala` | `MapExpansion`: setup tile/unit placement, Recruit (with Kaija), Move (with the Move specials), combat and retreat, Scorched Earth, Explore, Build (with the Build specials), Feast, units returning at end of year |
| `ui.scala` | Status panels (clan names in the player's color), the court strip on top (cards keep the pane's height and the strip scrolls sideways; see `styles.strip`), the board canvas, map clicks. Your hand is in the action pane, as in Arcs and Root: on your turn each card is a `CardMenuAction` (Soft) that opens Play / Wait / Replace / Remove / Upgrade; otherwise `Game.info` shows it as `CardInfoAction` pictures, with the Played cards |
| `creatures.scala` | Creatures module: creature kinds and cards, setup deck, apparition on lairs (`TilePlacedAction`), the Creature phase (`CreaturePhaseAction`), declaring and fighting creatures (`MoveEndAction`, `CreatureFightAction`), the More Creatures variant (`PassedAction`) |
| `bot.scala` | `BotXX`: random, with a bias to play cards and never cancel |
| `host.scala` | Headless bot games on the JVM; prints a summary per game, with event counts (cards played, Kaija and Scorched Earth events) |
| `RULES.md` | Rules summary from the rulebook, plus the status table |

Images are in `webp2/nort/images/`: `card/`, `tile/` (`start`, `start-5`,
`tile-01` to `tile-33`), `token/` (units by color, buildings, other tokens),
`ui/` (generated number badges, spot frames, target ring) and `expansion/`
(Wastelands/Uncharted Horizons tiles, New Blood clan boards, extra tokens; not
used yet).

## The map model (read before changing board code)

- Every border on a core tile ends at a tile corner, so each tile side belongs
  to exactly one area (`AreaSpec.edges`). Areas on neighbouring tiles that
  meet across a side join into one territory (union-find in `Board.compute`).
- A placement is legal when, after it, every border of every tile still has
  different territories on its two sides (`Board.consistent`). That is the
  rulebook's "borders must not stop in the middle". New tiles must also touch
  a placed tile and must not join two players' units.
- A territory is closed when every side owned by its areas has a tile next to
  it. Fame for 2 tiles is 1, for 3 or more tiles 2 (harvest); closing by
  exploring gives the number of tiles.
- Rotation `r` is quarter turns clockwise. `Side.rotate` and `Board.rotate`
  (the drawing positions) must stay in step.
- Territories have no names. They are numbered in `Board.territories` order
  (`Board.label`), and the map draws those numbers. Actions store an
  `AreaRef` (the territory's first area), which stays valid until a tile is placed.
- Units are stored per area (`Game.units`) and counted per territory, so
  merging territories needs no re-keying. Buildings are stored per space
  (`SpaceRef`).
- Tile positions in `tiles.scala` were read from the art by eye, then checked
  with overlays. If a building token or unit marker sits off its space, fix
  the numbers there.
  Commit `3760f8c7` (another session) already moved the label points of
  13 tiles inside their areas; tile-01's east area had been drawn in the
  wrong territory.

## Setup screen and options

- The clan picker shows each clan by name (no color) with its initial clan
  card and two upgrades; clans already picked show only their round emblem
  (`webp2/nort/images/clan/`, cut from the New Blood clan boards). The images
  come from `Meta.menuImages`, a hook added to the framework's `MetaBase`,
  because the game's assets aren't loaded yet in the menus. Picked clans use
  another hook, `factionChosenElem`.
- The setup screen (one page): each clan's row has Human/Bot and a color
  button; clicking the color cycles it, and a clan that had the new color
  takes the old one. This is the framework's `factionRowOptions` /
  `factionRowClick` hook; the colors are still `ColorOption` game options,
  just not listed with the others. A clan left without a color (only possible
  from old saved settings) gets a free one in seating order.
- Below that: game length (the Development deck scales: one card per player
  per year except the last, about a third Early, so 7 years is 2 + 4 and
  10 years 3 + 6 as in the rulebook), Fame victory only, First seat goes
  first, and the modules.
- In the game, clan names are still drawn in the placeholder clan colors
  (`styles.scala`) next to the player's color.

## Modules (groundwork)

`Module` in `options.scala` lists Creatures, Warchiefs, Wilderness,
Wastelands, New Blood, Uncharted Horizons and the 2v2 Teams variant. Each has
a `ModuleOption`, shown on setup page 2 as "coming later" and impossible to
turn on while `ready` is false. `game.modules` / `game.has(m)` tell which are
on. To implement one:

1. Set `ready = true` and give it an `expansion`. `Game.expansions` puts
   module expansions before `MapExpansion` and `CommonExpansion`, so a module
   can handle any core action first (return `UnknownContinue` to let the core
   handle it) or add its own actions.
2. Add its assets to `Meta.assets` with `has(options, module)` as the
   condition (done for the creature cards in `card/creature/`).
3. New Blood's clans go in `Meta.factions` (and `ColorOption.all` follows).
   Clans are picked before options, so check "New Blood clan without the
   New Blood module" in `validateFactionSeatingOptions`.
4. Write the rules summary in `RULES.md` first; the rulebooks aren't in the repo.

## Creatures module

- State is in `Game`: `creatureDeck`, `creatureDiscard`, `creatureLine` (the
  creatures on the map, in activation order), `creatureAt` (the area each one
  is in: the lair area, or a territory anchor after moving) and
  `creatureFights` (declared for the current Move). Helpers: `creaturesIn`,
  `hostileIn` (a Fallen Valkyrie), `bearIn`, `wolfIn`, `harvest` (`produce`
  without tile resources under a Wolf).
- The core code calls hooks that do nothing without the module:
  `TilePlacedAction` after setup, Explore and second chance tiles,
  `MoveEndAction` after the moves of a Move action, `CreaturePhaseAction`
  between Actions and Harvest, `PassedAction` after passing. The core's
  `CombatsAction` and retreats already know about `creatureFights`, and the
  recruit/move/build/explore filters about Brown Bears and Valkyries.
- `ShuffledTilesAction` is intercepted to shuffle the creature deck first;
  the last step calls `MapExpansion.perform` directly (an oracle action can't
  go through `Then`).
- Creature figures on the map are round tokens cut from the card art
  (`token/creature/`, ring color = the card's color code), drawn above the
  territory number. The creature line is a group in the court strip on top.
- `NORT_CREATURES=1` makes the headless host always use the module.

## Known simplifications and gaps

The status table and the Interpretations section in `RULES.md` are the full
list. In short:

- **Rule choices** for Kaija, Scorched Earth and many cards are listed under
  Interpretations in `RULES.md`; check them against the rulebook.
- **Scout Camp:** the redraw happens before the tile is shown on the map.
- **Defensive Strategy** prompts show who holds the card.
- **Missing assets:** one Advanced development card (51 of 52 in the data and
  images; nobody knows which), and green starting cards (green uses blue's).
- **Bots:** random (they now favour upgrading). Fine for testing, not for play.
- **Big territories** show their number on every area, but units are drawn
  only at the first area.

## What the 2026-10-03 playtest found

- All 35 tile overlays were checked (`tiles.scala` positions: number `x, y`,
  units `ux, uy`, building spaces). Many numbers or unit markers sat on a
  border or off the tile; tile-33's north number was in the wrong area. Fixed.
- A 3-player Local Game (Stag against two bots) through year 2, and a
  Bear/Snake game: tile placement and rotation, unit and building positions,
  fights (food, dice, towers, both sides wiped out), retreats, closing by
  exploring (+Stag bonus), harvest and winter matched the rules by hand.
- Bug fixed: the Unrest card's image name, which froze the client when a
  player took Unrest (`asset unrest not found`).

## Suggested next steps, in order

1. **Check the Interpretations in `RULES.md` with the owner** (they have the
   rulebook) and adjust.
2. **Board UI polish:** clicking anywhere in a territory; a preview of the
   tile at the chosen spot while picking a rotation; showing the explore tile
   on the map; a phone-size check; the very long separator lines in the log.
3. **A better bot** that recruits, builds and explores on purpose.
4. **A replay check** like `root/replay-check.scala`, to confirm undo and
   loading rebuild the same game.
5. **Expansions** (after the base game is solid): assets are in
   `expansion/`; the rulebooks are summarized nowhere yet.

## How to build and test (in a cloud session)

- **sbt isn't installed.** Download it:
  `curl -sSL -o sbt.tgz https://github.com/sbt/sbt/releases/download/v1.11.2/sbt-1.11.2.tgz && tar xzf sbt.tgz`.
  Then `cd scala-js-dom-reduced && sbt publishLocal` once. Maven Central
  sometimes answers 429 (rate limited); retry after a minute.
- **Client:** `cd haunt-roll-fail && SBT_OPTS="-Xmx6G -Xss4M" sbt -batch fullOptJS`
  (about 2–4 minutes). Commit only `hrf-opt.js`, `hrf-opt.js.map`,
  `hrf-opt/main.js` and `hrf-opt/main.js.map` from `target/`; restore or
  delete the rest of the churn (including `project/` and
  `scala-js-dom-reduced/` target files). Don't run `sbt clean`: it deletes
  committed `target/` files.
- **Bot games on the JVM:** make a separate project directory with
  `common.sbt` (change `%%%` to `%%`) plus `host.xsbt` as `build.sbt`,
  symlink the sources, then `sbt "runMain nort.Host"`. It plays 20 games with
  2–5 players and random colors, game lengths and victory options, and checks
  that every action and option serializes and parses back. Leave
  out `vast/host.scala`, which doesn't compile. Problems show as
  `UNMATCHING WRITE/PARSE` in the output or as `nort/game-error-*.txt` files
  (delete them, don't commit them). With `NORT_UPGRADES=1` the clan upgrade
  cards start in every deck, so their effects get played (bots rarely have
  3 lore); run both ways. The summary lines list how often each card and
  power event happened.
- **Tile overlays:** to check tile data against the art, draw each area's
  number point, unit point and spaces on the tile image (Pillow:
  `pip install pillow`) with a 0.1 grid; that's how the 2026-10-03 fixes
  were made. The unit points (`ux`, `uy`) were then placed by a search that
  keeps the full size unit figure (300 px of a 948 px tile, with its count
  and Kaija inside its outline) clear of the resource icons (found on the
  art: they have a white outline), the building spaces, the territory
  numbers and the tile's other figures, as close as possible to the old
  point and inside the area; check a changed tile the same way. In a fight
  the clans' figures share that spot at a smaller size.
- **Browser driving:** a small Node server around Playwright that keeps one
  page open and takes `goto`/`click`/`text`/`screenshot` commands over HTTP
  makes step-by-step play from the shell practical. Option buttons with
  images (tiles, cards) have no text; click them by position.
- **Browser:** serve `/play` (from `index.html` with
  `<base href="http://localhost:PORT/hrf/"/>`) and `/hrf/*` (files under
  `haunt-roll-fail/`) from one small local server, and open it on
  `localhost`, not `127.0.0.1`. Playwright is installed globally
  (`/opt/node22/lib/node_modules/playwright`). The live site can't be opened
  from the sandbox browser because of the proxy's certificate; check it with
  `curl -H "Referer: https://games.clean5110.com/play"` instead.
- **Deploy:** merge into `main`, rebuild if `hrf-opt.js` conflicts (it
  always does when both sides changed client code), then
  `git push origin HEAD:main` and `git push origin HEAD:refs/heads/deploy`.
  The server picks it up within a couple of minutes. Get the owner's OK first.

## Code gotchas

- The colmat shorthands (`./`, `.%`, `./~`) have overloads for lists of
  pairs, so `l./(_._1)` on a `$[(A, B)]` doesn't compile. Use `.map`,
  `.filter`, or a two-argument lambda.
- `Die` and `Info` already exist in the framework; the Northgard die is
  `NorthgardDie`.
- Actions marked `Soft` must only return an `Ask` (see `CLAUDE.md`). In
  `map.scala` the soft ones are the tile, spot, move-from and move-to choices.
- Every expansion's `perform` must end with `case _ => UnknownContinue`.
- Everything inside an action must serialize: case classes and objects that
  extend `Record` (effects, `AreaRef`, `Spot`, `DieFace`, `Cap`, `Figures`).
  Tuples don't: use a small `Record` case class instead.
- A card's effect is resolved through `ResolveEffectAction`, so copied
  effects (Enemy Secrets, Stolen Lore, Legendary Heroes) work like played
  ones. Effects that ask for a choice must check for an empty list first
  (Stolen Lore crashed a bot game when the token move left nothing to copy).
- `pkill -f` with a pattern that appears in the same shell command line
  kills that shell too.

## Owner context

- The owner plays on a physical copy and on Tabletopia, and has supplied the
  rulebooks (core, Wilderness, Wastelands, New Blood, Uncharted Horizons) as
  uploads. The PDFs aren't in the repo, so `RULES.md` is the record.
- The repository is a public fork, so the images in it are public. The owner
  was told and went ahead.
- The owner asks to "merge and deploy" when they want things live.
