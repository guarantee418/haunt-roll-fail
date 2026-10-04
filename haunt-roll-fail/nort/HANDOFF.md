# Northgard handoff notes

Notes for the next agent working on Northgard: Uncharted Lands (package
`nort`). Read this file, then `RULES.md` (the rules summary and status table),
then the Northgard paragraph in the top-level `CLAUDE.md`.

State on 2026-10-03 (third session): the base game is playable end to end,
with every card effect and all seven clan powers, and was checked against the
English core rulebook (the owner shared the rulebook PDFs in a Dropbox
folder; `RULES.md` lists what that check fixed). The Creatures module and its
More Creatures variant are done (`creatures.scala`, merged and deployed).
The Warchiefs module is done (`warchiefs.scala`, branch
`claude/nort-warchiefs`), from the Warchiefs expansion rulebook found online
(`Warchief_Expansion_rules_EN_light.pdf`); its 7 extra clan upgrade cards
(Egil's Fury, ...) were found in a Tabletop Simulator mod and are in the game
(`MoveSpecial`s `EgilMove` ... `SvarnMove` in `cards.scala`, effects in
`map.scala` and `creatures.scala`). TTS workshop saves can be downloaded
without Steam: `ISteamRemoteStorage/GetPublishedFileDetails` gives the
save's `file_url`; the save is BSON (`pip install pymongo`, `bson.decode`),
and each deck's `CustomDeck` has the `FaceURL` of its card sheet.

## What exists

| File | What it holds |
|---|---|
| `meta.scala` | Clans (picked by name, with their three clan cards shown), 2–6 players, which options go on which setup page, the asset lists (cards, tiles, units, buildings, `ui-` markers) |
| `options.scala` | Setup options: `ColorOption` per clan, `YearsOption` (5–10, default 7), `FameOnly`, `FirstSeatStarts`; `Module` and `ModuleOption` for modules and expansions, including the team variants `TeamsVariant` (2v2) and `Teams3v3` |
| `game.scala` | Factions, player colors by seat, resources, `FactionState`, `Game` (map state and helpers), `CommonExpansion` (setup, decks, the year loop, harvest, winter, end of year, scoring), `Debug.summary` |
| `cards.scala` | Every core card (name, fame, Flash, text, image) and its `Effect`; `MoveSpecial` / `BuildSpecial` mark Move and Build cards with extra rules |
| `effects.scala` | `CardsExpansion`: the card effects that aren't basic actions (recruit per resource, removing enemy units, copying cards, looking at hands, Defensive Strategy, ...) |
| `tiles.scala` | All 35 core tiles as data: areas, the sides each area owns, resources, lairs, building spaces, and borders (regular or rough) |
| `board.scala` | `Board`: placements, joining areas into territories, adjacency, closed/open, legal placements (`consistent`), drawing positions |
| `map.scala` | `MapExpansion`: setup tile/unit placement, Recruit (with Kaija), Move (with the Move specials), combat and retreat, Scorched Earth, Explore, Build (with the Build specials; `buildOptions` gives each building's free spaces, one per kind, and `buildChoice` asks for the space with `BuildSlotAction` (Soft) and `BuildSpaceAction`, ringed on the map, when there is more than one kind), Feast, units returning at end of year |
| `ui.scala` | Status panels (clan names in the player's color), the court strip on top (cards keep the pane's height and the strip scrolls sideways; see `styles.strip`), the board canvas, map clicks. Your hand is in the action pane, as in Arcs and Root: on your turn each card is a `CardMenuAction` (Soft) that selects it in place (`CardSelectedAction`, tap again for full screen; the other cards become `CardSwitchAction`, `NoExplode` so explode doesn't walk every order of the hand) with Play / Wait / Replace / Remove / Lore Tree below (Lore Tree: `LoreTreeAction` shows the clan upgrades not bought yet as pictures, `LoreTreeInfoAction` ones to view full screen without 3 Lore; a picked one opens a confirmation with the card large, a full screen button and `UpgradeCardAction` to wait with or remove the selected card; `LoreTreeBackAction` goes back; `Game.info` also lists the clan upgrades not bought yet under "Lore Tree (n Lore)" at all times, whatever Lore the player has, except while the Lore Tree itself is open; below it the player's clan board, `ClanBoardInfoAction`, with the clan power, warchief and warchief power, full screen on a tap (`ClanBoard` in `UI.onClick`; the `board-` images load on demand)); otherwise `Game.info` shows it as `CardInfoAction` pictures, with the Played cards |
| `creatures.scala` | Creatures module: creature kinds and cards, setup deck, apparition on lairs (`TilePlacedAction`), the Creature phase (`CreaturePhaseAction`), declaring and fighting creatures (`MoveEndAction`, `CreatureFightAction`), the More Creatures variant (`PassedAction`) |
| `warchiefs.scala` | Warchiefs module: names and powers (`Warchief`), step 1 powers (Signy, Brand), Liv's reroll prompts |
| `wilderness.scala` | Wilderness expansion: `Wild` (where each Environment tile's feature is), `WildernessExpansion` (tile pile setup with the Wyvern's Den, the new creatures' powers, Ancestral Graveyard, Geysers, Poisonous Swamp, harvest extras) |
| `grid.scala` | `TileGrid`, generated by `tools/tile-masks.py`: per tile, 24 x 24 cells with their area and clutter (0-9), used to put warchiefs, Kaija and creatures on open ground |
| `tools/tile-masks.py` | Makes the territory masks (`webp2/nort/images/tile/mask/<tile>-<area>.webp`) and `grid.scala` from the tile art and `tiles.scala`; rerun after changing a tile's areas, number points or spaces |
| `bot.scala` | `BotXX`: random, with a bias to play cards and never cancel |
| `host.scala` | Headless bot games on the JVM; prints a summary per game, with event counts (cards played, Kaija and Scorched Earth events) |
| `RULES.md` | Rules summary from the rulebook, plus the status table |

Images are in `webp2/nort/images/`: `card/`, `tile/` (`start`, `start-5`,
`tile-01` to `tile-33`), `token/` (units by color, buildings, other tokens),
`ui/` (generated number badges, spot frames, target ring) and `expansion/`
(Wastelands/Uncharted Horizons tiles, the clan boards, extra tokens; the
boards are shown by the clan picker's Warchief button).

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
  Options are stored space-separated (saved settings, the online game's
  `options` line), so `Meta.writeOption` writes them without spaces
  (`ColorOption(Bear,Red)`). Before 2026-10-04 the space split each color in
  two and online games lost every chosen color, falling back to seat order;
  `parseOption` drops those halves, so the games made then keep seat colors.
- Below that: game length (the Development deck scales: one card per player
  per year except the last, about a third Early, so 7 years is 2 + 4 and
  10 years 3 + 6 as in the rulebook), Fame victory only, First seat goes
  first, and the modules.
- In the game, clan names are still drawn in the placeholder clan colors
  (`styles.scala`) next to the player's color.

## Six players and teams (2026-10-04)

- Six players (`Meta.maxPlayers`) follow the five-player rules (both
  starting tiles, players 4–6 start with 3 food). The sixth color is
  `Orange`: `unit-orange` and `warchief-orange` were recolored from the
  `-original` figures. Red and orange figures now have the original's white
  outline like the others (the light parts of the `-original` image are kept
  over the recolored one), and `PlayerColor.cards` gives the starting cards' banner
  (orange uses yellow's, green blue's).
- Teams: the `ModuleOption`s for `TeamsVariant` (4 players) and `Teams3v3`
  (6 players) only show for that player count (`Meta.optionsFor`), and the
  wrong count is an error in `validateFactionSeatingOptions`. They have no
  expansion: the rules are in the core code. `game.teams`, `team(f)` (seat
  index mod 2), `allied`, `enemy`, `mates`, `sides`, `teamName`.
- Anything that means "enemy" uses `game.enemy(f, _)` instead of `.but(f)`
  (card effects in `effects.scala`, Defensive Strategy, Scorched Earth).
  `present(t)`, `controlled(f)` and retreats are unchanged: a teammate's
  territory isn't f's and isn't neutral.
- Moving through a teammate's territory: `mateHeld`, `passing(f)` (the
  territories f's figures are passing through), `MapExpansion.canPass`,
  `canEnter` and `destinations` (a bounded search that the figures can get
  out again with the moves left). `MoveDoneAction` isn't offered while
  `passing(f)` is non-empty, and figures passing through move on together.
- Harvest: `TeamTradeAction` (swap one resource for one of a teammate's) in
  `TradeAction`. Scoring: `CommonExpansion.best` and `sides` add up totals
  and tie-breakers per team, in `GameEndAction` and `DominationAction`.
- The status panels show each player's team. `NORT_TEAMS=1` makes the
  headless host always use teams with 4 or 6 players; the summary counts
  `team-pass` and `team-trade` events.
- The 2v2 rules come from a review of the final rulebook (the public
  Kickstarter rulebook has no team variant); 3v3 is not in the rulebook.
  Ask the owner to check them against their rulebook (`RULES.md`, Teams).

## Modules (groundwork)

`Module` in `options.scala` lists Creatures, Warchiefs, Wilderness,
Wastelands, New Blood, Uncharted Horizons and the 2v2 and 3v3 Teams variants. Each has
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

## Warchiefs module

- `game.chiefs` maps a clan to its warchief's area while it is on the map;
  `chiefIn`, `chiefReady`. `figures` counts it, `strength(t, f, attacking)`
  adds 2 or 3 (`Warchief.strength`), `removeFigures` takes it after the
  units. `FactionState.units` includes it (winter, Warlord, ties).
- Card effects on enemy units use `count` (units only), so they can't touch
  warchiefs.
- `MoveUnitsAction` and `HiddenUnitsAction` gained a `chief` field; each has
  a second constructor with the old arity, because saved games are parsed
  by constructor arity (`Serialize`). Do the same whenever a field is added
  to an action that is already in saved games.
- Combat: `ChiefStepOneAction` (Signy, Brand) runs before food, for player
  and creature fights; Liv's reroll is offered in `CombatRolledAction` and
  `CreaturePlayerRolledAction`.
- Clan picker: the framework hook `MetaGame.factionInfo` (label, title,
  contents) adds a button next to each faction in the "Play as" list
  (`hrf.scala`); Northgard's shows the clan board
  (`expansion/board/<clan>.webp`, through `menuImages`) and the warchief
  power. The warchief figures are `token/unit/warchief-<color>`.
- `NORT_WARCHIEFS=1` makes the headless host always use the module.

## Wilderness expansion (2026-10-04)

- Rules from the English rulebook found online
  (`https://tesera.ru/images/items/2181167/Northgard_Wilderness_Expansion_rules_EN.pdf`);
  summary and choices in `RULES.md` (Wilderness, and Interpretations).
- One option, `ModuleOption(Wilderness)`: the Environment tiles, plus with
  the Creatures module the five new deck creatures (`Creature.wild`), the
  Ancestral Graveyard (`Creature.spectral`, `game.spectrals`) and the
  Wyvern's Den (`Creature.wyvern`). `Module.priority` puts
  `WildernessExpansion` before `CreaturesExpansion`, so it can take
  `CreaturePhaseAction`, `CreatureEffectAction` and `TilePlacedAction`
  first and hand over by calling `CreaturesExpansion.perform` directly (as
  `CreaturesExpansion` does with `MapExpansion`). `EndOfYearAction` (Geysers)
  and `WinterAction` (Poisonous Swamp) hand over to `CommonExpansion` the
  same way.
- Tiles are `Tiles.environment` (ids `wild-*`, images
  `tile/wild-*.webp`, copies of `expansion/tile/tile-06` ... `-28`, matched to
  the TTS tiles). `BorderSpec.impassable` (`wall(a, b)`) is a border
  `Board.adjacent` ignores but `Board.consistent` still checks. Areas with no
  edges (`""` in `area`) are enclosed: the Wyvern's Den (`d`, always closed,
  one tile) and the Swamp (`m`). The Swamp's and the Lake's tile sides are
  thin strips that join the neighbours' territories.
- The Swamp reuses the team pass-through code: `game.passOnly(f, t)` is
  "a teammate's territory or the Swamp", `game.passing(f)` the territories
  f's figures pass through. `MoveAction` now offers only those while there
  are any (this also stops a 2v2 move from stranding figures in a
  teammate's territory).
- Spectral Warriors: `game.working(t)` is the buildings that have an effect
  (none with a Spectral Warrior); use it, not `buildingsIn`, for anything a
  building does.
- New creature images: `card/creature/` and `token/creature/` `draugr-jotunn-1/2`,
  `eldthurs-1/2`, `hvedrung-1`, `spectral-warrior-1/2`, `wyvern-1`, from the
  TTS mod 2847156187's card images (light = beige 1, medium = brown 2; the
  paw ring on the card shows it), tokens cut around each creature's head
  with the beige or brown ring of the core tokens.
- `tools/tile-masks.py` knows the orange impassable lines (`ORANGE`),
  areas without edges, a fifth area (`K-T` in `TileGrid.groups`) and leaves
  the Great Lake's water untinted. Running it on all tiles changes the
  masks and grid of the photo tiles (`start-5`, `tile-31` to `-33`); only the
  `wild-*` outputs were committed.
- `NORT_WILDERNESS=1` makes the headless host always use the module.

## New Blood expansion (2026-10-04)

- Seven clans in `Meta.factions` after the core ones (`NewBlood.clans`). No
  option: `Game.modules` adds `NewBlood` when one of them plays, and
  `NewBloodExpansion` (priority -2, before Wilderness, Creatures and
  Warchiefs) handles them. Rules and choices in `RULES.md` (New Blood, and
  Interpretations).
- Sources: TTS mod 3597126237 ("Northgard: Uncharted Horizons + all DLC
  [ENG]", made from the Tabletopia module): card sheet `deck240` (6x6, the 28
  cards plus three core repeats), all 14 clan boards, the Sacrificial Pyre,
  the Ox tokens (both faces, as `Custom_Token` states) and the High Tide
  token. Images: `card/clan/<clan>-<card>.webp`, `clan/<clan>.webp` (emblems
  cut from the boards like the core ones), `token/ox-<n>` (face up, effect)
  and `token/ox-back-<n>`, `token/high-tide`, `token/pyre` (not drawn yet),
  `token/lynx` (Brundr and Kaelinn: the Lynx emblem; no figure art exists).
  Tabletopia itself only has the Uncharted Horizons rulebook (WIP) with five
  of the clans; no New Blood rulebook was found.
- Companions: Kaija's plumbing is shared. `game.companion(f)` is Kaija
  (Bear), `game.lynx` (Lynx's Brundr and Kaelinn) or `game.brok` (Horse's
  second warchief, Warchiefs module only); `kaijaIn`, `kaijaReady`,
  `setCompanion`, `companionStrength` (2, 1, Brok 2 or 1 next to Eitria) and
  `restrained` (only Kaija can't enter enemy territories). Labels use
  `Companion(f)` and `Party(f, n, companion, chief)` instead of `Figures`.
- State in `Game`: `pyre` (owners of the units on it; `reserve` subtracts
  them), `dragonHarvest`, `tides`, `gear` (tokens on spaces; `buildOptions`
  skips those spaces), `gearPile`, `gearReady`, `gearUsed`, `gearFight`,
  `explored` (Warcraft), `howl`.
- Hooks in the core: `NewBloodExpansion.points`, `casualties`, `ignored` and
  `afterCombat` in `CombatResolveAction`; `surtr` in `MapExpansion.rolled`;
  `explored` and `HorseClosedAction` in `ExploreTurnAction`; `howl` in
  `RetreatToAction`; `GearAskAction` before the food step (only with New
  Blood); `AfterHarvestAction` after the Harvest. Other powers intercept
  core actions (`PlayResolveAction` for Kraken, Ox and Lynx clan cards,
  `ScorchedHarvestAction` for Dragon, `ChiefStepOneAction` for Kàra and
  Andhrimnir, `MoveStartAction` for Eitria and Brok's Precision), some only
  for a side effect before returning `UnknownContinue`.
- The status panes show Dragon's Pyre, Kraken's tokens in reserve and Ox's
  ready and used tokens. The map draws High Tide tokens by the territory
  number, Ox tokens on their spaces, Brundr and Kaelinn as a round token and
  Brok as a second warchief figure.
- `NORT_NEWBLOOD=1` makes the headless host use only the New Blood clans;
  without it all 14 clans are drawn from. With `NORT_UPGRADES=1` every New
  Blood card was played in bot games without errors.
- Not done: the Sacrificial Pyre isn't drawn (only listed in Dragon's pane);
  the clan picker's Warchief button shows the boards, but Brok has no figure
  of his own (he uses the warchief figure).

## Uncharted Horizons: Events and Alternative victory (2026-10-04)

- `horizons.scala`: `EventCard` / `EventsExpansion` (option
  `ModuleOption(EventsModule)`) and `VictoryCard` / `VictoryExpansion`
  (`ModuleOption(VictoryModule)`, with `VictoryModeOption(jarl)`), both
  priority -3 so they act before New Blood. Images in `card/event/`
  (landscape, 640x414) and `card/victory/`, cut from the TTS mod 3597126237
  sheets `deck235`, `deck236`, `deck237`. Rules in `RULES.md`.
- Setup: both intercept `ShuffledAchievementsAction` to shuffle their decks
  first, then go back to it with `game.internalPerform` (an oracle action
  can't go through `Then`).
- Events: `game.event` (this year's), `game.eventDeck`, `game.eventSteps`
  (the steps already resolved this year, so the intercepts of
  `RevealDevelopmentsAction`, `ScorchedHarvestAction` and
  `AfterHarvestAction` run once), and the Harvest changes `harvestSkip`,
  `harvestDouble`, `harvestLessFood` used by `EventsExpansion.harvest` and
  `fameFrom` in the core `HarvestAction`. Winter goes through
  `EventsExpansion.winterCost`; combats call `EventsExpansion.afterCombat`;
  explores call `EventsExpansion.explored`.
- Victory: `game.victory`, `game.progress` (validation counts by `advance`,
  only with the module), checked at `EndOfYearAction` (`AltVictoryAction`).
  `game.domination` is off with the module.
- The card strip shows this year's Event and the next one, and the victory
  cards; the status panes list each victory card (✓ when fulfilled) with the
  counts.
- `NORT_EVENTS=1` and `NORT_VICTORY=1` make the headless host always use the
  modules; the summary counts `event-<id>` and `alt-victory-<mode>`. All 20
  Events came up in bot games; the bots rarely meet victory conditions.

## Map drawing: territory colours and free ground

- Each territory is tinted with the colour of the player who controls it
  (`UI.tintOf`: yellow and green strong, red and blue medium to light, as the
  owner asked), gray once invaded (more than one clan present, or a creature
  fight declared there) and pink (`game.battle`) while its fight is resolved,
  like Root's red cloud. `game.battle` is set when a fight starts
  (`FightStartAction`, `CreatureCombatAction`) and cleared by
  `FightOverAction`, which wraps the fight's continuation, and by
  `CombatsAction`.
- The tints are the area masks in `tile/mask/`, filled with the colour and
  turned with the tile (`UI.tint`, made once per tile, area, turn and colour).
  Units, buildings, numbers and tokens are drawn above them. The masks come
  from the art: the borders are roads with white or yellow dashes, and
  `tools/tile-masks.py` grows each area from its sides, number and building
  spaces by a watershed on the dash density. The resource icons (found by
  their white rim or apple red; hand-placed in `ICONS` for the photo tiles)
  are cut out of the masks, so they show untinted. All 35 tiles were checked
  by eye. Red's tint is a dark red (brownish over the grass) so it stands
  apart from orange.
- Closed territories that give fame at the Harvest (no Wolf creature) and are
  controlled by one player have their border dashes redrawn in that player's
  colour (`UI.lineOf`, opaque). Where two such territories of different
  players meet, the dashes alternate between the two colours; a Rough border
  also gets a solid yellow line on each side of its dashes (white when one of
  the colours is yellow). The dashes are vector data in `nort/lines.scala`
  (each a bent bar: three points of its centre line and its width, ordered
  along the border), generated from the tile art and masks by
  `java haunt-roll-fail/nort/tools/BorderLines.java [--check DIR]` (run from
  the repository root; needs ImageMagick; `--check` draws what it found per
  tile). `UI.lineImage` draws a border once per tile, turn and colours at
  half size. A few junction stubs and dashes on busy art aren't found and
  stay white; the Wilderness walls (orange lines, lake, peaks) aren't
  redrawn.
- Both are player settings under "Interface" (in-game menu or the main
  menu's Settings), like Root's Clearing Rule: `TerritoryColorSetting`
  (Show / Fights Only: just the gray and pink / Hide) and
  `BorderColorSetting` ("Fame Borders": Show / Hide), in `nort/meta.scala`
  (`settingsList`, `settingsDefaults`), read in `UI.drawMap` through
  `callbacks.settings`. They're stored per browser (`nort.settings` in
  localStorage) and redraw the map when the dialog closes.
- Warchiefs are the size of a warrior figure and Kaija's round token matches a
  warrior's height. Both stand a little apart from their clan's figure on open
  ground. Creatures go on the territory's most open ground (`UI.freeSpot`,
  using `TileGrid`). The cost order is: off the map or out of the territory,
  then covering anything drawn (figures, warchiefs, Kaija, buildings,
  territory numbers, other creatures), then resource icons and building
  spaces (clutter 9, cubed), then busy art. Very small territories still get
  crowded.
- The Kaija and Scorched Earth tokens are round with transparent corners.

## Known simplifications and gaps

The status table and the Interpretations section in `RULES.md` are the full
list. In short:

- **Rule choices** for Kaija, Scorched Earth and many cards are listed under
  Interpretations in `RULES.md`; check them against the rulebook.
- **Scout Camp:** the redraw happens before the tile is shown on the map.
- **Defensive Strategy** prompts show who holds the card.
- **Assets checked against Tabletop Simulator (2026-10-04):** every core card,
  all 35 tiles (same orientation) and the tokens were matched by perceptual
  hash against the two TTS mods. TTS saves are fetched as described under the
  Warchiefs module (`GetPublishedFileDetails`, BSON); mod 2847156187 has one
  image per card and 1221x2564 tile textures (front on top, the tile square
  about 1180 px from about (20, 35)); mod 2838546142 has the digital card
  sheets (7x5) and the creature meshes. The missing 52nd
  card was Veiled Threats (Advanced, 2 fame: an opponent randomly discards 1
  card, or draw 1). TTS has green starting cards as photographed scans; ours
  are the blue digital art with the ribbon recoloured to their green. Tiles
  31-33 and start-5 were replaced by the sharper TTS scans, aligned to the old
  crops so the tile data still fits (tile-31's photo was slightly skewed).
  The TTS creature miniatures (OBJ meshes, tinted beige / brown / dark
  brown; Kaija and the Brown Bear share a mesh) were rendered and tried on the
  map; the owner preferred the round tokens cut from the card art, so those
  stay.
  TTS also has the seven warchiefs as standee portraits (Figurine_Custom,
  259x432), not used yet.
- **Missing assets:** orange starting cards (orange is not in the box and uses
  yellow's).
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
5. **Expansions**: Wilderness, New Blood, and Uncharted Horizons' Events and
   Alternative victory modules are done; Wastelands and the rest of
   Uncharted Horizons (Raids, Development cards, Training Fields, solo) are next (assets in `expansion/`; the TTS mod
   3597126237 has the Uncharted Horizons cards and Tabletopia its rulebook).

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
  that every action and option serializes and parses back (with 2–6 players,
  teams half the time with 4 or 6). Leave
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
