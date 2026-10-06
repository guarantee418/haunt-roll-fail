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
| `options.scala` | Setup options: `ColorOption` per clan, `YearsOption` (5–10, default 7), the Victory conditions (`VictoryChoice`: `StandardVictory`, `FameOnly`, `AltVictoryRandom`, `AltVictoryChosen`; `VictoryModeOption`, `VictoryCardOption`), `FirstSeatStarts`; `Module` and `ModuleOption` for modules and expansions, including the team variants `TeamsVariant` (2v2) and `Teams3v3` |
| `meta.scala` (Quick Game) | `quickMin`/`quickMax` 3 and `quickFactions` the 7 core clans: the main menu's Quick Game is always three core clans with the default (core) options |
| `game.scala` | Factions, player colors by seat, resources, `FactionState`, `Game` (map state and helpers), `CommonExpansion` (setup, decks, the year loop, harvest, winter, end of year, scoring), `Debug.summary` |
| `cards.scala` | Every core card (name, fame, Flash, text, image) and its `Effect`; `MoveSpecial` / `BuildSpecial` mark Move and Build cards with extra rules |
| `effects.scala` | `CardsExpansion`: the card effects that aren't basic actions (recruit per resource, removing enemy units, copying cards, looking at hands, Defensive Strategy, ...) |
| `tiles.scala` | All 35 core tiles as data: areas, the sides each area owns, resources, lairs, building spaces, and borders (regular or rough) |
| `board.scala` | `Board`: placements, joining areas into territories, adjacency, closed/open, legal placements (`consistent`), drawing positions |
| `map.scala` | `MapExpansion`: setup tile/unit placement, Recruit (with Kaija), Move (with the Move specials), combat and retreat, Scorched Earth, Explore, Build (with the Build specials; a Build card's builds go `BuildAction` → `BuildSpotAction` (Soft, a free space, tapped on the map or picked from the list; `buildSpots`) → `BuildPickAction` (Soft, the menu of all eight buildings with their costs, the ones that can't go there dimmed with the reason from `buildBlock`) → `BuildConfirmAction` (the building drawn on its space with a green check mark and a red cross above it, `BuildPreview`; the check mark is the confirm action's map target `ConfirmMark`, the cross cancels through `CancelMark` in `UI.clickable`), which goes on to `BuildPlaceAction`. Explore shows the drawn tile above the list of spots (`ExploreTileInfoAction`), so the player sees it before choosing where it goes. Placing a tile (setup, Explore, a second chance) works the same way: the tile is drawn at its spot (`TilePreview`, the `...TurnAction`s) with blue rotate arrows off its top corners (`ui-rotate-left`/`-right`, the map targets `RotateMark(d)` of the `...RotateAction`s, only when it has more than one legal turn) and the check mark and cross at its bottom, each button just outside its own corner of the tile (the scene margin widens on a side where the tile is at the edge); the buttons in the action pane still work too. The other builds (Halvard's Craft, New Blood clans) still use `buildOptions` and `buildChoice`), Feast, units returning at end of year |
| `ui.scala` | Status panels (clan names in the player's color), the court strip on top (cards keep the pane's height and the strip scrolls sideways; see `styles.strip`), the board canvas, map clicks, the build preview. A turn (`TurnAction` stage 0, `turnModes` in `game.scala`) starts with six choices at the top of the action pane: `TurnModeAction` (Soft) for Play cards (1 + any ⚡), Wait, Replace card for 1 Lore, Remove card for 2 Lore, Upgrade for 3 Lore (dimmed with the reason when not possible; the Lore icon is `ui-lore`), and Pass; the hand is shown below as pictures that open full screen. After a choice (`modeChoices`) the hand is shown as that choice's actions (`PlayCardAction`, `WaitCardAction`, `ReplaceCardAction`, `RemoveCardAction`, all `HandChoice` with the card image): tapping a card does it, cards that can't be used are dimmed with the reason, Cancel goes back. Upgrade first shows the clan upgrades (`UpgradePickAction`), then the hand twice: wait with a card, or remove one (`UpgradeCardAction`). After the main card, `playChoices` offers the Flash cards and End turn. Until the turn ends the hand doesn't open full screen (`HandInfoAction` in `Game.info`); the Played cards, the Lore Tree list and the clan board (`ClanBoardInfoAction`, `ClanBoard` in `UI.onClick`) still do. Otherwise `Game.info` shows the hand as `CardInfoAction` pictures, with the Played cards |
| `creatures.scala` | Creatures module: creature kinds and cards, setup deck, apparition on lairs (`TilePlacedAction`), the Creature phase (`CreaturePhaseAction`), declaring and fighting creatures (`MoveEndAction`, `CreatureFightAction`), the More Creatures variant (`PassedAction`) |
| `warchiefs.scala` | Warchiefs module: names and powers (`Warchief`), step 1 powers (Signy, Brand), Liv's reroll prompts |
| `wilderness.scala` | Wilderness expansion: `Wild` (where each Environment tile's feature is), `WildernessExpansion` (tile pile setup with the Wyvern's Den, the new creatures' powers, Ancestral Graveyard, Geysers, Poisonous Swamp, harvest extras) |
| `grid.scala` | `TileGrid`, generated by `tools/tile-masks.py`: per tile, 24 x 24 cells with their area and clutter (0-9), used to put warchiefs, Kaija and creatures on open ground |
| `tools/tile-masks.py` | Makes the territory masks (`webp2/nort/images/tile/mask/<tile>-<area>.webp`) and `grid.scala` from the tile art and `tiles.scala`; rerun after changing a tile's areas, number points or spaces |
| `bot.scala` | `BotXX`, the "Easy" bot: random, with a bias to play cards and never cancel |
| `bot-hard.scala` | `BotHard`, the "Hard" bot (see Bots below) |
| `host.scala` | Headless bot games on the JVM; prints a summary per game, with event counts (cards played, Kaija and Scorched Earth events, `attack-won`/`attack-lost`); `NORT_HARD`, `NORT_CORE`, `NORT_PLAYERS`, `NORT_TRACE`, `NORT_TIMING` (see Bots below) |
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
- Teams: the `ModuleOption`s for `TeamsVariant` (4 players), `Teams3v3`
  and `Teams2v2v2` (6 players, added 2026-10-05) only show for that player
  count (`Meta.optionsFor`), and the wrong count, or two team variants, is
  an error in `validateFactionSeatingOptions`. They have no expansion: the
  rules are in the core code. `Module.sides` gives each variant's number of
  teams; `game.teams`, `teamCount`, `team(f)` (seat index mod `teamCount`,
  so 2v2v2 pairs seats 1+4, 2+5, 3+6), `allied`, `enemy`, `mates`, `sides`,
  `teamName` (Team A, B, C).
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
- The end screen (`CommonExpansion.victory`, `GameOverWonAction`, 2026-10-05):
  the winners' clan cards (plus their warchief cards with Warchiefs or the
  warchief upgrade cards), "<Clan> tames these lands and triumphs as the
  supreme Jarl" ("tame ... Jarls" for teams), and how they won: for fame, the
  total split into fame tokens, cards, resources and Unrest (`finalFame`,
  `fameWhy`), plus the runner-up or the tie-break that decided it; for three
  territories with large buildings, the year and the territories; for
  Alternative victory, the mode and the cards fulfilled.
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
  power. The warchief figures are `token/unit/chief-<clan>-<color>` (`Warchief.figure`).
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
- Not done: the Sacrificial Pyre isn't drawn (only listed in Dragon's pane).

## Uncharted Horizons: Events and Alternative victory (2026-10-04)

- `horizons.scala`: `EventCard` / `EventsExpansion` (option
  `ModuleOption(EventsModule)`) and `VictoryCard` / `VictoryExpansion`
  (on with `AltVictoryRandom` or `AltVictoryChosen` under "Victory
  conditions", see `Meta.has`; older games have the hidden
  `ModuleOption(VictoryModule)`; mode `VictoryModeOption(jarl)`), both
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
  `game.domination` is off with the module. With `AltVictoryChosen` the
  cards come from the `VictoryCardOption`s instead of the shuffle;
  `Meta.validateVictoryCards` refuses to start unless exactly 1 Map Control
  and 2 Wealth cards (3 with teams) are chosen.
- The card strip shows this year's Event and the next one, and the victory
  cards; the status panes list each victory card (✓ when fulfilled) with the
  counts.
- `NORT_EVENTS=1` and `NORT_VICTORY=1` make the headless host always use the
  modules (Alternative victory with random or chosen cards, half each); the summary counts `event-<id>` and `alt-victory-<mode>`. All 20
  Events came up in bot games; the bots rarely meet victory conditions.

## Uncharted Horizons: the Automa (Solo module, 2026-10-04)

- `automa.scala`: `Automa` is a `Faction` in `Meta.factions` (picked with one
  clan; validation refuses other counts and teams), always a bot through
  the new framework hook `MetaGame.botOnly` (`hrf.scala` sets it to its
  default bot on every setup path). `Game.modules` adds `Solo` when it plays;
  `AutomaExpansion` has priority -5. Option `AutomaLevelOption(1..6)`.
- Its Leaders reuse the warchief and companion plumbing: Leader 1 is
  `game.chiefs(Automa)`, Leader 2 `game.leader2` (the companion); they are
  drawn as the white and black Leader miniatures (`Warchief.figure`,
  `Warchief.leader2`).
- State: `automaDeck`, `automaDiscard`, `automaActions` (drawn this year),
  `automaPlayed`, `automaDrawn`, `automaStart`. It intercepts
  `ShuffledAdvancedAction` (two cards out), `ShuffleStartingDecksAction`,
  `SetupPlaceAction`, `RevealDevelopmentsAction` (its draw), `TurnAction`,
  the combat food and die steps, `RetreatAction`, `TradeAction` and
  `EventStepAction`. Ties left by its priorities are asked of the player
  (`choose`).
- The cards (`AutomaCards.specs`) are transcribed from the Tabletopia
  module's 15 Automa cards (images `card/automa/01`..`15`, and
  `reference-1/2`): `AutomaSpec(flash, pass, first, second, picks)` with
  `AutomaRecruit(n, need, prio)`, `AutomaBuild(large, options)` (each option
  a building, or `SiloOrLodge`, with its priorities), `AutomaExplore(on,
  rotate)`, `AutomaMove1(from, to)`, `AutomaMove2(leader, from, to)`.
  Explore's rotations are measured with `Board.withPlaced`. The card strip
  shows the cards it played this year.
- The main menu has "Solo vs Automa" (framework hook `MetaGame.soloFaction`,
  `soloGame` in `hrf.scala`, URL `/play/nort/solo`): pick a clan, then the
  usual setup screen with the Automa added.
- `NORT_AUTOMA=1` makes the headless host play solo games (random level;
  Creatures on from level 3); the summary counts `automa-<action>`.
- Common code changes: `CommonExpansion.cardFame` (Achievement scoring, used
  at the end and for the Automa's last-year pick), `game.strongholdsToWin`,
  the Start of Year draw and Winter skip the Automa.

## Wastelands expansion (2026-10-05)

- `wastelands.scala`: `Waste` (tile ids, `controller`, `around`), the
  `CentralChoice` options (Standard, Random Wastelands tile, Random from all
  of them, one of the nine, or the Wilderness Great Lake `wild-lake`, which
  then stays out of the Environment tiles and gives its food through
  `wildLakeFood` when Wilderness is off; offered in every game, and
  any choice but Standard turns on `WastelandsExpansion` through `Meta.has`,
  while the Environment tiles and the Wastelands creatures need the module
  itself, `Waste.module`) and `WastelandsExpansion`
  (priority -4: before Events, so its Start of Year steps come before the
  Event's side effects). `game.wasteSteps` guards re-dispatched actions within
  a year (reset when `wasteYear` moves on at `StartYearAction`).
- Setup: `ShuffledTilesAction` is caught once to set `game.central` (and put
  Hrimgandr in the creature line), then re-dispatched; `MapExpansion` places
  `game.central` instead of `start`, and with five players
  `Waste.five(central)` east of it, joining the middle to the east territory
  with `Board.join`. `ShuffledTilesBackAction` adds the seven Environment
  tiles; with Wilderness, `game.environment` holds 12 drawn from both and
  Wilderness shuffles those in.
- Tiles: `Tiles.wastelands` (`waste-*`) and `Tiles.central` (`start-*`), in
  `tiles.scala`. `tile-masks.py` treats them like the Wilderness ones
  (`ORANGE`, `RINGED` for impassable middles left untinted, `NO_ICONS` where
  lava or ice passed for resource icons); `BorderLines.java` has `TEAL` for
  the pale dashes of three central tiles (some of their dashes are still
  missed, so their fame borders are partial).
- Creatures (`creatures.scala`): Rock Golem, Myrkalf, Giant Boar, Kobold,
  Valdemar and Hrimgandr kinds; combat hooks `creatureFace`,
  `creaturePoints`, `creatureIgnored` in `CreatureRolledAction`. Without the
  Creatures module, Hrimgandr's fights go to `CreaturesExpansion` through
  `creatureCombat`.
- Jötunn Blainn: `game.blainn` (owner and area), counted by `figures`,
  `strength` (2), `units` (Winter) and `removeFigures`. He follows the last
  figures leaving his territory (`follows`, on `MoveUnitsAction` and
  `RetreatToAction`), and `normalize` sends him back to the camp when alone.
- Map drawing (`ui.scala`): Blainn as a round token beside his clan's
  figure, or in the middle of the Jötnar Camp while waiting; Naströnd's two
  wood in its middle until taken; the creature strip also shows when only
  Hrimgandr is in the line.
- `NORT_WASTELANDS=1` makes the headless host always use the module. Every
  game gets a random central tile choice; `NORT_CENTRAL=1` never picks the
  standard tile, and `NORT_CENTRAL=<tile id>` always picks that one.

## Uncharted Horizons: Sea module (Raids, 2026-10-05)

- `sea.scala`: `RaidCard` (the 24 cards from the TTS mod 3597126237, images
  `card/raid/`), `Raid` (a Port's card, owner, units on the slots and years),
  `RaidKept` (an action waiting for the next Harvest or Start of Year) and
  `SeaExpansion` (priority -6, before every other module: its Raid phase must
  come before the Creature phase, which Wastelands, Wilderness and Creatures
  catch). Module `Sea` in `options.scala`; game state `raidDeck`, `ports`,
  `raids`, `raidKept`, `raidSteps` (per-year guards), `raidExplore` in `game.scala`.
- Beach tiles: four map cells, `Tiles.beach` (`beach-port`, `beach-sea`,
  `beach-wing-w`, `beach-wing-e`), cut from the rulebook's picture by a
  script (the art is upscaled; the port's top edge was filled in). On
  2026-10-06 the four pieces were put back together, scaled up 1.21 times so
  the wings fill their cells and the dashes match a map tile's, and cut again
  (the top of the filled-in edge cropped off); masks and grid redone. A side
  that no area owns is sea: `TileSpec.areaOn` gives None there, `Board`
  joins nothing across it and `frontier` offers no spot beyond it. The
  wings' areas own no side and are joined to the Port with `Board.join`.
  They are placed when `TilePlacedAction` of a first-round setup tile goes
  through (`beached`), turned so the sea is away from the starting tile.
  `tile-masks.py` handles them (`BEACH`: one area, the land; sea and
  transparent parts left out); `beach-sea` has no area and no grid entry.
- Hooks: `FactionState.units` and `Game.reserve` count the raiders,
  `Game.strength` adds the Port's +1 (`SeaExpansion.defense`, listed by
  `FightPoints` and the combat log), `MapExpansion.destinations` adds
  `sailing` (Port to Port, cost = moves left; `MoveCostLabel` says "by
  sea"), `explorable` takes open neutral territories while `raidExplore`,
  and `BoldMove` gives 2 fame per won combat in `CombatResolveAction` and
  `CreatureRolledAction`.
- The Raid phase is caught at `CreaturePhaseAction`; the Harvest cards at
  `AfterHarvestAction`; Elder's Wisdom's extra card at
  `RevealDevelopmentsAction`.
- Map drawing (`ui.scala`): each Port's Raid card on its sea cell, the
  raiders (with a count) on its left in their first year and on its right
  in the second. The court strip shows the Raid cards on the Ports and the
  kept ones; the player panel counts the units raiding.
- `NORT_SEA=1` makes the headless host always use the module.

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
  also gets a dotted yellow line (dark-rimmed dots) on each side of its dashes
  (white when one of the colours is yellow). The dashes are vector data in `nort/lines.scala`
  (each a bent bar: three points of its centre line and its width, ordered
  along the border), generated from the tile art and masks by
  `java haunt-roll-fail/nort/tools/BorderLines.java [--check DIR]` (run from
  the repository root; needs ImageMagick; `--check` draws what it found per
  tile). `UI.lineImage` draws a border once per tile, turn and colours at
  half size. A few junction stubs and dashes on busy art aren't found and
  stay white; the Wilderness walls (orange lines, lake, peaks) aren't
  redrawn. Walls with a territory on each side (the Peaks, the Poisonous
  Swamp's four corner lines, `start-5-wall`) get solid rails on both sides of
  their orange line when a side is controlled (`UI.wallImage`, yellow, white
  when a side is yellow), traced by the same tool into `BorderLines.walls`.
  The rings around the Wastelands' impassable middles (`RINGED`) have a
  territory on one side only and get none; the Lake's and the Poisonous
  Swamp's middle walls have no line on the art.
- Resource icons: `nort/icons.scala` lists each tile's icons (centre and
  radius), found as the holes in the masks by
  `java haunt-roll-fail/nort/tools/ResourceIcons.java` (the Peaks' wood isn't
  cut from their masks, so the tool adds it by hand). `UI.freeSpot` treats
  them as obstacles (covering one costs more than hanging over another
  territory), and `UI.iconImage` cuts each icon from the tile art (where no
  mask covers it) to draw it again over all the pieces. Rerun the tool after
  regenerating the masks.
- The layout has the map on the left and the player panels, log and action
  pane on the right (the owner's request on 2026-10-05; an earlier session had
  mirrored it with the log on the left). The ultrawide `layout` override puts
  the log on the far right. `layoutKey` was bumped to drop layouts cached in
  the browser.
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

## Bots (2026-10-05)

The setup screen offers "Easy" (`BotXX`, random) and "Hard" (`BotHard` in
`bot-hard.scala`) for every clan; Easy stays the default, and the Automa
always plays its own cards.

- **How Hard decides.** The framework explodes every choice into complete
  actions (which card, which units from where to where, which building on
  which space, which tile at which spot and turn). `HardEvaluation` scores
  each by trying it on the live game for a moment (`trying`/`gain`: units,
  buildings, Kaija, the warchief and the clan's resources are put back right
  after; tiles go through `Board.withPlaced`) and valuing the position, in
  hundredths of a fame point (`value`): each controlled territory's
  resources, closed-territory fame, Altars and Forges over the next (at most
  three) harvests, free building spaces; progress towards three closed
  territories with large buildings (`strongholdValue`); units, Kaija and the
  warchief; resources; the coming Winter (an Unrest card is −5 fame); open
  territories to explore from; and the risk of losing each territory to the
  enemies next to it (`risk`). Opponents' values count against it
  (`total`), the leader's more, so it attacks a clan close to winning.
- **Fights** use the exact odds of the two dice (`HardCombat`: the attacker
  picks "1 point or 1 casualty" before the defender rolls, towers add
  casualties, ties go to the defender), for attacking, food spent, the face
  chosen, and creatures (`creatureWin`).
- **Turns:** each card in hand is valued by what its effect would gain now
  (`effectGain`: the best recruits, moves, build, or a sample of three tiles
  from the pile for Explore); Wait, Replace, Remove and Upgrade are valued
  against that (upgrades nearly always win when 3 lore is there); Pass is
  worth the best Development card less the cards left in hand. Development
  picks count printed fame plus what the card does in the years left;
  Achievements count what they would score now.
- **Strategy notes it follows**, from the BoardGameGeek strategy forum (read
  through the `api.geekdo.com` JSON API, since the site's pages refuse
  scripts): most games end with three closed territories holding large
  buildings, so large spaces and closing territories matter; an attacker
  with equal strength wins about a third of the time, so it attacks with an
  edge or spends food; clan upgrades are almost always worth it; a clan
  short of resources builds a Food Silo or Woodcutter's Lodge first; Forges
  early; an attack waits until the defender has passed unless it is worth a
  lot; creatures are steered towards the other players and away from its own
  units (the "Dealing with Creatures" thread: `creatureSpot`, for a tie in the
  Creature phase, More Creatures and the Ancestral Graveyard). Nothing is
  taken from the Automa's cards (`automa.scala`); the territory valuation
  only covers similar ground to their priority lists (resources, buildings,
  closed territories, spaces, nearby enemies).
- **Not valued yet** (the Easy bot's random choice is used): Liv's reroll,
  Ox's Ancestral Equipment, Kobold and camp trades, Vedrfolnir, Gate of
  Helheim, the Events' unit choices, Bribery, Annexation's order, and the
  Alternative victory cards (it plays for fame and the three territories).
  Any action it fails to value falls back the same way and is logged
  (`HardEvaluation.failed`).
- **Testing:** in the JVM host (see How to build and test), `NORT_HARD=1`
  makes the first seat Hard and the others Easy (wins print as `HARD WON`),
  `NORT_HARD=all` makes every seat Hard; `NORT_CORE=1` plays the core game
  only; `NORT_PLAYERS=n` (with `NORT_BATCH`, `NORT_TIMES`) sets the player
  count and number of games; `NORT_TRACE=1` prints each Hard choice with its
  best alternatives (and the hand's card values before a Pass);
  `NORT_TIMING=1` prints decisions over 300 ms.
- **Results (2026-10-05, JVM host):** Hard in the first seat against Easy bots
  won 25 of 25 three-player core games, 20 of 20 four-player core games, and
  40 of 40 games with random modules and 2–6 players. In all-Hard games the
  attacker won 83% of fights (Easy attackers: 35%). Easy is close to random,
  so this shows a big gap, not how hard Hard is for people. Its slowest
  decisions take about 350 ms on the JVM (choosing a setup tile was 5 s until
  `setupPlacement` stopped valuing the opponents; Explore values only the
  bot's own position for the same reason).

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
  TTS mod 2847156187 also has the seven core warchiefs as standee portraits
  (Figurine_Custom, 259x432 PNGs). They are the map figures of those
  warchiefs (2026-10-05): `nort/tools/warchief-portraits.py` scales each one
  onto a 512x512 canvas with an outline in each player color and a white rim
  (`token/unit/chief-<clan>-<color>.webp`, loaded only for the clans in play),
  and `Warchief.figure` picks the image. New Blood has no standees, so its
  warchiefs (and Brok, `chief-brok-<color>`) are round tokens the size of
  Kaija's: the head from the clan card that shows them, in a ring of the
  player's color (`nort/tools/warchief-heads.py`, crop boxes in the script;
  `Warchief.round`). The Automa's Leaders are its white (Leader 1) and black
  (Leader 2) miniatures, cut from the components photo on page 2 of the
  Uncharted Horizons rulebook PDF in TTS mod 3597126237
  (`nort/tools/automa-leaders.py`, `token/unit/leader-<1|2>-<color>`); Tabletopia
  has no usable images outside a signed-in table. The old warchief figure
  (`warchief-<color>`) is a second warrior design: `Warchief.warrior` picks
  `unit-` or `warchief-` for each stack by a hash of the territory, clan and
  count, so it changes at random when units arrive or leave but is the same
  on every redraw and for every player.
- **Missing assets:** orange starting cards (orange is not in the box and uses
  yellow's).
- **Bots:** "Easy" is random (it favours upgrading); "Hard" plays to win (see Bots below).
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
3. **The Hard bot** (done 2026-10-05, see Bots below): the expansion choices it
   doesn't value yet (Liv's reroll, Ox's tokens, Kobold trades, Vedrfolnir, the
   Events' unit choices, Alternative victory cards) could get their own scores.
4. **A replay check** like `root/replay-check.scala`, to confirm undo and
   loading rebuild the same game.
5. **Expansions**: Wilderness, Wastelands, New Blood, and Uncharted Horizons' Events,
   Alternative victory and Sea modules are done; the rest of
   Uncharted Horizons (Development cards, Training Fields) are next (assets in `expansion/`; the TTS mod
   3597126237 has the Uncharted Horizons cards and its rulebook PDF).

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
