# HRF (haunt-roll-fail) fork

Fork of the HRF board game site (hrf.im), with the Twilight Council and
Knaves of the Deepwood factions (playtest and official Root: Homeland
versions; the Homeland ones are `faction-council.scala` and
`faction-knaves.scala`). Written in Scala:

- `haunt-roll-fail/` — the game client, compiled to JavaScript with Scala.js
  (`target/scala-2.13/hrf-opt.js`, about 8 MB, 2.5 MB gzipped)
- `good-game/` — the Akka HTTP server (`GoodGame.scala`) that serves the client
  and stores games in an HSQLDB database
- `scala-js-dom-reduced/` — DOM library the client depends on
  (`sbt publishLocal` once before building the client)

Northgard: Uncharted Lands is being added in `haunt-roll-fail/nort/`
(meta name `nort`, URL `/play/nort`), base game first, expansions later.
`nort/RULES.md` summarizes the rules (the rulebook PDFs aren't in the repo)
and tracks what is done. `nort/HANDOFF.md` has the state, design notes,
known gaps, next steps and how to build and test it. So far: the 7 clans (14 with New Blood), 2–6 players (six on the five-player rules), team
play with any split into teams on the 2v2 rules ("Free-for-all" or "Teams" above the clan
picker, `Meta.pickerModes`; each player's team on its setup row, `TeamOption`), the year loop with
decks, Wait/Replace/Remove/Upgrade/Pass, Flash cards, harvest trading (every player at once, each until "Done trading", like Root's Advanced Setup discards: `TradePhaseAction`, a `MultiAsk`, in `nort/game.scala`; a player done trading can't be traded with any more; a 1:1 trade with a teammate is an offer the teammate accepts or declines, `TeamOfferAction`, `TeamAcceptAction`, `TeamDeclineAction`, with the pending offers in `game.tradeOffers`: the proposer waits for the answer, a player owing an answer can't make offers, and a declined offer isn't offered again that harvest), winter
and Unrest, end-of-game scoring (the end screen shows the winner's clan card, "tames these lands and triumphs as the supreme Jarl", how they won, and a table of every clan's fame by where it came from, exploring, harvests, fights, creatures, effects, events, trading, raids, then cards, resources, Unrest and the total, the winners' columns in gold: `fameTable` in `nort/game.scala`, the sources noted as fame is gained, `FactionState.addFame` and `game.fameFrom`, so replays of older games show it too; asked for on Discord, 2026-11-21; `nort.Host` prints `FAME UNTRACKED` for fame gained without `addFame`), and the real card list in `nort/cards.scala`
(names, fame, text, images), the map: tile data in `nort/tiles.scala`
(areas, borders, resources, spaces, checked against the art), territories
and placement rules in `nort/board.scala`, and setup, Recruit, Move,
Explore, Build, Feast, combat and retreat in `nort/map.scala`. Every card
effect works (special ones in `nort/effects.scala`) and all seven clan
powers, including Bear's Kaija and Snake's Scorched Earth; the Creatures
module (with its More Creatures variant) is in `nort/creatures.scala`, the
Warchiefs module in `nort/warchiefs.scala` (the clan picker's Warchief
button shows each clan's warchief board), the Wilderness expansion
(Environment tiles with impassable borders, and five more creatures plus
the Ancestral Graveyard's Spectral Warriors and the Wyvern's Den) in
`nort/wilderness.scala` (tiles `wild-*` in `Tiles.environment`), and the
New Blood expansion's seven clans (Dragon, Horse, Kraken, Lynx, Ox, Rat,
Squirrel, with their warchiefs and 28 clan cards, from the TTS mod
3597126237) in `nort/newblood.scala`; picking one of them turns it on.
The fan-made Lion Kingdom (the video game's Kingdom of Neustria, designed here on 2026-10-21 with
Robotos's Chapel and alternative starting deck) is a fifteenth clan in `nort/lion.scala`: Lords on its Defense
Towers (a Lord stays while the Lion has figures in the territory, so it defends it: hidden `LordsWhileContested`, every new game
since a bug report on 2026-10-25; older games sent it back once an attacker moved in), and adds its 2 combat points to every fight in its territory, the Lion attacking or defending, against a clan or a creature (hidden `LordsInEveryFight`, every new game, after a bug report on 2026-10-25; older games count it only defending against a clan), Chapels instead of Carved Stones (1 lore each at harvest in new games, hidden `LionChapelLore`, 2026-10-25; 1 fame in older ones), warchief Hildegard, Faith (a fourth resource only it has: 1 for each casualty the enemy suffers in a fight its Lords or Hildegard join and 1 for each Chapel at harvest, instead of lore; its clan upgrades cost Faith, it trades like the other resources; the clan card becomes Ost without its free tower (flash), the upgrade Paladins takes its place (Recruit 1 and 1 more per Faith paid, up to 4), and Rise of the Twelve Peers builds a free Defense Tower before its Lord; hidden `LionFaith`, every new game, 2026-10-28; `Faith` and `Resource.of` in `nort/game.scala`, `CardVersions.lionFaith`, the icon `ui-faith` from the video game's by `nort/tools/faith-icon.py`; the clan board saying Faith, `expansion/board/lion-faith.webp`, `Warchief.board`, drawn by `nort/tools/lion-art.py board-faith`), the upgrade card Ost (a free Defense Tower, then for the rest of the year the Lords next to a
combat the Lion starts join it; in place of Abundance in every new game, hidden `LionOst`, 2026-10-25; Rise of the Twelve Peers recruits
in each territory with a Chapel and Hildegard's Vow gives 2 combat points for each Chapel next to the fight in new games, hidden
`LionChapelPeers` and `LionChapelVow`), and its own starting deck (the Adventure, Toil and Prey; hidden `LionOwnDeck` that every new game gets,
2026-10-24; it was the setup option "Lion Kingdom: alternative starting deck", `LionStartingDeck`, now hidden); picking it turns it on (it is in the clan picker's "Fan-made clans" section, `factionGroup`; not in Adset or the Random Clan picks); its art is made by `nort/tools/lion-art.py`, with the video game's Lion emblem, Hildegard (also on Hildegard's Vow) and the Lord token's Lord from the Northgard wiki; its warriors are a man-at-arms with a longsword and a longbowman (`lion-sword-`, `lion-bow-`, by `nort/tools/lion-warriors.py`, `Warchief.warrior`; no Norse helmets, as asked).
Uncharted Horizons' Events and Alternative victory conditions modules are in
`nort/horizons.scala`. Its 13 Development cards and 2 Achievements (option
"Uncharted Horizons Development cards", next to "Ban card draw developments")
are in `nort/horizon-devs.scala`, and its 5 map tiles with The Bridge (option
"Map tiles" in the setup's Uncharted Horizons section) are
`Tiles.horizons` (`horizon-*`, made by `nort/tools/horizon-tiles.py`). Its Sea module (Raids: a Beach tile with a Port for each
player, the Raid phase and the 24 Raid cards, from the TTS mod 3597126237,
whose rulebook PDF has the rules) is in `nort/sea.scala`; Port to Port costs 2 moves and the figures may move on with the rest (hidden `SailOnFromPort`, every new game; Discord, 2026-11-08; older games used up every move left, `SailOnFromPortAction(X)` switches a running game); with three players the Beaches are where the rulebook's 3 PLAYER SETUP picture puts them, the third one south under the east first tile (hidden `ThreePlayerBeaches`, every new game; 2026-11-22; `threePlayerBeaches` in `nort/sea.scala`); the Beach is four
map cells (`Tiles.beach`), and a tile side no area owns is sea. The Wastelands expansion (Environment tiles
`waste-*`, the Central tiles `start-*` chosen with the "Central tile" option, which
is offered in every game (standard tile, any of the nine, the Wilderness Great
Lake, or random) and needs
the module only for the rest of Wastelands,
five more creatures, Hrimgandr and Jötunn Blainn) is in `nort/wastelands.scala`. Tamer (Horizons) never moves Hrimgandr in games with
the hidden `TamerSparesHrimgandr` every new game gets (2026-11-21: a bot game's Tamer moved him, and with the Creatures module off the
step after stopped the game; `tamerCreatures` in `nort/horizon-devs.scala`), and his move does nothing in older games.
The solo Automa (Uncharted Horizons' Solo module) is
in `nort/automa.scala`, with its 15 cards transcribed from the Tabletopia
module ("Solo vs Automa" on the main menu). Its Training Fields module, a two-player duel on
a 4x3 grid of face-down tiles with seven Action cards each, is in
`nort/training.scala`: "Training Grounds" on the main menu opens a page
offering a local or an online game (the `modes` hooks in `meta.scala`,
`modeMenu` in `hrf.scala`); its Explore offers a face-down tile beside a territory the player controls, in games with the hidden `TrainingExploreSides` every new Training Grounds game gets (`Meta.modeOptions`; 2026-11-20: the far side of the neighbouring tile was checked, so a tile under the player's territory wasn't offered and one two tiles away was). "Tutorial", first on the main menu (the `tutorialLessons`,
`tutorialAbout` and `tutorialGame` hooks in `meta.scala`, `tutorialMenu` in `hrf.scala`, URL
`/play/nort/tutorial`), teaches the game as the Stag Clan: nine lessons on the rules and on
playing them on this site (`Tutorial.lessons` in `nort/tutorial.scala`, all the text in one place),
in the owner's teaching order (2026-10-10): how you win (stressing the sudden-death win), the map
(territories, open and closed), the deck and the year (one card a turn, passing), the four actions
(lore left for after the first year), harvest and winter (overexpanding flagged), combat (also
explained in the practice game at the first fight), the clan last, the site, the practice game;
then a practice game, Stag (first) against the Wolf Clan's Easy bot, with the hidden
`TutorialOption`: a "Tutorial tip" above the choices explains each one (`Tutorial.tip`, by the
kind of action offered; `preinfo` in `nort/ui.scala`), "Hide tips" turns them off (each browser
remembers it, `nort.tutorial-tips-hidden`) and "Tutorial Lessons" under the action pane opens the
lessons over the game (`TutorialLessonView`). The practice game is saved like the other local
games (kind `tutorial`). "Adset" on the main menu (the `linkedModes` hook) opens
its own meta, `nort-adset` (`MetaAdset` in `nort/adset.scala`, URL `/play/nort-adset`):
a free-for-all whose players are seats (`Seat`, "Player #n") that draft their clans
during setup (random seat order, players + 2 clans drawn, a ban, Wilderness or
Wastelands, the central tile, then picks and placements from the last seat), always
with Creatures and the Horizons Development cards. The first year's Development cards and the Achievements
are dealt before the draft and shown during it, in the action pane (`AdsetCardInfoAction`) and the card strip
(`drawCards` in `nort/ui.scala`; asked for on 2026-10-14). `Game` takes `players` (clans,
or seats in Adset) and maps them with `ptf`/`ftp`, as Root's Advanced Setup does;
asks for a clan go to its seat in `loggedPerform`, and the in-game choices are added
to `game.options` (`addOption`). Solo, Quick and Local (hotseat) games are saved in the
browser's localStorage as they are played (`newLocalGame` and `savedGamesMenu` in
`hrf.scala`, `LocalStorageJournal` in `journal.scala`; keys `<settingsKey>.<kind>.game.<time>`
with kind `solo`, `quick` or `hotseat`; the 12 newest of each kind are kept, and when storage
is full the oldest of any kind make room), so "Quick Game", "Local Game" and "Solo vs Automa"
offer a new game or a saved one to continue (straight to a new one when none is saved),
like Play Online. Training Grounds games aren't saved. Fan-made modules are listed under "Fan content" on the setup
screen (`Module.fan`, also in Adset): the Minerals module by Maciej Drewing (BoardGameGeek file 255809; asked for on
2026-10-21) is in `nort/minerals.scala`: Mines (a small building, `Mine`, not in `Building.all`, only in a territory with a creature
lair) roll the combat die at the harvest for Stone, Iron and Gem (`game.mineralBank`, each clan's `minerals`), each worth two
resources for trading, Winter and the final fame; Stone upgrades a building during a Build (`game.upgraded`, counted twice by
`Game.counted` and `working`, cleared when the space's building changes, `buildings_=`); in games with the hidden `BuildOrUpgrade` every new game gets, a Build builds or upgrades (the Stone upgrade or the PC version's Town Hall), not both (2026-11-16; `game.builtThisBuild`; `BuildOrUpgradeAction(X)` switches a running game), Iron rolls two dice in a fight, Gem takes a
discarded card's fame while playing cards. The player panels show the Mines and minerals (tapping them, the supply and rules). The "Tower PC version" (fan-made, asked for on 2026-10-22, `TowersModule`, also under Fan content) is in `nort/towers.scala`: a Defense Tower costs 2 wood (`TowersExpansion.cost`, in `buildCost`), is destroyed once the clan holding its territory has no figure left there (`sweep`, run by `Game.loggedPerform` before and after each action, `game.towerHolder`), kills 1 attacking unit per tower before each fight instead of adding a casualty after it (`TowerVolleyAction`, before `FightStartAction`; `killFirst`, the hook for any other rule that makes towers fire first), gives 1 combat point per tower against creatures (`towerPoints`), and lets the holder fight a creature that appears or moves into its territory before the creature's ability (`TowerChallengeAction`). Marzaus's "Warchiefs PC version" (asked for on 2026-10-22; `WarchiefsPC` in `nort/options.scala`, needs the warchiefs) makes the clan's warchief (not Kaija, Brok or the Automa's Leaders) cost 2 units, only into a territory with its Training Camp (1 of the Recruit's and the camp's extra unit there, so the starting Recruit 1 brings it, in games with the hidden `PCChiefCampPays` every new game gets; 2026-11-06, couldn't recruit it, as it took 2 of the Recruit's own units; `PCChiefCampPaysAction(X)` switches a running game), never placed at setup, killed only by 2 casualties in one fight (`game.toughness`; a single one left over spares it), passed by every one-figure removal outside a fight, and left out of Winter costs (`FactionState.winterUnits`); see `nort/RULES.md`. Robotos's "Lower skill floor Dragon variant" (Discord northgard-fanmade-content; asked for on 2026-10-24; `DragonVariant` in `nort/options.scala`; since 2026-10-26 picked as its own clan, "Dragon, lower skill floor", in the clan picker's "Fan-made clans" section with the Lion Kingdom, not on the setup screen or in Adset: `factionVariants` and `factionGroup` in `nort/meta.scala`, the `FactionVariant` hook in `meta.scala`, picked in `customGame` and `soloGame` in `hrf.scala`, which pass its option to the setup screen as always on; the option is now hidden and `Meta.retired`, so older games, Adset ones too, still load and log it) replaces the Dragon Clan's power and clan cards: no sacrifice to harvest and no extra food or wood, a reserve unit may go on an empty Pyre spot at the start of the Harvest (`PyreReserveAction`), and Dragon Clan, Enthrall and Reluctant Workforce empty a full Pyre for 1 food, 2 fame or a third card (`CardVersions.dragonVariant` in `nort/editions.scala`, before the other versions; `PyreEmptyAction` and `EnthrallAction` in `nort/newblood.scala`; card pictures by `nort/tools/card-text.py`, the board `expansion/board/dragon-variant.webp` by `nort/tools/board-text.py`). Fan versions show in the clan info only in games that play them (`CardVersions.fan`). Robotos's "Rough Border's Edition" Bear (Discord northgard-fanmade-content; asked for on 2026-10-31; `BearBorder` in `nort/options.scala`, rules in `nort/bear-border.scala`) is a third fan clan in the picker, "Bear Clan (variant)" (`factionVariants`; hidden option, logged, not in Adset): Kaija is a token on a border (`game.kaijaBorder`, drawn on the border's middle dash), placed before a Bear clan card on a regular border of a Bear territory; every clan, Bear too, treats that border as Rough (`Game.links`, used by the moves and retreats), unless it discards a resource for the Move (`KaijaTollAction`) or is Bear after The Bear Awakens; its cards are `CardVersions.bearBorder` (pictures by `nort/tools/card-text.py`, board `expansion/board/bear-border.webp` by `nort/tools/board-text.py`); see `nort/RULES.md`. Marzaus's "Fan creatures" (Discord northgard-fanmade-content; asked for on 2026-10-24; `AggressiveCreatures` in `nort/options.scala`, under Fan content, needs Creatures) takes away every creature's ability (`game.noAbilities`, checked in `bearIn`, `wolfIn`, `koboldIn`, `ghostIn` and the fight and Wastelands hooks) and makes every creature aggressive like the Fallen Valkyrie (`game.aggressive`, used by `hostileIn`): it attacks a clan whose territory it appears or moves into (`AggressiveCreaturesExpansion` in `nort/creatures.scala`), and units must fight a creature whose territory they move into; Hrimgandr still never moves; see `nort/RULES.md`. The Military paths module by Marzaus (Discord
northgard-fanmade-content; asked for on 2026-10-22) is in `nort/military.scala`: MXP (`game.mxp`) from fights: winning one, 1 per enemy figure it began with (`game.fightSize`; 2026-10-23), and won or lost, 2 per enemy
unit killed, 10 per warchief (2026-10-22), creatures (twice their fame) and enemy Defense Towers destroyed (3), and at 5, 50 and 100 MXP the
levels of a path picked at 5 (`game.paths`): Leadership (only with Warchiefs: a bodyguard, `game.guards`, drawn as its own figure (Horse's Eitria and Brok each have one in games with the hidden `HorseTwoGuards` every new game gets, `game.brokGuard`; 2026-11-06; `HorseTwoGuardsAction(X)` switches a running game), a huscarl with a kite shield marked with the Algiz rune and a spear, `token/unit/bodyguard-<color>`, by `nort/tools/bodyguard-figure.py`, asked for on 2026-10-23, rerolls, an extra move,
captures, a kill before attacking), Guardian (free Defense Towers on no space that kill before the fight, `game.towersUsed`; four
Militia, `game.militia`, figures that stay in the clan's territories (drawn as an unarmored peasant with a pitchfork, the bodyguard's size, `token/unit/militia-<color>`, by `nort/tools/militia-figure.py`; 2026-11-06), one dead one back each year, `game.militiaBack`; a free Fortress a year, `freeFortress`: Marzaus's "Palisade" is the Fortress) and
Legions (free Training Camps on no space that recruit 2 and train a unit each year, points for big armies, rerolls). Leadership's extra warchief move after a Move card starts only from a territory the clan controls (not out of a fight or on through an enemy's territory), in games with the hidden `ChiefMoveFromHeld` every new game gets (Discord, 2026-11-04; `ChiefMoveFromHeldAction(X)` switches a running game), and only after the warchief moved with the card (hidden `ChiefMoveAfterMove`, `game.chiefMoved`; 2026-11-04; `ChiefMoveAfterMoveAction(X)`). Leadership's and Legions' reroll of the combat die is offered against creatures too (once per fight, `MilitaryCreatureKeptAction` in `nort/military.scala`), in games with the hidden `MilitaryRerollCreatures` every new game gets (2026-11-04; `MilitaryRerollCreaturesAction(X)` switches a running game). A clan losing one unit outside a fight (Draugr, Valdemar, the Swamp, the Poisonous Swamp, the Volcano, Wyvern, events) with its bodyguard (or Brok's) or a Militia in that territory is asked right after which of them goes, as the bodyguard comes back each year (`noteLoss`, `lossAsk`, `LoneLossKeepAction`, `LoneLossSwapAction` in `nort/military.scala`, asked from `Game.loggedPerform` when the action goes straight on, `game.loneLosses`), in games with the hidden `PickLoneLoss` every new game gets (2026-11-07; `PickLoneLossAction(X)` switches a running game). A clan taking fight casualties where its bodyguard is, with other units there, is asked whether the bodyguard goes first even when it has no other choice to make (`guardChoice` in `nort/map.scala`, `pickCasualties`), in games with the hidden `GuardCasualtyAsked` every new game gets (2026-11-11: with the Warchiefs PC version's 2-casualty warchief he was never asked). A clan picking a fight's casualties loses the warchief only with its bodyguard, and Horse's Brok only with his (`guardsKept` in `CasualtyGoal`, `nort/figures.scala`), in games with the hidden `GuardBeforeChief` every new game gets (2026-11-11: in Saga and Clan Horse won a fight but lost Eitria and a unit, and her bodyguard went to the reserve with her, emptying the territory; `GuardBeforeChiefAction(X)` switches a running game); the Hard bot (and Robotos) values each casualty pick by what the figures lost are worth (a unit or Militia 38, the warchief 70 and the bodyguard it takes along, the bodyguard alone 15, as it comes back each year; `casualtiesGain` in `nort/bot-hard.scala`), where it picked at random before, and counts a casualty it inflicts as worth a unit when picking "1 point or 1 casualty" (`faceChoice`; Boar took the point in a fight it lost anyway, 2026-11-11). The readings of
the short post are listed in `nort/RULES.md` (Military paths); the player panels show the MXP, with the video game's Military Experience icon (crossed swords, `ui-mxp`, `MXP.icon`, from the game wiki's 20x20 file by `nort/tools/mxp-icon.py`; asked for on 2026-10-23), and the path (tapping them, every path). The End of the Year gains (Guardian's towers, Legions' camps, Leadership's units) never came in Wilderness games, as its Geysers skipped the expansions after it, until the hidden `EndOfYearAfterGeysers` every new game gets (`wilderness.scala`; 2026-11-01; `EndOfYearAfterGeysersAction(X)` switches a running game). The
code was checked against the English core rulebook on 2026-10-03; rule
choices the rulebook leaves open are listed under Interpretations in
`nort/RULES.md`.
The setup's "Fan victories" section, right under Fan content (2026-11-12; `FanVictories` and `FanVictory` in `nort/options.scala`: a fan module that is a victory goes in `FanVictories.modules`, a plain option extends `FanVictory`), holds Alternative fame gains and Domination victory; the PC version's trade, knowledge and other victories are to go there.
Marzaus's "Alternative fame gains" (Discord northgard-fanmade-content; asked for on 2026-11-01; `FameGainsModule` in `nort/options.scala`, under Fan victories, rules in `nort/fame-gains.scala`, needs the standard or Fame only victory) gives fame as things are done: 1 or 2 per building placed, 3 more for an Altar of Kings, 1 per enemy unit and 5 per warchief killed and 1 per win in fights against clans (the loser's figures lost with nowhere to retreat count as killed, hidden `FameRetreatKills`, every new game; 2026-11-06), 5 per upgrade, a Development card's fame when taken (none at the end, `cardFame`), and a face-down Achievement each clan keeps of 2 dealt before the first year (`game.fameKept`; none are revealed in the last year), resolved once on its turn for the fame it gives then (`FameResolveAction`); a clan with 120 fame controlling an Altar of Kings (in a closed territory, in games with the hidden `FameAltarClosed` every new game gets; 2026-11-06) wins at once (checked in `Game.loggedPerform`, `FameWinAction`; `NORT_FAME_GOAL` lowers it in bot games), the only way to win: three closed territories with large buildings don't, and after the last year the game goes on year after year until someone does (2026-11-02; `NORT_FAME_YEARS` caps it in bot games); readings in `nort/RULES.md`; `FameGainsOnAction(clans, fame, X)` turns it on in a running game, giving each clan the fame worked out from its log and dealing the kept Achievements (done for Winter for Clan, 2026-11-13). Tapping a clan's fame (its icon or number) in the player panel opens every clan's fame and Altar of Kings, how fame is gained and how the fame win works (`FameGainsView`, `fameGainsView` in `nort/ui.scala`; display only; 2026-11-13).
Marzaus's "Clan Fame upgrades (PC)" (Discord northgard-fanmade-content; asked for on 2026-11-11; `FameUpgradesModule` in `nort/options.scala`, under Fan content and in the PC version, rules in `nort/fame-upgrades.scala`): the seven core clans have abilities while their fame is at least 20, 50 or 100, losing them below (`FameUpgrade.all`, `game.fameUnlocked`, updated by `sweep` from `Game.loggedPerform`; once-a-year ones in `game.fameUsed`), fame is public (`fameHidden`), and the cards' fame (Development cards, Achievements, Unrest) is counted when each is taken, none at the end (`finalFame`; 2026-11-12), shown on the player panels (tapping them, every clan's, the lines wrapping: `onBoardWrap`, as in the Military paths list); Goat's sheep (Shepherd: one a year, no cap, 2026-11-12; `game.sheep`, moved before each Move card's Move, `SheepMovesAction`; drawn `token/sheep.webp` by `nort/tools/sheep-token.py`) and Boar's tamed creature (Taming: `game.tamed`, with the warchief; creatures leave Boar alone through `Game.robotosIn` and `FameCreaturesExpansion`); Valorous Leader's tougher Brand goes through `Game.chiefCost`, which the Warchiefs PC version's 2-casualty warchief uses too; readings in `nort/RULES.md`.
The owner's "PC version" mode (asked for on 2026-11-11, `PCVersion` in `nort/options.scala`, under Fan content, rules in `nort/pc.scala`; design and open questions in the Claude Doc "Northgard PC version mode: design proposal") is played after the video game: picking it turns on (and is turned off with) Creatures, Warchiefs, Sea, Events, Tower PC version, Warchiefs PC version, Minerals, Military paths, Aggressive creatures, Clan Fame upgrades and the Wilderness, Wastelands and Horizons map tiles (`PCVersion.needs`, `tiles`, `ModuleOption.forcedOn`), and picks Domination victory and Alternative fame gains, which can be turned off (`PCVersion.victories`, `alsoOn`, a `BaseOption` hook turned on with an option but not required by it; 2026-11-13); the whole map is built at setup (`buildMap`: a special central tile, a Beach with a Port per clan at the edge, every other space filled, the edge counting as coast, `Board.coast`), each clan starts on its Port (4 units) and the two small territories beside it (1 each), balanced: every home is copies of the same three tiles, 1 food in the Port territory, 1 wood on one side, 1 lore on the other (`attemptMap`), with a Town Hall (`game.halls`, the Town Hall Development card's hall, upgraded the video game's hall from the wiki, `token/building/town-hall*.webp` by `nort/tools/town-hall.py`; upgraded by a build for 1 Stone and 1 wood, `HallUpgradeAction`), creatures hold the territories between, fog of war hides what a clan hasn't had figures in or scouted (`game.scouted`, drawn black by `fogImage` in `nort/ui.scala` for the `viewers`), Explore scouts (`ScoutAction`; enemy territories need the upgraded hall, coastal ones a Defense Tower in the Port territory; a scout gives an Explore's fame and bonuses, the scouted territory counting as the one closed), only Wolves, Draugr and (from year 3) Myrkalfar move, Wolves come back where they started, and a clan ending a year without its Town Hall is out (`game.out`; its Raid units are lost with it, and it gets no three units back as a clan left with none does, 2026-11-12); the Hard bot and Robotos scout, move into creature territories and guard their Town Hall there (`scoutValue`, `hallValue` in `nort/bot-hard.scala`; 2026-11-12: the Robotos bot never left home); in new games (hidden `PCVersionUpdate`, 2026-11-11) the two-player map is an oval, every home has a lair territory with a small building space just outside it for a Mine, Mines give Stone with no roll (or, in games with the hidden `PCMineChoice` every new game gets, each clan picks that Stone or the Minerals roll for Stone, Iron or Gem at each harvest; 2026-11-13; `PCMineChoiceAction(X)` switches a running game); Raven's Rangers (Clan Fame upgrades) scouts once more, anywhere it doesn't see, after a scout (hidden `PCRangersScout`, every new game; 2026-11-13: it was never offered in PC games; `PCRangersScoutAction(X)` switches a running game), the 2, 4 and 6 clan maps are mirrored (`turnsOf` in `attemptMap`) and with 3 clans or more the Wilderness and Wastelands creatures are in (`moreCreatures`); the Town Hall is drawn beside the Port, not over it; Vedrfolnir's controller reveals one random territory it hasn't seen at the end of each year, enemy ones too (hidden `PCVedrfolnirReveal`, every new game; 2026-11-12; `PCVedrfolnirRevealAction(X)` switches a running game; the tile's info box says so, `TileAbilities.textIn`), and its Wastelands tile adding before the Harvest is not offered (hidden `PCVedrfolnirNoTile`, every new game; Bear was asked in Winter for Clan and the tile fit nowhere, 2026-11-12; `PCVedrfolnirNoTileAction(X)` switches a running game); "Domination victory" (`DominationVictory`, under Fan victories, shown only with the PC version) makes the last clan holding its Town Hall win at once, beside the other victory conditions (in games with the hidden `PCDominationChoice` every new game gets; older PC games always had it); the retired "Domination victory only" (`DominationOnly`, hidden, still in Winter for Clan) made it the only winner, the game going on after the last year; from Marzaus's playtest (2026-11-14), new games get hidden `PCSeenOnly` (no move or retreat into a territory the clan doesn't see, `PCExpansion.mayEnter`), `PCMinePerMine` (Stone or the roll for each Mine), `PCOneUpgrade` (one upgrade per Build, the Town Hall's too), `PCNavalPort` (a Port gives 3 combat points against an attack by sea, `game.navalFights`), `PCHallShield` (the Town Hall ignores 1 rolled casualty in its territory, against clans and creatures), `PCHallSafe` (no creature moves into or attacks a Town Hall in years 1 and 2), `PCArmyCap` (22 units, Winter costs up to 6 food 4 wood), `PCTowerPoints` (a Defense Tower gives 2 points against creatures), `PCScoutFame` (a scout gives 1 fame) and `PCScorchedSeen` (2026-11-15: the Scorched Earth token, the Snake Clan card's and Under Pressure's, only goes where Snake sees, `PCExpansion.mayScorch`, and Under Pressure may also move it to a neutral territory, collecting what it gives), each with a `<Option>Action(X)` switch; Jötunn Blainn waits at the Jötnar Camp of the built map, recruitable at the Harvest, in games with the hidden `PCJotnarCamp` every new game gets (2026-11-20: the camp was set only when its tile was explored, so in the PC version he was never there; `PCExpansion.jotnarCamp`, `PCJotnarCampAction(X)` switches a running game); `NORT_PC=1` in the headless host; see `nort/RULES.md`.
The setup screen lists the modules one section per box, as a player laid them out on 2026-10-26: "Base game" (Creatures module,
More creatures!), "Warchief expansion" (once "Warchiefs module"), "Wilderness expansion" and "Wastelands expansion", each split into
its "Environment tiles" and its creatures (`WildernessTiles`, `WildernessCreatures`, `WastelandsTiles`, `WastelandsCreatures` in
`nort/options.scala`; any part turns the expansion's code on, `Meta.has`; `Wild.tilesOn`/`creaturesOn`, `Waste.tilesOn`/`creaturesOn`
say which), with the folded "Ban Environment tiles" (`BanEnvironment`, the two Geysers, Ruins and High Peaks together; the Ancestral
Graveyard and the Wyvern's Den bring their Spectral Warriors and Wyvern) and "Ban creatures" (`BanCreature`, the expansions' deck
creatures) shown for the parts picked, then the Central tile, then "Uncharted Horizons" (Events, Sea, its map tiles); Balanced
starting tiles is under Fan content (2026-11-02; it sat right after Card edition). Older games keep `ModuleOption(Wilderness)` / `ModuleOption(Wastelands)`, both
parts, now hidden (`Meta.retired`, still logged at the start); Adset still adds the whole expansion it drafts.
On the map, territories are tinted with their controlling player's color,
turn gray when invaded and pink during the fight (resource icons and the
printed building space frames stay untinted: `tint` in `nort/ui.scala` cuts them out); closed territories that give fame have their border dashes
drawn in the controller's color (alternating where two players' meet,
dotted yellow or white rails beside Rough borders, solid ones beside
the orange impassable lines of the Peaks, the Poisonous Swamp's corners and the
walled five-player tile), from the dash data in
`nort/lines.scala` made by `nort/tools/BorderLines.java`; the area masks (`tile/mask/`) and the free-ground
grid for placing warchiefs, Kaija and creatures (`nort/grid.scala`) are made
from the tile art by `nort/tools/tile-masks.py` (it keeps figures off the Great Lake's water and,
on the Wilderness, Wastelands and Uncharted Horizons tiles, the rocks and the rock bands along impassable and rough borders; `tile-masks.py <tile ...>`
regenerates only those tiles' grid entries). Warchiefs, Kaija and
creatures are kept off the resource icons (`nort/icons.scala`, the holes in
the masks, made by `nort/tools/ResourceIcons.java`), and the icons are drawn
again over the pieces, so a figure that must overlap one never hides it. Tapping a piece on the map opens what it stands for, unless the tap
picks its territory for an offered action (`pieceInfo` in `nort/ui.scala`): a creature, Jötunn Blainn
or a Raid card its card, a warchief (and Horse's Brok) its board, power and upgrade card (`WarchiefInfo`,
`Meta.warchiefInfo`), and Kaija, Lynx's Brundr and Kaelinn, the High Tide and Scorched Earth tokens their
clan's three clan cards (`ClanAbilityInfo`, `Meta.clanAbility`). An Ox Ancestral Equipment token says what it does in a fight and how Ox takes it (`GearTokenInfo`, `gearTokenView`; asked for on 2026-10-21); when taking a token is offered, the tap takes it. The Automa's Leaders open nothing. The tiles with a rule of their own (the Wilderness and Wastelands Environment tiles, the Wastelands central tiles and The Bridge) keep their centrepiece (the Vedrfolnir eagle, ...) out of the territory tint, like the resource icons, with a small "i" at its top right; tapping either opens the tile's name, picture and rule (`TileAbilities` in `nort/tile-abilities.scala`, `TileAbilityInfo`; asked for on 2026-11-01). The fame icons printed on the Ruins, both Wyvern's Dens and Yggdrasil stay out of the tint too, and figures are kept off them (`TileAbilities.fame`; 2026-11-02).
The Northgard layout (`layouter` in `nort/ui.scala`) has the map on the left
and the player panels, log and action pane (choices and hand) on the right;
the owner asked for that on 2026-10-05, undoing an earlier mirrored layout
with the log on the left. Ultrawide screens
(width at least 2.1 times the height, e.g. 21:9) skip the layouter and get
a fixed layout (`layout` override in `nort/ui.scala`): map, then short
player panels in a row, the shared cards and the hand below them, and the
log on the far right, with the
hand cards sized to the pane (`--nort-hand-card`) so the whole hand fits. The shared cards strip (Developments, Achievements, Creatures and the rest, the `court` pane)
has a tab on its left that folds it into a one-line bar listing each group with its card count, and back;
the player panels right above it (ultrawide) grow into the room with a larger font (as far as their width
allows), or else the pane right under it (the map) does, and nothing else moves (`foldCourt` in `nort/ui.scala`). Folded, the player panels are laid out again in one row or several (2x2 for four panels on a phone), whichever gives the largest font that still fits their measured contents, each only as tall as it needs; the rest of the room goes to the pane below. They are measured again once their icons have loaded (an icon takes no room before), so a game loaded folded fits too (`refitFolded`). Open, a row of panels right above the strip (a phone held upright, three players or more) takes the room its contents need from the strip, up to half of it, and shrinks its font for the rest (`openPanels` in `foldCourt0`); their tables were cut off before (2026-10-18). On a phone the map's zoom and arrow buttons start folded into their one corner button until the player unfolds them (`mapButtons`, `<settingsKey>.map-buttons-folded`). The folded bar starts with "Year X of 7". Where the strip sits left of the panels at the top (16:9 and 16:10), its tab points sideways (◂ to fold, ▸ to unfold): folded, it is a narrow tab on the left with its text running down in two columns, and the panels run across the top from it, with the map and log under them (`courtSideways`). There the tab swaps the top row between the two instead of sharing it (asked for on 2026-10-12): the panels across it with the strip folded into that tab (the default), or the strip across the whole row and no panels (`courtWide`, remembered apart from the other shapes' folding as `<settingsKey>.court-wide`). On a tall screen (a phone held upright) the log is moved out of the player panels' row to beside the action pane at the bottom, the panels take that whole row under the strip and the height their contents need from the map, and during setup, when the strip is empty, its room too (`logBesideAction` in `nort/ui.scala`; asked for on 2026-10-29). With two players (or one) on a tall screen (a phone held upright), the panels are moved up beside the strip (`panelsBesideCourt`), wherever the layouter put them, so it folds sideways there too. Each browser remembers it (`<settingsKey>.court-folded`). The divider right of the map, the one between the log and the action pane, and the bottom edge of the player panels have round drag handles with arrows (`split` and `placeHandles` in `nort/ui.scala`; asked for on 2026-10-08): dragging moves the panes along that edge, each kept at least 15% of the window (moving the map's edge left, an unfolded row of court and panels above it follows; a row of panels starting at the edge shares the room), double-tapping one puts it back, and each browser remembers the offsets per screen shape (`<settingsKey>.split-<tall|wide|ultrawide>`, "x,y,panels,bottom"). The panels' edge stretches or shrinks the panels (to half their height at least) and their font with them, as far as their measured contents fit their width; the court right under them moves along, the panes under that give the room (the log down to 15%, then the action pane). Folded, the extra height is first given to `foldCourt` (its `extra`), which may lay the panels out in more rows with a larger font. The map's bottom edge has one too when panes are right under it (the log and the action pane on a phone held upright, asked for on 2026-10-11): dragging it up makes them taller and their font larger (by the square root of the height), down the other way. A layout with no pane past the map's right edge or under the log or the map shows none of those handles. The log folds in every layout (`foldLog` and `placeLogToggle` in `nort/ui.scala`): a small "–" button in its top right corner (bottom right when a handle is there) folds it into a "+ Log" strip, beside the pane it touches or across, and that pane (the action pane first, else the map) takes the room; each browser remembers it (`<settingsKey>.log-folded`). A page runs one game: `startGame` in `hrf.scala` ignores a second start (`gameStarted`, reset by Play Again), and the handles remove any left by an earlier game UI, after a player saw two of each handle on 2026-10-09. The player panels run left to right in turn order, starting with the clan that went first this year (the order doesn't change when the first player marker moves during the year; `panelOrder` in `nort/ui.scala`, `game.yearFirst`; Adset uses its random seat order, `game.seats`; asked for on 2026-10-13). The player panels show the card counts, then units, warchief and supply, then a small table with grid lines around its cells (`ledger` in `factionStatus`, `nort/ui.scala`; asked for on 2026-10-08): a column per resource and one for fame, the stockpile (other players' fame is "?" until the game is over, as the fame tokens are face down: only the clan whose player is asked or waiting on this screen, and its teammates, show theirs; `viewers` and `fameHidden` in `nort/ui.scala`; spectators see none; the setup screen's "Fame" choice, Private (the default) or Public, `PrivateFame`/`PublicFame` in `nort/options.scala`, shows everyone's in Public games), then under it the clan's next harvest as things stand
(`Harvest.forecast` in `nort/game.scala`, the same sums `HarvestAction` uses; the Dragon's food and wood as ranges, "+1-2", the upper end faint since its sacrifice adds 1 to only one of them) in green (grayed out once that year's harvest is collected, through trading and Winter, as it then only forecasts next year's: `game.harvested`; asked for on 2026-10-22) and its Winter cost (as losses, "-1") in red, on rows marked with a wheat sheaf and a snowflake (`SeasonIcon`, drawn by `nort/tools/panel-icons.py`); tapping the table opens the whole
Winter chart (`winterChart` in `nort/ui.scala`), except the lore (its icon or number), which opens the clan's Lore Tree (`LoreTree`, `loreTree`; moved there from the action pane on 2026-10-08). Each player can hide the
tints and the colored borders under "Interface" (the Territory Color and
Fame Borders settings in `nort/meta.scala`). The tints are the player colors themselves
(`PlayerColor.hex`: each player color is one shade for the names, the tint and the fame border dashes, `PlayerColor.ink` and `lineOf` in `nort/ui.scala` being the same; red is a neon `#ff0000`, 2026-11-04 and 07, and yellow the yellow units' `#fbfb05`, which the names had since 2026-10-18; every color made one shade on 2026-11-10), at 50% opacity; picking "Custom" under Territory Color
(`CustomTerritoryColor`) shows the "<Color> Territories" rows, where each player sets how
opaque each color is, 0 to 100% in steps of 10 (default 50%) (`TerritoryOpacity`, a
`CompactSetting`: its values are one row of small buttons in the settings screen,
`editSettings` in `hrf.scala`; rows are hidden while `Setting.visible` is false). The "Player Panels" setting (`StackedPanels`, the default since 2026-10-08, `CompactPanels`, or `ReverseStackedPanels`) shows every number in the panels under its icon, beside it ("Compact"), or above it ("Reverse Stacked"). A turn starts with six choices at
the top of the action pane (Play cards, Wait, Replace, Remove, Upgrade,
Pass); after one, tapping a card in hand does it. A Build card's builds: tap
a free space, pick from the menu of all buildings with their costs and how many are left in the supply (7 of each, `SupplyLeft` in `nort/map.scala`, also on the other build and replace choices), then
confirm with the green check mark (or cancel with the red cross) drawn above
the building on the map. Placing a tile works the same way: the tile is shown at its spot with
one button just outside each corner: rotate arrows at the top, the check mark and cross at the bottom. A retreat works like a Move: tap the destination on the map (or in the list),
then pick who goes. Every move (a Move card, a retreat, Brand's Bravery, the Lynx bonus Move) picks its figures one by one after the
destination (`FigurePicker` in `nort/figures.scala`; asked for on Discord on 2026-11-01, when one of Brundr and Kaelinn couldn't retreat alone):
the units as a counter ("−", "3 of 5 units", "+"), a Yes/No switch for the warchief and for each companion (Kaija, Brok, Brundr and
Kaelinn separately, by name: `game.lynxSwapped` names the two lynx slots, and picking the first slot's lynx alone out of a pair records `LynxFirstAction`, which swaps the names before the same recorded action as before (2026-11-02: "The cats are two separate units")), and the button that does it, naming who goes. The switches are
soft and the button is the same recorded action as before (`MoveUnitsAction`, `MoveOneLynxAction`, `RetreatPartyAction`, the new
`RetreatOneLynxAction`, `LynxGoAction`...), so it needs no option and older games load and replay unchanged; `FiguresExplodeAction` lists
every button for bots and move checks. Hidden Ways uses it too (`HiddenGoal`, `HiddenOneLynxAction`). Swamp and teammate passes, Stag's single-unit Move and Military Militia moves keep their own lists. Casualties picked by their player (`ChooseCasualties`) use the same picker (`CasualtyGoal`; 2026-11-02): the units counter (then Militia), Yes/No for the warchief, each lynx or companion, Jötunn Blainn and the Military paths' bodyguard, and a button naming the loss, the same recorded `CasualtyPickAction`, except losing both lynxes while keeping units, which the old list couldn't say (`CasualtyBothLynxAction`, `game.casualtyBothLynx`), and losing the bodyguard before all the units, as it comes back each year (`CasualtyGuardAction`, `game.casualtyGuard`; 2026-11-02; it only adds a choice, so no option). Development
and other card picks show the cards at hand-card size, and the last player to pass
still sees the one card left before taking it. Copies of the same card in hand (and among
the Played cards) are drawn as one card with the copies peeking out behind it
(`Card.handStack` in `nort/cards.scala`), in the hand and in every pick from it. The front card casts a shadow on the copies, which are darkened with a light edge, and its bottom right corner says how many there are ("×2"; asked for on Discord on 2026-10-14, as players kept missing their second Feast). Hand cards (and the card picks shown at hand-card size) are at most a fifth of the action pane, so five fit on a line (`--nort-hand-five`, set by `handFive` in `nort/ui.scala` at each layout; ultrawide keeps its own `--nort-hand-card`), and the discard pile, active cards and Lore Tree show small cards (`styles.discardCard`), so a dozen fit on the screen (a player had to scroll through his hand, 2026-11-22). As in Root, every card opens full screen when tapped and closes on a second tap; in those piles the second tap goes back to the pile (`PileCard`). Tapping a player's name in its panel opens its clan's info (`ClanInfo`, the same as the clan picker's "i" button); the clan board that was shown under the Lore Tree was removed for it on 2026-10-08 (asked for). The player panels show numbers with icons, on rows that don't wrap: cards to draw, in hand, played this year (the active cards) and discarded (card icons outlined yellow, green, white and red, `ui-card-draw`, `-hand`, `-played`, `-discard`, drawn on the start card back by `nort/tools/panel-icons.py`; tapping the white card shows the active cards, the red one the discard pile), then the units on the map (the Recruit card's figure, `ui-unit`; with the Sea module, the units away on Raids in brackets beside it with a longship, `ShipIcon`, `ui-ship`, drawn by `nort/tools/panel-icons.py`), the warchief with the Warchiefs module (its token and name, grayed out while in the reserve) and the units left in the supply (`SupplyIcon`, `ui-unit-supply`, the units icon grayed out, by `nort/tools/panel-icons.py`), then the food, wood, lore and fame table, and the first player marker beside the clan name (`ui-first-player`). A clan played by a bot has a robot's head before its name in its panel (`ui-bot`, by `nort/tools/player-type-icons.py`; asked for on Discord on 2026-11-20, a person gets none, as the owner asked); tapping it says the bot's level, Easy, Hard or Robotos (Robotos lists the cheats), or the Automa's level (`PlayerTypeInfo`, `playerType` in `nort/ui.scala`). The level comes from the seats the game was started with (`Callbacks.bot` in `hrf.scala`: an online game's lobby lines, a local game's save), so it is display only and no game records change; a bot it doesn't know is just "Bot" (`game.human`). Ox Clan's panel shows its Ancestral Equipment tokens themselves, the ready ones face up and those used this year grayed out (no line while it has none: "Ancestral Equipment: none" sat under the panels' drag handle); tapping them lists all seven, what each does and where it is (`gearRow`, `gearView` and `GearView` in `nort/ui.scala`; asked for on 2026-10-12). Kraken Clan's panel shows its two High Tide tokens the same way, those on the map in color and those in the reserve grayed out; tapping them says what High Tide does and where each token is (`tideRow`, `tideView` and `TideView` in `nort/ui.scala`; asked for on 2026-10-21, it said "High Tide: 0 in reserve").
Ox Clan picks the free small building space or Carved Stone space of an explored tile its Ancestral Equipment token goes on (`GearPlaceAction` in `nort/newblood.scala`, asked for on 2026-10-12), only in games with the hidden `OxPicksSpace` option every new game gets; older games keep the first free small building space, and so put no token on a tile with no free small building space. A running older game can be switched over with `OxPicksSpaceAction(X)` through an `hrf-games` fix; `OxPicksSpaceAction(GearPlaceAskAction(Ox, <tile>, <spot>, X))` also asks Ox to place the token a tile missed (Draugr against Winter, 2026-10-26: an Ox player explored `tile-05`, whose only free space is a Carved Stone space, and got none).
Replacing a building (Industrious Villagers, Horse's Craftsmen) when there is more than one to replace asks for the building first (tap it on the map or pick it in the list), then what it becomes (`ReplaceSpotAction` and `replaceAsk` in `nort/map.scala`; before, every building's choices were one list with the same names twice, reported on 2026-10-14). Only the soft first step is new, so the recorded moves and old games are unchanged. Craftsmen ("with no other restriction") also offers a Carved Stone in place of a small building on a small building space, which Industrious Villagers and builds keep to Carved Stone spaces (`replacements(f, craftsmen)`; 2026-11-01); it only adds choices, so no option.
A tile can't be placed where it joins two players' units (or a player's and a Fallen Valkyrie, which ignores the Robotos bot) into one territory; only the territories the tile joins are checked (`joins` in `nort/map.scala`), in games with the hidden `JoinedTilesOnly` option every new game gets. Older games check every territory on the map, so two clans sharing one anywhere (teammates, or Robotos beside a Valkyrie) stopped every Explore (Winter of Saga, 2026-10-13).
Naströnd (Wastelands, `waste-nastrond`) gives the two wood it prints, in its west and east territories, at harvest and to everything collecting from a territory (Quality of Life, Lay of the Land, ...), in games with the hidden `NastrondWood` option every new game gets (`Game.areaSpec`); the tile data had no wood there (2026-11-01), so older games keep none.
Dragon Clan's extra food or wood after harvesting is only one that one of its territories gave at that harvest, less what Snake's Scorched Earth took there (`dragonExtra` in `nort/newblood.scala`, `game.scorchedTook`; Dragon collected food from a scorched food territory, reported on Discord on 2026-10-19), in games with the hidden `DragonExtraFromTerritory` option every new game gets; older games offer either.
Snake's Scorched Earth offers only what its territory gives at that harvest after the Events (Infestation's 1 food less, a skipped or doubled harvest; `ScorchedHarvestAction` in `nort/game.scala`, `EventsExpansion.harvest`), in games with the hidden `ScorchedAfterEvents` every new game gets: in Saga and Clan (2026-11-10) Snake took food Infestation had already removed, Horse was left with -1 food and its next fight asked it to spend food from an empty list, which stopped the game. `ScorchedAfterEventsAction(X)` switches a running game and puts any food, wood or lore below 0 back to 0. Every food payment (fights, creatures, the Lion's and Economics) now skips its question when the stock is 0 or less, so a negative stock can't stop a game again.
A clan taking casualties in a fight picks which figures go when it has a choice (units, the warchief, Kaija or the other companions, Jötunn Blainn; `CasualtyPickAction` in `nort/map.scala`, asked for on 2026-10-11), only in games with the hidden `ChooseCasualties` option that every new game gets (`mandatoryFor` in `nort/meta.scala` and `nort/adset.scala`); older games keep taking units first so their replays don't change. A running older game can be switched over from a given fight by wrapping a recorded step in `ChooseCasualtiesAction(X)` through an `hrf-games` fix (`nort/game.scala`; made on 2026-10-15 for Winter of Saga, where Snake wanted to lose its warchief to the Fallen Valkyrie instead of its last unit).
Ancestral Curse's opponents pick the card they discard all at once, each on their own screen (a `MultiAsk`, `CurseAllAction` in `nort/effects.scala`; asked for on 2026-10-16), instead of one after another, only in games with the hidden `SimultaneousCurse` option that every new game gets; older games keep asking in turn order.
The Kraken Clan card offers only the food or wood that a territory Kraken controls with a High Tide token gives (`resourcesIn` in `nort/newblood.scala`; with neither, it goes straight to the draw), as the card says "from a territory" (reported on 2026-10-19), only in games with the hidden `KrakenFromTerritory` option that every new game gets; older games offer either.
Kraken's Knowledge from Beyond follows the print card (2026-10-20): each High Tide territory gets its own free choice of Recruit 1 there (its Training Camp adds a unit, like every Recruit card), Collect 1 resource that territory gives, or Draw 1, so the same one can be picked for every territory, only in games with the hidden `KnowledgeEachTerritory` option that every new game gets; older games offer each option once and any resource.
Before a Kraken card, with both High Tide tokens already out, Kraken may move one to another territory it controls (`TideMoveAction` and `tideMoves` in `nort/newblood.scala`; the board only says "place", and Kraken got nothing to do, reported on 2026-10-21), only in games with the hidden `KrakenMovesTide` option that every new game gets; a running older game can be switched over with `KrakenMovesTideAction(X)` through an `hrf-games` fix, like `KnowledgeEachTerritoryAction`.
Future Sight takes an Achievement only in the last year, when the Achievements are the cards on offer; before it, only a revealed Development card (`futureSightCards` in `nort/effects.scala`; asked for on 2026-10-21), in games with the hidden `FutureSightLastYear` option that every new game gets; older games could take a face-up Achievement in any earlier year, and replay that way. An Achievement taken by Future Sight or at Mimirsbrunn is kept aside, face down, to score at the end (`FactionState.aside`, counted in `deck`), not put on top of the draw pile, where a draw brought it into the hand in place of a card (2026-10-28), in games with the hidden `AchievementsAside` option every new game gets; a running older game can be switched over with `AchievementsAsideAction(X)`.
The setup's "Card edition" choice (`EditionChoice` in `nort/editions.scala`, also in Adset; asked for on 2026-10-22): 1st edition or 2nd edition (`EditionChoice.default`, new games, the Quick Game and the Tutorial). Games made before it have neither and play the 1st edition. Cards that differ are entries in `CardVersions` (`nort/editions.scala`), each naming the card it replaces, the option that turns it on and its own name, image, effect and text; `Card.info` gives the version in play (`Card.printed` is the first printing), so a later erratum or a fan-made card is one entry plus an option. `CardVersions.options` follows the game being played (set by `Game` and `addOption`; one game per page, and the JVM checks run one game at a time), and before a game, as in the clan picker, holds the default edition. The clan info shows the other edition's text under each card that has one. The 2nd edition cards (images `card/clan/*-2e.webp`, cut from screenshots forwarded from Discord): Stag Clan's card lets one unit make a Move 1 ignoring Rough borders from anywhere, a real Move that can attack (`StagMoveEffect`, `StagSingleMove`), Protector of the Land draws at most 3 (`ProtectorMaxEffect`), and Rapacious Exploitation then may draw 1 (`RapaciousDrawEffect`).
The printed New Blood cards and boards (photos from 2026-10-23; the site's were the TTS prototype) are on in games with the hidden `PrintedNewBlood` option every new game gets: 19 clan cards in `CardVersions.printedNewBlood`, and the Squirrel Clan card, missed until a player saw it on 2026-11-06 (1 food after the Recruit if the unit went into a territory producing food; hidden `PrintedSquirrelClan`, every new game) (`nort/editions.scala`, effects in `nort/newblood.scala`), the board rules (Lynx's Brundr and Kaelinn as two figures, `game.lynx2`, each starting placement may take one, so Lynx can start with both, in games with the hidden `LynxBothAtSetup` every new game gets (2026-10-31; before it a second one placed at setup moved the first), and one placement may take both, with one unit or with the warchief (`SetupLynxPairHereAction`, 2026-11-01; it only adds choices to those games); both count as units for Winter costs in games with the hidden `LynxesCostWinter` every new game gets (2026-11-01; `FactionState.winterUnits`; older games left them out); a Training Camp's extra recruit may be a lynx from the reserve, Lynx picking, in games with the hidden `LynxCampRecruitsLynx` every new game gets (2026-11-02; `CampRecruitAction` in `nort/map.scala`; older games always got a unit; a running game can be switched over with `LynxCampRecruitsLynxAction(X)`); with a bonus Move of only them; Mielikki brings a lynx (by name, `mielikkiAsk`; also when she attacks a creature, in games with the hidden `MielikkiAgainstCreatures` every new game gets, 2026-11-02, `MielikkiCreatureAction`; `MielikkiAgainstCreaturesAction(X)` switches a running game); Kraken places or moves High Tide before or after its cards and loses it with a lost fight; Dragon's Pyre; Squirrel after Winter, `AfterWinterAction`) and the board pictures with the printed text (`expansion/board/<clan>-print.webp`, drawn over the old text by `nort/tools/board-text.py`; `Warchief.board`, `Warchief.power`). Older games keep the TTS cards; see `nort/RULES.md` (New Blood).
The setup's "Undo" choice (`UndoChoice` in `nort/options.scala`, also in Adset; asked for on 2026-10-20): Unlimited undo (the default, and every older game), Limited undo, or No undo. It only changes the log's "Undo to here" (`undoBlocked`, a hook in `grey.scala` that `nort/ui.scala` overrides), so no game records change: No undo never offers it, and Limited undo doesn't go back past a die roll, a shuffle or a random pick (an `OracleAction`), nor past a card, tile or token taken from a face-down pile or a hand looked at (`Game.revealedSince`: each pile in `hiddenPiles` must still end with what it held then, and `peeked` counts cards drawn and put back on top and Spy's and Rapacious's looks at a hand).
A clan that can't pay for Winter takes its Unrest card itself: the card is shown and the player taps it (or, with none of the ten left, taps to lose 5 fame and discard the top card of their draw pile), one clan at a time in turn order (`WinterPayAction`, `TakeUnrestAction`, `UnrestGoneAction` in `nort/game.scala`, asked for on 2026-10-12 as it was easy to miss), only in games with the hidden `TakeUnrestCard` option that every new game gets; older games hand it over without asking.
After the die decides a fight (against a clan or a creature), a combat report (one sentence a line: who won and how, then the defender's and the attacker's totals, combat points, casualties inflicted and the fame a clan gained for defeating a creature, `FightSide.fame`, then each side's sources, defender first; the figures lost aren't listed, as the casualties say it; laid out on 2026-10-26 and 28) is shown to both sides before the retreat, to be tapped OK (`CombatReportAction` in `nort/map.scala`); each player can skip theirs with the in-game "Combat Report" setting under Interface (`ShowCombatReport`, the default, or `SkipCombatReport`, like Root's Ambush! setting; the UI then taps OK for them, `ask` in `nort/ui.scala`). The old game options `CombatReportAttackers` and `CombatReportDefenders` are hidden and do nothing. In a local game with more than one human clan, each time another of them is asked the action pane shows "Pass to <clan>" and a "Show Hand and Choices" button instead of its hand and choices, and every clan's fame shows "?" until the tap (`passTo`, `passedTo` and `passing` in `nort/ui.scala`, like Root's pass screen; `Callbacks.hotseat` in `hrf.scala`); notifications wait for the tap too. The "Pass and Play" setting under Interface (`ShowPassScreen`, the default, or `NoPassScreen` in `nort/meta.scala`) turns it off (asked for on 2026-10-17).
A clan power that goes off by itself (Raven, Stag, Boar, Goat, Wolf, Dragon's Pyre, Ox's token, Rat's units) is told to its player in a box after the action, "You activated your clan power in <territory>." with a button naming the gains ("Collect 1 Lore"), in games with the hidden `ClanPowerReports` option that every new game gets (`ClanPowers` and `ClanPowerReportAction` in `nort/map.scala`, asked for on 2026-10-12); the gains are already made, and the in-game "Clan Power Report" setting under Interface (`ShowClanPowerReport`, the default, or `SkipClanPowerReport`) skips it. The reroll step (Liv's, with the Warchiefs module, and Ox Clan's Ancestral Equipment token 3) shows what was rolled and both sides' combat points with that roll above "Reroll the combat die" and "Keep the result", and, for the attacker, that the defender rolls next (`RerollInfoAction` in `nort/map.scala`, asked for on 2026-10-13; an info box, so no game records change).
In games with the hidden `ReportsDontWait` option every new game gets (not the Tutorial's practice game, which explains the boxes), the combat report and the clan power report don't stop the game: each reader gets them as a notification (`ReportNote` in `nort/map.scala`, `game.notify`), shown over their screen the next time it updates (`showNotifications` in `nort/ui.scala`, which leaves out the kinds their Skip settings turn off); asked for on 2026-10-21, as nobody should wait for another player to tap OK on a box only they read.
Explore writes "drew a tile to explore" when the tile is drawn (`ExploreChooseAction` in `nort/map.scala`), so the log has a line between the draw and the placement and Limited undo can go back to the drawn tile to place it again (2026-10-21; the line before was the card played before the draw, which Limited undo refuses).
In every game, the log's word for how a fight ended ("won", "defeated", "drove", "wiped out") opens that fight's report (`CombatReport.link`, `FightReportView`).
Combat points and casualties are drawn as the battle die's axe and skull wherever they come up (fight info, die results, the log, card and rules text): `CombatIcon` and, for rules text, `CombatText` in `nort/game.scala`, with `ui-axe` and `ui-skull` cut from the die texture `token/die.webp` (TTS mod 2838546142) by `nort/tools/dice-icons.py`.
Images are in `webp2/nort/images/` (`card/`, `tile/`, `token/`), from a
Tabletopia export the owner uploaded, with gaps filled from two Tabletop
Simulator mods (Steam Workshop 2838546142 and 2847156187; see
`nort/HANDOFF.md`): all 35 core map tiles (`tile-31` to `tile-33` and
`start-5` are TTS scans aligned to the old photo crops), all 52 development
cards (Veiled Threats from TTS), and the warchief upgrade cards. Green starting cards are the blue ones with the ribbon
recoloured to the printed green. Expansion tiles, clan
boards and tokens are in `expansion/` for later. Unit figures are in
`token/unit/` (`unit-<color>`, and `warchief-<color>` for the Warchiefs
expansion), recolored from the `-original` images. The seven core clans'
warchiefs are drawn as their portraits instead (`chief-<clan>-<color>`, outlined
in the player's color, from the TTS standees by `nort/tools/warchief-portraits.py`;
`Warchief.figure`), and the New Blood warchiefs (and Horse's Brok) as pawns:
the video game's unit renders from the Northgard wiki (`expansion/warchief/<name>.webp`,
like the Lion's Hildegard), without their ground shadow, outlined the same way
(same names, `brok` for Brok, by `nort/tools/warchief-pawns.py`; asked for by the
owner on 2026-10-24 and 25, they were round tokens of the warchief's head, then
cut out of the clan cards' art). The Automa's Leaders are its two miniatures
(`leader-<1|2>-<color>`, white and black, cut from the Uncharted Horizons
rulebook photo by `nort/tools/automa-leaders.py`). The old warchief figure is
now a second warrior design: each stack of warriors is drawn with one of the
two, picked by a hash of the territory and the count (`Warchief.warrior`). Colors belong to
players, not clans: each clan's player picks one on its row of the setup screen
(`ColorOption`, default blue, red, yellow, purple, green, orange by seat;
`game.colors`); starting cards show that color's banner (orange has no
cards of its own and uses the yellow ones). The clan picker (custom, online and solo games) is a grid of
clan emblems (`factionTile` in `nort/meta.scala`, the `factionPick` helper in
`hrf.scala`); each clan's "i" button opens its clan ability, both clan
upgrades, warchief board and power, and warchief card (`factionInfo`). It also
offers two Random Clan picks, one of the seven core clans or one of all fourteen
(`randomFactions` in `nort/meta.scala`, the hook in `meta.scala`): the clan is
drawn among those not already picked and is shown only as "Random Clan" until
the game starts (`hidden` in `customGame` and `startSetup` in `hrf.scala`;
picking the drawn clan by hand draws again). The setup screen's first page has "Back" instead of "Cancel" when the clans were picked on the screen before (`picked` in `startSetup`): it returns to the picker with the picks kept, in every game (Northgard, Adset, Root and the rest; asked for on 2026-10-26). Those buttons (Back or Cancel, then Start or Next) are at the top of every setup page as well as the bottom (`buttons` in `setupQuestions`; 2026-10-26). The setup options
(colors, game length, victory conditions, first player, "Ban card draw developments"
(`NoDrawDevelopments`: leaves the seven Development cards that only draw cards
out of the decks), "Ban Defensive Strategy" (`NoDefensiveStrategy`, asked for on 2026-10-14;
also offered in Adset), Marzaus's Development card counts, under Fan content (moved there on 2026-10-26, asked for; once a "Development cards each year" section whose
default, one per player, is now the hidden `NormalDevelopments` older games keep) as two toggles that turn each other off (`DevelopmentsChoice`: `FewerDevelopments`, one fewer from year 2 on, "hard mode", asked for on 2026-10-22; or one extra, `ExtraDevelopment`, `game.devsIn(year)`,
suggested for two players on 2026-10-19, made for every player count on 2026-10-20; not against the
Automa; in the main setup and Adset; the card left after everyone passes is discarded; when the core decks can't last
every year, as with six players in a long game, the Uncharted Horizons Development cards are added, `ExtraDevelopment.needsHorizons`,
`game.horizonsDevelopments`, asked for on 2026-10-20, and the setup screen warns of it),
"Ban Development cards" (`BanDevelopment(id)`, one toggle per Development card, a folded section; the Uncharted Horizons ones
only with their option, all of them in Adset; `BanDevelopment.cards` gathers every ban for `ShuffledEarlyAction` and
`ShuffledAdvancedAction`, and the setup screen warns when the bans leave too few cards for every year, `Meta.tooManyBans`;
asked for on 2026-10-21), "One more Achievement card" (`ExtraAchievement`: the last year reveals players + 1
Achievements and the one left is set aside; asked for on 2026-10-19, also in Adset),
"Balanced starting tiles" (`BalancedStartTiles`, under Fan content since 2026-11-02 (first in it), off by default, also in Adset;
asked for on 2026-10-19 from Marzaus's group's house rule: each player's three starting tiles show at least one food,
one wood and one lore: each gets one tile from a food, a wood and a lore pile, a two-resource tile in the pile of the rarer
one (`startPileOf`), blank tiles not dealt, `dealStart` in `nort/map.scala`; picked over redrawing), and the modules and
expansions; Creatures, Warchiefs, Wilderness, Wastelands and Events are implemented, the others are shown but disabled) are in `nort/options.scala`.
"Victory conditions" picks one of: the standard rules, fame only, Alternative
victory with random cards (the rulebook's way), or Alternative victory with
cards chosen from the 21 listed below it (exactly 1 Map Control and 2 Wealth,
3 with teams, or Start is refused); plus Thane or Jarl (with teams, a team needs every card in both, the rulebook's 2 vs. 2 note, in games with the hidden `TeamVictoryAllCards` every new game gets, `TeamVictoryAllCardsAction(X)` switches a running game; 2026-11-07). Thane/Jarl and the
cards are listed only when they apply (`optionShown` in `nort/meta.scala`, a
hook the setup screen in `hrf.scala` calls for every option). Long sections fold under a tappable
heading with an arrow (`Folds` in `ui.scala`, `BaseOption.fold`): on the setup screen Game length, Victory conditions,
Fame, Undo, Card edition, Automa difficulty, Central tile and Adset's Wilderness or Wastelands
(every single-choice setup section, asked for on 2026-10-26) and Fan content (2026-10-29), in the settings Font Size, Scroll Speed (every game) and Territory Opacity. Folded,
a section shows only what is picked (Territory Opacity a one-line summary, `Setting.foldSummary`); every section starts
folded and the open ones are remembered in the browser (`hrf.open-sections`). A folded section with nothing picked shows its heading over "None" (before 2026-10-26 it vanished, so the
Ban sections could not be opened). Victory conditions stays open while the
chosen cards are the wrong number (`Meta.optionFoldOpen`). Northgard no longer sets `underConstruction`
in its `Meta` (setting it to `true` would put an "Under Construction" note under
its name on the game list and a disclaimer at the top of its menu). The main page (`/play`) ends with an "About" button that opens "About this site" (`Credits` in `hrf.scala`): JT's HRF engine, the AI note (Claude Code wrote most of the added code), the publishers, and links to hrf.im, HRF's source and JT's Ko-Fi; every game's About menu (Northgard's too, since then) starts with the same (asked for on 2026-10-30). Every game's own menu (`/play/nort`, `/play/root` and the rest) ends with "Back", which loads the main page `/play` afresh, or for Adset its parent game's menu `/play/nort` (`metaMenu` in `hrf.scala`, a meta whose `linkedModes` name it; asked for on 2026-10-31).
`nort/host.scala` runs bot games headless (JVM only,
like the other `host.scala` files). `live-check.scala` (`sbt "runMain hrf.LiveCheck <hrf-games checkout>"`, same setup)
loads every online game with the code as it is, the way a browser does; run it before deploying (see `nort/HANDOFF.md`). `nort/replay-check.scala` (`sbt "runMain nort.ReplayCheck"`,
same setup) checks that undo and loading rebuild the same game; see
`nort/HANDOFF.md`. Northgard has two bots: "Easy" (`BotXX` in
`nort/bot.scala`, random) and "Hard" (`BotHard` in `nort/bot-hard.scala`, which
values each choice by trying it on the game and scoring the position, and breaks creature movement
ties towards the human players it is against, told apart by the hidden `BotClanOption`/`BotSeatOption`
that `botOptions` adds for every bot, `game.human`; see Bots
in `nort/HANDOFF.md`), plus "Robotos" (`nort/robotos.scala`), the Hard bot with
cheats: no Winter costs, ignored by creatures, one more unit with every Recruit,
one more card each year, 1 more combat point when attacking, a head start (2 more food and
wood, 5 units in each setup placement), 25 units instead of 14 and upgrades for 2 lore; tapping the bot icon on its panel
lists the cheats.
Its rules come from a hidden `RobotosOption` that `startGame` in `hrf.scala` adds
for each clan set to it (`Meta.botOptions`), so every client and replay agrees.
Adset seats can be set to Robotos too: `MetaAdset.botOptions` adds a hidden
`RobotosSeatOption(seat)`, and the clan that seat drafts cheats (`game.cheaters`,
recomputed in `AdsetExpansion.assign`).

Pincer: A Gentleman's War (Julian Birke, a work in progress the owner's team playtests; asked for on 2026-11-23) is in
`haunt-roll-fail/pincer/` (meta `pincer`, URL `/play/pincer`, on the main menu since 2026-11-24: `HRF.menuMetas`), played from the
rulebook's second draft with the art of the Tabletop Simulator mod 3778939778 (`pincer/tools/pincer-art.py`, images in
`webp2/pincer/images/`). On the main menu since 2026-11-24 (`HRF.menuMetas`). `pincer/RULES.md` has the rules as played and the draft's open questions (the Boar's War penalty
isn't in it yet), `pincer/HANDOFF.md` the state, files and testing (`pincer.Host` bot games, `pincer.Dump`).

Company of Heroes: The Board Game, 2nd Edition (Bad Crow Games; a work in progress the owner's team helps test; asked for on
2026-11-23) is in `haunt-roll-fail/tactics/`, listed as "Tactics" (meta `tactics`, URL `/play/tactics`, on the main menu since
2026-11-24): the core rulebook's First Game Rules (RulesPal's copy), US Forces against the Wehrmacht on Moro River, with the Building
Boards, map panels and books of the Tabletop Simulator mod 2314664989 (`tactics/tools/tactics-art.py`, images in
`webp2/tactics/images/`; the map's lines read off the art by `tactics/tools/map-edges.py` and `map-data.py` into `Board.data`).
`tactics/RULES.md` has the rules as played and the readings, `tactics/HANDOFF.md` the state, files, testing (`tactics.Host`,
`tactics.Dump`, `tactics.Replay`) and what is next (the Full Game Rules, Commanders, the other factions and maps).

## Building

```
cd scala-js-dom-reduced && sbt publishLocal
cd haunt-roll-fail && sbt fullOptJS
```

- `index.html` loads the optimized build `hrf-opt.js` from `sbt fullOptJS`
  (Scala.js optimizer plus Closure Compiler, about 4 minutes from scratch).
  `sbt fastOptJS` still works for quick local builds, but its 49 MB
  `hrf-fastopt.js` is not used by the site and is git-ignored; to try it,
  temporarily point the script in `index.html` at `hrf-fastopt`.
- The client build needs more than sbt's default 1 GB heap or it fails with
  `OutOfMemoryError`. Use `SBT_OPTS="-Xmx6G -Xss4M"`.
- Of the build output under `target/`, only the optimized client is
  committed: `hrf-opt.js`, `hrf-opt.js.map`, `hrf-opt/main.js` and
  `hrf-opt/main.js.map`, so the server can run without building it (it
  compiles `good-game` itself when it starts). `.gitignore` ignores the rest
  of every `target/` (issue #11). After changing client code, run
  `sbt fullOptJS` and commit those four files.

## Live server

- URL: https://games.clean5110.com/play. `live-server.sh` runs it (`deploy`, `start`, `stop`,
  `restart`, `log`, `auto-deploy`, `backup-sync`, ...), and `setup-https.sh` sets up its
  Let's Encrypt certificate.
- Where it runs, how to reach it, DNS, https, starting and stopping, auto-deploy, the game
  copies and fixes, backups and the bug report setup are in `notes/server.md` in the private
  repository `guarantee418/hrf-games` (add it with `add_repo`). They were moved there on
  2026-11-17 because this repository is public. Keep server addresses, hosting and access
  details, and players' names, out of this repository.
- Deploying: no session can reach the server; it watches the GitHub branch `deploy` and
  deploys any commit on `main` that branch is pushed to, within about 2 minutes:

  ```
  git fetch origin main && git push origin origin/main:refs/heads/deploy
  ```

- Rankings (two ladders, Northgard with its Adset, and Root with its Advanced Setup and Mirror; asked for on 2026-11-18): there are no accounts, so the name a player types on
  joining a ranked game (`Meta.ranking`) is claimed with a random key kept in that browser (`PlayerKey`, `hrf.player-key`;
  `claimName` in `hrf.scala`): the server (`good-game/Ranking.scala`, files in `good-game/ranking/`, copied to `ranking/` in
  `hrf-games`) keeps the key's SHA-256 with the name and refuses the name to any other key. The key is the player's "player code",
  shown under Rankings, which another device can enter to play under the same names. A seat named before then claims its name when
  the game is opened. The first client to reach a game's end (`onOver` in `Runner.run`) posts its sides (`Meta.rankingSides`: a team
  or a clan; people only, by seat user) to `/report-result`, and the server keeps one report per game. "Rankings" on the Northgard menu
  (`rankingsMenu`, URL `/play/nort/rankings`) gets every result (`/rankings/nort,nort-adset`, names resolved: the seat's claimed name,
  else the name typed for it) and works out Elo there (`Elo` in `hrf.scala`: start 1000, K 32 split over the other sides, a winning
  side beats each losing side, sides rated by their people's average; fewer than 5 games is "provisional", shown with "?"), only
  games with people on two sides or more, and never a game with a bot in it, in ratings or stats (its lobby's `bot` line, checked by `Ranking.list` on the server, so games already reported drop out too; owner, 2026-11-21). The games that ended before are in `good-game/ranking-seed.tsv` (found with
  `LiveCheck <hrf-games> ranking`; their times only keep the order).
  Each seat reported also says what it played (`Meta.rankingSeat`, `RankingSeat.token`: "user/faction/seat/players/score", the clan
  and final fame, or the Root faction, a Vagabond with its character, and VP, "D" for a dominance), for the menu's "Faction stats"
  (filtered by player count) and "Turn order stats" (by player count), like the Root leagues' (rootleague.pliskin.dev): games,
  score (1 per win, shared between winning sides), relative score (per game, beside what a seat would expect) and average score;
  each player's page has them too, and the leaderboard the win rate, peak and last change. Root's Elo starts with the games
  reported from 2026-11-18 (owner's ask); its finished games before then are in the seed with every seat (bots too) as "*", no one,
  so they count in the stats but in no rating (`Result.rated`; `STATS` lines of `LiveCheck <hrf-games> ranking`). When a game ends,
  the rating changes it made are added as a line at the end of its log (`showRatings` in `hrf.scala`). Games with fan content
  (`Meta.rankingFan`: Northgard's fan clans only, the Lion Kingdom and the Dragon and Bear variants, `rankFan` in `nort/meta.scala`, as its fan modules and options are rated since 2026-11-21 (owner: much modular content, few players); Root's fan, playtest and mirror
  factions, fan maps, decks, hirelings and scenarios, `rankFan` in `root/meta.scala`; asked for on 2026-11-18) are reported with an
  "X fan" line, kept as the side `X:fan`, and count in the stats but in no rating (`Result.fan`). Ranked or unranked (asked for on 2026-11-22): an online Root or Northgard game's setup starts with a "Ranking" choice, Ranked
  (the default) or Unranked (`RankedGame`, `UnrankedGame`, each game's `RankingOption`, a core `RankingChoice` that `startSetup` in
  `hrf.scala` offers only online; Mirror never, as its factions are never rated). An unranked game is reported as a fan game: it
  counts in the stats, not in the ratings or win rates (owner, 2026-11-22). What isn't approved for ranked play is
  `Meta.rankingUnapproved` (Root's `unapproved` and `unapprovedPick`, which adds fan factions in Advanced Setup's pool; Northgard's
  fan clan variants, and the Lion Kingdom by faction): marked "unranked" beside the option on an online setup screen, and a Ranked game
  with any of it can't be started (`rankedCheck`, in `validateFactionSeatingOptions`). Root's Tidal Flats, Tundra and Gloom maps and
  the Dawn and Dusk deck are approved since 2026-11-22, and the Simple Landmarks and the Negabond (a second Vagabond, `approved`) since 2026-11-23 (owner). A ranked game with a bot gets a warning. The Discord bot's `/root` and
  `/northgard` take `ranked` (`LfgSetup.sections`), the catalog's tenth field marks each unapproved option (its menus say "makes the
  game unranked"), the post says Ranked or Unranked and why (`rankingLine`), and a Ranked game with unapproved picks is created
  Unranked (`rankedOptions`). Names are released or
  moved by committing `ranking-changes/<name>.txt` to `hrf-games` (`release <name>`, `rename <name> -> <new>`, `seat <user> <name>`, and `merge <name> -> <into>` for one player under two names, which gives `<into>` the first name's devices and seats and frees it, 2026-11-22;
  `Ranking.change`, applied once each by `Backup`, outcome in `<name>.result`).
  A name can have several player keys (each `names.tsv` row with a hash adds one, a row with an empty hash releases it), and
  its owner can set a code of their own under Rankings, "Set my own code" (8 characters or more, `codes.tsv`, salted SHA-256,
  never copied to `hrf-games`; `/set-code`): "Use a code from another device" then takes the name and that code (`/has-code`,
  `/sign-in`), which adds the device's key to the name; 5 wrong codes lock the name for an hour (`Ranking.wrong`, in memory).
  "Lost my code" sends a reset request (`/name-reset`, `Ranking.request`): the name, an optional message and what the server
  knows of the name go to `ranking/requests.tsv` (copied to `hrf-games`), and a GitHub issue labeled `name reset` gives only the
  request id, as issues are public; the owner releases the name with a `ranking-changes` file if satisfied (2026-11-19).
- Play Online's "Your Active Games" (`onlineGame` in `hrf.scala`, the server's `/my-games`) highlights in gold, with "Your turn", and lists
  first the games waiting on the signed-in player (asked for on 2026-11-20). The server doesn't run the games, so each client, once it has
  read the whole log and reached a question, posts the factions asked (`Journal.turn`, called from `Runner.run`'s `Ask` and `MultiAsk`
  cases; `/turn/<user>/<secret>/<game>`, kept in `turns.tsv` by `good-game/Turns.scala`), and `/my-games` matches them to the lobby's
  "user <faction> <user>" lines. A game nobody has opened since this was deployed shows no highlight until a player opens it. The tournament game ("Your Tournament Game", `/tournament/mine`, its seventh field from `Tournament.mine`) is highlighted the same way (2026-11-21). Each game in the list shows what's in it (2026-11-22, asked for): its map (Root's map, Northgard's central tile and expansion map tiles, the `webp2/lfg/emoji/` pictures, `Meta.overviewIcons`) and its factions' glyphs, in seating order (`overviewOf` in `hrf.scala`). A client reporting a turn adds the game's icons as a fourth line (`Meta.overview`, `Journal.turn`), kept in `overviews.tsv` (`Turns.setOverview`) and sent as `/my-games`' seventh field; for a game no client has reported, the server sends the lobby's seating and options instead and the client works them out (`Meta.overviewLobby`; Advanced Setup's seats draft their factions in the game, so those show the map only until someone opens it). The tournament game shows them too, from its game's reported icons (`/tournament/mine`'s eighth field, `Turns.overview`; 2026-11-22). Every game also says who else plays in it, "as <name> against <names>" like the tournament game: each seat's ranking name, else the name typed for it, and the bots by level, in seating order (`/my-games`' eleventh field, `othersOf` in `GoodGame.scala`), and another game's short name is NGUL, NGUL Adset or Root Adset (`myGame` in `hrf.scala`; owner, 2026-11-22). In Advanced Setup and Northgard's Adset, the drafted factions no seat took (and Adset's banned clan) follow the picked ones, grayed out ("off:<glyph>" in the overview, `hrf-overview-off`; `overviewOf` in `root/meta.scala` and `nort/meta.scala`; asked for on 2026-11-22). A finished game's report adds them too ("U <faction> ..." and "B <faction> ...", kept as the sides `U:` and `B:`; `Meta.rankingDraft`, `RankingSeat.draft`), and the games reported before have theirs in `good-game/ranking-drafts.tsv` (the `DRAFT` lines of `LiveCheck <hrf-games> ranking`, merged in by `Ranking.list`), for the Rankings' "Draft" tab: how often each faction drawn for a draft was picked (and banned), and its score per pick (`Elo.drafts`, `drafts` in `rankingsMenu`). A finished Root game's report also lists its landmarks and hirelings ("K <landmark> ..." and "H <hireling> ...", the sides `K:` and `H:`, not `L:`, a losing side; `Meta.rankingSetup`, `rankSetup` in `root/meta.scala`; asked for on 2026-11-23), the games before have theirs in `ranking-drafts.tsv` too (merged in per kind, so a game reported with a draft but before the landmarks still gets them), for Root's "Landmarks & Hirelings" tab: how many games had each and which factions won them (`Elo.setups`, `setups` in `rankingsMenu`). Each report also gives the map ("M <map>", the side `M:`; Root's map, the tournament's picked one, Northgard's central tile, `rankSetup` in `root/meta.scala` and `nort/meta.scala`; 2026-11-23, asked for), filled in for the games before in `ranking-drafts.tsv`; the tab is "Maps & Setup" for Root and "Maps" for Northgard.
  The games with a bot (the lobby's `bot` line, checked by `my-games`) or with fan content, which count toward no rating (`Meta.rankingFan`), are in an "Unranked Games" folder under them while they
  go on (finished ones go to Completed Games, marked "Unranked"; open by default while one waits on the player; 2026-11-21): the turn report adds a fifth line "fan" (`Runner.run`'s
  `unranked`), kept in `turns.tsv`'s fifth field (`Turns.fan`), and a finished game's result says it ("X:fan"); `/my-games` gives it as
  the tenth field. A game nobody has opened since that was deployed stays under Your Active Games until a player opens it.
- The main page (`/play`) has "My Online Games" and "My Account" above About (`topMenu` in `hrf.scala`; asked for on 2026-11-22),
  each its own page, `/play/my-games` and `/play/account` (served like a meta's page, `data-meta` naming them, and shown by a Root
  `HRFMetaUI` with `siteWide` set, whose Back loads `/play`). My Online Games is Play Online's list for every game (`onlineGame` with
  `siteWide`: the account row opens My Account, no New Online Game or Archived Games, "Start a new online game" buttons opening each
  game's `/play/<meta>/online/new`, Root's being Root Adset's, 2026-11-23). My Account (`myAccount`) gathers the names this device holds, each name's rating on every ladder
  (from `/rankings/<metas>`, tapping opens that game's Rankings), Discord and turn pings, other devices and codes, Switch account and
  Sign out; Play Online's Account and the Rankings' player code section stay. Client only.
- Fixing a running game: through the `hrf-games` repository (copy `games/<id>.tsv` to
  `fixes/<id>.tsv` and edit it; its README and `notes/server.md` say how).
- The in-game "Report a Bug" button posts to `/report-bug`; the server saves the report,
  copies it to `bug-reports/` in `hrf-games` and opens a GitHub issue labeled `bug report`.
  Reports never include the page URL, because player links contain the player's secret.
- Under "Report a Bug", every game's in-game menu has a "Main Menu" button (asked for on
  2026-10-13): it loads `/play/<meta>` afresh (`returnToTitle` in `hrf.scala`), where online and saved
  local games can be picked up again; a game kept only in memory (Training Grounds) asks first.

## Several sessions or accounts

More than one Claude account (or person) can work on the site. Each needs
write access to `guarantee418/haunt-roll-fail` (a GitHub collaborator) and
GitHub connected in Claude (https://claude.ai/connect-github); the Claude
GitHub App is already installed on the repo. Nothing else is needed: this
file covers the build and deploy, and `notes/server.md` in `hrf-games` the
server. No session can reach the server, and none needs to.

- Anyone with write access can deploy, and so restart the live server, by
  pushing the `deploy` branch. Only give write access to people trusted with
  that.
- Work on separate branches and merge through pull requests; don't have two
  sessions pushing to `main`.
- Don't edit the same area in parallel (e.g. two sessions both in `nort/`).
- The committed build output (`hrf-opt.js`, `hrf-opt.js.map`,
  `hrf-opt/main.js`, `hrf-opt/main.js.map`) conflicts whenever two branches
  both change client code. Never merge those files by hand: resolve the
  source conflicts, run `sbt fullOptJS` again, and commit the new build.

## Gotchas

- Over https (or on `localhost`), the loaders in
  `haunt-roll-fail/loader.scala` store files in Cache Storage with
  `cache.add`. When that rejects (a missing file or a failed request) they
  load the file directly instead. Without that fallback the game hangs on
  "Loading assets". Over plain http from an IP there is no secure context,
  `window.caches` is undefined, and the loaders always fetch directly. Never
  call `dom.window.caches.toOption.get` unguarded: it gives a black screen
  with `None.get` in the console. Test changes to the loaders both on
  `localhost` (cache path) and through a non-localhost http address (no
  cache).
- Browsers keep every image in Cache Storage under `hrf-image-cache-<imageDataVersion>`
  (`HRF.imageDataVersion` in `haunt-roll-fail/hrf.scala`) and never refetch it. When an image
  changes but keeps its file name, bump `imageDataVersion`, or players keep seeing the old one
  (the large Northgard buildings kept their black corners that way). Older caches are deleted
  on load.
- Northgard building space positions (`small`, `large`, `carved` in `nort/tiles.scala`) were
  measured from the printed slot frames on 2026-10-06; after moving any, rerun
  `nort/tools/tile-masks.py` for the grid (it regenerates the photo tiles' masks slightly
  differently on other library versions, so keep only the tiles you changed).
- The 23 Squires and Disciples deck cards (`SquiresDeck`, listed in
  `effectsSquires` in `haunt-roll-fail/root/cards.scala`) have images in
  `webp2/root/images/card/deck/`, `apprentice.webp` through
  `the-faithful.webp`. They come from the Leder Card Library
  (https://cards.ledergames.com/, data at
  `https://ledercards.netlify.app/cards.min.json`, images under
  `https://ledercards.netlify.app/cards/root/en-US/`), cropped and resized
  to the 512x708 size of the other deck cards.
- The Squires and Disciples card effects are in `root/deck-squires.scala`
  (`SquiresDeckExpansion`), with small hooks in `turn.scala` (Shadow Council,
  Brazen Demagogue), `battle.scala` (The Faithful, Friend ambushes),
  `game.scala` (Silver-Tongue rule, Brazen Demagogue scoring, Friend crafting
  limit) and `ui.scala`. Rulings follow the Root Database FAQ. Deliberate
  simplifications:
  - Friend of the ___: once per turn, on your turn, an X card in hand is
    replaced by a `FriendCard` of the chosen suit until the end of the turn
    (`revertDisguises` at `CleanUpAction`). On other players' turns it
    works for ambushes, and for paying a Squires card's cost with a card of
    the Friend's suit as any suit (`PayFriend`, offered by `payments`, e.g.
    Mice-in-a-Bush, which only took cards of the clearing's suit until
    Wood through Conquest on 2026-10-27; it only adds choices, so no option). A Dominance card can be disguised too, to be spent as
    another suit (e.g. to take a Dominance of that suit; 2026-10-21);
    spent, it goes to the available Dominances when the disguise ends.
  - Silver-Tongue: in games with the hidden default option `SilverTongueOneEffect` (2026-10-30) the chosen
    clearing counts as ruled for one effect, as the card says: it ends when the next recorded move made while it holds, and
    the questions it asks on the way (the Council's Assemble asks for Loyalists before it checks the ruling; 2026-10-30),
    come back to a menu, at the first `soft()` (`silverTongueOneEffect` in `root/deck-squires.scala`, which wraps the `Void`
    `loggedPerform` hands on, so replays stop at the same point), and a
    Negotiations there asks no card for the clearing's other ruler. Older games keep it until the end of the current phase,
    and their Negotiations still asks a card for the other faction that rules the clearing; a running game can be switched over
    with `SilverTongueOneEffectAction(X)` (done for Destiny in Right on 2026-10-30).
  - Brazen Demagogue: the dominance can be activated only right when it is
    taken; the player keeps scoring (`demagogue` flag in `FactionState`).
  - Feather Rufflers, Spy Network, Silver-Tongue and Friend are usable from
    the Birdsong, Daylight and Evening menus on your own turn.
  - Feather Rufflers is resolved when crafted in games with the hidden default option
    `FeatherRufflersOnCraft` (`root/meta.scala`, `FeatherRufflersCraftedAction` in `root/deck-squires.scala`):
    it places its two warriors in a clearing with the crafter's pieces and is discarded, as its parchment text
    box (like the Favor cards') says (2026-10-30: the Knaves crafted it with Apprentice and it stayed
    among their crafted cards). Older games keep it as a crafted card used later from those menus (and returned to hand to pay
    for a Squires card); a running one can be switched over with `FeatherRufflersOnCraftAction(X)` (the Air versus Might,
    2026-11-04, the Council crafting it again after returning it).
- The Dawn and Dusk deck (`effectsDusk` in `root/cards.scala`, effects in `root/deck-dusk.scala` and hooks in `battle.scala`,
  `turn.scala`, `craft.scala`, `movement.scala`) was checked against its 31 printed cards (`webp2/root/images/card/dusk/`) on
  2026-11-13 (it "had a lot of bugs"). New games get the hidden default option `DuskDeckFixes`: Fowl Play is in the deck (it was
  left out, with no reroll; `Deck.fowlPlay`, last in `Deck.catalog`, as saved games name cards by their place there): after the roll
  the attacker may pick a die and discard a bird card or Fowl Play to reroll it, again after each reroll (`FowlPlayAskAction` in
  `battle.scala`); Sly Dog is asked only when the attacker could battle another enemy there, and a skip passes on that battle alone
  (`SlyDogSkipAction`; it skipped Sly Dog for the rest of the turn); Old Friend is asked only for a draw of a card or more with cards in
  the discard pile, and is offered beside Informants at the Evening draw; a defender's Guard and Hoarders are offered only when they
  can save a piece, as an attacker's are. Readings kept: Public Relations is one more card of hand limit, Dusk Awakening draws after the
  hand limit, and Service of the Foxes, Rabbits and Mice must be used at the next Birdsong (no "may"). `ReplayCheck <games> all dusk`
  plays it with every official faction and counts the cards' choices (empty regions are left out of its comparison, as the cards'
  `game.dawn`, `noon` and `dusk` are registered lazily).
- The fan-made Doomed Expansion for Root (slugfacekillah; BGG 482234, PnP files on Google Drive, rules "LAW of DOOM", section D3;
  asked for on 2026-11-13, its three "Decks & Things" first): the Frankendeck is the Deck choice "Frankendeck (fan-made)"
  (`FrankenDeck` in `root/meta.scala`): a core of 30 in its own art (`card/franken/`, cut from the PnP 63x88 PNGs with their
  bleed trimmed; the 20 items are `Deck.frankenItems`, its Sabotage! is a Saboteurs card, and the ambushes and dominances are the
  usual cards drawn with the Franken art, `Deck.art` and `Deck.frankenArt`, set by `Game`) plus 30 persistent effects of the
  Might and Right (no Favors), Exiles and Partisans, Dawn and Dusk and Squires and Disciples decks, drawn at setup by suit and
  cost (`frankenRecipe`, `FrankenDeckAction` in `root/setup.scala`, before the map); the rest go out of the game. Its back is
  `card-back-art-fd`. `ReplayCheck <games> franken` plays it. The Action! Deck Booster (D3.2) is the toggle "Action! Deck Booster (fan-made)"
  under Deck, offered with 5 or more players (`ActionBooster`): its 10 cards (`Deck.actionBooster`, `ActionCard` in `root/deck-action.scala`,
  art `card/action/`) go into the deck once for each player above 5, once with 5, twice at most (`Deck.boosters`). An Action! card is spent from
  its owner's menus on their turn (`ActionCardMainAction`; a move or battle it starts is paid when done, `ActionPayAction`, like the Squires),
  the sword's extra hit also in a battle they are in (`ActionSwordHitAction`, in `BattlePostRollAction`); the bag adds 2 to the hand limit
  that turn (`HandLimitAction`); the Secret! cards are birds, either discarded to draw the top card, the Cancel one a counter-ambush
  (`askCounterAmbush`), the Score one 3 VP at the start of Birdsong (`SecretScoreAction`). Using a card as its item isn't done.
  `ReplayCheck <games> six action` (or `seven`) plays it. The Dark Deck (D3.1) is the Deck choice "Dark Deck (fan-made)" (`DarkDeck`):
  its 66 cards (`Deck.darkDeck`, art `card/dark/`, back `card-back-art-dd`), the Abomination suit (counts as fox, rabbit or mouse,
  never bird; `SuitAsset.matches`), the Cursed card (can't be spent or discarded, `DirectDiscard` leaves it in hand; crafted with 4 of
  any suit for 3 VP and the Cursed Idol, then out of the game), Dark Dominance (the Idol and 3 connected ruled clearings), the dark items
  added to the shared supply (Torch, Book of Names, Rabbit's Paw, Cursed Idol; `Item.title` is their display name, `name` what saves
  write) and the persistent effects, all in `root/deck-dark.scala` (`DarkDeckExpansion`, its header lists the readings). Traps and the
  Golden Knight is drawn on the map (`GoldenKnightMarker`, display only). Two pieces are ownerless-style hirelings added when their card is
  crafted (`ensure`): Bear Traps' two traps are tokens of the card's owner that can be battled (`TrapLayer`, owner `game.trapOwner`, which
  `HirelingEx.owner` reads, as an effect would make the hireling transfers hand it on), and Rodent Zero's 20 Infected are their own player
  (`InfectedHorde`, no owner, so asks for it go to the current player), defenders that always take the higher roll, counting for rule
  (`DarkDeckExpansion.plague` beside `game.unhired`), spreading at the end of the owner's Evening (at `CleanUpAction`, as the factions'
  expansions take `EveningEndAction` first) with the Hundreds' suit die. Neither is listed with the hirelings. The Vagabond's Book of
  Names (`HeroPlayer.named`, allied until the end of the turn, its marker unmoved), Rabbit's Paw (`HeroPlayer.paw`, in Daylight any other item)
  and Cursed Idol (never exhausted, damaged or discarded: `ToExhaust`, `ToDamage`, `ToDiscard` in `root/items.scala`; it counts
  toward the Evening item limit but is never the one dropped) follow Law of Doom D3.1.2; with Book of Names, aiding the named faction
  scores 1 and leaves its attitude unmoved, and Infamy in a battle alongside it scores every second time (`namedInfamy`, half rounded down).
  `ReplayCheck <games> base dark` plays it.
  The Doomed Vagabonds (D7) are the toggle "Doomed Vagabonds (fan-made)" under the Vagabond (`DoomedVagabonds` in `root/meta.scala`):
  seven more characters, Bard, Barkeep, Berserker, Blacksmith, Swindler, Zealot and Grouch (`DoomedCharacters` and
  `DoomedVagabondsExpansion` in `root/doomed-vagabonds.scala`, its header lists the readings), their starting items and special actions
  from the cards (Inspire takes hits with cards matching the clearing; Raging, Hammertime, Encouragement, Grift, Devour and Scram! are
  Daylight actions). Their pictures and titles are cut from the PnP cards by `root/tools/doomed-vb.py` (the art keeps the card's
  background, as there is no cut-out). `ReplayCheck <games> vb doomedvb` plays two Vagabonds a game and takes their special actions
  whenever they are offered.
  The Doomed Knave Captains (D6) are the toggle "Doomed Knave Captains (fan-made)" under the Knaves (`DoomedCaptains` in `root/meta.scala`):
  six more Captain cards, Bard, Barkeep, Berserker, Blacksmith, Swindler and Zealot (`Knaves.doomed` in `root/faction-knaves.scala`, its
  comment lists the readings), dealt with the twelve official ones (`Knaves.captainsFor`). Their pieces are the stickers' heads, cut by
  `root/tools/doomed-captains.py`. `ReplayCheck <games> kd doomedcap` picks them whenever dealt and takes their choices whenever offered.
  The Turnlings (D8) are eight double-sided hireling cards, sixteen hirelings offered in their own folded sections, "Staunch Turnlings
  (fan-made)" and "Demoted Turnlings (fan-made)", none picked by default (`Meta.turnlings`, apart from `Meta.hirelings`), and the toggle
  "Turn Over Hirelings" (`TurnOverHirelings`): a player reaching a hireling marker gives the hireling to another player, who rolls for
  its control (`TurnOverHirelingAction`). All of it is in `root/turnlings.scala` (`TurnlingsExpansion`, before `HirelingsExpansion`; its
  header lists the readings): Whisperers in forests, the Infected (their own warriors here, not the Dark Deck's `InfectedHorde`), the
  Golden Knight, Cardinals and Caravans, Doomsayers and the Flame, Vultures, Bugs, Grunts and Fine Swine, Forest Connections
  (`ForestShortcut` in `game.transports`), Bottom Feeder, Golden Pig, Corporate Overreach's Demand die and Miracle Cure-All (copies a
  crafted card through `dopple`). The Infected and the Golden Knight aren't drawn with the Dark Deck. Card art and pieces are cut from
  the PnP cards and stickers by `root/tools/turnlings-art.py` (`card/<style>-card`, `turnling-*`). `ReplayCheck <games> base
  hire:TMM,TIN,... turnover turn` plays them and takes their choices half the times they are offered.
  The Legends Scenarios (D5) are the folded choice "Legends Scenario (fan-made)" (`LegendsScenario`, `Meta.legendsOptions`): a
  thirteenth clearing, the scenario space (`LegendSpace`, `Clearing(0)`, registered by every `FactionState` and in
  `game.clearings` once placed, `game.limboClearings` for the Limbo loops), put at setup in a forest beside the most inner clearings
  (`game.legendForest`) and linked by trails (`LegendsExpansion.connected`, `linked` in `Roads` and `ForestRoads`), drawn at that
  forest's name (`center` in `root/ui.scala`). Castles & Catacombs and The Fifth Ruin (its Ring, a ninth item `Ring`, with eight Ring
  rules and five capture rules) are in `root/legends.scala`; Dark Rituals and Hunt the Legend, with the five Legendary Monsters (their
  own player `LegendBeast`, health in `game.monsterHealth`, roaming at the start of each Evening from round 2, blocking moves out of
  their clearing until appeased, slain at the ghost face for their weapon items `Mauler` ... `Bushblazer`), in
  `root/legends-monsters.scala`. Both headers list the readings. Art is cut from the PnP sticker sheet and cards by
  `root/tools/legends-art.py` (`webp2/root/images/legends/`, the items in `item/`). `ReplayCheck <games> base castles legend`,
  `vb fifth ring:<Rule> capture:<Rule> legend`, `base rituals monster:Pick<Monster> legend` and `vb hunt legend` play them.
  The Doomed factions are three fan factions in the picker's Fun Factions (`Meta.fun`), with their art cut from the PnP boards and token
  sheets by `root/tools/doomed-factions-art.py` (warriors from the Turnlings' pieces) and a simple bot (`GameEvaluationDoomed` in
  `root/bots/bot-doomed.scala`); each file's header lists the readings. The Warriors Wake (WW, `root/faction-wake.scala`): Vultures,
  Despair tokens (no buildings in their clearings, `Game.freeSlots`), the Mess (warriors removed in their Frenzy go there, `game.wakeFrenzy`
  in `game.dead`), Bolster, Converge, Frenzy, Feast, Consume and Scavenge; the discard pile is hidden from the others (Hoarders). The
  Doomsayers Union (DU, `root/faction-doomsayers.scala`): demand dice (`DemandDie`, black, orange, yellow), the union card (strikes,
  locks, Cats on Strike!), Recruit, Renegotiate, Crusade, Consecrate and Craft with revealed cards scored in Evening, Flames in enclosed
  forests (no enemy moves there, `validDest`) and The End, their win when no Flame is left to light. The Whisperers Guild (WG,
  `root/faction-whisperers.scala`, only with 5 or more players, `validateFactionCombination`): warriors in the forests, face-down Insight
  markers known only to the Guild (`insight`, shown to it in the action pane, `Game.viewInsight`, and to everyone as the hex back), revealed
  on other players' turns: `WhisperersExpansion` is second in `expansionsFor` and looks before every phase step (`TopLevelAction`) at the
  buildings placed, crafts made and removals since (`watch`), offering what they set off. The Favored Suit counts cards discarded from an
  enemy's hand (`WhisperersExpansion.discarded`, called from `DirectDiscard`), Midnight actions come at the start of a turn after another
  player's, and Instigate's points go to the Guild. `ReplayCheck <games> ww`, `du` and `wg` (5 and 6 players) play them.
  The Re-Imaginings are four more Fun Factions, each file's header listing its readings, art by `root/tools/reimaginings-art.py`.
  Eyrie's End (EE, `root/faction-eyries-end.scala`) is an `Aviary` (the Eyrie's Decree, Turmoil and leaders, with the new leaders Patron,
  Megalomaniac and Hedonistic in `faction-aviary.scala`): five Cardinals drawn at random with its warriors, which other factions may move as
  their own on their turn (The Red Menace, added to every `MoveInitAction` by `EyriesEndExpansion.redMenace`), caravans that craft and move
  with its warriors, the clearing's ruler drawing a card (Roving Caravans: the transport marker `CaravanDrawn` keeps it to once a move), and
  rule where tied on its turn (Lords of Corruption, in `ruleValue`); A New Roost places 3 warriors, as the rules say, and the board picture was changed from "up to 6" to 3 (2026-11-15; `new_roost_3` in the art script). Dawn of the Marquistadors (DM, `root/faction-marquistadors.scala`): the
  ship landing on an edge river clearing, outposts placed with two matching treaties (cards kept face up), treaties counted as warriors in
  battle, wood moved to the ship for the Keep (all 8 win). The Doomed Vagabond (DV, `root/faction-doomed-vagabond.scala`, 4 or more players)
  is a `Hero`: Despot Infamy (`faction-hero.scala`), peace offers with Hostile factions and the Collect Bounty quests (in `Quest.all`, so
  saves name them) offered in every menu of the current faction (`DoomedVagabondExpansion.anytime`, beside the Lilypad Negotiations in
  `internalPerform`), quests freshened each Evening. The Naifs of the Deep Woods (NW, `faction-knaves.scala`) are the Knaves with Alliance
  warriors (`Knaves.warriorName`). `ReplayCheck <games> ee`, `dm`, `dv` and `nw` play them.
  The Urban Legends maps (the Drive's "And what is THIS" folder; 2026-11-17) are two more Map choices, "Urban (fan-made)" and
  "Legends (fan-made)" (`UrbanMap`, `LegendsMap`; `UrbanBoard` and `LegendsBoard` in `root/maps.scala`, images in `urban/` and `legends-map/`,
  as `legends/` holds the Legends Scenarios art), also drawn by "Random" (not "Random Official"). The boards print no names, so the clearing
  and forest names are made up (Lumberyard, not Sawmill, as region ids must not clash with piece names); Urban's ruins are its three R slots and
  the 0 slot, Legends' its four keeps, and a Legends Scenario's space is always in Legends' stone circle (`Board.scenarioForest`).
  The regions, water masks and names are made by `root/tools/urban-legends-art.py`. `ReplayCheck <games> urban` or `legendsmap` plays them.
  The seven Doomed factions and Re-Imaginings are in Advanced Setup's faction pool too, off by default like the other fan factions
  (`AdvancedSetup.doomed`; 2026-11-17). With one of them in the pool, the draw leaves out sets that hold factions that can't play
  together, or the Whisperers Guild under 5 seats or the Doomed Vagabond under 4 (`AdvancedSetup.clash`); pools without one draw as before.
  `ReplayCheck <games> doomed adset` plays Advanced Setup games, the seats drafting at random.
- The Outsiders Expansion for Root (slugfacekillah; BGG's top-rated fan expansion; PnP files on Google Drive, rules "Law of Slug"; asked for on 2026-11-17) is being added one part at a time; Croakers Coven (`XC`) and the Grouch Vagabond were already on the site. Art is cut
  by `root/tools/outsiders-art.py` from the token sheets and boards, and the warriors from the meeple file "Meeples.svg" (Outsiders meeples
  by The Irreverend; its header says how to render it). Bone Patrol! (BP, `root/faction-bone-patrol.scala`, its header lists the readings)
  is a Fun Faction and in Advanced Setup's pool (`AdvancedSetup.doomed`): pugs (`Pug`) led by the Alpha (`Alpha`, never removed, an extra
  hit attacking, absorbs one hit where it is, Protective Breed; always rules its clearing, 99999 in `ruleValue`), stashes, and twelve bones,
  `Bone(suit)` face up (a token moving with the warriors, `BoneMove`) or `BuriedBone(suit)` face down (Tenacious, never hittable or
  attackable: `BonePatrolExpansion.hittable` in the battle's hit assignment, `validAttackTarget`), with the Rollover and the Bone Collection
  as locations of the player (`rollover`, `collection`). Obedient moves (`ObedientMove`) start or end at the Alpha; Guard Dogs is offered
  in every phase menu of the other players (`guardDogs`, shared rule in `rules` through `BonePatrolExpansion.guarded`, cleared at their
  `CleanUpAction`). `ReplayCheck <games> bp` plays it.
  The Winged Menace (WM, `root/faction-winged-menace.scala`, its header lists the readings) is a Fun Faction and in Advanced Setup's
  pool: bats (`Bat`) spawning in The Nexus (`TheNexus`, a region of its own next to the four caves, `WingedMenaceExpansion.adjacent`),
  the ruins turned into caves (`game.caves`, drawn as `Cave` and taking their slot when emptied, `freeSlots0`), Fight or Fright picked at
  each battle's start and revealed before the hits (Fright moves the enemy's warriors instead, `MenaceFrightAction`), Squatters
  (an infestation where it removes an enemy building), infestations on two rows (`top`, `bottom`: Spawn and draw bonuses), Guano as
  its own ownerless player (`GuanoMess`, not listed with the hirelings) and Dark Passage, offered in every phase menu of the other
  players. `ReplayCheck <games> wm` plays it.
  The Order of the Forest (OF, `root/faction-order-forest.scala`, its header lists the readings) is a Fun Faction and in Advanced
  Setup's pool: warriors (`Devotee`) and four suited High Priest pawns (`HighPriest(suit)`, warriors in battle removed last, `Rearguard`,
  never ruling, `case _ : HighPriest => 0` in `ruleValue`, moving alone ignoring rule, `PriestsMove`), the Foreseen and the Order Card
  (`foreseen`, `order` card piles; Daylight actions only in clearings matching it), Minister (soup and coins tokens), Witness (its hits
  swap enemy warriors for its own, the swapped ones going to The Ranks, `ranks`, a location of the Order holding other factions'
  figures, scored at Rest), abbeys (`OrderAbbey(suit)`) and Congregate (crafting with pawns and abbeys of their own suit, in one clearing,
  `pick`), Martyrs (removed pawns go to `altar`, each giving its suit's ability), Power of Faith (tokens add to its hits) and Reclamation
  (an enemy removing its warriors on its turn may return as many of one player's from the Ranks, at `ForcedRemoveCleanupAction`).
  `ReplayCheck <games> of` plays it.
  The Twelve Colonies (TW, `root/faction-twelve-colonies.scala`, its header lists the readings; "TC" is the Twilight Council's, so its
  images are `tw-*` in `faction/colonies/`) is a Fun Faction and in Advanced Setup's pool: snakes (`Snake`, warriors) and turtles
  (`Turtle`, warriors dealing no hits, Hard Shelled; on a path they are in `onPaths` and counted in `paths`, drawn at the path's middle,
  and any other player moving along that path by trail pays a card matching one of its clearings, `TWTollAction`), Capitols and Commons
  (Colonize, Recruit with ballots, Claim), and the Democratic Process each Birdsong: the one of 3 Laws (of 16, `ColonyLaw`, art `tw-law-<name>`)
  drawn and kept, nominations and seconds for an Election (`Nominee`), two voting rounds with ballots from the Public Trust (bought
  by removing warriors), the Colonies breaking ties. A Law in effect reaches the rest of the game through `TwelveColoniesExpansion`'s
  checks: `battleAllowed` in `canAttack` (the Paxes, Omnius Exitus), `usable` in `canPlace` and `passable` in `Roads`, `artifex` in
  `craftableWith`, `ruleless` in `RuledMove`, `noWild` in `canAmbushWith`. `ReplayCheck <games> tw` plays it.
  Old Man Tinker (OMT, `root/faction-old-man-tinker.scala`, its header lists the readings; images in `faction/tinker/`) is a Fun Faction
  and in Advanced Setup's pool: three robot pawns (`Robot`: Wolfbot, Birdbot, Owlbot, never hit, `OldManTinkerExpansion.hittable` in the
  battle's hit assignment) whose R.S.I. gears are warriors of their own in the robot's clearing (`RobotGear(robot)`, moving only all
  together with the robot, `RobotMove`; a hit turns one into a gear token there), gear tokens (`Gear`) and drones (`Drone`, warriors)
  sharing twenty tokens (`tokensLeft`), the Old Man (`OldMan`, a token that moves into forests too, so the player registers the forests as
  locations), factories, Production (level 0 to 2, gears or drones), the Breakage Dice, Bustle, Tinker, Repair, Overproduce, Restructure
  and Do-Over. `ReplayCheck <games> omt` plays it.
  Its Upstarts & Renegades deck is the Fan Deck "Upstarts & Renegades" (`UpstartsDeck`; 2026-11-18): the base deck's ambushes, items and
  dominances (its cards 1 to 29 are those) and 25 persistent effects (`Deck.upstarts`, art `card/upstarts/` and back `card-back-art-ur` cut
  by `root/tools/upstarts-art.py`), all in `root/deck-upstarts.scala` (`UpstartsDeckExpansion`, before the other decks' expansions; its header
  lists the readings; its Sabotage! cards are the Saboteurs, so `ExilesDeckExpansion` is on with it). State is `game.upstarts`. The Murder Hawk
  is a warrior of the crafter (`MurderHawk`, waiting in each faction's `HawkPerch`; moving with it ignores rule, `RuledMove`), the warriors on
  Baron Von Costume sit in `OnCostume`, the Trailblazers' trail is a transport (`TrailMove`, only in games with the deck), the Escorts' ruled
  clearing goes through `rules` (`escortRules`), Cooperative Housing adds a slot (`game.slots`), and one check before every recorded action
  scores its clearing's new enemy buildings, discards the Murder Hawk's card when the hawk is removed and Baron Von Costume when none of its
  owner's warriors are left on it. `ReplayCheck <games> base upstarts` plays it; `upstartsall` deals every effect out crafted at the first
  Birdsong (the hidden `UpstartsTestDeal`) and counts their choices.
- The fan deck "Sorcery of the Enchanted Woods" (`SorceryDeck`, a Fan Deck; 2026-11-19, asked for on Discord as "the Magic deck") is from
  the Tabletop Simulator mod 2413287097 (uploaded by slugfacekillah, "Not Mine": its designer isn't named; no rulebook, the card text is
  the rules): the base deck's ambushes, items and dominances, 25 persistent effects (`Deck.sorcery`) and a fifth, bird Dominance for six
  crafted improvements (`SorceryDominance`, last in `Deck.catalog`), all in `root/deck-sorcery.scala` (`SorceryDeckExpansion`; its header
  lists the readings). Art `card/sorcery/`, back `card-back-art-se` and the Garden of Glory and Wizards Tower pieces (`GloryGarden`, a
  token, and `WizardsTower`, a building, waiting off the map in `SorceryShelf`) are cut from the mod's sheet by `root/tools/sorcery-art.py`.
  State is `game.sorcery`. Living Buildings counts its owner's buildings twice in `ruleValue` and adds a rolled hit in battle; The Wizards
  Tower is a crafting piece of any suit (`frogCraft`); Treewalking is the `TreeWalk` transport; Hailstorm's picks are recorded one clearing at
  a time (`game.sorcery.hail`), as a soft pick made the bots try every combination. `ReplayCheck <games> base sorcery` plays it;
  `sorceryall` deals every effect but Hailstorm out crafted at the first Birdsong (the hidden `SorceryTestDeal`) and counts their choices.
  Its two maps are Fan Maps, "Deep Woods" and "Wastelands" (`DeepWoodsMap`, `WastelandsMap`; `DeepWoodsBoard` and `WastelandsBoard` in
  `root/maps.scala`, images in `deep-woods/` and `wastelands/` made by `root/tools/outsiders-maps.py`, which also prints the Deep Woods
  building slots), also drawn by "Random". Clearing and forest names are made up. Their own rules are toggles in the setup's "Fan Map
  Rules", shown only with their map (2026-11-17), unfolded right under the maps, both on by default (Settling 2026-11-18, Water and Barren Trails 2026-11-19; `defaultsFor`, and
  `Meta.settlingOn` in `completeSaved` for setups saved without that map picked), all in `root/outsiders-maps.scala` (`OutsidersMaps`, its header lists the
  readings). Off, the Deep Woods is played settled, each clearing tile baked into the art on the clearing of its number (Law of Slug
  S.1.7), and the Wastelands' barren trails are plain paths. "Deep Woods - Settling" (`DeepWoodsSettling`, S.1): clearings start
  unsettled (wild, crafting pieces pay for nothing, `Unusable` in `root/game.scala`; one slot; two pieces a player), drawn with their
  printed circle (`unsettled-<n>`) until a card settles them with a face-down tile (`tile-<n>`, `game.settled`; slots from
  `DeepWoodsBoard.tileSlots`, `OutsidersMaps.gates` for the UI); a move that would bring a third piece settles first (`mayEnter`), a
  placement is checked before the next step (`TopLevelAction`). "Wastelands - Water and Barren Trails" (`WastelandsWater`, S.2): Water
  (`game.water`, `waterSupply`), the `BarrenTrail` transport (paid with a Water, a staying warrior or a Vagabond item), the Water
  actions, Surrender and Seizure. The "Map Rules" button under the action pane (and tapping a barren trail) shows the rules and where
  the game stands (`"view-map-rules"` in `root/ui.scala`); it shows on both maps, saying so when their rules are off (2026-11-19). `ReplayCheck <games> deepwoods [settling]` or `wastelands [water]` plays them.
- The Capitalists and Anarchists deck (`CapitalistsDeck`, among the Decks since 2026-11-21, a Fan Deck before; the owner's own variation of Might and Right, designed in a thread on
  2026-11-19) keeps the base deck's items, ambushes, dominances and Armorers, puts Dawn and Dusk's three Services in place of the
  Favors, and changes the other effects (`Deck.capitalists`; rules in `root/deck-capitalists.scala`, `CapitalistsDeckExpansion`, its header
  lists them): Command Warren, Cobbler, Brutal Tactics (no point for the defender), Sappers (as printed, 1 mouse) and Scouting Party (cancels one ambush; 1 mouse) cost one card and are paid for at each use with a card matching the clearing (a bird
  matches) or by discarding them; Better Burrow Bank (1 rabbit) is optional and goes back to hand; Stand and Deliver! (1 mouse) must be used at the start of the next Birdsong
  and is discarded, the enemy drawing a card instead of scoring; Tax Collector (1 of any suit, draws two) and Codebreakers are used once when crafted; Royal Claim
  must be used at the next Birdsong. The effects are the base deck's objects, changed where `options.has(CapitalistsDeck)` (`turn.scala`,
  `battle.scala`, the effect hints in `root/ui.scala`). Pictures `card/capitalists/ca-*` by `root/tools/capitalists-art.py` (the base card
  with another card's cost plank and new text, a tan box for the once-used ones). `ReplayCheck <games> base capitalists` plays it;
  `capitalistsall` deals the effects out at the first Birdsong (the hidden `CapitalistsTestDeal`).
- The Noxious Battery (NX, `root/faction-noxious-battery.scala`, its header lists the readings; images `nx-*` in `faction/battery/`, cut by
  `root/tools/noxious-art.py` from the print-and-play folder's ASSETS; S.P. Shaman's fan Nemesis Expansion, art by disasterman; asked for on
  2026-11-18) is a Fun Faction and in Advanced Setup's pool (reach 5): skunks (`Skunk`), a cannon pawn (`Cannon`, starting damaged on a ruin,
  `damaged` in `NoxiousBatteryPlayer`; damaged it isn't hit, removed or battled alone, `NoxiousBatteryExpansion.hittable` in the battle's hit
  assignment and `untouchable` in `validAttackTarget`; a move with it ignores rule, in `RuledMove`; removed undamaged by an enemy it flips, the
  enemy scores 1 and the Bore's cannonballs roll to adjacent clearings), cannonballs (`Cannonball`, in The Bore, `bore`, scoring 3, 2, 1 when
  fired; Keep on Rollin' rolls one an enemy removes to an adjacent clearing), scent marks (`ScentMark`; Foul Stench moves each enemy's warriors
  and pawns out of their clearings at its `CleanUpAction`). Birdsong: Levy, Fire! (any number of times), Repair; Daylight: craft with scent
  marks, then 4 of Recruit, Battle, Rollout (`CannonballMove`), Rearm; Evening: Spray, Load, draw. `ReplayCheck <games> nx` plays it.
- United Dove Corps ii (UDC, `root/faction-dove-corps.scala`) and Arachnid Association ii (AA, `root/faction-arachnid-association.scala`) are Fan
  Factions and in Advanced Setup's pool (2026-11-19; asked for from a Discord post). There is no rulebook: the rules are their faction boards and
  Advanced Setup cards in the TTS mod "Root - Ultimate Collection" (its `Fan Factions` block), and each header lists the readings; the ii versions
  were picked over the originals. Art in `faction/dove/` and `faction/arachnid/`, cut by `root/tools/dove-arachnid-art.py`; their ? links the Root
  Database page. The Corps: ceasefire and peace tokens (`UnitedDoveCorpsExpansion.battleAllowed` in `canAttack`, `protectedFrom` in `canRemove`),
  refugee cards, a Mandate each Daylight. The Association: adult spiders and spiderlings (`SpiderMove` lets a move take one adult), webs in
  clearings (`SpiderWebFull` holds a captive) and on paths (`paths`, drawn at the path's middle), captives in `cocoon` (`held` says on which web),
  Hunters in `BattleDealHitAction`, path webs catching movers in `MoveListAction`, eggs, mutations (`AAMutation`; Agility is the `SpiderForest`
  transport), Weavers in `craftableWith`, Arachnophobia after the hand limit. `ReplayCheck <games> udc` or `aa` plays them, `dovearachnid adset`
  in Advanced Setup.
- A fan faction's in-game overlay (tapping its player panel) links the designer's print-and-play folder above "More Info"
  (`FanInfo.pnpLink`, the link its ? has; 2026-11-18). The fan decks' ? pictures are in the setup screen's image list
  (`original` in `hrf.scala`), as the setup screen loads no game assets: missing, the picture threw and the box had no Ok
  (2026-11-18); `showOverlay` now always shows the Ok.
- Every fan, playtest and mirror faction has a "?" (2026-11-17): who made it, then what it does and how it plays, a line
  each (`FanInfo.about` in `root/fan-info.scala`). The faction picker shows it as a "?" button beside the name (`Meta.factionInfo`,
  `xstyles.infoButton` in `pickElem`), and Advanced Setup's "Available Factions" toggles as the setup's usual ? (`IncludeFaction.explain`).
  Official factions have none. Display only.
  Every Doomed and Outsiders faction's ?, and the ? of the Doomed setup options (its decks, maps, Vagabonds, Captains, Turnlings and
  Legends Scenarios) and the Outsiders maps, link the designer's print-and-play folder for the whole expansion (`FanInfo.doomedLink`,
  `FanInfo.outsidersLink`; 2026-11-17 and 18): a new part of either gets that link too.
- Root's fan content is grouped apart from the official (2026-11-17): the faction picker's "Fan Factions" (once "Fun Factions",
  `factionGroup`) starts folded to one line ("18 Fan Factions") and opens on a tap (`Meta.foldedFactionGroups`, `grouped` in `hrf.scala`,
  remembered as `picker.<meta>.<group>`); the setup has "Fan Maps" (Tidal Flats, Tundra and Gloom too, since 2026-11-18) and "Fan Decks" sections (Mixed and Dawn and Dusk too, since 2026-11-18, and Might and Right since 2026-11-21, when Capitalists and Anarchists took its place among the Decks as the base deck's playable version; the three still count as official for the rankings) (`FanMap`, `FanDeck` in `root/meta.scala`:
  the same single choice as Map and Deck, through `OneOfGroup.oneOf`, so a folded Map shows "Urban (Fan Maps)"), Adset's "Available Fan
  Factions", and two folders (`BaseOption.folder`, a heading that folds several sections, `folder.<name>`): "Fan Hirelings" (the
  Turnlings) and "Scenarios" (the Legends Scenarios; Ring Rule, Capture Rule and the monster only once their scenario is picked,
  `Meta.optionShown`). A new fan map or deck extends `FanMap` or `FanDeck`, a fan faction goes in `Meta.fun`, a scenario sets
  `folder = Meta.scenarios`. Display only, as options are saved by name.
- The playtest Lilypad Diaspora versions (`LDvB`, `LDvC`, `LDvD`) and the Tidepool Diaspora (`TD`) are no longer offered
  (2026-11-17): they are `Meta.retiredFactions`, out of `Meta.factions`, but still parsed (`parseFaction`) and their `IncludeFaction`
  options kept (older Adset games list them), so older games load.
- The deck cards of the base, Exiles and Partisans and Squires and Disciples decks and the Homeland
  Lilypad Diaspora cards (`faction/invasive/card/hl-*`) have the newer text layout (no title in the text
  box) and the Homeland errata, from a player's Google Drive (2026-10-10). The shared deck's back follows
  the deck option (`deckBack` in `root/ui.scala`: `card-back-art`, `-ep`, `-sd`). The same Drive has
  the revised Council and Knaves boards (English and French), now `tc-board-h` and `kd-board-h`. The
  code already followed most of their changes (Banish hits only warriors and the Council picks where
  they go, Have at Thee! only for the Acting Captain, Run Away!'s forest picked by the enemy); since
  then, Empower's removed warriors wait in the supply (`empowered` in `CouncilPlayer`) until they go to
  the Loyalists, and Governors keeps the Knaves from crafting with a Captain or acclaim at a Governing
  assembly (Filch and Serve).
- Coffin Makers has three rules: forcibly removed warriors only (no option), any warrior returned to a
  supply (`UnthematicCoffinMakers`), or the Homeland errata, "warriors removed, not replaced, from the
  map to their supply" (`ErrataCoffinMakers`, the default and in the Official presets). In `root/game.scala`,
  `dead` is forcible removal, `recycle` any return, `removed` a removal from the map without being forced
  or replaced, and `unreplaced` the same for removals that went to the supply before the errata (Council's
  Empower). Replacements (Hundreds' Anoint and Mob, Lilypad's flips) and returns from off the map
  (officers, acolytes, payments, captives) don't go to Coffin Makers under the errata.
- The Gorge map (`GorgeBoard` in `root/maps.scala`, option `GorgeMap`) uses
  the plain board art the owner uploaded (2026-10-06, no score track, item slots
  or logo; `map-art.webp`), scaled and shifted onto the 2416x2214 frame of the
  earlier printed-board image so every position still fits. The board prints no clearing names, so
  the names (Ranch, Bluff, ...) are made up from the art; the layout card's
  numbers are 1 Ranch, 2 Mesa, 3 Camp, 4 Chapel, 5 Bluff, 6 Saloon,
  7 Lookout, 8 Rapids, 9 Homestead, 10 Forge, 11 Fork, 12 Pueblo. The dam
  path (Forge - Saloon) divides forests (Law of Root M.6.1), so the forests
  above and under it are adjacent in pairs (`acrossDam`: West - Center,
  North - East). The Fork - Pueblo path runs only through the dam's tunnel,
  so it makes no forests adjacent: Center and East (and West and North) are
  not, as a player pointed out on 2026-10-12. The bridge path (Homestead -
  Lookout) divides nothing.
- Root map drawing (`drawMap` in `root/ui.scala`): warriors of the same
  faction and kind in a region are drawn as one figure with a count badge
  (`stacks`, `drawCount`), like the Northgard unit counts. The in-game "Warriors" setting
  (`StackWarriors`, the default, or `SeparateWarriors` in `root/meta.scala`) can
  switch back to drawing every warrior. Every board has a score tracker
  along its bottom edge and an item tracker in its top left corner, where the
  printed boards have them: `scoreTrack` (centre of the 0 box, box spacing) and
  `itemSlots` in `Board` (`root/game.scala`), overridden for the board images
  of other sizes (Autumn, Winter, Mountain, Marsh) in `root/maps.scala`.
  `drawTrackers` draws the tracker image
  (`webp2/root/images/tracker/score-track.webp`, from the owner) there and
  `drawScoreTrack` puts each faction's VP marker on its
  score, stacked upwards when tied, with a count badge past 30. A clearing
  name that would cover the score tracker is drawn just above it instead
  (`nameOverTrack`). The in-game "Board Trackers" setting (`ShowBoardTrackers`, the
  default, or `HideBoardTrackers`) hides both trackers and their markers. Official VP marker art (`webp2/root/images/vp/`, `officialVP` in
  `root/ui.scala`) exists for the Homeland Twilight Council, Knaves and
  Lilypad Diaspora, cut from page 17 of `root-factions/`. The ten other Leder
  factions (Marquise, Eyrie, Alliance, Vagabond, Riverfolk, Lizards, Duchy,
  Corvids, Hundreds, Keepers) have markers made in the same style: the
  Council marker's laurel separated from its background, on a tile of the
  faction colour (the head colour darkened 8%), with the faction head from
  the Root Database (https://www.therootdatabase.com/,
  `/media/small_component_icons/custom/<animal>100.webp`, the same heads the
  official markers print). They aren't official art; swap in the official
  markers from Leder's print-and-play files if they become available.
  The mirror factions' markers recolour those heads to the mirror colour
  (Negabond's head is the Vagabond's in negative; Longtail Kaliph is a black
  rat on red). Fan factions are deliberately left on their `-glyph` head icon.
- The Gladiator, Cheat and Jailor Captain pieces
  (`webp2/root/images/faction/abduct/kd-captain-*.webp`) are the official
  Homeland meeples, rendered front-on from the 3D models (mesh plus texture)
  in the Tabletop Simulator mod "Root - Ultimate Collection - Homeland
  Expansion" (Steam workshop 3354438467; the save file comes from the
  `file_url` of Steam's public `GetPublishedFileDetails` API, BSON, the
  pieces are the `Vagabond - <name>` objects in its scripts).
- Lilypad Diaspora's Frog cards go into the shared deck only after every faction has set up, so no
  starting hand gets one (reported on Discord, 2026-10-10: the Knaves drew Settlers in a standard setup).
  New games get the hidden default option `FrogCardsAfterStartingHands` (`root/meta.scala`), and the shuffle
  then waits for `SetupDoneAction` (`faction-invasive-eee.scala`); games started before it keep shuffling them
  in during Lilypad's setup, so their replays don't change. `ReplayCheck <games> ldkd` checks starting hands
  (`oldfrogs` for the old timing).
  Advanced Setup draws the hands first (A.7, `CardDraftFive`) and sets up each faction as it is picked (A.8.3), so there the
  Diaspora shuffles them in during its own setup, as its board's 3rd setup step says, and a faction set up after it can draw
  them in its setup (the Woodland Alliance's supporters): new games get the hidden default option `FrogCardsAtDiasporaSetup`
  (`frogsAfterSetup` in `faction-invasive-eee.scala`; 2026-11-11, Betrayal of Right). Without the five-card draft the
  hands are drawn as each faction sets up, so they still wait. `ReplayCheck <games> ldkd draft` checks it.
- Discarded Frog cards skip the Lizard Cult's Lost Souls and go straight to the Pond (or the playtest Lilypads'
  piles), and so do Companions' disguised Frog cards (asked for on 2026-10-13). New games get the hidden
  default option `FrogCardsSkipLostSouls` (`root/meta.scala`, checked in `QuietDiscard` in `root/game.scala`); older
  games keep putting them in the Lost Souls. `ReplayCheck <games> ldlc` plays Lilypad with the Lizards. A running
  game can be switched over by hand: replace the last line of its log (a forced action `X`, e.g. `Next`) with
  `FrogCardsToPondAction(X)` through an `hrf-games` fix; it moves any Frog cards in the Lost Souls to the Pond and sets
  `game.frogsSkipLostSouls` (done for Power for Destiny on 2026-10-13).
- The Woodland Alliance's supporters that are Frog cards show the Frog card back on its player panel, last in the row, and
  are counted in its hint and info overlay ("5 supporters (3 Frog)"), as Frog cards in hand already do, since their back
  shows (`supporterBacks` in `root/ui.scala`; 2026-11-01). Display only.
- The Pond (Lilypad Diaspora's face-up Frog card pile) is shown only with the deck and the discard pile (or among the
  card piles on the map), like the Lost Souls, as any player may draw from it; it was also on the Diaspora's player
  panel until a player found the two copies confusing and the public spot was picked (2026-10-14).
- A spent Frog Ambush goes to the bottom of the Pond (`toPondBottom` in `root/faction-invasive-eee.scala`), as the Homeland
  card says; the log says so. The first reformatted Ambush cards (`hl-frog-ambush`, `card/deck/fr/frog-ambush`) left
  that out, and he replaced them on 2026-10-20.
  Every other discard of it (hand limit, Lizard Cult's Lost Souls skip, Companions) goes there too, as the card says
  "Discard this to the Pond bottom." (2026-10-22: the Lizards' hand limit discard put it on top), in games with the
  hidden default option `FrogAmbushToPondBottom` (`DirectDiscard` in `root/game.scala`, `pondBottom`); older games keep it on top.
  A running game can be switched over with `FrogAmbushToPondBottomAction(X)`, which also moves one already in the Pond to the bottom.
  Leder Games ruled that it goes to the bottom only when played as an ambush (relayed on 2026-10-23), so new games
  get the hidden default option `FrogAmbushBottomOnlyAsAmbush` instead, which turns `FrogAmbushToPondBottom` off
  (`frogAmbushAlwaysBottom` in `root/faction-invasive-eee.scala`): any other discard puts it on top like every Frog card. Games
  started from 2026-10-22 with `FrogAmbushToPondBottom` alone, or switched over by the action, keep it.
- Lilypad Diaspora's Companions stays with the faction that crafted it until the Frog card it turned into another suit
  is discarded (spent), and can't turn a second card meanwhile (asked for on 2026-10-15); it was
  discarded as soon as the card was turned. New games get the hidden default option `CompanionsUntilSpent`
  (`root/meta.scala`; `disguisedBy` in `InvasiveEEEPlayer`, `disguiseSpent` called from `DirectDiscard` in
  `root/game.scala`). A card added to the Eyrie decree is discarded only at Turmoil, so Companions stays until then.
  A running game can be switched over like the fixes below, with `CompanionsUntilSpentAction(X)`.
- Lilypad Diaspora's Negotiations is offered in every menu of an enemy's turn that a soft action makes (a player couldn't
  negotiate from the Twilight Council's Convene menu on 2026-10-16), not only where a faction's menus call the phase hooks
  (`birdsong`, `daylight`, `evening`): `internalPerform` in `root/game.scala` passes each Ask to
  `InvasiveEEEExpansion.anytimeNegotiations`, which adds it with that soft action as its `then`, as a soft action is safe to
  perform again. Not in menus about a card (`aboutCard`: the card may be the one given away), nor when that soft action holds a menu
  button (`unsaved`: the Knaves' move with a pending Nab carries a `WrapAction`, which a recorded move can't be read back with, so
  the negotiation broke the game on load; found by `ReplayCheck <games> ldkd` on 2026-10-18), sub-menus with Cancel, or
  Hobson's choices. It depends only on the game state, so bots, move checks and replays agree (an earlier try that tracked
  soft choices in a `Game` field broke them); old games replay the same, as it only adds choices. `ReplayCheck <games> ldtc`
  plays Lilypad with the Council. Nor in the payment for Riverfolk services (`paying`: `PayForServicesAction` is soft and its
  Ask is one hidden button the runner takes by itself; with Negotiations beside it the menu showed only Negotiations and the
  purchase never went on, 2026-11-04, buying Mercenaries; a card bought with it was already left out by `aboutCard`;
  no game could get past that menu, so it needs no option). `ReplayCheck <games> ldrf` plays Lilypad with the Riverfolk and
  fails if a payment menu offers Negotiations. Those menus head it with the Lilypad faction's name, as the button already says
  Negotiations ("Négociations" showed twice). The Council's "Convene Woodfolk" is in the Council's colour.
- Frog suit text (clearing names in menus and the log, and the two-suit gradients, `Frog` and `FoxFrog` ... in
  `root/styles.scala`) is the Homeland Lilypad Diaspora's yellow-green `#b7b13c` (owner, 2026-10-17, after trying the frog
  decree icons' `#7b9f3c`), and so are the playtest frog factions (`TD`, `LDvB` to `LDvD`, once teal `#3c7174`) and the frog
  part of the map's clearing name tints (`feature/text/frog`, `fox-frog`, `rabbit-frog`, `mouse-frog`,
  `fox-rabbit-mouse-frog`), so a frog clearing's name now looks like its `-hl` highlight. The old teal images were
  recoloured to it too (owner, 2026-10-17): the frog suit icon beside clearing names (`feature/clearing-suit-frog`), the
  frog card, deck and pile icons (`icon/card/*frog*`), the frog craft icons (`icon/craft/*frog*`), and the playtest frog
  faction's action icons, glyph, enclave tokens and titles (`td-*`, `ld-title`), by shifting the teal hue band onto
  `#b7b13c`. A clearing's name tint is named by its suits in Fox, Rabbit, Mouse, Frog order; every mix up to the four has one,
  three-suit ones with Frog since 2026-11-01 (Air of Conquest's Pass, Mouse with Foxburrow and a Peaceful enclave, stopped the map),
  and a mix with no image draws the plain name instead (`stk` in `drawMap`). The playtest frog cards (`faction/invasive/card/`, not `hl-*`), warriors (`td-frog`, `td-frog-x5`), boards and
  character art stay teal (owner's choice). The frog
  decree and relic icons stay `#7b9f3c`, the green of the Homeland frog cards (owner's choice). The solid `frog` tint is saved lossy (a one-colour lossless WebP is a few dozen bytes, the kind iPhones
  failed on).
- Lilypad Diaspora crafts by clearing (asked for on 2026-10-13): when more than one set of its enclaves could
  pay for a card, it picks the clearings (`InvasiveEEECraftSelectAction`), and the enclaves used stay used for the turn
  (`craftedIn` in `InvasiveEEEPlayer`) until removed (`ForcedRemoveTargetEffectAction`), so losing an unused enclave
  doesn't use up the craft of a used one, and flipping a used enclave doesn't free it. New games get the hidden default
  option `FrogsCraftByClearing` (`root/meta.scala`); older games craft by suit as before. A running game can be switched
  the same way as above, with `FrogsCraftByClearingAction(X)` (sets `game.frogsCraftByClearing`). The menu's picks and
  undos are `InvasiveEEECraftPickAction`, a `HalfExplode` that expands straight to the crafts it can still lead to (only the
  recorded one when validating a move): walking the menu tried every order of picks and undos, and bots and move checks ran
  out of memory with many enclaves (found 2026-10-16).
- Governors (17.2.3) stop Lilypad Diaspora's enclave flips at an enemy Council's Governing assemblies, as they already stopped
  placing (`canPlace`): Provoke, Reconcile, Fears Come to Pass, Negotiations and the Militias Frog card leave those clearings out
  (`flippable` in `root/faction-invasive-eee.scala`; reported on 2026-10-17, who could Provoke at Delta and Bayou).
  New games get the hidden default option `GovernorsStopEnclaveFlips`; older games keep flipping there so their replays don't
  change, and a running game can be switched over like the fixes above, with `GovernorsStopEnclaveFlipsAction(X)`. Provoke's log
  now says "flipped Peaceful Enclave to Militant Enclave".
- Governors don't stop the Keepers' Delve: delving moves a relic from the forest to the clearing and places nothing (owner,
  2026-10-21, withdrawing the rule asked for earlier that day; battling never needed an Entreat either). The hidden option
  `GovernorsStopDelve` (and `GovernorsStopDelveAction(X)`, used for Wind against Upheaval) that blocked the relics at an enemy
  Council's Governing assembly is no longer given to new games; in the games that have it the relics can be delved again, and
  the Delve still offers its Entreat choice so their recorded moves load (`RetinueDelveAction` in `root/faction-expedition.scala`).
  `ReplayCheck <games> ki` plays the Keepers with the Council.
- Lilypad Diaspora's Birdsong "Rally or Reconcile" is skipped when neither would do anything: no Militant enclave it may
  flip, and none it could place a warrior at (asked for on 2026-10-21); the Birdsong extras (Entreat and the rest)
  are still offered then, with "Done". New games get the hidden default option `SkipIdleRallyReconcile`; older games keep
  asking so their replays don't change.
- Lilypad Diaspora's Reconcile flips its Militant enclaves one at a time: each pick is its own move, the clearing's ruler draws
  its card, and then the Diaspora picks the next one or "Done" (`InvasiveEEEReconcileOneAction` and `InvasiveEEEReconcileNextAction`
  in `root/faction-invasive-eee.scala`; 2026-11-05: every enclave was picked at once, before any card was seen).
  New games get the hidden default option `ReconcileOneAtATime`; older games keep picking them all at once so their replays don't change.
- Lilypad Diaspora's Fears Come to Pass places its warriors once the warriors the battle or removal took are back in the supply, and
  with too few for every flipped clearing the Diaspora picks which get one (`InvasiveEEEFearsPlaceAction` in
  `root/faction-invasive-eee.scala`). The removed warriors waited off the map until the removal's cleanup, after the fears, so in
  Movement as Betrayal (2026-11-08) the Eyrie's battle at Effigy flipped Effigy and Lower Jaw but only Effigy, listed first, got a frog.
  New games get the hidden default option `FearsAfterRemoval`; older games keep the old way, and a running game can be switched over
  with `FearsAfterRemovalAction(X)`.
- When the Knaves' Nab battle sets off Lilypad Diaspora's Fears Come to Pass, the Knaves (the active player) pick whether the Fears
  or the Acting Captain's move after the Nab resolves first, as they have the same timing (Root rules channel, 2026-11-20): moving first,
  the Fears flip the Peaceful enclaves where the Knaves are after the move. The Fears wait in `nabFears` (`InvasiveEEEPlayer`) for the
  battle's end, where `KnavesNabAfterAction` asks (`KnavesNabFearsFirstAction`, `KnavesNabMoveFirstAction` in `root/faction-knaves.scala`),
  only when the Captain can move and the Diaspora has a Peaceful enclave; a Nab move put first sets them off when it is done or skipped.
  The Fears always came first, during the battle's removal. New games get the hidden `NabFearsOrder`, in `RulesVersion4`; a running game
  can be switched over with `NabFearsOrderAction(X)`. `ReplayCheck <games> ldkd` takes Nab half the times it is offered and picks the order at random.
- The Twilight Council's Convene Woodfolk (17.6.1) offers "Return all to hand" beside the revealed cards: every card left goes back
  to hand at once and none of them acts, ending the step (`CouncilConveneAllAction` in `root/faction-council.scala`; 2026-11-22, asked
  for in Right of Betrayal). It only adds a choice, so no option; the bots never take it.
- An Entreat (17.2.4) asks the Twilight Council to gain a Loyalist or place some in the clearing only when it has Loyalists it could
  place there; otherwise it gains one without being asked, or nothing happens when it can't (`CouncilEntreatAction` in
  `root/faction-council.scala`; 2026-10-29: the Council was asked only to "place 0"). New games get the hidden default option
  `EntreatGainUnasked`; older games keep asking so their replays don't change.
- The Twilight Council may clear a Mountain map path in Evening, right after Convene Woodfolk, when it didn't in Daylight
  (`EveningNAction(30, f : Council)` and `Council.clearPathWanted` in `root/faction-council.scala`): it reveals its cards in
  Birdsong and gets them back only in Evening, so its Daylight hand was empty and the path never came up (Discord,
  2026-11-04). New games get the hidden default option `CouncilEveningClearPath`; older games keep their Evening so their replays
  don't change, and a running game can be switched over with `CouncilEveningClearPathAction(X)`. `ReplayCheck <games> mountain`
  plays the Mountain map with the Council.
- Governors (17.2.3) stop Lilypad Diaspora's enclaves and the Corvids' plots at an enemy Council's Governing assemblies from
  crafting until the Council is entreated (`Council.craftingIn` in `root/faction-council.scala`, used by `craftEnclaves` and
  `MischiefPlayer.craft`; the other factions already left those clearings out). The Diaspora crafted with its Bayou enclave in
  Season in Control on 2026-10-30. New games get the hidden default option `GovernorsStopCrafting`; older games keep crafting there
  so their replays don't change, and a running game can be switched over with `GovernorsStopCraftingAction(X)`.
- The Riverfolk Company crafts with the icons its placed trade posts uncover on its board (Law of Root 8.2), not with pieces in a
  clearing, so Governors don't stop it: a trade post at an enemy Council's Governing assembly still crafts, with no Entreat (`craft` in
  `TraderPlayer`, `root/faction-trader.scala`). Before, it was left out, and in Destiny and Right (2026-11-07) the Riverfolk had to entreat
  at Haven to craft, closing the assembly and giving the Council a Loyalist. New games get the hidden default option
  `RiverfolkCraftUngoverned`; older games keep the old way, and a running game can be switched over with `RiverfolkCraftUngovernedAction(X)`.
- Governors (17.2.3) keep a correct exposure of a Corvid plot at an enemy Council's Governing assembly from removing it: the exposer may
  still show a card and guess, a wrong guess gives the card to the Corvids, and a right one leaves the plot face down, scores nothing and
  returns the card, and the exposer can't guess there again that turn unless the assembly stops governing (entreated) (`ExposeAction` in
  `root/faction-mischief.scala`, `game.governedGuesses`). The exposure removed the plot and scored (2026-11-10,
  Destiny and Right; the owner picked guessing with no removal over not offering it). New games get the hidden default option
  `GovernorsStopExposure`; older games keep the old way, and a running game can be switched over with `GovernorsStopExposureAction(X)`.
  `ReplayCheck <games> cctc` plays the Corvids with the Council, exposing whenever it can, and redoes each exposure on a copy with a
  Governing assembly there.
- The Twilight Council's Banish is offered only against an enemy with warriors it could move in the clearing (not the Warlord),
  or a Lilypad Diaspora Peaceful enclave there, as battling one sets off Fears Come to Pass (`banishable` in
  `root/faction-council.scala`; a lone Underground Duchy tunnel could be banished on 2026-10-30, and the frogs were pointed out).
  New games get the hidden default option `BanishNeedsWarriors`; older games keep offering it so their recorded moves load.
  While banishing, the defenseless extra Hit isn't logged ("dealt an extra Hit to defenseless"), as a Banish can't hit buildings or tokens.
  A banished defender ignores the first hit first (Stubborn, Armored, Devout Knights, Badger Bodyguards, Dazzle;
  `BattleExpansion.ignoresFirstHit` in `root/battle.scala`), so one warrior fewer moves (the Season for Revolution, 2026-11-04:
  two Stubborn Rats were banished by 2 hits). New games get the hidden default option `BanishAfterIgnoredHits`; older games keep the old
  way, and a running game can be switched over with `BanishAfterIgnoredHitsAction(X)`.
  A Banish moves the Knaves' Captains only once no other Knave warrior there is left to move (`Rearguard`, as hits reach a Captain
  only then; 2026-11-18, Control as Struggle: the Captain was banished from Pit and the Skunk stayed). New games get the hidden
  `BanishCaptainsLast`, the first fix of `RulesVersion2`; a running game can be switched over with `BanishCaptainsLastAction(X)`.
  `ReplayCheck <games> tckd` plays the Council with the Knaves.
- Outrage with no card of the clearing's suit to give shows the hand to the Woodland Alliance in its action pane, and the
  game waits for its "Ok" (`PeekCardsAsk` and `PeekCardsSeenAction` in `root/cards.scala`; the playtest Pacifism the same).
  It was only a notification, and the game went on (the Alliance couldn't see the Lilypad Diaspora's hand on 2026-10-17). New games
  get the hidden default option `OutragePeekNeedsOk`; older games keep the notification so their replays don't change, and a running game can be
  switched over like the fixes above, with `OutragePeekNeedsOkAction(X)` (done for Ground and Might on 2026-10-17).
  The owner asked for the notification back on 2026-10-31, as it held up the game: new games no longer get `OutragePeekNeedsOk`
  (still hidden, so the games that have it keep waiting; `InsurgentExpansion.peekWaits`), and a running game can be switched
  back with `OutragePeekNotifiesAction(X)`.
  In every game, the words "looked at" in the log line of that look ("Woodland Alliance looked at ... cards, no ... cards") are tappable
  and show the cards seen again to the faction that saw them (everyone once the game is over; others get "Only ... saw them"), so
  the Alliance's player can find them later without the Notifications list (`PeekedHandView` in `root/cards.scala`, shown by
  `onClick` in `root/ui.scala`; 2026-11-09). Log text only, so no game records change.
- Root remembers in each browser how many notifications (cards drawn, hands seen) it has shown in each online game
  (`<settingsKey>.notifications-seen.<lobby>`, `seenBefore` and `saveSeen` in `root/ui.scala`), saved at the first tap after
  they show, so opening a game shows only those that came since; before 2026-10-31 every notification of the game popped up
  each time a game was opened (taken for a pause in Season in Control). The "Notifications" button shows them all.
- Mice-in-a-Bush counts as an ambush card, so the attacker is offered a counter-ambush against it before its hit
  (`askCounterAmbush` in `root/battle.scala`, the same menu as against an ambush card, with 1 hit instead of 2, and no other ambush follows it, as it is the defender's ambush); it dealt
  its hit straight away until Ground and Might on 2026-10-18 (the Root rules channel confirmed it can be countered).
  New games get the hidden default option `CounterMiceInABush`; older games keep the old way so their replays don't change,
  and a running game can be switched over like the fixes above, with `CounterMiceInABushAction(X)`.
  `ReplayCheck <games> base squires` plays with the Squires and Disciples deck.
- `craftableWith` in `root/game.scala` matches the leftover costs to assets (augmenting paths) instead of trying every
  order of the assets, which took factorial time with many assets that pay only some costs (Lizard gardens, the Lost City).
- Knaves items are the Captains' items, not crafted items: the Vagabond
  can't take them in trade when it aids the Knaves (`AidTradeAction` in
  `root/faction-hero.scala`), the Hundreds can't loot them (Looters in
  `root/faction-horde.scala`), and the Knaves can't aid The Exile
  (`anytime` in `root/hirelings.scala`, `canAidExile` in `root/game.scala`).
- The Lizard Cult's clearing rule flag (`flag-bg-lizards.webp`, drawn under the cloth by `flag` in `root/ui.scala`) was
  recoloured on 2026-10-22 to the lime of its warriors and gardens (`#b6dc6c`), lighter than the olive it had (owner's ask).
- Lizard Cult crafting (house rule, asked for on 2026-10-13): gardens of the Outcast suit craft as
  usual, and every other garden pays only for wild (any suit) cost icons (`WildOnly` in `root/game.scala`, `craft` in
  `root/faction-fanatic.scala`). Law of Root 10.2.1 allows only Outcast gardens. Not gated by an option: it only adds
  craft choices, so recorded games replay the same.
- Lizard Cult Outcast gardens pay for their clearing's suits, not their printed suit (Homeland Lilypad Diaspora rules,
  posted on 2026-10-15): only frog at a Militant enclave, either at a Peaceful one, any of the Lost City's three,
  and either of a double suited Mousehold, Foxburrow or Rabbittown clearing (`c.asset`, a `OneOf`). New games get the hidden
  default option `LizardsCraftByClearing` (`root/meta.scala`, `FanaticExpansion.craftByClearing`); older games keep the
  printed suit. A running game can be switched over like the Frog fixes above, with `LizardsCraftByClearingAction(X)`
  (sets `game.lizardsCraftByClearing`). The two- and three-suit craft icons missing for landmark clearings
  (`craft-suit-fox-rabbit` ... `-rabbit-mouse-frog`) were composed by `root/tools/craft-suit-icons.py`; `CraftSuit` in
  `root/styles.scala` names them in Fox, Rabbit, Mouse, Frog order.
- Root's "Assign Hits: Auto Assign If No Choice" setting (`AutoAssignHits`, `ask` in `root/ui.scala`), the
  out-of-turn auto assign switch and the `AutoHitsAssignmentMode` game option count the ways to assign hits with
  `HitsVariants.distinct` (`root/game.scala`): how many of each piece of each faction are removed. The hits prompt
  picks figures in order, and the old check counted buildings picked in another order as another choice, so
  losing, say, both a sawmill and a workshop still asked (reported 2026-10-16).
- Those two settings act only on the asked player's own device while the game is open, so an absent player held up every
  battle (2026-10-18). Each player's in-game switches under the action pane while waiting, "Auto Assign Hits in
  Battle (If No Choice)" (`UseAutoAssignHits`) and "Auto Skip Ambush (If None)" (`AutoSkipAmbushIfNone`, `root/meta.scala`),
  are stored with the game (`callbacks.switches`) and shown whatever the Out of Turn Actions setting; any open client takes
  the only choice for that player (`shadowCheck` in `root/ui.scala`). New Adset games also start with
  `AutoHitsAssignmentMode`, as Root's do.
- Root's "Bluff Mode" (asked for on 2026-10-21): under Async in setup, `BluffMode` and `AutoAmbushSkippingMode`
  ("Auto Skip Ambushes", once "Auto Ambush Skipping", same saved name) are one choice (`OneOfGroup`), Bluff Mode the default
  for new games and Adset until 2026-11-17 (now off by default, see below); each has a "?" popup. Auto Skip skips an attacked player (and the attacker's counter-ambush, also
  against Mice-in-a-Bush) holding no ambush they can play (`battle.scala`). Bluff Mode asks every time, and stops the
  "Ambush!: Auto Skip If None" setting and the "Auto Skip Ambush (If None)" switch (`ask` and `shadowCheck` in
  `root/ui.scala`, `switchesFor` hides it), which skipped at once and so showed who held none. Skips that reveal nothing
  stay (empty hand, Scouting Party, Riverfolk's open hand, every ambush card seen). Games with neither option keep the old
  behaviour; UI only, so no records change. Forced Async Mode now turns on Auto Hits Assignment and Auto Skip Ambushes
  (`forcedOn`) instead of waiting for both to be picked.
- Root's "Hits and Ambushes – Offline Players" setup section (2026-11-12) replaced the Async
  section above: toggles `BluffMode` ("Ambush Bluff Mode"; off, an attacked player with no ambush is skipped),
  `AutoHitsAssignmentMode` ("Auto Assign Hits (If No Choice)", a choice again, no longer `settled`), `OffTurnHitsPreset` (the
  "Adjust priorities" order Forced Async had) and `OffTurnFieldHospitals` (only with the Marquise; Adset always), or `LiveGameMode`,
  which `blocked`s them all, so the game decides nothing and each player's own settings apply. Partisans are used off turn when
  nothing is discarded in every game that isn't a Live Game. New games get the hidden `HitsAndAmbushesSetup`, which tells them
  from older ones: `game.offlinePlayers`, `autoSkipAmbush`, `autoAssignHits`, `autoHits`, `offTurnPartisans`, `offTurnFieldHospitals`
  in `root/game.scala`. `AutoAmbushSkippingMode`, `ForcedAsyncMode` and `ForcedAsyncChooseHits` are `retired`: older games keep them
  and play as before. The two Off Turn Actions switches show only where they still change something (`switchesFor` in
  `root/ui.scala`), so in new games the box is gone unless the Out of Turn Actions setting adds shortcuts, and the Interface's
  "Assign Hits" and "Ambush!" fold into one "Hits and Ambushes when Online (Live Games)" section. `ReplayCheck <games> base offline`
  plays bot games with Bluff Mode off and the Field Hospitals plan; `preset` adds the hits preset, but bot games with the preset
  order (like `async`) run out of memory, as they did before the section.
  Auto Assign Hits is on by default and Ambush Bluff Mode off (`defaultsFor`; 2026-11-17), also when a browser's saved setup
  holds none of Auto Assign Hits, Live Game or the hits preset (`Meta.autoHitsOn`, in `completeSaved`): setups saved before the section
  never held Auto Assign Hits, as it was then always on, so it showed off, and held Bluff Mode, then the default.
- Root pass and play (asked for on 2026-10-17): in a local game with more than one human faction
  (`Callbacks.hotseat` in `hrf.scala`: a memory or localStorage journal), each time another human faction is asked
  the action pane shows "Pass to <faction>" and a "Show Hand and Choices" button instead of its hand and choices
  (`passTo` and `passedTo` in `root/ui.scala`). Notifications (cards drawn or given) wait for that tap too: `wait`
  shows none, and a tap marks only the revealed faction's as seen. The "Pass and Play" setting (`ShowPassScreen`,
  the default, or `NoPassScreen` in `root/meta.scala`) turns it off. Display only, so no game records change.
- Root hirelings (asked for on 2026-10-17): all 26 cards, the 20 of the Marauder, Riverfolk and Underworld
  packs and Homeland's Sunny Advocates / Bat Messengers, River Roamers / Frog Tinkers and Prosperous / Struggling Farmers,
  are in `root/hirelings.scala` and offered in setup (`Meta.hirelings`). The rules follow the cards and the hireling
  booklet from the TTS mod "Root - Ultimate Collection" (Steam workshop 2516434159; its `Hirelings` entries hold each card,
  face staunch, back demoted, the pieces, the 4/8/12 markers and the control die) and Law of Root appendix H (Root
  Database). Card art is `webp2/root/images/faction/hireling/card/<style>-card.webp` (loaded on demand as `hireling-card:`);
  the Homeland pieces were rendered front-on from the mod's meshes and textures; the markers' top tabs are
  `hireling-tab-4/8/12/back`. New games get the hidden `HirelingsByTheCard` option (older games keep the earlier rules so
  their replays don't change): the Bands stop only the enemies that moved into their clearing that turn
  (`game.movedInto`), the Forest Patrol saves one warrior of each removal, the Spring Uprising has 4 warriors and the Warm
  Sun Prophets start in every ruin. Judgment calls where the cards leave room: the River Roamers lend their lilypads'
  clearing suits as crafting icons for the rest of the turn (`extraCraft`, with a craft menu right away); the Frog
  Tinkers' card is one more crafting icon (a bird for any suit), discarded at the end of the turn; Bat Messengers notice
  the controller's new buildings and tokens in clearings before the next action (`game.batSeen`, like the 4/8/12
  thresholds' check) and offer the warrior then; the Vagabond can't be forced to move by the Warm Sun Prophets, and on a
  Highway Bandits path damages an item; pawns can't be burned or tolled; the Brigand's ruin items are added when setup
  is done if no Vagabond or Hundreds put them; the Struggling Farmers leave the clearings their former controller
  doesn't rule once handed over. The hireling markers still on the score track are drawn on their spaces (`drawScoreTrack`
  in `root/ui.scala`); tapping one, or the "Hirelings" button under the action pane, lists the hirelings in the game with
  their cards and who controls them (`showHirelings`), and a card opens larger. `ReplayCheck <games> base hire:FRP,SAD,...`
  plays bot games with those hirelings (`oldhire` without `HirelingsByTheCard`), checks every action writes and parses
  back, and lists how often each hireling choice was taken.
- The Knaves' Serve activates acclaim in every clearing sharing a suit with the acting clearing, and each acclaim pays for
  the suits the two share (`serveSuits` and `serveAsset` in `root/faction-knaves.scala`). It matched only clearings with
  exactly the same suits, so acclaim at a Lilypad Peaceful enclave (rabbit and frog) or a landmark's second suit was left out
  (the Knaves couldn't craft Coins with their rabbit acclaim in Bayou and Fen on 2026-10-21). Not gated by an option: it only
  adds craft choices, so recorded games replay the same.
- A Knaves craft (Filch or Serve, `KnavesCraftAction`) is recorded and spends the action before the card's costs are matched
  to the acclaims, so that pick has no Cancel (`CraftAction(f : Knaves, ...)` in `root/faction-knaves.scala`): the generic one's
  Cancel after a recorded move threw "cancel on non-soft external action", and Wind of Victory stopped loading on 2026-10-30 when
  the Knaves served Bake Sale (two rabbits) with frog, rabbit and rabbit-or-frog acclaim. Only takes away a choice that threw.
- The Knaves' captain list (player panel and info overlay) says "(retired)" for a captain the Knaves retired, "(removed)" for one
  an enemy removed from the map, and "(retired, removed)" for both (`status` in `KnavesPlayer`, `root/faction-knaves.scala`;
  "removed" is a captain that has been on the map, `placed`, and isn't now). Asked for on Discord on 2026-11-03 (Glory through
  Air: the Harrier, removed by the Keepers, read the same as the Vagrant). Display only, so no game records change.
- The Knaves' Have at Thee! (18.2.6) takes prisoners from every enemy warrior removed in the battle, the co-defender's too: the
  Twilight Council's Bats when the Knaves battle another faction at a Council assembly, or Riverfolk mercenaries (Root rules channel,
  2026-11-11; in Betrayal versus Glory the Woodland Alliance gave both hits to Bats at Peat and the Knaves were offered none, as only
  the defender's warriors were looked at). The Knaves pick which (`KnavesPrisonersAction`, `KnavesTakeCodefPrisonerAction` in
  `root/faction-knaves.scala`). New games get the hidden default option `PrisonersFromCodefenders`; older games keep the old way, and
  a running game can be switched over with `PrisonersFromCodefendersAction(X)`.
- In Advanced Setup the Knaves' four Captain cards are dealt when the factions are drawn, and the draft shows them under
  the Knaves (meeple, name and items; `FactionChoice.desc` in `root/setup.scala`, `game.draftCaptains`, asked for on
  2026-10-20); the Knaves then choose 3 of those. New games get the hidden default option `KnavesCaptainsInDraft`; older
  games deal them when the Knaves set up, so their replays don't change.
- A Twilight Council move or battle reveals its card before it happens (`CouncilRevealCardAction`), and the end of the action
  (`CouncilRevealedAction`) revealed or spent the card "in hand" again: when a Ferry move drew another copy of the same card,
  that copy was revealed too (Season against Land, 2026-10-26: two Feather Rufflers for one move), and a battle at an assembly
  would have discarded the hand copy instead. New games get the hidden default option `CouncilRevealOnce` (`acting` in
  `CouncilPlayer`); older games keep the old way, and a running game can be switched over with `CouncilRevealOnceAction(X)`.
- Forced Async Mode assigns every hit by a preset damage order, which players set at the end of their turn ("Adjust
  priorities": which of their kinds of buildings or tokens in one clearing goes first, and the Vagabond's items). The Async
  option "Players Choose Hits" (`ForcedAsyncChooseHits` in `root/meta.scala`, only with Forced Async; asked for in Season in
  Control on 2026-10-28, where the Woodland Alliance wanted to pick between a base and sympathy each time) lets players with a
  real choice pick their hits instead, and drops the end-of-turn order (`game.autoHits` in `root/game.scala`). A running game
  can be switched over with `ForcedAsyncChooseHitsAction(X)` (`root/turn.scala`). `ReplayCheck <games> base choosehits`
  (or `async` for the preset order) plays bot games with it.
- Root's setup screen folds its sections like Northgard's (asked for on 2026-10-28): Setup Type, Async, Map, Clearings,
  Landmarks, Deck, Cards, the AdSet buffs, the faction tweak sections (Marquise, Vagabond, Lizards, Corvids), Mirror Factions and the
  four Hirelings sections start folded, showing only what is picked (`FoldedOption` in `root/meta.scala`, which sets `BaseOption.fold`
  to `root.<group>`). Sections of a single option (Dominance, Riverfolk's Export) don't fold. Display only, so no game records change.
- Root's setup options were cleaned up on 2026-10-31: the Presets are "Site Defaults" and "Official Rules", each with a ?
  (`presetsInfo`, a hook in `meta.scala`; the Official | Hirelings, Sunday Games and Adset's expansion-pool presets are gone). Auto Hits
  Assignment and the card rulings Official Rules picked (Partisans discard birds, Coffin Makers errata, Propaganda Bureau, the two Tunnels
  rulings) are no longer choices (`Meta.settled`); Hostile Takeover and Shared History, which did nothing, and Coffin Makers' "any warrior"
  are no longer offered (`Meta.retired`, dropped from saved setups by `completeSaved`, still read so old games load). Every new game gets
  the hidden new-game options through `mandatoryFor` (`Meta.newGame`): before, a setup screen that started from options saved in the
  browser left out any hidden toggle added since, as toggles aren't added from the defaults. The AdSet setup buffs are always on, and the
  setup always Homelands, in Adset (`MetaAdset.mandatoryFor`, hidden there), and Off by default in the standard setup. The Async heading
  has a ? explaining the section (`groupInfo`, a hook in `meta.scala`, shown by `setupQuestions` in `hrf.scala`). Recorded games keep
  their options, so none of this changes a game already started.
  The site defaults (Site Defaults, and a new setup) are Random Official map, All Random clearings, the Mountain suited landmark choice
  and the Squires and Disciples deck, in Root and Adset (`defaultsFor`; 2026-10-31).
- Root's rules fixes are hidden options (`Meta.fixes`), but a new game records only its rules version (`RulesVersion1` in
  `root/meta.scala`, `Meta.newGame`), which `Game.options` expands to every fix it stands for (`RulesVersion.expand`; `Game.recorded`
  is what the game was made with, what `OptionsAction` and `GameOverAction` hold). Games made before 2026-11-15 recorded each fix and
  load as before. A new fix gets its hidden option (for older games and its `...Action(X)` switch) and goes into a new version
  (`RulesVersion2`, its `fixes` RulesVersion1's and the new one), made `RulesVersion.latest` and added to `RulesVersion.all`; never
  add to an existing version's fixes, as games that recorded it would change. A game's log no longer lists the fixes, the settled rulings
  or the rules version (`Meta.unlisted`, in `OptionsAction`; 2026-11-15: the opening log had about 35 such lines).
- Advanced Setup's insurgent lock (2026-11-04; Destiny and Right drafted four insurgents): the "Insurgent Lock" choice on the
  first setup page, Adset only (`InsurgentLockOn`, the default, or `InsurgentLockOff`; `Meta.adsetOnly` in `root/meta.scala`). With it, the
  last faction drawn, if insurgent, can't be picked until a militant faction (`AdvancedSetup.militant` in `root/setup.scala`: Marquise,
  Eyrie, Duchy, Hundreds, Keepers, and the Lilypad Diaspora, which the draw now counts too) has been, and the draft's heading says so. Games with neither option (started before it) draft as before.
- The Root tournament (asked for on 2026-11-20): "Tournament" on the Root menu (`tournamentMenu` in `hrf.scala`, URL
  `/play/root/tournament`, `Meta.tournament` names the meta its games use; Root's Adset menu has the button too, opening that page,
  2026-11-20) signs players up with their ranking name; the
  16th sign-up starts it, and the server (`good-game/Tournament.scala`, its header has the files and the hand changes,
  `tournament-changes/<name>.txt` in `hrf-games`) makes four Advanced Setup tables of four itself, from the version and
  options a client sent with its sign-up (`Meta.tournamentOptions` in `root/meta-adset.scala`: official factions without the
  Vagabond, no hirelings). Each table's winner, as reported to the rankings, goes to the final; its winner's name is golden
  (`hrf-golden` in `index.html`) on the Tournament and Rankings pages until the next champion. Play Online lists the player's
  tournament game first (`/tournament/mine`). A Discord webhook (`HRF_DISCORD_WEBHOOK`, or the file `discord-webhook`) is told
  of the start, each table's winner and the champion, and invited to sign up, with the Tournament page's link, once for each
  tournament as soon as its sign-ups open and a webhook is set (`invite` in `good-game/Tournament.scala`, `tournament/invited.tsv`;
  at the server's start, when the webhook is set, when a final starts or a tournament is cancelled; the `invite` change posts it
  again; asked for on 2026-11-20, as nobody could find the page, which is only on the Root menu), and at 8, 12 and 15 sign-ups. Each table (and the final) gets a Discord thread at its start naming its players, pinged by the Discord user ID each
  gave on the Tournament page (asked right after signing up, changeable there; `/tournament/discord`, `tournament/discord.tsv`, thread
  ids in `tournament/threads.tsv`), in the forum channel of a second webhook (`HRF_DISCORD_FORUM_WEBHOOK` or the file
  `discord-forum-webhook`, set by the `forum <url>` change), else in the announcements' channel; a forum channel gets every post in
  a thread, a text channel in the channel itself (2026-11-20).
  The tournament change `status` posts the running tournament's tables (else the last one's) to the announcements' webhook (the owner's
  "Royal Claim"; asked for on 2026-11-22): each table's round, every player's faction and VP in seat order (`game.ordering`; 2026-11-22), whose turn it is or its winner, and its
  spectator link (`Tournament.status`); a table with no status reported lists its players and whoever its last turn report waits on. The round and scores come from the clients' turn reports, a sixth line (`Meta.status`, Root's
  `statusOf`: "r<round>" then "<seat>:<faction>:<VP>"), kept in `statuses.tsv` (`Turns.setStatus`); a table nobody has opened since
  says "no progress reported yet".
  `Tournament.start` takes the server's URL argument for the link. The games have the hidden `TournamentDraft` (`root/tournament.scala`):
  the factions are drawn first (the flop), the first seat picks the deck (Capitalists and Anarchists, Exiles and Partisans or Squires and Disciples, in games with the hidden `TournamentCapitalists` every new tournament game gets, 2026-11-21; older ones offered Might and Right in its place), the second the map, on the Mountain the third the one
  landmark for the Tower's clearing (the Tower, a suited landmark or the Birdsong Tower), then the factions are picked as usual; the game starts with no
  map or deck, `game.options` and `game.board` are vars set by the picks, and `root/ui.scala` rebuilds its board view
  (`BoardView`) when the board changes and draws no map until it is picked (`game.mapPending`). `ReplayCheck <games> official
  tournament` plays it.
- Discord turn pings (asked for on 2026-11-20; `good-game/TurnPings.scala`, its header has the rules and files): each online Root game
  (`root`, `root-adset`; `TurnPings.metas`) gets a post (a thread) in the forum channel of the webhook `HRF_DISCORD_TURNS_WEBHOOK` or
  the file `discord-turns-webhook` (set by the tournament change `turns <url>`; the owner's "Command Warren" webhook, forum #root-games)
  when its lobby is written, naming its players and its spectator link, and a seat is @mentioned there a minute after the game starts waiting for
  it, and reminded 12 hours after its last ping, twice at most, whether or not its player has the game open (owner, 2026-11-22: "Always ping
  players even if their tab is open"); a sweep every 30 seconds (`TurnPings.start`, `sweep`) sends them, never within 2 minutes of their last
  ping in that game (it waits), or after the game's end (`over`). Each seat's start is the time on its `turns/state.tsv` row, so restarts lose and repeat
  none; seats waited for since before that (2026-11-21, when the owner found the pings inconsistent: timers lost at restarts, and pings
  within 10 minutes dropped) are pinged only by the `ping waiting` changes, and from their next turn on. The server can't read a game: it takes the factions asked from the clients' turn reports (`/turn`,
  `good-game/Turns.scala`, which calls `TurnPings.report`) and the lobby's "user <faction> <user>" lines. A soft step inside a MultiAsk (a card picked before "Keep") still reports every seat it asks (`turnMulti` in `runner.scala`): one player's pick took the others off the list (2026-11-22). Every open page reports the same turn, and for it a seat's reason only replaces a kept one it doesn't shorten (`Turns.report`): a page open since before a deploy reported "defend/LC" after a newer one's "defend/LC/KD", so the ping named no attacker (2026-11-23). Only seats under a ranking name whose owner gave a Discord user
  ID (or a username the bot found, or one the owner set) and didn't turn pings off ("Discord turn pings" under Rankings, `discordPings` in `rankingsMenu`; `/discord/mine`, `/discord/pings`,
  `tournament/discord-pings.tsv`; on unless turned off since 2026-11-20, asked for) are mentioned, each by their own mention
  (`allowed_mentions` lists only them, never @everyone). Each ping leads with the seat's faction or clan emoji (the one its reason names, else the seat's own in a standard game; 2026-11-22, as some pings had none), as does the first-turn line; a battle, a fight or an Entreat also names the other side with its emoji and name, "you're under attack ⚔️ by X", "your battle ⚔️ against X", "X entreated you" (the sword or axe right after "attack", "battle" or "fight", and the Partisans card right after "Partisans") (the reason's third part, "defend/<seat faction>/<other>", from the asking action's attacker and defender, else the clan whose turn it is or the other clan in the fought territory; 2026-11-22; a Northgard creature as "~<its name>", from the asking action's creature, else `game.creatureFights` or the fought territory, shown as "the **Brown Bear**" with the Discord server's emoji of that name if it has one), and says why the seat is asked, with an emoji in the server's style (asked for on 2026-11-21; `TurnPings.reasonOf`): the
  client adds a third line to its turn report, "<seat>=<reason>", from `Meta.turnReason` (`root/turn-reasons.scala` and
  `nort/turn-reasons.scala`, `TurnReasons`: the action that asked, `game.asker`, set in `internalPerform`, and the choices offered),
  kept in `turns.tsv`'s fourth column (`Turns.reason`): Root's under attack or a battle (the server's sword emoji), the cards kept
  at the start, the hand limit, the faction draft, the tournament picks, Partisans, prisoners, an Entreat,
  Field Hospitals, setup and the faction's own turn (its head); Northgard's fights (the axe), harvest trading and Winter (plain ⚖️
  and ❄️ after the words, so the webhook can post them, not the bot's `ng_trade` and `ng_winter` badges; 2026-11-23), card picks, setup, the Adset draft, raids, Events and the clan's own turn (its emblem). A Lilypad Diaspora negotiation is never a reason (2026-11-23, asked for: it is never owed, and the ping reminded the enemy of it): its menus report the seat's turn. The
  reasons say nothing only the seat may know (an ambush question and assigning hits are both "under attack"). `NORT_REASONS=1` in
  `nort.Host` prints them. A game's thread also says who takes the first turn, the first time the game waits on one seat alone
  (`announceFirst`, `turns/firsts.tsv`; mentioning them; only games first reported since 2026-11-21, when
  an Advanced Setup game's drawn first player opened it within the minute and the thread never said whose turn it was). Only the bot can post its own (application) emojis, `hrf_*` and `ng_*`, never a webhook (2026-11-22: pings showed ":hrf_rt_tw:" as text): a post holding one goes out from the bot into its thread, and when the bot can't post there the webhook posts it without them (`Tournament.post`, `Lfg.appEmojiIds`); the bot needs View Channel and Send Messages in Threads in the forum. The tournament change `no-discord` lists the claimed ranking names with no Discord to ping (none given, or a username no member has), most seats first (`Tournament.noDiscord`), and Play Online asks a device holding such a name, once, to add it ("Add my Discord" or "Not now", remembered per name as `hrf.discord-asked.<name>`; `discordRows` in `onlineGame`, `HRF.discordMissing`; 2026-11-22). The tournament change `ping waiting` pings every seat the games wait
  for at once (`TurnPings.pingWaiting`). `ping waiting <game> ...` pings only those games (their server ids, the `server` line of their lobbies), and its `.result` says for each
  who was pinged and why anyone wasn't (`TurnPings.pingWaitingIn`; 2026-11-21). A tournament table's game pings in its table's thread (the forum webhook's), with no
  turns webhook needed, every player who gave a user ID unless they turned pings off. A webhook can't ping a username, and the first tournament's players
  gave usernames, so no one was pinged (2026-11-20): with the Discord bot set up (`good-game/DiscordBot.scala`; its token in
  `HRF_DISCORD_BOT_TOKEN` or the file `discord-bot-token`, set by the tournament change `bot <token>`; the server is the
  announcements' webhook's, or `guild <id>`), a username is looked up among the server's members when given (`Tournament.setDiscord`;
  "notfound" if no member has it) and kept with the user ID found (`tournament/discord.tsv`'s third field, `Tournament.discordId`,
  used by the threads and the turn pings); without the bot only user IDs are taken ("nobot"). The owner can set a name's Discord for a player
  who didn't give it with the tournament change `discord <name> -> <username or user ID>` (`-> off` forgets it). The bot needs the Server Members
  Intent and only to be in the server. Keep its token out of this repository.
- Discord LFG games (`good-game/Lfg.scala`, `LfgSetup.scala`; their headers have the rules and files): in the LFG channel `/root` (Root:
  Advanced Setup, pinging the `root-lfg` role) and, since 2026-11-21, `/northgard` (pinging `ng-lfg`; its `mode` a standard game, the
  default, whose players pick their clans on Join from a menu only they see, or a random one drawn at the start, and each clan gets its
  seat's team, and the color its player picked in the second menu the clan pick brings up ("Clan and color", `lfg:color`; asked for
  on 2026-11-22), else one of the colors left in seating order, and the command's `central` option sets its central (starting) tile (2026-11-22, `nortStdCommand` in
  `LfgSetup.scala`; Adset drafts it in the game); or Adset, the clan draft; in both, Wilderness and Wastelands as tiles, creatures or both: a standard game's
  `WildernessTiles` ..., Adset's `WildPartsBoth` ... for the expansion drafted or agreed, `AdsetParts` in `nort/adset.scala`, whose absence
  is the whole module; the clans shown with the Discord server's own clan emojis, found by name (`Lfg.serverEmoji`), or for a clan
  it has none of, the bot's application emoji `ng_<clan>` uploaded at the server's start from `webp2/nort/images/clan/emoji/`, `Lfg.syncClanEmojis`) open a setup panel only the host sees, then post the game with Join, Leave, Start and Cancel; the server creates the game when the
  seats fill (or the host starts it), the bot DMs each player their seat link (every player since 2026-11-21, `welcome`;
  only players the site didn't know before) and Get my link gives it again. A player whose Discord is linked to a ranking name (given
  on the Tournament page, set by the `discord` change, or an earlier LFG seat's) plays under it from the start, so the game is in
  their Play Online (`rankingNameOf`, a `name` lobby line and a `seats.tsv` row, as the tournament does; 2026-11-21). The other way round, an LFG seat's player who claims a ranking name gives that name the Discord user ID they joined with, when it has none yet (`Lfg.learnDiscord`, after `/claim-name`; seats claimed before are caught up at the server's start, `learnAll`), so the Rankings page, the tournament and the turn pings of Play Online games know it too (2026-11-22). Each game is an `Lfg.Kind`; its options come from
  the client (`lfgOptions`, `lfgPlayers`, `lfgCatalog` in `meta.scala`, overridden in `root/meta-adset.scala` and `nort/adset.scala`,
  sent by Play Online), kept per meta (`lfg-catalog-<meta>.tsv`, `lfg-template-<meta>.tsv`; Root's keep their first names). The posts and the panel's menus show an emoji wherever they can (asked for on 2026-11-21): each section and pick
  (the Discord server's emoji of that name, else the server's emoji whose name matches the option's label, saved name or a word of
  them or its section, `LfgSetup.serverMatch`, so the server's emojis are used first; else the bot's own `hrf_<slug>`, made from the game's art by `haunt-roll-fail/tools/lfg-emoji.py`
  into `webp2/lfg/emoji/` with `icons.tsv` naming each option's icon and uploaded at the server's start, `Lfg.iconOf`, else a Unicode one,
  `LfgSetup.unicode`; a map or deck takes only a server emoji named for it, `LfgSetup.themed`). The bot's icons follow the server's own
  emojis' style (seen 2026-11-21): Root's factions as their heads, maps as a square of the map, decks as a card tilted in a white border,
  Northgard's options in a gold-ringed round badge with a ribbon naming them, like the server's clans and WILD and WASTE. And the Host, Players, seats and buttons; a changed picture needs a new slug, as an uploaded one is kept. The tournament change `emojis` lists the server's emojis and what each is shown for ("unused" for none). Northgard's
  online games get their posts and turn pings in their own forum (`TurnPings.webhookFor`: `HRF_DISCORD_NORT_TURNS_WEBHOOK` or the file
  `discord-nort-turns-webhook`, set by the tournament change `nort-turns <url>`); with none set they get none.
- The bot's `/card <name> [game] [private]` (2026-11-22, asked for as a "bot to call cards and other art"): posts a Root or Northgard card,
  faction or clan board, quest, landmark, hireling or creature with its picture, the name suggested as it is typed (autocomplete). The list
  is `webp2/lfg/cards.tsv`, made from the games' cards and asset lists by `hrf.LookupIndex` (`haunt-roll-fail/lookup-index.scala`, JVM host
  project): run it again and commit the file after adding or renaming cards. Discord fetches the pictures from `/art/<path>`, which serves
  only the files that list names (`Lookup.artFile`). The bot's commands other than the LFG ones are `BotCommand`s in `BotCommands.all`
  (`good-game/Lookup.scala`), registered with `/root` and `/northgard` and answered by `Lfg.interaction` (use and autocomplete); a new one
  (a translator, a rules helper) is one more entry there.
- The Discord server's roles and channels are managed through the bot by tournament changes (2026-11-24, asked for so a Claude
  session, which can't reach Discord, can do it): `roles`, `role <name> [| color #rrggbb] [| hoist] [| mentionable]`, `rename-role`,
  `give-role <user> -> <role>`, `take-role`, `channels`, `category <name> [| private] [| see <roles>] [| read-only <roles>]`,
  `channel <name> [| in <category>] [| text|voice|forum|announcement|stage] [| topic <text>] [| private] [| see ...] [| read-only ...] [| synced]`,
  `rename-channel`, `move-channel <name> -> <category>|none` (`good-game/DiscordServer.scala`, its header has the forms). Creating
  changes the one of that name if there is one. On purpose nothing is deleted, nobody kicked or banned, and a role is never given
  server permissions; every request carries the audit log reason "HRF server change". The bot needs Manage Roles and Manage Channels,
  and its role above the roles it manages.
- The bot's `/rule <rule> [game] [private]` (2026-11-22, asked for as a bot "to help cite the rules", not to interpret them): Root's
  rules by their Law of Root number (`4.3.2`, `H.1`, a section lists the rules under it) or name, with the rule's text and the link to
  it on the Root Database (`webp2/lfg/rules-root.tsv`, made by `haunt-roll-fail/tools/law-index.py` from the Root Database's copy, kept up
  to date with Leder's; official chapters only); Northgard's rulebooks have no numbers, so its rules are cited by book, page and section
  heading and shown with the page (`webp2/lfg/rules-nort.tsv` and the pages in `webp2/lfg/rulebook/nort/`, made by
  `haunt-roll-fail/tools/nort-rules-index.py` from the rulebook PDFs, kept in the project files, not in this repository; its sections are
  listed by hand in the script). `RuleLookup` in `good-game/Lookup.scala`; `/art` serves those pages too. Run the scripts again when
  the Law of Root changes or a rulebook is added.
- Advanced Setup's default faction pool leaves out the Vagabond, Negabond
  and the fan factions (`defaultFactions` in `root/meta-adset.scala`); the
  "Official | Riverfolk ..." presets add the Vagabond back.
- Root's layout (`layoutsFor` in `root/ui.scala`) mostly picks the "-fulldim" map, which fills the window's height, so
  going full screen used to widen the map and squeeze the player boards, log and cards (2026-10-14). In full
  screen the map is now capped at the height the window had before (`mapMaxHeight`, remembered per screen width as
  `root.windowed-height.<width>`, else 90% of the screen; `FullDimPane`'s `maxHeight` in `panes-again.scala`), so the
  side panes keep their width and the action pane takes the extra height. Other games don't pass `maxHeight`.
- Chancellor's Utopia (fan, `root/faction-utopia.scala`) shows its Corruption track on its player panel (2026-11-20, asked for on
  Discord): the seven spaces cut from its board (`cu-track-<v1|v3>-<n>`, `Utopia.trackBoard`, by `root/tools/utopia-track.py`), each
  space above the corruption with a morality token drawn on it (`-m`); space 1 never has one, so at corruption 1 no space is
  emptied but the first. Display only.
- The Twilight Council's Loyalists on its player panel are drawn head down, hanging like bats (`styles.hanging` in
  `root/styles.scala`; 2026-11-17). Display only.
- Tapping a Root faction's status pane opens its overlay (`onFactionStatus` in
  `root/ui.scala`) with its faction board. The boards of the base and
  expansion factions (`boardImage`; `mc-board`,
  `ed-board`, ..., `tc-board-h`, `kd-board-h`, loaded on demand) are the
  board fronts from the Tabletop Simulator mod "Root - Ultimate Collection"
  (Steam workshop 2516434159; its objects are JSON strings inside the
  `EVERYTHING['Standard'][<faction>]` Lua tables). Its Council and Knaves
  boards were replaced on 2026-10-10 by the revised ones a player shared (see above). Mirror factions show their original's board. Marquise,
  Eyrie, Alliance, Vagabond, Riverfolk, Lizards and Corvids have overlays
  like the Homeland ones: board, turn phases (`phases`), pieces, and a
  "More Info" toggle with short rule notes. Panes are tappable before a
  faction is set up too (`factionStatus` and `updateStatus`); a faction
  without state yet shows just its board ("Not set up yet").
- Once play starts, the Root map draws the ruins only when someone can use them (`ruinsHidden` in `root/ui.scala`): the
  Vagabond, the Hundreds, the Brigand hireling, or the Knaves with the Adventurer as a Captain, who draws a card for acclaim
  at a ruin (added 2026-10-31: Air of Conquest showed none). Display only, so no game records change.
- `itemSlots` (`itemGrid` for the usual 2x6 layout) places the item tracker;
  `drawTrackers` draws `tracker/item-track.webp`, on a dark translucent panel so
  its light slots show on the snowy maps, under
  them and `drawItemSlots` draws `game.uncrafted` on them.
- The boxes on the Root map (the item tracker, the score tracker and, with the Card Piles on Map setting, the card piles) never
  hide a piece (2026-11-09: in Movement as Betrayal the items covered the pieces in Effigy): warriors, tokens and pawns are
  placed off them (`overlayRects` in `root/ui.scala`, a cost in the free placement, and pieces kept from an earlier draw under one
  are placed again), the rule flags too (`findFlagSpot`), and the boxes are drawn after the Marquise's Keep but under every other
  piece, so a building in a slot under one still shows. The card piles' taps come after the pieces' (`cardPileTargets`). Display only.
- The Homeland Marsh map is `MarshBoard` in `root/maps.scala` (option
  `MarshMap`, images in `webp2/root/images/marsh/`; the map is the plain board art
  the owner uploaded on 2026-10-06, aligned to the earlier printed-board image and
  105 pixels taller than it (2400x2250, room for the score tracker below the
  Delta and Bayou names), and the flood markers come from the Homeland
  print-and-play PDF). The printed board
  has no clearing names, so the names (Thatch, Sedge, Weir, ...) are made up;
  comments give the Law of Root diagram numbers. The seven forests are named after wetland trees and woods (owner,
  2026-10-17): Alder (north-west), Willow (west), Osier (north), Cypress (centre), Tamarack (east), Mangrove
  (south-west, by the Delta) and Carr (south); only their labels, so saved games are unchanged. The names are drawn on
  the map at the forests' centres, under the pieces (`drawForestNames` in `root/ui.scala`). The Autumn, Winter, Lake,
  Mountain and Gorge maps got the same that day: their unnamed forests (and the Gorge's, once compass points) are named
  after trees (Maple, Fir, Linden, Cedar, Mesquite, ...), the old names (Witchwood, Deadwood, Mirkwood, Acres Wild,
  Heart of Oak, Liar Wind, Nada Moonshine) stay, and `Board.forestLabel` places a name where `center` picks at random
  or would put it on water. Tidal, Tundra and Gloom got theirs on 2026-10-30: Tupelo, Bayberry, ... on Tidal, Arolla, Silver Fir, ... on Tundra,
  Blackthorn, Hawthorn, ... on Gloom, drawn at the forests' centres.
  Those maps no longer draw their `map-woods` overlay (shown only with a faction that uses forests, like the Vagabond or
  the Knaves): it printed the old names, or the Gorge's compass letters, a second time (seen on 2026-10-17).
  Region ids must be unique across all boards (`Serialize.parseRegion`),
  so the Marsh river fork is Confluence, not Fork (Gorge has a Fork).
  Rules follow Law of Root M.5: with 1-4 players one clearing of each colored pair (`floodPairs`) is
  flooded at random and the paths on its flood marker (`floodPaths`) link its
  neighbours (`game.floodLinked`, used by `game.connected` and `Roads`); with
  5+ players one of each pair is left without a suit (`game.unsuited`) and
  gets Mousehold, Foxburrow or Rabbittown at random. Ruins go in Crossing and
  Confluence plus the two lowest numbered slots not flooded (`ruinsIn`).
- Root's Map section has a "Random Official" choice (`RandomOfficialMap` in `root/meta.scala`, asked for on 2026-10-30): one of Autumn,
  Winter, Lake, Mountain, Gorge and Marsh (`officialMaps`) is drawn when the game is created (the "Random" choice of any map, `RandomMap`,
  is retired since 2026-11-18: a setup a browser saved with it gets Random Official, `Meta.randomOfficial` in `completeSaved`), by the `startOptions` hook in `meta.scala` (Root and Adset override it; `hrf.scala`
  calls it at the setup screen's Start), so the game, its log and every client only ever see the drawn map. Default clearings
  can be picked with it and become No Clusters when the draw isn't Autumn, as on the other maps.
- Tapping a landmark on the Root map (Ferry, Lost City, Tower, Mousehold, Foxburrow, Rabbittown) shows its card large
  over the map (`landmarkCard` and `"view-landmark"` in `root/ui.scala`; asked for on 2026-10-29). The cards are the
  English ones from the Leder Card Library (`landmark-<name>.webp`, see the Squires and Disciples note), resized to 600 px
  wide in `webp2/root/images/card/landmark/` (loaded on demand as `landmark-card:`); there are no French ones.
  Tapping a hireling's piece on the map opens that hireling's card the same way (`"view-hireling-card"`, the overlay the
  Hirelings list opens; 2026-10-29).
- Tapping water on the Root map (a river, the Lake map's lake, a flooded clearing on the Tidal Flats or the Marsh) says what it
  does: what makes pieces move along it, the clearings by it, and only what uses it in that game (Riverfolk, Lilypad Diaspora,
  Riverboats, Boat Builders, Riversteads, the water hirelings, the Ferry); water that is only scenery (the sea on some maps) says so
  (`waterAt`, `"view-water"` in `root/ui.scala`; asked for on 2026-10-30). Where the water is comes from the masks
  `<map>/map-water.webp` (a quarter of the map's size, red for river or lake, blue for scenery), traced by hand in
  `root/tools/water-masks.py`; flooded clearings are tested against their flood images.
- Tapping the Tundra map's blizzard, or one of the snowflakes marking the spots it may move to, says what it does: it blocks the
  way (path or river) between its two clearings, which aren't adjacent while it is there, and after every round it moves at random
  to one of its six spots, maybe the same (`"view-blizzard"` in `root/ui.scala`; asked for on 2026-10-31).
- Simple Landmarks (the owner's own rules, 2026-11-20) are four toggles in their own setup section, "Simple Landmarks" (`SimpleLandmark`
  in `root/meta.scala`), beside any Landmarks choice: Forge (a crafting piece of its clearing for anyone with pieces there, `forgeCraft` in
  `frogCraft`), Market (once a turn, with pieces there, discard a card to draw one; offered in the phase menus), Birdsong Tower (ruling it
  at the start of Birdsong scores 1, at `BirdsongStartAction`) and Royal Tree (one more building slot, `game.slots`, and on your turn you rule
  it with any piece there, `treeRules` in `rules`), all in `root/simple-landmarks.scala` (its header lists the readings). They are placed at
  setup like the other landmark toggles, where the Landmarks Pack's setup cards put them, after the map's own landmarks. Their pieces are the
  Landmarks Pack's Legendary Forge, Black Market and Elder Treetop (`feature/simple-forge`, `simple-market`, `royal-tree`, cut from the Leder
  Card Library cards by `root/tools/simple-landmarks-art.py`) and the Tower. They are approved for ranked play (`unapproved` in `root/meta.scala`): the Birdsong Tower since 2026-11-20, the other three since
  2026-11-23 (owner); before, those three made a game fan content. In the tournament draft the Birdsong Tower is one of the third seat's
  choices for the Mountain's Tower clearing (`landmarks` in `root/tournament.scala`; the Tournament page says so). The draft keeps any
  Simple Landmark options when it sets the map's landmarks. `ReplayCheck <games> base simplelm` plays them, `birdsong` the Birdsong
  Tower alone.
- The Homeland landmarks Mousehold, Foxburrow and Rabbittown are landmark
  options on any map (`MouseholdLandmark` etc. in `root/meta.scala`). Each
  adds its suit to its clearing, kept when a Lilypad enclave covers the suit
  (`game.landmarkSuits`). Foxburrow is the `FoxburrowRoads` transport,
  Rabbittown a Daylight action and Mousehold a hook in `battle.scala`; the
  effects are in `MapsExpansion` in `root/maps.scala`.
  Rabbittown can be used for its battle alone when no warriors can be placed (empty supply, no other rabbit clearing,
  or a clearing they can't place in): the card is discarded, nothing is placed, and the battle follows (2026-10-31:
  the Hundreds with no warriors left in Season in Control). Not gated by an option: it only adds a choice.
  Mousehold places only removed warriors that went to the supply, and a Field Hospitals plan the Marquise made at the end of
  its turn asks instead of healing when Mousehold could take the cats for free (the Control versus Power, 2026-11-02: the plan
  put his cat at the Keep and Mousehold then offered it again). New games get the hidden `MouseholdFromSupply`
  (`MouseholdAction` in `root/maps.scala`, `ForcedRemoveProcessAction` in `root/faction-feline.scala`); a running game can be
  switched over with `MouseholdFromSupplyAction(X)`.
- A player buying Riverfolk services in Birdsong may buy up to 1, plus 1 for each Riverfolk trade post in a clearing where they
  have pieces, and what a purchase leaves of that limit is offered again right after it (`BirdsongNAction(22)` repeats while
  `limit` is above 0, `root/faction-trader.scala`): the first purchase used up the whole limit, so the Underground Duchy, at a
  trade post and able to buy 2, bought one card and couldn't buy another (2026-11-03, Ground in Conquest). New games get
  the hidden `BuyServicesAgain`; older games keep the single purchase so their replays don't change.
  The Landmarks choice "Map Default, or on Mountain a random suited landmark" (`MountainSuitedLandmark`;
  2026-10-30) plays Map Default, except that on the Mountain map one of Lost City, Mousehold, Foxburrow and
  Rabbittown is drawn at setup and goes in the Tower's clearing (Pass)
  instead of the Tower, making it two-suited unless it is of the clearing's own suit (`SuitedLandmarkAtTowerAction` in `root/setup.scala`; games
  before the hidden `SuitedLandmarkAnySuit`, in `RulesVersion3`, 2026-11-19, never drew the clearing's own suit). Air of
  Conquest was switched to it after its draft (Foxburrow), lobby and game options both, as an Adset game won't load
  when its `OptionsAction` differs from the lobby's `options` line.
- Root has a French display language: the "Language" setting (`EnglishLanguage`,
  `FrenchLanguage` in `root/meta.scala`, shared by all the Root metas) and a "Root en français"
  entry on the main menu (`rootLanguage` in `hrf.scala`, which only saves that setting). It is
  display only: `hrf.elem.Translation` passes every `Text`, `Header` and image id through the
  meta's `translation` when `html.materialize` draws it, so game state, saved moves and online
  games are the same in both languages, players in one game can each pick theirs, and switching
  mid-game redraws the page in place (`html.retranslate`). The words are in `root/french.scala`
  (whole pieces of text first, then names and terms inside longer ones; anything missing stays
  English), following the official French edition. A word the game uses with another meaning in one place gets a
  context in square brackets in the code, `"to [in order to]"` (Knaves' "flipped Teapot to Revel"): `Translation.text`
  in `elem.scala` shows it without the brackets, and `french.scala` translates it apart from the plain "to"
  (asked for on 2026-10-15). French card art for the base and Exiles and
  Partisans decks and the eight base and Riverfolk/Underworld faction boards are in
  `webp2/root/images/card/deck/fr/` and `faction/fr/` (asset prefix `fr:`), cut by
  `root/tools/french-assets.py` from the TTS mod "Root FR" (Steam workshop 1829904481); the boards
  (and the Hundreds, Keepers and Lilypad Diaspora ones) were replaced on 2026-10-10 by newer scans from a player.
  The same player's Google Drive gave the French Squires and Disciples cards, newer Ambushes (used for every deck)
  and the Homeland Frog cards (`hl-*`, `frog-ambush`, `hl-frog-dominance`) in `card/deck/fr/`, listed in `French.cards`
  with their names; `french-assets.py` no longer makes the Ambushes.
- Phones give a page only so much canvas memory, and once it is used up every new canvas draws nothing, with no error.
  Northgard's map turned each rotated tile into a canvas of its own (948x948, about 3.6 MB each); the PC version's whole map,
  mostly turned tiles as it is mirrored, used it up on a player's phone (2026-11-14): only the unturned tiles showed, and the
  tints and the fog of war (canvases too) were gone, so his own home was black and Snake's home in plain view. Map tiles are now
  `RawImageTurned` (`sprites.scala`), which `RenderableSprite` turns while drawing (`tileImage` in `nort/ui.scala`). Don't make a
  canvas per big image; small ones (the 237 px masks' tints and fog) are fine. So that a failed canvas never shows what the fog
  hides, a fog image that comes out blank (or whose mask isn't loaded) is not used (`fogsFailed`) and its whole tile is covered with
  `tile/fog.webp`, a 16 px image in the fog's colour (`fogSquare`, also over every tile not seen at all; it was a canvas).
  Checked by replaying the game in Chromium with canvases past a pixel budget left blank.
- Drawing an image that failed to load or decode throws on a canvas ("InvalidStateError: The HTMLImageElement provided is
  in the 'broken' state"), which on iPhones stopped Root's map partway (Ground and Might, Marsh map, 2026-10-16), and with
  it the action pane and the menu. The `broken-images` script in `index.html` wraps `drawImage` for every game: a broken
  image is skipped and its name kept in `window.hrfBrokenImages` (the loaders tag cached images with `data-url`, as they are
  shown from `blob:` addresses). Root's `drawMap` (`root/ui.scala`) writes those names, or any error it catches with its
  stack, across the top of the map (`mapNote`), so a player's screenshot shows them; map `hrf-opt.js` line:column positions
  to the sources with that build's `hrf-opt.js.map` (the `source-map` package in `/opt/node-tools` works). It was the Marsh
  map's `marsh/map-woods-2250.webp`, a fully transparent 2400x2250 overlay that iPhones failed to load even after it was
  saved again like the other maps' `map-woods` (lossy with an alpha chunk); as it drew nothing, the Marsh now has no woods
  overlay at all (removed from its assets in `root/meta.scala`, and `drawMap` draws `map-woods` only for maps that have one).
- Bot games can be run headless on the JVM with `root/host.scala` (see
  `host.xsbt` for the source exclusions); it also checks that every action
  serializes and parses back.
- The runner (`runner.scala`) takes an `Ask` with a single choice by itself without showing it. A step that
  must pause the game on one button (Northgard's combat report, `CombatReportAction`) adds `.needOk`; bots skip
  the hidden OK.
- Undo, and loading a game, rebuild it by replaying the recorded actions
  with `performVoid`, which stops at the first `Soft` action in a chain.
  So an action marked `with Soft` must only offer choices (return an `Ask`);
  if it changes the game (places pieces, moves on with `Next`), replays
  skip that and the game breaks (blank map, `... not found among ...` in
  the console). Lilypad Diaspora's `InvasiveEEEMusterAction` had this bug, and so did `MarchAction` (`root/movement.scala`):
  after its last move it went straight on to its forced `then`, so a reload or undo lost the Underground Duchy Brigadier's
  `WorkDoneAction` and the Brigadier could work again that turn. It now offers a forced `then` as the only choice
  (`NoAsk`, recorded), as `WageWarAction` does (found 2026-10-16). `ReplayCheck` ignores the Riverfolk's chosen currency
  (`TraderPlayer.current`, menu state set by soft actions) in its comparisons.
  `root/replay-check.scala` plays bot games and checks that replaying every
  prefix of the recorded actions matches the live game:
  `sbt "runMain root.ReplayCheck <games> [base|ld|tc|kd|ki] [dense]"` with the
  `host.xsbt` setup. `ReplayCheck <games> marsh` or `marsh5` plays the
  Marsh map with 4 or 5 players and the three Homeland landmarks.
- Looking at an earlier state in the log (tapping a line) no longer jumps to the latest state when other players' moves come
  in (`ui.overrideGame` checks in `runner.scala`; a spectator, 2026-10-21); "Back", or the player's own turn, does.
- A finished game's "Save Replay As File" (`Quine.save` in `quine.scala`) inlines the whole client and every image of
  the game, about 63 MB for Northgard, too big for Discord (2026-10-23). "Save Small Replay File" under it
  (Northgard and Root, not offline) saves an .html of a few dozen KB (`Quine.saveSmall`): it only sends the browser to
  `/play/<meta>#small-replay=<moves>`, the lobby lines and moves deflated (`CompressionStream`) and base64url encoded.
  `onDocumentLoad` in `hrf.scala` unpacks that (`Quine.openSmall`) into the page's `lobby` and `replay` elements and
  `data-replay`, as a full replay file has them (`HRF.replay` is lazy for it). The part after `#` never reaches the
  server, so it needs no server change, but the replay plays with the site's current code and images.
- Each online game has a Spectator link and one link per player. Spectator
  accounts can read the game but not add moves. A move posted by one gets a
  500 with `empty result set ... "right" = 'append'` in the server log.
- Player and spectator links (`/play/<meta>/<secret>`) are served with an
  empty `<title>` and no `og:title` (`gameIndex` in `GoodGame.scala`), so
  Discord and other chat apps show no "Chronicles" preview card under each
  shared link. The client sets the real title when it loads.
- An online game's page reloads itself when a new client is deployed (2026-11-22: a page open since before a deploy kept sending turn
  reports from the old code, so a ping named no attacker): it asks the server for the client build's fingerprint (`/client-version`,
  a hash of `hrf-opt.js`, `clientVersion` in `GoodGame.scala`) when the game starts, and again before each turn report and each time the
  page comes back into view, and reloads when it has changed, sending no report from the old code (`ClientVersion` in `journal.scala`).
  Nothing is lost, as the moves are on the server; a server-only deploy changes no fingerprint, so it reloads nothing.
- `good-game` wraps static files in `encodeResponse`, so the 8 MB client is
  sent gzipped (about 2.5 MB).
- In a Northgard game the tab's icon is the player's clan emblem (`clan/<clan>.webp`, the clan picker's; in Adset the clan the seat
  drafted), blinking while it is their turn, as Root does with its faction glyphs (`glyph` in `nort/meta.scala` and
  `nort/adset.scala`; asked for on 2026-11-04).
- The site's icon is a drop (the drawing), not HRF's crow: the
  favicon is a 64x64 PNG data URI in the `icon` link of `index.html`
  (`HRF.defaultGlyph`; games swap in faction glyphs), and `drop.png` is the
  backdrop behind the loading screen, in the crow's old dark red.
