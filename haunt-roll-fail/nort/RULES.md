# Northgard: Uncharted Lands — rules notes

Working notes for the HRF adaptation (package `nort`), summarized from the
core rulebook. The rulebook PDFs are not in the repo. Card text overrides
these rules ("the cards are always right").

On 2026-10-03 the code was checked against the English core rulebook (24
pages, including the Creatures module on pages 18–24) from the owner's
Dropbox. Fixed then: Scorched Earth may also go to a neutral territory; the
first setup tile must touch the starting tile itself; Boar's lore needs the
tile to close no territory at all (anyone's); Kaija counts when a tile would
join two players' units; the ten-card Unrest supply; and the second chance
draws a tile when no neutral territory is left.

## Status

| Part | State |
|---|---|
| Year loop (phases 1–5), seven years (or as set in the options) | done |
| Decks: draw / hand / active / discard, reshuffle only when drawing from an empty pile | done |
| Wait, Replace (1 lore), Remove (2 lore), Upgrade (3 lore), Pass | done |
| Development deck (2 Early + 4 Advanced per player), Achievements in year 7 | done |
| Card list with names, fame, text and images (`cards.scala`) | done: 21 clan (+7 warchief upgrades), 16 Early + 36 Advanced (Veiled Threats found in a TTS mod on 2026-10-04), 7 Achievement |
| Playing cards: Flash cards before/after the main card | done |
| Card effects | done: every card in `cards.scala` has an effect (see Interpretations below for the choices made); only Scout Camp's redraw happens before the tile is shown |
| Harvest trade (any 3 resources for 1) | done |
| Winter costs and Unrest cards | done (counts units, which never change yet) |
| End-of-game fame scoring | done (no territory fame yet; of the Achievements only Warlord scores) |
| Map tiles, territories, borders, exploring | done: tile data in `tiles.scala`, territories and placement rules in `board.scala`, setup, Recruit, Move, Explore, Build, Feast and combat in `map.scala`. Tile images in `webp2/nort/images/tile/` (`tile-31` to `tile-33` and `start-5`: TTS scans aligned to the earlier photo crops) |
| Card display (like Arcs): Development and Achievement cards in the `court` pane on top for everyone, your hand (and played cards) in the `hand` pane at the bottom; click or tap a card to see it full screen | done |
| Player colors, chosen per clan on the setup screen (default blue, red, yellow, purple, green, orange by seat) | done; green starting cards are the blue images with the ribbon recoloured to the printed green; there are no orange ones, orange uses the yellow ones |
| Six players (not in the rulebook) | done 2026-10-04: the five-player rules extended (see Six players below) |
| 2v2 Teams variant (core box) and 3v3 Teams (not in the rulebook, the 2v2 rules with six players) | done 2026-10-04 (see Teams below) |
| Game length 5–10 years (10-year variant: 3 Early + 6 Advanced per player), Fame victory only, First seat goes first | done (options) |
| Creatures module (core box), with the More Creatures variant | done 2026-10-03 (`creatures.scala`) |
| Warchiefs module (Warchiefs expansion) | done 2026-10-03 (`warchiefs.scala`) |
| "Ban card draw developments" house rule (not in the rulebook): leaves out Merchant, Market Place, Upgraded Market Place, Upgraded Trading Post, Greater Trade Routes, Trading Post and Cunning Merchant | done 2026-10-05 (`NoDrawDevelopments` in `options.scala`); Negociation, Spy, Ancestral Curse and Veiled Threats stay |
| The Warchiefs box's 7 extra clan upgrade cards | done 2026-10-03: with the module, or alone with the "Warchief upgrade cards" option |
| Wilderness expansion: Environment tiles and five more creatures, with the Ancestral Graveyard and the Wyvern's Den | done 2026-10-04 (`wilderness.scala`, see Wilderness below) |
| New Blood: seven more clans with their warchiefs and 28 clan cards | done 2026-10-04 (`newblood.scala`, see New Blood below) |
| Uncharted Horizons: Events module and Alternative victory conditions module (Thane and Jarl modes) | done 2026-10-04 (`horizons.scala`, see below) |
| Uncharted Horizons: Solo module (the Automa) | done 2026-10-04 (`automa.scala`, the 15 real cards) |
| Wastelands expansion: Environment and Central tiles, five more creatures, Hrimgandr and Jötunn Blainn | done 2026-10-05 (`wastelands.scala`, see Wastelands below) |
| Other modules and expansions | options shown, disabled; `Module` groundwork in `options.scala` |
| Buildings, combat, three-closed-territories win | done |
| Clan powers | done: all seven (Bear's Kaija and Snake's Scorched Earth added 2026-10-03) |
| Tile data checked against the art | done 2026-10-03 for all 35 core tiles (numbers, unit markers, building spaces) |
| No units at the end of a year and no neutral territory | done: a tile is drawn and placed anywhere it fits with an empty territory |
| Unrest supply (10 cards; then −5 fame and discard the top card) | done |
| Starting card list | Recruit, Move, Explore, Build, Feast ×2 (from the cards) |

## Components (core box)

- 7 clans: Bear, Boar, Goat, Raven, Snake, Stag, Wolf. 2–5 players (6 in
  this adaptation, see Six players below).
  Each player also picks a color, independent of the clan: blue, red,
  yellow, purple, green or orange (14 units, 6 starting cards with that
  color's banner; orange is not in the box and uses the yellow cards).
- Clan cards: 1 initial + 2 upgrades per clan.
- 16 Early Development, 36 Advanced Development, 7 Achievement, 10 Unrest cards.
- 35 map tiles: the starting tile (its own back), the 5-player starting tile
  (regular back, "5" marks) and 33 others.
- Buildings: 7 tokens of each of 8 types.
- Resources: food, wood, lore. Fame tokens are kept face down (hidden).
- Two Northgard dice.

## Setup

1. Development deck: shuffle Early, take 2 per player; shuffle Advanced, take
   4 per player; Early pile goes on top. Unused cards leave the game.
2. Achievements: random cards equal to the number of players, face up.
3. Starting tile in the middle (5 players: both 5-player starting tiles side
   by side, food symbol in the middle territory). Shuffle the other tiles.
4. Each player's deck: 6 starting cards + initial clan card, shuffled.
   The 2 upgrade cards are set aside.
5. First player: roll dice, most axes (reroll ties).
6. Starting resources in turn order: players 1–3 get 2 food + 2 wood;
   players 4–5 get 3 food + 2 wood.
7. Each player draws 3 map tiles. In turn order each places one touching the
   starting tile(s) orthogonally and puts 3 units in an empty territory of it.
   Then a second round: the tile may touch any placed tile, borders must
   continue, and the 3 units go in an empty territory of the new tile (never
   the same territory as the first group). Unused tiles go back and are
   shuffled. No clan powers or other gains during setup.

## A year

1. **Start of year**: each player draws 4 cards (+1 per Forge). The first
   player reveals one Development card per player (year 7: the Achievement
   cards instead).
2. **Actions**: from the first player clockwise, one action per turn until
   everyone has passed:
   - Play a card and resolve it. Flash cards (lightning symbol) may be played
     any number before and after the main card, one at a time, or alone.
   - Wait: card to the active area, no effect.
   - Replace: card to the active area, pay 1 lore, draw 1.
   - Remove: card out of the game, pay 2 lore, draw 2.
   - Upgrade: card to the active area or out of the game, pay 3 lore, take
     one clan upgrade card into hand.
   - Pass: hand and active area to the discard pile, take one revealed card
     and put it on top of the draw pile. First to pass takes the first player
     marker. With an empty hand, Pass is the only option.
   - Unrest cards can't be removed from the game.
3. **Harvest** (from the first player):
   - Fame for each controlled closed territory: 1 if it spans 1–2 tiles,
     2 if it spans 3 or more.
   - 3 fame per Altar of Kings.
   - 1 food / wood / lore per icon on controlled territories and buildings.
   - Trade: any 3 resources for 1 of choice, any number of times.
4. **Winter** (from the first player), by units on the map:

   | Units | 1–3 | 4–6 | 7–9 | 10–12 | 13+ |
   |---|---|---|---|---|---|
   | Cost | — | 1 food | 2 food | 3 food + 1 wood | 4 food + 2 wood |

   A player who can't pay pays what they can and puts an Unrest card on top
   of their draw pile (no effect, −5 fame at the end, can't be removed).
5. **End of year**: a player controlling three closed territories each with a
   large building wins (ties: fame, then territories controlled, then units,
   then buildings). Otherwise, after year 7 the game ends. A player with no
   units on the map places 3 in any neutral territory (drawing a tile to make
   one if there is none). Advance the year token.

## Map

- Territories are bounded by borders and tile edges. Regular borders are
  white dotted lines, Rough borders yellow dashed lines.
- Adjacent: share a border (corners don't count). Closed: fully bounded.
  Open: not yet fully bounded.
- A territory is controlled by the player with units in it; neutral if none
  (buildings stay).
- Tiles show food, wood or lore icons, small and large building spaces,
  Carved Stone spaces and creature lairs (Creatures module only).

## Card actions

- **Recruit N**: place up to N new units in controlled territories (or any
  neutral territory if the player has none on the map). Each Training Camp in
  a territory where at least one unit is recruited adds one more unit there.
  Limited by the 14 units in reserve.
- **Build**: in an owned territory on a matching free space; one building of
  each type per territory; limited by tokens; permanent. A small building
  goes on a small, large or Carved Stone space (not confirmed, see
  Interpretations), a large building only on a large space, a Carved Stone
  only on a Carved Stone space. When a building
  fits more than one kind of free space, the player picks the space (to keep
  a large space for a large building, or a Carved Stone space for a Carved
  Stone).
  - Small (1 wood): Food Silo (+1 food at harvest), Woodcutter Lodge (+1 wood),
    Defense Tower (+1 casualty to the defender's roll per tower), Training Camp
    (+1 unit when recruiting there), Carved Stone (+1 lore, only on a Carved
    Stone space).
  - Large (3 wood): Fortress (+2 combat points to the defender), Forge (+1 card
    drawn at start of year), Altar of Kings (+3 fame at harvest).
- **Move ×N**: N independent moves; each moves any units from a controlled
  territory across one border. Crossing a Rough border costs 2 moves (only if
  no regular border also connects the two territories) and can't be split
  between cards. Entering an enemy territory stops the units; all combats are
  resolved after all moves of the card.
- **Explore**: from a controlled open territory, draw a tile and place it
  orthogonally next to the map touching one of the player's open territories.
  Borders must continue; holes are allowed; it may not merge territories of
  different players. If no drawn tile fits, put it at the bottom and draw
  again. No tiles left: no exploring. Closing controlled territories gives the
  current player fame equal to the number of tiles in each (none for closing
  someone else's territory).
- **Feast**: wild — Recruit, Explore, Move or Build.
- **Draw**: draw cards; each one is kept, discarded or put back on top as the
  card says, in the order the player chooses. Can't be played if there aren't
  enough cards in draw + discard.
- **Special**: as written on the card.

## Combat

Units of two players in one territory. The attacker moved in; the defender
controlled it.

1. 1 combat point per unit.
2. Attacker adds the Move card's bonus and clan powers; defender adds
   buildings and clan powers.
3. Starting with the attacker, each may discard food, at most 1 per unit
   involved, +1 point each.
4. Each rolls one die, attacker first. Faces:
   2 points · 3 points · 1 point **or** 1 casualty (chosen; attacker chooses
   before the defender rolls) · 2 points + 1 casualty · 2 casualties ·
   1 point + 1 casualty.
5. A player who takes casualties equal to their units loses (both can lose).
   Otherwise the higher score wins; ties go to the defender.
6. Remove casualties.
7. The loser retreats all remaining units to any adjacent friendly or neutral
   territories across regular borders that aren't in a combat; if none,
   they are removed. Buildings stay with the territory.

## End of game (after year 7)

Fame = fame tokens + 1 per 3 resources left + fame printed on Development
and Achievement cards (Achievements scored now) − 5 per Unrest card.
Ties: territories controlled, then units, then buildings.

Variants: a 10-year game (3 Early + 6 Advanced per player, fame win only);
2v2 teams (below).

## Six players (not in the rulebook)

Built on the five-player rules:

- Both five-player starting tiles, as with five players.
- Starting resources: players 4, 5 and 6 get 3 food + 2 wood.
- The sixth player's color is orange: orange units and warchief
  (`token/unit/unit-orange`, `warchief-orange`, recolored from the
  `-original` images with a dark outline so they don't look like red or
  yellow), and the yellow starting cards.
- Development deck, Achievements and the Creatures module's first creatures
  scale with the number of players as usual. The deck has 16 Early and 35
  Advanced cards (in the data), so a six-player game of 8 years or more
  is short of Early cards: Advanced cards make up the difference, and in a
  10-year game the last years reveal fewer than six cards.

## Teams (2v2 in the core rulebook; 3v3 added here)

The final rulebook's 2v2 variant (the public Kickstarter rulebook doesn't
have it; summarized from a review, check against the rulebook):

- Four players in two teams; teammates sit opposite each other.
- Teammates add their scores together.
- Units may move through a teammate's territory but may not end their
  movement there.
- During the harvest, teammates may trade resources with each other 1:1.

3v3 (option "3v3 Teams", six players) uses the same rules with teams of
three; seats alternate between the teams (1, 3, 5 against 2, 4, 6).

## Clan powers

- **Bear**: Kaija token, recruited like a unit (also at setup); worth 2 combat
  points; moves with other units but can't enter enemy territory; doesn't
  count for winter.
- **Boar**: exploring without closing a territory gives 1 lore.
- **Goat**: building gives 1 food (small) or 2 food (large).
- **Raven**: closing their own territories by exploring collects those
  territories' resources (tiles and buildings) right away.
- **Snake**: before resolving a clan card, may move the Scorched Earth token
  to a neutral or enemy territory adjacent to one they control. +1 combat point fighting
  there (attacking or defending); at harvest may take one resource from that
  territory instead of its owner.
- **Stag**: +1 fame per territory conquered in combat or closed by exploring.
- **Wolf**: winning a combat as the attacker gives 1 food.

## Creatures module (core box, pages 18–24)

Nine creature cards and miniatures; the card shows the combat value, the
fame for defeating it, and the move priorities from left to right.

| Creature | Copies (colors) | Value | Fame | Priorities | Effect |
|---|---|---|---|---|---|
| Wolf | 3 (beige, brown, dark brown) | 4 | 1 | resources, buildings, units | Its territory gives no fame or resources at harvest, except from buildings (Snake can't take a tile resource there) |
| Brown Bear | 2 (beige, brown) | 6 | 3 | buildings, resources, units | No building or recruiting in its territory, and no exploring or moving units out of it (retreating after losing to a player is allowed) |
| Draugr | 2 (beige, brown) | 5 | 2 | units, buildings, resources | When it appears or moves into a controlled territory, that player removes 1 unit |
| Fallen Valkyrie | 2 (beige, brown) | 7 | 4 | resources, buildings, units | Doesn't share its territory: attacks the units there when it appears or moves; units moving in must stop and fight it |

- **Setup** (after K, before L): shuffle the cards of value 6 or less, put
  N+1 (N players) on top; shuffle the rest (with the Valkyries) below.
- **Apparition**: when a tile with a lair is placed (setup or Explore), the
  current player draws the top creature card, adds it to the right end of the
  creature line, and puts the creature on the lair. In setup that happens
  before the player's units go on the tile, and the creature does nothing
  else. From an Explore the creature doesn't move but acts at once. Empty
  draw pile: shuffle the creature discard pile.
- **Creature phase** (2.5, after the Actions, before the Harvest): from left
  to right each creature moves, then acts. It must move if it can, to an
  adjacent territory (Rough borders ignored) with no creature in it; first to
  territories with units, otherwise any; then by its priorities (building
  points: small 1, large 3; units; resources on tiles and buildings); the
  first player breaks remaining ties.
- **Attacking creatures**: only with a Move action (even one that moves no
  units), in neutral or friendly territories. After all moves and before the
  fights, the player declares up to one creature per territory they share
  with creatures. Those fights are ordered with the player fights, and
  nobody may retreat into their territories.
- **Combat**: the player counts units, card or building bonuses, clan powers
  and food (max 1 per unit) as usual; the creature its value. Each rolls a
  die (another player rolls the creature's). Casualties inflicted on a
  creature don't count; a creature always takes the point on the
  point-or-casualty face. A player who takes casualties equal to their units
  loses. Otherwise higher score wins, ties to the defender. A defeated
  creature's card goes to the creature discard pile and the player gains its
  fame. An attacking player who loses stays in the territory, unless the
  creature doesn't share its territory (then retreat); a player attacked by a
  creature retreats normally when losing. An attacker gets no building
  bonuses.
- **Variant, More Creatures!**: after picking their card when passing, a
  player may make a creature appear, unless there are already at least as
  many creatures on the map as players: on a lair without a creature, or
  if there is none, in any territory without a creature. It doesn't move but
  acts.

## Warchiefs module (Warchiefs expansion rulebook, 4 pages)

From the English rulebook (`Warchief_Expansion_rules_EN_light.pdf`, found
online). The box also has modular player and clan boards, a Kaija miniature
and 7 new clan upgrade cards (one per clan, illustrated with the warchief,
usable without the module); the boards are in `expansion/board/`, and the
board and the warchief's card are shown by the clan picker's Warchief button.

The card images come from the Tabletop Simulator mod "Northgard: Uncharted
Lands + Warchief + Wilderness" (Steam Workshop 2838546142, card sheet 12),
cut and resized to the other clan cards (`card/clan/<clan>-<card>.webp`).
All seven are Move cards, none Flash, and become a third clan upgrade:

| Clan | Card | Move | Effect |
|---|---|---|---|
| Wolf | Egil's Fury | 3, +1 casualty | Before combats are resolved, may remove 1 building from a territory being attacked (back to the reserve) |
| Stag | Brand's Bravery | 2, +1 point | For each combat won, you choose where the enemy retreats (legally) |
| Bear | Borgild's Shield | 2 | Before the Move, may place Kaija into Borgild's territory or vice versa; in each combat cancel 1 casualty inflicted by the defender |
| Snake | Signy's Celerity | 3 | May move through the territory with the Scorched Earth token without stopping |
| Raven | Liv's Cunning | 4 | In combat, may discard wood or lore instead of food for bonus points |
| Goat | Halvard's Craft | 2 | If you control new territories after the Move, may place 1 small building in one of them for free |
| Boar | Svarn's Menders | 2 | Friendly casualties go on the card; after all combats, place them in any of your territories |

- Setup: each clan's warchief goes to its reserve (phase H). In phase L a
  player may place the warchief instead of one unit (two units and the
  warchief).
- A warchief follows all unit rules: recruited (instead of a unit), moved,
  fights. When it dies it goes back to the reserve and can be recruited
  again. Card actions or abilities on an enemy unit can't target it
  (Raiding Party, Plunder, ...).
- Combat: worth 2 combat points (more with some powers); one unit for the
  food limit and for casualties (one casualty removes it). A player removing
  casualties may choose whether the warchief goes, unless all must.
- Powers (only in a combat the warchief is in; attacker's first when two
  apply in the same step):

| Clan | Warchief | Power |
|---|---|---|
| Bear | Borgild | Defending, worth 3 |
| Boar | Svarn | In an open territory or one with wood (not from buildings), worth 3 |
| Goat | Halvard | Defending, ignore 1 casualty inflicted by the attacker |
| Raven | Liv | Step 4: may reroll the combat die once and must accept it; attacking, before the defender rolls |
| Snake | Signy | Step 1: may place the Scorched Earth token in Signy's territory |
| Stag | Brand | Step 1: may move 1 friendly unit from an adjacent territory into Brand's (Rough borders ignored) |
| Wolf | Egil | Attacking, worth 3 |

## Wilderness expansion (rulebook, 8 pages)

From the English rulebook (`Northgard_Wilderness_Expansion_rules_EN.pdf`,
found online on tesera.ru). One option, "Wilderness", turns on both parts;
the creature parts need the Creatures module as well. The images come from
the Tabletop Simulator mod 2847156187 (cards, tiles) and the owner's
upload (`expansion/tile/`, the same tiles): tiles are `tile/wild-*.webp`.

### New creatures (Creatures module)

The Draugr Jötnar, Eldthursar and Hvedrung go into the creature deck (all
above value 6, so never among the first N+1). Priorities as printed on the
cards (hammer = buildings, viking = units, wood/food/lore = resources):

| Creature | Copies (colors) | Value | Fame | Priorities | Effect |
|---|---|---|---|---|---|
| Draugr Jötunn | 2 (beige, brown) | 8 | 5 | resources, buildings, units | When it appears or moves into a controlled territory, its owner must pay 2 resources of any type; otherwise it attacks (a player with 1 resource pays it and is attacked) |
| Eldthurs | 2 (beige, brown) | 8 | 5 | buildings, units, resources | When it appears or moves into a controlled territory, its owner removes 1 of their small buildings there (back to the reserve) |
| Hvedrung | 1 (beige) | 7 | 4 | buildings, resources, units | When it appears or moves into a territory, draw a creature card: it goes right after Hvedrung in the creature line, its miniature in Hvedrung's territory (it acts on appearing, and in the Creature phase it is activated next) |
| Spectral Warrior | 2 (beige, brown) | 4 | 1 | buildings, resources, units | Ancestral Graveyard only. Buildings in its territory have no effect (no extra resources or fame, no combat bonuses, ...). Removed from the game when eliminated |
| Wyvern | 1 (beige) | 9 | 6 | units, buildings, resources | Wyvern's Den only. Doesn't share a territory and attacks the units where it moves; every territory is adjacent to it for its moves. Before combat the player removes 1 unit. Losing outside its Den it goes back to the Den; losing in its Den it is removed from the game |

### Environment tiles module

- Setup: after step L (the setup tiles), the Environment tiles are shuffled
  into the map tile pile. With the Creatures module the Wyvern's Den is
  kept out, the first 3 × (players) tiles are set aside, the Den is shuffled
  into the rest, and the set-aside tiles go back on top.
- Impassable borders (uninterrupted orange lines, and the art of the Great
  Lake, the Poisonous Swamp and the High Peaks): nothing crosses them, and
  the territories on each side are not adjacent for any rule (moves,
  retreats, creatures, cards). Territories may still become adjacent through
  other tiles.
- Great Lake (Harvest): the player with the most units in the four
  territories around the lake collects 2 food; tied players collect 1 each.
  The food isn't any territory's (cards and clan powers ignore it).
- Geyser, 2 tiles (End of the Year): a player controlling a territory with a
  Geyser may place one unit there (once per Geyser in the territory). Not
  combined with the Training Camp benefit.
- Ruins, 2 tiles (Harvest): the controller collects the lore and fame shown
  (1 fame on one tile, 2 on the other), the fame even if the territory isn't
  closed.
- Swamp (Move): units can move through it but can't end a movement or
  retreat there; units moving through lose one of them. Creatures may end
  there. If an effect would make units end their movement there, they can't
  enter it at all.
- Poisonous Swamp (Harvest, impassable): at the end of the Harvest, one unit
  is removed from each territory around it.
- High Peaks, 2 tiles: impassable.
- Ancestral Graveyard (Creatures module; Creature phase): at the start of the
  Creature phase, the player controlling the Graveyard territory may place a
  Spectral Warrior on any territory with a creature's lair, card at the end
  of the creature line. Spectral Warriors removed from the game can't come
  back.
- Wyvern's Den (Creatures module; Explore and Harvest): when it is explored
  the Wyvern goes on it, card at the end of the creature line. The Den is a
  territory of one tile and gives 2 fame to a player controlling it at the
  Harvest.

## New Blood (2026-10-04)

Seven more clans, with their warchiefs: Dragon, Horse, Kraken, Lynx, Ox, Rat
and Squirrel (`newblood.scala`). There is no option: picking one of these
clans brings the expansion in (`Game.modules`).

Sources: the clan boards (`expansion/board/`, matching the boards in the TTS
mod) and the 28 clan cards, from the Tabletop Simulator mod "Northgard:
Uncharted Horizons + all DLC [ENG]" (Steam Workshop 3597126237, whose
Uncharted Horizons parts come from the Tabletopia module; card sheet
`deck240`, 6x6). Five of the clans (all but Horse and Rat) are also
described in the work-in-progress Uncharted Horizons rulebook on Tabletopia
(`northgard-uncharted-horizons-rulebook-light-en`); where it and the boards
differ (Kraken's High Tide, Squirrel's power), the boards are followed. No
New Blood rulebook was found on Tabletopia or in the TTS mods.

| Clan | Warchief | Clan power | Warchief power |
|---|---|---|---|
| Dragon | Surtr | Sacrificial Pyre (2 units). After each combat, enemy units lost go on it while there is room. To harvest, sacrifice a unit from it (back to its owner's reserve) or place one of your deployed units on it; then collect 1 extra food or wood. Setup: one own unit on it | Step 4: no casualty rolled → +1 casualty; no point rolled → +1 point |
| Horse | Eitria and Brok (two figures) | When you close a territory: collect 1 wood, or build 1 small building in it paying its cost | Both in the same territory: worth 3 instead of 4 |
| Kraken | Kàra | Before playing a Kraken card, may place a High Tide token (2) in a territory you control; it goes back when you no longer control it. Step 4: enemies fighting Kraken in a High Tide territory suffer 1 casualty | Defending, before the combat: may move a High Tide token to her territory |
| Lynx | Mielikki | Brundr and Kaelinn: a special unit worth 1 point, recruited like a unit (also at setup). Whenever you play a Lynx card, may Move 1 with it and any units with it, to a neutral or friendly territory, before or after the card | +1 point with a Flash card in your active area this year |
| Ox | Torfin | 7 Ancestral Equipment tokens (face-down pile, 1 on top). When you Explore, the top one goes face up on an available small building space of the tile (no building there; it stays when the territory changes hands). Before playing an Ox card, may take one from a territory you control into your reserve. Step 1: may use one face-up token, then flip it face down; they turn face up at the start of the next year | May use one more token |
| Rat | Eir | When you close territories you control, may place 1 unit from reserve in each | +1 point in a territory with food on its tiles (not Food Silos) |
| Squirrel | Andhrimnir | After harvesting: with at least 1 food, gain 1 food, or with at least 1 wood, gain 1 wood | Before step 1 of a defensive combat: gain 1 food |

Ancestral Equipment tokens: 1 step 2, +1 point if you spent food; 2 step 1,
+1 point; 3 step 4, reroll your die; 4 step 1, +2 points; 5 step 2, +2
points per food (instead of +1); 6 step 6, +1 casualty to the enemy; 7 step
6, ignore one of your casualties.

Clan cards (initial, two upgrades, and the warchief upgrade card; Flash
marked ⚡):

| Clan | Initial | Upgrade | Upgrade | Warchief upgrade |
|---|---|---|---|---|
| Dragon | ⚡ Dragon Clan: sacrifice 1 unit or place 1 deployed unit on the Pyre, then draw 1 and collect 1 food | ⚡ Capture for Sacrifice: from a territory you control, remove 1 enemy unit from an adjacent territory and put it on the Pyre; you may return 1 own unit from the Pyre to a territory you control | Reluctant Workforce: draw 1; sacrifice up to 2 units, 1 card each | Tenacious Grudge: Move 3, +1 point per unit on the Pyre |
| Horse | ⚡ Horse Clan: Build; small buildings (Carved Stone too) need no space | Craftsmen: Recruit 2, then may replace a building you control with another of the same size | ⚡ Quality of Life: may build a free Defense Tower needing no space, then collect a resource from each of your territories with a Defense Tower | Eitria and Brok's Precision: Move 2; before, may pay any 1 resource for +1 point and Move 3 |
| Kraken | ⚡ Kraken Clan: collect 1 food or 1 wood from a territory you control with a High Tide token; may draw 1 | ⚡ Endless Tide: may remove 1 enemy unit from an open territory for 1 lore; may draw 1 | Knowledge from Beyond: per High Tide territory you control, one of Recruit 1, Collect 1 resource, Draw 1 (each once) | Howl from the Sea: Move 2; where you win, add 1 unit per casualty inflicted; for each combat lost, 1 unit where you retreat |
| Lynx | ⚡ Lynx Clan: Explore; may put the tile at the bottom and draw another | ⚡ Fire Arrows: Move 1, +1 point and +1 casualty | Poaching: draw 2; if either is Flash, reveal it and draw 1 more | The Wise One: Move 2, +1 point per casualty inflicted |
| Ox | ⚡ Ox Clan: Move 1; the token may be taken after the card instead | Warcraft: Explore; 1 lore if the tile shows lore, otherwise another Explore | City Builder: Build, then 1 lore and may draw 1 | The True Hero: Move 2; before a combat with Torfin, may give up his power to remove a building there |
| Rat | ⚡ Rat Clan: Build; may first remove up to 2 units from your territories for 1 wood each | ⚡ Overwork: may remove 1 unit to collect everything from 1 territory you control (buildings too); may draw 1 | Proliferation: Recruit 1 in up to 3 closed territories you control | Blood Ties: Move 2, 1 lore per casualty suffered |
| Squirrel | ⚡ Squirrel Clan: Recruit 1, then 1 food or fame equal to your units in your territories / 4 | Cooking Mastery: Build, then may build a free Food Silo in a territory you control, even next to another | Economics: draw 1; pay up to 2 food, 1 card each | Eldrich: Move 2; each casualty the enemy rolls also hits their own units |

The warchief upgrade cards follow the Warchiefs box's rule: a third upgrade
with the Warchiefs module or the "Warchief upgrade cards" option.

## Uncharted Horizons: Events module (2026-10-04)

From the Uncharted Horizons rulebook on Tabletopia (work in progress) and
the 20 Event cards in the TTS mod 3597126237 (sheet `deck235`, 5x4); code in
`horizons.scala` (`EventsExpansion`), option "Events".

- Setup: shuffle the Event cards and make a face-up deck of (years − 1)
  cards. No Event in the first year. At the start of each later year the top
  card applies for that year (the rulebook flips it at the end of the
  previous year; same thing). The next one is visible in the card strip.
- Start of the year, after drawing (each player in turn order): God's Favor
  (1 lore or 3 fame), Offerings (may discard 1 lore: draw 1 or take a card
  from the discard pile), Early Spring (may take 1 food, 1 wood or 1 card),
  Supply from Homeland (1 resource or 1 card), Volcano Eruption (remove one
  of your buildings: small draws 1, large draws 2), Happy People (remove 1
  Unrest card, or 3 fame and 1 unit in a territory you control).
- Actions: New Horizons (closing an enemy or neutral territory by exploring
  gives 2 fame, +1 lore if 4+ tiles). Combats: Blood Moon (+1 casualty to
  both sides; 1 fame per unit lost), Conquests (attacker +1 point; a
  defender who wins gains 2 fame).
- Before harvesting: Myrkálfar's Levy (a territory producing 2+ resources:
  don't collect it or lose a unit there), Bountiful Year (one territory
  gives no fame but double resources), Infestation (each territory producing
  2+ resources: 1 food less and 1 fame, or lose a unit), Sailor Ghosts (the
  open territory with the most units loses 2 units, 2 fame each), Kraken's
  Attack (lose a unit in an open territory and discard 1 wood; with no open
  territory, 2 fame).
- Harvest: Frozen Sea (1 fame per territory controlled instead of per closed
  territory), Ceremonial Bonfire (1 fame per trade, and 2 wood can be traded
  for 1 lore). After harvesting: Draugr Invasion (2 fame; territories with 3+
  units lose 2, minus 1 per Defense Tower and 2 per Fortress), Earthquake
  (1 wood per territory with buildings, 1 fame each; each unpaid territory
  loses a building).
- Winter: Blizzard (+1 food and +1 wood), Harsh Winter (+1 unit per closed
  territory you control).

## Uncharted Horizons: Alternative victory conditions module (2026-10-04)

Same sources; the TTS mod has 8 Map Control cards (sheet `deck236`) and 13
Wealth cards (`deck237`), more than the rulebook's 5 and 8. Code in
`horizons.scala` (`VictoryExpansion`), under the "Victory conditions"
options: "Alternative victory, random cards" (the rulebook's setup) or
"Alternative victory, chosen cards" (the players pick exactly the cards the
setup would draw), and Thane (default) or Jarl.

- Setup: 1 Map Control card and 2 Wealth cards (3 with teams), face up.
  Cards needing the Creatures module (Creature Territories, Hunting) are
  skipped without it.
- At each end of year (before the usual checks), a player fulfilling the
  mode's conditions wins at once: Thane, the Map Control card and one Wealth
  card; Jarl, all of them. With teams, each card may be fulfilled by any
  player of the team. Ties: the most cards fulfilled, then fame. Three closed
  territories with large buildings no longer win; nobody fulfilling the
  conditions by the last year means the most fame wins.
- Map Control: Many Territories (6 closed), Large Buildings (4 large
  buildings), Spreading (8 territories), Creature Territories (3 closed with
  a lair), Vast Territory (a closed territory of 6+ tiles with a large
  building), Large Territories (3 closed with a large building), Two Larger
  Territories (2 closed of 5+ tiles with a large building), Mountains (6
  closed, each with a Rough or impassable border).
- Wealth, checked on the spot: Knowledge (3 upgrades; without the Warchiefs
  module 2 upgrades and 3 lore), Prosperity (50 fame tokens), Population (at
  most 1 unit in reserve, no Unrest card), Production (5 of each resource),
  Building Ownership (9 buildings in your territories).
- Wealth with validation counts, which never go down: Exploration (close 6
  territories you control), Architecture (build 5 buildings), Conquest (win
  5 combats against players), Hunting (defeat 3 creatures), Valhalla (lose
  6 units in combats against players or creatures), Development (6 fame from
  Development cards, the best total reached), Trading (6 Harvest trades),
  Refinement (upgrade or remove 4 cards). The status panes show each card
  with ✓ when fulfilled and the counts.

## Uncharted Horizons: Solo module, the Automa (2026-10-04)

From the Uncharted Horizons rulebook on Tabletopia (pages 13–18) and the 15
Automa cards and 2 reference cards from the Tabletopia module (screenshots
the owner gathered; images in `card/automa/`); code in `automa.scala`
(`AutomaCards.specs` is the cards' transcription). Pick "Solo vs Automa" on
the main menu, or "Automa (solo)" and one clan; the Automa is always a bot
(`Meta.botOnly`).

- The cards: two conditional actions each (the first possible is done),
  a Development card preference row, Flash on cards 4, 7, 11, 13 and 14, Pass
  on cards 8, 9, 12 and 13. Recruit needs 1 or 2 Leaders and units in its
  reserve; Build names its building (Altar of Kings, Fortress; "if possible a
  Carved Stone, otherwise" a Defense Tower or Training Camp; a Food Silo if
  its wood is at least its food, otherwise a Woodcutter's Lodge) and needs a
  free space of that size with 1 or 3 wood; Explore ranks the Automa's open
  territories, puts the tile on a spot next to the chosen one (its compass
  icon for the spot) and turns the tile by its rotation row; Move 1 ranks the
  territory to take a unit from (2+ units) and the one to put it in.
- Levels (option "Automa difficulty"): 1, the player also wins with four
  closed territories each with a large building; 2, more fame than the
  Automa after the last year; 3, the same with the Creatures module; 4 and
  up, the Automa draws one more card each year per level above 3.
- Setup: Enemy Secrets and Ancestral Curse leave the Development deck; the
  player goes first. The Automa places the top tile of the pile (drawing
  once more if it shows no resource) to the right of the starting tile, then
  above (or below) it, unturned when possible, and puts a Leader and two
  units on the territory with the most resources, food, wood, building
  spaces, open, farthest from the player.
- Its year: it draws 4 cards (+1 per Forge it controls, +1 per lore it has,
  which it spends, +1 per large building the player has more than it, +1 per
  level above 3). Each turn it reveals a card and does the first possible of
  its two actions (neither: next card); Flash cards play another card at
  once. Its last card with the Pass icon makes it pass if the player hasn't.
  Passing, it takes a Development card by the card's priorities (move,
  draw, recruit, explore, special, left, right), the remaining one if the
  player has passed, and in the last year the Achievement worth the most to
  it. Its cards count only for fame at the end.
- Actions: Recruit n (Leader 1, Leader 2, then units); Build; Explore; Move 1
  (one unit to a friendly or neutral territory); Move 2 with Leader 1 or 2
  (half the units, rounded up, of an adjacent territory reinforce the
  Leader, then the Leader moves: against the player with enough units to
  outnumber the defenders by at most 2, into a neutral territory with half
  its units, into its own with all but one). It ignores Rough borders and
  never empties a territory.
- Priorities narrow the candidates one after the other; a tie left is the
  player's choice. Leaders count as units (with the Warchiefs module they are
  warchiefs worth 3).
- Combat: it spends food to lead the player's best total by at most 2, or
  all it can; it takes the casualty on the choice face if that wipes out the
  enemy; casualties take units before Leaders; it retreats to one adjacent
  friendly or neutral territory with the most building points, resources,
  tiles, closed.
- Harvest as usual, then one trade of 3 wood for 1 lore with 6+ wood, one of
  3 food for 1 lore with 6+ food. No Winter costs, no Unrest. Events don't
  involve it.

## Wastelands expansion (rulebook, 12 pages; done 2026-10-05)

From the English rulebook the owner uploaded on 2026-10-05
(`Wastelands_Rulebook_150dpi_02122025_EN.pdf`, not in the repo). Uses some
Wilderness rules (impassable borders, the Wyvern).

**Components:** 7 Environment tiles, 10 Central tiles (2 of them five-player
tiles, marked with a 5), 11 creatures with cards: 2 Rock Golems, 2 Myrkalfar,
2 Giant Boars, 2 Kobolds (beige, brown each), Valdemar, Jötunn Blainn and
Hrimgandr (beige).

**New creatures** (added to the Creatures module deck, built as usual: cards
of value 6 or less shuffled, players + 1 of them on top, the rest below):
- Rock Golem: when spawning or moving, attacks only if there are buildings in
  the territory. In combat both sides add +1 axe for each skull rolled; if it
  rolls the skull/axe face it always takes the skull.
- Myrkalfar: moves twice, the second move not back to its starting point.
  The owner of the second territory discards 1 wood or, if they can't, loses
  2 fame. (Card: "Only when moving".)
- Kobold: its territory makes no fame at the Harvest (buildings included).
  Once per Harvest, the controller of a territory with a Kobold may exchange
  1 food for 1 wood or back (one exchange per Kobold).
- Giant Boar: when spawning or moving into a territory with wood (buildings
  included), attacks with +1 skull on its die. If the defender wins, +2 fame
  on their reward.
- Valdemar: when spawning or moving, removes 1 regular unit from the
  territory he arrives in. While he is alive all other creatures get +1 axe
  in combat.
- Strength / fame from the cards (TTS mod 3597126237): Rock Golem 7/4,
  Myrkalf 6/3, Giant Boar 5/2, Kobold 3/0, Valdemar 7/4 (beige only),
  Hrimgandr 8/8. Movement priorities: Rock Golem units > buildings >
  resources; Myrkalf buildings > resources > units; Giant Boar and Valdemar
  resources > buildings > units; Kobold buildings > units > resources.

**Environment tiles** (shuffled into the map tile pile at the end of setup;
with Wilderness too, use at most 12 Environment tiles in all):
- Kobold Camp (Harvest): once per controlled territory adjacent to it, a
  player may exchange 1 food or 1 wood for 1 food, 1 wood or 1 fame.
- Thor's Wrath (Combats): its controller gets +1 axe in all combats anywhere.
- Vedrfolnir (Harvest): just before the Harvest its controller may add the top
  map tile next to any open territory (not an Explore action: no clan or
  Development explore effects).
- Naströnd (Explore): impassable. When revealed, put 2 wood on it; the first
  player to control a territory adjacent to it takes them (a tie during an
  Explore: the explorer).
- Landvidi (Combats): in a combat there, the defender gets +2 axes.
- Urdarbrunn (Combats): in a combat there, the defender ignores 1 skull of the
  enemy die.
- Jötnar Camp (Explore, Recruit): when explored, Jötunn Blainn goes on it.
  Blainn is a neutral unit worth 2 axes. During the Harvest, in turn order, a
  player controlling a territory adjacent to the camp may pay 1 food on their
  turn to recruit him: he joins the clan as one of its units (1 unit for
  Winter), placed in a territory of theirs adjacent to the camp. Left alone in
  a territory, or defeated, he goes back to the camp and can be recruited
  again at the next Harvest.

**Central tiles** (setup step F: one drawn at random, or chosen, replaces the
starting tile; the others go back to the box). With five players, the
matching five-player tile (same kind of borders: regular, or impassable for
the Great Lake, Volcano and Relic) goes next to it as before; the territory
with its arrow counts as part of the central territory, ignoring the border
between them.
- Magma Flow (Start of Year): its controller draws 1 more card.
- Yggdrasil (Harvest): the controller of its centre territory gains 5 fame.
- Relic of the Gods (Harvest): impassable. Most units in adjacent territories:
  2 lore; every other player with a unit next to it: 1 lore; tied for most:
  1 lore each.
- Great Lake (Harvest): impassable. Most units in adjacent territories: 2
  food; tied: 1 each (the Wilderness Great Lake's rule). The food belongs to
  no territory, so clan powers and cards can't use it.
- Volcano (Start of Year): at the very start, the first player picks a target
  player (themselves allowed) and rolls a die: per axe the target gains 1
  fame, per skull they remove 1 unit from the map to their reserve (skull/axe
  face: both).
- Mimirsbrunn (Start of Year): after drawing, its controller may take one of
  this year's Development or Achievement cards face down on top of their deck;
  they then take no other Development or Achievement card this year.
- Hrimgandr's Lair (Creature, Winter): Hrimgandr starts on it (card next to
  the board). Strength 8, 8 fame. It never moves and doesn't share its
  territory; it only defends. While alive, Winter costs go up one level for
  everyone: 1-3 units 1 food, 4-6 2 food, 7-9 3 food 1 wood, 10-12 4 food 2
  wood, 13+ 4 food 2 wood. Once defeated it leaves the game, and from then
  on the controller of the Lair draws 1 more card at each Start of Year.
- Wyvern's Den (Creatures module and Wyvern only; Creature, Harvest): at the
  start of year 3 the Wyvern goes on it and into the creature line (Wilderness
  rules). Once it is defeated, the controller of the Den gains 2 fame each
  Harvest. Use this tile, not the Wilderness Den.
- Gate of Helheim (Creatures module only; Combats, Creatures): its controller
  gets +2 axes in any combat against creatures. At the start of the Creature
  phase the first player rolls a die; with at least one skull they draw 2
  creature cards, put one of those creatures on the Gate and the other card at
  the bottom of the creature deck.

**Art** (`webp2/nort/images/tile/waste-*`, `start-*`): the tiles from the
TTS mod 3597126237 (the ones already in `expansion/tile/` reused where they
were), cropped like the core tiles. The impassable five-player tile
(`start-5-wall`) isn't in the mod: it is the regular one with its dashed
borders redrawn as orange impassable lines. Creature cards and round tokens
(`card/creature/`, `token/creature/`) are from the mod's card images; the mod
has only the brown-paw cards, so the beige ones (`-1`) have the paw ring
recoloured. Jötunn Blainn's token is `token/blainn`.

## Expansions (later)

The rest of Uncharted
Horizons (Development and Raid cards, Training Fields, Solo/Automa, drafting
setup, more map tiles). The TTS mod 3597126237 has its cards and the
rulebook is on Tabletopia.

## Interpretations (choices made where the summary above is not enough)

Check these against the rulebook when it is at hand.

- **Automa**:
  - Its Leaders are its warchief (Leader 1) and its companion figure
    (Leader 2). Without the Warchiefs module they are worth 1 like units
    (the rulebook's example counts "Leader + 3 units" as 4); with it, 3.
  - Creatures: the Automa never moves into a territory with a creature;
    creature prompts aimed at it are answered by its bot.
  - The die's choice face: the casualty only when the enemy has one figure
    left in the fight.
  - Explore's rotation row is measured with the tile in place: resources,
    food, wood and spaces of the territory explored from; the closed icon
    prefers a turn that closes any territory; least rotations last.
  - Explore's territory row ranks the Automa's open territories that have a
    free spot; the compass icon also picks the spot next to that territory.
  - Build: it puts a small building on a small space, then a Carved Stone
    space, and on a large space only when no other is free.
  - Snake's Stolen Lore and Rapacious Exploitation adjustments aren't
    implemented (the Automa has no hand or active area).
- **Events**:
  - Happy People acts after drawing, with the other start-of-year Events, so
    its Unrest card may also come from the hand.
  - Blood Moon and Conquests apply to fights between players only. Blood
    Moon's fame counts every figure lost.
  - "Remove 1 unit" (Levy, Infestation, Kraken's Attack) takes a unit first,
    then the warchief, then a companion; Sailor Ghosts and Draugr Invasion
    take units only.
  - Earthquake: wood is paid for as many territories as possible; the unpaid
    ones are those with the fewest large buildings, and the player picks the
    building each one loses.
  - Levy and Infestation look at what a territory produces (tiles and
    buildings). Bountiful Year's territory may be open or closed.
- **Alternative victory**:
  - "Close 6 territories you control" counts territories closed by your own
    Explore actions. Architecture counts every building placed through a
    Build action (Quality of Life's tower doesn't count). Refinement counts
    Remove and Upgrade (Replace doesn't change the deck).
  - The cards are checked at the end of every year, including the last; the
    three-stronghold win stays off even when Large Territories is drawn (it
    is then simply a Map Control card).
- **New Blood** (no rulebook; the boards and cards are the source):
  - Brundr and Kaelinn, and Horse's second warchief Brok (Warchiefs module
    only), use Kaija's rules: one figure, moves with the units, taken as a
    casualty after the units and the warchief, can't be targeted by card
    effects on enemy units, doesn't count for the 14-unit limit. Unlike
    Kaija they may enter enemy territories. Brundr and Kaelinn don't count
    for winter (Uncharted Horizons rulebook); Brok does, like a warchief.
    Without the Warchiefs module Horse has no warchief and no Brok.
  - Brundr and Kaelinn's Move 1 doesn't cross Rough borders; the units with
    it may stay behind (any number go). It is offered before the card (or
    after it), and only for clan cards played, not copied ones.
  - Dragon: only units (not warchiefs or companions) go on the Pyre, and only
    from fights between players. Units on the Pyre are neither on the map
    nor in the reserve (Dragon starts with 13 in reserve). Without a
    sacrifice Dragon gets nothing at the Harvest (no fame, no resources); the
    choice is made before the Harvest. The extra food or wood needs a
    territory Dragon controls. A captured unit with the Pyre full just goes
    back to its owner's reserve.
  - Kraken: a High Tide token goes back as soon as Kraken has no figures in
    its territory. Its casualty applies to fights between players only. The
    Kraken Clan card's food or wood doesn't depend on the territory's icons.
  - Ox: the token goes on the first free small building space (not Carved
    Stone spaces) of the explored tile, even in a neutral or enemy territory;
    none if the tile has no such space. Tokens are used in fights between
    players only, chosen before food is spent. A space with a token can't be
    built on.
  - Horse: "close a territory" means one Horse controls, closed by Horse's
    Explore; one choice per territory. Quality of Life's resource is one of
    the kinds the territory produces.
  - Rat: the closing units are placed automatically (while the reserve
    lasts). Rat Clan's wood stays with Rat if the build doesn't use it.
  - Squirrel's after-harvest gain comes before the trades.
  - Howl from the Sea's unit for a lost combat goes where the first group
    retreats.

- **Wilderness**, where the rulebook says nothing:
  - The Swamp's unit is lost when the figures enter it (units first, then
    the warchief, then Kaija). Figures in the Swamp must leave it with the
    same Move, all together, before any other figures move. The Swamp
    can't be entered with a Fallen Valkyrie or a Wyvern in it, nor left with
    a Brown Bear in it, so no one can be stuck there. Hidden Ways, Bribery,
    Intimidate, retreats, recruiting, setup units and the "no units left"
    placement can't use it.
  - Draugr Jötunn: a player with 2 or more resources must pay (choosing
    which 2); only a player who can't pay is attacked.
  - Eldthurs and Draugr Jötunn act again when they can't move (like the core
    Draugr); Hvedrung doesn't: it only calls a creature when it appears or
    actually moves. Creatures appearing at setup don't act, Hvedrung
    included.
  - The Wyvern driven back to its Den gives no fame (it isn't defeated); it
    gives its 6 fame only when beaten in its Den. A player left with no
    figures after removing the unit before the fight loses it. The Wyvern
    does nothing when it appears (its Den was just explored, so it's
    empty).
  - The Great Lake counts units and warchiefs, not Kaija. The Poisonous
    Swamp removes a unit, or the warchief if there is none (not Kaija). Both
    act on every territory around the tile, however many of its sides it
    touches.
  - The Wyvern's Den gives exactly 2 fame at the Harvest (instead of the 1
    a closed territory of one tile would give), and like other fame from the
    map not with a Wolf creature in it; Ruins' fame likewise.
  - A Spectral Warrior's buildings: no resources from Food Silos,
    Woodcutter's Lodges and Carved Stones, no fame from Altars of Kings, no
    extra card from Forges, no Training Camp units, no Fortress or Defense
    Tower in combat. They still count as buildings (Builder, large building
    victory, creature priorities).
  - The Ancestral Graveyard's own territory is the one with the graves (the
    tile's lair is on the other side of the Rough border). A Spectral Warrior
    may go on a lair with a creature on it.

- **Building spaces** (not confirmed; the rulebook text isn't in the repo):
  a small building may go on a large building space as well as on a small
  or Carved Stone one. Before 2026-10-04 small buildings could only use
  small and Carved Stone spaces. If the rulebook says large spaces are only
  for large buildings, remove `LargeSpace` from the small buildings' kinds
  in `MapExpansion.buildOptions` (`nort/map.scala`); the space choice still
  works between small and Carved Stone spaces.
- **Kaija** (Bear): one figure. It may be one of the three setup figures
  (two units and Kaija), or be recruited instead of a unit. It moves like a
  unit, alone or with others, but not into a territory with enemy figures
  unless The Bear Awakens was played this year. It counts for control and
  for "1 food per figure" in combat, adds 2 combat points, and is taken as a
  casualty after all units. It doesn't count for winter, the 14-unit limit,
  Warlord or Training Camps. Removed Kaija goes back to the reserve.
- **Bear Clan card**: when Kaija is in the move, the territory it leaves
  gives its food and wood (tiles and buildings) before any fight.
- **Scorched Earth** (Snake): before resolving any Snake clan card, and
  again after the Snake Clan card's move, Snake may move the token to an
  enemy-held territory next to one it controls. Snake gets +1 combat point
  fighting there. At harvest, if another player controls that territory,
  Snake may take one resource of a kind it produces; that player gets one
  fewer.
- **Hunters, Woodcutters, Loremasters**: icons on tiles and buildings count;
  if the reserve is too small, units are placed one at a time by choice.
- **Raiding Party**: any enemy unit on the map; the resource comes from
  what the territory produces (tiles and buildings).
- **Future Sight**: may take a face-up Achievement before year 7.
- **Hidden Ways**: to any open territory, also an enemy one (then a fight).
- **Bribery**: to any adjacent territory (Rough borders ignored) holding at
  most one other player; the moved units attack whoever is there.
- **Teamwork**: two of Recruit 1, Move 1, Explore, Build, like Feast.
- **Enemy Secrets, Stolen Lore, Legendary Heroes** can't copy each other or
  Defensive Strategy.
- **Defensive Strategy**: every time a player plays a card, opponents holding
  it are asked in turn order (so a prompt shows who holds it). The cancelled
  card goes to the discard pile and the player goes on with their turn.
- **Glory of the Clan**: the territory's resources are collected after
  building (so a new Food Silo etc. counts).
- **Intimidate**: the pushed unit goes to any adjacent territory that is
  neutral or the defender's and not in a fight; if no defender is left, the
  territory is taken without a fight (no Stag fame).
- **Amenities**: small buildings never take a space; they are drawn next to
  the territory number.
- **Industrious Villagers**: a Carved Stone can only replace a building on a
  Carved Stone space.
- **Annexation**: the player chooses Explore first or Move first; exploring
  is optional.
- **Boar Clan**: "explores without closing any territory" is read literally:
  closing anyone's territory (or a neutral one) costs the lore.
- **Teams**, where the summary above says nothing:
  - Teams follow the seats: seats 1 and 3 (1, 3 and 5 with six players)
    against the others; the turn order alternates between the teams.
  - Teammates are never enemies: no fights, and cards and powers that target
    enemies, opponents or enemy territories (Plunder, Capture, Raiding Party,
    Bribery, Call to War, Spy, Ancestral Curse, Rapacious Exploitation, Enemy
    Secrets, Stolen Lore, Defensive Strategy, Scorched Earth) skip teammates.
    Hidden Ways can't go to a teammate's territory; Bribery can't move units
    into their own teammate's territory. Retreats can't go there either.
  - Moving through: units entering a teammate's territory must leave it with
    the same Move action, so they may only enter if their remaining moves
    can take them out (Rough borders count double); all the figures passing
    through move on together, and the Move can't end while any are there.
    Kaija may pass through a teammate's territory.
  - Tiles still can't join two players' territories, teammates included.
  - The 1:1 trade is a swap: give one resource, take one of another kind
    from the teammate, any number of times during your trade step.
  - Fame victory: the team with the highest total fame wins (both players).
    Ties: the teams' total territories, then units, then buildings.
  - Three closed territories with large buildings: a team wins when one of
    its players has them. If both teams do, the teams' total fame, then
    territories, units and buildings decide.
- **Creatures**, where the rulebook says nothing:
  - Kaija counts as a unit for the Draugr (units go first) and as 2 combat
    points; a Fallen Valkyrie in a territory makes a tile placement illegal
    if it would join it with units.
  - Setup units don't go in a Fallen Valkyrie's territory unless the tile has
    no other empty territory.
  - A Fallen Valkyrie must be fought by the units that share its territory at
    the end of any Move action; they can't move out before.
  - Two creatures in a territory both block nobody, but nobody may enter a
    territory with two Fallen Valkyries.
  - A player's own die: the point-or-casualty face is always taken as the
    point (a casualty does nothing to a creature). Axe Throwers adds 1 point,
    Shieldbearers cancel 1 casualty, Defense Towers do nothing (they only
    add casualties), Fortresses add 2 to a defender, Scorched Earth adds 1.
  - Wolf Clan collects 1 food for beating a creature as the attacker; Stag
    Clan gains 1 fame only for beating a Fallen Valkyrie (it takes the
    territory).
  - Recruiting into a neutral territory (no units on the map, some cards)
    can't use a Brown Bear's or Fallen Valkyrie's territory; Hidden Ways,
    Bribery, Intimidate and retreats can't go into a Fallen Valkyrie's.
  - The Wolf's harvest rule also applies to collecting "as at harvest"
    (Raven Clan closing, Glory of the Clan).
  - With the second chance tile, a lair creature acts as from an Explore.
- **Warchiefs**, where the rulebook says nothing or the game simplifies
  (confirmed by the owner on 2026-10-03):
  - Casualties take units first, then the warchief, then Kaija (the
    rulebook lets the player choose; keeping the warchief is the usual
    choice).
  - The warchief counts as a unit for winter, control, Warlord and ties, but
    not for the 14-unit supply (it's its own miniature) or Training Camps.
  - At setup a Bear player may place one unit, Kaija and Borgild together.
  - A retreating warchief goes with the first group (like Kaija).
  - The Draugr removes units before the warchief.
  - Powers also work in fights against creatures. Brand brings a unit from
    an adjacent territory held only by Stag.
  - Liv's reroll is offered after seeing the roll; the point-or-casualty
    choice comes after the reroll decision.
- **Warchief upgrade cards**:
  - The card bonuses also apply in fights against creatures started by the
    Move (Borgild's cancelled casualty, Liv's Cunning, Svarn's Menders).
    Confirmed by the owner on 2026-10-03.
  - Not confirmed yet:
    - Svarn's Menders brings back units only (not Kaija or the warchief),
      and only casualties, not units lost for having nowhere to retreat.
    - Halvard's Craft: a "new" territory is one with none of the areas the
      player controlled before the Move; the free building follows the usual
      build rules (space, one of each type, no Brown Bear).
    - Signy's Celerity: units in an enemy territory with the token may move
      on; a fight happens only if some stay.
- **Wastelands**, where the rulebook says nothing or the game simplifies:
  - Central tiles impassable in the middle (Relic, Great Lake, Volcano) have
    no middle territory, like the Wilderness Great Lake; the Kobold Camp,
    Jötnar Camp and Naströnd likewise. The printed wood on Naströnd and the
    apples on the Great Lake are reminders of their effects, not resources.
  - Five players with a regular central tile: the five-player tile's middle
    territory joins the central tile's east territory across the side as
    usual, and that territory is joined to the middle territory (the border
    between them ignored). With an impassable central tile the five-player
    tile's middle just joins the east shore.
  - The Wyvern's Den and the Gate of Helheim can only be chosen, or drawn,
    with the Creatures module. The central Den's middle still gives the usual
    fame of a closed territory; the 2 fame once the Wyvern is gone come on
    top. A Wyvern beaten away from its Den goes back to it, as in Wilderness.
  - Hrimgandr is in the game even without the Creatures module (it only
    defends, so only its fights are needed).
  - Wilderness together with Wastelands: 12 Environment tiles are drawn at
    random from both sets (the Wilderness Den still goes below the first
    tiles, as in Wilderness).
  - Jötunn Blainn moves with his clan's figures: he goes along with the last
    group of figures leaving his territory, and is a casualty after units and
    the warchief, before Kaija. Card effects on units don't target him.
  - The Volcano erupts every year, the first one included; its die removes
    units only (not the warchief, Kaija or Blainn), chosen by the target.
  - Myrkalf: the 1 wood is paid automatically when the owner has it.
  - Gate of Helheim: the skull/axe face counts as a skull; the two cards are
    drawn without reshuffling the discard pile (fewer if the deck is short).
  - Mimirsbrunn: the card goes on top of the draw pile and counts as the
    player's Development or Achievement card for the year (`foresaw`).
  - Kobold exchanges and the Kobold Camp happen right after the Harvest's
    collection, then Blainn's recruitment, then the usual trades.
  - Vedrfolnir's tile is placed like a second-chance tile next to an open
    territory; a tile that fits nowhere goes to the bottom of the pile.
