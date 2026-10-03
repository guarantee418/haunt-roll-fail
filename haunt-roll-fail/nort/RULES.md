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
| Card list with names, fame, text and images (`cards.scala`) | done: 21 clan, 16 Early + 35 Advanced (one Advanced card missing from the images), 7 Achievement |
| Playing cards: Flash cards before/after the main card | done |
| Card effects | done: every card in `cards.scala` has an effect (see Interpretations below for the choices made); only Scout Camp's redraw happens before the tile is shown |
| Harvest trade (any 3 resources for 1) | done |
| Winter costs and Unrest cards | done (counts units, which never change yet) |
| End-of-game fame scoring | done (no territory fame yet; of the Achievements only Warlord scores) |
| Map tiles, territories, borders, exploring | done: tile data in `tiles.scala`, territories and placement rules in `board.scala`, setup, Recruit, Move, Explore, Build, Feast and combat in `map.scala`. Tile images in `webp2/nort/images/tile/` (`tile-31` to `tile-33` and `start-5` from the owner's photos) |
| Card display (like Arcs): Development and Achievement cards in the `court` pane on top for everyone, your hand (and played cards) in the `hand` pane at the bottom; click or tap a card to see it full screen | done |
| Player colors, chosen per clan on the setup screen (default blue, red, yellow, purple, green by seat) | done; no green starting card images, green uses the blue ones |
| Game length 5–10 years (10-year variant: 3 Early + 6 Advanced per player), Fame victory only, First seat goes first | done (options) |
| Creatures module (core box), with the More Creatures variant | done 2026-10-03 (`creatures.scala`) |
| Other modules and expansions | options shown, disabled; `Module` groundwork in `options.scala` |
| Buildings, combat, three-closed-territories win | done |
| Clan powers | done: all seven (Bear's Kaija and Snake's Scorched Earth added 2026-10-03) |
| Tile data checked against the art | done 2026-10-03 for all 35 core tiles (numbers, unit markers, building spaces) |
| No units at the end of a year and no neutral territory | done: a tile is drawn and placed anywhere it fits with an empty territory |
| Unrest supply (10 cards; then −5 fame and discard the top card) | done |
| Starting card list | Recruit, Move, Explore, Build, Feast ×2 (from the cards) |

## Components (core box)

- 7 clans: Bear, Boar, Goat, Raven, Snake, Stag, Wolf. 2–5 players.
  Each player also picks a color, independent of the clan: blue, red,
  yellow, purple or green (14 units, 6 starting cards with that color's
  banner).
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
  each type per territory; limited by tokens; permanent.
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
2v2 teams.

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

## Expansions (later)

Wilderness (creatures, Environment tiles), Wastelands (creatures, Environment
and Central tiles), New Blood (7 more clans: Dragon, Horse, Kraken, Lynx, Ox,
Rat, Squirrel, with warchiefs), Uncharted Horizons (Development / Event /
Raid cards, alternative victory conditions, Training Fields, Solo/Automa,
drafting setup, more map tiles).

## Interpretations (choices made where the summary above is not enough)

Check these against the rulebook when it is at hand.

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
