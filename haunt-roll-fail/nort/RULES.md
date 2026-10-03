# Northgard: Uncharted Lands — rules notes

Working notes for the HRF adaptation (package `nort`), summarized from the
core rulebook. The rulebook PDFs are not in the repo. Card text overrides
these rules ("the cards are always right").

## Status

| Part | State |
|---|---|
| Year loop (phases 1–5), seven years | done |
| Decks: draw / hand / active / discard, reshuffle only when drawing from an empty pile | done |
| Wait, Replace (1 lore), Remove (2 lore), Upgrade (3 lore), Pass | done |
| Development deck (2 Early + 4 Advanced per player), Achievements in year 7 | done, cards are placeholders |
| Harvest trade (any 3 resources for 1) | done |
| Winter costs and Unrest cards | done (counts units, which never change yet) |
| End-of-game fame scoring | done (no territory fame yet) |
| Playing cards (Recruit, Explore, Move, Build, Feast, Draw, Special) | not started — needs the map and card list |
| Map tiles, territories, borders, exploring | not started — needs tile data |
| Buildings, combat, clan powers, three-closed-territories win | not started |
| Starting card list | provisional: Recruit ×2, Explore, Move, Build, Feast |
| Development, Achievement, clan initial/upgrade card contents | missing |

## Components (core box)

- 7 clans: Bear, Boar, Goat, Raven, Snake, Stag, Wolf. 2–5 players.
  Each player also picks a color (14 units, 6 starting cards of that color).
- Clan cards: 1 initial + 2 upgrades per clan.
- 16 Early Development, 36 Advanced Development, 7 Achievement, 10 Unrest cards.
- 35 map tiles including the starting tile; a second starting tile for 5 players.
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
  to an enemy territory adjacent to one they control. +1 combat point fighting
  there (attacking or defending); at harvest may take one resource from that
  territory instead of its owner.
- **Stag**: +1 fame per territory conquered in combat or closed by exploring.
- **Wolf**: winning a combat as the attacker gives 1 food.

## Expansions (later)

Wilderness (creatures, Environment tiles), Wastelands (creatures, Environment
and Central tiles), New Blood (7 more clans: Dragon, Horse, Kraken, Lynx, Ox,
Rat, Squirrel, with warchiefs), Uncharted Horizons (Development / Event /
Raid cards, alternative victory conditions, Training Fields, Solo/Automa,
drafting setup, more map tiles).
