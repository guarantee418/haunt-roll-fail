# Brass: Pittsburgh: the rules as they will be played here

Brass: Pittsburgh (game design Gavan Brown, Brass core systems Martin Wallace, Roxley Games, 2026), for 2 to 4 players. Written
from Roxley's English rulebook v2026.01 (11 pages, "a living document"; the latest is at roxley.com/brass-pittsburgh-rulebook).
The rulebook isn't in the repository. Page numbers below are the rulebook's.

Nothing is built yet: this file is the rules, the readings where the rulebook leaves room (marked **Reading**), and the questions
the rulebook doesn't answer (marked **Open**), which the components or Roxley must settle. See `HANDOFF.md` for the plan and the
data still missing.

## Components (p.2)

- 1 board, two sides: one for 2 players (marked with a 2-player icon at its top right), one for 3 and 4.
- 68 Location and Industry cards. **Open:** the card list, with each card's player-count icon (bottom right).
- 4 player boards (mats), 4 character tiles (two sides each; the side is the player's choice and only changes the look:
  **Open:** confirm), 4 player aid boards.
- 144 industry tiles, 36 per color: per color 7 Kerosene, 1 Oil City, 5 Coal Plants, 1 Polyphase Alternator, 8 Manufactured Goods,
  1 Locomotive Works, 3 Coke Plants, 3 Automobiles, 5 Steel Mills, 2 Skyscrapers.
- 84 link tiles, 21 per color: per color 12 trains (3 heavy on the mat, 2 heavy beside it, 9 light? see Setup) and pipelines.
  **Open:** the exact split of each color's 21 (Setup counts 2 heavy trains, 2 pipelines and 9 light trains in the supply, plus the
  mat's train and pipeline spaces).
- 4 VP markers, 4 income markers, 77 money tokens (1, 5, 15), 12 whiskey tokens.
- Cubes: 18 steel (gray), 18 coal (black), 14 iron (orange), 10 coke (red); 18 oil barrels. The general supply is limitless.
- 5 oilfield tiles, 6 Quality Rewards tiles, 5 merchant tiles.

## Setup (p.4-5)

Board:

1. The board side for the player count.
2. With 2 or 3 players, remove the cards without that player count's icon. Shuffle the deck onto its space.
3. With 2 players, remove the oilfield tile that produces 3 oil and the merchant tile that rewards $15. The oilfield spaces
   used are shown at the bottom of each side of the board.
4. A random merchant tile on each merchant space, gold border up.
5. The other oilfield tiles shuffled face down, one on each oilfield space.
6. 3 random Quality Rewards tiles face up on the Quality Rewards spaces; the rest go back to the box.
7. The coal and iron markets filled.
8. The steel markets filled; one coke cube in the top coke space of each coke market.
9. The general supply: money, whiskey, oil, cubes.

Each player:

1. A player mat.
2. $30.
3. A character tile (either side), and the industry tiles of its color.
4. The industry tiles stacked on their spaces of the mat, solid colored side up (some tiles have no solid colored side and are
   placed as they are).
5. One heavy train or pipeline on each of the mat's train and pipeline spaces; 2 heavy trains, 2 pipelines and 9 light trains in
   the player's personal supply.
6. VP marker on 0 of the progress track; income marker on space 10 (income level 0).
7. 8 cards in hand, kept hidden.
8. One more card face down on the player's discard pile (era 1 only).

Turn order: the character tiles shuffled onto the turn order track at random.

## Eras and rounds (p.6)

- Two eras of 8 rounds. **Reading:** an era ends after the round in which every hand is empty (p.11); with 8 cards in hand and
  2 actions a turn (1 on the first turn), the deck sizes give 8 rounds at each player count. **Open:** check against the card counts.
- A round: each player in turn order takes a turn, starting at the leftmost character tile.
- A turn: exactly 2 actions, one at a time; the same action may be taken twice. Exception: the first turn of era 1 is 1 action.
  Each action discards 1 card face up onto the player's discard pile.
- After both actions, draw back up to 8 while the deck lasts; once it is gone, hands shrink until they are empty.
- Money spent on a turn goes onto the player's character tile; it sets the next turn order.

## End of round (p.6)

1. **Turn order.** The character tiles are rearranged from least spent (left, first) to most spent (last); ties keep their relative
   order. The spent money goes back to the supply.
2. **Income.** Each player gains (or pays) its income level: the number on the coin under its income marker's space. Not after the
   last round of the game. **Reading:** also not done after the last round of era 1? The rulebook says only "the last round of the
   game", so era 1's last round pays income.
   - **Bankruptcy**, when a player can't pay a negative income: return all its whiskey to the supply, lose 40 VP (the VP marker may
     go below 0), and move its income marker to income level 0 (space 10). **Reading:** the player pays nothing then; it keeps its
     money. **Open:** does it pay what it can first?

## Income levels (p.7)

- The income level is the number on the coin below the marker's space (several spaces share a level).
- An income increase moves the marker up that many **spaces**; a decrease moves it down that many **levels**, to the highest space
  of the new level.
- No action may drop the income level below -10.

## Actions (p.6, 8-10)

Every action discards a card. Build needs a matching card; the others take any card.

### Build (p.8)

1. **Card.** A Location card: any industry the location allows, at that location, even outside the player's network. An Industry
   card: that industry, at a location in the player's network (anywhere if the player has no industry or link tile on the board).
   Any card and 1 whiskey: the card counts as any Location or Industry card.
2. **Tile and space.** The lowest accessible tile of that industry (see Accessible tiles) on an empty space of the location that
   allows it. If the location has an empty space showing only that industry, it must be used; otherwise a space showing several
   icons. Exception: upgrading (below).
   - Era 1: one industry tile per location per player. **Reading:** "you may only build a single industry tile in each location"
     is per player, as in the other Brass games. **Open:** confirm.
   - Tiles marked with the era 1 half-moon below their cost banner may be built only in era 1; those marked with the era 2 moon only
     in era 2.
3. **Cost.** The money, resources and income change shown left of the tile on the mat, in any order.
4. **Upgrading.** A tile whose cost shows the upgrade icon is built only by upgrading one of the player's own industries of the
   type shown, any level: the old tile must be the player's and flipped; it is removed at once and scores its VP; never on an empty
   space.
5. **Produced resources.** Cubes or oil shown at the tile's bottom right are taken from the supply onto it. If the tile is connected
   to one or more matching markets with empty spaces, they are sold there at once: as many as possible, to connected markets in the
   order the player picks, each cube earning the money (and coke's VP) on the space it fills. If the last one is sold, the tile
   flips and gives its rewards. What can't be sold stays on the tile. Selling to markets happens only when the tile is built.
6. **Already-flipped tiles** give their reward icons at once, except their VP, scored at the end of the era or when upgraded away.

**Oil City** (p.8, 10): built with the Oil City card (Kerosene icon) directly on an oilfield the player's pipelines connect to, in
era 2 only. All oil on that oilfield goes onto it, plus 3 more; oil later added to that oilfield goes onto the Oil City too. Oil
City sells nothing by itself; any player may take its oil through connected pipelines; it flips when the last barrel is taken, for
$25. **Open:** "built directly on an oilfield using a Kerosene card": is the card the Oil City location card or any Kerosene
industry card?

### Network (p.9)

1. Discard any card.
2. Place an available link tile on an empty line: adjacent to a location in the player's network (anywhere if it has no tiles on
   the board), paying its cost. Placing a train on a railroad line with a whiskey, income or free develop icon gains that bonus.
3. Optional, in the same action: pay $10 more and place a second link of any type (its cost too). The first link's effects resolve
   before the second is placed, and they may help it.

Link types and costs (the same in both eras):

- **Light train:** 1 coal. On railroad lines between locations. Scores 1 VP for each link icon in its two locations at each era's
  end; removed after era 1's scoring (back to the owner, built again in era 2).
- **Heavy train:** 2 steel. Like a light train, plus a reward when built (its type, p.10); stays on the board after era 1. Some heavy
  trains are locked on the mat until the industry tile right below them is built or developed; an unlocked one goes to the personal
  supply at once.
- **Pipeline:** 1 iron. On the black pipeline routes between a location and an oilfield. If its route shows oil icons, that many oil
  barrels go from the supply onto the oilfield. The first pipeline to an oilfield flips its tile: the left side's reward goes to the
  player at once; in era 1 the tile stays and at the end of era 1 the oil on its right side is added to the oilfield; in era 2 the
  right side's oil is added at once and the tile goes to its space at the bottom of the board. Pipelines stay all game and never score.

Heavy train types (p.10):

- **Resource train:** the coke, steel or oil shown on it is placed on it when built; it isn't sold to markets; only its owner may
  consume it, moved along trains to where it is needed. The steel on the steel train can't pay for its own building.
- **Whiskey train:** 1 whiskey when built.
- **Double link train:** scores its links twice.
- **Replacement train:** built on a line holding another player's light train: that train scores for its owner at once (as at an
  era's end) and goes back to their supply, then the replacement train takes the line.

Pipeline types (p.10):

- **Income pipeline:** its owner's income goes up by the number shown for each barrel travelling through it.
- **Money pipeline:** its owner gains the money shown for each barrel through it.
- **VP pipeline:** its owner gains the VP shown for each barrel through it.
- **Reserve pipeline:** adds 1 oil to the connected oilfield. No oil can travel through it (the oilfield's reward still flips). When
  placing a later pipeline, its owner may swap it for another pipeline from their supply, the reserve going back to their supply.

**Open:** the reward amounts on each player's pipelines and heavy trains (the mat), and whether the colors' sets differ.

### Develop (p.10)

1. Discard any card.
2. Remove 1 or 2 accessible industry tiles from the mat, from the same path or not; they are out of the game.
3. Pay 1 iron or steel for each (player's choice), by the usual sourcing; with no connection, from Vanderbilt.

**Reading:** a free develop (link icon, Locomotive Works) removes a tile without the card or the iron/steel.

### Sell (p.9)

Sellable industries: Kerosene Refineries, Manufactured Goods, Automobile Manufacturers. (Industries flipped by depletion aren't sold.)

1. Discard any card.
2. Pick one or more of the player's unflipped sellable tiles, each connected to at least one merchant tile showing its icon.
3. Oil: each tile needs the oil at its top right, from any connected oilfield through a pipeline (its owner's pipeline reward for
   each barrel), then along trains. Then the oil goes back to the supply.
4. Merchant rewards: for each connected face-up merchant tile sold to, its reward, then it flips face down. **Reading:** "each
   connected faceup merchant tile you are selling to": one merchant per tile sold, the player's pick among those connected, and
   a face-down merchant still buys but gives nothing. **Open:** confirm.
5. Quality Rewards (optional): count the stars on the tiles sold, plus 1 per whiskey spent; if the total meets a face-up Quality
   Rewards tile's requirement, the player may claim that one tile (only one per Sell action) and gain its reward; it flips face down.
6. The sold tiles flip and give the rewards at their bottom right (VP only at the era's end).

### Loan (p.10)

Discard any card, take $30 and move the income marker down 3 levels (to the highest space of the new level). Not allowed if it would
take the income level below -10. Loans are never repaid.

### Pass (p.6)

Discard any card and gain 1 whiskey.

## The network and connections (p.6-7)

- A **location** is a city or merchant space with one or more railroad lines. Oilfields aren't locations.
- A player's **network**: every location holding one of its industry tiles or adjacent to one of its link tiles.
- Two locations are **connected** when a continuous route of train links (any color, light or heavy; not pipelines) joins them.

## Resources (p.7)

Cube types: coal, iron, steel, coke. Cubes travel any distance along train links of any player; never through pipelines.
Oil leaves an oilfield through a pipeline, then travels along train links. Each cube may come from a different source:

1. **An industry tile:** free, from any unflipped tile of any player holding that cube and connected to where it is needed. Must
   be used first when possible. A tile whose last cube or barrel is taken flips and gives its rewards.
2. **A resource heavy train** the player owns (the player may, not must).
3. **A market** connected to where it is needed: any of the matching markets, paying the price of the cube taken. 3 coal/iron markets
   inside the board, 3 steel/coke markets around its edge. Selling coke to a market also scores the VP shown (4).
4. **Vanderbilt:** without a connection (or if the player chooses not to use one, p.7), any cube from Vanderbilt at his price: coal or
   iron $7, steel or coke $10.

**Reading:** step 1 is forced; steps 2, 3 and 4 are the player's choice ("If you can't source a cube from an industry tile and you
can't (or don't want to) source one from the market, Vanderbilt can deliver"). The comparison table on p.12 lists 2 as "heavy
trains or Vanderbilt or a connected market", in any order. **Open:** which tile when several are connected (the other Brass games
take the closest; Pittsburgh says "any").

## Whiskey (p.7)

At most 3 held (more are lost). One whiskey: a card counts as any Location or Industry card for a Build; or +1 star in a Sell.

## Accessible tiles (p.7)

The mat has five industry paths: Kerosene, Coal, Manufacturing, Coke, Iron & Steel. A tile is accessible when it is the bottom tile
of a path or the next one along the arrows. The steel path branches into the Skyscrapers; the coke and manufacturing paths have
shortcut branches to either side. A heavy train or pipeline space on the mat becomes accessible the same way, and moves to the
personal supply at once. **Open:** the whole mat layout, the branches and which tiles unlock which link pieces.

## Industry tiles (p.7)

Unflipped when newly built (some are already flipped), flipped once they have given their rewards and will score. A tile may show:
resources placed when built, oil to sell, quality stars, rewards when flipped (income, money, a bonus sell, whiskey), its VP and
its link points (the link icons that make adjacent trains score).

## Crown Jewel industries (p.10)

At the top of each path, unique effects:

- **Oil City** (Kerosene): see Build.
- **Polyphase Alternator** (Coal): already flipped; built by upgrading a Coal tile; gains money equal to twice the current income.
  At the end of the era, VP equal to the empty industry spaces on the player's mat.
- **Locomotive Works** (Manufacturing): era 2 only; already flipped; built by upgrading a Manufactured Goods tile; 3 free develops.
  At the end of the era, 4 VP per heavy train the player has built.
- **Skyscraper** (Iron & Steel branch): already flipped; an optional bonus sell (no card discarded), resolved at once.

**Open:** "Polyphase Alternator ... rewards with money equal to 2x your current income": income level, not space, and nothing if
negative? Its two VP icons and its "1x" icon.

## End of each era (p.11)

1. **Train links:** each train scores 1 VP per link icon in its two adjacent locations. Era 1: light trains are then removed back to
   their owners; heavy trains stay. Era 2: every train is removed as it is scored. Pipelines stay and never score.
2. **Industry tiles:** each flipped tile scores the VP at its bottom left; unflipped tiles score nothing.

End of era 1 only:

3. Every level I industry tile, flipped or not, is removed from the board to the box.
4. Oilfield production: the oil below each flipped oilfield tile goes onto its oilfield.
5. The deck: collect every discard pile, turn each pile's bottom card so all face the same way, shuffle them into a new deck, and
   each player draws 8. **Reading:** the player's own face-down card from setup goes into the new deck too.

## Winning (p.11)

Most VP after both eras. Ties: the highest space on the income track, then the most money.

## Differences from the other Brass games (p.12)

From the comparison table: no overbuilding (only upgrading one's own flipped tiles); 6 markets on the board, coal and iron inside,
steel and coke around the edge; coke sold to a market also scores 4 VP per cube; a second link costs $10 more; heavy trains stay
after era 1; the first pipeline to a face-down oilfield reveals it.
