# Pincer: A Gentleman's War — rules as played here

Pincer is Julian Birke's game (Birke Publishing), a work in progress. This site plays it from the
**second draft of the rulebook** (`Rulebook v2.pdf`, 2026-08-28, shared from the designer's Drive). The
pictures are from the Tabletop Simulator mod "Pincer: A Gentleman's War (Updated for First Draft)"
(Steam workshop 3778939778). The rulebook is a draft and leaves several things open; the choices made
here are listed under **Interpretations** so the playtesters can check them against what the designer
means. Change a rule in `game.scala` and note it here.

## Components

- Board: one side of the double-sided board (the mod has only that side), 149 hexes in 13 rows
  (`board.scala`). Terrain: wheat field (Wheat), water (Fish), hills (Mustang), forest (Lumber), stone
  (Stone), mountain (Iron).
- Per player (red, green, blue, yellow): 4 colonies, 7 musketeers, 6 cavalry, 5 cannons, 5 frigates,
  3 ports, 3 forts, 8 roads, 2 bridges, 3 Declare War cards.
- Six resource decks of 20: 19 resource cards and one Maneuver card each (Wheat: Retreat, Fish:
  Broadside, Mustang: Fortify, Lumber: Ambush, Stone: Outflank, Iron: Hideout).
- Two resource dice, each face a resource.
- 14 Royal Seals: 4 Secret Societies, 6 Trading Companies, 4 Military Badges. **8 seals win.**

## Setup

- Every player gets one of each resource card (taken out of the decks before they're shuffled).
- In seating order each player places a colony and a musketeer in one hex: land, not a mountain, not an
  edge hex, at least 3 hexes from every other colony over land (water separates).
- The player who placed last takes the first turn; play goes on in seating order.

## A turn

1. **Harvest** (always first): the active player rolls both dice and picks one for themselves; everyone
   else takes the other. Each colony gives its controller one card of their resource if a hex next to it
   has that terrain and no other player's unit in it. A colony annexed by an enemy gives to the
   annexing player.
2. Then, in any order (the draft's Mobilization and Development are only suggestions): move and attack
   with units, declare war or make peace, build, recruit, revive, trade, found or buy out trading
   companies. **End turn** passes to the next player.

## Costs

| | Cost |
|---|---|
| Colony | 2 Lumber, 2 Stone, Iron, Wheat |
| Road | Stone |
| Bridge | Stone, Lumber |
| Port | 2 Lumber, Iron |
| Fort | 3 Stone, Lumber (repair: Stone, Lumber) |
| Musketeer | 2 cards, each Wheat or Fish (revive: Wheat or Fish) |
| Cavalry | 2 Wheat, Mustang (revive: Mustang) |
| Cannon | 4 Iron (revive: 2 Iron) |
| Frigate | 2 Iron, 2 Lumber (revive: Iron, Lumber) |

## Units

Each unit has one action a turn: move, attack, or be revived. New and revived units do nothing until
their owner's next turn.

- **Musketeer**: move 1 or attack 1; attacks and captures land units only.
- **Cavalry**: move up to 2, or move 1 and then attack 1 (also after capturing with that move).
- **Cannon**: move 1 or attack up to 2; attacks land units, frigates and forts; captures land units only.
- **Frigate**: move up to 2 water hexes or attack up to 2; attacks frigates, land units and forts;
  captures frigates only; carries one land unit.
- Land units never enter water or mountains (a mountain only with Hideout, or one with their own colony),
  and never move through other players' units. Cannons and frigates don't shoot over mountains.

## Combat

- Only players at war attack each other. An attack injures the target (drawn on its side). A unit moving
  into a hex where every enemy unit is injured captures them; captured units go to the capturer, counted
  for the unit type that captured them. A captured frigate's passenger is captured too.
- Injured units can't attack (musketeers can with the Royal Musket Badge) but can move and capture.
  They are revived on their owner's turn, never the turn they were injured.
- **Counter-Action**: after each attack or capture by the active player, the player hit may move or attack
  with one unit not used that turn. A Counter-Action gets no Counter-Action back.
- **Forts**: an undamaged fort keeps enemies from attacking or entering its colony. Cannons and frigates
  damage it (drawn upside down); then only cannons and frigates can attack the units inside, and once
  they're all injured, enemy land units enter, capture them and annex the colony.
- **Annexation**: an enemy land unit in a colony annexes it while it stays. The annexing player collects
  its harvest, its roads and port give no trade bonus, and from the annexer's next turn they may remove
  the owner's damaged fort and build their own. A player whose colonies are all annexed may surrender.

## Maneuver cards

Played from the hand, then discarded:

- **Ambush** (Lumber): during another player's turn, a unit in a forest moves or attacks.
- **Outflank** (Stone): a unit on a stone hex gets one more move or attack, both before any Counter-Action.
- **Broadside** (Fish): the same for a frigate.
- **Retreat** (Wheat): when a unit on a wheat field is attacked, it moves to an empty hex within its
  movement range instead of being hit.
- **Fortify** (Mustang): when a unit on hills is attacked, the attack is deflected.
- **Hideout** (Iron): a unit next to a mountain enters it.

## War and peace

A player declares war by giving a Declare War card (three each): a **Gentleman's War** lets the declarer
attack from their next turn (the other player may attack at once); a **Boar's War** attacks at once, with
a penalty. Peace needs both: one offers on their turn, the other accepts. Players may give cards to
each other and return captured units, which go back into one of their owner's colonies or ports.

## Trade

Any cards for one card of a chosen resource from the bank, at 5:1, one less for each road network and
each port (never below 1:1). If another player owns that resource's Trading Company, the cards go to
them and the card comes from its stockpile.

## Royal Seals

- **Founder's Oath Society**: first to found 2 colonies after the starting one. Choose either die at
  every harvest. Stolen by an enemy occupying all your colonies.
- **Maritime Exchange Guild**: first to build 2 ports. Ports keep their bonus while blockaded. Stolen by
  an enemy blockading all your ports.
- **Stonewall Society**: first to build 2 forts. Repair a damaged fort at once, on another player's turn
  as your Counter-Action. Stolen when all your forts are destroyed.
- **Crossroads Guild**: first to complete 2 road networks. Revive with any resource cards. Stolen when all
  your road networks are cut off.
- **Trading Companies** (Golden Grain, Neptune's Bounty, Colonial Equine, Northern Axe, Crown & Chisel,
  Iron Mark): founded with 5 matching cards (the stockpile) by a player owning or annexing a colony on
  that terrain (a port, for Fish). Bought out by paying more matching cards than the stockpile. A company
  passes to a player who annexes all its owner's matching colonies or blockades all its matching ports,
  at the start of that player's turn.
- **Military Badges** (Royal Musket, Mounted Vanguard, Black Powder Volley, Naval Supremacy): the most
  captures by musketeers, cavalry, cannons or frigates; ties keep the holder. Musketeers attack while
  injured; cavalry move 3; cannons attack twice a turn; frigates are revived at once.

## Interpretations

Where the second draft is silent or unclear, this is what the site does (to be checked with the designer):

1. **Boar's War penalty**: the draft names it but doesn't say what it is; no penalty is applied yet, and
   the log says so.
2. **Harvest**: only the six hexes around a colony count, not its own hex.
3. **"2 of Either"** (musketeer): any two cards that are each Wheat or Fish, mixed or not.
4. **Maneuver cards aren't resources**: they can't pay costs or trades.
5. **The bank** gives a resource card of the asked type from the deck (or its discard pile), never the
   Maneuver card.
6. **Roads** join two neighboring land hexes (drawn on the edge between them); a road network is a pair
   of the player's colonies joined by its roads and bridges, so three colonies in a row are two networks.
   A road starts at the player's colony or the end of its road or bridge. Units move along their own
   networks only, from colony to colony in one step.
7. **Bridges** span one water hex in a straight line between two land hexes; every player's land units
   may cross them.
8. **Units of players at peace** don't share hexes.
9. **Ambush** is offered when the player is asked for a Counter-Action, in addition to it.
10. **Retreat and Fortify** are offered only to a player holding the card, so asking shows they hold it.
11. **Counter-Actions** may target any player the defender is at war with.
12. **Stonewall Society** is stolen by the player who removed the holder's last fort; **Crossroads Guild**
    by the single player annexing the holder's colonies when its networks are all cut.
13. **Naval Supremacy**: a frigate hit on another player's turn may be revived at once (paying); on its
    owner's turn it may be revived the turn it was injured.
14. **Buyout**: the buyer's cards become the new stockpile and the old stockpile goes to the old owner's hand.
15. **Surrender**: the player's units, roads, bridges, ports and forts leave the board, its annexed colonies
    become the annexer's, and its seals go back to the box (a company's stockpile to the discards).
16. **Gifts and peace terms** are free: the site enforces only giving cards and returning captured units.
