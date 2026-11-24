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

A Musketeer costs two Wheat, two Fish, or one of each (owner, 2026-11-24). The mod's Player Reference card shows only
"Wheat, Wheat or Fish, Fish", but the mixed payment is intended: don't change the game to match the card.

## Units

Each unit has one action a turn: move, attack, or be revived. New and revived units do nothing until
their owner's next turn. A move is made one hex at a time (one movement point each); a unit finishes its
action before another acts. **One unit a hex**, except a frigate's passenger: units pass through their
own player's units but never stop on them, and never pass through other players'.

- **Musketeer**: move 1 or attack 1; attacks and captures land units only.
- **Cavalry**: move up to 2, or move 1 and then attack 1 (also after capturing with that move).
- **Cannon**: move 1 or attack up to 2; attacks land units, frigates and forts; captures land units only.
- **Frigate**: move up to 2 water hexes or attack up to 2; attacks frigates, land units and forts;
  captures frigates only; carries one land unit.
- Land units never enter water or mountains (a mountain only with Hideout, or one with their own colony).
  Cannons and frigates don't shoot over mountains.
- A road network lets a land unit move from one of its player's colonies to a colony it joins for one
  movement point; a bridge joins its two land hexes for one point. Neither lets a unit attack across it.
- **Frigates**: a land unit embarks by moving onto its own frigate, which ends its action; it disembarks
  on a later turn. A passenger can't attack or be attacked, and is captured with its frigate.

## Combat

- Only players at war attack each other. An attack injures the target (drawn on its side). A unit moving
  into a hex where every enemy unit is injured captures them, as part of the move, which goes on;
  captured units go to the capturer, counted for the unit type that captured them. Entering another
  player's colony ends the move.
- Injured units can't attack (musketeers can with the Royal Musket Badge) but can move and capture.
  They are revived on their owner's turn, never the turn they were injured.
- **Counter-Action**: after a unit's action that attacked or captured (forts included; moving into an empty
  colony doesn't count), each player hit may move or attack with one unit not used that turn, once for
  that unit's action however many attacks or captures it made. A counter-attack (or capture) can only hit
  the player whose turn it is. A Counter-Action gets no Counter-Action back.
- **Forts**: an undamaged fort keeps enemies from attacking or entering its colony. Cannons and frigates
  damage it (drawn upside down); then only cannons and frigates can attack the units inside, and once
  they're all injured, enemy land units enter, capture them and annex the colony.
- **Annexation**: an enemy land unit in a colony annexes it while it stays, and so does another player's
  fort around it. The annexing player collects its harvest; its roads and port give no trade bonus or
  movement. At the start of a turn, a player's unit standing in a colony removes another player's damaged
  fort there; the annexing player may build their own fort from the following turn. A fort is repaired
  only while no other player's unit stands in it. A player whose colonies are all annexed may surrender;
  an annexing player with too few colony pieces left picks which of them to replace.

## Maneuver cards

Played from the hand, then discarded:

- **Ambush** (Lumber): during another player's turn, a unit in a forest moves or attacks.
- **Outflank** (Stone): a unit on a stone hex gets one more move or attack, both before any Counter-Action.
- **Broadside** (Fish): the same for a frigate.
- **Retreat** (Wheat): when a unit on a wheat field is attacked, it moves to an empty hex within its
  movement range instead of being hit.

Maneuver cards may be played without being at war. Hands are hidden, so every attack on a wheat field or
hills asks the defender, whether or not they hold Retreat or Fortify.
- **Fortify** (Mustang): when a unit on hills is attacked, the attack is deflected.
- **Hideout** (Iron): a unit next to a mountain enters it.

## War and peace

A player declares war by giving a Declare War card (three each): a **Gentleman's War** lets the declarer
attack from their next turn (the other player may attack at once); a **Boar's War** attacks at once, with
a penalty. The declarer can't offer peace until their next turn. Peace needs both: one offers terms on
their turn, the other accepts or declines. Terms may give and ask for cards and return captured units;
cards change hands at once. A prisoner exchange (units returned both ways) sends them to their owners'
supplies; units returned one way may be placed in one of their owner's empty colonies (or ports, for
frigates). Other promises are only words. **Cards change hands only in peace terms and trading company
trades.**

## Trade

Any cards for one card of a chosen resource from the bank, at 5:1, one less for each road network and
each port (never below 1:1). If another player owns that resource's Trading Company, the cards (at the
player's own rate) go to them and the top card of its stockpile to the player.

A road or bridge is **cut off** while another player has a land unit at each of its two ends: it then joins
nothing, for networks, trade, recruiting cavalry and movement. A colony can't be founded on a hex with a
road or bridge at its edge. Roads and bridges continue a path from the player's colony.

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
  that terrain (a port, for Fish). The resource's Maneuver card counts as a matching card. On their turn
  the owner may put matching cards on the stockpile or take its top card. Bought out by paying more
  matching cards than the stockpile. A company passes to a player who annexes all its owner's matching
  colonies or blockades all its matching ports, at the start of that player's turn.
- **Military Badges** (Royal Musket, Mounted Vanguard, Black Powder Volley, Naval Supremacy): the most
  captures by musketeers, cavalry, cannons or frigates; ties keep the holder. Musketeers attack while
  injured; cavalry move 3 (or 2 and attack); cannons attack twice a turn; frigates are revived at once,
  which isn't a Counter-Action, so the frigate may still counter-attack.

## Rulings

The answers to the playtest questions of 2026-11-23 are in the rules above, the ones marked unofficial
too, and the two given as opinions (returned prisoners, Mounted Vanguard capturing during its move).

## Interpretations

Where the draft and the rulings are silent, this is what the site does (to be checked with the designer):

1. **Boar's War penalty**: the draft names it but doesn't say what it is; no penalty is applied yet, and
   the log says so.
2. **Harvest**: only the six hexes around a colony count, not its own hex.
3. **"2 of Either"** (musketeer): any two cards that are each Wheat or Fish, mixed or not.
4. **Maneuver cards aren't resources**: they can't pay costs or trades (they can go on a company stockpile).
5. **The bank** gives a resource card of the asked type from the deck (or its discard pile), never the
   Maneuver card.
6. **Roads** join two neighboring land hexes (drawn on the edge between them); a road network is a pair
   of the player's colonies joined by its roads and bridges without passing through a third colony, so
   three colonies in a row are two networks, and moving from the first to the third takes two points.
7. **Bridges** span one water hex in a straight line between two land hexes; every player's land units
   may cross them.
8. **Ambush** is offered when the player is asked for a Counter-Action, in addition to it.
9. **Stonewall Society** is stolen by the player who removed the holder's last fort; **Crossroads Guild**
   by the single player whose cuts and annexations left the holder with no network.
10. **Naval Supremacy**: a frigate hit on another player's turn may be revived at once (paying); on its
    owner's turn it may be revived the turn it was injured.
11. **Buyout**: the buyer's cards become the new stockpile and the old stockpile goes to the old owner's hand.
12. **Surrender**: the player's units, roads, bridges, ports and forts leave the board, its annexed colonies
    become the annexers', the ones not replaced are removed, and its seals go back to the box (a company's
    stockpile to the discards).
13. **Outflank and Broadside**: after the unit's action, it gets a whole second one (a cavalry may move and
    attack again).
14. **Peace terms** may ask for up to 10 cards; a player who can't pay them can only decline.
