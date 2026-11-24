# Company of Heroes (2nd Edition) — rules as played here

Company of Heroes: The Board Game, 2nd Edition (Bad Crow Games, licensed from SEGA and Relic Entertainment) is listed
on the site as **Company of Heroes** (meta `tactics`, URL `/play/tactics`) while the owner's team helps test it. It plays the
core rulebook (RulesPal's copy, and the four books in the Tabletop Simulator mod "Company of Heroes - 2nd Edition", Steam
workshop 2314664989: Rulebook, Faction Guide, Mission Booklet, Solo Rulebook), US Forces against the Wehrmacht on
**Moro River**, one player each. The setup's **Rules** choice picks the **Full Game Rules** (every new game since
2026-11-24, `FullGameRules`) or the **First Game Rules** (`FirstGameRules`; games made before the choice have neither and
play these). The First Game Rules are below; the Full Game Rules add to them (**Full Game Rules**, further down).
With the Full Game Rules come the twelve core Commanders (**Commanders**, further down). Where the rules leave something open, the choice made is under
**Interpretations**.

## Setup

- Map: Moro River (MR1 and MR2 side by side, the Mission Booklet's 2-panel core mission). Twelve Control Points, where the
  booklet puts them, all dual: six Victory Point ones (two with Manpower, two with Munitions, two with Fuel) and two each
  of Manpower and Fuel, Fuel and Munitions, Munitions and Manpower (`Board.data`).
- The Spawn Areas are the back rows: the first player in the seating (team 1) at the top, the second at the bottom.
- Income 1 Manpower, 1 Munitions, 1 Fuel; stockpile 4 Manpower, 2 Munitions, 3 Fuel; 0 VP and XP.
- Game length (setup option): Short 18 VP (default, the rulebook's suggestion for a first game), Medium 21, Long 24.
- Each team rolls three Damage Dice; the most green symbols, then black, goes first (a tie rolls again). The team going
  second places its three starting Infantry first: any of its Tier 1 Infantry (US: Riflemen, Mortar Team; Wehrmacht:
  Grenadiers, MG42 Team), each in an empty hex of its Spawn Area.

## A round

1. **Maneuver**: the first team alternates every round. Four turns each, alternating, starting with the first team; each
   turn 3 CP, spent on any units, unused CP forfeited. One CP: move one hex, set up or pack up a weapon team, or build
   Sandbags (a unit with a Wrench, not in a Building). A unit takes at most 3 CP a round (2 with Slow). A Speed Die is a
   free CP, used before any CP on that unit. One unit a hex; units move through others (allied or enemy) but not end
   there, and a move through costs both CP at once. No moving into water hexes or across red lines; vehicles don't enter
   Buildings or Destroyed Buildings. Weapon teams move only packed up and fire only set up (Mortars and the AT Gun not in
   Buildings). Infantry boards its own transport (Jeep, Kubelwagen) next to it for 1 CP and leaves to a hex next to it
   for 1 CP; the vehicle carries it; regular Infantry fires from inside, nobody can target it, and it is destroyed with
   the vehicle.
2. **Damage**: set-up machine guns pick their targets first and pin the Infantry they hit (not in Buildings or
   transports, not set-up machine guns). Then units on the other team's Control Points neutralize them (not pinned
   Infantry). Then every unit able to fire (not pinned) picks one target in its range and the team's sight, in turn order,
   and puts all its damage on it. Each player gains 1 XP, plus 1 for each enemy unit it put relevant damage on. The HE
   dice are rolled (red miss, black hit, green crit for 2), then each target rolls its Defense Dice together: one for each
   die it resists (Defense Matrix), Building 3, Destroyed Building 1, or the best Cover line crossed (light 1, heavy 2)
   for Infantry, and its Defense upgrades. Each green or black die saves 1 damage; Flame can't be saved. Destroyed units
   give 1 VP to the other team and 1 XP to their owner. Two or more damage dice on a hex destroy its Sandbags.
3. **Supply**: neutralize again (pins from destroyed machine guns are gone), then Infantry (not pinned), and the Jeep and
   Kubelwagen, capture the neutral Control Points they stand on. Collect income. A player at the game's VP ends it:
   3 more VP to the team holding the most Victory Point Control Points (none on a tie), the most VP wins, and on a tie
   the team that went first in the first round. Otherwise the teams alternate purchases, the round's first team first,
   one at a time until both pass: spawn a unit (an empty hex in the Spawn Area, Infantry also into an empty transport
   there), an upgrade (XP or Munitions, each once, Panzer Grenadiers' AP twice), or the next Tier Building. Then pins
   end, Infantry in the Spawn Area, next to an ally that Reinforces or carried by one heals 1 for each, and each Wrench
   repairs an adjacent allied vehicle by 1.

## Full Game Rules

What the rulebook's grey boxes add (`full` in `game.scala`), and Moro River's own features from the Mission Booklet:

- **Operations deck** (4.0.1): the 14 cards of the mod (seven designs: 2/3, 2/4 twice, 2/5, 3/4 three times, 4/3 three times,
  3/5 twice, 4/5 twice; `Operations`) are shuffled at the start. Each round the first team draws the top card and picks its
  top (2 to 4 turns) or bottom (3 to 5) Operation; the other team learns only which side (the log and the status pane say
  "top (2-4)" or "bottom (3-5)") until the turns are done, when the card is revealed and discarded. An empty deck is
  reshuffled from the discards. Each team then has that many turns of 3 CP.
- **Retreat** (4.2): on its own turn, at any time and whatever CP it spent, an Infantry unit (not in a transport, not
  assaulting) may Retreat: free, up to six hexes toward a Retreat Point the player picks: a hex of the team's Spawn Area,
  or a non-Building hex next to the Major (the Retreat Point icon on its board). A set-up Weapon Team packs up for free.
  After the turns, each team in turn order has its **final Retreats**: it may start more, and its Retreating units not
  yet on a Retreat Point go on (those it doesn't move go toward the nearest one). A Retreating unit spends no CP and
  can't attack, spot, neutralize, capture, build, reinforce or be reinforced; it keeps its Cover. The status ends at the
  end of the Supply Phase (before Reinforcing) for units on a Retreat Point.
- **Assault** (6.0): any unit may move (1 CP, normal movement) into an adjacent hex held by an enemy unit where no other
  Assault is; both stand in the hex. The assaulting unit spends no more CP (its passenger may still leave). The Assault
  ends when the assaulted unit leaves the hex (the assaulting unit holds it) or in the Damage Phase's last step: if the
  assaulted unit was destroyed and the assaulting one survived, it holds the hex; if both survived, the assaulting unit
  falls back to the nearest empty hex toward a Retreat Point (Infantry Retreating until the round ends). In the hex the two
  may attack each other (only units that fire at range 1), with no Cover and no Frontal Armor; the assaulting Infantry
  gets none of the hex's Cover; other attacks on the assaulted unit trigger Cover as usual. No neutralization happens in
  the Damage Phase on a hex with an Assault. A Heavy Vehicle assaulting a hex with Sandbags destroys them only when it
  wins it. Infantry assaulting into a Building can be pinned.
- **Overwatch** (4.1.4): enemy Infantry moving onto a hex in a set-up machine gun's range, line of sight and Firing Arc
  takes 1 Damage at once, nothing saved (2 from two machine guns, ...); each hex moved onto counts, also a hex passed
  through and the hex an Infantry leaves a transport to. Not for Retreats, boarding a transport, or entering a Building
  (unless assaulting a machine gun there: only that one fires). A Camouflaged unit must be spotted (an enemy next to it,
  or a "spots Camouflage" unit in sight). A unit destroyed this way gives the machine gun's team 1 VP, and its owner no XP.
- **Firing Arcs and facing** (4.1.3, 4.1.7): a Weapon Team setting up picks one of the six hex sides to face, and fires
  only into its arc; packing up drops it. Heavy Vehicles always face a hex side (picked when spawned): each CP moves one
  forward or backward, turning one side before or after, or turns one side in place; through an occupied hex it goes
  straight on to the next for 2 CP. Fixed guns (straight arrows on the board) fire only into the arc: the Panzer IV's AI
  (and its AI upgrade), the Brummbar's HE; the others are turreted. A Heavy Vehicle may put each damage type on a target of
  its own (5.0.2).
- **Frontal Armor** (5.7): a Heavy Vehicle hit by AP from a unit in its frontal arc (not from its own hex) rolls 2 more
  Defense Dice, once.
- **HE Splash** (5.3.1): the Major's HE (always), the US Mortar's XP upgrade (1 XP) and the Brummbar's XP upgrade's other
  choice turn a unit's HE into HE Splash: it targets a hex (in range, sight and arc, with a visible enemy in or around it)
  and hits every unit in it and the six hexes around it that the attacker's team sees, its own units always, and the
  Buildings there. All the splash dice are rolled with the other HE dice, before them, and each die's result hits every
  unit. Splash doesn't trigger non-Building Cover, and a team's own splash doesn't trigger Building Cover for its units.
  Each visible enemy unit hit gives 1 XP, as damage assigned.
- **Buildings** (5.6.1): a Building has 2 health. HE (its result) and Flame assigned to Infantry inside also damage it, and
  units may fire HE and Flame at a visible Building with no visible enemy (and none of their own units) in it. A destroyed
  Building is a Destroyed Building for the rest of the game (darkened on the map); the units in it retreat to the nearest
  empty hex at the end of the Damage Phase, Infantry Retreating until the round ends. Destroyed Buildings can't be
  destroyed.
- **Tank traps**: Moro River's A and B hexes (the "T" hexes of `Board.data`) are permanent tank traps: Vehicles can't
  enter them; they can't be destroyed or assaulted.

## Commanders

With the Full Game Rules, the setup's **Commanders** choice (`DrawCommanders`, every new game since 2026-11-24, or `NoCommanders`)
plays rulebook 8.0 and 8.2 with the twelve core Commander cards (six a faction, read off the 2nd Edition Faction Guide's pages 11
and 17; `commanders.scala`):

- At setup each player draws three of their faction's six (a shuffle each). A player takes one when unlocking its first tier and keeps
  it. Tiers are unlocked left to right in the purchases (each a purchase, alternating as usual), for the XP printed at their top left,
  some only after a Tier Building. With all four, 2 XP trade for 1 Manpower, Munitions or Fuel any number of times (not a purchase).
- "One spawns for free" spawns that unit right after the tier is unlocked (in the Spawn Area; a Paratrooper on a visible hex), if there
  is room; the free Paratrooper or Airdropped Group counts toward its once-a-round limit.
- Call-ins: the M7 Priest (2 health, range 4, two indirect HE, fixed gun; 1 Manpower 3 Fuel), the Sherman Easy Eight (4 health, range 3,
  three AI and two AP, turreted; 3 Manpower 4 Fuel; a Sherman for the limit of 2), the Tiger (6 health, range 2, three AP and an AI,
  turreted, a Defense Die; 3 Manpower 5 Fuel), Assault Engineers (Riflemen with 2 Close Range Firepower, AI at range 1 only, and two
  Wrenches; the Riflemen's upgrades), Osttruppen (3 health, range 2, AI, a Wrench and a Speed Die; two squads for 4 Manpower 1 Munitions),
  Stormtroopers (4 health, range 2, AI and a Close Range Firepower; 5 Manpower 1 Munitions; next to an empty Building, Slow the first
  round; one a round).
- Emplacements (Festung Armor Doctrine): Grenadiers, Osttruppen and Panzer Grenadiers build MG Bunkers (2 health, range 2, AI, a machine
  gun: pins and Overwatch; 2 Munitions) and Pak 43s (3 health, range 4, three AP; 4 Manpower) in the purchases, one a Wrench a Supply
  Phase, not the round they spawned, on an adjacent empty open hex. They never move, set up with a facing like Weapon Teams, and capture
  from the next round.
- Opening abilities, at the start of a player's Maneuver Phase (their first turn, before any CP), each once a round: Recon Sweep (a hex
  and the hexes around it visible to the team this round, Camouflage too), Panzerwerfer Light Barrage (1 HE on a hex), P-47 Rocket Strike
  (2 AP on each of 3 hexes in a line), Fragmentation Bomb (2 HE on each of 3), Combat Group Advance and Armor Blitzkrieg (every Infantry,
  or Vehicle, may move once for free that turn). Strikes hit in the Damage Phase every unit in those hexes, allies too, and Buildings; they
  give no XP, need no sight and trigger no Cover but a Building's, nor Frontal Armor.
- Smoke (Defensive Smoke, Smoke Canisters): at the start of the Damage Phase, in turn order, each vehicle may pay 2 Munitions to Smoke its
  hex for the round: a unit in Smoke sees and attacks only next to it and is attacked only from next to it, and no one sees or fires (but
  indirect fire) past a smoked hex.
- Unit abilities: Defensive Stance (Riflemen, 1 CP: Heavy Cover this round even when assaulted, no more CP, moves or Retreats), Hull Down
  (1 CP: an adjacent vehicle gets 2 Defense Dice until it moves), Raid Point (after an Infantry's first CP, 1 Munitions for a free move),
  Armor Tactician (Infantry next to a Heavy Vehicle gets 2 Defense Dice as Cover, not in a Building).
- Upgrades: the free ones (Veteran Greyhounds, Wolverines, Ostwinds, Panzer Grenadiers, West Point Reserves, MG42 Armored Cars, Elite AT
  Crew's +1 Health and Speed Die) are on every such unit, present and future, and aren't offered for sale; the added ones (Camouflage,
  Recon in Force, Mechanized Infantry, G43 Rifles) once a unit; Ranger and .50 cal Jeep Munitions upgrades cost 1 less, Incendiary
  Mortars' Flame 1 XP; the LMGs add +1 Range to the Riflemen's AI and the Grenadiers' HE Munitions upgrades; Ranger Riflemen take 4 CP a
  round; Elite Sturmpanzers have no Slow; German Industry takes 2 Fuel off the Panzer Korps; US Breakthrough (1 Fuel) takes 1 Manpower off
  Infantry for the Supply Phase; West Point Reserves spawns a free Major in the purchases whenever there is none.
- Paratroopers (Riflemen, 5 Manpower 1 Munitions), Paradrop Equipment (a Weapon Team, 1 Manpower 5 Munitions) and Airdropped Group (two
  Weapon Teams on adjacent hexes, 3 Manpower 6 Munitions) spawn on any visible open empty hex, Double Slow the first round (1 CP), one of
  each a round.

## Units

From the Building Boards in the mod (`UnitType` in `game.scala`; the boards are in the game's "Building Boards" overlay):

| US Forces | Tier | | Wehrmacht | Tier |
|---|---|---|---|---|
| Riflemen, Mortar Team, Willys Jeep | 1 | | Grenadiers, MG42 Team, Kubelwagen | 1 |
| .50 Cal MG Team, M1 57mm AT Gun, M8 Greyhound | 2 (1 MP 2 Fuel) | | Panzer Grenadiers, Mortar Team, SdKfz 222 | 2 (1 MP 2 Fuel) |
| Major (one at a time), M4A3 Sherman, M10 Wolverine | 3 (1 MP 3 Fuel) | | Ostwind, Panzer IV, Brummbar | 3 (1 MP 3 Fuel) |

Vehicles: 3 of each tier 1 and 2 vehicle, 2 of each tier 3 one, at a time.

## Interpretations

1. **Grid**: the panels show 11 full rows between a half row at the top and one at the bottom, and every other row has
   half hexes at both ends; all of them are played. The Spawn Areas are the half rows (the back rows).
2. **Lines**: a Cover line drawn inside a hex covers that hex; Infantry gets it when the line from the attacker's hex
   center to its own crosses that edge (both edges when it passes a corner). A red line on either side of an edge
   closes it both ways. Water hexes ringed in red can't be entered; the river elsewhere (unringed) can.
3. **Line of sight**: Buildings and Destroyed Buildings between two hexes block sight and direct fire (a line along an
   edge is not blocked); indirect HE fires over them but still needs sight from some unit.
4. **Sandbags**: one Sandbags token covers two neighboring edges of its hex (Heavy Cover), the player picks which; a
   Heavy Vehicle moving in destroys them.
5. **Repair** picks the most damaged adjacent allied vehicle; Reinforce stacks as the rulebook says.
6. **First Game Rules**: the Major's attack is plain indirect HE, the Mortar's XP Splash upgrade isn't offered and the
   Brummbar's XP upgrade is only +1 HE.
7. **Firing Arc shape**: the rulebook says a cone from the hex in front; its Heavy Vehicle picture draws the front arc's
   edge through the middle of the two sides next to the front one, so the arc is 120 degrees wide: the hexes within 60
   degrees either side of the faced side, those on the edge included (at distance 1 the three hexes in front). The same
   arc is used for Weapon Teams, fixed guns, machine gun Overwatch and Frontal Armor.
8. **Retreat paths**: the player picks the Retreat Point; the unit goes along a shortest way to it (the first in reading
   order where there are several) and stops in the last empty hex of its six. The final Retreats move a Retreating unit
   the player doesn't move toward the nearest Retreat Point. A unit may Retreat on its turn only while its team has CP left
   that turn (the turn ends by itself at 0 CP); the final Retreats come after every turn anyway.
9. **Forced Retreats** (a failed Assault, a destroyed Building) go to the nearest empty hex, the one nearest a Retreat Point
   among them; a unit with nowhere to go is destroyed.
10. **Overwatch** counts each hex moved onto, the hex passed through by a 2 CP move too (it is moved onto).
11. **Splash on Sandbags**: each splash die counts once for each unit it hits in the hex and once for the hex toward the
    two Damage that destroy Sandbags.
12. **Not in the core box** (left out): barbed wire and tank traps that units build (the core units' Wrenches build only
    Sandbags), Terrain Pack 3's foot bridges, Lost Brigades, Bunkers and other special Buildings, Ice, Control Points in
    Buildings (Moro River has none).
13. **Commander readings**: Osttruppen's two icons beside the Wrench (a veteran star and a Speed Die) are read as the squad's Speed Die;
    the Paradrop and Airdropped Group costs are the whole cost of the units; Smoke "disrupting Pins" isn't modeled beyond the sight rules;
    Combat Group Advance's and Armor Blitzkrieg's free move lasts the turn the ability is used; a drawn Commander's cards are shown to
    both players.
14. **Board differences**: the Faction Guide's Building Board pages give the Brummbar 6 health for 2 Manpower 5 Fuel; the mod's board
    (played here) 5 health for 1 Manpower 5 Fuel.
