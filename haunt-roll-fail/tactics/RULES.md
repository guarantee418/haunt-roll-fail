# Company of Heroes (2nd Edition) — rules as played here

Company of Heroes: The Board Game, 2nd Edition (Bad Crow Games, licensed from SEGA and Relic Entertainment) is listed
on the site as **Tactics** (meta `tactics`, URL `/play/tactics`) while the owner's team helps test it. This first
version plays the core rulebook's **First Game Rules** (RulesPal's copy of the rulebook, and the four books in the
Tabletop Simulator mod "Company of Heroes - 2nd Edition", Steam workshop 2314664989: Rulebook, Faction Guide, Mission
Booklet, Solo Rulebook), US Forces against the Wehrmacht on **Moro River**, one player each. The Full Game Rules are
not in yet (see HANDOFF.md). Where the rules leave something open, the choice made is under **Interpretations**.

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

## Units

From the Building Boards in the mod (`UnitType` in `game.scala`; the boards are in the game's "Building Boards" overlay):

| US Forces | Tier | | Wehrmacht | Tier |
|---|---|---|---|---|
| Riflemen, Mortar Team, Willys Jeep | 1 | | Grenadiers, MG42 Team, Kubelwagen | 1 |
| .50 Cal MG Team, M1 57mm AT Gun, M8 Greyhound | 2 (1 MP 2 Fuel) | | Panzer Grenadiers, Mortar Team, SdKfz 222 | 2 (1 MP 2 Fuel) |
| Major (one at a time), M4A3 Sherman, M10 Wolverine | 3 (1 MP 3 Fuel) | | Ostwind, Panzer IV, Brummbar | 3 (1 MP 3 Fuel) |

Vehicles: 3 of each tier 1 and 2 vehicle, 2 of each tier 3 one, at a time.

## Interpretations

1. **Grid**: the panels show 13 full rows and a half row at the top and part of a 14th at the bottom; the compass marks
   the bottom row, so the playable rows are the half row through the compass row, and the half hexes at the ends of the
   shifted rows are playable. The Spawn Areas are the first and last of those rows (back rows).
2. **Lines**: a Cover line drawn inside a hex covers that hex; Infantry gets it when the line from the attacker's hex
   center to its own crosses that edge (both edges when it passes a corner). A red line on either side of an edge
   closes it both ways. Water hexes ringed in red can't be entered; the river elsewhere (unringed) can.
3. **Line of sight**: Buildings and Destroyed Buildings between two hexes block sight and direct fire (a line along an
   edge is not blocked); indirect HE fires over them but still needs sight from some unit.
4. **Sandbags**: one Sandbags token covers two neighboring edges of its hex (Heavy Cover), the player picks which; a
   Heavy Vehicle moving in destroys them.
5. **Repair** picks the most damaged adjacent allied vehicle; Reinforce stacks as the rulebook says.
6. **Full Game Rules left out**: firing arcs and facing, Overwatch, Assaults, Retreats, HE Splash (the Major's attack is
   plain indirect HE; the Mortar's XP Splash upgrade isn't offered, and the Brummbar's XP upgrade is only +1 HE),
   Operations deck (always 4 turns), tank traps and foot bridges on Moro River, barbed wire and tank traps, destructible
   Buildings, Commanders, Lost Brigades, Frontal Armor.
