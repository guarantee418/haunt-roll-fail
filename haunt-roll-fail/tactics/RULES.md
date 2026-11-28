# Company of Heroes (2nd Edition) — rules as played here

Company of Heroes: The Board Game, 2nd Edition (Bad Crow Games, licensed from SEGA and Relic Entertainment) is listed
on the site as **Company of Heroes** (meta `tactics`, URL `/play/tactics`) while the owner's team helps test it. It plays the
core rulebook (RulesPal's copy, and the four books in the Tabletop Simulator mod "Company of Heroes - 2nd Edition", Steam
workshop 2314664989: Rulebook, Faction Guide, Mission Booklet, Solo Rulebook), two of the five core factions (US Forces,
Wehrmacht, British Forces, Soviet Forces, Oberkommando West: **Factions**, further down) on
the Mission Booklet's six two-panel core missions (**Missions**, further down; Moro River by default), one player each, or
three to five players in teams (**Team games**, further down). The setup's **Rules** choice picks the **Full Game Rules** (every new game since
2026-11-24, `FullGameRules`) or the **First Game Rules** (`FirstGameRules`; games made before the choice have neither and
play these). The First Game Rules are below; the Full Game Rules add to them (**Full Game Rules**, further down).
With the Full Game Rules come the twelve core Commanders (**Commanders**, further down). Where the rules leave something open, the choice made is under
**Interpretations**.

## Setup

- Map: the setup's **Map** choice, one of the Mission Booklet's 2-panel core missions (**Missions**); games made before the
  choice play Moro River (MR1 and MR2 side by side). Twelve Control Points, where the booklet puts them, all dual: on Moro
  River six Victory Point ones (two with Manpower, two with Munitions, two with Fuel) and two each of Manpower and Fuel, Fuel
  and Munitions, Munitions and Manpower (`Board.data`).
- The Spawn Areas are the back rows: the first player in the seating (team 1) at the top, the second at the bottom (on
  Across the Don and Road to Moscow, the left and right edges).
- Income 1 Manpower, 1 Munitions, 1 Fuel; stockpile 4 Manpower, 2 Munitions, 3 Fuel (the mission's own on the others); 0 VP and XP.
- Game length (setup option): Short 18 VP (default, the rulebook's suggestion for a first game), Medium 21, Long 24.
- Each team rolls three Damage Dice; the most green symbols, then black, goes first (a tie rolls again). The team going
  second places its three starting Infantry first: any of its Tier 1 Infantry (US: Riflemen, Mortar Team; Wehrmacht:
  Grenadiers, MG42 Team), each in an empty hex of its Spawn Area.

## Team games

Since 2026-11-24, three to five players play in two teams on any mission but the free-for-all (rulebook p. 6, Team Game End Game Conditions, and 11.0, uneven
teams allowed): the Allies (US Forces, British Forces, Soviet Forces) against the Axis (Wehrmacht, Oberkommando West, Deutsches Afrikakorps), so the
setup refuses three players or more on one side (`validateFactionCombination` in `meta.scala`). Two players are two teams as
before, any two factions, and play as they did (`teamGame` in `game.scala` is false, every helper below gives the old answer).

- The team of the first seat is team 1 (the first Spawn Area); teammates share their team's Spawn Area (`teams`, `team`, `mates`,
  `enemies`, `foe`).
- **3v3** (2026-11-27, the owner's ask): six players, three a side, on a map of three panels or more: Wehrmacht, OKW and the
  Desert Warfare expansion's DAK against the three Allies, or with a faction taking a second seat ("Wehrmacht
  II", `SecondFaction` in `game.scala`), as the rulebook lets the same faction play twice in a team game: it plays its first's boards,
  units, Commanders, Lost Brigades and art, and its units are marked "II" on the map, with its own health badge colour. A second seat
  needs its first in the game and three players or more (`validateFactionCombination`). The rules are the team rules below: shared
  VPs, 2 starting Infantry each, alternating turns and purchases in seating order. The rulebook's team note doesn't cover 3v3, so
  these are readings to check against Desert Warfare's 6-player rules once its files are in.
- Teammates share VPs: a team's VPs are its players' together (`teamVP`), and the game ends at 3 more than the game length, 21,
  24 or 27 (`endVP`, p. 6). The 3 bonus VPs for the most Victory Point Control Points go to the team holding the most; a tie in
  VPs goes to the team that went first in the game. A unit destroyed gives its VP to the other team's first player (shared
  anyway).
- Each player starts with **2** Infantry, the rulebook's note for a team game on a smaller than recommended map (p. 7: 2 vs 2
  is for 3-panel maps, the missions here are 2-panel). One player of each team rolls for first (the teams' first players); the team going
  second places all its starting Infantry first, in seating order.
- Maneuver: the teams alternate turns; in a team's turn each player spends their own 3 CP, one after another in seating order
  (the rulebook has them play at the same time). The Operations card is drawn and picked by the first team's first player,
  and gives every player that many turns.
- Purchases: the teams alternate each purchase; teammates, simultaneous in the rulebook, take their purchases in turn, in the
  order A1, B1, A2, B2... (`interleaved`), skipping those who passed.
- Teammates are not enemies: they don't neutralize each other's Control Points, Assault each other or take each other's HE
  Splash as enemies (an ally in the splash is hit as one's own unit), and a team sees what any of its units sees, Recon Sweeps
  and spotting Camouflage too (`visible`, `seesHex`, `spottedIn`).
- Reinforce and Wrench repairs help teammates' units too (rulebook p. 25-26: "all sources of this ability may also benefit your
  teammates"). Not done: Commander abilities that help "allied" units still help only the player's own.

- Teammates (the rulebook's Commander FAQ 4): Repair (Wrenches), Reinforce, Retreat Points and sight always benefit teammates;
  other Commander abilities affect only their own player's units. Sight, Reinforce and Repair were shared from the start; a
  teammate's Retreat Point (a Major, a Transport that is one) is one for the whole team in games with the hidden
  `TeamRetreatPoints` every new game gets (2026-11-25; `retreatPoints` in `game.scala`), so team games made before keep their
  retreats.

### Free-for-all

The Mission Booklet's 8.0, **Moro River, 3 Way Free-for-all** (`MoroRiverFFA` in `missions.scala`, the Map choice
`MoroRiverFFAMap`), takes exactly three players, any factions, each a team of their own (`freeForAll`; rulebook 11.0):

- MR1 and MR2 with Moro River's terrain, cover and tank traps, and the mission's own twelve Control Points (from the mod's bag
  "6VP 3FFA Moro River", the same as the booklet's picture; six with Victory Points). Starting stockpile 6 Manpower, 4 Munitions,
  2 Fuel (the booklet; the bag's name says 4/2/3, Moro River's); the game ends at **15** VP, whatever the Game length (`trigger`).
- Three Spawn Areas of four hexes, read off the booklet's picture: the first seat's at the top left, the second's at the top
  right, the third's at the bottom middle (`areas`). Three starting Infantry each.
- Each player rolls three Damage Dice; the best goes first, a tie for the best rolls again. Turns go round in turn order, and the
  first player passes to the next each round. Starting Infantry are placed in the reverse order.
- A destroyed unit gives 1 VP to every player who assigned it Damage that Damage Phase (direct, HE Splash, strikes; 11.0), or who
  hit it with Overwatch. A unit destroyed with no Damage assigned to it (a passenger left with nowhere to go, a failed Assault with
  nowhere to fall back) gives none.
- Purchases go round one at a time in turn order. The 3 bonus VPs go to the player holding the most Victory Point Control Points.

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
- **Barbed wire and tank traps** (4.1.2, Additional Battlefield Defenses; 2026-11-25): a unit with a Wrench, not in a Building,
  not assaulting or assaulted, builds Barbed Wire or a Tank Trap on its own hex for 1 CP (both at once with two Wrenches;
  `defenseChoices`, `DefensesAction`). Barbed Wire keeps every Infantry out of its hex and Emplacements off it, whoever built it;
  a Heavy Vehicle moving in crushes it, and Light Vehicles ignore it. A Tank Trap keeps every Vehicle out. Units already there may
  leave but not come back. An Assault may still go into an enemy-held hex behind them (`canAssault`). Like Sandbags they have 2 flat
  health: two damage assigned to the hex destroys every Battlefield Defense there (`destroyDefenses`), and a unit in range may fire
  at an empty defended hex as at an empty Building, with any of its damage dice (`shotChoices`). They are drawn as a coil of wire
  across the hex and steel hedgehogs.
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

## Missions

The setup's **Map** choice (`MapChoice` in `game.scala`, the maps in `missions.scala`; every new game since 2026-11-24 has one,
Moro River by default) plays one of the Mission Booklet's six 2-panel core missions (4.0), each two of the four core panels side by
side as the mission's bag in the Tabletop Simulator mod lays them, with its stockpile and Control Points (the resource tokens in
its bag, which match the booklet's pictures):

| Mission | Panels | Stockpile (Manpower, Munitions, Fuel) | Spawn Areas |
|---|---|---|---|
| Moro River | MR1, MR2 | 4, 2, 3 | top and bottom |
| Across the Don | MR1, MR2 | 5, 3, 3 | left and right edges |
| Between Two Rivers | MR2, MR1 | 4, 4, 4 | top and bottom |
| Ardennes Forest | AF1, AF2 | 6, 2, 3 | top and bottom |
| Frozen Lake | AF2, AF1 | 6, 4, 3 | top and bottom |
| Road to Moscow | AF1, AF2 | 6, 4, 4 | left and right edges |

Four more use only the core panels (2026-11-25; `tools/missions-panels.py`): the 3.0 Skirmish Core Missions on one panel, two
players with 2 starting Infantry each and the game ending at 11 VP, and the 5.1 3 Panel Core (x2) Missions on three panels (two core
sets), for team games, every player starting with 2 Infantry. The mod has no bags for them, so their Control Points were read off the
booklet's pictures (pages 6 and 18), each token's centre mapped to its hex:

| Mission | Panels | Stockpile | Spawn Areas |
|---|---|---|---|
| The River (skirmish) | MR1 | 4, 2, 2 | top and bottom |
| Frozen Pond (skirmish) | AF2 | 4, 2, 4 | top and bottom |
| Between Two Rivers (3 panels) | MR2, MR1, MR2 | 4, 2, 6 | left and right edges |
| Ardennes Forest (3 panels) | AF2, AF1, AF2 | 1, 2, 3 | top and bottom |

Ten of the booklet's 4.1 2-panel LMP missions are on the Large Map Pack panels Hill 331 (HL1, HL2), Trois Ponts (TP1, TP2) and
Stalingrad (SG1, SG2) (2026-11-25; `tools/missions-lmp.py`). Their panels and Control Points come from the mod's mission bags; the
Buildings, Destroyed Buildings, Water (ringed in red), Ice and the Hill 331 Bunker were read by hand off the panels; the Spawn Areas are
the half hexes of the top and bottom rows under the booklet's spawn boxes, or the left and right edges:

| Mission | Panels | Stockpile | Spawn Areas |
|---|---|---|---|
| Hill 331 | HL1, HL2 | 6, 4, 2 | left and right edges |
| Ploiesti Outskirts | HL2, HL1 | 4, 4, 4 | top and bottom, middle |
| Verrieres Ridge | HL1, HL2 | 4, 2, 4 | top and bottom |
| Trois Ponts | TP1, TP2 | 4, 2, 3 | top left, bottom right |
| Caen | TP2, TP1 | 2, 6, 0 | top and bottom, middle |
| Carentan | TP1, TP2 | 6, 4, 4 | left and right edges |
| Stalingrad | SG1, SG2 | 2, 4, 0 | top left, bottom right |
| Bulge | SG2, SG1 | 4, 1, 6 | top right, bottom left |
| Leningrad | SG1, SG2 | 4, 6, 1 | left and right edges |
| Mongolian Train Station | SG2, SG1 | 3, 2, 2 | top middle, both bottom corners |

- **Bunker** (Full Game Rules 10.1): the Hill 331 pillbox is a Special Building with 4 health; Infantry inside gets an AP die
  attacking units outside it (`Board.bunkers`, "K" in the rows; `attacksOf`, `diceOnFull`).
- **Optional Modular Bunkers** (the A and B hexes some missions mark) are left out; Ploiesti's A hexes, multi-hex Buildings the panels
  don't print, are Buildings.
- **Ice on Stalingrad's panels**: the hexes mostly ice (the rule's fallback); the panels' snowflake icons sit on hex edges, and agree.
- **Stockpiles** are the booklet's; Trois Ponts' bag in the mod says 4/4/3, the booklet 4/2/3.

The rest of the booklet's LMP missions that have a bag in the mod are in too (2026-11-25; `tools/missions-bags.py`): the 4.1 missions
on the Monastery panels (MO1, MO2: the airstrip, the cross-shaped church, a multi-hex Building, and the ruins), the 5.0 and 5.2
3-panel missions, the 6.0 and 6.1 4-panel missions in a row, and the 8.1 and 8.2 free-for-alls. Each is laid as its bag lays it, a
panel with "r" being turned round (the booklet's arrow); the Control Points are the bag's tokens; the Spawn Areas and the Buildings,
Bunkers and Cathedrals the booklet adds come from its pictures (pages 11 to 25 and 30):

| Mission | Panels | Stockpile | Spawn Areas |
|---|---|---|---|
| Cathedral | MO1, MO2 | 4, 4, 2 | top and bottom; a Bunker (A, B) between the churches |
| Cologne | MO1, TP2r | 4, 4, 6 | top and bottom, left half |
| Desert Oasis | HL1, TP1 | 6, 2, 2 | top left, bottom right; the Hill 331 Bunker |
| No Man's Land | HL2, MO2r | 2, 5, 0 | top and bottom |
| Moscow Outskirts | SG1r, AF1 | 5, 4, 4 | top left, bottom right |
| Opposing Faiths | MO2, MO1 | 7, 4, 1 | top and bottom; the A and B hexes a Cathedral |
| Semois | MR2, MO1 | 5, 3, 4 | top right; Team 2 the bottom left corner |
| Hinderdam | MO2, MR1 | 4, 4, 3 | top left, bottom right |
| Ancient Fields | MO2, HL1, HL2 | 2, 6, 2 | top right, bottom left; a Bunker and two Buildings added |
| Airfield | TP1, TP2, MO2 | 4, 3, 3 | top left, bottom right; a Building added (A) |
| Hilltop Monastery | HL2, MO1, MO2 | 6, 6, 4 | top and bottom |
| Budapest | MR1, MR2, MO1 | 4, 2, 3 | top right, bottom left |
| Siege of Leningrad | SG2r, SG1r, AF1 | 6, 4, 0 | top middle; Team 2 both bottom corners |
| Opposing Faiths (3 panels) | MO1, MO2, MO1 | 6, 4, 2 | top and bottom; a Bunker (A, B) in the middle |
| The Wide Front | HL2, HL1, HL2 | 4, 6, 4 | top and bottom, middle |
| Trois Ponts (3 panels) | TP1, TP2, TP1 | 6, 6, 2 | both ends of the top and bottom rows |
| Two Islands | SG1, SG2, SG1 | 3, 2, 2 | top left, bottom right |
| Cologne (4 panels) | TP1, TP2, MO1r, MO2r | 4, 4, 6 | top right, bottom left |
| Trois Ponts Monastery | TP1, TP2, MO1r, MO2r | 6, 6, 4 | two parts of the top and of the bottom row |
| Hill 331 Monastery | HL1, HL2, MO1, MO2 | 3, 2, 1 | top and bottom, middle |
| Vengeance | MO2r, MO1r, MO1, MO2 | 6, 4, 4 | Team 1 top and bottom, Team 2 left and right edges |
| Opposing Faiths (4 panels) | MO1, MO2, MO2r, MO1r | 4, 2, 2 | top right, bottom left |
| Two Hilltops | HL1, HL2, HL2r, HL1r | 4, 4, 6 | Team 1 left edge and top, Team 2 right edge and bottom |
| Frozen Fork | SG2, SG1, SG2, SG1 | 3, 3, 3 | two parts of the top and of the bottom row |
| Six Bridges | TP2r, TP1r, TP1, TP2 | 5, 4, 3 | top and bottom, middle |
| Hill 331, 3 Way Free-for-all | HL1, HL2 | 6, 2, 4 | top middle, bottom right, bottom left; ends at 15 VP |
| Cologne, 4 Way Free-for-all | TP1, TP2, MO1r | 6, 4, 4 | the four corners; ends at 15 VP |

- **The mod's bags against the booklet**: three bags lay the booklet's map turned round (Cathedral, Cologne, Opposing Faiths), which
  is built the booklet's way; three put a panel where its hexes don't line up with the next one's, where the booklet's printed compass
  marks show otherwise: Hilltop Monastery has HL2 on the left (the bag HL1), The Wide Front's first HL2 isn't turned, and Opposing
  Faiths on 4 panels starts MO1, MO2 (the bag MO2, MO1). Their tokens stay where the bags put them.
- **Stockpiles** are the booklet's where the bags' names differ (Cathedral, Cologne, Desert Oasis, No Man's Land, Opposing Faiths, Semois,
  Ancient Fields, Airfield, Budapest, Cologne on 4 panels, Hill 331 Monastery, Hill 331 3 Way).
- **Hill 331, 3 Way**: the Bunker is a Destroyed Building ("A Hex"), and the three hexes the booklet rings below it Buildings.
- **Free-for-alls**: every player is on their own, as on Moro River's 3 Way (11.0); the map takes its number of players.
- **Monastery's church** is one multi-hex Building (5 hexes); the small roofless sheds around it are cover, not Buildings. Opposing
  Faiths' Cathedral is the "TP2 Cathedral / Tile" the booklet places over the round tower and four hexes beside it.
- A Hill 331 Bunker that a layout cuts in half at the map's edge (No Man's Land, Hilltop Monastery, The Wide Front) stays a Bunker.

The rest of the booklet is in too (2026-11-25; `tools/missions-grid.py`): the missions the mod has no bag for, whose Control Points
were read off the booklet's pictures (`tools/booklet-tokens.py` finds each token by its colour and groups them by hex; each map was then
checked against its picture and a few tokens corrected by hand, `FIX`), and the 7.0 square 4-panel missions and 8.3 and 8.4 5 Way
free-for-alls. The booklet draws the square maps with their panels upright, two rows of two (the mod's table turns them a quarter
round), so they keep the pointy-topped hexes: they are maps of two rows (`MapData.rows`), the hexes past column Z named AA, AB, ....
The square maps' bags put their tokens elsewhere than the booklet, whose pictures are followed.

| Mission | Panels | Stockpile | Spawn Areas |
|---|---|---|---|
| A River Too Far | MO2, MR1, MR2 | 1, 2, 5 | both top corners; bottom middle |
| Race Across the Rhine | HL2, MR1, MR2 | 7, 1, 0 | left and right edges; 2 starting Infantry each |
| Frozen River | SG1, AF2, AF1 | 4, 4, 4 | top left, bottom right |
| Frozen River (4 panels) | SG2, SG1, AF2, AF1 | 4, 4, 4 | two parts of the top and of the bottom row |
| Mud | HL2, MR1, MR2, HL1 | 6, 2, 4 | both ends of the top and bottom rows |
| Winter Solstice | SG1, SG2, AF1, AF2 | 3, 5, 1 | top left, bottom right |
| Summer Solstice | MO2, MR1, MR2, MO1 | 2, 3, 4 | top and bottom |
| Lorraine | MR1, MR2 / HL1, HL2 | 5, 4, 3 | left and right edges |
| Rails and Metal | AF2, AF1 / SG1r, SG2r | 4, 4, 4 | left and right edges |
| St. Hilaire | TP1r, TP2r / MO1, MO2 | 4, 4, 4 | upper left edge, lower right edge |
| Winter Storm | SG1, SG2 / AF1, AF2 | 6, 4, 2 | left and right edges |
| The Volga | TP1, TP2 / MR1, MR2 (the lower row 5.5 hexes right) | 6, 4, 2 | left and right edges of both rows |
| Race to Rome | MR1, MR2 / MO1, MO2 | 6, 4, 2 | upper left and right edges |
| St. Marie du Mont | MO2r, TP1 / TP2, MO1r | 6, 4, 3 | upper left and right edges |
| Cologne, 5 Way Free-for-all | TP1r, TP2r / MO1, MO2 | 5, 4, 3 | five numbered areas; ends at 15 VP |
| Hill 331, 5 Way Free-for-all | HL1, HL2 / HL1, HL2 | 3, 4, 5 | five numbered areas; ends at 15 VP |

- **Optional Modular Bunkers** (the booklet's A and B hexes, "B hexes are not illustrated"): the setup toggle "Optional Modular
  Bunkers" (`OptionalMapFeatures`) makes them Bunkers (Special Buildings, 4 health, Infantry inside gets an AP die attacking out) on
  the missions that mark them (`MapData.optional`), and puts the Hilltop Monastery's optional Victory Point in its tower; off, the map
  is as printed. A Control Point on a Bunker hex stays.
- **The Volga**: the lower row starts where its hexes line up with the upper row's (the booklet's picture puts it 160 of 380 pixels in).
- **Frozen River (4 panels)**: the booklet notes 7 Victory Points on Ice.

The 3-panel Between Two Rivers' badges mark the first full column and the half hexes beside it at each edge (`MapData.edge` 1.1).
The 3-panel missions may be played by two players too, each with 2 starting Infantry.

- **Side Spawn Areas**: the booklet's numbered badges mark the full hexes of the edge column; the half hexes cut by the edge
  beside them are taken too (`MapData.spawn`). Team 1 (the first player in the seating) is on the left.
- **Control Points in Buildings**: Across the Don and Road to Moscow have two each (Road to Moscow one in the Cathedral);
  Infantry holds them as on open ground, Vehicles can't enter them.
- **Moro River's bridge point**: the Control Point beside the bridge was a hex off (I4); new games have it in I5, where the booklet
  and the mod put it (`MoroRiver`); games made before the Map choice keep I4 (`MoroRiverFirst`).
- **Ardennes Forest panels** (AF1, AF2; Full Game Rules 10.2, 10.4): **Ice** tiles on the frozen lake: a unit attacked on Ice
  rolls Damage Dice by the Ice Matrix (the most for any damage type assigned to it: AI none; AP 1; HE 2 on a Heavy Vehicle,
  else 1; Flame none on Infantry, 1 on an Emplacement, else 2), and falls through, destroyed, if any die misses; units on Ice
  count as on open ground. **Multi-hex Buildings**: adjacent Buildings are one Building: units inside see and fire across it,
  and get no Building Cover against units in the same Building. The **Cathedral** (five hexes) is an indestructible
  multi-hex Building giving Infantry 3 Defense Dice, as a Building does. Bunkers are in the terrain packs, not the core
  missions, and aren't played. The Ardennes panels have no tank traps.

## Factions

Any two different factions may play (the rulebook lets even the same faction play in team games). Each faction's units are its three
Building Boards in the mod (`game.scala`, from "BRITISH HEADQUARTERS 1" on; `tactics-art.py` cuts the tokens), unlocked as before for
1 Manpower 2 Fuel and 1 Manpower 3 Fuel (the boards' backs, the same for every faction). Their Commanders are under **Commanders**.
The Faction Guide's own copies of the British boards (its page 21) differ from the mod's in a few upgrades, the mod's being played, as
with the Brummbar: the Mortar Emplacement there has a base Defense Die and only Camouflage for Munitions, the Bofors an XP +1 Sight and
Range and a Munitions +1 AI, the Churchill only the Defense XP upgrade (no Remove Slow).

- **Board icons** read as on the US and Wehrmacht boards; new ones: an eye is +1 Sight, an eye in a crosshair +1 Sight and +1
  Range (the Infantry Section), a tile of two soldiers two AI dice in one upgrade (the Centaur), Remove Slow, Heroic Sacrifice (the
  T-34's star: no VP for the enemy when it is destroyed, its owner still gains the XP), and the health upgrades, which add the health
  at once (the Faction Guide says so of the SU-76's).
- **British**: the Infantry Section builds the Mortar and Bofors Emplacements of the Platoon Command Post in the Supply Phase, paying
  their board cost, one per Wrench a round, on an empty open hex next to it, not the round it spawned (the British Operating Manual on
  the board's back); they aren't spawned. The Universal Carrier's two Munitions upgrades are a Flame die each; the first ends its
  Transport and Capture, and its passenger falls back to the nearest empty hex toward a Retreat Point. The Centaur's **Strafing Fire**:
  once its dice are on a target, the same dice may also go to one more enemy it could fire them at, next to that target, with full
  damage to each. The Churchill is Slow.
- **Soviet**: Conscripts have a Speed Die and a Wrench; the Sniper Squad fires only at range 3 (two gray range circles), and its
  Camouflage is its XP upgrade, as the board shows (the Faction Guide says it spawns with it); the **T-70** toggles a No-Fire Capture
  mode for 1 CP (it captures and can't fire, until switched back); the SU-85 is Slow.
- **OKW**: the SWS Supply Truck converts into the Tier 2 (OKW Battlegroup) and Tier 3 (Schwerer Panzer HQ) Buildings: unlocking a tier
  is a conversion of a truck on the map (not Retreating), paying the tier's cost; the truck becomes the Emplacement with its board's
  stats and full health, and its passenger falls back as above. The Battlegroup (3 health, no attack) reinforces and spots
  Camouflage, the Schwerer Panzer HQ (3 health) fires 1 AP at range 4. One of each at a time; a destroyed one is rebuilt by converting
  another truck for the same cost, and the tier stays unlocked. Tier 2 and 3 units may spawn next to their Tier's Building, with Slow
  the first round (Double Slow if already Slow). Sturmpioneers have a Close Range Firepower die and two Wrenches, and build Flak 37
  Emplacements (2 health, range 3, 2 AI) for 4 Munitions, up to two a round; Obersoldaten are Camouflaged; the Stuka Halftrack sets
  up and has a Firing Arc like a Weapon Team; the Raketenwerfer may set up in Buildings; the Sturmtiger's HE is HE Splash and it is Slow.
- **Left out**: the anti-aircraft symbols and the "treat AP (HE) as AI against aircraft" notes (no aircraft in the core rules played
  here).
- **Deutsches Afrikakorps** (DAK, 2026-11-27, the Desert Warfare expansion's Axis faction): its three Building Boards are the pictures in
  Bad Crow's "3rd printing balance changes" sheet, a Google Doc the owner shared (not in the mod; `tactics-art.py factions <dir> <out> dak`
  cuts them saved as `dak-1.jpg` ... in `<dir>`). DAK Headquarters: Assault Grenadiers (Speed Die; two XP upgrades of +1 Close Range
  Firepower each, read off the soldier-with-lightning icon), Panzerjaeger (a Wrench), Flakvierling (a Light Vehicle that pins like a
  machine gun and sets up with a Firing Arc like a Weapon Team). DAK Mechanized Kompanie: the **SdKfz 250 Half-Track** (no attack,
  Transport, Capture, Reinforce; Infantry spawned into it on the round it spawned cost 2 Manpower less; its Munitions upgrade makes it
  the Mortar Halftrack: 2 indirect HE, range 2 to 4, sets up with a Firing Arc, no more Transport, its passenger falling back as from the
  Universal Carrier), the StuG III D (HE Splash, fixed HE and AI), the 8-Rad (a No-Fire Capture mode like the T-70). DAK Panzerarmee
  Kommand: Panzer III (turreted, Heroic Sacrifice for XP, two +1 AI), the 88mm FLAK 36 (a Light Vehicle that sets up like a Weapon Team,
  Slow), the **Command Tiger** (one at a time; its XP upgrade lets the DAK, once a round in its Maneuver Phase, put the command marker on one
  of its Vehicles, which captures that round: the board's "gain Capture for next round" read as the round it is placed). Its Commanders
  (8 core, 4 Desert Warfare) and Lost Brigades are on the same sheet and not in yet: a DAK player draws none.

## Commanders

With the Full Game Rules, the setup's **Commanders** choice (`DrawCommanders`, every new game since 2026-11-24, or `NoCommanders`)
plays rulebook 8.0 and 8.2 with the thirty core Commander cards (six a faction, read off the 2nd Edition Faction Guide's pages 11, 17,
22, 28 and 35, with its Commander FAQ, pages 2 to 6; `commanders.scala`):

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

- **British, Soviet and OKW Commanders** (2026-11-24) reuse the rules above where their cards do (Covert Training is Ambush Training,
  Elite Artillery Tactics a Recon Sweep, IL-2 a P-47 Strike, Breakthrough Tactics and Grand Assault the free moves, Smoke Ambush the
  Smoke, Hit the Dirt Defensive Stance for Conscripts, Recon Mode a Raid Point for Vehicles at 2 Munitions). New:
  - Call-ins, read off the cards: Staghound (Light Vehicle, 4 health, range 3, 2 AP; 2 Manpower 3 Fuel), .55 cal Armor Piercing Sniper (3
    health, range 3 only, 2 AI and an AP, +1 Sight, Camouflaged; 3 Manpower 2 Munitions), Churchill AVRE (6 health, range 2, Slow, a
    Defense Die, 2 turreted HE and a forward AI pair with a third HE, its HE HE Splash; 3 Manpower 5 Fuel; a Churchill for the limit),
    Royal Engineers (Infantry Sections with 2 Close Range Firepower instead of their AI, 4 health, 2 Wrenches, the Section's upgrades;
    they build the British Emplacements too), DShK 38 HMG (4 health, range 3, 2 AI, a machine gun, +1 Sight; 4 Manpower 1 Munitions),
    T-34-85 (4 health, range 3, AI and 2 AP turreted; 3 Manpower 4 Fuel; a T-34 for the limit), Panzerfusiliers (4 health, range 2-3, 2 AP,
    a Speed Die; 4 Manpower 1 Munitions), Jaeger Recon Squad (4 health, range 2, AI, Camouflage, +2 Sight and spots Camouflage; 3
    Manpower), Puma (Light Vehicle, 3 health, range 3, 2 AP and an AI; 2 Manpower 3 Fuel), Kettenkrad (Light Vehicle, 1 health, 2 Speed
    Dice, Camouflage, Heroic Sacrifice, captures, no attack; 2 Manpower 1 Fuel), StG44 Rifle Volksgrenadiers (2 AI, no Defense Die).
  - Emplacements built like the British ones, one per Wrench a round: the Forward Emplacement (3 health, Reinforce, Retreat Point, Spots
    Camouflage, Infantry may spawn next to it with Slow; 2 Manpower), the 17 Pounder (4 health, range 4, 4 AP, a Defense Die; 5
    Manpower), the Soviet Forward HQ (an empty Building next to Conscripts becomes an Emplacement, 3 health, Reinforce, Retreat Point,
    Infantry spawn next to it with Slow, one at a time; 2 Manpower), the LeFH 18 (3 health, range 5, 2 indirect HE; 4 Munitions; the
    Veteran one +1 HE), the OKW's Veteran Pak 43 (3 health, range 4, 3 AP and +1; 4 Manpower). A free Emplacement goes next to one of its
    builders and counts as that builder's for the round.
  - Special spawns: Supply Recon (a Scout Car for 3 Munitions), NKVD (two Conscript squads for 5 Munitions), Infiltration Commandos (an
    Infantry Section next to an empty Building, Slow the first round, 5 Manpower 1 Munitions, one a round), Commando Glider (a Forward
    Emplacement on a visible open hex and a Tier the faction has Infantry next to it, Slow the first round, 5 Manpower 3 Munitions, one
    a round). Thorough Salvage trades 2 Manpower for 1 Fuel or 2 Munitions in the purchases.
  - In play: Advanced Emplacements (once the Damage is assigned, each targeted Emplacement may pay 1 Munitions for +1 Defense Die), Smoke
    Emplacement (Emplacements Smoke for 1 Munitions), Vehicle Crew Repairs (Vehicles Smoke for 3 Munitions and Repair 1 in the Supply
    Phase), Emergency War Speed (after a Vehicle's first CP, 1 Health for a free move; not at 1 Health), Hold the Line (Infantry within 1
    hex of a Control Point its team holds has 2 Defense Dice as Heavy Cover, also Assaulting or Assaulted, not in a Building), Operation
    Crocodile (the Churchill's AI becomes 2 forward Flame and it loses Slow; its AI upgrade is no longer offered), Ramming Maneuver (an
    Assaulting T-34 or T-34-85 adds 2 AP on the unit it Assaults), Assault Tactics (the Scout Car's No-Fire mode, 1 CP; on a Control Point
    it gives 1 more of the point's first resource other than VP, a reading of "the player chooses"), Rapid Conscription (2 CP 1 Munitions
    at the start: 1 Manpower for each Damage the faction's Infantry takes that round, Overwatch too), Fortifications Doctrine (1 CP 1
    Munitions: the faction's Emplacements are Camouflaged that round), Breakthrough Doctrine (Heroic Sacrifice for 3 Munitions on Infantry,
    +1 Defense Die Assaulting), Conscript Support (new Conscripts spawn with both +1 Health upgrades).
  - Free and cheaper upgrades as on the cards (Veteran Staghounds, Cromwells, SU-76s, SU-85s, Mortars, T-34 Wave, T-70 Infantry Tank, T-80,
    Heavy and Scout Luchs, Rocket Support, Anti-Tank Ambush, Bolster Infantry Squads, WASP, Ranged Tactics, Panther and Sturmtiger
    Reinforcements), and the added ones (Thompson SMGs, Infantry Support's Reinforce, Flammenwerfer, HEAT Shells). A Commander's +1 Health
    adds the health at once.
  - "Requires and spawns at Tier N": the OKW units spawn next to that Tier's Building or in the Spawn Area, as their tier's units do.

### Expansion Commanders

The setup's **Expansion Commanders** toggle (`ExpansionCommanders`, only with Draw three Commanders; 2026-11-24) adds the expansion
packs' Commanders to the ones each player draws three from (`Commander.pool`; games without it, and every game made before it, draw
from the core six). So far the US Forces' nine, from the Faction Guide's pages 11 and 12, the Wehrmacht's eight, the British Forces' eight, the Soviet Forces' twelve and the OKW's seven (44 in all) (Elite Commanders Collection: Light Cavalry,
Heavy Cavalry, Tactical Support, Motor Pool; Pathfinder Pack: Pathfinder Company; StuG Pack: Devil's Brigade; Lost Brigades Pack: Cavalry
Rifle, Urban Assault, Engineer Assault Companies). Their units were read off the cards' call-in panels (health, range, a die for each
figure, turreted for curved arrows): M5 Stuart (heavy vehicle, health 3, range 2, AI and AP, two Speed Dice, 2 Manpower 3 Fuel), M26
Pershing (6, range 2, 2 AP 2 AI, 3/5), T34 Calliope (4, range 4, 2 indirect HE with Splash, 2/5), Sherman 105mm Dozer (6, range 2, 2 AI
2 HE, a Defense Die, Sherman chassis, 3/5), M36 Jackson (3, range 4, 3 AP, a Speed Die, Wolverine chassis, 3/5), I.R. Pathfinder Squad
(infantry, 3, range 3, 1 AI, spots Camouflage, Camouflage, 3 Manpower), Cavalry Riflemen (5, range 2, 1 AI, Close Range Firepower, a
Defense Die, 4 Manpower 1 Munitions), Devil's Brigade (Riflemen with 5 health) and the US MG Bunker (the Wehrmacht's, built by Riflemen).
The Wehrmacht's eight (pages 17 and 18; StuG Pack: StuG Assault Doctrine, StuG Tactics; Elite Commanders Collection: Terror, Tank
Destroyer, Assault Support Doctrines; Lost Brigades Pack: Infantry Storm, Festung Support, Lightning War Doctrines) bring the StuG III G
(heavy vehicle, health 3, range 3, a fixed AI and AP, 2/3), King Tiger (8, range 3, Slow, 2 fixed AI and 2 AP, a Defense Die, 4/5), Jaeger
Sniper (infantry, 3, range 3 only, 2 AI, a Speed Die, Sight 3 spotting Camouflage, Camouflage, 3 Manpower 2 Munitions), Jagdpanther (5,
range 4, 3 fixed AP, a Defense Die, 3/4), Elefant (7, range 4, Slow, 3 fixed AP, 3/5) and the SdKfz 251 Half-track (light vehicle, 3,
range 3, 1 AI, a Transport that Reinforces and is a Retreat Point; Infantry may spawn next to it, Slow the first round; 2/3).
Readings:

- The StuG Pack's two Commanders are also the OKW's (its page): they are drawn by the Wehrmacht here, and will be by the OKW too.
- Tank Destroyer Doctrine marks an enemy Vehicle on the map (its hex in the opening), which then rolls no Defense Dice that round, not even
  for Frontal Armor or the damage types it resists.
- Assault Support Doctrine gives the Kubelwagen the Scout Car's No-Fire collection (1 more of the point's first non-VP resource).
- Breakthrough Tactics (Terror Doctrine) is the US Step on it; Tactical Movement is Combat Group Advance; the Stuka strafe is the P-47
  strike's 2 AP a hex; Recon Plane is Recon Sweep.
- The British Forces' eight (pages 23 and 24; Elite Commanders Collection: Hammer Doctrine, Royal Artillery Regiment, Royal Armored Corps,
  Tank Hunter Regiment; StuG Pack: Special Service Brigade; Lost Brigades Pack: Assault Infantry, Assault Officer, Vanguard Operations
  Regiments) bring the Sexton Gun Carriage (heavy vehicle, health 2, range 3, 2 fixed indirect HE with Splash, 1/4), Comet (6, range 2, 2 AP
  1 AI, a Speed Die, 3/5), Sherman Firefly (4, range 4, 3 AP, 2/5), Valentine Mk. XI (3, range 3, 2 fixed AI and 1 AP, a Defense Die
  Assaulting, 2/3) and Commandos (infantry, 3, range 2, 1 AI, Close Range Firepower, dropped like Paratroopers, 5 Manpower 1 Munitions).
  The Firefly's Tulip and the Commandos' Gammon Bombs are other target choices: 3 HE in place of the unit's whole attack (Tulip paying 2
  Munitions, Gammon only at an adjacent target). English Industry is a trade of 2 Munitions for 1 Fuel in the purchases, as often as wanted.
- The Soviet Forces' twelve (pages 29 and 30; Elite Commanders Collection: Frontal Assault Doctrine, Guards Motor, Urban Defense, Urban
  Assault Tactics; Eastern Reinforcements Pack: Counterattack Tactics, Heroes of Stalingrad; Lost Brigades Pack: Siege Militia, Frontline
  Tactics; StuG Pack: Soviet Spetsnaz Brigade, Terror, Shock Rifle and Soviet Industry Tactics) bring Shock Troops (infantry, 5, range 2, 1
  AI, Close Range Firepower, a Defense Die, 4/1), the IS-2 (7, range 2, 1 AI 2 AP, 3/5), Guards Rifle Infantry (4, range 3 only, 2 AI or 2
  AP, 4/1), Anti-Infantry and Anti-Tank Partisans (2, range 2, 1 AI or 1 AP, Heroic Sacrifice, spawned next to an empty Building like
  Stormtroopers, Slow, 1 Manpower 4 Munitions, one a round), the Forward Station (an Emplacement Conscripts make of an empty Building next
  to them, 3 health, a Wrench; Vehicles may spawn next to it; one at a time), the KV-2 (5, range 3, 2 indirect HE with Splash, 2/5), the
  Penal Battalion (5, a Defense Die, 2 AI at range 2 or 2 HE at range 1, never Pinned, 4/1), the ISU-152 (7, range 4, Slow, 3 fixed AP or
  3 HE, 3/5) and the KV-8 (5, range 3, Slow, 3 fixed AI and a Flame, 1 more Flame Assaulting or at a unit in a Building; 3 Manpower 2
  Fuel as printed).
  - "Picks either damage type each attack" is another target choice; it uses up the unit's whole attack, so it is offered only before the
    unit has assigned any damage.
  - The IS-2's shells: its AP damage at Infantry in a Building also hurts the Building, and the Infantry get 1 less Defense Die there.
  - Soviet Architecture: +1 Defense Die for the Soviets' Infantry in any Building (Assaulting into one too), and 1 less damage to a Building
    with their Infantry in it.
  - Decoy: on the third CP of a round, a Sniper or Guards Rifle Infantry moves 3 hexes and can't fire that round.
  - Battlefield Promotions: the Conscripts become a new unit of the other type in the same hex, 2 more health (5 at most), keeping their
    Flame upgrade; it is a purchase.
  - Siege Militia's free Partisans and the free Anti-Tank Partisans count toward their one a round.
- The OKW's seven (pages 36 and 37; Elite Commanders Collection: Terror Doctrine West, Tank Destroyer West; StuG Pack: StuG Assault
  Doctrine and StuG Tactics, the Wehrmacht's cards in the OKW's colors; Lost Brigades Pack: Storm, Encirclement and Joint Operations
  Doctrines) bring the Flammpanzer 38 Hetzer (heavy vehicle, 3, range 3, a Flame and 3 fixed AI, a Defense Die, 3 Manpower 2 Fuel; Assaulting,
  its damage may also go to a second target next to the first, like the Centaur's Strafing Fire), and the Wehrmacht's StuG III G, King
  Tiger, Jagdpanther and Stormtroopers as OKW units (their types shared, `UnitType.shared`, with OKW tokens); OKW units of Tier 2 and 3 spawn
  next to their Tier Buildings as the cards say ("Requires and spawns at Tier 2/3"). Ober Defense is bought with Advanced Emplacements'
  brace step, up to 4 Munitions; Tank Desant Tactics' free Infantry is a Volksgrenadier squad for the OKW; Veteran Kettenkrad is +1 health
  and Camouflage.
- A panel naming a call-in with no "one spawns for free" brings no free unit (`...Plain` abilities).
- Strategic Maneuvers is the Scout Car's No-Fire mode for Stuarts; Combined Arms counts a unit next to another of the same player's;
  Requisition Officer takes 1 Manpower off every Sherman and Wolverine chassis (Easy Eight, Dozer, Jackson too).
- Motor Pool's two Jeeps are one purchase of 5 Munitions, with no limit but the Jeeps' 3; Airdrop Beacon lets Infantry spawn next to a
  Pathfinder Squad as next to a Major (Slow the first round), not Pathfinders; Airdropped Combat Group is three Tier Infantry, any mix,
  next to Pathfinders, Slow (not Double Slow).
- Devil's Brigade: Riflemen for every Riflemen ability; Water is passable for it. Demolition, once a Damage Phase for each Devil's Brigade,
  destroys an adjacent standing Building at its very end (Full Game Rules only), the units inside falling back as from any destroyed Building.
- Cavalry Riflemen's free Assault: once a round, after spending a CP, onto an adjacent enemy-held hex, with no CP.
- The Dozer destroys the Sandbags of the hex it Assaults (the field defenses this game has) at the start of the Damage Phase.
- Urban Assault's Smoke is offered with the other Smoke at the start of the Damage Phase, for Riflemen in an Assault.

### The Deutsches Afrikakorps' Commanders

The DAK's twelve cards (2026-11-27; Bad Crow's "3rd printing balance changes" sheet, cut by `tools/dak-commander-art.py`): eight core,
drawn three at a time like the other factions' six (Armored Warfare Support, Armored Airpower Support, Italian Combined Armor, Italian
Combined Arms, Italian Field Engineers, Defensive Warfare, Desert Skorpion, Italian Coastal), and four Desert Warfare cards, drawn only
with Expansion Commanders (Italian Light Support, Italian Desert Raid, Assault Support Doctrine, Italian Tank Assault). The abilities a
card shares with another faction's (Hull Down, Hold the Line, Step on it, the Nebelwerfer and Stuka strikes, Recon Plane, the vehicles'
free move, the 8-Rad's No-Fire income, the Elefant) play by those rules. The Elefant and the MG Bunker are the Wehrmacht's units, with
DAK tokens. Readings:

- **Units** read off the cards: Kradschützen 2 health, range 2, 1 AI, Capture, 2 Speed Dice (the doubled runner); Semovente 2 health,
  range 3, 2 fixed AP or 2 fixed HE picked each attack; Bersaglieri 3 health, range 3, 2 AI, a Defense Die; Guastatori 4 health, range 2,
  2 AI, 2 Wrenches; Panzerpioneer 5 health, range 3, 1 indirect HE, 2 Wrenches; 105 Howitzer 4 health, range 1 to 5 (only the 5 is
  printed), 3 indirect HE, Slow, sets up like a Weapon Team; 210/22 Artillery Emplacement 4 health, range 4 to 6, 3 indirect HE, 5
  Manpower; L6/40 1 health, range 2, 1 turreted AI, a Speed Die, the T-70's No-Fire Capture mode, two for each bought, six at most;
  Flammpanzer III a Panzer III (its upgrades and limit) with Flame in place of AP.
- **Kradschützen's rider**: a move of a Kradschützen at 2 Health may leave the rider (1 health, Heroic Sacrifice) on the hex the move
  starts from, the Kradschützen losing 1 Health; the rider spots and captures like Infantry but spends no CP and never fires.
- **Bersaglieri**: when its first move of the round starts in its team's Spawn Area or in a Transport, it may then move once more for free.
- **Guastatori**: at Infantry in a Building or at an Emplacement their AI dice are Flame, except in an Assault.
- **Smoke and Cover**: a Guastatori pinned by a machine gun may, when it would pick a target, Smoke its hex instead; the pin is broken
  when the machine gun isn't next to it (Smoke blocks attacks from further away), but the machine gun's damage stands.
- **Battlefield Salvage**: each of your units destroyed gives the XP as usual and one trade, in your purchases, of 1 XP for 1 Manpower,
  Munitions or Fuel.
- **Counter Offensive**: with no Command Tiger in play, one spawns for free in the purchases (like West Point Reserves' Major).
- **Prepared Positions**: Emplacements and Vehicle Weapon Teams (Flakvierling, 88mm FLAK 36, 105 Howitzer, Mortar Halftrack) are
  Camouflaged and have one more Defense Die that round.
- **250/3 Funkpanzerwagen**: Infantry may spawn into an empty SdKfz 250 anywhere (Slow the first round outside the Spawn Area), and
  none gets the 250's Manpower discount; with it, and with Recon Tractor's Spot Camo upgrade, the 250 can't take the Mortar Halftrack.
- **Stuka JU-87 Anti-Tank Loiter** and **Recon Plane**: planes aren't in yet, so they are played as the other factions' strike and
  Recon Plane. Transfer Depots lasts the Supply Phase, as US Breakthrough does.

## Lost Brigades

The rulebook's 9.0 Lost Brigade Building Extensions, the setup's Lost Brigades option (Full Game Rules only; `brigades.scala`, 2026-11-24),
with the Faction Guide's twelve cards of each faction (US 3.3, Wehrmacht 4.3, British 5.3, Soviet 6.3, OKW 7.3) and the universal Panther
Lost Brigade (8.0), a Command Panther of each faction ("Captured" for the Allies), shuffled into each faction's deck (13 cards).

- At setup each player draws three (after the Commanders, when both are played). Once, at any time of the purchases, a player may pick one
  of the three for the XP at its top left, and keeps it. A brigade of 0 XP may be picked at the start, in turn order, before the starting
  Infantry; then the Infantry it names may be among the starting Infantry, up to its number ("one", or Assault and Royal Engineers "up to
  three"). The StG44 Rifle Volksgrenadiers' starting squad is an StG44 squad.
- A brigade's units need no Tier Building and can be bought up to the usual limits. Its unit is bought at the card's price (`price`, over
  the Commander's), and it gives no free unit. OKW brigade units spawn from the Spawn Area only, never next to a Tier Building.
- Each brigade gives the abilities of the Commander tier with the same unit (`abilities`, which `unlocked` checks as for a Commander):
  the call-in, the emplacement a squad builds (the bunkers, Pak 43s, 17 Pounder, Forward Emplacement, Forward HQ, LeFH 18), Paradrop
  Equipment, the Partisans (one of each type a round), the G43 Stormtroopers (one a round).
- Readings:
  - The Repair Station is the Forward Station: Conscripts turn an empty Building next to them into it, it has a Wrench, and Vehicles
    may spawn next to it, Slow, one at a time.
  - Veteran Devil's Brigade: Devil's Brigade for 4 Manpower 4 Munitions, with both their AP upgrades (not offered as upgrades).
  - Veteran Commandos: dropped as with the Commandos Commander, for 5 Manpower 2 Munitions, with Gammon Bombs and its Camouflage.
  - The Churchill Crocodile is a Churchill with Operation Crocodile (2 forward Flame), for 2 Manpower 4 Fuel.
  - The SdKfz 251's configuration is one upgrade, +2 Flame or +2 AP, for 4 Munitions.
  - Elite Grenadier Squad Leaders: 4 health, range 3, 2 AI (the boxed figure read as a second AI die) and 1 HE (the green
    explosion), Camouflage and a Wrench, 4 Manpower 3 Munitions.
  - Command Panther: 5 health, range 3, 2 AP and 2 AI turreted, Spots Camo, 3 Manpower 5 Fuel, one at a time.
- The Deutsches Afrikakorps' seventeen (2026-11-28; the change sheet's "Lost brigades: Dak" cards, its sixteen and a Command Panther):
  105 Howitzer (2 XP), 210/22 Artillery Emplacement (2), Bersaglieri (0, one as starting Infantry), Command Panther (3), Elefant (3),
  Guastatori (0, one), Kradschützen (1), L6/40 (1), Marder III (1), Nebelwerfer (1), Panzerpioneer (0, one), Semovente (1), Veteran G43
  Stormtroopers (3), StuG III G (1), Veteran Coastal Reserves (2), Veteran Flammpanzer III (2), Veteran Tank Busters (2). Readings:
  - The units the DAK Commanders already bring are those units (the cards print the same figures), at the card's price.
  - Marder III: 2 health, range 4, 2 fixed AP, 2 Manpower 3 Fuel. Veteran Tank Busters: 5 health, range 3, 3 AP, a Speed Die,
    4 Manpower 3 Munitions.
  - Nebelwerfer: an Infantry Weapon Team, 4 health, range 2 to 5, 1 indirect Flame and 1 indirect HE (one curved arrow over both), no
    Capture (the red sign), no Setup in Buildings or Transports, 5 Manpower 1 Fuel. "Targets two units adjacent to each other" is the
    Centaur's Strafing Fire: its damage may go, in full, to a second unit next to its target (the sheet's FAQ: each target takes the
    HE, rolled for each).
  - Veteran Coastal Reserves: a unit of its own (a Panzerjaeger with Coastal Reserves' Health, its AP and its Camouflage): 5 health,
    range 3, 1 AI 1 AP, a Wrench, Camouflage, 4 Manpower 3 Munitions; it counts as a Panzerjaeger for the 210/22, Hull Down and
    Coastal Wall.
  - Veteran Flammpanzer III: the Flammpanzer III with Heroic Sacrifice and both AI Munitions upgrades (3 AI turreted, the star and the
    three figures on the card), 3 Manpower 2 Fuel, a Panzer III for the limit.
  - The DAK's Elefant has 4 AP, as both its cards print (the Wehrmacht's has 3): a fourth AP for the DAK's, the Commanders' too.

## 3rd printing

Bad Crow's "3rd printing balance changes" sheet's new rules, each a setup toggle in the "3rd printing" section (off in every older game):

- **Barricades** (`Barricades`, Full Game Rules only; 2026-11-28): Infantry with a Wrench builds one on an edge of its hex for 1 CP (not in
  a Building, a Transport or an Assault). It is barbed wire and Sandbags on that edge: Infantry in the hex gets 2 Defense Dice of Cover
  against attacks across that edge, and no unit moves across it from either hex (the builder neither), a Retreat neither; Assaults go
  across it. Two damage to the hex destroys it with the hex's other Battlefield Defenses, and units may fire at it as at barbed wire.
  Readings: the Cover goes only to the hex it was built in ("to the hex on the side it's placed on"); a Heavy Vehicle can't drive across
  it, an Assault on foot can.
- **Blitz Cubes** (`BlitzCubes`; 2026-11-28): in its team's first turn of each round a player may spend any of the turn's 3 cubes as
  orange cubes: each moves one unit up to 2 hexes (through an occupied hex into an empty one, as a 2 CP move; a Heavy Vehicle in the Full
  Game Rules straight ahead, keeping its facing), Overwatch as usual. It is also a CP on that unit this round. It builds nothing, sets
  up or packs up nothing and pays for no Commander ability. The orange cubes stay on the unit: each takes a CP off what it may spend
  in the later rounds (with 3, none). At the start of the Damage Phase, before pins, each unit with orange cubes may take one off and
  then doesn't attack. Readings: the orange cubes are the turn's own 3 cubes ("instead of the regular CP"), the first turn is each team's
  first turn of the round, and the cubes placed this round don't lower the unit's cap until the next round.
- **Planes and anti-air** (`Planes`, Full Game Rules only; 2026-11-28; the sheet's Planes rules and its eighteen plane cards, three
  a faction: Anti-Tank, Bomber and Recon; the P47 Thunderbolt for the US, the Stuka JU-87 for the three German factions, the IL-2
  for the Soviets and the Hawker Typhoon for the British). A plane (`Aircraft` in `game.scala`, kind `Plane`): 2 health, range 2, 1 AI
  forward, a Speed Die, 2 Fuel, one at a time; the Recon has +2 Sight and Spots Camo. Each player's three cards lie by its Commander
  (the sheet's first way; the other, shuffling them into the Lost Brigade deck, isn't played); once a game, in the purchases, one is
  unlocked for its 2 XP, then bought (spawned in the Spawn Area, facing a hex side) for 2 Fuel.
  - It moves only forward: each CP (or its Speed Die) flies it one hex and turns it one hex side or not, or two hexes straight on, over
    anything (Buildings, water, impassable lines, units, Barricades) onto an empty hex. It never captures, neutralizes, retreats, sets
    up, assaults or is assaulted (the sheet: its "hover" over units isn't played in TTS either; one unit a hex, as there).
  - It fires forward only. The Anti-Tank and the Bomber may pay 3 Munitions for an attack of 2 AP or 2 HE in place of its AI (the
    sheet's rules say "instead of their standard 1 AI damage", the cards "add": the rules are followed).
  - Only AI damages it, and only from an anti-air unit (the sheet's list: the Willys Jeep, M4A3 Sherman, SdKfz 222, Ostwind, M3A1 Scout
    Car, SU-76 with its HE as AI, Bofors, Centaur, Flak 37 emplacement, Schwerer Panzer HQ with its AP as AI, and the DAK's Flakvierling
    and 88mm FLAK 36), in range and seen, in its arc if set up or a fixed gun; a set-up unit must be set up. Splash, strikes and the rest
    do nothing to it (`Matrix`). It rolls a Defense Die for each AI, as a Light Vehicle.
  - Anti-air Overwatch: each hex a plane flies onto in the range of an enemy anti-air unit (one that sets up only set up and in its
    arc; not pinned) deals it 1 damage, once for the hex however many cover it.
  - Readings: the Command Tiger's marker never goes on a plane (it isn't a Vehicle); the 88mm FLAK 36, with only AP, gives Overwatch but
    can't fire at a plane.
- **New Commanders** (`ThirdPrintingCommanders`, with Draw three Commanders; 2026-11-28): the sheet's new core-faction Commanders join each
  player's draw (`Commander.printing`), the expansion ones only with Expansion Commanders too. Played so far:
  - Wehrmacht **Breakthrough Offensive**: Rapid Production (3 XP; a Vehicle spending its first CP of the round to leave a Spawn Area may
    move once more for free, as Bersaglieri; one Kubelwagen free), Panzerwerfer (1 XP, Tier 2: 3 health, range 2 to 4, 2 indirect HE, a
    Speed Die, sets up with a Firing Arc, the second target as the Centaur's Strafing Fire, 1 Manpower 4 Fuel), Tiger Call-In (1 XP, Tier 3,
    no free Tiger), Tiger Ace (5 XP, Tier 3: a Veterancy option, +1 Range for 2 XP, read off the blue target icon; one Tiger free).
  - Soviet **Shock Army**: HM-38 120mm Mortars (3 XP: the Mortar's Splash XP upgrade costs 1 XP; one Mortar spawns with it free), Penal
    Battalions (1 XP, the existing call-in), Katyusha (1 XP, Tier 2: 2 health, range 2 to 5, 1 indirect Flame and 1 indirect HE, a Speed
    Die, Slow, sets up with a Firing Arc, the second target as above, 1 Manpower 4 Fuel), T-34-85 (4 XP, Tier 3, one free).
  - The DAK's versions of the universal German **StuG Assault Doctrine** and **StuG Tactics** (with the DAK's expansion Commanders): as the
    Wehrmacht's, but Tank Desant's free Infantry is an Assault Grenadier, Target Weak Point gives StuG III Ds +1 HE, Turretless Terror gives
    Ds Camouflage too, and the free StuG is a III D or G, the player's pick. "Panzerjaegers can attack with just CRF at Range 1" isn't
    played yet.
  - **Intel Disruption Doctrine** and **Tank Buster Doctrine**, the universal German cards, one each for the Wehrmacht, the OKW and the
    DAK: Intel Disruption is the Recon Plane opening; Marder III Call-In (the DAK's Marder III, with the Wehrmacht's and OKW's tokens);
    Disruption Beacons (an Infantry's first CP of a round and 2 Munitions: Camouflage this round, and it moves no more); Travel Speed
    (Rapid Production; the two free Vehicles up to Tier 2 are picked from the faction's board and the Marder III); the Tank Buster
    Doctrine opening is the Mark Vehicle one; Pact of Steel (Tier 2 Vehicles 1 Manpower less; the free Infantry any Tier 1 Infantry of
    the board, no Weapon Team); Tank Busters (a Munitions upgrade of any Infantry for 3: its attack becomes 3 AP at range 3, its Close
    Range dice gone; Health, Defense and Camouflage kept).
  - US **Special Operations**: Mark Vehicle; SSF Commandos (the Devil's Brigade call-in, whose unit is the one the card describes); M3 Lee
    (3 health, range 3, 2 fixed AI, 1 turreted AP, 2 fixed HE, 1 Manpower 4 Fuel); Assault Operation (a Devil's Brigade buys its Riflemen
    XP and Munitions upgrades for nothing, one option each; one still standing neutralizes in the Supply Phase the Control Point it stands
    on or assaulted this round, even pinned or after a failed Assault, and no enemy captures it there).
  - British **Special Operations**: Mark Vehicle; SSF Canadians (an Infantry Section, with its upgrades, that takes +1 AI or AP for nothing
    once, crosses water and gets 3 Defense Dice in a Destroyed Building, 4 Manpower); M3 Grant (as the Lee with 1 AI and Sight 3); Assault
    Operation (the same, with the Infantry Section's XP Health and either Munitions upgrade).
  - The rest (Field Ordnance, Cavalry Support, Luftwaffe Supply, Reserve Army, Defense in Depth, Atlantic Wall, Rocketry Support, Army in
    Exile, the DAK's Desperate Battle and Last Stand, the Desert Rats', Stuart Honey and Stretch Goal
    Commanders) and the core factions' new Lost Brigades are still to come (`tools/third-printing-art.py` lists them by picture).

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
12. **Not in the core box** (left out): Terrain Pack 3's foot bridges, Bunkers and the terrain packs' other special Buildings.
    Barbed wire and tank traps (2026-11-25): an Assault on an empty hex holding only a Battlefield Defense isn't offered (shoot it
    instead); one CP builds one defense type per Wrench, but Sandbags stay their own 1 CP action.
13. **Commander readings**: Osttruppen's two icons beside the Wrench (a veteran star and a Speed Die) are read as the squad's Speed Die;
    the Paradrop and Airdropped Group costs are the whole cost of the units; Smoke "disrupting Pins" isn't modeled beyond the sight rules;
    Combat Group Advance's and Armor Blitzkrieg's free move lasts the turn the ability is used; a drawn Commander's cards are shown to
    both players.
14. **Board differences**: the Faction Guide's Building Board pages give the Brummbar 6 health for 2 Manpower 5 Fuel; the mod's board
    (played here) 5 health for 1 Manpower 5 Fuel.
