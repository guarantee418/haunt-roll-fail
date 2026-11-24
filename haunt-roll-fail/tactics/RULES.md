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
teams allowed): the Allies (US Forces, British Forces, Soviet Forces) against the Axis (Wehrmacht, Oberkommando West), so the
setup refuses three players or more on one side (`validateFactionCombination` in `meta.scala`). Two players are two teams as
before, any two factions, and play as they did (`teamGame` in `game.scala` is false, every helper below gives the old answer).

- The team of the first seat is team 1 (the first Spawn Area); teammates share their team's Spawn Area (`teams`, `team`, `mates`,
  `enemies`, `foe`).
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
from the core six). So far the US Forces' nine, from the Faction Guide's pages 11 and 12, the Wehrmacht's eight, the British Forces' eight and the Soviet Forces' twelve (Elite Commanders Collection: Light Cavalry,
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
    Sandbags), Terrain Pack 3's foot bridges, Lost Brigades, Bunkers and the terrain packs' other special Buildings.
13. **Commander readings**: Osttruppen's two icons beside the Wrench (a veteran star and a Speed Die) are read as the squad's Speed Die;
    the Paradrop and Airdropped Group costs are the whole cost of the units; Smoke "disrupting Pins" isn't modeled beyond the sight rules;
    Combat Group Advance's and Armor Blitzkrieg's free move lasts the turn the ability is used; a drawn Commander's cards are shown to
    both players.
14. **Board differences**: the Faction Guide's Building Board pages give the Brummbar 6 health for 2 Manpower 5 Fuel; the mod's board
    (played here) 5 health for 1 Manpower 5 Fuel.
