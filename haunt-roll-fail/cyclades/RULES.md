# Cyclades: the rules as played here

Cyclades (Bruno Cathala and Ludovic Maublanc, Matagot, 2009), the **base game**, for 2 to 5 players. The rulebook is the English
one (Matagot's 2009 rulebook, translation by Eric Harlaux); the Creatures' texts are those of the Tabletop Simulator mods'
cards. The rulebook isn't in the repository. This file lists the rules as the site plays them, and every choice made where the
rulebook leaves room (marked **Reading**).

## Setup

- The board: 2 and 3 players play the 2-3 side, 5 players the 4-5 side, 4 players the 4-5 side's left half with the 2-3 side's
  right half, as the rulebook's 4-player setup shows (`Layouts` in `board.scala`).
- Each player starts with 5 GP, and the 2 Troops and 2 Fleets of the rulebook's setup page for that number of players (one Troop on
  each of two isles, a Fleet next to each). Each start is worth 2 GP of revenue. The colors' places are the setup pages';
  **Reading:** the players take them in seating order, whatever their colors (`Layout.starts`).
- The 17 Creature cards are shuffled into the draw pile. The first cycle's offering order is drawn at random.
- **Reading:** the board prints no names, so the isles are named after the islands of the Cyclades (Naxos, Paros, ...) and the
  sea spaces by row and column (A4 ... K7, as shown on the map when tapped).

## A cycle

1. **Creatures.** The Creature on the 2 GP space no one called is discarded; the others move right to fill the empty spaces, and
   the empty spaces are filled from the draw pile (shuffling the discard pile when it runs out). The first cycle has one Creature
   (4 GP), the second two (4 and 3 GP; the first cycle's moves to 3 GP if no one called it).
2. **The gods.** 5 players: the four gods in random order. 4 and 2 players: the last of the four is face down, and comes first
   next cycle, the other three shuffled. 3 players: two face up, two face down; next cycle the face-down pair, then a new shuffle.
   Apollo is always last.
3. **Revenue.** 1 GP for each prosperity marker the player controls: printed on its isles, Apollo's markers on them, and the
   commerce spaces (the arrows) with its Fleets.
4. **Offerings.** In turn order, each player puts its marker on a god, offering more than that god's current offering; the player
   outbid at once offers to another god. Apollo is free, and any number may choose him. Each Priest takes 1 GP off the offering,
   which costs at least 1 GP. A player can't offer what it couldn't pay. The offerings are paid once every marker is placed.
   With 2 players each has two markers and makes two offerings (it may outbid itself), and must be able to pay both.
5. **Actions,** god by god in the gods' order, Apollo last (several Apollos in their order). A god no one won is skipped.
6. **End.** The next cycle's offering order is the reverse of this cycle's action order. A player with 2 Metropolises (3 with 2
   players) at the end of a cycle wins; between several, the one with the most GP; **Reading:** still tied, they share the win.

## The gods

Every god but Apollo: call Creatures (2, 3 or 4 GP by space; each Temple, and a Metropolis, takes 1 GP off once a cycle, at least
1 GP), recruit, build (2 GP a building, on a free building space of one of the player's isles), and its special action, in any
order and as often as it can pay.

- **Poseidon:** a Fleet free, then 1, 2, 3 GP (4 a turn at most, 8 Fleets in all), in a sea space next to one of the player's
  isles, empty or with its own Fleets. Ports. Moving Fleets: 1 GP, the Fleets of one space move up to 3 spaces, taking along or
  leaving Fleets on the way; entering another player's Fleets ends the move with a battle.
- **Ares:** a Troop free, then 2, 3, 4 GP (8 Troops in all), on its isles. Fortresses. Moving Troops: 1 GP, some or all of an isle's
  Troops to another isle joined by a chain of its Fleets; an isle with another player's Troops means a battle, one without Troops is
  taken (buildings and Metropolis with it); an isle left empty stays its owner's (a territory marker). A player's last isle can't be
  attacked unless taking it would give the attacker its winning Metropolises.
- **Zeus:** a Priest free, a second for 4 GP. Temples. For 1 GP, discard a Creature on offer and draw another in its place.
- **Athena:** a Philosopher free, a second for 4 GP. Universities. 4 Philosophers become a Metropolis at once.
- **Apollo:** 1 GP (4 GP with a single isle; **Reading:** or none). The first to choose Apollo also places a prosperity marker on
  one of its isles (16 in all).

## Battles

Each round both roll the battle die (0, 1, 1, 2, 2, 3) and add their units; the defender adds 1 for each of its Fortresses on the
isle (land) or its Ports on the isles next to the sea space (sea), a Metropolis counting as both. The lower total loses a unit,
both on a tie. While both have units the defender may retreat, then the attacker; on land to one of its isles joined to the battle's
by its Fleets, at sea to a next space empty or its own. A land battle both lose leaves the isle to the defender, with a territory
marker.

## Metropolises

Founded at once with 4 Philosophers, or with a Port, a Fortress, a Temple and a University (on any of the player's isles, which are
discarded), on one of the player's isles without one. A Metropolis covers some of its isle's building spaces (read from the board's
dotted square: `IsleData.metro`); the buildings that don't fit are destroyed. With no isle for it, the Philosophers or buildings are
discarded. A Metropolis counts as every building, and is taken with its isle.

**Readings:** the player picks the Metropolis' isle; the buildings discarded for it are one of each type, those on that isle first;
when its isle has too many buildings, the last built go.

## Creatures

Called for their price, used at once, then discarded; a figure stays on the board until its Creature is called again.

- **The Fates:** revenue again. **Dryad / Satyr:** steal a Priest / Philosopher. **Griffon:** half another player's GP (rounded
  down). **Harpy:** remove a Troop. **Giant:** destroy a building (not a Metropolis). **Cyclops:** replace one of your buildings
  with another type. **Pegasus:** move Troops from one of your isles to any isle, with no Fleets. **Sylph:** move your Fleets 10
  spaces in all. **Sphinx:** sell Fleets, Troops, Priests and Philosophers for 2 GP each. **Siren:** replace another player's lone
  Fleet with yours (from elsewhere on the board if you have none left). **Chimera:** use a Creature from the discard pile, then
  shuffle the discard pile into the draw pile.
- **Kraken:** on a sea space, destroying its Fleets; each 1 GP moves it on a space, destroying the Fleets there. No Fleet may enter
  its space.
- **Polyphemus:** on an isle; the Fleets next to it are pushed a space away (the caller picks where; where they can't go, to a space
  not next to the isle, empty or of their color, they are destroyed). **Reading:** while he is there no Fleet may enter, or be
  recruited in, a space next to his isle, so no Troop lands there through Fleets.
- **Minotaur:** on one of your isles; he counts as 2 Troops defending it, never retreats (so the defender can't), and is lost after
  the isle's Troops, when the defender loses one more round. **Medusa:** on an isle; its Troops can't leave it (nor retreat).
  **Chiron:** on one of your isles; Pegasus, the Giant and the Harpy can't be used on it.
- **Readings:** Chiron and the Minotaur go on the caller's isles, Medusa and Polyphemus on any isle. A Siren's "lone" Fleet is a
  space holding a single Fleet. Temple discounts are taken automatically, as many as help.

## Not done

- The expansions (Hades, Titans, Monuments, Utopia...).
- GP are shown to everyone (the rulebook keeps them behind screens).
