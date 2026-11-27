# Cyclades: handoff

Meta `cyclades`, URL `/play/cyclades`; the base game, 2 to 5 players, with an Easy bot. Not on the main menu yet (`HRF.menuMetas`).
RULES.md has the rules as played and the readings.

## Files

- `board.scala`: cells (row, column; odd rows shifted half a space right), `Sea` and `Isle` areas, `Isles.big` (the 4-5 side) and
  `Isles.small` (the 2-3 side) with each isle's cells, printed prosperity, building spaces and the spaces under its Metropolis, and
  `Layouts` for 2, 3, 4 and 5 players: the map image, its grid (`x0`, `y0`, `dx`, `dy` in the image's pixels), commerce spaces and
  the rulebook's starting places.
- `game.scala`: the rules. A cycle is `CycleStartAction` (Creature track), `GodsAction`, `RevenueAction`, `OfferAction` (one marker
  at a time; an outbid marker, `Ousted`, offers next), `PayOfferingsAction`, `GodTurnAction` (each god's `MainAction` menu, Apollo's
  `ApolloTurnAction`), `CycleEndAction`. Fleets on the move are `game.moving` (`FleetMove`), a battle `game.battle` (`Battle`, the
  attacker's units off the board until it ends).
- `bot.scala`: `BotXX`, the Easy bot; `BotRandom` for the tests.
- `ui.scala`, `meta.scala`, `styles.scala`.
- `host.scala` (JVM only): `Host` (bot games), `Check` (the layouts' data), `LogDump` (one game's log as text).
- `tools/cyclades-art.py`: makes `webp2/cyclades/images/` from the Tabletop Simulator mods' images; its header lists them.

## The board data

Read off the board images with the hex grid overlaid (cells, isles, cornucopias, white building squares and the red dotted
Metropolis square), and checked against the rulebook's setup pages: every color's start is worth 2 GP, as the rulebook says
(`Check`). The 4-player board is the mod's own image of the two halves together.

## Testing

In a JVM project with the sources (see Pincer's HANDOFF.md, the same setup with `cyclades` added):

    sbt "runMain cyclades.Check"
    CYCLADES_GAMES=30 CYCLADES_RANDOM=0.4 sbt "runMain cyclades.Host"   # 2-5 bots, 40% of the choices at random
    CYCLADES_RANDOM=0.4 sbt "runMain cyclades.LogDump 5"

`CYCLADES_RANDOM` reaches every Creature, battles and retreats; the Easy bot alone rarely fights.

## Next

- The main menu card and How to Play (done) / a Tutorial, rankings, Discord pings (`TurnPings.slots`), as the other games have.
- Hidden GP (behind screens) for online games.
- A better bot: it rarely fights or calls Creatures.
- The expansions.
