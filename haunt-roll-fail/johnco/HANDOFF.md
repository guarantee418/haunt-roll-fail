# John Company: handoff

Parked (2026-11-27), to be built after the games in progress. Asked for by the owner, who is on the Wehrlegig Discord: Cole Wehrle
and Matt (Wehrlegig's public relations) said they'd love to see the game played more, and a group there plays it on Tabletop Simulator.

`RULES.md` has the rules from the rulebook, in short, with the readings and the open questions.

## State

- Nothing is built, and this folder isn't compiled (not in `common.sbt`).
- Names: meta `johnco`, URL `/play/johnco`, package `johnco`, folder `haunt-roll-fail/johnco/`, images in `webp2/johnco/images/`.

## Rights

- The game is licensed under **Creative Commons BY-NC-SA 4.0** (rulebook, last page). Hosting it on this non-commercial site is allowed
  with credit (Cole Wehrle, Wehrlegig Games; art by Janek Lipiński and Amita Pai) and a link to the licence. Images cut or changed from it
  must carry the same licence: say so on the About page and in `webp2/johnco/` (a `LICENSE` note there).
- Wehrlegig's encouragement makes it welcome on top of that. Keep a written OK (a Discord message is enough) before announcing it, and
  credit them on the About page like the other publishers.

## Materials

The owner's Dropbox print-and-play folder (2026-11-27), not in this repository (333 MB):

- `Rulebooks/`: Rules of Play (48 pages, text layer), the Crown (Solo) Handbook, the Player Aid, the Letter to the Player.
- `Board/`: the game board, whole and divided.
- `Cards/`: bridge-size cards (offices, laws, prestige, blackmail, setup, scenario, failure, Crown AI; fronts and backs) and mini-euro
  cards (the enterprises).
- `Punchboard/`: every token (fronts and backs), and the Ganjifa tiles (the India events).
- `Hard Components/`: the flags, the wood manifest, and STLs of the Elephant, a dome and the tower levels.

Card backs are in reverse order for double-sided printing. The official Tabletop Simulator module (by Drew Wehrle, rulebook credits) is
another source if the PDFs are hard to cut.

## An existing digital table

https://jc2e.larnth.dev (sent by the owner on 2026-11-27) already plays John Company: Second Edition online: live without an account,
async with one, 3-6 families, rooms by invitation code, spectators, the original art. Before building, ask the TTS group and Wehrlegig
whether players want a second one. What this site adds is its Discord (turn pings, LFG, rankings, the heckler) and the other games' players;
if that site's author is open to it, linking to it, or working with it, may serve the game better than a copy.

## Online design

The owner's ruling (2026-11-27): all table talk and non-binding diplomacy happen on Discord, in each game's thread (turn pings,
`TurnPings.slots`) and in DMs. The game only holds what the rules make binding:

- **Transfers at any time** (the rulebook's immediate, binding deals): family cash, enterprises, face-down blackmail cards (with peeks),
  Company and firm shares, promise cards. One "Transfer" button for every player whenever the game is waiting, offered like Root's
  Lilypad Negotiations (`InvasiveEEEExpansion.anytimeNegotiations`): only from soft menus, so replays stay the same. A transfer is an offer
  the receiver accepts or declines when it costs them something (a swap); a gift goes through at once.
- **Promise cards** as cards held by players: "call in" is an in-game action that asks the guarantor to carry it out, the rules deciding
  what "if they can" means card by card.
- **Consents** the rules require (Nepotism, the Chairman's election, Debt past 3, firm investment, mergers, hostile takeovers, Coalitions
  in Opposition): each asks the players concerned in the game at once (a `MultiAsk`, like Northgard's harvest trading), each answering
  Yes or No; the deal behind the answer happens on Discord. The Chairman's election ends as soon as one candidate has a majority.
- **Simultaneous steps**: attrition rolls, firm strategies (secret, revealed together), and Parliament's voting rounds (each round asks
  every player in turn order, as the rules say; the Prime Minister then stops or goes on).
- **Ping reasons** (`Meta.turnReason`) per office and step ("your turn as President of Madras", "vote on Calico Acts", "consent to
  hiring ..."), and public results posted in the thread (elections, laws passed, Company failure), like the Root jests.

## Plan

Follow Cyclades' layout (`cyclades/HANDOFF.md`):

- `tools/johnco-art.py`: cut `webp2/johnco/images/` from the print-and-play PDFs (board, cards, tokens, tiles).
- `board.scala`: the eight regions (orders, values, connections, home ports, export icons, borders and their shapes, loot), sea zones,
  prize boxes, tracks. `cards.scala`: offices, laws, prestige, blackmail, enterprises, setup and scenario cards, failure cards, events.
  A `Check` that the data adds up (setup cards, counts).
- `game.scala`: the nine phases, offices along the ribbon, checks, India (events, Cascade, Crises, Elephant), Parliament, scoring;
  then Deregulation and firms.
- `bot.scala`: an Easy bot for testing (random, never consenting to nepotism it doesn't hold); later the Crown as the solo opponent.
- `host.scala` (JVM only): bot games, with random choices mixed in to reach every rule.
- `ui.scala`, `meta.scala`, `styles.scala`, `serialize.scala`, `package.scala`; the Discord section (role, channels, turns webhook,
  announcements desk, a heckler) once it's listed.

Then add `johnco` to `common.sbt` and `johnco.Meta -> johnco.UI` to the metas in `hrf.scala`.
