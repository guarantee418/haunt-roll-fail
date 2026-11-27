# Brass: Pittsburgh: handoff

Parked (2026-11-27): to be built after the other games are done. Asked for by the owner, who reviews online board games for
publishers; Roxley's reviewers played physical copies, and the owner is to review it digitally. Keep it **unlisted** until Roxley
says it may be public: playable at its URL, not in `HRF.menuMetas`, no Discord section or announcements.

`RULES.md` has the rules from Roxley's rulebook v2026.01 (public download, roxley.com/brass-pittsburgh-rulebook; not in this
repository), with the readings and the open questions.

## State

- Nothing is built. This folder isn't compiled: it is not in `common.sbt`, so `.scala` files can be added here before the game is
  wired in.
- Names: meta `brass`, URL `/play/brass`, package `brass`, folder `haunt-roll-fail/brass/`, images in `webp2/brass/images/`.

## What is missing (needed before building)

The rulebook has every rule but not the game's data. In order of value:

1. **The card list:** the 68 Location and Industry cards, their counts and the player-count icon on each (which go at 2 and 3
   players). Nothing can be built without it.
2. **The player mat:** every industry tile's level, era limit (half moon / moon), cost (money, cubes, income change, upgrade icon),
   produced cubes or oil, oil to sell, stars, flip rewards, VP and link points; the five paths with their arrows and branches; which
   heavy train or pipeline each mat space holds and which tile unlocks it; each heavy train's type and reward; each pipeline's type and
   amount. The rulebook's picture of the mat is too small and partly covered by its lettered labels. Also whether the four colors' mats
   differ.
3. **The 2-player side of the board** (not pictured), and a clean picture of the 3-4 player side.
4. **The tile pools:** the 5 merchant tiles (icon and reward), the 6 Quality Rewards tiles (star requirement and reward), the 5
   oilfield tiles (left reward, right oil) and the reward printed under each oilfield space at the bottom of the board.
5. **Art** at full size: board, mat, tiles, cards, characters, tokens. Best is Roxley's print or press files, or a Tabletop Simulator /
   Tabletopia module; failing that, photos or scans.

## First reading of the board (from the rulebook's setup picture, p.4-5; unchecked)

The picture is the side with the 2-player mark at its top right (the setup callout points at it), so this may be the 2-player side;
p.11's scoring example shows another side ("The West" where this one has Eads Bridge). Check against the real board.

Industry icons on the slots: Kerosene (can), Manufactured Goods (crate), Coal (cart), Iron & Steel (furnace), Automobile (car),
Skyscraper (tower). Each `[...]` is one slot; `+` is a slot showing two icons.

- Saginaw: [Mfg+Car] [Coal] [Coal]
- Detroit: [Mfg+Kero] [Iron+Kero] [Car]
- Buffalo: [Mfg+Kero] [Iron] [Mfg+Kero]
- Rochester: [Mfg] [Iron+Mfg]
- Erie: [Car] [Iron+Mfg]
- Chicago & Whiting: [Sky+Kero] [Sky] [Mfg+Kero] [Iron]
- Toledo: [Mfg+Kero] [Iron+Kero] [Car]
- Cleveland: [Sky+Kero] [Mfg+Car] [Iron+Kero]
- Columbus: [Mfg+Coal] [Coal]
- Indianapolis: [Car] [Mfg]
- Pittsburgh: [Coal] [Mfg+Kero] [Iron+Mfg] [Iron+Coal] [Mfg+Kero]
- Wilkes-Barre & Scranton: [Coal] [Coal] [Coal]
- Bethlehem: [Iron+Coal] [Iron+Mfg]
- Harrisburg: [Coal] [Iron+Coal]
- Clarksburg: [Coal] [Coal] [Iron+Coal]
- Baltimore: [Mfg+Kero] [Iron+Kero] [Mfg+Kero]
- Philadelphia: [Kero+Car] [Iron+Mfg] [Sky]
- Boston: [Mfg+Car] [Sky], and a merchant space
- New York & Newark: [Mfg+Kero] [Sky] [Kero] [Iron]
- Albany, Cincinnati: no slots, 3 link icons each (junctions).

Merchant spaces (the tile set up there in the picture): Canada (Car, 8), Eads Bridge (Kerosene, 4), Boston (Mfg, 6), The South
(Kerosene and Mfg, 15; the 2-player game removes the $15 tile). Link icons: Canada 3, Eads Bridge 3, The South 2, Albany 3,
Cincinnati 3.

Markets: coal and iron by Toledo (4), Wilkes-Barre (2), Clarksburg (3): 4 iron and 4 coal spaces each; the number is likely the
price. Steel and coke at Canada (6), Eads Bridge (5), Philadelphia (4): 3 steel and 2 coke spaces, the bottom coke space marked 4
(the VP). **Open:** what the numbers mean exactly, and the price of each space.

Oilfields: Trenton, Lima & Findlay, Bradford & Olean, Venango, Macksburg & Burning Springs. Pipeline routes (black lines with barrel
icons) join them to Chicago, Toledo, Cleveland, Erie, Pittsburgh, Columbus.

Railroad line icons seen: whiskey (Canada - Detroit, Columbus - Pittsburgh, Indianapolis - Cincinnati, Albany - Boston), income
(Saginaw - Chicago 2, Bethlehem - Harrisburg 3, Rochester - Albany 3), develop (Buffalo - Rochester, Eads Bridge - Indianapolis,
Harrisburg - Philadelphia). The lines themselves still have to be traced.

Board edges: progress track 0-100 (VP and income on the same track; income levels on coins beside it), Vanderbilt (coal or iron $7,
steel or coke $10), the link cost box (pipeline 1 iron, light train 1 coal, heavy train 2 steel, a second link +$10), the 3 Quality
Rewards spaces, the oilfield reward row and the turn order track for the 4 characters.

## Plan

Follow Cyclades' layout (`cyclades/HANDOFF.md`):

- `board.scala`: locations, slots, railroad lines with their icons, pipeline routes with their oil, markets, merchant spaces,
  oilfields; both sides. A `Check` that the data is consistent (every line joins two places, slot counts, market sizes).
- `game.scala`: the rules. A round is the players' turns (`TurnAction`, two `ActionAction`s), `RoundEndAction` (turn order by money
  spent, income, bankruptcy), `EraEndAction` (scoring, removals, oil, the new deck). Resource sourcing (`sources`: connected tiles
  first, then the player's heavy trains, markets, Vanderbilt) is the heart of it and should be one function used by Build, Network,
  Develop and Sell, with the connections recomputed from the train links.
- `bot.scala`: an Easy bot, then something better; Brass bots are hard to make good.
- `host.scala` (JVM only): `brass.Host` bot games, with random choices mixed in to reach every rule.
- `ui.scala`, `meta.scala`, `styles.scala`, `serialize.scala`, `package.scala`.
- `tools/brass-art.py`: makes `webp2/brass/images/` from the art files once there are any.

Then add `brass` to `common.sbt` and `brass.Meta -> brass.UI` to the metas in `hrf.scala` (not `menuMetas` until Roxley agrees).
