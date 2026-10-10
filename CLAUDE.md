# HRF (haunt-roll-fail) fork

Fork of the HRF board game site (hrf.im), with the Twilight Council and
Knaves of the Deepwood factions (playtest and official Root: Homeland
versions; the Homeland ones are `faction-council.scala` and
`faction-knaves.scala`). Written in Scala:

- `haunt-roll-fail/` — the game client, compiled to JavaScript with Scala.js
  (`target/scala-2.13/hrf-opt.js`, about 8 MB, 1.8 MB gzipped)
- `good-game/` — the Akka HTTP server (`GoodGame.scala`) that serves the client
  and stores games in an HSQLDB database
- `scala-js-dom-reduced/` — DOM library the client depends on
  (`sbt publishLocal` once before building the client)

Northgard: Uncharted Lands is being added in `haunt-roll-fail/nort/`
(meta name `nort`, URL `/play/nort`), base game first, expansions later.
`nort/RULES.md` summarizes the rules (the rulebook PDFs aren't in the repo)
and tracks what is done. `nort/HANDOFF.md` has the state, design notes,
known gaps, next steps and how to build and test it. So far: the 7 clans (14 with New Blood), 2–6 players (six on the five-player rules), team
play with any split into teams on the 2v2 rules ("Free-for-all" or "Teams" above the clan
picker, `Meta.pickerModes`; each player's team on its setup row, `TeamOption`), the year loop with
decks, Wait/Replace/Remove/Upgrade/Pass, Flash cards, harvest trading, winter
and Unrest, end-of-game scoring (the end screen shows the winner's clan card, "tames these lands and triumphs as the supreme Jarl", and how they won), and the real card list in `nort/cards.scala`
(names, fame, text, images), the map: tile data in `nort/tiles.scala`
(areas, borders, resources, spaces, checked against the art), territories
and placement rules in `nort/board.scala`, and setup, Recruit, Move,
Explore, Build, Feast, combat and retreat in `nort/map.scala`. Every card
effect works (special ones in `nort/effects.scala`) and all seven clan
powers, including Bear's Kaija and Snake's Scorched Earth; the Creatures
module (with its More Creatures variant) is in `nort/creatures.scala`, the
Warchiefs module in `nort/warchiefs.scala` (the clan picker's Warchief
button shows each clan's warchief board), the Wilderness expansion
(Environment tiles with impassable borders, and five more creatures plus
the Ancestral Graveyard's Spectral Warriors and the Wyvern's Den) in
`nort/wilderness.scala` (tiles `wild-*` in `Tiles.environment`), and the
New Blood expansion's seven clans (Dragon, Horse, Kraken, Lynx, Ox, Rat,
Squirrel, with their warchiefs and 28 clan cards, from the TTS mod
3597126237) in `nort/newblood.scala`; picking one of them turns it on.
Uncharted Horizons' Events and Alternative victory conditions modules are in
`nort/horizons.scala`. Its 13 Development cards and 2 Achievements (option
"Uncharted Horizons Development cards", next to "Ban card draw developments")
are in `nort/horizon-devs.scala`, and its 5 map tiles with The Bridge (option
"Uncharted Horizons map tiles", under the Central tile choice) are
`Tiles.horizons` (`horizon-*`, made by `nort/tools/horizon-tiles.py`). Its Sea module (Raids: a Beach tile with a Port for each
player, the Raid phase and the 24 Raid cards, from the TTS mod 3597126237,
whose rulebook PDF has the rules) is in `nort/sea.scala`; the Beach is four
map cells (`Tiles.beach`), and a tile side no area owns is sea. The Wastelands expansion (Environment tiles
`waste-*`, the Central tiles `start-*` chosen with the "Central tile" option, which
is offered in every game (standard tile, any of the nine, the Wilderness Great
Lake, or random) and needs
the module only for the rest of Wastelands,
five more creatures, Hrimgandr and Jötunn Blainn) is in `nort/wastelands.scala`.
The solo Automa (Uncharted Horizons' Solo module) is
in `nort/automa.scala`, with its 15 cards transcribed from the Tabletopia
module ("Solo vs Automa" on the main menu). Its Training Fields module, a two-player duel on
a 4x3 grid of face-down tiles with seven Action cards each, is in
`nort/training.scala`: "Training Grounds" on the main menu opens a page
offering a local or an online game (the `modes` hooks in `meta.scala`,
`modeMenu` in `hrf.scala`). "Adset" on the main menu (the `linkedModes` hook) opens
its own meta, `nort-adset` (`MetaAdset` in `nort/adset.scala`, URL `/play/nort-adset`):
a free-for-all whose players are seats (`Seat`, "Player #n") that draft their clans
during setup (random seat order, players + 2 clans drawn, a ban, Wilderness or
Wastelands, the central tile, then picks and placements from the last seat), always
with Creatures and the Horizons Development cards. `Game` takes `players` (clans,
or seats in Adset) and maps them with `ptf`/`ftp`, as Root's Advanced Setup does;
asks for a clan go to its seat in `loggedPerform`, and the in-game choices are added
to `game.options` (`addOption`). Solo, Quick and Local (hotseat) games are saved in the
browser's localStorage as they are played (`newLocalGame` and `savedGamesMenu` in
`hrf.scala`, `LocalStorageJournal` in `journal.scala`; keys `<settingsKey>.<kind>.game.<time>`
with kind `solo`, `quick` or `hotseat`; the 12 newest of each kind are kept, and when storage
is full the oldest of any kind make room), so "Quick Game", "Local Game" and "Solo vs Automa"
offer a new game or a saved one to continue (straight to a new one when none is saved),
like Play Online. Training Grounds games aren't saved. The
code was checked against the English core rulebook on 2026-10-03; rule
choices the rulebook leaves open are listed under Interpretations in
`nort/RULES.md`.
On the map, territories are tinted with their controlling player's color,
turn gray when invaded and pink during the fight (resource icons and the
printed building space frames stay untinted: `tint` in `nort/ui.scala` cuts them out); closed territories that give fame have their border dashes
drawn in the controller's color (alternating where two players' meet,
dotted yellow or white rails beside Rough borders, solid ones beside
the orange impassable lines of the Peaks, the Poisonous Swamp's corners and the
walled five-player tile), from the dash data in
`nort/lines.scala` made by `nort/tools/BorderLines.java`; the area masks (`tile/mask/`) and the free-ground
grid for placing warchiefs, Kaija and creatures (`nort/grid.scala`) are made
from the tile art by `nort/tools/tile-masks.py` (it keeps figures off the Great Lake's water and,
on the Wilderness, Wastelands and Uncharted Horizons tiles, the rocks and the rock bands along impassable and rough borders; `tile-masks.py <tile ...>`
regenerates only those tiles' grid entries). Warchiefs, Kaija and
creatures are kept off the resource icons (`nort/icons.scala`, the holes in
the masks, made by `nort/tools/ResourceIcons.java`), and the icons are drawn
again over the pieces, so a figure that must overlap one never hides it. Tapping a piece on the map opens what it stands for, unless the tap
picks its territory for an offered action (`pieceInfo` in `nort/ui.scala`): a creature, Jötunn Blainn
or a Raid card its card, a warchief (and Horse's Brok) its board, power and upgrade card (`WarchiefInfo`,
`Meta.warchiefInfo`), and Kaija, Lynx's Brundr and Kaelinn, the High Tide and Scorched Earth tokens their
clan's three clan cards (`ClanAbilityInfo`, `Meta.clanAbility`). The Automa's Leaders open nothing.
The Northgard layout (`layouter` in `nort/ui.scala`) has the map on the left
and the player panels, log and action pane (choices and hand) on the right;
the owner asked for that on 2026-10-05, undoing an earlier mirrored layout
with the log on the left. Ultrawide screens
(width at least 2.1 times the height, e.g. 21:9) skip the layouter and get
a fixed layout (`layout` override in `nort/ui.scala`): map, then short
player panels in a row, the shared cards and the hand below them, and the
log on the far right, with the
hand cards sized to the pane (`--nort-hand-card`) so the whole hand fits. The shared cards strip (Developments, Achievements, Creatures and the rest, the `court` pane)
has a tab on its left that folds it into a one-line bar listing each group with its card count, and back;
the player panels right above it (ultrawide) grow into the room with a larger font (as far as their width
allows), or else the pane right under it (the map) does, and nothing else moves (`foldCourt` in `nort/ui.scala`). Folded, the player panels are laid out again in one row or several (2x2 for four panels on a phone), whichever gives the largest font that still fits their measured contents, each only as tall as it needs; the rest of the room goes to the pane below. They are measured again once their icons have loaded (an icon takes no room before), so a game loaded folded fits too (`refitFolded`). The folded bar starts with "Year X of 7". Where the strip sits left of the panels at the top (16:9 and 16:10), its tab points sideways (◂ to fold, ▸ to unfold): folded, it is a narrow tab on the left with its text running down in two columns, and the panels run across the top from it, with the map and log under them (`courtSideways`). With two players (or one) on a tall screen (a phone held upright), the panels are moved up beside the strip (`panelsBesideCourt`), wherever the layouter put them, so it folds sideways there too. Each browser remembers it (`<settingsKey>.court-folded`). The divider right of the map, the one between the log and the action pane, and the bottom edge of the player panels have round drag handles with arrows (`split` and `placeHandles` in `nort/ui.scala`; asked for by Gman on 2026-10-08): dragging moves the panes along that edge, each kept at least 15% of the window (moving the map's edge left, an unfolded row of court and panels above it follows; a row of panels starting at the edge shares the room), double-tapping one puts it back, and each browser remembers the offsets per screen shape (`<settingsKey>.split-<tall|wide|ultrawide>`, "x,y,panels"). The panels' edge stretches or shrinks the panels (to half their height at least) and their font with them, as far as their measured contents fit their width; the court right under them moves along, the panes under that give the room (the log down to 15%, then the action pane). Folded, the extra height is first given to `foldCourt` (its `extra`), which may lay the panels out in more rows with a larger font. A layout with no pane past the map's right edge or under the log (a phone held upright) shows no handle. A page runs one game: `startGame` in `hrf.scala` ignores a second start (`gameStarted`, reset by Play Again), and the handles remove any left by an earlier game UI, after Robotos saw two of each handle on 2026-10-09. The player panels show the card counts, then units, warchief and supply, then a small table with grid lines around its cells (`ledger` in `factionStatus`, `nort/ui.scala`; asked for by the player Robotos on 2026-10-08): a column per resource and one for fame, the stockpile, then under it the clan's next harvest as things stand
(`Harvest.forecast` in `nort/game.scala`, the same sums `HarvestAction` uses; the Dragon's food and wood as ranges, "+1-2", the upper end faint since its sacrifice adds 1 to only one of them) in green and its Winter cost (as losses, "-1") in red, on rows marked with a wheat sheaf and a snowflake (`SeasonIcon`, drawn by `nort/tools/panel-icons.py`); tapping the table opens the whole
Winter chart (`winterChart` in `nort/ui.scala`), except the lore (its icon or number), which opens the clan's Lore Tree (`LoreTree`, `loreTree`; Gman moved it there from the action pane on 2026-10-08). Each player can hide the
tints and the colored borders under "Interface" (the Territory Color and
Fame Borders settings in `nort/meta.scala`). The tints are the player colors themselves
(`PlayerColor.hex`, the same colors as the names), at 50% opacity; picking "Custom" under Territory Color
(`CustomTerritoryColor`) shows the "<Color> Territories" rows, where each player sets how
opaque each color is, 0 to 100% in steps of 10 (default 50%) (`TerritoryOpacity`, a
`CompactSetting`: its values are one row of small buttons in the settings screen,
`editSettings` in `hrf.scala`; rows are hidden while `Setting.visible` is false). The "Player Panels" setting (`StackedPanels`, the default since 2026-10-08, `CompactPanels`, or `ReverseStackedPanels`) shows every number in the panels under its icon, beside it ("Compact"), or above it ("Reverse Stacked"). A turn starts with six choices at
the top of the action pane (Play cards, Wait, Replace, Remove, Upgrade,
Pass); after one, tapping a card in hand does it. A Build card's builds: tap
a free space, pick from the menu of all buildings with their costs, then
confirm with the green check mark (or cancel with the red cross) drawn above
the building on the map. Placing a tile works the same way: the tile is shown at its spot with
one button just outside each corner: rotate arrows at the top, the check mark and cross at the bottom. A retreat works like a Move: tap the destination on the map (or in the list),
then pick who goes: everyone, the units without Kaija and the warchief, one unit, or Kaija or the warchief alone (`RetreatPickAction` and `retreatParties` in `nort/map.scala`). Development
and other card picks show the cards at hand-card size, and the last player to pass
still sees the one card left before taking it. Copies of the same card in hand (and among
the Played cards) are drawn as one card with the copies peeking out behind it
(`Card.handStack` in `nort/cards.scala`), in the hand and in every pick from it. Tapping a player's name in its panel opens its clan's info (`ClanInfo`, the same as the clan picker's "i" button); the clan board that was shown under the Lore Tree was removed for it on 2026-10-08 (asked for by Robotos). The player panels show numbers with icons, on rows that don't wrap: cards to draw, in hand, played this year (the active cards) and discarded (card icons outlined yellow, green, white and red, `ui-card-draw`, `-hand`, `-played`, `-discard`, drawn on the start card back by `nort/tools/panel-icons.py`; tapping the white card shows the active cards, the red one the discard pile), then the units on the map (the Recruit card's figure, `ui-unit`; with the Sea module, the units away on Raids in brackets beside it with a longship, `ShipIcon`, `ui-ship`, drawn by `nort/tools/panel-icons.py`), the warchief with the Warchiefs module (its token and name, grayed out while in the reserve) and the units left in the supply (`SupplyIcon`, `ui-unit-supply`, the units icon grayed out, by `nort/tools/panel-icons.py`), then the food, wood, lore and fame table, and the first player marker beside the clan name (`ui-first-player`).
After the die decides a fight (against a clan or a creature), a combat report (who won and how, each side's combat points and casualties with their sources, what each lost) is shown to both sides before the retreat, to be tapped OK (`CombatReportAction` in `nort/map.scala`); each player can skip theirs with the in-game "Combat Report" setting under Interface (`ShowCombatReport`, the default, or `SkipCombatReport`, like Root's Ambush! setting; the UI then taps OK for them, `ask` in `nort/ui.scala`). The old game options `CombatReportAttackers` and `CombatReportDefenders` are hidden and do nothing. In every game, the log's word for how a fight ended ("won", "defeated", "drove", "wiped out") opens that fight's report (`CombatReport.link`, `FightReportView`).
Combat points and casualties are drawn as the battle die's axe and skull wherever they come up (fight info, die results, the log, card and rules text): `CombatIcon` and, for rules text, `CombatText` in `nort/game.scala`, with `ui-axe` and `ui-skull` cut from the die texture `token/die.webp` (TTS mod 2838546142) by `nort/tools/dice-icons.py`.
Images are in `webp2/nort/images/` (`card/`, `tile/`, `token/`), from a
Tabletopia export the owner uploaded, with gaps filled from two Tabletop
Simulator mods (Steam Workshop 2838546142 and 2847156187; see
`nort/HANDOFF.md`): all 35 core map tiles (`tile-31` to `tile-33` and
`start-5` are TTS scans aligned to the old photo crops), all 52 development
cards (Veiled Threats from TTS), and the warchief upgrade cards. Green starting cards are the blue ones with the ribbon
recoloured to the printed green. Expansion tiles, clan
boards and tokens are in `expansion/` for later. Unit figures are in
`token/unit/` (`unit-<color>`, and `warchief-<color>` for the Warchiefs
expansion), recolored from the `-original` images. The seven core clans'
warchiefs are drawn as their portraits instead (`chief-<clan>-<color>`, outlined
in the player's color, from the TTS standees by `nort/tools/warchief-portraits.py`;
`Warchief.figure`), and the New Blood warchiefs (and Horse's Brok) as round
tokens with their head from their clan card (same names, `brok` for Brok, by
`nort/tools/warchief-heads.py`). The Automa's Leaders are its two miniatures
(`leader-<1|2>-<color>`, white and black, cut from the Uncharted Horizons
rulebook photo by `nort/tools/automa-leaders.py`). The old warchief figure is
now a second warrior design: each stack of warriors is drawn with one of the
two, picked by a hash of the territory and the count (`Warchief.warrior`). Colors belong to
players, not clans: each clan's player picks one on its row of the setup screen
(`ColorOption`, default blue, red, yellow, purple, green, orange by seat;
`game.colors`); starting cards show that color's banner (orange has no
cards of its own and uses the yellow ones). The clan picker (custom, online and solo games) is a grid of
clan emblems (`factionTile` in `nort/meta.scala`, the `factionPick` helper in
`hrf.scala`); each clan's "i" button opens its clan ability, both clan
upgrades, warchief board and power, and warchief card (`factionInfo`). It also
offers two Random Clan picks, one of the seven core clans or one of all fourteen
(`randomFactions` in `nort/meta.scala`, the hook in `meta.scala`): the clan is
drawn among those not already picked and is shown only as "Random Clan" until
the game starts (`hidden` in `customGame` and `startSetup` in `hrf.scala`;
picking the drawn clan by hand draws again). The setup options
(colors, game length, victory conditions, first player, "Ban card draw developments"
(`NoDrawDevelopments`: leaves the seven Development cards that only draw cards
out of the decks), and the modules and
expansions; Creatures, Warchiefs, Wilderness, Wastelands and Events are implemented, the others are shown but disabled) are in `nort/options.scala`.
"Victory conditions" picks one of: the standard rules, fame only, Alternative
victory with random cards (the rulebook's way), or Alternative victory with
cards chosen from the 21 listed below it (exactly 1 Map Control and 2 Wealth,
3 with teams, or Start is refused); plus Thane or Jarl. Thane/Jarl and the
cards are listed only when they apply (`optionShown` in `nort/meta.scala`, a
hook the setup screen in `hrf.scala` calls for every option). Long sections fold under a tappable
heading with an arrow (`Folds` in `ui.scala`, `BaseOption.fold`): on the setup screen Game length, Victory conditions,
Automa difficulty and Central tile, in the settings Font Size, Scroll Speed (every game) and Territory Opacity. Folded,
a section shows only what is picked (Territory Opacity a one-line summary, `Setting.foldSummary`); every section starts
folded and the open ones are remembered in the browser (`hrf.open-sections`). Victory conditions stays open while the
chosen cards are the wrong number (`Meta.optionFoldOpen`). Northgard no longer sets `underConstruction`
in its `Meta` (setting it to `true` would put an "Under Construction" note under
its name on the game list and a disclaimer at the top of its menu).
`nort/host.scala` runs bot games headless (JVM only,
like the other `host.scala` files). `nort/replay-check.scala` (`sbt "runMain nort.ReplayCheck"`,
same setup) checks that undo and loading rebuild the same game; see
`nort/HANDOFF.md`. Northgard has two bots: "Easy" (`BotXX` in
`nort/bot.scala`, random) and "Hard" (`BotHard` in `nort/bot-hard.scala`, which
values each choice by trying it on the game and scoring the position; see Bots
in `nort/HANDOFF.md`), plus "Robotos" (`nort/robotos.scala`), the Hard bot with
cheats: no Winter costs, ignored by creatures, one more unit with every Recruit,
one more card each year, 1 more combat point when attacking, a head start (2 more food and
wood, 5 units in each setup placement), 25 units instead of 14 and upgrades for 2 lore; its panel says "(Robotos)", and
tapping that lists the cheats.
Its rules come from a hidden `RobotosOption` that `startGame` in `hrf.scala` adds
for each clan set to it (`Meta.botOptions`), so every client and replay agrees.
Adset seats can be set to Robotos too: `MetaAdset.botOptions` adds a hidden
`RobotosSeatOption(seat)`, and the clan that seat drafts cheats (`game.cheaters`,
recomputed in `AdsetExpansion.assign`).

## Building

```
cd scala-js-dom-reduced && sbt publishLocal
cd haunt-roll-fail && sbt fullOptJS
```

- `index.html` loads the optimized build `hrf-opt.js` from `sbt fullOptJS`
  (Scala.js optimizer plus Closure Compiler, about 4 minutes from scratch).
  `sbt fastOptJS` still works for quick local builds, but its 49 MB
  `hrf-fastopt.js` is not used by the site and is git-ignored; to try it,
  temporarily point the script in `index.html` at `hrf-fastopt`.
- The client build needs more than sbt's default 1 GB heap or it fails with
  `OutOfMemoryError`. Use `SBT_OPTS="-Xmx6G -Xss4M"`.
- Build output under `target/` is committed, including `hrf-opt.js`.
  After changing client code, run `sbt fullOptJS` and commit `hrf-opt.js`,
  `hrf-opt.js.map`, `hrf-opt/main.js` and `hrf-opt/main.js.map`
  so the server can run without rebuilding. Don't commit the rest of the
  `target/` churn a build produces.

## Live server

- URL: https://games.clean5110.com/play (Let's Encrypt certificate).
  `http://` links redirect to it.
- Oracle Cloud Always Free instance `hrf-server`: Ubuntu 24.04 aarch64,
  2 OCPUs, 12 GB RAM, ephemeral public IP 157.151.177.11
- DNS: A record `games` -> 157.151.177.11 under Custom Records in the
  Squarespace DNS settings for `clean5110.com`. The bare domain and `www`
  are a Squarespace website; leave the Squarespace presets alone. If the IP
  changes, update the `games` record.
- SSH from the owner's Mac: `ssh -i ~/.ssh/oracle.key ubuntu@157.151.177.11`
- Checkout at `~/hrf` on a local branch `hrf`; database at
  `~/hrf/good-game-database*`; `SBT_OPTS` is set in `~/.bashrc`
- The server runs in a tmux session named `hrf`, listening on port 443, with
  a redirect server on port 80
- Ports 80, 443 and 7070 are open in the Oracle security list and in the
  instance's iptables (rules placed above the `REJECT` line, saved with
  `netfilter-persistent save`). `/etc/sysctl.d/50-hrf-ports.conf` lets the
  non-root server bind ports below 1024.

### https

- The server turns on https when `certificate.pkcs12` (PKCS12, empty
  password) exists in its working directory, `~/hrf/good-game`, and rereads
  that file within a minute when it changes, so renewals need no restart.
- When it runs on a port other than 80 and that file or `good-game/acme/`
  exists, it also starts a port 80 server. That server serves Let's Encrypt
  challenges from `good-game/acme/` and redirects everything else to the URL
  argument, keeping the path.
- Set up with `~/hrf/setup-https.sh games.clean5110.com` (arguments:
  hostname, optional email for expiry notices). It allows non-root use of
  ports 80/443, opens them in iptables, installs certbot and the renewal
  hook, and gets the certificate. It is safe to run again, for example for a
  new hostname; then change `URL` in `live-server.sh`, restart, and
  `sudo certbot delete --cert-name <old name>`.
- Certbot renews through `certbot.timer`. Its hook
  `/etc/letsencrypt/renewal-hooks/deploy/hrf-pkcs12.sh` rewrites
  `certificate.pkcs12` for the hostname it was set up for.
- `certificate.pkcs12` holds the private key and is gitignored. Never commit it.
- Old `http://157.151.177.11:7070` links can be sent to the redirect server
  (only while the server is not on 7070, or it makes a redirect loop):
  `sudo iptables -t nat -A PREROUTING -p tcp --dport 7070 -j REDIRECT --to-ports 80`
  then `sudo netfilter-persistent save`.

### Starting, stopping and deploying

`live-server.sh` in the checkout runs the server in the tmux session `hrf`
with the right arguments. Run it from the owner's Mac over ssh:

```
ssh -i ~/.ssh/oracle.key ubuntu@157.151.177.11 '~/hrf/live-server.sh deploy'
```

- `deploy`: stop the server, check out `origin/main` (`git checkout -f -B hrf
  origin/main`; `-f` because builds on the server modify committed `target/`
  files), then start it and wait until it prints `Started server.`
  `deploy <commit>` deploys that commit instead (used by auto-deploy, below).
- `start`, `stop`, `restart`: as named. `start` does nothing if the tmux
  session already exists.
- `log`: the last 40 lines of server output, e.g. after an error page.
- `install-autostart`: adds a crontab entry
  `@reboot sleep 30 && ~/hrf/live-server.sh start`, so the server starts
  after a reboot. Installed once; output of those starts goes to
  `~/hrf-autostart.log`. Running it again changes nothing.
- The script starts tmux with a login shell, so `sbt` and `SBT_OPTS` come
  from the profile and `~/.bashrc` even under cron.
- `run create ...` (same arguments as the server) only creates the database,
  then exits. It was already run once; don't run it again.
- The URL arguments (`URL` in the script) must be the public address, not
  `localhost` or the IP. The server writes them into every page, and only
  serves `/hrf/` files to requests whose `Referer` starts with that URL.
  Starting it with a different address than people use gives a blank page or
  a "Loading assets" hang.
- `pkill -f "hrf[.]gg[.]GoodGame"` in the script: the brackets stop `pkill`
  from matching the command line of the shell running it.
- `Address already in use` on start means an old server is still running:
  `live-server.sh stop`, then `start`.

### Deploying from GitHub (auto-deploy)

Cloud Claude sessions can't ssh to the server, so the server watches the
GitHub branch `deploy` instead. To deploy what is on `main`:

```
git fetch origin main && git push origin origin/main:refs/heads/deploy
```

- A crontab entry runs `live-server.sh auto-deploy` every 2 minutes. When
  `deploy` points at a commit it hasn't handled, it runs `deploy` for that
  commit: the same stop, checkout and start as above, so the server restarts.
- It only deploys commits that are on `main`; a `deploy` branch pointing
  anywhere else is logged as `Not deploying ...: not on main` and skipped.
- `~/hrf-deployed` holds the last `deploy` commit it handled (written before
  deploying, so a commit that fails to start isn't retried every 2 minutes;
  push a new commit or delete the file to retry). Output goes to
  `~/hrf-auto-deploy.log`. Runs don't overlap (`flock` on
  `~/.hrf-auto-deploy.lock`).
- No `deploy` branch on GitHub means nothing happens.
- `install-auto-deploy` adds the crontab entry. Running it again changes
  nothing. To turn auto-deploy off, remove the `auto-deploy` line with
  `crontab -e`.
- A manual `live-server.sh deploy` still deploys `origin/main`; auto-deploy
  only acts when `deploy` moves.

### Game copies, fixes and backups

Cloud sessions can't reach the server, so the games are copied to a private
GitHub repository, `guarantee418/hrf-games`, and fixed through it.

- Every minute the server (`good-game/Backup.scala`) writes every game as
  text to the directory named in `good-game/backup-dir` (or
  `HRF_BACKUP_DIR`), `~/hrf-games`, a clone of that repository, and applies
  the fixes committed to its `fixes/`. `live-server.sh backup-sync` (cron,
  every 5 minutes, log in `~/hrf-backup.log`) commits that copy, pulls and
  pushes; the server's files win conflicts. `good-game/games-README.md` is
  copied into the repository as its README: it describes the files and how
  to fix a game.
- To fix a game from a cloud session: add `guarantee418/hrf-games` with
  `add_repo`, copy `games/<id>.tsv` to `fixes/<id>.tsv`, edit the moves,
  keep the first line (`# game <id> entries <n>`), commit and push to its
  `main`. The server applies it only if the game still has `<n>` entries,
  and writes `fixes/<id>.result` and the old log `fixes/<id>.before.tsv`.
  Players reload afterwards: the server refuses moves that would leave a gap
  in a game's log, so a stale browser can't corrupt it.
- The repository holds the player secrets (`users.tsv`, `plays.tsv`): the
  owner chose that so a restore keeps every link working. It must stay
  private; never copy those files anywhere public, including this repo.
- Set up once with `~/hrf/live-server.sh install-backup`: it makes the deploy
  key `~/.ssh/hrf-games`, clones the repository (or prints the key to add
  under the repository's Settings > Deploy keys, with write access), writes
  `good-game/backup-dir` and adds the cron entry. The server reads
  `backup-dir` each minute, so no restart is needed.
- Once a day the server also saves a database copy in
  `~/hrf/good-game/db-backups/` (`.tar.gz`), kept 14 days.
- `sbt "run restore <database> <directory>"` in `good-game` rebuilds a new
  database from a copy of the repository.
- `backup-dir` and `db-backups/` are gitignored.

### Bug reports

- The in-game menu (under "Interface") has a "Report a Bug" button. The
  player types a summary and description; the client adds the game, factions,
  options, online game id, the last 300 moves, the last 50 console errors and
  warnings (collected by the `console-capture` script in `index.html`) and the
  browser. It posts that to `/report-bug`, and the server opens a GitHub issue
  labeled `bug report` and shows the player its link (to add a screenshot).
- The server saves every report to `~/hrf/good-game/bug-reports/` first, so
  none are lost if posting fails.
- Posting needs a GitHub token: a fine-grained personal access token for
  `guarantee418/haunt-roll-fail` with Issues: read and write. Put it in
  `~/hrf/good-game/github-token` (or the `HRF_GITHUB_TOKEN` environment
  variable) and `chmod 600` it; the server reads it for each report, so no
  restart is needed. `github-repo` / `HRF_GITHUB_REPO` overrides the
  repository. Without a token the reports are only saved on the server.
  `github-token` and `bug-reports/` are gitignored. Never commit the token.
- Limits: 10 reports per IP address per hour, 60,000 characters per report,
  and only from pages whose `Referer` starts with the server URL. A failed
  post is logged as `Bug report not posted: ...` (see `live-server.sh log`).
- Reports never include the page URL, because player links contain the
  player's secret.

## Several sessions or accounts

More than one Claude account (or person) can work on the site. Each needs
write access to `guarantee418/haunt-roll-fail` (a GitHub collaborator) and
GitHub connected in Claude (https://claude.ai/connect-github); the Claude
GitHub App is already installed on the repo. Nothing else is needed: this
file covers the build, server and deploy. No session can ssh to the server
(the key is only on the owner's Mac), and none needs to.

- Anyone with write access can deploy, and so restart the live server, by
  pushing the `deploy` branch. Only give write access to people trusted with
  that.
- Work on separate branches and merge through pull requests; don't have two
  sessions pushing to `main`.
- Don't edit the same area in parallel (e.g. two sessions both in `nort/`).
- The committed build output (`hrf-opt.js`, `hrf-opt.js.map`,
  `hrf-opt/main.js`, `hrf-opt/main.js.map`) conflicts whenever two branches
  both change client code. Never merge those files by hand: resolve the
  source conflicts, run `sbt fullOptJS` again, and commit the new build.

## Gotchas

- Over https (or on `localhost`), the loaders in
  `haunt-roll-fail/loader.scala` store files in Cache Storage with
  `cache.add`. When that rejects (a missing file or a failed request) they
  load the file directly instead. Without that fallback the game hangs on
  "Loading assets". Over plain http from an IP there is no secure context,
  `window.caches` is undefined, and the loaders always fetch directly. Never
  call `dom.window.caches.toOption.get` unguarded: it gives a black screen
  with `None.get` in the console. Test changes to the loaders both on
  `localhost` (cache path) and through a non-localhost http address (no
  cache).
- Browsers keep every image in Cache Storage under `hrf-image-cache-<imageDataVersion>`
  (`HRF.imageDataVersion` in `haunt-roll-fail/hrf.scala`) and never refetch it. When an image
  changes but keeps its file name, bump `imageDataVersion`, or players keep seeing the old one
  (the large Northgard buildings kept their black corners that way). Older caches are deleted
  on load.
- Northgard building space positions (`small`, `large`, `carved` in `nort/tiles.scala`) were
  measured from the printed slot frames on 2026-10-06; after moving any, rerun
  `nort/tools/tile-masks.py` for the grid (it regenerates the photo tiles' masks slightly
  differently on other library versions, so keep only the tiles you changed).
- The 23 Squires and Disciples deck cards (`SquiresDeck`, listed in
  `effectsSquires` in `haunt-roll-fail/root/cards.scala`) have images in
  `webp2/root/images/card/deck/`, `apprentice.webp` through
  `the-faithful.webp`. They come from the Leder Card Library
  (https://cards.ledergames.com/, data at
  `https://ledercards.netlify.app/cards.min.json`, images under
  `https://ledercards.netlify.app/cards/root/en-US/`), cropped and resized
  to the 512x708 size of the other deck cards.
- The Squires and Disciples card effects are in `root/deck-squires.scala`
  (`SquiresDeckExpansion`), with small hooks in `turn.scala` (Shadow Council,
  Brazen Demagogue), `battle.scala` (The Faithful, Friend ambushes),
  `game.scala` (Silver-Tongue rule, Brazen Demagogue scoring, Friend crafting
  limit) and `ui.scala`. Rulings follow the Root Database FAQ. Deliberate
  simplifications:
  - Friend of the ___: once per turn, on your turn, an X card in hand is
    replaced by a `FriendCard` of the chosen suit until the end of the turn
    (`revertDisguises` at `CleanUpAction`). On other players' turns it only
    works for ambushes.
  - Silver-Tongue: the chosen clearing counts as ruled until the end of the
    current phase, not for a single effect.
  - Brazen Demagogue: the dominance can be activated only right when it is
    taken; the player keeps scoring (`demagogue` flag in `FactionState`).
  - Feather Rufflers, Spy Network, Silver-Tongue and Friend are usable from
    the Birdsong, Daylight and Evening menus on your own turn.
- The deck cards of the base, Exiles and Partisans and Squires and Disciples decks and the Homeland
  Lilypad Diaspora cards (`faction/invasive/card/hl-*`) have the newer text layout (no title in the text
  box) and the Homeland errata, from a player's Google Drive (2026-10-10). The shared deck's back follows
  the deck option (`deckBack` in `root/ui.scala`: `card-back-art`, `-ep`, `-sd`). The same Drive has
  the revised Council and Knaves boards (English and French), now `tc-board-h` and `kd-board-h`. The
  code already followed most of their changes (Banish hits only warriors and the Council picks where
  they go, Have at Thee! only for the Acting Captain, Run Away!'s forest picked by the enemy); since
  then, Empower's removed warriors wait in the supply (`empowered` in `CouncilPlayer`) until they go to
  the Loyalists, and Governors keeps the Knaves from crafting with a Captain or acclaim at a Governing
  assembly (Filch and Serve).
- Coffin Makers has three rules: forcibly removed warriors only (no option), any warrior returned to a
  supply (`UnthematicCoffinMakers`), or the Homeland errata, "warriors removed, not replaced, from the
  map to their supply" (`ErrataCoffinMakers`, the default and in the Official presets). In `root/game.scala`,
  `dead` is forcible removal, `recycle` any return, `removed` a removal from the map without being forced
  or replaced, and `unreplaced` the same for removals that went to the supply before the errata (Council's
  Empower). Replacements (Hundreds' Anoint and Mob, Lilypad's flips) and returns from off the map
  (officers, acolytes, payments, captives) don't go to Coffin Makers under the errata.
- The Gorge map (`GorgeBoard` in `root/maps.scala`, option `GorgeMap`) uses
  the plain board art the owner uploaded (2026-10-06, no score track, item slots
  or logo; `map-art.webp`), scaled and shifted onto the 2416x2214 frame of the
  earlier printed-board image so every position still fits. The board prints no clearing names, so
  the names (Ranch, Bluff, ...) are made up from the art; the layout card's
  numbers are 1 Ranch, 2 Mesa, 3 Camp, 4 Chapel, 5 Bluff, 6 Saloon,
  7 Lookout, 8 Rapids, 9 Homestead, 10 Forge, 11 Fork, 12 Pueblo. The dam
  path (Forge - Saloon) divides forests and is crossed by the Fork - Pueblo
  path, so the four forests around that crossing are adjacent in pairs
  (`damCrossing`); the bridge path (Homestead - Lookout) divides nothing.
- Root map drawing (`drawMap` in `root/ui.scala`): warriors of the same
  faction and kind in a region are drawn as one figure with a count badge
  (`stacks`, `drawCount`), like the Northgard unit counts. The in-game "Warriors" setting
  (`StackWarriors`, the default, or `SeparateWarriors` in `root/meta.scala`) can
  switch back to drawing every warrior. Every board has a score tracker
  along its bottom edge and an item tracker in its top left corner, where the
  printed boards have them: `scoreTrack` (centre of the 0 box, box spacing) and
  `itemSlots` in `Board` (`root/game.scala`), overridden for the board images
  of other sizes (Autumn, Winter, Mountain, Marsh) in `root/maps.scala`.
  `drawTrackers` draws the tracker image
  (`webp2/root/images/tracker/score-track.webp`, from the owner) there and
  `drawScoreTrack` puts each faction's VP marker on its
  score, stacked upwards when tied, with a count badge past 30. A clearing
  name that would cover the score tracker is drawn just above it instead
  (`nameOverTrack`). The in-game "Board Trackers" setting (`ShowBoardTrackers`, the
  default, or `HideBoardTrackers`) hides both trackers and their markers. Official VP marker art (`webp2/root/images/vp/`, `officialVP` in
  `root/ui.scala`) exists for the Homeland Twilight Council, Knaves and
  Lilypad Diaspora, cut from page 17 of `root-factions/`. The ten other Leder
  factions (Marquise, Eyrie, Alliance, Vagabond, Riverfolk, Lizards, Duchy,
  Corvids, Hundreds, Keepers) have markers made in the same style: the
  Council marker's laurel separated from its background, on a tile of the
  faction colour (the head colour darkened 8%), with the faction head from
  the Root Database (https://www.therootdatabase.com/,
  `/media/small_component_icons/custom/<animal>100.webp`, the same heads the
  official markers print). They aren't official art; swap in the official
  markers from Leder's print-and-play files if they become available.
  The mirror factions' markers recolour those heads to the mirror colour
  (Negabond's head is the Vagabond's in negative; Longtail Kaliph is a black
  rat on red). Fan factions are deliberately left on their `-glyph` head icon.
- The Gladiator, Cheat and Jailor Captain pieces
  (`webp2/root/images/faction/abduct/kd-captain-*.webp`) are the official
  Homeland meeples, rendered front-on from the 3D models (mesh plus texture)
  in the Tabletop Simulator mod "Root - Ultimate Collection - Homeland
  Expansion" (Steam workshop 3354438467; the save file comes from the
  `file_url` of Steam's public `GetPublishedFileDetails` API, BSON, the
  pieces are the `Vagabond - <name>` objects in its scripts).
- Knaves items are the Captains' items, not crafted items: the Vagabond
  can't take them in trade when it aids the Knaves (`AidTradeAction` in
  `root/faction-hero.scala`), the Hundreds can't loot them (Looters in
  `root/faction-horde.scala`), and the Knaves can't aid The Exile
  (`anytime` in `root/hirelings.scala`, `canAidExile` in `root/game.scala`).
- Advanced Setup's default faction pool leaves out the Vagabond, Negabond
  and the fan factions (`defaultFactions` in `root/meta-adset.scala`); the
  "Official | Riverfolk ..." presets add the Vagabond back.
- Tapping a Root faction's status pane opens its overlay (`onFactionStatus` in
  `root/ui.scala`) with its faction board. The boards of the base and
  expansion factions (`boardImage`; `mc-board`,
  `ed-board`, ..., `tc-board-h`, `kd-board-h`, loaded on demand) are the
  board fronts from the Tabletop Simulator mod "Root - Ultimate Collection"
  (Steam workshop 2516434159; its objects are JSON strings inside the
  `EVERYTHING['Standard'][<faction>]` Lua tables). Its Council and Knaves
  boards were replaced on 2026-10-10 by the revised ones a player shared (see above). Mirror factions show their original's board. Marquise,
  Eyrie, Alliance, Vagabond, Riverfolk, Lizards and Corvids have overlays
  like the Homeland ones: board, turn phases (`phases`), pieces, and a
  "More Info" toggle with short rule notes. Panes are tappable before a
  faction is set up too (`factionStatus` and `updateStatus`); a faction
  without state yet shows just its board ("Not set up yet").
- `itemSlots` (`itemGrid` for the usual 2x6 layout) places the item tracker;
  `drawTrackers` draws `tracker/item-track.webp`, on a dark translucent panel so
  its light slots show on the snowy maps, under
  them and `drawItemSlots` draws `game.uncrafted` on them.
- The Homeland Marsh map is `MarshBoard` in `root/maps.scala` (option
  `MarshMap`, images in `webp2/root/images/marsh/`; the map is the plain board art
  the owner uploaded on 2026-10-06, aligned to the earlier printed-board image and
  105 pixels taller than it (2400x2250, room for the score tracker below the
  Delta and Bayou names), and the flood markers come from the Homeland
  print-and-play PDF). The printed board
  has no clearing names, so the names (Thatch, Sedge, Weir, ...) are made up;
  comments give the Law of Root diagram numbers.
  Region ids must be unique across all boards (`Serialize.parseRegion`),
  so the Marsh river fork is Confluence, not Fork (Gorge has a Fork).
  Rules follow Law of Root M.5: with 1-4 players one clearing of each colored pair (`floodPairs`) is
  flooded at random and the paths on its flood marker (`floodPaths`) link its
  neighbours (`game.floodLinked`, used by `game.connected` and `Roads`); with
  5+ players one of each pair is left without a suit (`game.unsuited`) and
  gets Mousehold, Foxburrow or Rabbittown at random. Ruins go in Crossing and
  Confluence plus the two lowest numbered slots not flooded (`ruinsIn`).
- The Homeland landmarks Mousehold, Foxburrow and Rabbittown are landmark
  options on any map (`MouseholdLandmark` etc. in `root/meta.scala`). Each
  adds its suit to its clearing, kept when a Lilypad enclave covers the suit
  (`game.landmarkSuits`). Foxburrow is the `FoxburrowRoads` transport,
  Rabbittown a Daylight action and Mousehold a hook in `battle.scala`; the
  effects are in `MapsExpansion` in `root/maps.scala`.
- Root has a French display language: the "Language" setting (`EnglishLanguage`,
  `FrenchLanguage` in `root/meta.scala`, shared by all the Root metas) and a "Root en français"
  entry on the main menu (`rootLanguage` in `hrf.scala`, which only saves that setting). It is
  display only: `hrf.elem.Translation` passes every `Text`, `Header` and image id through the
  meta's `translation` when `html.materialize` draws it, so game state, saved moves and online
  games are the same in both languages, players in one game can each pick theirs, and switching
  mid-game redraws the page in place (`html.retranslate`). The words are in `root/french.scala`
  (whole pieces of text first, then names and terms inside longer ones; anything missing stays
  English), following the official French edition. French card art for the base and Exiles and
  Partisans decks and the eight base and Riverfolk/Underworld faction boards are in
  `webp2/root/images/card/deck/fr/` and `faction/fr/` (asset prefix `fr:`), cut by
  `root/tools/french-assets.py` from the TTS mod "Root FR" (Steam workshop 1829904481); the boards
  (and the Hundreds, Keepers and Lilypad Diaspora ones) were replaced on 2026-10-10 by newer scans from a player.
- Bot games can be run headless on the JVM with `root/host.scala` (see
  `host.xsbt` for the source exclusions); it also checks that every action
  serializes and parses back.
- The runner (`runner.scala`) takes an `Ask` with a single choice by itself without showing it. A step that
  must pause the game on one button (Northgard's combat report, `CombatReportAction`) adds `.needOk`; bots skip
  the hidden OK.
- Undo, and loading a game, rebuild it by replaying the recorded actions
  with `performVoid`, which stops at the first `Soft` action in a chain.
  So an action marked `with Soft` must only offer choices (return an `Ask`);
  if it changes the game (places pieces, moves on with `Next`), replays
  skip that and the game breaks (blank map, `... not found among ...` in
  the console). Lilypad Diaspora's `InvasiveEEEMusterAction` had this bug.
  `root/replay-check.scala` plays bot games and checks that replaying every
  prefix of the recorded actions matches the live game:
  `sbt "runMain root.ReplayCheck <games> [base|ld|tc|kd] [dense]"` with the
  `host.xsbt` setup. `ReplayCheck <games> marsh` or `marsh5` plays the
  Marsh map with 4 or 5 players and the three Homeland landmarks.
- Each online game has a Spectator link and one link per player. Spectator
  accounts can read the game but not add moves. A move posted by one gets a
  500 with `empty result set ... "right" = 'append'` in the server log.
- Player and spectator links (`/play/<meta>/<secret>`) are served with an
  empty `<title>` and no `og:title` (`gameIndex` in `GoodGame.scala`), so
  Discord and other chat apps show no "Chronicles" preview card under each
  shared link. The client sets the real title when it loads.
- `good-game` wraps static files in `encodeResponse`, so the 8 MB client is
  sent gzipped (about 1.8 MB).
- The site's icon is a drop (the owner's drawing), not HRF's crow: the
  favicon is a 64x64 PNG data URI in the `icon` link of `index.html`
  (`HRF.defaultGlyph`; games swap in faction glyphs), and `drop.png` is the
  backdrop behind the loading screen, in the crow's old dark red.
