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

- URL: https://games.clean5110.com/play (`games.clean5110.com` points at the
  server's IP). Links with `:7070` also work.
- Oracle Cloud Always Free instance `hrf-server`: Ubuntu 24.04 aarch64,
  2 OCPUs, 12 GB RAM, ephemeral public IP 157.151.177.11. If the IP changes,
  update the DNS record for `games.clean5110.com`.
- SSH from the owner's Mac: `ssh -i ~/.ssh/oracle.key ubuntu@157.151.177.11`
- Checkout at `~/hrf` on a local branch `hrf`; database at
  `~/hrf/good-game-database*`; `SBT_OPTS` is set in `~/.bashrc`
- The server runs in a tmux session named `hrf`, listening on port 7070
- Port 7070 is open in the Oracle security list and in the instance's
  iptables (rule placed above the `REJECT` line, saved with
  `netfilter-persistent save`). Port 443 is open in the security list, and
  an iptables NAT rule forwards it to 7070
  (`iptables -t nat -A PREROUTING -p tcp --dport 443 -j REDIRECT --to-ports 7070`,
  also saved). The server itself does not bind 443.

### https

- The server turns on https when `certificate.pkcs12` (PKCS12, empty
  password) exists in its working directory, `~/hrf/good-game`. It then
  serves https only, so `http://` links stop working.
- The certificate is for `games.clean5110.com` and expires 2026-12-30.
  `certificate.pkcs12.off` next to it is a copy of the same file. Renewal is
  not set up yet: a renewed certificate has to be converted to PKCS12 and
  copied to `~/hrf/good-game/certificate.pkcs12`, then the server restarted.
- `certificate.pkcs12` holds the private key and is gitignored. Never commit it.
- Port 80 is not set up, so `http://games.clean5110.com` does not redirect to
  https. The server logs "Started redirect server." whenever the certificate
  exists, even if it can't bind port 80.

### Starting, stopping and deploying

`live-server.sh` in the checkout runs the server in the tmux session `hrf`
with the right arguments. Run it from the owner's Mac over ssh:

```
ssh -i ~/.ssh/oracle.key ubuntu@157.151.177.11 '~/hrf/live-server.sh deploy'
```

- `deploy`: stop the server, check out `origin/main` (`git checkout -f -B hrf
  origin/main`; `-f` because builds on the server modify committed `target/`
  files), then start it and wait until it prints `Started server.`
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
- Not yet set up: certificate renewal.

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
- Bot games can be run headless on the JVM with `root/host.scala` (see
  `host.xsbt` for the source exclusions); it also checks that every action
  serializes and parses back.
- Each online game has a Spectator link and one link per player. Spectator
  accounts can read the game but not add moves. A move posted by one gets a
  500 with `empty result set ... "right" = 'append'` in the server log.
- `good-game` wraps static files in `encodeResponse`, so the 8 MB client is
  sent gzipped (about 1.8 MB).
