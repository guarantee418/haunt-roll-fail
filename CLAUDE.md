# HRF (haunt-roll-fail) fork

Fork of the HRF board game site (hrf.im), with the Twilight Council faction
(playtest and official Root: Homeland versions). Written in Scala:

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

- URL: http://157.151.177.11:7070/play (plain http, no domain yet)
- Oracle Cloud Always Free instance `hrf-server`: Ubuntu 24.04 aarch64,
  2 OCPUs, 12 GB RAM, ephemeral public IP 157.151.177.11
- SSH from the owner's Mac: `ssh -i ~/.ssh/oracle.key ubuntu@157.151.177.11`
- Checkout at `~/hrf` on a local branch `hrf`; database at
  `~/hrf/good-game-database*`; `SBT_OPTS` is set in `~/.bashrc`
- The server runs in a tmux session named `hrf`
- Port 7070 is open in the Oracle security list and in the instance's
  iptables (rule placed above the `REJECT` line, saved with
  `netfilter-persistent save`)

### Deploying a change

```
tmux attach -t hrf        # then Ctrl-C to stop the running server
cd ~/hrf
git fetch origin main
git checkout -f -B hrf origin/main
cd ~/hrf/good-game
sbt "run run ../good-game-database ../haunt-roll-fail http://157.151.177.11:7070 http://157.151.177.11:7070/hrf/ 7070"
```

- `-f` is needed because builds on the server modify committed `target/` files.
- `run create ...` (same arguments) only creates the database, then exits. It
  was already run once; don't run it again.
- The URL arguments must be the public address, not `localhost`. The server
  only serves `/hrf/` files to requests whose `Referer` starts with that URL.
- `Address already in use` on start means an old server is still running:
  `tmux kill-session -t hrf; pkill -f hrf.gg.GoodGame`, then start again in
  a new `tmux new -s hrf`.
- Not yet set up: starting automatically after a reboot, and https.

## Gotchas

- The site is served over plain http from an IP, which is not a secure
  context, so `window.caches` (Cache Storage) is undefined. The loaders in
  `haunt-roll-fail/loader.scala` fall back to fetching directly when it is
  missing. Never call `dom.window.caches.toOption.get` unguarded: it gives a
  black screen with `None.get` in the console. Testing on `localhost` hides
  this, so test through a non-localhost address.
- `good-game` wraps static files in `encodeResponse`, so the 8 MB client is
  sent gzipped (about 1.8 MB).
