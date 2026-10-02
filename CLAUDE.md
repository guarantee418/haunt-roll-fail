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

### Deploying a change

Run from the owner's Mac. It stops the server, checks out `main` and starts
the server again in the background:

```
ssh -i ~/.ssh/oracle.key ubuntu@157.151.177.11 'tmux kill-session -t hrf; pkill -f "hrf[.]gg[.]GoodGame"; cd ~/hrf && git fetch -q origin main && git checkout -q -f -B hrf origin/main && tmux new -d -s hrf -c ~/hrf/good-game && tmux send-keys -t hrf "sbt \"run run ../good-game-database ../haunt-roll-fail https://games.clean5110.com https://games.clean5110.com/hrf/ 7070\"" Enter'
```

Check that it started (look for `Started server.`) and read its log with:

```
ssh -i ~/.ssh/oracle.key ubuntu@157.151.177.11 'tmux capture-pane -pt hrf -S -200 | tail -40'
```

- `-f` is needed because builds on the server modify committed `target/` files.
- `run create ...` (same arguments) only creates the database, then exits. It
  was already run once; don't run it again.
- The URL arguments must be the public address, not `localhost` or the IP.
  The server writes them into every page, and only serves `/hrf/` files to
  requests whose `Referer` starts with that URL. Starting it with a different
  address than people use gives a blank page or a "Loading assets" hang.
- `pkill -f "hrf[.]gg[.]GoodGame"`: the brackets stop `pkill` from matching
  the ssh command's own shell. Plain `pkill -f hrf.gg.GoodGame` is fine when
  typed on the server.
- `Address already in use` on start means an old server is still running:
  run the `tmux kill-session` and `pkill` part again, then start it again.
- Not yet set up: starting automatically after a reboot, and certificate
  renewal.

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
- The 23 Homeland deck cards in the `card/deck` assets in
  `haunt-roll-fail/root/meta.scala`, `apprentice` through `the-faithful`,
  have no images in `webp2/root/images/card/deck/`. They 404 and the
  cards show without pictures.
- Each online game has a Spectator link and one link per player. Spectator
  accounts can read the game but not add moves. A move posted by one gets a
  500 with `empty result set ... "right" = 'append'` in the server log.
- `good-game` wraps static files in `encodeResponse`, so the 8 MB client is
  sent gzipped (about 1.8 MB).
