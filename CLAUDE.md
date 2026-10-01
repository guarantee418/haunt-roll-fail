# HRF (haunt-roll-fail) fork

Fork of the HRF board game site (hrf.im), with the Twilight Council faction
(playtest and official Root: Homeland versions). Written in Scala:

- `haunt-roll-fail/` — the game client, compiled to JavaScript with Scala.js
  (`target/scala-2.13/hrf-fastopt.js`, about 49 MB)
- `good-game/` — the Akka HTTP server (`GoodGame.scala`) that serves the client
  and stores games in an HSQLDB database
- `scala-js-dom-reduced/` — DOM library the client depends on
  (`sbt publishLocal` once before building the client)

## Building

```
cd scala-js-dom-reduced && sbt publishLocal
cd haunt-roll-fail && sbt fastOptJS
```

- The client build needs more than sbt's default 1 GB heap or it fails with
  `OutOfMemoryError`. Use `SBT_OPTS="-Xmx6G -Xss4M"`.
- Build output under `target/` is committed, including `hrf-fastopt.js`.
  After changing client code, rebuild and commit `hrf-fastopt.js`,
  `hrf-fastopt.js.map`, `hrf-fastopt/main.js` and `hrf-fastopt/main.js.map`
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
  `netfilter-persistent save`). Ports 80 and 443 are also open in the
  security list; `setup-https.sh` opens them in iptables

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
- Not yet set up: starting automatically after a reboot, and https (steps
  below, not run yet).

### Setting up https

The server turns on https when `good-game/certificate.pkcs12` exists
(PKCS12, empty password), and rereads that file within a minute when it
changes. When it runs on a port other than 80 and that file or
`good-game/acme/` exists, it also starts a port 80 server that serves Let's
Encrypt challenges from `good-game/acme/` and redirects everything else to
the URL argument, keeping the path. The free hostname
`157-151-177-11.sslip.io` resolves to the server's IP.

1. Done: the Oracle security list has ingress rules for TCP 80 and 443.
2. Deploy this code, then run `~/hrf/setup-https.sh` (optional
   arguments: hostname, email for expiry notices). It allows non-root use of
   ports 80/443, opens them in iptables, installs certbot and a renewal hook,
   and creates `good-game/acme/`. The first time, it stops at the port 80
   check: restart the server (still on 7070) and run it again. It then gets
   the certificate and writes `certificate.pkcs12`.
3. Restart the server on port 443:
   ```
   sbt "run run ../good-game-database ../haunt-roll-fail https://157-151-177-11.sslip.io https://157-151-177-11.sslip.io/hrf/ 443"
   ```
4. Only after that, send old `http://157.151.177.11:7070` links to the
   redirect server (doing it while the server is still on 7070 makes a
   redirect loop):
   ```
   sudo iptables -t nat -A PREROUTING -p tcp --dport 7070 -j REDIRECT --to-ports 80
   sudo netfilter-persistent save
   ```

- Certbot renews through `certbot.timer`. Its hook
  `/etc/letsencrypt/renewal-hooks/deploy/hrf-pkcs12.sh` rewrites
  `certificate.pkcs12`, so no restart is needed.
- `certificate.pkcs12` holds the private key and is gitignored. Never commit it.
- The hostname contains the IP, which is ephemeral. If the IP changes, the
  hostname changes too, and setup has to be done again for the new name.

## Gotchas

- The site is served over plain http from an IP, which is not a secure
  context, so `window.caches` (Cache Storage) is undefined. The loaders in
  `haunt-roll-fail/loader.scala` fall back to fetching directly when it is
  missing. Never call `dom.window.caches.toOption.get` unguarded: it gives a
  black screen with `None.get` in the console. Testing on `localhost` hides
  this, so test through a non-localhost address.
- `good-game` wraps static files in `encodeResponse`, so the 49 MB client is
  sent gzipped (about 4.3 MB).
