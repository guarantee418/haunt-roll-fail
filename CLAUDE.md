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

- URL: https://games.clean5110.com/play (Let's Encrypt certificate)
- Oracle Cloud Always Free instance `hrf-server`: Ubuntu 24.04 aarch64,
  2 OCPUs, 12 GB RAM, ephemeral public IP 157.151.177.11
- DNS: A record `games` -> 157.151.177.11 under Custom Records in the
  Squarespace DNS settings for `clean5110.com`. The bare domain and `www`
  are a Squarespace website; leave the Squarespace presets alone
- SSH from the owner's Mac: `ssh -i ~/.ssh/oracle.key ubuntu@157.151.177.11`
- Checkout at `~/hrf` on a local branch `hrf`; database at
  `~/hrf/good-game-database*`; `SBT_OPTS` is set in `~/.bashrc`
- The server runs in a tmux session named `hrf`, on port 443, with a
  redirect server on port 80
- Ports 80, 443 and 7070 are open in the Oracle security list and in the
  instance's iptables (rules placed above the `REJECT` line, saved with
  `netfilter-persistent save`)

### Deploying a change

```
tmux attach -t hrf        # then Ctrl-C to stop the running server
cd ~/hrf
git fetch origin main
git checkout -f -B hrf origin/main
cd ~/hrf/good-game
sbt "run run ../good-game-database ../haunt-roll-fail https://games.clean5110.com https://games.clean5110.com/hrf/ 443"
```

- `-f` is needed because builds on the server modify committed `target/` files.
- `run create ...` (same arguments) only creates the database, then exits. It
  was already run once; don't run it again.
- The URL arguments must be the public address, not `localhost`. The server
  only serves `/hrf/` files to requests whose `Referer` starts with that URL.
- `Address already in use` on start means an old server is still running:
  `tmux kill-session -t hrf; pkill -f hrf.gg.GoodGame`, then start again in
  a new `tmux new -s hrf`.
- Not yet set up: starting automatically after a reboot.

### https

The server turns on https when `good-game/certificate.pkcs12` exists
(PKCS12, empty password), and rereads that file within a minute when it
changes. When it runs on a port other than 80 and that file or
`good-game/acme/` exists, it also starts a port 80 server that serves Let's
Encrypt challenges from `good-game/acme/` and redirects everything else to
the URL argument, keeping the path.

- Set up with `~/hrf/setup-https.sh games.clean5110.com` (arguments:
  hostname, optional email for expiry notices). It allows non-root use of
  ports 80/443 (`/etc/sysctl.d/50-hrf-ports.conf`), opens them in iptables,
  installs certbot and the renewal hook, and gets the certificate. It is safe
  to run again, for example for a new hostname; then restart the server with
  the new URL and `sudo certbot delete --cert-name <old name>`.
- Certbot renews through `certbot.timer`. Its hook
  `/etc/letsencrypt/renewal-hooks/deploy/hrf-pkcs12.sh` rewrites
  `certificate.pkcs12` for the hostname it was set up for, so no restart is
  needed.
- `certificate.pkcs12` holds the private key and is gitignored. Never commit it.
- Old `http://157.151.177.11:7070` links can be sent to the redirect server
  (only while the server is not on 7070, or it makes a redirect loop):
  `sudo iptables -t nat -A PREROUTING -p tcp --dport 7070 -j REDIRECT --to-ports 80`
  then `sudo netfilter-persistent save`.
- If the public IP changes, update the `games` A record in Squarespace.

## Gotchas

- Over plain http from a non-localhost address (not a secure context),
  `window.caches` (Cache Storage) is undefined. The live site is https now,
  but the loaders in `haunt-roll-fail/loader.scala` still fall back to
  fetching directly when it is missing. Never call
  `dom.window.caches.toOption.get` unguarded: it gives a black screen with
  `None.get` in the console.
- `good-game` wraps static files in `encodeResponse`, so the 49 MB client is
  sent gzipped (about 4.3 MB).
