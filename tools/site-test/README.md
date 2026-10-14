# Site test

`site-test.js` opens the live site in Chrome (Puppeteer), loads a game's
menu, starts a Quick Game and waits until its map is drawn. It prints each
stage with its time, the public IP the browser used (to check which network
the test ran from), console errors, page errors and failed requests, and
saves a screenshot of each stage and `report.json` in `site-test-output/`.
It exits 0 when everything passed, 1 otherwise.

Needs Node 18 or newer. On a Mac without Homebrew, Node can be installed in
the home folder with nvm (no admin password):

```
curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.40.3/install.sh | bash
# close and reopen Terminal
nvm install 22
```

Run it (the first `npm install` also downloads Puppeteer's own Chrome):

```
cd tools/site-test
npm install
node site-test.js
```

Without the repository, fetch the two files on their own:

```
mkdir -p ~/site-test && cd ~/site-test
for f in package.json site-test.js; do curl -fsSLO https://raw.githubusercontent.com/guarantee418/haunt-roll-fail/main/tools/site-test/$f; done
npm install && node site-test.js
```

Options (`node site-test.js --help`):

- `--viewport 390x844` to test at phone size (default 1600x900)
- `--mode menu` to only load the menu
- `--meta <name>` for another game's menu (`/play/<name>`)
- `--url <url>` for another server, e.g. `http://localhost:7070`
- `--headful` to watch the browser
- `--timeout <seconds>` per stage (default 180)
- `--chrome <path>` to use an installed Chrome, `--proxy <url>` to send its
  traffic through a proxy (in a Claude cloud session:
  `--chrome /opt/pw-browsers/chromium-1194/chrome-linux/chrome --proxy "$HTTPS_PROXY"`)

A fresh browser profile is used each run, so Quick Game always starts a new
game and nothing saved in your own browser is touched.
