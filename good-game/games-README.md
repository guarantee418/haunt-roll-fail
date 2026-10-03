# HRF games

The games on https://games.clean5110.com, copied here by the server
(`good-game/Backup.scala` and `live-server.sh backup-sync` in
`guarantee418/haunt-roll-fail`). The server writes the files each minute and
pushes them every 5 minutes. Don't edit them by hand except in `fixes/`;
the server overwrites them.

**Keep this repository private.** `users.tsv` and `plays.tsv` hold the
secrets in every player's link: anyone who can read them can play as anyone.

## Files

Fields are separated by tabs. Tabs, newlines and backslashes inside a field
are written `\t`, `\n` and `\\`. Lines starting with `#` are headers.

- `journals.tsv`: every game (and lobby): id, name, public, status,
  message, number of entries (moves)
- `users.tsv`: id, name, secret
- `access.tsv`: game id, user id, right (`full`, `read`, `append`)
- `plays.tsv`: game id, user id, secret. That player's link is
  `https://games.clean5110.com/play/<meta>/<secret>`, where `<meta>` is the
  game (`root`, `arcs`, `nort`, ...)
- `games/<id>.tsv`: one game. The first line is `# game <id> entries <n>`,
  then one line per entry: index (from 0), user id, text (the recorded
  action, as the client writes it)

The git history keeps every earlier version, so a game can be looked at as it
was at any time.

## Fixing a game

1. Copy `games/<id>.tsv` to `fixes/<id>.tsv` (any name ending in `.tsv`
   works; not `.before.tsv`).
2. Edit the copy: delete the last lines to undo moves, or change an entry.
   Keep the first line exactly as it is: its entry count tells the server
   which version of the game you edited.
3. Commit and push to `main`.

Within about 6 minutes the server pulls the fix. If the game still has the
number of entries in the first line (nobody has moved since), it replaces the
game's whole log with the lines of the fix, in one transaction. Then it writes:

- `fixes/<name>.result`: `Applied: ...` or `Not applied: <reason>`
- `fixes/<name>.before.tsv`: the log it replaced. To undo the fix, copy that
  to a new fix (with the current first line from `games/<id>.tsv`).

Each fix file is handled once (`fixes/applied.log`, by content). To try
again, after `Not applied`, copy the game again and change the fix file
(new first line). The server never changes or deletes the files you commit;
delete old fixes whenever you like.

Players should reload the game after a fix. Until they do, a player whose
browser still has the removed moves can't add moves (the server refuses moves
that would leave a gap in the log).

## Restoring

Besides this copy, the server keeps a daily database copy for 14 days in
`~/hrf/good-game/db-backups/`. Each `.tar.gz` holds the database files
(`good-game-database.script`, `.properties`, ...): stop the server, move
the old `~/hrf/good-game-database.*` files away and unpack one in `~/hrf`
with `tar xzf`.

To rebuild a database from this repository instead (on a new server, say),
stop the server and run, in `~/hrf/good-game`:

```
sbt "run restore ../good-game-database <path to a clone of this repository>"
```

It refuses to overwrite an existing database, so move the old
`good-game-database.*` files away first.
