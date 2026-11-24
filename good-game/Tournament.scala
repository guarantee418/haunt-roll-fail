package hrf.gg

import slick.jdbc.HsqldbProfile.api._

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path, Paths}

import scala.concurrent.Await
import scala.concurrent.duration.Duration

// The Root tournament (asked for on 2026-11-20). Players sign up with a ranking name (Ranking.scala) on the Tournament page
// (/play/root/tournament); the 16th sign-up starts it: the server shuffles the players into four tables of four and creates
// their games itself, Advanced Setup games (root-adset) with the tournament draft (TournamentDraft in root/tournament.scala).
// Each table's winner, as the game reports it to the rankings (/report-result), goes to the final, created as soon as the four
// winners are known, and the final's winner is the champion, whose name is drawn in gold until the next tournament has one.
// Sign-ups for the next tournament open as soon as a final is created.
//
// The options of the games come from the clients: each sign-up sends the version and options line its client would give a
// tournament game (the latest kept in tournament/template.tsv), so the server never has to know the game's options.
//
// Files, in tournament/ in the server's working directory (copied to the games backup by Backup):
//   events.tsv    time, event, tournament number, then the event's fields:
//                   signup <name> <key hash>, withdraw <name>,
//                   table <id> <names> <lobby> <game> <spectator secret> <users> <secrets> (comma-separated, in seat order),
//                   winner <table id> <name>, replace <table id> <old name> <new name>, champion <name>, cancel
//                 names in lower case, as Ranking keeps them; the final is table "F"
//   template.tsv  version, options line
//   invited.tsv   the tournaments whose sign-ups were announced on Discord (time, number), so each invitation goes out once
//   threads.tsv   the Discord threads made for the posts (key, thread id): "<n>" for a tournament, "<n> <table>" for a table
//   discord.tsv   the Discord user ID or username each name's owner gave (name, value, and the user ID a username was found as by
//                 the Discord bot, DiscordBot.scala; the last row counts); pinged by the ID, a username without one is only shown
//
// Changes by hand go through the games backup (hrf-games) like the ranking's: each file tournament-changes/<name>.txt there is
// applied once (Backup.scala), its outcome in tournament-changes/<name>.result. One change a line:
//   remove <name>                     takes a name off the sign-ups
//   winner <table> <name>             the table's winner (a shared win, or a table that won't finish), A to D or F
//   replace <table> <name> -> <new>   a player at a table is replaced: the seat now plays (and is ranked) under the new name
//   cancel                            the running tournament ends with no champion; sign-ups for the next one open
//   webhook <url>                     the Discord webhook (written to the file discord-webhook; "webhook off" removes it);
//                                     delete the change file once its .result says "set"; forgets the threads made
//   forum <url>                       a forum channel's webhook for the tables' threads (the file discord-forum-webhook;
//                                     "forum off" removes it); delete the change file once its .result says "set"
//   invite                            posts the invitation to sign up again
//   turns <url>                       the webhook of the channel for turn pings and new games (TurnPings.scala; the file
//                                     discord-turns-webhook, "turns off" removes it); delete the change file once its .result says "set"
//   nort-turns <url>                  the same for Northgard's games (the file discord-nort-turns-webhook, "nort-turns off")
//   pincer-turns <url>                the same for Pincer's (discord-pincer-turns-webhook) and Company of Heroes' (tactics-turns,
//   tactics-turns <url>               discord-tactics-turns-webhook); the bot's make-webhook change can make and set them itself
//   bot <token>                       the Discord bot's token, so a username given on the Tournament page pings (DiscordBot.scala;
//                                     the file discord-bot-token, "bot off" removes it); delete the change file once its .result is read
//   guild <id>                        the Discord server the bot looks usernames up in, if not the webhook's (the file discord-guild)
//   ping waiting                      pings every seat the games wait for at once (TurnPings.pingWaiting)
//   status                            posts the tournament's tables to the announcements' webhook: each table's round, its players'
//                                     factions and scores, whose turn it is, or its winner, and its spectator link (Tournament.status)
//   ping waiting <game> ...           pings the seats those games wait for (their server ids, the "server" line of their
//                                     lobbies), saying for each who was pinged and why anyone wasn't (TurnPings.pingWaitingIn)
//   pings <game> ...                  what the server knows of those games' turn pings: each turn report's row (log index, seats
//                                     waited for, time) and each ping sent, last 40 lines (TurnPings.history), to check a missed ping
//   lfg-key <public key>              the Discord application's public key, for /root, /northgard and the LFG posts' buttons (Lfg.scala;
//                                     the file discord-public-key, "lfg-key off" removes it); registers the commands in the Discord server
//   lfg-commands                      registers /root and /northgard again
//   emojis                            lists the Discord server's emojis and the LFG options and sections each is shown for
//   discord <name> -> <username>      a name's Discord, for a player who didn't give it (a username the bot finds, or a user ID;
//                                     "-> off" forgets it)
//   roles, role, rename-role, give-role, take-role, channels, category, channel, rename-channel, move-channel, order
//                                     the Discord server's roles and channels, through the bot (DiscordServer.scala has the forms)
//   self-roles, self-role, self-role-off, role-picker
//                                     the roles members may pick for themselves with /roles or the role picker post (SelfRoles.scala)
// The Discord channel of the webhook in HRF_DISCORD_WEBHOOK (or the file discord-webhook) is invited to sign up, with the link to
// the Tournament page, once for each tournament as soon as its sign-ups open and a webhook is set (at the server's start, when the
// webhook is set, when a final starts or a tournament is cancelled; asked for on 2026-11-20, as nobody could find the page), told
// how the sign-ups go (8, 12 and 15 of 16), and told when a tournament starts, each table is won, the final starts and the champion
// is known. Each table (the final too) gets a Discord thread of its own (asked for on 2026-11-20), opened at its start with its
// players, pinged by the Discord user ID they gave on the Tournament page (/tournament/discord), in the forum channel of the
// webhook in HRF_DISCORD_FORUM_WEBHOOK (or the file discord-forum-webhook), else in the announcements' channel. A forum channel
// gets every post in a thread (each tournament has one for the rest); a text channel, which takes no thread, in the channel itself.
object Tournament {
    import GoodGame._
    import Backup.{rows, row}

    val size = 16
    val seats = 4
    val meta = "root-adset"
    val heatIds = List("A", "B", "C", "D")
    val finalId = "F"

    val dir = Paths.get("tournament")
    val events = dir.resolve("events.tsv")
    val template = dir.resolve("template.tsv")
    val invited = dir.resolve("invited.tsv")

    var db : Database = null

    // the site's address (the server's URL argument), for the sign-up link
    var site = ""

    def signUpLink = site + "/play/root/tournament"

    def execute[E <: Effect](actions : DBIOAction[_, NoStream, E]*) = Await.result(db.run(DBIO.seq(actions : _*).withPinnedSession), Duration.Inf)

    def start(database : Database, url : String) {
        db = database
        site = url.stripSuffix("/")

        try invite(false) catch { case e : Exception => println("Tournament invitation not posted: " + e) }
    }

    // DISCORD: the invitation to sign up for the tournament taking sign-ups, once for each (again with again, the "invite" change)
    def invite(again : Boolean) : String = synchronized {
        val cc = cups()

        openNumber(cc) match {
            case None => "not done: no tournament taking sign-ups"
            case Some(_) if webhook.isEmpty => "not done: no webhook"
            case Some(n) if !again && rows(invited).exists(_.lift(1).contains(n.toString)) => "already invited"
            case Some(n) =>
                val count = cc.find(_.n == n).map(_.signups.size).getOrElse(0)

                announce("**Root Tournament " + n + ": sign-ups are open!**\n" +
                    size + " players, four tables of " + seats + ", each an Advanced Setup game with the tournament draft: the first seat picks the deck, the second the map, and the factions are drafted from a flop of five (official factions, no Vagabond). " +
                    "Each table's winner goes to the final, and the champion's name is golden on the Rankings until the next champion. Every game counts toward the rankings.\n" +
                    "**Sign up with your ranking name:** " + signUpLink + "\n" +
                    count + " of " + size + " signed up so far. It starts by itself when the " + size + "th player signs up.", n)

                Files.createDirectories(dir)
                Ranking.append(invited, row(Ranking.now(), n))

                "invited"
        }
    }

    def newSecret(n : Int) = {
        val random = new java.security.SecureRandom()
        0.until(n).map(_ => "abcdefghijklmnopqrstuvwxyz".charAt(random.nextInt(26))).mkString("")
    }

    case class Table(id : String, names : List[String], lobby : String, game : String, spectate : String, users : List[String], secrets : List[String], winner : Option[String]) {
        def title(n : Int) = "Tournament " + n + (if (id == finalId) " Final" else " Table " + id)
    }

    case class Cup(n : Int, signups : List[(String, String)], tables : List[Table], champion : Option[String], cancelled : Boolean) {
        def started = tables.nonEmpty
        def heats = tables.filter(_.id != finalId)
        def fin = tables.find(_.id == finalId)
        def done = champion.nonEmpty || cancelled
    }

    def cups() : List[Cup] = {
        var m = Map[Int, Cup]()

        def cup(n : Int) = m.getOrElse(n, Cup(n, Nil, Nil, None, false))

        def table(c : Cup, id : String)(f : Table => Table) = c.copy(tables = c.tables.map(t => if (t.id == id) f(t) else t))

        rows(events).foreach {
            case _ :: kind :: num :: rest if num.forall(_.isDigit) && num.nonEmpty =>
                val n = num.toInt
                val c = cup(n)
                val next = (kind, rest) match {
                    case ("signup", name :: hash :: _) if !c.signups.exists(_._1 == name) => c.copy(signups = c.signups :+ (name -> hash))
                    case ("withdraw", name :: _) => c.copy(signups = c.signups.filter(_._1 != name))
                    case ("table", id :: names :: lobby :: game :: spectate :: users :: secrets :: _) =>
                        c.copy(tables = c.tables.filter(_.id != id) :+ Table(id, names.split(',').toList, lobby, game, spectate, users.split(',').toList, secrets.split(',').toList, None))
                    case ("winner", id :: name :: _) => table(c, id)(_.copy(winner = Some(name)))
                    case ("replace", id :: from :: to :: _) => table(c, id)(t => t.copy(names = t.names.map(x => if (x == from) to else x)))
                    case ("champion", name :: _) => c.copy(champion = Some(name))
                    case ("cancel", _) => c.copy(cancelled = true)
                    case _ => c
                }
                m += n -> next
            case _ =>
        }

        m.values.toList.sortBy(_.n)
    }

    def event(kind : String, n : Int, fields : Any*) = Ranking.append(events, row((List[Any](Ranking.now(), kind, n) ++ fields) : _*))

    // the tournament taking sign-ups: the first one, or the one after the last, once that one has its final (or has ended)
    def openNumber(cc : List[Cup]) : Option[Int] = cc.lastOption match {
        case None => Some(1)
        case Some(c) if !c.started && !c.cancelled => Some(c.n)
        case Some(c) if c.fin.nonEmpty || c.done => Some(c.n + 1)
        case _ => None
    }

    // the champion of the last tournament won: the golden name
    def champion(cc : List[Cup]) : Option[String] = cc.flatMap(_.champion).lastOption

    def display(k : String) = Ranking.displays().getOrElse(k, k)

    // what the Tournament page shows, a line each, fields separated by tabs
    def state() : String = synchronized {
        val cc = cups()
        val dd = Ranking.displays()
        def d(k : String) = dd.getOrElse(k, k)

        val open = openNumber(cc).map { n =>
            val c = cc.find(_.n == n).getOrElse(Cup(n, Nil, Nil, None, false))
            List(row("open", n, c.signups.size, size)) ++ c.signups.map(s => row("signup", n, d(s._1)))
        }.getOrElse(List(row("closed")))

        val running = cc.filter(c => c.started).flatMap { c =>
            c.tables.map(t => row((List[Any]("table", c.n, t.id, t.spectate, t.winner.map(d).getOrElse("")) ++ t.names.map(d)) : _*)) ++
            c.champion.map(x => row("champion", c.n, d(x))).toList ++
            (if (c.cancelled) List(row("cancelled", c.n)) else Nil)
        }

        (List(row("size", size, seats)) ++ champion(cc).map(x => row("golden", d(x))).toList ++ open ++ running).mkString("\n")
    }

    // the names a player code holds, in lower case
    def ownedKeys(key : String) = {
        val h = Ranking.sha(key)
        Ranking.claims().values.filter(_.hashes.contains(h)).map(_.key).toSet
    }

    // a sign-up with a name the code holds, or a free name it then claims
    def signUp(key : String, name : String, version : String, options : String) : String = synchronized {
        val k = Ranking.clean(name).toLowerCase

        if (k.isEmpty || key.length < 16)
            return "invalid"

        if (!options.split(' ').contains("TournamentDraft") || version.isEmpty)
            return "invalid"

        val cc = cups()

        val n = openNumber(cc) match {
            case Some(n) => n
            case None => return "closed"
        }

        val c = cc.find(_.n == n).getOrElse(Cup(n, Nil, Nil, None, false))

        if (c.signups.exists(_._1 == k))
            return "already"

        // one sign-up a browser (a player code): the same person can't sit at two seats
        if (c.signups.exists(_._2 == Ranking.sha(key)))
            return "one"

        // playing at a table of a tournament still running
        if (cc.filter(x => x.started && !x.done).exists(x => x.tables.exists(t => t.winner.isEmpty && t.names.contains(k))))
            return "playing"

        val claimed = Ranking.claimName(name, key)

        if (!claimed.startsWith("ok"))
            return claimed

        Files.createDirectories(dir)
        Files.write(template, (row(version.take(32), options.take(20000)) + "\n").getBytes(UTF_8))

        event("signup", n, k, Ranking.sha(key))

        val count = c.signups.size + 1

        if (count >= size)
            begin(n)
        else
        if (count == size / 2 || count == size * 3 / 4 || count == size - 1)
            announce(display(k) + " signed up for Root Tournament " + n + ": " + count + " of " + size + (if (count == size - 1) ", one spot left!" else ".") + " Sign up: " + signUpLink, n)

        "ok"
    }

    def withdraw(key : String, name : String) : String = synchronized {
        val k = Ranking.clean(name).toLowerCase
        val cc = cups()

        openNumber(cc).flatMap(n => cc.find(_.n == n)) match {
            case Some(c) if c.signups.exists(_._1 == k) && ownedKeys(key).contains(k) =>
                event("withdraw", c.n, k)
                "ok"
            case _ => "no"
        }
    }

    // the games of the tournaments still running at the tables of the names a code holds: tournament, table, title, player link
    def mine(key : String) : String = synchronized {
        val kk = ownedKeys(key)

        // the Discord each of the code's names gave: "discord", name, value, "ping" when it has a user ID to ping
        val dd = kk.toList.sorted.flatMap(k => discordOf(k).map(v => row("discord", display(k), v, if (storedId(k).nonEmpty) "ping" else "")))

        cups().filter(c => c.started && !c.done).flatMap { c =>
            c.tables.filter(_.winner.isEmpty).flatMap { t =>
                t.names.zipWithIndex.filter(x => kk.contains(x._1)).map { case (k, i) =>
                    // the seventh field: "1" while the game waits on this seat (Turns.scala, the lobby's "user <faction> <user>"; 2026-11-21)
                    val turn = TurnPings.lobbyOf(t.game).flatMap(_.lines.map(_.split(" ").toList).collectFirst { case "user" :: f :: u :: Nil if u == t.users(i) => f })
                        .exists(f => Turns.asked(t.game).contains(f)) && !TurnPings.over(t.game)
                    // the eighth: the game's map and factions as icons, as a client reported them (Turns.overview; 2026-11-22)
                    row(c.n, t.id, t.title(c.n), t.secrets(i), display(k), t.names.filter(_ != k).map(display).mkString(", "), if (turn) "1" else "0", Turns.overview(t.game).mkString(" "))
                }
            }
        }.++(dd).mkString("\n")
    }

    // STATUS POSTS (asked for on 2026-11-22, the "status" change): the running tournament's tables (else the last one's), each with its
    // round, every player's faction and score (Turns.status, from the clients' turn reports), whose turn it is (Turns.asked) or its winner,
    // and its spectator link, posted to the announcements' webhook
    def status() : String = {
        val cc = cups().filter(_.started)
        cc.filterNot(_.done).lastOption.orElse(cc.lastOption) match {
            case None => "not done: no tournament has started"
            case Some(c) =>
                def seatName(t : Table, seat : String) = seat.stripPrefix("P").toIntOption.flatMap(i => t.names.lift(i - 1)).map(display).getOrElse(seat)
                val parts = c.tables.map { t =>
                    val head = "**" + t.title(c.n).replace("Tournament " + c.n + " ", "") + "**"
                    val watch = "Watch: <" + site + "/play/" + meta + "/" + t.spectate + ">"
                    val ss = Turns.status(t.game)
                    val players = ss.drop(1).map(_.split(":").toList).collect { case seat :: faction :: score :: _ =>
                        seatName(t, seat) + " (" + faction.replace('_', ' ') + ") " + (if (score == "D") "dominance" else score + " VP") }
                    val round = ss.headOption.filter(_.startsWith("r")).flatMap(_.drop(1).toIntOption)
                    val waiting = Turns.asked(t.game).map(s => seatName(t, s)).distinct
                    val state = t.winner.map(w => "won by **" + display(w) + "**").getOrElse {
                        round match {
                            case None => "no progress reported yet" + (if (waiting.nonEmpty) ", waiting on " + waiting.mkString(", ") else "")
                            case Some(0) => "drafting" + (if (waiting.nonEmpty) ", waiting on " + waiting.mkString(", ") else "")
                            case Some(r) => "round " + r + (if (waiting.nonEmpty) ", waiting on " + waiting.mkString(", ") else "")
                        }
                    }
                    head + ": " + state + "\n" + (if (players.nonEmpty) players else t.names.map(display)).mkString(" · ") + "\n" + watch
                }
                announce("**Root Tournament " + c.n + "** status\n\n" + parts.mkString("\n\n"), c.n)
                "posted " + c.tables.size + " tables"
        }
    }

    // a table's own Discord thread, started with its players (mentioned when they gave their Discord user ID) and the spectator link
    def tableThread(n : Int, t : Table) {
        announce("**" + t.title(n).replace("Tournament", "Root Tournament") + "**: " + t.names.map(tagged).mkString(", ") + "\n" +
            "Your game is at the top of Play Online, or on the Tournament page: " + signUpLink + "\n" +
            "Watch it: " + site + "/play/" + meta + "/" + t.spectate + "\n" +
            "This thread is for the table: arrange times and talk about the game here.", n, t.id)
    }

    // the 16th sign-up: four tables of four, in random order
    def begin(n : Int) {
        val c = cups().find(_.n == n).get
        val names = scala.util.Random.shuffle(c.signups.map(_._1)).take(size)

        heatIds.zip(names.grouped(seats).toList).foreach { case (id, l) => create(n, id, l) }

        val dd = Ranking.displays()
        announce("**Root Tournament " + n + " has started!** Find your game at the top of Play Online, or on the Tournament page.\n" +
            heatIds.zip(names.grouped(seats).toList).map { case (id, l) => "Table " + id + ": " + l.map(k => dd.getOrElse(k, k)).mkString(", ") }.mkString("\n"), n)

        cups().find(_.n == n).toList.flatMap(_.heats).foreach(t => tableThread(n, t))
    }

    // a table's game, made as the client makes an online game (arrangeOnlineGame in hrf.scala): a host, a lobby, the game and a
    // seat (and a link) for each player and a spectator; the players' names are written to the lobby and their seats count under
    // those names in the rankings
    def create(n : Int, id : String, names : List[String]) {
        val (version, options) = rows(template).headOption match {
            case Some(v :: o :: _) => (v, o)
            case _ => throw new Error("no tournament game template")
        }

        val title = Table(id, Nil, "", "", "", Nil, Nil, None).title(n)
        val dd = Ranking.displays()

        val host = User(title + " HOST", newSecret(16), newSecret(16))
        val lobby = newSecret(16)
        val game = newSecret(16)
        val spectator = User("Spectator", newSecret(16), newSecret(16))
        val spectate = newSecret(16)
        val players = names.indices.map(i => (User("P" + (i + 1), newSecret(16), newSecret(16)), newSecret(16))).toList

        val intro =
            List("meta " + meta, "version " + version, "title " + title) ++
            players.zipWithIndex.map { case ((u, _), i) => "user P" + (i + 1) + " " + u.id } ++
            List("seating " + players.indices.map(i => "P" + (i + 1)).mkString(" "), "options " + options, "server " + game)

        val named = players.zip(names).map { case ((u, _), k) => (u.id, "name " + u.id + " " + dd.getOrElse(k, k)) }

        def rights(j : String, u : String, rr : String*) = rr.map(r => AccessRight(j, u, r))

        execute(
            users += host,
            journals += Journal(title + " LOBBY", false, "", "", lobby),
            journals += Journal(title, false, "", "", game),
            accessRights ++= rights(lobby, host.id, "full", "read", "append") ++ rights(game, host.id, "full", "read", "append"),
            users += spectator,
            accessRights ++= rights(lobby, spectator.id, "read", "append") ++ rights(game, spectator.id, "read"),
            plays += Play(lobby, spectator.id, spectate),
            users ++= players.map(_._1),
            accessRights ++= players.flatMap { case (u, _) => rights(lobby, u.id, "read", "append") ++ rights(game, u.id, "read", "append") },
            plays ++= players.map { case (u, s) => Play(lobby, u.id, s) },
            entries ++= (intro.map(t => (host.id, t)) ++ named).zipWithIndex.map { case ((u, t), i) => Entry(lobby, i, u, t) },
            entries += Entry(game, 0, host.id, "StartAction(\"" + version + "\")")
        )

        players.zip(names).foreach { case ((u, _), k) => Ranking.append(Ranking.seats, row(u.id, k)) }

        event("table", n, id, names.mkString(","), lobby, game, spectate, players.map(_._1.id).mkString(","), players.map(_._2).mkString(","))
    }

    // a finished game's result (Ranking.report): a table's single winner goes on; the final's winner is the champion
    def reported(game : String, sides : String) : Unit = synchronized {
        val cc = cups()

        cc.filter(c => c.started && !c.done).foreach { c =>
            c.tables.find(t => t.game == game && t.winner.isEmpty).foreach { t =>
                val won = sides.split(';').toList.filter(_.startsWith("W:")).flatMap(_.drop(2).split(',').toList).map(_.takeWhile(_ != '/')).filter(_.nonEmpty)
                val ww = won.flatMap(u => t.users.indexOf(u) match { case -1 => None ; case i => Some(t.names(i)) }).distinct

                ww match {
                    case w :: Nil => win(c.n, t.id, w)
                    case _ =>
                        val text = "Root Tournament " + c.n + ", " + t.title(c.n).split(' ').drop(2).mkString(" ") + " ended with " + (if (ww.isEmpty) "no winner" else "a shared win (" + ww.map(display).mkString(", ") + ")") + ": the organizer picks who goes on."
                        announce(text, c.n)
                        announce(text, c.n, t.id)
                }
            }
        }
    }

    def win(n : Int, id : String, k : String) {
        event("winner", n, id, k)

        val c = cups().find(_.n == n).get

        if (id == finalId) {
            event("champion", n, k)
            announce("**" + display(k) + " is the champion of Root Tournament " + n + "!**", n)
            announce("**" + display(k) + " won the final and is the champion!**", n, id)
        }
        else {
            announce(display(k) + " won Table " + id + " of Root Tournament " + n + ".", n)
            announce("**" + display(k) + " won this table** and goes to the final.", n, id)

            if (c.heats.size == heatIds.size && c.heats.forall(_.winner.nonEmpty) && c.fin.isEmpty) {
                val l = scala.util.Random.shuffle(c.heats.flatMap(_.winner))
                create(n, finalId, l)
                announce("**The final of Root Tournament " + n + " has started:** " + l.map(display).mkString(", ") + ". Sign-ups for Tournament " + (n + 1) + " are open.", n)
                cups().find(_.n == n).flatMap(_.fin).foreach(t => tableThread(n, t))
                invite(false)
            }
        }
    }

    // the changes of a tournament-changes file, one a line; the outcome of each
    def change(text : String) : List[String] = synchronized {
        text.split('\n').toList.map(_.trim).filter(l => l.nonEmpty && !l.startsWith("#")).map { line =>
            val (word, rest) = (line.takeWhile(_ != ' '), line.dropWhile(_ != ' ').trim)
            val cc = cups()
            val running = cc.filter(c => c.started && !c.done).lastOption
            def key(s : String) = Ranking.clean(s).toLowerCase

            val result = word match {
                case "remove" =>
                    openNumber(cc).flatMap(n => cc.find(_.n == n)).filter(_.signups.exists(_._1 == key(rest))) match {
                        case Some(c) => event("withdraw", c.n, key(rest)); "removed"
                        case None => "not done: not signed up"
                    }
                case "winner" =>
                    val (id, name) = (rest.takeWhile(_ != ' ').toUpperCase, key(rest.dropWhile(_ != ' ')))
                    running.flatMap(c => c.tables.find(_.id == id).map(t => (c, t))) match {
                        case Some((c, t)) if t.winner.isEmpty && t.names.contains(name) => win(c.n, id, name); "done"
                        case Some((_, t)) if t.winner.nonEmpty => "not done: the table has a winner"
                        case Some(_) => "not done: not at that table"
                        case None => "not done: no such table"
                    }
                case "replace" =>
                    val id = rest.takeWhile(_ != ' ').toUpperCase
                    rest.dropWhile(_ != ' ').split(" -> ").toList.map(key) match {
                        case from :: to :: Nil if to.nonEmpty =>
                            running.flatMap(c => c.tables.find(_.id == id).map(t => (c, t))) match {
                                case Some((c, t)) if t.names.contains(from) && !t.names.contains(to) =>
                                    event("replace", c.n, id, from, to)
                                    Ranking.append(Ranking.seats, row(t.users(t.names.indexOf(from)), to))
                                    "replaced"
                                case _ => "not done: no such player at that table"
                            }
                        case _ => "not done: replace <table> <name> -> <new name>"
                    }
                case "cancel" =>
                    running match {
                        case Some(c) => event("cancel", c.n); announce("Root Tournament " + c.n + " was cancelled.", c.n); invite(false); "cancelled"
                        case None => "not done: no tournament running"
                    }
                // the Discord webhook, set from the games backup as no session can reach the server; written to the file
                // discord-webhook, and the URL is never written back in the .result
                case "webhook" =>
                    if (rest.startsWith("https://discord.com/api/webhooks/") || rest.startsWith("https://discordapp.com/api/webhooks/")) {
                        Files.write(Paths.get("discord-webhook"), (rest + "\n").getBytes(UTF_8))
                        Files.deleteIfExists(threads)
                        plainChannels = Set()
                        announce("The Root tournament's announcements will be posted here.")
                        invite(false)
                        "set"
                    }
                    else if (rest == "off") {
                        Files.deleteIfExists(Paths.get("discord-webhook"))
                        "removed"
                    }
                    else
                        "not done: webhook <https://discord.com/api/webhooks/...> or webhook off"
                // the forum channel's webhook for the tables' threads (the file discord-forum-webhook); "forum off" sends them
                // to the announcements' webhook again
                case "forum" =>
                    if (rest.startsWith("https://discord.com/api/webhooks/") || rest.startsWith("https://discordapp.com/api/webhooks/")) {
                        Files.write(Paths.get("discord-forum-webhook"), (rest + "\n").getBytes(UTF_8))
                        Files.deleteIfExists(threads)
                        plainChannels = Set()
                        "set"
                    }
                    else if (rest == "off") {
                        Files.deleteIfExists(Paths.get("discord-forum-webhook"))
                        "removed"
                    }
                    else
                        "not done: forum <https://discord.com/api/webhooks/...> or forum off"
                case "invite" => invite(true)
                // the channel (a forum: a post for each game) for the turn pings and new games (TurnPings.scala; 2026-11-20), Northgard's
                // (2026-11-21), Pincer's and Company of Heroes' (2026-11-24)
                case w if TurnPings.slot(w).nonEmpty => TurnPings.setHook(TurnPings.slot(w).get, rest)
                // the Discord bot's token, so usernames can be pinged (DiscordBot.scala; the file discord-bot-token, never written
                // back in the .result); "bot off" removes it
                case "bot" =>
                    if (rest == "off") {
                        Files.deleteIfExists(Paths.get("discord-bot-token"))
                        DiscordBot.found.clear()
                        "removed"
                    }
                    else if (rest.length >= 50 && !rest.contains(' ') && rest.count(_ == '.') == 2) {
                        Files.write(Paths.get("discord-bot-token"), (rest + "\n").getBytes(UTF_8))
                        DiscordBot.found.clear()
                        DiscordBot.missing.clear()
                        DiscordBot.guild match {
                            case None => "set, but no Discord server: set a webhook, or the change guild <server id>"
                            case Some(g) =>
                                try DiscordBot.get("https://discord.com/api/v10/guilds/" + g, Some(rest)) match {
                                    case Some(_) => "set"
                                    case None => "set, but Discord refused it: check the token, and that the bot was invited to the server"
                                }
                                catch { case e : Exception => "set, but Discord couldn't be reached: " + e.getClass.getSimpleName }
                        }
                    }
                    else
                        "not done: bot <the bot's token from the Developer Portal> or bot off"
                // the Discord server the bot looks usernames up in, when it isn't the webhook's (the file discord-guild)
                case "guild" =>
                    if (rest == "off") { Files.deleteIfExists(Paths.get("discord-guild")) ; "removed" }
                    else if (isDiscordId(rest)) { Files.write(Paths.get("discord-guild"), (rest + "\n").getBytes(UTF_8)) ; DiscordBot.found.clear() ; "set" }
                    else "not done: guild <the server's ID> or guild off"
                // the Discord application's public key, for the LFG posts' slash command and buttons (Lfg.scala; the file
                // discord-public-key), and /root registered in the Discord server; "lfg-key off" removes it
                case "lfg-key" =>
                    if (rest == "off") { Files.deleteIfExists(Paths.get("discord-public-key")) ; "removed" }
                    else if (rest.length == 64 && rest.forall(c => "0123456789abcdefABCDEF".contains(c))) {
                        Files.write(Paths.get("discord-public-key"), (rest.toLowerCase + "\n").getBytes(UTF_8))
                        "set; " + Lfg.commands + " " + Lfg.register()
                    }
                    else "not done: lfg-key <the application's public key from the Developer Portal> or lfg-key off"
                // the server's emojis and what the LFG posts show each for (LfgSetup.serverEmojiUse; 2026-11-21)
                case "emojis" => LfgSetup.serverEmojiUse()
                // /root registered again (after the bot's token or the server changed)
                case "lfg-commands" => Lfg.syncClanEmojis() + "; " + Lfg.commands + " " + Lfg.register()
                // a name's Discord, set by the owner for a player who didn't give it (2026-11-20, matched from the server's member
                // list): discord <name> -> <username or user ID>, looked up by the Discord bot like one given on the Tournament page;
                // discord <name> -> off forgets it
                case "discord" =>
                    rest.split(" -> ").toList.map(_.trim) match {
                        case name :: value :: Nil if name.nonEmpty =>
                            val k = key(name)
                            val v = value.stripPrefix("<@").stripPrefix("@").stripSuffix(">").trim
                            if (!Ranking.displays().contains(k))
                                "not done: no such ranking name"
                            else if (v == "off") {
                                Files.createDirectories(dir)
                                Ranking.append(discords, row(k, ""))
                                "forgotten"
                            }
                            else if (isDiscordId(v)) {
                                Files.createDirectories(dir)
                                Ranking.append(discords, row(k, v))
                                "set"
                            }
                            else if (!isUsername(v))
                                "not done: not a Discord username or user ID"
                            else if (!DiscordBot.configured)
                                "not done: no Discord bot (the bot change) to look the username up"
                            else DiscordBot.lookup(v) match {
                                case DiscordBot.Found(id) =>
                                    Files.createDirectories(dir)
                                    Ranking.append(discords, row(k, DiscordBot.cleanName(v), id))
                                    "set (found)"
                                case DiscordBot.NotFound => "not done: no member of the Discord server has that name"
                                case DiscordBot.Unavailable => "not done: Discord couldn't be asked; try again"
                            }
                        case _ => "not done: discord <name> -> <username or user ID>, or -> off"
                    }
                // every seat the games wait for, pinged at once (TurnPings.pingWaiting; 2026-11-20)
                case "ping" if rest == "waiting" => TurnPings.pingWaiting() + " pinged"
                // every table's progress and spectator link, posted (2026-11-22)
                case "status" => status()
                // the claimed ranking names with no Discord given, and those whose Discord is a username no user ID was found for, so
                // nobody can be pinged (2026-11-22; Play Online asks those players once, discordAsk in hrf.scala)
                case "no-discord" => noDiscord()
                // only those games (2026-11-21)
                case "pings" => TurnPings.history(rest.split("[ ,]+").toList.filter(_.nonEmpty)).mkString("\n")
                case "ping" if rest.startsWith("waiting ") => TurnPings.pingWaitingIn(rest.drop(8).split("[ ,]+").toList.filter(_.nonEmpty)).mkString("; ")
                // the Discord server's roles and channels, through the bot (DiscordServer.scala; 2026-11-24)
                case w if DiscordServer.words(w) => DiscordServer.change(w, rest)
                // the roles members pick for themselves, and the role picker post (SelfRoles.scala; 2026-11-24)
                case w if SelfRoles.words(w) => SelfRoles.change(w, rest)
                case _ => "not done: not a change (remove, winner, replace, cancel, webhook, forum, turns, nort-turns, pincer-turns, tactics-turns, bot, guild, discord, ping waiting, pings, status, no-discord, lfg-key, lfg-commands, invite, or the server changes roles, role, rename-role, give-role, take-role, channels, category, channel, rename-channel, move-channel, order, webhooks, make-webhook, rename-webhook, test-ping, self-roles, self-role, self-role-off and role-picker)"
            }

            (if (word == "webhook" || word == "forum" || TurnPings.slot(word).nonEmpty || word == "bot") word else line) + ": " + result
        }
    }

    // the names with no Discord to ping, each with how many seats play under it, the most played first
    def noDiscord() : String = {
        val claimed = Ranking.claims().keySet
        val seats = Ranking.seatsOf(claimed).groupBy(_._2).view.mapValues(_.size).toMap
        def line(k : String) = display(k) + " (" + seats.getOrElse(k, 0) + " seats)"
        val none = claimed.toList.filter(k => discordOf(k).isEmpty).sortBy(k => (-seats.getOrElse(k, 0), k))
        val noId = claimed.toList.filter(k => discordOf(k).nonEmpty && storedId(k).isEmpty && discordId(k).isEmpty).sortBy(k => (-seats.getOrElse(k, 0), k))
        (none.size + " of " + claimed.size + " names have no Discord" :: none.map("  " + line(_))) ++
            (if (noId.isEmpty) Nil else (noId.size + " gave a username no member of the server has, so they can't be pinged" :: noId.map(k => "  " + line(k) + ": " + discordOf(k).getOrElse(""))))
    }.mkString("\n")

    // DISCORD: a message to the channel of the webhook, if one is set up
    def hook(env : String, file : String) : Option[String] =
        sys.env.get(env).map(_.trim).filter(_.nonEmpty).orElse {
            val f = Paths.get(file)
            if (Files.exists(f)) Some(new String(Files.readAllBytes(f), UTF_8).trim).filter(_.nonEmpty) else None
        }

    // the announcements' webhook
    def webhook = hook("HRF_DISCORD_WEBHOOK", "discord-webhook")

    // a forum channel's webhook for the tables' threads, if they go to a channel of their own (2026-11-20)
    def forumWebhook = hook("HRF_DISCORD_FORUM_WEBHOOK", "discord-forum-webhook")

    lazy val client = java.net.http.HttpClient.newHttpClient()


    // DISCORD THREADS (asked for on 2026-11-20): with the webhook in a forum channel, each tournament has its thread ("Root Tournament
    // <n>": the invitation, the sign-ups, the start, the winners, the champion), and each table its own ("Root Tournament <n> · Table A",
    // "· Final"), started with its players, @mentioned when they gave their Discord user ID, and told its winner. A webhook posts in a
    // forum only with a thread_name (a new thread) or a thread_id; a thread's id is the channel_id of its first message, kept in
    // threads.tsv. In a plain text channel the thread fields are refused, so everything goes to the channel as before.
    val threads = dir.resolve("threads.tsv")
    val discords = dir.resolve("discord.tsv")

    def threadKey(n : Int, id : String = "") = if (id == "") n.toString else n + " " + id
    def threadName(n : Int, id : String = "") = "Root Tournament " + n + (if (id == "") "" else if (id == finalId) " · Final" else " · Table " + id)

    def threadOf(key : String, file : Path = threads) : Option[String] = rows(file).filter(_.headOption.contains(key)).lastOption.flatMap(_.lift(1))

    // the posts go out one at a time, in order, so a thread exists before the next post to it
    lazy val poster = java.util.concurrent.Executors.newSingleThreadExecutor()

    // the Discord user ID (a mention) or name (plain text) a player gave for a ranking name
    def discordOf(k : String) : Option[String] = rows(discords).filter(_.headOption.contains(k)).lastOption.flatMap(_.lift(1)).filter(_.nonEmpty)

    def isDiscordId(v : String) = v.length >= 17 && v.length <= 20 && v.forall(_.isDigit)

    def isUsername(v : String) = v.length >= 2 && v.length <= 32 && v.forall(c => c.isLetterOrDigit || c == '_' || c == '.')

    // the user ID to ping for a ranking name: the ID it gave, the one its username was found as when it was given (the row's third
    // field), or the username looked up now by the Discord bot (DiscordBot.scala), if one is set up
    def discordId(k : String) : Option[String] = rows(discords).filter(_.headOption.contains(k)).lastOption.flatMap { r =>
        r.lift(1).filter(_.nonEmpty).flatMap { v =>
            if (isDiscordId(v)) Some(v) else r.lift(2).filter(isDiscordId).orElse(if (DiscordBot.configured) DiscordBot.idOf(v) else None)
        }
    }

    // the user ID kept for a ranking name, given or found when its username was given (no lookup: for the pages)
    def storedId(k : String) : Option[String] = rows(discords).filter(_.headOption.contains(k)).lastOption.flatMap(r => r.lift(1).filter(isDiscordId).orElse(r.lift(2).filter(isDiscordId)))

    // "Name (@mention)" for a table's thread
    def tagged(k : String) = display(k) + discordId(k).map(i => " (<@" + i + ">)").orElse(discordOf(k).map(v => " (@" + v + ")")).getOrElse("")

    // a player's Discord user ID (17 to 20 digits), or username with the Discord bot set up, or "" to forget it; a username is
    // looked up at once (outside the lock: it asks Discord), and kept with the ID it was found as. Without the bot, only an ID:
    // a webhook can mention (ping) no one by username, and the usernames given first pinged no one (2026-11-20)
    def setDiscord(key : String, name : String, value : String) : String = {
        val k = Ranking.clean(name).toLowerCase
        val v = value.trim.stripPrefix("<@").stripPrefix("@").stripSuffix(">").trim

        if (!ownedKeys(key).contains(k))
            "no"
        else
        if (v.nonEmpty && !isDiscordId(v) && !isUsername(v))
            "invalid"
        else
        if (v.nonEmpty && !isDiscordId(v) && !DiscordBot.configured)
            "nobot"
        else {
            val found = if (v.nonEmpty && !isDiscordId(v)) DiscordBot.lookup(v) else DiscordBot.Unavailable

            if (found == DiscordBot.NotFound)
                "notfound"
            else synchronized {
                Files.createDirectories(dir)
                found match {
                    case DiscordBot.Found(id) => Ranking.append(discords, row(k, DiscordBot.cleanName(v), id))
                    case _ => Ranking.append(discords, row(k, if (isDiscordId(v)) v else DiscordBot.cleanName(v)))
                }
                "ok"
            }
        }
    }

    def json(s : String) = "\"" + s.flatMap {
        case '"' => "\\\""
        case '\\' => "\\\\"
        case '\n' => "\\n"
        case c if c < ' ' => ""
        case c => c.toString
    } + "\""

    // to the tournament's thread (and, with table, to that table's thread too; new tables' threads are started with start)
    def announce(text : String, n : Int = 0, table : String = "") {
        println("Tournament: " + text.replace("\n", "; "))

        Ranking.append(dir.resolve("announcements.tsv"), row(Ranking.now(), text))

        (if (table != "") forumWebhook.orElse(webhook) else webhook).foreach { url =>
            val number = if (n > 0) n else cups().lastOption.map(_.n).getOrElse(1)
            val key = threadKey(number, table)
            val name = threadName(number, table)
            val ids = "<@(\\d{17,20})>".r.findAllMatchIn(text).map(_.group(1)).toList.distinct

            poster.execute(() => post(url, text, key, name, ids))
        }
    }

    // the webhook's channel took a plain post after refusing a thread (Discord text channels do)
    @volatile var plainChannels = Set[String]()

    // THE BOT'S OWN EMOJIS (2026-11-22: turn pings showed ":hrf_rt_tw:" as plain text): the bot's application emojis (hrf_<slug>,
    // ng_<clan>, Lfg.syncClanEmojis) can be used only by the bot itself, never by a webhook, whatever the server's permissions. A post
    // holding one goes out from the bot into its thread (it needs View Channel and Send Messages in Threads there); when it can't (no
    // token, no thread yet, refused), the webhook posts it without them, rather than with their names as text
    val customEmoji = "<a?:[A-Za-z0-9_]+:(\\d+)> ?".r

    def appEmojis(text : String) = { val ids = Lfg.appEmojiIds ; customEmoji.findAllMatchIn(text).exists(m => ids.contains(m.group(1))) }

    def withoutAppEmojis(text : String) = { val ids = Lfg.appEmojiIds ; customEmoji.replaceAllIn(text, m => if (ids.contains(m.group(1))) "" else scala.util.matching.Regex.quoteReplacement(m.matched)) }

    def post(url : String, text0 : String, key : String, name : String, ids : List[String], file : Path = threads) {
        val byBot = appEmojis(text0) && DiscordBot.token.exists(t => threadOf(key, file).exists { thread =>
            DiscordBot.send("POST", "https://discord.com/api/v10/channels/" + thread + "/messages",
                "{\"content\":" + json(text0.take(1900)) + ",\"allowed_mentions\":{\"parse\":[],\"users\":[" + ids.map(json).mkString(",") + "]}}", Some(t)).nonEmpty
        })
        if (byBot)
            return

        val text = withoutAppEmojis(text0)

        def send(fields : String, query : String) = {
            val body = "{\"content\":" + json(text.take(1900)) + ",\"allowed_mentions\":{\"parse\":[],\"users\":[" + ids.map(json).mkString(",") + "]}" + fields + "}"
            val request = java.net.http.HttpRequest.newBuilder(java.net.URI.create(url + "?wait=true" + query))
                .header("Content-Type", "application/json")
                .POST(java.net.http.HttpRequest.BodyPublishers.ofString(body, UTF_8))
                .build()
            // Discord's rate limit (a table's threads start together): wait as long as it says, a few times
            var r = client.send(request, java.net.http.HttpResponse.BodyHandlers.ofString(UTF_8))
            var tries = 0
            while (r.statusCode == 429 && tries < 5) {
                val wait = "\"retry_after\"\\s*:\\s*([0-9.]+)".r.findFirstMatchIn(r.body).flatMap(_.group(1).toDoubleOption).getOrElse(2.0)
                Thread.sleep((wait.min(30.0) * 1000).toLong + 250)
                r = client.send(request, java.net.http.HttpResponse.BodyHandlers.ofString(UTF_8))
                tries += 1
            }
            r
        }

        def ok(r : java.net.http.HttpResponse[String]) = r.statusCode / 100 == 2

        // a new thread named name, its id kept for the next posts
        def fresh() = {
            val r = send(",\"thread_name\":" + json(name.take(100)), "")
            if (ok(r))
                "\"channel_id\"\\s*:\\s*\"(\\d+)\"".r.findFirstMatchIn(r.body).foreach(m => Ranking.append(file, row(key, m.group(1))))
            r
        }

        try {
            // the thread kept, or a new one (a kept thread may be gone, or from another webhook's channel), or the channel itself;
            // a channel that took no thread (a text channel) is tried first after that, until it refuses a plain post
            val plain = () => { val r = send("", "") ; plainChannels = if (ok(r)) plainChannels + url else plainChannels - url ; r }
            val inThread = List[() => java.net.http.HttpResponse[String]](
                () => threadOf(key, file).map(t => send("", "&thread_id=" + t)).getOrElse(null),
                () => fresh())
            val r = (if (plainChannels.contains(url)) plain +: inThread else inThread :+ plain).foldLeft(null : java.net.http.HttpResponse[String]) { (last, f) =>
                if (last != null && (ok(last) || last.statusCode / 100 != 4)) last else Option(f()).getOrElse(last)
            }

            if (r != null && !ok(r))
                println("Tournament announcement not posted: " + r.statusCode + " " + r.body.take(200))
        }
        catch {
            case e : Exception => println("Tournament announcement not posted: " + e)
        }
    }
}
