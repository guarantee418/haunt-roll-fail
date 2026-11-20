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
//
// Changes by hand go through the games backup (hrf-games) like the ranking's: each file tournament-changes/<name>.txt there is
// applied once (Backup.scala), its outcome in tournament-changes/<name>.result. One change a line:
//   remove <name>                     takes a name off the sign-ups
//   winner <table> <name>             the table's winner (a shared win, or a table that won't finish), A to D or F
//   replace <table> <name> -> <new>   a player at a table is replaced: the seat now plays (and is ranked) under the new name
//   cancel                            the running tournament ends with no champion; sign-ups for the next one open
//   webhook <url>                     the Discord webhook (written to the file discord-webhook; "webhook off" removes it);
//                                     delete the change file once its .result says "set"
// The Discord channel of the webhook in HRF_DISCORD_WEBHOOK (or the file discord-webhook) is told when a tournament starts,
// each table is won, the final starts and the champion is known.
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

    var db : Database = null

    def execute[E <: Effect](actions : DBIOAction[_, NoStream, E]*) = Await.result(db.run(DBIO.seq(actions : _*).withPinnedSession), Duration.Inf)

    def start(database : Database) {
        db = database
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

        if (c.signups.size + 1 >= size)
            begin(n)

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

        cups().filter(c => c.started && !c.done).flatMap { c =>
            c.tables.filter(_.winner.isEmpty).flatMap { t =>
                t.names.zipWithIndex.filter(x => kk.contains(x._1)).map { case (k, i) =>
                    row(c.n, t.id, t.title(c.n), t.secrets(i), display(k), t.names.filter(_ != k).map(display).mkString(", "))
                }
            }
        }.mkString("\n")
    }

    // the 16th sign-up: four tables of four, in random order
    def begin(n : Int) {
        val c = cups().find(_.n == n).get
        val names = scala.util.Random.shuffle(c.signups.map(_._1)).take(size)

        heatIds.zip(names.grouped(seats).toList).foreach { case (id, l) => create(n, id, l) }

        val dd = Ranking.displays()
        announce("**Root Tournament " + n + " has started!** Find your game at the top of Play Online, or on the Tournament page.\n" +
            heatIds.zip(names.grouped(seats).toList).map { case (id, l) => "Table " + id + ": " + l.map(k => dd.getOrElse(k, k)).mkString(", ") }.mkString("\n"))
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
                    case _ => announce("Root Tournament " + c.n + ", " + t.title(c.n).split(' ').drop(2).mkString(" ") + " ended with " + (if (ww.isEmpty) "no winner" else "a shared win (" + ww.map(display).mkString(", ") + ")") + ": the organizer picks who goes on.")
                }
            }
        }
    }

    def win(n : Int, id : String, k : String) {
        event("winner", n, id, k)

        val c = cups().find(_.n == n).get

        if (id == finalId) {
            event("champion", n, k)
            announce("**" + display(k) + " is the champion of Root Tournament " + n + "!**")
        }
        else {
            announce(display(k) + " won Table " + id + " of Root Tournament " + n + ".")

            if (c.heats.size == heatIds.size && c.heats.forall(_.winner.nonEmpty) && c.fin.isEmpty) {
                val l = scala.util.Random.shuffle(c.heats.flatMap(_.winner))
                create(n, finalId, l)
                announce("**The final of Root Tournament " + n + " has started:** " + l.map(display).mkString(", ") + ". Sign-ups for Tournament " + (n + 1) + " are open.")
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
                        case Some(c) => event("cancel", c.n); announce("Root Tournament " + c.n + " was cancelled."); "cancelled"
                        case None => "not done: no tournament running"
                    }
                // the Discord webhook, set from the games backup as no session can reach the server; written to the file
                // discord-webhook, and the URL is never written back in the .result
                case "webhook" =>
                    if (rest.startsWith("https://discord.com/api/webhooks/") || rest.startsWith("https://discordapp.com/api/webhooks/")) {
                        Files.write(Paths.get("discord-webhook"), (rest + "\n").getBytes(UTF_8))
                        announce("The Root tournament's announcements will be posted here.")
                        "set"
                    }
                    else if (rest == "off") {
                        Files.deleteIfExists(Paths.get("discord-webhook"))
                        "removed"
                    }
                    else
                        "not done: webhook <https://discord.com/api/webhooks/...> or webhook off"
                case _ => "not done: not a change (remove, winner, replace, cancel or webhook)"
            }

            (if (word == "webhook") "webhook" else line) + ": " + result
        }
    }

    // DISCORD: a message to the channel of the webhook, if one is set up
    def webhook : Option[String] =
        sys.env.get("HRF_DISCORD_WEBHOOK").map(_.trim).filter(_.nonEmpty).orElse {
            val f = Paths.get("discord-webhook")
            if (Files.exists(f)) Some(new String(Files.readAllBytes(f), UTF_8).trim).filter(_.nonEmpty) else None
        }

    lazy val client = java.net.http.HttpClient.newHttpClient()

    def json(s : String) = "\"" + s.flatMap {
        case '"' => "\\\""
        case '\\' => "\\\\"
        case '\n' => "\\n"
        case c if c < ' ' => ""
        case c => c.toString
    } + "\""

    def announce(text : String) {
        println("Tournament: " + text.replace("\n", "; "))

        Ranking.append(dir.resolve("announcements.tsv"), row(Ranking.now(), text))

        webhook.foreach { url =>
            try {
                val request = java.net.http.HttpRequest.newBuilder(java.net.URI.create(url))
                    .header("Content-Type", "application/json")
                    .POST(java.net.http.HttpRequest.BodyPublishers.ofString("{\"content\":" + json(text.take(1900)) + ",\"allowed_mentions\":{\"parse\":[]}}", UTF_8))
                    .build()

                client.sendAsync(request, java.net.http.HttpResponse.BodyHandlers.ofString(UTF_8))
            }
            catch {
                case e : Exception => println("Tournament announcement not posted: " + e)
            }
        }
    }
}
