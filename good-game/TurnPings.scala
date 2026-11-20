package hrf.gg

import slick.jdbc.HsqldbProfile.api._

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path, Paths}

import scala.concurrent.Await
import scala.concurrent.duration.Duration

// Discord turn pings (asked for on 2026-11-20): Root's online games each get a post (a thread) in the forum channel of the webhook
// in HRF_DISCORD_TURNS_WEBHOOK (or the file discord-turns-webhook, set by the tournament change "turns <url>"), opened when the game
// is created, with its players and spectator link, and a player is @mentioned there when it becomes their turn.
//
// The server can't read a game, so the clients tell it which factions the game asks (/turn, Turns.scala), and the lobby's
// "user <faction> <user>" lines say whose seats they are. A seat newly waited for is pinged a minute later if:
//   - the game still waits for it then (a later report hasn't taken it off),
//   - its player hasn't had the game open in the last 90 seconds (their client reads the log while it is open),
//   - its seat plays under a ranking name whose owner gave a Discord user ID and turned turn pings on (Rankings, "Discord turn pings"),
//   - it wasn't pinged in that game in the last 10 minutes.
// Several prompts in a row (a whole turn) are one ping, as the seat stays waited for between them.
//
// Files, in turns/ in the server's working directory:
//   state.tsv     game, log index, the users waited for (comma-separated): the latest report of each game (the last row counts)
//   threads.tsv   game, the Discord thread made for it
//   pings.tsv     time, game, user: the pings sent
//   created.tsv   game: the games whose post went out, so each goes out once
// and in tournament/: discord-pings.tsv  name, on or off (the last row counts); off unless turned on
object TurnPings {
    import GoodGame._
    import Backup.{rows, row}

    val dir = Paths.get("turns")
    val state = dir.resolve("state.tsv")
    val threads = dir.resolve("threads.tsv")
    val pinged = dir.resolve("pings.tsv")
    val created = dir.resolve("created.tsv")
    val settings = Tournament.dir.resolve("discord-pings.tsv")

    // the games that get a thread and pings
    val metas = Map("root" -> "Root", "root-adset" -> "Root Advanced Setup")

    val delay = 60 * 1000L
    val watching = 90 * 1000L
    val cooldown = 10 * 60 * 1000L

    def webhook = Tournament.hook("HRF_DISCORD_TURNS_WEBHOOK", "discord-turns-webhook")

    def run[R](a : DBIOAction[R, NoStream, Nothing]) : R = Await.result(Tournament.db.run(a), Duration.Inf)

    // the last time each user read each game's log: (user, game) -> time
    val reads = new java.util.concurrent.ConcurrentHashMap[(String, String), java.lang.Long]()

    def read(user : String, game : String) { reads.put((user, game), System.currentTimeMillis) }

    def readSince(user : String, game : String, ms : Long) = Option(reads.get((user, game))).exists(t => System.currentTimeMillis - t < ms)

    case class Report(index : Int, users : Set[String])

    var reports : Map[String, Report] = null

    def latest : Map[String, Report] = {
        if (reports == null)
            reports = rows(state).collect { case g :: i :: u :: _ if i.toIntOption.nonEmpty => g -> Report(i.toInt, u.split(',').filter(_.nonEmpty).toSet) }.toMap
        reports
    }

    var lastPing : Map[(String, String), Long] = null

    def pings : Map[(String, String), Long] = {
        if (lastPing == null)
            lastPing = rows(pinged).collect { case t :: g :: u :: _ => (g, u) -> scala.util.Try(java.time.LocalDateTime.parse(t).toInstant(java.time.ZoneOffset.UTC).toEpochMilli).getOrElse(0L) }.toMap
        lastPing
    }

    lazy val timer = java.util.concurrent.Executors.newSingleThreadScheduledExecutor()

    // a game's lobby lines, by the "server <game>" line its lobby holds
    case class Lobby(meta : String, title : String, lines : List[String], lobby : String)

    val lobbies = new java.util.concurrent.ConcurrentHashMap[String, Lobby]()

    def lobbyOf(game : String) : Option[Lobby] = Option(lobbies.get(game)).orElse {
        run(entries.filter(_.text === ("server " + game)).map(_.journalId).result.headOption).map { l =>
            val lines = run(entries.filter(_.journalId === l).sortBy(_.index).map(_.text).result).toList
            def first(k : String) = lines.find(_.startsWith(k + " ")).map(_.drop(k.length + 1)).getOrElse("")
            val r = Lobby(first("meta"), first("title"), lines, l)
            lobbies.put(game, r)
            r
        }
    }

    // the seats' names: the ranking name the seat plays under, else the name typed for it
    def nameOf(lobby : Lobby, user : String) : String =
        Ranking.seatNames().get(user).map(Tournament.display).orElse {
            (lobby.lines ++ run(entries.filter(_.journalId === lobby.lobby).map(_.text).result).toList).reverse
                .find(t => t.startsWith("name " + user + " ") || t.startsWith("prename " + user + " ")).map(_.split(' ').drop(2).mkString(" "))
        }.getOrElse("an open seat")

    // the Discord user ID to ping for a seat: its ranking name's, if pings are on for it
    def mentionOf(user : String) : Option[String] =
        Ranking.seatNames().get(user).filter(pingsOn).flatMap(Tournament.discordOf).filter(Tournament.isDiscordId)

    def pingsOn(k : String) = rows(settings).filter(_.headOption.contains(k)).lastOption.flatMap(_.lift(1)).contains("on")

    // "ok", or "no" for a name the code doesn't hold
    def setPings(key : String, name : String, on : Boolean) : String = synchronized {
        val k = Ranking.clean(name).toLowerCase
        if (!Tournament.ownedKeys(key).contains(k))
            "no"
        else {
            Ranking.append(settings, row(k, if (on) "on" else "off"))
            "ok"
        }
    }

    // the names a code holds: name, Discord, pings on or off
    def mine(key : String) : String =
        Tournament.ownedKeys(key).toList.sorted.map(k => row(Tournament.display(k), Tournament.discordOf(k).getOrElse(""), if (pingsOn(k)) "on" else "off")).mkString("\n")

    def post(game : String, lobby : Lobby, text : String, ids : List[String]) {
        webhook.foreach { url =>
            val name = (if (lobby.title.nonEmpty) lobby.title else "Game " + game.take(6)) + " · " + metas.getOrElse(lobby.meta, lobby.meta)
            Tournament.poster.execute(() => Tournament.post(url, text, game, name, ids, threads))
        }
    }

    // a new game's lobby was written (its first lines, with "server <game>"): its post, with its players and spectator link
    def lobbyWritten(lobby : String, lines : List[String]) {
        try {
            val game = lines.find(_.startsWith("server ")).map(_.drop(7).trim).getOrElse("")
            val meta = lines.find(_.startsWith("meta ")).map(_.drop(5).trim).getOrElse("")
            if (game.nonEmpty && metas.contains(meta) && webhook.nonEmpty && !rows(created).exists(_.headOption.contains(game))) {
                Ranking.append(created, row(game))
                val l = Lobby(meta, lines.find(_.startsWith("title ")).map(_.drop(6)).getOrElse(""), lines, lobby)
                lobbies.put(game, l)
                val seats = lines.filter(_.startsWith("user ")).map(_.split(' ').toList).collect { case _ :: f :: u :: _ => (f, u) }
                val bots = lines.count(_.startsWith("bot "))
                val spectator = run((for { p <- plays.filter(_.journalId === lobby) ; u <- users.filter(_.id === p.userId).filter(_.name === "Spectator") } yield p.secret).result.headOption)
                val ids = seats.flatMap(s => mentionOf(s._2))
                val who = seats.map { case (_, u) => nameOf(l, u) + mentionOf(u).map(i => " (<@" + i + ">)").getOrElse("") }
                val text = "New " + metas(meta) + " game: **" + l.title + "**\n" +
                    "Players: " + (if (who.isEmpty) "none" else who.mkString(", ")) + (if (bots > 0) ", and " + bots + " bot" + (if (bots > 1) "s" else "") else "") + "\n" +
                    spectator.map(s => "Watch: <" + Tournament.site + "/play/" + meta + "/" + s + ">\n").getOrElse("") +
                    "Each player is mentioned here when it's their turn, if they turned turn pings on under Rankings."
                post(game, l, text, ids)
            }
        }
        catch {
            case e : Exception => println("Turn pings: new game not posted: " + e)
        }
    }

    // a client's report (Turns.report): the game asks these factions after the log's first index entries
    def report(game : String, index : Int, factions : List[String]) : Unit =
        try {
            if (webhook.nonEmpty)
                lobbyOf(game).filter(l => metas.contains(l.meta)).foreach { l =>
                    val seats = l.lines.map(_.split(' ').toList).collect { case "user" :: f :: u :: _ => f -> u }.toMap
                    waiting(game, index, factions.flatMap(seats.get).toSet)
                }
        }
        catch {
            case e : Exception => println("Turn pings: report failed: " + e)
        }

    def waiting(game : String, index : Int, waiting : Set[String]) : Unit = synchronized {
        // (Turns.report has already dropped stale reports, and kept those after a fix shortened the log)
        val before = latest.get(game)
        if (before.exists(b => b.index == index && b.users == waiting))
            return

        reports = latest + (game -> Report(index, waiting))
        Ranking.append(state, row(game, index, waiting.toList.sorted.mkString(",")))

        val fresh = waiting -- before.map(_.users).getOrElse(Set())
        fresh.foreach { u =>
            timer.schedule(new Runnable { def run() = try ping(game, u) catch { case e : Exception => println("Turn ping failed: " + e) } }, delay, java.util.concurrent.TimeUnit.MILLISECONDS)
        }
    }

    def ping(game : String, user : String) : Unit = synchronized {
        val now = System.currentTimeMillis
        if (!latest.get(game).exists(_.users.contains(user)))
            return
        if (readSince(user, game, watching))
            return
        if (pings.get((game, user)).exists(now - _ < cooldown))
            return
        val id = mentionOf(user).getOrElse(return)
        val lobby = lobbyOf(game).getOrElse(return)
        if (!metas.contains(lobby.meta))
            return

        lastPing = pings + ((game, user) -> now)
        Ranking.append(pinged, row(Ranking.now(), game, user))

        post(game, lobby, "<@" + id + ">, it's your turn in **" + lobby.title + "**" + " (" + nameOf(lobby, user) + ").", List(id))
    }
}
