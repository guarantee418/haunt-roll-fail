package hrf.gg

import slick.jdbc.HsqldbProfile.api._

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path, Paths}

import scala.concurrent.Await
import scala.concurrent.duration.Duration

// Discord turn pings (asked for on 2026-11-20): a tournament table's game pings in its table's thread (Tournament.scala); the
// other Root online games each get a post (a thread) in the forum channel of the webhook
// in HRF_DISCORD_TURNS_WEBHOOK (or the file discord-turns-webhook, set by the tournament change "turns <url>"), opened when the game
// is created, with its players and spectator link, and a player is @mentioned there when it becomes their turn.
//
// The server can't read a game, so the clients tell it which factions the game asks (/turn, Turns.scala), and the lobby's
// "user <faction> <user>" lines say whose seats they are. Every 30 seconds a sweep (started with the server) pings the seats due
// (2026-11-21: the owner found the pings inconsistent; a ping was a timer a restart lost, and one within 10 minutes of the last was
// dropped, so a seat pinged for an ambush wasn't for the turn that followed). A seat waited for since <start> is due:
//   - a minute after start,
//   - again 12 hours after its last ping, at most twice ("reminder"),
// whether or not its player has the game open (2026-11-22, owner: "Always ping players even if their tab is open"; a player
// with the game open was skipped, and one who didn't see it was never pinged), and pinged then if:
//   - it wasn't pinged in that game in the last 2 minutes (it waits until then, as the sweep comes back),
//   - its seat plays under a ranking name with a Discord (given, or set by the owner) and turn pings not turned off (Rankings,
//     "Discord turn pings"; on unless turned off since 2026-11-20, asked for: "start pinging people whenever it is their turn").
// Several prompts in a row (a whole turn) are one ping, as the seat stays waited for between them. When a seat started being
// waited for is the time on its state.tsv row, so a restart neither loses nor repeats a ping. A seat waited for since before
// that (rows with no time, and the games Turns.scala knows of but this file doesn't, reports from before the turns webhook was
// set, added at the server's start) isn't pinged by the sweep, as an abandoned game can't be told from a live one: the
// "ping waiting" changes ping it, and its next turn is pinged as usual.
//
// Files, in turns/ in the server's working directory:
//   state.tsv     game, log index, the users waited for (comma-separated), time: each game's reports (its last row counts)
//   threads.tsv   game, the Discord thread made for it
//   pings.tsv     time, game, user: the pings sent
//   created.tsv   game: the games whose post went out, so each goes out once
//   firsts.tsv    game, watch or told: the games whose first player the thread is told of (announceFirst)
// and in tournament/: discord-pings.tsv  name, on or off (the last row counts); on unless turned off
// The tournament change "ping waiting" pings at once every seat the games wait for (the same checks but the minute's delay).
//
// NORTHGARD (2026-11-21, with its Discord LFG games, Lfg.scala): Northgard's online games (nort, nort-adset) get the same, in the
// forum channel of their own webhook, HRF_DISCORD_NORT_TURNS_WEBHOOK or the file discord-nort-turns-webhook (the change
// "nort-turns <url>"), their threads in threads-nort.tsv; with none set, they get no posts or pings (webhookFor).
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
    val metas = Map("root" -> "Root", "root-adset" -> "Root Advanced Setup", "nort" -> "Northgard", "nort-adset" -> "Northgard Adset")

    val nortMetas = Set("nort", "nort-adset")
    val nortThreads = dir.resolve("threads-nort.tsv")

    // the channel a game's posts and pings go to, and the file its threads are kept in
    def webhookFor(meta : String) = if (nortMetas.contains(meta)) nortWebhook else webhook
    def threadsFor(meta : String) = if (nortMetas.contains(meta)) nortThreads else threads

    val delay = 60 * 1000L
    val watching = 90 * 1000L
    val gap = 2 * 60 * 1000L
    val remind = 12 * 60 * 60 * 1000L
    val reminders = 2
    val every = 30L

    def webhook = Tournament.hook("HRF_DISCORD_TURNS_WEBHOOK", "discord-turns-webhook")
    def nortWebhook = Tournament.hook("HRF_DISCORD_NORT_TURNS_WEBHOOK", "discord-nort-turns-webhook")

    def run[R](a : DBIOAction[R, NoStream, Nothing]) : R = Await.result(Tournament.db.run(a), Duration.Inf)

    // the last time each user read each game's log: (user, game) -> time
    val reads = new java.util.concurrent.ConcurrentHashMap[(String, String), java.lang.Long]()

    def read(user : String, game : String) { reads.put((user, game), System.currentTimeMillis) }

    def readSince(user : String, game : String, ms : Long) = Option(reads.get((user, game))).exists(t => System.currentTimeMillis - t < ms)

    def lastRead(user : String, game : String) : Long = Option(reads.get((user, game))).map(_.longValue).getOrElse(0L)

    def time(t : String) : Option[Long] = scala.util.Try(java.time.LocalDateTime.parse(t).toInstant(java.time.ZoneOffset.UTC).toEpochMilli).toOption

    case class Report(index : Int, users : Set[String])

    var reports : Map[String, Report] = null

    def latest : Map[String, Report] = {
        if (reports == null)
            reports = rows(state).collect { case g :: i :: u :: _ if i.toIntOption.nonEmpty => g -> Report(i.toInt, u.split(',').filter(_.nonEmpty).toSet) }.toMap
        reports
    }

    // the pings sent: (game, user) -> their times
    var sent : Map[(String, String), List[Long]] = null

    def pings : Map[(String, String), List[Long]] = {
        if (sent == null)
            sent = rows(pinged).collect { case t :: g :: u :: _ => ((g, u), time(t).getOrElse(0L)) }.groupBy(_._1).map { case (k, l) => k -> l.map(_._2) }
        sent
    }

    def lastPing(game : String, user : String) : Long = pings.get((game, user)).map(_.max).getOrElse(0L)

    // since when each seat has been waited for: (game, user) -> time
    var waitedSince : Map[(String, String), Long] = null

    def since : Map[(String, String), Long] = {
        if (waitedSince == null) {
            var m = Map[(String, String), Long]()
            var now = Map[String, Set[String]]()
            rows(state).collect { case g :: i :: u :: rest if i.toIntOption.nonEmpty => (g, u.split(',').filter(_.nonEmpty).toSet, rest.headOption.flatMap(time)) }.foreach { case (g, us, t) =>
                val before = now.getOrElse(g, Set())
                (before -- us).foreach(u => m -= ((g, u)))
                (us -- before).foreach(u => m += ((g, u)) -> t.getOrElse(0L))
                now += g -> us
            }
            waitedSince = m
        }
        waitedSince
    }

    lazy val timer = java.util.concurrent.Executors.newSingleThreadScheduledExecutor()

    // at the server's start: the games Turns knows of and this file doesn't, then the sweep every 30 seconds
    def start() {
        try {
            Turns.all.foreach { case (g, (n, factions)) => if (!latest.contains(g)) seed(g, n, factions) }
        }
        catch {
            case e : Exception => println("Turn pings: games not added: " + e)
        }
        timer.scheduleAtFixedRate(new Runnable { def run() = try sweep() catch { case e : Throwable => println("Turn pings: sweep failed: " + e) } }, every, every, java.util.concurrent.TimeUnit.SECONDS)
    }

    // a game's report that Turns kept but this file missed: its seats waited for since a time unknown
    def seed(g : String, n : Int, factions : List[String]) : Unit = synchronized {
        lobbyOf(g).filter(l => metas.contains(l.meta)).foreach { l =>
            val seats = l.lines.map(_.split(' ').toList).collect { case "user" :: f :: u :: _ => f -> u }.toMap
            val users = factions.flatMap(seats.get).toSet
            reports = latest + (g -> Report(n, users))
            waitedSince = since -- since.keys.filter(_._1 == g) ++ users.map(u => (g, u) -> 0L)
            Files.createDirectories(dir)
            Ranking.append(state, row(g, n, users.toList.sorted.mkString(",")))
        }
    }

    // games that have ended (their log holds the game over), never pinged: a game's last report is from before its end, as the
    // client asks nothing after it, so its last seat would look waited for; asked only when a ping is due, and kept once true
    var ended = Set[String]()

    def over(g : String) : Boolean = ended.contains(g) || {
        val r = run(entries.filter(_.journalId === g).filter(_.text like "%GameOver%").exists.result)
        if (r)
            ended += g
        r
    }

    // the seats due a ping (see above), pinged
    def sweep() : Unit = synchronized {
        val now = System.currentTimeMillis
        latest.toList.sortBy(_._1).foreach { case (g, r) =>
            r.users.toList.sorted.foreach { u =>
                val start = since.getOrElse((g, u), 0L)
                val mine = pings.getOrElse((g, u), Nil).filter(_ >= start)
                val due =
                    if (mine.isEmpty)
                        now - start >= delay
                    else
                        mine.size <= reminders && now - (start +: mine).max >= remind
                if (start > 0 && due && now - lastPing(g, u) >= gap && !over(g))
                    try attempt(g, u, reminder = mine.nonEmpty) catch { case e : Exception => println("Turn ping failed: " + e) }
            }
        }
    }

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
    // (in a tournament game, unless turned off: the players gave their ID to be pinged in their table's thread)
    // (in a Discord LFG game, Lfg.scala: the ID the player joined with, unless the name they play under turned pings off)
    def mentionOf(user : String, tournament : Boolean = false) : Option[String] =
        Lfg.discordOf(user).filter(_ => !Ranking.seatNames().get(user).exists(k => setting(k) == Some(false))).orElse(
        Ranking.seatNames().get(user).filter(k => if (tournament) setting(k) != Some(false) else pingsOn(k)).flatMap(Tournament.discordId))

    def setting(k : String) : Option[Boolean] = rows(settings).filter(_.headOption.contains(k)).lastOption.flatMap(_.lift(1)).map(_ == "on")

    def pingsOn(k : String) = !setting(k).contains(false)

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
        Tournament.ownedKeys(key).toList.sorted.map(k => row(Tournament.display(k), Tournament.discordOf(k).getOrElse(""), if (pingsOn(k)) "on" else "off", if (Tournament.storedId(k).nonEmpty) "ping" else "")).mkString("\n")

    // a tournament table's game: its table's own thread (Tournament.scala), which its players already follow
    def table(game : String) : Option[(Int, String)] = Tournament.cups().flatMap(c => c.tables.find(_.game == game).map(t => (c.n, t.id))).headOption

    def post(game : String, lobby : Lobby, text : String, ids : List[String]) {
        table(game) match {
            case Some((n, id)) =>
                Tournament.forumWebhook.orElse(Tournament.webhook).foreach { url =>
                    Tournament.poster.execute(() => Tournament.post(url, text, Tournament.threadKey(n, id), Tournament.threadName(n, id), ids))
                }
            case None =>
                webhookFor(lobby.meta).foreach { url =>
                    val name = (if (lobby.title.nonEmpty) lobby.title else "Game " + game.take(6)) + " · " + metas.getOrElse(lobby.meta, lobby.meta)
                    Tournament.poster.execute(() => Tournament.post(url, text, game, name, ids, threadsFor(lobby.meta)))
                }
        }
    }

    // a new game's lobby was written (its first lines, with "server <game>"): its post, with its players and spectator link
    def lobbyWritten(lobby : String, lines : List[String]) {
        try {
            val game = lines.find(_.startsWith("server ")).map(_.drop(7).trim).getOrElse("")
            val meta = lines.find(_.startsWith("meta ")).map(_.drop(5).trim).getOrElse("")
            if (game.nonEmpty && metas.contains(meta) && webhookFor(meta).nonEmpty && !introduced(game)) {
                val l = Lobby(meta, lines.find(_.startsWith("title ")).map(_.drop(6)).getOrElse(""), lines, lobby)
                lobbies.put(game, l)
                introduce(game, l, "New")
            }
        }
        catch {
            case e : Exception => println("Turn pings: new game not posted: " + e)
        }
    }

    def introduced(game : String) = rows(created).exists(_.headOption.contains(game))

    // a game's post, with its players and spectator link, which opens its thread: when its lobby is written ("New"), or before the
    // first ping of a game made before turn pings or before the turns webhook was set (2026-11-21), so its thread starts with it
    def introduce(game : String, l : Lobby, what : String) {
        Ranking.append(created, row(game))
        val lines = l.lines
        val seats = lines.filter(_.startsWith("user ")).map(_.split(' ').toList).collect { case _ :: f :: u :: _ => (f, u) }
        val bots = lines.count(_.startsWith("bot "))
        val spectator = run((for { p <- plays.filter(_.journalId === l.lobby) ; u <- users.filter(_.id === p.userId).filter(_.name === "Spectator") } yield p.secret).result.headOption)
        val ids = seats.flatMap(s => mentionOf(s._2))
        val who = seats.map { case (_, u) => nameOf(l, u) + mentionOf(u).map(i => " (<@" + i + ">)").getOrElse("") }
        val text = what + " " + metas(l.meta) + " game: **" + l.title + "**\n" +
            "Players: " + (if (who.isEmpty) "none" else who.mkString(", ")) + (if (bots > 0) ", and " + bots + " bot" + (if (bots > 1) "s" else "") else "") + "\n" +
            spectator.map(s => "Watch: <" + Tournament.site + "/play/" + l.meta + "/" + s + ">\n").getOrElse("") +
            "Each player is mentioned here when it's their turn, unless they turned turn pings off under Rankings."
        post(game, l, text, ids)
    }

    // a client's report (Turns.report): the game asks these factions after the log's first index entries
    def report(game : String, index : Int, factions : List[String]) : Unit =
        try {
            // kept whether or not a webhook is set yet, so the pings can start as soon as one is
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

        val now = System.currentTimeMillis
        val was = before.map(_.users).getOrElse(Set())
        reports = latest + (game -> Report(index, waiting))
        waitedSince = since -- (was -- waiting).map(u => (game, u)) ++ (waiting -- was).map(u => (game, u) -> now)
        Files.createDirectories(dir)
        Ranking.append(state, row(game, index, waiting.toList.sorted.mkString(","), Ranking.now()))
        // (the sweep pings the seats newly waited for a minute from now)

        // WHO GOES FIRST (2026-11-21: in an Advanced Setup game the seat order is drawn in the game, the first player opened the
        // game before the minute was up, so the thread never said it was their turn): a game whose first report comes now is
        // watched (firsts.tsv, "watch"), and the first time it waits for one seat alone the thread says who that is, mentioning
        // them ("told"); games reported before this keep their pings only
        if (before.isEmpty)
            Ranking.append(firsts, row(game, "watch"))
        if (waiting.size == 1 && firstState(game).contains("watch"))
            try announceFirst(game, waiting.head) catch { case e : Exception => println("Turn pings: first player not posted: " + e) }
    }

    val firsts = dir.resolve("firsts.tsv")

    def firstState(game : String) : Option[String] = rows(firsts).filter(_.headOption.contains(game)).lastOption.flatMap(_.lift(1))

    def announceFirst(game : String, user : String) {
        Ranking.append(firsts, row(game, "told"))
        lobbyOf(game).filter(l => metas.contains(l.meta)).foreach { l =>
            if (introduced(game) || table(game).nonEmpty) {
                val id = mentionOf(user, table(game).nonEmpty)
                post(game, l, reasonOf(game, l, user)._1 + "**" + nameOf(l, user) + "** takes the first turn in **" + l.title + "**" + id.map(i => " (<@" + i + ">)").getOrElse("") + ".", id.toList)
                // the seat is told now: its first ping waits for its next turn
                id.foreach { _ =>
                    sent = pings + ((game, user) -> (System.currentTimeMillis :: pings.getOrElse((game, user), Nil)))
                    Ranking.append(pinged, row(Ranking.now(), game, user))
                }
            }
        }
    }

    // every seat the games wait for, at once (the "ping waiting" change): how many were pinged
    def pingWaiting() : Int = synchronized {
        latest.toList.sortBy(_._1).flatMap { case (g, r) => r.users.toList.sorted.map(u => (g, u)) }.count { case (g, u) =>
            try ping(g, u) catch { case e : Exception => println("Turn ping failed: " + e) ; false }
        }
    }

    // the seats the given games wait for, pinged at once (the "ping waiting <game> ..." change; 2026-11-21): a line for each game,
    // saying who was pinged and why anyone wasn't
    def pingWaitingIn(games : List[String]) : List[String] = synchronized {
        games.map { g =>
            // a game whose reports came while no turns webhook was set has none here: the one Turns kept stands in for it
            if (latest.get(g).isEmpty)
                Turns.last(g).foreach { case (n, factions) => seed(g, n, factions) }
            latest.get(g) match {
                case None => g + ": no turn report yet (no one has opened it since turn reports came)"
                case Some(r) if r.users.isEmpty => g + ": waits for no seat with a user"
                case Some(r) =>
                    val name = lobbyOf(g).map(l => (if (l.title.nonEmpty) l.title + " " else "")).getOrElse("")
                    g + " " + name + "(log " + r.index + "): " + r.users.toList.sorted.map { u =>
                        val who = lobbyOf(g).map(l => nameOf(l, u)).getOrElse(u)
                        who + " " + (try attempt(g, u) catch { case e : Exception => "failed: " + e })
                    }.mkString(", ")
            }
        }
    }

    // WHY IT'S THEIR TURN (asked for on 2026-11-21): the reason the seat's client gave (Turns.reason; the client's Meta.turnReason,
    // root/turn-reasons.scala and nort/turn-reasons.scala: a tag, maybe "/<faction>") as an emoji in the server's own style (the
    // Discord server's sword or axe, its deck, map and faction emojis, else the bot's icons, LfgSetup.emojiText) and a few words;
    // ("", "it's your turn") for a client that gave none
    def reasonOf(game : String, lobby : Lobby, user : String) : (String, String) = {
        val seat = lobby.lines.map(_.split(" ").toList).collectFirst { case "user" :: s :: u :: _ if u == user => s }
        val r = seat.flatMap(s => Turns.reason(game, s)).getOrElse("")
        val (tag, faction) = r.split("/", 2).toList match {
            case t :: f :: Nil => (t, Some(f))
            case t :: _ => (t, None)
            case _ => ("", None)
        }
        val nort = nortMetas.contains(lobby.meta)
        // the faction's (or clan's) emoji: the one the reason names, else the seat's own when the seat is a faction (Root's and
        // Northgard's standard games; an Advanced Setup or Adset seat drafts its faction in the game, so only a reason names it)
        def iconOf(f : String) = if (nort) Lfg.clanIcon(f) else LfgSetup.emojiText("IncludeFaction(" + f + ")")
        val head = faction.map(iconOf).filter(_.nonEmpty).orElse(seat.map(iconOf).filter(_.nonEmpty)).getOrElse("")
        // (the server's are :sword-root: and :axe-ng:, seen on 2026-11-21)
        def sword = LfgSetup.serverNamed(List("swordroot", "sword", "swords", "battle", "attack")).getOrElse("⚔️ ")
        def axe = LfgSetup.serverNamed(List("axeng", "axe", "battleaxe", "fight")).getOrElse("🪓 ")
        def e(key : String) = LfgSetup.emojiText(key)
        val (icon, words) = (nort, tag) match {
            case (false, "defend") => (sword, "you're under attack")
            case (false, "battle") => (sword, "your battle needs you")
            case (false, "keep") => (e("StandardDeck"), "pick the cards you keep")
            case (false, "handlimit") => (e("StandardDeck"), "discard down to your hand limit")
            case (false, "draft") => (e("title"), "your pick in the faction draft")
            case (false, "pickdeck") => (e("StandardDeck"), "pick the deck")
            case (false, "pickmap") => (e("RandomOfficialMap"), "pick the map")
            case (false, "picklandmark") => (e("TowerLandmark"), "pick the landmark for the Tower's clearing")
            case (false, "partisans") => (e("ExilesDeck"), "Partisans can join your battle")
            case (false, "prisoners") => (e("IncludeFaction(KD)"), "prisoners to deal with")
            case (false, "negotiate") => (e("IncludeFaction(LDvE)"), "a negotiation awaits you")
            case (false, "entreat") => (e("IncludeFaction(TC)"), "you were entreated")
            case (false, "hospitals") => (e("IncludeFaction(MC)"), "Field Hospitals: save your cats?")
            case (false, "setup") => (head, "time to set up")
            case (true, "defend") => (axe, "you're under attack")
            case (true, "fight") => (axe, "your fight needs you")
            case (true, "trade") => (e("turn:trade"), "harvest trading")
            case (true, "winter") => (e("turn:winter"), "Winter is here")
            case (true, "cards") => (e("HorizonsDevelopments"), "pick a card")
            case (true, "draft") => (e("title"), "your pick in the clan draft")
            case (true, "setup") => (head, "time to place your clan")
            case (true, "raid") => (e("ModuleOption(Sea)"), "your raid")
            case (true, "event") => (e("ModuleOption(EventsModule)"), "an event needs you")
            case (_, "asked") => (head, "you're needed")
            case _ => (head, "it's your turn")
        }
        // every ping leads with the faction's emoji (2026-11-22: "the discord notifications don't consistently give the faction
        // emoji"), then the reason's own (the sword, the cards ...) when it has another
        (if (icon == head) head else head + icon, words)
    }

    // true when the ping went out
    def ping(game : String, user : String) : Boolean = attempt(game, user) == "pinged"

    // "pinged", or why not
    def attempt(game : String, user : String, reminder : Boolean = false) : String = synchronized {
        val now = System.currentTimeMillis
        if (!latest.get(game).exists(_.users.contains(user)))
            return "not waited for"
        if (now - lastPing(game, user) < gap)
            return "not pinged: pinged in the last 2 minutes"
        val lobby = lobbyOf(game).getOrElse(return "not pinged: lobby not found")
        if (!metas.contains(lobby.meta))
            return "not pinged: not a Root or Northgard game"
        if (table(game).isEmpty && webhookFor(lobby.meta).isEmpty)
            return "not pinged: no turns webhook (the " + (if (nortMetas.contains(lobby.meta)) "nort-turns" else "turns") + " change)"
        val id = mentionOf(user, table(game).nonEmpty).getOrElse(return "not pinged: no Discord, or turn pings off")

        // a game with no thread of its own yet gets its post first (a tournament table's game uses its table's thread)
        if (table(game).isEmpty && webhookFor(lobby.meta).nonEmpty && !introduced(game))
            introduce(game, lobby, "Ongoing")

        sent = pings + ((game, user) -> (now :: pings.getOrElse((game, user), Nil)))
        Files.createDirectories(dir)
        Ranking.append(pinged, row(Ranking.now(), game, user))

        val (icon, words) = reasonOf(game, lobby, user)
        post(game, lobby, icon + "<@" + id + ">, " + (if (reminder) "a reminder: " + (if (words == "it's your turn") "it's still your turn" else words) else words) + " in **" + lobby.title + "**" + " (" + nameOf(lobby, user) + ").", List(id))
        "pinged"
    }
}
