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
// "user <faction> <user>" lines say whose seats they are. Every 10 seconds a sweep (started with the server) pings the seats due
// (2026-11-21: the owner found the pings inconsistent; a ping was a timer a restart lost, and one within 10 minutes of the last was
// dropped, so a seat pinged for an ambush wasn't for the turn that followed). A seat waited for since <start> is due:
//   - 10 seconds after start, whatever it is asked (owner, 2026-11-24; it was a minute, 30 seconds for a battle on another's turn),
//   - again 12 hours after its last ping, at most twice ("reminder"),
// whether or not its player has the game open (2026-11-22, owner: "Always ping players even if their tab is open"; a player
// with the game open was skipped, and one who didn't see it was never pinged), and pinged then if:
//   - it wasn't pinged in that game in the last minute (2 minutes before 2026-11-24; it waits until then, as the sweep comes back),
//   - its seat plays under a ranking name with a Discord (given, or set by the owner) and turn pings not turned off (Rankings,
//     "Discord turn pings"; on unless turned off since 2026-11-20, asked for: "start pinging people whenever it is their turn").
// Several prompts in a row (a whole turn) are one ping, as the seat stays waited for between them; but a seat still waited for
// after the log passed a turn or phase boundary since the last report (`boundaries`: a turn ended or began, a Northgard year's
// phase) starts a new wait, pinged like any other (2026-11-24, owner: "it was complete separate turns": gg picked a Development
// card, his turn ended, the creature phase and harvest ran on and the Kobold exchange asked Stag again with nobody else asked in
// between, so it was counted as the wait already pinged, and no ping came for 12 hours). The new wait is two state.tsv rows, without
// the seat then with it. When a seat started being
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
// The tournament change "ping waiting" pings at once every seat the games wait for (the same checks but the 10 seconds' delay).
//
// NORTHGARD (2026-11-21, with its Discord LFG games, Lfg.scala): Northgard's online games (nort, nort-adset) get the same, in the
// forum channel of their own webhook, HRF_DISCORD_NORT_TURNS_WEBHOOK or the file discord-nort-turns-webhook (the change
// "nort-turns <url>"), their threads in threads-nort.tsv; with none set, they get no posts or pings (webhookFor).
//
// PINCER AND COMPANY OF HEROES (2026-11-24): the same again, each in its own forum (pincer-games, coh-games) through its own webhook,
// HRF_DISCORD_PINCER_TURNS_WEBHOOK or the file discord-pincer-turns-webhook (the change "pincer-turns <url>"), and
// HRF_DISCORD_TACTICS_TURNS_WEBHOOK or discord-tactics-turns-webhook ("tactics-turns <url>"), threads in threads-pincer.tsv and
// threads-tactics.tsv. The bot can make those webhooks itself, with a name and avatar of their own, and set them in one go: the
// change "make-webhook <name> | in <channel> | avatar <file> | for pincer-turns" (DiscordServer.scala). Their clients give no
// reasons yet, so their pings say "it's your turn", led by the game's own emoji (a drum, a field radio).
//
// CYCLADES (2026-11-27): the same in #cyclades-games, through HRF_DISCORD_CYCLADES_TURNS_WEBHOOK or discord-cyclades-turns-webhook (the change
// "cyclades-turns <url>", or make-webhook ... | for cyclades-turns), threads in threads-cyclades.tsv, its pings led by a temple.
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
    val metas = Map("root" -> "Root", "root-adset" -> "Root Advanced Setup", "nort" -> "Northgard", "nort-adset" -> "Northgard Adset",
        "pincer" -> "Pincer", "tactics" -> "Company of Heroes", "cyclades" -> "Cyclades")

    val rootMetas = Set("root", "root-adset")
    val nortMetas = Set("nort", "nort-adset")
    val nortThreads = dir.resolve("threads-nort.tsv")

    // each turns channel: its change's name, the environment variable and file its webhook's URL is kept in, its threads' file
    case class Slot(name : String, env : String, file : String, threads : Path)

    val slots = List(
        Slot("turns", "HRF_DISCORD_TURNS_WEBHOOK", "discord-turns-webhook", threads),
        Slot("nort-turns", "HRF_DISCORD_NORT_TURNS_WEBHOOK", "discord-nort-turns-webhook", nortThreads),
        Slot("pincer-turns", "HRF_DISCORD_PINCER_TURNS_WEBHOOK", "discord-pincer-turns-webhook", dir.resolve("threads-pincer.tsv")),
        Slot("tactics-turns", "HRF_DISCORD_TACTICS_TURNS_WEBHOOK", "discord-tactics-turns-webhook", dir.resolve("threads-tactics.tsv")),
        Slot("cyclades-turns", "HRF_DISCORD_CYCLADES_TURNS_WEBHOOK", "discord-cyclades-turns-webhook", dir.resolve("threads-cyclades.tsv")))

    def slot(name : String) : Option[Slot] = slots.find(_.name == name)

    def slotOf(meta : String) : Slot = slot(meta match {
        case m if nortMetas.contains(m) => "nort-turns"
        case "pincer" => "pincer-turns"
        case "tactics" => "tactics-turns"
        case "cyclades" => "cyclades-turns"
        case _ => "turns"
    }).get

    def hookOf(s : Slot) = Tournament.hook(s.env, s.file)

    // the channel a game's posts and pings go to, and the file its threads are kept in
    def webhookFor(meta : String) = hookOf(slotOf(meta))
    def threadsFor(meta : String) = slotOf(meta).threads

    // a turns channel's webhook set (the threads made through the one before forgotten), or removed with "off"
    def setHook(s : Slot, url : String) : String =
        if (url.startsWith("https://discord.com/api/webhooks/") || url.startsWith("https://discordapp.com/api/webhooks/")) {
            Files.write(Paths.get(s.file), (url + "\n").getBytes(UTF_8))
            Files.deleteIfExists(s.threads)
            Tournament.plainChannels = Set()
            "set"
        }
        else if (url == "off") {
            Files.deleteIfExists(Paths.get(s.file))
            "removed"
        }
        else
            "not done: " + s.name + " <https://discord.com/api/webhooks/...> or " + s.name + " off"

    // the emoji a game with no faction emojis leads its pings with
    val gameIcon = Map("pincer" -> "🥁 ", "tactics" -> "🎖️ ", "cyclades" -> "🏛️ ")
    // the games whose clients give reasons (pincer/online.scala, tactics/online.scala; 2026-11-27)
    val sideReasons = Set("pincer", "tactics")

    // 10 seconds for every wait (owner, 2026-11-24): it was a minute, and 30 seconds for a battle on another's turn (Tournament 1
    // Table A: the Duchy answered an ambush before the bot's ping, due 60 to 90 seconds in, came)
    val delay = 10 * 1000L
    val battleDelay = delay
    val watching = 90 * 1000L
    val gap = 60 * 1000L
    val remind = 12 * 60 * 60 * 1000L
    val reminders = 2
    val every = 10L

    def webhook = hookOf(slots(0))
    def nortWebhook = hookOf(slots(1))

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

    // at the server's start: the games Turns knows of and this file doesn't, then the sweep every 10 seconds
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
                        now - start >= (if (inBattle(g, u)) battleDelay else delay)
                    else
                        mine.size <= reminders && now - (start +: mine).max >= remind
                if (start > 0 && due && now - lastPing(g, u) >= gap && !over(g))
                    try attempt(g, u, reminder = mine.nonEmpty) catch { case e : Exception => println("Turn ping failed: " + e) }
            }
        }
    }

    // the seat is asked about a battle on another's turn: its client's reason is "defend" (Root's ambushes and hits, Northgard's fights)
    def inBattle(game : String, user : String) : Boolean = lobbyOf(game).exists { l =>
        l.lines.map(_.split(" ").toList).collectFirst { case "user" :: s :: u :: _ if u == user => s }.flatMap(s => Turns.reason(game, s)).exists(r => r.startsWith("defend") || r.startsWith("banished"))
    }

    // what the server knows of the given games' pings (the "pings <game> ..." change; 2026-11-24, to check a missed ping): each
    // report's row of state.tsv (log index, the users waited for, the time), then the pings sent, with the seats' names, last 40 lines
    def history(games : List[String]) : List[String] = synchronized {
        games.flatMap { g =>
            val l = lobbyOf(g)
            def who(us : String) = us.split(',').filter(_.nonEmpty).map(u => l.map(nameOf(_, u)).getOrElse(u)).mkString(",")
            val reports = rows(state).filter(_.headOption.contains(g)).map { case _ :: i :: us :: rest => rest.headOption.getOrElse("?") + " waits " + i + " " + who(us) ; case r => r.mkString(" ") }
            val sent = rows(pinged).filter(_.lift(1).contains(g)).map(r => r.headOption.getOrElse("?") + " pinged " + who(r.lift(2).getOrElse("")))
            (g + ": " + l.map(_.title).getOrElse("lobby not found") + ", reason now: " + Turns.reasonsOf(g)) +: (reports ++ sent).sorted.takeRight(40).map("  " + _)
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

    // (now: post on the calling thread, for a post already on the poster's, like the hecklers'; persona: Tournament.post's)
    def post(game : String, lobby : Lobby, text : String, ids : List[String], persona : String = "", now : Boolean = false) {
        def go(f : () => Unit) = if (now) f() else Tournament.poster.execute(() => f())
        table(game) match {
            case Some((n, id)) =>
                Tournament.forumWebhook.orElse(Tournament.webhook).foreach { url =>
                    go(() => Tournament.post(url, text, Tournament.threadKey(n, id), Tournament.threadName(n, id), ids, persona = persona))
                }
            case None =>
                webhookFor(lobby.meta).foreach { url =>
                    val name = (if (lobby.title.nonEmpty) lobby.title else "Game " + game.take(6)) + " · " + metas.getOrElse(lobby.meta, lobby.meta)
                    go(() => Tournament.post(url, text, game, name, ids, threadsFor(lobby.meta), persona))
                }
        }
    }

    // FINAL SCORES (owner, 2026-11-25: "when a game ends, always have Command Warren post the final scores with a congratulation
    // message"): the first result a finished game's clients report (/report-result, Ranking.report) is posted in the game's thread by its
    // turns webhook (Command Warren for Root): the winners congratulated, mentioned when they can be pinged, then every seat's faction and
    // final score, winners first, best score first, seat order on ties. The sides are "W:" and "L:", each seat "<user>/<faction>/<seat>/
    // <players>/<score>" (RankingSeat.token), people only. A tournament table's game is left out: Tournament.reported congratulates it.
    def finished(game : String, sides : String) : Unit = try {
        if (table(game).isEmpty) lobbyOf(game).filter(l => metas.contains(l.meta) && webhookFor(l.meta).nonEmpty).foreach { lobby =>
            case class S(won : Boolean, user : String, faction : String, seat : Int, score : String) {
                def rank = if (score == "D") Int.MaxValue else score.toIntOption.getOrElse(-1)
            }
            val seats = sides.split(';').toList.flatMap(t => t.split(":", 2).toList match {
                case w :: rest :: Nil if w == "W" || w == "L" => rest.split(',').toList.flatMap(x => x.split("/", -1).toList match {
                    case u :: f :: seat :: _ :: sc :: _ if u.nonEmpty && u != "*" => Some(S(w == "W", u, f.replace('_', ' '), seat.toIntOption.getOrElse(99), sc))
                    case _ => None
                })
                case _ => Nil
            }).distinctBy(_.user)

            if (seats.nonEmpty) {
                val unit = if (rootMetas.contains(lobby.meta) || lobby.meta == "tactics") "VP" else if (lobby.meta.startsWith("nort")) "fame" else if (lobby.meta == "pincer") "Royal Seals" else "points"
                def points(s : S) = if (s.score == "D") "dominance" else if (s.score.isEmpty) "" else s.score + " " + unit
                // each seat's faction (or clan) emoji before its name (2026-11-25, asked for): the report names the faction as the game
                // shows it, so a Root faction is found by its label in the LFG catalog ("Vagabond (Thief)" by "Vagabond"), a clan by its
                // first word ("Stag Clan"); none for a faction with no emoji
                def icon(s : S) : String = try {
                    if (nortMetas.contains(lobby.meta))
                        (Lfg.coreClans ++ Lfg.bloodClans).find(c => s.faction.toLowerCase.startsWith(c.toLowerCase + " ") || s.faction.equalsIgnoreCase(c)).map(Lfg.clanIcon).getOrElse("")
                    else if (rootMetas.contains(lobby.meta)) {
                        val name = s.faction.takeWhile(_ != '(').trim
                        LfgSetup.catalog(Lfg.root.meta).flatMap(_.options.find(o => o.name.startsWith("IncludeFaction(") && o.label.trim.equalsIgnoreCase(name)))
                            .map(o => LfgSetup.emojiText(o.name)).getOrElse("")
                    }
                    // Pincer's colors and Company of Heroes' factions by the emoji their LFG picks show (2026-11-27)
                    else Lfg.sideOf(lobby.meta, s.faction).map(_.emoji).filter(_.nonEmpty).map(_ + " ").getOrElse("")
                } catch { case _ : Exception => "" }
                val winners = seats.filter(_.won)
                val ids = winners.flatMap(w => mentionOf(w.user))
                def who(s : S) = "**" + nameOf(lobby, s.user) + "**" + mentionOf(s.user).map(i => " (<@" + i + ">)").getOrElse("")
                val title = "**" + (if (lobby.title.nonEmpty) lobby.title else "the game") + "**"
                val head = winners match {
                    case Nil => "🏁 " + title + " has ended."
                    case w :: Nil => "🎉 Congratulations to " + who(w) + " on winning " + title + " as the **" + w.faction + "**" + (if (points(w).nonEmpty) ", with " + points(w) else "") + "!"
                    case ww => "🎉 Congratulations to " + ww.map(who).mkString(" and ") + " on winning " + title + " together!"
                }
                val lines = seats.sortBy(s => (!s.won, -s.rank, s.seat)).map(s => (if (s.won) "🏆 " else "▫️ ") + icon(s) + nameOf(lobby, s.user) + " (" + s.faction + ")" + (if (points(s).nonEmpty) " · " + points(s) else ""))
                post(game, lobby, head + "\n**Final scores**\n" + lines.mkString("\n"), ids)
            }
        }
    }
    catch { case e : Exception => println("Turn pings: final scores not posted: " + e) }

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

    // the log entries that end one turn or phase and start another (Root's turns, Northgard's turns and the year's phases): a seat
    // waited for on both sides of one is asked again in a new wait
    val boundaries = List("StartPlayerTurnAction", "EndTurnAction", "NextTurnAction", "StartYearAction", "ActionsPhaseAction",
        "CreaturePhaseAction", "HarvestAction", "WinterAction")

    def crossed(game : String, from : Int, to : Int) : Boolean =
        run(entries.filter(_.journalId === game).filter(_.index >= from).filter(_.index < to).map(_.text).result)
            .exists(t => boundaries.exists(b => t.startsWith(b + "(") || t == b))

    def waiting(game : String, index : Int, waiting : Set[String]) : Unit = synchronized {
        // (Turns.report has already dropped stale reports, and kept those after a fix shortened the log)
        val before = latest.get(game)
        if (before.exists(b => b.index == index && b.users == waiting))
            return

        val now = System.currentTimeMillis
        val all = before.map(_.users).getOrElse(Set())

        // seats still waited for after the log passed a turn or phase boundary: a new wait
        val fresh = if (before.exists(_.index < index) && (all & waiting).nonEmpty && crossed(game, before.get.index, index)) all & waiting else Set[String]()
        if (fresh.nonEmpty) {
            Files.createDirectories(dir)
            Ranking.append(state, row(game, index, (all -- fresh).toList.sorted.mkString(","), Ranking.now()))
        }
        val was = all -- fresh

        // a wait of a day or more just ended: the slow turn's troll gets the crow (Heckler.slow; 2026-11-25)
        // (only when the game now waits on someone else: a stray report waiting on nobody ends no turn)
        if (waiting.nonEmpty) (was -- waiting).foreach(u => since.get((game, u)).filter(t => now - t >= 24 * 3600 * 1000L).foreach(t => Heckler.slow(game, u, (now - t) / 3600000)))

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
        // "<tag>/<faction>", and for a battle or an Entreat "/<other faction>" too (who attacks whom, who entreated; 2026-11-22)
        val (tag, faction, other) = r.split("/", 3).toList match {
            case t :: f :: o :: _ => (t, Some(f), Some(o).filter(_.nonEmpty))
            case t :: f :: Nil => (t, Some(f), None)
            case t :: _ => (t, None, None)
            case _ => ("", None, None)
        }
        val nort = nortMetas.contains(lobby.meta)
        // the faction's (or clan's) emoji: the one the reason names, else the seat's own when the seat is a faction (Root's and
        // Northgard's standard games; an Advanced Setup or Adset seat drafts its faction in the game, so only a reason names it)
        def iconOf(f : String) = if (nort) Lfg.clanIcon(f) else LfgSetup.emojiText("IncludeFaction(" + f + ")")
        val head = gameIcon.get(lobby.meta).getOrElse(faction.map(iconOf).filter(_.nonEmpty).orElse(seat.map(iconOf).filter(_.nonEmpty)).getOrElse(""))
        // (the server's are :sword-root: and :axe-ng:, seen on 2026-11-21)
        def sword = LfgSetup.serverNamed(List("swordroot", "sword", "swords", "battle", "attack")).getOrElse("⚔️ ")
        def axe = LfgSetup.serverNamed(List("axeng", "axe", "battleaxe", "fight")).getOrElse("🪓 ")
        def e(key : String) = LfgSetup.emojiText(key)
        def w(icon : String) = icon.trim
        // a Northgard reason's icon never the bot's own (application) emoji: only the bot can post those, so a ping holding one went out
        // from the bot instead of the Northgard turns webhook (2026-11-27), and a heckler's post dropped it; the server's emoji, else Unicode
        def nortIcon(key : String, unicode : String) = Some(e(key)).filter(i => i.nonEmpty && !Tournament.appEmojis(i)).getOrElse(unicode + " ")
        // the other faction as its emoji and name ("by <emoji>**Marquise de Cat**"), its name from the LFG catalog when it has one
        // (a Northgard creature comes as "~<its name>": the Discord server's emoji of that name, if any, and the name)
        def creature(n : String) = LfgSetup.serverNamed(List(LfgSetup.norm(n))).getOrElse("") + "the **" + n + "**"
        val them = other.map { o => if (o.startsWith("~")) creature(o.drop(1)) else {
            val key = "IncludeFaction(" + o + ")"
            val name = if (nort) o else LfgSetup.catalog(Lfg.root.meta).map(_.label(key)).filter(_ != key).getOrElse(o)
            iconOf(o) + "**" + name + "**"
        } }
        // PINCER AND COMPANY OF HEROES (2026-11-27, asked for): the game's icon, then the seat's color or faction emoji, and why
        if (sideReasons.contains(lobby.meta)) {
            def side(f : String) = Lfg.sideOf(lobby.meta, f)
            val mine = faction.orElse(seat).flatMap(side).map(_.emoji).filter(_.nonEmpty).map(_ + " ").getOrElse("")
            val vs = other.map(o => side(o).map(x => (if (x.emoji.nonEmpty) x.emoji + " " else "") + "**" + x.label + "**").getOrElse("**" + o + "**"))
            def by(word : String) = vs.map(" " + word + " " + _).getOrElse("")
            val words = (lobby.meta, tag) match {
                case ("pincer", "setup") => "place your first colony"
                case ("pincer", "defend") => "you're under attack ⚔️" + by("by") + ": retreat, fortify or take the hit"
                case ("pincer", "counter") => "counter-action ⚔️" + by("against") + "?"
                case ("pincer", "peace") => vs.map(_ + " offers peace 🕊️").getOrElse("a peace offer 🕊️")
                case ("pincer", "surrender") => "pick a colony to keep 🏚️" + by("from")
                case ("pincer", "prisoner") => "place a returned prisoner ⛓️"
                case ("pincer", "revive") => "revive your frigate ⛵?"
                case ("pincer", "founders") => "Founder's Oath 📜: pick the harvest" + by("for")
                case ("pincer", "harvest") => "pick your harvest 🌾"
                case ("tactics", "setup") => "time to set up"
                case ("tactics", "ops") => "pick the Operation card 📋"
                case ("tactics", "fire") => "Damage Phase: assign your fire 🎯"
                case ("tactics", "brace") => "your targeted units can brace 🛡️"
                case ("tactics", "smoke") => "smoke your vehicles 💨?"
                case ("tactics", "retreat") => "Final Retreats 🏃"
                case ("tactics", "demolish") => "demolitions 💥"
                case ("tactics", "supply") => "Supply Phase: purchase and spawn 🏭"
                case (_, "asked") => "you're needed"
                case _ => "it's your turn"
            }
            return (head + mine, words)
        }
        val (icon, words) = if (gameIcon.contains(lobby.meta)) (head, if (tag == "asked") "you're needed" else "it's your turn") else (nort, tag) match {
            // (the sword or axe right after "attack", "battle" or "fight", 2026-11-22, asked for; the Partisans card right after "Partisans", 2026-11-23; a plain Unicode 🏥 after "Field Hospitals", 2026-11-23, asked for; trading and Winter with plain Unicode ⚖️ and ❄️ after
            // the words, not the bot's badges, so the Northgard webhook can post them itself, 2026-11-23)
            case (false, "defend") => (head, "you're under attack " + w(sword) + them.map(" by " + _).getOrElse(""))
            case (false, "battle") => (head, "your battle " + w(sword) + them.map(" against " + _).getOrElse("") + " needs you")
            // a Twilight Council Banish (2026-11-24, asked for: it said "under attack" with the sword): the dashing-away puff, as the
            // Bats drive the warriors out rather than kill them
            case (false, "banished") => (head, "you're being banished 💨" + them.map(" by " + _).getOrElse(""))
            case (false, "banish") => (head, "your banish 💨" + them.map(" of " + _).getOrElse("") + " needs you")
            case (false, "keep") => (e("StandardDeck"), "pick the cards you keep")
            case (false, "handlimit") => (e("StandardDeck"), "discard down to your hand limit")
            case (false, "draft") => (e("title"), "your pick in the faction draft")
            case (false, "pickdeck") => (e("StandardDeck"), "pick the deck")
            case (false, "pickmap") => (e("RandomOfficialMap"), "pick the map")
            case (false, "picklandmark") => (e("TowerLandmark"), "pick the landmark for the Tower's clearing")
            case (false, "partisans") => (head, ("Partisans " + w(e("ExilesDeck"))).trim + " can join your battle")
            case (false, "prisoners") => (e("IncludeFaction(KD)"), "prisoners to deal with")
            // (a "negotiate" reason, from a page on the client before 2026-11-23, is just the seat's turn: negotiations are never owed,
            // and the ping reminded the enemy of them)
            case (false, "negotiate") => (head, "it's your turn")
            case (false, "entreat") => (e("IncludeFaction(TC)"), them.map(_ + " entreated you").getOrElse("you were entreated"))
            case (false, "hospitals") => (e("IncludeFaction(MC)"), "Field Hospitals 🏥: save your cats?")
            // a draw from the deck or the Lilypad Diaspora's Pond (2026-11-24, asked for): the deck's card emoji and a lilypad (the
            // Discord server's emoji of that name, else the Unicode lotus)
            case (false, "pond") => (head, "draw from the deck " + w(e("StandardDeck")) + " or the Pond " + w(LfgSetup.serverNamed(List("lilypad", "lilypads", "lilypond", "pond")).getOrElse("🪷")))
            case (false, "setup") => (head, "time to set up")
            case (true, "defend") => (head, "you're under attack " + w(axe) + them.map(" by " + _).getOrElse(""))
            case (true, "fight") => (head, "your fight " + w(axe) + them.map(" against " + _).getOrElse("") + " needs you")
            case (true, "trade") => (head, "harvest trading ⚖️")
            case (true, "winter") => (head, "Winter ❄️ is here")
            case (true, "kobold") => (head, "the Kobolds offer an exchange ⚖️")
            // the Discord server's Northgard card emoji (:ng_card:; 2026-11-27, asked for: the ping showed the bot's cards badge, which only
            // the bot can post, so the ping came from the bot, not the Northgard webhook, and the heckler's lost it), else a Unicode card
            case (true, "cards") => (head, "pick a card " + w(LfgSetup.serverNamed(List("ngcard", "card", "cards", "cardback", "cardbacks")).getOrElse("🃏")))
            case (true, "draft") => (nortIcon("title", "📜"), "your pick in the clan draft")
            case (true, "setup") => (head, "time to place your clan")
            case (true, "raid") => (nortIcon("ModuleOption(Sea)", "⛵"), "your raid")
            case (true, "event") => (nortIcon("ModuleOption(EventsModule)", "🎲"), "an event needs you")
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
            return "not pinged: pinged in the last minute"
        val lobby = lobbyOf(game).getOrElse(return "not pinged: lobby not found")
        if (!metas.contains(lobby.meta))
            return "not pinged: not a game with turn pings"
        if (table(game).isEmpty && webhookFor(lobby.meta).isEmpty)
            return "not pinged: no turns webhook (the " + slotOf(lobby.meta).name + " change)"
        val id = mentionOf(user, table(game).nonEmpty).getOrElse(return "not pinged: no Discord, or turn pings off")

        // a game with no thread of its own yet gets its post first (a tournament table's game uses its table's thread)
        if (table(game).isEmpty && webhookFor(lobby.meta).nonEmpty && !introduced(game))
            introduce(game, lobby, "Ongoing")

        sent = pings + ((game, user) -> (now :: pings.getOrElse((game, user), Nil)))
        Files.createDirectories(dir)
        Ranking.append(pinged, row(Ranking.now(), game, user))

        val (icon, words) = reasonOf(game, lobby, user)
        val text = icon + "<@" + id + ">, " + (if (reminder) "a reminder: " + (if (words == "it's your turn") "it's still your turn" else words) else words) + " in **" + lobby.title + "**" + " (" + nameOf(lobby, user) + ")."

        // a player with the troll role (Heckler.scala; 2026-11-25) gets one message from the game's heckler, the ping with its jab under
        // it, in place of the turns webhook's own ping and a second message (asked for: "don't have them get double pinged, the Court Crow
        // should replace Command Warren"); everyone else gets the plain ping
        val why = lobby.lines.map(_.split(" ").toList).collectFirst { case "user" :: s :: u :: _ if u == user => s }.flatMap(s => Turns.reason(game, s)).getOrElse("")
        Heckler.ping(lobby.meta, why.split("/").head, why.split("/").lift(1).getOrElse(""), id, text, reminder, (t, persona) => post(game, lobby, t, List(id), persona, now = true), game)
        "pinged"
    }
}
