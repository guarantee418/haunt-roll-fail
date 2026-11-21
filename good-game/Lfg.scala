package hrf.gg

import slick.jdbc.HsqldbProfile.api._

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path, Paths}

// DISCORD LFG GAMES (asked for on 2026-11-20): Root games set up from a looking-for-group post on Discord, so nobody has to hand out
// seat links. In the LFG channel, /root (players 2 to 6, a title if wanted) has the Discord bot (DiscordBot.scala) post the game with
// Join, Leave, Start and Cancel buttons. When its seats fill, or when its host presses Start with two players or more, the server
// creates the game itself, as the tournament's games are made (Tournament.create): an Advanced Setup game (root-adset) with the
// options the site gives a new game by default, as the latest client to open Play Online sent them (POST /lfg/template, kept in
// turns/lfg-template.tsv: meta, version, options), its map drawn as the client draws it (startOptions). The post then has a Get my
// link button: each player gets their own seat link in a reply only they see. The game gets its post in the turns channel as any
// online game (TurnPings.lobbyWritten), and its players are pinged there on their turns by the Discord user IDs they joined with,
// unless they turned turn pings off for the ranking name they play under.
//
// NORTHGARD (2026-11-21, the same for Northgard): /northgard posts a Northgard: Adset game (nort-adset, the clan draft, as Root's
// Advanced Setup drafts the factions), pinging the ng-lfg role; its setup panel and the command's options are Northgard's (Kind,
// LfgSetup.scala), and its game gets its post and turn pings in the Northgard turns channel (TurnPings.webhookFor). Each game is
// a Kind: its meta, the command, the role; a post keeps its meta (a ninth field of its open row; none, an older post, Root's).
//
// Discord sends the slash command and the button presses to POST /discord/interactions, signed with the application's public key
// (HRF_DISCORD_PUBLIC_KEY, or the file discord-public-key, set by the tournament change "lfg-key <key>"); /root is registered in
// the Discord server (DiscordBot.guild) with the bot's token when the key is set and at the server's start.
//
// Mentions: Discord user IDs only (allowed_mentions), never @everyone, and the root-lfg role once, when a post goes up (`announced`). The
// lists in the post are its allowed mentions, so it is highlighted for them (the first version notifies them, as its host and first
// players; an edit notifies no one); the players are pinged once more when their game starts.
//
// STANDARD NORTHGARD (2026-11-21): /northgard's mode picks a standard game (nort, the default) or Adset. A standard game's clans are
// picked on Join: Join (and Clan, to change it) shows the player a menu of the clans not taken, the core and New Blood ones, or a
// random core or random clan, drawn when the game starts; the pick edits the post with the bot's token (the post is the bot's own
// message). The game starts when every seat is taken and every player has a clan, or when the host presses Start and all have one;
// each clan gets its seat's color and team as the setup screen gives them (ColorOption, TeamOption).
//
// File turns/lfg.tsv, the posts' events (the last state counts):
//   time, open, id, host's Discord ID, host's name, seats, title, options, meta
//   time, join, id, Discord ID, name, clan    time, clan, id, Discord ID, clan    time, leave, id, Discord ID    time, cancel, id
//   time, start, id, lobby, game, spectator secret, Discord IDs, seat users, seat secrets (comma-separated, in seat order)
object Lfg {
    import GoodGame._
    import Backup.{rows, row}

    val file = TurnPings.dir.resolve("lfg.tsv")

    // a game LFG posts set up: its meta and label, the game's name, the slash command and its description, the role a new post pings
    // (the first of these names the Discord server has), the server's emoji for its posts (LfgSetup.emojiNames), the seats
    case class Kind(meta : String, label : String, game : String, command : String, description : String, roles : List[String], emoji : String, minSeats : Int = 2, maxSeats : Int = 6, clans : Boolean = false) {
        // the client's defaults (POST /lfg/template): Root's keeps its first name
        def template = TurnPings.dir.resolve(if (meta == "root-adset") "lfg-template.tsv" else "lfg-template-" + meta + ".tsv")
    }

    val root = Kind("root-adset", "Root: Advanced Setup", "Root", "root", "Look for players for an online Root game (Advanced Setup) on the site", List("root-lfg"), "title")
    val nortStd = Kind("nort", "Northgard", "Northgard", "northgard", "Look for players for an online Northgard game (standard, or Adset with a clan draft) on the site", List("ng-lfg", "northgard-lfg"), "title-nort", clans = true)
    val nort = Kind("nort-adset", "Northgard: Adset", "Northgard", "northgard", "Look for players for an online Northgard game (standard, or Adset with a clan draft) on the site", List("ng-lfg", "northgard-lfg"), "title-nort")

    // (a command with several kinds picks one with its "mode" option, the first by default)
    val kinds = List(root, nortStd, nort)

    def kindsOf(command : String) = kinds.filter(_.command == command)

    // THE CLANS a standard Northgard game's players pick from, as the client names them, and the random picks
    val coreClans = List("Bear", "Boar", "Goat", "Raven", "Snake", "Stag", "Wolf")
    val bloodClans = List("Dragon", "Horse", "Kraken", "Lynx", "Ox", "Rat", "Squirrel")
    val randomCore = "RandomCore"
    val randomAny = "RandomAny"
    val colors = List("Blue", "Red", "Yellow", "Purple", "Green", "Orange")

    // THE CLAN ICONS (asked for on 2026-11-21): the Discord server's own clan emojis, found by name (bear, bearclan, bear_clan, ng_bear,
    // ...: the name with its non-letters dropped is the clan's, maybe with "ng" before or "clan" after; LfgSetup.emojiMap), or for a clan
    // the server has none of, the clan emblem as the bot's own application emoji (ng_bear ...), uploaded by the server from the client's
    // webp2/nort/images/clan/emoji/<clan>.png at its start; shown in the clan menu and beside each player's clan in the post
    @volatile var assets = "."
    @volatile var clanEmojis : Map[String, String] = Map()

    def emojiName(c : String) = "ng_" + c.toLowerCase

    def syncClanEmojis() : String = (DiscordBot.token match {
        case None => "no bot token"
        case Some(t) =>
            try {
                DiscordBot.get("https://discord.com/api/v10/oauth2/applications/@me", Some(t)).map(b => str(obj(Json.parse(b)).get("id"))).filter(_.nonEmpty) match {
                    case None => "no application"
                    case Some(a) =>
                        val url = "https://discord.com/api/v10/applications/" + a + "/emojis"
                        def have() = DiscordBot.get(url, Some(t)).toList.flatMap(b => obj(Json.parse(b)).get("items").collect { case l : List[_] => l.map(obj) }.getOrElse(Nil))
                            .map(e => str(e.get("name")) -> str(e.get("id"))).filter(_._2.nonEmpty).toMap
                        var found = have()
                        (coreClans ++ bloodClans).foreach { c =>
                            val f = Paths.get(assets, "webp2", "nort", "images", "clan", "emoji", c.toLowerCase + ".png")
                            if (serverEmoji(c).isEmpty && !found.contains(emojiName(c)) && Files.exists(f)) {
                                val image = "data:image/png;base64," + java.util.Base64.getEncoder.encodeToString(Files.readAllBytes(f))
                                DiscordBot.send("POST", url, "{\"name\":" + json(emojiName(c)) + ",\"image\":" + json(image) + "}", Some(t)).foreach { b =>
                                    val e = obj(Json.parse(b))
                                    found += str(e.get("name")) -> str(e.get("id"))
                                }
                            }
                        }
                        clanEmojis = (coreClans ++ bloodClans).flatMap(c => found.get(emojiName(c)).map(c -> _)).toMap
                        (coreClans ++ bloodClans).count(c => serverEmoji(c).nonEmpty) + " clan icons on the server, " + clanEmojis.size + " of the bot's"
                }
            }
            catch { case e : Exception => "failed: " + e.getClass.getSimpleName }
    })

    // the server's emoji for a clan: (name, id, animated)
    def serverEmoji(c : String) : Option[(String, String, Boolean)] = {
        val k = c.toLowerCase
        LfgSetup.emojiMap().toList.sortBy(_._1).collectFirst { case (n, (id, a)) if {
            val m = n.toLowerCase.filter(_.isLetter)
            m == k || m == k + "clan" || m == "ng" + k || m == "ng" + k + "clan" || m == "clan" + k
        } => (n, id, a) }
    }

    // the clan's emoji: the server's, else the bot's own
    def clanEmoji(c : String) : Option[(String, String, Boolean)] = serverEmoji(c).orElse(clanEmojis.get(c).map(id => (emojiName(c), id, false)))

    // in a message, and in a menu option
    def clanIcon(c : String) = clanEmoji(c).map { case (n, id, a) => "<" + (if (a) "a" else "") + ":" + n + ":" + id + "> " }.getOrElse("")
    // a clan as the post and the replies show it: its emoji alone (asked for on 2026-11-21), else its name ("a random clan" ...)
    // (a random pick as ❓ with how many clans it draws from, 7 or 14, asked for on 2026-11-21)
    def clanShown(c : String) = c match {
        case `randomCore` => "❓ (7)"
        case `randomAny` => "❓ (14)"
        case _ => Some(clanIcon(c).trim).filter(_.nonEmpty).getOrElse(clanLabel(c))
    }

    def clanIconField(c : String) = if (c == randomCore || c == randomAny) ",\"emoji\":{\"name\":\"❓\"}" else clanIconField0(c)
    def clanIconField0(c : String) = clanEmoji(c).map { case (n, id, a) => ",\"emoji\":{\"id\":" + json(id) + ",\"name\":" + json(n) + ",\"animated\":" + a + "}" }.getOrElse("")

    def clanLabel(c : String) = c match {
        case `randomCore` => "a random core clan"
        case `randomAny` => "a random clan"
        case "" => "no clan yet"
        case x => x + " Clan"
    }

    def kind(meta : String) = kinds.find(_.meta == meta).getOrElse(root)

    def commands = kinds.map("/" + _.command).mkString(", ")

    def publicKey = Tournament.hook("HRF_DISCORD_PUBLIC_KEY", "discord-public-key")

    // options: the ones its host picked on the setup panel (LfgSetup.scala); none, the site's defaults (the template)
    case class Post(id : String, host : String, size : Int, title : String, players : List[(String, String)], cancelled : Boolean, game : Option[Started], options : List[String] = Nil, meta : String = root.meta, clans : Map[String, String] = Map()) {
        def kind = Lfg.kind(meta)
        def clan(u : String) = clans.getOrElse(u, "")
        // every player has a clan (always, in a game whose clans aren't picked on Join)
        def clansPicked = !kind.clans || players.forall(x => clan(x._1).nonEmpty)
        // the concrete clans other players took
        def taken(u : String) = players.map(_._1).filter(_ != u).map(clan).filter(c => coreClans.contains(c) || bloodClans.contains(c)).toSet
    }
    case class Started(lobby : String, game : String, spectate : String, ids : List[String], users : List[String], secrets : List[String])

    def posts() : Map[String, Post] = {
        var m = Map[String, Post]()
        rows(file).foreach {
            case _ :: "open" :: id :: host :: name :: size :: title :: rest => m += id -> Post(id, host, size.toIntOption.getOrElse(4), title, List(host -> name), false, None, rest.headOption.toList.flatMap(_.split(' ').toList.filter(_.nonEmpty)), rest.lift(1).filter(_.nonEmpty).getOrElse(root.meta))
            case _ :: "join" :: id :: u :: name :: rest => m.get(id).foreach(p => if (!p.players.exists(_._1 == u)) m += id -> p.copy(players = p.players :+ (u -> name), clans = p.clans ++ rest.headOption.filter(_.nonEmpty).map(u -> _)))
            case _ :: "clan" :: id :: u :: c :: _ => m.get(id).foreach(p => m += id -> p.copy(clans = p.clans + (u -> c)))
            case _ :: "leave" :: id :: u :: _ => m.get(id).foreach(p => m += id -> p.copy(players = p.players.filter(_._1 != u), clans = p.clans - u))
            case _ :: "cancel" :: id :: _ => m.get(id).foreach(p => m += id -> p.copy(cancelled = true))
            case _ :: "start" :: id :: lobby :: game :: spectate :: ids :: us :: ss :: _ =>
                def l(s : String) = s.split(',').toList.filter(_.nonEmpty)
                m.get(id).foreach(p => m += id -> p.copy(game = Some(Started(lobby, game, spectate, l(ids), l(us), l(ss)))))
            case _ =>
        }
        m
    }

    def event(fields : Any*) = { Files.createDirectories(TurnPings.dir) ; Ranking.append(file, row((Ranking.now() +: fields) : _*)) }

    // the Discord user ID of an LFG game's seat (a seat user), for turn pings (TurnPings.mentionOf)
    @volatile var seatIds : Map[String, String] = null

    def discordOf(user : String) : Option[String] = {
        if (seatIds == null)
            seatIds = posts().values.flatMap(_.game).flatMap(s => s.users.zip(s.ids)).toMap
        seatIds.get(user)
    }

    // the options the site's latest client gives a new LFG game: (version, options), from Play Online
    def setTemplate(body : String) : String = synchronized {
        body.split('\n').toList.map(_.trim) match {
            case m :: v :: o :: _ if kinds.exists(_.meta == m) && v.nonEmpty && v.length <= 32 && v.forall(c => c.isLetterOrDigit || c == '.' || c == '-') && o.nonEmpty && o.length <= 20000 =>
                val ok = o.forall(c => c.isLetterOrDigit || c == ' ' || c == '(' || c == ')' || c == ',' || c == '_')
                val template = kind(m).template
                if (ok && !rows(template).lastOption.contains(List(m, v, o))) {
                    Files.createDirectories(TurnPings.dir)
                    Files.write(template, (row(m, v, o) + "\n").getBytes(UTF_8))
                }
                if (ok) "ok" else "no"
            case _ => "no"
        }
    }

    // the map drawn for a game, as the client's startOptions (root/meta.scala) draws it (Northgard draws nothing at the start)
    val maps = List("AutumnMap", "WinterMap", "LakeMap", "MountainMap", "TidalMap", "TundraMap", "GloomMap", "GorgeMap", "MarshMap", "UrbanMap", "LegendsMap", "DeepWoodsMap", "WastelandsMap")
    val officialMaps = List("AutumnMap", "WinterMap", "LakeMap", "MountainMap", "GorgeMap", "MarshMap")

    def startOptions(options : List[String]) : List[String] =
        if (!options.contains("RandomMap") && !options.contains("RandomOfficialMap"))
            options
        else {
            val map = scala.util.Random.shuffle(if (options.contains("RandomOfficialMap")) officialMaps else maps).head
            val o = options.map(x => if (x == "RandomMap" || x == "RandomOfficialMap") map else x)
            (if (map != "AutumnMap" && o.contains("DefaultClearings")) o.filter(x => x != "DefaultClearings" && x != "NoClustersClearings") :+ "NoClustersClearings" else o).distinct
        }

    def randomTitle(k : Kind) = if (k.game == "Northgard") {
        val n = scala.util.Random.shuffle(List("Winter", "Saga", "Clan", "Jarl", "Raid", "Feast", "Harvest", "Fame", "Lore", "Valhalla", "Frost", "Blood", "Iron", "Kin", "Hearth", "Storm", "Glory", "Exile"))
        val c = scala.util.Random.shuffle(List("for", "against", "of", "and", "through", "in", "under"))
        n.head + " " + c.head + " " + n.last
    } else {
        val n = scala.util.Random.shuffle(List("Power", "Victory", "Glory", "Destiny", "Might", "Fight", "Right", "Betrayal", "Wood", "Land", "Air", "Ground", "War", "Dominance", "Wind", "Struggle", "Upheaval", "Control", "Command", "Conquest", "Movement", "Revolution", "Season"))
        val c = scala.util.Random.shuffle(List("for", "against", "versus", "through", "and", "of", "in", "as"))
        n.head + " " + c.head + " " + n.last
    }

    // a name as the lobby keeps it (the client's sanitize(32)): no tabs or new lines, no < > " \
    def clean(s : String) = s.filter(c => c >= 32 && c != '<' && c != '>' && c != '"' && c != '\\' && c != '\t').trim.take(32)

    // THE RANKING NAME a Discord user plays under (2026-11-21: LFG games didn't show under Play Online, as a seat is listed there only
    // once its player types a ranking name in it): the claimed name whose Discord they are (given on the Tournament page or set by the
    // change "discord"), else the one their seat in an earlier LFG game played under
    def rankingNameOf(id : String) : Option[String] = {
        val claimed = Ranking.claims()
        rows(Tournament.discords).groupBy(_.headOption.getOrElse("")).values.map(_.last).toList
            .filter(r => r.lift(1).contains(id) || r.lift(2).contains(id)).flatMap(_.headOption).filter(claimed.contains).sorted.headOption.orElse {
                val named = Ranking.seatNames()
                posts().values.toList.flatMap(_.game).flatMap(s => s.ids.zip(s.users)).filter(_._1 == id).flatMap(x => named.get(x._2)).filter(claimed.contains).sorted.headOption
            }
    }

    // the game, made as Tournament.create makes a table's: a host, a lobby, the game and a seat (and a link) for each player and a
    // spectator; a player with a ranking name (rankingNameOf) plays under it from the start, as at the tournament ("name" in the lobby,
    // the seat counted under it, so the game is in their Play Online), and the others' seats are named with their Discord name until
    // they type their own (a "prename", as New Online Game's notes)
    def create(p : Post) : Either[String, Started] = {
        val meta = p.meta
        val (version, options) = (if (p.options.nonEmpty) LfgSetup.catalog(meta).map(c => (c.version, p.options)) else None).getOrElse(rows(p.kind.template).lastOption match {
            case Some(m :: v :: o :: _) if m == meta => (v, o.split(' ').toList.filter(_.nonEmpty))
            case _ => return Left("The server doesn't know the game's options yet: open Play Online on the site once, then press Start again.")
        })

        import Tournament.{execute, newSecret}

        val title = p.title
        val host = User(title + " HOST", newSecret(16), newSecret(16))
        val lobby = newSecret(16)
        val game = newSecret(16)
        val spectator = User("Spectator", newSecret(16), newSecret(16))
        val spectate = newSecret(16)
        val seats = p.players.indices.map(i => (User("Player #" + (i + 1), newSecret(16), newSecret(16)), newSecret(16))).toList

        // a standard Northgard game: each player's clan (random picks drawn now among the clans no one took), the seats named by
        // clan, and each clan the color and team of its place in the seating, as the setup screen gives them
        val clanOf : List[String] = if (!p.kind.clans) Nil else {
            var taken = p.players.map(x => p.clan(x._1)).filter(c => coreClans.contains(c) || bloodClans.contains(c)).toSet
            p.players.map(x => p.clan(x._1)).map {
                case c if c == randomCore || c == randomAny =>
                    val pool = (if (c == randomCore) coreClans else coreClans ++ bloodClans).filterNot(taken.contains)
                    val d = scala.util.Random.shuffle(pool).headOption.getOrElse(scala.util.Random.shuffle((coreClans ++ bloodClans).filterNot(taken.contains)).head)
                    taken += d
                    d
                case c => c
            }
        }
        val seatName = if (p.kind.clans) clanOf else p.players.indices.map(i => "P" + (i + 1)).toList
        val order = scala.util.Random.shuffle(seatName)
        val clanOptions = if (!p.kind.clans) Nil else
            order.zipWithIndex.flatMap { case (c, i) => List("ColorOption(" + c + "," + colors(i % colors.size) + ")", "TeamOption(" + c + "," + (i % 2) + ")") }
        val gameOptions = (if (p.kind.clans) options.filterNot(o => o.startsWith("ColorOption(") || o.startsWith("TeamOption(")) ++ clanOptions else options)
        val ranked = p.players.map(x => rankingNameOf(x._1))
        val dd = Ranking.displays()

        val lines =
            List("meta " + meta, "version " + version, "title " + title) ++
            seats.zip(seatName).map { case ((u, _), f) => "user " + f + " " + u.id } ++
            seats.zip(p.players).zip(ranked).collect { case (((u, _), (_, name)), None) => "prename " + u.id + " " + clean(name) }.filter(_.split(' ').length > 2) ++
            List(
                "seating " + order.mkString(" "),
                "options " + (if (meta == root.meta) startOptions(gameOptions) else gameOptions).distinct.mkString(" "),
                "server " + game
            )

        // a ranked player's seat names itself, as a player typing their name would
        val named = seats.zip(ranked).collect { case ((u, _), Some(k)) => (u.id, "name " + u.id + " " + clean(dd.getOrElse(k, k))) }

        def rights(j : String, u : String, rr : String*) = rr.map(r => AccessRight(j, u, r))

        execute(
            users += host,
            journals += Journal(title + " LOBBY", false, "", "", lobby),
            journals += Journal(title, false, "", "", game),
            accessRights ++= rights(lobby, host.id, "full", "read", "append") ++ rights(game, host.id, "full", "read", "append"),
            users += spectator,
            accessRights ++= rights(lobby, spectator.id, "read", "append") ++ rights(game, spectator.id, "read"),
            plays += Play(lobby, spectator.id, spectate),
            users ++= seats.map(_._1),
            accessRights ++= seats.flatMap { case (u, _) => rights(lobby, u.id, "read", "append") ++ rights(game, u.id, "read", "append") },
            plays ++= seats.map { case (u, s) => Play(lobby, u.id, s) },
            entries ++= (lines.map(t => (host.id, t)) ++ named).zipWithIndex.map { case ((u, t), i) => Entry(lobby, i, u, t) },
            entries += Entry(game, 0, host.id, "StartAction(\"" + version + "\")")
        )

        val s = Started(lobby, game, spectate, p.players.map(_._1), seats.map(_._1.id), seats.map(_._2))
        event("start", p.id, lobby, game, spectate, s.ids.mkString(","), s.users.mkString(","), s.secrets.mkString(","))
        seatIds = null

        seats.zip(ranked).foreach { case ((u, _), k) => k.foreach(k => Ranking.append(Ranking.seats, row(u.id, k))) }

        // its post in the turns channel, with its players and spectator link (and turn pings from there)
        TurnPings.lobbyWritten(lobby, lines)

        Right(s)
    }

    def link(p : Post, secret : String) = Tournament.site + "/play/" + p.meta + "/" + secret

    // DISCORD INTERACTIONS

    def json(s : String) = Tournament.json(s)

    def mention(id : String) = "<@" + id + ">"

    // emoji: a Unicode emoji shown before the label (Join's 👍, asked for on 2026-11-20)
    def button(label : String, style : Int, action : String, id : String, emoji : String = "") =
        "{\"type\":2,\"style\":" + style + ",\"label\":" + json(label) + ",\"custom_id\":" + json("lfg:" + action + ":" + id) +
            (if (emoji.isEmpty) "" else ",\"emoji\":{\"name\":" + json(emoji) + "}") + "}"

    def buttons(bb : String*) = if (bb.isEmpty) "[]" else "[{\"type\":1,\"components\":[" + bb.mkString(",") + "]}]"

    // the post as it stands: its text and buttons; nobody is pinged by an edit; the players it lists are its allowed mentions (`listed`)
    // THE LFG ROLE (asked for on 2026-11-20): a new post pings the Discord server's role of this name (its members asked for LFG
    // posts), once, when it goes up; the post's edits ping no one. Looked up by the bot, for 10 minutes. Every version of the post
    // starts with the role's mention and allows it, so it stays highlighted after a Join or Leave (2026-11-21: the edits dropped it,
    // then kept the text but not the allowance, so nobody saw it highlighted); an edit notifies no one, so only the first one pings.
    // Root's posts ping root-lfg, Northgard's ng-lfg (Kind.roles)
    @volatile var roleCache : (Long, List[(String, String)]) = (0L, Nil)

    def role(k : Kind) : Option[String] = {
        if (System.currentTimeMillis - roleCache._1 > 10 * 60 * 1000) {
            roleCache = (System.currentTimeMillis, roleCache._2)
            try (DiscordBot.token, DiscordBot.guild) match {
                case (Some(t), Some(g)) =>
                    DiscordBot.get("https://discord.com/api/v10/guilds/" + g + "/roles", Some(t)).foreach { b =>
                        Json.parse(b) match {
                            case l : List[_] => roleCache = (System.currentTimeMillis, l.map(obj).map(r => (str(r.get("name")).toLowerCase, str(r.get("id")))).filter(_._2.nonEmpty))
                            case _ =>
                        }
                    }
                case _ =>
            }
            catch { case e : Exception => println("LFG: role not read: " + e) }
        }
        k.roles.flatMap(n => roleCache._2.find(_._1 == n.toLowerCase).map(_._2)).headOption
    }

    // the new post, pinging the LFG role
    def announced(p : Post) : String = message(p)

    // the players the post lists (the host and the players, or the started game's), allowed as mentions so the post is highlighted
    // for each of them, as Discord highlights a message only for those it lets it mention (2026-11-21: the owner saw his own name
    // in a post that wasn't highlighted for him); an edit notifies no one, so a Join or Leave pings nobody
    def listed(p : Post) : List[String] = (p.game.map(_.ids).getOrElse(p.host :: p.players.map(_._1))).filter(_.nonEmpty).distinct.take(100)

    def message(p : Post) : String = {
        val label = p.kind.label
        val picks = if (p.options.isEmpty) "" else LfgSetup.catalog(p.meta).map(c => LfgSetup.summary(c, p.options)).getOrElse("")
        val head = LfgSetup.emojiText(p.kind.emoji)
        val (text, bb) = p.game match {
            case Some(s) =>
                (head + "**" + label + ": " + p.title + "** has started with " + s.ids.size + " players: " + s.ids.map(mention).mkString(", ") + "\n" +
                    "Your seat link was sent to you by DM; **Get my link** gives it again (only you see it). Your turns are pinged in the game's post in the games channel.\n" +
                    "Watch: <" + link(p, s.spectate) + ">",
                    buttons(button("Get my link", 1, "link", p.id)))
            case None if p.cancelled =>
                ("~~" + label + ": " + p.title + "~~ was cancelled by its host.", buttons())
            case None if p.kind.clans =>
                val minSeats = p.kind.minSeats
                (head + "**" + label + ": " + p.title + "**, looking for players (" + p.players.size + "/" + p.size + ")\n" + picks +
                    "Host: " + mention(p.host) + "\n" +
                    "Players: " + p.players.map(x => mention(x._1) + " " + (p.clan(x._1) match {
                        case c if c == randomCore || c == randomAny || clanIcon(c).nonEmpty => clanShown(c)
                        case c => "(" + clanLabel(c) + ")"
                    })).mkString(", ") + "\n" +
                    "Press **Select clan** to join, or to change your clan. The game starts by itself when all " + p.size + " seats are taken and everyone has a clan, or when the host presses **Start** (" + minSeats + " players or more).",
                    // (one button joins and changes the clan, asked for on 2026-11-21; the Clan button of the first posts still works)
                    buttons(button("Select clan", 1, "join", p.id, "👍"), button("Leave", 2, "leave", p.id), button("Start", 3, "start", p.id), button("Cancel", 4, "cancel", p.id)))
            case None =>
                val minSeats = p.kind.minSeats
                (head + "**" + label + ": " + p.title + "**, looking for players (" + p.players.size + "/" + p.size + ")\n" + picks +
                    "Host: " + mention(p.host) + "\n" +
                    "Players: " + p.players.map(x => mention(x._1)).mkString(", ") + "\n" +
                    "Press **Join** to play. The game starts by itself when all " + p.size + " seats are taken, or when the host presses **Start** (" + minSeats + " players or more).",
                    buttons(button("Join", 1, "join", p.id, "👍"), button("Leave", 2, "leave", p.id), button("Start", 3, "start", p.id), button("Cancel", 4, "cancel", p.id)))
        }
        "{\"content\":" + json(role(p.kind).map("<@&" + _ + "> ").getOrElse("") + text) + ",\"components\":" + bb +
            ",\"allowed_mentions\":{\"parse\":[],\"users\":[" + listed(p).map(json).mkString(",") + "]" + role(p.kind).map(r => ",\"roles\":[" + json(r) + "]").getOrElse("") + "}}"
    }

    def reply(kind : Int, data : String) = "{\"type\":" + kind + ",\"data\":" + data + "}"

    // a game set up on the panel (LfgSetup.scala): its post in the channel, as a follow-up to the panel (the host already in)
    def posted(d : LfgSetup.Draft, app : String, token : String) = synchronized {
        val id = Tournament.newSecret(10)
        event("open", id, d.host, clean(d.name), d.n, d.title, d.state.actual.mkString(" "), d.state.c.meta)
        val body = announced(posts()(id))
        Tournament.poster.execute { () =>
            try DiscordBot.send("POST", "https://discord.com/api/v10/webhooks/" + app + "/" + token, body, None)
            catch { case e : Exception => println("LFG: post not made: " + e) }
        }
    }

    // a reply only the one who pressed sees
    def privately(text : String) = reply(4, "{\"content\":" + json(text) + ",\"flags\":64,\"allowed_mentions\":{\"parse\":[]}}")

    // the players pinged once, when their game starts: a follow-up message to the post (the interaction's webhook, no bot rights needed)
    def announceStart(app : String, token : String, p : Post, s : Started) {
        val text = s.ids.map(mention).mkString(" ") + ": **" + p.title + "** is ready. Your seat link is in your DMs from the bot (or press **Get my link** above)."
        val body = "{\"content\":" + json(text) + ",\"allowed_mentions\":{\"parse\":[],\"users\":[" + s.ids.take(100).map(json).mkString(",") + "]}}"
        Tournament.poster.execute { () =>
            try DiscordBot.send("POST", "https://discord.com/api/v10/webhooks/" + app + "/" + token, body, None)
            catch { case e : Exception => println("LFG: start not announced: " + e) }
        }
    }

    // SEAT LINKS BY DM: every player gets their seat link from the bot in a direct message when their game starts (2026-11-21,
    // asked for: the players didn't find Get my link; before, only players the site didn't know yet got one). A player the site
    // doesn't know yet (no ranking name was given their Discord, on the Tournament page or by the change "discord", and none was
    // typed in an LFG game they joined before) gets a word on what to do with it; Get my link still works for anyone the DM missed
    def known(id : String) : Boolean =
        rows(Tournament.discords).groupBy(_.headOption.getOrElse("")).values.map(_.last).exists(r => r.lift(1).contains(id) || r.lift(2).contains(id)) || {
            val named = Ranking.seatNames()
            posts().values.flatMap(_.game).exists(s => s.ids.zip(s.users).exists { case (i, u) => i == id && named.contains(u) })
        }

    def welcome(p : Post, s : Started) {
        if (DiscordBot.configured) {
            val players = s.ids.zip(s.secrets).map { case (id, secret) => (id, secret, known(id)) }
            Tournament.poster.execute { () =>
                players.foreach { case (id, secret, old) =>
                    val game = p.kind.game + (if (p.kind.label.contains(": ")) " (" + p.kind.label.split(": ").last + ")" else "")
                    val text =
                        if (old)
                            "**" + p.title + "**, your " + game + " game, has started.\n" +
                            "**Your seat:** " + link(p, secret) + "\n" +
                            "Keep this link to yourself, as it plays your seat. Your turns are pinged in the game's post on the Discord server."
                        else
                            "Welcome to Clean5110! You joined **" + p.title + "**, an online game of " + game + ".\n" +
                            "**Your seat:** " + link(p, secret) + "\n" +
                            "Open it and type the name you want to play under: that name is yours on the site's rankings from then on. " +
                            "Keep this link to yourself, as it plays your seat. Your turns are pinged in the game's post on the Discord server."
                    try { if (!DiscordBot.dm(id, text)) println("LFG: no DM to a player (they may not take DMs); Get my link still works") }
                    catch { case e : Exception => println("LFG: DM failed: " + e) }
                }
            }
        }
    }

    // Discord's signature (Ed25519, of the timestamp and the body) with the application's public key
    def verified(signature : String, timestamp : String, body : Array[Byte]) : Boolean = publicKey.exists { k =>
        try {
            def hex(s : String) = s.grouped(2).map(Integer.parseInt(_, 16).toByte).toArray
            val spki = hex("302a300506032b6570032100") ++ hex(k)
            val key = java.security.KeyFactory.getInstance("Ed25519").generatePublic(new java.security.spec.X509EncodedKeySpec(spki))
            val v = java.security.Signature.getInstance("Ed25519")
            v.initVerify(key)
            v.update(timestamp.getBytes(UTF_8))
            v.update(body)
            v.verify(hex(signature))
        }
        catch {
            case e : Exception => false
        }
    }

    type J = Map[String, Any]

    def obj(x : Any) : J = x match { case m : Map[_, _] => m.asInstanceOf[J] ; case Some(v) => obj(v) ; case _ => Map() }
    def str(x : Option[Any]) = x.collect { case s : String => s }.getOrElse("")

    // an interaction (None: refused, a bad signature), and Discord's answer
    def interaction(signature : Option[String], timestamp : Option[String], body : Array[Byte]) : Option[String] = {
        if (signature.isEmpty || timestamp.isEmpty || !verified(signature.get, timestamp.get, body))
            return None

        val i = obj(Json.parse(new String(body, UTF_8)))
        val member = obj(i.get("member"))
        val user = obj(member.get("user").orElse(i.get("user")))
        val uid = str(user.get("id"))
        val name = Some(str(member.get("nick"))).filter(_.nonEmpty).orElse(Some(str(user.get("global_name"))).filter(_.nonEmpty)).getOrElse(str(user.get("username")))
        val data = obj(i.get("data"))

        Some(try {
            str(i.get("type")) match {
                case "1" => "{\"type\":1}"
                case "2" if uid.nonEmpty && kinds.exists(_.command == str(data.get("name"))) => command(str(data.get("name")), uid, name, data)
                case "3" if uid.nonEmpty && str(data.get("custom_id")).startsWith("lfgs:") =>
                    LfgSetup.press(uid, str(data.get("custom_id")), data.get("values").collect { case l : List[_] => l.collect { case v : String => v } }.getOrElse(Nil), str(i.get("application_id")), str(i.get("token")))
                case "3" if uid.nonEmpty => press(uid, name, str(data.get("custom_id")), str(i.get("application_id")), str(i.get("token")),
                    data.get("values").collect { case l : List[_] => l.collect { case v : String => v } }.getOrElse(Nil), str(i.get("channel_id")), str(obj(i.get("message")).get("id")))
                case _ => privately("That isn't something I know.")
            }
        }
        catch {
            case e : Exception =>
                println("LFG: interaction failed: " + e)
                privately("Something went wrong on the server.")
        })
    }

    // /root (or /northgard) [players] [title] [options]: a new post, its host already in
    def command(cmd : String, uid : String, name : String, data : J) : String = synchronized {
        val options = data.get("options").collect { case l : List[_] => l.map(obj) }.getOrElse(Nil).map(o => str(o.get("name")) -> o.get("value").orNull).toMap
        // the mode: one of the command's kinds, the first by default
        val k = options.get("mode").flatMap(v => Option(v)).collect { case v : String => v }.flatMap(m => kindsOf(cmd).find(_.meta == m)).getOrElse(kindsOf(cmd).head)
        import k.{minSeats, maxSeats}
        val size = options.get("players").map(v => str(Option(v)).toIntOption.getOrElse(4)).getOrElse(4).max(minSeats).min(maxSeats)
        val title = Some(clean(str(options.get("title").flatMap(Option(_))).replace("*", "").replace("~", "").replace("`", ""))).filter(_.nonEmpty).getOrElse(randomTitle(k))
        // the setup panel, with every option, once the site has sent them (LfgSetup.scala); else the post with the site's defaults
        val picks = LfgSetup.sections(k).map(_._1).flatMap(n => options.get(n).flatMap(v => Option(v)).collect { case v : String => v }.toList.flatMap(v => LfgSetup.translate(k, n, v)))
        LfgSetup.open(k, uid, clean(name), size, title, picks).getOrElse {
            val id = Tournament.newSecret(10)
            event("open", id, uid, clean(name), size, title, "", k.meta)
            reply(4, announced(posts()(id)))
        }
    }

    // the clan menu (only the player sees it): the clans no other player took, and the random picks; the pick edits the post
    // (its channel and message, from the button's interaction, ride in the menu's id)
    def clanMenu(p : Post, uid : String, channel : String, message : String, text : String) = {
        val taken = p.taken(uid)
        val choices = (coreClans ++ bloodClans).filterNot(taken.contains).map(c => (c + " Clan", c)) ++ List(("Random core clan (7)", randomCore), ("Random clan (14)", randomAny))
        val menu = LfgSetup.select("lfg:pick:" + p.id + ":" + channel + ":" + message, "Pick your clan", 1, 1,
            choices.map { case (l, v) => (l, v, p.clan(uid) == v, clanIconField(v), "") })
        reply(4, "{\"content\":" + json(text) + ",\"flags\":64,\"components\":[" + menu + "],\"allowed_mentions\":{\"parse\":[]}}")
    }

    // the post edited with the bot's token, from an interaction that isn't the post's own (the clan menu)
    def editPost(channel : String, msg : String, q : Post) {
        if (channel.nonEmpty && msg.nonEmpty)
            Tournament.poster.execute { () =>
                try DiscordBot.send("PATCH", "https://discord.com/api/v10/channels/" + channel + "/messages/" + msg, message(q), DiscordBot.token)
                catch { case e : Exception => println("LFG: post not edited: " + e) }
            }
    }

    def press(uid : String, name : String, custom : String, app : String, token : String, values : List[String] = Nil, channel : String = "", messageId : String = "") : String = synchronized {
        custom.split(':').toList match {
            // a clan picked in the clan menu
            case "lfg" :: "pick" :: id :: ch :: msg :: Nil =>
                posts().get(id) match {
                    case None => privately("This game isn't known any more.")
                    case Some(p) if p.game.nonEmpty => privately("**" + p.title + "** has already started.")
                    case Some(p) if p.cancelled => privately("**" + p.title + "** was cancelled.")
                    case Some(p) =>
                        val c = values.headOption.getOrElse("")
                        val seated = p.players.exists(_._1 == uid)
                        if (!(coreClans ++ bloodClans ++ List(randomCore, randomAny)).contains(c))
                            privately("That isn't a clan I know.")
                        else if (p.taken(uid).contains(c))
                            clanMenu(p, uid, ch, msg, "**" + c + " Clan** was just taken. Pick another.")
                        else if (!seated && p.players.size >= p.size)
                            privately("All seats are taken.")
                        else {
                            if (seated) event("clan", id, uid, c) else event("join", id, uid, clean(name), c)
                            val q = p.copy(players = if (seated) p.players else p.players :+ (uid -> clean(name)), clans = p.clans + (uid -> c))
                            def done(text : String) = reply(7, "{\"content\":" + json(text) + ",\"components\":[],\"allowed_mentions\":{\"parse\":[]}}")
                            if (q.players.size >= q.size && q.clansPicked)
                                create(q) match {
                                    case Left(why) =>
                                        editPost(ch, msg, q)
                                        done(why)
                                    case Right(s) =>
                                        val r = q.copy(game = Some(s))
                                        editPost(ch, msg, r)
                                        announceStart(app, token, r, s)
                                        welcome(r, s)
                                        done("You play " + clanShown(c) + " in **" + p.title + "**, which has started: your seat link is in your DMs.")
                                }
                            else {
                                editPost(ch, msg, q)
                                done("You play " + clanShown(c) + " in **" + p.title + "**." + (if (q.players.size < q.size) " The game starts when all " + q.size + " seats are taken." else ""))
                            }
                        }
                }
            case "lfg" :: action :: id :: Nil =>
                posts().get(id) match {
                    case None => privately("This game isn't known any more.")
                    case Some(p) =>
                        val host = p.host == uid
                        val seated = p.players.exists(_._1 == uid)
                        def update(q : Post) = reply(7, message(q))
                        def start(q : Post) = create(q) match {
                            case Left(why) => privately(why)
                            case Right(s) =>
                                val r = q.copy(game = Some(s))
                                announceStart(app, token, r, s)
                                welcome(r, s)
                                update(r)
                        }

                        (action, p.game) match {
                            case ("link", Some(s)) =>
                                s.ids.indexOf(uid) match {
                                    case -1 => privately("You aren't playing in **" + p.title + "**. Watch it: <" + link(p, s.spectate) + ">")
                                    case i => privately("Your seat in **" + p.title + "**: " + link(p, s.secrets(i)) + "\nKeep this link to yourself: it plays your seat. Your game is also under Play Online once you type a ranking name in it.")
                                }
                            case (_, Some(_)) => privately("**" + p.title + "** has already started.")
                            case (_, None) if p.cancelled => privately("**" + p.title + "** was cancelled.")
                            case ("join" | "clan", None) if p.kind.clans && seated => clanMenu(p, uid, channel, messageId, "Pick your clan for **" + p.title + "** (now " + clanShown(p.clan(uid)) + ").")
                            case ("clan", None) if p.kind.clans => privately("Press **Select clan** first.")
                            case ("join", None) if p.kind.clans && p.players.size < p.size => clanMenu(p, uid, channel, messageId, "Pick your clan to join **" + p.title + "**.")
                            case ("join", None) if seated => privately("You're already in.")
                            case ("join", None) if p.players.size >= p.size => privately("All seats are taken.")
                            case ("join", None) =>
                                event("join", id, uid, clean(name))
                                val q = p.copy(players = p.players :+ (uid -> clean(name)))
                                if (q.players.size >= q.size && q.clansPicked) start(q) else update(q)
                            case ("leave", None) if host => privately("You're the host: press **Cancel** to call the game off.")
                            case ("leave", None) if !seated => privately("You haven't joined.")
                            case ("leave", None) =>
                                event("leave", id, uid)
                                update(p.copy(players = p.players.filter(_._1 != uid)))
                            case ("start", None) if !host => privately("Only the host (" + mention(p.host) + ") can start the game early.")
                            case ("start", None) if p.players.size < p.kind.minSeats => privately("At least " + p.kind.minSeats + " players are needed.")
                            case ("start", None) if !p.clansPicked => privately("Not everyone has a clan yet: " + p.players.filter(x => p.clan(x._1).isEmpty).map(x => mention(x._1)).mkString(", ") + " (press **Select clan**).")
                            case ("start", None) => start(p)
                            case ("cancel", None) if !host => privately("Only the host (" + mention(p.host) + ") can cancel the game.")
                            case ("cancel", None) =>
                                event("cancel", id)
                                update(p.copy(cancelled = true))
                            case _ => privately("That isn't something I know.")
                        }
                }
            case _ => privately("That isn't something I know.")
        }
    }

    // the commands (/root, /northgard) in the Discord server, with the bot's token; the application is the bot's own. A PUT sets every
    // command the server has, so they all go together
    def register() : String = {
        (DiscordBot.token, DiscordBot.guild) match {
            case (None, _) => "not registered: no bot token (the change bot <token>)"
            case (_, None) => "not registered: no Discord server (a webhook, or the change guild <id>)"
            case (Some(t), Some(g)) =>
                try {
                    val app = DiscordBot.get("https://discord.com/api/v10/oauth2/applications/@me", Some(t)).map(b => str(obj(Json.parse(b)).get("id"))).filter(_.nonEmpty)
                    app match {
                        case None => "not registered: Discord didn't give the bot's application"
                        case Some(a) =>
                            val command = kinds.map(_.command).distinct.map(kindsOf(_)).map { kk =>
                                val k = kk.head
                                // the mode, for a command with several kinds; each option once, with the first kind's choices
                                val mode = if (kk.size < 2) "" else ",{\"type\":3,\"name\":\"mode\",\"description\":\"Standard (clans picked on Join) or Adset (a clan draft); standard if not given\",\"required\":false,\"choices\":[" +
                                    kk.map(x => "{\"name\":" + json(if (x.clans) "Standard" else x.label.split(": ").last) + ",\"value\":" + json(x.meta) + "}").mkString(",") + "]}"
                                val opts = kk.flatMap(LfgSetup.commandOptions).foldLeft(List[(String, String, List[(String, String)])]()) { case (l, o) => if (l.exists(_._1 == o._1)) l else l :+ o }
                                "{\"name\":" + json(k.command) + ",\"type\":1,\"description\":" + json(k.description) + ",\"options\":[" +
                                "{\"type\":4,\"name\":\"players\",\"description\":\"How many players (" + k.minSeats + " to " + k.maxSeats + ", 4 if not given)\",\"required\":false,\"min_value\":" + k.minSeats + ",\"max_value\":" + k.maxSeats + "}," +
                                "{\"type\":3,\"name\":\"title\",\"description\":\"The game's name (a random one if not given)\",\"required\":false,\"max_length\":32}" + mode +
                                opts.map { case (n, d, cc) =>
                                    ",{\"type\":3,\"name\":" + json(n) + ",\"description\":" + json(d + " (the site's default if not given)") + ",\"required\":false,\"choices\":[" +
                                    cc.map { case (label, value) => "{\"name\":" + json(LfgSetup.cut(label, 100)) + ",\"value\":" + json(value) + "}" }.mkString(",") + "]}"
                                }.mkString + "]}"
                            }.mkString("[", ",", "]")
                            if (DiscordBot.send("PUT", "https://discord.com/api/v10/applications/" + a + "/guilds/" + g + "/commands", command, Some(t)).nonEmpty) "registered" else "not registered: Discord refused it"
                    }
                }
                catch {
                    case e : Exception => "not registered: " + e.getClass.getSimpleName
                }
        }
    }

    // directory: the client's, where the clan icons are
    def start(directory : String) {
        assets = directory
        new Thread(() => {
            if (DiscordBot.configured)
                println("LFG: " + syncClanEmojis())
            if (publicKey.nonEmpty)
                println("LFG: " + commands + " " + register())
        }).start()
    }
}
