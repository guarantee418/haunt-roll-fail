package hrf.gg

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.Files

// THE ROAST ME ROLE (2026-11-25, asked for: "a notification role that makes fun of you while you play the game"): a member with the
// role (default "Roast Me"; the owner makes it with the role change and lists it with self-role, so members pick it with /roles) gets,
// under each of their turn pings, a jab from the game's heckler, posted through the same webhook in the same thread under the heckler's
// own name (and picture, where it has one): a webhook can post as any name, per message. Since 2026-11-25 (asked for: no double pings)
// that one message is the ping itself, the jab on its second line, in place of the turns webhook's plain ping; the jab fits why they
// were pinged (Turns.reason's tag, as TurnPings.reasonOf reads it).
//
// The hecklers, one a game, each in its own setting:
//   Root              the Court Crow: a Corvid in a jester's cap (art by haunt-roll-fail/tools/heckler-art.py, served from /art/)
//   Northgard         Ratatoskr (the Squirrel Clan's emblem), the squirrel of Norse myth who runs up and down Yggdrasil carrying insults between the eagle at
//                     the top (Vedrfolnir) and the dragon at the roots (Nidhogg); so his jabs are what "the eagle says"
//   Pincer            the Pamphleteer, an 18th-century broadsheet satirist
//   Company of Heroes the Sergeant Major, over the field radio
// The jabs make fun of the play, never the person, and keep to what a ping already says (nothing only the player may know).
// Root's Court Crow also mocks what happens to a player on anyone's turn (a 3-0 roll against them, a Raze, a Revolt, a Lizard
// conversion ...; jests, below, and root/jests.scala).
//
// The tournament changes:
//   roast-role <role>   the role that gets the jabs, if not "Roast Me" (roast-role off: no jabs at all)
//   roasts              the role, whether the server has it, and how many members have it
// Kept in tournament/roast-role.txt.
object Heckler {
    val file = Tournament.dir.resolve("roast-role.txt")
    val defaultRole = "Roast Me"

    def roleName : Option[String] =
        if (!Files.exists(file)) Some(defaultRole)
        else Some(new String(Files.readAllBytes(file), UTF_8).trim).filter(n => n.nonEmpty && n != "off")

    case class Voice(name : String, avatar : String, lines : Map[String, List[String]])

    // the role's ID on the server, looked up again every 10 minutes (a role renamed or made since)
    @volatile var cached : (Long, Option[String]) = (0L, None)

    def roleId : Option[String] = {
        val now = System.currentTimeMillis
        if (now - cached._1 > 10 * 60 * 1000L)
            cached = (now, for { g <- DiscordBot.guild ; n <- roleName ; rr <- DiscordServer.roles(g).toOption ; r <- DiscordServer.findRole(rr, n).toOption } yield r.id)
        cached._2
    }

    def roasted(uid : String) : Boolean = (for {
        g <- DiscordBot.guild
        role <- roleId
        have <- SelfRoles.memberRoles(g, uid).toOption
    } yield have.contains(role)).getOrElse(false)

    // the last jab each player got, so the next is another
    val last = new java.util.concurrent.ConcurrentHashMap[String, String]()

    def pick(uid : String, options : List[String]) : String = {
        val l = options.filterNot(o => Option(last.get(uid)).contains(o)) match { case Nil => options ; case l => l }
        val r = l(scala.util.Random.nextInt(l.size))
        last.put(uid, r)
        r
    }

    def voiceOf(meta : String) : Voice = meta match {
        case "nort" | "nort-adset" => ratatoskr
        case "pincer" => pamphleteer
        case "tactics" => sergeant
        case _ => crow
    }

    // a turn ping (called from TurnPings.attempt; its post goes out on the poster's thread, so the role check, which asks Discord, holds
    // nothing up and the posts keep their order): a player with the role gets it from the game's heckler, under its own name and picture,
    // with the jab on the next line, in one message that pings them once; everyone else gets the plain ping from the turns webhook
    def ping(meta : String, tag : String, uid : String, text : String, reminder : Boolean, post : (String, String) => Unit) {
        Tournament.poster.execute { () =>
            val troll = roleName.nonEmpty && DiscordBot.configured && (try roasted(uid) catch { case e : Exception => println("Heckler: " + e) ; false })
            if (troll) {
                val v = voiceOf(meta)
                val options = if (reminder) v.lines("reminder") else v.lines.getOrElse(tag, v.lines("turn"))
                val line = pick(uid, options).replace("{name}", "<@" + uid + ">")
                post(text + "\n" + line, persona(v) + (if (v == crow) gifFor(if (reminder) "reminder" else tag).getOrElse("") else ""))
            }
            else
                post(text, "")
        }
    }

    // THE CROW'S GIFS (2026-11-25, asked for: "sometimes the crow just sends a trolling gif, with some context"): about one Court
    // Crow post in three carries a GIF under its line, which gives the context. Half of those are the crow's own (webp2/lfg/gifs/, by
    // haunt-roll-fail/tools/heckler-art.py, served from /art/): the dice for a 3-0 roll, the crow popping out of its trash can for something
    // trashed, else the crow rocking with laughter; the other half a laugh from Klipy, the GIF library Discord's own picker searches: the
    // first 18 results of its "lol" search, which the owner picked by screen recording ("use any or all of these", 2026-11-25), linked from
    // Klipy's own servers (the smaller copy of each, under 4 MB, so Discord shows them quickly), as the picker's links are:
    //   prince-shirleytemple           Prince Shirleytemple
    //   simpsons-laugh-1               Simpsons Laugh
    //   haha-117                       Haha
    //   lmfao-too-funny                Lmfao Too Funny
    //   big-lol                        Big Lol
    //   baby-laughing-43               Baby Laughing
    //   blchota-hillary                Hilary Duff Texts LOL
    //   lol-cute-6                     Lol Cute
    //   laughing-218                   Spongebob and Patrick Star Laughing
    //   laugh-out-loud-ridiculous      Laugh Out Loud Ridiculous
    //   lol-6531                       Lol
    //   tom-and-jerry-jerry-mouse-1    Tom And Jerry Jerry Mouse
    //   lol-laugh-out-loud-20          Lol Laugh Out Loud
    //   kitty-forman-debra-jo-rupp-1   Kitty Forman (Debra Jo Rupp) Laughing Hysterically
    //   laughing-pikachu               Pikachu Laughing on a Branch
    //   lol-lmao-100                   Lol Lmao
    //   lmao-lmao-meme                 Lmao Lmao Meme
    //   little-pills-39                Little Pills LOL Animation
    val klipyLaughs = List(
        "https://static.klipy.com/ii/c3a19a0b747a76e98651f2b9a3cca5ff/8a/b1/uAaLRrFA.gif",
        "https://static.klipy.com/ii/4e7bea9f7a3371424e6c16ebc93252fe/18/38/UyPdecHVR8SBPz1.gif",
        "https://static.klipy.com/ii/e293a233a303a98e471f78d04e13a1b0/ac/4d/KZ3vBWqp.gif",
        "https://static.klipy.com/ii/935d7ab9d8c6202580a668421940ec81/c2/4a/2uJzYSWJ.gif",
        "https://static.klipy.com/ii/ce286d05b8e1a47cd4f32b0e1b6dec0e/5e/4c/9jR1Jwnr.gif",
        "https://static.klipy.com/ii/f87f46a2c5aeaeed4c68910815f73eaf/ef/d0/lmB9Dbkn.gif",
        "https://static.klipy.com/ii/c3a19a0b747a76e98651f2b9a3cca5ff/e0/31/40SwAR5S.gif",
        "https://static.klipy.com/ii/e293a233a303a98e471f78d04e13a1b0/da/ba/6bDDB3x4.gif",
        "https://static.klipy.com/ii/e293a233a303a98e471f78d04e13a1b0/1c/a3/McmeX2Fb.gif",
        "https://static.klipy.com/ii/935d7ab9d8c6202580a668421940ec81/a6/72/bJ8ezmXB.gif",
        "https://static.klipy.com/ii/35ccce3d852f7995dd2da910f2abd795/7d/2e/fIZpx06J.gif",
        "https://static.klipy.com/ii/d7aec6f6f171607374b2065c836f92f4/6c/f5/Cgrc4DeB.gif",
        "https://static.klipy.com/ii/e1b92bb53e0c9e442408bc677a56c789/81/e6/RPl8NLhOqZHx.gif",
        "https://static.klipy.com/ii/d6b0ce929193df3c242ac34b5654d2ce/79/f2/OfDbEjjb.gif",
        "https://static.klipy.com/ii/f87f46a2c5aeaeed4c68910815f73eaf/40/4b/DxYVYtuU.gif",
        "https://static.klipy.com/ii/4e7bea9f7a3371424e6c16ebc93252fe/ed/03/qbL0FdsbeUhFelt6bnnA.gif",
        "https://static.klipy.com/ii/4e7bea9f7a3371424e6c16ebc93252fe/ad/4c/dUMUAjdqCO3Y.gif",
        "https://static.klipy.com/ii/925f17378dd1893b674a723c07535afe/4c/63/EBINH69p.gif")

    // EMOTIONAL DAMAGE (2026-11-25, the owner's pick: "the emotional damage, just saying, great"): the Klipy video the owner linked
    // (static.klipy.com/.../N9AZXRDopoEX.mp4), made a GIF, as a webhook embed shows no video (webp2/lfg/gifs/emotional-damage.gif, served
    // from /art/). It is half of the GIFs for the harshest things: a 3-0 roll against them, a Raze, a Revolt, Turmoil, a bomb, a Sanctify,
    // and last place
    val harsh = Set("rolled30", "raze", "revolt", "turmoil", "bomb", "sanctify", "last", "finishedlast")

    // TERRIFIED (2026-11-25, the owner's pick for being "scared from a fight"): the Klipy video the owner linked
    // (static.klipy.com/.../2W84h3Sw.mp4), made a GIF (webp2/lfg/gifs/terrified.gif): half of the GIFs when the player is under attack
    // or being banished
    val scared = Set("defend", "banished")

    // VEGETA IN THE RAIN (2026-11-25, the owner's pick): the Klipy video the owner linked (static.klipy.com/.../2n80SKFK.mp4), made a GIF
    // (webp2/lfg/gifs/vegeta-rain.gif): half of the GIFs for the gloomy things: their own attack whiffing, an ambush, a conversion, a
    // reminder that it's still their turn, a lost game or a long way behind
    val gloomy = Set("whiffed", "ambushed", "convert", "reminder", "lost", "behind")

    def gifFor(tag : String) : Option[String] =
        if (Tournament.site.isEmpty || scala.util.Random.nextInt(3) != 0) None
        else Some(if (scared.contains(tag) && scala.util.Random.nextBoolean()) Tournament.site + "/art/lfg/gifs/terrified.gif"
        else if (gloomy.contains(tag) && scala.util.Random.nextBoolean()) Tournament.site + "/art/lfg/gifs/vegeta-rain.gif"
        else if (harsh.contains(tag) && scala.util.Random.nextBoolean()) Tournament.site + "/art/lfg/gifs/emotional-damage.gif"
        else if (scala.util.Random.nextBoolean()) klipyLaughs(scala.util.Random.nextInt(klipyLaughs.size)) else Tournament.site + "/art/lfg/gifs/" + (tag match {
            case "rolled30" | "whiffed" | "defend" | "battle" => "crow-dice.gif"
            case "raze" | "revolt" | "sanctify" | "bomb" | "turmoil" | "handlimit" | "banished" => "crow-can.gif"
            case _ => "crow-laugh.gif"
        })).map(g => ",\"embeds\":[{\"color\":8798896,\"image\":{\"url\":" + Tournament.json(g) + "}}]")

    // the webhook post's own name and picture for a heckler (Tournament.post's persona)
    def persona(v : Voice) : String =
        ",\"username\":" + Tournament.json(v.name) + (if (v.avatar.nonEmpty && Tournament.site.nonEmpty) ",\"avatar_url\":" + Tournament.json(Tournament.site + "/art/" + v.avatar) else "")

    // OFF-TURN JESTS (2026-11-25, asked for: "have the crow do off turn things, like laugh at you being attacked and your opponent
    // rolled 3-0, or laugh that a mob razed your buildings, a revolt wiped you out, or lizards converting"): Root's clients send what
    // happened to whom lately with their turn reports (the seventh line, "<n>:<tag>/<seat>/<other faction>/<clearing>", numbered from
    // the game's first event: root/jests.scala, MetaGame.jests). Each number is taken once (turns/jests.tsv: game, the last number
    // taken); a game's first report only sets where it starts, so a game running before this, or a page reopened, mocks nothing old.
    // An event whose victim has the Roast Me role gets the Court Crow's post in the game's thread, at most 3 a report.
    val taken = TurnPings.dir.resolve("jests.tsv")

    var lastTaken : Map[String, Int] = null

    def takenOf : Map[String, Int] = {
        if (lastTaken == null)
            lastTaken = Backup.rows(taken).collect { case g :: n :: _ if n.toIntOption.nonEmpty => g -> n.toInt }.toMap
        lastTaken
    }

    def jests(game : String, events : List[String]) : Unit = try {
        val parsed = events.flatMap(e => e.split(":", 2).toList match {
            case n :: rest :: Nil if n.toIntOption.nonEmpty => Some(n.toInt -> rest.split("/").toList)
            case _ => None
        })
        val top = parsed.map(_._1).maxOption.getOrElse(0)
        val fresh = synchronized {
            takenOf.get(game) match {
                case None =>
                    lastTaken = takenOf + (game -> top)
                    java.nio.file.Files.createDirectories(TurnPings.dir)
                    Ranking.append(taken, Backup.row(game, top))
                    Nil
                case Some(was) if top > was =>
                    lastTaken = takenOf + (game -> top)
                    Ranking.append(taken, Backup.row(game, top))
                    parsed.filter(_._1 > was)
                case _ => Nil
            }
        }
        if (fresh.nonEmpty && roleName.nonEmpty && DiscordBot.configured)
            TurnPings.lobbyOf(game).filter(l => TurnPings.rootMetas.contains(l.meta)).foreach { lobby =>
                val seats = lobby.lines.map(_.split(' ').toList).collect { case "user" :: f :: u :: _ => f -> u }.toMap
                val table = TurnPings.table(game).nonEmpty
                fresh.sortBy(_._1).take(3).foreach { case (_, tag :: seat :: rest) =>
                    val other = rest.headOption.getOrElse("")
                    val where = rest.lift(1).getOrElse("")
                    for { lines <- crowJests.get(tag) ; user <- seats.get(seat) ; id <- TurnPings.mentionOf(user, table) }
                        Tournament.poster.execute { () =>
                            try {
                                if (roasted(id)) {
                                    val line = pick(id, lines).replace("{name}", "<@" + id + ">").replace("{other}", factionName(other)).replace("{where}", if (where.nonEmpty) where else "the woods")
                                    TurnPings.post(game, lobby, line, Nil, persona(crow) + gifFor(tag).getOrElse(""), now = true)
                                }
                            }
                            catch {
                                case e : Exception => println("Heckler: " + e)
                            }
                        }
                case _ =>
                }
            }
    }
    catch {
        case e : Exception => println("Heckler: jests not taken: " + e)
    }

    // a Root faction's name, as the LFG catalog has it (Lfg.root.meta), else its short name; the bot's own emojis are left out, as
    // only the bot can post them and these go out through the webhook
    def factionName(short : String) : String = if (short.isEmpty) "someone" else {
        val key = "IncludeFaction(" + short + ")"
        "**" + LfgSetup.catalog(Lfg.root.meta).map(_.label(key)).filter(_ != key).getOrElse(short) + "**"
    }

    val crowJests = Map(
        "rolled30" -> List(
            "HA! {other} rolled 3-0 against {name} in {where}. The dice have chosen a side, and it isn't yours.",
            "Caw caw caw! A 3-0 roll at {where}. {name}, your warriors didn't even get to swing.",
            "{other} rolled a 3, {name} rolled a 0. I'd call it a battle, but that would be generous.",
            "3-0 at {where}! {name}, have you tried rolling the die the other way up?"),
        "whiffed" -> List(
            "{name} attacked {other} at {where} and rolled a 0. Bold strategy. Terrible dice.",
            "Caw! {name} charged into {where} and rolled 0-3. {other} sends their thanks.",
            "{name}'s attack at {where}: zero hits dealt, three taken. I've seen sparrows do better."),
        "ambushed" -> List(
            "AMBUSH! {name} walked right into {other}'s trap at {where}. The crows saw it coming. Everyone saw it coming.",
            "{name} marched into {where} without checking the bushes. {other} was in the bushes.",
            "Ambushed at {where}, {name}! Rule one of the Woodland: never trust a quiet clearing."),
        "raze" -> List(
            "The Mob just razed {name}'s buildings in {where}. Don't worry, rubble has a rustic charm.",
            "{other}'s rats burned down {where}. {name}, I hope you weren't attached to anything there.",
            "Raze! {name}'s things in {where} are now a nice open field. The crows thank you for the view.",
            "{name}, the Mob came to {where}. The Mob left. Your buildings did not."),
        "revolt" -> List(
            "REVOLT in {where}! {name} has been wiped out of it by {other}. Viva la Woodland!",
            "The {other} rose up in {where} and {name} went down. Should have listened to the people, {name}.",
            "{name}, {where} has revolted. Everything you had there is now a very sympathetic base.",
            "Caw! A Revolt in {where} cleared out {name}. Sympathy: one. {name}: zero."),
        "convert" -> List(
            "{other} converted one of {name}'s warriors in {where}. It looks happier already.",
            "{name}, your warrior in {where} has found religion. Lizard religion.",
            "Another of {name}'s warriors joins the Cult in {where}. They say the robes are very comfortable."),
        "sanctify" -> List(
            "{other} sanctified {name}'s building in {where}. It's a garden now. A lovely garden. Not yours.",
            "{name}, your building in {where} has been redecorated by {other}. Very green. Very lizard."),
        "turmoil" -> List(
            "TURMOIL! {name}'s Eyrie tripped over its own Decree. The crows are crying with laughter.",
            "{name}, the Decree collapsed. Your leader has been deposed. Your nobles are embarrassed. I'm delighted.",
            "Caw! The Eyrie's Decree broke and {name} fell into Turmoil. Birds of a feather fall together."),
        "bomb" -> List(
            "BOOM! A Corvid bomb went off on {name}'s warriors in {where}. That was one of ours. You're welcome.",
            "{name}, that plot in {where}? It was a bomb. It's always a bomb."))

    // the tournament changes
    val words = Set("roast-role", "roasts", "troll-intro")

    def change(word : String, rest : String) : String = word match {
        case "roast-role" =>
            if (rest.isEmpty) "not done: roast-role <role> or roast-role off"
            else {
                Files.createDirectories(file.getParent)
                Files.write(file, (rest.trim.stripPrefix("@") + "\n").getBytes(UTF_8))
                cached = (0L, None)
                if (rest.trim == "off") "no jabs" else "set; " + status
            }
        case "roasts" => status
        case "troll-intro" => intro()
    }

    // FIRST TROLL PINGS (2026-11-25, asked for: "look for a reason to give each troll member their first troll ping", then "not the
    // welcome, just look through their games and find something"): the "troll-intro" change gives every member with the role one jab, at
    // once, by their game's heckler, in the thread of the game it is about, for the first of these found in their games:
    //   waiting      a game still going has been waiting on them (TurnPings' wait, and for how long)
    //   last         they're last on VP in a Root game still going (the clients' status reports, Turns.status), to the leader
    //   finishedlast they finished last in a finished game (ranking/results.tsv: its seats, factions and final scores)
    //   pings        a game needed 4 or more turn pings for them (turns/pings.tsv)
    //   behind       8 VP or more behind the leader in a Root game still going
    //   lost         they lost a finished game, to its winner
    // Nothing found, nothing posted: there is no welcome. Each member gets one (turns/troll-intros.tsv, their Discord user ID), and running
    // it again only reaches members who got nothing before. The result says what each got, or why nothing.
    val intros = TurnPings.dir.resolve("troll-intros.tsv")

    // the server's members with the role (Discord gives 1000 a page)
    def roleMembers(g : String, role : String) : List[String] = {
        var out = List[String]()
        var after = "0"
        var more = true
        while (more) {
            val page = DiscordServer.call("GET", "/guilds/" + g + "/members?limit=1000&after=" + after).toOption.map(DiscordServer.list).getOrElse(Nil)
            page.foreach { m =>
                val id = m.get("user").collect { case u : Map[_, _] => u.asInstanceOf[Map[String, Any]] }.map(DiscordServer.str(_, "id")).getOrElse("")
                val roles = m.get("roles").collect { case l : List[_] => l.collect { case s : String => s } }.getOrElse(Nil)
                if (id.nonEmpty && roles.contains(role))
                    out :+= id
                if (id.nonEmpty) after = id
            }
            more = page.size == 1000
        }
        out
    }

    case class Seat(game : String, lobby : TurnPings.Lobby, seat : String, user : String)

    // a finished game's seat, from its reported result: won, faction, score
    case class Finish(seat : Seat, won : Boolean, faction : String, score : Int, winner : String, top : Int, lowest : Boolean, time : String)

    def intro() : String = {
        val g = DiscordBot.guild.getOrElse(return "not done: no Discord server known")
        cached = (0L, None)
        val role = roleId.getOrElse(return "not done: " + status)
        val done = Backup.rows(intros).flatMap(_.headOption).toSet
        val members = roleMembers(g, role)
        if (members.isEmpty)
            return "no member has the role"
        val now = System.currentTimeMillis

        // the Discord user ID each seat's user pings (asked once per user: a username may need the bot to look it up)
        val ids = scala.collection.mutable.Map[(String, Boolean), Option[String]]()
        def idOf(u : String, game : String) = { val t = TurnPings.table(game).nonEmpty ; ids.getOrElseUpdate((u, t), TurnPings.mentionOf(u, t)) }

        // a game the heckler can post in: a tournament table, or a game whose turns webhook is set
        def postable(l : TurnPings.Lobby, game : String) = TurnPings.table(game).nonEmpty || TurnPings.webhookFor(l.meta).nonEmpty

        // every seat of every game the site has turn reports for, by the Discord user ID it pings
        val seats : Map[String, List[Seat]] = Turns.all.keys.toList.sorted.flatMap { game =>
            TurnPings.lobbyOf(game).filter(l => TurnPings.metas.contains(l.meta) && postable(l, game)).toList.flatMap { l =>
                l.lines.map(_.split(' ').toList).collect { case "user" :: s :: u :: _ => (s, u) }.flatMap { case (s, u) =>
                    idOf(u, game).map(_ -> Seat(game, l, s, u))
                }
            }
        }.groupBy(_._1).map { case (k, l) => k -> l.map(_._2) }

        // every seat of every finished game, newest first, by the Discord user ID it pings
        val finished : Map[String, List[Finish]] = Backup.rows(Ranking.results).flatMap {
            case game :: meta :: time :: _ :: sides :: _ if TurnPings.metas.contains(meta) =>
                val all = sides.split(';').toList.flatMap(t => t.split(":", 2).toList match {
                    case w :: rest :: Nil if w == "W" || w == "L" => rest.split('/').toList match {
                        case u :: f :: _ :: _ :: sc :: _ => Some((w == "W", u, f.replace('_', ' '), if (sc == "D") 999 else sc.toIntOption.getOrElse(0)))
                        case _ => None
                    }
                    case _ => None
                })
                val win = all.filter(_._1).maxByOption(_._4)
                TurnPings.lobbyOf(game).filter(l => postable(l, game)).toList.flatMap { l =>
                    all.filter(_._2 != "*").flatMap { case (won, u, f, sc) =>
                        idOf(u, game).map(_ -> Finish(Seat(game, l, "", u), won, f, sc, win.map(_._3).getOrElse("the winner"), win.map(_._4).getOrElse(0),
                            all.size > 1 && all.forall(_._4 >= sc) && all.exists(_._4 > sc), time))
                    }
                }
            case _ => Nil
        }.groupBy(_._1).map { case (k, l) => k -> l.map(_._2).sortBy(_.time).reverse }

        def going(s : Seat) = !TurnPings.over(s.game)

        def vp(t : String) = t.split(":").toList match { case _ :: f :: v :: _ => Some((f.replace('_', ' '), if (v == "D") 999 else v.toIntOption.getOrElse(0))) ; case _ => None }

        // a Root game's standings from its last status report: (faction, VP) by seat
        def standings(s : Seat) = Turns.status(s.game).filter(_.contains(":")).flatMap(t => t.split(":").headOption.flatMap(seat => vp(t).map(seat -> _)))

        def score(meta : String, n : Int) = if (n == 999) "a dominance" else n + (if (TurnPings.nortMetas.contains(meta)) " fame" else " VP")

        members.map { id =>
            if (done.contains(id))
                "<@" + id + ">: had their first one already"
            else {
                val mine = seats.getOrElse(id, Nil)
                val live = mine.filter(going)
                val ends = finished.getOrElse(id, Nil)
                val v = (s : Seat) => voiceOf(s.lobby.meta)
                def line(s : Seat, key : String) = v(s).lines.get(key).map(l => pick(id, l).replace("{name}", "<@" + id + ">").replace("{game}", "**" + s.lobby.title + "**"))

                val waiting = live.flatMap { s =>
                    TurnPings.latest.get(s.game).filter(_.users.contains(s.user)).map(_ => s -> TurnPings.since.getOrElse((s.game, s.user), 0L))
                }.sortBy(_._2).headOption.flatMap { case (s, since) =>
                    val hours = if (since > 0) ((now - since) / (60 * 60 * 1000L)).toInt else -1
                    val how = if (hours < 0) "a while" else if (hours < 1) "a few minutes" else if (hours == 1) "an hour" else if (hours < 48) hours + " hours" else (hours / 24) + " days"
                    line(s, "intro-waiting").map(l => (s, "waiting", l.replace("{time}", how)))
                }

                def vpLine(key : String, ok : (Int, Int) => Boolean) = live.filter(s => TurnPings.rootMetas.contains(s.lobby.meta)).flatMap { s =>
                    val all = standings(s)
                    for {
                        (_, (faction, mv)) <- all.find(_._1 == s.seat)
                        (_, (leader, top)) <- all.maxByOption(_._2._2)
                        if all.size > 1 && top > mv && ok(mv, top) && (key != "intro-last" || all.forall(_._2._2 >= mv))
                        l <- line(s, key)
                    } yield (s, key.stripPrefix("intro-"), l.replace("{faction}", "**" + faction + "**").replace("{vp}", mv.toString)
                        .replace("{leader}", "**" + leader + "**").replace("{top}", score(s.lobby.meta, top)).replace("{gap}", (top - mv).toString))
                }.headOption

                def finishLine(key : String, ok : Finish => Boolean) = ends.filter(ok).flatMap { f =>
                    line(f.seat, key).map(l => (f.seat, key.stripPrefix("intro-"), l.replace("{faction}", "**" + f.faction + "**").replace("{score}", score(f.seat.lobby.meta, f.score))
                        .replace("{winner}", "**" + f.winner + "**").replace("{top}", score(f.seat.lobby.meta, f.top))))
                }.headOption

                val pings = mine.map(s => s -> TurnPings.pings.getOrElse((s.game, s.user), Nil).size).filter(_._2 >= 4).sortBy(-_._2).headOption.flatMap { case (s, n) =>
                    line(s, "intro-pings").map(l => (s, "pings", l.replace("{n}", n.toString)))
                }

                val chosen : Option[(Seat, String, String)] = waiting
                    .orElse(vpLine("intro-last", (_, _) => true))
                    .orElse(finishLine("intro-finishedlast", _.lowest))
                    .orElse(pings)
                    .orElse(vpLine("intro-behind", (mv, top) => top - mv >= 8))
                    .orElse(finishLine("intro-lost", f => !f.won))

                chosen match {
                    case None => "<@" + id + ">: nothing found (" + live.size + " games going, " + ends.size + " finished, that the site can post in)"
                    case Some((s, why, text)) =>
                        val voice = v(s)
                        TurnPings.post(s.game, s.lobby, text, Nil, persona(voice) + (if (voice == crow) gifFor(why match { case "finishedlast" | "last" => "finishedlast" ; case "pings" | "waiting" => "reminder" ; case w => w }).getOrElse("") else ""))
                        Ranking.append(intros, Backup.row(id, s.game, why, Ranking.now()))
                        "<@" + id + "> (" + TurnPings.nameOf(s.lobby, s.user) + "): " + why + " in " + s.lobby.title + " (" + s.game + ")"
                }
            }
        }.mkString("\n  ", "\n  ", "")
    }

    def status : String = roleName match {
        case None => "no roast role (roast-role off)"
        case Some(n) =>
            val g = DiscordBot.guild.getOrElse(return "the role is @" + n + "; no Discord server known")
            cached = (0L, None)
            roleId match {
                case None => "the role is @" + n + ", but the server has no role of that name (make it with: role " + n + " | mentionable, then self-role " + n + ")"
                case Some(id) =>
                    val count = DiscordServer.call("GET", "/guilds/" + g + "/members?limit=1000").toOption.map(b =>
                        ("\"" + id + "\"").r.findAllMatchIn(b).size).map(_ + " members have it").getOrElse("members not counted")
                    "the role is @" + n + " (" + id + "): " + count + (if (SelfRoles.listed.contains(id)) ", and members may pick it" else "; members can't pick it yet (self-role " + n + ")")
            }
    }

    // ROOT: the Court Crow, a Corvid jester
    val crow = Voice("The Court Crow", "lfg/webhooks/heckler-root.png", Map(
        "turn" -> List(
            "Caw! Your turn, {name}. The whole Woodland is holding its breath. Mostly to stop laughing.",
            "{name}, it's your move. I've already plotted against it. Face down, of course.",
            "Your turn, {name}. Try not to give the Vagabond another item this time.",
            "Ah, {name}. The forest asked me to remind you that crafting is allowed. Encouraged, even.",
            "{name}'s turn! Quick, everyone, act surprised when they forget Evening again.",
            "It's your turn, {name}. The cats have built three sawmills while you were away. Just saying.",
            "{name}, your turn. I'd tell you the winning move, but where's the fun in that?",
            "Wake up, {name}! Even the Lizards' Lost Souls have moved since your last turn."),
        "defend" -> List(
            "Under attack, {name}! Do you have an ambush, or are you bluffing? I know. I always know.",
            "{name}, they're hitting you. Remember: you can't lose warriors you never placed. Think about it.",
            "Someone's battling {name}. I've got a seed on the attacker. Nothing personal.",
            "Defend yourself, {name}! Or don't. The crows will tidy up either way."),
        "battle" -> List(
            "{name}, your battle needs you. Roll high. Or roll at all, really.",
            "A battle! {name}, may the dice be kinder to you than your last three moves were.",
            "{name} is attacking. Bold. Ill-advised, but bold."),
        "banished" -> List(
            "{name}, the Bats are shooing you out. Mind the door on your way.",
            "Banished, {name}! Don't worry, the next clearing over is lovely this time of year."),
        "banish" -> List(
            "{name}, time to banish someone. Shoo them nicely. Or not."),
        "keep" -> List(
            "Pick your cards, {name}. Hint: the birds are the good ones. Probably.",
            "{name}, choose wisely. I'll be watching which ones you throw away."),
        "handlimit" -> List(
            "Too many cards, {name}! Hoarding is my thing. Discard some.",
            "{name}, your hand limit says no. Drop the ones you were never going to play anyway."),
        "draft" -> List(
            "Your pick, {name}. Take the Corvids. You won't. But you should.",
            "{name}, pick a faction. Any faction. The Woodland can't wait all day. Well, it can. It does."),
        "pickdeck" -> List("{name} picks the deck. Choose the one where you win, obviously."),
        "pickmap" -> List("{name} picks the map. Somewhere with lots of places to hide your mistakes, please."),
        "picklandmark" -> List("{name}, pick a landmark. Something to look at while you lose."),
        "partisans" -> List("{name}, the Partisans want in. They're like me: they only show up when it's messy."),
        "prisoners" -> List("{name}, prisoners to deal with. I hear the Knaves run a lovely little jail."),
        "entreat" -> List("{name}, you've been entreated. Politely! How unusual for this table."),
        "hospitals" -> List("{name}, Field Hospitals: is that cat worth a card? Be honest."),
        "pond" -> List("{name}, the deck or the Pond? Either way, the frogs are judging you."),
        "setup" -> List("Set up, {name}. Every great disaster starts with a single warrior."),
        "intro-waiting" -> List(
            "Caw! Welcome to the trolls, {name}. I've been watching {game}. It's been waiting on you for {time}. The whole Woodland has aged.",
            "{name}, you joined the trolls! Lovely. Meanwhile {game} has been waiting on you for {time}. I've started nesting in it."),
        "intro-last" -> List(
            "Welcome to the trolls, {name}! I see your {faction} sits on {vp} VP in {game}, dead last. {leader} has {top}. I'll be here all game.",
            "{name}, a troll at last! Let's look at {game}... {faction}, {vp} VP, last place. {leader} on {top}. Oh, this will be fun."),
        "intro-finishedlast" -> List(
            "Caw! I went through your old games, {name}. {game}: {faction}, {score}, dead last. {winner} won with {top}. The crows still talk about it.",
            "{name}, welcome to the trolls. I found {game} in the archives. Last place, {score}, as the {faction}. I had it framed."),
        "intro-pings" -> List(
            "{name}! I counted. {game} needed {n} turn pings to get you moving. {n}! The crows have shorter attention spans and even we noticed.",
            "Welcome to the trolls, {name}. Fun fact: {game} pinged you {n} times. I'm number {n} plus one."),
        "intro-behind" -> List(
            "{name}, your {faction} in {game}: {vp} VP. {leader}: {top}. That's {gap} VP of daylight. I can see it from my perch.",
            "Welcome to the trolls, {name}! {gap} VP behind {leader} in {game}. Don't worry, I'm sure they'll slow down. They won't."),
        "intro-lost" -> List(
            "Caw! Digging through your games, {name}. {game}: you lost as the {faction} with {score}. {winner} won with {top}. I remember everything.",
            "{name}, welcome. Remember {game}? You don't want to. The {faction}, {score}, and {winner} winning with {top}. I'll be bringing it up."),
        "reminder" -> List(
            "Still waiting, {name}. The crows have started a betting pool on when you'll move.",
            "{name}, a reminder: the game hasn't moved. I checked. Twice.",
            "Caw. Caw. CAW. That's crow for \"{name}, it's still your turn\".",
            "{name}, the Woodland has grown a new forest waiting for you. Several, actually.")))

    // NORTHGARD: Ratatoskr, carrying the eagle's insults down Yggdrasil
    val ratatoskr = Voice("Ratatoskr", "lfg/webhooks/heckler-nort.png", Map(
        "turn" -> List(
            "Up the tree and down again, {name}! The eagle says it's your turn, and that you'd better not waste it.",
            "{name}! The eagle at the top of Yggdrasil says it has seen better clans. It names several. Your turn.",
            "Your turn, {name}. Nidhogg says hello. Nidhogg also says your borders look delicious.",
            "Quick, {name}! Play a card. The eagle is timing you and frankly it's embarrassing for both of us.",
            "{name}, the eagle asks if you know that Recruit puts units on the map. Not in your pocket. On the map.",
            "It's your turn, {name}. I ran nine realms to tell you, and you'd better make it worth my legs.",
            "{name}, the gods are watching your turn. Well, one is. He's laughing."),
        "defend" -> List(
            "{name}, you're under attack! The eagle saw it coming. You, apparently, didn't.",
            "Someone's at your gate, {name}. The dragon at the roots says to bring snacks.",
            "{name}, a fight! The eagle says it's betting against you. The eagle is usually right."),
        "fight" -> List(
            "{name}, your fight! May your axes be sharper than your strategy.",
            "A fight, {name}! The eagle says roll well. Nidhogg says roll badly. I just carry the messages."),
        "trade" -> List(
            "Harvest trading, {name}! The eagle says no one will give you a fair deal. The eagle is a cynic.",
            "{name}, time to trade. Try not to swap your last food for wood again."),
        "winter" -> List(
            "Winter is here, {name}. The eagle wants to know if you remembered food this year.",
            "{name}, Winter! I've buried my nuts. Did you store anything?",
            "Brr, {name}. Winter. The dragon at the roots says frozen clans taste the same."),
        "kobold" -> List("{name}, the Kobolds want to trade. Watch your purse, they're worse than me."),
        "cards" -> List(
            "Pick a card, {name}. The eagle says pick the good one. Helpful bird.",
            "{name}, a card to pick! The eagle has opinions. They're all rude."),
        "draft" -> List("{name}, pick your clan. The eagle says pick the Squirrels. I agree, for no reason at all."),
        "setup" -> List("{name}, place your clan. Somewhere with food this time, the eagle suggests."),
        "raid" -> List("{name}, your raid! The longships are ready. Are you?"),
        "event" -> List("{name}, an event! Even the Norns had to look twice at your luck."),
        "intro-waiting" -> List(
            "Welcome to the trolls, {name}! The eagle says {game} has been waiting on you for {time}. The eagle has a lot to say about that.",
            "{name}! Ratatoskr here, your new heckler. {game} has waited {time} for you. I've run up Yggdrasil and back a dozen times since."),
        "intro-finishedlast" -> List(
            "Welcome to the trolls, {name}! The eagle remembers {game}: the {faction}, {score}, last of all. {winner} took it with {top}. The eagle never forgets."),
        "intro-pings" -> List(
            "{name}! The eagle counted: {game} needed {n} turn pings to wake you. I ran up and down Yggdrasil for every one of them."),
        "intro-lost" -> List(
            "Welcome to the trolls, {name}! The eagle sends its regards from {game}, where the {faction} lost with {score} and {winner} won with {top}."),
        "reminder" -> List(
            "{name}, I've run up and down the World Tree twice waiting for you. My legs are tired.",
            "Still your turn, {name}! The eagle has started composing a poem about you. It is not flattering.",
            "{name}! Nidhogg has finished gnawing a whole root while you were away. Your turn.",
            "A reminder from the top of Yggdrasil, {name}: the year won't harvest itself.")))

    // PINCER: the Pamphleteer, a satirist of the gentleman's war
    val pamphleteer = Voice("The Pamphleteer", "", Map(
        "turn" -> List(
            "EXTRA! EXTRA! {name} is to take a turn! Readers are advised to expect very little.",
            "This morning's broadsheet: \"{name}'s Grand Strategy: A Mystery Even To {name}\". Your turn.",
            "{name}, your move. The gentlemen of the coffee house have wagered against you. Heavily.",
            "Your turn, {name}. The press awaits your next blunder with the keenest interest.",
            "{name}, your colonies are in the papers again. Not the good pages."),
        "intro-waiting" -> List("EXTRA! {name} joins the trolls, while {game} has waited on them for {time}. Readers are advised to bring a book."),
        "intro-pings" -> List("EXTRA! {name} needed {n} turn pings in {game}! The editors are considering a weekly column."),
        "intro-lost" -> List("FROM OUR ARCHIVES: {name} lost {game} with {score}. {winner} won with {top}. A reader writes: \"Who could forget?\""),
        "reminder" -> List(
            "STILL WAITING: a second edition on {name}'s turn, which has not yet occurred.",
            "{name}, the ink has dried on three pamphlets about your absence. Your move.")))

    // COMPANY OF HEROES: the Sergeant Major, over the field radio
    val sergeant = Voice("Sergeant Major", "", Map(
        "turn" -> List(
            "On your feet, {name}! It's your turn, and that's an order!",
            "{name}! Your squads are standing around like it's a parade. Move them!",
            "Listen up, {name}: the objective is the Control Points, not the scenic route. Your turn.",
            "{name}, I've seen better tactics from a ration tin. Your turn. Prove me wrong.",
            "Your turn, {name}. And if I see one more unit stood in the open, I'll be writing to your mother."),
        "asked" -> List("{name}, you're needed on the line. Double time!"),
        "intro-waiting" -> List("Welcome to the trolls, {name}! {game} has been waiting on you for {time}. On your feet, soldier!"),
        "intro-pings" -> List("{name}! It took {n} radio calls to get you moving in {game}! {n}! Next time I'm sending a runner with a bucket of water."),
        "intro-lost" -> List("At ease, {name}. I read the after-action report on {game}: {score}, while {winner} took it with {top}. We'll be drilling that."),
        "reminder" -> List(
            "{name}! Are you still asleep in that foxhole? It's STILL your turn!",
            "This is the Sergeant Major, {name}. The war is waiting. On YOU.")))
}
