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
    // (faction: the seat's faction its reason names, "RF" for the Riverfolk Company: on the otters' own turns, the otters' lines and GIF)
    def ping(meta : String, tag : String, faction : String, uid : String, text : String, reminder : Boolean, post : (String, String) => Unit, game : String = "") {
        Tournament.poster.execute { () =>
            val troll = roleName.nonEmpty && DiscordBot.configured && (try roasted(uid) catch { case e : Exception => println("Heckler: " + e) ; false })
            if (troll) {
                val v = voiceOf(meta)
                val otters = v == crow && tag == "turn" && faction == "RF" && !reminder
                val own = if (v == crow && tag == "turn" && !reminder && faction != "RF" && scala.util.Random.nextBoolean()) factionTurns.get(faction) else None
                val options = if (reminder) v.lines("reminder") else if (otters) otterLines else own.getOrElse(v.lines.getOrElse(tag, v.lines("turn")))
                val line = pick(uid, options).replace("{name}", "<@" + uid + ">")
                post(text + "\n" + line, persona(v) + (if (v == crow) gifFor(if (reminder) "reminder" else if (otters) "otters" else tag, game).getOrElse("") else ""))
            }
            else
                post(text, "")
        }
    }

    // THE CROW'S GIFS (2026-11-25, asked for: "sometimes the crow just sends a trolling gif, with some context"): a Court Crow
    // post may carry a GIF under its line, only when the line gives it a reason (gifFor below). They are the crow's own (webp2/lfg/gifs/, by
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
    val harsh = Set("wiped", "rolled30", "raze", "revolt", "turmoil", "bomb", "sanctify", "last", "finishedlast")

    // TERRIFIED (2026-11-25, the owner's pick for being "scared from a fight"): the Klipy video the owner linked
    // (static.klipy.com/.../2W84h3Sw.mp4), made a GIF (webp2/lfg/gifs/terrified.gif): half of the GIFs when the player is under attack
    // or being banished
    val scared = Set("defend", "banished")

    // VEGETA IN THE RAIN (2026-11-25, the owner's pick: "Vegeta in the rain is like losing at 29"): the Klipy video the owner linked
    // (static.klipy.com/.../2n80SKFK.mp4), made a GIF (webp2/lfg/gifs/vegeta-rain.gif): always with a Root game lost on 29 VP (gameOver
    // below, and a first jab about one)
    // MILHOUSE FLIRTING (2026-11-25, the owner's pick for "otters trying to sell": static.klipy.com/.../bY4YC31kOYNViO9J0qs.mp4, made a GIF,
    // webp2/lfg/gifs/milhouse-flirt.gif): half of the GIFs on the Riverfolk Company's own turns

    def embed(url : String) = ",\"embeds\":[{\"color\":8798896,\"image\":{\"url\":" + Tournament.json(url) + "}}]"

    // A GIF only when it says something about the line, and sparingly (owner, 2026-11-25: "you don't need to send GIFs for no reason";
    // then "GIFs should be used sparingly. They're funny when used sparingly. Too frequently and they become annoying from cluttering
    // the thread"): a game lost on 29 VP always gets Vegeta; anything else that has a GIF of its own gets it one time in four, and never
    // within half an hour of the last GIF in that game's thread (lastGif). A 3-0 or 0-3 roll gets the crow's dice, a Lizard Cult
    // conversion the unmasking lizard (convert.gif), under attack or banished a terrified Rick Moranis, the otters' own turns Milhouse,
    // an ambush or the Faithful's lucky draw a laugh, the harshest things Emotional Damage or the crow in its can; a plain turn ping,
    // a reminder or anything else none
    val lastGif = new java.util.concurrent.ConcurrentHashMap[String, java.lang.Long]()
    val gifGap = 30 * 60 * 1000L

    def gifFor(tag : String, game : String = "") : Option[String] = {
        val art = Tournament.site + "/art/lfg/gifs/"
        def r = scala.util.Random
        def laugh = if (r.nextBoolean()) klipyLaughs(r.nextInt(klipyLaughs.size)) else art + "crow-laugh.gif"
        val now = System.currentTimeMillis
        val quiet = game.isEmpty || Option(lastGif.get(game)).forall(t => now - t > gifGap)
        val gif = tag match {
            case _ if Tournament.site.isEmpty => None
            case "lost29" => Some(art + "vegeta-rain.gif")
            case _ if !quiet || r.nextInt(4) != 0 => None
            case "rolled30" | "whiffed" => Some(art + "crow-dice.gif")
            case "convert" => Some(art + "convert.gif")
            case t if scared.contains(t) => Some(art + "terrified.gif")
            case "otters" => Some(art + "milhouse-flirt.gif")
            case "faithful" | "ambushed" => Some(laugh)
            case t if harsh.contains(t) => Some(art + (if (r.nextBoolean()) "emotional-damage.gif" else "crow-can.gif"))
            case _ => None
        }
        if (gif.nonEmpty && game.nonEmpty)
            lastGif.put(game, now)
        gif.map(embed)
    }

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
                                    TurnPings.post(game, lobby, line, Nil, persona(crow) + gifFor(tag, game).getOrElse(""), now = true)
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
        "wiped" -> List(
            "{name} has left {where}. Not by choice.",
            "{other} cleared {name} out of {where}. Every last piece. Tidy work.",
            "{where} is now a {name}-free zone. {other} handled the paperwork."),
        "zerovp" -> List(
            "Round 3, {name}, and still on zero. Bold minimalist strategy.",
            "{name} is on 0 VP in round 3. Somewhere, a score track is weeping.",
            "Zero VP in round 3, {name}. Saving them all for the end? Interesting plan."),
        "closing" -> List(
            "{other} is about to win, {name}. You're about to watch.",
            "{other} just passed 25 VP. {name}, the view from last place must be lovely.",
            "{other} is closing in on 30, {name}, and you're closing in on the bottom of the score track."),
        "hostile" -> List(
            "The Vagabond is Hostile to {name} now. Even the guy with no friends has standards.",
            "{name}, the Vagabond has turned on you. Smart Vagabond.",
            "{name} made the Vagabond Hostile. Now there's someone in the woods who hates you on purpose."),
        "prisoner" -> List(
            "{other} took {name}'s warrior prisoner at {where}. At least someone wanted it.",
            "{name}, your warrior from {where} is now a Knaves house guest. Indefinitely."),
        "dominance" -> List(
            "A dominance card, {name}? Bold, desperate, or both?",
            "{name} activated a dominance. Your points are gone. Your hope remains. For now.",
            "{name} has given up on points and gone for a dominance. The crows respect the audacity. Not the odds."),
        "faithful" -> List(
            "Faith: rewarded. {name}: not.",
            "HA! {other}'s Faithful hit {name} in {where} AND drew the matching card. The deck is in on it, {name}.",
            "Caw! The Faithful hit {name} at {where}, flipped the top card, and of course it matched. Of course it did.",
            "{name}, {other} took an extra swing at you in {where} and the deck said \"keep it\". Even the cards are against you.",
            "The top card of the deck had one job, {name}: not to match {where}. It matched. {other} keeps The Faithful."),
        "rolled30" -> List(
            "3-0, {name}. The dice didn't just pick a side at {where}, they built a house there.",
            "{name}'s warriors at {where} died the way they lived: rolling zeros.",
            "A perfect 3-0 at {where}. {other} thanks {name} for the demonstration.",
            "HA! {other} rolled 3-0 against {name} in {where}. The dice have chosen a side, and it isn't yours.",
            "Caw caw caw! A 3-0 roll at {where}. {name}, your warriors didn't even get to swing.",
            "{other} rolled a 3, {name} rolled a 0. I'd call it a battle, but that would be generous.",
            "3-0 at {where}! {name}, have you tried rolling the die the other way up?"),
        "whiffed" -> List(
            "Zero hits at {where}, {name}. Have you considered diplomacy?",
            "{name}'s warriors charged into {where} and tripped. All of them.",
            "{name} attacked {other} at {where} and rolled a 0. Bold strategy. Terrible dice.",
            "Caw! {name} charged into {where} and rolled 0-3. {other} sends their thanks.",
            "{name}'s attack at {where}: zero hits dealt, three taken. I've seen sparrows do better."),
        "ambushed" -> List(
            "{other} sat in that bush at {where} for three turns, {name}. Did you not hear the giggling?",
            "{name}'s scouting report for {where}: \"probably fine.\"",
            "AMBUSH! {name} walked right into {other}'s trap at {where}. The crows saw it coming. Everyone saw it coming.",
            "{name} marched into {where} without checking the bushes. {other} was in the bushes.",
            "Ambushed at {where}, {name}! Rule one of the Woodland: never trust a quiet clearing."),
        "raze" -> List(
            "{where} has been razed. {name}'s insurance does not cover rats.",
            "Think of it as free demolition, {name}. {where} needed a fresh start anyway.",
            "The Mob just razed {name}'s buildings in {where}. Don't worry, rubble has a rustic charm.",
            "{other}'s rats burned down {where}. {name}, I hope you weren't attached to anything there.",
            "Raze! {name}'s things in {where} are now a nice open field. The crows thank you for the view.",
            "{name}, the Mob came to {where}. The Mob left. Your buildings did not."),
        "revolt" -> List(
            "{where} revolted, {name}. Turns out the locals had opinions about you.",
            "Revolt at {where}! {name}'s pieces there are now a historical footnote.",
            "REVOLT in {where}! {name} has been wiped out of it by {other}. Viva la Woodland!",
            "The {other} rose up in {where} and {name} went down. Should have listened to the people, {name}.",
            "{name}, {where} has revolted. Everything you had there is now a very sympathetic base.",
            "Caw! A Revolt in {where} cleared out {name}. Sympathy: one. {name}: zero."),
        "convert" -> List(
            "Another of {name}'s warriors traded them in for a robe and a garden at {where}. Can't blame it.",
            "Honestly, joining the Cult at {where} is the best career move that warrior ever made, {name}.",
            "{other} converted one of {name}'s warriors in {where}. It looks happier already.",
            "{name}, your warrior in {where} has found religion. Lizard religion.",
            "Another of {name}'s warriors joins the Cult in {where}. They say the robes are very comfortable."),
        "sanctify" -> List(
            "{name}'s building in {where} is a shrine now. Finally, something there worth visiting.",
            "{other} sanctified {name}'s building in {where}. It's a garden now. A lovely garden. Not yours.",
            "{name}, your building in {where} has been redecorated by {other}. Very green. Very lizard."),
        "turmoil" -> List(
            "The Decree was a house of cards, {name}, and you built it in a wind tunnel.",
            "TURMOIL for {name}! The new Eyrie leader is said to be \"slightly less embarrassing.\"",
            "TURMOIL! {name}'s Eyrie tripped over its own Decree. The crows are crying with laughter.",
            "{name}, the Decree collapsed. Your leader has been deposed. Your nobles are embarrassed. I'm delighted.",
            "Caw! The Eyrie's Decree broke and {name} fell into Turmoil. Birds of a feather fall together."),
        "bomb" -> List(
            "{name} stepped on a plot in {where}. It went boom. Shocking, I know.",
            "Kaboom in {where}! The Corvids send their regards, {name}, and a cleaning bill.",
            "BOOM! A Corvid bomb went off on {name}'s warriors in {where}. That was one of ours. You're welcome.",
            "{name}, that plot in {where}? It was a bomb. It's always a bomb."))

    // FACTION TURN LINES (2026-11-25, "add all of them"): on a faction's own turn, half the time the crow uses its lines (the Riverfolk's
    // are the otters', always); the reason's faction is the seat's
    val factionTurns : Map[String, List[String]] = Map(
        "MC" -> List(
            "Another sawmill, {name}? Bold, building in a forest full of people who hate you.",
            "The Keep is lovely, {name}. Pity it's the only thing you're keeping."),
        "ED" -> List(
            "Read the Decree carefully, {name}. Your ancestors are watching. They're disappointed.",
            "The Decree grows, {name}. So does the list of ways it breaks."),
        "WA" -> List(
            "Power to the people, {name}! All three of them.",
            "{name}, the revolution will be televised. The ratings are terrible."),
        "VB" -> List(
            "Whose side are you on today, {name}? Asking for everyone you've betrayed.",
            "Roaming the woods with a bag of other people's stuff, {name}. Living the dream."),
        "LC" -> List(
            "Preach, {name}. The congregation is three outcasts and a garden.",
            "The Lost Souls grow, {name}. So does my concern for your plan."),
        "UD" -> List(
            "The Duchy digs, {name}. Down. Like your score.",
            "Your Ministers have sat idle so long they've unionized, {name}."),
        "CC" -> List(
            "Ah, {name}, a fellow corvid. I'll be watching your plots. Professionally.",
            "Place a plot, {name}. Make it a bomb. It's always a bomb."),
        "LH" -> List(
            "The Warlord demands loot, {name}. Your opponents demand you stop.",
            "Mob time, {name}. Burn something useful to you, for once."),
        "KI" -> List(
            "Delve, {name}! Find something shinier than your current position.",
            "The Keepers march on, {name}, carrying relics nobody asked for."),
        "TC" -> List(
            "The Council convenes again, {name}. Democracy is so tiring.",
            "Your Loyalists are hanging there like bats, {name}. Because they are."),
        "KD" -> List(
            "Steal something, {name}. It's what you're good at. It's the only thing you're good at.",
            "The Knaves are up, {name}. Lock up the valuables. And the warriors."),
        "LDvE" -> List(
            "The frogs are restless, {name}. Provoke someone. You know you want to.",
            "Ribbit, {name}. That's frog for \"please make a good move for once.\""))

    // the otters' own turns (the Riverfolk Company, "otters trying to sell")
    val otterLines = List(
        "{name}, the otters are open for business. Step right up, nobody's buying.",
        "Your turn, {name}. Quick, put the prices up again, I'm sure that'll work this time.",
        "{name}'s otters are flirting with every faction on the board. The board has left them on read.",
        "Trade posts, river boats, a winning smile. {name}, have you tried just being good at Root?")

    // LOSING AT 29 (2026-11-25, asked for: "Vegeta in the rain is like losing at 29"): when a Root game's result is reported (the
    // /report-result its first client to reach the end sends; Ranking.report took it), each troll member who lost on exactly 29 VP gets the
    // Court Crow in the game's thread, with Vegeta in the rain, once
    val lost29 = List(
        "{name} lost {game} on **29 VP** as the {faction}. One point. ONE. {winner} took it with {top}.",
        "29 VP, {name}. Twenty. Nine. The crows will be singing about this one for years. {winner} won with {top}.",
        "So close, {name}. {game}: the {faction} on 29, {winner} on {top}. Somewhere, it's raining.")

    val lostByOne = List(
        "{name} lost {game} by ONE point as the {faction}. {winner} took it with {top}. So close you can taste it. It tastes like defeat.",
        "One VP, {name}. One more craft, one more battle, one less mistake. {winner} won {game} with {top}.",
        "{name} finished {game} on {score}, {winner} on {top}. Second place is just the first loser, but closer.")

    val wonByOne = List(
        "{name} won {game} as the {faction} with {top}... by one point. I'll allow it. Barely.",
        "Congratulations, {name}. A one-point win in {game}. Even your victories are nail-biters.",
        "{name} took {game} by a single VP. The crows had money on the other side. We're not happy.")

    def gameOver(game : String, meta : String, sides : String) : Unit = if (roleName.nonEmpty && DiscordBot.configured && TurnPings.rootMetas.contains(meta)) try {
        val all = sides.split(';').toList.flatMap(t => t.split(":", 2).toList match {
            case w :: rest :: Nil if w == "W" || w == "L" => rest.split(',').toList.flatMap(x => x.split('/').toList match {
                case u :: f :: _ :: _ :: sc :: _ => Some((w == "W", u, f.replace('_', ' '), sc))
                case _ => None
            })
            case _ => Nil
        })
        val win = all.filter(_._1).headOption
        TurnPings.lobbyOf(game).foreach { lobby =>
            val table = TurnPings.table(game).nonEmpty
            // CLOSE FINISHES (2026-11-25, "add all of them"): a loss by one VP, and a win by one VP, the crow grudging; 29 VP is Vegeta's
            val top = win.flatMap(_._4.toIntOption)
            val second = all.filter(!_._1).flatMap(_._4.toIntOption).maxOption
            all.flatMap { case x @ (w, u, f, sc) =>
                if (!w && sc == "29") Some((x, lost29, "lost29"))
                else if (!w && top.nonEmpty && sc.toIntOption == top.map(_ - 1)) Some((x, lostByOne, ""))
                else if (w && second.nonEmpty && sc.toIntOption == second.map(_ + 1)) Some((x, wonByOne, ""))
                else None
            }.foreach { case ((_, u, f, sc), lines, gif) =>
                TurnPings.mentionOf(u, table).foreach { id =>
                    Tournament.poster.execute { () =>
                        try {
                            if (roasted(id)) {
                                val line = pick(id, lines).replace("{name}", "<@" + id + ">").replace("{game}", "**" + lobby.title + "**").replace("{faction}", "**" + f + "**")
                                    .replace("{winner}", win.map(w => "**" + w._3 + "**").getOrElse("the winner")).replace("{top}", win.map(w => if (w._4 == "D") "a dominance" else w._4 + " VP").getOrElse("more")).replace("{score}", sc + " VP")
                                TurnPings.post(game, lobby, line, Nil, persona(crow) + (if (gif.nonEmpty) gifFor(gif).getOrElse("") else ""), now = true)
                            }
                        }
                        catch { case e : Exception => println("Heckler: " + e) }
                    }
                }
            }
        }
    }
    catch { case e : Exception => println("Heckler: game over not taken: " + e) }

    // SLOW TURNS (2026-11-25, "add all of them"): a troll member whose wait in a Root game lasted a day or more gets the crow in the
    // game's thread once they've moved (TurnPings.waiting, when the wait ends)
    val slowLines = List(
        "That turn took {hours} hours, {name}. Was it worth it? (It wasn't.)",
        "{name} finally moved in {game}, after {hours} hours. The crows had started a memorial.",
        "{hours} hours, {name}. Civilizations have risen and fallen faster. The Eyrie certainly has.",
        "Welcome back, {name}! {hours} hours for one turn in {game}. We kept your seat warm. Barely.")

    def slow(game : String, user : String, hours : Long) : Unit = if (roleName.nonEmpty && DiscordBot.configured) try {
        TurnPings.lobbyOf(game).filter(l => TurnPings.rootMetas.contains(l.meta)).foreach { lobby =>
            TurnPings.mentionOf(user, TurnPings.table(game).nonEmpty).foreach { id =>
                Tournament.poster.execute { () =>
                    try {
                        if (roasted(id)) {
                            val line = pick(id, slowLines).replace("{name}", "<@" + id + ">").replace("{game}", "**" + lobby.title + "**").replace("{hours}", hours.toString)
                            TurnPings.post(game, lobby, line, Nil, persona(crow), now = true)
                        }
                    }
                    catch { case e : Exception => println("Heckler: " + e) }
                }
            }
        }
    }
    catch { case e : Exception => println("Heckler: slow turn not taken: " + e) }

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
                        TurnPings.post(s.game, s.lobby, text, Nil, persona(voice) + (if (voice == crow) gifFor(if (text.contains("29 VP")) "lost29" else why match { case "finishedlast" | "last" => "finishedlast" ; case w => w }, s.game).getOrElse("") else ""))
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
            "{name}, it's your turn. Take your time. The game's only been going since the Marquise was a kitten.",
            "Oh good, it's {name}'s turn. Grab a snack, everyone.",
            "{name}, I've prepared a list of your best moves. It's blank. Good luck.",
            "Your move, {name}. I've seen trees make faster decisions, and they're rooted.",
            "{name}, statistically you're due for a good turn. Statistically.",
            "Your turn, {name}. Remember your last one? Neither does anyone else. It was that forgettable.",
            "{name}, it's your turn. Please don't do what you did last time.",
            "Your strategy is like the forest at night, {name}: nobody can see it, including you.",
            "{name}, I've watched worms with better long-term plans.",
            "The other factions met and voted, {name}. They'd like you to keep playing exactly like this.",
            "Your turn, {name}. If you're going to lose, at least lose quickly.",
            "{name}, every move you make is a gift to someone. Usually not you.",
            "Caw! Your turn, {name}. The whole Woodland is holding its breath. Mostly to stop laughing.",
            "{name}, it's your move. I've already plotted against it. Face down, of course.",
            "Your turn, {name}. Try not to give the Vagabond another item this time.",
            "Ah, {name}. The forest asked me to remind you that crafting is allowed. Encouraged, even.",
            "{name}'s turn! Quick, everyone, act surprised when they forget Evening again.",
            "It's your turn, {name}. The cats have built three sawmills while you were away. Just saying.",
            "{name}, your turn. I'd tell you the winning move, but where's the fun in that?",
            "Wake up, {name}! Even the Lizards' Lost Souls have moved since your last turn."),
        "defend" -> List(
            "{name}, you're under attack. On the bright side, you were losing anyway.",
            "Have you considered surrendering in advance, {name}? It saves time.",
            "Your warriors asked me to tell you they quit, {name}.",
            "Under attack, {name}! Do you have an ambush, or are you bluffing? I know. I always know.",
            "{name}, they're hitting you. Remember: you can't lose warriors you never placed. Think about it.",
            "Someone's battling {name}. I've got a seed on the attacker. Nothing personal.",
            "Defend yourself, {name}! Or don't. The crows will tidy up either way."),
        "battle" -> List(
            "Attacking again, {name}? The defenders are starting to feel sorry for you.",
            "Roll, {name}. The dice have been looking forward to this.",
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
            "It's been 12 hours, {name}. The Eyrie has had three leaders since your last move.",
            "Moss has started growing on your warriors, {name}.",
            "Reminder, {name}: Root is a game where you take turns. This is one of them.",
            "I've seen glaciers with more momentum, {name}.",
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
