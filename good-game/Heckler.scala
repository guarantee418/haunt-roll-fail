package hrf.gg

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.Files

// THE ROAST ME ROLE (2026-11-25, asked for: "a notification role that makes fun of you while you play the game"): a member with the
// role (default "Roast Me"; the owner makes it with the role change and lists it with self-role, so members pick it with /roles) gets,
// under each of their turn pings, a jab from the game's heckler, posted through the same webhook in the same thread under the heckler's
// own name (and picture, where it has one): a webhook can post as any name, per message. The jab names the player without pinging
// them again (allowed_mentions lists no one), and it fits why they were pinged (Turns.reason's tag, as TurnPings.reasonOf reads it).
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

    // a jab for a ping (called from TurnPings.attempt, after the ping is queued): the role is checked on the poster's thread, so the
    // ping isn't held up by it, and the jab follows the ping
    def heckle(meta : String, tag : String, uid : String, name : String, reminder : Boolean, post : (String, String) => Unit) {
        if (roleName.isEmpty || !DiscordBot.configured)
            return
        Tournament.poster.execute { () =>
            try {
                if (roasted(uid)) {
                    val v = voiceOf(meta)
                    val options = if (reminder) v.lines("reminder") else v.lines.getOrElse(tag, v.lines("turn"))
                    val line = pick(uid, options).replace("{name}", name)
                    post(line, persona(v) + (if (v == crow) gifFor(tag).getOrElse("") else ""))
                }
            }
            catch {
                case e : Exception => println("Heckler: " + e)
            }
        }
    }

    // THE CROW'S GIFS (2026-11-25, asked for: "sometimes the crow just sends a trolling gif, with some context"): about one Court
    // Crow post in three carries one of its animated GIFs (webp2/lfg/gifs/, by haunt-roll-fail/tools/heckler-art.py, served from
    // /art/) under its line, which gives the context: the dice for a 3-0 roll, the crow popping out of its trash can for something
    // trashed, else the crow rocking with laughter
    def gifFor(tag : String) : Option[String] =
        if (Tournament.site.isEmpty || scala.util.Random.nextInt(3) != 0) None
        else Some(tag match {
            case "rolled30" | "whiffed" | "defend" | "battle" => "crow-dice.gif"
            case "raze" | "revolt" | "sanctify" | "bomb" | "turmoil" | "handlimit" | "banished" => "crow-can.gif"
            case _ => "crow-laugh.gif"
        }).map(g => ",\"embeds\":[{\"color\":8798896,\"image\":{\"url\":" + Tournament.json(Tournament.site + "/art/lfg/gifs/" + g) + "}}]")

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
    val words = Set("roast-role", "roasts")

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
        "reminder" -> List(
            "{name}! Are you still asleep in that foxhole? It's STILL your turn!",
            "This is the Sergeant Major, {name}. The war is waiting. On YOU.")))
}
