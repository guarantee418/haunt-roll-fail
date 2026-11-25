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
                    val avatar = if (v.avatar.nonEmpty && Tournament.site.nonEmpty) ",\"avatar_url\":" + Tournament.json(Tournament.site + "/art/" + v.avatar) else ""
                    post(line, ",\"username\":" + Tournament.json(v.name) + avatar)
                }
            }
            catch {
                case e : Exception => println("Heckler: " + e)
            }
        }
    }

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
