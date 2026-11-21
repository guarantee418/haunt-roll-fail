package hrf.gg

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path, Paths}

// DISCORD SETUP PANEL (asked for on 2026-11-20): /root opens a panel only its host sees, with every option New Online Game has,
// as Discord menus, the site's defaults picked; Open the game posts the game in the channel for players to join (Lfg.scala), and
// the game is created with these options as soon as its last seat is taken.
//
// The options come from the client (MetaGame.lfgCatalog, sent by Play Online when the server's copy differs: POST /lfg/catalog/
// <meta>/<hash>, kept in turns/lfg-catalog.tsv), so the server never lists them itself: each option's page and section on the setup
// screen, its name, the player counts it's offered for, and how options turn each other on and off, which State works out as the
// setup screen does (hrf.options.OptionsState, ported). A page of the setup screen is one or more pages of the panel: a Discord
// message holds five rows, four menus and a row of buttons, and a menu lists 25 options at most. A section whose options exclude
// each other is a menu of one pick; one of toggles, a menu of several.
//
// The panels are kept in memory only (a restart loses one being set up; /root starts again). The Discord server's own emojis are
// shown beside the options they stand for (emojiNames; factions by their short name), looked up with the bot's token.
//
// Each game (Lfg.Kind: Root's Advanced Setup, Northgard's Adset since 2026-11-21) has its own catalog (Root's in lfg-catalog.tsv,
// the others in lfg-catalog-<meta>.tsv), the sections its command sets (sections) and the ones its panel lists (pages).
object LfgSetup {
    import Backup.{rows, row}

    def file(meta : String) = TurnPings.dir.resolve(if (meta == Lfg.root.meta) "lfg-catalog.tsv" else "lfg-catalog-" + meta + ".tsv")

    case class Opt(name : String, page : Int, ns : Set[Int], toggle : Boolean, cond : Boolean, important : Boolean, group : String, label : String)

    case class Catalog(meta : String, hash : String, version : String, players : List[Int], need : Option[(String, Int)],
        mandatory : Map[Int, List[String]], defaults : Map[Int, List[String]], presets : Map[Int, List[(String, List[String])]],
        options : List[Opt], required : Map[String, List[List[String]]], blocked : Map[String, List[List[String]]],
        on : Map[String, List[String]], also : Map[String, List[String]], off : Map[String, List[String]], clear : Map[String, List[String]]) {
        val byName = options.map(o => o.name -> o).toMap
        def all(n : Int) = options.filter(_.ns.contains(n)).map(_.name)
        def label(o : String) = byName.get(o).map(_.label).getOrElse(o)
    }

    val cached = new java.util.concurrent.ConcurrentHashMap[String, (Long, Option[Catalog])]()

    def catalog(meta : String) : Option[Catalog] = {
        val f = file(meta)
        if (!Files.exists(f))
            return None
        val t = Files.getLastModifiedTime(f).toMillis
        if (!Option(cached.get(meta)).exists(_._1 == t))
            cached.put(meta, (t, parse(new String(Files.readAllBytes(f), UTF_8)).filter(_.meta == meta)))
        cached.get(meta)._2
    }

    def words(s : String) = s.split(' ').toList.filter(_.nonEmpty)

    def parse(text : String) : Option[Catalog] = scala.util.Try {
        val lines = text.split('\n').toList.map(_.split("\t", -1).toList)
        val (meta, hash) = lines.head match { case m :: h :: _ => (m, h) ; case _ => throw new Exception("no header") }
        val version = lines.lift(1).flatMap(_.headOption).getOrElse("")
        val rest = lines.drop(2)
        def alts(k : String, a : String) = if (k == "0") Nil else a.split('|').toList.map(words)
        def per(kind : String) = rest.collect { case `kind` :: n :: v :: _ => n.toInt -> words(v) }.toMap
        def rel(kind : String) = rest.collect { case `kind` :: o :: v :: _ => o -> words(v) }.toMap
        def rel2(kind : String) = rest.collect { case `kind` :: o :: k :: v :: _ => o -> alts(k, v) }.toMap
        Catalog(meta, hash, version,
            rest.collectFirst { case "players" :: v :: _ => words(v).flatMap(_.toIntOption) }.getOrElse(Nil),
            rest.collectFirst { case "need" :: p :: k :: _ => (p, k.toIntOption.getOrElse(0)) },
            per("mandatory"), per("defaults"),
            rest.collect { case "preset" :: n :: t :: v :: _ => n.toInt -> (t, words(v)) }.groupBy(_._1).view.mapValues(_.map(_._2)).toMap,
            rest.collect { case "option" :: n :: p :: ns :: t :: c :: i :: g :: l :: _ =>
                Opt(n, p.toInt, ns.split(',').flatMap(_.toIntOption).toSet, t == "1", c == "1", i == "1", g, l) },
            rel2("required"), rel2("blocked"), rel("on"), rel("also"), rel("off"), rel("clear"))
    }.toOption

    // the hash of the copy kept: "have" when the client's is the same, else "need"
    def check(meta : String, hash : String) = if (catalog(meta).exists(c => c.meta == meta && c.hash == hash)) "have" else "need"

    // the client's catalog: "<version>\n<lines>"
    def setCatalog(meta : String, hash : String, body : String) : String = synchronized {
        val ok = Lfg.kinds.exists(_.meta == meta) && hash.nonEmpty && hash.length <= 20 && hash.forall(_.isDigit) && body.length < 4000000
        val parsed = if (ok) parse(meta + "\t" + hash + "\n" + body) else None
        parsed.filter(c => c.players.nonEmpty && c.options.nonEmpty) match {
            case Some(_) =>
                Files.createDirectories(TurnPings.dir)
                Files.write(file(meta), (meta + "\t" + hash + "\n" + body).getBytes(UTF_8))
                // the command's choices come from it: registered again
                if (Lfg.publicKey.nonEmpty)
                    new Thread(() => println("LFG: " + Lfg.commands + " " + Lfg.register())).start()
                "ok"
            case None => "no"
        }
    }

    // THE OPTIONS' STATE: hrf.options.OptionsState, with the options by name
    case class State(c : Catalog, all : List[String], mandatory : List[String], selected : List[String], dimmed : List[String]) {
        val actual = (mandatory ++ selected).distinct

        def required(o : String) = c.required.getOrElse(o, List(Nil))
        def blocked(o : String) = c.blocked.getOrElse(o, Nil)
        def forcedOn(o : String) = c.on.getOrElse(o, Nil)
        def alsoOn(o : String) = c.also.getOrElse(o, Nil)
        def forcedOff(o : String) = c.off.getOrElse(o, Nil)
        def clear(o : String) = c.clear.getOrElse(o, Nil)
        def toggle(o : String) = c.byName.get(o).exists(_.toggle)

        def filtered(options : List[String]) : List[String] = {
            var remaining = all.intersect(options)
            def resulting = mandatory ++ remaining
            def drop(f : String => Boolean) {
                var found = true
                while (found) {
                    val x = remaining.filter(f)
                    found = x.nonEmpty
                    remaining = remaining.diff(x)
                }
            }
            drop(o => required(o).forall(_.diff(resulting).nonEmpty))
            drop(o => blocked(o).exists(_.diff(resulting).isEmpty))
            drop(o => forcedOn(o).diff(resulting).nonEmpty)
            drop(o => forcedOff(o).intersect(resulting).nonEmpty)
            if (remaining != options) filtered(remaining) else options
        }

        def enabled(o : String) : Boolean =
            if (mandatory.contains(o)) false
            else if (selected.contains(o)) toggle(o)
            else required(o).exists(_.diff(actual).isEmpty) && blocked(o).forall(_.diff(actual).nonEmpty)

        def click(o : String) : State =
            if (selected.contains(o)) {
                if (!toggle(o)) this else copy(selected = selected.filter(_ != o)).checkDimmed()
            }
            else {
                val on = all.intersect(forcedOn(o) ++ alsoOn(o))
                val off = forcedOff(o)
                val cl = clear(o)
                copy(selected = (o :: on ++ selected.diff(off).diff(cl)).distinct, dimmed = (selected.intersect(off) ++ dimmed.filter(_ != o).diff(on)).diff(cl).distinct).checkDimmed()
            }

        def checkDimmed() : State = {
            var remaining = filtered(selected)
            var candidates = selected.diff(remaining) ++ dimmed
            var found = true
            while (found) {
                found = false
                candidates.foreach { x =>
                    if (!found && filtered(remaining :+ x).size > remaining.size) {
                        remaining :+= x
                        candidates = candidates.filter(_ != x)
                        found = true
                    }
                }
            }
            val retired = selected.diff(remaining)
            State.make(c, all, mandatory, remaining, retired ++ dimmed.diff(remaining))
        }

        // listed on the setup screen: on a page, offered for these players, and, for one listed only once what it needs is picked, that
        def shown(o : Opt) = o.page >= 0 && all.contains(o.name) && (!o.cond || required(o.name).exists(_.forall(r => actual.contains(r))))
    }

    object State {
        def make(c : Catalog, all : List[String], mandatory : List[String], selected : List[String], dimmed : List[String]) =
            new State(c, all, all.intersect(mandatory), all.intersect(selected), dimmed.intersect(all))

        def of(c : Catalog, n : Int, picked : List[String]) = make(c, c.all(n), c.mandatory.getOrElse(n, Nil), picked, Nil).checkDimmed()

        def defaults(c : Catalog, n : Int) = of(c, n, c.defaults.getOrElse(n, Nil))
    }

    // THE DISCORD SERVER'S EMOJIS: name -> id, looked up with the bot's token, for 10 minutes
    @volatile var emojis : (Long, Map[String, (String, Boolean)]) = (0L, Map())

    def emojiMap() : Map[String, (String, Boolean)] = {
        if (System.currentTimeMillis - emojis._1 > 10 * 60 * 1000) {
            emojis = (System.currentTimeMillis, emojis._2)
            try {
                (DiscordBot.token, DiscordBot.guild) match {
                    case (Some(t), Some(g)) =>
                        DiscordBot.get("https://discord.com/api/v10/guilds/" + g + "/emojis", Some(t)).foreach { b =>
                            Json.parse(b) match {
                                case l : List[_] =>
                                    val m = l.map(Lfg.obj).map(e => Lfg.str(e.get("name")).toLowerCase -> (Lfg.str(e.get("id")), e.get("animated").contains(true))).filter(_._2._1.nonEmpty).toMap
                                    emojis = (System.currentTimeMillis, m)
                                case _ =>
                            }
                        }
                    case _ =>
                }
            }
            catch {
                case e : Exception => println("LFG: emojis not read: " + e)
            }
        }
        emojis._2
    }

    // the server's emoji names for options (asked for on 2026-11-20: rm the random map, rc random clearings, sd Squires and Disciples,
    // ep Exiles and Partisans, base the base deck); a faction in the draft by its short name (ld the Lilypad Diaspora)
    val emojiNames = Map(
        "AutumnMap" -> "autumn", "WinterMap" -> "winter", "LakeMap" -> "lake", "MarshMap" -> "marsh", "MountainMap" -> "mount", "GorgeMap" -> "gorge",
        "WastelandsMap" -> "wastelands", "RandomOfficialMap" -> "rm", "RandomMap" -> "rm", "AllRandomClearings" -> "rc",
        "SquiresDeck" -> "sd", "StandardDeck" -> "base", "ExilesDeck" -> "ep", "HarrierNoCoins" -> "harrier", "IncludeFaction(LDvE)" -> "ld",
        "players 3" -> "3p", "players 4" -> "4p", "players 5" -> "5p", "title" -> "adset",
        // Northgard's posts (2026-11-21): its emoji, if the server has one of these names
        "title-nort" -> "northgard", "LandsWilderness" -> "wilderness", "LandsWastelands" -> "wastelands")

    def emojiOf(key : String) : Option[(String, String, Boolean)] = {
        def server(x : String) = emojiMap().get(x).map { case (id, a) => (x, id, a) }
        // the server's emoji of that name (a faction by its short name), else one whose name matches the option's (`serverMatch`), else
        // the bot's own icon (Lfg.iconOf, made from the game's art by tools/lfg-emoji.py), else for a deck with none of these the base
        // deck's (asked for on 2026-11-20: only ep and sd have their own)
        emojiNames.get(key).orElse(Some(key).filter(_.startsWith("IncludeFaction(")).map(_.stripPrefix("IncludeFaction(").stripSuffix(")").toLowerCase))
            .flatMap(server).orElse(serverMatch(key)).orElse(Lfg.iconEmoji(key))
            .orElse(Some(key).filter(k => k.endsWith("Deck") && !k.startsWith("#") && !unicode.contains(k)).flatMap(_ => server("base")))
    }

    // THE SERVER'S EMOJIS BY NAME (2026-11-21: the server has emojis the posts didn't use): an option (or "#<section>") takes the
    // server's emoji whose name, its letters and digits in lower case, maybe with "ng" or "root" before it, is the option's label, its
    // saved name (without the Option(...) around it, or Map, Deck, Landmark, Central, Option or Module after it), or a word of its label
    // of four letters or more, in that order (the clans' names are left to Lfg.serverEmoji)
    def norm(s : String) = s.toLowerCase.filter(c => c.isLetterOrDigit && c < 128)

    def optOf(key : String) : Option[Opt] = Lfg.kinds.map(_.meta).distinct.flatMap(m => catalog(m).flatMap(_.byName.get(key))).headOption

    val ignoredWords = Set("only", "with", "from", "each", "that", "this", "your", "their", "cards", "card", "tiles", "tile", "rule", "rules",
        "mode", "game", "full", "none", "default", "seat", "chooses", "allowed", "player", "players", "first", "second", "after", "year",
        "years", "base", "module", "option", "expansion", "version", "played", "both", "parts", "more", "extra", "central", "center", "include",
        "landmark", "landmarks", "deck", "map", "available")

    def nameWords(s : String) = s.replaceAll("([a-z])([A-Z])", "$1 $2").split("[^A-Za-z0-9']+").toList.map(norm).filter(w => w.length >= 4 && !ignoredWords.contains(w))

    // the names to look for: the label, the saved name (with and without what is around it), the saved name's words, the section's
    // words, the label's words
    def candidates(key : String) : List[String] = {
        val inner = key.replaceAll("^[A-Za-z]+\\((.*)\\)$", "$1").replaceAll("^[A-Za-z]+\\(\"?(.*?)\"?\\)$", "$1")
        val bare = inner.replaceAll("(Map|Deck|Landmark|Landmarks|Central|Option|Module)$", "")
        val o = optOf(key)
        val label = if (key.startsWith("#")) key.drop(1) else o.map(_.label).getOrElse("")
        (List(norm(label), norm(key.stripPrefix("#")), norm(inner), norm(bare)) ++ nameWords(inner) ++ o.toList.flatMap(x => nameWords(x.group)) ++ nameWords(label))
            .filter(_.length >= 2).distinct
    }

    def serverMatch(key : String) : Option[(String, String, Boolean)] = {
        val m = emojiMap()
        if (m.isEmpty) None
        else {
            val clans = (Lfg.coreClans ++ Lfg.bloodClans).map(_.toLowerCase).toSet
            val byNorm = m.toList.sortBy(_._1).map { case (n, (id, a)) => (norm(n), (n, id, a)) }
            def find(c : String) = byNorm.collectFirst { case (n, e) if (n == c || n == "ng" + c || n == "root" + c || n == c + "s" || n + "s" == c) && !clans.contains(n) => e }
            candidates(key).iterator.map(find).collectFirst { case Some(e) => e }
        }
    }

    // the server's emojis and the options and sections that show each (the tournament change "emojis")
    def serverEmojiUse() : String = {
        val keys = Lfg.kinds.map(_.meta).distinct.flatMap(m => catalog(m).toList.flatMap(c => c.options.map(_.name) ++ c.options.map("#" + _.group))).distinct
        val used = keys.flatMap(k => emojiOf(k).map(e => e._1 -> k)).groupBy(_._1).map { case (n, l) => n -> l.map(_._2) }
        emojiMap().keys.toList.sorted.map(n => n + ": " + used.get(n).map(_.take(6).mkString(", ")).getOrElse(if ((Lfg.coreClans ++ Lfg.bloodClans).exists(c => Lfg.serverEmoji(c).exists(_._1 == n))) "a clan" else "unused")).mkString("; ")
    }

    // THE UNICODE EMOJIS of the options and sections with no picture of their own (asked for on 2026-11-21: "use icons/emojis as much as
    // possible"); a section by "#<its name>", which a faction's section finds as that faction's icon (icons.tsv) first
    val unicode = Map(
        // Root
        "#Map" -> "🗺️", "#Fan Maps" -> "🗺️", "#Fan Map Rules" -> "📜", "#Deck" -> "🃏", "#Fan Decks" -> "🃏", "#Clearings" -> "🌲",
        "#Landmarks" -> "🏛️", "#Simple Landmarks" -> "🏛️", "#Hirelings" -> "💰", "#Staunch Hirelings" -> "💰", "#Demoted Hirelings" -> "💰",
        "#Staunch Turnlings" -> "🔄", "#Demoted Turnlings" -> "🔄", "#Turnlings" -> "🔄", "#Hirelings Clash" -> "⚔️",
        "#Insurgent Lock" -> "🔒", "#Available Factions" -> "🐾", "#Available Fan Factions" -> "🐾", "#Seating Order" -> "🪑",
        "#Dominance" -> "👑", "#Hits and Ambushes – Offline Players" -> "🎯", "#Legends Scenario" -> "📜", "#Legendary Monster" -> "👹",
        "#Ring Rule" -> "💍", "#Capture Rule" -> "🫳", "#Mirror Factions" -> "🪞",
        "RandomOfficialMap" -> "🎲", "RandomMap" -> "🎲", "AllRandomClearings" -> "🎲", "DefaultClearings" -> "🌲", "ConnectedClearings" -> "🔗",
        "NoClustersClearings" -> "✳️", "SuitPairsClearings" -> "👯", "ThreeFourFiveClearings" -> "🔢", "MixedDeck" -> "🔀",
        "MapDefaultLandmarks" -> "🗺️", "MountainSuitedLandmark" -> "⛰️", "NoLandmarks" -> "🚫", "NoHirelings" -> "🚫",
        "ThreeRandomHirelings" -> "🎲", "SelectedHirelings" -> "✅", "HirelingsByTheCard" -> "📇", "TurnOverHirelings" -> "🔄",
        "InsurgentLockOn" -> "🔒", "InsurgentLockOff" -> "🔓", "SeatingRandom" -> "🎲", "SeatingGiven" -> "📋", "TotalWarDominance" -> "⚔️",
        "BluffMode" -> "🃏", "AutoHitsAssignmentMode" -> "🤖", "LiveGameMode" -> "🟢", "OffTurnHitsPreset" -> "📋",
        "OffTurnFieldHospitals" -> "🏥", "CastlesAndCatacombs" -> "🏰", "DarkRituals" -> "🕯️", "HuntTheLegend" -> "🏹", "TheFifthRuin" -> "💍",
        "DeepWoodsSettling" -> "🏕️", "WastelandsWater" -> "💧", "OneTrueEmpire" -> "👑", "RootingForTheUnderdog" -> "🐶",
        // Northgard
        "#Game length" -> "⏳", "#Victory conditions" -> "🏆", "#Fame" -> "⭐", "#Undo" -> "↩️", "#Card edition" -> "🃏",
        "#Base game" -> "🐺", "#Warchief expansion" -> "⚔️", "#Wilderness expansion" -> "🌲", "#Wastelands expansion" -> "🌋",
        "#Wilderness or Wastelands" -> "🗺️", "#Wilderness, if played" -> "🌲", "#Wastelands, if played" -> "🌋",
        "#Uncharted Horizons" -> "⛵", "#Development cards" -> "🃏", "#Fan content" -> "🛠️", "#Fan victories" -> "🏅",
        "#Central tile" -> "🎯", "#First player" -> "🥇", "#Ban Development cards" -> "🚫", "#Ban Environment tiles" -> "🚫",
        "#Ban creatures" -> "🚫",
        "YearsOption(5)" -> "5️⃣", "YearsOption(6)" -> "6️⃣", "YearsOption(7)" -> "7️⃣", "YearsOption(8)" -> "8️⃣", "YearsOption(9)" -> "9️⃣",
        "YearsOption(10)" -> "🔟", "PrivateFame" -> "🔒", "UnlimitedUndo" -> "♾️", "LimitedUndo" -> "⏪", "NoUndo" -> "🚫",
        "FirstEdition" -> "1️⃣", "SecondEdition" -> "2️⃣", "RandomAnyCentral" -> "🎲", "RandomCentral" -> "🎲", "LandsNeither" -> "🚫",
        "LandsSeatChooses" -> "🪑", "VictoryModeOption(false)" -> "🛡️", "VictoryModeOption(true)" -> "👑", "TeamVictoryAllCards" -> "🤝",
        "FirstSeatStarts" -> "🥇",
        // the posts' titles, with no server emoji
        "title" -> "🌳", "title-nort" -> "🪓")

    // the emoji: the server's or the bot's, else a Unicode one
    def emojiOrUnicode(key : String) : Option[Either[(String, String, Boolean), String]] = emojiOf(key).map(Left(_)).orElse(unicode.get(key).map(Right(_)))

    // in a message: <:name:id>, or the Unicode emoji
    def emojiText(key : String) = emojiOrUnicode(key).map {
        case Left((n, id, a)) => "<" + (if (a) "a" else "") + ":" + n + ":" + id + "> "
        case Right(u) => u + " "
    }.getOrElse("")

    // in a menu's option
    def emojiField(key : String) = emojiOrUnicode(key).map {
        case Left((n, id, a)) => ",\"emoji\":{\"id\":" + Lfg.json(id) + ",\"name\":" + Lfg.json(n) + ",\"animated\":" + a + "}"
        case Right(u) => ",\"emoji\":{\"name\":" + Lfg.json(u) + "}"
    }.getOrElse("")

    // the picks a post shows (2026-11-21: the options its host chose, in the command and on the setup panel, not the defaults they
    // left as they were): each section of the setup screen with those picks, in the setup screen's order, the server's emoji beside a
    // pick that has one (the factions in the draft by their emoji alone); left out are the options the setup screen doesn't list
    // (hidden, mandatory), those whose needs aren't picked in the game (`all`; Northgard's Thane with the standard victory), the clans'
    // colors and teams and, to fit a Discord message, the sections past `limit` characters
    def summary(c : Catalog, options : List[String], all : List[String] = Nil, limit : Int = 1300) : String = {
        val mandatory = c.mandatory.values.flatten.toSet
        val picked = options.toSet ++ all
        val listed = c.options.filter(o => options.contains(o.name) && o.page >= 0 && !mandatory.contains(o.name))
            .filterNot(o => o.name.startsWith("ColorOption(") || o.name.startsWith("TeamOption("))
            .filter(o => c.required.getOrElse(o.name, List(Nil)).exists(_.forall(picked.contains)))
        val pool = c.need.map(_._1)
        val lines = listed.map(_.group).distinct.map { g =>
            val oo = listed.filter(_.group == g)
            val emojiOnly = pool.exists(p => oo.forall(_.name.startsWith(p)))
            emojiText("#" + g) + "**" + g + "**: " + oo.map(o => if (emojiOnly) emojiText(o.name).trim match { case "" => o.label ; case e => e } else emojiText(o.name) + o.label).mkString(if (emojiOnly) " " else ", ")
        }
        var out = List[String]()
        var left = lines
        while (left.nonEmpty && (out :+ left.head).mkString("\n").length <= limit) { out :+= left.head ; left = left.tail }
        (out ++ (if (left.isEmpty) Nil else List("…and " + left.size + " more " + (if (left.size == 1) "section" else "sections") + " (all of them are in the game's log)")))
            .map(_ + "\n").mkString
    }

    // THE PANELS, by id: the host, their name, the players, the title, the options, the page shown
    // chosen: the options the host picked (in the command, on the panel), which the post lists
    case class Draft(id : String, host : String, name : String, n : Int, title : String, state : State, page : Int, note : String = "", chosen : List[String] = Nil) {
        def picked = chosen.filter(state.actual.contains)
    }

    val drafts = new java.util.concurrent.ConcurrentHashMap[String, Draft]()

    // a menu: its section, the options it lists, whether it takes several; or the players' menu, or the presets'
    case class Menu(group : String, options : List[Opt], many : Boolean, kind : String = "options")

    // the setup screen's sections /root sets (one pick each), beside the map and the deck
    val commandSections = List("Clearings", "Hirelings", "Landmarks", "Insurgent Lock")

    // NORTHGARD (2026-11-21): /northgard sets the game length, the victory conditions, the Wilderness or Wastelands, the warchiefs,
    // the fame and the undo (not the card edition: always the site's default, the 2nd edition, as the owner asked on 2026-11-21);
    // the panel lists Uncharted Horizons (the Sea), the Development cards, Fan content and
    // Fan victories, the rest keeping the site's defaults
    // Both modes (standard and Adset) take Wilderness and Wastelands as tiles, creatures or both (asked for on 2026-11-21): in a standard
    // game the expansion's parts (WildernessTiles ...), in Adset the parts of the expansion drafted or agreed (WildPartsBoth ...); a
    // standard game also takes the Creatures module (and More creatures!), which Adset always has. These sections ("@...") have the
    // same choices in both modes (`translate` turns them into each mode's options)
    val nortCommand = List(("length", "The game's length", "Game length"), ("victory", "The victory conditions", "Victory conditions"),
        ("wilderness", "Wilderness: tiles, creatures or both", "@wilderness"), ("wastelands", "Wastelands: tiles, creatures or both", "@wastelands"),
        ("warchiefs", "The Warchief expansion", "Warchief expansion"),
        ("fame", "Whether fame is public", "Fame"), ("undo", "How much undo is allowed", "Undo"))
    val nortStdCommand = nortCommand.take(2) ++ List(("creatures", "The Creatures module (Wilderness and Wastelands creatures turn it on)", "@creatures")) ++ nortCommand.drop(2)
    val nortAdsetCommand = nortCommand.take(2) ++ List(("lands", "Wilderness or Wastelands", "Wilderness or Wastelands")) ++ nortCommand.drop(2)
    val nortPanel = List("Uncharted Horizons", "Development cards", "Fan content", "Fan victories")

    def isNort(meta : String) = meta == Lfg.nort.meta || meta == Lfg.nortStd.meta

    // the fixed choices of the "@..." sections, and the options each stands for in each mode
    val parts = Map(
        "@wilderness" -> List(("Environment tiles and creatures", "both"), ("Environment tiles only", "tiles"), ("Creatures only", "creatures")),
        "@wastelands" -> List(("Environment tiles and creatures", "both"), ("Environment tiles only", "tiles"), ("Creatures only", "creatures")),
        "@creatures" -> List(("Creatures module", "on"), ("Creatures module and More creatures!", "more")))

    def translate(k : Lfg.Kind, key : String, v : String) : List[String] = (k.meta, key, v) match {
        case ("nort", "wilderness" | "wastelands", _) =>
            val (t, c) = if (key == "wilderness") ("WildernessTiles", "WildernessCreatures") else ("WastelandsTiles", "WastelandsCreatures")
            v match {
                case "tiles" => List(t)
                case "creatures" => List("ModuleOption(Creatures)", c)
                case _ => List("ModuleOption(Creatures)", t, c)
            }
        case ("nort", "creatures", "more") => List("ModuleOption(Creatures)", "MoreCreatures")
        case ("nort", "creatures", _) => List("ModuleOption(Creatures)")
        case ("nort-adset", "wilderness" | "wastelands", _) =>
            List((if (key == "wilderness") "WildParts" else "WasteParts") + (v match { case "tiles" => "Tiles" ; case "creatures" => "Creatures" ; case _ => "Both" }))
        case ("nort-adset", "creatures", _) => Nil
        case _ => List(v)
    }

    // the sections a post lists, beside the Important ones
    def summarySections(meta : String) = if (meta == Lfg.nortStd.meta) List("Base game", "Warchief expansion", "Fame", "Undo") else if (isNort(meta)) List("Warchief expansion", "Fame", "Undo") else commandSections

    // the command's options: its name, description and the setup screen's section (Root's map and deck: its Important sections, "#0", "#1")
    def sections(k : Lfg.Kind) : List[(String, String, String)] =
        if (k == Lfg.nort) nortAdsetCommand
        else if (k == Lfg.nortStd) nortStdCommand
        else List(("map", "The map", "#0"), ("deck", "The deck", "#1"), ("clearings", "How the clearings' suits are set", "Clearings"), ("hirelings", "The hirelings", "Hirelings"),
            ("landmarks", "A landmark", "Landmarks"), ("insurgent_lock", "Whether the last insurgent is locked", "Insurgent Lock"))
    // the panel's sections: the hits and ambushes (the section whose name starts so), and the hirelings in their draft, when the
    // hirelings picked aren't none
    val hitsSection = "Hits and Ambushes"
    val hirelingDraft = List("Demoted Hirelings", "Staunch Hirelings")
    val noHirelings = "NoHirelings"

    // the map and the deck: the Important sections, each with the one that shares its choice (Fan Maps, Fan Decks)
    def mainSections(s : State) : List[List[Opt]] = {
        val shown = s.c.options.filter(s.shown)
        var main = List[List[Opt]]()
        shown.filter(_.important).map(_.group).distinct.map(g => shown.filter(_.group == g)).foreach { g =>
            val i = main.indexWhere(m => m.size + g.size <= 25 && m.exists(o => s.forcedOff(o.name).exists(x => g.exists(_.name == x))))
            if (i >= 0) main = main.updated(i, main(i) ++ g) else main :+= g
        }
        main
    }

    // THE PANEL, one page (asked for on 2026-11-20): what /root didn't set. The map, the deck, the clearings, the hirelings, the
    // landmarks and the insurgent lock are given with /root (else the site's defaults), so the panel has the hits and ambushes, the
    // factions in the draft (the first section of them: the official ones) and, with hirelings, the hirelings in their draft. The rest
    // (faction rules, fan factions...) keep the site's defaults.
    def pages(d : Draft) : List[List[Menu]] = if (isNort(d.state.c.meta)) {
        // Northgard: a menu for each of its panel's sections, of several when it has toggles
        val s = d.state
        def listed(g : List[Opt]) = g.filter(o => !s.mandatory.contains(o.name) && (s.selected.contains(o.name) || s.enabled(o.name)))
        val shown = s.c.options.filter(s.shown)
        List(nortPanel.map(g => listed(shown.filter(_.group == g)).take(25)).filter(_.nonEmpty).map(oo => Menu(oo.head.group, oo, oo.exists(_.toggle))).take(4))
    } else {
        val s = d.state
        def listed(g : List[Opt]) = g.filter(o => !s.mandatory.contains(o.name) && (s.selected.contains(o.name) || s.enabled(o.name)))
        val shown = s.c.options.filter(s.shown)
        def section(g : String) = shown.filter(_.group == g)
        val hits = shown.find(_.group.startsWith(hitsSection)).map(o => Menu(hitsSection, listed(section(o.group)), true)).toList
        val pool = s.c.need.map(_._1).flatMap(p => shown.find(_.name.startsWith(p))).map(o => Menu("Factions in the draft", listed(section(o.group)), true)).toList
        val hirelings = s.actual.exists(o => s.c.byName.get(o).exists(_.group == "Hirelings")) && !s.actual.contains(noHirelings)
        val draft = if (hirelings) hirelingDraft.map(g => Menu(g, listed(section(g)), true)) else Nil
        List((hits ++ pool ++ draft).take(4).filter(_.options.nonEmpty))
    }

    def cut(s : String, n : Int) = if (s.length <= n) s else s.take(n - 1) + "…"

    // a menu: each option's name, value, whether it's shown as picked, emoji and a line under it ("" for none)
    def select(id : String, placeholder : String, min : Int, max : Int, options : List[(String, String, Boolean, String, String)]) =
        "{\"type\":1,\"components\":[{\"type\":3,\"custom_id\":" + Lfg.json(id) + ",\"placeholder\":" + Lfg.json(cut(placeholder, 150)) +
        ",\"min_values\":" + min + ",\"max_values\":" + max + ",\"options\":[" +
        options.map { case (label, value, on, emoji, line) => "{\"label\":" + Lfg.json(cut(label, 100)) + ",\"value\":" + Lfg.json(value) + ",\"default\":" + on + emoji +
            (if (line.isEmpty) "" else ",\"description\":" + Lfg.json(cut(line, 100))) + "}" }.mkString(",") + "]}]}"

    def button(label : String, style : Int, id : String, disabled : Boolean = false) =
        "{\"type\":2,\"style\":" + style + ",\"label\":" + Lfg.json(label) + ",\"custom_id\":" + Lfg.json(id) + ",\"disabled\":" + disabled + "}"

    // the panel as Discord shows it: the page's menus, Back, Next, Open the game and Cancel
    def panel(d : Draft) : String = {
        val s = d.state
        val pp = pages(d)
        val page = d.page.max(0).min(pp.size - 1)
        val menus = pp(page)
        val k = "lfgs:" + d.id + ":"

        def picked(o : Opt) = s.selected.contains(o.name)
        // each option named with its section in a menu of one pick (the closed menu shows only the pick) and in the menu of several
        // sections; a menu of several shows no picks when closed (each would take a line), only a count or the picks' names, and
        // under each option whether it's picked, a pick changing it
        val rows = menus.zipWithIndex.map { case (m, i) =>
            val id = k + "m:" + page + ":" + i
            m.kind match {
                case "choices" =>
                    val now = m.options.filter(picked).map(o => o.group + ": " + o.label).mkString(" · ")
                    select(id, if (now.isEmpty) m.group else now, 0, m.options.size,
                        m.options.map(o => (o.group + ": " + o.label, o.name, false, emojiField(o.name), if (picked(o)) "✅ Picked" else "Pick to switch to this")))
                case _ if m.many =>
                    val on = m.options.count(picked)
                    select(id, m.group + ": " + on + " of " + m.options.size + " · pick to change", 0, m.options.size,
                        m.options.map(o => (o.label, o.name, false, emojiField(o.name), if (picked(o)) "✅ Picked · pick to take it out" else "Not picked · pick to add it")))
                case _ =>
                    select(id, m.group, 1, 1, m.options.map(o => (m.group + ": " + o.label, o.name, picked(o), emojiField(o.name), "")))
            }
        }

        val buttons = "{\"type\":1,\"components\":[" + List(
            button("−", 2, k + "fewer", !s.c.players.exists(_ < d.n)), button(d.n + " players", 2, k + "count", true), button("+", 2, k + "more", !s.c.players.exists(_ > d.n)),
            button("Open the game", 3, k + "open"), button("Cancel", 4, k + "cancel")).mkString(",") + "]}"

        val kind = Lfg.kind(s.c.meta)
        val text = emojiText(kind.emoji) + "**" + kind.label + ": " + d.title + "**, " + d.n + " players\n" +
            summary(s.c, d.picked, s.actual) +
            (if (d.note.nonEmpty) "**" + d.note + "**\n" else "") +
            (if (isNort(kind.meta))
            "The game length, victory conditions, " + (if (kind.clans) "Creatures, " else "the expansion drafted, ") + "Wilderness and Wastelands parts, warchiefs, fame and undo are as given with /" + kind.command + " (or the site's defaults). " +
            "Pick the Uncharted Horizons modules, the Development cards and any fan content, then press **Open the game** to post it here for players to join. Only you see this."
            else
            "The map, deck, clearings, hirelings, landmarks and insurgent lock are as given with /root (or the site's defaults). Pick the hits and ambushes, the factions in the draft" +
            (if (menus.exists(m => hirelingDraft.contains(m.group))) " and the hirelings in their draft" else "") +
            ", then press **Open the game** to post it here for players to join. Only you see this.")

        "{\"content\":" + Lfg.json(cut(text, 1990)) + ",\"flags\":64,\"components\":[" + (rows :+ buttons).mkString(",") + "],\"allowed_mentions\":{\"parse\":[]}}"
    }

    def reply(kind : Int, data : String) = "{\"type\":" + kind + ",\"data\":" + data + "}"

    // /root: a new panel, or nothing without a catalog (the plain post then, as before the panel)
    // picks: the options given with the command (map, deck...), picked over the defaults as if tapped on the panel
    def open(k : Lfg.Kind, uid : String, name : String, n : Int, title : String, picks : List[String] = Nil) : Option[String] = catalog(k.meta).map { c =>
        val players = if (c.players.contains(n)) n else c.players.find(_ >= n).getOrElse(c.players.last)
        var state = State.defaults(c, players)
        picks.foreach(o => if (!state.selected.contains(o) && state.enabled(o)) state = state.click(o))
        val d = Draft(Tournament.newSecret(10), uid, name, players, title, state, 0, chosen = picks.filter(state.selected.contains).distinct)
        drafts.put(d.id, d)
        reply(4, panel(d))
    }

    // THE COMMAND'S OPTIONS (asked for on 2026-11-20): /root map, deck, clearings, hirelings and landmarks, their choices the panel's
    // (the catalog's, for four players), registered with the command (Lfg.register, again when the catalog changes)
    // (Northgard's: the choices of its sections that are always listed, so a victory card waits for the panel)
    def commandOptions(k : Lfg.Kind) : List[(String, String, List[(String, String)])] = catalog(k.meta).toList.flatMap { c =>
        val s = State.defaults(c, 4)
        val main = mainSections(s)
        val shown = c.options.filter(s.shown).filter(o => !s.mandatory.contains(o.name))
        def section(g : String) = if (g.startsWith("#")) main.lift(g.drop(1).toInt).getOrElse(Nil) else shown.filter(_.group == g).filter(o => !isNort(k.meta) || !o.cond)
        sections(k).map { case (n, d, g) => (n, d, if (g.startsWith("@")) parts.getOrElse(g, Nil) else section(g).take(25).map(o => (o.label, o.name))) }.filter(_._3.nonEmpty)
    }

    // a menu picked or a button pressed on a panel
    def press(uid : String, custom : String, values : List[String], app : String, token : String) : String = {
        val parts = custom.split(':').toList
        val d = Option(drafts.get(parts.lift(1).getOrElse(""))).getOrElse(return Lfg.privately("This setup was lost (the server restarted): type " + Lfg.commands + " again."))
        if (d.host != uid)
            return Lfg.privately("Only the host can change this game's options.")
        val c = d.state.c
        def show(x : Draft) = { drafts.put(x.id, x) ; reply(7, panel(x)) }

        parts.drop(2) match {
            case ("fewer" | "more") :: Nil =>
                val n = (if (parts(2) == "fewer") c.players.filter(_ < d.n).lastOption else c.players.find(_ > d.n)).getOrElse(d.n)
                show(d.copy(n = n, state = State.of(c, n, d.state.selected), note = ""))
            case "players" :: Nil =>
                val n = values.headOption.flatMap(_.toIntOption).filter(c.players.contains).getOrElse(d.n)
                show(d.copy(n = n, state = State.of(c, n, d.state.selected), note = ""))
            case "preset" :: Nil =>
                val l = c.presets.getOrElse(d.n, Nil)
                values.headOption.flatMap(_.toIntOption).flatMap(l.lift) match {
                    case Some((t, oo)) => show(d.copy(state = State.of(c, d.n, oo), note = t + ": set"))
                    case None => show(d)
                }
            case "m" :: p :: i :: Nil =>
                pages(d).lift(p.toIntOption.getOrElse(-1)).flatMap(_.lift(i.toIntOption.getOrElse(-1))) match {
                    case None => show(d.copy(note = ""))
                    case Some(m) if m.kind == "options" || m.kind == "choices" =>
                        val offered = m.options.map(_.name)
                        var s = d.state
                        // as if each were tapped on the setup screen: a menu of several changes each one picked (in or out), a menu
                        // of one picks it
                        values.filter(offered.contains).foreach { o =>
                            if (s.selected.contains(o)) { if (m.many && s.toggle(o)) s = s.click(o) }
                            else if (s.enabled(o)) s = s.click(o)
                        }
                        show(d.copy(state = s, note = "", chosen = (d.chosen ++ values.filter(offered.contains)).distinct))
                    case Some(_) => show(d.copy(note = ""))
                }
            case "back" :: Nil => show(d.copy(page = (d.page - 1).max(0), note = ""))
            case "next" :: Nil => show(d.copy(page = (d.page + 1).min(pages(d).size - 1), note = ""))
            case "cancel" :: Nil =>
                drafts.remove(d.id)
                reply(7, "{\"content\":\"Cancelled: nothing was posted.\",\"components\":[],\"flags\":64}")
            case "open" :: Nil =>
                val missing = c.need.map { case (prefix, k) => (d.n + k) - d.state.actual.count(_.startsWith(prefix)) }.filter(_ > 0)
                missing match {
                    case Some(x) => show(d.copy(note = "Pick " + x + " more " + (if (x == 1) "faction" else "factions") + " for the draft: at least one for each player."))
                    case None =>
                        drafts.remove(d.id)
                        Lfg.posted(d, app, token)
                        reply(7, "{\"content\":" + Lfg.json("**" + d.title + "** is posted below. The game starts by itself when all " + d.n + " seats are taken.") + ",\"components\":[],\"flags\":64}")
                }
            case _ => show(d)
        }
    }
}
