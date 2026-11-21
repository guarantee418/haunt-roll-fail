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
object LfgSetup {
    import Backup.{rows, row}

    val file = TurnPings.dir.resolve("lfg-catalog.tsv")

    case class Opt(name : String, page : Int, ns : Set[Int], toggle : Boolean, cond : Boolean, important : Boolean, group : String, label : String)

    case class Catalog(meta : String, hash : String, version : String, players : List[Int], need : Option[(String, Int)],
        mandatory : Map[Int, List[String]], defaults : Map[Int, List[String]], presets : Map[Int, List[(String, List[String])]],
        options : List[Opt], required : Map[String, List[List[String]]], blocked : Map[String, List[List[String]]],
        on : Map[String, List[String]], also : Map[String, List[String]], off : Map[String, List[String]], clear : Map[String, List[String]]) {
        val byName = options.map(o => o.name -> o).toMap
        def all(n : Int) = options.filter(_.ns.contains(n)).map(_.name)
        def label(o : String) = byName.get(o).map(_.label).getOrElse(o)
    }

    @volatile var cached : Option[(Long, Catalog)] = None

    def catalog() : Option[Catalog] = {
        if (!Files.exists(file))
            return None
        val t = Files.getLastModifiedTime(file).toMillis
        if (!cached.exists(_._1 == t))
            cached = parse(new String(Files.readAllBytes(file), UTF_8)).map(c => (t, c))
        cached.map(_._2)
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
    def check(meta : String, hash : String) = if (catalog().exists(c => c.meta == meta && c.hash == hash)) "have" else "need"

    // the client's catalog: "<version>\n<lines>"
    def setCatalog(meta : String, hash : String, body : String) : String = synchronized {
        val ok = meta == Lfg.meta && hash.nonEmpty && hash.length <= 20 && hash.forall(_.isDigit) && body.length < 4000000
        val parsed = if (ok) parse(meta + "\t" + hash + "\n" + body) else None
        parsed.filter(c => c.players.nonEmpty && c.options.nonEmpty) match {
            case Some(_) =>
                Files.createDirectories(TurnPings.dir)
                Files.write(file, (meta + "\t" + hash + "\n" + body).getBytes(UTF_8))
                // /root's choices come from it: registered again
                if (Lfg.publicKey.nonEmpty)
                    new Thread(() => println("LFG: /root " + Lfg.register())).start()
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
        "players 3" -> "3p", "players 4" -> "4p", "players 5" -> "5p", "title" -> "adset")

    def emojiOf(key : String) : Option[(String, String, Boolean)] = {
        val n = emojiNames.get(key).orElse(Some(key).filter(_.startsWith("IncludeFaction(")).map(_.stripPrefix("IncludeFaction(").stripSuffix(")").toLowerCase))
        n.flatMap(x => emojiMap().get(x).map { case (id, a) => (x, id, a) })
    }

    // in a message: <:name:id>
    def emojiText(key : String) = emojiOf(key).map { case (n, id, a) => "<" + (if (a) "a" else "") + ":" + n + ":" + id + "> " }.getOrElse("")

    // in a menu's option
    def emojiField(key : String) = emojiOf(key).map { case (n, id, a) => ",\"emoji\":{\"id\":" + Lfg.json(id) + ",\"name\":" + Lfg.json(n) + ",\"animated\":" + a + "}" }.getOrElse("")

    // the picks a post shows: the Important ones (map, deck...) and the factions in the draft
    def summary(c : Catalog, options : List[String]) : String = {
        val pool = c.need.map(_._1).toList.flatMap(p => options.filter(_.startsWith(p)))
        val important = options.filter(o => c.byName.get(o).exists(_.important)).map(o => emojiText(o) + c.label(o))
        (if (important.isEmpty) "" else important.mkString(" · ") + "\n") +
        (if (pool.isEmpty) "" else "Factions in the draft: " + pool.map(o => emojiText(o).trim match { case "" => c.label(o) ; case e => e }).mkString(" ") + "\n")
    }

    // THE PANELS, by id: the host, their name, the players, the title, the options, the page shown
    case class Draft(id : String, host : String, name : String, n : Int, title : String, state : State, page : Int, note : String = "")

    val drafts = new java.util.concurrent.ConcurrentHashMap[String, Draft]()

    // a menu: its section, the options it lists, whether it takes several; or the players' menu, or the presets'
    case class Menu(group : String, options : List[Opt], many : Boolean, kind : String = "options")

    // the sections one menu holds together, each option named with its section (asked for on 2026-11-20: what is random must say
    // what it is): the setup screen's section names
    val together = List("Clearings", "Hirelings")

    // THE PANEL, one page (asked for on 2026-11-20): Discord shows four menus and a row of buttons, so it has the main picks (the
    // Important sections, the map and the deck, each with the section that shares its choice: Fan Maps, Fan Decks), the clearings and
    // hirelings together, and the factions in the draft (the first section of them: the official ones). The rest (faction rules,
    // landmarks, fan factions...) keep the site's defaults.
    def pages(d : Draft) : List[List[Menu]] = {
        val s = d.state
        def listed(g : List[Opt]) = g.filter(o => !s.mandatory.contains(o.name) && (s.selected.contains(o.name) || s.enabled(o.name)))
        val shown = s.c.options.filter(s.shown)
        def section(g : String) = shown.filter(_.group == g)
        var main = List[List[Opt]]()
        shown.filter(_.important).map(_.group).distinct.map(section).foreach { g =>
            val i = main.indexWhere(m => m.size + g.size <= 25 && m.exists(o => s.forcedOff(o.name).exists(x => g.exists(_.name == x))))
            if (i >= 0) main = main.updated(i, main(i) ++ g) else main :+= g
        }
        val picks = main.take(2).map(g => Menu(g.head.group, listed(g), false))
        val choices = Menu(together.mkString(" and "), listed(together.flatMap(section)), true, "choices")
        val pool = s.c.need.map(_._1).flatMap(p => shown.find(_.name.startsWith(p))).map(o => Menu("Factions in the draft", listed(section(o.group)), true)).toList
        List((picks :+ choices) ++ pool).map(_.filter(_.options.nonEmpty))
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

        val text = emojiText("title") + "**" + Lfg.label + ": " + d.title + "**, " + d.n + " players\n" +
            summary(s.c, s.actual) +
            (if (d.note.nonEmpty) "**" + d.note + "**\n" else "") +
            "Pick the map, the deck, the clearings and hirelings and the factions (the site's defaults are picked; everything else is as the site sets it), then press **Open the game** to post it here for players to join. Only you see this."

        "{\"content\":" + Lfg.json(cut(text, 1990)) + ",\"flags\":64,\"components\":[" + (rows :+ buttons).mkString(",") + "],\"allowed_mentions\":{\"parse\":[]}}"
    }

    def reply(kind : Int, data : String) = "{\"type\":" + kind + ",\"data\":" + data + "}"

    // /root: a new panel, or nothing without a catalog (the plain post then, as before the panel)
    // picks: the options given with the command (map, deck...), picked over the defaults as if tapped on the panel
    def open(uid : String, name : String, n : Int, title : String, picks : List[String] = Nil) : Option[String] = catalog().filter(_.meta == Lfg.meta).map { c =>
        val players = if (c.players.contains(n)) n else c.players.find(_ >= n).getOrElse(c.players.last)
        var state = State.defaults(c, players)
        picks.foreach(o => if (!state.selected.contains(o) && state.enabled(o)) state = state.click(o))
        val d = Draft(Tournament.newSecret(10), uid, name, players, title, state, 0)
        drafts.put(d.id, d)
        reply(4, panel(d))
    }

    // THE COMMAND'S OPTIONS (asked for on 2026-11-20): /root map, deck, clearings, hirelings and landmarks, their choices the panel's
    // (the catalog's, for four players), registered with the command (Lfg.register, again when the catalog changes)
    def commandOptions() : List[(String, String, List[(String, String)])] = catalog().filter(_.meta == Lfg.meta).toList.flatMap { c =>
        val s = State.defaults(c, 4)
        val main = pages(Draft("", "", "", 4, "", s, 0)).head.filter(m => m.kind == "options" && !m.many)
        val shown = c.options.filter(s.shown).filter(o => !s.mandatory.contains(o.name))
        def section(g : String) = shown.filter(_.group == g)
        List(("map", "The map", main.lift(0).map(_.options).getOrElse(Nil)), ("deck", "The deck", main.lift(1).map(_.options).getOrElse(Nil)),
            ("clearings", "How the clearings' suits are set", section("Clearings")), ("hirelings", "The hirelings", section("Hirelings")),
            ("landmarks", "A landmark", section("Landmarks")))
            .map { case (n, d, oo) => (n, d, oo.take(25).map(o => (o.label, o.name))) }.filter(_._3.nonEmpty)
    }

    // a menu picked or a button pressed on a panel
    def press(uid : String, custom : String, values : List[String], app : String, token : String) : String = {
        val parts = custom.split(':').toList
        val d = Option(drafts.get(parts.lift(1).getOrElse(""))).getOrElse(return Lfg.privately("This setup was lost (the server restarted): type /root again."))
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
                        show(d.copy(state = s, note = ""))
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
