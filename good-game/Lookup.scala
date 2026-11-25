package hrf.gg

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path, Paths}

// THE BOT'S OTHER SLASH COMMANDS (asked for on 2026-11-22): commands besides the LFG ones (Lfg.scala), each a BotCommand in
// BotCommands.all. Lfg.register sends them to Discord with /root and /northgard (a PUT sets every command the server has), and
// Lfg.interaction hands each one its use (type 2) and the autocomplete of its options (type 4). A later command (a translator,
// a rules helper) is one more BotCommand in the list.
trait BotCommand {
    // its name, as typed after the slash
    def name : String
    // its registration: the JSON of a Discord application command
    def registration : String
    // a use of it: Discord's answer (Lfg.reply)
    def run(uid : String, user : String, data : Lfg.J) : String
    // an option being typed: Discord's answer, the choices (type 8)
    def autocomplete(data : Lfg.J) : String = choices(Nil)

    def choices(l : List[(String, String)]) = "{\"type\":8,\"data\":{\"choices\":[" + l.take(25).map { case (n, v) => "{\"name\":" + Lfg.json(LfgSetup.cut(n, 100)) + ",\"value\":" + Lfg.json(v.take(100)) + "}" }.mkString(",") + "]}}"

    // the options given: name -> value (a string, a number's text, or a boolean)
    def options(data : Lfg.J) : Map[String, Any] = data.get("options").collect { case l : List[_] => l.map(Lfg.obj) }.getOrElse(Nil).map(o => Lfg.str(o.get("name")) -> o.get("value").orNull).toMap
    // the option being typed in an autocomplete
    def focused(data : Lfg.J) : Option[(String, String)] = data.get("options").collect { case l : List[_] => l.map(Lfg.obj) }.getOrElse(Nil)
        .find(_.get("focused").contains(true)).map(o => Lfg.str(o.get("name")) -> Lfg.str(o.get("value")))
}

object BotCommands {
    val all : List[BotCommand] = List(CardLookup, RuleLookup, SelfRoles)

    def of(name : String) = all.find(_.name == name)
}

// /card <name> [game] (asked for on 2026-11-22): a card, faction or clan board and the like, by its name in the game, posted with its
// picture in the channel. Names come from webp2/lfg/cards.tsv in the client's directory, made by the client's LookupIndex
// (haunt-roll-fail/lookup-index.scala) from the games' cards and images; the name option suggests matches as it is typed. Discord
// fetches the picture from /art/<path>, which serves only the images in that list (the site's own images need the site as Referer).
object CardLookup extends BotCommand {
    val name = "card"

    case class Art(game : String, kind : String, name : String, group : String, path : String) {
        def gameLabel = Lookup.games.getOrElse(game, game)
        // as the suggestions list it: "Anvil · Might and Right (Root)"
        def label = name + " · " + group + " (" + gameLabel + ")"
        lazy val key = Lookup.norm(name)
    }

    @volatile var loaded : (Long, List[Art]) = (-1L, Nil)

    def file = Paths.get(Lfg.assets, "webp2", "lfg", "cards.tsv")

    // the list, read again when the file changes (a deploy)
    def arts : List[Art] = {
        val f = file
        val t = if (Files.exists(f)) Files.getLastModifiedTime(f).toMillis else 0L
        if (t != loaded._1)
            loaded = (t, if (t == 0L) Nil else
                new String(Files.readAllBytes(f), UTF_8).split('\n').toList.drop(1).map(_.split('\t').toList).collect {
                    case g :: k :: n :: gr :: p :: _ if p.startsWith("webp2/") && !p.contains("..") => Art(g, k, n, gr, p.trim)
                })
        loaded._2
    }

    // the image files the list names (and so /art serves; the rulebook pages are RuleLookup's)
    def served(path : String) = arts.exists(_.path == path)

    def registration = "{\"name\":\"card\",\"type\":1,\"description\":\"Show a card or other art from Root or Northgard, as the site has it\",\"options\":[" +
        "{\"type\":3,\"name\":\"name\",\"description\":\"The card's name (pick from the suggestions)\",\"required\":true,\"autocomplete\":true,\"max_length\":100}," +
        "{\"type\":3,\"name\":\"game\",\"description\":\"Only this game's cards\",\"required\":false,\"choices\":[" +
        Lookup.games.toList.map { case (v, n) => "{\"name\":" + Lfg.json(n) + ",\"value\":" + Lfg.json(v) + "}" }.mkString(",") + "]}," +
        "{\"type\":5,\"name\":\"private\",\"description\":\"Show it only to me\",\"required\":false}]}"

    // the best matches for what was typed: the whole name, then its start, a word's start, anywhere, and every word somewhere
    def search(q : String, game : Option[String]) : List[Art] = {
        val k = Lookup.norm(q)
        val words = q.toLowerCase.split("[^\\p{L}\\p{N}]+").toList.filter(_.nonEmpty).map(Lookup.norm)
        val l = arts.filter(a => game.forall(_ == a.game))
        if (k.isEmpty)
            l
        else {
            def score(a : Art) : Int = {
                val wordsOf = a.name.toLowerCase.split("[^\\p{L}\\p{N}]+").toList.map(Lookup.norm)
                if (a.key == k) 0
                else if (a.key.startsWith(k)) 1
                else if (wordsOf.exists(_.startsWith(k))) 2
                else if (a.key.contains(k)) 3
                else if (words.nonEmpty && words.forall(w => wordsOf.exists(_.startsWith(w)))) 4
                else if (words.nonEmpty && words.forall(w => a.key.contains(w))) 5
                else if ((a.key + Lookup.norm(a.group)).contains(k)) 6
                else 99
            }
            l.map(a => a -> score(a)).filter(_._2 < 99).sortBy { case (a, s) => (s, a.name.length, a.name.toLowerCase, Lookup.later(a.group), a.game, a.group) }.map(_._1)
        }
    }

    override def autocomplete(data : Lfg.J) = {
        val o = options(data)
        val game = o.get("game").collect { case v : String => v }.filter(Lookup.games.contains)
        val typed = focused(data).filter(_._1 == "name").map(_._2).getOrElse("")
        choices(search(typed, game).take(25).map(a => a.label -> ("@" + a.path.stripPrefix("webp2/"))))
    }

    def run(uid : String, user : String, data : Lfg.J) = {
        val o = options(data)
        val game = o.get("game").collect { case v : String => v }.filter(Lookup.games.contains)
        val typed = o.get("name").collect { case v : String => v }.getOrElse("").trim
        val hidden = o.get("private").contains(true)
        // a suggestion picked is its path; anything else typed is searched
        val picked = Some(typed).filter(_.startsWith("@")).map("webp2/" + _.drop(1)).flatMap(p => arts.find(_.path == p))
        val found = picked.map(List(_)).getOrElse(search(typed, game))
        found match {
            case Nil if arts.isEmpty => Lfg.privately("The card list isn't on the server yet.")
            case Nil => Lfg.privately("No card called **" + Lfg.clean(typed) + "**" + game.map(g => " in " + Lookup.games(g)).getOrElse("") + ". Pick one of the suggestions as you type.")
            case a :: rest =>
                // the others with the same name (another deck's or edition's), so they can be asked for
                val also = rest.filter(_.key == a.key).take(5)
                val more = if (also.isEmpty) "" else "\nAlso: " + also.map(x => x.group + " (" + x.gameLabel + ")").mkString(", ")
                val embed = "{\"title\":" + Lfg.json(a.name) + ",\"description\":" + Lfg.json(a.group + " · " + a.gameLabel + more) +
                    ",\"color\":" + Lookup.colors.getOrElse(a.game, 0x888888) +
                    ",\"image\":{\"url\":" + Lfg.json(Lookup.artUrl(a.path)) + "}}"
                Lfg.reply(4, "{\"embeds\":[" + embed + "]" + (if (hidden) ",\"flags\":64" else "") + ",\"allowed_mentions\":{\"parse\":[]}}")
        }
    }
}

// /rule <rule> [game] (asked for on 2026-11-22): a rule cited, not interpreted. Root's by its Law of Root number or name, with its text and
// the link to it on the Root Database (webp2/lfg/rules-root.tsv, made by haunt-roll-fail/tools/law-index.py from the Root Database's copy);
// Northgard's rulebooks have no numbers, so a rule there is its book, page and section heading, shown with the page itself
// (webp2/lfg/rules-nort.tsv and webp2/lfg/rulebook/nort/, made by haunt-roll-fail/tools/nort-rules-index.py from the rulebook PDFs).
object RuleLookup extends BotCommand {
    val name = "rule"

    case class Rule(game : String, id : String, number : String, title : String, group : String, text : String, link : String, path : String) {
        // as the suggestions list it: "6.5.1 Battle · Marquise de Cat (Root)", "Combat steps · Core rulebook, p. 14 (Northgard)"
        def label = if (game == "root") number + " " + title + " · " + group.replaceFirst("^[0-9A-Z]+\\. ", "") + " (Root)" else title + " · " + group + " (Northgard)"
        lazy val key = Lookup.norm(title)
        lazy val words = title.toLowerCase.split("[^\\p{L}\\p{N}]+").toList.filter(_.nonEmpty).map(Lookup.norm)
    }

    @volatile var loaded : (Long, List[Rule]) = (-1L, Nil)

    def files = List("root", "nort").map(g => g -> Paths.get(Lfg.assets, "webp2", "lfg", "rules-" + g + ".tsv"))

    def rows(f : Path) = new String(Files.readAllBytes(f), UTF_8).split('\n').toList.drop(1).filter(_.trim.nonEmpty).map(_.split('\t').toList)

    // the lists, read again when a file changes (a deploy)
    def rules : List[Rule] = {
        val t = files.map { case (_, f) => if (Files.exists(f)) Files.getLastModifiedTime(f).toMillis else 0L }.sum
        if (t != loaded._1)
            loaded = (t, files.filter(x => Files.exists(x._2)).flatMap {
                case ("root", f) => rows(f).collect { case n :: ti :: ch :: tx :: l :: _ => Rule("root", n, n, ti, ch, tx, l.trim, "") }
                case (_, f) => rows(f).collect { case b :: pg :: se :: p :: _ if p.startsWith("webp2/") && !p.contains("..") =>
                    Rule("nort", b + "|" + pg + "|" + se, "", se, b + ", p. " + pg, "", "", p.trim) }
            })
        loaded._2
    }

    // the rulebook pages the list names (and so /art serves)
    def served(path : String) = rules.exists(_.path == path)

    def registration = "{\"name\":\"rule\",\"type\":1,\"description\":\"Cite a rule: Root's by its Law of Root number or name, Northgard's by its name\",\"options\":[" +
        "{\"type\":3,\"name\":\"rule\",\"description\":\"A rule number (4.3.2) or name (pick from the suggestions)\",\"required\":true,\"autocomplete\":true,\"max_length\":100}," +
        "{\"type\":3,\"name\":\"game\",\"description\":\"Only this game's rules\",\"required\":false,\"choices\":[" +
        Lookup.games.toList.map { case (v, n) => "{\"name\":" + Lfg.json(n) + ",\"value\":" + Lfg.json(v) + "}" }.mkString(",") + "]}," +
        "{\"type\":5,\"name\":\"private\",\"description\":\"Show it only to me\",\"required\":false}]}"

    // a Law of Root number: 4, 4.3, 4.3.2, 2.1.1.I, H.2.1 (a dot or comma, any case)
    def numberOf(q : String) = Some(q.trim.toUpperCase.replace(',', '.').stripSuffix(".")).filter(_.matches("([0-9]{1,2}|[A-Z])(\\.[0-9]{1,2})*(\\.[IVX]+)?"))

    // the best matches: a number and the rules under it; or the name, then its start, a word's start, anywhere, every word somewhere
    def search(q : String, game : Option[String]) : List[Rule] = {
        val l = rules.filter(r => game.forall(_ == r.game))
        numberOf(q).filter(n => l.exists(r => r.game == "root" && (r.number == n || r.number.startsWith(n + ".")))) match {
            case Some(n) =>
                l.filter(r => r.game == "root" && (r.number == n || r.number.startsWith(n + "."))).sortBy(r => (r.number != n, r.number.count(_ == '.')))
            case None =>
                val k = Lookup.norm(q)
                val words = q.toLowerCase.split("[^\\p{L}\\p{N}]+").toList.filter(_.nonEmpty).map(Lookup.norm)
                if (k.isEmpty)
                    l.filter(r => r.game == "nort" || r.number.count(_ == '.') <= 1)
                else {
                    def score(r : Rule) : Int =
                        if (r.key == k) 0
                        else if (r.key.startsWith(k)) 1
                        else if (r.words.exists(_.startsWith(k))) 2
                        else if (r.key.contains(k)) 3
                        else if (words.nonEmpty && words.forall(w => r.words.exists(_.startsWith(w)))) 4
                        else if (words.nonEmpty && words.forall(w => (r.key + Lookup.norm(r.group)).contains(w))) 5
                        else if (words.nonEmpty && words.forall(w => Lookup.norm(r.text).contains(w))) 7
                        else 99
                    l.map(r => r -> score(r)).filter(_._2 < 99).sortBy { case (r, s) => (s, r.title.length, r.game, r.number.count(_ == '.')) }.map(_._1)
                }
        }
    }

    override def autocomplete(data : Lfg.J) = {
        val o = options(data)
        val game = o.get("game").collect { case v : String => v }.filter(Lookup.games.contains)
        val typed = focused(data).filter(_._1 == "rule").map(_._2).getOrElse("")
        choices(search(typed, game).take(25).map(r => r.label -> ("@" + r.game + ":" + r.id)))
    }

    def run(uid : String, user : String, data : Lfg.J) = {
        val o = options(data)
        val game = o.get("game").collect { case v : String => v }.filter(Lookup.games.contains)
        val typed = o.get("rule").collect { case v : String => v }.getOrElse("").trim
        val hidden = o.get("private").contains(true)
        // a suggestion picked is its game and id; anything else typed is searched
        val picked = Some(typed).filter(_.startsWith("@")).map(_.drop(1)).flatMap(v => rules.find(r => r.game + ":" + r.id == v))
        val found = picked.map(List(_)).getOrElse(search(typed, game))
        found match {
            case Nil if rules.isEmpty => Lfg.privately("The rule lists aren't on the server yet.")
            case Nil => Lfg.privately("No rule **" + Lfg.clean(typed) + "**" + game.map(g => " in " + Lookup.games(g)).getOrElse("") + ". Type a Law of Root number (4.3.2) or pick one of the suggestions.")
            case r :: _ =>
                val embed = if (r.game == "root") {
                    // a section shows the rules under it by number and title
                    val under = rules.filter(x => x.game == "root" && x.number.startsWith(r.number + ".") && x.number.count(_ == '.') == r.number.count(_ == '.') + 1)
                    val more = if (under.isEmpty) "" else "\n\n" + under.map(x => "**" + x.number + "** " + x.title).mkString("\n")
                    "{\"title\":" + Lfg.json(LfgSetup.cut(r.number + " " + r.title, 256)) + ",\"url\":" + Lfg.json(r.link) +
                    ",\"description\":" + Lfg.json(LfgSetup.cut(r.text + more, 4000)) +
                    ",\"footer\":{\"text\":" + Lfg.json("Law of Root · " + r.group + " · from the Root Database") + "}" +
                    ",\"color\":" + Lookup.colors("root") + "}"
                }
                else {
                    // the other sections on that page, so the reader knows where to look
                    val also = rules.filter(x => x.game == "nort" && x.path == r.path && x.id != r.id).map(_.title)
                    "{\"title\":" + Lfg.json(LfgSetup.cut(r.title, 256)) +
                    ",\"description\":" + Lfg.json("Northgard: Uncharted Lands, " + r.group + (if (also.isEmpty) "" else "\nAlso on this page: " + also.mkString(", "))) +
                    ",\"color\":" + Lookup.colors("nort") + ",\"image\":{\"url\":" + Lfg.json(Lookup.artUrl(r.path)) + "}}"
                }
                Lfg.reply(4, "{\"embeds\":[" + embed + "]" + (if (hidden) ",\"flags\":64" else "") + ",\"allowed_mentions\":{\"parse\":[]}}")
        }
    }
}

object Lookup {
    // the games the list has: its game field -> the name shown
    val games = scala.collection.immutable.ListMap("root" -> "Root", "nort" -> "Northgard")
    val colors = Map("root" -> 0xd9792b, "nort" -> 0x5b8fb9)

    // the same card in another deck's art (the Frankendeck's and the Dark Deck's ambushes ...) after the usual one
    def later(group : String) = if (group == "Frankendeck" || group == "Dark Deck") 1 else 0

    // lower case letters and digits only, accents dropped
    def norm(s : String) = java.text.Normalizer.normalize(s.toLowerCase, java.text.Normalizer.Form.NFD).filter(_.isLetterOrDigit)

    def artUrl(path : String) = Tournament.site + "/art/" + path.stripPrefix("webp2/")

    // /art/<path>: the file, if the list names it
    def artFile(path : String) : Option[Path] = {
        val p = "webp2/" + path
        if (path.contains("..") || !(CardLookup.served(p) || RuleLookup.served(p) || p.startsWith("webp2/lfg/webhooks/") || p.startsWith("webp2/lfg/gifs/"))) None
        else Some(Paths.get(Lfg.assets, p)).filter(Files.isRegularFile(_))
    }
}
