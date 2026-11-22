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
    val all : List[BotCommand] = List(CardLookup)

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

    // the image files the list names (and so /art serves)
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
        if (path.contains("..") || !CardLookup.served(p)) None
        else Some(Paths.get(Lfg.assets, p)).filter(Files.isRegularFile(_))
    }
}
