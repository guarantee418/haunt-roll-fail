package hrf
//
// THE ART INDEX for the Discord bot's /card command (good-game/Lookup.scala; asked for on 2026-11-22): every Root and Northgard card,
// faction or clan board and the like, by its name in the game, with its image file. Run it in the JVM host project (see
// nort/HANDOFF.md) after adding or renaming cards:
//   sbt "runMain hrf.LookupIndex <haunt-roll-fail directory>"
// It writes webp2/lfg/cards.tsv: game, kind, name, what it belongs to (a deck, a clan, ...), image path from the client's directory.
// Images are taken from the games' asset lists (Meta.assets), so they are the ones the site shows; a name with no image file is left out.
import hrf.colmat._
import hrf.meta._

import java.nio.file.{Files, Paths}

object LookupIndex {
    case class Entry(game : String, kind : String, name : String, group : String, path : String)

    def main(args : Array[String]) {
        val dir = args.headOption.getOrElse(".")

        // an asset's name -> its file, as the client loads it
        def sources(m : MetaGame) : Map[String, String] = m.assets./~(_.get)./(a => a.name -> ("webp2/" + m.path + "/images/" + a.copy(ext = "webp").src)).toMap

        val out = new scala.collection.mutable.ListBuffer[Entry]

        def add(game : String, kind : String, name : String, group : String, path : |[String]) =
            path.%(p => Files.exists(Paths.get(dir, p))).foreach { p =>
                if (out.exists(e => e.game == game && e.path == p).not)
                    out += Entry(game, kind, name.trim, group, p)
            }

        // ROOT
        {
            val src = sources(root.Meta)
            def deckOf(path : String) = path.split('/').reverse.drop(1).head match {
                case "deck" => "Might and Right"
                case "dusk" => "Dawn and Dusk"
                case "capitalists" => "Capitalists and Anarchists"
                case "franken" => "Frankendeck"
                case "dark" => "Dark Deck"
                case "action" => "Action! Deck Booster"
                case "upstarts" => "Upstarts & Renegades"
                case "sorcery" => "Sorcery of the Enchanted Woods"
                case "invasive" | "card" => "Lilypad Diaspora"
                case x => x
            }
            def card(c : root.DeckCard) = {
                val id = root.Deck.art(c.id)
                src.get(id).orElse(src.get(id.toLowerCase))
            }
            // the decks' cards in their usual art, then the Frankendeck's and the Dark Deck's art for the shared ones
            $((false, false), (true, false), (false, true)).foreach { case (franken, dark) =>
                root.Deck.frankenArt = franken
                root.Deck.darkArt = dark
                root.Deck.catalog.foreach {
                    case c : root.DeckCard =>
                        val name = c match {
                            case root.Ambush(s) => "Ambush (" + s.name + ")"
                            case _ => c.name
                        }
                        card(c).foreach(p => add("root", "card", name, deckOf(p), Some(p)))
                    case _ =>
                }
            }
            root.Deck.frankenArt = false
            root.Deck.darkArt = false

            root.Quest.all.foreach(q => add("root", "quest", q.name + " (" + q.suit.name + ")", "Vagabond quest", src.get("quest:" + q.id)))
            $("ferry" -> "Ferry", "lost-city" -> "Lost City", "tower" -> "Tower", "mousehold" -> "Mousehold", "foxburrow" -> "Foxburrow", "rabbittown" -> "Rabbittown").foreach { case (id, n) =>
                add("root", "landmark", n, "Landmark", src.get("landmark-card:" + id))
            }
            (root.Meta.hirelings ++ root.Meta.turnlings).foreach(h => add("root", "hireling", h.name, "Hireling", src.get("hireling-card:" + h.style + "-card")))
            (root.Meta.factions ++ root.Meta.hirelings).distinct.foreach { f =>
                val board = $("-board-h", "-board", "-board-v3", "-board-v2", "-board-v1")./(x => src.get(f.style.toLowerCase + x)).flatten.headOption
                add("root", "faction", f.name, "Faction board", board)
            }
        }

        // NORTHGARD
        {
            import nort._
            val src = sources(nort.Meta)
            def info(kind : String, i : CardInfo, group : String) = add("nort", kind, i.name, group, src.get(i.image))

            Cards.start.values.$.foreach(i => info("card", i.copy(image = i.image.replace("card-start-", "card-start-blue-")), "Starting card"))
            Cards.start.values.$.foreach(i => info("card", i, "Starting card"))
            Cards.clan.foreach { case (f, l) => l.foreach(i => info("card", i, f.name + " Clan")) }
            CardVersions.all.foreach(v => info("card", v.info, v.edition + (v.card match { case ClanCard(f, _) => ", " + f.name + " Clan" ; case _ => "" })))
            Cards.developments.values.$.sortBy(_.name).foreach(i => info("card", i, "Development"))
            Cards.achievements.values.$.sortBy(_.name).foreach(i => info("card", i, "Achievement"))
            info("card", UnrestCard.info, "Unrest")
            (Creature.all ++ Creature.expansion ++ Creature.waste :+ Creature.hrimgandr).foreach(c => add("nort", "creature", c.info.name, "Creature, card " + c.n, src.get("card-creature-" + c.token.drop("creature-".length))))
            info("creature", BlainnCard.info, "Creature")
            EventCard.all.foreach(c => info("card", c.info, "Event"))
            VictoryCard.all.foreach(c => info("card", c.info, "Alternative victory"))
            RaidCard.all.foreach(c => info("card", c.info, "Raid"))
            nort.Meta.clans.foreach(f => add("nort", "board", f.name + " Clan", "Clan board", src.get("board-" + f.style)))
            Warchief.printedBoards.foreach(f => add("nort", "board", f.name + " Clan", "Clan board, printed", src.get("board-" + f.style + "-print")))
        }

        val file = Paths.get(dir, "webp2", "lfg", "cards.tsv")
        Files.createDirectories(file.getParent)
        val lines = out.toList.sortBy(e => (e.game, e.name.toLowerCase, e.group))./(e => $(e.game, e.kind, e.name, e.group, e.path)./(_.replace('\t', ' ').replace('\n', ' ')).mkString("\t"))
        Files.write(file, ("game\tkind\tname\tgroup\tpath\n" + lines.mkString("\n") + "\n").getBytes("UTF-8"))
        println("LookupIndex: " + out.size + " entries, " + out.groupBy(e => e.game + " " + e.kind).view.mapValues(_.size).toList.sorted.mkString(", "))
    }
}
