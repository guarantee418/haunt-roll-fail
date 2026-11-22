package nort
//
//
//
//
import hrf.colmat._
//
//
//
//

// WHY A SEAT IS ASKED, for its Discord turn ping (good-game/TurnPings.scala; asked for on 2026-11-21, like Root's,
// root/turn-reasons.scala): a fight ("fight" for the clan whose turn it is, "defend" for the others), harvest trading, Winter,
// a card pick, setup, the Adset draft, a Sea raid, an Event, or the clan's own turn; "/<clan>" names the clan whose emblem the ping shows. Worked out from
// the action that asked (game.asker) and the choices offered, the same on every client. Display only: no game records change.
object TurnReasons {
    def tagOf(game : Game, f : |[Faction], actions : $[Any]) : String = {
        val name = game.asker @@ {
            case p : Product => p.productPrefix
            case _ => ""
        }
        val head = f./("/" + _.short).|("")
        val mine = f.exists(game.highlight.current.has)

        val tag =
            if (actions.exists(a => a.isInstanceOf[AdsetBanAction] || a.isInstanceOf[AdsetPickAction]) || name.startsWith("Adset")) "draft"
            else if (actions.exists(_.isInstanceOf[PickCardAction])) "cards"
            else if (name.startsWith("Trade") || name.startsWith("TeamOffer") || name.startsWith("TeamAccept") || name.startsWith("TeamDecline")) "trade"
            else if (name.startsWith("Winter") || name.contains("Unrest")) "winter"
            else if (Seq("Combat", "Fight", "Casualt", "Retreat", "Reroll").exists(name.contains)) mine.?("fight").|("defend")
            else if (name.startsWith("Setup")) "setup"
            else if (name.startsWith("Raid")) "raid"
            else if (name.startsWith("Event")) "event"
            else if (mine) "turn"
            else "asked"

        // the other clan in a fight (2026-11-22, "who is x who", for NGUL too): the attacker or defender the asking action names, else
        // the clan whose turn it is for the defender, and the other clan with figures in the fought territory for the attacker
        val versus : String = if (tag != "fight" && tag != "defend") "" else f.flatMap { self =>
            val named : List[Faction] = game.asker match {
                case p : Product => p.productElementNames.zip(p.productIterator).collect { case ("attacker" | "defender", x : Faction) if x != self => x }.toList.distinct
                case _ => Nil
            }
            // (only clans with figures there: a creature's fight names no clan)
            val there : List[Faction] = game.battle.toList.flatMap(a => game.present(game.board.territory(a))).filter(_ != self).distinct
            val guess : Option[Faction] =
                if (tag == "defend") game.highlight.current.filter(there.contains)
                else there match { case List(o) => Some(o) ; case _ => None }
            (named match { case List(o) => Some(o) ; case _ => guess }).filter(_ != self)
        }.map("/" + _.short).getOrElse("")

        (tag == "draft").?(tag).|(tag + head + (if (head.nonEmpty) versus else ""))
    }

    def of(game : Game, p : Any, actions : $[Any] = $) : String = p @@ {
        case f : Faction => tagOf(game, |(f), actions)
        case p : Player => tagOf(game, game.ptf.get(p), actions)
        case _ => ""
    }
}
