package pincer
//
//
//
//
import hrf.colmat._
//
//
//
//

import hrf.meta._
import hrf.options._
import hrf.elem._

// THE SITE'S ONLINE FEATURES FOR PINCER (2026-11-27, asked for: "make pincer and coh features match root and northgard"): ranked or
// unranked games and the rankings (Meta.ranking, rankSides, rankSeat), why a seat is asked for its Discord turn ping (TurnReasons), and
// the Pamphleteer's jests (Jests, good-game/Heckler.scala). Display and reporting only: no game records change.

// RANKED OR UNRANKED: offered only online (startSetup in hrf.scala); an unranked game counts in the stats, not in the ratings
trait RankingOption extends RankingChoice with OneOfGroup {
    val group = "Ranking".txt
    override def fold = |("Ranking")
}

case object RankedGame extends RankingOption {
    val ranked = true
    def valueOn = "Ranked".hlb
    override val explain = $(
        "Counts toward the Elo ratings and the color and turn order stats, when every seat is a person (a game with a bot never counts).",
    )
}

case object UnrankedGame extends RankingOption {
    val ranked = false
    def valueOn = "Unranked".hlb
    override val explain = $(
        "Doesn't count toward the Elo ratings or win rates; the color and turn order stats still count it.",
    )
}

object RankingOption {
    val all : $[RankingOption] = $(RankedGame, UnrankedGame)
}

object Online {
    // every player is a side of their own; the score is the Royal Seals held
    def rankSides(game : Game, winners : $[Any]) : $[($[Faction], Boolean)] = game.setup./(f => $(f) -> winners.contains(f))

    def rankSeat(game : Game, f : Faction) : |[RankingSeat] =
        game.setup.has(f).?(RankingSeat(f.name, game.setup.indexOf(f) + 1, game.setup.num, |(game.sealsOf(f).num.toString)))

    def rankFan(game : Game) : Boolean = game.options.has(UnrankedGame)
}

// WHY A SEAT IS ASKED, for its Discord turn ping (good-game/TurnPings.scala, reasonOf), like Root's and Northgard's: "<tag>/<color>", and
// "/<other color>" for an attack, a counter-action, a peace offer, a surrender or the Founder's Oath; from the choices offered, the same on
// every client
object TurnReasons {
    def of(game : Game, f : Faction, actions : $[Any]) : String = {
        val names = actions./~{ case p : Product => |(p.productPrefix) ; case _ => None }
        def has(prefixes : String*) = names.exists(n => prefixes.exists(n.startsWith))
        // the other player the choices name: an attacker, the one offering peace (a counter-action's through its Counter)
        val others = actions./~{
            case p : Product => p.productIterator.$./~{
                case e : Faction => $(e)
                case c : Counter => $(c.active)
                case _ => $
            }
            case _ => $
        }.distinct.but(f)

        val tag =
            if (has("PlaceStart")) "setup"
            else if (has("RetreatAction", "FortifyAction", "TakeHit")) "defend"
            else if (has("CounterUnit", "AmbushMenu", "AmbushUnit", "CounterPass")) "counter"
            else if (has("AcceptPeace", "DeclinePeace")) "peace"
            else if (has("KeepColony")) "surrender"
            else if (has("PlacePrisoner", "SupplyPrisoner")) "prisoner"
            else if (has("FrigateRevive")) "revive"
            else if (has("FoundersChoose")) "founders"
            else if (has("HarvestChoose")) "harvest"
            else if (game.current == f) "turn"
            else "asked"

        val versus = $("defend", "counter", "peace", "surrender", "founders").has(tag).??(others.single./("/" + _.short).|(""))
        tag + "/" + f.short + versus
    }
}

// THE PAMPHLETEER'S JESTS (good-game/Heckler.scala): what happens to a player on anyone's turn, noted as it plays out and numbered in
// order, so every client numbers them alike; the clients send the last few with their turn reports (Meta.jests), the server takes
// each number once. "<tag>/<victim>/<other>/<what>":
//   captured   the victim's frigate was captured (the other the captor; the land units are captured too often to mock each)
//   seal       the victim lost a Royal Seal to the other (the what the seal)
//   war        the other declared war on the victim
//   colony     the other took a colony of the victim's in a surrender (the what the hex)
object Jests {
    def note(game : Game, tag : String, victim : Faction, other : Faction, what : String) {
        if (victim != other) {
            val e = tag + "/" + victim.short + "/" + other.short + "/" + what.replace('/', ' ')
            // (a Royal Seal passed back and forth within one action is noted once)
            if (game.jests.lastOption.has(e).not)
                game.jests :+= e
        }
    }

    def report(game : Game, seat : Faction => String) : $[String] = {
        val all = game.jests
        all.zipWithIndex.drop(all.num - 12)./~ { case (e, i) =>
            e.split('/').toList match {
                case tag :: v :: rest => game.setup.find(_.short == v)./(f => seat(f)).%(_.nonEmpty)./(s => (i + 1) + ":" + (tag :: s :: rest).mkString("/"))
                case _ => None
            }
        }
    }
}
