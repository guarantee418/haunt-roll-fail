package tactics
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

// THE SITE'S ONLINE FEATURES FOR COMPANY OF HEROES (2026-11-27, asked for: "make pincer and coh features match root and northgard"):
// ranked or unranked games and the rankings (Meta.ranking, rankSides, rankSeat), why a seat is asked for its Discord turn ping
// (TurnReasons), and the Assault Specialist's jests (Jests, good-game/Heckler.scala). Display and reporting only: no game records change.

// RANKED OR UNRANKED: offered only online (startSetup in hrf.scala); an unranked game counts in the stats, not in the ratings
trait RankingOption extends RankingChoice with OneOfGroup {
    val group = "Ranking".txt
    override def fold = |("Ranking")
}

case object RankedGame extends RankingOption {
    val ranked = true
    def valueOn = "Ranked".hlb
    override val explain = $(
        "Counts toward the Elo ratings and the faction and turn order stats, when every seat is a person (a game with a bot never counts).",
    )
}

case object UnrankedGame extends RankingOption {
    val ranked = false
    def valueOn = "Unranked".hlb
    override val explain = $(
        "Doesn't count toward the Elo ratings or win rates; the faction and turn order stats still count it.",
    )
}

object RankingOption {
    val all : $[RankingOption] = $(RankedGame, UnrankedGame)
}

object Online {
    // a team is one side (Allies or Axis), else each player; the score is the side's Victory Points
    def rankSides(game : Game, winners : $[Any]) : $[($[Faction], Boolean)] = game.teams./(l => l -> l.exists(f => winners.contains(f)))

    def rankSeat(game : Game, f : Faction) : |[RankingSeat] =
        game.setup.has(f).?(RankingSeat(f.name, game.setup.indexOf(f) + 1, game.setup.num, |(game.teamVP(game.team(f)).toString)))

    def rankFan(game : Game) : Boolean = game.options.has(UnrankedGame)
}

// WHY A SEAT IS ASKED, for its Discord turn ping (good-game/TurnPings.scala, reasonOf), like Root's and Northgard's: "<tag>/<faction>",
// from the choices offered, the same on every client
object TurnReasons {
    def of(game : Game, f : Faction, actions : $[Any]) : String = {
        val names = actions./~{ case p : Product => |(p.productPrefix) ; case _ => None }
        def has(prefixes : String*) = names.exists(n => prefixes.exists(n.startsWith))

        val tag =
            if (has("StartPick", "StartPlace", "BrigadeStart")) "setup"
            else if (has("OpPick")) "ops"
            else if (has("BraceAction", "OberBrace", "BraceDone")) "brace"
            else if (has("TargetAction", "HoldFire", "Strafe", "NoStrafe", "SplashAction", "ShotAction")) "fire"
            else if (has("SmokeAction", "SmokeDone")) "smoke"
            else if (has("FinalRetreat", "RetreatDone")) "retreat"
            else if (has("Demolition")) "demolish"
            else if (has("Spawn", "FreeSpawn", "Build", "Upgrade", "Drop", "Breakthrough", "SovietIndustry", "CommanderTier", "Unlock")) "supply"
            else if (game.current == f) "turn"
            else "asked"

        tag + "/" + f.short
    }
}

// THE ASSAULT SPECIALIST'S JESTS (good-game/Heckler.scala): what happens to a player on anyone's turn, noted as it plays out and
// numbered in order, so every client numbers them alike; the clients send the last few with their turn reports (Meta.jests), the
// server takes each number once. "<tag>/<victim>/<other>/<what>":
//   destroyed     the victim's unit was destroyed (the other its killer, the what the unit)
//   neutralized   the other neutralized a Control Point of the victim's (the what the hex)
object Jests {
    def note(game : Game, tag : String, victim : Faction, other : Faction, what : String) {
        if (victim != other) {
            val e = tag + "/" + victim.short + "/" + other.short + "/" + what.replace('/', ' ')
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
