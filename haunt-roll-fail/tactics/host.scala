package tactics
//
//
//
//
import hrf.colmat._
import hrf.logger._
//
//
//
//

// Headless bot games on the JVM (see HANDOFF.md): `sbt "runMain tactics.Host"`, TACTICS_GAMES games of two Easy bots
// (default 20), each checking that every recorded action writes and parses back. TACTICS_RULES=first plays the First Game Rules,
// anything else the Full Game Rules (here, in Dump and in Replay), with Commanders unless TACTICS_COMMANDERS=0; TACTICS_MAP=<id> plays that mission's map (moro, don, rivers, ardennes, frozen, moscow, moroffa); TACTICS_FACTIONS=BR,OKW (US, WM, BR, SU, OKW) the factions; TACTICS_EXPANSION=1 adds the expansion Commanders, TACTICS_BRIGADES=1 the Lost Brigades, TACTICS_OPTIONAL=1 the optional Modular Bunkers.
object Rules {
    // TACTICS_FACTIONS=BR,OKW plays those two (the first goes first in the seating); US and Wehrmacht by default
    def factions : $[Faction] = sys.env.get("TACTICS_FACTIONS")./(_.split(",").toList./(x => Meta.parseFaction(x).get)).|($(US, Wehrmacht))

    def options : $[Meta.O] = (sys.env.get("TACTICS_RULES") == Some("first")).?($[Meta.O]()).|($[Meta.O](FullGameRules) ++ (sys.env.get("TACTICS_COMMANDERS") != Some("0")).$(DrawCommanders)) ++ MapChoice.all.%(_.map.id == sys.env.getOrElse("TACTICS_MAP", "")) ++ (sys.env.get("TACTICS_EXPANSION") == Some("1")).$(ExpansionCommanders) ++ (sys.env.get("TACTICS_BRIGADES") == Some("1")).$(LostBrigades) ++ (sys.env.get("TACTICS_OPTIONAL") == Some("1")).$(OptionalMapFeatures) ++ $(TeamRetreatPoints)
}

object Host extends hrf.host.BaseHost {
    val gaming = tactics.gaming
    val path = "tactics"

    type W = Faction

    override val limit = 60000

    def askBot(g : G, f : F, actions : $[UserAction]) = new BotXX(f).ask(actions, 0)(g)

    def factions = Rules.factions
    def subjects = factions

    def batch = $(() => new G(factions.shuffle, Rules.options))

    def factionName(f : F) = f.name
    def nameWinner(f : F) = f.name

    def winners(a : Action)(implicit g : G) : $[Faction] = a match {
        case GameOverWonAction(_, f) => $(f)
        case _ => $()
    }

    def winnersFromFaction(f : F)(implicit g : G) = $(f)

    def serializer = tactics.Serialize
    def start = StartAction(version)
    def times = sys.env.get("TACTICS_GAMES")./(_.toInt).|(20)
}

// a game's state in two lines (Dump prints it after the moves' game, Replay after replaying them, to compare)
object State {
    def print(game : Game) {
        println("ROUND " + game.round + " OVER " + game.isOver + " " + game.setup.map(f => f.name + " " + game.states(f).stock.toList.sortBy(_._1.name).map(x => x._1.name + "=" + x._2).mkString(",") + " tier " + game.states(f).tier + " units " + game.units(f).map(u => u.utype.name + "@" + game.where(u).name + ":" + game.health(u)).mkString("/")).mkString(" | "))
        println("CONTROL " + game.control.toList.sortBy(_._1.name).map(x => x._1.name + ":" + x._2.short).mkString(" "))
    }
}

// `sbt "runMain tactics.Dump <steps> [round]"` plays one bot game for that many steps (or, given a round, until damage is being
// assigned in that round) and prints its recorded moves, one a line, as a browser's local game journal holds them (to load a game
// in mid-play and look at it)
object Dump {
    def main(args : Array[String]) {
        val steps = args.lift(0)./(_.toInt).|(1500)
        val game = new Game(Rules.factions, Rules.options)
        var continue : Continue = StartContinue
        var a : Action = StartAction(gaming.version)
        var out = $[String]()
        val until = args.lift(1)./(_.toInt)
        def milestone(c : Continue) : Boolean = c match {
            case _ : Milestone => true
            case Log(_, _, c) => milestone(c)
            case _ => false
        }
        var i = 0
        while (i < steps && game.isOver.not && until.forall(r => (game.round >= r && game.phase == "Damage" && game.assignments.any).not)) {
            i += 1
            // the browser records the moves, and a Milestone's action too
            if ((a.isSoft.not && a.is[ExternalAction]) || milestone(continue))
                out :+= Serialize.write(a.unwrap)
            continue = game.performContinue(|(continue), a, false).continue
            a = Host.askFaction(game, continue)
        }
        State.print(game)
        println("DUMP-BEGIN")
        out.foreach(println)
        println("DUMP-END")
    }
}

// `sbt "runMain tactics.Replay <file>"` replays recorded moves (one a line, as `Dump` prints them) as a browser loading the game
// does: the forced steps go on by themselves, and each recorded move is validated against what the game expects at that point
object Replay {
    def main(args : Array[String]) {
        val moves = scala.io.Source.fromFile(args(0)).getLines().toList.filter(_.trim.nonEmpty)./(Serialize.parseAction)
        val game = new Game(Rules.factions, Rules.options)

        def auto(c : Continue) : |[Action] = c match {
            case Force(a) => |(a)
            case Then(a) => |(a)
            case Log(_, _, c) => auto(c)
            case DelayedContinue(_, c) => auto(c)
            case _ => None
        }

        var continue : Continue = game.performContinue(None, moves.head, false).continue
        var rest = moves.drop(1)
        var n = 1
        while (rest.any) {
            auto(continue) match {
                case Some(a) =>
                    continue = game.performContinue(|(continue), a, false).continue
                case None =>
                    val a = rest.head
                    if (game.validate(continue, a).not) {
                        println("MOVE " + n + " NOT VALID: " + Serialize.write(a))
                        println("ON: " + continue)
                        sys.exit(1)
                    }
                    continue = game.performContinue(|(continue), a, false).continue
                    rest = rest.drop(1)
                    n += 1
            }
        }
        println("ALL " + moves.num + " MOVES VALID, round " + game.round + ", " + game.phase)
        State.print(game)
    }
}
