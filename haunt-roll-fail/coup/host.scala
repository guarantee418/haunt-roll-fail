package coup
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

object Host extends hrf.host.BaseHost {
    val gaming = coup.gaming
    val path = "coup"

    type W = Faction

    def botFor(f : F) = {
        val DL = new BotDL(f, 0.5, 0.5, 0.5)
        val Honest = new BotDL(f, 0.5, 1.0, 0.0)
        val HRF = new BotXX(f)

        f match {
            case Amalthea => Honest
            // case Thebe => Honest
            // case Io => Honest
            // case Europa => Honest
            // case Ganymede => Honest
            // case Amalthea => DL
            // case Thebe => DL
            // case Io => DL
            // case Europa => DL
            // case Ganymede => DL
            case Callisto => DL
            // case Europa => HRF
            // case Ganymede => HRF
            // case Callisto => HRF
            case _ => null
        }
    }

    def askBot(g : G, f : F, actions : $[UserAction]) = botFor(f).ask(actions, 0)(g)

    def factions = $(Amalthea, Thebe, Io, Europa, Ganymede, Callisto)
    def subjects = factions

    def batch = factions.%(botFor(_) != null).permutations.$./(l => () => new G(l))

    def factionName(f : F) = f.name
    def nameWinner(f : F) = f.name

    def winners(a : Action)(implicit g : G) = a @@ {
        case GameOverWonAction(_, f) => $(f)
    }

    def winnersFromFaction(f : F)(implicit g : G) = $(f)

    def serializer = coup.Serialize
    def start = StartAction(version)
    def times = 1000 / batch.num
    def winners(a : Action) = a @@ {
        case GameOverAction(f, _) => $(f)
    }
}
