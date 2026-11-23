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
// (default 20), each checking that every recorded action writes and parses back.
object Host extends hrf.host.BaseHost {
    val gaming = tactics.gaming
    val path = "tactics"

    type W = Faction

    override val limit = 20000

    def askBot(g : G, f : F, actions : $[UserAction]) = new BotXX(f).ask(actions, 0)(g)

    def factions = Faction.all
    def subjects = factions

    def batch = $(() => new G(factions.shuffle, $))

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
