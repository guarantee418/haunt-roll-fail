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

    override val limit = 60000

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

// `sbt "runMain tactics.Dump <steps>"` plays one bot game for that many steps and prints its recorded moves, one a line,
// as a browser's local game journal holds them (to load a game in mid-play and look at it)
object Dump {
    def main(args : Array[String]) {
        val steps = args.lift(0)./(_.toInt).|(1500)
        val game = new Game(Faction.all, $)
        var continue : Continue = StartContinue
        var a : Action = StartAction(gaming.version)
        var out = $[String]()
        var i = 0
        while (i < steps && game.isOver.not) {
            i += 1
            if (a.isSoft.not && a.is[ExternalAction])
                out :+= Serialize.write(a.unwrap)
            continue = game.performContinue(|(continue), a, false).continue
            a = Host.askFaction(game, continue)
        }
        println("ROUND " + game.round + " OVER " + game.isOver + " " + game.setup.map(f => f.name + " " + game.states(f).stock.toList.map(x => x._1.name + "=" + x._2).mkString(",") + " tier " + game.states(f).tier + " units " + game.units(f).map(_.utype.name).mkString("/")).mkString(" | "))
        println("CONTROL " + game.control.toList.map(x => x._1.name + ":" + x._2.short).mkString(" "))
        println("DUMP-BEGIN")
        out.foreach(println)
        println("DUMP-END")
    }
}
