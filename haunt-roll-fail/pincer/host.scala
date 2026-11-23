package pincer
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

// Headless bot games on the JVM (see HANDOFF.md): `sbt "runMain pincer.Host"`, PINCER_GAMES games of 2 to 4 Easy bots
// (default 20), each checking that every recorded action writes and parses back.
object Host extends hrf.host.BaseHost {
    val gaming = pincer.gaming
    val path = "pincer"

    type W = Faction

    override val limit = 60000

    def askBot(g : G, f : F, actions : $[UserAction]) = new BotXX(f).ask(actions, 0)(g)

    def factions = Faction.all
    def subjects = factions

    def batch = $(() => new G(factions.take(2 + scala.util.Random.nextInt(3)).shuffle, $))

    def factionName(f : F) = f.name
    def nameWinner(f : F) = f.name

    def winners(a : Action)(implicit g : G) : $[Faction] = a match {
        case GameOverWonAction(_, f) => $(f)
        case _ => $()
    }

    def winnersFromFaction(f : F)(implicit g : G) = $(f)

    def serializer = pincer.Serialize
    def start = StartAction(version)
    def times = sys.env.get("PINCER_GAMES")./(_.toInt).|(20)
}

// `sbt "runMain pincer.Dump <steps> <players>"` plays one bot game for that many steps and prints its recorded moves,
// one a line, as a browser's local game journal holds them (to load a game in mid-play and look at it)
object Dump {
    def main(args : Array[String]) {
        val steps = args.lift(0)./(_.toInt).|(1500)
        val n = args.lift(1)./(_.toInt).|(2)
        val game = new Game(Faction.all.take(n), $)
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
        println("DUMP-BEGIN")
        out.foreach(println)
        println("DUMP-END")
    }
}
