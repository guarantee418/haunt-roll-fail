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

    // PINCER_TUTORIAL=1 plays the Tutorial's practice game (its seating and options) and prints each kind of choice Red was offered
    // with a tip ("TIP") or without ("NO TIP")
    val tutorial = sys.env.get("PINCER_TUTORIAL") == Some("1")
    var tipsSeen = Set[String]()

    def askBot(g : G, f : F, actions : $[UserAction]) = {
        if (tutorial && f == Tutorial.side && actions.any) {
            val kinds = actions./(_.unwrap.getClass.getSimpleName).distinct.sorted.mkString(",")
            val key = Tutorial.tip(g, f, actions).any.?("TIP ").|("NO TIP ") + kinds
            if (tipsSeen.contains(key).not) { tipsSeen += key ; println(key) }
        }
        new BotXX(f).ask(actions, 0)(g)
    }

    def factions = Faction.all
    def subjects = factions

    def batch = $(() => tutorial.?(new G(Tutorial.seating, Tutorial.options)).|(new G(factions.take(2 + scala.util.Random.nextInt(3)).shuffle, $)))

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
        def milestone(c : Continue) : Boolean = c match {
            case _ : Milestone => true
            case Log(_, _, c) => milestone(c)
            case _ => false
        }
        var i = 0
        while (i < steps && game.isOver.not) {
            i += 1
            // the browser records the moves, and a Milestone's action too
            if ((a.isSoft.not && a.is[ExternalAction]) || milestone(continue))
                out :+= Serialize.write(a.unwrap)
            continue = game.performContinue(|(continue), a, false).continue
            a = Host.askFaction(game, continue)
        }
        println("DUMP-BEGIN")
        out.foreach(println)
        println("DUMP-END")
    }
}
