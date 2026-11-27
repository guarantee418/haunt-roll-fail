package cyclades
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

// Headless bot games on the JVM (see HANDOFF.md): `sbt "runMain cyclades.Host"`, CYCLADES_GAMES games of 2 to 5 Easy bots
// (default 20), each checking that every recorded action writes and parses back.
object Host extends hrf.host.BaseHost {
    val gaming = cyclades.gaming
    val path = "cyclades"

    type W = Faction

    override val limit = 30000

    // CYCLADES_RANDOM=0.3 makes that share of the choices at random, to reach every rule
    val random = sys.env.get("CYCLADES_RANDOM")./(_.toDouble).|(0.0)

    def askBot(g : G, f : F, actions : $[UserAction]) = (math.random() < random).?(new BotRandom(f) : EvalBot).|(new BotXX(f)).ask(actions, 0)(g)

    def factions = Faction.all
    def subjects = factions

    def batch = $(() => new G(factions.take(2 + scala.util.Random.nextInt(4)), $))

    def factionName(f : F) = f.name
    def nameWinner(f : F) = f.name

    def winners(a : Action)(implicit g : G) : $[Faction] = a match {
        case GameOverWonAction(_, f) => $(f)
        case _ => $()
    }

    def winnersFromFaction(f : F)(implicit g : G) = $(f)

    def serializer = cyclades.Serialize
    def start = StartAction(version)
    def times = sys.env.get("CYCLADES_GAMES")./(_.toInt).|(20)
}

// `sbt "runMain cyclades.Check"`: the layouts' data, each start's Fleet next to its isle, the prosperity and spaces as read
object Check {
    def main(args : Array[String]) {
        Layouts.all.foreach { l =>
            println("Layout " + l.name + ": " + l.cells.num + " cells, " + l.seas.num + " sea spaces, " + l.isles.num + " isles, " + l.commerce.num + " commerce, prosperity " + l.isles./(_.prosperity).sum)
            l.isles.foreach { d => assert(d.cells.forall(l.cells.has), "isle off the board " + d.isle) }
            l.commerce.foreach { c => assert(l.seas.has(Sea(c)), "commerce not a sea space " + c) }
            l.starts.foreach { s =>
                s.foreach { st =>
                    val i = Isle(st.isle)
                    val sea = Sea(Cell(st.fleet._1, st.fleet._2))
                    assert(l.islands.has(i), "no isle " + st.isle + " in " + l.name)
                    assert(l.seasAround(i).has(sea), "start fleet " + sea.name + " not by " + st.isle + " in " + l.name)
                }
                val rev = s./(st => l.data(Isle(st.isle)).prosperity).sum + s.count(st => l.commerce.has(Cell(st.fleet._1, st.fleet._2)))
                assert(rev == 2, "start revenue " + rev + " in " + l.name)
            }
        }
        println("OK")
    }
}
// `sbt "runMain cyclades.LogDump <players>"` plays one bot game and prints its log as text (JVM only)
object LogDump {
    def main(args : Array[String]) {
        val n = args.lift(0)./(_.toInt).|(3)
        val game = new Game(Faction.all.take(n), $)
        var continue : Continue = StartContinue
        var a : Action = StartAction(gaming.version)
        var i = 0
        while (i < 30000 && game.isOver.not) {
            i += 1
            val r = game.performContinue(|(continue), a, false)
            r.effects.foreach {
                case PrintLog(e) => println(e.text)
                case _ =>
            }
            def walk(c : Continue) : Unit = c match {
                case Log(m, _, n) => println(m.text) ; walk(n)
                case _ =>
            }
            walk(r.continue)
            continue = r.continue
            a = Host.askFaction(game, continue)
        }
        println("STEPS " + i)
    }
}
