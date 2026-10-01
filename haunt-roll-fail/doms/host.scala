package doms
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

import scala.collection.parallel.CollectionConverters._

object Host extends hrf.host.BaseHost {
    val gaming = doms.gaming
    val path = "doms"

    type W = Faction

    def askBot(g : G, f : F, actions: $[UserAction]) = new BotXX(f).ask(actions, 0)(g)

    def factions = $(Mammal, Reptile, Bird, Amphibian, Arachnid, Insect)
    def subjects = factions

    def batch = {
        val allComb = factions.combinations(4).$
        val repeat = 0.to(15).map(_ => factions).$

        def allSeatings(factions : $[Faction]) = factions.permutations.$
        def randomSeating(factions : $[Faction]) = allSeatings(factions).shuffle.head

        val base = 16.times(factions)

        base./(l => () => new G(l, $))
    }

    def factionName(f : F): String = f.name
    def nameWinner(f : F) = f.name

    def winners(a : Action)(implicit g : G) = a @@ {
        case GameOverWonAction(_, f) => $(f)
    }

    def winnersFromFaction(f : F)(implicit g : G) = $(f)

    override val limit : Int = 80000

    def serializer = doms.Serialize
    def start = StartAction(version)
    def times = 10
    def winners(a : Action) = a @@ {
        case GameOverWonAction(_, f) => $(f)
    }
}
