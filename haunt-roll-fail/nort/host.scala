package nort
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
    val gaming = nort.gaming
    val path = "nort"

    type W = Faction

    def askBot(g : G, f : F, actions : $[UserAction]) = new BotXX(f).ask(actions, 0)(g)

    def factions = $(Bear, Boar, Goat, Raven, Snake, Stag, Wolf)
    def subjects = factions

    def batch = $(2, 3, 4, 5)./(n => () => new G(factions.shuffle.take(n), $))

    def factionName(f : F) = f.name
    def nameWinner(f : F) = f.name

    Debug.stats = true

    // NORT_UPGRADES=1: clan upgrades start in the decks, to test their effects
    Debug.upgradesInDeck = sys.env.get("NORT_UPGRADES").has("1")

    def winners(a : Action)(implicit g : G) = a @@ {
        case GameOverWonAction(_, f) => $(f)
    }

    def winnersFromFaction(f : F)(implicit g : G) = $(f)

    def serializer = nort.Serialize
    def start = StartAction(version)
    def times = 5
}
