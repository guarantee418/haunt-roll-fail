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

    // Random colors, game length and victory options
    def batch = $(2, 3, 4, 5)./(n => () => {
        val l = factions.shuffle.take(n)
        val colors = l.zip(PlayerColor.all.shuffle)./{ case (f, c) => ColorOption(f, c) }
        // NORT_CREATURES=1: always with the Creatures module (and the More Creatures variant half the time); NORT_WARCHIEFS=1: always with Warchiefs
        val creatures = sys.env.get("NORT_CREATURES").has("1") || random() < 0.5
        val options = colors ++ $(YearsOption.all.shuffle.head) ++ (random() < 0.3).$(FameOnly) ++ (random() < 0.3).$(FirstSeatStarts) ++
            creatures.$(ModuleOption(Creatures)) ++ (creatures && random() < 0.5).$(MoreCreatures) ++
            (sys.env.get("NORT_WARCHIEFS").has("1") || random() < 0.5).$(ModuleOption(Warchiefs))
        options.foreach(o => assert(Meta.parseOption(Meta.writeOption(o)) == $(o), o))
        new G(l, options)
    })

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

    // Five-player ten-year games with creatures take more than the default 4000 steps
    override val limit = 12000
}
