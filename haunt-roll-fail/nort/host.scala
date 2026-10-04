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

    // NORT_NEWBLOOD=1: only the New Blood clans
    def factions = sys.env.get("NORT_NEWBLOOD").has("1").?(NewBlood.clans).|($(Bear, Boar, Goat, Raven, Snake, Stag, Wolf) ++ NewBlood.clans)
    def subjects = factions

    // Random colors, game length and victory options; teams half the time with four or six players (NORT_TEAMS=1: always)
    // NORT_AUTOMA=1: solo games, one clan against the Automa
    def batch = $(2, 3, 4, 5, 6)./(n => () => {
        val solo = sys.env.get("NORT_AUTOMA").has("1")
        val l = solo.?(factions.shuffle.take(1) :+ Automa).|(factions.shuffle.take(n))
        val colors = l.zip(PlayerColor.all.shuffle)./{ case (f, c) => ColorOption(f, c) }
        // NORT_CREATURES=1: always with the Creatures module (and the More Creatures variant half the time); NORT_WARCHIEFS=1: always with Warchiefs; NORT_WILDERNESS=1: always with Wilderness
        val creatures = sys.env.get("NORT_CREATURES").has("1") || random() < 0.5
        val teams = Module.teams.toList.%{ case (_, k) => k == n && solo.not && (sys.env.get("NORT_TEAMS").has("1") || random() < 0.5) }./{ case (m, _) => ModuleOption(m) }
        // NORT_VICTORY=1: always with Alternative victory, with random or chosen cards
        val alt = sys.env.get("NORT_VICTORY").has("1") || random() < 0.5
        val chosen = alt && random() < 0.5
        val usable = VictoryCard.all.%(c => creatures || VictoryExpansion.needsCreatures(c).not).shuffle
        val victory = alt.?(chosen.?(AltVictoryChosen : VictoryChoice).|(AltVictoryRandom)).|((random() < 0.3).?(FameOnly : VictoryChoice).|(StandardVictory)) +:
            ((alt && random() < 0.5).$(VictoryModeOption(true)) ++ chosen.??(usable.%(_.mapControl).take(1) ++ usable.%(_.mapControl.not).take(teams.any.?(3).|(2)))./(VictoryCardOption(_)))
        val options = colors ++ $(YearsOption.all.shuffle.head) ++ victory ++ (random() < 0.3).$(FirstSeatStarts) ++
            creatures.$(ModuleOption(Creatures)) ++ (creatures && random() < 0.5).$(MoreCreatures) ++
            (sys.env.get("NORT_WARCHIEFS").has("1") || random() < 0.5).$(ModuleOption(Warchiefs)) ++ (random() < 0.3).$(WarchiefCards) ++
            (sys.env.get("NORT_WILDERNESS").has("1") || random() < 0.5).$(ModuleOption(Wilderness)) ++
            // NORT_EVENTS=1: always with the Events module
            (sys.env.get("NORT_EVENTS").has("1") || random() < 0.5).$(ModuleOption(EventsModule)) ++
            teams
        val level = solo.$(AutomaLevelOption(1 + (random() * 6).toInt))
        val all = options ++ level ++ level.exists(_.level >= 3).$(ModuleOption(Creatures))
        all.foreach(o => assert(Meta.parseOption(Meta.writeOption(o)) == $(o), o))
        new G(l, all.distinct)
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

    // Five- and six-player ten-year games with creatures take more than the default 4000 steps
    override val limit = 12000
}
