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

    // NORT_HARD=1: the first seat plays the Hard bot (BotHard), the others the Easy one; NORT_HARD=all: every seat Hard;
    // NORT_HARD=robotos: the first seat plays Robotos (BotRobotos, with its cheats), the others Hard
    def hard(g : G, f : F) = sys.env.get("NORT_HARD") match {
        case Some("all") | Some("robotos") => true
        case Some("1") => g.players.head == f
        case _ => false
    }

    // Adset games: the seat's clan once drafted
    def clan(g : G, p : F) : Faction = g.ptf.get(p).orNull

    def askBot(g : G, p : F, actions : $[UserAction]) = {
        val f = clan(g, p)
        val bot = (f == null || g.adset).?(new BotAdset(p, hard(g, p)) : Bot).|(g.robotos(f).?(new BotRobotos(f) : Bot).|(hard(g, p).?(new BotHard(f) : Bot).|(new BotXX(f))))
        val start = System.nanoTime
        val r = bot.ask(actions, 0)(g)
        val ms = (System.nanoTime - start) / 1000000
        // NORT_TTRACE=1: every choice of the bots, in short
        if (sys.env.get("NORT_TTRACE").has("1")) println((hard(g, p).?("H ").|("E ")) + p + " " + (f != null).??(g.states(f).fame.toString) + " " + r.immediate.unwrap.toString.take(150))
        // NORT_TIMING=1: the slowest decisions of the Hard bot
        if (sys.env.get("NORT_TIMING").has("1") && hard(g, p) && ms > 300)
            println("SLOW " + ms + "ms " + g.setup.num + "p year " + g.year + " " + actions.num + " actions, first " + actions.head.unwrap.toString.take(80))
        // NORT_TRACE=1: what the Hard bot chose, with its best alternatives
        if (sys.env.get("NORT_TRACE").has("1") && hard(g, p) && f != null && actions.num > 1) {
            val l = g.explode(actions, false, None).notOf[Hidden].%(_.isInstanceOf[Unavailable].not)
            val e = new HardEvaluation(f)(g)
            val scored = l./(a => a -> e.eval(a).head.weight).sortBy(-_._2)
            if (scored.exists(_._1.unwrap.isInstanceOf[PassAction]))
                println("   hand: " + g.states(f).hand./(c => c.name + "=" + e.cardValue(c).round).mkString(", ") + " pile " + g.pile.num + " explorable " + MapExpansion.explorable(f, false)(g).num + " sample " + g.pile.take(2)./(t => MapExpansion.placements(t, Some(MapExpansion.explorable(f, false)(g)), false)(g).num))
            println("Y" + g.year + " " + f + " food " + g.states(f).food + " wood " + g.states(f).wood + " lore " + g.states(f).lore + " fame " + g.states(f).fame + " units " + g.onMap(f) + " | " + scored.take(4)./{ case (a, w) => w + " " + a.unwrap.toString.take(110) }.mkString("\n      "))
        }
        r
    }

    // NORT_NEWBLOOD=1: only the New Blood clans
    def factions = sys.env.get("NORT_NEWBLOOD").has("1").?(NewBlood.clans).|($(Bear, Boar, Goat, Raven, Snake, Stag, Wolf) ++ NewBlood.clans)
    def subjects = factions

    // Random colors, game length and victory options; teams half the time with four or six players (NORT_TEAMS=1: always)
    // NORT_AUTOMA=1: solo games, one clan against the Automa
    def batch = sys.env.get("NORT_PLAYERS")./(_.toInt)./(n => $(n, n, n, n, n).take(sys.env.get("NORT_BATCH")./(_.toInt).|(5))).|($(2, 3, 4, 5, 6))./(n => () => {
        val solo = sys.env.get("NORT_AUTOMA").has("1")
        // NORT_TRAINING=1: Training Fields duels between two core clans
        val training = sys.env.get("NORT_TRAINING").has("1")
        // NORT_CORE=1: the core game only (seven years, no modules), with NORT_PLAYERS players if set
        val core = sys.env.get("NORT_CORE").has("1")
        val l = training.?($(Bear, Boar, Goat, Raven, Snake, Stag, Wolf).shuffle.take(2)).|(solo.?(factions.shuffle.take(1) :+ Automa).|(factions.shuffle.take(n)))
        val colors = l.zip(PlayerColor.all.shuffle)./{ case (f, c) => ColorOption(f, c) }
        // NORT_CREATURES=1: always with the Creatures module (and the More Creatures variant half the time); NORT_WARCHIEFS=1: always with Warchiefs; NORT_WILDERNESS=1: always with Wilderness
        val creatures = sys.env.get("NORT_CREATURES").has("1") || random() < 0.5
        val wastelands = sys.env.get("NORT_WASTELANDS").has("1") || random() < 0.5
        val teams = Module.teams.toList.%{ case (_, k) => k == n && solo.not && (sys.env.get("NORT_TEAMS").has("1") || random() < 0.5) }./{ case (m, _) => ModuleOption(m) }.shuffle.take(1)
        // NORT_VICTORY=1: always with Alternative victory, with random or chosen cards
        val alt = sys.env.get("NORT_VICTORY").has("1") || random() < 0.5
        val chosen = alt && random() < 0.5
        val usable = VictoryCard.all.%(c => creatures || VictoryExpansion.needsCreatures(c).not).shuffle
        val victory = alt.?(chosen.?(AltVictoryChosen : VictoryChoice).|(AltVictoryRandom)).|((random() < 0.3).?(FameOnly : VictoryChoice).|(StandardVictory)) +:
            ((alt && random() < 0.5).$(VictoryModeOption(true)) ++ chosen.??(usable.%(_.mapControl).take(1) ++ usable.%(_.mapControl.not).take(teams.any.?(3).|(2)))./(VictoryCardOption(_)))
        val options = colors ++ $(YearsOption.all.shuffle.head) ++ victory ++ (random() < 0.3).$(FirstSeatStarts) ++ (random() < 0.5).$(CombatReportAttackers) ++ (random() < 0.5).$(CombatReportDefenders) ++
            creatures.$(ModuleOption(Creatures)) ++ (creatures && random() < 0.5).$(MoreCreatures) ++
            (sys.env.get("NORT_WARCHIEFS").has("1") || random() < 0.5).$(ModuleOption(Warchiefs)) ++ (random() < 0.3).$(WarchiefCards) ++ (random() < 0.2).$(WarchiefPawns) ++ (random() < 0.3).$(NoDrawDevelopments) ++
            // NORT_HORIZONS=1: always with the Uncharted Horizons Development cards and map tiles
            (sys.env.get("NORT_HORIZONS").has("1") || random() < 0.5).$(HorizonsDevelopments) ++ (sys.env.get("NORT_HORIZONS").has("1") || random() < 0.5).$(HorizonsTiles) ++
            (sys.env.get("NORT_WILDERNESS").has("1") || random() < 0.5).$(ModuleOption(Wilderness)) ++
            // NORT_EVENTS=1: always with the Events module
            (sys.env.get("NORT_EVENTS").has("1") || random() < 0.5).$(ModuleOption(EventsModule)) ++
            // NORT_SEA=1: always with the Sea module
            (sys.env.get("NORT_SEA").has("1") || random() < 0.5).$(ModuleOption(Sea)) ++
            // NORT_WASTELANDS=1: always with Wastelands; a random central tile choice either way (it needs no module),
            // NORT_CENTRAL=1: never the standard tile; NORT_CENTRAL=<tile id>: always that tile
            wastelands.$(ModuleOption(Wastelands)) ++ $(CentralChoice.all.%(c => creatures || c.tile.forall(Waste.creatureOnly.has(_).not)).%(c => c != StandardCentral || sys.env.get("NORT_CENTRAL").has("1").not)
                .%(c => sys.env.get("NORT_CENTRAL").%(_ != "1").forall(c.tile.has)).shuffle.head) ++
            teams
        val level = solo.$(AutomaLevelOption(1 + (random() * 6).toInt))
        val all = training.?(colors ++ $(TrainingFieldsOption) ++ (random() < 0.3).$(FirstSeatStarts)).|(core.?(colors).|(options ++ level ++ level.exists(_.level >= 3).$(ModuleOption(Creatures))))
        all.foreach(o => assert(Meta.parseOption(Meta.writeOption(o)) == $(o), o))
        // NORT_ADSET=1: Adset games, with seats that draft their clans (random setup choices)
        if (sys.env.get("NORT_ADSET").has("1")) {
            val lands = (n > 2).?(AdsetLands.all).|(AdsetLands.all.but(LandsSeatChooses)).shuffle.head
            val adset = $(YearsOption.all.shuffle.head) ++ victory ++ (random() < 0.5).$(ModuleOption(Warchiefs)) ++ (random() < 0.5).$(ModuleOption(Sea)) :+ lands
            MetaAdset.createGame(MetaAdset.factions.take(n), adset.distinct)
        }
        else {
            // NORT_HARD=robotos: the first seat is Robotos
            val robotos = sys.env.get("NORT_HARD").has("robotos").$(RobotosOption(l.head))
            new G(l, (all ++ robotos).distinct)
        }
    })

    def factionName(f : F) = f match {
        case f : Faction => f.name
        case p : Seat => p.name
    }
    def nameWinner(f : W) = f.name

    Debug.stats = true

    // NORT_UPGRADES=1: clan upgrades start in the decks, to test their effects
    Debug.upgradesInDeck = sys.env.get("NORT_UPGRADES").has("1")

    def winners(a : Action)(implicit g : G) = a @@ {
        case GameOverWonAction(_, l, _) => l
    }

    def winnersFromFaction(f : F)(implicit g : G) = {
        // NORT_UNVALUED=1: after each game, the actions so far the Hard bot left to the Easy bot's valuation (by class)
        if (sys.env.get("NORT_UNVALUED").has("1"))
            println("UNVALUED " + HardEvaluation.unvalued.toList.sortBy(-_._2)./{ case (k, n) => k + " " + n }.mkString(", "))

        if (hard(g, f) && sys.env.get("NORT_HARD").has("1"))
            println("HARD WON " + factionName(f) + " in a " + g.setup.num + "-player game")
        val c = g.ptf.get(f).|(f.asInstanceOf[Faction])
        if (g.robotos(c))
            println("ROBOTOS WON " + c.name + " in a " + g.setup.num + "-player game")
        $(c)
    }

    def serializer = nort.Serialize
    def start = StartAction(version)
    def times = sys.env.get("NORT_TIMES")./(_.toInt).|(5)

    // Five- and six-player ten-year games with creatures take more than the default 4000 steps
    override val limit = 12000
}
