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

import hrf.meta._
import hrf.options._
import hrf.elem._

import nort.elem._


case class UnknownOption(o : String) extends GameOption {
    val group = "Unknown"
    val valueOn = "Unknown Option " ~ o
}


object Meta extends MetaGame { mmm =>
    val gaming = nort.gaming

    type F = Faction

    def tagF = implicitly

    val name = "nort"
    val label = "Northgard: Uncharted Lands"

    // The core clans, then New Blood's
    val factions = $(Bear, Boar, Goat, Raven, Snake, Stag, Wolf) ++ NewBlood.clans :+ Automa

    // The clans with cards and boards (not the Automa)
    val clans = factions.but(Automa)

    // The Automa plays by its cards, never by a person
    override def botOnly(f : Faction) = f == Automa

    // The main menu's "Solo vs Automa"
    override def soloFaction = |(Automa)

    // The main menu's "Training Grounds": Uncharted Horizons' Training Fields duel, local or online, between two core clans
    // (the clans only name the players: they have no powers there)
    override def modes = $(("Training Grounds", "training"))

    override def modeAbout(mode : String) =
        "Uncharted Horizons' Training Fields: a quick duel for two players on a small map of twelve tiles, most of them face down.".txt ~ Break ~
        "Each turn, play one of your seven Action cards. Control resources and buildings: the first to 5 victory points wins.".txt

    override def modeFactions(mode : String) = $(Bear, Boar, Goat, Raven, Snake, Stag, Wolf).shuffle.take(2)

    override def modeOptions(mode : String) = $(TrainingFieldsOption)

    val minPlayers = 2
    // Six players build on the five-player rules
    override val maxPlayers = 6

    // Games made before the Victory conditions options turned the Alternative victory module on with its module option
    // Training Fields is turned on by the main menu's Training Grounds only
    override val hiddenOptions = $(ModuleOption(VictoryModule), TrainingFieldsOption)

    // New Blood has no option: picking one of its clans brings it in
    val options : $[O] = ColorOption.all ++ YearsOption.all ++ $(StandardVictory, FameOnly, AltVictoryRandom, AltVictoryChosen, VictoryModeOption(false), VictoryModeOption(true)) ++ VictoryCardOption.all ++ $(FirstSeatStarts, WarchiefCards, NoDrawDevelopments) ++ Module.all.but(NewBlood).but(Solo).but(VictoryModule).but(TrainingFields)./(ModuleOption) ++ $(MoreCreatures) ++ CentralChoice.all ++ AutomaLevelOption.all ++ hiddenOptions

    // Colors only for the clans in the game; 2v2 Teams only with four players, 3v3 and 2v2v2 Teams only with six
    override def optionsFor(n : Int, l : $[F]) = options.%{
        case ColorOption(f, _) => l.has(f)
        case ModuleOption(m) if Module.teams.contains(m) => Module.teams(m) == n && l.has(Automa).not
        case AutomaLevelOption(_) => l.has(Automa)
        case ModuleOption(VictoryModule) => false
        case _ => true
    }

    // The Victory conditions list stays short: Thane / Jarl only with an Alternative victory choice, the cards only with chosen cards
    // Training Fields has only the colors and the first player
    override def optionShown(o : O, selected : $[O]) = o match {
        case o if selected.has(TrainingFieldsOption) => o.is[ColorOption] || o == FirstSeatStarts
        case VictoryModeOption(_) => VictoryChoice.alternative.exists(selected.has)
        case VictoryCardOption(_) => selected.has(AltVictoryChosen)
        case _ => true
    }

    // Colors are chosen on each clan's row of the setup screen, the rest below
    override def optionPages(n : Int, l : $[F]) = {
        val all = optionsFor(n, l)
        $(all.diff(all.of[ColorOption]))
    }

    override def factionRowOptions(f : F, l : $[F]) = PlayerColor.all./(ColorOption(f, _))

    override def factionRowNone = "No color".txt

    // Next color; a clan that had it takes this clan's old color
    override def factionRowClick(f : F, l : $[F], selected : $[O]) = {
        val current = selected.of[ColorOption].%(_.clan == f)./(_.color).single
        val next = current./(c => (PlayerColor.all ++ PlayerColor.all).dropWhile(_ != c).drop(1).head).|(PlayerColor.all.first)
        val other = selected.of[ColorOption].%(o => o.color == next && o.clan != f)./(_.clan).single
        $(ColorOption(f, next)) ++ other./~(g => current./(ColorOption(g, _)))
    }

    // Colors by seat, as before colors could be chosen
    override def defaultsFor(n : Int, l : $[F]) = (l.zip(PlayerColor.all)./{ case (f, c) => ColorOption(f, c) : O } :+ (YearsOption.standard : O) :+ (StandardVictory : O) :+ (VictoryModeOption(false) : O) :+ (StandardCentral : O)) ++ l.has(Automa).$(AutomaLevelOption(2) : O)

    override def quickOptions = options./(o => o -> 0.0).toMap

    def has(options : $[O], m : Module) = options.has(ModuleOption(m)) || (m == TrainingFields && options.has(TrainingFieldsOption)) || (m == VictoryModule && VictoryChoice.alternative.exists(options.has)) || (m == Wastelands && CentralChoice.picked(options))

    // Quick Game: always three players, core clans and core rules (no option is turned on beyond the defaults)
    val quickMin = 3
    val quickMax = 3

    override def quickFactions = $(Bear, Boar, Goat, Raven, Snake, Stag, Wolf)

    def randomGameName() = {
        val n = $("Winter", "Clan", "Fjord", "Longship", "Saga", "Draugr").shuffle
        val c = $("for", "against", "versus", "through", "and", "of", "in", "as").shuffle
        n.head + " " + c.head + " " + n.last
    }

    def validateFactionCombination(factions : $[Faction]) = None ||
        (factions.num < 2).?(ErrorResult("Minimum two clans")) ||
        (factions.has(Automa) && factions.num != 2).?(ErrorResult("The Automa plays against one clan only")) ||
        (factions.num > 6).?(ErrorResult("Maximum six clans")) |
        InfoResult("Northgard: Uncharted Lands")

    def validateFactionSeatingOptions(factions : $[Faction], options : $[O]) : ValidationResult =
        if (options.has(TrainingFieldsOption))
            ((factions.num != 2 || factions.has(Automa)).?(ErrorResult("Training Fields is a duel for two clans")) | InfoResult("Training Fields"))
        else
            validateFactionCombination(factions) && {
                val colored = options.of[ColorOption]
                val missing = factions.%(f => colored.exists(_.clan == f).not)
                val teams = Module.teams.toList.%{ case (m, n) => has(options, m) && factions.num != n }
                teams.any.?(ErrorResult(teams./{ case (m, n) => m.label + " needs " + n + " players" }.mkString(", "))) ||
                (Module.teams.keys.count(has(options, _)) > 1).?(ErrorResult("Choose one team variant")) ||
                (factions.has(Automa) && options.of[AutomaLevelOption].exists(_.level >= 3) && has(options, Creatures).not).?(ErrorResult("Automa levels 3 and up need the Creatures module")) ||
                validateVictoryCards(factions, options) ||
                missing.any.?(WarningResult(missing./(factionName).mkString(", ") + " will get a free color")) |
                InfoResult("Northgard: Uncharted Lands")
            }

    // Alternative victory with chosen cards: exactly one Map Control card and two Wealth cards (three with teams)
    def validateVictoryCards(factions : $[Faction], options : $[O]) : |[ValidationResult] = if (options.has(AltVictoryChosen).not) None else {
        val chosen = options.of[VictoryCardOption]./(_.card)
        val maps = chosen.count(_.mapControl)
        val wealth = chosen.count(_.mapControl.not)
        val needed = Module.teams.exists { case (m, n) => has(options, m) && factions.num == n }.?(3).|(2)
        (maps != 1 || wealth != needed).?(ErrorResult("Choose 1 Map Control card (" + maps + " chosen) and " + needed + " Wealth cards (" + wealth + " chosen)"))
    }

    def factionName(f : Faction) = (f == Automa).?("Automa (solo)").|(f.name + " Clan")
    // No clan colors here: colors belong to players and are chosen on the setup screen
    def factionElem(f : Faction) = factionName(f).txt
    // The initial clan card and its two upgrades
    override def factionNote(f : Faction) =
        if (f == Automa)
            HorizontalBreak ~ "The solo opponent from Uncharted Horizons: a neutral clan with two Leaders that plays by its own cards. Pick it and one clan.".txt
        else
            HorizontalBreak ~ $(0, 1, 2)./(n => Image(ClanCard(f, n).info.image, styles.menuCard)).merge
    // Once picked, just the clan's emblem
    override def factionChosenElem(f : Faction) = (f == Automa).?(factionElem(f).spn(xstyles.bold)).|(Image("clan-" + f.style, styles.menuIcon) ~ factionElem(f).spn(xstyles.bold))

    // The clan picker's Warchief button: the clan board with the warchief's portrait and power, and the warchief's upgrade card (Warchiefs box)
    override def factionInfo(f : Faction) = (f != Automa).?((
        "Warchief".txt,
        Warchief.name(f).hlb ~ ", " ~ factionName(f) ~ " warchief",
        $(
            Image(Warchief.board(f), styles.menuBoard),
            ("Warchief power: ".hl ~ Warchief.power(f)).div(styles.menuText),
            Image(ClanCard(f, 3).info.image, styles.menuWarchiefCard),
            (ClanCard(f, 3).name.hl ~ " (clan upgrade): " ~ ClanCard(f, 3).info.text).div(styles.menuText),
        )
    ))

    // Images shown in the menus, before the game's assets are loaded
    override def menuImages = (
        clans./~(f => $(0, 1, 2, 3)./(n => ClanCard(f, n).info.image)./(i => i -> ("/hrf/webp2/nort/images/card/clan/" + i.drop("card-clan-".length) + ".webp"))) ++
        clans./(f => ("clan-" + f.style) -> ("/hrf/webp2/nort/images/clan/" + f.style + ".webp")) ++
        clans./(f => Warchief.board(f) -> ("/hrf/webp2/nort/images/expansion/board/" + Warchief.boardFile(f) + ".webp"))
    ).toMap

    def createGame(factions : $[Faction], options : $[O]) = new Game(factions, options)

    // Hard: BotHard (bot-hard.scala); the Automa plays by its own cards
    def getBots(f : Faction) = (f == Automa).?($("Easy")).|($("Easy", "Hard"))

    def getBot(f : Faction, b : String) = (f, b) match {
        case (f : Faction, "Hard") if f != Automa => new BotHard(f)
        case (f : Faction, _) => new BotXX(f)
    }

    def defaultBot(f : Faction) = "Easy"

    def writeFaction(f : Faction) = f.short
    def parseFaction(s : String) : |[Faction] = factions.%(_.short == s).single

    // Options are stored space-separated (saved setup, the online game's "options" line), so they are written without spaces:
    // ColorOption(Bear, Red) became two words and every color was lost
    def writeOption(o : O) = Serialize.write(o).replace(" ", "")
    // The halves of a color written with a space, in games created before that fix, are dropped: those games keep their colors by seat
    def parseOption(s : String) =
        if (s.startsWith("ColorOption(") && s.endsWith(")").not || (s.endsWith(")") && PlayerColor.all.exists(c => s == c.name + ")")))
            $
        else
            $(options.find(o => writeOption(o) == s) || options.find(o => o.toString == s) | (UnknownOption(s)))

    def parseAction(s : String) : Action = Serialize.parseAction(s)
    def writeAction(a : Action) : String = Serialize.write(a)

    val start = StartAction(gaming.version)

    // Images are in webp2/nort/images/; the green starting cards are the blue ones with the ribbon recoloured to the printed green; there are no orange ones, so orange uses the yellow ones
    val assets =
    ConditionalAssetsList((factions : $[F], options : $[O]) => true, "card/start", "card-start-", "webp")(
        PlayerColor.all./~(c => $("recruit", "move", "explore", "build", "feast")./(n => ImageAsset(c.id + "-" + n, c.cards + "-" + n + (n == "feast").??("-1"))))
    ) ::
    ConditionalAssetsList((factions : $[F], options : $[O]) => true, "card/clan", "card-clan-", "webp")(
        Cards.clan.values.$.flatten./(_.image.drop("card-clan-".length))./(ImageAsset(_))
    ) ::
    ConditionalAssetsList((factions : $[F], options : $[O]) => true, "card/dev", "card-dev-", "webp")(
        Cards.developments.keys.$./(ImageAsset(_))
    ) ::
    ConditionalAssetsList((factions : $[F], options : $[O]) => true, "card/achievement", "card-achievement-", "webp")(
        Cards.achievements.keys.$./(ImageAsset(_))
    ) ::
    ConditionalAssetsList((factions : $[F], options : $[O]) => has(options, Creatures), "card/creature", "card-creature-", "webp")(
        Creature.all./(c => ImageAsset(c.token.drop("creature-".length)))
    ) ::
    ConditionalAssetsList((factions : $[F], options : $[O]) => has(options, Creatures) && has(options, Wilderness), "card/creature", "card-creature-", "webp")(
        Creature.expansion./(c => ImageAsset(c.token.drop("creature-".length)))
    ) ::
    // Wastelands: its creatures, Hrimgandr and Jötunn Blainn
    ConditionalAssetsList((factions : $[F], options : $[O]) => has(options, Wastelands), "card/creature", "card-creature-", "webp")(
        (Creature.waste :+ Creature.hrimgandr)./(c => ImageAsset(c.token.drop("creature-".length))) :+ ImageAsset("blainn")
    ) ::
    ConditionalAssetsList((factions : $[F], options : $[O]) => true, "card", "card-", "webp")(
        $(ImageAsset("unrest"))
    ) ::
    // The Automa's cards (Solo module)
    ConditionalAssetsList((factions : $[F], options : $[O]) => factions.has(Automa), "card/automa", "card-automa-", "webp")(
        1.to(15).$./(n => ImageAsset("%02d".format(n))) ++ $(ImageAsset("reference-1"), ImageAsset("reference-2"))
    ) ::
    // Uncharted Horizons: the Event cards and the Alternative victory cards
    ConditionalAssetsList((factions : $[F], options : $[O]) => has(options, EventsModule), "card/event", "card-event-", "webp")(
        EventCard.all./(c => ImageAsset(c.id))
    ) ::
    ConditionalAssetsList((factions : $[F], options : $[O]) => has(options, VictoryModule), "card/victory", "card-victory-", "webp")(
        VictoryCard.all./(c => ImageAsset(c.id))
    ) ::
    ConditionalAssetsList((factions : $[F], options : $[O]) => true, "tile", "tile-", "webp")(
        Tiles.all.diff(Tiles.environment).diff(Tiles.wastelands).diff(Tiles.central).diff(Tiles.beach)./(t => ImageAsset(t.id))
    ) ::
    // Training Fields: the Action cards in each player color, and their backs
    ConditionalAssetsList((factions : $[F], options : $[O]) => options.has(TrainingFieldsOption), "card/training", "training-", "webp")(
        PlayerColor.all./~(c => (Drill.all./(_.id) :+ "back")./(n => ImageAsset(c.id + "-" + n)))
    ) ::
    // Training Fields: the back of the tiles still face down
    ConditionalAssetsList((factions : $[F], options : $[O]) => options.has(TrainingFieldsOption), "tile", "tile-", "webp")(
        $(ImageAsset("back"))
    ) ::
    // The shape of each area, tinted on the map with the colour of the player who controls it
    ConditionalAssetsList((factions : $[F], options : $[O]) => true, "tile/mask", "mask-", "webp")(
        Tiles.all.diff(Tiles.environment).diff(Tiles.wastelands).diff(Tiles.central).diff(Tiles.beach)./~(t => t.areas./(a => ImageAsset(t.id + "-" + a.id)))
    ) ::
    // Sea: the Beach tiles and the Raid cards
    ConditionalAssetsList((factions : $[F], options : $[O]) => has(options, Sea), "tile", "tile-", "webp")(
        Tiles.beach./(t => ImageAsset(t.id))
    ) ::
    ConditionalAssetsList((factions : $[F], options : $[O]) => has(options, Sea), "tile/mask", "mask-", "webp")(
        Tiles.beach./~(t => t.areas./(a => ImageAsset(t.id + "-" + a.id)))
    ) ::
    ConditionalAssetsList((factions : $[F], options : $[O]) => has(options, Sea), "card/raid", "card-raid-", "webp")(
        RaidCard.all./(c => ImageAsset(c.id))
    ) ::
    // Wilderness: the Environment tiles
    ConditionalAssetsList((factions : $[F], options : $[O]) => has(options, Wilderness) || options.has(WildLakeCentral) || options.has(RandomAnyCentral), "tile", "tile-", "webp")(
        Tiles.environment./(t => ImageAsset(t.id))
    ) ::
    ConditionalAssetsList((factions : $[F], options : $[O]) => has(options, Wilderness) || options.has(WildLakeCentral) || options.has(RandomAnyCentral), "tile/mask", "mask-", "webp")(
        Tiles.environment./~(t => t.areas./(a => ImageAsset(t.id + "-" + a.id)))
    ) ::
    // Wastelands: the Environment and central tiles
    ConditionalAssetsList((factions : $[F], options : $[O]) => has(options, Wastelands), "tile", "tile-", "webp")(
        (Tiles.wastelands ++ Tiles.central)./(t => ImageAsset(t.id))
    ) ::
    ConditionalAssetsList((factions : $[F], options : $[O]) => has(options, Wastelands), "tile/mask", "mask-", "webp")(
        (Tiles.wastelands ++ Tiles.central)./~(t => t.areas./(a => ImageAsset(t.id + "-" + a.id)))
    ) ::
    // The clan boards (clan power, warchief and power), shown below the Lore Tree; loaded when shown
    ConditionalAssetsList((factions : $[F], options : $[O]) => true, "expansion/board", "board-", "webp", lzy = Laziness.OnDemand)(
        mmm.factions./(f => ImageAsset(f.style, Warchief.boardFile(f)))
    ) ::
    ConditionalAssetsList((factions : $[F], options : $[O]) => true, "token/unit", "unit-", "webp")(
        PlayerColor.all./(c => ImageAsset(c.id, "unit-" + c.id))
    ) ::
    // The warchiefs, and the Automa's Leaders
    ConditionalAssetsList((factions : $[F], options : $[O]) => has(options, Warchiefs) || factions.has(Automa), "token/unit", "warchief-", "webp")(
        PlayerColor.all./(c => ImageAsset(c.id, "warchief-" + c.id))
    ) ::
    // The warchiefs as portraits (core clans) or round tokens (New Blood, and Horse's Brok) in each player color (Warchief.figure); only the clans in play
    (Warchief.portraits ++ Warchief.tokens).flatMap(f => (f.style +: (f == Horse).$("brok")).map(n => ConditionalAssetsList((factions : $[F], options : $[O]) => has(options, Warchiefs) && factions.has(f), "token/unit", "chief-" + n + "-", "webp")(
        PlayerColor.all./(c => ImageAsset(c.id, "chief-" + n + "-" + c.id))
    ))) :::
    ConditionalAssetsList((factions : $[F], options : $[O]) => true, "token", "token-", "webp")(
        $(ImageAsset("kaija"), ImageAsset("scorched-earth"))
    ) ::
    ConditionalAssetsList((factions : $[F], options : $[O]) => has(options, Wastelands), "token", "token-", "webp")(
        $(ImageAsset("blainn"), ImageAsset("wood"))
    ) ::
    ConditionalAssetsList((factions : $[F], options : $[O]) => has(options, Wastelands), "token/creature", "creature-", "webp")(
        (Creature.waste :+ Creature.hrimgandr)./(c => ImageAsset(c.token.drop("creature-".length)))
    ) ::
    // New Blood: Brundr and Kaelinn, the High Tide tokens, the Ancestral Equipment tokens
    ConditionalAssetsList((factions : $[F], options : $[O]) => factions.exists(NewBlood.clans.has), "token", "token-", "webp")(
        $(ImageAsset("lynx"), ImageAsset("high-tide"), ImageAsset("pyre")) ++ 1.to(7).$./~(n => $(ImageAsset("ox-" + n), ImageAsset("ox-back-" + n)))
    ) ::
    ConditionalAssetsList((factions : $[F], options : $[O]) => has(options, Creatures), "token/creature", "creature-", "webp")(
        Creature.all./(c => ImageAsset(c.token.drop("creature-".length)))
    ) ::
    ConditionalAssetsList((factions : $[F], options : $[O]) => has(options, Creatures) && has(options, Wilderness), "token/creature", "creature-", "webp")(
        Creature.expansion./(c => ImageAsset(c.token.drop("creature-".length)))
    ) ::
    ConditionalAssetsList((factions : $[F], options : $[O]) => true, "token/building", "building-", "webp")(
        Building.all./(b => ImageAsset(b.image.drop("building-".length)))
    ) ::
    ConditionalAssetsList((factions : $[F], options : $[O]) => true, "ui", "ui-", "webp")(
        (1.to(99).map(n => ImageAsset("label-" + n)) ++ 1.to(15).map(n => ImageAsset("count-" + n)) ++ 1.to(40).map(n => ImageAsset("spot-" + n))).toList :+ ImageAsset("target") :+ ImageAsset("lore") :+ ImageAsset("food") :+ ImageAsset("wood") :+ ImageAsset("fame") :+ ImageAsset("confirm") :+ ImageAsset("cancel") :+ ImageAsset("rotate-left") :+ ImageAsset("rotate-right")
    ) ::
    $

    override val showAbout = false

    // Map display settings, chosen by each player under "Interface" (like Root's Clearing Rule)
    override def settingsList = super.settingsList ++ $(ShowTerritoryColor, FightsTerritoryColor, HideTerritoryColor, ShowBorderColor, HideBorderColor)
    override def settingsDefaults = super.settingsDefaults ++ $(ShowTerritoryColor, ShowBorderColor)
}


// Territories tinted with their controller's colour (gray when invaded and pink during a fight are kept with Fights Only)
trait TerritoryColorSetting extends hrf.Setting with OneOfGroup {
    val group = "Territory Color"
}

case object ShowTerritoryColor extends TerritoryColorSetting {
    val valueOn = "Show".hlb
}

case object FightsTerritoryColor extends TerritoryColorSetting {
    val valueOn = "Fights Only".hlb
}

case object HideTerritoryColor extends TerritoryColorSetting {
    val valueOn = "Hide".hlb
}


// The border dashes of closed territories that give fame, in their controllers' colours
trait BorderColorSetting extends hrf.Setting with OneOfGroup {
    val group = "Fame Borders"
}

case object ShowBorderColor extends BorderColorSetting {
    val valueOn = "Show".hlb
}

case object HideBorderColor extends BorderColorSetting {
    val valueOn = "Hide".hlb
}
