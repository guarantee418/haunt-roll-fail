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

import hrf.meta._
import hrf.elem._

object Meta extends MetaGame { mmm =>
    val gaming = cyclades.gaming

    type F = Faction

    def tagF = implicitly

    val name = "cyclades"
    val label = "Cyclades"

    val factions = Faction.all

    val minPlayers = 2

    val options = $[O]()

    val quickMin = 2
    val quickMax = 5

    override val about = $[Elem](
        "Cyclades".hlb ~ " by Bruno Cathala and Ludovic Maublanc, illustrated by Miguel Coimbra, published by Matagot (2009); the base game.",
        "The art is from the Tabletop Simulator mods of Cyclades.",
    )

    override def howToPlayLinks = $[(Elem, String)](
        ("Cyclades".hh ~ " on Matagot's site") -> "https://www.matagot.com/en/our-games/cyclades",
        ("Cyclades".hh ~ " on BoardGameGeek, with its rulebook") -> "https://boardgamegeek.com/boardgame/54998/cyclades",
    )

    override def howToPlay = $[(Elem, $[Elem])](
        ("The rules".txt, $[Elem](
            "The rules are the game's own rulebook's: this site keeps to them for you, offering only the moves they allow. Where they leave room, the choices made here are in the game's RULES notes.",
        )),
        ("Starting a game".txt, $[Elem](
            "Quick Game".hh ~ " starts at once against Easy bots.",
            "Local Game".hh ~ " lets you pick 2 to 5 players and make each one a person or a bot; people take turns on this device.",
            "Play Online".hh ~ " makes a game with a link for each player, so everyone plays from their own device, whenever they like.",
        )),
        ("Making your moves".txt, $[Elem](
            "When it's your turn, your choices are listed in the action pane, and the isles and sea spaces they concern are ringed on the map.",
            "Tap a choice in the list, or tap a ringed isle or sea space on the map. If it has several choices, they open over the map: pick one, or Cancel.",
            "Offerings are made in two steps: tap a god, then the GP to offer.",
            "Zoom the map with the mouse wheel or by pinching, and drag it to look around.",
        )),
        ("Seeing what's going on".txt, $[Elem](
            "The player panels show each player's GP, Priests, Philosophers, Fleets, Troops, buildings and Metropolises; the panel under them shows the cycle, the gods' order and offerings, and the Creatures on offer.",
            "Tap a Creature anywhere to see its card large; tap again to close it.",
        )),
        ("Undo, replays and help".txt, $[Elem](
            "Tap a line in the log to see the game as it was then: from there you can replay the game from that point, or undo back to it.",
            "Interface".hh ~ " under the action pane has the display settings, " ~ "Report a Bug".hh ~ " sends a report to the site, and " ~ "Main Menu".hh ~ " goes back to this menu.",
        )),
    )

    def randomGameName() = {
        val n = $("Isles", "Gods", "Oracles", "Triremes", "Hoplites", "Temples", "Offerings", "Krakens", "Harbors", "Philosophers").shuffle
        val c = $("for", "against", "versus", "through", "and", "of", "in", "as").shuffle
        n.head + " " + c.head + " " + n.last
    }

    def validateFactionCombination(factions : $[Faction]) = None ||
        (factions.num < 2).?(ErrorResult("Select at least two players")) ||
        (factions.num > 5).?(ErrorResult("Max five players")) |
        InfoResult("Cyclades")

    def validateFactionSeatingOptions(factions : $[Faction], options : $[O]) = InfoResult("Cyclades")

    def factionName(f : Faction) = f.name
    def factionElem(f : Faction) = f.name.styled(f)

    override def setupDesign = true
    override def factionsWord = "players"
    override def setupRowsLabel = "Players"
    override def menuImages = Faction.all./(f => ("menu-" + f.id) -> ("/hrf/webp2/cyclades/images/piece/troop-" + f.id + ".webp")).toMap
    override def factionTile(f : Faction) = |(Div(Image("menu-" + f.id, ExternalStyle("setup-emblem"), "")) ~ Div(f.name.hlb))
    override def factionRowIcon(f : Faction) : Elem = Image("menu-" + f.id, ExternalStyle("setup-row-emblem"), "")
    override def factionChosenElem(f : Faction) = factionRowIcon(f) ~ factionElem(f).spn(xstyles.bold)

    override def glyph(f : F) : |[String] = |("piece:troop-" + f.id)

    def createGame(factions : $[Faction], options : $[O]) = new Game(factions, options)

    def getBots(f : Faction) = $("Easy")

    def getBot(f : Faction, b : String) = new BotXX(f)

    def defaultBot(f : Faction) = "Easy"

    def writeFaction(f : Faction) = f.short
    def parseFaction(s : String) : |[Faction] = factions.%(_.short == s).single

    def writeOption(o : O) = o.toString
    def parseOption(s : String) = options.%(o => o.toString == s)

    def parseAction(s : String) : Action = Serialize.parseAction(s)
    def writeAction(a : Action) : String = Serialize.write(a)

    val start = StartAction(gaming.version)

    val assets =
    ConditionalAssetsList((factions : $[F], options : $[O]) => true, "", "", "webp")(
        ImageAsset("select") ::
        ImageAsset("target") ::
        ImageAsset("prosperity") ::
    $) ::
    ConditionalAssetsList((factions : $[F], options : $[O]) => factions.num <= 3, "", "", "webp")(ImageAsset("map-23") :: $) ::
    ConditionalAssetsList((factions : $[F], options : $[O]) => factions.num == 4, "", "", "webp")(ImageAsset("map-4") :: $) ::
    ConditionalAssetsList((factions : $[F], options : $[O]) => factions.num >= 5, "", "", "webp")(ImageAsset("map-5") :: $) ::
    ConditionalAssetsList((factions : $[F], options : $[O]) => true, "piece", "piece:", "webp")(
        Faction.all./~(f => $("troop", "fleet", "territory")./(n => ImageAsset(n + "-" + f.id)))
    ) ::
    ConditionalAssetsList((factions : $[F], options : $[O]) => true, "god", "god:", "webp")(
        (God.main ++ $(Apollo))./~(g => $(ImageAsset(g.id), ImageAsset("head-" + g.id)))
    ) ::
    ConditionalAssetsList((factions : $[F], options : $[O]) => true, "building", "building:", "webp")(
        (Building.all./(_.id) :+ "metropolis")./(ImageAsset(_))
    ) ::
    ConditionalAssetsList((factions : $[F], options : $[O]) => true, "creature", "creature:", "webp")(
        Creature.all./(c => ImageAsset(c.id))
    ) ::
    ConditionalAssetsList((factions : $[F], options : $[O]) => true, "figure", "figure:", "webp")(
        Creature.all.%(_.figure)./(c => ImageAsset(c.id))
    ) ::
    ConditionalAssetsList((factions : $[F], options : $[O]) => true, "card", "card:", "webp")(
        $(ImageAsset("priest"), ImageAsset("philosopher"))
    ) ::
    $
}
