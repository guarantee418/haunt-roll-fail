package tactics
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
    val gaming = tactics.gaming

    type F = Faction

    def tagF = implicitly

    val name = "tactics"
    val label = "Hex Tactics"

    val factions = Faction.all

    val minPlayers = 2

    val options = $[O]()

    val quickMin = 2
    val quickMax = 2

    override val about = $[Elem](
        "Hex Tactics".hlb ~ " (a working name), a hex-based tactical war game of the owner's own design, in development.",
        "Two players deploy units, move them over the hexes and fight for the map's sectors; the first to 10 VP wins.",
    )

    def randomGameName() = {
        val n = $("Ridge", "Town", "Depot", "Farm", "River", "Forest", "Hills", "Crossing", "Line", "Front").shuffle
        val c = $("for", "against", "versus", "through", "and", "of", "beyond", "over").shuffle
        n.head + " " + c.head + " " + n.last
    }

    def validateFactionCombination(factions : $[Faction]) = None ||
        (factions.num < 2).?(ErrorResult("Select two players")) ||
        (factions.num > 2).?(ErrorResult("Two players only")) |
        InfoResult("Hex Tactics")

    def validateFactionSeatingOptions(factions : $[Faction], options : $[O]) = InfoResult("Hex Tactics")

    def factionName(f : Faction) = f.name
    def factionElem(f : Faction) = f.name.styled(f)

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

    // the board and pieces are drawn in code (ui.scala), so there are no images to load
    val assets = $[ConditionalAssetsList]()
}
