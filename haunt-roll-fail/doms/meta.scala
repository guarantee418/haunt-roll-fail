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

import hrf.meta._
import hrf.options._
import hrf.elem._


case class UnknownOption(o : String) extends GameOption {
    val group = "Unknown"
    val valueOn = "Unknown Option " ~ o
}


object Meta extends MetaGame { mmm =>
    val gaming = doms.gaming

    type F = Faction

    def tagF = implicitly

    val name = "doms"
    val label = "Dominant Species"

    val factions = $(Mammal, Reptile, Bird, Amphibian, Arachnid, Insect)

    val minPlayers = 2

    override val hiddenOptions = $

    val options = $ ++ hiddenOptions

    val quickMin = 6
    val quickMax = 6

    def randomGameName() = {
        val n = $("Nature", "Evolution", "Adjustment", "Progress", "Extinction").shuffle
        val c = $("for", "against", "versus", "through", "and", "of", "in", "as").shuffle
        n.head + " " + c.head + " " + n.last
    }

    def validateFactionCombination(factions : $[Faction]) = None ||
        (factions.num < 2).?(ErrorResult("Minimum two factions")) ||
        (factions.num > 6).?(ErrorResult("Max six factions")) |
        InfoResult("Dominant Species")

    def validateFactionSeatingOptions(factions : $[Faction], options : $[O]) = validateFactionCombination(factions)

    def factionName(f : Faction) = f.name
    def factionElem(f : Faction) = f.name.styled(f)

    override def glyph(g : G) : |[String] = g.highlight.current./(_.style + "-glyph")
    override def glyph(f : F) : |[String] = |(f.style + "-glyph")
    override def glyph(g : G, f : F) : |[String] = glyph(f).%!(_ => g.highlight.faction.has(f) && hrf.HRF.uptime() / 1000 % 2 == 1)

    def createGame(factions : $[Faction], options : $[O]) = new Game(factions, options)

    def getBots(f : Faction) = $("Easy")

    def getBot(f : Faction, b : String) = (f, b) match {
        case (f : Faction, _) => new BotXX(f)
    }

    def defaultBot(f : Faction) = "Easy"

    def writeFaction(f : Faction) = f.short
    def parseFaction(s : String) : |[Faction] = factions.%(_.short == s).single

    def writeOption(o : O) = Serialize.write(o)
    def parseOption(s : String) = $(options.find(o => writeOption(o) == s) || options.find(o => o.toString == s) | (UnknownOption(s)))

    def parseAction(s : String) : Action = Serialize.parseAction(s)
    def writeAction(a : Action) : String = Serialize.write(a)

    val start = StartAction(gaming.version)

    override def bodyFont = Some("arno-pro-smbd-caption")
    override def titleFont = Some("chiller")

    val assets =
    ConditionalAssetsList((factions : $[F], options : $[O]) => true)(
        ImageAsset("map") ::
        ImageAsset("chart") ::
        ImageAsset("chart-2") ::
    $) ::
    ConditionalAssetsList((factions : $[F], options : $[O]) => true, "icon")(
        ImageAsset("black-arrow-en") ::
        ImageAsset("black-arrow-es") ::
        ImageAsset("black-arrow-n") ::
        ImageAsset("black-arrow-s") ::
        ImageAsset("black-arrow-wn") ::
        ImageAsset("black-arrow-ws") ::
        ImageAsset("black-cone") ::
        ImageAsset("black-cube") ::
        ImageAsset("black-cylinder") ::
        ImageAsset("black-snowflake") ::
        ImageAsset("blue-arrow-en") ::
        ImageAsset("blue-arrow-es") ::
        ImageAsset("blue-arrow-n") ::
        ImageAsset("blue-arrow-s") ::
        ImageAsset("blue-arrow-wn") ::
        ImageAsset("blue-arrow-ws") ::
        ImageAsset("blue-cone") ::
        ImageAsset("blue-cube") ::
        ImageAsset("blue-cylinder") ::
        ImageAsset("blue-snowflake") ::
        ImageAsset("green-arrow-en") ::
        ImageAsset("green-arrow-es") ::
        ImageAsset("green-arrow-n") ::
        ImageAsset("green-arrow-s") ::
        ImageAsset("green-arrow-wn") ::
        ImageAsset("green-arrow-ws") ::
        ImageAsset("green-cone") ::
        ImageAsset("green-cube") ::
        ImageAsset("green-cylinder") ::
        ImageAsset("green-snowflake") ::
        ImageAsset("red-arrow-en") ::
        ImageAsset("red-arrow-es") ::
        ImageAsset("red-arrow-n") ::
        ImageAsset("red-arrow-s") ::
        ImageAsset("red-arrow-wn") ::
        ImageAsset("red-arrow-ws") ::
        ImageAsset("red-cone") ::
        ImageAsset("red-cube") ::
        ImageAsset("red-cylinder") ::
        ImageAsset("red-snowflake") ::
        ImageAsset("white-arrow-en") ::
        ImageAsset("white-arrow-es") ::
        ImageAsset("white-arrow-n") ::
        ImageAsset("white-arrow-s") ::
        ImageAsset("white-arrow-wn") ::
        ImageAsset("white-arrow-ws") ::
        ImageAsset("white-cone") ::
        ImageAsset("white-cube") ::
        ImageAsset("white-cylinder") ::
        ImageAsset("white-snowflake") ::
        ImageAsset("yellow-cone") ::
        ImageAsset("yellow-cube") ::
        ImageAsset("yellow-cylinder") ::
        ImageAsset("yellow-snowflake") ::
        ImageAsset("yellow-arrow-en") ::
        ImageAsset("yellow-arrow-es") ::
        ImageAsset("yellow-arrow-n") ::
        ImageAsset("yellow-arrow-s") ::
        ImageAsset("yellow-arrow-wn") ::
        ImageAsset("yellow-arrow-ws") ::
        ImageAsset("yellow-arrow-en-en") ::
        ImageAsset("yellow-arrow-es-es") ::
        ImageAsset("yellow-arrow-n-n") ::
        ImageAsset("yellow-arrow-s-s") ::
        ImageAsset("yellow-arrow-wn-wn") ::
        ImageAsset("yellow-arrow-ws-ws") ::
        ImageAsset("yellow-arrow-ne") ::
        ImageAsset("yellow-arrow-se") ::
        ImageAsset("yellow-arrow-w") ::
        ImageAsset("yellow-arrow-e") ::
        ImageAsset("yellow-arrow-nw") ::
        ImageAsset("yellow-arrow-sw") ::
        ImageAsset("pain") ::
        ImageAsset("kill") ::
    $) ::
    ConditionalAssetsList((factions : $[F], options : $[O]) => true, "resource", scale = 50)(
        ImageAsset("grass") ::
        ImageAsset("grub") ::
        ImageAsset("meat") ::
        ImageAsset("seed") ::
        ImageAsset("sun") ::
        ImageAsset("water") ::
        ImageAsset("snowflake") ::
    $) ::
    ConditionalAssetsList((factions : $[F], options : $[O]) => true, "card")(
        ImageAsset("symbiotic") ::
        ImageAsset("predator") ::
        ImageAsset("parasitism") ::
        ImageAsset("omnivore") ::
        ImageAsset("nocturnal") ::
        ImageAsset("niche-biomes") ::
        ImageAsset("metamorphosis") ::
        ImageAsset("mass-exodus") ::
        ImageAsset("intelligence") ::
        ImageAsset("instinct") ::
        ImageAsset("immigrants") ::
        ImageAsset("ice-sheet") ::
        ImageAsset("ice-age") ::
        ImageAsset("hibernation") ::
        ImageAsset("habitat") ::
        ImageAsset("fertile") ::
        ImageAsset("fecundity") ::
        ImageAsset("evolution") ::
        ImageAsset("ecodiversity") ::
        ImageAsset("disease") ::
        ImageAsset("cold-snap") ::
        ImageAsset("catastrophe") ::
        ImageAsset("blight") ::
        ImageAsset("biomass") ::
        ImageAsset("aquatic") ::
        ImageAsset("biodiversity") ::
        ImageAsset("survival") ::
    $) ::
    ConditionalAssetsList((factions : $[F], options : $[O]) => true, "tile")(
        ImageAsset("desert") ::
        ImageAsset("forest") ::
        ImageAsset("jungle") ::
        ImageAsset("mountain") ::
        ImageAsset("savannah") ::
        ImageAsset("sea") ::
        ImageAsset("tundra") ::
        ImageAsset("wetland") ::
        ImageAsset("empty", "unknown") ::
        ImageAsset("cover") ::
    $) ::
    ConditionalAssetsList((factions : $[F], options : $[O]) => true, "letter")(
        ImageAsset("empty-1") ::
        ImageAsset("empty-2") ::
        ImageAsset("empty-3") ::
        ImageAsset("empty-4") ::
        ImageAsset("empty-5") ::
        ImageAsset("empty-6") ::
        ImageAsset("empty-7") ::
        ImageAsset("empty-8") ::
        ImageAsset("empty-9") ::
        ImageAsset("empty-10") ::
        ImageAsset("empty-11") ::
        ImageAsset("empty-12") ::
        ImageAsset("empty-13") ::
        ImageAsset("empty-14") ::
        ImageAsset("empty-15") ::
        ImageAsset("empty-16") ::
        ImageAsset("empty-17") ::
        ImageAsset("empty-18") ::
        ImageAsset("empty-19") ::
        ImageAsset("empty-20") ::
        ImageAsset("empty-21") ::
        ImageAsset("empty-22") ::
        ImageAsset("desert-1") ::
        ImageAsset("desert-2") ::
        ImageAsset("desert-3") ::
        ImageAsset("desert-4") ::
        ImageAsset("forest-1") ::
        ImageAsset("forest-2") ::
        ImageAsset("forest-3") ::
        ImageAsset("forest-4") ::
        ImageAsset("jungle-1") ::
        ImageAsset("jungle-2") ::
        ImageAsset("jungle-3") ::
        ImageAsset("jungle-4") ::
        ImageAsset("mountain-1") ::
        ImageAsset("mountain-2") ::
        ImageAsset("mountain-3") ::
        ImageAsset("mountain-4") ::
        ImageAsset("savannah-1") ::
        ImageAsset("savannah-2") ::
        ImageAsset("savannah-3") ::
        ImageAsset("savannah-4") ::
        ImageAsset("sea-1") ::
        ImageAsset("sea-2") ::
        ImageAsset("sea-3") ::
        ImageAsset("sea-4") ::
        ImageAsset("sea-5") ::
        ImageAsset("sea-6") ::
        ImageAsset("tundra-1") ::
        ImageAsset("tundra-2") ::
        ImageAsset("tundra-3") ::
        ImageAsset("tundra-4") ::
        ImageAsset("tundra-5") ::
        ImageAsset("tundra-6") ::
        ImageAsset("tundra-7") ::
        ImageAsset("tundra-8") ::
        ImageAsset("tundra-9") ::
        ImageAsset("tundra-10") ::
        ImageAsset("tundra-11") ::
        ImageAsset("tundra-12") ::
        ImageAsset("wetland-1") ::
        ImageAsset("wetland-2") ::
        ImageAsset("wetland-3") ::
        ImageAsset("wetland-4") ::
    $) ::
    $

    override def extLinks = $(
    ) ++ super.extLinks

}
