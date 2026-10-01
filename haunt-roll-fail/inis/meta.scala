package inis
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
    val gaming = inis.gaming

    type F = Faction

    def tagF = implicitly

    val name = "inis"
    val label = "INIS"

    val factions = $(Blue, Green, Orange, White)

    val minPlayers = 2

    override val hiddenOptions = $

    val options = $ ++ hiddenOptions

    val quickMin = 4
    val quickMax = 4

    def randomGameName() = {
        val n = $("Wind", "Isle", "Men", "Prophecy", "Extinction").shuffle
        val c = $("for", "against", "versus", "through", "and", "of", "in", "as").shuffle
        n.head + " " + c.head + " " + n.last
    }

    def validateFactionCombination(factions : $[Faction]) = None ||
        (factions.num < 2).?(ErrorResult("Minimum two factions")) ||
        (factions.num > 4).?(ErrorResult("Max six factions")) |
        InfoResult("INIS")

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

    override def bodyFont = Some("sherwood")

    val assets =
    ConditionalAssetsList((factions : $[F], options : $[O]) => true, "territory")(
        ImageAsset("haven") ::
        ImageAsset("hy-breasil") ::
        ImageAsset("salmons-creek") ::
        ImageAsset("isle-of-jay") ::
        ImageAsset("lost-vale") ::
        ImageAsset("stone-circle") ::
        ImageAsset("salt-mine") ::
        ImageAsset("gates-of-tir-na-nog") ::
        ImageAsset("highlands") ::
        ImageAsset("meadows") ::
        ImageAsset("hills") ::
        ImageAsset("forest") ::
        ImageAsset("mountain") ::
        ImageAsset("valley") ::
        ImageAsset("iron-mine") ::
        ImageAsset("swamp") ::
        ImageAsset("plains") ::
        ImageAsset("inis-mona") ::
        ImageAsset("moor") ::
        ImageAsset("aber") ::
        ImageAsset("cove") ::
        ImageAsset("misty-lands") ::
    $) ::
    $

    override def extLinks = $(
    ) ++ super.extLinks

}
