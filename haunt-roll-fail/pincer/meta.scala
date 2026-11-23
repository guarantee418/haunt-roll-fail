package pincer
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
    val gaming = pincer.gaming

    type F = Faction

    def tagF = implicitly

    val name = "pincer"
    val label = "Pincer: A Gentleman's War"

    val factions = Faction.all

    val minPlayers = 2

    val options = $[O]()

    val quickMin = 2
    val quickMax = 4

    override val about = $[Elem](
        "Pincer: A Gentleman's War".hlb ~ " by Julian Birke (Birke Publishing), a work in progress played here from the second draft of its rulebook.",
        "The art is from the Tabletop Simulator mod \"Pincer: A Gentleman's War (Updated for First Draft)\".",
    )

    def randomGameName() = {
        val n = $("Empire", "Colonies", "Muskets", "Frigates", "Forts", "Trade", "Crowns", "Seals", "Honour", "Powder").shuffle
        val c = $("for", "against", "versus", "through", "and", "of", "in", "as").shuffle
        n.head + " " + c.head + " " + n.last
    }

    def validateFactionCombination(factions : $[Faction]) = None ||
        (factions.num < 2).?(ErrorResult("Select at least two players")) ||
        (factions.num > 4).?(ErrorResult("Max four players")) |
        InfoResult("Pincer")

    def validateFactionSeatingOptions(factions : $[Faction], options : $[O]) = InfoResult("Pincer")

    def factionName(f : Faction) = f.name
    def factionElem(f : Faction) = f.name.styled(f)

    override def glyph(f : F) : |[String] = |("piece:colony-" + f.id)

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
        ImageAsset("map") ::
        ImageAsset("hex-select") ::
        ImageAsset("hex-target") ::
    $) ::
    ConditionalAssetsList((factions : $[F], options : $[O]) => true, "piece", "piece:", "webp")(
        Faction.all./~(f =>
            $("colony", "fort", "port", "musketeer", "cavalry", "cannon", "frigate")./(n => ImageAsset(n + "-" + f.id)) ++
            $("road", "bridge")./~(n => $("e", "ne", "se")./(d => ImageAsset(n + "-" + f.id + "-" + d)))
        )
    ) ::
    ConditionalAssetsList((factions : $[F], options : $[O]) => true, "card", "card:", "webp")(
        (Resource.all./(_.image) ++ Maneuver.all./(_.image) :+ "card-back")./(ImageAsset(_))
    ) ::
    ConditionalAssetsList((factions : $[F], options : $[O]) => true, "seal", "seal:", "webp")(
        Seal.all./(_.image)./(ImageAsset(_))
    ) ::
    ConditionalAssetsList((factions : $[F], options : $[O]) => true, "die", "die:", "webp")(
        Resource.all./(_.id)./(ImageAsset(_))
    ) ::
    ConditionalAssetsList((factions : $[F], options : $[O]) => true, "war", "war:", "webp")(
        Faction.all./~(f => $("gentleman", "boar")./(n => ImageAsset(n + "-" + f.id)))
    ) ::
    $
}
