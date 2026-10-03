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

    override val underConstruction = true

    val factions = $(Bear, Boar, Goat, Raven, Snake, Stag, Wolf)

    val minPlayers = 2
    override val maxPlayers = 5

    override val hiddenOptions = $

    val options = $ ++ hiddenOptions

    val quickMin = 2
    val quickMax = 4

    def randomGameName() = {
        val n = $("Winter", "Clan", "Fjord", "Longship", "Saga", "Draugr").shuffle
        val c = $("for", "against", "versus", "through", "and", "of", "in", "as").shuffle
        n.head + " " + c.head + " " + n.last
    }

    def validateFactionCombination(factions : $[Faction]) = None ||
        (factions.num < 2).?(ErrorResult("Minimum two clans")) ||
        (factions.num > 5).?(ErrorResult("Maximum five clans")) |
        InfoResult("Northgard: Uncharted Lands")

    def validateFactionSeatingOptions(factions : $[Faction], options : $[O]) = validateFactionCombination(factions)

    def factionName(f : Faction) = f.name + " Clan"
    def factionElem(f : Faction) = f.name.styled(f)

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

    // Images are in webp2/nort/images/; there are no green starting cards, so green uses the blue ones
    val assets =
    ConditionalAssetsList((factions : $[F], options : $[O]) => true, "card/start", "card-start-", "webp")(
        PlayerColor.all./~(c => $("recruit", "move", "explore", "build", "feast")./(n => ImageAsset(c.id + "-" + n, (c == Green).?("blue").|(c.id) + "-" + n + (n == "feast").??("-1"))))
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
    ConditionalAssetsList((factions : $[F], options : $[O]) => true, "card", "card-", "webp")(
        $(ImageAsset("unrest"))
    ) ::
    ConditionalAssetsList((factions : $[F], options : $[O]) => true, "tile", "tile-", "webp")(
        Tiles.all./(t => ImageAsset(t.id))
    ) ::
    ConditionalAssetsList((factions : $[F], options : $[O]) => true, "token/unit", "unit-", "webp")(
        PlayerColor.all./(c => ImageAsset(c.id, "unit-" + c.id))
    ) ::
    ConditionalAssetsList((factions : $[F], options : $[O]) => true, "token/building", "building-", "webp")(
        Building.all./(b => ImageAsset(b.image.drop("building-".length)))
    ) ::
    ConditionalAssetsList((factions : $[F], options : $[O]) => true, "ui", "ui-", "webp")(
        (1.to(99).map(n => ImageAsset("label-" + n)) ++ 1.to(15).map(n => ImageAsset("count-" + n)) ++ 1.to(40).map(n => ImageAsset("spot-" + n))).toList :+ ImageAsset("target")
    ) ::
    $

    override val about = $(
        "An adaptation of the " ~ "Northgard: Uncharted Lands".hl ~ " board game.",
        " ".pre.div,
        "Very much " ~ "under construction".styled(xstyles.warning) ~ ".",
        "The base game comes first; the expansions after that.",
    )
}
