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
    val label = "Company of Heroes"

    val factions = Faction.all

    val minPlayers = 2

    val options = $[O]() ++ RulesChoice.all ++ CommandersChoice.all ++ Length.all

    // new games play the Full Game Rules (2026-11-24); a game with neither choice (made before) plays the First Game Rules
    override def defaultsFor(n : Int, l : $[F]) = $(FullGameRules, DrawCommanders, ShortGame)

    // a setup saved in the browser before the rules choice gets the Full Game Rules
    override def completeSaved(n : Int, l : $[F], options : $[O]) : $[O] = {
        val o = options.of[RulesChoice].any.?(options).|(options :+ FullGameRules)
        o.of[CommandersChoice].any.?(o).|(o :+ DrawCommanders)
    }

    val quickMin = 2
    val quickMax = 2

    override val about = $[Elem](
        "Company of Heroes: The Board Game".hlb ~ ", 2nd Edition, by Bad Crow Games, licensed from SEGA and Relic Entertainment: a work in progress played here by a team helping to test it.",
        "The rules are the core rulebook's Full Game Rules with its twelve Commanders (or its First Game Rules, a setup choice); the art is from the Tabletop Simulator mod \"Company of Heroes - 2nd Edition\".",
        "© SEGA. Developed by Relic Entertainment Inc. SEGA, the SEGA logo, Relic Entertainment and Company of Heroes are either registered trademarks or trademarks of SEGA Corporation or its affiliates.",
    )

    def randomGameName() = {
        val n = $("River", "Ridge", "Crossing", "Hedgerow", "Bridge", "Farm", "Mill", "Ford", "Orchard", "Chapel").shuffle
        val c = $("at", "by", "over", "past", "beyond", "under").shuffle
        "The " + n.head + " " + c.head + " the " + n.last
    }

    def validateFactionCombination(factions : $[Faction]) = None ||
        (factions.num != 2).?(ErrorResult("Select two players")) |
        InfoResult("Company of Heroes")

    def validateFactionSeatingOptions(factions : $[Faction], options : $[O]) = InfoResult("Company of Heroes")

    def factionName(f : Faction) = f.name
    def factionElem(f : Faction) = f.name.styled(f)

    override def glyph(f : F) : |[String] = |("emblem:" + f.id)

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
        ImageAsset("hex-spawn-us") ::
        ImageAsset("hex-spawn-wehrmacht") ::
    $) ::
    ConditionalAssetsList((factions : $[F], options : $[O]) => true, "token", "token:", "webp")(
        UnitType.all./(t => ImageAsset(t.id + "-" + t.faction.id))
    ) ::
    ConditionalAssetsList((factions : $[F], options : $[O]) => true, "emblem", "emblem:", "webp")(
        Faction.all./(f => ImageAsset(f.id))
    ) ::
    ConditionalAssetsList((factions : $[F], options : $[O]) => true, "res", "res:", "webp")(
        $(Manpower, Munitions, Fuel, VP, XP)./(r => ImageAsset(r.id))
    ) ::
    ConditionalAssetsList((factions : $[F], options : $[O]) => true, "dmg", "dmg:", "webp")(
        $(AI, AP, HE, Flame)./(d => ImageAsset(d.id))
    ) ::
    ConditionalAssetsList((factions : $[F], options : $[O]) => true, "face", "face:", "webp")(
        Face.all./(f => ImageAsset(f.toString.toLowerCase))
    ) ::
    ConditionalAssetsList((factions : $[F], options : $[O]) => true, "status", "status:", "webp")(
        $("pinned", "setup", "nofire", "slow", "retreat", "splash", "flag-us", "flag-wehrmacht")./(ImageAsset(_))
    ) ::
    ConditionalAssetsList((factions : $[F], options : $[O]) => true, "ops", "ops:", "webp")(
        (0.until(Operations.designs.num).$./("ops-" + _) :+ "ops-back")./(ImageAsset(_))
    ) ::
    ConditionalAssetsList((factions : $[F], options : $[O]) => true, "commander", "commander:", "webp", lzy = Laziness.OnDemand)(
        Commander.all./(c => ImageAsset(c.id))
    ) ::
    ConditionalAssetsList((factions : $[F], options : $[O]) => true, "board", "board:", "webp", lzy = Laziness.OnDemand)(
        Faction.all./~(f => 1.to(3)./(t => ImageAsset(Tiers.board(f, t))))
    ) ::
    $
}
