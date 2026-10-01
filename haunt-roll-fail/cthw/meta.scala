package cthw
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

import cthw.gaming._

import hrf.meta._
import hrf.elem._
import hrf.reflect._

object Meta extends MetaGame {
    val gaming = cthw.gaming

    type F = Faction

    def tagF = implicitly

    val name = "cthw"
    val label = "Cthulhu Wars"

    val factions = $(GC, CC, BG, YS)

    val minPlayers = 3

    val options = $[O]()

    val quickMin = 4
    val quickMax = 4

    def randomGameName() = {
        val n = $("Power", "Victory", "Glory", "Destiny", "Might", "Fight", "Right", "Betrayal", "Wood", "Land", "Air", "Ground").shuffle
        val c = $("for", "against", "versus", "through", "and", "of", "in", "as").shuffle
        n.head + " " + c.head + " " + n.last
    }

    def validateFactionCombination(factions : $[Faction]) = {
        if (factions.num < 2)
            ErrorResult("Select at least two factions")
        else
        if (factions.num > 5)
            ErrorResult("Max five factions")
        else
            InfoResult("~~~")
    }

    def validateFactionSeatingOptions(factions : $[Faction], options : $[O]) = InfoResult("~~~")

    def factionName(f : Faction) = f.name
    def factionElem(f : Faction) = f.name.styled(f)
//    override def factionNote(f : Faction) : Elem = f.note

////    override def factionGlyph(f : F) = Some(f.style + "-glyph")

    def createGame(factions : $[Faction], options : $[O]) = new Game(factions, factions, options)

    def getBots(f : Faction) = f match {
        case _ => $("Normal", "Easy")
    }

    def getBot(f : Faction, b : String) = null /* (f, b) match {
        case (f : Faction, "Easy") => new BotXX(f)
        case (f : Faction, "Normal") => new BotXX(f)
    } */

    def defaultBot(f : Faction) = "Normal"

    def writeFaction(f : Faction) = f.short
    def parseFaction(s : String) = factions.%(_.short == s).single

    def writeOption(o : O) = o.toString
    def parseOption(s : String) = options.%(o => o.toString == s)

    def parseAction(s : String) : Action = { println(s); Serialize.parseAction(s) }
    def writeAction(a : Action) : String = Serialize.write(a)

    val start = StartAction(gaming.version)

    override def bodyFont = Some("bohemian-typewriter")

    val assets =
    ConditionalAssetsList((factions : $[Faction], options : $[O]) => true, "images")(
        ImageAsset("an-acolyte") ::
        ImageAsset("an-cathedral") ::
        ImageAsset("an-glyph") ::
        ImageAsset("an-reanimated") ::
        ImageAsset("an-un-man") ::
        ImageAsset("an-yothan") ::
        ImageAsset("bg-acolyte") ::
        ImageAsset("bg-dark-young") ::
        ImageAsset("bg-fungi") ::
        ImageAsset("bg-ghoul") ::
        ImageAsset("bg-glyph") ::
        ImageAsset("bg-shub") ::
        ImageAsset("cc-acolyte") ::
        ImageAsset("cc-flying-polyp") ::
        ImageAsset("cc-glyph") ::
        ImageAsset("cc-hunting-horror") ::
        ImageAsset("cc-nightgaunt") ::
        ImageAsset("cc-nyarly") ::
        ImageAsset("earth33-bright") ::
        ImageAsset("earth33-dark") ::
        ImageAsset("earth33-place").makeLossless ::
        ImageAsset("earth35-bright") ::
        ImageAsset("earth35-dark") ::
        ImageAsset("earth35-place").makeLossless ::
        ImageAsset("earth53-bright") ::
        ImageAsset("earth53-dark") ::
        ImageAsset("earth53-place").makeLossless ::
        ImageAsset("earth55-bright") ::
        ImageAsset("earth55-dark") ::
        ImageAsset("earth55-place").makeLossless ::
        ImageAsset("gate") ::
        ImageAsset("gc-acolyte") ::
        ImageAsset("gc-cthulhu") ::
        ImageAsset("gc-deep-one") ::
        ImageAsset("gc-glyph") ::
        ImageAsset("gc-shoggoth") ::
        ImageAsset("gc-starspawn") ::
        ImageAsset("kill") ::
        ImageAsset("n-ghast") ::
        ImageAsset("n-gug") ::
        ImageAsset("n-shantak") ::
        ImageAsset("n-star-vampire") ::
        ImageAsset("ow-abomination") ::
        ImageAsset("ow-acolyte") ::
        ImageAsset("ow-glyph") ::
        ImageAsset("ow-mutant") ::
        ImageAsset("ow-spawn-of-yog-sothoth") ::
        ImageAsset("ow-yog-sothoth") ::
        ImageAsset("pain") ::
        ImageAsset("sl-acolyte") ::
        ImageAsset("sl-formless-spawn") ::
        ImageAsset("sl-glyph") ::
        ImageAsset("sl-serpent-man") ::
        ImageAsset("sl-tsathoggua") ::
        ImageAsset("sl-wizard") ::
        ImageAsset("ww-acolyte") ::
        ImageAsset("ww-glyph") ::
        ImageAsset("ww-gnoph-keh") ::
        ImageAsset("ww-ice-age") ::
        ImageAsset("ww-ithaqua") ::
        ImageAsset("ww-rhan-tegoth") ::
        ImageAsset("ww-wendigo") ::
        ImageAsset("ys-acolyte") ::
        ImageAsset("ys-byakhee") ::
        ImageAsset("ys-desecration") ::
        ImageAsset("ys-glyph") ::
        ImageAsset("ys-hastur") ::
        ImageAsset("ys-king-in-yellow") ::
        ImageAsset("ys-undead") ::
    $) ::
    ConditionalAssetsList((factions : $[Faction], options : $[O]) => true, "info")(
        ImageAsset("an-header") ::
        ImageAsset("bg-header") ::
        ImageAsset("cc-header") ::
        ImageAsset("faction-board-unique-ability-border") ::
        ImageAsset("gc-header") ::
        ImageAsset("header-border") ::
        ImageAsset("loyalty-card") ::
        ImageAsset("ow-header") ::
        ImageAsset("question-mark") ::
        ImageAsset("requirement") ::
        ImageAsset("sign-aa") ::
        ImageAsset("sign-oo") ::
        ImageAsset("sign-ww") ::
        ImageAsset("sl-header") ::
        ImageAsset("spellbook") ::
        ImageAsset("units-separator") ::
        ImageAsset("ww-header") ::
        ImageAsset("ys-header") ::
    $) ::
    ConditionalAssetsList((factions : $[Faction], options : $[O]) => true, "info", ext = "jpg")(
        ImageAsset("an-background") ::
        ImageAsset("bg-background") ::
        ImageAsset("cc-background") ::
        ImageAsset("gc-background") ::
        ImageAsset("ow-background") ::
        ImageAsset("sl-background") ::
        ImageAsset("ww-background") ::
        ImageAsset("ys-background") ::
    $) ::
    $
}
