package hrf.meta
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

import hrf.base._
import hrf.elem._
import hrf.options._

trait Laziness

object Laziness {
    case object Immediate extends Laziness
    case object Later extends Laziness
    case object OnDemand extends Laziness
}

case class ImageAsset(name : String, path : String, filename : String, ext : String, lzy : Laziness, scale : Double, lossless : Boolean) {
    def src = path + (path != "").??("/") + filename + (ext != "").??(".") + ext
    def makeLossless = copy(lossless = true)
    def scaled(s : Double) = copy(scale = s)
}

object ImageAsset {
    def apply(name : String) : ImageAsset = ImageAsset(name, "", name, "", Laziness.Immediate, 100, false)
    def apply(name : String, filename : String) : ImageAsset = ImageAsset(name, "", filename, "", Laziness.Immediate, 100, false)
    def apply(name : String, filename : String, ext : String) : ImageAsset = ImageAsset(name, "", filename, ext, Laziness.Immediate, 100, false)
    def apply(name : String, filename : String, ext : String, lzy : Laziness) : ImageAsset = ImageAsset(name, "", filename, ext, lzy, 100, false)
    def apply(name : String, filename : String, ext : String, lzy : Laziness, scale : Int) : ImageAsset = ImageAsset(name, "", filename, ext, lzy, scale, false)
    def apply(name : String, filename : String, ext : String, lzy : Laziness, scale : Int, lossless : Boolean) : ImageAsset = ImageAsset(name, "", filename, ext, lzy, scale, lossless)
}

abstract class ValidationResult(val ok : Boolean, val message : String, val style : Style) extends GoodMatch {
    def &&(v : ValidationResult) : ValidationResult = this @@ {
        case r : ErrorResult => r
        case r =>
            v @@ {
                case v : ErrorResult => v
                case v =>
                    r @@ {
                        case r : WarningResult => r
                        case r =>
                            v @@ {
                                case v : WarningResult => v
                                case v => v
                            }
                    }
            }
    }

    def &&(v : |[ValidationResult]) : ValidationResult = v./(this && _).|(this)
}

case class InfoResult(m : String) extends ValidationResult(true, m, xstyles.highlight)
case class WarningResult(m : String) extends ValidationResult(true, m, xstyles.warning)
case class ErrorResult(m : String) extends ValidationResult(false, m, xstyles.error)


trait GameOption extends BaseOption

// RANKED OR UNRANKED (asked for on 2026-11-22): the setup choice of an online game of a meta with a ranking (Root's, Northgard's),
// offered only online (startSetup in hrf.scala). An unranked game is reported as a fan game: it counts in the stats, not in the
// ratings or win rates (owner, 2026-11-22); games made before it have neither and count as before
trait RankingChoice extends GameOption {
    val ranked : Boolean
}

trait MetaBase {
    val gaming : Gaming

    type G = gaming.G
    type F <: gaming.F
    type O = GameOption

    implicit def tagF : scala.reflect.ClassTag[F]

    import gaming._

    val name : String
    val label : String

    val recommendedVersion : |[String] = None

    def settingsKey : String = name
    def settingsList : $[hrf.Setting] =
        $(hrf.ShowTips, hrf.HideTips) ++
        $(hrf.CondensedButtonSpacing, hrf.NormalSpacing, hrf.ExpandedButtonSpacing) ++
        $(hrf.GameFontFace, hrf.CodeFontFace, hrf.SystemFontFace) ++
        $(hrf.SmallerFontSize, hrf.SmallFontSize, hrf.NormalFontSize, hrf.LargeFontSize, hrf.LargerFontSize) ++
        $(hrf.AlwaysFullScreen, hrf.TripleClickFullScreen, hrf.NeverFullScreen) ++
        $(hrf.SlowestScrollSpeed, hrf.SlowerScrollSpeed, hrf.SlowScrollSpeed, hrf.NormalScrollSpeed, hrf.FastScrollSpeed, hrf.FasterScrollSpeed, hrf.FastestScrollSpeed)

    // (name, saved setting, its replacement): each browser swaps the saved setting once, remembering it as <settingsKey>.moved.<name>,
    // for a default that changed after players had saved the old one
    def settingsMoves : $[(String, String, String)] = $

    def settingsDefaults : $[hrf.Setting] = $(hrf.ShowTips, hrf.NormalSpacing, hrf.GameFontFace, hrf.NormalFontSize, hrf.TripleClickFullScreen, hrf.NormalScrollSpeed)

    // the display language: (name, text, image id), see hrf.elem.Translation
    def translation(settings : $[hrf.Setting]) : (String, String => String, String => String) = ("", identity, identity)

    val about : $[Elem] = $

    val showAbout : Boolean = true

    val underConstruction : Boolean = false

    val factions : $[F]

    def factionGroup(f : F) : |[Elem] = None
    // The picker's groups (by their text) that fold under their heading, folded until opened (Folds)
    def foldedFactionGroups : $[String] = $

    val options : $[O]
    def optionsFor(n : Int, l : $[F]) : $[O] = options
    def optionPages(n : Int, l : $[F]) : $[$[O]] = $(optionsFor(n, l))
    def presetsFor(n : Int, l : $[F]) : $[(Elem, $[O], $[O])] = $()
    // What each preset sets, in the order of presetsFor, shown by a ? on its button (none when empty)
    def presetsInfo(n : Int, l : $[F]) : $[$[Elem]] = $
    // A ? on a setup section's heading that explains the section as a whole (given any of its options; none when empty)
    def groupInfo(o : O) : $[Elem] = $
    def defaultsFor(n : Int, l : $[F]) : $[O] = $
    def mandatoryFor(n : Int, l : $[F]) : $[O] = $
    def hiddenOptions : $[O] = $
    // Options a game gets from its bots' difficulties (bots that play by other rules); added by startGame, never saved
    def botOptions(seating : $[F], bots : Map[F, String]) : $[O] = $
    // The setup screen's options as the new game gets them: random choices (Root's Random map) are drawn here, once, before the game is created
    def startOptions(options : $[O]) : $[O] = options
    // Whether an option is listed on the setup screen with the current choices; used to show options only when they apply
    def optionShown(o : O, selected : $[O]) : Boolean = true
    // Whether a foldable section (BaseOption.fold) is shown open even when folded, e.g. while it holds a mistake to fix
    def optionFoldOpen(fold : String, factions : $[F], selected : $[O]) : Boolean = false

    val indistinguishableFactions : Boolean = false
    val gradualFactions : Boolean = false
    val insignificantSeating : Boolean = false

    def randomGameName() : String

    def validateFactionCombination(factions : $[F]) : ValidationResult
    def validateFactionSeatingOptions(factions : $[F], options : $[O]) : ValidationResult

    def minPlayers : Int
    def maxPlayers : Int = factions.num

    def randomQuickGame() : (F, $[F], Map[F, String], $[O])

    def factionName(f : F) : String
    def factionElem(f : F) : Elem
    // The faction's name on the rankings page, in the faction's own colour when it has one (the bars take it too)
    def rankingElem(f : F) : Elem = factionElem(f)
    // The rankings page's tiers, best first (none: no tiers); a tier's icon is one of the game's images
    def rankingTiers : $[RankingTier] = $
    // what a tier is called ("Tier", Northgard's "League")
    def rankingTierWord : String = "Tier"
    // titles by wins, lowest first (Northgard's player ranks)
    def rankingTitles : $[(String, Int)] = $
    // the ladder and stats can be filtered by mode (1v1, 2v2, free-for-all, teams), as Northgard's ranked play is
    def rankingModes : Boolean = false
    def factionNote(f : F) : Elem = Empty
    // The faction picker shows the factions as a grid of tiles when this gives one (Northgard: the clan's emblem
    // and name), each with the factionInfo button, instead of rows with factionElem and factionNote
    def factionTile(f : F) : |[Elem] = None
    // The reworked setup screens (Root, its Advanced Setup, Northgard): tiles, sections folded to a line each, the buttons kept in view
    def setupDesign : Boolean = false
    // what the picker and the setup rows call the factions
    def factionsWord : String = "factions"
    def setupRowsLabel : String = "Factions"
    // the faction's icon on its setup row
    def factionRowIcon(f : F) : Elem = Empty
    // an option shown as a tile in a grid (Advanced Setup's factions)
    def optionTile(o : O) : |[Elem] = None
    // a tile twice as wide, for an option shown with a picture
    def optionTileWide(o : O) : Boolean = false
    // a line under the setup rows about the seating order
    def seatingNote(selected : $[O]) : Elem = Empty
    // Random picks offered in the faction picker: the label (or tile) and the factions to pick from. The faction is drawn when
    // the pick is made, among those not already in the game, and stays hidden (shown as randomName) until the game starts.
    def randomFactions : $[(Elem, $[F])] = $
    def randomName : String = "Random"
    // A faction's variant picked as its own entry in the faction picker (Northgard: Robotos's Dragon): the faction, its name on the
    // setup screen, its tile (or row), its look once picked, its info pop-up (as factionInfo), its picker group (as factionGroup), and
    // the options picking it brings to the setup screen, always on there like the picker mode's
    case class FactionVariant(faction : F, name : String, tile : Elem, chosen : Elem, info : |[(Elem, Elem, $[Elem])], group : |[Elem], options : $[O])
    def factionVariants : $[FactionVariant] = $
    // A faction already picked for the game, in the faction picker
    def factionChosenElem(f : F) : Elem = factionElem(f).spn(xstyles.bold) ~ factionNote(f)

    // Options chosen on the faction's own row of the setup screen (like a player color): the current one is shown, clicking changes it.
    // They should be left out of optionPages.
    def factionRowOptions(f : F, l : $[F]) : $[O] = $
    def factionRowNone : Elem = "None".txt
    // More buttons on the faction's row, after the factionRowOptions one, each cycling through its options; selected is what the
    // game has so far, the picker mode included (Northgard: the player's team, in a team game)
    def factionRowMore(f : F, l : $[F], selected : $[O]) : $[$[O]] = $
    // Choices offered above the faction picker, one of them always picked (Northgard: free-for-all or teams): each one's label
    // and the options it brings to the setup screen, where they are always on; the first is the default
    def pickerModes : $[(Elem, $[O])] = $
    // The options restored from the last setup with as many players, completed for these factions (Northgard: colors
    // for the clans that weren't in that game)
    def completeSaved(n : Int, l : $[F], options : $[O]) : $[O] = options
    // The options to turn on, in order, when the row is clicked; by default the next one
    def factionRowClick(f : F, l : $[F], selected : $[O]) : $[O] = {
        val all = factionRowOptions(f, l)
        (all ++ all).dropWhile(o => selected.has(o).not).drop(1).take(1).some.|(all.take(1))
    }

    // A button next to a faction in the faction picker that opens a pop-up about it: the button's label, the pop-up's title and contents
    def factionInfo(f : F) : |[(Elem, Elem, $[Elem])] = None

    // A faction only bots can play (Northgard's Automa): always set to its default bot
    def botOnly(f : F) : Boolean = false

    // A solo opponent (Northgard's Automa): the main menu offers a solo game against it
    def soloFaction : |[F] = None

    // Other ways to play on the main menu (Northgard's Training Grounds), each with a page offering a local or an online game:
    // the menu label and the URL segment
    def modes : $[(String, String)] = $
    // A line about the mode on its page, the factions it is played with (in seating order) and the options it always has on
    def modeAbout(mode : String) : Elem = Empty
    def modeFactions(mode : String) : $[F] = $
    def modeOptions(mode : String) : $[O] = $
    // Rankings (good-game/Ranking.scala, Rankings in hrf.scala): the ladder a finished online game goes to (metas with the same
    // one share it, as Northgard and its Adset do), and its sides (a player, or a team) with whether each won; bots are left out later
    def ranking : |[String] = None
    // The site's tournament (good-game/Tournament.scala, tournamentMenu in hrf.scala; Root's, 2026-11-20): the meta its games are
    // played in, shown as "Tournament" on this meta's menu, and the options that meta gives a tournament game
    def tournament : |[String] = None
    def tournamentOptions : $[O] = $
    // Discord LFG games (good-game/Lfg.scala, 2026-11-20): the meta a game started from a Discord LFG post is played in, and the
    // options the site gives a new game of it by default (before startOptions draws the map), sent to the server by Play Online
    def lfgOptions : $[O] = $
    // The player counts a game started from Discord can have (none: no Discord setup), and the options to pick at least as many of
    // as the players plus k (Root's Advanced Setup: the factions in the draft, "IncludeFaction(" and 0)
    def lfgPlayers : $[Int] = $
    def lfgNeed : |[(String, Int)] = None
    // Every setup option, for the Discord setup panel (good-game/Lfg.scala; 2026-11-20): tab-separated lines the server turns into
    // menus, so the options are never written out on the server. For each player count, the mandatory options, the defaults and
    // the presets; for each option, its page on the setup screen (-1: not listed), the player counts it's offered for, whether it
    // toggles, whether it's listed only once what it needs is picked (optionShown), whether it's Important, its section and its
    // name; then how options turn each other on and off (BaseOption's required, blocked, forcedOn, alsoOn, forcedOff, clear)
    def lfgCatalog : $[String] = {
        def w(l : $[BaseOption]) = l./(o => writeOption(o.asInstanceOf[O])).join(" ")
        def alts(l : $[$[BaseOption]]) = l.num + "\t" + l./(w).join("|")
        def seats(n : Int) = factions.take(n)
        def clean(s : String) = s.replace("\t", " ").replace("\n", " ").trim
        val top = lfgPlayers.max
        val all = optionsFor(top, seats(top))
        val pages = optionPages(top, seats(top))./(_.intersect(all))
        def page(o : O) = hiddenOptions.contains(o).?(-1).|(pages.indexWhere(_.contains(o)))
        val base = all./(o => o : BaseOption)

        $("players\t" + lfgPlayers.mkString(" ")) ++
        lfgNeed./{ case (p, k) => "need\t" + p + "\t" + k }.$ ++
        lfgPlayers./~{ n =>
            val l = seats(n)
            $("mandatory\t" + n + "\t" + w(mandatoryFor(n, l)), "defaults\t" + n + "\t" + w(completeSaved(n, l, defaultsFor(n, l)))) ++
            presetsFor(n, l)./{ case (title, included, excluded) => "preset\t" + n + "\t" + clean(title.text) + "\t" + w(included ++ defaultsFor(n, l).diff(excluded)) }
        } ++
        all./~{ o =>
            val n = writeOption(o)
            $("option\t" + n + "\t" + page(o) + "\t" + lfgPlayers.%(k => optionsFor(k, seats(k)).contains(o)).mkString(",") + "\t" + o.toggle.?(1).|(0) + "\t" +
                optionShown(o, $).?(0).|(1) + "\t" + o.is[ImportantOption].?(1).|(0) + "\t" + clean(o.group.text) + "\t" + clean(o.valueOn.text) + "\t" + rankingUnapproved(o).?(1).|(0),
                "required\t" + n + "\t" + alts(o.required(base)),
                "blocked\t" + n + "\t" + alts(o.blocked(base)),
                "on\t" + n + "\t" + w(o.forcedOn(base)),
                "also\t" + n + "\t" + w(o.alsoOn(base)),
                "off\t" + n + "\t" + w(o.forcedOff(base)),
                "clear\t" + n + "\t" + w(o.clear(base)))
        }
    }
    // the seat a turn report names (good-game/Turns.scala), as the lobby's "user <seat> <user>" lines write it: the faction asked,
    // unless the game's seats aren't its factions (Root's Advanced Setup: a seat drafts its faction in the game)
    def turnSeat(g : G, f : Any) : String = writeFaction(f.asInstanceOf[F])
    // why that seat is asked, for its Discord turn ping (good-game/TurnPings.scala; asked for on 2026-11-21): a short tag ("defend",
    // "trade", ...), with "/<faction>" for a ping showing the faction's icon, or "" for none; never anything only the seat may know
    def turnReason(g : G, f : Any, actions : $[Any]) : String = ""
    // what happened to whom lately, for the Roast Me heckler's jests (good-game/Heckler.scala; Root's root/jests.scala): the last
    // events, numbered from the game's first, "<n>:<tag>/<seat>/..."; display only
    def jests(g : G) : $[String] = $
    // Play Online's games list (2026-11-22): icons summing up a game, its map and then its factions. An icon is an image name: a
    // faction's glyph (glyph(f)) or "lfg:<name>", a picture of webp2/lfg/emoji (the Discord emojis', lfg-emoji.py). overviewIcons
    // names the options (as writeOption writes them) that have one, the map first. overview is reported by a client playing the game
    // (with the turn, good-game/Turns.scala), overviewLobby worked out from the lobby's seats and options for a game nobody has
    // reported yet (Advanced Setup's seats draft their factions in the game, so it gives none)
    def overviewIcons : $[(String, String)] = $
    def overviewOptions(options : $[String]) : $[String] = overviewIcons.%((o, _) => options.has(o))./((_, i) => "lfg:" + i).distinct
    def overview(g : G) : $[String] = $
    // the game's progress, for the tournament's status posts (good-game/Tournament.scala; 2026-11-22): "r<round>" (0 before the
    // first turn), then "<seat>:<faction name, spaces as _>:<score>" for each faction, the seat as turnSeat writes it
    def status(g : G) : $[String] = $
    def overviewLobby(factions : $[String], options : $[String]) : $[String] = overviewOptions(options) ++ factions./~(parseFaction)./~(f => glyph(f)).distinct
    def rankingSides(g : G, winners : $[Any]) : $[($[F], Boolean)] = $
    // and, for the faction and turn order stats, what each seat played: its faction, its place in the turn order (from 1), the
    // number of players and its score (VP or fame, "D" for a Root dominance)
    def rankingSeat(g : G, f : F) : |[RankingSeat] = None
    // a game played with fan-made content: kept in the stats, left out of the ratings (asked for on 2026-11-18)
    def rankingFan(g : G) : Boolean = false
    // an option not approved for ranked play (2026-11-22): marked "unranked" on the setup screen and in the Discord setup panel, and a
    // ranked game can't be started with it (validateFactionSeatingOptions)
    def rankingUnapproved(o : O) : Boolean = false
    // a draft's factions no seat took (2026-11-22, asked for: Advanced Setup's offered factions, for pick rates): "U" and the
    // faction, as rankingSeat names it, for one offered and not picked, "B" for one banned from the draft (Northgard's Adset)
    def rankingDraft(g : G) : $[(String, String)] = $
    // the game's landmarks and hirelings, for the stats (2026-11-23, asked for): "K" and a landmark's name, "H" and a hireling's
    def rankingSetup(g : G) : $[(String, String)] = $
    // Other games on the main menu that are their own meta (Northgard's Adset): the menu label and the meta's name
    def linkedModes : $[(String, String)] = $
    // A tutorial on the main menu (Northgard's): a line about it, its lessons (each a title and its paragraphs), and the
    // practice game it ends with: the seating, the bots by faction (the rest are played by the person) and the options
    def tutorialAbout : Elem = Empty
    def tutorialLessons : $[(Elem, $[Elem])] = $
    def tutorialGame : |[($[F], Map[F, String], $[O])] = None
    // A note at the top of the main menu (Northgard's Adset: its rules in short)
    def menuAbout : |[Elem] = None
    // A "How to Play" page on the game's menu (Pincer and Company of Heroes, 2026-11-24): links to the publisher's own pages for
    // the rules (label and URL), then how the game is played on this site, a heading and its paragraphs each; never the rules
    def howToPlayLinks : $[(Elem, String)] = $
    def howToPlay : $[(Elem, $[Elem])] = $

    // Image name -> URL for images used in the menus (faction notes, option explanations), before the game's assets load
    def menuImages : Map[String, String] = Map()

    def glyph() : |[String] = None
    def glyph(g : G) : |[String] = glyph()
    def glyph(f : F) : |[String] = glyph()
    def glyph(g : G, f : F) : |[String] = glyph(f).||(glyph(g))

    def bodyFont : |[String] = None
    def bodyFontSupportsItalic : Boolean = false
    def titleFont : |[String] = bodyFont

    def createGame(factions : $[F], options : $[O]) : G

    val start : ExternalAction

    def writeFaction(f : F) : String
    def parseFaction(s : String) : |[F]

    def writeOption(f : O) : String
    def parseOption(s : String) : $[O]

    def parseActionExternal(s : String) : ExternalAction = {
        if (s.startsWith("// "))
            CommentAction(s.drop("// ".length))
        else
            parseAction(s) match {
                case a : ExternalAction => a
                case a : ForcedAction => DoAction(a)
            }
    }

    def writeActionExternal(a : ExternalAction) : String = {
        writeAction(a.unwrap)
    }

    def parseAction(s : String) : Action
    def writeAction(a : Action) : String

    case class ConditionalAssetsList(condition : ($[F], $[O]) => Boolean, path : String = "", prefix : String = "", ext : String = "png", lzy : Laziness = Laziness.Immediate, scale : Double = 100, lossless : Boolean = false)(val list : $[ImageAsset]) {
        def get = {
            var r = list

            if (path != "")
                r = r./(a => (a.path == "").?(a.copy(path = path)).|(a))

            if (ext != "")
                r = r./(a => (a.ext == "").?(a.copy(ext = ext)).|(a))

            if (prefix != "")
                r = r./(a => a.copy(name = prefix + a.name))

            if (lzy != Laziness.Immediate)
                r = r./(a => a.copy(lzy = lzy))

            if (scale != 100)
                r = r./(a => a.copy(scale = scale))

            if (lossless)
                r = r./(a => a.copy(lossless = true))

            r
        }
    }

    def path = name

    // Assets a game may well not need (Root's other maps): loaded at once only if their condition holds, otherwise when first drawn
    def deferrable(a : ConditionalAssetsList) : Boolean = false

    val assets : $[ConditionalAssetsList]

    def intLinks : $[(Elem, String)] = $

    def extLinksName : Elem = "External Links"

    def extLinks : $[(Elem, String)] = $(
        // "If you want to support HRF development, please help bring this project to live" ~ Break ~ 
        // ("Straytones: Live At 24:1900 Festival - Vinyl Run Kickstarter".hl -> "https://www.kickstarter.com/projects/122musicmanagement/straytones-live-at-24-1900-festival-vinyl-run-kickstarter"),
    )

    def support : $[(String, String)] = $

    def tips : $[Elem] = $(
        "You can disable tooltips in the settings.",
        "You can change font size in the settings.",
        "You can change button size in the settings.",
        "Click on any line in the log to undo to that moment in the game.",
        "Ask other players before undoing dice rolls or card reveal.",
        "You can undo even if it's not your turn.",
        "You can undo another undo, and return to the original timeline.",
    )

}

trait MetaBots extends MetaBase {
    import hrf.bot._

    val gaming : BotGaming

    import gaming._

    def getBots(f : F) : $[String]
    def getBot(f : F, bot : String) : gaming.Bot
    def defaultBot(f : F) : String

    val quickMin : Int
    val quickMax : Int
    def quickFactions = factions
    def quickOptions = options./(_ -> 0.5).toMap

    def randomQuickGame() : (F, $[F], Map[F, String], $[O]) = {
        1000.timesDo { () =>
            val n = randomInRange(quickMin, quickMax)
            // (a combination that fits, before the options, which take far longer)
            val l = Iterator.continually(quickFactions.shuffle.take(n)).take(200).find(x => validateFactionCombination(x).is[ErrorResult].not).getOrElse(quickFactions.shuffle.take(n))
            val f = l.shuffle.first
            // (a local game is never ranked)
            val all = optionsFor(n, l).%(_.is[RankingChoice].not)
            val defaults = defaultsFor(n, l).%(_.is[RankingChoice].not)
            val mandatory = mandatoryFor(n, l)

            val o = all./~(o => quickOptions.get(o).%(_ > random())./(_ => o))
            val state = OptionsState(all, mandatory, o.shuffle, defaults).checkDimmed()
            val r = state.selected

            validateFactionSeatingOptions(l, r) @@ {
                case ErrorResult(_) =>
                case _ =>
                    val d = l.but(f)./(e => e -> defaultBot(e)).toMap
                    // as New Local Game starts (without startOptions a random map stayed unpicked, and the game failed to start)
                    return (f, l, d, startOptions(state.actual))
            }
        }

        throw new Error("could not create a valid quick game setup")
    }
}


// A tier of the rankings page (MetaGame.rankingTiers): its name, the lowest rating in it (None: the bottom tier), a line about it
// and its icon among the game's images
case class RankingTier(name : String, min : |[Double], note : String, icon : |[String])

// A seat of a finished ranked game (MetaGame.rankingSeat)
object RankingSeat {
    // a faction's name as a report writes it, spaces as "_" (also the draft's "U:" and "B:" lists, MetaGame.rankingDraft)
    def name(faction : String) = faction.replace(' ', '_').toList.filter(c => c.isLetterOrDigit || "_'().-!&".contains(c)).mkString.take(48)
    // the kinds of those lists: the draft's left (U) and banned (B), the landmarks (K), the hirelings (H) and the map (M; 2026-11-23) (not "L", a losing side)
    val kinds = $("U", "B", "K", "H", "M")
    // what a report adds for a draft and the setup: ";U:name,name;B:name;K:name;H:name;M:map"
    def draft(l : $[(String, String)]) : String = kinds./{ k => val nn = l.filter(_._1 == k).map(x => name(x._2)).filter(_.nonEmpty) ; nn.any.??(";" + k + ":" + nn.mkString(",")) }.mkString
}

case class RankingSeat(faction : String, seat : Int, players : Int, score : |[String]) {
    // what a report adds to the seat's user: "/faction/seat/players/score", spaces in the faction as "_" (Elo.entry in hrf.scala)
    def token = "/" + RankingSeat.name(faction) + "/" + seat + "/" + players + "/" + score.|("").toList.filter(_.isLetterOrDigit).mkString.take(8)
}

trait MetaGame extends MetaBase with MetaBots
