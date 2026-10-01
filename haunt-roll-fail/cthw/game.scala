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

import hrf.tracker._

import hrf.elem._
import cthw.elem._

trait Player extends hrf.base.BasePlayer with Elementary {
    def name : String
}

sealed abstract class Glyph(val inPlay : Boolean, val onMap : Boolean)

case object Ocean extends Glyph(true, true)
case object GlyphAA extends Glyph(true, true)
case object GlyphOO extends Glyph(true, true)
case object GlyphWW extends Glyph(true, true)
case object NoGlyph extends Glyph(true, true)
case object Pool extends Glyph(false, false)
case object Prison extends Glyph(false, false)
case object Deep extends Glyph(true, false)
case object Slumber extends Glyph(true, false)
case object Sorcery extends Glyph(true, false)

case class Region(name : String, glyph : Glyph) {
//    override def toString = if (glyph == Ocean) name.styled("sea") else if (glyph == Deep) GC.styled(name) else name.styled("region")
//    def +(ia : IceAges) = toString + ia.toString
}

trait Board {
    def name : String
    def regions : List[Region]
    def connected(region : Region) : List[Region]
    def starting(faction : Faction) : List[Region]
    def distance(a : Region, b : Region) : Int
}

object EarthMap4v35 extends Board {
    val name = "Earth Map (4 players 3/5 variant)"

    val ArcticOcean = Region("Arctic Ocean", Ocean)
    val Scandinavia = Region("Scandinavia", GlyphWW)
    val Europe = Region("Europe", GlyphWW)
    val NorthAsia = Region("North Asia", GlyphWW)
    val SouthAsia = Region("South Asia", GlyphWW)
    val Arabia = Region("Arabia", GlyphWW)
    val EastAfrica = Region("East Africa", GlyphAA)
    val WestAfrica = Region("West Africa", GlyphAA)
    val NorthAtlantic = Region("North Atlantic", Ocean)
    val SouthAtlantic = Region("South Atlantic", Ocean)
    val Antarctica = Region("Antarctica", NoGlyph)
    val SouthPacific = Region("South Pacific", Ocean)
    val SouthAmerica = Region("South America", GlyphOO)
    val NorthAmerica = Region("North America", GlyphOO)
    val NorthPacific = Region("North Pacific", Ocean)
    val IndianOcean = Region("Indian Ocean", Ocean)
    val Australia = Region("Australia", GlyphAA)

    val regions = List(ArcticOcean, NorthAtlantic, SouthAtlantic, NorthPacific, IndianOcean, SouthPacific, Scandinavia, Europe, NorthAsia, SouthAsia, Arabia, WestAfrica, EastAfrica, NorthAmerica, SouthAmerica, Australia, Antarctica)
    val west = List(ArcticOcean, NorthPacific, NorthAmerica, NorthAtlantic, Australia, SouthPacific, SouthAmerica, SouthAtlantic, Antarctica)
    val east = List(Scandinavia, Europe, NorthAsia, SouthAsia, Arabia, WestAfrica, EastAfrica, IndianOcean)

    def connected(region : Region) = region match {
        case ArcticOcean => List(NorthAmerica, NorthAtlantic, Scandinavia, NorthAsia, NorthPacific)
        case NorthAtlantic => List(NorthPacific, NorthAmerica, ArcticOcean, Scandinavia, Europe, Arabia, WestAfrica, SouthAtlantic, SouthAmerica)
        case SouthAtlantic => List(Antarctica, SouthPacific, SouthAmerica, NorthAtlantic, WestAfrica, EastAfrica, IndianOcean)
        case NorthPacific => List(ArcticOcean, NorthAmerica, NorthAtlantic, SouthAmerica, SouthPacific, IndianOcean, SouthAsia, NorthAsia)
        case IndianOcean => List(Antarctica, SouthAtlantic, EastAfrica, Arabia, SouthAsia, NorthPacific, SouthPacific, Australia)
        case SouthPacific => List(Antarctica, SouthAtlantic, SouthAmerica, NorthPacific, IndianOcean, Australia)
        case Scandinavia => List(Europe, NorthAtlantic, ArcticOcean, NorthAsia)
        case Europe => List(NorthAtlantic, Scandinavia, NorthAsia, Arabia)
        case NorthAsia => List(ArcticOcean, NorthPacific, SouthAsia, Arabia, Europe, Scandinavia)
        case SouthAsia => List(NorthAsia, NorthPacific, IndianOcean, Arabia)
        case Arabia => List(NorthAtlantic, Europe, NorthAsia, SouthAsia, IndianOcean, EastAfrica, WestAfrica)
        case WestAfrica => List(NorthAtlantic, Arabia, EastAfrica, SouthAtlantic)
        case EastAfrica => List(Arabia, IndianOcean, SouthAtlantic, WestAfrica)
        case NorthAmerica => List(ArcticOcean, NorthAtlantic, SouthAmerica, NorthPacific)
        case SouthAmerica => List(NorthAmerica, NorthAtlantic, SouthAtlantic, SouthPacific, NorthPacific)
        case Australia => List(SouthPacific, IndianOcean)
        case Antarctica => List(SouthPacific, SouthAtlantic, IndianOcean)
    }

    def distance(a : Region, b : Region) =
        if (a == b)
            0
        else
        if (connected(a).contains(b))
            1
        else
        if (connected(a)./~(connected).contains(b))
            2
        else
        if (connected(a)./~(connected)./~(connected).contains(b))
            3
        else
            4

    def starting(faction : Faction) = faction match {
        case _ => Nil
        /*
        case GC => List(SouthPacific)
        case CC => List(SouthAsia)
        case BG => List(WestAfrica)
        case YS => List(Europe)
        case SL => List(NorthAmerica)
        case WW => List(ArcticOcean, Antarctica)
        case OW => regions
        */
    }
}

sealed trait UnitType {
    def name = toString
    def plural = name + "s"
}
case object Cultist extends UnitType
case object Monster extends UnitType
case object GOO extends UnitType
case object Token extends UnitType

abstract class UnitClass(val name : String, val utype : UnitType, val cost : Int) {
    def plural = name + "s"
}

sealed abstract class Spellbook(val name : String) extends Record {
    def full : String
    override def toString = full
}

abstract class NeutralSpellbook(name : String) extends Spellbook(name) {
    override def full = name//.styled("nt")
}

abstract class FactionSpellbook(val faction : Faction, name : String) extends Spellbook(name) {
    override def full = faction.name//.styled(name)
}

abstract class Requirement(val text : String, val es : Int = 0)

trait UnitFigure
trait Effect
trait Figure


trait Faction extends Player with Record with Named with Styling {
    val expansion : Expansion
    def name : String
    def short : String
    def style : String
}

case object GC extends Faction {
    val expansion = null
    def name = "Great Cthulhu"
    def short = "GC"
    def style = "gc"
}

case object CC extends Faction {
    val expansion = null
    def name = "Crawling Chaos"
    def short = "CC"
    def style = "cc"
}

case object BG extends Faction {
    val expansion = null
    def name = "Black Goat"
    def short = "BG"
    def style = "bg"
}

case object YS extends Faction {
    val expansion = null
    def name = "Yellow Sign"
    def short = "YS"
    def style = "ys"
}

case object SL extends Faction {
    val expansion = null
    def name = "Sleeper"
    def short = "SL"
    def style = "sl"
}

case object WW extends Faction {
    val expansion = null
    def name = "Wind Walker"
    def short = "WW"
    def style = "ww"
}

case object OW extends Faction {
    val expansion = null
    def name = "Opener of the Way"
    def short = "OW"
    def style = "ow"
}

trait PlayerState {
    def game : Game
    //def has(e : Effect) = faction.abilities.contains(e) || effects.contains(e) || abilities.contains(e) || services.contains(e)
    //def can(e : Effect) = has(e) && !used.contains(e) && !ignored.contains(e)

    //def figures(name : String, init : List[Figure] = Nil, rule : Figure => Boolean = (e : Figure) => true) = null //l game.pieces.another[Figure](name + "-" + faction.short, init, rule)

    var scorelog : List[Elem] = Nil

    def abilities : List[Effect] = Nil
    var effects : List[Effect] = Nil
    var services : List[Effect] = Nil
    var used : List[Effect] = Nil
    var ignored : List[Effect] = Nil

    var vp : Int = 0

    def faction : Faction

    //val pool = figures("pool", faction.figures, _.faction == faction)
    //var limbo = figures("limbo", Nil, _.faction == faction)

    //def pooled(p : Piece) = pool.%(_.piece == p).num

    //def inpool(p : Piece) = pool.%(_.piece == p).any

    //def all(p : Piece) = game.board.clearings./~(c => game.xclearings(c).%(_.faction == faction).%(_.piece == p)./(_ => c))

}

trait NoGameOverTrigger { self : Action => }

case class StartAction(version : String) extends StartGameAction with GameVersion

trait Expansion {
    def perform(game : Game, action : Action) : Continue
}

class Game(val players : List[Player], val setup : List[Faction], val options : List[Meta.O]) extends BaseGame with ContinueGame with LoggedGame { game =>
    def isOver = false

    val expansions = setup./(_.expansion).distinct

    var pstates = Map[Faction, PlayerState]()
    var factions : List[Faction] = Nil
    var gameover : GameOver = null

    implicit val implicitGame = game

    setup.foreach(f => pstates += f -> new PlayerState() { def faction = f; def game = implicitGame })

    def info(waiting : List[Faction], self : Option[Faction], actions : List[UserAction]) : $[Info] = Nil

    def loggedPerform(action : Action, soft : Void) : Continue = {
        // +++("> " + action)

        val c = performInternal(action)

        c match {
            case Ask(_, Nil) =>
                println("")
                println("")
                println("")
                println("WTF!!!")
                println("Empty Ask as a result of " + action)
            case _ =>
        }

        // +++("    < " + c)

        if (pstates.size == factions.num && !action.is[NoGameOverTrigger]) {
            if (gameover != null)
                return gameover

                /*
            val ww = factions.%(f => f.vp >= 30)
            if (ww.any) {
                val cw = factions.%(f => ww.%(w => f.coalition == Some(w)).any)
                val winners = ww ++ cw

                highlights :+= NothingHighlight
                highlights :+= NothingHighlight
                highlights :+= NothingHighlight

                log(winners./(_.elem).join(", "), "won")

                gameover = GameOver(winners, "Game Over" ~ Break ~ winners./(f => f.elem ~ " won" ~ Break ~ (of(f).vp >= 0).??(of(f).scorelog.join(Break))).join(Break), (null +: Nil)./(f => {
                    Ask(f)(winners./~(w =>
                        $(GameOverWonAction(f, w)) ++ (of(w).vp >= 0).?(GameOverTextAction(f, of(w).scorelog.join(HorizontalBreak)))
                        ) ++ $(GameOverMapsAction(f)) ++ hrf.HRF.flag("offline").not.?(GameOverSaveReplayAction)
                    )
                }))

                return gameover
            }
            */
        }

        /*
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        highlightFaction = c match {
            case Ask(f, _, _) => $(f)
            case MultiAsk(a) => a./(_.faction)
            case _ => Nil
        }
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        */

        def fix(f : Faction) = f

        c @@ {
            case c : Ask => c.copy(faction = fix(c.faction))
            case c : MultiAsk => c.copy(asks = c.asks./(c => c.copy(faction = fix(c.faction))))
            case c => c
        }
    }


    def performInternal(action : Action) : Continue = {
        val cc = expansions./(_.perform(this, action)).but(UnknownContinue)

        if (cc.num > 1)
            if (cc.distinct.num > 1 && action.is[SideEffectOnly].not)
                throw new Error("Conflicting continue on " + action)

        if (cc.any) {
            cc(0)
        }
        else action match {
            // INIT
            case StartAction(version) =>
                log("HRF".hl, "version", gaming.version.hlb)
                log("Cthulhu Wars".hlb)

                if (version != gaming.version)
                    log("Saved game version", version.hlb)

                if (players.of[Faction].any)
                    log("Factions", players.of[Faction]./(_.elem).comma)

                options.foreach { o =>
                    log(o.group, o.valueOn)
                }

                Milestone(null)
        }
    }
}
