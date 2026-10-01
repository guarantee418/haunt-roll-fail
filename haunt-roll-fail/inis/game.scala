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

import hrf.tracker4._
import hrf.tracker4.implicits._
import hrf.elem._

import inis.elem._

import scala.collection.immutable.ListMap


trait Faction extends NamedToString with Styling with Elementary with BasePlayer with Record {
    def short = name
    def style = name.toLowerCase
    override def elem : Elem = name.styled(this)(styles.title)(xstyles.bold)
}


case object Blue extends Faction
case object Green extends Faction
case object Orange extends Faction
case object White extends Faction

case object Factions {
    def all : $[Faction] = $(Blue, Green, Orange, White)
}

case class Figure(faction : Faction, index : Int) extends Record

trait Region {
    def -->(f : Faction)(implicit tracker : IdentityTracker[Region, Figure]) = tracker.get(this).%(_.faction == f).take(1).single.|!(f.name + " not found in " + this)
}

case class Reserve(faction : Faction) extends Region
case class Eliminated(faction : Faction) extends Region


trait GameImplicits {
    implicit def factionToState(f : Faction)(implicit game : Game) : FactionState = game.states(f).as[FactionState].get

    // implicit def regionToContent(r : Region)(implicit game : Game) : $[Figure] = game.species.get(r)

    def log(s : Any*)(implicit game : Game) {
        game.log(s : _*)
    }

    implicit class FactionEx(f : Faction)(implicit game : Game) {
        def log(s : Any*) { if (game.logging) game.log((f +: s.$) : _*) }
    }

    def options(implicit game : Game) = game.options
    def factions(implicit game : Game) = game.factions
    // def board(implicit game : Game) = game.board
}


trait RegionKey extends Key { self : Action =>
    val region : Region
}


//[[ BLACKER
class FactionState(val faction : Faction)(implicit game : Game) {
    // val reserve : Region = game.species.register(Reserve(faction), _.faction == faction,  1.to(12)./(Figure(faction, _)))

}
//]]

case class StartAction(version : String) extends StartGameAction with GameVersion
case class InisStartAction(version : String, factions : $[Faction], options : $[hrf.meta.GameOption]) extends ForcedAction
case class InisGameOverAction(version : String, factions : $[Faction], options : $[hrf.meta.GameOption], winner : Faction, pretenders : ListMap[Faction, Int], chapter : Int) extends ForcedAction


case object DoNothingAction extends ForcedAction


trait Expansion {
    def perform(a : Action, soft : Void)(implicit game : Game) : Continue

    implicit class ActionMatch(val a : Action) {
        def @@(t : Action => Continue) = t(a)
        def @@(t : Action => Boolean) = t(a)
    }
}

class Game(val setup : $[Faction], val options : $[Meta.O]) extends BaseGame with ContinueGame with LoggedGame {
    private implicit val game = this

    var isOver = false

    val expansions : $[Expansion] = $(CommonExpansion)

    var seating : $[Faction] = setup

    var factions : $[Faction] = setup
    var states = Map[Faction, FactionState]()

    // val board = BaseBoard

    object highlight {
        var faction : $[Faction] = $
        var current : |[Faction] = None
    }

    def info(waiting : $[Faction], self : |[Faction], actions : $[UserAction]) : $[Info] = {
        // Info((tile1.$ ++ stack1.num.times(0x2B21.toChar.toString.hh.spn(xstyles.larger150))).intersperse(None))(Break ~ "Terrain stacks") ::
        // Info((tile2.$ ++ stack2.num.times(0x2B21.toChar.toString.hh.spn(xstyles.larger150))).intersperse(None))(Break ~ "Terrain stacks") ::
        // Info((tile3.$ ++ stack3.num.times(0x2B21.toChar.toString.hh.spn(xstyles.larger150))).intersperse(None))(Break ~ "Terrain stacks") ::
        // Info(game.discard.num.hlb, "played,", game.deck.num.hlb, "in the deck")("Domination Cards") ::
        $
    }
    def convertForLog(s : $[Any]) : $[Any] = s./~{
        case Empty => None

        case NotInLog(_) => None
        case AltInLog(_, m) => |(m)
        // case d : DominanceCard => |(OnClick(d, d.elem.spn(xlo.pointer)))
        case l : $[Any] => convertForLog(l)
        case x => |(x)
    }

    override def log(s : Any*) {
        super.log(convertForLog(s.$) : _*)
    }

    def loggedPerform(action : Action, soft : Void) : Continue = {
        // println("> " + action)

        val c = action.as[SelfPerform]./(_.perform(soft)).|(internalPerform(action, soft))

        highlight.faction = c match {
            case Ask(f, _) => $(f)
            case MultiAsk(a, _) => a./(_.faction)
            case _ => Nil
        }

        // println("< " + c)

        c
    }

    def internalPerform(action : Action, soft : Void) : Continue = {
        expansions.foreach { e =>
            e.perform(action, soft) @@ {
                case UnknownContinue =>
                case Force(another) =>
                    if (action.isSoft.not && another.isSoft)
                        soft()

                    return another.as[SelfPerform]./(_.perform(soft)).|(internalPerform(another, soft))
                case TryAgain => return internalPerform(action, soft)
                case c => return c
            }
        }

        throw new Error("unknown continue on " + action)
    }
}

object CommonExpansion extends Expansion {
    def perform(action : Action, soft : Void)(implicit game : Game) = action @@ {
        // INIT
        case StartAction(version) =>
            log("HRF".hl, "version", gaming.version.hlb)
            log("INIS".hlb.styled(styles.title))

            if (version != gaming.version)
                log("Saved game version", version.hlb)

            options.foreach { o =>
                log(o.group, o.valueOn)
            }

            game.setup.foreach { f =>
                game.states += f -> new FactionState(f)
            }

            Then(InisStartAction(version, game.seating, options))

        // ...

        // case GameOverAction(winner) =>
        //     val winners = $(winner)

        //     game.highlight.current = |(winner)
        //     game.isOver = true

        //     winners.foreach(f => f.log("won"))

        //     GameOver(winners, "Game Over", winners./~(f => $(GameOverWonAction(null, f))))

        case DoNothingAction =>
            log("do nothing")

            Ask(game.setup.first)
                .add(DoNothingAction.as("Eh..."))
                .add(DoNothingAction.as("Oh..."))
    }
}
