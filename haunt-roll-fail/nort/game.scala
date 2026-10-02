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

import hrf.elem._

import nort.elem._


trait Faction extends NamedToString with Styling with Elementary with BasePlayer with Record {
    def short = name
    def style = name.toLowerCase
    override def elem : Elem = ("Clan of the " + name).styled(this)(styles.title)(xstyles.bold)
}

// Base game clans
case object Stag extends Faction
case object Goat extends Faction
case object Wolf extends Faction
case object Raven extends Faction


trait GameImplicits {
    implicit def factionToState(f : Faction)(implicit game : Game) : FactionState = game.states(f)

    def log(s : Any*)(implicit game : Game) {
        game.log(s : _*)
    }

    implicit class FactionEx(f : Faction)(implicit game : Game) {
        def log(s : Any*) { if (game.logging) game.log((f +: s.$) : _*) }
    }

    def options(implicit game : Game) = game.options
    def factions(implicit game : Game) = game.factions
}


class FactionState(val faction : Faction)(implicit game : Game)


case class StartAction(version : String) extends StartGameAction with GameVersion
case object UnderConstructionAction extends ForcedAction
case object EndGameAction extends ForcedAction


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

    object highlight {
        var faction : $[Faction] = $
        var current : |[Faction] = None
    }

    def info(waiting : $[Faction], self : |[Faction], actions : $[UserAction]) : $[Info] = $

    def convertForLog(s : $[Any]) : $[Any] = s./~{
        case Empty => None
        case NotInLog(_) => None
        case AltInLog(_, m) => |(m)
        case l : $[Any] => convertForLog(l)
        case x => |(x)
    }

    override def log(s : Any*) {
        super.log(convertForLog(s.$) : _*)
    }

    def loggedPerform(action : Action, soft : Void) : Continue = {
        val c = action.as[SelfPerform]./(_.perform(soft)).|(internalPerform(action, soft))

        highlight.faction = c match {
            case Ask(f, _) => $(f)
            case MultiAsk(a, _) => a./(_.faction)
            case _ => Nil
        }

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
            log("Northgard: Uncharted Lands".hlb.styled(styles.title))

            if (version != gaming.version)
                log("Saved game version", version.hlb)

            options.foreach { o =>
                log(o.group, o.valueOn)
            }

            game.setup.foreach { f =>
                game.states += f -> new FactionState(f)
            }

            Then(UnderConstructionAction)

        // Placeholder until the base game rules are implemented
        case UnderConstructionAction =>
            log("This game is very much", "under construction".styled(xstyles.warning))

            Ask(game.setup.first)
                .add(Info("Northgard: Uncharted Lands".hl, "is", "under construction".styled(xstyles.warning)))
                .add(Info("Nothing is playable yet"))
                .add(EndGameAction.as("End Game"))

        case EndGameAction =>
            game.isOver = true

            GameOver($, "Game Over" ~ Break ~ "Northgard: Uncharted Lands is under construction")
    }
}
