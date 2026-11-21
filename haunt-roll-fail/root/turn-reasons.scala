package root
//
//
//
//
import hrf.colmat._
//
//
//
//

// WHY A SEAT IS ASKED, for its Discord turn ping (good-game/TurnPings.scala; asked for on 2026-11-21: "if it's their turn because
// they're being attacked, display the Root sword ... or to start the game to discard, card icons ... partisans or returning
// prisoners"). Worked out from the action that asked (game.asker, set in internalPerform) and the choices offered, both of which
// every client sees the same, so the reason is the same whoever reports it. The tags say nothing only the seat may know: an
// ambush question and assigning hits are both "defend" (or "battle" for the attacker), so a ping never tells whether the seat
// holds an ambush card. "/<faction>" names the faction whose head the ping shows. Display only: no game records change.
object TurnReasons {
    def tagOf(game : Game, f : |[Faction], actions : $[Any]) : String = {
        val asker = game.asker
        val name = asker @@ {
            case p : Product => p.productPrefix
            case _ => ""
        }
        def holds[T](a : Any)(implicit t : scala.reflect.ClassTag[T]) = a @@ {
            case p : Product => p.productIterator.exists(t.runtimeClass.isInstance)
            case _ => false
        }
        val head = f./("/" + _.short).|("")

        val tag =
            if (actions.exists(_.isInstanceOf[TournamentDeckAction])) "pickdeck"
            else if (actions.exists(_.isInstanceOf[TournamentMapAction])) "pickmap"
            else if (actions.exists(_.isInstanceOf[TournamentLandmarkAction])) "picklandmark"
            else if (actions.exists(_.isInstanceOf[PartisansAction])) "partisans"
            else asker @@ {
                case SetupNextFactionAction(_, _) => "draft"
                case SetupDoneAction => "keep"
                case _ if name.contains("HandLimit") => "handlimit"
                case _ if name.startsWith("CouncilEntreat") => "entreat"
                case OpportunityDiscardCardAction(_, ToHeal(_, _), _, _, _) => "hospitals"
                case _ if name.startsWith("FieldHospitals") || name.startsWith("HospitalPlan") => "hospitals"
                case _ if name.startsWith("KnavesFreePrisoners") || name.contains("Prisoner") => "prisoners"
                case _ if name.startsWith("InvasiveEEENegotiate") => "negotiate"
                case a if holds[Battle](a) => f.has(game.current).?("battle").|("defend")
                case _ if name.startsWith("Starting") || name.startsWith("Setup") || name.startsWith("FactionSetup") => "setup"
                case _ if f.has(game.current) => "turn"
                case _ => "asked"
            }

        tag @@ {
            case "draft" | "keep" | "pickdeck" | "pickmap" | "picklandmark" => tag
            case _ => tag + head
        }
    }

    def of(game : Game, p : Any, actions : $[Any] = $) : String = p @@ {
        case f : Faction => tagOf(game, |(f), actions)
        case p : Player => tagOf(game, game.ptf.get(p), actions)
        case _ => ""
    }
}
