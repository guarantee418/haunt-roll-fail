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

        // the other side (2026-11-22, "for attacks and entreaties, can you say who is x who?"): a battle's attacker for the
        // defending side, its defender for the attacking side, and the faction that entreated the Council; "/<faction>" after the head
        val battle = asker @@ {
            case p : Product => p.productIterator.collectFirst { case b : Battle => b }
            case _ => None
        }
        val other : Option[Faction] = asker @@ {
            case CouncilEntreatAction(e, _, _, _) => Some(e)
            case _ => battle.map(b => if (f.exists(x => x == b.attacker || b.ally.has(x))) b.defender else b.attacker)
        }
        // a Twilight Council Banish (its Bats driving the enemy out, 17.6.1.I) is a battle, but its pings say "banish" (2026-11-24,
        // asked for): "banished" for the enemy, "banish" for the Council
        val banishing = { implicit val g : Game = game; battle.exists(_.attacker.as[Council].?(_.used.has(Banishing))) }
        val versus = other.filterNot(o => f.has(o))./("/" + _.short).|("")

        val tag =
            if (actions.exists(_.isInstanceOf[TournamentDeckAction])) "pickdeck"
            else if (actions.exists(_.isInstanceOf[TournamentMapAction])) "pickmap"
            else if (actions.exists(_.isInstanceOf[TournamentLandmarkAction])) "picklandmark"
            else if (actions.exists(_.isInstanceOf[PartisansAction])) "partisans"
            // a draw the Lilypad Diaspora's Pond offers in place of the deck (2026-11-24, asked for: an Outrage's supporter waited on
            // the Woodland Alliance and the ping only said it was needed)
            else if (actions.exists { case WrapAction(_ : InvasiveEEEPondDrawAction) => true ; case _ : InvasiveEEEPondDrawAction => true ; case _ => false }) "pond"
            else asker @@ {
                case SetupNextFactionAction(_, _) => "draft"
                case SetupDoneAction => "keep"
                case _ if name.contains("HandLimit") => "handlimit"
                case _ if name.startsWith("CouncilEntreat") => "entreat"
                case OpportunityDiscardCardAction(_, ToHeal(_, _), _, _, _) => "hospitals"
                case _ if name.startsWith("FieldHospitals") || name.startsWith("HospitalPlan") => "hospitals"
                case _ if name.startsWith("KnavesFreePrisoners") || name.contains("Prisoner") => "prisoners"
                // (no "negotiate": a Lilypad Diaspora negotiation is never owed, its menus are ones the seat opened on its own turn,
                // and a ping saying "a negotiation awaits you" reminded the enemy of what the Diaspora hopes it forgets; 2026-11-23,
                // asked for. It is the seat's turn, or it's needed, like any other menu)
                case a if holds[Battle](a) && banishing => f.has(game.current).?("banish").|("banished")
                case a if holds[Battle](a) => f.has(game.current).?("battle").|("defend")
                case _ if name.startsWith("Starting") || name.startsWith("Setup") || name.startsWith("FactionSetup") => "setup"
                case _ if f.has(game.current) => "turn"
                case _ => "asked"
            }

        tag @@ {
            case "draft" | "keep" | "pickdeck" | "pickmap" | "picklandmark" => tag
            case "defend" | "battle" | "banish" | "banished" | "entreat" if head.nonEmpty => tag + head + versus
            case _ => tag + head
        }
    }

    def of(game : Game, p : Any, actions : $[Any] = $) : String = p @@ {
        case f : Faction => tagOf(game, |(f), actions)
        case p : Player => tagOf(game, game.ptf.get(p), actions)
        case _ => ""
    }
}
