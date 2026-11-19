package root
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


// the Doomed Expansion's factions (Warriors Wake, Doomsayers Union, Whisperers Guild): random, avoiding dominances
class GameEvaluationDoomed(faction : Faction)(implicit game : Game) extends GameEvaluation(game, faction) {
    def eval(a : Action) : List[Evaluation] = {
        if (game.states.contains(self).not)
            return $(Evaluation(-1 - (math.random() * 7).~, "proto random"))

        var result : List[Evaluation] = Nil

        implicit class condToEval(val bool : Boolean) {
            def |=> (e : (Int, String)) { if (bool) result +:= Evaluation(e._1, e._2) }
        }

        val g = game

        val others = g.factions.but(faction)

        a.unwrap match {
            case ActivateDominanceAction(f, d, _) =>
                true |=> -500 -> "dont"

            case TakeDominanceAction(f, d, _) =>
                true |=> -500 -> "dont"

            case BattleAmbushAction(_, _, _, _, _, _) =>
                true |=> 1000 -> "ambush"

            case BattleCounterAmbushAction(_, _, _, _, _, _) =>
                true |=> 1000 -> "ct-ambush"


            case PayGiveWarriorsAction(_, s, n, _, GetServicesAction(_, _, List(offers))) =>
                true |=> 50 - (math.pow(10, 2 + n) * math.random()).round.toInt -> "cost"


            // the Winged Menace: act rather than pass
            case MenaceSpawnAction(f, _, n, _) =>
                true |=> 40 + 10 * n -> "spawn"

            case MenaceExploreAction(f, _, _) =>
                true |=> 80 -> "explore"

            case MenaceBuildRowAction(f, _, _, _) =>
                true |=> 60 -> "build"

            case MenaceSquatAction(f, _, _, _) =>
                true |=> 70 -> "squat"

            case MenaceBiteTargetAction(f, _, _, _) =>
                true |=> 30 -> "bite"

            case MenaceSwarmTargetAction(f, c, e) =>
                true |=> 50 -> "swarm"

            // the Order of the Forest
            case OrderMinisterAction(f, _, t) =>
                true |=> 70 -> "minister"

            case OrderBuildAction(f, _, _) =>
                true |=> 60 -> "build"

            case OrderWitnessTargetAction(f, _, _, n) =>
                true |=> 50 - 10 * n -> "witness"

            case OrderCongregateAction(f, _, _) =>
                true |=> 80 -> "congregate"

            case OrderConfessAction(f, _) =>
                true |=> 20 -> "confess"

            // Old Man Tinker
            case OMTDoOverAction(f) =>
                true |=> -1000 -> "do-over"

            case OMTRepairAction(f, _, n) =>
                true |=> 50 + 10 * n -> "repair"

            case OMTBuildAction(f, _) =>
                true |=> 90 -> "build"

            case OMTTinkerAction(f, _, a, b) =>
                true |=> 30 + 5 * a -> "tinker"

            case OMTRestructureAction(f, _, k) =>
                true |=> (k == 1).?(40).|(-50) -> "restructure"

            // the Twelve Colonies
            case TWColonizeClearingAction(f, _, _, _) =>
                true |=> 80 -> "colonize"

            case TWClaimAction(f, n) =>
                true |=> 20 * n -> "claim"

            case TWRecruitAction(f, _, l) =>
                true |=> 40 -> "recruit"

            // the United Dove Corps
            case UDCMandateAction(f, m) =>
                true |=> (m @@ {
                    case UnitedDoveCorps.emergency => (f.ceasefires.num < 2).?(60).|(20)
                    case UnitedDoveCorps.protection => 10 + 20 * f.ceasefires.num
                    case _ => (f.hand.num >= 3).?(40).|(5) + (f.refugees.num > 2).??(10)
                }) + (math.random() * 20).round.toInt -> "mandate"

            case UDCCeasefireClearingAction(f, _) =>
                true |=> 90 -> "ceasefire"

            case UDCDeclarePeaceAction(f, _) =>
                true |=> 80 -> "peace"

            case UDCBuildCardsAction(f, _, _, _) =>
                true |=> 70 -> "build"

            case UDCRecruitClearingAction(f, _) =>
                true |=> 30 -> "recruit"

            case UDCEvacuateCardAction(f, _, _) =>
                true |=> 50 -> "evacuate"

            // the Arachnid Association
            case AAWeaveStartAction(f, _, o) =>
                true |=> (o.any.?(60).|(70) + (math.random() * 20).round.toInt) -> "weave"

            case AAWeaveCardAction(f, _, _, _) =>
                true |=> 30 -> "weave more"

            case AALayCardAction(f, _, _) =>
                true |=> 90 -> "lay"

            case AACaptureCardAction(f, _, _, _, _) =>
                true |=> 80 -> "capture"

            case AAConsumeMutateAction(f, _, _) =>
                true |=> (f.mutations.num < 3).?(60).|(10) + (math.random() * 20).round.toInt -> "mutate"

            case AAConsumeVPAction(f, _) =>
                true |=> 40 -> "consume"

            case AAPhobiaPayAction(e, f, _, _) =>
                true |=> (ArachnidAssociationExpansion.frightened(f, e).num > 2).?(40).|(-20) -> "pay off"

            case MenaceActiveAction(f, b, fright) =>
                true |=> (math.random() * 20).round.toInt -> "fight or fright"

            case _ =>
        }

        result.none |=> 0 -> "none"

        true |=> -((1 + math.random() * 7).round.toInt) -> "random"

        result.sortBy(v => -v.weight.abs)
    }

}
