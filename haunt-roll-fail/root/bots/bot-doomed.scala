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

            // the Twelve Colonies
            case TWColonizeClearingAction(f, _, _, _) =>
                true |=> 80 -> "colonize"

            case TWClaimAction(f, n) =>
                true |=> 20 * n -> "claim"

            case TWRecruitAction(f, _, l) =>
                true |=> 40 -> "recruit"

            case MenaceActiveAction(f, b, fright) =>
                true |=> (math.random() * 20).round.toInt -> "fight or fright"

            case _ =>
        }

        result.none |=> 0 -> "none"

        true |=> -((1 + math.random() * 7).round.toInt) -> "random"

        result.sortBy(v => -v.weight.abs)
    }

}
