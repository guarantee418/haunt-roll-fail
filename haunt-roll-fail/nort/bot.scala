package nort
//
//
//
//
import hrf.colmat._
import hrf.compute._
import hrf.logger._
//
//
//
//

class BotXX(f : Faction) extends EvalBot {
    def eval(actions : $[UserAction])(implicit game : Game) : Compute[$[ActionEval]] = {
        if (game.training) {
            val ev = new TrainingEvaluation(f, 40)
            return actions./{ a => ActionEval(a, ev.eval(a)) }
        }

        val ev = new GameEvaluation(f)
        actions./{ a => ActionEval(a, BotXX.noTrade(a) ++ ev.eval(a)) }
    }
}

object BotXX {
    // The Easy bot never trades: it picked exchanges at random and could give away the food it needed for Winter.
    // Every exchange choice comes with a Done or "no more" choice, so pushing them last is enough.
    def trade(a : Action) : Boolean = a.unwrap match {
        case _ : TradeForAction | _ : TeamTradeAction | _ : TeamOfferAction | _ : TeamAcceptAction | _ : BonfireTradeAction | _ : BarterPayAction | _ : KoboldSwapAction | _ : CampTradeAction => true
        case _ => false
    }

    def noTrade(a : Action) : $[Evaluation] = trade(a).??($(Evaluation(-10000, "never trade")))
}

class GameEvaluation(val self : Faction)(implicit val game : Game) {
    def eval(a : Action) : $[Evaluation] = {
        var result : $[Evaluation] = Nil

        implicit class condToEval(val bool : Boolean) {
            def |=> (e : (Int, String)) { if (bool) result +:= Evaluation(e._1, e._2) }
        }

        a.unwrap @@ {
            case CancelAction => true |=> -1000 -> "cancel"
            case PlayCardAction(_, _, _) => true |=> 20 -> "play a card"
            case MoveDoneAction(_, _, _) => true |=> -5 -> "stop moving early"
            case RecruitDoneAction(_, _, _) => true |=> -5 -> "stop recruiting early"
            case BuildDoneAction(_, _, _) => true |=> -5 -> "stop building"
            case UpgradeCardAction(_, _, _, _) => true |=> 15 -> "upgrade"
            case DefensiveCancelAction(_, _, _, _) => true |=> -3 -> "keep defensive strategy"
            case TeamTradeAction(_, _, _, _, _) => true |=> -3 -> "trade with a teammate"
            case _ =>
        }

        result.none |=> 0 -> "none"

        true |=> -((1 + math.random() * 7).round.toInt) -> "random"

        result.sortBy(v => -v.weight.abs)
    }
}
