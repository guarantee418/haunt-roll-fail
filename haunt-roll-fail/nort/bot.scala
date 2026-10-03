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
        val ev = new GameEvaluation(f)
        actions./{ a => ActionEval(a, ev.eval(a)) }
    }
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
            case BuildDoneAction(_, _) => true |=> -5 -> "stop building"
            case _ =>
        }

        result.none |=> 0 -> "none"

        true |=> -((1 + math.random() * 7).round.toInt) -> "random"

        result.sortBy(v => -v.weight.abs)
    }
}
