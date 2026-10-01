package coup
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
        val ev =
            if (f == Amalthea || !true)
                new GameEvaluation04(game, f)
            else
                new GameEvaluation03(game, f)

        actions./{ a => ActionEval(a, ev.eval(a)) }
    }
}

trait GEvaluation {
    def eval(a : Action) : $[Evaluation]
}
