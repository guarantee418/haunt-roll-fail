package root
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


class BotRR extends EvalBot {
    def eval(actions : List[UserAction])(implicit game : Game) : Compute[$[ActionEval]] = {
        actions./{ a => ActionEval(a, Evaluation((random() * 10).round.toInt, "random") :: Nil) }
    }
}
