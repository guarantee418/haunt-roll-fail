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

import hrf.elem._

import nort.elem._

// "Robotos": the Hard bot (bot-hard.scala), cheating. A clan set to "Bot / Robotos" gets RobotosOption (options.scala)
// when the game starts (Meta.botOptions), and the game then plays by other rules for it:
// - no Winter costs (EventsExpansion.winterCost);
// - creatures ignore it: they never move into a territory with its figures (CreaturesExpansion.destinations), and what
//   they do when they appear or move (a Draugr's casualty, a Fallen Valkyrie's attack, ...) doesn't touch it (below);
// - one more unit with every Recruit, on top of Training Camps and the rest (TrainingCampsAction in map.scala).
// The Hard bot's valuation knows these rules for any clan that has them (game.robotos), so it plays them out,
// and opponents played by the Hard bot judge a Robotos clan rightly.

class BotRobotos(f : Faction) extends BotHard(f)

object RobotosExpansion extends Expansion {
    def perform(action : Action, soft : Void)(implicit game : Game) = action @@ {
        // A creature appearing or moving where a Robotos clan has figures does nothing to it
        case CreatureEffectAction(c, then) if game.creatureAt.contains(c) && game.present(game.board.territory(game.creatureAt(c))).exists(game.robotos) =>
            val t = game.board.territory(game.creatureAt(c))
            game.present(t).%(game.robotos).foreach(f => log(c, "ignores", f, "(Robotos)".hl))
            game.note("robotos-ignored")
            Then(then)

        case _ => UnknownContinue
    }
}
