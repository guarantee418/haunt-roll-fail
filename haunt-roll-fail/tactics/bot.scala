package tactics
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

// The Easy bot: rules of thumb and chance. It deploys while it can afford to, attacks whatever it can hurt,
// moves toward the sectors it doesn't hold, and ends its phases otherwise.
class BotXX(f : Faction) extends EvalBot {
    def eval(actions : $[UserAction])(implicit game : Game) : Compute[$[ActionEval]] = {
        val ev = new GameEvaluation(game, f)
        actions./{ a => ActionEval(a, ev.eval(a)) }
    }
}

class GameEvaluation(val game : Game, val self : Faction) {
    def eval(a : Action) : $[Evaluation] = {
        var result : $[Evaluation] = Nil

        implicit class condToEval(val bool : Boolean) {
            def |=> (e : (Int, String)) { if (bool) result +:= Evaluation(e._1, e._2) }
        }

        val s = game.state

        // the sectors it doesn't hold
        val wanted = s.board.all.%(_.sector).%(t => s.control.get(t.hex).has(self.name).not)./(_.hex)

        def nearest(h : Hex) = wanted./(w => Hex.distance(h, w)).minOption.|(0)

        a.unwrap match {
            case DeployAction(_, t, c) =>
                val cost = s.rules.unitType(t).get.cost
                true |=> (600 + cost.manpower * 20 + cost.munitions * 15 + cost.fuel * 15) -> "deploy"
                true |=> -(nearest(c.hex) * 10) -> "near a sector"
            case ToActionPhaseAction(_) => true |=> 100 -> "to action"
            case MoveAction(_, id, c) =>
                val from = s.unit(id).get.hex
                val sector = wanted.has(c.hex)
                val closer = nearest(c.hex) < nearest(from)
                val enemy = s.units.values.exists(u => u.owner != self.name && Hex.distance(u.hex, c.hex) <= 2)
                sector |=> 900 -> "capture"
                closer |=> 400 -> "closer to a sector"
                enemy |=> 150 -> "toward the enemy"
                true |=> 200 -> "move"
            case AttackAction(_, a, t) =>
                val u = s.unit(a).get
                val v = s.unit(t).get
                val w = u.weapon.get
                val chance = (7 - Combat.needed(s, u, w, v)).max(0) / 6.0
                val expected = (w.dice * chance * Combat.damagePerHit(w, v)).round.toInt
                (expected > 0) |=> (700 + expected * 40) -> "hurt"
                (expected >= v.health) |=> 400 -> "destroy"
                true |=> 50 -> "attack"
            case EndActionsAction(_) => true |=> 30 -> "end actions"
            case EndTurnAction(_) => true |=> 0 -> "end turn"
            case _ =>
        }

        result.none |=> 0 -> "none"

        true |=> -((1 + math.random() * 90).round.toInt) -> "random"

        result.sortBy(v => -v.weight.abs)
    }
}
