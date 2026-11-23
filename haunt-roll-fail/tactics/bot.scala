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

// The Easy bot: rules of thumb and chance. It reinforces its hurt infantry at home and deploys while it can
// afford to, attacks whatever it can hurt (most of all what it can destroy, for the kill VP), pulls badly hurt
// infantry back out of the enemy's reach, moves toward the sectors it doesn't hold, and ends its phases otherwise.
// With few orders it moves less for its own sake, as each move spends one.
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

        // the enemy units it knows of: with the fog of war, only those its side has spotted
        val enemies = if (s.rules.fog) s.spottedBy(self.name) else s.units.values.toList.%(_.owner != self.name)

        // the sectors it doesn't hold
        val wanted = s.board.all.%(_.sector).%(t => s.control.get(t.hex).has(self.name).not)./(_.hex)

        def nearest(h : Hex) = wanted./(w => Hex.distance(h, w)).minOption.|(0)

        // enemy units that could reach the hex with their weapons next turn (their movement, then their range)
        def threats(h : Hex) = enemies.%(e => e.weapon.exists(w => Hex.distance(e.hex, h) <= e.movement + w.range))

        a.unwrap match {
            case DeployAction(_, t, c) =>
                val cost = s.rules.unitType(t).get.cost
                true |=> (600 + cost.manpower * 20 + cost.munitions * 15 + cost.fuel * 15) -> "deploy"
                true |=> -(nearest(c.hex) * 10) -> "near a sector"
            case ReinforceAction(_, id) =>
                val u = s.unit(id).get
                true |=> (750 + (u.fullHealth - u.health) * 20) -> "reinforce"
            case ToActionPhaseAction(_) => true |=> 100 -> "to action"
            case MoveAction(_, id, c) =>
                val from = s.unit(id).get.hex
                val sector = wanted.has(c.hex)
                val closer = nearest(c.hex) < nearest(from)
                val enemy = enemies.exists(u => Hex.distance(u.hex, c.hex) <= 2)
                // with the fog, a move that shows its side 3 hexes or more it doesn't see now is worth something
                val sees = s.rules.fog && s.unit(id).exists(u => (s.board.visibleHexes(List(u.at(c.hex))) -- s.visibleHexes(self.name)).size >= 3)
                val u = s.unit(id).get
                val hurt = u.kind == Infantry && u.health * 2 <= u.fullHealth
                sector |=> 900 -> "capture"
                (sees && hurt.not) |=> 120 -> "look around"
                closer |=> 400 -> "closer to a sector"
                (enemy && hurt.not) |=> 150 -> "toward the enemy"
                (enemy && hurt) |=> -300 -> "hurt, keep away"
                true |=> 200 -> "move"
                s.ordersLeft.exists(_ <= 1) |=> -150 -> "last order"
            case AttackAction(_, a, t) =>
                val u = s.unit(a).get
                val v = s.unit(t).get
                val w = u.weapon.get
                val chance = (7 - Combat.needed(s, u, w, v)).max(0) / 6.0
                val expected = (w.dice * chance * Combat.damagePerHit(w, v)).round.toInt
                (expected > 0) |=> (700 + expected * 40) -> "hurt"
                (expected >= v.health) |=> 400 -> "destroy"
                true |=> 50 -> "attack"
            case RetreatAction(_, id, c) =>
                val u = s.unit(id).get
                val danger = threats(u.hex).num
                val badly = u.health * 3 <= u.fullHealth
                val hurt = u.health * 2 <= u.fullHealth
                val safer = threats(c.hex).num < danger
                (hurt && danger > 0) |=> 850 -> "pull back"
                (badly && danger > 0) |=> 150 -> "badly hurt"
                (hurt && safer) |=> 100 -> "out of reach"
                (hurt.not || danger == 0) |=> -400 -> "no need"
            case EndActionsAction(_) => true |=> 30 -> "end actions"
            case EndTurnAction(_) => true |=> 0 -> "end turn"
            case _ =>
        }

        result.none |=> 0 -> "none"

        true |=> -((1 + math.random() * 90).round.toInt) -> "random"

        result.sortBy(v => -v.weight.abs)
    }
}
