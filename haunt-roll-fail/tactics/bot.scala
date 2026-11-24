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

// The Easy bot: rules of thumb and chance. It moves toward the Control Points it doesn't hold, sets up its weapon teams,
// shoots what it can hurt most, buys units and upgrades, and unlocks the tiers.
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

        import game._

        def wanted = Board.points.keys.$.%(h => control.get(h).has(self).not)

        def nearest(h : Hex) = wanted./(Board.distance(h, _)).minOr(99)

        a.unwrap match {
            case StartPlaceAction(_, _, h, _) => true |=> (100 - nearest(h) * 5) -> "near points"
            case StartPickAction(_, t, _) => (t.weaponTeam.not) |=> 50 -> "rifles"
            case MoveAction(_, u, p, _) =>
                val from = nearest(where(u))
                val to = nearest(p.last)
                (Board.points.contains(p.last) && control.get(p.last).has(self).not) |=> 700 -> "take point"
                (to < from) |=> (300 + (p.num == 1).??(50)) -> "closer"
                (to > from) |=> -200 -> "away"
                (Board.points.contains(where(u)) && control.get(where(u)).has(self).not) |=> -500 -> "stay on point"
                (u.kind == Infantry && Board.terrain(p.last) == Building) |=> 120 -> "building"
            case SetUpAction(_, u, _) => (nearest(where(u)) <= 4) |=> 400 -> "set up"
            case PackUpAction(_, u, _) => true |=> -100 -> "stay set up"
            case SandbagsAction(_, u, _, _) => (Board.points.contains(where(u))) |=> 150 -> "dig in"
            case EmbarkAction(_, _, _, _, _) => true |=> -50 -> "walk"
            case DisembarkAction(_, _, h, _) => true |=> 100 -> "out"
            case EndTurnAction(_, _) => true |=> 0 -> "end"
            case TargetAction(_, u, t, d, _, _) =>
                val hurt = d.count(x => Matrix.resist(t.kind, x.dmg).has(false)) * 2 + d.count(x => Matrix.resist(t.kind, x.dmg).has(true))
                true |=> (500 + hurt * 100 - health(t) * 20) -> "fire"
            case HoldFireAction(_, _, _, _) => true |=> -100 -> "hold"
            case SpawnAction(_, t, h) =>
                val n = units(self).num
                (n < 9) |=> (600 + t.tier * 120 + t.vehicle.??(60) - (t.weaponTeam && units(self).count(_.utype.weaponTeam) >= 2).??(500) - nearest(h) * 3) -> "spawn"
            case UpgradeAction(_, u, i, e) => true |=> (250 + u.utype.tier * 50) -> "upgrade"
            case UnlockAction(_, t) => (units(self).num >= 4) |=> 800 -> "unlock"
            case PassAction(_) => true |=> 100 -> "pass"
            case _ =>
        }

        result.none |=> 0 -> "none"

        true |=> -((1 + math.random() * 90).round.toInt) -> "random"

        result.sortBy(v => -v.weight.abs)
    }
}
