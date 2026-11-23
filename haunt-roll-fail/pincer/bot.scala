package pincer
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

// The Easy bot: a few rules of thumb and a lot of chance. It expands, builds, recruits, declares war on
// the player with the most seals, attacks and captures when it can, and ends its turn otherwise.
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

        val leader = factions.but(self).maxBy(e => sealsOf(e).num * 10 + colonyOf(e).num)

        a.unwrap match {
            case BuildColonyAction(_, _, _) => true |=> 900 -> "colony"
            case FoundCompanyAction(_, _) => true |=> 950 -> "company"
            case BuyoutAction(_, c, n) => (n == stockpiles(c.resource).num + 1) |=> 700 -> "buyout"
            case BuildFortAction(_, _, _) => true |=> 500 -> "fort"
            case BuildPortAction(_, _, _, _) => true |=> 550 -> "port"
            case BuildRoadAction(_, a, b, _) =>
                val toward = colonyOf(self).exists(c => Board.distance(b, c) < Board.distance(a, c) || Board.distance(a, c) < Board.distance(b, c))
                true |=> (200 + toward.??(100)) -> "road"
            case BuildBridgeAction(_, _, _, _, _) => true |=> 150 -> "bridge"
            case RepairFortAction(_, _, _) => true |=> 600 -> "repair"
            case StonewallRepairAction(_, _, _, _) => true |=> 600 -> "repair"
            case RemoveFortAction(_, _) => true |=> 800 -> "remove fort"
            case RecruitAction(_, k, _, _) => (units(self).num < 6 + colonyOf(self).num * 2) |=> (400 + Kind.all.indexOf(k) * 30) -> "recruit"
            case ReviveAction(_, _, _, _) => true |=> 650 -> "revive"
            case FrigateReviveNowAction(_, _, _, _) => true |=> 650 -> "revive"
            case TradeAction(_, r, paid) =>
                val need = $[Resource](Lumber, Stone, Iron, Wheat)
                (trades < 2 && self.hand.of[Resource].num >= 7 && need.has(r) && self.hand.count(r) < 2 && paid.has(r).not) |=> 300 -> "trade"
                true |=> -200 -> "trade otherwise"
            case DeclareWarAction(_, e, boar) =>
                (e == leader && turn > 6 && boar.not) |=> 350 -> "war on the leader"
                true |=> -300 -> "peace is fine"
            case OfferPeaceAction(_, e) => (e != leader) |=> 100 -> "peace"
            case AcceptPeaceAction(_, e) => (e != leader) |=> 300 -> "accept peace"
            case DeclinePeaceAction(_, e) => (e == leader) |=> 300 -> "decline"
            case GiveCardAction(_, _, _) => true |=> -1000 -> "keep cards"
            case ReturnPrisonerAction(_, _) => true |=> -1000 -> "keep prisoners"
            case SurrenderAction(_) => true |=> -2000 -> "never"
            case AttackAction(_, _, v, _) => true |=> (750 + (v.kind == Cannon).??(50)) -> "attack"
            case AttackFortAction(_, _, _, _) => true |=> 720 -> "attack fort"
            case MoveAction(_, u, h, c) =>
                val capture = others(self, h).any
                val annex = colonies.get(h).%(_ != self).any
                val enemy = colonies.toList.filter(_._2 != self).map(_._1)
                val closer = enemy.exists(c => Board.distance(h, c) < Board.distance(at(u), c))
                val site = colonySites(self).none && Board.edge(h).not && Board.terrain(h).land && colonies.keys.forall(c => Board.landDistance(c, h, 2).none)
                capture |=> 850 -> "capture"
                annex |=> 820 -> "annex"
                site |=> 260 -> "colony site"
                (closer && wars.exists(_.between(self, leader))) |=> 220 -> "advance"
                (colonies.get(at(u)).has(self) && on(at(u)).%(_.faction == self).num == 1 && c.none) |=> -150 -> "keep guard"
                true |=> 120 -> "move"
            case HideoutAction(_, _, _) => true |=> -100 -> "hideout"
            case ExtraActionAction(_, _, _) => true |=> -50 -> "extra"
            case RetreatAction(_, _, _, _, _, _) => true |=> 300 -> "retreat"
            case FortifyAction(_, _, _, _, _) => true |=> 400 -> "fortify"
            case TakeHitAction(_, _, _, _, _) => true |=> 0 -> "take hit"
            case CounterPassAction(_, _) => true |=> 0 -> "pass"
            case EndTurnAction(_) => true |=> 50 -> "end turn"
            case _ =>
        }

        result.none |=> 0 -> "none"

        true |=> -((1 + math.random() * 90).round.toInt) -> "random"

        result.sortBy(v => -v.weight.abs)
    }
}
