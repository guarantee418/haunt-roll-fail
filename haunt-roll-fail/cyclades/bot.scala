package cyclades
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

// The Easy bot: it offers little and to the god that brings it nearest a Metropolis (Athena's Philosophers, a building it
// lacks), keeps a Fleet and a Troop or two, claims empty isles and attacks weaker ones within reach.
class BotXX(f : Faction) extends EvalBot {
    def eval(actions : $[UserAction])(implicit game : Game) : Compute[$[ActionEval]] = {
        val ev = new GameEvaluation(game, f)
        actions./{ a => ActionEval(a, ev.eval(a)) }
    }
}

class GameEvaluation(val game : Game, val self : Faction) {
    import game._

    lazy val mine = isles(self)
    lazy val missing = Building.all.%(b => mine.exists(i => buildingsOn(i).has(b)).not)
    lazy val room = mine.exists(i => freeSlots(i) > 0)

    // the isles its Troops could take: empty ones, and weaker ones
    def attackable(i : Isle, n : Int) = owner.get(i) match {
        case None => true
        case Some(e) if e == self => false
        case Some(e) => n > troopsAt(i) + minotaurOn(i).??(2) + buildingsOn(i).count(_ == Fortress) + metropolises.contains(i).??(1) + 1
    }

    lazy val troopChance = mine.exists(i => troopTargets(self, i, false).exists(x => attackable(x, troopsAt(i))))

    def godValue(g : God) : Int = g match {
        case Athena => 260 + self.philosophers * 90 + (missing.has(University) && room).??(60)
        case Poseidon => (fleetsOn(self) < 2).?(300).|(150) + (missing.has(Port) && room).??(60)
        case Ares => (troopsOn(self) < 2).?(260).|(140) + troopChance.??(220) + (missing.has(Fortress) && room).??(60)
        case Zeus => 150 + (self.priests < 2).??(60) + (missing.has(Temple) && room).??(60)
        case Apollo => 120 + (self.gold < 4).??(200)
    }

    def eval(a : Action) : $[Evaluation] = {
        var result : $[Evaluation] = Nil

        implicit class condToEval(val bool : Boolean) {
            def |=> (e : (Int, String)) { if (bool) result +:= Evaluation(e._1, e._2) }
        }

        a.unwrap match {
            case BidAction(_, _, g, n, _, _) =>
                true |=> (godValue(g) - payment(self, n) * 45) -> "offering"
            case ApolloBidAction(_, _, _, _) =>
                true |=> godValue(Apollo) -> "Apollo"
            case RecruitPhilosopherAction(_, c) =>
                true |=> (900 - c * 60 + self.philosophers * 50) -> "philosopher"
            case RecruitPriestAction(_, c) =>
                (c == 0) |=> 600 -> "free priest"
                true |=> -100 -> "priest"
            case RecruitFleetAction(_, s, c) =>
                (c == 0 || fleetsOn(self) < 3) |=> (550 - c * 80 + layout.commerce.has(s.cell).??(150)) -> "fleet"
                true |=> -100 -> "enough fleets"
            case RecruitTroopAction(_, i, c) =>
                (c == 0 || troopsOn(self) < 4) |=> (550 - c * 60 + metropolises.contains(i).??(100)) -> "troop"
                true |=> -100 -> "enough troops"
            case BuildAction(_, b, i) =>
                missing.has(b) |=> 800 -> "a building it lacks"
                (missing.has(b).not && self.gold < 8) |=> -100 -> "has one"
                true |=> (100 + metropolises.contains(i).not.??(20)) -> "build"
            case BuyCreatureAction(_, _, c, cost) =>
                (c == Fates) |=> (300 - cost * 30) -> "fates"
                (c == Satyr && self.philosophers == 3 && setup.exists(e => e != self && e.philosophers > 0)) |=> 950 -> "a metropolis"
                true |=> (-150 - cost * 40) -> "save"
            case ReplaceCreatureAction(_, _, _) =>
                true |=> -300 -> "keep"
            case TroopsMoveAction(_, from, to, n, _, _) =>
                val claim = owner.get(to).none
                val strong = attackable(to, n)
                val leaveOne = troopsAt(from) - n >= 1 || metropolises.contains(from).not
                (claim && leaveOne) |=> 820 -> "claim"
                (owner.get(to).%(_ != self).any && strong && leaveOne) |=> (760 + n * 10) -> "attack"
                true |=> -200 -> "stay"
            case FleetsTakeAction(_, s, n, sylph, _) =>
                true |=> (sylph > 0).?(200).|(-150) -> "move fleets"
            case FleetsStepAction(_, to) =>
                val near = layout.neighbors(to).of[Isle].exists(i => owner.get(i).%(_ == self).none)
                val enemy = fleets.get(to).%(_.f != self)
                val strong = moving.exists(m => enemy.forall(e => m.n > e.n + 1))
                (enemy.any && strong) |=> 500 -> "attack fleet"
                enemy.any |=> -500 -> "stronger fleet"
                (near && layout.commerce.has(to.cell)) |=> 300 -> "commerce"
                near |=> 250 -> "toward an isle"
                true |=> 50 -> "move"
            case FleetsStopAction(_) =>
                true |=> 150 -> "stop"
            case FleetsPickUpAction(_, _) =>
                true |=> -1000 -> "no"
            case FleetsDropAction(_, _) =>
                true |=> -1000 -> "no"
            case SylphDoneAction(_, _) =>
                true |=> 100 -> "done"
            case FightOnAction(_, b, defender) =>
                true |=> 200 -> "fight"
            case RetreatAction(_, b, _) =>
                val mine = (b.defender == self).?(b.d).|(b.a)
                val theirs = (b.defender == self).?(b.a).|(b.d + b.minotaur.??(2))
                (mine + 1 < theirs) |=> 300 -> "retreat"
                true |=> -100 -> "hold"
            case PlaceMetropolisAction(_, i, _, _) =>
                true |=> (troopsAt(i) * 50 + freeSlots(i) * 10) -> "metropolis"
            case ProsperityAction(_, i, _) =>
                true |=> (troopsAt(i) * 30 + metropolises.contains(i).??(100)) -> "prosperity"
            case PlaceFigureAction(_, c, area, _) =>
                area match {
                    case i : Isle =>
                        val own = owner.get(i).has(self)
                        (c == Medusa && own.not && troopsAt(i) > 0) |=> 300 -> "freeze"
                        ((c == Chiron || c == Minotaur) && own) |=> (200 + metropolises.contains(i).??(200)) -> "guard"
                        (c == Polyphemus && own) |=> 200 -> "guard"
                    case s : Sea =>
                        fleets.get(s).%(_.f != self).foreach(e => true |=> (200 + e.n * 100) -> "sink")
                        fleets.get(s).%(_.f == self).foreach(e => true |=> -500 -> "own")
                }
            case KrakenStepAction(_, to, _) =>
                fleets.get(to).%(_.f != self).foreach(e => true |=> 300 -> "sink")
                true |=> -300 -> "save"
            case KrakenDoneAction(_, _) =>
                true |=> 100 -> "stay"
            case SphinxDoneAction(_, _) =>
                true |=> 100 -> "done"
            case SphinxSellPriestAction(_, _) | SphinxSellFleetAction(_, _, _) | SphinxSellTroopAction(_, _, _) | SphinxSellPhilosopherAction(_, _) =>
                true |=> -300 -> "keep"
            case StealPhilosopherAction(_, e, _) =>
                true |=> (e.philosophers * 50) -> "steal"
            case StealPriestAction(_, e, _) =>
                true |=> (e.priests * 50) -> "steal"
            case GriffonAction(_, e, _) =>
                true |=> e.gold * 20 -> "gold"
            case HarpyAction(_, i, _) =>
                owner.get(i).has(self) |=> -800 -> "own"
            case GiantAction(_, i, _, _) =>
                owner.get(i).has(self) |=> -800 -> "own"
            case EndGodTurnAction(_) =>
                true |=> 100 -> "done"
            case _ =>
        }

        result.none |=> 0 -> "none"

        true |=> -((1 + math.random() * 60).round.toInt) -> "random"

        result.sortBy(v => -v.weight.abs)
    }
}

// For the headless tests (CYCLADES_RANDOM in host.scala): any choice at random, to reach every rule
class BotRandom(f : Faction) extends EvalBot {
    def eval(actions : $[UserAction])(implicit game : Game) : Compute[$[ActionEval]] =
        actions./{ a => ActionEval(a, $(Evaluation(a.unwrap match {
            case _ : FleetsPickUpAction | _ : FleetsDropAction => -1000
            case _ : EndGodTurnAction | _ : SphinxDoneAction | _ : KrakenDoneAction | _ : FleetsStopAction => (math.random() * 300).round.toInt - 100
            case _ => (math.random() * 400).round.toInt
        }, "random"))) }
}
