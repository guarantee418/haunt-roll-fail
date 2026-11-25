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

// The Easy bot: it saves for what brings the nearest Royal Seal (colonies, ports, forts, road networks, trading companies),
// walks a unit to where a colony can stand, keeps the cards that plan needs, and fights over the last seals.
class BotXX(f : Faction) extends EvalBot {
    def eval(actions : $[UserAction])(implicit game : Game) : Compute[$[ActionEval]] = {
        val ev = new GameEvaluation(game, f)
        actions./{ a => ActionEval(a, ev.eval(a)) }
    }
}

class GameEvaluation(val game : Game, val self : Faction) {
    import game._

    val hand = self.hand.of[Resource]

    val leader = factions.but(self).some./(_.maxBy(e => sealsOf(e).num * 10 + colonyOf(e).num)).|(self)
    val free = sealsOf(self).num
    // the seals no one holds that peace can bring
    lazy val open = Seal.companies.%(c => seals.contains(c).not).num + Seal.societies.%(c => seals.contains(c).not).num
    // the peaceful seals are nearly gone, or the game is long: fight for the rest
    lazy val warlike = turn > 60 && (open <= 2 || turn > 120)

    def cost(b : Build) : $[Resource] = b.cost

    // the hexes a colony could be founded on, and the units to walk there
    lazy val sites : $[Hex] = (colonyOf(self).num < Pieces.colonies).??(Board.hexes.%{ h =>
        colonies.contains(h).not && Board.edge(h).not && Board.terrain(h).land && Board.terrain(h) != Mountain && touchesRoad(h).not &&
        on(h).forall(_.faction == self) && colonies.keys.forall(c => Board.landDistance(c, h, 2).none)
    })

    // steps over land (around mountains and other players' hexes) to the nearest site, from everywhere
    lazy val toSite : Map[Hex, Int] = {
        var d = sites./(_ -> 0).toMap
        var front = sites
        while (front.any) {
            val next = front./~(Board.neighbors).distinct.%(h => d.contains(h).not && Board.terrain(h).land && Board.terrain(h) != Mountain && others(self, h).none && colonies.get(h).%(_ != self).none)
            front.foreach { x => Board.neighbors(x).%(next.has).foreach(h => if (d.contains(h).not) d += h -> (d(x) + 1)) }
            front = next
        }
        d
    }

    lazy val settler : |[Figure] = units(self).%(u => u.land && embarked.contains(u).not && toSite.contains(at(u))).some./(_.minBy(u => (toSite(at(u)), u.kind != Musketeer, u.index)))

    // what to save for: the goal with the fewest cards missing, colonies before the rest
    lazy val goals : $[($[Resource], Int)] = {
        val colony = (colonyOf(self).num < Pieces.colonies && settler.any).$((cost(ColonyBuild), -3))
        val port = (portSites(self).any && ports.values.count(_.owner == self) < 2).$((cost(PortBuild), -1))
        val fort = (fortSites(self).any && forts.values.count(_.owner == self) < 2).$((cost(FortBuild), -1))
        val unit = (units(self).num < 2 || (warlike && units(self).num < 8)).$(($[Resource](Wheat, Fish), -1))
        val found = Seal.companies.%(c => seals.contains(c).not && companyBases(self, c.resource).any)./(c => (1.to(5).$./(_ => c.resource), 0))
        val buy = Seal.companies.%(c => seals.get(c).%(_ != self).any && companyBases(self, c.resource).any)./(c => (1.to(stockpiles(c.resource).num + 1).$./(_ => c.resource), 1))
        // (a goal waiting for cards the bank has none of left kept every card in hand, and the game at a standstill)
        (colony ++ port ++ fort ++ unit ++ found ++ buy).%{ case (l, _) => missing(l).distinct.forall(r => bankHas(r) || tradeSource(self, r).any) }
    }

    def missing(l : $[Resource]) : $[Resource] = l.diff(hand)

    lazy val goal : $[Resource] = goals.some./(_.minBy { case (l, bias) => missing(l).num + bias })./(_._1).|($)
    lazy val needs : $[Resource] = missing(goal)

    // paying with cards the goal needs
    def spoils(paid : $[Resource]) = goal.any && hand.diff(paid).intersect(goal).num < hand.intersect(goal).num

    def yields(r : Resource) = colonyOf(self).count(c => annexed(c).not && Board.neighbors(c).exists(h => Board.terrain(h).resource == r && others(self, h).none))

    def harvest(r : Resource) = yields(r) * (10 + needs.count(_ == r) * 30 + (goal.has(r)).??(5))

    // a road between colonies not yet joined
    def roadGain(b : Hex) : Boolean = {
        val ends = roadEnds(self)
        val cc = ownColonies(self)
        cc.exists(c => cc.exists(e => e != c && joined(self, e).has(c).not && Board.distance(b, c) < ends.%(x => x != c && joined(self, c).has(x).not && linked(allLinks(self), c).has(x).not)./(x => Board.distance(x, c)).minOr(99)))
    }

    def spread(h : Hex) = Board.neighbors(h)./(n => Board.terrain(n).resource).distinct.num * 100 + Board.neighbors(h).count(n => Board.terrain(n) == Water) * 5

    def eval(a : Action) : $[Evaluation] = {
        var result : $[Evaluation] = Nil

        implicit class condToEval(val bool : Boolean) {
            def |=> (e : (Int, String)) { if (bool) result +:= Evaluation(e._1, e._2) }
        }

        a.unwrap match {
            case PlaceStartAction(_, h, _) => true |=> (200 + spread(h)) -> "varied harvests"
            case HarvestChooseAction(_, mine, _) => true |=> (200 + harvest(mine)) -> "harvest"
            case FoundersChooseAction(_, _, pick, _, _) => true |=> (200 + harvest(pick)) -> "harvest"
            case BuildColonyAction(_, _, _) => true |=> 900 -> "colony"
            case FoundCompanyAction(_, _) => true |=> 950 -> "company"
            case BuyoutAction(_, c, n) => (n == stockpiles(c.resource).num + 1) |=> 700 -> "buyout"
            case BuildFortAction(_, _, p) => (spoils(p).not || goal == cost(FortBuild)) |=> 500 -> "fort"
            case BuildPortAction(_, _, _, p) => (spoils(p).not || goal == cost(PortBuild)) |=> 550 -> "port"
            case BuildRoadAction(_, a, b, p) =>
                val gain = (colonyOf(self).num >= 3 || settler.none || sites.none) && (roadGain(a) || roadGain(b))
                (gain && spoils(p).not) |=> 450 -> "join colonies"
                (goal.none && hand.num > 14) |=> 120 -> "spend a full hand"
                true |=> -300 -> "no road"
            case BuildBridgeAction(_, _, _, b, p) =>
                (colonyOf(self).num >= 3 && roadGain(b) && spoils(p).not) |=> 440 -> "join colonies"
                true |=> -300 -> "no bridge"
            case RepairFortAction(_, _, _) => true |=> 600 -> "repair"
            case StonewallRepairAction(_, _, _, _) => true |=> 600 -> "repair"
            case RecruitAction(_, k, _, p) =>
                val few = units(self).num < 2
                few |=> 520 -> "a unit to settle with"
                (few.not && spoils(p).not && units(self).num < (warlike || hand.num > 14).?(20).|(colonyOf(self).num + 1)) |=> (300 + Kind.all.indexOf(k) * 20) -> "recruit"
                true |=> -200 -> "enough units"
            case ReviveAction(_, _, p, _) => spoils(p).not |=> 650 -> "revive"
            case FrigateReviveNowAction(_, _, _, _) => true |=> 650 -> "revive"
            case TradeAction(_, r, paid) =>
                val l = paid
                (needs.has(r) && spoils(l).not) |=> 350 -> "trade for the plan"
                // a full hand goes back to the bank
                (trades < 3 && hand.num > 16 && spoils(l).not && paid.has(r).not && hand.count(_ == r) < 2) |=> 140 -> "a full hand"
                true |=> -200 -> "trade otherwise"
            case DeclareWarAction(_, e, boar) =>
                (warlike && e == leader && boar.not) |=> 350 -> "war on the leader"
                (turn > 250 && boar.not) |=> 340 -> "war with anyone"
                true |=> -300 -> "peace is fine"
            case OfferPeaceAction(_, e, _) =>
                (units(self).num < units(e).num - 2) |=> 105 -> "peace when weaker"
                true |=> -100 -> "keep the war"
            case AcceptPeaceAction(_, e, t) => (units(self).num < units(e).num && t.demand.none) |=> 300 -> "accept peace when weaker"
            case DeclinePeaceAction(_, e, t) => true |=> 100 -> "decline"
            case DepositAction(_, _, _) => true |=> -500 -> "keep cards"
            case WithdrawAction(_, _) => true |=> -500 -> "leave the stockpile"
            case PlacePrisonerAction(_, _, _, _, _) => true |=> 200 -> "place"
            case KeepColonyAction(_, _, _, _) => true |=> 100 -> "colony"
            case DoneUnitAction(_, _, _) => true |=> 130 -> "done"
            case SurrenderAction(_) =>
                // a lost position held forever kept some games from ending
                (turn > 200) |=> 300 -> "give up"
                true |=> -290 -> "hold on"
            case AttackAction(_, _, v, _) => true |=> (750 + (v.kind == Cannon).??(50)) -> "attack"
            case AttackFortAction(_, _, _, _) => true |=> 720 -> "attack fort"
            case StepAction(_, u, h, c) =>
                val capture = others(self, h).any
                val annex = colonies.get(h).%(_ != self).any
                val enemy = colonies.toList.filter { case (_, e) => e != self && canAttack(self, e) }.map(_._1)
                val closer = enemy.exists(c => Board.distance(h, c) < Board.distance(at(u), c))
                val settling = settler.has(u) && u.land && Board.terrain(h).land && toSite.get(h).exists(_ < toSite(at(u)))
                capture |=> 850 -> "capture"
                annex |=> 820 -> "annex"
                settling |=> 260 -> "toward a colony site"
                (closer && settler.has(u).not && enemy.any) |=> 220 -> "advance"
                // the colony's road neighbors block no one's harvest here: a unit on its own colony keeps it
                // a long game has come to a standstill: look around
                (turn > 250 && math.random() < 0.4) |=> 180 -> "stir"
                true |=> -150 -> "stay"
            case HideoutAction(_, _, _) => true |=> -100 -> "hideout"
            case ExtraActionAction(_, _, _) => true |=> -50 -> "extra"
            case RetreatAction(_, _, _, _, _, _) => true |=> 300 -> "retreat"
            case FortifyAction(_, _, _, _, _) => true |=> 400 -> "fortify"
            case TakeHitAction(_, _, _, _, _) => true |=> 0 -> "take hit"
            case CounterPassAction(_, _) => true |=> 110 -> "pass"
            case EndTurnAction(_) => true |=> 110 -> "end turn"
            case _ =>
        }

        result.none |=> 0 -> "none"

        true |=> -((1 + math.random() * 90).round.toInt) -> "random"

        result.sortBy(v => -v.weight.abs)
    }
}
