package tactics

// The game loop: the actions players take and the reducer that applies them, one at a time, to a
// GameState. Pure: applyAction gives a new state, or the reason the action isn't allowed and changes nothing.
// In HRF every recorded action will go through it, so replaying the actions rebuilds the same game.
//
// A match is played in turns; in each turn the players take theirs in order, and each player's turn has three phases:
// - Income: on entering it, the player gets the rules' base income and every sector they hold gives its income
//   and scores its VP. They may deploy units in their zone, paying each one's cost.
// - Action: their units move and attack. Each unit attacks at most once a turn; it may move before attacking,
//   in as many moves as its movement points allow, but not after.
// - End: on leaving it, the player's units are no longer suppressed (suppression lasts through the suppressed
//   player's next turn), and the next player's units get their movement and attack back.
// A unit ending a move on a sector, or deployed on one, captures it: it is its owner's until another player's unit
// ends a move there. A player at the rules' VP target wins; after that every action is refused.
//
// With an order range in the rules (Rules.orders):
// - In their Income phase the player draws their order limit for the turn (DrawOrders, the number rolled outside,
//   as attack dice are), before leaving the phase; only the player sees it on the screen.
// - Every move, attack and retreat in the Action phase spends an order; on spending the last, the Action phase
//   ends by itself.
// Retreats: a hurt infantry unit falls back toward its zone (Movement.retreatPath) and can't move or attack again
// that turn. In the Income phase an infantry unit in its owner's zone can be reinforced: the rules' reinforcement
// cost buys back its reinforcement health, up to full. Destroying an enemy unit scores the rules' kill VP.

sealed trait TacticsAction {
    def player : String
}

case class DeployUnit(player : String, unitType : String, at : Hex) extends TacticsAction
case class MoveUnit(player : String, unit : String, to : Hex) extends TacticsAction
case class AttackUnit(player : String, attacker : String, target : String, rolls : List[Int]) extends TacticsAction
case class EndPhaseAction(player : String) extends TacticsAction
case class DrawOrders(player : String, limit : Int) extends TacticsAction
case class RetreatUnit(player : String, unit : String) extends TacticsAction
case class ReinforceUnit(player : String, unit : String) extends TacticsAction

object Turns {
    def applyAction(s : GameState, a : TacticsAction) : Either[String, GameState] =
        for {
            _ <- Either.cond(s.players.nonEmpty, (), "no match has started")
            _ <- s.winner.map(w => "the game is over: " + w + " won").toLeft(())
            _ <- Either.cond(s.players.contains(a.player), (), "no player " + a.player)
            _ <- Either.cond(s.current.contains(a.player), (), "it is " + s.current.get + "'s turn")
            t <- perform(s.copy(lastReport = None), a)
        } yield checkWinner(t)

    // Every action in order, stopping at the first refused
    def applyAll(s : GameState, actions : Seq[TacticsAction]) : Either[String, GameState] =
        actions.foldLeft[Either[String, GameState]](Right(s))((r, a) => r.flatMap(applyAction(_, a)))

    private def inPhase(s : GameState, p : Phase, what : String) = Either.cond(s.phase == p, (), what + " only in the " + p.name + " phase, not the " + s.phase.name)

    private def own(s : GameState, player : String, id : String) =
        s.unit(id).toRight("no unit " + id).filterOrElse(_.owner == player, id + " isn't " + player + "'s")

    private def perform(s : GameState, a : TacticsAction) : Either[String, GameState] = a match {
        case DeployUnit(p, name, h) =>
            for {
                _ <- inPhase(s, IncomePhase, "units are deployed")
                t <- s.rules.unitType(name).toRight("no unit type " + name)
                _ <- Either.cond(s.zones.getOrElse(p, Set()).contains(h), (), h.toString + " isn't in " + p + "'s deployment zone")
                _ <- Either.cond(s.player(p).resources.covers(t.cost), (), name + " costs " + t.cost + "; " + p + " has " + s.player(p).resources)
                n = s.deployed + 1
                placed <- s.place(t.make(p + "-" + n, p, h))
            } yield capture(pay(placed.copy(deployed = n), p, t.cost), h)

        case MoveUnit(p, id, h) =>
            for {
                _ <- inPhase(s, ActionPhase, "units move")
                u <- own(s, p, id)
                _ <- Either.cond(!u.attacked, (), id + " has attacked this turn and can't move")
                _ <- hasOrders(s)
                moved <- Movement.move(s, id, h)
            } yield spendOrder(capture(moved, h))

        case AttackUnit(p, attacker, target, rolls) =>
            for {
                _ <- inPhase(s, ActionPhase, "units attack")
                u <- own(s, p, attacker)
                _ <- Either.cond(!u.attacked, (), attacker + " has already attacked this turn")
                _ <- hasOrders(s)
                result <- Combat.attack(s, attacker, target, rolls)
            } yield {
                val (t, report) = result
                val bounty = if (report.destroyed) s.rules.killVP else 0
                val e = t.player(p)
                spendOrder(t.copy(units = t.units + (attacker -> t.units(attacker).copy(attacked = true)), lastReport = Some(report),
                    economy = t.economy + (p -> e.copy(vp = e.vp + bounty))))
            }

        case RetreatUnit(p, id) =>
            for {
                _ <- inPhase(s, ActionPhase, "units retreat")
                u <- own(s, p, id)
                _ <- hasOrders(s)
                moved <- Movement.retreat(s, id)
            } yield spendOrder(capture(moved, moved.units(id).hex))

        case ReinforceUnit(p, id) =>
            for {
                _ <- inPhase(s, IncomePhase, "units are reinforced")
                u <- own(s, p, id)
                _ <- Either.cond(u.kind == Infantry, (), "only infantry is reinforced")
                _ <- Either.cond(u.damaged, (), id + " isn't hurt")
                _ <- Either.cond(s.inZone(p, u.hex), (), id + " isn't in " + p + "'s deployment zone")
                _ <- Either.cond(s.player(p).resources.covers(s.rules.reinforceCost), (), "reinforcing costs " + s.rules.reinforceCost + "; " + p + " has " + s.player(p).resources)
            } yield pay(s.copy(units = s.units + (id -> u.copy(health = (u.health + s.rules.reinforceHealth).min(u.fullHealth), maxHealth = u.fullHealth))), p, s.rules.reinforceCost)

        case DrawOrders(p, n) =>
            for {
                _ <- inPhase(s, IncomePhase, "orders are drawn")
                range <- s.rules.orders.toRight("these rules have no order limit")
                _ <- Either.cond(s.exactOrderLimit == 0, (), p + " has already drawn their orders")
                _ <- Either.cond(n >= range._1 && n <= range._2, (), "the order limit is " + range._1 + " to " + range._2 + ", not " + n)
            } yield s.copy(exactOrderLimit = n)

        case EndPhaseAction(p) if s.phase == IncomePhase && !s.ordersDrawn =>
            Left(p + " hasn't drawn their orders")

        case EndPhaseAction(p) =>
            Right(s.phase match {
                case IncomePhase => s.copy(phase = ActionPhase)
                case ActionPhase => s.copy(phase = EndPhase)
                case EndPhase => beginTurn(nextPlayer(recover(s, p)))
            })
    }

    private def hasOrders(s : GameState) = Either.cond(s.ordersLeft.forall(_ > 0), (), s.current.get + " has no orders left")

    // An order is spent; on the last, the Action phase ends
    private def spendOrder(s : GameState) =
        if (s.rules.orders.isEmpty)
            s
        else {
            val t = s.copy(ordersUsed = s.ordersUsed + 1)
            if (t.ordersLeft.contains(0)) t.copy(phase = EndPhase) else t
        }

    private def pay(s : GameState, p : String, cost : Resources) =
        s.copy(economy = s.economy + (p -> s.player(p).copy(resources = s.player(p).resources - cost)))

    // The sector at h, if it is one, goes to the unit standing there
    def capture(s : GameState, h : Hex) : GameState =
        (s.board.get(h), s.at(h)) match {
            case (Some(t), Some(u)) if t.sector => s.copy(control = s.control + (h -> u.owner))
            case _ => s
        }

    // The player's units are no longer suppressed
    private def recover(s : GameState, p : String) =
        s.copy(units = s.units.map { case (id, u) => id -> (if (u.owner == p) u.copy(suppressed = false) else u) })

    private def nextPlayer(s : GameState) = {
        val next = (s.active + 1) % s.players.size
        s.copy(active = next, turn = if (next == 0) s.turn + 1 else s.turn)
    }

    // What the player gets at the start of their turn: the base income and their sectors' income and VP
    def income(s : GameState, p : String) : (Resources, Int) = {
        val sectors = s.sectorsOf(p)
        (s.rules.base + Resources.sum(sectors.map(_.income)), sectors.flatMap(_.vp).sum)
    }

    // The current player's Income phase begins: their units are ready again, and income and VP come in
    def beginTurn(s : GameState) : GameState = {
        val p = s.current.get
        val (gain, vp) = income(s, p)
        val ready = s.units.map { case (id, u) => id -> (if (u.owner == p) u.copy(spent = 0, attacked = false) else u) }
        val e = s.player(p)

        checkWinner(s.copy(phase = IncomePhase, units = ready, economy = s.economy + (p -> e.copy(resources = e.resources + gain, vp = e.vp + vp)), exactOrderLimit = 0, ordersUsed = 0))
    }

    // The first player, in turn order, at the VP target wins
    def checkWinner(s : GameState) : GameState =
        if (s.winner.isDefined)
            s
        else
            s.copy(winner = s.players.find(p => s.player(p).vp >= s.rules.vpTarget))
}
