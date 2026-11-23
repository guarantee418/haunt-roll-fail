package tactics

import scala.annotation.tailrec
import scala.collection.immutable.TreeSet

// Movement: what entering a hex costs a unit, and pathfinding (Dijkstra) over the board.
// Pure functions of a GameState.
//
// Rules:
// - Entering a hex costs its terrain's cost for the unit's kind (Terrain.cost); the hex left costs nothing.
// - A unit can't enter terrain its kind can't (no cost), or a hex off the board.
// - It can pass through hexes of units with the same owner, but not end its move there.
// - It can't enter a hex with another owner's unit at all.
// - A move costs the cheapest path's cost, which must be at most the unit's movement points left.
// - Ties between paths of the same cost are broken the same way every time (by cost, then q, then r),
//   so every client and replay finds the same path.

case class Path(hexes : List[Hex], cost : Int) {
    def start = hexes.head
    def end = hexes.last
    def steps = hexes.size - 1
}

object Movement {
    // What it costs the unit to enter the hex on its way, None when it can't
    def enterCost(s : GameState, u : CombatUnit, h : Hex) : Option[Int] =
        s.board.get(h).flatMap(_.terrain.cost(u.kind)).filter(_ => s.at(h).forall(_.owner == u.owner))

    // Whether the unit can end a move in the hex
    def canEnd(s : GameState, u : CombatUnit, h : Hex) = h != u.hex && enterCost(s, u, h).isDefined && !s.occupied(h)

    private implicit val frontierOrder : Ordering[(Int, Hex)] = Ordering.by { case (c, h) => (c, h.q, h.r) }

    // Dijkstra from the unit's hex, as far as the budget goes and stopping once the target, if any, is
    // settled. Gives the cheapest cost to each hex found and the hex before it on that path.
    private def search(s : GameState, u : CombatUnit, budget : Int, target : Option[Hex]) : (Map[Hex, Int], Map[Hex, Hex]) = {
        @tailrec
        def loop(frontier : TreeSet[(Int, Hex)], cost : Map[Hex, Int], from : Map[Hex, Hex]) : (Map[Hex, Int], Map[Hex, Hex]) =
            frontier.headOption match {
                case None =>
                    (cost, from)

                case Some((_, h)) if target.contains(h) =>
                    (cost, from)

                case Some(next @ (c, h)) =>
                    val better = h.neighbors.flatMap { n =>
                        enterCost(s, u, n).map(c + _).filter(nc => nc <= budget && cost.get(n).forall(nc < _)).map(n -> _)
                    }

                    // A hex found again for less leaves its old, dearer entry behind; the cheaper one comes
                    // out first, so when the old one comes out its neighbors are all found no cheaper
                    loop(frontier - next ++ better.map(_.swap), cost ++ better, from ++ better.map { case (n, _) => n -> h })
            }

        loop(TreeSet((0, u.hex)), Map(u.hex -> 0), Map())
    }

    // Every hex the unit can move to with its movement points left, and the cheapest cost of each
    def reachable(s : GameState, u : CombatUnit) : Map[Hex, Int] =
        if (!u.alive)
            Map()
        else
            search(s, u, u.movesLeft, None)._1.filter { case (h, _) => canEnd(s, u, h) }

    // The cheapest path from the unit to the hex, whatever its movement points left (to plan
    // several turns ahead); None if it can't end its move there or there is no way
    def path(s : GameState, u : CombatUnit, to : Hex) : Option[Path] =
        if (to == u.hex)
            Some(Path(List(u.hex), 0))
        else
        if (!canEnd(s, u, to))
            None
        else {
            val (cost, from) = search(s, u, Int.MaxValue, Some(to))

            cost.get(to).map { c =>
                Path(List.unfold(Option(to))(_.map(h => (h, from.get(h)))).reverse, c)
            }
        }

    // RETREAT: a hurt infantry unit falls back up to rules.retreatHexes hexes toward its deployment zone,
    // whatever the terrain costs (only terrain it can't enter stops it), passing its own side's units but not
    // the enemy's, to the free hex nearest the zone (then the fewest hexes, then by r and q); it must end nearer
    // the zone than it started. The path found, start and end included, its cost the hexes gone.
    def retreatPath(s : GameState, u : CombatUnit) : Option[Path] = {
        def step(h : Hex) = h.neighbors.filter(n => s.board.get(n).exists(_.terrain.passable(u.kind)) && s.at(n).forall(_.owner == u.owner))

        // breadth first, each hex reached by its first path in (r, q) order, so the same every time
        val paths = 1.to(s.rules.retreatHexes).foldLeft(List(List(u.hex))) { (found, _) =>
            val seen = found.map(_.head).toSet
            val next = found.filter(_.size == found.map(_.size).max).flatMap(p => step(p.head).sortBy(n => (n.r, n.q)).map(_ :: p))
            found ++ next.filter(p => !seen.contains(p.head)).groupBy(_.head).values.map(_.head).toList.sortBy(p => (p.head.r, p.head.q))
        }

        val here = s.zoneDistance(u.owner, u.hex)

        here.flatMap { d =>
            paths.filter(p => p.head != u.hex && !s.occupied(p.head))
                .map(p => (s.zoneDistance(u.owner, p.head).get, p))
                .filter(_._1 < d)
                .sortBy { case (z, p) => (z, p.size, p.head.r, p.head.q) }
                .headOption.map { case (_, p) => Path(p.reverse, p.size - 1) }
        }
    }

    // Why the unit can't retreat now, or its path if it can
    def canRetreat(s : GameState, u : CombatUnit) : Either[String, Path] =
        for {
            _ <- Either.cond(u.alive, (), u.id + " is out of action")
            _ <- Either.cond(u.kind == Infantry, (), "only infantry retreats")
            _ <- Either.cond(u.damaged, (), u.id + " isn't hurt")
            _ <- Either.cond(!u.attacked, (), u.id + " has attacked or retreated this turn")
            _ <- Either.cond(!s.inZone(u.owner, u.hex), (), u.id + " is already in its zone")
            p <- retreatPath(s, u).toRight(u.id + " has no way back toward its zone")
        } yield p

    // The state after the unit retreats: it can't move or attack again this turn
    def retreat(s : GameState, id : String) : Either[String, GameState] =
        s.unit(id).toRight("no unit " + id).flatMap(u => canRetreat(s, u).map { p =>
            s.copy(units = s.units + (id -> u.at(p.end).copy(spent = u.movement, attacked = true)))
        })

    // The state after the unit moves to the hex along the cheapest path, spending its cost
    def move(s : GameState, id : String, to : Hex) : Either[String, GameState] =
        s.unit(id) match {
            case None =>
                Left("no unit " + id)
            case Some(u) if !u.alive =>
                Left(id + " is out of action")
            case Some(u) if to == u.hex =>
                Left(id + " is already at " + to)
            case Some(u) if !s.board.contains(to) =>
                Left(to.toString + " is not on the board")
            case Some(u) =>
                path(s, u, to) match {
                    case None =>
                        Left(id + " can't get to " + to)
                    case Some(p) if p.cost > u.movesLeft =>
                        Left(id + " needs " + p.cost + " movement to get to " + to + ", and has " + u.movesLeft)
                    case Some(p) =>
                        Right(s.copy(units = s.units + (id -> u.at(to).copy(spent = u.spent + p.cost))))
                }
        }
}
