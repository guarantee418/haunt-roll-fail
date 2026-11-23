package tactics

// The game state: the board, the units on it and, for a match, the players, their stock and VP, who holds
// which sector, and whose turn and phase it is. Immutable: every change gives a new state.
// At most one unit stands in a hex, and every unit stands on the board; a state that breaks
// either throws IllegalArgumentException when made, so a GameState is always valid.
//
// The match fields all have defaults, so a GameState of a board and units alone works for the movement
// and combat rules. A match starts from GameState.start, and goes on through Turns.applyAction.

case class GameState(
    board : Board,
    units : Map[String, CombatUnit] = Map(),
    players : List[String] = Nil,                 // in turn order
    economy : Map[String, PlayerState] = Map(),
    control : Map[Hex, String] = Map(),           // which player holds each sector captured
    zones : Map[String, Set[Hex]] = Map(),        // where each player deploys
    rules : Rules = Rules(),
    turn : Int = 1,                               // a turn is all players' turns once round
    active : Int = 0,                             // whose turn it is, in players
    phase : Phase = IncomePhase,
    deployed : Int = 0,                           // units deployed so far, for their ids
    winner : Option[String] = None,
    lastReport : Option[CombatReport] = None,     // the last action's attack, if it was one
    exactOrderLimit : Int = 0,                    // the current player's order limit this turn, drawn in their Income phase; 0 before
    ordersUsed : Int = 0                          // the orders they have spent this turn
) {
    units.foreach { case (id, u) => require(id == u.id, "unit " + u.id + " is kept under id " + id) }

    // Who stands where
    val occupants : Map[Hex, CombatUnit] = units.values.map(u => u.hex -> u).toMap

    require(occupants.size == units.size, "two units in one hex: " + units.values.groupBy(_.hex).filter(_._2.size > 1).keys.mkString(", "))
    units.values.foreach(u => require(board.contains(u.hex), "unit " + u.id + " is off the board at " + u.hex))

    require(players.distinct.size == players.size, "a player is listed twice")
    require(players.isEmpty || (active >= 0 && active < players.size), "active must be a player")
    control.foreach { case (h, p) => require(players.contains(p) && board.get(h).exists(_.sector), p + " holds " + h + ", which isn't a sector or isn't theirs") }
    economy.values.foreach(e => require(!e.resources.negative, "resources can't be negative"))
    require(exactOrderLimit >= 0 && ordersUsed >= 0, "orders can't be negative")

    def unit(id : String) : Option[CombatUnit] = units.get(id)
    def at(h : Hex) : Option[CombatUnit] = occupants.get(h)
    def occupied(h : Hex) = occupants.contains(h)

    def current : Option[String] = players.lift(active)
    def player(p : String) : PlayerState = economy.getOrElse(p, PlayerState())
    def unitsOf(p : String) : List[CombatUnit] = units.values.filter(_.owner == p).toList.sortBy(_.id)
    def sectorsOf(p : String) : List[HexTile] = control.toList.filter(_._2 == p).flatMap(c => board.get(c._1)).sortBy(t => (t.q, t.r))
    def over = winner.isDefined

    // The current player's orders left this turn: None when the rules have no order limit, or before it is drawn
    def ordersLeft : Option[Int] = rules.orders.filter(_ => exactOrderLimit > 0).map(_ => (exactOrderLimit - ordersUsed).max(0))
    def ordersDrawn = rules.orders.isEmpty || exactOrderLimit > 0

    // What the player's side sees, and the enemy units it has spotted (vision.scala)
    def visibleHexes(p : String) : Set[Hex] = board.visibleHexes(unitsOf(p))
    def spottedBy(p : String) : List[CombatUnit] = Vision.spottedUnits(this, p)

    // The nearest the hex is to the player's deployment zone, in hexes (0 in it); None with no zone
    def zoneDistance(p : String, h : Hex) : Option[Int] = zones.get(p).filter(_.nonEmpty).map(_.map(_.distance(h)).min)
    def inZone(p : String, h : Hex) = zones.get(p).exists(_.contains(h))

    // Places a new unit, on an empty hex its kind can enter
    def place(u : CombatUnit) : Either[String, GameState] =
        if (units.contains(u.id))
            Left("there is already a unit " + u.id)
        else
        if (!board.contains(u.hex))
            Left(u.hex.toString + " is not on the board")
        else
        if (occupied(u.hex))
            Left(u.hex.toString + " is taken by " + occupants(u.hex).id)
        else
        if (!board.get(u.hex).exists(_.terrain.passable(u.kind)))
            Left(u.kind.name + " can't stand on " + board.get(u.hex).get.terrain.name)
        else
            Right(copy(units = units + (u.id -> u)))

    // The hexes the unit can move to this turn and what each costs; empty for an unknown unit
    def reachable(id : String) : Map[Hex, Int] = unit(id).map(Movement.reachable(this, _)).getOrElse(Map())

    // The cheapest path for the unit to the hex, whatever its movement points left
    def path(id : String, to : Hex) : Option[Path] = unit(id).flatMap(Movement.path(this, _, to))

    def move(id : String, to : Hex) : Either[String, GameState] = Movement.move(this, id, to)

    def canAttack(attacker : String, target : String) = Combat.canAttack(this, attacker, target)

    def attack(attacker : String, target : String, rolls : Seq[Int]) : Either[String, (GameState, CombatReport)] = Combat.attack(this, attacker, target, rolls)

    // Every unit, whoever's, gets its movement points and attack back and is no longer suppressed.
    // A match uses Turns.applyAction instead, which does this for one player at a time.
    def newTurn = copy(units = units.map { case (id, u) => id -> u.copy(spent = 0, suppressed = false, attacked = false) })

    def applyAction(a : TacticsAction) : Either[String, GameState] = Turns.applyAction(this, a)
}

object GameState {
    // A new match: the first player's turn 1 Income phase, with their income and VP already in.
    // The units given start on the board and hold the sectors they stand on.
    def start(board : Board, players : List[String], zones : Map[String, Set[Hex]], units : List[CombatUnit] = Nil, rules : Rules = Rules()) : Either[String, GameState] = {
        val problems =
            (if (players.size < 2) List("a match needs two players or more") else Nil) ++
            (if (players.distinct.size < players.size) List("a player is listed twice") else Nil) ++
            players.filter(p => zones.get(p).forall(_.isEmpty)).map(_ + " has no deployment zone") ++
            zones.toList.flatMap { case (p, z) => z.filter(h => !board.contains(h)).map(h => p + "'s zone has " + h + ", off the board") } ++
            units.filter(u => !players.contains(u.owner)).map(u => u.id + " belongs to no player") ++
            (if (units.map(_.id).distinct.size < units.size) List("two units with one id") else Nil)

        if (problems.nonEmpty)
            Left(problems.mkString("; "))
        else
            scala.util.Try(GameState(board, units.map(u => u.id -> u).toMap, players, players.map(_ -> PlayerState(rules.start)).toMap, zones = zones, rules = rules)).toEither.left.map(_.getMessage).map { s =>
                Turns.beginTurn(units.foldLeft(s)((s, u) => Turns.capture(s, u.hex)))
            }
    }
}
