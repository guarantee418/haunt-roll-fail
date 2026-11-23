package tactics

// The game state: the board and the units on it. Immutable: every change gives a new state.
// At most one unit stands in a hex, and every unit stands on the board; a state that breaks
// either throws IllegalArgumentException when made, so a GameState is always valid.

case class GameState(board : Board, units : Map[String, CombatUnit] = Map()) {
    units.foreach { case (id, u) => require(id == u.id, "unit " + u.id + " is kept under id " + id) }

    // Who stands where
    val occupants : Map[Hex, CombatUnit] = units.values.map(u => u.hex -> u).toMap

    require(occupants.size == units.size, "two units in one hex: " + units.values.groupBy(_.hex).filter(_._2.size > 1).keys.mkString(", "))
    units.values.foreach(u => require(board.contains(u.hex), "unit " + u.id + " is off the board at " + u.hex))

    def unit(id : String) : Option[CombatUnit] = units.get(id)
    def at(h : Hex) : Option[CombatUnit] = occupants.get(h)
    def occupied(h : Hex) = occupants.contains(h)

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

    // Every unit gets its movement points back
    def newTurn = copy(units = units.map { case (id, u) => id -> u.copy(spent = 0) })
}
