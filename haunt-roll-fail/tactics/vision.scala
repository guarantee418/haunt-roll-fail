package tactics

// Vision: what each side sees and which enemy units it has spotted. Pure functions of a GameState.
//
// Rules:
// - A unit sees the hexes within its sightRange that are in its line of sight (Board.lineOfSight: blocking
//   terrain between; units don't block). A side sees every hex any of its living units sees (Board.visibleHexes).
// - An enemy unit is spotted when it stands in a hex the side sees, unless it is camouflaged: a camouflaged unit is
//   spotted only by a unit next to it (1 hex), or by one with a sight range of 4 or more that sees its hex.
// - Spotting is shared: a unit may attack what any unit of its side has spotted (Combat, with the rules' fog).

object Vision {
    // the sight range that sees through camouflage
    val keenSight = 4

    // Whether the unit sees the target
    def spots(s : GameState, u : CombatUnit, t : CombatUnit) : Boolean = {
        val d = u.hex.distance(t.hex)

        u.alive && t.alive && s.board.contains(u.hex) && s.board.contains(t.hex) &&
        d <= u.sightRange && s.board.lineOfSight(u.hex, t.hex) &&
        (!t.camouflaged || d <= 1 || u.sightRange >= keenSight)
    }

    // Whether the player's side has spotted the unit (their own units always)
    def spotted(s : GameState, p : String, t : CombatUnit) : Boolean =
        t.owner == p || s.unitsOf(p).exists(spots(s, _, t))

    // The other players' units the player's side has spotted, by id
    def spottedUnits(s : GameState, p : String) : List[CombatUnit] =
        s.units.values.filter(t => t.owner != p && spotted(s, p, t)).toList.sortBy(_.id)

    // What the player sees of the units: their own and the enemy's spotted ones
    def seenUnits(s : GameState, p : String) : List[CombatUnit] = s.unitsOf(p) ++ spottedUnits(s, p)
}
