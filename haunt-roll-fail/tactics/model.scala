package tactics

// The data models: movement kinds, terrain, cover, tiles and units. No game rules here
// beyond what each terrain costs to enter.

// How a unit moves, which decides what terrain costs it
sealed abstract class MoveKind(val name : String)

case object Infantry extends MoveKind("Infantry")
case object Vehicle extends MoveKind("Vehicle")

object MoveKind {
    val all : List[MoveKind] = List(Infantry, Vehicle)
}

// `infantry` and `vehicle` are the movement points it costs to enter a hex of the terrain, None where it can't go
sealed abstract class Terrain(val name : String, val blocking : Boolean, val infantry : Option[Int], val vehicle : Option[Int]) {
    def cost(kind : MoveKind) : Option[Int] = kind match {
        case Infantry => infantry
        case Vehicle => vehicle
    }

    def passable(kind : MoveKind) = cost(kind).isDefined
}

case object Plains extends Terrain("Plains", false, Some(1), Some(1))
case object Forest extends Terrain("Forest", false, Some(2), Some(3))
case object Hills extends Terrain("Hills", false, Some(2), Some(2))
case object Water extends Terrain("Water", false, None, None)
case object Mountain extends Terrain("Mountain", true, None, None)
case object Building extends Terrain("Building", true, Some(2), None)

object Terrain {
    val all : List[Terrain] = List(Plains, Forest, Hills, Water, Mountain, Building)
}

sealed abstract class Cover(val name : String, val level : Int) extends Ordered[Cover] {
    def compare(o : Cover) = level.compare(o.level)
}

case object NoCover extends Cover("None", 0)
case object LightCover extends Cover("Light", 1)
case object HeavyCover extends Cover("Heavy", 2)

object Cover {
    val all : List[Cover] = List(NoCover, LightCover, HeavyCover)
}

// `income` is what the tile gives its controller at the start of each of their turns, and `vp` the victory
// points it scores them then; a tile with either is a sector, which units capture
case class HexTile(q : Int, r : Int, terrain : Terrain = Plains, cover : Cover = NoCover, income : Resources = Resources.none, vp : Option[Int] = None) {
    require(!income.negative && vp.forall(_ >= 0), "a tile's income and VP can't be negative")

    def hex = Hex(q, r)
    def blocksSight = terrain.blocking
    def sector = income != Resources.none || vp.exists(_ > 0)
}

// Called CombatUnit, not Unit, as a class named Unit would hide Scala's own Unit type in this package.
// `movement` is the unit's movement points each turn and `spent` how many it has used this turn.
// `weapon` is what it attacks with (None: it can't attack), `suppressed` whether fire has pinned it down,
// and `attacked` whether it has attacked this turn.
case class CombatUnit(id : String, owner : String, health : Int, movement : Int, armor : Int, q : Int, r : Int, kind : MoveKind = Infantry, spent : Int = 0, weapon : Option[Weapon] = None, suppressed : Boolean = false, attacked : Boolean = false) {
    require(health >= 0, "health must be at least 0, got " + health)
    require(movement >= 0, "movement must be at least 0, got " + movement)
    require(armor >= 0, "armor must be at least 0, got " + armor)
    require(spent >= 0 && spent <= movement, "spent must be 0 to " + movement + ", got " + spent)

    def armed(w : Weapon) = copy(weapon = Some(w))

    def hex = Hex(q, r)
    def at(h : Hex) = copy(q = h.q, r = h.r)
    def alive = health > 0
    def movesLeft = movement - spent
}
