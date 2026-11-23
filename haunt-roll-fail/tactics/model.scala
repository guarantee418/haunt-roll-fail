package tactics

// The data models: terrain, cover, tiles and units. No game rules here.

sealed abstract class Terrain(val name : String, val blocking : Boolean)

case object Plains extends Terrain("Plains", false)
case object Forest extends Terrain("Forest", false)
case object Hills extends Terrain("Hills", false)
case object Water extends Terrain("Water", false)
case object Mountain extends Terrain("Mountain", true)
case object Building extends Terrain("Building", true)

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

case class HexTile(q : Int, r : Int, terrain : Terrain = Plains, cover : Cover = NoCover, resources : Option[Int] = None, vp : Option[Int] = None) {
    def hex = Hex(q, r)
    def blocksSight = terrain.blocking
}

// Called CombatUnit, not Unit, as a class named Unit would hide Scala's own Unit type in this package
case class CombatUnit(id : String, owner : String, health : Int, movement : Int, armor : Int, q : Int, r : Int) {
    require(health >= 0, "health must be at least 0, got " + health)
    require(movement >= 0, "movement must be at least 0, got " + movement)
    require(armor >= 0, "armor must be at least 0, got " + armor)

    def hex = Hex(q, r)
    def at(h : Hex) = copy(q = h.q, r = h.r)
    def alive = health > 0
}
