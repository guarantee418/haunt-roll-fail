package nort
//
//
//
//
import hrf.colmat._
import hrf.logger._
//
//
//
//


// Tile sides in clockwise order
trait Side extends NamedToString with Record {
    def index : Int
    def opposite : Side = Side.all((index + 2) % 4)
    def rotate(r : Int) : Side = Side.all((index + r) % 4)
    def dx : Int
    def dy : Int
}

case object North extends Side { val index = 0; val dx = 0; val dy = -1 }
case object East extends Side { val index = 1; val dx = 1; val dy = 0 }
case object South extends Side { val index = 2; val dx = 0; val dy = 1 }
case object West extends Side { val index = 3; val dx = -1; val dy = 0 }

object Side {
    val all : $[Side] = $(North, East, South, West)
}


trait SpaceKind extends NamedToString with Record
// Small building space
case object SmallSpace extends SpaceKind
// Small building space with the Carved Stone icon
case object CarvedSpace extends SpaceKind
case object LargeSpace extends SpaceKind

// Positions are fractions of the tile, from its top left corner
case class SpaceSpec(kind : SpaceKind, x : Double, y : Double)

case class AreaSpec(id : String, edges : $[Side], food : Int, wood : Int, lore : Int, lair : Boolean, spaces : $[SpaceSpec], x : Double, y : Double)

case class BorderSpec(a : String, b : String, rough : Boolean)

case class TileSpec(id : String, areas : $[AreaSpec], borders : $[BorderSpec]) {
    def area(a : String) = areas.find(_.id == a).get
    // The area that owns side s when the tile is turned r quarter turns clockwise
    def areaAt(s : Side, r : Int) : AreaSpec = areas.find(_.edges.exists(_.rotate(r) == s)).get
}


// Every border ends at a tile corner, so each tile side belongs to a single area,
// and areas of neighbouring tiles join into one territory across the shared side
object Tiles {
    trait Feature
    case class Res(food : Int, wood : Int, lore : Int) extends Feature
    case object Lair extends Feature
    case class Space(s : SpaceSpec) extends Feature

    val food = Res(1, 0, 0)
    val wood = Res(0, 1, 0)
    val lore = Res(0, 0, 1)
    val lair = Lair
    def small(x : Double, y : Double) = Space(SpaceSpec(SmallSpace, x, y))
    def carved(x : Double, y : Double) = Space(SpaceSpec(CarvedSpace, x, y))
    def large(x : Double, y : Double) = Space(SpaceSpec(LargeSpace, x, y))

    // edges: letters of the sides this area owns, e.g. "NW"
    def area(id : String, edges : String, x : Double, y : Double)(features : Feature*) = {
        val f = features.$
        val r = f.of[Res]
        AreaSpec(id, edges.toList./(c => Side.all.find(_.name.head == c).get), r./(_.food).sum, r./(_.wood).sum, r./(_.lore).sum, f.has(Lair), f.of[Space]./(_.s), x, y)
    }

    def border(a : String, b : String) = BorderSpec(a, b, false)
    def rough(a : String, b : String) = BorderSpec(a, b, true)

    def tile(id : String)(areas : AreaSpec*)(borders : BorderSpec*) = TileSpec(id, areas.$, borders.$)

    val start = tile("start")(
        area("n", "N", 0.5, 0.15)(),
        area("e", "E", 0.78, 0.5)(),
        area("s", "S", 0.5, 0.85)(),
        area("w", "W", 0.15, 0.5)(),
    )(border("n", "w"), border("n", "e"), border("w", "e"), border("w", "s"), border("e", "s"))

    // The second starting tile for five players goes next to the first, food in the middle territory
    val start5 = tile("start-5")(
        area("n", "N", 0.55, 0.2)(),
        area("e", "E", 0.85, 0.5)(),
        area("s", "S", 0.6, 0.85)(),
        area("w", "W", 0.2, 0.45)(food),
    )(border("n", "w"), border("n", "e"), border("n", "s"), border("w", "s"), border("e", "s"))

    val regular : $[TileSpec] = $(
        tile("tile-01")(
            area("n", "N", 0.5, 0.15)(food),
            area("e", "E", 0.85, 0.3)(small(0.82, 0.53)),
            area("s", "S", 0.6, 0.9)(small(0.39, 0.81)),
            area("w", "W", 0.15, 0.6)(lair),
        )(border("n", "w"), border("n", "e"), border("w", "e"), border("w", "s"), rough("e", "s")),
        tile("tile-02")(
            area("n", "N", 0.68, 0.32)(small(0.45, 0.2)),
            area("s", "ESW", 0.35, 0.85)(food, small(0.76, 0.68)),
        )(border("n", "s")),
        tile("tile-03")(
            area("n", "N", 0.5, 0.18)(lair),
            area("e", "E", 0.7, 0.4)(carved(0.85, 0.56)),
            area("s", "S", 0.35, 0.85)(small(0.5, 0.79)),
            area("w", "W", 0.12, 0.6)(food),
        )(border("n", "w"), rough("n", "e"), rough("w", "s"), border("e", "s"), rough("w", "e")),
        tile("tile-04")(
            area("n", "NW", 0.55, 0.15)(large(0.27, 0.32)),
            area("e", "E", 0.88, 0.4)(lair),
            area("s", "S", 0.62, 0.92)(carved(0.44, 0.83)),
        )(border("n", "e"), border("n", "s"), border("e", "s")),
        tile("tile-05")(
            area("n", "NW", 0.2, 0.55)(carved(0.27, 0.25)),
            area("s", "ES", 0.45, 0.6)(large(0.71, 0.67)),
        )(border("n", "s")),
        tile("tile-06")(
            area("n", "N", 0.7, 0.15)(small(0.45, 0.2)),
            area("e", "E", 0.9, 0.45)(small(0.8, 0.6)),
            area("s", "S", 0.5, 0.75)(lore),
            area("w", "W", 0.1, 0.45)(wood),
        )(border("n", "w"), border("n", "e"), border("w", "e"), rough("w", "s"), border("e", "s")),
        tile("tile-07")(
            area("n", "N", 0.25, 0.12)(carved(0.53, 0.2)),
            area("m", "WE", 0.45, 0.48)(small(0.17, 0.5), food),
            area("s", "S", 0.15, 0.9)(lair),
        )(border("n", "m"), border("m", "s")),
        tile("tile-08")(
            area("n", "N", 0.85, 0.08)(),
            area("m", "WE", 0.3, 0.4)(small(0.68, 0.37)),
            area("s", "S", 0.25, 0.85)(small(0.47, 0.77)),
        )(rough("n", "m"), border("m", "s")),
        tile("tile-09")(
            area("n", "NW", 0.35, 0.45)(small(0.2, 0.2), food),
            area("e", "E", 0.9, 0.3)(carved(0.85, 0.5)),
            area("s", "S", 0.6, 0.65)(small(0.35, 0.79)),
        )(border("n", "e"), border("n", "s"), border("e", "s")),
        tile("tile-10")(
            area("n", "NW", 0.5, 0.3)(food, small(0.2, 0.39)),
            area("e", "E", 0.88, 0.55)(small(0.79, 0.58)),
            area("s", "S", 0.15, 0.85)(small(0.4, 0.82)),
        )(border("n", "e"), border("n", "s"), border("e", "s")),
        tile("tile-11")(
            area("n", "N", 0.45, 0.2)(wood, small(0.6, 0.2)),
            area("s", "ESW", 0.2, 0.55)(large(0.5, 0.7)),
        )(border("n", "s")),
        tile("tile-12")(
            area("n", "N", 0.75, 0.15)(small(0.5, 0.2)),
            area("m", "WE", 0.45, 0.55)(wood, small(0.75, 0.6)),
            area("s", "S", 0.5, 0.9)(),
        )(border("n", "m"), border("m", "s")),
        tile("tile-13")(
            area("a", "NESW", 0.2, 0.5)(large(0.5, 0.45), wood),
        )(),
        tile("tile-14")(
            area("n", "N", 0.85, 0.08)(small(0.47, 0.17)),
            area("m", "WE", 0.35, 0.47)(small(0.83, 0.45)),
            area("s", "S", 0.3, 0.88)(food),
        )(rough("n", "m"), border("m", "s")),
        tile("tile-15")(
            area("n", "NW", 0.15, 0.55)(large(0.27, 0.28)),
            area("e", "E", 0.88, 0.3)(small(0.82, 0.58)),
            area("s", "S", 0.55, 0.88)(lore),
        )(border("n", "e"), border("n", "s"), border("e", "s")),
        tile("tile-16")(
            area("n", "NW", 0.3, 0.5)(small(0.3, 0.29), food),
            area("e", "E", 0.7, 0.5)(small(0.81, 0.55)),
            area("s", "S", 0.25, 0.9)(lore),
        )(border("n", "e"), rough("n", "s"), border("e", "s")),
        tile("tile-17")(
            area("n", "N", 0.3, 0.12)(lair),
            area("s", "ESW", 0.3, 0.6)(large(0.7, 0.71)),
        )(border("n", "s")),
        tile("tile-18")(
            area("n", "NW", 0.15, 0.45)(carved(0.34, 0.25)),
            area("e", "E", 0.75, 0.3)(small(0.81, 0.5)),
            area("s", "S", 0.6, 0.9)(wood),
        )(border("n", "e"), rough("n", "s"), rough("e", "s")),
        tile("tile-19")(
            area("n", "NW", 0.15, 0.5)(small(0.29, 0.3)),
            area("e", "E", 0.8, 0.45)(lore),
            area("s", "S", 0.8, 0.85)(small(0.5, 0.84)),
        )(rough("n", "e"), rough("n", "s"), border("e", "s")),
        tile("tile-20")(
            area("n", "NW", 0.2, 0.6)(food, small(0.21, 0.41)),
            area("s", "ES", 0.85, 0.45)(lore, small(0.76, 0.74)),
        )(rough("n", "s")),
        tile("tile-21")(
            area("n", "NW", 0.25, 0.65)(food, small(0.48, 0.21)),
            area("s", "ES", 0.85, 0.25)(large(0.59, 0.74)),
        )(border("n", "s")),
        tile("tile-22")(
            area("n", "N", 0.6, 0.12)(small(0.42, 0.18)),
            area("e", "E", 0.6, 0.6)(lair),
            area("s", "S", 0.35, 0.88)(lore),
            area("w", "W", 0.12, 0.75)(carved(0.16, 0.48)),
        )(rough("n", "w"), rough("n", "e"), border("w", "e"), border("w", "s"), border("e", "s")),
        tile("tile-23")(
            area("n", "NW", 0.25, 0.45)(small(0.4, 0.19), lore),
            area("e", "E", 0.85, 0.4)(small(0.83, 0.59)),
            area("s", "S", 0.8, 0.85)(lair),
        )(border("n", "e"), border("n", "s"), border("e", "s")),
        tile("tile-24")(
            area("n", "NW", 0.5, 0.15)(large(0.24, 0.35)),
            area("e", "E", 0.85, 0.45)(small(0.79, 0.59)),
            area("s", "S", 0.6, 0.9)(small(0.36, 0.82)),
        )(border("n", "e"), border("n", "s"), border("e", "s")),
        tile("tile-25")(
            area("n", "NW", 0.15, 0.55)(large(0.37, 0.25)),
            area("e", "E", 0.7, 0.4)(lair),
            area("s", "S", 0.15, 0.85)(small(0.38, 0.82)),
        )(border("n", "e"), border("n", "s"), rough("e", "s")),
        tile("tile-26")(
            area("n", "NW", 0.3, 0.45)(food),
            area("e", "E", 0.88, 0.25)(small(0.8, 0.45)),
            area("s", "S", 0.6, 0.75)(small(0.3, 0.82), wood),
        )(border("n", "e"), rough("n", "s"), border("e", "s")),
        tile("tile-27")(
            area("n", "NW", 0.15, 0.55)(large(0.25, 0.3)),
            area("e", "E", 0.9, 0.4)(wood, small(0.82, 0.6)),
            area("s", "S", 0.45, 0.9)(lair),
        )(border("n", "e"), border("n", "s"), border("e", "s")),
        tile("tile-28")(
            area("n", "NW", 0.15, 0.55)(large(0.25, 0.28)),
            area("s", "ES", 0.85, 0.55)(wood, small(0.47, 0.78)),
        )(border("n", "s")),
        tile("tile-29")(
            area("n", "N", 0.4, 0.08)(wood),
            area("e", "ES", 0.55, 0.45)(large(0.75, 0.7)),
            area("w", "W", 0.1, 0.55)(small(0.22, 0.4)),
        )(border("n", "w"), border("n", "e"), border("w", "e")),
        tile("tile-30")(
            area("n", "N", 0.7, 0.2)(small(0.45, 0.17)),
            area("e", "E", 0.85, 0.65)(wood),
            area("s", "S", 0.65, 0.9)(small(0.38, 0.8)),
            area("w", "W", 0.1, 0.65)(lair),
        )(border("n", "w"), border("n", "e"), border("w", "e"), border("w", "s"), border("e", "s")),
        tile("tile-31")(
            area("n", "NW", 0.2, 0.55)(large(0.22, 0.26)),
            area("e", "E", 0.75, 0.35)(small(0.8, 0.6)),
            area("s", "S", 0.6, 0.9)(lore),
        )(rough("n", "e"), rough("n", "s"), border("e", "s")),
        tile("tile-32")(
            area("n", "NW", 0.2, 0.55)(small(0.3, 0.32)),
            area("e", "E", 0.88, 0.3)(wood),
            area("s", "S", 0.6, 0.85)(small(0.41, 0.82)),
        )(border("n", "e"), border("n", "s"), rough("e", "s")),
        tile("tile-33")(
            area("n", "N", 0.15, 0.25)(carved(0.45, 0.15)),
            area("m", "WE", 0.6, 0.5)(large(0.18, 0.56)),
            area("s", "S", 0.7, 0.9)(lair),
        )(rough("n", "m"), border("m", "s")),
    )

    val all : $[TileSpec] = start +: start5 +: regular

    val byId : Map[String, TileSpec] = all./(t => t.id -> t).toMap

    def apply(id : String) = byId(id)
}
