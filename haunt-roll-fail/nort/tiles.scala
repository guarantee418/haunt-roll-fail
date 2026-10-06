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

// x, y: the territory number; ux, uy: the unit markers
case class AreaSpec(id : String, edges : $[Side], food : Int, wood : Int, lore : Int, lair : Boolean, spaces : $[SpaceSpec], x : Double, y : Double, ux : Double, uy : Double)

// impassable: a Wilderness border (orange line, or the art of a lake, swamp or peaks) nothing crosses
case class BorderSpec(a : String, b : String, rough : Boolean, impassable : Boolean = false)

case class TileSpec(id : String, areas : $[AreaSpec], borders : $[BorderSpec]) {
    def area(a : String) = areas.find(_.id == a).get
    // The area that owns side s when the tile is turned r quarter turns clockwise
    def areaAt(s : Side, r : Int) : AreaSpec = areaOn(s, r).get
    // None for a side no area owns: the sea of the Beach tiles (Sea module), which joins nothing and lets no tile in
    def areaOn(s : Side, r : Int) : |[AreaSpec] = areas.find(_.edges.exists(_.rotate(r) == s))
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

    // edges: letters of the sides this area owns, e.g. "NW"; units go right of the number unless ux, uy are given.
    // Every ux, uy is set so the unit figure (with its count and Kaija) stays clear of the resource icons,
    // building spaces, territory numbers and the other areas' figures on the tile
    def area(id : String, edges : String, x : Double, y : Double, ux : Double = -1, uy : Double = -1)(features : Feature*) = {
        val f = features.$
        val r = f.of[Res]
        AreaSpec(id, edges.toList./(c => Side.all.find(_.name.head == c).get), r./(_.food).sum, r./(_.wood).sum, r./(_.lore).sum, f.has(Lair), f.of[Space]./(_.s), x, y, (ux < 0).?(x + 0.16).|(ux), (uy < 0).?(y).|(uy))
    }

    def border(a : String, b : String) = BorderSpec(a, b, false)
    def rough(a : String, b : String) = BorderSpec(a, b, true)
    def wall(a : String, b : String) = BorderSpec(a, b, false, true)

    def tile(id : String)(areas : AreaSpec*)(borders : BorderSpec*) = TileSpec(id, areas.$, borders.$)

    val start = tile("start")(
        area("n", "N", 0.5, 0.15, 0.66, 0.15)(),
        area("e", "E", 0.65, 0.5, 0.84, 0.5)(),
        area("s", "S", 0.5, 0.85, 0.66, 0.85)(),
        area("w", "W", 0.15, 0.5, 0.31, 0.5)(),
    )(border("n", "w"), border("n", "e"), border("w", "e"), border("w", "s"), border("e", "s"))

    // The second starting tile for five players goes next to the first, food in the middle territory
    val start5 = tile("start-5")(
        area("n", "N", 0.42, 0.12, 0.6, 0.14)(),
        area("e", "E", 0.88, 0.5, 0.7, 0.52)(),
        area("s", "S", 0.38, 0.85, 0.55, 0.85)(),
        area("w", "W", 0.25, 0.5, 0.13, 0.32)(food),
    )(border("n", "w"), border("n", "e"), border("n", "s"), border("w", "s"), border("e", "s"))

    val regular : $[TileSpec] = $(
        tile("tile-01")(
            area("n", "N", 0.5, 0.15, 0.72, 0.15)(food),
            area("e", "E", 0.9, 0.18, 0.59, 0.47)(small(0.82, 0.53)),
            area("s", "S", 0.53, 0.73, 0.69, 0.88)(small(0.39, 0.81)),
            area("w", "W", 0.1, 0.72, 0.2, 0.42)(lair),
        )(border("n", "w"), border("n", "e"), border("w", "e"), border("w", "s"), rough("e", "s")),
        tile("tile-02")(
            area("n", "N", 0.28, 0.12, 0.66, 0.13)(small(0.45, 0.2)),
            area("s", "ESW", 0.35, 0.85, 0.51, 0.85)(food, small(0.76, 0.68)),
        )(border("n", "s")),
        tile("tile-03")(
            area("n", "N", 0.38, 0.08, 0.5, 0.27)(lair),
            area("e", "E", 0.88, 0.18, 0.62, 0.52)(carved(0.85, 0.56)),
            area("s", "S", 0.22, 0.92, 0.71, 0.88)(small(0.5, 0.79)),
            area("w", "W", 0.1, 0.62, 0.1, 0.29)(food),
        )(border("n", "w"), rough("n", "e"), rough("w", "s"), border("e", "s"), rough("w", "e")),
        tile("tile-04")(
            area("n", "NW", 0.55, 0.12, 0.52, 0.31)(large(0.27, 0.32)),
            area("e", "E", 0.6, 0.52, 0.85, 0.28)(lair),
            area("s", "S", 0.25, 0.9, 0.66, 0.88)(carved(0.44, 0.83)),
        )(border("n", "e"), border("n", "s"), border("e", "s")),
        tile("tile-05")(
            area("n", "NW", 0.14, 0.48, 0.48, 0.26)(carved(0.27, 0.25)),
            area("s", "ES", 0.3, 0.82, 0.85, 0.38)(large(0.71, 0.67)),
        )(border("n", "s")),
        tile("tile-06")(
            area("n", "N", 0.24, 0.08, 0.66, 0.17)(small(0.45, 0.2)),
            area("e", "E", 0.68, 0.52, 0.86, 0.84)(small(0.8, 0.6)),
            area("s", "S", 0.3, 0.92, 0.44, 0.74)(lore),
            area("w", "W", 0.12, 0.4, 0.1, 0.73)(wood),
        )(border("n", "w"), border("n", "e"), border("w", "e"), rough("w", "s"), border("e", "s")),
        tile("tile-07")(
            area("n", "N", 0.25, 0.12, 0.74, 0.22)(carved(0.53, 0.2)),
            area("m", "WE", 0.45, 0.48, 0.61, 0.48)(small(0.17, 0.5), food),
            area("s", "S", 0.15, 0.9, 0.31, 0.88)(lair),
        )(border("n", "m"), border("m", "s")),
        tile("tile-08")(
            area("n", "N", 0.4, 0.07, 0.57, 0.13)(),
            area("m", "WE", 0.3, 0.4, 0.12, 0.4)(small(0.68, 0.37)),
            area("s", "S", 0.25, 0.85, 0.68, 0.88)(small(0.47, 0.77)),
        )(rough("n", "m"), border("m", "s")),
        tile("tile-09")(
            area("n", "NW", 0.15, 0.45, 0.45, 0.36)(small(0.2, 0.2), food),
            area("e", "E", 0.7, 0.64, 0.86, 0.25)(carved(0.85, 0.5)),
            area("s", "S", 0.18, 0.9, 0.6, 0.88)(small(0.35, 0.79)),
        )(border("n", "e"), border("n", "s"), border("e", "s")),
        tile("tile-10")(
            area("n", "NW", 0.4, 0.42, 0.4, 0.17)(food, small(0.2, 0.39)),
            area("e", "E", 0.93, 0.74, 0.86, 0.32)(small(0.79, 0.58)),
            area("s", "S", 0.25, 0.95, 0.64, 0.88)(small(0.4, 0.82)),
        )(border("n", "e"), border("n", "s"), border("e", "s")),
        tile("tile-11")(
            area("n", "N", 0.46, 0.06, 0.81, 0.24)(wood, small(0.6, 0.2)),
            area("s", "ESW", 0.2, 0.55, 0.75, 0.6)(large(0.5, 0.7)),
        )(border("n", "s")),
        tile("tile-12")(
            area("n", "N", 0.26, 0.1, 0.71, 0.28)(small(0.5, 0.2)),
            area("m", "WE", 0.32, 0.5, 0.5, 0.58)(wood, small(0.75, 0.6)),
            area("s", "S", 0.5, 0.9, 0.66, 0.88)(),
        )(border("n", "m"), border("m", "s")),
        tile("tile-13")(
            area("a", "NESW", 0.2, 0.5, 0.72, 0.45)(large(0.44, 0.44), wood),
        )(),
        tile("tile-14")(
            area("n", "N", 0.26, 0.1, 0.68, 0.13)(small(0.47, 0.17)),
            area("m", "WE", 0.3, 0.47, 0.55, 0.48)(small(0.83, 0.45)),
            area("s", "S", 0.3, 0.88, 0.68, 0.85)(food),
        )(rough("n", "m"), border("m", "s")),
        tile("tile-15")(
            area("n", "NW", 0.15, 0.55, 0.5, 0.22)(large(0.27, 0.28)),
            area("e", "E", 0.65, 0.47, 0.84, 0.28)(small(0.82, 0.58)),
            area("s", "S", 0.66, 0.92, 0.46, 0.86)(lore),
        )(border("n", "e"), border("n", "s"), border("e", "s")),
        tile("tile-16")(
            area("n", "NW", 0.3, 0.5, 0.12, 0.53)(small(0.3, 0.29), food),
            area("e", "E", 0.6, 0.53, 0.86, 0.28)(small(0.81, 0.55)),
            area("s", "S", 0.36, 0.75, 0.18, 0.88)(lore),
        )(border("n", "e"), rough("n", "s"), border("e", "s")),
        tile("tile-17")(
            area("n", "N", 0.3, 0.12, 0.46, 0.13)(lair),
            area("s", "ESW", 0.3, 0.6, 0.46, 0.6)(large(0.7, 0.71)),
        )(border("n", "s")),
        tile("tile-18")(
            area("n", "NW", 0.1, 0.45, 0.3, 0.49)(carved(0.34, 0.25)),
            area("e", "E", 0.63, 0.52, 0.8, 0.25)(small(0.81, 0.5)),
            area("s", "S", 0.24, 0.9, 0.58, 0.84)(wood),
        )(border("n", "e"), rough("n", "s"), rough("e", "s")),
        tile("tile-19")(
            area("n", "NW", 0.08, 0.62, 0.28, 0.54)(small(0.29, 0.3)),
            area("e", "E", 0.93, 0.5, 0.74, 0.52)(lore),
            area("s", "S", 0.2, 0.92, 0.71, 0.88)(small(0.5, 0.84)),
        )(rough("n", "e"), rough("n", "s"), border("e", "s")),
        tile("tile-20")(
            area("n", "NW", 0.2, 0.6, 0.44, 0.25)(food, small(0.21, 0.41)),
            area("s", "ES", 0.92, 0.3, 0.74, 0.46)(lore, small(0.76, 0.74)),
        )(rough("n", "s")),
        tile("tile-21")(
            area("n", "NW", 0.2, 0.52, 0.36, 0.45)(food, small(0.48, 0.21)),
            area("s", "ES", 0.86, 0.2, 0.86, 0.42)(large(0.59, 0.74)),
        )(border("n", "s")),
        tile("tile-22")(
            area("n", "N", 0.62, 0.1, 0.63, 0.29)(small(0.42, 0.18)),
            area("e", "E", 0.9, 0.3, 0.78, 0.54)(lair),
            area("s", "S", 0.75, 0.92, 0.37, 0.8)(lore),
            area("w", "W", 0.08, 0.7, 0.1, 0.26)(carved(0.16, 0.48)),
        )(rough("n", "w"), rough("n", "e"), border("w", "e"), border("w", "s"), border("e", "s")),
        tile("tile-23")(
            area("n", "NW", 0.25, 0.45, 0.61, 0.3)(small(0.4, 0.19), lore),
            area("e", "E", 0.6, 0.54, 0.86, 0.28)(small(0.83, 0.59)),
            area("s", "S", 0.16, 0.92, 0.6, 0.88)(lair),
        )(border("n", "e"), border("n", "s"), border("e", "s")),
        tile("tile-24")(
            area("n", "NW", 0.48, 0.08, 0.52, 0.28)(large(0.24, 0.35)),
            area("e", "E", 0.66, 0.5, 0.85, 0.3)(small(0.79, 0.59)),
            area("s", "S", 0.2, 0.93, 0.6, 0.88)(small(0.36, 0.82)),
        )(border("n", "e"), border("n", "s"), border("e", "s")),
        tile("tile-25")(
            area("n", "NW", 0.55, 0.12, 0.71, 0.15)(large(0.33, 0.24)),
            area("e", "E", 0.72, 0.4, 0.82, 0.68)(lair),
            area("s", "S", 0.33, 0.6, 0.59, 0.88)(small(0.38, 0.82)),
        )(border("n", "e"), border("n", "s"), rough("e", "s")),
        tile("tile-26")(
            area("n", "NW", 0.58, 0.24, 0.27, 0.43)(food),
            area("e", "E", 0.93, 0.7, 0.86, 0.24)(small(0.78, 0.5)),
            area("s", "S", 0.6, 0.75, 0.55, 0.57)(small(0.3, 0.82), wood),
        )(border("n", "e"), rough("n", "s"), border("e", "s")),
        tile("tile-27")(
            area("n", "NW", 0.12, 0.52, 0.36, 0.55)(large(0.25, 0.3)),
            area("e", "E", 0.65, 0.48, 0.64, 0.3)(wood, small(0.82, 0.6)),
            area("s", "S", 0.38, 0.92, 0.64, 0.88)(lair),
        )(border("n", "e"), border("n", "s"), border("e", "s")),
        tile("tile-28")(
            area("n", "NW", 0.15, 0.55, 0.5, 0.17)(large(0.25, 0.28)),
            area("s", "ES", 0.88, 0.24, 0.76, 0.46)(wood, small(0.47, 0.78)),
        )(border("n", "s")),
        tile("tile-29")(
            area("n", "N", 0.7, 0.07, 0.41, 0.13)(wood),
            area("e", "ES", 0.45, 0.62, 0.8, 0.38)(large(0.72, 0.7)),
            area("w", "W", 0.1, 0.2, 0.1, 0.68)(small(0.2, 0.38)),
        )(border("n", "w"), border("n", "e"), border("w", "e")),
        tile("tile-30")(
            area("n", "N", 0.53, 0.32, 0.66, 0.13)(small(0.45, 0.17)),
            area("e", "E", 0.88, 0.65, 0.86, 0.84)(wood),
            area("s", "S", 0.2, 0.92, 0.6, 0.85)(small(0.38, 0.8)),
            area("w", "W", 0.08, 0.68, 0.3, 0.48)(lair),
        )(border("n", "w"), border("n", "e"), border("w", "e"), border("w", "s"), border("e", "s")),
        tile("tile-31")(
            area("n", "NW", 0.2, 0.55, 0.45, 0.25)(large(0.22, 0.26)),
            area("e", "E", 0.66, 0.5, 0.82, 0.28)(small(0.8, 0.6)),
            area("s", "S", 0.24, 0.94, 0.43, 0.86)(lore),
        )(rough("n", "e"), rough("n", "s"), border("e", "s")),
        tile("tile-32")(
            area("n", "NW", 0.2, 0.55, 0.55, 0.2)(small(0.3, 0.32)),
            area("e", "E", 0.88, 0.3, 0.88, 0.68)(wood),
            area("s", "S", 0.18, 0.92, 0.62, 0.85)(small(0.41, 0.82)),
        )(border("n", "e"), border("n", "s"), rough("e", "s")),
        tile("tile-33")(
            area("n", "N", 0.28, 0.1, 0.66, 0.18)(carved(0.45, 0.15)),
            area("m", "WE", 0.6, 0.55, 0.76, 0.5)(large(0.18, 0.56)),
            area("s", "S", 0.48, 0.8, 0.67, 0.87)(lair),
        )(rough("n", "m"), border("m", "s")),
    )

    // Wilderness expansion, Environment tiles module (wilderness.scala has their rules). Areas with no edges
    // are enclosed by the other areas of their tile (the Wyvern's Den, the Swamp)
    val environment : $[TileSpec] = $(
        // The lake is impassable; its four shores meet at the corners
        tile("wild-lake")(
            area("n", "N", 0.4, 0.06, 0.64, 0.09)(),
            area("e", "E", 0.93, 0.42, 0.89, 0.64)(),
            area("s", "S", 0.6, 0.94, 0.36, 0.9)(),
            area("w", "W", 0.07, 0.62, 0.11, 0.36)(),
        )(border("n", "e"), border("e", "s"), border("s", "w"), border("w", "n"), wall("n", "s"), wall("e", "w")),
        tile("wild-geyser-1")(
            area("n", "N", 0.28, 0.1, 0.78, 0.13)(small(0.56, 0.19)),
            area("s", "ESW", 0.5, 0.55, 0.22, 0.86)(),
        )(border("n", "s")),
        tile("wild-geyser-2")(
            area("n", "NE", 0.62, 0.36, 0.42, 0.13)(lair),
            area("s", "SW", 0.34, 0.62, 0.6, 0.88)(),
        )(border("n", "s")),
        tile("wild-ruins-1")(
            area("n", "NEW", 0.5, 0.12, 0.76, 0.15)(),
            area("s", "S", 0.3, 0.6, 0.7, 0.62)(lore),
        )(border("n", "s")),
        tile("wild-ruins-2")(
            area("n", "N", 0.25, 0.07, 0.86, 0.08)(lair),
            area("s", "ESW", 0.2, 0.45, 0.82, 0.55)(lore),
        )(rough("n", "s")),
        // The Swamp in the middle; units may only pass through it
        tile("wild-swamp")(
            area("n", "N", 0.5, 0.05, 0.75, 0.05)(),
            area("e", "E", 0.95, 0.5, 0.95, 0.75)(),
            area("s", "S", 0.5, 0.95, 0.25, 0.95)(),
            area("w", "W", 0.05, 0.5, 0.05, 0.25)(),
            area("m", "", 0.65, 0.45, 0.5, 0.78)(lair),
        )(border("m", "n"), border("m", "e"), border("m", "s"), border("m", "w")),
        // The Poisonous Swamp is impassable, and so are the orange lines at its corners
        tile("wild-poison")(
            area("n", "N", 0.42, 0.06, 0.8, 0.07)(),
            area("e", "E", 0.94, 0.38, 0.9, 0.78)(),
            area("s", "S", 0.6, 0.94, 0.2, 0.92)(),
            area("w", "W", 0.06, 0.6, 0.08, 0.2)(),
        )(wall("n", "e"), wall("e", "s"), wall("s", "w"), wall("w", "n"), wall("n", "s"), wall("e", "w")),
        tile("wild-peaks-1")(
            area("n", "NW", 0.12, 0.5, 0.42, 0.43)(small(0.22, 0.28), wood),
            area("s", "ES", 0.55, 0.62, 0.4, 0.86)(large(0.75, 0.77)),
        )(wall("n", "s")),
        tile("wild-peaks-2")(
            area("n", "NW", 0.12, 0.5, 0.42, 0.42)(small(0.23, 0.29), wood),
            area("s", "ES", 0.55, 0.62, 0.35, 0.84)(carved(0.73, 0.77)),
        )(wall("n", "s")),
        tile("wild-graveyard")(
            area("n", "NW", 0.3, 0.42, 0.44, 0.27)(large(0.21, 0.21), lair),
            area("s", "ES", 0.62, 0.6, 0.8, 0.86)(),
        )(rough("n", "s")),
        // The Wyvern's Den: a territory of one tile, always closed
        tile("wild-den")(
            area("n", "N", 0.5, 0.06, 0.76, 0.07)(),
            area("e", "E", 0.93, 0.45, 0.92, 0.66)(),
            area("s", "SW", 0.07, 0.45, 0.4, 0.9)(),
            area("d", "", 0.26, 0.45, 0.66, 0.46)(),
        )(border("d", "n"), border("d", "e"), border("d", "s"), border("n", "e"), border("n", "s"), border("e", "s")),
    )

    // Wastelands expansion (wastelands.scala has their rules). Environment tiles: the Kobold Camp, the Jötnar Camp and
    // Naströnd are impassable in the middle (no area), their territories around it are the ones next to it
    val wastelands : $[TileSpec] = $(
        tile("waste-kobold")(
            area("w", "W", 0.06, 0.42, 0.07, 0.62)(),
            area("s", "S", 0.5, 0.94, 0.3, 0.92)(),
            area("n", "NE", 0.5, 0.06, 0.9, 0.5)(lair),
        )(rough("w", "s"), border("s", "n"), border("n", "w")),
        tile("waste-jotnar")(
            area("w", "W", 0.06, 0.55, 0.07, 0.75)(),
            area("s", "S", 0.45, 0.95, 0.27, 0.93)(),
            area("n", "NE", 0.85, 0.1, 0.92, 0.45)(),
        )(rough("w", "s"), border("s", "n"), border("n", "w")),
        tile("waste-nastrond")(
            area("n", "N", 0.4, 0.05, 0.62, 0.06)(),
            area("e", "E", 0.95, 0.45, 0.93, 0.6)(),
            area("s", "S", 0.6, 0.95, 0.36, 0.93)(),
            area("w", "W", 0.05, 0.6, 0.07, 0.45)(),
        )(border("n", "e"), border("e", "s"), border("s", "w"), border("w", "n"), wall("n", "s"), wall("e", "w")),
        tile("waste-landvidi")(
            area("n", "N", 0.55, 0.07, 0.38, 0.1)(),
            area("s", "ESW", 0.2, 0.75, 0.6, 0.8)(),
        )(border("n", "s")),
        tile("waste-thor")(
            area("n", "N", 0.25, 0.06, 0.82, 0.12)(lair),
            area("s", "ESW", 0.15, 0.85, 0.8, 0.85)(),
        )(rough("n", "s")),
        tile("waste-urdarbrunn")(
            area("n", "NW", 0.15, 0.15, 0.5, 0.07)(),
            area("s", "ES", 0.85, 0.4, 0.55, 0.88)(),
        )(rough("n", "s")),
        tile("waste-vedrfolnir")(
            area("n", "NEW", 0.5, 0.05, 0.82, 0.06)(),
            area("s", "S", 0.45, 0.88, 0.72, 0.52)(),
        )(rough("n", "s")),
    )

    // Wastelands central tiles, one of which replaces the starting tile: the middle territory (c, or d for the
    // Wyvern's Den) is enclosed by four territories at the sides; the Relic of the Gods, the Great Lake and the
    // Volcano are impassable in the middle. The five-player tiles go east of it, with regular or impassable borders
    val central : $[TileSpec] = $(
        tile("start-magma")(
            area("c", "", 0.35, 0.62, 0.6, 0.7)(),
            area("n", "N", 0.5, 0.05, 0.7, 0.07)(),
            area("e", "E", 0.95, 0.5, 0.93, 0.65)(),
            area("s", "S", 0.5, 0.95, 0.3, 0.93)(),
            area("w", "W", 0.05, 0.5, 0.07, 0.35)(),
        )(border("c", "n"), border("c", "e"), border("c", "s"), border("c", "w"), border("n", "e"), border("e", "s"), border("s", "w"), border("w", "n")),
        tile("start-yggdrasil")(
            area("c", "", 0.7, 0.62, 0.4, 0.66)(),
            area("n", "N", 0.5, 0.05, 0.7, 0.07)(),
            area("e", "E", 0.95, 0.5, 0.93, 0.65)(),
            area("s", "S", 0.5, 0.95, 0.3, 0.93)(),
            area("w", "W", 0.05, 0.5, 0.07, 0.35)(),
        )(border("c", "n"), border("c", "e"), border("c", "s"), border("c", "w"), border("n", "e"), border("e", "s"), border("s", "w"), border("w", "n")),
        tile("start-relic")(
            area("n", "N", 0.4, 0.04, 0.64, 0.06)(),
            area("e", "E", 0.96, 0.42, 0.94, 0.62)(),
            area("s", "S", 0.6, 0.96, 0.36, 0.94)(),
            area("w", "W", 0.04, 0.6, 0.06, 0.38)(),
        )(border("n", "e"), border("e", "s"), border("s", "w"), border("w", "n"), wall("n", "s"), wall("e", "w")),
        tile("start-lake")(
            area("n", "N", 0.45, 0.05, 0.7, 0.06)(),
            area("e", "E", 0.95, 0.5, 0.95, 0.3)(),
            area("s", "S", 0.55, 0.95, 0.3, 0.95)(),
            area("w", "W", 0.04, 0.5, 0.05, 0.7)(),
        )(border("n", "e"), border("e", "s"), border("s", "w"), border("w", "n"), wall("n", "s"), wall("e", "w")),
        tile("start-volcano")(
            area("n", "N", 0.5, 0.05, 0.3, 0.06)(),
            area("e", "E", 0.95, 0.5, 0.94, 0.3)(),
            area("s", "S", 0.5, 0.95, 0.7, 0.94)(),
            area("w", "W", 0.05, 0.5, 0.06, 0.7)(),
        )(border("n", "e"), border("e", "s"), border("s", "w"), border("w", "n"), wall("n", "s"), wall("e", "w")),
        tile("start-mimir")(
            area("c", "", 0.7, 0.45, 0.5, 0.7)(),
            area("n", "N", 0.5, 0.05, 0.7, 0.07)(),
            area("e", "E", 0.95, 0.5, 0.93, 0.65)(),
            area("s", "S", 0.5, 0.95, 0.3, 0.93)(),
            area("w", "W", 0.05, 0.5, 0.07, 0.35)(),
        )(border("c", "n"), border("c", "e"), border("c", "s"), border("c", "w"), border("n", "e"), border("e", "s"), border("s", "w"), border("w", "n")),
        tile("start-hrimgandr")(
            area("c", "", 0.75, 0.5, 0.5, 0.72)(),
            area("n", "N", 0.5, 0.05, 0.7, 0.07)(),
            area("e", "E", 0.95, 0.5, 0.93, 0.65)(),
            area("s", "S", 0.5, 0.95, 0.3, 0.93)(),
            area("w", "W", 0.05, 0.5, 0.07, 0.35)(),
        )(border("c", "n"), border("c", "e"), border("c", "s"), border("c", "w"), border("n", "e"), border("e", "s"), border("s", "w"), border("w", "n")),
        tile("start-den")(
            area("d", "", 0.25, 0.45, 0.72, 0.62)(),
            area("n", "N", 0.5, 0.05, 0.7, 0.07)(),
            area("e", "E", 0.95, 0.5, 0.93, 0.65)(),
            area("s", "S", 0.5, 0.95, 0.3, 0.93)(),
            area("w", "W", 0.05, 0.5, 0.07, 0.35)(),
        )(border("d", "n"), border("d", "e"), border("d", "s"), border("d", "w"), border("n", "e"), border("e", "s"), border("s", "w"), border("w", "n")),
        tile("start-helheim")(
            area("c", "", 0.22, 0.5, 0.5, 0.68)(),
            area("n", "N", 0.5, 0.05, 0.7, 0.07)(),
            area("e", "E", 0.95, 0.5, 0.93, 0.65)(),
            area("s", "S", 0.5, 0.95, 0.3, 0.93)(),
            area("w", "W", 0.05, 0.5, 0.07, 0.35)(),
        )(border("c", "n"), border("c", "e"), border("c", "s"), border("c", "w"), border("n", "e"), border("e", "s"), border("s", "w"), border("w", "n")),
        tile("start-5-open")(
            area("n", "N", 0.55, 0.12, 0.35, 0.16)(),
            area("m", "WE", 0.5, 0.55, 0.75, 0.55)(),
            area("s", "S", 0.55, 0.9, 0.3, 0.9)(),
        )(border("n", "m"), border("m", "s")),
        tile("start-5-wall")(
            area("n", "N", 0.55, 0.12, 0.35, 0.16)(),
            area("m", "WE", 0.5, 0.55, 0.75, 0.55)(),
            area("s", "S", 0.55, 0.9, 0.3, 0.9)(),
        )(wall("n", "m"), wall("m", "s")),
    )

    // Uncharted Horizons' Sea module (sea.scala): a Beach tile is the Port's land tile with the sea south of it and
    // a shore wing on each side of the sea, all turned so the Port faces the map. The sea's sides, the wings' southern
    // sides and the Port's southern side own no area: they join nothing and no tile goes there. The Port territory is
    // the Port's land above the shore's dashes. Each wing's land is two ordinary areas, split by the dashes running to
    // its outer top corner: the strip along its top (joining the tile placed beside the Port) and the strip along its
    // outer side; the sand and sea beyond the shore's dashes belong to no area
    val beach : $[TileSpec] = $(
        tile("beach-port")(
            area("p", "NEW", 0.25, 0.07, 0.72, 0.08)(small(0.21, 0.32)),
        )(),
        tile("beach-wing-w")(
            area("n", "N", 0.82, 0.07, 0.68, 0.06)(),
            area("w", "W", 0.3, 0.58, 0.33, 0.4)(),
        )(border("n", "w")),
        tile("beach-wing-e")(
            area("n", "N", 0.2, 0.05, 0.5, 0.18)(),
            area("e", "E", 0.77, 0.62, 0.95, 0.88)(),
        )(border("n", "e")),
        tile("beach-sea")()(),
    )

    // Uncharted Horizons' five map tiles (TTS mod 3597126237), shuffled into the map tiles with the HorizonsTiles option.
    // The Bridge: two cliffs (n, s) behind impassable lines, linked over the valley (m) by the bridge, so n and s are adjacent
    val horizons : $[TileSpec] = $(
        tile("horizon-1")(
            area("n", "NW", 0.45, 0.1, 0.1, 0.5)(small(0.21, 0.2)),
            area("s", "ES", 0.9, 0.45, 0.47, 0.88)(small(0.72, 0.77)),
        )(border("n", "s")),
        tile("horizon-2")(
            area("n", "NEW", 0.5, 0.06, 0.9, 0.13)(lair, wood, large(0.66, 0.29)),
            area("s", "S", 0.2, 0.8, 0.66, 0.7)(lore),
        )(rough("n", "s")),
        tile("horizon-3")(
            area("n", "NW", 0.3, 0.04, 0.05, 0.45)(),
            area("s", "ES", 0.88, 0.45, 0.4, 0.25)(food, lair),
        )(rough("n", "s")),
        tile("horizon-4")(
            area("n", "NEW", 0.5, 0.06, 0.3, 0.36)(small(0.18, 0.18), small(0.83, 0.15), small(0.62, 0.42)),
            area("s", "S", 0.2, 0.84, 0.62, 0.88)(lore),
        )(border("n", "s")),
        tile("horizon-bridge")(
            area("n", "N", 0.64, 0.22, 0.25, 0.17)(food),
            area("m", "EW", 0.25, 0.5, 0.64, 0.5)(wood, small(0.85, 0.48)),
            area("s", "S", 0.3, 0.9, 0.68, 0.88)(),
        )(wall("n", "m"), wall("m", "s"), border("n", "s")),
    )

    val all : $[TileSpec] = $(start, start5) ++ regular ++ environment ++ wastelands ++ central ++ beach ++ horizons

    val byId : Map[String, TileSpec] = all./(t => t.id -> t).toMap

    def apply(id : String) = byId(id)
}
