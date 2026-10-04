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

import hrf.elem._

import nort.elem._


// A tile on the map: its position and how many quarter turns clockwise it is turned
case class Placement(tile : String, x : Int, y : Int, r : Int) extends Record {
    def spec = Tiles(tile)
}

// One area of a placed tile; a territory is one or more areas joined across tile sides
case class AreaRef(x : Int, y : Int, id : String) extends GameElementary with Record {
    def elem(implicit game : Game) = ("Territory " + game.board.label(this)).hl
}

// A building space: area and index into its spaces; from SpaceRef.extra on, buildings that take no space (Amenities)
case class SpaceRef(area : AreaRef, index : Int) extends Record

object SpaceRef {
    val extra = 100
}


trait Building extends NamedToString with Elementary with Record {
    def large : Boolean
    def image = "building-" + name.replaceAll("([a-z])([A-Z])", "$1-$2").toLowerCase
    def cost = large.?(3).|(1)
    def title = name.replaceAll("([a-z])([A-Z])", "$1 $2")
    override def elem : Elem = title.hl
}

case object FoodSilo extends Building { val large = false }
case object WoodcutterLodge extends Building { val large = false }
case object DefenseTower extends Building { val large = false }
case object TrainingCamp extends Building { val large = false }
case object CarvedStone extends Building { val large = false }
case object Fortress extends Building { val large = true }
case object Forge extends Building { val large = true }
case object AltarOfKings extends Building { val large = true }

object Building {
    val all : $[Building] = $(FoodSilo, WoodcutterLodge, DefenseTower, TrainingCamp, CarvedStone, Fortress, Forge, AltarOfKings)

    // Seven tokens of each type
    val tokens = 7
}


// A territory: areas joined across tile sides; its first area names it
case class Territory(areas : $[AreaRef]) {
    def anchor = areas.head
}


class Board {
    var placements : $[Placement] = $

    private var territoryCache : |[$[Territory]] = None
    private var indexCache : Map[AreaRef, Territory] = Map()

    def at(x : Int, y : Int) = placements.find(p => p.x == x && p.y == y)

    def place(p : Placement) {
        placements :+= p
        territoryCache = None
    }

    def spec(a : AreaRef) : AreaSpec = at(a.x, a.y).get.spec.area(a.id)

    // The area of the tile at x, y that owns side s, if a tile is there
    def areaAt(x : Int, y : Int, s : Side) : |[AreaRef] = at(x, y)./(p => AreaRef(x, y, p.spec.areaAt(s, p.r).id))

    def sides(a : AreaRef) : $[Side] = {
        val p = at(a.x, a.y).get
        spec(a).edges./(_.rotate(p.r))
    }

    // Join areas across shared tile sides
    private def compute(placements : $[Placement]) : $[Territory] = {
        val all = placements./~(p => p.spec.areas./(a => AreaRef(p.x, p.y, a.id)))
        val parent = scala.collection.mutable.Map[AreaRef, AreaRef]()
        all.foreach(a => parent(a) = a)
        def find(a : AreaRef) : AreaRef = if (parent(a) == a) a else { val r = find(parent(a)); parent(a) = r; r }
        val byXY = placements./(p => (p.x, p.y) -> p).toMap

        placements.foreach { p =>
            Side.all.foreach { s =>
                byXY.get((p.x + s.dx, p.y + s.dy)).foreach { q =>
                    val a = AreaRef(p.x, p.y, p.spec.areaAt(s, p.r).id)
                    val b = AreaRef(q.x, q.y, q.spec.areaAt(s.opposite, q.r).id)
                    parent(find(a)) = find(b)
                }
            }
        }

        val order = all.zipWithIndex.toMap
        all.groupBy(find).values.$./(l => Territory(l.sortBy(order)))./(t => t).sortBy(t => order(t.anchor))
    }

    def territories : $[Territory] = {
        if (territoryCache.none) {
            val t = compute(placements)
            territoryCache = |(t)
            indexCache = t./~(t => t.areas./(_ -> t)).toMap
        }

        territoryCache.get
    }

    def territory(a : AreaRef) : Territory = { territories; indexCache(a) }

    // Number shown on the map and in the action list
    def label(a : AreaRef) : Int = territories.indexWhere(_.areas.contains(a)) + 1

    def tiles(t : Territory) : Int = t.areas./(a => (a.x, a.y)).distinct.num

    def closed(t : Territory) : Boolean = t.areas.forall(a => sides(a).forall(s => at(a.x + s.dx, a.y + s.dy).any))

    def open(t : Territory) = closed(t).not

    // Adjacent territories, true when a regular border connects them; impassable borders don't connect
    def adjacent(t : Territory) : $[(Territory, Boolean)] = {
        val links = t.areas./~{ a =>
            val p = at(a.x, a.y).get
            p.spec.borders.%(_.impassable.not)./~{ b =>
                if (b.a == a.id) $(AreaRef(a.x, a.y, b.b) -> b.rough)
                else if (b.b == a.id) $(AreaRef(a.x, a.y, b.a) -> b.rough)
                else $
            }
        }./{ case (o, rough) => territory(o) -> rough }.filter(_._1 != t)

        links.groupBy(_._1).toList./{ case (o, l) => o -> l.exists(_._2.not) }.sortBy(x => territories.indexOf(x._1))
    }

    // A placement is legal when every border of every tile still separates two territories;
    // otherwise some border would stop in the middle of a territory
    def consistent(placements : $[Placement]) : Boolean = consistent(placements, compute(placements))

    private def consistent(placements : $[Placement], t : $[Territory]) : Boolean = {
        val index = t./~(t => t.areas./(_ -> t)).toMap
        placements.forall(p => p.spec.borders.forall(b => index(AreaRef(p.x, p.y, b.a)) != index(AreaRef(p.x, p.y, b.b))))
    }

    // The territories with p added, when that placement is consistent; computes them once for both
    def tryPlace(p : Placement) : |[$[Territory]] = {
        val all = placements :+ p
        val t = compute(all)
        consistent(all, t).?(t)
    }

    // Territories the areas of a new placement would join, with the result
    def preview(p : Placement) : $[Territory] = compute(placements :+ p)

    def empty(x : Int, y : Int) = at(x, y).none

    // Empty spots touching a placed tile
    def frontier : $[(Int, Int)] = placements./~(p => Side.all./(s => (p.x + s.dx, p.y + s.dy))).distinct.%{ case (x, y) => empty(x, y) }.sortBy { case (x, y) => (y, x) }

    // Number shown on the map for an empty spot
    def spotLabel(x : Int, y : Int) : Int = frontier.indexOf((x, y)) + 1

    def bounds : (Int, Int, Int, Int) = {
        val xs = placements./(_.x)
        val ys = placements./(_.y)
        (xs.min, ys.min, xs.max, ys.max)
    }

    // Area position on the map in tile units
    def point(a : AreaRef) : (Double, Double) = {
        val p = at(a.x, a.y).get
        val s = spec(a)
        val (x, y) = rotate(s.x, s.y, p.r)
        (a.x + x, a.y + y)
    }

    // Where the units of an area's territory are drawn
    def unitPoint(a : AreaRef) : (Double, Double) = {
        val p = at(a.x, a.y).get
        val s = spec(a)
        val (x, y) = rotate(s.ux, s.uy, p.r)
        (a.x + x, a.y + y)
    }

    def point(s : SpaceRef) : (Double, Double) = {
        if (s.index >= SpaceRef.extra) {
            // Next to the area's number
            val (x, y) = point(s.area)
            val k = s.index - SpaceRef.extra
            return (x - 0.2 - 0.2 * (k % 2), y + 0.2 * (k / 2))
        }

        val p = at(s.area.x, s.area.y).get
        val sp = spec(s.area).spaces(s.index)
        val (x, y) = rotate(sp.x, sp.y, p.r)
        (s.area.x + x, s.area.y + y)
    }

    def rotate(x : Double, y : Double, r : Int) : (Double, Double) = (r % 4) match {
        case 0 => (x, y)
        case 1 => (1 - y, x)
        case 2 => (1 - x, 1 - y)
        case 3 => (y, 1 - x)
    }
}
