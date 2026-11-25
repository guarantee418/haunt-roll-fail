package tactics
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
import tactics.elem._

// THE MAP: Moro River, the two panels MR1 and MR2 of the 2nd Edition core set side by side, from the Tabletop Simulator mod
// "Company of Heroes - 2nd Edition" (map.webp is the two panels at a fifth of their size, 2400x1800). Pointy-top hexes in 13 rows:
// the first and the last are half rows cut by the map's edges, and are the Spawn Areas. The rows with an even number (counting from 0)
// are shifted half a hex right, and have 14 hexes, cut in half at both ends; the others have 13.
// The lines along the hex edges were read off the art by tactics/tools/map-data.py and checked by eye (see HANDOFF.md):
// yellow Light Cover and green Heavy Cover inside a hex give its Infantry cover against attacks crossing that edge,
// red lines are impassable, water hexes ringed in red can't be entered. The Control Points are where the Mission Booklet puts them.

sealed abstract class Terrain(val name : String) extends Record
case object Open extends Terrain("Open ground")
case object Water extends Terrain("Water")
case object Building extends Terrain("Building")
case object Ruin extends Terrain("Destroyed Building")
// Ardennes Forest: the Cathedral, an indestructible multi-hex Special Building, and the frozen lake's Ice tiles (Full Game Rules 10.2, 10.4)
case object Cathedral extends Terrain("Cathedral")
case object Ice extends Terrain("Ice")

sealed abstract class Cover(val name : String, val dice : Int) extends Record
case object LightCover extends Cover("Light Cover", 1)
case object HeavyCover extends Cover("Heavy Cover", 2)

// a hex by column (0 at the left) and row (0 at the top)
case class Hex(c : Int, r : Int) extends Record with Elementary {
    def name = ('A' + c).toChar.toString + (r + 1)
    def elem = name.styled(styles.hex)
    // axial coordinates: the even rows are shifted right
    def q = c - (r + 1) / 2
}

// a map: its rows (see MoroRiver), its picture, the starting stockpile (Manpower, Munitions, Fuel), and whether adjacent Buildings
// are multi-hex Buildings (Full Game Rules 10.2: units see and fire inside one, and get no Cover from units in the same Building)
// (and how many panels wide it is, 1200 px a panel; the starting Infantry each player places, else 3, 2 in team games; how many
// players it takes, if not any; how far from the edge, in hexes, its side Spawn Areas reach)
class MapData(val id : String, val name : String, val image : String, val stock : (Int, Int, Int), val multiHex : Boolean, val data : $[String], val x0 : Double = 185.2, val sides : Boolean = false, val areas : $[$[Hex]] = $(), val trigger : |[Int] = None, val panels : Int = 2, val infantry : |[Int] = None, val players : |[Int] = None, val edge : Double = 0.75) {
    val width = 1200 * panels
    val height = 1800

    val dx = 184.6
    val y0 = 148.2
    val dy = 150.3
    val side = 100.2

    def x(h : Hex) : Double = x0 + (h.c - 1) * dx + (h.r % 2 == 0).?(dx / 2).|(0)
    def y(h : Hex) : Double = y0 + (h.r - 1) * dy

    // "row column terrain edges extras": terrain W water, B building, D destroyed building; the six edges from the north-east
    // clockwise, H heavy cover, L light cover, X impassable; extras T a tank trap (Full Game Rules), then the Control Point's
    // resources, V Victory Points, M Manpower, U Munitions, F Fuel

    val parsed = data./(_.split(" ").$)./(l => (Hex(l(1).toInt, l(0).toInt), l(2), l(3), l(4)))

    val hexes : $[Hex] = parsed./(_._1).sortBy(h => (h.r, h.c))

    val terrain : Map[Hex, Terrain] = parsed./{ case (h, t, _, _) => h -> (t match {
        case "W" => Water
        case "B" => Building
        case "D" => Ruin
        case "C" => Cathedral
        case "I" => Ice
        case _ => Open
    }) }.toMap

    // edges: 0 north-east, 1 east, 2 south-east, 3 south-west, 4 west, 5 north-west
    val directions = $((1, -1), (1, 0), (0, 1), (-1, 1), (-1, 0), (0, -1))

    val byAxial = hexes./(h => (h.q, h.r) -> h).toMap

    def at(q : Int, r : Int) : |[Hex] = byAxial.get((q, r))

    def toward(h : Hex, e : Int) : |[Hex] = at(h.q + directions(e)._1, h.r + directions(e)._2)

    val cover : Map[(Hex, Int), Cover] = parsed./~{ case (h, _, e, _) => e.zipWithIndex./~{
        case ('H', i) => $((h, i) -> HeavyCover)
        case ('L', i) => $((h, i) -> LightCover)
        case _ => $()
    } }.toMap

    // an impassable line on either side of an edge closes it both ways, and so does a water hex
    val closed : Set[(Hex, Hex)] = parsed./~{ case (h, _, e, _) => e.zipWithIndex./~{
        case ('X', i) => toward(h, i)./~(n => $((h, n), (n, h)))
        case _ => $()
    } }.toSet

    val traps : $[Hex] = parsed.%(_._4.contains('T'))./(_._1)

    val points : Map[Hex, $[Res]] = parsed./~{ case (h, _, _, x) =>
        val l = x.toList./~{
            case 'V' => $(VP)
            case 'M' => $(Manpower)
            case 'U' => $(Munitions)
            case 'F' => $(Fuel)
            case _ => $()
        }
        l.any.$(h -> l)
    }.toMap

    val neighbors : Map[Hex, $[Hex]] = hexes./(h => h -> 0.until(6).$./~(e => toward(h, e))).toMap

    def adjacent(a : Hex, b : Hex) = neighbors(a).has(b)

    // adjacent with no impassable line between them
    def joined(a : Hex, b : Hex) = adjacent(a, b) && closed.contains((a, b)).not

    def distance(a : Hex, b : Hex) = {
        val dq = a.q - b.q
        val dr = a.r - b.r
        (dq.abs + dr.abs + (dq + dr).abs) / 2
    }

    def within(a : Hex, n : Int) = hexes.%(b => distance(a, b) <= n)

    // the Spawn Areas: the back rows, or with `sides` the hexes along the left (Team 1) and right (Team 2) edges, as the Mission Booklet's
    // Across the Don and Road to Moscow put them (its numbered badges mark the full hexes of that column; the half hexes there are taken too)
    // a free-for-all mission's numbered Spawn Areas, one a player; else the back rows (or the left and right edges)
    def spawn(team : Int) : $[Hex] = if (areas.any) areas(team) else sides.?(hexes.%(h => (team == 0).?(x(h) < dx * edge).|(x(h) > width - dx * edge))).|(hexes.%(h => h.r == (team == 0).?(0).|(12))).%(h => terrain(h) != Water)

    // LINE OF SIGHT: Buildings and Destroyed Buildings between two hexes block it; a line running along an edge isn't blocked,
    // so it is drawn twice, nudged to either side, and blocked only if both are
    def cube(h : Hex) = (h.q.toDouble, h.r.toDouble, (-h.q - h.r).toDouble)

    def round(x : Double, y : Double, z : Double) : |[Hex] = {
        var rx = math.round(x).toDouble
        var ry = math.round(y).toDouble
        var rz = math.round(z).toDouble
        val ex = (rx - x).abs
        val ey = (ry - y).abs
        val ez = (rz - z).abs
        if (ex > ey && ex > ez)
            rx = -ry - rz
        else
        if (ey > ez)
            ry = -rx - rz
        at(rx.toInt, ry.toInt)
    }

    def between(a : Hex, b : Hex, nudge : Double) : $[Hex] = {
        val n = distance(a, b)
        val (ax, ay, az) = cube(a)
        val (bx, by, bz) = cube(b)
        1.until(n).$./~{ i =>
            val t = i.toDouble / n
            round(ax + (bx - ax) * t + nudge, ay + (by - ay) * t + nudge, az + (bz - az) * t - 2 * nudge)
        }
    }

    def blocks(h : Hex) = terrain(h) == Building || terrain(h) == Ruin || terrain(h) == Cathedral

    // the multi-hex Buildings: each group of adjacent Buildings (and the Cathedral), on maps that have them
    val buildingGroup : Map[Hex, Int] = {
        var g = Map[Hex, Int]()
        if (multiHex)
            hexes.%(h => terrain(h) == Building || terrain(h) == Cathedral).foreach { h =>
                if (g.contains(h).not) {
                    val id = g.size
                    var front = $(h)
                    while (front.any) {
                        front.foreach(x => g += x -> id)
                        front = front./~(x => neighbors(x).%(n => (terrain(n) == Building || terrain(n) == Cathedral) && g.contains(n).not)).distinct
                    }
                }
            }
        g
    }

    def sameBuilding(a : Hex, b : Hex) = buildingGroup.get(a).any && buildingGroup.get(a) == buildingGroup.get(b)

    def lineBlocked(a : Hex, b : Hex, block : Hex => Boolean) : Boolean =
        between(a, b, 1e-6).exists(block) && between(a, b, -1e-6).exists(block)

    // (a hex of a multi-hex Building at either end doesn't block the line: it crosses only one of its outer lines)
    def clear(a : Hex, b : Hex) = lineBlocked(a, b, h => blocks(h) && sameBuilding(h, a).not && sameBuilding(h, b).not).not

    // the edges of the target hex a line from the attacker's hex crosses (two when it runs through a corner)
    def crossed(target : Hex, from : Hex) : $[Int] = {
        val px = math.sqrt(3) * (from.q - target.q + (from.r - target.r) / 2.0)
        val py = 1.5 * (from.r - target.r)
        val angle = math.toDegrees(math.atan2(py, px))
        val normals = $(-60.0, 0.0, 60.0, 120.0, 180.0, -120.0)
        0.until(6).$.%{ e =>
            val d = ((angle - normals(e)) % 360 + 540) % 360 - 180
            d.abs <= 30.0001
        }
    }

    // the direction from one hex to another, in degrees (0 east, clockwise, as the edges' normals: north-east is -60)
    def angle(from : Hex, to : Hex) : Double = {
        val px = math.sqrt(3) * (to.q - from.q + (to.r - from.r) / 2.0)
        val py = 1.5 * (to.r - from.r)
        math.toDegrees(math.atan2(py, px))
    }

    val normals = $(-60.0, 0.0, 60.0, 120.0, 180.0, -120.0)

    // a Firing Arc (Full Game Rules): the hexes within 60 degrees either side of the side a unit faces, those on the edge included
    // (the rulebook's Heavy Vehicle picture draws the front arc's edge through the middle of the two neighboring sides)
    def inCone(from : Hex, to : Hex, e : Int) : Boolean = {
        val d = ((angle(from, to) - normals(e)) % 360 + 540) % 360 - 180
        d.abs <= 60.0001
    }

    def coverAgainst(target : Hex, from : Hex) : |[Cover] = (from != target).??(crossed(target, from)./~(e => cover.get((target, e)))).maxByOption(_.dice)
}

// the games made before the map choice: the Moro River rows as first read, with the Control Point beside the bridge one hex off
object MoroRiverFirst extends MapData("moro", "Moro River", "map", (4, 2, 3), false, MoroRiverRows.rows)

object MoroRiverRows {
    val rows = $(
        "0 0 . ...... -",
        "0 1 . ...... -",
        "0 2 . ...... -",
        "0 3 W .XXXX. -",
        "0 4 . ...... -",
        "0 5 . ...... -",
        "0 6 . ...... -",
        "0 7 . ...... -",
        "0 8 . ...... -",
        "0 9 . ...H.. -",
        "0 10 . ...... -",
        "0 11 . ...... -",
        "0 12 . ...... -",
        "1 0 . ...... -",
        "1 1 . ...... -",
        "1 2 . HHHHH. -",
        "1 3 W XXXXXX -",
        "1 4 . ...... -",
        "1 5 . ...... -",
        "1 6 . ...HH. -",
        "1 7 . ...... -",
        "1 8 . ...... -",
        "1 9 . ...... -",
        "1 10 . ...HH. -",
        "1 11 . ...... -",
        "1 12 . ...... -",
        "1 13 . ...... -",
        "2 0 . ...... -",
        "2 1 . LL...L -",
        "2 2 . .....X -",
        "2 3 . ...... -",
        "2 4 . HX...H -",
        "2 5 . ...HH. T",
        "2 6 . ...... -",
        "2 7 . ...... -",
        "2 8 . ...... -",
        "2 9 . ...... -",
        "2 10 . HH.... -",
        "2 11 . ...... -",
        "2 12 . ...... -",
        "3 0 . ...... -",
        "3 1 . ...... -",
        "3 2 . ...... VM",
        "3 3 . ...... -",
        "3 4 . ....LL -",
        "3 5 . XX.LL. -",
        "3 6 . ....H. -",
        "3 7 . ...... -",
        "3 8 . ...HHH FU",
        "3 9 . ...... -",
        "3 10 . ...LL. -",
        "3 11 . HHHH.. -",
        "3 12 . ...... -",
        "3 13 . ...... -",
        "4 0 . LLLL.. -",
        "4 1 . ...... -",
        "4 2 . ...... -",
        "4 3 . XX.... -",
        "4 4 . ..HXHH MF",
        "4 5 . ...HH. -",
        "4 6 . ...... -",
        "4 7 . ...... -",
        "4 8 . ..HHH. -",
        "4 9 . ...... -",
        "4 10 . ...... -",
        "4 11 . ...... UM",
        "4 12 . ...... -",
        "5 0 . ...... -",
        "5 1 . ...... VF",
        "5 2 . ...... -",
        "5 3 . ...... -",
        "5 4 B ...... -",
        "5 5 W XXXXXX -",
        "5 6 . ...... T",
        "5 7 D ...... -",
        "5 8 . ...... -",
        "5 9 . .X.... -",
        "5 10 . ..HHH. T",
        "5 11 . ...... -",
        "5 12 . .LL... -",
        "5 13 . ...LL. -",
        "6 0 . ...... -",
        "6 1 B ...... -",
        "6 2 . ..X... -",
        "6 3 . ...X.. -",
        "6 4 B ...... -",
        "6 5 . ...... VU",
        "6 6 W XXXXXX -",
        "6 7 . ...... VU",
        "6 8 B ...... -",
        "6 9 . X..... -",
        "6 10 . .....X -",
        "6 11 B ...... -",
        "6 12 . L....L -",
        "7 0 . .LL... -",
        "7 1 . ...LL. -",
        "7 2 . ...... -",
        "7 3 . HH...H T",
        "7 4 . ....X. -",
        "7 5 . ...... -",
        "7 6 D ...... -",
        "7 7 . ...... T",
        "7 8 W XXXXXX -",
        "7 9 B ...... -",
        "7 10 . ...... -",
        "7 11 . ...... -",
        "7 12 . ...... VF",
        "7 13 . ...... -",
        "8 0 . L....L -",
        "8 1 . ...... UM",
        "8 2 . ...... -",
        "8 3 . ...... -",
        "8 4 . HH...H FU",
        "8 5 . ...... -",
        "8 6 . ...... -",
        "8 7 . HH.... -",
        "8 8 . XHH..H MF",
        "8 9 . ...XX. -",
        "8 10 . ...... -",
        "8 11 . ...... -",
        "8 12 . L..LLL -",
        "9 0 . ...... -",
        "9 1 . HH..HH -",
        "9 2 . ...... -",
        "9 3 . ...... -",
        "9 4 . ...... -",
        "9 5 . ...... -",
        "9 6 . HH...H -",
        "9 7 . .H.... -",
        "9 8 . ...XX. -",
        "9 9 . ...... -",
        "9 10 . ...... -",
        "9 11 . ...LL. VM",
        "9 12 . ...... -",
        "9 13 . ...... -",
        "10 0 . ...... -",
        "10 1 . ...... -",
        "10 2 . ...... -",
        "10 3 . ...... -",
        "10 4 . ...... -",
        "10 5 . LL.... -",
        "10 6 . L....L -",
        "10 7 . HH.... T",
        "10 8 . LL..X. -",
        "10 9 . ...... -",
        "10 10 . ...... -",
        "10 11 . ...... -",
        "10 12 . ...... -",
        "11 0 . ...... -",
        "11 1 . ...... -",
        "11 2 . ...... -",
        "11 3 . ...... -",
        "11 4 . LL.... -",
        "11 5 . HH...H -",
        "11 6 . ...... -",
        "11 7 . LL.... -",
        "11 8 . ...... -",
        "11 9 . ...... -",
        "11 10 . ...... -",
        "11 11 W XXXXXX -",
        "11 12 . ...X.. -",
        "11 13 . ...... -",
        "12 0 . ...... -",
        "12 1 . L..... -",
        "12 2 . ...... -",
        "12 3 . L..... -",
        "12 4 . ...... -",
        "12 5 . .L.... -",
        "12 6 . ...... -",
        "12 7 . ...... -",
        "12 8 . ...... -",
        "12 9 . ...... -",
        "12 10 . L....L -",
        "12 11 . ...... -",
        "12 12 W XX..XX -"
    )
}

// Ardennes Forest: the back of the Moro River panels, AF2 on the left and AF1 on the right as the Mission Booklet lays them; read by
// tools/map-edges.py and tools/ardennes-data.py (the cover lines are all Light Cover; no impassable lines or water)
object Maps {
    val all = $[MapData](MoroRiverFirst, MoroRiver, AcrossTheDon, BetweenTwoRivers, ArdennesForest, FrozenLake, RoadToMoscow)
}

// the map of the game being played (set by Game; one game a page, and the JVM checks play one at a time)
object Board {
    var current : MapData = MoroRiverFirst

    def width = current.width
    def height = current.height
    def x0 = current.x0
    def dx = current.dx
    def y0 = current.y0
    def dy = current.dy
    def side = current.side
    def x(h : Hex) = current.x(h)
    def y(h : Hex) = current.y(h)
    def hexes = current.hexes
    def terrain = current.terrain
    def directions = current.directions
    def at(q : Int, r : Int) = current.at(q, r)
    def toward(h : Hex, e : Int) = current.toward(h, e)
    def cover = current.cover
    def closed = current.closed
    def traps = current.traps
    def points = current.points
    def neighbors = current.neighbors
    def adjacent(a : Hex, b : Hex) = current.adjacent(a, b)
    def joined(a : Hex, b : Hex) = current.joined(a, b)
    def distance(a : Hex, b : Hex) = current.distance(a, b)
    def within(a : Hex, n : Int) = current.within(a, n)
    def spawn(team : Int) = current.spawn(team)
    def between(a : Hex, b : Hex, nudge : Double) = current.between(a, b, nudge)
    def blocks(h : Hex) = current.blocks(h)
    def sameBuilding(a : Hex, b : Hex) = current.sameBuilding(a, b)
    def lineBlocked(a : Hex, b : Hex, block : Hex => Boolean) = current.lineBlocked(a, b, block)
    def clear(a : Hex, b : Hex) = current.clear(a, b)
    def crossed(target : Hex, from : Hex) = current.crossed(target, from)
    def coverAgainst(target : Hex, from : Hex) = current.coverAgainst(target, from)
    def angle(from : Hex, to : Hex) = current.angle(from, to)
    def normals = current.normals
    def inCone(from : Hex, to : Hex, e : Int) = current.inCone(from, to, e)
}
