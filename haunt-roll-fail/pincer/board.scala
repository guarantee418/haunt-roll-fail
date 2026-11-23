package pincer
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
import pincer.elem._

// THE BOARD: the Tabletop Simulator mod's board image (map.webp, 1376x1376), a grid of pointy-top hexes in rows,
// 11 hexes in the even rows and 12 (two of them cut by the frame) in the odd ones, which are shifted half a hex left.
// The terrain of each hex was read from the image (pincer/tools/, see HANDOFF.md) and checked by eye.
// The board is double-sided; the mod has one side, so only that one is here.

sealed abstract class Terrain(val name : String, val resource : Resource, val letter : Char) extends Record {
    def land = this != Water
}

case object Field extends Terrain("Wheat field", Wheat, 'W')
case object Water extends Terrain("Water", Fish, 'S')
case object Hills extends Terrain("Hills", Mustang, 'H')
case object Forest extends Terrain("Forest", Lumber, 'F')
case object Rocks extends Terrain("Stone", Stone, 'R')
case object Mountain extends Terrain("Mountain", Iron, 'M')

object Terrain {
    val all = $(Field, Water, Hills, Forest, Rocks, Mountain)
}

// a hex by row (0 at the top) and column (0 at the left; in the odd rows column 0 is the cut hex at the left edge)
case class Hex(c : Int, r : Int) extends Record with Elementary {
    def name = ('A' + c).toChar.toString + (r + 1)
    def elem = name.styled(styles.hex)
    def q = c - (r + (r & 1)) / 2
}

object Board {
    val x0 = 85.5
    val y0 = 69.0
    val w = 119.5
    val h = 104.0

    val width = 1376
    val height = 1376

    val rows = $(
        "WFHFFWSRHMW",
        "RFHFFRSFHFWR",
        "HFWSSHSSFSS",
        "WFMHSSSSSWSS",
        "HFFHSSSSSSS",
        "MWRSSSSSFFSH",
        "FHHSFSSMHSW",
        "HFFWSWHSHRFH",
        "FHSSMFFSWFF",
        "SSSFHHRSSHHM",
        "SWFFFWHSFRW",
        "SHHRHHFHHFHF",
        "HFFMWFRHWFF",
    )

    val terrain : Map[Hex, Terrain] = rows.zipWithIndex.flatMap { case (s, r) => s.zipWithIndex.map { case (ch, c) => Hex(c, r) -> Terrain.all.find(_.letter == ch).get } }.toMap

    val hexes : $[Hex] = terrain.keys.$.sortBy(x => (x.r, x.c))

    def x(x : Hex) : Double = x0 + w * (x.c - (x.r % 2) * 0.5)
    def y(x : Hex) : Double = y0 + h * x.r

    def edge(x : Hex) = x.r == 0 || x.r == rows.num - 1 || x.c == 0 || x.c == rows(x.r).length - 1

    private val byAxial = hexes./(x => (x.q, x.r) -> x).toMap

    def at(q : Int, r : Int) : |[Hex] = byAxial.get((q, r))

    val directions = $((1, 0), (1, -1), (0, -1), (-1, 0), (-1, 1), (0, 1))

    val neighbors : Map[Hex, $[Hex]] = hexes./(x => x -> directions./~((dq, dr) => at(x.q + dq, x.r + dr))).toMap

    def adjacent(a : Hex, b : Hex) = neighbors(a).has(b)

    def distance(a : Hex, b : Hex) = {
        val dq = a.q - b.q
        val dr = a.r - b.r
        (dq.abs + dr.abs + (dq + dr).abs) / 2
    }

    // the direction from a to b, for road and bridge images: "e" (east or west), "ne" (north-east or south-west), "se"
    def direction(a : Hex, b : Hex) = {
        val dq = (b.q - a.q).sign
        val dr = (b.r - a.r).sign
        (dq, dr) match {
            case (1, 0) | (-1, 0) => "e"
            case (1, -1) | (-1, 1) => "ne"
            case _ => "se"
        }
    }

    // the land hex across the water hex w from a, if any (a bridge spans one water hex in a straight line)
    def across(a : Hex, w : Hex) : |[Hex] = at(2 * w.q - a.q, 2 * w.r - a.r)

    // the hexes at most 2 away, with whether a shot from a reaches them: at 2, through a hex next to both that isn't a mountain
    def within(a : Hex, n : Int, overMountains : Boolean) : $[Hex] = {
        val one = neighbors(a)
        if (n <= 1)
            one
        else
            (one ++ hexes.%(b => distance(a, b) == 2 && (overMountains || neighbors(a).exists(m => adjacent(m, b) && terrain(m) != Mountain)))).distinct
    }

    // the distance between two hexes over land only (water separates), None when more than limit
    def landDistance(a : Hex, b : Hex, limit : Int) : |[Int] = {
        var seen = Set(a)
        var front = $(a)
        var d = 0
        while (front.any && d < limit) {
            if (front.has(b))
                return |(d)
            d += 1
            front = front./~(neighbors).distinct.%(x => seen.contains(x).not && terrain(x).land)
            seen ++= front
        }
        front.has(b).?(d)
    }
}
