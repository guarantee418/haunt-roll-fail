package cyclades
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
import cyclades.elem._

// THE BOARD: the round spaces of the sea, in rows, every odd row shifted half a space right. A cell is (row, column), with
// (0, 0) at the middle of the board; the isles cover some of the cells. The three layouts are the game's two boards (the 2-3
// player side and the 4-5 player side) and the 4-player board the rulebook makes of the 4-5 side's left half and the 2-3 side's
// right half. The cells, isles, prosperity and building spaces were read from the board images (tools/, see HANDOFF.md).

case class Cell(r : Int, k : Int) extends Record {
    // half a space right on the odd rows
    def x : Double = k + ((r % 2 + 2) % 2) * 0.5
    def name = ('A' + r + 5).toChar.toString + (k + 7)
}

sealed trait Area extends Record with Elementary

// a sea space, named by its row and column, as the board prints none
case class Sea(cell : Cell) extends Area {
    def name = cell.name
    def elem = ("Sea " + name).styled(styles.sea)
}

// an isle, named after the islands of the Cyclades (the board names none)
case class Isle(name : String) extends Area {
    def elem = name.styled(styles.isle)
}

case class IsleData(isle : Isle, cells : $[Cell], prosperity : Int, slots : Int, metro : Int)

object Isles {
    private def i(n : String, cells : $[(Int, Int)], p : Int, slots : Int, metro : Int) = IsleData(Isle(n), cells./((r, k) => Cell(r, k)), p, slots, metro)

    // the 4-5 player side
    val big = $(
        i("Naxos", $((-4, -3), (-4, -2), (-3, -4), (-3, -3)), 0, 4, 2),
        i("Delos", $((-1, -4)), 2, 1, 1),
        i("Andros", $((-5, 0), (-4, 1)), 1, 2, 2),
        i("Paros", $((-2, -1), (-1, -1)), 1, 2, 2),
        i("Syros", $((0, -2), (1, -2)), 1, 2, 2),
        i("Tinos", $((-2, 1), (-2, 2)), 1, 2, 2),
        i("Mykonos", $((-2, 4), (-1, 3)), 1, 2, 2),
        i("Ios", $((0, 1)), 2, 1, 1),
        i("Milos", $((1, -5), (2, -4), (2, -3)), 0, 2, 1),
        i("Amorgos", $((3, -2), (4, -2), (4, -1)), 0, 3, 2),
        i("Kea", $((2, 2)), 2, 1, 1),
        i("Serifos", $((3, 0), (4, 1), (5, 1)), 0, 2, 1),
        i("Thira", $((1, 4), (2, 4), (3, 3), (4, 3)), 0, 3, 1),
    )

    // the 2-3 player side
    val small = $(
        i("Sifnos", $((-4, -1)), 2, 1, 1),
        i("Kythnos", $((-3, 0), (-3, 1)), 1, 2, 2),
        i("Paros", $((-2, -1), (-1, -1)), 1, 2, 2),
        i("Anafi", $((-1, 1)), 2, 1, 1),
        i("Folegandros", $((0, -2), (1, -3), (1, -2)), 0, 3, 2),
        i("Sikinos", $((1, 0)), 2, 1, 1),
        i("Donousa", $((1, 2), (2, 2), (3, 1), (4, 1)), 0, 3, 1),
        i("Kimolos", $((3, -2), (4, -1), (5, -1)), 0, 2, 1),
    )
}

// A start: the isle with a Troop on it, the sea space with a Fleet on it
case class Start(isle : String, fleet : (Int, Int))

case class Layout(name : String, image : String, width : Int, height : Int, x0 : Double, y0 : Double, dx : Double, dy : Double,
    cells : $[Cell], isles : $[IsleData], commerce : $[Cell], starts : $[$[Start]]) {

    val isleOf : Map[Cell, Isle] = isles./~(d => d.cells./(_ -> d.isle)).toMap
    val data : Map[Isle, IsleData] = isles./(d => d.isle -> d).toMap
    val seas : $[Sea] = cells.%(c => isleOf.contains(c).not)./(Sea)
    val islands : $[Isle] = isles./(_.isle)
    val areas : $[Area] = seas ++ islands

    def px(c : Cell) = x0 + c.x * dx
    def py(c : Cell) = y0 + c.r * dy

    private val cellSet = cells.toSet

    def neighborCells(c : Cell) : $[Cell] = cells.%(d => d != c && (d.r - c.r).abs <= 1 && (d.x - c.x).abs <= ((d.r == c.r).?(1.0).|(0.5)) + 0.01)

    def areaOf(c : Cell) : Area = isleOf.get(c).|(Sea(c))

    // areas touching: two sea spaces side by side, an isle and the sea spaces around it
    val neighbors : Map[Area, $[Area]] = areas./(a => a -> (a match {
        case Sea(c) => neighborCells(c)./(areaOf).distinct
        case i : Isle => data(i).cells./~(neighborCells)./(areaOf).distinct.but(i)
    })).toMap

    def seasAround(i : Isle) : $[Sea] = neighbors(i).of[Sea]
    def seasNext(s : Sea) : $[Sea] = neighbors(s).of[Sea]

    // an isle's middle, for its pieces
    def center(i : Isle) : (Double, Double) = {
        val l = data(i).cells
        (l./(px).sum / l.num, l./(py).sum / l.num)
    }
}

object Layouts {
    // the 4-5 player side's sea, inside the ring
    private val bigCells = (-6).to(6).$./~(r => (-7).to(7).$./(k => Cell(r, k))).%{ c =>
        val x = c.x * 212
        val y = c.r * 212 * math.sqrt(3) / 2
        math.sqrt(x * x + (y - 5) * (y - 5)) <= 1090
    }

    // the 2-3 player side's oval, by row: its first and last columns
    private val smallRows = $(-5 -> (-1, 0), -4 -> (-1, 1), -3 -> (-2, 1), -2 -> (-2, 2), -1 -> (-3, 2), 0 -> (-3, 3), 1 -> (-3, 2), 2 -> (-2, 2), 3 -> (-2, 1), 4 -> (-1, 1), 5 -> (-1, 0))
    private val smallCells = smallRows./~{ case (r, (a, b)) => a.to(b).$./(k => Cell(r, k)) }

    private def c(l : (Int, Int)*) = l.$./((r, k) => Cell(r, k))

    val five = Layout("5", "map-5", 2048, 2048, 1011, 980.5, 188.5, 159, bigCells, Isles.big,
        c((-5, -3), (-5, 2), (0, -5), (0, 5), (5, -3), (5, 2)), $(
            $(Start("Naxos", (-4, -1)), Start("Ios", (-1, 1))),
            $(Start("Delos", (0, -3)), Start("Thira", (1, 3))),
            $(Start("Mykonos", (-2, 3)), Start("Milos", (0, -5))),
            $(Start("Paros", (0, -1)), Start("Serifos", (5, 2))),
            $(Start("Andros", (-3, 1)), Start("Amorgos", (5, -3))),
        ))

    // the 4-player board: the 4-5 side's left half and the 2-3 side's right half
    val four = Layout("4", "map-4", 1680, 2048, 1011, 980.5, 188.5, 159,
        (bigCells.%(_.x <= 0) ++ smallCells.%(_.x > 0)).distinct,
        Isles.big.%(_.cells.forall(_.x < 0)) ++ Isles.small.%(_.cells.forall(_.x > 0)),
        c((-5, -3), (0, -5), (5, -3), (-5, 0), (0, 3), (5, 0)), $(
            $(Start("Naxos", (-4, -1)), Start("Sikinos", (0, 1))),
            $(Start("Paros", (0, -1)), Start("Amorgos", (5, -3))),
            $(Start("Delos", (0, -3)), Start("Donousa", (3, 0))),
            $(Start("Milos", (0, -5)), Start("Kythnos", (-2, 1))),
        ))

    val three = Layout("3", "map-23", 2054, 2013, 1027, 1006.4, 181, 156.8, smallCells, Isles.small,
        c((-5, 0), (0, -3), (0, 3), (5, 0)), $(
            $(Start("Sifnos", (-3, -1)), Start("Donousa", (1, 1))),
            $(Start("Kythnos", (-2, 1)), Start("Folegandros", (0, -3))),
            $(Start("Paros", (0, 0)), Start("Kimolos", (5, 0))),
        ))

    val two = three.copy(name = "2", starts = $(
        $(Start("Sifnos", (-3, -1)), Start("Donousa", (0, 2))),
        $(Start("Kythnos", (-2, 1)), Start("Kimolos", (5, 0))),
    ))

    def of(players : Int) = players match {
        case 2 => two
        case 3 => three
        case 4 => four
        case _ => five
    }

    val all = $(two, three, four, five)
}
