package tactics

// The starter skirmish: a two-player map of pointy-top hexes in rows, 13 hexes in the even rows and 12 in the odd
// ones (shifted half a hex right), so that turning it half round its middle hex gives the same map: each side gets the
// mirror image of the other's ground. Only engine types, so HexCheck tests it with the rest.
//
// Hexes are named like C4 on the screen: column letter (A at the left), row number (1 at the top).

object Skirmish {
    val rows = 9

    def columns(row : Int) = if (row % 2 == 0) 13 else 12

    // offset (column, row) to axial and back; odd rows are shifted half a hex right ("odd-r")
    def axial(col : Int, row : Int) = Hex(col - (row - (row & 1)) / 2, row)
    def column(h : Hex) = h.q + (h.r - (h.r & 1)) / 2

    def name(h : Hex) = ('A' + column(h)).toChar.toString + (h.r + 1)

    // the middle hex, F5 is G5's left neighbour; turning half round it maps (q, r) to (2c.q - q, 2c.r - r)
    val middle = axial(6, 4)

    def turned(h : Hex) = Hex(2 * middle.q - h.q, 2 * middle.r - h.r)

    // the first four rows and the middle row up to the middle hex; the rest is them turned half round.
    // . plains, f forest, h hills, w water, m mountain, b building
    private val north = List(
        "..ff...h..w..",
        ".f..m.ff..w.",
        "...h..b...hh.",
        "..ff..w..ff.",
        "..h.f.b"
    )

    private def terrainOf(c : Char) : Terrain = c match {
        case '.' => Plains
        case 'f' => Forest
        case 'h' => Hills
        case 'w' => Water
        case 'm' => Mountain
        case 'b' => Building
    }

    // forests and hills give light cover, buildings heavy
    private def coverOf(t : Terrain) : Cover = t match {
        case Forest | Hills => LightCover
        case Building => HeavyCover
        case _ => NoCover
    }

    // the sectors on the west (Red) half, and the town in the middle: name, column, row, income, VP
    case class Sector(name : String, col : Int, row : Int, income : Resources, vp : Option[Int])

    private val westSectors = List(
        Sector("Supply depot", 2, 1, Resources(munitions = 2), None),
        Sector("Fuel dump", 2, 7, Resources(fuel = 2), None),
        Sector("Farm", 4, 4, Resources(manpower = 1), None),
        Sector("Ridge", 7, 0, Resources.none, Some(1))
    )

    val town = Sector("Town", 6, 4, Resources(manpower = 1), Some(2))

    // every sector, the east ones being the west ones turned half round
    val sectors : List[(Hex, String)] =
        (town :: westSectors).map(s => axial(s.col, s.row) -> s.name) ++ westSectors.map(s => turned(axial(s.col, s.row)) -> s.name)

    private val sectorTiles : Map[Hex, Sector] =
        ((town :: westSectors).map(s => axial(s.col, s.row) -> s) ++ westSectors.map(s => turned(axial(s.col, s.row)) -> s)).toMap

    val board : Board = {
        val drawn = north.zipWithIndex.flatMap { case (line, row) =>
            line.zipWithIndex.map { case (c, col) => axial(col, row) -> terrainOf(c) }
        }.toMap

        val all = drawn ++ drawn.map { case (h, t) => turned(h) -> t }

        Board.of(all.toList.map { case (h, t) =>
            val s = sectorTiles.get(h)
            HexTile(h.q, h.r, t, coverOf(t), s.map(_.income).getOrElse(Resources.none), s.flatMap(_.vp))
        })
    }

    def sectorName(h : Hex) : Option[String] = sectors.find(_._1 == h).map(_._2)

    // the two west columns of every row, and their turned image; only hexes infantry can stand on
    val westZone : Set[Hex] = 0.until(rows).flatMap(r => List(axial(0, r), axial(1, r))).filter(h => board.get(h).exists(_.terrain.passable(Infantry))).toSet
    val eastZone : Set[Hex] = westZone.map(turned)

    val rules = Rules(vpTarget = 10, start = Resources(manpower = 6, munitions = 2), base = Resources(manpower = 2, munitions = 1), orders = Some((3, 5)), fog = true)

    // each side starts with a squad of riflemen in the middle of its zone
    def startingUnits(west : String, east : String) = List(
        Roster.riflemen.make(west + "-A", west, axial(1, 4)),
        Roster.riflemen.make(east + "-A", east, turned(axial(1, 4)))
    )

    // the match: the first player on the west side, the second on the east
    def start(west : String, east : String) : Either[String, GameState] =
        GameState.start(board, List(west, east), Map(west -> westZone, east -> eastZone), startingUnits(west, east), rules)
}
