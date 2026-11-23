package tactics

// The board: the tiles by their coordinates, so the map can be any shape, and the queries on them.

class Board(initial : Iterable[HexTile] = Nil) {
    private var tiles : Map[Hex, HexTile] = initial.map(t => t.hex -> t).toMap

    // Adds a tile, replacing any already at its coordinate
    def add(t : HexTile) : Unit = tiles += t.hex -> t

    def get(h : Hex) : Option[HexTile] = tiles.get(h)
    def contains(h : Hex) = tiles.contains(h)
    def size = tiles.size
    def all : List[HexTile] = tiles.values.toList

    def distance(a : Hex, b : Hex) = Hex.distance(a, b)

    // The tiles next to h that are on the board
    def neighbors(h : Hex) : List[HexTile] = h.neighbors.flatMap(tiles.get)

    // True when no hex strictly between a and b has blocking terrain.
    // - The two ends never block: a unit in a building can see out and be seen.
    // - Hexes missing from the board are open.
    // - A line running exactly along a hex edge is traced on both sides of it, and
    //   is blocked only if both sides are.
    // Throws NoSuchElementException when either end isn't on the board.
    def lineOfSight(a : Hex, b : Hex) : Boolean = {
        List(a, b).foreach { h =>
            if (!contains(h))
                throw new NoSuchElementException(h.toString + " is not on the board")
        }

        List(1e-6, -1e-6).exists(n => clear(Hex.line(a, b, n)))
    }

    private def clear(line : List[Hex]) = line.drop(1).dropRight(1).forall(h => !tiles.get(h).exists(_.blocksSight))
}

object Board {
    // A hexagon of tiles around (0, 0)
    def hexagon(radius : Int, terrain : Terrain = Plains) : Board = new Board(
        for {
            q <- (-radius).to(radius)
            r <- (-radius).max(-q - radius).to(radius.min(-q + radius))
        } yield HexTile(q, r, terrain)
    )
}
