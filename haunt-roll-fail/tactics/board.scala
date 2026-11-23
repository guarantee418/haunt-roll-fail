package tactics

// The board: the tiles by their coordinates, so the map can be any shape, and the queries on them.
// Immutable: `add` gives a new board.

case class Board(tiles : Map[Hex, HexTile] = Map()) {
    // A board with the tile added, replacing any already at its coordinate
    def add(t : HexTile) = copy(tiles = tiles + (t.hex -> t))

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

    // Every hex any of the living units sees: within its sight range and in its line of sight (its own hex too)
    def visibleHexes(units : Iterable[CombatUnit]) : Set[Hex] =
        units.filter(u => u.alive && contains(u.hex)).flatMap { u =>
            tiles.keys.filter(h => h.distance(u.hex) <= u.sightRange && lineOfSight(u.hex, h))
        }.toSet

    private def clear(line : List[Hex]) = line.drop(1).dropRight(1).forall(h => !tiles.get(h).exists(_.blocksSight))
}

object Board {
    def of(tiles : Iterable[HexTile]) = Board(tiles.map(t => t.hex -> t).toMap)

    // A hexagon of tiles around (0, 0)
    def hexagon(radius : Int, terrain : Terrain = Plains) : Board = of(
        for {
            q <- (-radius).to(radius)
            r <- (-radius).max(-q - radius).to(radius.min(-q + radius))
        } yield HexTile(q, r, terrain)
    )
}
