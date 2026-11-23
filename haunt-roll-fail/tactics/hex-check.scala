package tactics

// Checks of the hex math and movement, run on the JVM: `runMain tactics.HexCheck` (or scalac and
// java, see HANDOFF.md). Exits with 1 if any check fails.

object HexCheck {
    var failed = 0
    var passed = 0

    def check(name : String)(ok : => Boolean) : Unit = {
        val result = try ok catch { case e : Throwable => println("  " + e) ; false }
        if (result) passed += 1 else { failed += 1 ; println("FAIL " + name) }
    }

    def throws[E <: Throwable](f : => Any)(implicit m : scala.reflect.ClassTag[E]) =
        try { f ; false } catch { case e : Throwable => m.runtimeClass.isInstance(e) }

    // A radius 3 hexagon of plains with the given tiles changed
    def board(changed : (Int, Int, Terrain)*) = changed.foldLeft(Board.hexagon(3)) { case (b, (q, r, t)) => b.add(HexTile(q, r, t)) }

    // A one-hex-wide corridor from (0, 0) to (n, 0)
    def corridor(n : Int) = Board.of(0.to(n).map(q => HexTile(q, 0)))

    def soldier(id : String, q : Int, r : Int, mp : Int = 3, owner : String = "red") = CombatUnit(id, owner, 10, mp, 1, q, r)
    def tank(id : String, q : Int, r : Int, mp : Int = 4, owner : String = "red") = CombatUnit(id, owner, 20, mp, 3, q, r, Vehicle)

    def state(b : Board, us : CombatUnit*) = GameState(b, us.map(u => u.id -> u).toMap)

    def contiguous(p : Path) = p.hexes.zip(p.hexes.drop(1)).forall { case (a, b) => a.distance(b) == 1 }

    def main(args : Array[String]) : Unit = {
        hexMath()
        movement()

        println(passed.toString + " passed, " + failed + " failed")
        if (failed > 0)
            sys.exit(1)
    }

    def hexMath() : Unit = {
        // Distance
        check("same hex is 0") { Hex.distance(Hex(2, -1), Hex(2, -1)) == 0 }
        check("every neighbor is 1") { Hex(0, 0).neighbors.forall(Hex(0, 0).distance(_) == 1) }

        List(
            (Hex(0, 0), Hex(3, 0), 3),
            (Hex(0, 0), Hex(3, -1), 3),
            (Hex(0, 0), Hex(2, 2), 4),
            (Hex(-2, 3), Hex(2, -1), 4),
            (Hex(1, -3), Hex(-2, 1), 4)
        ).foreach { case (a, b, d) =>
            check("distance " + a + " " + b + " is " + d) { Hex.distance(a, b) == d && Hex.distance(b, a) == d }
        }

        // Neighbors
        check("radius 2 hexagon has 19 tiles") { Board.hexagon(2).size == 19 }
        check("centre has 6 neighbors") { Board.hexagon(2).neighbors(Hex(0, 0)).size == 6 }
        check("edge tile gets only tiles on the board") {
            Board.hexagon(1).neighbors(Hex(1, 0)).map(_.hex).toSet == Set(Hex(0, 0), Hex(1, -1), Hex(0, 1))
        }

        // Lines
        check("straight row") { Hex.line(Hex(0, 0), Hex(3, 0)) == List(Hex(0, 0), Hex(1, 0), Hex(2, 0), Hex(3, 0)) }
        check("line has both ends and no gaps") {
            val (a, b) = (Hex(-2, 3), Hex(3, -2))
            val l = Hex.line(a, b)
            l.head == a && l.last == b && l.size == Hex.distance(a, b) + 1 && l.zip(l.drop(1)).forall { case (x, y) => x.distance(y) == 1 }
        }

        // Line of sight
        check("open board is clear") { board().lineOfSight(Hex(0, 0), Hex(3, 0)) && board().lineOfSight(Hex(-3, 0), Hex(3, -3)) }
        check("mountain between blocks, both ways") {
            val b = board((1, 0, Mountain))
            !b.lineOfSight(Hex(0, 0), Hex(2, 0)) && !b.lineOfSight(Hex(2, 0), Hex(0, 0))
        }
        check("forest with heavy cover doesn't block") { Board.hexagon(3).add(HexTile(1, 0, Forest, HeavyCover)).lineOfSight(Hex(0, 0), Hex(2, 0)) }
        check("blockers at the ends don't block") { board((0, 0, Building), (2, 0, Mountain)).lineOfSight(Hex(0, 0), Hex(2, 0)) }
        check("adjacent always visible") { board((1, 0, Mountain)).lineOfSight(Hex(0, 0), Hex(1, 0)) }
        check("blocker off the line is ignored") { board((0, 1, Mountain)).lineOfSight(Hex(0, 0), Hex(3, 0)) }
        // (0, 0) to (2, -1) runs exactly along the edge between (1, 0) and (1, -1)
        check("edge line with one side blocked is clear") { board((1, 0, Mountain)).lineOfSight(Hex(0, 0), Hex(2, -1)) }
        check("edge line with both sides blocked is blocked") { !board((1, 0, Mountain), (1, -1, Building)).lineOfSight(Hex(0, 0), Hex(2, -1)) }
        check("end off the board throws") { throws[NoSuchElementException](board().lineOfSight(Hex(0, 0), Hex(10, 0))) }
        check("adding a tile leaves the old board as it was") {
            val b = board()
            b.add(HexTile(1, 0, Mountain))
            b.get(Hex(1, 0)).get.terrain == Plains
        }

        // Units
        check("unit moves") {
            val u = soldier("u1", 0, 0).at(Hex(2, -1))
            u.q == 2 && u.r == -1 && u.alive
        }
        check("negative stats rejected") { throws[IllegalArgumentException](CombatUnit("u1", "red", -1, 4, 0, 0, 0)) }
    }

    def movement() : Unit = {
        // Terrain costs
        check("infantry costs") {
            Plains.cost(Infantry) == Some(1) && Forest.cost(Infantry) == Some(2) && Hills.cost(Infantry) == Some(2) &&
            Building.cost(Infantry) == Some(2) && Water.cost(Infantry) == None && Mountain.cost(Infantry) == None
        }
        check("vehicle costs") {
            Plains.cost(Vehicle) == Some(1) && Forest.cost(Vehicle) == Some(3) && Hills.cost(Vehicle) == Some(2) &&
            Building.cost(Vehicle) == None && Water.cost(Vehicle) == None && Mountain.cost(Vehicle) == None
        }

        // Reachable hexes
        check("open plains: 2 movement reaches the 18 hexes within 2") {
            val r = state(board(), soldier("a", 0, 0, mp = 2)).reachable("a")
            r.keySet == Board.hexagon(2).tiles.keySet - Hex(0, 0) && r.forall { case (h, c) => c == h.distance(Hex(0, 0)) }
        }
        check("forest costs 2 to enter") {
            val r = state(board((1, 0, Forest)), soldier("a", 0, 0, mp = 2)).reachable("a")
            r.get(Hex(1, 0)) == Some(2) && r.get(Hex(1, -1)) == Some(1) && !r.contains(Hex(2, 0))
        }
        check("0 movement reaches nothing") { state(board(), soldier("a", 0, 0, mp = 0)).reachable("a").isEmpty }
        check("unit ringed by water reaches nothing") {
            val b = Hex(0, 0).neighbors.foldLeft(board()) { case (b, h) => b.add(HexTile(h.q, h.r, Water)) }
            state(b, soldier("a", 0, 0, mp = 6)).reachable("a").isEmpty
        }
        check("unit out of action reaches nothing") { state(board(), soldier("a", 0, 0).copy(health = 0)).reachable("a").isEmpty }
        check("unknown unit reaches nothing") { state(board()).reachable("nobody").isEmpty }

        // Impassable terrain
        check("no path into water or a mountain") {
            val s = state(board((1, 0, Water), (0, 1, Mountain)), soldier("a", 0, 0))
            s.path("a", Hex(1, 0)).isEmpty && s.path("a", Hex(0, 1)).isEmpty
        }
        check("mountain in the way is gone around") {
            val p = state(board((1, 0, Mountain)), soldier("a", 0, 0)).path("a", Hex(2, 0)).get
            p.cost == 3 && p.steps == 3 && !p.hexes.contains(Hex(1, 0)) && contiguous(p)
        }
        check("infantry enters a building, a vehicle can't") {
            val s = state(board((1, 0, Building)), soldier("a", 0, 0), tank("t", -1, 0))
            s.reachable("a").get(Hex(1, 0)) == Some(2) && s.path("t", Hex(1, 0)).isEmpty
        }
        check("vehicle goes around a forest it could cross") {
            // Through (1, 0) and (2, 0): 3 + 3 + 1 = 7; around them on plains: 4
            val p = state(board((1, 0, Forest), (2, 0, Forest)), tank("t", 0, 0)).path("t", Hex(3, 0)).get
            p.cost == 4 && p.hexes.forall(h => h != Hex(1, 0) && h != Hex(2, 0))
        }
        check("cut off by a wall of mountains") {
            val wall = (-3).to(3).flatMap(r => if (Board.hexagon(3).contains(Hex(1, r))) Some((1, r, Mountain : Terrain)) else None)
            state(board(wall : _*), soldier("a", 0, 0, mp = 20)).path("a", Hex(3, 0)).isEmpty
        }

        // Multi-hex paths
        check("straight path over plains") {
            val p = state(board(), soldier("a", 0, 0)).path("a", Hex(3, 0)).get
            p == Path(List(Hex(0, 0), Hex(1, 0), Hex(2, 0), Hex(3, 0)), 3)
        }
        check("cheaper long way beats dear short way") {
            // Straight through two forests: 2 + 2 + 1 = 5; the 4 step way around is 4
            val p = state(board((1, 0, Forest), (2, 0, Forest)), soldier("a", 0, 0)).path("a", Hex(3, 0)).get
            p.cost == 4 && p.steps == 4 && p.start == Hex(0, 0) && p.end == Hex(3, 0) && contiguous(p)
        }
        check("path cost adds the terrain entered") {
            val b = board((1, 0, Hills), (2, 0, Forest), (3, 0, Hills))
            val p = state(b, soldier("a", 0, 0)).path("a", Hex(3, 0)).get
            p.cost == p.hexes.drop(1).map(h => b.get(h).get.terrain.cost(Infantry).get).sum
        }
        check("path is the same every time") {
            val s = state(board((1, 0, Mountain)), soldier("a", 0, 0))
            1.to(10).map(_ => s.path("a", Hex(2, 0))).toSet.size == 1
        }
        check("path ignores movement points left, for planning") { state(board(), soldier("a", -3, 0, mp = 1)).path("a", Hex(3, 0)).map(_.cost) == Some(6) }

        // Unit blocking
        check("enemy in a corridor blocks it") { state(corridor(3), soldier("a", 0, 0), soldier("e", 1, 0, owner = "blue")).path("a", Hex(3, 0)).isEmpty }
        check("friend in a corridor is passed through") {
            val s = state(corridor(3), soldier("a", 0, 0), soldier("f", 1, 0))
            s.path("a", Hex(3, 0)).map(_.cost) == Some(3) && s.reachable("a").keySet == Set(Hex(2, 0), Hex(3, 0))
        }
        check("can't end on a unit") {
            val s = state(board(), soldier("a", 0, 0), soldier("f", 1, 0), soldier("e", 0, 1, owner = "blue"))
            s.path("a", Hex(1, 0)).isEmpty && s.path("a", Hex(0, 1)).isEmpty && s.move("a", Hex(1, 0)).isLeft
        }
        check("two units in one hex can't be made") { throws[IllegalArgumentException](state(board(), soldier("a", 0, 0), soldier("b", 0, 0))) }
        check("unit off the board can't be made") { throws[IllegalArgumentException](state(board(), soldier("a", 9, 0))) }
        check("placing") {
            val s = state(board((2, 0, Water), (3, 0, Building)), soldier("a", 0, 0))
            s.place(soldier("b", 0, 0)).isLeft &&             // taken
            s.place(soldier("a", 1, 0)).isLeft &&             // id taken
            s.place(soldier("b", 9, 0)).isLeft &&             // off the board
            s.place(soldier("b", 2, 0)).isLeft &&             // water
            s.place(tank("t", 3, 0)).isLeft &&                // vehicle in a building
            s.place(soldier("b", 3, 0)).map(_.at(Hex(3, 0)).map(_.id)) == Right(Some("b"))
        }

        // Moving
        check("move gives a new state and leaves the old one") {
            val s = state(board((1, 0, Forest)), soldier("a", 0, 0))
            val t = s.move("a", Hex(1, 0)).toOption.get
            s.unit("a").get.hex == Hex(0, 0) && t.unit("a").get.hex == Hex(1, 0) && t.unit("a").get.spent == 2 &&
            t.at(Hex(1, 0)).map(_.id) == Some("a") && t.at(Hex(0, 0)).isEmpty
        }
        check("moves spend points until there are none") {
            val s = state(board(), soldier("a", 0, 0))
            val t = s.move("a", Hex(2, 0)).toOption.get
            t.unit("a").get.movesLeft == 1 && t.reachable("a").keySet == Hex(2, 0).neighbors.toSet &&
            t.move("a", Hex(3, -1)).isRight && t.move("a", Hex(0, 0)).isLeft
        }
        check("move too far is refused") { state(board(), soldier("a", 0, 0, mp = 2)).move("a", Hex(3, 0)).isLeft }
        check("bad moves are refused") {
            val s = state(board((1, 0, Water)), soldier("a", 0, 0))
            s.move("nobody", Hex(1, 0)).isLeft && s.move("a", Hex(0, 0)).isLeft && s.move("a", Hex(9, 0)).isLeft && s.move("a", Hex(1, 0)).isLeft
        }
        check("new turn gives movement back") {
            val t = state(board(), soldier("a", 0, 0)).move("a", Hex(3, 0)).toOption.get
            t.reachable("a").isEmpty && t.newTurn.unit("a").get.movesLeft == 3
        }
        check("every reachable hex can be moved to for its cost") {
            val s = state(board((1, 0, Forest), (0, 1, Hills), (-1, 0, Water), (1, -1, Building)), soldier("a", 0, 0, mp = 4), soldier("f", 0, -1), soldier("e", -1, 1, owner = "blue"))
            s.reachable("a").forall { case (h, c) => s.move("a", h).toOption.flatMap(_.unit("a")).map(u => u.hex == h && u.spent == c) == Some(true) }
        }
    }
}
