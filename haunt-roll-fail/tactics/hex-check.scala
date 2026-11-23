package tactics

// Checks of the hex math, run on the JVM: `runMain tactics.HexCheck` (or scalac and scala, see HANDOFF.md).
// Exits with 1 if any check fails.

object HexCheck {
    var failed = 0
    var passed = 0

    def check(name : String)(ok : => Boolean) : Unit = {
        val result = try ok catch { case e : Throwable => println("  " + e) ; false }
        if (result) passed += 1 else { failed += 1 ; println("FAIL " + name) }
    }

    def throws[E <: Throwable](f : => Any)(implicit m : scala.reflect.ClassTag[E]) =
        try { f ; false } catch { case e : Throwable => m.runtimeClass.isInstance(e) }

    def main(args : Array[String]) : Unit = {
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
        def board(blocked : (Int, Int, Terrain)*) = {
            val b = Board.hexagon(3)
            blocked.foreach { case (q, r, t) => b.add(HexTile(q, r, t)) }
            b
        }

        check("open board is clear") { board().lineOfSight(Hex(0, 0), Hex(3, 0)) && board().lineOfSight(Hex(-3, 0), Hex(3, -3)) }
        check("mountain between blocks, both ways") {
            val b = board((1, 0, Mountain))
            !b.lineOfSight(Hex(0, 0), Hex(2, 0)) && !b.lineOfSight(Hex(2, 0), Hex(0, 0))
        }
        check("forest with heavy cover doesn't block") {
            val b = Board.hexagon(3)
            b.add(HexTile(1, 0, Forest, HeavyCover))
            b.lineOfSight(Hex(0, 0), Hex(2, 0))
        }
        check("blockers at the ends don't block") { board((0, 0, Building), (2, 0, Mountain)).lineOfSight(Hex(0, 0), Hex(2, 0)) }
        check("adjacent always visible") { board((1, 0, Mountain)).lineOfSight(Hex(0, 0), Hex(1, 0)) }
        check("blocker off the line is ignored") { board((0, 1, Mountain)).lineOfSight(Hex(0, 0), Hex(3, 0)) }
        // (0, 0) to (2, -1) runs exactly along the edge between (1, 0) and (1, -1)
        check("edge line with one side blocked is clear") { board((1, 0, Mountain)).lineOfSight(Hex(0, 0), Hex(2, -1)) }
        check("edge line with both sides blocked is blocked") { !board((1, 0, Mountain), (1, -1, Building)).lineOfSight(Hex(0, 0), Hex(2, -1)) }
        check("end off the board throws") { throws[NoSuchElementException](board().lineOfSight(Hex(0, 0), Hex(10, 0))) }

        // Units
        check("unit moves") {
            val u = CombatUnit("u1", "red", 10, 4, 2, 0, 0).at(Hex(2, -1))
            u.q == 2 && u.r == -1 && u.alive
        }
        check("negative stats rejected") { throws[IllegalArgumentException](CombatUnit("u1", "red", -1, 4, 0, 0, 0)) }

        println(passed.toString + " passed, " + failed + " failed")
        if (failed > 0)
            sys.exit(1)
    }
}
