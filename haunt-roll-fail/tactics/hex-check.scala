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
        combat()
        matches()
        skirmish()

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

    def combat() : Unit = {
        import Weapons._

        // A red rifleman at (0, 0) and a blue target
        def duel(target : CombatUnit, b : Board = board(), w : Weapon = rifle) = state(b, soldier("a", 0, 0).armed(w), target)
        def enemy(q : Int, r : Int, health : Int = 10, armor : Int = 0) = CombatUnit("e", "blue", health, 3, armor, q, r)
        def enemyTank(q : Int, r : Int, armor : Int = 3) = CombatUnit("e", "blue", 20, 4, armor, q, r, Vehicle)
        val wide = Board.hexagon(6)
        def report(s : GameState, rolls : Int*) = s.attack("a", "e", rolls).toOption.get._2
        def error(s : GameState, rolls : Int*) = s.attack("a", "e", rolls).swap.toOption.get

        // Range
        check("in range at 1 and at the weapon's range") { duel(enemy(1, 0)).canAttack("a", "e") && duel(enemy(3, 0)).canAttack("a", "e") }
        check("out of range") { !duel(enemy(4, 0), wide).canAttack("a", "e") && error(duel(enemy(3, 1), wide), 6, 6, 6).contains("4 hexes away") }
        check("mortar can't fire at an adjacent hex") {
            !duel(enemy(1, 0), w = mortar).canAttack("a", "e") && duel(enemy(2, 0), w = mortar).canAttack("a", "e")
        }

        // Line of sight
        check("mountain between blocks the attack") {
            val s = duel(enemy(2, 0), board((1, 0, Mountain)))
            !s.canAttack("a", "e") && error(s, 6, 6, 6).contains("can't see")
        }
        check("a unit between doesn't block") { state(board(), soldier("a", 0, 0).armed(rifle), soldier("f", 1, 0), enemy(2, 0)).canAttack("a", "e") }
        check("target in a building can be seen") { duel(enemy(2, 0), board((2, 0, Building))).canAttack("a", "e") }

        // Hitting
        check("each die hits on the accuracy or more") {
            val r = report(duel(enemy(1, 0)), 4, 3, 6)
            r.needed == 4 && r.hits == 2 && r.damagePerHit == 2 && r.damage == 4 && r.health == 6
        }
        check("rolls are kept in the report") { report(duel(enemy(1, 0)), 1, 5, 2).rolls == List(1, 5, 2) }

        // Cover
        def covered(c : Cover) = duel(enemy(2, 0), Board.hexagon(3).add(HexTile(2, 0, Plains, c)))
        check("light cover needs 1 more") { val r = report(covered(LightCover), 4, 4, 5) ; r.needed == 5 && r.hits == 1 && r.cover == LightCover }
        check("heavy cover needs 2 more") { val r = report(covered(HeavyCover), 4, 5, 5) ; r.needed == 6 && r.hits == 0 && r.damage == 0 }
        check("a 6 hits whatever the cover") {
            val s = state(Board.hexagon(3).add(HexTile(2, 0, Forest, HeavyCover)), soldier("a", 0, 0).armed(machineGun).copy(suppressed = true), enemy(2, 0))
            val r = report(s, 6, 5, 5, 5, 5)
            r.needed == 6 && r.hits == 1
        }
        check("a 1 always misses") { report(duel(enemy(1, 0), w = rifle.copy(accuracy = 2)), 1, 1, 2).hits == 1 }
        check("suppressed attacker needs 1 more") { report(state(board(), soldier("a", 0, 0).armed(rifle).copy(suppressed = true), enemy(1, 0)), 4, 4, 5).needed == 5 }

        // Armor
        check("armor takes off damage") { val r = report(duel(enemy(1, 0, armor = 1)), 6, 6, 6) ; r.damagePerHit == 1 && r.damage == 3 }
        check("armor can stop all damage") { val r = report(duel(enemy(1, 0, armor = 2)), 6, 6, 6) ; r.hits == 3 && r.damage == 0 && r.health == 10 }
        check("rifle does nothing to a tank") { report(duel(enemyTank(2, 0)), 6, 6, 6).damage == 0 }
        check("anti-tank gun goes through tank armor") {
            val r = report(duel(enemyTank(2, 0), w = antiTank), 5, 6)
            r.damagePerHit == 6 && r.damage == 12 && r.health == 8
        }
        check("armor beyond penetration still counts") { report(duel(enemyTank(2, 0, armor = 5), w = antiTank), 6, 6).damagePerHit == 4 }
        check("anti-tank gun is poor against infantry") { report(duel(enemy(2, 0), w = antiTank), 6, 6).damage == 2 }

        // Destruction
        check("unit at 0 health is destroyed and taken off") {
            val (s, r) = duel(enemy(1, 0, health = 3)).attack("a", "e", Seq(6, 6, 1)).toOption.get
            r.destroyed && r.health == 0 && r.damage == 3 && s.unit("e").isEmpty && !s.occupied(Hex(1, 0))
        }
        check("exactly enough damage destroys") { duel(enemy(1, 0, health = 4)).attack("a", "e", Seq(6, 6, 1)).toOption.get._2.destroyed }
        check("one short leaves it standing") {
            val (s, r) = duel(enemy(1, 0, health = 5)).attack("a", "e", Seq(6, 6, 1)).toOption.get
            !r.destroyed && s.unit("e").get.health == 1
        }
        check("destroyed unit's hex can be moved into") {
            val s = duel(enemy(1, 0, health = 1)).attack("a", "e", Seq(6, 1, 1)).toOption.get._1
            s.move("a", Hex(1, 0)).isRight
        }

        // Suppression
        check("infantry hit is suppressed until the new turn") {
            val (s, r) = duel(enemy(1, 0, armor = 2)).attack("a", "e", Seq(6, 1, 1)).toOption.get
            r.suppressed && s.unit("e").get.suppressed && !s.newTurn.unit("e").get.suppressed
        }
        check("no hit, no suppression") { !report(duel(enemy(1, 0)), 1, 2, 3).suppressed }
        check("vehicles aren't suppressed") { !report(duel(enemyTank(2, 0)), 6, 6, 6).suppressed }
        check("already suppressed isn't reported again") {
            val s = state(board(), soldier("a", 0, 0).armed(rifle), enemy(1, 0).copy(suppressed = true))
            val (t, r) = s.attack("a", "e", Seq(6, 1, 1)).toOption.get
            !r.suppressed && t.unit("e").get.suppressed
        }

        // State
        check("attack gives a new state and leaves the old one") {
            val s = duel(enemy(1, 0))
            val t = s.attack("a", "e", Seq(6, 6, 6)).toOption.get._1
            s.unit("e").get.health == 10 && t.unit("e").get.health == 4 && t.unit("a") == s.unit("a")
        }
        check("other units are left alone") {
            val s = state(board(), soldier("a", 0, 0).armed(rifle), enemy(1, 0), soldier("o", 0, 1, owner = "blue"))
            s.attack("a", "e", Seq(6, 6, 6)).toOption.get._1.unit("o") == s.unit("o")
        }
        check("same rolls, same result") {
            val s = duel(enemy(2, 0), board((2, 0, Hills)))
            1.to(5).map(_ => s.attack("a", "e", Seq(5, 3, 6))).toSet.size == 1
        }
        check("targets lists who can be attacked") {
            val s = state(wide.add(HexTile(0, 2, Mountain)), soldier("a", 0, 0).armed(rifle), soldier("f", 1, 0),
                soldier("b1", 2, 0, owner = "blue"), soldier("b2", 0, 3, owner = "blue"), soldier("b3", -4, 2, owner = "blue"))
            // f is a friend, b2 is behind the mountain, b3 is out of range
            Combat.targets(s, "a").map(_.id) == List("b1")
        }

        // Invalid attacks
        check("unknown attacker or target") { duel(enemy(1, 0)).attack("x", "e", Seq(6, 6, 6)).isLeft && duel(enemy(1, 0)).attack("a", "x", Seq(6, 6, 6)).isLeft }
        check("own side can't be attacked") {
            val s = state(board(), soldier("a", 0, 0).armed(rifle), soldier("f", 1, 0))
            !s.canAttack("a", "f") && !s.canAttack("a", "a") && s.attack("a", "f", Seq(6, 6, 6)).swap.toOption.get.contains("same side")
        }
        check("unarmed unit can't attack") { state(board(), soldier("a", 0, 0), enemy(1, 0)).attack("a", "e", Seq(6, 6, 6)).swap.toOption.get.contains("no weapon") }
        check("out of action can't attack or be attacked") {
            !state(board(), soldier("a", 0, 0).armed(rifle).copy(health = 0), enemy(1, 0)).canAttack("a", "e") &&
            !duel(enemy(1, 0, health = 0)).canAttack("a", "e")
        }
        check("wrong number of dice") { error(duel(enemy(1, 0)), 6, 6).contains("3 dice") && error(duel(enemy(1, 0)), 6, 6, 6, 6).contains("3 dice") }
        check("rolls must be 1 to 6") { error(duel(enemy(1, 0)), 0, 6, 6).contains("1 to 6") && error(duel(enemy(1, 0)), 7, 6, 6).contains("1 to 6") }
        check("a refused attack changes nothing") {
            val s = duel(enemy(5, 0), wide)
            s.attack("a", "e", Seq(6, 6, 6)).isLeft && s.unit("e").get.health == 10
        }
        check("bad weapons can't be made") {
            throws[IllegalArgumentException](rifle.copy(range = 0)) && throws[IllegalArgumentException](rifle.copy(accuracy = 7)) &&
            throws[IllegalArgumentException](rifle.copy(dice = 0)) && throws[IllegalArgumentException](mortar.copy(range = 1))
        }
    }

    def matches() : Unit = {
        import Roster._

        // A radius 4 board: the Crossroads in the middle (2 VP, 1 munitions), a fuel depot that red starts on,
        // a manpower village that blue starts on; red deploys on the west edge, blue on the east
        val crossroads = Hex(0, 0)
        val depot = Hex(-2, 0)
        val village = Hex(2, 0)
        val map = Board.hexagon(4)
            .add(HexTile(0, 0, income = Resources(munitions = 1), vp = Some(2)))
            .add(HexTile(-2, 0, income = Resources(fuel = 1)))
            .add(HexTile(2, 0, income = Resources(manpower = 1)))
            .add(HexTile(-4, 2, income = Resources(fuel = 2)))
        val zones = Map("red" -> Set(Hex(-4, 0), Hex(-4, 1), Hex(-4, 2)), "blue" -> Set(Hex(4, 0), Hex(4, -1), Hex(4, -2)))
        val rules = Rules(vpTarget = 6)

        def opening = GameState.start(map, List("red", "blue"), zones, List(riflemen.make("r1", "red", depot), riflemen.make("b1", "blue", village)), rules).toOption.get

        def ok(s : GameState, as : TacticsAction*) = as.foldLeft(s)((s, a) => s.applyAction(a).fold(e => throw new Exception(a.toString + ": " + e), identity))
        def refused(s : GameState, a : TacticsAction) = s.applyAction(a).swap.toOption.get
        def end(p : String) = EndPhaseAction(p)
        def pass(s : GameState, p : String) = ok(s, end(p), end(p), end(p))

        // Starting
        check("match starts at red's turn 1 Income, red's income in") {
            val s = opening
            s.turn == 1 && s.current == Some("red") && s.phase == IncomePhase &&
            s.player("red").resources == Resources(manpower = 8, fuel = 1) && s.player("blue").resources == Resources(manpower = 6)
        }
        check("starting units hold the sectors they stand on") { opening.control == Map(depot -> "red", village -> "blue") }
        check("bad matches can't start") {
            GameState.start(map, List("red"), zones).isLeft && GameState.start(map, List("red", "red"), zones).isLeft &&
            GameState.start(map, List("red", "green"), zones).isLeft &&
            GameState.start(map, List("red", "blue"), zones + ("red" -> Set(Hex(9, 0)))).isLeft &&
            GameState.start(map, List("red", "blue"), zones, List(riflemen.make("x", "green", depot))).isLeft &&
            GameState.start(map, List("red", "blue"), zones, List(riflemen.make("x", "red", depot), riflemen.make("y", "blue", depot))).isLeft
        }
        check("no match, no actions") { GameState(map).applyAction(end("red")).isLeft }

        // Turn order and phases
        check("only the current player acts") { refused(opening, end("blue")).contains("red's turn") && refused(opening, end("green")).contains("no player") }
        check("phases go Income, Action, End, then the next player's Income") {
            val s = opening
            val a = ok(s, end("red"))
            val e = ok(a, end("red"))
            val b = ok(e, end("red"))
            a.phase == ActionPhase && e.phase == EndPhase && b.phase == IncomePhase && b.current == Some("blue") && b.turn == 1 &&
            pass(b, "blue").turn == 2 && pass(b, "blue").current == Some("red")
        }
        check("each action in its own phase") {
            val s = opening
            refused(s, MoveUnit("red", "r1", Hex(-1, 0))).contains("Action phase") &&
            refused(s, AttackUnit("red", "r1", "b1", List(6, 6, 6))).contains("Action phase") &&
            refused(ok(s, end("red")), DeployUnit("red", "Riflemen", Hex(-4, 0))).contains("Income phase") &&
            refused(ok(s, end("red"), end("red")), MoveUnit("red", "r1", Hex(-1, 0))).contains("Action phase")
        }

        // Deploying
        check("deploying pays the cost and names the unit") {
            val s = ok(opening, DeployUnit("red", "Riflemen", Hex(-4, 0)))
            s.unit("red-1").map(u => u.owner == "red" && u.hex == Hex(-4, 0) && u.weapon == Some(Weapons.rifle)) == Some(true) &&
            s.player("red").resources == Resources(manpower = 5, fuel = 1) &&
            ok(s, DeployUnit("red", "Riflemen", Hex(-4, 1))).unit("red-2").isDefined
        }
        check("deploying on a sector captures it") { ok(opening, DeployUnit("red", "Riflemen", Hex(-4, 2))).control.get(Hex(-4, 2)) == Some("red") }
        check("deploying refused: too dear, outside the zone, taken, unknown type") {
            val s = opening
            refused(s, DeployUnit("red", "Tank", Hex(-4, 0))).contains("costs") &&
            refused(s, DeployUnit("red", "Riflemen", Hex(-3, 0))).contains("deployment zone") &&
            refused(s, DeployUnit("red", "Riflemen", Hex(4, 0))).contains("deployment zone") &&
            refused(ok(s, DeployUnit("red", "Riflemen", Hex(-4, 0))), DeployUnit("red", "Riflemen", Hex(-4, 0))).contains("taken") &&
            refused(s, DeployUnit("red", "Dragon", Hex(-4, 0))).contains("no unit type")
        }
        check("can't spend what you haven't got") {
            val s = ok(opening, DeployUnit("red", "Riflemen", Hex(-4, 0)), DeployUnit("red", "Riflemen", Hex(-4, 1)))
            s.player("red").resources.manpower == 2 && refused(s, DeployUnit("red", "Riflemen", Hex(-4, 2))).contains("costs")
        }

        // Moving, attacking and their limits
        val redAct = ok(opening, end("red"))

        check("moving onto a sector captures it, and the one left stays held") {
            val s = ok(redAct, MoveUnit("red", "r1", crossroads))
            s.control.get(crossroads) == Some("red") && s.control.get(depot) == Some("red")
        }
        check("can't move or attack with another player's unit") {
            refused(redAct, MoveUnit("red", "b1", Hex(3, 0))).contains("isn't red's") && refused(redAct, AttackUnit("red", "b1", "r1", List(6, 6, 6))).contains("isn't red's")
        }
        check("move, move again, then attack") {
            val s = ok(redAct, MoveUnit("red", "r1", Hex(-1, 0)), MoveUnit("red", "r1", crossroads), AttackUnit("red", "r1", "b1", List(6, 1, 1)))
            s.unit("r1").get.attacked && s.lastReport.map(_.hits) == Some(1)
        }
        check("one attack a turn, and no moving after it") {
            val s = ok(redAct, MoveUnit("red", "r1", crossroads), AttackUnit("red", "r1", "b1", List(1, 1, 1)))
            refused(s, AttackUnit("red", "r1", "b1", List(6, 6, 6))).contains("already attacked") &&
            refused(s, MoveUnit("red", "r1", Hex(0, 1))).contains("can't move")
        }
        check("a refused attack doesn't use up the unit's attack") {
            val s = ok(redAct, MoveUnit("red", "r1", crossroads))
            s.applyAction(AttackUnit("red", "r1", "b1", List(6, 6))).isLeft && !s.unit("r1").get.attacked &&
            ok(s, AttackUnit("red", "r1", "b1", List(6, 6, 6))).unit("b1").get.health == 7
        }
        check("lastReport only holds the last action's attack") {
            val s = ok(redAct, MoveUnit("red", "r1", crossroads), AttackUnit("red", "r1", "b1", List(1, 1, 1)))
            s.lastReport.isDefined && ok(s, end("red")).lastReport.isEmpty
        }
        check("an enemy unit ending its move on a sector takes it") {
            val s = GameState.start(map, List("red", "blue"), zones, List(riflemen.make("r1", "red", depot), riflemen.make("b1", "blue", Hex(-1, 0))), rules).toOption.get
            // b1 steps beside the depot, r1 leaves it, b1 moves in
            val t = ok(pass(s, "red"), end("blue"), MoveUnit("blue", "b1", Hex(-2, 1)), end("blue"), end("blue"))
            val u = ok(t, end("red"), MoveUnit("red", "r1", Hex(-3, 0)), end("red"), end("red"))
            val v = ok(u, end("blue"), MoveUnit("blue", "b1", depot))
            s.control.get(depot) == Some("red") && v.control.get(depot) == Some("blue")
        }

        // A full match
        check("full match: capture, income, suppression, VP win") {
            val s0 = opening

            // Red's turn 1: a rifleman deployed, r1 takes the Crossroads and hits b1 once
            val s1 = ok(s0, DeployUnit("red", "Riflemen", Hex(-4, 0)), end("red"), MoveUnit("red", "r1", crossroads), AttackUnit("red", "r1", "b1", List(6, 1, 1)))
            val hit = s1.lastReport.get
            val s2 = ok(s1, end("red"), end("red"))

            // Blue's turn 1: base income and the village; b1 is still suppressed, so it needs a 5
            val blueIncome = s2.player("blue").resources == Resources(manpower = 9) && s2.unit("b1").get.suppressed
            val s3 = ok(s2, end("blue"), AttackUnit("blue", "b1", "r1", List(5, 4, 1)))
            val back = s3.lastReport.get
            val s4 = ok(s3, end("blue"), end("blue"))

            // Red's turn 2: the Crossroads scores 2 VP and a munition; b1 recovered at the end of blue's turn
            val turn2 = s4.turn == 2 && s4.current == Some("red") && s4.player("red").vp == 2 &&
                s4.player("red").resources == Resources(manpower = 7, munitions = 1, fuel = 2) && !s4.unit("b1").get.suppressed &&
                !s4.unit("r1").get.attacked && s4.unit("r1").get.spent == 0

            // Turns 2 and 3 go by; red scores at the start of turns 3 and 4 and wins
            val s5 = pass(pass(s4, "red"), "blue")
            val s6 = pass(pass(s5, "red"), "blue")

            hit.damage == 1 && hit.suppressed && blueIncome && back.needed == 5 && back.hits == 1 && turn2 &&
            s5.player("red").vp == 4 && s5.winner.isEmpty &&
            s6.turn == 4 && s6.phase == IncomePhase && s6.player("red").vp == 6 && s6.winner == Some("red") && s6.over &&
            refused(s6, end("red")).contains("game is over") && refused(s6, end("blue")).contains("game is over")
        }
        check("destroying a unit leaves its sector held") {
            val s = GameState.start(map, List("red", "blue"), zones, List(riflemen.make("r1", "red", Hex(1, 0)), riflemen.make("b1", "blue", village).copy(health = 1)), rules).toOption.get
            val t = ok(s, end("red"), AttackUnit("red", "r1", "b1", List(6, 1, 1)))
            t.unit("b1").isEmpty && t.control.get(village) == Some("blue")
        }
        check("the same actions give the same match") {
            val actions = List(DeployUnit("red", "Riflemen", Hex(-4, 0)), end("red"), MoveUnit("red", "r1", crossroads),
                AttackUnit("red", "r1", "b1", List(6, 4, 2)), end("red"), end("red"), end("blue"), MoveUnit("blue", "b1", Hex(1, 1)))
            Turns.applyAll(opening, actions) == Turns.applyAll(opening, actions) && Turns.applyAll(opening, actions).isRight
        }
        check("applyAll stops at the first refused action") {
            Turns.applyAll(opening, List(end("red"), end("blue"), end("red"))).swap.toOption.get.contains("red's turn")
        }
    }
    // The starter skirmish map (scenario.scala)
    def skirmish() : Unit = {
        import Skirmish.{rows, columns, turned, axial, column, name, middle, sectors, westZone, eastZone}

        val b = Skirmish.board

        check("skirmish: 9 rows of 13 and 12 hexes") {
            b.size == 5 * 13 + 4 * 12 && 0.until(rows).forall(r => b.all.count(_.r == r) == columns(r))
        }
        check("skirmish: turning half round gives the same map") {
            b.all.forall(t => b.get(turned(t.hex)).exists(u => u.terrain == t.terrain && u.cover == t.cover && u.income == t.income && u.vp == t.vp))
        }
        check("skirmish: hex names") {
            name(axial(0, 0)) == "A1" && name(axial(12, 8)) == "M9" && name(middle) == "G5" && column(axial(7, 3)) == 7
        }
        check("skirmish: 9 sectors, the town in the middle") {
            b.all.count(_.sector) == 9 && b.get(middle).exists(t => t.terrain == Building && t.vp == Some(2)) && sectors.map(_._1).distinct.size == 9 &&
            sectors.forall(s => b.get(s._1).exists(_.sector))
        }
        check("skirmish: zones mirror each other, on ground infantry can stand on, with no sector") {
            westZone.size == eastZone.size && westZone.size >= 15 && westZone.intersect(eastZone).isEmpty &&
            (westZone ++ eastZone).forall(h => b.get(h).exists(t => t.terrain.passable(Infantry) && !t.sector))
        }
        check("skirmish: every sector reachable by infantry from both zones") {
            val walker = Roster.riflemen.make("w", "x", westZone.head)
            sectors.forall { case (h, _) =>
                List(westZone, eastZone).forall(z => z.exists(from => Movement.path(GameState(b), walker.at(from), h).isDefined))
            }
        }
        check("skirmish: a match starts, Red first with its income in") {
            val s = Skirmish.start("Red", "Blue").toOption.get
            s.current == Some("Red") && s.phase == IncomePhase && s.units.size == 2 &&
            s.player("Red").resources == Resources(manpower = 8, munitions = 3) && s.player("Blue").resources == Resources(manpower = 6, munitions = 2) &&
            s.unit("Red-A").exists(u => westZone.contains(u.hex)) && s.unit("Blue-A").exists(u => eastZone.contains(u.hex))
        }
        check("skirmish: Red deploys, moves toward the town, and Blue gets its turn") {
            val s = Skirmish.start("Red", "Blue").toOption.get
            val zone = westZone.toList.filter(h => !s.occupied(h)).sortBy(h => (h.r, h.q))
            val t = Turns.applyAll(s, List(DeployUnit("Red", "Riflemen", zone.head), EndPhaseAction("Red"))).toOption.get
            val step = t.reachable("Red-A").keys.minBy(h => (h.distance(middle), h.r, h.q))
            val u = Turns.applyAll(t, List(MoveUnit("Red", "Red-A", step), EndPhaseAction("Red"), EndPhaseAction("Red"))).toOption.get
            u.current == Some("Blue") && u.unit("Red-1").isDefined && step.distance(middle) < axial(1, 4).distance(middle) &&
            u.player("Blue").resources == Resources(manpower = 8, munitions = 3)
        }
    }
}
