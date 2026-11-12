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


// PC version (option ModuleOption(PCVersion), under Fan content; asked for by Gman on 2026-11-11): a mode inspired by the Northgard
// video game. RULES.md (PC version) has the rules and the readings; the design proposal with Gman's questions is the Claude Doc
// "Northgard PC version mode: design proposal".
// - SETUP: the map is built at once (buildMap): the central tile (always a special one), a Beach with a Port for each clan at the
//   edge (homes), the two tiles beside each Port, then every other space, each tile in a legal turn, from the map tiles with the
//   Uncharted Horizons ones and every Wilderness and Wastelands Environment tile (Gman, 2026-11-11), which the mode turns on. A side with no tile beyond it is
//   coast and closes its territory (Board.coast). Each clan's home is its Port territory and the two small territories beside it
//   (the Beach's wings, each joined to the tile beside the Port); it places its starting units in the Port territory, then in one of
//   the two, and 1 unit goes into any of the three still empty. Creatures hold the territories between the homes.
// - The TOWN HALL in each Port territory: a large building on no space, never built, moved or destroyed (game.halls). A Build can
//   upgrade it for 1 Stone (Minerals) and 1 wood (HallUpgradeAction); then the clan can scout enemy territories.
// - FOG OF WAR: a clan sees the territories it (or a teammate) has had figures in or has scouted (game.scouted, sweep); units only move
//   into territories it sees. Explore scouts one bordering territory for each Explore (ScoutAction); with a Defense Tower in its Port
//   territory, any coastal one too.
// - CREATURES: only Wolves and Draugr move, Myrkalfar from year 3; the others act where they are. A Wolf killed comes back to where it
//   was placed at the next Creature phase, unless a clan holds that territory (game.wolfHomes).
// - A clan that ends a year (after Winter) without its Town Hall territory is out (game.out): its figures and buildings leave the map,
//   its cards, resources and fame are gone, and it passes every turn. The last clan or team left wins at once.

// SETUP
// The map tiles with every Environment tile (Wilderness, Wastelands), shuffled
case class PCTilesAction(shuffled : $[String]) extends ShuffledAction[String]
// A starting placement: in the Port territory (round 1), in one of the two beside it (round 2)
case class PCPlaceAction(round : Int, l : $[Faction]) extends ForcedAction

// TOWN HALL
case class HallUpgradeAction(self : Faction, area : AreaRef, times : Int, e : BuildEffect, smallOnly : Boolean, then : ForcedAction) extends BaseAction("Build")("Upgrade your", TownHall.elem, "for", Amount(1.hl, Stone.elem), "and", Amount(1.hl, Wood.elem)) with MapTarget { def target = area }

// SCOUTING
case class ScoutAction(self : Faction, area : AreaRef, times : Int, e : ExploreEffect, then : ForcedAction) extends BaseAction("Scout", (times > 1).?(("(" + times + " left)").txt))(area) with MapTarget { def target = area }
case class ScoutDoneAction(self : Faction, then : ForcedAction) extends BaseAction(None)("Stop scouting")
// Vedrfolnir at the end of the year: the territory drawn at random among those its controller hasn't seen (PCVedrfolnirReveal)
case class VedrfolnirRevealAction(self : Faction, random : AreaRef) extends RandomAction[AreaRef]

// THE END OF THE YEAR: clans without their Town Hall are out
case class HallLostAction(l : $[Faction]) extends ForcedAction
case class LastStandingAction(winners : $[Faction]) extends ForcedAction


// The Town Hall: drawn and named like a building, but not one of the buildings (game.halls)
object TownHall extends Elementary {
    val image = "building-town-hall"
    val upgraded = "building-town-hall-upgraded"
    def elem : Elem = "Town Hall".hl
}

// The Town Hall's tap: what it does
case class TownHallInfo(f : Faction)


object PCExpansion extends Expansion {
    def on(implicit game : Game) = game.has(PCVersion)

    // MAP LAYOUT: the spaces of the map and each Port (its spot and the side its sea is on), by the number of clans, clockwise
    // (wide: the two-player map with its three middle rows five tiles wide, an oval; Gman, 2026-11-11; PCVersionUpdate)
    def layout(n : Int, wide : Boolean = false) : ($[(Int, Int)], $[((Int, Int), Side)]) = n match {
        case 2 if wide =>
            (for (y <- -2.to(2).$; x <- -2.to(2).$; if y.abs < 2 || x.abs < 2) yield (x, y), $(((0, -2), North), ((0, 2), South)))
        case 2 =>
            (for (y <- -2.to(2).$; x <- -1.to(1).$) yield (x, y), $(((0, -2), North), ((0, 2), South)))
        case 3 =>
            (for (y <- -2.to(2).$; x <- -2.to(2).$) yield (x, y), $(((0, -2), North), ((2, 1), East), ((-2, 1), West)))
        case 4 =>
            (for (y <- -3.to(3).$; x <- -3.to(3).$; if x.abs + y.abs <= 4) yield (x, y), $(((0, -3), North), ((3, 0), East), ((0, 3), South), ((-3, 0), West)))
        case _ =>
            val cells = for (y <- -2.to(2).$; x <- -4.to(4).$; if (x.abs == 4 && y.abs == 2).not) yield (x, y)
            val south = (n >= 6).?($(((2, 2), South), ((-2, 2), South))).|($(((0, 2), South)))
            (cells, $(((-2, -2), North), ((2, -2), North), ((4, 0), East)) ++ south :+ ((-4, 0), West))
    }

    // Two territories at least between neighbours' homes, three with two clans
    def gap(n : Int) = (n == 2).?(3).|(2)

    // The turn of a Beach whose sea lies in direction d from its Port (as in the Sea module)
    def rotation(d : Side) = SeaExpansion.rotation(d)

    // The perpendicular directions of a Port whose sea is in direction d: towards its west wing, towards its east wing
    def wings(d : Side) : $[Side] = $(West.rotate(rotation(d)), East.rotate(rotation(d)))

    // A small deterministic random sequence from the shuffled pile, so every client builds the same map
    class Seq(seed : Long) {
        private var s = seed
        def next(n : Int) : Int = {
            s = (s * 6364136223846793005L + 1442695040888963407L)
            (((s >>> 33) % n + n) % n).toInt
        }
    }

    // The homes: each clan's Port area and the two areas beside it (the wings' land joined to the tiles beside the Port)
    case class Home(port : AreaRef, sides : $[AreaRef])

    // Build the whole map; the homes, in the layout's Port order
    def buildMap(tiles : $[String])(implicit game : Game) : $[Home] = {
        val (cells, ports) = layout(game.arity, game.options.has(PCVersionUpdate))
        val board = game.board

        var best : |[($[Placement], $[String], $[Home])] = None

        // Tries with other turns and other tile orders until the homes are apart and none joins another (the last good try is kept;
        // after every 10 tries one territory less between the homes will do)
        var attempt = 0
        var done = false

        while (done.not && (attempt < 60 || best.none && attempt < 120)) {
            val seq = new Seq(tiles.mkString(",").hashCode.toLong * 31 + attempt)
            val rest = tiles.drop(attempt % tiles.num.max(1)) ++ tiles.take(attempt % tiles.num.max(1))
            val result = attemptMap(cells, ports, rest, seq, (attempt / 20).min(3))

            if (result.any) {
                val (placements, left, homes) = result.get
                best = result
                done = apart(placements, homes, gap(game.arity) - attempt / 10)
            }

            attempt += 1
        }

        val (placements, left, homes) = best.get
        placements.foreach(board.place)
        board.coast = true
        game.pile = left
        homes
    }

    // One try: the central tile, the Beaches, the tiles beside the Ports, then the rest in rings from the centre
    // The map being built: its placements, and the territories so far as a union-find of areas that a try can undo, so each tile and
    // turn is checked in a moment (Board.tryPlace computes every territory again, much too slow for the hundreds of tries in a browser)
    class Grow {
        val placed = scala.collection.mutable.ArrayBuffer[Placement]()
        private val byXY = scala.collection.mutable.HashMap[(Int, Int), Placement]()
        private val parent = scala.collection.mutable.HashMap[AreaRef, AreaRef]()
        private val borders = scala.collection.mutable.ArrayBuffer[(AreaRef, AreaRef)]()

        def has(x : Int, y : Int) = byXY.contains((x, y))

        // What each placement joined, so the last ones can be taken back
        private val undos = scala.collection.mutable.ArrayBuffer[($[AreaRef], $[AreaRef], Int)]()

        def removeLast() {
            val p = placed.remove(placed.size - 1)
            byXY.remove((p.x, p.y))
            val (undo, areas, n) = undos.remove(undos.size - 1)
            borders.remove(borders.size - n, n)
            undo.reverse.foreach(k => parent(k) = k)
            areas.foreach(parent.remove)
        }

        // Places all of l, or none
        def tryAddAll(l : $[Placement]) : Boolean = {
            val k = l.takeWhile(tryAdd).num
            if (k < l.num)
                0.until(k).foreach(_ => removeLast())
            k == l.num
        }

        private def find(a : AreaRef) : AreaRef = {
            var x = a
            while (parent(x) != x)
                x = parent(x)
            x
        }

        // Places p when every border on the map still separates two territories (as Board.consistent)
        def tryAdd(p : Placement) : Boolean = {
            val undo = scala.collection.mutable.ArrayBuffer[AreaRef]()
            val areas = p.spec.areas./(a => AreaRef(p.x, p.y, a.id))
            areas.foreach(a => parent(a) = a)

            Side.all.foreach { s =>
                byXY.get((p.x + s.dx, p.y + s.dy)).foreach { q =>
                    (p.spec.areaOn(s, p.r), q.spec.areaOn(s.opposite, q.r)) match {
                        case (Some(x), Some(y)) =>
                            val ra = find(AreaRef(p.x, p.y, x.id))
                            val rb = find(AreaRef(q.x, q.y, y.id))
                            if (ra != rb) {
                                parent(ra) = rb
                                undo += ra
                            }
                        case _ =>
                    }
                }
            }

            val mine = p.spec.borders./(b => (AreaRef(p.x, p.y, b.a), AreaRef(p.x, p.y, b.b)))
            val ok = mine.forall { case (a, b) => find(a) != find(b) } && borders.forall { case (a, b) => find(a) != find(b) }

            if (ok) {
                placed += p
                byXY((p.x, p.y)) = p
                borders ++= mine
                undos += ((undo.toList, areas, mine.num))
            }
            else {
                undo.foreach(k => parent(k) = k)
                areas.foreach(parent.remove)
            }

            ok
        }
    }

    // One try: the central tile, the Beaches, the tiles beside the Ports, then the rest in rings from the centre
    def attemptMap(cells : $[(Int, Int)], ports : $[((Int, Int), Side)], tiles : $[String], seq : Seq, level : Int)(implicit game : Game) : |[($[Placement], $[String], $[Home])] = {
        val g = new Grow
        var pile = tiles

        g.tryAdd(Placement(game.central, 0, 0, 0))

        ports.foreach { case ((px, py), d) =>
            val r = rotation(d)
            val (sx, sy) = (px + d.dx, py + d.dy)
            val (w, e) = (West.rotate(r), East.rotate(r))
            $("beach-port" -> (px, py), "beach-sea" -> (sx, sy), "beach-wing-w" -> (sx + w.dx, sy + w.dy), "beach-wing-e" -> (sx + e.dx, sy + e.dy)).foreach { case (tile, (x, y)) =>
                g.tryAdd(Placement(tile, x, y, r))
            }
        }

        // The tile beside a Port at (x, y), its Port on side q, the wing on side d: those two sides in different areas, and the wing's
        // side the only one of its area, with no lair, so the wing's territory is that area and the wing
        def besideFits(p : Placement, d : Side, q : Side) = {
            val o = p.spec.areaOn(d, p.r)
            val i = p.spec.areaOn(q, p.r)
            o.any && i.any && o != i && o.get.edges.num == 1 && o.get.lair.not
        }

        // Mirrored maps (Gman, 2026-11-11: "When possible, it would be good to mirror starting positions"): where the layout turns onto
        // itself (two clans, four and six), each tile placed is also placed, turned the same way, at the cells the map's turns take it
        // to, so every home sees the same map around it (PCVersionUpdate; the first levels of retries only, then it may go)
        val turnsOf : $[Int] = (game.options.has(PCVersionUpdate) && level < 2).??(game.arity match {
            case 2 | 6 => $(2)
            case 4 => $(1, 2, 3)
            case _ => $[Int]()
        })
        def images(p : Placement) = turnsOf./{ k =>
            val (x, y) = 0.until(k).foldLeft((p.x, p.y)) { case ((x, y), _) => (-y, x) }
            Placement(p.tile, x, y, (p.r + k) % 4)
        }
        def put(p : Placement) = (p +: images(p)).distinctBy(q => (q.x, q.y)).use(l => l.num == 1 + turnsOf.num && l.drop(1).forall(q => cells.has((q.x, q.y)) && g.has(q.x, q.y).not) && g.tryAddAll(l))

        // A tile for (x, y) from the pile, or with from a copy of that tile (nothing to do where a mirrored tile is already)
        def fill(x : Int, y : Int, ok : Placement => Boolean, from : |[String] = None) : Boolean = {
            if (turnsOf.any && g.has(x, y))
                return true
            val turns = seq.next(4)
            if (from.any)
                return 0.until(4).exists { k =>
                    val p = Placement(from.get, x, y, (k + turns) % 4)
                    ok(p) && put(p)
                }
            pile.find { t =>
                0.until(4).exists { k =>
                    val p = Placement(t, x, y, (k + turns) % 4)
                    ok(p) && put(p)
                }
            }./(t => pile = pile.diff($(t))).any
        }

        // Balanced homes (Gman, 2026-11-11): the Port territory gives only food, one small territory only 1 wood, the other only 1 lore
        // Each level of retries (20 tries each) asks less, when the tiles can't make it (six clans need twelve such tiles):
        // 0 exactly 1 food, 1 wood and 1 lore; 1 at least that much, the others none; 2 the side's wood or lore, any food;
        // 3 anything
        def only(a : AreaSpec, food : Int, wood : Int, lore : Int) = level match {
            case 0 => a.food == food && a.wood == wood && a.lore == lore
            case 1 => (a.food > 0) == (food > 0) && (a.wood > 0) == (wood > 0) && (a.lore > 0) == (lore > 0)
            case 2 => (a.wood > 0) == (wood > 0) && (a.lore > 0) == (lore > 0)
            case _ => true
        }
        def alone(a : AreaSpec) = level >= 2 || a.edges.num == 1

        // The wood on the same side of every Port (left or right, picked once)
        val woodFirst = seq.next(2) == 0

        val beside = ports./~{ case ((px, py), d) =>
            wings(d).zipWithIndex./{ case (w, i) => ((px + w.dx, py + w.dy), d, w.opposite, (i == 0) == woodFirst) }
        }

        def besideOk(d : Side, q : Side, wood : Boolean)(p : Placement) =
            besideFits(p, d, q) && only(p.spec.areaOn(d, p.r).get, 0, wood.??(1), wood.not.??(1)) && p.spec.areaOn(q, p.r).exists(a => alone(a) && only(a, 0, 0, 0) && a.lair.not)

        // Mines (Gman, 2026-11-11): beside its food area the inland tile has a lair in an area of its own on a side across the Port's
        // line, so with the tile beyond that side giving an area of its own with a small building space, every home has a lair
        // territory with a space for a Mine just outside it
        // (in games with the hidden PCVersionUpdate every new game gets; older games keep the maps they were dealt)
        val mines = game.options.has(PCVersionUpdate) && level < 3

        def mineSide(d : Side, x : Int, y : Int)(p : Placement) : |[Side] = mines.??(
            Side.all.%(s => s != d && s != d.opposite).find(s => cells.has((x + s.dx, y + s.dy)) && g.has(x + s.dx, y + s.dy).not &&
                p.spec.areaOn(s, p.r).exists(a => a.edges.num == 1 && a.lair && a.wood == 0 && a.lore == 0 && a.food == 0)))

        def inlandOk(d : Side, x : Int, y : Int)(p : Placement) =
            p.spec.areaOn(d, p.r).exists(a => alone(a) && only(a, (level < 2).??(1), 0, 0) && a.lair.not) && (mines.not || mineSide(d, x, y)(p).any)

        // The tile beyond the lair: its side towards it in an area of its own with a small building space
        def mineOk(s : Side)(p : Placement) =
            p.spec.areaOn(s.opposite, p.r).exists(a => a.edges.num == 1 && a.lair.not && a.spaces.exists(_.kind == SmallSpace))

        // Every clan's home gets the same three tiles (copies, turned to fit), so the homes are alike: a tile for the wood side, one
        // for the lore side and one inland of the Port, each picked once among the tiles that fit; with none, tiles from the pile
        def pick(ok : Placement => Boolean, but : $[String]) : |[String] = {
            val l = tiles.distinct.diff(but).%(t => 0.until(4).exists(r => ok(Placement(t, 0, 0, r))))
            l.any.?(l(seq.next(l.num)))
        }

        val ((px0, py0), d0) = ports.head
        val sideOf = beside.take(2)./{ case (_, d, q, wood) => wood -> q }.toMap
        val woodTile = (level < 3).??(pick(besideOk(d0, sideOf(true), true), $))
        val loreTile = (level < 3).??(pick(besideOk(d0, sideOf(false), false), woodTile.$))
        val foodTile = (level < 3).??(pick(inlandOk(d0, px0 - d0.dx, py0 - d0.dy), woodTile.$ ++ loreTile.$))
        val mineTile = mines.??(pick(mineOk(North), woodTile.$ ++ loreTile.$ ++ foodTile.$))
        val copies = woodTile.any && loreTile.any && foodTile.any && (mines.not || mineTile.any)

        if (copies)
            pile = pile.diff(woodTile.$ ++ loreTile.$ ++ foodTile.$ ++ mineTile.$)

        val placedBeside = beside.forall { case ((x, y), d, q, wood) =>
            fill(x, y, besideOk(d, q, wood), copies.??(wood.?(woodTile).|(loreTile)))
        }

        if (placedBeside.not)
            return None

        // The tile inland of each Port: its side towards the Port in an area of its own giving 1 food; with the beside tiles' areas
        // by the Port giving nothing, the Port territory is the Port and those three areas, and gives 1 food
        // (mirrored, a home's tiles are already there where another home's were placed)
        val inland = ports.forall { case ((px, py), d) =>
            val (x, y) = (px - d.dx, py - d.dy)
            (turnsOf.any && g.has(x, y)) ||
            fill(x, y, inlandOk(d, x, y), copies.??(foodTile)) && (mines.not || {
                val s = mineSide(d, x, y)(g.placed.find(p => p.x == x && p.y == y).get).get
                fill(x + s.dx, y + s.dy, mineOk(s), copies.??(mineTile))
            })
        }

        if (inland.not)
            return None

        cells.filter(c => g.has(c._1, c._2).not).map(c => (c, seq.next(1000))).sortBy { case ((x, y), k) => (x.abs + y.abs, k) }.foreach { case ((x, y), _) =>
            fill(x, y, _ => true)
        }

        val b = new Board
        g.placed.foreach(b.place)

        val homes = ports./{ case ((px, py), d) =>
            val (sx, sy) = (px + d.dx, py + d.dy)
            Home(AreaRef(px, py, "p"), wings(d)./(w => AreaRef(sx + w.dx, sy + w.dy, "n")))
        }

        // Every home territory its own, none of them the central one or another's
        val all = homes./~(h => h.port +: h.sides)./(b.territory)
        if (all.distinct.num != all.num || all.exists(_.areas.exists(a => a.x == 0 && a.y == 0)))
            return None

        // The Port territory gives food and nothing else
        def gives(t : Territory) = t.areas./(b.spec)
        if (level < 3 && homes.exists(h => gives(b.territory(h.port)).use(l => l./(_.food).sum == 0 || l./(_.wood).sum > 0 || l./(_.lore).sum > 0)))
            return None

        |((b.placements, pile, homes))
    }

    // Whether each home is at least n territories from every other (counting the territories in between)
    def apart(placements : $[Placement], homes : $[Home], n : Int) : Boolean = {
        val b = new Board
        placements.foreach(b.place)
        val sets = homes./(h => (h.port +: h.sides)./(b.territory))

        sets.indices.forall { i =>
            // Breadth first from home i: the territories between it and the nearest other home
            var seen = sets(i)
            var front = sets(i)
            var depth = 0
            var hit = false
            while (front.any && hit.not && depth <= n) {
                val next = front./~(t => b.adjacent(t).map(_._1)).distinct.diff(seen)
                if (next.exists(t => sets.indices.exists(j => j != i && sets(j).has(t))))
                    hit = true
                else {
                    seen ++= next
                    front = next
                    depth += 1
                }
            }
            hit.not || depth >= n
        }
    }

    // The territories between the homes: on a shortest way from one home to another, not in a home
    def between(homes : $[Home])(implicit game : Game) : $[Territory] = {
        val sets = homes./(h => (h.port +: h.sides)./(game.board.territory))
        val home = sets.flatten

        def distances(from : $[Territory]) : Map[Territory, Int] = {
            var d = from./(_ -> 0).toMap
            var front = from
            var k = 0
            while (front.any) {
                k += 1
                val next = front./~(t => game.board.adjacent(t).map(_._1)).distinct.%(t => d.contains(t).not)
                next.foreach(t => d += t -> k)
                front = next
            }
            d
        }

        val ds = sets./(distances)

        game.board.territories.diff(home).%{ t =>
            sets.indices.exists(i => sets.indices.exists(j => i < j && {
                val dij = sets(j)./(x => ds(i).getOrElse(x, 999)).min
                ds(i).getOrElse(t, 999) + ds(j).getOrElse(t, 999) <= dij
            }))
        }
    }

    // FOG OF WAR: what f (and its teammates) sees
    def sees(f : Faction, t : Territory)(implicit game : Game) : Boolean =
        on.not || (f +: game.mates(f)).exists(g => game.present(t).has(g) || game.scouted.getOrElse(g, $[AreaRef]()).exists(t.areas.contains))

    // What any of these viewers sees (none: everything, for spectators and once the game is over)
    def visible(viewers : $[Faction], t : Territory)(implicit game : Game) : Boolean =
        on.not || game.isOver || viewers.none || viewers.exists(sees(_, t))

    // Every territory a clan has had figures in stays seen; run by Game.loggedPerform around each action
    def sweep(implicit game : Game) {
        game.factions.foreach { f =>
            val l = game.board.territories.%(t => game.present(t).has(f))./(_.anchor)
            val old = game.scouted.getOrElse(f, $[AreaRef]())
            val add = l.%(a => old.has(a).not)
            if (add.any)
                game.scouted += f -> (old ++ add)
        }
    }

    def scout(f : Faction, t : Territory)(implicit game : Game) {
        game.scouted += f -> (game.scouted.getOrElse(f, $[AreaRef]()) :+ t.anchor)
    }

    // TOWN HALL
    def hallOf(f : Faction)(implicit game : Game) : |[Territory] = game.halls.get(f)./(game.board.territory)

    def holdsHall(f : Faction)(implicit game : Game) = hallOf(f).exists(game.controlled(f).has)

    def upgraded(f : Faction)(implicit game : Game) = game.hallsUpgraded.has(f)

    // The Town Hall's upgrade, offered with the builds: f holds its Town Hall, has 1 Stone and 1 wood
    def hallUpgrade(f : Faction, e : BuildEffect, times : Int, smallOnly : Boolean, then : ForcedAction)(implicit game : Game) : $[UserAction] =
        (on && times > 0 && upgraded(f).not && holdsHall(f) && f.mineral(Stone) >= 1 && f.wood >= 1).$(HallUpgradeAction(f, game.halls(f), times, e, smallOnly, then))

    def canUpgrade(f : Faction)(implicit game : Game) = on && upgraded(f).not && holdsHall(f) && f.mineral(Stone) >= 1 && f.wood >= 1

    // SCOUTING: the territories f may scout, not seen yet: those bordering its own, and with its watchtower (a Defense Tower in its
    // Port territory) the coastal ones; an enemy's needs the upgraded Town Hall
    def coastal(t : Territory)(implicit game : Game) : Boolean =
        game.ports.exists(t.areas.contains) || t.areas.exists(a => game.board.sides(a).exists(s => game.board.at(a.x + s.dx, a.y + s.dy).none))

    def scoutable(f : Faction)(implicit game : Game) : $[Territory] = {
        val mine = game.controlled(f)
        val near = mine./~(t => game.board.adjacent(t).map(_._1))
        val watch = hallOf(f).exists(h => mine.has(h) && game.working(h).has(DefenseTower))
        val coast = watch.??(game.board.territories.%(coastal))
        (near ++ coast).distinct.%(t => sees(f, t).not).%(t => upgraded(f) || game.present(t).exists(game.enemy(f, _)).not)
    }

    // CREATURES: which ones move in the Creature phase
    def moves(c : Creature)(implicit game : Game) : Boolean = c.kind match {
        case CreatureWolf | Draugr => true
        case Myrkalf => game.year >= 3
        case _ => false
    }

    // With 3 clans or more the Wilderness and Wastelands creatures are in the deck too, so there are creatures for more territories
    // between the homes (Gman, 2026-11-11; PCVersionUpdate)
    def moreCreatures(implicit game : Game) = on && game.options.has(PCVersionUpdate) && game.arity >= 3

    // The clans still in the game
    // The last clan (or team) holding its Town Hall wins
    def dominationWins(implicit game : Game) : Boolean = game.options.has(PCDominationChoice).not || game.options.has(DominationVictory) || game.options.has(DominationOnly)

    def standing(implicit game : Game) : $[Faction] = game.factions.%(f => game.out.has(f).not)

    def perform(action : Action, soft : Void)(implicit game : Game) : Continue = action @@ {
        case _ if on.not => UnknownContinue

        // SETUP: once the central tile is picked and the creatures shuffled, the whole map, then the starting units
        case ShuffledCreaturesRestAction(top, rest, tiles) =>
            game.creatureDeck = top ++ rest

            log("The creature cards were shuffled")

            Shuffle[String](tiles ++ Wild.tiles ++ Waste.tiles, PCTilesAction(_))

        case PCTilesAction(l) =>
            val homes : $[Home] = buildMap(l)

            log("The map was built")

            val order = game.from(game.first)

            order.zip(homes).foreach { case (f, h) =>
                game.halls += f -> h.port
                game.homes += f -> (h.port +: h.sides)
                game.scouted += f -> (h.port +: h.sides)./(game.board.territory)./(_.anchor)
                game.ports :+= h.port
                game.raids += h.port -> Raid(None, None, 0, 0)

                f.log("starts on the Port in", h.port, "with a", TownHall)
            }

            // Creatures on the territories between the homes, the ones with a lair first, nearest the centre first
            val lands = between(homes).sortBy(t => (t.areas.exists(a => game.board.spec(a).lair).not, t.areas./(a => a.x.abs + a.y.abs).min))
                .%(t => game.creaturesIn(t).none)

            lands.foreach { t =>
                if (game.creatureDeck.any) {
                    val c = game.creatureDeck.head
                    game.creatureDeck = game.creatureDeck.drop(1)
                    val a = t.areas.find(a => game.board.spec(a).lair).|(t.anchor)
                    game.creatureLine :+= c
                    game.creatureAt += c -> a
                    if (c.kind == CreatureWolf)
                        game.wolfHomes += c -> a

                    log(c, "holds", t.anchor)
                }
            }

            Then(PCPlaceAction(1, order))

        // Every clan: 4 units in its Port territory, 1 in each small territory beside it (Gman, 2026-11-11); Kaija, a lynx, Brok or
        // the Automa's second Leader in place of one of the Port's units, and Robotos 2 more there
        case PCPlaceAction(_, l) =>
            l.foreach { f =>
                val port :: sides = game.homes(f)
                val companion = game.kaijaReady(f)
                game.addUnits(port, f, companion.?(3).|(4) + game.robotos(f).??(2))
                if (companion) {
                    game.setCompanion(f, |(port))
                    game.note("kaija-setup")
                }
                f.log("placed", companion.?(("three units and " ~ OneCompanion(f).elem0)).|(("four units".txt)), "in", port)
                sides.foreach { a =>
                    game.addUnits(a, f, 1)
                    f.log("placed a unit in", a)
                }
            }

            Then(StartYearAction)

        // The setup placements go on with the next clan
        case SetupPlaceAction(round, l) =>
            Then(PCPlaceAction(round, l))

        // TOWN HALL
        case HallUpgradeAction(f, a, times, e, smallOnly, then) =>
            MineralsExpansion.spend(f, $(Stone))
            f.wood -= 1
            game.hallsUpgraded :+= f

            f.log("upgraded their", TownHall, "for", Amount(1.hl, Stone.elem), "and", Amount(1.hl, Wood.elem))

            game.note("hall-upgrade")

            Then(BuildAction(f, e, times - 1, smallOnly, then))

        // SCOUTING: each Explore scouts one territory
        case ExploreAction(f, draw, times, redraw, e, then) =>
            val l = (times > 0).??(scoutable(f))

            if (l.none)
                Then(then)
            else
                Ask(f).each(l)(t => ScoutAction(f, t.anchor, times, e, then)).add(ScoutDoneAction(f, then))

        case ScoutAction(f, a, times, e, then) =>
            val t = game.board.territory(a)
            scout(f, t)

            f.log("scouted", a)

            game.note("scout")

            // Gman, 2026-11-11: "You still get fame and all the other bonuses when exploring." The scouted territory counts as
            // the one the Explore closed, whoever holds it (it is never held yet), and nothing else on the map closes
            val tile = game.board.placements.find(p => p.x == a.x && p.y == a.y).get.tile

            // Lay of the Land: everything the territory shows
            if (e.collect)
                MapExpansion.collect(f, t, "from the scouted territory".txt)

            // The fame for closing it, 1 per tile
            val n = game.board.tiles(t)
            f.fame += n
            f.log("gained", n.hl, FameIcon(), "for scouting", a)

            if (f == Stag) {
                f.fame += 1
                f.log("gained", 1.hl, "more", FameIcon(), "for closing a territory")
                ClanPowers.note(PowerNote(f, |(a), f.elem ~ " gains 1 fame for each territory it closes.", fame = 1))
            }

            if (f == Raven) {
                val (food, wood, lore) = MapExpansion.collect(f, t, "from the scouted territory".txt)
                if (food + wood + lore > 0)
                    ClanPowers.note(PowerNote(f, |(a), f.elem ~ " collects the resources of the territories it closes by exploring.", food, wood, lore))
            }

            // Boar Clan: the scout closes nothing on the map, which is already whole
            if (f == Boar) {
                f.lore += 1
                f.log("collected", 1.hl, Lore, "for exploring without closing a territory")
                ClanPowers.note(PowerNote(f, None, f.elem ~ " collects 1 lore for exploring without closing a territory.", lore = 1))
            }

            // Events: New Horizons; Alternative victory: territories closed
            EventsExpansion.explored(f, $(t), $(t))

            // New Blood: Ox Clan's token on the tile, Rat Clan's unit (only where no creature or other clan is)
            if (game.has(NewBlood))
                NewBloodExpansion.explored(f, tile, Spot(a.x, a.y), (game.present(t).none && game.creaturesIn(t).none).??($(t)))

            val next = ClanPowers.report(ExploreAction(f, 1, times - 1, false, e, then))
            val horse = (f == Horse).?(HorseClosedAction(f, $(a), next) : ForcedAction).|(next)
            val ox = (f == Ox && game.options.has(OxPicksSpace) && game.gearPile.any).?(GearPlaceAskAction(f, tile, Spot(a.x, a.y), horse) : ForcedAction).|(horse)

            Then(ox)

        case ScoutDoneAction(f, then) =>
            Then(then)

        // A clan out of the game doesn't come back with three units in a neutral territory, as a clan with none left on the map
        // does (4-player bot games, 2026-11-12)
        case ReturnUnitsAction(f, then) if game.out.has(f) =>
            Then(then)

        // CREATURES: only Wolves and Draugr move (Myrkalfar from year 3); the others act where they are
        case CreatureActivateAction(c :: rest) if game.creatureLine.has(c) && moves(c).not =>
            Then(CreatureEffectAction(c, CreatureActivateAction(rest)))

        // At the start of the Creature phase, killed Wolves come back where they were placed, unless a clan holds it
        case CreaturePhaseAction if game.wolvesBack < game.year =>
            game.wolvesBack = game.year

            game.wolfHomes.foreach { case (c, a) =>
                val t = game.board.territory(a)
                if (game.creatureLine.has(c).not && game.present(t).none && game.creaturesIn(t).none) {
                    game.creatureDeck = game.creatureDeck.but(c)
                    game.creatureDiscard = game.creatureDiscard.but(c)
                    game.creatureLine :+= c
                    game.creatureAt += c -> a
                    game.note("wolf-back")

                    log(c, "came back to", t.anchor)
                }
            }

            game.internalPerform(CreaturePhaseAction, soft)

        // OUT OF THE GAME: a clan that is out passes every turn
        case TurnAction(f, _) if game.out.has(f) =>
            f.passed = true
            Then(NextTurnAction(f))

        // VEDRFOLNIR: the map is whole, so instead of adding a tile its controller reveals one territory it hasn't seen, drawn at
        // random, enemy ones too, at the end of each year; not a scout, so no fame or Explore bonuses (Gman, 2026-11-12;
        // PCVedrfolnirReveal); before the Town Hall check, which may end the game
        case EndOfYearAction if game.options.has(PCVedrfolnirReveal) && game.wasteSteps.has("vedrfolnir-reveal-" + game.year).not =>
            game.wasteSteps :+= "vedrfolnir-reveal-" + game.year

            val l = Waste.controller(Waste.vedrfolnir, "s").%(f => game.out.has(f).not)./~(f => game.board.territories.%(t => sees(f, t).not)./(_.anchor))

            Waste.controller(Waste.vedrfolnir, "s") match {
                case Some(f) if l.any => Random[AreaRef](l, VedrfolnirRevealAction(f, _))
                case _ => game.internalPerform(EndOfYearAction, soft)
            }

        case VedrfolnirRevealAction(f, a) =>
            scout(f, game.board.territory(a))
            game.note("vedrfolnir-reveal")

            f.log("revealed", a, "with", Waste.elem(Waste.vedrfolnir))

            Then(EndOfYearAction)

        // THE END OF THE YEAR: a clan without its Town Hall is out
        case EndOfYearAction if game.hallsChecked < game.year =>
            game.hallsChecked = game.year

            val lost = standing.%(f => holdsHall(f).not)

            // Nobody would be left: nobody is out
            if (lost.any && CommonExpansion.sides(standing.diff(lost)).none) {
                log("No clan holds its", TownHall, "and nobody is out")
                game.internalPerform(EndOfYearAction, soft)
            }
            else
            if (lost.any)
                Then(HallLostAction(lost))
            else
                game.internalPerform(EndOfYearAction, soft)

        case HallLostAction(l) =>
            l.foreach { f =>
                game.out :+= f

                val held = game.controlled(f)

                held.foreach(t => game.buildingsIn(t).foreach { case (s, _) => game.buildings -= s })

                game.board.territories.foreach { t =>
                    if (game.figures(t, f) > 0)
                        game.removeFigures(t, f, game.figures(t, f) + 3)
                }

                game.chiefs -= f

                // Its units away on Raids are lost too: they came back to the Port and the clan was in again (bot games, 2026-11-12)
                game.raids = game.raids.map { case (p, r) => p -> r.owner.has(f).?(r.copy(owner = None, units = 0, years = 0)).|(r) }

                f.hand = $
                f.draw = $
                f.discard = $
                f.active = $
                f.food = 0
                f.wood = 0
                f.lore = 0
                f.fame = 0

                log(DoubleLine)
                f.log("ended the year without their", TownHall, "and is out of the game")
            }

            val sides = CommonExpansion.sides(standing)

            // The last one standing wins with Domination victory (in games with the hidden PCDominationChoice every new game gets;
            // older ones always); a game with nobody left ends anyway
            if (sides.num == 0 || (sides.num == 1 && PCExpansion.dominationWins))
                Then(LastStandingAction(sides.flatten))
            else
                game.internalPerform(EndOfYearAction, soft)

        // Mines give Stone, with no die roll, as upgrades matter so much here (Gman, 2026-11-11): 1 a Mine (an upgraded one counts
        // twice), as long as the supply has Stone; in games with the hidden PCVersionUpdate every new game gets
        case MineHarvestAction(f :: rest, then) if game.options.has(PCVersionUpdate) =>
            val n = MineralsExpansion.mines(f).min(MineralsExpansion.left(Stone))

            if (n == 0)
                log("No", Stone, "is left to extract")
            else {
                f.gainMineral(Stone, n)
                game.mineralBank += Stone -> (MineralsExpansion.left(Stone) - n)

                f.log("extracted", MineralsLabel(n.times(Stone)))

                game.note("mined-stone")
            }

            Then(MineHarvestAction(rest, then))

        // Domination victory only: after the last year the game goes on until one clan (or team) is left holding its Town Hall; the
        // bot games stop after Debug.fameExtraYears more years and score as usual
        case GameEndAction if game.options.has(DominationOnly) && game.year < game.lastYear + Debug.fameExtraYears =>
            log(DoubleLine)
            log("More than one clan still holds its", TownHall, Comma, "so the game goes on")

            game.note("domination-extra-year")

            Then(game.from(game.first).foldRight(NewYearAction : ForcedAction)((f, then) => ReturnUnitsAction(f, then)))

        case LastStandingAction(winners) =>
            game.isOver = true
            game.highlight.current = winners.single

            winners.foreach(f => f.log("won"))

            Debug.summary(game)

            CommonExpansion.victory(winners, $("Won in year " ~ game.year.hl ~ " as the last " ~ (winners.num > 1).?("team").|("clan") ~ " holding " ~ (winners.num > 1).?("its Town Halls").|("its Town Hall") ~ "."))

        case _ => UnknownContinue
    }
}
