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


// Uncharted Horizons' Solo module: the Automa, a neutral clan with two Leaders (Leader 1 is its warchief, Leader 2 its
// companion figure) that plays one card of its own each turn. Rules from the Uncharted Horizons rulebook on Tabletopia
// (pages 13-18); the 15 Automa cards transcribed from the Tabletopia module (images in card/automa/)

// PLAYING PRIORITIES: each narrows the candidate territories (or tiles) to the best ones, in order
trait AutomaPriority extends Record
case object Richest extends AutomaPriority
case object Poorest extends AutomaPriority
case object MostBuildingPoints extends AutomaPriority
case object LeastBuildingPoints extends AutomaPriority
case object Largest extends AutomaPriority
case object Smallest extends AutomaPriority
case object LeaderThere extends AutomaPriority
case object NextToLeader extends AutomaPriority
case object NoLeader extends AutomaPriority
case object MostOpenings extends AutomaPriority
case object LeastOpenings extends AutomaPriority
case object MostSpaces extends AutomaPriority
case object LeastSpaces extends AutomaPriority
case object MostFood extends AutomaPriority
case object LeastFood extends AutomaPriority
case object MostWood extends AutomaPriority
case object LeastWood extends AutomaPriority
case object MostAutomaUnits extends AutomaPriority
case object LeastAutomaUnits extends AutomaPriority
case object MostCamps extends AutomaPriority
case object LeastCamps extends AutomaPriority
case object Northernmost extends AutomaPriority
case object Southernmost extends AutomaPriority
case object Easternmost extends AutomaPriority
case object Westernmost extends AutomaPriority
case object ClosedFirst extends AutomaPriority
case object OpenFirst extends AutomaPriority
case object NearestPlayer extends AutomaPriority
case object FarthestPlayer extends AutomaPriority
// Explore: turn the tile clockwise as little as possible
case object LeastRotations extends AutomaPriority

// ACTIONS
trait AutomaAct extends Record
// Recruit n, possible with at least need Leaders and units in the reserve
case class AutomaRecruit(n : Int, need : Int, prio : $[AutomaPriority]) extends AutomaAct
// Build on a large (3 wood) or small (1 wood) space: the first option possible
case class AutomaBuild(large : Boolean, options : $[AutomaBuildOption]) extends AutomaAct
// Explore from the territory chosen by on, turning the tile by rotate
case class AutomaExplore(on : $[AutomaPriority], rotate : $[AutomaPriority]) extends AutomaAct
case class AutomaMove1(from : $[AutomaPriority], to : $[AutomaPriority]) extends AutomaAct
// Leader 1 or 2: reinforcements from an adjacent territory (from), then a Move 1 with the Leader (to)
case class AutomaMove2(leader : Int, from : $[AutomaPriority], to : $[AutomaPriority]) extends AutomaAct

// The kinds of Development card the Automa takes, in order; Left and Right are the first and last card on display
trait AutomaBuilding extends Record
case class BuildThis(b : Building) extends AutomaBuilding
// A Food Silo if the Automa has at least as much wood as food, otherwise a Woodcutter's Lodge
case object SiloOrLodge extends AutomaBuilding
case class AutomaBuildOption(what : AutomaBuilding, prio : $[AutomaPriority]) extends Record

trait AutomaPick extends Record
case object PickMove extends AutomaPick
case object PickDraw extends AutomaPick
case object PickRecruit extends AutomaPick
case object PickExplore extends AutomaPick
case object PickSpecial extends AutomaPick
case object PickLeft extends AutomaPick
case object PickRight extends AutomaPick

case class AutomaSpec(flash : Boolean, pass : Boolean, first : AutomaAct, second : AutomaAct, picks : $[AutomaPick])

case class AutomaCard(n : Int) extends Card {
    def spec = AutomaCards.specs(n - 1)
    def info = CardInfo("Automa card " + n, "card-automa-" + "%02d".format(n), 0, spec.flash, MapEffect, "")
}

object AutomaCards {
    // The priority rows (left to right) shared by many cards
    private val reinforce1 = $(NextToLeader, NoLeader, MostAutomaUnits, FarthestPlayer, Poorest, Smallest)
    private val reinforce2 = $(NextToLeader, NoLeader, FarthestPlayer, MostAutomaUnits, Poorest, Smallest)
    private val unitFrom = $(NoLeader, MostAutomaUnits, ClosedFirst, FarthestPlayer, Poorest, Smallest)
    private val carved = AutomaBuildOption(BuildThis(CarvedStone), $(FarthestPlayer, Smallest, Poorest, LeastBuildingPoints, MostSpaces))
    private val tower = AutomaBuildOption(BuildThis(DefenseTower), $(NearestPlayer, Largest, Richest, MostBuildingPoints, ClosedFirst, LeastAutomaUnits))

    val specs : $[AutomaSpec] = $(
        // 1
        AutomaSpec(false, false,
            AutomaMove2(1, reinforce1, $(NearestPlayer, MostBuildingPoints, LeastAutomaUnits, Richest, MostSpaces, Largest)),
            AutomaMove2(2, reinforce2, $(NearestPlayer, LeastAutomaUnits, MostBuildingPoints, Richest, Largest, MostSpaces)),
            $(PickSpecial, PickMove, PickLeft)),
        // 2
        AutomaSpec(false, false,
            AutomaMove2(1, reinforce1, $(NearestPlayer, MostBuildingPoints, LeastAutomaUnits, Richest, MostSpaces, Largest)),
            AutomaRecruit(2, 2, $(LeaderThere, LeastAutomaUnits, NearestPlayer, MostCamps, Largest, MostBuildingPoints)),
            $(PickSpecial, PickRecruit, PickRight)),
        // 3
        AutomaSpec(false, false,
            AutomaRecruit(1, 1, $(Richest, Largest, FarthestPlayer, MostBuildingPoints, MostCamps, OpenFirst)),
            AutomaExplore($(FarthestPlayer, Largest, LeastSpaces, Poorest, LeastOpenings, Northernmost), $(Richest, MostWood, ClosedFirst, MostSpaces, LeastRotations)),
            $(PickDraw, PickSpecial, PickLeft)),
        // 4
        AutomaSpec(true, false,
            AutomaRecruit(2, 1, $(NearestPlayer, Richest, MostBuildingPoints, MostCamps, Largest, LeaderThere)),
            AutomaMove2(1, reinforce1, $(NearestPlayer, Richest, LeastAutomaUnits, MostBuildingPoints, MostSpaces, Largest)),
            $(PickSpecial, PickMove, PickRight)),
        // 5
        AutomaSpec(false, false,
            AutomaMove2(2, reinforce2, $(NearestPlayer, LeastAutomaUnits, MostBuildingPoints, Richest, Largest, MostSpaces)),
            AutomaRecruit(2, 2, $(LeaderThere, LeastAutomaUnits, MostCamps, NearestPlayer, Largest, MostBuildingPoints)),
            $(PickMove, PickExplore, PickRight)),
        // 6
        AutomaSpec(false, false,
            AutomaMove2(2, reinforce2, $(NearestPlayer, LeastAutomaUnits, MostBuildingPoints, Richest, Largest, MostSpaces)),
            AutomaMove2(1, reinforce1, $(NearestPlayer, Richest, LeastAutomaUnits, MostBuildingPoints, MostSpaces, Largest)),
            $(PickRecruit, PickMove, PickRight)),
        // 7
        AutomaSpec(true, false,
            AutomaMove1(unitFrom, $(LeastAutomaUnits, Richest, OpenFirst, FarthestPlayer, MostSpaces, Largest)),
            AutomaRecruit(2, 2, $(LeaderThere, NearestPlayer, Largest, MostBuildingPoints, MostCamps, Richest)),
            $(PickDraw, PickSpecial, PickRight)),
        // 8
        AutomaSpec(false, true,
            AutomaExplore($(Largest, LeastSpaces, Poorest, LeastOpenings, Southernmost), $(MostWood, ClosedFirst, Richest, MostSpaces, LeastRotations)),
            AutomaMove1(unitFrom, $(OpenFirst, FarthestPlayer, Richest, MostSpaces, Largest)),
            $(PickSpecial, PickDraw, PickRight)),
        // 9
        AutomaSpec(false, true,
            AutomaBuild(true, $(AutomaBuildOption(BuildThis(AltarOfKings), $(FarthestPlayer, MostAutomaUnits, Largest, Poorest, LeastBuildingPoints)))),
            AutomaBuild(false, $(carved, tower)),
            $(PickDraw, PickRecruit, PickRight)),
        // 10
        AutomaSpec(false, false,
            AutomaRecruit(1, 1, $(Richest, MostBuildingPoints, Largest, FarthestPlayer, MostCamps, OpenFirst)),
            AutomaExplore($(Largest, LeastSpaces, Poorest, LeastOpenings, Northernmost), $(Richest, MostWood, ClosedFirst, MostSpaces, LeastRotations)),
            $(PickSpecial, PickExplore, PickLeft)),
        // 11
        AutomaSpec(true, false,
            AutomaExplore($(Largest, LeastOpenings, LeastSpaces, Poorest, Northernmost), $(MostFood, Richest, ClosedFirst, MostSpaces, LeastRotations)),
            AutomaMove1(unitFrom, $(OpenFirst, Richest, FarthestPlayer, MostSpaces, Largest)),
            $(PickMove, PickSpecial, PickLeft)),
        // 12
        AutomaSpec(false, true,
            AutomaBuild(true, $(AutomaBuildOption(BuildThis(AltarOfKings), $(FarthestPlayer, ClosedFirst, Largest, Poorest, LeastBuildingPoints)))),
            AutomaBuild(false, $(AutomaBuildOption(SiloOrLodge, $(FarthestPlayer, Poorest, LeastBuildingPoints, Smallest, MostSpaces)))),
            $(PickSpecial, PickDraw, PickLeft)),
        // 13
        AutomaSpec(true, true,
            AutomaRecruit(1, 1, $(LeaderThere, MostBuildingPoints, MostCamps, Richest, OpenFirst, NearestPlayer)),
            AutomaMove2(2, reinforce2, $(NearestPlayer, LeastAutomaUnits, MostBuildingPoints, Richest, Largest, MostSpaces)),
            $(PickMove, PickExplore, PickLeft)),
        // 14
        AutomaSpec(true, false,
            AutomaBuild(true, $(AutomaBuildOption(BuildThis(Fortress), $(NearestPlayer, ClosedFirst, Largest, Richest, MostBuildingPoints)))),
            AutomaBuild(false, $(carved, AutomaBuildOption(BuildThis(TrainingCamp), $(NearestPlayer, Largest, Richest, MostBuildingPoints, MostSpaces)))),
            $(PickMove, PickDraw, PickLeft)),
        // 15
        AutomaSpec(false, false,
            AutomaExplore($(FarthestPlayer, Poorest, LeastOpenings, Largest, LeastSpaces, Easternmost), $(Richest, MostFood, ClosedFirst, MostSpaces, LeastRotations)),
            AutomaBuild(false, $(tower)),
            $(PickMove, PickRecruit, PickLeft)),
    )

    val all : $[AutomaCard] = 1.to(specs.num).$./(AutomaCard(_))
}

case class AutomaCardLabel(card : AutomaCard) extends Elementary {
    def elem = ("Automa card " + card.n).hl
}

// SETUP
case class ShuffledAutomaAction(shuffled : $[AutomaCard], rest : $[Faction]) extends ShuffledAction[AutomaCard]
// redraw: a tile without resources may still be put back once; tries: tiles left to try
case class AutomaSetupAction(round : Int, l : $[Faction], redraw : Boolean, tries : Int) extends ForcedAction
case class AutomaSetupUnitsAction(round : Int, l : $[Faction], area : |[AreaRef]) extends ForcedAction

// THE YEAR
case class AutomaDrawAction(n : Int, then : ForcedAction) extends ForcedAction
case class AutomaReshuffledAction(shuffled : $[AutomaCard], then : ForcedAction) extends ShuffledAction[AutomaCard]
case class AutomaTurnAction(chain : Boolean) extends ForcedAction
case class AutomaTryAction(card : AutomaCard, step : Int, chain : Boolean) extends ForcedAction
// reveal: the picks come from the top card of the Automa pile, which is then discarded
case class AutomaPassAction(picks : $[AutomaPick], reveal : Boolean) extends ForcedAction
case class AutomaPassRevealAction(shuffled : $[AutomaCard]) extends ShuffledAction[AutomaCard]

// ACTIONS: steps with an area are offered to the player when the priorities leave a tie
case class AutomaRecruitAction(left : Int, prio : $[AutomaPriority], area : |[AreaRef], then : ForcedAction) extends ForcedAction
case class AutomaBuildAction(b : AutomaBuild, area : |[AreaRef], then : ForcedAction) extends ForcedAction
// from: the territory it explores from; spot: where the tile goes
case class AutomaExploreAction(e : AutomaExplore, from : |[AreaRef], spot : |[Spot], tries : Int, then : ForcedAction) extends ForcedAction
case class AutomaMove1Action(m : AutomaMove1, from : |[AreaRef], to : |[AreaRef], then : ForcedAction) extends ForcedAction
case class AutomaReinforceAction(m : AutomaMove2, area : |[AreaRef], then : ForcedAction) extends ForcedAction
case class AutomaLeaderMoveAction(m : AutomaMove2, area : |[AreaRef], then : ForcedAction) extends ForcedAction


object AutomaExpansion extends Expansion {
    // Leaders count as units; with the Warchiefs module they are warchiefs with +2 combat points
    def leaderStrength(implicit game : Game) : Int = game.has(Warchiefs).?(3).|(1)

    def level(implicit game : Game) : Int = options.of[AutomaLevelOption].single./(_.level).|(2)

    def player(implicit game : Game) : Faction = game.setup.but(Automa).head

    def mine(implicit game : Game) = game.controlled(Automa)

    def leaderAt(k : Int)(implicit game : Game) : |[AreaRef] = (k == 1).?(game.chiefs.get(Automa)).|(game.leader2)

    def leaderIn(t : Territory)(implicit game : Game) = game.chiefIn(t, Automa) || game.kaijaIn(t, Automa)

    def leaderTerritories(implicit game : Game) = $(1, 2)./~(leaderAt)./(game.board.territory)

    // Moves needed to reach t from the player's territories (Rough borders count 2)
    def distances(implicit game : Game) : Map[Territory, Int] = {
        var dist = game.controlled(player)./(_ -> 0).toMap
        var changed = true
        while (changed) {
            changed = false
            dist.toList.foreach { case (t, d) =>
                game.board.adjacent(t).foreach { case (o, regular) =>
                    val n = d + regular.?(1).|(2)
                    if (dist.get(o).forall(_ > n)) {
                        dist += o -> n
                        changed = true
                    }
                }
            }
        }
        dist
    }

    def openings(t : Territory)(implicit game : Game) : Int =
        t.areas./~(a => game.board.sides(a)./(s => (a.x + s.dx, a.y + s.dy))).distinct.count { case (x, y) => game.board.empty(x, y) }

    def freeSpaces(t : Territory)(implicit game : Game) : Int =
        t.areas./~(a => game.board.spec(a).spaces.indices./(i => SpaceRef(a, i))).count(s => game.buildings.contains(s).not && game.gear.contains(s).not)

    def buildingPoints(t : Territory)(implicit game : Game) : Int = game.buildingsIn(t).map(_._2.large.?(3).|(1)).sum

    def center(t : Territory)(implicit game : Game) : (Double, Double) = {
        val l = t.areas./(game.board.point)
        (l.map(_._1).sum / l.num, l.map(_._2).sum / l.num)
    }

    // A priority's value for a territory, higher is better
    def value(p : AutomaPriority, t : Territory, dist : => Map[Territory, Int])(implicit game : Game) : Double = {
        def d : Double = dist.getOrElse(t, 99).toDouble
        lazy val (food, wood, lore) = game.produce(t)
        p match {
            case Richest => food + wood + lore
            case Poorest => -(food + wood + lore)
            case MostBuildingPoints => buildingPoints(t)
            case LeastBuildingPoints => -buildingPoints(t)
            case Largest => game.board.tiles(t)
            case Smallest => -game.board.tiles(t)
            case LeaderThere => leaderIn(t).??(1)
            case NextToLeader => game.board.adjacent(t).exists(x => leaderIn(x._1)).??(1)
            case NoLeader => leaderIn(t).not.??(1)
            case MostOpenings => openings(t)
            case LeastOpenings => -openings(t)
            case MostSpaces => freeSpaces(t)
            case LeastSpaces => -freeSpaces(t)
            case MostFood => food
            case LeastFood => -food
            case MostWood => wood
            case LeastWood => -wood
            case MostAutomaUnits => game.count(t, Automa)
            case LeastAutomaUnits => -game.count(t, Automa)
            case MostCamps => game.working(t).count(_ == TrainingCamp)
            case LeastCamps => -game.working(t).count(_ == TrainingCamp)
            case Northernmost => -center(t)._2
            case Southernmost => center(t)._2
            case Easternmost => center(t)._1
            case Westernmost => -center(t)._1
            case ClosedFirst => game.board.closed(t).??(1)
            case OpenFirst => game.board.open(t).??(1)
            case NearestPlayer => -d
            case FarthestPlayer => d
            case LeastRotations => 0
        }
    }

    // The candidates left after applying the priorities in order
    def best(l : $[Territory], prio : $[AutomaPriority])(implicit game : Game) : $[Territory] = {
        lazy val dist = distances
        prio.foldLeft(l) { (l, p) =>
            if (l.num <= 1) l
            else {
                val v = l./(t => t -> value(p, t, dist)).toMap
                val m = v.values.max
                l.%(t => v(t) == m)
            }
        }
    }

    // The territory chosen by the priorities; a tie left is the player's choice
    def choose(l : $[Territory], prio : $[AutomaPriority], next : AreaRef => ForcedAction)(implicit game : Game) : Continue = {
        val b = best(l, prio)
        if (b.num == 1)
            Then(next(b.head.anchor))
        else
            Ask(player).each(b)(t => next(t.anchor).as(t.anchor)("Automa".hl, "choose between the tied territories"))
    }

    // CONDITIONS
    def recruitTargets(implicit game : Game) = {
        val l = mine.%(t => game.bearIn(t).not && game.hostileIn(t).not && game.swampIn(t).not)
        l.any.?(l).|(game.board.territories.%(t => game.present(t).none && game.hostileIn(t).not && game.swampIn(t).not))
    }

    def canRecruit(implicit game : Game) = MapExpansion.canRecruit(Automa) && recruitTargets.any

    // Leaders and units in the reserve
    def inReserve(implicit game : Game) = game.reserve(Automa) + game.chiefReady(Automa).??(1) + game.kaijaReady(Automa).??(1)

    def buildOptions(implicit game : Game) = MapExpansion.buildOptions(Automa, BuildEffect(), false)

    def building(w : AutomaBuilding)(implicit game : Game) : Building = w match {
        case BuildThis(b) => b
        case SiloOrLodge => (Automa.wood >= Automa.food).?(FoodSilo).|(WoodcutterLodge)
    }

    // The first option of a Build action that can be built somewhere
    def buildChoice(b : AutomaBuild)(implicit game : Game) : |[AutomaBuildOption] =
        b.options.find(o => buildOptions.exists(x => x._2 == building(o.what) && x._2.large == b.large))

    // Explore: the Automa's open territories with a free spot next to them
    def exploreFrom(implicit game : Game) : $[Territory] = explorable.%(t => spotsNext(t).any)

    def spotsNext(t : Territory)(implicit game : Game) : $[Spot] =
        game.board.frontier.%{ case (x, y) => Side.all.exists(s => game.board.areaAt(x + s.dx, y + s.dy, s.opposite).exists(t.areas.contains)) }./{ case (x, y) => Spot(x, y) }

    def explorable(implicit game : Game) = mine.%(game.board.open).%(t => game.bearIn(t).not)

    // Friendly or neutral territories next to t, crossing Rough borders, where Automa units can go
    def neighbours(t : Territory)(implicit game : Game) : $[Territory] =
        game.board.adjacent(t).map(_._1).%(o => game.present(o).forall(_ == Automa)).%(o => game.hostileIn(o).not && game.swampIn(o).not && game.creaturesIn(o).none)

    def move1Sources(implicit game : Game) : $[Territory] = move1Pairs.map(_._1).distinct

    def move1Pairs(implicit game : Game) : $[(Territory, Territory)] =
        mine.%(t => game.count(t, Automa) >= 2 && game.bearIn(t).not && game.hostileIn(t).not)./~(t => neighbours(t)./(o => t -> o))

    // Leader k's territory, the units that could reinforce it, and where it could go
    def reinforcements(k : Int)(implicit game : Game) : $[Territory] = leaderAt(k)./(game.board.territory).$./~(lt =>
        game.board.adjacent(lt).map(_._1).%(o => game.present(o) == $(Automa) && game.count(o, Automa) >= 2 && game.bearIn(o).not && game.hostileIn(o).not))

    // Units that can go with the Leader: one figure always stays behind
    def movable(lt : Territory, k : Int)(implicit game : Game) : Int = {
        val other = leaderAt(3 - k).exists(lt.areas.contains)
        math.max(0, game.count(lt, Automa) - other.not.??(1))
    }

    def destinations(k : Int, extra : Int)(implicit game : Game) : $[Territory] = leaderAt(k)./(game.board.territory).$./~{ lt =>
        if (game.bearIn(lt) || game.hostileIn(lt))
            $
        else {
            val n = movable(lt, k) + extra
            val all = game.board.adjacent(lt).map(_._1).%(o => game.hostileIn(o).not && game.swampIn(o).not && game.creaturesIn(o).none && MapExpansion.enterable(o))
                .%(o => game.present(o).forall(g => g == Automa || game.enemy(Automa, g)))
                // Never alone into an enemy territory
                .%(o => game.present(o).but(Automa).none || n >= 1)
            val l = all.%(o => leaderAt(3 - k).exists(o.areas.contains).not)
            // Into the other Leader's territory only if there is nowhere else
            (n > 0 || all.exists(o => game.present(o).none)).??(l.any.?(l).|(all)) : $[Territory]
        }
    }

    def canMove2(k : Int)(implicit game : Game) = leaderAt(k).any && {
        val lt = game.board.territory(leaderAt(k).get)
        val extra = reinforcements(k)./(o => (game.count(o, Automa) + 1) / 2).maxOption.|(0)
        (movable(lt, k) + extra > 0) && destinations(k, extra).any
    }

    def possible(a : AutomaAct)(implicit game : Game) : Boolean = a match {
        case AutomaRecruit(_, need, _) => canRecruit && inReserve >= need
        case b : AutomaBuild => buildChoice(b).any
        case AutomaExplore(_, _) => exploreFrom.any && game.pile.any
        case AutomaMove1(_, _) => move1Pairs.any
        case AutomaMove2(k, _, _) => canMove2(k)
    }

    def act(a : AutomaAct, then : ForcedAction)(implicit game : Game) : Continue = a match {
        case AutomaRecruit(n, _, prio) => Then(AutomaRecruitAction(n, prio, None, then))
        case b : AutomaBuild => Then(AutomaBuildAction(b, None, then))
        case e : AutomaExplore => Then(AutomaExploreAction(e, None, None, game.pile.num, then))
        case m : AutomaMove1 => Then(AutomaMove1Action(m, None, None, then))
        case m : AutomaMove2 => Then(AutomaReinforceAction(m, None, then))
    }

    // Closed territories once p is placed
    def closedWith(p : Placement)(implicit game : Game) : Int = {
        val all = game.board.placements :+ p
        def at(x : Int, y : Int) = all.find(q => q.x == x && q.y == y)
        game.board.preview(p).count(_.areas.forall { a =>
            val q = at(a.x, a.y).get
            q.spec.area(a.id).edges./(_.rotate(q.r)).forall(s => at(a.x + s.dx, a.y + s.dy).any)
        })
    }

    // Spots for a new tile next to the Automa's open territories
    def spots(implicit game : Game) : $[Spot] = {
        val open = explorable
        game.board.frontier.%{ case (x, y) => Side.all.exists(s => game.board.areaAt(x + s.dx, y + s.dy, s.opposite).exists(a => open.exists(_.areas.contains(a)))) }./{ case (x, y) => Spot(x, y) }
    }

    def spotValue(p : AutomaPriority, s : Spot) : Double = p match {
        case Northernmost => -s.y
        case Southernmost => s.y
        case Easternmost => s.x
        case Westernmost => -s.x
        case _ => 0
    }

    // The Development card kinds
    def kind(c : Card) : AutomaPick = c.effect match {
        case MoveEffect(_, _, _, _) => PickMove
        case DrawEffect(_, _, _, _) | NegotiationEffect | ResourcefulEffect => PickDraw
        case RecruitEffect(_, _) | RecruitPerEffect(_) => PickRecruit
        case ExploreEffect(_, _, _, _, _) => PickExplore
        case _ => PickSpecial
    }

    def pick(picks : $[AutomaPick])(implicit game : Game) : |[Card] = {
        val l = game.display
        if (l.none)
            None
        else
        // The last year: the Achievement giving the Automa the most fame, then the player the most
        if (game.year == game.lastYear)
            l.sortBy(c => (-CommonExpansion.cardFame(Automa, c), -CommonExpansion.cardFame(player, c))).headOption
        else
        // The player has passed: the card left
        if (player.passed)
            l.headOption
        else
            picks.foldLeft(None : |[Card]) { (r, p) =>
                r.orElse(p match {
                    case PickLeft => l.headOption
                    case PickRight => l.lastOption
                    case k => l.find(c => kind(c) == k)
                })
            }.orElse(l.headOption)
    }

    // Combat: food to lead the player's best total by at most 2, or all it can if that isn't enough
    def food(t : Territory, attacking : Boolean, e : MoveEffect, spent : |[Int])(implicit game : Game) : Int = {
        val opp = game.present(t).but(Automa).single.|(player)
        val here = game.working(t)
        val fortress = attacking.??(2 * here.count(_ == Fortress))
        val them = game.strength(t, opp, attacking.not) + fortress + attacking.not.??(e.bonus) + spent.|(math.min(opp.food, game.figures(t, opp)))
        val us = game.strength(t, Automa, attacking) + attacking.??(e.bonus) + attacking.not.??(2 * here.count(_ == Fortress))
        val max = math.min(Automa.food, game.figures(t, Automa))
        math.min(max, math.max(0, them + 2 - us))
    }

    def perform(action : Action, soft : Void)(implicit game : Game) = action @@ {
        // SETUP: without Enemy Secrets and Ancestral Curse; the Automa's deck instead of a clan deck; the player goes first
        case ShuffledAdvancedAction(l) if l.exists(c => c == Development("enemy-secrets") || c == Development("ancestral-curse")) =>
            game.internalPerform(ShuffledAdvancedAction(l.%(c => c != Development("enemy-secrets") && c != Development("ancestral-curse"))), soft)

        case ShuffleStartingDecksAction(Automa :: rest) =>
            Shuffle[AutomaCard](AutomaCards.all, ShuffledAutomaAction(_, rest))

        case ShuffledAutomaAction(l, rest) =>
            game.automaDeck = l
            log("Automa".hl, "shuffled its", l.num.hl, "cards (level", level.hl ~ ")")
            Then(ShuffleStartingDecksAction(rest))

        case ShuffleStartingDecksAction(Nil) =>
            Random[Faction]($(player), FirstPlayerAction(_))

        // Map setup: the Automa places the top tile of the pile (once more if it shows no resource) next to the
        // starting tile, then above (or below) its first tile, unturned when possible; Leader 1 (then 2) and two units go
        // on its richest territory
        case SetupPlaceAction(round, Automa :: rest) =>
            Then(AutomaSetupAction(round, Automa :: rest, true, game.pile.num))

        case AutomaSetupAction(round, l, redraw, tries) =>
            val first = game.automaStart
            val wanted = (round == 1).?($(Spot(1, 0))).|(first./(s => $(Spot(s.x, s.y - 1), Spot(s.x, s.y + 1))).|($))

            val tile = game.pile.head
            game.pile = game.pile.drop(1)

            val rich = Tiles(tile).areas.exists(a => a.food + a.wood + a.lore > 0)
            val legal = MapExpansion.setupPlacements(tile, round)
            val spots = wanted.%(s => legal.exists(_._1 == s)) ++ legal.map(_._1).distinct.%(s => first.forall(f => (s.x - f.x).abs + (s.y - f.y).abs == 1))

            if ((rich.not && redraw && spots.any) || spots.none) {
                game.pile :+= tile
                if (tries > 1)
                    Then(AutomaSetupAction(round, l, redraw && rich, tries - 1))
                else
                    Then(SetupPlaceAction(round, l.drop(1)))
            }
            else {
                val s = spots.head
                val r = MapExpansion.rotations(legal, s).head
                game.board.place(Placement(tile, s.x, s.y, r))
                if (round == 1)
                    game.automaStart = |(s)

                Automa.log("placed a tile")

                Then(TilePlacedAction(Automa, tile, s, true, AutomaSetupUnitsAction(round, l, None)))
            }

        case AutomaSetupUnitsAction(round, l, None) =>
            val s = game.board.placements.last
            val all = Tiles(s.tile).areas./(a => game.board.territory(AreaRef(s.x, s.y, a.id))).distinct.%(t => game.present(t).none).%(t => game.swampIn(t).not && game.hostileIn(t).not)

            if (all.none)
                Then(SetupPlaceAction(round, l.drop(1)))
            else
                choose(all, $(Richest, MostFood, MostWood, MostSpaces, OpenFirst, FarthestPlayer), a => AutomaSetupUnitsAction(round, l, |(a)))

        case AutomaSetupUnitsAction(round, l, Some(a)) =>
            game.addUnits(a, Automa, 2)
            if (round == 1)
                game.chiefs += Automa -> a
            else
                game.leader2 = |(a)

            Automa.log("placed", WarchiefElem(Automa).elem, "and two units in", a)

            Then(SetupPlaceAction(round, l.drop(1)))

        // START OF THE YEAR: 4 cards, +1 per Forge, +1 per lore (spent), +1 per large building the player has more, +1 per level above 3
        case RevealDevelopmentsAction if game.automaDrawn.not =>
            game.automaDrawn = true

            game.automaDiscard ++= game.automaPlayed ++ game.automaActions
            game.automaPlayed = $
            game.automaActions = $

            val forges = mine./~(game.working).count(_ == Forge)
            val lore = Automa.lore
            Automa.lore = 0
            val large = math.max(0, game.controlled(player)./~(game.buildingsIn).count(_._2.large) - mine./~(game.buildingsIn).count(_._2.large))
            val n = 4 + forges + lore + large + math.max(0, level - 3)

            Automa.log("draws", n.hl, "cards")

            Then(AutomaDrawAction(n, RevealDevelopmentsAction))

        case StartYearAction =>
            game.automaDrawn = false
            UnknownContinue

        case AutomaDrawAction(n, then) =>
            if (n <= 0)
                Then(then)
            else
            if (game.automaDeck.any) {
                game.automaActions :+= game.automaDeck.head
                game.automaDeck = game.automaDeck.drop(1)
                Then(AutomaDrawAction(n - 1, then))
            }
            else
            if (game.automaDiscard.any)
                Shuffle[AutomaCard](game.automaDiscard, AutomaReshuffledAction(_, AutomaDrawAction(n, then)))
            else
                Then(then)

        case AutomaReshuffledAction(l, then) =>
            game.automaDiscard = $
            game.automaDeck = l
            Then(then)

        // ACTIONS
        case TurnAction(Automa, _) =>
            game.highlight.current = |(Automa)
            Then(AutomaTurnAction(false))

        case AutomaTurnAction(chain) =>
            game.automaActions match {
                case Nil =>
                    if (chain)
                        Then(NextTurnAction(Automa))
                    else
                    // Without cards the Automa passes; if the player hasn't, it picks by the top card of its pile
                    if (player.passed.not && game.automaDeck.any)
                        Then(AutomaPassAction(game.automaDeck.head.spec.picks, true))
                    else
                        Then(AutomaPassAction($, false))

                case c :: rest =>
                    game.automaActions = rest
                    game.automaPlayed :+= c

                    // The last card with the Pass icon: the Automa passes if the player hasn't
                    if (rest.none && c.spec.pass && player.passed.not && chain.not) {
                        Automa.log("revealed", AutomaCardLabel(c), "(its last, with the Pass icon)")
                        Then(AutomaPassAction(c.spec.picks, false))
                    }
                    else {
                        Automa.log("revealed", AutomaCardLabel(c), c.spec.flash.?("(Flash)".txt).|(Empty))
                        Then(AutomaTryAction(c, 1, chain))
                    }
            }

        case AutomaTryAction(c, step, chain) =>
            if (step > 2) {
                Automa.log("could do neither action and draws another card")
                Then(AutomaTurnAction(chain))
            }
            else {
                val a = (step == 1).?(c.spec.first).|(c.spec.second)
                if (possible(a)) {
                    game.note("automa-" + (a match {
                        case AutomaRecruit(_, _, _) => "recruit"
                        case AutomaBuild(_, _) => "build"
                        case AutomaExplore(_, _) => "explore"
                        case AutomaMove1(_, _) => "move1"
                        case AutomaMove2(_, _, _) => "move2"
                    }))
                    act(a, c.spec.flash.?(AutomaTurnAction(true) : ForcedAction).|(NextTurnAction(Automa)))
                }
                else
                    Then(AutomaTryAction(c, step + 1, chain))
            }

        case AutomaPassAction(picks, reveal) =>
            if (reveal && game.automaDeck.any) {
                // The card it picked by is discarded
                game.automaDiscard :+= game.automaDeck.head
                game.automaDeck = game.automaDeck.drop(1)
            }

            if (factions.forall(_.passed.not)) {
                game.first = Automa
                Automa.log("passed and took the first player marker")
            }
            else
                Automa.log("passed")

            Automa.passed = true
            game.automaDiscard ++= game.automaActions
            game.automaActions = $

            pick(picks).foreach { c =>
                game.display = game.display.diff($(c))
                Automa.discard :+= c
                Automa.log("took", c)
            }

            Then(PassedAction(Automa))

        // RECRUIT: Leader 1, then Leader 2, then units
        case AutomaRecruitAction(left, prio, None, then) =>
            if (left <= 0 || canRecruit.not)
                Then(then)
            else
                choose(recruitTargets, prio, a => AutomaRecruitAction(left, prio, |(a), then))

        case AutomaRecruitAction(left, prio, Some(a), then) =>
            if (game.chiefReady(Automa)) {
                game.chiefs += Automa -> a
                Automa.log("recruited", WarchiefElem(Automa), "in", a)
            }
            else
            if (game.kaijaReady(Automa)) {
                game.leader2 = |(a)
                Automa.log("recruited", Companion(Automa), "in", a)
            }
            else {
                game.addUnits(a, Automa, 1)
                Automa.log("recruited in", a)
            }
            Then(AutomaRecruitAction(left - 1, prio, None, then))

        // BUILD: the card's building (the first of its options that can be built), where its priorities say
        case AutomaBuildAction(b, None, then) =>
            buildChoice(b) match {
                case Some(o) =>
                    val kind = building(o.what)
                    choose(buildOptions.filter(_._2 == kind).map(x => game.board.territory(x._1)).distinct, o.prio, a => AutomaBuildAction(b, |(a), then))
                case None =>
                    Then(then)
            }

        case AutomaBuildAction(b, Some(a), then) =>
            buildChoice(b).foreach { o =>
                val kind = building(o.what)
                buildOptions.find(x => x._2 == kind && game.board.territory(x._1).areas.contains(a)).foreach { case (a, b, s, cost) =>
                    Automa.wood -= cost
                    game.buildings += s -> b
                    Automa.log("built", b, "in", a)
                    game.advance(Automa, "architecture")
                }
            }
            Then(then)

        // EXPLORE: from the territory chosen by the card, on its free spot (the compass priority, or the player's choice
        // between spots), with the top tile turned by the card's rotation priorities
        case AutomaExploreAction(e, None, _, tries, then) =>
            if (exploreFrom.none || game.pile.none)
                Then(then)
            else
                choose(exploreFrom, e.on, a => AutomaExploreAction(e, |(a), None, tries, then))

        case AutomaExploreAction(e, Some(from), None, tries, then) =>
            val l = spotsNext(game.board.territory(from))
            val compass = e.on.%(p => $[AutomaPriority](Northernmost, Southernmost, Easternmost, Westernmost).has(p))
            val b = compass.foldLeft(l)((l, p) => { val m = l./(spotValue(p, _)).max ; l.%(spotValue(p, _) == m) })

            if (b.none)
                Then(then)
            else
            if (b.num == 1)
                Then(AutomaExploreAction(e, |(from), |(b.head), tries, then))
            else
                Ask(player).each(b)(s => AutomaExploreAction(e, |(from), |(s), tries, then).as(s)("Automa".hl, "explores: choose between the spots"))

        case AutomaExploreAction(e, Some(from), Some(spot), tries, then) =>
            if (tries <= 0 || game.pile.none) {
                Automa.log("found no tile to explore with")
                Then(then)
            }
            else {
                val tile = game.pile.head
                game.pile = game.pile.drop(1)

                val rs = MapExpansion.rotations(MapExpansion.placements(tile, |($(game.board.territory(from))), false), spot)

                if (rs.none) {
                    game.pile :+= tile
                    Then(AutomaExploreAction(e, |(from), |(spot), tries - 1, then))
                }
                else {
                    // Each turn measured with the tile in place: the territory explored from, and how many territories are closed
                    val before = game.board.territories.count(game.board.closed)
                    val r = e.rotate.foldLeft(rs) { (rs, p) =>
                        if (rs.num <= 1) rs
                        else {
                            val v = rs./(r => r -> game.board.withPlaced(Placement(tile, spot.x, spot.y, r)) {
                                val t = game.board.territory(from)
                                p match {
                                    case ClosedFirst => (game.board.territories.count(game.board.closed) > before).??(1).toDouble
                                    case LeastRotations => -r.toDouble
                                    case p => value(p, t, Map())
                                }
                            }).toMap
                            val m = v.values.max
                            rs.%(r => v(r) == m)
                        }
                    }.head

                    game.exploring = $(tile)
                    game.internalPerform(ExploreTurnAction(Automa, tile, spot, r, 1, ExploreEffect(), then), soft)
                }
            }

        // MOVE 1: one unit from the territory chosen by the first priorities (2+ units) to a friendly or neutral one next to it
        case AutomaMove1Action(m, None, _, then) =>
            choose(move1Sources, m.from, a => AutomaMove1Action(m, |(a), None, then))

        case AutomaMove1Action(m, Some(from), None, then) =>
            val src = game.board.territory(from)
            choose(move1Pairs.filter(_._1 == src).map(_._2), m.to, a => AutomaMove1Action(m, |(from), |(a), then))

        case AutomaMove1Action(m, Some(from), Some(a), then) =>
            val src = game.board.territory(from)
            val dst = game.board.territory(a)
            game.removeUnits(src, Automa, 1)
            game.addUnits(dst.anchor, Automa, 1)
            Automa.log("moved 1 unit from", from, "to", a)
            Then(then)

        // MOVE 2 WITH A LEADER: first, half the units (rounded up) of an adjacent territory reinforce the Leader
        case AutomaReinforceAction(m, None, then) =>
            val l = reinforcements(m.leader)
            if (l.none)
                Then(AutomaLeaderMoveAction(m, None, then))
            else
                choose(l, m.from, a => AutomaReinforceAction(m, |(a), then))

        case AutomaReinforceAction(m, Some(a), then) =>
            val src = game.board.territory(a)
            val n = (game.count(src, Automa) + 1) / 2
            val to = leaderAt(m.leader).get
            game.removeUnits(src, Automa, n)
            game.addUnits(game.board.territory(to).anchor, Automa, n)
            Automa.log("reinforced", (m.leader == 1).?(WarchiefElem(Automa).elem).|(Companion(Automa).elem), "with", n.hl, (n == 1).?("unit").|("units"), "from", a)
            Then(AutomaLeaderMoveAction(m, None, then))

        // Then the Leader moves, with: against the player, enough units to outnumber the defenders by at most 2;
        // into a neutral territory, half the units (rounded up); into its own territory, all units but one
        case AutomaLeaderMoveAction(m, None, then) =>
            val l = destinations(m.leader, 0)
            if (l.none) {
                Automa.log("could not move", (m.leader == 1).?(WarchiefElem(Automa).elem).|(Companion(Automa).elem))
                Then(then)
            }
            else
                choose(l, m.to, a => AutomaLeaderMoveAction(m, |(a), then))

        case AutomaLeaderMoveAction(m, Some(a), then) =>
            val from = leaderAt(m.leader).get
            val lt = game.board.territory(from)
            val dst = game.board.territory(a)
            val enemy = game.present(dst).but(Automa)
            val units = movable(lt, m.leader)
            val n =
                if (enemy.any) math.min(units, math.max(1, enemy./(game.figures(dst, _)).sum + 2 - 1))
                else if (game.present(dst).none) math.min(units, (game.count(lt, Automa) + 1) / 2)
                else units

            game.removeUnits(lt, Automa, n)
            game.addUnits(dst.anchor, Automa, n)
            if (m.leader == 1)
                game.chiefs += Automa -> dst.anchor
            else
                game.leader2 = |(dst.anchor)

            Automa.log("moved", Party(Automa, n, m.leader == 2, m.leader == 1), "from", from, "to", a, enemy.any.?("and attacked " ~ enemy./(_.elem).join(", ")).|(Empty))

            if (enemy.any) {
                game.combats :+= dst.anchor
                Then(CombatsAction(Automa, MoveEffect(2), then))
            }
            else
                Then(then)

        // COMBAT: the food rule, the die choice, the retreat
        case CombatFoodStartAction(Automa, defender, a, e, then) =>
            val k = food(game.board.territory(a), true, e, None)
            Automa.food -= k
            if (k > 0)
                Automa.log("spent", k.hl, Food)
            Then(CombatFoodPaidAction(Automa, defender, a, e, $(k), then))

        case CombatFoodPaidAction(attacker, Automa, a, e, food, then) if food.num == 1 =>
            val k = this.food(game.board.territory(a), false, e, |(food(0)))
            Automa.food -= k
            if (k > 0)
                Automa.log("spent", k.hl, Food)
            Then(CombatRollAction(attacker, Automa, a, e, food :+ k, $, then))

        case CombatFaceAction(attacker, defender, a, e, food, faces, face, then) if face.choice && (faces.num == 0).?(attacker).|(defender) == Automa =>
            val t = game.board.territory(a)
            val opp = (attacker == Automa).?(defender).|(attacker)
            // The casualty if it wipes out the enemy, otherwise the point
            val choice = (game.figures(t, opp) <= 1).?(NorthgardDie.casualty).|(NorthgardDie.point)
            Automa.log("took", choice)
            MapExpansion.rolled(attacker, defender, a, e, food, faces, choice, then)

        // Retreat all together to one adjacent friendly or neutral territory: most building points, most resources, largest, closed
        case RetreatAction(Automa, a, rough, then) if game.retreatBy.none =>
            val t = game.board.territory(a)
            val n = game.count(t, Automa)
            val l1 = game.chiefIn(t, Automa)
            val l2 = game.kaijaIn(t, Automa)

            if (n == 0 && l1.not && l2.not)
                Then(then)
            else {
                val fighting = (game.combats ++ game.creatureFights./(_.area))./(game.board.territory)
                val to = game.board.adjacent(t).map(_._1).%(o => fighting.has(o).not).%(o => game.present(o).but(Automa).none).%(o => game.hostileIn(o).not).%(o => game.swampIn(o).not)

                if (to.none) {
                    game.removeFigures(t, Automa, game.figures(t, Automa))
                    Automa.log("had nowhere to retreat and lost", Party(Automa, n, l2, l1))
                    Then(then)
                }
                else {
                    val o = best(to, $(MostBuildingPoints, Richest, Largest, ClosedFirst)).head
                    game.removeUnits(t, Automa, n)
                    game.addUnits(o.anchor, Automa, n)
                    if (l1)
                        game.chiefs += Automa -> o.anchor
                    if (l2)
                        game.leader2 = |(o.anchor)
                    Automa.log("retreated", Party(Automa, n, l2, l1), "to", o.anchor)
                    Then(then)
                }
            }

        // HARVEST: one trade of 3 wood for 1 lore with 6 or more wood, one of 3 food for 1 lore with 6 or more food
        case TradeAction(Automa, then) =>
            if (Automa.wood >= 6) {
                Automa.wood -= 3
                Automa.lore += 1
                Automa.log("traded", 3.hl, Wood, "for", 1.hl, Lore)
            }
            if (Automa.food >= 6) {
                Automa.food -= 3
                Automa.lore += 1
                Automa.log("traded", 3.hl, Food, "for", 1.hl, Lore)
            }
            Then(then)

        // Events: the Automa takes no part in them
        case EventStepAction(step, Automa :: rest, then) =>
            Then(EventStepAction(step, rest, then))

        case _ => UnknownContinue
    }
}
