package root
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
import root.elem._

// The Outsiders Expansion's Old Man Tinker (slugfacekillah, v2.1; Law of Slug OMT and its faction board): a beaver from the future
// with three robots, factories that produce gears and drones, and the Old Man himself bustling about with the gears.
//
// Readings (the rules leave these open):
// - Robots: each robot is a pawn, and each gear on its R.S.I. is a warrior of its own (RobotGear) in the robot's clearing, so the
//   gears rule, deal and take hits and move as warriors do; a robot moves only with all its gears (a broken robot, with none, doesn't
//   move). A gear taken off by a hit (in battle or by the Breakage Die) becomes a gear token in the robot's clearing. The robots
//   themselves can't be hit or removed. Effects of other factions that move warriors may part a robot from its gears; they rejoin it
//   at the start of the Tinker's turn
// - Tokens: the twenty double-sided tokens are shared: gear tokens, drones and the robots' gears together never exceed twenty
// - The Old Man: a token in battle (removed, the remover scores 1 and the Tinker puts him in a forest next to that clearing); Bustle
//   moves him from a clearing to an adjacent clearing or forest, or from a forest to one of its clearings, ignoring rule, taking with
//   him up to three gear tokens from where he is (so they are picked up and dropped off on the way)
// - Production: the level goes from 0 to 2 (the board's track; the Breakage Dice are two); at level 0 a factory places nothing and
//   scores nothing
// - Meltdown: each removal of factories lowers the level by one (two factories are never in one clearing, so a battle removes one)
// - Craft: each gear token removed from the Old Man's clearing is one crafting piece of that clearing's suit; the gears used are
//   removed once the crafting is done
// - Build: a factory needs a free building slot, like any building
// - Do-Over: offered at the start of each turn; everything is removed and set up again, scored points are kept

abstract class Robot(val id0 : String, val name0 : String) extends Pawn with Movable with Tenacious {
    override def id = id0
    override def name = name0
}

case object Wolfbot extends Robot("wolfbot", "Wolfbot")
case object Birdbot extends Robot("birdbot", "Birdbot")
case object Owlbot extends Robot("owlbot", "Owlbot")

case class RobotGear(robot : Robot) extends Warrior {
    override def id = "rsi-gear"
    override def name = robot.name + " Gear"
}

case object Drone extends Warrior {
    override def id = "drone"
    override def name = "Drone"
}

case object Gear extends Token {
    override def id = "gear"
    override def name = "Gear"
}

case object OldMan extends Token {
    override def id = "old-man"
    override def name = "Old Man"
}

case object TinkerFactory extends Building {
    override def id = "factory"
    override def name = "Factory"
}

case object MadeOfMetal extends FactionEffect {
    val name = "Made of Metal"
}

case object SuperiorTechnology extends FactionEffect {
    val name = "Superior Technology"
}

case object Meltdown extends FactionEffect {
    val name = "Meltdown"
}

case object BreakageDie extends Elementary {
    def elem = "Breakage Die".styled(OMT)
}

trait OldManTinker extends WarriorFaction {
    val clashKey = OMT

    val warrior = Drone

    def abilities(options : $[Meta.O]) = $(DaylightCraft, MadeOfMetal, SuperiorTechnology, Meltdown)

    def pieces(options : $[Meta.O]) = Drone *** 20 ++ Gear *** 20 ++ OldMan *** 1 ++ TinkerFactory *** 3 ++
        TinkerRobots.robots./~(r => RobotGear(r) *** 3) ++ TinkerRobots.robots

    // the robots move only with all their gears
    override val transports : $[$[Transport]] = $($(RuledMove, RobotMove))

    override def note : Elem = SpacedDash ~ "V" ~ "2.1".hh ~ " by " ~ "slugfacekillah".hl ~ HorizontalBreak ~ "Outsiders Expansion (fan-made)"

    def advertising = Wolfbot.img(this) ~ Drone.img(this) ~ TinkerFactory.img(this)

    def motto = "Produce".styled(this)

    override def initialDamagePriorities(options : $[Meta.O]) = $(Drone, Gear, Wolfbot, Birdbot, Owlbot, TinkerFactory, OldMan)
}

case object OMT extends OldManTinker {
    val name = "Old Man Tinker"
    val short = "OMT"
    val style = "omt"
    val priority = "V''''''"
}

object TinkerRobots {
    val robots = $[Robot](Wolfbot, Birdbot, Owlbot)

    val die = Die.custom($[Robot](Wolfbot, Wolfbot, Birdbot, Birdbot, Owlbot, Owlbot))
}

case object RobotMove extends Transport {
    override def allows(f : Faction, o : Region, d : Region, l : $[Movable])(implicit game : Game) = allows(f, o, d) && TinkerRobots.robots.forall { r =>
        val n = l.count(RobotGear(r))
        if (l.has(r))
            n > 0 && n == f.at(o).count(RobotGear(r))
        else
            n == 0
    }
}

trait OMTDaylightQuestion extends FactionAction {
    override def self : OldManTinker

    def question(implicit game : Game) = self.elem ~ SpacedDash ~ Daylight.elem ~ Break ~
        Div(
            1.to(4)./(_ => Image("action-black", styles.action, "")).take(self.acted) ~
            1.to(4)./(_ => Image(self.style + "-action", styles.action)).drop(self.acted),
            styles.margined)
}

// SETUP
case class OMTMaterializeAction(self : OldManTinker, c : Clearing, r : Forest) extends BaseAction("Materialize the robots in a clearing with the fewest pieces, and", OldMan.of(self), "in a forest next to it")(c, "and", r)
case class OMTDoOverAction(self : OldManTinker) extends BaseAction(Birdsong, "start")("Do-Over".styled(self), "(remove every piece and set up again)")

// BIRDSONG
case class OMTRolledAction(self : OldManTinker, rolled : $[Robot]) extends RolledAction[Robot]
case class OMTBustleAction(self : OldManTinker, o : Region, d : Region, n : Int) extends BaseAction("Bustle".styled(self), "(" ~ "Old Man".styled(self) ~ " moves)")(implicit g => d.elem(g) ~ (n > 0).?(" with " ~ n.hl ~ " " ~ (n == 1).?("gear").|("gears")).|(Empty))

// DAYLIGHT
case class OMTCrusadeMainAction(self : OldManTinker) extends OptionAction("Crusade".styled(self), dt.Battle) with OMTDaylightQuestion with Soft
case class OMTCrusadeMovedAction(self : OldManTinker) extends ForcedAction
case class OMTTinkerMainAction(self : OldManTinker, l : $[(Int, Int)]) extends OptionAction("Tinker".styled(self), "(flip up to 3 tokens)") with OMTDaylightQuestion with Soft
case class OMTTinkerAction(self : OldManTinker, r : Region, toDrone : Int, toGear : Int) extends BaseAction("Tinker".styled(self), "in")(implicit g => r.elem(g) ~ ": " ~ (toDrone.hl ~ " gears to drones, " ~ toGear.hl ~ " drones to gears"))
case class OMTRepairMainAction(self : OldManTinker, l : $[(Robot, Int)]) extends OptionAction("Repair".styled(self), "(gears onto a robot)") with OMTDaylightQuestion with Soft
case class OMTRepairAction(self : OldManTinker, r : Robot, n : Int) extends BaseAction("Repair".styled(self))(implicit g => r.of(self) ~ " with " ~ n.hl ~ " " ~ (n == 1).?("gear").|("gears"))
case class OMTOverproduceMainAction(self : OldManTinker, l : $[Clearing]) extends OptionAction("Overproduce".styled(self), "(a card matching a factory)") with OMTDaylightQuestion with Soft
case class OMTOverproduceAction(self : OldManTinker, c : Clearing, d : DeckCard) extends BaseAction("Overproduce".styled(self), "in")(c, "spending", d.img) with ViewCard
case class OMTDoneAction(self : OldManTinker) extends ForcedAction
case class OMTBuildAction(self : OldManTinker, c : Clearing) extends BaseAction("Build".styled(self), TinkerFactory.of(self))(c)

// EVENING
case class OMTRestructureAction(self : OldManTinker, d : DeckCard, k : Int) extends BaseAction("Restructure".styled(self), "spending a card matching a factory")(implicit g => d.elem(g) ~ ": " ~ (k @@ {
    case 1 => "raise the production level"
    case -1 => "lower the production level"
    case _ => "flip the production marker"
}).txt) with ViewCard
case class OMTTakeDiscardAction(self : OldManTinker, d : DeckCard) extends BaseAction("Draw".styled(self), "or take a card from the discard pile matching", "Old Man".styled(self) ~ "'s clearing")(d.img) with ViewCard
case class OMTDrawTwoAction(self : OldManTinker) extends BaseAction("Draw".styled(self), "or take a card from the discard pile matching", "Old Man".styled(self) ~ "'s clearing")("Draw", 2.cards)


class OldManTinkerPlayer(val faction : OldManTinker)(implicit val game : Game) extends FactionState {
    // the Old Man and the gears he carries can be in forests
    game.board.forests.foreach(r => location(r))

    var level = 1
    var drones = false
    var acted = 0
    var craftPaid = 0
    var crusading = false
    var dest : |[Clearing] = None
    var rolled = false
    var produced = false
    var bustled = 0
    var built = false

    def regions : $[Region] = clearings ++ game.board.forests

    def onMap(p : Piece) = regions./(r => at(r).count(p)).sum

    def oldMan : |[Region] = regions.%(r => at(r).has(OldMan)).headOption

    def oldManClearing : |[Clearing] = oldMan./~(_.as[Clearing])

    def robotAt(r : Robot) : |[Clearing] = clearings.%(c => at(c).has(r)).headOption

    def rsi(r : Robot) = all(RobotGear(r)).num

    def tokensLeft = 20 - onMap(Gear) - onMap(Drone) - TinkerRobots.robots./(rsi).sum

    def craft = oldManClearing./~(c => at(c).count(Gear).times(c.asset))

    def factories = all(TinkerFactory)
}


object OldManTinkerExpansion extends FactionExpansion[OldManTinker] {
    def tinkers(implicit game : Game) = factions.of[OldManTinker].%(game.states.contains)

    // robots are never hit
    def hittable(p : Piece) = p.is[Robot].not

    def forestsNear(c : Clearing)(implicit game : Game) = game.board.forests.%(r => game.board.fromForest(r).has(c))

    def bustleTo(f : OldManTinker, o : Region)(implicit game : Game) : $[Region] = o match {
        case o : Clearing => game.connected(o) ++ forestsNear(o)
        case o : Forest => game.board.fromForest(o)
        case _ => $
    }

    def buildable(f : OldManTinker)(implicit game : Game) = f.pool(TinkerFactory).??(clearings.%(c => f.rules(c) && f.canPlace(c) && f.canBuild(c) && f.at(c).has(TinkerFactory).not &&
        game.connected(c).count(d => f.at(d).has(TinkerFactory)) <= 1 &&
        game.connected(c).%(d => f.at(d).has(TinkerFactory)).forall(d => game.connected(d).count(x => f.at(x).has(TinkerFactory)) == 0)))

    def factoryCards(f : OldManTinker)(implicit game : Game) = f.hand.%(d => f.factories.exists(c => d.matches(c.cost)))

    def tinkerings(f : OldManTinker)(implicit game : Game) : $[(Int, Int)] = f.oldMan./~(r =>
        0.to(min(3, f.at(r).count(Gear)))./~(a => 0.to(min(3 - a, f.at(r).count(Drone)))./(b => (a, b))).%{ case (a, b) => a + b > 0 })

    def repairs(f : OldManTinker)(implicit game : Game) : $[(Robot, Int)] = f.oldManClearing./~(c => TinkerRobots.robots.%(r => f.at(c).has(r))./~(r =>
        1.to(min(3, min(f.at(c).count(Gear), 3 - f.rsi(r))))./(n => (r, n))))

    def parted(f : OldManTinker)(implicit game : Game) : $[(Robot, Clearing, Clearing)] = TinkerRobots.robots./~(r => f.robotAt(r)./~(c =>
        clearings.but(c).%(x => f.at(x).has(RobotGear(r)))./(x => (r, x, c))))

    def materialize(f : OldManTinker)(implicit game : Game) = {
        val l = clearings.%(f.canPlace)
        val n = l./(c => factions./(e => e.at(c).num).sum).minOr(0)
        l.%(c => factions./(e => e.at(c).num).sum == n)./~(c => forestsNear(c)./(r => OMTMaterializeAction(f, c, r)))
    }

    def produce(f : OldManTinker, c : Clearing, score : Boolean)(implicit game : Game) {
        val p = f.drones.?(Drone).|(Gear)
        val k = min(f.level, f.tokensLeft)

        f.reserve --> k.times(p) --> c

        if (k > 0)
            f.log("produced", k.hl, (k == 1).?(p.name).|(p.name + "s").styled(f), "at the", TinkerFactory.of(f), "in", c)

        if (score && f.level > 0 && k == f.level)
            f.oscore(1)("producing in", c)
    }

    // a hit on a robot: its leftmost gear falls into its clearing
    def breakGear(f : OldManTinker, r : Robot)(implicit game : Game) {
        f.robotAt(r).foreach { c =>
            if (f.at(c).has(RobotGear(r))) {
                f.from(c) --> RobotGear(r) --> f.reserve
                f.reserve --> Gear --> c

                f.log("lost a gear from", r.of(f), "in", c, (f.rsi(r) == 0).?("and it is broken"))
            }
        }
    }

    def perform(action : Action, soft : Void)(implicit game : Game) = action @@ {
        // SETUP
        case CreatePlayerAction(f : OldManTinker) =>
            game.states += f -> new OldManTinkerPlayer(f)

            FactionInitAction(f)

        case FactionSetupAction(f : OldManTinker) =>
            Ask(f).group(f, "materializes").add(materialize(f))

        case OMTMaterializeAction(f, c, r) =>
            game.highlights :+= PlaceHighlight($(c))

            TinkerRobots.robots.foreach { b =>
                f.reserve --> b --> c
                f.reserve --> 2.times(RobotGear(b)) --> c
            }

            f.reserve --> OldMan --> r

            f.level = 1
            f.drones = false

            f.log("materialized the robots in", c, "with two gears each, and", OldMan.of(f), "in", r)

            if (f.used.has(DoOverUsed))
                Next
            else
                SetupFactionsAction

        // DO-OVER
        case BirdsongNAction(5, f : OldManTinker) =>
            soft()

            Ask(f).add(OMTDoOverAction(f)).skip(Next).birdsong(f)

        // gears parted from their robot rejoin it
        case BirdsongNAction(15, f : OldManTinker) if parted(f).any =>
            parted(f).foreach { case (r, x, c) =>
                val n = f.at(x).count(RobotGear(r))
                f.from(x) --> n.times(RobotGear(r)) --> c
                f.log("moved", n.hl, "gears of", r.of(f), "back to it in", c)
            }

            Next

        case OMTDoOverAction(f) =>
            f.used :+= DoOverUsed

            f.regions.foreach { r =>
                f.from(r) --> f.reserve
            }

            f.log("started over with a", "Do-Over".styled(f))

            Ask(f).group(f, "materializes again").add(materialize(f))

        // ROLL
        case BirdsongNAction(20, f : OldManTinker) if f.rolled.not && f.level > 0 =>
            f.rolled = true

            Roll[Robot](f.level.times(TinkerRobots.die), l => OMTRolledAction(f, l), f.elem ~ " rolls the " ~ BreakageDie.elem)

        case OMTRolledAction(f, l) =>
            f.log("rolled", l./(_.of(f)).comma, "on the", BreakageDie.elem)

            l.distinct.foreach(r => breakGear(f, r))

            Next

        // PRODUCE
        case BirdsongNAction(30, f : OldManTinker) if f.produced.not =>
            f.produced = true

            f.factories.foreach(c => produce(f, c, true))

            Next

        // BUSTLE
        case BirdsongNAction(40, f : OldManTinker) if f.bustled < 3 && f.oldMan.any =>
            soft()

            val o = f.oldMan.get
            val g = min(3, f.at(o).count(Gear))

            Ask(f).group("Bustle".styled(f), "(" ~ (3 - f.bustled).hl ~ " moves left)")
                .each(bustleTo(f, o)./~(d => 0.to(g).reverse./(n => (d, n)))) { case (d, n) => OMTBustleAction(f, o, d, n) }
                .done(Next)
                .birdsong(f)

        case OMTBustleAction(f, o, d, n) =>
            f.from(o) --> OldMan --> d
            f.from(o) --> n.times(Gear) --> d

            f.bustled += 1

            f.log("bustled", OldMan.of(f), "from", o, "to", d, (n > 0).?("with"), (n > 0).?(n.hl), (n > 0).?((n == 1).?("gear").|("gears")))

            Repeat

        // DAYLIGHT
        case DaylightNAction(20, f : OldManTinker) =>
            XCraftMainAction(f)

        case DaylightNAction(30, f : OldManTinker) if f.crafted.num > f.craftPaid =>
            val n = f.crafted.num - f.craftPaid
            f.craftPaid = f.crafted.num

            f.oldManClearing.foreach { c =>
                val k = min(n, f.at(c).count(Gear))
                f.from(c) --> k.times(Gear) --> f.reserve
                f.log("removed", k.hl, (k == 1).?("gear").|("gears"), "from", c, "for crafting")
            }

            Next

        case DaylightNAction(40, f : OldManTinker) =>
            implicit val ask = builder

            if (f.acted < 4) {
                val bb = clearings.%(f.canAttackIn)
                + OMTCrusadeMainAction(f).!(f.moveFrom.none && bb.none)

                val tt = tinkerings(f)
                + OMTTinkerMainAction(f, tt).!(tt.none)

                val rr = repairs(f)
                + OMTRepairMainAction(f, rr).!(rr.none)

                val oo = f.factories.%(c => f.hand.exists(_.matches(c.cost)))
                + OMTOverproduceMainAction(f, oo).!(oo.none).!(f.level == 0, "production level 0")
            }

            + EndTurnSoftAction(f, "Actions", ForfeitActions(4 - f.acted))

            ask(f).daylight(f)

        case OMTCrusadeMainAction(f) =>
            val bb = clearings.%(f.canAttackIn)

            Ask(f).group("Crusade".styled(f))
                .when(f.moveFrom.any)(MoveInitAction(f, f, $, NoMessage, f.moveFrom, f.movable, $(CancelAction), OMTCrusadeMovedAction(f)).as("Move, then battle"))
                .when(bb.any)(BattleInitAction(f, f, NoMessage, bb, $(CancelAction), OMTDoneAction(f)).as("Battle"))
                .cancel

        // the last destination, for a Crusade's battle
        case MoveFinishedAction(f : OldManTinker, o, d : Clearing, then) if f.dest.has(d).not =>
            f.dest = Some(d)

            MoveFinishedAction(f, o, d, then)

        case OMTCrusadeMovedAction(f) =>
            f.dest.filter(d => f.canAttackIn(d)) match {
                case Some(d) =>
                    BattleInitAction(f, f, NoMessage, $(d), $(OMTDoneAction(f).as("Skip the battle")), OMTDoneAction(f))
                case None =>
                    OMTDoneAction(f)
            }

        case OMTTinkerMainAction(f, l) =>
            Ask(f).each(l) { case (a, b) => OMTTinkerAction(f, f.oldMan.get, a, b) }.cancel

        case OMTTinkerAction(f, r, a, b) =>
            f.from(r) --> a.times(Gear) --> f.reserve
            f.from(r) --> b.times(Drone) --> f.reserve
            f.reserve --> a.times(Drone) --> r
            f.reserve --> b.times(Gear) --> r

            f.log("tinkered in", r, Comma, "turning", a.hl, "gears into drones and", b.hl, "drones into gears")

            OMTDoneAction(f)

        case OMTRepairMainAction(f, l) =>
            Ask(f).each(l) { case (r, n) => OMTRepairAction(f, r, n) }.cancel

        case OMTRepairAction(f, r, n) =>
            val c = f.robotAt(r).get

            f.from(c) --> n.times(Gear) --> f.reserve
            f.reserve --> n.times(RobotGear(r)) --> c

            f.log("repaired", r.of(f), "in", c, "with", n.hl, (n == 1).?("gear").|("gears"))

            OMTDoneAction(f)

        case OMTOverproduceMainAction(f, l) =>
            Ask(f).each(l./~(c => f.hand.%(_.matches(c.cost))./(d => (c, d)))) { case (c, d) => OMTOverproduceAction(f, c, d) }.cancel

        case OMTOverproduceAction(f, c, d) =>
            f.hand --> d --> discard.quiet

            f.log("overproduced in", c, "spending", d)

            produce(f, c, false)

            OMTDoneAction(f)

        case OMTDoneAction(f) =>
            f.crusading = false
            f.dest = None
            f.acted += 1

            Repeat

        // BUILD
        case DaylightNAction(60, f : OldManTinker) =>
            soft()

            val l = buildable(f)

            if (l.none)
                NoAsk(f)(Next)
            else
                Ask(f).group("Build".styled(f), TinkerFactory.of(f)).each(l)(c => OMTBuildAction(f, c)).done(Next).daylight(f)

        case OMTBuildAction(f, c) =>
            game.highlights :+= PlaceHighlight($(c))

            f.reserve --> TinkerFactory --> c

            f.log("built", TinkerFactory.of(f), "in", c)

            Repeat

        // nothing to hit: only robots
        case BattleAssignHitsAction(f : OldManTinker, b, n, s, then) if n + s > 0 && f.at(b.clearing).forall(p => hittable(p).not) =>
            then

        // MELTDOWN
        case ForcedRemoveTargetEffectAction(e, c, f : OldManTinker, TinkerFactory, then) =>
            if (f.level > 0) {
                f.level -= 1

                f.log("lowered the production level to", f.level.hl, "with", Meltdown.of(f))
            }

            then

        // MADE OF METAL: a gear knocked off a robot falls into its clearing
        case ForcedRemoveTargetEffectAction(e, c, f : OldManTinker, p : RobotGear, then) =>
            f.limbo(c) --> p --> f.reserve
            f.reserve --> Gear --> c

            f.log("dropped a", Gear.of(f), "from", p.robot.of(f), "in", c, (f.rsi(p.robot) == 0).?("and it is broken"))

            then

        // THE OLD MAN: back to a forest next to his clearing
        case ForcedRemoveTargetEffectAction(e, c, f : OldManTinker, OldMan, then) =>
            val l = forestsNear(c)

            if (l.none)
                then
            else
                Ask(f).group(OldMan.of(f), "goes to a forest next to", c).each(l)(r => OMTOldManBackAction(f, c, r, then))

        case OMTOldManBackAction(f, c, r, then) =>
            f.limbo(c) --> OldMan --> r

            f.log("put", OldMan.of(f), "in", r)

            then

        // EVENING
        case EveningNAction(20, f : OldManTinker) =>
            soft()

            val l = factoryCards(f)

            if (l.none)
                NoAsk(f)(Next)
            else
                Ask(f).group("Restructure".styled(f), "(level", f.level.hl ~ ",", f.drones.?("drones").|("gears") ~ ")")
                    .each(l./~(d => $(1, -1, 0)./(k => (d, k)))) { case (d, k) => OMTRestructureAction(f, d, k).!(k == 1 && f.level >= 2).!(k == -1 && f.level <= 0) }
                    .done(Next)
                    .evening(f)

        case OMTRestructureAction(f, d, k) =>
            f.hand --> d --> discard.quiet

            if (k == 0)
                f.drones = f.drones.not
            else
                f.level += k

            if (k == 0)
                f.log("restructured spending", d, "and turned the production marker to", f.drones.?("drones").|("gears").hh)
            else
                f.log("restructured spending", d, "and set the production level to", f.level.hl)

            Repeat

        case NightStartAction(f : OldManTinker) =>
            val l = f.oldManClearing./~(c => pile.%(_.matches(c.cost)))

            if (l.none)
                EveningDrawAction(f, 2)
            else
                Ask(f).add(OMTDrawTwoAction(f)).each(l)(d => OMTTakeDiscardAction(f, d))

        case OMTDrawTwoAction(f) =>
            EveningDrawAction(f, 2)

        case OMTTakeDiscardAction(f, d) =>
            pile --> d --> f.hand

            f.log("took", d, "from the discard pile")

            EveningDrawAction(f, 0)

        case FactionCleanUpAction(f : OldManTinker) =>
            f.acted = 0
            f.craftPaid = 0
            f.crusading = false
            f.dest = None
            f.rolled = false
            f.produced = false
            f.bustled = 0
            f.built = false

            CleanUpAction(f)

        case _ => UnknownContinue
    }
}

case object DoOverUsed extends HiddenEffect

case class OMTOldManBackAction(self : OldManTinker, c : Clearing, r : Forest, then : ForcedAction) extends BaseAction(OldMan.of(self), "goes to a forest next to", c)(r)
