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

// The Noxious Battery (S.P. Shaman, Nemesis Expansion, art by disasterman; Fan Law of Root NB on the Root Database and its faction
// board): skunks with a rusty cannon, scoring by firing cannonballs into the clearings around it and spraying scent marks that drive
// the other players' warriors out of their clearings.
//
// Readings (the rules leave these open):
// - The Bore: three spaces scoring 3, 2 and 1 left to right, filled from the right; a fire takes the leftmost loaded cannonball, so it
//   scores as many points as there are cannonballs loaded
// - Scent Marks track: its five spaces, left to right, show nothing, a draw, a load, nothing and a load, uncovered as marks are placed
//   (from the left) and covered again when a mark comes back (on the rightmost empty space)
// - Levy: a warrior in the cannon's clearing, then one at each cannonball token on the map, while the supply lasts
// - Fire!: each removal is an enemy warrior there, or, once no enemy has a warrior there, another enemy piece, as the Battery
//   picks ("warriors first"); pieces are removed as if defending in battle (the Lizard Cult gets acolytes), and buildings and tokens score as usual
// - Foul Stench: at the end of an enemy's Evening, it moves all its warriors and pawns in a clearing with a scent mark together to an
//   adjacent clearing without one, one clearing at a time, until none is left in a scent mark's clearing or none can go anywhere (the
//   bots' Clockwork Nose made the rule for everyone: a clearing with a scent mark is never a destination, so it always ends)
// - The cannon: a pawn, taking hits only after the Battery's warriors there; damaged, it isn't hit, removed or battled alone; an enemy
//   removing it undamaged flips it, scores 1 and moves each loaded cannonball to an adjacent clearing of its choice
// - Keep on Rollin': a cannonball another player removes goes to an adjacent clearing of that player's choice, scoring nothing
// - Show of Force: a move with the cannon ignores rule; Rollout moves one cannonball with each warrior moved
// - Rearm: the card pays for returning any number of the cannonballs in the cannon's clearing (picked) to the supply
// - Spray: with fewer scent marks left than clearings to mark, the Battery picks them one at a time

case object Skunk extends Warrior {
    override def id = "skunk"
    override def name = "Skunk"
}

case object Cannon extends Pawn with Movable {
    override def id = "cannon"
    override def name = "Cannon"
    override def imgid(f : Faction) = f.style + "-cannon"
}

case object Cannonball extends Token with Movable {
    override def id = "cannonball"
    override def name = "Cannonball"
}

case object ScentMark extends Token {
    override def id = "scent"
    override def name = "Scent Mark"
}

case object TheBore extends SpecialRegion {
    override def name = "The Bore"
}

case object FoulStench extends FactionEffect {
    val name = "Foul Stench"
}

case object ShowOfForce extends FactionEffect {
    val name = "Show of Force"
}

case object KeepOnRollin extends FactionEffect {
    val name = "Keep on Rollin'"
}

case object Corroded extends FactionEffect {
    val name = "Corroded"
}

// Rollout: one cannonball with each warrior moved
case object CannonballMove extends Transport {
    override def allows(f : Faction, o : Region, d : Region, l : $[Movable])(implicit game : Game) = allows(f, o, d) && l.count(Cannonball) <= l.of[Warrior].num

    override def order(l : $[Movable]) : Int = l.has(Cannon).??(90) + l.of[Warrior].num * 9 + l.num
    override def sortBy(m : Movable) : Int = (m == Cannon).??(-2) + (m == Skunk).??(-1) + (m == Cannonball).??(1)
}

trait NoxiousBattery extends WarriorFaction {
    val clashKey = NX

    val warrior = Skunk

    def abilities(options : $[Meta.O]) = $(DaylightCraft, FoulStench, ShowOfForce, KeepOnRollin, Corroded)

    override val transports : $[$[Transport]] = $($(RuledMove, CannonballMove))

    def pieces(options : $[Meta.O]) = Skunk *** 15 ++ Cannon *** 1 ++ Cannonball *** 3 ++ ScentMark *** 5

    override def note : Elem = SpacedDash ~ "by " ~ "S.P. Shaman".hl ~ HorizontalBreak ~ "Nemesis Expansion (fan-made)"

    def advertising = Cannon.img(this) ~ Skunk.img(this) ~ ScentMark.img(this)

    def motto = "Fire".styled(this)

    override def initialDamagePriorities(options : $[Meta.O]) = $(Skunk, Cannonball, ScentMark, Cannon)
}

case object NX extends NoxiousBattery {
    val name = "Noxious Battery"
    val short = "NX"
    val style = "nx"
    val priority = "V'''''''"
}

object NoxiousBattery {
    // the Scent Marks track's spaces, left to right: (draws, loads)
    val track = $((0, 0), (1, 0), (0, 1), (0, 0), (0, 1))

    // the Bore's spaces, left to right
    val bore = $(3, 2, 1)
}

trait NXDaylightQuestion extends FactionAction {
    override def self : NoxiousBattery

    def question(implicit game : Game) = self.elem ~ SpacedDash ~ Daylight.elem ~ Break ~
        Div(
            1.to(4)./(_ => Image("action-black", styles.action, "")).take(self.acted) ~
            1.to(4)./(_ => Image(self.style + "-action", styles.action)).drop(self.acted),
            styles.margined)
}

case class NXSetupAction(self : NoxiousBattery, c : Clearing) extends BaseAction(self, "places", Cannon.of(self), "in")(c)

case class NXLevyAction(self : NoxiousBattery) extends ForcedAction

case class NXFireAction(self : NoxiousBattery, c : Clearing, vp : Int) extends BaseAction("Fire!".styled(self), "(" ~ vp.vp ~ ")")(c)
case class NXFireHitAction(self : NoxiousBattery, c : Clearing, left : Int) extends ForcedAction
case class NXFireRemoveAction(self : NoxiousBattery, c : Clearing, e : Faction, p : Piece, left : Int) extends BaseAction("Fire!".styled(self), "removes in", c)(p.of(e))
case class NXFireDamageAction(self : NoxiousBattery, c : Clearing, e : Hero) extends BaseAction("Fire!".styled(self), "removes in", c)(e, "damages an item")

case class NXRecruitAction(self : NoxiousBattery, c : Clearing) extends OptionAction("Recruit".styled(self), Skunk.imgd(self)) with NXDaylightQuestion
case class NXBattleAction(self : NoxiousBattery, l : $[Clearing]) extends OptionAction("Battle".styled(self), dt.Battle) with NXDaylightQuestion with Soft with Only[BattleStartAction] { def tag = implicitly }
case class NXRolloutAction(self : NoxiousBattery) extends OptionAction("Rollout".styled(self), dt.Move) with NXDaylightQuestion with Soft with Only[MoveListAction] { def tag = implicitly }
case class NXRearmAction(self : NoxiousBattery, c : Clearing) extends OptionAction("Rearm".styled(self), Cannonball.imgd(self)) with NXDaylightQuestion with Soft
case class NXRearmCardAction(self : NoxiousBattery, c : Clearing, d : DeckCard) extends BaseAction("Rearm".styled(self), "in", c, "with")(d.img) with ViewCard with Soft
case class NXRearmReturnAction(self : NoxiousBattery, c : Clearing, d : DeckCard, n : Int) extends BaseAction("Rearm".styled(self), "in", c, "with", d)("Return", n.hl, (n == 1).?("cannonball").|("cannonballs"))
case class NXDoneAction(f : NoxiousBattery, then : ForcedAction) extends ForcedAction

case class NXSprayAction(self : NoxiousBattery) extends ForcedAction
case class NXSprayPickAction(self : NoxiousBattery, c : Clearing) extends BaseAction("Spray".styled(self), ScentMark.of(self), "in")(c)

case class NXStenchAction(self : Faction, t : NoxiousBattery, c : Clearing, d : Clearing) extends BaseAction(FoulStench.of(t), "move every warrior and pawn from", c, "to")(d)

case class NXRollAction(self : Faction, t : NoxiousBattery, c : Clearing, d : Clearing, then : ForcedAction) extends BaseAction(KeepOnRollin.of(t), Cannonball.of(t), "from", c, "rolls to")(d)
case class NXSpillAction(self : Faction, t : NoxiousBattery, c : Clearing, n : Int, then : ForcedAction) extends ForcedAction
case class NXSpillToAction(self : Faction, t : NoxiousBattery, c : Clearing, d : Clearing, n : Int, then : ForcedAction) extends BaseAction(Corroded.of(t), Cannonball.of(t), "from", TheBore.name.styled(t), "rolls to")(d)


class NoxiousBatteryPlayer(val faction : NoxiousBattery)(implicit val game : Game) extends FactionState {
    val bore = location(TheBore, u => u.faction == faction && u.piece == Cannonball)

    var acted = 0
    var damaged = true

    def cannon : |[Clearing] = all(Cannon).single

    def marks = all(ScentMark).distinct

    def craft = marks./(_.asset)

    def spaces = NoxiousBattery.track.take(5 - pooled(ScentMark))

    def draws = 1 + spaces.map(_._1).sum

    def loads = 1 + spaces.map(_._2).sum

    // the points of the next fire: the leftmost loaded space
    def nextVP = NoxiousBattery.bore.drop(3 - bore.num).starting.|(0)
}


object NoxiousBatteryExpansion extends FactionExpansion[NoxiousBattery] {
    def batteries(implicit game : Game) = factions.of[NoxiousBattery].%(game.states.contains)

    def cannonAt(f : Faction, o : Region)(implicit game : Game) = f @@ {
        case f : NoxiousBattery => game.states.contains(f) && f.at(o).has(Cannon)
        case _ => false
    }

    // a damaged cannon is never hit
    def hittable(x : Figure)(implicit game : Game) = x.piece != Cannon || (x.faction @@ {
        case f : NoxiousBattery => f.damaged.not
        case _ => true
    })

    // a damaged cannon alone in its clearing can't be battled
    def untouchable(e : Faction, c : Clearing)(implicit game : Game) = e @@ {
        case e : NoxiousBattery => e.damaged && e.at(c).any && e.at(c).forall(_ == Cannon)
        case _ => false
    }

    def scented(c : Region)(implicit game : Game) = batteries.exists(_.at(c).has(ScentMark))

    def movers(f : Faction, c : Region)(implicit game : Game) = f.at(c).of[Movable].%(p => p.is[Warrior] || p.is[Pawn])

    // Foul Stench: the moves still owed by f, a clearing with a scent mark where it has warriors or pawns and a clearing next to it without one
    def stench(f : Faction)(implicit game : Game) =
        clearings.%(scented).%(c => movers(f, c).any).%(c => f.canMoveFrom(c))./~(c => game.connected(c).%!(scented).%(f.validDest.has)./(d => (c, d)))

    def fireable(f : NoxiousBattery)(implicit game : Game) =
        f.cannon.$./~(game.connected).%(c => f.enemies.exists(e => e.at(c).exists(p => p.is[Tenacious].not && BonePatrolExpansion.hittable(p))))

    // what one cannonball hit can take: the enemy warriors there, or other enemy pieces once no enemy warrior is left
    def targets(f : NoxiousBattery, c : Clearing)(implicit game : Game) : $[(Faction, Piece)] = {
        val l = f.enemies.%(e => f.canRemove(c)(e))./~(e => e.at(c).%(p => p.is[Tenacious].not && BonePatrolExpansion.hittable(p) && OldManTinkerExpansion.hittable(p) && hittable(Figure(e, p, -1))).distinct./(p => (e, p)))
        val w = l.%{ case (_, p) => p.is[Warrior] }
        w.any.?(w).|(l)
    }

    def perform(action : Action, soft : Void)(implicit game : Game) = action @@ {
        // SETUP
        case CreatePlayerAction(f : NoxiousBattery) =>
            game.states += f -> new NoxiousBatteryPlayer(f)

            FactionInitAction(f)

        case FactionSetupAction(f : NoxiousBattery) =>
            val all = clearings.%(f.canPlace)
            val ruined = all.%(game.ruins.contains)
            val l = ruined.diff(options.has(SetupTypeHomelands).??(game.homelands)).some.|(ruined).some.|(all)

            Ask(f).each(l)(c => NXSetupAction(f, c)).needOk

        case NXSetupAction(f, c) =>
            game.homelands :+= c

            f.reserve --> Cannon --> c

            f.damaged = true

            f.log("placed", Cannon.of(f), "in", c, "damaged side up")

            SetupFactionsAction

        // THE CANNON
        case ForcedRemoveAction(e, c, f : NoxiousBattery, Cannon, n, m, then) if e != f && f.damaged.not =>
            f.damaged = true

            e.oscore(1)("damaging", Cannon.of(f))

            e.log("damaged", Cannon.of(f), "in", c, "with", Corroded.of(f))

            NXSpillAction(e, f, c, f.bore.num, then)

        case ForcedRemoveAction(e, c, f : NoxiousBattery, Cannon, n, m, then) =>
            f.log("kept", Cannon.of(f), "in", c)

            then

        case NXSpillAction(e, f, c, n, then) =>
            val l = game.connected(c)

            if (n <= 0 || f.bore.none || l.none)
                then
            else
                Ask(e).each(l)(d => NXSpillToAction(e, f, c, d, n, then))

        case NXSpillToAction(e, f, c, d, n, then) =>
            f.bore --> Cannonball --> d

            e.log("rolled", Cannonball.of(f), "from", TheBore.name.styled(f), "to", d)

            NXSpillAction(e, f, c, n - 1, then)

        // nothing left to hit: only a damaged cannon
        case BattleAssignHitsAction(f : NoxiousBattery, b, n, s, then) if n + s > 0 && f.damaged && f.at(b.clearing).forall(_ == Cannon) =>
            then

        // KEEP ON ROLLIN'
        case ForcedRemoveTargetEffectAction(e, c, f : NoxiousBattery, Cannonball, then) if e != f =>
            val l = game.connected(c)

            f.limbo(c) --> Cannonball --> c

            then match {
                case ForcedRemoveScoreAction(_, _, _, _, _, _, next) =>
                    if (l.none)
                        next
                    else
                        Ask(e).each(l)(d => NXRollAction(e, f, c, d, next))
                case _ =>
                    if (l.none)
                        then
                    else
                        Ask(e).each(l)(d => NXRollAction(e, f, c, d, then))
            }

        case NXRollAction(e, f, c, d, then) =>
            f.from(c) --> Cannonball --> d

            e.log("rolled", Cannonball.of(f), "from", c, "to", d)

            then

        // FOUL STENCH
        case CleanUpAction(f) if f.is[NoxiousBattery].not && game.states.contains(f) && batteries.exists(_.friends(f).not) && stench(f).any =>
            val t = batteries.%(_.friends(f).not).first

            Ask(f).each(stench(f)) { case (c, d) => NXStenchAction(f, t, c, d) }

        case NXStenchAction(f, t, c, d) =>
            val l = movers(f, c)

            f.from(c) --> l --> d

            f.log("moved", l./(_.of(f)).comma, "from", c, "to", d, "for", FoulStench.of(t))

            CleanUpAction(f)

        // BIRDSONG
        case BirdsongNAction(20, f : NoxiousBattery) =>
            NXLevyAction(f)

        case NXLevyAction(f) =>
            val l = f.cannon.$ ++ clearings./~(c => f.at(c).count(Cannonball).times(c))
            val ll = l.%(f.canPlace).take(f.pooled(Skunk))

            if (ll.any) {
                ll.foreach(c => f.reserve --> Skunk --> c)

                f.log("levied", Skunk.sof(f), "in", ll./(_.elem).comma)
            }

            Next

        case BirdsongNAction(40, f : NoxiousBattery) =>
            soft()

            val l = fireable(f)

            if (f.bore.none || l.none)
                NoAsk(f)(Next)
            else
                Ask(f).each(l)(c => NXFireAction(f, c, f.nextVP)).done(Next).birdsong(f)

        case NXFireAction(f, c, vp) =>
            game.highlights :+= NukeHighlight($(c))

            f.bore --> Cannonball --> c

            f.nscore(vp)("firing")(f, "fired", Cannon.of(f), "at", c, ForVP)

            NXFireHitAction(f, c, f.at(c).count(Cannonball))

        case NXFireHitAction(f, c, left) =>
            val l = targets(f, c)
            val h = f.enemies.of[Hero].%(_.at(c).has(Vagabond))

            if (left <= 0 || (l.none && h.none))
                Repeat
            else
                Ask(f)
                    .each(l) { case (e, p) => NXFireRemoveAction(f, c, e, p, left) }
                    .each(h)(e => NXFireDamageAction(f, c, e))

        case NXFireRemoveAction(f, c, e, p, left) =>
            // as if defending in battle
            if (e.is[Fanatic] && e.used.has(Revenge).not)
                e.used :+= Revenge

            f.log("hit", p.of(e), "in", c)

            TryForcedRemoveAction(f, c, e, p, p.is[Scoring].??(1), Removing, ForcedRemoveFinishedAction(f, NXFireHitAction(f, c, left - 1)), ForcedRemoveFinishedAction(f, NXFireHitAction(f, c, left - 1)))

        case NXFireDamageAction(f, c, e) =>
            f.log("hit", e, "in", c)

            BattleAssignHitsAction(e, Battle(c, f, None, e, None, None, None, 0, 0, 0, Repeat), 1, 0, ForcedRemoveFinishedAction(f, Repeat))

        case BirdsongNAction(60, f : NoxiousBattery) if f.damaged && f.cannon.any =>
            f.damaged = false

            f.log("repaired", Cannon.of(f))

            Next

        // DAYLIGHT
        case DaylightNAction(20, f : NoxiousBattery) =>
            XCraftMainAction(f)

        case DaylightNAction(60, f : NoxiousBattery) =>
            implicit val ask = builder

            if (f.acted < 4) {
                f.cannon.foreach { c =>
                    + NXRecruitAction(f, c).!(f.pool(Skunk).not, "max").!(f.canPlace(c).not, "can't place")
                }

                val att = clearings.%(f.canAttackIn)
                + NXBattleAction(f, att).!(att.none)

                + NXRolloutAction(f).!(f.moveFrom.none)

                f.cannon.foreach { c =>
                    + NXRearmAction(f, c).!(f.at(c).has(Cannonball).not, "no cannonball").!(f.hand.exists(_.matches(c.cost)).not, "no card")
                }
            }

            + EndTurnSoftAction(f, "Turn", ForfeitActions(4 - f.acted))

            ask(f).daylight(f)

        case NXRecruitAction(f, c) =>
            game.highlights :+= PlaceHighlight($(c))

            f.reserve --> Skunk --> c

            f.log("recruited", Skunk.of(f), "in", c)

            NXDoneAction(f, Repeat)

        case NXBattleAction(f, l) =>
            BattleInitAction(f, f, NoMessage, l, $(CancelAction), NXDoneAction(f, Repeat))

        case NXRolloutAction(f) =>
            MarchAction(f, 1, 1, NXDoneAction(f, Repeat))

        case NXRearmAction(f, c) =>
            Ask(f).each(f.hand)(d => NXRearmCardAction(f, c, d).!(d.matches(c.cost).not)).cancel

        case NXRearmCardAction(f, c, d) =>
            Ask(f).each(1.to(f.at(c).count(Cannonball)).$.reverse)(n => NXRearmReturnAction(f, c, d, n)).cancel

        case NXRearmReturnAction(f, c, d, n) =>
            f.hand --> d --> discard.quiet

            f.from(c) --> n.times(Cannonball) --> f.reserve

            f.log("rearmed", "with", d, Comma, "returning", n.hl, (n == 1).?("cannonball").|("cannonballs"), "from", c)

            NXDoneAction(f, Repeat)

        case NXDoneAction(f, then) =>
            f.acted += 1

            then

        // EVENING
        case EveningNAction(20, f : NoxiousBattery) =>
            val gone = f.marks.%(c => f.at(c).count(Skunk) < 2)

            gone.foreach(c => f.from(c) --> ScentMark --> f.reserve)

            if (gone.any)
                f.log("removed", ScentMark.sof(f), "from", gone./(_.elem).comma)

            NXSprayAction(f)

        case NXSprayAction(f) =>
            val l = clearings.%(c => f.at(c).count(Skunk) >= 2 && f.at(c).has(ScentMark).not)

            if (l.none || f.pool(ScentMark).not)
                Next
            else
            if (l.num <= f.pooled(ScentMark)) {
                l.foreach(c => f.reserve --> ScentMark --> c)

                f.log("sprayed", ScentMark.sof(f), "in", l./(_.elem).comma)

                Next
            }
            else
                Ask(f).each(l)(c => NXSprayPickAction(f, c)).evening(f)

        case NXSprayPickAction(f, c) =>
            f.reserve --> ScentMark --> c

            f.log("sprayed", ScentMark.of(f), "in", c)

            NXSprayAction(f)

        case EveningNAction(40, f : NoxiousBattery) =>
            val n = min(f.loads, min(f.pooled(Cannonball), 3 - f.bore.num))

            if (n > 0) {
                f.reserve --> n.times(Cannonball) --> f.bore

                f.log("loaded", n.hl, (n == 1).?("cannonball").|("cannonballs"), "into", TheBore.name.styled(f))
            }

            Next

        case NightStartAction(f : NoxiousBattery) =>
            EveningDrawAction(f, f.draws)

        case FactionCleanUpAction(f : NoxiousBattery) =>
            f.acted = 0

            CleanUpAction(f)

        case _ => UnknownContinue
    }

}
