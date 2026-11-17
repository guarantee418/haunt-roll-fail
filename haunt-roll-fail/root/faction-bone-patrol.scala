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

// The Outsiders Expansion's Bone Patrol! (slugfacekillah, v2.1; Law of Slug BP and its faction board): pugs led by the Alpha, fetching
// the bones buried around the Woodland back to their stashes for points, to craft with or to bury again.
//
// Readings (the rules leave these open):
// - Bones: four of each suit; a buried (face-down) bone is a piece no other player can see, battle, remove or count; buried bones keep
//   the clearing's suit, and a clearing of two suits gets the suit with more bones left
// - The Alpha: deals 1 extra hit attacking; it is never removed (battles skip it, effects that remove every piece spare it) and can be
//   moved only on Bone Patrol's turn; its clearing is always ruled by Bone Patrol (above Lords of the Forest, gardens and burrows)
// - Protective Breed: the Alpha takes one hit in each battle in its clearing, so the attacker needs one more hit than Bone Patrol has
//   other warriors there to reach its buildings and tokens
// - Guard Dogs: offered in every phase menu of another player's turn, for a clearing Bone Patrol rules where that player has pieces and
//   a card matching it; the card goes to Bone Patrol's hand and the player rules the clearing too until the end of their turn
// - Obedient: every move of Bone Patrol warriors starts or ends in the Alpha's clearing; face-up bones move with them, one a warrior
// - Stashes track: its spaces, left to right, show a scout, a draw, a scout and two draws, uncovered as stashes are built (from the
//   left) and covered again when a removed stash comes back (on the rightmost empty space)
// - Bone Collection: three spaces a suit, the rightmost showing a draw; the Evening draw is 1, or the draws showing on both tracks if
//   more (so 3 at the start, with the collection empty)
// - Rollover: with 4 or more bones there at the start of Birdsong, any number are buried one at a time in matching clearings with no
//   Bone Patrol piece; bones removed by others go there too
// - Recall: each bone scores 2, and goes to the collection (while its suit has room) or to the Rollover, which scores 1 more when that
//   suit's collection is full
// - Fetch/Bury: each bone is flipped at most once an Evening

case object Pug extends Warrior

case object Alpha extends Warrior with Tenacious {
    override def name = "The Alpha"
    override def plural = "The Alpha"
}

case object Stash extends Building {
    override def plural = "Stashes"
}

case class Bone(suit : BaseSuit) extends Token with Movable {
    override def name = "Bone"
    override def of(f : Faction) = (suit.name + " Bone").styled(f)
    override def imgid(f : Faction) = f.style + "-bone-" + suit.name.toLowerCase
}

case class BuriedBone(suit : BaseSuit) extends Token with Tenacious {
    override def name = "Buried Bone"
    override def of(f : Faction) = ("Buried " + suit.name + " Bone").styled(f)
    override def imgid(f : Faction) = f.style + "-bone-buried"
}

case object BoneRollover extends SpecialRegion {
    override def name = "Rollover"
}

case object BoneCollection extends SpecialRegion {
    override def name = "Bone Collection"
}

case object TheAlpha extends FactionEffect {
    val name = "The Alpha"
}

case object GuardDogs extends FactionEffect {
    val name = "Guard Dogs"
}

case object Obedient extends FactionEffect {
    val name = "Obedient"
}

case object ProtectiveBreed extends BattleEffect {
    val name = "Protective Breed"
}

case object AlphaHit extends BattleEffect {
    val name = "The Alpha"
}

case object ObedientMove extends Transport {
    override def allows(f : Faction, o : Region, d : Region)(implicit game : Game) = f @@ {
        case f : BonePatrol => f.alpha.exists(a => a == o || a == d)
        case _ => true
    }

    override def allows(f : Faction, o : Region, d : Region, l : $[Movable])(implicit game : Game) = allows(f, o, d) && (f @@ {
        case f : BonePatrol => l.has(Alpha).not || game.current == f
        case _ => true
    })
}

case object BoneMove extends Transport {
    override def allows(f : Faction, o : Region)(implicit game : Game) = (f, o) @@ {
        case (f : BonePatrol, o : Clearing) => f.at(o).of[Warrior].any
        case _ => false
    }

    override def allows(f : Faction, o : Region, d : Region, l : $[Movable])(implicit game : Game) = allows(f, o, d) && l./~(_.as[Bone]).num <= l.of[Warrior].num

    override def order(l : $[Movable]) : Int = l.of[Warrior].num * 9 + l.num
    override def sortBy(m : Movable) : Int = (m == Alpha).??(-2) + (m == Pug).??(-1) + m.is[Bone].??(1)
}

// the Alpha's scout: a move the Alpha is in
case object AlphaMove extends Transport {
    override def allows(f : Faction, o : Region)(implicit game : Game) = f @@ {
        case f : BonePatrol => f.alpha.exists(_ == o)
        case _ => false
    }

    override def allows(f : Faction, o : Region, d : Region, l : $[Movable])(implicit game : Game) = allows(f, o, d) && l.has(Alpha)
}

trait BonePatrol extends WarriorFaction {
    val clashKey = BP

    val warrior = Pug

    def abilities(options : $[Meta.O]) = $(EveningCraft, TheAlpha, GuardDogs, Obedient)

    override val transports : $[$[Transport]] = $($(RuledMove, ObedientMove, BoneMove))

    def pieces(options : $[Meta.O]) = Pug *** 15 ++ Alpha *** 1 ++ Stash *** 4 ++ FoxRabbitMouse./~(s => Bone(s) *** 4 ++ BuriedBone(s) *** 4)

    override def note : Elem = SpacedDash ~ "V" ~ "2.1".hh ~ " by " ~ "slugfacekillah".hl ~ HorizontalBreak ~ "Outsiders Expansion (fan-made)"

    def advertising = FoxRabbitMouse./(s => Bone(s).img(this)).merge

    def motto = "Fetch".styled(this)

    override def initialDamagePriorities(options : $[Meta.O]) = $(Pug) ++ FoxRabbitMouse./(Bone) ++ $(Stash)
}

case object BP extends BonePatrol {
    val name = "Bone Patrol!"
    val short = "BP"
    val style = "bp"
    val priority = "V''"
}

object BonePatrol {
    // the Stashes track's spaces, left to right: (scouts, draws)
    val track = $((1, 0), (0, 1), (1, 0), (0, 2))
}

trait BPDaylightQuestion extends FactionAction {
    override def self : BonePatrol

    def question(implicit game : Game) = self.elem ~ SpacedDash ~ Daylight.elem ~ Break ~
        Div(
            1.to(3 + self.extra)./(_ => Image("action-black", styles.action, "")).take(self.acted) ~
            (1.to(3)./(_ => Image(self.style + "-action", styles.action)) ++ (0.until(self.extra)./(_ => Image("action-bird", styles.action)))).drop(self.acted),
        styles.margined)
}

case class BoneSetupCentralAction(self : BonePatrol, c : Clearing, l : $[Clearing]) extends BaseAction(self, "starts with", Alpha.of(self), "in")(c) with Soft
case class BoneSetupAction(self : BonePatrol, c : Clearing, l : $[Clearing]) extends BaseAction(self, "places", Pug.sof(self), "in")(l.comma)

case class BoneRolloverAction(self : BonePatrol) extends ForcedAction
case class BoneBuryAction(self : BonePatrol, s : BaseSuit, c : Clearing) extends BaseAction("Rollover".styled(self), "bury", "bone".styled(self), "in")(c, "(" ~ s.elem ~ ")")

case class BoneRecallAction(self : BonePatrol, c : Clearing, s : BaseSuit, collect : Boolean) extends BaseAction("Recall".styled(self), Bone(s).of(self), "from", c, "to")(collect.?("Bone Collection".styled(self)).|("Rollover".styled(self)))

case class BoneScoutAction(self : BonePatrol, l : $[Clearing], n : Int, total : Int) extends BaseAction("Scout".styled(self), "(" ~ n.hl ~ "/" ~ total.hl ~ ")")(Alpha.of(self), "moves") with Soft
case class BoneScoutedAction(self : BonePatrol) extends ForcedAction

case class BoneMarchAction(self : BonePatrol) extends OptionAction("March".styled(self), dt.Move, dt.Move) with BPDaylightQuestion with Soft with Only[MoveListAction] { def tag = implicitly }
case class BoneBattleAction(self : BonePatrol, l : $[Clearing]) extends OptionAction("Battle".styled(self), dt.Battle) with BPDaylightQuestion with Soft with Only[BattleStartAction] { def tag = implicitly }
case class BoneRecruitAction(self : BonePatrol, n : Int, l : $[Clearing]) extends OptionAction("Recruit".styled(self), l.num.times(Pug.imgd(self))) with BPDaylightQuestion with Soft with Only[PlacePieceClearingsAction] { def tag = implicitly }
case class BoneBuildAction(self : BonePatrol, l : $[Clearing]) extends OptionAction("Build".styled(self), Stash.of(self), Stash.imgd(self)) with BPDaylightQuestion with Soft with Only[BoneBuildClearingCardAction] { def tag = implicitly }
case class BoneBuildClearingAction(self : BonePatrol, c : Clearing) extends BaseAction("Build", Stash.of(self), "in")(c) with Soft
case class BoneBuildClearingCardAction(self : BonePatrol, c : Clearing, d : DeckCard) extends BaseAction("Build", Stash.of(self), "in", c, "with")(d.img) with ViewCard
case class BoneExtraAction(self : BonePatrol, n : Int) extends OptionAction("Extra action".hl, n.times(dt.CardSuit(Bird))) with BPDaylightQuestion with Soft
case class BoneExtraDoneAction(f : BonePatrol) extends ForcedAction
case class BoneDoneAction(f : BonePatrol, then : ForcedAction) extends ForcedAction

case class BoneFetchAction(self : BonePatrol, c : Clearing, s : BaseSuit) extends BaseAction("Fetch".styled(self), "a buried bone in")(c, "(" ~ s.elem ~ ")")
case class BoneBuryBackAction(self : BonePatrol, c : Clearing, s : BaseSuit) extends BaseAction("Bury".styled(self), Bone(s).of(self), "in")(c)

case class GuardDogsMainAction(self : Faction, t : BonePatrol, l : $[Clearing], then : ForcedAction) extends BaseAction(GuardDogs.of(t))("Give", t, "a card to share rule") with Soft
case class GuardDogsClearingAction(self : Faction, t : BonePatrol, c : Clearing, then : ForcedAction) extends BaseAction(GuardDogs.of(t), "share rule of")(c) with Soft
case class GuardDogsAction(self : Faction, t : BonePatrol, c : Clearing, d : DeckCard, then : ForcedAction) extends BaseAction("Give", t, "for rule of", c)(d.img) with ViewCard


class BonePatrolPlayer(val faction : BonePatrol)(implicit val game : Game) extends FactionState {
    val rollover = location(BoneRollover, u => u.faction == faction && u.piece.is[Bone])
    val collection = location(BoneCollection, u => u.faction == faction && u.piece.is[Bone])

    var acted = 0
    var extra = 0
    var scouted = 0
    // bones flipped this Evening: the clearing, the suit and whether it was buried
    var flipped : $[(Clearing, BaseSuit, Boolean)] = $
    // Guard Dogs: the players sharing rule of a clearing until the end of their turn
    var guards : $[(Faction, Clearing)] = $

    def craft = collection.$./~(_.piece.as[Bone])./(_.suit)

    def alpha : |[Clearing] = all(Alpha).single

    def collected(s : BaseSuit) = collection.$.count(_.piece == Bone(s))

    def uncovered = 4 - pooled(Stash)

    def spaces = BonePatrol.track.take(uncovered)

    def scouts = spaces.map(_._1).sum

    def trackDraws = spaces.map(_._2).sum

    def collectionDraws = FoxRabbitMouse.%(s => collected(s) == 0).num

    def draws = max(1, trackDraws + collectionDraws)
}


object BonePatrolExpansion extends FactionExpansion[BonePatrol] {
    def patrols(implicit game : Game) = factions.of[BonePatrol].%(game.states.contains)

    // Guard Dogs: a player sharing rule of a clearing with Bone Patrol
    def guarded(f : Faction, c : Region)(implicit game : Game) = patrols.exists(t => t.guards.exists { case (g, x) => g == f && x == c })

    // pieces other players can hit: never the Alpha or a buried bone
    def hittable(p : Piece) = p != Alpha && p.is[BuriedBone].not

    // the fewest steps from c to one of l
    def distance(c : Clearing, l : $[Clearing])(implicit game : Game) : Int = {
        var seen = $(c)
        var front = $(c)
        var n = 0

        while (front.any && front.intersect(l).none) {
            n += 1
            front = front./~(game.connected).distinct.diff(seen)
            seen ++= front
        }

        front.any.?(n).|(999)
    }

    def suitFor(f : BonePatrol, c : Clearing, from : $[BaseSuit])(implicit game : Game) =
        c.suits.of[BaseSuit].%(FoxRabbitMouse.has).%(from.has).sortBy(s => -from.count(s)).starting

    def bury(f : BonePatrol, s : BaseSuit, c : Clearing)(implicit game : Game) {
        f.rollover --> Bone(s) --> f.reserve
        f.reserve --> BuriedBone(s) --> c
    }

    def buryable(f : BonePatrol)(implicit game : Game) = {
        val l = f.rollover.$./~(_.piece.as[Bone])./(_.suit)

        clearings.%(c => f.at(c).none).%(f.canPlace)./~(c => c.suits.of[BaseSuit].%(l.has).distinct./(s => (s, c)))
    }

    def recallable(f : BonePatrol)(implicit game : Game) =
        f.all(Stash).distinct.%(c => f.at(c).of[Warrior].any)./~(c => f.at(c)./~(_.as[Bone])./(_.suit).distinct./(s => (c, s)))

    def flippable(f : BonePatrol)(implicit game : Game) =
        clearings.%(c => f.at(c).of[Warrior].any)./~(c =>
            FoxRabbitMouse./~(s =>
                (f.at(c).count(BuriedBone(s)) > f.flipped.count((c, s, false))).?((c, s, true)).$ ++
                (f.at(c).count(Bone(s)) > f.flipped.count((c, s, true))).?((c, s, false)).$
            )
        )

    def guardable(f : Faction, t : BonePatrol)(implicit game : Game) =
        clearings.%(c => t.rules(c) && f.present(c) && guarded(f, c).not && f.hand.exists(_.matches(c.cost)))

    def guardDogs(f : Faction)(implicit game : Game, ask : ActionCollector) {
        if (game.current == f)
            patrols.%(_ != f).%(_.friends(f).not).foreach { t =>
                val l = guardable(f, t)

                if (l.any)
                    + GuardDogsMainAction(f, t, l, Repeat)
            }
    }

    override def birdsong(f : Faction)(implicit game : Game, ask : ActionCollector) = guardDogs(f)
    override def daylight(f : Faction)(implicit game : Game, ask : ActionCollector) = guardDogs(f)
    override def evening(f : Faction)(implicit game : Game, ask : ActionCollector) = guardDogs(f)

    def perform(action : Action, soft : Void)(implicit game : Game) = action @@ {
        // SETUP
        case CreatePlayerAction(f : BonePatrol) =>
            game.states += f -> new BonePatrolPlayer(f)

            FactionInitAction(f)

        case FactionSetupAction(f : BonePatrol) =>
            val l = clearings.%(f.canPlace).%(c => factions.but(f)./~(_.at(c)).of[Building].none).diff(options.has(SetupTypeHomelands).??(game.homelands))
            val ll = l.%(c => game.connected(c).%(l.has).num >= 2)

            // Advanced Setup: a homeland with 2 or more clearings between it and the other homelands, if there is one
            val lll = options.has(SetupTypeHomelands).??(ll.%(c => distance(c, game.homelands) >= 3).some || ll.%(c => distance(c, game.homelands) >= 2).some).|(ll)

            Ask(f).each(lll)(c => BoneSetupCentralAction(f, c, game.connected(c).%(l.has))).needOk

        case BoneSetupCentralAction(f, c, l) =>
            Ask(f).each(l.combinations(2).$)(x => BoneSetupAction(f, c, x)).cancel

        case BoneSetupAction(f, c, l) =>
            game.homelands :+= c

            f.reserve --> Alpha --> c
            f.reserve --> 2.times(Pug) --> c
            f.reserve --> Stash --> c

            l.foreach(x => f.reserve --> Pug --> x)

            f.log("placed", Alpha.of(f) ~ ",", 2.of(Pug.name).styled(f), "and", Stash.of(f), "in", c, "and", Pug.sof(f), "in", l./(_.elem).comma)

            FoxRabbitMouse.foreach(s => f.reserve --> 4.times(Bone(s)) --> f.rollover)

            clearings.%(x => f.at(x).none).foreach { x =>
                suitFor(f, x, f.rollover.$./~(_.piece.as[Bone])./(_.suit)).foreach { s =>
                    bury(f, s, x)
                }
            }

            f.log("buried", (12 - f.rollover.num).hl, "bones", Comma, f.rollover.num.hl, "went to the", "Rollover".styled(f))

            SetupFactionsAction

        // THE ALPHA
        case ForcedRemoveAction(e, c, f : BonePatrol, Alpha, n, m, then) =>
            f.log("kept", Alpha.of(f), "in", c)

            then

        case BattleBonusAction(b, f : BonePatrol, o, fs, os, fr, or, fh, oh, fe, oe) if f.used.has(AlphaHit).not && f.at(b.clearing).has(Alpha) =>
            f.used :+= AlphaHit

            f.log("dealt an extra", 1.hit, "with", Alpha.of(f))

            BattleBonusAction(b, f, o, fs, os, fr, or, fh, oh, fe + 1, oe)

        // PROTECTIVE BREED
        case BattleAssignHitsAction(f : BonePatrol, b, n, s, then) if n + s > 0 && f.used.has(ProtectiveBreed).not && f.at(b.clearing).has(Alpha) =>
            f.used :+= ProtectiveBreed

            f.log("took a", "hit".styled(styles.hit), "with", Alpha.of(f))

            BattleAssignHitsAction(f, b, n - (n > 0).??(1), s - (n == 0).??(1), then)

        // nothing left to hit: only the Alpha and buried bones
        case BattleAssignHitsAction(f : BonePatrol, b, n, s, then) if n + s > 0 && f.at(b.clearing).forall(p => hittable(p).not) =>
            then

        // BONES
        case ForcedRemoveTargetEffectAction(e, c, f : BonePatrol, p : Bone, then) =>
            f.limbo(c) --> p --> f.rollover

            then

        // GUARD DOGS
        case GuardDogsMainAction(f, t, l, then) =>
            Ask(f).each(l)(c => GuardDogsClearingAction(f, t, c, then)).cancel

        case GuardDogsClearingAction(f, t, c, then) =>
            Ask(f).each(f.hand)(d => GuardDogsAction(f, t, c, d, then).!(d.matches(c.cost).not)).cancel

        case GuardDogsAction(f, t, c, d, then) =>
            f.hand --> d --> t.hand

            t.guards :+= (f, c)

            f.log("gave", t, "a card to share rule of", c, "with", GuardDogs.of(t))

            then

        case CleanUpAction(f) if patrols.exists(_.guards.exists { case (g, _) => g == f }) =>
            patrols.foreach(t => t.guards = t.guards.filter { case (g, _) => g != f })

            CleanUpAction(f)

        // BIRDSONG
        case BirdsongNAction(30, f : BonePatrol) if f.rollover.num >= 4 && buryable(f).any =>
            BoneRolloverAction(f)

        case BoneRolloverAction(f) =>
            val l = buryable(f)

            if (l.none)
                Next
            else
                Ask(f).each(l) { case (s, c) => BoneBuryAction(f, s, c) }.done(Next).birdsong(f)

        case BoneBuryAction(f, s, c) =>
            bury(f, s, c)

            f.log("buried a bone from the", "Rollover".styled(f), "in", c)

            BoneRolloverAction(f)

        case BirdsongNAction(50, f : BonePatrol) =>
            soft()

            val l = recallable(f)

            if (l.none)
                NoAsk(f)(Next)
            else
                Ask(f)
                    .each(l) { case (c, s) => BoneRecallAction(f, c, s, true).!(f.collected(s) >= 3, "full") }
                    .each(l) { case (c, s) => BoneRecallAction(f, c, s, false) }
                    .done(Next)
                    .birdsong(f)

        case BoneRecallAction(f, c, s, collect) =>
            if (collect)
                f.from(c) --> Bone(s) --> f.collection
            else
                f.from(c) --> Bone(s) --> f.rollover

            val n = 2 + (collect.not && f.collected(s) >= 3).??(1)

            f.nscore(n)("recalling")(f, "recalled", Bone(s).of(f), "from", c, "to the", collect.?("Bone Collection").|("Rollover").styled(f), ForVP)

            Repeat

        case BirdsongNAction(70, f : BonePatrol) =>
            soft()

            val l = f.alpha.%(f.canMoveFrom).$.%(c => f.moveFrom.has(c))

            if (f.scouted >= f.scouts || l.none)
                NoAsk(f)(Next)
            else
                Ask(f)
                    .add(BoneScoutAction(f, l, f.scouted + 1, f.scouts))
                    .skip(Next)
                    .birdsong(f)

        case BoneScoutAction(f, l, _, _) =>
            MoveInitAction(f, f, game.transports./($) ** f.transports ** $($(AlphaMove)), NoMessage, l, l, $(CancelAction), BoneScoutedAction(f))

        case BoneScoutedAction(f) =>
            f.scouted += 1

            Repeat

        // DAYLIGHT
        case DaylightNAction(60, f : BonePatrol) =>
            implicit val ask = builder

            if (f.acted < 3 + f.extra) {
                + BoneMarchAction(f).!(f.moveFrom.none)

                val att = clearings.%(f.canAttackIn)
                + BoneBattleAction(f, att).!(att.none)

                val st = f.all(Stash).%(f.canPlace)
                + BoneRecruitAction(f, min(st.num, f.pooled(Pug)), st).!(f.pool(Pug).not, "max").!(st.none, "no stash")

                val b1 = clearings.%(f.canPlace)
                val b2 = b1.%(f.rules)
                val b3 = b2.%(f.canBuild)
                val b4 = b3.%(c => f.hand.exists(_.matches(c.cost)))

                + BoneBuildAction(f, b4)
                    .!(f.pool(Stash).not, "max")
                    .!(b2.none, "no rule")
                    .!(b3.none, "no slot")
                    .!(b4.none, "no card")
            }

            + BoneExtraAction(f, f.hand.%(_.suit == Bird).num)
                .!(f.hand.%(_.suit == Bird).none, "no bird cards")

            + EndTurnSoftAction(f, "Turn", ForfeitActions(3 + f.extra - f.acted))

            ask(f).daylight(f)

        case BoneMarchAction(f) =>
            MarchAction(f, 1, 2, BoneDoneAction(f, Repeat))

        case BoneBattleAction(f, l) =>
            BattleInitAction(f, f, NoMessage, l, $(CancelAction), BoneDoneAction(f, Repeat))

        case BoneRecruitAction(f, n, l) =>
            val ll = l.combinations(n).$.%(x => l.diff(x).distinct.num == l.diff(x).num)

            Ask(f).each(ll)(l => PlacePieceClearingsAction(f, l, Pug, BoneDoneAction(f, Repeat))).ocancel

        case BoneBuildAction(f, l) =>
            Ask(f).each(l)(c => BoneBuildClearingAction(f, c)).cancel

        case BoneBuildClearingAction(f, c) =>
            Ask(f).each(f.hand)(d => BoneBuildClearingCardAction(f, c, d).!(d.matches(c.cost).not)).cancel

        case BoneBuildClearingCardAction(f, c, d) =>
            game.highlights :+= PlaceHighlight($(c))

            f.hand --> d --> discard.quiet

            f.reserve --> Stash --> c

            f.log("built", Stash.of(f), "in", c, "with", d)

            BoneDoneAction(f, Repeat)

        case BoneExtraAction(f, _) =>
            OptionalDiscardCardAction(f, GetExtra, Bird, BoneExtraDoneAction(f))

        case BoneExtraDoneAction(f) =>
            f.log("got an", "extra action".hl, "with", f.drawn.get)

            f.drawn --> discard.quiet

            f.extra += 1

            Repeat

        case BoneDoneAction(f, then) =>
            f.acted += 1

            then

        // EVENING
        case EveningNAction(30, f : BonePatrol) =>
            soft()

            val l = flippable(f)

            if (l.none)
                NoAsk(f)(Next)
            else
                Ask(f)
                    .each(l.%(_._3)) { case (c, s, _) => BoneFetchAction(f, c, s) }
                    .each(l.%!(_._3)) { case (c, s, _) => BoneBuryBackAction(f, c, s) }
                    .done(Next)
                    .evening(f)

        case BoneFetchAction(f, c, s) =>
            f.from(c) --> BuriedBone(s) --> f.reserve
            f.reserve --> Bone(s) --> c

            f.flipped :+= (c, s, true)

            f.log("fetched", Bone(s).of(f), "in", c)

            Repeat

        case BoneBuryBackAction(f, c, s) =>
            f.from(c) --> Bone(s) --> f.reserve
            f.reserve --> BuriedBone(s) --> c

            f.flipped :+= (c, s, false)

            f.log("buried a bone in", c)

            Repeat

        case EveningNAction(50, f : BonePatrol) =>
            XCraftMainAction(f)

        case NightStartAction(f : BonePatrol) =>
            EveningDrawAction(f, f.draws)

        case FactionCleanUpAction(f : BonePatrol) =>
            f.acted = 0
            f.extra = 0
            f.scouted = 0
            f.flipped = $

            CleanUpAction(f)

        case _ => UnknownContinue
    }

}
