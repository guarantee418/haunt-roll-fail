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

// The Doomed Expansion's Warriors Wake (slugfacekillah, v1.6; the rules are its faction board's): vultures that place a warrior
// where enemy warriors die, battle in a Frenzy whose dead go into the Mess, feast on the enemies there for points and consume
// any warrior there for an action, spread despair and keep the discard pile on their board.
//
// Readings (the board leaves these open):
// - Setup: the corner opposite a taken corner (the Keep's when the Marquise starts in one), as the other corner factions do; with
//   Homelands setup any clearing that isn't a homeland
// - Stench of Death: a warrior for each removal of enemy warriors (forcibly, by anyone) from a clearing where the Warriors Wake have no
//   piece, so one per clearing per battle, as the first one placed gives them a piece there
// - Looming Shadow: no building can be placed in a clearing with despair; a removed enemy building or token may place despair
//   (from the track, in a clearing without one) in place of its points
// - Hoarders: the discard pile is shown only to the Warriors Wake (display only; cards that take a card from it still can)
// - Bolster: with 5 or more clearings to bolster, the Warriors Wake pick 4; a bird card matches every clearing
// - Frenzy: the battles, and the bird cards spent for more, in any order; warriors removed from the map during the Frenzy, any
//   faction's, go into the Mess instead of their supply
// - Feast: 2 warriors score 1, 3 or 4 score 2, 5 or more score 4
// - Consume: the action is taken first and the warrior returned after (from the Mess to its owner's supply); the craft uses the
//   despair tokens as crafting pieces of their clearings' suits
// - Scavenge: with too few cards in the discard pile, the rest are looked at from the top of the shared deck; the cards not taken
//   are discarded
// - The Despair track's spaces show, left to right: look at 1 more, 1 more, take 1 more, look at 2 more, shuffle back into the
//   deck, take 1 more and look at 1 more, look at 2 more, take 1 more and look at 1 more

case object WakeVulture extends Warrior {
    override def name = "Vulture"
}

case object Despair extends Token

case object WakeMess extends SpecialRegion {
    override def name = "Mess"
}

// the supply, as a choice to take a warrior from
case object WakeSupply extends SpecialRegion {
    override def elem(implicit game : Game) : Elem = "supply".hh
}

case object StenchOfDeath extends FactionEffect {
    val name = "Stench of Death"
}

case object LoomingShadow extends FactionEffect {
    val name = "Looming Shadow"
}

case object WakeHoarders extends FactionEffect {
    val name = "Hoarders"
}

trait WarriorsWake extends WarriorFaction {
    val clashKey = WW

    val warrior = WakeVulture

    def abilities(options : $[Meta.O]) = $(StenchOfDeath, LoomingShadow, WakeHoarders)

    def pieces(options : $[Meta.O]) = WakeVulture *** 15 ++ Despair *** 8

    override def note : Elem = SpacedDash ~ "by " ~ "slugfacekillah".hl

    def advertising = 3.times(Image(this.style + "-despair", styles.piece)).merge

    def motto = "Feast".styled(this)

    override def initialDamagePriorities(options : $[Meta.O]) = $(WakeVulture, Despair)
}

case object WW extends WarriorsWake {
    val name = "Warriors Wake"
    val short = "WW"
    val style = "ww"
    val priority = "V"
}

object WarriorsWake {
    val track = 8

    // what each uncovered space of the Despair track adds to Scavenge: (look, take, shuffle back)
    val spaces = $((1, 0, false), (1, 0, false), (0, 1, false), (2, 0, false), (0, 0, true), (1, 1, false), (2, 0, false), (1, 1, false))

    def feast(n : Int) = n match {
        case 0 | 1 => 0
        case 2 => 1
        case 3 | 4 => 2
        case _ => 4
    }

    // Looming Shadow: no building in a clearing with despair
    def shadowed(c : Clearing)(implicit game : Game) = factions.of[WarriorsWake].%(game.states.contains).exists(_.at(c).has(Despair))

    // where the WakeHoarders' discard pile can be looked at
    def hoarded(implicit game : Game) = factions.of[WarriorsWake].%(game.states.contains).any
}

case object WakeFrenzy extends Message {
    def elem(implicit game : Game) = "Frenzy"
}

case object WakeConvergeMsg extends Message {
    def elem(implicit game : Game) = "Converge"
}

case object WakeConsume extends Message {
    def elem(implicit game : Game) = "Consume"
}

case class WakeBolsterAction(self : WarriorsWake, d : DeckCard, l : $[Clearing]) extends BaseAction("Bolster".styled(self), "with")(d.img) with ViewCard
case class WakeBolsterNextAction(self : WarriorsWake, l : $[Clearing], n : Int) extends ForcedAction
case class WakeBolsterPlaceAction(self : WarriorsWake, c : Clearing, l : $[Clearing], n : Int) extends BaseAction("Bolster".styled(self), "in")(c)

case class WakeFrenzyBattleAction(self : WarriorsWake, l : $[Clearing]) extends BaseAction("Frenzy".styled(self))("Battle".styled(self), dt.Battle) with Soft
case class WakeFrenzyCardAction(self : WarriorsWake, d : DeckCard) extends BaseAction("Frenzy".styled(self), "one more battle for")(d.img) with ViewCard
case class WakeBattledAction(self : WarriorsWake, then : ForcedAction) extends ForcedAction

case class WakeFeastAction(self : WarriorsWake, l : $[Figure]) extends ForcedAction

case class WakeMessAction(self : WarriorsWake, r : Region) extends BaseAction("Start of", Evening, "put a", WakeVulture.of(self), "in the", "Mess".styled(self), "from")(r)

case class WakeConsumeMoveAction(self : WarriorsWake) extends BaseAction("Consume".styled(self))("Move".styled(self), dt.Move) with Soft
case class WakeConsumeBattleAction(self : WarriorsWake, l : $[Clearing]) extends BaseAction("Consume".styled(self))("Battle".styled(self), dt.Battle) with Soft
case class WakeConsumeDrawAction(self : WarriorsWake) extends BaseAction("Consume".styled(self))("Draw a card")
case class WakeConsumeDespairAction(self : WarriorsWake, l : $[Clearing]) extends BaseAction("Consume".styled(self))("Place", Despair.of(self), Despair.imgd(self)) with Soft
case class WakeConsumeDespairClearingAction(self : WarriorsWake, c : Clearing) extends BaseAction("Place", Despair.of(self), "in")(c)
case class WakeConsumeCraftAction(self : WarriorsWake) extends BaseAction("Consume".styled(self))("Craft".styled(self), "with", Despair.of(self)) with Soft
case class WakeConsumePayAction(self : WarriorsWake, then : ForcedAction) extends ForcedAction
case class WakeConsumeReturnAction(self : WarriorsWake, u : Figure, then : ForcedAction) extends BaseAction("Return from the", "Mess".styled(self))(u.piece.of(u.faction), u.piece.imgd(u.faction))

case class WakeDespairInsteadAction(self : WarriorsWake, c : Clearing, e : Faction, p : Piece, then : ForcedAction) extends BaseAction(Despair.of(self), "or points for", p.of(e), "in", c)("Place", Despair.of(self), "in", c)
case class WakeScoreAnywayAction(self : WarriorsWake, c : Clearing, e : Faction, p : Piece, n : Int, m : Message, then : ForcedAction) extends BaseAction(Despair.of(self), "or points for", p.of(e), "in", c)("Score", n.vp)

case class WakeScavengeShuffledAction(self : WarriorsWake, shuffled : $[DeckCard]) extends ShuffledAction[DeckCard]
case class WakeScavengeLookAction(self : WarriorsWake) extends ForcedAction
case class WakeScavengeBackAction(self : WarriorsWake, l : $[DeckCard]) extends ForcedAction
case class WakeScavengeDeckShuffledAction(self : WarriorsWake, shuffled : $[DeckCard]) extends ShuffledAction[DeckCard]
case class WakeScavengeTakeAction(self : WarriorsWake) extends ForcedAction
case class WakeScavengeKeepAction(self : WarriorsWake, l : $[DeckCard]) extends ForcedAction


class WarriorsWakePlayer(val faction : WarriorsWake)(implicit val game : Game) extends FactionState {
    val mess = location(WakeMess, u => u.piece.is[Warrior])

    val scavenged = cards("scavenged")

    var bolstered = false
    var feasted = false
    var battles = 0
    var frenzy = 2

    def craft = all(Despair)./(_.asset)

    def uncovered = WarriorsWake.track - pooled(Despair)

    def spaces = WarriorsWake.spaces.take(uncovered)

    def looks = 1 + spaces./(_._1).sum
    def takes = 1 + spaces./(_._2).sum
    def shuffles = spaces.exists(_._3)

    def enemyMess = mess.$.%(_.faction != faction)
}


object WarriorsWakeExpansion extends FactionExpansion[WarriorsWake] {
    def bolsterable(f : WarriorsWake, d : DeckCard)(implicit game : Game) = clearings.%(c => d.matches(c.cost)).%(c => f.at(c).has(WakeVulture).not).%(f.canPlace)

    def perform(action : Action, soft : Void)(implicit game : Game) = action @@ {
        // SETUP
        case CreatePlayerAction(f : WarriorsWake) =>
            game.states += f -> new WarriorsWakePlayer(f)

            FactionInitAction(f)

        case FactionSetupAction(f : WarriorsWake) if options.has(SetupTypeHomelands) =>
            val l = clearings.diff(game.homelands).%(f.canPlace)
            val ll = l.%(c => factions.but(f)./~(_.at(c)).none).some.|(l)

            Ask(f).each(ll)(c => StartingClearingAction(f, c).as(c)(f, "starts in")).needOk

        case FactionSetupAction(f : WarriorsWake) =>
            StartingCornerAction(f)

        case StartingClearingAction(f : WarriorsWake, c) =>
            game.homelands :+= c

            f.reserve --> 4.times(WakeVulture) --> c
            f.reserve --> Despair --> c

            f.log("placed", 4.of(WakeVulture.name).styled(f), "and", Despair.of(f), "in", c)

            SetupFactionsAction

        // STENCH OF DEATH
        case ForcedRemoveTargetEffectAction(e, c, o, p : Warrior, then) if o != e && factions.of[WarriorsWake].exists(f => f != o && f.friends(o).not && f.pool(WakeVulture) && f.present(c).not && f.canPlace(c)) =>
            factions.of[WarriorsWake].%(f => f != o && f.friends(o).not && f.pool(WakeVulture) && f.present(c).not && f.canPlace(c)).foreach { f =>
                f.reserve --> WakeVulture --> c

                f.log("placed", WakeVulture.of(f), "in", c, "with", StenchOfDeath.of(f))
            }

            TryAgain

        // LOOMING SHADOW
        case ForcedRemoveScoreAction(f : WarriorsWake, c, e, p, n, m, then) if e != f && n > 0 && (p.is[Building] || p.is[Token]) && f.pool(Despair) && f.at(c).has(Despair).not && f.canPlace(c) =>
            Ask(f)
                .add(WakeScoreAnywayAction(f, c, e, p, n, m, then))
                .add(WakeDespairInsteadAction(f, c, e, p, then))

        case WakeScoreAnywayAction(f, c, e, p, n, m, then) =>
            f.oscore(n)(m, p.ofg(e))

            then

        case WakeDespairInsteadAction(f, c, e, p, then) =>
            f.reserve --> Despair --> c

            f.log("placed", Despair.of(f), "in", c, "for", p.of(e), "with", LoomingShadow.of(f))

            then

        // BIRDSONG
        case BirdsongNAction(30, f : WarriorsWake) =>
            soft()

            if (f.bolstered || f.pool(WakeVulture).not)
                NoAsk(f)(Next)
            else
                Ask(f)
                    .each(f.hand.get)(d => WakeBolsterAction(f, d, bolsterable(f, d)).!(bolsterable(f, d).none))
                    .skip(Next)
                    .birdsong(f)

        case WakeBolsterAction(f, d, _) =>
            f.hand --> d --> discard.quiet

            f.bolstered = true

            f.log("bolstered with", d)

            WakeBolsterNextAction(f, bolsterable(f, d), min(4, f.pooled(WakeVulture)))

        case WakeBolsterNextAction(f, l, n) =>
            if (n <= 0 || l.none)
                Next
            else
            if (l.num <= n) {
                game.highlights :+= PlaceHighlight(l)

                l.foreach(c => f.reserve --> WakeVulture --> c)

                f.log("placed", WakeVulture.of(f), "in", l.comma)

                Next
            }
            else
                Ask(f).group("Bolster".styled(f), "(" ~ n.hl ~ " more)").each(l)(c => WakeBolsterPlaceAction(f, c, l.but(c), n))

        case WakeBolsterPlaceAction(f, c, l, n) =>
            game.highlights :+= PlaceHighlight($(c))

            f.reserve --> WakeVulture --> c

            f.log("placed", WakeVulture.of(f), "in", c)

            WakeBolsterNextAction(f, l, n - 1)

        // CONVERGE
        case BirdsongNAction(50, f : WarriorsWake) =>
            soft()

            val l = clearings.%(c => f.at(c).has(WakeVulture)).%(c => game.connected(c).exists(d => d != c && f.at(d).has(WakeVulture)))

            if (l.none)
                NoAsk(f)(Next)
            else
                MoveInitAction(f, f, game.transports./($) ** $($(AllWarriors, WakeConverge)), WakeConvergeMsg, l, f.movable, $(Next.as("End", "Converge".styled(f))), Repeat)

        // DAYLIGHT: FRENZY
        case DaylightNAction(30, f : WarriorsWake) =>
            game.wakeFrenzy = Some(f)

            f.battles = 0
            f.frenzy = 2

            Next

        case DaylightNAction(40, f : WarriorsWake) =>
            soft()

            val att = clearings.%(f.canAttackIn)
            val birds = f.hand.get.%(_.suit == Bird)

            Ask(f)
                .group("Frenzy".styled(f), "(" ~ (f.frenzy - f.battles).hl ~ " battle" ~ (f.frenzy - f.battles != 1).??("s") ~ " left)")
                .add(WakeFrenzyBattleAction(f, att).!(att.none, "nowhere").!(f.battles >= f.frenzy, "no battles left"))
                .each(birds)(d => WakeFrenzyCardAction(f, d))
                .add(Next.as("End", "Frenzy".styled(f)))
                .daylight(f)

        case WakeFrenzyBattleAction(f, l) =>
            BattleInitAction(f, f, WakeFrenzy, l, $(CancelAction), WakeBattledAction(f, Repeat))

        case WakeBattledAction(f, then) =>
            f.battles += 1

            then

        case WakeFrenzyCardAction(f, d) =>
            f.hand --> d --> discard.quiet

            f.frenzy += 1

            f.log("spent", d, "for one more battle")

            Repeat

        case DaylightNAction(50, f : WarriorsWake) =>
            game.wakeFrenzy = None

            Next

        // DAYLIGHT: FEAST
        case DaylightNAction(60, f : WarriorsWake) =>
            soft()

            val l = f.enemyMess

            if (f.feasted || l.num < 2)
                NoAsk(f)(Next)
            else {
                implicit val convF = (x : Figure, s : Boolean) => FigureSelect(x).elem(s)(styles.iii)

                XXSelectObjectsAction(f, l)
                    .withGroup("Feast".styled(f) ~ " on enemy warriors in the " ~ "Mess".styled(f) ~ " (2: " ~ 1.vp ~ ", 3 or 4: " ~ 2.vp ~ ", 5 or more: " ~ 4.vp ~ ")")
                    .withRule(_.atLeast(2))
                    .withThen(l => WakeFeastAction(f, l))(l => "Feast on".hl ~ " " ~ l.num.hl ~ " warriors for " ~ WarriorsWake.feast(l.num).vp)
                    .withExtra($(Next.as("Skip")))
            }

        case WakeFeastAction(f, l) =>
            f.feasted = true

            l.foreach(_ --> game.recycle)

            f.nscore(WarriorsWake.feast(l.num))("feasting")(f, "feasted on", l./(u => u.piece.of(u.faction)).comma, ForVP)

            Next

        // EVENING: THE MESS
        case EveningNAction(10, f : WarriorsWake) =>
            soft()

            if (f.mess.any)
                NoAsk(f)(Next)
            else {
                val l = clearings.%(c => f.at(c).has(WakeVulture))

                Ask(f)
                    .each(l)(c => WakeMessAction(f, c))
                    .when(f.pool(WakeVulture))(WakeMessAction(f, WakeSupply))
                    .skip(Next)
                    .evening(f)
            }

        case WakeMessAction(f, r) =>
            f.from((r == WakeSupply).?(Pool).|(r)) --> WakeVulture --> f.mess

            f.log("put", WakeVulture.of(f), "in the", "Mess".styled(f), "from", r)

            Next

        // EVENING: CONSUME
        case EveningNAction(20, f : WarriorsWake) =>
            soft()

            if (f.mess.none)
                NoAsk(f)(Next)
            else {
                val att = clearings.%(f.canAttackIn)
                val mv = f.moveFrom
                val ds = clearings.%(f.rules).%(c => f.at(c).has(Despair).not).%(f.canPlace)
                val cr = f.hand.%(f.craftable)

                Ask(f)
                    .group("Consume".styled(f), "a warrior from the", "Mess".styled(f), "(" ~ f.mess.num.hl ~ ")")
                    .add(WakeConsumeMoveAction(f).!(mv.none, "nowhere"))
                    .add(WakeConsumeBattleAction(f, att).!(att.none, "nowhere"))
                    .add(WakeConsumeDrawAction(f))
                    .add(WakeConsumeDespairAction(f, ds).!(f.pool(Despair).not, "none left").!(ds.none, "nowhere"))
                    .add(WakeConsumeCraftAction(f).!(cr.none, "nothing craftable"))
                    .add(Next.as("End", "Consume".styled(f)))
                    .evening(f)
            }

        case WakeConsumeMoveAction(f) =>
            MoveInitAction(f, f, $, WakeConsume, f.moveFrom, f.movable, $(CancelAction), WakeConsumePayAction(f, Repeat))

        case WakeConsumeBattleAction(f, l) =>
            BattleInitAction(f, f, WakeConsume, l, $(CancelAction), WakeConsumePayAction(f, Repeat))

        case WakeConsumeDrawAction(f) =>
            WakeConsumePayAction(f, DrawCardsAction(f, 1, WakeConsume, AddCardsAction(f, Repeat)))

        case WakeConsumeDespairAction(f, l) =>
            Ask(f).each(l)(c => WakeConsumeDespairClearingAction(f, c)).cancel

        case WakeConsumeDespairClearingAction(f, c) =>
            game.highlights :+= PlaceHighlight($(c))

            f.reserve --> Despair --> c

            f.log("placed", Despair.of(f), "in", c)

            WakeConsumePayAction(f, Repeat)

        case WakeConsumeCraftAction(f) =>
            YYSelectObjectsAction(f, f.hand)
                .withGroup("Craft".styled(f) ~ " with " ~ f.craft.diff(f.crafted)./(dt.CraftSuit).merge)
                .withRule(f.craftable)
                .withThen(d => CraftAction(f, d, WakeConsume, WakeConsumePayAction(f, CraftPerformAction(f, d, WakeConsume))))(d => game.desc("Craft", d, "with", d.as[CraftCard]./~(_.cost./(dt.CraftSuit)).merge))("Craft")
                .withExtra($(NoHand, CancelAction))

        case WakeConsumePayAction(f, then) =>
            val l = f.mess.$
            val ll = l.%(u => l.indexWhere(x => x.faction == u.faction && x.piece == u.piece) == l.indexOf(u))

            if (ll.none)
                then
            else
                Ask(f).each(ll)(u => WakeConsumeReturnAction(f, u, then))

        case WakeConsumeReturnAction(f, u, then) =>
            u --> game.recycle

            f.log("consumed", u.piece.of(u.faction), "from the", "Mess".styled(f))

            then

        // EVENING: REMOVE DESPAIR
        case EveningNAction(30, f : WarriorsWake) =>
            val l = f.all(Despair).%(c => f.at(c).has(WakeVulture).not)

            l.foreach { c =>
                f.from(c) --> Despair --> f.reserve
            }

            if (l.any)
                f.log("removed", Despair.of(f), "from", l.comma)

            Next

        // EVENING: SCAVENGE
        case EveningNAction(40, f : WarriorsWake) =>
            if (pile.num > 1)
                Shuffle[DeckCard](pile, WakeScavengeShuffledAction(f, _))
            else
                WakeScavengeLookAction(f)

        case WakeScavengeShuffledAction(f, l) =>
            pile --> l --> pile

            WakeScavengeLookAction(f)

        case WakeScavengeLookAction(f) =>
            val n = f.looks
            val a = pile.get.take(n)

            pile --> a --> f.scavenged

            val b = deck.get.take(n - a.num)

            deck --> b --> f.scavenged

            if (f.scavenged.any) {
                f.log("scavenged", f.scavenged.num.cards, (a.any && b.any).?("(" ~ a.num.hl ~ " from the discard pile, " ~ b.num.hl ~ " from the deck)"), (a.none).?("from the deck"))

                if (f.shuffles)
                    XXSelectObjectsAction(f, f.scavenged.get)
                        .withGroup("Scavenge".styled(f) ~ ": shuffle cards back into the deck")
                        .withRuleNone(_.upTo(f.scavenged.num))
                        .withThen(WakeScavengeBackAction(f, _))(l => l.none.?("Shuffle none back".hl).|("Shuffle back".hl ~ l./(" " ~ _.elem)))
                        .withExtra($(NoHand))
                else
                    WakeScavengeTakeAction(f)
            }
            else
                Next

        case WakeScavengeBackAction(f, l) =>
            if (l.none)
                WakeScavengeTakeAction(f)
            else {
                f.scavenged --> l --> deck

                f.log("shuffled", l.num.cards, "back into the deck")

                Shuffle[DeckCard](deck, WakeScavengeDeckShuffledAction(f, _))
            }

        case WakeScavengeDeckShuffledAction(f, l) =>
            deck --> l --> deck

            WakeScavengeTakeAction(f)

        case WakeScavengeTakeAction(f) =>
            val n = f.takes

            if (f.scavenged.none)
                Next
            else
            if (f.scavenged.num <= n)
                WakeScavengeKeepAction(f, f.scavenged.get)
            else
                XXSelectObjectsAction(f, f.scavenged.get)
                    .withGroup("Scavenge".styled(f) ~ ": take " ~ n.cards)
                    .withRule(_.num(n))
                    .withThen(WakeScavengeKeepAction(f, _))(l => "Take".hl ~ l./(" " ~ _.elem))
                    .withExtra($(NoHand))

        case WakeScavengeKeepAction(f, l) =>
            f.scavenged --> l --> f.hand

            f.log("took", l.num.cards)

            if (f.scavenged.any) {
                f.log("discarded", f.scavenged.get)

                f.scavenged --> discard.quiet
            }

            Next

        // EVENING: HAND LIMIT
        case NightStartAction(f : WarriorsWake) =>
            EveningHandLimitAction(f)

        case FactionCleanUpAction(f : WarriorsWake) =>
            f.bolstered = false
            f.feasted = false
            f.battles = 0
            f.frenzy = 2

            game.wakeFrenzy = None

            CleanUpAction(f)

        case _ => UnknownContinue
    }
}

// Converge: all the warriors in a clearing, ignoring rule, into an adjacent clearing with Warriors Wake warriors
case object WakeConverge extends Transport with FactionEffect {
    val name = "Converge"

    override def allows(f : Faction, o : Region, d : Region)(implicit game : Game) = (f, o, d) @@ {
        case (f : WarriorsWake, o : Clearing, d : Clearing) => f.at(o).has(WakeVulture) && f.at(d).has(WakeVulture)
        case _ => false
    }
}
