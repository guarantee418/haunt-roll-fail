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

case class Squires(suit : BaseSuit) extends CardEffect {
    override val name = suit.name + " Squires"
}

case object SkyCouriers extends CardEffect {
    override val name = "Sky Couriers"
}

case object SpyNetwork extends CardEffect {
    override val name = "Spy Network"
}

case object SilverTongue extends CardEffect {
    override val name = "Silver-Tongue"
}

case object ShadowCouncil extends CardEffect {
    override val name = "Shadow Council"
}

case object FeatherRufflers extends CardEffect {
    override val name = "Feather Rufflers"
}

case object BoldLeadership extends CardEffect with BattleEffect {
    override val name = "Bold Leadership"
}

case object SupplyTrain extends CardEffect {
    override val name = "Supply Train"
}

case object Tactitian extends CardEffect with BattleEffect {
    override val name = "Tactician"
}

case object Apprentice extends CardEffect {
    override val name = "Apprentice"
}

case object Lookouts extends CardEffect with BattleEffect {
    override val name = "Lookouts"
}

case object HiddenWarrens extends CardEffect {
    override val name = "Hidden Warrens"
}

case object TheFaithful extends CardEffect with BattleEffect {
    override val name = "The Faithful"
}

case object Riversteads extends CardEffect {
    override val name = "Riversteads"
}

case object StandardBearer extends CardEffect {
    override val name = "Standard Bearer"
}

case object RaidingParty extends CardEffect {
    override val name = "Raiding Party"
}

case object MiceInABush extends CardEffect with BattleEffect {
    override val name = "Mice-in-a-Bush"
}

case object BrazenDemagogue extends CardEffect {
    override val name = "Brazen Demagogue"
}


trait FriendOfThe extends CardEffect {
    val suit : BaseSuit
}

object FriendOfThe {
    def apply(s : BaseSuit) = s @@ {
        case Fox => FriendOfTheFoxes
        case Rabbit => FriendOfTheRabbits
        case Mouse => FriendOfTheMice
    }
}

case object FriendOfTheFoxes extends FriendOfThe {
    override val name = "Friend of the Foxes"
    val suit = Fox
}

case object FriendOfTheRabbits extends FriendOfThe {
    override val name = "Friend of the Rabbits"
    val suit = Rabbit
}

case object FriendOfTheMice extends FriendOfThe {
    override val name = "Friend of the Mice"
    val suit = Mouse
}


// A card in hand that a Friend of the ___ treats as another suit until the end of the turn
case class FriendCard(card : DeckCard, disguise : Suit) extends DeckCard with Record {
    def suit = disguise
    def name = card.name + " (" + disguise.name + ")"
    def id = card.id
    override def altS = card.altS + " as " + disguise.name
    def altL = card.altL + " - treated as a " + disguise.name + " card"
}


// Moves only to the given region
case class ToRegion(r : Region) extends Transport {
    override def allows(f : Faction, o : Region, d : Region)(implicit game : Game) = d == r
}

// Moves only to or from the given region
case class ToOrFromRegion(r : Region) extends Transport {
    override def allows(f : Faction, o : Region, d : Region)(implicit game : Game) = o == r || d == r
}

// Moves between any two clearings sharing a suit, ignoring paths
case object HiddenWarrensMove extends Transport {
    override def allows(f : Faction, o : Region, d : Region)(implicit game : Game) = (o, d) @@ {
        case (o : Clearing, d : Clearing) => o != d && o.suits.intersect(d.suits).any
        case _ => false
    }
}


trait SquiresPayment extends Record
case class PaySelf(e : CardEffect) extends SquiresPayment
case class PayReturn(e : CardEffect) extends SquiresPayment
case class PayCard(d : DeckCard) extends SquiresPayment


case class ForcedBy(f : Faction) extends Message {
    def elem(implicit game : Game) = " forced by " ~ ShadowCouncil.elem ~ " of " ~ f.elem
}


case class SquiresPayAction(self : Faction, p : SquiresPayment, then : ForcedAction) extends ForcedAction
case class SquiresContinueAction(then : ForcedAction) extends ForcedAction

case class SkyCouriersMainAction(self : Faction) extends BaseAction(Birdsong, "start")(SkyCouriers) with Soft
case class SkyCouriersAction(self : Faction, l : $[DeckCard]) extends ForcedAction

case class ShadowCouncilMainAction(self : Faction) extends BaseAction(Birdsong, "start")(ShadowCouncil)
case class ShadowCouncilCardAction(self : Faction, d : DeckCard, l : $[(Clearing, Faction, Faction)]) extends BaseAction(ShadowCouncil, "spends")(d.img) with ViewCard with Soft
case class ShadowCouncilBattleAction(self : Faction, d : DeckCard, c : Clearing, e : Faction, o : Faction) extends BaseAction(ShadowCouncil, "spends", d, "and forces")(e, "to battle", o, "in", c)
case class ShadowCouncilReturnAction(self : Faction) extends BaseAction(ShadowCouncil)("No valid target, return", ShadowCouncil, "to hand")

case class FeatherRufflersMainAction(self : WarriorFaction, p : Phase, l : $[Clearing]) extends BaseAction(p)(FeatherRufflers) with Soft
case class FeatherRufflersAction(self : WarriorFaction, c : Clearing) extends BaseAction(FeatherRufflers, "places warriors in")(c)

case class SpyNetworkMainAction(self : Faction, p : Phase, l : $[(Faction, DeckCard)]) extends BaseAction(p)(SpyNetwork) with Soft
case class SpyNetworkTargetAction(self : Faction, e : Faction, d : DeckCard) extends BaseAction(SpyNetwork, "takes from", e)(d.img) with ViewCard with Soft
case class SpyNetworkGiveAction(self : Faction, e : Faction, x : DeckCard, d : DeckCard) extends BaseAction(SpyNetwork, "gives", e)(d.img) with ViewCard
case class SpyNetworkShuffleAction(shuffled : $[DeckCard], then : ForcedAction) extends ShuffledAction[DeckCard]

case class SilverTongueMainAction(self : Faction, p : Phase, l : $[Clearing]) extends BaseAction(p)(SilverTongue) with Soft
case class SilverTongueClearingAction(self : Faction, c : Clearing) extends BaseAction(SilverTongue, "treats as ruled")(c) with Soft
case class SilverTongueAction(self : Faction, c : Clearing, then : ForcedAction) extends ForcedAction

case class FriendMainAction(self : Faction, p : Phase, s : BaseSuit) extends BaseAction(p)(FriendOfThe(s)) with Soft
case class FriendCardAction(self : Faction, s : BaseSuit, d : DeckCard) extends BaseAction(FriendOfThe(s), "treats as another suit")(d.img) with ViewCard with Soft
case class FriendDisguiseAction(self : Faction, s : BaseSuit, d : DeckCard, t : Suit) extends BaseAction(FriendOfThe(s), "treats", d, "as")(t)

case class SquiresMainAction(self : Faction, s : BaseSuit, lm : $[Region], lb : $[Clearing]) extends BaseAction(Daylight)(Squires(s)) with Soft
case class SquiresModeAction(self : Faction, s : BaseSuit, move : Boolean, lm : $[Region], lb : $[Clearing]) extends ForcedAction with Soft

case class ApprenticeMainAction(self : Faction) extends BaseAction(Birdsong, "start")(Apprentice)
case class ApprenticeCraftAction(self : Faction, d : DeckCard) extends BaseAction(Apprentice, "crafts at no cost")(d.img) with ViewCard
case class ApprenticeShowHandAction(self : Faction) extends BaseAction(Apprentice)("Nothing to craft, show hand")

case class HiddenWarrensMainAction(self : Faction) extends BaseAction(Birdsong, "start")(HiddenWarrens) with Soft
case class RiversteadsAction(self : Faction) extends BaseAction(Birdsong, "start")(Riversteads)

case class TacticianAction(self : Faction, b : Battle, then : ForcedAction) extends ForcedAction with Soft
case class LookoutsAction(self : WarriorFaction, b : Battle, then : ForcedAction) extends ForcedAction
case class MiceInABushAction(self : Faction, b : Battle, then : ForcedAction) extends ForcedAction
case class TheFaithfulAction(self : Faction, c : Clearing, then : ForcedAction) extends BaseAction(self, "in battle")("Deal an extra", 1.hit, "with", TheFaithful)
case class TheFaithfulRevealAction(self : Faction, c : Clearing, then : ForcedAction) extends ForcedAction
case class StandardBearerCheckAction(self : Faction, c : Clearing, then : ForcedAction) extends ForcedAction

case class BrazenDemagogueMainAction(self : Faction, then : ForcedAction) extends ForcedAction
case class BrazenDemagogueTakeAction(self : Faction, d : Dominance, then : ForcedAction) extends BaseAction(BrazenDemagogue, "takes")(d.img) with ViewCard
case class BrazenDemagogueActivateAction(self : Faction, d : Dominance, then : ForcedAction) extends BaseAction(BrazenDemagogue)("Activate", d, "and keep scoring points")


object SquiresDeckExpansion extends Expansion {
    def active(setup : $[Faction], options : $[Meta.O]) = options.has(SquiresDeck)

    def isCardOf(e : Effect)(d : DeckCard) = d @@ {
        case CraftEffectCard(_, _, _, x) => x == e
        case _ => false
    }

    def returnToHand(f : Faction, e : CardEffect)(implicit game : Game) {
        f.effects :-= e

        f.stuck --> f.stuck.%(isCardOf(e)) --> f.hand
    }

    def payments(f : Faction, e : CardEffect, self : CardEffect => SquiresPayment, cost : SuitCost)(implicit game : Game) : $[SquiresPayment] =
        f.has(e).$(self(e)) ++ f.hand.%(_.matches(cost))./(PayCard)

    def paymentElem(p : SquiresPayment)(implicit game : Game) : Elem = p @@ {
        case PaySelf(e) => "Discard " ~ e.elem
        case PayReturn(e) => "Return " ~ e.elem ~ " to hand"
        case PayCard(d) => "Discard " ~ d.elem
    }

    def defaultTransports(f : Faction)(implicit game : Game) = game.transports./($) ** f.transports ** f.is[Hero].?($($(MoveBoots), $(MoveDoubleBoots))).|($($()))

    def moveFromWith(f : Faction, tt : $[$[Transport]])(implicit game : Game) : $[Region] = f.movePlans(f.movable.%(f.canMoveFrom), tt).keys.$

    def squiresMoveFrom(f : Faction, s : BaseSuit)(implicit game : Game) = moveFromWith(f, defaultTransports(f)).%(_.as[Clearing].?(_.cost.matched(s)))

    def apprenticeCraftable(f : Faction)(d : DeckCard)(implicit game : Game) : Boolean = d @@ {
        case d if d.suit == Frog && f.is[InvasiveDDD] => false
        case d : CraftItemCard => game.uncrafted.has(d.item)
        case d : CraftEffectCard if f.has(d.effect) => false
        case d : CraftEffectCard if d.effect.is[FriendOfThe] && f.effects.of[FriendOfThe].any => false
        case d : CraftEffectCard => true
        case d : Favor => true
        case _ => false
    }

    def shadowCouncilTargets(d : DeckCard)(implicit game : Game) : $[(Clearing, Faction, Faction)] =
        clearings.%(c => d.matches(c.cost))./~(c => factions./~(e => e.canAttackList(c)./(o => (c, e, o))))

    def spyNetworkTargets(f : Faction)(implicit game : Game) : $[(Faction, DeckCard)] =
        factions.but(f)./~(e => e.stuck.$.of[CraftEffectCard]./(d => (e, d)))

    def silverTongueRules(f : Faction, c : Region)(implicit game : Game) : Boolean =
        game.states.get(f).?(_.silverTongue.?{ case (x, t, p) => x == c && t == game.turn && p == game.phase })

    def friendAmbush(f : Faction, c : Clearing)(d : DeckCard)(implicit game : Game) : Boolean = d @@ {
        case Ambush(s : BaseSuit) if FoxRabbitMouse.has(s) => c.cost.matched(s).not && f.can(FriendOfThe(s))
        case _ => false
    }

    def revertDisguises()(implicit game : Game) {
        factions.foreach { f =>
            f.disguises.foreach { fc =>
                game.cards.locationsOf(fc).starting.foreach { l =>
                    game.cards.move(l, fc, f.friendTrash)
                    game.cards.move(f.disguised, fc.card, l)
                }
            }

            f.disguises = $
        }
    }

    def phased(f : Faction, p : Phase)(implicit game : Game, ask : ActionCollector) {
        if (game.current != f)
            return

        f.as[WarriorFaction].foreach { f =>
            if (f.can(FeatherRufflers)) {
                val l = clearings.%(f.present).%(f.canPlace)
                + FeatherRufflersMainAction(f, p, l).!(f.pool(f.warrior).not, "no warriors").!(l.none, "no clearings")
            }
        }

        if (f.can(SpyNetwork)) {
            val l = spyNetworkTargets(f)
            + SpyNetworkMainAction(f, p, l).!(f.hand.none, "no cards").!(l.none, "no crafted cards")
        }

        if (f.can(SilverTongue)) {
            val l = clearings.%(f.present).%(c => f.rules(c).not)
            + SilverTongueMainAction(f, p, l).!(l.none, "no clearings")
        }

        FoxRabbitMouse.foreach { s =>
            if (f.can(FriendOfThe(s)))
                + FriendMainAction(f, p, s).!(f.hand.%(_.suit == s).%(_.is[Dominance].not).none, "no " + s.name + " cards")
        }
    }

    override def birdsong(f : Faction)(implicit game : Game, ask : ActionCollector) {
        phased(f, Birdsong)
    }

    override def daylight(f : Faction)(implicit game : Game, ask : ActionCollector) {
        FoxRabbitMouse.foreach { s =>
            if (f.can(Squires(s))) {
                val lm = squiresMoveFrom(f, s)
                val lb = clearings.%(_.cost.matched(s)).%(f.canAttackIn)

                + SquiresMainAction(f, s, lm, lb).!(lm.none && lb.none)
            }
        }

        phased(f, Daylight)
    }

    override def evening(f : Faction)(implicit game : Game, ask : ActionCollector) {
        phased(f, Evening)
    }

    def perform(action : Action, soft : Void)(implicit game : Game) = action @@ {
        // PAYMENTS
        case SquiresContinueAction(then) =>
            then

        case SquiresPayAction(f, PaySelf(e), then) =>
            f.removeStuckEffect(e)

            f.log("discarded", e)

            then

        case SquiresPayAction(f, PayReturn(e), then) =>
            returnToHand(f, e)

            f.log("returned", e, "to hand")

            then

        case SquiresPayAction(f, PayCard(d), then) =>
            f.hand --> d --> discard.quiet

            f.log("discarded", d)

            then

        case MoveListAction(self, f, t, m, from, to, l, SquiresPayAction(ff, p, then)) =>
            SquiresPayAction(ff, p, ForceAction(MoveListAction(self, f, t, m, from, to, l, then)))

        case MoveFinishedAction(f, from, to, SquiresPayAction(ff, p, then)) =>
            SquiresPayAction(ff, p, MoveFinishedAction(f, from, to, then))

        case CantMoveAction(self, f, t, m, from, to, SquiresPayAction(_, _, then)) =>
            ForceAction(CantMoveAction(self, f, t, m, from, to, then))

        case BattleStartAction(self, f, a, m, c, o, i, SquiresPayAction(ff, p, then)) =>
            SquiresPayAction(ff, p, BattleStartAction(self, f, a, m, c, o, i, then))

        case BattleImpossibleAction(self, f, l, SquiresPayAction(_, _, then)) =>
            ForceAction(BattleImpossibleAction(self, f, l, then))

        // SKY COURIERS
        case SkyCouriersMainAction(f) =>
            XXSelectObjectsAction(f, f.hand)
                .withGroup(SkyCouriers.elem ~ " spends cards to draw one more")
                .withRule(_.atLeast(1))
                .withThen(SkyCouriersAction(f, _))(l => "Spend".hl ~ l./(" " ~ _.elem) ~ " to draw " ~ (l.num + 1).hl ~ " " ~ (l.num + 1).cards)
                .withExtra($(NoHand, CancelAction))

        case SkyCouriersAction(f, l) =>
            f.hand --> l --> discard.quiet

            f.removeStuckEffect(SkyCouriers)

            f.log("spent", l, "with", SkyCouriers, "and discarded it")

            DrawCardsAction(f, l.num + 1, WithEffect(SkyCouriers), AddCardsAction(f, Repeat))

        // SHADOW COUNCIL
        case ShadowCouncilMainAction(f) =>
            val l = f.hand./(d => d -> shadowCouncilTargets(d))

            if (l.exists(_._2.any))
                Ask(f).group(ShadowCouncil, "spends a card to force a battle")
                    .each(l)((d, t) => ShadowCouncilCardAction(f, d, t).!(t.none, "no targets"))
                    .add(NoHand)
            else
                Ask(f).add(ShadowCouncilReturnAction(f))

        case ShadowCouncilCardAction(f, d, l) =>
            Ask(f).group(ShadowCouncil, "spends", d, "to force")
                .each(l)(x => ShadowCouncilBattleAction(f, d, x._1, x._2, x._3))
                .cancel

        case ShadowCouncilBattleAction(f, d, c, e, o) =>
            f.hand --> d --> discard.quiet

            returnToHand(f, ShadowCouncil)

            f.log("spent", d, "and returned", ShadowCouncil, "to hand")

            ForceAction(BattleConfirmAction(e, e, e, ForcedBy(f), c, o, None, Repeat))

        case ShadowCouncilReturnAction(f) =>
            returnToHand(f, ShadowCouncil)

            f.log("found no battle to force and returned", ShadowCouncil, "to hand")

            Repeat

        // FEATHER RUFFLERS
        case FeatherRufflersMainAction(f, p, l) =>
            Ask(f).group(FeatherRufflers, "places", min(2, f.pooled(f.warrior)).hl, f.warrior.of(f), "in")
                .each(l)(c => FeatherRufflersAction(f, c))
                .cancel

        case FeatherRufflersAction(f, c) =>
            val n = min(2, f.pooled(f.warrior))

            n.times(f.warrior).foreach { w =>
                f.reserve --> w --> c
            }

            f.removeStuckEffect(FeatherRufflers)

            f.log("placed", n.times(f.warrior)./(_.of(f)).comma, "in", c, "and discarded", FeatherRufflers)

            Repeat

        // SPY NETWORK
        case SpyNetworkMainAction(f, p, l) =>
            Ask(f).group(SpyNetwork, "takes a crafted card")
                .each(l)((e, d) => SpyNetworkTargetAction(f, e, d))
                .cancel

        case SpyNetworkTargetAction(f, e, x) =>
            Ask(f).group(SpyNetwork, "gives", e, "a card for", x)
                .each(f.hand)(d => SpyNetworkGiveAction(f, e, x, d))
                .add(NoHand)
                .cancel

        case SpyNetworkGiveAction(f, e, x @ CraftEffectCard(_, _, _, effect), d) =>
            f.hand --> d --> e.hand

            e.notify($(ViewCardInfoAction(e, Gave(f, e).elem(game), d)))

            e.effects :-= effect

            e.stuck --> x --> f.hand

            f.effects :-= SpyNetwork

            f.stuck --> f.stuck.%(isCardOf(SpyNetwork)) --> deck

            f.log("gave", e, "a card and took", x, "with", SpyNetwork, "then shuffled it into the deck")

            Shuffle[DeckCard](deck, SpyNetworkShuffleAction(_, Repeat))

        case SpyNetworkShuffleAction(l, then) =>
            deck --> l --> deck

            then

        // SILVER TONGUE
        case SilverTongueMainAction(f, p, l) =>
            Ask(f).group(SilverTongue, "treats a clearing as ruled until the end of", p)
                .each(l)(c => SilverTongueClearingAction(f, c))
                .cancel

        case SilverTongueClearingAction(f, c) =>
            Ask(f).group(SilverTongue, "in", c)
                .each(payments(f, SilverTongue, PaySelf, c.cost))(p => SquiresPayAction(f, p, SilverTongueAction(f, c, Repeat)).as(paymentElem(p)))
                .cancel

        case SilverTongueAction(f, c, then) =>
            f.used :+= SilverTongue

            f.silverTongue = Some((c, game.turn, game.phase))

            f.log("treated", c, "as ruled until the end of", game.phase, "with", SilverTongue)

            then

        // FRIEND OF THE
        case FriendMainAction(f, p, s) =>
            Ask(f).group(FriendOfThe(s), "treats a", s, "card as another suit")
                .each(f.hand)(d => FriendCardAction(f, s, d).!(d.suit != s || d.is[Dominance]))
                .add(NoHand)
                .cancel

        case FriendCardAction(f, s, d) =>
            Ask(f).group(FriendOfThe(s), "treats", d, "as")
                .each($[Suit](Fox, Rabbit, Mouse, Bird).but(s))(t => FriendDisguiseAction(f, s, d, t))
                .cancel

        case FriendDisguiseAction(f, s, d, t) =>
            val fc = FriendCard(d, t)

            f.used :+= FriendOfThe(s)

            f.hand --> d --> f.disguised

            game.cards.another[DeckCard]("friend-" + f.short + "-" + game.turn, $(fc)) --> fc --> f.hand

            f.disguises :+= fc

            f.log("treated", d, "as", t, "with", FriendOfThe(s), "until the end of the turn")

            Repeat

        case BattleAmbushAction(f, b, d @ Ambush(s : BaseSuit), true, true, then) if friendAmbush(f, b.clearing)(d) =>
            f.used :+= FriendOfThe(s)

            f.log("ambushed as", Bird, "with", FriendOfThe(s))

            ForceAction(BattleAmbushAction(f, b, d, true, true, then))

        case BattleCounterAmbushAction(f, b, d @ Ambush(s : BaseSuit), true, true, then) if friendAmbush(f, b.clearing)(d) =>
            f.used :+= FriendOfThe(s)

            f.log("counter-ambushed as", Bird, "with", FriendOfThe(s))

            ForceAction(BattleCounterAmbushAction(f, b, d, true, true, then))

        case CleanUpAction(f) if factions.exists(_.disguises.any) =>
            revertDisguises()

            CleanUpAction(f)

        // SQUIRES
        case SquiresMainAction(f, s, lm, lb) =>
            Ask(f).group(Squires(s))
                .add(SquiresModeAction(f, s, true, lm, lb).as("Move", dt.Move, "from a", s, "clearing").!(lm.none))
                .add(SquiresModeAction(f, s, false, lm, lb).as("Battle", dt.Battle, "in a", s, "clearing").!(lb.none))
                .cancel

        case SquiresModeAction(f, s, move, lm, lb) =>
            def then(p : SquiresPayment) = SquiresPayAction(f, p, UsedEffectAction(f, Squires(s), Repeat))

            Ask(f).group(Squires(s), "to", move.?("move").|("battle"))
                .each(payments(f, Squires(s), PaySelf, s))(p =>
                    if (move)
                        MoveInitAction(f, f, defaultTransports(f), WithEffect(Squires(s)), lm, lm, $(CancelAction), then(p)).as(paymentElem(p))
                    else
                        BattleInitAction(f, f, WithEffect(Squires(s)), lb, $(CancelAction), then(p)).as(paymentElem(p))
                )
                .cancel

        // APPRENTICE
        case ApprenticeMainAction(f) =>
            val l = f.hand.%(apprenticeCraftable(f))

            if (l.any)
                Ask(f).group(Apprentice, "crafts a card at no cost")
                    .each(f.hand)(d => ApprenticeCraftAction(f, d).!(l.has(d).not))
                    .add(NoHand)
            else
                Ask(f).add(ApprenticeShowHandAction(f))

        case ApprenticeCraftAction(f, d) =>
            f.removeStuckEffect(Apprentice)

            f.log("discarded", Apprentice)

            CraftPerformAction(f, d, WithEffect(Apprentice))

        case ApprenticeShowHandAction(f) =>
            f.used :+= Apprentice

            if (f.hand.any)
                f.log("could not craft with", Apprentice, "and showed their hand", f.hand.$)
            else
                f.log("could not craft with", Apprentice, "with an empty hand")

            factions.but(f).foreach { e =>
                e.notify(f.hand./(d => ViewCardInfoAction(e, FactionHand(f).elem(game), d)))
            }

            Repeat

        // HIDDEN WARRENS
        case HiddenWarrensMainAction(f) =>
            val l = moveFromWith(f, $($(HiddenWarrensMove)))

            MoveInitAction(f, f, $($(HiddenWarrensMove)), WithEffect(HiddenWarrens), l, l, $(CancelAction), SquiresPayAction(f, PayReturn(HiddenWarrens), Repeat))

        // RIVERSTEADS
        case RiversteadsAction(f) =>
            val n = game.riverside.%(f.present).num

            f.removeStuckEffect(Riversteads)

            if (n > 0)
                DrawCardsAction(f, n, AndDiscarded(Riversteads), AddCardsAction(f, Repeat))
            else {
                f.log("had no pieces by the river and discarded", Riversteads)

                Repeat
            }

        // SUPPLY TRAIN, RAIDING PARTY
        case MoveFinishedAction(f, from, to, then) if f == game.current && then.is[BattleDefenderPreRollAction].not && then.is[BattleAttackerPreRollAction].not && (f.can(SupplyTrain) || f.can(RaidingParty)) =>
            val tt = defaultTransports(f) ** $($(ToOrFromRegion(to)))
            val st = f.can(SupplyTrain).??(moveFromWith(f, tt))
            val rp = f.can(RaidingParty) && to.as[Clearing].?(f.canAttackIn)

            if (st.any || rp)
                Ask(f).group("After moving to", to)
                    .when(st.any)(MoveInitAction(f, f, tt, WithEffect(SupplyTrain), st, st, $(CancelAction), SquiresPayAction(f, PayReturn(SupplyTrain), then)).as("Move to or from", to, "and return", SupplyTrain, "to hand"))
                    .when(rp)(BattleInitAction(f, f, WithEffect(RaidingParty), to.as[Clearing].toList, $(CancelAction), SquiresPayAction(f, PayReturn(RaidingParty), then)).as("Battle in", to, "and return", RaidingParty, "to hand"))
                    .skip(SquiresContinueAction(then))
            else
                then

        // TACTICIAN
        case BattleDefenderPreRollAction(b) if b.defender.can(Lookouts) && b.defender.as[WarriorFaction].?(f => f.pool(f.warrior) && f.canPlace(b.clearing)) =>
            LookoutsAction(b.defender.as[WarriorFaction].get, b, BattleDefenderPreRollAction(b))

        case BattleDefenderPreRollAction(b) if b.defender.can(Tactitian) && moveFromWith(b.defender, defaultTransports(b.defender) ** $($(ToRegion(b.clearing)))).any =>
            TacticianAction(b.defender, b, BattleDefenderPreRollAction(b))

        case BattleAttackerPreRollAction(b) if b.attacker.can(Tactitian) && moveFromWith(b.attacker, defaultTransports(b.attacker) ** $($(ToRegion(b.clearing)))).any =>
            TacticianAction(b.attacker, b, BattleAttackerPreRollAction(b))

        case TacticianAction(f, b, then) =>
            val tt = defaultTransports(f) ** $($(ToRegion(b.clearing)))
            val l = moveFromWith(f, tt)

            Ask(f).group(f, "before the roll")
                .add(MoveInitAction(f, f, tt, WithEffect(Tactitian), l, l, $(CancelAction), SquiresPayAction(f, PayReturn(Tactitian), then)).as("Move to", b.clearing, "and return", Tactitian, "to hand"))
                .add(IgnoredEffectAction(f, Tactitian, then).as("Skip"))

        // LOOKOUTS
        case LookoutsAction(f, b, then) =>
            val c = b.clearing

            f.used :+= Lookouts

            f.reserve --> f.warrior --> c

            f.log("placed", f.warrior.of(f), "in", c, "with", Lookouts)

            Ask(f).group(Lookouts)
                .each(payments(f, Lookouts, PayReturn, c.cost))(p => SquiresPayAction(f, p, then).as(paymentElem(p)))

        // MICE IN A BUSH
        case BattleAskAmbushAction(b, then) if b.defender.can(MiceInABush) && b.attacker.has(ScoutingParty).not && b.defender.canRemove(b.clearing)(b.attacker) =>
            val f = b.defender

            Ask(f).group(f, "can ambush with", MiceInABush)
                .each(payments(f, MiceInABush, PaySelf, b.clearing.cost))(p => SquiresPayAction(f, p, MiceInABushAction(f, b, then)).as(paymentElem(p)))
                .add(IgnoredEffectAction(f, MiceInABush, BattleAskAmbushAction(b, then)).as("Skip"))
                .add(NoHand)

        case MiceInABushAction(f, b, then) =>
            f.used :+= MiceInABush

            f.log("ambushed with", MiceInABush, "dealing", 1.hit)

            BattleAmbushHitsAction(b, 1, BattleAskAmbushAction(b, then))

        // BOLD LEADERSHIP
        case BattleBonusAction(b, f, o, fs, os, fr, or, fh, oh, fe, oe) if f == b.attacker && f.can(BoldLeadership) && f.canRemove(b.clearing)(o) =>
            f.used :+= BoldLeadership

            f.log("dealt an extra", 1.hit, "with", BoldLeadership)

            val q = BattleBonusAction(b, f, o, fs, os, fr, or, fh, oh, fe + 1, oe)

            Ask(f).group(BoldLeadership)
                .each(payments(f, BoldLeadership, PayReturn, b.clearing.cost))(p => SquiresPayAction(f, p, q).as(paymentElem(p)))

        // THE FAITHFUL
        case TheFaithfulAction(f, c, then) =>
            f.used :+= TheFaithful

            f.log("dealt an extra", 1.hit, "with", TheFaithful)

            DrawCardsFromDeckAction(f, 1, WithEffect(TheFaithful), TheFaithfulRevealAction(f, c, then))

        case TheFaithfulRevealAction(f, c, then) =>
            val l = f.drawn.$

            f.drawn --> discard.quiet

            if (l.any)
                f.log("revealed", l, "from the deck and discarded it")

            if (l.exists(_.matches(c.cost)).not) {
                f.removeStuckEffect(TheFaithful)

                f.log("discarded", TheFaithful)
            }

            then

        // STANDARD BEARER
        case BattleFinishedAction(b) if b.attacker.can(StandardBearer) && b.then.is[StandardBearerCheckAction].not =>
            BattleFinishedAction(b.copy(then = StandardBearerCheckAction(b.attacker, b.clearing, b.then)))

        case StandardBearerCheckAction(f, c, then) =>
            if (f.can(StandardBearer) && f.canAttackIn(c))
                Ask(f).group(StandardBearer)
                    .add(BattleInitAction(f, f, WithEffect(StandardBearer), $(c), $(CancelAction), SquiresPayAction(f, PayReturn(StandardBearer), then)).as("Battle again in", c, "and return", StandardBearer, "to hand"))
                    .skip(SquiresContinueAction(then))
            else
                then

        // BRAZEN DEMAGOGUE
        case BrazenDemagogueMainAction(f, then) =>
            if (game.dominances.any)
                Ask(f).group(BrazenDemagogue, "takes a dominance card")
                    .each(game.dominances.$)(d => BrazenDemagogueTakeAction(f, d, then))
            else {
                f.removeStuckEffect(BrazenDemagogue)

                f.log("found no available dominance and discarded", BrazenDemagogue)

                then
            }

        case BrazenDemagogueTakeAction(f, d, then) =>
            game.dominances --> d --> f.hand

            f.removeStuckEffect(BrazenDemagogue)

            f.log("took", d, "with", BrazenDemagogue)

            if (f.dominance.none && f.coalition.none && f.is[Hero].not)
                Ask(f).group(BrazenDemagogue)
                    .add(BrazenDemagogueActivateAction(f, d, then))
                    .skip(SquiresContinueAction(then))
            else
                then

        case BrazenDemagogueActivateAction(f, d, then) =>
            f.hand --> d --> f.stuck

            f.dominance = Some(d)
            f.demagogue = true

            f.log("activated", d, "and kept their victory points")

            then

        case _ => UnknownContinue
    }

}
