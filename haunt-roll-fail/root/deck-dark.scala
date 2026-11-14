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


// The Dark Deck (The Doomed Expansion, fan-made by slugfacekillah; Law of Doom D3.1; Gman, 2026-11-13): a 66 card deck, the Deck
// choice `DarkDeck`. Its six Abomination cards count as fox, rabbit or mouse, never bird (`Abomination` in `game.scala`); the Cursed
// card (`Cursed`) can't be spent or discarded (a discard leaves it where it was, or puts it back in hand), only crafted with four of any
// suit for 3 points and the Cursed Idol, then it leaves the game; a fifth Dominance needs three adjacent ruled clearings and the Idol;
// the Dark Deck shared supply adds a Torch, the Book of Names, the Rabbit's Paw and the Cursed Idol to the items (`supply`).
//
// Readings where the cards leave room (the Law of Doom has no more on them):
// - "At start of battle" discards (Murderous Mice, Foxes of Fortune, Bunny Bandits) are offered with the other extra hits, after the roll
// - Words of Undoing's reroll is offered to its owner as attacker or defender, once per turn, after the roll (one die or both)
// - Transmutations places, in each clearing of its suit, a warrior of a player with pieces there and warriors left (the crafter picks
//   when several), or else one of the crafter's own
// - Rabbit Roulette, Blood Gate, Infernal Designs and Doppleganger are offered once per turn in every phase of their owner's turn
// - Mouster Criminal and Felonious Fox swap a crafted card for a crafted card, or a crafted item for a crafted item, not the
//   Vagabond's items (the lettered ones can't be told apart)
// - Foaming Fury: a move, a battle, or a move and then a battle; after both, the battle is fought again until either side has no
//   pieces left there to take hits
// - Blood Gate moves up to three warriors for each warrior removed; another player's warriors only if that player agrees
// - Bear Traps' traps stay until the card is discarded; they aren't pieces on the map that can be battled
// - Deadly Deadly Poison: the poisoned bowl goes to a random player in the clearing, who loses a point for each bowl
// - Forbidden Science holds the warriors removed from the map
// - Doppleganger copies another player's crafted card for the rest of the turn; using up the copy discards the Doppleganger
// - Rodent Zero's Infected and the Vagabond's Book of Names and Rabbit's Paw actions aren't done: Rodent Zero is a card without an effect

case object WordsOfUndoing extends CardEffect {
    override val name = "Words of Undoing"
}

case class Transmutations(suit : BaseSuit) extends CardEffect {
    override val name = suit.name + " Transmutations"
}

// Murderous Mice, Foxes of Fortune and Bunny Bandits
case class Bloodlust(suit : BaseSuit) extends CardEffect {
    override val name = suit @@ {
        case Mouse => "Murderous Mice"
        case Fox => "Foxes of Fortune"
        case _ => "Bunny Bandits"
    }
}

case object RabbitRoulette extends CardEffect {
    override val name = "Rabbit Roulette"
}

case object RapaciousRabbit extends CardEffect {
    override val name = "Rapacious Rabbit"
}

// Mouster Criminal and Felonious Fox
case class Swindle(suit : BaseSuit) extends CardEffect {
    override val name = (suit == Fox).?("Felonious Fox").|("Mouster Criminal")
}

case object InfernalDesigns extends CardEffect {
    override val name = "Infernal Designs"
}

case object CleansingFire extends CardEffect {
    override val name = "Cleansing Fire"
}

case object FoamingFury extends CardEffect {
    override val name = "Foaming Fury"
}

case object BloodGate extends CardEffect {
    override val name = "Blood Gate"
}

case object MeatForTheMasses extends CardEffect {
    override val name = "Meat for the Masses"
}

case object RodentZero extends CardEffect {
    override val name = "Rodent Zero"
}

case object ForbiddenScience extends CardEffect {
    override val name = "Forbidden Science"
}

case object BearTraps extends CardEffect {
    override val name = "Bear Traps"
}

case object BottomFeeder extends CardEffect {
    override val name = "Bottom Feeder"
}

case object GoldenKnight extends CardEffect {
    override val name = "Golden Knight"
}

case object DevotionToTheDeep extends CardEffect {
    override val name = "Devotion to the Deep"
}

case object DeadlyPoison extends CardEffect {
    override val name = "Deadly Deadly Poison"
}

case object Doppleganger extends CardEffect {
    override val name = "Doppleganger"
}

case object DevotionHit extends BattleEffect

// drawn on the map (`displayRegion` in `game.scala`), not pieces of any faction
case object BearTrapMarker extends Token
case object GoldenKnightMarker extends Token

// Where the warriors on Forbidden Science wait
case object OnForbiddenScience extends SpecialRegion


case class DarkMenuAction(self : Faction, p : Phase, e : CardEffect) extends BaseAction(p)(e) with Soft

// CURSED
case class CursedReturnAction(self : Faction, then : ForcedAction) extends ForcedAction

// WORDS OF UNDOING
case class WordsUndoAction(self : Faction, e : Faction, d : DeckCard) extends BaseAction(Birdsong, "start")(WordsOfUndoing, "discards", d, "of", e)
case class DarkRollAction(b : Battle, fr : Int, or : Int, l : $[Faction]) extends ForcedAction
case class WordsRerollAction(self : Faction, b : Battle, fr : Int, or : Int, x : $[Int], l : $[Faction]) extends ForcedAction
case class WordsRolledAction(self : Faction, b : Battle, kept : $[Int], x : $[Int], l : $[Faction]) extends RolledAction[Int] { def rolled = x }
case class WordsKeepAction(self : Faction, b : Battle, fr : Int, or : Int, l : $[Faction]) extends ForcedAction

// TRANSMUTATIONS
case class TransmuteMainAction(self : Faction, s : BaseSuit) extends BaseAction(Birdsong, "start")(Transmutations(s), "places a warrior in every", s, "clearing")
case class TransmuteAction(self : Faction, s : BaseSuit, l : $[Clearing]) extends ForcedAction
case class TransmutePlaceAction(self : Faction, s : BaseSuit, c : Clearing, e : WarriorFaction, l : $[Clearing]) extends BaseAction(Transmutations(s), "places a warrior in", c)(e.warrior.of(e), e.warrior.imgd(e))

// BLOODLUST
case class BloodlustMainAction(self : Faction, s : BaseSuit, attacker : Boolean, then : BattlePostRollAction) extends BaseAction(self, "in battle")("Deal an extra", 1.hit, "or more with", Bloodlust(s)) with Soft
case class BloodlustPickAction(self : Faction, s : BaseSuit, attacker : Boolean, l : $[DeckCard], then : BattlePostRollAction) extends ForcedAction with Soft
case class BloodlustAction(self : Faction, s : BaseSuit, attacker : Boolean, l : $[DeckCard], then : BattlePostRollAction) extends BaseAction(Bloodlust(s))("Discard", Bloodlust(s), l.any.$("and", l), "for", (1 + l.num).hits)

// DEVOTION TO THE DEEP
case class DevotionHitAction(self : Faction, then : ForcedAction) extends BaseAction(self, "in battle")("Deal an extra", 1.hit, "with", DevotionToTheDeep)

// RABBIT ROULETTE
case class RouletteClearingAction(self : Faction, c : Clearing) extends BaseAction(RabbitRoulette, "in")(c)
case class RouletteAskAction(self : Faction, c : Clearing, l : $[Faction]) extends ForcedAction
case class RouletteAddAction(self : Faction, c : Clearing, e : Faction, d : DeckCard, l : $[Faction]) extends BaseAction(RabbitRoulette, "in", c, "adds a card")(d.img) with ViewCard
case class RouletteTopAction(self : Faction, c : Clearing) extends ForcedAction
case class RouletteDealAction(self : Faction, c : Clearing, shuffled : $[DeckCard]) extends ShuffledAction[DeckCard]

// RAPACIOUS RABBIT
case class RapaciousAction(self : Faction, d : DeckCard) extends BaseAction(RapaciousRabbit, "discards a card to draw", 2.hl)(d.img) with ViewCard

// SWINDLE
case class SwindleMainAction(self : Faction, s : BaseSuit) extends BaseAction(Birdsong, "start")(Swindle(s)) with Soft
case class SwindleCardMainAction(self : Faction, s : BaseSuit, d : DeckCard) extends BaseAction(Swindle(s), "swaps")(d.img) with ViewCard with Soft
case class SwindleCardAction(self : Faction, s : BaseSuit, d : DeckCard, e : Faction, x : DeckCard) extends BaseAction(Swindle(s), "swaps", d, "for")(x.img, e) with ViewCard
case class SwindleItemMainAction(self : Faction, s : BaseSuit, i : ItemRef) extends BaseAction(Swindle(s), "swaps")(i.img) with Soft
case class SwindleItemAction(self : Faction, s : BaseSuit, i : ItemRef, e : Faction, x : ItemRef) extends BaseAction(Swindle(s), "swaps", i, "for")(x.img, e)

// INFERNAL DESIGNS
case class InfernalCraftAction(self : Faction, i : Item, a : SuitAsset) extends BaseAction(InfernalDesigns, "exhausts an item to activate a crafting piece again")(i.exhaust.img, dt.Arrow, dt.CraftSuit(a))
case class InfernalTakeMainAction(self : Faction, d : DeckCard) extends BaseAction(InfernalDesigns, "exhausts two items to take an item card from the discard pile")(d.img) with ViewCard with Soft
case class InfernalTakeAction(self : Faction, d : DeckCard, a : Item, b : Item) extends BaseAction(InfernalDesigns, "takes", d, "exhausting")(a.exhaust.img, b.exhaust.img)

// CLEANSING FIRE
case class CleansingMainAction(self : Faction, c : Clearing) extends BaseAction(Evening, "start", CleansingFire, "removes all enemy pieces in")(c) with Soft
case class CleansingPickAction(self : Faction, c : Clearing, l : $[DeckCard]) extends ForcedAction with Soft
case class CleansingFireAction(self : Faction, c : Clearing, l : $[DeckCard]) extends BaseAction(CleansingFire, "in", c)("Spend", l, "and end the turn")

// FOAMING FURY
case class FuryMainAction(self : Faction, moved : Boolean) extends ForcedAction
case class FuryMovedAction(self : Faction) extends ForcedAction
case class FuryBattleAction(self : Faction, c : Clearing, o : Faction, again : Boolean) extends BaseAction(FoamingFury, again.?("battles until either side can't take hits"), "in", c)(o)
case class FuryAgainAction(self : Faction, c : Clearing, o : Faction, then : ForcedAction) extends ForcedAction
case class FuryDoneAction(self : Faction) extends ForcedAction

// BLOOD GATE
case class BloodGateClearingAction(self : WarriorFaction, c : Clearing) extends BaseAction(BloodGate, "opens in")(c) with Soft
case class BloodGateWhoAction(self : WarriorFaction, c : Clearing, e : WarriorFaction, out : Boolean) extends BaseAction(BloodGate, "in", c)(e, "warriors", out.?("out of").|("into"), c) with Soft
case class BloodGateWhereAction(self : WarriorFaction, c : Clearing, e : WarriorFaction, out : Boolean, o : Clearing) extends BaseAction(BloodGate, "moves", e, "warriors", out.?("from").|("to"), c, out.?("to").|("from"))(o) with Soft
case class BloodGateAction(self : WarriorFaction, c : Clearing, e : WarriorFaction, out : Boolean, o : Clearing, n : Int) extends BaseAction(BloodGate, "moves", e, "warriors", out.?("from").|("to"), c, out.?("to").|("from"), o)("Move", n.hl, e.warrior.of(e), "removing", ((n + 2) / 3).hl, self.warrior.of(self))
case class BloodGateConsentAction(self : WarriorFaction, c : Clearing, e : WarriorFaction, out : Boolean, o : Clearing, n : Int) extends ForcedAction
case class BloodGateAgreeAction(self : WarriorFaction, c : Clearing, e : WarriorFaction, out : Boolean, o : Clearing, n : Int, yes : Boolean) extends BaseAction(self, "asks to move", n.hl, e.warrior.of(e), "with", BloodGate)(yes.?("Agree").|("Refuse"))
case class BloodGateMoveAction(self : WarriorFaction, c : Clearing, e : WarriorFaction, out : Boolean, o : Clearing, n : Int) extends ForcedAction

// MEAT FOR THE MASSES
case class MeatAction(self : WarriorFaction, c : Clearing) extends BaseAction(Daylight, "start", MeatForTheMasses, "removes a warrior in")(c)
case class MeatKeepAction(self : Faction, c : Clearing) extends ForcedAction

// BEAR TRAPS
case class BearTrapAction(self : Faction, c : Clearing) extends BaseAction(BearTraps, "places a trap in")(c)

// BOTTOM FEEDER
case class BottomFeederAction(self : Faction, n : Int) extends BaseAction(Evening, BottomFeeder)("Draw", n.cards, "from the discard pile")
case class BottomFeederDeckAction(self : Faction, n : Int) extends BaseAction(Evening, BottomFeeder)("Draw", n.cards, "from the deck")

// GOLDEN KNIGHT
case class KnightAskAction(self : Faction) extends ForcedAction
case class KnightPlaceAction(self : Faction, c : Clearing) extends BaseAction(GoldenKnight, "places the Knight in")(c)
case class KnightHitAction(self : Faction, e : Faction, c : Clearing, then : ForcedAction) extends ForcedAction

// FORBIDDEN SCIENCE
case class ScienceCheckAction(then : ForcedAction) extends ForcedAction
case class SciencePlaceAction(self : WarriorFaction, c : Clearing, then : ForcedAction) extends BaseAction(ForbiddenScience, "places a warrior in")(c)

// DEADLY DEADLY POISON
case class PoisonAction(self : Faction, c : Clearing) extends BaseAction(DeadlyPoison, "serves soup in")(c)
case class PoisonServedAction(self : Faction, c : Clearing, n : Int, random : Faction) extends RandomAction[Faction]

// DOPPLEGANGER
case class DoppleAction(self : Faction, e : Faction, d : CraftEffectCard) extends BaseAction(Doppleganger, "copies a crafted card")(d.img, e) with ViewCard


object DarkDeckExpansion extends Expansion {
    def active(setup : $[Faction], options : $[Meta.O]) = options.has(DarkDeck)

    // The Dark Deck shared supply
    val supply = $[Item](Torch, BookOfNames, RabbitsPaw, CursedIdol)

    def cursed(d : Card) = d.suit == Cursed

    def hasIdol(f : Faction)(implicit game : Game) : Boolean =
        f.forTrade.exists(_.item == CursedIdol) ||
        f.as[Hero].?(_.inv.exists(_.item == CursedIdol)) ||
        factions.of[Hero].%(_.coalition.has(f)).exists(h => h.forTrade.exists(_.item == CursedIdol) || h.inv.exists(_.item == CursedIdol))

    // three clearings, each ruled, linked by paths (one beside both others, or in a row)
    def darkDominanceMet(f : Faction)(implicit game : Game) : Boolean = hasIdol(f) && {
        val ruled = clearings.%(f.rules)
        ruled.exists(c => game.connected(c).intersect(ruled).use(l => l.num >= 2 || l.exists(o => game.connected(o).intersect(ruled).but(c).any)))
    }

    def readyItems(f : Faction)(implicit game : Game) : $[Item] = (f @@ {
        case f : Hero => f.inv.ready./(_.item)
        case f => f.forTrade.ready./(_.item)
    }).but(CursedIdol)

    def exhaustItem(f : Faction, i : Item)(implicit game : Game) {
        f @@ {
            case f : Hero => f.exhaust(i)
            case f =>
                f.forTrade :-= i.pristine
                f.forTrade :+= i.exhaust
        }
    }

    def traps(implicit game : Game) = game.traps

    // enemy pieces can't be placed in or moved from a clearing with a trap
    def trapped(f : Faction, c : Clearing)(implicit game : Game) = game.traps.exists { case (e, o) => o == c && e != f && e.friends(f).not }

    def phased(f : Faction, p : Phase)(implicit game : Game, ask : ActionCollector) {
        if (game.current != f)
            return

        $[CardEffect](RabbitRoulette, BloodGate, InfernalDesigns, Doppleganger).%(f.can).foreach { e =>
            + DarkMenuAction(f, p, e)
        }

        if (p == Daylight) {
            if (f.can(RapaciousRabbit))
                + DarkMenuAction(f, p, RapaciousRabbit)

            if (f.can(BearTraps))
                + DarkMenuAction(f, p, BearTraps)
        }

        if (p == Evening) {
            if (f.can(DeadlyPoison))
                + DarkMenuAction(f, p, DeadlyPoison)
        }
    }

    override def birdsong(f : Faction)(implicit game : Game, ask : ActionCollector) {
        phased(f, Birdsong)
    }

    override def daylight(f : Faction)(implicit game : Game, ask : ActionCollector) {
        phased(f, Daylight)
    }

    override def evening(f : Faction)(implicit game : Game, ask : ActionCollector) {
        phased(f, Evening)
    }

    // the start of Birdsong extras (turn.scala, beside Saboteurs)
    def birdsongStart(f : Faction)(implicit game : Game) : $[UserAction] =
        f.has(WordsOfUndoing).??(factions.but(f)./~(e => e.stuck.$.of[CraftEffectCard]./(d => WordsUndoAction(f, e, d)))) ++
        FoxRabbitMouse.%(s => f.can(Transmutations(s)))./(s => TransmuteMainAction(f, s)) ++
        FoxRabbitMouse.%(s => f.has(Swindle(s)))./(s => SwindleMainAction(f, s).!(swindleCards(f, s).none && swindleItems(f).none, "nothing to swap"))

    def swindleCards(f : Faction, s : BaseSuit)(implicit game : Game) = f.stuck.$.of[CraftEffectCard].%(_.effect != Swindle(s))

    def swindleItems(f : Faction)(implicit game : Game) = f.is[Hero].not.??(f.forTrade.distinct)

    // the extra hits after the roll (battle.scala, beside Sappers and Partisans)
    def postRoll(f : Faction, b : Battle, fh : Int, oh : Int, fe : Int, oe : Int, full : $[Faction], ft : => Int, ot : => Int)(implicit game : Game) : $[UserAction] = {
        var actions : $[UserAction] = $

        val attacker = f == b.attacker

        val then = BattlePostRollAction(b, fh, oh, fe, oe, full)

        FoxRabbitMouse.%(s => f.has(Bloodlust(s))).foreach { s =>
            if (attacker && fh + fe < ot)
                actions :+= BloodlustMainAction(f, s, true, then)

            if (attacker.not && oh + oe < ft)
                actions :+= BloodlustMainAction(f, s, false, then)
        }

        if (f.has(DevotionToTheDeep) && f.used.has(DevotionHit).not && game.riverside.has(b.clearing)) {
            if (attacker && fh + fe < ot)
                actions :+= DevotionHitAction(f, BattlePostRollAction(b, fh, oh, fe + 1, oe, full))

            if (attacker.not && oh + oe < ft)
                actions :+= DevotionHitAction(f, BattlePostRollAction(b, fh, oh, fe, oe + 1, full))
        }

        actions
    }

    def bloodlustCards(f : Faction, s : BaseSuit, l : $[DeckCard])(implicit game : Game) = f.hand.diff(l).%(d => d.suit != Bird && d.matches(s))

    def rouletteClearings(f : Faction)(implicit game : Game) = clearings.%(f.present).%(c => factions.but(f).exists(_.present(c)))

    // starting with the player using it, then in turn order
    def around(f : Faction, l : $[Faction])(implicit game : Game) = {
        val order = factions.dropWhile(_ != f) ++ factions.takeWhile(_ != f)
        order.%(l.has)
    }

    def furyBattles(f : Faction)(implicit game : Game) = clearings.%(f.canAttackIn)./~(c => f.canAttackList(c)./(o => (c, o)))

    def bloodGateWho(f : WarriorFaction, c : Clearing)(implicit game : Game) : $[(WarriorFaction, Boolean)] =
        factions.of[WarriorFaction]./~(e => $(
            (e.at(c).count(e.warrior) - (e == f).??(1) > 0).?((e, true)),
            clearings.but(c).exists(o => e.at(o).count(e.warrior) > 0).?((e, false))
        ).flatten)

    def bloodGateMax(f : WarriorFaction, c : Clearing, e : WarriorFaction, out : Boolean, o : Clearing)(implicit game : Game) = {
        val mine = f.at(c).count(f.warrior)
        val there = e.at(out.?(c).|(o)).count(e.warrior)

        // removing x of f's warriors in c moves up to 3x; if f moves its own out of c, the removed ones aren't moved
        1.to(3 * mine).%(n => (n + 2) / 3 <= mine && n <= there && ((e == f && out).not || n + (n + 2) / 3 <= mine)).maxOr(0)
    }

    def onScience(implicit game : Game) = (factions ++ hirelings).%(game.states.contains)./~(e => e.from(OnForbiddenScience).$)

    def perform(action : Action, soft : Void)(implicit game : Game) = action @@ {
        // CURSED - can't be discarded: back to hand, or left where it was (the discard in `game.scala` keeps it)
        case CursedReturnAction(f, then) =>
            then

        case HandLimitAction(f, n, then) if f.hand.exists(cursed) && f.hand.num > n =>
            val kept = f.hand.%(cursed)
            val rest = f.hand.%!(cursed)
            val min = max(0, n + f.used.has(ActionBag).??(2) - kept.num)
            val most = min + f.has(PublicRelations).??(1) + (f.has(RapaciousRabbit) && game.phase == Evening).??(1)

            if (rest.num <= min)
                then
            else
                XXSelectObjectsAction(f, rest)
                    .withGroup(f.elem ~ " keeps " ~ $(min, most).distinct./(_.cards).join(" or ") ~ " and " ~ kept./(_.elem).comma ~ " to hand limit")
                    .withRule(_.atLeast(min).upTo(most))
                    .withThen(l => HandLimitDiscardAction(f, rest.diff(l), then))(l => "Keep".hl ~ l./(" " ~ _.elem))
                    .withExtra($(NoHand))

        case CraftPerformAction(f, d @ CraftItemCard(Cursed, _, _, item, vp, _), m) =>
            game.uncrafted :-= item
            f.forTrade +:= item.pristine

            f.hand --> d --> game.outOfGameCards

            CraftScoreAction(f, d, d.vp, m, AnotherPlayerCraftsItemAction(f, Repeat))

        case CraftPerformAction(f, d @ CraftEffectCard(_, _, _, GoldenKnight), m) =>
            f.hand --> d --> f.stuck
            f.effects :+= GoldenKnight

            CraftEffectAction(f, d, m, KnightAskAction(f))

        case KnightAskAction(f) =>
            val l = clearings.%(f.present)

            if (l.any)
                Ask(f).group(GoldenKnight, "places the Knight").each(l)(c => KnightPlaceAction(f, c))
            else
                Repeat

        // DOMINANCE
        case CheckDominanceAction(f, then) if f.dominance.has(DarkDominance) && darkDominanceMet(f) =>
            f.vp = 999
            f.log("achieved", "Dark Dominance".styled(Abomination))

            CheckDominanceAction(f, then)

        // MENU
        case DarkMenuAction(f, p, e) =>
            val ask = Ask(f).group(e)

            (e match {
                case RabbitRoulette =>
                    ask.each(rouletteClearings(f))(c => RouletteClearingAction(f, c)).when(rouletteClearings(f).none)(Info("No clearing with your and an enemy's pieces"))

                case BloodGate =>
                    f.as[WarriorFaction] match {
                        case Some(w : WarriorFaction) =>
                            val l = clearings.%(c => w.at(c).count(w.warrior) > 0)
                            ask.each(l)(c => BloodGateClearingAction(w, c).!(bloodGateWho(w, c).none, "nothing to move")).when(l.none)(Info("No warriors on the map"))
                        case None =>
                            ask.add(Info("No warriors"))
                    }

                case InfernalDesigns =>
                    val ii = readyItems(f).distinct
                    val aa = f.craft.distinct
                    val cards = pile.$.of[CraftItemCard].distinct

                    ask.each(ii./~(i => aa./(a => (i, a))))((i, a) => InfernalCraftAction(f, i, a))
                        .each(cards)(d => InfernalTakeMainAction(f, d).!(readyItems(f).num < 2, "two items needed"))
                        .when(ii.none)(Info("No ready items"))

                case Doppleganger =>
                    val l = factions.but(f)./~(e => e.stuck.$.of[CraftEffectCard].%(d => f.has(d.effect).not)./(d => (e, d)))
                    ask.each(l)((e, d) => DoppleAction(f, e, d)).when(l.none)(Info("No crafted cards to copy"))

                case RapaciousRabbit =>
                    ask.each(f.hand.$)(d => RapaciousAction(f, d).!(cursed(d))).when(f.hand.none)(Info("No cards"))

                case BearTraps =>
                    ask.each(clearings.%(f.present))(c => BearTrapAction(f, c).!(game.traps.has((f, c)), "trapped"))

                case DeadlyPoison =>
                    ask.each(clearings.%(f.present))(c => PoisonAction(f, c))

                case _ =>
                    ask
            }).cancel

        // WORDS OF UNDOING - discard
        case WordsUndoAction(f, e, CraftEffectCard(_, _, _, effect)) =>
            f.removeStuckEffect(WordsOfUndoing)

            e.removeStuckEffect(effect)

            f.log("discarded", WordsOfUndoing, "to discard", effect, "of", e)

            Repeat

        // WORDS OF UNDOING - reroll; and the Infected take the higher roll
        case BattleRolledAction(b, fr, or) =>
            DarkRollAction(b, fr, or, $(b.attacker, b.defender).distinct.%(_.can(WordsOfUndoing)))

        case DarkRollAction(b, fr, or, Nil) =>
            BattleRolledDoneAction(b, fr, or)

        case DarkRollAction(b, fr, or, f :: rest) =>
            Ask(f).group(WordsOfUndoing, Break ~ b.attacker.elem ~ " rolled " ~ fr.roll.elem ~ ", " ~ b.defender.elem ~ " rolled " ~ or.roll.elem)
                .each($(fr, or).distinct)(x => WordsRerollAction(f, b, fr, or, $(x), rest).as("Reroll", x.roll))
                .add(WordsRerollAction(f, b, fr, or, $(fr, or), rest).as("Reroll both dice"))
                .add(WordsKeepAction(f, b, fr, or, rest).as("Keep the roll"))
                .needOk

        case WordsKeepAction(f, b, fr, or, rest) =>
            DarkRollAction(b, fr, or, rest)

        case WordsRerollAction(f, b, fr, or, x, rest) =>
            f.used :+= WordsOfUndoing

            val kept = $(fr, or).diff(x)

            Roll[Int](x./(_ => D4), r => WordsRolledAction(f, b, kept, r, rest), f.elem ~ " rerolls with " ~ WordsOfUndoing.elem)

        case WordsRolledAction(f, b, kept, x, rest) =>
            f.log("rerolled", x./(_.roll).comma, "with", WordsOfUndoing)

            val l = kept ++ x

            DarkRollAction(b, l.max, l.min, rest)

        // TRANSMUTATIONS
        case TransmuteMainAction(f, s) =>
            f.used :+= Transmutations(s)

            f.log("used", Transmutations(s))

            TransmuteAction(f, s, clearings.%(_.asset.matches(s)))

        case TransmuteAction(f, s, Nil) =>
            Repeat

        case TransmuteAction(f, s, c :: rest) =>
            val there = factions.of[WarriorFaction].%(_.present(c)).%(e => e.pool(e.warrior)).%(_.canPlace(c))
            val l = there.any.?(there).|(f.as[WarriorFaction].%(e => e.pool(e.warrior)).%(_.canPlace(c)).$)

            if (l.none)
                TransmuteAction(f, s, rest)
            else
                Ask(f).each(l)(e => TransmutePlaceAction(f, s, c, e, rest))

        case TransmutePlaceAction(f, s, c, e, rest) =>
            e.reserve --> e.warrior --> c

            f.log("placed", e.warrior.of(e), "in", c, "with", Transmutations(s))

            TransmuteAction(f, s, rest)

        // BLOODLUST
        case BloodlustMainAction(f, s, a, then) =>
            BloodlustPickAction(f, s, a, $, then)

        case BloodlustPickAction(f, s, a, l, then) =>
            Ask(f).group(Bloodlust(s), "discards", s.elem, "cards for more hits")
                .each(bloodlustCards(f, s, l))(d => BloodlustPickAction(f, s, a, l :+ d, then).as("Add", d))
                .add(BloodlustAction(f, s, a, l, then))
                .cancel

        case BloodlustAction(f, s, a, l, then) =>
            f.removeStuckEffect(Bloodlust(s))

            f.hand --> l --> discard.quiet

            f.log("discarded", Bloodlust(s), l.any.$("and", l), "to deal", (1 + l.num).hits)

            if (a)
                then.copy(fe = then.fe + 1 + l.num)
            else
                then.copy(oe = then.oe + 1 + l.num)

        // DEVOTION TO THE DEEP
        case DevotionHitAction(f, then) =>
            f.used :+= DevotionHit

            f.log("dealt an extra", 1.hit, "with", DevotionToTheDeep)

            then

        // RABBIT ROULETTE
        case RouletteClearingAction(f, c) =>
            f.used :+= RabbitRoulette

            f.log("played", RabbitRoulette, "in", c)

            RouletteAskAction(f, c, around(f, factions.%(_.present(c))))

        case RouletteAskAction(f, c, Nil) =>
            RouletteTopAction(f, c)

        case RouletteAskAction(f, c, e :: rest) if e.hand.none =>
            RouletteAskAction(f, c, rest)

        case RouletteAskAction(f, c, e :: rest) =>
            Ask(e).group(RabbitRoulette, "in", c, "takes a card from", e).each(e.hand.$)(d => RouletteAddAction(f, c, e, d, rest)).needOk

        case RouletteAddAction(f, c, e, d, rest) =>
            e.hand --> d --> game.roulette

            e.log("added a card to the pool")

            RouletteAskAction(f, c, rest)

        case RouletteTopAction(f, c) if deck.none && pile.any =>
            Shuffle[DeckCard](pile, ShufflePileAction(_, RouletteTopAction(f, c)))

        case RouletteTopAction(f, c) =>
            deck.$.starting.foreach { d =>
                deck --> d --> game.roulette

                f.notify($(ViewCardInfoAction(f, RabbitRoulette.elem, d)))

                f.log("looked at the top card of the deck and added it")
            }

            Shuffle[DeckCard](game.roulette, RouletteDealAction(f, c, _))

        case RouletteDealAction(f, c, l) =>
            val l = around(f, factions.%(_.present(c)))

            game.roulette.$.indexed.foreach { (d, i) =>
                val e = l(i % l.num)

                game.roulette --> d --> e.hand

                e.notify($(ViewCardInfoAction(e, RabbitRoulette.elem, d)))
            }

            f.log("dealt out the pool")

            Repeat

        // RAPACIOUS RABBIT
        case RapaciousAction(f, d) =>
            f.used :+= RapaciousRabbit

            f.hand --> d --> discard(f, "with", RapaciousRabbit)

            DrawCardsAction(f, 2, WithEffect(RapaciousRabbit), AddCardsAction(f, Repeat))

        // SWINDLE
        case SwindleMainAction(f, s) =>
            Ask(f).group(Swindle(s), "swaps a crafted card or item")
                .each(swindleCards(f, s))(d => SwindleCardMainAction(f, s, d))
                .each(swindleItems(f))(i => SwindleItemMainAction(f, s, i))
                .cancel

        case SwindleCardMainAction(f, s, d) =>
            val l = factions.but(f)./~(e => e.stuck.$.of[CraftEffectCard]./(x => (e, x)))
            Ask(f).group(Swindle(s), "swaps", d, "for")
                .each(l)((e, x) => SwindleCardAction(f, s, d, e, x).!(f.has(x.effect) || d.as[CraftEffectCard].?(d => e.has(d.effect)), "already has it"))
                .cancel

        case SwindleItemMainAction(f, s, i) =>
            val l = factions.but(f).%!(_.is[Hero])./~(e => e.forTrade.distinct./(x => (e, x)))
            Ask(f).group(Swindle(s), "swaps", i, "for")
                .each(l)((e, x) => SwindleItemAction(f, s, i, e, x))
                .cancel

        case SwindleCardAction(f, s, d, e, x) =>
            f.removeStuckEffect(Swindle(s))

            f.stuck --> d --> e.stuck
            e.stuck --> x --> f.stuck

            d.as[CraftEffectCard].foreach { d =>
                f.effects :-= d.effect
                e.effects :+= d.effect
                swapped(f, e, d.effect)
            }

            x.as[CraftEffectCard].foreach { x =>
                e.effects :-= x.effect
                f.effects :+= x.effect
                swapped(e, f, x.effect)
            }

            f.log("discarded", Swindle(s), "and swapped", d, "for", x, "of", e)

            Repeat

        case SwindleItemAction(f, s, i, e, x) =>
            f.removeStuckEffect(Swindle(s))

            f.forTrade :-= i
            e.forTrade :-= x
            f.forTrade :+= x
            e.forTrade :+= i

            f.log("discarded", Swindle(s), "and swapped", i, "for", x, "of", e)

            Repeat

        // INFERNAL DESIGNS
        case InfernalCraftAction(f, i, a) =>
            f.used :+= InfernalDesigns

            exhaustItem(f, i)

            f.extraCraft :+= a

            f.log("exhausted", i.exhaust, "with", InfernalDesigns, "to activate", dt.CraftSuit(a), "again")

            Repeat

        case InfernalTakeMainAction(f, d) =>
            val ii = readyItems(f)

            Ask(f).group(InfernalDesigns, "takes", d)
                .each(ii.indices.$./~(i => ii.indices.$.%(_ > i)./(j => (ii(i), ii(j)))).distinct)((a, b) => InfernalTakeAction(f, d, a, b))
                .cancel

        case InfernalTakeAction(f, d, a, b) =>
            f.used :+= InfernalDesigns

            exhaustItem(f, a)
            exhaustItem(f, b)

            pile --> d --> f.hand

            f.log("exhausted", a.exhaust, "and", b.exhaust, "with", InfernalDesigns, "to take", d, "from the discard pile")

            Repeat

        // MEAT FOR THE MASSES - start of Daylight
        case DaylightNAction(0, f) if f.can(MeatForTheMasses) && f.as[WarriorFaction].?(f => clearings.exists(c => f.at(c).has(f.warrior))) =>
            val w = f.as[WarriorFaction].get

            Ask(f).group("Start of " ~ Daylight.elem)
                .each(clearings.%(c => w.at(c).has(w.warrior)))(c => MeatAction(w, c))
                .skip(UsedEffectAction(f, MeatForTheMasses, DaylightNAction(0, f)))

        case MeatAction(f, c) =>
            f.used :+= MeatForTheMasses

            f.from(c) --> f.warrior --> game.recycle

            f.log("removed", f.warrior.of(f), "in", c, "with", MeatForTheMasses)

            DrawCardsAction(f, 2, WithEffect(MeatForTheMasses), MeatKeepAction(f, c))

        case MeatKeepAction(f, c) =>
            val l = f.drawn.$
            val keep = l.%(_.matches(c.cost))

            f.log("revealed", l, keep.any.$("and took", keep))

            f.drawn --> keep --> f.hand
            f.drawn --> discard.quiet

            DaylightNAction(0, f)

        // CLEANSING FIRE and FOAMING FURY - start of Evening
        case EveningNAction(0, f) if f.can(CleansingFire) || f.can(FoamingFury) =>
            val fire = f.can(CleansingFire).??(clearings.%(f.present).%(c => f.enemies.exists(_.present(c))))

            Ask(f).group("Start of " ~ Evening.elem)
                .each(fire)(c => CleansingMainAction(f, c).!(f.hand.%(_.matches(c.cost)).num < 2, "two matching cards needed"))
                .when(f.can(FoamingFury))(FuryMainAction(f, false).as(FoamingFury))
                .skip(UsedEffectAction(f, CleansingFire, UsedEffectAction(f, FoamingFury, EveningNAction(0, f))))

        case CleansingMainAction(f, c) =>
            CleansingPickAction(f, c, $)

        case CleansingPickAction(f, c, l) =>
            Ask(f).group(CleansingFire, "spends two", c.cost.elem, "cards")
                .each(f.hand.diff(l).%(_.matches(c.cost)))(d => (l.num == 1).?[UserAction](CleansingFireAction(f, c, l :+ d)).|(CleansingPickAction(f, c, l :+ d).as("Spend", d)))
                .cancel

        case CleansingFireAction(f, c, l) =>
            f.used :+= CleansingFire

            f.hand --> l --> discard.quiet

            f.log("spent", l, "with", CleansingFire, "in", c)

            NukeAction(f, f.enemies, $(c), NukeType.ClearSector, EveningEndAction(f))

        case FuryMainAction(f, moved) =>
            val l = furyBattles(f)
            val m = f.moveFrom

            Ask(f).group(FoamingFury, moved.?("battles until either side can't take hits"))
                .when(moved.not)(MoveInitAction(f, f, $, WithEffect(FoamingFury), m, f.movable, $(CancelAction), FuryMovedAction(f)).as("Move".hl, dt.Move).!(m.none, "can't move"))
                .each(l)((c, o) => FuryBattleAction(f, c, o, moved))
                .add(FuryDoneAction(f).as(moved.?("Done").|("Skip")))
                .needOk

        case FuryMovedAction(f) =>
            FuryMainAction(f, true)

        case FuryBattleAction(f, c, o, again) =>
            BattleStartAction(f, f, f, WithEffect(FoamingFury), c, o, None, again.?(FuryAgainAction(f, c, o, FuryDoneAction(f))).|(FuryDoneAction(f)))

        case FuryAgainAction(f, c, o, then) =>
            if (f.canAttackList(c).has(o) && o.present(c)) {
                f.log("fought again with", FoamingFury)

                BattleStartAction(f, f, f, WithEffect(FoamingFury), c, o, None, FuryAgainAction(f, c, o, then))
            }
            else
                then

        case FuryDoneAction(f) =>
            UsedEffectAction(f, FoamingFury, EveningNAction(0, f))

        // BLOOD GATE
        case BloodGateClearingAction(f, c) =>
            Ask(f).group(BloodGate, "in", c)
                .each(bloodGateWho(f, c))((e, out) => BloodGateWhoAction(f, c, e, out))
                .cancel

        case BloodGateWhoAction(f, c, e, out) =>
            val l = clearings.but(c).%(o => bloodGateMax(f, c, e, out, o) > 0)

            Ask(f).group(BloodGate, "moves", e, "warriors", out.?("out of").|("into"), c, out.?("to").|("from"))
                .each(l)(o => BloodGateWhereAction(f, c, e, out, o))
                .cancel

        case BloodGateWhereAction(f, c, e, out, o) =>
            Ask(f).group(BloodGate)
                .each(1.to(bloodGateMax(f, c, e, out, o)).$)(n => BloodGateAction(f, c, e, out, o, n))
                .cancel

        case BloodGateAction(f, c, e, out, o, n) if e != f =>
            BloodGateConsentAction(f, c, e, out, o, n)

        case BloodGateAction(f, c, e, out, o, n) =>
            BloodGateMoveAction(f, c, e, out, o, n)

        case BloodGateConsentAction(f, c, e, out, o, n) =>
            Ask(e).group(f, "opens a", BloodGate, "to move", n.hl, e.warrior.of(e), out.?("from").|("to"), c, out.?("to").|("from"), o)
                .add(BloodGateAgreeAction(f, c, e, out, o, n, true))
                .add(BloodGateAgreeAction(f, c, e, out, o, n, false))
                .needOk

        case BloodGateAgreeAction(f, c, e, out, o, n, true) =>
            e.log("agreed")

            BloodGateMoveAction(f, c, e, out, o, n)

        case BloodGateAgreeAction(f, c, e, out, o, n, false) =>
            e.log("refused", BloodGate)

            Repeat

        case BloodGateMoveAction(f, c, e, out, o, n) =>
            f.used :+= BloodGate

            val x = (n + 2) / 3

            x.times(f.warrior).foreach { w =>
                f.from(c) --> w --> game.recycle
            }

            val (from, to) = out.?((c, o)).|((o, c))

            n.times(e.warrior).foreach { w =>
                e.from(from) --> w --> to
            }

            f.log("removed", x.times(f.warrior.of(f)).comma, "in", c, "to open", BloodGate)
            e.log("moved", n.times(e.warrior.of(e)).comma, "from", from, "to", to)

            Repeat

        // BEAR TRAPS
        case BearTrapAction(f, c) =>
            f.used :+= BearTraps

            game.traps :+= (f, c)

            f.log("placed a trap in", c, "with", BearTraps)

            Repeat

        // BOTTOM FEEDER
        case EveningDrawAction(f, n) if f.can(BottomFeeder) && pile.any && n > 0 =>
            Ask(f).group(BottomFeeder)
                .add(BottomFeederAction(f, n))
                .add(BottomFeederDeckAction(f, n))
                .needOk

        case BottomFeederDeckAction(f, n) =>
            UsedEffectAction(f, BottomFeeder, EveningDrawAction(f, n))

        case BottomFeederAction(f, n) =>
            f.used :+= BottomFeeder

            val l = pile.$.takeRight(n)

            pile --> l --> f.hand

            f.log("drew", l, "from the discard pile with", BottomFeeder)

            if (l.num < n)
                DrawCardsAction(f, n - l.num, NotInLog(OnTurn(game.turn)), AddCardsAction(f, EveningHandLimitAction(f)))
            else
                EveningHandLimitAction(f)

        // GOLDEN KNIGHT
        case KnightPlaceAction(f, c) =>
            game.knights :+= (f, c)

            f.log("placed the Knight in", c, "with", GoldenKnight)

            Repeat

        case MoveCompleteAction(self, f, from, to : Clearing, l, e, then) if e.has(Neutral).not && game.knights.exists { case (k, c) => c == to && k != f && k.friends(f).not && k.present(to) } =>
            val q = game.knights.%{ case (k, c) => c == to && k != f && k.friends(f).not && k.present(to) }.lefts.foldLeft(then)((q, k) => KnightHitAction(k, f, to, q))

            MoveCompleteAction(self, f, from, to, l, e :+ Neutral, q)

        case KnightHitAction(k, f, c, then) =>
            if (f.present(c) && k.canRemove(c)(f)) {
                k.log("hit", f, "in", c, "with", GoldenKnight)

                BattleAssignHitsAction(f, Battle(c, k, None, f, None, None, None, 0, 0, 0, then), 1, 0, ForcedRemoveFinishedAction(k, then))
            }
            else
                then

        // FORBIDDEN SCIENCE
        case ForcedRemoveCleanupAction(e, then) if factions.exists(_.has(ForbiddenScience)) && (factions ++ hirelings).%(game.states.contains).exists(x => board.clearings.exists(c => x.limbo(c).$.exists(_.piece.is[Warrior]))) =>
            (factions ++ hirelings).%(game.states.contains).foreach { x =>
                board.clearings.foreach { c =>
                    x.limbo(c).$.%(_.piece.is[Warrior]).foreach { w =>
                        w --> OnForbiddenScience
                    }
                }
            }

            factions.%(_.has(ForbiddenScience)).foreach(_.log("put the removed warriors on", ForbiddenScience))

            ForcedRemoveCleanupAction(e, ScienceCheckAction(then))

        case ScienceCheckAction(then) if onScience.num >= 3 =>
            val l = onScience.take(3)

            l.foreach { w =>
                w --> Pool
            }

            val o = factions.%(_.has(ForbiddenScience)).starting

            o.foreach(_.log("returned", l./(w => w.piece.of(w.faction)).comma, "from", ForbiddenScience))

            o match {
                case Some(f : WarriorFaction) if f.pool(f.warrior) && clearings.%(f.rules).%(f.canPlace).any =>
                    Ask(f).group(ForbiddenScience).each(clearings.%(f.rules).%(f.canPlace))(c => SciencePlaceAction(f, c, ScienceCheckAction(then)))
                case _ =>
                    ScienceCheckAction(then)
            }

        case ScienceCheckAction(then) =>
            then

        case SciencePlaceAction(f, c, then) =>
            f.reserve --> f.warrior --> c

            f.log("placed", f.warrior.of(f), "in", c, "with", ForbiddenScience)

            then

        // DEADLY DEADLY POISON
        case PoisonAction(f, c) =>
            f.used :+= DeadlyPoison

            val l = factions.%(_.present(c))

            f.log("served", l.num.hl, "bowls of soup in", c, "with", DeadlyPoison)

            Random[Faction](l, r => PoisonServedAction(f, c, l.num, r))

        case PoisonServedAction(f, c, n, e) =>
            e.log("took the poison")

            e.oscore(-n)("from", DeadlyPoison)

            Repeat

        // DOPPLEGANGER
        case DoppleAction(f, e, d) =>
            f.used :+= Doppleganger

            f.dopple = $(d.effect)

            f.log("copied", d, "of", e, "with", Doppleganger)

            Repeat

        case CleanUpAction(f) if factions.exists(_.dopple.any) =>
            factions.foreach(_.dopple = $)

            CleanUpAction(f)

        case _ => UnknownContinue
    }

    // a crafted card changed hands (Mouster Criminal, Felonious Fox): what it put on the map goes with it
    def swapped(f : Faction, e : Faction, effect : Effect)(implicit game : Game) {
        if (effect == BearTraps)
            game.traps = game.traps./{ case (o, c) => (o == f).?((e, c)).|((o, c)) }

        if (effect == GoldenKnight)
            game.knights = game.knights./{ case (o, c) => (o == f).?((e, c)).|((o, c)) }
    }

    // a crafted card was discarded (`removeStuckEffect` in `game.scala`)
    def discarded(f : Faction, effect : Effect)(implicit game : Game) {
        if (effect == BearTraps) {
            if (game.traps.exists(_._1 == f))
                f.log("removed the traps of", BearTraps)

            game.traps = game.traps.%((o, _) => o != f)
        }

        if (effect == GoldenKnight)
            game.knights = game.knights.%((o, _) => o != f)

        if (effect == ForbiddenScience && factions.but(f).exists(_.has(ForbiddenScience)).not)
            onScience.foreach { w =>
                w --> Pool
            }
    }
}
