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


// The Doomed Vagabonds (The Doomed Expansion, fan-made by slugfacekillah; Law of Doom D7; Gman, 2026-11-15): seven more Vagabond
// characters, offered with the option `DoomedVagabonds`. Their starting items and special actions follow the cards (the card shows the
// item each special action exhausts); the Law of Doom says nothing more about them.
//
// Readings where the cards leave room:
// - Bard's Inspire: each card matching the clearing discarded takes one hit in place of an item, the Bard picking how many as hits come
//   (in battle, as attacker or defender), and the Bard draws a card at the end of every battle it is in
// - Berserker's Raging: a battle that exhausts two swords in place of one; after the roll the Berserker deals extra hits equal to the
//   lower of the two dice
// - Blacksmith's Hammertime: the Repair action, exhausting any other item in place of a hammer
// - Barkeep's Encouragement: once in Daylight, exhaust a tea to ready two exhausted items (or as many as there are) or draw two cards
// - Swindler's Grift: exhaust a torch, then swap an unexhausted, undamaged item with a different one in the shared supply
// - Zealot's Devour: exhaust a torch, then discard bird cards one by one, 1 point each
// - Grouch's Scram!: exhaust a torch; each enemy (hirelings too) moves half its warriors and pawns there (rounded up) to adjacent
//   clearings of its choice, in one move each, ignoring rule

case object Inspire extends FactionEffect with DisplayEffect with BattleEffect { val name = "Inspire" }
case object Raging extends FactionEffect with DisplayEffect { val name = "Raging" }
case object Hammertime extends FactionEffect with DisplayEffect { val name = "Hammertime" }
case object Encouragement extends FactionEffect with DisplayEffect { val name = "Encouragement" }
case object Grift extends FactionEffect with DisplayEffect { val name = "Grift" }
case object Devour extends FactionEffect with DisplayEffect { val name = "Devour" }
case object Scram extends FactionEffect with DisplayEffect { val name = "Scram!" }

// battle markers, cleared when the battle ends
case object RagingBattle extends BattleEffect
case object InspireDone extends BattleEffect
case object InspireDrawn extends BattleEffect

case object Bard extends Character {
    def starting(options : $[Meta.O]) = $(Torch, Teapot, Boots, Coins)
    val special = $(Inspire)
}

case object Barkeep extends Character {
    def starting(options : $[Meta.O]) = $(Torch, Teapot, Crossbow, Hammer)
    val special = $(Encouragement)
}

case object Berserker extends Character {
    def starting(options : $[Meta.O]) = $(Torch, Teapot, Sword, Sword)
    val special = $(Raging)
}

case object Blacksmith extends Character {
    def starting(options : $[Meta.O]) = $(Torch, Hammer, Coins, Bag)
    val special = $(Hammertime)
}

case object Swindler extends Character {
    def starting(options : $[Meta.O]) = $(Torch, Boots, Sword, Bag)
    val special = $(Grift)
}

case object Zealot extends Character {
    def starting(options : $[Meta.O]) = $(Torch, Boots, Coins, Crossbow)
    val special = $(Devour)
}

case object Grouch extends Character {
    def starting(options : $[Meta.O]) = $(Torch, Bag, Coins, Crossbow)
    val special = $(Scram)
}

object DoomedCharacters {
    val characters = $[Character](Bard, Barkeep, Berserker, Blacksmith, Swindler, Zealot, Grouch)
}


case class RagingMainAction(self : Hero, c : Clearing) extends OptionAction(Raging.of(self), Sword.img, Sword.img) with VBDaylightQuestion with Soft with Only[BattleStartAction] { def tag = implicitly }
case class RagingStartAction(self : Hero, then : ForcedAction) extends ForcedAction

case class InspireDiscardAction(self : Hero, b : Battle, n : Int, s : Int, d : DeckCard, then : ForcedAction) extends BaseAction(Inspire.of(self), "discard to take a hit")(d.img) with ViewCard
case class InspireDoneAction(self : Hero, b : Battle, n : Int, s : Int, then : ForcedAction) extends BaseAction(None)("Damage items".hl)

case class HammertimeMainAction(self : Hero) extends OptionAction(Hammertime.of(self), Image("item-any", styles.piece)) with VBDaylightQuestion with Soft with Only[HammertimeAction] { def tag = implicitly }
case class HammertimeRepairAction(self : Hero, i : ItemRef) extends ForcedAction with Soft
case class HammertimeAction(self : Hero, i : ItemRef, e : Item) extends ForcedAction

case class EncouragementMainAction(self : Hero) extends OptionAction(Encouragement.of(self), Teapot.img) with VBDaylightQuestion with Soft with Only[EncouragementDrawAction] { def tag = implicitly }
case class EncouragementReadyMainAction(self : Hero, n : Int) extends BaseAction(Encouragement.of(self))("Ready", n.hl, "items") with Soft
case class EncouragementReadyAction(self : Hero, l : $[ItemRef]) extends ForcedAction
case class EncouragementDrawAction(self : Hero) extends BaseAction(Encouragement.of(self))("Draw", 2.hl, "cards")

case class GriftMainAction(self : Hero) extends OptionAction(Grift.of(self), Torch.img) with VBDaylightQuestion with Soft with Only[GriftAction] { def tag = implicitly }
case class GriftItemAction(self : Hero, i : Item) extends BaseAction(Grift.of(self), "swap")(i.img, i) with Soft
case class GriftAction(self : Hero, i : Item, j : Item) extends BaseAction(Grift.of(self), "swap", i, "for")(j.img, j)

case class DevourMainAction(self : Hero) extends OptionAction(Devour.of(self), Torch.img) with VBDaylightQuestion with Soft with Only[DevourAction] { def tag = implicitly }
case class DevourAction(self : Hero, d : DeckCard, first : Boolean) extends BaseAction(Devour.of(self), "discard a bird card for", 1.vp)(d.img) with ViewCard

case class ScramMainAction(self : Hero, c : Clearing, l : $[Faction]) extends OptionAction(Scram.of(self), Torch.img) with VBDaylightQuestion with Soft with Only[ScramAction] { def tag = implicitly }
case class ScramAction(self : Hero, c : Clearing, l : $[Faction]) extends BaseAction(Scram.of(self), "in", c)("Scram!".hl, l./(_.elem).comma)
case class ScramNextAction(self : Hero, c : Clearing, l : $[Faction]) extends ForcedAction
case class ScramMoveAction(self : Faction, h : Hero, c : Clearing, d : Clearing, l : $[Faction]) extends BaseAction("Scram!".styled(h), "from", c, "to")(d)


object DoomedVagabondsExpansion extends Expansion {
    def active(setup : $[Faction], options : $[Meta.O]) = options.has(DoomedVagabonds) && setup.of[Hero].any

    def character(f : Faction)(implicit game : Game) = f.as[Hero].%(game.states.contains)./~(_.character)

    // the pieces Scram! moves: half the warriors and pawns, rounded up, warriors first
    def scrammed(e : Faction, c : Clearing)(implicit game : Game) : $[Piece] = {
        val l = e.at(c).%(p => p.is[Warrior] || p.is[Pawn])
        val sorted = l.%(_.is[Warrior]) ++ l.%!(_.is[Warrior])
        sorted.take((l.num + 1) / 2)
    }

    override def daylight(f : Faction)(implicit game : Game, ask : ActionCollector) {
        if (game.current != f)
            return

        f.as[Hero].foreach { h =>
            val c = h.region.as[Clearing]

            if (h.can(Raging))
                c.foreach { c =>
                    val att = h.canAttackList(c)
                    + RagingMainAction(h, c).!(h.ready(Sword) < 2, "not enough swords").!(att.none, "no enemy")
                }

            if (h.can(Hammertime))
                + HammertimeMainAction(h).!(h.inv.damaged.none, "no damaged items").!(h.inv.ready.%!(_.damaged).none, "no items")

            if (h.can(Encouragement))
                + EncouragementMainAction(h).!(h.lacks(Teapot), "no tea")

            if (h.can(Grift))
                + GriftMainAction(h).!(h.lacks(Torch), "no torch").!(game.uncrafted.none, "empty supply")

            if (h.can(Devour))
                + DevourMainAction(h).!(h.lacks(Torch), "no torch").!(h.hand.%(_.suit == Bird).none, "no bird cards")

            if (h.can(Scram))
                c.foreach { c =>
                    val l = (factions ++ hirelings).but(h).%(game.states.contains).%(e => scrammed(e, c).any)
                    + ScramMainAction(h, c, l).!(h.lacks(Torch), "no torch").!(l.none, "no enemies")
                }
        }
    }

    def perform(action : Action, soft : Void)(implicit game : Game) = action @@ {
        // RAGING
        case RagingMainAction(f, c) =>
            BattleInitAction(f, f, WithItems(Sword, Sword), $(c), $(CancelAction), RagingStartAction(f, Repeat))

        case BattleStartAction(self, f, a, m, c, o, i, RagingStartAction(hero, then)) =>
            hero.exhaust(Sword)
            hero.exhaust(Sword)

            hero.used :+= RagingBattle

            hero.log("is", Raging.of(hero))

            BattleStartAction(self, f, a, m, c, o, i, then)

        case BattleBonusAction(b, f : Hero, o, fs, os, fr, or, fh, oh, fe, oe) if f.used.has(RagingBattle) =>
            f.used :-= RagingBattle

            val n = min(fr, or)

            if (n > 0)
                f.log("dealt", n.hits, "more with", Raging.of(f))

            BattleBonusAction(b, f, o, fs, os, fr, or, fh, oh, fe + n, oe)

        // INSPIRE
        case BattleAssignHitsAction(f : Hero, b, n, s, then) if n > 0 && f.has(Inspire) && f.used.has(InspireDone).not && f.hand.%(_.matches(b.clearing.cost)).any =>
            Ask(f).group(Inspire.of(f), "in", b.clearing, SpacedDash, n.hits, "to take")
                .each(f.hand.%(_.matches(b.clearing.cost)))(d => InspireDiscardAction(f, b, n, s, d, then))
                .add(InspireDoneAction(f, b, n, s, then))

        case InspireDiscardAction(f, b, n, s, d, then) =>
            f.hand --> d --> discard.quiet

            f.log("discarded", d, "to take a hit with", Inspire.of(f))

            if (n > 1)
                BattleAssignHitsAction(f, b, n - 1, s, then)
            else
                VagabondDealHitsAlliesAction(f, b, $, $, then)

        case InspireDoneAction(f, b, n, s, then) =>
            f.used :+= InspireDone

            BattleAssignHitsAction(f, b, n, s, then)

        case VagabondDealHitsAction(f, l, then) if f.used.has(InspireDone) =>
            f.used :-= InspireDone

            VagabondDealHitsAction(f, l, then)

        case BattleFinishedAction(b) if b.parties.of[Hero].%(game.states.contains).exists(h => h.has(Inspire) && h.used.has(InspireDrawn).not) =>
            val h = b.parties.of[Hero].%(game.states.contains).%(h => h.has(Inspire) && h.used.has(InspireDrawn).not).first

            h.used :+= InspireDrawn

            h.log("drew a card after the battle with", Inspire.of(h))

            DrawCardsAction(h, 1, NoMessage, AddCardsAction(h, BattleFinishedAction(b)))

        // HAMMERTIME
        case HammertimeMainAction(f) =>
            SelectHeroItemsAction(f, Hammertime.of(f) ~ SpacedDash ~ "Repair an item", $(CancelAction))(_.num(1))(ToRepair(_))(l => HammertimeRepairAction(f, l(0)))

        case HammertimeRepairAction(f, i) =>
            SelectHeroItemsAction(f, Hammertime.of(f) ~ SpacedDash ~ "Exhaust an item to repair " ~ i.elem, $(CancelAction))(_.num(1))(ToExhaust(_))(l => HammertimeAction(f, i, l(0).item))

        case HammertimeAction(f, i, e) =>
            f.exhaust(e)

            val r = i.copy(damaged = false)

            f.inv :-= i
            f.inv :+= r

            f.log("repaired", r, "with", Hammertime.of(f), "exhausting", e.exhaust)

            Repeat

        // ENCOURAGEMENT
        case EncouragementMainAction(f) =>
            val n = min(2, f.inv.exhausted.num)

            Ask(f).group(Encouragement.of(f), "exhausting", Teapot.exhaust)
                .add(EncouragementReadyMainAction(f, n).!(n == 0, "no exhausted items"))
                .add(EncouragementDrawAction(f))
                .cancel

        case EncouragementReadyMainAction(f, n) =>
            SelectHeroItemsAction(f, Encouragement.of(f) ~ SpacedDash ~ "Ready " ~ n.hl ~ " items", $(CancelAction))(_.num(n))(ToRefresh(_))(l => EncouragementReadyAction(f, l))

        case EncouragementReadyAction(f, l) =>
            f.exhaust(Teapot)

            f.used :+= Encouragement

            l.foreach { i =>
                f.inv :-= i
                f.inv :+= i.copy(exhausted = false)
            }

            f.log("exhausted", Teapot.exhaust, "and readied", l, "with", Encouragement.of(f))

            Repeat

        case EncouragementDrawAction(f) =>
            f.exhaust(Teapot)

            f.used :+= Encouragement

            f.log("exhausted", Teapot.exhaust, "to draw", 2.cards, "with", Encouragement.of(f))

            DrawCardsAction(f, 2, NoMessage, AddCardsAction(f, Repeat))

        // GRIFT
        case GriftMainAction(f) =>
            Ask(f).group(Grift.of(f), "with", Torch.exhaust)
                .each(f.inv.%!(_.exhausted).%!(_.damaged)./(_.item).distinct.but(CursedIdol))(i => GriftItemAction(f, i).!(game.uncrafted.but(i).none))
                .cancel

        case GriftItemAction(f, i) =>
            Ask(f).group(Grift.of(f), "swap", i, "for")
                .each(game.uncrafted.distinct.but(i))(j => GriftAction(f, i, j))
                .cancel

        case GriftAction(f, i, j) =>
            f.exhaust(Torch)

            f.inv :-= i.pristine
            f.inv :+= j.pristine

            game.uncrafted :-= j
            game.uncrafted :+= i

            f.log("swapped", i, "for", j, "from the supply with", Grift.of(f), "and", Torch.exhaust)

            Repeat

        // DEVOUR
        case DevourMainAction(f) =>
            Ask(f).group(Devour.of(f), "with", Torch.exhaust)
                .each(f.hand.%(_.suit == Bird))(d => DevourAction(f, d, true))
                .cancel

        case DevourAction(f, d, first) =>
            if (first) {
                f.exhaust(Torch)
                f.log("exhausted", Torch.exhaust, "to", Devour.of(f))
            }

            f.hand --> d --> discard.quiet

            f.nscore(1)("devouring", d)(f, "discarded", d, "and", ScoredVP)

            val l = f.hand.%(_.suit == Bird)

            if (l.none)
                Repeat
            else
                Ask(f).group(Devour.of(f))
                    .each(l)(d => DevourAction(f, d, false))
                    .done(Repeat)

        // SCRAM!
        case ScramMainAction(f, c, l) =>
            Ask(f).add(ScramAction(f, c, l)).cancel

        case ScramAction(f, c, l) =>
            f.exhaust(Torch)

            f.log("exhausted", Torch.exhaust, "and shouted", Scram.of(f), "in", c)

            ScramNextAction(f, c, l)

        case ScramNextAction(f, c, l) =>
            l.%(e => scrammed(e, c).any) match {
                case e :: rest =>
                    val dd = game.connected(c)

                    if (dd.none) {
                        e.log("had nowhere to go from", c)
                        ScramNextAction(f, c, rest)
                    }
                    else
                        Ask(e).group(e, "must move", scrammed(e, c)./(_.of(e)).comma, "from", c, "to")
                            .each(dd)(d => ScramMoveAction(e, f, c, d, rest))
                case _ =>
                    Repeat
            }

        case ScramMoveAction(e, f, c, d, rest) =>
            val l = scrammed(e, c)

            game.highlights :+= MoveHighlight(c, d)

            e.from(c) --> l --> d

            e.log("moved", l./(_.of(e)).comma, "from", c, "to", d, "with", Scram.of(f))

            ScramNextAction(f, c, rest)

        case _ => UnknownContinue
    }
}
