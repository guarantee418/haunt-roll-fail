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
import hrf.meta._
import hrf.options._
import root.elem._

import helper.&

// The Doomed Expansion's Legendary Monsters (slugfacekillah, Law of Doom D5.M) and the two Legends Scenarios that use them,
// Dark Rituals (D5.3) and Hunt the Legend (D5.4). The scenario space itself is in root/legends.scala.
//
// Readings (the rules leave these open):
// - The monster is its own player (`LegendBeast`, no owner, like the Dark Deck's Infected) with one pawn; its health is
//   `game.monsterHealth`, 1 being the ghost face. It deals the full roll (it counts as three warriors) and its card's extra hits
//   in every battle it is in; Shadowmaw takes only extra hits (the rolled ones dealt to it are lost), the Red Knight ignores the
//   first hit of each battle, the White Wolf takes the higher roll when defending
// - Roaming: at the start of each Evening from the second round, the player whose turn it is may discard a card of the monster's
//   clearing (any card in the unsuited space is a bird) to skip it; else the top card of the deck is discarded: matching the
//   clearing (birds always do) and the player there, the monster attacks them; otherwise they move it to an adjacent clearing the
//   card matches (birds: any), if there is one
// - Monsters block: no piece moves out of the monster's clearing until its player has paid to appease it (a card of the clearing,
//   for the rest of the turn); in Dark Rituals nothing moves out of the scenario space at all
// - Slaying: at the ghost face, on their turn, a player holding both of the card's items (a Vagabond's ready, which it exhausts)
//   removes the monster and takes its weapon, as an item that never leaves play but by a capture (the capture rule then takes any
//   item). The weapons: The Mauler (an extra hit, or that enemy discards a random card), Spirit Sword (an extra hit, and the
//   higher roll when defending), Bloody Chainsaw (two extra hits, once per turn), The Slaymore (three extra hits, once on your
//   turn) and Bushblazer (an extra hit; once in Evening, every enemy piece with none of its warriors in a clearing of yours is
//   removed); all of them used whenever they can
// - Dark Rituals: the monster is picked in setup (or drawn among those whose items can be had). A player alone in the space (but
//   for the monster) may remove one of its warriors there (a Vagabond damages an item) to move the monster once to an adjacent
//   clearing or have it attack anyone in its clearing; enemy pieces it removes then score for that player
// - Hunt the Legend: three markers drawn among the monsters whose items can be had. The die moves as the monster would (it never
//   attacks), and loses 1 each time it roams, moved or not. Investigating costs a card of a clearing with the player's pieces; a
//   peeked marker placed on the map goes face down in that clearing, and the last marker can't be placed that way

case object MonsterPawn extends Pawn with Attacking with Tenacious

// the Legends die (1 is the ghost) and the monster markers, drawn on the map
case class LegendsDie(n : Int) extends Token {
    override def id = "die"
}
case class MonsterMarker(m : |[LegendMonster]) extends Token {
    override def id = "marker"
}

case object LegendBeast extends Hireling {
    val name = "Legendary Monster"
    val short = "LGM"
    val style = "lgm"

    val demoted = false
    val setup = false

    val clashKey = LegendBeast

    def pieces(options : $[Meta.O]) = MonsterPawn *** 1
}

abstract class LegendMonster(val title : String, val health : Int, val slay : $[Item], val weapon : Item, val id : String, val ability : String) extends Record with Elementary {
    def elem = title.styled(LegendBeast)
}

case object Shadowmaw extends LegendMonster("Shadowmaw", 2, $(Torch, RabbitsPaw), Mauler, "shadowmaw", "can only be dealt extra hits")
case object WhiteWolf extends LegendMonster("White Wolf", 3, $(Teapot, Coins), SpiritSword, "white-wolf", "uses the higher roll in battle")
case object Beaverbot extends LegendMonster("Beaverbot", 4, $(Sword, Hammer), Chainsaw, "beaverbot", "deals two extra hits")
case object RedKnight extends LegendMonster("Red Knight", 5, $(Crossbow, Boots), Slaymore, "red-knight", "ignores one hit")
case object DemonBear extends LegendMonster("Demon Bear", 6, $(Crossbow, Torch), Bushblazer, "demon-bear", "deals one extra hit")

object LegendMonster {
    def all = $[LegendMonster](Shadowmaw, WhiteWolf, Beaverbot, RedKnight, DemonBear)
}


case class MonsterSetupAction(then : ForcedAction) extends ForcedAction
case class MonsterChosenAction(m : LegendMonster, then : ForcedAction) extends RandomAction[LegendMonster] { def random = m }
case class HuntStackAction(shuffled : $[LegendMonster], then : ForcedAction) extends ShuffledAction[LegendMonster]

case class RoamAction(self : Faction, then : ForcedAction) extends ForcedAction
case class RoamAppeaseAction(self : Faction, c : Clearing, d : DeckCard, then : ForcedAction) extends BaseAction(self, "can pay to appease the", "Monster".styled(LegendBeast), "before it roams")(d.img) with ViewCard
case class RoamFlipAction(self : Faction, then : ForcedAction) extends ForcedAction
case class RoamMoveAction(self : Faction, o : Clearing, d : Clearing, then : ForcedAction) extends BaseAction(self, "moves the", "Monster".styled(LegendBeast), "from", o, "to")(d)
case class RoamMovedAction(self : Faction, then : ForcedAction) extends ForcedAction
case class MonsterAttackAction(self : Faction, e : Faction, c : Clearing, then : ForcedAction) extends ForcedAction
case class RiderDoneAction(then : ForcedAction) extends ForcedAction

case class AppeaseMainAction(self : Faction, c : Clearing) extends BaseAction("Monster".styled(LegendBeast))("Pay to appease in", c) with Soft
case class AppeaseAction(self : Faction, c : Clearing, d : DeckCard) extends BaseAction(self, "pays to appease the", "Monster".styled(LegendBeast), "in", c)(d.img) with ViewCard

case class SlayAction(self : Faction, m : LegendMonster) extends BaseAction("Monster".styled(LegendBeast))("Slay", m, "with", m.slay./(_.img))

case class RitualMainAction(self : Faction) extends BaseAction("Dark Rituals".hh)("Sacrifice to control the", "Monster".styled(LegendBeast)) with Soft
case class RitualMoveAction(self : Faction, o : Clearing, d : Clearing) extends BaseAction("Dark Rituals".hh, "moves the", "Monster".styled(LegendBeast), "to")(d)
case class RitualAttackAction(self : Faction, e : Faction, c : Clearing) extends BaseAction("Dark Rituals".hh, "attacks with the", "Monster".styled(LegendBeast), "in", c)(e)

case class InvestigateMainAction(self : Faction) extends BaseAction("Hunt the Legend".hh)("Investigate the monster") with Soft
case class InvestigateAction(self : Faction, c : Clearing, d : DeckCard) extends BaseAction("Hunt the Legend".hh, "investigates in", c)(d.img) with ViewCard
case class InvestigateTopAction(self : Faction) extends BaseAction("Hunt the Legend".hh)("Put it back on top")
case class InvestigateBottomAction(self : Faction) extends BaseAction("Hunt the Legend".hh)("Put it at the bottom")
case class InvestigatePlaceAction(self : Faction, c : Clearing) extends BaseAction("Hunt the Legend".hh)("Place it in", c, "and score", 1.vp)

case class MaulerHitAction(self : Faction, then : BattleBonusAction) extends BaseAction(Mauler.elem, "in battle")("Deal an extra", 1.hit)
case class MaulerDiscardAction(self : Faction, e : Faction, then : BattleBonusAction) extends BaseAction(Mauler.elem, "in battle")(e, "discards a random card")
case class MaulerDiscardedAction(self : Faction, e : Faction, d : DeckCard, then : BattleBonusAction) extends RandomAction[DeckCard] { def random = d }

case class BushblazerAction(self : Faction, c : Clearing) extends BaseAction(Bushblazer.elem)("Burn the defenseless in", c)
case class BushblazerRemoveAction(self : Faction, c : Clearing, l : $[(Faction, Piece)]) extends ForcedAction


case object DarkRituals extends LegendsScenario {
    def title = "Dark Rituals"
    def suited = false
    override val explain = $(
        "Dark Rituals".hh ~ SpacedDash ~ "a " ~ "Legends Scenario".hh ~ " from " ~ "The Doomed Expansion".hl ~ " by " ~ "slugfacekillah".hl ~ ":",
        "a " ~ "Legendary Monster".hh ~ " starts in the " ~ "scenario space".hh ~ &(", which only warriors and pawns can enter") ~ &(" and nothing leaves."),
        "Alone there, a player sacrifices a warrior to move the monster" ~ &(" or have it attack;") ~ &(" each Evening it roams;") ~ &(" items can be captured."),
        FanInfo.doomedLink,
    )
}

case object HuntTheLegend extends LegendsScenario {
    def title = "Hunt the Legend"
    def suited = true
    override val explain = $(
        "Hunt the Legend".hh ~ SpacedDash ~ "a " ~ "Legends Scenario".hh ~ " from " ~ "The Doomed Expansion".hl ~ " by " ~ "slugfacekillah".hl ~ ":",
        "three monster markers are stacked and the " ~ "Legends die".hh ~ " roams the map" ~ &(", counting down;") ~ &(" each ghost leaves a marker,") ~ &(" and the last one is the monster."),
        "Players investigate the markers in Daylight;" ~ &(" items can be captured."),
        FanInfo.doomedLink
    )
}

// the monster Dark Rituals starts with
trait MonsterChoice extends GameOption with ToggleOption with OneOfGroup {
    val group = "Legendary Monster".hh
    override def folder = Meta.scenarios
    def monster : LegendMonster
    val valueOn = monster.title.hh
    override def required(all : $[BaseOption]) = $($(DarkRituals))
    override val explain = $(monster.title.hh ~ SpacedDash ~ "health " ~ monster.health.hl ~ ", " ~ monster.ability ~ "; slain with " ~ monster.slay./(_.title.hh).join(" and ") ~ ".", "None picked: one is drawn at random.")
}

case object PickShadowmaw extends MonsterChoice { def monster = Shadowmaw }
case object PickWhiteWolf extends MonsterChoice { def monster = WhiteWolf }
case object PickBeaverbot extends MonsterChoice { def monster = Beaverbot }
case object PickRedKnight extends MonsterChoice { def monster = RedKnight }
case object PickDemonBear extends MonsterChoice { def monster = DemonBear }


object MonstersExpansion extends Expansion {
    def active(setup : $[Faction], options : $[Meta.O]) = on(options)

    def on(options : $[Meta.O]) = options.has(DarkRituals) || options.has(HuntTheLegend)

    def options(implicit game : Game) = game.options

    def monsterIn(implicit game : Game) : |[Clearing] = game.monster.any.??(game.states.contains(LegendBeast).??(game.clearings.%(c => LegendBeast.at(c).has(MonsterPawn)).starting))

    def roamer(implicit game : Game) : |[Clearing] = monsterIn || game.legendDie

    def what(implicit game : Game) : Elem = game.monster./(_.elem).|("Legends die".hh)

    def markers(c : Clearing)(implicit game : Game) : $[Figure] =
        game.legendDie.has(c).$(Figure(Neutral, LegendsDie(game.legendDieValue), 0)) ++
        monsterIn.has(c).$(Figure(Neutral, LegendsDie(game.monsterHealth), 1)) ++
        game.huntMarkers.%(_._1 == c).indexed./{ case ((_, m, up), n) => Figure(Neutral, MonsterMarker(up.?(m)), 2 + n) }

    // pieces can't move out of the monster's clearing until appeased, nor out of the space in Dark Rituals
    def blocked(f : Faction, o : Clearing)(implicit game : Game) : Boolean = f != LegendBeast && (
        (options.has(DarkRituals) && o == LegendSpace && game.legendForest.any) ||
        (monsterIn.has(o) && game.appeased.has((f, o)).not)
    )

    // the items a monster's slayer must hold
    def deckItems(implicit game : Game) = game.playingDeck.of[CraftItemCard]./(_.item)

    def available(m : LegendMonster)(implicit game : Game) = m.slay.forall {
        case Torch => game.factions.exists(_.is[Hero]) || deckItems.has(Torch)
        case RabbitsPaw => deckItems.has(RabbitsPaw)
        case _ => true
    }

    def slayReady(f : Faction, m : LegendMonster)(implicit game : Game) = f @@ {
        case f : Hero => m.slay.forall(i => f.inv.ready.exists(_.item == i))
        case f => m.slay.forall(i => LegendsExpansion.items(f).has(i))
    }

    def summon(m : LegendMonster, c : Clearing)(implicit game : Game) {
        game.monster = |(m)
        game.monsterHealth = m.health

        DarkDeckExpansion.ensure(LegendBeast)

        LegendBeast.reserve --> MonsterPawn --> c

        log("The", m, "appeared in", c, "with", m.health.hl, "health:", m.ability)
    }

    def holds(f : Faction, i : Item)(implicit game : Game) = game.states.contains(f) && LegendsExpansion.items(f).has(i)

    def controls(f : Faction)(implicit game : Game) = options.has(DarkRituals) && game.legendForest.any && LegendsExpansion.holders == $(f) && sacrifice(f)

    def sacrifice(f : Faction)(implicit game : Game) = f @@ {
        case f : Hero => f.region == LegendSpace && f.inv.notDamaged.%(r => Item.legends.has(r.item).not && r.item != CursedIdol).any
        case f => f.at(LegendSpace).of[Warrior].any
    }

    def defenseless(f : Faction, c : Clearing)(implicit game : Game) : $[(Faction, Piece)] =
        f.enemies.%(game.states.contains).%(e => e != LegendBeast).%(f.canRemove(c)).%(e => e.at(c).of[Warrior].none)./~(e => e.at(c).%(p => p.is[Pawn].not && p.is[Tenacious].not && p.is[Ruins].not)./(e -> _))

    def bushblaze(f : Faction)(implicit game : Game) : $[Clearing] = holds(f, Bushblazer).??(game.clearings.%(f.present).%(c => defenseless(f, c).any))

    def weaponReady(f : Faction, i : Item)(implicit game : Game) = holds(f, i) && game.weaponUsed.has((f, i, game.turn)).not

    // the extra hits still to add in a battle: (side, tag, hits); the Mauler's is a choice
    def bonuses(b : Battle, side : Faction)(implicit game : Game) : $[(Faction, String, Int)] = {
        val k = side.short + "-"

        $[(Faction, String, Int)]() ++
        (side == LegendBeast).??(game.monster.$./~{
            case Beaverbot => $((side, k + "ability", 2))
            case DemonBear => $((side, k + "ability", 1))
            case _ => $
        }) ++
        holds(side, SpiritSword).$((side, k + "spirit", 1)) ++
        holds(side, Bushblazer).$((side, k + "blaze", 1)) ++
        weaponReady(side, Chainsaw).$((side, k + "chainsaw", 2)) ++
        (weaponReady(side, Slaymore) && game.current == side).$((side, k + "slaymore", 3)) ++
        holds(side, Mauler).$((side, k + "mauler", 0))
    }.%(x => game.legendBonusDone.has(x._2).not)

    override def birdsong(f : Faction)(implicit game : Game, ask : ActionCollector) {
        anytime(f)
    }

    override def daylight(f : Faction)(implicit game : Game, ask : ActionCollector) {
        if (options.has(HuntTheLegend) && game.huntStack.any && game.monster.none && f.hand.any)
            + InvestigateMainAction(f).!(game.clearings.%(f.present).%(c => f.hand.exists(_.matches(c.cost))).none, "no matching card")

        anytime(f)
    }

    override def evening(f : Faction)(implicit game : Game, ask : ActionCollector) {
        if (holds(f, Bushblazer) && game.weaponUsed.has((f, Bushblazer, game.turn)).not)
            bushblaze(f).foreach(c => + BushblazerAction(f, c))

        anytime(f)
    }

    def anytime(f : Faction)(implicit game : Game, ask : ActionCollector) {
        if (game.current != f)
            return

        monsterIn.foreach { c =>
            if (f.present(c) && game.appeased.has((f, c)).not && (options.has(DarkRituals) && c == LegendSpace).not)
                + AppeaseMainAction(f, c).!(f.hand.%(_.matches(c.cost)).none, "no matching card")

            game.monster.foreach { m =>
                if (game.monsterHealth <= 1 && slayReady(f, m))
                    + SlayAction(f, m)
            }
        }

        if (controls(f) && monsterIn.any)
            + RitualMainAction(f)
    }

    def perform(action : Action, soft : Void)(implicit game : Game) = action @@ {
        // SETUP: after the capture rule is drawn
        case MonsterSetupAction(then) if options.has(DarkRituals) =>
            val l = options.of[MonsterChoice]./(_.monster).some.|(LegendMonster.all.%(available).some.|(LegendMonster.all))

            Random[LegendMonster](l, MonsterChosenAction(_, then))

        case MonsterChosenAction(m, then) =>
            summon(m, LegendSpace)

            then

        case MonsterSetupAction(then) =>
            Shuffle[LegendMonster](LegendMonster.all.%(available).some.|(LegendMonster.all), HuntStackAction(_, then))

        case HuntStackAction(l, then) =>
            game.huntStack = l.take(3)
            game.legendDie = |(LegendSpace)
            game.legendDieValue = 6

            log(game.huntStack.num.hl, "monster markers were stacked face down, and the", "Legends die".hh, "set to", 6.hl, "in the", LegendSpace)

            then

        // ROAMING: at the start of each Evening from the second round
        case EveningStartAction(f) if on(options) && game.factions.has(f) && game.turn > game.factions.num && game.roamedTurn != game.turn && roamer.any =>
            game.roamedTurn = game.turn

            RoamAction(f, EveningStartAction(f))

        case RoamAction(f, then) =>
            val c = roamer.get
            val l = f.hand.%(_.matches(c.cost))

            if (l.any)
                Ask(f).group(f, "can pay to appease the", what, "in", c).each(l)(d => RoamAppeaseAction(f, c, d, then)).add(RoamFlipAction(f, then).as("Let it roam"))
            else
                RoamFlipAction(f, then)

        case RoamAppeaseAction(f, c, d, then) =>
            f.hand --> d --> discard.quiet

            f.log("discarded", d, "to keep the", what, "from roaming")

            then

        case RoamFlipAction(f, then) =>
            if (deck.none && pile.any)
                Shuffle[DeckCard](pile, ShufflePileAction(_, RoamFlipAction(f, then)))
            else
            deck.$.starting match {
                case None =>
                    log("No card was left to roam the", what)

                    RoamMovedAction(f, then)

                case Some(d) =>
                    deck --> d --> discard.quiet

                    val c = roamer.get

                    log("The", what, "roamed with", d)

                    if (game.monster.any && monsterIn.has(c) && d.matches(c.cost) && f.present(c))
                        MonsterAttackAction(f, f, c, RoamMovedAction(f, then))
                    else {
                        val l = game.connected(c).but(c).%(o => d.suit == Bird || d.matches(o.cost))

                        if (l.none) {
                            log("The", what, "stayed in", c)

                            RoamMovedAction(f, then)
                        }
                        else
                            Ask(f).group(f, "moves the", what, "from", c).each(l)(o => RoamMoveAction(f, c, o, RoamMovedAction(f, then)))
                    }
            }

        case RoamMoveAction(f, o, d, then) =>
            if (game.legendDie.has(o))
                game.legendDie = |(d)
            else
                LegendBeast.from(o) --> MonsterPawn --> d

            f.log("moved the", what, "from", o, "to", d)

            then

        case RoamMovedAction(f, then) =>
            if (game.monster.none && game.legendDie.any) {
                val c = game.legendDie.get

                game.legendDieValue -= 1

                if (game.legendDieValue > 1)
                    log("The", "Legends die".hh, "went down to", game.legendDieValue.hl)
                else
                if (game.huntStack.num > 1) {
                    game.huntMarkers :+= ((c, game.huntStack.first, false))
                    game.huntStack = game.huntStack.drop(1)
                    game.legendDieValue = 6

                    log("The", "Legends die".hh, "showed the ghost: a monster marker was placed face down in", c, "and the die set to", 6.hl)
                }
                else {
                    val m = game.huntStack.first

                    game.huntMarkers :+= ((c, m, true))
                    game.huntStack = $
                    game.legendDie = None

                    log("The", "Legends die".hh, "showed the ghost for the last marker")

                    summon(m, c)
                }
            }

            then

        case MonsterAttackAction(f, e, c, then) =>
            log("The", what, "attacked", e, "in", c)

            BattleStartAction(f, LegendBeast, LegendBeast, NoMessage, c, e, None, then)

        case RiderDoneAction(then) =>
            game.monsterRider = None

            then

        // BLOCKING: paying to appease
        case AppeaseMainAction(f, c) =>
            Ask(f).group(f, "pays to appease the", what, "in", c).each(f.hand.%(_.matches(c.cost)))(d => AppeaseAction(f, c, d)).cancel

        case AppeaseAction(f, c, d) =>
            f.hand --> d --> discard.quiet

            game.appeased :+= (f, c)

            f.log("discarded", d, "to appease the", what, "in", c)

            Repeat

        // SLAYING
        case SlayAction(f, m) =>
            f @@ {
                case f : Hero =>
                    m.slay.foreach { i =>
                        val r = f.inv.ready.%(_.item == i).first
                        f.inv = f.inv.diff($(r)) :+ r.exhaust
                    }
                case _ =>
            }

            val c = monsterIn.get

            LegendBeast.from(c) --> MonsterPawn --> LegendBeast.reserve

            game.monster = None
            game.monsterSlain = true

            f.log("slew the", m, "in", c, "with", m.slay./(_.elem).commaAnd, "and took", m.weapon)

            f.forTrade :+= m.weapon.pristine

            AcquireItemsAction(f, Repeat)

        // DARK RITUALS: a sacrifice moves the monster or has it attack
        case RitualMainAction(f) =>
            val c = monsterIn.get

            Ask(f).group("Dark Rituals".hh, "sacrifices to")
                .each(game.connected(c).but(c))(d => RitualMoveAction(f, c, d))
                .each((game.factions ++ game.hirelings).but(LegendBeast).%(game.states.contains).%(_.present(c)))(e => RitualAttackAction(f, e, c))
                .cancel

        case RitualMoveAction(f, o, d) =>
            sacrificed(f)

            LegendBeast.from(o) --> MonsterPawn --> d

            f.log("moved the", what, "from", o, "to", d)

            Repeat

        case RitualAttackAction(f, e, c) =>
            sacrificed(f)

            game.monsterRider = |(f)

            MonsterAttackAction(f, e, c, RiderDoneAction(Repeat))

        // HUNT THE LEGEND: investigating
        case InvestigateMainAction(f) =>
            Ask(f).group("Hunt the Legend".hh, "investigates")
                .some(game.clearings.%(f.present))(c => f.hand.%(_.matches(c.cost))./(d => InvestigateAction(f, c, d)))
                .cancel

        case InvestigateAction(f, c, d) =>
            f.hand --> d --> discard.quiet

            f.log("discarded", d, "to peek at the top monster marker")

            val m = game.huntStack.first

            Ask(f).group("The top marker is", m, "(" ~ m.ability ~ ")")
                .add(InvestigateTopAction(f))
                .when(game.huntStack.num > 1)(InvestigateBottomAction(f))
                .when(game.huntStack.num > 1)(InvestigatePlaceAction(f, c))

        case InvestigateTopAction(f) =>
            f.log("put the marker back on top")

            Repeat

        case InvestigateBottomAction(f) =>
            game.huntStack = game.huntStack.drop(1) :+ game.huntStack.first

            f.log("put the marker at the bottom")

            Repeat

        case InvestigatePlaceAction(f, c) =>
            game.huntMarkers :+= ((c, game.huntStack.first, false))
            game.huntStack = game.huntStack.drop(1)

            f.oscore(1)("placing a monster marker in", c)

            Repeat

        // BUSHBLAZER
        case BushblazerAction(f, c) =>
            game.weaponUsed :+= ((f, Bushblazer, game.turn))

            f.log("burned the defenseless in", c, "with", Bushblazer)

            BushblazerRemoveAction(f, c, defenseless(f, c))

        case BushblazerRemoveAction(f, c, Nil) =>
            ForcedRemoveFinishedAction(f, Repeat)

        case BushblazerRemoveAction(f, c, (e, p) :: rest) =>
            if (e.at(c).has(p))
                ForcedRemoveAction(f, c, e, p, p.is[Scoring].??(1), Removing, BushblazerRemoveAction(f, c, rest))
            else
                BushblazerRemoveAction(f, c, rest)

        // BATTLES
        case BattleStartedAction(b) if game.legendBonusDone.any =>
            game.legendBonusDone = $

            BattleStartedAction(b)

        case BattleAdjustRollAction(b, f, o, fs, os, fr, or) if f == LegendBeast && fs < 3 =>
            BattleAdjustRollAction(b, f, o, 3, os, fr, or)

        case BattleAdjustRollAction(b, f, o, fs, os, fr, or) if o == LegendBeast && os < 3 =>
            BattleAdjustRollAction(b, f, o, fs, 3, fr, or)

        case BattleAdjustRollAction(b, f, o, fs, os, fr, or) if fr > or && ((o == LegendBeast && game.monster.has(WhiteWolf)) || holds(o, SpiritSword)) =>
            o.log("took the higher roll with", (o == LegendBeast).?(WhiteWolf.elem).|(SpiritSword.elem))

            BattleAdjustRollAction(b, f, o, fs, os, or, fr)

        case BattleFinishRollAction(b, f, o, fs, os, fr, or, fh, oh) if game.monster.has(Shadowmaw) && ((o == LegendBeast && fh > 0) || (f == LegendBeast && oh > 0)) =>
            log("The", Shadowmaw, "can only be dealt extra hits")

            BattleFinishRollAction(b, f, o, fs, os, fr, or, (o == LegendBeast).?(0).|(fh), (f == LegendBeast).?(0).|(oh))

        case BattleBonusAction(b, f, o, fs, os, fr, or, fh, oh, fe, oe) if (bonuses(b, f) ++ bonuses(b, o)).any =>
            val (x, tag, n) = (bonuses(b, f) ++ bonuses(b, o)).first
            val e = (x == f).?(o).|(f)

            game.legendBonusDone :+= tag

            if (tag.endsWith("chainsaw"))
                game.weaponUsed :+= ((x, Chainsaw, game.turn))

            if (tag.endsWith("slaymore"))
                game.weaponUsed :+= ((x, Slaymore, game.turn))

            val bb = BattleBonusAction(b, f, o, fs, os, fr, or, fh, oh, fe, oe)

            if (tag.endsWith("mauler"))
                Ask(x).group(Mauler.elem)
                    .add(MaulerHitAction(x, bb))
                    .when(e.hand.any)(MaulerDiscardAction(x, e, bb))
            else {
                val source = tag.endsWith("ability").?(game.monster.get.elem).||(tag.endsWith("spirit").?(SpiritSword.elem)).||(tag.endsWith("blaze").?(Bushblazer.elem)).||(tag.endsWith("chainsaw").?(Chainsaw.elem)).|(Slaymore.elem)

                x.log("dealt", n.hits, "more with", source)

                BattleBonusAction(b, f, o, fs, os, fr, or, fh, oh, fe + (x == f).??(n), oe + (x == o).??(n))
            }

        case MaulerHitAction(x, bb) =>
            x.log("dealt an extra hit with", Mauler)

            bb.copy(fe = bb.fe + (x == bb.f).??(1), oe = bb.oe + (x == bb.o).??(1))

        case MaulerDiscardAction(x, e, bb) =>
            Random[DeckCard](e.hand.$, MaulerDiscardedAction(x, e, _, bb))

        case MaulerDiscardedAction(x, e, d, bb) =>
            e.hand --> d --> discard.quiet

            x.log("made", e, "discard", d, "with", Mauler)

            bb

        // the monster's hits lower its health
        case BattleAssignHitsAction(LegendBeast, b, n, s, then) if n + s > 0 && game.monster.has(RedKnight) && game.legendBonusDone.has("ignore").not =>
            game.legendBonusDone :+= "ignore"

            log("The", RedKnight, "ignored a hit")

            BattleAssignHitsAction(LegendBeast, b, max(0, n - 1), (n > 0).?(s).|(max(0, s - 1)), then)

        case BattleAssignHitsAction(LegendBeast, b, n, s, then) =>
            if (n + s > 0 && game.monster.any) {
                game.monsterHealth = max(1, game.monsterHealth - n - s)

                log("The", game.monster.get, "took", (n + s).hits, "and is at", (game.monsterHealth > 1).?(game.monsterHealth.hl).|("the ghost".hh))
            }

            then

        // what the monster removes scores only for the player who had it attack
        case ForcedRemoveScoreAction(LegendBeast, c, f, p, n, m, then) =>
            game.monsterRider.%(_ != f) match {
                case Some(r) => ForcedRemoveScoreAction(r, c, f, p, n, m, then)
                case None => then
            }

        // the end of a turn: appeasements end
        case CleanUpAction(f) if game.appeased.any =>
            game.appeased = $

            CleanUpAction(f)

        case _ => UnknownContinue
    }

    def sacrificed(f : Faction)(implicit game : Game) {
        f @@ {
            case f : Hero =>
                val r = f.inv.notDamaged.%(r => Item.legends.has(r.item).not && r.item != CursedIdol).first
                f.inv = f.inv.diff($(r)) :+ r.damage

                f.log("damaged", r.item, "in the", LegendSpace)

            case f =>
                val w = f.at(LegendSpace).of[Warrior].first

                f.from(LegendSpace) --> w --> f.reserve

                f.log("sacrificed", w.of(f), "in the", LegendSpace)
        }
    }
}
