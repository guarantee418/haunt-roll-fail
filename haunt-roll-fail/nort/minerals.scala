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


// The Minerals module (option ModuleOption(MineralsModule), under Fan content), fan-made by Maciej "Niedźwiedź" Drewing:
// BoardGameGeek file 255809 "[Fan-made] Module: Minerals" (2023), here from its Polish print-and-play version (Czacha Games).
// RULES.md (Minerals) lists the choices made where the module is short

// Stone, Iron and Gem, shown as their icons (ui-stone, ui-iron, ui-gem, cut from the module's rules)
trait Mineral extends NamedToString with Elementary with Record {
    override def elem : Elem = Image("ui-" + name.toLowerCase, styles.inlineIcon).alt(name)
}

case object Stone extends Mineral
case object Iron extends Mineral
case object Gem extends Mineral

object Mineral {
    val all : $[Mineral] = $(Stone, Iron, Gem)
}

// The Mine, a small building (the module's 7 tokens), built only in a territory with a creature lair; not in Building.all,
// so games without the module never offer it (MineralsExpansion.buildings)
case object Mine extends Building { val large = false }

// Minerals as "1 [stone] 2 [gem]"
case class MineralsLabel(l : $[Mineral]) extends Elementary {
    def elem : Elem = Mineral.all.%(l.has)./(m => Amount(l.count(m).hl, m.elem)).join(" ")
}

// HARVEST: each player with Mines in turn rolls the combat die, then picks the minerals the symbols pay for
case class MineHarvestAction(l : $[Faction], then : ForcedAction) extends ForcedAction
case class MineRolledAction(f : Faction, mines : Int, random : DieFace, rest : $[Faction], then : ForcedAction) extends RandomAction[DieFace]
case class MineTakeAction(self : Faction, mines : Int, face : DieFace, take : $[Mineral], rest : $[Faction], then : ForcedAction) extends BaseAction("Mines".hl, "rolled", face, "for", mines.hl, (mines == 1).?("Mine").|("Mines"), Comma, "extract")(MineralsLabel(take))

// STONE: one of f's buildings upgraded during a Build (then goes back to the Build)
case class StoneUpgradeAction(self : Faction, space : SpaceRef, building : Building, cost : Int, then : ForcedAction) extends BaseAction("Upgrade a building for", cost.hl, Stone)(building, "in", space.area) with MapTarget { def target = space }

// IRON: after a side's food in a fight, whether it spends an Iron to roll two dice
case class IronAction(self : Faction, use : Boolean, then : ForcedAction) extends BaseAction(self, "may spend an", Iron, "to roll two combat dice")(use.?("Spend".txt ~ " " ~ Iron.elem ~ ", roll two dice").|("Roll one die".txt))
case class DicePair(a : DieFace, b : DieFace) extends Record
case class IronRolledAction(attacker : Faction, defender : Faction, area : AreaRef, e : MoveEffect, food : $[Int], faces : $[DieFace], random : DicePair, then : ForcedAction) extends RandomAction[DicePair]
// Two dice with "1 point or 1 casualty" faces: how many of them are points
case class CreatureIronRolledAction(f : Faction, area : AreaRef, c : Creature, e : MoveEffect, attacking : Boolean, food : Int, random : DicePair, then : ForcedAction) extends RandomAction[DicePair]
case class IronChooseAction(self : Faction, attacker : Faction, defender : Faction, area : AreaRef, e : MoveEffect, food : $[Int], faces : $[DieFace], choice : DieFace, then : ForcedAction) extends BaseAction(self, "rolled two dice with", Iron, Break, FightInfo(attacker, defender, area, e, food, faces, true))(choice)

// RUBY: the fame of a card in the discard pile, which goes on top of the draw pile; stage as in TurnAction
case class GemAction(self : Faction, card : Card, stage : Int) extends BaseAction("Spend a", Gem, "for the fame of a discarded card", "(it goes on top of your draw pile)")(card.handImg, Amount(card.fame.hl, FameIcon())) with ViewObject[Card] { def obj = card }
// Back to the cards in hand after a Gem spent before playing one
case class GemPlayAction(f : Faction) extends ForcedAction

// TRADES: a mineral counts as two of the three resources paid for one
case class MineralTradeAction(self : Faction, pay : $[Mineral], also : |[Resource], gain : Resource, then : ForcedAction) extends BaseAction("Trade", "three resources for one", "(a mineral counts as two)")("Pay", MineralsLabel(pay), also./(_.elem).|(Empty), "for", gain)

// WINTER: minerals paying what food and wood can't
case class MineralWinterAction(self : Faction, pay : $[Mineral], rest : $[Faction]) extends BaseAction("Winter: pay with minerals", "(each counts as two food or wood)")("Pay", MineralsLabel(pay))
case class MineralWinterSkipAction(self : Faction, rest : $[Faction]) extends BaseAction("Winter: pay with minerals", "(each counts as two food or wood)")("Keep them and take an", UnrestCard)

// The panel's minerals (tapped: MineralsView)
case object MineralsView


object MineralsExpansion extends Expansion {
    // The minerals in the supply: 3, 2 and 1.5 per player (rounded up) of Stone, Iron and Gem, as in the module's table for 2 to 5 players
    def supply(players : Int) : Map[Mineral, Int] = Map(Stone -> 3 * players, Iron -> 2 * players, Gem -> (3 * players + 1) / 2)

    // The buildings in this game: the Mine only with the module
    def buildings(implicit game : Game) : $[Building] = Building.all ++ game.has(MineralsModule).$(Mine) ++ game.has(LionKingdom).$(Chapel)

    def lair(t : Territory)(implicit game : Game) : Boolean = t.areas.exists(a => game.board.spec(a).lair)

    // f's Mines; one upgraded with Stone counts as two
    def mines(f : Faction)(implicit game : Game) : Int = game.controlled(f)./~(game.working).count(_ == Mine)

    def left(m : Mineral)(implicit game : Game) = game.mineralBank.getOrElse(m, 0)

    // Every haul the rolled symbols can pay for, and none paying for more: 2 axes a Stone, an axe and a skull an Iron, 2 skulls a Gem,
    // as far as the supply goes. "1 point or 1 casualty" gives one symbol per Mine, each picked apart
    def hauls(face : DieFace, mines : Int)(implicit game : Game) : $[$[Mineral]] = {
        val axes = face.points * mines
        val skulls = face.casualties * mines
        val free = face.choice.??(mines)

        def fits(s : Int, i : Int, r : Int) =
            if (face.choice) 2 * (s + i + r) <= free
            else 2 * s + i <= axes && i + 2 * r <= skulls

        val max = (axes + skulls + free) / 2
        val all = for (s <- 0.to(math.min(max, left(Stone))).$; i <- 0.to(math.min(max, left(Iron))).$; r <- 0.to(math.min(max, left(Gem))).$; if fits(s, i, r)) yield (s, i, r)
        val best = all.%{ case (s, i, r) => all.exists { case (s2, i2, r2) => s2 >= s && i2 >= i && r2 >= r && (s2, i2, r2) != (s, i, r) }.not }

        best.%(_ != ((0, 0, 0))).sortBy { case (s, i, r) => (-r, -i, -s) }./{ case (s, i, r) => s.times(Stone : Mineral) ++ i.times(Iron : Mineral) ++ r.times(Gem : Mineral) }
    }

    // Stone: f's buildings it can upgrade now (once per Build), 1 Stone for a small one, 2 for a large one
    def upgrades(f : Faction, then : ForcedAction)(implicit game : Game) : $[UserAction] =
        (game.has(MineralsModule) && game.stoneUsed.not && f.mineral(Stone) > 0).??(
            game.controlled(f)./~(game.buildingsIn).%((s, _) => game.upgraded.has(s).not).%((_, b) => f.mineral(Stone) >= cost(b))./((s, b) => StoneUpgradeAction(f, s, b, cost(b), then))
        )

    def cost(b : Building) = b.large.?(2).|(1)

    // Gem: the cards in f's discard pile that give fame, while playing cards (stage as in TurnAction)
    def gems(f : Faction, stage : Int)(implicit game : Game) : $[UserAction] =
        (game.has(MineralsModule) && f.mineral(Gem) > 0).??(f.discard.distinct.%(_.fame > 0)./(c => GemAction(f, c, stage)))

    // The harvest trades with minerals: a mineral and a resource, or two minerals, for a resource not paid in
    def trades(f : Faction, then : ForcedAction)(implicit game : Game) : $[UserAction] = {
        if (game.has(MineralsModule).not || f.mineralCount == 0)
            return $

        val held = Mineral.all.%(f.mineral(_) > 0)
        val withResource = held./~(m => Resource.all.%(f.has(_) > 0)./~(r => Resource.all.%(_ != r)./(x => MineralTradeAction(f, $(m), |(r), x, then))))
        val pairs = held./~(m => held.%(n => Mineral.all.indexOf(n) >= Mineral.all.indexOf(m)).%(n => n != m || f.mineral(m) >= 2)./(n => $(m, n)))
        val twoMinerals = pairs./~(p => Resource.all./(x => MineralTradeAction(f, p, None, x, then)))

        withResource ++ twoMinerals
    }

    // Winter: what food and wood f is short of
    def short(f : Faction)(implicit game : Game) : (Int, Int) = {
        val (food, wood) = EventsExpansion.winterCost(f)
        (math.max(0, food - f.food), math.max(0, wood - f.wood))
    }

    // The fewest minerals that pay the shortfall, every mix of them f can give
    def winterPayments(f : Faction)(implicit game : Game) : $[$[Mineral]] = {
        val (food, wood) = short(f)
        val k = (food + wood + 1) / 2

        if (k == 0 || game.robotos(f) || f.mineralCount < k)
            $
        else
            Mineral.all.foldLeft($($[Mineral]()))((l, m) => l./~(p => 0.to(math.min(f.mineral(m), k - p.num)).$./(n => p ++ n.times(m)))).%(_.num == k)
    }

    // Spent minerals leave the game
    def spend(f : Faction, l : $[Mineral])(implicit game : Game) {
        l.foreach(m => f.gainMineral(m, -1))
    }

    def perform(action : Action, soft : Void)(implicit game : Game) : Continue = action @@ {
        // HARVEST: the Mines, after the harvest and its extras (Dragon, Squirrel, the Kobolds), before the trades
        case TradePhaseAction(Nil, then) if game.minedYear != game.year =>
            game.minedYear = game.year

            Then(MineHarvestAction(game.from(game.first).%(f => mines(f) > 0), TradePhaseAction($, then)))

        case MineHarvestAction(Nil, then) =>
            Then(then)

        case MineHarvestAction(f :: rest, then) =>
            if (Mineral.all.forall(left(_) == 0)) {
                log("No minerals are left to extract")

                Then(then)
            }
            else
                Random[DieFace](NorthgardDie.faces, MineRolledAction(f, mines(f), _, rest, then))

        case MineRolledAction(f, n, face, rest, then) =>
            f.log("rolled", face, "for", n.hl, (n == 1).?("Mine").|("Mines"))

            game.note("mine-roll")

            val l = hauls(face, n)

            if (l.none) {
                f.log("extracted nothing")

                Then(MineHarvestAction(rest, then))
            }
            else
                Ask(f).each(l)(take => MineTakeAction(f, n, face, take, rest, then))

        case MineTakeAction(f, _, _, take, rest, then) =>
            take.foreach { m =>
                f.gainMineral(m, 1)
                game.mineralBank += m -> (left(m) - 1)
            }

            f.log("extracted", MineralsLabel(take))

            take.distinct.foreach(m => game.note("mined-" + m.name.toLowerCase))

            Then(MineHarvestAction(rest, then))

        // STONE
        case StoneUpgradeAction(f, s, b, cost, then) =>
            spend(f, cost.times(Stone))
            game.upgraded :+= s
            game.stoneUsed = true

            f.log("upgraded", b, "in", s.area, "with", MineralsLabel(cost.times(Stone)))

            game.note("stone-upgrade")

            Then(then)

        // IRON: once a side has spent its food (the attacker's, then the defender's)
        case CombatFoodPaidAction(attacker, defender, a, e, food, then) if game.has(MineralsModule) && {
            val side = (food.num == 1).?(attacker).|(defender)
            side != Automa && side.mineral(Iron) > 0 && game.ironAsked.has(side).not
        } =>
            val side = (food.num == 1).?(attacker).|(defender)
            val next = CombatFoodPaidAction(attacker, defender, a, e, food, then)

            Ask(side).add(IronAction(side, true, next)).add(IronAction(side, false, next))

        case IronAction(f, use, then) =>
            game.ironAsked :+= f

            if (use) {
                spend(f, $(Iron))
                game.ironFight :+= f

                f.log("spent", Iron, "to roll two combat dice")

                game.note("iron")
            }

            Then(then)

        // In a fight with a creature, the player's die (casualties don't hurt creatures, so a choice face is a point)
        case CreatureFoodPaidAction(f, a, c, e, attacking, food, then) if game.has(MineralsModule) && f != Automa && f.mineral(Iron) > 0 && game.ironAsked.has(f).not =>
            val next = CreatureFoodPaidAction(f, a, c, e, attacking, food, then)

            Ask(f).add(IronAction(f, true, next)).add(IronAction(f, false, next))

        case CreatureFoodPaidAction(f, a, c, e, attacking, food, then) if game.ironFight.has(f) =>
            Random[DicePair](NorthgardDie.faces./~(x => NorthgardDie.faces./(y => DicePair(x, y))), CreatureIronRolledAction(f, a, c, e, attacking, food, _, then))

        case CreatureIronRolledAction(f, a, c, e, attacking, food, DicePair(x, y), then) =>
            game.ironFight = game.ironFight.but(f)

            f.log("rolled", x, "and", y, "with", Iron)

            val l = $(x, y)

            Then(CreatureFaceAction(f, a, c, e, attacking, food, DieFace(l./(d => d.choice.?(1).|(d.points)).sum, l./(_.casualties).sum, false), then))

        case CombatRollAction(attacker, defender, a, e, food, faces, then) if faces.num < 2 && game.ironFight.has((faces.num == 0).?(attacker).|(defender)) =>
            Random[DicePair](NorthgardDie.faces./~(x => NorthgardDie.faces./(y => DicePair(x, y))), IronRolledAction(attacker, defender, a, e, food, faces, _, then))

        case IronRolledAction(attacker, defender, a, e, food, faces, DicePair(x, y), then) =>
            val self = (faces.num == 0).?(attacker).|(defender)

            self.log("rolled", x, "and", y, "with", Iron)

            val l = $(x, y)
            val base = DieFace(l.%(_.choice.not)./(_.points).sum, l.%(_.choice.not)./(_.casualties).sum, false)
            val k = l.count(_.choice)

            if (k == 0)
                MapExpansion.rolled(attacker, defender, a, e, food, faces, base, then)
            else
                Ask(self).each(0.to(k).$.reverse)(n => IronChooseAction(self, attacker, defender, a, e, food, faces, base.copy(points = base.points + n, casualties = base.casualties + k - n), then))

        case IronChooseAction(self, attacker, defender, a, e, food, faces, choice, then) =>
            self.log("took", choice)

            MapExpansion.rolled(attacker, defender, a, e, food, faces, choice, then)

        // RUBY
        case GemAction(f, c, stage) =>
            spend(f, $(Gem))
            f.discard = f.discard.diff($(c))
            f.draw = c +: f.draw
            f.fame += c.fame

            f.log("spent", Gem, "and gained", c.fame.hl, FameIcon(), "for", c, Comma, "now on top of their draw pile")

            game.note("gem")

            Then((stage == 0).?(GemPlayAction(f) : ForcedAction).|(TurnAction(f, stage)))

        case GemPlayAction(f) =>
            CommonExpansion.playChoices(f, 0)

        // TRADES
        case MineralTradeAction(f, pay, also, x, then) =>
            spend(f, pay)
            also.foreach(r => f.gain(r, -1))
            f.gain(x, 1)

            f.log("traded", MineralsLabel(pay), also./(_.elem).|(Empty), "for", x)

            game.note("mineral-trade")

            Then(CommonExpansion.afterTrade(f, then))

        // WINTER: before an Unrest card, the minerals may pay what is missing
        case WinterPayAction(f :: rest) if game.has(MineralsModule) && winterPayments(f).any =>
            Ask(f).each(winterPayments(f))(p => MineralWinterAction(f, p, rest)).add(MineralWinterSkipAction(f, rest))

        case MineralWinterAction(f, pay, rest) =>
            val (food, wood) = short(f)

            spend(f, pay)
            f.food += food
            f.wood += wood

            f.log("paid", MineralsLabel(pay), "for", $[(Int, Resource)](food -> Food, wood -> Wood).filter(_._1 > 0).map { case (n, r) => Amount(n.hl, r.elem) }.join(" and "), "of their Winter costs")

            game.note("mineral-winter")

            Then(WinterPayAction(f :: rest))

        // The same as WinterPayAction without the minerals
        case MineralWinterSkipAction(f, rest) =>
            if (CommonExpansion.payWinter(f))
                Ask(f).add(CommonExpansion.unrestLeft.?(TakeUnrestAction(f, f.winterUnits, rest)).|(UnrestGoneAction(f, f.winterUnits, rest))).needOk
            else
                Then(WinterPayAction(rest))

        case _ => UnknownContinue
    }
}
