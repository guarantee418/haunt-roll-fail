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


// Uncharted Horizons' Development cards (option HorizonsDevelopments; the cards are in Cards.horizonsEarly and
// Cards.horizonsAdvanced, their texts from the TTS mod 3597126237). RULES.md lists the choices made where a text is short

// Explorer: Move 2, then may draw 1 if no combat was triggered (ExplorerAfterAction)
case object ExplorerMove extends MoveSpecial
// Forged for War: Move 2, +1 combat point (axe) per territory of the attacker next to each combat
case object ForgedMove extends MoveSpecial
// The same once the points of one combat are counted (ForgedMove is kept for the next combat)
case object ForgedFight extends MoveSpecial

// Simple Trading (n = 1, once) and Local Trading (n = 2, any number of times): pay n resources for 1, then may draw 1
case class TradeEffect(n : Int) extends Effect
// Archery Range: roll a die against an adjacent enemy territory, 1 unit removed per skull (+1 with a Defense Tower)
case object ArcheryEffect extends Effect
// Silent Watchers: per Defense Tower (up to 3), a different one of: collect 1 resource there, recruit 1 there, draw 1
case object WatchersEffect extends Effect
// Fateful Gifts: 1 of each resource a closed territory f controls doesn't produce
case object GiftsEffect extends Effect
// Tamer: 1 unit to any neutral territory, or 1 creature moved once or twice, then activated
case object TamerEffect extends Effect
// Brewer: collect everything a territory f controls gives at harvest
case object BrewerEffect extends Effect
// Healer: Recruit 1, then may draw 1
case object HealerEffect extends Effect
// Fleeting Prosperity: a small building f controls works 3 times, then goes back to the reserve
case object ProsperityEffect extends Effect
// House: Build, then may draw 1
case object HouseEffect extends Effect
// Emissary: Explore, then may draw 1
case object EmissaryEffect extends Effect


case class ExplorerAfterAction(f : Faction, fights : Int, then : ForcedAction) extends ForcedAction

case class BarterAction(f : Faction, n : Int, once : Boolean, then : ForcedAction) extends ForcedAction
case class BarterPayAction(self : Faction, card : String, pay : $[Resource], get : Resource, once : Boolean, then : ForcedAction) extends BaseAction(card.hl, "exchange")(pay./(_.elem).join(" "), "for", get)
case class BarterDoneAction(self : Faction, card : String, then : ForcedAction) extends BaseAction(card.hl)("Exchange no more")

case class ArcheryAction(self : Faction, area : AreaRef, enemy : Faction, tower : Boolean, then : ForcedAction) extends BaseAction("Archery Range".hl, "shoot at")(area, InParens(enemy)) with MapTarget { def target = area }
case class ArcheryRolledAction(f : Faction, area : AreaRef, enemy : Faction, tower : Boolean, random : DieFace, then : ForcedAction) extends RandomAction[DieFace]

case class WatchersAction(f : Faction, l : $[AreaRef], used : $[Int], then : ForcedAction) extends ForcedAction
case class WatchersChoiceAction(self : Faction, area : AreaRef, option : Int, r : |[Resource], rest : $[AreaRef], used : $[Int], then : ForcedAction) extends BaseAction("Silent Watchers".hl, "for the", DefenseTower, "in", area)(WatchersLabel(option, r)) with MapTarget { def target = area }
case class WatchersDoneAction(self : Faction, then : ForcedAction) extends BaseAction("Silent Watchers".hl)("Use no more towers")

case class WatchersLabel(option : Int, r : |[Resource]) extends Elementary {
    def elem = option match {
        case 0 => "Recruit 1 unit there".txt
        case 1 => "Collect " ~ Amount(1.hl, r.get.elem)
        case _ => "Draw 1 card".txt
    }
}

case class GiftsAction(self : Faction, area : AreaRef, then : ForcedAction) extends BaseAction("Fateful Gifts".hl, "collect what isn't produced in")(area) with MapTarget { def target = area }

case class TamerUnitFromAction(self : Faction, area : AreaRef, then : ForcedAction) extends BaseAction("Tamer".hl, "move 1 unit to a neutral territory from")(area) with Soft with MapTarget { def target = area }
case class TamerUnitToAction(self : Faction, from : AreaRef, area : AreaRef, then : ForcedAction) extends BaseAction("Tamer".hl, "move 1 unit from", from, "to")(area) with MapTarget { def target = area }
case class TamerCreatureAction(self : Faction, c : Creature, then : ForcedAction) extends BaseAction("Tamer".hl, "move a creature")(c, "in", TamerWhere(c)) with Soft
case class TamerCreatureToAction(self : Faction, c : Creature, area : AreaRef, first : Boolean, then : ForcedAction) extends BaseAction("Tamer".hl, "move", c, first.?("to").|("on to"))(area) with MapTarget { def target = area }
case class TamerCreatureStopAction(self : Faction, c : Creature, then : ForcedAction) extends BaseAction("Tamer".hl)("Leave", c, "there")

case class TamerWhere(c : Creature) extends GameElementary {
    def elem(implicit game : Game) = game.creatureAt(c).elem
}

case class BrewerAction(self : Faction, area : AreaRef, then : ForcedAction) extends BaseAction("Brewer".hl, "collect as at harvest from")(area) with MapTarget { def target = area }

case class ProsperityAction(self : Faction, space : SpaceRef, building : Building, then : ForcedAction) extends BaseAction("Fleeting Prosperity".hl, "use 3 times, then remove")(building, "in", space.area) with MapTarget { def target = space.area }


object HorizonDevsExpansion extends Expansion {
    def tradeName(n : Int) = (n == 1).?("Simple Trading").|("Local Trading")

    // Territories f controls with a working Defense Tower
    def towers(f : Faction)(implicit game : Game) : $[Territory] = game.controlled(f).%(t => game.working(t).has(DefenseTower))

    // Archery Range: enemy units next to f's territories, with whether one of f's territories next to them has a Defense Tower
    def archeryTargets(f : Faction)(implicit game : Game) : $[(Territory, Faction, Boolean)] =
        CardsExpansion.adjacentEnemyUnits(f)./{ case (t, g) => (t, g, towers(f).exists(m => game.board.adjacent(m).exists(_._1 == t))) }

    def missing(t : Territory)(implicit game : Game) : $[Resource] = {
        val (food, wood, lore) = game.produce(t)
        $[(Resource, Int)](Food -> food, Wood -> wood, Lore -> lore).filter(_._2 == 0).map(_._1)
    }

    def giftTerritories(f : Faction)(implicit game : Game) = game.controlled(f).%(game.board.closed).%(t => missing(t).any)

    // Neutral territories a unit can be sent to: no figures, no creature, not the Swamp
    def neutral(implicit game : Game) = game.board.territories.%(t => game.present(t).none && game.creaturesIn(t).none && game.swampIn(t).not)

    def tamerSources(f : Faction)(implicit game : Game) = game.board.territories.%(t => game.count(t, f) > 0)

    // Where a creature can be moved: an adjacent territory without a creature
    def creatureMoves(c : Creature)(implicit game : Game) : $[Territory] =
        game.board.adjacent(game.board.territory(game.creatureAt(c))).map(_._1).%(o => game.creaturesIn(o).none)

    def tamerCreatures(implicit game : Game) = game.creatureLine.%(c => game.creatureAt.contains(c) && creatureMoves(c).any)

    // Small buildings that do something when used: Food Silo, Woodcutter Lodge, Carved Stone, Training Camp
    def prosperous(f : Faction)(implicit game : Game) : $[(SpaceRef, Building)] =
        game.controlled(f)./~(t => game.buildingsIn(t).%{ case (_, b) => b.large.not && b != DefenseTower && game.working(t).has(b) })

    // A territory with at least one Rough border to another territory (Mountaineer)
    def rough(t : Territory)(implicit game : Game) : Boolean = t.areas.exists { a =>
        game.board.at(a.x, a.y).get.spec.borders.exists(b => b.rough && b.impassable.not && ((b.a == a.id && t.areas.has(a.copy(id = b.b)).not) || (b.b == a.id && t.areas.has(a.copy(id = b.a)).not)))
    }

    def playable(f : Faction, e : Effect)(implicit game : Game) : Boolean = e match {
        case TradeEffect(n) => f.resources >= n || CommonExpansion.available(f) > 0
        case ArcheryEffect => archeryTargets(f).any
        case WatchersEffect => towers(f).any
        case GiftsEffect => giftTerritories(f).any
        case TamerEffect => (tamerSources(f).any && neutral.any) || tamerCreatures.any
        case BrewerEffect => game.controlled(f).exists(t => NewBloodExpansion.resourcesIn(t).any)
        case HealerEffect => MapExpansion.playable(f, RecruitEffect(1))
        case ProsperityEffect => prosperous(f).any
        case HouseEffect => MapExpansion.playable(f, BuildEffect())
        case EmissaryEffect => MapExpansion.playable(f, ExploreEffect())
        case _ => false
    }

    def resolve(f : Faction, e : Effect, then : ForcedAction)(implicit game : Game) : Continue = e match {
        case TradeEffect(n) => Then(BarterAction(f, n, n == 1, MayDrawAction(f, then)))

        case ArcheryEffect =>
            Ask(f).each(archeryTargets(f)) { case (t, g, tower) => ArcheryAction(f, t.anchor, g, tower, then) }

        case WatchersEffect => Then(WatchersAction(f, towers(f)./(_.anchor), $, then))

        case GiftsEffect =>
            Ask(f).each(giftTerritories(f))(t => GiftsAction(f, t.anchor, then))

        case TamerEffect =>
            Ask(f)
                .add(neutral.any.??(tamerSources(f)./(t => TamerUnitFromAction(f, t.anchor, then))))
                .each(tamerCreatures)(c => TamerCreatureAction(f, c, then))

        case BrewerEffect =>
            Ask(f).each(game.controlled(f).%(t => NewBloodExpansion.resourcesIn(t).any))(t => BrewerAction(f, t.anchor, then))

        case HealerEffect => Then(RecruitAction(f, 1, RecruitNormal, $, MayDrawAction(f, then)))

        case ProsperityEffect =>
            Ask(f).each(prosperous(f)) { case (s, b) => ProsperityAction(f, s, b, then) }

        case HouseEffect => Then(BuildAction(f, BuildEffect(), 1, false, MayDrawAction(f, then)))

        case EmissaryEffect => Then(ExploreAction(f, 1, 1, false, ExploreEffect(), MayDrawAction(f, then)))

        case _ => Then(then)
    }

    def perform(action : Action, soft : Void)(implicit game : Game) = action @@ {
        // Draw 1 card or not (also New Blood's cards)
        case MayDrawAction(f, then) =>
            if (CommonExpansion.available(f) > 0)
                Ask(f).add(DrawCardsAction(f, 1, then).as("Draw 1 card")(f)).add(then.as("Draw no card")(f))
            else
                Then(then)

        // EXPLORER: the number of fights before the move tells whether it triggered one
        case MoveStartAction(f, e, then) if e.special == ExplorerMove =>
            Then(MoveStartAction(f, e.copy(special = PlainMove), ExplorerAfterAction(f, game.fights, then)))

        case ExplorerAfterAction(f, fights, then) =>
            if (game.fights == fights)
                Then(MayDrawAction(f, then))
            else
                Then(then)

        // FORGED FOR WAR: +1 point per territory of the attacker next to the combat, counted when it starts
        case FightStartAction(f, a, e, then) if e.special == ForgedMove =>
            Then(FightStartAction(f, a, forged(f, a, e), then))

        case CreatureFightAction(f, a, c, e, then) if e.special == ForgedMove =>
            Then(CreatureFightAction(f, a, c, forged(f, a, e), then))

        // TRADING
        case BarterAction(f, n, once, then) =>
            // Never give back a resource that is paid in
            val pays = CommonExpansion.payments(f, n)./(_.sortBy(Resource.all.indexOf(_))).distinct.%(p => Resource.all.exists(r => p.has(r).not))

            if (pays.none)
                Then(then)
            else
                Ask(f).some(pays)(p => Resource.all.%(r => p.has(r).not)./(r => BarterPayAction(f, tradeName(n), p, r, once, then))).add(BarterDoneAction(f, tradeName(n), then))

        case BarterPayAction(f, _, pay, get, once, then) =>
            pay.foreach(r => f.gain(r, -1))
            f.gain(get, 1)
            f.log("exchanged", pay./(_.elem).join(" "), "for", 1.hl, get, "with", tradeName(pay.num).hl)
            Then(once.?(then).|(BarterAction(f, pay.num, once, then)))

        case BarterDoneAction(f, _, then) =>
            Then(then)

        // ARCHERY RANGE
        case ArcheryAction(f, a, g, tower, then) =>
            Random[DieFace](NorthgardDie.faces, ArcheryRolledAction(f, a, g, tower, _, then))

        case ArcheryRolledAction(f, a, g, tower, face, then) =>
            // A face with a choice counts as a skull
            val skulls = face.casualties + face.choice.??(1)
            val t = game.board.territory(a)
            val n = math.min(game.count(t, g), skulls + tower.??(1))

            f.log("rolled", face, "with", "Archery Range".hl, tower.?("(" ~ DefenseTower.elem ~ " " ~ "+1" ~ ")").|(Empty))

            if (n > 0) {
                game.removeUnits(t, g, n)
                f.log("removed", n.hl, (n == 1).?("unit").|("units"), "of", g, "in", a)
            }
            else
                f.log("removed no unit")

            Then(then)

        // SILENT WATCHERS
        case WatchersAction(f, l, used, then) =>
            l match {
                case a :: rest if used.num < 3 =>
                    val recruit = (used.has(0).not && game.reserve(f) > 0).$(WatchersChoiceAction(f, a, 0, None, rest, used :+ 0, then))
                    val collect = used.has(1).not.??(NewBloodExpansion.resourcesIn(game.board.territory(a))./(r => WatchersChoiceAction(f, a, 1, |(r), rest, used :+ 1, then)))
                    val draw = (used.has(2).not && CommonExpansion.available(f) > 0).$(WatchersChoiceAction(f, a, 2, None, rest, used :+ 2, then))
                    val all = recruit ++ collect ++ draw

                    if (all.none)
                        Then(WatchersAction(f, rest, used, then))
                    else
                        Ask(f).add(all).add(WatchersAction(f, rest, used, then).as("Nothing for this tower")("Silent Watchers".hl)).add(WatchersDoneAction(f, then))

                case _ => Then(then)
            }

        case WatchersChoiceAction(f, a, option, r, rest, used, then) =>
            val next = WatchersAction(f, rest, used, then)

            option match {
                case 0 =>
                    game.addUnits(a, f, 1)
                    f.log("recruited in", a, "with", "Silent Watchers".hl)
                    Then(next)
                case 1 =>
                    f.gain(r.get, 1)
                    f.log("collected", 1.hl, r.get, "from", a, "with", "Silent Watchers".hl)
                    Then(next)
                case _ =>
                    f.log("drew a card with", "Silent Watchers".hl)
                    Then(DrawCardsAction(f, 1, next))
            }

        case WatchersDoneAction(f, then) =>
            Then(then)

        // FATEFUL GIFTS
        case GiftsAction(f, a, then) =>
            val l = missing(game.board.territory(a))
            l.foreach(r => f.gain(r, 1))
            f.log("collected", l./(r => Amount(1.hl, r.elem)).join(", "), "with", "Fateful Gifts".hl)
            Then(then)

        // TAMER
        case TamerUnitFromAction(f, a, then) =>
            Ask(f).each(neutral.%(t => t.areas.contains(a).not))(t => TamerUnitToAction(f, a, t.anchor, then)).cancel

        case TamerUnitToAction(f, from, to, then) =>
            game.removeUnits(game.board.territory(from), f, 1)
            game.addUnits(to, f, 1)
            f.log("moved a unit from", from, "to", to, "with", "Tamer".hl)
            Then(then)

        case TamerCreatureAction(f, c, then) =>
            Ask(f).each(creatureMoves(c))(t => TamerCreatureToAction(f, c, t.anchor, true, then)).cancel

        case TamerCreatureToAction(f, c, to, first, then) =>
            game.creatureAt += c -> to
            game.note("creature-move")
            f.log("moved", c, "to", to, "with", "Tamer".hl)

            val more = first.??(creatureMoves(c))

            if (more.any)
                Ask(f).each(more)(t => TamerCreatureToAction(f, c, t.anchor, false, then)).add(TamerCreatureStopAction(f, c, then))
            else
                Then(CreatureEffectAction(c, then))

        case TamerCreatureStopAction(f, c, then) =>
            Then(CreatureEffectAction(c, then))

        // BREWER
        case BrewerAction(f, a, then) =>
            MapExpansion.collect(f, game.board.territory(a), "with " ~ "Brewer".hl)
            Then(then)

        // FLEETING PROSPERITY
        case ProsperityAction(f, s, b, then) =>
            b match {
                case FoodSilo => f.gain(Food, 3) ; f.log("collected", 3.hl, Food, "from", b, "with", "Fleeting Prosperity".hl)
                case WoodcutterLodge => f.gain(Wood, 3) ; f.log("collected", 3.hl, Wood, "from", b, "with", "Fleeting Prosperity".hl)
                case CarvedStone => f.gain(Lore, 3) ; f.log("collected", 3.hl, Lore, "from", b, "with", "Fleeting Prosperity".hl)
                case _ =>
                    val n = math.min(3, game.reserve(f))
                    if (n > 0) {
                        game.addUnits(s.area, f, n)
                        f.log("recruited", n.hl, "with", b, "and", "Fleeting Prosperity".hl, "in", s.area)
                    }
            }

            game.buildings -= s
            f.log("returned", b, "in", s.area, "to the reserve")

            Then(then)

        case _ => UnknownContinue
    }

    // Forged for War: the attacker's territories next to the combat, the territory just won in an earlier combat included
    def forged(f : Faction, a : AreaRef, e : MoveEffect)(implicit game : Game) : MoveEffect = {
        val t = game.board.territory(a)
        val n = game.controlled(f).but(t).count(m => game.board.adjacent(m).exists(_._1 == t))

        if (n > 0)
            f.log("gained", CombatIcon.axes(n), "from", "Forged for War".hl)

        e.copy(bonus = e.bonus + n, special = ForgedFight)
    }
}
