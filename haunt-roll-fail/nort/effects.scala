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


// Card effects beyond the basic actions (map.scala) and the drawing cards (game.scala)

// Hunters, Woodcutters, Loremasters: recruit 1 unit per resource of that kind in each territory
case class RecruitPerEffect(r : Resource) extends Effect
// Call to War: 1 unit in each territory next to an enemy territory
case object CallToWarEffect extends Effect
// Osmosis: 1 unit in up to 3 different territories, each open or with wood on its tiles
case object OsmosisEffect extends Effect
// Raven Mercenaries: Recruit 2 in one neutral territory, may pay any 2 resources for 1 more
case object MercenariesEffect extends Effect
// Plunder: remove 1 unit from an adjacent enemy territory and draw 1 card
case object PlunderEffect extends Effect
// Capture: remove 1 enemy unit from an adjacent territory and add 1 unit to one of yours
case object CaptureEffect extends Effect
// Raiding Party: remove 1 enemy unit and collect 1 resource shown on its territory
case object RaidEffect extends Effect
// Future Sight: take an available Development or Achievement card now instead of when passing
case object FutureSightEffect extends Effect
// Conqueror: ignore enemy Defense Towers and Fortresses this year
case object ConquerorEffect extends Effect
// Hidden Ways: move any units from one territory to any open territory
case object HiddenWaysEffect extends Effect
// Bribery: move up to 2 enemy units from one territory to an adjacent one
case object BriberyEffect extends Effect
// Teamwork: two different basic actions in any order
case object TeamworkEffect extends Effect
// Annexation: Move 1 and, before or after, may Explore
case object AnnexationEffect extends Effect
// Spy: discard 1 card from an opponent's hand, draw 1
case object SpyEffect extends Effect
// Ancestral Curse: each opponent discards a card of their choice, draw 1
case object CurseEffect extends Effect
// Rapacious Exploitation: pick a card from an opponent's hand; they discard it or give 2 resources
case object RapaciousEffect extends Effect
// Enemy Secrets: copy a card in the active area of an adjacent enemy
case object SecretsEffect extends Effect
// Stolen Lore: copy a card in the active area of the player with the Scorched Earth token
case object StolenLoreEffect extends Effect
// Legendary Heroes: resolve again a card played this year
case object HeroesEffect extends Effect
// Defensive Strategy: only played on an opponent's turn, to cancel their card
case object DefensiveEffect extends Effect


// Up to n units in a territory
case class Cap(area : AreaRef, n : Int) extends Record

case class RecruitCapsAction(f : Faction, caps : $[Cap], placed : $[AreaRef], then : ForcedAction) extends ForcedAction
case class RecruitCapAction(self : Faction, area : AreaRef, caps : $[Cap], placed : $[AreaRef], then : ForcedAction) extends BaseAction("Recruit in")(area) with MapTarget { def target = area }

case class MercenariesExtraAction(f : Faction, then : ForcedAction) extends ForcedAction
case class MercenariesPayAction(self : Faction, pay : $[Resource], area : AreaRef, then : ForcedAction) extends BaseAction("Raven Mercenaries", "one more unit in", area)("Pay", pay./(_.elem).join(" "))

case class PlunderAction(self : Faction, area : AreaRef, enemy : Faction, then : ForcedAction) extends BaseAction("Plunder", "remove a unit in")(area, "(" ~ enemy.elem ~ ")") with MapTarget { def target = area }
case class CaptureAction(self : Faction, area : AreaRef, enemy : Faction, then : ForcedAction) extends BaseAction("Capture", "remove a unit in")(area, "(" ~ enemy.elem ~ ")") with MapTarget { def target = area }
case class CaptureAddAction(self : Faction, area : AreaRef, then : ForcedAction) extends BaseAction("Capture", "add a unit in")(area) with MapTarget { def target = area }
case class RaidAction(self : Faction, area : AreaRef, enemy : Faction, then : ForcedAction) extends BaseAction("Raiding Party", "remove a unit in")(area, "(" ~ enemy.elem ~ ")") with MapTarget { def target = area }
case class RaidCollectAction(self : Faction, r : Resource, then : ForcedAction) extends BaseAction("Raiding Party", "collect")(r)

case class FutureSightAction(self : Faction, card : Card, then : ForcedAction) extends BaseAction("Future Sight", "take a card")(card.img, Break, card)

case class HiddenFromAction(self : Faction, from : AreaRef, then : ForcedAction) extends BaseAction("Hidden Ways", "move from")(from) with Soft with MapTarget { def target = from }
case class HiddenToAction(self : Faction, from : AreaRef, to : AreaRef, then : ForcedAction) extends BaseAction("Hidden Ways", "move from", from, "to")(to) with Soft with MapTarget { def target = to }
case class HiddenUnitsAction(self : Faction, from : AreaRef, to : AreaRef, n : Int, kaija : Boolean, then : ForcedAction) extends BaseAction("Hidden Ways", "move from", from, "to", to)(Figures(n, kaija))

case class BriberyFromAction(self : Faction, from : AreaRef, enemy : Faction, then : ForcedAction) extends BaseAction("Bribery", "move units from")(from, "(" ~ enemy.elem ~ ")") with Soft with MapTarget { def target = from }
case class BriberyToAction(self : Faction, from : AreaRef, enemy : Faction, to : AreaRef, then : ForcedAction) extends BaseAction("Bribery", "move", enemy, "units from", from, "to")(to) with Soft with MapTarget { def target = to }
case class BriberyUnitsAction(self : Faction, from : AreaRef, enemy : Faction, to : AreaRef, n : Int, then : ForcedAction) extends BaseAction("Bribery", "move", enemy, "units from", from, "to", to)(Figures(n, false))

case class TeamworkAction(f : Faction, done : $[Effect], then : ForcedAction) extends ForcedAction
case class TeamworkChoiceAction(self : Faction, e : Effect, done : $[Effect], then : ForcedAction) extends BaseAction("Teamwork", done.any.?("second action").|("first action"))(FeastLabel(e))

case class AnnexationOrderAction(self : Faction, exploreFirst : Boolean, then : ForcedAction) extends BaseAction("Annexation")(exploreFirst.?("Explore, then Move 1").|("Move 1, then maybe Explore"))
case class AnnexationExploreAction(f : Faction, then : ForcedAction) extends ForcedAction
case class AnnexationExploreYesAction(self : Faction, then : ForcedAction) extends BaseAction("Annexation")("Explore")

case class SpyAction(self : Faction, enemy : Faction, then : ForcedAction) extends BaseAction("Spy", "look at the hand of")(enemy)
case class SpyDiscardAction(self : Faction, enemy : Faction, card : Card, then : ForcedAction) extends BaseAction("Spy", "discard from", enemy, "hand")(card.img, Break, card)

case class CurseAction(f : Faction, l : $[Faction], then : ForcedAction) extends ForcedAction
case class CurseDiscardAction(self : Faction, f : Faction, card : Card, l : $[Faction], then : ForcedAction) extends BaseAction("Ancestral Curse", "discard a card")(card.img, Break, card)

case class RapaciousAction(self : Faction, enemy : Faction, then : ForcedAction) extends BaseAction("Rapacious Exploitation", "look at the hand of")(enemy)
case class RapaciousPickAction(self : Faction, enemy : Faction, card : Card, then : ForcedAction) extends BaseAction("Rapacious Exploitation", "choose from", enemy, "hand")(card.img, Break, card)
case class RapaciousDiscardAction(self : Faction, f : Faction, card : Card, then : ForcedAction) extends BaseAction("Rapacious Exploitation", "by", f)("Discard", card)
case class RapaciousGiveAction(self : Faction, f : Faction, card : Card, pay : $[Resource], then : ForcedAction) extends BaseAction("Rapacious Exploitation", "by", f)("Keep", card, "and give", pay./(_.elem).join(" "))

case class CopyEffectAction(self : Faction, owner : Faction, card : Card, then : ForcedAction) extends BaseAction("Copy a card of", owner)(card.img, Break, card)

case class DefensiveAskAction(f : Faction, card : Card, stage : Int, l : $[Faction]) extends ForcedAction
case class DefensiveCancelAction(self : Faction, f : Faction, card : Card, stage : Int) extends BaseAction("Defensive Strategy", "against", f)("Cancel", card)
case class DefensiveAllowAction(self : Faction, f : Faction, card : Card, stage : Int, l : $[Faction]) extends BaseAction("Defensive Strategy", "against", f)("Let", card, "resolve")


object CardsExpansion extends Expansion {
    val DefensiveStrategy = Development("defensive-strategy")

    val basics : $[Effect] = $(RecruitEffect(1), MoveEffect(1), ExploreEffect(), BuildEffect())

    // Enemy units: territories and factions with units (not just Kaija) other than f
    def enemyUnits(f : Faction)(implicit game : Game) : $[(Territory, Faction)] =
        game.board.territories./~(t => game.present(t).but(f).%(g => game.count(t, g) > 0)./(g => t -> g))

    // The same, next to a territory f controls
    def adjacentEnemyUnits(f : Faction)(implicit game : Game) : $[(Territory, Faction)] = {
        val mine = game.controlled(f)
        enemyUnits(f).%{ case (t, _) => game.present(t).has(f).not && mine.exists(m => game.board.adjacent(m).exists(_._1 == t)) }
    }

    def perCaps(f : Faction, r : Resource)(implicit game : Game) : $[Cap] =
        game.controlled(f)./{ t =>
            val (food, wood, lore) = game.produce(t)
            Cap(t.anchor, r match { case Food => food; case Wood => wood; case Lore => lore })
        }.%(_.n > 0)

    def callToWarCaps(f : Faction)(implicit game : Game) : $[Cap] =
        game.controlled(f).%(t => game.board.adjacent(t).exists { case (o, _) => game.present(o).but(f).any })./(t => Cap(t.anchor, 1))

    def hiddenSources(f : Faction)(implicit game : Game) = game.controlled(f).%(t => game.board.territories.exists(o => o != t && game.board.open(o)))

    def briberySources(f : Faction)(implicit game : Game) : $[(Territory, Faction)] =
        enemyUnits(f).%{ case (t, g) => briberyTargets(t, g).any }

    // Adjacent territories enemy units can be moved to, not making a three-way territory
    def briberyTargets(t : Territory, g : Faction)(implicit game : Game) : $[Territory] =
        game.board.adjacent(t).map(_._1).%(o => game.present(o).but(g).num <= 1)

    def futureSightCards(f : Faction)(implicit game : Game) : $[Card] =
        f.foresaw.not.??(game.display ++ (game.year < game.lastYear).??(game.achievements))

    def opponentsWithCards(f : Faction)(implicit game : Game) = factions.but(f).%(_.hand.any)

    // Effects that may be copied: not the copying cards themselves
    def copyable(f : Faction, e : Effect)(implicit game : Game) = e match {
        case SecretsEffect | StolenLoreEffect | HeroesEffect | DefensiveEffect | MapEffect => false
        case e => CommonExpansion.playableEffect(f, e)
    }

    def secretsCards(f : Faction)(implicit game : Game) : $[(Faction, Card)] = {
        val mine = game.controlled(f)
        val owners = game.board.territories.%(t => game.present(t).has(f).not && mine.exists(m => game.board.adjacent(m).exists(_._1 == t)))./~(t => game.present(t).single).distinct.but(f)
        owners./~(g => g.active.distinct.%(c => copyable(f, c.effect))./(c => g -> c))
    }

    def stolenLoreCards(f : Faction)(implicit game : Game) : $[(Faction, Card)] =
        game.scorched./(game.board.territory)./(game.present).|($).single.but(f)./~(g => g.active.distinct.%(c => copyable(f, c.effect))./(c => g -> c))

    def heroesCards(f : Faction)(implicit game : Game) : $[Card] = f.played.distinct.%(c => copyable(f, c.effect))

    def playable(f : Faction, e : Effect)(implicit game : Game) : Boolean = e match {
        case RecruitPerEffect(r) => game.reserve(f) > 0 && perCaps(f, r).any
        case CallToWarEffect => game.reserve(f) > 0 && callToWarCaps(f).any
        case OsmosisEffect => MapExpansion.canRecruit(f) && MapExpansion.recruitTargets(f, RecruitOsmosis, $).any
        case MercenariesEffect => MapExpansion.canRecruit(f) && MapExpansion.recruitTargets(f, RecruitNeutralSame, $).any
        case PlunderEffect => adjacentEnemyUnits(f).any
        case CaptureEffect => adjacentEnemyUnits(f).any
        case RaidEffect => enemyUnits(f).any
        case FutureSightEffect => futureSightCards(f).any
        case ConquerorEffect => f.conqueror.not
        case HiddenWaysEffect => hiddenSources(f).any
        case BriberyEffect => briberySources(f).any
        case TeamworkEffect => basics.exists(MapExpansion.playable(f, _))
        case AnnexationEffect => MapExpansion.playable(f, MoveEffect(1)) || MapExpansion.playable(f, ExploreEffect())
        case SpyEffect => opponentsWithCards(f).any
        case CurseEffect => opponentsWithCards(f).any
        case RapaciousEffect => opponentsWithCards(f).any
        case SecretsEffect => secretsCards(f).any
        case StolenLoreEffect => stolenLoreCards(f).any
        case HeroesEffect => heroesCards(f).any
        case _ => false
    }

    def resolve(f : Faction, e : Effect, then : ForcedAction)(implicit game : Game) : Continue = e match {
        case RecruitPerEffect(r) => Then(RecruitCapsAction(f, perCaps(f, r), $, then))
        case CallToWarEffect => Then(RecruitCapsAction(f, callToWarCaps(f), $, then))
        case OsmosisEffect => Then(RecruitAction(f, 3, RecruitOsmosis, $, then))
        case MercenariesEffect => Then(RecruitAction(f, 2, RecruitNeutralSame, $, MercenariesExtraAction(f, then)))

        case PlunderEffect =>
            Ask(f).each(adjacentEnemyUnits(f))((t, g) => PlunderAction(f, t.anchor, g, then))

        case CaptureEffect =>
            Ask(f).each(adjacentEnemyUnits(f))((t, g) => CaptureAction(f, t.anchor, g, then))

        case RaidEffect =>
            Ask(f).each(enemyUnits(f))((t, g) => RaidAction(f, t.anchor, g, then))

        case FutureSightEffect =>
            Ask(f).each(futureSightCards(f))(c => FutureSightAction(f, c, then))

        case ConquerorEffect =>
            f.conqueror = true
            f.log("will ignore enemy", DefenseTower, "and", Fortress, "this year")
            Then(then)

        case HiddenWaysEffect =>
            Ask(f).each(hiddenSources(f))(t => HiddenFromAction(f, t.anchor, then))

        case BriberyEffect =>
            Ask(f).each(briberySources(f))((t, g) => BriberyFromAction(f, t.anchor, g, then))

        case TeamworkEffect => Then(TeamworkAction(f, $, then))

        case AnnexationEffect =>
            Ask(f)
                .when(MapExpansion.playable(f, ExploreEffect()))(AnnexationOrderAction(f, true, then))
                .add(AnnexationOrderAction(f, false, then))

        case SpyEffect =>
            Ask(f).each(opponentsWithCards(f))(g => SpyAction(f, g, then))

        case CurseEffect => Then(CurseAction(f, game.from(f).drop(1), then))

        case RapaciousEffect =>
            Ask(f).each(opponentsWithCards(f))(g => RapaciousAction(f, g, then))

        case SecretsEffect =>
            if (secretsCards(f).none) {
                f.log("found no card to copy")
                Then(then)
            }
            else
                Ask(f).each(secretsCards(f))((g, c) => CopyEffectAction(f, g, c, then))

        case StolenLoreEffect =>
            if (stolenLoreCards(f).none) {
                f.log("found no card to copy")
                Then(then)
            }
            else
                Ask(f).each(stolenLoreCards(f))((g, c) => CopyEffectAction(f, g, c, then))

        case HeroesEffect =>
            if (heroesCards(f).none) {
                f.log("found no card to copy")
                Then(then)
            }
            else
                Ask(f).each(heroesCards(f))(c => CopyEffectAction(f, f, c, then))

        case _ => Then(then)
    }

    def perform(action : Action, soft : Void)(implicit game : Game) = action @@ {
        // RECRUIT A NUMBER PER TERRITORY
        case RecruitCapsAction(f, caps, placed, then) =>
            val l = caps.%(_.n > 0)
            val total = l./(_.n).sum

            if (l.none || game.reserve(f) == 0)
                Then(TrainingCampsAction(f, placed, then))
            else
            if (total <= game.reserve(f)) {
                l.foreach { c =>
                    game.addUnits(c.area, f, c.n)
                    f.log("recruited", c.n.hl, "in", c.area)
                }
                Then(TrainingCampsAction(f, placed ++ l./(_.area), then))
            }
            else
                Ask(f).each(l./(_.area))(a => RecruitCapAction(f, a, l, placed, then))

        case RecruitCapAction(f, a, caps, placed, then) =>
            game.addUnits(a, f, 1)
            f.log("recruited in", a)
            Then(RecruitCapsAction(f, caps./(c => (c.area == a).?(c.copy(n = c.n - 1)).|(c)), placed :+ a, then))

        // RAVEN MERCENARIES
        case MercenariesExtraAction(f, then) =>
            val pp = CommonExpansion.payments(f, 2)

            game.recruited.lastOption.%(_ => game.reserve(f) > 0 && pp.any) match {
                case Some(a) => Ask(f).each(pp)(p => MercenariesPayAction(f, p, a, then)).skip(then)
                case None => Then(then)
            }

        case MercenariesPayAction(f, p, a, then) =>
            p.foreach(r => f.gain(r, -1))
            game.addUnits(a, f, 1)
            f.log("paid", p./(_.elem).join(" "), "and recruited one more unit in", a)
            Then(then)

        // REMOVING ENEMY UNITS
        case PlunderAction(f, a, g, then) =>
            game.removeUnits(game.board.territory(a), g, 1)
            f.log("removed a unit of", g, "in", a)
            Then(DrawCardsAction(f, 1, then))

        case CaptureAction(f, a, g, then) =>
            game.removeUnits(game.board.territory(a), g, 1)
            f.log("removed a unit of", g, "in", a)

            val mine = game.controlled(f)
            if (mine.none || game.reserve(f) == 0)
                Then(then)
            else
                Ask(f).each(mine)(t => CaptureAddAction(f, t.anchor, then))

        case CaptureAddAction(f, a, then) =>
            game.addUnits(a, f, 1)
            f.log("added a unit in", a)
            Then(then)

        case RaidAction(f, a, g, then) =>
            val t = game.board.territory(a)
            game.removeUnits(t, g, 1)
            f.log("removed a unit of", g, "in", a)

            val (food, wood, lore) = game.produce(t)
            val l = $[(Resource, Int)](Food -> food, Wood -> wood, Lore -> lore).filter(_._2 > 0).map(_._1)

            if (l.none)
                Then(then)
            else
                Ask(f).each(l)(r => RaidCollectAction(f, r, then))

        case RaidCollectAction(f, r, then) =>
            f.gain(r, 1)
            f.log("collected", 1.hl, r)
            Then(then)

        // FUTURE SIGHT
        case FutureSightAction(f, c, then) =>
            game.display = game.display.diff($(c))
            game.achievements = game.achievements.diff($(c))
            f.draw = c +: f.draw
            f.foresaw = true

            f.log("took", c, "and placed it on top of their draw pile")

            Then(then)

        // HIDDEN WAYS
        case HiddenFromAction(f, from, then) =>
            val t = game.board.territory(from)
            Ask(f).each(game.board.territories.%(o => o != t && game.board.open(o)))(o => HiddenToAction(f, from, o.anchor, then)).cancel

        case HiddenToAction(f, from, to, then) =>
            val t = game.board.territory(from)
            val n = game.count(t, f)
            val kaija = game.kaijaIn(t, f) && (game.awakened || game.present(game.board.territory(to)).but(f).none)

            Ask(f)
                .each(n.to(1, -1).$)(k => HiddenUnitsAction(f, from, to, k, false, then))
                .some(kaija.$(n.to(0, -1).$).flatten)(k => $(HiddenUnitsAction(f, from, to, k, true, then)))
                .cancel

        case HiddenUnitsAction(f, from, to, n, kaija, then) =>
            val src = game.board.territory(from)
            val dst = game.board.territory(to)

            game.removeUnits(src, f, n)
            game.addUnits(dst.anchor, f, n)
            if (kaija)
                game.kaija = |(dst.anchor)

            val enemy = game.present(dst).but(f)

            f.log("moved", Figures(n, kaija), "from", from, "to", to, enemy.any.?("and attacked " ~ enemy./(_.elem).join(", ")).|(Empty))

            if (enemy.any)
                game.combats :+= dst.anchor

            Then(CombatsAction(f, MoveEffect(0), then))

        // BRIBERY
        case BriberyFromAction(f, from, g, then) =>
            val t = game.board.territory(from)
            Ask(f).each(briberyTargets(t, g))(o => BriberyToAction(f, from, g, o.anchor, then)).cancel

        case BriberyToAction(f, from, g, to, then) =>
            val n = math.min(2, game.count(game.board.territory(from), g))
            Ask(f).each(n.to(1, -1).$)(k => BriberyUnitsAction(f, from, g, to, k, then)).cancel

        case BriberyUnitsAction(f, from, g, to, n, then) =>
            val dst = game.board.territory(to)

            game.removeUnits(game.board.territory(from), g, n)
            game.addUnits(dst.anchor, g, n)

            val other = game.present(dst).but(g)

            f.log("moved", Figures(n, false), "of", g, "from", from, "to", to, other.any.?("into a fight with " ~ other./(_.elem).join(", ")).|(Empty))

            if (other.any)
                Then(FightAction(g, dst.anchor, MoveEffect(0), then))
            else
                Then(then)

        // TEAMWORK
        case TeamworkAction(f, done, then) =>
            val l = basics.%(e => done.has(e).not).%(MapExpansion.playable(f, _))

            if (l.none || done.num >= 2)
                Then(then)
            else
                Ask(f).each(l)(e => TeamworkChoiceAction(f, e, done, then)).skipIf(done.any)(then)

        case TeamworkChoiceAction(f, e, done, then) =>
            MapExpansion.resolve(f, e, TeamworkAction(f, done :+ e, then))

        // ANNEXATION
        case AnnexationOrderAction(f, true, then) =>
            Then(ExploreAction(f, 1, 1, false, ExploreEffect(), MoveAction(f, 1, MoveEffect(1), then)))

        case AnnexationOrderAction(f, false, then) =>
            Then(MoveAction(f, 1, MoveEffect(1), AnnexationExploreAction(f, then)))

        case AnnexationExploreAction(f, then) =>
            if (MapExpansion.playable(f, ExploreEffect()))
                Ask(f).add(AnnexationExploreYesAction(f, then)).skip(then)
            else
                Then(then)

        case AnnexationExploreYesAction(f, then) =>
            Then(ExploreAction(f, 1, 1, false, ExploreEffect(), then))

        // LOOKING AT HANDS
        case SpyAction(f, g, then) =>
            f.log("looked at the hand of", g)
            Ask(f).each(g.hand.distinct)(c => SpyDiscardAction(f, g, c, then))

        case SpyDiscardAction(f, g, c, then) =>
            g.hand = g.hand.diff($(c))
            g.discard :+= c
            f.log("made", g, "discard", c)
            Then(DrawCardsAction(f, 1, then))

        case CurseAction(f, l, then) =>
            l.%(_.hand.any) match {
                case g :: rest => Ask(g).each(g.hand.distinct)(c => CurseDiscardAction(g, f, c, rest, then))
                case Nil => Then(DrawCardsAction(f, 1, then))
            }

        case CurseDiscardAction(g, f, c, l, then) =>
            g.hand = g.hand.diff($(c))
            g.discard :+= c
            g.log("discarded", c)
            Then(CurseAction(f, l, then))

        case RapaciousAction(f, g, then) =>
            f.log("looked at the hand of", g)
            Ask(f).each(g.hand.distinct)(c => RapaciousPickAction(f, g, c, then))

        case RapaciousPickAction(f, g, c, then) =>
            f.log("chose", c, "from the hand of", g)
            Ask(g).add(RapaciousDiscardAction(g, f, c, then)).each(CommonExpansion.payments(g, 2))(p => RapaciousGiveAction(g, f, c, p, then))

        case RapaciousDiscardAction(g, f, c, then) =>
            g.hand = g.hand.diff($(c))
            g.discard :+= c
            g.log("discarded", c)
            Then(then)

        case RapaciousGiveAction(g, f, c, p, then) =>
            p.foreach { r =>
                g.gain(r, -1)
                f.gain(r, 1)
            }
            g.log("gave", p./(_.elem).join(" "), "to", f, "to keep the card")
            Then(then)

        // COPYING
        case CopyEffectAction(f, g, c, then) =>
            f.log("copied", c)
            Then(ResolveEffectAction(f, c.effect, then))

        // DEFENSIVE STRATEGY
        case DefensiveAskAction(f, c, stage, l) =>
            l match {
                case h :: rest if h.hand.has(DefensiveStrategy) => Ask(h).add(DefensiveCancelAction(h, f, c, stage)).add(DefensiveAllowAction(h, f, c, stage, rest))
                case _ :: rest => Then(DefensiveAskAction(f, c, stage, rest))
                case Nil => Then(PlayResolveAction(f, c, stage))
            }

        case DefensiveCancelAction(h, f, c, stage) =>
            h.hand = h.hand.diff($(DefensiveStrategy))
            h.active :+= DefensiveStrategy
            f.active = f.active.diff($(c))
            f.played = f.played.diff($(c))
            f.discard :+= c

            h.log("played", DefensiveStrategy, "and cancelled", c)
            game.note("Defensive-Strategy")

            Then(TurnAction(f, stage))

        case DefensiveAllowAction(h, f, c, stage, rest) =>
            Then(DefensiveAskAction(f, c, stage, rest))

        case _ => UnknownContinue
    }
}
