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


// New Blood: seven more clans (Dragon, Horse, Kraken, Lynx, Ox, Rat, Squirrel), with their warchiefs.
// Clan powers and card texts are from the clan boards and cards (Tabletop Simulator mod 3597126237, made from
// the Tabletopia module); RULES.md has the summary and the choices made where they say nothing

// Card effects that aren't basic actions
// Dragon Clan: sacrifice 1 unit or place 1 deployed unit on the Pyre, then draw 1 and collect 1 food
case object DragonClanEffect extends Effect
// Capture for Sacrifice: an enemy unit next to f's territory goes on the Pyre; may bring 1 own unit back from it
case object SacrificeCaptureEffect extends Effect
// Reluctant Workforce: draw 1, may sacrifice up to 2 units for 1 card each
case object WorkforceEffect extends Effect
// Craftsmen: Recruit 2, then may replace a building
case object CraftsmenEffect extends Effect
// Quality of Life: a free Defense Tower needing no space, then 1 resource from each territory with a Defense Tower
case object QualityEffect extends Effect
// Kraken Clan: 1 food or 1 wood, with a High Tide token in a territory f controls; may draw 1
case object KrakenClanEffect extends Effect
// Endless Tide: may remove 1 enemy unit from an open territory for 1 lore; may draw 1
case object EndlessTideEffect extends Effect
// Knowledge from Beyond: per High Tide territory, one of Recruit 1, Collect 1, Draw 1 (each once)
case object KnowledgeEffect extends Effect
// Poaching: draw 2; a Flash card among them draws 1 more
case object PoachingEffect extends Effect
// Warcraft: Explore; 1 lore if the tile shows lore, otherwise another Explore
case object WarcraftEffect extends Effect
// City Builder: Build, then 1 lore and may draw 1
case object CityBuilderEffect extends Effect
// Rat Clan: may remove up to 2 units for 1 wood each, then Build
case object RatClanEffect extends Effect
// Overwork: may remove 1 unit to collect everything from 1 territory; may draw 1
case object OverworkEffect extends Effect
// Squirrel Clan: Recruit 1, then 1 food or fame for a quarter of the units in f's territories
case object SquirrelClanEffect extends Effect
// Cooking Mastery: Build, then may build a free Food Silo, even next to another
case object CookingEffect extends Effect
// Economics: draw 1, may pay up to 2 food for 1 card each
case object EconomicsEffect extends Effect


case class NewBloodLabel(s : String) extends Elementary {
    def elem = s.hl
}

// CLAN CARDS: the powers before (and after) the card
case class ClanCardAction(f : Faction, card : ClanCard, then : ForcedAction) extends ForcedAction

// DRAGON
case class PyreAskAction(f : Faction, optional : Boolean, then : ForcedAction) extends ForcedAction
case class SacrificeAction(self : Faction, owner : Faction, then : ForcedAction) extends BaseAction("Sacrificial Pyre".hl)("Sacrifice a unit of", owner)
case class PyrePlaceAction(self : Faction, area : AreaRef, then : ForcedAction) extends BaseAction("Sacrificial Pyre".hl, "place one of your units from")(area) with MapTarget { def target = area }
case class DragonHarvestAction(f : Faction, ok : Boolean) extends ForcedAction
case class SacrificeCaptureAction(self : Faction, area : AreaRef, enemy : Faction, then : ForcedAction) extends BaseAction("Capture for Sacrifice".hl, "remove a unit in")(area, InParens(enemy)) with MapTarget { def target = area }
case class PyreReturnAction(self : Faction, area : AreaRef, then : ForcedAction) extends BaseAction("Capture for Sacrifice".hl, "return your unit from the Pyre to")(area) with MapTarget { def target = area }
case class WorkforceAction(f : Faction, left : Int, then : ForcedAction) extends ForcedAction
case class AfterHarvestNBAction(l : $[Faction], then : ForcedAction) extends ForcedAction
case class HarvestExtraAction(self : Faction, r : Resource, then : ForcedAction) extends BaseAction(self, "after harvesting")("Collect", 1.hl, r)

// HORSE
case class HorseClosedAction(f : Faction, l : $[AreaRef], then : ForcedAction) extends ForcedAction
case class HorseWoodAction(self : Faction, area : AreaRef, rest : $[AreaRef], then : ForcedAction) extends BaseAction("Horse Clan".hl, "closed", area)("Collect", 1.hl, Wood)
case class CraftsmenAction(f : Faction, then : ForcedAction) extends ForcedAction
case class CraftsmenReplaceAction(self : Faction, area : AreaRef, space : SpaceRef, building : Building, then : ForcedAction) extends BaseAction("Craftsmen".hl, "replace a building in", area, "with")(building, SupplyLeft(building)) with MapTarget { def target = area }
case class QualityBuildAction(self : Faction, area : AreaRef, then : ForcedAction) extends BaseAction("Quality of Life".hl, "build a free", DefenseTower, "in")(area) with MapTarget { def target = area }
case class QualityCollectAction(f : Faction, l : $[AreaRef], then : ForcedAction) extends ForcedAction
case class QualityTakeAction(self : Faction, area : AreaRef, r : Resource, rest : $[AreaRef], then : ForcedAction) extends BaseAction("Quality of Life".hl, "collect from", area)(r)
case class PrecisionPayAction(self : Faction, r : Resource, e : MoveEffect, then : ForcedAction) extends BaseAction("Eitria and Brok's Precision".hl, Amount("+1".hl, CombatIcon.axe) ~ " and Move 3")("Pay", 1.hl, r)

// KRAKEN
case class TidePlaceAction(self : Faction, area : AreaRef, then : ForcedAction) extends BaseAction("High Tide".hl, "place a token in")(area) with MapTarget { def target = area }
case class KaraTideAction(self : Faction, from : AreaRef, area : AreaRef, then : ForcedAction) extends BaseAction(WarchiefElem(Kraken), "move a", "High Tide".hl, "token to her territory from")(from) with MapTarget { def target = from }
case class KrakenCollectAction(self : Faction, r : Resource, then : ForcedAction) extends BaseAction("Kraken Clan".hl)("Collect", 1.hl, r)
case class EndlessTideAction(self : Faction, area : AreaRef, enemy : Faction, then : ForcedAction) extends BaseAction("Endless Tide".hl, "remove a unit for", 1.hl, Lore, "in")(area, InParens(enemy)) with MapTarget { def target = area }
case class KnowledgeAction(f : Faction, l : $[AreaRef], used : $[Int], then : ForcedAction) extends ForcedAction
case class KnowledgeChoiceAction(self : Faction, area : AreaRef, option : Int, r : |[Resource], rest : $[AreaRef], used : $[Int], then : ForcedAction) extends BaseAction("Knowledge from Beyond".hl, "for", area)(KnowledgeLabel(option, r)) with MapTarget { def target = area }

case class KnowledgeLabel(option : Int, r : |[Resource]) extends Elementary {
    def elem = option match {
        case 0 => "Recruit 1 unit there".txt
        case 1 => "Collect " ~ Amount(1.hl, r.get.elem)
        case _ => "Draw 1 card".txt
    }
}

// LYNX
case class LynxMoveAction(f : Faction, then : ForcedAction) extends ForcedAction
case class LynxToAction(self : Faction, to : AreaRef, then : ForcedAction) extends BaseAction("Brundr and Kaelinn".hl, "Move 1 to")(to) with Soft with MapTarget { def target = to }
case class LynxUnitsAction(self : Faction, to : AreaRef, n : Int, then : ForcedAction) extends BaseAction("Brundr and Kaelinn".hl, "Move 1 to", to, "with")((n == 0).?("no units".txt).|((n == 1).?("1 unit").|(n.toString + " units").txt))
case class PoachingAction(f : Faction, then : ForcedAction) extends ForcedAction

// OX
case class GearTakeAction(self : Faction, space : SpaceRef, n : Int, then : ForcedAction) extends BaseAction("Ancestral Equipment".hl, "take a token from")(space.area, "(" ~ GearName(n).elem ~ ")") with MapTarget { def target = space.area }
case class GearAskAction(attacker : Faction, defender : Faction, area : AreaRef, e : MoveEffect, then : ForcedAction) extends ForcedAction
case class GearUseAction(self : Faction, n : Int, area : AreaRef, limit : Int, hero : Boolean, then : ForcedAction) extends BaseAction("Ancestral Equipment".hl, "use a token in the fight in", area)(GearName(n))
case class TrueHeroAction(self : Faction, space : SpaceRef, building : Building, then : ForcedAction) extends BaseAction("The True Hero".hl, "give up Torfin's second token to remove a building in", space.area)(building) with MapTarget { def target = space.area }
case class GearRerollAction(self : Faction, again : ForcedAction) extends BaseAction("Ancestral Equipment".hl)("Reroll the combat die")
case class GearKeepAction(self : Faction, keep : ForcedAction) extends BaseAction("Ancestral Equipment".hl)("Keep the result")
case class WarcraftAfterAction(f : Faction, then : ForcedAction) extends ForcedAction

// The Ancestral Equipment tokens, by number
case class GearName(n : Int) extends Elementary {
    def text = n match {
        case 1 => "+1 point if you spend food"
        case 2 => "+1 point"
        case 3 => "reroll your die"
        case 4 => "+2 points"
        case 5 => "+2 points per food"
        case 6 => "+1 casualty"
        case _ => "ignore 1 casualty"
    }
    def elem = ("Token " + n).hl ~ " (" ~ CombatText(text) ~ ")"
}

// RAT
case class RatRemoveAction(self : Faction, area : AreaRef, left : Int, then : ForcedAction) extends BaseAction("Rat Clan".hl, "remove a unit for", 1.hl, Wood, "from")(area) with MapTarget { def target = area }
case class RatAskAction(f : Faction, left : Int, then : ForcedAction) extends ForcedAction
case class OverworkRemoveAction(self : Faction, area : AreaRef, then : ForcedAction) extends BaseAction("Overwork".hl, "remove a unit from")(area) with MapTarget { def target = area }
case class OverworkCollectAction(self : Faction, area : AreaRef, then : ForcedAction) extends BaseAction("Overwork".hl, "collect everything from")(area) with MapTarget { def target = area }

// SQUIRREL
case class SquirrelAfterAction(f : Faction, then : ForcedAction) extends ForcedAction
case class SquirrelFameAction(self : Faction, n : Int, then : ForcedAction) extends BaseAction("Squirrel Clan".hl)("Gain", n.hl, FameIcon())
case class CookingAction(f : Faction, then : ForcedAction) extends ForcedAction
case class EconomicsPayAction(self : Faction, n : Int, then : ForcedAction) extends BaseAction("Economics".hl, "pay food to draw cards")("Pay", n.hl, Food)

// Either: draw 1 card (optional)
case class MayDrawAction(f : Faction, then : ForcedAction) extends ForcedAction


object NewBloodExpansion extends Expansion {
    // A unit on the Pyre at the start (Dragon Clan's setup)
    val pyreSize = 2

    def room(implicit game : Game) = pyreSize - game.pyre.num

    // Territories with Dragon units that can go on the Pyre
    def deployed(f : Faction)(implicit game : Game) = game.board.territories.%(t => game.count(t, f) > 0)

    def sacrificeOptions(f : Faction)(implicit game : Game) : Boolean = game.pyre.any || (room > 0 && deployed(f).any)

    // Kraken's High Tide tokens leave with the last Kraken figure
    def cleanup()(implicit game : Game) {
        if (game.tides.any)
            game.tides = game.tides.%(a => game.figures(game.board.territory(a), Kraken) > 0).distinctBy(a => game.board.territory(a).anchor)
    }

    def tideTerritories(implicit game : Game) = game.controlled(Kraken).%(game.tideIn)

    def tideTargets(implicit game : Game) = (game.tides.num < 2).??(game.controlled(Kraken).%(t => game.tideIn(t).not))

    // Ox's tokens on the map in territories Ox controls
    def gearOnMap(f : Faction)(implicit game : Game) : $[(SpaceRef, Int)] =
        game.gear.toList.%{ case (s, _) => game.controlled(f).exists(_.areas.contains(s.area)) }.sortBy(_._2)

    // Dragon Clan's Surtr: a casualty if none was rolled, a point if none was rolled
    def surtr(self : Faction, t : Territory, face : DieFace)(implicit game : Game) : DieFace =
        if (self == Dragon && game.chiefIn(t, Dragon) && (face.points == 0 || face.casualties == 0)) {
            val f = DieFace(face.points.max(1), face.casualties.max(1), false)
            self.log("got", f, "with", WarchiefElem(Dragon))
            f
        }
        else
            face

    // Combat points from clan powers and cards (e: the attacker's card)
    def points(f : Faction, t : Territory, e : MoveEffect, attacking : Boolean, food : Int)(implicit game : Game) : Int =
        (attacking && f == Dragon && e.special == GrudgeMove).??(game.pyre.num) +
        (f == Ox).??(game.gearFight./{
            case 1 => (food > 0).??(1)
            case 2 => 1
            case 4 => 2
            case 5 => food
            case _ => 0
        }.sum)

    // Casualties from clan powers and cards
    def casualties(f : Faction, t : Territory, e : MoveEffect, attacking : Boolean)(implicit game : Game) : Int =
        (attacking && e.special == FireArrowsMove).??(1) +
        (f == Ox && game.gearFight.has(6)).??(1) +
        (f == Kraken && game.tideIn(t)).??(1)

    // Casualties f ignores
    def ignored(f : Faction)(implicit game : Game) : Int = (f == Ox && game.gearFight.has(7)).??(1)

    // After the casualties of a fight between players
    def afterCombat(attacker : Faction, defender : Faction, t : Territory, e : MoveEffect, aUnits : Int, dUnits : Int, aLost : Int, dLost : Int, winner : |[Faction])(implicit game : Game) {
        // Dragon Clan: enemy units lost go on the Sacrificial Pyre, while there is room
        // (Svarn's Menders puts the attacker's casualties on the card during the combat, so the Pyre doesn't get them)
        $((attacker, defender, dUnits), (defender, attacker, (e.special != SvarnMove).??(aUnits))).foreach { case (f, g, n) =>
            if (f == Dragon && game.enemy(f, g)) {
                val k = math.min(n, room)
                if (k > 0) {
                    game.pyre ++= 1.to(k).$./(_ => g)
                    f.log("put", k.hl, (k == 1).?("unit").|("units"), "of", g, "on the", "Sacrificial Pyre".hl)
                    game.note("pyre-capture")
                }
            }
        }

        // Blood Ties: 1 lore per casualty suffered
        if (attacker == Rat && e.special == BloodTiesMove && aLost > 0) {
            attacker.lore += aLost
            attacker.log("collected", aLost.hl, Lore, "with", "Blood Ties".hl)
        }

        // Howl from the Sea: a unit per casualty inflicted where Kraken wins, one where it retreats when it loses
        if (attacker == Kraken && e.special == HowlMove) {
            if (winner.has(attacker)) {
                val k = math.min(dLost, game.reserve(attacker))
                if (k > 0) {
                    game.addUnits(t.anchor, attacker, k)
                    attacker.log("added", k.hl, (k == 1).?("unit").|("units"), "with", "Howl from the Sea".hl)
                }
            }
            else
            if (winner.has(defender))
                game.howl = true
        }
    }

    def howl(f : Faction, to : AreaRef)(implicit game : Game) {
        if (f == Kraken && game.howl) {
            game.howl = false
            if (game.reserve(f) > 0) {
                game.addUnits(to, f, 1)
                f.log("added a unit in", to, "with", "Howl from the Sea".hl)
            }
        }
    }

    // After an Explore: Ox's token on a small building space of the tile, Rat's units in the territories it closed
    def explored(f : Faction, tile : String, spot : Spot, closed : $[Territory])(implicit game : Game) {
        if (f == Ox && game.gearPile.any) {
            val spaces = Tiles(tile).areas./~(a => a.spaces.indices.%(i => a.spaces(i).kind == SmallSpace)./(i => SpaceRef(AreaRef(spot.x, spot.y, a.id), i)))
                .%(s => game.buildings.contains(s).not && game.gear.contains(s).not)

            spaces.headOption.foreach { s =>
                val n = game.gearPile.head
                game.gearPile = game.gearPile.drop(1)
                game.gear += s -> n
                f.log("placed", GearName(n), "on the explored tile")
                game.note("gear-place")
            }
        }

        if (f == Rat)
            closed.foreach { t =>
                if (game.reserve(f) > 0) {
                    game.addUnits(t.anchor, f, 1)
                    f.log("placed a unit in the closed territory", t.anchor)
                    game.note("rat-close")
                }
            }
    }

    def tileHasLore(tile : String) = Tiles(tile).areas.exists(a => a.lore > 0 || a.spaces.exists(_.kind == CarvedSpace))

    // Lynx: Brundr and Kaelinn's Move 1 (with the units there) to a neutral or friendly territory
    def lynxTargets(implicit game : Game) : $[Territory] = game.lynx match {
        case Some(a) =>
            val t = game.board.territory(a)
            if (game.bearIn(t) || game.hostileIn(t)) $
            else game.board.adjacent(t).filter(_._2).map(_._1)
                .%(o => game.present(o).forall(_ == Lynx))
                .%(o => game.hostileIn(o).not && game.swampIn(o).not && MapExpansion.enterable(o))
        case None => $
    }

    // Territories without a Defense Tower where Quality of Life can put one
    def qualityTargets(f : Faction)(implicit game : Game) : $[Territory] =
        (game.buildings.values.count(_ == DefenseTower) < Building.tokens).??(game.controlled(f).%(t => game.bearIn(t).not && game.buildingsIn(t).exists(_._2 == DefenseTower).not))

    def resourcesIn(t : Territory)(implicit game : Game) : $[Resource] = {
        val (food, wood, lore) = game.harvest(t)
        $[(Resource, Int)](Food -> food, Wood -> wood, Lore -> lore).filter(_._2 > 0).map(_._1)
    }

    // The build options with some more wood (Rat Clan's units removed)
    def buildableWith(f : Faction, extra : Int)(implicit game : Game) : Boolean = {
        f.wood += extra
        val r = MapExpansion.buildOptions(f, BuildEffect(), false).any
        f.wood -= extra
        r
    }

    def unitsIn(f : Faction)(implicit game : Game) = game.controlled(f).%(t => game.count(t, f) > 0)

    def playable(f : Faction, e : Effect)(implicit game : Game) : Boolean = e match {
        case DragonClanEffect => sacrificeOptions(f)
        case SacrificeCaptureEffect => CardsExpansion.adjacentEnemyUnits(f).any
        case WorkforceEffect => CommonExpansion.available(f) > 0 || game.pyre.any
        case CraftsmenEffect => MapExpansion.playable(f, RecruitEffect(2)) || MapExpansion.replacements(f).any
        case QualityEffect => qualityTargets(f).any || game.controlled(f).exists(t => game.buildingsIn(t).exists(_._2 == DefenseTower))
        case KrakenClanEffect => tideTerritories.any || tideTargets.any
        case EndlessTideEffect => CardsExpansion.enemyUnits(f).exists(x => game.board.open(x._1)) || CommonExpansion.available(f) > 0
        case KnowledgeEffect => tideTerritories.any || tideTargets.any
        case PoachingEffect => CommonExpansion.available(f) > 0
        case WarcraftEffect => MapExpansion.playable(f, ExploreEffect())
        case CityBuilderEffect => MapExpansion.playable(f, BuildEffect())
        case RatClanEffect => buildableWith(f, math.min(2, game.onMap(f)))
        case OverworkEffect => unitsIn(f).any
        case SquirrelClanEffect => true
        case CookingEffect => MapExpansion.playable(f, BuildEffect())
        case EconomicsEffect => CommonExpansion.available(f) > 0
        case e => HorizonDevsExpansion.playable(f, e)
    }

    def resolve(f : Faction, e : Effect, then : ForcedAction)(implicit game : Game) : Continue = e match {
        case DragonClanEffect =>
            Then(PyreAskAction(f, false, DrawCardsAction(f, 1, ResolveEffectAction(f, CollectEffect(Food, 1), then))))

        case SacrificeCaptureEffect =>
            Ask(f).each(CardsExpansion.adjacentEnemyUnits(f))((t, g) => SacrificeCaptureAction(f, t.anchor, g, then))

        case WorkforceEffect =>
            Then(DrawCardsAction(f, 1, WorkforceAction(f, 2, then)))

        case CraftsmenEffect =>
            Then(RecruitAction(f, 2, RecruitNormal, $, CraftsmenAction(f, then)))

        case QualityEffect =>
            Ask(f).each(qualityTargets(f))(t => QualityBuildAction(f, t.anchor, then))
                .add(QualityCollectAction(f, game.controlled(f).%(t => game.buildingsIn(t).exists(_._2 == DefenseTower))./(_.anchor), then).as("Build no Defense Tower")("Quality of Life".hl))

        case KrakenClanEffect =>
            if (tideTerritories.none) {
                f.log("has no", "High Tide".hl, "token in a territory they control")
                Then(MayDrawAction(f, then))
            }
            else
                Ask(f).each($(Food, Wood))(r => KrakenCollectAction(f, r, then))

        case EndlessTideEffect =>
            Ask(f).each(CardsExpansion.enemyUnits(f).filter(x => game.board.open(x._1)))((t, g) => EndlessTideAction(f, t.anchor, g, then))
                .add(MayDrawAction(f, then).as("Remove no unit")("Endless Tide".hl))

        case KnowledgeEffect =>
            Then(KnowledgeAction(f, tideTerritories./(_.anchor), $, then))

        case PoachingEffect =>
            Then(DrawTempAction(f, 2, PoachingAction(f, then)))

        case WarcraftEffect =>
            game.explored = None
            Then(ExploreAction(f, 1, 1, false, ExploreEffect(), WarcraftAfterAction(f, then)))

        case CityBuilderEffect =>
            Then(BuildAction(f, BuildEffect(), 1, false, ResolveEffectAction(f, CollectEffect(Lore, 1), MayDrawAction(f, then))))

        case RatClanEffect =>
            Then(RatAskAction(f, 2, BuildAction(f, BuildEffect(), 1, false, then)))

        case OverworkEffect =>
            Ask(f).each(unitsIn(f))(t => OverworkRemoveAction(f, t.anchor, then))
                .add(MayDrawAction(f, then).as("Remove no unit")("Overwork".hl))

        case SquirrelClanEffect =>
            Then(RecruitAction(f, 1, RecruitNormal, $, SquirrelAfterAction(f, then)))

        case CookingEffect =>
            Then(BuildAction(f, BuildEffect(), 1, false, CookingAction(f, then)))

        case EconomicsEffect =>
            Then(DrawCardsAction(f, 1, EconomicsStartAction(f, then)))

        case e => HorizonDevsExpansion.resolve(f, e, then)
    }

    def perform(action : Action, soft : Void)(implicit game : Game) = action @@ {
        // SETUP: Dragon Clan's first unit on the Pyre
        case StartAction(_) =>
            if (game.setup.has(Dragon))
                game.pyre = $(Dragon)
            UnknownContinue

        // START OF YEAR: Ox's used tokens turn face up; Dragon hasn't chosen its sacrifice yet
        case StartYearAction =>
            game.gearReady ++= game.gearUsed
            game.gearUsed = $
            game.dragonHarvest = None
            UnknownContinue

        // Kraken's tokens follow its figures
        case MoveAction(_, _, _, _) | CombatsAction(_, _, _) | RetreatAction(_, _, _, _) | ReturnUnitsAction(_, _) =>
            cleanup()
            UnknownContinue

        // CLAN CARDS: Kraken's High Tide token, Ox's Ancestral Equipment, Lynx's Brundr and Kaelinn
        case PlayResolveAction(f, c : ClanCard, stage) if c.clan == f && $[Faction](Kraken, Ox, Lynx).has(f) =>
            Then(ClanCardAction(f, c, TurnAction(f, (c.flash && stage < 2).?(1).|(2))))

        case ClanCardAction(f, c, after) =>
            val resolve = ResolveEffectAction(f, c.effect, after)

            f match {
                case Kraken =>
                    cleanup()
                    if (tideTargets.any)
                        Ask(f).each(tideTargets)(t => TidePlaceAction(f, t.anchor, resolve)).add(resolve.as("Place no token")("High Tide".hl))
                    else
                        Then(resolve)

                case Ox =>
                    val l = gearOnMap(f)
                    if (l.any)
                        Ask(f).each(l)((s, n) => GearTakeAction(f, s, n, resolve))
                            .when(c.n == 0)(ResolveEffectAction(f, c.effect, ClanCardLaterAction(f, after)).as("Take a token after the card")("Ancestral Equipment".hl))
                            .add(resolve.as("Take no token")("Ancestral Equipment".hl))
                    else
                    if (c.n == 0)
                        Then(ResolveEffectAction(f, c.effect, ClanCardLaterAction(f, after)))
                    else
                        Then(resolve)

                case Lynx =>
                    if (lynxTargets.any)
                        Ask(f).add(LynxMoveAction(f, resolve).as("Move them before the card")("Brundr and Kaelinn".hl))
                            .add(ResolveEffectAction(f, c.effect, LynxLaterAction(f, after)).as("Move them after the card")("Brundr and Kaelinn".hl))
                            .add(resolve.as("Don't move them")("Brundr and Kaelinn".hl))
                    else
                        Then(ResolveEffectAction(f, c.effect, LynxLaterAction(f, after)))

                case _ =>
                    Then(resolve)
            }

        // Ox Clan card: the token taken after the card
        case ClanCardLaterAction(f, then) =>
            val l = gearOnMap(f)
            if (l.any)
                Ask(f).each(l)((s, n) => GearTakeAction(f, s, n, then)).add(then.as("Take no token")("Ancestral Equipment".hl))
            else
                Then(then)

        case LynxLaterAction(f, then) =>
            if (lynxTargets.any)
                Ask(f).add(LynxMoveAction(f, then).as("Move them")("Brundr and Kaelinn".hl)).add(then.as("Don't move them")("Brundr and Kaelinn".hl))
            else
                Then(then)

        // DRAGON
        case PyreAskAction(f, optional, then) =>
            val owners = game.pyre.distinct
            val from = (room > 0).??(deployed(f))

            if (owners.none && from.none) {
                f.log("could neither sacrifice nor place a unit on the", "Sacrificial Pyre".hl)
                Then(then)
            }
            else
                Ask(f).each(owners)(g => SacrificeAction(f, g, then)).each(from)(t => PyrePlaceAction(f, t.anchor, then))
                    .when(optional)(then.as("Do nothing")("Sacrificial Pyre".hl))

        case SacrificeAction(f, g, then) =>
            game.pyre = game.pyre.diff($(g))
            f.log("sacrificed a unit of", g, "from the", "Sacrificial Pyre".hl)
            game.note("pyre-sacrifice")
            Then(then)

        case PyrePlaceAction(f, a, then) =>
            game.removeUnits(game.board.territory(a), f, 1)
            game.pyre :+= f
            f.log("placed a unit from", a, "on the", "Sacrificial Pyre".hl)
            Then(then)

        // Harvest: Dragon must sacrifice or place a unit on the Pyre first
        case ScorchedHarvestAction if factions.has(Dragon) && game.dragonHarvest.none =>
            val owners = game.pyre.distinct
            val from = (room > 0).??(deployed(Dragon))

            if (owners.none && from.none) {
                Dragon.log("could neither sacrifice nor place a unit on the", "Sacrificial Pyre".hl, "and won't harvest")
                game.dragonHarvest = |(false)
                Then(ScorchedHarvestAction)
            }
            else
                Ask(Dragon).each(owners)(g => SacrificeAction(Dragon, g, DragonHarvestAction(Dragon, true))).each(from)(t => PyrePlaceAction(Dragon, t.anchor, DragonHarvestAction(Dragon, true)))
                    .add(DragonHarvestAction(Dragon, false).as("Don't harvest this year")("Sacrificial Pyre".hl))

        case DragonHarvestAction(f, ok) =>
            game.dragonHarvest = |(ok)
            if (ok.not)
                f.log("won't harvest this year")
            Then(ScorchedHarvestAction)

        // After harvesting: Dragon's extra food or wood, Squirrel's extra food or wood
        case AfterHarvestAction(then) =>
            Then(AfterHarvestNBAction(game.from(game.first).%(f => f == Dragon || f == Squirrel), then))

        case AfterHarvestNBAction(Nil, then) =>
            Then(then)

        case AfterHarvestNBAction(f :: rest, then) =>
            val next = AfterHarvestNBAction(rest, then)
            val l =
                if (f == Dragon) (game.dragonHarvest.has(true) && game.controlled(f).any).??($(Food, Wood))
                else Resource.all.take(2).%(r => f.has(r) > 0)

            if (l.none)
                Then(next)
            else
                Ask(f).each(l)(r => HarvestExtraAction(f, r, next))

        case HarvestExtraAction(f, r, then) =>
            f.gain(r, 1)
            f.log("collected", 1.hl, r, (f == Dragon).?("for the sacrifice").|("with the clan power"))
            Then(then)

        case SacrificeCaptureAction(f, a, g, then) =>
            game.removeUnits(game.board.territory(a), g, 1)

            if (room > 0) {
                game.pyre :+= g
                f.log("put a unit of", g, "from", a, "on the", "Sacrificial Pyre".hl)
            }
            else
                f.log("removed a unit of", g, "in", a, "(the", "Sacrificial Pyre".hl, "is full)")

            val l = game.pyre.has(f).??(game.controlled(f).%(t => game.bearIn(t).not && game.hostileIn(t).not))

            if (l.none)
                Then(then)
            else
                Ask(f).each(l)(t => PyreReturnAction(f, t.anchor, then)).add(then.as("Leave it there")("Capture for Sacrifice".hl))

        case PyreReturnAction(f, a, then) =>
            game.pyre = game.pyre.diff($(f))
            game.addUnits(a, f, 1)
            f.log("returned a unit from the", "Sacrificial Pyre".hl, "to", a)
            Then(then)

        case WorkforceAction(f, left, then) =>
            val owners = game.pyre.distinct

            if (left <= 0 || owners.none || CommonExpansion.available(f) == 0)
                Then(then)
            else
                Ask(f).each(owners)(g => SacrificeAction(f, g, DrawCardsAction(f, 1, WorkforceAction(f, left - 1, then))))
                    .add(then.as("Sacrifice no more")("Reluctant Workforce".hl))

        // HORSE
        case HorseClosedAction(f, Nil, then) =>
            Then(then)

        case HorseClosedAction(f, a :: rest, then) =>
            val t = game.board.territory(a)
            val next = HorseClosedAction(f, rest, then)
            val builds = game.controlled(f).has(t).??(MapExpansion.buildOptions(f, BuildEffect(), true).%(x => game.board.territory(x._1) == t))

            Ask(f).add(HorseWoodAction(f, a, rest, then))
                .each(builds) { case (a, b, s, cost) => MapExpansion.buildChoice(f, a, b, s, cost, 1, BuildEffect(), true, next) }

        case HorseWoodAction(f, a, rest, then) =>
            f.wood += 1
            f.log("collected", 1.hl, Wood, "for closing", a)
            Then(HorseClosedAction(f, rest, then))

        case CraftsmenAction(f, then) =>
            val l = MapExpansion.replacements(f)

            if (l.none)
                Then(then)
            else
                Ask(f).each(l) { case (s, b) => CraftsmenReplaceAction(f, s.area, s, b, then) }.add(then.as("Keep the buildings")("Craftsmen".hl))

        case CraftsmenReplaceAction(f, a, s, b, then) =>
            val old = game.buildings(s)
            game.buildings += s -> b
            f.log("replaced", old, "with", b, "in", a)
            Then(then)

        case QualityBuildAction(f, a, then) =>
            val t = game.board.territory(a)
            val s = SpaceRef(t.anchor, SpaceRef.extra + game.buildings.keys.count(s => s.area == t.anchor && s.index >= SpaceRef.extra))
            game.buildings += s -> DefenseTower
            f.log("built", DefenseTower, "in", a, "with", "Quality of Life".hl)
            Then(QualityCollectAction(f, game.controlled(f).%(t => game.buildingsIn(t).exists(_._2 == DefenseTower))./(_.anchor), then))

        case QualityCollectAction(f, Nil, then) =>
            Then(then)

        case QualityCollectAction(f, a :: rest, then) =>
            val l = resourcesIn(game.board.territory(a))

            if (l.none)
                Then(QualityCollectAction(f, rest, then))
            else
                Ask(f).each(l)(r => QualityTakeAction(f, a, r, rest, then))

        case QualityTakeAction(f, a, r, rest, then) =>
            f.gain(r, 1)
            f.log("collected", 1.hl, r, "from", a)
            Then(QualityCollectAction(f, rest, then))

        // Eitria and Brok's Precision: before the Move, any 1 resource for +1 point and Move 3
        case MoveStartAction(f, e, then) if e.special == PrecisionMove && e.n == 2 && f.resources > 0 =>
            Ask(f).each(Resource.all.%(f.has(_) > 0))(r => PrecisionPayAction(f, r, e, then))
                .add(MoveStartAction(f, e.copy(special = PlainMove), then).as("Pay nothing")("Eitria and Brok's Precision".hl))

        case PrecisionPayAction(f, r, e, then) =>
            f.gain(r, -1)
            f.log("paid", 1.hl, r, "for", "Eitria and Brok's Precision".hl)
            Then(MoveStartAction(f, e.copy(n = 3, bonus = e.bonus + 1), then))

        // KRAKEN
        case TidePlaceAction(f, a, then) =>
            game.tides :+= a
            f.log("placed a", "High Tide".hl, "token in", a)
            game.note("tide-place")
            Then(then)

        // Kàra defending, before the fight: a High Tide token may come to her territory
        case ChiefStepOneAction(Kraken :: Nil, a, then) if game.chiefIn(game.board.territory(a), Kraken) && game.tides.any && game.tideIn(game.board.territory(a)).not =>
            Ask(Kraken).each(game.tides)(o => KaraTideAction(Kraken, o, a, then)).add(then.as("Leave the tokens where they are")(WarchiefElem(Kraken)))

        case KaraTideAction(f, from, a, then) =>
            game.tides = game.tides.diff($(from)) :+ a
            f.log("moved a", "High Tide".hl, "token to", a, "with", WarchiefElem(f))
            Then(then)

        // Andhrimnir defending: 1 food before step 1
        case ChiefStepOneAction(Squirrel :: Nil, a, then) if game.chiefIn(game.board.territory(a), Squirrel) =>
            Squirrel.food += 1
            Squirrel.log("collected", 1.hl, Food, "with", WarchiefElem(Squirrel))
            UnknownContinue

        case KrakenCollectAction(f, r, then) =>
            f.gain(r, 1)
            f.log("collected", 1.hl, r)
            Then(MayDrawAction(f, then))

        case EndlessTideAction(f, a, g, then) =>
            game.removeUnits(game.board.territory(a), g, 1)
            f.lore += 1
            f.log("removed a unit of", g, "in", a, "and collected", 1.hl, Lore)
            Then(MayDrawAction(f, then))

        case KnowledgeAction(f, l, used, then) =>
            l match {
                case Nil => Then(then)
                case a :: rest =>
                    val recruit = (used.has(0).not && game.reserve(f) > 0 && game.controlled(f).exists(_.areas.contains(a))).$(KnowledgeChoiceAction(f, a, 0, None, rest, used :+ 0, then))
                    val collect = used.has(1).not.??(Resource.all./(r => KnowledgeChoiceAction(f, a, 1, |(r), rest, used :+ 1, then)))
                    val draw = (used.has(2).not && CommonExpansion.available(f) > 0).$(KnowledgeChoiceAction(f, a, 2, None, rest, used :+ 2, then))
                    val all = recruit ++ collect ++ draw

                    if (all.none)
                        Then(then)
                    else
                        Ask(f).add(all)
            }

        case KnowledgeChoiceAction(f, a, option, r, rest, used, then) =>
            option match {
                case 0 =>
                    game.addUnits(a, f, 1)
                    f.log("recruited in", a)
                    Then(KnowledgeAction(f, rest, used, then))
                case 1 =>
                    f.gain(r.get, 1)
                    f.log("collected", 1.hl, r.get)
                    Then(KnowledgeAction(f, rest, used, then))
                case _ =>
                    f.log("drew a card")
                    Then(DrawCardsAction(f, 1, KnowledgeAction(f, rest, used, then)))
            }

        // LYNX
        case LynxMoveAction(f, then) =>
            Ask(f).each(lynxTargets)(o => LynxToAction(f, o.anchor, then)).add(then.as("Don't move them")("Brundr and Kaelinn".hl))

        case LynxToAction(f, to, then) =>
            val n = game.lynx./(a => game.count(game.board.territory(a), f)).|(0)
            Ask(f).each(n.to(0, -1).$)(k => LynxUnitsAction(f, to, k, then)).cancel

        case LynxUnitsAction(f, to, n, then) =>
            val src = game.board.territory(game.lynx.get)
            val dst = game.board.territory(to)
            val chief = false

            game.removeUnits(src, f, n)
            game.addUnits(dst.anchor, f, n)
            game.lynx = |(dst.anchor)

            f.log("moved", Party(f, n, true, chief), "from", src.anchor, "to", to)
            game.note("lynx-move")

            Then(then)

        case PoachingAction(f, then) =>
            val flash = f.drawn.%(_.flash)
            f.hand ++= f.drawn
            f.drawn = $

            if (flash.any) {
                f.log("drew", flash./(_.elem).join(", "), "and drew one more card")
                Then(DrawCardsAction(f, 1, then))
            }
            else {
                f.log("drew", 2.cards)
                Then(then)
            }

        // OX
        case GearTakeAction(f, s, n, then) =>
            game.gear -= s
            game.gearReady :+= n
            f.log("took", GearName(n), "from", s.area)
            game.note("gear-take")
            Then(then)

        case GearAskAction(attacker, defender, a, e, then) =>
            val t = game.board.territory(a)
            val torfin = game.chiefIn(t, Ox)
            val limit = 1 + torfin.??(1)

            // The True Hero: Torfin's second token may be given up to remove a building there
            val hero = attacker == Ox && e.special == TrueHeroMove && torfin && game.buildingsIn(t).any

            if ((attacker == Ox || defender == Ox) && (game.gearReady.any || hero))
                Then(GearStepAction(Ox, a, limit, hero, then))
            else
                Then(then)

        case GearUseAction(f, n, a, limit, hero, then) =>
            game.gearReady = game.gearReady.diff($(n))
            game.gearUsed :+= n
            game.gearFight :+= n
            f.log("used", GearName(n))
            game.note("gear-use")

            Then(GearStepAction(f, a, limit, hero, then))

        case GearStepAction(f, a, limit, hero, then) =>
            val t = game.board.territory(a)

            val heroic = hero && limit == 2 && game.gearFight.none

            if (game.gearFight.num >= limit || game.gearReady.none && heroic.not)
                Then(then)
            else
                Ask(f).each(game.gearReady.sorted)(k => GearUseAction(f, k, a, limit, hero, then))
                    .some(heroic.??(game.buildingsIn(t)))((s, b) => $(TrueHeroAction(f, s, b, GearStepAction(f, a, 1, false, then))))
                    .add(then.as("Use no more tokens")("Ancestral Equipment".hl))

        case TrueHeroAction(f, s, b, then) =>
            game.buildings -= s
            f.log("removed", b, "from", s.area, "with", "The True Hero".hl)
            Then(then)

        case GearRerollAction(f, again) =>
            f.log("rerolled the die with", GearName(3))
            Then(again)

        case GearKeepAction(f, keep) =>
            Then(keep)

        case WarcraftAfterAction(f, then) =>
            if (game.explored.exists(tileHasLore)) {
                f.lore += 1
                f.log("collected", 1.hl, Lore, "with", "Warcraft".hl)
                Then(then)
            }
            else
            if (game.explored.any && MapExpansion.playable(f, ExploreEffect())) {
                f.log("explores again with", "Warcraft".hl)
                game.explored = None
                Then(ExploreAction(f, 1, 1, false, ExploreEffect(), then))
            }
            else
                Then(then)

        // RAT
        case RatAskAction(f, left, then) =>
            val l = (left > 0).??(unitsIn(f))

            if (l.none)
                Then(then)
            else
                Ask(f).each(l)(t => RatRemoveAction(f, t.anchor, left, then)).add(then.as("Remove no more units")("Rat Clan".hl))

        case RatRemoveAction(f, a, left, then) =>
            game.removeUnits(game.board.territory(a), f, 1)
            f.wood += 1
            f.log("removed a unit from", a, "for", 1.hl, Wood)
            Then(RatAskAction(f, left - 1, then))

        case OverworkRemoveAction(f, a, then) =>
            game.removeUnits(game.board.territory(a), f, 1)
            f.log("removed a unit from", a)

            val l = game.controlled(f)
            if (l.none)
                Then(MayDrawAction(f, then))
            else
                Ask(f).each(l)(t => OverworkCollectAction(f, t.anchor, then))

        case OverworkCollectAction(f, a, then) =>
            MapExpansion.collect(f, game.board.territory(a), "with " ~ "Overwork".hl)
            Then(MayDrawAction(f, then))

        // SQUIRREL
        case SquirrelAfterAction(f, then) =>
            val n = game.controlled(f)./(t => game.count(t, f)).sum / 4

            Ask(f).add(ResolveEffectAction(f, CollectEffect(Food, 1), then).as("Collect", 1.hl, Food)("Squirrel Clan".hl))
                .when(n > 0)(SquirrelFameAction(f, n, then))

        case SquirrelFameAction(f, n, then) =>
            f.fame += n
            f.log("gained", n.hl, FameIcon())
            Then(then)

        case CookingAction(f, then) =>
            val l = MapExpansion.buildOptions(f, BuildEffect(discount = 1, duplicate = true), true).%(_._2 == FoodSilo)

            if (l.none)
                Then(then)
            else
                Ask(f).each(l) { case (a, b, s, cost) => MapExpansion.buildChoice(f, a, b, s, cost, 1, BuildEffect(discount = 1, duplicate = true), true, then) }
                    .add(then.as("Build no Food Silo")("Cooking Mastery".hl))

        case EconomicsStartAction(f, then) =>
            val max = math.min(2, f.food)

            if (max == 0 || CommonExpansion.available(f) == 0)
                Then(then)
            else
                Ask(f).each(0.to(max).$)(k => EconomicsPayAction(f, k, then))

        case EconomicsPayAction(f, n, then) =>
            if (n > 0) {
                f.food -= n
                f.log("paid", n.hl, Food, "to draw", n.cards)
            }
            Then(DrawCardsAction(f, n, then))

        case _ => UnknownContinue
    }
}

case class ClanCardLaterAction(f : Faction, then : ForcedAction) extends ForcedAction
case class LynxLaterAction(f : Faction, then : ForcedAction) extends ForcedAction
case class EconomicsStartAction(f : Faction, then : ForcedAction) extends ForcedAction
case class GearStepAction(f : Faction, area : AreaRef, limit : Int, hero : Boolean, then : ForcedAction) extends ForcedAction
