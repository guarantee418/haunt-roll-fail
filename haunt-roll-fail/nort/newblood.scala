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
// Knowledge from Beyond: per High Tide territory, one of Recruit 1, Collect 1, Draw 1 (each once without KnowledgeEachTerritory)
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

// New Blood as printed (PrintedNewBlood, CardVersions in editions.scala)
// Enslavement: 1 or 2 enemy units from different territories next to f's onto the Pyre; with it full, first may sacrifice 1 for 1 food
case object EnslavementEffect extends Effect
// Reluctant Forces: draw 3, sacrifice 1 unit from the Pyre per card kept, the others back on the draw or the discard pile
case object ReluctantForcesEffect extends Effect
// Endless Tide: Recruit 1 in a neutral open territory, or replace an enemy unit in an enemy's open territory with one of f's
case object EndlessTidePrintEffect extends Effect
// Nimble Paws: each lynx may pull 1 enemy unit from a territory next to it (across Rough borders too); the fights follow
case object NimblePawsEffect extends Effect
// Ancient Battlegrounds: Explore; 1 lore if the tile shows lore, or else another Explore
case object BattlegroundsEffect extends Effect
// City Builder: Build; a small building may draw 1, a large one collects 1 lore
case object CityBuilderPrintEffect extends Effect
// Cooking Mastery: Build, then may build a free Food Silo needing no space in a territory f controls
case object CookingPrintEffect extends Effect
// Economics: draw 1; 1 food for a second card, then 2 more for a third
case object EconomicsPrintEffect extends Effect


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
case class CraftsmenReplaceAction(self : Faction, area : AreaRef, space : SpaceRef, building : Building, then : ForcedAction) extends BaseAction("Craftsmen".hl, "replace", OldBuilding(space), "in", area, "with")(building, SupplyLeft(building)) with MapTarget { def target = space }
case class QualityBuildAction(self : Faction, area : AreaRef, then : ForcedAction) extends BaseAction("Quality of Life".hl, "build a free", DefenseTower, "in")(area) with MapTarget { def target = area }
case class QualityCollectAction(f : Faction, l : $[AreaRef], then : ForcedAction) extends ForcedAction
case class QualityTakeAction(self : Faction, area : AreaRef, r : Resource, rest : $[AreaRef], then : ForcedAction) extends BaseAction("Quality of Life".hl, "collect from", area)(r)
case class PrecisionPayAction(self : Faction, r : Resource, e : MoveEffect, then : ForcedAction) extends BaseAction("Eitria and Brok's Precision".hl, Amount("+1".hl, CombatIcon.axe) ~ " and Move 3")("Pay", 1.hl, r)

// KRAKEN
case class TidePlaceAction(self : Faction, area : AreaRef, then : ForcedAction) extends BaseAction("High Tide".hl, "place a token in")(area) with MapTarget { def target = area }
case class TideMoveAction(self : Faction, from : AreaRef, area : AreaRef, then : ForcedAction) extends BaseAction("High Tide".hl, "move a token from", from, "to")(area) with MapTarget { def target = area }
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
// The token itself is the target (its space), so two tokens in one territory are told apart on the map
case class GearTakeAction(self : Faction, space : SpaceRef, n : Int, then : ForcedAction) extends BaseAction("Ancestral Equipment".hl, "take a token from")(space.area, "(" ~ GearName(n).elem ~ ")") with MapTarget { def target = space }
// After an Explore (OxPicksSpace): the space of the tile the next token goes on
case class GearPlaceAskAction(f : Faction, tile : String, spot : Spot, then : ForcedAction) extends ForcedAction
case class GearPlaceAction(self : Faction, space : SpaceRef, n : Int, then : ForcedAction) extends BaseAction("Ancestral Equipment".hl, "place", GearName(n).elem, "on")((g : Game) => SpaceName(space)(g).txt, "in", space.area) with MapTarget { def target = space }
case class GearAskAction(attacker : Faction, defender : Faction, area : AreaRef, e : MoveEffect, then : ForcedAction) extends ForcedAction
case class GearCreatureAskAction(f : Faction, area : AreaRef, e : MoveEffect, attacking : Boolean, then : ForcedAction) extends ForcedAction
case class GearUseAction(self : Faction, n : Int, area : AreaRef, limit : Int, hero : Boolean, then : ForcedAction) extends BaseAction("Ancestral Equipment".hl, "use a token in the fight in", area)(GearName(n))
case class TrueHeroAction(self : Faction, space : SpaceRef, building : Building, then : ForcedAction) extends BaseAction("The True Hero".hl, "give up Torfin's second token to remove a building in", space.area)(building) with MapTarget { def target = space }
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

// PRINTED NEW BLOOD (PrintedNewBlood)
// Dragon
case class EnslaveFoodAction(self : Faction, owner : Faction, then : ForcedAction) extends BaseAction("Enslavement".hl, "the", "Sacrificial Pyre".hl, "is full")("Sacrifice a unit of", owner, "to collect", 1.hl, Food)
case class EnslaveAction(f : Faction, from : $[AreaRef], then : ForcedAction) extends ForcedAction
case class EnslaveTakeAction(self : Faction, area : AreaRef, enemy : Faction, from : $[AreaRef], then : ForcedAction) extends BaseAction("Enslavement".hl, "put on the", "Sacrificial Pyre".hl, "a unit from")(area, InParens(enemy)) with MapTarget { def target = area }
case class ForcesAction(f : Faction, then : ForcedAction) extends ForcedAction
case class ForcesKeepAction(self : Faction, c : Card, owner : Faction, then : ForcedAction) extends BaseAction("Reluctant Forces".hl, "sacrifice a unit of", owner, "to keep")(c)
case class ForcesReturnAction(self : Faction, discard : Boolean, then : ForcedAction) extends BaseAction("Reluctant Forces".hl, "the other cards")(discard.?("On the discard pile").|("On top of the draw pile"))
case class SurtrAskAction(f : Faction, then : ForcedAction) extends ForcedAction
case class SurtrSacrificeAction(self : Faction, owner : Faction, then : ForcedAction) extends BaseAction("Surtr's Wrath".hl, "before the fight")("Sacrifice a unit of", owner, "for", Amount(1.hl, CombatIcon.axe))

// Kraken
case class TidePrintAction(self : Faction, area : AreaRef, then : ForcedAction) extends BaseAction("Endless Tide".hl, "recruit 1 in the neutral territory")(area) with MapTarget { def target = area }
case class TideReplaceAction(self : Faction, area : AreaRef, enemy : Faction, then : ForcedAction) extends BaseAction("Endless Tide".hl, "replace a unit with one of yours in")(area, InParens(enemy)) with MapTarget { def target = area }
case class TideLaterAction(f : Faction, then : ForcedAction) extends ForcedAction
case class KaraPlaceAction(self : Faction, area : AreaRef, then : ForcedAction) extends BaseAction(WarchiefElem(Kraken), "place a", "High Tide".hl, "token in her territory")(area)

// Lynx
case class LynxGoAction(self : Faction, from : AreaRef, to : AreaRef, n : Int, units : Int, then : ForcedAction) extends BaseAction("Brundr and Kaelinn".hl, "bonus Move from", from, "to", to)(LynxParty(n, units)) with MapTarget { def target = to }
case class LynxParty(n : Int, units : Int) extends Elementary {
    def elem = ((n == 2).?("both lynxes").|("one lynx") + (units > 0).??(" and " + units + (units == 1).?(" unit").|(" units"))).txt
}
case class MoveOneLynxAction(self : Faction, from : AreaRef, to : AreaRef, n : Int, chief : Boolean, cost : Int, left : Int, e : MoveEffect, then : ForcedAction) extends BaseAction("Move from", from, "to", to)(MoveOneLynxLabel(n, chief))
case class MoveOneLynxLabel(n : Int, chief : Boolean) extends Elementary {
    def elem = ($("one lynx".txt) ++ (n > 0).$((n == 1).?("1 unit").|(n.toString + " units").txt) ++ chief.$("the warchief".txt)).join(" and ")
}
case class NimbleAction(f : Faction, used : $[AreaRef], then : ForcedAction) extends ForcedAction
case class NimblePullAction(self : Faction, lynx : AreaRef, from : AreaRef, enemy : Faction, used : $[AreaRef], then : ForcedAction) extends BaseAction("Nimble Paws".hl, "pull a unit to", lynx, "from")(from, InParens(enemy)) with MapTarget { def target = from }
case class MielikkiLynxAction(self : Faction, from : AreaRef, area : AreaRef, then : ForcedAction) extends BaseAction(WarchiefElem(Lynx), "bring a lynx to her territory from")(from) with MapTarget { def target = from }

// Ox
case class BattlegroundsAfterAction(f : Faction, then : ForcedAction) extends ForcedAction
case class BattlegroundsLoreAction(self : Faction, then : ForcedAction) extends BaseAction("Ancient Battlegrounds".hl)("Collect", 1.hl, Lore)
case class BattlegroundsExploreAction(self : Faction, then : ForcedAction) extends BaseAction("Ancient Battlegrounds".hl)("Explore again")
case class CityBuilderAfterAction(f : Faction, then : ForcedAction) extends ForcedAction

// Squirrel
case class CookingPrintAction(f : Faction, then : ForcedAction) extends ForcedAction
case class CookingSiloAction(self : Faction, area : AreaRef, then : ForcedAction) extends BaseAction("Cooking Mastery".hl, "build a free", FoodSilo, "in")(area) with MapTarget { def target = area }
case class EconomicsMoreAction(self : Faction, n : Int, cost : Int, then : ForcedAction) extends BaseAction("Economics".hl)("Pay", cost.hl, Food, "to draw a", (n == 2).?("second").|("third"), "card")
case class AfterWinterAction(then : ForcedAction) extends ForcedAction
case class WinterExtraAction(self : Faction, r : Resource, then : ForcedAction) extends BaseAction(self, "after paying Winter")("Collect", 1.hl, r)
case class EconomicsAskAction(f : Faction, n : Int, then : ForcedAction) extends ForcedAction
case class LynxAfterAction(f : Faction, caring : Boolean, then : ForcedAction) extends ForcedAction
case class LynxBonusAction(f : Faction, units : $[AreaRef], then : ForcedAction) extends ForcedAction
case class LynxBonusFromAction(self : Faction, from : AreaRef, units : $[AreaRef], then : ForcedAction) extends BaseAction("Brundr and Kaelinn".hl, "bonus Move from")(from) with Soft with MapTarget { def target = from }


object NewBloodExpansion extends Expansion {
    // A unit on the Pyre at the start (Dragon Clan's setup)
    val pyreSize = 2

    def room(implicit game : Game) = pyreSize - game.pyre.num

    // Territories with Dragon units that can go on the Pyre
    def deployed(f : Faction)(implicit game : Game) = game.board.territories.%(t => game.count(t, f) > 0)

    def sacrificeOptions(f : Faction)(implicit game : Game) : Boolean = game.pyre.any || (room > 0 && deployed(f).any)

    // Dragon Clan's extra resource after harvesting: food or wood, from one of its territories (DragonExtraFromTerritory: only one that
    // gave it at this harvest, less what Scorched Earth took there; scorched false leaves that out, for the harvest forecast)
    def dragonExtra(f : Faction, scorched : Boolean = true)(implicit game : Game) : $[Resource] = {
        val territories = game.controlled(f)

        if (territories.none)
            $
        else if (game.options.has(DragonExtraFromTerritory).not)
            $(Food, Wood)
        else
            $(Food, Wood).%{ r =>
                territories.exists { t =>
                    val (food, wood, _) = EventsExpansion.harvest(t)
                    val took = (scorched && game.scorchedTook.has(f -> r) && game.scorchedIn(t)).??(1)
                    (r == Food).?(food).|(wood) - took > 0
                }
            }
    }

    // Kraken's High Tide tokens leave with the last Kraken figure
    def cleanup()(implicit game : Game) {
        if (game.tides.any)
            game.tides = game.tides.%(a => game.figures(game.board.territory(a), Kraken) > 0).distinctBy(a => game.board.territory(a).anchor)
    }

    def tideTerritories(implicit game : Game) = game.controlled(Kraken).%(game.tideIn)

    def tideTargets(implicit game : Game) = (game.tides.num < 2).??(game.controlled(Kraken).%(t => game.tideIn(t).not))

    // With both tokens out, a clan card may move one to another territory Kraken controls (KrakenMovesTide)
    def tideMoves(implicit game : Game) = (game.options.has(KrakenMovesTide) && game.tides.num >= 2).??(game.tides./~(o => game.controlled(Kraken).%(t => game.tideIn(t).not)./(t => (o, t.anchor))))

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
        pointParts(f, t, e, attacking, food).map(_._1).sum

    // The same, each with where it comes from, for the fight info, the log and the combat report
    def pointParts(f : Faction, t : Territory, e : MoveEffect, attacking : Boolean, food : Int)(implicit game : Game) : $[(Int, Elem)] = $(
        (attacking && f == Dragon && e.special == GrudgeMove).??(game.pyre.num) -> "Tenacious Grudge".hl,
        (attacking && f == Dragon).??(game.surtrPoints) -> "Surtr's Wrath".hl,
        gearPoints(f, food) -> "Ancestral Equipment".hl
    )

    // Ox Clan's Ancestral Equipment tokens used in the fight: their combat points (against creatures too)
    def gearPoints(f : Faction, food : Int)(implicit game : Game) : Int =
        (f == Ox).??(game.gearFight./{
            case 1 => (food > 0).??(1)
            case 2 => 1
            case 4 => 2
            case 5 => food
            case _ => 0
        }.sum)

    // Casualties from clan powers and cards
    def casualties(f : Faction, t : Territory, e : MoveEffect, attacking : Boolean)(implicit game : Game) : Int =
        casualtyParts(f, t, e, attacking).map(_._1).sum

    // The same, each with where it comes from, for the fight info and the combat report
    def casualtyParts(f : Faction, t : Territory, e : MoveEffect, attacking : Boolean)(implicit game : Game) : $[(Int, Elem)] = $(
        (attacking && e.special == FireArrowsMove).??(1) -> "Fire Arrows".hl,
        (f == Ox && game.gearFight.has(6)).??(1) -> "Ancestral Equipment".hl,
        (f == Kraken && game.tideIn(t)).??(1) -> "High Tide".hl
    )

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
                    ClanPowers.note(PowerNote(f, |(t.anchor), f.elem ~ " put " ~ k.hl ~ (k == 1).?(" unit").|(" units") ~ " of " ~ g.elem ~ " lost in the fight on the " ~ "Sacrificial Pyre".hl ~ "."))
                    game.note("pyre-capture")
                }
            }
        }

        // High Tide (as printed): the token goes when Kraken loses the fight there
        if (printed && game.tideIn(t) && winner.exists(w => w != Kraken && (attacker == Kraken || defender == Kraken))) {
            game.tides = game.tides.%(a => t.areas.contains(a).not)
            Kraken.log("lost the", "High Tide".hl, "token in", t.anchor)
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
                    attacker.log("added", k.hl, (k == 1).?("unit").|("units"), "with", ClanCard(Kraken, 3).name.hl)
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
                f.log("added a unit in", to, "with", ClanCard(Kraken, 3).name.hl)
            }
        }
    }

    // The free spaces of an explored tile Ox's next token can go on: small building spaces, and Carved Stone spaces with OxPicksSpace
    def gearSpaces(tile : String, spot : Spot)(implicit game : Game) : $[SpaceRef] = {
        val kinds = game.options.has(OxPicksSpace).?($[SpaceKind](SmallSpace, CarvedSpace)).|($[SpaceKind](SmallSpace))
        Tiles(tile).areas./~(a => a.spaces.indices.%(i => kinds.has(a.spaces(i).kind))./(i => SpaceRef(AreaRef(spot.x, spot.y, a.id), i)))
            .%(s => game.buildings.contains(s).not && game.gear.contains(s).not)
    }

    // After an Explore: Ox's token on a small building space of the tile, Rat's units in the territories it closed
    // (with OxPicksSpace, Ox picks the space afterwards, GearPlaceAskAction)
    def explored(f : Faction, tile : String, spot : Spot, closed : $[Territory])(implicit game : Game) {
        if (f == Ox && game.gearPile.any && game.options.has(OxPicksSpace).not) {
            val spaces = Tiles(tile).areas./~(a => a.spaces.indices.%(i => a.spaces(i).kind == SmallSpace)./(i => SpaceRef(AreaRef(spot.x, spot.y, a.id), i)))
                .%(s => game.buildings.contains(s).not && game.gear.contains(s).not)

            spaces.headOption.foreach { s =>
                val n = game.gearPile.head
                game.gearPile = game.gearPile.drop(1)
                game.gear += s -> n
                f.log("placed", GearName(n), "on the explored tile")
                ClanPowers.note(PowerNote(f, |(s.area), f.elem ~ " placed " ~ GearName(n).elem ~ " on the explored tile."))
                game.note("gear-place")
            }
        }

        if (f == Rat)
            closed.foreach { t =>
                if (game.reserve(f) > 0) {
                    game.addUnits(t.anchor, f, 1)
                    f.log("placed a unit in the closed territory", t.anchor)
                    ClanPowers.note(PowerNote(f, |(t.anchor), f.elem ~ " placed a unit from its reserve in the closed territory."))
                    game.note("rat-close")
                }
            }
    }

    // Step 1 of a fight Ox is in: its Ancestral Equipment tokens, one more with Torfin there
    def gearStep(a : AreaRef, e : MoveEffect, attacking : Boolean, then : ForcedAction)(implicit game : Game) : Continue = {
        val t = game.board.territory(a)
        val torfin = game.chiefIn(t, Ox)
        val limit = 1 + torfin.??(1)

        // The True Hero: Torfin's second token may be given up to remove a building there
        val hero = attacking && e.special == TrueHeroMove && torfin && game.buildingsIn(t).any

        if (game.gearReady.any || hero)
            Then(GearStepAction(Ox, a, limit, hero, then))
        else
            Then(then)
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

    // NEW BLOOD AS PRINTED (PrintedNewBlood)
    def printed(implicit game : Game) = game.options.has(PrintedNewBlood)

    // Lynx's bonus Move: the territories with lynxes, and where they can go (a Move 1, into any territory, without creatures)
    def lynxSources(implicit game : Game) : $[Territory] = game.lynxes./(game.board.territory).distinct.%(t => game.bearIn(t).not && game.hostileIn(t).not && lynxGoals(t).any)

    def lynxGoals(t : Territory)(implicit game : Game) : $[Territory] =
        game.board.adjacent(t).filter(_._2).map(_._1).%(o => game.hostileIn(o).not && game.bearIn(o).not && game.swampIn(o).not && game.creaturesIn(o).none && MapExpansion.enterable(o))
            .%(o => game.present(o).forall(g => g == Lynx || game.enemy(Lynx, g)))

    // Nimble Paws: for each lynx not used yet, an enemy unit next to it (across Rough borders too)
    def nimbleTargets(f : Faction, used : $[AreaRef])(implicit game : Game) : $[(Territory, Territory, Faction)] =
        game.lynxes./(game.board.territory).distinct.%(t => used.count(t.anchor) < game.lynxesIn(t) && game.bearIn(t).not && game.hostileIn(t).not)./~{ t =>
            game.board.adjacent(t).map(_._1).%(_ != t)./~(o => game.present(o).%(game.enemy(f, _)).%(g => game.count(o, g) > 0)./(g => (t, o, g)))
        }

    // Endless Tide: neutral open territories (no figures, no creatures), and enemies' open territories
    def tideNeutral(implicit game : Game) : $[Territory] =
        game.board.territories.%(t => game.board.open(t) && game.present(t).none && game.creaturesIn(t).none && game.swampIn(t).not)

    def tideEnemy(f : Faction)(implicit game : Game) : $[(Territory, Faction)] =
        game.board.territories.%(game.board.open)./~(t => game.present(t) match {
            case $(g) if game.enemy(f, g) && game.count(t, g) > 0 => $((t, g))
            case _ => $
        })

    // Cooking Mastery: territories f controls without a Food Silo
    def siloTargets(f : Faction)(implicit game : Game) : $[Territory] =
        (game.buildingsLeft(FoodSilo) > 0).??(game.controlled(f).%(t => game.bearIn(t).not && game.buildingsIn(t).exists(_._2 == FoodSilo).not))

    // A building on no space (Quality of Life's Defense Tower, Cooking Mastery's Food Silo)
    def spaceless(t : Territory)(implicit game : Game) = SpaceRef(t.anchor, SpaceRef.extra + game.buildings.keys.count(s => s.area == t.anchor && s.index >= SpaceRef.extra))

    // High Tide tokens: where one may be placed, and where one may be moved (with PrintedNewBlood, any token any time)
    def tideRelocations(implicit game : Game) : $[(AreaRef, AreaRef)] = game.tides./~(o => game.controlled(Kraken).%(t => game.tideIn(t).not)./(t => (o, t.anchor)))

    // Mielikki's Wisdom: 1 lore, once for the card, when a fight it started rolled a casualty
    def wisdom(f : Faction, e : MoveEffect, rolled : Boolean)(implicit game : Game) {
        if (e.special == MielikkiMove && rolled && game.wisdomTaken.not) {
            game.wisdomTaken = true
            f.lore += 1
            f.log("collected", 1.hl, Lore, "with", "Mielikki's Wisdom".hl)
        }
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
        case OverworkEffect => printed.?(game.onMap(f) > 0).|(unitsIn(f).any)
        case SquirrelClanEffect => true
        case CookingEffect => MapExpansion.playable(f, BuildEffect())
        case EconomicsEffect => CommonExpansion.available(f) > 0
        case EnslavementEffect => CardsExpansion.adjacentEnemyUnits(f).any && (room > 0 || game.pyre.any)
        case ReluctantForcesEffect => CommonExpansion.available(f) > 0
        case EndlessTidePrintEffect => game.reserve(f) > 0 && (tideNeutral.any || tideEnemy(f).any)
        case NimblePawsEffect => nimbleTargets(f, $).any
        case BattlegroundsEffect => MapExpansion.playable(f, ExploreEffect())
        case CityBuilderPrintEffect => MapExpansion.playable(f, BuildEffect())
        case CookingPrintEffect => MapExpansion.playable(f, BuildEffect())
        case EconomicsPrintEffect => CommonExpansion.available(f) > 0
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
            else {
                // KrakenFromTerritory: only what a High Tide territory gives, as the card says "from a territory"
                val l = game.options.has(KrakenFromTerritory).?($(Food, Wood).%(r => tideTerritories.exists(t => resourcesIn(t).has(r)))).|($(Food, Wood))

                if (l.none) {
                    f.log("has no", Food, "or", Wood, "in a", "High Tide".hl, "territory they control")
                    Then(MayDrawAction(f, then))
                }
                else
                    Ask(f).each(l)(r => KrakenCollectAction(f, r, then))
            }

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
            // As printed: a unit anywhere
            Ask(f).each(printed.?(game.board.territories.%(t => game.count(t, f) > 0)).|(unitsIn(f)))(t => OverworkRemoveAction(f, t.anchor, then))
                .add(MayDrawAction(f, then).as("Remove no unit")("Overwork".hl))

        case SquirrelClanEffect =>
            Then(RecruitAction(f, 1, RecruitNormal, $, SquirrelAfterAction(f, then)))

        case CookingEffect =>
            Then(BuildAction(f, BuildEffect(), 1, false, CookingAction(f, then)))

        case EconomicsEffect =>
            Then(DrawCardsAction(f, 1, EconomicsStartAction(f, then)))

        case EnslavementEffect =>
            if (room == 0 && game.pyre.any)
                Ask(f).each(game.pyre.distinct)(g => EnslaveFoodAction(f, g, EnslaveAction(f, $, then)))
                    .add(EnslaveAction(f, $, then).as("Sacrifice no unit")("Enslavement".hl))
            else
                Then(EnslaveAction(f, $, then))

        case ReluctantForcesEffect =>
            Then(DrawTempAction(f, 3, ForcesAction(f, then)))

        case EndlessTidePrintEffect =>
            val neutral = (game.reserve(f) > 0).??(tideNeutral)
            val enemy = (game.reserve(f) > 0).??(tideEnemy(f))

            if (neutral.none && enemy.none) {
                f.log("could neither recruit nor replace a unit with", "Endless Tide".hl)
                Then(then)
            }
            else
                Ask(f).each(neutral)(t => TidePrintAction(f, t.anchor, then)).each(enemy)((t, g) => TideReplaceAction(f, t.anchor, g, then))

        case NimblePawsEffect =>
            Then(NimbleAction(f, $, then))

        case BattlegroundsEffect =>
            game.explored = None
            Then(ExploreAction(f, 1, 1, false, ExploreEffect(), BattlegroundsAfterAction(f, then)))

        case CityBuilderPrintEffect =>
            game.lastBuilt = None
            Then(BuildAction(f, BuildEffect(), 1, false, CityBuilderAfterAction(f, then)))

        case CookingPrintEffect =>
            Then(BuildAction(f, BuildEffect(), 1, false, CookingPrintAction(f, then)))

        case EconomicsPrintEffect =>
            Then(DrawCardsAction(f, 1, EconomicsAskAction(f, 2, then)))

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
                // As printed: a token placed or moved before or after the card
                case Kraken if printed =>
                    cleanup()
                    val later = ResolveEffectAction(f, c.effect, TideLaterAction(f, after))

                    if (tideTargets.any || tideRelocations.any)
                        Ask(f).each(tideTargets)(t => TidePlaceAction(f, t.anchor, resolve)).each(tideRelocations)((o, a) => TideMoveAction(f, o, a, resolve))
                            .add(later.as("Place or move a token after the card")("High Tide".hl))
                            .add(resolve.as("Place no token")("High Tide".hl))
                    else
                        Then(later)

                // As printed: only the cards with the lynx icon (not Mielikki's Wisdom); the bonus Move takes only the lynxes,
                // and Lynx Clan's redraw forgoes it (so the card is played without it once they have moved)
                case Lynx if printed =>
                    game.lynxForgone = false
                    val caring = c.n == 2
                    val later = ResolveEffectAction(f, c.effect, LynxAfterAction(f, caring, after))

                    if (c.n == 3)
                        Then(resolve)
                    else
                    if (lynxSources.any)
                        Ask(f).add(LynxBonusAction(f, $, ResolveEffectAction(f, (c.n == 0).?(ExploreEffect() : Effect).|(c.effect), after)).as("Move them before the card")("Brundr and Kaelinn".hl))
                            .add(later.as("Move them after the card")("Brundr and Kaelinn".hl))
                            .add(resolve.as("Don't move them")("Brundr and Kaelinn".hl))
                    else
                        Then(later)

                case Kraken =>
                    cleanup()
                    if (tideTargets.any)
                        Ask(f).each(tideTargets)(t => TidePlaceAction(f, t.anchor, resolve)).add(resolve.as("Place no token")("High Tide".hl))
                    else
                    if (tideMoves.any)
                        Ask(f).each(tideMoves)((o, a) => TideMoveAction(f, o, a, resolve)).add(resolve.as("Move no token")("High Tide".hl))
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
            // As printed: a deployed unit goes on the Pyre only when it is empty
            val from = printed.?(game.pyre.none.??(deployed(Dragon))).|((room > 0).??(deployed(Dragon)))

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
            Then(AfterHarvestNBAction(game.from(game.first).%(f => f == Dragon || f == Squirrel && printed.not), then))

        case AfterHarvestNBAction(Nil, then) =>
            Then(then)

        case AfterHarvestNBAction(f :: rest, then) =>
            val next = AfterHarvestNBAction(rest, then)
            val l =
                if (f == Dragon) game.dragonHarvest.has(true).??(dragonExtra(f))
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
                MapExpansion.replaceAsk(f, "Craftsmen", l, then).add(then.as("Keep the buildings")("Craftsmen".hl))

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
        case ChiefStepOneAction(Kraken :: Nil, a, then) if game.chiefIn(game.board.territory(a), Kraken) && (game.tides.any || printed && game.tides.num < 2) && game.tideIn(game.board.territory(a)).not =>
            // As printed: she may also place one from the reserve
            Ask(Kraken).each(game.tides)(o => KaraTideAction(Kraken, o, a, then)).when(printed && game.tides.num < 2)(KaraPlaceAction(Kraken, a, then))
                .add(then.as("Leave the tokens where they are")(WarchiefElem(Kraken)))

        case TideMoveAction(f, from, a, then) =>
            game.tides = game.tides.diff($(from)) :+ a
            f.log("moved a", "High Tide".hl, "token from", from, "to", a)
            then

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
                case a :: rest if game.options.has(KnowledgeEachTerritory) =>
                    // The print card (Marzaus, 2026-10-20): any option for each territory, collecting only what it gives
                    val recruit = (game.reserve(f) > 0 && game.controlled(f).exists(_.areas.contains(a))).$(KnowledgeChoiceAction(f, a, 0, None, rest, used, then))
                    val collect = resourcesIn(game.board.territory(a))./(r => KnowledgeChoiceAction(f, a, 1, |(r), rest, used, then))
                    val draw = (CommonExpansion.available(f) > 0).$(KnowledgeChoiceAction(f, a, 2, None, rest, used, then))
                    val all = recruit ++ collect ++ draw

                    if (all.none)
                        Then(KnowledgeAction(f, rest, used, then))
                    else
                        Ask(f).add(all)

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
                    // A Recruit, so its Training Camps add their units, as with every Recruit card
                    if (game.options.has(KnowledgeEachTerritory))
                        Then(TrainingCampsAction(f, $(a), KnowledgeAction(f, rest, used, then)))
                    else
                        Then(KnowledgeAction(f, rest, used, then))
                case 1 =>
                    f.gain(r.get, 1)
                    if (game.options.has(KnowledgeEachTerritory))
                        f.log("collected", 1.hl, r.get, "from", a)
                    else
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

        case GearPlaceAskAction(f, tile, spot, then) =>
            val l = (f == Ox && game.gearPile.any).??(gearSpaces(tile, spot))

            if (l.none)
                Then(then)
            else
                Ask(f).each(l)(s => GearPlaceAction(f, s, game.gearPile.head, then))

        case GearPlaceAction(f, s, n, then) =>
            game.gearPile = game.gearPile.diff($(n))
            game.gear += s -> n
            f.log("placed", GearName(n), "in", s.area)
            game.note("gear-place")
            Then(then)

        case GearAskAction(attacker, defender, a, e, then) =>
            if (attacker == Ox || defender == Ox)
                gearStep(a, e, attacker == Ox, then)
            else
                Then(then)

        // Against a creature, the same tokens (a casualty more does nothing, as creatures ignore casualties)
        case GearCreatureAskAction(f, a, e, attacking, then) =>
            if (f == Ox)
                gearStep(a, e, attacking, then)
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
            MilitaryExpansion.razed(f, s, b)
            game.buildings -= s
            f.log("removed", b, "from", s.area, "with", ClanCard(Ox, 3).name.hl)
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

        // NEW BLOOD AS PRINTED (PrintedNewBlood)
        // Dragon
        case EnslaveFoodAction(f, g, then) =>
            game.pyre = game.pyre.diff($(g))
            f.food += 1
            f.log("sacrificed a unit of", g, "from the", "Sacrificial Pyre".hl, "and collected", 1.hl, Food)
            game.note("pyre-sacrifice")
            Then(then)

        case EnslaveAction(f, from, then) =>
            val l = (from.num < 2 && room > 0).??(CardsExpansion.adjacentEnemyUnits(f).%((t, _) => from.has(t.anchor).not))

            if (l.none)
                Then(then)
            else
                Ask(f).each(l)((t, g) => EnslaveTakeAction(f, t.anchor, g, from, then)).when(from.any)(then.as("Take no more")("Enslavement".hl))

        case EnslaveTakeAction(f, a, g, from, then) =>
            game.removeUnits(game.board.territory(a), g, 1)
            game.pyre :+= g
            f.log("put a unit of", g, "from", a, "on the", "Sacrificial Pyre".hl)
            Then(EnslaveAction(f, from :+ a, then))

        case ForcesAction(f, then) =>
            if (f.drawn.none)
                Then(then)
            else {
                val owners = game.pyre.distinct
                Ask(f).some(owners.any.??(f.drawn.distinct))(c => owners./(g => ForcesKeepAction(f, c, g, then)))
                    .add(ForcesReturnAction(f, false, then))
                    .add(ForcesReturnAction(f, true, then))
            }

        case ForcesKeepAction(f, c, g, then) =>
            f.drawn = f.drawn.diff($(c))
            f.hand :+= c
            game.pyre = game.pyre.diff($(g))
            f.log("sacrificed a unit of", g, "from the", "Sacrificial Pyre".hl, "to keep a card")
            game.note("pyre-sacrifice")
            Then(ForcesAction(f, then))

        case ForcesReturnAction(f, discard, then) =>
            if (discard) {
                f.discard ++= f.drawn
                f.log("discarded", f.drawn./(_.elem).join(", "))
                f.drawn = $
                Then(then)
            }
            else {
                // Cards drawn and put back on top: Limited undo can't go back past them
                game.peeked += 1
                Then(ResolveDrawnAction(f, 0, 0, f.drawn.num, then))
            }

        // Surtr's Wrath: units sacrificed from the Pyre before the fight, 1 point each
        case GearAskAction(attacker, defender, a, e, then) if attacker == Dragon && e.special == SurtrMove && game.pyre.any =>
            Then(SurtrAskAction(Dragon, GearAskAction(attacker, defender, a, e.copy(special = PlainMove), then)))

        case GearCreatureAskAction(f, a, e, attacking, then) if attacking && f == Dragon && e.special == SurtrMove && game.pyre.any =>
            Then(SurtrAskAction(Dragon, GearCreatureAskAction(f, a, e.copy(special = PlainMove), attacking, then)))

        case SurtrAskAction(f, then) =>
            if (game.pyre.none)
                Then(then)
            else
                Ask(f).each(game.pyre.distinct)(g => SurtrSacrificeAction(f, g, then)).add(then.as("Sacrifice no more")("Surtr's Wrath".hl))

        case SurtrSacrificeAction(f, g, then) =>
            game.pyre = game.pyre.diff($(g))
            game.surtrPoints += 1
            f.log("sacrificed a unit of", g, "from the", "Sacrificial Pyre".hl, "for", Amount(1.hl, CombatIcon.axe), "with", "Surtr's Wrath".hl)
            game.note("pyre-sacrifice")
            Then(SurtrAskAction(f, then))

        // Kraken
        case TidePrintAction(f, a, then) =>
            game.addUnits(a, f, 1)
            f.log("recruited in", a, "with", "Endless Tide".hl)
            Then(TrainingCampsAction(f, $(a), then))

        case TideReplaceAction(f, a, g, then) =>
            val t = game.board.territory(a)
            game.removeUnits(t, g, 1)
            game.addUnits(a, f, 1)
            f.log("replaced a unit of", g, "in", a, "with one of theirs", game.present(t).has(g).?("and attacked " ~ g.elem).|(Empty))

            if (game.present(t).has(g)) {
                game.combats :+= t.anchor
                Then(CombatsAction(f, MoveEffect(0), then))
            }
            else
                Then(then)

        case TideLaterAction(f, then) =>
            cleanup()
            if (tideTargets.any || tideRelocations.any)
                Ask(f).each(tideTargets)(t => TidePlaceAction(f, t.anchor, then)).each(tideRelocations)((o, a) => TideMoveAction(f, o, a, then))
                    .add(then.as("Place no token")("High Tide".hl))
            else
                Then(then)

        case KaraPlaceAction(f, a, then) =>
            game.tides :+= a
            f.log("placed a", "High Tide".hl, "token in", a, "with", WarchiefElem(f))
            game.note("tide-place")
            Then(then)

        // Lynx: the bonus Move, only the lynxes (and the units Caring Siblings recruited with them)
        case LynxAfterAction(f, caring, then) =>
            if (game.lynxForgone) {
                f.log("forwent the bonus Move of", "Brundr and Kaelinn".hl)
                Then(then)
            }
            else
            if (lynxSources.any)
                Ask(f).add(LynxBonusAction(f, caring.??(game.recruited), then).as("Move them")("Brundr and Kaelinn".hl)).add(then.as("Don't move them")("Brundr and Kaelinn".hl))
            else
                Then(then)

        case LynxBonusAction(f, units, then) =>
            Ask(f).each(lynxSources)(t => LynxBonusFromAction(f, t.anchor, units, then)).add(then.as("Don't move them")("Brundr and Kaelinn".hl))

        case LynxBonusFromAction(f, from, units, then) =>
            val t = game.board.territory(from)
            val n = game.lynxesIn(t)
            val u = math.min(game.count(t, f), units.count(a => t.areas.contains(a)))

            Ask(f).some(lynxGoals(t))(o => 1.to(n).$./~(k => 0.to(u).$./(j => LynxGoAction(f, from, o.anchor, k, j, then)))).cancel

        case LynxGoAction(f, from, to, n, units, then) =>
            val src = game.board.territory(from)
            val dst = game.board.territory(to)

            if (n == 2 || game.lynx2.exists(src.areas.contains).not)
                game.moveCompanion(f, src, dst.anchor)
            else
                game.lynx2 = |(dst.anchor)

            game.removeUnits(src, f, units)
            game.addUnits(dst.anchor, f, units)

            val enemy = game.present(dst).%(game.enemy(f, _))

            f.log("moved", LynxParty(n, units), "from", from, "to", to, enemy.any.?("and attacked " ~ enemy./(_.elem).join(", ")).|(Empty))
            game.note("lynx-move")

            if (enemy.any) {
                game.combats :+= dst.anchor
                Then(CombatsAction(f, MoveEffect(0), then))
            }
            else
                Then(then)

        // A regular Move taking one of the two lynxes in a territory
        case MoveOneLynxAction(f, from, to, n, chief, cost, left, e, then) =>
            val src = game.board.territory(from)
            val dst = game.board.territory(to)

            game.removeUnits(src, f, n)
            game.addUnits(dst.anchor, f, n)
            game.lynx2 = |(dst.anchor)
            if (chief)
                game.chiefs += f -> dst.anchor

            val enemy = game.present(dst).%(game.enemy(f, _))

            f.log("moved", MoveOneLynxLabel(n, chief), "from", from, "to", to, enemy.any.?("and attacked " ~ enemy./(_.elem).join(", ")).|(Empty), game.passOnly(f, dst).?("(passing through)".txt).|(Empty))

            if (game.swampIn(dst) && game.swampIn(src).not) {
                game.removeFigures(dst, f, 1)
                game.note("swamp")
                f.log("lost a unit in the", "Swamp".hl)
            }

            if (enemy.any && game.combats.has(dst.anchor).not)
                game.combats :+= dst.anchor

            Then(MoveAction(f, left - cost, e, then))

        // Lynx Clan's redraw forgoes the bonus Move
        case ExploreRedrawAction(f, _, _, _, _) if f == Lynx && printed =>
            game.lynxForgone = true
            UnknownContinue

        case NimbleAction(f, used, then) =>
            val l = nimbleTargets(f, used)

            if (l.none)
                Then(then)
            else
                Ask(f).each(l) { case (t, o, g) => NimblePullAction(f, t.anchor, o.anchor, g, used, then) }.add(then.as("Pull no more")("Nimble Paws".hl))

        case NimblePullAction(f, a, from, g, used, then) =>
            val t = game.board.territory(a)
            game.removeUnits(game.board.territory(from), g, 1)
            game.addUnits(t.anchor, g, 1)
            f.log("pulled a unit of", g, "from", from, "to", t.anchor, "with", "Nimble Paws".hl)

            val next = NimbleAction(f, used :+ t.anchor, then)

            if (game.present(t).has(f) && game.present(t).has(g)) {
                game.combats :+= t.anchor
                Then(CombatsAction(f, MoveEffect(0), next))
            }
            else
                Then(next)

        // Mielikki attacking: a lynx from anywhere to her territory
        case ChiefStepOneAction(Lynx :: rest, a, then) if rest.any && printed && game.chiefIn(game.board.territory(a), Lynx) && game.lynxes.exists(o => game.board.territory(a).areas.contains(o).not) =>
            val t = game.board.territory(a)
            val next = ChiefStepOneAction(rest, a, then)
            Ask(Lynx).each(game.lynxes.%(o => t.areas.contains(o).not).distinct)(o => MielikkiLynxAction(Lynx, o, a, next)).add(next.as("Bring no lynx")(WarchiefElem(Lynx)))

        case MielikkiLynxAction(f, from, a, then) =>
            if (game.lynx2.has(from))
                game.lynx2 = |(a)
            else
                game.lynx = |(a)
            f.log("brought a lynx from", from, "with", WarchiefElem(f))
            game.note("mielikki")
            Then(then)

        // Ox
        case BattlegroundsAfterAction(f, then) =>
            val again = game.explored.any && MapExpansion.playable(f, ExploreEffect())

            if (game.explored.exists(tileHasLore))
                Ask(f).add(BattlegroundsLoreAction(f, then)).when(again)(BattlegroundsExploreAction(f, then))
            else
            if (again)
                Ask(f).add(BattlegroundsExploreAction(f, then)).add(then.as("Explore no more")("Ancient Battlegrounds".hl))
            else
                Then(then)

        case BattlegroundsLoreAction(f, then) =>
            f.lore += 1
            f.log("collected", 1.hl, Lore, "with", "Ancient Battlegrounds".hl)
            Then(then)

        case BattlegroundsExploreAction(f, then) =>
            f.log("explores again with", "Ancient Battlegrounds".hl)
            game.explored = None
            Then(ExploreAction(f, 1, 1, false, ExploreEffect(), then))

        case CityBuilderAfterAction(f, then) =>
            game.lastBuilt match {
                case Some(b) if b.large =>
                    f.lore += 1
                    f.log("collected", 1.hl, Lore, "with", "City Builder".hl)
                    Then(then)
                case Some(_) =>
                    Then(MayDrawAction(f, then))
                case None =>
                    Then(then)
            }

        // Squirrel
        case CookingPrintAction(f, then) =>
            val l = siloTargets(f)

            if (l.none)
                Then(then)
            else
                Ask(f).each(l)(t => CookingSiloAction(f, t.anchor, then)).add(then.as("Build no Food Silo")("Cooking Mastery".hl))

        case CookingSiloAction(f, a, then) =>
            val t = game.board.territory(a)
            game.buildings += spaceless(t) -> FoodSilo
            f.log("built", FoodSilo, "in", a, "with", "Cooking Mastery".hl)
            Then(then)

        case EconomicsAskAction(f, n, then) =>
            val cost = (n == 2).?(1).|(2)

            if (n > 3 || f.food < cost || CommonExpansion.available(f) == 0)
                Then(then)
            else
                Ask(f).add(EconomicsMoreAction(f, n, cost, then)).add(then.as("Draw no more")("Economics".hl))

        case EconomicsMoreAction(f, n, cost, then) =>
            f.food -= cost
            f.log("paid", cost.hl, Food, "to draw a card")
            Then(DrawCardsAction(f, 1, EconomicsAskAction(f, n + 1, then)))

        // Squirrel Clan as printed: its extra food or wood after paying Winter
        case AfterWinterAction(then) if factions.has(Squirrel) && printed =>
            val l = Resource.all.take(2).%(r => Squirrel.has(r) > 0)

            if (l.none)
                Then(then)
            else
                Ask(Squirrel).each(l)(r => WinterExtraAction(Squirrel, r, then))

        case WinterExtraAction(f, r, then) =>
            f.gain(r, 1)
            f.log("collected", 1.hl, r, "with the clan power")
            Then(then)

        case _ => UnknownContinue
    }
}

case class ClanCardLaterAction(f : Faction, then : ForcedAction) extends ForcedAction
case class LynxLaterAction(f : Faction, then : ForcedAction) extends ForcedAction
case class EconomicsStartAction(f : Faction, then : ForcedAction) extends ForcedAction
case class GearStepAction(f : Faction, area : AreaRef, limit : Int, hero : Boolean, then : ForcedAction) extends ForcedAction
