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


// An empty spot next to the map where a tile can go
case class Spot(x : Int, y : Int) extends GameElementary with Record {
    def elem(implicit game : Game) = ("Spot " + game.board.spotLabel(x, y)).hl
}

case class TileRef(id : String) extends Elementary with Record {
    def elem = Image("tile-" + id, styles.tile)
}

// A tile being placed, shown on the map at its spot until confirmed, with rotate arrows (RotateMark), the check mark (confirm) and the cross (cancel) just outside its corners
trait TilePreview extends MapTarget {
    def tile : String
    def spot : Spot
    def r : Int
    def target = ConfirmMark
}

// The rotate arrows on a tile being placed (d = 1 clockwise, -1 counterclockwise)
case class RotateMark(d : Int)

case class RotateLabel(d : Int) extends Elementary {
    def elem = (d > 0).?("Rotate right (90°)").|("Rotate left (−90°)").txt
}

// Actions the map can be clicked for
trait MapTarget {
    def target : Any
}


// SETUP
case class ShuffledTilesAction(shuffled : $[String]) extends ShuffledAction[String]
case class SetupPlaceAction(round : Int, l : $[Faction]) extends ForcedAction
case class SetupTileAction(self : Faction, round : Int, l : $[Faction], tile : String) extends BaseAction(self, "places a tile")(TileRef(tile)) with Soft with ViewObject[TileRef] { def obj = TileRef(tile) }
case class SetupSpotAction(self : Faction, round : Int, l : $[Faction], tile : String, spot : Spot) extends BaseAction("Place the tile at")(spot) with Soft with MapTarget { def target = spot }
case class SetupRotateAction(self : Faction, round : Int, l : $[Faction], tile : String, spot : Spot, r : Int, d : Int) extends BaseAction("Place the tile at", spot)(RotateLabel(d)) with Soft with MapTarget { def target = RotateMark(d) }
case class SetupTurnAction(self : Faction, round : Int, l : $[Faction], tile : String, spot : Spot, r : Int) extends BaseAction("Place the tile at", spot)("Confirm") with TilePreview
case class SetupUnitsAction(self : Faction, round : Int, l : $[Faction], area : AreaRef) extends BaseAction("Place three units in")(area) with MapTarget { def target = area }
case class SetupKaijaAction(self : Faction, round : Int, l : $[Faction], area : AreaRef) extends BaseAction("Place two units and", Companion(self), "in")(area) with MapTarget { def target = area }
// Warchiefs module: the warchief instead of one unit (and Kaija instead of another)
case class SetupChiefAction(self : Faction, round : Int, l : $[Faction], area : AreaRef, kaija : Boolean) extends BaseAction(kaija.?("Place one unit, " ~ Companion(self).elem0 ~ " and your warchief in").|("Place two units and your warchief in"))(area) with MapTarget { def target = area }
// With Kaija or the warchief to place: tap the territory first, then pick what goes there
case class SetupAreaAction(self : Faction, round : Int, l : $[Faction], area : AreaRef) extends BaseAction("Place your starting units in")(area) with Soft with MapTarget { def target = area }
case class SetupUnitsHereAction(self : Faction, round : Int, l : $[Faction], area : AreaRef) extends BaseAction("Place in", area)("Three units")
case class SetupKaijaHereAction(self : Faction, round : Int, l : $[Faction], area : AreaRef) extends BaseAction("Place in", area)("Two units and", Companion(self))
case class SetupChiefHereAction(self : Faction, round : Int, l : $[Faction], area : AreaRef, kaija : Boolean) extends BaseAction("Place in", area)(kaija.?("One unit, " ~ Companion(self).elem0 ~ " and your warchief").|("Two units and your warchief"))
case class ShuffledTilesBackAction(shuffled : $[String]) extends ShuffledAction[String]
case class SetupUnitsAskAction(f : Faction, round : Int, l : $[Faction], tile : String, spot : Spot) extends ForcedAction

// A tile was placed (setup, exploring or a second chance); the Creatures module makes a creature appear on a lair
case class TilePlacedAction(f : Faction, tile : String, spot : Spot, setup : Boolean, then : ForcedAction) extends ForcedAction

// RECRUIT
case class RecruitAction(f : Faction, left : Int, mode : RecruitMode, placed : $[AreaRef], then : ForcedAction) extends ForcedAction
case class RecruitPlaceAction(self : Faction, area : AreaRef, left : Int, mode : RecruitMode, placed : $[AreaRef], then : ForcedAction) extends BaseAction("Recruit", (left > 1).?("(" ~ left.hl ~ " left)").|(""), "in")(area) with MapTarget { def target = area }
case class RecruitKaijaAction(self : Faction, area : AreaRef, left : Int, mode : RecruitMode, placed : $[AreaRef], then : ForcedAction) extends BaseAction("Recruit", Companion(self), "in")(area)
case class RecruitChiefAction(self : Faction, area : AreaRef, left : Int, mode : RecruitMode, placed : $[AreaRef], then : ForcedAction) extends BaseAction("Recruit", WarchiefElem(self), "in")(area)
case class RecruitDoneAction(self : Faction, placed : $[AreaRef], then : ForcedAction) extends BaseAction("Recruit")("Done")
// With Kaija or the warchief ready: tap the territory first, then pick what to recruit there
case class RecruitAreaAction(self : Faction, area : AreaRef, left : Int, mode : RecruitMode, placed : $[AreaRef], then : ForcedAction) extends BaseAction("Recruit", (left > 1).?("(" ~ left.hl ~ " left)").|(""), "in")(area) with Soft with MapTarget { def target = area }
case class RecruitUnitHereAction(self : Faction, area : AreaRef, left : Int, mode : RecruitMode, placed : $[AreaRef], then : ForcedAction) extends BaseAction("Recruit in", area)("A regular unit")
case class RecruitKaijaHereAction(self : Faction, area : AreaRef, left : Int, mode : RecruitMode, placed : $[AreaRef], then : ForcedAction) extends BaseAction("Recruit in", area)(Companion(self))
case class RecruitChiefHereAction(self : Faction, area : AreaRef, left : Int, mode : RecruitMode, placed : $[AreaRef], then : ForcedAction) extends BaseAction("Recruit in", area)(WarchiefElem(self))
case class TrainingCampsAction(f : Faction, placed : $[AreaRef], then : ForcedAction) extends ForcedAction

// MOVE
case class MoveAction(f : Faction, left : Int, e : MoveEffect, then : ForcedAction) extends ForcedAction
case class MoveFromAction(self : Faction, from : AreaRef, left : Int, e : MoveEffect, then : ForcedAction) extends BaseAction("Move", "(" ~ left.hl ~ " left)", "from")(from) with Soft with MapTarget { def target = from }
case class MoveToAction(self : Faction, from : AreaRef, to : AreaRef, cost : Int, left : Int, e : MoveEffect, then : ForcedAction) extends BaseAction("Move from", from, "to")(to, (g : Game) => MoveCostLabel(from, to, cost)(g)) with Soft with MapTarget { def target = to }
// chief: the warchief moves too (Warchiefs module); saved games from before it have no chief
case class MoveUnitsAction(self : Faction, from : AreaRef, to : AreaRef, n : Int, kaija : Boolean, chief : Boolean, cost : Int, left : Int, e : MoveEffect, then : ForcedAction) extends BaseAction("Move from", from, "to", to)(Party(self, n, kaija, chief)) {
    def this(self : Faction, from : AreaRef, to : AreaRef, n : Int, kaija : Boolean, cost : Int, left : Int, e : MoveEffect, then : ForcedAction) = this(self, from, to, n, kaija, false, cost, left, e, then)
}
case class MoveDoneAction(self : Faction, e : MoveEffect, then : ForcedAction) extends BaseAction("Move")("Done")
// All moves are made; the Creatures module asks which creatures are attacked before the fights
case class MoveEndAction(f : Faction, e : MoveEffect, then : ForcedAction) extends ForcedAction

// WARCHIEF UPGRADE CARDS
case class MoveStartAction(f : Faction, e : MoveEffect, then : ForcedAction) extends ForcedAction
case class MovesMadeAction(f : Faction, e : MoveEffect, then : ForcedAction) extends ForcedAction
case class BorgildShieldAction(self : Faction, kaijaMoves : Boolean, e : MoveEffect, then : ForcedAction) extends BaseAction("Borgild's Shield".hl)(BorgildShieldLabel(kaijaMoves))

case class BorgildShieldLabel(kaijaMoves : Boolean) extends GameElementary {
    def elem(implicit game : Game) = kaijaMoves.?("Place " ~ "Kaija".hl ~ " in " ~ Warchief.elem(Bear) ~ "'s territory").|("Place " ~ Warchief.elem(Bear) ~ " in " ~ "Kaija".hl ~ "'s territory")
}
case class BorgildShieldSkipAction(self : Faction, e : MoveEffect, then : ForcedAction) extends BaseAction("Borgild's Shield".hl)("Leave them where they are")
case class EgilFuryAction(self : Faction, space : SpaceRef, building : Building, e : MoveEffect, then : ForcedAction) extends BaseAction("Egil's Fury".hl, "remove a building from a territory being attacked")(building, "in", space.area) with MapTarget { def target = space.area }
case class EgilFurySkipAction(self : Faction, e : MoveEffect, then : ForcedAction) extends BaseAction("Egil's Fury".hl)("Remove no building")
case class LivCunningAskAction(f : Faction, defender : Faction, area : AreaRef, e : MoveEffect, spent : $[Resource], then : ForcedAction) extends ForcedAction
case class LivCunningAction(self : Faction, defender : Faction, area : AreaRef, e : MoveEffect, r : Resource, spent : $[Resource], then : ForcedAction) extends BaseAction("Liv's Cunning".hl, "spend for the fight", spent.any.?("(" ~ spent./(_.elem).join(" ") ~ " so far)").|(Empty), Break, FightInfo(self, defender, area, e, $))("1", r)
case class LivCunningDoneAction(self : Faction, defender : Faction, area : AreaRef, e : MoveEffect, spent : $[Resource], then : ForcedAction) extends BaseAction("Liv's Cunning".hl, Break, FightInfo(self, defender, area, e, $))(spent.none.?("Spend nothing").|("Throw dice"))
case class HalvardCraftSkipAction(self : Faction, then : ForcedAction) extends BaseAction("Halvard's Craft".hl)("Build nothing")
case class SvarnMendAction(f : Faction, then : ForcedAction) extends ForcedAction
case class SvarnMendToAction(self : Faction, area : AreaRef, then : ForcedAction) extends BaseAction("Svarn's Menders".hl, "place a", CombatIcon.skull, "back in")(area) with MapTarget { def target = area }
case class SvarnMendFigureToAction(self : Faction, area : AreaRef, figure : String, then : ForcedAction) extends BaseAction("Svarn's Menders".hl, "place", SvarnMendFigure(self, figure), "back in")(area) with MapTarget { def target = area }
case class BrandRetreatPickAction(self : Faction, loser : Faction, from : AreaRef, to : AreaRef, rough : Boolean, then : ForcedAction) extends BaseAction("Brand's Bravery".hl, "choose where", loser, "retreats from", from)(to) with Soft with MapTarget { def target = to }
case class BrandRetreatToAction(self : Faction, loser : Faction, from : AreaRef, to : AreaRef, n : Int, kaija : Boolean, rough : Boolean, then : ForcedAction) extends BaseAction("Brand's Bravery".hl, loser, "retreats from", from, "to", to)(RetreatGroup(loser, from, n, kaija))
case class BrandRetreatPartyAction(self : Faction, loser : Faction, from : AreaRef, to : AreaRef, n : Int, kaija : Boolean, chief : Boolean, rough : Boolean, then : ForcedAction) extends BaseAction("Brand's Bravery".hl, loser, "retreats from", from, "to", to)(RetreatParty(loser, n, kaija, chief))
case class CombatFoodPaidAction(attacker : Faction, defender : Faction, area : AreaRef, e : MoveEffect, food : $[Int], then : ForcedAction) extends ForcedAction
case class CombatsAction(f : Faction, e : MoveEffect, then : ForcedAction) extends ForcedAction
case class FightAction(self : Faction, area : AreaRef, e : MoveEffect, then : ForcedAction) extends BaseAction("Fight in")(area) with MapTarget { def target = area }
case class FightStartAction(f : Faction, area : AreaRef, e : MoveEffect, then : ForcedAction) extends ForcedAction
// A fight (with a clan or a creature) is over: the map stops marking its territory
case class FightOverAction(then : ForcedAction) extends ForcedAction
case class IntimidateAction(self : Faction, area : AreaRef, to : AreaRef, e : MoveEffect, then : ForcedAction) extends BaseAction("Intimidate", "push a defending unit from", area, "to")(to) with MapTarget { def target = to }
case class IntimidateSkipAction(self : Faction, area : AreaRef, e : MoveEffect, then : ForcedAction) extends BaseAction("Intimidate")("Fight them all")
case class AxeAction(self : Faction, defender : Faction, area : AreaRef, e : MoveEffect, food : $[Int], face : DieFace, then : ForcedAction) extends BaseAction("Axe Throwers", Break, FightInfo(self, defender, area, e, food))(face)

// COMBAT
case class CombatFoodAction(self : Faction, attacker : Faction, defender : Faction, area : AreaRef, e : MoveEffect, food : $[Int], then : ForcedAction) extends BaseAction(self, "spends food for the fight", Break, FightInfo(attacker, defender, area, e, food.dropRight(1)))((food.last == 0).?("No food").|(Amount(food.last.hl, Food.elem)))
case class CombatRollAction(attacker : Faction, defender : Faction, area : AreaRef, e : MoveEffect, food : $[Int], faces : $[DieFace], then : ForcedAction) extends ForcedAction
case class CombatRolledAction(attacker : Faction, defender : Faction, area : AreaRef, e : MoveEffect, food : $[Int], faces : $[DieFace], random : DieFace, then : ForcedAction) extends RandomAction[DieFace]
// Liv's reroll (Warchiefs module): roll again, then go on with the face without offering another reroll
case class CombatRerollAction(attacker : Faction, defender : Faction, area : AreaRef, e : MoveEffect, food : $[Int], faces : $[DieFace], then : ForcedAction) extends ForcedAction
case class CombatRerolledAction(attacker : Faction, defender : Faction, area : AreaRef, e : MoveEffect, food : $[Int], faces : $[DieFace], random : DieFace, then : ForcedAction) extends RandomAction[DieFace]
case class CombatFaceAction(attacker : Faction, defender : Faction, area : AreaRef, e : MoveEffect, food : $[Int], faces : $[DieFace], face : DieFace, then : ForcedAction) extends ForcedAction
case class CombatFoodStartAction(attacker : Faction, defender : Faction, area : AreaRef, e : MoveEffect, then : ForcedAction) extends ForcedAction
case class CombatChooseAction(self : Faction, attacker : Faction, defender : Faction, area : AreaRef, e : MoveEffect, food : $[Int], faces : $[DieFace], choice : DieFace, then : ForcedAction) extends BaseAction(self, "rolled", DieChoice, Break, FightInfo(attacker, defender, area, e, food, faces, true))(choice)
case class CombatResolveAction(attacker : Faction, defender : Faction, area : AreaRef, e : MoveEffect, food : $[Int], faces : $[DieFace], then : ForcedAction) extends ForcedAction
// rough: the retreating units may cross Rough borders (Wolf Clan card)
// The combat report (FightReport): to each of l in turn, who taps OK, then the fight goes on (a retreat or what follows it)
case class CombatReportAction(l : $[Faction], then : ForcedAction) extends ForcedAction
case class CombatReportDoneAction(self : Faction, rest : $[Faction], then : ForcedAction) extends BaseAction(CombatReport(self))("OK")
// The clan power report (ClanPowers): to each clan with notes in turn, who taps the button, then the game goes on
case class ClanPowerReportAction(then : ForcedAction) extends ForcedAction
case class ClanPowerReportDoneAction(self : Faction, then : ForcedAction) extends BaseAction(ClanPowerReport(self))(ClanPowerCollect(self))

// Casualties chosen by their player (ChooseCasualties): which figures go, then the fight goes on (then decides it again, now
// with the pick in game.casualtyPicks)
case class CasualtyPickAction(self : Faction, area : AreaRef, k : Int, n : Int, kaija : Boolean, chief : Boolean, blainn : Boolean, then : ForcedAction) extends BaseAction(self, "takes", CombatIcon.skulls(k), "in", area)(CasualtyParty(self, n, kaija, chief, blainn))

case class CasualtyParty(f : Faction, n : Int, kaija : Boolean, chief : Boolean, blainn : Boolean) extends GameElementary {
    def elem(implicit game : Game) = "Lose " ~ (((n > 0 || kaija || chief).$(Party(f, n, kaija, chief).elem) ++ blainn.$("Jötunn Blainn".hl)) : $[Elem]).join(" and ")
}

case class RetreatAction(f : Faction, from : AreaRef, rough : Boolean, then : ForcedAction) extends ForcedAction
// Retreating: pick the destination (on the map or in the list), then who goes there (everyone, the units, one unit,
// Kaija or the warchief alone), or cancel
case class RetreatPickAction(self : Faction, from : AreaRef, to : AreaRef, rough : Boolean, then : ForcedAction) extends BaseAction("Retreat from", from, "to")(to) with Soft with MapTarget { def target = to }
// Older games: the warchief went with the first group
case class RetreatToAction(self : Faction, from : AreaRef, to : AreaRef, n : Int, kaija : Boolean, rough : Boolean, then : ForcedAction) extends BaseAction("Retreat from", from, "to", to)(RetreatGroup(self, from, n, kaija))
case class RetreatPartyAction(self : Faction, from : AreaRef, to : AreaRef, n : Int, kaija : Boolean, chief : Boolean, rough : Boolean, then : ForcedAction) extends BaseAction("Retreat from", from, "to", to)(RetreatParty(self, n, kaija, chief))
// The figures go
case class RetreatMoveAction(f : Faction, from : AreaRef, to : AreaRef, n : Int, kaija : Boolean, chief : Boolean, rough : Boolean, then : ForcedAction) extends ForcedAction

// The units retreating together in older games; the warchief went with the first group, like Kaija
case class RetreatGroup(f : Faction, from : AreaRef, n : Int, kaija : Boolean) extends GameElementary {
    def elem(implicit game : Game) = "(" ~ Party(f, n, kaija, game.chiefIn(game.board.territory(from), f)).elem ~ ")"
}

case class RetreatParty(f : Faction, n : Int, kaija : Boolean, chief : Boolean) extends GameElementary {
    def elem(implicit game : Game) = "(" ~ Party(f, n, kaija, chief).elem ~ ")"
}

// SNAKE CLAN
case class ScorchedAction(f : Faction, then : ForcedAction) extends ForcedAction
case class ScorchedPlaceAction(self : Faction, area : AreaRef, then : ForcedAction) extends BaseAction("Scorched Earth".hl, "token to")(area) with MapTarget { def target = area }
case class ScorchedSkipAction(self : Faction, then : ForcedAction) extends BaseAction("Scorched Earth".hl)("Leave the token where it is")

// EXPLORE
case class ExploreAction(f : Faction, draw : Int, times : Int, redraw : Boolean, e : ExploreEffect, then : ForcedAction) extends ForcedAction
case class ExploreDrawAction(f : Faction, draw : Int, tries : Int, times : Int, redraw : Boolean, e : ExploreEffect, then : ForcedAction) extends ForcedAction
case class ExploreChooseAction(f : Faction, times : Int, redraw : Boolean, e : ExploreEffect, then : ForcedAction) extends ForcedAction
case class ExploreTileAction(self : Faction, tile : String, times : Int, e : ExploreEffect, then : ForcedAction) extends BaseAction("Explore with")(TileRef(tile)) with Soft with ViewObject[TileRef] { def obj = TileRef(tile) }
// The drawn tile, shown above the spots so it is known before choosing where it goes
case class ExploreTileInfoAction(self : Faction, tile : String) extends BaseInfo("Exploring with")(TileRef(tile)) with ViewObject[TileRef] with OnClickInfo { def obj = TileRef(tile) ; def param = TileRef(tile) }
case class ExploreSpotAction(self : Faction, tile : String, spot : Spot, times : Int, e : ExploreEffect, then : ForcedAction) extends BaseAction("Place the tile at")(spot) with Soft with MapTarget { def target = spot }
case class ExploreRotateAction(self : Faction, tile : String, spot : Spot, r : Int, d : Int, times : Int, e : ExploreEffect, then : ForcedAction) extends BaseAction("Place the tile at", spot)(RotateLabel(d)) with Soft with MapTarget { def target = RotateMark(d) }
case class ExploreTurnAction(self : Faction, tile : String, spot : Spot, r : Int, times : Int, e : ExploreEffect, then : ForcedAction) extends BaseAction("Place the tile at", spot)("Confirm") with TilePreview
case class ExploreRedrawAction(self : Faction, tile : String, times : Int, e : ExploreEffect, then : ForcedAction) extends BaseAction("Scout Camp")("Put", TileRef(tile), "at the bottom and draw another")

// BUILD
// smallOnly: Carpentry Mastery after a large building
case class BuildAction(f : Faction, e : BuildEffect, times : Int, smallOnly : Boolean, then : ForcedAction) extends ForcedAction
case class BuildPlaceAction(self : Faction, area : AreaRef, building : Building, space : SpaceRef, cost : Int, times : Int, e : BuildEffect, smallOnly : Boolean, then : ForcedAction) extends BaseAction("Build in", area)(building, "(" ~ (cost == 0).?("free".txt).|(Amount(cost.hl, Wood.elem)) ~ ")", SupplyLeft(building)) with MapTarget { def target = area }
// A building that fits more than one kind of free space in the territory: the player picks the space
case class BuildSlotAction(self : Faction, area : AreaRef, building : Building, cost : Int, times : Int, e : BuildEffect, smallOnly : Boolean, then : ForcedAction) extends BaseAction("Build in", area)(building, "(" ~ (cost == 0).?("free".txt).|(Amount(cost.hl, Wood.elem)) ~ ")", SupplyLeft(building)) with Soft with MapTarget { def target = area }
case class BuildSpaceAction(self : Faction, area : AreaRef, building : Building, space : SpaceRef, kind : SpaceKind, cost : Int, times : Int, e : BuildEffect, smallOnly : Boolean, then : ForcedAction) extends BaseAction("Build", building, "in", area, "on")(SpaceLabel(kind)) with MapTarget { def target = space }
// Build on a card: tap a free building space, pick a building from the menu (every building with its cost),
// then confirm it, previewed on the space with a check mark and a cross above it
case class BuildSpotAction(self : Faction, space : SpaceRef, times : Int, e : BuildEffect, smallOnly : Boolean, then : ForcedAction) extends BaseAction("Build", Comma, "tap a building space")((g : Game) => SpaceName(space)(g).txt, "in", space.area) with Soft with MapTarget { def target = space }
case class BuildPickAction(self : Faction, space : SpaceRef, building : Building, cost : Int, times : Int, e : BuildEffect, smallOnly : Boolean, then : ForcedAction) extends BaseAction("Build on", (g : Game) => SpaceName(space)(g).txt, "in", space.area)(BuildingLabel(building, cost)) with Soft with MapTarget { def target = space }
case class BuildConfirmAction(self : Faction, area : AreaRef, building : Building, space : SpaceRef, cost : Int, times : Int, e : BuildEffect, smallOnly : Boolean, then : ForcedAction) extends BaseAction("Build", building, "in", area)("Confirm") with MapTarget with BuildPreview { def target = ConfirmMark }
// The building being confirmed, drawn on its space; the check mark above it confirms, the cross cancels
trait BuildPreview {
    def space : SpaceRef
    def building : Building
}
// The check mark and the cross drawn on the map to confirm or cancel a building or a tile being placed
case object ConfirmMark
case object CancelMark
case class BuildDoneAction(self : Faction, e : BuildEffect, then : ForcedAction) extends BaseAction("Build")("Done")
case class BuildFinishAction(f : Faction, e : BuildEffect, then : ForcedAction) extends ForcedAction
case class ReplaceBuildingAction(self : Faction, area : AreaRef, space : SpaceRef, building : Building, then : ForcedAction) extends BaseAction("Industrious Villagers", "replace", OldBuilding(space), "in", area, "with")(building, SupplyLeft(building)) with MapTarget { def target = space }
// Replacing one of several buildings (Industrious Villagers, Horse's Craftsmen): tap the building first, then pick what it becomes
case class ReplaceSpotAction(self : Faction, source : String, space : SpaceRef, then : ForcedAction) extends BaseAction(source.hl, Comma, "tap a building to replace")(OldBuilding(space), "in", space.area) with Soft with MapTarget { def target = space }
// The building on a space, named in a replacement's label
case class OldBuilding(s : SpaceRef) extends GameElementary {
    def elem(implicit game : Game) = game.buildings.get(s)./(_.elem).|("a building".txt)
}
case class ReplaceSkipAction(self : Faction, then : ForcedAction) extends BaseAction("Industrious Villagers")("Keep the buildings")

// FEAST
case class FeastChoiceAction(self : Faction, effect : Effect, then : ForcedAction) extends BaseAction("Feast")(FeastText(effect))

object SpaceLabel {
    def apply(k : SpaceKind) : String = k match {
        case SmallSpace => "A small building space"
        case LargeSpace => "A large building space"
        case CarvedSpace => "A Carved Stone space"
    }
}

object SpaceName {
    def apply(s : SpaceRef)(implicit game : Game) : String =
        if (s.index >= SpaceRef.extra) "No space needed" else SpaceLabel(MapExpansion.spaceKind(s))
}

case class BuildingLabel(b : Building, cost : Int) extends GameElementary {
    def elem(implicit game : Game) =
        Image(b.image, styles.buildIcon) ~ b.title.hl ~ " (" ~ (cost == 0).?("free".txt).|(Amount(cost.hl, Wood.elem)) ~ ") " ~ SupplyLeft(b).elem
}

// How many of a building are left in the supply (7 of each, less those on the map)
case class SupplyLeft(b : Building) extends GameElementary {
    def elem(implicit game : Game) = "(" ~ game.buildingsLeft(b).hl ~ " left in supply)"
}

// How many map tiles are left in the pile to explore with (after the ones drawn)
case object TilesLeft extends GameElementary {
    def elem(implicit game : Game) = "(" ~ game.pile.num.hl ~ " " ~ (game.pile.num == 1).?("tile").|("tiles") ~ " left in pile)"
}

// A Feast (or Teamwork) choice's button: Explore says how many tiles are left in the pile
case class FeastText(e : Effect) extends GameElementary {
    def elem(implicit game : Game) = e match {
        case _ : ExploreEffect => FeastLabel(e).txt ~ " " ~ TilesLeft.elem
        case _ => FeastLabel(e).txt
    }
}

object FeastLabel {
    def apply(e : Effect) : String = e match {
        case RecruitEffect(_, _) => "Recruit 1"
        case MoveEffect(_, _, _, _) => "Move 1"
        case ExploreEffect(_, _, _, _, _) => "Explore"
        case _ => "Build"
    }
}

// END OF YEAR
case class ReturnUnitsAction(f : Faction, then : ForcedAction) extends ForcedAction
case class ReturnUnitsToAction(self : Faction, area : AreaRef, then : ForcedAction) extends BaseAction(self, "has no units left; place three in")(area) with MapTarget { def target = area }
// No neutral territory left: draw a tile to make one
case class SecondChanceDrawAction(f : Faction, tries : Int, then : ForcedAction) extends ForcedAction
case class SecondChanceSpotAction(self : Faction, tile : String, spot : Spot, then : ForcedAction) extends BaseAction(self, "has no units and no neutral territory is left; place", TileRef(tile), "at")(spot) with Soft with MapTarget { def target = spot }
case class SecondChanceRotateAction(self : Faction, tile : String, spot : Spot, r : Int, d : Int, then : ForcedAction) extends BaseAction("Place the tile at", spot)(RotateLabel(d)) with Soft with MapTarget { def target = RotateMark(d) }
case class SecondChanceTurnAction(self : Faction, tile : String, spot : Spot, r : Int, then : ForcedAction) extends BaseAction("Place the tile at", spot)("Confirm") with TilePreview


// The companion figure of a clan: Kaija (Bear), Brundr and Kaelinn (Lynx), Brok (Horse's second warchief)
case class Companion(f : Faction) extends Elementary {
    def name = f match {
        case Lynx => "Brundr and Kaelinn"
        case Horse => "Brok"
        case Automa => "Leader 2"
        case _ => "Kaija"
    }
    def elem0 : Elem = name.hl
    def elem : Elem = name.hl
}

// A figure Svarn's Menders brings back (MendFigures): the warchief, the companion or Jötunn Blainn
case class SvarnMendFigure(f : Faction, figure : String) extends GameElementary {
    def elem(implicit game : Game) = figure match {
        case "chief" => WarchiefElem(f).elem
        case "companion" => Companion(f).elem
        case _ => "Jötunn Blainn".hl
    }
}

// Some units of a clan, maybe with its companion and its warchief
case class Party(f : Faction, n : Int, kaija : Boolean, chief : Boolean) extends GameElementary {
    def elem(implicit game : Game) = ((n > 0).$((n == 1).?("1 unit").|(n.toString + " units").txt) ++ kaija.$(Companion(f).elem) ++ chief.$((f == Horse && kaija).?(Warchief.elem(f)).|("the warchief".txt))).join(" and ")
}

// Where a fight is and what each side has, shown under the question of each step of the fight:
// each side's combat points before the die, with where they come from (the same sums CombatResolveAction uses)
// food: what each side has spent so far (the attacker first)
// faces: the dice already rolled (the attacker first), counted in; choosing: the next side to roll took "1 point or 1 casualty" and is choosing
case class FightInfo(attacker : Faction, defender : Faction, area : AreaRef, e : MoveEffect, food : $[Int], faces : $[DieFace] = $, choosing : Boolean = false) extends GameElementary {
    def elem(implicit game : Game) = {
        val t = game.board.territory(area)
        val here = game.working(t)
        // Conqueror: the attacker ignores Fortresses and Defense Towers
        val towers = attacker.conqueror.?(0).|(here.count(_ == DefenseTower))

        def side(f : Faction, attacking : Boolean, spent : |[Int], die : |[DieFace], choice : Boolean) : Elem = {
            val extra = attacking.?($(
                e.bonus -> "the card".txt,
                game.eventIs("conquests").??(1) -> "Conquests".hl,
                game.robotos(f).??(1) -> "Robotos".hl
            )).|($(
                attacker.conqueror.?(0).|(2 * here.count(_ == Fortress)) -> Fortress.elem
            )) ++ $(
                (f == Snake && game.scorchedIn(t)).??(1) -> "Scorched Earth".hl
            ) ++ NewBloodExpansion.pointParts(f, t, e, attacking, spent.|(0)) ++ game.has(Wastelands).??(WastelandsExpansion.pointParts(f, t, attacking)) ++ $(
                spent.|(0) -> Food.elem,
                die./(_.points).|(0) -> "the die".txt
            )

            val casualties = attacking.?($(
                (e.special == EgilMove).??(1) -> "the card".txt
            )).|($(towers -> DefenseTower.elem)) ++ NewBloodExpansion.casualtyParts(f, t, e, attacking) ++ $(die./(_.casualties).|(0) -> "the die".txt)

            FightPoints(f, t, attacking, extra, casualties, (attacking && (e.special == ShieldMove || e.special == BorgildMove)).??(1) + NewBloodExpansion.ignored(f), choice)
        }

        val chooser = choosing.?(faces.num).|(-1)

        "The fight is in ".txt ~ area.elem ~
        Break ~ side(attacker, true, food.lift(0), faces.lift(0), chooser == 0) ~
        Break ~ side(defender, false, food.lift(1), faces.lift(1), chooser == 1)
    }
}

// The same for a fight against a creature
// food, die: what the player spent and rolled so far (the reroll step), counted in
case class CreatureFightInfo(f : Faction, area : AreaRef, c : Creature, e : MoveEffect, attacking : Boolean, food : Int = 0, die : |[DieFace] = None) extends GameElementary {
    def elem(implicit game : Game) = {
        val t = game.board.territory(area)
        val waste = game.has(Wastelands)

        val extra = $(
            attacking.??(e.bonus) -> "the card".txt,
            (attacking && e.special == AxeMove).??(1) -> "Axe Throwers".hl,
            (attacking && game.robotos(f)).??(1) -> "Robotos".hl,
            (attacking && f == Dragon && e.special == GrudgeMove).??(game.pyre.num) -> "Tenacious Grudge".hl,
            attacking.not.??(2 * game.working(t).count(_ == Fortress)) -> Fortress.elem,
            (f == Snake && game.scorchedIn(t)).??(1) -> "Scorched Earth".hl,
            NewBloodExpansion.gearPoints(f, food) -> "Ancestral Equipment".hl
        ) ++ waste.??(WastelandsExpansion.playerCreatureParts(f, t, attacking)) ++ $(
            food -> Food.elem,
            // Casualties don't hurt creatures, so a player always takes the point
            die./(d => d.choice.?(1).|(d.points)).|(0) -> "the die".txt
        )

        val more = waste.??(WastelandsExpansion.creatureParts(c, t, attacking))
        val bonus = more.map(_._1).sum

        "The fight is in ".txt ~ area.elem ~ Break ~
        FightPoints(f, t, attacking, extra, $, (attacking && (e.special == ShieldMove || e.special == BorgildMove)).??(1) + NewBloodExpansion.ignored(f)) ~ Break ~
        c.elem ~ ": " ~ CombatIcon.axes(c.kind.value + bonus) ~ more.any.?(" (" ~ ($(c.kind.value -> "its strength".txt) ++ more)./{ case (n, what) => n.hl ~ " from " ~ what }.join(", ") ~ ")").|(Empty)
    }
}

// Liv's reroll (Warchiefs module) and Ancestral Equipment 3 (Ox Clan): what the die showed and where the fight stands
// with it, above "Reroll the combat die" and "Keep the result"; keep: the Keep action's next step, which holds the face
case class RerollInfoAction(self : Faction, liv : Boolean, keep : ForcedAction) extends BaseInfo((g : Game) => liv.?(Warchief.elem(Raven)(g)).|("Ancestral Equipment".hl))(RerollInfo(keep))

// (in a block of its own: the info box lays out its contents in a row, which would drop the line breaks)
case class RerollInfo(keep : ForcedAction) extends GameElementary {
    def elem(implicit game : Game) = lines.div(xlo.fullwidth)

    def lines(implicit game : Game) : Elem = keep match {
        case CombatFaceAction(attacker, defender, a, e, food, faces, face, _) =>
            // A "point or casualty" face is shown both ways, as when choosing
            val info = face.choice.?(FightInfo(attacker, defender, a, e, food, faces, true)).|(FightInfo(attacker, defender, a, e, food, faces :+ face))

            "You rolled ".txt ~ face.elem ~ Break ~ info.elem ~ faces.none.?(Break ~ defender.elem ~ " rolls after you").|(Empty)

        case CreatureFaceAction(f, a, c, e, attacking, food, face, _) =>
            "You rolled ".txt ~ face.elem ~ Break ~ CreatureFightInfo(f, a, c, e, attacking, food, |(face)).elem ~ Break ~ c.elem ~ " rolls after you"

        case _ => Empty
    }
}

// Move to a territory: the cost when more than 1 (a Rough border, or all the moves left to sail from Port to Port)
object MoveCostLabel {
    def apply(from : AreaRef, to : AreaRef, cost : Int)(implicit game : Game) : Elem =
        if (cost <= 1) Empty
        else
        if (game.board.adjacent(game.board.territory(from)).exists(_._1 == game.board.territory(to))) "(Rough border)".txt
        else "(by sea)".txt
}

// A side's combat points before the die: units, Kaija, the warchief, Jötunn Blainn, then the extras;
// sources worth nothing are left out; choosing: the side is choosing "1 point or 1 casualty", so its total with the point is added
object FightPoints {
    // The sources worth something, with what they are worth (they add up to game.strength plus the extras)
    def parts(f : Faction, t : Territory, attacking : Boolean, extra : $[(Int, Elem)])(implicit game : Game) : $[(Int, Elem)] = {
        val units = game.count(t, f)
        ($(units -> (units == 1).?("unit").|("units").txt) ++ others(f, t, attacking) ++ extra).filter(_._1 > 0)
    }

    // The figures beside the units, and the Port
    def others(f : Faction, t : Territory, attacking : Boolean)(implicit game : Game) : $[(Int, Elem)] = $(
        game.companionStrength(t, f) -> Companion(f).elem,
        Warchief.strength(t, f, attacking) -> Warchief.elem(f),
        game.blainnIn(t, f).??(2) -> "Jötunn Blainn".hl,
        SeaExpansion.defense(t, f, attacking) -> "the Port".hl
    ).filter(_._1 > 0)

    def apply(f : Faction, t : Territory, attacking : Boolean, extra : $[(Int, Elem)], casualties : $[(Int, Elem)], cancels : Int, choosing : Boolean = false)(implicit game : Game) : Elem = {
        val points = parts(f, t, attacking, extra)
        val total = points.map(_._1).sum
        val more = casualties.filter(_._1 > 0)./{ case (n, what) => Amount(("+" + n).hl, CombatIcon.skull) ~ " from " ~ what } ++
            (cancels > 0).$("cancels " ~ CombatIcon.skulls(cancels))

        f.elem ~ attacking.?(" (attacking)").|(" (defending)") ~ ": " ~ CombatIcon.axes(total) ~
        points.any.?(" (" ~ points./{ case (n, what) => n.hl ~ " from " ~ what }.join(", ") ~ ")").|(Empty) ~
        more.any.?(", " ~ more.join(", ")).|(Empty) ~
        choosing.?("; " ~ CombatIcon.axes(total + 1) ~ " if taking the " ~ CombatIcon.axe).|(Empty)
    }
}

// The combat report, shown once the die has decided a fight (CombatResolveAction, or CreatureResolveAction against a
// creature) and before any retreat: who won and how, then each side's combat points and casualties with where they
// come from, and what it lost. Asked of both sides (CombatReport.readers), never of the Automa; a player whose
// "Combat Report" setting is Skip taps OK without seeing it (the UI does, ask in ui.scala).
// f: the clan (None for a creature, named by who); points: the sources worth something; casualties: the sources of
// the casualties inflicted, a negative one cancels (its Elem then says how, "ignored by ..."); lost: figures removed;
// fate: what became of a creature ("was defeated")
case class FightSide(f : |[Faction], who : Elem, attacking : Boolean, points : $[(Int, Elem)], total : Int, casualties : $[(Int, Elem)], inflicted : Int, lost : Int, fate : Elem = Empty)

object FightSide {
    def apply(f : Faction, attacking : Boolean, points : $[(Int, Elem)], total : Int, casualties : $[(Int, Elem)], inflicted : Int, lost : Int)(implicit game : Game) : FightSide =
        FightSide(|(f), f.elem, attacking, points, total, casualties.filter(_._1 != 0), inflicted, lost)
}

// winner: true for the attacker, None when both sides were wiped out; wipe: won by killing all the loser's units
case class FightReport(area : AreaRef, attacker : FightSide, defender : FightSide, winner : |[Boolean], wipe : Boolean)

// The report as self sees it: "You" for self's side
case class CombatReport(self : Faction) extends GameElementary {
    def elem(implicit game : Game) = game.fightReport./(CombatReport.text(_, |(self))).|(Empty)
}

// Every fight's report, kept in the log: the word that says who won ("won", "defeated", "wiped out") opens it
case class FightReportView(r : FightReport)

object CombatReport {
    // Who is shown the report of a fight between these clans (or one clan and a creature): both sides but the Automa.
    // Each player's "Combat Report" setting decides whether they see it (ShowCombatReport) or it is skipped
    def readers(attacker : |[Faction], defender : |[Faction])(implicit game : Game) : $[Faction] =
        (attacker.$ ++ defender.$).but(Automa)

    // The word in the log that opens the fight's report
    def link(word : String)(implicit game : Game) : Elem =
        game.fightReport./(r => OnClick(FightReportView(r), word.spn(styles.tappable)(xlo.pointer)) : Elem).|(word.txt)

    // The report's text; self: whose side is "You" (None in the log, where clans are named)
    def text(r : FightReport, self : |[Faction])(implicit game : Game) : Elem = {
        def name(s : FightSide) : Elem = (self.any && s.f == self).?("You".txt).|(s.who)

        val headline = r.winner./(w => w.?(r.attacker).|(r.defender)) match {
            case None => "Both sides were wiped out in ".txt ~ r.area.elem ~ "."
            case Some(w) =>
                name(w) ~ " won the fight in " ~ r.area.elem ~ " " ~ (
                    if (r.wipe) "by killing all " ~ self.none.?(w.attacking.?(r.defender).|(r.attacker).who).|(w.f.has(self.get).?("enemy".txt).|("your".txt)) ~ " units " ~ CombatIcon.skull
                    else if (r.attacker.total == r.defender.total) "with as many " ~ CombatIcon.axe ~ " (the defender wins ties)"
                    else "with more " ~ CombatIcon.axe
                ) ~ "."
        }

        def side(s : FightSide) : Elem = {
            val from = s.points./{ case (n, what) => n.hl ~ " from " ~ what }
            val hits = s.casualties./{ case (n, what) => (n > 0).?(n.hl ~ " from " ~ what).|((-n).hl ~ " " ~ what) }

            name(s) ~ s.attacking.?(" (attacking)").|(" (defending)") ~ " had " ~ CombatIcon.axes(s.total) ~
            from.any.?(" (" ~ from.join(", ") ~ ")").|(Empty) ~
            hits.any.?(", inflicted " ~ CombatIcon.skulls(s.inflicted) ~ " (" ~ hits.join(", ") ~ ")").|(Empty) ~
            (s.lost > 0).?(" and lost " ~ s.lost.hl ~ (s.lost == 1).?(" unit").|(" units")).|(Empty) ~
            (s.fate != Empty).?(" and " ~ s.fate).|(Empty) ~ "."
        }

        // Your side first
        val sides = (self.any && r.defender.f == self).?($(r.defender, r.attacker)).|($(r.attacker, r.defender))

        headline ~ Break ~ side(sides(0)) ~ Break ~ side(sides(1))
    }
}

// A combat report or clan power report sent as a notification (ReportsDontWait), its text as it was then
case class ReportNote(self : Faction, combat : Boolean, body : Elem) extends BaseInfo(combat.?("Combat report").|("Clan power").hl)(body)

// The clan power report (asked for by Robotos on 2026-10-12): after an action that set off a clan's own power without
// asking anything (Raven, Stag, Boar, Goat, Wolf, Dragon's Pyre, Ox's Ancestral Equipment, Rat's units), a box tells
// its player where it went off and what it did, with a button naming what was collected ("Collect 1 Lore"). The power
// has already done its work when the box shows (the button only tells it), so the game is the same with or without it.
// Notes are kept in game.powerNotes until the next ClanPowerReportAction (after each explored tile, a Build, a fight).
// Only in games with the hidden ClanPowerReports option, never for the Automa; each player's "Clan Power Report"
// setting shows it or skips it (the UI taps the button, ask in ui.scala)
case class PowerNote(f : Faction, area : |[AreaRef], what : Elem, food : Int = 0, wood : Int = 0, lore : Int = 0, fame : Int = 0)

case class ClanPowerReport(self : Faction) extends GameElementary {
    def elem(implicit game : Game) = ClanPowers.text(self)
}

case class ClanPowerCollect(self : Faction) extends GameElementary {
    def elem(implicit game : Game) = ClanPowers.button(self)
}

object ClanPowers {
    // The report step after an action, only in games with the option: older games recorded their action chains
    // without it, and the client checks recorded actions against the chain when it loads a game
    def report(then : ForcedAction)(implicit game : Game) : ForcedAction =
        game.options.has(ClanPowerReports).?(ClanPowerReportAction(then) : ForcedAction).|(then)

    def note(n : PowerNote)(implicit game : Game) {
        if (game.options.has(ClanPowerReports) && n.f != Automa)
            game.powerNotes :+= n
    }

    def text(self : Faction)(implicit game : Game) : Elem =
        game.powerNotes.%(_.f == self)./(n => ("You activated your clan power" ~ n.area./(a => " in " ~ a.elem).|(Empty) ~ ".").div ~ n.what.div).join(Break)

    def button(self : Faction)(implicit game : Game) : Elem = {
        val l = game.powerNotes.%(_.f == self)
        val res = $(l./(_.food).sum -> Food.elem, l./(_.wood).sum -> Wood.elem, l./(_.lore).sum -> Lore.elem).filter(_._1 > 0)./{ case (n, e) => Amount(n, e) }
        val fame = l./(_.fame).sum

        if (res.any && fame > 0)
            "Collect " ~ res.join(", ") ~ " and gain " ~ Amount(fame, FameIcon())
        else
        if (res.any)
            "Collect " ~ res.join(", ")
        else
        if (fame > 0)
            "Gain " ~ Amount(fame, FameIcon())
        else
            "OK".txt
    }
}

// Some units, maybe with Kaija and the warchief
case class Figures(n : Int, kaija : Boolean, chief : Boolean = false) extends Elementary with Record {
    def this(n : Int, kaija : Boolean) = this(n, kaija, false)
    def elem : Elem = ((n > 0).$((n == 1).?("1 unit").|(n.toString + " units")) ++ kaija.$("Kaija") ++ chief.$("the warchief")).mkString(" and ").txt
}


// The Northgard die
case class DieFace(points : Int, casualties : Int, choice : Boolean) extends Elementary with Record {
    def elem : Elem =
        if (choice) CombatIcon.axes(1) ~ " or " ~ CombatIcon.skulls(1)
        else $(
            (points > 0).?(CombatIcon.axes(points)),
            (casualties > 0).?(CombatIcon.skulls(casualties))
        ).flatten.join(" and ")
}

case object DieChoice extends Elementary {
    def elem = CombatIcon.axes(1) ~ " or " ~ CombatIcon.skulls(1) ~ ", taking"
}

object NorthgardDie {
    val faces : $[DieFace] = $(DieFace(2, 0, false), DieFace(3, 0, false), DieFace(0, 0, true), DieFace(2, 1, false), DieFace(0, 2, false), DieFace(1, 1, false))

    val point = DieFace(1, 0, false)
    val casualty = DieFace(0, 1, false)
}


object MapExpansion extends Expansion {
    // The clan powers of an attacker that won a fight
    def attackWon(attacker : Faction, a : AreaRef)(implicit game : Game) {
        if (attacker == Wolf) {
            attacker.food += 1
            attacker.log("collected", 1.hl, Food, "for winning as the attacker")
            ClanPowers.note(PowerNote(attacker, |(a), attacker.elem ~ " collects 1 food for winning a fight as the attacker.", food = 1))
        }

        if (attacker == Stag) {
            attacker.fame += 1
            attacker.log("gained", 1.hl, FameIcon(), "for conquering a territory")
            ClanPowers.note(PowerNote(attacker, |(a), attacker.elem ~ " gains 1 fame for each territory it conquers.", fame = 1))
        }
    }

    def recruitTargets(f : Faction, mode : RecruitMode, placed : $[AreaRef])(implicit game : Game) : $[Territory] = {
        val neutral = game.board.territories.%(t => game.present(t).none)
        val mine = game.controlled(f)

        val none = game.anyOnMap(f).not

        val l = mode match {
            case RecruitNormal => none.?(neutral).|(mine)
            case RecruitSame => none.?(neutral).|(mine)
            case RecruitNeutral => mine ++ neutral
            // Allies from the Wild: neutral territories, or one this recruit already went into (2 in one, or 1 in each of 2)
            case RecruitNeutralOnly => neutral ++ placed./(game.board.territory).distinct.diff(neutral)
            // Raven Mercenaries: a neutral territory, then the same one, which the first unit made Raven's
            case RecruitNeutralSame => neutral ++ placed./(game.board.territory).distinct.diff(neutral)
            case RecruitSameAny => mine ++ neutral
            case RecruitOsmosis => mine.%(t => game.board.open(t) || t.areas.exists(a => game.board.spec(a).wood > 0)).%(t => placed.forall(p => t.areas.contains(p).not))
            case RecruitClosed => mine.%(game.board.closed).%(t => placed.forall(p => t.areas.contains(p).not))
        }

        val same = mode @@ {
            case RecruitSame | RecruitNeutralSame | RecruitSameAny => true
            case _ => false
        }

        // Creatures: no recruiting with a Brown Bear, and a Fallen Valkyrie shares its territory with nobody; nobody stays in the Swamp
        val r = l.%(t => game.bearIn(t).not && game.hostileIn(t).not && game.swampIn(t).not)

        if (same && placed.any)
            r.%(t => t.areas.contains(placed.head) || game.board.territory(placed.head) == t)
        else
            r
    }

    // Territories f can move out of: their own, not fighting; with Infiltration also enemy territories they moved into
    // No moving out of a Brown Bear's territory, nor out of a Fallen Valkyrie's before fighting it
    def moveSources(f : Faction, e : MoveEffect = MoveEffect(1))(implicit game : Game) =
        ((e.special == InfiltrateMove).?(game.board.territories.%(t => game.figures(t, f) > 0)).|(game.controlled(f)) ++
            // Signy's Celerity: units go on through the territory with the Scorched Earth token
            (e.special == SignyMove).??(game.board.territories.%(t => game.scorchedIn(t) && game.figures(t, f) > 0 && game.controlled(f).has(t).not)) ++
            // Units passing through a teammate's territory or the Swamp
            game.passing(f))
            .%(t => game.bearIn(t).not && game.hostileIn(t).not)

    // Two creatures that don't share their territory can't both be fought: nobody may enter
    def enterable(t : Territory)(implicit game : Game) = game.creaturesIn(t).count(_.kind.shares.not) < 2

    // f's figures (with Kaija when kaija) entering a teammate's territory or the Swamp t with rem moves left
    // must be able to move on, since they can't stop there
    def canPass(f : Faction, t : Territory, rem : Int, e : MoveEffect, kaija : Boolean)(implicit game : Game) : Boolean =
        rem > 0 && game.bearIn(t).not && game.hostileIn(t).not && game.board.adjacent(t).exists { case (o, regular) =>
            val c = moveCost(regular.not, e.ignoreRough)
            c <= rem && canEnter(f, o, rem - c, e, kaija)
        }

    // Kaija can't enter enemy territories unless awakened; a teammate's territory only to pass through
    def canEnter(f : Faction, o : Territory, rem : Int, e : MoveEffect, kaija : Boolean)(implicit game : Game) : Boolean =
        enterable(o) &&
        (kaija.not || game.restrained(f).not || game.present(o).forall(game.allied(f, _))) &&
        (game.passOnly(f, o).not || canPass(f, o, rem, e, kaija))

    // Where f's figures in t can move with left moves, and the cost; figures passing through a teammate's territory all move on together
    def destinations(f : Faction, t : Territory, left : Int, e : MoveEffect)(implicit game : Game) : $[(Territory, Int)] = {
        val kaija = game.passOnly(f, t) && game.kaijaIn(t, f)
        // Sea module: from Port to Port with all the moves left
        (game.board.adjacent(t).map { case (o, regular) => o -> moveCost(regular.not, e.ignoreRough) } ++ SeaExpansion.sailing(f, t, left).filter(x => game.board.adjacent(t).exists(_._1 == x._1).not))
            .filter { case (o, c) => c <= left && canEnter(f, o, left - c, e, kaija) }
    }

    // Tiles a setup placement may go next to: the starting tile(s) in the first round, any tile in the second
    def setupPlacements(tile : String, round : Int)(implicit game : Game) : $[(Spot, Int)] =
        placements(tile, None, true).%{ case (s, _) => round > 1 || Side.all.exists(d => centre.has((s.x + d.dx, s.y + d.dy))) }

    // The starting tiles' spots (the central tile can be the Wilderness Great Lake, whose id doesn't start with "start")
    def centre(implicit game : Game) : $[(Int, Int)] = $((0, 0)) ++ (game.arity >= 5).$((1, 0))

    def moveCost(rough : Boolean, ignoreRough : Boolean) = (rough && ignoreRough.not).?(2).|(1)

    // The legal turns of a tile at a spot
    def rotations(l : $[(Spot, Int)], spot : Spot) : $[Int] = l.filter(_._1 == spot).map(_._2).distinct.sorted

    // The next legal turn from r, clockwise (d = 1) or counterclockwise (d = -1)
    def rotate(rs : $[Int], r : Int, d : Int) : Int = 1.to(3).map(i => (r + d * i + 4) % 4).find(rs.has).|(r)

    // The tile shown at its spot, with Rotate buttons when it can be turned, and Confirm
    def setupPreview(f : Faction, round : Int, l : $[Faction], tile : String, spot : Spot, rs : $[Int], r : Int) =
        Ask(f)
            .when(rs.num > 1)(SetupRotateAction(f, round, l, tile, spot, r, 1))
            .when(rs.num > 1)(SetupRotateAction(f, round, l, tile, spot, r, -1))
            .add(SetupTurnAction(f, round, l, tile, spot, r))
            .cancel

    def explorePreview(f : Faction, tile : String, spot : Spot, rs : $[Int], r : Int, times : Int, e : ExploreEffect, then : ForcedAction) =
        Ask(f)
            .when(rs.num > 1)(ExploreRotateAction(f, tile, spot, r, 1, times, e, then))
            .when(rs.num > 1)(ExploreRotateAction(f, tile, spot, r, -1, times, e, then))
            .add(ExploreTurnAction(f, tile, spot, r, times, e, then))
            .cancel

    // Legal tile placements: next to tiles in `next` (any placed tile when None), keeping borders continuous,
    // never joining units of two players, with units placed in setup needing an empty area on the new tile
    // Cached until the board or a figure changes: the bots ask for it again for every spot they look at,
    // and each call rebuilds the territories for every free spot and rotation
    def placements(tile : String, near : Option[$[Territory]], setup : Boolean)(implicit game : Game) : $[(Spot, Int)] = {
        val key = (tile, near, setup, game.board.placements, game.units, game.companions, game.chiefs, game.creatureLine, game.creatureAt, game.blainn)

        game.placementCache.getOrElse(key, {
            val r = computePlacements(tile, near, setup)
            if (game.placementCache.size >= 32)
                game.placementCache.clear()
            game.placementCache(key) = r
            r
        })
    }

    // The players with figures in t (Kaija counts as Bear Clan's)
    private def occupants(t : Territory)(implicit game : Game) : $[Faction] =
        (t.areas./~(a => game.unitsAt(a).keys) ++ game.companions.filter(x => t.areas.contains(x._2)).map(_._1) ++ game.chiefs.toList.filter(x => t.areas.contains(x._2)).map(_._1) ++ game.blainn.toList.filter(x => t.areas.contains(x._2)).map(_._1)).distinct

    // A Fallen Valkyrie shares its territory with nobody
    private def valkyrieIn(t : Territory)(implicit game : Game) : Boolean =
        game.creatureLine.exists(c => c.kind.shares.not && t.areas.contains(game.creatureAt(c)))

    // JoinedTilesOnly: the tile at (x, y) joins territories holding two players' units, or a player's units and a Fallen Valkyrie
    // (but not the Robotos bot's, which it ignores, hostileIn). Only the territories joined count: two clans already sharing one
    // (teammates, or Robotos beside a Valkyrie) elsewhere on the map, or in a territory the tile only extends, stop nothing
    private def joins(preview : $[Territory], x : Int, y : Int)(implicit game : Game) : Boolean =
        preview.%(_.areas.exists(a => a.x == x && a.y == y)).exists { t =>
            val parts = game.board.territories.%(o => o.areas.forall(t.areas.contains)).%(o => occupants(o).any || valkyrieIn(o))
            val players = parts./~(occupants).distinct
            parts.num > 1 && (players.num > 1 || players.any && players.exists(game.robotos).not && parts.exists(valkyrieIn))
        }

    private def computePlacements(tile : String, near : Option[$[Territory]], setup : Boolean)(implicit game : Game) : $[(Spot, Int)] = {
        val board = game.board

        val spots = board.frontier.%{ case (x, y) =>
            near.forall(l => Side.all.exists { s =>
                board.areaAt(x + s.dx, y + s.dy, s.opposite).exists(a => l.contains(board.territory(a)))
            })
        }

        spots./~{ case (x, y) =>
            0.until(4)./~{ r =>
                val p = Placement(tile, x, y, r)
                board.tryPlace(p)./~{ preview =>
                    val mixed = game.options.has(JoinedTilesOnly).?(joins(preview, x, y)).|(preview.exists { t =>
                        val players = occupants(t)
                        players.num > 1 || players.any && valkyrieIn(t)
                    })
                    val room = setup.not || p.spec.areas.exists(a => preview.find(_.areas.contains(AreaRef(x, y, a.id))).get.areas.forall(b => game.unitsAt(b).isEmpty))
                    (mixed.not && room).?(Spot(x, y) -> r)
                }
            }
        }
    }

    def explorable(f : Faction, anywhere : Boolean)(implicit game : Game) : $[Territory] =
        anywhere.?(game.board.territories.%(game.board.open)).|(game.controlled(f).%(game.board.open) ++ game.raidExplore.??(game.board.territories.%(game.board.open).%(t => game.present(t).none))).%(t => game.bearIn(t).not)

    // Each building f can build in each territory, with the free spaces it can go on (the first of each kind):
    // a small building on a small or Carved Stone space, a Carved Stone only on a Carved Stone space,
    // a large building only on a large space
    def buildOptions(f : Faction, e : BuildEffect, smallOnly : Boolean)(implicit game : Game) : $[(AreaRef, Building, $[SpaceRef], Int)] = {
        game.controlled(f).%(t => game.bearIn(t).not)./~{ t =>
            val here = game.buildingsIn(t).map(_._2)
            // Ancestral Equipment tokens (Ox Clan) keep their spaces from being built on
            val free = t.areas./~(a => game.board.spec(a).spaces.indices./(i => SpaceRef(a, i))).%(s => game.buildings.contains(s).not && game.gear.contains(s).not)

            MineralsExpansion.buildings.%(b => e.duplicate || here.has(b).not).%(b => game.buildingsLeft(b) > 0).%(b => smallOnly.not || b.large.not).%(b => b != Mine || MineralsExpansion.lair(t))./~{ b =>
                val kinds = b match {
                    case CarvedStone => $(CarvedSpace)
                    case b if b.large => $(LargeSpace)
                    case _ => $(SmallSpace, CarvedSpace)
                }
                val spaces =
                    // Amenities: small buildings take no space
                    if ((e.special == AmenitiesBuild || e.special == HorseBuild) && b.large.not)
                        $(SpaceRef(t.anchor, SpaceRef.extra + game.buildings.keys.count(s => s.area == t.anchor && s.index >= SpaceRef.extra)))
                    else
                        kinds./~(k => free.%(s => spaceKind(s) == k).take(1))
                val cost = math.max(0, b.cost - e.discount)
                (spaces.any && f.wood >= cost).?((t.anchor, b, spaces, cost))
            }
        }
    }

    // The free spaces f can build on (with Amenities, a spot by the territory's number for small buildings needing no space)
    def buildSpots(f : Faction, e : BuildEffect, smallOnly : Boolean)(implicit game : Game) : $[SpaceRef] =
        game.controlled(f).%(t => game.bearIn(t).not)./~{ t =>
            val free = t.areas./~(a => game.board.spec(a).spaces.indices./(i => SpaceRef(a, i))).%(s => game.buildings.contains(s).not && game.gear.contains(s).not)
            val extra = noSpace(e).$(SpaceRef(t.anchor, SpaceRef.extra + game.buildings.keys.count(s => s.area == t.anchor && s.index >= SpaceRef.extra)))
            (free ++ extra).%(s => MineralsExpansion.buildings.exists(b => buildBlock(f, e, smallOnly, s, b).none))
        }

    // Amenities and Horse Clan: small buildings take no space
    def noSpace(e : BuildEffect) = e.special == AmenitiesBuild || e.special == HorseBuild

    def buildCost(b : Building, e : BuildEffect) = math.max(0, b.cost - e.discount)

    // Why b can't be built on s, if it can't (the same rules as buildOptions)
    def buildBlock(f : Faction, e : BuildEffect, smallOnly : Boolean, s : SpaceRef, b : Building)(implicit game : Game) : |[String] = {
        val t = game.board.territory(s.area)
        val extra = s.index >= SpaceRef.extra
        val cost = buildCost(b, e)

        if (smallOnly && b.large) |("small buildings only")
        else if (extra && b.large) |("needs a large space")
        else if (extra.not && noSpace(e) && b.large.not) |("needs no space")
        else if (extra.not && b == CarvedStone && spaceKind(s) != CarvedSpace) |("needs a Carved Stone space")
        else if (extra.not && b.large && spaceKind(s) != LargeSpace) |("needs a large space")
        else if (extra.not && b.large.not && spaceKind(s) == LargeSpace) |("large buildings only")
        else if (b == Mine && MineralsExpansion.lair(t).not) |("needs a creature lair")
        else if (e.duplicate.not && game.buildingsIn(t).exists(_._2 == b)) |("already in this territory")
        else if (game.buildingsLeft(b) <= 0) |("none left")
        else if (f.wood < cost) |("needs " + cost + " wood")
        else None
    }

    def spaceKind(s : SpaceRef)(implicit game : Game) : SpaceKind = game.board.spec(s.area).spaces(s.index).kind

    // Building b in a: straight away when it fits only one space, else the player picks the space
    def buildChoice(f : Faction, a : AreaRef, b : Building, spaces : $[SpaceRef], cost : Int, times : Int, e : BuildEffect, smallOnly : Boolean, then : ForcedAction) : UserAction =
        if (spaces.num == 1)
            BuildPlaceAction(f, a, b, spaces.head, cost, times, e, smallOnly, then)
        else
            BuildSlotAction(f, a, b, cost, times, e, smallOnly, then)

    // Industrious Villagers: f's buildings and what each could become
    def replacements(f : Faction)(implicit game : Game) : $[(SpaceRef, Building)] =
        game.controlled(f)./~(game.buildingsIn)./~{ case (s, old) =>
            val carved = s.index >= SpaceRef.extra || game.board.spec(s.area).spaces(s.index).kind == CarvedSpace
            MineralsExpansion.buildings.%(_ != old).%(_.large == old.large).%(b => b != CarvedStone || carved).%(b => b != Mine || MineralsExpansion.lair(game.board.territory(s.area))).%(b => game.buildingsLeft(b) > 0)./(b => s -> b)
        }

    // A replacement offer: with more than one building to replace, the building is tapped first (ReplaceSpotAction)
    def replaceAsk(f : Faction, source : String, l : $[(SpaceRef, Building)], then : ForcedAction)(implicit game : Game) : Ask = {
        val spaces = l.map(_._1).distinct

        if (spaces.num == 1)
            replaceChoices(f, source, l, then)
        else
            Ask(f).each(spaces)(s => ReplaceSpotAction(f, source, s, then))
    }

    def replaceChoices(f : Faction, source : String, l : $[(SpaceRef, Building)], then : ForcedAction)(implicit game : Game) : Ask =
        if (source == "Craftsmen")
            Ask(f).each(l) { case (s, b) => CraftsmenReplaceAction(f, s.area, s, b, then) }
        else
            Ask(f).each(l) { case (s, b) => ReplaceBuildingAction(f, s.area, s, b, then) }

    def setupUnits(f : Faction, round : Int, l : $[Faction], a : AreaRef)(implicit game : Game) = {
        game.addUnits(a, f, 3)

        f.log("placed three units in", a)

        robotosSetup(f, round, a)

        Then(SetupPlaceAction(round, l.drop(1)))
    }

    def setupKaija(f : Faction, round : Int, l : $[Faction], a : AreaRef)(implicit game : Game) = {
        game.addUnits(a, f, 2)
        game.setCompanion(f, |(a))
        game.note("kaija-setup")

        f.log("placed two units and", Companion(f), "in", a)

        robotosSetup(f, round, a)

        Then(SetupPlaceAction(round, l.drop(1)))
    }

    def setupChief(f : Faction, round : Int, l : $[Faction], a : AreaRef, kaija : Boolean)(implicit game : Game) = {
        game.addUnits(a, f, kaija.?(1).|(2))
        game.chiefs += f -> a
        if (kaija)
            game.setCompanion(f, |(a))
        game.note("chief-setup")

        f.log(kaija.?("placed one unit, " ~ Companion(f).elem0 ~ " and").|("placed two units and"), WarchiefElem(f), "in", a)

        robotosSetup(f, round, a)

        Then(SetupPlaceAction(round, l.drop(1)))
    }

    def recruitUnit(f : Faction, a : AreaRef)(implicit game : Game) : Unit = {
        game.addUnits(a, f, 1)

        f.log("recruited in", a)
    }

    def recruitKaija(f : Faction, a : AreaRef)(implicit game : Game) : Unit = {
        game.setCompanion(f, |(a))
        game.note("kaija-recruit")

        f.log("recruited", Companion(f), "in", a)
    }

    def recruitChief(f : Faction, a : AreaRef)(implicit game : Game) : Unit = {
        game.chiefs += f -> a
        game.note("chief-recruit")

        f.log("recruited", WarchiefElem(f), "in", a)
    }

    // Robotos places one more unit with its first setup group
    def robotosSetup(f : Faction, round : Int, a : AreaRef)(implicit game : Game) {
        // Robotos places 5 units each time instead of 3
        if (game.robotos(f)) {
            game.addUnits(a, f, 2)
            f.log("placed two more units in", a, "(Robotos)".hl)
        }
    }

    def canRecruit(f : Faction)(implicit game : Game) = game.reserve(f) > 0 || game.kaijaReady(f) || game.chiefReady(f)

    // Closed territories of 3 or more tiles, for Protector of the Land
    def bigClosed(f : Faction)(implicit game : Game) = game.controlled(f).%(game.board.closed).%(t => game.board.tiles(t) >= 3)

    // Neutral or enemy territories next to f's, where the Scorched Earth token can go (not a teammate's)
    def scorchable(f : Faction)(implicit game : Game) : $[Territory] = {
        val mine = game.controlled(f)
        game.board.territories.%(t => game.present(t).forall(game.enemy(f, _)))
            .%(t => mine.exists(m => game.board.adjacent(m).exists(_._1 == t)))
            .%(t => game.scorchedIn(t).not)
    }

    def playable(f : Faction, e : Effect)(implicit game : Game) : Boolean = e match {
        case RecruitEffect(_, mode) => canRecruit(f) && recruitTargets(f, mode, $).any
        case AwakenEffect => true
        case ProtectorEffect => bigClosed(f).any && CommonExpansion.available(f) > 0
        // A Move action may be played just to attack creatures
        case MoveEffect(_, _, _, _) => moveSources(f).any || CreaturesExpansion.attackable(f).any
        case e : ExploreEffect => game.pile.any && explorable(f, e.anywhere).any
        case e : BuildEffect => buildOptions(f, e, false).any
        case FeastEffect => $(RecruitEffect(1), MoveEffect(1), ExploreEffect(), BuildEffect()).exists(playable(f, _))
        case e => CardsExpansion.playable(f, e)
    }

    def resolve(f : Faction, e : Effect, then : ForcedAction)(implicit game : Game) : Continue = e match {
        case RecruitEffect(n, mode) => Then(RecruitAction(f, n, mode, $, then))
        case AwakenEffect =>
            game.awakened = true
            f.log("woke Kaija: it can enter enemy territories this year")
            Then(RecruitAction(f, 2, RecruitNormal, $, then))
        case ProtectorEffect =>
            val n = bigClosed(f).num
            f.log("drew", n.hl, (n == 1).?("card").|("cards"), "for closed territories of 3 or more tiles")
            Then(DrawCardsAction(f, n, then))
        case e : MoveEffect => Then(MoveStartAction(f, e, then))
        case e : ExploreEffect => Then(ExploreAction(f, e.draw, e.times, e.redraw, e, then))
        case e : BuildEffect => Then(BuildAction(f, e, e.times, false, then))
        case FeastEffect =>
            Ask(f).each($(RecruitEffect(1), MoveEffect(1), ExploreEffect(), BuildEffect()).%(playable(f, _)))(e => FeastChoiceAction(f, e, then))
        case e => CardsExpansion.resolve(f, e, then)
    }

    // A die result is in; Axe Throwers adds 1 point or 1 casualty to the attacker's
    def rolled(attacker : Faction, defender : Faction, a : AreaRef, e : MoveEffect, food : $[Int], faces : $[DieFace], rolled : DieFace, then : ForcedAction)(implicit game : Game) : Continue = {
        // Surtr (Dragon Clan's warchief): a casualty if none was rolled, a point if none was rolled
        val self = faces.none.?(attacker).|(defender)
        val face = NewBloodExpansion.surtr(self, game.board.territory(a), rolled)

        if (faces.none && e.special == AxeMove)
            Ask(attacker).add(AxeAction(attacker, defender, a, e, food, face.copy(points = face.points + 1), then)).add(AxeAction(attacker, defender, a, e, food, face.copy(casualties = face.casualties + 1), then))
        else
            Then(CombatRollAction(attacker, defender, a, e, food, faces :+ face, then))
    }

    // Collect what a territory produces, as at harvest
    def collect(f : Faction, t : Territory, reason : Elem)(implicit game : Game) : (Int, Int, Int) = {
        val (food, wood, lore) = game.harvest(t)
        f.food += food
        f.wood += wood
        f.lore += lore
        if (food + wood + lore > 0)
            f.log("collected", $(food -> Food, wood -> Wood, lore -> Lore).filter(_._1 > 0)./{ case (n, r) => Amount(n.hl, r.elem) }.join(", "), reason)
        (food, wood, lore)
    }

    // Where f's figures in t can retreat to: not into a fight, nor where a creature is being attacked or a Fallen Valkyrie is
    def retreats(f : Faction, t : Territory, rough : Boolean)(implicit game : Game) : $[Territory] = {
        val fighting = (game.combats ++ game.creatureFights./(_.area))./(game.board.territory)
        game.board.adjacent(t).filter(rough || _._2).map(_._1).%(o => fighting.has(o).not).%(o => game.present(o).but(f).none).%(o => game.hostileIn(o).not).%(o => game.swampIn(o).not)
    }

    // How many go (all, or one) after picking where: with one destination the pick was made for the player, so there is
    // nothing to cancel back to, but the choice is still shown so the player sees where the units go
    def retreatCount(ask : Ask, f : Faction, from : AreaRef, rough : Boolean)(implicit game : Game) =
        (retreats(f, game.board.territory(from), rough).num > 1).?(ask.cancel).|(ask.needOk)

    // The ways f can take k casualties in t: units, Kaija (or Brundr and Kaelinn, or Brok), the warchief, Blainn, units first
    def casualtyParties(f : Faction, t : Territory, k : Int)(implicit game : Game) : $[(Int, Boolean, Boolean, Boolean)] = {
        val n = game.count(t, f)
        def may(b : Boolean) = b.?($(false, true)).|($(false))

        for (chief <- may(game.chiefIn(t, f)); blainn <- may(game.blainnIn(t, f)); kaija <- may(game.kaijaIn(t, f)) ; s = $(kaija, chief, blainn).count(b => b) ; if s <= k && k - s <= n)
            yield (k - s, kaija, chief, blainn)
    }.sortBy(p => $(p._2, p._3, p._4).count(b => b))

    // Asks the first of l (a clan and the casualties it takes) with a choice to make, then again; None once all are picked
    // (Robotos's request of 2026-10-11; only in games with ChooseCasualties, and never for the Automa)
    def pickCasualties(l : $[(Faction, Int)], t : Territory, again : ForcedAction)(implicit game : Game) : |[Ask] =
        game.options.has(ChooseCasualties).??(l.%{ case (f, k) => k > 0 && f != Automa && game.casualtyPicks.contains(f).not }./{ case (f, k) => (f, k, casualtyParties(f, t, k)) }.%(_._3.num > 1).headOption./{ case (f, k, l) =>
            Ask(f).each(l) { case (n, kaija, chief, blainn) => CasualtyPickAction(f, t.anchor, k, n, kaija, chief, blainn, again) }
        })

    // Who can retreat together: everyone, then the units without Kaija and the warchief, one unit, Kaija alone, the warchief alone
    def retreatParties(f : Faction, from : AreaRef)(implicit game : Game) : $[(Int, Boolean, Boolean)] = {
        val t = game.board.territory(from)
        val n = game.count(t, f)
        val kaija = game.kaijaIn(t, f)
        val chief = game.chiefIn(t, f)

        $((n, kaija, chief)) ++ (n > 0).$((n, false, false)) ++ (n > 1).$((1, false, false)) ++ kaija.$((0, true, false)) ++ chief.$((0, false, true))
    }.distinct

    // The resources a set of tiles shows
    def shown(tiles : $[String]) : $[Resource] = Resource.all.%(r => tiles./~(Tiles(_).areas).exists(a => r match {
        case Food => a.food > 0
        case Wood => a.wood > 0
        case Lore => a.lore > 0
    }))

    // The pile a tile goes in for BalancedStartTiles: the resource it shows that the fewest tiles show (lore, then food), none for a blank tile
    lazy val startPileOf : Map[String, Resource] = {
        val all = Tiles.regular ++ Tiles.horizons
        val count = Resource.all./(r => r -> all.count(t => shown($(t.id)).has(r))).toMap
        all./~(t => shown($(t.id)).sortBy(count).headOption./(t.id -> _)).toMap
    }

    // Three starting tiles from the top of the pile; with BalancedStartTiles, the first food, wood and lore tiles in the pile
    // (the piles of startPileOf), or the top tile when a pile has run out
    def dealStart()(implicit game : Game) : $[String] = {
        if (game.options.has(BalancedStartTiles)) {
            var hand : $[String] = $

            Resource.all.foreach { r =>
                val t = game.pile.find(startPileOf.get(_).has(r)).|(game.pile.head)
                hand :+= t
                game.pile = game.pile.diff($(t))
            }

            hand
        }
        else {
            val hand = game.pile.take(3)
            game.pile = game.pile.drop(3)
            hand
        }
    }

    def perform(action : Action, soft : Void)(implicit game : Game) = action @@ {
        // SETUP
        case ShuffledTilesAction(l) =>
            game.pile = l

            // Wastelands: a central tile can replace the starting tile
            game.board.place(Placement(game.central, 0, 0, 0))

            // Five and six players: both starting tiles; with a Wastelands central tile, the five-player tile with the same
            // borders, whose middle territory is part of the central territory
            if (game.arity >= 5) {
                game.board.place(Placement(Waste.five(game.central), 1, 0, 0))

                if (game.central != "start" && Waste.impassable.has(game.central).not)
                    game.board.join(AreaRef(0, 0, Waste.middle(game.central)), AreaRef(0, 0, "e"))
            }

            // Adset: the tiles were drawn before the draft; the clans are picked as they place their tiles
            if (game.adset)
                Then(AdsetTurnAction(1, game.seats.reverse))
            else {
                game.factions.foreach { f =>
                    game.tileHand += f -> dealStart()
                }

                log("Each player drew three map tiles")

                Then(SetupPlaceAction(1, game.from(game.first)))
            }

        case SetupPlaceAction(1, Nil) =>
            Then(SetupPlaceAction(2, game.from(game.first)))

        case SetupPlaceAction(2, Nil) =>
            game.factions.foreach { f =>
                game.pile ++= game.tileHand(f)
                game.tileHand += f -> $
            }

            Shuffle[String](game.pile, ShuffledTilesBackAction(_))

        case ShuffledTilesBackAction(l) =>
            game.pile = l

            log("The unused tiles went back into the pile")

            Then(StartYearAction)

        case SetupPlaceAction(round, f :: rest) =>
            Ask(f).each(game.tileHand(f).%(t => setupPlacements(t, round).any))(t => SetupTileAction(f, round, f :: rest, t))
                .bailHard(SetupPlaceAction(round, rest))

        case SetupTileAction(f, round, l, tile) =>
            Ask(f).each(setupPlacements(tile, round).map(_._1).distinct)(s => SetupSpotAction(f, round, l, tile, s)).cancel

        case SetupSpotAction(f, round, l, tile, spot) =>
            val rs = rotations(setupPlacements(tile, round), spot)

            setupPreview(f, round, l, tile, spot, rs, rs.head)

        case SetupRotateAction(f, round, l, tile, spot, r, d) =>
            val rs = rotations(setupPlacements(tile, round), spot)

            setupPreview(f, round, l, tile, spot, rs, rotate(rs, r, d))

        case SetupTurnAction(f, round, l, tile, spot, r) =>
            game.tileHand += f -> game.tileHand(f).diff($(tile))
            game.board.place(Placement(tile, spot.x, spot.y, r))

            f.log("placed a tile")

            Then(TilePlacedAction(f, tile, spot, true, SetupUnitsAskAction(f, round, l, tile, spot)))

        case SetupUnitsAskAction(f, round, l, tile, spot) =>
            val all = Tiles(tile).areas./(a => game.board.territory(AreaRef(spot.x, spot.y, a.id))).distinct.%(t => game.present(t).none).%(t => game.swampIn(t).not)
            // Not with a Fallen Valkyrie, unless there is no other choice
            val empty = all.exists(t => game.hostileIn(t).not).?(all.%(t => game.hostileIn(t).not)).|(all)

            // One territory per row when Kaija or the warchief could go there too; what to place is asked after it is picked
            if (game.kaijaReady(f) || game.chiefReady(f))
                Ask(f).each(empty)(t => SetupAreaAction(f, round, l, t.anchor))
            else
                Ask(f).each(empty)(t => SetupUnitsAction(f, round, l, t.anchor))

        // The usual placement first: the warchief with two units; a second figure (Kaija, Brok) in the other starting
        // territory, or with the warchief in the second one if the first got three units
        case SetupAreaAction(f, round, l, a) =>
            val kaija = game.kaijaReady(f)
            val chief = game.chiefReady(f)

            Ask(f)
                .when(chief && (kaija.not || round == 1))(SetupChiefHereAction(f, round, l, a, false))
                .when(chief && kaija)(SetupChiefHereAction(f, round, l, a, true))
                .when(chief.not && kaija)(SetupKaijaHereAction(f, round, l, a))
                .when(chief && kaija && round == 2)(SetupChiefHereAction(f, round, l, a, false))
                .when(chief && kaija)(SetupKaijaHereAction(f, round, l, a))
                .add(SetupUnitsHereAction(f, round, l, a))
                .cancel

        case TilePlacedAction(f, tile, spot, setup, then) =>
            Then(then)

        case SetupUnitsAction(f, round, l, a) =>
            setupUnits(f, round, l, a)

        case SetupUnitsHereAction(f, round, l, a) =>
            setupUnits(f, round, l, a)

        case SetupKaijaAction(f, round, l, a) =>
            setupKaija(f, round, l, a)

        case SetupKaijaHereAction(f, round, l, a) =>
            setupKaija(f, round, l, a)

        case SetupChiefAction(f, round, l, a, kaija) =>
            setupChief(f, round, l, a, kaija)

        case SetupChiefHereAction(f, round, l, a, kaija) =>
            setupChief(f, round, l, a, kaija)

        // RECRUIT
        case RecruitAction(f, left, mode, placed, then) =>
            val targets = (left > 0 && canRecruit(f)).??(recruitTargets(f, mode, placed))

            if (targets.none)
                Then(TrainingCampsAction(f, placed, then))
            else
                Ask(f).some(targets) { t =>
                    val l =
                        (game.reserve(f) > 0).$(RecruitPlaceAction(f, t.anchor, left, mode, placed, then)) ++
                        game.kaijaReady(f).$(RecruitKaijaAction(f, t.anchor, left, mode, placed, then)) ++
                        game.chiefReady(f).$(RecruitChiefAction(f, t.anchor, left, mode, placed, then))

                    // One territory per row; what to recruit there is asked after it is picked
                    if (l.num > 1)
                        $(RecruitAreaAction(f, t.anchor, left, mode, placed, then))
                    else
                        l
                }
                    .when(placed.any)(RecruitDoneAction(f, placed, then))

        case RecruitAreaAction(f, a, left, mode, placed, then) =>
            Ask(f)
                .when(game.reserve(f) > 0)(RecruitUnitHereAction(f, a, left, mode, placed, then))
                .when(game.kaijaReady(f))(RecruitKaijaHereAction(f, a, left, mode, placed, then))
                .when(game.chiefReady(f))(RecruitChiefHereAction(f, a, left, mode, placed, then))
                .cancel

        case RecruitPlaceAction(f, a, left, mode, placed, then) =>
            recruitUnit(f, a)
            Then(RecruitAction(f, left - 1, mode, placed :+ a, then))

        case RecruitUnitHereAction(f, a, left, mode, placed, then) =>
            recruitUnit(f, a)
            Then(RecruitAction(f, left - 1, mode, placed :+ a, then))

        case RecruitKaijaAction(f, a, left, mode, placed, then) =>
            recruitKaija(f, a)
            Then(RecruitAction(f, left - 1, mode, placed :+ a, then))

        case RecruitKaijaHereAction(f, a, left, mode, placed, then) =>
            recruitKaija(f, a)
            Then(RecruitAction(f, left - 1, mode, placed :+ a, then))

        case RecruitChiefAction(f, a, left, mode, placed, then) =>
            recruitChief(f, a)
            Then(RecruitAction(f, left - 1, mode, placed :+ a, then))

        case RecruitChiefHereAction(f, a, left, mode, placed, then) =>
            recruitChief(f, a)
            Then(RecruitAction(f, left - 1, mode, placed :+ a, then))

        case RecruitDoneAction(f, placed, then) =>
            Then(TrainingCampsAction(f, placed, then))

        case TrainingCampsAction(f, placed, then) =>
            game.recruited = placed

            // Robotos gets one more unit with every Recruit, where it recruited first
            if (game.robotos(f) && placed.any && game.reserve(f) > 0) {
                game.addUnits(placed.head, f, 1)
                f.log("recruited", 1.hl, "more in", placed.head, "(Robotos)".hl)
                game.note("robotos-recruit")
            }

            placed./(game.board.territory).distinct.foreach { t =>
                val camps = game.working(t).count(_ == TrainingCamp)
                val n = math.min(camps, game.reserve(f))
                if (n > 0) {
                    game.addUnits(t.anchor, f, n)
                    f.log("recruited", n.hl, "more with", TrainingCamp, "in", t.anchor)
                }
            }

            Then(then)

        // MOVE
        case MoveAction(f, left, e, then) =>
            // Figures passing through a teammate's territory or the Swamp move on first
            val sources = (left > 0).??(game.passing(f).any.?(game.passing(f)).|(moveSources(f, e)).%(t => destinations(f, t, left, e).any))

            if (sources.none)
                Then(MovesMadeAction(f, e, then))
            else
                Ask(f).each(sources)(t => MoveFromAction(f, t.anchor, left, e, then))
                    // Team play: units can't stop in a teammate's territory
                    .when(game.passing(f).none)(MoveDoneAction(f, e, then))

        case MoveFromAction(f, from, left, e, then) =>
            val t = game.board.territory(from)

            Ask(f).each(destinations(f, t, left, e))((o, cost) => MoveToAction(f, from, o.anchor, cost, left, e, then)).cancel

        case MoveToAction(f, from, to, cost, left, e, then) =>
            val t = game.board.territory(from)
            val dst = game.board.territory(to)
            val n = game.count(t, f)
            // Kaija can't enter enemy territories unless awakened, nor a teammate's it couldn't move on from
            val kaija = game.kaijaIn(t, f) && (game.restrained(f).not || game.present(dst).forall(game.allied(f, _))) &&
                (game.passOnly(f, dst).not || canPass(f, dst, left - cost, e, true))

            val chief = game.chiefIn(t, f)

            // Figures passing through a teammate's territory or the Swamp move on together
            if (game.passOnly(f, t))
                Ask(f).add(MoveUnitsAction(f, from, to, n, game.kaijaIn(t, f), chief, cost, left, e, then)).cancel
            else
            Ask(f)
                .each(n.to(1, -1).$)(k => MoveUnitsAction(f, from, to, k, false, false, cost, left, e, then))
                .some(kaija.$(n.to(0, -1).$).flatten)(k => $(MoveUnitsAction(f, from, to, k, true, false, cost, left, e, then)))
                .some(chief.$(n.to(0, -1).$).flatten)(k => $(MoveUnitsAction(f, from, to, k, false, true, cost, left, e, then)))
                .some((chief && kaija).$(n.to(0, -1).$).flatten)(k => $(MoveUnitsAction(f, from, to, k, true, true, cost, left, e, then)))
                .cancel

        case MoveUnitsAction(f, from, to, n, kaija, chief, cost, left, e, then) =>
            val src = game.board.territory(from)
            val dst = game.board.territory(to)

            // Bear Clan card: Kaija collects the food and wood of the territory it leaves
            if (kaija)
                game.note("kaija-move")

            if (kaija && e.special == BearMove) {
                game.note("kaija-collect")
                val (food, wood, _) = game.produce(src)
                f.food += food
                f.wood += wood
                if (food + wood > 0)
                    f.log("collected", food.hl, Food, "and", wood.hl, Wood, "with", "Kaija".hl)
            }

            game.removeUnits(src, f, n)
            game.addUnits(dst.anchor, f, n)
            if (kaija)
                game.setCompanion(f, |(dst.anchor))
            if (chief)
                game.chiefs += f -> dst.anchor

            val enemy = game.present(dst).%(game.enemy(f, _))

            f.log("moved", Party(f, n, kaija, chief), "from", from, "to", to, enemy.any.?("and attacked " ~ enemy./(_.elem).join(", ")).|(Empty), game.passOnly(f, dst).?("(passing through)".txt).|(Empty))

            // The Swamp (Wilderness): figures going in lose one of them
            if (game.swampIn(dst) && game.swampIn(src).not) {
                game.removeFigures(dst, f, 1)
                game.note("swamp")
                f.log("lost a unit in the", "Swamp".hl)
            }

            if (enemy.any && game.combats.has(dst.anchor).not)
                game.combats :+= dst.anchor

            if (game.mateHeld(f, dst))
                game.note("team-pass")

            Then(MoveAction(f, left - cost, e, then))

        case MoveDoneAction(f, e, then) =>
            Then(MovesMadeAction(f, e, then))

        // Before the Move: what f holds (Halvard's Craft), and Borgild's Shield bringing Kaija and Borgild together
        case MoveStartAction(f, e, then) =>
            game.heldBefore = game.controlled(f)./~(_.areas)
            game.mended = 0
            game.mendedFigures = $

            val apart = game.kaija.any && game.chiefs.contains(f) && game.board.territory(game.kaija.get) != game.board.territory(game.chiefs(f))

            if (e.special == BorgildMove && f == Bear && apart)
                Ask(f).add(BorgildShieldAction(f, true, e, then)).add(BorgildShieldAction(f, false, e, then)).add(BorgildShieldSkipAction(f, e, then))
            else
                Then(MoveAction(f, e.n, e, then))

        case BorgildShieldAction(f, kaijaMoves, e, then) =>
            if (kaijaMoves)
                game.kaija = game.chiefs.get(f)
            else
                game.chiefs += f -> game.kaija.get

            f.log("brought", "Kaija".hl, "and", WarchiefElem(f), "together with", "Borgild's Shield".hl)

            Then(MoveAction(f, e.n, e, then))

        case BorgildShieldSkipAction(f, e, then) =>
            Then(MoveAction(f, e.n, e, then))

        // After the moves: Egil's Fury may remove a building from a territory being attacked
        case MovesMadeAction(f, e, then) =>
            val attacked = game.combats.%(a => game.present(game.board.territory(a)).num > 1)./(game.board.territory)
            val l = (e.special == EgilMove).??(attacked./~(game.buildingsIn))

            if (l.any)
                Ask(f).each(l) { case (s, b) => EgilFuryAction(f, s, b, e, then) }.add(EgilFurySkipAction(f, e, then))
            else
                Then(MoveEndAction(f, e, then))

        case EgilFuryAction(f, s, b, e, then) =>
            game.buildings -= s

            f.log("removed", b, "from", s.area, "with", "Egil's Fury".hl)

            Then(MoveEndAction(f, e, then))

        case EgilFurySkipAction(f, e, then) =>
            Then(MoveEndAction(f, e, then))

        case HalvardCraftSkipAction(f, then) =>
            Then(then)

        case MoveEndAction(f, e, then) =>
            Then(CombatsAction(f, e, then))

        case CombatsAction(f, e, then) =>
            game.battle = None

            val l = game.combats.%(a => game.present(game.board.territory(a)).num > 1)
            // Creatures attacked by this Move action (Creatures module)
            val cl = game.creatureFights.%(x => game.creatureLine.has(x.creature) && game.figures(game.board.territory(x.area), f) > 0)

            if (l.none && cl.none) {
                game.creatureFights = $
                game.combats = $
                // Snake Clan card: the token may move after the move
                if (e.special == SnakeMove)
                    Then(ScorchedAction(f, then))
                else
                // Svarn's Menders: the casualties come back
                if (e.special == SvarnMove && (game.mended > 0 || game.mendedFigures.any))
                    Then(SvarnMendAction(f, then))
                else
                // Halvard's Craft: a free small building in a newly controlled territory
                if (e.special == HalvardMove) {
                    val fresh = game.controlled(f).%(t => t.areas.forall(a => game.heldBefore.has(a).not))./(_.anchor)
                    val l = buildOptions(f, BuildEffect(discount = 1), true).%(x => fresh.has(x._1))

                    if (l.none)
                        Then(then)
                    else
                        Ask(f).each(l) { case (a, b, s, _) => buildChoice(f, a, b, s, 0, 1, BuildEffect(discount = 1), true, then) }.add(HalvardCraftSkipAction(f, then))
                }
                else
                    Then(then)
            }
            else
            if (l.num == 1 && cl.none)
                Then(FightAction(f, l.head, e, CombatsAction(f, e, then)))
            else
            if (l.none && cl.num == 1)
                Then(CreatureFightAction(f, cl.head.area, cl.head.creature, e, CombatsAction(f, e, then)))
            else
                Ask(f).each(l)(a => FightAction(f, a, e, CombatsAction(f, e, then)))
                    .each(cl)(x => CreatureFightAction(f, x.area, x.creature, e, CombatsAction(f, e, then)))

        case FightAction(f, a, e, then) =>
            game.combats = game.combats.but(a)

            val t = game.board.territory(a)
            val defender = game.present(t).but(f).head
            val fighting = (game.combats ++ game.creatureFights./(_.area))./(game.board.territory)

            // Intimidate: one defending unit may be pushed to a neutral or defender territory next door
            val push = (e.special == IntimidateMove && game.count(t, defender) > 0).??(game.board.adjacent(t).map(_._1).%(o => fighting.has(o).not).%(o => game.present(o).but(defender).none).%(o => game.hostileIn(o).not).%(o => game.swampIn(o).not))

            if (push.any)
                Ask(f).each(push)(o => IntimidateAction(f, a, o.anchor, e, then)).add(IntimidateSkipAction(f, a, e, then))
            else
                Then(FightStartAction(f, a, e, then))

        case IntimidateAction(f, a, to, e, then) =>
            val t = game.board.territory(a)
            val defender = game.present(t).but(f).head

            game.removeUnits(t, defender, 1)
            game.addUnits(to, defender, 1)

            f.log("pushed a unit of", defender, "from", a, "to", to)

            // FAQ: the combat has started (Intimidate is in its step 1), so pushing the last defender out wins it,
            // with what winning brings (Wolf's food, Stag's fame, Conquest), though no die is rolled
            if (game.present(t).but(f).none) {
                game.fights += 1
                f.log("won the fight in", a, "with", "Intimidate".hl)
                EventsExpansion.afterCombat(f, defender, 0, 0, |(f))
                game.note("attack-won")
                attackWon(f, a)

                Then(ClanPowers.report(then))
            }
            else
                Then(FightStartAction(f, a, e, then))

        case IntimidateSkipAction(f, a, e, then) =>
            Then(FightStartAction(f, a, e, then))

        case FightStartAction(f, a, e, then) =>
            val t = game.board.territory(a)

            if (game.present(t).but(f).none) {
                f.log("took", a, "without a fight")
                Then(then)
            }
            else {
                game.fights += 1
                game.battle = |(a)

                val defender = game.present(t).but(f).head

                log(f, "fights", defender, "in", a)

                val food = CombatFoodStartAction(f, defender, a, e, FightOverAction(then))
                // New Blood: Ox Clan's Ancestral Equipment tokens, chosen in step 1
                val step = game.has(NewBlood).?(GearAskAction(f, defender, a, e, food) : ForcedAction).|(food)

                // Step 1: Signy and Brand (Warchiefs module), the attacker's first
                if (game.has(Warchiefs))
                    Then(ChiefStepOneAction($(f, defender), a, step))
                else
                    Then(step)
            }

        case FightOverAction(then) =>
            game.battle = None
            game.gearFight = $
            game.ironAsked = $
            game.ironFight = $

            Then(then)

        // Liv's Cunning: wood or lore may be spent like food, one at a time
        case CombatFoodStartAction(f, defender, a, e, then) if e.special == LivMove =>
            Then(LivCunningAskAction(f, defender, a, e, $, then))

        case LivCunningAskAction(f, defender, a, e, spent, then) =>
            val max = game.figures(game.board.territory(a), f)

            Ask(f)
                .some(Resource.all.%(r => f.has(r) > 0 && spent.num < max))(r => $(LivCunningAction(f, defender, a, e, r, spent, then)))
                .add(LivCunningDoneAction(f, defender, a, e, spent, then))

        case CombatFoodStartAction(f, defender, a, e, then) =>
            val t = game.board.territory(a)
            val max = math.min(f.food, game.figures(t, f))

            if (max == 0)
                Then(CombatFoodAction(f, f, defender, a, e, $(0), then))
            else
                Ask(f).each(0.to(max).$)(k => CombatFoodAction(f, f, defender, a, e, $(k), then))

        case LivCunningAction(f, defender, a, e, r, spent, then) =>
            f.gain(r, -1)

            Then(LivCunningAskAction(f, defender, a, e, spent :+ r, then))

        case LivCunningDoneAction(f, defender, a, e, spent, then) =>
            if (spent.any)
                f.log("spent", spent./(_.elem).join(" "), "with", "Liv's Cunning".hl)

            Then(CombatFoodPaidAction(f, defender, a, e, $(spent.num), then))

        case CombatFoodAction(self, attacker, defender, a, e, food, then) =>
            self.food -= food.last

            if (food.last > 0)
                self.log("spent", food.last.hl, Food)

            Then(CombatFoodPaidAction(attacker, defender, a, e, food, then))

        case CombatFoodPaidAction(attacker, defender, a, e, food, then) =>
            if (food.num == 1) {
                val t = game.board.territory(a)
                val max = math.min(defender.food, game.figures(t, defender))

                if (max == 0)
                    Then(CombatFoodAction(defender, attacker, defender, a, e, food :+ 0, then))
                else
                    Ask(defender).each(0.to(max).$)(k => CombatFoodAction(defender, attacker, defender, a, e, food :+ k, then))
            }
            else
                Then(CombatRollAction(attacker, defender, a, e, food, $, then))

        case CombatRollAction(attacker, defender, a, e, food, faces, then) =>
            if (faces.num == 2)
                Then(CombatResolveAction(attacker, defender, a, e, food, faces, then))
            else
                Random[DieFace](NorthgardDie.faces, CombatRolledAction(attacker, defender, a, e, food, faces, _, then))

        case CombatRolledAction(attacker, defender, a, e, food, faces, face, then) =>
            val self = (faces.num == 0).?(attacker).|(defender)

            self.log("rolled", face)

            // Liv may reroll once (Warchiefs module)
            if (Warchief.reroll(game.board.territory(a), self))
                Ask(self)
                    .add(RerollInfoAction(self, true, CombatFaceAction(attacker, defender, a, e, food, faces, face, then)))
                    .add(LivRerollAction(self, CombatRerollAction(attacker, defender, a, e, food, faces, then)))
                    .add(LivKeepAction(self, CombatFaceAction(attacker, defender, a, e, food, faces, face, then)))
            else
            // Ancestral Equipment 3 (Ox Clan): roll again
            if (self == Ox && game.gearFight.has(3))
                Ask(self)
                    .add(RerollInfoAction(self, false, CombatFaceAction(attacker, defender, a, e, food, faces, face, then)))
                    .add(GearRerollAction(self, CombatRerollAction(attacker, defender, a, e, food, faces, then)))
                    .add(GearKeepAction(self, CombatFaceAction(attacker, defender, a, e, food, faces, face, then)))
            else
                Then(CombatFaceAction(attacker, defender, a, e, food, faces, face, then))

        case CombatRerollAction(attacker, defender, a, e, food, faces, then) =>
            Random[DieFace](NorthgardDie.faces, CombatRerolledAction(attacker, defender, a, e, food, faces, _, then))

        case CombatRerolledAction(attacker, defender, a, e, food, faces, face, then) =>
            val self = (faces.num == 0).?(attacker).|(defender)

            self.log("rolled", face)

            Then(CombatFaceAction(attacker, defender, a, e, food, faces, face, then))

        case CombatFaceAction(attacker, defender, a, e, food, faces, face, then) =>
            val self = (faces.num == 0).?(attacker).|(defender)

            if (face.choice)
                Ask(self).add(CombatChooseAction(self, attacker, defender, a, e, food, faces, NorthgardDie.point, then))
                    .add(CombatChooseAction(self, attacker, defender, a, e, food, faces, NorthgardDie.casualty, then))
            else
                rolled(attacker, defender, a, e, food, faces, face, then)

        case CombatChooseAction(self, attacker, defender, a, e, food, faces, choice, then) =>
            self.log("took", choice)

            rolled(attacker, defender, a, e, food, faces, choice, then)

        case AxeAction(attacker, defender, a, e, food, face, then) =>
            attacker.log("took", face, "with", "Axe Throwers".hl)

            Then(CombatRollAction(attacker, defender, a, e, food, $(face), then))

        case CombatResolveAction(attacker, defender, a, e, food, faces, then) =>
            val t = game.board.territory(a)
            val here = game.working(t)
            val au = game.figures(t, attacker)
            val du = game.figures(t, defender)
            // Conqueror: the attacker ignores Fortresses and Defense Towers
            val fortress = attacker.conqueror.?(0).|(2 * here.count(_ == Fortress))
            val towers = attacker.conqueror.?(0).|(here.count(_ == DefenseTower))
            // Snake Clan fights better on its Scorched Earth
            val scorched = game.scorchedIn(t)
            val ab = (attacker == Snake && scorched).??(1)
            val db = (defender == Snake && scorched).??(1)

            // New Blood: clan powers, warchiefs, upgrade cards and Ancestral Equipment tokens
            val anb = NewBloodExpansion.points(attacker, t, e, true, food(0))
            val dnb = NewBloodExpansion.points(defender, t, e, false, food(1))

            // Wastelands: Thor's Wrath, Landvidi; Urdarbrunn's defender ignores a casualty
            val aws = game.has(Wastelands).??(WastelandsExpansion.pointParts(attacker, t, true))
            val dws = game.has(Wastelands).??(WastelandsExpansion.pointParts(defender, t, false))
            val aw = aws.map(_._1).sum
            val dw = dws.map(_._1).sum
            val urdar = game.has(Wastelands).??(WastelandsExpansion.ignored(t))

            // Casualties each side inflicts; Halvard defending ignores 1
            val halvard = math.min(faces(0).casualties, Warchief.shield(t, defender, false))
            // Egil's Fury: +1 casualty; New Blood bonuses; Eldrich (Squirrel): the defender's rolled casualties hit them too
            val eldrich = (e.special == EldrichMove).??(faces(1).casualties)
            // Events: Blood Moon adds a casualty to both sides
            val moon = game.eventIs("blood-moon").??(1)
            val ac = math.max(0, faces(0).casualties - halvard + (e.special == EgilMove).??(1) + NewBloodExpansion.casualties(attacker, t, e, true) + eldrich + moon - NewBloodExpansion.ignored(defender) - urdar)
            // Shieldbearers cancel 1 casualty inflicted by the defender
            val shield = ((e.special == ShieldMove || e.special == BorgildMove) && faces(1).casualties + towers > 0).??(1)
            val dc = math.max(0, faces(1).casualties + towers - shield + NewBloodExpansion.casualties(defender, t, e, false) + moon - NewBloodExpansion.ignored(attacker))

            // Each side picks its casualties when it has a choice (ChooseCasualties), before anything is told
            val pick = pickCasualties($(attacker -> math.min(dc, au), defender -> math.min(ac, du)), t, CombatResolveAction(attacker, defender, a, e, food, faces, then))

            if (pick.any)
                pick.get
            else {
                // The Wise One (Lynx): 1 point per casualty inflicted
                val wise = (e.special == WiseOneMove).??(math.min(ac, du))

                // Events: Conquests gives the attacker 1 point
                val conquests = game.eventIs("conquests").??(1)

                // Robotos: 1 more combat point when it attacks
                val robo = game.robotos(attacker).??(1)

                val as = game.strength(t, attacker, true) + e.bonus + ab + anb + aw + wise + conquests + robo + food(0) + faces(0).points
                val ds = game.strength(t, defender, false) + fortress + db + dnb + dw + food(1) + faces(1).points

                if (game.kaijaIn(t, attacker) || game.kaijaIn(t, defender))
                    game.note("kaija-fight")
                if (ab + db > 0)
                    game.note("scorched-fight")

                def extra(l : (Int, Elem)*) : Elem = l.toList.filter(_._1 > 0).map { case (n, what) => "(" ~ n.hl ~ " from " ~ what ~ ")" }.join(" ")

                if (game.chiefIn(t, attacker) || game.chiefIn(t, defender))
                    game.note("chief-fight")

                attacker.log("scored", CombatIcon.axes(as), extra((FightPoints.others(attacker, t, true) ++ $(e.bonus -> "the card".txt, ab -> "Scorched Earth".hl) ++ NewBloodExpansion.pointParts(attacker, t, e, true, food(0)) ++ aws ++ $(wise -> "The Wise One".hl, conquests -> "Conquests".hl, robo -> "Robotos".hl)) : _*), (ac > 0 || halvard > 0).?("and inflicted " ~ CombatIcon.skulls(ac)).|(Empty), (halvard > 0).?("(" ~ 1.hl ~ " ignored by " ~ Warchief.elem(Goat) ~ ")").|(Empty))
                defender.log("scored", CombatIcon.axes(ds), extra((FightPoints.others(defender, t, false) ++ $(fortress -> Fortress.elem, db -> "Scorched Earth".hl) ++ NewBloodExpansion.pointParts(defender, t, e, false, food(1)) ++ dws) : _*), (dc > 0 || shield > 0).?("and inflicted " ~ CombatIcon.skulls(dc)).|(Empty), extra(towers -> DefenseTower.elem), (shield > 0).?("(" ~ 1.hl ~ " cancelled by " ~ "Shieldbearers".hl ~ ")").|(Empty))

                val winner =
                    if (ac >= du && dc >= au) None
                    else if (ac >= du) |(attacker)
                    else if (dc >= au) |(defender)
                    else if (as > ds) |(attacker)
                    else |(defender)

                // The combat report, from the same sums
                def report(f : Faction, attacking : Boolean, extra : $[(Int, Elem)], casualties : $[(Int, Elem)], total : Int, inflicted : Int, lost : Int) =
                    FightSide(f, attacking, FightPoints.parts(f, t, attacking, extra), total, casualties, inflicted, lost)

                game.fightReport = |(FightReport(a,
                    report(attacker, true, $(e.bonus -> "the card".txt, ab -> "Scorched Earth".hl) ++ NewBloodExpansion.pointParts(attacker, t, e, true, food(0)) ++ aws ++ $(wise -> "The Wise One".hl, conquests -> "Conquests".hl, robo -> "Robotos".hl, food(0) -> Food.elem, faces(0).points -> "the die".txt),
                        $(faces(0).casualties -> "the die".txt, (e.special == EgilMove).??(1) -> "the card".txt) ++ NewBloodExpansion.casualtyParts(attacker, t, e, true) ++ $(eldrich -> "Eldrich".hl, moon -> "Blood Moon".hl,
                            -halvard -> ("ignored by " ~ Warchief.elem(Goat)), -NewBloodExpansion.ignored(defender) -> ("ignored by " ~ "Ancestral Equipment".hl), -urdar -> "ignored by Urdarbrunn".hl),
                        as, ac, math.min(dc, au)),
                    report(defender, false, $(fortress -> Fortress.elem, db -> "Scorched Earth".hl) ++ NewBloodExpansion.pointParts(defender, t, e, false, food(1)) ++ dws ++ $(food(1) -> Food.elem, faces(1).points -> "the die".txt),
                        $(faces(1).casualties -> "the die".txt, towers -> DefenseTower.elem) ++ NewBloodExpansion.casualtyParts(defender, t, e, false) ++ $(moon -> "Blood Moon".hl,
                            -shield -> ("cancelled by " ~ "Shieldbearers".hl), -NewBloodExpansion.ignored(attacker) -> ("ignored by " ~ "Ancestral Equipment".hl)),
                        ds, dc, math.min(ac, du)),
                    winner./(_ == attacker), winner.exists(w => (w == attacker).?(ac >= du).|(dc >= au))))

                val readers = CombatReport.readers(|(attacker), |(defender))

                val before = game.count(t, attacker)
                val dbefore = game.count(t, defender)
                val specials = game.specialsIn(t, attacker)
                game.removeCasualties(t, attacker, math.min(dc, au))
                game.removeCasualties(t, defender, math.min(ac, du))

                // Events (Blood Moon, Conquests) and the Alternative victory counts
                EventsExpansion.afterCombat(attacker, defender, math.min(dc, au), math.min(ac, du), winner)

                // Sacrificial Pyre, Blood Ties, Howl from the Sea
                NewBloodExpansion.afterCombat(attacker, defender, t, e, before - game.count(t, attacker), dbefore - game.count(t, defender), math.min(dc, au), math.min(ac, du), winner)

                // Sea module: Bold Maneuver gives 2 fame per combat won
                if (e.special == BoldMove && winner.has(attacker)) {
                    attacker.fame += 2
                    attacker.log("gained", 2.hl, FameIcon(), "for winning with", RaidCard("bold-maneuver"))
                }

                // Svarn's Menders: the attacker's casualties wait on the card
                if (e.special == SvarnMove) {
                    game.mended += before - game.count(t, attacker)
                    if (game.options.has(MendFigures))
                        game.mendedFigures ++= specials.diff(game.specialsIn(t, attacker))
                }

                // For the bot game summaries (host.scala)
                game.note(winner.has(attacker).?("attack-won").|("attack-lost"))

                def next(then : ForcedAction) = Then(CombatReportAction(readers, ClanPowers.report(then)))

                winner match {
                    case None =>
                        log("Both sides were", CombatReport.link("wiped out"))
                        next(then)

                    case Some(w) =>
                        val loser = (w == attacker).?(defender).|(attacker)

                        w.log(CombatReport.link("won"), "the fight in", a)

                        if (w == attacker)
                            attackWon(attacker, a)

                        if (game.figures(t, loser) > 0) {
                            // Brand's Bravery: the winning attacker chooses where the loser retreats
                            if (w == attacker && e.special == BrandMove)
                                game.retreatBy = |(attacker)

                            next(RetreatAction(loser, a, loser == attacker && e.ignoreRough, then))
                        }
                        else
                            next(then)
                }
            }

        case CasualtyPickAction(f, _, k, n, kaija, chief, blainn, then) =>
            game.casualtyPicks += f -> (n, kaija, chief, blainn)
            game.note((n < k).?("casualties-figure").|("casualties-units"))

            Then(then)

        // ReportsDontWait: each reader gets the report as a notification, and the game goes on
        case CombatReportAction(l, then) if game.options.has(ReportsDontWait) =>
            game.fightReport.foreach { r =>
                l.foreach(f => game.notify(f, $(ReportNote(f, true, CombatReport.text(r, |(f))))))
            }

            game.fightReport = None
            Then(then)

        case CombatReportAction(l, then) =>
            if (l.none) {
                game.fightReport = None
                Then(then)
            }
            else
                // needOk: an Ask with a single choice is otherwise taken by the runner without showing it
                Ask(l.head).add(CombatReportDoneAction(l.head, l.tail, then)).needOk

        case CombatReportDoneAction(_, rest, then) =>
            Then(CombatReportAction(rest, then))

        case ClanPowerReportAction(then) if game.options.has(ReportsDontWait) =>
            game.powerNotes./(_.f).distinct.foreach { f =>
                game.notify(f, $(ReportNote(f, false, ClanPowers.text(f) ~ Break ~ ClanPowers.button(f))))
                game.note("power-report")
            }

            game.powerNotes = $
            Then(then)

        case ClanPowerReportAction(then) =>
            game.powerNotes.headOption match {
                case Some(n) =>
                    // needOk: an Ask with a single choice is otherwise taken by the runner without showing it
                    Ask(n.f).add(ClanPowerReportDoneAction(n.f, then)).needOk
                case None =>
                    Then(then)
            }

        case ClanPowerReportDoneAction(f, then) =>
            game.note("power-report")
            game.powerNotes = game.powerNotes.%(_.f != f)
            Then(ClanPowerReportAction(then))

        case RetreatAction(f, a, rough, then) =>
            val t = game.board.territory(a)
            val n = game.count(t, f)
            val kaija = game.kaijaIn(t, f)

            if (n == 0 && kaija.not && game.chiefIn(t, f).not) {
                game.retreatBy = None
                Then(then)
            }
            else {
                val to = retreats(f, t, rough)

                if (to.none) {
                    val chief = game.chiefIn(t, f)
                    game.removeFigures(t, f, game.figures(t, f))
                    f.log("had nowhere to retreat and lost", Party(f, n, kaija, chief))
                    game.retreatBy = None
                    Then(then)
                }
                else
                if (game.retreatBy.any)
                    Ask(game.retreatBy.get).each(to)(o => BrandRetreatPickAction(game.retreatBy.get, f, a, o.anchor, rough, then))
                else
                    Ask(f).each(to)(o => RetreatPickAction(f, a, o.anchor, rough, then))
            }

        case RetreatPickAction(f, from, to, rough, then) =>
            retreatCount(Ask(f).each(retreatParties(f, from)) { case (n, kaija, chief) => RetreatPartyAction(f, from, to, n, kaija, chief, rough, then) }, f, from, rough)

        case BrandRetreatPickAction(self, f, from, to, rough, then) =>
            retreatCount(Ask(self).each(retreatParties(f, from)) { case (n, kaija, chief) => BrandRetreatPartyAction(self, f, from, to, n, kaija, chief, rough, then) }, f, from, rough)

        case BrandRetreatToAction(_, f, from, to, n, kaija, rough, then) =>
            Then(RetreatToAction(f, from, to, n, kaija, rough, then))

        case BrandRetreatPartyAction(_, f, from, to, n, kaija, chief, rough, then) =>
            Then(RetreatMoveAction(f, from, to, n, kaija, chief, rough, then))

        case RetreatPartyAction(f, from, to, n, kaija, chief, rough, then) =>
            Then(RetreatMoveAction(f, from, to, n, kaija, chief, rough, then))

        // Svarn's Menders: each casualty back in one of f's territories
        // and the warchief, companion and Jötunn Blainn first, when they were casualties too (MendFigures)
        case SvarnMendAction(f, then) =>
            val l = game.controlled(f)

            if (l.any && game.mendedFigures.any)
                Ask(f).each(l)(t => SvarnMendFigureToAction(f, t.anchor, game.mendedFigures.head, then))
            else
            if (game.mended <= 0 || l.none || game.reserve(f) <= 0) {
                game.mended = 0
                game.mendedFigures = $
                Then(then)
            }
            else
                Ask(f).each(l)(t => SvarnMendToAction(f, t.anchor, then))

        case SvarnMendFigureToAction(f, a, figure, then) =>
            game.mendedFigures = game.mendedFigures.drop(1)

            figure match {
                case "chief" => game.chiefs += f -> a
                case "companion" => game.setCompanion(f, |(a))
                case "blainn" => game.blainn = |((f, a))
            }

            game.note("svarn-mend-" + figure)

            f.log("placed", SvarnMendFigure(f, figure), "back in", a, "with", "Svarn's Menders".hl)

            Then(SvarnMendAction(f, then))

        case SvarnMendToAction(f, a, then) =>
            game.addUnits(a, f, 1)
            game.mended -= 1

            f.log("placed a", CombatIcon.skull, "back in", a, "with", "Svarn's Menders".hl)

            Then(SvarnMendAction(f, then))

        // Older games: the warchief goes with the first group
        case RetreatToAction(f, from, to, n, kaija, rough, then) =>
            Then(RetreatMoveAction(f, from, to, n, kaija, game.chiefIn(game.board.territory(from), f), rough, then))

        case RetreatMoveAction(f, from, to, n, kaija, chief, rough, then) =>
            game.removeUnits(game.board.territory(from), f, n)
            game.addUnits(to, f, n)
            if (kaija)
                game.setCompanion(f, |(to))
            if (chief)
                game.chiefs += f -> to

            f.log("retreated", Party(f, n, kaija, chief), "to", to)

            // Howl from the Sea (Kraken): a unit where a lost combat retreats
            NewBloodExpansion.howl(f, to)

            Then(RetreatAction(f, from, rough, then))

        // SNAKE CLAN
        case ScorchedAction(f, then) =>
            val l = scorchable(f)

            if (l.none)
                Then(then)
            else
                Ask(f).each(l)(t => ScorchedPlaceAction(f, t.anchor, then)).add(ScorchedSkipAction(f, then))

        case ScorchedPlaceAction(f, a, then) =>
            game.scorched = |(a)
            game.note("scorched-place")

            f.log("moved the", "Scorched Earth".hl, "token to", a)

            Then(then)

        case ScorchedSkipAction(f, then) =>
            Then(then)

        // EXPLORE
        case ExploreAction(f, draw, times, redraw, e, then) =>
            if (times <= 0 || game.pile.none || explorable(f, e.anywhere).none)
                Then(then)
            else
                Then(ExploreDrawAction(f, draw, game.pile.num, times, redraw, e, then))

        case ExploreDrawAction(f, draw, tries, times, redraw, e, then) =>
            if (game.exploring.num >= draw || game.pile.none || tries <= 0)
                Then(ExploreChooseAction(f, times, redraw, e, then))
            else {
                val tile = game.pile.head
                game.pile = game.pile.drop(1)

                if (placements(tile, |(explorable(f, e.anywhere)), false).any) {
                    game.exploring :+= tile
                    Then(ExploreDrawAction(f, draw, tries - 1, times, redraw, e, then))
                }
                else {
                    game.pile :+= tile
                    log("A tile that could not be placed went to the bottom of the pile")
                    Then(ExploreDrawAction(f, draw, tries - 1, times, redraw, e, then))
                }
            }

        case ExploreChooseAction(f, times, redraw, e, then) =>
            if (game.exploring.none) {
                f.log("found no tile to explore with")
                Then(then)
            }
            else {
                // A log line of its own (Robotos, 2026-10-21): "Undo to here" on it goes back to the tile drawn but not yet
                // placed, which Limited undo allows, while the line before was the card played before the draw
                if (game.exploring.num == 1)
                    f.log("drew a tile to explore")
                else
                    f.log("drew", game.exploring.num.hl, "tiles to explore")

                Ask(f).each(game.exploring)(t => ExploreTileAction(f, t, times, e, then))
                    .when(redraw && game.exploring.num == 1 && game.pile.any)(ExploreRedrawAction(f, game.exploring.head, times, e, then))
            }

        case ExploreRedrawAction(f, tile, times, e, then) =>
            game.exploring = $
            game.pile :+= tile

            f.log("put a tile at the bottom of the pile")

            Then(ExploreDrawAction(f, 1, game.pile.num, times, false, e, then))

        case ExploreTileAction(f, tile, times, e, then) =>
            Ask(f).add(ExploreTileInfoAction(f, tile)).each(placements(tile, |(explorable(f, e.anywhere)), false).map(_._1).distinct)(s => ExploreSpotAction(f, tile, s, times, e, then)).cancel

        case ExploreSpotAction(f, tile, spot, times, e, then) =>
            val rs = rotations(placements(tile, |(explorable(f, e.anywhere)), false), spot)

            explorePreview(f, tile, spot, rs, rs.head, times, e, then)

        case ExploreRotateAction(f, tile, spot, r, d, times, e, then) =>
            val rs = rotations(placements(tile, |(explorable(f, e.anywhere)), false), spot)

            explorePreview(f, tile, spot, rs, rotate(rs, r, d), times, e, then)

        case ExploreTurnAction(f, tile, spot, r, times, e, then) =>
            val before = game.board.territories.%(game.board.open)
            val rest = game.exploring.diff($(tile))

            game.exploring = $
            game.pile ++= rest

            if (rest.any)
                log("The other tiles went to the bottom of the pile")

            // Lay of the Land: collect everything shown on the tile
            if (e.collect) {
                val spec = Tiles(tile)
                val (food, wood, lore) = (spec.areas./(_.food).sum, spec.areas./(_.wood).sum, spec.areas./(_.lore).sum)
                f.food += food
                f.wood += wood
                f.lore += lore
                f.log("collected", food.hl, Food, Comma, wood.hl, Wood, "and", lore.hl, Lore, "from the tile")
            }

            game.board.place(Placement(tile, spot.x, spot.y, r))

            f.log("explored")

            // Territories this tile closed, and those of them f controls
            val closing = game.board.territories.%(game.board.closed).%(t => before.exists(b => b.areas.exists(t.areas.contains)))
            val closed = closing.%(t => game.present(t) == $(f))

            closed.foreach { t =>
                val n = game.board.tiles(t)
                f.fame += n
                f.log("closed", t.anchor, "and gained", n.hl, FameIcon())

                if (f == Stag) {
                    f.fame += 1
                    f.log("gained", 1.hl, "more", FameIcon(), "for closing a territory")
                    ClanPowers.note(PowerNote(f, |(t.anchor), f.elem ~ " gains 1 fame for each territory it closes.", fame = 1))
                }

                if (f == Raven) {
                    val (food, wood, lore) = collect(f, t, "from the closed territory")
                    if (food + wood + lore > 0)
                        ClanPowers.note(PowerNote(f, |(t.anchor), f.elem ~ " collects the resources of its own territories closed by exploring.", food, wood, lore))
                }
            }

            // Boar Clan: exploring without closing any territory
            if (closing.none && f == Boar) {
                f.lore += 1
                f.log("collected", 1.hl, Lore, "for exploring without closing a territory")
                ClanPowers.note(PowerNote(f, None, f.elem ~ " collects 1 lore for exploring without closing a territory.", lore = 1))
            }

            game.explored = |(tile)

            // Events: New Horizons; Alternative victory: territories closed
            EventsExpansion.explored(f, closing, closed)

            // New Blood: Ox Clan's Ancestral Equipment token on the tile, Rat Clan's units in closed territories
            if (game.has(NewBlood))
                NewBloodExpansion.explored(f, tile, spot, closed)

            // The clan power report once the tile is placed (and any creature on it has appeared)
            val next = ClanPowers.report(ExploreAction(f, 1, times - 1, false, e, then))

            // Horse Clan: 1 wood or a small building for each closed territory
            val horse = (f == Horse && closed.any).?(HorseClosedAction(f, closed./(_.anchor), next) : ForcedAction).|(next)

            // New Blood: Ox Clan picks the space for its token (OxPicksSpace)
            val ox = (f == Ox && game.options.has(OxPicksSpace) && game.gearPile.any).?(GearPlaceAskAction(f, tile, spot, horse) : ForcedAction).|(horse)

            Then(TilePlacedAction(f, tile, spot, false, ox))

        // BUILD
        case BuildAction(f, e, times, smallOnly, then) =>
            val spots = (times > 0).??(buildSpots(f, e, smallOnly))
            // Minerals module: one building upgraded with Stone, whether or not anything is built
            val upgrades = MineralsExpansion.upgrades(f, BuildAction(f, e, times, smallOnly, then))

            if (spots.none && upgrades.none)
                Then(BuildFinishAction(f, e, then))
            else
                Ask(f).each(spots)(s => BuildSpotAction(f, s, times, e, smallOnly, then))
                    .add(upgrades)
                    .add(BuildDoneAction(f, e, then))

        // The menu: every building with its cost, the ones that can't go on this space dimmed
        case BuildSpotAction(f, s, times, e, smallOnly, then) =>
            Ask(f).each(MineralsExpansion.buildings)(b => BuildPickAction(f, s, b, buildCost(b, e), times, e, smallOnly, then).!(buildBlock(f, e, smallOnly, s, b).any, buildBlock(f, e, smallOnly, s, b).|("")))
                .cancel

        case BuildPickAction(f, s, b, cost, times, e, smallOnly, then) =>
            Ask(f).add(BuildConfirmAction(f, game.board.territory(s.area).anchor, b, s, cost, times, e, smallOnly, then)).cancel

        case BuildConfirmAction(f, a, b, s, cost, times, e, smallOnly, then) =>
            Force(BuildPlaceAction(f, a, b, s, cost, times, e, smallOnly, then))

        // The spaces the building fits, the same ones buildOptions found
        case BuildSlotAction(f, a, b, cost, times, e, smallOnly, then) =>
            val spaces = buildOptions(f, e, smallOnly).%(x => x._1 == a && x._2 == b)./~(_._3)

            Ask(f).each(spaces)(s => BuildSpaceAction(f, a, b, s, spaceKind(s), cost, times, e, smallOnly, then)).cancel

        case BuildSpaceAction(f, a, b, s, _, cost, times, e, smallOnly, then) =>
            Then(BuildPlaceAction(f, a, b, s, cost, times, e, smallOnly, then))

        case BuildPlaceAction(f, a, b, s, cost, times, e, smallOnly, then) =>
            f.wood -= cost
            game.buildings += s -> b

            f.log("built", b, "in", a)

            if (f == Goat) {
                val n = b.large.?(2).|(1)
                f.food += n
                f.log("collected", n.hl, Food, "for building")
                ClanPowers.note(PowerNote(f, |(a), f.elem ~ " collects food for each building: 1 for a small one, 2 for a large one.", food = n))
            }

            // Glory of the Clan: collect the territory's resources as at harvest
            if (e.special == GloryBuild)
                collect(f, game.board.territory(a), "from the territory")

            // Carpentry Mastery: once a large building is built, the other must be small
            Then(BuildAction(f, e, times - 1, smallOnly || (e.special == CarpentryBuild && b.large), then))

        case BuildDoneAction(f, e, then) =>
            Then(BuildFinishAction(f, e, then))

        case BuildFinishAction(f, e, then) =>
            game.stoneUsed = false

            // Industrious Villagers: may replace one of f's buildings by another of the same size
            val l = (e.special == IndustriousBuild).??(replacements(f))

            if (l.none)
                Then(ClanPowers.report(then))
            else
                replaceAsk(f, "Industrious Villagers", l, ClanPowers.report(then)).add(ReplaceSkipAction(f, ClanPowers.report(then)))

        case ReplaceBuildingAction(f, a, s, b, then) =>
            val old = game.buildings(s)
            game.buildings += s -> b

            f.log("replaced", old, "with", b, "in", a)

            Then(then)

        case ReplaceSkipAction(f, then) =>
            Then(then)

        // The new buildings for the tapped one (the replacement itself, ReplaceBuildingAction or CraftsmenReplaceAction, is what the game records)
        case ReplaceSpotAction(f, source, s, then) =>
            replaceChoices(f, source, replacements(f).filter(_._1 == s), then).cancel

        // FEAST
        case FeastChoiceAction(f, e, then) =>
            resolve(f, e, then)

        // NO UNITS LEFT
        case ReturnUnitsAction(f, then) =>
            val neutral = game.board.territories.%(t => game.present(t).none).%(t => game.hostileIn(t).not).%(t => game.swampIn(t).not)

            if (game.anyOnMap(f))
                Then(then)
            else
            if (neutral.none)
                Then(SecondChanceDrawAction(f, game.pile.num, then))
            else
                Ask(f).each(neutral)(t => ReturnUnitsToAction(f, t.anchor, then))

        // A tile with an empty territory, placed anywhere it fits
        case SecondChanceDrawAction(f, tries, then) =>
            if (tries <= 0 || game.pile.none) {
                f.log("found no tile to make a neutral territory")
                Then(then)
            }
            else {
                val tile = game.pile.head
                game.pile = game.pile.drop(1)

                val l = placements(tile, None, true)

                if (l.none) {
                    game.pile :+= tile
                    Then(SecondChanceDrawAction(f, tries - 1, then))
                }
                else
                    Ask(f).each(l.map(_._1).distinct)(s => SecondChanceSpotAction(f, tile, s, then))
            }

        case SecondChanceSpotAction(f, tile, spot, then) =>
            val rs = rotations(placements(tile, None, true), spot)

            Ask(f)
                .when(rs.num > 1)(SecondChanceRotateAction(f, tile, spot, rs.head, 1, then))
                .when(rs.num > 1)(SecondChanceRotateAction(f, tile, spot, rs.head, -1, then))
                .add(SecondChanceTurnAction(f, tile, spot, rs.head, then))

        case SecondChanceRotateAction(f, tile, spot, r, d, then) =>
            val rs = rotations(placements(tile, None, true), spot)
            val n = rotate(rs, r, d)

            Ask(f)
                .when(rs.num > 1)(SecondChanceRotateAction(f, tile, spot, n, 1, then))
                .when(rs.num > 1)(SecondChanceRotateAction(f, tile, spot, n, -1, then))
                .add(SecondChanceTurnAction(f, tile, spot, n, then))

        case SecondChanceTurnAction(f, tile, spot, r, then) =>
            game.board.place(Placement(tile, spot.x, spot.y, r))

            f.log("drew and placed a tile to make a neutral territory")

            Then(TilePlacedAction(f, tile, spot, false, ReturnUnitsAction(f, then)))

        case ReturnUnitsToAction(f, a, then) =>
            game.addUnits(a, f, math.min(3, game.reserve(f)))

            f.log("placed three new units in", a)

            Then(then)

        case _ => UnknownContinue
    }
}
