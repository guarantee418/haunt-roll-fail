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

// A tile being placed, shown on the map at its spot until confirmed
trait TilePreview {
    def tile : String
    def spot : Spot
    def r : Int
}

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
case class SetupRotateAction(self : Faction, round : Int, l : $[Faction], tile : String, spot : Spot, r : Int, d : Int) extends BaseAction("Place the tile at", spot)(RotateLabel(d)) with Soft
case class SetupTurnAction(self : Faction, round : Int, l : $[Faction], tile : String, spot : Spot, r : Int) extends BaseAction("Place the tile at", spot)("Confirm") with TilePreview
case class SetupUnitsAction(self : Faction, round : Int, l : $[Faction], area : AreaRef) extends BaseAction("Place three units in")(area) with MapTarget { def target = area }
case class SetupKaijaAction(self : Faction, round : Int, l : $[Faction], area : AreaRef) extends BaseAction("Place two units and Kaija in")(area)
case class ShuffledTilesBackAction(shuffled : $[String]) extends ShuffledAction[String]

// RECRUIT
case class RecruitAction(f : Faction, left : Int, mode : RecruitMode, placed : $[AreaRef], then : ForcedAction) extends ForcedAction
case class RecruitPlaceAction(self : Faction, area : AreaRef, left : Int, mode : RecruitMode, placed : $[AreaRef], then : ForcedAction) extends BaseAction("Recruit", (left > 1).?("(" ~ left.hl ~ " left)").|(""), "in")(area) with MapTarget { def target = area }
case class RecruitKaijaAction(self : Faction, area : AreaRef, left : Int, mode : RecruitMode, placed : $[AreaRef], then : ForcedAction) extends BaseAction("Recruit", "Kaija".hl, "in")(area)
case class RecruitDoneAction(self : Faction, placed : $[AreaRef], then : ForcedAction) extends BaseAction("Recruit")("Done")
case class TrainingCampsAction(f : Faction, placed : $[AreaRef], then : ForcedAction) extends ForcedAction

// MOVE
case class MoveAction(f : Faction, left : Int, e : MoveEffect, then : ForcedAction) extends ForcedAction
case class MoveFromAction(self : Faction, from : AreaRef, left : Int, e : MoveEffect, then : ForcedAction) extends BaseAction("Move", "(" ~ left.hl ~ " left)", "from")(from) with Soft with MapTarget { def target = from }
case class MoveToAction(self : Faction, from : AreaRef, to : AreaRef, cost : Int, left : Int, e : MoveEffect, then : ForcedAction) extends BaseAction("Move from", from, "to")(to, (cost > 1).?("(Rough border)").|("")) with Soft with MapTarget { def target = to }
case class MoveUnitsAction(self : Faction, from : AreaRef, to : AreaRef, n : Int, kaija : Boolean, cost : Int, left : Int, e : MoveEffect, then : ForcedAction) extends BaseAction("Move from", from, "to", to)(Figures(n, kaija))
case class MoveDoneAction(self : Faction, e : MoveEffect, then : ForcedAction) extends BaseAction("Move")("Done")
case class CombatsAction(f : Faction, e : MoveEffect, then : ForcedAction) extends ForcedAction
case class FightAction(self : Faction, area : AreaRef, e : MoveEffect, then : ForcedAction) extends BaseAction("Fight in")(area) with MapTarget { def target = area }
case class FightStartAction(f : Faction, area : AreaRef, e : MoveEffect, then : ForcedAction) extends ForcedAction
case class IntimidateAction(self : Faction, area : AreaRef, to : AreaRef, e : MoveEffect, then : ForcedAction) extends BaseAction("Intimidate", "push a defending unit from", area, "to")(to) with MapTarget { def target = to }
case class IntimidateSkipAction(self : Faction, area : AreaRef, e : MoveEffect, then : ForcedAction) extends BaseAction("Intimidate")("Fight them all")
case class AxeAction(self : Faction, defender : Faction, area : AreaRef, e : MoveEffect, food : $[Int], face : DieFace, then : ForcedAction) extends BaseAction("Axe Throwers")(face)

// COMBAT
case class CombatFoodAction(self : Faction, attacker : Faction, defender : Faction, area : AreaRef, e : MoveEffect, food : $[Int], then : ForcedAction) extends BaseAction(self, "spends food for the fight")((food.last == 0).?("No food").|(food.last.hl ~ " " ~ Food.elem))
case class CombatRollAction(attacker : Faction, defender : Faction, area : AreaRef, e : MoveEffect, food : $[Int], faces : $[DieFace], then : ForcedAction) extends ForcedAction
case class CombatRolledAction(attacker : Faction, defender : Faction, area : AreaRef, e : MoveEffect, food : $[Int], faces : $[DieFace], random : DieFace, then : ForcedAction) extends RandomAction[DieFace]
case class CombatChooseAction(self : Faction, attacker : Faction, defender : Faction, area : AreaRef, e : MoveEffect, food : $[Int], faces : $[DieFace], choice : DieFace, then : ForcedAction) extends BaseAction(self, "rolled", DieChoice)(choice)
case class CombatResolveAction(attacker : Faction, defender : Faction, area : AreaRef, e : MoveEffect, food : $[Int], faces : $[DieFace], then : ForcedAction) extends ForcedAction
// rough: the retreating units may cross Rough borders (Wolf Clan card)
case class RetreatAction(f : Faction, from : AreaRef, rough : Boolean, then : ForcedAction) extends ForcedAction
case class RetreatToAction(self : Faction, from : AreaRef, to : AreaRef, n : Int, kaija : Boolean, rough : Boolean, then : ForcedAction) extends BaseAction("Retreat from", from, "to")(to, "(" ~ Figures(n, kaija).elem ~ ")") with MapTarget { def target = to }

// SNAKE CLAN
case class ScorchedAction(f : Faction, then : ForcedAction) extends ForcedAction
case class ScorchedPlaceAction(self : Faction, area : AreaRef, then : ForcedAction) extends BaseAction("Scorched Earth".hl, "token to")(area) with MapTarget { def target = area }
case class ScorchedSkipAction(self : Faction, then : ForcedAction) extends BaseAction("Scorched Earth".hl)("Leave the token where it is")

// EXPLORE
case class ExploreAction(f : Faction, draw : Int, times : Int, redraw : Boolean, e : ExploreEffect, then : ForcedAction) extends ForcedAction
case class ExploreDrawAction(f : Faction, draw : Int, tries : Int, times : Int, redraw : Boolean, e : ExploreEffect, then : ForcedAction) extends ForcedAction
case class ExploreChooseAction(f : Faction, times : Int, redraw : Boolean, e : ExploreEffect, then : ForcedAction) extends ForcedAction
case class ExploreTileAction(self : Faction, tile : String, times : Int, e : ExploreEffect, then : ForcedAction) extends BaseAction("Explore with")(TileRef(tile)) with Soft with ViewObject[TileRef] { def obj = TileRef(tile) }
case class ExploreSpotAction(self : Faction, tile : String, spot : Spot, times : Int, e : ExploreEffect, then : ForcedAction) extends BaseAction("Place the tile at")(spot) with Soft with MapTarget { def target = spot }
case class ExploreRotateAction(self : Faction, tile : String, spot : Spot, r : Int, d : Int, times : Int, e : ExploreEffect, then : ForcedAction) extends BaseAction("Place the tile at", spot)(RotateLabel(d)) with Soft
case class ExploreTurnAction(self : Faction, tile : String, spot : Spot, r : Int, times : Int, e : ExploreEffect, then : ForcedAction) extends BaseAction("Place the tile at", spot)("Confirm") with TilePreview
case class ExploreRedrawAction(self : Faction, tile : String, times : Int, e : ExploreEffect, then : ForcedAction) extends BaseAction("Scout Camp")("Put", TileRef(tile), "at the bottom and draw another")

// BUILD
// smallOnly: Carpentry Mastery after a large building
case class BuildAction(f : Faction, e : BuildEffect, times : Int, smallOnly : Boolean, then : ForcedAction) extends ForcedAction
case class BuildPlaceAction(self : Faction, area : AreaRef, building : Building, space : SpaceRef, cost : Int, times : Int, e : BuildEffect, smallOnly : Boolean, then : ForcedAction) extends BaseAction("Build in", area)(building, "(" ~ (cost == 0).?("free".txt).|(cost.hl ~ " " ~ Wood.elem) ~ ")") with MapTarget { def target = area }
case class BuildDoneAction(self : Faction, e : BuildEffect, then : ForcedAction) extends BaseAction("Build")("Done")
case class BuildFinishAction(f : Faction, e : BuildEffect, then : ForcedAction) extends ForcedAction
case class ReplaceBuildingAction(self : Faction, area : AreaRef, space : SpaceRef, building : Building, then : ForcedAction) extends BaseAction("Industrious Villagers", "replace a building in", area, "with")(building) with MapTarget { def target = area }
case class ReplaceSkipAction(self : Faction, then : ForcedAction) extends BaseAction("Industrious Villagers")("Keep the buildings")

// FEAST
case class FeastChoiceAction(self : Faction, effect : Effect, then : ForcedAction) extends BaseAction("Feast")(FeastLabel(effect))

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


// Some units, maybe with Kaija
case class Figures(n : Int, kaija : Boolean) extends Elementary with Record {
    def elem : Elem = ((n > 0).$((n == 1).?("1 unit").|(n.toString + " units")) ++ kaija.$("Kaija")).mkString(" and ").txt
}


// The Northgard die
case class DieFace(points : Int, casualties : Int, choice : Boolean) extends Elementary with Record {
    def elem : Elem =
        if (choice) "1 point or 1 casualty".hl
        else $(
            (points > 0).?(points.hl ~ " " ~ (points == 1).?("point").|("points")),
            (casualties > 0).?(casualties.hl ~ " " ~ (casualties == 1).?("casualty").|("casualties"))
        ).flatten.join(" and ")
}

case object DieChoice extends Elementary {
    def elem = "1 point or 1 casualty".hl ~ ", taking"
}

object NorthgardDie {
    val faces : $[DieFace] = $(DieFace(2, 0, false), DieFace(3, 0, false), DieFace(0, 0, true), DieFace(2, 1, false), DieFace(0, 2, false), DieFace(1, 1, false))

    val point = DieFace(1, 0, false)
    val casualty = DieFace(0, 1, false)
}


object MapExpansion extends Expansion {
    def recruitTargets(f : Faction, mode : RecruitMode, placed : $[AreaRef])(implicit game : Game) : $[Territory] = {
        val neutral = game.board.territories.%(t => game.present(t).none)
        val mine = game.controlled(f)

        val none = game.anyOnMap(f).not

        val l = mode match {
            case RecruitNormal => none.?(neutral).|(mine)
            case RecruitSame => none.?(neutral).|(mine)
            case RecruitNeutral => mine ++ neutral
            case RecruitNeutralOnly => neutral
            case RecruitNeutralSame => neutral
            case RecruitSameAny => mine ++ neutral
            case RecruitOsmosis => mine.%(t => game.board.open(t) || t.areas.exists(a => game.board.spec(a).wood > 0)).%(t => placed.forall(p => t.areas.contains(p).not))
        }

        val same = mode @@ {
            case RecruitSame | RecruitNeutralSame | RecruitSameAny => true
            case _ => false
        }

        if (same && placed.any)
            l.%(t => t.areas.contains(placed.head) || game.board.territory(placed.head) == t)
        else
            l
    }

    // Territories f can move out of: their own, not fighting; with Infiltration also enemy territories they moved into
    def moveSources(f : Faction, e : MoveEffect = MoveEffect(1))(implicit game : Game) =
        (e.special == InfiltrateMove).?(game.board.territories.%(t => game.figures(t, f) > 0)).|(game.controlled(f))

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
    def placements(tile : String, near : Option[$[Territory]], setup : Boolean)(implicit game : Game) : $[(Spot, Int)] = {
        val board = game.board

        val spots = board.frontier.%{ case (x, y) =>
            near.forall(l => Side.all.exists { s =>
                board.areaAt(x + s.dx, y + s.dy, s.opposite).exists(a => l.contains(board.territory(a)))
            })
        }

        spots./~{ case (x, y) =>
            0.until(4)./~{ r =>
                val p = Placement(tile, x, y, r)
                val all = board.placements :+ p
                if (board.consistent(all).not)
                    None
                else {
                    val preview = board.preview(p)
                    val mixed = preview.exists(t => t.areas./~(a => game.unitsAt(a).keys).distinct.num > 1)
                    val room = setup.not || p.spec.areas.exists(a => preview.find(_.areas.contains(AreaRef(x, y, a.id))).get.areas.forall(b => game.unitsAt(b).isEmpty))
                    (mixed.not && room).?(Spot(x, y) -> r)
                }
            }
        }
    }

    def explorable(f : Faction, anywhere : Boolean)(implicit game : Game) : $[Territory] =
        anywhere.?(game.board.territories.%(game.board.open)).|(game.controlled(f).%(game.board.open))

    def buildOptions(f : Faction, e : BuildEffect, smallOnly : Boolean)(implicit game : Game) : $[(AreaRef, Building, SpaceRef, Int)] = {
        game.controlled(f)./~{ t =>
            val here = game.buildingsIn(t).map(_._2)
            val free = t.areas./~(a => game.board.spec(a).spaces.indices./(i => SpaceRef(a, i))).%(s => game.buildings.contains(s).not)

            Building.all.%(b => e.duplicate || here.has(b).not).%(b => game.buildings.values.count(_ == b) < Building.tokens).%(b => smallOnly.not || b.large.not)./~{ b =>
                val kinds = b match {
                    case CarvedStone => $(CarvedSpace)
                    case b if b.large => $(LargeSpace)
                    case _ => $(SmallSpace, CarvedSpace)
                }
                // Keep Carved Stone spaces for Carved Stones when possible
                val space =
                    // Amenities: small buildings take no space
                    if (e.special == AmenitiesBuild && b.large.not)
                        |(SpaceRef(t.anchor, SpaceRef.extra + game.buildings.keys.count(s => s.area == t.anchor && s.index >= SpaceRef.extra)))
                    else
                        kinds./~(k => free.%(s => game.board.spec(s.area).spaces(s.index).kind == k)).headOption
                val cost = math.max(0, b.cost - e.discount)
                space.%(_ => f.wood >= cost)./(s => (t.anchor, b, s, cost))
            }
        }
    }

    // Industrious Villagers: f's buildings and what each could become
    def replacements(f : Faction)(implicit game : Game) : $[(SpaceRef, Building)] =
        game.controlled(f)./~(game.buildingsIn)./~{ case (s, old) =>
            val carved = s.index >= SpaceRef.extra || game.board.spec(s.area).spaces(s.index).kind == CarvedSpace
            Building.all.%(_ != old).%(_.large == old.large).%(b => b != CarvedStone || carved).%(b => game.buildings.values.count(_ == b) < Building.tokens)./(b => s -> b)
        }

    def canRecruit(f : Faction)(implicit game : Game) = game.reserve(f) > 0 || game.kaijaReady(f)

    // Closed territories of 3 or more tiles, for Protector of the Land
    def bigClosed(f : Faction)(implicit game : Game) = game.controlled(f).%(game.board.closed).%(t => game.board.tiles(t) >= 3)

    // Enemy territories next to f's, where the Scorched Earth token can go
    def scorchable(f : Faction)(implicit game : Game) : $[Territory] = {
        val mine = game.controlled(f)
        game.board.territories.%(t => game.present(t).but(f).any && game.present(t).has(f).not)
            .%(t => mine.exists(m => game.board.adjacent(m).exists(_._1 == t)))
            .%(t => game.scorchedIn(t).not)
    }

    def playable(f : Faction, e : Effect)(implicit game : Game) : Boolean = e match {
        case RecruitEffect(_, mode) => canRecruit(f) && recruitTargets(f, mode, $).any
        case AwakenEffect => true
        case ProtectorEffect => bigClosed(f).any && CommonExpansion.available(f) > 0
        case MoveEffect(_, _, _, _) => moveSources(f).any
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
        case e : MoveEffect => Then(MoveAction(f, e.n, e, then))
        case e : ExploreEffect => Then(ExploreAction(f, e.draw, e.times, e.redraw, e, then))
        case e : BuildEffect => Then(BuildAction(f, e, e.times, false, then))
        case FeastEffect =>
            Ask(f).each($(RecruitEffect(1), MoveEffect(1), ExploreEffect(), BuildEffect()).%(playable(f, _)))(e => FeastChoiceAction(f, e, then))
        case e => CardsExpansion.resolve(f, e, then)
    }

    // A die result is in; Axe Throwers adds 1 point or 1 casualty to the attacker's
    def rolled(attacker : Faction, defender : Faction, a : AreaRef, e : MoveEffect, food : $[Int], faces : $[DieFace], face : DieFace, then : ForcedAction)(implicit game : Game) : Continue =
        if (faces.none && e.special == AxeMove)
            Ask(attacker).add(AxeAction(attacker, defender, a, e, food, face.copy(points = face.points + 1), then)).add(AxeAction(attacker, defender, a, e, food, face.copy(casualties = face.casualties + 1), then))
        else
            Then(CombatRollAction(attacker, defender, a, e, food, faces :+ face, then))

    // Collect what a territory produces
    def collect(f : Faction, t : Territory, reason : Elem)(implicit game : Game) {
        val (food, wood, lore) = game.produce(t)
        f.food += food
        f.wood += wood
        f.lore += lore
        if (food + wood + lore > 0)
            f.log("collected", $(food -> Food, wood -> Wood, lore -> Lore).filter(_._1 > 0)./{ case (n, r) => n.hl ~ " " ~ r.elem }.join(", "), reason)
    }

    def perform(action : Action, soft : Void)(implicit game : Game) = action @@ {
        // SETUP
        case ShuffledTilesAction(l) =>
            game.pile = l

            game.board.place(Placement("start", 0, 0, 0))

            if (factions.num >= 5)
                game.board.place(Placement("start-5", 1, 0, 0))

            game.factions.foreach { f =>
                game.tileHand += f -> game.pile.take(3)
                game.pile = game.pile.drop(3)
            }

            log("Each player drew three map tiles")

            Then(SetupPlaceAction(1, game.from(game.first)))

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
            val starts = game.board.territories.%(t => t.areas.exists(a => game.board.at(a.x, a.y).get.tile.startsWith("start")))
            val near = (round == 1).?(starts)

            Ask(f).each(game.tileHand(f).%(t => placements(t, near, true).any))(t => SetupTileAction(f, round, f :: rest, t))
                .bailHard(SetupPlaceAction(round, rest))

        case SetupTileAction(f, round, l, tile) =>
            val starts = game.board.territories.%(t => t.areas.exists(a => game.board.at(a.x, a.y).get.tile.startsWith("start")))
            val near = (round == 1).?(starts)

            Ask(f).each(placements(tile, near, true).map(_._1).distinct)(s => SetupSpotAction(f, round, l, tile, s)).cancel

        case SetupSpotAction(f, round, l, tile, spot) =>
            val starts = game.board.territories.%(t => t.areas.exists(a => game.board.at(a.x, a.y).get.tile.startsWith("start")))
            val near = (round == 1).?(starts)
            val rs = rotations(placements(tile, near, true), spot)

            setupPreview(f, round, l, tile, spot, rs, rs.head)

        case SetupRotateAction(f, round, l, tile, spot, r, d) =>
            val starts = game.board.territories.%(t => t.areas.exists(a => game.board.at(a.x, a.y).get.tile.startsWith("start")))
            val near = (round == 1).?(starts)
            val rs = rotations(placements(tile, near, true), spot)

            setupPreview(f, round, l, tile, spot, rs, rotate(rs, r, d))

        case SetupTurnAction(f, round, l, tile, spot, r) =>
            game.tileHand += f -> game.tileHand(f).diff($(tile))
            game.board.place(Placement(tile, spot.x, spot.y, r))

            f.log("placed a tile")

            val empty = Tiles(tile).areas./(a => game.board.territory(AreaRef(spot.x, spot.y, a.id))).distinct.%(t => game.present(t).none)

            Ask(f).each(empty)(t => SetupUnitsAction(f, round, l, t.anchor))
                .some(empty.%(_ => game.kaijaReady(f)))(t => $(SetupKaijaAction(f, round, l, t.anchor)))

        case SetupUnitsAction(f, round, l, a) =>
            game.addUnits(a, f, 3)

            f.log("placed three units in", a)

            Then(SetupPlaceAction(round, l.drop(1)))

        case SetupKaijaAction(f, round, l, a) =>
            game.addUnits(a, f, 2)
            game.kaija = |(a)
            game.note("kaija-setup")

            f.log("placed two units and Kaija in", a)

            Then(SetupPlaceAction(round, l.drop(1)))

        // RECRUIT
        case RecruitAction(f, left, mode, placed, then) =>
            val targets = (left > 0 && canRecruit(f)).??(recruitTargets(f, mode, placed))

            if (targets.none)
                Then(TrainingCampsAction(f, placed, then))
            else
                Ask(f).some(targets)(t =>
                    (game.reserve(f) > 0).$(RecruitPlaceAction(f, t.anchor, left, mode, placed, then)) ++
                    game.kaijaReady(f).$(RecruitKaijaAction(f, t.anchor, left, mode, placed, then))
                )
                    .when(placed.any)(RecruitDoneAction(f, placed, then))

        case RecruitPlaceAction(f, a, left, mode, placed, then) =>
            game.addUnits(a, f, 1)

            f.log("recruited in", a)

            Then(RecruitAction(f, left - 1, mode, placed :+ a, then))

        case RecruitKaijaAction(f, a, left, mode, placed, then) =>
            game.kaija = |(a)
            game.note("kaija-recruit")

            f.log("recruited", "Kaija".hl, "in", a)

            Then(RecruitAction(f, left - 1, mode, placed :+ a, then))

        case RecruitDoneAction(f, placed, then) =>
            Then(TrainingCampsAction(f, placed, then))

        case TrainingCampsAction(f, placed, then) =>
            game.recruited = placed

            placed./(game.board.territory).distinct.foreach { t =>
                val camps = game.buildingsIn(t).count(_._2 == TrainingCamp)
                val n = math.min(camps, game.reserve(f))
                if (n > 0) {
                    game.addUnits(t.anchor, f, n)
                    f.log("recruited", n.hl, "more with", TrainingCamp, "in", t.anchor)
                }
            }

            Then(then)

        // MOVE
        case MoveAction(f, left, e, then) =>
            val sources = (left > 0).??(moveSources(f, e).%(t => game.board.adjacent(t).exists { case (_, regular) => moveCost(regular.not, e.ignoreRough) <= left }))

            if (sources.none)
                Then(CombatsAction(f, e, then))
            else
                Ask(f).each(sources)(t => MoveFromAction(f, t.anchor, left, e, then))
                    .add(MoveDoneAction(f, e, then))

        case MoveFromAction(f, from, left, e, then) =>
            val t = game.board.territory(from)

            Ask(f).some(game.board.adjacent(t)) { case (o, regular) =>
                val cost = moveCost(regular.not, e.ignoreRough)
                (cost <= left).$(MoveToAction(f, from, o.anchor, cost, left, e, then))
            }.cancel

        case MoveToAction(f, from, to, cost, left, e, then) =>
            val t = game.board.territory(from)
            val n = game.count(t, f)
            // Kaija can't enter enemy territories unless awakened
            val kaija = game.kaijaIn(t, f) && (game.awakened || game.present(game.board.territory(to)).but(f).none)

            Ask(f)
                .each(n.to(1, -1).$)(k => MoveUnitsAction(f, from, to, k, false, cost, left, e, then))
                .some(kaija.$(n.to(0, -1).$).flatten)(k => $(MoveUnitsAction(f, from, to, k, true, cost, left, e, then)))
                .cancel

        case MoveUnitsAction(f, from, to, n, kaija, cost, left, e, then) =>
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
                game.kaija = |(dst.anchor)

            val enemy = game.present(dst).but(f)

            f.log("moved", Figures(n, kaija), "from", from, "to", to, enemy.any.?("and attacked " ~ enemy./(_.elem).join(", ")).|(Empty))

            if (enemy.any && game.combats.has(dst.anchor).not)
                game.combats :+= dst.anchor

            Then(MoveAction(f, left - cost, e, then))

        case MoveDoneAction(f, e, then) =>
            Then(CombatsAction(f, e, then))

        case CombatsAction(f, e, then) =>
            val l = game.combats.%(a => game.present(game.board.territory(a)).num > 1)

            if (l.none) {
                game.combats = $
                // Snake Clan card: the token may move after the move
                if (e.special == SnakeMove)
                    Then(ScorchedAction(f, then))
                else
                    Then(then)
            }
            else
            if (l.num == 1)
                Then(FightAction(f, l.head, e, CombatsAction(f, e, then)))
            else
                Ask(f).each(l)(a => FightAction(f, a, e, CombatsAction(f, e, then)))

        case FightAction(f, a, e, then) =>
            game.combats = game.combats.but(a)

            val t = game.board.territory(a)
            val defender = game.present(t).but(f).head
            val fighting = game.combats./(game.board.territory)

            // Intimidate: one defending unit may be pushed to a neutral or defender territory next door
            val push = (e.special == IntimidateMove && game.count(t, defender) > 0).??(game.board.adjacent(t).map(_._1).%(o => fighting.has(o).not).%(o => game.present(o).but(defender).none))

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

                val defender = game.present(t).but(f).head

                log(f, "fights", defender, "in", a)

                val max = math.min(f.food, game.figures(t, f))

                if (max == 0)
                    Then(CombatFoodAction(f, f, defender, a, e, $(0), then))
                else
                    Ask(f).each(0.to(max).$)(k => CombatFoodAction(f, f, defender, a, e, $(k), then))
            }

        case CombatFoodAction(self, attacker, defender, a, e, food, then) =>
            self.food -= food.last

            if (food.last > 0)
                self.log("spent", food.last.hl, Food)

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
            val here = game.buildingsIn(t).map(_._2)
            val au = game.figures(t, attacker)
            val du = game.figures(t, defender)
            // Conqueror: the attacker ignores Fortresses and Defense Towers
            val fortress = attacker.conqueror.?(0).|(2 * here.count(_ == Fortress))
            val towers = attacker.conqueror.?(0).|(here.count(_ == DefenseTower))
            // Snake Clan fights better on its Scorched Earth
            val scorched = game.scorchedIn(t)
            val ab = (attacker == Snake && scorched).??(1)
            val db = (defender == Snake && scorched).??(1)

            val as = game.strength(t, attacker) + e.bonus + ab + food(0) + faces(0).points
            val ds = game.strength(t, defender) + fortress + db + food(1) + faces(1).points
            // Casualties each side inflicts
            val ac = faces(0).casualties
            // Shieldbearers cancel 1 casualty inflicted by the defender
            val shield = (e.special == ShieldMove && faces(1).casualties + towers > 0).??(1)
            val dc = faces(1).casualties + towers - shield

            if (game.kaijaIn(t, attacker) || game.kaijaIn(t, defender))
                game.note("kaija-fight")
            if (ab + db > 0)
                game.note("scorched-fight")

            def extra(l : (Int, Elem)*) : Elem = l.toList.filter(_._1 > 0).map { case (n, what) => "(" ~ n.hl ~ " from " ~ what ~ ")" }.join(" ")
            def kaija(f : Faction) = game.kaijaIn(t, f).?("(" ~ 2.hl ~ " from " ~ "Kaija".hl ~ ")").|(Empty)

            attacker.log("scored", as.hl, kaija(attacker), extra(e.bonus -> "the card".txt, ab -> "Scorched Earth".hl), "and inflicted", ac.hl, (ac == 1).?("casualty").|("casualties"))
            defender.log("scored", ds.hl, kaija(defender), extra(fortress -> Fortress.elem, db -> "Scorched Earth".hl), "and inflicted", dc.hl, (dc == 1).?("casualty").|("casualties"), extra(towers -> DefenseTower.elem), (shield > 0).?("(" ~ 1.hl ~ " cancelled by " ~ "Shieldbearers".hl ~ ")").|(Empty))

            val winner =
                if (ac >= du && dc >= au) None
                else if (ac >= du) |(attacker)
                else if (dc >= au) |(defender)
                else if (as > ds) |(attacker)
                else |(defender)

            game.removeFigures(t, attacker, math.min(dc, au))
            game.removeFigures(t, defender, math.min(ac, du))

            winner match {
                case None =>
                    log("Both sides were wiped out")
                    Then(then)

                case Some(w) =>
                    val loser = (w == attacker).?(defender).|(attacker)

                    w.log("won the fight in", a)

                    if (w == attacker) {
                        if (attacker == Wolf) {
                            attacker.food += 1
                            attacker.log("collected", 1.hl, Food, "for winning as the attacker")
                        }

                        if (attacker == Stag) {
                            attacker.fame += 1
                            attacker.log("gained", 1.hl, "fame for conquering a territory")
                        }
                    }

                    if (game.figures(t, loser) > 0)
                        Then(RetreatAction(loser, a, loser == attacker && e.ignoreRough, then))
                    else
                        Then(then)
            }

        case RetreatAction(f, a, rough, then) =>
            val t = game.board.territory(a)
            val n = game.count(t, f)
            val kaija = game.kaijaIn(t, f)

            if (n == 0 && kaija.not)
                Then(then)
            else {
                val fighting = game.combats./(game.board.territory)
                val to = game.board.adjacent(t).filter(rough || _._2).map(_._1).%(o => fighting.has(o).not).%(o => game.present(o).but(f).none)

                if (to.none) {
                    game.removeFigures(t, f, game.figures(t, f))
                    f.log("had nowhere to retreat and lost", Figures(n, kaija))
                    Then(then)
                }
                else
                    // Kaija goes with the first group
                    Ask(f).some(to)(o => $(RetreatToAction(f, a, o.anchor, n, kaija, rough, then)) ++ (n > 1).$(RetreatToAction(f, a, o.anchor, 1, kaija, rough, then)))
            }

        case RetreatToAction(f, from, to, n, kaija, rough, then) =>
            game.removeUnits(game.board.territory(from), f, n)
            game.addUnits(to, f, n)
            if (kaija)
                game.kaija = |(to)

            f.log("retreated", Figures(n, kaija), "to", to)

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
            else
                Ask(f).each(game.exploring)(t => ExploreTileAction(f, t, times, e, then))
                    .when(redraw && game.exploring.num == 1 && game.pile.any)(ExploreRedrawAction(f, game.exploring.head, times, e, then))

        case ExploreRedrawAction(f, tile, times, e, then) =>
            game.exploring = $
            game.pile :+= tile

            f.log("put a tile at the bottom of the pile")

            Then(ExploreDrawAction(f, 1, game.pile.num, times, false, e, then))

        case ExploreTileAction(f, tile, times, e, then) =>
            Ask(f).each(placements(tile, |(explorable(f, e.anywhere)), false).map(_._1).distinct)(s => ExploreSpotAction(f, tile, s, times, e, then)).cancel

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

            val closed = game.board.territories.%(game.board.closed).%(t => before.exists(b => b.areas.exists(t.areas.contains))).%(t => game.present(t) == $(f))

            closed.foreach { t =>
                val n = game.board.tiles(t)
                f.fame += n
                f.log("closed", t.anchor, "and gained", n.hl, "fame")

                if (f == Stag) {
                    f.fame += 1
                    f.log("gained", 1.hl, "more fame for closing a territory")
                }

                if (f == Raven)
                    collect(f, t, "from the closed territory")
            }

            if (closed.none && f == Boar) {
                f.lore += 1
                f.log("collected", 1.hl, Lore, "for exploring without closing a territory")
            }

            Then(ExploreAction(f, 1, times - 1, false, e, then))

        // BUILD
        case BuildAction(f, e, times, smallOnly, then) =>
            val options = (times > 0).??(buildOptions(f, e, smallOnly))

            if (options.none)
                Then(BuildFinishAction(f, e, then))
            else
                Ask(f).each(options) { case (a, b, s, cost) => BuildPlaceAction(f, a, b, s, cost, times, e, smallOnly, then) }
                    .add(BuildDoneAction(f, e, then))

        case BuildPlaceAction(f, a, b, s, cost, times, e, smallOnly, then) =>
            f.wood -= cost
            game.buildings += s -> b

            f.log("built", b, "in", a)

            if (f == Goat) {
                val n = b.large.?(2).|(1)
                f.food += n
                f.log("collected", n.hl, Food, "for building")
            }

            // Glory of the Clan: collect the territory's resources as at harvest
            if (e.special == GloryBuild)
                collect(f, game.board.territory(a), "from the territory")

            // Carpentry Mastery: once a large building is built, the other must be small
            Then(BuildAction(f, e, times - 1, smallOnly || (e.special == CarpentryBuild && b.large), then))

        case BuildDoneAction(f, e, then) =>
            Then(BuildFinishAction(f, e, then))

        case BuildFinishAction(f, e, then) =>
            // Industrious Villagers: may replace one of f's buildings by another of the same size
            val l = (e.special == IndustriousBuild).??(replacements(f))

            if (l.none)
                Then(then)
            else
                Ask(f).each(l) { case (s, b) => ReplaceBuildingAction(f, s.area, s, b, then) }.add(ReplaceSkipAction(f, then))

        case ReplaceBuildingAction(f, a, s, b, then) =>
            val old = game.buildings(s)
            game.buildings += s -> b

            f.log("replaced", old, "with", b, "in", a)

            Then(then)

        case ReplaceSkipAction(f, then) =>
            Then(then)

        // FEAST
        case FeastChoiceAction(f, e, then) =>
            resolve(f, e, then)

        // NO UNITS LEFT
        case ReturnUnitsAction(f, then) =>
            val neutral = game.board.territories.%(t => game.present(t).none)

            if (game.anyOnMap(f) || neutral.none)
                Then(then)
            else
                Ask(f).each(neutral)(t => ReturnUnitsToAction(f, t.anchor, then))

        case ReturnUnitsToAction(f, a, then) =>
            game.addUnits(a, f, math.min(3, game.reserve(f)))

            f.log("placed three new units in", a)

            Then(then)

        case _ => UnknownContinue
    }
}
