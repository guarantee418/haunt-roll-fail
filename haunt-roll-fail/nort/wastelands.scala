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
import hrf.meta._
import hrf.options._

import nort.elem._


// Wastelands expansion (12-page rulebook): five creatures for the Creatures module (two Rock Golems, two Myrkalfar,
// two Giant Boars, two Kobolds, Valdemar), seven Environment tiles shuffled into the map tiles, and ten Central tiles,
// one of which may replace the starting tile (CentralChoice, offered in every game: picking one turns on the
// expansion's code for it, without the Environment tiles and creatures unless the Wastelands module is on too). The tiles are in Tiles.wastelands and Tiles.central,
// the creatures in creatures.scala. Card and tile art from the TTS mod 3597126237.


// Where each tile's feature is: tile and area
object Waste {
    val kobold = "waste-kobold"
    val jotnar = "waste-jotnar"
    val nastrond = "waste-nastrond"
    val landvidi = "waste-landvidi"
    val thor = "waste-thor"
    val urdarbrunn = "waste-urdarbrunn"
    val vedrfolnir = "waste-vedrfolnir"

    val magma = "start-magma"
    val yggdrasil = "start-yggdrasil"
    val relic = "start-relic"
    val lake = "start-lake"
    val volcano = "start-volcano"
    val mimir = "start-mimir"
    val hrimgandr = "start-hrimgandr"
    val den = "start-den"
    val helheim = "start-helheim"

    // The central tiles impassable in the middle take the five-player tile with impassable borders
    // (the Wilderness Great Lake can be the central tile too)
    val impassable = $(relic, lake, volcano, Wild.lake)
    // Only with the Creatures module
    val creatureOnly = $(den, helheim)

    val centrals = $(magma, yggdrasil, relic, lake, volcano, mimir, hrimgandr, den, helheim)

    def tiles : $[String] = Tiles.wastelands./(_.id)

    def name(tile : String) : String = tile match {
        case "start" => "Standard starting tile"
        case Wild.lake => "Great Lake (Wilderness)"
        case `magma` => "Magma Flow"
        case `yggdrasil` => "Yggdrasil"
        case `relic` => "Relic of the Gods"
        case `lake` => "Great Lake"
        case `volcano` => "Volcano"
        case `mimir` => "Mimirsbrunn"
        case `hrimgandr` => "Hrimgandr's Lair"
        case `den` => "Wyvern's Den"
        case `helheim` => "Gate of Helheim"
        case `kobold` => "Kobold Camp"
        case `jotnar` => "Jötnar Camp"
        case `nastrond` => "Naströnd"
        case `landvidi` => "Landvidi"
        case `thor` => "Thor's Wrath"
        case `urdarbrunn` => "Urdarbrunn"
        case `vedrfolnir` => "Vedrfolnir"
        case _ => tile
    }

    def elem(tile : String) : Elem = name(tile).hl

    // The middle area of a central tile
    def middle(tile : String) = (tile == den).?("d").|("c")

    // The five-player tile that goes east of the central tile
    def five(tile : String) = if (tile == "start") "start-5" else if (impassable.has(tile)) "start-5-wall" else "start-5-open"

    // The territories holding area id of a tile on the map
    def at(tile : String, id : String)(implicit game : Game) : $[Territory] =
        game.board.placements.%(_.tile == tile)./(p => game.board.territory(AreaRef(p.x, p.y, id))).distinct

    def in(t : Territory, tile : String, id : String)(implicit game : Game) : Boolean =
        t.areas.exists(a => a.id == id && game.board.at(a.x, a.y).exists(_.tile == tile))

    // Who controls the territory with that area, if anybody
    def controller(tile : String, id : String)(implicit game : Game) : |[Faction] =
        at(tile, id)./~(t => factions.%(f => game.controlled(f).has(t))).single

    // The territories around (on) each such tile: the camps, Naströnd and the impassable central tiles
    def around(tile : String)(implicit game : Game) : $[$[Territory]] =
        game.board.placements.%(_.tile == tile)./(p => p.spec.areas./(a => game.board.territory(AreaRef(p.x, p.y, a.id))).distinct)

    def aroundSpot(spot : Spot)(implicit game : Game) : $[Territory] =
        game.board.at(spot.x, spot.y)./~(p => p.spec.areas./(a => game.board.territory(AreaRef(p.x, p.y, a.id)))).distinct

    // The whole Wastelands module: its Environment tiles and creatures, not just a central tile
    def module(implicit game : Game) = game.options.has(ModuleOption(Wastelands))

    def landvidiIn(t : Territory)(implicit game : Game) = in(t, landvidi, "s")

    def urdarbrunnIn(t : Territory)(implicit game : Game) = in(t, urdarbrunn, "s")

    def hrimgandrAlive(implicit game : Game) = game.creatureLine.has(Creature.hrimgandr)

    def valdemarAlive(implicit game : Game) = game.creatureLine.exists(_.kind == Valdemar)

    // The Wyvern of the central Wyvern's Den was placed and is gone
    def wyvernSlain(implicit game : Game) = game.denWyvern && game.creatureLine.has(Creature.wyvern).not

    // Jötunn Blainn waits at his camp
    def blainnAtCamp(implicit game : Game) = game.jotnarCamp.any && game.blainn.none
}


// The central tile (setup step F); offered in every game
abstract class CentralChoice(val label : String) extends GameOption with OneOfGroup with ImportantOption {
    val group = "Central tile".txt
    def valueOn = label.txt
    def tile : |[String] = None
}

case object StandardCentral extends CentralChoice("Standard starting tile") {
    override val explain = $("The core game's starting tile.".txt)
}

case object RandomCentral extends CentralChoice("Random Wastelands central tile") {
    override val explain = $("One of the Wastelands central tiles, drawn at random (the Wyvern's Den and the Gate of Helheim only with the " ~ "Creatures".hl ~ " module).")
}

case object RandomAnyCentral extends CentralChoice("Random, any center tile") {
    override val explain = $("The standard starting tile, the Wilderness Great Lake or one of the Wastelands central tiles, drawn at random (the Wyvern's Den and the Gate of Helheim only with the " ~ "Creatures".hl ~ " module).")
}

abstract class CentralTileChoice(val id : String, text : String) extends CentralChoice(Waste.name(id)) {
    override def tile = |(id)
    override def decorate(e : Elem) = e ~ Waste.creatureOnly.has(id).?(" (Creatures module)".spn(xstyles.smaller85)).|(Empty)
    override val explain = $(Waste.name(id).hl ~ ": " ~ text)
    override def required(all : $[BaseOption]) = $(Waste.creatureOnly.has(id).$[BaseOption](ModuleOption(Creatures)))
}

case object MagmaFlowCentral extends CentralTileChoice(Waste.magma, "at the Start of Year, its controller draws 1 more card.")
case object YggdrasilCentral extends CentralTileChoice(Waste.yggdrasil, "at each Harvest, the controller of the tree's territory gains 5 fame.")
case object RelicCentral extends CentralTileChoice(Waste.relic, "impassable; at each Harvest, the most units next to it gain 2 lore, everyone else with a unit next to it 1 (all 1 when tied).")
case object GreatLakeCentral extends CentralTileChoice(Waste.lake, "impassable; at each Harvest, the most units next to it gain 2 food (1 each when tied).")
case object VolcanoCentral extends CentralTileChoice(Waste.volcano, "impassable; at the start of each year the first player picks a player and rolls a die: 1 fame per axe, 1 unit removed per skull.")
case object MimirCentral extends CentralTileChoice(Waste.mimir, "at the Start of Year, its controller may take one of the year's Development or Achievement cards on top of their deck, and takes no other that year.")
case object HrimgandrCentral extends CentralTileChoice(Waste.hrimgandr, "Hrimgandr (strength 8, 8 fame) lives there, never moves and raises everyone's Winter costs one step; once it is defeated, the Lair's controller draws 1 more card each year.")
case object DenCentral extends CentralTileChoice(Waste.den, "the Wyvern comes out at the start of year 3; once it is defeated, the Den's controller gains 2 fame at each Harvest.")
case object WildLakeCentral extends CentralTileChoice(Wild.lake, "the Wilderness Environment tile: impassable; at each Harvest, the most units and warchiefs next to it gain 2 food (1 each when tied).")
case object HelheimCentral extends CentralTileChoice(Waste.helheim, "its controller gets +2 axes against creatures; at each Creature phase, on a skull, a creature comes out of it.")

object CentralChoice {
    val tiles : $[CentralChoice] = $(MagmaFlowCentral, YggdrasilCentral, RelicCentral, GreatLakeCentral, VolcanoCentral, MimirCentral, HrimgandrCentral, DenCentral, HelheimCentral, WildLakeCentral)
    val all : $[CentralChoice] = $(StandardCentral, RandomCentral, RandomAnyCentral) ++ tiles

    // A choice other than the standard tile turns on the Wastelands code (Meta.has)
    def picked(options : $[BaseOption]) = options.of[CentralChoice].exists(_ != StandardCentral)
}


// SETUP
case class CentralPickedAction(tiles : $[String], random : String) extends RandomAction[String]
case class WasteEnvironmentAction(tiles : $[String], shuffled : $[String]) extends ShuffledAction[String]
case class ShuffledWasteTilesAction(shuffled : $[String]) extends ShuffledAction[String]

// START OF YEAR
case class EruptAction(then : ForcedAction) extends ForcedAction
case class EruptTargetAction(self : Faction, target : Faction, then : ForcedAction) extends BaseAction("Volcano".hl, "choose the player it erupts on")(target)
case class EruptRolledAction(target : Faction, random : DieFace, then : ForcedAction) extends RandomAction[DieFace]
case class EruptRemoveAction(target : Faction, left : Int, then : ForcedAction) extends ForcedAction
case class EruptUnitAction(self : Faction, area : AreaRef, left : Int, then : ForcedAction) extends BaseAction("Volcano".hl, "remove a unit from")(area) with MapTarget { def target = area }
case class MimirTakeAction(self : Faction, card : Card) extends BaseAction("Mimirsbrunn".hl, "put a card on top of your deck (and take no other this year)")(card.img, Break, card)
case class MimirSkipAction(self : Faction) extends BaseAction("Mimirsbrunn".hl)("Take no card")

// CREATURES
case class MyrkalfChoiceAction(self : Faction, c : Creature, to : AreaRef, start : AreaRef, second : Boolean, l : $[Creature]) extends BaseAction("Creature phase:", c, "is tied between territories; move it to")(to) with MapTarget { def target = to }
case class MyrkalfMoveAction(c : Creature, to : AreaRef, start : AreaRef, second : Boolean, l : $[Creature]) extends ForcedAction
case class GateRolledAction(random : DieFace) extends RandomAction[DieFace]
case class GatePickAction(self : Faction, c : Creature, other : |[Creature]) extends BaseAction("Gate of Helheim".hl, "choose the creature that comes out")(c)

// HARVEST
case class VedrfolnirAction(self : Faction) extends BaseAction("Vedrfolnir".hl)("Add the top map tile next to an open territory")
case class VedrfolnirSkipAction(self : Faction) extends BaseAction("Vedrfolnir".hl)("Add no tile")
case class VedrfolnirSpotAction(self : Faction, tile : String, spot : Spot) extends BaseAction("Vedrfolnir".hl, "place", TileRef(tile), "at")(spot) with Soft with MapTarget { def target = spot }
case class VedrfolnirRotateAction(self : Faction, tile : String, spot : Spot, r : Int, d : Int) extends BaseAction("Place the tile at", spot)(RotateLabel(d)) with Soft with MapTarget { def target = RotateMark(d) }
case class VedrfolnirTurnAction(self : Faction, tile : String, spot : Spot, r : Int) extends BaseAction("Place the tile at", spot)("Confirm") with TilePreview
case class WasteHarvestAction(l : $[Faction], then : ForcedAction) extends ForcedAction
case class WasteTradeAction(f : Faction, kobolds : Int, camps : Int, rest : $[Faction], then : ForcedAction) extends ForcedAction
case class KoboldSwapAction(self : Faction, give : Resource, kobolds : Int, camps : Int, rest : $[Faction], then : ForcedAction) extends BaseAction("Kobold", "exchange one for one", (kobolds > 1).?("(" ~ kobolds.hl ~ " left)").|(Empty))("Give", give, "for", (give == Food).?(Wood).|(Food))
// gain: None is 1 fame
case class CampTradeAction(self : Faction, give : Resource, gain : |[Resource], kobolds : Int, camps : Int, rest : $[Faction], then : ForcedAction) extends BaseAction("Kobold Camp".hl, "exchange", (camps > 1).?("(" ~ camps.hl ~ " left)").|(Empty))("Give", give, "for", gain./(_.elem).|("1 fame".txt))
case class WasteTradeDoneAction(self : Faction, rest : $[Faction], then : ForcedAction) extends BaseAction("Kobold exchanges")("Done")
case class BlainnStepAction(l : $[Faction], then : ForcedAction) extends ForcedAction
case class BlainnRecruitAction(self : Faction, area : AreaRef, then : ForcedAction) extends BaseAction("Jötunn Blainn".hl, "recruit him for", 1.hl, Food.elem, "into")(area) with MapTarget { def target = area }
case class BlainnSkipAction(self : Faction, rest : $[Faction], then : ForcedAction) extends BaseAction("Jötunn Blainn".hl)("Don't recruit him")


object WastelandsExpansion extends Expansion {
    // COMBAT: Thor's Wrath gives its controller 1 point anywhere; Landvidi's defender gets 2
    def points(f : Faction, t : Territory, attacking : Boolean)(implicit game : Game) : Int =
        Waste.controller(Waste.thor, "s").has(f).??(1) + (attacking.not && Waste.landvidiIn(t)).??(2)

    // Urdarbrunn's defender ignores a casualty of the attacker's die
    def ignored(t : Territory)(implicit game : Game) : Int = Waste.urdarbrunnIn(t).??(1)

    // A creature's die: the choice is a point, but a Rock Golem takes the skull; a Giant Boar attacking adds a skull
    def creatureFace(c : Creature, attacking : Boolean, random : DieFace) : DieFace = {
        val face = random.choice.?((c.kind == RockGolem).?(NorthgardDie.casualty).|(NorthgardDie.point)).|(random)
        (c.kind == GiantBoar && attacking.not).?(face.copy(casualties = face.casualties + 1)).|(face)
    }

    // Extra points of the player and the creature in a fight against a creature
    def creaturePoints(f : Faction, t : Territory, c : Creature, attacking : Boolean, face : DieFace, cface : DieFace)(implicit game : Game) : (Int, Int) = {
        val golem = c.kind == RockGolem
        // Urdarbrunn: a defending creature ignores a skull of the player's die
        val skulls = math.max(0, face.casualties - (attacking && Waste.urdarbrunnIn(t)).??(1))
        val player = points(f, t, attacking) + Waste.controller(Waste.helheim, "c").has(f).??(2) + golem.??(skulls)
        val creature = golem.??(cface.casualties) + (c.kind != Valdemar && Waste.valdemarAlive).??(1) + (attacking && Waste.landvidiIn(t)).??(2)
        (player, creature)
    }

    // A player defending in Urdarbrunn ignores one casualty
    def creatureIgnored(f : Faction, t : Territory, attacking : Boolean)(implicit game : Game) : Int = (attacking.not && Waste.urdarbrunnIn(t)).??(1)

    // START OF YEAR: Magma Flow, and Hrimgandr's Lair once Hrimgandr is gone
    def extraDraw(f : Faction)(implicit game : Game) : Int =
        Waste.controller(Waste.magma, "c").has(f).??(1) + (game.central == Waste.hrimgandr && Waste.hrimgandrAlive.not && Waste.controller(Waste.hrimgandr, "c").has(f)).??(1)

    // WINTER: Hrimgandr raises the costs one step (three more units)
    def winterUnits(implicit game : Game) : Int = (game.has(Wastelands) && Waste.hrimgandrAlive).??(3)

    // HARVEST: the units (all figures) of each player in the territories around a tile
    def most(tile : String)(implicit game : Game) : $[Map[Faction, Int]] = Waste.around(tile)./(l => factions./(g => g -> l./(t => game.figures(t, g)).sum).toMap)

    // The Great Lake: the most units around it 2 food, 1 each when tied
    def lakeFood(f : Faction)(implicit game : Game) : Int = most(Waste.lake)./{ m =>
        val top = m.values.max
        val tied = m.keys.toList.filter(m(_) == top)
        (top > 0 && tied.has(f)).??((tied.num == 1).?(2).|(1))
    }.sum

    // The Relic of the Gods: the most units around it 2 lore, the others there 1; all 1 when tied for the most
    def relicLore(f : Faction)(implicit game : Game) : Int = most(Waste.relic)./{ m =>
        val top = m.values.max
        val tied = m.keys.toList.filter(m(_) == top)
        (m(f) > 0).??((tied.num == 1 && tied.has(f)).?(2).|(1))
    }.sum

    // Fame: Yggdrasil 5, the central Wyvern's Den 2 once the Wyvern is gone (none with a Kobold there)
    def fame(f : Faction)(implicit game : Game) : Int =
        Waste.at(Waste.yggdrasil, "c").%(t => game.controlled(f).has(t)).%(t => game.koboldIn(t).not).num * 5 +
        Waste.wyvernSlain.??(Waste.at(Waste.den, "d").%(t => game.controlled(f).has(t)).%(t => game.koboldIn(t).not).num * 2)

    // The Wilderness Great Lake as the central tile, by its own rule; with Wilderness on, Wilderness collects it
    def wildLakeFood(f : Faction)(implicit game : Game) : Int = (game.central == Wild.lake && game.has(Wilderness).not).??(WildernessExpansion.lakeFood(f).sum)

    // Fame, food and lore at the next harvest, for the player panels
    def forecast(f : Faction)(implicit game : Game) : (Int, Int, Int) = (fame(f), lakeFood(f) + wildLakeFood(f), relicLore(f))

    def harvest(f : Faction)(implicit game : Game) {
        val n = fame(f)
        if (n > 0) {
            f.fame += n
            f.log("gained", n.hl, "fame from", Waste.controller(Waste.yggdrasil, "c").has(f).?(Waste.elem(Waste.yggdrasil)).|(Waste.elem(Waste.den)))
        }

        val food = lakeFood(f)
        if (food > 0) {
            f.food += food
            f.log("collected", food.hl, Food, "from the", Waste.elem(Waste.lake))
        }

        val wild = wildLakeFood(f)
        if (wild > 0) {
            f.food += wild
            f.log("collected", wild.hl, Food, "from the", "Great Lake".hl)
        }

        val lore = relicLore(f)
        if (lore > 0) {
            f.lore += lore
            f.log("collected", lore.hl, Lore, "from the", Waste.elem(Waste.relic))
        }
    }

    // Kobold creatures in f's territories, and f's territories next to a Kobold Camp
    def kobolds(f : Faction)(implicit game : Game) = game.controlled(f)./(t => game.creaturesIn(t).count(_.kind == Kobold)).sum

    def camps(f : Faction)(implicit game : Game) = Waste.around(Waste.kobold)./(l => l.count(t => game.controlled(f).has(t))).sum

    // Where Blainn can be recruited: f's territories next to his camp
    def blainnTargets(f : Faction)(implicit game : Game) : $[Territory] =
        game.jotnarCamp./~(Waste.aroundSpot).%(t => game.controlled(f).has(t)).%(t => game.hostileIn(t).not)

    // Naströnd's 2 wood: to the only player controlling a territory next to it (the explorer when tied)
    def nastrond(explorer : |[Faction])(implicit game : Game) {
        game.nastrond.foreach { spot =>
            val l = Waste.aroundSpot(spot)
            val holders = game.from(game.first).%(f => l.exists(t => game.controlled(f).has(t)))

            val taker = explorer.%(holders.has) || (holders.num == 1).?(holders.head)

            taker.foreach { f =>
                game.nastrond = game.nastrond.but(spot)
                f.wood += 2
                game.note("nastrond")
                f.log("collected", 2.hl, Wood, "from", Waste.elem(Waste.nastrond))
            }
        }
    }

    // Kept true at every action: Blainn alone goes back to his camp; Naströnd's wood goes to the first player next to it
    def normalize()(implicit game : Game) {
        game.blainn.foreach { case (f, a) =>
            val t = game.board.territory(a)
            if (game.figures(t, f) <= 1 || game.present(t).num > 1 && game.battle.none && game.combats.has(t.anchor).not && game.combats.exists(t.areas.contains).not) {
                if (game.figures(t, f) <= 1) {
                    game.blainn = None
                    game.note("blainn-back")
                    log("Jötunn Blainn".hl, "was left alone and went back to the", Waste.elem(Waste.jotnar))
                }
            }
        }

        if (game.nastrond.any)
            nastrond(None)
    }

    // Blainn goes along with the last of his owner's figures leaving his territory
    def follows(f : Faction, from : AreaRef, n : Int, kaija : Boolean, chief : Boolean)(implicit game : Game) : Boolean = {
        val src = game.board.territory(from)
        game.blainnIn(src, f) && game.count(src, f) - n + (game.kaijaIn(src, f) && kaija.not).??(1) + (game.chiefIn(src, f) && chief.not).??(1) <= 0
    }

    // Creature combat actions, handed to the Creatures module when it's off (Hrimgandr can be in the game without it)
    def creatureCombat(a : Action) : Boolean = a match {
        case _ : MoveEndAction | _ : CreatureDeclareAction | _ : CreatureAttackAction | _ : CreatureDeclareDoneAction | _ : CreatureFightAction |
            _ : CreatureCombatAction | _ : CreatureFoodStartAction | _ : CreatureCunningAskAction | _ : CreatureCunningAction | _ : CreatureCunningDoneAction |
            _ : CreatureFoodAction | _ : CreatureFoodPaidAction | _ : CreaturePlayerRolledAction | _ : CreatureRerollAction | _ : CreatureRerolledAction |
            _ : CreatureFaceAction | _ : CreatureRolledAction => true
        case _ => false
    }

    // The central tile in place of the starting tile; Hrimgandr lives in his Lair
    def central(tiles : $[String], tile : String, soft : Void)(implicit game : Game) : Continue = {
        game.central = tile

        log("The central tile is", Waste.elem(tile))

        if (tile == Waste.hrimgandr) {
            val c = Creature.hrimgandr
            game.creatureLine :+= c
            game.creatureAt += c -> AreaRef(0, 0, "c")
            log(c, "lives in its Lair")
        }

        game.internalPerform(ShuffledTilesAction(tiles), soft)
    }

    def perform(action : Action, soft : Void)(implicit game : Game) : Continue = {
        if (action.isSoft.not && game.states.any)
            normalize()

        action @@ {
            case a if game.has(Creatures).not && game.creatureLine.any && creatureCombat(a) =>
                CreaturesExpansion.perform(a, soft)

            // SETUP: the central tile, chosen or drawn
            case ShuffledTilesAction(tiles) if game.wasteSteps.has("central").not =>
                game.wasteSteps :+= "central"

                options.of[CentralChoice].single match {
                    case Some(RandomCentral) =>
                        Random[String](Waste.centrals.%(t => game.has(Creatures) || Waste.creatureOnly.has(t).not), CentralPickedAction(tiles, _))
                    case Some(RandomAnyCentral) =>
                        Random[String]("start" +: Waste.centrals.%(t => game.has(Creatures) || Waste.creatureOnly.has(t).not) :+ Wild.lake, CentralPickedAction(tiles, _))
                    case Some(c) if c.tile.any =>
                        central(tiles, c.tile.get, soft)
                    case _ =>
                        UnknownContinue
                }

            case CentralPickedAction(tiles, tile) =>
                central(tiles, tile, soft)

            // After setup, the Environment tiles go into the map tile pile; with Wilderness at most 12 of both expansions
            case ShuffledTilesBackAction(l) if Waste.module && game.wasteSteps.has("environment").not =>
                if (game.has(Wilderness))
                    Shuffle[String](Wild.tiles ++ Waste.tiles, WasteEnvironmentAction(l, _))
                else {
                    game.wasteSteps :+= "environment"
                    game.pile = l
                    log("The unused tiles went back into the pile")
                    Shuffle[String](l ++ Waste.tiles, ShuffledWasteTilesAction(_))
                }

            case WasteEnvironmentAction(l, all) =>
                game.wasteSteps :+= "environment"
                game.environment = |(all.take(12))
                log("Twelve Environment tiles of Wilderness and Wastelands were drawn")
                game.internalPerform(ShuffledTilesBackAction(l), soft)

            case ShuffledWasteTilesAction(l) =>
                game.pile = l
                log("The Environment tiles were shuffled into the map tiles")
                Then(StartYearAction)

            // A tile revealed: Naströnd's wood, Jötunn Blainn at his camp
            case TilePlacedAction(f, Waste.nastrond, spot, setup, then) =>
                game.nastrond :+= spot
                log("2".hl, Wood, "were placed on", Waste.elem(Waste.nastrond))
                nastrond(|(f))
                UnknownContinue

            case TilePlacedAction(f, Waste.jotnar, spot, setup, then) =>
                game.jotnarCamp = |(spot)
                log("Jötunn Blainn".hl, "waits at the", Waste.elem(Waste.jotnar))
                UnknownContinue

            // START OF YEAR: the Wyvern of the central Den comes out at the start of year 3; the Volcano erupts
            case StartYearAction if game.has(Wastelands) && game.wasteYear <= game.year =>
                game.wasteYear = game.year + 1
                game.wasteSteps = $

                val next = (game.central == Waste.volcano).?(EruptAction(StartYearAction) : ForcedAction).|(StartYearAction)

                if (game.central == Waste.den && game.has(Creatures) && game.year + 1 == 3 && game.denWyvern.not) {
                    val c = Creature.wyvern
                    game.denWyvern = true
                    game.creatureLine :+= c
                    game.creatureAt += c -> AreaRef(0, 0, "d")
                    game.note("wyvern")
                    log(c, "came out of the", Waste.elem(Waste.den))
                    Then(CreatureEffectAction(c, next))
                }
                else
                    Then(next)

            case EruptAction(then) =>
                Ask(game.first).each(game.from(game.first))(g => EruptTargetAction(game.first, g, then))

            case EruptTargetAction(f, g, then) =>
                f.log("chose", g, "for the", Waste.elem(Waste.volcano))
                Random[DieFace](NorthgardDie.faces, EruptRolledAction(g, _, then))

            case EruptRolledAction(g, face, then) =>
                log("The", Waste.elem(Waste.volcano), "rolled", face)

                val fame = face.points + face.choice.??(1)
                val lost = face.casualties + face.choice.??(1)

                if (fame > 0) {
                    g.fame += fame
                    g.log("gained", fame.hl, "fame")
                }

                game.note("volcano")

                Then(EruptRemoveAction(g, lost, then))

            case EruptRemoveAction(g, left, then) =>
                // Units only, not the warchief, Kaija or Blainn
                val l = game.board.territories.%(t => game.count(t, g) > 0)

                if (left <= 0 || l.none)
                    Then(then)
                else
                if (g == Automa || l.num == 1)
                    Then(EruptUnitAction(g, l.head.anchor, left, then))
                else
                    Ask(g).each(l)(t => EruptUnitAction(g, t.anchor, left, then))

            case EruptUnitAction(g, a, left, then) =>
                game.removeUnits(game.board.territory(a), g, 1)
                g.log("lost a unit in", a, "to the", Waste.elem(Waste.volcano))
                Then(EruptRemoveAction(g, left - 1, then))

            // Mimirsbrunn: after drawing, one of the year's cards on top of the controller's deck
            case ActionsPhaseAction if game.wasteSteps.has("mimir").not =>
                game.wasteSteps :+= "mimir"

                Waste.controller(Waste.mimir, "c").%(_ => game.display.any) match {
                    case Some(f) => Ask(f).each(game.display)(c => MimirTakeAction(f, c)).add(MimirSkipAction(f))
                    case None => Then(ActionsPhaseAction)
                }

            case MimirTakeAction(f, c) =>
                game.display = game.display.diff($(c))
                f.draw = c +: f.draw
                f.foresaw = true
                game.note("mimir")
                f.log("took", c, "with", Waste.elem(Waste.mimir), "and placed it on top of their draw pile")
                Then(ActionsPhaseAction)

            case MimirSkipAction(f) =>
                Then(ActionsPhaseAction)

            // CREATURES
            case CreatureEffectAction(c, then) if c.kind == Wyvern && game.has(Wilderness).not =>
                WildernessExpansion.perform(action, soft)

            // A Rock Golem attacks if there are buildings; a Giant Boar if there is wood (buildings too); Valdemar kills a unit
            case CreatureEffectAction(c, then) if c.kind == RockGolem || c.kind == GiantBoar || c.kind == Valdemar =>
                val t = game.board.territory(game.creatureAt(c))

                game.present(t).single match {
                    case Some(o) if c.kind == RockGolem && game.buildingsIn(t).any =>
                        game.note("golem-attack")
                        log(c, "attacks", o, "in", t.anchor)
                        Then(CreatureCombatAction(o, t.anchor, c, MoveEffect(0), false, then))

                    case Some(o) if c.kind == GiantBoar && game.produce(t)._2 > 0 =>
                        game.note("boar-attack")
                        log(c, "attacks", o, "in", t.anchor)
                        Then(CreatureCombatAction(o, t.anchor, c, MoveEffect(0), false, then))

                    case Some(o) if c.kind == Valdemar && game.count(t, o) > 0 =>
                        game.removeUnits(t, o, 1)
                        game.note("valdemar")
                        o.log("lost a unit to", c, "in", t.anchor)
                        Then(then)

                    case _ =>
                        Then(then)
                }

            // Hrimgandr never moves
            case CreatureActivateAction(c :: rest) if c.kind == Hrimgandr =>
                Then(CreatureActivateAction(rest))

            // A Myrkalf moves twice, not back where it started; the owner where it stops pays 1 wood or loses 2 fame
            case CreatureActivateAction(c :: rest) if c.kind == Myrkalf && game.creatureLine.has(c) =>
                val start = game.creatureAt(c)
                val l = CreaturesExpansion.destinations(c)

                if (l.none) {
                    log(c, "could not move")
                    Then(CreatureActivateAction(rest))
                }
                else
                if (l.num == 1)
                    Then(MyrkalfMoveAction(c, l.head.anchor, start, false, rest))
                else
                    Ask(game.first).each(l)(t => MyrkalfChoiceAction(game.first, c, t.anchor, start, false, rest))

            case MyrkalfChoiceAction(f, c, to, start, second, rest) =>
                Then(MyrkalfMoveAction(c, to, start, second, rest))

            case MyrkalfMoveAction(c, to, start, second, rest) =>
                game.creatureAt += c -> to
                game.note("creature-move")

                log(c, "moved to", to)

                val home = game.board.territory(start)
                val l = second.not.??(CreaturesExpansion.destinations(c).%(_ != home))

                if (second || l.none) {
                    val t = game.board.territory(to)

                    game.present(t).single.foreach { o =>
                        game.note("myrkalf")

                        if (o.wood > 0) {
                            o.wood -= 1
                            o.log("paid", 1.hl, Wood, "to", c)
                        }
                        else {
                            o.fame -= 2
                            o.log("could not pay", Wood, "to", c, "and lost", 2.hl, "fame")
                        }
                    }

                    Then(CreatureActivateAction(rest))
                }
                else
                if (l.num == 1)
                    Then(MyrkalfMoveAction(c, l.head.anchor, start, true, rest))
                else
                    Ask(game.first).each(l)(t => MyrkalfChoiceAction(game.first, c, t.anchor, start, true, rest))

            // The Gate of Helheim: at the start of the Creature phase, on a skull, one of two creatures comes out of it
            case CreaturePhaseAction if game.central == Waste.helheim && game.has(Creatures) && game.wasteSteps.has("gate").not =>
                game.wasteSteps :+= "gate"
                Random[DieFace](NorthgardDie.faces, GateRolledAction(_))

            case GateRolledAction(face) =>
                log("The", Waste.elem(Waste.helheim), "rolled", face)

                val drawn = game.creatureDeck.take(2)

                if ((face.casualties > 0 || face.choice) && drawn.any) {
                    game.creatureDeck = game.creatureDeck.drop(drawn.num)
                    Ask(game.first).each(drawn)(c => GatePickAction(game.first, c, drawn.but(c).single))
                }
                else
                    Then(CreaturePhaseAction)

            case GatePickAction(f, c, other) =>
                game.creatureDeck ++= other
                game.creatureLine :+= c
                game.creatureAt += c -> AreaRef(0, 0, "c")
                game.note("gate")

                log(c, "came out of the", Waste.elem(Waste.helheim))

                Then(CreatureEffectAction(c, CreaturePhaseAction))

            // BLAINN goes along with the last figures leaving his territory
            case MoveUnitsAction(f, from, to, n, kaija, chief, cost, left, e, then) if follows(f, from, n, kaija, chief) =>
                game.blainn = |((f, game.board.territory(to).anchor))
                f.log("took", "Jötunn Blainn".hl, "along")
                UnknownContinue

            case RetreatToAction(f, from, to, n, kaija, rough, then) if follows(f, from, n, kaija, true) =>
                game.blainn = |((f, to))
                f.log("took", "Jötunn Blainn".hl, "along")
                UnknownContinue

            // VEDRFOLNIR: just before the Harvest, its controller may add the top map tile next to an open territory
            case ScorchedHarvestAction if game.wasteSteps.has("vedrfolnir").not =>
                game.wasteSteps :+= "vedrfolnir"

                Waste.controller(Waste.vedrfolnir, "s").%(_ => game.pile.any) match {
                    case Some(f) => Ask(f).add(VedrfolnirAction(f)).add(VedrfolnirSkipAction(f))
                    case None => Then(ScorchedHarvestAction)
                }

            case VedrfolnirSkipAction(f) =>
                Then(ScorchedHarvestAction)

            case VedrfolnirAction(f) =>
                val tile = game.pile.head
                game.pile = game.pile.drop(1)

                val l = MapExpansion.placements(tile, |(game.board.territories.%(game.board.open)), false)

                if (l.none) {
                    game.pile :+= tile
                    f.log("drew a tile that fits nowhere and put it at the bottom of the pile")
                    Then(ScorchedHarvestAction)
                }
                else
                    Ask(f).each(l.map(_._1).distinct)(s => VedrfolnirSpotAction(f, tile, s))

            case VedrfolnirSpotAction(f, tile, spot) =>
                val rs = MapExpansion.rotations(MapExpansion.placements(tile, |(game.board.territories.%(game.board.open)), false), spot)

                Ask(f)
                    .when(rs.num > 1)(VedrfolnirRotateAction(f, tile, spot, rs.head, 1))
                    .when(rs.num > 1)(VedrfolnirRotateAction(f, tile, spot, rs.head, -1))
                    .add(VedrfolnirTurnAction(f, tile, spot, rs.head))

            case VedrfolnirRotateAction(f, tile, spot, r, d) =>
                val rs = MapExpansion.rotations(MapExpansion.placements(tile, |(game.board.territories.%(game.board.open)), false), spot)
                val n = MapExpansion.rotate(rs, r, d)

                Ask(f)
                    .when(rs.num > 1)(VedrfolnirRotateAction(f, tile, spot, n, 1))
                    .when(rs.num > 1)(VedrfolnirRotateAction(f, tile, spot, n, -1))
                    .add(VedrfolnirTurnAction(f, tile, spot, n))

            case VedrfolnirTurnAction(f, tile, spot, r) =>
                game.board.place(Placement(tile, spot.x, spot.y, r))
                game.note("vedrfolnir")

                f.log("added a tile with", Waste.elem(Waste.vedrfolnir))

                Then(TilePlacedAction(f, tile, spot, false, ScorchedHarvestAction))

            // HARVEST: the Kobolds' and the Kobold Camp's exchanges, then Jötunn Blainn's recruitment, before the trades
            case AfterHarvestAction(then) if then.is[WasteHarvestAction].not =>
                Then(AfterHarvestAction(WasteHarvestAction(game.from(game.first), then)))

            case WasteHarvestAction(Nil, then) =>
                Then(BlainnStepAction(game.from(game.first), then))

            case WasteHarvestAction(f :: rest, then) =>
                Then(WasteTradeAction(f, kobolds(f), camps(f), rest, then))

            case WasteTradeAction(f, k, c, rest, then) =>
                val swaps = (k > 0).??($(Food, Wood).%(f.has(_) > 0)./(r => KoboldSwapAction(f, r, k, c, rest, then)))
                val trades = (c > 0).??($(Food, Wood).%(f.has(_) > 0)./~(r => $(|(Food), |(Wood), None).%(_ != |(r))./(g => CampTradeAction(f, r, g, k, c, rest, then))))

                if (swaps.none && trades.none)
                    Then(WasteHarvestAction(rest, then))
                else
                    Ask(f).add(swaps).add(trades).add(WasteTradeDoneAction(f, rest, then))

            case KoboldSwapAction(f, r, k, c, rest, then) =>
                val other = (r == Food).?(Wood).|(Food)
                f.gain(r, -1)
                f.gain(other, 1)
                game.note("kobold-swap")
                f.log("exchanged", 1.hl, r, "for", 1.hl, other, "with a", "Kobold".hl)
                Then(WasteTradeAction(f, k - 1, c, rest, then))

            case CampTradeAction(f, r, g, k, c, rest, then) =>
                f.gain(r, -1)
                g match {
                    case Some(x) => f.gain(x, 1)
                    case None => f.fame += 1
                }
                game.note("kobold-camp")
                f.log("exchanged", 1.hl, r, "for", g./(x => 1.hl ~ " " ~ x.elem).|(1.hl ~ " fame"), "at the", Waste.elem(Waste.kobold))
                Then(WasteTradeAction(f, k, c - 1, rest, then))

            case WasteTradeDoneAction(f, rest, then) =>
                Then(WasteHarvestAction(rest, then))

            case BlainnStepAction(Nil, then) =>
                Then(then)

            case BlainnStepAction(f :: rest, then) =>
                val l = Waste.blainnAtCamp.??(blainnTargets(f)).%(_ => f.food > 0)

                if (l.none)
                    Then(BlainnStepAction(rest, then))
                else
                    Ask(f).each(l)(t => BlainnRecruitAction(f, t.anchor, then)).add(BlainnSkipAction(f, rest, then))

            case BlainnRecruitAction(f, a, then) =>
                f.food -= 1
                game.blainn = |((f, a))
                game.note("blainn-recruit")
                f.log("recruited", "Jötunn Blainn".hl, "into", a, "for", 1.hl, Food)
                Then(then)

            case BlainnSkipAction(f, rest, then) =>
                Then(BlainnStepAction(rest, then))

            case _ => UnknownContinue
        }
    }
}
