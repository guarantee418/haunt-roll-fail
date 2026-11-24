package tactics
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

import hrf.meta._
import hrf.options._
import hrf.elem._
import tactics.elem._

// COMPANY OF HEROES: THE BOARD GAME, 2ND EDITION (Bad Crow Games), listed on the site as "Company of Heroes" (meta `tactics`) while it is played here for testing.
// The rules are the core rulebook's First Game Rules (RULES.md has them as played and the choices made); the units are the US Forces'
// and the Wehrmacht's three Building Boards, read off the Tabletop Simulator mod's images.

trait Faction extends BasePlayer with Named with Styling with Record {
    def short : String
    def id : String
    def color : String
}

case object US extends Faction {
    def name = "US Forces"
    def short = "US"
    def id = "us"
    def color = "#6f9a3e"
}

case object Wehrmacht extends Faction {
    def name = "Wehrmacht"
    def short = "WM"
    def id = "wehrmacht"
    def color = "#c9423b"
}

object Faction {
    val all = $[Faction](US, Wehrmacht)
}


// RESOURCES: Manpower, Munitions and Fuel are spent, Victory Points win; Experience is earned in combat

sealed abstract class Res(val name : String, val id : String) extends Record with Named with Styling {
    def icon : Elem = Hint(name, Image("res:" + id, styles.iconOnly, name))
    override def elem : Elem = (Image("res:" + id, styles.icon, "") ~ name.styled(this)).spn(xlo.nowrap)
}

case object Manpower extends Res("Manpower", "manpower")
case object Munitions extends Res("Munitions", "munitions")
case object Fuel extends Res("Fuel", "fuel")
case object VP extends Res("Victory Points", "vp")
case object XP extends Res("Experience", "xp")

object Res {
    val spent = $[Res](Manpower, Munitions, Fuel)
    val points = $[Res](VP, Manpower, Munitions, Fuel)
}

case class Cost(mp : Int, mun : Int, fuel : Int) extends Record {
    def list : $[(Res, Int)] = $[(Res, Int)]((Manpower, mp), (Munitions, mun), (Fuel, fuel)).filter(_._2 > 0)
    def elem : Elem = list./{ case (r, n) => n.hlb ~ r.icon }.join(" ")
}


// DAMAGE: each die is one point of one type; the Defense Matrix (the back of the Tier 3 Building Boards) says who resists what

sealed abstract class Dmg(val name : String, val short : String, val id : String) extends Record with Elementary {
    def icon : Elem = Hint(name, Image("dmg:" + id, styles.iconOnly, short))
    def elem = (Image("dmg:" + id, styles.icon, "") ~ name.hl).spn(xlo.nowrap)
}

case object AI extends Dmg("Anti-Infantry", "AI", "ai")
case object AP extends Dmg("Armor Piercing", "AP", "ap")
case object HE extends Dmg("High Explosive", "HE", "he")
case object Flame extends Dmg("Flame", "Flame", "flame")

// an attack die: its type, and whether it is indirect (fires over Buildings)
case class Attack(dmg : Dmg, indirect : Boolean = false) extends Record {
    def icon : Elem = dmg.icon ~ indirect.?(Hint("indirect", "⌒".hl)).|(Empty)
}

sealed abstract class Kind(val name : String, val letter : String, val heart : String) extends Record with Elementary {
    def elem = name.hl
}

case object Infantry extends Kind("Infantry", "G", "green")
case object LightVehicle extends Kind("Light Vehicle", "Y", "yellow")
case object HeavyVehicle extends Kind("Heavy Vehicle", "R", "red")
case object Emplacement extends Kind("Emplacement", "P", "purple")

object Matrix {
    // None: immune; true: a Defense Die for each such die (for HE, each hit or crit); false: no defense
    def resist(k : Kind, d : Dmg) : |[Boolean] = (k, d) match {
        case (HeavyVehicle, AI) => None
        case (HeavyVehicle, Flame) => None
        case (Infantry, AP) => |(true)
        case (LightVehicle, AI) => |(true)
        case (LightVehicle, HE) => |(true)
        case (HeavyVehicle, HE) => |(true)
        case (Emplacement, AI) => |(true)
        case (Emplacement, AP) => |(true)
        case _ => |(false)
    }

    def relevant(k : Kind, d : Dmg) = resist(k, d).any
}

// the white Damage Die: green and black faces succeed on defense; for HE, green is a crit (2), black a hit, red a miss
sealed abstract class Face(val name : String, val color : String) extends Record with Elementary {
    def success = color != "red"
    def crit = color == "green"
    def elem = Hint(name, Image("face:" + toString.toLowerCase, styles.die, name))
}

case object FaceAI extends Face("Anti-Infantry", "black")
case object FaceAP extends Face("Armor Piercing", "black")
case object FaceHE extends Face("High Explosive", "green")
case object FaceFlame extends Face("Flame", "red")
case object FaceSpeed extends Face("Speed", "green")
case object FaceSpent extends Face("Spent Speed", "red")

object Face {
    val all = $[Face](FaceAI, FaceAP, FaceHE, FaceFlame, FaceSpeed, FaceSpent)
    def die = CustomDie(all)
}


// UPGRADES: each unit buys each of its upgrades once (some twice), the XP one with Experience and the other with Munitions;
// some offer a choice ("or")

sealed abstract class Effect(val name : String) extends Record with Elementary {
    def elem : Elem = name.hl
}

case class AddAttack(attack : Attack) extends Effect("+1 " + attack.dmg.short + attack.indirect.??(" (indirect)")) {
    override def elem : Elem = "+1".hl ~ " " ~ attack.icon ~ " " ~ attack.dmg.name ~ attack.indirect.??(" (indirect)")
}
case object DefenseUp extends Effect("+1 Defense Die")
case object SpeedUp extends Effect("+1 Speed Die")
case object Camouflage extends Effect("Camouflage")
case object SpotsCamo extends Effect("+2 Sight, spots Camouflage")
case object ReinforceUp extends Effect("Veteran: Reinforce")
case object RangeUp extends Effect("+1 Range")
case object SpawnAdjacent extends Effect("Veteran: Infantry may Spawn next to it (Slow the first round)")

case class Slot(xp : Boolean, cost : Int, options : $[Effect], times : Int = 1) extends Record {
    def res : Res = xp.?(XP).|(Munitions)
}


// UNITS: the Building Boards. Health, range (the least range, then the most), damage, bonus abilities, cost, upgrades

sealed abstract class UnitType(val name : String, val faction : Faction, val tier : Int, val kind : Kind, val health : Int, val minRange : Int, val range : Int, val attacks : $[Attack], val cost : Cost, val upgrades : $[Slot]) extends Record with Elementary {
    def id = toString.toLowerCase
    def elem = name.styled(faction)
    def weaponTeam = false
    def machineGun = false
    def noBuildingSetup = false
    def wrenches = 0
    def transport = false
    def capture = kind == Infantry || kind == Emplacement
    def speed = (kind == LightVehicle).??(1)
    def sight = 2
    def reinforce = false
    def defense = 0
    def slow = 0
    def splash = false
    // how many may be on the map at once (vehicles by the component limits: 3 of each tier 1 and 2 vehicle, 2 of each tier 3)
    def limit = kind @@ {
        case LightVehicle | HeavyVehicle => (tier == 3).?(2).|(3)
        case _ => 99
    }
    def vehicle = kind == LightVehicle || kind == HeavyVehicle
}

object UnitType {
    val ai = Attack(AI)
    val ap = Attack(AP)
    val he = Attack(HE)
    val heIndirect = Attack(HE, true)
    val flame = Attack(Flame)
    val flameIndirect = Attack(Flame, true)

    val all = $[UnitType](Riflemen, USMortar, Jeep, FiftyCal, ATGun, Greyhound, Major, Sherman, Wolverine,
        Grenadiers, MG42, Kubelwagen, PanzerGrenadiers, WMortar, SdKfz222, Ostwind, PanzerIV, Brummbar)

    def of(f : Faction) = all.%(_.faction == f)
}

import UnitType._

// US HEADQUARTERS 1
case object Riflemen extends UnitType("Riflemen", US, 1, Infantry, 4, 1, 2, $(ai), Cost(4, 0, 0),
    $(Slot(true, 2, $(AddAttack(ap), AddAttack(ai))), Slot(false, 3, $(AddAttack(ap), AddAttack(ai))))) {
    override def wrenches = 1
}
case object USMortar extends UnitType("Mortar Team", US, 1, Infantry, 3, 2, 5, $(heIndirect), Cost(4, 0, 0),
    $(Slot(false, 2, $(SpeedUp)))) {
    override def id = "us-mortar"
    override def weaponTeam = true
    override def noBuildingSetup = true
}
case object Jeep extends UnitType("Willys Jeep", US, 1, LightVehicle, 2, 1, 2, $(ai), Cost(2, 0, 1),
    $(Slot(true, 1, $(ReinforceUp)), Slot(false, 2, $(AddAttack(ai))))) {
    override def transport = true
    override def capture = true
}
// US COMPANY COMMAND POST 2
case object FiftyCal extends UnitType(".50 Cal MG Team", US, 2, Infantry, 3, 1, 3, $(ai), Cost(3, 0, 0),
    $(Slot(true, 1, $(SpeedUp)), Slot(false, 3, $(AddAttack(flame))))) {
    override def id = "fifty-cal"
    override def weaponTeam = true
    override def machineGun = true
    override def sight = 3
}
case object ATGun extends UnitType("M1 57mm AT Gun", US, 2, Infantry, 3, 1, 4, $(ap, ap), Cost(4, 0, 0),
    $(Slot(true, 2, $(AddAttack(ap))), Slot(false, 3, $(Camouflage)))) {
    override def id = "at-gun"
    override def weaponTeam = true
    override def noBuildingSetup = true
}
case object Greyhound extends UnitType("M8 Greyhound", US, 2, LightVehicle, 3, 1, 3, $(ai, he), Cost(2, 0, 3),
    $(Slot(true, 1, $(DefenseUp)), Slot(false, 4, $(AddAttack(he)))))
// US BATTALION COMMAND POST 3
case object Major extends UnitType("Major", US, 3, Infantry, 3, 2, 3, $(heIndirect, heIndirect), Cost(3, 0, 0),
    $(Slot(true, 2, $(SpawnAdjacent)), Slot(false, 4, $(SpotsCamo)))) {
    override def reinforce = true
    override def splash = true
    override def limit = 1
}
case object Sherman extends UnitType("M4A3 Sherman", US, 3, HeavyVehicle, 4, 1, 2, $(ai, ai, ap), Cost(2, 0, 4),
    $(Slot(true, 1, $(SpeedUp)), Slot(false, 3, $(AddAttack(ap)))))
case object Wolverine extends UnitType("M10 Wolverine", US, 3, HeavyVehicle, 3, 1, 3, $(ap, ap), Cost(2, 0, 3),
    $(Slot(true, 3, $(AddAttack(ap))), Slot(false, 3, $(SpeedUp)))) {
    override def speed = 1
}

// WEHRMACHT HEADQUARTERS 1
case object Grenadiers extends UnitType("Grenadiers", Wehrmacht, 1, Infantry, 4, 1, 2, $(ai), Cost(4, 0, 0),
    $(Slot(true, 2, $(Camouflage)), Slot(false, 3, $(AddAttack(heIndirect))))) {
    override def wrenches = 1
}
case object MG42 extends UnitType("MG42 Team", Wehrmacht, 1, Infantry, 3, 1, 3, $(ai), Cost(4, 0, 0),
    $(Slot(true, 1, $(AddAttack(ai))), Slot(false, 3, $(AddAttack(ai), DefenseUp)))) {
    override def id = "mg42"
    override def weaponTeam = true
    override def machineGun = true
    override def sight = 3
}
case object Kubelwagen extends UnitType("Kubelwagen", Wehrmacht, 1, LightVehicle, 2, 1, 2, $(ai), Cost(2, 0, 1),
    $(Slot(true, 1, $(ReinforceUp)), Slot(false, 2, $(SpeedUp)))) {
    override def transport = true
    override def capture = true
}
// PANZERGRENADIER KOMPANIE 2
case object PanzerGrenadiers extends UnitType("Panzer Grenadiers", Wehrmacht, 2, Infantry, 5, 1, 3, $(ai, ap), Cost(5, 0, 0),
    $(Slot(true, 1, $(DefenseUp)), Slot(false, 3, $(AddAttack(ap)), 2))) {
    override def id = "panzer-grenadiers"
    override def wrenches = 1
}
case object WMortar extends UnitType("Mortar Team", Wehrmacht, 2, Infantry, 3, 2, 5, $(heIndirect), Cost(3, 0, 0),
    $(Slot(true, 2, $(AddAttack(flameIndirect))), Slot(false, 2, $(SpeedUp)))) {
    override def id = "wm-mortar"
    override def weaponTeam = true
    override def noBuildingSetup = true
}
case object SdKfz222 extends UnitType("SdKfz 222", Wehrmacht, 2, LightVehicle, 3, 1, 3, $(ai, ap), Cost(2, 0, 3),
    $(Slot(true, 1, $(SpeedUp)), Slot(false, 3, $(AddAttack(ai), DefenseUp)))) {
    override def id = "sdkfz-222"
}
// PANZER KORPS 3
case object Ostwind extends UnitType("Flakpanzer \"Ostwind\"", Wehrmacht, 3, HeavyVehicle, 3, 1, 2, $(ai, ai, ai), Cost(3, 0, 2),
    $(Slot(true, 2, $(AddAttack(ai))), Slot(false, 3, $(SpotsCamo)))) {
    override def speed = 1
}
case object PanzerIV extends UnitType("Panzer IV", Wehrmacht, 3, HeavyVehicle, 4, 1, 3, $(ai, ap, ap), Cost(2, 0, 4),
    $(Slot(true, 2, $(DefenseUp)), Slot(false, 2, $(AddAttack(ai))))) {
    override def id = "panzer-iv"
}
// the XP upgrade's other choice, HE Splash, is a Full Game rule
case object Brummbar extends UnitType("Sturmpanzer \"Brummbar\"", Wehrmacht, 3, HeavyVehicle, 5, 1, 3, $(he, he), Cost(1, 0, 5),
    $(Slot(true, 1, $(AddAttack(he))), Slot(false, 3, $(RangeUp)))) {
    override def defense = 1
    override def slow = 1
}

// the cost of unlocking a Tier Building (on the back of its board), the same for both factions
object Tiers {
    def cost(tier : Int) = tier @@ {
        case 2 => Cost(1, 0, 2)
        case 3 => Cost(1, 0, 3)
        case _ => Cost(0, 0, 0)
    }

    def name(f : Faction, tier : Int) = (f, tier) @@ {
        case (US, 1) => "US Headquarters"
        case (US, 2) => "Company Command Post"
        case (US, 3) => "Battalion Command Post"
        case (Wehrmacht, 1) => "Wehrmacht Headquarters"
        case (Wehrmacht, 2) => "Panzergrenadier Kompanie"
        case (Wehrmacht, 3) => "Panzer Korps"
        case _ => "Tier " + tier
    }

    def board(f : Faction, tier : Int) = f.id + "-" + tier
}

case class Squad(faction : Faction, utype : UnitType, n : Int) extends Record with Elementary {
    def elem = (Image("token:" + utype.id + "-" + faction.id, styles.icon, "") ~ utype.name.styled(faction) ~ (n > 1).?((" " + n).styled(faction) : Elem).|(Empty)).spn(xlo.nowrap)
    def kind = utype.kind
}

case class Bought(slot : Int, effect : Effect) extends Record

// a damage assignment: an attacker's dice on a target
case class Assignment(attacker : Squad, target : Squad, dice : $[Attack]) extends Record

// game length: the VP that end the game
sealed abstract class Length(val name : String, val vp : Int) extends GameOption with OneOfGroup with ImportantOption {
    val group = "Game length".txt
    def valueOn = name.hl ~ " (" ~ vp.hlb ~ " VP)"
    override val explain = $(
        "The game ends in the Supply Phase of the round a player reaches this many Victory Points. The rulebook suggests " ~ "Short".hl ~ " (" ~ 18.hl ~ ") for the first game.",
    )
}
case object ShortGame extends Length("Short", 18)
case object MediumGame extends Length("Medium", 21)
case object LongGame extends Length("Long", 24)

object Length {
    val all = $[Length](ShortGame, MediumGame, LongGame)
}


// VIEWS

object Images {
    def token(u : Squad) = Image("token:" + u.utype.id + "-" + u.faction.id, styles.icon, u.utype.name)
}


// MAP KEYS: actions a tap on the map can take

trait HexKey { self : UserAction =>
    def hex : Hex
}

trait SquadKey { self : UserAction =>
    def squad : Squad
}


// ACTIONS

case class StartAction(version : String) extends StartGameAction with GameVersion
case class FirstRolledAction(rolled : $[Face]) extends RolledAction[Face]

case class StartUnitsAction(f : Faction, n : Int) extends ForcedAction
case class StartPickAction(self : Faction, t : UnitType, n : Int) extends BaseAction("Starting Infantry", (n + 1).hlb, "of", 3.hlb)(Images.token(Squad(self, t, 0)) ~ t.elem) with Soft
case class StartPlaceAction(self : Faction, t : UnitType, hex : Hex, n : Int) extends BaseAction("Place", t.elem, "in the Spawn Area")(hex) with HexKey

case class RoundStartAction(f : Faction) extends ForcedAction

// maneuver: three Command Points a turn, alternating, four turns each
case class TurnAction(f : Faction, k : Int) extends ForcedAction
case class ManeuverAction(f : Faction, k : Int) extends ForcedAction
case class UnitMenuAction(self : Faction, squad : Squad, k : Int) extends BaseAction((g : Game) => g.cpNote(self))(squad, (g : Game) => g.unitNote(squad)) with Soft with SquadKey
case class MoveAction(self : Faction, squad : Squad, path : $[Hex], k : Int) extends BaseAction(squad, "in", (g : Game) => g.where(squad).elem)("Move to", path.last, (path.num > 1).?(("(through " + path.head.name + ", " + path.num + " CP)").txt).|(Empty), (g : Game) => g.costNote(squad, path.num)) with HexKey { def hex = path.last }
case class EmbarkAction(self : Faction, squad : Squad, vehicle : Squad, hex : Hex, k : Int) extends BaseAction(squad, "in", (g : Game) => g.where(squad).elem)("Board", vehicle, "in", hex, (g : Game) => g.costNote(squad, 1)) with HexKey
case class DisembarkAction(self : Faction, squad : Squad, hex : Hex, k : Int) extends BaseAction(squad, "in", (g : Game) => g.carrier(squad).get.elem)("Leave to", hex, (g : Game) => g.costNote(squad, 1)) with HexKey
case class SetUpAction(self : Faction, squad : Squad, k : Int) extends BaseAction(squad, "in", (g : Game) => g.where(squad).elem)("Set up to fire", (g : Game) => g.costNote(squad, 1))
case class PackUpAction(self : Faction, squad : Squad, k : Int) extends BaseAction(squad, "in", (g : Game) => g.where(squad).elem)("Pack up to move", (g : Game) => g.costNote(squad, 1))
case class SandbagsAction(self : Faction, squad : Squad, edge : Int, k : Int) extends BaseAction(squad, "builds Sandbags in", (g : Game) => g.where(squad).elem, (g : Game) => g.costNote(squad, 1))("Facing", Sandbags.facing(edge).hl)
case class EndTurnAction(self : Faction, k : Int) extends BaseAction(None)("End turn", (g : Game) => (g.cpLeft > 0).?(("(forfeit " + g.cpLeft + " CP)").spn(styles.small) : Elem).|(Empty))

// damage: machine guns first, then everyone, in turn order; then the defense rolls
case class DamagePhaseAction(f : Faction) extends ForcedAction
case class AssignNextAction(step : Int, order : $[Faction]) extends ForcedAction
case class TargetAction(self : Faction, unit : Squad, target : Squad, dice : $[Attack], step : Int, order : $[Faction]) extends BaseAction(unit, "in", (g : Game) => g.where(unit).elem, (g : Game) => g.assignNote(unit))("Attack", target, "in", (g : Game) => g.where(target).elem, "with", dice./(_.icon).merge) with SquadKey { def squad = target }
case class HoldFireAction(self : Faction, squad : Squad, step : Int, order : $[Faction]) extends BaseAction(squad, "in", (g : Game) => g.where(squad).elem, (g : Game) => g.assignNote(squad))("Hold fire")
case class ResolveDamageAction(f : Faction) extends ForcedAction
case class HERolledAction(f : Faction, rolled : $[Face]) extends RolledAction[Face]
case class DefenseRollAction(f : Faction, he : $[Face]) extends ForcedAction
case class DefenseRolledAction(f : Faction, he : $[Face], rolled : $[Face]) extends RolledAction[Face]
case class ApplyDamageAction(f : Faction, he : $[Face], defense : $[Face]) extends ForcedAction

// supply
case class SupplyPhaseAction(f : Faction) extends ForcedAction
case class PurchaseAction(f : Faction) extends ForcedAction
case class SpawnMenuAction(self : Faction, t : UnitType) extends BaseAction("Purchase and Spawn")(Images.token(Squad(self, t, 0)) ~ t.elem, "for", t.cost.elem) with Soft
case class SpawnAction(self : Faction, t : UnitType, hex : Hex) extends BaseAction("Spawn", t.elem, "for", t.cost.elem)(hex, (g : Game) => g.spawnNote(self, t, hex)) with HexKey
case class UpgradesMenuAction(self : Faction) extends BaseAction("Purchase")("Upgrades") with Soft
case class UpgradeAction(self : Faction, squad : Squad, slot : Int, effect : Effect) extends BaseAction(squad, "in", (g : Game) => g.where(squad).elem)(effect, "for", (g : Game) => g.slotCost(squad, slot)) with SquadKey
case class UnlockAction(self : Faction, tier : Int) extends BaseAction("Purchase")("Unlock", Tiers.name(self, tier).hl, ("(Tier " + tier + ")").txt, "for", Tiers.cost(tier).elem)
case class PassAction(self : Faction) extends BaseAction("Purchase")("Pass", "(no more purchases this round)".spn(styles.small))
case class PurchaseNextAction(f : Faction) extends ForcedAction
case class SupplyEndAction(f : Faction) extends ForcedAction

case class GameOverAction(winner : Faction) extends ForcedAction
case class GameOverWonAction(self : Faction, f : Faction) extends BaseInfo("Game Over")(f, "won")


object Sandbags {
    val names = $("north-east", "east", "south-east", "south-west", "west", "north-west")
    // a sandbag covers two neighboring edges of its hex
    def facing(e : Int) = names(e) + " and " + names((e + 1) % 6)
    def edges(e : Int) = $(e, (e + 1) % 6)
}


class Player(val faction : Faction) {
    var stock = Map[Res, Int](Manpower -> 4, Munitions -> 2, Fuel -> 3, VP -> 0, XP -> 0)
    var tier = 1
    var passed = false
}

class Game(val setup : $[Faction], val options : $[Meta.O]) extends BaseGame with ContinueGame with LoggedGame { game =>
    var isOver = false

    val factions : $[Faction] = setup

    val states = setup./(f => f -> new Player(f)).toMap

    implicit def faction2player(f : Faction) : Player = states(f)

    def team(f : Faction) = setup.indexOf(f)

    def enemy(f : Faction) = setup.but(f).only

    val length : Length = options.of[Length].single.|(ShortGame)

    // the map
    var pos = Map[Squad, Hex]()
    var carried = Map[Squad, Squad]()
    var health = Map[Squad, Int]()
    var bought = Map[Squad, $[Bought]]()
    var setUp = Set[Squad]()
    var pinned = Map[Squad, Squad]()
    // the round a unit Spawned next to the Major has Slow
    var slowed = Map[Squad, Int]()
    var sandbags = Map[Hex, Set[Int]]()
    var control = Map[Hex, Faction]()
    var spawned = Map[UnitType, Int]()

    // the round
    var round = 0
    var first : Faction = null
    var gameFirst : Faction = null
    var current : Faction = null
    var phase = "Setup"
    var cpLeft = 0
    var cp = Map[Squad, Int]()
    var sped = Map[Squad, Int]()
    var assignments = $[Assignment]()
    var decided = Set[Squad]()
    var lastRolls = $[Face]()

    var highlightFaction = $[Faction]()

    // SQUADS

    def units(f : Faction) : $[Squad] = pos.keys.$.%(_.faction == f).sortBy(u => (UnitType.all.indexOf(u.utype), u.n))
    def all : $[Squad] = setup./~(units)
    def where(u : Squad) : Hex = pos(u)
    def carrier(u : Squad) : |[Squad] = carried.get(u)
    def passenger(v : Squad) : |[Squad] = carried.find(_._2 == v).map(_._1)
    def isPassenger(u : Squad) = carried.contains(u)
    // the unit standing in a hex (a passenger is inside its vehicle)
    def occupant(h : Hex) : |[Squad] = pos.find { case (u, x) => x == h && isPassenger(u).not }.map(_._1)

    def has(u : Squad, e : Effect) = bought.getOrElse(u, $[Bought]()).exists(_.effect == e)
    def effects(u : Squad) = bought.getOrElse(u, $[Bought]())./(_.effect)
    def attacksOf(u : Squad) : $[Attack] = u.utype.attacks ++ effects(u).collect { case AddAttack(a) => a }
    def rangeOf(u : Squad) = u.utype.range + effects(u).count(RangeUp)
    def sightOf(u : Squad) = u.utype.sight + has(u, SpotsCamo).??(2)
    def spotsCamo(u : Squad) = has(u, SpotsCamo)
    def camouflaged(u : Squad) = has(u, Camouflage)
    def defenseOf(u : Squad) = u.utype.defense + effects(u).count(DefenseUp)
    def speedOf(u : Squad) = u.utype.speed + effects(u).count(SpeedUp)
    def reinforces(u : Squad) = u.utype.reinforce || has(u, ReinforceUp)
    def maxHealth(u : Squad) = u.utype.health
    def onBuilding(u : Squad) = Board.terrain(where(u)) == Building || Board.terrain(where(u)) == Ruin

    // the most CP a unit may take a round (Slow takes one away)
    def cap(u : Squad) = 3 - u.utype.slow - slowed.get(u).has(round).??(1)

    def onBoard(f : Faction, t : UnitType) = units(f).count(_.utype == t)

    // CONTROL POINTS and income: 1 of each resource, and 1 for each resource of each Control Point held

    def pointsOf(f : Faction) : $[Hex] = control.toList.filter(_._2 == f).map(_._1).sortBy(h => (h.r, h.c))

    def income(f : Faction, r : Res) : Int = (r != VP).??(1) + pointsOf(f)./(h => Board.points(h).count(r)).sum

    // MOVEMENT: one CP a hex; a Speed Die is a free CP used before any CP

    def freeLeft(u : Squad) = (cp.getOrElse(u, 0) == 0).??(speedOf(u) - sped.getOrElse(u, 0))
    def paidLeft(u : Squad) = min(cpLeft, cap(u) - cp.getOrElse(u, 0))
    def pointsLeft(u : Squad) = freeLeft(u) + paidLeft(u)

    def spend(u : Squad, n : Int) {
        0.until(n).foreach { _ =>
            if (freeLeft(u) > 0)
                sped += u -> (sped.getOrElse(u, 0) + 1)
            else {
                cp += u -> (cp.getOrElse(u, 0) + 1)
                cpLeft -= 1
            }
        }
    }

    def costNote(u : Squad, n : Int) : Elem = {
        val free = min(n, freeLeft(u))
        ("(" + (free > 0).??(free + " Speed" + (n > free).??(", ")) + (n > free).??((n - free) + " CP") + ")").spn(styles.small)
    }

    def cpNote(f : Faction) : Elem = "Maneuver: " ~ cpLeft.hlb ~ " CP left this turn"

    def unitNote(u : Squad) : Elem = {
        val l = $[Elem]() ++
            (freeLeft(u) > 0).$(("Speed " + freeLeft(u)).txt) ++
            $((cp.getOrElse(u, 0) + "/" + cap(u) + " CP").txt) ++
            setUp.contains(u).$("set up".txt)
        ("(" ~ l.join(", ") ~ ")").spn(styles.small)
    }

    def canEnter(u : Squad, h : Hex) = Board.terrain(h) != Water && (u.utype.vehicle.not || (Board.terrain(h) != Building && Board.terrain(h) != Ruin))

    def stepsFrom(u : Squad, h : Hex) = Board.neighbors(h).%(n => Board.joined(h, n) && canEnter(u, n))

    // the moves of a unit standing in a hex: into an empty hex, or through an occupied one into an empty one (both CP at once)
    def moves(u : Squad) : $[$[Hex]] = {
        val h = where(u)
        val left = pointsLeft(u)
        val one = stepsFrom(u, h)
        (left >= 1).??(one.%(occupant(_).none)./($(_))) ++
        (left >= 2).??(one.%(occupant(_).any)./~(m => stepsFrom(u, m).%(n => n != h && occupant(n).none && one.has(n).not)./($(m, _)))).distinctBy(_.last)
    }

    def transports(u : Squad) : $[Squad] = (u.kind == Infantry && pointsLeft(u) >= 1).??(stepsFrom(u, where(u))./~(occupant).%(v => v.faction == u.faction && v.utype.transport && passenger(v).none))

    def mayMove(u : Squad) = setUp.contains(u).not

    def unitActions(f : Faction, u : Squad, k : Int) : $[UserAction] = {
        val left = pointsLeft(u)
        if (left <= 0)
            $
        else
        if (isPassenger(u))
            stepsFrom(u, where(u)).%(occupant(_).none)./(h => DisembarkAction(f, u, h, k))
        else {
            val m = mayMove(u).??(moves(u)./(p => MoveAction(f, u, p, k)) ++ transports(u)./(v => EmbarkAction(f, u, v, where(v), k)))
            val s = u.utype.weaponTeam.??(setUp.contains(u).?($(PackUpAction(f, u, k))).|((u.utype.noBuildingSetup && onBuilding(u)).not.$(SetUpAction(f, u, k))))
            val b = (u.utype.wrenches > 0 && onBuilding(u).not).??(0.until(6).$.%(e => Sandbags.edges(e).forall(sandbags.getOrElse(where(u), Set()).contains).not)./(e => SandbagsAction(f, u, e, k)))
            m ++ s ++ b
        }
    }

    def moveSquad(u : Squad, h : Hex) {
        pos += u -> h
        passenger(u).foreach(p => pos += p -> h)
        if (u.kind == HeavyVehicle && sandbags.contains(h)) {
            sandbags -= h
            log(u, "crushed the Sandbags in", h)
        }
    }

    def removeSquad(u : Squad) {
        passenger(u).foreach(removeSquad)
        pos -= u
        carried -= u
        health -= u
        bought -= u
        setUp -= u
        pinned -= u
        pinned = pinned.filter(_._2 != u)
        slowed -= u
        cp -= u
        sped -= u
    }

    // SIGHT: a team sees what any of its units sees, within its sight and with no Building or Destroyed Building between them;
    // a Camouflaged unit only from next to it, or by a unit that spots Camouflage

    def sees(s : Squad, t : Squad) : Boolean = {
        val d = Board.distance(where(s), where(t))
        d <= sightOf(s) && Board.clear(where(s), where(t)) && (camouflaged(t).not || d <= 1 || spotsCamo(s))
    }

    def visible(f : Faction, t : Squad) = isPassenger(t).not && units(f).exists(s => sees(s, t))

    // ATTACKS

    def canFire(u : Squad) = pinned.contains(u).not && (u.utype.weaponTeam.not || setUp.contains(u)) && (isPassenger(u).not || u.utype.weaponTeam.not)

    // the dice a unit puts on a target: all of them with a clear line, only the indirect ones over Buildings
    def diceOn(u : Squad, t : Squad) : $[Attack] = {
        val d = Board.distance(where(u), where(t))
        if (d < u.utype.minRange || d > rangeOf(u) || d == 0 || visible(u.faction, t).not)
            $
        else
        if (Board.clear(where(u), where(t)))
            attacksOf(u)
        else
            attacksOf(u).%(_.indirect)
    }

    def targetsOf(u : Squad) : $[(Squad, $[Attack])] = canFire(u).??(units(enemy(u.faction)).map(t => (t, diceOn(u, t))).filter(_._2.nonEmpty))

    def assignNote(u : Squad) : Elem = ("(" + attacksOf(u)./(_.dmg.short).mkString(", ") + ", range " + (u.utype.minRange > 1).??(u.utype.minRange + "-") + rangeOf(u) + ")").spn(styles.small)

    def relevant(t : Squad, dice : $[Attack]) = dice.exists(a => Matrix.relevant(t.kind, a.dmg))

    // the defense a target gets from where it stands: Infantry in a Building 3 dice, in a Destroyed Building 1, else the best
    // Cover line (or Sandbags) crossed by an attack on it
    def staticCover(t : Squad, from : $[Hex]) : Int = {
        if (t.kind != Infantry)
            0
        else {
            val h = where(t)
            Board.terrain(h) @@ {
                case Building => 3
                case Ruin => 1
                case _ =>
                    from./{ x =>
                        val edges = Board.crossed(h, x)
                        val lines = edges./~(e => Board.cover.get((h, e)))./(_.dice)
                        val bags = edges.exists(e => sandbags.getOrElse(h, Set()).contains(e)).??(2)
                        (lines :+ bags).max
                    }.maxOr(0)
            }
        }
    }

    // TURN ORDER

    def other(f : Faction) = enemy(f)

    def turnOrder = $(first, other(first))

    def spawnHexes(f : Faction, t : UnitType) : $[Hex] = {
        val area = Board.spawn(team(f)).%(occupant(_).none)
        val transports = (t.kind == Infantry).??(Board.spawn(team(f))./~(occupant).%(v => v.faction == f && v.utype.transport && passenger(v).none)./(where))
        val major = (t.kind == Infantry).??(units(f).%(u => has(u, SpawnAdjacent) && isPassenger(u).not)./~(u => Board.neighbors(where(u)).%(n => Board.joined(where(u), n) && Board.terrain(n) != Water && occupant(n).none)))
        (area ++ transports ++ major).distinct.%(h => t.vehicle.not || (Board.terrain(h) != Building && Board.terrain(h) != Ruin))
    }

    def spawnNote(f : Faction, t : UnitType, h : Hex) : Elem =
        occupant(h).%(_.utype.transport)./(v => ("(in " + v.utype.name + ")").spn(styles.small) : Elem).|(Board.spawn(team(f)).has(h).not.?("(next to the Major, Slow this round)".spn(styles.small) : Elem).|(Empty))

    def affordable(f : Faction, c : Cost) = f.stock(Manpower) >= c.mp && f.stock(Munitions) >= c.mun && f.stock(Fuel) >= c.fuel

    def pay(f : Faction, c : Cost) {
        f.stock += Manpower -> (f.stock(Manpower) - c.mp)
        f.stock += Munitions -> (f.stock(Munitions) - c.mun)
        f.stock += Fuel -> (f.stock(Fuel) - c.fuel)
    }

    def gain(f : Faction, r : Res, n : Int) {
        f.stock += r -> (f.stock(r) + n)
    }

    def buyable(f : Faction) : $[UnitType] = UnitType.of(f).%(t => t.tier <= f.tier && affordable(f, t.cost) && onBoard(f, t) < t.limit && spawnHexes(f, t).any)

    def slotCost(u : Squad, slot : Int) : Elem = {
        val s = u.utype.upgrades(slot)
        s.cost.hlb ~ s.res.icon
    }

    def upgrades(f : Faction) : $[(Squad, Int, Effect)] = units(f)./~{ u =>
        u.utype.upgrades.zipWithIndex./~{ case (s, i) =>
            val n = bought.getOrElse(u, $[Bought]()).count(_.slot == i)
            (n < s.times && f.stock(s.res) >= s.cost).??(s.options.%(e => s.times > 1 || n == 0)./(e => (u, i, e)))
        }
    }

    def unlockable(f : Faction) : |[Int] = (f.tier < 3).?(f.tier + 1).%(t => affordable(f, Tiers.cost(t)))

    def loggedPerform(action : Action, soft : Void) : Continue = {
        val c = performInternal(action, soft)

        highlightFaction = c match {
            case Ask(f, _) => $(f)
            case MultiAsk(a, _) => a./(_.faction)
            case _ => $
        }

        c
    }

    def info(waiting : $[Faction], self : |[Faction], actions : $[UserAction]) : $[Info] = $

    def createSquad(f : Faction, t : UnitType, h : Hex) : Squad = {
        val n = spawned.getOrElse(t, 0) + 1
        spawned += t -> n
        val u = Squad(f, t, n)
        occupant(h).%(v => v.faction == f && v.utype.transport && t.kind == Infantry).foreach { v => carried += u -> v }
        pos += u -> h
        health += u -> t.health
        u
    }

    // the damage each target takes, from the HE dice rolled and the Defense Dice rolled (None to count the Defense Dice needed)
    case class Hit(target : Squad, he : $[Face], defense : Int, damage : Int)

    def damageTargets = assignments./(_.target).distinct.%(pos.contains)

    def resolve(he : $[Face], defense : |[$[Face]]) : $[Hit] = {
        var heLeft = he
        var defLeft = defense.|($)
        damageTargets./{ t =>
            val l = assignments.%(_.target == t)
            val dice = l./~(_.dice).%(a => Matrix.resist(t.kind, a.dmg).any)
            val heN = dice.count(_.dmg == HE)
            val faces = heLeft.take(heN)
            heLeft = heLeft.drop(heN)
            val heDamage = faces./(f => f.crit.?(2).|(f.success.??(1))).sum
            val plain = dice.count(a => a.dmg == AI || a.dmg == AP)
            val flame = dice.count(_.dmg == Flame)
            val resisted = dice.count(a => (a.dmg == AI || a.dmg == AP) && Matrix.resist(t.kind, a.dmg).has(true)) + Matrix.resist(t.kind, HE).has(true).??(faces.count(_.success))
            val n = resisted + staticCover(t, l./(a => where(a.attacker)).distinct) + defenseOf(t)
            val rolled = defLeft.take(n)
            defLeft = defLeft.drop(n)
            val saved = rolled.count(_.success)
            Hit(t, faces, n, max(0, plain + heDamage - saved) + flame)
        }
    }

    def performInternal(a : Action, soft : Void) : Continue = {
        implicit val action = a

        action match {
            // SETUP
            case StartAction(version) =>
                log("HRF".hl, "version", gaming.version.hlb)
                log("Company of Heroes".hlb, "(2nd Edition, First Game Rules)", "on", "Moro River".hl)

                if (version != gaming.version)
                    log("Saved game version", version.hlb)

                log("Players", setup./(_.elem).comma)
                log("The game ends at", length.vp.hlb, "Victory Points")

                // each team rolls three Damage Dice; the most green, then black, symbols goes first
                Roll[Face](6.times(Face.die), FirstRolledAction(_))

            case FirstRolledAction(l) =>
                lastRolls = l
                val a = l.take(3)
                val b = l.drop(3)
                def score(x : $[Face]) = x.count(_.crit) * 10 + x.count(f => f.success && f.crit.not)
                log(setup(0), "rolled", a./(_.elem).merge, "and", setup(1), "rolled", b./(_.elem).merge)
                if (score(a) == score(b)) {
                    log("A tie, rolling again")
                    Roll[Face](6.times(Face.die), FirstRolledAction(_))
                }
                else {
                    first = (score(a) > score(b)).?(setup(0)).|(setup(1))
                    gameFirst = first
                    log(first, "goes first;", other(first), "places its starting Infantry first")
                    StartUnitsAction(other(first), 0)
                }

            case StartUnitsAction(f, 3) =>
                if (f != first)
                    StartUnitsAction(first, 0)
                else
                    Milestone(RoundStartAction(first))

            case StartUnitsAction(f, n) =>
                current = f
                Ask(f).each(UnitType.of(f).%(t => t.tier == 1 && t.kind == Infantry))(t => StartPickAction(f, t, n))

            case StartPickAction(f, t, n) =>
                Ask(f).each(Board.spawn(team(f)).%(occupant(_).none))(h => StartPlaceAction(f, t, h, n)).cancel

            case StartPlaceAction(f, t, h, n) =>
                val u = createSquad(f, t, h)
                log(f, "placed", u, "in", h)
                StartUnitsAction(f, n + 1)

            // MANEUVER PHASE: the first team alternates; four turns of 3 CP each
            case RoundStartAction(f) =>
                round += 1
                first = f
                phase = "Maneuver"
                cp = Map()
                sped = Map()
                log(DoubleLine)
                log("Round", ("#" + round).hl, "-", f, "goes first")
                TurnAction(f, 0)

            case TurnAction(f, k) =>
                if (k >= 8)
                    DamagePhaseAction(first)
                else {
                    current = f
                    cpLeft = 3
                    log("Turn", (k / 2 + 1).hl, "of", 4.hl, "for", f)
                    ManeuverAction(f, k)
                }

            case ManeuverAction(f, k) =>
                val ready = units(f).%(u => unitActions(f, u, k).any)
                if (cpLeft == 0 && ready.none)
                    TurnAction(other(f), k + 1)
                else
                    Ask(f)
                        .group(cpNote(f))
                        .each(ready)(u => UnitMenuAction(f, u, k))
                        .add(EndTurnAction(f, k))

            case UnitMenuAction(f, u, k) =>
                Ask(f).add(unitActions(f, u, k)).cancel

            case MoveAction(f, u, path, k) =>
                val from = where(u)
                spend(u, path.num)
                moveSquad(u, path.last)
                log(u, "moved from", from, "to", path.last, (path.num > 1).??("through " + path.head.name))
                ManeuverAction(f, k)

            case EmbarkAction(f, u, v, h, k) =>
                spend(u, 1)
                carried += u -> v
                pos += u -> h
                log(u, "boarded", v, "in", h)
                ManeuverAction(f, k)

            case DisembarkAction(f, u, h, k) =>
                spend(u, 1)
                val v = carried(u)
                carried -= u
                pos += u -> h
                log(u, "left", v, "to", h)
                ManeuverAction(f, k)

            case SetUpAction(f, u, k) =>
                spend(u, 1)
                setUp += u
                log(u, "set up in", where(u))
                ManeuverAction(f, k)

            case PackUpAction(f, u, k) =>
                spend(u, 1)
                setUp -= u
                log(u, "packed up in", where(u))
                ManeuverAction(f, k)

            case SandbagsAction(f, u, e, k) =>
                spend(u, 1)
                sandbags += where(u) -> (sandbags.getOrElse(where(u), Set()) ++ Sandbags.edges(e))
                log(u, "built Sandbags in", where(u), "facing", Sandbags.facing(e))
                ManeuverAction(f, k)

            case EndTurnAction(f, k) =>
                if (cpLeft > 0)
                    log(f, "forfeited", cpLeft.hl, "CP")
                TurnAction(other(f), k + 1)

            // DAMAGE PHASE: machine guns pin first, then every unit picks one target; all damage is simultaneous
            case DamagePhaseAction(f) =>
                phase = "Damage"
                current = null
                cpLeft = 0
                assignments = $
                decided = Set()
                log(DoubleLine)
                log("Damage Phase")
                AssignNextAction(1, turnOrder)

            case AssignNextAction(1, Nil) =>
                assignments.foreach { a =>
                    val t = a.target
                    if (t.kind == Infantry && Board.terrain(where(t)) != Building && Board.terrain(where(t)) != Ruin && isPassenger(t).not && (t.utype.machineGun && setUp.contains(t)).not && pinned.contains(t).not) {
                        pinned += t -> a.attacker
                        log(t, "was pinned by", a.attacker)
                    }
                }

                // units on the other team's Control Points neutralize them, unless pinned
                control.toList.foreach { case (h, f) =>
                    occupant(h).%(u => u.faction != f && pinned.contains(u).not).foreach { u =>
                        control -= h
                        log(u, "neutralized", f, "Control Point in", h)
                    }
                }

                AssignNextAction(2, turnOrder)

            case AssignNextAction(2, Nil) =>
                // experience: 1 for each player, and 1 for each enemy unit it assigned damage that can hurt it
                setup.foreach { f =>
                    val hit = assignments.%(_.attacker.faction == f).%(a => relevant(a.target, a.dice))./(_.target).distinct
                    gain(f, XP, 1 + hit.num)
                    log(f, "gained", (1 + hit.num).hl, "XP")
                }
                ResolveDamageAction(first)

            case AssignNextAction(step, f :: rest) =>
                current = f
                val l = units(f).%(u => decided.contains(u).not).%(u => step == 2 || (u.utype.machineGun && setUp.contains(u))).%(u => targetsOf(u).any)
                l.starting match {
                    case Some(u) =>
                        Ask(f).each(targetsOf(u)) { case (t, d) => TargetAction(f, u, t, d, step, f :: rest) }.add(HoldFireAction(f, u, step, f :: rest))
                    case None =>
                        AssignNextAction(step, rest)
                }

            case TargetAction(f, u, t, d, step, order) =>
                assignments :+= Assignment(u, t, d)
                decided += u
                log(u, "attacks", t, "with", d./(_.icon).merge)
                AssignNextAction(step, order)

            case HoldFireAction(f, u, step, order) =>
                decided += u
                AssignNextAction(step, order)

            case ResolveDamageAction(f) =>
                val n = damageTargets./(t => assignments.%(_.target == t)./~(_.dice).count(a => a.dmg == HE && Matrix.resist(t.kind, HE).any)).sum
                if (n > 0)
                    Roll[Face](n.times(Face.die), HERolledAction(f, _))
                else
                    DefenseRollAction(f, $)

            // (no oracle action without its roll: a replay only finds them where something was rolled)
            case HERolledAction(f, he) =>
                log("High Explosive dice rolled", he./(_.elem).merge)
                DefenseRollAction(f, he)

            case DefenseRollAction(f, he) =>
                val n = resolve(he, None)./(_.defense).sum
                if (n > 0)
                    Roll[Face](n.times(Face.die), DefenseRolledAction(f, he, _))
                else
                    ApplyDamageAction(f, he, $)

            case DefenseRolledAction(f, he, defense) =>
                ApplyDamageAction(f, he, defense)

            case ApplyDamageAction(f, he, defense) =>
                lastRolls = he ++ defense
                if (defense.any)
                    log("Defense dice rolled", defense./(_.elem).merge)

                val hits = resolve(he, |(defense))

                hits.foreach { h =>
                    health += h.target -> (health(h.target) - h.damage)
                    log(h.target, "in", where(h.target), "took", h.damage.hl, "damage", (h.defense > 0).??("(" + h.defense + " Defense Dice)"))
                }

                // two or more damage dice on a hex destroy its Sandbags
                sandbags.keys.foreach { x =>
                    if (assignments.%(a => pos.get(a.target).has(x))./(_.dice.num).sum >= 2) {
                        sandbags -= x
                        log("The Sandbags in", x, "were destroyed")
                    }
                }

                // destroyed units: a VP to the other team, an XP to their owner
                hits./(_.target).%(t => health(t) <= 0).foreach { t =>
                    val lost = t +: passenger(t).$
                    lost.foreach { x =>
                        log(x, "was destroyed")
                        gain(other(x.faction), VP, 1)
                        gain(x.faction, XP, 1)
                    }
                    removeSquad(t)
                }

                assignments = $

                SupplyPhaseAction(first)

            // SUPPLY PHASE
            case SupplyPhaseAction(f) =>
                phase = "Supply"
                current = null
                log(DoubleLine)
                log("Supply Phase")

                control.toList.foreach { case (h, e) =>
                    occupant(h).%(u => u.faction != e && pinned.contains(u).not).foreach { u =>
                        control -= h
                        log(u, "neutralized", e, "Control Point in", h)
                    }
                }

                Board.points.keys.$.sortBy(h => (h.r, h.c)).foreach { h =>
                    if (control.contains(h).not)
                        occupant(h).%(u => (u.kind == Infantry && pinned.contains(u).not) || u.kind == Emplacement || (u.utype.vehicle && u.utype.capture)).foreach { u =>
                            control += h -> u.faction
                            log(u, "captured the", Board.points(h)./(_.elem).comma, "Control Point in", h)
                        }
                }

                setup.foreach { e =>
                    Res.points.foreach(r => gain(e, r, income(e, r)))
                    log(e, "collected", Res.points./(r => income(e, r).hl ~ r.icon).join(" "))
                }

                val ended = setup.%(_.stock(VP) >= length.vp)

                if (ended.any) {
                    val vps = setup./(e => e -> pointsOf(e).count(h => Board.points(h).has(VP)))
                    val most = vps.map(_._2).max
                    vps.filter(_._2 == most).single.foreach { case (e, n) =>
                        gain(e, VP, 3)
                        log(e, "gained", 3.hl, "VP for holding the most Victory Point Control Points")
                    }
                    val best = setup./(_.stock(VP)).max
                    val winner = setup.%(_.stock(VP) == best).single.|(gameFirst)
                    GameOverAction(winner)
                }
                else {
                    setup.foreach(_.passed = false)
                    PurchaseAction(first)
                }

            case PurchaseAction(f) =>
                if (f.passed)
                    PurchaseNextAction(f)
                else {
                    current = f
                    Ask(f)
                        .group("Supply:", Res.spent./(r => f.stock(r).hlb ~ r.icon).join(" "), f.stock(XP).hlb ~ XP.icon)
                        .each(buyable(f))(t => SpawnMenuAction(f, t))
                        .add(upgrades(f).any.$(UpgradesMenuAction(f)))
                        .add(unlockable(f)./(t => UnlockAction(f, t)).$)
                        .add(PassAction(f))
                }

            case SpawnMenuAction(f, t) =>
                Ask(f).each(spawnHexes(f, t))(h => SpawnAction(f, t, h)).cancel

            case SpawnAction(f, t, h) =>
                pay(f, t.cost)
                val u = createSquad(f, t, h)
                if (Board.spawn(team(f)).has(h).not && isPassenger(u).not)
                    slowed += u -> (round + 1)
                log(f, "spawned", u, "in", h, "for", t.cost.elem)
                PurchaseNextAction(f)

            case UpgradesMenuAction(f) =>
                Ask(f).each(upgrades(f)) { case (u, i, e) => UpgradeAction(f, u, i, e) }.cancel

            case UpgradeAction(f, u, i, e) =>
                val s = u.utype.upgrades(i)
                gain(f, s.res, -s.cost)
                bought += u -> (bought.getOrElse(u, $[Bought]()) :+ Bought(i, e))
                log(f, "upgraded", u, "with", e, "for", s.cost.hl, s.res.icon)
                PurchaseNextAction(f)

            case UnlockAction(f, t) =>
                pay(f, Tiers.cost(t))
                f.tier = t
                log(f, "unlocked", Tiers.name(f, t).hl, "(Tier " + t + ")")
                PurchaseNextAction(f)

            case PassAction(f) =>
                f.passed = true
                log(f, "passed")
                PurchaseNextAction(f)

            case PurchaseNextAction(f) =>
                if (other(f).passed.not)
                    PurchaseAction(other(f))
                else
                if (f.passed.not)
                    PurchaseAction(f)
                else
                    SupplyEndAction(first)

            case SupplyEndAction(f) =>
                pinned = Map()

                // reinforce: Infantry in the Spawn Area, next to allies that Reinforce, or carried by one, heal 1 for each
                setup./~(units).%(_.kind == Infantry).foreach { u =>
                    val h = where(u)
                    val area = (isPassenger(u).not && Board.spawn(team(u.faction)).has(h)).??(1)
                    val near = units(u.faction).%(v => v != u && reinforces(v) && Board.joined(h, where(v)) && isPassenger(v).not).num
                    val ride = carrier(u).%(reinforces).any.??(1)
                    val heal = min(maxHealth(u) - health(u), area + near + ride)
                    if (heal > 0) {
                        health += u -> (health(u) + heal)
                        log(u, "was reinforced by", heal.hl)
                    }
                }

                // repair: each Wrench repairs an adjacent allied vehicle or emplacement by 1 (the most damaged)
                setup./~(units).%(_.utype.wrenches > 0).foreach { u =>
                    1.to(u.utype.wrenches).foreach { _ =>
                        units(u.faction).%(v => v != u && (v.utype.vehicle || v.kind == Emplacement) && carrier(u).has(v).not && health(v) < maxHealth(v) && Board.joined(where(u), where(v)))
                            .sortBy(v => health(v) * 100 / maxHealth(v)).starting.foreach { v =>
                                health += v -> (health(v) + 1)
                                log(u, "repaired", v)
                            }
                    }
                }

                Milestone(RoundStartAction(other(first)))

            case GameOverAction(w) =>
                isOver = true
                current = null
                setup.foreach(f => log(f, "finished with", f.stock(VP).hlb, "VP"))
                log(w, "won")
                GameOver($(w), "Game Over", $(GameOverWonAction(null, w)))
        }
    }
}
