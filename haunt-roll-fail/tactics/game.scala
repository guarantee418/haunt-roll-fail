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
// The rules are the core rulebook's First Game Rules, and with the "Full Game Rules" option (every new game, 2026-11-24) its Full Game Rules:
// the Operations deck, Retreats, Assaults, Overwatch, firing arcs and Heavy Vehicle facing, HE Splash, Frontal Armor, destructible Buildings
// and Moro River's permanent tank traps (RULES.md has them as played and the choices made). Games without the option play the First Game
// Rules as before. The units are the US Forces' and the Wehrmacht's three Building Boards, read off the Tabletop Simulator mod's images.

trait Faction extends BasePlayer with Named with Styling with Record {
    def short : String
    def id : String
    def color : String
    // the side in a team game (2026-11-24): US, British and Soviet are Allies, the Wehrmacht and OKW Axis
    def axis : Boolean = false
    // a second seat of a faction (2026-11-27): it plays its first's boards, units, Commanders and art (`id`), as the rulebook lets
    // the same faction play twice in a team game; the first itself is its own base
    def base : Faction = this
    def second : Boolean = base != this
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
    override def axis = true
    def color = "#c9423b"
}

// the other three core factions' Building Boards (2026-11-24), read off the mod's images like the first two
case object British extends Faction {
    def name = "British Forces"
    def short = "BR"
    def id = "british"
    def color = "#c8a24a"
}

case object Soviet extends Faction {
    def name = "Soviet Forces"
    def short = "SU"
    def id = "soviet"
    def color = "#a0203c"
}

case object OKW extends Faction {
    def name = "Oberkommando West"
    def short = "OKW"
    def id = "okw"
    override def axis = true
    def color = "#5d7a99"
}

// the second seats (2026-11-27, for 3v3 until the Desert Warfare expansion's Afrika Korps is in): "Wehrmacht II" and so on, a team game's
// second player of a faction; the name, short and colour are its own, everything else its first's
sealed abstract class SecondFaction(val first : Faction, val color : String) extends Faction {
    def name = first.name + " II"
    def short = first.short + "2"
    def id = first.id
    override def axis = first.axis
    override def base = first
}

case object US2 extends SecondFaction(US, "#4f7a2a")
case object Wehrmacht2 extends SecondFaction(Wehrmacht, "#9c2a24")
case object British2 extends SecondFaction(British, "#9c7a2e")
case object Soviet2 extends SecondFaction(Soviet, "#6e1028")
case object OKW2 extends SecondFaction(OKW, "#3b5571")

object Faction {
    val all = $[Faction](US, Wehrmacht, British, Soviet, OKW)
    val seconds = $[Faction](US2, Wehrmacht2, British2, Soviet2, OKW2)
    val seats = all ++ seconds
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
// Full Game Rules: all of the unit's HE Damage becomes HE Splash (the US Mortar's XP upgrade, an option of the Brummbar's)
case object SplashUp extends Effect("HE Splash")
// Commanders: an Elite AT Gun's +1 Health; the British, Soviet and OKW boards' health upgrades (the unit gains the health at once)
case object HealthUp extends Effect("+1 Health")
// the British, Soviet and OKW boards: +1 Sight (an eye), +1 Sight and +1 Range (an eye in a crosshair), two more dice in one upgrade,
// Remove Slow, Heroic Sacrifice (no VP for the enemy when destroyed), and the Universal Carrier's flamethrower (no more Transport or Capture)
case object SightUp extends Effect("+1 Sight")
case object SightRangeUp extends Effect("+1 Sight and +1 Range")
case class AddAttacks(attack : Attack) extends Effect("+2 " + attack.dmg.short) {
    override def elem : Elem = "+2".hl ~ " " ~ attack.icon ~ attack.icon ~ " " ~ attack.dmg.name
}
case object RemoveSlow extends Effect("Remove Slow")
case object HeroicSacrifice extends Effect("Heroic Sacrifice (no VP for the enemy when destroyed)")
case object FlameRefit extends Effect("+1 Flame (no more Transport or Capture)")
// the British Thompson SMGs (Lend Lease Assault Tactics): two Close Range Firepower dice
case object CloseFire extends Effect("+2 Close Range Firepower")

case class Slot(xp : Boolean, cost : Int, options : $[Effect], times : Int = 1) extends Record {
    def res : Res = xp.?(XP).|(Munitions)
}


// UNITS: the Building Boards. Health, range (the least range, then the most), damage, bonus abilities, cost, upgrades

sealed abstract class UnitType(val name : String, val faction : Faction, val tier : Int, val kind : Kind, val health : Int, val minRange : Int, val range : Int, val attacks : $[Attack], val cost : Cost, val upgrades : $[Slot]) extends Record with Elementary {
    def id = toString.toLowerCase
    def elem = name.styled(faction)
    def weaponTeam = false
    // set up to fire, picking a facing in the Full Game Rules: Weapon Teams, and Emplacements (which never move)
    // (not an Emplacement with no attack: the OKW Battlegroup)
    def setsUp = (weaponTeam || kind == Emplacement) && attacks.any
    // how many of its base AI dice are Close Range Firepower, fired only at range 1
    def close = 0
    // brought by a Commander: bought only once its tier is unlocked (`Commanders`)
    def commander = false
    // the vehicles sharing a chassis count toward one limit (the Easy Eight is a Sherman)
    def chassis : UnitType = this
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
    // Full Game Rules: a Retreat Point for its team's Infantry (the Major)
    def retreatPoint = false
    // Full Game Rules: the damage types a Heavy Vehicle fires from a fixed gun (straight arrows on its board, a forward Firing Arc);
    // the others are turreted (curved arrows, any direction)
    def fixed : $[Dmg] = $
    // how many may be on the map at once (vehicles by the component limits: 3 of each tier 1 and 2 vehicle, 2 of each tier 3)
    def limit = kind @@ {
        case LightVehicle | HeavyVehicle => (tier == 3).?(2).|(3)
        case _ => 99
    }
    def vehicle = kind == LightVehicle || kind == HeavyVehicle
    // effects it always has (Obersoldaten's Camouflage, the OKW Battlegroup's Spots Camo)
    def innate : $[Effect] = $
    // an Emplacement built by Infantry with Wrenches (British, OKW): the unit types that build it; it isn't spawned
    def builtBy : $[UnitType] = $
    // an OKW Tier Building: an Emplacement a SWS Supply Truck converts into; it isn't spawned
    def tierBuilding = 0
    def purchasable = builtBy.none && tierBuilding == 0
    // the Centaur's Strafing Fire: its damage may also go, in full, to a second unit next to its target
    def strafing = false
    // the T-70: for 1 CP, toggles a No-Fire mode in which it captures
    def captureMode = false
    // an Emplacement built in an empty Building (the Soviet Forward HQ)
    def inBuildings = false
    // the other damage it may pick for an attack ("picks either damage type each attack"), and the range of that attack
    def alt : $[Attack] = $
    def altRange = range
}

object UnitType {
    val ai = Attack(AI)
    val ap = Attack(AP)
    val he = Attack(HE)
    val heIndirect = Attack(HE, true)
    val flame = Attack(Flame)
    val flameIndirect = Attack(Flame, true)

    // (new types go at the end: units are listed in this order)
    lazy val all = $[UnitType](Riflemen, USMortar, Jeep, FiftyCal, ATGun, Greyhound, Major, Sherman, Wolverine,
        Grenadiers, MG42, Kubelwagen, PanzerGrenadiers, WMortar, SdKfz222, Ostwind, PanzerIV, Brummbar,
        Priest, EasyEight, AssaultEngineers, Tiger, Osttruppen, Stormtroopers, Pak43, MGBunker,
        InfantrySection, Vickers, UniversalCarrier, SixPounder, MortarEmplacement, Bofors, Cromwell, Centaur, Churchill,
        Conscripts, SovietMortar, ScoutCar, ZiS3, SniperSquad, T70, T34, SU76, SU85,
        Sturmpioneers, Volksgrenadiers, SWSTruck, Raketenwerfer, StukaHalftrack, Luchs, Obersoldaten, Panther, Sturmtiger,
        Battlegroup, SchwererHQ, Flak37,
        RoyalEngineers, ForwardEmplacement, SeventeenPounder, ChurchillAVRE, Staghound, APSniper, DShK, T3485, ForwardHQ,
        Panzerfusilier, JaegerRecon, Puma, LeFH18, OKWPak43, Kettenkrad, StG44Volksgrenadiers,
        Stuart, Pershing, Calliope, Dozer, Jackson, Pathfinder, CavalryRiflemen, DevilsBrigade, USMGBunker,
        StuG, KingTiger, JaegerSniper, Jagdpanther, Elefant, Halftrack,
        Sexton, Comet, Firefly, Valentine, Commandos,
        ShockTroops, IS2, GuardsRifle, AIPartisans, ATPartisans, ForwardStation, KV2, PenalBattalion, ISU152, KV8,
        Hetzer,
        EliteGrenadiers, CommandPantherUS, CommandPantherWM, CommandPantherBR, CommandPantherSU, CommandPantherOKW)

    // the Wehrmacht units the OKW's expansion Commanders bring too (the StuG Pack's shared Commanders, and the ECC's King Tiger and
    // Jagdpanther): an OKW squad of the Wehrmacht's type, drawn with its own token
    lazy val shared = $[(UnitType, Faction)](StuG -> OKW, KingTiger -> OKW, Jagdpanther -> OKW, Stormtroopers -> OKW)

    def of(f : Faction) = all.%(_.faction == f.base)
}

import UnitType._

// US HEADQUARTERS 1
case object Riflemen extends UnitType("Riflemen", US, 1, Infantry, 4, 1, 2, $(ai), Cost(4, 0, 0),
    $(Slot(true, 2, $(AddAttack(ap), AddAttack(ai))), Slot(false, 3, $(AddAttack(ap), AddAttack(ai))))) {
    override def wrenches = 1
}
// the XP upgrade, HE Splash, is a Full Game rule (added after the Munitions one, so the upgrades older games recorded keep their numbers)
case object USMortar extends UnitType("Mortar Team", US, 1, Infantry, 3, 2, 5, $(heIndirect), Cost(4, 0, 0),
    $(Slot(false, 2, $(SpeedUp)), Slot(true, 1, $(SplashUp)))) {
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
    override def retreatPoint = true
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
// the hull machine gun (AI, and the Munitions upgrade's AI) is fixed, the main gun (AP) turreted
case object PanzerIV extends UnitType("Panzer IV", Wehrmacht, 3, HeavyVehicle, 4, 1, 3, $(ai, ap, ap), Cost(2, 0, 4),
    $(Slot(true, 2, $(DefenseUp)), Slot(false, 2, $(AddAttack(ai))))) {
    override def id = "panzer-iv"
    override def fixed = $(AI)
}
// the XP upgrade's other choice, HE Splash, is a Full Game rule; its gun is fixed
case object Brummbar extends UnitType("Sturmpanzer \"Brummbar\"", Wehrmacht, 3, HeavyVehicle, 5, 1, 3, $(he, he), Cost(1, 0, 5),
    $(Slot(true, 1, $(AddAttack(he), SplashUp)), Slot(false, 3, $(RangeUp)))) {
    override def defense = 1
    override def slow = 1
    override def fixed = $(HE)
}

// BRITISH HEADQUARTERS 1
case object InfantrySection extends UnitType("Infantry Section", British, 1, Infantry, 4, 1, 2, $(ai), Cost(4, 0, 0),
    $(Slot(true, 1, $(HealthUp)), Slot(false, 3, $(AddAttack(ap), SightRangeUp)))) {
    override def id = "infantry-section"
    override def wrenches = 1
}
case object Vickers extends UnitType("Vickers .303 Machine Gun", British, 1, Infantry, 3, 1, 3, $(ai), Cost(4, 0, 0),
    $(Slot(true, 1, $(SpeedUp)), Slot(false, 2, $(HealthUp)))) {
    override def weaponTeam = true
    override def machineGun = true
    override def sight = 3
}
// its two Munitions upgrades are a flamethrower each; the first ends its Transport and Capture
case object UniversalCarrier extends UnitType("Universal Carrier", British, 1, LightVehicle, 2, 1, 2, $(ai), Cost(2, 0, 1),
    $(Slot(true, 1, $(DefenseUp)), Slot(false, 3, $(FlameRefit), 2))) {
    override def id = "universal-carrier"
    override def transport = true
    override def capture = true
}
// PLATOON COMMAND POST 2: the two Emplacements are built by Infantry Sections in the Supply Phase
case object SixPounder extends UnitType("6 Pounder Anti-Tank Gun", British, 2, Infantry, 4, 1, 4, $(ap), Cost(3, 0, 0),
    $(Slot(true, 1, $(AddAttack(ap))), Slot(false, 2, $(SpeedUp)))) {
    override def id = "six-pounder"
    override def weaponTeam = true
    override def noBuildingSetup = true
}
case object MortarEmplacement extends UnitType("Mortar Emplacement", British, 2, Emplacement, 3, 2, 5, $(heIndirect, heIndirect), Cost(2, 0, 3),
    $(Slot(true, 2, $(SplashUp)), Slot(false, 3, $(DefenseUp, Camouflage)))) {
    override def id = "mortar-emplacement"
    override def builtBy = $(InfantrySection)
}
case object Bofors extends UnitType("Bofors Emplacement", British, 2, Emplacement, 3, 1, 3, $(ai, ai), Cost(3, 0, 0),
    $(Slot(true, 2, $(AddAttack(ai))), Slot(false, 3, $(SightUp)))) {
    override def builtBy = $(InfantrySection)
}
// COMPANY COMMAND POST 3
case object Cromwell extends UnitType("Cromwell", British, 3, HeavyVehicle, 4, 1, 2, $(ai, ap), Cost(2, 0, 4),
    $(Slot(true, 1, $(SpeedUp)), Slot(false, 3, $(AddAttack(ap)), 2))) {
    override def speed = 1
    override def fixed = $(AI)
}
case object Centaur extends UnitType("Centaur AA", British, 3, HeavyVehicle, 3, 1, 3, $(ai, ai), Cost(3, 0, 2),
    $(Slot(true, 3, $(AddAttacks(ai))), Slot(false, 4, $(SpotsCamo)))) {
    override def strafing = true
}
case object Churchill extends UnitType("Churchill", British, 3, HeavyVehicle, 6, 1, 3, $(ai, ap, ap), Cost(2, 0, 4),
    $(Slot(true, 2, $(DefenseUp, RemoveSlow)), Slot(false, 3, $(AddAttack(ai))))) {
    override def slow = 1
    // (Operation Crocodile's Flame is forward too)
    override def fixed = $(AI, Flame)
}

// SOVIET HEADQUARTERS 1
case object Conscripts extends UnitType("Conscripts", Soviet, 1, Infantry, 3, 1, 2, $(ai), Cost(4, 0, 0),
    $(Slot(true, 2, $(AddAttack(flame))), Slot(false, 3, $(HealthUp), 2))) {
    override def wrenches = 1
    override def speed = 1
}
case object SovietMortar extends UnitType("Mortar Team", Soviet, 1, Infantry, 3, 2, 5, $(heIndirect), Cost(4, 0, 0),
    $(Slot(true, 2, $(SplashUp)), Slot(false, 4, $(AddAttack(heIndirect))))) {
    override def id = "sov-mortar"
    override def weaponTeam = true
    override def noBuildingSetup = true
}
case object ScoutCar extends UnitType("M3A1 Scout Car", Soviet, 1, LightVehicle, 2, 1, 2, $(ai), Cost(2, 0, 1),
    $(Slot(true, 1, $(AddAttack(ai))), Slot(false, 3, $(DefenseUp)))) {
    override def id = "scout-car"
    override def transport = true
    override def capture = true
}
// SUPPORT KAMPANEYE 2
case object ZiS3 extends UnitType("76mm AT Gun", Soviet, 2, Infantry, 4, 1, 4, $(ap, ap), Cost(5, 0, 0),
    $(Slot(true, 1, $(DefenseUp, AddAttack(ap))), Slot(false, 3, $(AddAttack(ap))))) {
    override def id = "zis3"
    override def weaponTeam = true
    override def noBuildingSetup = true
}
// fires only at range 3
case object SniperSquad extends UnitType("Sniper Squad", Soviet, 2, Infantry, 3, 3, 3, $(ai, ai), Cost(3, 0, 0),
    $(Slot(true, 2, $(Camouflage)), Slot(false, 4, $(AddAttack(ai), AddAttack(ap))))) {
    override def id = "sniper"
    override def sight = 3
}
case object T70 extends UnitType("T-70 Light Tank", Soviet, 2, HeavyVehicle, 2, 1, 2, $(ai, ap), Cost(2, 0, 3),
    $(Slot(true, 2, $(SpotsCamo)), Slot(false, 3, $(SpeedUp)))) {
    override def id = "t70"
    override def speed = 1
    override def fixed = $(AI)
    override def captureMode = true
}
// ARMOR KAMPANEYE 3
case object T34 extends UnitType("T-34 Tank", Soviet, 3, HeavyVehicle, 3, 1, 2, $(ai, ap, ap), Cost(2, 0, 3),
    $(Slot(true, 2, $(HeroicSacrifice)), Slot(false, 2, $(AddAttack(ai))))) {
    override def id = "t34"
}
case object SU76 extends UnitType("SU-76 Assault Gun", Soviet, 3, HeavyVehicle, 2, 1, 3, $(he, he, he), Cost(1, 0, 4),
    $(Slot(true, 1, $(SpeedUp)), Slot(false, 3, $(HealthUp)))) {
    override def id = "su76"
    override def fixed = $(HE)
}
case object SU85 extends UnitType("SU-85 Tank Destroyer", Soviet, 3, HeavyVehicle, 4, 1, 3, $(ap, ap, ap), Cost(2, 0, 4),
    $(Slot(true, 2, $(AddAttack(ap), RemoveSlow)), Slot(false, 2, $(SightUp)))) {
    override def id = "su85"
    override def slow = 1
    override def fixed = $(AP)
}

// OBERKOMMANDO WEST HQ 1: the Sturmpioneers build Flak Guns, the SWS Supply Truck converts into the Tier 2 and 3 Buildings
case object Sturmpioneers extends UnitType("Sturmpioneers", OKW, 1, Infantry, 3, 1, 2, $(ai, ai), Cost(4, 0, 0),
    $(Slot(true, 2, $(AddAttack(ap))))) {
    override def close = 1
    override def wrenches = 2
}
case object Volksgrenadiers extends UnitType("Volksgrenadiers", OKW, 1, Infantry, 3, 1, 3, $(ai), Cost(4, 0, 0),
    $(Slot(true, 1, $(HealthUp), 2), Slot(false, 3, $(AddAttack(ap), AddAttack(ai))))) {
    override def defense = 1
}
case object SWSTruck extends UnitType("SWS Supply Truck", OKW, 1, LightVehicle, 2, 1, 0, $, Cost(2, 0, 1),
    $(Slot(true, 1, $(HealthUp)), Slot(false, 2, $(ReinforceUp)))) {
    override def id = "sws-truck"
    override def transport = true
    override def capture = true
}
// OKW BATTLEGROUP 2
case object Raketenwerfer extends UnitType("Raketenwerfer", OKW, 2, Infantry, 3, 1, 4, $(ap), Cost(3, 0, 0),
    $(Slot(true, 1, $(SpeedUp)), Slot(false, 3, $(AddAttack(ap)), 2))) {
    override def weaponTeam = true
}
// sets up and has a Firing Arc like a Weapon Team
case object StukaHalftrack extends UnitType("Stuka Halftrack", OKW, 2, LightVehicle, 3, 2, 5, $(heIndirect, heIndirect), Cost(1, 0, 4),
    $(Slot(true, 2, $(RangeUp)), Slot(false, 4, $(SplashUp)))) {
    override def id = "stuka-halftrack"
    override def weaponTeam = true
}
case object Luchs extends UnitType("Luchs Light Tank", OKW, 2, HeavyVehicle, 2, 1, 2, $(ai, ai), Cost(2, 0, 3),
    $(Slot(true, 1, $(HealthUp)), Slot(false, 3, $(SpeedUp, DefenseUp)))) {
    override def sight = 3
}
// SCHWERER PANZER HQ 3
case object Obersoldaten extends UnitType("Obersoldaten", OKW, 3, Infantry, 4, 1, 3, $(ai, flame), Cost(4, 0, 0),
    $(Slot(true, 2, $(AddAttack(ai))), Slot(false, 2, $(SpeedUp)))) {
    override def innate = $(Camouflage)
}
case object Panther extends UnitType("Panther", OKW, 3, HeavyVehicle, 5, 1, 3, $(ai, ap, ap, ap), Cost(3, 0, 5),
    $(Slot(true, 1, $(DefenseUp)), Slot(false, 3, $(SightUp)))) {
    override def fixed = $(AI)
}
case object Sturmtiger extends UnitType("Sturmtiger", OKW, 3, HeavyVehicle, 7, 1, 2, $(ai, ai, he, he), Cost(2, 0, 6),
    $(Slot(true, 3, $(RangeUp)), Slot(false, 4, $(AddAttack(he))))) {
    override def slow = 1
    override def splash = true
    override def fixed = $(AI, HE)
}
// the OKW Tier Buildings on the map, and the Flak Gun the Sturmpioneers build (4 Munitions, on the Sturmpioneers' board)
case object Battlegroup extends UnitType("OKW Battlegroup", OKW, 2, Emplacement, 3, 1, 0, $, Cost(1, 0, 2), $) {
    override def reinforce = true
    override def innate = $(SpotsCamo)
    override def tierBuilding = 2
    override def limit = 1
}
case object SchwererHQ extends UnitType("Schwerer Panzer HQ", OKW, 3, Emplacement, 3, 1, 4, $(ap), Cost(1, 0, 3), $) {
    override def id = "schwerer-hq"
    override def tierBuilding = 3
    override def limit = 1
}
case object Flak37 extends UnitType("Flak 37", OKW, 1, Emplacement, 2, 1, 3, $(ai, ai), Cost(0, 4, 0), $) {
    override def id = "flak"
    override def builtBy = $(Sturmpioneers)
}

// THE BRITISH, SOVIET AND OKW COMMANDER UNITS (Faction Guide pages 22, 28 and 35)
case object RoyalEngineers extends UnitType("Royal Engineers", British, 1, Infantry, 4, 1, 2, $(ai, ai), Cost(4, 0, 0), InfantrySection.upgrades) {
    override def id = "royal-engineers"
    override def commander = true
    override def close = 2
    override def wrenches = 2
}
case object ForwardEmplacement extends UnitType("Forward Emplacement", British, 1, Emplacement, 3, 1, 0, $, Cost(2, 0, 0), $) {
    override def id = "forward-emplacement"
    override def commander = true
    override def builtBy = $(InfantrySection, RoyalEngineers)
    override def reinforce = true
    override def retreatPoint = true
    override def innate = $(SpotsCamo, SpawnAdjacent)
}
case object SeventeenPounder extends UnitType("17 Pounder Emplacement", British, 1, Emplacement, 4, 1, 4, $(ap, ap, ap, ap), Cost(5, 0, 0), $) {
    override def id = "seventeen-pounder"
    override def commander = true
    override def builtBy = $(InfantrySection, RoyalEngineers)
    override def defense = 1
}
// its HE turreted, its AI and a third HE (HE Splash) forward
case object ChurchillAVRE extends UnitType("Churchill AVRE", British, 3, HeavyVehicle, 6, 1, 2, $(he, he, ai, ai, he), Cost(3, 0, 5), $) {
    override def id = "churchill-avre"
    override def commander = true
    override def chassis = Churchill
    override def slow = 1
    override def defense = 1
    override def splash = true
    override def fixed = $(AI)
}
case object Staghound extends UnitType("Staghound", British, 2, LightVehicle, 4, 1, 3, $(ap, ap), Cost(2, 0, 3), $) {
    override def commander = true
}
case object APSniper extends UnitType(".55 cal Armor Piercing Sniper", British, 1, Infantry, 3, 3, 3, $(ai, ai, ap), Cost(3, 2, 0), $) {
    override def id = "ap-sniper"
    override def commander = true
    override def sight = 3
    override def innate = $(Camouflage)
}
case object DShK extends UnitType("DShK 38 HMG", Soviet, 1, Infantry, 4, 1, 3, $(ai, ai), Cost(4, 1, 0), $) {
    override def id = "dshk"
    override def commander = true
    override def weaponTeam = true
    override def machineGun = true
    override def sight = 3
}
case object T3485 extends UnitType("T-34-85 Medium Tank", Soviet, 3, HeavyVehicle, 4, 1, 3, $(ai, ap, ap), Cost(3, 0, 4), $) {
    override def id = "t3485"
    override def commander = true
    override def chassis = T34
}
// built by Conscripts in an empty Building next to them
case object ForwardHQ extends UnitType("Forward HQ", Soviet, 1, Emplacement, 3, 1, 0, $, Cost(2, 0, 0), $) {
    override def id = "forward-hq"
    override def commander = true
    override def builtBy = $(Conscripts)
    override def inBuildings = true
    override def reinforce = true
    override def retreatPoint = true
    override def innate = $(SpawnAdjacent)
    override def limit = 1
}
case object Panzerfusilier extends UnitType("Panzerfusiliers", OKW, 1, Infantry, 4, 2, 3, $(ap, ap), Cost(4, 1, 0), $) {
    override def commander = true
    override def speed = 1
}
case object JaegerRecon extends UnitType("Jaeger Recon Squad", OKW, 1, Infantry, 4, 1, 2, $(ai), Cost(3, 0, 0), $) {
    override def id = "jaeger-recon"
    override def commander = true
    override def innate = $(Camouflage, SpotsCamo)
}
case object Puma extends UnitType("Puma", OKW, 2, LightVehicle, 3, 1, 3, $(ap, ap, ai), Cost(2, 0, 3), $) {
    override def commander = true
}
case object LeFH18 extends UnitType("LeFH 18 Artillery", OKW, 1, Emplacement, 3, 1, 5, $(heIndirect, heIndirect), Cost(0, 4, 0), $) {
    override def id = "lefh18"
    override def commander = true
    override def builtBy = $(Sturmpioneers)
}
case object OKWPak43 extends UnitType("Pak 43 Emplacement", OKW, 1, Emplacement, 3, 1, 4, $(ap, ap, ap), Cost(4, 0, 0), $) {
    override def id = "okw-pak43"
    override def commander = true
    override def builtBy = $(Sturmpioneers)
    override def innate = $(AddAttack(ap))
}
case object Kettenkrad extends UnitType("Kettenkrad", OKW, 1, LightVehicle, 1, 1, 0, $, Cost(2, 0, 1), $) {
    override def commander = true
    override def speed = 2
    override def capture = true
    override def innate = $(Camouflage, HeroicSacrifice)
}
case object StG44Volksgrenadiers extends UnitType("StG44 Rifle Volksgrenadiers", OKW, 1, Infantry, 3, 1, 3, $(ai, ai), Cost(4, 0, 0), Volksgrenadiers.upgrades) {
    override def id = "stg44-volks"
    override def commander = true
}

// COMMANDER UNITS (Full Game Rules, commanders.scala): called in once their Commander's tier is unlocked, read off the Faction Guide's
// core Commander cards. Their tier is the Tier Building they need.
case object Priest extends UnitType("M7 Priest", US, 3, HeavyVehicle, 2, 1, 4, $(heIndirect, heIndirect), Cost(1, 0, 3), $) {
    override def commander = true
    override def fixed = $(HE)
}
case object EasyEight extends UnitType("Sherman Easy Eight", US, 3, HeavyVehicle, 4, 1, 3, $(ai, ai, ai, ap, ap), Cost(3, 0, 4), $) {
    override def id = "easy-eight"
    override def commander = true
    override def chassis = Sherman
}
// Riflemen may spawn as Assault Engineers: Close Range Firepower in place of their AI, two Wrenches, the Riflemen's upgrades
case object AssaultEngineers extends UnitType("Assault Engineers", US, 1, Infantry, 4, 1, 2, $(ai, ai), Cost(4, 0, 0), Riflemen.upgrades) {
    override def id = "assault-engineers"
    override def commander = true
    override def close = 2
    override def wrenches = 2
}
case object Tiger extends UnitType("Tiger", Wehrmacht, 3, HeavyVehicle, 6, 1, 2, $(ap, ap, ap, ai), Cost(3, 0, 5), $) {
    override def commander = true
    override def defense = 1
}
// two squads spawn for each one bought
case object Osttruppen extends UnitType("Osttruppen", Wehrmacht, 1, Infantry, 3, 1, 2, $(ai), Cost(4, 1, 0), $) {
    override def commander = true
    override def wrenches = 1
    override def speed = 1
}
case object Stormtroopers extends UnitType("Stormtroopers", Wehrmacht, 1, Infantry, 4, 1, 2, $(ai, ai), Cost(5, 1, 0), $) {
    override def commander = true
    override def close = 1
}
// Emplacements, built by Infantry with Wrenches in the Supply Phase on an adjacent hex
case object Pak43 extends UnitType("Pak 43 Emplacement", Wehrmacht, 1, Emplacement, 3, 1, 4, $(ap, ap, ap), Cost(4, 0, 0), $) {
    override def commander = true
}
case object MGBunker extends UnitType("MG Bunker", Wehrmacht, 1, Emplacement, 2, 1, 2, $(ai), Cost(0, 2, 0), $) {
    override def id = "mg-bunker"
    override def commander = true
    override def machineGun = true
}

// THE US EXPANSION COMMANDERS' UNITS (2026-11-24; Faction Guide pages 11 and 12, the Elite Commanders Collection, Pathfinder, StuG and
// Lost Brigades packs, read off their call-in panels: health, range, AI and AP figures, curved arrows for turrets)
case object Stuart extends UnitType("M5 Stuart", US, 2, HeavyVehicle, 3, 1, 2, $(ai, ap), Cost(2, 0, 3), $) {
    override def commander = true
    override def speed = 2
}
case object Pershing extends UnitType("M26 Pershing", US, 3, HeavyVehicle, 6, 1, 2, $(ap, ap, ai, ai), Cost(3, 0, 5), $) {
    override def commander = true
}
case object Calliope extends UnitType("T34 Calliope", US, 3, HeavyVehicle, 4, 1, 4, $(heIndirect, heIndirect), Cost(2, 0, 5), $) {
    override def commander = true
    override def splash = true
}
// assaulting, it destroys the Sandbags of the hex it assaults at the start of the Damage Phase
case object Dozer extends UnitType("Sherman 105mm Dozer", US, 3, HeavyVehicle, 6, 1, 2, $(ai, ai, he, he), Cost(3, 0, 5), $) {
    override def commander = true
    override def defense = 1
    override def chassis = Sherman
}
case object Jackson extends UnitType("M36 Jackson", US, 3, HeavyVehicle, 3, 1, 4, $(ap, ap, ap), Cost(3, 0, 5), $) {
    override def commander = true
    override def speed = 1
    override def chassis = Wolverine
}
case object Pathfinder extends UnitType("I.R. Pathfinder Squad", US, 1, Infantry, 3, 1, 3, $(ai), Cost(3, 0, 0), $) {
    override def commander = true
    override def innate = $(SpotsCamo, Camouflage)
}
// after a move, it may Assault an adjacent hex for free (`cavalryCharge`)
case object CavalryRiflemen extends UnitType("Cavalry Riflemen", US, 1, Infantry, 5, 1, 2, $(ai), Cost(4, 1, 0), $) {
    override def id = "cavalry-riflemen"
    override def commander = true
    override def defense = 1
    override def innate = $(CloseFire)
}
// Riflemen with +1 Health that may enter Water; a Destroyed Building gives them 3 Defense Dice
case object DevilsBrigade extends UnitType("Devil's Brigade", US, 1, Infantry, 5, 1, 2, $(ai), Cost(4, 0, 0), Riflemen.upgrades) {
    override def id = "devils-brigade"
    override def commander = true
    override def wrenches = 1
}
// THE WEHRMACHT EXPANSION COMMANDERS' UNITS (2026-11-24; Faction Guide pages 17 and 18: StuG, Elite Commanders Collection, Pathfinder
// and Lost Brigades packs); straight arrows on a Heavy Vehicle's panel are its fixed gun
case object StuG extends UnitType("StuG III G", Wehrmacht, 2, HeavyVehicle, 3, 1, 3, $(ai, ap), Cost(2, 0, 3), $) {
    override def id = "stug"
    override def commander = true
    override def fixed = $(AI, AP)
}
case object KingTiger extends UnitType("King Tiger", Wehrmacht, 3, HeavyVehicle, 8, 1, 3, $(ai, ai, ap, ap), Cost(4, 0, 5), $) {
    override def id = "king-tiger"
    override def commander = true
    override def slow = 1
    override def defense = 1
    override def fixed = $(AI)
}
case object JaegerSniper extends UnitType("Jaeger Sniper", Wehrmacht, 1, Infantry, 3, 3, 3, $(ai, ai), Cost(3, 2, 0), $) {
    override def id = "jaeger-sniper"
    override def commander = true
    override def sight = 3
    override def speed = 1
    override def innate = $(SpotsCamo, Camouflage)
}
case object Jagdpanther extends UnitType("Jagdpanther", Wehrmacht, 3, HeavyVehicle, 5, 1, 4, $(ap, ap, ap), Cost(3, 0, 4), $) {
    override def commander = true
    override def defense = 1
    override def fixed = $(AP)
}
case object Elefant extends UnitType("Elefant", Wehrmacht, 3, HeavyVehicle, 7, 1, 4, $(ap, ap, ap), Cost(3, 0, 5), $) {
    override def commander = true
    override def slow = 1
    override def fixed = $(AP)
}
// Infantry may spawn next to it (Slow the first round); a Transport that Reinforces and is a Retreat Point
case object Halftrack extends UnitType("SdKfz 251 Half-track", Wehrmacht, 2, LightVehicle, 3, 1, 3, $(ai), Cost(2, 0, 3), $) {
    override def id = "sdkfz251"
    override def commander = true
    override def transport = true
    override def reinforce = true
    override def retreatPoint = true
    override def innate = $(SpawnAdjacent)
}
// THE BRITISH EXPANSION COMMANDERS' UNITS (2026-11-24; Faction Guide pages 23 and 24)
case object Sexton extends UnitType("Sexton Gun Carriage", British, 2, HeavyVehicle, 2, 1, 3, $(heIndirect, heIndirect), Cost(1, 0, 4), $) {
    override def commander = true
    override def splash = true
    override def fixed = $(HE)
}
case object Comet extends UnitType("Comet", British, 3, HeavyVehicle, 6, 1, 2, $(ap, ap, ai), Cost(3, 0, 5), $) {
    override def commander = true
    override def speed = 1
}
// Tulip: it may fire 3 HE instead for 2 Munitions (`targetChoices`)
case object Firefly extends UnitType("Sherman Firefly", British, 3, HeavyVehicle, 4, 1, 4, $(ap, ap, ap), Cost(2, 0, 5), $) {
    override def commander = true
}
// a Defense Die when Assaulting
case object Valentine extends UnitType("Valentine Mk. XI", British, 2, HeavyVehicle, 3, 1, 3, $(ai, ai, ap), Cost(2, 0, 3), $) {
    override def commander = true
    override def fixed = $(AI)
}
// spawned on any visible non-Building hex, Double Slow, one a round
case object Commandos extends UnitType("Commandos", British, 1, Infantry, 3, 1, 2, $(ai), Cost(5, 1, 0), $) {
    override def commander = true
    override def innate = $(CloseFire)
    override def purchasable = false
}
// THE SOVIET EXPANSION COMMANDERS' UNITS (2026-11-24; Faction Guide pages 29 and 30: Elite Commanders Collection, Eastern
// Reinforcements, StuG and Lost Brigades packs); two gray range circles and a red one: range 3 only, as the Sniper Squad's
case object ShockTroops extends UnitType("Shock Troops", Soviet, 1, Infantry, 5, 1, 2, $(ai), Cost(4, 1, 0), $) {
    override def id = "shock-troops"
    override def commander = true
    override def defense = 1
    override def innate = $(CloseFire)
}
case object IS2 extends UnitType("IS-2 Heavy Tank", Soviet, 3, HeavyVehicle, 7, 1, 2, $(ai, ap, ap), Cost(3, 0, 5), $) {
    override def id = "is2"
    override def commander = true
}
case object GuardsRifle extends UnitType("Guards Rifle Infantry", Soviet, 1, Infantry, 4, 3, 3, $(ai, ai), Cost(4, 1, 0), $) {
    override def id = "guards-rifle"
    override def commander = true
    override def alt = $(ap, ap)
}
// Partisans: Heroic Sacrifice, spawned next to an empty Building with Slow, one a round
case object AIPartisans extends UnitType("Anti-Infantry Partisans", Soviet, 1, Infantry, 2, 1, 2, $(ai), Cost(1, 4, 0), $) {
    override def id = "ai-partisans"
    override def commander = true
    override def purchasable = false
    override def innate = $(HeroicSacrifice)
}
case object ATPartisans extends UnitType("Anti-Tank Partisans", Soviet, 1, Infantry, 2, 1, 2, $(ap), Cost(1, 4, 0), $) {
    override def id = "at-partisans"
    override def commander = true
    override def purchasable = false
    override def innate = $(HeroicSacrifice)
}
// built by Conscripts in an empty Building next to them; Vehicles may spawn next to it (Slow the first round); one at a time
case object ForwardStation extends UnitType("Forward Station", Soviet, 1, Emplacement, 3, 1, 0, $, Cost(2, 0, 0), $) {
    override def id = "forward-station"
    override def commander = true
    override def builtBy = $(Conscripts)
    override def inBuildings = true
    override def wrenches = 1
    override def limit = 1
}
case object KV2 extends UnitType("KV-2 Assault Gun", Soviet, 3, HeavyVehicle, 5, 1, 3, $(heIndirect, heIndirect), Cost(2, 0, 5), $) {
    override def id = "kv2"
    override def commander = true
    override def splash = true
}
// it cannot be Pinned
case object PenalBattalion extends UnitType("Penal Battalion", Soviet, 1, Infantry, 5, 1, 2, $(ai, ai), Cost(4, 1, 0), $) {
    override def id = "penal"
    override def commander = true
    override def defense = 1
    override def alt = $(he, he)
    override def altRange = 1
}
case object ISU152 extends UnitType("ISU-152", Soviet, 3, HeavyVehicle, 7, 1, 4, $(ap, ap, ap), Cost(3, 0, 5), $) {
    override def id = "isu152"
    override def commander = true
    override def slow = 1
    override def fixed = $(AP, HE)
    override def alt = $(he, he, he)
}
// 1 more Flame when Assaulting or firing at a unit in a Building
case object KV8 extends UnitType("KV-8 Flame Tank", Soviet, 3, HeavyVehicle, 5, 1, 3, $(ai, ai, ai, flame), Cost(3, 0, 2), $) {
    override def id = "kv8"
    override def commander = true
    override def slow = 1
    override def fixed = $(AI)
}
// THE OKW EXPANSION COMMANDERS' UNIT (2026-11-24; Faction Guide pages 36 and 37); Assaulting, it may deal its damage to a second target
case object Hetzer extends UnitType("Flammpanzer 38 Hetzer", OKW, 3, HeavyVehicle, 3, 1, 3, $(flame, ai, ai, ai), Cost(3, 0, 2), $) {
    override def commander = true
    override def defense = 1
    override def fixed = $(AI)
}
case object USMGBunker extends UnitType("MG Bunker", US, 1, Emplacement, 2, 1, 2, $(ai), Cost(0, 2, 0), $) {
    override def id = "mg-bunker"
    override def commander = true
    override def machineGun = true
    override def builtBy = $(Riflemen, AssaultEngineers, DevilsBrigade, CavalryRiflemen)
}

// THE LOST BRIGADES' OWN UNITS (2026-11-24; brigades.scala): the Elite Grenadier Squad Leaders, and the universal Command Panther of
// each faction (turreted AP and AI, Spots Camo, one at a time)
case object EliteGrenadiers extends UnitType("Elite Grenadier Squad Leaders", Wehrmacht, 1, Infantry, 4, 1, 3, $(ai, ai, he), Cost(4, 3, 0), $) {
    override def id = "elite-grenadiers"
    override def commander = true
    override def wrenches = 1
    override def innate = $(Camouflage)
}
sealed abstract class CommandPanther(f : Faction, n : String) extends UnitType(n, f, 3, HeavyVehicle, 5, 1, 3, $(ap, ap, ai, ai), Cost(3, 0, 5), $) {
    override def id = "command-panther"
    override def commander = true
    override def limit = 1
    override def innate = $(SpotsCamo)
}
case object CommandPantherUS extends CommandPanther(US, "Captured Command Panther")
case object CommandPantherWM extends CommandPanther(Wehrmacht, "Command Panther")
case object CommandPantherBR extends CommandPanther(British, "Captured Command Panther")
case object CommandPantherSU extends CommandPanther(Soviet, "Captured Command Panther")
case object CommandPantherOKW extends CommandPanther(OKW, "Command Panther")

// the cost of unlocking a Tier Building (on the back of its board), the same for every faction
object Tiers {
    def cost(tier : Int) = tier @@ {
        case 2 => Cost(1, 0, 2)
        case 3 => Cost(1, 0, 3)
        case _ => Cost(0, 0, 0)
    }

    def name(f : Faction, tier : Int) = (f.base, tier) @@ {
        case (US, 1) => "US Headquarters"
        case (US, 2) => "Company Command Post"
        case (US, 3) => "Battalion Command Post"
        case (Wehrmacht, 1) => "Wehrmacht Headquarters"
        case (Wehrmacht, 2) => "Panzergrenadier Kompanie"
        case (Wehrmacht, 3) => "Panzer Korps"
        case (British, 1) => "British Headquarters"
        case (British, 2) => "Platoon Command Post"
        case (British, 3) => "Company Command Post"
        case (Soviet, 1) => "Soviet Headquarters"
        case (Soviet, 2) => "Support Kampaneye"
        case (Soviet, 3) => "Armor Kampaneye"
        case (OKW, 1) => "Oberkommando West HQ"
        case (OKW, 2) => "OKW Battlegroup"
        case (OKW, 3) => "Schwerer Panzer HQ"
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

// the rules: the First Game Rules, or the Full Game Rules (the grey boxes of the rulebook); games made before the choice have neither,
// and play the First Game Rules
sealed abstract class RulesChoice(val name : String, val text : String) extends GameOption with OneOfGroup with ImportantOption {
    val group = "Rules".txt
    def valueOn = name.hl
    override val explain = $[Elem](text.txt)
}
// every new game (2026-11-25): a teammate's Retreat Point (a Major, a Transport that is one) is one for the whole team
case object TeamRetreatPoints extends GameOption with ToggleOption {
    val group = "Team games".txt
    def valueOn = "Teammates' Retreat Points".txt
}

case object FullGameRules extends RulesChoice("Full Game Rules", "The whole game: the Operations deck sets the number of turns, Retreats, Assaults, machine gun Overwatch, firing arcs and Heavy Vehicle facing, HE Splash, Frontal Armor, destructible Buildings and Moro River's tank traps.")
case object FirstGameRules extends RulesChoice("First Game Rules", "The rulebook's rules for a first game: four turns a round, no Retreats, Assaults, Overwatch, arcs or facing.")

object RulesChoice {
    val all = $[RulesChoice](FullGameRules, FirstGameRules)
}

// the map: the Mission Booklet's two-panel core missions (missions.scala); games made before the choice play Moro River as first read (MoroRiverFirst)
// the setup screen shows them in one Map folder (folded, it names the map picked), in sections by the booklet's chapters, each map a
// tile with its picture (tools/map-thumbs.py), its Spawn Areas and Control Points marked, and how many play (Meta.optionTile); one
// choice across the sections (oneOf)
sealed abstract class MapChoice(val map : MapData, val text : String) extends GameOption with OneOfGroup with ImportantOption {
    lazy val section : String = MapChoice.sectionOf(map)
    lazy val group = ("Map: " + section).txt
    override def oneOf : Any = "Map"
    override def folder = |("Map")
    def valueOn = map.name.hl
    override val explain = $[Elem](text.txt)
    // who plays it: the free-for-alls their number, the skirmishes two, the rest two to five (teams)
    def playersText = (map.areas.num > 2).?(map.areas.num + " players, each on their own").|((map.panels == 1).?("2 players").|((map.panels * map.rows > 2).?("2 to 6 players").|("2 to 5 players")))
    def panelsText = (map.rows * map.panels > 1).?((map.rows * map.panels) + " panels" + (map.rows > 1).??(", square")).|("1 panel")
}
case object MoroRiverMap extends MapChoice(MoroRiver, "MR1 and MR2: a river with fords, tank traps and Buildings. Spawn Areas at the top and bottom. Starting stockpile 4 Manpower, 2 Munitions, 3 Fuel.")
case object AcrossTheDonMap extends MapChoice(AcrossTheDon, "MR1 and MR2, played across: the Spawn Areas are the left and right edges, the Control Points elsewhere. Starting stockpile 5 Manpower, 3 Munitions, 3 Fuel.")
case object BetweenTwoRiversMap extends MapChoice(BetweenTwoRivers, "MR2 and MR1 the other way round: the two rivers at the edges. Spawn Areas at the top and bottom. Starting stockpile 4 Manpower, 4 Munitions, 4 Fuel.")
case object ArdennesForestMap extends MapChoice(ArdennesForest, "AF1 and AF2, the backs of the Moro River panels: a snowy forest, the Cathedral (a multi-hex Special Building), multi-hex Buildings and Ice tiles on the frozen lake, split at both edges. Spawn Areas at the top and bottom. Starting stockpile 6 Manpower, 2 Munitions, 3 Fuel.")
case object FrozenLakeMap extends MapChoice(FrozenLake, "AF2 and AF1: the frozen lake's Ice tiles in the middle, the Cathedral on the right. Spawn Areas at the top and bottom. Starting stockpile 6 Manpower, 4 Munitions, 3 Fuel.")
case object RoadToMoscowMap extends MapChoice(RoadToMoscow, "AF1 and AF2, played across: the Spawn Areas are the left and right edges. Starting stockpile 6 Manpower, 4 Munitions, 4 Fuel.")
case object TheRiverMap extends MapChoice(TheRiver, "Skirmish, two players: MR1 alone, the river and its bridge at the top. Each player starts with 2 Infantry; the game ends at 11 VP. Starting stockpile 4 Manpower, 2 Munitions, 2 Fuel.")
case object FrozenPondMap extends MapChoice(FrozenPond, "Skirmish, two players: AF2 alone, the frozen pond's Ice tiles on the right. Each player starts with 2 Infantry; the game ends at 11 VP. Starting stockpile 4 Manpower, 2 Munitions, 4 Fuel.")
case object BetweenTwoRiversWideMap extends MapChoice(BetweenTwoRiversWide, "Three panels (MR2, MR1, MR2), for team games: the Spawn Areas are the left and right edges; highly asymmetrical. Each player starts with 2 Infantry. Starting stockpile 4 Manpower, 2 Munitions, 6 Fuel.")
case object ArdennesForestWideMap extends MapChoice(ArdennesForestWide, "Three panels (AF2, AF1, AF2), for team games: the airfield in the middle, Spawn Areas at the top and bottom; highly asymmetrical. Each player starts with 2 Infantry. Starting stockpile 1 Manpower, 2 Munitions, 3 Fuel.")
case object HillThreeThirtyOneMap extends MapChoice(HillThreeThirtyOne, "Large Map Pack, HL1, HL2: the hill and its Bunker in the middle; the Spawn Areas are the left and right edges. Starting stockpile 6 Manpower, 4 Munitions, 2 Fuel.")
case object PloiestiOutskirtsMap extends MapChoice(PloiestiOutskirts, "Large Map Pack, HL2, HL1: two multi-hex Buildings (not printed on the panels) either side of the middle. Starting stockpile 4 Manpower, 4 Munitions, 4 Fuel.")
case object VerrieresRidgeMap extends MapChoice(VerrieresRidge, "Large Map Pack, HL1, HL2: the ridge and its Bunker. Starting stockpile 4 Manpower, 2 Munitions, 4 Fuel.")
case object TroisPontsMap extends MapChoice(TroisPonts, "Large Map Pack, TP1, TP2: the river town and its bridges. Starting stockpile 4 Manpower, 2 Munitions, 3 Fuel.")
case object CaenMap extends MapChoice(Caen, "Large Map Pack, TP2, TP1: the river in the middle; highly asymmetrical. Starting stockpile 2 Manpower, 6 Munitions, 0 Fuel.")
case object CarentanMap extends MapChoice(Carentan, "Large Map Pack, TP1, TP2: the Spawn Areas are the left and right edges; highly asymmetrical. Starting stockpile 6 Manpower, 4 Munitions, 4 Fuel.")
case object StalingradMap extends MapChoice(Stalingrad, "Large Map Pack, SG1, SG2: the frozen Volga (Ice) and the railway bridge. Starting stockpile 2 Manpower, 4 Munitions, 0 Fuel.")
case object BulgeMap extends MapChoice(Bulge, "Large Map Pack, SG2, SG1: the frozen river through the middle. Starting stockpile 4 Manpower, 1 Munitions, 6 Fuel.")
case object LeningradMap extends MapChoice(Leningrad, "Large Map Pack, SG1, SG2: the Spawn Areas are the left and right edges. Starting stockpile 4 Manpower, 6 Munitions, 1 Fuel.")
case object MongolianTrainStationMap extends MapChoice(MongolianTrainStation, "Large Map Pack, SG2, SG1: Team 2 spawns at both bottom corners; highly asymmetrical. Starting stockpile 3 Manpower, 2 Munitions, 2 Fuel.")
case object MoroRiverFFAMap extends MapChoice(MoroRiverFFA, "Three players, each on their own: MR1 and MR2 with other Control Points and three Spawn Areas (top left, top right, bottom). Every player who assigned Damage to a destroyed unit gains a VP; the game ends at 15 VP. Starting stockpile 6 Manpower, 4 Munitions, 2 Fuel.")
case object CathedralMissionMap extends MapChoice(CathedralMission, "Large Map Pack, MO1, MO2: The Monastery's two churches, a Bunker between them. Starting stockpile 4 Manpower, 4 Munitions, 2 Fuel.")
case object CologneMissionMap extends MapChoice(CologneMission, "Large Map Pack, MO1, TP2r: The town and the river on the left, the Monastery turned round; Spawn Areas on the left half. Starting stockpile 4 Manpower, 4 Munitions, 6 Fuel.")
case object DesertOasisMap extends MapChoice(DesertOasis, "Large Map Pack, HL1, TP1: The hill, its Bunker, and the river town. Starting stockpile 6 Manpower, 2 Munitions, 2 Fuel.")
case object NoMansLandMap extends MapChoice(NoMansLand, "Large Map Pack, HL2, MO2r: The hills and the Monastery turned round. Starting stockpile 2 Manpower, 5 Munitions, 0 Fuel.")
case object MoscowOutskirtsMap extends MapChoice(MoscowOutskirts, "Large Map Pack, SG1r, AF1: Stalingrad turned round and the snowy forest; Spawn Areas at opposite corners. Starting stockpile 5 Manpower, 4 Munitions, 4 Fuel.")
case object OpposingFaithsMap extends MapChoice(OpposingFaiths, "Large Map Pack, MO2, MO1: The Monastery; a multi-hex Cathedral on the left. Starting stockpile 7 Manpower, 4 Munitions, 1 Fuel.")
case object SemoisMap extends MapChoice(Semois, "Large Map Pack, MR2, MO1: Moro River and the Monastery; Team 2 spawns in the bottom left corner, among tank traps; highly asymmetrical. Starting stockpile 5 Manpower, 3 Munitions, 4 Fuel.")
case object HinderdamMap extends MapChoice(Hinderdam, "Large Map Pack, MO2, MR1: The Monastery and the Moro River with its fords. Starting stockpile 4 Manpower, 4 Munitions, 3 Fuel.")
case object AncientFieldsMap extends MapChoice(AncientFields, "Large Map Pack, MO2, HL1, HL2: The Monastery and Hill 331, a Bunker and two Buildings added. Starting stockpile 2 Manpower, 6 Munitions, 2 Fuel.")
case object AirfieldMissionMap extends MapChoice(AirfieldMission, "Large Map Pack, TP1, TP2, MO2: Trois Ponts and the Monastery with its airstrip. Starting stockpile 4 Manpower, 3 Munitions, 3 Fuel.")
case object HilltopMonasteryMap extends MapChoice(HilltopMonastery, "Large Map Pack, HL2, MO1, MO2: Hill 331 and both Monastery panels. Starting stockpile 6 Manpower, 6 Munitions, 4 Fuel.")
case object BudapestMap extends MapChoice(Budapest, "Large Map Pack, MR1, MR2, MO1: Moro River and the Monastery. Starting stockpile 4 Manpower, 2 Munitions, 3 Fuel.")
case object SiegeOfLeningradMap extends MapChoice(SiegeOfLeningrad, "Large Map Pack, SG2r, SG1r, AF1: Stalingrad turned round and the snowy forest; Team 2 spawns at both bottom corners; highly asymmetrical. Starting stockpile 6 Manpower, 4 Munitions, 0 Fuel.")
case object OpposingFaithsThreeMap extends MapChoice(OpposingFaithsThree, "Large Map Pack, MO1, MO2, MO1: The Monastery and another MO1, a Bunker in the middle. Starting stockpile 6 Manpower, 4 Munitions, 2 Fuel.")
case object TheWideFrontMap extends MapChoice(TheWideFront, "Large Map Pack, HL2, HL1, HL2: Three Hill 331 panels. Starting stockpile 4 Manpower, 6 Munitions, 4 Fuel.")
case object TroisPontsThreeMap extends MapChoice(TroisPontsThree, "Large Map Pack, TP1, TP2, TP1: Trois Ponts and another TP1; each team spawns at both ends. Starting stockpile 6 Manpower, 6 Munitions, 2 Fuel.")
case object TwoIslandsMap extends MapChoice(TwoIslands, "Large Map Pack, SG1, SG2, SG1: Stalingrad and another SG1: the frozen Volga twice. Starting stockpile 3 Manpower, 2 Munitions, 2 Fuel.")
case object CologneFourMap extends MapChoice(CologneFour, "Large Map Pack, TP1, TP2, MO1r, MO2r: Trois Ponts and both Monastery panels turned round. Starting stockpile 4 Manpower, 4 Munitions, 6 Fuel.")
case object TroisPontsMonasteryMap extends MapChoice(TroisPontsMonastery, "Large Map Pack, TP1, TP2, MO1r, MO2r: Trois Ponts and both Monastery panels turned round. Starting stockpile 6 Manpower, 6 Munitions, 4 Fuel.")
case object HillMonasteryMap extends MapChoice(HillMonastery, "Large Map Pack, HL1, HL2, MO1, MO2: Hill 331 and the Monastery. Starting stockpile 3 Manpower, 2 Munitions, 1 Fuel.")
case object VengeanceMap extends MapChoice(Vengeance, "Large Map Pack, MO2r, MO1r, MO1, MO2: Four Monastery panels; Team 1 spawns at the top and bottom, Team 2 at the left and right edges. Starting stockpile 6 Manpower, 4 Munitions, 4 Fuel.")
case object OpposingFaithsFourMap extends MapChoice(OpposingFaithsFour, "Large Map Pack, MO1, MO2, MO2r, MO1r: Four Monastery panels. Starting stockpile 4 Manpower, 2 Munitions, 2 Fuel.")
case object TwoHilltopsMap extends MapChoice(TwoHilltops, "Large Map Pack, HL1, HL2, HL2r, HL1r: Four Hill 331 panels; each team spawns along its edge and on one side. Starting stockpile 4 Manpower, 4 Munitions, 6 Fuel.")
case object FrozenForkMap extends MapChoice(FrozenFork, "Large Map Pack, SG2, SG1, SG2, SG1: Four Stalingrad panels. Starting stockpile 3 Manpower, 3 Munitions, 3 Fuel.")
case object SixBridgesMap extends MapChoice(SixBridges, "Large Map Pack, TP2r, TP1r, TP1, TP2: Four Trois Ponts panels, the river crossed six times. Starting stockpile 5 Manpower, 4 Munitions, 3 Fuel.")
case object HillThreeThirtyOneFFAMap extends MapChoice(HillThreeThirtyOneFFA, "Large Map Pack, HL1, HL2: Three players, each on their own; the Bunker is a Destroyed Building, three Buildings below it. Starting stockpile 6 Manpower, 2 Munitions, 4 Fuel.")
case object CologneFFAMap extends MapChoice(CologneFFA, "Large Map Pack, TP1, TP2, MO1r: Four players, each on their own, one in each corner. Starting stockpile 6 Manpower, 4 Munitions, 4 Fuel.")
case object LorraineMap extends MapChoice(Lorraine, "Large Map Pack, MR1, MR2 / HL1, HL2: Moro River above Hill 331; the Spawn Areas are the left and right edges. Starting stockpile 5 Manpower, 4 Munitions, 3 Fuel.")
case object RailsAndMetalMap extends MapChoice(RailsAndMetal, "Large Map Pack, AF2, AF1 / SG1r, SG2r: The snowy forest above Stalingrad turned round; the Spawn Areas are the left and right edges. Starting stockpile 4 Manpower, 4 Munitions, 4 Fuel.")
case object StHilaireMap extends MapChoice(StHilaire, "Large Map Pack, TP1r, TP2r / MO1, MO2: Trois Ponts turned round above the Monastery; Team 1 spawns along the upper left edge, Team 2 the lower right. Starting stockpile 4 Manpower, 4 Munitions, 4 Fuel.")
case object WinterStormMap extends MapChoice(WinterStorm, "Large Map Pack, SG1, SG2 / AF1, AF2: Stalingrad above the snowy forest; the Spawn Areas are the left and right edges. Starting stockpile 6 Manpower, 4 Munitions, 2 Fuel.")
case object TheVolgaMap extends MapChoice(TheVolga, "Large Map Pack, TP1, TP2 / MR1, MR2: Trois Ponts above Moro River, the lower row shifted right; each team spawns along both rows' edges. Starting stockpile 6 Manpower, 4 Munitions, 2 Fuel.")
case object RaceToRomeMap extends MapChoice(RaceToRome, "Large Map Pack, MR1, MR2 / MO1, MO2: Moro River above the Monastery; the Spawn Areas are the upper left and right edges. Starting stockpile 6 Manpower, 4 Munitions, 2 Fuel.")
case object StMarieDuMontMap extends MapChoice(StMarieDuMont, "Large Map Pack, MO2r, TP1 / TP2, MO1r: The Monastery and Trois Ponts crosswise; the Spawn Areas are the upper left and right edges. Starting stockpile 6 Manpower, 4 Munitions, 3 Fuel.")
case object CologneFiveFFAMap extends MapChoice(CologneFiveFFA, "Large Map Pack, TP1r, TP2r / MO1, MO2: Five players, each on their own: Trois Ponts turned round above the Monastery. Starting stockpile 5 Manpower, 4 Munitions, 3 Fuel.")
case object HillSixSixTwoFFAMap extends MapChoice(HillSixSixTwoFFA, "Large Map Pack, HL1, HL2 / HL1, HL2: Five players, each on their own: two Hill 331s, one above the other. Starting stockpile 3 Manpower, 4 Munitions, 5 Fuel.")
case object ARiverTooFarMap extends MapChoice(ARiverTooFar, "Large Map Pack, MO2, MR1, MR2: The Monastery and Moro River; Team 1 spawns at both top corners. Starting stockpile 1 Manpower, 2 Munitions, 5 Fuel.")
case object RaceAcrossTheRhineMap extends MapChoice(RaceAcrossTheRhine, "Large Map Pack, HL2, MR1, MR2: Hill 331 and Moro River; the Spawn Areas are the left and right edges; every player starts with 2 Infantry. Starting stockpile 7 Manpower, 1 Munitions, 0 Fuel.")
case object FrozenRiverMap extends MapChoice(FrozenRiver, "Large Map Pack, SG1, AF2, AF1: Stalingrad and the snowy forest, its frozen river. Starting stockpile 4 Manpower, 4 Munitions, 4 Fuel.")
case object FrozenRiverFourMap extends MapChoice(FrozenRiverFour, "Large Map Pack, SG2, SG1, AF2, AF1: Stalingrad and the snowy forest; 7 Victory Points on Ice. Starting stockpile 4 Manpower, 4 Munitions, 4 Fuel.")
case object MudMap extends MapChoice(Mud, "Large Map Pack, HL2, MR1, MR2, HL1: Moro River between two Hill 331 panels; each team spawns at both ends. Starting stockpile 6 Manpower, 2 Munitions, 4 Fuel.")
case object WinterSolsticeMap extends MapChoice(WinterSolstice, "Large Map Pack, SG1, SG2, AF1, AF2: Stalingrad and the snowy forest. Starting stockpile 3 Manpower, 5 Munitions, 1 Fuel.")
case object SummerSolsticeMap extends MapChoice(SummerSolstice, "Large Map Pack, MO2, MR1, MR2, MO1: Moro River between two Monastery panels. Starting stockpile 2 Manpower, 3 Munitions, 4 Fuel.")

// the booklet's optional Modular Bunkers (and the Hilltop Monastery's optional Victory Point), on the missions that mark them
case object OptionalMapFeatures extends GameOption with ToggleOption {
    val group = "Map: options".txt
    override def folder = |("Map")
    def valueOn = "Optional Modular Bunkers".hl
    override val explain = $[Elem]("The Mission Booklet's optional Modular Bunkers (its A and B hexes) become Bunkers on the missions that mark them, and the Hilltop Monastery's optional Victory Point is in its Cathedral tower.".txt)
}

object MapChoice {
    val core = $[MapData](MoroRiver, AcrossTheDon, BetweenTwoRivers, ArdennesForest, FrozenLake, RoadToMoscow, BetweenTwoRiversWide, ArdennesForestWide)

    // the booklet's chapters, in its order
    val sections = $("Core missions", "Skirmishes", "Core panels, 3 panels", "Large Map Pack, 2 panels", "Large Map Pack, 3 panels",
        "Large Map Pack, 4 panels", "Large Map Pack, square", "Free-for-all")

    def sectionOf(m : MapData) : String =
        if (m.areas.num > 2) "Free-for-all"
        else if (m.panels == 1) "Skirmishes"
        else if (core.has(m)) (m.panels == 3).?("Core panels, 3 panels").|("Core missions")
        else if (m.rows > 1) "Large Map Pack, square"
        else "Large Map Pack, " + m.panels + " panels"

    // the choices in the setup's order, section by section
    lazy val ordered = sections./~(n => all.%(_.section == n))

    val all = $[MapChoice](MoroRiverMap, AcrossTheDonMap, BetweenTwoRiversMap, ArdennesForestMap, FrozenLakeMap, RoadToMoscowMap, MoroRiverFFAMap, TheRiverMap, FrozenPondMap, BetweenTwoRiversWideMap, ArdennesForestWideMap,
        HillThreeThirtyOneMap, PloiestiOutskirtsMap, VerrieresRidgeMap, TroisPontsMap, CaenMap, CarentanMap, StalingradMap, BulgeMap, LeningradMap, MongolianTrainStationMap,
        CathedralMissionMap, CologneMissionMap, DesertOasisMap, NoMansLandMap, MoscowOutskirtsMap, OpposingFaithsMap, SemoisMap, HinderdamMap, AncientFieldsMap, AirfieldMissionMap, HilltopMonasteryMap, BudapestMap, SiegeOfLeningradMap, OpposingFaithsThreeMap, TheWideFrontMap, TroisPontsThreeMap, TwoIslandsMap, CologneFourMap, TroisPontsMonasteryMap, HillMonasteryMap, VengeanceMap, OpposingFaithsFourMap, TwoHilltopsMap, FrozenForkMap, SixBridgesMap, HillThreeThirtyOneFFAMap, CologneFFAMap,
        LorraineMap, RailsAndMetalMap, StHilaireMap, WinterStormMap, TheVolgaMap, RaceToRomeMap, StMarieDuMontMap, CologneFiveFFAMap, HillSixSixTwoFFAMap, ARiverTooFarMap, RaceAcrossTheRhineMap, FrozenRiverMap, FrozenRiverFourMap, MudMap, WinterSolsticeMap, SummerSolsticeMap)
}

// THE ICE MATRIX (Full Game Rules 10.4): how many dice a unit attacked on an Ice tile rolls for each damage type assigned to it
object IceMatrix {
    def dice(k : Kind, d : Dmg) : Int = (k, d) match {
        case (_, AI) => 0
        case (_, AP) => 1
        case (HeavyVehicle, HE) => 2
        case (_, HE) => 1
        case (Infantry, Flame) => 0
        case (Emplacement, Flame) => 1
        case (_, Flame) => 2
    }
}


// THE OPERATIONS DECK (Full Game Rules): 14 cards, each with a top Operation of 2 to 4 turns and a bottom one of 3 to 5, from the
// mod's deck: its seven designs, card `n` showing design `Operations.art(n)`

case class OpCard(n : Int) extends Record with Elementary {
    def top = Operations.designs(Operations.art(n))._1
    def bottom = Operations.designs(Operations.art(n))._2
    def turns(upper : Boolean) = upper.?(top).|(bottom)
    def image = "ops:ops-" + Operations.art(n)
    def elem = (top + "/" + bottom).hl
}

object Operations {
    val designs = $((2, 3), (2, 4), (2, 5), (3, 4), (4, 3), (3, 5), (4, 5))
    val art = $(0, 1, 1, 2, 3, 3, 3, 4, 4, 4, 5, 5, 6, 6)
    val deck = 0.until(art.num).$./(OpCard)
    def side(upper : Boolean) = upper.?("top (2-4)").|("bottom (3-5)")
}

// a splash: an attacker's HE Splash dice on a hex and the hexes around it; a shot: dice on a Building with no visible enemy in it
case class Splash(attacker : Squad, hex : Hex, n : Int) extends Record
case class Shot(attacker : Squad, hex : Hex, dice : $[Attack]) extends Record


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
case class StartPickAction(self : Faction, t : UnitType, n : Int) extends BaseAction("Starting Infantry", (n + 1).hlb, "of", (g : Game) => g.startInfantry.hlb)(Images.token(Squad(self, t, 0)) ~ t.elem) with Soft
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
// Full Game Rules: barbed wire (keeps Infantry out) and tank traps (keep Vehicles out), built on the unit's hex for 1 CP
case class DefensesAction(self : Faction, squad : Squad, wire : Boolean, trap : Boolean, k : Int) extends BaseAction(squad, "builds in", (g : Game) => g.where(squad).elem, (g : Game) => g.costNote(squad, 1))((wire && trap).?("Barbed Wire and a Tank Trap").|(wire.?("Barbed Wire").|("a Tank Trap")).hl)
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
case class SpawnMenuAction(self : Faction, t : UnitType) extends BaseAction("Purchase and Spawn")(Images.token(Squad(self, t, 0)) ~ t.elem, "for", (g : Game) => g.costOf(self, t).elem) with Soft
case class SpawnAction(self : Faction, t : UnitType, hex : Hex) extends BaseAction("Spawn", t.elem, "for", (g : Game) => g.costOf(self, t).elem)(hex, (g : Game) => g.spawnNote(self, t, hex)) with HexKey
case class UpgradesMenuAction(self : Faction) extends BaseAction("Purchase")("Upgrades") with Soft
case class UpgradeAction(self : Faction, squad : Squad, slot : Int, effect : Effect) extends BaseAction(squad, "in", (g : Game) => g.where(squad).elem)(effect, "for", (g : Game) => g.slotCost(squad, slot)) with SquadKey
case class UnlockAction(self : Faction, tier : Int) extends BaseAction("Purchase")("Unlock", Tiers.name(self, tier).hl, ("(Tier " + tier + ")").txt, "for", (g : Game) => g.tierCost(self, tier).elem)
case class PassAction(self : Faction) extends BaseAction("Purchase")("Pass", "(no more purchases this round)".spn(styles.small))
case class PurchaseNextAction(f : Faction) extends ForcedAction
case class SupplyEndAction(f : Faction) extends ForcedAction

// FULL GAME RULES
// the Operations deck: shuffled at the start, the round's first team draws a card and picks its top or bottom Operation
case class OpsShuffledAction(shuffled : $[OpCard]) extends ShuffledAction[OpCard]
case class OpsReshuffledAction(f : Faction, shuffled : $[OpCard]) extends ShuffledAction[OpCard]
case class OpDrawAction(f : Faction) extends ForcedAction
case class OpPickAction(self : Faction, card : OpCard, upper : Boolean) extends BaseAction("Operation", "(the other team learns only top or bottom)".spn(styles.small))(Image(card.image, styles.opCard) ~ " " ~ upper.?("Top").|("Bottom").hl ~ ": " ~ card.turns(upper).hlb ~ " turns", ("(" + Operations.side(upper) + ")").spn(styles.small))

// facing: a Heavy Vehicle always faces a hex side; a Weapon Team picks one when it sets up
case class SpawnFaceAction(self : Faction, squad : Squad, edge : Int) extends BaseAction(squad, "spawned in", (g : Game) => g.where(squad).elem, "faces")(Facing.elem(edge))
case class SetUpFacingAction(self : Faction, squad : Squad, edge : Int, k : Int) extends BaseAction(squad, "in", (g : Game) => g.where(squad).elem, "sets up to fire", (g : Game) => g.costNote(squad, 1), "facing")(Facing.elem(edge))
// a Heavy Vehicle's CP: forward or backward, rotating one hex side before or after, or rotating in place (the path is then only its own hex)
case class HeavyMoveAction(self : Faction, squad : Squad, path : $[Hex], edge : Int, k : Int) extends BaseAction(squad, "in", (g : Game) => g.where(squad).elem, "facing", (g : Game) => g.facingNote(squad))((g : Game) => g.heavyMoveNote(squad, path, edge)) with HexKey { def hex = path.last }
// an Assault: moving into an adjacent hex held by an enemy (a Heavy Vehicle also says where it then faces, else -1)
case class AssaultAction(self : Faction, squad : Squad, hex : Hex, edge : Int, k : Int) extends BaseAction(squad, "in", (g : Game) => g.where(squad).elem)("Assault", (g : Game) => g.occupant(hex)./(_.elem).|(Empty), "in", hex, (edge >= 0).?(("facing " + Facing.names(edge)).txt).|(Empty), (g : Game) => g.costNote(squad, 1)) with HexKey
// a Retreat: up to six hexes toward a Retreat Point, free; during a turn, or in the final Retreats after the turns
case class RetreatAction(self : Faction, squad : Squad, point : Hex, end : Hex, k : Int) extends BaseAction(squad, "in", (g : Game) => g.where(squad).elem, "may Retreat (free)")("Retreat toward", point, (end != point).?(("(ends in " + end.name + ")").txt).|(Empty)) with HexKey { def hex = end }
case class FinalRetreatAction(self : Faction, squad : Squad, point : Hex, end : Hex, order : $[Faction]) extends BaseAction("Final Retreats:", squad, "in", (g : Game) => g.where(squad).elem)("Retreat toward", point, (end != point).?(("(ends in " + end.name + ")").txt).|(Empty)) with HexKey { def hex = end }
case class RetreatStepAction(order : $[Faction]) extends ForcedAction
case class RetreatDoneAction(self : Faction, order : $[Faction]) extends BaseAction(None)("Done retreating")
// HE Splash on a hex, and fire at a Building with no visible enemy in it
case class SplashAction(self : Faction, unit : Squad, hex : Hex, n : Int, step : Int, order : $[Faction]) extends BaseAction(unit, "in", (g : Game) => g.where(unit).elem, (g : Game) => g.assignNote(unit))("HE Splash on", hex, (g : Game) => g.splashNote(unit, hex), "with", n.times(Attack(HE).icon).merge) with HexKey
case class ShotAction(self : Faction, unit : Squad, hex : Hex, dice : $[Attack], step : Int, order : $[Faction]) extends BaseAction(unit, "in", (g : Game) => g.where(unit).elem, (g : Game) => g.assignNote(unit))((g : Game) => (g.terrainAt(hex) == Building).?("Fire at the Building in").|("Fire at the defenses in").txt, hex, "with", dice./(_.icon).merge) with HexKey

// COMMANDERS
case class CommandersShuffledAction(f : Faction, shuffled : $[Commander]) extends ShuffledAction[Commander]
case class BrigadesShuffledAction(f : Faction, shuffled : $[Brigade]) extends ShuffledAction[Brigade]
case class BrigadeOpeningAction(order : $[Faction]) extends ForcedAction
case class BrigadeStartAction(self : Faction, b : Brigade, rest : $[Faction]) extends BaseAction("Lost Brigade", "(0 XP ones may be picked now, before the starting Infantry)".spn(styles.small))(b.elem, (b.starting > 0).?(("(" + (b.starting > 1).?("up to " + b.starting).|("one") + " as starting Infantry)").spn(styles.small) : Elem).|(Empty))
case class BrigadeStartSkipAction(self : Faction, rest : $[Faction]) extends BaseAction(None)("Not now")
case class BrigadePickAction(self : Faction, b : Brigade) extends BaseAction("Lost Brigade", "(you keep the one you pick)".spn(styles.small))(b.elem, "for", b.xp.hlb, XP.icon)
case class CommanderTierAction(self : Faction, c : Commander, i : Int) extends BaseAction("Commander", (i == 0).?("(you keep the Commander you take)".spn(styles.small)).|(Empty))((i == 0).?(c.elem ~ ": ").|(Empty), c.tiers(i).ability, "for", c.tiers(i).xp.hlb, XP.icon, (c.tiers(i).needs > 0).?(("(needs Tier " + c.tiers(i).needs + ")").spn(styles.small)).|(Empty))
case class TradeXPAction(self : Faction, r : Res) extends BaseAction("Commander")("Trade", 2.hlb, XP.icon, "for", 1.hlb, r.icon)
case class FreeMajorAction(self : Faction, hex : Hex) extends BaseAction("West Point Reserves:", "a free", Major.elem)(hex) with HexKey
case class BreakthroughAction(self : Faction) extends BaseAction("Commander")("US Breakthrough: pay", 1.hlb, Fuel.icon, "for Infantry to cost 1 less", Manpower.icon, "this Supply Phase")
case class DropMenuAction(self : Faction, a : Ability) extends BaseAction("Commander")(a, (g : Game) => g.dropNote(a)) with Soft
case class DropAction(self : Faction, a : Ability, t : UnitType, hex : Hex) extends BaseAction(a, (g : Game) => g.dropNote(a))(Images.token(Squad(self, t, 0)) ~ t.elem, "in", hex) with HexKey
case class BuildMenuAction(self : Faction, t : UnitType) extends BaseAction("Commander")("Build", Images.token(Squad(self, t, 0)) ~ t.elem, "for", t.cost.elem) with Soft
case class SovietIndustryAction(self : Faction) extends BaseAction("Commander")("Soviet Industry: pay", 1.hlb, Manpower.icon, "for Vehicles costing", 1.hlb, Fuel.icon, "less this Supply Phase")
case class DecoyAction(self : Faction, squad : Squad, k : Int) extends BaseAction(squad, "in", (g : Game) => g.where(squad).elem)("Decoy: the third CP for a move of 3 hexes, No-Fire this round")
case class PromoteAction(self : Faction, squad : Squad, to : UnitType) extends BaseAction("Battlefield Promotions:", squad, "in", (g : Game) => g.where(squad).elem)("Become", to, "for", 4.hlb, Munitions.icon)
case class OberBraceAction(self : Faction, squad : Squad, x : Int, order : $[Faction]) extends BaseAction("Ober Defense:", squad, "in", (g : Game) => g.where(squad).elem)("+" + x, "Defense Dice for", x.hlb, Munitions.icon) with SquadKey
case class EnglishIndustryAction(self : Faction) extends BaseAction("Commander")("English Industry: pay", 2.hlb, Munitions.icon, "for", 1.hlb, Fuel.icon)
case class StepOnItAction(self : Faction, squad : Squad, k : Int) extends BaseAction(squad, "in", (g : Game) => g.where(squad).elem)((g : Game) => (g.unlocked(self, WMBreakthrough) || g.unlocked(self, OKWBreakthrough)).?("Breakthrough Tactics").|("Step on it").txt, ": a free move for", 2.hlb, Munitions.icon)
case class CavalryChargeAction(self : Faction, squad : Squad, hex : Hex, k : Int) extends BaseAction(squad, "in", (g : Game) => g.where(squad).elem)("Cavalry Riflemen: a free Assault on", (g : Game) => g.occupant(hex)./(_.elem).|(Empty), "in", hex) with HexKey
case class DemolitionStepAction(order : $[Faction]) extends ForcedAction
case class DemolitionAction(self : Faction, squad : Squad, hex : Hex, order : $[Faction]) extends BaseAction("Demolition: 4 HE to an adjacent Building")(squad, "in", (g : Game) => g.where(squad).elem, "destroys the Building in", hex) with HexKey
case class DemolitionDoneAction(self : Faction, order : $[Faction]) extends BaseAction(None)("No more Demolition")
case class ReconModeAction(self : Faction, squad : Squad, k : Int) extends BaseAction(squad, "in", (g : Game) => g.where(squad).elem)("Recon Mode: a free move for", 2.hlb, Munitions.icon)
case class WarSpeedAction(self : Faction, squad : Squad, k : Int) extends BaseAction(squad, "in", (g : Game) => g.where(squad).elem)("Emergency War Speed: a free move for", 1.hlb, "Health")
case class SalvageAction(self : Faction, r : Res) extends BaseAction("Commander")("Thorough Salvage: pay", 2.hlb, Manpower.icon, "for", (r == Fuel).?(1).|(2).hlb, r.icon)
case class BraceAction(self : Faction, squad : Squad, order : $[Faction]) extends BaseAction("Advanced Emplacements: +1 Defense Die for", 1.hlb, Munitions.icon)(squad, "in", (g : Game) => g.where(squad).elem) with SquadKey
case class BraceDoneAction(self : Faction, rest : $[Faction]) extends BaseAction(None)("Done")
case class BraceStepAction(order : $[Faction]) extends ForcedAction
case class AssignedAction(f : Faction) extends ForcedAction
case class EmplaceMenuAction(self : Faction, t : UnitType) extends BaseAction("Emplacements")("Build", Images.token(Squad(self, t, 0)) ~ t.elem, "for", t.cost.elem) with Soft
case class ConvertAction(self : Faction, tier : Int, truck : Squad) extends BaseAction("Tier Buildings")("Convert", truck, "in", (g : Game) => g.where(truck).elem, "into the", Tiers.name(self, tier).hl, ("(Tier " + tier + ")").txt, "for", (g : Game) => g.tierCost(self, tier).elem)
case class NoFireModeAction(self : Faction, squad : Squad, k : Int) extends BaseAction(squad, "in", (g : Game) => g.where(squad).elem)((g : Game) => g.noFire.contains(squad).?("Back to firing mode").|("No-Fire Capture mode"), (g : Game) => g.costNote(squad, 1))
case class StrafeAction(self : Faction, unit : Squad, target : Squad, dice : $[Attack], step : Int, order : $[Faction]) extends BaseAction(unit, "Strafing Fire: also")("Attack", target, "in", (g : Game) => g.where(target).elem, "with", dice./(_.icon).merge) with SquadKey { def squad = target }
case class NoStrafeAction(self : Faction, unit : Squad, step : Int, order : $[Faction]) extends BaseAction(unit, "Strafing Fire: also")("No second target")
case class BuildAction(self : Faction, builder : Squad, t : UnitType, hex : Hex) extends BaseAction("Build", t.elem, "for", t.cost.elem)(builder, "in", (g : Game) => g.where(builder).elem, "builds in", hex) with HexKey
case class FreeNextAction(f : Faction) extends ForcedAction
case class FreeSpawnAction(self : Faction, t : UnitType, hex : Hex) extends BaseAction("Free", (g : Game) => g.freeNote)(Images.token(Squad(self, t, 0)) ~ t.elem, "in", hex) with HexKey
case class DefensiveStanceAction(self : Faction, squad : Squad, k : Int) extends BaseAction(squad, "in", (g : Game) => g.where(squad).elem)("Defensive Stance", "(Heavy Cover this round, no more CP)".spn(styles.small), (g : Game) => g.costNote(squad, 1))
case class HullDownAction(self : Faction, squad : Squad, vehicle : Squad, k : Int) extends BaseAction(squad, "in", (g : Game) => g.where(squad).elem)("Hull Down", vehicle, "in", (g : Game) => g.where(vehicle).elem, "(2 Defense Dice until it moves)".spn(styles.small), (g : Game) => g.costNote(squad, 1))
case class RaidPointAction(self : Faction, squad : Squad, k : Int) extends BaseAction(squad, "in", (g : Game) => g.where(squad).elem)("Raid Point: a free move for", 1.hlb, Munitions.icon)
case class OpeningAction(self : Faction, a : Ability, k : Int) extends BaseAction("Start of the Maneuver Phase")(a, (g : Game) => g.openingNote(a))
case class OpeningHexAction(self : Faction, a : Ability, hex : Hex, k : Int) extends BaseAction("Start of the Maneuver Phase:", a, (g : Game) => g.openingNote(a))(hex) with HexKey
case class OpeningLineStartAction(self : Faction, a : Ability, hex : Hex, k : Int) extends BaseAction("Start of the Maneuver Phase:", a, (g : Game) => g.openingNote(a), "from")(hex) with HexKey with Soft
case class OpeningLineAction(self : Faction, a : Ability, hexes : $[Hex], k : Int) extends BaseAction(a, "on three hexes")(hexes./(_.elem).join(", ")) with HexKey { def hex = hexes.last }
// Ice (Full Game Rules 10.4): the checks of the units attacked on Ice tiles
case class IceCheckAction(f : Faction) extends ForcedAction
case class IceRolledAction(f : Faction, rolled : $[Face]) extends RolledAction[Face]
case class SmokeStepAction(order : $[Faction]) extends ForcedAction
case class SmokeAction(self : Faction, vehicle : Squad, order : $[Faction]) extends BaseAction("Smoke for", (g : Game) => g.smokeCost(vehicle).|(2).hlb, Munitions.icon)(vehicle, "in", (g : Game) => g.where(vehicle).elem) with SquadKey { def squad = vehicle }
case class SmokeDoneAction(self : Faction, order : $[Faction]) extends BaseAction(None)("No more Smoke")

case class GameOverAction(winner : Faction) extends ForcedAction
case class GameOverWonAction(self : Faction, f : Faction) extends BaseInfo("Game Over")(f, "won")


object Facing {
    val names = $("north-east", "east", "south-east", "south-west", "west", "north-west")
    val arrows = $("↗", "→", "↘", "↙", "←", "↖")
    def elem(e : Int) = (arrows(e) + " " + names(e)).hl
}

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

    // TEAMS (2026-11-24): two players are two teams; three to five play Allies against Axis (rulebook p. 6 and 11.0, uneven teams
    // allowed), the team of the first seat being team 1 (its Spawn Area the first). Teammates share VPs, sight and their Control Points
    // a free-for-all mission (the Mission Booklet's 3 Way FFA, 8.0) plays every player as a team of their own
    lazy val freeForAll = map.areas.num > 2

    lazy val teamGame = setup.num > 2 && freeForAll.not

    lazy val teams : $[$[Faction]] =
        if (teamGame) {
            val (a, b) = setup.partition(_.axis == setup(0).axis)
            $(a, b)
        }
        else
            setup./(f => $(f))

    def team(f : Faction) = teams.indexWhere(_.has(f))

    def mates(f : Faction) : $[Faction] = teams(team(f))

    def enemies(f : Faction) : $[Faction] = setup.%(e => foe(e, f))

    def foe(a : Faction, b : Faction) = team(a) != team(b)

    def teamVP(i : Int) = teams(i)./(_.stock(VP)).sum

    // the VPs that end the game: 3 more in team games (p. 6), the mission's own in a free-for-all
    def endVP = map.trigger.|(length.vp + teamGame.??(3))

    // the starting Infantry each player places: 3, 2 in team games (2-panel missions), or the map's own number
    def startInfantry = map.infantry.|(teamGame.?(2).|(3))

    // the teams in turn order from team i
    def rotation(i : Int) : $[$[Faction]] = teams.drop(i) ++ teams.take(i)

    // the order teams alternate in, teammates in seating order: A1, B1, A2, B2...
    def interleaved(f : Faction) : $[Faction] = {
        val r = rotation(team(f))
        (0 until r./(_.num).max).toList./~(i => r./~(_.lift(i).toList))
    }

    // a destroyed unit's VP: to the other team, or in a free-for-all to every player who assigned it Damage (11.0)
    def killed(x : Squad, by : $[Faction]) {
        Jests.note(this, "destroyed", x.faction, by.%(e => foe(e, x.faction)).headOption.|(other(x.faction)), x.utype.name)
        if (has(x, HeroicSacrifice).not) {
            if (freeForAll)
                by.distinct.%(e => foe(e, x.faction)).foreach { e =>
                    gain(e, VP, 1)
                    log(e, "gained", 1.hl, "VP for", x)
                }
            else
                gain(other(x.faction), VP, 1)
        }
    }

    // who assigned Damage to a unit in this Damage Phase
    def damagers(t : Squad) : $[Faction] = (assignments.%(_.target == t)./(_.attacker.faction) ++
        splashes.%(s => splashed(s.attacker, s.hex).has(t))./(_.attacker.faction) ++ strikes.%(s => pos.get(t).exists(s.hexes.has))./(_.faction)).distinct

    val length : Length = options.of[Length].single.|(ShortGame)

    // the map: Board answers for it
    val map : MapData = {
        val m = options.of[MapChoice].single./(_.map).|(MoroRiverFirst)
        (options.has(OptionalMapFeatures) && m.optional.any).?(m.withOptional).|(m)
    }
    Board.current = map
    setup.foreach { f =>
        val (m, u, l) = map.stock
        states(f).stock += Manpower -> m
        states(f).stock += Munitions -> u
        states(f).stock += Fuel -> l
    }

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
    // built barbed wire and tank traps (Full Game Rules), not the map's permanent tank traps
    var wire = Set[Hex]()
    var traps = Set[Hex]()
    var control = Map[Hex, Faction]()
    var spawned = Map[UnitType, Int]()

    // the round
    var round = 0

    // what happened to whom, for the Assault Specialist's jests (display only: online.scala)
    var jests : $[String] = $
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

    // FULL GAME RULES
    val full = options.has(FullGameRules)
    var opsDeck = $[OpCard]()
    var opsDiscard = $[OpCard]()
    var opsCard : |[OpCard] = None
    var opsPicker : Faction = null
    var opsUpper = true
    var opsTurns = 4
    // the hex side a Heavy Vehicle, or a set-up Weapon Team, faces (0 north-east, clockwise)
    var facing = Map[Squad, Int]()
    // the Assaults: the assaulting unit, in the hex of the unit it assaults
    var assaults = Map[Squad, Squad]()
    var retreating = Set[Squad]()
    // Retreats that end at the end of the round wherever the unit is (a failed Assault, a destroyed Building)
    var retreatEnds = Set[Squad]()
    var finalMoved = Set[Squad]()
    // a Heavy Vehicle's damage types already assigned this Damage Phase (one target per type)
    var used = Map[Squad, Set[Dmg]]()
    var splashes = $[Splash]()
    var shots = $[Shot]()
    var buildingDamage = Map[Hex, Int]()
    var ruined = Set[Hex]()
    // units that fell through the Ice this Damage Phase
    var sunk = Set[Squad]()
    // the T-70s (and Scout Cars, Assault Tactics) in their No-Fire mode
    var noFire = Set[Squad]()
    // British, Soviet and OKW Commanders: the vehicles that smoked with Vehicle Crew Repairs, the Emplacements braced with Advanced
    // Emplacements this Damage Phase, the round of a faction's Rapid Conscription and Fortifications Doctrine
    var crewRepairs = Set[Squad]()
    var braced = Set[Squad]()
    var conscription = Map[Faction, Int]()
    var fortified = Map[Faction, Int]()

    // COMMANDERS (commanders.scala)
    val commanders = full && options.has(DrawCommanders)
    // the Lost Brigades (brigades.scala), and whether anything gives abilities: Commanders or brigades
    val brigades = full && options.has(LostBrigades)
    val abilities = commanders || brigades
    var drawn = Map[Faction, $[Commander]]()
    var brigadesDrawn = Map[Faction, $[Brigade]]()
    var brigadeOf = Map[Faction, Brigade]()
    var commanderOf = Map[Faction, Commander]()
    var tiersOf = Map[Faction, Int]()
    // the round each once-a-round ability (Paratroopers, a Paradrop, an Airdropped Group, Stormtroopers, an opening ability) was last used
    var usedIn = Map[(Faction, Ability), Int]()
    // this round: the hexes a Recon Sweep shows, the Smoke, the strikes to resolve, Riflemen in a Defensive Stance, vehicles Hull Down
    var eyes = Map[Faction, Set[Hex]]()
    var smoke = Set[Hex]()
    var strikes = $[Strike]()
    var stance = Set[Squad]()
    var hullDown = Set[Squad]()
    // free moves this turn (Raid Point, Combat Group Advance, Armor Blitzkrieg), and the Infantry a Raid Point was paid for this round
    var bonus = Map[Squad, Int]()
    var raided = Set[Squad]()
    // Cavalry Riflemen that made their free Assault this round; Combined Arms (the round it was used)
    var charged = Set[Squad]()
    var combinedArms = Map[Faction, Int]()
    // Tank Destroyer Doctrine: the marked vehicles and the round they were marked
    var marked = Map[Squad, Int]()
    // Soviet Industry this Supply Phase; Decoy units this round
    var sovietIndustry = Set[Faction]()
    var decoyed = Set[Squad]()
    // Ober Defense: the Defense Dice bought this round
    var oberBraced = Map[Squad, Int]()
    // Double Slow the first round (Paratroopers, Paradrops)
    var doubleSlow = Set[Squad]()
    // Emplacements built by each unit this Supply Phase, the round each unit spawned, the factions with US Breakthrough this Supply Phase
    var built = Map[Squad, Int]()
    var spawnRound = Map[Squad, Int]()
    var breakthrough = Set[Faction]()
    // the free units still to spawn after a tier was unlocked: the types to pick from, how they spawn (spawn, para, drop), an upgrade
    var freeQueue = $[(List[UnitType], String, |[Effect])]()
    var lastDrop : |[Hex] = None

    var highlightFaction = $[Faction]()

    // SQUADS

    def units(f : Faction) : $[Squad] = pos.keys.$.%(_.faction == f).sortBy(u => (UnitType.all.indexOf(u.utype), u.n))
    def all : $[Squad] = setup./~(units)
    def where(u : Squad) : Hex = pos(u)
    def carrier(u : Squad) : |[Squad] = carried.get(u)
    def passenger(v : Squad) : |[Squad] = carried.find(_._2 == v).map(_._1)
    def isPassenger(u : Squad) = carried.contains(u)
    // the unit standing in a hex (a passenger is inside its vehicle)
    def occupant(h : Hex) : |[Squad] = pos.find { case (u, x) => x == h && isPassenger(u).not && assaults.contains(u).not }.map(_._1)
    // every unit standing in a hex: the occupant, and a unit assaulting it
    def standing(h : Hex) : $[Squad] = pos.toList.filter { case (u, x) => x == h && isPassenger(u).not }.map(_._1).sortBy(u => (setup.indexOf(u.faction), UnitType.all.indexOf(u.utype), u.n))

    def has(u : Squad, e : Effect) = effects(u).contains(e)
    def boughtEffects(u : Squad) = bought.getOrElse(u, $[Bought]())./(_.effect)
    // what it bought, and what its Commander gives it (less what it already bought)
    def effects(u : Squad) : $[Effect] = { val b = boughtEffects(u) ; b ++ (freeEffects(u) ++ u.utype.innate).diff(b) }
    def attacksOf(u : Squad) : $[Attack] = {
        // a Bunker (Full Game Rules 10.1): its Infantry has an AP die (only against units outside it, `diceOnFull`)
        val bunker = (full && u.kind == Infantry && pos.get(u).exists(h => Board.bunkers.has(h) && ruined.contains(h).not) && isPassenger(u).not).$(UnitType.ap)
        val l = bunker ++ u.utype.attacks ++ effects(u)./~{ case AddAttack(a) => $(a) ; case AddAttacks(a) => $(a, a) ; case FlameRefit => $(UnitType.flame) ; case CloseFire => $(UnitType.ai, UnitType.ai) ; case _ => $ }
        // Operation Crocodile: the Churchill's AI becomes 2 forward Flame
        if (u.utype == Churchill && unlocked(u.faction, OperationCrocodile)) l.%(_.dmg != AI) ++ $(UnitType.flame, UnitType.flame) else l
    }
    // the base AI dice that are Close Range Firepower, and the Thompson SMGs'
    def closeAttacks(u : Squad) : $[Attack] = u.utype.attacks.%(_.dmg == AI).take(u.utype.close) ++ effects(u).count(CloseFire).times($(UnitType.ai, UnitType.ai)).flatten
    // the Wrenches: Kettenkrad Engineers have one
    def wrenchesOf(u : Squad) = u.utype.wrenches + (u.utype == Kettenkrad && unlocked(u.faction, KettenkradEngineers)).??(1) +
        ((u.utype == PenalBattalion || u.utype == ShockTroops) && unlocked(u.faction, BattlefieldPromotions)).??(1)
    def rangeOf(u : Squad) = u.utype.range + effects(u).count(RangeUp) + effects(u).count(SightRangeUp) + lmgRange(u) + combined(u).??(1)
    def sightOf(u : Squad) = u.utype.sight + has(u, SpotsCamo).??(2) + effects(u).count(SightUp) + effects(u).count(SightRangeUp) + combined(u).??(1) + boysAT(u).??(1)
    // Combined Arms: a unit next to another of its faction's this round
    def combined(u : Squad) = combinedArms.get(u.faction).has(round) && pos.contains(u) && units(u.faction).exists(v => v != u && pos.contains(v) && Board.adjacent(where(v), where(u)))
    // a vehicle that transports Infantry and captures (the Universal Carrier stops with its flamethrower), and the T-70 in its No-Fire mode
    def carries(v : Squad) = v.utype.transport && has(v, FlameRefit).not
    def captures(v : Squad) = (v.utype.capture && has(v, FlameRefit).not) || noFire.contains(v)
    def spotsCamo(u : Squad) = has(u, SpotsCamo)
    def camouflaged(u : Squad) = has(u, Camouflage) || (u.kind == Emplacement && fortified.get(u.faction).has(round))
    def defenseOf(u : Squad) = u.utype.defense + effects(u).count(DefenseUp)
    def speedOf(u : Squad) = u.utype.speed + effects(u).count(SpeedUp)
    def reinforces(u : Squad) = u.utype.reinforce || has(u, ReinforceUp)
    def maxHealth(u : Squad) = u.utype.health + effects(u).count(HealthUp)
    // a Building destroyed in the Full Game Rules is a Destroyed Building
    def terrainAt(h : Hex) : Terrain = ruined.contains(h).?(Ruin : Terrain).|(Board.terrain(h))
    def inBuilding(h : Hex) = terrainAt(h) == Building || terrainAt(h) == Ruin || terrainAt(h) == Cathedral
    // open ground, Ice too
    def isOpen(h : Hex) = terrainAt(h) == Open || terrainAt(h) == Ice
    // Ice tiles (Full Game Rules)
    def onIce(u : Squad) = full && pos.contains(u) && terrainAt(where(u)) == Ice
    // a hex of a multi-hex Building that is still standing
    def sameBuilding(a : Hex, b : Hex) = Board.sameBuilding(a, b) && ruined.contains(a).not && ruined.contains(b).not
    def onBuilding(u : Squad) = inBuilding(where(u))

    // the most CP a unit may take a round (Slow takes one away)
    def cap(u : Squad) = rangers(u).?(4).|(3) - slowOf(u) - slowed.get(u).has(round).??(doubleSlow.contains(u).?(2).|(1))

    def onBoard(f : Faction, t : UnitType) = units(f).count(_.utype.chassis == t.chassis)

    // CONTROL POINTS and income: 1 of each resource, and 1 for each resource of each Control Point held

    def pointsOf(f : Faction) : $[Hex] = control.toList.filter(_._2 == f).map(_._1).sortBy(h => (h.r, h.c))

    def income(f : Faction, r : Res) : Int = (r != VP).??(1) + pointsOf(f)./(h => Board.points(h).count(r)).sum +
        // Assault Tactics: a Scout Car in No-Fire mode on a Control Point, 1 more of its (first) resource
        units(f).count(v => ((v.utype == ScoutCar && unlocked(f, SupplyOperations)) || (v.utype == Kubelwagen && unlocked(f, AssaultSupport))) && noFire.contains(v) && Board.points.get(where(v)).exists(_.%(_ != VP).headOption.has(r)))

    // MOVEMENT: one CP a hex; a Speed Die is a free CP used before any CP

    def speedLeft(u : Squad) = (cp.getOrElse(u, 0) == 0).??(speedOf(u) - sped.getOrElse(u, 0))
    def freeLeft(u : Squad) = speedLeft(u) + bonus.getOrElse(u, 0)
    def paidLeft(u : Squad) = min(cpLeft, cap(u) - cp.getOrElse(u, 0))
    def pointsLeft(u : Squad) = freeLeft(u) + paidLeft(u)

    def spend(u : Squad, n : Int) {
        0.until(n).foreach { _ =>
            if (speedLeft(u) > 0)
                sped += u -> (sped.getOrElse(u, 0) + 1)
            else
            if (bonus.getOrElse(u, 0) > 0)
                bonus += u -> (bonus(u) - 1)
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

    // Moro River's tank traps (Full Game Rules) keep out Vehicles
    def canEnter(u : Squad, h : Hex) = canAssault(u, h) && (u.utype.vehicle.not || traps.contains(h).not) && (u.kind != Infantry || wire.contains(h).not)

    // the same, but past built barbed wire and tank traps, as an Assault on an enemy unit behind them may go
    def canAssault(u : Squad, h : Hex) = (terrainAt(h) != Water || u.utype == DevilsBrigade) && (u.utype.vehicle.not || (inBuilding(h).not && (full && Board.traps.has(h)).not))

    // a hex with built Battlefield Defenses that two damage would destroy: sandbags, barbed wire, tank traps
    def defended(h : Hex) = sandbags.contains(h) || wire.contains(h) || traps.contains(h)

    // all Battlefield Defenses of a hex are destroyed together
    def destroyDefenses(h : Hex) {
        val l = $[String]() ++ sandbags.contains(h).$("Sandbags") ++ wire.contains(h).$("Barbed Wire") ++ traps.contains(h).$("Tank Trap")
        sandbags -= h
        wire -= h
        traps -= h
        if (l.any)
            log("The", l.mkString(", "), "in", h, (l.num > 1).?("were").|("was"), "destroyed")
    }

    // the barbed wire and tank traps a unit with a Wrench may build on its hex (1 CP; both at once with two Wrenches); not in a
    // Building, nor where an Assault is going on
    def defenseChoices(f : Faction, u : Squad, k : Int) : $[UserAction] = (full && wrenchesOf(u) > 0 && onBuilding(u).not && isPassenger(u).not && assaultAt(where(u)).not && assaults.contains(u).not).??{
        val h = where(u)
        val w = wire.contains(h).not
        val t = traps.contains(h).not && Board.traps.has(h).not
        w.$(DefensesAction(f, u, true, false, k)) ++ t.$(DefensesAction(f, u, false, true, k)) ++ (w && t && wrenchesOf(u) >= 2).$(DefensesAction(f, u, true, true, k))
    }

    def stepsFrom(u : Squad, h : Hex) = Board.neighbors(h).%(n => Board.joined(h, n) && canEnter(u, n))

    // the moves of a unit standing in a hex: into an empty hex, or through an occupied one into an empty one (both CP at once)
    def moves(u : Squad) : $[$[Hex]] = {
        val h = where(u)
        val left = pointsLeft(u)
        val one = stepsFrom(u, h)
        (left >= 1).??(one.%(occupant(_).none)./($(_))) ++
        (left >= 2).??(one.%(occupant(_).any)./~(m => stepsFrom(u, m).%(n => n != h && occupant(n).none && one.has(n).not)./($(m, _)))).distinctBy(_.last)
    }

    def transports(u : Squad) : $[Squad] = (u.kind == Infantry && pointsLeft(u) >= 1).??(stepsFrom(u, where(u))./~(occupant).%(v => v.faction == u.faction && carries(v) && passenger(v).none))

    def mayMove(u : Squad) = setUp.contains(u).not && u.kind != Emplacement && stance.contains(u).not

    def unitActions(f : Faction, u : Squad, k : Int) : $[UserAction] = {
        val left = pointsLeft(u)
        if (left <= 0)
            $
        else
        if (full && (retreating.contains(u) || assaults.contains(u) || stance.contains(u)))
            // a Retreating unit spends no CP, an Assaulting one no more (its passenger may still leave), nor one in a Defensive Stance
            $
        else
        if (full && isPassenger(u).not && u.kind == HeavyVehicle)
            heavyOptions(u)./{ case (p, e, true) => AssaultAction(f, u, p.last, e, k) ; case (p, e, false) => HeavyMoveAction(f, u, p, e, k) } ++ noFireMode(u).$(NoFireModeAction(f, u, k)) ++ commanderActions(f, u, k)
        else
        if (full && isPassenger(u).not) {
            val m = mayMove(u).??(moves(u)./(p => MoveAction(f, u, p, k)) ++ assaultHexes(u)./(h => AssaultAction(f, u, h, -1, k)) ++ transports(u)./(v => EmbarkAction(f, u, v, where(v), k)))
            val s = u.utype.setsUp.??(setUp.contains(u).?($(PackUpAction(f, u, k))).|((u.utype.noBuildingSetup && onBuilding(u)).not.??(0.until(6).$./(e => SetUpFacingAction(f, u, e, k)))))
            val b = (wrenchesOf(u) > 0 && onBuilding(u).not).??(0.until(6).$.%(e => Sandbags.edges(e).forall(sandbags.getOrElse(where(u), Set()).contains).not)./(e => SandbagsAction(f, u, e, k)))
            m ++ s ++ b ++ defenseChoices(f, u, k) ++ noFireMode(u).$(NoFireModeAction(f, u, k)) ++ commanderActions(f, u, k)
        }
        else
        if (isPassenger(u))
            stepsFrom(u, where(u)).%(occupant(_).none)./(h => DisembarkAction(f, u, h, k))
        else {
            val m = mayMove(u).??(moves(u)./(p => MoveAction(f, u, p, k)) ++ transports(u)./(v => EmbarkAction(f, u, v, where(v), k)))
            val s = u.utype.weaponTeam.??(setUp.contains(u).?($(PackUpAction(f, u, k))).|((u.utype.noBuildingSetup && onBuilding(u)).not.$(SetUpAction(f, u, k))))
            val b = (wrenchesOf(u) > 0 && onBuilding(u).not).??(0.until(6).$.%(e => Sandbags.edges(e).forall(sandbags.getOrElse(where(u), Set()).contains).not)./(e => SandbagsAction(f, u, e, k)))
            m ++ s ++ b ++ noFireMode(u).$(NoFireModeAction(f, u, k))
        }
    }

    def moveSquad(u : Squad, h : Hex) {
        if (hullDown.contains(u)) {
            hullDown -= u
            log("The sandbags around", u, "were destroyed")
        }
        pos += u -> h
        passenger(u).foreach(p => pos += p -> h)
        if (u.kind == HeavyVehicle && sandbags.contains(h)) {
            sandbags -= h
            log(u, "crushed the Sandbags in", h)
        }
        if (u.kind == HeavyVehicle && wire.contains(h) && assaults.contains(u).not) {
            wire -= h
            log(u, "crushed the Barbed Wire in", h)
        }
    }

    def removeSquad(u : Squad) {
        passenger(u).foreach(removeSquad)
        noFire -= u
        stance -= u
        hullDown -= u
        bonus -= u
        raided -= u
        doubleSlow -= u
        built -= u
        facing -= u
        assaults -= u
        assaults = assaults.filter(_._2 != u)
        retreating -= u
        retreatEnds -= u
        finalMoved -= u
        used -= u
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
        d <= sightOf(s) && lineClear(where(s), where(t)) && (camouflaged(t).not || d <= 1 || spotsCamo(s)) && ((smoke.contains(where(s)) || smoke.contains(where(t))).not || d <= 1)
    }

    // a line not blocked by Buildings, Destroyed Buildings or Smoke (Commanders) between its ends
    def lineClear(a : Hex, b : Hex) = Board.lineBlocked(a, b, h => (Board.blocks(h) && sameBuilding(h, a).not && sameBuilding(h, b).not) || smoke.contains(h)).not

    // a Recon Sweep shows its hex and the hexes around it to the team, Camouflage too
    def eyed(f : Faction, h : Hex) = eyes.getOrElse(f, Set()).exists(e => e == h || Board.adjacent(e, h))

    // (a Retreating unit doesn't spot, in the Full Game Rules)
    def visible(f : Faction, t : Squad) = isPassenger(t).not && (mates(f)./~(units).exists(s => sees(s, t) && (full && retreating.contains(s)).not) || (pos.contains(t) && mates(f).exists(eyed(_, where(t)))))

    // ATTACKS

    def canFire(u : Squad) = pinned.contains(u).not && noFire.contains(u).not && decoyed.contains(u).not && (u.utype.setsUp.not || setUp.contains(u)) && (isPassenger(u).not || u.utype.weaponTeam.not) && (full && retreating.contains(u)).not

    // the dice a unit puts on a target: all of them with a clear line, only the indirect ones over Buildings
    def diceOn(u : Squad, t : Squad) : $[Attack] = if (full) diceOnFull(u, t) else {
        val d = Board.distance(where(u), where(t))
        if (d < u.utype.minRange || d > rangeOf(u) || d == 0 || visible(u.faction, t).not)
            $
        else
        if (Board.clear(where(u), where(t)))
            attacksOf(u)
        else
            attacksOf(u).%(_.indirect)
    }

    def targetsOf(u : Squad) : $[(Squad, $[Attack])] = canFire(u).??(enemies(u.faction)./~(units).map(t => (t, diceOn(u, t))).filter(_._2.nonEmpty))

    // the target choices of a unit: in the Full Game Rules a Heavy Vehicle may put each damage type on a target of its own
    def targetChoices(u : Squad) : $[(Squad, $[Attack])] = targetsOf(u)./~{ case (t, d) =>
        val types = d./(_.dmg).distinct
        val fresh = used.get(u).forall(_.isEmpty)
        val alt = fresh.??(tulip(u, t).$((t, 3.times(UnitType.he)))) ++ gammon(u, t).$((t, 3.times(UnitType.he))) ++
            (fresh && u.utype.alt.any && Board.distance(where(u), where(t)) <= u.utype.altRange).$((t, u.utype.alt)) ++
            // the KV-8: 1 more Flame Assaulting or at a unit in a Building
            (u.utype == KV8 && (assaults.get(u).has(t) || inBuilding(where(t)))).$((t, d :+ UnitType.flame))
        if (full && u.kind == HeavyVehicle && types.num > 1)
            $((t, d)) ++ types./(x => (t, d.%(_.dmg == x))) ++ alt
        else
            $((t, d)) ++ alt
    }

    // Tulip: a Firefly may deal 3 HE instead of its attack, for 2 Munitions; Gammon Bombs: Commandos 3 HE to an adjacent target
    def tulip(u : Squad, t : Squad) = u.utype == Firefly && u.faction.stock(Munitions) >= 2
    def gammon(u : Squad, t : Squad) = u.utype == Commandos && unlocked(u.faction, GammonBombs) && Board.distance(where(u), where(t)) <= 1

    def assignNote(u : Squad) : Elem = ("(" + attacksOf(u)./(_.dmg.short).mkString(", ") + ", range " + (u.utype.minRange > 1).??(u.utype.minRange + "-") + rangeOf(u) + ")").spn(styles.small)

    def relevant(t : Squad, dice : $[Attack]) = dice.exists(a => Matrix.relevant(t.kind, a.dmg))

    // the defense a target gets from where it stands: Infantry in a Building 3 dice, in a Destroyed Building 1, else the best
    // Cover line (or Sandbags) crossed by an attack on it
    def staticCover(t : Squad, from : $[Hex]) : Int = {
        if (t.kind != Infantry)
            0
        else {
            val h = where(t)
            terrainAt(h) @@ {
                case Building | Cathedral => 3
                // a Devil's Brigade in a Destroyed Building gets 3
                case Ruin => (t.utype == DevilsBrigade && unlocked(t.faction, DevilsBrigadeSpawn)).?(3).|(1)
                case _ =>
                    from./{ x =>
                        val edges = Board.crossed(h, x)
                        val lines = edges./~(e => Board.cover.get((h, e)))./(_.dice)
                        // Riflemen Field Defenses: 1 more behind Sandbags
                        val bags = edges.exists(e => sandbags.getOrElse(h, Set()).contains(e)).??(2 + ((riflemen(t) && unlocked(t.faction, RiflemenFieldDefenses)) || (t.utype == Conscripts && unlocked(t.faction, ConscriptPacks))).??(1))
                        (lines :+ bags).max
                    }.maxOr(0)
            }
        }
    }

    // TURN ORDER

    // the next team's first player
    def other(f : Faction) = teams((team(f) + 1) % teams.num).head

    def turnOrder = rotation(team(first)).flatten

    // the next player in a round's turn: a teammate still to play this turn, else the other team's first player
    def nextTurn(f : Faction) : (Faction, Boolean) = mates(f).dropWhile(_ != f).drop(1).starting./(m => (m, false)).|((other(f), true))

    def spawnHexes(f : Faction, t : UnitType) : $[Hex] = {
        val area = Board.spawn(team(f)).%(occupant(_).none)
        val transports = (t.kind == Infantry).??(Board.spawn(team(f))./~(occupant).%(v => v.faction == f && carries(v) && passenger(v).none)./(where))
        val major = (t.kind == Infantry).??(units(f).%(u => has(u, SpawnAdjacent) && isPassenger(u).not && (u.utype == Pathfinder && t == Pathfinder).not)./~(u => Board.neighbors(where(u)).%(n => Board.joined(where(u), n) && terrainAt(n) != Water && occupant(n).none)))
        // OKW: Tier 2 and 3 units next to their Tier Building
        val building = brigadeUnit(f, t).?($[Hex]()).|(((t.tier >= 2).??(tierBuildingOf(f, t.tier).$) ++ (t == Obersoldaten && unlocked(f, TerrorWestOber)).??(tierBuildingOf(f, 2).$))./~(u => Board.neighbors(where(u)).%(n => Board.joined(where(u), n) && terrainAt(n) != Water && occupant(n).none)))
        // the Soviet Forward Station: Vehicles next to it
        val station = t.vehicle.??(units(f).%(_.utype == ForwardStation)./~(u => Board.neighbors(where(u)).%(n => Board.joined(where(u), n) && terrainAt(n) != Water && occupant(n).none)))
        if (t == Stormtroopers || t == AIPartisans || t == ATPartisans)
            stormHexes(f)
        else
            (area ++ transports ++ major ++ building ++ station).distinct.%(h => t.vehicle.not || inBuilding(h).not)
    }

    // OKW's Tier Buildings on the map
    def tierBuildingOf(f : Faction, tier : Int) : |[Squad] = units(f).%(_.utype.tierBuilding == tier).single

    def spawnNote(f : Faction, t : UnitType, h : Hex) : Elem =
        occupant(h).%(_.utype.transport)./(v => ("(in " + v.utype.name + ")").spn(styles.small) : Elem).|(Board.spawn(team(f)).has(h).not.?(((t.tier >= 2).??(tierBuildingOf(f, t.tier)).%(b => Board.adjacent(where(b), h))./(b => "(next to the " + b.utype.name + ", " + (t.slow > 0).?("Double ").|("") + "Slow next round)").|(units(f).%(u => u.utype != Major && has(u, SpawnAdjacent) && Board.adjacent(where(u), h)).starting./(u => "(next to " + u.utype.name + ", Slow this round)").|("(next to the Major, Slow this round)"))).spn(styles.small) : Elem).|(Empty))

    def affordable(f : Faction, c : Cost) = f.stock(Manpower) >= c.mp && f.stock(Munitions) >= c.mun && f.stock(Fuel) >= c.fuel

    def pay(f : Faction, c : Cost) {
        f.stock += Manpower -> (f.stock(Manpower) - c.mp)
        f.stock += Munitions -> (f.stock(Munitions) - c.mun)
        f.stock += Fuel -> (f.stock(Fuel) - c.fuel)
    }

    def gain(f : Faction, r : Res, n : Int) {
        f.stock += r -> (f.stock(r) + n)
    }

    def buyable(f : Faction) : $[UnitType] = (UnitType.of(f).%(t => t.tier <= f.tier && t.commander.not) ++ calledIn(f)).distinct.%(_.purchasable).%(t => affordable(f, costOf(f, t)) && onBoard(f, t) < t.limit && spawnHexes(f, t).any)

    def slotCost(u : Squad, slot : Int) : Elem = {
        val s = slotAt(u, slot)
        slotPrice(u, slot).hlb ~ s.res.icon
    }

    // (an upgrade a Commander already gives the unit isn't offered; a Commander's own upgrade only once a unit)
    def upgrades(f : Faction) : $[(Squad, Int, Effect)] = units(f)./~{ u =>
        slotsOf(u)./~{ case (i, s) =>
            val n = bought.getOrElse(u, $[Bought]()).count(_.slot == i)
            (n < s.times && f.stock(s.res) >= slotPrice(u, i)).??(s.options.%(e => s.times > 1 || n == 0).%(e => (u.utype == Churchill && e == AddAttack(UnitType.ai) && unlocked(f, OperationCrocodile)).not).%(e => e != SplashUp || full).%(e => freeEffects(u).has(e).not).%(e => i < 10 || has(u, e).not).%(e => (i < 2 && u.utype == DevilsBrigade && unlocked(f, VeteranDevils)).not)./(e => (u, i, e)))
        }
    }

    def unlockable(f : Faction) : |[Int] = (f.tier < 3 && f.base != OKW).?(f.tier + 1).%(t => affordable(f, tierCost(f, t)))

    // OKW converts a SWS Supply Truck (on the map, not Retreating) into its next Tier Building, or into one it lost (one of each at a time)
    def conversions(f : Faction) : $[UserAction] = (f.base == OKW).??(
        $(2, 3).%(t => t <= f.tier + 1 && tierBuildingOf(f, t).none && affordable(f, tierCost(f, t)))./~(t =>
            units(f).%(v => v.utype == SWSTruck && retreating.contains(v).not)./(v => ConvertAction(f, t, v))))

    // the Emplacements built by Infantry (British Mortar and Bofors Emplacements, OKW Flak Guns) a faction may build now
    def emplacements(f : Faction) : $[UserAction] = (UnitType.of(f).%(t => t.commander.not) ++ calledIn(f)).%(t => t.builtBy.any && t.tier <= f.tier && affordable(f, t.cost) && onBoard(f, t) < t.limit && buildersOf(f, t).any)./(t => EmplaceMenuAction(f, t))

    // the units that may build an Emplacement now, each with the adjacent hexes it may build on
    def buildersOf(f : Faction, t : UnitType) : $[(Squad, $[Hex])] =
        if (t.builtBy.none)
            builders(f)
        else
            units(f).filter(u => t.builtBy.contains(u.utype) && retreating.contains(u).not && spawnRound.get(u).has(round).not && built.getOrElse(u, 0) < wrenchesOf(u))
                .map(u => (u, Board.neighbors(where(u)).filter(h => Board.joined(where(u), h) && t.inBuildings.?(terrainAt(h) == Building).|(isOpen(h) && wire.contains(h).not) && standing(h).none))).filter(_._2.nonEmpty)

    // a unit has assigned dice: done, except a Heavy Vehicle in the Full Game Rules with damage types left to put on other targets
    def assigned(u : Squad, d : $[Attack]) {
        if (full && u.kind == HeavyVehicle) {
            used += u -> (used.getOrElse(u, Set[Dmg]()) ++ d./(_.dmg))
            // an attack in place of its own (Tulip, an ISU-152's HE) is its whole attack
            if (targetChoices(u).none && shotChoices(u).none || d.exists(a => attacksOf(u).forall(_.dmg != a.dmg)))
                decided += u
        }
        else
            decided += u
    }

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
        occupant(h).%(v => v.faction == f && carries(v) && t.kind == Infantry).foreach { v => carried += u -> v }
        pos += u -> h
        health += u -> maxHealth(u)
        // Conscript Support Tactics: new Conscripts come with both +1 Health upgrades
        if (t == Conscripts && unlocked(f, ConscriptSupport)) {
            bought += u -> $(Bought(1, HealthUp), Bought(1, HealthUp))
            health += u -> maxHealth(u)
        }
        spawnRound += u -> round
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

    // FULL GAME RULES

    // a unit's Firing Arc, and a Heavy Vehicle's frontal arc: its own hex, and the hexes within 60 degrees either side of the hex side it
    // faces (a hex on the edge of the cone is in it); a unit with no facing fires in any direction
    def inArc(u : Squad, h : Hex) : Boolean = facing.get(u) match {
        case Some(e) => h == where(u) || Board.inCone(where(u), h, e)
        case None => true
    }

    def facingNote(u : Squad) : Elem = facing.get(u)./(e => Facing.elem(e)).|(Empty)

    def heavyMoveNote(u : Squad, path : $[Hex], e : Int) : Elem =
        if (path == $(where(u)))
            "Turn to face".txt ~ " " ~ Facing.elem(e) ~ " " ~ costNote(u, 1)
        else
            "Move to".txt ~ " " ~ path.last.elem ~ " facing " ~ Facing.elem(e) ~ (path.num > 1).?((" (through " + path.head.name + ")").txt).|(Empty) ~ " " ~ costNote(u, path.num)

    def fixedGun(u : Squad, a : Attack) = u.utype.fixed.has(a.dmg)

    def hasSplash(u : Squad) = full && (u.utype.splash || has(u, SplashUp))

    // the dice a unit hasn't assigned yet this Damage Phase
    def remaining(u : Squad) : $[Attack] = attacksOf(u).%(a => used.getOrElse(u, Set[Dmg]()).contains(a.dmg).not)

    // HEAVY VEHICLE MOVEMENT: one CP moves it one hex forward or backward, turning one hex side before or after, or turns it in place;
    // through an occupied hex it goes straight on to the next for two CP. Each choice: the path (only its own hex for a turn in place),
    // the side it then faces, and whether it is an Assault
    def heavyStep(u : Squad, h : Hex, d : Int) : |[Hex] = Board.toward(h, d).%(n => Board.joined(h, n) && canAssault(u, n))

    def heavyOptions(u : Squad) : $[($[Hex], Int, Boolean)] = {
        val h = where(u)
        val e = facing.getOrElse(u, 0)
        val left = pointsLeft(u)
        var l = $[($[Hex], Int, Boolean)]()
        if (left >= 1) {
            l :+= (($(h), (e + 1) % 6, false))
            l :+= (($(h), (e + 5) % 6, false))
        }
        $(0, 1, 5).foreach { pre =>
            val f1 = (e + pre) % 6
            val posts = (pre == 0).?($(0, 1, 5)).|($(0))
            $(f1, (f1 + 3) % 6).foreach { d =>
                heavyStep(u, h, d).foreach { a =>
                    if (left >= 1 && occupant(a).none && standing(a).none && canEnter(u, a))
                        posts.foreach(p => l :+= (($(a), (f1 + p) % 6, false)))
                    if (left >= 1 && assaultable(u, a))
                        posts.foreach(p => l :+= (($(a), (f1 + p) % 6, true)))
                    if (left >= 2 && standing(a).any && canEnter(u, a))
                        heavyStep(u, a, d).%(b => b != h && standing(b).none && canEnter(u, b)).foreach(b => posts.foreach(p => l :+= (($(a, b), (f1 + p) % 6, false))))
                }
            }
        }
        l.distinct
    }

    // ASSAULTS: moving into an adjacent hex held by an enemy, where no other Assault is
    def assaultAt(h : Hex) = assaults.exists { case (_, d) => pos.get(d).has(h) }

    def assaultable(u : Squad, h : Hex) = full && isPassenger(u).not && mayMove(u) && pointsLeft(u) >= 1 && canAssault(u, h) && assaultAt(h).not && occupant(h).exists(v => foe(v.faction, u.faction))

    def assaultHexes(u : Squad) : $[Hex] = Board.neighbors(where(u)).%(n => Board.joined(where(u), n)).%(assaultable(u, _))

    def crush(u : Squad) {
        if (u.kind == HeavyVehicle && sandbags.contains(where(u))) {
            sandbags -= where(u)
            log(u, "crushed the Sandbags in", where(u))
        }
        if (u.kind == HeavyVehicle && wire.contains(where(u))) {
            wire -= where(u)
            log(u, "crushed the Barbed Wire in", where(u))
        }
    }

    // an Assault ends when the assaulted unit leaves its hex: the assaulting unit holds it
    def settleAssaults() {
        assaults.foreach { case (a, d) =>
            if (pos.get(d) != pos.get(a)) {
                assaults -= a
                if (pos.contains(a)) {
                    log(a, "holds", where(a))
                    crush(a)
                }
            }
        }
    }

    // RETREATS: a Retreat Point is a hex of the team's Spawn Area, or a hex next to an ally that is one (the Major), not a Building
    // teammates' Retreat Points count too (Commander FAQ 4: Retreat always benefits teammates), in games with the hidden TeamRetreatPoints
    def retreatPoints(f : Faction) : $[Hex] = (Board.spawn(team(f)) ++ options.has(TeamRetreatPoints).?(mates(f)).|($(f))./~(units).%(v => v.utype.retreatPoint && isPassenger(v).not && retreating.contains(v).not)./~(v => Board.neighbors(where(v)).%(n => Board.joined(where(v), n)))).distinct.%(h => isOpen(h))

    def onRetreatPoint(u : Squad) = retreatPoints(u.faction).has(where(u))

    def mayRetreat(u : Squad) = full && u.kind == Infantry && isPassenger(u).not && assaults.contains(u).not && retreating.contains(u).not

    // a Retreat passes through units and Battlefield Defenses, but ends in an empty hex it may enter
    def passable(u : Squad, h : Hex) = (terrainAt(h) != Water || u.utype == DevilsBrigade) && (u.utype.vehicle.not || inBuilding(h).not)

    def mayEnd(u : Squad, h : Hex) = canEnter(u, h) && standing(h).none

    def distancesTo(u : Squad, goals : $[Hex]) : Map[Hex, Int] = {
        var d = goals.%(passable(u, _))./(_ -> 0).toMap
        var front = d.keys.$
        var i = 0
        while (front.any) {
            i += 1
            front = front./~(h => Board.neighbors(h).%(n => Board.joined(h, n) && passable(u, n) && d.contains(n).not)).distinct
            front.foreach(n => d += n -> i)
        }
        d
    }

    // where a Retreat toward a point ends: up to six hexes along a shortest way, in the last empty hex of it
    def retreatEnd(u : Squad, point : Hex) : Hex = {
        val d = distancesTo(u, $(point))
        var cur = where(u)
        var path = $[Hex]()
        var going = d.contains(cur)
        while (going && path.num < 6 && cur != point) {
            Board.neighbors(cur).%(n => Board.joined(cur, n) && d.get(n).has(d(cur) - 1)).sortBy(h => (h.r, h.c)).starting match {
                case Some(n) =>
                    path :+= n
                    cur = n
                case None =>
                    going = false
            }
        }
        path.reverse.find(mayEnd(u, _)).|(where(u))
    }

    // the Retreats a unit may make: each Retreat Point, and where the Retreat toward it ends (one choice for each end)
    def retreatChoices(u : Squad) : $[(Hex, Hex)] = retreatPoints(u.faction).map(p => (p, retreatEnd(u, p))).filter(_._2 != where(u)).sortBy { case (p, e) => (Board.distance(e, p), e.r, e.c) }.distinctBy(_._2)

    def retreatTo(u : Squad, end : Hex) {
        retreating += u
        if (setUp.contains(u)) {
            setUp -= u
            facing -= u
        }
        pos += u -> end
        settleAssaults()
    }

    // a passenger whose vehicle stops transporting (an OKW truck converted, a Universal Carrier's flamethrower): to the nearest empty
    // hex toward a Retreat Point, or destroyed with nowhere to go
    def evacuate(u : Squad) {
        val h = where(u)
        carried -= u
        fallBack(u) match {
            case Some(x) =>
                pos += u -> x
                log(u, "left the vehicle in", h, "for", x)
            case None =>
                log(u, "had nowhere to go and was destroyed")
                killed(u, damagers(u))
                gain(u.faction, XP, 1)
                removeSquad(u)
        }
    }

    // a forced Retreat (a failed Assault, a destroyed Building): to the nearest empty hex, the one nearest a Retreat Point
    def fallBack(u : Squad) : |[Hex] = {
        val toPoint = distancesTo(u, retreatPoints(u.faction))
        var seen = Set(where(u))
        var front = $(where(u))
        var found = $[Hex]()
        while (front.any && found.none) {
            front = front./~(h => Board.neighbors(h).%(n => Board.joined(h, n) && passable(u, n) && seen.contains(n).not)).distinct
            seen ++= front
            found = front.%(mayEnd(u, _))
        }
        found.sortBy(h => (toPoint.getOrElse(h, 999), h.r, h.c)).starting
    }

    // OVERWATCH: an enemy Infantry moving onto a hex in range, sight and Firing Arc of a set-up machine gun takes 1 Damage from it (into a
    // Building, only from the machine gun it assaults there); a Camouflaged one only if spotted. Returns whether it survived
    def covers(m : Squad, h : Hex) = {
        val d = Board.distance(where(m), h)
        d <= rangeOf(m) && inArc(m, h) && (d == 0 || Board.clear(where(m), h))
    }

    def spottedIn(f : Faction, u : Squad, h : Hex) = camouflaged(u).not || mates(f)./~(units).exists(s => retreating.contains(s).not && (Board.distance(where(s), h) <= 1 || (spotsCamo(s) && Board.distance(where(s), h) <= sightOf(s) && Board.clear(where(s), h))))

    def overwatch(u : Squad, h : Hex, assaulted : |[Squad]) : Boolean = {
        if (full.not || u.kind != Infantry || retreating.contains(u) || pos.contains(u).not)
            return pos.contains(u)

        val mgs = enemies(u.faction)./~(units).%(m => m.utype.machineGun && setUp.contains(m) && isPassenger(m).not && retreating.contains(m).not)
        val by = inBuilding(h).?(mgs.%(m => assaulted.has(m))).|(mgs).%(m => covers(m, h) && spottedIn(m.faction, u, h))

        if (by.none)
            return true

        health += u -> (health(u) - by.num)
        log(u, "took", by.num.hl, "Overwatch damage in", h, "from", by./(_.elem).comma)
        conscripted(u, by.num)

        if (health(u) > 0)
            return true

        log(u, "was destroyed")
        killed(u, by./(_.faction))
        removeSquad(u)
        false
    }

    // HE SPLASH: on a hex in range and sight, hitting every unit in it and around it that the attacker's team sees (its own always)
    def area(h : Hex) : $[Hex] = h +: Board.neighbors(h)

    def seesHex(f : Faction, h : Hex) = mates(f).exists(eyed(_, h)) || mates(f)./~(units).exists(s => retreating.contains(s).not && Board.distance(where(s), h) <= sightOf(s) && lineClear(where(s), h) && (smoke.contains(where(s)).not || Board.distance(where(s), h) <= 1))

    def splashed(a : Squad, h : Hex) : $[Squad] = area(h)./~(standing).%(t => foe(t.faction, a.faction).not || visible(a.faction, t))

    def splashNote(u : Squad, h : Hex) : Elem = {
        val l = splashed(u, h)
        val own = l.%(v => foe(v.faction, u.faction).not).num
        ("(" + l.%(v => foe(v.faction, u.faction)).num + " enemy" + (own > 0).??(", " + own + " friendly") + ")").spn(styles.small)
    }

    def splashHexes(u : Squad) : $[Hex] = {
        val dice = remaining(u).%(_.dmg == HE)
        (hasSplash(u) && dice.any && canFire(u)).??(Board.hexes.%{ h =>
            val d = Board.distance(where(u), h)
            d >= max(1, u.utype.minRange) && d <= rangeOf(u) && seesHex(u.faction, h) &&
            dice.forall(a => (a.indirect || lineClear(where(u), h)) && (fixedGun(u, a).not || inArc(u, h))) && (smoke.contains(where(u)).not || d <= 1) &&
            (u.utype.setsUp.not || inArc(u, h)) &&
            splashed(u, h).exists(v => foe(v.faction, u.faction))
        })
    }

    // BUILDINGS: 2 health; units may fire HE and Flame at one with no visible enemy (nor ally) in it
    def shotChoices(u : Squad) : $[(Hex, $[Attack])] = (full && canFire(u) && hasSplash(u).not).??(Board.hexes.%(h => terrainAt(h) == Building || wire.contains(h) || traps.contains(h))./~{ h =>
        val d = Board.distance(where(u), h)
        // a Building takes HE and Flame; barbed wire and tank traps any damage assigned to them
        val dice = remaining(u).%(a => a.dmg == HE || a.dmg == Flame || terrainAt(h) != Building).%(a => a.indirect || lineClear(where(u), h)).%(a => fixedGun(u, a).not || inArc(u, h)).%(_ => smoke.contains(where(u)).not || d <= 1)
        val empty = standing(h).%(t => foe(t.faction, u.faction).not || visible(u.faction, t)).none
        (d >= max(1, u.utype.minRange) && d <= rangeOf(u) && seesHex(u.faction, h) && empty && dice.any && (u.utype.setsUp.not || inArc(u, h))).?((h, dice)).$
    })

    def diceOnFull(u : Squad, t : Squad) : $[Attack] = {
        if (pos.contains(t).not || isPassenger(t))
            return $

        val d = Board.distance(where(u), where(t))
        val pair = assaults.get(u).has(t) || assaults.get(t).has(u)

        // in an Assault both units are in one hex: only they attack each other there, and only units that fire at range 1
        if (d == 0 && (pair.not || u.utype.minRange > 1))
            return $

        if (d > 0 && (d < u.utype.minRange || d > rangeOf(u)))
            return $

        if (visible(u.faction, t).not)
            return $

        if (u.utype.setsUp && inArc(u, where(t)).not)
            return $

        // in Smoke a unit attacks, and is attacked, only from next to it
        if ((smoke.contains(where(u)) || smoke.contains(where(t))) && d > 1)
            return $

        val clear = d == 0 || lineClear(where(u), where(t))

        // Close Range Firepower only at range 1 (or in an Assault)
        val dice = (d > 1 && closeAttacks(u).any).?(remaining(u).diff(closeAttacks(u))).|(remaining(u))

        // the Bunker's AP die only against units outside it
        val inside = (d == 0 && Board.bunkers.has(where(u))).?(dice.diff($(UnitType.ap))).|(dice)

        inside.%(a => clear || a.indirect).%(a => fixedGun(u, a).not || inArc(u, where(t))).%(a => a.dmg != HE || hasSplash(u).not)
    }

    // the Ice checks: each unit attacked on an Ice tile rolls the most dice the Ice Matrix gives any damage type assigned to it
    def iceChecks : $[(Squad, Int)] = full.??(all.%(onIce).%(t => isPassenger(t).not)./{ t =>
        val types = assignments.%(_.target == t)./~(_.dice)./(_.dmg) ++
            splashes.%(s => area(s.hex).has(where(t)) && splashed(s.attacker, s.hex).has(t)).map(_ => HE : Dmg) ++
            strikes.%(_.hexes.has(where(t)))./~(_.dice)./(_.dmg)
        (t, types./(d => IceMatrix.dice(t.kind, d)).maxOr(0))
    }.filter(_._2 > 0))

    def value(x : $[Face]) = x./(f => f.crit.?(2).|(f.success.??(1))).sum

    // the HE dice rolled in the Full Game Rules, in this order: the splashes', the shots', then each target's
    def heNeeded : Int = splashes./(_.n).sum + strikes./(s => s.hexes.num * s.dice.count(_.dmg == HE)).sum + shots./(_.dice.count(_.dmg == HE)).sum +
        assignments./(_.target).distinct.%(pos.contains).%(t => sunk.contains(t).not)./(t => assignments.%(_.target == t)./~(_.dice).count(a => a.dmg == HE && Matrix.resist(t.kind, HE).any)).sum

    // the damage each unit takes (and each Building), from the HE dice rolled and the Defense Dice rolled (None to count the Defense Dice):
    // the Defense Matrix, Cover (not against an attack from the same hex, nor for the assaulting unit; non-Building Cover not against
    // splash, Building Cover not against the team's own splash), Frontal Armor (2 dice against AP from the front, not from the same hex),
    // and Defense upgrades; Flame can't be saved
    def resolveFull(he : $[Face], defense : |[$[Face]]) : ($[Hit], Map[Hex, Int]) = {
        var heLeft = he
        def take(n : Int) = {
            val r = heLeft.take(n)
            heLeft = heLeft.drop(n)
            r
        }

        var defLeft = defense.|($)
        var building = Map[Hex, Int]()

        def hurt(h : Hex, n : Int) {
            if (n > 0 && terrainAt(h) == Building)
                building += h -> (building.getOrElse(h, 0) + n)
        }

        val splashFaces = splashes./(s => (s, take(s.n)))
        // Commander strikes: each hex its own dice
        val strikeFaces = strikes./~(s => s.hexes./(h => (s, h, take(s.dice.count(_.dmg == HE)))))
        val shotFaces = shots./(s => (s, take(s.dice.count(_.dmg == HE))))

        splashFaces.foreach { case (s, x) => area(s.hex).foreach(h => hurt(h, value(x))) }
        shotFaces.foreach { case (s, x) => hurt(s.hex, value(x) + s.dice.count(_.dmg == Flame)) }
        strikeFaces.foreach { case (_, h, x) => hurt(h, value(x)) }

        val targets = (assignments./(_.target) ++ splashFaces./~{ case (s, _) => splashed(s.attacker, s.hex) } ++ strikeFaces./~{ case (_, h, _) => standing(h) }).distinct.%(pos.contains).%(t => sunk.contains(t).not)

        val hits = targets./{ t =>
            val h = where(t)
            val l = assignments.%(_.target == t)
            val dice = l./~(_.dice).%(a => Matrix.resist(t.kind, a.dmg).any)
            val direct = take(dice.count(_.dmg == HE))
            val splash = splashFaces.%{ case (s, _) => area(s.hex).has(h) && splashed(s.attacker, s.hex).has(t) }
            val struck = strikeFaces.%(_._2 == h)
            val strikeAP = struck./(_._1.dice.count(_.dmg == AP)).sum * Matrix.resist(t.kind, AP).any.??(1)
            val faces = direct ++ splash.flatMap(_._2) ++ struck.flatMap(_._3)
            val heDamage = Matrix.resist(t.kind, HE).any.??(value(faces))
            val plain = dice.count(a => a.dmg == AI || a.dmg == AP) + strikeAP
            val flame = dice.count(_.dmg == Flame)
            val resisted = dice.count(a => (a.dmg == AI || a.dmg == AP) && Matrix.resist(t.kind, a.dmg).has(true)) + Matrix.resist(t.kind, AP).has(true).??(strikeAP) + Matrix.resist(t.kind, HE).has(true).??(faces.count(_.success))
            val others = l./(a => where(a.attacker)).distinct.%(_ != h)
            val enemySplash = splash.exists(x => foe(x._1.attacker.faction, t.faction)) || struck.exists(x => foe(x._1.faction, t.faction))
            // IS-2 122mm Shells: Infantry in a Building it fires at get 1 less Defense Die
            val shells = l.%(a => a.attacker.utype == IS2 && unlocked(a.attacker.faction, IS2Shells))
            val base0 =
                if (t.kind != Infantry || assaults.contains(t)) 0
                else if (inBuilding(h)) (others.%(x => sameBuilding(x, h).not).any || enemySplash).??(staticCover(t, $))
                else staticCover(t, others)
            val base = (shells.any && inBuilding(h)).?(max(0, base0 - 1)).|(base0)
            // Commanders: Defensive Stance is Heavy Cover even in an Assault; Armor Tactician 2 dice next to a Heavy Vehicle (not in a
            // Building, no stacking with other Cover)
            val tactician = t.kind == Infantry && inBuilding(h).not && (unlocked(t.faction, ArmorTactician) || unlocked(t.faction, TankDesant) || unlocked(t.faction, InfantryTankSupport)) && units(t.faction).exists(v => v.kind == HeavyVehicle && isPassenger(v).not && Board.adjacent(where(v), h))
            val cover = (l.any && (stance.contains(t) || tactician || holdsLine(t))).?(max(base, 2)).|(base)
            val frontal = (t.kind == HeavyVehicle && l.exists(a => where(a.attacker) != h && a.dice.exists(_.dmg == AP) && inArc(t, where(a.attacker)))).??(2)
            val hull = (hullDown.contains(t) && l.any).??(2)
            // Breakthrough Doctrine: Infantry with Heroic Sacrifice +1 when Assaulting; Advanced Emplacements: +1 for a braced Emplacement
            val assaulting = (assaults.contains(t) && has(t, HeroicSacrifice) && (unlocked(t.faction, WillNotDieOld) || unlocked(t.faction, SecondWave))).??(1) + (assaults.contains(t) && t.utype == Valentine).??(1)
            // Tank Destroyer Doctrine: a marked vehicle has no Defense Dice this round
            // Soviet Architecture: Infantry +1 in a Building
            val architecture = (t.kind == Infantry && inBuilding(h) && unlocked(t.faction, SovietArchitecture)).??(1)
            val n = marked.get(t).has(round).?(0).|(resisted + cover + frontal + hull + defenseOf(t) + assaulting + braced.contains(t).??(1) + architecture + oberBraced.getOrElse(t, 0))
            val rolled = defLeft.take(n)
            defLeft = defLeft.drop(n)
            val saved = rolled.count(_.success)
            if (t.kind == Infantry)
                hurt(h, value(direct) + flame + inBuilding(h).??(shells./(_.dice.count(_.dmg == AP)).sum))
            Hit(t, faces, n, max(0, plain + heDamage - saved) + flame)
        }

        (hits, building)
    }

    // COMMANDERS

    def unlocked(f : Faction, a : Ability) = (commanders && commanderOf.get(f).exists(c => c.tiers.take(tiersOf.getOrElse(f, 0)).exists(_.ability == a))) || (brigades && brigadeOf.get(f).exists(_.abilities.has(a)))

    // a unit a Lost Brigade brings (OKW: spawned from the Spawn Area only, not next to a Tier Building)
    def brigadeUnit(f : Faction, t : UnitType) = brigades && brigadeOf.get(f).exists(_.unit.has(t))

    def riflemen(u : Squad) = u.utype == Riflemen || u.utype == AssaultEngineers || u.utype == DevilsBrigade

    // Ranger Endurance Training: Riflemen take 4 CP a round
    def rangers(u : Squad) = riflemen(u) && unlocked(u.faction, RangerTraining)

    def slowOf(u : Squad) = (u.utype == Brummbar && unlocked(u.faction, EliteSturmpanzer)).?(0).|((has(u, RemoveSlow) || (u.utype == Churchill && unlocked(u.faction, OperationCrocodile))).?(0).|(u.utype.slow))

    // M1919A6 LMG and LMG-42: the Riflemen's AI Munitions upgrade, the Grenadiers' HE one, also give +1 Range
    def lmgRange(u : Squad) =
        (riflemen(u) && unlocked(u.faction, LMGRiflemen) && bought.getOrElse(u, $[Bought]()).exists(b => b.slot == 1 && b.effect == AddAttack(UnitType.ai))).??(1) +
        boysAT(u).??(1) +
        (u.utype == Grenadiers && (unlocked(u.faction, LMG42) || unlocked(u.faction, LMG42Free)) && bought.getOrElse(u, $[Bought]()).exists(b => b.slot == 1 && b.effect == AddAttack(UnitType.heIndirect))).??(1)

    // Boys AT Rifles: Infantry Sections with their Munitions AP upgrade also +1 Sight and Range
    def boysAT(u : Squad) = u.utype == InfantrySection && unlocked(u.faction, BoysATRifles) && bought.getOrElse(u, $[Bought]()).exists(_.slot == 1)

    // the upgrades a Commander gives units
    def freeEffects(u : Squad) : $[Effect] = abilities.??(u.utype @@ {
        case Major if unlocked(u.faction, WestPointReserves) => $(SpawnAdjacent, SpotsCamo)
        case Greyhound if unlocked(u.faction, VeteranGreyhounds) => $(DefenseUp)
        case Wolverine if unlocked(u.faction, VeteranWolverines) => $(AddAttack(UnitType.ap), SpeedUp)
        case ATGun if unlocked(u.faction, EliteATCrew) => $(HealthUp, SpeedUp)
        case Ostwind if unlocked(u.faction, VeteranOstwinds) => $(AddAttack(UnitType.ai), SpotsCamo)
        case SdKfz222 if unlocked(u.faction, MG42ArmoredCars) => $(AddAttack(UnitType.ai))
        case PanzerGrenadiers if unlocked(u.faction, VeteranPanzerGrenadiers) => $(DefenseUp, AddAttack(UnitType.ap), AddAttack(UnitType.ap))
        case InfantrySection | Vickers if unlocked(u.faction, BolsterInfantry) && boughtEffects(u).has(HealthUp) => $(DefenseUp)
        case Staghound if unlocked(u.faction, VeteranStaghounds) => $(AddAttack(UnitType.ap))
        case Cromwell if unlocked(u.faction, VeteranCromwells) => $(SpeedUp, AddAttack(UnitType.ap), AddAttack(UnitType.ap))
        case T34 if unlocked(u.faction, T34Wave) => $(AddAttack(UnitType.ai), SpeedUp)
        case ZiS3 if unlocked(u.faction, ATAmbushTactics) => $(Camouflage)
        case SU76 if unlocked(u.faction, VeteranSU76) => $(SpeedUp, HealthUp)
        case SU85 if unlocked(u.faction, VeteranSU85) => $(AddAttack(UnitType.ap), SightUp, RemoveSlow)
        case T70 => unlocked(u.faction, T70InfantryTank).$(DefenseUp) ++ unlocked(u.faction, T80Turret).??($(HealthUp, AddAttack(UnitType.ai)))
        case Luchs => unlocked(u.faction, HeavyLuchs).$(DefenseUp) ++ unlocked(u.faction, ScoutLuchs).$(SpeedUp)
        case StukaHalftrack if unlocked(u.faction, RocketSupport) => $(RangeUp, SplashUp)
        case LeFH18 if unlocked(u.faction, VeteranLeFH) => $(AddAttack(UnitType.heIndirect))
        case Stuart if unlocked(u.faction, BrowningM2) => $(AddAttack(UnitType.ai), RangeUp)
        case Sherman if unlocked(u.faction, Sherman76) => $(SpeedUp, AddAttack(UnitType.ap))
        case Jeep if unlocked(u.faction, SupportJeeps) => $(ReinforceUp)
        case USMortar if unlocked(u.faction, Mortars81mm) && boughtEffects(u).has(SplashUp) => $(AddAttack(UnitType.heIndirect))
        case Pathfinder if unlocked(u.faction, AirdropBeacon) => $(SpawnAdjacent)
        case DevilsBrigade if unlocked(u.faction, VeteranDevils) => $(AddAttack(UnitType.ap), AddAttack(UnitType.ap))
        case PenalBattalion | ShockTroops if unlocked(u.faction, BattlefieldPromotions) => $(SpeedUp)
        case Kettenkrad if unlocked(u.faction, VeteranKettenkrad) => $(HealthUp, Camouflage)
        case Volksgrenadiers if unlocked(u.faction, EncirclementVolks) => $(HealthUp, HealthUp)
        case Centaur if unlocked(u.faction, VeteranCentaurs) => $(AddAttacks(UnitType.ai), SpotsCamo)
        case Commandos if unlocked(u.faction, GammonBombs) => $(Camouflage)
        case StuG => unlocked(u.faction, TargetWeakPoint).$(AddAttack(UnitType.ap)) ++ unlocked(u.faction, TurretlessTerror).$(Camouflage)
        case _ => $[Effect]()
    })

    // the upgrade slots a Commander adds (numbered from 10)
    def commanderSlots(u : Squad) : $[(Int, Slot)] = abilities.??(
        (unlocked(u.faction, AmbushTraining) && u.kind == Infantry).$((10, Slot(false, 3, $(Camouflage)))) ++
        (unlocked(u.faction, ReconInForce) && u.kind == LightVehicle).$((11, Slot(false, 2, $(SpotsCamo)))) ++
        (unlocked(u.faction, MechanizedInfantry) && (u.utype == Grenadiers || u.utype == PanzerGrenadiers)).$((12, Slot(false, 2, $(SpeedUp)))) ++
        (unlocked(u.faction, G43Rifles) && (u.utype == Grenadiers || u.utype == Stormtroopers || u.utype == PanzerGrenadiers)).$((13, Slot(false, 2, $(AddAttack(UnitType.ai))))) ++
        (unlocked(u.faction, CovertTraining) && u.kind == Infantry).$((14, Slot(false, 3, $(Camouflage)))) ++
        (unlocked(u.faction, ThompsonSMGs) && (u.utype == InfantrySection || u.utype == RoyalEngineers)).$((15, Slot(false, 4, $(CloseFire)))) ++
        (unlocked(u.faction, InfantrySupport) && u.utype == UniversalCarrier).$((16, Slot(false, 2, $(ReinforceUp)))) ++
        ((unlocked(u.faction, WillNotDieOld) || unlocked(u.faction, SecondWave)) && u.kind == Infantry).$((17, Slot(false, 3, $(HeroicSacrifice)))) ++
        (unlocked(u.faction, Flammenwerfer) && u.utype == Sturmpioneers).$((18, Slot(false, 4, $(AddAttack(UnitType.flame))))) ++
        (unlocked(u.faction, HEATShells) && (u.utype == Puma || u.utype == Panther)).$((19, Slot(false, 3, $(AddAttack(UnitType.ap))))) ++
        (unlocked(u.faction, LightInfantryTraining) && riflemen(u)).$((20, Slot(false, 2, $(SpeedUp)))) ++
        (unlocked(u.faction, DevilsAmbush) && u.utype == DevilsBrigade).$((21, Slot(false, 4, $(Camouflage, CloseFire)))) ++
        (unlocked(u.faction, FlameProjectors) && u.utype == Halftrack).$((22, Slot(false, 4, $(AddAttacks(UnitType.flame))))) ++
        (unlocked(u.faction, Pak251) && u.utype == Halftrack).$((23, Slot(false, 4, $(AddAttacks(UnitType.ap))))) ++
        (unlocked(u.faction, StuGCloseRange) && u.kind == Infantry && u.utype.weaponTeam.not).$((24, Slot(false, 2, $(CloseFire)))) ++
        (unlocked(u.faction, BattlefieldPromotions) && (u.utype == PenalBattalion || u.utype == ShockTroops)).$((25, Slot(true, 2, $(AddAttack(UnitType.flame))))) ++
        (unlocked(u.faction, UrbanSharpshooters) && (u.utype == GuardsRifle || u.utype == SniperSquad)).$((26, Slot(false, 2, $(DefenseUp)))) ++
        (unlocked(u.faction, ISUFactoryMods) && u.utype == ISU152).$((27, Slot(false, 3, $(RemoveSlow, DefenseUp)))) ++
        (unlocked(u.faction, HalftrackConfigurable) && u.utype == Halftrack).$((28, Slot(false, 4, $(AddAttacks(UnitType.flame), AddAttacks(UnitType.ap)))))
    )

    def slotAt(u : Squad, i : Int) : Slot = (i < 10).?(u.utype.upgrades(i)).|(commanderSlots(u).find(_._1 == i).get._2)

    def slotsOf(u : Squad) : $[(Int, Slot)] = u.utype.upgrades.zipWithIndex./{ case (s, i) => (i, s) } ++ commanderSlots(u)

    // what an upgrade costs: Ranger and .50 cal Jeep Munitions upgrades 1 less, the Wehrmacht Mortar's Flame 1 XP with Incendiary Mortars
    def slotPrice(u : Squad, i : Int) : Int = {
        val s = slotAt(u, i)
        val f = u.faction
        if (i == 1 && riflemen(u) && unlocked(f, RangerTraining)) max(0, s.cost - 1)
        else if (i == 1 && u.utype == Jeep && unlocked(f, JeepMG)) max(0, s.cost - 1)
        else if (i == 0 && u.utype == WMortar && unlocked(f, IncendiaryMortars)) 1
        else if (i == 1 && u.utype == SniperSquad && unlocked(f, RangedTactics)) max(0, s.cost - 2)
        else if (i == 1 && u.utype == SovietMortar && unlocked(f, VeteranMortars)) max(0, s.cost - 2)
        else if (i == 1 && u.utype == UniversalCarrier && unlocked(f, WASPFlamethrower)) max(0, s.cost - 1)
        else if (i == 1 && u.utype == InfantrySection && unlocked(f, TankHunters)) max(0, s.cost - 2)
        else s.cost
    }

    // what a unit costs: US Breakthrough takes 1 Manpower off Infantry this Supply Phase
    def costOf(f : Faction, t : UnitType) : Cost = {
        // a Lost Brigade's unit at the card's price
        val b = brigades.??(brigadeOf.get(f).%(_.unit.has(t))./~(_.price))
        val base = b.|(t.cost)
        val c = (breakthrough.contains(f) && t.kind == Infantry).?(Cost(max(0, base.mp - 1), base.mun, base.fuel)).|(base)
        if (t.vehicle && sovietIndustry.contains(f)) Cost(c.mp, c.mun, max(0, c.fuel - 1))
        else if ((t.chassis == Sherman || t.chassis == Wolverine) && unlocked(f, RequisitionOfficer)) Cost(max(0, c.mp - 1), c.mun, c.fuel)
        else if (t == Panther && unlocked(f, PantherReinforcements)) Cost(max(0, c.mp - 1), c.mun, max(0, c.fuel - 1))
        else if (t == Sturmtiger && unlocked(f, SturmtigerReinforcements)) Cost(c.mp, c.mun, max(0, c.fuel - 2))
        else c
    }

    def tierCost(f : Faction, tier : Int) : Cost = {
        val c = Tiers.cost(tier)
        val d = (tier == 3 && unlocked(f, GermanIndustry)).?(Cost(c.mp, c.mun, max(0, c.fuel - 2))).|(c)
        (tier == 3 && unlocked(f, TerrorWestOber)).?(Cost(max(0, d.mp - 1), d.mun, d.fuel)).|(d)
    }

    // the Commander units a faction may buy
    def calledIn(f : Faction) : $[UnitType] =
        unlocked(f, PriestCallIn).$(Priest) ++ unlocked(f, EasyEightCallIn).$(EasyEight) ++ unlocked(f, AssaultEngineerSpawn).$(AssaultEngineers) ++
        unlocked(f, TigerCallIn).$(Tiger) ++ unlocked(f, OsttruppenSquads).$(Osttruppen) ++
        (unlocked(f, StormtrooperSpawn) && usedIn.get((f, StormtrooperSpawn)).has(round).not).$(Stormtroopers) ++
        unlocked(f, RoyalEngineerSpawn).$(RoyalEngineers) ++ unlocked(f, ChurchillAVRECallIn).$(ChurchillAVRE) ++
        (unlocked(f, StaghoundCallIn) || unlocked(f, VeteranStaghounds)).$(Staghound) ++ unlocked(f, APSniperCallIn).$(APSniper) ++
        unlocked(f, DShKCallIn).$(DShK) ++ unlocked(f, T3485CallIn).$(T3485) ++ unlocked(f, PanzerfusilierCallIn).$(Panzerfusilier) ++
        unlocked(f, JaegerCallIn).$(JaegerRecon) ++ unlocked(f, PumaCallIn).$(Puma) ++
        (unlocked(f, KettenkradCallIn) || unlocked(f, KettenkradEngineers)).$(Kettenkrad) ++ unlocked(f, StG44Volks).$(StG44Volksgrenadiers) ++
        // the Commander Emplacements, built by Infantry
        (unlocked(f, ForwardEmplacements) || unlocked(f, CommandoGlider)).$(ForwardEmplacement) ++ unlocked(f, SeventeenPounderAbility).$(SeventeenPounder) ++
        unlocked(f, ForwardHQAbility).$(ForwardHQ) ++ (unlocked(f, LeFH18Ability) || unlocked(f, VeteranLeFH)).$(LeFH18) ++ unlocked(f, VeteranPak43).$(OKWPak43) ++
        // the US expansion Commanders' units
        (unlocked(f, StuartCallIn) || unlocked(f, StuartCallInFree) || unlocked(f, BrowningM2)).$(Stuart) ++
        (unlocked(f, PershingCallIn) || unlocked(f, PershingCallInPlain) || unlocked(f, Outmaneuver)).$(Pershing) ++
        (unlocked(f, CalliopeCallIn) || unlocked(f, CalliopeCallInPlain)).$(Calliope) ++
        (unlocked(f, DozerCallIn) || unlocked(f, DozerCallInPlain)).$(Dozer) ++ (unlocked(f, JacksonCallIn) || unlocked(f, JacksonCallInPlain)).$(Jackson) ++
        unlocked(f, PathfinderSquads).$(Pathfinder) ++ unlocked(f, CavalryRiflemenCallIn).$(CavalryRiflemen) ++ unlocked(f, DevilsBrigadeSpawn).$(DevilsBrigade) ++
        unlocked(f, EasyEightCallInPlain).$(EasyEight) ++ unlocked(f, AssaultEngineerSpawnPlain).$(AssaultEngineers) ++ unlocked(f, USMGBunkers).$(USMGBunker) ++
        // the Wehrmacht expansion Commanders' units
        (unlocked(f, StuGCallIn) || unlocked(f, StuGCallInFree) || unlocked(f, TargetWeakPoint)).$(StuG) ++ unlocked(f, KingTigerCallIn).$(KingTiger) ++
        (unlocked(f, JaegerSniperFree) || unlocked(f, JaegerSniperCallIn)).$(JaegerSniper) ++ unlocked(f, JagdpantherCallIn).$(Jagdpanther) ++
        unlocked(f, ElefantCallIn).$(Elefant) ++ (unlocked(f, HalftrackCallIn) || unlocked(f, HalftrackCallInFree)).$(Halftrack) ++
        (unlocked(f, OsttruppenSquadsPlain) && unlocked(f, OsttruppenSquads).not).$(Osttruppen) ++
        // the British expansion Commanders' units
        (unlocked(f, SextonCallIn) || unlocked(f, SextonCallInFree)).$(Sexton) ++ unlocked(f, CometCallIn).$(Comet) ++
        (unlocked(f, FireflyCallIn) || unlocked(f, FireflyCallInFree)).$(Firefly) ++ (unlocked(f, ValentineCallIn) || unlocked(f, ValentineCallInFree)).$(Valentine) ++
        (unlocked(f, StaghoundCallInPlain) && unlocked(f, StaghoundCallIn).not && unlocked(f, VeteranStaghounds).not).$(Staghound) ++
        (unlocked(f, ChurchillAVRECallInFree) && unlocked(f, ChurchillAVRECallIn).not).$(ChurchillAVRE) ++
        (unlocked(f, APSniperCallInPlain) && unlocked(f, APSniperCallIn).not).$(APSniper) ++
        (unlocked(f, RoyalEngineerSpawnPlain) && unlocked(f, RoyalEngineerSpawn).not).$(RoyalEngineers) ++
        // the Soviet expansion Commanders' units
        unlocked(f, ShockTroopsCallIn).$(ShockTroops) ++ (unlocked(f, IS2CallIn) || unlocked(f, IS2Shells)).$(IS2) ++
        (unlocked(f, GuardsRifleFree) || unlocked(f, GuardsRifleCallIn)).$(GuardsRifle) ++ unlocked(f, ForwardStationAbility).$(ForwardStation) ++
        unlocked(f, KV2CallIn).$(KV2) ++ (unlocked(f, PenalBattalionFree) || unlocked(f, PenalBattalionCallIn)).$(PenalBattalion) ++
        (unlocked(f, ISU152CallIn) || unlocked(f, ISU152CallInFree) || unlocked(f, ISUFactoryMods)).$(ISU152) ++
        (unlocked(f, KV8CallIn) || unlocked(f, KV8CallInFree)).$(KV8) ++ (unlocked(f, T3485CallInPlain) && unlocked(f, T3485CallIn).not).$(T3485) ++
        (unlocked(f, DShKCallInPlain) && unlocked(f, DShKCallIn).not).$(DShK) ++
        // the OKW expansion Commanders' units
        (unlocked(f, HetzerCallIn) || unlocked(f, HetzerCallInPlain) || unlocked(f, OKWBreakthrough)).$(Hetzer) ++
        (unlocked(f, PanzerfusilierCallInPlain) && unlocked(f, PanzerfusilierCallIn).not).$(Panzerfusilier) ++
        (unlocked(f, JaegerCallInPlain) && unlocked(f, JaegerCallIn).not).$(JaegerRecon) ++
        (unlocked(f, OKWPak43Ability) && unlocked(f, VeteranPak43).not).$(OKWPak43) ++
        (unlocked(f, VeteranKettenkrad) && unlocked(f, KettenkradCallIn).not && unlocked(f, KettenkradEngineers).not).$(Kettenkrad) ++
        // the Lost Brigades' own units
        unlocked(f, EliteGrenadierLeaders).$(EliteGrenadiers) ++ unlocked(f, CrocodileCallIn).$(Churchill) ++
        unlocked(f, CommandPantherCallIn).??(UnitType.all.%(t => t.faction == f.base && t.isInstanceOf[CommandPanther]))

    // the Lost Brigade a player may pick now (once, for its XP)
    def brigadePicks(f : Faction) : $[UserAction] = (brigades && brigadeOf.contains(f).not).??(brigadesDrawn.getOrElse(f, $[Brigade]()).%(b => f.stock(XP) >= b.xp)./(b => BrigadePickAction(f, b)))

    // the starting Infantry a 0 XP Lost Brigade picked at the start gives, up to its number
    def brigadeStart(f : Faction) : $[UnitType] = brigades.??(brigadeOf.get(f).$./~(b => Brigade.startUnit(b).%(t => units(f).count(_.utype == t) < b.starting).$))

    // Commandos: 5 Manpower 1 Munitions, 2 Munitions for the Lost Brigade's Veteran Commandos
    def commandosCost(f : Faction) = unlocked(f, VeteranCommandos).?(Cost(5, 2, 0)).|(Cost(5, 1, 0))

    def onceLeft(f : Faction, a : Ability) = unlocked(f, a) && usedIn.get((f, a)).has(round).not

    // the T-70's No-Fire mode, and the Scout Car's with Assault Tactics
    def noFireMode(u : Squad) = u.utype.captureMode || (u.utype == ScoutCar && unlocked(u.faction, SupplyOperations)) || (u.utype == Stuart && unlocked(u.faction, StrategicManeuvers)) || (u.utype == Kubelwagen && unlocked(u.faction, AssaultSupport))

    // what a unit pays to Smoke its hex at the start of the Damage Phase, if it may: Vehicles 2 Munitions (Defensive Smoke, Smoke
    // Canisters, Smoke Ambush) or 3 with Vehicle Crew Repairs, Emplacements 1 (Smoke Emplacement)
    def smokeCost(v : Squad) : |[Int] = {
        val f = v.faction
        if (urbanSmoke(v)) |(1)
        else if (v.utype.vehicle && unlocked(f, VehicleCrewRepairs)) |(3)
        else if (v.utype.vehicle && (unlocked(f, DefensiveSmoke) || unlocked(f, SmokeCanisters) || unlocked(f, SmokeAmbush) || unlocked(f, SmokeCanistersStuka))) |(2)
        else if (v.kind == Emplacement && unlocked(f, SmokeEmplacement)) |(1)
        else None
    }

    // a destroyed Building: the units in it fall back out of it
    def collapse(x : Hex) {
        standing(x).foreach { u =>
            fallBack(u).foreach { y =>
                pos += u -> y
                passenger(u).foreach(p => pos += p -> y)
                if (u.kind == Infantry) {
                    retreating += u
                    retreatEnds += u
                    if (setUp.contains(u)) {
                        setUp -= u
                        facing -= u
                    }
                }
                log(u, "retreated out of the destroyed Building to", y)
            }
        }
    }

    // Demolition: each Devil's Brigade not yet used this Damage Phase, with a standing Building next to it
    var demolished = Set[Squad]()

    def demolitions(f : Faction) : $[(Squad, Hex)] = (full && unlocked(f, Demolition)).??(units(f).%(u => u.utype == DevilsBrigade && isPassenger(u).not && demolished.contains(u).not)./~(u =>
        Board.neighbors(where(u)).%(h => terrainAt(h) == Building)./(h => (u, h))))

    // Urban Assault: assaulting Riflemen may Smoke the hex they assault
    def urbanSmoke(v : Squad) = ((riflemen(v) && unlocked(v.faction, UrbanAssault)) || (v.utype == ShockTroops && unlocked(v.faction, ShockTroopsCallIn))) && assaults.get(v).exists(pos.contains)

    // the hex a unit's Smoke goes on
    def smokeHex(v : Squad) = urbanSmoke(v).?(where(assaults(v))).|(where(v))

    // Rapid Conscription: each Damage to the faction's Infantry this round gives it 1 Manpower
    def conscripted(u : Squad, n : Int) {
        if (n > 0 && u.kind == Infantry && conscription.get(u.faction).has(round)) {
            gain(u.faction, Manpower, n)
            log(u.faction, "gained", n.hl, Manpower.icon, "(Rapid Conscription)")
        }
    }

    // Hold the Line: Infantry within 1 hex of a Control Point its team holds
    def holdsLine(t : Squad) = t.kind == Infantry && unlocked(t.faction, HoldTheLine) && inBuilding(where(t)).not && pointsOf(t.faction).exists(p => Board.distance(p, where(t)) <= 1)

    // Infiltration Commandos: next to an empty Building (not a Destroyed one)
    def infiltrationHexes(f : Faction) : $[Hex] = Board.hexes.%(h => terrainAt(h) == Building && standing(h).none)./~(b => Board.neighbors(b).%(n => Board.joined(b, n)))
        .distinct.%(h => terrainAt(h) != Water && standing(h).none).sortBy(h => (h.r, h.c))

    // the British, Soviet and OKW special spawns and the Scavenge Doctrine's trades
    def commanderSpawns(f : Faction) : $[UserAction] = abilities.??(
        (unlocked(f, SupplyRecon) && f.stock(Munitions) >= 3 && onBoard(f, ScoutCar) < ScoutCar.limit && spawnHexes(f, ScoutCar).any).$(DropMenuAction(f, SupplyRecon)) ++
        (unlocked(f, NKVDConscripts) && f.stock(Munitions) >= 5 && spawnHexes(f, Conscripts).any).$(DropMenuAction(f, NKVDConscripts)) ++
        (onceLeft(f, InfiltrationCommandos) && affordable(f, Cost(5, 1, 0)) && infiltrationHexes(f).any).$(DropMenuAction(f, InfiltrationCommandos)) ++
        (onceLeft(f, CommandoGlider) && affordable(f, Cost(5, 3, 0)) && dropHexes(f).exists(h => Board.neighbors(h).exists(n => isOpen(n) && standing(n).none && n != h))).$(DropMenuAction(f, CommandoGlider)) ++
        (unlocked(f, ThoroughSalvage) && f.stock(Manpower) >= 2).??($(SalvageAction(f, Fuel), SalvageAction(f, Munitions))) ++
        (unlocked(f, MotorPool) && f.stock(Munitions) >= 5 && onBoard(f, Jeep) < Jeep.limit && spawnHexes(f, Jeep).any).$(DropMenuAction(f, MotorPool)) ++
        (onceLeft(f, AirdroppedCombatGroup) && affordable(f, Cost(5, 6, 0)) && beaconHexes(f).any).$(DropMenuAction(f, AirdroppedCombatGroup)) ++
        (unlocked(f, UniversalCarriers) && f.stock(Munitions) >= 5 && onBoard(f, UniversalCarrier) < UniversalCarrier.limit && spawnHexes(f, UniversalCarrier).any).$(DropMenuAction(f, UniversalCarriers)) ++
        (onceLeft(f, CommandosSpawn) && affordable(f, commandosCost(f)) && dropHexes(f).any).$(DropMenuAction(f, CommandosSpawn)) ++
        (unlocked(f, EnglishIndustry) && f.stock(Munitions) >= 2).$(EnglishIndustryAction(f)) ++
        ((unlocked(f, SovietIndustryScout) || unlocked(f, SovietIndustryT70)) && sovietIndustry.contains(f).not && f.stock(Manpower) >= 1).$(SovietIndustryAction(f)) ++
        $(AIPartisansSpawn, ATPartisansSpawn, ATPartisansSpawnFree).%(a => onceLeft(f, a) && affordable(f, Cost(1, 4, 0)) && stormHexes(f).any)./(a => DropMenuAction(f, a)) ++
        units(f).%(u => u.utype == Conscripts && unlocked(f, BattlefieldPromotions) && f.stock(Munitions) >= 4 && isPassenger(u).not)./~(u => $(PromoteAction(f, u, ShockTroops), PromoteAction(f, u, PenalBattalion))) ++
        (unlocked(f, SupplyConvoy) && f.stock(Munitions) >= 3 && onBoard(f, Kubelwagen) < Kubelwagen.limit && spawnHexes(f, Kubelwagen).any).$(DropMenuAction(f, SupplyConvoy))
    )

    // Airdropped Combat Group: the empty hexes next to a Pathfinder Squad
    def beaconHexes(f : Faction) : $[Hex] = units(f).%(u => u.utype == Pathfinder && isPassenger(u).not)./~(u => Board.neighbors(where(u)).%(n => Board.joined(where(u), n) && terrainAt(n) != Water && standing(n).none)).distinct.sortBy(h => (h.r, h.c))

    def beaconTypes(f : Faction) : $[UnitType] = UnitType.of(f).%(t => t.kind == Infantry && t.tier <= f.tier && t.commander.not && t != Pathfinder)

    // where units spawn off the Spawn Area: a visible non-Building empty hex (Paratroopers, Paradrops, Airdropped Groups), next to an
    // empty Building (Stormtroopers)
    def dropHexes(f : Faction) : $[Hex] = Board.hexes.%(h => isOpen(h) && standing(h).none && seesHex(f, h))

    def stormHexes(f : Faction) : $[Hex] = Board.hexes.%(h => (terrainAt(h) == Building || terrainAt(h) == Ruin) && standing(h).none)./~(b => Board.neighbors(b).%(n => Board.joined(b, n)))
        .distinct.%(h => terrainAt(h) != Water && standing(h).none).sortBy(h => (h.r, h.c))

    def weaponTeams(f : Faction) : $[UnitType] = UnitType.of(f).%(t => t.weaponTeam && t.commander.not && t.tier <= f.tier)

    // the actions a Commander adds to a unit in the Maneuver Phase: Defensive Stance, Hull Down, Raid Point
    def commanderActions(f : Faction, u : Squad, k : Int) : $[UserAction] = abilities.??(
        ((riflemen(u) && unlocked(f, DefensiveStance)) || (u.utype == Conscripts && unlocked(f, HitTheDirt))).&&(onBuilding(u).not && paidLeft(u) >= 1 && isPassenger(u).not).$(DefensiveStanceAction(f, u, k)) ++
        // Armored Assault Tactics' Recon Mode, Emergency War Speed: a free move for a vehicle after its first CP
        (u.utype.vehicle && unlocked(f, ReconMode) && cp.getOrElse(u, 0) == 1 && raided.contains(u).not && f.stock(Munitions) >= 2).$(ReconModeAction(f, u, k)) ++
        (u.utype.vehicle && unlocked(f, EmergencyWarSpeed) && cp.getOrElse(u, 0) == 1 && raided.contains(u).not && health(u) > 1).$(WarSpeedAction(f, u, k)) ++
        ((u.utype == Grenadiers || u.utype == Osttruppen || u.utype == PanzerGrenadiers) && unlocked(f, HullDown) && pointsLeft(u) >= 1 && isPassenger(u).not).??(
            Board.neighbors(where(u))./~(occupant).%(v => v.faction == f && v.utype.vehicle && hullDown.contains(v).not)./(v => HullDownAction(f, u, v, k))) ++
        (u.kind == Infantry && (unlocked(f, RaidPoint) || unlocked(f, RallyPoint)) && cp.getOrElse(u, 0) == 1 && raided.contains(u).not && f.stock(Munitions) >= 1).$(RaidPointAction(f, u, k)) ++
        ((u.utype == SniperSquad || u.utype == GuardsRifle) && unlocked(f, Decoy) && cp.getOrElse(u, 0) == 2 && paidLeft(u) >= 1 && decoyed.contains(u).not && mayMove(u) && isPassenger(u).not).$(DecoyAction(f, u, k)) ++
        (u.utype.vehicle && (unlocked(f, StepOnIt) || unlocked(f, WMBreakthrough) || unlocked(f, OKWBreakthrough)) && cp.getOrElse(u, 0) == 1 && raided.contains(u).not && f.stock(Munitions) >= 2).$(StepOnItAction(f, u, k)) ++
        // Cavalry Riflemen: after a move, a free Assault on an adjacent hex, once a round
        (u.utype == CavalryRiflemen && unlocked(f, CavalryRiflemenCallIn) && full && cp.getOrElse(u, 0) >= 1 && charged.contains(u).not && isPassenger(u).not && mayMove(u)).??(
            Board.neighbors(where(u)).%(h => Board.joined(where(u), h) && canEnter(u, h) && assaultAt(h).not && occupant(h).exists(v => foe(v.faction, f)))./(h => CavalryChargeAction(f, u, h, k)))
    )

    // the opening abilities: at the start of the faction's Maneuver Phase (its first turn, before any CP is spent)
    def openings(f : Faction, k : Int) : $[UserAction] = (abilities && k < 2 && cpLeft == 3).??(
        Commander.opening.keys.$.%(a => onceLeft(f, a)).%(a => cpLeft >= Commander.opening(a)._1 && f.stock(Munitions) >= Commander.opening(a)._2)./~{
            case a @ (ReconSweep | PanzerwerferBarrage | ArtilleryEye) => Board.hexes./(h => OpeningHexAction(f, a, h, k))
            case a @ (P47Strike | FragmentationBomb | IL2Strike | P47Strafing | StukaStrafe | PerimeterOverwatch | HawkerTyphoon | KatyushaStrike) => Board.hexes./(h => OpeningLineStartAction(f, a, h, k))
            case a @ (HowitzerBarrage | ReconPlane | ConcentrationBarrage | ReconOverflight | ML20Barrage) => Board.hexes./(h => OpeningHexAction(f, a, h, k))
            case a @ MarkVehicle => enemies(f)./~(units).%(v => v.utype.vehicle && isPassenger(v).not)./(v => where(v)).distinct./(h => OpeningHexAction(f, a, h, k))
            case a => $(OpeningAction(f, a, k))
        })

    // three hexes in a straight line from a hex
    def lines(h : Hex) : $[$[Hex]] = 0.until(6).$./~{ d => Board.toward(h, d)./~(b => Board.toward(b, d)./(c => $(h, b, c))) }

    def dropNote(a : Ability) : Elem = (a @@ {
        case Paratroopers => "(Riflemen, " + "5 Manpower 1 Munitions, Double Slow)"
        case ParadropEquipment => "(a weapon team, 1 Manpower 5 Munitions, Double Slow)"
        case AirdroppedGroup => "(two weapon teams side by side, 3 Manpower 6 Munitions, Double Slow)"
        case SupplyRecon => "(a Scout Car for 3 Munitions)"
        case NKVDConscripts => "(two Conscript squads for 5 Munitions)"
        case InfiltrationCommandos => "(an Infantry Section next to an empty Building, 5 Manpower 1 Munitions, Slow)"
        case CommandoGlider => "(a Forward Emplacement on a visible hex, and an Infantry next to it with Slow, 5 Manpower 3 Munitions)"
        case MotorPool => "(two Jeeps for 5 Munitions)"
        case SupplyConvoy => "(a Kubelwagen for 3 Munitions)"
        case UniversalCarriers => "(two Universal Carriers for 5 Munitions)"
        case CommandosSpawn => "(Commandos on a visible hex, 5 Manpower 1 Munitions, Double Slow)"
        case AIPartisansSpawn | ATPartisansSpawn | ATPartisansSpawnFree => "(next to an empty Building, 1 Manpower 4 Munitions, Slow)"
        case AirdroppedCombatGroup => "(3 Infantry next to a Pathfinder Squad, Slow, 5 Manpower 6 Munitions)"
        case _ => ""
    }).spn(styles.small)

    def freeNote : Elem = freeQueue.headOption./(q => (q._2 @@ {
        case "para" | "drop" | "drop2" => "(on a visible hex, Double Slow)"
        case _ => ""
    }).spn(styles.small) : Elem).|(Empty)

    def openingNote(a : Ability) : Elem = Commander.opening.get(a)./{ case (c, m) => ("(" + c + " CP, " + m + " Munitions)").spn(styles.small) : Elem }.|(Empty)

    // the Supply Phase purchases a Commander adds
    def commanderPurchases(f : Faction) : $[UserAction] = abilities.??(
        commanderOf.get(f).$./~{ c =>
            val i = tiersOf.getOrElse(f, 0)
            (i < 4).??(c.tiers.lift(i).%(t => f.stock(XP) >= t.xp && f.tier >= t.needs)./(t => CommanderTierAction(f, c, i)).$)
        } ++
        commanderOf.get(f).none.??(drawn.getOrElse(f, $[Commander]()).%(c => f.stock(XP) >= c.tiers(0).xp)./(c => CommanderTierAction(f, c, 0))) ++
        (tiersOf.getOrElse(f, 0) >= 4 && f.stock(XP) >= 2).??(Res.spent./(r => TradeXPAction(f, r))) ++
        (unlocked(f, WestPointReserves) && units(f).exists(_.utype == Major).not).??(spawnHexes(f, Major)./(h => FreeMajorAction(f, h))) ++
        (unlocked(f, USBreakthrough) && breakthrough.contains(f).not && f.stock(Fuel) >= 1).$(BreakthroughAction(f)) ++
        (onceLeft(f, Paratroopers) && affordable(f, Cost(5, 1, 0)) && dropHexes(f).any).$(DropMenuAction(f, Paratroopers)) ++
        (onceLeft(f, ParadropEquipment) && affordable(f, Cost(1, 5, 0)) && dropHexes(f).any).$(DropMenuAction(f, ParadropEquipment)) ++
        (onceLeft(f, AirdroppedGroup) && affordable(f, Cost(3, 6, 0)) && dropHexes(f).exists(h => dropHexes(f).exists(Board.adjacent(h, _)))).$(DropMenuAction(f, AirdroppedGroup)) ++
        (unlocked(f, FestungBunkers) && affordable(f, MGBunker.cost) && builders(f).any).$(BuildMenuAction(f, MGBunker)) ++
        (unlocked(f, Pak43Emplacement) && affordable(f, Pak43.cost) && builders(f).any).$(BuildMenuAction(f, Pak43)) ++
        commanderSpawns(f)
    )

    // the units that may build an Emplacement now: Grenadiers, Osttruppen and Panzer Grenadiers with a Wrench left this Supply Phase, not
    // spawned this round; each with the adjacent hexes it may build on
    def builders(f : Faction) : $[(Squad, $[Hex])] = units(f).%(u => (u.utype == Grenadiers || u.utype == Osttruppen || u.utype == PanzerGrenadiers) && retreating.contains(u).not && spawnRound.get(u).has(round).not && built.getOrElse(u, 0) < u.utype.wrenches)
        ./(u => (u, Board.neighbors(where(u)).%(h => Board.joined(where(u), h) && isOpen(h) && standing(h).none))).filter(_._2.any)

    // the hexes a dropped unit may take, for each type
    def dropChoices(f : Faction, a : Ability) : $[(UnitType, Hex)] = a @@ {
        case Paratroopers => dropHexes(f)./(h => (Riflemen, h))
        case SupplyRecon => spawnHexes(f, ScoutCar)./(h => (ScoutCar, h))
        case NKVDConscripts => spawnHexes(f, Conscripts)./(h => (Conscripts, h))
        case InfiltrationCommandos => infiltrationHexes(f)./(h => (InfantrySection, h))
        case CommandoGlider => dropHexes(f).%(h => Board.neighbors(h).exists(n => isOpen(n) && standing(n).none))./(h => (ForwardEmplacement, h))
        case MotorPool => spawnHexes(f, Jeep)./(h => (Jeep, h))
        case SupplyConvoy => spawnHexes(f, Kubelwagen)./(h => (Kubelwagen, h))
        case UniversalCarriers => spawnHexes(f, UniversalCarrier)./(h => (UniversalCarrier, h))
        case CommandosSpawn => dropHexes(f)./(h => (Commandos, h))
        case AIPartisansSpawn => stormHexes(f)./(h => (AIPartisans, h))
        case ATPartisansSpawn | ATPartisansSpawnFree => stormHexes(f)./(h => (ATPartisans, h))
        case AirdroppedCombatGroup => beaconTypes(f)./~(t => beaconHexes(f)./(h => (t, h)))
        case _ => weaponTeams(f)./~(t => dropHexes(f)./(h => (t, h)))
    }

    // the free spawns of a queue entry: (type, hex) pairs
    def freeChoices(f : Faction) : $[(UnitType, Hex)] = freeQueue.headOption.$./~{ case (types, mode, _) =>
        types.filter(t => onBoard(f, t) < t.limit).flatMap { t =>
            val hexes = mode @@ {
                case "para" | "drop" => dropHexes(f)
                case "drop2" => dropHexes(f).%(h => lastDrop.exists(Board.adjacent(_, h)))
                // the Commando Glider's Infantry: an empty non-Building hex next to its Forward Emplacement
                case "glider" => lastDrop.$./~(x => Board.neighbors(x).%(n => Board.joined(x, n) && isOpen(n) && standing(n).none))
                case "beacon" => beaconHexes(f)
                // a free Emplacement built next to one of its builders
                case "builder" => units(f).%(b => t.builtBy.has(b.utype))./~(b => Board.neighbors(where(b)).%(n => Board.joined(where(b), n) && isOpen(n) && standing(n).none)).distinct
                case _ => spawnHexes(f, t)
            }
            hexes./(h => (t, h))
        }
    }

    def queueFree(f : Faction, a : Ability) {
        freeQueue = a @@ {
            case AirdroppedGroup => List((weaponTeams(f).toList, "drop", None), (weaponTeams(f).toList, "drop2", None))
            case RaidPoint | P47Strike => List((List(Riflemen), "para", None))
            case OsttruppenSquads => List((List(Osttruppen), "spawn", None), (List(Osttruppen), "spawn", None))
            case USBreakthrough => List((weaponTeams(f).toList, "spawn", None))
            case AmbushTraining => List((List(Grenadiers, MG42), "spawn", |(Camouflage)))
            case CovertTraining => List((List(InfantrySection, Vickers), "spawn", |(Camouflage)))
            case WASPFlamethrower => List((List(UniversalCarrier), "spawn", |(FlameRefit)))
            case SmokeEmplacement => List((List(MortarEmplacement), "builder", None))
            case SeventeenPounderAbility => List((List(SeventeenPounder), "builder", None))
            case VeteranPak43 => List((List(OKWPak43), "builder", None))
            case G43Rifles => List((List(Grenadiers), "spawn", |(AddAttack(UnitType.ai))))
            case FlameProjectors => List((List(Halftrack), "spawn", |(AddAttacks(UnitType.flame))))
            case GammonBombs => List((List(Commandos), "para", None))
            case SovietArchitecture => List((List(AIPartisans), "spawn", None))
            case ATPartisansSpawnFree => List((List(ATPartisans), "spawn", None))
            case Pak251 => List((List(Halftrack), "spawn", |(AddAttacks(UnitType.ap))))
            case _ => Commander.free(a, f).toList./(t => (List(t), "spawn", None))
        }
        // the free units spawned by a once-a-round ability count toward its limit
        if (a == RaidPoint || a == P47Strike)
            usedIn += (f, Paratroopers) -> round
        if (a == AirdroppedGroup)
            usedIn += (f, AirdroppedGroup) -> round
        if (a == TurretlessTerror)
            usedIn += (f, StormtrooperSpawn) -> round
        if (a == GammonBombs)
            usedIn += (f, CommandosSpawn) -> round
        if (a == SovietArchitecture)
            usedIn += (f, AIPartisansSpawn) -> round
        if (a == ATPartisansSpawnFree)
            usedIn += (f, ATPartisansSpawnFree) -> round
    }

    def performInternal(a : Action, soft : Void) : Continue = {
        implicit val action = a

        action match {
            // SETUP
            case StartAction(version) =>
                log("HRF".hl, "version", gaming.version.hlb)
                log("Company of Heroes".hlb, full.?("(2nd Edition, Full Game Rules)").|("(2nd Edition, First Game Rules)"), "on", map.name.hl)

                if (version != gaming.version)
                    log("Saved game version", version.hlb)

                log("Players", setup./(_.elem).comma)
                log("The game ends at", endVP.hlb, "Victory Points", teamGame.?("(a team's together)").|(""))

                if (full)
                    Shuffle[OpCard](Operations.deck, OpsShuffledAction(_))
                else
                    // each team rolls three Damage Dice; the most green, then black, symbols goes first
                    Roll[Face]((3 * teams.num).times(Face.die), FirstRolledAction(_))

            case OpsShuffledAction(l) =>
                opsDeck = l
                log("The Operations deck was shuffled")
                if (commanders)
                    Shuffle[Commander](Commander.pool(setup(0), options), CommandersShuffledAction(setup(0), _))
                else
                if (brigades)
                    Shuffle[Brigade](Brigade.of(setup(0)), BrigadesShuffledAction(setup(0), _))
                else
                    Roll[Face]((3 * teams.num).times(Face.die), FirstRolledAction(_))

            // each player draws three Commanders of their faction's six
            case CommandersShuffledAction(f, l) =>
                drawn += f -> l.take(3)
                log(f, "drew three Commanders")
                setup.dropWhile(_ != f).drop(1).starting match {
                    case Some(e) => Shuffle[Commander](Commander.pool(e, options), CommandersShuffledAction(e, _))
                    case None if brigades => Shuffle[Brigade](Brigade.of(setup(0)), BrigadesShuffledAction(setup(0), _))
                    case None => Roll[Face]((3 * teams.num).times(Face.die), FirstRolledAction(_))
                }

            // each player draws three of their faction's Lost Brigades
            case BrigadesShuffledAction(f, l) =>
                brigadesDrawn += f -> l.take(3)
                log(f, "drew three Lost Brigades")
                setup.dropWhile(_ != f).drop(1).starting match {
                    case Some(e) => Shuffle[Brigade](Brigade.of(e), BrigadesShuffledAction(e, _))
                    case None => Roll[Face]((3 * teams.num).times(Face.die), FirstRolledAction(_))
                }

            // in turn order, a Lost Brigade of 0 XP may be picked before the starting Infantry
            case BrigadeOpeningAction(order) =>
                order match {
                    case f :: rest if brigadeOf.contains(f).not && brigadesDrawn.getOrElse(f, $[Brigade]()).exists(_.xp == 0) =>
                        Ask(f).each(brigadesDrawn(f).%(_.xp == 0))(b => BrigadeStartAction(f, b, rest)).add(BrigadeStartSkipAction(f, rest))
                    case _ :: rest => BrigadeOpeningAction(rest)
                    case Nil => StartUnitsAction(rotation(team(first)).last.head, 0)
                }

            case BrigadeStartAction(f, b, rest) =>
                brigadeOf += f -> b
                log(f, "picked the Lost Brigade", b)
                BrigadeOpeningAction(rest)

            case BrigadeStartSkipAction(f, rest) =>
                BrigadeOpeningAction(rest)

            case BrigadePickAction(f, b) =>
                gain(f, XP, -b.xp)
                brigadeOf += f -> b
                log(f, "picked the Lost Brigade", b, "for", b.xp.hl, XP.icon)
                PurchaseAction(f)

            case FirstRolledAction(l) =>
                lastRolls = l
                def score(x : $[Face]) = x.count(_.crit) * 10 + x.count(f => f.success && f.crit.not)
                // a player of each team rolls three; the team going last in turn order places its starting Infantry first
                val rolled = teams.indices.toList./(i => (teams(i).head, l.drop(3 * i).take(3)))
                if (teams.num == 2)
                    log(rolled(0)._1, "rolled", rolled(0)._2./(_.elem).merge, "and", rolled(1)._1, "rolled", rolled(1)._2./(_.elem).merge)
                else
                    rolled.foreach { case (f, x) => log(f, "rolled", x./(_.elem).merge) }
                val best = rolled.map(x => score(x._2)).max
                if (rolled.filter(x => score(x._2) == best).size > 1) {
                    log("A tie, rolling again")
                    Roll[Face]((3 * teams.num).times(Face.die), FirstRolledAction(_))
                }
                else {
                    first = rolled.filter(x => score(x._2) == best).head._1
                    gameFirst = first
                    if (teamGame)
                        log(mates(first)./(_.elem).comma, "go first;", enemies(first)./(_.elem).comma, "place their starting Infantry first")
                    else
                    if (freeForAll)
                        log(first, "goes first, then", rotation(team(first)).drop(1)./(_.head.elem).comma, "(they place their starting Infantry in the reverse order)")
                    else
                        log(first, "goes first;", other(first), "places its starting Infantry first")
                    if (brigades)
                        BrigadeOpeningAction(turnOrder)
                    else
                        StartUnitsAction(rotation(team(first)).last.head, 0)
                }

            // three starting Infantry each, two in team games, as the missions are 2-panel ones (p. 7); the team going second places first
            case StartUnitsAction(f, n) if n >= startInfantry =>
                nextTurn(f) match {
                    case (m, false) => StartUnitsAction(m, 0)
                    case _ if team(f) != team(first) => StartUnitsAction(teams((team(f) + teams.num - 1) % teams.num).head, 0)
                    case _ => Milestone(RoundStartAction(first))
                }

            case StartUnitsAction(f, n) =>
                current = f
                Ask(f).each(UnitType.of(f).%(t => t.tier == 1 && t.kind == Infantry && t.commander.not) ++ brigadeStart(f))(t => StartPickAction(f, t, n))

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
                smoke = Set()
                eyes = Map()
                braced = Set()
                oberBraced = Map()
                stance = Set()
                raided = Set()
                charged = Set()
                decoyed = Set()
                bonus = Map()
                if (full) {
                    finalMoved = Set()
                    OpDrawAction(f)
                }
                else
                    TurnAction(f, 0)

            // the first team draws an Operation and picks its top (2 to 4 turns) or bottom (3 to 5); the other team learns only which
            case OpDrawAction(f) =>
                if (opsDeck.none)
                    Shuffle[OpCard](opsDiscard, OpsReshuffledAction(f, _))
                else {
                    current = f
                    Ask(f).add(OpPickAction(f, opsDeck.head, true)).add(OpPickAction(f, opsDeck.head, false))
                }

            case OpsReshuffledAction(f, l) =>
                opsDeck = l
                opsDiscard = $
                log("The Operations deck was shuffled again")
                OpDrawAction(f)

            case OpPickAction(f, c, upper) =>
                opsDeck = opsDeck.drop(1)
                opsCard = |(c)
                opsPicker = f
                opsUpper = upper
                opsTurns = c.turns(upper)
                log(f, "picked the", Operations.side(upper).hl, "Operation")
                TurnAction(f, 0)

            case TurnAction(f, k) =>
                if (k >= (full.?(teams.num * opsTurns).|(4 * teams.num))) {
                    if (full) {
                        opsCard.foreach { c =>
                            log("The Operation card was", c.elem, "and", opsPicker, "picked", opsTurns.hl, "turns")
                            opsDiscard :+= c
                        }
                        opsCard = None
                        RetreatStepAction(turnOrder)
                    }
                    else
                        DamagePhaseAction(first)
                }
                else {
                    current = f
                    cpLeft = 3
                    bonus = Map()
                    if (full)
                        log("Turn", (k / teams.num + 1).hl, "for", f)
                    else
                        log("Turn", (k / teams.num + 1).hl, "of", 4.hl, "for", f)
                    ManeuverAction(f, k)
                }

            case ManeuverAction(f, k) =>
                settleAssaults()
                val ready = units(f).%(u => unitActions(f, u, k).any || (cpLeft > 0 && mayRetreat(u) && retreatChoices(u).any))
                if (cpLeft == 0 && ready.none)
                    nextTurn(f) match { case (m, last) => TurnAction(m, k + last.??(1)) }
                else
                    Ask(f)
                        .group(cpNote(f))
                        .each(ready)(u => UnitMenuAction(f, u, k))
                        .add(openings(f, k))
                        .add(EndTurnAction(f, k))

            case UnitMenuAction(f, u, k) =>
                Ask(f).add(unitActions(f, u, k)).add(mayRetreat(u).??(retreatChoices(u)./{ case (p, e) => RetreatAction(f, u, p, e, k) })).cancel

            case MoveAction(f, u, path, k) =>
                val from = where(u)
                spend(u, path.num)
                moveSquad(u, path.last)
                log(u, "moved from", from, "to", path.last, (path.num > 1).??("through " + path.head.name))
                // Overwatch on each hex it moved onto
                path.foreach(h => overwatch(u, h, None))
                ManeuverAction(f, k)

            case HeavyMoveAction(f, u, path, e, k) =>
                val from = where(u)
                if (path == $(from)) {
                    spend(u, 1)
                    facing += u -> e
                    log(u, "turned to face", Facing.names(e), "in", from)
                }
                else {
                    spend(u, path.num)
                    moveSquad(u, path.last)
                    facing += u -> e
                    log(u, "moved from", from, "to", path.last, (path.num > 1).??("through " + path.head.name), "facing", Facing.names(e))
                }
                ManeuverAction(f, k)

            case AssaultAction(f, u, h, e, k) =>
                val d = occupant(h).get
                spend(u, 1)
                if (e >= 0)
                    facing += u -> e
                pos += u -> h
                passenger(u).foreach(p => pos += p -> h)
                assaults += u -> d
                log(u, "assaulted", d, "in", h)
                overwatch(u, h, |(d))
                ManeuverAction(f, k)

            case SovietIndustryAction(f) =>
                gain(f, Manpower, -1)
                sovietIndustry += f
                log(f, "paid", 1.hl, Manpower.icon, "for Soviet Industry")
                PurchaseAction(f)

            case PromoteAction(f, u, t) =>
                gain(f, Munitions, -4)
                val h = where(u)
                val left = health(u)
                val wasSet = bought.getOrElse(u, $[Bought]()).exists(_.effect == AddAttack(UnitType.flame))
                removeSquad(u)
                val v = createSquad(f, t, h)
                health += v -> min(maxHealth(v), left + 2)
                if (wasSet)
                    bought += v -> $(Bought(25, AddAttack(UnitType.flame)))
                log(u, "became", v, "for", 4.hl, Munitions.icon, "(Battlefield Promotions)")
                PurchaseNextAction(f)

            case DecoyAction(f, u, k) =>
                spend(u, 1)
                decoyed += u
                bonus += u -> (bonus.getOrElse(u, 0) + 3)
                log(u, "made a Decoy move: No-Fire this round")
                ManeuverAction(f, k)

            case EnglishIndustryAction(f) =>
                gain(f, Munitions, -2)
                gain(f, Fuel, 1)
                log(f, "paid", 2.hl, Munitions.icon, "for", 1.hl, Fuel.icon, "(English Industry)")
                PurchaseAction(f)

            case StepOnItAction(f, u, k) =>
                gain(f, Munitions, -2)
                raided += u
                bonus += u -> (bonus.getOrElse(u, 0) + 1)
                log(f, "paid", 2.hl, Munitions.icon, "for a Step on it move of", u)
                ManeuverAction(f, k)

            case CavalryChargeAction(f, u, h, k) =>
                val d = occupant(h).get
                charged += u
                pos += u -> h
                assaults += u -> d
                log(u, "charged", d, "in", h, "(a free Assault)")
                overwatch(u, h, |(d))
                ManeuverAction(f, k)

            case ReconModeAction(f, u, k) =>
                gain(f, Munitions, -2)
                raided += u
                bonus += u -> (bonus.getOrElse(u, 0) + 1)
                log(f, "paid", 2.hl, Munitions.icon, "for a Recon Mode move of", u)
                ManeuverAction(f, k)

            case WarSpeedAction(f, u, k) =>
                health += u -> (health(u) - 1)
                raided += u
                bonus += u -> (bonus.getOrElse(u, 0) + 1)
                log(u, "spent", 1.hl, "Health for an Emergency War Speed move")
                ManeuverAction(f, k)

            case SalvageAction(f, r) =>
                gain(f, Manpower, -2)
                gain(f, r, (r == Fuel).?(1).|(2))
                log(f, "paid", 2.hl, Manpower.icon, "for", (r == Fuel).?(1).|(2).hl, r.icon, "(Thorough Salvage)")
                PurchaseAction(f)

            case NoFireModeAction(f, u, k) =>
                spend(u, 1)
                if (noFire.contains(u)) {
                    noFire -= u
                    log(u, "went back to firing mode")
                }
                else {
                    noFire += u
                    log(u, "went into No-Fire Capture mode")
                }
                ManeuverAction(f, k)

            case DefensiveStanceAction(f, u, k) =>
                spend(u, 1)
                stance += u
                log(u, "took a Defensive Stance in", where(u))
                ManeuverAction(f, k)

            case HullDownAction(f, u, v, k) =>
                spend(u, 1)
                hullDown += v
                log(u, "put sandbags around", v, "in", where(v))
                ManeuverAction(f, k)

            case RaidPointAction(f, u, k) =>
                gain(f, Munitions, -1)
                raided += u
                bonus += u -> (bonus.getOrElse(u, 0) + 1)
                log(f, "paid", 1.hl, Munitions.icon, "for a Raid Point move of", u)
                ManeuverAction(f, k)

            case OpeningAction(f, a, k) =>
                val (c, m) = Commander.opening(a)
                cpLeft -= c
                gain(f, Munitions, -m)
                usedIn += (f, a) -> round
                val l = units(f).%(u => isPassenger(u).not && retreating.contains(u).not && (a @@ {
                    case ArmorBlitzkrieg | GrandAssault | Outmaneuver => u.utype.vehicle
                    case CombatGroupAdvance | BreakthroughTactics | TacticalMovement | HeroicCharge | ForMotherRussia => u.kind == Infantry
                    case _ => false
                }))
                l.foreach(u => bonus += u -> (bonus.getOrElse(u, 0) + 1))
                if (a == RapidConscription || a == RapidConscriptionPenal || a == RecoupLosses)
                    conscription += f -> round
                if (a == FortifiedCamo)
                    fortified += f -> round
                if (a == CombinedArms)
                    combinedArms += f -> round
                log(f, "used", a, "for", c.hl, "CP and", m.hl, Munitions.icon)
                ManeuverAction(f, k)

            case OpeningHexAction(f, a, h, k) =>
                val (c, m) = Commander.opening(a)
                cpLeft -= c
                gain(f, Munitions, -m)
                usedIn += (f, a) -> round
                if (a == ReconSweep || a == ArtilleryEye || a == ReconPlane || a == ReconOverflight)
                    eyes += f -> (eyes.getOrElse(f, Set()) + h)
                else
                if (a == MarkVehicle)
                    occupant(h).foreach(v => marked += v -> round)
                else
                    strikes :+= Strike(f, a, $(h))
                log(f, "used", a, "on", h, "for", c.hl, "CP and", m.hl, Munitions.icon)
                ManeuverAction(f, k)

            case OpeningLineStartAction(f, a, h, k) =>
                Ask(f).each(lines(h))(l => OpeningLineAction(f, a, l, k)).cancel

            case OpeningLineAction(f, a, l, k) =>
                val (c, m) = Commander.opening(a)
                cpLeft -= c
                gain(f, Munitions, -m)
                usedIn += (f, a) -> round
                strikes :+= Strike(f, a, l)
                log(f, "used", a, "on", l./(_.elem).comma, "for", c.hl, "CP and", m.hl, Munitions.icon)
                ManeuverAction(f, k)

            case RetreatAction(f, u, p, e, k) =>
                val from = where(u)
                retreatTo(u, e)
                log(u, "retreated from", from, "toward", p, "to", e)
                ManeuverAction(f, k)

            // after the turns, each team in turn order may Retreat, and its Retreating units go on toward a Retreat Point
            case RetreatStepAction(Nil) =>
                DamagePhaseAction(first)

            case RetreatStepAction(order @ (f :: rest)) =>
                settleAssaults()
                val l = units(f).%(u => finalMoved.contains(u).not && (mayRetreat(u) || (retreating.contains(u) && onRetreatPoint(u).not))).%(u => retreatChoices(u).any)
                if (l.none)
                    RetreatStepAction(rest)
                else {
                    current = f
                    Ask(f).group("Final Retreats").some(l)(u => retreatChoices(u)./{ case (p, e) => FinalRetreatAction(f, u, p, e, order) }).add(RetreatDoneAction(f, rest))
                }

            case FinalRetreatAction(f, u, p, e, order) =>
                val from = where(u)
                retreatTo(u, e)
                finalMoved += u
                log(u, "retreated from", from, "toward", p, "to", e)
                RetreatStepAction(order)

            // the Retreating units not moved go on toward the nearest Retreat Point
            case RetreatDoneAction(f, rest) =>
                units(f).%(u => retreating.contains(u) && onRetreatPoint(u).not && finalMoved.contains(u).not).foreach { u =>
                    retreatChoices(u).starting.foreach { case (p, e) =>
                        val from = where(u)
                        retreatTo(u, e)
                        finalMoved += u
                        log(u, "retreated from", from, "toward", p, "to", e)
                    }
                }
                RetreatStepAction(rest)

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
                overwatch(u, h, None)
                ManeuverAction(f, k)

            case SetUpFacingAction(f, u, e, k) =>
                spend(u, 1)
                setUp += u
                facing += u -> e
                log(u, "set up in", where(u), "facing", Facing.names(e))
                ManeuverAction(f, k)

            case SetUpAction(f, u, k) =>
                spend(u, 1)
                setUp += u
                log(u, "set up in", where(u))
                ManeuverAction(f, k)

            case PackUpAction(f, u, k) =>
                spend(u, 1)
                setUp -= u
                facing -= u
                log(u, "packed up in", where(u))
                ManeuverAction(f, k)

            case DefensesAction(f, u, w, t, k) =>
                spend(u, 1)
                if (w)
                    wire += where(u)
                if (t)
                    traps += where(u)
                log(u, "built", (w && t).?("Barbed Wire and a Tank Trap").|(w.?("Barbed Wire").|("a Tank Trap")), "in", where(u))
                ManeuverAction(f, k)

            case SandbagsAction(f, u, e, k) =>
                spend(u, 1)
                sandbags += where(u) -> (sandbags.getOrElse(where(u), Set()) ++ Sandbags.edges(e))
                log(u, "built Sandbags in", where(u), "facing", Sandbags.facing(e))
                ManeuverAction(f, k)

            case EndTurnAction(f, k) =>
                if (cpLeft > 0)
                    log(f, "forfeited", cpLeft.hl, "CP")
                nextTurn(f) match { case (m, last) => TurnAction(m, k + last.??(1)) }

            // DAMAGE PHASE: machine guns pin first, then every unit picks one target; all damage is simultaneous
            case DamagePhaseAction(f) =>
                phase = "Damage"
                current = null
                cpLeft = 0
                assignments = $
                splashes = $
                shots = $
                used = Map()
                decided = Set()
                log(DoubleLine)
                log("Damage Phase")
                demolished = Set()
                // a Sherman Dozer assaulting destroys the Sandbags of the hex it assaults
                assaults.toList.foreach { case (u, d) =>
                    if (u.utype == Dozer && pos.contains(d) && sandbags.contains(where(d))) {
                        sandbags -= where(d)
                        log(u, "destroyed the Sandbags in", where(d))
                    }
                }
                if (abilities)
                    SmokeStepAction(turnOrder)
                else
                    AssignNextAction(1, turnOrder)

            // Commanders' Smoke: at the start of the Damage Phase, vehicles may pay 2 Munitions to Smoke their hex
            case SmokeStepAction(Nil) =>
                AssignNextAction(1, turnOrder)

            case SmokeStepAction(order @ (f :: rest)) =>
                val l = units(f).%(v => isPassenger(v).not && smoke.contains(smokeHex(v)).not && smokeCost(v).exists(c => f.stock(Munitions) >= c))
                if (l.none)
                    SmokeStepAction(rest)
                else {
                    current = f
                    Ask(f).each(l)(v => SmokeAction(f, v, order)).add(SmokeDoneAction(f, rest))
                }

            case SmokeAction(f, v, order) =>
                val c = smokeCost(v).|(2)
                gain(f, Munitions, -c)
                if (c == 3)
                    crewRepairs += v
                smoke += smokeHex(v)
                log(v, "smoked", smokeHex(v))
                SmokeStepAction(order)

            case SmokeDoneAction(f, rest) =>
                SmokeStepAction(rest)

            case AssignNextAction(1, Nil) =>
                assignments.foreach { a =>
                    val t = a.target
                    // (in the Full Game Rules, Infantry assaulting into a Building may be pinned)
                    if (t.kind == Infantry && (inBuilding(where(t)) && (full && assaults.contains(t)).not).not && isPassenger(t).not && (t.utype.machineGun && setUp.contains(t)).not && pinned.contains(t).not && t.utype != PenalBattalion) {
                        pinned += t -> a.attacker
                        log(t, "was pinned by", a.attacker)
                    }
                }

                // units on the other team's Control Points neutralize them, unless pinned (or Retreating, or in an Assault)
                control.toList.foreach { case (h, f) =>
                    occupant(h).%(u => foe(u.faction, f) && pinned.contains(u).not && (full && (retreating.contains(u) || assaultAt(h))).not).foreach { u =>
                        control -= h
                        log(u, "neutralized", f, "Control Point in", h)
                    }
                }

                AssignNextAction(2, turnOrder)

            case AssignNextAction(2, Nil) =>
                // Ramming Maneuver: an Assaulting T-34 deals 2 AP to the unit it Assaults
                assaults.toList.foreach { case (u, d) =>
                    if (u.utype.chassis == T34 && unlocked(u.faction, RammingManeuver) && pos.contains(d)) {
                        assignments :+= Assignment(u, d, $(UnitType.ap, UnitType.ap))
                        log(u, "rams", d, "with", $(UnitType.ap, UnitType.ap)./(_.icon).merge)
                    }
                }
                BraceStepAction(turnOrder)

            // Advanced Emplacements: once Damage is assigned, each targeted Emplacement may spend 1 Munitions for +1 Defense Die
            case BraceStepAction(Nil) =>
                AssignedAction(first)

            case BraceStepAction(order @ (f :: rest)) =>
                // Ober Defense: each targeted Obersoldaten squad may buy Defense Dice
                def targeted(u : Squad) = assignments.exists(_.target == u) || splashes.exists(x => splashed(x.attacker, x.hex).has(u)) || strikes.exists(_.hexes.has(where(u)))
                val obers = unlocked(f, OberDefense).??(units(f).%(u => u.utype == Obersoldaten && oberBraced.contains(u).not && targeted(u)))
                val l = unlocked(f, AdvancedEmplacements).??(units(f).%(u => u.kind == Emplacement && braced.contains(u).not && (assignments.exists(_.target == u) || splashes.exists(x => splashed(x.attacker, x.hex).has(u)) || strikes.exists(_.hexes.has(where(u)))))).%(_ => f.stock(Munitions) >= 1)
                val ober = obers./~(u => 1.to(min(f.stock(Munitions), 4)).$./(x => OberBraceAction(f, u, x, order)))
                if (l.none && ober.none)
                    BraceStepAction(rest)
                else {
                    current = f
                    Ask(f).each(l)(u => BraceAction(f, u, order)).add(ober).add(BraceDoneAction(f, rest))
                }

            case OberBraceAction(f, u, x, order) =>
                gain(f, Munitions, -x)
                oberBraced += u -> x
                log(u, "paid", x.hl, Munitions.icon, "for", ("+" + x).hl, "Defense Dice (Ober Defense)")
                BraceStepAction(order)

            case BraceAction(f, u, order) =>
                gain(f, Munitions, -1)
                braced += u
                log(u, "braced for", 1.hl, Munitions.icon, "(+1 Defense Die)")
                BraceStepAction(order)

            case BraceDoneAction(f, rest) =>
                BraceStepAction(rest)

            case AssignedAction(_) =>
                // experience: 1 for each player, and 1 for each enemy unit it assigned damage that can hurt it
                setup.foreach { f =>
                    val hit = (assignments.%(_.attacker.faction == f).%(a => relevant(a.target, a.dice) || (onIce(a.target) && a.dice.exists(d => IceMatrix.dice(a.target.kind, d.dmg) > 0)))./(_.target) ++ splashes.%(_.attacker.faction == f)./~(s => splashed(s.attacker, s.hex).%(v => foe(v.faction, f)))).distinct
                    gain(f, XP, 1 + hit.num)
                    log(f, "gained", (1 + hit.num).hl, "XP")
                }
                if (full)
                    IceCheckAction(first)
                else
                    ResolveDamageAction(first)

            case IceCheckAction(f) =>
                val n = iceChecks.map(_._2).sum
                if (n > 0)
                    Roll[Face](n.times(Face.die), IceRolledAction(f, _))
                else
                    ResolveDamageAction(f)

            case IceRolledAction(f, rolled) =>
                var left = rolled
                iceChecks.foreach { case (t, n) =>
                    val x = left.take(n)
                    left = left.drop(n)
                    if (x.exists(_.success.not)) {
                        sunk += t
                        log(t, "fell through the Ice in", where(t), x./(_.elem).merge)
                    }
                    else
                        log(t, "held on the Ice in", where(t), x./(_.elem).merge)
                }
                ResolveDamageAction(f)

            case AssignNextAction(step, f :: rest) =>
                current = f
                def hexTargets(u : Squad) = (step == 2).??(splashHexes(u)./(h => SplashAction(f, u, h, remaining(u).count(_.dmg == HE), step, f :: rest)) ++ shotChoices(u)./{ case (h, d) => ShotAction(f, u, h, d, step, f :: rest) })
                val l = units(f).%(u => decided.contains(u).not).%(u => step == 2 || (u.utype.machineGun && setUp.contains(u))).%(u => targetChoices(u).any || hexTargets(u).any)
                l.starting match {
                    case Some(u) =>
                        Ask(f).each(targetChoices(u)) { case (t, d) => TargetAction(f, u, t, d, step, f :: rest) }.add(hexTargets(u)).add(HoldFireAction(f, u, step, f :: rest))
                    case None =>
                        AssignNextAction(step, rest)
                }

            case TargetAction(f, u, t, d, step, order) =>
                if (u.utype == Firefly && d.num == 3 && d.forall(_.dmg == HE)) {
                    gain(f, Munitions, -2)
                    log(u, "fired Tulip rockets for", 2.hl, Munitions.icon)
                }
                // Strafing Fire: the same dice may also hit, in full, a unit next to the target
                val second = (u.utype.strafing || (u.utype == Hetzer && assaults.contains(u))).??(targetsOf(u).filter(p => p._1 != t && Board.adjacent(where(p._1), where(t)) && d.forall(p._2.has)).map(_._1))
                assignments :+= Assignment(u, t, d)
                assigned(u, d)
                log(u, "attacks", t, "with", d./(_.icon).merge)
                if (second.any)
                    Ask(f).each(second)(x => StrafeAction(f, u, x, d, step, order)).add(NoStrafeAction(f, u, step, order))
                else
                    AssignNextAction(step, order)

            case StrafeAction(f, u, t, d, step, order) =>
                assignments :+= Assignment(u, t, d)
                log(u, "strafes", t, "with", d./(_.icon).merge)
                AssignNextAction(step, order)

            case NoStrafeAction(f, u, step, order) =>
                AssignNextAction(step, order)

            case SplashAction(f, u, h, n, step, order) =>
                splashes :+= Splash(u, h, n)
                assigned(u, remaining(u).%(_.dmg == HE))
                log(u, "fires HE Splash on", h, "with", n.times(Attack(HE).icon).merge)
                AssignNextAction(step, order)

            case ShotAction(f, u, h, d, step, order) =>
                shots :+= Shot(u, h, d)
                assigned(u, d)
                log(u, (terrainAt(h) == Building).?("fires at the Building in").|("fires at the defenses in"), h, "with", d./(_.icon).merge)
                AssignNextAction(step, order)

            case HoldFireAction(f, u, step, order) =>
                decided += u
                AssignNextAction(step, order)

            case ResolveDamageAction(f) if full =>
                val n = heNeeded
                if (n > 0)
                    Roll[Face](n.times(Face.die), HERolledAction(f, _))
                else
                    DefenseRollAction(f, $)

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
                val n = full.?(resolveFull(he, None)._1).|(resolve(he, None))./(_.defense).sum
                if (n > 0)
                    Roll[Face](n.times(Face.die), DefenseRolledAction(f, he, _))
                else
                    ApplyDamageAction(f, he, $)

            case DefenseRolledAction(f, he, defense) =>
                ApplyDamageAction(f, he, defense)

            case ApplyDamageAction(f, he, defense) if full =>
                lastRolls = he ++ defense
                if (defense.any)
                    log("Defense dice rolled", defense./(_.elem).merge)

                val (hits, building) = resolveFull(he, |(defense))
                val fights = assaults.toList

                hits.foreach { h =>
                    health += h.target -> (health(h.target) - h.damage)
                    conscripted(h.target, h.damage)
                    log(h.target, "in", where(h.target), "took", h.damage.hl, "damage", (h.defense > 0).??("(" + h.defense + " Defense Dice)"))
                }

                // Buildings: 2 health; a destroyed one is a Destroyed Building
                var fallen = $[Hex]()
                building.foreach { case (x, n0) =>
                    // Soviet Architecture: 1 less to a Building with the faction's Infantry in it
                    val n = standing(x).exists(u => u.kind == Infantry && unlocked(u.faction, SovietArchitecture)).?(max(0, n0 - 1)).|(n0)
                    buildingDamage += x -> (buildingDamage.getOrElse(x, 0) + n)
                    log("The Building in", x, "took", n.hl, "damage")
                    if (buildingDamage(x) >= Board.bunkers.has(x).?(4).|(2)) {
                        ruined += x
                        fallen :+= x
                        log("The Building in", x, "was destroyed")
                    }
                }

                // two or more damage dice on a hex (each splash die once for each unit it hits there and once for the hex) destroy its Sandbags
                (sandbags.keys.$ ++ wire ++ traps).distinct.foreach { x =>
                    val direct = assignments.%(a => pos.get(a.target).has(x))./(_.dice.num).sum
                    val shot = shots.%(_.hex == x)./(_.dice.num).sum
                    val splash = splashes.%(s => area(s.hex).has(x))./(s => s.n * (1 + splashed(s.attacker, s.hex).%(t => where(t) == x).num)).sum
                    val strike = strikes.%(_.hexes.has(x))./(_.dice.num).sum
                    if (direct + shot + splash + strike >= 2)
                        destroyDefenses(x)
                }

                // destroyed units (and those that fell through the Ice): a VP to the other team, an XP to their owner
                (sunk.$ ++ hits./(_.target).%(t => pos.contains(t) && health(t) <= 0)).distinct.%(pos.contains).foreach { t =>
                    val lost = t +: passenger(t).$
                    lost.foreach { x =>
                        log(x, "was destroyed")
                        killed(x, damagers(t))
                        gain(x.faction, XP, 1)
                    }
                    removeSquad(t)
                }

                // Assaults: the assaulting unit wins the hex if the assaulted one was destroyed; if both survive it falls back
                fights.foreach { case (a, d) =>
                    assaults -= a
                    if (pos.contains(a)) {
                        if (pos.contains(d).not) {
                            log(a, "won the Assault and holds", where(a))
                            crush(a)
                        }
                        else
                            fallBack(a) match {
                                case Some(x) =>
                                    pos += a -> x
                                    passenger(a).foreach(p => pos += p -> x)
                                    if (a.kind == Infantry) {
                                        retreating += a
                                        retreatEnds += a
                                    }
                                    log(a, "failed the Assault and fell back to", x)
                                case None =>
                                    log(a, "failed the Assault, had nowhere to fall back and was destroyed")
                                    killed(a, damagers(a))
                                    gain(a.faction, XP, 1)
                                    removeSquad(a)
                            }
                    }
                }

                // the units in a destroyed Building retreat out of it
                fallen.foreach(collapse)

                if (strikes.any)
                    log("Strikes hit", strikes./~(_.hexes).distinct./(_.elem).comma)
                assignments = $
                splashes = $
                shots = $
                strikes = $
                sunk = Set()
                used = Map()

                DemolitionStepAction(turnOrder)

            // Demolition (Devil's Brigade): at the very end of the Damage Phase, 4 HE to an adjacent Building, destroying it
            case DemolitionStepAction(Nil) =>
                SupplyPhaseAction(first)

            case DemolitionStepAction(order @ (f :: rest)) =>
                val l = demolitions(f)
                if (l.none)
                    DemolitionStepAction(rest)
                else {
                    current = f
                    Ask(f).each(l) { case (u, h) => DemolitionAction(f, u, h, order) }.add(DemolitionDoneAction(f, rest))
                }

            case DemolitionAction(f, u, h, order) =>
                demolished += u
                ruined += h
                log(u, "demolished the Building in", h)
                collapse(h)
                DemolitionStepAction(order)

            case DemolitionDoneAction(f, rest) =>
                DemolitionStepAction(rest)

            case ApplyDamageAction(f, he, defense) =>
                lastRolls = he ++ defense
                if (defense.any)
                    log("Defense dice rolled", defense./(_.elem).merge)

                val hits = resolve(he, |(defense))

                hits.foreach { h =>
                    health += h.target -> (health(h.target) - h.damage)
                    conscripted(h.target, h.damage)
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
                        killed(x, damagers(t))
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
                    occupant(h).%(u => foe(u.faction, e) && pinned.contains(u).not && (full && retreating.contains(u)).not).foreach { u =>
                        control -= h
                        log(u, "neutralized", e, "Control Point in", h)
                        Jests.note(this, "neutralized", e, u.faction, h.name)
                    }
                }

                Board.points.keys.$.sortBy(h => (h.r, h.c)).foreach { h =>
                    if (control.contains(h).not)
                        occupant(h).%(u => (u.kind == Infantry && pinned.contains(u).not && (full && retreating.contains(u)).not) || u.kind == Emplacement || (u.utype.vehicle && captures(u))).foreach { u =>
                            control += h -> u.faction
                            log(u, "captured the", Board.points(h)./(_.elem).comma, "Control Point in", h)
                        }
                }

                setup.foreach { e =>
                    Res.points.foreach(r => gain(e, r, income(e, r)))
                    log(e, "collected", Res.points./(r => income(e, r).hl ~ r.icon).join(" "))
                }

                // teams share VPs: a team's VPs are its players' together
                val sides = teams.indices.toList
                val ended = sides.%(i => teamVP(i) >= endVP)

                if (ended.any) {
                    val vps = sides./(i => i -> teams(i)./(e => pointsOf(e).count(h => Board.points(h).has(VP))).sum)
                    val most = vps.map(_._2).max
                    vps.filter(_._2 == most).single.foreach { case (i, n) =>
                        val e = teams(i).head
                        gain(e, VP, 3)
                        if (teamGame)
                            log(teams(i)./(_.elem).comma, "gained", 3.hl, "VP for holding the most Victory Point Control Points")
                        else
                            log(e, "gained", 3.hl, "VP for holding the most Victory Point Control Points")
                    }
                    val best = sides./(teamVP).max
                    val winner = sides.%(i => teamVP(i) == best).single./(teams(_).head).|(teams(team(gameFirst)).head)
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
                        .add(commanderPurchases(f))
                        .add(brigadePicks(f))
                        .add(emplacements(f))
                        .add(conversions(f))
                        .add(PassAction(f))
                }

            case SpawnMenuAction(f, t) =>
                Ask(f).each(spawnHexes(f, t))(h => SpawnAction(f, t, h)).cancel

            case SpawnAction(f, t, h) =>
                val c = costOf(f, t)
                pay(f, c)
                val u = createSquad(f, t, h)
                if (Board.spawn(team(f)).has(h).not && isPassenger(u).not) {
                    slowed += u -> (round + 1)
                    // OKW: Double Slow for a unit already Slow
                    if (t.slow > 0)
                        doubleSlow += u
                }
                if (t == Stormtroopers)
                    usedIn += (f, StormtrooperSpawn) -> round
                log(f, "spawned", u, "in", h, "for", c.elem)
                // Osttruppen: two squads for one
                if (t == Osttruppen) {
                    freeQueue = List((List(Osttruppen), "spawn", None))
                    FreeNextAction(f)
                }
                else
                // in the Full Game Rules a Heavy Vehicle faces a hex side
                if (full && t.kind == HeavyVehicle)
                    Ask(f).each(0.until(6).$)(e => SpawnFaceAction(f, u, e))
                else
                    PurchaseNextAction(f)

            case SpawnFaceAction(f, u, e) =>
                facing += u -> e
                log(u, "faces", Facing.names(e))
                FreeNextAction(f)

            // COMMANDER PURCHASES
            case CommanderTierAction(f, c, i) =>
                val t = c.tiers(i)
                gain(f, XP, -t.xp)
                if (i == 0) {
                    commanderOf += f -> c
                    log(f, "took", c, "as Commander")
                }
                // a Commander's +1 Health adds the health at once
                val before = units(f)./(u => u -> maxHealth(u)).toMap
                tiersOf += f -> (i + 1)
                log(f, "unlocked", t.ability, "for", t.xp.hl, XP.icon)
                units(f).foreach(u => health += u -> (health(u) + max(0, maxHealth(u) - before(u))))
                queueFree(f, t.ability)
                FreeNextAction(f)

            case TradeXPAction(f, r) =>
                gain(f, XP, -2)
                gain(f, r, 1)
                log(f, "traded", 2.hl, XP.icon, "for", 1.hl, r.icon)
                PurchaseAction(f)

            case FreeMajorAction(f, h) =>
                val u = createSquad(f, Major, h)
                log(f, "spawned", u, "in", h, "for free (West Point Reserves)")
                PurchaseAction(f)

            case BreakthroughAction(f) =>
                gain(f, Fuel, -1)
                breakthrough += f
                log(f, "paid", 1.hl, Fuel.icon, "for US Breakthrough")
                PurchaseAction(f)

            case DropMenuAction(f, a) =>
                Ask(f).each(dropChoices(f, a)) { case (t, h) => DropAction(f, a, t, h) }.cancel

            case DropAction(f, a, t, h) =>
                val c = a @@ {
                    case Paratroopers => Cost(5, 1, 0)
                    case ParadropEquipment => Cost(1, 5, 0)
                    case SupplyRecon => Cost(0, 3, 0)
                    case NKVDConscripts => Cost(0, 5, 0)
                    case InfiltrationCommandos => Cost(5, 1, 0)
                    case CommandoGlider => Cost(5, 3, 0)
                    case MotorPool => Cost(0, 5, 0)
                    case SupplyConvoy => Cost(0, 3, 0)
                    case UniversalCarriers => Cost(0, 5, 0)
                    case CommandosSpawn => commandosCost(f)
                    case AIPartisansSpawn | ATPartisansSpawn | ATPartisansSpawnFree => Cost(1, 4, 0)
                    case AirdroppedCombatGroup => Cost(5, 6, 0)
                    case _ => Cost(3, 6, 0)
                }
                pay(f, c)
                usedIn += (f, a) -> round
                val u = createSquad(f, t, h)
                if (a == InfiltrationCommandos || a == AirdroppedCombatGroup || a == AIPartisansSpawn || a == ATPartisansSpawn || a == ATPartisansSpawnFree)
                    slowed += u -> (round + 1)
                else
                if (a != SupplyRecon && a != NKVDConscripts && a != CommandoGlider && a != MotorPool && a != SupplyConvoy && a != UniversalCarriers) {
                    slowed += u -> (round + 1)
                    doubleSlow += u
                }
                log(f, (a == Paratroopers || a == ParadropEquipment || a == AirdroppedGroup).?("dropped").|("spawned"), u, "in", h, "for", c.elem)
                if (a == NKVDConscripts) {
                    freeQueue = List((List(Conscripts), "spawn", None))
                    FreeNextAction(f)
                }
                else
                if (a == MotorPool || a == UniversalCarriers) {
                    freeQueue = List(((a == MotorPool).?(Jeep : UnitType).|(UniversalCarrier) :: Nil, "spawn", None))
                    FreeNextAction(f)
                }
                else
                if (a == AirdroppedCombatGroup) {
                    freeQueue = List((beaconTypes(f).toList, "beacon", None), (beaconTypes(f).toList, "beacon", None))
                    FreeNextAction(f)
                }
                else
                if (a == CommandoGlider) {
                    lastDrop = |(h)
                    freeQueue = List((UnitType.of(f).%(t => t.kind == Infantry && t.tier <= f.tier && t.commander.not).toList, "glider", None))
                    FreeNextAction(f)
                }
                else
                if (a == AirdroppedGroup) {
                    lastDrop = |(h)
                    freeQueue = List((weaponTeams(f).toList, "drop2", None))
                    FreeNextAction(f)
                }
                else
                    PurchaseNextAction(f)

            case BuildMenuAction(f, t) =>
                Ask(f).some(builders(f)) { case (b, l) => l./(h => BuildAction(f, b, t, h)) }.cancel

            case EmplaceMenuAction(f, t) =>
                Ask(f).some(buildersOf(f, t)) { case (b, l) => l./(h => BuildAction(f, b, t, h)) }.cancel

            // OKW: the truck becomes the Tier Building (full health); its passenger falls back to the nearest empty hex toward a Retreat Point
            case ConvertAction(f, t, v) =>
                pay(f, tierCost(f, t))
                val h = where(v)
                passenger(v).foreach(evacuate)
                removeSquad(v)
                val u = createSquad(f, (t == 2).?(Battlegroup : UnitType).|(SchwererHQ), h)
                if (t > f.tier) {
                    f.tier = t
                    log(f, "unlocked", Tiers.name(f, t).hl, "(Tier " + t + "):", v, "in", h, "became", u)
                }
                else
                    log(f, "rebuilt", Tiers.name(f, t).hl, ":", v, "in", h, "became", u)
                PurchaseNextAction(f)

            case BuildAction(f, b, t, h) =>
                pay(f, t.cost)
                built += b -> (built.getOrElse(b, 0) + 1)
                val u = createSquad(f, t, h)
                log(b, "built", u, "in", h, "for", t.cost.elem)
                PurchaseNextAction(f)

            // the free units of an unlocked tier, one at a time (none if there is no room)
            case FreeNextAction(f) =>
                if (freeQueue.none)
                    PurchaseNextAction(f)
                else {
                    val l = freeChoices(f)
                    if (l.none) {
                        log("No room for a free", freeQueue.head._1./(_.elem).join(" or "))
                        freeQueue = freeQueue.tail
                        FreeNextAction(f)
                    }
                    else
                        Ask(f).each(l) { case (t, h) => FreeSpawnAction(f, t, h) }
                }

            case FreeSpawnAction(f, t, h) =>
                val (_, mode, extra) = freeQueue.head
                freeQueue = freeQueue.tail
                val u = createSquad(f, t, h)
                if (mode == "glider" || mode == "beacon")
                    slowed += u -> (round + 1)
                else
                if (mode == "builder") {
                    // it counts as the Emplacement of one builder next to it this round
                    units(f).%(b => t.builtBy.has(b.utype) && Board.adjacent(where(b), h)).sortBy(b => built.getOrElse(b, 0)).starting.foreach(b => built += b -> (built.getOrElse(b, 0) + 1))
                }
                else
                if (mode != "spawn") {
                    slowed += u -> (round + 1)
                    doubleSlow += u
                }
                if (mode == "drop")
                    lastDrop = |(h)
                extra.foreach { e =>
                    val slot = e @@ {
                        case Camouflage => unlocked(f, CovertTraining).?(14).|(10)
                        case FlameRefit => 1
                        case AddAttacks(a) if a == UnitType.flame => 22
                        case AddAttacks(a) if a == UnitType.ap => 23
                        case _ => 13
                    }
                    // the WASP: both Flame upgrades
                    val n = (e == FlameRefit).?(2).|(1)
                    bought += u -> (bought.getOrElse(u, $[Bought]()) ++ n.times(Bought(slot, e)))
                }
                log(f, "spawned", u, "in", h, "for free", extra./(e => ("with " + e.name).txt).|(Empty))
                if (full && t.kind == HeavyVehicle)
                    Ask(f).each(0.until(6).$)(e => SpawnFaceAction(f, u, e))
                else
                    FreeNextAction(f)

            case UpgradesMenuAction(f) =>
                Ask(f).each(upgrades(f)) { case (u, i, e) => UpgradeAction(f, u, i, e) }.cancel

            case UpgradeAction(f, u, i, e) =>
                val s = slotAt(u, i)
                val n = slotPrice(u, i)
                gain(f, s.res, -n)
                val wasCarrier = carries(u)
                bought += u -> (bought.getOrElse(u, $[Bought]()) :+ Bought(i, e))
                log(f, "upgraded", u, "with", e, "for", n.hl, s.res.icon)
                // the British, Soviet and OKW health upgrades add the health at once
                if (e == HealthUp)
                    health += u -> (health(u) + 1)
                // the Universal Carrier's flamethrower: its passenger falls back
                if (wasCarrier && carries(u).not)
                    passenger(u).foreach(evacuate)
                PurchaseNextAction(f)

            case UnlockAction(f, t) =>
                pay(f, tierCost(f, t))
                f.tier = t
                log(f, "unlocked", Tiers.name(f, t).hl, "(Tier " + t + ")")
                PurchaseNextAction(f)

            case PassAction(f) =>
                f.passed = true
                log(f, "passed")
                PurchaseNextAction(f)

            // the teams alternate purchases; teammates (simultaneous in the rulebook) take their turns one after another
            case PurchaseNextAction(f) =>
                val order = interleaved(first)
                val i = order.indexOf(f)
                (order.drop(i + 1) ++ order.take(i + 1)).%(_.passed.not).starting match {
                    case Some(n) => PurchaseAction(n)
                    case None => SupplyEndAction(first)
                }

            case SupplyEndAction(f) =>
                pinned = Map()
                breakthrough = Set()
                sovietIndustry = Set()
                built = Map()

                // the Retreats end for units on a Retreat Point, and for those that fell back this round
                if (full) {
                    retreating.$.sortBy(u => (setup.indexOf(u.faction), UnitType.all.indexOf(u.utype), u.n)).foreach { u =>
                        if (retreatEnds.contains(u) || onRetreatPoint(u)) {
                            retreating -= u
                            log(u, "finished its Retreat")
                        }
                    }
                    retreatEnds = Set()
                }

                // reinforce: Infantry in the Spawn Area, next to allies that Reinforce, or carried by one, heal 1 for each (not Retreating ones)
                setup./~(units).%(_.kind == Infantry).%(u => retreating.contains(u).not).foreach { u =>
                    val h = where(u)
                    val area = (isPassenger(u).not && Board.spawn(team(u.faction)).has(h)).??(1)
                    val near = mates(u.faction)./~(units).%(v => v != u && reinforces(v) && Board.joined(h, where(v)) && isPassenger(v).not && retreating.contains(v).not).num
                    val ride = carrier(u).%(reinforces).any.??(1)
                    val heal = min(maxHealth(u) - health(u), area + near + ride)
                    if (heal > 0) {
                        health += u -> (health(u) + heal)
                        log(u, "was reinforced by", heal.hl)
                    }
                }

                // repair: each Wrench repairs an adjacent allied vehicle or emplacement by 1 (the most damaged)
                setup./~(units).%(wrenchesOf(_) > 0).%(u => retreating.contains(u).not).foreach { u =>
                    1.to(wrenchesOf(u)).foreach { _ =>
                        mates(u.faction)./~(units).%(v => v != u && (v.utype.vehicle || v.kind == Emplacement) && carrier(u).has(v).not && health(v) < maxHealth(v) && Board.joined(where(u), where(v)))
                            .sortBy(v => health(v) * 100 / maxHealth(v)).starting.foreach { v =>
                                health += v -> (health(v) + 1)
                                log(u, "repaired", v)
                            }
                    }
                }

                // Vehicle Crew Repairs: a vehicle that paid for its Smoke repairs 1
                crewRepairs.%(v => pos.contains(v) && health(v) < maxHealth(v)).foreach { v =>
                    health += v -> (health(v) + 1)
                    log(v, "repaired itself (Vehicle Crew Repairs)")
                }
                crewRepairs = Set()

                Milestone(RoundStartAction(other(first)))

            case GameOverAction(w) =>
                isOver = true
                current = null
                setup.foreach(f => log(f, "finished with", f.stock(VP).hlb, "VP"))
                if (teamGame) {
                    teams.foreach(t => log(t./(_.elem).comma, "together", teamVP(team(t.head)).hlb, "VP"))
                    log(mates(w)./(_.elem).comma, "won")
                    GameOver(mates(w), "Game Over", mates(w)./(m => GameOverWonAction(null, m)))
                }
                else {
                    log(w, "won")
                    GameOver($(w), "Game Over", $(GameOverWonAction(null, w)))
                }
        }
    }
}
