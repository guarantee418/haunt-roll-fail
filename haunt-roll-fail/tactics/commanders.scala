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

// COMMANDERS (rulebook 8.0, Full Game Rules 8.2): the six core Commander cards of each faction, read off the 2nd Edition Faction Guide
// (pages 11 and 17; art by tools/commander-art.py). Each player draws three of their faction's six at setup and takes one when they
// first unlock a tier: the card's first panel (the Commander itself) is its tier 1, then left to right, each for the XP printed at its top
// left, some needing a Tier Building first. Once all four are unlocked, 2 XP trade for 1 Manpower, Munitions or Fuel at any time of the
// purchases. What each ability does is in the game's checks (`unlocked` in game.scala); the readings are in RULES.md.

sealed abstract class Ability(val name : String, val text : String) extends Record with Elementary {
    def elem = name.hl
}

// US
case object LMGRiflemen extends Ability("M1919A6 LMG", "The Riflemen's AI Munitions upgrade also gives +1 Range.")
case object ReconSweep extends Ability("Recon Sweep", "At the start of your Maneuver Phase spend your very first 1 CP and 1 Munitions: a hex and the hexes around it are visible to your team this round, Camouflage too.")
case object PriestCallIn extends Ability("Priest Call-In", "The M7 Priest can be bought. One spawns for free.")
case object AirdroppedGroup extends Ability("Airdropped Group", "Spawn 2 weapon teams on visible adjacent non-Building hexes, Double Slow the first round, for 3 Manpower and 6 Munitions; one a round. One Group spawns for free.")
case object RangerTraining extends Ability("Ranger Endurance Training", "Riflemen may take 4 CP a round, and their Munitions upgrade costs 1 less.")
case object DefensiveStance extends Ability("Defensive Stance", "Riflemen may spend 1 CP to drop to the ground: Heavy Cover this round, even if Assaulted; they spend no more CP, move or Retreat this round. Not in a Building.")
case object WestPointReserves extends Ability("West Point Reserves", "Majors have their XP and Munitions upgrades. Whenever you have no Major in play, one spawns for free in the purchases.")
case object AssaultEngineerSpawn extends Ability("Assault Engineers", "Riflemen may spawn as Assault Engineers. One spawns for free.")
case object DefensiveSmoke extends Ability("Defensive Smoke", "Vehicles may pay 2 Munitions to Smoke their hex at the start of the Damage Phase.")
case object ArmorTactician extends Ability("Armor Tactician", "Infantry next to your Heavy Vehicles gets +2 Defense Dice (as Cover; not in Buildings).")
case object EasyEightCallIn extends Ability("Sherman Easy Eight", "The Sherman Easy Eight can be bought (a Sherman for the limit). One spawns for free.")
case object JeepMG extends Ability(".50 cal Jeeps", "The Jeep's AI Munitions upgrade costs 1 less.")
case object VeteranGreyhounds extends Ability("Veteran Greyhounds", "Greyhounds have their Defense upgrade. One spawns for free.")
case object VeteranWolverines extends Ability("Veteran Wolverines", "Wolverines have their XP and Munitions upgrades. One spawns for free.")
case object Paratroopers extends Ability("Paratroopers", "Spawn Riflemen on any visible non-Building hex, Double Slow the first round, for 5 Manpower and 1 Munitions; one a round.")
case object RaidPoint extends Ability("Raid Point", "After spending a first CP on an Infantry, pay 1 Munitions to move it once more for free. One Paratrooper spawns for free.")
case object ParadropEquipment extends Ability("Paradrop Equipment", "Spawn a weapon team on any visible non-Building hex, Double Slow the first round, for 1 Manpower and 5 Munitions; one a round.")
case object P47Strike extends Ability("P-47 Anti-Tank Rocket Strike", "At the start of your Maneuver Phase spend your very first 2 CP and 2 Munitions: 3 hexes in a line each take 2 AP in the Damage Phase. One Paratrooper spawns for free.")
case object USBreakthrough extends Ability("US Breakthrough", "Pay 1 Fuel: Infantry costs 1 less Manpower this Supply Phase. One Weapon Team spawns for free.")
case object CombatGroupAdvance extends Ability("Combat Group Advance", "At the start of your Maneuver Phase spend your very first 1 CP and 3 Munitions: every Infantry may make a free move this turn.")
case object EliteATCrew extends Ability("Elite M1 57mm Crew", "AT Guns have +1 Health and a Speed Die. One spawns for free.")

// Wehrmacht
case object AmbushTraining extends Ability("Ambush Training", "Infantry can upgrade to Camouflage for 3 Munitions. One Grenadier or MG42 Team spawns with Camouflage for free.")
case object PanzerwerferBarrage extends Ability("Panzerwerfer Light Barrage", "At the start of your Maneuver Phase spend your very first 1 CP and 1 Munitions: a hex takes 1 HE in the Damage Phase.")
case object HullDown extends Ability("Hull Down", "Grenadiers, Osttruppen and Panzer Grenadiers may spend 1 CP to sandbag an adjacent Vehicle: 2 Defense Dice until it moves.")
case object VeteranOstwinds extends Ability("Veteran Ostwinds", "Ostwinds have their XP and Munitions upgrades. One spawns for free.")
case object ReconInForce extends Ability("Recon in Force", "Light Vehicles can upgrade to +2 Sight and spot Camouflage for 2 Munitions.")
case object IncendiaryMortars extends Ability("Incendiary Mortars", "The Mortar's XP Flame upgrade costs 1 XP. One spawns for free.")
case object FragmentationBomb extends Ability("Fragmentation Bomb", "At the start of your Maneuver Phase spend your very first 2 CP and 2 Munitions: 3 hexes in a line each take 2 HE in the Damage Phase.")
case object TigerCallIn extends Ability("Tiger Call-In", "The Tiger can be bought. One spawns for free.")
case object MechanizedInfantry extends Ability("Mechanized Infantry", "Grenadiers and Panzer Grenadiers can upgrade to +1 Speed Die for 2 Munitions.")
case object MG42ArmoredCars extends Ability("MG42 Armored Cars", "Sd.Kfz. 222s have their AI Munitions upgrade and may still buy the Defense one. One spawns for free.")
case object SmokeCanisters extends Ability("Smoke Canisters", "Vehicles may pay 2 Munitions to Smoke their hex at the start of the Damage Phase.")
case object Mechanized222Surge extends Ability("Mechanized 222 Surge", "One Sd.Kfz. 222 and one Panzer Grenadier squad spawn for free.")
case object FestungBunkers extends Ability("MG Bunker", "Grenadiers, Osttruppen and Panzer Grenadiers may build MG Bunkers for 2 Munitions.")
case object OsttruppenSquads extends Ability("Osttruppen", "Osttruppen can be bought, two squads for each. One pair spawns for free.")
case object Pak43Emplacement extends Ability("Pak 43 Emplacement", "Grenadiers, Osttruppen and Panzer Grenadiers can build a Pak 43 Emplacement for 4 Manpower.")
case object GermanIndustry extends Ability("German Industry", "The Panzer Korps (Tier 3) costs 2 less Fuel. One Kubelwagen spawns for free.")
case object ArmorBlitzkrieg extends Ability("Armor Blitzkrieg", "At the start of your Maneuver Phase spend your very first 1 CP and 3 Munitions: every Vehicle may make a free move this turn.")
case object EliteSturmpanzer extends Ability("Elite Sturmpanzer", "Brummbars no longer have Slow. One spawns for free.")
case object LMG42 extends Ability("LMG-42", "The Grenadiers' HE Munitions upgrade also gives +1 Range.")
case object StormtrooperSpawn extends Ability("Stormtroopers", "Stormtroopers can be bought: they spawn next to an empty Building, Slow the first round; one a round.")
case object G43Rifles extends Ability("G43 Rifles", "Grenadiers, Stormtroopers and Panzer Grenadiers can upgrade to +1 AI for 2 Munitions. One Grenadier spawns with it for free.")
case object VeteranPanzerGrenadiers extends Ability("Veteran Panzer Grenadiers", "Panzer Grenadiers have their XP Defense and both AP Munitions upgrades. One spawns for free.")

// a card: the Commander's name, its four abilities with their XP costs and the Tier Building each needs (0 for none)
case class Tier(ability : Ability, xp : Int, needs : Int = 0) extends Record

sealed abstract class Commander(val id : String, val name : String, val faction : Faction, val tiers : $[Tier]) extends Record with Elementary {
    def elem = name.styled(faction)
    def image = "commander:" + id
}

case object ReconSupportCompany extends Commander("recon-support", "Recon Support Company", US, $(Tier(LMGRiflemen, 1), Tier(ReconSweep, 1), Tier(PriestCallIn, 4, 3), Tier(AirdroppedGroup, 5, 2)))
case object RangerCompany extends Commander("ranger", "Ranger Company", US, $(Tier(RangerTraining, 1), Tier(DefensiveStance, 1), Tier(PriestCallIn, 4, 3), Tier(WestPointReserves, 4, 3)))
case object ArmorCompany extends Commander("armor-company", "Armor Company", US, $(Tier(AssaultEngineerSpawn, 3), Tier(DefensiveSmoke, 2), Tier(ArmorTactician, 1), Tier(EasyEightCallIn, 4, 3)))
case object MechanizedCompany extends Commander("mechanized-company", "Mechanized Company", US, $(Tier(JeepMG, 1), Tier(VeteranGreyhounds, 4, 2), Tier(DefensiveSmoke, 2), Tier(VeteranWolverines, 4, 3)))
case object AirborneCompany extends Commander("airborne", "Airborne Company", US, $(Tier(Paratroopers, 2), Tier(RaidPoint, 4), Tier(ParadropEquipment, 2, 2), Tier(P47Strike, 3)))
case object MobileSupportCompany extends Commander("mobile-support", "Mobile Support Company", US, $(Tier(USBreakthrough, 3), Tier(CombatGroupAdvance, 1), Tier(EliteATCrew, 4, 2), Tier(PriestCallIn, 1, 3)))

case object JaegerDoctrine extends Commander("jaeger", "Jaeger Infantry Doctrine", Wehrmacht, $(Tier(AmbushTraining, 3), Tier(PanzerwerferBarrage, 1), Tier(HullDown, 1), Tier(VeteranOstwinds, 4, 3)))
case object SpearheadDoctrine extends Commander("spearhead", "Spearhead Doctrine", Wehrmacht, $(Tier(ReconInForce, 1), Tier(IncendiaryMortars, 3, 2), Tier(FragmentationBomb, 1), Tier(TigerCallIn, 5, 3)))
case object MechanizedDoctrine extends Commander("mechanized-doctrine", "Mechanized Doctrine", Wehrmacht, $(Tier(MechanizedInfantry, 1), Tier(MG42ArmoredCars, 4, 2), Tier(SmokeCanisters, 2), Tier(Mechanized222Surge, 5, 2)))
case object FestungDoctrine extends Commander("festung", "Festung Armor Doctrine", Wehrmacht, $(Tier(FestungBunkers, 1), Tier(OsttruppenSquads, 3), Tier(Pak43Emplacement, 1), Tier(TigerCallIn, 5, 3)))
case object PanzerkorpsDoctrine extends Commander("panzerkorps", "Panzerkorps Doctrine", Wehrmacht, $(Tier(GermanIndustry, 3), Tier(SmokeCanisters, 2), Tier(ArmorBlitzkrieg, 1), Tier(EliteSturmpanzer, 5, 3)))
case object EliteTroopsDoctrine extends Commander("elite-troops", "Elite Troops Doctrine", Wehrmacht, $(Tier(LMG42, 1), Tier(StormtrooperSpawn, 2), Tier(G43Rifles, 3, 2), Tier(VeteranPanzerGrenadiers, 4, 2)))

object Commander {
    val all = $[Commander](ReconSupportCompany, RangerCompany, ArmorCompany, MechanizedCompany, AirborneCompany, MobileSupportCompany,
        JaegerDoctrine, SpearheadDoctrine, MechanizedDoctrine, FestungDoctrine, PanzerkorpsDoctrine, EliteTroopsDoctrine)

    def of(f : Faction) = all.%(_.faction == f)

    // the abilities spent at the start of a Maneuver Phase: CP, Munitions
    val opening = Map[Ability, (Int, Int)](ReconSweep -> (1, 1), P47Strike -> (2, 2), CombatGroupAdvance -> (1, 3), PanzerwerferBarrage -> (1, 1), FragmentationBomb -> (2, 2), ArmorBlitzkrieg -> (1, 3))

    // the units that spawn for free when a tier is unlocked
    def free(a : Ability, f : Faction) : $[UnitType] = a @@ {
        case PriestCallIn => $(Priest)
        case AssaultEngineerSpawn => $(AssaultEngineers)
        case EasyEightCallIn => $(EasyEight)
        case VeteranGreyhounds => $(Greyhound)
        case VeteranWolverines => $(Wolverine)
        case EliteATCrew => $(ATGun)
        case VeteranOstwinds => $(Ostwind)
        case IncendiaryMortars => $(WMortar)
        case TigerCallIn => $(Tiger)
        case MG42ArmoredCars => $(SdKfz222)
        case Mechanized222Surge => $(SdKfz222, PanzerGrenadiers)
        case GermanIndustry => $(Kubelwagen)
        case EliteSturmpanzer => $(Brummbar)
        case G43Rifles => $(Grenadiers)
        case VeteranPanzerGrenadiers => $(PanzerGrenadiers)
        case _ => $[UnitType]()
    }
}

// the setup's Commanders choice (Full Game Rules): three drawn for each player (the rulebook), or none; games made before have neither
sealed abstract class CommandersChoice(val name : String, val text : String) extends GameOption with OneOfGroup with ImportantOption {
    val group = "Commanders".txt
    def valueOn = name.hl
    override val explain = $[Elem](text.txt)
}
case object DrawCommanders extends CommandersChoice("Draw three Commanders", "Each player draws three of their faction's six core Commanders and plays one, unlocking its tiers with Experience (Full Game Rules only).")
case object NoCommanders extends CommandersChoice("No Commanders", "No Commander cards.")

object CommandersChoice {
    val all = $[CommandersChoice](DrawCommanders, NoCommanders)
}

// a strike from an opening ability: the hexes it hits in the Damage Phase
case class Strike(faction : Faction, ability : Ability, hexes : $[Hex]) extends Record {
    // the dice each hex takes
    def dice : $[Attack] = ability @@ {
        case P47Strike => $(Attack(AP), Attack(AP))
        case FragmentationBomb => $(Attack(HE), Attack(HE))
        case PanzerwerferBarrage => $(Attack(HE))
        case _ => $[Attack]()
    }
}
