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
// (pages 11, 17, 22, 28 and 35; art by tools/commander-art.py; its Commander FAQ, pages 2 to 6, settles some readings). Each player draws three of their faction's six at setup and takes one when they
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

// British (Faction Guide page 22)
case object AdvancedEmplacements extends Ability("Advanced Emplacements", "Once Damage is assigned, each targeted Emplacement may spend 1 Munitions for +1 Defense Die this round.")
case object ForwardEmplacements extends Ability("Forward Emplacement", "Infantry Sections can build Forward Emplacements (3 health, Spots Camo, Reinforce, Retreat Point) for 2 Manpower; Infantry may spawn next to one, Slow the first round.")
case object SmokeEmplacement extends Ability("Smoke Emplacement", "Each Emplacement may pay 1 Munitions to Smoke its hex at the start of the Damage Phase. One Mortar Emplacement is built for free next to an Infantry Section.")
case object SeventeenPounderAbility extends Ability("17 Pounder Emplacement", "Infantry Sections can build 17 Pounder Emplacements (4 health, range 4, 4 AP, a Defense Die) for 5 Manpower. One is built for free next to an Infantry Section.")
case object RoyalEngineerSpawn extends Ability("Royal Engineer Regiment", "Infantry Sections may spawn as Royal Engineers: 4 health, 2 Close Range Firepower and no other AI, 2 Wrenches. One spawns for free.")
case object VehicleCrewRepairs extends Ability("Vehicle Crew Repairs", "Vehicles may pay 3 Munitions to Smoke their hex at the start of the Damage Phase, then Repair 1 in the Supply Phase.")
case object ChurchillAVRECallIn extends Ability("Churchill AVRE", "The Churchill AVRE can be bought (a Churchill for the limit).")
case object EmergencyWarSpeed extends Ability("Emergency War Speed", "After the first CP on a Vehicle, it may spend 1 Health to move again for free, as with a Speed Die. One Churchill AVRE spawns for free.")
case object CovertTraining extends Ability("Covert Training", "Infantry can upgrade to Camouflage for 3 Munitions. One Infantry Section or Vickers MG spawns with Camouflage for free.")
case object StaghoundCallIn extends Ability("Staghound Call-In", "The Staghound can be bought. One spawns for free.")
case object CommandoGlider extends Ability("Commando Glider", "Spawn a Forward Emplacement on a visible non-Building hex and any one Infantry next to it, Slow the first round, for 5 Manpower and 3 Munitions; one a round.")
case object ThompsonSMGs extends Ability("Thompson SMGs", "Infantry Sections can upgrade to 2 more Close Range Firepower for 4 Munitions.")
case object InfiltrationCommandos extends Ability("Infiltration Commandos", "Spawn an Infantry Section next to an empty Building, Slow the first round, for 5 Manpower and 1 Munitions; one a round.")
case object VeteranStaghounds extends Ability("Veteran Staghounds", "Staghounds have +1 AP. One spawns for free.")
case object APSniperCallIn extends Ability(".55 cal Armor Piercing Sniper", "The .55 cal Armor Piercing Sniper can be bought. One spawns for free.")
case object HoldTheLine extends Ability("Hold the Line", "Infantry within 1 hex of a Control Point your team holds gets 2 Defense Dice (Heavy Cover, replacing other Cover), also when Assaulting or Assaulted; not in Buildings.")
case object OperationCrocodile extends Ability("Operation Crocodile", "Churchills deal 2 forward Flame in place of all their AI, and have no Slow. One spawns for free.")
case object InfantrySupport extends Ability("Infantry Support Regiment", "Universal Carriers can upgrade to Reinforce for 2 Munitions.")
case object WASPFlamethrower extends Ability("WASP Flamethrower", "The Universal Carrier's Flame upgrades cost 1 less. One spawns for free with both.")
case object BolsterInfantry extends Ability("Bolster Infantry Squads", "Infantry Sections and Vickers MGs with their +1 Health upgrade also have a Defense Die.")
case object VeteranCromwells extends Ability("Veteran Cromwells", "Cromwells have their XP Speed Die and both AP Munitions upgrades. One spawns for free.")

// Soviet (page 28)
case object SupplyOperations extends Ability("Assault Tactics", "Scout Cars may spend 1 CP to toggle a No-Fire mode: on a Control Point, one in it gains 1 more of the point's resource when income is collected.")
case object SupplyRecon extends Ability("Supply Recon", "Scout Cars may be bought for 3 Munitions. One spawns for free.")
case object T34Wave extends Ability("T-34 Wave", "T-34s have their AI Munitions upgrade and a Speed Die. One spawns for free.")
case object RammingManeuver extends Ability("Ramming Maneuver", "An Assaulting T-34 deals 2 AP to the unit it Assaults in the Damage Phase.")
case object RangedTactics extends Ability("Soviet Ranged Tactics", "The Sniper Squad's Munitions upgrade costs 2 less.")
case object DShKCallIn extends Ability("DShK 38 HMG", "The DShK 38 HMG can be bought. One spawns for free.")
case object ATAmbushTactics extends Ability("Anti-Tank Ambush Tactics", "76mm AT Guns have Camouflage. One spawns for free.")
case object VeteranSU76 extends Ability("Veteran SU-76 Assault Guns", "SU-76s have their XP Speed Die and Munitions Health upgrades.")
case object ArtilleryEye extends Ability("Elite Artillery Tactics", "At the start of your Maneuver Phase spend your very first 1 CP and 1 Munitions: a hex and the hexes around it are visible to your team this round, Camouflage too.")
case object VeteranMortars extends Ability("Veteran Mortars", "The Mortar Team's HE Munitions upgrade costs 2 less. One spawns for free.")
case object WaveAndShell extends Ability("Wave and Shell", "One Mortar Team and one T-34 spawn for free.")
case object ConscriptSupport extends Ability("Conscript Support Tactics", "New Conscripts spawn with both their +1 Health Munitions upgrades. One spawns for free.")
case object HitTheDirt extends Ability("Hit the Dirt", "Conscripts may spend 1 CP to drop to the ground: Heavy Cover this round, even if Assaulted; they spend no more CP, move or Retreat this round. Not in a Building.")
case object RapidConscription extends Ability("Rapid Conscription", "At the start of your Maneuver Phase spend your very first 2 CP and 1 Munitions: this round, each Damage your Infantry takes gives you 1 Manpower.")
case object T3485CallIn extends Ability("T-34-85 Medium Tank", "The T-34-85 can be bought (a T-34 for the limit). One spawns for free.")
case object NKVDConscripts extends Ability("NKVD Disruption Tactics", "Two Conscript squads can spawn for 5 Munitions. One spawns for free.")
case object ForwardHQAbility extends Ability("Forward HQ", "Conscripts can turn an empty Building next to them into a Forward HQ (an Emplacement: 3 health, Reinforce, Retreat Point) for 2 Manpower, one at a time; Infantry may spawn next to it, Slow the first round.")
case object IL2Strike extends Ability("IL-2 Anti-Tank Strike", "At the start of your Maneuver Phase spend your very first 2 CP and 2 Munitions: 3 hexes in a line each take 2 AP in the Damage Phase.")
case object VeteranSU85 extends Ability("Veteran SU-85s", "SU-85s have their XP and Munitions upgrades and no Slow. One spawns for free.")
case object ReconMode extends Ability("Armored Assault Tactics", "After the first CP on a Vehicle, pay 2 Munitions to move it once more for free, as with a Speed Die.")
case object T70InfantryTank extends Ability("T-70 Infantry Tank", "T-70s have a Defense Die. One spawns for free.")
case object SmokeAmbush extends Ability("Smoke Ambush", "Vehicles may pay 2 Munitions to Smoke their hex at the start of the Damage Phase.")
case object T80Turret extends Ability("T-80 Two Man Turret", "T-70s have +1 Health and +1 AI. One spawns for free.")

// OKW (page 35)
case object WillNotDieOld extends Ability("Breakthrough Doctrine", "Infantry can upgrade to Heroic Sacrifice for 3 Munitions; Infantry with Heroic Sacrifice has +1 Defense Die when Assaulting.")
case object PanzerfusilierCallIn extends Ability("Panzerfusilier", "Panzerfusiliers can be bought. One spawns for free.")
case object BreakthroughTactics extends Ability("Breakthrough Tactics", "At the start of your Maneuver Phase spend your very first 1 CP and 3 Munitions: every Infantry may make a free move this turn. One Volksgrenadier squad spawns for free.")
case object PantherReinforcements extends Ability("Panther Reinforcements", "Panthers cost 1 less Manpower and 1 less Fuel.")
case object ThoroughSalvage extends Ability("Scavenge Doctrine", "In the purchases, pay 2 Manpower for 1 Fuel or for 2 Munitions, any number of times.")
case object JaegerCallIn extends Ability("Jaeger Recon Squad", "The Jaeger Recon Squad can be bought. One spawns for free.")
case object PumaCallIn extends Ability("Puma Call-In", "The Puma can be bought. One spawns for free.")
case object VeteranLeFH extends Ability("Veteran LeFH 18 Artillery", "Sturmpioneers can build LeFH 18 Artillery Emplacements with +1 HE for 4 Munitions.")
case object KettenkradCallIn extends Ability("Kettenkrad", "The Kettenkrad can be bought.")
case object HeavyLuchs extends Ability("Heavy Luchs", "Luchs have their Defense Die Munitions upgrade and may still buy the Speed one. One spawns for free.")
case object GrandAssault extends Ability("Grand Assault", "At the start of your Maneuver Phase spend your very first 1 CP and 3 Munitions: every Vehicle may make a free move this turn. One Kettenkrad spawns for free.")
case object SturmtigerReinforcements extends Ability("Sturmtiger Reinforcements", "Sturmtigers cost 2 less Fuel.")
case object FortifiedCamo extends Ability("Fortifications Doctrine", "At the start of your Maneuver Phase spend your very first 1 CP and 1 Munitions: your Emplacements are Camouflaged this round.")
case object LeFH18Ability extends Ability("LeFH 18 Artillery", "Sturmpioneers can build LeFH 18 Artillery Emplacements for 4 Munitions.")
case object ScoutLuchs extends Ability("Scout Luchs", "Luchs have their Speed Die Munitions upgrade and may still buy the Defense one. One spawns for free.")
case object VeteranPak43 extends Ability("Veteran Pak 43", "Sturmpioneers can build Pak 43 Emplacements with +1 AP for 4 Manpower. One is built for free next to a Sturmpioneer squad.")
case object Flammenwerfer extends Ability("Flammenwerfer 35", "Sturmpioneers can upgrade to +1 Flame for 4 Munitions.")
case object RocketSupport extends Ability("Rocket Support", "Stuka Halftracks have their XP and Munitions upgrades. One spawns for free.")
case object StG44Volks extends Ability("Grand Offense Doctrine", "Volksgrenadiers may spawn as StG44 Rifle Volksgrenadiers: +1 AI and no Defense Die.")
case object KettenkradEngineers extends Ability("Kettenkrad Engineers", "Kettenkrads have a Wrench: they Repair and build Sandbags. One spawns for free.")
case object HEATShells extends Ability("HEAT Shells", "Pumas and Panthers can upgrade to +1 AP for 3 Munitions. One Panther spawns for free.")

// US expansion Commanders (2026-11-24; Faction Guide page 11 and 12: Elite Commanders Collection, Pathfinder Pack, StuG Pack, Lost
// Brigades Pack). The panels saying "Call-In" with no free unit are the `...Plain` abilities
case object LightInfantryTraining extends Ability("Light Infantry Training", "Riflemen can upgrade to +1 Speed Die for 2 Munitions.")
case object StuartCallIn extends Ability("Stuart Call-In", "The M5 Stuart can be bought.")
case object StuartCallInFree extends Ability("Stuart Call-In", "The M5 Stuart can be bought. One spawns for free.")
case object StrategicManeuvers extends Ability("Strategic Maneuvers", "Stuarts may spend 1 CP to toggle a No-Fire mode in which they Capture, and 1 CP to go back. One Riflemen spawns for free.")
case object BrowningM2 extends Ability("Browning M2 .50cal", "All Stuarts gain 1 AI die and +1 Range. One Stuart spawns for free.")
case object RiflemenFieldDefenses extends Ability("Riflemen Field Defenses", "Riflemen gain +1 Defense Die when Sandbags give them cover.")
case object CombinedArms extends Ability("Combined Arms", "At the start of your Maneuver Phase spend your very first 2 CP and 2 Munitions: your units next to another of yours get +1 Sight and Range this round.")
case object RequisitionOfficer extends Ability("Requisition Officer", "Wolverines and Shermans (and the units on their chassis) cost 1 less Manpower. One Riflemen spawns for free.")
case object PershingCallIn extends Ability("Pershing Call-In", "The M26 Pershing can be bought. One spawns for free.")
case object PershingCallInPlain extends Ability("Pershing Call-In", "The M26 Pershing can be bought.")
case object USMGBunkers extends Ability("MG Bunker", "Riflemen can build MG Bunkers for 2 Munitions.")
case object P47Strafing extends Ability("P-47 Strafing Support", "At the start of your Maneuver Phase spend your very first 2 CP and 2 Munitions: 3 hexes in a line each take 2 HE in the Damage Phase.")
case object CalliopeCallIn extends Ability("Calliope Call-In", "The T34 Calliope can be bought. One spawns for free.")
case object CalliopeCallInPlain extends Ability("Calliope Call-In", "The T34 Calliope can be bought.")
case object MotorPool extends Ability("Motor Pool", "Two Jeeps can spawn for 5 Munitions. One Jeep spawns for free.")
case object Outmaneuver extends Ability("Outmaneuver", "At the start of your Maneuver Phase spend your very first 1 CP and 3 Munitions: every Vehicle may make a free move this turn. One Pershing spawns for free.")
case object PathfinderSquads extends Ability("I.R. Pathfinder Squad", "Pathfinder Squads can be bought: 3 health, range 3, 1 AI, +2 Sight spotting Camouflage, Camouflage.")
case object AirdropBeacon extends Ability("Airdrop Beacon", "Bought Infantry other than Pathfinders may spawn next to a Pathfinder Squad, Slow the first round. One Pathfinder spawns for free.")
case object Mortars81mm extends Ability("81mm Mortars", "Mortar Teams with their Splash XP upgrade also gain 1 HE. One Mortar Team spawns for free.")
case object AirdroppedCombatGroup extends Ability("Airdropped Combat Group", "Spawn 3 Infantry other than Pathfinders next to a Pathfinder Squad, Slow the first round, for 5 Manpower and 6 Munitions; one a round.")
case object DevilsBrigadeSpawn extends Ability("Devil's Brigade", "Riflemen may spawn as Devil's Brigade: +1 Health, they may enter Water, and a Destroyed Building gives them 3 Defense Dice. One spawns for free.")
case object DevilsAmbush extends Ability("Ambush", "Devil's Brigade can upgrade to Camouflage and Close Range Firepower for 4 Munitions.")
case object Demolition extends Ability("Demolition", "At the very end of the Damage Phase, each Devil's Brigade may deal 4 HE to one adjacent Building (destroying it), but not to the units inside.")
case object Sherman76 extends Ability("Sherman 76s", "Shermans have their XP and Munitions upgrades. One Sherman spawns for free.")
case object StepOnIt extends Ability("Step on it", "After spending a first CP on a Vehicle, pay 2 Munitions to move it once more for free.")
case object CavalryRiflemenCallIn extends Ability("Cavalry Riflemen", "Cavalry Riflemen can be bought: 5 health, range 2, 1 AI, Close Range Firepower, a Defense Die; after a move they may Assault an adjacent hex for free. One spawns for free.")
case object EasyEightCallInPlain extends Ability("Sherman Easy Eight", "The Sherman Easy Eight can be bought (a Sherman for the limit).")
case object DozerCallIn extends Ability("Sherman 105mm Dozer", "The Sherman Dozer can be bought (a Sherman for the limit); Assaulting, it destroys the hex's Sandbags at the start of the Damage Phase. One spawns for free.")
case object DozerCallInPlain extends Ability("Sherman 105mm Dozer", "The Sherman Dozer can be bought (a Sherman for the limit); Assaulting, it destroys the hex's Sandbags at the start of the Damage Phase.")
case object UrbanAssault extends Ability("Urban Assault", "When Assaulting, Riflemen may pay 1 Munitions to Smoke the Assaulted hex at the start of the Damage Phase. One Riflemen spawns for free.")
case object HowitzerBarrage extends Ability("Howitzer Barrage", "At the start of your Maneuver Phase spend your very first 1 CP and 1 Munitions: a hex takes 1 HE in the Damage Phase.")
case object JacksonCallIn extends Ability("M36 Jackson", "The M36 Jackson can be bought (a Wolverine for the limit). One spawns for free.")
case object JacksonCallInPlain extends Ability("M36 Jackson", "The M36 Jackson can be bought (a Wolverine for the limit).")
case object AssaultEngineerSpawnPlain extends Ability("Assault Engineers", "Riflemen may spawn as Assault Engineers.")
case object SupportJeeps extends Ability("Support Jeeps", "Jeeps have their Reinforce XP upgrade. One Jeep spawns for free.")

// Wehrmacht expansion Commanders (2026-11-24; Faction Guide pages 17 and 18: StuG Pack, Elite Commanders Collection, Pathfinder Pack,
// Lost Brigades Pack)
case object StuGCloseRange extends Ability("StuG Assault Doctrine", "All Regular Infantry can upgrade to Close Range Firepower for 2 Munitions.")
case object StuGCallIn extends Ability("StuG III G Call-In", "The StuG III G can be bought.")
case object StuGCallInFree extends Ability("StuG III G Call-In", "The StuG III G can be bought. One spawns for free.")
case object TankDesant extends Ability("Tank Desant Tactics", "Infantry gain +2 Defense Dice next to a Heavy Vehicle (not in a Building, no stacking with other Cover). One Grenadier spawns for free.")
case object TargetWeakPoint extends Ability("Target Weak Point", "StuG III Gs have +1 AP. One spawns for free.")
case object TurretlessTerror extends Ability("Turretless Terror", "StuG III Gs have Camouflage. One Stormtrooper spawns for free (it counts toward the one a round).")
case object LMG42Free extends Ability("LMG-42", "The Grenadiers' HE Munitions upgrade also gives +1 Range. One Grenadier spawns for free.")
case object WMBreakthrough extends Ability("Breakthrough Tactics", "After spending a first CP on a Vehicle, pay 2 Munitions to move it once more for free.")
case object StukaStrafe extends Ability("Stuka JU-87 Anti-Tank Strafe", "At the start of your Maneuver Phase spend your very first 2 CP and 2 Munitions: 3 hexes in a line each take 2 AP in the Damage Phase.")
case object KingTigerCallIn extends Ability("King Tiger Call-In", "The King Tiger can be bought. One spawns for free.")
case object MarkVehicle extends Ability("Tank Destroyer Doctrine", "At the start of your Maneuver Phase spend your very first 1 CP and 2 Munitions to mark an enemy Vehicle: it has no Defense Dice this round.")
case object JaegerSniperFree extends Ability("Jaeger Sniper", "Jaeger Snipers can be bought: 3 health, range 3, 2 AI, a Speed Die, +3 Sight spotting Camouflage, Camouflage. One spawns for free.")
case object JaegerSniperCallIn extends Ability("Jaeger Sniper", "Jaeger Snipers can be bought: 3 health, range 3, 2 AI, a Speed Die, +3 Sight spotting Camouflage, Camouflage.")
case object JagdpantherCallIn extends Ability("Jagdpanther Call-In", "The Jagdpanther can be bought. One spawns for free.")
case object AssaultSupport extends Ability("Assault Support Doctrine", "Kubelwagens may spend 1 CP to toggle a No-Fire mode; in it on a Control Point they collect 1 more of its resource.")
case object SupplyConvoy extends Ability("Supply Convoy", "Kubelwagens can be bought for 3 Munitions. One spawns for free.")
case object ReconPlane extends Ability("Recon Plane", "At the start of your Maneuver Phase spend your very first 1 CP and 1 Munitions: a hex and the hexes around it are visible to your team this round, Camouflage too.")
case object ElefantCallIn extends Ability("Elefant Call-In", "The Elefant can be bought. One spawns for free.")
case object HalftrackCallIn extends Ability("SdKfz 251 Half-track", "The SdKfz 251 can be bought; Infantry may spawn next to it, Slow the first round.")
case object HalftrackCallInFree extends Ability("SdKfz 251 Half-track", "The SdKfz 251 can be bought; Infantry may spawn next to it, Slow the first round. One spawns for free.")
case object FlameProjectors extends Ability("Flame Projectors", "SdKfz 251s can upgrade to 2 Flame for 4 Munitions. One spawns with it for free.")
case object Pak251 extends Ability("Pak 251 Half-track", "SdKfz 251s can upgrade to +2 AP for 4 Munitions. One spawns with it for free.")
case object OsttruppenSquadsPlain extends Ability("Osttruppen", "Osttruppen can be bought, two squads for each.")
case object TacticalMovement extends Ability("Tactical Movement", "At the start of your Maneuver Phase spend your very first 1 CP and 3 Munitions: every Infantry may make a free move this turn.")

// British expansion Commanders (2026-11-24; Faction Guide pages 23 and 24: Elite Commanders Collection, StuG Pack, Lost Brigades Pack)
case object EnglishIndustry extends Ability("English Industry", "In the purchases, 2 Munitions may be spent in place of 1 Fuel.")
case object SextonCallIn extends Ability("Sexton Gun Carriage", "The Sexton can be bought.")
case object SextonCallInFree extends Ability("Sexton Gun Carriage", "The Sexton can be bought. One spawns for free.")
case object CometCallIn extends Ability("Comet Call-In", "The Comet can be bought. One spawns for free.")
case object PerimeterOverwatch extends Ability("Perimeter Overwatch", "At the start of your Maneuver Phase spend your very first 2 CP and 2 Munitions: 3 hexes in a line each take 2 HE in the Damage Phase.")
case object UniversalCarriers extends Ability("Royal Armored Corps", "Two Universal Carriers can spawn for 5 Munitions. One spawns for free.")
case object ConcentrationBarrage extends Ability("Concentration Barrage", "At the start of your Maneuver Phase spend your very first 1 CP and 1 Munitions: a hex takes 1 HE in the Damage Phase.")
case object FireflyCallIn extends Ability("Firefly Call-In", "The Sherman Firefly can be bought; Tulip: it may deal 3 HE instead of its attack for 2 Munitions.")
case object FireflyCallInFree extends Ability("Firefly Call-In", "The Sherman Firefly can be bought; Tulip: it may deal 3 HE instead of its attack for 2 Munitions. One spawns for free.")
case object TankHunters extends Ability("Tank Hunters", "The Infantry Sections' AP Munitions upgrade costs 2 less.")
case object BoysATRifles extends Ability("Boys AT Rifles", "Infantry Sections with their Munitions AP upgrade also gain +1 Sight and Range. One Infantry Section spawns for free.")
case object HawkerTyphoon extends Ability("Hawker Typhoon Anti-Tank Support", "At the start of your Maneuver Phase spend your very first 2 CP and 2 Munitions: 3 hexes in a line each take 2 AP in the Damage Phase.")
case object CommandosSpawn extends Ability("Commandos", "Commandos spawn on any visible non-Building hex, Double Slow, for 5 Manpower and 1 Munitions; one a round: 3 health, range 2, 1 AI, Close Range Firepower.")
case object StaghoundCallInPlain extends Ability("Staghound Call-In", "The Staghound can be bought.")
case object GammonBombs extends Ability("Gammon Bombs", "Commandos have Camouflage, and may deal 3 HE to an adjacent target instead of their attack. One spawns for free (it counts toward the one a round).")
case object VeteranCentaurs extends Ability("Veteran Centaurs", "Centaurs have their XP and Munitions upgrades. One spawns for free.")
case object ValentineCallIn extends Ability("Valentine Mk. XI", "The Valentine can be bought: a Defense Die when Assaulting.")
case object ValentineCallInFree extends Ability("Valentine Mk. XI", "The Valentine can be bought: a Defense Die when Assaulting. One spawns for free.")
case object InfantryTankSupport extends Ability("Infantry Tank Support", "Infantry gain +2 Defense Dice next to a Heavy Vehicle (not in a Building, no stacking with other Cover). One Infantry Section spawns for free.")
case object ChurchillAVRECallInFree extends Ability("Churchill AVRE", "The Churchill AVRE can be bought (a Churchill for the limit). One spawns for free.")
case object APSniperCallInPlain extends Ability(".55 cal Armor Piercing Sniper", "The .55 cal Armor Piercing Sniper can be bought.")
case object HeroicCharge extends Ability("Heroic Charge", "At the start of your Maneuver Phase spend your very first 1 CP and 3 Munitions: every Infantry may make a free move this turn. One Infantry Section spawns for free.")
case object RoyalEngineerSpawnPlain extends Ability("Vanguard Operations", "Infantry Sections may spawn as Royal Engineers.")

// Soviet expansion Commanders (2026-11-24; Faction Guide pages 29 and 30: Elite Commanders Collection, Eastern Reinforcements Pack, StuG
// Pack, Lost Brigades Pack)
case object SecondWave extends Ability("2nd Wave", "Infantry can upgrade to Heroic Sacrifice for 3 Munitions; Infantry with Heroic Sacrifice +1 Defense Die when Assaulting.")
case object ShockTroopsCallIn extends Ability("Shock Troops", "Shock Troops can be bought: 5 health, range 2, 1 AI, Close Range Firepower, a Defense Die; Assaulting, they may pay 1 Munitions to Smoke the Assaulted hex at the start of the Damage Phase. One spawns for free.")
case object IS2CallIn extends Ability("IS-2 Heavy Tank Call-In", "The IS-2 can be bought.")
case object IS2Shells extends Ability("IS-2 122mm Shells", "The IS-2s' AP damage hurts Buildings, and Infantry in a Building it fires at get 1 less Defense Die. One IS-2 spawns for free.")
case object GuardsRifleFree extends Ability("Guards Rifle Infantry", "Guards Rifle Infantry can be bought: 4 health, range 3 only, 2 AI or 2 AP, picked each attack. One spawns for free.")
case object GuardsRifleCallIn extends Ability("Guards Rifle Infantry", "Guards Rifle Infantry can be bought: 4 health, range 3 only, 2 AI or 2 AP, picked each attack.")
case object SovietIndustryScout extends Ability("Soviet Industry", "Pay 1 Manpower: Vehicles cost 1 less Fuel this Supply Phase. One Scout Car spawns for free.")
case object SovietIndustryT70 extends Ability("Soviet Industry", "Pay 1 Manpower: Vehicles cost 1 less Fuel this Supply Phase. One T-70 spawns for free.")
case object AIPartisansSpawn extends Ability("Anti-Infantry Partisans", "Anti-Infantry Partisans (2 health, range 2, 1 AI, Heroic Sacrifice) spawn next to an empty Building, Slow the first round, for 1 Manpower and 4 Munitions; one a round.")
case object ATPartisansSpawn extends Ability("Anti-Tank Partisans", "Anti-Tank Partisans (2 health, range 2, 1 AP, Heroic Sacrifice) spawn next to an empty Building, Slow the first round, for 1 Manpower and 4 Munitions; one a round.")
case object ATPartisansSpawnFree extends Ability("Anti-Tank Partisans", "Anti-Tank Partisans (2 health, range 2, 1 AP, Heroic Sacrifice) spawn next to an empty Building, Slow the first round, for 1 Manpower and 4 Munitions; one a round. One spawns for free (it counts toward the one a round).")
case object ForwardStationAbility extends Ability("Forward Station", "Conscripts can make an empty Building next to them a Forward Station (3 health, a Wrench) for 2 Manpower; Vehicles may spawn next to it, Slow the first round. One at a time.")
case object KV2CallIn extends Ability("KV-2 Assault Gun Call-In", "The KV-2 can be bought. One spawns for free.")
case object RallyPoint extends Ability("Rally Point", "After spending a first CP on an Infantry, pay 1 Munitions to move it once more for free.")
case object PenalBattalionFree extends Ability("Penal Battalion", "Penal Battalions can be bought: 5 health, a Defense Die, 2 AI at range 2 or 2 HE at range 1, picked each attack; they can't be Pinned. One spawns for free.")
case object PenalBattalionCallIn extends Ability("Penal Battalion", "Penal Battalions can be bought: 5 health, a Defense Die, 2 AI at range 2 or 2 HE at range 1, picked each attack; they can't be Pinned.")
case object ReconOverflight extends Ability("Recon Overflight", "At the start of your Maneuver Phase spend your very first 1 CP and 1 Munitions: a hex and the hexes around it are visible to your team this round, Camouflage too.")
case object ISU152CallIn extends Ability("ISU-152 Call-In", "The ISU-152 can be bought: 3 AP or 3 HE, picked each attack.")
case object ISU152CallInFree extends Ability("ISU-152 Call-In", "The ISU-152 can be bought: 3 AP or 3 HE, picked each attack. One spawns for free.")
case object ISUFactoryMods extends Ability("ISU-152 Factory Modifications", "An ISU-152 can pay 3 Munitions to lose its Slow for good and gain a Defense Die. One ISU-152 spawns for free.")
case object Decoy extends Ability("Decoy", "Spending the third CP on a Sniper or Guards Rifle Infantry, it may go No-Fire this round to move 2 more hexes.")
case object UrbanSharpshooters extends Ability("Urban Sharpshooters", "Guards Rifle Infantry and Snipers can upgrade to a Defense Die for 2 Munitions. One Sniper spawns for free.")
case object SovietArchitecture extends Ability("Soviet Architecture", "Infantry get +1 Defense Die in a Building, and the Buildings they are in take 1 less damage. One Anti-Infantry Partisans spawns for free (it counts toward the one a round).")
case object ConscriptPacks extends Ability("Conscript Engineer Packs", "Conscripts gain +1 Defense Die when Sandbags give them cover.")
case object ForMotherRussia extends Ability("For Mother Russia", "At the start of your Maneuver Phase spend your very first 1 CP and 3 Munitions: every Infantry may make a free move this turn.")
case object RapidConscriptionPenal extends Ability("Rapid Conscription", "At the start of your Maneuver Phase spend your very first 2 CP and 1 Munitions: this round each Damage to your Infantry gives you 1 Manpower. One Penal Battalion spawns for free.")
case object BattlefieldPromotions extends Ability("Battlefield Promotions", "Conscripts can become Shock Troops or Penal Battalions for 4 Munitions (5 health at most, their damage kept); Penal Battalions and Shock Troops gain a Speed Die and a Wrench and may buy the Conscripts' Flame upgrade.")
case object KatyushaStrike extends Ability("Katyusha Rocket Strike", "At the start of your Maneuver Phase spend your very first 2 CP and 2 Munitions: 3 hexes in a line each take 2 HE in the Damage Phase.")
case object KV8CallIn extends Ability("KV-8 Flame Tank", "The KV-8 can be bought: 1 more Flame when Assaulting or firing at a unit in a Building.")
case object KV8CallInFree extends Ability("KV-8 Flame Tank", "The KV-8 can be bought: 1 more Flame when Assaulting or firing at a unit in a Building. One spawns for free.")
case object T3485CallInPlain extends Ability("T-34-85 Medium Tank", "The T-34-85 can be bought (a T-34 for the limit).")
case object DShKCallInPlain extends Ability("DShK 38 Machine Gun", "The DShK 38 can be bought.")
case object ML20Barrage extends Ability("ML-20 152mm Light Barrage", "At the start of your Maneuver Phase spend your very first 1 CP and 1 Munitions: a hex takes 1 HE in the Damage Phase.")

// OKW expansion Commanders (2026-11-24; Faction Guide pages 36 and 37: Elite Commanders Collection, StuG Pack, Lost Brigades Pack)
case object TerrorWestOber extends Ability("Terror Doctrine West", "Obersoldaten may also spawn next to the Tier 2 Building (still needing Tier 3). Tier 3 costs 1 less Manpower.")
case object RecoupLosses extends Ability("Recoup Losses", "At the start of your Maneuver Phase spend your very first 2 CP and 1 Munitions: this round each Damage to your Infantry gives you 1 Manpower.")
case object OberDefense extends Ability("Ober Defense", "Once Damage is assigned, each targeted Obersoldaten squad may pay X Munitions for +X Defense Dice this round. One Obersoldaten spawns for free.")
case object PanzerfusilierCallInPlain extends Ability("Panzerfusiliers", "Panzerfusiliers can be bought.")
case object VeteranKettenkrad extends Ability("Storm Doctrine", "Kettenkrads can be bought, Veteran: 2 health and Camouflage. One spawns for free.")
case object HetzerCallIn extends Ability("Flammpanzer 38 Hetzer", "The Hetzer can be bought; Assaulting, it may deal its damage to two targets. One spawns for free.")
case object HetzerCallInPlain extends Ability("Flammpanzer 38 Hetzer", "The Hetzer can be bought; Assaulting, it may deal its damage to two targets.")
case object EncirclementVolks extends Ability("Encirclement Doctrine", "Volksgrenadiers have their 2 extra squad members upgrade (+2 health). One spawns for free.")
case object OKWBreakthrough extends Ability("Breakthrough Tactics", "After spending a first CP on a Vehicle, pay 2 Munitions to move it once more for free. One Volksgrenadier and one Hetzer spawn for free.")
case object JaegerCallInPlain extends Ability("Jaeger Recon Squad", "Jaeger Recon Squads can be bought.")
case object OKWPak43Ability extends Ability("Pak 43 Emplacement", "Sturmpioneers can build a Pak 43 Emplacement for 4 Manpower.")
case object SmokeCanistersStuka extends Ability("Smoke Canisters", "Vehicles may pay 2 Munitions to Smoke their hex at the start of the Damage Phase. One Stuka Halftrack spawns for free.")

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

case object AdvancedEmplacementsCommander extends Commander("advanced-emplacements", "Advanced Emplacements", British, $(Tier(AdvancedEmplacements, 1), Tier(ForwardEmplacements, 2), Tier(SmokeEmplacement, 4, 2), Tier(SeventeenPounderAbility, 3)))
case object RoyalEngineerRegiment extends Commander("royal-engineer", "Royal Engineer Regiment", British, $(Tier(RoyalEngineerSpawn, 3), Tier(VehicleCrewRepairs, 2), Tier(ChurchillAVRECallIn, 1, 3), Tier(EmergencyWarSpeed, 5, 3)))
case object CommandoRegiment extends Commander("commando", "Commando Regiment", British, $(Tier(CovertTraining, 3), Tier(ForwardEmplacements, 2), Tier(StaghoundCallIn, 4, 2), Tier(CommandoGlider, 3)))
case object LendLeaseTactics extends Commander("lend-lease", "Lend Lease Assault Tactics", British, $(Tier(ThompsonSMGs, 2), Tier(StaghoundCallIn, 4, 2), Tier(InfiltrationCommandos, 2), Tier(VeteranStaghounds, 4, 2)))
case object SpecialWeaponsDoctrine extends Commander("special-weapons", "Special Weapons Doctrine", British, $(Tier(APSniperCallIn, 3), Tier(HoldTheLine, 1), Tier(ForwardEmplacements, 2), Tier(OperationCrocodile, 4, 3)))
case object InfantrySupportRegiment extends Commander("infantry-support", "Infantry Support Regiment", British, $(Tier(InfantrySupport, 1), Tier(WASPFlamethrower, 4), Tier(BolsterInfantry, 2), Tier(VeteranCromwells, 4, 3)))

case object AssaultTactics extends Commander("assault-tactics", "Assault Tactics", Soviet, $(Tier(SupplyOperations, 1), Tier(SupplyRecon, 3), Tier(T34Wave, 4, 3), Tier(RammingManeuver, 1)))
case object SovietRangedTactics extends Commander("ranged-tactics", "Soviet Ranged Tactics", Soviet, $(Tier(RangedTactics, 1), Tier(DShKCallIn, 3), Tier(ATAmbushTactics, 4, 2), Tier(VeteranSU76, 2, 3)))
case object EliteArtilleryTactics extends Commander("elite-artillery", "Elite Artillery Tactics", Soviet, $(Tier(ArtilleryEye, 1), Tier(VeteranMortars, 3), Tier(T34Wave, 2, 3), Tier(WaveAndShell, 5, 3)))
case object ConscriptSupportTactics extends Commander("conscript-support", "Conscript Support Tactics", Soviet, $(Tier(ConscriptSupport, 3), Tier(HitTheDirt, 1), Tier(RapidConscription, 2), Tier(T3485CallIn, 4, 3)))
case object NKVDDisruptionTactics extends Commander("nkvd", "NKVD Disruption Tactics", Soviet, $(Tier(NKVDConscripts, 3), Tier(ForwardHQAbility, 2), Tier(IL2Strike, 1), Tier(VeteranSU85, 4, 3)))
case object ArmoredAssaultTactics extends Commander("armored-assault", "Armored Assault Tactics", Soviet, $(Tier(ReconMode, 1), Tier(T70InfantryTank, 4, 2), Tier(SmokeAmbush, 2), Tier(T80Turret, 4, 2)))

// the US expansion Commanders (Faction Guide pages 11 and 12)
case object LightCavalryCompany extends Commander("light-cavalry", "Light Cavalry Company", US, $(Tier(LightInfantryTraining, 1), Tier(StuartCallIn, 1, 2), Tier(StrategicManeuvers, 3), Tier(BrowningM2, 4, 2)))
case object HeavyCavalryCompany extends Commander("heavy-cavalry", "Heavy Cavalry Company", US, $(Tier(RiflemenFieldDefenses, 1), Tier(CombinedArms, 2), Tier(RequisitionOfficer, 3), Tier(PershingCallIn, 5, 3)))
case object TacticalSupportCompany extends Commander("tactical-support", "Tactical Support Company", US, $(Tier(USMGBunkers, 1), Tier(StuartCallInFree, 4, 2), Tier(P47Strafing, 1), Tier(CalliopeCallIn, 4, 3)))
case object MotorPoolCompany extends Commander("motor-pool", "Motor Pool", US, $(Tier(MotorPool, 3), Tier(CalliopeCallInPlain, 1, 3), Tier(PershingCallInPlain, 1, 3), Tier(Outmaneuver, 5, 3)))
case object PathfinderCompany extends Commander("pathfinder", "Pathfinder Company", US, $(Tier(PathfinderSquads, 1), Tier(AirdropBeacon, 4), Tier(Mortars81mm, 3), Tier(AirdroppedCombatGroup, 3, 3)))
case object DevilsBrigadeCompany extends Commander("devils-brigade", "Devil's Brigade", US, $(Tier(DevilsBrigadeSpawn, 3), Tier(DevilsAmbush, 1), Tier(Demolition, 1), Tier(Sherman76, 4, 3)))
case object CavalryRifleCompany extends Commander("cavalry-rifle", "Cavalry Rifle Company", US, $(Tier(StepOnIt, 1), Tier(CavalryRiflemenCallIn, 3), Tier(EasyEightCallInPlain, 1, 3), Tier(DozerCallIn, 5, 3)))
case object UrbanAssaultCompany extends Commander("urban-assault", "Urban Assault Company", US, $(Tier(UrbanAssault, 3), Tier(HowitzerBarrage, 1), Tier(JacksonCallInPlain, 2, 3), Tier(DozerCallIn, 5, 3)))
case object EngineerAssaultCompany extends Commander("engineer-assault", "Engineer Assault Company", US, $(Tier(AssaultEngineerSpawnPlain, 1), Tier(SupportJeeps, 3), Tier(DozerCallInPlain, 2, 3), Tier(JacksonCallIn, 5, 3)))

// the Wehrmacht expansion Commanders (Faction Guide pages 17 and 18)
case object StuGAssaultDoctrine extends Commander("stug-assault", "StuG Assault Doctrine", Wehrmacht, $(Tier(StuGCloseRange, 1), Tier(StuGCallIn, 1, 2), Tier(TankDesant, 3), Tier(TargetWeakPoint, 4, 2)))
case object StuGTactics extends Commander("stug-tactics", "StuG Tactics", Wehrmacht, $(Tier(StormtrooperSpawn, 2), Tier(StuGCallInFree, 4, 2), Tier(FragmentationBomb, 1), Tier(TurretlessTerror, 3)))
case object TerrorDoctrine extends Commander("terror", "Terror Doctrine", Wehrmacht, $(Tier(LMG42Free, 3), Tier(WMBreakthrough, 1), Tier(StukaStrafe, 1), Tier(KingTigerCallIn, 5, 3)))
case object TankDestroyerDoctrine extends Commander("tank-destroyer", "Tank Destroyer Doctrine", Wehrmacht, $(Tier(MarkVehicle, 1), Tier(JaegerSniperFree, 3), Tier(Pak43Emplacement, 1), Tier(JagdpantherCallIn, 4, 3)))
case object AssaultSupportDoctrine extends Commander("assault-support", "Assault Support Doctrine", Wehrmacht, $(Tier(AssaultSupport, 1), Tier(SupplyConvoy, 3), Tier(ReconPlane, 1), Tier(ElefantCallIn, 5, 3)))
case object InfantryStormDoctrine extends Commander("infantry-storm", "Infantry Storm Doctrine", Wehrmacht, $(Tier(JaegerSniperCallIn, 1), Tier(HalftrackCallInFree, 4, 2), Tier(FlameProjectors, 4, 2), Tier(StukaStrafe, 1)))
case object FestungSupportDoctrine extends Commander("festung-support", "Festung Support Doctrine", Wehrmacht, $(Tier(OsttruppenSquads, 3), Tier(HalftrackCallIn, 1, 2), Tier(HullDown, 1), Tier(Pak251, 4, 2)))
case object LightningWarDoctrine extends Commander("lightning-war", "Lightning War Doctrine", Wehrmacht, $(Tier(OsttruppenSquadsPlain, 1), Tier(TacticalMovement, 1), Tier(HalftrackCallInFree, 4, 2), Tier(TigerCallIn, 5, 3)))

// the British expansion Commanders (Faction Guide pages 23 and 24)
case object HammerDoctrine extends Commander("hammer", "Hammer Doctrine", British, $(Tier(EnglishIndustry, 1), Tier(SextonCallInFree, 4, 2), Tier(ForwardEmplacements, 2), Tier(CometCallIn, 5, 3)))
case object RoyalArtilleryRegiment extends Commander("royal-artillery", "Royal Artillery Regiment", British, $(Tier(APSniperCallIn, 3), Tier(SextonCallIn, 1, 2), Tier(PerimeterOverwatch, 1), Tier(SeventeenPounderAbility, 3)))
case object RoyalArmoredCorps extends Commander("royal-armored", "Royal Armored Corps", British, $(Tier(UniversalCarriers, 3), Tier(ConcentrationBarrage, 1), Tier(FireflyCallIn, 1, 3), Tier(CometCallIn, 5, 3)))
case object TankHunterRegiment extends Commander("tank-hunter", "Tank Hunter Regiment", British, $(Tier(TankHunters, 1), Tier(BoysATRifles, 3), Tier(HawkerTyphoon, 1), Tier(FireflyCallInFree, 4, 3)))
case object SpecialServiceBrigade extends Commander("special-service", "Special Service Brigade", British, $(Tier(CommandosSpawn, 2), Tier(StaghoundCallInPlain, 1, 2), Tier(GammonBombs, 3), Tier(VeteranCentaurs, 4, 3)))
case object AssaultInfantryRegiment extends Commander("assault-infantry", "Assault Infantry Regiment", British, $(Tier(ThompsonSMGs, 2), Tier(ValentineCallIn, 1, 2), Tier(InfantryTankSupport, 3), Tier(ChurchillAVRECallInFree, 5, 3)))
case object AssaultOfficerRegiment extends Commander("assault-officer", "Assault Officer Regiment", British, $(Tier(APSniperCallInPlain, 1), Tier(ValentineCallIn, 1, 2), Tier(HeroicCharge, 3), Tier(OperationCrocodile, 5, 3)))
case object VanguardOperationsRegiment extends Commander("vanguard", "Vanguard Operations Regiment", British, $(Tier(RoyalEngineerSpawnPlain, 1), Tier(ValentineCallInFree, 4, 2), Tier(ForwardEmplacements, 2), Tier(SeventeenPounderAbility, 3)))

// the Soviet expansion Commanders (Faction Guide pages 29 and 30)
case object FrontalAssaultDoctrine extends Commander("frontal-assault", "Frontal Assault Doctrine", Soviet, $(Tier(SecondWave, 1), Tier(ShockTroopsCallIn, 3), Tier(IS2CallIn, 1, 3), Tier(IS2Shells, 5, 3)))
case object GuardsMotorTactics extends Commander("guards-motor", "Guards Motor Tactics", Soviet, $(Tier(MarkVehicle, 1), Tier(GuardsRifleFree, 3), Tier(IS2CallIn, 1, 3), Tier(IS2Shells, 5, 3)))
case object UrbanDefenseTactics extends Commander("urban-defense", "Urban Defense Tactics", Soviet, $(Tier(SovietIndustryScout, 3), Tier(AIPartisansSpawn, 2), Tier(ForwardStationAbility, 2), Tier(KV2CallIn, 4, 3)))
case object UrbanAssaultTactics extends Commander("urban-assault-tactics", "Urban Assault Tactics", Soviet, $(Tier(RallyPoint, 1), Tier(ForwardHQAbility, 2), Tier(ATPartisansSpawnFree, 3), Tier(KV2CallIn, 4, 3)))
case object CounterattackTactics extends Commander("counterattack", "Counterattack Tactics", Soviet, $(Tier(PenalBattalionFree, 3), Tier(ReconOverflight, 1), Tier(ISU152CallIn, 1, 3), Tier(ISUFactoryMods, 5, 3)))
case object HeroesOfStalingrad extends Commander("stalingrad", "Heroes of Stalingrad", Soviet, $(Tier(GuardsRifleCallIn, 1), Tier(Decoy, 1), Tier(UrbanSharpshooters, 3, 2), Tier(ISU152CallInFree, 5, 3)))
case object SiegeMilitia extends Commander("siege-militia", "Siege Militia", Soviet, $(Tier(AIPartisansSpawn, 2), Tier(SovietArchitecture, 2), Tier(ATPartisansSpawn, 2), Tier(ISU152CallInFree, 5, 3)))
case object FrontlineTactics extends Commander("frontline", "Frontline Tactics", Soviet, $(Tier(ConscriptPacks, 1), Tier(ForMotherRussia, 1), Tier(SovietIndustryT70, 4, 2), Tier(ISU152CallInFree, 5, 3)))
case object SpetsnazBrigade extends Commander("spetsnaz", "Soviet Spetsnaz Brigade", Soviet, $(Tier(PenalBattalionCallIn, 1), Tier(ShockTroopsCallIn, 3), Tier(RapidConscriptionPenal, 4), Tier(BattlefieldPromotions, 2)))
case object TerrorTactics extends Commander("terror-tactics", "Terror Tactics", Soviet, $(Tier(GuardsRifleFree, 3), Tier(KatyushaStrike, 1), Tier(KV8CallIn, 1, 3), Tier(T3485CallIn, 4, 3)))
case object ShockRifleTactics extends Commander("shock-rifle", "Shock Rifle Tactics", Soviet, $(Tier(DShKCallInPlain, 1), Tier(ML20Barrage, 1), Tier(ATAmbushTactics, 4, 2), Tier(KV8CallInFree, 4, 3)))
case object SovietIndustryTactics extends Commander("soviet-industry", "Soviet Industry Tactics", Soviet, $(Tier(DShKCallIn, 3), Tier(ForwardStationAbility, 2), Tier(T3485CallInPlain, 1, 3), Tier(KV8CallInFree, 4, 3)))

// the OKW expansion Commanders (Faction Guide pages 36 and 37); the StuG Pack's two are the Wehrmacht's cards
case object TerrorDoctrineWest extends Commander("terror-west", "Terror Doctrine West", OKW, $(Tier(TerrorWestOber, 1), Tier(RecoupLosses, 2), Tier(OberDefense, 4, 3), Tier(KingTigerCallIn, 5, 3)))
case object TankDestroyerWest extends Commander("tank-destroyer-west", "Tank Destroyer West", OKW, $(Tier(MarkVehicle, 1), Tier(PanzerfusilierCallInPlain, 1), Tier(PumaCallIn, 4, 2), Tier(JagdpantherCallIn, 4, 3)))
case object StuGAssaultDoctrineOKW extends Commander("stug-assault-okw", "StuG Assault Doctrine", OKW, StuGAssaultDoctrine.tiers)
case object StuGTacticsOKW extends Commander("stug-tactics-okw", "StuG Tactics", OKW, StuGTactics.tiers)
case object StormDoctrine extends Commander("storm", "Storm Doctrine", OKW, $(Tier(VeteranKettenkrad, 3), Tier(LeFH18Ability, 1), Tier(StukaStrafe, 1), Tier(HetzerCallIn, 4, 3)))
case object EncirclementDoctrine extends Commander("encirclement", "Encirclement Doctrine", OKW, $(Tier(EncirclementVolks, 3), Tier(FragmentationBomb, 1), Tier(HetzerCallInPlain, 1, 3), Tier(OKWBreakthrough, 5)))
case object JointOperationsDoctrine extends Commander("joint-operations", "Joint Operations Doctrine", OKW, $(Tier(JaegerCallInPlain, 1), Tier(OKWPak43Ability, 1), Tier(SmokeCanistersStuka, 4, 2), Tier(HetzerCallIn, 4, 3)))

case object BreakthroughDoctrine extends Commander("breakthrough", "Breakthrough Doctrine", OKW, $(Tier(WillNotDieOld, 1), Tier(PanzerfusilierCallIn, 3), Tier(BreakthroughTactics, 3), Tier(PantherReinforcements, 2, 3)))
case object ScavengeDoctrine extends Commander("scavenge", "Scavenge Doctrine", OKW, $(Tier(ThoroughSalvage, 1), Tier(JaegerCallIn, 3), Tier(PumaCallIn, 4, 2), Tier(VeteranLeFH, 2)))
case object EliteArmoredDoctrine extends Commander("elite-armored", "Elite Armored Doctrine", OKW, $(Tier(KettenkradCallIn, 1), Tier(HeavyLuchs, 4, 2), Tier(GrandAssault, 4), Tier(SturmtigerReinforcements, 2, 3)))
case object FortificationsDoctrine extends Commander("fortifications", "Fortifications Doctrine", OKW, $(Tier(FortifiedCamo, 1), Tier(LeFH18Ability, 1), Tier(ScoutLuchs, 4, 2), Tier(VeteranPak43, 4)))
case object FirestormDoctrine extends Commander("firestorm", "Firestorm Doctrine", OKW, $(Tier(Flammenwerfer, 2), Tier(KettenkradCallIn, 1), Tier(PumaCallIn, 4, 2), Tier(RocketSupport, 4, 2)))
case object GrandOffenseDoctrine extends Commander("grand-offense", "Grand Offense Doctrine", OKW, $(Tier(StG44Volks, 1), Tier(PumaCallIn, 1, 2), Tier(KettenkradEngineers, 3), Tier(HEATShells, 5, 3)))

object Commander {
    val all = $[Commander](ReconSupportCompany, RangerCompany, ArmorCompany, MechanizedCompany, AirborneCompany, MobileSupportCompany,
        JaegerDoctrine, SpearheadDoctrine, MechanizedDoctrine, FestungDoctrine, PanzerkorpsDoctrine, EliteTroopsDoctrine,
        AdvancedEmplacementsCommander, RoyalEngineerRegiment, CommandoRegiment, LendLeaseTactics, SpecialWeaponsDoctrine, InfantrySupportRegiment,
        AssaultTactics, SovietRangedTactics, EliteArtilleryTactics, ConscriptSupportTactics, NKVDDisruptionTactics, ArmoredAssaultTactics,
        BreakthroughDoctrine, ScavengeDoctrine, EliteArmoredDoctrine, FortificationsDoctrine, FirestormDoctrine, GrandOffenseDoctrine,
        LightCavalryCompany, HeavyCavalryCompany, TacticalSupportCompany, MotorPoolCompany, PathfinderCompany, DevilsBrigadeCompany,
        CavalryRifleCompany, UrbanAssaultCompany, EngineerAssaultCompany,
        StuGAssaultDoctrine, StuGTactics, TerrorDoctrine, TankDestroyerDoctrine, AssaultSupportDoctrine, InfantryStormDoctrine,
        FestungSupportDoctrine, LightningWarDoctrine,
        HammerDoctrine, RoyalArtilleryRegiment, RoyalArmoredCorps, TankHunterRegiment, SpecialServiceBrigade, AssaultInfantryRegiment,
        AssaultOfficerRegiment, VanguardOperationsRegiment,
        FrontalAssaultDoctrine, GuardsMotorTactics, UrbanDefenseTactics, UrbanAssaultTactics, CounterattackTactics, HeroesOfStalingrad,
        SiegeMilitia, FrontlineTactics, SpetsnazBrigade, TerrorTactics, ShockRifleTactics, SovietIndustryTactics,
        TerrorDoctrineWest, TankDestroyerWest, StuGAssaultDoctrineOKW, StuGTacticsOKW, StormDoctrine, EncirclementDoctrine, JointOperationsDoctrine)

    // the expansion Commanders (the setup's Expansion Commanders option)
    val expansion = $[Commander](LightCavalryCompany, HeavyCavalryCompany, TacticalSupportCompany, MotorPoolCompany, PathfinderCompany,
        DevilsBrigadeCompany, CavalryRifleCompany, UrbanAssaultCompany, EngineerAssaultCompany,
        StuGAssaultDoctrine, StuGTactics, TerrorDoctrine, TankDestroyerDoctrine, AssaultSupportDoctrine, InfantryStormDoctrine,
        FestungSupportDoctrine, LightningWarDoctrine,
        HammerDoctrine, RoyalArtilleryRegiment, RoyalArmoredCorps, TankHunterRegiment, SpecialServiceBrigade, AssaultInfantryRegiment,
        AssaultOfficerRegiment, VanguardOperationsRegiment,
        FrontalAssaultDoctrine, GuardsMotorTactics, UrbanDefenseTactics, UrbanAssaultTactics, CounterattackTactics, HeroesOfStalingrad,
        SiegeMilitia, FrontlineTactics, SpetsnazBrigade, TerrorTactics, ShockRifleTactics, SovietIndustryTactics,
        TerrorDoctrineWest, TankDestroyerWest, StuGAssaultDoctrineOKW, StuGTacticsOKW, StormDoctrine, EncirclementDoctrine, JointOperationsDoctrine)

    // the core six of a faction (every game without the option, and every game made before it)
    def of(f : Faction) = all.diff(expansion).%(_.faction == f)

    // the Commanders a player draws from
    def pool(f : Faction, options : $[BaseOption]) = options.has(ExpansionCommanders).?(all.%(_.faction == f)).|(of(f))

    // the abilities spent at the start of a Maneuver Phase: CP, Munitions
    val opening = Map[Ability, (Int, Int)](ReconSweep -> (1, 1), P47Strike -> (2, 2), CombatGroupAdvance -> (1, 3), PanzerwerferBarrage -> (1, 1), FragmentationBomb -> (2, 2), ArmorBlitzkrieg -> (1, 3),
        ArtilleryEye -> (1, 1), IL2Strike -> (2, 2), BreakthroughTactics -> (1, 3), GrandAssault -> (1, 3), RapidConscription -> (2, 1), FortifiedCamo -> (1, 1),
        CombinedArms -> (2, 2), P47Strafing -> (2, 2), Outmaneuver -> (1, 3), HowitzerBarrage -> (1, 1),
        StukaStrafe -> (2, 2), MarkVehicle -> (1, 2), ReconPlane -> (1, 1), TacticalMovement -> (1, 3),
        PerimeterOverwatch -> (2, 2), ConcentrationBarrage -> (1, 1), HawkerTyphoon -> (2, 2), HeroicCharge -> (1, 3),
        ReconOverflight -> (1, 1), ForMotherRussia -> (1, 3), RapidConscriptionPenal -> (2, 1), KatyushaStrike -> (2, 2), ML20Barrage -> (1, 1),
        RecoupLosses -> (2, 1))

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
        case RoyalEngineerSpawn => $(RoyalEngineers)
        case EmergencyWarSpeed => $(ChurchillAVRE)
        case StaghoundCallIn | VeteranStaghounds => $(Staghound)
        case APSniperCallIn => $(APSniper)
        case OperationCrocodile => $(Churchill)
        case VeteranCromwells => $(Cromwell)
        case SupplyRecon => $(ScoutCar)
        case T34Wave => $(T34)
        case WaveAndShell => $(SovietMortar, T34)
        case DShKCallIn => $(DShK)
        case ATAmbushTactics => $(ZiS3)
        case VeteranMortars => $(SovietMortar)
        case ConscriptSupport | NKVDConscripts => $(Conscripts)
        case T3485CallIn => $(T3485)
        case VeteranSU85 => $(SU85)
        case T70InfantryTank | T80Turret => $(T70)
        case PanzerfusilierCallIn => $(Panzerfusilier)
        case BreakthroughTactics => $(Volksgrenadiers)
        case JaegerCallIn => $(JaegerRecon)
        case PumaCallIn => $(Puma)
        case HeavyLuchs | ScoutLuchs => $(Luchs)
        case GrandAssault | KettenkradEngineers => $(Kettenkrad)
        case RocketSupport => $(StukaHalftrack)
        case HEATShells => $(Panther)
        case StuartCallInFree | BrowningM2 => $(Stuart)
        case StrategicManeuvers | RequisitionOfficer | UrbanAssault => $(Riflemen)
        case PershingCallIn | Outmaneuver => $(Pershing)
        case CalliopeCallIn => $(Calliope)
        case MotorPool | SupportJeeps => $(Jeep)
        case AirdropBeacon => $(Pathfinder)
        case Mortars81mm => $(USMortar)
        case DevilsBrigadeSpawn => $(DevilsBrigade)
        case Sherman76 => $(Sherman)
        case CavalryRiflemenCallIn => $(CavalryRiflemen)
        case DozerCallIn => $(Dozer)
        case JacksonCallIn => $(Jackson)
        case StuGCallInFree | TargetWeakPoint => $(StuG)
        case TankDesant => $((f == OKW).?(Volksgrenadiers : UnitType).|(Grenadiers))
        case LMG42Free => $(Grenadiers)
        case OberDefense => $(Obersoldaten)
        case VeteranKettenkrad => $(Kettenkrad)
        case HetzerCallIn => $(Hetzer)
        case EncirclementVolks => $(Volksgrenadiers)
        case OKWBreakthrough => $(Volksgrenadiers, Hetzer)
        case SmokeCanistersStuka => $(StukaHalftrack)
        case TurretlessTerror => $(Stormtroopers)
        case KingTigerCallIn => $(KingTiger)
        case JaegerSniperFree => $(JaegerSniper)
        case JagdpantherCallIn => $(Jagdpanther)
        case SupplyConvoy => $(Kubelwagen)
        case ElefantCallIn => $(Elefant)
        case HalftrackCallInFree => $(Halftrack)
        case SextonCallInFree => $(Sexton)
        case CometCallIn => $(Comet)
        case UniversalCarriers => $(UniversalCarrier)
        case FireflyCallInFree => $(Firefly)
        case BoysATRifles | InfantryTankSupport | HeroicCharge => $(InfantrySection)
        case VeteranCentaurs => $(Centaur)
        case ValentineCallInFree => $(Valentine)
        case ChurchillAVRECallInFree => $(ChurchillAVRE)
        case ShockTroopsCallIn => $(ShockTroops)
        case IS2Shells => $(IS2)
        case GuardsRifleFree => $(GuardsRifle)
        case SovietIndustryScout => $(ScoutCar)
        case SovietIndustryT70 => $(T70)
        case KV2CallIn => $(KV2)
        case PenalBattalionFree | RapidConscriptionPenal => $(PenalBattalion)
        case ISU152CallInFree | ISUFactoryMods => $(ISU152)
        case UrbanSharpshooters => $(SniperSquad)
        case KV8CallInFree => $(KV8)
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

// the expansion packs' Commanders (Elite Commanders Collection, Pathfinder, StuG, Eastern Reinforcements, Lost Brigades), drawn with the
// core ones (2026-11-24); only with Draw three Commanders
case object ExpansionCommanders extends GameOption with ToggleOption {
    val group = CommandersChoice.group
    def valueOn = "Expansion Commanders".hl
    override val explain = $[Elem]("The players also draw from the expansion packs' Commanders (Elite Commanders Collection, Pathfinder, StuG and Lost Brigades packs, from the Faction Guide): every faction's (44).".txt)
    override def required(all : $[BaseOption]) = $($(DrawCommanders))
}

object CommandersChoice {
    val group = "Commanders".txt

    val all = $[CommandersChoice](DrawCommanders, NoCommanders)
}

// a strike from an opening ability: the hexes it hits in the Damage Phase
case class Strike(faction : Faction, ability : Ability, hexes : $[Hex]) extends Record {
    // the dice each hex takes
    def dice : $[Attack] = ability @@ {
        case P47Strike => $(Attack(AP), Attack(AP))
        case P47Strafing => $(Attack(HE), Attack(HE))
        case StukaStrafe | HawkerTyphoon => $(Attack(AP), Attack(AP))
        case PerimeterOverwatch | KatyushaStrike => $(Attack(HE), Attack(HE))
        case ML20Barrage => $(Attack(HE))
        case ConcentrationBarrage => $(Attack(HE))
        case HowitzerBarrage => $(Attack(HE))
        case FragmentationBomb => $(Attack(HE), Attack(HE))
        case PanzerwerferBarrage => $(Attack(HE))
        case IL2Strike => $(Attack(AP), Attack(AP))
        case _ => $[Attack]()
    }
}
