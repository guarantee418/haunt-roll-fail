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
        BreakthroughDoctrine, ScavengeDoctrine, EliteArmoredDoctrine, FortificationsDoctrine, FirestormDoctrine, GrandOffenseDoctrine)

    def of(f : Faction) = all.%(_.faction == f)

    // the abilities spent at the start of a Maneuver Phase: CP, Munitions
    val opening = Map[Ability, (Int, Int)](ReconSweep -> (1, 1), P47Strike -> (2, 2), CombatGroupAdvance -> (1, 3), PanzerwerferBarrage -> (1, 1), FragmentationBomb -> (2, 2), ArmorBlitzkrieg -> (1, 3),
        ArtilleryEye -> (1, 1), IL2Strike -> (2, 2), BreakthroughTactics -> (1, 3), GrandAssault -> (1, 3), RapidConscription -> (2, 1), FortifiedCamo -> (1, 1))

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
        case IL2Strike => $(Attack(AP), Attack(AP))
        case _ => $[Attack]()
    }
}
