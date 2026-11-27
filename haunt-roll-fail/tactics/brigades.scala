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

// LOST BRIGADES (rulebook 9.0, Full Game Rules; the Faction Guide's x.3 pages and 8.0, the Universal Panther Lost Brigade; 2026-11-24):
// each player shuffles their faction's Lost Brigades (its twelve, and the universal Command Panther) and draws three at setup; once, at
// any time of the purchases, they pick one for the XP at its top left, never switching. A Lost Brigade needs no Tier Building, and its
// units can be bought any number of times up to the limits. Those of 0 XP may be picked at the start of the game, in turn order, before the
// starting Infantry, and some then give starting Infantry ("one", or "up to three", may be selected as starting infantry). They give no free
// unit. All OKW Lost Brigades spawn from the Spawn Area. A brigade gives the same abilities as the Commanders with that unit (`unlocked` in
// game.scala), at the card's own price (`cost`); readings in RULES.md.

// a Lost Brigade card: its XP, the abilities it gives, the unit it brings and that unit's price on the card, and how many may be starting Infantry
sealed abstract class Brigade(val id : String, val name : String, val faction : Faction, val xp : Int, val abilities : $[Ability], val unit : |[UnitType] = None, val price : |[Cost] = None, val starting : Int = 0) extends Record with Elementary {
    def elem = name.styled(faction)
    def image = "brigade:" + id
    def text : String = abilities.lastOption./(_.text).|("") + (starting == 1).??(" One may be selected as starting Infantry.") + (starting > 1).??(" Up to " + starting + " may be selected as starting Infantry.")
}

// US
case object LBAssaultEngineers extends Brigade("lb-assault-engineers", "Assault Engineers", US, 0, $(AssaultEngineerSpawnPlain), |(AssaultEngineers), |(Cost(4, 0, 0)), 3)
case object LBCavalryRiflemen extends Brigade("lb-cavalry-riflemen", "Cavalry Riflemen", US, 0, $(CavalryRiflemenCallIn), |(CavalryRiflemen), |(Cost(4, 1, 0)), 1)
case object LBVeteranDevils extends Brigade("lb-veteran-devils", "Veteran Devil's Brigade", US, 3, $(DevilsBrigadeSpawn, VeteranDevils), |(DevilsBrigade), |(Cost(4, 4, 0)))
case object LBUSMGBunker extends Brigade("lb-us-mg-bunker", "M2HB .50 cal Heavy Machine Gun Bunker", US, 1, $(USMGBunkers))
case object LBParadropEquipment extends Brigade("lb-paradrop", "Paradrop Equipment", US, 2, $(ParadropEquipment))
case object LBEasyEight extends Brigade("lb-easy-eight", "Sherman Easy Eight", US, 2, $(EasyEightCallInPlain), |(EasyEight), |(Cost(3, 0, 4)))
case object LBPriest extends Brigade("lb-priest", "M7B1 Priest Howitzer Motor Carriage", US, 2, $(PriestCallIn), |(Priest), |(Cost(1, 0, 3)))
case object LBStuart extends Brigade("lb-stuart", "M5A1 Stuart", US, 1, $(StuartCallIn), |(Stuart), |(Cost(2, 0, 3)))
case object LBPershing extends Brigade("lb-pershing", "Pershing", US, 3, $(PershingCallInPlain), |(Pershing), |(Cost(3, 0, 5)))
case object LBCalliope extends Brigade("lb-calliope", "Calliope", US, 2, $(CalliopeCallInPlain), |(Calliope), |(Cost(2, 0, 5)))
case object LBJackson extends Brigade("lb-jackson", "M36 Jackson", US, 3, $(JacksonCallInPlain), |(Jackson), |(Cost(3, 0, 5)))
case object LBDozer extends Brigade("lb-dozer", "Sherman 105mm Bulldozer", US, 3, $(DozerCallInPlain), |(Dozer), |(Cost(3, 0, 5)))

// WEHRMACHT
case object LBEliteGrenadiers extends Brigade("lb-elite-grenadiers", "Elite Grenadier Squad Leaders", Wehrmacht, 2, $(EliteGrenadierLeaders), |(EliteGrenadiers), |(Cost(4, 3, 0)))
case object LBJaegerSniper extends Brigade("lb-jaeger-sniper", "Jaeger Sniper", Wehrmacht, 0, $(JaegerSniperCallIn), |(JaegerSniper), |(Cost(3, 2, 0)), 1)
case object LBMG42Bunker extends Brigade("lb-mg42-bunker", "MG42 Heavy Machine Gun Bunker", Wehrmacht, 1, $(FestungBunkers))
case object LBOsttruppen extends Brigade("lb-osttruppen", "Osttruppen", Wehrmacht, 1, $(OsttruppenSquadsPlain), |(Osttruppen), |(Cost(4, 1, 0)))
case object LBPak43 extends Brigade("lb-pak43", "Pak 43 Emplacement", Wehrmacht, 1, $(Pak43Emplacement))
case object LBStormtroopers extends Brigade("lb-stormtroopers", "Veteran G43 Stormtroopers", Wehrmacht, 3, $(StormtrooperSpawn), |(Stormtroopers), |(Cost(5, 2, 0)))
case object LBTiger extends Brigade("lb-tiger", "Tiger", Wehrmacht, 3, $(TigerCallIn), |(Tiger), |(Cost(3, 0, 5)))
case object LBStuG extends Brigade("lb-stug", "StuG III Ausf. G Assault Gun", Wehrmacht, 1, $(StuGCallIn), |(StuG), |(Cost(2, 0, 3)))
case object LBKingTiger extends Brigade("lb-king-tiger", "King Tiger", Wehrmacht, 3, $(KingTigerCallIn), |(KingTiger), |(Cost(4, 0, 5)))
case object LBJagdpanther extends Brigade("lb-jagdpanther", "Jagdpanther", Wehrmacht, 2, $(JagdpantherCallIn), |(Jagdpanther), |(Cost(3, 0, 4)))
case object LBElefant extends Brigade("lb-elefant", "Elefant", Wehrmacht, 3, $(ElefantCallIn), |(Elefant), |(Cost(3, 0, 5)))
case object LBHalftrack extends Brigade("lb-halftrack", "SdKfz 251 Half-track", Wehrmacht, 2, $(HalftrackCallIn, HalftrackConfigurable), |(Halftrack), |(Cost(2, 0, 3)))

// BRITISH
case object LBSeventeenPounder extends Brigade("lb-17-pounder", "17 Pounder Emplacement", British, 2, $(SeventeenPounderAbility))
case object LBAPSniper extends Brigade("lb-ap-sniper", ".55 cal Armor-Piercing Sniper", British, 0, $(APSniperCallInPlain), |(APSniper), |(Cost(3, 2, 0)), 1)
case object LBForwardEmplacement extends Brigade("lb-forward-emplacement", "Forward Emplacement", British, 2, $(ForwardEmplacements))
case object LBCommandos extends Brigade("lb-commandos", "Veteran Commandos", British, 3, $(CommandosSpawn, GammonBombs, VeteranCommandos))
case object LBRoyalEngineers extends Brigade("lb-royal-engineers", "Royal Engineers", British, 0, $(RoyalEngineerSpawnPlain), |(RoyalEngineers), |(Cost(4, 0, 0)), 3)
case object LBStaghound extends Brigade("lb-staghound", "Staghound", British, 1, $(StaghoundCallInPlain), |(Staghound), |(Cost(2, 0, 3)))
case object LBChurchillAVRE extends Brigade("lb-churchill-avre", "Veteran Churchill AVRE", British, 3, $(ChurchillAVRECallIn), |(ChurchillAVRE), |(Cost(3, 0, 5)))
case object LBCrocodile extends Brigade("lb-crocodile", "Churchill Crocodile", British, 2, $(OperationCrocodile, CrocodileCallIn), |(Churchill), |(Cost(2, 0, 4)))
case object LBSexton extends Brigade("lb-sexton", "Sexton Gun Carriage", British, 1, $(SextonCallIn), |(Sexton), |(Cost(1, 0, 4)))
case object LBComet extends Brigade("lb-comet", "Comet", British, 3, $(CometCallIn), |(Comet), |(Cost(3, 0, 5)))
case object LBFirefly extends Brigade("lb-firefly", "Sherman Firefly", British, 2, $(FireflyCallIn), |(Firefly), |(Cost(2, 0, 5)))
case object LBValentine extends Brigade("lb-valentine", "Valentine Mk. XI", British, 1, $(ValentineCallIn), |(Valentine), |(Cost(2, 0, 3)))

// SOVIET
case object LBDShK extends Brigade("lb-dshk", "DShK 38 Heavy Machine Gun", Soviet, 0, $(DShKCallInPlain), |(DShK), |(Cost(4, 1, 0)), 1)
case object LBForwardHQ extends Brigade("lb-forward-hq", "Forward HQ", Soviet, 2, $(ForwardHQAbility))
case object LBGuardsRifle extends Brigade("lb-guards-rifle", "Guards Rifle Infantry", Soviet, 0, $(GuardsRifleCallIn), |(GuardsRifle), |(Cost(4, 1, 0)), 1)
case object LBPartisans extends Brigade("lb-partisans", "Anti-Tank / Anti-Infantry Partisans", Soviet, 2, $(AIPartisansSpawn, ATPartisansSpawn))
case object LBPenalBattalion extends Brigade("lb-penal", "Penal Battalion", Soviet, 0, $(PenalBattalionCallIn), |(PenalBattalion), |(Cost(4, 1, 0)), 1)
case object LBRepairStation extends Brigade("lb-repair-station", "Repair Station", Soviet, 2, $(ForwardStationAbility))
case object LBShockTroops extends Brigade("lb-shock-troops", "Shock Troops", Soviet, 0, $(ShockTroopsCallIn), |(ShockTroops), |(Cost(4, 1, 0)), 1)
case object LBT3485 extends Brigade("lb-t3485", "T-34-85 Medium Tank", Soviet, 2, $(T3485CallInPlain), |(T3485), |(Cost(3, 0, 4)))
case object LBIS2 extends Brigade("lb-is2", "IS-2 Heavy Tank", Soviet, 3, $(IS2CallIn), |(IS2), |(Cost(3, 0, 5)))
case object LBKV2 extends Brigade("lb-kv2", "KV-2 Assault Gun", Soviet, 2, $(KV2CallIn), |(KV2), |(Cost(2, 0, 5)))
case object LBISU152 extends Brigade("lb-isu152", "ISU-152 Heavy Assault Gun", Soviet, 3, $(ISU152CallIn), |(ISU152), |(Cost(3, 0, 5)))
case object LBKV8 extends Brigade("lb-kv8", "KV-8 Flamethrower Tank", Soviet, 2, $(KV8CallIn), |(KV8), |(Cost(3, 0, 2)))

// OKW
case object LBJaegerRecon extends Brigade("lb-jaeger-recon", "Jaeger Recon Squad", OKW, 0, $(JaegerCallInPlain), |(JaegerRecon), |(Cost(3, 0, 0)), 1)
case object LBKettenkrad extends Brigade("lb-kettenkrad", "Kettenkrad", OKW, 0, $(KettenkradCallIn), |(Kettenkrad), |(Cost(2, 0, 1)))
case object LBLeFH18 extends Brigade("lb-lefh18", "LeFH 18 Artillery", OKW, 1, $(LeFH18Ability))
case object LBStG44Volks extends Brigade("lb-stg44-volks", "StG44 Rifle Volksgrenadiers", OKW, 0, $(StG44Volks), None, None, 1)
case object LBStormtroopersOKW extends Brigade("lb-stormtroopers-okw", "Veteran G43 Stormtroopers", OKW, 3, $(StormtrooperSpawn), |(Stormtroopers), |(Cost(5, 2, 0)))
case object LBPak43OKW extends Brigade("lb-pak43-okw", "Pak 43 Emplacement", OKW, 1, $(OKWPak43Ability))
case object LBPanzerfusilier extends Brigade("lb-panzerfusilier", "Panzerfüsilier", OKW, 0, $(PanzerfusilierCallInPlain), |(Panzerfusilier), |(Cost(4, 1, 0)), 1)
case object LBPuma extends Brigade("lb-puma", "Puma", OKW, 1, $(PumaCallIn), |(Puma), |(Cost(2, 0, 3)))
case object LBJagdpantherOKW extends Brigade("lb-jagdpanther-okw", "Jagdpanther", OKW, 2, $(JagdpantherCallIn), |(Jagdpanther), |(Cost(3, 0, 4)))
case object LBKingTigerOKW extends Brigade("lb-king-tiger-okw", "King Tiger", OKW, 3, $(KingTigerCallIn), |(KingTiger), |(Cost(4, 0, 5)))
case object LBStuGOKW extends Brigade("lb-stug-okw", "StuG III Ausf. G Assault Gun", OKW, 1, $(StuGCallIn), |(StuG), |(Cost(2, 0, 3)))
case object LBHetzer extends Brigade("lb-hetzer", "Flammpanzer 38 Hetzer", OKW, 2, $(HetzerCallInPlain), |(Hetzer), |(Cost(3, 0, 2)))

// UNIVERSAL: the Panther Lost Brigade, a Command Panther of each faction (captured, for the Allies)
case object LBCommandPantherUS extends Brigade("lb-command-panther-us", "Captured Command Panther", US, 3, $(CommandPantherCallIn), |(CommandPantherUS), |(Cost(3, 0, 5)))
case object LBCommandPantherWM extends Brigade("lb-command-panther-wehrmacht", "Command Panther", Wehrmacht, 3, $(CommandPantherCallIn), |(CommandPantherWM), |(Cost(3, 0, 5)))
case object LBCommandPantherBR extends Brigade("lb-command-panther-british", "Captured Command Panther", British, 3, $(CommandPantherCallIn), |(CommandPantherBR), |(Cost(3, 0, 5)))
case object LBCommandPantherSU extends Brigade("lb-command-panther-soviet", "Captured Command Panther", Soviet, 3, $(CommandPantherCallIn), |(CommandPantherSU), |(Cost(3, 0, 5)))
case object LBCommandPantherOKW extends Brigade("lb-command-panther-okw", "Command Panther", OKW, 3, $(CommandPantherCallIn), |(CommandPantherOKW), |(Cost(3, 0, 5)))

object Brigade {
    val all = $[Brigade](
        LBAssaultEngineers, LBCavalryRiflemen, LBVeteranDevils, LBUSMGBunker, LBParadropEquipment, LBEasyEight,
        LBPriest, LBStuart, LBPershing, LBCalliope, LBJackson, LBDozer,
        LBEliteGrenadiers, LBJaegerSniper, LBMG42Bunker, LBOsttruppen, LBPak43, LBStormtroopers,
        LBTiger, LBStuG, LBKingTiger, LBJagdpanther, LBElefant, LBHalftrack,
        LBSeventeenPounder, LBAPSniper, LBForwardEmplacement, LBCommandos, LBRoyalEngineers, LBStaghound,
        LBChurchillAVRE, LBCrocodile, LBSexton, LBComet, LBFirefly, LBValentine,
        LBDShK, LBForwardHQ, LBGuardsRifle, LBPartisans, LBPenalBattalion, LBRepairStation,
        LBShockTroops, LBT3485, LBIS2, LBKV2, LBISU152, LBKV8,
        LBJaegerRecon, LBKettenkrad, LBLeFH18, LBStG44Volks, LBStormtroopersOKW, LBPak43OKW,
        LBPanzerfusilier, LBPuma, LBJagdpantherOKW, LBKingTigerOKW, LBStuGOKW, LBHetzer,
        LBCommandPantherUS, LBCommandPantherWM, LBCommandPantherBR, LBCommandPantherSU, LBCommandPantherOKW)

    // a faction's deck: its twelve and its Command Panther
    def of(f : Faction) = all.%(_.faction == f.base)

    // the starting Infantry a brigade gives
    def startUnit(b : Brigade) : |[UnitType] = b @@ {
        case LBStG44Volks => |(StG44Volksgrenadiers)
        case _ => (b.starting > 0).?(b.unit).|(None)
    }
}

// the setup's Lost Brigades option (Full Game Rules only)
case object LostBrigades extends GameOption with ToggleOption {
    val group = "Lost Brigades".txt
    def valueOn = "Lost Brigade Building Extensions".hl
    override val explain = $[Elem]("Each player draws three of their faction's Lost Brigades (its twelve and the Command Panther) and may pick one, once, for its XP: its units need no Tier Building. Those of 0 XP may be picked at the start, some giving starting Infantry (Full Game Rules only).".txt)
    override def required(all : $[BaseOption]) = $($(FullGameRules))
}
