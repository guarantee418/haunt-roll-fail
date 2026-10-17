package root
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
import root.elem._

case object NewHireling extends DisplayEffect {
    override val name = "New Hireling"
}

trait Hireling extends Faction with DisplayEffect {
    override lazy val elem : Elem = name.styled(this)
    val demoted : Boolean
    val setup : Boolean

    val priority = ""
    def abilities(options : $[Meta.O]) : $[Effect] = $

    def advertising = Empty
    def motto = Empty
}

trait HirelingState extends FactionState {
    def craft = $
}

class BasicHirelingState(val faction : Hireling)(implicit val game : Game) extends HirelingState


case object StreetBand extends Hireling with WarriorFaction {
    val name = "Street Band"
    val short = "STB"
    val style = "stb"

    val demoted = true
    val setup = false

    val clashKey = StreetBand

    val warrior = Weasel

    def pieces(options : $[Meta.O]) = Weasel *** 5
}

case object Weasel extends Warrior

class StreetBandState(val faction : StreetBand.type)(implicit val game : Game) extends HirelingState {
    var enchanted : $[Clearing] = $
}


trait PawnHireling extends Hireling

case object StoicProtector extends PawnHireling {
    val name = "Stoic Protector"
    val short = "STP"
    val style = "stp"

    val demoted = true
    val setup = false

    val clashKey = StoicProtector

    override val transports : $[$[Transport]] = $($(FreeMove))

    def pieces(options : $[Meta.O]) = Deer *** 1
}

case object Deer extends Pawn with Movable with Tenacious

class PawnHirelingState(val faction : PawnHireling)(implicit val game : Game) extends HirelingState {
    val pawn = reserve.$.only
}


case object BanditGangs extends Hireling with WarriorFaction {
    val name = "Bandit Gangs"
    val short = "BDG"
    val style = "bdg"

    val demoted = true
    val setup = false

    val clashKey = BanditGangs

    val warrior = Porcupine

    def pieces(options : $[Meta.O]) = Porcupine *** 4
}

case object Porcupine extends Warrior


case class SkipHeal(c : Clearing) extends RemoveEffect

case object FelinePhysicians extends Hireling {
    val name = "Feline Physicians"
    val short = "FPH"
    val style = "fph"

    val demoted = true
    val setup = false

    val clashKey = MC

    def pieces(options : $[Meta.O]) = $
}

class FelinePhysiciansState(val faction : FelinePhysicians.type)(implicit val game : Game) extends HirelingState {
}


case object BluebirdNobles extends Hireling {
    val name = "Bluebird Nobles"
    val short = "BBN"
    val style = "bbn"

    val demoted = true
    val setup = false

    val clashKey = ED

    def pieces(options : $[Meta.O]) = $
}


case object RabbitScouts extends Hireling with BattleEffect {
    val name = "Rabbit Scouts"
    val short = "RSC"
    val style = "rsc"

    val demoted = true
    val setup = false

    val clashKey = WA

    def pieces(options : $[Meta.O]) = $
}


case object Brigand extends Hireling {
    val name = "The Brigand"
    val short = "BRI"
    val style = "bri"

    val demoted = true
    val setup = false

    val clashKey = VB

    def pieces(options : $[Meta.O]) = $
}

case object BrigandExplore extends Effect
case object BrigandSteal extends Effect


case object LizardEnvoys extends Hireling {
    val name = "Lizard Envoys"
    val short = "LEN"
    val style = "len"

    val demoted = true
    val setup = false

    val clashKey = LC

    def pieces(options : $[Meta.O]) = $
}


case object OtterDivers extends Hireling {
    val name = "Otter Divers"
    val short = "ODV"
    val style = "odv"

    val demoted = true
    val setup = false

    val clashKey = RF

    def pieces(options : $[Meta.O]) = $
}


case object MoleArtisians extends Hireling {
    val name = "Mole Artisians"
    val short = "MAR"
    val style = "mar"

    val demoted = true
    val setup = false

    val clashKey = UD

    def pieces(options : $[Meta.O]) = $
}

class MoleArtisiansState(val faction : MoleArtisians.type)(implicit val game : Game) extends HirelingState {
    var display = cards("display")

    var revealed = cards("revealed")
}


case object RavenSentries extends Hireling with BattleEffect {
    val name = "Raven Sentries"
    val short = "RSN"
    val style = "rsn"

    val demoted = true
    val setup = false

    val clashKey = CC

    def pieces(options : $[Meta.O]) = $
}


case object RatSmugglers extends Hireling with Effect {
    val name = "Rat Smugglers"
    val short = "RSM"
    val style = "rsm"

    val demoted = true
    val setup = false

    val clashKey = LH

    def pieces(options : $[Meta.O]) = $
}


case object BadgerBodyguards extends Hireling with BattleEffect {
    val name = "Badger Bodyguards"
    val short = "BBG"
    val style = "bbg"

    val demoted = true
    val setup = false

    val clashKey = KI

    def pieces(options : $[Meta.O]) = $
}


case object StrayMove extends Transport {
    override def allows(f : Faction, o : Region, d : Region)(implicit game : Game) = (o, d) @@ {
        case (o : Region, d : Region) => f.rules(o) || f.rules(d)
        case _ => false
    }

    override def allows(f : Faction, o : Region, d : Region, l : $[Movable])(implicit game : Game) = (f, o, l) @@ {
        case (f : Faction, o : Region, l : $[Movable]) => f.at(o).of[Warrior].take(l.num) == l
    } && allows(f, o, d)
}

trait StrayCat extends Warrior {
    override def id = "stray-cat"
    override val name = toString
    override def plural = "Stray Cats"
}

case object StrayCat extends StrayCat {
    override val name = "Stray Cat"
}

case object Jeff extends StrayCat
case object Smokie extends StrayCat
case object Maru extends StrayCat

case object Momcat extends StrayCat
case object Black extends StrayCat
case object Dolphin extends StrayCat

case object Madchen extends StrayCat
case object Leo extends StrayCat
case object Monstra extends StrayCat

case object Takahara extends StrayCat
case object Kaban extends StrayCat
case object Rouge extends StrayCat

case object ForestPatrol extends Hireling with WarriorFaction {
    val name = "Forest Patrol"
    val short = "FRP"
    val style = "frp"

    val demoted = false
    val setup = true.not.not.not

    val clashKey = MC

    override val transports = $($(StrayMove))

    val warrior = StrayCat

    def pieces(options : $[Meta.O]) = $(Jeff, Smokie, Maru, Momcat, Black, Dolphin, Madchen, Leo, Monstra, Takahara, Kaban, Rouge)
}

case object FirstAid extends SpecialRegion

class ForestPatrolState(val faction : ForestPatrol.type)(implicit val game : Game) extends HirelingState {
    val hospital = location(FirstAid, u => u.faction == faction && u.piece.is[StrayCat])
}


case object Vulture extends Warrior

case object LastDynasty extends Hireling with WarriorFaction {
    val name = "Last Dynasty"
    val short = "LDN"
    val style = "ldn"

    val demoted = false
    val setup = true

    val clashKey = ED

    val warrior = Vulture

    def pieces(options : $[Meta.O]) = Vulture *** 5
}


case object Animal extends Warrior

case object SpringUprising extends Hireling with WarriorFaction with BattleEffect {
    val name = "Spring Uprising"
    val short = "SUP"
    val style = "sup"

    val demoted = false
    val setup = true

    val clashKey = WA

    val warrior = Animal

    def pieces(options : $[Meta.O]) = Animal *** 4
}


case object ForestOnlyRoads extends Transport {
    override def allows(f : Faction, o : Region, d : Region)(implicit game : Game) = (o, d) @@ {
        case (o : Forest, d : Forest) => game.board.forestsConnected(o, d)
        case _ => false
    }
}

case object Bear extends Pawn with Movable with Attacking

case object TheExile extends PawnHireling {
    val name = "The Exile"
    val short = "EXL"
    val style = "exl"

    val demoted = false
    val setup = true

    val clashKey = VB

    override val transports : $[$[Transport]] = $($(FreeMove, ForestOnlyRoads))

    def pieces(options : $[Meta.O]) = Bear *** 1
}

class TheExileState(val faction : TheExile.type)(implicit val game : Game) extends HirelingState {
    board.forests.foreach(c => location(c))

    val pawn = reserve.$.only

    var ready : $[Item] = $(Club, Club, Club)

    var exhausted : $[Item] = $

    def region : Forest = game.pieces.find(pawn) match {
        case Some((f, r : Forest)) => r
        case q => println(q); throw new Error("bear nowhere to be found")
    }
}

case object Aiding extends Message {
    def elem(implicit game : Game) = "aiding"
}


case object RiverfolkFlotilla extends PawnHireling {
    val name = "Riverfolk Flotilla"
    val short = "RFF"
    val style = "rff"

    val demoted = false
    val setup = true

    val clashKey = RF

    override def abilities(options : $[Meta.O]) = $(Riverboats)

    override val transports : $[$[Transport]] = $($(FreeMove, Riverboat))

    def pieces(options : $[Meta.O]) = Flotilla *** 1
}

case object Flotilla extends Pawn with Attacking with Movable with Tenacious


case object Chameleon extends Warrior

case object WarmSunProphets extends Hireling with WarriorFaction {
    val name = "Warm Sun Prophets"
    val short = "WSP"
    val style = "wsp"

    val demoted = false
    val setup = true

    val clashKey = LC

    val warrior = Chameleon

    def pieces(options : $[Meta.O]) = Chameleon *** 4
}


case object Magpie extends Warrior

case object CorvidSpies extends Hireling with WarriorFaction {
    val name = "Corvid Spies"
    val short = "CSP"
    val style = "csp"

    val demoted = false
    val setup = true

    val clashKey = CC

    val warrior = Magpie

    def pieces(options : $[Meta.O]) = Magpie *** 6
}


case object Mouldwarp extends Warrior
case object Foothold extends Token

case object SunwardExpedition extends Hireling with WarriorFaction {
    val name = "Sunward Expedition"
    val short = "SWE"
    val style = "swe"

    val demoted = false
    val setup = true

    val clashKey = UD

    val warrior = Mouldwarp

    def pieces(options : $[Meta.O]) = Mouldwarp *** 8 ++ Foothold *** 3
}


case object Vole extends Warrior
case object Vault extends Building

case object VaultKeepers extends Hireling with WarriorFaction {
    val name = "Vault Keepers"
    val short = "VLK"
    val style = "vlk"

    val demoted = false
    val setup = true

    val clashKey = KI

    val warrior = Vole

    def pieces(options : $[Meta.O]) = Vole *** 6 ++ Vault *** 6
}


case object Varmint extends Warrior

case object FlameBearers extends Hireling with WarriorFaction {
    val name = "Flame Bearers"
    val short = "FMB"
    val style = "fmb"

    val demoted = false
    val setup = true

    val clashKey = LH

    val warrior = Varmint

    def pieces(options : $[Meta.O]) = Varmint *** 6
}


trait RockStar extends Warrior {
    override def id = "rock-star"
    override def plural = "Rock Stars"
}

case object RockStar extends RockStar {
    override val name = "Rock Star"
}

case object IanAnderson extends RockStar { override val name = "Ian Anderson" }
case object ThomYorke extends RockStar { override val name = "Thom Yorke" }
case object FreddyMercury extends RockStar { override val name = "Freddy Mercury" }
case object SteveWilson extends RockStar { override val name = "Steve Wilson" }
case object ChrisKarrer extends RockStar { override val name = "Chris Karrer" }
case object TimSmith extends RockStar { override val name = "Tim Smith" }
case object Vangelis extends RockStar { override val name = "Vangelis" }
case object MarkKnopfler extends RockStar { override val name = "Mark Knopfler" }
case object JohnnyCash extends RockStar { override val name = "Johnny Cash" }
case object RickWakeman extends RockStar { override val name = "Rick Wakeman" }

case object DavidSylvian extends RockStar { override val name = "David Sylvian" }
case object BobDylan extends RockStar { override val name = "Bob Dylan" }
case object DavidBowie extends RockStar { override val name = "David Bowie" }
case object JeffLynne extends RockStar { override val name = "Jeff Lynne" }
case object RogerChapman extends RockStar { override val name = "Roger Chapman" }
case object PeterHammill extends RockStar { override val name = "Peter Hammill" }
case object SteveHackett extends RockStar { override val name = "Steve Hackett" }
case object WilliamDDrake extends RockStar { override val name = "William D. Drake" }
case object SandyDenny extends RockStar { override val name = "Sandy Denny" }
case object DaveCousins extends RockStar { override val name = "Dave Cousins" }
case object LouReed extends RockStar { override val name = "Lou Reed" }
case object KateBush extends RockStar { override val name = "Kate Bush" }
case object ElvisPresley extends RockStar { override val name = "Elvis Presley" }

case object PopularBand extends Hireling with WarriorFaction {
    val name = "Popular Band"
    val short = "PLB"
    val style = "plb"

    val demoted = false
    val setup = true

    val clashKey = StreetBand

    val warrior = RockStar

    def pieces(options : $[Meta.O]) =
        IanAnderson ::
        ThomYorke ::
        FreddyMercury ::
        SteveWilson ::
        ChrisKarrer ::
        TimSmith ::
        Vangelis ::
        MarkKnopfler ::
        JohnnyCash ::
        RickWakeman ::
        DavidSylvian ::
        BobDylan ::
        DavidBowie ::
        JeffLynne ::
        RogerChapman ::
        PeterHammill ::
        SteveHackett

}


case object Studio extends SpecialRegion {
}

class PopularBandState(val faction : PopularBand.type)(implicit val game : Game) extends HirelingState {
    var enchanted : $[Clearing] = $

    val studio = location(Studio, u => u.faction == faction && u.piece.is[RockStar])
}


case object Elk extends Pawn with Movable with Tenacious

case object FuriousProtector extends PawnHireling {
    val name = "Furious Protector"
    val short = "FPR"
    val style = "fpr"

    val demoted = false
    val setup = true

    val clashKey = StoicProtector

    override val transports : $[$[Transport]] = $($(FreeMove))

    def pieces(options : $[Meta.O]) = Elk *** 1
}


case object HighwayBandits extends Hireling {
    val name = "Highway Bandits"
    val short = "HWB"
    val style = "hwb"

    val demoted = false
    val setup = true

    val clashKey = BanditGangs

    def pieces(options : $[Meta.O]) = Bandit *** 4
}

case object Bandit extends Pawn

case class PathBetween(a : Clearing, b : Clearing) extends SpecialRegion

class HighwayBanditsState(val faction : HighwayBandits.type)(implicit val game : Game) extends HirelingState {
    val paths = clearings.combinations(2).$./(l => l(0) -> l(1)).%((a, b) => game.connected(a).has(b))./((a, b) => location(PathBetween(a, b), u => u.faction == faction && u.piece == Bandit))
}


// Root: Homeland hirelings (cards and pieces from the TTS mod "Root - Ultimate Collection", Steam workshop 2516434159)
case object Advocate extends Warrior

case object SunnyAdvocates extends Hireling with WarriorFaction {
    val name = "Sunny Advocates"
    val short = "SAD"
    val style = "sad"

    val demoted = false
    val setup = true

    val clashKey = TCvA

    val warrior = Advocate

    def pieces(options : $[Meta.O]) = Advocate *** 8
}

case object BatMessengers extends Hireling {
    val name = "Bat Messengers"
    val short = "BMS"
    val style = "bms"

    val demoted = true
    val setup = false

    val clashKey = TCvA

    def pieces(options : $[Meta.O]) = $
}


case object Roamer extends Warrior
case object Lilypad extends Token

case object RiverRoamers extends Hireling with WarriorFaction {
    val name = "River Roamers"
    val short = "RVR"
    val style = "rvr"

    val demoted = false
    val setup = true

    val clashKey = LDvD

    val warrior = Roamer

    def pieces(options : $[Meta.O]) = Roamer *** 10 ++ Lilypad *** 4
}

// The River Roamers' crafting with lilypads, once a turn, before their other Daylight action
case object RoamersCrafted extends Effect

case object FrogTinkers extends Hireling {
    val name = "Frog Tinkers"
    val short = "FRT"
    val style = "frt"

    val demoted = true
    val setup = false

    val clashKey = LDvD

    def pieces(options : $[Meta.O]) = $
}

class FrogTinkersState(val faction : FrogTinkers.type)(implicit val game : Game) extends HirelingState {
    val display = cards("display")
}


case object FarmerWarrior extends Warrior {
    override def id = "farmer"
    override def name = "Farmer"
}

trait FarmersHireling extends Hireling with WarriorFaction {
    val clashKey = ProsperousFarmers

    val warrior = FarmerWarrior

    def pieces(options : $[Meta.O]) = FarmerWarrior *** 9 ++ Farm *** 4
}

case object ProsperousFarmers extends FarmersHireling {
    val name = "Prosperous Farmers"
    val short = "PFR"
    val style = "pfr"

    val demoted = false
    val setup = true
}

case object StrugglingFarmers extends FarmersHireling {
    val name = "Struggling Farmers"
    val short = "SFR"
    val style = "sfr"

    val demoted = true
    val setup = false
}

class StrugglingFarmersState(val faction : StrugglingFarmers.type)(implicit val game : Game) extends HirelingState {
    // who gave the hireling away last, for the when-hired removal
    var former : |[Faction] = None
}


case class HirelingTransfersAction(f : Faction) extends ForcedAction
case class GiveAwayHirelingAction(self : Faction, l : $[Hireling], h : Hireling) extends BaseAction(self, "gives away")(h) with Soft
case class GiveAwayHirelingToAction(self : Faction, h : Hireling, e : Faction) extends BaseAction(self, "gives away", h)(e)
case class HirelingPurchasesAction(f : Faction) extends ForcedAction
case class HireHirelingAction(self : Faction, h : Hireling) extends BaseAction(self, "hires")(h)
case class TakeHirelingAction(f : Faction, h : Hireling, then : ForcedAction) extends ForcedAction
case class ControlRolledAction(f : Faction, h : Hireling, n : Int, then : ForcedAction) extends RolledAction[Int] { def rolled = $(n) }
case class OnTakeHirelingAction(f : Faction, h : Hireling, then : ForcedAction) extends ForcedAction


case class PlaceHirelingWarriorAction(self : Player, h : Hireling, p : Piece, c : Clearing, then : ForcedAction) extends BaseAction(self, "places", p.of(h), "in")(c)
case class PlaceHirelingWarriorsAction(self : Player, h : Hireling, p : $[Piece], c : Clearing, then : ForcedAction) extends BaseAction(self, "places", p./(_.of(h)).comma, "in")(c)


case class FelinePhysiciansAction(f : WarriorFaction, c : Clearing, d : DeckCard, r : Region, then : ForcedAction) extends ForcedAction
case class FelinePhysiciansIgnoreAction(f : WarriorFaction, c : Clearing, then : ForcedAction) extends ForcedAction

case class MoleArtisiansRevealAction(self : Faction, d : DeckCard, then : ForcedAction) extends BaseAction(MoleArtisians, "can keep revealed crafted card")(d.img) with ViewCard
case class MoleArtisiansDiscardAction(self : Faction, d : DeckCard, then : ForcedAction) extends BaseAction()("Discard")

case class BluebirdNoblesAction(self : Faction, n : Int) extends BaseAction()("Score", n.hl, "ruled clearings with", BluebirdNobles)

case class LizardEnvoysMainAction(self : Faction) extends BaseAction()("Take a recently discarded card with", LizardEnvoys) with Soft
case class LizardEnvoysAction(self : Faction, d : DeckCard) extends BaseAction(LizardEnvoys)(d.img) with ViewCard

case class StreetBandMainAction(self : Faction, l : $[Clearing]) extends BaseAction(Hirelings)(StreetBand) with Soft

case class BanditGangsMainAction(self : Faction, l : $[Clearing], r : $[$[Clearing]], o : Int) extends BaseAction(Hirelings)(BanditGangs) with Soft
case class BanditGangsMassRecruitAction(self : WarriorFaction, l : $[Clearing], o : Int) extends BaseAction("Recruit", l.num.times(self.warrior.imgd(self)).merge)(implicit g => l./(_.elem).comma.merge)

case class RatSmugglersMainAction(self : Faction, lm : $[Region], lw : $[Clearing], then : ForcedAction) extends BaseAction(Hirelings)(RatSmugglers) with Soft
case class RatSmugglersAction(self : Faction, d : DeckCard, then : ForcedAction) extends ForcedAction
case class DiscardItemCardAction(f : Faction, d : DeckCard, then : ForcedAction) extends ForcedAction

case class BrigandExploreMainAction(self : Faction, l : $[Clearing], then : ForcedAction) extends BaseAction(Brigand)("Explore") with Soft
case class BrigandExploreAction(self : Faction, c : Clearing, then : ForcedAction) extends BaseAction(Brigand, "explores a ruin in")(c)
case class BrigandTakeRuinItemAction(self : Faction, c : Clearing, i : Item, then : ForcedAction) extends BaseAction(self, "explores ruins in", c, "and finds an item")("Take", i.img, i)
case class BrigandIgnoreRuinItemAction(self : Faction, c : Clearing, then : ForcedAction) extends BaseAction(None)("Don't take an item".styled(styles.hit))

case class BrigandStealMainAction(self : Faction, l : $[Faction], then : ForcedAction) extends BaseAction(Brigand)("Steal") with Soft
case class BrigandStealAction(self : Faction, i : ItemRef, e : Faction, then : ForcedAction) extends BaseAction(Brigand, "steals from")(e)

case class PopularBandStudioAction(self : Player, h : PopularBand.type, shuffled : $[RockStar]) extends ShuffledAction[RockStar]
case class PopularBandSetupAction(self : Player, h : PopularBand.type) extends ForcedAction

case class HighwayBanditMainPlaceAction(self : Player, h : HighwayBandits.type, alt : $[UserAction], then : ForcedAction) extends ForcedAction with Soft
case class HighwayBanditPlaceAction(self : Player, h : HighwayBandits.type, a : Clearing, b : Clearing, then : ForcedAction) extends ForcedAction
case class HighwayBanditsSetupAction(self : Player, h : HighwayBandits.type) extends ForcedAction


case class ShuffledStrayCatsAction(self : Player, h : ForestPatrol.type, shuffled : $[StrayCat]) extends ShuffledAction[StrayCat]
case class ForestPatrolMainAction(self : Faction, h : ForestPatrol.type, m : $[Region], b : $[Clearing], r : $[Clearing], p : Int) extends BaseAction(None)(ForestPatrol) with Soft
case class MovePatrolAction(self : Faction, l : $[Region]) extends BaseAction(ForestPatrol)("Move".styled(ForestPatrol), dt.Move, "then", "Battle".styled(ForestPatrol), dt.Battle) with Soft
case class MoveDonePatrolAction(self : Faction) extends ForcedAction
case class AttackPatrolAction(self : Faction, l : $[Clearing]) extends BaseAction(ForestPatrol)("Battle".styled(ForestPatrol), dt.Battle) with Soft

case class LastDynastyMainAction(self : Faction, h : LastDynasty.type, c : |[Clearing]) extends BaseAction(Hirelings)(LastDynasty) with Soft
case class LastDynastyRecruitAction(self : Faction, h : LastDynasty.type, c : Clearing) extends BaseAction("Recruit", implicit g => h.pooled(h.warrior).times(h.warrior.imgd(h)).merge, "and Battle in")(c)

case class UprisingSetupWarriorsAction(self : Player, h : SpringUprising.type, rolled : $[BaseSuit], done : $[Clearing], then : ForcedAction) extends RolledAction[BaseSuit]
case class PlaceUprisingWarriorsAction(self : Player, h : SpringUprising.type, l : $[BaseSuit], done : $[Clearing], then : ForcedAction) extends ForcedAction
case class SpringUprisingMainAction(self : Faction, h : SpringUprising.type) extends BaseAction()(h)
case class SpringUprisingAction(self : Faction, h : SpringUprising.type, s : BaseSuit) extends RolledAction[BaseSuit] { def rolled = $(s) }
case class SpringUprisingNukeAction(self : Faction, h : SpringUprising.type, c : Clearing) extends BaseAction("Remove", h.warrior.of(h), "and all enemies from")(c)
case class SpringUprisingRemoveAction(self : Faction, h : SpringUprising.type, c : Clearing) extends BaseAction("Remove", h.warrior.of(h), "from")(c)

case class AidTheExileMainAction(self : Faction, h : TheExile.type) extends BaseAction(Hirelings)("Aid", TheExile) with Soft
case class AidTheExileAction(self : Faction, h : TheExile.type, i : ItemRef) extends ForcedAction
case class TheExileMainAction(self : Faction, h : TheExile.type) extends BaseAction(Hirelings)(TheExile) with Soft
case class TheExileMoveAction(self : Faction, h : TheExile.type) extends BaseAction(TheExile)("Move".styled(TheExile), dt.Move) with Soft
case class TheExileBattleAction(self : Faction, h : TheExile.type) extends BaseAction(None)("Battle".styled(TheExile), dt.Battle) with Soft
case class TheExileExhaustAction(self : Faction, h : TheExile.type, n :  Int, then : ForcedAction) extends ForcedAction
case class TheExileDoneAction(self : Faction, h : TheExile.type) extends ForcedAction
case class StartingHirelingForestAction(self : Player, h : Hireling, r : Forest) extends BaseAction(h, "starts in")(implicit g => board.forestName(r))

case class WarmSunProphetsMainAction(self : Faction, h : WarmSunProphets.type) extends BaseAction(Hirelings)(WarmSunProphets) with Soft

case class RiverfolkFlotillaAction(self : Faction, h : RiverfolkFlotilla.type) extends BaseAction()("Draw", "cards".hh, "and move", Flotilla.of(h))
case class RiverfolkFlotillaMoveAction(self : Faction, h : RiverfolkFlotilla.type) extends ForcedAction
case class RiverfolkFlotillaBattleAction(self : Faction, h : RiverfolkFlotilla.type) extends ForcedAction

case class FlameBearersSetupAction(self : Player, h : FlameBearers.type) extends ForcedAction

// helpers shared by several hirelings
case class HirelingPlaceAnyAction(self : Faction, h : Hireling, p : Piece, n : Int, l : $[Clearing], then : ForcedAction) extends ForcedAction
case class HirelingBattleEachAction(self : Faction, h : Hireling, l : $[Clearing], optional : Boolean, then : ForcedAction) extends ForcedAction
case class HirelingMoveBattleAction(self : Faction, h : Hireling, then : ForcedAction) extends BaseAction(h)("Move".styled(h), dt.Move, "then", "Battle".styled(h), dt.Battle) with Soft
case class HirelingMovedBattleAction(self : Faction, h : Hireling, then : ForcedAction) extends ForcedAction
case class HirelingRecruitEachAction(self : Faction, h : WarriorFaction, at : $[Clearing], then : ForcedAction) extends BaseAction(h)("Place", h.warrior.of(h), "at each", implicit g => at.distinct./(_.elem).comma.merge) with Soft
case class HirelingRecruitAction(self : Faction, h : WarriorFaction, l : $[Clearing], then : ForcedAction) extends BaseAction("Place", implicit g => l.num.times(h.warrior.imgd(h)).merge)(implicit g => l./(_.elem).comma.merge)
case class HirelingRemovePieceAction(self : Faction, h : Hireling, c : Clearing, e : Faction, p : Piece, then : ForcedAction) extends BaseAction(h, "removes in", c)(p.img(e), p.of(e))

// WARM SUN PROPHETS
case class WarmSunProphetsBattleAction(self : Faction, h : WarmSunProphets.type, e : Faction, c : Clearing) extends BaseAction(h, "can force")("Force", e, "to", "battle".hh, "in", c) with Soft
case class WarmSunProphetsMoveAction(self : Faction, h : WarmSunProphets.type, e : Faction, c : Clearing) extends BaseAction(h, "can force")("Force", e, "to", "move".hh, "from", c) with Soft
case class WarmSunProphetsMoveToAction(self : Faction, h : WarmSunProphets.type, e : Faction, c : Clearing, d : Clearing, t : $[Transport], l : $[Piece]) extends BaseAction("Force", e, "to move from", c, "to")(d, l./(_.img(e)).merge)
case class WarmSunProphetsMovedAction(self : Faction, h : WarmSunProphets.type, e : Faction, d : Clearing) extends ForcedAction

// SUNWARD EXPEDITION
case class SunwardMainAction(self : Faction, h : SunwardExpedition.type) extends BaseAction(Hirelings)(SunwardExpedition) with Soft
case class SunwardRemoveFootholdAction(self : Faction, h : SunwardExpedition.type, c : Clearing) extends BaseAction(h, "has no footholds left, remove one from")(c)
case class SunwardFootholdAction(self : Faction, h : SunwardExpedition.type, cancel : Boolean) extends ForcedAction
case class SunwardFootholdClearingAction(self : Faction, h : SunwardExpedition.type, c : Clearing, cancel : Boolean) extends BaseAction(h, "places a", Foothold.of(h), "in")(c) with Soft
case class SunwardPlaceAction(self : Faction, h : SunwardExpedition.type, c : Clearing, n : Int) extends BaseAction(h, "places a", Foothold.of(h), "in", c, "and")(n.hl, Mouldwarp.nof(n)(h), implicit g => n.times(Mouldwarp.imgd(h)).merge)
case class SunwardSecondAction(self : Faction, h : SunwardExpedition.type) extends ForcedAction

// CORVID SPIES
case class CorvidSpiesMainAction(self : Faction, h : CorvidSpies.type) extends BaseAction(Hirelings)(CorvidSpies) with Soft
case class CorvidSpiesPlaceAction(self : Faction, h : CorvidSpies.type, n : Int) extends ForcedAction
case class CorvidSpiesSecondAction(self : Faction, h : CorvidSpies.type) extends ForcedAction
case class CorvidSpiesStealAction(self : Faction, h : CorvidSpies.type, c : Clearing) extends BaseAction(h)("Take a random card from each enemy in", c)

// FLAME BEARERS
case class FlameBearersMainAction(self : Faction, h : FlameBearers.type) extends BaseAction()(h)
case class FlameBearersBurnAction(self : Faction, h : FlameBearers.type, l : $[Clearing]) extends ForcedAction
case class FlameBearersSpreadAction(self : Faction, h : FlameBearers.type) extends ForcedAction

// VAULT KEEPERS
case class VaultKeepersMainAction(self : Faction, h : VaultKeepers.type) extends BaseAction(Hirelings)(VaultKeepers) with Soft
case class VaultKeepersPlaceAction(self : Faction, h : VaultKeepers.type, c : Clearing, warrior : Boolean) extends BaseAction(h, "places a", Vault.of(h), warrior.?("and a"), warrior.?(Vole.of(h)), "in")(c)
case class VaultKeepersSkipAction(self : Faction, h : VaultKeepers.type) extends BaseAction(h)("Place no", Vault.of(h))
case class VaultKeepersSecondAction(self : Faction, h : VaultKeepers.type) extends ForcedAction

// HIGHWAY BANDITS
case class HighwayBanditsMainAction(self : Faction, h : HighwayBandits.type) extends BaseAction(Hirelings)(HighwayBandits) with Soft
case class HighwayBanditRemoveAction(self : Faction, h : HighwayBandits.type, a : Clearing, b : Clearing) extends BaseAction(h, "has no bandits left, remove one from")(a, dt.Arrow, b)
case class HighwayBanditBonusAction(self : WarriorFaction, c : Clearing, then : ForcedAction) extends BaseAction(HighwayBandits)("Place", self.warrior.of(self), "in", c)
case class HighwayBanditTollAction(self : Faction, c : Clearing, p : Piece, then : ForcedAction) extends BaseAction(self, "must pay", HighwayBandits)("Remove", p.img(self), p.of(self))

// POPULAR BAND
case class PopularBandMainAction(self : Faction, h : PopularBand.type) extends BaseAction(Hirelings)(PopularBand) with Soft
case class PopularBandConcertAction(self : Faction, h : PopularBand.type, c : Clearing) extends BaseAction(h, "plays a concert in")(c)
case class PopularBandPullAction(self : Faction, h : PopularBand.type, c : Clearing) extends ForcedAction
case class PopularBandPullOneAction(self : Faction, h : PopularBand.type, c : Clearing, d : Clearing, e : Faction, p : Piece) extends BaseAction(h, "can draw warriors to", c)(p.img(e), p.of(e), "from", d)
case class PopularBandPlaceAction(self : Faction, h : PopularBand.type) extends ForcedAction

// FURIOUS PROTECTOR
case class FuriousProtectorRemoveAction(self : Faction, h : FuriousProtector.type, l : $[Faction]) extends ForcedAction

// SUNNY ADVOCATES
case class SunnyAdvocatesSetupAction(self : Player, h : SunnyAdvocates.type, l : $[BaseSuit]) extends ForcedAction
case class SunnyAdvocatesSetupClearingAction(self : Player, h : SunnyAdvocates.type, s : BaseSuit, c : Clearing, l : $[BaseSuit]) extends BaseAction(h, "places two", Advocate.sof(h), "in a", s, "clearing")(c)
case class SunnyAdvocatesMainAction(self : Faction, h : SunnyAdvocates.type) extends BaseAction(Hirelings)(SunnyAdvocates) with Soft
case class SunnyAdvocatesSuitAction(self : Faction, h : SunnyAdvocates.type, s : BaseSuit) extends BaseAction(h, "converts in")(s, "clearings")
case class SunnyAdvocatesReplaceAction(self : Faction, h : SunnyAdvocates.type, l : $[Clearing]) extends ForcedAction
case class SunnyAdvocatesReplaceOneAction(self : WarriorFaction, h : SunnyAdvocates.type, c : Clearing, e : Faction, p : Piece, l : $[Clearing]) extends BaseAction(h, "replaces in", c)(p.img(e), p.of(e))
case class SunnyAdvocatesSpreadAction(self : Faction, h : SunnyAdvocates.type) extends ForcedAction

// BAT MESSENGERS
case class BatMessengersAction(self : WarriorFaction, l : $[Clearing], c : Clearing, then : ForcedAction) extends BaseAction(BatMessengers, "can place a warrior at new pieces in", implicit g => l./(_.elem).comma.merge)("Place", self.warrior.of(self), "in", c)
case class BatMessengersAskAction(self : WarriorFaction, l : $[Clearing], then : ForcedAction) extends ForcedAction

// RIVER ROAMERS
case class RiverRoamersCraftAction(self : Faction, h : RiverRoamers.type) extends BaseAction(Hirelings)(RiverRoamers, "craft using", Lilypad.sof(h))
case class RiverRoamersMainAction(self : Faction, h : RiverRoamers.type) extends BaseAction(Hirelings)(RiverRoamers) with Soft
case class RiverRoamersCraftMoreAction(self : Faction, h : RiverRoamers.type) extends BaseAction(Hirelings)(RiverRoamers, "craft using", Lilypad.sof(h)) with Soft
case class RiverRoamersCraftMenuAction(self : Faction, h : RiverRoamers.type, cancel : Boolean) extends ForcedAction
case class RiverRoamersPlaceAction(self : Player, h : RiverRoamers.type, c : Clearing, then : ForcedAction) extends BaseAction(h, "places", Roamer.sof(h), "in")(c)

// FROG TINKERS
case class FrogTinkersAction(self : Faction, h : FrogTinkers.type) extends BaseAction()("Draw a card for", h)

// FARMERS
case class FarmersSetupAction(self : Player, h : ProsperousFarmers.type) extends ForcedAction
case class FarmersPlaceAction(self : Faction, h : FarmersHireling, c : Clearing, farm : Boolean, warrior : Boolean, then : ForcedAction) extends BaseAction(h, "places in")(c, farm.?(Farm.img(h)), warrior.?(FarmerWarrior.img(h)))
case class ProsperousFarmersMainAction(self : Faction, h : ProsperousFarmers.type) extends BaseAction(Hirelings)(ProsperousFarmers) with Soft
case class ProsperousFarmersHarvestAction(self : Faction, h : ProsperousFarmers.type) extends BaseAction(h)("Draw a card and place a", Farm.of(h))
case class ProsperousFarmersFarmAction(self : Faction, h : ProsperousFarmers.type) extends ForcedAction
case class ProsperousFarmersSecondAction(self : Faction, h : ProsperousFarmers.type) extends ForcedAction
case class StrugglingFarmersMainAction(self : Faction, h : StrugglingFarmers.type) extends BaseAction(Hirelings)(StrugglingFarmers) with Soft

case object Hirelings extends Message {
    def elem(implicit game : Game) = "Hirelings"
}

object HirelingsExpansion extends Expansion {
    def active(setup : $[Faction], options : $[Meta.O]) = options.of[HirelingsOption].but(NoHirelings).any

    override def extraMoveFrom(f : Faction)(implicit game : Game) = f @@ {
        case h : TheExile.type => $(h.region)
        case _ => $()
    }

    override def birdsong(f : Faction)(implicit game : Game, ask : ActionCollector) {
        anytime(f)
    }

    override def daylight(f : Faction)(implicit game : Game, ask : ActionCollector) {
        StreetBand.?%(f.can).foreach { h =>
            + StreetBandMainAction(f, clearings.%(f.canPlace))
                .!(h.pool(Weasel).not, "no warriors")
        }

        BanditGangs.?%(f.can).foreach { h =>
            val cc = (h.pool(Porcupine)).??(clearings.%(f.present).%(f.canPlace))
            f.as[WarriorFaction]./ { f =>
                val bb = h.all(Porcupine).%(f.canPlace)
                val rr = bb.combinations(min(bb.num, f.pooled(f.warrior))).$
                + BanditGangsMainAction(f, cc, rr, bb.num)
                    .!(h.pool(Porcupine).not && f.pool(f.warrior).not, "no warriors")
                    .!(cc.none && rr.none, "can't place")
            }.| {
                + BanditGangsMainAction(f, cc, $, 0)
                    .!(h.pool(Porcupine).not, "no warriors")
                    .!(cc.none, "can't place")
            }
        }

        RatSmugglers.?%(f.can).foreach { h =>
            val lm = f.moveFrom
            val lb = clearings.%(c => f.canAttackList(c).any)

            + RatSmugglersMainAction(f, lm, lb, Repeat)
                .!(f.hand.get.of[CraftItemCard].none, "no craft cards")
                .!(lm.none && lb.none, "can't attack can't move")
        }

        if (f.has(Brigand)) {
            val rr = game.ruins.keys.$
            val rrr = rr.%(f.present)

            + BrigandExploreMainAction(f, rrr, Repeat).!(f.used.has(BrigandExplore), "used").!(rr.none, "no ruins").!(rrr.none, "no presence")
        }

        if (f.has(Brigand)) {
            val ff = factions.but(f).%(_.hand.any)
            val fff = ff.%(e => clearings.exists(c => f.present(c) && e.present(c)))

            + BrigandStealMainAction(f, fff, Repeat)
                .!(f.used.has(BrigandSteal), "used")
                .!(f.forTrade.forall(_.exhausted), "no items")
                .!(ff.none, "no cards")
                .!(fff.none, "no presence")
        }

        ForestPatrol.?%(f.can).foreach { h =>
            val lm = h.moveFrom
            val lb = clearings.%(c => h.canAttackList(c).any)

            val p = h.hospital.num
            val cc = (p > 0).??(clearings.%(f.canPlace).%(c => h.present(c)))

            + ForestPatrolMainAction(f, h, lm, lb, cc, p)
        }

        LastDynasty.?%(f.can).foreach { h =>
            + LastDynastyMainAction(f, h, h.presence.of[Clearing].single)
        }

        TheExile.?%(f.can).foreach { h =>
            + TheExileMainAction(f, h).!(h.ready.none, "no items")
        }

        WarmSunProphets.?%(f.can).foreach { h =>
            + WarmSunProphetsMainAction(f, h)
        }

        SunwardExpedition.?%(f.can).foreach { h =>
            + SunwardMainAction(f, h)
        }

        CorvidSpies.?%(f.can).foreach { h =>
            + CorvidSpiesMainAction(f, h)
        }

        VaultKeepers.?%(f.can).foreach { h =>
            + VaultKeepersMainAction(f, h)
        }

        HighwayBandits.?%(f.can).foreach { h =>
            + HighwayBanditsMainAction(f, h).!(banditPaths(f).none && h.all(Bandit).none, "no paths")
        }

        PopularBand.?%(f.can).foreach { h =>
            + PopularBandMainAction(f, h)
        }

        SunnyAdvocates.?%(f.can).foreach { h =>
            + SunnyAdvocatesMainAction(f, h)
        }

        RiverRoamers.?%(f.can).foreach { h =>
            if (f.used.has(RoamersCrafted).not)
                + RiverRoamersCraftAction(f, h).!(h.all(Lilypad).none, "no lilypads").!(f.hand.none, "no cards")
            else
                + RiverRoamersCraftMoreAction(f, h).!(f.hand.%(f.craftable).none, "nothing craftable")
            + RiverRoamersMainAction(f, h)
        }

        ProsperousFarmers.?%(f.can).foreach { h =>
            + ProsperousFarmersMainAction(f, h)
        }

        StrugglingFarmers.?%(f.can).foreach { h =>
            + StrugglingFarmersMainAction(f, h).!(clearings.%(f.rules).%(h.canPlace).none, "no ruled clearings")
        }

        anytime(f)
    }

    override def evening(f : Faction)(implicit game : Game, ask : ActionCollector) {
        anytime(f)
    }

    def anytime(f : Faction)(implicit game : Game, ask : ActionCollector) {
        // Knaves' items are their Captains' items, not crafted items to give
        TheExile.?%(hirelings.has).%(_ => f.is[Knaves].not).foreach { h =>
            val cc = f.presence.intersect(game.fromForest(h.region))
            val ii = f.forTrade

            + AidTheExileMainAction(f, h)
                .!(ii.none, "no items")
                .!(cc.none, "not near the forest")
        }
    }

    // paths without a bandit linked to a clearing with f's pieces (bandits can't go on rivers, so only paths)
    // the Highway Bandits' place on the path between two clearings, in either order
    def pathOf(a : Clearing, b : Clearing)(implicit game : Game) : |[(Faction, Region)] =
        HighwayBandits.paths.find(_._2 @@ {
            case PathBetween(x, y) => (x == a && y == b) || (x == b && y == a)
            case _ => false
        })

    def banditPaths(f : Faction)(implicit game : Game) : $[(Clearing, Clearing)] =
        HighwayBandits.paths.filter(l => game.pieces.get(l).none).map(_._2).of[PathBetween].map(p => (p.a, p.b)).filter { case (a, b) => game.connected(a).has(b) && (f.present(a) || f.present(b)) }

    def banditOn(a : Clearing, b : Clearing)(implicit game : Game) : Boolean =
        hirelings.has(HighwayBandits) && pathOf(a, b).exists(l => game.pieces.get(l).any)

    // a clearing's suits as a crafting icon (a card's suit for Frog Tinkers, a bird for any suit)
    def cardAsset(d : DeckCard) : SuitAsset = d.suit @@ {
        case Bird => OneOf(FoxRabbitMouse)
        case s : BaseSuit => s
        case s => s
    }

    // Bat Messengers: the controller's buildings and tokens on the map
    def batPieces(f : Faction)(implicit game : Game) : $[(Piece, Region)] = clearings./~(c => f.at(c).%(p => p.is[Building] || p.is[Token])./(p => (p : Piece) -> (c : Region)))

    def warriorsIn(c : Clearing)(implicit game : Game) : Int = (factions ++ hirelings).%(game.states.contains)./(_.at(c).of[Warrior].num).sum

    def perform(action : Action, soft : Void)(implicit game : Game) = action @@ {
        // BAT MESSENGERS: when its controller places a building or token, they may place a warrior at it
        case a if a.isSoft.not && game.turn > 0 && hirelings.has(BatMessengers) && game.batOwner != BatMessengers.owner =>
            game.batOwner = BatMessengers.owner
            game.batSeen = game.batOwner./~(batPieces)

            TryAgain

        case a if a.isSoft.not && game.turn > 0 && hirelings.has(BatMessengers) && game.batOwner.any && {
            val f = game.batOwner.get
            val now = batPieces(f)
            val seen : $[(Piece, Region)] = game.batSeen
            val kinds : $[Piece] = now.map(_._1).distinct
            val placed : $[(Piece, Region)] = kinds.filter(p => now.count(x => x._1 == p) > seen.count(x => x._1 == p)).flatMap(p => now.filter(x => x._1 == p).diff(seen.filter(x => x._1 == p)))
            game.batSeen = now
            placed.map(_._2).of[Clearing].any && f.is[WarriorFaction] && {
                game.batPlaced = placed.map(_._2).of[Clearing]
                true
            }
        } =>
            val f = game.batOwner.get.asInstanceOf[WarriorFaction]
            val l = game.batPlaced
            game.batPlaced = $

            val then : ForcedAction = a match {
                case a : ForcedAction => a
                case a : Choice => ForceAction(a)
                case a => throw new Error("bat messengers before " + a)
            }

            BatMessengersAskAction(f, l, then)


        case a if a.isSoft.not && game.turn > 0 && game.thresholds.any && game.thresholds.min <= factions./(_.vp).max =>
            game.thresholds :-= game.thresholds.min

            val f = factions.maxBy(_.vp)

            f.effects :+= NewHireling

            f.log("got a hireling")

            TryAgain

        // STREET BAND
        case SetupHirelingAction(f, h : StreetBand.type) =>
            game.states += h -> new StreetBandState(h)

            game.hirelings :+= h

            log(f, "setup", h)

            SetupNextHirelingAction

        case StreetBandMainAction(f, l) =>
            Ask(f).each(l)(PlaceHirelingWarriorAction(f, StreetBand, Weasel, _, UsedEffectAction(f, StreetBand, Repeat))).cancel

        case ForcedRemoveTargetEffectAction(e, c, h @ StreetBand, p @ Weasel, then) =>
            if (StreetBand.enchanted.has(c))
                if (StreetBand.at(c).none)
                    StreetBand.enchanted :-= c

            then

        case BattlePostHitInAction(b, e, f, p @ Weasel, then) =>
            e.log("muted", p.name.styled(f))

            then

        // STOIC PROTECTOR
        case MoveListAction(self, h @ StoicProtector, t, m, from : Clearing, to : Clearing, l, then) if t.has(Ferry) =>
            h.log("moved on", Ferry.elem, "from", from, "to", to)

            Force(MoveListAction(self, h, $, NoMessage, from, to, l, FerryAction(self, from, to, then)))

        case MoveListAction(self, h @ StoicProtector, t, m, from : Clearing, to : Clearing, l, then) if t.any =>
            h.log("moved", "from", from, "to", to)

            Force(MoveListAction(self, h, $, NoMessage, from, to, l, then))

        case OnTakeHirelingAction(f, h @ StoicProtector, then) =>
            if (h.presence.none) {
                val l = clearings.%(f.present).%(f.canPlace)
                Ask(f).each(l)(PlaceHirelingWarriorAction(f, h, Deer, _, then)).bail(then)
            }
            else
                then

        // BANDIT GANGS
        case BanditGangsMainAction(f, l, r, o) =>
            Ask(f)
              .add(f.as[WarriorFaction]./~(f => r./(BanditGangsMassRecruitAction(f, _, o))))
              .add(l./(PlaceHirelingWarriorAction(f, BanditGangs, Porcupine, _, UsedEffectAction(f, BanditGangs, Repeat))))
              .cancel

        case BanditGangsMassRecruitAction(f, l, o) =>
            game.highlights :+= PlaceHighlight(l.distinct)

            l.distinct.foreach { c =>
                val ll = l.%(_ == c)
                ll.foreach(r => f.reserve --> f.warrior --> r)
                f.log("recruited", ll./(_ => f.warrior)./(_.of(f)).comma, "in", c, "with", BanditGangs)
            }

            if (o > l.num && f.totalWar)
                f.oscore(o - l.num)("recruiting")

            UsedEffectAction(f, BanditGangs, Repeat)

        case BattlePostHitInAction(b, e, f, p @ Porcupine, then) =>
            e.log("shaved", p.name.styled(f))

            then

        case OnTakeHirelingAction(f, h @ BanditGangs, then) =>
            if (h.presence.none) {
                val l = clearings.%(f.present).%(f.canPlace)
                Ask(f).each(l)(PlaceHirelingWarriorAction(f, h, Porcupine, _, then)).bail(then)
            }
            else
                NoAsk(f)(then)

        // FELINE PHYSICIANS
        case SetupHirelingAction(f, h : FelinePhysicians.type) =>
            game.states += h -> new FelinePhysiciansState(h)

            game.hirelings :+= h

            log(f, "setup", h)

            SetupNextHirelingAction

        case ForcedRemoveProcessAction(f : WarriorFaction, then) if f.can(FelinePhysicians) && f.ignored.has(IgnoreWarriorRemovalEffects).not =>
            val l = (clearings.%(f.canPlace) ++ f.as[Underground]./(_.burrow)).%(f.present)
            val c = board.clearings.diff(f.ignored.of[SkipHeal]./(_.c)).maxBy(c => f.limbo(c).$.%(_.piece.is[Warrior]).num)

            if (l.any && f.limbo(c).$.%(_.piece.is[Warrior]).num > 0) {
                YYSelectObjectsAction(f, f.hand)
                    .withGroup(FelinePhysicians.elem ~ " can heal " ~ f.limbo(c).$.%(_.piece.is[Warrior])./(x => x.piece.img(x.faction)).merge ~ " with")
                    .withRule(_.matches(c.cost))
                    .withThensInfo(d => l./(r => FelinePhysiciansAction(f, c, d, r, then).as(r)("and place them in")))(l./(r => (Info(r) : InfoAction).apply("and place them in")))
                    .withExtra($(NoHand, FelinePhysiciansIgnoreAction(f, c, ForcedRemoveProcessAction(f, then)).as("Skip", FelinePhysicians)))
            }
            else {
                f.used :+= FelinePhysicians

                ForcedRemoveProcessAction(f, then)
            }

        case FelinePhysiciansAction(f, s, d, r, then) =>
            f.hand --> d --> discard.quiet

            val l = f.limbo(s).$

            l --> r

            FelinePhysicians.log("healed", l./(_.elem).comma, "to", r, "with", d)

            then

        case FelinePhysiciansIgnoreAction(f, c, then) =>
            f.ignored :+= SkipHeal(c)

            then

        // BLUEBIRD NOBLES
        case BluebirdNoblesAction(f, n) =>
            f.used :+= BluebirdNobles
            f.oscore(n /↓ 3)("with", BluebirdNobles)
            Repeat

        //  BRIGAND
        case BrigandExploreMainAction(f, l, then) =>
            Ask(f).each(l)(BrigandExploreAction(f, _, then)).cancel

        case BrigandExploreAction(f, c, then) =>
            game.highlights :+= PlaceHighlight($(c))

            Brigand.logtemp("explored", c, "ruins")

            f.used :+= BrigandExplore

            implicit val ev : MatchesGood[$[Item]] = goodMatchList(goodMatch)

            val fromRuins = f @@ {
                case f : Hero => f.fromRuins
                case f : Horde => f.fromRuins
                case _ => Nil
            }

            Ask(f)
                .each(game.ruins(c).items)(i => BrigandTakeRuinItemAction(f, c, i, then).!(fromRuins.contains(i)))
                .add(BrigandIgnoreRuinItemAction(f, c, then))
                .needOk

        case BrigandTakeRuinItemAction(f, c, i, then) =>
            val rest = game.ruins(c).items :- i

            if (rest.any)
                game.ruins += c -> Ruins(rest)
            else
                game.ruins -= c

            f.forTrade :+= i.pristine

            f @@ {
                case f : Hero => f.fromRuins :+= i
                case f : Horde => f.fromRuins :+= i
                case _ =>
            }

            Brigand.log("explored", c, "ruins", "and found", i)

            AcquireItemsAction(f, then)

        case BrigandIgnoreRuinItemAction(f, c, then) =>
            Brigand.log("explored", c, "ruins", "but took nothing")

            Repeat

        case BrigandStealMainAction(f, l, then) =>
            YYSelectObjectsAction(f, f.forTrade./(ToExhaust))
                .withGroup(Brigand.elem)
                .withRule(_.ref.exhausted.not)
                .withThens(i => l./(BrigandStealAction(f, i.ref, _, then)))
                .withExtra($(CancelAction))

        case BrigandStealAction(f, i, e, then) =>
            f.forTrade :-= i
            f.forTrade :+= i.exhaust

            f.used :+= BrigandSteal

            Brigand.log("stole a card from", e, "with", i)

            StealCardAction(f, e, Repeat)

        // LIZARD ENVOYS
        case LizardEnvoysMainAction(f) =>
            val l = game.pile.reverse.take(5).reverse
            val max = l./(d => l.%(_.suit == d.suit).num).max
            val s = l.%(d => l.%(_.suit == d.suit).num == max)./(_.suit).distinct

            Ask(f).each(l)(d => LizardEnvoysAction(f, d).!(d.suit.in(s).not).!(d.is[Ambush])).cancel.needOk

        case LizardEnvoysAction(f, d) =>
            f.used :+= LizardEnvoys

            game.pile --> d --> f.hand

            f.log("took", d, "from the dicard pile with", LizardEnvoys)

            Repeat

        // MOLE ARTISIANS
        case SetupHirelingAction(f, h : MoleArtisians.type) =>
            game.states += h -> new MoleArtisiansState(h)

            game.hirelings :+= h

            log(f, "setup", h)

            SetupNextHirelingAction

        case MoleArtisiansRevealAction(f, d, q) =>
            MoleArtisians.display --> d --> MoleArtisians.revealed

            MoleArtisians.log("kept revealed", d)

            q

        case MoleArtisiansDiscardAction(f, d, q) =>
            MoleArtisians.display --> d --> discard.quiet

            q

        // RAT SMUGGLERS
        case RatSmugglersMainAction(f, lm, lb, then) =>
            YYSelectObjectsAction(f, f.hand)
                .withGroup(RatSmugglers.elem)
                .withRule(_.is[CraftItemCard])
                .withThens(d =>
                    lm.any.$(MoveInitAction(f, f, $, WithEffect(RatSmugglers), lm, f.movable, $(CancelAction), DiscardItemCardAction(f, d, then)).as("Move", dt.Move)) ++
                    lb.any.$(BattleInitAction(f, f, WithEffect(RatSmugglers), lb, $(CancelAction), DiscardItemCardAction(f, d, then)).as("Battle", dt.Battle))
                )
                .withExtra($(CancelAction))

        case MoveListAction(self, f, t, m, from, to, l, DiscardItemCardAction(ff, dd, then)) =>
            DiscardItemCardAction(ff, dd, ForceAction(MoveListAction(self, f, t, m, from, to, l, then)))

        case BattleStartAction(self, f, a, m, c, o, i, DiscardItemCardAction(ff, dd, then)) =>
            DiscardItemCardAction(ff, dd, BattleStartAction(self, f, a, m, c, o, i, then))

        case LegalAAAConveneAction(f, h, c, e, DiscardItemCardAction(ff, dd, then)) =>
            DiscardItemCardAction(ff, dd, LegalAAAConveneAction(f, h, c, e, then))

        case DiscardItemCardAction(f, d, then) =>
            f.hand --> d --> discard.quiet

            f.log("discarded", d)

            then

        // POPULAR BAND
        case SetupHirelingAction(f, h : PopularBand.type) =>
            game.states += h -> new PopularBandState(h)

            game.hirelings :+= h

            log(f, "setup", h)

            Shuffle[RockStar](h.reserve.$.pieces.of[RockStar], l => PopularBandStudioAction(f, h, l))

        case PopularBandStudioAction(f, h, l) =>
            h.reserve --> h.studio

            h.studio --> l.take(5) --> h.reserve

            PopularBandSetupAction(f, h)

        case PopularBandSetupAction(f, h) =>
            if (h.presence.num == 2)
                SetupNextHirelingAction
            else
                Ask(f)
                    .each(clearings.diff(h.presence))(c => PlaceHirelingWarriorAction(f, h, h.reserve.$.pieces(0), c, PopularBandSetupAction(f, h)))
                    .needOk

        // FURIOUS PROTECTOR
        case SetupHirelingAction(f, h : FuriousProtector.type) =>
            game.states += h -> new BasicHirelingState(h)

            game.hirelings :+= h

            log(f, "setup", h)

            Ask(f).each(clearings)(PlaceHirelingWarriorAction(f, h, Elk, _, SetupNextHirelingAction))

        // HIGHWAY BANDITS
        case SetupHirelingAction(f, h : HighwayBandits.type) =>
            game.states += h -> new HighwayBanditsState(h)

            game.hirelings :+= h

            log(f, "setup", h)

            HighwayBanditsSetupAction(f, h)

        case HighwayBanditsSetupAction(f, h) =>
            if (h.reserve.num == 2)
                SetupNextHirelingAction
            else
                HighwayBanditMainPlaceAction(f, h, $, HighwayBanditsSetupAction(f, h))

        case HighwayBanditMainPlaceAction(f, h, alt, then) =>
            Ask(f)
                .each(clearings.combinations(2).$./(l => l(0) -> l(1)).%((a, b) => game.connected(a).has(b))){ case (a, b) => HighwayBanditPlaceAction(f, h, a, b, then).as(a, dt.Arrow, b) }
                .add(alt)
                .needOk

        case HighwayBanditPlaceAction(f, h, a, b, then) =>
            h.reserve --> Bandit --> PathBetween(a, b)

            h.log("placed", Bandit.of(h), "on the path between", a, "and", b)

            then

        // FOREST PATROL
        case SetupHirelingAction(f, h : ForestPatrol.type) =>
            game.states += h -> new ForestPatrolState(h)

            game.hirelings :+= h

            log(f, "setup", h)

            Shuffle[StrayCat](ForestPatrol.pieces(options), ShuffledStrayCatsAction(f, h, _))

        case ShuffledStrayCatsAction(f, h, l) =>
            clearings.lazyZip(l).foreach { (c, p) =>
                h.reserve --> p --> c
            }

            h.log("placed", StrayCat.sof(h), "through the forest")

            SetupNextHirelingAction

        case ForestPatrolMainAction(f, h, m, b, r, p) =>
            Ask(f)
              .add(MovePatrolAction(f, m))
              .add(AttackPatrolAction(f, b))
              .add(r./(PlaceHirelingWarriorsAction(f, h, h.hospital.pieces, _, UsedEffectAction(f, h, Repeat))))
              .cancel

        case MovePatrolAction(f, l) =>
            MoveInitAction(f, ForestPatrol, $, NoMessage, l, ForestPatrol.movable, $(CancelAction), MoveDonePatrolAction(f))

        case MoveDonePatrolAction(f) =>
            val l = clearings.%(ForestPatrol.canAttackIn)

            val done = UsedEffectAction(f, ForestPatrol, Repeat)

            BattleInitAction(f, ForestPatrol, NoMessage, l, $(done.as("Done")), done)

        case AttackPatrolAction(f, l) =>
            BattleInitAction(f, ForestPatrol, NoMessage, l, $(CancelAction), UsedEffectAction(f, ForestPatrol, Repeat))

        case PlaceHirelingWarriorsAction(f, ForestPatrol, l, c, then) =>
            game.highlights :+= PlaceHighlight($(c))

            ForestPatrol.hospital --> l --> c

            log(f, "placed", l./(_.of(ForestPatrol)).comma, "in", c)

            then

        case BattlePostHitInAction(b, e, f, p : StrayCat, then) =>
            e.log("intercepted", p.name.styled(f))

            then

        case ForcedRemoveTargetEffectAction(e, c, f : ForestPatrol.type, p : StrayCat, then) if options.has(HirelingsByTheCard).not =>
            f.limbo(c) --> p --> f.hospital

            then

        // LAST DYNASTY
        case SetupHirelingAction(f, h : LastDynasty.type) =>
            game.states += h -> new BasicHirelingState(h)

            game.hirelings :+= h

            log(f, "setup", h)

            Ask(f).each(clearings.diff(board.inner))(PlaceHirelingWarriorsAction(f, h, 5.times(Vulture), _, SetupNextHirelingAction))

        case LastDynastyMainAction(f, h, c) =>
            if (c.none)
                Ask(f).each(clearings.diff(board.inner))(LastDynastyRecruitAction(f, h, _)).cancel
            else
            if (h.rules(c.get))
                MoveInitAction(f, h, game.transports./($) ** h.transports ** $($(AllWarriors)), NoMessage, c.$, h.movable, $, BattleInitAction(f, h, NoMessage, $, $, UsedEffectAction(f, h, Repeat)))
            else
                BattleInitAction(f, h, NoMessage, c.$, $(CancelAction), BattleInitAction(f, h, NoMessage, c.$, $, UsedEffectAction(f, h, Repeat)))

        case MoveFinishedAction(f, from, to : Clearing, BattleInitAction(ff, h : LastDynasty.type, m, Nil, extra, then)) =>
            MoveFinishedAction(f, from, to, BattleInitAction(ff, h, m, $(to), extra, then))

        case LastDynastyRecruitAction(f, h, c) =>
            h.reserve --> c

            h.log("respawned in", c)

            BattleInitAction(f, h, NoMessage, $(c), $, UsedEffectAction(f, h, Repeat))

        case BattlePostHitInAction(b, e, f, p @ Vulture, then) =>
            e.log("baited", p.name.styled(f))

            then

        // SPRING UPRISING
        case SetupHirelingAction(f, h : SpringUprising.type) =>
            game.states += h -> new BasicHirelingState(h)

            game.hirelings :+= h

            log(f, "setup", h)

            Roll[BaseSuit]($(BaseSuitDie, BaseSuitDie), l => UprisingSetupWarriorsAction(f, h, l, $, SetupNextHirelingAction), f.elem ~ " rolls two uprising dice")

        case UprisingSetupWarriorsAction(f, h, l, done, then) =>
            h.log("rolled", l./(_.elem).comma)

            PlaceUprisingWarriorsAction(f, h, l, done, then)

        case PlaceUprisingWarriorsAction(f, h, l, done, then) if l.num == done.num =>
            then

        case PlaceUprisingWarriorsAction(f, h, l, done, then) =>
            val rest = l.diff(done./~(_.suits.single))

            Ask(f).each(clearings)(c => PlaceHirelingWarriorAction(f, SpringUprising, Animal, c, PlaceUprisingWarriorsAction(f, h, l, done :+ c, then)).!(rest.exists(_.matches(c.cost)).not))

        case SpringUprisingMainAction(f, h) =>
            Roll[BaseSuit]($(BaseSuitDie), l => SpringUprisingAction(f, h, l(0)), f.elem ~ " rolls an uprising die")

        case SpringUprisingAction(f, h, s) =>
            h.log("rolled", s)

            val l = clearings.%(_.cost.matched(s))
            val u = l.%(h.present)
            val n = u.%(c => h.enemies.%(f.canRemove(c)).%(_.present(c)).any)

            f.used :+= SpringUprising

            Ask(f)
                .each(n)(c => SpringUprisingNukeAction(f, h, c))
                .each(u.diff(n))(c => SpringUprisingRemoveAction(f, h, c))
                .each(l)(c => PlaceHirelingWarriorAction(f, h, h.warrior, c, Repeat).!(f.canPlace(c).not, "can't place").!(h.pool(h.warrior).not, "maximum"))
                .needOk
                .bailout(Repeat.as("Skip"))

        case SpringUprisingNukeAction(f, h, c) =>
            h.log("started an", "Uprising".styled(h), "in", c)

            NukeAction(h, h.enemies.%(h.canRemove(c)), $(c), NukeType.ClearSector, ForceAction(SpringUprisingRemoveAction(f, h, c)))

        case SpringUprisingRemoveAction(f, h, c) =>
            h.from(c) --> h.warrior --> game.removed

            h.log("removed", h.warrior.of(h), "from", c)

            Repeat

        case OnTakeHirelingAction(f, h : SpringUprising.type, then) =>
            if (h.presence.none)
                Roll[BaseSuit]($(BaseSuitDie), l => UprisingSetupWarriorsAction(f, h, l, $, then), f.elem ~ " rolls an uprising die")
            else
                then

        case BattlePostHitInAction(b, e, f, p @ Animal, then) =>
            e.log("kicked", p.name.styled(f))

            then

        // THE EXILE
        case SetupHirelingAction(f, h : TheExile.type) =>
            game.states += h -> new TheExileState(h)

            game.hirelings :+= h

            log(f, "setup", h)

            Ask(f).each(board.forests)(StartingHirelingForestAction(f, h, _))

        case StartingHirelingForestAction(f, h : TheExile.type, r) =>
            h.pawn --> r

            h.log("started in", board.forestName(r))

            SetupNextHirelingAction

        case AidTheExileMainAction(f, h) =>
            YYSelectObjectsAction(f, f.forTrade./(ToDiscard))
                .withGroup("Aid " ~ TheExile.elem)
                .withThen(i => AidTheExileAction(f, h, i.ref))(i => "Aid " ~ TheExile.elem ~ " with " ~ i.ref.elem)("Aid " ~ TheExile.elem ~ " with")
                .withExtra($(CancelAction))

        case AidTheExileAction(f, h, i) =>
            f.forTrade :-= i

            if (i.exhausted)
                h.exhausted :+= i.item
            else
                h.ready :+= i.item

            f.oscore(1)("aiding", h, "with", i)

            DrawCardsAction(f, 1, Aiding, AddCardsAction(f, Repeat))

        case TheExileMainAction(f, h) =>
            Ask(f)
                .add(TheExileMoveAction(f, h).!(h.ready.num < 1))
                .add(TheExileBattleAction(f, h).!(h.ready.num < 2))
                .doneIf(f.used.has(TheExile))(TheExileDoneAction(f, h))
                .cancelIf(f.used.has(TheExile).not)

        case TheExileMoveAction(f, h) =>
            MoveInitAction(f, h, h.transports, NoMessage, $(h.region), h.movable, $, TheExileExhaustAction(f, h, 1, ForceAction(TheExileMainAction(f, h))))

        case TheExileBattleAction(f, h) =>
            BattleInitAction(f, h, NoMessage, board.fromForest(h.region), $, TheExileExhaustAction(f, h, 2, ForceAction(TheExileMainAction(f, h))))

        case move @ MoveListAction(self, f, t, m, from, to, l, then : TheExileExhaustAction) =>
            then.copy(then = ForceAction(move.copy(then = then.then)))

        case battle @ BattleStartAction(self, f, a, m, c, o, i, then : TheExileExhaustAction) =>
            then.copy(then = battle.copy(then = then.then))

        case TheExileExhaustAction(f, h, n, then) =>
            val e = min(n, h.ready.num)
            val d = n - e

            val el = h.ready.take(e)
            h.ready = h.ready.drop(e)
            h.exhausted = h.exhausted ++ el

            h.log("exhausted", el)

            if (d > 0) {
                val dl = h.exhausted.takeRight(d)
                h.exhausted = h.exhausted.dropRight(d)

                h.log("dropped", dl)
            }

            f.used :+= TheExile

            then

        case BattleAssignHitsAction(h : TheExile.type, b, n, _, then) =>
            TheExileExhaustAction(h.owner | game.current, h, n, then)

        case TheExileDoneAction(f, h) =>
            h.ready = h.exhausted ++ h.ready
            h.exhausted = $

            Repeat

        // RIVERFOLK FLOTILLА
        case SetupHirelingAction(f, h : RiverfolkFlotilla.type) =>
            game.states += h -> new PawnHirelingState(h)

            game.hirelings :+= h

            log(f, "setup", h)

            Ask(f).each(clearings.diff(board.inner).intersect(game.riverside))(PlaceHirelingWarriorAction(f, h, Flotilla, _, SetupNextHirelingAction)).needOk

        case RiverfolkFlotillaAction(f, h : RiverfolkFlotilla.type) =>
            val c = h.all(Flotilla).only
            val l = factions.%(_.present(c))

            f.used :+= RiverfolkFlotilla

            l.foldLeft(RiverfolkFlotillaMoveAction(f, h) : ForcedAction)((q, e) => DrawCardsAction(e, 1, WithEffect(RiverfolkFlotilla), AddCardsAction(e, q)))

        case RiverfolkFlotillaMoveAction(f, h : RiverfolkFlotilla.type) =>
            val from = h.all(Flotilla)
            val plans = h.movePlans(from.%(h.canMoveFrom), game.transports./($) ** h.transports)

            val l = from./(o => plans.get(o) @@ {
                case Some((t, l)) => MoveFromAction(f, h, t, JustElem(RiverfolkFlotilla), o, l, RiverfolkFlotillaBattleAction(f, h))
                case None => MoveFromAction(f, h, $, JustElem(RiverfolkFlotilla), o, $, Repeat).!(f.canMoveFrom(o).not, "snared").!(true)
            })

            Ask(f).add(l)

        case RiverfolkFlotillaBattleAction(f, h : RiverfolkFlotilla.type) =>
            BattleInitAction(f, h, NoMessage, h.all(Flotilla), $(Repeat.as("Skip", "Battle".hh)), Repeat)

        // WARM SUN PROPHETS
        case SetupHirelingAction(f, h : WarmSunProphets.type) =>
            game.states += h -> new BasicHirelingState(h)

            game.hirelings :+= h

            log(f, "setup", h)

            if (options.has(HirelingsByTheCard)) {
                val l = game.ruins.keys.$.%(h.canPlace).take(h.pooled(h.warrior))

                l.foreach(c => h.reserve --> h.warrior --> c)

                if (l.any)
                    h.log("placed", l.num.of(h.warrior.name.styled(h)), "in the ruins of", l./(_.elem).comma)

                SetupNextHirelingAction
            }
            else
                Ask(f).each(game.ruins.keys.$.combinations(4).$)(l => l.foldLeft[ForcedAction](SetupNextHirelingAction)((then, c) => ForceAction(PlaceHirelingWarriorAction(f, h, h.warrior, c, then))).as(l.comma)(h)).needOk

        // CORVID SPIES
        case SetupHirelingAction(f, h : CorvidSpies.type) =>
            game.states += h -> new BasicHirelingState(h)

            game.hirelings :+= h

            log(f, "setup", h)

            Ask(f).each(clearings.combinations(2).$.%(l => l(0).asset.matches(l(1).cost) || l(1).asset.matches(l(0).cost)))(l => l.foldLeft[ForcedAction](SetupNextHirelingAction)((then, c) => ForceAction(PlaceHirelingWarriorAction(f, h, h.warrior, c, then))).as(l.comma)(h)).needOk

        // SUNWARD EXPEDITION
        case SetupHirelingAction(f, h : SunwardExpedition.type) =>
            game.states += h -> new BasicHirelingState(h)

            game.hirelings :+= h

            log(f, "setup", h)

            Ask(f)
                .each(clearings)(c => PlaceHirelingWarriorsAction(f, h, $(h.warrior, h.warrior, h.warrior, Foothold), c, SetupNextHirelingAction))
                .needOk

        // FLAME BEARERS
        case SetupHirelingAction(f, h : FlameBearers.type) =>
            game.states += h -> new BasicHirelingState(h)

            game.hirelings :+= h

            log(f, "setup", h)

            FlameBearersSetupAction(f, h)

        case FlameBearersSetupAction(f, h) =>
            if (h.all(h.warrior).num == 2)
                SetupNextHirelingAction
            else
                Ask(f)
                    .each(clearings)(c => PlaceHirelingWarriorAction(f, h, h.warrior, c, FlameBearersSetupAction(f, h)))
                    .needOk

        // VAULT KEEPERS
        case SetupHirelingAction(f, h : VaultKeepers.type) =>
            game.states += h -> new BasicHirelingState(h)

            game.hirelings :+= h

            log(f, "setup", h)

            Ask(f)
                .each(clearings.%(h.canBuild))(c => PlaceHirelingWarriorsAction(f, h, $(h.warrior, h.warrior, Vault), c, SetupNextHirelingAction))
                .needOk

        // HELPERS
        case HirelingPlaceAnyAction(f, h, p, n, l, then) =>
            if (n <= 0 || h.pool(p).not)
                then
            else
                Ask(f).each(l.%(h.canPlace))(c => PlaceHirelingWarriorAction(f, h, p, c, HirelingPlaceAnyAction(f, h, p, n - 1, l, then))).bail(then)

        case HirelingBattleEachAction(f, h, l, optional, then) =>
            val ll = l.distinct.%(h.canAttackIn)

            if (ll.none)
                then
            else
                Ask(f)
                    .each(ll)(c => BattleInitAction(f, h, NoMessage, $(c), $, HirelingBattleEachAction(f, h, l.distinct.but(c), optional, then)).as("Battle".styled(h), dt.Battle, "in", c)(h))
                    .skipIf(optional)(then)

        case HirelingMoveBattleAction(f, h, then) =>
            MoveInitAction(f, h, $, NoMessage, h.moveFrom, h.movable, $(HirelingMovedBattleAction(f, h, then).as("Skip", "Move".styled(h))), HirelingMovedBattleAction(f, h, then))

        case HirelingMovedBattleAction(f, h, then) =>
            BattleInitAction(f, h, NoMessage, clearings.%(h.canAttackIn), $(then.as("Skip", "Battle".styled(h))), then)

        case HirelingRecruitEachAction(f, h, at, then) =>
            val l = at.%(h.canPlace)
            val n = min(l.num, h.pooled(h.warrior))

            Ask(f).each(l.combinations(n).$)(r => HirelingRecruitAction(f, h, r, then)).cancel

        case HirelingRecruitAction(f, h, l, then) =>
            game.highlights :+= PlaceHighlight(l.distinct)

            l.foreach(c => h.reserve --> h.warrior --> c)

            h.log("placed", l.num.of(h.warrior.name.styled(h)), "in", l.distinct./(_.elem).comma)

            then

        case HirelingRemovePieceAction(f, h, c, e, p, then) =>
            ForcedRemoveAction(h, c, e, p, 0, Removing, then)

        // WARM SUN PROPHETS
        case OnTakeHirelingAction(f, h : WarmSunProphets.type, then) if h.all(Chameleon).none =>
            h.logtemp("has no", Chameleon.sof(h), "on the map")

            HirelingPlaceAnyAction(f, h, Chameleon, 1, clearings, then)

        case WarmSunProphetsMainAction(f, h) =>
            val cc = clearings.%(c => h.at(c).has(Chameleon))
            val done = UsedEffectAction(f, h, Repeat)

            Ask(f)
                .some(cc)(c => factions.%(_.present(c))./~{ e =>
                    $(
                        WarmSunProphetsBattleAction(f, h, e, c).!(e.canAttackList(c).none, "can't battle"),
                        WarmSunProphetsMoveAction(f, h, e, c).!(e.is[Hero], "Vagabond").!(e.canMoveFrom(c).not, "can't move").!(e.movePlans($(c), game.transports./($) ** e.transports).none, "can't move")
                    )
                })
                .each(clearings.%(c => h.at(c).has(Chameleon).not).%(h.canPlace))(c => PlaceHirelingWarriorAction(f, h, Chameleon, c, done).!(h.pool(Chameleon).not, "no warriors"))
                .cancel

        case WarmSunProphetsBattleAction(f, h, e, c) =>
            val done = UsedEffectAction(f, h, Repeat)

            Ask(f)
                .group(h, "forces", e, "to battle in", c)
                .each(e.canAttackList(c))(o => BattleConfirmAction(f, e, e, NoMessage, c, o, None, done))
                .cancel

        case WarmSunProphetsMoveAction(f, h, e, c) =>
            val plans = e.movePlans($(c), game.transports./($) ** e.transports)
            val (tt, dd) = plans(c)
            val mv = e.at(c).of[Movable]
            val cm = 1.to(mv.num)./~(n => mv.combinations(n))

            Ask(f)
                .some(dd.of[Clearing]) { d =>
                    val parties : $[($[Transport], $[Movable])] = tt.flatMap((t : $[Transport]) => cm.filter((l : $[Movable]) => t.forall(_.allows(e, c, d, l))).map((l : $[Movable]) => (t, l)))
                    parties.any.??($(parties.maxBy(_._2.num)).map { case (t, l) => WarmSunProphetsMoveToAction(f, h, e, c, d, t, l) })
                }
                .cancel

        case WarmSunProphetsMoveToAction(f, h, e, c, d, t, l) =>
            h.log("forced", e, "to move")

            Force(MoveListAction(f, e, t, NoMessage, c, d, l, WarmSunProphetsMovedAction(f, h, e, d)))

        case WarmSunProphetsMovedAction(f, h, e, d) =>
            val done = UsedEffectAction(f, h, Repeat)

            Ask(f)
                .group(h, "can force", e, "to battle in", d)
                .each(e.canAttackList(d))(o => BattleConfirmAction(f, e, e, NoMessage, d, o, None, done))
                .add(done.as("Skip", "Battle".hh))

        // SUNWARD EXPEDITION
        case SunwardMainAction(f, h) =>
            if (h.pool(Foothold).not && h.all(Foothold).any)
                Ask(f).each(h.all(Foothold))(c => SunwardRemoveFootholdAction(f, h, c)).cancel
            else
                SunwardFootholdAction(f, h, true)

        case SunwardRemoveFootholdAction(f, h, c) =>
            f.used :+= h

            h.from(c) --> Foothold --> h.reserve

            h.log("took back the", Foothold.of(h), "from", c)

            SunwardFootholdAction(f, h, false)

        case SunwardFootholdAction(f, h, cancel) =>
            val cc = clearings.%(c => h.at(c).has(Foothold).not).%(h.canPlace)

            if (h.pool(Foothold) && cc.any)
                Ask(f).each(cc)(c => SunwardFootholdClearingAction(f, h, c, cancel)).cancelIf(cancel)
            else
            if (cancel)
                Ask(f).add(SunwardSecondAction(f, h).as("Move or place", Mouldwarp.sof(h))(h)).cancel
            else
                SunwardSecondAction(f, h)

        case SunwardFootholdClearingAction(f, h, c, cancel) =>
            Ask(f).each(0.to(min(3, h.pooled(Mouldwarp))).$.reverse)(n => SunwardPlaceAction(f, h, c, n)).cancelIf(cancel)

        case SunwardPlaceAction(f, h, c, n) =>
            f.used :+= h

            game.highlights :+= PlaceHighlight($(c))

            h.reserve --> (Foothold +: n.times(Mouldwarp)) --> c

            h.log("placed a", Foothold.of(h), (n > 0).?("and"), (n > 0).?(n.of(Mouldwarp.name.styled(h))), "in", c)

            SunwardSecondAction(f, h)

        case SunwardSecondAction(f, h) =>
            if (f.used.has(h).not)
                f.used :+= h

            val done = Repeat

            Ask(f)
                .group(SunwardExpedition)
                .add(HirelingMoveBattleAction(f, h, done))
                .add(HirelingRecruitEachAction(f, h, h.all(Foothold), done).!(h.all(Foothold).%(h.canPlace).none, "no footholds").!(h.pool(Mouldwarp).not, "no warriors"))
                .add(done.as("Skip"))

        // CORVID SPIES
        case CorvidSpiesMainAction(f, h) =>
            CorvidSpiesPlaceAction(f, h, 0)

        case CorvidSpiesPlaceAction(f, h, n) =>
            val cc = clearings.%(c => h.enemies.exists(_.present(c))).%(h.canPlace)

            if (n >= 2 || h.pool(Magpie).not || cc.none)
                CorvidSpiesSecondAction(f, h)
            else
                Ask(f).group(h, "places", (n == 0).?("first").|("second"), Magpie.of(h))
                    .each(cc)(c => PlaceHirelingWarriorAction(f, h, Magpie, c, (n == 0).?(UsedEffectAction(f, h, CorvidSpiesPlaceAction(f, h, n + 1)) : ForcedAction).|(CorvidSpiesPlaceAction(f, h, n + 1))))
                    .cancelIf(n == 0)

        // reached from the soft first step when no Spy can be placed, so only the recorded choices mark the Spies used
        case CorvidSpiesSecondAction(f, h) =>
            val cc = clearings.%(c => h.at(c).has(Magpie))
            val done = f.used.has(h).?(Repeat).|(UsedEffectAction(f, h, Repeat))

            Ask(f)
                .group(CorvidSpies)
                .add(UsedEffectAction(f, h, HirelingBattleEachAction(f, h, cc, false, Repeat)).as("Battle".styled(h), dt.Battle, "in each clearing with", Magpie.sof(h))(h).!(cc.%(h.canAttackIn).none, "no targets"))
                .each(cc)(c => CorvidSpiesStealAction(f, h, c).!(factions.%!(_.friends(f)).%(_.present(c)).%(_.hand.any).none, "no cards"))
                .add(done.as("Skip"))

        case CorvidSpiesStealAction(f, h, c) =>
            if (f.used.has(h).not)
                f.used :+= h

            val l = factions.%!(_.friends(f)).%(_.present(c)).%(_.hand.any)

            h.log("spied in", c)

            l.foldLeft(Repeat : ForcedAction)((q, e) => StealCardAction(f, e, q))

        // FLAME BEARERS
        case OnTakeHirelingAction(f, h : FlameBearers.type, then) if h.all(Varmint).none =>
            HirelingPlaceAnyAction(f, h, Varmint, 2, clearings, then)

        case FlameBearersMainAction(f, h) =>
            f.used :+= h

            FlameBearersBurnAction(f, h, clearings./~(c => h.at(c).count(Varmint).times(c)))

        case FlameBearersBurnAction(f, h, Nil) =>
            ForcedRemoveFinishedAction(h, FlameBearersSpreadAction(f, h))

        case FlameBearersBurnAction(f, h, c :: rest) =>
            val pieces = h.enemies.%(e => game.states.contains(e)).%(h.canRemove(c))./~(e => e.at(c).distinct.%(p => p.is[Tenacious].not && p.is[Pawn].not)./(p => (e, p)))
            val warriors = pieces.%((e, p) => p.is[Warrior])
            val l = warriors.some.|(pieces)

            if (l.none)
                FlameBearersBurnAction(f, h, rest.but(c))
            else
                Ask(f).group(h, "burn in", c).each(l)((e, p) => HirelingRemovePieceAction(f, h, c, e, p, FlameBearersBurnAction(f, h, rest)))

        case FlameBearersSpreadAction(f, h) =>
            val cc = h.all(Varmint)./~(c => c +: game.connected(c)).distinct.%(h.canPlace)

            if (h.pool(Varmint).not)
                Repeat
            else
            if (cc.any)
                Ask(f).group(h, "spread").each(cc)(c => PlaceHirelingWarriorAction(f, h, Varmint, c, Repeat))
            else
                HirelingPlaceAnyAction(f, h, Varmint, 2, clearings, Repeat)

        // VAULT KEEPERS
        case OnTakeHirelingAction(f, h : VaultKeepers.type, then) if h.presence.none =>
            val l = (h.pool(Vault).?(Vault) ++ h.pooled(Vole).times(Vole).take(2)).$

            Ask(f).group(h, "returns").each(clearings.%(c => h.pool(Vault).?(h.canBuild(c)).|(h.canPlace(c))))(c => PlaceHirelingWarriorsAction(f, h, l, c, then)).bail(then)

        case VaultKeepersMainAction(f, h) =>
            val adjacent = h.all(Vault)./~(c => c +: game.connected(c)).distinct.%(h.canBuild)

            if (h.pool(Vault).not)
                Ask(f).add(VaultKeepersSkipAction(f, h)).cancel
            else
            if (adjacent.any)
                Ask(f).group(h, "places a", Vault.of(h)).each(adjacent)(c => VaultKeepersPlaceAction(f, h, c, false)).add(VaultKeepersSkipAction(f, h)).cancel
            else
                Ask(f).group(h, "places a", Vault.of(h), "and a", Vole.of(h)).each(clearings.%(h.canBuild))(c => VaultKeepersPlaceAction(f, h, c, h.pool(Vole))).add(VaultKeepersSkipAction(f, h)).cancel

        case VaultKeepersPlaceAction(f, h, c, w) =>
            f.used :+= h

            game.highlights :+= PlaceHighlight($(c))

            h.reserve --> (Vault +: w.$(Vole)) --> c

            h.log("placed a", Vault.of(h), w.?("and a"), w.?(Vole.of(h)), "in", c)

            VaultKeepersSecondAction(f, h)

        case VaultKeepersSkipAction(f, h) =>
            f.used :+= h

            VaultKeepersSecondAction(f, h)

        case VaultKeepersSecondAction(f, h) =>
            val cc = h.all(Vault).distinct

            Ask(f)
                .group(VaultKeepers)
                .add(HirelingBattleEachAction(f, h, cc, true, Repeat).as("Battle".styled(h), dt.Battle, "in each clearing with a", Vault.of(h))(h).!(cc.%(h.canAttackIn).none, "no targets"))
                .add(HirelingRecruitEachAction(f, h, h.all(Vault), Repeat).!(cc.%(h.canPlace).none, "no vaults").!(h.pool(Vole).not, "no warriors"))
                .add(Repeat.as("Skip"))

        // HIGHWAY BANDITS
        case HighwayBanditsMainAction(f, h) =>
            val paths = banditPaths(f)

            if (h.pool(Bandit))
                Ask(f).group(h, "places a", Bandit.of(h), "on a path").each(paths)((a, b) => HighwayBanditPlaceAction(f, h, a, b, UsedEffectAction(f, h, Repeat)).as(a, dt.Arrow, b)(h)).cancel
            else
                Ask(f).each(h.paths.map(_._2).of[PathBetween].filter(p => h.at(p).any))(p => HighwayBanditRemoveAction(f, h, p.a, p.b)).cancel

        case HighwayBanditRemoveAction(f, h, a, b) =>
            f.used :+= h

            h.from(PathBetween(a, b)) --> Bandit --> h.reserve

            h.log("took back a", Bandit.of(h), "from the path between", a, "and", b)

            val paths = banditPaths(f)

            Ask(f).group(h, "places a", Bandit.of(h), "on a path").each(paths)((a, b) => HighwayBanditPlaceAction(f, h, a, b, Repeat).as(a, dt.Arrow, b)(h)).bail(Repeat)

        case MoveCompleteAction(self, f, from : Clearing, to : Clearing, l, e, then) if e.has(HighwayBandits).not && banditOn(from, to) =>
            val next = MoveCompleteAction(self, f, from, to, l, e :+ HighwayBandits, then)
            val h = HighwayBandits

            if (h.friends(f).not) {
                val moving = l.distinct.%(p => f.at(to).has(p)).%(p => p.is[Tenacious].not && p.is[Pawn].not)

                if (f.is[Hero] && l.exists(_.is[Pawn]))
                    BattleAssignHitsAction(f, Battle(to, h, None, f, None, None, None, 0, 0, 0, DummyAction), 1, 0, next)
                else
                if (moving.any && h.canRemove(to)(f))
                    Ask(f).group(f, "was ambushed by", h, "on the path to", to).each(moving)(p => HighwayBanditTollAction(f, to, p, ForcedRemoveFinishedAction(h, next)))
                else
                    next
            }
            else
            if (f.has(h))
                f.as[WarriorFaction].%(f => f.pool(f.warrior) && f.canPlace(to))./(f => Ask(f).add(HighwayBanditBonusAction(f, to, next)).skip(next) : Continue).|(next)
            else
                next

        case HighwayBanditTollAction(f, c, p, then) =>
            ForcedRemoveAction(HighwayBandits, c, f, p, 0, Removing, then)

        case HighwayBanditBonusAction(f, c, then) =>
            game.highlights :+= PlaceHighlight($(c))

            f.reserve --> f.warrior --> c

            f.log("placed", f.warrior.of(f), "in", c, "with", HighwayBandits)

            then

        // POPULAR BAND
        case OnTakeHirelingAction(f, h : PopularBand.type, then) if h.all(RockStar).none && clearings.exists(c => h.at(c).of[RockStar].any).not =>
            val p = h.reserve.$.pieces.of[RockStar].starting

            p./(p => Ask(f).group(h, "returns").each(clearings.%(f.present).%(h.canPlace))(c => PlaceHirelingWarriorAction(f, h, p, c, then)).bail(then) : Continue).|(then)

        case PopularBandMainAction(f, h) =>
            val cc = clearings.%(c => h.at(c).of[RockStar].any)

            if (cc.any)
                Ask(f).group(h, "plays a concert").each(cc)(c => PopularBandConcertAction(f, h, c)).cancel
            else
                Ask(f).add(PopularBandPlaceAction(f, h).as("Place a", RockStar.of(h))(h)).cancel

        case PopularBandConcertAction(f, h, c) =>
            f.used :+= h

            h.log("played a concert in", c)

            PopularBandPullAction(f, h, c)

        case PopularBandPullAction(f, h, c) =>
            val l = game.connected(c)./~(d => factions.%(_.canMoveFrom(d))./~(e => e.at(d).of[Warrior].distinct./(p => PopularBandPullOneAction(f, h, c, d, e, p))))

            Ask(f).group(h, "can draw warriors to", c).add(l).add(PopularBandPlaceAction(f, h).as("Done")(h))

        case PopularBandPullOneAction(f, h, c, d, e, p) =>
            Force(MoveListAction(f, e, $, NoMessage, d, c, $(p), PopularBandPullAction(f, h, c)))

        case PopularBandPlaceAction(f, h) =>
            if (f.used.has(h).not)
                f.used :+= h

            h.reserve.$.pieces.of[RockStar].starting./(p => Ask(f).group(h, "can place a", RockStar.of(h)).each(clearings.%(h.canPlace))(c => PlaceHirelingWarriorAction(f, h, p, c, Repeat)).skip(Repeat) : Continue).|(Repeat)

        // FURIOUS PROTECTOR
        case FuriousProtectorRemoveAction(f, h, l) =>
            if (f.used.has(h).not)
                f.used :+= h

            val c = h.all(Elk).only

            l.%(e => e.at(c).of[Warrior].any && h.canRemove(c)(e)) match {
                case Nil => ForcedRemoveFinishedAction(h, Repeat)
                case e :: rest => Ask(f).group(h, "strikes in", c).each(e.at(c).of[Warrior].distinct)(p => HirelingRemovePieceAction(f, h, c, e, p, FuriousProtectorRemoveAction(f, h, rest)))
            }

        // SUNNY ADVOCATES
        case SetupHirelingAction(f, h : SunnyAdvocates.type) =>
            game.states += h -> new BasicHirelingState(h)

            game.hirelings :+= h

            log(f, "setup", h)

            SunnyAdvocatesSetupAction(f, h, FoxRabbitMouse)

        case SunnyAdvocatesSetupAction(f, h, Nil) =>
            SetupNextHirelingAction

        case SunnyAdvocatesSetupAction(f, h, s :: rest) =>
            Ask(f).each(clearings.%(_.cost.matched(s)).%(h.canPlace))(c => SunnyAdvocatesSetupClearingAction(f, h, s, c, rest)).bail(SunnyAdvocatesSetupAction(f, h, rest))

        case SunnyAdvocatesSetupClearingAction(f, h, s, c, rest) =>
            game.highlights :+= PlaceHighlight($(c))

            h.reserve --> $(Advocate, Advocate).take(h.pooled(Advocate)) --> c

            h.log("placed two", Advocate.sof(h), "in", c)

            SunnyAdvocatesSetupAction(f, h, rest)

        case OnTakeHirelingAction(f, h : SunnyAdvocates.type, then) if h.all(Advocate).none =>
            HirelingPlaceAnyAction(f, h, Advocate, 1, clearings, then)

        case SunnyAdvocatesMainAction(f, h) =>
            Ask(f).group(h, "convert in").each(FoxRabbitMouse)(s => SunnyAdvocatesSuitAction(f, h, s)).cancel

        case SunnyAdvocatesSuitAction(f, h, s) =>
            f.used :+= h

            h.log("preached in", s, "clearings")

            SunnyAdvocatesReplaceAction(f, h, clearings.%(_.cost.matched(s)).%(c => h.at(c).has(Advocate)))

        case SunnyAdvocatesReplaceAction(f, h, Nil) =>
            ForcedRemoveFinishedAction(h, SunnyAdvocatesSpreadAction(f, h))

        case SunnyAdvocatesReplaceAction(f, h, c :: rest) =>
            f.as[WarriorFaction].%(f => f.pool(f.warrior) && f.canPlace(c)) match {
                case Some(w) =>
                    val l = f.enemies.%(e => game.states.contains(e)).%(h.canRemove(c))./~(e => e.at(c).of[Warrior].distinct.%(_.is[Tenacious].not)./(p => (e, p)))

                    if (l.none)
                        SunnyAdvocatesReplaceAction(f, h, rest)
                    else
                        Ask(f).group(h, "convert in", c).each(l)((e, p) => SunnyAdvocatesReplaceOneAction(w, h, c, e, p, rest))

                case None =>
                    SunnyAdvocatesReplaceAction(f, h, rest)
            }

        case SunnyAdvocatesReplaceOneAction(f, h, c, e, p, rest) =>
            f.reserve --> f.warrior --> c

            h.log("converted", p.of(e), "into", f.warrior.of(f), "in", c)

            ForcedRemoveAction(h, c, e, p, 0, NoMessage, SunnyAdvocatesReplaceAction(f, h, rest))

        case SunnyAdvocatesSpreadAction(f, h) =>
            val near = clearings.%(c => h.at(c).has(Advocate).not && f.present(c)).%(h.canPlace)

            HirelingPlaceAnyAction(f, h, Advocate, 1, near.some.|(clearings), Repeat)

        // BAT MESSENGERS
        case BatMessengersAskAction(f, l, then) =>
            if (l.none || f.pool(f.warrior).not)
                then
            else
                Ask(f).each(l.distinct.%(f.canPlace))(c => BatMessengersAction(f, l, c, BatMessengersAskAction(f, l :- c, then))).skip(then)

        case BatMessengersAction(f, l, c, then) =>
            game.highlights :+= PlaceHighlight($(c))

            f.reserve --> f.warrior --> c

            f.log("placed", f.warrior.of(f), "in", c, "with", BatMessengers)

            then

        // RIVER ROAMERS
        case SetupHirelingAction(f, h : RiverRoamers.type) =>
            game.states += h -> new BasicHirelingState(h)

            game.hirelings :+= h

            log(f, "setup", h)

            Ask(f).each(game.riverside.%(h.canPlace))(c => RiverRoamersPlaceAction(f, h, c, SetupNextHirelingAction)).bail(SetupNextHirelingAction)

        case OnTakeHirelingAction(f, h : RiverRoamers.type, then) if h.all(Lilypad).none =>
            Ask(f).group(h, "return").each(game.riverside.%(h.canPlace))(c => RiverRoamersPlaceAction(f, h, c, then)).bail(then)

        case RiverRoamersPlaceAction(f, h, c, then) =>
            game.highlights :+= PlaceHighlight($(c))

            val l = h.pooled(Roamer).times(Roamer).take(2) ++ (h.at(c).has(Lilypad).not && h.pool(Lilypad)).$(Lilypad)

            h.reserve --> l --> c

            h.log("placed", l./(_.of(h)).comma, "in", c)

            then

        case RiverRoamersCraftAction(f, h) =>
            f.used :+= RoamersCrafted

            val l = h.all(Lilypad)./(_.asset)

            f.extraCraft ++= l

            h.log("lent", f, l./(dt.CraftSuit).merge, "for crafting")

            RiverRoamersCraftMenuAction(f, h, false)

        case RiverRoamersCraftMoreAction(f, h) =>
            RiverRoamersCraftMenuAction(f, h, true)

        // the usual craft menu, but right after lending the lilypads (a recorded action) it ends with Done, not Cancel
        case RiverRoamersCraftMenuAction(f, h, cancel) =>
            YYSelectObjectsAction(f, f.hand)
                .withGroup("Craft".styled(f) ~ " with " ~ (f.craft ++ f.frogCraft ++ f.extraCraft).diff(f.crafted)./(dt.CraftSuit).merge)
                .withRule(f.craftable)
                .withThen(d => CraftAction(f, d, NoMessage, CraftPerformAction(f, d, NoMessage)))(d => game.desc("Craft", d, "with", d.as[CraftCard]./~(_.cost./(dt.CraftSuit)).merge))("Craft")
                .withExtra($(NoHand, cancel.?(CancelAction).|(Repeat.as("Done"))))

        case RiverRoamersMainAction(f, h) =>
            val cc = clearings.%(c => h.at(c).has(Lilypad))
            val done = UsedEffectAction(f, h, Repeat)

            Ask(f)
                .group(RiverRoamers)
                .add(HirelingBattleEachAction(f, h, cc, false, done).as("Battle".styled(h), dt.Battle, "at each", Lilypad.of(h))(h).!(cc.%(h.canAttackIn).none, "no targets"))
                .each(game.riverside.%(h.canPlace))(c => RiverRoamersPlaceAction(f, h, c, done).!(h.pool(Roamer).not && h.pool(Lilypad).not, "no pieces"))
                .cancel

        // FROG TINKERS
        case FrogTinkersAction(f, h) =>
            if (deck.none && pile.any)
                Shuffle[DeckCard](pile, ShufflePileAction(_, ForceAction(FrogTinkersAction(f, h))))
            else {
                f.used :+= h

                deck.$.starting.foreach { d =>
                    deck --> d --> h.display

                    f.extraCraft :+= cardAsset(d)

                    h.log("tinkered with", d)
                }

                Repeat
            }

        // FARMERS
        case SetupHirelingAction(f, h : ProsperousFarmers.type) =>
            game.states += h -> new BasicHirelingState(h)

            game.hirelings :+= h

            log(f, "setup", h)

            FarmersSetupAction(f, h)

        case FarmersSetupAction(f, h) =>
            if (h.all(Farm).num >= 2 || h.pool(Farm).not)
                SetupNextHirelingAction
            else
                Ask(f).each(clearings.%(h.canBuild).%(c => h.at(c).has(Farm).not))(c => FarmersPlaceAction(f.as[Faction].|(h), h, c, true, true, FarmersSetupAction(f, h))).bail(SetupNextHirelingAction)

        case FarmersPlaceAction(f, h, c, farm, warrior, then) =>
            game.highlights :+= PlaceHighlight($(c))

            h.reserve --> (farm.$(Farm) ++ (warrior && h.pool(FarmerWarrior)).$(FarmerWarrior)) --> c

            h.log("placed", farm.?(Farm.of(h)), (farm && warrior).?("and"), warrior.?(FarmerWarrior.of(h)), "in", c)

            then

        case OnTakeHirelingAction(f, h : ProsperousFarmers.type, then) if h.all(Farm).none && h.pool(Farm) =>
            Ask(f).group(h, "return").each(clearings.%(f.present).%(h.canBuild))(c => FarmersPlaceAction(f, h, c, true, true, then)).bail(then)

        case ProsperousFarmersMainAction(f, h) =>
            if (h.all(Farm).exists(f.rules))
                Ask(f).add(ProsperousFarmersHarvestAction(f, h)).cancel
            else
                Ask(f).add(ProsperousFarmersSecondAction(f, h).as("Battle at a", Farm.of(h), "or place", FarmerWarrior.sof(h))(h)).cancel

        case ProsperousFarmersHarvestAction(f, h) =>
            f.used :+= h

            DrawCardsAction(f, 1, WithEffect(h), AddCardsAction(f, ProsperousFarmersFarmAction(f, h)))

        case ProsperousFarmersFarmAction(f, h) =>
            val cc = clearings.%(h.canBuild)

            if (h.pool(Farm) && cc.any) {
                val least = cc./(warriorsIn).min

                Ask(f).group(h, "plant a", Farm.of(h)).each(cc.%(c => warriorsIn(c) == least))(c => FarmersPlaceAction(f, h, c, true, false, ProsperousFarmersSecondAction(f, h)))
            }
            else
                ProsperousFarmersSecondAction(f, h)

        case ProsperousFarmersSecondAction(f, h) =>
            if (f.used.has(h).not)
                f.used :+= h

            val cc = h.all(Farm).distinct

            Ask(f)
                .group(ProsperousFarmers)
                .add(BattleInitAction(f, h, NoMessage, cc.%(h.canAttackIn), $, Repeat).as("Battle".styled(h), dt.Battle, "at a", Farm.of(h))(h).!(cc.%(h.canAttackIn).none, "no targets"))
                .add(HirelingRecruitEachAction(f, h, h.all(Farm), Repeat).!(cc.%(h.canPlace).none, "no farms").!(h.pool(FarmerWarrior).not, "no warriors"))
                .bailout(Repeat.as("Skip"))

        case GiveAwayHirelingToAction(f, h : StrugglingFarmers.type, e) if { h.former = |(f) ; false } =>
            NeverAction

        case OnTakeHirelingAction(f, h : StrugglingFarmers.type, then) if h.former.any =>
            val former = h.former.get

            h.former = None

            clearings.%(c => h.at(c).any).%(c => former.rules(c).not).foreach { c =>
                val l = h.at(c)

                h.from(c) --> h.reserve

                h.log("left", c, "taking", l./(_.of(h)).comma, "with them")
            }

            OnTakeHirelingAction(f, h, then)

        case StrugglingFarmersMainAction(f, h) =>
            val cc = clearings.%(f.rules).%(h.canPlace)
            val done = UsedEffectAction(f, h, Repeat)

            Ask(f)
                .group(h, "can settle in a ruled clearing")
                .some(cc)(c =>
                    (h.pool(Farm) && h.canBuild(c) && h.pool(FarmerWarrior)).$(FarmersPlaceAction(f, h, c, true, true, DrawCardsAction(f, 1, WithEffect(h), AddCardsAction(f, done)))) ++
                    (h.pool(Farm) && h.canBuild(c)).$(FarmersPlaceAction(f, h, c, true, false, done)) ++
                    h.pool(FarmerWarrior).$(FarmersPlaceAction(f, h, c, false, true, done))
                )
                .cancel

        // farms give no points when removed
        case ForcedRemoveScoreAction(e, c, f : FarmersHireling, p @ Farm, n, m, then) if n != 0 =>
            ForcedRemoveScoreAction(e, c, f, p, 0, m, then)

        // THE BANDS: who moved where this turn, for the Bands' hold on enemies that move into their clearing
        case MoveCompleteAction(self, f, from, to : Clearing, l, e, then) if options.has(HirelingsByTheCard) && e.has(PopularBand).not =>
            if (game.movedInto.has((f, to)).not)
                game.movedInto :+= (f, to)

            if (hirelings.has(StreetBand) && StreetBand.friends(f).not && StreetBand.at(to).any && StreetBand.enchanted.has(to).not) {
                StreetBand.log("started playing in", to)

                StreetBand.enchanted :+= to
            }

            if (hirelings.has(PopularBand) && PopularBand.friends(f).not && PopularBand.at(to).any && PopularBand.enchanted.has(to).not) {
                PopularBand.log("started playing in", to)

                PopularBand.enchanted :+= to
            }

            MoveCompleteAction(self, f, from, to, l, e :+ PopularBand, then)

        // FOREST PATROL: one of the Patrol warriors removed at once goes on the card, the rest to the supply
        case ForcedRemoveCleanupAction(f, then) if options.has(HirelingsByTheCard) && hirelings.has(ForestPatrol) && clearings.exists(c => ForestPatrol.limbo(c).any) =>
            val h = ForestPatrol

            clearings.%(c => h.limbo(c).any).starting.foreach { c =>
                val p = h.limbo(c).$.starting.get.piece

                h.limbo(c) --> p --> h.hospital

                h.log("carried", p.name.styled(h), "to the first aid")
            }

            clearings.foreach(c => h.limbo(c) --> game.dead)

            ForcedRemoveCleanupAction(f, then)

        // THE BRIGAND: "R" items go under the ruins as if the Vagabond were in play, unless a Vagabond or the Hundreds put them
        case SetupDoneAction if hirelings.has(Brigand) && game.ruins.any && game.ruins.values.forall(_.items.none) =>
            AddRuinItemsAction(SetupDoneAction)

        case SetupHirelingAction(f, h : FrogTinkers.type) =>
            game.states += h -> new FrogTinkersState(h)

            game.hirelings :+= h

            log(f, "setup", h)

            SetupNextHirelingAction

        case SetupHirelingAction(f, h : StrugglingFarmers.type) =>
            game.states += h -> new StrugglingFarmersState(h)

            game.hirelings :+= h

            log(f, "setup", h)

            SetupNextHirelingAction

        // SETUP
        case SetupHirelingAction(f, h : PawnHireling) =>
            game.states += h -> new PawnHirelingState(h)

            game.hirelings :+= h

            log(f, "setup pawn", h)

            SetupNextHirelingAction

        case SetupHirelingAction(f, h) =>
            game.states += h -> new BasicHirelingState(h)

            game.hirelings :+= h

            log(f, "setup basic", h)

            SetupNextHirelingAction

        // HELPERS
        case PlaceHirelingWarriorsAction(f, h, l, c, then) =>
            game.highlights :+= PlaceHighlight($(c))

            h.reserve --> l --> c

            log(f, "placed", l./(_.of(h)).comma, "in", c)

            then

        case PlaceHirelingWarriorAction(f, h, p, c, then) =>
            game.highlights :+= PlaceHighlight($(c))

            h.reserve --> p --> c

            log(f, "placed", p.of(h), "in", c)

            then

        // TURN
        case OnTakeHirelingAction(f, h, then) =>
            then

        case HireHirelingAction(f, h) =>
            f.effects :-= NewHireling

            game.unhired :-= h

            TakeHirelingAction(f, h, HirelingPurchasesAction(f))

        case _ => UnknownContinue
    }

}
