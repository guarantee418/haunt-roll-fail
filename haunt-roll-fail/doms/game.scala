package doms
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

import hrf.tracker4._
import hrf.tracker4.implicits._
import hrf.elem._

import doms.elem._


trait Faction extends NamedToString with Styling with Elementary with BasePlayer with Record {
    def short = name
    def style = name.toLowerCase
    override def elem : Elem = name.styled(this)(styles.title)(xstyles.bold)
}


case object Mammal extends Faction
case object Reptile extends Faction
case object Bird extends Faction
case object Amphibian extends Faction
case object Arachnid extends Faction
case object Insect extends Faction

case object Factions {
    def order : $[Faction] = $(Mammal, Reptile, Bird, Amphibian, Arachnid, Insect)
}


trait Resource extends NamedToString with Styling with Record {
    override def elem = name.styled(this)
    def img = Image(name, styles.token)
}

case object Grass extends Resource
case object Grub extends Resource
case object Meat extends Resource
case object Seed extends Resource
case object Sun extends Resource
case object Water extends Resource

object Resources {
    def all = $[Resource](Grass, Grub, Meat, Seed, Sun, Water)
}

abstract class Terrain(val points : $[Int]) extends NamedToString with Styling with Record {
    def id = toString.toLowerCase
    override def elem = name.styled(this)
}

case object Tundra extends Terrain($(1))
case object Mountain extends Terrain($(3, 2))
case object Desert extends Terrain($(4, 2))
case object Forest extends Terrain($(5, 3, 2))
case object Jungle extends Terrain($(6, 3, 2))
case object Savannah extends Terrain($(7, 4, 2))
case object Wetland extends Terrain($(8, 4, 2, 1))
case object Sea extends Terrain($(9, 5, 3, 2))


object Terrains {
    val stack = 6.times(Sea) ++ 3.times(Desert) ++ 3.times(Forest) ++ 3.times(Jungle) ++ 3.times(Mountain) ++ 3.times(Savannah) ++ 3.times(Wetland)
}


abstract class DominanceCard(val id : String) extends NamedToString with Record with Elementary {
    override def elem = name.hl
}

case object Aquatic       extends DominanceCard("aquatic")
case object Biodiversity  extends DominanceCard("biodiversity")
case object Biomass       extends DominanceCard("biomass")
case object Blight        extends DominanceCard("blight")
case object Catastrophe   extends DominanceCard("catastrophe")
case object ColdSnap      extends DominanceCard("cold-snap") { override val name = "Cold Snap" }
case object Disease       extends DominanceCard("disease")
case object Ecodiversity  extends DominanceCard("ecodiversity")
case object Evolution     extends DominanceCard("evolution")
case object Fecundity     extends DominanceCard("fecundity")
case object Fertile       extends DominanceCard("fertile")
case object Habitat       extends DominanceCard("habitat")
case object Hibernation   extends DominanceCard("hibernation")
case object IceAge        extends DominanceCard("ice-age") { override val name = "Ice Age" }
case object IceSheet      extends DominanceCard("ice-sheet") { override val name = "Ice Sheet" }
case object Immigrants    extends DominanceCard("immigrants")
case object Instinct      extends DominanceCard("instinct")
case object Intelligence  extends DominanceCard("intelligence")
case object MassExodus    extends DominanceCard("mass-exodus") { override val name = "Mass Exodus" }
case object Metamorphosis extends DominanceCard("metamorphosis")
case object NicheBiomes   extends DominanceCard("niche-biomes") { override val name = "Niche Biomes" }
case object Nocturnal     extends DominanceCard("nocturnal")
case object Omnivore      extends DominanceCard("omnivore")
case object Parasitism    extends DominanceCard("parasitism")
case object Predator      extends DominanceCard("predator")
case object Symbiotic     extends DominanceCard("symbiotic")

case object SurvivalCard extends DominanceCard("survival")

object DominanceCards {
    def deck = $[DominanceCard](
        Aquatic       ,
        Biodiversity  ,
        Biomass       ,
        Blight        ,
        Catastrophe   ,
        ColdSnap      ,
        Disease       ,
        Ecodiversity  ,
        Evolution     ,
        Fecundity     ,
        Fertile       ,
        Habitat       ,
        Hibernation   ,
        IceSheet      ,
        Immigrants    ,
        Instinct      ,
        Intelligence  ,
        MassExodus    ,
        Metamorphosis ,
        NicheBiomes   ,
        Nocturnal     ,
        Omnivore      ,
        Parasitism    ,
        Predator      ,
        Symbiotic     ,
    )

    def special = $(IceAge, SurvivalCard)
}

trait Phase extends Elementary with NamedToString with Record {
    override def elem : Elem = name.hh

}

trait Fixed { self : Phase => }
trait Invisible { self : Phase => }

case class NotYetAvailable(faction : Faction) extends Phase
case class Unassigned(faction : Faction) extends Phase

case object Initiative extends Phase
case class Adaptation(n : Int) extends Phase { override def elem = "Adaptation".hh ~ " " ~ roman(n).hl }
case object PrepareRegression extends Phase with Invisible
trait CommonRegression extends Phase
case class Regression(n : Int) extends CommonRegression { override def elem = "Regression".hh ~ " " ~ roman(n).hl }
case object ReptileRegression extends CommonRegression with Fixed { override def elem = "Free " ~ Reptile.elem ~ " " ~ "Regression".hh }
case object CompleteRegression extends Phase with Invisible
case class Abundance(n : Int) extends Phase { override def elem = "Abundance".hh ~ " " ~ roman(n).hl }
case object Wasteland extends Phase
case object CompleteWasteland extends Phase with Invisible
case object Depletion extends Phase
case class Glaciation(n : Int) extends Phase { override def elem = "Glaciation".hh ~ |(n).but(0)./(n => " " ~ roman(n).hl) }
case class Speciation(r : Resource) extends Phase { override def elem = "Speciation".hh ~ " " ~ r.name.styled(r) }
case object InsectSpeciation extends Phase with Fixed { override def elem = "Free " ~ Insect.elem ~ " " ~ "Speciation".hh }
case class Wanderlust(n : Int) extends Phase { override def elem = "Wanderlust".hh ~ " " ~ roman(n).hl }
case class Migration(n : Int) extends Phase { override def elem = "Migration".hh ~ " " ~ n.hl ~ " species"}
case object BirdMigration extends Phase with Fixed { override def elem = Bird.elem ~ " " ~ "go up to two tiles when migrate".hh }
case object ArachnidCompetition extends Phase with Fixed { override def elem = "Free " ~ Arachnid.elem ~ " " ~ "Competition".hh }
case class Competition(a : Terrain, b : Terrain) extends Phase { override def elem = "Competition".hh ~ " " ~ a.elem  ~ " " ~ b.elem }
case class Domination(n : Int) extends Phase { override def elem = "Domination".hh ~ " " ~ roman(n).hl }
case object MammalExtinction extends Phase with Fixed { override def elem = Mammal.elem ~ " save one specie" }
case object Extinction extends Phase with Fixed
case object Survival extends Phase with Fixed
case object Reseed extends Phase with Fixed

object Phases {
    def flow = $(
        Initiative,
        Adaptation(1),
        Adaptation(2),
        Adaptation(3),
        PrepareRegression,
        Regression(1),
        Regression(2),
        ReptileRegression,
        CompleteRegression,
        Abundance(1),
        Abundance(2),
        Wasteland,
        CompleteWasteland,
        Depletion,
        Glaciation(0),
        Glaciation(1),
        Glaciation(2),
        Glaciation(3),
        Speciation(Meat),
        Speciation(Sun),
        Speciation(Seed),
        Speciation(Water),
        Speciation(Grub),
        Speciation(Grass),
        InsectSpeciation,
        Wanderlust(1),
        Wanderlust(2),
        Wanderlust(3),
        Migration(7),
        Migration(6),
        Migration(5),
        Migration(4),
        Migration(3),
        Migration(2),
        BirdMigration,
        ArachnidCompetition,
        Competition(Jungle, Wetland),
        Competition(Wetland, Desert),
        Competition(Desert, Forest),
        Competition(Forest, Savannah),
        Competition(Savannah, Mountain),
        Competition(Mountain, Sea),
        Competition(Sea, Jungle),
        Domination(1),
        Domination(2),
        Domination(3),
        Domination(4),
        Domination(5),
        MammalExtinction,
        Extinction,
        Survival,
        Reseed,
    )
}


trait Board {
    val name : String
}

case class Tile(x : Int, y : Int) extends Region with GameElementary with Record {
    def elem(implicit game : Game) = game.terrain.get(this)./{ x =>
        (x.name + " " + roman(game.letters(this))).styled(x)
    }.|(("Empty" + game.letters.get(this)./(" " + " ABCDEFGHKLMNOPRSTVWXYZ"(_)).|("")).hh).styled(xlo.nowrap)
}

case class Corner(x : Int, y : Int) extends GameElementary with Record {
    def elem(implicit game : Game) = game.tiles(this)./(_.elem).join(" / ")
}

case object BaseBoard extends Board with Record {
    val name = "Base Map"

    val tiles : $[Tile] = -9.to(9)./~(i => -10.to(10)./~(j => (i % 3 == 0 && j % 2 == 0 && (i / 3 + j / 2) % 2 == 0 && (i.abs + j.abs < 18)).?(Tile(i, j))))

    val corners : $[Corner] = tiles./~(t => $(Corner(t.x + 2, t.y), Corner(t.x - 2, t.y), Corner(t.x + 1, t.y + 2), Corner(t.x + 1, t.y - 2), Corner(t.x - 1, t.y + 2), Corner(t.x - 1, t.y - 2))).distinct
}

case class Figure(faction : Faction, index : Int) extends Record

case class Pawn(faction : Faction, index : Int) extends Record

trait Region {
    def -->(f : Faction)(implicit tracker : IdentityTracker[Region, Figure]) = tracker.get(this).%(_.faction == f).take(1).single.|!(f.name + " not found in " + this)
}

case class Reserve(faction : Faction) extends Region
case class Eliminated(faction : Faction) extends Region


trait GameImplicits {
    implicit def factionToState(f : Faction)(implicit game : Game) : FactionState = game.states(f).as[FactionState].get

    implicit def regionToContent(r : Region)(implicit game : Game) : $[Figure] = game.species.get(r)
    implicit def phaseToContent(r : Phase)(implicit game : Game) : $[Pawn] = game.pawns.get(r)
    implicit def phaseToEx(r : Phase)(implicit game : Game) = KEx[Phase, Pawn](r)(game.pawns)

    def log(s : Any*)(implicit game : Game) {
        game.log(s : _*)
    }

    implicit class FactionEx(f : Faction)(implicit game : Game) {
        def log(s : Any*) { if (game.logging) game.log((f +: s.$) : _*) }
    }

    def options(implicit game : Game) = game.options
    def factions(implicit game : Game) = game.factions
    def board(implicit game : Game) = game.board

    def color(f : Faction) : String = f @@ {
        case Mammal => "white"
        case Reptile => "black"
        case Insect => "green"
        case Amphibian => "blue"
        case Arachnid => "red"
        case Bird => "yellow"
    }

    def roman(n : Int) : String = n @@ {
        case 1 => "I"
        case 2 => "II"
        case 3 => "III"
        case 4 => "IV"
        case 5 => "V"
        case 6 => "VI"
        case 7 => "VII"
        case 8 => "VIII"
        case 9 => "IX"
        case 10 => "X"
        case 11 => "XI"
        case 12 => "XII"
        case _ => "?"
    }

    implicit def species(implicit game : Game) = game.species
    implicit def pawns(implicit game : Game) = game.pawns

    implicit class TileEx(x : Tile)(implicit game : Game) {
        def terrain = game.terrain.get(x)
        def real = game.terrain.contains(x)
        def empty = game.terrain.contains(x).not
        def of(f : Faction) = x.%(_.faction == f)
        def corners = game.corners(x)
        def tiles = game.tiles(x)
    }

    implicit class TileListEx(l : $[Tile])(implicit game : Game) {
        def real = l.%(game.terrain.contains)
        def empty = l.%!(game.terrain.contains)
    }

    implicit class CornerEx(x : Corner)(implicit game : Game) {
        def resource = game.resources.get(x)
        def tiles = game.tiles(x)
    }

    implicit def convert(r : Resource) = Image(r.name, styles.tokenBig)

}


trait RemoveResourceKey extends Key { self : Action =>
    val corner : Corner
}

trait PlaceResourceKey extends Key { self : Action =>
    val corner : Corner
    val resource : Resource
}

trait TileFactionKey extends Key { self : Action =>
    val tile : Tile
    val faction : Faction
}

trait SpeciationKey extends Key { self : Action =>
    val tile : Tile
    val faction : Faction
    val count : Int
}

trait UnitKey extends Key { self : Action =>
    val unit : Figure
}

trait UnitCompetitionKey extends UnitKey { self : Action => }

trait TileKey extends Key { self : Action =>
    val tile : Tile
}

trait DirectionKey extends Key { self : Action =>
    val faction : Faction
    val from : Tile
    val to : Tile
}

trait DirectionToKey extends DirectionKey { self : Action => }
trait DirectionFromKey extends DirectionKey { self : Action => }

trait DominationKey extends TileKey { self : Action => }

trait TileTerrainKey extends Key { self : Action =>
    val tile : Tile
    val terrain : Terrain
}


//[[ BLACKER
class FactionState(val faction : Faction)(implicit game : Game) {
    var vp = 0

    val fixed : $[Resource] = faction @@ {
        case Mammal => $(Meat, Meat)
        case Reptile => $(Sun, Sun)
        case Insect => $(Grass, Grass)
        case Bird => $(Seed, Seed)
        case Amphibian => $(Water, Water, Water)
        case Arachnid => $(Grub, Grub)
    }

    var adaptation : $[Resource] = $

    def resources = fixed ++ adaptation

    var regression : $[Resource] = $

    def claim(x : Tile) : Int = {
        if (x.of(faction).none)
            return 0

        val rr = x.corners./~(_.resource)
        val ll = fixed ++ adaptation
        Resources.all./(r => rr.count(r) * ll.count(r)).sum
    }

    def survival() = board.tiles.%(_.terrain.has(Tundra))./(_.of(faction).num).sum

    val reserve : Region = game.species.register(Reserve(faction), _.faction == faction,  1.to(game.setup.num @@ {
        case 6 => 34
        case 5 => 39
        case 4 => 44
        case 3 => 49
        case 2 => 54
    })./(Figure(faction, _)))

    val eliminated : Region = game.species.register(Eliminated(faction), _.faction == faction, $)

    val pawns : Phase = game.pawns.register(Unassigned(faction), _.faction == faction, 1.to(game.setup.num @@ {
        case 6 => 3
        case 5 => 4
        case 4 => 5
        case 3 => 6
        case 2 => 7
    })./(Pawn(faction, _)))

    val future : Phase = game.pawns.register(NotYetAvailable(faction), _.faction == faction, 8.to(10)./(Pawn(faction, _)))

    def score(n : Int, message : Any*) {
        faction.vp += n

        faction.log("scored", n.vp.styled(styles.title), message.$)
    }

    def bonus(n : Int, message : Any*) {
        score(min(45, n*(n+1)/2), message : _*)
    }

}
//]]

case class StartAction(version : String) extends StartGameAction with GameVersion
case class ShuffledTerrainAction(shuffled : $[Terrain]) extends ShuffledAction[Terrain]
case class ShuffledDeckAction(shuffled : $[DominanceCard]) extends ShuffledAction[DominanceCard]

case object SetupSpeciesAction extends ForcedAction

case class ReplenishMarketAction(then : ForcedAction) extends ForcedAction
case class ReseedAction(then : ForcedAction) extends ForcedAction
case class ReseedShuffledAction(shuffled : $[Resource], then : ForcedAction) extends ShuffledAction[Resource]

case object IceAgeAction extends ForcedAction

case object PlanningPhaseAction extends ForcedAction
case class PlanningMainAction(f : Faction, then : ForcedAction) extends ForcedAction
case class PlanningAction(f : Faction, p : Phase, then : ForcedAction) extends ForcedAction with Key
case class PlanningDoneAction(f : Faction) extends ForcedAction

case object ExecutionPhaseAction extends ForcedAction
case class ExecuteMainAction(p : Phase) extends ForcedAction
case class ExecuteAction(p : Phase) extends ForcedAction
case class EndAction(p : Phase) extends ForcedAction

case class ImproveInitiativeAction(f : Faction, then : ForcedAction) extends ForcedAction
case class AdaptationAction(f : Faction, r : Resource, then : ForcedAction) extends ForcedAction
case class RegressionAction(f : Faction, r : Resource, then : ForcedAction) extends ForcedAction
case class AbundanceAction(f : Faction, r : Resource, c : Corner, then : ForcedAction) extends ForcedAction with PlaceResourceKey { val corner = c ; val resource = r }
case class WastelandAction(f : Faction, r : Resource, then : ForcedAction) extends ForcedAction
case class DepletionAction(f : Faction, r : Resource, c : Corner, then : ForcedAction) extends ForcedAction with RemoveResourceKey { val corner = c }
case class GlaciationMainAction(f : Faction, then : ForcedAction) extends ForcedAction with Soft
case class GlaciationAction(f : Faction, x : Tile, then : ForcedAction) extends ForcedAction with TileTerrainKey { val tile = x ; val terrain = Tundra }

case class SpeciationMainAction(f : Faction, r : Resource, c : Corner, l : $[Tile], cancel : Boolean, then : ForcedAction) extends ForcedAction with Soft with PlaceResourceKey { val corner = c ; val resource = r }
case class SpeciationAction(f : Faction, x : Tile, n : Int, then : ForcedAction) extends ForcedAction with SpeciationKey { val faction = f ; val tile = x ; val count = n }

case class WanderlustMainAction(f : Faction, then : ForcedAction) extends ForcedAction with Soft
case class WanderlustStackAction(f : Faction, t : Terrain, stack : Int, then : ForcedAction) extends ForcedAction with Soft
case class WanderlustAction(f : Faction, x : Tile, t : Terrain, stack : Int, then : ForcedAction) extends ForcedAction with TileTerrainKey { val tile = x ; val terrain = t }
case class WanderlustResourceAction(f : Faction, r : Resource, c : Corner, then : ForcedAction) extends ForcedAction with PlaceResourceKey { val corner = c ; val resource = r }
case class WanderlustMoveAction(f : Faction, x : Tile, then : ForcedAction) extends ForcedAction
case class WanderlustMoveFromAction(f : Faction, o : Tile, u : Figure, x : Tile, then : ForcedAction) extends ForcedAction with DirectionFromKey with UnitKey { val unit = u ; val faction = f ; val from = o ; val to = x }

case class MigrateMainAction(f : Faction, n : Int, l : $[Figure], then : ForcedAction) extends ForcedAction with Soft
case class MigrateFromAction(f : Faction, o : Tile, u : Figure, then : ForcedAction) extends ForcedAction with Soft with TileFactionKey { val faction = f ; val tile = o }
case class MigrateToAction(f : Faction, o : Tile, u : Figure, x : Tile, then : ForcedAction) extends ForcedAction with DirectionToKey with TileKey { val faction = f ; val from = o ; val to = x ; val tile = x }

case class CompetitionMainAction(f : Faction, l : $[Terrain], then : ForcedAction) extends ForcedAction with Soft
case class CompetitionAction(f : Faction, x : Tile, e : Faction, u : Figure, then : ForcedAction) extends ForcedAction with UnitCompetitionKey { val unit = u }

case class DominationAction(f : Faction, x : Tile, then : ForcedAction) extends ForcedAction with DominationKey { val tile = x }
case class TakeAction(f : Faction, x : Tile, c : DominanceCard, then : ForcedAction) extends ForcedAction
case class CardAction(f : Faction, x : Tile, c : DominanceCard, then : ForcedAction) extends ForcedAction

case class AquaticResourceAction(f : Faction, r : Resource, c : Corner, then : ForcedAction) extends ForcedAction with PlaceResourceKey { val corner = c ; val resource = r }

case class BiomassAction(f : Faction, l : $[Tile], then : ForcedAction) extends ForcedAction with Soft

case class BlightMainAction(f : Faction, x : Tile, then : ForcedAction) extends ForcedAction with Soft with TileKey { val tile = x }
case class BlightAction(f : Faction, c : Corner, then : ForcedAction) extends ForcedAction with RemoveResourceKey { val corner = c }

case class MassExodusMainAction(f : Faction, x : Tile, cancel : Boolean, then : ForcedAction) extends ForcedAction with Soft with TileKey { val tile = x }
case class MassExodusToAction(f : Faction, x : Tile, d : Tile, then : ForcedAction) extends ForcedAction with Soft
case class MassExodusAction(f : Faction, x : Tile, d : Tile, l : $[Figure], then : ForcedAction) extends ForcedAction with DirectionKey { val faction = f ; val from = x ; val to = d }

case class HibernationMainAction(f : Faction, x : Tile, then : ForcedAction) extends ForcedAction with Soft with TileKey { val tile = x }
case class HibernationAction(f : Faction, x : Tile, n : Int, then : ForcedAction) extends ForcedAction with SpeciationKey { val faction = f ; val tile = x ; val count = n }

case class CatastropheMainAction(f : Faction, x : Tile, then : ForcedAction) extends ForcedAction with Soft with TileKey { val tile = x }
case class CatastropheAction(f : Faction, l : $[Tile], then : ForcedAction) extends ForcedAction with Soft

case class SymbioticAction(l : $[Faction], shuffled : $[Resource], then : ForcedAction) extends ShuffledAction[Resource]

case class FecundityMainAction(f : Faction, l : $[Tile], then : ForcedAction) extends ForcedAction with Soft

case class PredatorMainAction(f : Faction, l : $[Tile], then : ForcedAction) extends ForcedAction with Soft

case class EvolutionMainAction(f : Faction, l : $[Faction], then : ForcedAction) extends ForcedAction with Soft
case class EvolutionAction(f : Faction, x : Tile, e : Faction, then : ForcedAction) extends ForcedAction

case class DiseaseMainAction(l : $[Faction], then : ForcedAction) extends ForcedAction with Soft
case class DiseaseAction(f : Faction, r : Resource, then : ForcedAction) extends ForcedAction

case class ImmigrantsMainAction(l : $[Faction], then : ForcedAction) extends ForcedAction with Soft
case class ImmigrantsPawnAction(f : Faction, then : ForcedAction) extends ForcedAction
case class ImmigrantsAction(f : Faction, then : ForcedAction) extends ForcedAction

case class FertileAction(f : Faction, x : Tile, then : ForcedAction) extends ForcedAction

case class MetamorphosisAction(f : Faction, r : Resource, then : ForcedAction) extends ForcedAction

case class SaveMammalAction(f : Faction, x : Tile, then : ForcedAction) extends ForcedAction

case class GameOverAction(winner : Faction) extends ForcedAction
case class GameOverWonAction(self : Faction, f : Faction) extends BaseInfo("Game Over")(f, "won", "(" ~ NameReference(f.name, f).hl ~ ")")

case object DoNothingAction extends ForcedAction


trait Expansion {
    def perform(a : Action, soft : Void)(implicit game : Game) : Continue

    implicit class ActionMatch(val a : Action) {
        def @@(t : Action => Continue) = t(a)
        def @@(t : Action => Boolean) = t(a)
    }
}

class Game(val setup : $[Faction], val options : $[Meta.O]) extends BaseGame with ContinueGame with LoggedGame {
    private implicit val game = this

    var isOver = false

    val expansions : $[Expansion] = $(CommonExpansion)

    var order : $[Faction] = Factions.order.intersect(setup)
    var factions : $[Faction] = order.reverse
    var states = Map[Faction, FactionState]()

    val board = BaseBoard

    var terrain : Map[Tile, Terrain] = $(
        Tile(0, 0) -> Tundra,
        Tile(0,-4) -> Wetland,
        Tile(0, 4) -> Mountain,
        Tile(3, 2) -> Desert,
        Tile(3, -2) -> Savannah,
        Tile(-3, 2) -> Forest,
        Tile(-3, -2) -> Jungle,
    ).toMap

    var placed : $[Terrain] = terrain.values.$

    var letters : Map[Tile, Int] = $(
        Tile(0, 0) -> 1,
        Tile(0,-4) -> 1,
        Tile(0, 4) -> 1,
        Tile(3, 2) -> 1,
        Tile(3, -2) -> 1,
        Tile(-3, 2) -> 1,
        Tile(-3, -2) -> 1,
    ).toMap

    var resources : Map[Corner, Resource] = $(
        Corner(2, -4) -> Grass,
        Corner(2, 0) -> Grass,
        Corner(4, 0) -> Sun,
        Corner(1, 2) -> Sun,
        Corner(2, 4) -> Meat,
        Corner(-1, 2) -> Meat,
        Corner(-2, 4) -> Seed,
        Corner(-2, 0) -> Seed,
        Corner(-4, 0) -> Grub,
        Corner(-1, -2) -> Grub,
        Corner(-2, -4) -> Water,
        Corner(1, -2) -> Water,
    ).toMap

    var deck : $[DominanceCard] = $
    var market : $[DominanceCard] = $
    var discard : $[DominanceCard] = $

    var adaptation : $[Resource] = $
    var regression : $[Resource] = $
    var abundance : $[Resource] = $
    var wasteland : $[Resource] = $
    var depletion : $[Resource] = $
    var wanderlust : $[Resource] = $

    var stack1 : $[Terrain] = $
    var stack2 : $[Terrain] = $
    var stack3 : $[Terrain] = $

    var tile1 : |[Terrain] = None
    var tile2 : |[Terrain] = None
    var tile3 : |[Terrain] = None

    var dominated : $[Tile] = $

    var saved : $[Figure] = $

    object highlight {
        var faction : $[Faction] = $
        var cylinder : |[Phase] = None
        var current : |[Faction] = None
    }

    var iceAge : Boolean = false

    implicit val species = new IdentityTracker[Region, Figure]

    implicit val pawns = new IdentityTracker[Phase, Pawn]

    def usage(r : Resource) : Int = factions./(_.adaptation.count(r)).sum + resources.values.count(_ == r) +
        adaptation.count(r) +
        regression.count(r) +
        abundance.count(r) +
        wasteland.count(r) +
        depletion.count(r) +
        wanderlust.count(r)

    def available : $[Resource] = Resources.all./~(r => (20 - usage(r)).times(r))

    def dsc(t : Tile) : Elem = terrain.get(t)./{ x =>
        (x.name + " " + roman(letters(t))).styled(x)
    }.|(("Empty" + letters.get(t)./(" " + "ABCDEFGHKLMNOPRSTVWXYZ"(_))).hh)

    def corners(t : Tile) = board.corners.%(c => ((c.x - t.x).abs + (c.y - t.y).abs).between(2, 3))
    def tiles(t : Corner) = board.tiles.%(c => ((c.x - t.x).abs + (c.y - t.y).abs).between(2, 3))
    def tiles(t : Tile) = board.tiles.%(c => ((c.x - t.x).abs + (c.y - t.y).abs).between(4, 5))


    def reletter() {
        val l = terrain.keys./~(tiles).$.diff(terrain.keys.$).sortBy(t => t.x * 1000 + t.y)

        l.indexed.foreach { (t, i) =>
            letters += t -> (i + 1)
        }
    }

    def info(waiting : $[Faction], self : |[Faction], actions : $[UserAction]) : $[Info] = {
        Info((tile1.$ ++ stack1.num.times(0x2B21.toChar.toString.hh.spn(xstyles.larger150))).intersperse(None))(Break ~ "Terrain stacks") ::
        Info((tile2.$ ++ stack2.num.times(0x2B21.toChar.toString.hh.spn(xstyles.larger150))).intersperse(None))(Break ~ "Terrain stacks") ::
        Info((tile3.$ ++ stack3.num.times(0x2B21.toChar.toString.hh.spn(xstyles.larger150))).intersperse(None))(Break ~ "Terrain stacks") ::
        Info(game.discard.num.hlb, "played,", game.deck.num.hlb, "in the deck")("Domination Cards") ::
        $
    }
    def convertForLog(s : $[Any]) : $[Any] = s./~{
        case Empty => None

        case NotInLog(_) => None
        case AltInLog(_, m) => |(m)
        case d : DominanceCard => |(OnClick(d, d.elem.spn(xlo.pointer)))
        case l : $[Any] => convertForLog(l)
        case x => |(x)
    }

    override def log(s : Any*) {
        super.log(convertForLog(s.$) : _*)
    }

    def loggedPerform(action : Action, soft : Void) : Continue = {
        // println("> " + action)

        val c = action.as[SelfPerform]./(_.perform(soft)).|(internalPerform(action, soft))

        highlight.faction = c match {
            case Ask(f, _) => $(f)
            case MultiAsk(a, _) => a./(_.faction)
            case _ => Nil
        }

        // println("< " + c)

        c
    }

    def internalPerform(action : Action, soft : Void) : Continue = {
        expansions.foreach { e =>
            e.perform(action, soft) @@ {
                case UnknownContinue =>
                case Force(another) =>
                    if (action.isSoft.not && another.isSoft)
                        soft()

                    return another.as[SelfPerform]./(_.perform(soft)).|(internalPerform(another, soft))
                case TryAgain => return internalPerform(action, soft)
                case c => return c
            }
        }

        throw new Error("unknown continue on " + action)
    }
}

object CommonExpansion extends Expansion {
    def scoreTile(x : Tile)(implicit game : Game) {
        val points = x.terrain./~(_.points)

        val l = game.order.reverse.%(x.of(_).any)

        l.sortBy(x.of(_).num).reverse.lazyZip(points).foreach { (e, n) =>
            e.score(n, "from", x.terrain)
        }
    }

    def perform(action : Action, soft : Void)(implicit game : Game) = action @@ {
        // INIT
        case StartAction(version) =>
            log("HRF".hl, "version", gaming.version.hlb)
            log("Dominant Species".hlb.styled(styles.title))

            if (version != gaming.version)
                log("Saved game version", version.hlb)

            options.foreach { o =>
                log(o.group, o.valueOn)
            }

            game.setup.foreach { f =>
                game.states += f -> new FactionState(f)
            }

            board.tiles.foreach { t => species.register(t) }

            Phases.flow.foreach { p => pawns.register(p) }

            Shuffle[Terrain](Terrains.stack, ShuffledTerrainAction(_))

        case ShuffledTerrainAction(l) =>
            log("Terrain tiles were shuffled")

            game.stack1 = l.take(8)
            game.stack2 = l.drop(8).take(8)
            game.stack3 = l.drop(8).drop(8).take(8)

            Shuffle[DominanceCard](DominanceCards.deck, ShuffledDeckAction(_))

        case ShuffledDeckAction(l) =>
            log("Dominance deck was shuffled")

            game.deck = l ++ $(IceAge)

            SetupSpeciesAction

        case SetupSpeciesAction =>
            factions./(f => (f @@ {
                case Mammal => $(Forest, Mountain, Mountain, Desert)
                case Reptile => $(Mountain, Desert, Desert, Savannah)
                case Insect => $(Desert, Savannah, Savannah, Wetland)
                case Amphibian => $(Savannah, Wetland, Wetland, Jungle)
                case Arachnid => $(Wetland, Jungle, Jungle, Forest)
                case Bird => $(Jungle, Forest, Forest, Mountain)
            }).foreach { t =>
                f.reserve --> f --> board.tiles.%(game.terrain.get(_).has(t)).only
            })

            game.reletter()

            ReplenishMarketAction(ReseedAction(PlanningPhaseAction))

        case ReplenishMarketAction(then) =>
            var any = false

            while (game.market.num < 5 && game.deck.any) {
                game.market ++= game.deck.take(1)
                game.deck = game.deck.drop(1)
                any = true
            }

            if (any)
                log("Market replenished")

            then

        case ReseedAction(then) =>
            if (Glaciation(0).none)
                Glaciation(1) --> Glaciation(0)

            if (Glaciation(1).none)
                Glaciation(2) --> Glaciation(1)

            if (Glaciation(2).none)
                Glaciation(3) --> Glaciation(2)

            game.dominated = $
            game.saved = $

            game.regression = game.adaptation
            game.adaptation = $

            game.depletion = game.wasteland
            game.wasteland = game.abundance
            game.abundance = $

            game.wanderlust = $

            if (game.tile1.none) {
                game.tile1 = game.stack1.starting
                game.stack1 = game.stack1.dropFirst
            }

            if (game.tile2.none) {
                game.tile2 = game.stack2.starting
                game.stack2 = game.stack2.dropFirst
            }

            if (game.tile3.none) {
                game.tile3 = game.stack3.starting
                game.stack3 = game.stack3.dropFirst
            }

            Shuffle[Resource](game.available, ReseedShuffledAction(_, then))

        case ReseedShuffledAction(l, then) =>
            game.adaptation = l.take(4)
            game.abundance = l.drop(4).take(4)
            game.wanderlust = l.drop(4).drop(4).take(4)

            log("Elements reseeded")

            Milestone(then)

        // PLANNING
        case PlanningPhaseAction =>
            game.highlight.cylinder = None

            game.highlight.current = factions.starting

            val f = factions.first

            PlanningMainAction(f, PlanningDoneAction(f))

        case PlanningMainAction(f, then) =>
            Ask(f).group("Plan")
                .each(Phases.flow.notOf[Invisible])(p =>
                    PlanningAction(f, p, then).as(p./(c => Image(color(c.faction) + "-" + "cylinder", styles.cylinder)), p)
                        .!(p.is[Fixed])
                        .!(p.any)
                        .!(p.is[Regression] && game.regression.none)
                        .!(p.is[Wasteland.type] && game.wasteland.none)
                        .!(p.is[Depletion.type] && game.depletion.none)
                        .!(p.is[Wanderlust.type] && game.tile1.none && game.tile2.none && game.tile3.none)
                )


        case PlanningAction(f, p, then) =>
            f.log("planned", p)

            f.pawns.take(1) --> p

            then

        case PlanningDoneAction(f) =>
            val rest = (factions.dropWhile(_ != f).drop(1) ++ factions).%(_.pawns.any)

            game.highlight.current = rest.starting

            if (rest.any)
                PlanningMainAction(rest.first, PlanningDoneAction(rest.first))
            else
                Milestone(ExecutionPhaseAction)

        case ExecutionPhaseAction =>
            ExecuteMainAction(Initiative)

        case ExecuteMainAction(p) =>
            game.highlight.cylinder = |(p)
            game.highlight.current = p.$.single./(_.faction)

            ExecuteAction(p)

        case EndAction(p) =>
            ExecuteMainAction(Phases.flow.dropWhile(_ != p).drop(1).first)

        // EXECUTE
        case ExecuteAction(p @ Initiative) =>
            p.single./(_.faction)./{ f =>
                log(p)

                p.$ --> f.pawns

                Ask(f).group(Initiative)
                    .add(ImproveInitiativeAction(f, PlanningMainAction(f, EndAction(p))).as("Improve initiative").!(factions.starting.has(f)))
                    .skip(PlanningMainAction(f, EndAction(p)))
                    .needOk
            }.|(Then(EndAction(p)))

        case ImproveInitiativeAction(f, then) =>
            val a = factions.takeWhile(_ != f)
            val b = factions.dropWhile(_ != f)

            game.factions = a.dropLast ++ $(b.first) ++ $(a.last) ++ b.dropFirst

            f.log("improved initiative")

            then

        case ExecuteAction(p @ Adaptation(_)) =>
            p.single./(_.faction)./{ f =>
                log(p)

                p.$ --> f.pawns

                YYSelectObjectsAction(f, game.adaptation)
                    .withGroup("Adaptation".hh)
                    .withRule(_ => f.resources.num < 6)
                    .withThen { r =>
                        AdaptationAction(f, r, EndAction(p)).as("Add", r)
                    }("Add")
                    .withExtras(EndAction(p).as("Skip"))
                    .ask
            }.|(Then(EndAction(p)))

        case AdaptationAction(f, r, then) =>
            f.adaptation :+= r

            game.adaptation :-= r

            f.log("gained", r, "adaptation")

            then

        case ExecuteAction(p @ PrepareRegression) =>
            factions.foreach { f =>
                f.regression = game.regression.distinct.intersect(f.adaptation)
            }

            EndAction(p)

        case ExecuteAction(p : CommonRegression) =>
            val f = p.single./(_.faction).||((p == ReptileRegression).?(Reptile).%(factions.has))

            f./{ f =>
                game.highlight.current = |(f)

                if (p.any || f.regression.any)
                    log(p)

                p.$ --> f.pawns

                Ask(f).group("Regression".hh)
                    .each(f.regression) { r =>
                        RegressionAction(f, r, EndAction(p)).as("Protect", r, r.img)
                    }
                    .skip(EndAction(p))
            }.|(Then(EndAction(p)))

        case RegressionAction(f, r, then) =>
            f.regression :-= r

            f.log("protected", r, "from", "Regression".hl)

            then

        case ExecuteAction(p @ CompleteRegression) =>
            factions.foreach { f =>
                f.adaptation = f.adaptation.diff(f.regression)
                f.regression.foreach { r =>
                    f.log("lost", r, "due to", "Regression".hl)
                }
            }

            EndAction(p)

        case ExecuteAction(p @ Abundance(_)) =>
            p.single./(_.faction)./{ f =>
                log(p)

                p.$ --> f.pawns

                YYSelectObjectsAction(f, game.abundance)
                    .withGroup("Abundance".hh)
                    .withThens { r =>
                        board.corners.diff(game.resources.keys.$).%(c => game.tiles(c).real.any)./( t =>
                            AbundanceAction(f, r, t, EndAction(p)).as(r, "in", t)
                        )
                    }
                    .withExtras(EndAction(p).as("Skip"))
                    .ask
            }.|(Then(EndAction(p)))

        case AbundanceAction(f, r, t, then) =>
            game.abundance :-= r

            game.resources += t -> r

            f.log("placed", r, t)

            then

        case ExecuteAction(p @ Wasteland) =>
            p.single./(_.faction)./{ f =>
                log(p)

                p.$ --> f.pawns

                Ask(f).group("Wasteland".hh)
                    .each(game.wasteland) { r =>
                        WastelandAction(f, r, EndAction(p)).as("Protect", r, r.img)
                    }
                    .skip(EndAction(p))
                    .needOk
            }.|(Then(EndAction(p)))

        case WastelandAction(f, r, then) =>
            game.wasteland :-= r

            f.log("removed", r, "from", "Wasteland".hl)

            then

        case ExecuteAction(p @ CompleteWasteland) =>
            board.corners.%(_.resource.$.intersect(game.wasteland).any).foreach { c =>
                if (game.tiles(c).%(_.terrain.has(Tundra)).any) {
                    log(c.resource, "was removed from", c)

                    game.resources -= c
                }
            }

            EndAction(p)

        case ExecuteAction(p @ Depletion) =>
            p.single./(_.faction)./{ f =>
                log(p)

                p.$ --> f.pawns

                YYSelectObjectsAction(f, game.depletion)
                    .withGroup("Depletion".hh)
                    .withThens { r =>
                        board.corners.%(_.resource.has(r))./( c =>
                            DepletionAction(f, r, c, EndAction(p)).as(r, "in", c)
                        )
                    }
                    .withExtras(EndAction(p).as("Skip"))
                    .ask
            }.|(Then(EndAction(p)))

        case DepletionAction(f, r, c, then) =>
            game.resources -= c

            f.log("depleted", r, "in", c)

            then

        case ExecuteAction(p @ Glaciation(0)) =>
            p.single./(_.faction)./{ f =>
                log(p)

                p.$ --> f.pawns

                Then(GlaciationMainAction(f, EndAction(p)))
            }.|(Then(EndAction(p)))

        case GlaciationMainAction(f, then) if game.terrain.values.$.count(Tundra) >= 12 =>
            Then(then)

        case GlaciationMainAction(f, then) =>
            Ask(f).group("Glaciation")
                .each(board.tiles.real.%(_.terrain.but(Tundra).any).%(_.tiles.exists(_.terrain.has(Tundra))))(x => GlaciationAction(f, x, then).as(x))
                .skip(then)
                .needOk

        case GlaciationAction(f, x, then) =>
            f.log("glaciated", x)

            game.terrain += x -> Tundra

            game.letters += x -> game.terrain.values.$.count(Tundra)

            val n = game.tiles(x).%(_.terrain.has(Tundra)).num

            f.bonus(n, "glaciating")

            factions.foreach { e =>
                x.of(e).dropFirst --> e.reserve
            }

            board.corners.%(_.resource.any).foreach { c =>
                if (c.tiles./~(_.terrain).count(Tundra) >= 3) {
                    f.log("removed", c.resource, "in", c)

                    game.resources -= c
                }
            }

            then

        case ExecuteAction(p : Glaciation) =>
            EndAction(p)

        case ExecuteAction(p @ Speciation(r)) =>
            p.single./(_.faction)./{ f =>
                log(p)

                p.$ --> f.pawns

                Ask(f).group("Speciation".hh)
                    .each(game.resources.keys.%(c => game.resources(c) == r).$) { c =>
                        SpeciationMainAction(f, r, c, game.tiles(c).real, true, EndAction(p)).as(r, r.img, MDash, c)
                    }
                    .add(EndAction(p).as("Skip")(" "))
            }.|(Then(EndAction(p)))

        case SpeciationMainAction(f, r, c, l, cancel, then) =>
            Ask(f)
                .some(l)(x => 1.to(game.terrain(x) @@ {
                    case Tundra => 1
                    case Mountain | Desert => 2
                    case Forest | Jungle | Savannah => 3
                    case Wetland | Sea => 4
                }).reverse./(n =>
                    SpeciationAction(f, x, n, SpeciationMainAction(f, r, c, l.but(x), false, then)).as(n.times(Image(color(f) + "-" + "cube", styles.cube)))("Speciate", r, r.img, MDash, x)
                        .!(f.reserve.num < n)
                ))
                .group(" ")
                .doneIf(cancel.not)(then)
                .cancelIf(cancel)

        case ExecuteAction(p @ InsectSpeciation) if factions.has(Insect).not =>
            EndAction(p)

        case ExecuteAction(p @ InsectSpeciation) =>
            val f = Insect

            game.highlight.current = |(f)

            Ask(f)
                .each(board.tiles.real)(x =>
                    SpeciationAction(f, x, 1, EndAction(p)).as(Image(color(f) + "-" + "cube", styles.cube))("Speciate", x)
                        .!(f.reserve.num < 1)
                )
                .add(EndAction(p).as("Skip")(" "))

        case SpeciationAction(f, t, n, then) =>
            f.reserve.take(n) --> t

            f.log("speciated", t, "with", n.hl, "species")

            then

        case ExecuteAction(p @ Wanderlust(_)) =>
            p.single./(_.faction)./{ f =>
                log(p)

                p.$ --> f.pawns

                Then(WanderlustMainAction(f, EndAction(p)))
            }.|(Then(EndAction(p)))

        case WanderlustMainAction(f, then) =>
            Ask(f).group("Wanderlust".hh)
                .mut(game.tile1)((a, t) => a.add(WanderlustStackAction(f, t, 1, then).as(t)))
                .mut(game.tile2)((a, t) => a.add(WanderlustStackAction(f, t, 2, then).as(t)))
                .mut(game.tile3)((a, t) => a.add(WanderlustStackAction(f, t, 3, then).as(t)))
                .skip(then)

        case WanderlustStackAction(f, t, stack, then) =>
            val l = game.board.tiles.diff(game.terrain.keys.$).%(game.letters.contains)

            Ask(f).group("Wanderlust".hh, t)
                .each(l)(x => WanderlustAction(f, x, t, stack, then).as(x))
                .cancel

        case WanderlustAction(f, x, t, stack, then) =>
            stack @@ {
                case 1 => game.tile1 = None
                case 2 => game.tile2 = None
                case 3 => game.tile3 = None
            }

            game.terrain += x -> t

            game.letters += x -> (4 - (game.stack1 ++ game.tile1 ++ game.stack2 ++ game.tile2 ++ game.stack3 ++ game.tile3).count(t) + (t == Sea).??(2))

            game.reletter()

            f.log("discovered", x)

            val n = game.tiles(x).real.num

            f.bonus(n, "exploring")

            val l = x.corners.%!(game.resources.contains)

            val next = WanderlustMoveAction(game.order.first, x, then)

            YYSelectObjectsAction(f, game.wanderlust)
                .withGroup("Wanderlust".hh)
                .withThens(r => l./(c => WanderlustResourceAction(f, r, c, next).as(r, "in", c)))
                .withExtras(next.as("Skip"))

        case WanderlustResourceAction(f, r, c, then) =>
            game.wanderlust :-= r

            game.resources += c -> r

            f.log("placed", r, "at", c)

            then

        case WanderlustMoveAction(f, x, then) =>
            val next = game.order.dropWhile(_ != f).dropFirst.starting./(WanderlustMoveAction(_, x, then)).|(then)

            Ask(f).group("Wanderlust".hh, "to", x, "from")
                .some(game.tiles(x).real)(o => o.of(f)./(u => WanderlustMoveFromAction(f, o, u, x, WanderlustMoveAction(f, x, then)).as(Image(color(f) + "-" + "cube", styles.cube))(o)))
                .group(" ")
                .done(next)

        case WanderlustMoveFromAction(f, o, u, x, then) =>
            u --> x

            f.log("moved from", o, "to", x)

            then

        case ExecuteAction(p @ Migration(n)) =>
            p.single./(_.faction)./{ f =>
                log(p)

                p.$ --> f.pawns

                Then(MigrateMainAction(f, n, $, EndAction(p)))
            }.|(Then(EndAction(p)))

        case MigrateMainAction(f, n, l, then) if n == l.num =>
            Then(then)

        case MigrateMainAction(f, n, l, then) =>
            Ask(f).group("Migrate".hh, "from")
                .each(board.tiles.real.%(_.of(f).diff(l).any)) { o =>
                    val u = o.of(f).diff(l).first
                    MigrateFromAction(f, o, u, MigrateMainAction(f, n, l :+ u, then)).as(o)
                }
                .group(" ")
                .done(then)

        case MigrateFromAction(f, o, u, then) =>
            Ask(f).group("Migrate".hh, "from", o)
                .each(game.tiles(o).real.use(l => l ++ (f == Bird).??(l./~(x => x +: game.tiles(x).real).but(o).distinct.diff(l))))(x => MigrateToAction(f, o, u, x, then).as(x))
                .cancel

        case MigrateToAction(f, o, u, x, then) =>
            u --> x

            f.log("moved from", o, "to", x)

            then

        case ExecuteAction(p @ BirdMigration) =>
            EndAction(p)

        case ExecuteAction(p @ ArachnidCompetition) if factions.has(Arachnid).not =>
            EndAction(p)

        case ExecuteAction(p @ ArachnidCompetition) =>
            val f = Arachnid

            game.highlight.current = |(f)

            Ask(f).group(Arachnid, "Competition".hh)
                .some(board.tiles.%(_.of(f).any)) { x =>
                    x.$.%(_.faction != f)./(u => CompetitionAction(f, x, u.faction, u, EndAction(p)).as(u.faction, Image(color(u.faction) + "-" + "cube", styles.cube))(x))
                }
                .skip(EndAction(p))

        case ExecuteAction(p @ Competition(a, b)) =>
            p.single./(_.faction)./{ f =>
                log(p)

                p.$ --> f.pawns

                Then(CompetitionMainAction(f, $(a, b, Tundra), EndAction(p)))
            }.|(Then(EndAction(p)))

        case CompetitionMainAction(f, l, then) =>
            Ask(f).group("Competition".hh)
                .some(board.tiles.%(_.terrain.$.intersect(l).any).%(_.of(f).any)) { x =>
                    x.$.%(_.faction != f)./(u => CompetitionAction(f, x, u.faction, u, CompetitionMainAction(f, l.but(x.terrain), then)).as(u.faction, Image(color(u.faction) + "-" + "cube", styles.cube))(x))
                }
                .add(then.as("Done")(" "))
                .needOkIf(l.any)

        case CompetitionAction(f, x, e, u, then) =>
            u --> e.eliminated

            f.log("eliminated", e, "in", x)

            then

        case ExecuteAction(p @ Domination(_)) =>
            p.single./(_.faction)./{ f =>
                log(p)

                p.$ --> f.pawns

                Ask(f).group("Domination".hh)
                    .each(board.tiles.real.diff(game.dominated)) { x =>
                        DominationAction(f, x, EndAction(p)).as(x).!(x.none)
                    }
                    .skip(EndAction(p))
            }.|(Then(EndAction(p)))

        case DominationAction(initiator, x, then) =>
            game.dominated :+= x

            initiator.log("checked", x)

            scoreTile(x)

            val l = game.order.reverse.%(x.of(_).any)
            val d = l.%(f => f.claim(x) > l.but(f)./(_.claim(x)).maxOr(0)).single

            if (d.any && game.market.any) {
                val f = d.get

                Ask(f).group("Card")
                    .each(game.market)(c => TakeAction(f, x, c, then).as(c))
                    .needOk
            }
            else
                Then(then)

        case TakeAction(f, x, c, then) =>
            game.market :-= c
            game.discard :+= c

            f.log("took", c)

            CardAction(f, x, c, then)

        // ICE AGE
        case CardAction(f, _, IceAge, then) =>
            game.iceAge = true

            game.order.foreach { e =>
                val n = board.tiles.real.%(x => e.claim(x) > factions.but(e)./(_.claim(x)).max).num

                if (n > 0)
                    e.bonus(n, "from", IceAge)
            }

            then

        // AQUATIC
        case CardAction(f, _, Aquatic, then) =>
            val l = board.tiles.%(x => x.terrain.has(Sea) || x.terrain.has(Wetland))./~(game.corners).distinct.%(_.resource.none)

            if (l.any)
                YYSelectObjectsAction(f, game.available.distinct)
                    .withGroup(Aquatic)
                    .withThens(r => l./( c => AquaticResourceAction(f, r, c, then).as(r, "in", c)))
                    .ask
            else
                Then(then)

        case AquaticResourceAction(f, r, c, then) =>
            game.resources += c -> r

            f.log("placed", r, "in", c)

            Ask(f)
                .some(game.tiles(c).%(x => x.terrain.has(Sea) || x.terrain.has(Wetland)))(x => 1.to(4).reverse./(n =>
                    SpeciationAction(f, x, n, then).as(n.times(Image(color(f) + "-" + "cube", styles.cube)))("Place in", x)
                        .!(f.reserve.num < n)
                ))
                .group(" ")
                .skip(then)

        // BIODIVERSITY
        case CardAction(f, _, Biodiversity, then) =>
            val n = board.tiles.real.%(x => x.of(f).any && x.$.diff(x.of(f)).any).num

            f.score(n, "with", Biodiversity)

            then

        // BIOMASS
        case CardAction(f, _, Biomass, then) =>
            val l = board.tiles.real.%(x => x.num > x.corners./~(_.resource).num)

            BiomassAction(f, l, then)

        case BiomassAction(f, l, then) if l.none =>
            Then(then)

        case BiomassAction(f, l, then) =>
            Ask(f).group(Biomass)
                .some(l) { x =>
                    x.$./(u => CompetitionAction(f, x, u.faction, u, BiomassAction(f, l.but(x), then)).as(u.faction, Image(color(u.faction) + "-" + "cube", styles.cube))(x))
                }
                .needOk

        // BLIGHT
        case CardAction(f, _, Blight, then) =>
            Ask(f).group(Blight)
                .each(board.tiles.real) { x =>
                    BlightMainAction(f, x, then).as(x)
                }

        case BlightMainAction(f, x, then) =>
            val l = x.corners.%(_.resource.any)

            if (l.num <= 1)
                Then(then)
            else
                Ask(f).group(Blight)
                    .each(l) { c =>
                        BlightAction(f, c, BlightMainAction(f, x, then)).as(c.resource, "in", c)
                    }

        case BlightAction(f, c, then) =>
            f.log("removed", c.resource, "in", c)

            game.resources -= c

            then

        // FECUNDITY
        case CardAction(f, _, Fecundity, then) =>
            val l = board.tiles.%(_.of(f).any)

            FecundityMainAction(f, l, then)

        case FecundityMainAction(f, l, then) if l.none =>
            Then(then)

        case FecundityMainAction(f, l, then) if f.reserve.none =>
            Then(then)

        case FecundityMainAction(f, l, then) =>
            Ask(f).group(Fecundity)
                .each(l)(x => SpeciationAction(f, x, 1, FecundityMainAction(f, l.but(x), then)).as(x))
                .done(then)

        // PREDATOR
        case CardAction(f, _, Predator, then) =>
            val l = board.tiles.%(_.of(f).any).%(_.$.exists(_.faction != f))

            PredatorMainAction(f, l, then)

        case PredatorMainAction(f, l, then) if l.none =>
            Then(then)

        case PredatorMainAction(f, l, then) =>
            Ask(f).group(Predator)
                .some(l) { x =>
                    x.$.%(_.faction != f)./(u => CompetitionAction(f, x, u.faction, u, PredatorMainAction(f, l.but(x), then)).as(u.faction, Image(color(u.faction) + "-" + "cube", styles.cube))(x))
                }
                .needOk

        // MASS EXODUS
        case CardAction(f, _, MassExodus, then) =>
            Ask(f).group(MassExodus)
                .each(board.tiles.real) { x =>
                    MassExodusMainAction(f, x, true, then).as(x)
                }

        case MassExodusMainAction(f, x, cancel, then) =>
            if (x.none)
                Then(then)
            else
                Ask(f).group(MassExodus, "from", x, "to")
                    .each(x.tiles.real) { d =>
                        MassExodusToAction(f, x, d, then).as(d)
                    }
                    .cancelIf(cancel)

        case MassExodusToAction(f, x, d, then) =>
            Ask(f).group(MassExodus, "from", x, "to", d)
                .some(factions) { e =>
                    1.to(x.of(e).num)./(n => MassExodusAction(f, x, d, x.of(e).take(n), then).as(n.times(Image(color(e) + "-" + "cube", styles.cube)))(e))
                }
                .cancel

        case MassExodusAction(f, x, d, l, then) =>
            l --> d

            f.log("moved", l./(_.faction).comma, "from", x, "to", d)

            MassExodusMainAction(f, x, false, then)

        // HIBERNATION
        case CardAction(f, _, Hibernation, then) =>
            if (f.eliminated.none)
                Then(then)
            else
                Ask(f).group(Hibernation)
                    .each(board.tiles.real) { x =>
                        HibernationMainAction(f, x, then).as(x)
                    }

        case HibernationMainAction(f, x, then) =>
            Ask(f).group(Hibernation, "in", x)
                .each(1.to(min(5, f.eliminated.num)).$)(n => HibernationAction(f, x, n, then).as(n.times(Image(color(f) + "-" + "cube", styles.cube))))
                .cancel

        case HibernationAction(f, x, n, then) =>
            val l = f.eliminated.take(n)

            l --> x

            game.saved ++= l

            f.log("hibernated", n.hl, "in", x)

            then

        // CATASTROPHE
        case CardAction(f, _, Catastrophe, then) =>
            Ask(f).group(Catastrophe)
                .each(board.tiles.real) { x =>
                    CatastropheMainAction(f, x, then).as(x)
                }

        case CatastropheMainAction(f, x, then) =>
            if (x.num > 1)
                Ask(f).group("Catastrophe".hh, "in", x)
                    .each(x.$)(u => CompetitionAction(f, x, u.faction, u, CatastropheMainAction(f, x, then)).as(u.faction, Image(color(u.faction) + "-" + "cube", styles.cube)))
                    .needOk
            else
                Then(CatastropheAction(f, x.tiles.%(_.any), then))

        case CatastropheAction(f, l, then) if l.none =>
            Then(then)

        case CatastropheAction(f, l, then) =>
            Ask(f).group(Catastrophe)
                .some(l) { x =>
                    x.$./(u => CompetitionAction(f, x, u.faction, u, CatastropheAction(f, l.but(x), then)).as(u.faction, Image(color(u.faction) + "-" + "cube", styles.cube))(x))
                }
                .needOk

        // COLD SNAP
        case CardAction(f, _, ColdSnap, then) =>
            board.tiles.%(_.terrain.has(Tundra)).foreach { x =>
                factions.but(f)./(x.of)./~(_.starting).foreach { u =>
                    u --> u.faction.eliminated

                    u.faction.log("eliminated in", x)
                }
            }

            then

        // EVOLUTION
        case CardAction(f, _, Evolution, then) =>
            EvolutionMainAction(f, $, then)

        case EvolutionMainAction(f, l, then) if l.num >= 2 =>
            Then(then)

        case EvolutionMainAction(f, l, then) =>
            Ask(f).group(Evolution)
                .some(board.tiles.%(_.any)) { x =>
                    val ee = factions.but(f).diff(l).%(e => x.of(e).any)
                    ee./(e => EvolutionAction(f, x, e, EvolutionMainAction(f, l :+ e, then)).as(e, 1.times(Image(color(e) + "-" + "cube", styles.cube)))(x))
                }
                .bail(then)
                .needOk

        case EvolutionAction(f, x, e, then) =>
            x.of(e).first --> e.eliminated

            f.reserve.take(1) --> x

            f.log("replaced", e, "in", x)

            then

        // METAMORPHOSIS
        case CardAction(f, _, Metamorphosis, then) =>
            if (f.adaptation.any)
                YYSelectObjectsAction(f, game.available.distinct)
                    .withGroup(Metamorphosis)
                    .withThen(r => MetamorphosisAction(f, r, then).as("Gain", r))("Gain")
            else
                Then(then)

        case MetamorphosisAction(f, r, then) =>
            f.adaptation :+= r

            f.log("took", r)

            YYSelectObjectsAction(f, f.adaptation)
                .withGroup(Metamorphosis)
                .withThen(r => DiseaseAction(f, r, then).as("Discard", r))("Discard")

        // DISIEASE
        case CardAction(f, _, Disease, then) =>
            val l = game.order.%(_.resources.num > f.resources.num).%(_.adaptation.any)

            DiseaseMainAction(l, then)

        case DiseaseMainAction(l, then) if l.none =>
            Then(then)

        case DiseaseMainAction(l, then) =>
            val f = l.first

            YYSelectObjectsAction(f, f.adaptation)
                .withGroup(Disease)
                .withThen(r => DiseaseAction(f, r, DiseaseMainAction(l.dropFirst, then)).as("Discard", r))("Discard")

        case DiseaseAction(f, r, then) =>
            f.adaptation :-= r

            f.log("lost", r)

            then

        // IMMIGRANTS
        case CardAction(f, _, Immigrants, then) =>
            val l = game.order

            ImmigrantsMainAction(l, then)

        case ImmigrantsMainAction(l, then) if l.none =>
            Then(then)

        case ImmigrantsMainAction(l, then) =>
            val f = l.first
            val next = ImmigrantsMainAction(l.dropFirst, then)

            YYSelectObjectsAction(f, f.adaptation)
                .withGroup(Immigrants)
                .withThen(r => DiseaseAction(f, r, next).as("Discard", r))("Discard Element")
                .withExtras(ImmigrantsPawnAction(f, next).as("Discard Action Pawn".hh).!(f.pawns.none), ImmigrantsAction(f, next).as("Discard Species"))

        case ImmigrantsPawnAction(f, then) =>
            f.pawns.take(1) --> f.future

            f.log("lost", "Action Pawn".hh)

            then

        case ImmigrantsAction(f, then) =>
            board.tiles.foreach { x =>
                x.of(f).dropLast.foreach { u =>
                    u --> u.faction.eliminated

                    u.faction.log("eliminated in", x)
                }
            }

            then

        // HABITAT
        case CardAction(f, _, Habitat, then) =>
            val l = board.corners.diff(game.resources.keys.$).%(c => game.tiles(c).real.any)

            if (l.any)
                YYSelectObjectsAction(f, game.available.distinct)
                    .withGroup(Habitat)
                    .withThens(r => l./(c => AbundanceAction(f, r, c, then).as(r, "in", c)))
                    .ask
            else
                Then(then)

        // ECODIVERSITY
        case CardAction(f, _, Ecodiversity, then) =>
            val n = game.resources.values.%(f.resources.has).num

            f.score(n, "with", Ecodiversity)

            then

        // NOCTURNAL
        case CardAction(f, _, Nocturnal, then) =>
            if (factions.starting.has(f).not)
                ImproveInitiativeAction(f, then)
            else
                then

        // ICE SHEET
        case CardAction(f, _, IceSheet, then) =>
            GlaciationMainAction(f, then)

        // INSTINCT
        case CardAction(f, _, Instinct, then) =>
            if (f.pawns.any)
                PlanningMainAction(f, then)
            else
                then

        // FERTILE
        case CardAction(f, _, Fertile, then) =>
            Ask(f).group(Fertile)
                .each(board.tiles.real.%(_.of(f).any))(x => FertileAction(f, x, then).as(x))
                .needOk

        case FertileAction(f, x, then) =>
            f.log("chose", x)

            f.score(x.num, "with", Fertile)

            then

        // SYMBIOTIC
        case CardAction(f, _, Symbiotic, then) =>
            val l = ($(f) ++ factions.but(f).%(_.resources.num < f.resources.num)).%(_.resources.num < 6)

            ShuffleTake[Resource](game.available, l.num, SymbioticAction(l, _, then))

        case SymbioticAction(l, q, then) =>
            l.lazyZip(q).foreach { (f, r) =>
                f.adaptation :+= r

                f.log("gained", r)
            }

            then

        // NICHE BIOMES
        case CardAction(f, x, NicheBiomes, then) =>
            val points = x.terrain./~(_.points)

            game.order.but(f).foreach { e =>
                if (e.vp > f.vp) {
                    e.vp -= points.first

                    if (e.vp < 0)
                        e.vp = 0

                    e.log("lost", points.first.vp)
                }
            }

            then

        // OMNIVORE
        case CardAction(f, _, Omnivore, then) =>
            f.future.take(1) --> f.pawns

            f.log("gained", 1.hl, "action pawn")

            then

        // PARASITISM
        case CardAction(f, _, Parasitism, then) =>
            game.order.dropWhile(_ != f).foreach { e =>
                e.future.take(1) --> e.pawns

                e.log("gained", 1.hl, "action pawn")
            }

            then

        // INTELLIGENCE
        case CardAction(f, _, Intelligence, then) =>
            game.order.reverse.dropWhile(_ != f).reverse.foreach { e =>
                e.future.take(1) --> e.pawns

                e.log("gained", 1.hl, "action pawn")
            }

            then

        case CardAction(f, _, c, then) =>
            log(c, "// TODO")

            then

        case ExecuteAction(p @ MammalExtinction) if factions.has(Mammal).not =>
            EndAction(p)

        case ExecuteAction(p @ MammalExtinction) =>
            val f = Mammal

            game.highlight.current = |(f)

            val l = board.tiles.real.%(_.of(f).diff(game.saved).any).%(f.claim(_) == 0)

            Ask(f).group("Prevent", "Extinction".hh, "in")
                .each(l)(x => SaveMammalAction(f, x, EndAction(p)).as(x))
                .skip(EndAction(p))

        case SaveMammalAction(f, x, then) =>
            game.saved ++= x.of(f).diff(game.saved).take(1)

            Milestone(then)

        case ExecuteAction(p @ Extinction) =>
            board.tiles.real.foreach { x =>
                factions.%(x.of(_).any).foreach { f =>
                    if (f.claim(x) == 0) {
                        val l = x.of(f)
                        val s = game.saved.intersect(l)

                        l.diff(s) --> f.eliminated

                        if (l.diff(s).any)
                            f.log("became extinct in", x)

                        if (s.any)
                            f.log("saved", s.num.hl, "specie".s(s.num), "in", x)
                    }
                }
            }

            Milestone(EndAction(p))

        case ExecuteAction(p @ Survival) =>
            factions.%(f => f.survival() > factions.but(f)./(_.survival()).max).foreach { f =>
                val n = board.tiles.%(_.terrain.has(Tundra)).%(_.of(f).any).num

                f.bonus(n, "surviving in", Tundra)
            }

            Milestone(EndAction(p))

        case ExecuteAction(p @ Reseed) =>
            if (game.iceAge)
                Milestone(IceAgeAction)
            else
            if (board.tiles.exists(_.any).not && factions.exists(_.reserve.any).not) {
                log("All species are extinct")

                Milestone(IceAgeAction)
            }
            else
                ReplenishMarketAction(ReseedAction(PlanningPhaseAction))

        case IceAgeAction =>
            board.tiles.real.foreach(scoreTile)

            val winner = game.order.%(_.vp == game.order./(_.vp).max).first

            Milestone(GameOverAction(winner))

        case GameOverAction(winner) =>
            val winners = $(winner)

            game.highlight.current = |(winner)
            game.isOver = true

            winners.foreach(f => f.log("won"))

            GameOver(winners, "Game Over", winners./~(f => $(GameOverWonAction(null, f))))

        case DoNothingAction =>
            log("do nothing")

            Ask(game.setup.first)
                .add(DoNothingAction.as("Eh..."))
                .add(DoNothingAction.as("Oh..."))
    }
}
