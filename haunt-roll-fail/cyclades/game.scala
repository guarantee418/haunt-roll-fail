package cyclades
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
import cyclades.elem._

// CYCLADES (Bruno Cathala and Ludovic Maublanc, Matagot, 2009), the base game, after the English rulebook and the Creatures'
// reference booklet as transcribed in the Tabletop Simulator mods. RULES.md lists the rules as read and the choices made.

trait Faction extends BasePlayer with Named with Styling with Record {
    def name = toString
    def short = name
    def id = name.toLowerCase
}

case object Red extends Faction
case object Yellow extends Faction
case object Blue extends Faction
case object Black extends Faction
case object Green extends Faction

object Faction {
    val all = $[Faction](Red, Yellow, Blue, Black, Green)
}


// GODS

sealed abstract class God(val name : String) extends Record with Elementary {
    def id = name.toLowerCase
    def elem = name.styled(styles.god)
}

case object Poseidon extends God("Poseidon")
case object Ares extends God("Ares")
case object Zeus extends God("Zeus")
case object Athena extends God("Athena")
case object Apollo extends God("Apollo")

object God {
    val main = $[God](Poseidon, Ares, Zeus, Athena)
}


// BUILDINGS: one for each of the four main gods; a Metropolis counts as all four

sealed abstract class Building(val name : String, val god : God) extends Record with Elementary {
    def id = name.toLowerCase
    def elem = name.styled(styles.building)
}

case object Port extends Building("Port", Poseidon)
case object Fortress extends Building("Fortress", Ares)
case object Temple extends Building("Temple", Zeus)
case object University extends Building("University", Athena)

object Building {
    val all = $[Building](Port, Fortress, Temple, University)
    def of(g : God) = all.%(_.god == g).single
}


// CREATURES: 17 cards, one of each; five have a figure that stays on the board

sealed abstract class Creature(val name : String, val text : String, val figure : Boolean = false) extends Record with Elementary {
    def id = name.toLowerCase.replace("the ", "")
    def elem = name.styled(styles.creature)
}

case object Fates extends Creature("The Fates", "Collect your revenue again, as at the start of the cycle.")
case object Polyphemus extends Creature("Polyphemus", "Place Polyphemus on an isle. The Fleets around it are pushed one space away (where they can't go, they are destroyed), and no Fleet can come next to the isle while he is there.", true)
case object Dryad extends Creature("Dryad", "Steal a Priest from another player.")
case object Satyr extends Creature("Satyr", "Steal a Philosopher from another player.")
case object Sylph extends Creature("Sylph", "Move your Fleets a total of 10 spaces.")
case object Sphinx extends Creature("Sphinx", "Sell any of your Fleets, Troops, Priests and Philosophers for 2 GP each.")
case object Siren extends Creature("Siren", "Replace another player's lone Fleet with one of yours.")
case object Minotaur extends Creature("Minotaur", "Place the Minotaur on one of your isles: he counts as 2 Troops defending it, never retreats, and is lost after the isle's Troops.", true)
case object Cyclops extends Creature("Cyclops", "Replace one of your buildings with a building of another type.")
case object Kraken extends Creature("Kraken", "Place the Kraken on a sea space, destroying the Fleets there; for 1 GP a space it moves on, destroying the Fleets it meets. No Fleet may enter its space.", true)
case object Chimera extends Creature("Chimera", "Use any Creature in the discard pile, then shuffle the discard pile into the draw pile.")
case object Chiron extends Creature("Chiron", "Place Chiron on one of your isles: Pegasus, the Giant and the Harpy can't be used against it.", true)
case object Pegasus extends Creature("Pegasus", "Move Troops from one of your isles to any isle, with no chain of Fleets.")
case object Harpy extends Creature("Harpy", "Remove a Troop from the board.")
case object Griffon extends Creature("Griffon", "Take half of another player's GP (rounded down).")
case object Giant extends Creature("Giant", "Destroy a building (not a Metropolis).")
case object Medusa extends Creature("Medusa", "Place Medusa on an isle: the Troops there can't leave it.", true)

object Creature {
    val all = $[Creature](Fates, Polyphemus, Dryad, Satyr, Sylph, Sphinx, Siren, Minotaur, Cyclops, Kraken, Chimera, Chiron, Pegasus, Harpy, Griffon, Giant, Medusa)
    // the prices of the track's three spaces
    val prices = $(4, 3, 2)
}

// an offering marker; each player has one, two in a 2-player game
case class Marker(f : Faction, n : Int) extends Record

// a marker outbid on a god, to offer again to another
case class Ousted(m : Marker, g : God) extends Record

// a god's turn this cycle, for the marker that won it
case class Turn(g : God, m : Marker) extends Record

case class Stack(f : Faction, n : Int) extends Record

// a battle, the attacker's units off the board until it ends
case class Battle(attacker : Faction, defender : Faction, area : Area, a : Int, d : Int, minotaur : Boolean, from : |[Isle], then : ForcedAction) extends Record

// fleets on the move: their space, how many, the moves left (and a Sylph's moves left after these)
case class FleetMove(f : Faction, at : Sea, n : Int, steps : Int, sylph : Boolean, then : ForcedAction) extends Record


// VIEWS

case class ViewCreatureInfoAction(self : Faction, creature : Creature) extends BaseInfo(Break ~ "Creatures")(Image("creature:" + creature.id, styles.card)) with ViewObject[Creature] { def obj = creature }

trait AreaKey { self : UserAction =>
    def area : Area
}


// ACTIONS

case class StartAction(version : String) extends StartGameAction with GameVersion
case class CreaturesShuffledAction(shuffled : $[Creature]) extends ShuffledAction[Creature]
case class SeatsShuffledAction(shuffled : $[Marker]) extends ShuffledAction[Marker]

case class CycleStartAction(n : Int) extends ForcedAction
case class FillTrackAction(then : ForcedAction) extends ForcedAction
case class ReshuffledCreaturesAction(shuffled : $[Creature], then : ForcedAction) extends ShuffledAction[Creature]
case class GodsAction(n : Int) extends ForcedAction
case class GodsShuffledAction(shuffled : $[God]) extends ShuffledAction[God]
case class RevenueAction(n : Int) extends ForcedAction

// offerings
case class OfferAction(queue : $[Marker], ousted : |[Ousted]) extends ForcedAction
case class BidMenuAction(self : Faction, m : Marker, g : God, queue : $[Marker], ousted : |[Ousted]) extends BaseAction((game : Game) => game.offerQuestion(m))((game : Game) => game.godOption(g)) with Soft
case class BidAction(self : Faction, m : Marker, g : God, n : Int, queue : $[Marker], ousted : |[Ousted]) extends BaseAction((game : Game) => game.offerQuestion(m) ~ " " ~ g.elem)(n.hl, "GP", (game : Game) => game.payNote(self, n))
case class ApolloBidAction(self : Faction, m : Marker, queue : $[Marker], ousted : |[Ousted]) extends BaseAction((g : Game) => g.offerQuestion(m))(Apollo, "(free)")
case class PayOfferingsAction(n : Int) extends ForcedAction

// the gods' actions
case class GodTurnAction(order : $[Turn]) extends ForcedAction
case class MainAction(f : Faction) extends ForcedAction
case class EndGodTurnAction(self : Faction) extends BaseAction(None)("Done".hl)
case class BuyCreatureAction(self : Faction, slot : Int, c : Creature, cost : Int) extends BaseAction("Creatures")("Call", c, "for", cost.hl, "GP") with ViewObject[Creature] { def obj = c }
case class RecruitFleetAction(self : Faction, sea : Sea, cost : Int) extends BaseAction("Recruit a Fleet", (g : Game) => g.costNote(cost))("in", sea) with AreaKey { def area = sea }
case class RecruitTroopAction(self : Faction, isle : Isle, cost : Int) extends BaseAction("Recruit a Troop", (g : Game) => g.costNote(cost))("on", isle) with AreaKey { def area = isle }
case class RecruitPriestAction(self : Faction, cost : Int) extends BaseAction("Recruit")("a Priest", (g : Game) => g.costNote(cost))
case class RecruitPhilosopherAction(self : Faction, cost : Int) extends BaseAction("Recruit")("a Philosopher", (g : Game) => g.costNote(cost))
case class BuildAction(self : Faction, b : Building, isle : Isle) extends BaseAction("Build a", b, "for", 2.hl, "GP")("on", isle) with AreaKey { def area = isle }
case class ReplaceCreatureAction(self : Faction, slot : Int, c : Creature) extends BaseAction(Zeus, "for", 1.hl, "GP")("Discard", c, "and draw another")

// moving Fleets (Poseidon, 1 GP; the Sylph)
case class FleetsFromAction(self : Faction, sea : Sea, sylph : Int, then : ForcedAction) extends BaseAction((sylph > 0).?("Sylph:".hl ~ " move Fleets (" ~ sylph.hl ~ " spaces left)").|("Move Fleets for " ~ 1.hl ~ " GP"))("from", sea) with AreaKey with Soft { def area = sea }
case class FleetsTakeAction(self : Faction, sea : Sea, n : Int, sylph : Int, then : ForcedAction) extends BaseAction("Move Fleets from", sea)(n.hl, (n == 1).?("Fleet").|("Fleets"))
case class FleetsMenuAction(f : Faction) extends ForcedAction
case class FleetsStepAction(self : Faction, to : Sea) extends BaseAction((g : Game) => g.movingQuestion)("Move to", to, (g : Game) => g.enemyNote(self, to)) with AreaKey { def area = to }
case class FleetsPickUpAction(self : Faction, n : Int) extends BaseAction((g : Game) => g.movingQuestion)("Take along", n.hl, "more")
case class FleetsDropAction(self : Faction, n : Int) extends BaseAction((g : Game) => g.movingQuestion)("Leave", n.hl, "here")
case class FleetsStopAction(self : Faction) extends BaseAction((g : Game) => g.movingQuestion)("Stop here")
case class SylphDoneAction(self : Faction, then : ForcedAction) extends BaseAction("Sylph".hl)("Done")

// moving Troops (Ares, 1 GP; Pegasus)
case class TroopsFromAction(self : Faction, from : Isle, pegasus : Boolean, then : ForcedAction) extends BaseAction(pegasus.?(Pegasus.elem ~ ": move Troops").|("Move Troops for " ~ 1.hl ~ " GP"))("from", from) with AreaKey with Soft { def area = from }
case class TroopsToAction(self : Faction, from : Isle, to : Isle, pegasus : Boolean, then : ForcedAction) extends BaseAction("Move Troops from", from)("to", to, (g : Game) => g.isleNote(self, to)) with AreaKey with Soft { def area = to }
case class TroopsMoveAction(self : Faction, from : Isle, to : Isle, n : Int, pegasus : Boolean, then : ForcedAction) extends BaseAction("Move Troops from", from, "to", to)(n.hl, (n == 1).?("Troop").|("Troops"))

// battles
case class BattleRoundAction(b : Battle) extends ForcedAction
case class BattleRolledAction(b : Battle, rolled : $[Int]) extends RolledAction[Int]
case class RetreatAskAction(b : Battle, defender : Boolean) extends ForcedAction
case class RetreatAction(self : Faction, b : Battle, to : Area) extends BaseAction((g : Game) => g.battleQuestion(b))("Retreat to", to) with AreaKey { def area = to }
case class FightOnAction(self : Faction, b : Battle, defender : Boolean) extends BaseAction((g : Game) => g.battleQuestion(b))("Fight on")
case class BattleEndAction(b : Battle) extends ForcedAction

// creatures
case class UseCreatureAction(f : Faction, c : Creature, then : ForcedAction) extends ForcedAction
case class StealPriestAction(self : Faction, e : Faction, then : ForcedAction) extends BaseAction(Dryad)("Steal a Priest from", e)
case class StealPhilosopherAction(self : Faction, e : Faction, then : ForcedAction) extends BaseAction(Satyr)("Steal a Philosopher from", e)
case class HarpyAction(self : Faction, isle : Isle, then : ForcedAction) extends BaseAction(Harpy)("Remove a", (g : Game) => g.owner(isle).elem, "Troop from", isle) with AreaKey { def area = isle }
case class GriffonAction(self : Faction, e : Faction, then : ForcedAction) extends BaseAction(Griffon)("Take half the GP of", e)
case class GiantAction(self : Faction, isle : Isle, b : Building, then : ForcedAction) extends BaseAction(Giant)("Destroy the", b, "on", isle) with AreaKey { def area = isle }
case class SphinxSellFleetAction(self : Faction, sea : Sea, then : ForcedAction) extends BaseAction(Sphinx, "sell for", 2.hl, "GP")("a Fleet in", sea) with AreaKey { def area = sea }
case class SphinxSellTroopAction(self : Faction, isle : Isle, then : ForcedAction) extends BaseAction(Sphinx, "sell for", 2.hl, "GP")("a Troop on", isle) with AreaKey { def area = isle }
case class SphinxSellPriestAction(self : Faction, then : ForcedAction) extends BaseAction(Sphinx, "sell for", 2.hl, "GP")("a Priest")
case class SphinxSellPhilosopherAction(self : Faction, then : ForcedAction) extends BaseAction(Sphinx, "sell for", 2.hl, "GP")("a Philosopher")
case class SphinxDoneAction(self : Faction, then : ForcedAction) extends BaseAction(Sphinx)("Done")
case class SirenAction(self : Faction, sea : Sea, then : ForcedAction) extends BaseAction(Siren)("Replace the", (g : Game) => g.fleets(sea).f.elem, "Fleet in", sea) with AreaKey { def area = sea }
case class SirenFromAction(self : Faction, sea : Sea, from : Sea, then : ForcedAction) extends BaseAction(Siren, "with your Fleet")("from", from) with AreaKey { def area = from }
case class CyclopsAction(self : Faction, isle : Isle, b : Building, to : Building, then : ForcedAction) extends BaseAction(Cyclops, "replace the", b, "on", isle)("with a", to) with AreaKey { def area = isle }
case class ChimeraAction(self : Faction, c : Creature, then : ForcedAction) extends BaseAction(Chimera, "use from the discard pile")(c) with ViewObject[Creature] { def obj = c }
case class ChimeraShuffledAction(shuffled : $[Creature], then : ForcedAction) extends ShuffledAction[Creature]
case class PlaceFigureAction(self : Faction, c : Creature, area : Area, then : ForcedAction) extends BaseAction("Place", c)("on", area) with AreaKey
case class PushAskAction(f : Faction, isle : Isle, then : ForcedAction) extends ForcedAction
case class PushAction(self : Faction, from : Sea, to : |[Sea], isle : Isle, then : ForcedAction) extends BaseAction(Polyphemus, "pushes the", (g : Game) => g.fleets(from).f.elem, "Fleets in", from)(to./(s => "to " ~ s.elem).|("Destroyed".txt)) with AreaKey { def area = to.|(from) }
case class KrakenStepAction(self : Faction, to : Sea, then : ForcedAction) extends BaseAction(Kraken, "moves on for", 1.hl, "GP")("to", to, (g : Game) => g.fleets.get(to)./(s => " (destroys " ~ s.n.hl ~ " " ~ s.f.elem ~ ")").|(Empty)) with AreaKey { def area = to }
case class KrakenDoneAction(self : Faction, then : ForcedAction) extends BaseAction(Kraken)("Stay")

// metropolises
case class MetropolisAction(f : Faction, philosophers : Boolean, then : ForcedAction) extends ForcedAction
case class PlaceMetropolisAction(self : Faction, isle : Isle, philosophers : Boolean, then : ForcedAction) extends BaseAction("Place the Metropolis")("on", isle) with AreaKey { def area = isle }

// Apollo
case class ApolloTurnAction(f : Faction, first : Boolean, then : ForcedAction) extends ForcedAction
case class ProsperityAction(self : Faction, isle : Isle, then : ForcedAction) extends BaseAction(Apollo, "places a prosperity marker")("on", isle) with AreaKey { def area = isle }

case class CycleEndAction(n : Int) extends ForcedAction
case class GameOverAction(winners : $[Faction]) extends ForcedAction
case class GameOverWonAction(self : Faction, f : Faction) extends BaseInfo("Game Over")(f, "won")


class Player(val faction : Faction) {
    var gold = 5
    var priests = 0
    var philosophers = 0
}

class Game(val setup : $[Faction], val options : $[Meta.O]) extends BaseGame with ContinueGame with LoggedGame { game =>
    var isOver = false

    val factions : $[Faction] = setup

    val states = setup./(f => f -> new Player(f)).toMap

    implicit def faction2player(f : Faction) : Player = states(f)

    val layout = Layouts.of(setup.num)

    val two = setup.num == 2

    // the Metropolises that win, at the end of a cycle
    val goal = two.?(3).|(2)

    val markers : $[Marker] = setup./~(f => 0.until(two.?(2).|(1))./(Marker(f, _)))

    // MAP
    var fleets = Map[Sea, Stack]()
    var troops = Map[Isle, Int]()
    var owner = Map[Isle, Faction]()
    var buildings = Map[Isle, $[Building]]()
    var metropolises = Set[Isle]()
    var prosperity = Map[Isle, Int]()
    var figures = Map[Creature, Area]()

    // CREATURES
    var drawPile = $[Creature]()
    var discardPile = $[Creature]()
    var track = $[|[Creature]](None, None, None)
    var templesUsed = Map[Faction, Int]()

    // CYCLE
    var cycle = 0
    var order = $[Marker]()
    var gods = $[God]()
    var hidden = $[God]()
    var bids = Map[God, (Marker, Int)]()
    var apollo = $[Marker]()
    var acted = $[Marker]()

    // the god turn under way
    var current : Faction = null
    var currentGod : God = null
    var recruited = 0
    var moving : |[FleetMove] = None
    var sylphLeft = 0
    var battle : |[Battle] = None
    var lastRoll = $[Int]()

    var highlightFaction = $[Faction]()

    // PIECES

    def isles(f : Faction) : $[Isle] = layout.islands.%(i => owner.get(i).has(f))
    def fleetsOn(f : Faction) = fleets.values.%(_.f == f)./(_.n).sum + moving.%(_.f == f)./(_.n).sum + battle.%(b => b.attacker == f && b.area.is[Sea])./(_.a).sum
    def troopsOn(f : Faction) = troops.toList.filter(p => owner.get(p._1).has(f)).map(_._2).sum + battle.%(b => b.attacker == f && b.area.is[Isle])./(_.a).sum
    def fleetReserve(f : Faction) = 8 - fleetsOn(f)
    def troopReserve(f : Faction) = 8 - troopsOn(f)
    def fleetSeas(f : Faction) : $[Sea] = layout.seas.%(s => fleets.get(s).exists(_.f == f))

    def troopsAt(i : Isle) = troops.getOrElse(i, 0)
    def buildingsOn(i : Isle) : $[Building] = buildings.getOrElse(i, $)
    def ownFleets(f : Faction, s : Sea) = fleets.get(s).%(_.f == f)./(_.n).|(0)

    def slots(i : Isle) = layout.data(i).slots - metropolises.contains(i).??(layout.data(i).metro)
    def freeSlots(i : Isle) = slots(i) - buildingsOn(i).num

    // a Metropolis counts as every building
    def countOf(f : Faction, b : Building) = isles(f)./(i => buildingsOn(i).count(_ == b) + metropolises.contains(i).??(1)).sum
    def metropolisesOf(f : Faction) = isles(f).count(metropolises.contains)

    def krakenAt : |[Sea] = figures.get(Kraken).collect { case s : Sea => s }
    def polyphemusAt : |[Isle] = figures.get(Polyphemus).collect { case i : Isle => i }

    // no Fleet in the Kraken's space, nor next to Polyphemus' isle
    def fleetMayEnter(s : Sea) = krakenAt.has(s).not && polyphemusAt.forall(p => layout.seasAround(p).has(s).not)

    // REVENUE: the isles' prosperity, printed and Apollo's markers, and the commerce spaces with the player's Fleets
    def revenue(f : Faction) = isles(f)./(i => layout.data(i).prosperity + prosperity.getOrElse(i, 0)).sum + layout.commerce.count(c => fleets.get(Sea(c)).exists(_.f == f))

    def prosperityLeft = 16 - prosperity.values.sum

    // OFFERINGS: each Priest takes 1 off an offering, which costs at least 1

    def payment(f : Faction, n : Int) = max(1, n - f.priests)

    def owed(f : Faction, except : |[Marker]) = bids.values.$.%{ case (m, _) => m.f == f && except.has(m).not }./{ case (_, n) => payment(f, n) }.sum

    def maxBid(f : Faction, m : Marker) = {
        val left = f.gold - owed(f, |(m))
        (left >= 1).?(left + f.priests).|(0)
    }

    def offerQuestion(m : Marker) : Elem = two.?("Offering " ~ (m.n + 1).hl ~ " of 2 to").|("Offering to")

    def godOption(g : God) : Elem = g.elem ~ bids.get(g)./{ case (m, n) => (" (" ~ m.f.elem ~ " offered " ~ n.hl ~ ")").spn(styles.small) }.|(Empty)

    def payNote(f : Faction, n : Int) : Elem = (payment(f, n) != n).?((" (pays " + payment(f, n) + " with Priests)").spn(styles.small)).|(Empty)

    def costNote(n : Int) : Elem = (n == 0).?(" (free)".txt).|(" for " ~ n.hl ~ " GP")

    // RECRUITING: the first is free, the next ones cost more

    def recruitCost(g : God, k : Int) : |[Int] = g match {
        case Poseidon => $(0, 1, 2, 3).lift(k)
        case Ares => $(0, 2, 3, 4).lift(k)
        case Zeus | Athena => $(0, 4).lift(k)
        case _ => None
    }

    def fleetSites(f : Faction) : $[Sea] = isles(f)./~(layout.seasAround).distinct.%(s => fleets.get(s).forall(_.f == f) && fleetMayEnter(s))

    // MOVING TROOPS: along a chain of the player's Fleets

    def chainFrom(f : Faction, i : Isle) : $[Sea] = {
        var seen = layout.seasAround(i).%(s => ownFleets(f, s) > 0)
        var front = seen
        while (front.any) {
            val next = front./~(layout.seasNext).distinct.%(s => seen.has(s).not && ownFleets(f, s) > 0)
            seen ++= next
            front = next
        }
        seen
    }

    def linkedIsles(f : Faction, i : Isle) : $[Isle] = chainFrom(f, i)./~(s => layout.neighbors(s).of[Isle]).distinct.but(i)

    def medusaOn(i : Isle) = figures.get(Medusa).has(i)
    def chironOn(i : Isle) = figures.get(Chiron).has(i)
    def minotaurOn(i : Isle) = figures.get(Minotaur).has(i)

    // a player's last isle can be attacked only by a player who would win by taking it
    def mayAttack(f : Faction, i : Isle) : Boolean = owner.get(i) match {
        case Some(e) if e != f => isles(e).num > 1 || metropolisesOf(f) + metropolises.contains(i).??(1) >= goal
        case _ => true
    }

    def troopTargets(f : Faction, from : Isle, pegasus : Boolean) : $[Isle] =
        (troopsAt(from) > 0 && owner.get(from).has(f) && medusaOn(from).not).??(
            pegasus.?(layout.islands.but(from).%(i => chironOn(i).not)).|(linkedIsles(f, from).%(i => polyphemusAt.has(i).not)).%(i => mayAttack(f, i))
        )

    def isleNote(f : Faction, i : Isle) : Elem = owner.get(i) match {
        case Some(e) if e != f && (troopsAt(i) > 0 || minotaurOn(i)) => " (attack " ~ e.elem ~ ")"
        case Some(e) if e != f => " (take from " ~ e.elem ~ ")"
        case None => " (claim)".txt
        case _ => Empty
    }

    def enemyNote(f : Faction, s : Sea) : Elem = fleets.get(s).%(_.f != f)./(e => " (attack " ~ e.n.hl ~ " " ~ e.f.elem ~ ")").|(Empty)

    def movingQuestion : Elem = moving./(m => "Moving " ~ m.n.hl ~ " " ~ (m.n == 1).?("Fleet").|("Fleets") ~ " in " ~ m.at.elem ~ " (" ~ m.steps.hl ~ " " ~ (m.steps == 1).?("move").|("moves") ~ " left)").|(Empty)

    def battleQuestion(b : Battle) : Elem = b.attacker.elem ~ " " ~ b.a.hl ~ " against " ~ b.defender.elem ~ " " ~ (b.d + b.minotaur.??(2)).hl ~ " in " ~ b.area.elem

    // BATTLES: a die and the units each, the defender's Fortresses on the isle or Ports next to the sea space

    def defenseBonus(b : Battle) : Int = b.area match {
        case i : Isle => owner.get(i).has(b.defender).??(buildingsOn(i).count(_ == Fortress) + metropolises.contains(i).??(1))
        case s : Sea => layout.neighbors(s).of[Isle].%(i => owner.get(i).has(b.defender))./(i => buildingsOn(i).count(_ == Port) + metropolises.contains(i).??(1)).sum
    }

    def retreats(b : Battle, defender : Boolean) : $[Area] = {
        val f = defender.?(b.defender).|(b.attacker)
        b.area match {
            case i : Isle =>
                if (defender && (b.minotaur || medusaOn(i)))
                    $
                else
                    linkedIsles(f, i).%(x => owner.get(x).has(f))
            case s : Sea =>
                layout.seasNext(s).%(x => fleets.get(x).forall(_.f == f) && fleetMayEnter(x))
        }
    }

    // CREATURES

    def creatureCost(f : Faction, slot : Int) = {
        val base = Creature.prices(slot)
        val temples = countOf(f, Temple) - templesUsed.getOrElse(f, 0)
        max(1, base - max(0, temples))
    }

    def useTemples(f : Faction, slot : Int) {
        val base = Creature.prices(slot)
        val n = base - creatureCost(f, slot)
        if (n > 0)
            templesUsed += f -> (templesUsed.getOrElse(f, 0) + n)
    }

    // MAIN MENU of a god's turn

    def mainMenu(f : Faction) : Continue = {
        val g = currentGod
        val gold = f.gold
        var ask = Ask(f)

        // Creatures, with every god but Apollo
        track.zipWithIndex.foreach { case (Some(c), i) =>
            val cost = creatureCost(f, i)
            if (cost <= gold)
                ask = ask.add(BuyCreatureAction(f, i, c, cost))
            case _ =>
        }

        val cost = recruitCost(g, recruited).%(_ <= gold)

        g match {
            case Poseidon =>
                cost.%(_ => fleetReserve(f) > 0).foreach { c => ask = ask.each(fleetSites(f))(s => RecruitFleetAction(f, s, c)) }
            case Ares =>
                cost.%(_ => troopReserve(f) > 0).foreach { c => ask = ask.each(isles(f))(i => RecruitTroopAction(f, i, c)) }
            case Zeus =>
                cost.foreach { c => ask = ask.add(RecruitPriestAction(f, c)) }
            case Athena =>
                cost.foreach { c => ask = ask.add(RecruitPhilosopherAction(f, c)) }
            case _ =>
        }

        Building.of(g).%(_ => gold >= 2).foreach { b =>
            ask = ask.each(isles(f).%(i => freeSlots(i) > 0))(i => BuildAction(f, b, i))
        }

        g match {
            case Poseidon if gold >= 1 =>
                ask = ask.each(fleetSeas(f))(s => FleetsFromAction(f, s, 0, MainAction(f)))
            case Ares if gold >= 1 =>
                ask = ask.each(isles(f).%(i => troopTargets(f, i, false).any))(i => TroopsFromAction(f, i, false, MainAction(f)))
            case Zeus if gold >= 1 =>
                track.zipWithIndex.foreach { case (Some(c), i) => ask = ask.add(ReplaceCreatureAction(f, i, c)) ; case _ => }
            case _ =>
        }

        ask.add(EndGodTurnAction(f))
    }

    // INFO

    def info(waiting : $[Faction], self : |[Faction], actions : $[UserAction]) : $[Info] =
        track.flatten./(c => ViewCreatureInfoAction(self.|(null), c))

    // PIECES

    def addFleets(f : Faction, s : Sea, n : Int) {
        if (n > 0)
            fleets += s -> Stack(f, ownFleets(f, s) + n)
    }

    def removeFleets(s : Sea, n : Int) {
        fleets.get(s).foreach { st =>
            if (st.n <= n) fleets -= s
            else fleets += s -> st.copy(n = st.n - n)
        }
    }

    def setTroops(i : Isle, n : Int) {
        if (n > 0) troops += i -> n else troops -= i
    }

    // an isle taken: its buildings and Metropolis go with it
    def take(f : Faction, i : Isle, n : Int) {
        owner += i -> f
        setTroops(i, n)
    }

    def destroyBuilding(i : Isle, b : Building) {
        buildings += i -> (buildingsOn(i) :- b)
    }

    def distinctBuildings(f : Faction) = Building.all.forall(b => isles(f).exists(i => buildingsOn(i).has(b)))

    // the next thing a player's new pieces lead to: a Metropolis, from 4 Philosophers or the 4 buildings
    def checkMetropolis(f : Faction, then : ForcedAction) : ForcedAction =
        if (f.philosophers >= 4)
            MetropolisAction(f, true, then)
        else
        if (distinctBuildings(f))
            MetropolisAction(f, false, then)
        else
            then

    def next(m : Marker) = order((order.indexOf(m) + 1) % order.num)

    def income(f : Faction) {
        val n = revenue(f)
        f.gold += n
        log(f, "collected", n.hl, "GP")
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

    def performInternal(a : Action, soft : Void) : Continue = {
        implicit val action = a

        action match {
            // SETUP
            case StartAction(version) =>
                log("HRF".hl, "version", gaming.version.hlb)
                log("Cyclades".hlb, "(base game)")

                if (version != gaming.version)
                    log("Saved game version", version.hlb)

                log("Players", setup./(_.elem).comma)

                setup.zipWithIndex.foreach { case (f, n) =>
                    layout.starts(n).foreach { st =>
                        val i = Isle(st.isle)
                        val s = Sea(Cell(st.fleet._1, st.fleet._2))
                        take(f, i, 1)
                        addFleets(f, s, 1)
                        log(f, "starts on", i, "with a Fleet in", s)
                    }
                }

                Shuffle[Creature](Creature.all, CreaturesShuffledAction(_))

            case CreaturesShuffledAction(l) =>
                drawPile = l
                Shuffle[Marker](markers, SeatsShuffledAction(_))

            case SeatsShuffledAction(l) =>
                order = l
                log("Offering order", l./(_.f.elem).comma)
                CycleStartAction(1)

            // A CYCLE: the Creatures, the gods, revenue, offerings, the gods' actions
            case CycleStartAction(n) =>
                cycle = n
                templesUsed = Map()
                bids = Map()
                apollo = $
                acted = $

                log(DoubleLine)
                log("Cycle", n.hlb)

                // the Creature on the 2 GP space no one called is discarded; the others move right to fill the spaces
                if (n >= 3) {
                    track(2).foreach { c => discardPile :+= c ; log(c, "was discarded") }
                    track = track.updated(2, None)
                }
                if (n == 2 && track(0).any && track(1).none)
                    track = $(None, track(0), None)
                else
                if (n >= 3) {
                    val l = track.flatten
                    track = (3 - l.num).times(None : |[Creature]) ++ l./(c => |(c))
                }

                FillTrackAction(GodsAction(n))

            case FillTrackAction(then) =>
                // the first cycle fills the 4 GP space, the second the 4 and 3 GP spaces
                val upto = (cycle == 1).?(1).|((cycle == 2).?(2).|(3))
                track.take(upto).indexWhere(_.none) match {
                    case -1 => then
                    case i =>
                        if (drawPile.none) {
                            if (discardPile.none)
                                then
                            else
                                Shuffle[Creature](discardPile, ReshuffledCreaturesAction(_, FillTrackAction(then)))
                        }
                        else {
                            val c = drawPile.head
                            drawPile = drawPile.drop(1)
                            track = track.updated(i, |(c))
                            log(c, "is on the", Creature.prices(i).hl, "GP space")
                            FillTrackAction(then)
                        }
                }

            case ReshuffledCreaturesAction(l, then) =>
                drawPile = l
                discardPile = $
                log("The discard pile was shuffled into the draw pile")
                then

            case GodsAction(n) =>
                if (setup.num == 3 && n % 2 == 0 && hidden.num == 2) {
                    gods = hidden
                    hidden = $
                    RevenueAction(n)
                }
                else
                if ((setup.num == 4 || two) && hidden.any)
                    Shuffle[God](God.main.diff(hidden), GodsShuffledAction(_))
                else
                    Shuffle[God](God.main, GodsShuffledAction(_))

            case GodsShuffledAction(l) =>
                setup.num match {
                    case 5 =>
                        gods = l
                    case 3 =>
                        gods = l.take(2)
                        hidden = l.drop(2)
                    case _ =>
                        // the god face down last cycle comes first
                        val all = hidden ++ l
                        gods = all.take(3)
                        hidden = all.drop(3)
                }
                RevenueAction(cycle)

            case RevenueAction(n) =>
                log("The gods:", gods./(_.elem).comma, "and", Apollo)
                if (hidden.any)
                    log("Face down:", hidden./(_.elem).comma)

                setup.foreach(income)

                OfferAction(order, None)

            // OFFERINGS: in turn order, an outbid player at once offers to another god
            case OfferAction(queue, ousted) =>
                val next = ousted./(o => (o.m, |(o.g))).||(queue.headOption./(m => (m, None : |[God])))
                next match {
                    case None => PayOfferingsAction(cycle)
                    case Some((m, except)) =>
                        val f = m.f
                        val rest = ousted.any.?(queue).|(queue.drop(1))
                        val top = maxBid(f, m)
                        val l = gods.%(g => except.has(g).not && bids.get(g)./(_._2 + 1).|(1) <= top)
                        Ask(f).each(l)(g => BidMenuAction(f, m, g, rest, ousted)).add(ApolloBidAction(f, m, rest, ousted))
                }

            case BidMenuAction(f, m, g, rest, ousted) =>
                val min = bids.get(g)./(_._2 + 1).|(1)
                Ask(f).each(min.to(maxBid(f, m)).$)(n => BidAction(f, m, g, n, rest, ousted)).cancel

            case BidAction(f, m, g, n, rest, _) =>
                val out = bids.get(g)./(_._1)
                bids += g -> (m, n)
                log(f, "offered", n.hl, "to", g)
                out match {
                    case Some(o) =>
                        log(o.f, "was outbid")
                        OfferAction(rest, |(Ousted(o, g)))
                    case None =>
                        OfferAction(rest, None)
                }

            case ApolloBidAction(f, m, rest, _) =>
                apollo :+= m
                log(f, "turned to", Apollo, ("(" + apollo.num + ")").txt)
                OfferAction(rest, None)

            case PayOfferingsAction(_) =>
                gods.foreach { g =>
                    bids.get(g).foreach { case (m, n) =>
                        val p = payment(m.f, n)
                        m.f.gold -= p
                        log(m.f, "paid", p.hl, "GP to", g)
                    }
                }
                GodTurnAction(gods./~(g => bids.get(g)./(b => Turn(g, b._1))) ++ apollo./(m => Turn(Apollo, m)))

            // THE GODS' ACTIONS, in the gods' order, Apollo last
            case GodTurnAction(Nil) =>
                CycleEndAction(cycle)

            case GodTurnAction(Turn(g, m) :: rest) =>
                current = m.f
                currentGod = g
                recruited = 0
                acted :+= m
                log(SingleLine)
                log(m.f, "acts with", g)
                if (g == Apollo)
                    ApolloTurnAction(m.f, apollo.headOption.has(m), GodTurnAction(rest))
                else {
                    pending = rest
                    MainAction(m.f)
                }

            case MainAction(f) =>
                mainMenu(f)

            case EndGodTurnAction(f) =>
                GodTurnAction(pending)

            case ApolloTurnAction(f, first, then) =>
                val n = (isles(f).num > 1).?(1).|(4)
                f.gold += n
                log(f, "received", n.hl, "GP from", Apollo)
                if (first && prosperityLeft > 0 && isles(f).any)
                    Ask(f).each(isles(f))(i => ProsperityAction(f, i, then))
                else
                    then

            case ProsperityAction(f, i, then) =>
                prosperity += i -> (prosperity.getOrElse(i, 0) + 1)
                log(f, "placed a prosperity marker on", i)
                then

            // RECRUITING AND BUILDING
            case RecruitFleetAction(f, s, cost) =>
                f.gold -= cost
                recruited += 1
                addFleets(f, s, 1)
                log(f, "recruited a Fleet in", s, (cost > 0).?("for " + cost + " GP").|(""))
                MainAction(f)

            case RecruitTroopAction(f, i, cost) =>
                f.gold -= cost
                recruited += 1
                setTroops(i, troopsAt(i) + 1)
                log(f, "recruited a Troop on", i, (cost > 0).?("for " + cost + " GP").|(""))
                MainAction(f)

            case RecruitPriestAction(f, cost) =>
                f.gold -= cost
                recruited += 1
                f.priests += 1
                log(f, "recruited a Priest", (cost > 0).?("for " + cost + " GP").|(""))
                MainAction(f)

            case RecruitPhilosopherAction(f, cost) =>
                f.gold -= cost
                recruited += 1
                f.philosophers += 1
                log(f, "recruited a Philosopher", (cost > 0).?("for " + cost + " GP").|(""))
                checkMetropolis(f, MainAction(f))

            case BuildAction(f, b, i) =>
                f.gold -= 2
                buildings += i -> (buildingsOn(i) :+ b)
                log(f, "built a", b, "on", i)
                checkMetropolis(f, MainAction(f))

            // METROPOLISES
            case MetropolisAction(f, phil, then) =>
                val l = isles(f).%(i => metropolises.contains(i).not)
                if (l.none) {
                    if (phil) {
                        f.philosophers -= 4
                        log(f, "discarded 4 Philosophers, with no isle for a Metropolis")
                    }
                    else {
                        Building.all.foreach { b => isles(f).find(i => buildingsOn(i).has(b)).foreach(i => destroyBuilding(i, b)) }
                        log(f, "discarded the 4 buildings, with no isle for a Metropolis")
                    }
                    then
                }
                else
                    Ask(f).each(l)(i => PlaceMetropolisAction(f, i, phil, then))

            case PlaceMetropolisAction(f, i, phil, then) =>
                if (phil) {
                    f.philosophers -= 4
                    log(f, "discarded 4 Philosophers")
                }
                else {
                    // one building of each type, those on the Metropolis' isle first
                    Building.all.foreach { b =>
                        (i +: isles(f).but(i)).find(x => buildingsOn(x).has(b)).foreach(x => destroyBuilding(x, b))
                    }
                    log(f, "discarded a", Port, Fortress, Temple, "and", University)
                }
                metropolises += i
                // the Metropolis covers some building spaces: what doesn't fit is destroyed
                while (freeSlots(i) < 0) {
                    val b = buildingsOn(i).last
                    destroyBuilding(i, b)
                    log("The", b, "on", i, "was destroyed")
                }
                log(f, "founded a", "Metropolis".hl, "on", i)
                checkMetropolis(f, then)

            // ZEUS: a new Creature for 1 GP
            case ReplaceCreatureAction(f, i, c) =>
                f.gold -= 1
                discardPile :+= c
                track = track.updated(i, None)
                log(f, "discarded", c)
                if (drawPile.none && discardPile.any)
                    Shuffle[Creature](discardPile, ReshuffledCreaturesAction(_, ReplaceDrawAction(f, i)))
                else
                    ReplaceDrawAction(f, i)

            case ReplaceDrawAction(f, i) =>
                drawPile.headOption.foreach { c =>
                    drawPile = drawPile.drop(1)
                    track = track.updated(i, |(c))
                    log(c, "took its place")
                }
                MainAction(f)

            // MOVING FLEETS
            case FleetsFromAction(f, s, sylph, then) =>
                val n = ownFleets(f, s)
                Ask(f).each(1.to(n).$.reverse)(k => FleetsTakeAction(f, s, k, sylph, then)).cancel

            case FleetsTakeAction(f, s, n, sylph, then) =>
                if (sylph == 0) {
                    f.gold -= 1
                    log(f, "paid", 1.hl, "GP to move", n.hl, "Fleets from", s)
                }
                else
                    log(f, "moves", n.hl, "Fleets from", s, "with the", Sylph)
                removeFleets(s, n)
                moving = |(FleetMove(f, s, n, (sylph > 0).?(sylph).|(3), sylph > 0, then))
                FleetsMenuAction(f)

            case FleetsMenuAction(f) =>
                val m = moving.get
                val here = ownFleets(f, m.at)
                Ask(f)
                    .each((m.steps > 0).??(layout.seasNext(m.at).%(fleetMayEnter)))(s => FleetsStepAction(f, s))
                    .each(1.to(here).$)(k => FleetsPickUpAction(f, k))
                    .each(1.until(m.n).$)(k => FleetsDropAction(f, k))
                    .add(FleetsStopAction(f))

            case FleetsPickUpAction(f, k) =>
                val m = moving.get
                removeFleets(m.at, k)
                moving = |(m.copy(n = m.n + k))
                log(f, "took along", k.hl, "more")
                FleetsMenuAction(f)

            case FleetsDropAction(f, k) =>
                val m = moving.get
                addFleets(f, m.at, k)
                moving = |(m.copy(n = m.n - k))
                log(f, "left", k.hl, "in", m.at)
                FleetsMenuAction(f)

            case FleetsStepAction(f, to) =>
                val m = moving.get
                val left = m.steps - 1
                if (m.sylph)
                    sylphLeft = left
                fleets.get(to).%(_.f != f) match {
                    case Some(e) =>
                        moving = None
                        log(f, "attacked", e.f, "in", to, "with", m.n.hl, "Fleets")
                        BattleRoundAction(Battle(f, e.f, to, m.n, e.n, false, None, afterFleets(f, m)))
                    case None =>
                        moving = |(m.copy(at = to, steps = left))
                        log(f, "moved to", to)
                        FleetsMenuAction(f)
                }

            case FleetsStopAction(f) =>
                val m = moving.get
                addFleets(f, m.at, m.n)
                moving = None
                if (m.sylph)
                    sylphLeft = m.steps
                afterFleets(f, m)

            case SylphDoneAction(f, then) =>
                sylphLeft = 0
                then

            // MOVING TROOPS
            case TroopsFromAction(f, from, pegasus, then) =>
                Ask(f).each(troopTargets(f, from, pegasus))(i => TroopsToAction(f, from, i, pegasus, then)).cancel

            case TroopsToAction(f, from, to, pegasus, then) =>
                Ask(f).each(1.to(troopsAt(from)).$.reverse)(n => TroopsMoveAction(f, from, to, n, pegasus, then)).cancel

            case TroopsMoveAction(f, from, to, n, pegasus, then) =>
                if (pegasus.not) {
                    f.gold -= 1
                    log(f, "paid", 1.hl, "GP to move Troops")
                }
                setTroops(from, troopsAt(from) - n)
                if (troopsAt(from) == 0)
                    log(f, "left a territory marker on", from)
                owner.get(to) match {
                    case Some(e) if e != f && (troopsAt(to) > 0 || minotaurOn(to)) =>
                        log(f, "landed", n.hl, "Troops on", to, "against", e)
                        BattleRoundAction(Battle(f, e, to, n, troopsAt(to), minotaurOn(to), |(from), then))
                    case Some(e) if e != f =>
                        take(f, to, n)
                        log(f, "took", to, "from", e, "with", n.hl, "Troops")
                        then
                    case _ =>
                        take(f, to, troopsAt(to) + n)
                        log(f, "moved", n.hl, "Troops to", to)
                        then
                }

            // BATTLES
            case BattleRoundAction(b) =>
                battle = |(b)
                if (b.area.is[Isle])
                    setTroops(b.area.asInstanceOf[Isle], b.d)
                Roll[Int]($(CustomDie($(0, 1, 1, 2, 2, 3)), CustomDie($(0, 1, 1, 2, 2, 3))), BattleRolledAction(b, _))

            case BattleRolledAction(b, l) =>
                lastRoll = l
                val bonus = defenseBonus(b)
                val ta = l(0) + b.a
                val td = l(1) + b.d + b.minotaur.??(2) + bonus
                log(b.attacker, "rolled", l(0).hl, "for", ta.hlb, "against", b.defender, "rolled", l(1).hl, "for", td.hlb, (bonus > 0).?("(" + bonus + " from buildings)").|(""))
                var nb = b
                if (ta <= td) {
                    nb = nb.copy(a = nb.a - 1)
                    log(b.attacker, "lost a", b.area.is[Sea].?("Fleet").|("Troop"))
                }
                if (td <= ta) {
                    if (nb.d > 0) {
                        nb = nb.copy(d = nb.d - 1)
                        log(b.defender, "lost a", b.area.is[Sea].?("Fleet").|("Troop"))
                    }
                    else {
                        nb = nb.copy(minotaur = false)
                        figures -= Minotaur
                        log("The", Minotaur, "fell")
                    }
                }
                battle = |(nb)
                nb.area match {
                    case i : Isle => setTroops(i, nb.d)
                    case s : Sea => if (nb.d > 0) fleets += s -> Stack(nb.defender, nb.d) else fleets -= s
                }
                if (nb.a > 0 && (nb.d > 0 || nb.minotaur))
                    RetreatAskAction(nb, true)
                else
                    BattleEndAction(nb)

            case RetreatAskAction(b, defender) =>
                val f = defender.?(b.defender).|(b.attacker)
                val l = retreats(b, defender)
                if (l.none)
                    defender.?(RetreatAskAction(b, false) : ForcedAction).|(BattleRoundAction(b))
                else
                    Ask(f).each(l)(x => RetreatAction(f, b, x)).add(FightOnAction(f, b, defender))

            case FightOnAction(f, b, defender) =>
                defender.?(RetreatAskAction(b, false) : ForcedAction).|(BattleRoundAction(b))

            case RetreatAction(f, b, to) =>
                if (f == b.defender) {
                    to match {
                        case i : Isle =>
                            setTroops(i, troopsAt(i) + b.d)
                            take(b.attacker, b.area.asInstanceOf[Isle], b.a)
                            log(f, "retreated to", i)
                            log(b.attacker, "took", b.area)
                        case s : Sea =>
                            fleets -= b.area.asInstanceOf[Sea]
                            addFleets(f, s, b.d)
                            addFleets(b.attacker, b.area.asInstanceOf[Sea], b.a)
                            log(f, "retreated to", s)
                    }
                }
                else {
                    to match {
                        case i : Isle =>
                            setTroops(i, troopsAt(i) + b.a)
                            log(f, "retreated to", i)
                        case s : Sea =>
                            addFleets(f, s, b.a)
                            log(f, "retreated to", s)
                    }
                }
                battle = None
                b.then

            case BattleEndAction(b) =>
                battle = None
                b.area match {
                    case i : Isle =>
                        if (b.a > 0) {
                            take(b.attacker, i, b.a)
                            log(b.attacker, "took", i)
                        }
                        else
                        if (b.d == 0 && b.minotaur.not)
                            log(b.defender, "kept", i, "with a territory marker")
                        else
                            log(b.defender, "held", i)
                    case s : Sea =>
                        if (b.a > 0) {
                            addFleets(b.attacker, s, b.a)
                            log(b.attacker, "won the battle in", s)
                        }
                        else
                        if (b.d > 0)
                            log(b.defender, "held", s)
                        else
                            log("Both fleets were destroyed in", s)
                }
                b.then

            // CREATURES
            case BuyCreatureAction(f, i, c, cost) =>
                useTemples(f, i)
                f.gold -= cost
                track = track.updated(i, None)
                log(f, "called", c, "for", cost.hl, "GP")
                UseCreatureAction(f, c, DiscardCreatureAction(c, MainAction(f)))

            // the card is discarded once used; a figure stays on the board until the Creature is called again
            case DiscardCreatureAction(c, then) =>
                discardPile :+= c
                then

            case UseCreatureAction(f, c, then) =>
                val others = setup.but(f)
                c match {
                    case Fates =>
                        income(f)
                        then
                    case Dryad =>
                        val l = others.%(_.priests > 0)
                        l.any.?(Ask(f).each(l)(e => StealPriestAction(f, e, then)) : Continue).|(then)
                    case Satyr =>
                        val l = others.%(_.philosophers > 0)
                        l.any.?(Ask(f).each(l)(e => StealPhilosopherAction(f, e, then)) : Continue).|(then)
                    case Harpy =>
                        val l = layout.islands.%(i => troopsAt(i) > 0 && chironOn(i).not)
                        l.any.?(Ask(f).each(l)(i => HarpyAction(f, i, then)) : Continue).|(then)
                    case Griffon =>
                        val l = others.%(_.gold > 1)
                        l.any.?(Ask(f).each(l)(e => GriffonAction(f, e, then)) : Continue).|(then)
                    case Giant =>
                        val l = layout.islands.%(i => chironOn(i).not)./~(i => buildingsOn(i).distinct./(b => GiantAction(f, i, b, then)))
                        l.any.?(Ask(f).add(l) : Continue).|(then)
                    case Pegasus =>
                        val l = isles(f).%(i => troopTargets(f, i, true).any)
                        l.any.?(Ask(f).each(l)(i => TroopsFromAction(f, i, true, then)) : Continue).|(then)
                    case Sylph =>
                        sylphLeft = 10
                        SylphMenuAction(f, then)
                    case Sphinx =>
                        SphinxMenuAction(f, then)
                    case Siren =>
                        val l = layout.seas.%(s => fleets.get(s).exists(st => st.f != f && st.n == 1))
                        l.any.?(Ask(f).each(l)(s => SirenAction(f, s, then)) : Continue).|(then)
                    case Cyclops =>
                        val l = isles(f)./~(i => buildingsOn(i).distinct./~(b => Building.all.but(b)./(to => CyclopsAction(f, i, b, to, then))))
                        l.any.?(Ask(f).add(l) : Continue).|(then)
                    case Chimera =>
                        val l = discardPile.distinct.but(Chimera)
                        l.any.?(Ask(f).each(l)(x => ChimeraAction(f, x, then)) : Continue).|(then)
                    case Kraken =>
                        Ask(f).each(layout.seas)(s => PlaceFigureAction(f, Kraken, s, then))
                    case Polyphemus | Medusa =>
                        Ask(f).each(layout.islands)(i => PlaceFigureAction(f, c, i, then))
                    case Chiron | Minotaur =>
                        isles(f).any.?(Ask(f).each(isles(f))(i => PlaceFigureAction(f, c, i, then)) : Continue).|(then)
                }

            case StealPriestAction(f, e, then) =>
                e.priests -= 1
                f.priests += 1
                log(f, "stole a Priest from", e)
                then

            case StealPhilosopherAction(f, e, then) =>
                e.philosophers -= 1
                f.philosophers += 1
                log(f, "stole a Philosopher from", e)
                checkMetropolis(f, then)

            case HarpyAction(f, i, then) =>
                setTroops(i, troopsAt(i) - 1)
                log(f, "removed a", owner(i), "Troop from", i)
                then

            case GriffonAction(f, e, then) =>
                val n = e.gold / 2
                e.gold -= n
                f.gold += n
                log(f, "took", n.hl, "GP from", e)
                then

            case GiantAction(f, i, b, then) =>
                destroyBuilding(i, b)
                log(f, "destroyed the", b, "on", i)
                then

            case SylphMenuAction(f, then) =>
                val l = fleetSeas(f)
                if (sylphLeft <= 0 || l.none)
                    then
                else
                    Ask(f).each(l)(s => FleetsFromAction(f, s, sylphLeft, SylphMenuAction(f, then))).add(SylphDoneAction(f, then))

            case SphinxMenuAction(f, then) =>
                val again = SphinxMenuAction(f, then)
                Ask(f)
                    .each(fleetSeas(f))(s => SphinxSellFleetAction(f, s, again))
                    .each(isles(f).%(i => troopsAt(i) > 0))(i => SphinxSellTroopAction(f, i, again))
                    .add((f.priests > 0).$(SphinxSellPriestAction(f, again)))
                    .add((f.philosophers > 0).$(SphinxSellPhilosopherAction(f, again)))
                    .add(SphinxDoneAction(f, then))

            case SphinxSellFleetAction(f, s, then) =>
                removeFleets(s, 1)
                f.gold += 2
                log(f, "sold a Fleet in", s)
                then

            case SphinxSellTroopAction(f, i, then) =>
                setTroops(i, troopsAt(i) - 1)
                f.gold += 2
                log(f, "sold a Troop on", i)
                then

            case SphinxSellPriestAction(f, then) =>
                f.priests -= 1
                f.gold += 2
                log(f, "sold a Priest")
                then

            case SphinxSellPhilosopherAction(f, then) =>
                f.philosophers -= 1
                f.gold += 2
                log(f, "sold a Philosopher")
                then

            case SphinxDoneAction(f, then) =>
                then

            case SirenAction(f, s, then) =>
                if (fleetReserve(f) > 0) {
                    val e = fleets(s).f
                    fleets += s -> Stack(f, 1)
                    log(f, "replaced the", e, "Fleet in", s)
                    then
                }
                else
                    Ask(f).each(fleetSeas(f))(x => SirenFromAction(f, s, x, then))

            case SirenFromAction(f, s, from, then) =>
                val e = fleets(s).f
                removeFleets(from, 1)
                fleets += s -> Stack(f, 1)
                log(f, "replaced the", e, "Fleet in", s, "with the one from", from)
                then

            case CyclopsAction(f, i, b, to, then) =>
                buildings += i -> ((buildingsOn(i) :- b) :+ to)
                log(f, "replaced the", b, "on", i, "with a", to)
                checkMetropolis(f, then)

            case ChimeraAction(f, c, then) =>
                discardPile = discardPile :- c
                log(f, "used", c, "from the discard pile")
                UseCreatureAction(f, c, ChimeraEndAction(c, then))

            case ChimeraEndAction(c, then) =>
                discardPile :+= c
                Shuffle[Creature](drawPile ++ discardPile, ChimeraShuffledAction(_, then))

            case ChimeraShuffledAction(l, then) =>
                drawPile = l
                discardPile = $
                log("The discard pile was shuffled into the draw pile")
                then

            case PlaceFigureAction(f, c, area, then) =>
                figures += c -> area
                log(f, "placed", c, "on", area)
                c match {
                    case Kraken =>
                        val s = area.asInstanceOf[Sea]
                        fleets.get(s).foreach { st => fleets -= s ; log(c, "destroyed", st.n.hl, st.f, (st.n == 1).?("Fleet").|("Fleets")) }
                        KrakenMenuAction(f, then)
                    case Polyphemus =>
                        PushAskAction(f, area.asInstanceOf[Isle], then)
                    case _ =>
                        then
                }

            case KrakenMenuAction(f, then) =>
                Ask(f).each((f.gold >= 1).??(layout.seasNext(krakenAt.get)))(s => KrakenStepAction(f, s, then)).add(KrakenDoneAction(f, then))

            case KrakenStepAction(f, s, then) =>
                f.gold -= 1
                figures += Kraken -> s
                log(Kraken, "moved to", s)
                fleets.get(s).foreach { st => fleets -= s ; log(Kraken, "destroyed", st.n.hl, st.f, (st.n == 1).?("Fleet").|("Fleets")) }
                KrakenMenuAction(f, then)

            case KrakenDoneAction(f, then) =>
                then

            case PushAskAction(f, i, then) =>
                val around = layout.seasAround(i)
                around.%(s => fleets.contains(s)).headOption match {
                    case Some(s) =>
                        val st = fleets(s)
                        val l = layout.seasNext(s).%(x => around.has(x).not && krakenAt.has(x).not && fleets.get(x).forall(_.f == st.f))
                        Ask(f).each(l)(x => PushAction(f, s, |(x), i, then)).add(l.none.$(PushAction(f, s, None, i, then)))
                    case None =>
                        then
                }

            case PushAction(f, s, to, i, then) =>
                val st = fleets(s)
                fleets -= s
                to match {
                    case Some(x) =>
                        addFleets(st.f, x, st.n)
                        log(Polyphemus, "pushed the", st.f, "Fleets from", s, "to", x)
                    case None =>
                        log(Polyphemus, "destroyed the", st.f, "Fleets in", s)
                }
                PushAskAction(f, i, then)

            // END OF A CYCLE
            case CycleEndAction(n) =>
                current = null
                currentGod = null
                // the first to act offers last next cycle
                order = acted.reverse
                val w = setup.%(f => metropolisesOf(f) >= goal)
                if (w.any) {
                    val best = w./(_.gold).max
                    GameOverAction(w.%(_.gold == best))
                }
                else
                    CycleStartAction(n + 1)

            case GameOverAction(w) =>
                isOver = true
                w.foreach(f => log(f, "won with", metropolisesOf(f).hl, "Metropolises"))
                GameOver(w, "Game Over", w./(f => GameOverWonAction(null, f)))

            case a : SelfPerform =>
                a.perform(soft)(this)
        }
    }

    // the gods' turns left in this cycle
    var pending = $[Turn]()

    // after fleets moved or fought: the god's menu, or the Sylph's next move
    def afterFleets(f : Faction, m : FleetMove) : ForcedAction = m.then
}

case class ReplaceDrawAction(f : Faction, slot : Int) extends ForcedAction
case class DiscardCreatureAction(c : Creature, then : ForcedAction) extends ForcedAction
case class SylphMenuAction(f : Faction, then : ForcedAction) extends ForcedAction
case class SphinxMenuAction(f : Faction, then : ForcedAction) extends ForcedAction
case class ChimeraEndAction(c : Creature, then : ForcedAction) extends ForcedAction
case class KrakenMenuAction(f : Faction, then : ForcedAction) extends ForcedAction
