package pincer
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
import pincer.elem._

// PINCER: A GENTLEMAN'S WAR (Julian Birke, Birke Publishing), after the second draft of the rulebook (2026-08-28).
// RULES.md lists the rules as read and the choices made where the draft leaves them open.

trait Faction extends BasePlayer with Named with Styling with Record {
    def name = toString
    def short = name.take(1)
    def id = name.toLowerCase
}

case object Red extends Faction
case object Green extends Faction
case object Blue extends Faction
case object Yellow extends Faction

object Faction {
    val all = $[Faction](Red, Green, Blue, Yellow)
}


// CARDS: six resources, each its own deck with one Maneuver card in it

trait Card extends Record with Elementary {
    def name : String
    def resource : Resource
    def image : String
}

sealed abstract class Resource(val name : String) extends Card with Named with Styling {
    def resource = this
    def image = id
    def id = name.toLowerCase
    override def elem = name.styled(this)
}

case object Wheat extends Resource("Wheat")
case object Fish extends Resource("Fish")
case object Mustang extends Resource("Mustang")
case object Lumber extends Resource("Lumber")
case object Stone extends Resource("Stone")
case object Iron extends Resource("Iron")

object Resource {
    val all = $[Resource](Wheat, Fish, Mustang, Lumber, Stone, Iron)
}

sealed abstract class Maneuver(val name : String, val resource : Resource) extends Card {
    def image = name.toLowerCase
    def elem = name.styled(resource)(styles.maneuver)
}

case object Retreat extends Maneuver("Retreat", Wheat)
case object Broadside extends Maneuver("Broadside", Fish)
case object Fortify extends Maneuver("Fortify", Mustang)
case object Ambush extends Maneuver("Ambush", Lumber)
case object Outflank extends Maneuver("Outflank", Stone)
case object Hideout extends Maneuver("Hideout", Iron)

object Maneuver {
    val all = $[Maneuver](Retreat, Broadside, Fortify, Ambush, Outflank, Hideout)
    def of(r : Resource) = all.%(_.resource == r).only
}

object Deck {
    // 120 resource cards, 20 a deck: 19 resources and the Maneuver
    def of(r : Resource) : $[Card] = 19.times(r) :+ Maneuver.of(r)
}


// PIECES

sealed abstract class Kind(val name : String, val count : Int, val land : Boolean) extends Record with Elementary {
    def id = name.toLowerCase
    def elem = name.styled(styles.kind)
    def plural = name + "s"
}

case object Musketeer extends Kind("Musketeer", 7, true)
case object Cavalry extends Kind("Cavalry", 6, true)
case object Cannon extends Kind("Cannon", 5, true)
case object Frigate extends Kind("Frigate", 5, false)

object Kind {
    val all = $[Kind](Musketeer, Cavalry, Cannon, Frigate)
}

case class Figure(faction : Faction, kind : Kind, index : Int) extends Record with Elementary {
    def elem = (faction.name + " " + kind.name).styled(faction)
    def land = kind.land
}

object Pieces {
    val colonies = 4
    val ports = 3
    val forts = 3
    val roads = 8
    val bridges = 2
}

case class Fort(owner : Faction, damaged : Boolean) extends Record
case class Port(owner : Faction, colony : Hex) extends Record
case class Road(owner : Faction, a : Hex, b : Hex) extends Record
case class Bridge(owner : Faction, a : Hex, water : Hex, b : Hex) extends Record

// what a piece is built for
sealed abstract class Build(val name : String, val cost : $[Resource]) extends Record with Elementary {
    def elem = name.styled(styles.kind)
}

case object ColonyBuild extends Build("Colony", $(Lumber, Lumber, Stone, Stone, Iron, Wheat))
case object RoadBuild extends Build("Road", $(Stone))
case object BridgeBuild extends Build("Bridge", $(Stone, Lumber))
case object PortBuild extends Build("Port", $(Lumber, Lumber, Iron))
case object FortBuild extends Build("Fort", $(Stone, Stone, Stone, Lumber))
case object RepairBuild extends Build("Repair", $(Stone, Lumber))

object Costs {
    // each item of a cost is the resources that may pay it
    def recruit(k : Kind) : $[$[Resource]] = k match {
        case Musketeer => $($(Wheat, Fish), $(Wheat, Fish))
        case Cavalry => $($(Wheat), $(Wheat), $(Mustang))
        case Cannon => $($(Iron), $(Iron), $(Iron), $(Iron))
        case Frigate => $($(Iron), $(Iron), $(Lumber), $(Lumber))
    }

    def revive(k : Kind) : $[$[Resource]] = k match {
        case Musketeer => $($(Wheat, Fish))
        case Cavalry => $($(Mustang))
        case Cannon => $($(Iron), $(Iron))
        case Frigate => $($(Iron), $(Lumber))
    }

    def build(b : Build) : $[$[Resource]] = b.cost./($(_))

    // the different ways to pay with a hand, as sorted lists of resources
    def ways(hand : $[Card], cost : $[$[Resource]]) : $[$[Resource]] = {
        def go(left : $[Resource], rest : $[$[Resource]]) : $[$[Resource]] = rest match {
            case Nil => $($())
            case req :: more => req.distinct.%(left.has)./~(r => go(left :- r, more)./(r +: _))
        }
        go(hand.of[Resource], cost)./(_.sortBy(Resource.all.indexOf)).distinct
    }
}


// ROYAL SEALS: 14, eight to win

sealed abstract class Seal(val name : String, val image : String) extends Record with Elementary {
    def elem = name.styled(styles.seal)
}

sealed abstract class Society(name : String, image : String) extends Seal(name, image)
case object FoundersOath extends Society("Founder's Oath Society", "founders-oath")
case object MaritimeExchange extends Society("Maritime Exchange Guild", "maritime-exchange")
case object StonewallSociety extends Society("Stonewall Society", "stonewall-society")
case object CrossroadsGuild extends Society("Crossroads Guild", "crossroads-guild")

sealed abstract class Company(name : String, image : String, val resource : Resource) extends Seal(name, image)
case object GoldenGrain extends Company("Golden Grain", "golden-grain", Wheat)
case object NeptunesBounty extends Company("Neptune's Bounty", "neptunes-bounty", Fish)
case object ColonialEquine extends Company("Colonial Equine", "colonial-equine", Mustang)
case object NorthernAxe extends Company("Northern Axe", "northern-axe", Lumber)
case object CrownAndChisel extends Company("Crown & Chisel", "crown-and-chisel", Stone)
case object IronMark extends Company("Iron Mark", "iron-mark", Iron)

sealed abstract class Badge(name : String, image : String, val kind : Kind) extends Seal(name, image)
case object RoyalMusket extends Badge("Royal Musket Badge", "royal-musket", Musketeer)
case object MountedVanguard extends Badge("Mounted Vanguard", "mounted-vanguard", Cavalry)
case object BlackPowderVolley extends Badge("Black Powder Volley", "black-powder-volley", Cannon)
case object NavalSupremacy extends Badge("Naval Supremacy", "naval-supremacy", Frigate)

object Seal {
    val societies = $[Society](FoundersOath, MaritimeExchange, StonewallSociety, CrossroadsGuild)
    val companies = $[Company](GoldenGrain, NeptunesBounty, ColonialEquine, NorthernAxe, CrownAndChisel, IronMark)
    val badges = $[Badge](RoyalMusket, MountedVanguard, BlackPowderVolley, NavalSupremacy)
    val all = $[Seal]() ++ societies ++ companies ++ badges
    def company(r : Resource) = companies.%(_.resource == r).only
    def badge(k : Kind) = badges.%(_.kind == k).only
    val toWin = 8
}

case class War(declarer : Faction, target : Faction, boar : Boolean) extends Record {
    def between(a : Faction, b : Faction) = (declarer == a && target == b) || (declarer == b && target == a)
}


// VIEWS

trait ViewCard extends ViewObject[Card] { self : UserAction =>
    def card : Card
    def obj = card
}

case class ViewCardInfoAction(self : Faction, card : Card) extends BaseInfo(Break ~ "Hand")(Image("card:" + card.image, styles.card)) with ViewCard
case class DrewCardInfoAction(self : Faction, card : Card) extends BaseInfo(Break ~ "Drew")(Image("card:" + card.image, styles.card)) with ViewCard
case class ViewSealInfoAction(self : Faction, seal : Seal) extends BaseInfo(Break ~ "Royal Seals")(Image("seal:" + seal.image, styles.sealImg)) with ViewObject[Seal] { def obj = seal }

object Images {
    implicit class CardImg(val c : Card) {
        def img = Image("card:" + c.image, styles.card)
    }
    implicit class SealImg(val s : Seal) {
        def img = Image("seal:" + s.image, styles.sealImg)
    }
}

import Images._


// MAP KEYS: actions a tap on the map can take

trait HexKey { self : UserAction =>
    def hex : Hex
}

trait FigureKey { self : UserAction =>
    def figure : Figure
}


// ACTIONS

case class StartAction(version : String) extends StartGameAction with GameVersion
case class ShuffleDecksAction(left : $[Resource]) extends ForcedAction
case class ShuffledDeckAction(r : Resource, shuffled : $[Card], then : ForcedAction) extends ShuffledAction[Card]

case class SetupPlaceAction(n : Int) extends ForcedAction
case class PlaceStartAction(self : Faction, hex : Hex, n : Int) extends BaseAction(self, "places a starting", "Colony".hl, "and", Musketeer)(hex) with HexKey

case class TurnStartAction(f : Faction) extends ForcedAction
case class HarvestRolledAction(f : Faction, rolled : $[Resource]) extends RolledAction[Resource]
case class HarvestChooseAction(self : Faction, mine : Resource, other : Resource) extends BaseAction("Harvest: the others take the other die")("Take", mine)
case class FoundersChooseAction(self : Faction, f : Faction, pick : Resource, mine : Resource, other : Resource) extends BaseAction(FoundersOath, "lets you take either die")("Take", pick)
case class FoundersAskAction(f : Faction, mine : Resource, other : Resource) extends ForcedAction
case class HarvestAction(f : Faction, mine : Resource, other : Resource, choices : $[Draw]) extends ForcedAction
case class Draw(f : Faction, r : Resource) extends Record
case class DrawNextAction(draws : $[Draw], then : ForcedAction) extends ForcedAction
case class ReshuffledAction(r : Resource, shuffled : $[Card], then : ForcedAction) extends ShuffledAction[Card]

case class MainAction(f : Faction) extends ForcedAction
case class EndTurnAction(self : Faction) extends BaseAction(None)("End turn")
case class NextTurnAction(f : Faction) extends ForcedAction

// units
case class UnitMenuAction(self : Faction, figure : Figure) extends BaseAction("Units")(figure, "in", (g : Game) => g.at(figure).elem) with Soft with FigureKey
case class MoveAction(self : Faction, figure : Figure, hex : Hex, counter : |[Counter]) extends BaseAction(figure, "in", (g : Game) => g.at(figure).elem)("Move to", hex, (g : Game) => g.moveNote(figure, hex)) with HexKey
case class AttackAction(self : Faction, unit : Figure, target : Figure, counter : |[Counter]) extends BaseAction(unit, "in", (g : Game) => g.at(unit).elem)("Attack", target, "in", (g : Game) => g.at(target).elem) with FigureKey { def figure = target }
case class AttackFortAction(self : Faction, figure : Figure, hex : Hex, counter : |[Counter]) extends BaseAction(figure, "in", (g : Game) => g.at(figure).elem)("Attack", (g : Game) => g.forts(hex).owner.elem, "Fort in", hex) with HexKey
case class ReviveAction(self : Faction, figure : Figure, paid : $[Resource], then : ForcedAction) extends BaseAction(figure, "in", (g : Game) => g.at(figure).elem)("Revive", "for", paid.intersperse(",")) with FigureKey
case class HideoutAction(self : Faction, figure : Figure, hex : Hex) extends BaseAction(figure, "in", (g : Game) => g.at(figure).elem)("Play", Hideout, "and enter", hex) with HexKey
case class ExtraActionAction(self : Faction, figure : Figure, card : Maneuver) extends BaseAction(figure, "in", (g : Game) => g.at(figure).elem)("Play", card, "for one more move or attack")

case class ExtraNextAction(f : Faction, figure : Figure) extends ForcedAction
case class ResolveCountersAction(f : Faction) extends ForcedAction

// counter-actions
case class Counter(defender : Faction, active : Faction, ambush : Boolean) extends Record
case class CounterAskAction(d : Faction, f : Faction, ambushed : Boolean) extends ForcedAction
case class CounterUnitAction(self : Faction, figure : Figure, counter : Counter) extends BaseAction("Counter-Action against", counter.active)(figure, "in", (g : Game) => g.at(figure).elem) with Soft with FigureKey
case class AmbushMenuAction(self : Faction, f : Faction) extends BaseAction("Counter-Action against", f)("Play", Ambush, "(another move or attack with a unit in a forest)") with Soft
case class AmbushUnitAction(self : Faction, figure : Figure, counter : Counter) extends BaseAction(Ambush)(figure, "in", (g : Game) => g.at(figure).elem) with Soft with FigureKey
case class CounterPassAction(self : Faction, f : Faction) extends BaseAction("Counter-Action against", f)("Pass")
case class StonewallRepairAction(self : Faction, hex : Hex, paid : $[Resource], f : |[Faction]) extends BaseAction(StonewallSociety)("Repair Fort in", hex, "for", paid.intersperse(",")) with HexKey
case class FrigateReviveNowAction(self : Faction, figure : Figure, paid : $[Resource], then : ForcedAction) extends BaseAction(NavalSupremacy)("Revive", figure, "now for", paid.intersperse(",")) with FigureKey
case class FrigateReviveSkipAction(self : Faction, then : ForcedAction) extends BaseAction(NavalSupremacy)("Don't revive")

// defensive maneuvers
case class DefendAskAction(f : Faction, figure : Figure, target : Figure, counter : |[Counter]) extends ForcedAction
case class RetreatAction(self : Faction, target : Figure, hex : Hex, f : Faction, figure : Figure, counter : |[Counter]) extends BaseAction(figure, "attacks", target)("Play", Retreat, "and move to", hex) with HexKey
case class FortifyAction(self : Faction, target : Figure, f : Faction, figure : Figure, counter : |[Counter]) extends BaseAction(figure, "attacks", target)("Play", Fortify, "and deflect the attack")
case class TakeHitAction(self : Faction, target : Figure, f : Faction, figure : Figure, counter : |[Counter]) extends BaseAction(figure, "attacks", target)("Take the hit")
case class HitAction(f : Faction, figure : Figure, target : Figure, counter : |[Counter]) extends ForcedAction
case class AfterAttackAction(f : Faction, figure : Figure, defender : Faction, counter : |[Counter]) extends ForcedAction

// building
case class BuildMenuAction(self : Faction) extends BaseAction(None)("Build") with Soft
case class BuildColonyAction(self : Faction, hex : Hex, paid : $[Resource]) extends BaseAction("Build", ColonyBuild, "for", paid.intersperse(","))("Colony in", hex) with HexKey
case class BuildRoadAction(self : Faction, a : Hex, b : Hex, paid : $[Resource]) extends BaseAction("Build", RoadBuild, "for", paid.intersperse(","))("Road between", a, "and", b) with HexKey { def hex = b }
case class BuildBridgeAction(self : Faction, a : Hex, w : Hex, b : Hex, paid : $[Resource]) extends BaseAction("Build", BridgeBuild, "for", paid.intersperse(","))("Bridge from", a, "over", w, "to", b) with HexKey { def hex = w }
case class BuildPortAction(self : Faction, hex : Hex, colony : Hex, paid : $[Resource]) extends BaseAction("Build", PortBuild, "for", paid.intersperse(","))("Port in", hex, "for the Colony in", colony) with HexKey
case class BuildFortAction(self : Faction, hex : Hex, paid : $[Resource]) extends BaseAction("Build", FortBuild, "for", paid.intersperse(","))("Fort in", hex) with HexKey
case class RepairFortAction(self : Faction, hex : Hex, paid : $[Resource]) extends BaseAction("Repair", "for", paid.intersperse(","))("Fort in", hex) with HexKey
case class RemoveFortAction(self : Faction, hex : Hex) extends BaseAction("Annexed Colonies")("Remove", (g : Game) => g.forts(hex).owner.elem, "damaged Fort in", hex) with HexKey

// recruiting
case class RecruitMenuAction(self : Faction) extends BaseAction(None)("Recruit") with Soft
case class RecruitAction(self : Faction, kind : Kind, hex : Hex, paid : $[Resource]) extends BaseAction("Recruit", kind, "for", paid.intersperse(","))("in", hex) with HexKey

// trade
case class TradeMenuAction(self : Faction) extends BaseAction(None)("Trade", (g : Game) => ("(" + g.rate(self) + ":1)").hl) with Soft
case class TradeForAction(self : Faction, r : Resource) extends BaseAction("Trade", (g : Game) => g.rate(self).hl, "cards for")(r, (g : Game) => g.tradeSource(self, r)./(e => " from " ~ Seal.company(r).elem ~ " (" ~ e.elem ~ ")").|(Empty)) with Soft
case class TradePayAction(self : Faction, r : Resource, paid : $[Resource]) extends BaseAction("Trade for", r, "paying", paid.intersperse(","))("Add", (paid.lastOption : |[Card])./(_.elem).|(Empty)) with Soft
case class TradeAction(self : Faction, r : Resource, paid : $[Resource]) extends BaseAction("Trade for", r)("Pay", paid.intersperse(","))

// trading companies
case class CompanyMenuAction(self : Faction) extends BaseAction(None)("Trading Companies") with Soft
case class FoundCompanyAction(self : Faction, company : Company) extends BaseAction("Trading Companies")("Found", company, "with", 5.hl, company.resource)
case class BuyoutAction(self : Faction, company : Company, n : Int) extends BaseAction("Trading Companies")("Forcible buyout of", company, "with", n.hl, company.resource)

// diplomacy
case class DiplomacyMenuAction(self : Faction) extends BaseAction(None)("Diplomacy") with Soft
case class DeclareWarAction(self : Faction, e : Faction, boar : Boolean) extends BaseAction("Declare War on", e)(boar.?("Boar's War".hl ~ " (attack at once)").|("Gentleman's War".hl ~ " (attack from your next turn)"))
case class OfferPeaceAction(self : Faction, e : Faction) extends BaseAction("Diplomacy")("Offer peace to", e)
case class PeaceAskAction(e : Faction, f : Faction) extends ForcedAction
case class AcceptPeaceAction(self : Faction, f : Faction) extends BaseAction(f, "offers peace")("Accept")
case class DeclinePeaceAction(self : Faction, f : Faction) extends BaseAction(f, "offers peace")("Decline")
case class GiveMenuAction(self : Faction, e : Faction) extends BaseAction("Diplomacy")("Give cards to", e) with Soft
case class GiveCardAction(self : Faction, e : Faction, card : Card) extends BaseAction("Give", e)(card)
case class ReturnPrisonerAction(self : Faction, figure : Figure) extends BaseAction("Return captured units")(figure)
case class PlacePrisonerAction(self : Faction, figure : Figure, hex : Hex, f : Faction) extends BaseAction(figure, "returned")("Place in", hex) with HexKey
case class SurrenderAction(self : Faction) extends BaseAction("Every Colony annexed")("Surrender".hl)

case class GameOverAction(winners : $[Faction]) extends ForcedAction
case class GameOverWonAction(self : Faction, f : Faction) extends BaseInfo("Game Over")(f, "won")


class Player(val faction : Faction) {
    var hand : $[Card] = $
    var warCards = 3
    var coloniesFounded = 0
    var portsBuilt = 0
    var fortsBuilt = 0
    var captured : $[(Kind, Figure)] = $
    var fortDestroyer : |[Faction] = None
}

class Game(val setup : $[Faction], val options : $[Meta.O]) extends BaseGame with ContinueGame with LoggedGame { game =>
    var isOver = false

    var factions : $[Faction] = setup

    val states = setup./(f => f -> new Player(f)).toMap

    implicit def faction2player(f : Faction) : Player = states(f)

    // map
    var pos = Map[Figure, Hex]()
    var injured = Map[Figure, Int]()
    var embarked = Map[Figure, Figure]()
    var colonies = Map[Hex, Faction]()
    var annexedAt = Map[Hex, (Faction, Int)]()
    var forts = Map[Hex, Fort]()
    var ports = Map[Hex, Port]()
    var roads = $[Road]()
    var bridges = $[Bridge]()

    // cards
    var decks = Resource.all./(r => r -> $[Card]()).toMap
    var discards = Resource.all./(r => r -> $[Card]()).toMap
    var stockpiles = Resource.all./(r => r -> $[Card]()).toMap

    var seals = Map[Seal, Faction]()
    var wars = $[War]()
    var gentleman = $[(Faction, Faction)]()

    // turn
    var turn = 0
    var current : Faction = null
    var rolled = $[Resource]()
    var acted = Set[Figure]()
    var fresh = Set[Figure]()
    var movedOne = Set[Figure]()
    var attacks = Map[Figure, Int]()
    var extra = Map[Figure, Maneuver]()
    var counterUsed = Set[Figure]()
    var pendingCounters = $[Faction]()
    var peaceOffered = $[Faction]()
    var trades = 0

    var highlightFaction = $[Faction]()

    // where a figure is: its own hex, or for one embarked, its frigate's
    def at(u : Figure) : Hex = pos(u)
    def on(h : Hex) : $[Figure] = pos.toList.filter(_._2 == h).map(_._1).sortBy(u => (setup.indexOf(u.faction), Kind.all.indexOf(u.kind), u.index))
    def units(f : Faction) : $[Figure] = pos.keys.$.%(_.faction == f).sortBy(u => (Kind.all.indexOf(u.kind), u.index))
    def isInjured(u : Figure) = injured.contains(u)

    def supply(f : Faction, k : Kind) : $[Figure] = 1.to(k.count).$./(Figure(f, k, _)).%(u => pos.contains(u).not && factions./~(_.captured.map(_._2)).has(u).not)

    def colonyOf(f : Faction) : $[Hex] = colonies.toList.filter(_._2 == f).map(_._1).sortBy(x => (x.r, x.c))

    // the player whose land unit stands in another player's colony
    def occupier(h : Hex) : |[Faction] = colonies.get(h)./~(o => on(h).%(u => u.land && embarked.contains(u).not && u.faction != o)./(_.faction).distinct.single)

    def annexed(h : Hex) = occupier(h).any

    // who collects from a colony: its owner, or the player annexing it
    def controller(h : Hex) : Faction = occupier(h).|(colonies(h))

    def ownColonies(f : Faction) = colonyOf(f).%(annexed(_).not)

    def blockaded(h : Hex) : |[Faction] = ports.get(h)./~(p => on(h).%(u => u.kind == Frigate && u.faction != p.owner)./(_.faction).distinct.single)

    def isAtWar(a : Faction, b : Faction) = wars.exists(_.between(a, b))

    def canAttack(f : Faction, e : Faction) = f != e && isAtWar(f, e) && gentleman.has((f, e)).not

    def hasSeal(f : Faction, s : Seal) = seals.get(s).has(f)

    def sealsOf(f : Faction) = Seal.all.%(s => seals.get(s).has(f))

    def ruleFigures = factions

    // ROADS

    def links(f : Faction) : $[(Hex, Hex)] = roads.%(_.owner == f)./(r => (r.a, r.b)) ++ bridges.%(_.owner == f)./(b => (b.a, b.b))

    def networkOf(f : Faction, start : Hex) : Set[Hex] = {
        val l = links(f)
        var seen = Set(start)
        var front = $(start)
        while (front.any) {
            front = front./~(x => l./~ { case (a, b) => if (a == x) $(b) else if (b == x) $(a) else $() }).distinct.%(x => seen.contains(x).not)
            seen ++= front
        }
        seen
    }

    // each pair of the player's own (not annexed) colonies joined by its roads and bridges is a road network
    def networks(f : Faction) : Int = {
        val cc = ownColonies(f)
        cc.combinations(2).$.count { case $(a, b) => networkOf(f, a).contains(b) ; case _ => false }
    }

    def inNetwork(f : Faction, h : Hex) = ownColonies(f).but(h).exists(c => networkOf(f, h).contains(c))

    def portBonus(f : Faction) = ports.toList.count { case (h, p) => p.owner == f && annexed(p.colony).not && (blockaded(h).none || hasSeal(f, MaritimeExchange)) }

    def rate(f : Faction) = max(1, 5 - networks(f) - portBonus(f))

    // TRADE

    def tradeSource(f : Faction, r : Resource) : |[Faction] = seals.get(Seal.company(r)).%(_ != f).%(_ => stockpiles(r).any)

    def bankHas(r : Resource) = decks(r).has(r) || discards(r).has(r)

    // MOVEMENT

    def landPassable(f : Faction, h : Hex) = Board.terrain(h) match {
        case Water => false
        case Mountain => colonies.get(h).has(f)
        case _ => true
    }

    // other players' figures in a hex: units may not move through them, and enter them only to capture or annex
    def others(f : Faction, h : Hex) = on(h).%(_.faction != f)

    def enterable(f : Faction, u : Figure, h : Hex) : Boolean = {
        val o = others(f, h)
        val fort = forts.get(h).%(_.owner != f)
        val colony = colonies.get(h).%(_ != f)
        if (u.land) {
            if (fort.exists(_.damaged.not))
                false
            else
            if (o.any)
                o.forall(v => v.land && isInjured(v) && embarked.contains(v).not && canAttack(f, v.faction))
            else
            if (colony.any)
                canAttack(f, colony.get)
            else
                true
        }
        else
            o.forall(v => v.kind == Frigate && isInjured(v) && canAttack(f, v.faction)) && o.%(_.kind == Frigate).any.?(true).|(o.none)
    }

    // where a figure can move this action, with how far
    def moves(u : Figure) : $[Hex] = {
        val f = u.faction
        val start = at(u)
        val range = u.kind match {
            case Cavalry if hasSeal(f, MountedVanguard) => 3
            case Cavalry => 2
            case Frigate => 2
            case _ => 1
        }

        if (u.kind == Frigate) {
            var seen = Set(start)
            var front = $(start)
            var result = $[Hex]()
            1.to(range).foreach { _ =>
                front = front./~(Board.neighbors).distinct.%(x => seen.contains(x).not && Board.terrain(x) == Water)
                seen ++= front
                val ok = front.%(x => enterable(f, u, x))
                result ++= ok
                front = ok.%(x => others(f, x).none)
            }
            result.distinct
        }
        else {
            def step(x : Hex) : $[Hex] = {
                val n = Board.neighbors(x) ++ bridges./~(b => (b.a == x).?(b.b) ++ (b.b == x).?(b.a)) ++
                    (colonies.get(x).has(f) && annexed(x).not).??(ownColonies(f).but(x).%(c => networkOf(f, x).contains(c)))
                n.distinct
            }

            var seen = Set(start)
            var front = $(start)
            var result = $[Hex]()
            1.to(range).foreach { _ =>
                val next = front./~(step).distinct.%(x => seen.contains(x).not)
                seen ++= next
                // embarking: onto a water hex with the player's frigate that carries nobody
                val boats = next.%(x => Board.terrain(x) == Water && on(x).exists(v => v.faction == f && v.kind == Frigate && embarked.values.$.has(v).not && isInjured(v).not))
                val land = next.%(x => landPassable(f, x) && enterable(f, u, x))
                result ++= land ++ boats
                // a capture, an annexation or an embarkation ends the move
                front = land.%(x => others(f, x).none && colonies.get(x).%(_ != f).none)
            }
            result.distinct.but(start)
        }
    }

    def moveNote(u : Figure, h : Hex) : Elem = {
        val f = u.faction
        val o = others(f, h)
        if (o.any && u.land && Board.terrain(h) != Water)
            " (capture " ~ o./(_.elem).comma ~ ")"
        else
        if (o.any && u.kind == Frigate)
            " (capture " ~ o./(_.elem).comma ~ ")"
        else
        if (u.land && Board.terrain(h) == Water)
            " (embark)".txt
        else
        if (colonies.get(h).%(_ != f).any)
            " (annex)".txt
        else
            Empty
    }

    // ATTACKS

    def attackRange(u : Figure) = u.kind match {
        case Cannon | Frigate => 2
        case _ => 1
    }

    // a unit in a colony is safe from attack behind the colony owner's undamaged fort, and behind a damaged one from all but cannons and frigates
    def shielded(u : Figure, v : Figure) = {
        val h = at(v)
        forts.get(h).%(_.owner == v.faction) match {
            case Some(Fort(_, false)) => true
            case Some(Fort(_, true)) => u.kind != Cannon && u.kind != Frigate
            case None => false
        }
    }

    def targets(u : Figure) : $[Figure] = {
        val f = u.faction
        val hexes = Board.within(at(u), attackRange(u), false)
        hexes./~(on).%(v => v.faction != f && canAttack(f, v.faction) && isInjured(v).not && embarked.contains(v).not).%{ v =>
            u.kind match {
                case Musketeer | Cavalry | Cannon => v.land || u.kind == Cannon
                case Frigate => true
            }
        }.%(v => shielded(u, v).not)
    }

    def fortTargets(u : Figure) : $[Hex] = (u.kind == Cannon || u.kind == Frigate).??(Board.within(at(u), attackRange(u), false).%(h => forts.get(h).exists(t => t.damaged.not && canAttack(u.faction, t.owner))))

    def mayAct(u : Figure) = acted.contains(u).not && fresh.contains(u).not

    def mayAttack(u : Figure) = {
        val f = u.faction
        fresh.contains(u).not && embarked.contains(u).not && (isInjured(u).not || (u.kind == Musketeer && hasSeal(f, RoyalMusket))) &&
        (acted.contains(u).not || movedOne.contains(u) || (u.kind == Cannon && hasSeal(f, BlackPowderVolley) && attacks.get(u).has(1)))
    }

    def mayMove(u : Figure) = mayAct(u)

    def revivable(u : Figure) = isInjured(u) && fresh.contains(u).not && acted.contains(u).not && (injured(u) < turn || (u.kind == Frigate && hasSeal(u.faction, NavalSupremacy)))

    def reviveCost(u : Figure) : $[$[Resource]] = hasSeal(u.faction, CrossroadsGuild).?(Costs.revive(u.kind)./(_ => Resource.all)).|(Costs.revive(u.kind))

    def unitActions(f : Faction, u : Figure, counter : |[Counter]) : $[UserAction] = {
        val m = (counter.any || mayMove(u)).??(moves(u)./(h => MoveAction(f, u, h, counter)))
        val a = (counter.any.?(embarked.contains(u).not && (isInjured(u).not || (u.kind == Musketeer && hasSeal(f, RoyalMusket)))).|(mayAttack(u))).??(targets(u)./(v => AttackAction(f, u, v, counter)) ++ fortTargets(u)./(h => AttackFortAction(f, u, h, counter)))
        val r = counter.none.??(revivable(u).??(Costs.ways(f.hand, reviveCost(u))./(p => ReviveAction(f, u, p, MainAction(f)))))
        val e = (counter.none && mayAct(u) && extra.contains(u).not).??(
            (f.hand.has(Outflank) && u.land && Board.terrain(at(u)) == Rocks).$(ExtraActionAction(f, u, Outflank)) ++
            (f.hand.has(Broadside) && u.kind == Frigate).$(ExtraActionAction(f, u, Broadside))
        )
        val hide = (counter.none && mayMove(u) && f.hand.has(Hideout) && u.land && embarked.contains(u).not).??(Board.neighbors(at(u)).%(h => Board.terrain(h) == Mountain && others(f, h).none && colonies.get(h).forall(c => c == f || canAttack(f, c)))./(h => HideoutAction(f, u, h)))
        m ++ a ++ r ++ e ++ hide
    }

    // BUILDING

    def colonySites(f : Faction) : $[Hex] = (colonyOf(f).num < Pieces.colonies).??(units(f).%(_.land).%(u => embarked.contains(u).not)./(at).distinct.%{ h =>
        colonies.contains(h).not && Board.edge(h).not && Board.terrain(h).land && others(f, h).none &&
        colonies.keys.forall(c => Board.landDistance(c, h, 2).none)
    })

    def startSites : $[Hex] = Board.hexes.%{ h =>
        Board.terrain(h).land && Board.terrain(h) != Mountain && Board.edge(h).not && on(h).none &&
        colonies.keys.forall(c => Board.landDistance(c, h, 2).none)
    }

    // a road joins two neighboring land hexes; it starts at the player's colony or at the end of one of its roads or bridges
    def roadSites(f : Faction) : $[(Hex, Hex)] = (roads.count(_.owner == f) < Pieces.roads).??{
        val ends = ownColonies(f) ++ links(f).flatMap { case (a, b) => $(a, b) }
        ends.distinct./~(a => Board.neighbors(a).%(b => Board.terrain(a).land && Board.terrain(b).land)./(b => (a, b)))
            ./{ case (a, b) => if (Board.hexes.indexOf(a) < Board.hexes.indexOf(b)) (a, b) else (b, a) }.distinct
            .%{ case (a, b) => roads.exists(r => r.a == a && r.b == b).not }
    }

    def bridgeSites(f : Faction) : $[(Hex, Hex, Hex)] = (bridges.count(_.owner == f) < Pieces.bridges).??{
        val ends = ownColonies(f) ++ links(f).flatMap { case (a, b) => $(a, b) }
        ends.distinct./~(a => Board.neighbors(a).%(w => Board.terrain(w) == Water)./~(w => Board.across(a, w).%(b => Board.terrain(b).land)./(b => (a, w, b))))
            .%{ case (a, w, b) => bridges.exists(_.water == w).not }
    }

    def portSites(f : Faction) : $[(Hex, Hex)] = (ports.values.count(_.owner == f) < Pieces.ports).??(ownColonies(f).%(c => ports.values.exists(_.colony == c).not)./~(c => Board.neighbors(c).%(h => Board.terrain(h) == Water && ports.contains(h).not)./(h => (h, c))))

    def fortSites(f : Faction) : $[Hex] = (forts.values.count(_.owner == f) < Pieces.forts).??(
        (ownColonies(f) ++ colonies.keys.$.%(h => occupier(h).has(f) && annexedAt.get(h).exists { case (e, t) => e == f && t < turn })).%(h => forts.contains(h).not)
    )

    def repairSites(f : Faction) : $[Hex] = forts.toList.filter(_._2.owner == f).filter(_._2.damaged).map(_._1)

    def removableForts(f : Faction) : $[Hex] = forts.toList.%{ case (h, t) => t.owner != f && t.damaged && occupier(h).has(f) && annexedAt.get(h).exists { case (e, n) => e == f && n < turn } }.map(_._1)

    // RECRUITING

    def recruitSites(f : Faction, k : Kind) : $[Hex] = k match {
        case Frigate => ports.toList.filter(_._2.owner == f).%{ case (h, p) => annexed(p.colony).not && blockaded(h).none && on(h).none }.map(_._1)
        case _ => ownColonies(f).%(h => on(h).none).%{ h =>
            k match {
                case Musketeer => true
                case Cavalry => inNetwork(f, h)
                case Cannon => forts.get(h).exists(_.owner == f)
                case _ => false
            }
        }
    }

    // COMPANIES

    // a player qualifies for a company with a colony it owns or annexes on that terrain (a port, for fish)
    def companyBases(f : Faction, r : Resource) : $[Hex] = r match {
        case Fish => ports.toList.filter(_._2.owner == f).map(_._1)
        case _ => colonies.keys.$.%(h => Board.terrain(h).resource == r && controller(h) == f)
    }

    // INFO

    def preinfo(waiting : $[Faction], self : |[Faction], actions : $[UserAction]) : $[Info] = $

    def info(waiting : $[Faction], self : |[Faction], actions : $[UserAction]) : $[Info] =
        self.%(states.contains)./~(f => f.hand./(ViewCardInfoAction(f, _)))

    implicit def descCard(g : Game, d : Card) = d.img

    // HAND

    def pay(f : Faction, paid : $[Resource]) {
        paid.foreach { r =>
            f.hand = f.hand :- r
            discards += r -> (discards(r) :+ r)
        }
    }

    def discard(f : Faction, c : Card) {
        f.hand = f.hand :- c
        discards += c.resource -> (discards(c.resource) :+ c)
    }

    def takeFromBank(r : Resource) : Boolean = {
        if (decks(r).has(r)) {
            decks += r -> (decks(r) :- r)
            true
        }
        else
        if (discards(r).has(r)) {
            discards += r -> (discards(r) :- r)
            true
        }
        else
            false
    }

    // PIECES

    def capture(f : Faction, by : Kind, v : Figure) {
        val passengers = embarked.toList.filter(_._2 == v).map(_._1)
        (v +: passengers).foreach { x =>
            pos -= x
            injured -= x
            embarked -= x
            f.captured :+= (by, x)
            log(f, "captured", x, "with", by)
        }
    }

    def moveFigure(u : Figure, h : Hex) {
        pos += u -> h
        embarked.toList.filter(_._2 == u).foreach { case (p, _) => pos += p -> h }
    }

    // SEALS

    def claim(s : Seal, f : Faction) {
        if (seals.get(s).has(f).not) {
            seals.get(s) match {
                case Some(e) => log(f, "took", s, "from", e)
                case None => log(f, "claimed", s)
            }
            seals += s -> f
        }
    }

    def checkSeals() {
        // annexations: when they started
        colonies.keys.foreach { h =>
            occupier(h) match {
                case Some(e) => if (annexedAt.get(h).map(_._1).has(e).not) { annexedAt += h -> (e, turn) ; log(e, "annexed", colonies(h), "Colony in", h) }
                case None => if (annexedAt.contains(h)) { annexedAt -= h ; log("Colony in", h, "is no longer annexed") }
            }
        }

        // secret societies: the first to build
        factions.foreach { f =>
            if (seals.contains(FoundersOath).not && f.coloniesFounded >= 2) claim(FoundersOath, f)
            if (seals.contains(MaritimeExchange).not && f.portsBuilt >= 2) claim(MaritimeExchange, f)
            if (seals.contains(StonewallSociety).not && f.fortsBuilt >= 2) claim(StonewallSociety, f)
            if (seals.contains(CrossroadsGuild).not && networks(f) >= 2) claim(CrossroadsGuild, f)
        }

        // and stolen
        seals.get(FoundersOath).foreach { h =>
            val cc = colonyOf(h)
            if (cc.any)
                cc./(occupier).distinct.single.foreach(_.foreach(e => claim(FoundersOath, e)))
        }

        seals.get(MaritimeExchange).foreach { h =>
            val pp = ports.toList.filter(_._2.owner == h).map(_._1)
            if (pp.any)
                pp./(blockaded).distinct.single.foreach(_.foreach(e => claim(MaritimeExchange, e)))
        }

        seals.get(StonewallSociety).foreach { h =>
            if (forts.values.exists(_.owner == h).not)
                h.fortDestroyer.foreach(e => claim(StonewallSociety, e))
        }

        seals.get(CrossroadsGuild).foreach { h =>
            if (networks(h) == 0 && links(h).any)
                colonyOf(h)./~(occupier).distinct.single.foreach(e => claim(CrossroadsGuild, e))
        }

        // military achievements: the most captures by each kind of unit; ties keep the holder
        Seal.badges.foreach { b =>
            val counts = factions./(f => f -> f.captured.count(_._1 == b.kind))
            val best = counts.map(_._2).maxOr(0)
            if (best > 0) {
                val holder = seals.get(b)
                val held = holder./~(h => counts.find(_._1 == h).map(_._2)).|(0)
                if (best > held)
                    claim(b, counts.filter(_._2 == best).head._1)
            }
        }
    }

    // a seal held by a player who surrendered goes back
    def winners = factions.%(f => sealsOf(f).num >= Seal.toWin)

    def loggedPerform(action : Action, soft : Void) : Continue = {
        val c = performInternal(action, soft)

        if (action.isSoft.not && isOver.not && current != null) {
            checkSeals()

            val w = winners
            if (w.any)
                return GameOverAction(w)
        }

        highlightFaction = c match {
            case Ask(f, _) => $(f)
            case MultiAsk(a, _) => a./(_.faction)
            case _ => $
        }

        c
    }

    def next(f : Faction) = factions((factions.indexOf(f) + 1) % factions.num)

    def drawsFor(f : Faction, mine : Resource, other : Resource, choices : $[Draw]) : $[Draw] = {
        val order = factions.dropWhile(_ != f) ++ factions.takeWhile(_ != f)
        order./~{ e =>
            val r = (e == f).?(mine).|(choices.find(_.f == e)./(_.r).|(other))
            colonies.keys.$.sortBy(h => (h.r, h.c)).%(h => controller(h) == e).%{ h =>
                Board.neighbors(h).exists(x => Board.terrain(x).resource == r && on(x).forall(_.faction == e))
            }./(_ => Draw(e, r))
        }
    }

    def performInternal(a : Action, soft : Void) : Continue = {
        implicit val action = a

        action match {
            // SETUP
            case StartAction(version) =>
                log("HRF".hl, "version", gaming.version.hlb)
                log("Pincer: A Gentleman's War".hlb, "(rulebook second draft)")

                if (version != gaming.version)
                    log("Saved game version", version.hlb)

                log("Players", setup./(_.elem).comma)

                // the starting hands are one card of each resource, taken out before the decks are shuffled
                setup.foreach { f => f.hand = Resource.all }

                Resource.all.foreach { r => decks += r -> (Deck.of(r) ++ $()).diff(setup.num.times(r)) }

                ShuffleDecksAction(Resource.all)

            case ShuffleDecksAction(Nil) =>
                SetupPlaceAction(0)

            case ShuffleDecksAction(r :: rest) =>
                Shuffle[Card](decks(r), ShuffledDeckAction(r, _, ShuffleDecksAction(rest)))

            case ShuffledDeckAction(r, l, then) =>
                decks += r -> l
                log("Shuffled", r, "deck")
                then

            case SetupPlaceAction(n) =>
                if (n >= setup.num) {
                    val first = setup.last
                    log(first, "placed last and takes the first turn")
                    Milestone(TurnStartAction(first))
                }
                else {
                    val f = setup(n)
                    Ask(f).each(startSites)(h => PlaceStartAction(f, h, n))
                }

            case PlaceStartAction(f, h, n) =>
                colonies += h -> f
                val u = supply(f, Musketeer).head
                pos += u -> h
                log(f, "placed a Colony and", Musketeer, "in", h)
                SetupPlaceAction(n + 1)

            // TURN
            case TurnStartAction(f) =>
                turn += 1
                current = f
                acted = Set()
                fresh = Set()
                movedOne = Set()
                attacks = Map()
                extra = Map()
                counterUsed = Set()
                pendingCounters = $
                peaceOffered = $
                trades = 0
                gentleman = gentleman.filter(_._1 != f)

                log(DoubleLine)
                log("Turn", ("#" + turn).hl, f)

                // trading companies change hands at the start of the turn of a player who annexes or blockades all their owner's bases
                Seal.companies.foreach { c =>
                    seals.get(c).%(_ != f).foreach { e =>
                        val bases = companyBases(e, c.resource) ++ colonies.keys.$.%(h => colonies(h) == e && Board.terrain(h).resource == c.resource && controller(h) != e)
                        val ok = bases.any && bases.forall(h => occupier(h).has(f) || blockaded(h).has(f))
                        if (ok)
                            claim(c, f)
                    }
                }

                Roll[Resource]($(CustomDie(Resource.all), CustomDie(Resource.all)), HarvestRolledAction(f, _))

            case HarvestRolledAction(f, l) =>
                rolled = l
                log(f, "rolled", l(0), "and", l(1))

                if (l(0) == l(1))
                    FoundersAskAction(f, l(0), l(1))
                else
                    Ask(f).add(HarvestChooseAction(f, l(0), l(1))).add(HarvestChooseAction(f, l(1), l(0)))

            case HarvestChooseAction(f, mine, other) =>
                log(f, "took", mine, "and left", other, "to the others")
                FoundersAskAction(f, mine, other)

            case FoundersAskAction(f, mine, other) =>
                seals.get(FoundersOath).%(_ != f).%(factions.has).%(_ => mine != other) match {
                    case Some(h) => Ask(h).add(FoundersChooseAction(h, f, other, mine, other)).add(FoundersChooseAction(h, f, mine, mine, other))
                    case None => HarvestAction(f, mine, other, $)
                }

            case FoundersChooseAction(h, f, r, mine, other) =>
                log(h, "took", r, "with", FoundersOath)
                HarvestAction(f, mine, other, $(Draw(h, r)))

            case HarvestAction(f, mine, other, choices) =>
                val draws = drawsFor(f, mine, other, choices)
                if (draws.none)
                    log("No one collected")
                DrawNextAction(draws, MainAction(f))

            case DrawNextAction(Nil, then) =>
                then

            case DrawNextAction(Draw(f, r) :: rest, then) =>
                if (decks(r).none) {
                    if (discards(r).none) {
                        log("No", r, "cards left for", f)
                        DrawNextAction(rest, then)
                    }
                    else
                        Shuffle[Card](discards(r), ReshuffledAction(r, _, DrawNextAction(Draw(f, r) :: rest, then)))
                }
                else {
                    val c = decks(r).head
                    decks += r -> decks(r).drop(1)
                    f.hand :+= c
                    log(f, "collected a card from the", r, "deck")
                    if (c.is[Maneuver])
                        this.notify(f, $(DrewCardInfoAction(f, c)))
                    DrawNextAction(rest, then)
                }

            case ReshuffledAction(r, l, then) =>
                discards += r -> $
                decks += r -> l
                log("Reshuffled the", r, "discards")
                then

            case MainAction(f) if factions.has(f).not =>
                NextTurnAction(f)

            case MainAction(f) =>
                val ask = Ask(f)
                    .group("Units")
                    .each(units(f).%(u => unitActions(f, u, None).any))(u => UnitMenuAction(f, u))
                    .group(" ")
                    .add(BuildMenuAction(f).!!!(this))
                    .add(RecruitMenuAction(f).!!!(this))
                    .add(TradeMenuAction(f).!!!(this))
                    .add(CompanyMenuAction(f).!!!(this))
                    .add(DiplomacyMenuAction(f).!!!(this))
                    .add(EndTurnAction(f))

                if (colonyOf(f).any && colonyOf(f).forall(annexed))
                    ask.add(SurrenderAction(f))
                else
                    ask

            case EndTurnAction(f) =>
                log(f, "ended the turn")
                NextTurnAction(f)

            case NextTurnAction(f) =>
                if (factions.has(f))
                    TurnStartAction(next(f))
                else
                    TurnStartAction(factions.find(e => setup.indexOf(e) > setup.indexOf(f)).|(factions.head))

            // UNITS
            case UnitMenuAction(f, u) =>
                Ask(f).add(unitActions(f, u, None)).cancel

            case MoveAction(f, u, h, counter) =>
                ambushPlayed(f, counter)
                val from = at(u)
                val wasEmbarked = embarked.contains(u)
                val path = Board.distance(from, h)
                val captives = others(f, h).%(v => (u.land && v.land) || (u.kind == Frigate && v.kind == Frigate))

                if (u.land && Board.terrain(h) == Water) {
                    val boat = on(h).%(v => v.faction == f && v.kind == Frigate).head
                    embarked += u -> boat
                    log(u, "embarked on", boat, "in", h)
                }
                else {
                    if (wasEmbarked) {
                        embarked -= u
                        log(u, "disembarked to", h)
                    }
                    else
                        log(u, "moved from", from, "to", h)
                }

                moveFigure(u, h)

                captives.foreach(v => capture(f, u.kind, v))

                counter match {
                    case Some(c) =>
                        counterUsed += u
                        log(f, "used a Counter-Action", c.ambush.??(" with Ambush"))
                        if (c.ambush)
                            CounterAskAction(f, c.active, true)
                        else
                            ResolveCountersAction(c.active)
                    case None =>
                        if (u.kind == Cavalry && path == 1 && movedOne.contains(u).not && acted.contains(u).not && embarked.contains(u).not)
                            movedOne += u
                        else
                            movedOne -= u

                        acted += u

                        pendingCounters ++= captives./(_.faction).distinct
                        afterOwnAction(f, u)
                }

            case HideoutAction(f, u, h) =>
                discard(f, Hideout)
                log(f, "played", Hideout)
                log(u, "moved from", at(u), "to", h)
                moveFigure(u, h)
                acted += u
                afterOwnAction(f, u)

            case ExtraActionAction(f, u, card) =>
                discard(f, card)
                extra += u -> card
                log(f, "played", card, "for", u)
                Ask(f).add(unitActions(f, u, None))

            case ExtraNextAction(f, u) =>
                extra -= u
                acted -= u
                movedOne -= u
                val l = unitActions(f, u, None).%(a => a.is[MoveAction] || a.is[AttackAction] || a.is[AttackFortAction])
                Ask(f).add(l).skip(ResolveCountersAction(f))

            case AttackAction(f, u, v, counter) =>
                ambushPlayed(f, counter)
                log(u, "attacked", v, "in", at(v))

                counter match {
                    case Some(_) => counterUsed += u
                    case None =>
                        acted += u
                        movedOne -= u
                        attacks += u -> (attacks.getOrElse(u, 0) + 1)
                }

                DefendAskAction(f, u, v, counter)

            case DefendAskAction(f, u, v, counter) =>
                val e = v.faction
                val t = Board.terrain(at(v))
                val retreat = (t == Field && e.hand.has(Retreat)).??(moves(v).%(h => on(h).none && Board.terrain(h) != Water && colonies.get(h).%(_ != e).none))
                val fortify = t == Hills && e.hand.has(Fortify)

                if (retreat.any || fortify)
                    Ask(e)
                        .each(retreat)(h => RetreatAction(e, v, h, f, u, counter))
                        .when(fortify)(FortifyAction(e, v, f, u, counter))
                        .add(TakeHitAction(e, v, f, u, counter))
                else
                    HitAction(f, u, v, counter)

            case RetreatAction(e, v, h, f, u, counter) =>
                discard(e, Retreat)
                log(e, "played", Retreat)
                log(v, "retreated to", h)
                moveFigure(v, h)
                counterUsed += v
                AfterAttackAction(f, u, e, counter)

            case FortifyAction(e, v, f, u, counter) =>
                discard(e, Fortify)
                log(e, "played", Fortify, "and deflected the attack")
                counterUsed += v
                AfterAttackAction(f, u, e, counter)

            case TakeHitAction(e, v, f, u, counter) =>
                HitAction(f, u, v, counter)

            case HitAction(f, u, v, counter) =>
                injured += v -> turn
                log(v, "was injured")

                if (v.kind == Frigate && hasSeal(v.faction, NavalSupremacy) && f != v.faction) {
                    val ways = Costs.ways(v.faction.hand, reviveCost(v))
                    if (ways.any)
                        return Ask(v.faction).each(ways)(p => FrigateReviveNowAction(v.faction, v, p, AfterAttackAction(f, u, v.faction, counter))).add(FrigateReviveSkipAction(v.faction, AfterAttackAction(f, u, v.faction, counter)))
                }

                AfterAttackAction(f, u, v.faction, counter)

            case FrigateReviveNowAction(e, v, p, then) =>
                pay(e, p)
                injured -= v
                log(e, "revived", v, "at once with", NavalSupremacy, "for", p.intersperse(","))
                then

            case FrigateReviveSkipAction(e, then) =>
                then

            case AttackFortAction(f, u, h, counter) =>
                ambushPlayed(f, counter)
                val t = forts(h)
                forts += h -> t.copy(damaged = true)
                log(u, "attacked", t.owner, "Fort in", h, "and damaged it")

                counter match {
                    case Some(_) => counterUsed += u
                    case None =>
                        acted += u
                        movedOne -= u
                        attacks += u -> (attacks.getOrElse(u, 0) + 1)
                }

                AfterAttackAction(f, u, t.owner, counter)

            case AfterAttackAction(f, u, e, counter) =>
                counter match {
                    case Some(c) if c.ambush => CounterAskAction(c.defender, c.active, true)
                    case Some(c) => ResolveCountersAction(c.active)
                    case None =>
                        if (factions.has(e))
                            pendingCounters :+= e
                        afterOwnAction(f, u)
                }

            case ResolveCountersAction(f) =>
                pendingCounters match {
                    case d :: rest =>
                        pendingCounters = rest
                        CounterAskAction(d, f, false)
                    case Nil =>
                        MainAction(f)
                }

            case CounterAskAction(d, f, ambushed) =>
                val counter = Counter(d, f, false)
                val ready = units(d).%(u => counterUsed.contains(u).not && unitActions(d, u, Some(counter)).any)
                val ambush = (d.hand.has(Ambush) && ambushed.not).??(units(d).%(u => Board.terrain(at(u)) == Forest && counterUsed.contains(u).not && unitActions(d, u, Some(counter.copy(ambush = true))).any))
                val repair = hasSeal(d, StonewallSociety).??(repairSites(d)./~(h => Costs.ways(d.hand, Costs.build(RepairBuild))./(p => StonewallRepairAction(d, h, p, |(f)))))

                Ask(d)
                    .each(ready)(u => CounterUnitAction(d, u, counter))
                    .when(ambush.any)(AmbushMenuAction(d, f))
                    .add(repair)
                    .add(CounterPassAction(d, f))

            case CounterUnitAction(d, u, counter) =>
                Ask(d).add(unitActions(d, u, Some(counter))).cancel

            case AmbushMenuAction(d, f) =>
                val counter = Counter(d, f, true)
                Ask(d).each(units(d).%(u => Board.terrain(at(u)) == Forest && counterUsed.contains(u).not && unitActions(d, u, Some(counter)).any))(u => AmbushUnitAction(d, u, counter)).cancel

            case AmbushUnitAction(d, u, counter) =>
                Ask(d).add(unitActions(d, u, Some(counter))).cancel

            case CounterPassAction(d, f) =>
                ResolveCountersAction(f)

            case StonewallRepairAction(d, h, p, f) =>
                pay(d, p)
                forts += h -> forts(h).copy(damaged = false)
                log(d, "repaired the Fort in", h, "with", StonewallSociety)
                f match {
                    case Some(f) => ResolveCountersAction(f)
                    case None => MainAction(d)
                }

            case ReviveAction(f, u, p, then) =>
                pay(f, p)
                injured -= u
                acted += u
                fresh += u
                log(f, "revived", u, "for", p.intersperse(","))
                then

            // BUILDING
            case BuildMenuAction(f) =>
                def ways(b : Build) = Costs.ways(f.hand, Costs.build(b))
                Ask(f)
                    .add(colonySites(f).flatMap(h => ways(ColonyBuild).map(p => BuildColonyAction(f, h, p))))
                    .add(roadSites(f).flatMap { case (a, b) => ways(RoadBuild).map(p => BuildRoadAction(f, a, b, p)) })
                    .add(bridgeSites(f).flatMap { case (a, w, b) => ways(BridgeBuild).map(p => BuildBridgeAction(f, a, w, b, p)) })
                    .add(portSites(f).flatMap { case (h, c) => ways(PortBuild).map(p => BuildPortAction(f, h, c, p)) })
                    .add(fortSites(f).flatMap(h => ways(FortBuild).map(p => BuildFortAction(f, h, p))))
                    .add(repairSites(f).flatMap(h => ways(RepairBuild).map(p => RepairFortAction(f, h, p))))
                    .each(removableForts(f))(h => RemoveFortAction(f, h))
                    .cancel

            case BuildColonyAction(f, h, p) =>
                pay(f, p)
                colonies += h -> f
                f.coloniesFounded += 1
                log(f, "founded a Colony in", h, "for", p.intersperse(","))
                MainAction(f)

            case BuildRoadAction(f, a, b, p) =>
                pay(f, p)
                roads :+= Road(f, a, b)
                log(f, "built a Road between", a, "and", b)
                MainAction(f)

            case BuildBridgeAction(f, a, w, b, p) =>
                pay(f, p)
                bridges :+= Bridge(f, a, w, b)
                log(f, "built a Bridge from", a, "over", w, "to", b)
                MainAction(f)

            case BuildPortAction(f, h, c, p) =>
                pay(f, p)
                ports += h -> Port(f, c)
                f.portsBuilt += 1
                log(f, "built a Port in", h)
                MainAction(f)

            case BuildFortAction(f, h, p) =>
                pay(f, p)
                forts += h -> Fort(f, false)
                f.fortsBuilt += 1
                log(f, "built a Fort in", h)
                MainAction(f)

            case RepairFortAction(f, h, p) =>
                pay(f, p)
                forts += h -> forts(h).copy(damaged = false)
                log(f, "repaired the Fort in", h)
                MainAction(f)

            case RemoveFortAction(f, h) =>
                val e = forts(h).owner
                forts -= h
                e.fortDestroyer = |(f)
                log(f, "removed", e, "Fort in", h)
                MainAction(f)

            // RECRUITING
            case RecruitMenuAction(f) =>
                Ask(f).add(Kind.all.%(k => supply(f, k).any).flatMap(k => recruitSites(f, k).flatMap(h => Costs.ways(f.hand, Costs.recruit(k)).map(p => RecruitAction(f, k, h, p))))).cancel

            case RecruitAction(f, k, h, p) =>
                pay(f, p)
                val u = supply(f, k).head
                pos += u -> h
                fresh += u
                log(f, "recruited", k, "in", h, "for", p.intersperse(","))
                MainAction(f)

            // TRADE
            case TradeMenuAction(f) =>
                Ask(f).each(Resource.all.%(r => tradeSource(f, r).any || bankHas(r)))(r => TradeForAction(f, r).!(f.hand.of[Resource].num < rate(f))).cancel

            case TradeForAction(f, r) =>
                tradePay(f, r, $)

            case TradePayAction(f, r, paid) =>
                tradePay(f, r, paid)

            case TradeAction(f, r, paid) =>
                trades += 1
                tradeSource(f, r) match {
                    case Some(e) =>
                        paid.foreach { x => f.hand = f.hand :- x ; e.hand :+= x }
                        stockpiles += r -> stockpiles(r).drop(1)
                        f.hand :+= r
                        log(f, "traded", paid.intersperse(","), "to", e, "for", r, "from", Seal.company(r))
                    case None =>
                        pay(f, paid)
                        if (takeFromBank(r)) {
                            f.hand :+= r
                            log(f, "traded", paid.intersperse(","), "for", r)
                        }
                        else
                            log(f, "traded", paid.intersperse(","), "but the bank had no", r)
                }
                MainAction(f)

            // COMPANIES
            case CompanyMenuAction(f) =>
                Ask(f)
                    .each(Seal.companies.%(c => seals.contains(c).not && companyBases(f, c.resource).any && f.hand.count(c.resource) >= 5))(c => FoundCompanyAction(f, c))
                    .add(Seal.companies.%(c => seals.get(c).%(_ != f).any && companyBases(f, c.resource).any).flatMap(c => (stockpiles(c.resource).num + 1).to(f.hand.count(c.resource)).toList.map(n => BuyoutAction(f, c, n))))
                    .cancel

            case FoundCompanyAction(f, c) =>
                5.times(c.resource).foreach(r => f.hand = f.hand :- r)
                stockpiles += c.resource -> 5.times(c.resource)
                log(f, "founded", c, "with", 5.hl, c.resource)
                claim(c, f)
                MainAction(f)

            case BuyoutAction(f, c, n) =>
                val e = seals(c)
                e.hand ++= stockpiles(c.resource)
                n.times(c.resource).foreach(r => f.hand = f.hand :- r)
                log(f, "bought out", c, "from", e, "with", n.hl, c.resource, ";", e, "took the", stockpiles(c.resource).num.hl, "cards of its stockpile")
                stockpiles += c.resource -> n.times(c.resource)
                claim(c, f)
                MainAction(f)

            // DIPLOMACY
            case DiplomacyMenuAction(f) =>
                val others = factions.but(f)
                Ask(f)
                    .add(others.%(e => isAtWar(f, e).not && f.warCards > 0).flatMap(e => $(DeclareWarAction(f, e, false), DeclareWarAction(f, e, true))))
                    .each(others.%(e => isAtWar(f, e) && peaceOffered.has(e).not))(e => OfferPeaceAction(f, e))
                    .each(others.%(_ => f.hand.any))(e => GiveMenuAction(f, e))
                    .each(f.captured.map(_._2).%(u => factions.has(u.faction)))(u => ReturnPrisonerAction(f, u))
                    .cancel

            case DeclareWarAction(f, e, boar) =>
                f.warCards -= 1
                wars :+= War(f, e, boar)
                if (boar) {
                    log(f, "declared a", "Boar's War".hl, "on", e)
                    log("The Boar's War penalty isn't in the rulebook's second draft yet, so none is applied")
                }
                else {
                    gentleman :+= (f, e)
                    log(f, "declared a", "Gentleman's War".hl, "on", e)
                }
                MainAction(f)

            case OfferPeaceAction(f, e) =>
                peaceOffered :+= e
                log(f, "offered peace to", e)
                PeaceAskAction(e, f)

            case PeaceAskAction(e, f) =>
                Ask(e).add(AcceptPeaceAction(e, f)).add(DeclinePeaceAction(e, f))

            case AcceptPeaceAction(e, f) =>
                wars.%(_.between(e, f)).foreach { w => w.declarer.warCards += 1 }
                wars = wars.%(_.between(e, f).not)
                gentleman = gentleman.%!{ case (a, b) => (a == e && b == f) || (a == f && b == e) }
                log(e, "accepted peace with", f)
                MainAction(f)

            case DeclinePeaceAction(e, f) =>
                log(e, "declined peace with", f)
                MainAction(f)

            case GiveMenuAction(f, e) =>
                Ask(f).each(f.hand.distinct)(c => GiveCardAction(f, e, c)).cancel

            case GiveCardAction(f, e, c) =>
                f.hand = f.hand :- c
                e.hand :+= c
                log(f, "gave a card to", e)
                this.notify(e, $(DrewCardInfoAction(e, c)))
                MainAction(f)

            case ReturnPrisonerAction(f, u) =>
                f.captured = f.captured.filter(_._2 != u)
                log(f, "returned", u)
                val e = u.faction
                val sites = ownColonies(e) ++ ports.toList.filter(_._2.owner == e).map(_._1).%(h => u.kind == Frigate)
                val ok = sites.%(h => u.land != (Board.terrain(h) == Water))
                if (ok.none) {
                    log(u, "has nowhere to go and waits in", e, "supply")
                    MainAction(f)
                }
                else
                    Ask(e).each(ok)(h => PlacePrisonerAction(e, u, h, f))

            case PlacePrisonerAction(e, u, h, f) =>
                pos += u -> h
                fresh += u
                log(u, "was placed in", h)
                MainAction(f)

            case SurrenderAction(f) =>
                log(f, "surrendered")
                units(f).foreach { u => pos -= u ; injured -= u ; embarked -= u }
                embarked = embarked.filter(_._2.faction != f)
                colonyOf(f).foreach { h =>
                    occupier(h).foreach { e => colonies += h -> e }
                }
                roads = roads.%(_.owner != f)
                bridges = bridges.%(_.owner != f)
                ports = ports.filter(_._2.owner != f)
                forts = forts.filter(_._2.owner != f)
                wars = wars.%(w => w.declarer != f && w.target != f)
                sealsOf(f).foreach { s =>
                    seals -= s
                    s match {
                        case c : Company =>
                            discards += c.resource -> (discards(c.resource) ++ stockpiles(c.resource))
                            stockpiles += c.resource -> $
                        case _ =>
                    }
                }
                factions = factions.but(f)
                if (factions.num == 1)
                    GameOverAction(factions)
                else
                    NextTurnAction(f)

            case GameOverAction(w) =>
                isOver = true
                w.foreach(f => log(f, "won with", sealsOf(f).num.hl, "Royal Seals"))
                GameOver(w, "Game Over", w./(f => GameOverWonAction(null, f)))

            case a : SelfPerform =>
                a.perform(soft)(this)
        }
    }

    // the cards paid for a trade are picked one at a time, in the order of Resource.all, so each set is offered once
    def tradePay(f : Faction, r : Resource, paid : $[Resource]) : Continue = {
        val n = rate(f)
        if (paid.num >= n)
            Ask(f).add(TradeAction(f, r, paid)).cancel
        else {
            val left = f.hand.of[Resource].diff(paid)
            val from = paid.lastOption./(x => Resource.all.indexOf(x)).|(0)
            Ask(f).each(Resource.all.drop(from).%(left.has))(x => TradePayAction(f, r, paid :+ x)).cancel
        }
    }

    def ambushPlayed(f : Faction, counter : |[Counter]) {
        if (counter.exists(_.ambush)) {
            discard(f, Ambush)
            log(f, "played", Ambush)
        }
    }

    def afterOwnAction(f : Faction, u : Figure) : Continue =
        if (extra.contains(u))
            ExtraNextAction(f, u)
        else
            ResolveCountersAction(f)
}

