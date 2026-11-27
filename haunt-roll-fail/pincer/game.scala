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


// ICONS (tools/icons): with a word on choices, alone where a number or a place makes them clear; the alt text says what they are

object Icons {
    val all = $("trade", "war-card", "war", "peace", "wax-seal", "build", "recruit", "company", "diplomacy", "end-turn", "move", "revive", "done",
        "decline", "repair", "notifications", "interface", "bug", "home", "list", "hex", "boar")

    // the cards' icon is the back of the cards
    private def image(s : String) = (s == "cards").?("card:card-back").|("icon:" + (s == "seal").?("wax-seal").|(s))
    def apply(s : String) : Elem = Image(image(s), (s == "cards").?(styles.cardIcon).|(styles.icon), "")
    def only(s : String, alt : String) : Elem = Hint(alt, Image(image(s), (s == "cards").?(styles.cardIconOnly).|(styles.iconOnly), alt))
    def piece(s : String, f : Faction) : Elem = Image("piece:" + s + "-" + f.id, styles.icon, "")
    // a payment, as pictures of its cards
    def paid(l : $[Resource]) : Elem = l./(_.pic : Elem).merge
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
    def pic : Elem = Hint(name, Image("die:" + id, styles.iconOnly, name))
    override def elem = (Image("die:" + id, styles.icon, "") ~ name.styled(this)).spn(xlo.nowrap)
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
    def elem = (Image("piece:" + kind.id + "-" + faction.id, styles.icon, faction.name) ~ kind.name.styled(faction)).spn(xlo.nowrap)
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
case class EndTurnAction(self : Faction) extends BaseAction(None)(Icons("end-turn") ~ "End turn")
case class NextTurnAction(f : Faction) extends ForcedAction

// units: an action is done one step at a time, and finished before another unit acts
case class Counter(defender : Faction, active : Faction, ambush : Boolean) extends Record

case class UnitMenuAction(self : Faction, figure : Figure) extends BaseAction("Units")(figure, "in", (g : Game) => g.at(figure).elem) with Soft with FigureKey
case class StepAction(self : Faction, figure : Figure, hex : Hex, counter : |[Counter]) extends BaseAction(figure, "in", (g : Game) => g.at(figure).elem, (g : Game) => g.stepsNote(figure))(Icons("move") ~ "Move to", hex, (g : Game) => g.moveNote(figure, hex)) with HexKey
case class AttackAction(self : Faction, unit : Figure, target : Figure, counter : |[Counter]) extends BaseAction(unit, "in", (g : Game) => g.at(unit).elem)(Icons("war") ~ "Attack", target, "in", (g : Game) => g.at(target).elem) with FigureKey { def figure = target }
case class AttackFortAction(self : Faction, figure : Figure, hex : Hex, counter : |[Counter]) extends BaseAction(figure, "in", (g : Game) => g.at(figure).elem)(Icons("war") ~ "Attack", (g : Game) => g.forts(hex).owner.elem, "Fort in", hex) with HexKey
case class ReviveAction(self : Faction, figure : Figure, paid : $[Resource], then : ForcedAction) extends BaseAction(figure, "in", (g : Game) => g.at(figure).elem)(Icons("revive") ~ "Revive", "for", Icons.paid(paid)) with FigureKey
case class HideoutAction(self : Faction, figure : Figure, hex : Hex) extends BaseAction(figure, "in", (g : Game) => g.at(figure).elem)("Play", Hideout, "and enter", hex) with HexKey
case class ExtraActionAction(self : Faction, figure : Figure, card : Maneuver) extends BaseAction(figure, "in", (g : Game) => g.at(figure).elem)("Play", card, "for one more move or attack")
case class DoneUnitAction(self : Faction, figure : Figure, counter : |[Counter]) extends BaseAction(figure, "in", (g : Game) => g.at(figure).elem, (g : Game) => g.stepsNote(figure))(Icons("done") ~ "Done")

case class ContinueUnitAction(f : Faction, figure : Figure, counter : |[Counter]) extends ForcedAction
case class FinishUnitAction(f : Faction, figure : Figure, counter : |[Counter]) extends ForcedAction
case class ResolveCountersAction(f : Faction) extends ForcedAction

// counter-actions
case class CounterAskAction(d : Faction, f : Faction, ambushed : Boolean) extends ForcedAction
case class CounterUnitAction(self : Faction, figure : Figure, counter : Counter) extends BaseAction("Counter-Action against", counter.active)(figure, "in", (g : Game) => g.at(figure).elem) with Soft with FigureKey
case class AmbushMenuAction(self : Faction, f : Faction) extends BaseAction("Counter-Action against", f)("Play", Ambush, "(another move or attack with a unit in a forest)") with Soft
case class AmbushUnitAction(self : Faction, figure : Figure, counter : Counter) extends BaseAction(Ambush)(figure, "in", (g : Game) => g.at(figure).elem) with Soft with FigureKey
case class CounterPassAction(self : Faction, f : Faction) extends BaseAction("Counter-Action against", f)(Icons("decline") ~ "Pass")
case class StonewallRepairAction(self : Faction, hex : Hex, paid : $[Resource], f : |[Faction]) extends BaseAction(StonewallSociety)(Icons("repair") ~ "Repair Fort in", hex, "for", Icons.paid(paid)) with HexKey
case class FrigateReviveNowAction(self : Faction, figure : Figure, paid : $[Resource], then : ForcedAction) extends BaseAction(NavalSupremacy)(Icons("revive") ~ "Revive", figure, "now for", Icons.paid(paid)) with FigureKey
case class FrigateReviveSkipAction(self : Faction, then : ForcedAction) extends BaseAction(NavalSupremacy)(Icons("decline") ~ "Don't revive")

// defensive maneuvers: asked on every attack on wheat or hills, so asking tells nothing of the hand
case class DefendAskAction(f : Faction, figure : Figure, target : Figure, counter : |[Counter]) extends ForcedAction
case class RetreatAction(self : Faction, target : Figure, hex : Hex, f : Faction, figure : Figure, counter : |[Counter]) extends BaseAction(figure, "attacks", target)("Play", Retreat, "and move to", hex) with HexKey
case class FortifyAction(self : Faction, target : Figure, f : Faction, figure : Figure, counter : |[Counter]) extends BaseAction(figure, "attacks", target)("Play", Fortify, "and deflect the attack")
case class TakeHitAction(self : Faction, target : Figure, f : Faction, figure : Figure, counter : |[Counter]) extends BaseAction(figure, "attacks", target)("Take the hit")
case class HitAction(f : Faction, figure : Figure, target : Figure, counter : |[Counter]) extends ForcedAction
case class AfterAttackAction(f : Faction, figure : Figure, defender : Faction, counter : |[Counter]) extends ForcedAction

// building
case class BuildMenuAction(self : Faction) extends BaseAction(None)(Icons("build") ~ "Build") with Soft
case class BuildColonyAction(self : Faction, hex : Hex, paid : $[Resource]) extends BaseAction("Build", ColonyBuild, "for", Icons.paid(paid))(Icons.piece("colony", self) ~ "Colony in", hex) with HexKey
case class BuildRoadAction(self : Faction, a : Hex, b : Hex, paid : $[Resource]) extends BaseAction("Build", RoadBuild, "for", Icons.paid(paid))("Road between", a, "and", b) with HexKey { def hex = b }
case class BuildBridgeAction(self : Faction, a : Hex, w : Hex, b : Hex, paid : $[Resource]) extends BaseAction("Build", BridgeBuild, "for", Icons.paid(paid))("Bridge from", a, "over", w, "to", b) with HexKey { def hex = w }
case class BuildPortAction(self : Faction, hex : Hex, colony : Hex, paid : $[Resource]) extends BaseAction("Build", PortBuild, "for", Icons.paid(paid))(Icons.piece("port", self) ~ "Port in", hex, "for the Colony in", colony) with HexKey
case class BuildFortAction(self : Faction, hex : Hex, paid : $[Resource]) extends BaseAction("Build", FortBuild, "for", Icons.paid(paid))(Icons.piece("fort", self) ~ "Fort in", hex) with HexKey
// what each building costs, shown above the Build choices
case class BuildCostInfoAction(self : Faction, build : Build) extends BaseInfo("Costs")(build, Icons.paid(build.cost))
case class RepairFortAction(self : Faction, hex : Hex, paid : $[Resource]) extends BaseAction("Repair", "for", Icons.paid(paid))(Icons("repair") ~ "Fort in", hex) with HexKey

// recruiting
case class RecruitMenuAction(self : Faction) extends BaseAction(None)(Icons("recruit") ~ "Recruit") with Soft
case class RecruitAction(self : Faction, kind : Kind, hex : Hex, paid : $[Resource]) extends BaseAction("Recruit", Icons.piece(kind.id, self) ~ kind.elem, "for", Icons.paid(paid))("in", hex) with HexKey

// trade
case class TradeMenuAction(self : Faction) extends BaseAction(None)(Icons("trade") ~ "Trade", (g : Game) => ("(" + g.rate(self) + ":1)").hl) with Soft
case class TradeForAction(self : Faction, r : Resource) extends BaseAction("Trade", (g : Game) => g.rate(self).hl, "cards for")(r, (g : Game) => g.tradeSource(self, r)./(e => " from " ~ Seal.company(r).elem ~ " (" ~ e.elem ~ ")").|(Empty)) with Soft
case class TradePayAction(self : Faction, r : Resource, paid : $[Resource]) extends BaseAction("Trade for", r, "paying", Icons.paid(paid))("Add", (paid.lastOption : |[Card])./(_.elem).|(Empty)) with Soft
case class TradeAction(self : Faction, r : Resource, paid : $[Resource]) extends BaseAction("Trade for", r)("Pay", Icons.paid(paid))

// trading companies
case class CompanyMenuAction(self : Faction) extends BaseAction(None)(Icons("company") ~ "Trading Companies") with Soft
case class FoundCompanyAction(self : Faction, company : Company) extends BaseAction("Trading Companies")("Found", company, "with", 5.hl, company.resource, "cards")
case class BuyoutAction(self : Faction, company : Company, n : Int) extends BaseAction("Trading Companies")("Forcible buyout of", company, "with", n.hl, company.resource, "cards")
case class DepositAction(self : Faction, company : Company, card : Card) extends BaseAction(company, "stockpile")("Deposit", card)
case class WithdrawAction(self : Faction, company : Company) extends BaseAction(company, "stockpile")(Icons("cards") ~ "Take the top card")

// diplomacy
case class Terms(give : $[Card], demand : $[Resource], release : $[Figure], ransom : $[Figure]) extends Record

case class DiplomacyMenuAction(self : Faction) extends BaseAction(None)(Icons("diplomacy") ~ "Diplomacy") with Soft
case class DeclareWarAction(self : Faction, e : Faction, boar : Boolean) extends BaseAction("Declare War on", e)(boar.?(Icons("boar")).|(Icons("war")) ~ boar.?("Boar's War".hl ~ " (attack at once)").|("Gentleman's War".hl ~ " (attack from your next turn)"))
case class PeaceTermsAction(self : Faction, e : Faction, now : Terms, terms : Terms, last : Elem) extends BaseAction("Peace terms for", e, (g : Game) => g.termsElem(self, e, now))(last) with SelfExplode {
    // the terms can't be listed in full: the bots offer none, and the plain offer validates every step of them
    def explode(withSoft : Boolean)(implicit g : Game) = withSoft.$(this) ++ $(OfferPeaceAction(self, e, Terms($, $, $, $)))
}
case class OfferPeaceAction(self : Faction, e : Faction, terms : Terms) extends BaseAction("Peace terms for", e, (g : Game) => g.termsElem(self, e, terms))(Icons("peace") ~ "Offer peace") with SelfValidate {
    def validate(a : Action) = a @@ {
        case OfferPeaceAction(f, x, _) => f == self && x == e
        case PeaceTermsAction(f, x, _, _, _) => f == self && x == e
        case _ => false
    }
}
case class PeaceAskAction(e : Faction, f : Faction, terms : Terms) extends ForcedAction
case class AcceptPeaceAction(self : Faction, f : Faction, terms : Terms) extends BaseAction(f, "offers peace", (g : Game) => g.termsElem(f, self, terms))(Icons("done") ~ "Accept")
case class DeclinePeaceAction(self : Faction, f : Faction, terms : Terms) extends BaseAction(f, "offers peace")(Icons("decline") ~ "Decline")
case class PlaceReturnedAction(l : $[Figure], then : ForcedAction) extends ForcedAction
case class PlacePrisonerAction(self : Faction, figure : Figure, hex : Hex, l : $[Figure], then : ForcedAction) extends BaseAction(figure, "returned")("Place in", hex) with HexKey
case class SupplyPrisonerAction(self : Faction, figure : Figure, l : $[Figure], then : ForcedAction) extends BaseAction(figure, "returned")("Keep in the supply")

case class SurrenderAction(self : Faction) extends BaseAction("Every Colony annexed")(Icons("peace") ~ "Surrender".hl)
case class SurrenderColoniesAction(f : Faction, left : $[Faction]) extends ForcedAction
case class KeepColonyAction(self : Faction, f : Faction, hex : Hex, left : $[Faction]) extends BaseAction("Too few Colonies to replace all of", f, "annexed")("Replace the Colony in", hex) with HexKey

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
    var fortRemovedAt = Map[Hex, Int]()
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
    var moved = Map[Figure, Int]()
    var stopped = Set[Figure]()
    var attacks = Map[Figure, Int]()
    var extra = Map[Figure, Maneuver]()
    var counterUsed = Set[Figure]()
    var pendingCounters = $[Faction]()
    var peaceOffered = $[Faction]()
    var declared = $[Faction]()
    var trades = 0

    var highlightFaction = $[Faction]()

    // what happened to whom, for the Pamphleteer's jests (display only: online.scala)
    var jests : $[String] = $

    // FIGURES: one unit a hex, but for a frigate's passenger

    def at(u : Figure) : Hex = pos(u)
    def on(h : Hex) : $[Figure] = pos.toList.filter(_._2 == h).map(_._1).sortBy(u => (setup.indexOf(u.faction), Kind.all.indexOf(u.kind), u.index))
    def landOn(h : Hex) : |[Figure] = on(h).%(u => u.land && embarked.contains(u).not).starting
    def units(f : Faction) : $[Figure] = pos.keys.$.%(_.faction == f).sortBy(u => (Kind.all.indexOf(u.kind), u.index))
    def isInjured(u : Figure) = injured.contains(u)
    def passenger(v : Figure) : |[Figure] = embarked.toList.find(_._2 == v).map(_._1)

    def supply(f : Faction, k : Kind) : $[Figure] = 1.to(k.count).$./(Figure(f, k, _)).%(u => pos.contains(u).not && factions./~(_.captured.map(_._2)).has(u).not)

    def colonyOf(f : Faction) : $[Hex] = colonies.toList.filter(_._2 == f).map(_._1).sortBy(x => (x.r, x.c))

    // CONTROL: a colony is controlled by another player's land unit in it, else by another player's fort around it, else by its owner

    def controller(h : Hex) : Faction = {
        val o = colonies(h)
        landOn(h).map(_.faction).%(_ != o).||(forts.get(h).map(_.owner).%(_ != o)).|(o)
    }

    def annexer(h : Hex) : |[Faction] = colonies.get(h).%(_ => controller(h) != colonies(h)).map(_ => controller(h))

    def annexed(h : Hex) = annexer(h).any

    def ownColonies(f : Faction) = colonyOf(f).%(annexed(_).not)

    def blockaded(h : Hex) : |[Faction] = ports.get(h)./~(p => on(h).%(u => u.kind == Frigate && u.faction != p.owner)./(_.faction).distinct.single)

    def isAtWar(a : Faction, b : Faction) = wars.exists(_.between(a, b))

    def canAttack(f : Faction, e : Faction) = f != e && isAtWar(f, e) && gentleman.has((f, e)).not

    def hasSeal(f : Faction, s : Seal) = seals.get(s).has(f)

    def sealsOf(f : Faction) = Seal.all.%(s => seals.get(s).has(f))

    // ROADS: a road or bridge is cut off while another player has a land unit at each of its ends

    def cutBy(a : Hex, b : Hex, owner : Faction) : |[Faction] = (landOn(a), landOn(b)) match {
        case (Some(x), Some(y)) if x.faction == y.faction && x.faction != owner => |(x.faction)
        case _ => None
    }

    def allLinks(f : Faction) : $[(Hex, Hex)] = roads.%(_.owner == f)./(r => (r.a, r.b)) ++ bridges.%(_.owner == f)./(b => (b.a, b.b))

    def links(f : Faction) : $[(Hex, Hex)] = allLinks(f).%{ case (a, b) => cutBy(a, b, f).none }

    def linked(l : $[(Hex, Hex)], x : Hex) = l.flatMap { case (a, b) => if (a == x) $(b) else if (b == x) $(a) else $() }

    // the player's own colonies joined to a colony by its roads and bridges without passing through another colony
    def joined(f : Faction, start : Hex) : $[Hex] = {
        val l = links(f)
        var seen = Set(start)
        var front = $(start)
        var result = $[Hex]()
        while (front.any) {
            val next = front./~(x => linked(l, x)).distinct.%(x => seen.contains(x).not)
            seen ++= next
            result ++= next.%(colonies.contains)
            front = next.%(x => colonies.contains(x).not)
        }
        result.%(c => colonies.get(c).has(f) && annexed(c).not).distinct
    }

    // a road network joins two of the player's own colonies: three in a row are two networks
    def networks(f : Faction) : Int = {
        val cc = ownColonies(f)
        cc./(c => joined(f, c).%(cc.has).num).sum / 2
    }

    def inNetwork(f : Faction, h : Hex) = joined(f, h).any

    def portBonus(f : Faction) = ports.toList.count { case (h, p) => p.owner == f && annexed(p.colony).not && (blockaded(h).none || hasSeal(f, MaritimeExchange)) }

    def rate(f : Faction) = max(1, 5 - networks(f) - portBonus(f))

    // TRADE

    def tradeSource(f : Faction, r : Resource) : |[Faction] = seals.get(Seal.company(r)).%(_ != f).%(_ => stockpiles(r).any)

    def bankHas(r : Resource) = decks(r).has(r) || discards(r).has(r)

    def matching(f : Faction, r : Resource) = f.hand.%(_.resource == r).sortBy(_.is[Maneuver])

    // MOVEMENT, one step at a time

    def range(u : Figure) = u.kind match {
        case Cavalry if hasSeal(u.faction, MountedVanguard) => 3
        case Cavalry => 2
        case Frigate => 2
        case _ => 1
    }

    // the hexes one step away: neighbors, the far end of a bridge, and for a land unit in the player's own colony, the colonies its roads join
    def stepsFrom(u : Figure, x : Hex) : $[Hex] =
        if (u.kind == Frigate)
            Board.neighbors(x).%(h => Board.terrain(h) == Water)
        else
            (Board.neighbors(x) ++ bridges./~(b => (b.a == x).?(b.b) ++ (b.b == x).?(b.a)) ++
                (colonies.get(x).has(u.faction) && annexed(x).not).??(joined(u.faction, x))).distinct

    // other players' figures in a hex
    def others(f : Faction, h : Hex) = on(h).%(_.faction != f)

    def boatFor(u : Figure, h : Hex) : |[Figure] = on(h).%(v => v.faction == u.faction && v.kind == Frigate && passenger(v).none && isInjured(v).not).starting

    // whether a step into h is allowed (capturing, annexing or embarking), the other players' units it captures being all of only's, if given
    def canEnter(u : Figure, h : Hex, only : |[Faction]) : Boolean = {
        val f = u.faction
        val o = others(f, h)
        val ok = only.%(_ => o.any).forall(e => o.forall(_.faction == e))
        if (u.kind == Frigate)
            Board.terrain(h) == Water && ok && o.forall(v => v.kind == Frigate && isInjured(v) && canAttack(f, v.faction))
        else
        if (Board.terrain(h) == Water)
            boatFor(u, h).any && o.none
        else
        if (Board.terrain(h) == Mountain && colonies.get(h).has(f).not)
            false
        else
        if (forts.get(h).exists(t => t.owner != f && t.damaged.not))
            false
        else
        if (o.any)
            ok && o.forall(v => v.land && isInjured(v) && embarked.contains(v).not && canAttack(f, v.faction))
        else
            colonies.get(h).%(_ != f).forall(e => canAttack(f, e) && only.forall(_ == e))
    }

    // a unit may end a step in a hex with none of the player's other units (or on its own empty frigate)
    def canStop(u : Figure, h : Hex) = (u.land && Board.terrain(h) == Water) || on(h).%(v => v != u && v.faction == u.faction).none

    // embarking and entering another player's colony end the move
    def endsMove(u : Figure, h : Hex) = (u.land && Board.terrain(h) == Water) || colonies.get(h).%(_ != u.faction).any

    def canSettle(u : Figure, from : Hex, h : Hex, left : Int, only : |[Faction]) : Boolean =
        canStop(u, h) || (left > 0 && endsMove(u, h).not && stepsFrom(u, h).but(from).exists(n => canEnter(u, n, only) && canSettle(u, h, n, left - 1, only)))

    def stepTargets(u : Figure, only : |[Faction]) : $[Hex] = {
        val from = at(u)
        val left = range(u) - moved.getOrElse(u, 0)
        (left > 0 && stopped.contains(u).not).??(stepsFrom(u, from).%(h => canEnter(u, h, only) && canSettle(u, from, h, left - 1, only)))
    }

    // the empty hexes a unit could reach, for Retreat
    def reachableEmpty(u : Figure) : $[Hex] = {
        var seen = Set(at(u))
        var front = $(at(u))
        var result = $[Hex]()
        1.to(range(u)).foreach { _ =>
            val next = front./~(x => stepsFrom(u, x)).distinct.%(x => seen.contains(x).not)
            seen ++= next
            val ok = next.%(x => (u.kind == Frigate).?(Board.terrain(x) == Water).|(Board.terrain(x).land && (Board.terrain(x) != Mountain || colonies.get(x).has(u.faction))))
                .%(x => others(u.faction, x).none && colonies.get(x).%(_ != u.faction).none && forts.get(x).%(_.owner != u.faction).none)
            result ++= ok.%(x => on(x).none)
            front = ok
        }
        result.distinct
    }

    def stepsNote(u : Figure) : Elem = moved.get(u).%(_ > 0)./(n => ("(moved " + n + " of " + range(u) + ")").spn(styles.small)).|(Empty)

    def moveNote(u : Figure, h : Hex) : Elem = {
        val f = u.faction
        val o = others(f, h)
        if (o.any)
            " (capture " ~ o./(_.elem).comma ~ ")"
        else
        if (u.land && Board.terrain(h) == Water)
            " (embark)".txt
        else
        if (colonies.get(h).%(_ != f).any)
            " (annex)".txt
        else
        if (on(h).exists(_.faction == f))
            " (passing)".txt
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
        forts.get(at(v)).%(_.owner == v.faction) match {
            case Some(Fort(_, false)) => true
            case Some(Fort(_, true)) => u.kind != Cannon && u.kind != Frigate
            case None => false
        }
    }

    // passengers can't be attacked; a counter-attack only hits the player whose turn it is
    def targets(u : Figure, only : |[Faction]) : $[Figure] = {
        val f = u.faction
        Board.within(at(u), attackRange(u), false)./~(on).%(v => v.faction != f && only.forall(_ == v.faction) && canAttack(f, v.faction) && isInjured(v).not && embarked.contains(v).not)
            .%(v => v.land || u.kind == Cannon || u.kind == Frigate)
            .%(v => shielded(u, v).not)
    }

    def fortTargets(u : Figure, only : |[Faction]) : $[Hex] = (u.kind == Cannon || u.kind == Frigate).??(Board.within(at(u), attackRange(u), false).%(h => forts.get(h).exists(t => t.damaged.not && only.forall(_ == t.owner) && canAttack(u.faction, t.owner))))

    def healthyEnough(u : Figure) = isInjured(u).not || (u.kind == Musketeer && hasSeal(u.faction, RoyalMusket))

    // on its owner's turn a unit moves, or attacks, or (cavalry) moves up to one less than its range and attacks;
    // cannons with Black Powder Volley attack twice
    def mayMoveNow(u : Figure) = acted.contains(u).not && fresh.contains(u).not && attacks.getOrElse(u, 0) == 0

    def mayAttackNow(u : Figure) = {
        val m = moved.getOrElse(u, 0)
        val a = attacks.getOrElse(u, 0)
        acted.contains(u).not && fresh.contains(u).not && embarked.contains(u).not && healthyEnough(u) && (
            (m == 0 && a == 0) ||
            (u.kind == Cavalry && a == 0 && m < range(u)) ||
            (u.kind == Cannon && a == 1 && m == 0 && hasSeal(u.faction, BlackPowderVolley))
        )
    }

    def started(u : Figure) = moved.getOrElse(u, 0) > 0 || attacks.getOrElse(u, 0) > 0

    def revivable(u : Figure) = isInjured(u) && fresh.contains(u).not && acted.contains(u).not && started(u).not && (injured(u) < turn || (u.kind == Frigate && hasSeal(u.faction, NavalSupremacy)))

    def reviveCost(u : Figure) : $[$[Resource]] = hasSeal(u.faction, CrossroadsGuild).?(Costs.revive(u.kind)./(_ => Resource.all)).|(Costs.revive(u.kind))

    def unitActions(f : Faction, u : Figure, counter : |[Counter]) : $[UserAction] = counter match {
        case Some(c) =>
            val m = (started(u).not || moved.getOrElse(u, 0) > 0).??(stepTargets(u, |(c.active))./(h => StepAction(f, u, h, counter)))
            val a = (started(u).not && embarked.contains(u).not && healthyEnough(u)).??(targets(u, |(c.active))./(v => AttackAction(f, u, v, counter)) ++ fortTargets(u, |(c.active))./(h => AttackFortAction(f, u, h, counter)))
            m ++ a
        case None =>
            val m = mayMoveNow(u).??(stepTargets(u, None)./(h => StepAction(f, u, h, None)))
            val a = mayAttackNow(u).??(targets(u, None)./(v => AttackAction(f, u, v, None)) ++ fortTargets(u, None)./(h => AttackFortAction(f, u, h, None)))
            val r = revivable(u).??(Costs.ways(f.hand, reviveCost(u))./(p => ReviveAction(f, u, p, MainAction(f))))
            val fresh = mayMoveNow(u) && started(u).not
            val e = (fresh && extra.contains(u).not).??(
                (f.hand.has(Outflank) && u.land && Board.terrain(at(u)) == Rocks).$(ExtraActionAction(f, u, Outflank)) ++
                (f.hand.has(Broadside) && u.kind == Frigate).$(ExtraActionAction(f, u, Broadside))
            )
            val hide = (fresh && f.hand.has(Hideout) && u.land && embarked.contains(u).not).??(Board.neighbors(at(u)).%(h => Board.terrain(h) == Mountain && on(h).none && colonies.get(h).forall(c => c == f || canAttack(f, c)))./(h => HideoutAction(f, u, h)))
            m ++ a ++ r ++ e ++ hide
    }

    // BUILDING

    def touchesRoad(h : Hex) = roads.exists(r => r.a == h || r.b == h) || bridges.exists(b => b.a == h || b.b == h)

    // a colony is founded where the player's land unit stands, with no road on the hex's edges, 3 hexes from any colony over land
    def colonySites(f : Faction) : $[Hex] = (colonyOf(f).num < Pieces.colonies).??(units(f).%(_.land).%(u => embarked.contains(u).not)./(at).distinct.%{ h =>
        colonies.contains(h).not && Board.edge(h).not && Board.terrain(h).land && touchesRoad(h).not &&
        colonies.keys.forall(c => Board.landDistance(c, h, 2).none)
    })

    def startSites : $[Hex] = Board.hexes.%{ h =>
        Board.terrain(h).land && Board.terrain(h) != Mountain && Board.edge(h).not && on(h).none &&
        colonies.keys.forall(c => Board.landDistance(c, h, 2).none)
    }

    def roadEnds(f : Faction) = (ownColonies(f) ++ allLinks(f).flatMap { case (a, b) => $(a, b) }).distinct

    // a road joins two neighboring land hexes, continuing the player's path from its colony
    def roadSites(f : Faction) : $[(Hex, Hex)] = (roads.count(_.owner == f) < Pieces.roads).??{
        roadEnds(f)./~(a => Board.neighbors(a).%(b => Board.terrain(a).land && Board.terrain(b).land)./(b => (a, b)))
            ./{ case (a, b) => if (Board.hexes.indexOf(a) < Board.hexes.indexOf(b)) (a, b) else (b, a) }.distinct
            .%{ case (a, b) => roads.exists(r => r.a == a && r.b == b).not }
            .%{ case (a, b) => colonies.get(a).%(_ != f).none && colonies.get(b).%(_ != f).none }
    }

    def bridgeSites(f : Faction) : $[(Hex, Hex, Hex)] = (bridges.count(_.owner == f) < Pieces.bridges).??{
        roadEnds(f)./~(a => Board.neighbors(a).%(w => Board.terrain(w) == Water)./~(w => Board.across(a, w).%(b => Board.terrain(b).land)./(b => (a, w, b))))
            .%{ case (a, w, b) => bridges.exists(_.water == w).not && colonies.get(b).%(_ != f).none }
    }

    def portSites(f : Faction) : $[(Hex, Hex)] = (ports.values.count(_.owner == f) < Pieces.ports).??(ownColonies(f).%(c => ports.values.exists(_.colony == c).not)./~(c => Board.neighbors(c).%(h => Board.terrain(h) == Water && ports.contains(h).not && on(h).none)./(h => (h, c))))

    // the annexing player builds a fort from its next turn, and not the turn it removed the owner's
    def fortSites(f : Faction) : $[Hex] = (forts.values.count(_.owner == f) < Pieces.forts).??(
        (ownColonies(f) ++ colonies.keys.$.%(h => annexer(h).has(f) && annexedAt.get(h).exists { case (e, t) => e == f && t < turn } && fortRemovedAt.get(h).forall(_ < turn)))
            .distinct.%(h => forts.contains(h).not)
    )

    // a fort is repaired only while no other player's unit stands in it
    def repairSites(f : Faction) : $[Hex] = forts.toList.filter(_._2.owner == f).filter(_._2.damaged).map(_._1).%(h => others(f, h).none)

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

    // a player qualifies for a company with a colony it controls on that terrain (a port, for fish)
    def companyBases(f : Faction, r : Resource) : $[Hex] = r match {
        case Fish => ports.toList.filter(_._2.owner == f).map(_._1)
        case _ => colonies.keys.$.%(h => Board.terrain(h).resource == r && controller(h) == f)
    }

    // DIPLOMACY

    def termsElem(f : Faction, e : Faction, t : Terms) : Elem = {
        val l = $[Elem]() ++
            t.give.any.$(f.elem ~ " gives " ~ t.give./(_.elem).comma) ++
            t.demand.any.$(e.elem ~ " gives " ~ t.demand./(_.elem).comma) ++
            t.release.any.$(f.elem ~ " returns " ~ t.release./(_.elem).comma) ++
            t.ransom.any.$(e.elem ~ " returns " ~ t.ransom./(_.elem).comma)
        Break ~ l.some./(_.join(Break)).|("no terms".txt)
    }

    // INFO

    def preinfo(waiting : $[Faction], self : |[Faction], actions : $[UserAction]) : $[Info] =
        self.%(_ => actions.exists(_.unwrap @@ {
            case _ : BuildColonyAction | _ : BuildRoadAction | _ : BuildBridgeAction | _ : BuildPortAction | _ : BuildFortAction | _ : RepairFortAction => true
            case _ => false
        }))./~(f => $(ColonyBuild, PortBuild, FortBuild, RepairBuild, RoadBuild, BridgeBuild)./(BuildCostInfoAction(f, _)))

    def info(waiting : $[Faction], self : |[Faction], actions : $[UserAction]) : $[Info] =
        self.%(states.contains)./~(f => f.hand./(ViewCardInfoAction(f, _)))

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
        if (v.kind == Frigate)
            Jests.note(this, "captured", v.faction, f, v.kind.name)
        (v +: passenger(v).$).foreach { x =>
            pos -= x
            injured -= x
            embarked -= x
            f.captured :+= (by, x)
            log(f, "captured", x, "with", by)
        }
    }

    def moveFigure(u : Figure, h : Hex) {
        pos += u -> h
        passenger(u).foreach(p => pos += p -> h)
    }

    def removeFigure(u : Figure) {
        pos -= u
        injured -= u
        embarked -= u
    }

    // SEALS

    def claim(s : Seal, f : Faction) {
        if (seals.get(s).has(f).not) {
            seals.get(s) match {
                case Some(e) =>
                    log(f, "took", s, "from", e)
                    Jests.note(this, "seal", e, f, s.name)
                case None => log(f, "claimed", s)
            }
            seals += s -> f
        }
    }

    def checkSeals() {
        // annexations: when they started
        colonies.keys.foreach { h =>
            annexer(h) match {
                case Some(e) => if (annexedAt.get(h).map(_._1).has(e).not) { annexedAt += h -> (e, turn) ; log(e, "annexed", colonies(h), "Colony in", h) }
                case None => if (annexedAt.contains(h)) { annexedAt -= h ; log(colonies(h), "Colony in", h, "is no longer annexed") }
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
                cc./(annexer).distinct.single.foreach(_.foreach(e => claim(FoundersOath, e)))
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
            if (networks(h) == 0 && allLinks(h).any) {
                val culprits = allLinks(h)./~{ case (a, b) => cutBy(a, b, h) } ++ colonyOf(h)./~(annexer)
                culprits.distinct.single.foreach(e => claim(CrossroadsGuild, e))
            }
        }

        // military achievements: the most captures by each kind of unit; ties keep the holder
        Seal.badges.foreach { b =>
            val counts = factions./(f => f -> f.captured.count(_._1 == b.kind))
            val best = counts.map(_._2).maxOr(0)
            if (best > 0) {
                val held = seals.get(b)./~(h => counts.find(_._1 == h).map(_._2)).|(0)
                if (best > held)
                    claim(b, counts.filter(_._2 == best).head._1)
            }
        }
    }

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

    // the cards matching a company's resource, plain ones first
    def takeMatching(f : Faction, r : Resource, n : Int) : $[Card] = {
        val l = matching(f, r).take(n)
        l.foreach(c => f.hand = f.hand :- c)
        l
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

                Resource.all.foreach { r => decks += r -> Deck.of(r).diff(setup.num.times(r)) }

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
                moved = Map()
                stopped = Set()
                attacks = Map()
                extra = Map()
                counterUsed = Set()
                pendingCounters = $
                peaceOffered = $
                declared = $
                trades = 0
                gentleman = gentleman.filter(_._1 != f)

                log(DoubleLine)
                log("Turn", ("#" + turn).hl, f)

                // a player whose unit stands in a colony removes another player's damaged fort there
                forts.toList.foreach { case (h, t) =>
                    if (t.owner != f && t.damaged && landOn(h).exists(_.faction == f)) {
                        forts -= h
                        fortRemovedAt += h -> turn
                        t.owner.fortDestroyer = |(f)
                        log(f, "removed", t.owner, "damaged Fort in", h)
                    }
                }

                // trading companies change hands at the start of the turn of a player who annexes or blockades all their owner's bases
                Seal.companies.foreach { c =>
                    seals.get(c).%(_ != f).foreach { e =>
                        val bases = companyBases(e, c.resource) ++ colonies.keys.$.%(h => colonies(h) == e && Board.terrain(h).resource == c.resource && controller(h) != e)
                        val ok = bases.any && bases.forall(h => annexer(h).has(f) || blockaded(h).has(f))
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

            case StepAction(f, u, h, counter) =>
                counter.%(_.ambush).foreach { c => if (counterUsed.contains(u).not) ambushPlayed(f) }
                counter.foreach(_ => counterUsed += u)

                val from = at(u)
                val captives = others(f, h).%(v => (u.land && v.land) || (u.kind == Frigate && v.kind == Frigate))

                if (u.land && Board.terrain(h) == Water) {
                    val boat = boatFor(u, h).get
                    embarked += u -> boat
                    log(u, "embarked on", boat, "in", h)
                }
                else
                if (embarked.contains(u)) {
                    embarked -= u
                    log(u, "disembarked to", h)
                }
                else
                    log(u, "moved from", from, "to", h)

                moveFigure(u, h)
                moved += u -> (moved.getOrElse(u, 0) + 1)

                if (endsMove(u, h))
                    stopped += u

                captives.foreach(v => capture(f, u.kind, v))

                // a capture gives its victims a Counter-Action, once the unit's action is over
                if (counter.none)
                    pendingCounters = (pendingCounters ++ captives./(_.faction)).distinct.%(factions.has)

                ContinueUnitAction(f, u, counter)

            case ContinueUnitAction(f, u, counter) =>
                val l = counter match {
                    case Some(c) => stepTargets(u, |(c.active))./(h => StepAction(f, u, h, counter))
                    case None =>
                        mayMoveNow(u).??(stepTargets(u, None)./(h => StepAction(f, u, h, None))) ++
                        mayAttackNow(u).??(targets(u, None)./(v => AttackAction(f, u, v, None)) ++ fortTargets(u, None)./(h => AttackFortAction(f, u, h, None)))
                }

                if (l.none)
                    FinishUnitAction(f, u, counter)
                else
                    Ask(f).add(l).when(canStop(u, at(u)))(DoneUnitAction(f, u, counter))

            case DoneUnitAction(f, u, counter) =>
                FinishUnitAction(f, u, counter)

            case FinishUnitAction(f, u, counter) =>
                counter match {
                    case Some(c) =>
                        if (c.ambush)
                            CounterAskAction(c.defender, c.active, true)
                        else
                            ResolveCountersAction(c.active)
                    case None =>
                        if (extra.contains(u)) {
                            // Outflank and Broadside: one more move or attack, before any Counter-Action
                            extra -= u
                            moved -= u
                            attacks -= u
                            stopped -= u
                            ContinueUnitAction(f, u, None)
                        }
                        else {
                            acted += u
                            ResolveCountersAction(f)
                        }
                }

            case HideoutAction(f, u, h) =>
                discard(f, Hideout)
                log(f, "played", Hideout)
                log(u, "moved from", at(u), "to", h)
                moveFigure(u, h)
                moved += u -> range(u)
                FinishUnitAction(f, u, None)

            case ExtraActionAction(f, u, card) =>
                discard(f, card)
                extra += u -> card
                log(f, "played", card, "for", u)
                ContinueUnitAction(f, u, None)

            case AttackAction(f, u, v, counter) =>
                counter.%(_.ambush).foreach { c => if (counterUsed.contains(u).not) ambushPlayed(f) }
                log(u, "attacked", v, "in", at(v))

                counter match {
                    case Some(_) => counterUsed += u
                    case None => attacks += u -> (attacks.getOrElse(u, 0) + 1)
                }

                DefendAskAction(f, u, v, counter)

            case DefendAskAction(f, u, v, counter) =>
                val e = v.faction
                val t = Board.terrain(at(v))
                val retreat = (t == Field && e.hand.has(Retreat)).??(reachableEmpty(v))
                val fortify = t == Hills && e.hand.has(Fortify)

                if (t == Field || t == Hills)
                    Ask(e)
                        .each(retreat)(h => RetreatAction(e, v, h, f, u, counter))
                        .when(fortify)(FortifyAction(e, v, f, u, counter))
                        .add(TakeHitAction(e, v, f, u, counter))
                        .needOk
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
                counter.%(_.ambush).foreach { c => if (counterUsed.contains(u).not) ambushPlayed(f) }
                val t = forts(h)
                forts += h -> t.copy(damaged = true)
                log(u, "attacked", t.owner, "Fort in", h, "and damaged it")

                counter match {
                    case Some(_) => counterUsed += u
                    case None => attacks += u -> (attacks.getOrElse(u, 0) + 1)
                }

                AfterAttackAction(f, u, t.owner, counter)

            case AfterAttackAction(f, u, e, counter) =>
                counter match {
                    case Some(c) => FinishUnitAction(f, u, counter)
                    case None =>
                        if (factions.has(e))
                            pendingCounters = (pendingCounters :+ e).distinct
                        ContinueUnitAction(f, u, None)
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
                        val c = stockpiles(r).head
                        stockpiles += r -> stockpiles(r).drop(1)
                        f.hand :+= c
                        log(f, "traded", Icons.paid(paid), "to", e, "for the top card of", Seal.company(r))
                        if (c.is[Maneuver])
                            this.notify(f, $(DrewCardInfoAction(f, c)))
                    case None =>
                        pay(f, paid)
                        if (takeFromBank(r)) {
                            f.hand :+= r
                            log(f, "traded", Icons.paid(paid), "for", r)
                        }
                        else
                            log(f, "traded", Icons.paid(paid), "but the bank had no", r)
                }
                MainAction(f)

            // COMPANIES
            case CompanyMenuAction(f) =>
                Ask(f)
                    .each(Seal.companies.%(c => seals.contains(c).not && companyBases(f, c.resource).any && matching(f, c.resource).num >= 5))(c => FoundCompanyAction(f, c))
                    .add(Seal.companies.%(c => seals.get(c).%(_ != f).any && companyBases(f, c.resource).any).flatMap(c => (stockpiles(c.resource).num + 1).to(matching(f, c.resource).num).toList.map(n => BuyoutAction(f, c, n))))
                    .add(Seal.companies.%(c => seals.get(c).has(f)).flatMap(c => matching(f, c.resource).distinct.map(x => DepositAction(f, c, x))))
                    .add(Seal.companies.%(c => seals.get(c).has(f) && stockpiles(c.resource).any).map(c => WithdrawAction(f, c)))
                    .cancel

            case FoundCompanyAction(f, c) =>
                val l = takeMatching(f, c.resource, 5)
                stockpiles += c.resource -> l
                log(f, "founded", c, "with", 5.hl, c.resource, "cards")
                claim(c, f)
                MainAction(f)

            case BuyoutAction(f, c, n) =>
                val e = seals(c)
                e.hand ++= stockpiles(c.resource)
                log(f, "bought out", c, "from", e, "with", n.hl, c.resource, "cards;", e, "took the", stockpiles(c.resource).num.hl, "cards of its stockpile")
                stockpiles += c.resource -> takeMatching(f, c.resource, n)
                claim(c, f)
                MainAction(f)

            case DepositAction(f, c, x) =>
                f.hand = f.hand :- x
                stockpiles += c.resource -> (x +: stockpiles(c.resource))
                log(f, "deposited a card in", c)
                MainAction(f)

            case WithdrawAction(f, c) =>
                val x = stockpiles(c.resource).head
                stockpiles += c.resource -> stockpiles(c.resource).drop(1)
                f.hand :+= x
                log(f, "took a card from", c)
                MainAction(f)

            // DIPLOMACY
            case DiplomacyMenuAction(f) =>
                val others = factions.but(f)
                Ask(f)
                    .add(others.%(e => isAtWar(f, e).not && f.warCards > 0).flatMap(e => $(DeclareWarAction(f, e, false), DeclareWarAction(f, e, true))))
                    .each(others.%(e => isAtWar(f, e) && peaceOffered.has(e).not && declared.has(e).not))(e => PeaceTermsAction(f, e, Terms($, $, $, $), Terms($, $, $, $), "Offer peace to " ~ e.elem))
                    .cancel

            case DeclareWarAction(f, e, boar) =>
                f.warCards -= 1
                wars :+= War(f, e, boar)
                Jests.note(this, "war", e, f, "")
                declared :+= e
                if (boar) {
                    log(f, "declared a", "Boar's War".hl, "on", e)
                    log("The Boar's War penalty isn't in the rulebook's second draft yet, so none is applied")
                }
                else {
                    gentleman :+= (f, e)
                    log(f, "declared a", "Gentleman's War".hl, "on", e)
                }
                MainAction(f)

            // the terms are built a piece at a time: cards each side gives and captured units each side returns
            case PeaceTermsAction(f, e, _, t, _) =>
                Ask(f)
                    .add(OfferPeaceAction(f, e, t))
                    .each(f.hand.diff(t.give).distinct)(c => PeaceTermsAction(f, e, t, t.copy(give = t.give :+ c), "Give " ~ c.elem))
                    .each((t.demand.num < 10).??(Resource.all))(r => PeaceTermsAction(f, e, t, t.copy(demand = t.demand :+ r), "Ask for " ~ r.elem))
                    .each(f.captured.map(_._2).%(_.faction == e).diff(t.release))(u => PeaceTermsAction(f, e, t, t.copy(release = t.release :+ u), "Return " ~ u.elem))
                    .each(e.captured.map(_._2).%(_.faction == f).diff(t.ransom))(u => PeaceTermsAction(f, e, t, t.copy(ransom = t.ransom :+ u), "Ask for the return of " ~ u.elem))
                    .cancel

            case OfferPeaceAction(f, e, t) =>
                peaceOffered :+= e
                log(f, "offered peace to", e, termsElem(f, e, t))
                PeaceAskAction(e, f, t)

            case PeaceAskAction(e, f, t) =>
                val can = t.demand.diff(e.hand).none
                Ask(e).add(AcceptPeaceAction(e, f, t).!(can.not, "you don't have the cards")).add(DeclinePeaceAction(e, f, t))

            case AcceptPeaceAction(e, f, t) =>
                log(e, "accepted peace with", f)

                // the cards change hands at once
                val give = t.give.%(f.hand.has)
                give.foreach { c => f.hand = f.hand :- c ; e.hand :+= c }
                t.demand.foreach { r => e.hand = e.hand :- r ; f.hand :+= r }
                give.foreach(c => this.notify(e, $(DrewCardInfoAction(e, c))))

                // a prisoner exchange returns units to the supply; units returned one way may be placed in their owner's empty colony or port
                val release = t.release.%(u => f.captured.exists(_._2 == u))
                val ransom = t.ransom.%(u => e.captured.exists(_._2 == u))
                f.captured = f.captured.filter(x => release.has(x._2).not)
                e.captured = e.captured.filter(x => ransom.has(x._2).not)
                (release ++ ransom).foreach(u => log(u, "was returned"))

                wars.%(_.between(e, f)).foreach { w => w.declarer.warCards += 1 }
                wars = wars.%(_.between(e, f).not)
                gentleman = gentleman.%!{ case (a, b) => (a == e && b == f) || (a == f && b == e) }

                if (release.any && ransom.any)
                    MainAction(f)
                else
                    PlaceReturnedAction(release ++ ransom, MainAction(f))

            case DeclinePeaceAction(e, f, t) =>
                log(e, "declined peace with", f)
                MainAction(f)

            case PlaceReturnedAction(Nil, then) =>
                then

            case PlaceReturnedAction(u :: rest, then) =>
                val e = u.faction
                val sites = (u.kind == Frigate).?(ports.toList.filter(_._2.owner == e).map(_._1)).|(ownColonies(e)).%(h => on(h).none)
                if (sites.none || factions.has(e).not)
                    PlaceReturnedAction(rest, then)
                else
                    Ask(e).each(sites)(h => PlacePrisonerAction(e, u, h, rest, then)).add(SupplyPrisonerAction(e, u, rest, then))

            case PlacePrisonerAction(e, u, h, rest, then) =>
                pos += u -> h
                fresh += u
                log(u, "was placed in", h)
                PlaceReturnedAction(rest, then)

            case SupplyPrisonerAction(e, u, rest, then) =>
                PlaceReturnedAction(rest, then)

            case SurrenderAction(f) =>
                log(f, "surrendered")
                units(f).foreach(removeFigure)
                embarked = embarked.filter(_._2.faction != f)
                roads = roads.%(_.owner != f)
                bridges = bridges.%(_.owner != f)
                ports = ports.filter(_._2.owner != f)
                forts = forts.filter(_._2.owner != f)
                wars = wars.%(w => w.declarer != f && w.target != f)
                sealsOf(f).foreach { s =>
                    seals -= s
                    s match {
                        case c : Company =>
                            stockpiles(c.resource).foreach(x => discards += x.resource -> (discards(x.resource) :+ x))
                            stockpiles += c.resource -> $
                        case _ =>
                    }
                }
                factions = factions.but(f)
                SurrenderColoniesAction(f, factions)

            // each annexing player's colony replaces the surrendering player's; with too few colonies left, it picks which
            case SurrenderColoniesAction(f, Nil) =>
                colonyOf(f).foreach { h =>
                    colonies -= h
                    forts -= h
                    log(f, "Colony in", h, "was removed")
                }
                if (factions.num == 1)
                    GameOverAction(factions)
                else
                    NextTurnAction(f)

            case SurrenderColoniesAction(f, e :: rest) =>
                val l = colonyOf(f).%(h => annexer(h).has(e))
                val room = Pieces.colonies - colonyOf(e).num
                if (l.none || room <= 0)
                    SurrenderColoniesAction(f, rest)
                else
                if (l.num <= room) {
                    l.foreach { h => colonies += h -> e ; log(e, "took the Colony in", h) ; Jests.note(this, "colony", f, e, h.name) }
                    SurrenderColoniesAction(f, rest)
                }
                else
                    Ask(e).each(l)(h => KeepColonyAction(e, f, h, e :: rest))

            case KeepColonyAction(e, f, h, left) =>
                colonies += h -> e
                log(e, "took the Colony in", h)
                SurrenderColoniesAction(f, left)

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

    def ambushPlayed(f : Faction) {
        discard(f, Ambush)
        log(f, "played", Ambush)
    }
}
