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

// The Outsiders Expansion's Twelve Colonies (slugfacekillah, inspired by lhammer, v2.1; Law of Slug TC and its faction board):
// snakes and turtles who colonize the Woodland with Capitols and Commons, and put a Law to every player's vote each Birdsong.
//
// Readings (the rules leave these open):
// - Turtles: on a path, a turtle is off the map (it is in no clearing, can't be battled or removed, rules nothing); in a clearing it is
//   a warrior that deals no hits (a clearing with only turtles is not defenseless). A turtle moves from a clearing onto an adjacent path
//   or from a path into one of its clearings, ignoring rule, as one move of a Campaign; moving with snakes ("half the distance"), the
//   turtles of the clearing the snakes leave may go onto the path they take. Hard Shelled ignores one hit assigned to the Colonies in a
//   clearing with a turtle, once a battle
// - Tolls: to move along a path with a turtle (by its trail, not a river or the Ferry), another player gives the Colonies a card
//   matching one of its clearings; without one, it can't take that path
// - The Public Trust: ballots are bought only during the Democratic Process, on the buyer's nomination or voting turn, by removing
//   warriors from one clearing (the Colonies two for each ballot); factions with no warriors can't buy. Ballots aren't traded between
//   players (Political Favours need table talk)
// - Nominate: in point order from lowest (ties in turn order), each player places one of the three nomination markers on a target or
//   seconds an unseconded nomination (its own too), or passes; an Election with no seconded nomination fails
// - Voting: two rounds, from the highest score; on its turn a player may buy ballots and give any number of them to any of the options,
//   then is done; one that gave none in the first round can't vote in the second. A Directive passes with more Yea than Nay, an Election
//   goes to the seconded nomination with the most ballots; ties are the Colonies' to decide
// - Laws: Pax Vulpes, Lepus and Mus stop battles in clearings of their suit (the instead-move is not offered); Omnius Exitus moves each
//   player's warriors and pawns to one adjacent clearing of their choice (removed if there is none) and returns its other pieces to the
//   supply, and while it holds nothing can be placed in, moved into or out of, or battled in the clearing; Amplius Actum asks each player
//   once, when it passes; Communia Divitiae deals the cards back at random in turn order; Status Aequalis lets the Colonies pick among
//   tied players; Valorem Historiae's slots are buildings (not ones that can't be removed) and ruins with items; Arcitectus Corvidae
//   lets everyone move ignoring rule (warriors' presence for adjacency changes nothing on these maps); Non Variablis stops bird cards
//   paying for a Colonize, a Recruit and an ambush in another suit's clearing
// - Colonize: the cards spent must match the clearing and the current Law (any card matching the clearing for an "all" Law or with
//   no Law in effect); a Capitol needs a building slot, a Common doesn't
// - Claim: once a turn, 1 point for each Capitol on the map (each one uncovers a flag)

case object Snake extends Warrior {
    override def id = "snake"
    override def name = "Snake"
}

case object Turtle extends Warrior {
    override def id = "turtle"
    override def name = "Turtle"
}

case object Capitol extends Building {
    override def id = "capitol"
    override def name = "Capitol"
}

case object Common extends Token {
    override def id = "common"
    override def name = "Common"
}

// Valorem Historiae's marker: never removed, ruling nothing
case object HistoricalSite extends Building with Tenacious {
    override def id = "historical-site"
    override def name = "Historical Site"
}

case object OnPaths extends SpecialRegion {
    override def name = "On Paths"
}

case object HardShelled extends FactionEffect {
    val name = "Hard Shelled"
}

case object SlowButSteady extends FactionEffect {
    val name = "Slow but Steady"
}

case object Tolls extends FactionEffect {
    val name = "Tolls"
}

case object ShellIgnored extends BattleEffect {
    val name = "Hard Shelled"
}

case object TurtlesNoHits extends BattleEffect {
    val name = "Turtles"
}

case object FerisHit extends BattleEffect {
    val name = "Feris Ratio"
}

case object TollPaid extends HiddenEffect
case object MilitarisUsed extends HiddenEffect
case object AmpliusUsed extends HiddenEffect

abstract class ColonyLaw(val name : String, val suit : |[BaseSuit], val election : Boolean, val image : String, val text : String) extends Record with Elementary {
    def elem = name.styled(TW).styled(xstyles.bold)
    def kind = election.?("Election").|("Directive")
    def suitName = suit./(_.name).|("All")
}

case object MilitarisSubsidium extends ColonyLaw("Militaris Subsidium", Some(Fox), true, "militaris-subsidium", "Elect a player. At the start of their Birdsong, the elected player may place up to 3 warriors in any clearing where they have pieces.")
case object FerisRatio extends ColonyLaw("Feris Ratio", Some(Fox), true, "feris-ratio", "Elect a player. The elected player does an extra hit when attacking in battle.")
case object OmniusExitus extends ColonyLaw("Omnius Exitus", Some(Fox), true, "omnius-exitus", "Elect a clearing on the map. All players must immediately move and/or remove their pieces from the elected clearing, ignoring rule if they must. While in effect, this clearing cannot be used in any way.")
case object PaxVulpes extends ColonyLaw("Pax Vulpes", Some(Fox), false, "pax-vulpes", "Players cannot initiate battles in fox clearings.")
case object AmpliusActum extends ColonyLaw("Amplius Actum", Some(Rabbit), false, "amplius-actum", "All players must make a choice: during their Daylight, they may take an additional move or initiate a battle. Otherwise, they may immediately place 2 warriors in a clearing they currently rule.")
case object CommuniaDivitiae extends ColonyLaw("Communia Divitiae", Some(Rabbit), false, "communia-divitiae", "Remove a random card from each player's hand and place it face down on the table. Shuffle those cards and then deal them out.")
case object PublicoAuxilio extends ColonyLaw("Publico Auxilio", Some(Rabbit), false, "publico-auxilio", "All players immediately draw 2 cards and discard 1 of them.")
case object PaxLepus extends ColonyLaw("Pax Lepus", Some(Rabbit), false, "pax-lepus", "Players cannot initiate battles in rabbit clearings.")
case object DefensioOptimum extends ColonyLaw("Defensio Optimum", Some(Mouse), false, "defensio-optimum", "All players who are defending in battle take the higher roll.")
case object ArtifexContentio extends ColonyLaw("Artifex Contentio", Some(Mouse), false, "artifex-contentio", "Players cannot craft items or improvements. They can, however, use the improvements which they have already crafted.")
case object InsidaeTutela extends ColonyLaw("Insidae Tutela", Some(Mouse), false, "insidae-tutela", "Ambush cards cannot be played against other players.")
case object PaxMus extends ColonyLaw("Pax Mus", Some(Mouse), false, "pax-mus", "Players cannot initiate battles in mouse clearings.")
case object NonVariablis extends ColonyLaw("Non Variablis", None, false, "non-variablis", "Bird cards cannot be played as wild. If they were put into play previously, you may still treat them as if wild.")
case object ArcitectusCorvidae extends ColonyLaw("Arcitectus Corvidae", None, false, "arcitectus-corvidae", "All players can ignore rule while moving.")
case object ValoremHistoriae extends ColonyLaw("Valorem Historiae", None, true, "valorem-historiae", "Elect a slot on the map that has a building or ruin with an item. Remove all pieces from that slot and place the Historical Site marker; it is never removed from the map. A ruin's items become craftable.")
case object StatusAequalis extends ColonyLaw("Status Aequalis", None, false, "status-aequalis", "The player with the fewest victory points gains 2 victory points. The player with the most victory points loses 2 victory points.")

object ColonyLaw {
    val all = $[ColonyLaw](MilitarisSubsidium, FerisRatio, OmniusExitus, PaxVulpes, AmpliusActum, CommuniaDivitiae, PublicoAuxilio, PaxLepus, DefensioOptimum, ArtifexContentio, InsidaeTutela, PaxMus, NonVariablis, ArcitectusCorvidae, ValoremHistoriae, StatusAequalis)

    def pax(l : ColonyLaw) = l @@ {
        case PaxVulpes => Some(Fox)
        case PaxLepus => Some(Rabbit)
        case PaxMus => Some(Mouse)
        case _ => None
    }
}

// a nomination's target: a player, a clearing, or a slot (a building of a player, or a ruin, in a clearing)
case class Nominee(f : |[Faction], c : |[Clearing], p : |[Piece]) extends GameElementary with Record {
    def elem(implicit game : Game) : Elem = (f, c, p) @@ {
        case (Some(f), None, None) => f.elem
        case (None, Some(c), None) => c.elem
        case (Some(f), Some(c), Some(p)) => p.of(f) ~ " in " ~ c.elem
        case (None, Some(c), Some(p)) => "Ruins".hh ~ " in " ~ c.elem
        case _ => "?".txt
    }
}

case class Nomination(target : Nominee, by : Faction, seconded : Boolean)

trait TwelveColonies extends WarriorFaction {
    val clashKey = TW

    val warrior = Snake

    def abilities(options : $[Meta.O]) = $(DaylightCraft, HardShelled, SlowButSteady, Tolls)

    def pieces(options : $[Meta.O]) = Snake *** 20 ++ Turtle *** 5 ++ Capitol *** 6 ++ Common *** 6 ++ HistoricalSite *** 1

    // turtles move only with their own actions
    override val transports : $[$[Transport]] = $($(RuledMove, SnakesMove))

    override def note : Elem = SpacedDash ~ "V" ~ "2.1".hh ~ " by " ~ "slugfacekillah".hl ~ HorizontalBreak ~ "Outsiders Expansion (fan-made)"

    def advertising = Snake.img(this) ~ Turtle.img(this) ~ Capitol.img(this)

    def motto = "Legislate".styled(this)

    override def initialDamagePriorities(options : $[Meta.O]) = $(Snake, Turtle, Common, Capitol)
}

case object TW extends TwelveColonies {
    val name = "Twelve Colonies"
    val short = "TW"
    val style = "tw"
    val priority = "V'''''"
}

case object SnakesMove extends Transport {
    override def allows(f : Faction, o : Region, d : Region, l : $[Movable])(implicit game : Game) = l.has(Turtle).not && allows(f, o, d)
}

trait TWDaylightQuestion extends FactionAction {
    override def self : TwelveColonies

    def question(implicit game : Game) = self.elem ~ SpacedDash ~ Daylight.elem ~ Break ~
        Div(
            1.to(3)./(_ => Image("action-black", styles.action, "")).take(self.acted) ~
            1.to(3)./(_ => Image(self.style + "-action", styles.action)).drop(self.acted),
            styles.margined)
}

// SETUP
case class TWFoundAction(self : TwelveColonies, cap : Clearing, com : Clearing) extends BaseAction("Found the first colonies:", Capitol.of(self), "and", Common.of(self))(Capitol.of(self), "in", cap, Comma, Common.of(self), "in", com)
case class TWLawsShuffledAction(self : TwelveColonies, shuffled : $[ColonyLaw], then : ForcedAction) extends ShuffledAction[ColonyLaw]
case class TWDrawLawsAction(self : TwelveColonies, then : ForcedAction) extends ForcedAction
case class TWKeepLawAction(self : TwelveColonies, l : ColonyLaw, then : ForcedAction) extends BaseAction(self, "keeps one Law for the next Birdsong")(l.elem, "(" ~ l.kind ~ ", " ~ l.suitName ~ ")", Break, l.text)

// BIRDSONG
case class TWRecruitAction(self : TwelveColonies, c : Clearing, l : $[Warrior]) extends BaseAction("Recruit".styled(self), "with a ballot")(l./(_.of(self)).comma, "in", c)
case class TWConveneAction(self : TwelveColonies) extends ForcedAction
case class TWNominateTurnAction(self : TwelveColonies, e : Faction, rest : $[Faction]) extends ForcedAction
case class TWNominateAction(self : Faction, t : TwelveColonies, n : Nominee, rest : $[Faction]) extends BaseAction("Nominate")(n)
case class TWSecondAction(self : Faction, t : TwelveColonies, i : Int, rest : $[Faction]) extends BaseAction("Second the nomination of")(implicit g => t.nominations(i).target.elem)
case class TWNominatePassAction(self : Faction, t : TwelveColonies, rest : $[Faction]) extends BaseAction(None)("Pass")
case class TWNominationsDoneAction(self : TwelveColonies) extends ForcedAction
case class TWVoteTurnAction(self : TwelveColonies, e : Faction, round : Int, rest : $[Faction]) extends ForcedAction
case class TWVoteAction(self : Faction, t : TwelveColonies, k : Int, n : Int, round : Int, rest : $[Faction]) extends BaseAction("Vote")(implicit g => "Give " ~ n.hl ~ " " ~ (n == 1).?("ballot").|("ballots") ~ " to " ~ t.optionElem(k))
case class TWBuyBallotAction(self : Faction, t : TwelveColonies, c : Clearing, round : Int, rest : $[Faction]) extends BaseAction("Buy a ballot from", "the Public Trust".hh, "removing", implicit g => (self == t).?("two warriors").|("a warrior"), "from")(c)
case class TWVoteDoneAction(self : Faction, t : TwelveColonies, round : Int, rest : $[Faction]) extends BaseAction(None)(implicit g => t.votedNow.has(self).?("Done voting").|("Abstain"))
case class TWAdjournAction(self : TwelveColonies) extends ForcedAction
case class TWDecideTieAction(self : TwelveColonies, k : Int) extends BaseAction("Break the tie")(implicit g => self.optionElem(k))
case class TWLawResultAction(self : TwelveColonies, k : Int) extends ForcedAction
case class TWLawPassedAction(self : TwelveColonies, then : ForcedAction) extends ForcedAction

// THE LAWS' EFFECTS
case class TWExitusAction(self : TwelveColonies, c : Clearing, l : $[Faction], then : ForcedAction) extends ForcedAction
case class TWExitusToAction(self : Faction, t : TwelveColonies, c : Clearing, d : Clearing, l : $[Faction], then : ForcedAction) extends BaseAction(OmniusExitus.elem, "moves the pieces out of", c, "to")(d)
case class TWAmpliusAskAction(self : TwelveColonies, l : $[Faction], then : ForcedAction) extends ForcedAction
case class TWAmpliusExtraAction(self : Faction, t : TwelveColonies, l : $[Faction], then : ForcedAction) extends BaseAction(AmpliusActum.elem)("An additional move or battle each Daylight")
case class TWAmpliusPlaceAction(self : Faction, t : TwelveColonies, c : Clearing, l : $[Faction], then : ForcedAction) extends BaseAction(AmpliusActum.elem)("Place 2 warriors in", c)
case class TWCommuniaAction(self : TwelveColonies, l : $[Faction], from : $[Faction], picked : $[DeckCard], then : ForcedAction) extends ForcedAction
case class TWCommuniaPickedAction(self : TwelveColonies, e : Faction, shuffled : $[DeckCard], rest : $[Faction], from : $[Faction], picked : $[DeckCard], then : ForcedAction) extends ShuffledAction[DeckCard]
case class TWCommuniaDealAction(self : TwelveColonies, shuffled : $[DeckCard], to : $[Faction], then : ForcedAction) extends ShuffledAction[DeckCard]
case class TWPublicoAction(self : TwelveColonies, l : $[Faction], then : ForcedAction) extends ForcedAction
case class TWPublicoDiscardAskAction(self : TwelveColonies, e : Faction, l : $[Faction], then : ForcedAction) extends ForcedAction
case class TWPublicoDiscardAction(self : Faction, t : TwelveColonies, d : DeckCard, l : $[Faction], then : ForcedAction) extends BaseAction(PublicoAuxilio.elem, "discard one of the two cards")(d.img) with ViewCard
case class TWAequalisAction(self : TwelveColonies, low : |[Faction], then : ForcedAction) extends ForcedAction
case class TWAequalisPickAction(self : TwelveColonies, e : Faction, low : Boolean, then : ForcedAction) extends BaseAction(StatusAequalis.elem, low.?("gains").|("loses"), 2.vp)(e)
case class TWMilitarisAction(self : Faction, t : TwelveColonies, c : Clearing, n : Int) extends BaseAction(MilitarisSubsidium.elem, "place warriors")(n.hl, "in", c)
case class TWAmpliusDoneAction(self : Faction) extends ForcedAction

// DAYLIGHT
case class TWCampaignMoveMainAction(self : TwelveColonies) extends OptionAction("Campaign".styled(self), "(two moves)", dt.Move) with TWDaylightQuestion with Soft
case class TWCampaignMoveAction(self : TwelveColonies, k : Int) extends ForcedAction
case class TWTurtleStepAction(self : TwelveColonies, o : |[Clearing], a : Clearing, b : Clearing, d : |[Clearing], n : Int, k : Int) extends BaseAction("Move", Turtle.sof(self))(implicit g => { val p = (a, b) ; n.hl ~ " " ~ Turtle.nof(n)(self) ~ " " ~ (o./(o => "from " ~ o.elem(g) ~ " onto the path to " ~ (p._1 == o).?(p._2).|(p._1).elem(g)).|("from the path between " ~ p._1.elem(g) ~ " and " ~ p._2.elem(g) ~ " into " ~ d.get.elem(g))) })
case class TWTurtleFollowAction(self : TwelveColonies, o : Clearing, d : Clearing, then : ForcedAction) extends ForcedAction
case class TWTurtleFollowToAction(self : TwelveColonies, o : Clearing, d : Clearing, n : Int, then : ForcedAction) extends BaseAction(Turtle.sof(self), "follow onto the path from", o, "to", d)(n.hl, Turtle.nof(n)(self))
case class TWCampaignBattleMainAction(self : TwelveColonies, l : $[Clearing]) extends OptionAction("Campaign".styled(self), "(two battles)", dt.Battle) with TWDaylightQuestion with Soft
case class TWCampaignBattleAction(self : TwelveColonies) extends ForcedAction
case class TWColonizeMainAction(self : TwelveColonies, l : $[(Clearing, Piece)]) extends OptionAction("Colonize".styled(self), Capitol.imgd(self), Common.imgd(self)) with TWDaylightQuestion with Soft
case class TWColonizeClearingAction(self : TwelveColonies, c : Clearing, p : Piece, n : Int) extends BaseAction("Colonize".styled(self))(p.of(self), "in", c, "(" ~ n.cards ~ ")") with Soft
case class TWColonizeAction(self : TwelveColonies, c : Clearing, p : Piece, l : $[DeckCard]) extends ForcedAction
case class TWClaimAction(self : TwelveColonies, n : Int) extends OptionAction("Claim".styled(self), n.vp) with TWDaylightQuestion
case class TWDoneAction(self : TwelveColonies) extends ForcedAction

// TOLLS
case class TWTollAction(self : Faction, t : TwelveColonies, d : DeckCard, then : ForcedAction) extends BaseAction("Pay", t, "the toll for the path with a", Turtle.of(t))(d.img) with ViewCard


class TwelveColoniesPlayer(val faction : TwelveColonies)(implicit val game : Game) extends FactionState {
    val onPaths = location(OnPaths, u => u.faction == faction && u.piece == Turtle)

    // the turtles on each path, by its two clearings in board order
    var paths : Map[(Clearing, Clearing), Int] = Map()

    var ballots : Map[Faction, Int] = Map()
    var trust = 0

    var deck : $[ColonyLaw] = $
    var proposals : $[ColonyLaw] = $
    var kept : |[ColonyLaw] = None
    var law : |[ColonyLaw] = None
    var target : |[Nominee] = None

    var nominations : $[Nomination] = $
    var votes : Map[Int, Int] = Map()
    var votedNow : $[Faction] = $
    var abstained : $[Faction] = $

    // the players that chose Amplius Actum's additional move or battle
    var amplius : $[Faction] = $

    var acted = 0
    var claimed = false
    var drafted = false
    var campaigning = false

    def craft = (all(Capitol) ++ all(Common))./(_.asset)

    def ballotsOf(e : Faction) = ballots.getOrElse(e, 0)

    def candidates = nominations.%(_.seconded)

    // -1 Yea, -2 Nay, 0, 1, 2 the seconded nominations A, B and C
    def optionElem(k : Int)(implicit game : Game) : Elem = k @@ {
        case -1 => "Yea".hl
        case -2 => "Nay".hl
        case k => $("A", "B", "C")(k).hl ~ " (" ~ candidates(k).target.elem ~ ")"
    }

    def options = law.none.??(kept.?(_.election).?(candidates.indices.$).|($(-1, -2)))

    def draws = all(Common).num / 2
}


object TwelveColoniesExpansion extends FactionExpansion[TwelveColonies] {
    def colonies(implicit game : Game) = factions.of[TwelveColonies].%(game.states.contains)

    def inEffect(l : ColonyLaw)(implicit game : Game) = colonies.exists(_.law.has(l))

    def elected(l : ColonyLaw)(implicit game : Game) = colonies.%(_.law.has(l))./~(_.target)

    def key(a : Clearing, b : Clearing)(implicit game : Game) = (board.clearings.indexOf(a) <= board.clearings.indexOf(b)).?((a, b)).|((b, a))

    // Omnius Exitus's clearing
    def exitus(implicit game : Game) = elected(OmniusExitus)./~(_.c)

    def usable(c : Clearing)(implicit game : Game) = colonies.none || exitus.has(c).not

    def battleAllowed(c : Clearing)(implicit game : Game) = colonies.none || (usable(c) && colonies./~(_.law)./~(ColonyLaw.pax).forall(s => c.suits.has(s).not))

    def artifex(implicit game : Game) = colonies.any && inEffect(ArtifexContentio)

    def ruleless(implicit game : Game) = colonies.any && inEffect(ArcitectusCorvidae)

    def noWild(implicit game : Game) = colonies.any && inEffect(NonVariablis)

    def tolled(o : Clearing, d : Clearing)(implicit game : Game) = colonies.%(_.paths.getOrElse(key(o, d), 0) > 0)

    // a path another player can take: none of the Colonies' turtles on it, or a card to pay the toll
    def passable(f : Faction, o : Clearing, d : Clearing)(implicit game : Game) = colonies.none || (usable(o) && usable(d) && (
        f.is[TwelveColonies] || f.is[Hireling] || tolled(o, d).none || f.hand.exists(x => x.matches(o.cost) || x.matches(d.cost))))

    def lawMatch(f : TwelveColonies, d : DeckCard, c : Clearing)(implicit game : Game) = {
        val wild = d.suit == Bird && noWild
        wild.not && d.matches(c.cost) && f.law./~(_.suit).forall(s => d.matches(s))
    }

    def cost(n : Int) = n / 2 + 1

    def order(implicit game : Game) = factions.sortBy(_.vp)

    def targets(f : TwelveColonies, l : ColonyLaw)(implicit game : Game) : $[Nominee] = l match {
        case FerisRatio | MilitarisSubsidium => factions./(e => Nominee(Some(e), None, None))
        case OmniusExitus => clearings./(c => Nominee(None, Some(c), None))
        case ValoremHistoriae => clearings./~(c => factions./~(e => e.at(c).of[Building].%(_.is[Tenacious].not).distinct./(p => Nominee(Some(e), Some(c), Some(p))))) ++
            game.ruins.keys.$.%(c => game.ruins(c).items.any)./(c => Nominee(None, Some(c), Some(Ruins(game.ruins(c).items))))
        case _ => $
    }

    def colonizable(f : TwelveColonies)(implicit game : Game) = clearings.%(f.canPlace).%(c => f.at(c).has(Capitol).not && f.at(c).has(Common).not)./~(c =>
        (f.rules(c) && f.canBuild(c) && f.pool(Capitol)).$((c, Capitol : Piece)) ++
        (f.at(c).of[Warrior].any && f.pool(Common)).$((c, Common : Piece)))

    def colonizeCost(f : TwelveColonies, p : Piece)(implicit game : Game) = cost(f.all(p).num)

    def turtleSteps(f : TwelveColonies, k : Int)(implicit game : Game) : $[TWTurtleStepAction] =
        clearings.%(c => f.at(c).has(Turtle))./~(c => game.connected(c).%(usable).%(x => usable(c))./~(x => $(f.at(c).count(Turtle), 1).distinct./(n => TWTurtleStepAction(f, Some(c), key(c, x)._1, key(c, x)._2, None, n, k)))) ++
        f.paths.toList.filter(_._2 > 0).flatMap { case (p : (Clearing, Clearing), m : Int) => $(p._1, p._2).%(usable).%(f.canPlace)./~(d => $(m, 1).distinct./(n => TWTurtleStepAction(f, None, p._1, p._2, Some(d), n, k))) }

    def recruits(f : TwelveColonies)(implicit game : Game) : $[TWRecruitAction] = (f.ballotsOf(f) > 0).??(
        clearings.%(f.canPlace).%(c => f.at(c).has(Capitol))./~(c => $[Warrior](Snake, Turtle).%(f.pool)./(w => TWRecruitAction(f, c, $(w)))) ++
        clearings.%(f.canPlace).%(c => f.at(c).has(Common))./~(c => $[$[Warrior]]($(Snake, Snake), $(Snake, Turtle), $(Turtle, Turtle)).%(l => f.pooled(Snake) >= l.count(Snake) && f.pooled(Turtle) >= l.count(Turtle))./(l => TWRecruitAction(f, c, l))))

    // where a player can buy a ballot by removing warriors
    def buyFrom(t : TwelveColonies, e : Faction)(implicit game : Game) = (t.trust > 0).??(clearings.%(c => e.at(c).of[Warrior].num >= (e == t).?(2).|(1)))

    def perform(action : Action, soft : Void)(implicit game : Game) = action @@ {
        // SETUP
        case CreatePlayerAction(f : TwelveColonies) =>
            game.states += f -> new TwelveColoniesPlayer(f)

            FactionInitAction(f)

        case FactionSetupAction(f : TwelveColonies) =>
            val pairs = clearings.%(f.canPlace)./~(a => game.connected(a).%(f.canPlace)./(b => (a, b)))
            val n = pairs./{ case (a, b) => factions./(e => e.at(a).num + e.at(b).num).sum }.minOr(0)

            Ask(f).group(f, "founds the first colonies in two adjacent clearings with the fewest pieces")
                .each(pairs.%{ case (a, b) => factions./(e => e.at(a).num + e.at(b).num).sum == n }) { case (a, b) => TWFoundAction(f, a, b) }

        case TWFoundAction(f, a, b) =>
            game.highlights :+= PlaceHighlight($(a, b))

            f.reserve --> $(Snake, Snake, Turtle, Capitol) --> a
            f.reserve --> $(Snake, Snake, Turtle, Common) --> b

            f.log("founded", Capitol.of(f), "in", a, "and", Common.of(f), "in", b, "with two", Snake.sof(f), "and a", Turtle.of(f), "in each")

            f.ballots = factions./(e => e -> (e == f).?(3).|(1)).toMap
            f.trust = 12 - f.ballots.values.sum

            Shuffle[ColonyLaw](ColonyLaw.all, TWLawsShuffledAction(f, _, TWDrawLawsAction(f, SetupFactionsAction)))

        case TWLawsShuffledAction(f, l, then) =>
            f.deck = l

            then

        case TWDrawLawsAction(f, then) =>
            f.proposals = f.deck.take(3)
            f.deck = f.deck.drop(3)

            if (f.proposals.none)
                then
            else {
                f.log("drew", f.proposals.num.hl, "Laws")

                Ask(f).each(f.proposals)(l => TWKeepLawAction(f, l, then))
            }

        case TWKeepLawAction(f, l, then) =>
            f.kept = Some(l)

            f.log("kept a Law")

            val rest = f.proposals.but(l)

            f.proposals = $

            Shuffle[ColonyLaw](f.deck ++ rest, TWLawsShuffledAction(f, _, then))

        // HARD SHELLED: turtles deal no hits, and the Colonies ignore one hit where a turtle is
        case BattleAdjustRollAction(b, f, o, fs, os, fr, or) if $(f, o).of[TwelveColonies].exists(x => x.used.has(TurtlesNoHits).not && x.at(b.clearing).has(Turtle)) =>
            val l = $(f, o).of[TwelveColonies].%(_.at(b.clearing).has(Turtle))

            l.foreach(_.used :+= TurtlesNoHits)

            // a clearing with turtles is not defenseless
            l.foreach { x =>
                if (x == o && os - x.at(b.clearing).count(Turtle) <= 0)
                    x.used :+= Defenseless
            }

            def turtles(x : Faction) = x.as[TwelveColonies]./(_.at(b.clearing).count(Turtle)).|(0)

            BattleAdjustRollAction(b, f, o, max(0, fs - turtles(f)), max(0, os - turtles(o)), fr, or)

        case BattleAssignHitsAction(f : TwelveColonies, b, n, s, then) if n + s > 0 && f.used.has(ShellIgnored).not && f.at(b.clearing).has(Turtle) =>
            f.used :+= ShellIgnored

            f.log("ignored a hit with", HardShelled.of(f))

            BattleAssignHitsAction(f, b, n - (n > 0).??(1), s - (n == 0).??(1), then)

        // DEFENSIO OPTIMUM
        case BattleAdjustRollAction(b, f, o, fs, os, fr, or) if fr > or && inEffect(DefensioOptimum) =>
            BattleAdjustRollAction(b, f, o, fs, os, or, fr)

        // FERIS RATIO
        case BattleBonusAction(b, f, o, fs, os, fr, or, fh, oh, fe, oe) if elected(FerisRatio).exists(_.f.has(f)) && f.used.has(FerisHit).not =>
            f.used :+= FerisHit

            f.log("dealt an extra", 1.hit, "with", FerisRatio.elem)

            BattleBonusAction(b, f, o, fs, os, fr, or, fh, oh, fe + 1, oe)

        // INSIDAE TUTELA
        case BattleAskAmbushAction(b, then) if inEffect(InsidaeTutela) =>
            then

        // MILITARIS SUBSIDIUM
        case BirdsongNAction(10, e) if elected(MilitarisSubsidium).exists(_.f.has(e)) && e.used.has(MilitarisUsed).not =>
            e.used :+= MilitarisUsed

            e.as[WarriorFaction] match {
                case Some(w) if w.pool(w.warrior) =>
                    val l = clearings.%(c => e.at(c).any).%(e.canPlace)

                    Ask(e).group(MilitarisSubsidium.elem)
                        .some(l)(c => 1.to(min(3, w.pooled(w.warrior))).reverse./(n => TWMilitarisAction(e, colonies.head, c, n)))
                        .skip(BirdsongNAction(10, e))
                case _ =>
                    BirdsongNAction(10, e)
            }

        case TWMilitarisAction(e, t, c, n) =>
            val w = e.as[WarriorFaction].get

            e.reserve --> n.times(w.warrior) --> c

            e.log("placed", w.warrior.sof(e), "(" ~ n.hl ~ ")", "in", c, "with", MilitarisSubsidium.elem)

            BirdsongNAction(10, e)

        // AMPLIUS ACTUM
        case DaylightNAction(0, e) if colonies.exists(_.amplius.has(e)) && inEffect(AmpliusActum) && e.used.has(AmpliusUsed).not =>
            val q = TWAmpliusDoneAction(e)

            Ask(e).group(AmpliusActum.elem, "an additional move or battle")
                .add(MoveInitAction(e, e, $, NoMessage, e.moveFrom, e.movable, $(CancelAction), q).as("Move").!(e.moveFrom.none))
                .add(BattleInitAction(e, e, NoMessage, clearings.%(e.canAttackIn), $(CancelAction), q).as("Battle").!(clearings.%(e.canAttackIn).none))
                .skip(q)

        case TWAmpliusDoneAction(e) =>
            e.used :+= AmpliusUsed

            DaylightNAction(0, e)

        // TOLLS
        case MoveListAction(self, f, t, m, o : Clearing, d : Clearing, l, then) if t.has(Roads) && f.is[TwelveColonies].not && f.is[Hireling].not && f.used.has(TollPaid).not && tolled(o, d).any =>
            val x = tolled(o, d).head

            Ask(f).group(Tolls.of(x), "for the path from", o, "to", d)
                .each(f.hand.%(c => c.matches(o.cost) || c.matches(d.cost)))(c => TWTollAction(f, x, c, ForceAction(MoveListAction(self, f, t, m, o, d, l, then))))

        case TWTollAction(f, x, c, then) =>
            f.hand --> c --> x.hand

            f.used :+= TollPaid

            f.log("paid", x, "a toll with", c)

            then

        case MoveFinishedAction(f, o, d, then) if f.used.has(TollPaid) =>
            f.used :-= TollPaid

            MoveFinishedAction(f, o, d, then)

        // turtles follow the snakes onto the path
        case MoveFinishedAction(f : TwelveColonies, o : Clearing, d : Clearing, then) if f.campaigning && f.at(o).has(Turtle) && game.connected(o).has(d) && then.is[TWTurtleFollowAction].not =>
            MoveFinishedAction(f, o, d, TWTurtleFollowAction(f, o, d, then))

        case TWTurtleFollowAction(f, o, d, then) =>
            Ask(f).group(Turtle.sof(f), "may follow onto the path from", o, "to", d)
                .each(1.to(f.at(o).count(Turtle)).$.reverse)(n => TWTurtleFollowToAction(f, o, d, n, then))
                .skip(then)

        case TWTurtleFollowToAction(f, o, d, n, then) =>
            f.from(o) --> n.times(Turtle) --> f.onPaths

            f.paths += key(o, d) -> (f.paths.getOrElse(key(o, d), 0) + n)

            f.log("moved", n.of(Turtle.name).styled(f), "onto the path from", o, "to", d)

            then

        // BIRDSONG
        case BirdsongNAction(20, f : TwelveColonies) =>
            soft()

            val l = recruits(f)

            if (l.none)
                NoAsk(f)(Next)
            else
                Ask(f).group("Recruit".styled(f), "(" ~ f.ballotsOf(f).hl ~ " ballots)").add(l).done(Next).birdsong(f)

        case TWRecruitAction(f, c, l) =>
            game.highlights :+= PlaceHighlight($(c))

            f.ballots += f -> (f.ballotsOf(f) - 1)
            f.trust += 1

            f.reserve --> l --> c

            f.log("placed a ballot in", "the Public Trust".hh, "and recruited", l./(_.of(f)).comma, "in", c)

            Repeat

        case BirdsongNAction(30, f : TwelveColonies) =>
            if (f.kept.any)
                TWConveneAction(f)
            else
                Next

        case TWConveneAction(f) =>
            val l = f.kept.get

            f.log("put", l.elem, "(" ~ l.kind ~ ")", "to the vote:", l.text.txt)

            f.nominations = $
            f.votes = Map()
            f.votedNow = $
            f.abstained = $

            if (l.election)
                TWNominateTurnAction(f, order.head, order.drop(1))
            else
                TWVoteTurnAction(f, order.reverse.head, 1, order.reverse.drop(1))

        // NOMINATE
        case TWNominateTurnAction(f, e, rest) =>
            val l = f.kept.get
            val tt = (f.nominations.num < 3).??(targets(f, l).%(n => f.nominations.exists(_.target == n).not))

            Ask(e).group("Nominate".styled(f), "for", l.elem)
                .each(tt)(n => TWNominateAction(e, f, n, rest))
                .each(f.nominations.indices.$.%(i => f.nominations(i).seconded.not))(i => TWSecondAction(e, f, i, rest))
                .add(TWNominatePassAction(e, f, rest))

        case TWNominateAction(e, f, n, rest) =>
            f.nominations :+= Nomination(n, e, false)

            e.log("nominated", n)

            rest.starting./(TWNominateTurnAction(f, _, rest.drop(1))).|(TWNominationsDoneAction(f))

        case TWSecondAction(e, f, i, rest) =>
            f.nominations = f.nominations.updated(i, f.nominations(i).copy(seconded = true))

            e.log("seconded the nomination of", f.nominations(i).target)

            rest.starting./(TWNominateTurnAction(f, _, rest.drop(1))).|(TWNominationsDoneAction(f))

        case TWNominatePassAction(e, f, rest) =>
            rest.starting./(TWNominateTurnAction(f, _, rest.drop(1))).|(TWNominationsDoneAction(f))

        case TWNominationsDoneAction(f) =>
            f.nominations = f.nominations.%(_.seconded)

            if (f.nominations.none) {
                f.log(f.kept.get.elem, "had no seconded nomination and failed")

                f.kept = None

                Next
            }
            else {
                f.log("The candidates are", f.nominations.indices./(i => f.optionElem(i)).comma)

                TWVoteTurnAction(f, order.reverse.head, 1, order.reverse.drop(1))
            }

        // VOTING
        case TWVoteTurnAction(f, e, round, rest) if round == 2 && f.abstained.has(e) =>
            rest.starting./(TWVoteTurnAction(f, _, round, rest.drop(1))).|((round == 1).?(TWVoteTurnAction(f, order.reverse.head, 2, order.reverse.drop(1))).|(TWAdjournAction(f)))

        case TWVoteTurnAction(f, e, round, rest) =>
            val n = f.ballotsOf(e)
            val oo = f.kept.get.election.?(f.candidates.indices.$).|($(-1, -2))

            Ask(e).group("Vote".styled(f), "on", f.kept.get.elem, "(round " ~ round.hl ~ ", " ~ n.hl ~ " " ~ (n == 1).?("ballot").|("ballots") ~ ")")
                .some(oo)(k => (n > 0).??($(1, n).distinct./(x => TWVoteAction(e, f, k, x, round, rest))))
                .each(buyFrom(f, e))(c => TWBuyBallotAction(e, f, c, round, rest))
                .add(TWVoteDoneAction(e, f, round, rest))

        case TWVoteAction(e, f, k, n, round, rest) =>
            f.ballots += e -> (f.ballotsOf(e) - n)
            f.votes += k -> (f.votes.getOrElse(k, 0) + n)
            f.votedNow :+= e

            e.log("gave", n.hl, (n == 1).?("ballot").|("ballots"), "to", f.optionElem(k))

            TWVoteTurnAction(f, e, round, rest)

        case TWBuyBallotAction(e, f, c, round, rest) =>
            val w = e.at(c).of[Warrior].sortBy(p => e.as[WarriorFaction].?(_.warrior == p).?(0).|(1)).take((e == f).?(2).|(1))

            e.from(c) --> w --> e.reserve

            f.trust -= 1
            f.ballots += e -> (f.ballotsOf(e) + 1)

            e.log("bought a ballot from", "the Public Trust".hh, "removing", w./(_.of(e)).comma, "from", c)

            TWVoteTurnAction(f, e, round, rest)

        case TWVoteDoneAction(e, f, round, rest) =>
            if (f.votedNow.has(e).not) {
                e.log("abstained")

                if (round == 1)
                    f.abstained :+= e
            }

            f.votedNow = f.votedNow.but(e)

            rest.starting./(TWVoteTurnAction(f, _, round, rest.drop(1))).|((round == 1).?(TWVoteTurnAction(f, order.reverse.head, 2, order.reverse.drop(1))).|(TWAdjournAction(f)))

        // ADJOURN
        case TWAdjournAction(f) =>
            val total = f.votes.values.sum
            val mine = (total + 1) / 2

            f.ballots += f -> (f.ballotsOf(f) + mine)
            f.trust += total - mine

            val oo = f.kept.get.election.?(f.candidates.indices.$).|($(-1, -2))
            val top = oo./(f.votes.getOrElse(_, 0)).max
            val tied = oo.%(k => f.votes.getOrElse(k, 0) == top)

            f.log("counted", total.hl, "ballots:", oo./(k => f.optionElem(k) ~ " " ~ f.votes.getOrElse(k, 0).hl).comma)

            if (tied.num > 1)
                Ask(f).group(f, "breaks the tie").each(tied)(k => TWDecideTieAction(f, k))
            else
                TWLawResultAction(f, tied.head)

        case TWDecideTieAction(f, k) =>
            f.log("broke the tie for", f.optionElem(k))

            TWLawResultAction(f, k)

        case TWLawResultAction(f, k) =>
            val l = f.kept.get

            f.kept = None
            f.votedNow = $

            if (k == -2) {
                f.log(l.elem, "failed and was removed from the game")

                Next
            }
            else {
                f.law.foreach(o => f.log(o.elem, "was replaced and removed from the game"))

                f.law = Some(l)
                f.target = (k >= 0).?(f.candidates(k).target)
                f.amplius = $

                f.log(l.elem, "passed", f.target./(t => "electing ".txt ~ t.elem))

                TWLawPassedAction(f, Next)
            }

        // THE LAWS PASSED
        case TWLawPassedAction(f, then) =>
            f.law.get match {
                case OmniusExitus =>
                    val c = f.target.get.c.get

                    TWExitusAction(f, c, factions.%(_.at(c).any), then)

                case AmpliusActum =>
                    TWAmpliusAskAction(f, factions, then)

                case CommuniaDivitiae =>
                    TWCommuniaAction(f, factions.%(_.hand.any), $, $, then)

                case PublicoAuxilio =>
                    TWPublicoAction(f, factions, then)

                case StatusAequalis =>
                    TWAequalisAction(f, None, then)

                case ValoremHistoriae =>
                    val t = f.target.get
                    val c = t.c.get

                    (t.f, t.p) @@ {
                        case (Some(e), Some(p)) if e.at(c).has(p) =>
                            e.from(c) --> p --> e.reserve

                            f.log("removed", p.of(e), "from", c)
                        case (None, Some(Ruins(items))) if game.ruins.contains(c) =>
                            game.uncrafted ++= game.ruins(c).items

                            game.ruins -= c

                            f.log("removed the ruins of", c, "and its items became craftable")
                        case _ =>
                    }

                    if (f.pool(HistoricalSite) && game.freeSlots(c) > 0) {
                        f.reserve --> HistoricalSite --> c

                        f.log("placed", HistoricalSite.of(f), "in", c)
                    }

                    then

                case _ =>
                    then
            }

        case TWExitusAction(f, c, l, then) =>
            l.starting match {
                case None =>
                    then
                case Some(e) =>
                    val rest = e.from(c).$.%(_.piece.is[Movable].not).%(_.piece.is[Tenacious].not)

                    rest.foreach(_ --> game.pool)

                    if (rest.any)
                        e.log("returned", rest./(_.elem).comma, "from", c, "to the supply")

                    val dd = game.connected(c).%(usable).%(d => d != c && e.canPlace(d))

                    if (e.from(c).$.%(_.piece.is[Movable]).none)
                        TWExitusAction(f, c, l.drop(1), then)
                    else
                    if (dd.none) {
                        val m = e.from(c).$.%(_.piece.is[Movable])

                        m.foreach(_ --> game.pool)

                        e.log("removed", m./(_.elem).comma, "from", c)

                        TWExitusAction(f, c, l.drop(1), then)
                    }
                    else
                        Ask(e).group(OmniusExitus.elem, "moves the pieces out of", c).each(dd)(d => TWExitusToAction(e, f, c, d, l.drop(1), then))
            }

        case TWExitusToAction(e, f, c, d, l, then) =>
            val m = e.from(c).$.%(_.piece.is[Movable])

            m.foreach(_ --> d)

            e.log("moved", m./(_.elem).comma, "from", c, "to", d)

            TWExitusAction(f, c, l, then)

        case TWAmpliusAskAction(f, l, then) =>
            l.starting match {
                case None =>
                    then
                case Some(e) =>
                    val place = e.as[WarriorFaction].?(w => w.pooled(w.warrior) >= 2).??(clearings.%(e.rules).%(e.canPlace))

                    Ask(e).group(AmpliusActum.elem)
                        .add(TWAmpliusExtraAction(e, f, l.drop(1), then))
                        .each(place)(c => TWAmpliusPlaceAction(e, f, c, l.drop(1), then))
            }

        case TWAmpliusExtraAction(e, f, l, then) =>
            f.amplius :+= e

            e.log("chose an additional move or battle each Daylight")

            TWAmpliusAskAction(f, l, then)

        case TWAmpliusPlaceAction(e, f, c, l, then) =>
            val w = e.as[WarriorFaction].get

            e.reserve --> 2.times(w.warrior) --> c

            e.log("placed", 2.of(w.warrior.name).styled(e), "in", c)

            TWAmpliusAskAction(f, l, then)

        case TWCommuniaAction(f, l, from, picked, then) =>
            l.starting match {
                case Some(e) =>
                    Shuffle[DeckCard](e.hand.get, TWCommuniaPickedAction(f, e, _, l.drop(1), from, picked, then))
                case None =>
                    if (picked.none)
                        then
                    else
                        Shuffle[DeckCard](picked, TWCommuniaDealAction(f, _, from, then))
            }

        case TWCommuniaPickedAction(f, e, l, rest, from, picked, then) =>
            e.hand --> l.head --> f.drawn

            TWCommuniaAction(f, rest, from :+ e, picked :+ l.head, then)

        case TWCommuniaDealAction(f, l, to, then) =>
            l.lazyZip(to).foreach { (d, e) =>
                f.drawn --> d --> e.hand
            }

            f.log("dealt out a random card of each player's hand with", CommuniaDivitiae.elem)

            then

        case TWPublicoAction(f, l, then) =>
            l.starting match {
                case Some(e) =>
                    DrawCardsAction(e, 2, NoMessage, AddCardsAction(e, TWPublicoDiscardAskAction(f, e, l.drop(1), then)))
                case None =>
                    then
            }

        case TWPublicoDiscardAskAction(f, e, l, then) =>
            if (e.hand.none)
                TWPublicoAction(f, l, then)
            else
                Ask(e).group(PublicoAuxilio.elem, "discards a card").each(e.hand)(d => TWPublicoDiscardAction(e, f, d, l, then))

        case TWPublicoDiscardAction(e, f, d, l, then) =>
            e.hand --> d --> discard(e)

            TWPublicoAction(f, l, then)

        case TWAequalisAction(f, None, then) =>
            val low = factions./(_.vp).min
            val ll = factions.%(_.vp == low)

            if (ll.num > 1)
                Ask(f).group(StatusAequalis.elem).each(ll)(e => TWAequalisPickAction(f, e, true, then))
            else
                ForceAction(TWAequalisPickAction(f, ll.head, true, then))

        case TWAequalisPickAction(f, e, true, then) =>
            e.oscore(2)("with", StatusAequalis.elem)

            val high = factions./(_.vp).max
            val hh = factions.%(_.vp == high)

            if (hh.num > 1)
                Ask(f).group(StatusAequalis.elem).each(hh)(e => TWAequalisPickAction(f, e, false, then))
            else
                ForceAction(TWAequalisPickAction(f, hh.head, false, then))

        case TWAequalisPickAction(f, e, false, then) =>
            e.oscore(-2)("with", StatusAequalis.elem)

            then

        // DAYLIGHT
        case DaylightNAction(20, f : TwelveColonies) =>
            XCraftMainAction(f)

        case DaylightNAction(40, f : TwelveColonies) =>
            implicit val ask = builder

            if (f.acted < 3) {
                val mm = f.moveFrom.any || turtleSteps(f, 0).any
                + TWCampaignMoveMainAction(f).!(mm.not)

                val bb = clearings.%(f.canAttackIn)
                + TWCampaignBattleMainAction(f, bb).!(bb.none)

                val cc = colonizable(f)
                + TWColonizeMainAction(f, cc).!(cc.none)

                + TWClaimAction(f, f.all(Capitol).num).!(f.claimed, "once per turn")
            }

            + EndTurnSoftAction(f, "Actions", ForfeitActions(3 - f.acted))

            ask(f).daylight(f)

        case TWCampaignMoveMainAction(f) =>
            Ask(f).group("Campaign".styled(f), "move 1 of 2")
                .when(f.moveFrom.any)(MoveInitAction(f, f, $, NoMessage, f.moveFrom, f.movable, $(CancelAction), TWCampaignMoveAction(f, 1)).as("Move", Snake.sof(f)))
                .add(turtleSteps(f, 0))
                .cancel

        case TWCampaignMoveAction(f, k) =>
            f.campaigning = false

            if (k >= 2 || (f.moveFrom.none && turtleSteps(f, k).none))
                TWDoneAction(f)
            else {
                f.campaigning = true

                Ask(f).group("Campaign".styled(f), "move 2 of 2")
                    .when(f.moveFrom.any)(MoveInitAction(f, f, $, NoMessage, f.moveFrom, f.movable, $(CancelAction), TWCampaignMoveAction(f, 2)).as("Move", Snake.sof(f)))
                    .add(turtleSteps(f, k))
                    .done(TWDoneAction(f))
            }

        case TWTurtleStepAction(f, o, a, b, d, n, k) =>
            val p = (a, b)

            o @@ {
                case Some(o) =>
                    f.from(o) --> n.times(Turtle) --> f.onPaths

                    f.paths += p -> (f.paths.getOrElse(p, 0) + n)

                    f.log("moved", n.of(Turtle.name).styled(f), "from", o, "onto the path to", (p._1 == o).?(p._2).|(p._1))
                case None =>
                    f.onPaths --> n.times(Turtle) --> d.get

                    f.paths += p -> (f.paths(p) - n)

                    f.log("moved", n.of(Turtle.name).styled(f), "from the path into", d.get)
            }

            TWCampaignMoveAction(f, k + 1)

        case TWCampaignBattleMainAction(f, l) =>
            BattleInitAction(f, f, NoMessage, l, $(CancelAction), TWCampaignBattleAction(f))

        case TWCampaignBattleAction(f) =>
            val l = clearings.%(f.canAttackIn)

            if (l.none)
                TWDoneAction(f)
            else
                BattleInitAction(f, f, NoMessage, l, $(TWDoneAction(f).as("Skip the second battle")), TWDoneAction(f))

        case TWColonizeMainAction(f, l) =>
            Ask(f).each(l) { case (c, p) => TWColonizeClearingAction(f, c, p, colonizeCost(f, p)) }.cancel

        case TWColonizeClearingAction(f, c, p, n) =>
            XXSelectObjectsAction(f, f.hand)
                .withGroup("Colonize".styled(f) ~ " " ~ p.of(f) ~ " in " ~ c.elem ~ ", spending " ~ n.cards ~ " matching " ~ c.elem ~ f.law./~(_.suit)./(s => " and " ~ s.elem).|(Empty))
                .withRule(_.num(n).all(_.forall(d => lawMatch(f, d, c))))
                .withThen(l => TWColonizeAction(f, c, p, l))(l => "Spend".hl ~ l./(" " ~ _.elem))
                .withExtra($(NoHand, CancelAction))

        case TWColonizeAction(f, c, p, l) =>
            game.highlights :+= PlaceHighlight($(c))

            f.hand --> l --> discard.quiet

            f.reserve --> p --> c

            f.log("colonized", c, "with", p.of(f), "spending", l./(_.elem).comma)

            if (p == Common) {
                val k = f.all(Common).num

                if (k == 3)
                    f.oscore(2)("colonizing")
                if (k == 5)
                    f.oscore(3)("colonizing")

                // Starting Over
                if (k == 1) {
                    val w = $[Warrior](Snake, Snake, Turtle, Turtle).%(f.pool).take(2)
                    f.reserve --> w.take(2) --> c
                    f.log("placed", w.take(2)./(_.of(f)).comma, "with the first", Common.of(f))
                }
            }

            TWDoneAction(f)

        case TWClaimAction(f, n) =>
            f.claimed = true

            f.oscore(n)("claiming", n.hl, Capitol.sof(f))

            TWDoneAction(f)

        case TWDoneAction(f) =>
            f.campaigning = false
            f.acted += 1

            Repeat

        // EVENING
        case EveningNAction(20, f : TwelveColonies) =>
            if (f.trust > 0) {
                f.trust -= 1
                f.ballots += f -> (f.ballotsOf(f) + 1)
            }

            order.but(f).foreach { e =>
                if (f.trust > 0) {
                    f.trust -= 1
                    f.ballots += e -> (f.ballotsOf(e) + 1)
                }
            }

            f.log("distributed ballots from", "the Public Trust".hh)

            Next

        case NightStartAction(f : TwelveColonies) =>
            EveningDrawAction(f, 1 + f.draws)

        case EveningAfterHandLimitAction(f : TwelveColonies) if f.drafted.not =>
            f.drafted = true

            TWDrawLawsAction(f, EveningAfterHandLimitAction(f))

        case FactionCleanUpAction(f : TwelveColonies) =>
            f.acted = 0
            f.claimed = false
            f.drafted = false
            f.campaigning = false

            CleanUpAction(f)

        case _ => UnknownContinue
    }

}
