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

// The Doomed Expansion's Turnlings (slugfacekillah, Law of Doom D8): eight double-sided hireling cards, each side its own hireling as
// the official ones are, and the turnling rule (the toggle "Turn Over Hirelings"): whoever takes a hireling marker turns the hireling
// over to another player, who rolls for its control.
//
// Readings (the cards leave these open):
// - Turn Over Hirelings: the hirer gives the hireling to another player, never keeps it ("you mean I have to give it to one of you?");
//   it applies to every hireling in the game, the official ones too, as D8.0 replaces the gaining rule (H.1.1)
// - Infected and the Golden Knight aren't drawn with the Dark Deck (D8.0); the Infected die is the Hundreds' suit die, as in the Dark
//   Deck; the Infected move one into each clearing of the rolled suit from an adjacent clearing (from one of that suit only when it
//   has two or more), the controller picking from where; a "faction warrior" is a warrior of a faction, not of a hireling
// - Infectious Spirits: each clearing of the rolled suit the controller moves into on its turn offers a warrior (the Vagabond
//   refreshes an item), once per clearing
// - Golden Knight: its hit is taken by an enemy of the controller moving into its clearing, with the controller as the attacker
// - Golden Pig: the cards not kept stay in the deck, which is shuffled
// - Rouge Agents: the controller's rule counts the Cardinals, as with every hireling, and once in Daylight it may move and then battle
//   with them (the card's "treat them as your own warriors"; other uses as one's own warriors aren't done)
// - Cardinal Carts: a player moving warriors or a pawn out of a clearing with a Caravan into one without may take it along; the
//   controller draws a card each time a Caravan comes into a clearing it rules
// - Union Members: the Flame moves to a forest whose every adjacent clearing has a Doomsayer at the start of Birdsong and from the
//   controller's Daylight; burning a forest removes the enemy warriors there (the Vagabond can't be removed)
// - Corporate Overreach: the Demand die is a three-sided die (Crusade!, Place Token or Building!, Craft!); Place Token or Building!
//   places a building of the controller's faction in a clearing it rules with a free slot, or a token in a clearing with its pieces,
//   from its supply; one it can't take costs one more control marker at the end of the turn
// - Converging Coup: three Vultures in each of a fox, a mouse and a rabbit clearing (the rules; the card prints two)
// - Bottom Feeder: the discard pile is still shuffled when the deck runs out (the "no shuffling" rule isn't done); each draw of
//   the controller may take its first card from the top of the discard pile
// - Buzzling Buggers: a player moving out of a clearing with Bugs takes one Bug along; "non-edge" clearings are the board's inner ones
// - Miracle Cure-All: a Cure-All can copy an enemy's crafted card (an enemy with pieces in a clearing with the controller's) for the rest
//   of the turn, as the Dark Deck's Doppleganger does, or return a crafted item of a faction other than the Vagabond to the supply;
//   discarded on the controller's own turn
// - Gristly Grunts: an enemy of the controller may spend a card matching a clearing with its pieces and Grunts, once on its turn, to move
//   the Grunts there (by its own rule) and then battle with them; "do so a second time" repeats the same choice
// - Fine Swine: a hit on a Swine moves it to an adjacent clearing the hitting player picks; nothing removes them

trait Turnling extends Hireling {
    override def abilities(options : $[Meta.O]) : $[Effect] = $
}

class TurnlingState(val faction : Turnling)(implicit val game : Game) extends HirelingState {
    board.forests.foreach(r => location(r))

    var knight : |[Clearing] = None
    var caravans : $[Clearing] = $
    var flame : |[Forest] = None
    var suit : |[BaseSuit] = None
    var spirited : $[Clearing] = $
    var cures : Int = 0
    var drawing : Boolean = false
    var owed : Boolean = false
    var user : |[Faction] = None
}

// Whisperers
case object Whisperer extends Warrior

case object MidnightMurderers extends Turnling with WarriorFaction {
    val name = "Midnight Murderers"
    val short = "TMM"
    val style = "tmm"

    val demoted = false
    val setup = true

    val clashKey = MidnightMurderers

    val warrior = Whisperer

    override val transports : $[$[Transport]] = $($())

    def pieces(options : $[Meta.O]) = Whisperer *** 10
}

case object ForestConnections extends Turnling {
    val name = "Forest Connections"
    val short = "TFC"
    val style = "tfc"

    val demoted = true
    val setup = false

    val clashKey = MidnightMurderers

    def pieces(options : $[Meta.O]) = $
}

// Forest Connections: on its turn the controller may move through a forest into another clearing next to it
case object ForestShortcut extends Transport {
    override def allows(f : Faction)(implicit game : Game) = f.can(ForestConnections) && game.current == f

    override def allows(f : Faction, o : Region, d : Region)(implicit game : Game) = allows(f, o) && (o, d) @@ {
        case (o : Clearing, d : Clearing) => o != d && game.board.forests.exists(x => game.fromForest(x).use(l => l.has(o) && l.has(d)))
        case _ => false
    }
}

// Infected
case object TheInfected extends Turnling with WarriorFaction {
    val name = "The Infected"
    val short = "TIN"
    val style = "tin"

    val demoted = false
    val setup = false

    val clashKey = TheInfected

    val warrior = Infected

    def pieces(options : $[Meta.O]) = Infected *** 20
}

case object InfectiousSpirits extends Turnling {
    val name = "Infectious Spirits"
    val short = "TIS"
    val style = "tis"

    val demoted = true
    val setup = false

    val clashKey = TheInfected

    def pieces(options : $[Meta.O]) = $
}

// Golden Knight
case object KnightTurnling extends Turnling {
    val name = "Golden Knight"
    val short = "TGK"
    val style = "tgk"

    val demoted = false
    val setup = false

    val clashKey = KnightTurnling

    def pieces(options : $[Meta.O]) = $
}

case object GoldenPig extends Turnling {
    val name = "Golden Pig"
    val short = "TGP"
    val style = "tgp"

    val demoted = true
    val setup = false

    val clashKey = KnightTurnling

    def pieces(options : $[Meta.O]) = $
}

// Cardinals
case object Cardinal extends Warrior

case object RougeAgents extends Turnling with WarriorFaction {
    val name = "Rouge Agents"
    val short = "TRA"
    val style = "tra"

    val demoted = false
    val setup = false

    val clashKey = RougeAgents

    val warrior = Cardinal

    def pieces(options : $[Meta.O]) = Cardinal *** 5
}

case object CardinalCarts extends Turnling {
    val name = "Cardinal Carts"
    val short = "TCC"
    val style = "tcc"

    val demoted = true
    val setup = true

    val clashKey = RougeAgents

    def pieces(options : $[Meta.O]) = $
}

case object CartCaravan extends Pawn {
    override def name = "Caravan"
}

// Doomsayers
case object Doomsayer extends Warrior

case object UnionFlame extends Pawn {
    override def name = "Flame"
}

case object UnionMembers extends Turnling with WarriorFaction {
    val name = "Union Members"
    val short = "TUM"
    val style = "tum"

    val demoted = false
    val setup = true

    val clashKey = UnionMembers

    val warrior = Doomsayer

    def pieces(options : $[Meta.O]) = Doomsayer *** 10
}

case object CorporateOverreach extends Turnling {
    val name = "Corporate Overreach"
    val short = "TCO"
    val style = "tco"

    val demoted = true
    val setup = false

    val clashKey = UnionMembers

    def pieces(options : $[Meta.O]) = $
}

// Vultures
case object CoupVulture extends Warrior {
    override def name = "Vulture"
}

case object ConvergingCoup extends Turnling with WarriorFaction {
    val name = "Converging Coup"
    val short = "TCV"
    val style = "tcv"

    val demoted = false
    val setup = true

    val clashKey = ConvergingCoup

    val warrior = CoupVulture

    def pieces(options : $[Meta.O]) = CoupVulture *** 10
}

case object FeederTurnling extends Turnling {
    val name = "Bottom Feeder"
    val short = "TBF"
    val style = "tbf"

    val demoted = true
    val setup = false

    val clashKey = ConvergingCoup

    def pieces(options : $[Meta.O]) = $
}

// Bugs
case object Bug extends Warrior

case object BuzzlingBuggers extends Turnling with WarriorFaction {
    val name = "Buzzling Buggers"
    val short = "TBB"
    val style = "tbb"

    val demoted = false
    val setup = true

    val clashKey = BuzzlingBuggers

    val warrior = Bug

    override val transports : $[$[Transport]] = $($())

    def pieces(options : $[Meta.O]) = Bug *** 10
}

case object MiracleCureAll extends Turnling {
    val name = "Miracle Cure-All"
    val short = "TMC"
    val style = "tmc"

    val demoted = true
    val setup = false

    val clashKey = BuzzlingBuggers

    def pieces(options : $[Meta.O]) = $
}

// Pigs
case object Grunt extends Warrior

case object GristlyGrunts extends Turnling with WarriorFaction {
    val name = "Gristly Grunts"
    val short = "TGG"
    val style = "tgg"

    val demoted = false
    val setup = true

    val clashKey = GristlyGrunts

    val warrior = Grunt

    def pieces(options : $[Meta.O]) = Grunt *** 10
}

case object Swine extends Warrior

case object FineSwine extends Turnling with WarriorFaction {
    val name = "Fine Swine"
    val short = "TFS"
    val style = "tfs"

    val demoted = true
    val setup = true

    val clashKey = GristlyGrunts

    val warrior = Swine

    override val transports : $[$[Transport]] = $($(SingleFigure))

    def pieces(options : $[Meta.O]) = Swine *** 3
}

// one figure at a time (Fine Swine), ignoring rule
case object SingleFigure extends Transport {
    override def allows(f : Faction, o : Region, d : Region, l : $[Movable])(implicit game : Game) = l.num == 1
}

// the Grunts moved by an enemy of their controller, by that enemy's rule
case object GruntRule extends Transport {
    override def allows(f : Faction, o : Region, d : Region)(implicit game : Game) = (o, d) @@ {
        case (o : Clearing, d : Clearing) => game.states.get(GristlyGrunts).of[TurnlingState]./~(_.user).exists(u => u.rules(o) || u.rules(d))
        case _ => false
    }
}

// an enemy's once-a-turn use of the Grunts
case object GruntsUsed extends Effect


// SETUP
case class TurnlingSetupForestsAction(self : Player, h : MidnightMurderers.type) extends ForcedAction
case class TurnlingSetupSuitsAction(self : Player, h : Turnling, l : $[BaseSuit]) extends ForcedAction
case class TurnlingSetupSuitAction(self : Player, h : Turnling, s : BaseSuit, c : Clearing, l : $[BaseSuit]) extends BaseAction(h, "setup in a", s, "clearing")(c)
case class TurnlingSetupFlameAction(self : Player, h : UnionMembers.type, r : Forest) extends BaseAction(h, "places the", UnionFlame.name.styled(h), "in")(r)
case class TurnlingSetupSwineAction(self : Player, h : FineSwine.type, c : Clearing) extends BaseAction(h, "places all", Swine.sof(h), "in")(c)

// TURNING OVER
case class TurnOverHirelingAction(self : Faction, h : Hireling, e : Faction) extends BaseAction(self, "turns over", h, "to")(e)

// HELPERS
case class TurnPlaceEachAction(self : Faction, h : WarriorFaction, l : $[Region], then : ForcedAction) extends ForcedAction
case class TurnPlaceOneAction(self : Faction, h : WarriorFaction, c : Region, l : $[Region], then : ForcedAction) extends BaseAction(h, "places", h.warrior.of(h), "in")(c)
case class TurnMoveBattleAction(self : Faction, h : Hireling, then : ForcedAction) extends ForcedAction
case class TurnSpendCardAction(self : Faction, h : Turnling, d : DeckCard, then : ForcedAction) extends ForcedAction

// MIDNIGHT MURDERERS
case class MurderersMainAction(self : Faction, h : MidnightMurderers.type) extends BaseAction("Start of", Birdsong)(h) with Soft
case class MurderersPlaceAction(self : Faction, h : MidnightMurderers.type, l : $[Forest]) extends ForcedAction
case class MurderersPlaceOneAction(self : Faction, h : MidnightMurderers.type, r : Forest, l : $[Forest]) extends BaseAction(h, "places", Whisperer.of(h), "in")(r)
case class MurderersConvergeAction(self : Faction, h : MidnightMurderers.type, c : Clearing) extends BaseAction(h, "converge on")(c)

// INFECTED
case class InfectedHiredAction(self : Faction, h : TheInfected.type, c : Clearing, then : ForcedAction) extends BaseAction(h, "places three", Infected.sof(h), "in")(c)
case class InfectedRollAction(self : Faction, h : Turnling) extends BaseAction("Start of", Birdsong)(h, "roll the Infected die")
case class InfectedRolledAction(self : Faction, h : Turnling, s : BaseSuit) extends RolledAction[BaseSuit] { def rolled = $(s) }
case class InfectedMoveAction(self : Faction, h : TheInfected.type, s : BaseSuit, l : $[Clearing]) extends ForcedAction
case class InfectedFromAction(self : Faction, h : TheInfected.type, s : BaseSuit, c : Clearing, o : Clearing, l : $[Clearing]) extends BaseAction(h, "moves an", Infected.of(h), "into", c, "from")(o)
case class InfectedSpreadAction(self : Faction, h : TheInfected.type, l : $[Clearing]) extends ForcedAction
case class InfectedReplaceAction(self : Faction, h : TheInfected.type, c : Clearing, e : WarriorFaction, l : $[Clearing]) extends BaseAction(h, "replaces a warrior in", c)(e.warrior.of(e), e.warrior.imgd(e))
case class SpiritsPlaceAction(self : WarriorFaction, h : InfectiousSpirits.type, c : Clearing, then : ForcedAction) extends BaseAction(h, "in", c)("Place", self.warrior.of(self))
case class SpiritsRefreshAction(self : Hero, h : InfectiousSpirits.type, c : Clearing, then : ForcedAction) extends BaseAction(h, "in", c)("Refresh an item")

// GOLDEN KNIGHT
case class KnightTurnlingPlaceAction(self : Faction, h : KnightTurnling.type, c : Clearing, then : ForcedAction) extends BaseAction(h, "places the Knight in")(c)
case class KnightTurnlingHitAction(k : Faction, f : Faction, c : Clearing, then : ForcedAction) extends ForcedAction
case class GoldenPigDrawAction(self : Faction, h : GoldenPig.type, then : ForcedAction) extends BaseAction(h)("Draw a card")
case class GoldenPigMainAction(self : Faction, h : GoldenPig.type, n : Int) extends BaseAction("Start of", Birdsong)(h, "look at", n.cards)
case class GoldenPigShuffleAction(shuffled : $[DeckCard], then : ForcedAction) extends ShuffledAction[DeckCard]
case class GoldenPigKeepAction(self : Faction, h : GoldenPig.type, d : DeckCard) extends BaseAction(h, "keeps one card")(d.img) with ViewCard

// CARDINALS
case class RougeAgentsMainAction(self : Faction, h : RougeAgents.type, then : ForcedAction) extends BaseAction("Start of", Birdsong)(h, "replace enemy warriors")
case class RougeAgentsReplaceAction(self : Faction, h : RougeAgents.type, n : Int, done : $[Clearing], then : ForcedAction) extends ForcedAction
case class RougeAgentsReplaceOneAction(self : Faction, h : RougeAgents.type, c : Clearing, e : WarriorFaction, n : Int, done : $[Clearing], then : ForcedAction) extends BaseAction(h, "replaces in", c)(e.warrior.of(e), e.warrior.imgd(e))
case class CaravanBringAction(self : Faction, f : Faction, from : Clearing, to : Clearing, then : ForcedAction) extends BaseAction(CardinalCarts)("Take the", CartCaravan.name.styled(CardinalCarts), "from", from, "to", to)
case class CaravanArrivedAction(self : Faction, to : Clearing, then : ForcedAction) extends ForcedAction

// DOOMSAYERS
case class UnionMainAction(self : Faction, h : UnionMembers.type) extends BaseAction("Start of", Birdsong)(h) with Soft
case class UnionPlaceAction(self : Faction, h : UnionMembers.type) extends BaseAction(h)("Place", Doomsayer.sof(h), "around the", UnionFlame.name.styled(h))
case class UnionMoveBattleAction(self : Faction, h : UnionMembers.type) extends BaseAction(h)("Move".styled(h), dt.Move, "then", "Battle".styled(h), dt.Battle, ", twice")
case class UnionFlameMainAction(self : Faction, h : UnionMembers.type) extends BaseAction(h)("Move the", UnionFlame.name.styled(h)) with Soft
case class UnionFlameAction(self : Faction, h : UnionMembers.type, r : Forest, then : ForcedAction) extends BaseAction(h, "moves the", UnionFlame.name.styled(h), "to")(r)
case class DemandRollAction(self : Faction, h : CorporateOverreach.type) extends BaseAction("Start of", Birdsong)(h, "roll the Demand die")
case class DemandRolledAction(self : Faction, h : CorporateOverreach.type, n : Int) extends RolledAction[Int] { def rolled = $(n) }
case class DemandCrusadeAction(self : Faction, h : CorporateOverreach.type) extends BaseAction(h, "Crusade!".hl)("Move", dt.Move, "then", "Battle", dt.Battle)
case class DemandPlaceAction(self : Faction, h : CorporateOverreach.type, p : Piece, c : Clearing) extends BaseAction(h, "Place Token or Building!".hl)(p.of(self), p.imgd(self), "in", c)
case class DemandCraftAction(self : Faction, h : CorporateOverreach.type) extends BaseAction(h, "Craft!".hl)("Craft")
case class DemandSkipAction(self : Faction, h : CorporateOverreach.type) extends BaseAction(h)("Don't".styled(styles.hit), "(one more control marker goes)")

// VULTURES
case class CoupMainAction(self : Faction, h : ConvergingCoup.type) extends BaseAction(Hirelings)(h) with Soft
case class CoupPlaceAction(self : Faction, h : ConvergingCoup.type, c : Clearing) extends BaseAction(h, "places", CoupVulture.sof(h), "around")(c)
case class CoupBattleAction(self : Faction, h : ConvergingCoup.type, c : Clearing) extends BaseAction(h, "battle in")(c)
case class CoupDiscardAction(self : Faction, h : ConvergingCoup.type, c : Clearing) extends ForcedAction
case class FeederPileDrawAction(self : Faction, h : FeederTurnling.type, n : Int, m : Message, then : ForcedAction) extends ForcedAction
case class FeederDeckDrawAction(self : Faction, h : FeederTurnling.type, n : Int, m : Message, then : ForcedAction) extends ForcedAction
case class FeederTopAction(self : Faction, h : FeederTurnling.type, m : Message, then : ForcedAction) extends ForcedAction
case class FeederDoneAction(h : FeederTurnling.type, then : ForcedAction) extends ForcedAction

// BUGS
case class BugsMainAction(self : Faction, h : BuzzlingBuggers.type) extends BaseAction(Hirelings)(h) with Soft
case class BugsSuitAction(self : Faction, h : BuzzlingBuggers.type, s : BaseSuit) extends BaseAction(h, "places", Bug.sof(h), "in")(s, "clearings")
case class BugsGatherAction(self : Faction, h : BuzzlingBuggers.type, c : Clearing) extends BaseAction(h, "gather in")(c)
case class BugsFromAction(self : Faction, h : BuzzlingBuggers.type, c : Clearing, o : Clearing) extends BaseAction(h, "move a", Bug.of(h), "into", c, "from")(o)
case class BugsBattleAction(self : Faction, h : BuzzlingBuggers.type, c : Clearing) extends ForcedAction
case class CureAllTakeAction(self : Faction, h : MiracleCureAll.type) extends BaseAction("Start of", Birdsong)(h, "take a Miracle Cure-All")
case class CureAllMainAction(self : Faction, h : MiracleCureAll.type) extends BaseAction(h)("Discard a Miracle Cure-All") with Soft
case class CureAllCopyAction(self : Faction, h : MiracleCureAll.type, e : Faction, d : CraftEffectCard) extends BaseAction(h, "copies a crafted card")(d.img, "of", e) with ViewCard
case class CureAllReturnAction(self : Faction, h : MiracleCureAll.type, e : Faction, i : ItemRef) extends BaseAction(h, "returns an item to the supply")(i.item.img, i.item, "of", e)

// PIGS
case class GruntsMainAction(self : Faction, h : GristlyGrunts.type, again : Boolean) extends BaseAction(Hirelings)(h, again.?("again")) with Soft
case class GruntsPayAction(self : Faction, h : GristlyGrunts.type, c : Clearing) extends BaseAction(h)("Place two", Grunt.sof(h), "in", c) with Soft
case class GruntsPlaceAction(self : Faction, h : GristlyGrunts.type, d : DeckCard, c : Clearing) extends ForcedAction
case class GruntsMoveBattleAction(self : Faction, h : GristlyGrunts.type) extends ForcedAction
case class GruntsAgainAction(self : Faction, h : GristlyGrunts.type) extends ForcedAction
case class GruntsAgainPaidAction(self : Faction, h : GristlyGrunts.type, d : DeckCard) extends ForcedAction
case class GruntsHireMainAction(self : Faction, h : GristlyGrunts.type) extends BaseAction(Hirelings)(h, "as your warriors") with Soft
case class GruntsHirePayAction(self : Faction, h : GristlyGrunts.type, c : Clearing) extends BaseAction(h, "as your warriors")(c) with Soft
case class GruntsHireAction(self : Faction, h : GristlyGrunts.type, d : DeckCard, c : Clearing) extends ForcedAction
case class GruntsHireDoneAction(self : Faction, h : GristlyGrunts.type, then : ForcedAction) extends ForcedAction
case class GruntsHireBattleAction(self : Faction, h : GristlyGrunts.type, c : Clearing) extends ForcedAction
case class DemandBattleAction(self : Faction, h : CorporateOverreach.type) extends ForcedAction
case class SwineSecondMoveAction(self : Faction, h : FineSwine.type) extends ForcedAction
case class SwineMainAction(self : Faction, h : FineSwine.type) extends BaseAction(Hirelings)(h) with Soft
case class SwineBuyAction(self : Faction, h : FineSwine.type, c : Clearing, e : Faction, p : Piece) extends BaseAction(h, "forecloses in", c)(p.of(e), p.imgd(e)) with Soft
case class SwineBuyPaidAction(self : Faction, h : FineSwine.type, c : Clearing, e : Faction, p : Piece, l : $[DeckCard]) extends ForcedAction
case class SwineMoveAction(self : Faction, h : FineSwine.type) extends BaseAction(h)("Move a", Swine.of(h), "twice") with Soft
case class SwineShoveAction(self : Faction, h : FineSwine.type, c : Clearing, d : Clearing, then : ForcedAction) extends BaseAction(h, "shoves a", Swine.of(h), "from", c, "to")(d)


object TurnlingsExpansion extends Expansion {
    def active(setup : $[Faction], options : $[Meta.O]) = options.has(TurnOverHirelings) || options.of[IncludeHireling].exists(_.hireling.is[Turnling])

    implicit def turnlingState(h : Turnling)(implicit game : Game) : TurnlingState = game.states(h).as[TurnlingState].get

    def in(h : Turnling)(implicit game : Game) = game.states.contains(h) && hirelings.has(h)

    def owner(h : Turnling)(implicit game : Game) : |[Faction] = in(h).??(factions.%(_.has(h)).single)

    // the Infected and the Golden Knight aren't played with the Dark Deck (Law of Doom D8.0)
    def drawable(h : Hireling, options : $[Meta.O]) = options.has(DarkDeck).not || $[Hireling](TheInfected, InfectiousSpirits, KnightTurnling, GoldenPig).has(h).not

    def adjacentForests(c : Clearing)(implicit game : Game) : $[Forest] = board.forests.%(r => game.fromForest(r).has(c))

    // the warriors of factions in a clearing (the Infected replace them)
    def factionWarriors(c : Clearing)(implicit game : Game) : $[WarriorFaction] = factions.of[WarriorFaction].%(e => e.at(c).has(e.warrior))

    // the enemy warriors of a faction in a clearing (Rouge Agents)
    def enemyWarriors(f : Faction, c : Clearing)(implicit game : Game) : $[WarriorFaction] =
        (factions ++ hirelings).%(game.states.contains).of[WarriorFaction].%(e => f.friends(e).not).%(e => e.at(c).has(e.warrior)).but(RougeAgents)

    // the forests the Flame may move to
    def flameForests(implicit game : Game) : $[Forest] = UnionMembers.?%(in)./~(h => board.forests.%(r => h.flame.has(r).not).%(r => game.fromForest(r).any && game.fromForest(r).forall(c => h.at(c).has(Doomsayer))))

    // the pieces a faction may place for Place Token or Building!
    def demandPlacements(f : Faction)(implicit game : Game) : $[(Piece, Clearing)] = {
        val bb = f.reserve.$./(_.piece).of[Building].distinct
        val tt = f.reserve.$./(_.piece).of[Token].distinct

        val fitting : Piece => Clearing => Boolean = {
            case Garden(s) => c => c.cost.matched(s)
            case TradePost(s) => c => c.cost.matched(s)
            case Base(s) => c => c.cost.matched(s)
            case _ => c => true
        }

        f.is[Mischief].not.??(
            bb./~(p => clearings.%(f.rules).%(f.canBuild).%(fitting(p))./(c => p -> c)) ++
            tt./~(p => clearings.%(f.present).%(f.canPlace).%(fitting(p)).%(c => p != Sympathy || f.at(c).has(Sympathy).not)./(c => p -> c))
        )
    }

    def cureCopies(f : Faction)(implicit game : Game) : $[(Faction, CraftEffectCard)] =
        factions.but(f).%(e => f.friends(e).not).%(e => clearings.exists(c => f.present(c) && e.present(c)))./~(e => e.stuck.$.of[CraftEffectCard]./(d => e -> d))

    def cureReturns(f : Faction)(implicit game : Game) : $[(Faction, ItemRef)] =
        factions.notOf[Hero]./~(e => e.forTrade./(i => e -> i))

    // the start of Birdsong (turn.scala)
    def birdsongStart(f : Faction)(implicit game : Game) : $[UserAction] =
        MidnightMurderers.?%(f.can)./(h => MurderersMainAction(f, h)).$ ++
        TheInfected.?%(f.can)./(h => InfectedRollAction(f, h)).$ ++
        InfectiousSpirits.?%(f.can)./(h => InfectedRollAction(f, h)).$ ++
        GoldenPig.?%(f.can).%(_ => deck.any && clearings.exists(f.rules))./(h => GoldenPigMainAction(f, h, clearings.%(f.rules).num)).$ ++
        RougeAgents.?%(f.can).%(h => h.pool(Cardinal) && clearings.exists(c => enemyWarriors(f, c).any))./(h => RougeAgentsMainAction(f, h, UsedEffectAction(f, h, Repeat))).$ ++
        UnionMembers.?%(f.can)./(h => UnionMainAction(f, h)).$ ++
        CorporateOverreach.?%(f.can)./(h => DemandRollAction(f, h)).$ ++
        MiracleCureAll.?%(f.can).%(_.cures < factions.num)./(h => CureAllTakeAction(f, h)).$

    override def daylight(f : Faction)(implicit game : Game, ask : ActionCollector) {
        RougeAgents.?%(f.can).foreach { h =>
            + HirelingMoveBattleAction(f, h, UsedEffectAction(f, h, Repeat)).!(h.moveFrom.none && clearings.%(h.canAttackIn).none, "can't move or battle")
        }

        if (f.has(UnionMembers) && flameForests.any)
            + UnionFlameMainAction(f, UnionMembers)

        ConvergingCoup.?%(f.can).foreach { h =>
            + CoupMainAction(f, h)
        }

        BuzzlingBuggers.?%(f.can).foreach { h =>
            + BugsMainAction(f, h)
        }

        GristlyGrunts.?%(f.can).foreach { h =>
            + GruntsMainAction(f, h, false).!(f.hand.none && h.presence.none, "no cards")
        }

        // an enemy of the Grunts' controller, once on its turn
        GristlyGrunts.?%(in).%(h => owner(h).exists(o => o.friends(f).not)).%(_ => f.used.has(GruntsUsed).not).foreach { h =>
            + GruntsHireMainAction(f, h).!(clearings.%(h.present).%(f.present).%(c => f.hand.exists(_.matches(c.cost))).none, "no cards")
        }

        FineSwine.?%(f.can).foreach { h =>
            + SwineMainAction(f, h)
        }

        anytime(f)
    }

    override def birdsong(f : Faction)(implicit game : Game, ask : ActionCollector) {
        anytime(f)
    }

    override def evening(f : Faction)(implicit game : Game, ask : ActionCollector) {
        anytime(f)
    }

    def anytime(f : Faction)(implicit game : Game, ask : ActionCollector) {
        if (f.has(MiracleCureAll) && MiracleCureAll.cures > 0 && (cureCopies(f).any || cureReturns(f).any))
            + CureAllMainAction(f, MiracleCureAll)
    }

    // the Neutral markers drawn in a region (game.scala displayRegion)
    def markers(r : Region)(implicit game : Game) : $[Figure] =
        hirelings.of[Turnling].%(game.states.contains)./~{ h =>
            r match {
                case c : Clearing =>
                    h.knight.has(c).$(Figure(Neutral, GoldenKnightMarker, 7)) ++
                    h.caravans.%(_ == c).indices.$./(n => Figure(Neutral, CartCaravan, n))
                case r : Forest =>
                    h.flame.has(r).$(Figure(Neutral, UnionFlame, 0))
                case _ => $[Figure]()
            }
        }

    def perform(action : Action, soft : Void)(implicit game : Game) = action @@ {
        // SETUP
        case SetupHirelingAction(f, h : Turnling) =>
            game.states += h -> new TurnlingState(h)

            game.hirelings :+= h

            log(f, "setup", h)

            (h match {
                case h : MidnightMurderers.type => TurnlingSetupForestsAction(f, h)
                case h : CardinalCarts.type => TurnlingSetupSuitsAction(f, h, FoxRabbitMouse)
                case h : ConvergingCoup.type => TurnlingSetupSuitsAction(f, h, FoxRabbitMouse)
                case h : GristlyGrunts.type => TurnlingSetupSuitsAction(f, h, FoxRabbitMouse)
                case h : BuzzlingBuggers.type =>
                    board.inner.%(h.canPlace).take(h.pooled(Bug)).foreach(c => h.reserve --> Bug --> c)

                    h.log("placed", Bug.sof(h), "in the clearings off the edge")

                    SetupNextHirelingAction
                case h : UnionMembers.type =>
                    Ask(f).group(h, "places the", UnionFlame.name.styled(h)).each(board.forests)(r => TurnlingSetupFlameAction(f, h, r)).needOk
                case h : FineSwine.type =>
                    Ask(f).group(h).each(clearings.%(h.canPlace))(c => TurnlingSetupSwineAction(f, h, c)).needOk
                case _ => SetupNextHirelingAction
            }) : Continue

        case TurnlingSetupForestsAction(f, h) =>
            board.forests.take(h.pooled(Whisperer)).foreach(r => h.reserve --> Whisperer --> r)

            h.log("placed a", Whisperer.of(h), "in each forest")

            SetupNextHirelingAction

        case TurnlingSetupSuitsAction(f, h, Nil) =>
            SetupNextHirelingAction

        case TurnlingSetupSuitsAction(f, h, s :: rest) =>
            val l = clearings.%(_.cost.matched(s)).%(c => h @@ {
                case h : CardinalCarts.type => h.caravans.has(c).not
                case h => h.canPlace(c)
            })

            Ask(f).group(h, "setup in a", s, "clearing").each(l)(c => TurnlingSetupSuitAction(f, h, s, c, rest)).bail(TurnlingSetupSuitsAction(f, h, rest))

        case TurnlingSetupSuitAction(f, h, s, c, rest) =>
            h @@ {
                case h : CardinalCarts.type =>
                    h.caravans :+= c

                    h.log("placed a", CartCaravan.name.styled(h), "in", c)
                case h : ConvergingCoup.type =>
                    h.reserve --> $(CoupVulture, CoupVulture, CoupVulture).take(h.pooled(CoupVulture)) --> c

                    h.log("placed three", CoupVulture.sof(h), "in", c)
                case h : GristlyGrunts.type =>
                    h.reserve --> $(Grunt, Grunt).take(h.pooled(Grunt)) --> c

                    h.log("placed two", Grunt.sof(h), "in", c)
                case _ =>
            }

            TurnlingSetupSuitsAction(f, h, rest)

        case TurnlingSetupFlameAction(f, h, r) =>
            h.flame = |(r)

            val l = game.fromForest(r).%(h.canPlace).take(h.pooled(Doomsayer))

            l.foreach(c => h.reserve --> Doomsayer --> c)

            h.log("lit the", UnionFlame.name.styled(h), "in", r, "and placed", Doomsayer.sof(h), "around it")

            SetupNextHirelingAction

        case TurnlingSetupSwineAction(f, h, c) =>
            h.reserve --> c

            h.log("placed", Swine.sof(h), "in", c)

            SetupNextHirelingAction

        // TURNING OVER
        case HireHirelingAction(f, h) if options.has(TurnOverHirelings) && factions.but(f).any =>
            Ask(f).group(f, "turns over", h, "to").each(factions.but(f))(e => TurnOverHirelingAction(f, h, e))

        case TurnOverHirelingAction(f, h, e) =>
            f.effects :-= NewHireling

            game.unhired :-= h

            f.log("turned over", h, "to", e)

            TakeHirelingAction(e, h, HirelingPurchasesAction(f))

        // HELPERS
        case TurnPlaceEachAction(f, h, l, then) =>
            val ll = l.distinct.%(r => r.as[Clearing].?(h.canPlace).|(true))

            if (ll.none || h.pool(h.warrior).not)
                then
            else
            if (h.pooled(h.warrior) >= ll.num) {
                ll.foreach(r => h.reserve --> h.warrior --> r)

                h.log("placed", ll.num.of(h.warrior.name.styled(h)), "in", ll./(_.elem).comma)

                then
            }
            else
                Ask(f).group(h, "places", h.warrior.of(h)).each(ll)(r => TurnPlaceOneAction(f, h, r, ll.but(r), then))

        case TurnPlaceOneAction(f, h, r, l, then) =>
            h.reserve --> h.warrior --> r

            h.log("placed", h.warrior.of(h), "in", r)

            TurnPlaceEachAction(f, h, l, then)

        case TurnMoveBattleAction(f, h, then) =>
            val battle = HirelingMovedBattleAction(f, h, then)

            if (h.moveFrom.any)
                MoveInitAction(f, h, $, NoMessage, h.moveFrom, h.movable, $(battle.as("Skip", "Move".styled(h))), battle)
            else
                battle

        case TurnSpendCardAction(f, h, d, then) =>
            f.hand --> d --> discard.quiet

            f.log("spent", d, "for", h)

            then

        // MIDNIGHT MURDERERS
        case MurderersMainAction(f, h) =>
            val near = clearings.%(c => adjacentForests(c).exists(r => h.at(r).any))

            Ask(f).group(h)
                .add(MurderersPlaceAction(f, h, board.forests).as("Place", Whisperer.sof(h), "in forests").!(h.pool(Whisperer).not, "no warriors"))
                .each(near)(c => MurderersConvergeAction(f, h, c))
                .cancel

        case MurderersPlaceAction(f, h, l) =>
            f.used :+= h

            if (l.none || h.pool(Whisperer).not)
                Repeat
            else
                Ask(f).group(h, "places", Whisperer.sof(h), "in forests").each(l)(r => MurderersPlaceOneAction(f, h, r, l.but(r))).done(Repeat)

        case MurderersPlaceOneAction(f, h, r, l) =>
            h.reserve --> Whisperer --> r

            h.log("placed", Whisperer.of(h), "in", r)

            MurderersPlaceAction(f, h, l)

        case MurderersConvergeAction(f, h, c) =>
            f.used :+= h

            val from = adjacentForests(c).%(r => h.at(r).any)

            val n = from./(r => h.at(r).num).sum

            from.foreach(r => h.from(r) --> c)

            h.log("moved", n.of(Whisperer.name.styled(h)), "from", from./(_.elem).comma, "to", c)

            if (h.canAttackIn(c))
                BattleInitAction(f, h, NoMessage, $(c), $, Repeat)
            else
                Repeat

        // INFECTED
        case OnTakeHirelingAction(f, h : TheInfected.type, then) if h.presence.none =>
            val l = clearings.%(_.asset.matches(Mouse)).%(h.canPlace)

            Ask(f).group(h, "places three", Infected.sof(h)).each(l)(c => InfectedHiredAction(f, h, c, then)).bail(then)

        case InfectedHiredAction(f, h, c, then) =>
            h.reserve --> $(Infected, Infected, Infected) --> c

            h.log("appeared in", c)

            then

        case InfectedRollAction(f, h) =>
            f.used :+= h

            Roll[BaseSuit]($(BaseSuitDie), l => InfectedRolledAction(f, h, l(0)), f.elem ~ " rolls the Infected die")

        case InfectedRolledAction(f, h : InfectiousSpirits.type, s) =>
            h.suit = |(s)
            h.spirited = $

            f.log("rolled", s, "for", h)

            Repeat

        case InfectedRolledAction(f, h : TheInfected.type, s) =>
            f.log("rolled", s, "for the", Infected.sof(h))

            InfectedMoveAction(f, h, s, clearings.%(_.asset.matches(s)))

        case InfectedMoveAction(f, h, s, Nil) =>
            InfectedSpreadAction(f, h, clearings.%(_.asset.matches(s)).%(h.present))

        case InfectedMoveAction(f, h, s, c :: rest) =>
            val from = game.connected(c).%(o => h.at(o).count(Infected) >= o.asset.matches(s).?(2).|(1))

            if (from.none)
                InfectedMoveAction(f, h, s, rest)
            else
                Ask(f).group(h, "moves an", Infected.of(h), "into", c).each(from)(o => InfectedFromAction(f, h, s, c, o, rest))

        case InfectedFromAction(f, h, s, c, o, rest) =>
            h.from(o) --> Infected --> c

            h.log("moved from", o, "to", c)

            InfectedMoveAction(f, h, s, rest)

        case InfectedSpreadAction(f, h, Nil) =>
            Repeat

        case InfectedSpreadAction(f, h, c :: rest) =>
            val l = factionWarriors(c)

            if (l.none || h.pool(Infected).not)
                InfectedSpreadAction(f, h, rest)
            else
                Ask(f).group(h, "replaces a warrior in", c).each(l)(e => InfectedReplaceAction(f, h, c, e, rest))

        case InfectedReplaceAction(f, h, c, e, rest) =>
            e.from(c) --> e.warrior --> e.reserve
            h.reserve --> Infected --> c

            h.log("replaced", e.warrior.of(e), "in", c)

            InfectedSpreadAction(f, h, rest)

        // the Infected take the higher roll as defenders
        case BattleAdjustRollAction(b, f, o, fs, os, fr, or) if fr > or && b.defender == TheInfected =>
            BattleAdjustRollAction(b, f, o, fs, os, or, fr)

        // Infectious Spirits: a clearing of the rolled suit moved into on the controller's turn
        case MoveCompleteAction(self, f, from, to : Clearing, l, e, then) if e.has(InfectiousSpirits).not && game.current == f && f.has(InfectiousSpirits) && InfectiousSpirits.suit.exists(s => to.asset.matches(s)) && InfectiousSpirits.spirited.has(to).not =>
            val h = InfectiousSpirits
            val next = MoveCompleteAction(self, f, from, to, l, e :+ h, then)

            h.spirited :+= to

            val place = f.as[WarriorFaction].%(w => w.pool(w.warrior) && w.canPlace(to))./(w => SpiritsPlaceAction(w, h, to, next))
            val refresh = f.as[Hero].%(_.inv.exists(i => i.exhausted && i.damaged.not))./(v => SpiritsRefreshAction(v, h, to, next))

            if (place.none && refresh.none)
                next
            else
                Ask(f).group(h).add(place.$).add(refresh.$).skip(next)

        case SpiritsPlaceAction(f, h, c, then) =>
            f.reserve --> f.warrior --> c

            f.log("placed", f.warrior.of(f), "in", c, "with", h)

            then

        case SpiritsRefreshAction(f, h, c, then) =>
            val i = f.inv.%(i => i.exhausted && i.damaged.not).first

            f.inv = f.inv.diff($(i)) :+ i.refresh

            f.log("refreshed", i.item, "with", h)

            then

        // GOLDEN KNIGHT
        case OnTakeHirelingAction(f, h : KnightTurnling.type, then) =>
            if (h.knight.any)
                h.log("left", h.knight.get)

            h.knight = None

            val l = f.as[Hero]./(_.region).$.of[Clearing] ++ f.is[Hero].not.??(clearings.%(f.rules))

            Ask(f).group(h, "places the Knight").each(l)(c => KnightTurnlingPlaceAction(f, h, c, then)).bail(then)

        case KnightTurnlingPlaceAction(f, h, c, then) =>
            h.knight = |(c)

            f.log("placed the", h, "in", c)

            then

        case MoveCompleteAction(self, f, from, to : Clearing, l, e, then) if e.has(KnightTurnling).not && in(KnightTurnling) && KnightTurnling.knight.has(to) && owner(KnightTurnling).exists(k => k != f && k.friends(f).not) =>
            val k = owner(KnightTurnling).get

            MoveCompleteAction(self, f, from, to, l, e :+ KnightTurnling, KnightTurnlingHitAction(k, f, to, then))

        case KnightTurnlingHitAction(k, f, c, then) =>
            if (f.present(c) && k.canRemove(c)(f)) {
                KnightTurnling.log("hit", f, "in", c)

                BattleAssignHitsAction(f, Battle(c, k, None, f, None, None, None, 0, 0, 0, then), 1, 0, ForcedRemoveFinishedAction(k, then))
            }
            else
                then

        case OnTakeHirelingAction(f, h : GoldenPig.type, then) =>
            Ask(f).group(h).add(GoldenPigDrawAction(f, h, then)).skip(then)

        case GoldenPigDrawAction(f, h, then) =>
            DrawCardsAction(f, 1, WithEffect(h), AddCardsAction(f, then))

        case GoldenPigMainAction(f, h, n) =>
            f.used :+= h

            Ask(f).group(h, "keeps one card").each(deck.$.take(n))(d => GoldenPigKeepAction(f, h, d))

        case GoldenPigKeepAction(f, h, d) =>
            deck --> d --> f.hand

            f.log("kept a card with", h, "and shuffled the deck")

            Shuffle[DeckCard](deck, GoldenPigShuffleAction(_, Repeat))

        case GoldenPigShuffleAction(l, then) =>
            deck --> l --> deck

            then

        // CARDINALS
        case RougeAgentsMainAction(f, h, then) =>
            RougeAgentsReplaceAction(f, h, 2, $, then)

        case OnTakeHirelingAction(f, h : RougeAgents.type, then) =>
            RougeAgentsReplaceAction(f, h, 2, $, then)

        case RougeAgentsReplaceAction(f, h, n, done, then) =>
            val l = clearings.diff(done)./~(c => enemyWarriors(f, c)./(e => (c, e)))

            if (n <= 0 || l.none || h.pool(Cardinal).not)
                then
            else
                Ask(f).group(h, "replaces an enemy warrior").each(l)((c, e) => RougeAgentsReplaceOneAction(f, h, c, e, n, done, then))

        case RougeAgentsReplaceOneAction(f, h, c, e, n, done, then) =>
            e.from(c) --> e.warrior --> e.reserve
            h.reserve --> Cardinal --> c

            h.log("replaced", e.warrior.of(e), "in", c)

            RougeAgentsReplaceAction(f, h, n - 1, done :+ c, then)

        // Cardinal Carts: a Caravan may come along
        case MoveCompleteAction(self, f, from : Clearing, to : Clearing, l, e, then) if e.has(CardinalCarts).not && in(CardinalCarts) && CardinalCarts.caravans.has(from) && CardinalCarts.caravans.has(to).not && l.exists(p => p.is[Warrior] || p.is[Pawn]) =>
            val next = MoveCompleteAction(self, f, from, to, l, e :+ CardinalCarts, then)

            Ask(self).add(CaravanBringAction(self, f, from, to, next)).skip(next)

        case CaravanBringAction(self, f, from, to, then) =>
            val h = CardinalCarts

            h.caravans = h.caravans.diff($(from)) :+ to

            f.log("took a", CartCaravan.name.styled(h), "from", from, "to", to)

            CaravanArrivedAction(f, to, then)

        case CaravanArrivedAction(f, to, then) =>
            owner(CardinalCarts).%(_.rules(to)) match {
                case Some(o) => DrawCardsAction(o, 1, WithEffect(CardinalCarts), AddCardsAction(o, then))
                case None => then
            }

        // DOOMSAYERS
        case UnionMainAction(f, h) =>
            Ask(f).group(h)
                .add(UnionPlaceAction(f, h).!(h.pool(Doomsayer).not, "no warriors"))
                .add(UnionMoveBattleAction(f, h))
                .each(flameForests)(r => UnionFlameAction(f, h, r, Repeat))
                .cancel

        case UnionPlaceAction(f, h) =>
            f.used :+= h

            TurnPlaceEachAction(f, h, h.flame./~(r => game.fromForest(r)), Repeat)

        case UnionMoveBattleAction(f, h) =>
            f.used :+= h

            TurnMoveBattleAction(f, h, TurnMoveBattleAction(f, h, Repeat))

        case UnionFlameMainAction(f, h) =>
            Ask(f).group(h, "moves the", UnionFlame.name.styled(h), "to").each(flameForests)(r => UnionFlameAction(f, h, r, Repeat)).cancel

        case UnionFlameAction(f, h, r, then) =>
            h.flame = |(r)

            val burnt = (factions ++ hirelings).%(game.states.contains).%(e => e != h && h.friends(e).not)./~(e => e.from(r).$.%(_.piece.is[Warrior]).%(_.piece.is[Tenacious].not))

            burnt.foreach { u =>
                u --> u.faction.reserve
            }

            h.log("moved the", UnionFlame.name.styled(h), "to", r, burnt.any.$("and burned", burnt./(u => u.piece.of(u.faction)).comma))

            then

        case DemandRollAction(f, h) =>
            f.used :+= h

            Roll[Int]($(Die.from(1, 1, 2, 2, 3, 3)), l => DemandRolledAction(f, h, l(0)), f.elem ~ " rolls the Demand die")

        case DemandRolledAction(f, h, n) =>
            h.owed = true

            val name = n @@ {
                case 1 => "Crusade!"
                case 2 => "Place Token or Building!"
                case _ => "Craft!"
            }

            val ask = n @@ {
                case 1 =>
                    val can = f.moveFrom.any || clearings.exists(f.canAttackIn)
                    Ask(f).group(h, "Crusade!".hl).add(DemandCrusadeAction(f, h).!(can.not, "can't move or battle"))
                case 2 =>
                    Ask(f).group(h, "Place Token or Building!".hl).each(demandPlacements(f))((p, c) => DemandPlaceAction(f, h, p, c))
                case _ =>
                    Ask(f).group(h, "Craft!".hl).add(DemandCraftAction(f, h).!(f.hand.%(f.craftable).none, "nothing craftable"))
            }

            f.log("rolled", name.hl, "for", h)

            ask.add(DemandSkipAction(f, h))

        case DemandSkipAction(f, h) =>
            f.log("didn't take the demand of", h)

            Repeat

        case DemandCrusadeAction(f, h) =>
            h.owed = false

            val battle = DemandBattleAction(f, h)

            if (f.moveFrom.any)
                MoveInitAction(f, f, $, WithEffect(h), f.moveFrom, f.movable, $(battle.as("Skip", "Move")), battle)
            else
                battle

        case DemandBattleAction(f, h) =>
            val l = clearings.%(f.canAttackIn)

            if (l.any)
                BattleInitAction(f, f, WithEffect(h), l, $(Repeat.as("Skip", "Battle")), Repeat)
            else
                Repeat

        case DemandPlaceAction(f, h, p, c) =>
            h.owed = false

            f.reserve --> p --> c

            f.log("placed", p.of(f), "in", c, "with", h)

            Repeat

        case DemandCraftAction(f, h) =>
            h.owed = false

            YYSelectObjectsAction(f, f.hand)
                .withGroup("Craft".styled(f) ~ " with " ~ (f.craft ++ f.frogCraft ++ f.extraCraft).diff(f.crafted)./(dt.CraftSuit).merge)
                .withRule(f.craftable)
                .withThen(d => CraftAction(f, d, NoMessage, CraftPerformAction(f, d, NoMessage)))(d => game.desc("Craft", d, "with", d.as[CraftCard]./~(_.cost./(dt.CraftSuit)).merge))("Craft")
                .withExtra($(NoHand))

        case EndPlayerTurnAction(f) if in(CorporateOverreach) && f.has(CorporateOverreach) && CorporateOverreach.owed =>
            val h = CorporateOverreach

            h.owed = false

            if (f.contracts.has(h)) {
                f.contracts = f.contracts.diff($(h))

                f.log("lost a control marker of", h)
            }

            EndPlayerTurnAction(f)

        // VULTURES
        case CoupMainAction(f, h) =>
            val empty = clearings.%(c => h.at(c).has(CoupVulture).not).%(h.canPlace)
            val fight = clearings.%(c => h.at(c).has(CoupVulture)).%(h.canAttackIn)

            Ask(f).group(h)
                .each(empty)(c => CoupPlaceAction(f, h, c).!(h.pool(CoupVulture).not, "no warriors"))
                .each(fight)(c => CoupBattleAction(f, h, c))
                .cancel

        case CoupPlaceAction(f, h, c) =>
            f.used :+= h

            val n = min(game.connected(c).num, h.pooled(CoupVulture))

            h.reserve --> n.times(CoupVulture) --> c

            h.log("placed", n.of(CoupVulture.name.styled(h)), "in", c)

            Repeat

        case CoupBattleAction(f, h, c) =>
            f.used :+= h

            BattleInitAction(f, h, NoMessage, $(c), $, CoupDiscardAction(f, h, c))

        case CoupDiscardAction(f, h, c) =>
            deck.$.starting match {
                case Some(d) =>
                    deck --> d --> discard.quiet

                    h.log("discarded", d, "from the deck")

                    if (d.suit == Bird && h.canAttackIn(c))
                        Ask(f).group(h, "battle again in", c).add(BattleInitAction(f, h, NoMessage, $(c), $, Repeat).as("Battle".styled(h), dt.Battle, "in", c)(h)).skip(Repeat)
                    else
                        Repeat
                case None =>
                    Repeat
            }

        // Bottom Feeder: the controller's draws may start from the top of the discard pile
        case DrawCardsAction(f, n, m, then) if n > 0 && pile.any && in(FeederTurnling) && f.has(FeederTurnling) && FeederTurnling.drawing.not =>
            val h = FeederTurnling

            Ask(f)
                .group(f, "draws", n.cards)
                .add(FeederPileDrawAction(f, h, n, m, then).as("Draw", pile.$.last, "from the discard pile")(h))
                .add(FeederDeckDrawAction(f, h, n, m, then).as("Draw from the deck")(h))

        case FeederDeckDrawAction(f, h, n, m, then) =>
            h.drawing = true

            DrawCardsAction(f, n, m, FeederDoneAction(h, then))

        case FeederPileDrawAction(f, h, n, m, then) =>
            h.drawing = true

            if (n > 1)
                DrawCardsAction(f, n - 1, m, FeederTopAction(f, h, m, then))
            else
                FeederTopAction(f, h, m, then)

        case FeederTopAction(f, h, m, then) =>
            pile.$.lastOption.foreach { d =>
                pile --> d --> f.drawn

                f.log("drew", d, "from the discard pile with", h)
            }

            FeederDoneAction(h, then)

        case FeederDoneAction(h, then) =>
            h.drawing = false

            then

        // BUGS
        case BugsMainAction(f, h) =>
            val gather = clearings.%(c => game.connected(c).exists(o => h.at(o).has(Bug)))

            Ask(f).group(h)
                .each(FoxRabbitMouse)(s => BugsSuitAction(f, h, s).!(h.pool(Bug).not, "no warriors"))
                .each(gather)(c => BugsGatherAction(f, h, c))
                .cancel

        case BugsSuitAction(f, h, s) =>
            f.used :+= h

            TurnPlaceEachAction(f, h, clearings.%(_.cost.matched(s)), Repeat)

        case BugsGatherAction(f, h, c) =>
            f.used :+= h

            val from = game.connected(c).%(o => h.at(o).has(Bug))

            Ask(f).group(h, "gather in", c).each(from)(o => BugsFromAction(f, h, c, o)).done(BugsBattleAction(f, h, c))

        case BugsFromAction(f, h, c, o) =>
            h.from(o) --> Bug --> c

            h.log("moved a", Bug.of(h), "from", o, "to", c)

            val from = game.connected(c).%(o => h.at(o).has(Bug))

            Ask(f).group(h, "gather in", c).each(from)(o => BugsFromAction(f, h, c, o)).done(BugsBattleAction(f, h, c))

        case BugsBattleAction(f, h, c) =>
            if (h.canAttackIn(c))
                BattleInitAction(f, h, NoMessage, $(c), $, Repeat)
            else
                Repeat

        // a player moving out of a clearing with Bugs takes one along
        case MoveCompleteAction(self, f, from : Clearing, to : Clearing, l, e, then) if e.has(BuzzlingBuggers).not && f != BuzzlingBuggers && in(BuzzlingBuggers) && BuzzlingBuggers.at(from).has(Bug) && l.exists(p => p.is[Warrior] || p.is[Pawn]) =>
            val h = BuzzlingBuggers

            h.from(from) --> Bug --> to

            h.log("followed", f, "from", from, "to", to)

            MoveCompleteAction(self, f, from, to, l, e :+ h, then)

        case CureAllTakeAction(f, h) =>
            f.used :+= h

            h.cures += 1

            f.log("took a", h, "(" ~ h.cures.hl ~ ")")

            Repeat

        case CureAllMainAction(f, h) =>
            Ask(f).group(h)
                .each(cureCopies(f))((e, d) => CureAllCopyAction(f, h, e, d))
                .each(cureReturns(f))((e, i) => CureAllReturnAction(f, h, e, i))
                .cancel

        case CureAllCopyAction(f, h, e, d) =>
            h.cures -= 1

            f.dopple = $(d.effect)

            f.log("discarded a", h, "to copy", d, "of", e)

            Repeat

        case CureAllReturnAction(f, h, e, i) =>
            h.cures -= 1

            e.forTrade = e.forTrade.diff($(i))

            game.uncrafted :+= i.item

            f.log("discarded a", h, "to return", i.item, "of", e, "to the supply")

            Repeat

        case CleanUpAction(f) if options.has(DarkDeck).not && factions.exists(_.dopple.any) =>
            factions.foreach(_.dopple = $)

            CleanUpAction(f)

        // PIGS
        case GruntsMainAction(f, h, again) =>
            val pay = clearings.%(f.present).%(h.canPlace).%(c => f.hand.exists(_.matches(c.cost)))

            Ask(f).group(h)
                .each(pay)(c => GruntsPayAction(f, h, c).!(h.pool(Grunt).not, "no warriors"))
                .add(GruntsMoveBattleAction(f, h).as("Move".styled(h), dt.Move, "then", "Battle".styled(h), dt.Battle)(h).!(h.moveFrom.none && clearings.%(h.canAttackIn).none, "can't move or battle"))
                .when(again)(Repeat.as("Done"))
                .when(again.not)(CancelAction)

        case GruntsPayAction(f, h, c) =>
            YYSelectObjectsAction(f, f.hand)
                .withGroup(h.elem ~ " places two " ~ Grunt.sof(h) ~ " in " ~ c.elem)
                .withRule(_.matches(c.cost))
                .withThen(d => GruntsPlaceAction(f, h, d, c))(d => game.desc("Spend", d))("Spend")
                .withExtra($(NoHand, CancelAction))

        case GruntsPlaceAction(f, h, d, c) =>
            f.used :+= h

            f.hand --> d --> discard.quiet

            h.reserve --> $(Grunt, Grunt).take(h.pooled(Grunt)) --> c

            h.log("came to", c, "for", d)

            GruntsAgainAction(f, h)

        case GruntsMoveBattleAction(f, h) =>
            f.used :+= h

            TurnMoveBattleAction(f, h, GruntsAgainAction(f, h))

        case GruntsAgainAction(f, h) if f.used.has(GruntsUsed) =>
            Repeat

        case GruntsAgainAction(f, h) =>
            f.used :+= GruntsUsed

            val cards = f.hand.%(d => clearings.%(h.present).exists(c => d.matches(c.cost)))

            if (cards.none)
                Repeat
            else
                YYSelectObjectsAction(f, f.hand)
                    .withGroup(h.elem ~ " can go again for a card matching their clearing")
                    .withRule(d => cards.has(d))
                    .withThen(d => GruntsAgainPaidAction(f, h, d))(d => game.desc("Spend", d))("Spend")
                    .withExtra($(NoHand, Repeat.as("Skip")))

        case GruntsAgainPaidAction(f, h, d) =>
            f.hand --> d --> discard.quiet

            f.log("spent", d, "for", h)

            Ask(f).add(GruntsMainAction(f, h, true))

        case GruntsHireMainAction(f, h) =>
            val l = clearings.%(h.present).%(f.present).%(c => f.hand.exists(_.matches(c.cost)))

            Ask(f).group(h, "as your warriors")
                .each(l)(c => GruntsHirePayAction(f, h, c))
                .cancel

        case GruntsHirePayAction(f, h, c) =>
            YYSelectObjectsAction(f, f.hand)
                .withGroup(h.elem ~ " in " ~ c.elem ~ " as your warriors")
                .withRule(_.matches(c.cost))
                .withThen(d => GruntsHireAction(f, h, d, c))(d => game.desc("Spend", d))("Spend")
                .withExtra($(NoHand, CancelAction))

        case GruntsHireAction(f, h, d, c) =>
            f.used :+= GruntsUsed

            f.hand --> d --> discard.quiet

            h.user = |(f)

            f.log("spent", d, "to lead", Grunt.sof(h), "in", c)

            val battle = GruntsHireBattleAction(f, h, c)

            MoveInitAction(f, h, game.transports./($) ** $($(GruntRule)), NoMessage, $(c), $(c), $(battle.as("Skip", "Move")), battle)

        case MoveFinishedAction(f, from, to : Clearing, GruntsHireBattleAction(ff, h, c)) if c != to =>
            MoveFinishedAction(f, from, to, GruntsHireBattleAction(ff, h, to))

        case GruntsHireBattleAction(f, h, c) =>
            val done = GruntsHireDoneAction(f, h, Repeat)

            if (h.canAttackIn(c))
                BattleInitAction(f, h, NoMessage, $(c), $(done.as("Skip", "Battle")), done)
            else
                done

        case GruntsHireDoneAction(f, h, then) =>
            h.user = None

            then

        case SwineMainAction(f, h) =>
            val l = clearings.%(h.present)./~(c => (factions ++ hirelings).%(game.states.contains).%(e => f.friends(e).not)./~(e => e.at(c).%(p => p.is[Building] || p.is[Token]).distinct./(p => (c, e, p))))
                .%{ case (c, e, p) => f.hand.count(_.matches(c.cost)) >= e.at(c).%(p => p.is[Building] || p.is[Token]).num }
                .%{ case (c, e, p) => f.canRemove(c)(e) }

            Ask(f).group(h)
                .each(l)({ case (c, e, p) => SwineBuyAction(f, h, c, e, p) })
                .add(SwineMoveAction(f, h))
                .cancel

        case SwineBuyAction(f, h, c, e, p) =>
            val n = e.at(c).%(p => p.is[Building] || p.is[Token]).num

            XXSelectObjectsAction(f, f.hand)
                .withGroup(h.elem ~ " forecloses " ~ p.of(e) ~ " in " ~ c.elem ~ " for " ~ n.cards)
                .withRule(_.num(n).each(_.matches(c.cost)))
                .withThen(l => SwineBuyPaidAction(f, h, c, e, p, l))(l => "Spend".hl ~ l./(" " ~ _.elem))
                .withExtra($(NoHand, CancelAction))

        case SwineBuyPaidAction(f, h, c, e, p, l) =>
            f.used :+= h

            f.hand --> l --> discard.quiet

            f.log("spent", l, "with", h)

            ForcedRemoveAction(f, c, e, p, p.is[Scoring].??(1), Removing, ForcedRemoveFinishedAction(f, Repeat))

        case SwineMoveAction(f, h) =>
            MoveInitAction(f, h, game.transports./($) ** $($(SingleFigure)), NoMessage, h.moveFrom, h.movable, $(CancelAction), SwineSecondMoveAction(f, h))

        case SwineSecondMoveAction(f, h) =>
            f.used :+= h

            if (h.moveFrom.any)
                MoveInitAction(f, h, game.transports./($) ** $($(SingleFigure)), NoMessage, h.moveFrom, h.movable, $(Repeat.as("Skip")), Repeat)
            else
                Repeat

        // the Swine deal no hits, and a hit on one moves it
        case BattleAdjustRollAction(b, f, o, fs, os, fr, or) if os > 0 && o == FineSwine =>
            BattleAdjustRollAction(b, f, o, fs, 0, fr, or)

        case ForcedRemoveTargetEffectAction(e, c, f : FineSwine.type, Swine, then) =>
            val l = game.connected(c)

            if (l.none) {
                f.limbo(c) --> Swine --> c

                then
            }
            else
                Ask(e @@ {
                    case x : Hireling => x.owner.|(game.current)
                    case x => x
                }).group(f, "shoves a", Swine.of(f)).each(l)(d => SwineShoveAction(e, f, c, d, then))

        case SwineShoveAction(e, h, c, d, then) =>
            h.limbo(c) --> Swine --> d

            h.log("was shoved from", c, "to", d)

            then

        case _ => UnknownContinue
    }
}
