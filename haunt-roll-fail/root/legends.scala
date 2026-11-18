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
import hrf.meta._
import hrf.options._
import root.elem._

import helper.&

// The Doomed Expansion's Legends Scenarios (slugfacekillah, Law of Doom D5): a thirteenth "clearing", the scenario space, in a forest,
// with each scenario's own rules. The scenario is a setup choice (`LegendsScenario` in `root/meta.scala`).
//
// Readings (the rules leave these open):
// - The space goes in a forest drawn at random among those beside the most non-edge clearings (the board's inner ones), and is
//   linked by its trails to those of them; Castles & Catacombs links it to the four corners too. A player may always leave it by a
//   catacomb, as after the scenario's battles only one player has pieces there; out of a catacomb, to any corner (the simpler choice)
// - No Scoring: the space has no building slots, the Vagabond scores nothing for the Fifth Ruin, and in Castles & Catacombs (and Dark
//   Rituals) any piece there that isn't a warrior or a pawn goes back to its supply at the end of the action; other scoring there
//   (a quest, a plot, a token in Hunt the Legend) isn't stopped
// - The scenario battles start before the next action once two players have warriors or pawns there, the one already there
//   defending, and go on until one of them has none left (a Vagabond with no undamaged item goes to the forest the space is in); the
//   one left then removes its warriors beyond three
// - The Fifth Ruin: the space's ruin is placed with the others, and when setup is done without a Vagabond, it goes to the clearing with
//   the most open slots and no ruin (the first of the board's clearings among them) and the space is taken away
// - The Ring: drawn as an item like the others; a Vagabond exhausts it as an item, any other bearer once per turn. Its rule is picked
//   (or drawn) from the eight ringbearer and exhaust rules, and the capture rule from the four (or Fat Fingers, no capture), Fat
//   Fingers going with the two rules that take no capture rule (Bonafied Prince, Ring of Rage). "Your Rule" isn't offered
// - Friendship Ring: the Vagabond moves a relationship marker one space towards Allied without scoring; any other bearer makes one
//   enemy a friend until the end of its turn (everywhere: it can't battle or remove that enemy's pieces, nor they its own)
// - Bonafied Prince: a building of the bearer's from its supply, in a clearing it rules with an open slot, scoring nothing; the
//   Vagabond completes one of the quests of its clearing's suit with two cards of that suit (birds too) instead of items
// - The Precious: the bearer's warriors swapped off the map wait on its board until swapped back where the Ring is, one swap a turn;
//   the Ring on the map moves one clearing a turn for free (the Vagabond's pawn is never swapped)
// - Power of the Schwartz: enemy warriors and pieces go to an adjacent clearing, ignoring rule
// - Seizure & Surrender: three hits dealt to the bearer in battle in one turn (removing a building to capture isn't done)
// - Pickpockets: in Daylight, at a clearing where the player and the bearer both have pieces, once a turn

case object LegendSpace extends Clearing(0) {
    override def name = "Scenario Space"
}

// The Precious: the bearer's warriors swapped off the map
case object RingBox extends SpecialRegion

// The Ring on the map (The Precious)
case object RingMarker extends Token

// a Vagabond's Ring is exhausted as an item; any other bearer's once per turn
case object RingExhausted extends Effect
case object RingPrecious extends Effect
case object RingMoved extends Effect
case object RingSchwartz extends Effect
case object RingReturn extends Effect
case object PickpocketsUsed extends Effect
case object CaptureDeclared extends Effect


case class LegendsSpaceAction(r : Forest, then : ForcedAction) extends RandomAction[Forest] { def random = r }
case class LegendsClashAction(then : ForcedAction) extends ForcedAction
case class LegendsClashDoneAction(then : ForcedAction) extends ForcedAction
case class FifthRuinPoolAction(then : ForcedAction) extends ForcedAction
case class FifthRuinRareAction(shuffled : $[Item], fifth : Boolean, then : ForcedAction) extends ShuffledAction[Item]
case class FifthRuinFillAction(shuffled : $[Item], ruins : $[Clearing], then : ForcedAction) extends ShuffledAction[Item]
case class RingRulesDrawnAction(shuffled : $[RingRule], then : ForcedAction) extends ShuffledAction[RingRule]
case class CaptureRulesDrawnAction(shuffled : $[CaptureRule], then : ForcedAction) extends ShuffledAction[CaptureRule]

case class RallyRoundAction(self : Faction) extends BaseAction("Ring".styled(styles.itemText))("Rally Round!".hh, Ring.img)
case class FriendshipRingAction(self : Faction) extends BaseAction("Ring".styled(styles.itemText))("Friendship Ring".hh, Ring.img) with Soft
case class FriendshipAttitudeAction(self : Hero, e : Faction) extends BaseAction("Friendship Ring".hh)("Improve", e, "relationship")
case class FriendshipEnemyAction(self : Faction, e : Faction) extends BaseAction("Friendship Ring".hh)("Befriend", e)
case class BonafiedPrinceAction(self : Faction) extends BaseAction("Ring".styled(styles.itemText))("Bonafied Prince".hh, Ring.img) with Soft
case class PrinceBuildAction(self : Faction, p : Piece, c : Clearing) extends BaseAction("Bonafied Prince".hh, "builds")(p.of(self), p.imgd(self), "in", c)
case class PrinceQuestAction(self : Hero, q : Quest) extends BaseAction("Bonafied Prince".hh, "completes a quest")(q.img) with Soft
case class PrinceQuestPaidAction(self : Hero, q : Quest, l : $[DeckCard]) extends ForcedAction
case class MerchandisingAction(self : Faction) extends BaseAction("Ring".styled(styles.itemText))("Merchandising".hh, Ring.img) with Soft
case class MerchandisingClearingAction(self : Faction, c : Clearing) extends BaseAction("Merchandising".hh, "in")(c)
case class MerchandisingPullAction(self : Faction, c : Clearing, rest : $[Clearing], n : Int) extends ForcedAction
case class MerchandisingMoveAction(self : Faction, c : Clearing, o : Clearing, e : Faction, p : Piece, rest : $[Clearing], n : Int) extends BaseAction("Merchandising".hh, "from", o)(p.of(e), LegendsExpansion.pimg(p, e))
case class PreciousMainAction(self : Faction) extends BaseAction("Ring".styled(styles.itemText))("The Precious".hh, Ring.img) with Soft
case class PreciousSwapOffAction(self : Faction, c : Clearing, n : Int) extends BaseAction("The Precious".hh, "swaps the", "Ring".styled(styles.itemText), "with warriors in")(c, "(" ~ n.hl ~ ")")
case class PreciousSwapOnAction(self : Faction, n : Int) extends BaseAction("The Precious".hh)("Swap the", "Ring".styled(styles.itemText), "back for", n.hl, "warriors")
case class PreciousMoveAction(self : Faction, c : Clearing) extends BaseAction("The Precious".hh, "moves the", "Ring".styled(styles.itemText), "to")(c)
case class ReturnKingMainAction(self : Faction) extends BaseAction("Ring".styled(styles.itemText))("Return of the King".hh, Ring.img) with Soft
case class ReturnKingAction(self : Faction, c : Clearing, e : Faction, p : Piece) extends BaseAction("Return of the King".hh, "removes")(p.of(e), LegendsExpansion.pimg(p, e), "in", c)
case class ReturnKingRewardAction(self : Faction, c : Clearing) extends ForcedAction
case class ReturnKingPlaceAction(self : WarriorFaction, c : Clearing) extends BaseAction("Return of the King".hh)("Place", self.warrior.of(self), "in", c)
case class ReturnKingDrawAction(self : Faction) extends BaseAction("Return of the King".hh)("Draw", 1.hl, "card")
case class SchwartzMainAction(self : Faction) extends BaseAction("Ring".styled(styles.itemText))("Power of the Schwartz".hh, Ring.img) with Soft
case class SchwartzWarriorsAction(self : Faction, c : Clearing, e : WarriorFaction, n : Int) extends BaseAction("Power of the Schwartz".hh, "in", c)("Move", n.hl, e.warrior.sof(e), e.warrior.imgd(e)) with Soft
case class SchwartzPieceAction(self : Faction, c : Clearing, e : Faction, p : Piece) extends BaseAction("Power of the Schwartz".hh, "in", c)("Move", p.of(e), LegendsExpansion.pimg(p, e)) with Soft
case class SchwartzWarriorsToAction(self : Faction, c : Clearing, e : WarriorFaction, n : Int, d : Clearing) extends BaseAction("Power of the Schwartz".hh, "moves", n.hl, e.warrior.sof(e), "to")(d)
case class SchwartzPieceToAction(self : Faction, c : Clearing, e : Faction, p : Piece, d : Clearing) extends BaseAction("Power of the Schwartz".hh, "moves", p.of(e), "to")(d)

case class CaptureDeclareAction(self : Faction, b : Battle) extends BaseAction("Ring".styled(styles.itemText), "capture")("Capture", Ring.img, "(deal no hits)")
case class CaptureDeclineAction(self : Faction, b : Battle) extends BaseAction(None)("Battle")
case class CaptureItemAction(self : Faction, e : Faction, i : Item, then : ForcedAction) extends BaseAction("Capture from", e)(i.img, i)
case class FilchMoveAction(self : Faction, b : Battle) extends ForcedAction
case class FilchMovedAction(self : Faction, e : Faction, then : ForcedAction) extends ForcedAction
case class PickpocketsMainAction(self : Faction) extends BaseAction("Ring".styled(styles.itemText))("Pickpockets".hh, Ring.img) with Soft
case class PickpocketsClearingAction(self : Faction, c : Clearing, e : Faction) extends BaseAction("Pickpockets".hh, "at")(c, "(" ~ e.elem ~ ")") with Soft
case class PickpocketsRevealAction(self : Faction, c : Clearing, e : Faction, l : $[DeckCard]) extends ForcedAction
case class PickpocketsRolledAction(self : Faction, c : Clearing, e : Faction, l : $[DeckCard], x : Int) extends RolledAction[Int] { def rolled = $(x) }


// the scenario chosen in setup
trait LegendsScenario extends GameOption with ToggleOption with OneOfGroup {
    val group = "Legends Scenario".hh
    override def folder = Meta.scenarios
    def title : String
    def suited : Boolean
    val valueOn = title.hh
}

case object CastlesAndCatacombs extends LegendsScenario {
    def title = "Castles & Catacombs"
    def suited = true
    override val explain = $(
        "Castles & Catacombs".hh ~ SpacedDash ~ "a " ~ "Legends Scenario".hh ~ " from " ~ "The Doomed Expansion".hl ~ " by " ~ "slugfacekillah".hl ~ ":",
        "a thirteenth " ~ "clearing".hh ~ ", the " ~ "scenario space".hh ~ &(" of every suit, in a forest,") ~ &(" linked to its clearings off the edge") ~ &(" and to the four corners."),
        "Only warriors and pawns can be there;" ~ &(" players arriving where another is battle") ~ &(" until one is gone,") ~ &(" who keeps at most three."),
        FanInfo.doomedLink,
    )
}

case object TheFifthRuin extends LegendsScenario {
    def title = "The Fifth Ruin"
    def suited = false
    override def required(all : $[BaseOption]) = $($(IncludeFaction(VB)), $(IncludeFaction(NB)), $(IncludeFaction(MB)), $(IncludeFaction(LH)))
    override val explain = $(
        "The Fifth Ruin".hh ~ SpacedDash ~ "a " ~ "Legends Scenario".hh ~ " from " ~ "The Doomed Expansion".hl ~ " by " ~ "slugfacekillah".hl ~ ":",
        "a fifth ruin in the " ~ "scenario space".hh ~ &(" only the Vagabond can enter,") ~ &(" with a rare item: ") ~ &("Hammer, Tea, Crossbow, Coins or the " ~ "Ring".hh ~ "."),
        "The " ~ "Ring".hh ~ " has a rule of its own and a way to capture it, " ~ &("picked below or drawn."),
        FanInfo.doomedLink,
    )
}

// the Ring's rules (D5.R): ringbearer and exhaust rules, then the capture rules
trait RingRule extends GameOption with ToggleOption with OneOfGroup {
    val group = "Ring Rule".hh
    override def folder = Meta.scenarios
    def title : String
    def text : String
    val valueOn = title.hh
    override def required(all : $[BaseOption]) = $($(TheFifthRuin))
    override val explain = $(title.hh ~ SpacedDash ~ text, "None picked: one is drawn at random.")
    def noCapture = false
}

case object RallyRound extends RingRule {
    def title = "Rally Round!"
    def text = "the bearer may exhaust the Ring in Daylight to discard their hand and draw five cards."
}

case object FriendshipRing extends RingRule {
    def title = "Friendship Ring"
    def text = "the bearer may exhaust the Ring in Evening: a Vagabond moves a relationship marker one space towards Allied (no points), anyone else treats one enemy as a friend until the end of the turn."
}

case object BonafiedPrince extends RingRule {
    def title = "Bonafied Prince"
    def text = "the bearer may exhaust the Ring in Daylight to build once at no cost, or, as Vagabond, to complete a quest with two cards of its suit instead of items. No capture rule."
    override def noCapture = true
}

case object Merchandising extends RingRule {
    def title = "Merchandising"
    def text = "the bearer may exhaust the Ring in Daylight to pick a clearing with their pieces and pull one enemy warrior or pawn into it from each adjacent clearing; two or more score 1 point."
}

case object ThePrecious extends RingRule {
    def title = "The Precious"
    def text = "on their turn the bearer may lose a point to swap up to three of their warriors on the map with the Ring, or back; the Ring on the map moves one clearing a turn, ignoring rule."
}

case object RingOfRage extends RingRule {
    def title = "Ring of Rage"
    def text = "in the bearer's battles, each die rolls one more (0-3 becomes 1-4). No capture rule."
    override def noCapture = true
}

case object ReturnOfTheKing extends RingRule {
    def title = "Return of the King"
    def text = "once in Evening the bearer may remove an enemy warrior where they have pieces, then place a warrior there or draw a card."
}

case object PowerOfTheSchwartz extends RingRule {
    def title = "Power of the Schwartz"
    def text = "once a turn, from a clearing with their pieces, the bearer may move up to half (rounded up) of an enemy's warriors, or one enemy token or building, to an adjacent clearing, ignoring rule."
}

trait CaptureRule extends GameOption with ToggleOption with OneOfGroup {
    val group = "Capture Rule".hh
    override def folder = Meta.scenarios
    def title : String
    def text : String
    val valueOn = title.hh
    override def required(all : $[BaseOption]) = $($(TheFifthRuin), $(DarkRituals), $(HuntTheLegend))
    override val explain = $(title.hh ~ SpacedDash ~ text, "None picked: one is drawn at random (no capture with Bonafied Prince or Ring of Rage).")
}

case object FatFingers extends CaptureRule {
    def title = "Fat Fingers"
    def text = "no capture: the first bearer keeps the Ring."
}

case object LilLooters extends CaptureRule {
    def title = "Lil' Looters"
    def text = "attacking the bearer, a player may declare a capture and deal no hits; ruling the clearing after the battle (or the bearer gone), they take the Ring."
}

case object FiveFingerFilch extends CaptureRule {
    def title = "5-Finger Filch"
    def text = "attacking the bearer, a player may declare a capture and deal no hits; moving out of the clearing after the battle, they take the Ring."
}

case object SeizureAndSurrender extends CaptureRule {
    def title = "Seizure & Surrender"
    def text = "a player dealing three hits to the bearer in one turn takes the Ring."
}

case object Pickpockets extends CaptureRule {
    def title = "Pickpockets"
    def text = "once a turn in Daylight, at a clearing with their pieces and the bearer's, a player reveals cards of its suit and rolls a die: that many are discarded (birds first), and with any left they take the Ring."
}


object LegendsExpansion extends Expansion {
    def active(setup : $[Faction], options : $[Meta.O]) = options.of[LegendsScenario].any

    def on(options : $[Meta.O]) = options.of[LegendsScenario].any

    def placed(implicit game : Game) = game.legendForest.any

    def restricted(implicit game : Game) = options.has(CastlesAndCatacombs) || options.has(DarkRituals)

    // the board's clearings off the edge linked by trails, and the corners by catacombs
    def trails(implicit game : Game) : $[Clearing] = game.legendForest./~(r => game.fromForest(r).intersect(board.inner))

    def links(implicit game : Game) : $[Clearing] = (trails ++ options.has(CastlesAndCatacombs).??(board.corners).intersect(game.clearings)).distinct

    // the space's connections for everything but movement, which only the Vagabond uses in the Fifth Ruin
    def connected(c : Clearing)(implicit game : Game) : $[Clearing] =
        if (placed.not || options.has(TheFifthRuin))
            $
        else
        if (c == LegendSpace)
            links
        else
            links.has(c).$(LegendSpace)

    // movement along a trail or catacomb (Roads)
    def linked(f : Faction, o : Clearing, d : Clearing)(implicit game : Game) : Boolean = placed && (o == LegendSpace || d == LegendSpace) && {
        val other = (o == LegendSpace).?(d).|(o)
        links.has(other) && (options.has(TheFifthRuin).not || (f.is[Hero] && (d != LegendSpace || factions.of[Hero].%(_ != f).forall(_.at(LegendSpace).none))))
    }

    def canPlace(f : Faction, c : Clearing)(implicit game : Game) = c != LegendSpace || options.has(TheFifthRuin).not

    // the warriors and pawns in the space, and the other pieces
    def holders(implicit game : Game) : $[Faction] = (factions ++ hirelings).but(LegendBeast).%(game.states.contains).%(_.at(LegendSpace).exists(p => p.is[Warrior] || p.is[Pawn]))

    def litter(implicit game : Game) : $[(Faction, Piece)] = (factions ++ hirelings).but(LegendBeast).%(game.states.contains)./~(f => f.at(LegendSpace).%(p => p.is[Warrior].not && p.is[Pawn].not)./(f -> _))

    def excess(f : Faction)(implicit game : Game) = f.at(LegendSpace).of[Warrior].num - max(0, 3 - f.at(LegendSpace).of[Pawn].num)

    def helpless(f : Faction)(implicit game : Game) = f.as[Hero].?(_.inv.notDamaged.%(_.item != CursedIdol).none) && f.at(LegendSpace).of[Warrior].none

    def clashPending(implicit game : Game) = placed && restricted && game.legendsBusy.not && game.turn > 0 && (
        litter.any || holders.num > 1 || holders.exists(f => excess(f) > 0)
    )

    // THE RING
    def ringRule(implicit game : Game) : |[RingRule] = game.ringRule
    def captureRule(implicit game : Game) : |[CaptureRule] = game.captureRule

    def items(f : Faction)(implicit game : Game) : $[Item] = f @@ {
        case f : Hero => f.inv./(_.item)
        case f : Horde => f.hoard.command ++ f.hoard.prowess
        case f => f.forTrade./(_.item)
    }

    // the player with the Ring, or whose Ring is on the map (The Precious)
    def bearer(implicit game : Game) : |[Faction] = factions.%(game.states.contains).%(f => items(f).has(Ring)).starting || game.ringAt.any.??(game.ringOwner)

    def bears(f : Faction)(implicit game : Game) = options.has(TheFifthRuin) && bearer.has(f)

    def ringReady(f : Faction)(implicit game : Game) = bears(f) && game.ringAt.none && (f @@ {
        case f : Hero => f.inv.exists(r => r.item == Ring && r.exhausted.not && r.damaged.not)
        case f => f.used.has(RingExhausted).not
    })

    def exhaustRing(f : Faction)(implicit game : Game) {
        f @@ {
            case f : Hero =>
                val r = f.inv.%(r => r.item == Ring && r.exhausted.not && r.damaged.not).first
                f.inv = f.inv.diff($(r)) :+ r.exhaust
            case f =>
                f.used :+= RingExhausted
        }
    }

    def takeItem(e : Faction, i : Item)(implicit game : Game) {
        e @@ {
            case e : Hero =>
                val r = e.inv.%(_.item == i).sortBy(r => r.damaged.??(2) + r.exhausted.??(1)).first
                e.inv = e.inv.diff($(r))
            case e : Horde =>
                if (e.hoard.command.has(i))
                    e.hoard.command = e.hoard.command.diff($(i))
                else
                    e.hoard.prowess = e.hoard.prowess.diff($(i))
            case e =>
                e.forTrade = e.forTrade.diff($(e.forTrade.%(_.item == i).first))
        }
    }

    // the items a capture can take from a player (only the Ring in the Fifth Ruin, any with the monsters)
    def capturable(e : Faction)(implicit game : Game) : $[Item] =
        if (MonstersExpansion.on(options))
            game.states.contains(e).??(items(e).distinct.but(CursedIdol))
        else
            items(e).%(_ == Ring).take(1)

    def captures(implicit game : Game) = options.has(TheFifthRuin) || MonstersExpansion.on(options)

    // whom a capture can be tried on
    def victim(e : Faction)(implicit game : Game) = MonstersExpansion.on(options).?(capturable(e).any).|(bearer.has(e))

    def prize(implicit game : Game) : Elem = MonstersExpansion.on(options).?("an item".txt).|("the " ~ Ring.elem)

    def capture(f : Faction, e : Faction, then : ForcedAction)(implicit game : Game) : Continue = {
        val l = capturable(e)

        if (l.none) {
            f.log("found nothing to capture from", e)
            then
        }
        else
            Ask(f).group(f, "captures from", e).each(l.distinct)(i => CaptureItemAction(f, e, i, then))
    }

    def friendly(a : Faction, b : Faction)(implicit game : Game) = game.ringFriends.has((a, b)) || game.ringFriends.has((b, a))

    def adjacent(c : Clearing)(implicit game : Game) = game.connected(c)

    // a piece's picture in a choice (pawns have no small one)
    def pimg(p : Piece, f : Faction) : Elem = p match {
        case _ : Pawn => p.img(f)
        case _ => p.imgd(f)
    }

    override def birdsong(f : Faction)(implicit game : Game, ask : ActionCollector) {
        anytime(f)
    }

    override def daylight(f : Faction)(implicit game : Game, ask : ActionCollector) {
        if (bears(f)) {
            if (ringRule.has(RallyRound))
                + RallyRoundAction(f).!(ringReady(f).not, "exhausted")

            if (ringRule.has(BonafiedPrince))
                + BonafiedPrinceAction(f).!(ringReady(f).not, "exhausted").!(princeBuilds(f).none && princeQuests(f).none, "nothing to build")

            if (ringRule.has(Merchandising))
                + MerchandisingAction(f).!(ringReady(f).not, "exhausted").!(merchandise(f).none, "no clearing")
        }

        if (captureRule.has(Pickpockets) && captures && f.used.has(PickpocketsUsed).not && pickpocketClearings(f).any)
            + PickpocketsMainAction(f)

        anytime(f)
    }

    override def evening(f : Faction)(implicit game : Game, ask : ActionCollector) {
        if (bears(f)) {
            if (ringRule.has(FriendshipRing))
                + FriendshipRingAction(f).!(ringReady(f).not, "exhausted").!(friendshipTargets(f).none, "no enemies")

            if (ringRule.has(ReturnOfTheKing) && f.used.has(RingReturn).not)
                + ReturnKingMainAction(f).!(returnTargets(f).none, "no enemy warriors")
        }

        anytime(f)
    }

    def anytime(f : Faction)(implicit game : Game, ask : ActionCollector) {
        if (bears(f)) {
            if (ringRule.has(ThePrecious))
                + PreciousMainAction(f).!(preciousOptions(f).none, "nothing to do")

            if (ringRule.has(PowerOfTheSchwartz) && f.used.has(RingSchwartz).not)
                + SchwartzMainAction(f).!(schwartz(f).none, "no enemies")
        }
    }

    def princeBuilds(f : Faction)(implicit game : Game) : $[(Piece, Clearing)] =
        f.reserve.$./(_.piece).of[Building].distinct./~(p => clearings.but(LegendSpace).%(f.rules).%(f.canBuild).%(c => p @@ {
            case Garden(s) => c.cost.matched(s)
            case Base(s) => c.cost.matched(s)
            case _ => true
        })./(c => (p : Piece) -> c))

    def princeQuests(f : Faction)(implicit game : Game) : $[Quest] = f.as[Hero]./~(h => h.region.as[Clearing].but(LegendSpace)./~(c => game.quests.take(3).%(_.suit.matches(c.cost)).%(q => h.hand.%(_.matches(q.suit.as[BaseSuit]./(s => s : SuitCost).|(AnySuit))).num >= 2)))

    def merchandise(f : Faction)(implicit game : Game) : $[Clearing] = clearings.%(f.present).%(c => adjacent(c).exists(o => pullable(f, o).any))

    def pullable(f : Faction, o : Clearing)(implicit game : Game) : $[(Faction, Piece)] =
        (factions ++ hirelings).%(game.states.contains).%(e => f.friends(e).not && friendly(f, e).not)./~(e => e.at(o).%(p => p.is[Warrior] || (p.is[Pawn] && e.is[Hero]))./(e -> _)).distinct

    def friendshipTargets(f : Faction)(implicit game : Game) : $[Faction] = f @@ {
        case f : Hero => factions.but(f).%(e => f.attitude.get(e).has(Allied).not)
        case f => factions.but(f).%(e => f.friends(e).not && friendly(f, e).not)
    }

    def returnTargets(f : Faction)(implicit game : Game) : $[(Clearing, Faction, Piece)] =
        clearings.%(f.present)./~(c => f.enemies.%(e => friendly(f, e).not).%(f.canRemove(c)).%(e => e.at(c).of[Warrior].any)./(e => (c, e, e.at(c).of[Warrior].first : Piece)))

    def preciousOptions(f : Faction)(implicit game : Game) : $[UserAction] =
        if (game.ringAt.none)
            f.used.has(RingPrecious).not.??(clearings.but(LegendSpace).%(c => f.at(c).of[Warrior].any)./(c => PreciousSwapOffAction(f, c, min(3, f.at(c).of[Warrior].num))))
        else
            (f.at(RingBox).any && f.used.has(RingPrecious).not).$(PreciousSwapOnAction(f, f.at(RingBox).num)) ++
            f.used.has(RingMoved).not.??(adjacent(game.ringAt.get).but(LegendSpace)./(c => PreciousMoveAction(f, c)))

    def schwartz(f : Faction)(implicit game : Game) : $[(Clearing, Faction)] =
        clearings.%(f.present).%(c => adjacent(c).but(LegendSpace).any)./~(c => f.enemies.%(e => friendly(f, e).not).%(e => e.at(c).exists(p => p.is[Warrior] || ((p.is[Building] || p.is[Token]) && movableBuilding(p))))./(e => c -> e))

    def movableBuilding(p : Piece) = p != Keep && p.is[Ruins].not

    def pickpocketClearings(f : Faction)(implicit game : Game) : $[(Clearing, Faction)] = factions.but(f).%(game.states.contains).%(victim)./~(b => clearings.%(b.present).%(f.present).but(LegendSpace).%(c => f.hand.exists(_.matches(c.cost)))./(c => c -> b))

    def perform(action : Action, soft : Void)(implicit game : Game) = action @@ {
        // THE SCENARIO BATTLES: before the next action
        case a if a.isSoft.not && clashPending && a.is[LegendsClashAction].not =>
            val then : ForcedAction = a match {
                case a : ForcedAction => a
                case a : Choice => ForceAction(a)
                case a => throw new Error("legends clash before " + a)
            }

            LegendsClashAction(then)

        case LegendsClashAction(then) if litter.any =>
            litter.foreach { case (f, p) =>
                f.from(LegendSpace) --> p --> f.reserve

                f.log("returned", p.of(f), "from the", LegendSpace, "to the supply")
            }

            LegendsClashAction(then)

        case LegendsClashAction(then) if holders.exists(helpless) && holders.num > 1 =>
            holders.%(helpless).foreach { f =>
                f.from(LegendSpace) --> f.at(LegendSpace).of[Pawn] --> game.legendForest.get

                f.log("left the", LegendSpace, "for", game.legendForest.get)
            }

            LegendsClashAction(then)

        case LegendsClashAction(then) if holders.num > 1 =>
            val l = holders
            val arrived = l.diff(game.legendHolders)
            val stayed = l.intersect(game.legendHolders)

            val pairs =
                arrived./~(a => stayed./(d => (a, d))) ++
                l.contains(game.current).??(l.but(game.current)./(d => (game.current, d))) ++
                l./~(a => l.but(a)./(d => (a, d)))

            (pairs.%{ case (a, d) => a.validAttackTarget(LegendSpace)(d) }.starting : |[(Faction, Faction)]) @@ {
                case Some((a, d)) =>
                    game.legendsBusy = true

                    a.log("battled", d, "in the", LegendSpace)

                    BattleStartAction(a, a, a, NoMessage, LegendSpace, d, None, LegendsClashDoneAction(then)) : Continue

                case None =>
                    // nobody there can battle the others: they share the space
                    game.legendHolders = l

                    then : Continue
            }

        case LegendsClashAction(then) if holders.exists(f => excess(f) > 0) =>
            holders.%(f => excess(f) > 0).foreach { f =>
                val n = excess(f)

                f.from(LegendSpace) --> f.at(LegendSpace).of[Warrior].take(n) --> f.reserve

                f.log("removed", n.hl, "warriors beyond three from the", LegendSpace)
            }

            LegendsClashAction(then)

        case LegendsClashAction(then) =>
            game.legendHolders = holders

            then

        case LegendsClashDoneAction(then) =>
            game.legendsBusy = false

            then

        // SETUP: the space goes in a forest by the most clearings off the edge, after the map and the landmarks, or in the
        // forest the map prints a place for (the Legends map's stone circle)
        case SetupHirelingsAction if game.legendForest.none && game.legendDone.not =>
            val ff = board.forests.%(r => game.fromForest(r).intersect(board.inner).any)
            val best = ff.%(r => game.fromForest(r).intersect(board.inner).num == ff./(r => game.fromForest(r).intersect(board.inner).num).maxOr(0))

            board.scenarioForest match {
                case Some(f) => Force(LegendsSpaceAction(f, SetupHirelingsAction))
                case None => Random[Forest](best.some.|(board.forests), LegendsSpaceAction(_, SetupHirelingsAction))
            }

        case LegendsSpaceAction(r, then) =>
            game.legendForest = |(r)
            game.legendDone = true

            val s = options.of[LegendsScenario].first

            game.mapping += LegendSpace -> s.suited.??(FoxRabbitMouse)
            game.original += LegendSpace -> s.suited.??(FoxRabbitMouse)

            if (s.suited.not)
                game.unsuited :+= LegendSpace

            game.graveyard += LegendSpace -> $

            if (options.has(TheFifthRuin))
                game.ruins += LegendSpace -> Ruins($)

            log("The", LegendSpace, "of", s.title.hh, "was placed in", r, "linked to", links./(_.elem).commaAnd)

            if (options.has(TheFifthRuin))
                Shuffle[RingRule]($(RallyRound, FriendshipRing, BonafiedPrince, Merchandising, ThePrecious, RingOfRage, ReturnOfTheKing, PowerOfTheSchwartz), RingRulesDrawnAction(_, then))
            else
            if (MonstersExpansion.on(options))
                Shuffle[CaptureRule]($(LilLooters, FiveFingerFilch, SeizureAndSurrender, Pickpockets), CaptureRulesDrawnAction(_, MonsterSetupAction(then)))
            else
                then

        case RingRulesDrawnAction(l, then) =>
            game.ringRule = options.of[RingRule].single || l.starting

            log("The", Ring.elem, "has the rule", game.ringRule.get.title.hh ~ ":", game.ringRule.get.text)

            Shuffle[CaptureRule]($(LilLooters, FiveFingerFilch, SeizureAndSurrender, Pickpockets), CaptureRulesDrawnAction(_, then))

        case CaptureRulesDrawnAction(l, then) =>
            game.captureRule = game.ringRule.exists(_.noCapture).?(FatFingers).||(options.of[CaptureRule].single).||(l.starting)

            log("The capture rule is", game.captureRule.get.title.hh ~ ":", game.captureRule.get.text)

            then

        // THE FIFTH RUIN: the pool of rare items, once every faction is set up
        case AddRuinItemsAction(then) if options.has(TheFifthRuin) && game.legendPool.not =>
            game.legendPool = true

            FifthRuinPoolAction(then)

        case FifthRuinPoolAction(then) =>
            val vb = factions.of[Hero].%(game.states.contains)
            val warlord = factions.of[Horde].%(game.states.contains).any

            // without a Vagabond the fifth ruin goes to a clearing, and the space back to the box
            if (vb.none && game.ruins.contains(LegendSpace)) {
                game.ruins -= LegendSpace

                val l = clearings.but(LegendSpace).%(c => game.ruins.contains(c).not)
                val c = l.%(c => game.freeSlots(c) == l./(game.freeSlots).max).first

                game.ruins += c -> Ruins($)

                game.legendForest = None
                game.mapping -= LegendSpace
                game.unsuited :-= LegendSpace

                log("With no Vagabond, the fifth ruin went to", c, "and the", LegendSpace, "back to the box")
            }

            val fifth = vb.any

            var pool = $[Item](Hammer, Teapot, Crossbow, Coins, Ring)

            pool = pool.diff(vb./~(_.inv./(_.item)))

            if (warlord && fifth.not)
                pool = pool.intersect($(Coins, Ring))

            Shuffle[Item](pool.some.|($(Ring)), FifthRuinRareAction(_, fifth && warlord, then))

        case FifthRuinRareAction(l, under, then) =>
            val rare = l.first

            log("The rare item was drawn for the ruins")

            if (under) {
                game.ruins += LegendSpace -> Ruins($(rare))

                Shuffle[Item]($(Boots, Sword, Hammer, Bag), FifthRuinFillAction(_, game.ruins.keys.$.but(LegendSpace), then))
            }
            else
                Shuffle[Item]($(Boots, Sword, Hammer, Bag, rare), FifthRuinFillAction(_, game.ruins.keys.$, then))

        case FifthRuinFillAction(l, ruins, then) =>
            ruins.lazyZip(l).foreach { (r, i) =>
                game.ruins += r -> Ruins(game.ruins(r).items :+ i)
            }

            then

        // exploring the fifth ruin scores nothing
        case TakeRuinItemAction(f, LegendSpace, i) =>
            val rest = game.ruins(LegendSpace).items :- i

            if (rest.any)
                game.ruins += LegendSpace -> Ruins(rest)
            else
                game.ruins -= LegendSpace

            f.inv :+= i.ref(false, false)
            f.fromRuins :+= i

            f.log("explored the fifth ruin with", Torch.exhaust.elem ~ " and found", i)

            Repeat

        // RALLY ROUND!
        case RallyRoundAction(f) =>
            exhaustRing(f)

            val n = f.hand.num

            f.hand --> discard.quiet

            f.log("exhausted the", Ring.elem, "with", "Rally Round!".hh ~ ",", "discarded", n.cards, "and drew", 5.cards)

            DrawCardsAction(f, 5, NoMessage, AddCardsAction(f, Repeat))

        // FRIENDSHIP RING
        case FriendshipRingAction(f) =>
            Ask(f).group("Friendship Ring".hh)
                .some(friendshipTargets(f)) { e => f @@ {
                    case h : Hero => $(FriendshipAttitudeAction(h, e))
                    case f => $(FriendshipEnemyAction(f, e))
                }}
                .cancel

        case FriendshipAttitudeAction(f, e) =>
            exhaustRing(f)

            val a : Attitude = f.attitude.get(e).|(Indifferent) match {
                case Hostile => Indifferent
                case Indifferent => Amiable
                case Amiable => Friendly
                case _ => Allied
            }

            f.attitude += e -> a

            f.log("exhausted the", Ring.elem, "and became", a, "to", e, "with", "Friendship Ring".hh)

            Repeat

        case FriendshipEnemyAction(f, e) =>
            exhaustRing(f)

            game.ringFriends :+= (f, e)

            f.log("exhausted the", Ring.elem, "and befriended", e, "until the end of the turn with", "Friendship Ring".hh)

            Repeat

        // BONAFIED PRINCE
        case BonafiedPrinceAction(f) =>
            Ask(f).group("Bonafied Prince".hh)
                .each(princeBuilds(f)) { case (p, c) => PrinceBuildAction(f, p, c) }
                .some(f.as[Hero].$)(h => princeQuests(h)./(q => PrinceQuestAction(h, q)))
                .cancel

        case PrinceBuildAction(f, p, c) =>
            exhaustRing(f)

            f.reserve --> p --> c

            f.log("exhausted the", Ring.elem, "and built", p.of(f), "in", c, "with", "Bonafied Prince".hh)

            Repeat

        case PrinceQuestAction(f, q) =>
            XXSelectObjectsAction(f, f.hand)
                .withGroup("Bonafied Prince".hh ~ " completes " ~ q.elem ~ " with " ~ 2.cards)
                .withRule(_.num(2).each(_.matches(q.suit.as[BaseSuit]./(s => s : SuitCost).|(AnySuit))))
                .withThen(l => PrinceQuestPaidAction(f, q, l))(l => "Discard".hl ~ l./(" " ~ _.elem))
                .withExtra($(NoHand, CancelAction))

        case PrinceQuestPaidAction(f, q, l) =>
            exhaustRing(f)

            f.hand --> l --> discard.quiet

            f.log("exhausted the", Ring.elem, "and discarded", l, "with", "Bonafied Prince".hh)

            QuestCompleteAction(f, q, $)

        // MERCHANDISING
        case MerchandisingAction(f) =>
            Ask(f).group("Merchandising".hh).each(merchandise(f))(c => MerchandisingClearingAction(f, c)).cancel

        case MerchandisingClearingAction(f, c) =>
            exhaustRing(f)

            f.log("exhausted the", Ring.elem, "for", "Merchandising".hh, "in", c)

            MerchandisingPullAction(f, c, adjacent(c).%(o => pullable(f, o).any), 0)

        case MerchandisingPullAction(f, c, Nil, n) =>
            if (n >= 2)
                f.oscore(1)("with", "Merchandising".hh)

            Repeat

        case MerchandisingPullAction(f, c, o :: rest, n) =>
            Ask(f).group("Merchandising".hh, "pulls into", c, "from", o)
                .each(pullable(f, o)) { case (e, p) => MerchandisingMoveAction(f, c, o, e, p, rest, n) }
                .skip(MerchandisingPullAction(f, c, rest, n))

        case MerchandisingMoveAction(f, c, o, e, p, rest, n) =>
            e.from(o) --> p --> c

            f.log("pulled", p.of(e), "from", o, "to", c)

            MerchandisingPullAction(f, c, rest, n + 1)

        // RETURN OF THE KING
        case ReturnKingMainAction(f) =>
            Ask(f).group("Return of the King".hh)
                .each(returnTargets(f)) { case (c, e, p) => ReturnKingAction(f, c, e, p) }
                .cancel

        case ReturnKingAction(f, c, e, p) =>
            f.used :+= RingReturn

            ForcedRemoveAction(f, c, e, p, 0, Removing, ForcedRemoveFinishedAction(f, ReturnKingRewardAction(f, c)))

        case ReturnKingRewardAction(f, c) =>
            Ask(f).group("Return of the King".hh)
                .some(f.as[WarriorFaction].$)(w => w.pool(w.warrior).$(ReturnKingPlaceAction(w, c).!(w.canPlace(c).not)))
                .add(ReturnKingDrawAction(f))

        case ReturnKingPlaceAction(f, c) =>
            f.reserve --> f.warrior --> c

            f.log("placed", f.warrior.of(f), "in", c, "with", "Return of the King".hh)

            Repeat

        case ReturnKingDrawAction(f) =>
            f.log("drew a card with", "Return of the King".hh)

            DrawCardsAction(f, 1, NoMessage, AddCardsAction(f, Repeat))

        // POWER OF THE SCHWARTZ
        case SchwartzMainAction(f) =>
            Ask(f).group("Power of the Schwartz".hh)
                .some(schwartz(f)) { case (c, e) =>
                    e.as[WarriorFaction]./(w => w.at(c).count(w.warrior)).%(_ > 0)./(n => SchwartzWarriorsAction(f, c, e.asInstanceOf[WarriorFaction], (n + 1) / 2)).$ ++
                    e.at(c).%(p => (p.is[Building] || p.is[Token]) && movableBuilding(p)).distinct./(p => SchwartzPieceAction(f, c, e, p))
                }
                .cancel

        case SchwartzWarriorsAction(f, c, e, n) =>
            Ask(f).group("Power of the Schwartz".hh, "moves", n.hl, e.warrior.sof(e), "from", c, "to")
                .each(adjacent(c).but(LegendSpace))(d => SchwartzWarriorsToAction(f, c, e, n, d))
                .cancel

        case SchwartzPieceAction(f, c, e, p) =>
            Ask(f).group("Power of the Schwartz".hh, "moves", p.of(e), "from", c, "to")
                .each(adjacent(c).but(LegendSpace))(d => SchwartzPieceToAction(f, c, e, p, d).!(p.is[Building] && game.freeSlots(d) < 1, "no open slot"))
                .cancel

        case SchwartzWarriorsToAction(f, c, e, n, d) =>
            f.used :+= RingSchwartz

            e.from(c) --> e.at(c).%(_ == e.warrior).take(n) --> d

            f.log("moved", n.hl, e.warrior.sof(e), "from", c, "to", d, "with", "Power of the Schwartz".hh)

            Repeat

        case SchwartzPieceToAction(f, c, e, p, d) =>
            f.used :+= RingSchwartz

            e.from(c) --> p --> d

            f.log("moved", p.of(e), "from", c, "to", d, "with", "Power of the Schwartz".hh)

            Repeat

        // RING OF RAGE: each die one more
        case BattleRolledDoneAction(b, fr, or) if options.has(TheFifthRuin) && ringRule.has(RingOfRage) && game.ringRaged.has(b).not && bearer.exists(r => r == b.attacker || r == b.defender) =>
            game.ringRaged = |(b)

            bearer.get.log("rolled one more on each die with", "Ring of Rage".hh)

            BattleRolledDoneAction(b, fr + 1, or + 1)

        // CAPTURES IN BATTLE: Lil' Looters, 5-Finger Filch
        case BattleStartedAction(b) if captures && captureRule.exists(r => r == LilLooters || r == FiveFingerFilch) && b.attacker != LegendBeast &&
            victim(b.defender) && b.attacker.used.has(CaptureDeclared).not && b.attacker.ignored.has(CaptureDeclared).not && capturable(b.defender).any =>
            Ask(b.attacker).group(captureRule.get.title.hh)
                .add(CaptureDeclareAction(b.attacker, b))
                .add(CaptureDeclineAction(b.attacker, b))

        case CaptureDeclareAction(f, b) =>
            f.used :+= CaptureDeclared

            f.log("wanted to capture", prize, "with", captureRule.get.title.hh)

            BattleStartedAction(b)

        case CaptureDeclineAction(f, b) =>
            f.ignored :+= CaptureDeclared

            BattleStartedAction(b)

        case BattleAdjustRollAction(b, f, o, fs, os, fr, or) if fs > 0 && b.attacker.used.has(CaptureDeclared) =>
            BattleAdjustRollAction(b, f, o, 0, os, fr, or)

        case BattleResolvedAction(b) if b.attacker.used.has(CaptureDeclared) || b.attacker.ignored.has(CaptureDeclared) =>
            val f = b.attacker
            val declared = f.used.has(CaptureDeclared)

            f.used = f.used.but(CaptureDeclared)
            f.ignored = f.ignored.but(CaptureDeclared)

            if (declared.not)
                BattleResolvedAction(b)
            else
            if (captureRule.has(LilLooters)) {
                if (f.rules(b.clearing) || b.defender.present(b.clearing).not)
                    capture(f, b.defender, BattleResolvedAction(b))
                else {
                    f.log("failed to capture", prize)

                    BattleResolvedAction(b)
                }
            }
            else
                FilchMoveAction(f, b)

        case FilchMoveAction(f, b) =>
            val l = f.movePlans($(b.clearing), game.transports./($) ** f.transports)

            if (l.none) {
                f.log("could not move to capture", prize)

                BattleResolvedAction(b)
            }
            else
                MoveInitAction(f, f, game.transports./($) ** f.transports, NoMessage, $(b.clearing), $(b.clearing), $(BattleResolvedAction(b).as("Skip", "Capture")), FilchMovedAction(f, b.defender, BattleResolvedAction(b)))

        case FilchMovedAction(f, e, then) =>
            capture(f, e, then)

        case CaptureItemAction(f, e, i, then) =>
            takeItem(e, i)

            if (i == Ring) {
                if (game.ringAt.any) {
                    game.ringAt = None
                    e.from(RingBox) --> e.reserve
                }
            }

            f.forTrade :+= i.pristine

            f.log("captured", i, "from", e)

            AcquireItemsAction(f, then)

        // SEIZURE & SURRENDER: three hits to the bearer in a turn
        case BattleAssignHitsAction(e, b, n, s, then) if captures && captureRule.has(SeizureAndSurrender) && n > 0 && e != LegendBeast && victim(e) && game.seizureCounted.has(b).not =>
            game.seizureCounted = |(b)

            val f = (e == b.defender).?(b.attacker).|(b.defender)

            if (f == LegendBeast)
                BattleAssignHitsAction(e, b, n, s, then)
            else {
            game.seizureHits += f -> (game.seizureHits.get(f).|(0) + n)

            if (game.seizureHits(f) >= 3 && capturable(e).any) {
                game.seizureHits += f -> -999

                f.log("dealt three hits to", e, "this turn")

                capture(f, e, BattleAssignHitsAction(e, b, n, s, then))
            }
            else
                BattleAssignHitsAction(e, b, n, s, then)
            }

        // PICKPOCKETS
        case PickpocketsMainAction(f) =>
            Ask(f).group("Pickpockets".hh).each(pickpocketClearings(f)) { case (c, e) => PickpocketsClearingAction(f, c, e) }.cancel

        case PickpocketsClearingAction(f, c, e) =>
            XXSelectObjectsAction(f, f.hand)
                .withGroup("Pickpockets".hh ~ " reveals cards matching " ~ c.elem)
                .withRule(_.upTo(f.hand.num).each(_.matches(c.cost)))
                .withThen(l => PickpocketsRevealAction(f, c, e, l))(l => "Reveal".hl ~ l./(" " ~ _.elem))
                .withExtra($(NoHand, CancelAction))

        case PickpocketsRevealAction(f, c, e, l) =>
            f.used :+= PickpocketsUsed

            f.log("revealed", l, "to pick the pockets of", e, "in", c)

            Roll[Int]($(D4), r => PickpocketsRolledAction(f, c, e, l, r(0)), f.elem ~ " picks pockets")

        case PickpocketsRolledAction(f, c, e, l, x) =>
            val lost = (l.%(_.suit == Bird) ++ l.%(_.suit != Bird)).take(x)

            f.hand --> lost --> discard.quiet

            f.log("rolled", x.roll ~ ",", "discarded", lost.any.?(lost).|("none"))

            if (l.num > lost.num)
                capture(f, e, Repeat)
            else {
                f.log("failed to pick the pockets")

                Repeat
            }

        // THE PRECIOUS
        case PreciousMainAction(f) =>
            Ask(f).group("The Precious".hh).add(preciousOptions(f)).cancel

        case PreciousSwapOffAction(f, c, n) =>
            f.used :+= RingPrecious

            f.oscore(-1)("with", "The Precious".hh)

            f.from(c) --> f.at(c).of[Warrior].take(n) --> RingBox

            takeItem(f, Ring)

            game.ringAt = |(c)
            game.ringOwner = |(f)

            f.log("swapped", n.hl, "warriors in", c, "with the", Ring.elem)

            Repeat

        case PreciousSwapOnAction(f, n) =>
            f.used :+= RingPrecious

            val c = game.ringAt.get

            f.oscore(-1)("with", "The Precious".hh)

            f.from(RingBox) --> c

            game.ringAt = None

            f.forTrade :+= Ring.pristine

            f.log("swapped the", Ring.elem, "in", c, "back for", n.hl, "warriors")

            AcquireItemsAction(f, Repeat)

        case PreciousMoveAction(f, c) =>
            f.used :+= RingMoved

            game.ringAt = |(c)

            f.log("moved the", Ring.elem, "to", c)

            Repeat

        // the end of a turn: Friendship and Seizure counts end
        case CleanUpAction(f) if game.ringFriends.any || game.seizureHits.any =>
            game.ringFriends = $
            game.seizureHits = Map()

            CleanUpAction(f)

        case _ => UnknownContinue
    }
}
