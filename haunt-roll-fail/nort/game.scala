package nort
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

import nort.elem._


trait Faction extends NamedToString with Styling with Elementary with BasePlayer with Record {
    def short = name
    def style = name.toLowerCase
    override def elem : Elem = (name + " Clan").styled(this)(styles.title)(xstyles.bold)
}

// The seven clans of the core game
case object Bear extends Faction
case object Boar extends Faction
case object Goat extends Faction
case object Raven extends Faction
case object Snake extends Faction
case object Stag extends Faction
case object Wolf extends Faction


// Player colors mark a player's units and starting cards; they are not tied to the clan
trait PlayerColor extends NamedToString with Styling with Elementary with Record {
    def id = name.toLowerCase
    override def elem : Elem = name.styled(this)
}

case object Blue extends PlayerColor
case object Red extends PlayerColor
case object Yellow extends PlayerColor
case object Purple extends PlayerColor
case object Green extends PlayerColor

object PlayerColor {
    val all : $[PlayerColor] = $(Blue, Red, Yellow, Purple, Green)
}


trait Resource extends NamedToString with Styling with Elementary with Record {
    override def elem : Elem = name.styled(this)
}

case object Food extends Resource
case object Wood extends Resource
case object Lore extends Resource

object Resource {
    val all : $[Resource] = $(Food, Wood, Lore)
}


trait GameImplicits {
    implicit def factionToState(f : Faction)(implicit game : Game) : FactionState = game.states(f)

    def log(s : Any*)(implicit game : Game) {
        game.log(s : _*)
    }

    implicit class FactionEx(f : Faction)(implicit game : Game) {
        def log(s : Any*) { if (game.logging) game.log((f +: s.$) : _*) }
    }

    def options(implicit game : Game) = game.options
    def factions(implicit game : Game) = game.factions
}


class FactionState(val faction : Faction)(implicit game : Game) {
    var food = 0
    var wood = 0
    var lore = 0
    var fame = 0

    // Units on the map
    def units = game.onMap(faction)

    var draw : $[Card] = $
    var hand : $[Card] = $
    var active : $[Card] = $
    var discard : $[Card] = $

    var upgrades : $[Card] = $

    // Cards drawn by a card effect, before choosing what to keep
    var drawn : $[Card] = $

    var passed = false

    def deck = draw ++ hand ++ active ++ discard

    def unrest = deck.count(_ == UnrestCard)

    def resources = food + wood + lore

    def has(r : Resource) = r match {
        case Food => food
        case Wood => wood
        case Lore => lore
    }

    def gain(r : Resource, n : Int) = r match {
        case Food => food += n
        case Wood => wood += n
        case Lore => lore += n
    }
}


object Winter {
    // Food and wood owed for the number of units on the map
    def cost(units : Int) : (Int, Int) =
        if (units <= 3) (0, 0)
        else if (units <= 6) (1, 0)
        else if (units <= 9) (2, 0)
        else if (units <= 12) (3, 1)
        else (4, 2)
}


case class StartAction(version : String) extends StartGameAction with GameVersion
case class ShuffledEarlyAction(shuffled : $[Card]) extends ShuffledAction[Card]
case class ShuffledAdvancedAction(shuffled : $[Card]) extends ShuffledAction[Card]
case class ShuffledAchievementsAction(shuffled : $[Card]) extends ShuffledAction[Card]
case class ShuffleStartingDecksAction(l : $[Faction]) extends ForcedAction
case class ShuffledStartingDeckAction(f : Faction, shuffled : $[Card], l : $[Faction]) extends ShuffledAction[Card]
case class FirstPlayerAction(random : Faction) extends RandomAction[Faction]

case class DrawCardsAction(f : Faction, n : Int, then : ForcedAction) extends ForcedAction
case class DrawTempAction(f : Faction, n : Int, then : ForcedAction) extends ForcedAction
case class ShuffledDiscardAction(f : Faction, shuffled : $[Card], then : ForcedAction) extends ShuffledAction[Card]

case object StartYearAction extends ForcedAction
case object RevealDevelopmentsAction extends ForcedAction
case object ActionsPhaseAction extends ForcedAction
// stage 0: start of the turn, 1: a Flash card was played, 2: the main card was played
case class TurnAction(f : Faction, stage : Int) extends ForcedAction
case class NextTurnAction(f : Faction) extends ForcedAction

case class PlayCardAction(self : Faction, card : Card, stage : Int) extends BaseAction(card)("Play", card.flash.?("(Flash)").|(""))
case class WaitCardAction(self : Faction, card : Card) extends BaseAction(card)("Wait")
case class ReplaceCardAction(self : Faction, card : Card) extends BaseAction(card)("Replace", "(" ~ 1.hl ~ " " ~ Lore.elem ~ ")")
case class RemoveCardAction(self : Faction, card : Card) extends BaseAction(card)("Remove", "(" ~ 2.hl ~ " " ~ Lore.elem ~ ")")
case class UpgradeCardAction(self : Faction, card : Card, upgrade : Card, remove : Boolean) extends BaseAction(card)("Upgrade to", upgrade, remove.?("and remove").|("and wait"), "(" ~ 3.hl ~ " " ~ Lore.elem ~ ")")
case class PassAction(self : Faction) extends BaseAction("Actions")("Pass")
case class EndTurnAction(self : Faction) extends BaseAction("Actions")("End turn")

case class ResolveDrawnAction(f : Faction, keep : Int, discard : Int, back : Int, then : ForcedAction) extends ForcedAction
case class KeepDrawnAction(self : Faction, card : Card, keep : Int, discard : Int, back : Int, then : ForcedAction) extends BaseAction("Keep", (keep > 1).?(keep.hl ~ " cards").|("a card"))(card)
case class DiscardDrawnAction(self : Faction, card : Card, keep : Int, discard : Int, back : Int, then : ForcedAction) extends BaseAction("Discard", (discard > 1).?(discard.hl ~ " cards").|("a card"))(card)
case class ReturnDrawnAction(self : Faction, card : Card, keep : Int, discard : Int, back : Int, then : ForcedAction) extends BaseAction("Return to the top of the draw pile", "(the last one ends on top)")(card)
case class NegotiationTakeAction(self : Faction, card : Card, then : ForcedAction) extends BaseAction("Negociation", "take a card from the discard pile")(card)
case class NegotiationDrawAction(self : Faction, then : ForcedAction) extends BaseAction("Negociation")("Draw a card")
case class ResourcefulAction(f : Faction, then : ForcedAction) extends ForcedAction
case class ResourcefulPayAction(self : Faction, pay : $[Resource], then : ForcedAction) extends BaseAction("Resourceful People", "pay any 3 resources to draw 1 more card")("Pay", pay./(_.elem).join(" "))
case class PickCardAction(self : Faction, card : Card) extends BaseAction("Take a card")(card.img, Break, card)

case object HarvestAction extends ForcedAction
case class TradeAction(f : Faction, then : ForcedAction) extends ForcedAction
case class TradeForAction(self : Faction, pay : $[Resource], gain : Resource, then : ForcedAction) extends BaseAction("Trade", "three resources for one")("Pay", pay./(_.elem).join(" "), "for", gain)
case object WinterAction extends ForcedAction
case object EndOfYearAction extends ForcedAction
case object GameEndAction extends ForcedAction
case object NewYearAction extends ForcedAction
case class DominationAction(rulers : $[Faction]) extends ForcedAction

case class GameOverWonAction(self : Faction, f : Faction) extends BaseInfo("Game Over")(f, "won")


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

    val expansions : $[Expansion] = $(MapExpansion, CommonExpansion)

    var seating : $[Faction] = setup

    var factions : $[Faction] = setup
    var states = Map[Faction, FactionState]()

    // Colors go round the table in seating order
    val colors : Map[Faction, PlayerColor] = setup.zip(PlayerColor.all).toMap

    val lastYear = 7

    var year = 0
    var first : Faction = setup.first

    val board = new Board

    // Map tiles still to be drawn, and each player's tiles during setup
    var pile : $[String] = $
    var tileHand : Map[Faction, $[String]] = Map()
    // Tiles drawn by an Explore action, before one is placed
    var exploring : $[String] = $

    var units : Map[AreaRef, Map[Faction, Int]] = Map()
    var buildings : Map[SpaceRef, Building] = Map()

    // Territories where a Move action started a fight
    var combats : $[AreaRef] = $

    // Fights so far, for statistics
    var fights = 0

    // Units per player
    val unitLimit = 14

    def unitsAt(a : AreaRef) : Map[Faction, Int] = units.getOrElse(a, Map())

    def count(t : Territory, f : Faction) : Int = t.areas./(a => unitsAt(a).getOrElse(f, 0)).sum

    def present(t : Territory) : $[Faction] = seating.%(f => count(t, f) > 0)

    def controlled(f : Faction) : $[Territory] = board.territories.%(t => present(t) == $(f))

    def onMap(f : Faction) : Int = units.values./(_.getOrElse(f, 0)).sum

    def reserve(f : Faction) : Int = unitLimit - onMap(f)

    def addUnits(a : AreaRef, f : Faction, n : Int) {
        val m = unitsAt(a)
        units += a -> (m + (f -> (m.getOrElse(f, 0) + n)))
    }

    def removeUnits(t : Territory, f : Faction, n : Int) {
        var left = n
        t.areas.foreach { a =>
            val m = unitsAt(a)
            val k = math.min(left, m.getOrElse(f, 0))
            if (k > 0) {
                left -= k
                val v = m.getOrElse(f, 0) - k
                units += a -> ((v > 0).?(m + (f -> v)).|(m - f))
            }
        }
    }

    def buildingsIn(t : Territory) : $[(SpaceRef, Building)] = buildings.toList.%{ case (s, _) => t.areas.contains(s.area) }.sortBy(_._2.toString)

    // Food, wood and lore shown on a territory and its buildings
    def produce(t : Territory) : (Int, Int, Int) = {
        val specs = t.areas./(board.spec)
        val here = buildingsIn(t).map(_._2)
        (specs./(_.food).sum + here.count(_ == FoodSilo), specs./(_.wood).sum + here.count(_ == WoodcutterLodge), specs./(_.lore).sum + here.count(_ == CarvedStone))
    }

    // Closed controlled territories with at least one large building
    def strongholds(f : Faction) : $[Territory] = controlled(f).%(board.closed).%(t => buildingsIn(t).exists(_._2.large))

    var developments : $[Card] = $
    var achievements : $[Card] = $
    var display : $[Card] = $

    object highlight {
        var faction : $[Faction] = $
        var current : |[Faction] = None
    }

    // Seating order starting with f
    def from(f : Faction) = seating.dropWhile(_ != f) ++ seating.takeWhile(_ != f)

    def info(waiting : $[Faction], self : |[Faction], actions : $[UserAction]) : $[Info] = {
        (year > 0).$(Info("Year", year.hlb, "of", lastYear.hl)) ++
        display.any.$(Info((year == lastYear).?("Achievements").|("Developments") ~ Break ~ display./(_.img).merge)) ++
        self.%(states.contains)./~(f =>
            $(Info("Hand:", f.hand.any.?(Break ~ f.hand./(_.img).merge).|("empty".txt))) ++
            f.active.any.$(Info("Played:", Break ~ f.active./(_.img).merge)) ++
            $(Info("Fame", f.fame.hlb))
        )
    }

    def convertForLog(s : $[Any]) : $[Any] = s./~{
        case Empty => None
        case NotInLog(_) => None
        case AltInLog(_, m) => |(m)
        case l : $[Any] => convertForLog(l)
        case x => |(x)
    }

    override def log(s : Any*) {
        super.log(convertForLog(s.$) : _*)
    }

    def loggedPerform(action : Action, soft : Void) : Continue = {
        val c = action.as[SelfPerform]./(_.perform(soft)).|(internalPerform(action, soft))

        highlight.faction = c match {
            case Ask(f, _) => $(f)
            case MultiAsk(a, _) => a./(_.faction)
            case _ => Nil
        }

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

// Set by the headless host to print a summary of each game
object Debug {
    var stats = false

    def summary(g : Game) {
        if (stats)
            println("  year " + g.year + ", " + g.board.placements.size + " tiles, " + g.buildings.size + " buildings, " + g.fights + " fights, " +
                g.factions.map(f => f.name + " " + g.onMap(f) + "u " + g.states(f).fame + "f " + g.strongholds(f).size + "s").mkString(", "))
    }
}

object CommonExpansion extends Expansion {
    // All ways to pay three resources out of what f has
    def payments(f : Faction)(implicit game : Game) : $[$[Resource]] =
        Resource.all./~(a => Resource.all./~(b => Resource.all./(c => $(a, b, c))))
            ./(_.sortBy(Resource.all.indexOf(_))).distinct
            .%(p => Resource.all.forall(r => p.count(_ == r) <= f.has(r)))

    def available(f : Faction)(implicit game : Game) = f.draw.num + f.discard.num

    // Only cards whose effect is implemented can be played
    def playable(f : Faction, c : Card)(implicit game : Game) = c.effect match {
        case DrawEffect(n, _, _, _) => available(f) >= n
        case CollectEffect(_, _) => true
        case NegotiationEffect => available(f) > 0
        case ResourcefulEffect => available(f) >= 2
        case e => MapExpansion.playable(f, e)
    }

    def perform(action : Action, soft : Void)(implicit game : Game) = action @@ {
        // SETUP
        case StartAction(version) =>
            log("HRF".hl, "version", gaming.version.hlb)
            log("Northgard: Uncharted Lands".hlb.styled(styles.title))
            log("This game is very much", "under construction".styled(xstyles.warning))

            if (version != gaming.version)
                log("Saved game version", version.hlb)

            options.foreach { o =>
                log(o.group, o.valueOn)
            }

            game.setup.foreach { f =>
                game.states += f -> new FactionState(f)
            }

            Shuffle[Card](Cards.earlyCards, ShuffledEarlyAction(_))

        case ShuffledEarlyAction(l) =>
            game.developments = l.take(2 * factions.num)

            Shuffle[Card](Cards.advancedCards, ShuffledAdvancedAction(_))

        case ShuffledAdvancedAction(l) =>
            game.developments ++= l.take(4 * factions.num)

            Shuffle[Card](Cards.achievementCards, ShuffledAchievementsAction(_))

        case ShuffledAchievementsAction(l) =>
            game.achievements = l.take(factions.num)

            Then(ShuffleStartingDecksAction(factions))

        case ShuffleStartingDecksAction(Nil) =>
            Random[Faction](factions, FirstPlayerAction(_))

        case ShuffleStartingDecksAction(f :: rest) =>
            f.upgrades = $(ClanCard(f, 1), ClanCard(f, 2))

            Shuffle[Card](Cards.starting(game.colors(f)) :+ ClanCard(f, 0), ShuffledStartingDeckAction(f, _, rest))

        case ShuffledStartingDeckAction(f, l, rest) =>
            f.draw = l

            Then(ShuffleStartingDecksAction(rest))

        case FirstPlayerAction(f) =>
            game.first = f

            log(f, "goes first")

            game.from(f).zipWithIndex.foreach { case (f, i) =>
                f.food = (i < 3).?(2).|(3)
                f.wood = 2

                f.log("plays", game.colors(f), "and starts with", f.food.hl, Food, "and", f.wood.hl, Wood)
            }

            Shuffle[String](Tiles.regular./(_.id), ShuffledTilesAction(_))

        // DRAWING
        case DrawTempAction(f, n, then) =>
            if (n <= 0)
                Then(then)
            else
            if (f.draw.any) {
                f.drawn :+= f.draw.head
                f.draw = f.draw.drop(1)

                Then(DrawTempAction(f, n - 1, then))
            }
            else
            if (f.discard.any)
                Shuffle[Card](f.discard, ShuffledDiscardAction(f, _, DrawTempAction(f, n, then)))
            else
                Then(then)

        case ResolveDrawnAction(f, k, d, b, then) =>
            val n = f.drawn.num

            if (n == 0)
                Then(then)
            else
            if (k >= n) {
                f.hand ++= f.drawn
                f.drawn = $

                f.log("kept", n.cards)

                Then(then)
            }
            else
            if (k == 0 && d >= n) {
                f.discard ++= f.drawn
                f.log("discarded", f.drawn./(_.elem).join(", "))
                f.drawn = $

                Then(then)
            }
            else
            if (k == 0 && d == 0 && f.drawn.distinct.num == 1) {
                f.draw = f.drawn ++ f.draw
                f.drawn = $

                f.log("returned", n.cards, "to the top of their draw pile")

                Then(then)
            }
            else
            if (k > 0)
                Ask(f).each(f.drawn.distinct)(c => KeepDrawnAction(f, c, k, d, b, then))
            else
            if (d > 0)
                Ask(f).each(f.drawn.distinct)(c => DiscardDrawnAction(f, c, k, d, b, then))
            else
                Ask(f).each(f.drawn.distinct)(c => ReturnDrawnAction(f, c, k, d, b, then))

        case KeepDrawnAction(f, c, k, d, b, then) =>
            f.drawn = f.drawn.diff($(c))
            f.hand :+= c

            f.log("kept a card")

            Then(ResolveDrawnAction(f, k - 1, d, b, then))

        case DiscardDrawnAction(f, c, k, d, b, then) =>
            f.drawn = f.drawn.diff($(c))
            f.discard :+= c

            f.log("discarded", c)

            Then(ResolveDrawnAction(f, k, d - 1, b, then))

        case ReturnDrawnAction(f, c, k, d, b, then) =>
            f.drawn = f.drawn.diff($(c))
            f.draw = c +: f.draw

            f.log("returned a card to the top of their draw pile")

            Then(ResolveDrawnAction(f, k, d, b - 1, then))

        case NegotiationTakeAction(f, c, then) =>
            f.discard = f.discard.diff($(c))
            f.hand :+= c

            f.log("took", c, "from their discard pile")

            Then(then)

        case NegotiationDrawAction(f, then) =>
            f.log("drew a card")

            Then(DrawCardsAction(f, 1, then))

        case ResourcefulAction(f, then) =>
            val pp = payments(f)

            if (pp.none || f.draw.none && f.discard.none)
                Then(then)
            else
                Ask(f).each(pp)(p => ResourcefulPayAction(f, p, then)).skip(then)

        case ResourcefulPayAction(f, p, then) =>
            p.foreach(x => f.gain(x, -1))

            f.log("paid", p./(_.elem).join(" "), "to draw another card")

            Then(DrawCardsAction(f, 1, then))

        case DrawCardsAction(f, n, then) =>
            if (n <= 0)
                Then(then)
            else
            if (f.draw.any) {
                f.hand :+= f.draw.head
                f.draw = f.draw.drop(1)

                Then(DrawCardsAction(f, n - 1, then))
            }
            else
            if (f.discard.any)
                Shuffle[Card](f.discard, ShuffledDiscardAction(f, _, DrawCardsAction(f, n, then)))
            else
                Then(then)

        case ShuffledDiscardAction(f, l, then) =>
            f.log("shuffled their discard pile into a new draw pile")

            f.discard = $
            f.draw = l

            Then(then)

        // 1. START OF YEAR
        case StartYearAction =>
            game.year += 1

            log(DoubleLine)
            log("Year", game.year.hlb)

            factions.foreach { f =>
                f.passed = false
            }

            // Each controlled Forge draws one more card
            Then(game.from(game.first).foldRight(RevealDevelopmentsAction : ForcedAction)((f, then) => DrawCardsAction(f, 4 + game.controlled(f)./~(game.buildingsIn).map(_._2).count(_ == Forge), then)))

        case RevealDevelopmentsAction =>
            if (game.year == game.lastYear) {
                game.display = game.achievements
                game.achievements = $

                log("Achievement cards", game.display./(_.elem).join(", "), "are available")
            }
            else {
                game.display = game.developments.take(factions.num)
                game.developments = game.developments.drop(factions.num)

                if (game.display.any)
                    log("Development cards", game.display./(_.elem).join(", "), "are available")
            }

            Then(ActionsPhaseAction)

        // 2. ACTIONS
        case ActionsPhaseAction =>
            Then(TurnAction(game.first, 0))

        case TurnAction(f, stage) =>
            game.highlight.current = |(f)

            val cards = f.hand.distinct

            if (stage == 0)
                Ask(f)
                    .some(cards)(c =>
                        playable(f, c).$(PlayCardAction(f, c, stage)) ++
                        $(WaitCardAction(f, c)) ++
                        (f.lore >= 1).$(ReplaceCardAction(f, c)) ++
                        (f.lore >= 2 && c.removable).$(RemoveCardAction(f, c)) ++
                        (f.lore >= 3).??(f.upgrades./~(u => $(UpgradeCardAction(f, c, u, false)) ++ c.removable.$(UpgradeCardAction(f, c, u, true))))
                    )
                    .add(PassAction(f))
            else
                Ask(f)
                    .each(cards.%(c => playable(f, c) && (stage == 1 || c.flash)))(c => PlayCardAction(f, c, stage))
                    .add(EndTurnAction(f))

        case PlayCardAction(f, c, stage) =>
            f.hand = f.hand.diff($(c))
            f.active :+= c

            f.log("played", c)

            val after = TurnAction(f, (c.flash && stage < 2).?(1).|(2))

            c.effect match {
                case DrawEffect(n, k, d, b) =>
                    Then(DrawTempAction(f, n, ResolveDrawnAction(f, k, d, b, after)))

                case CollectEffect(r, n) =>
                    f.gain(r, n)

                    f.log("collected", n.hl, r)

                    Then(after)

                case NegotiationEffect =>
                    Ask(f)
                        .each(f.discard.distinct)(c => NegotiationTakeAction(f, c, after))
                        .when(f.draw.any || f.discard.any)(NegotiationDrawAction(f, after))

                case ResourcefulEffect =>
                    Then(DrawCardsAction(f, 2, ResourcefulAction(f, after)))

                case e =>
                    MapExpansion.resolve(f, e, after)
            }

        case EndTurnAction(f) =>
            Then(NextTurnAction(f))

        case WaitCardAction(f, c) =>
            f.hand = f.hand.diff($(c))
            f.active :+= c

            f.log("waited")

            Then(NextTurnAction(f))

        case ReplaceCardAction(f, c) =>
            f.hand = f.hand.diff($(c))
            f.active :+= c
            f.lore -= 1

            f.log("replaced a card for", 1.hl, Lore)

            Then(DrawCardsAction(f, 1, NextTurnAction(f)))

        case RemoveCardAction(f, c) =>
            f.hand = f.hand.diff($(c))
            f.lore -= 2

            f.log("removed", c, "from the game for", 2.hl, Lore)

            Then(DrawCardsAction(f, 2, NextTurnAction(f)))

        case UpgradeCardAction(f, c, u, remove) =>
            f.hand = f.hand.diff($(c))

            if (remove)
                f.log("removed", c, "from the game")
            else
                f.active :+= c

            f.lore -= 3
            f.upgrades = f.upgrades.diff($(u))
            f.hand :+= u

            f.log("upgraded to", u, "for", 3.hl, Lore)

            Then(NextTurnAction(f))

        case PassAction(f) =>
            f.discard ++= f.hand ++ f.active
            f.hand = $
            f.active = $

            if (factions.forall(_.passed.not)) {
                game.first = f

                f.log("passed and took the first player marker")
            }
            else
                f.log("passed")

            f.passed = true

            if (game.display.any)
                Ask(f).each(game.display)(c => PickCardAction(f, c))
            else
                Then(NextTurnAction(f))

        case PickCardAction(f, c) =>
            game.display = game.display.diff($(c))
            f.draw = c +: f.draw

            f.log("took", c, "and placed it on top of their draw pile")

            Then(NextTurnAction(f))

        case NextTurnAction(f) =>
            game.from(f).drop(1).:+(f).%(_.passed.not).headOption match {
                case Some(n) => Then(TurnAction(n, 0))
                case None =>
                    game.highlight.current = None
                    game.display = $
                    Then(HarvestAction)
            }

        // 3. HARVEST
        case HarvestAction =>
            log(SingleLine)
            log("Harvest")

            game.from(game.first).foreach { f =>
                val territories = game.controlled(f)

                val fame = territories.%(game.board.closed)./(t => (game.board.tiles(t) >= 3).?(2).|(1)).sum
                if (fame > 0) {
                    f.fame += fame
                    f.log("gained", fame.hl, "fame from closed territories")
                }

                val altars = territories./~(game.buildingsIn).map(_._2).count(_ == AltarOfKings)
                if (altars > 0) {
                    f.fame += 3 * altars
                    f.log("gained", (3 * altars).hl, "fame from", AltarOfKings)
                }

                val (food, wood, lore) = territories./(game.produce).foldLeft((0, 0, 0))((a, b) => (a._1 + b._1, a._2 + b._2, a._3 + b._3))
                f.food += food
                f.wood += wood
                f.lore += lore
                f.log("collected", food.hl, Food, Comma, wood.hl, Wood, "and", lore.hl, Lore)
            }

            Then(game.from(game.first).foldRight(WinterAction : ForcedAction)((f, then) => TradeAction(f, then)))

        case TradeAction(f, then) =>
            val pp = payments(f)

            if (pp.none)
                Then(then)
            else
                Ask(f)
                    .some(pp)(p => Resource.all./(r => TradeForAction(f, p, r, then)))
                    .done(then)

        case TradeForAction(f, p, r, then) =>
            p.foreach(x => f.gain(x, -1))
            f.gain(r, 1)

            f.log("traded", p./(_.elem).join(" "), "for", r)

            Then(TradeAction(f, then))

        // 4. WINTER
        case WinterAction =>
            log(SingleLine)
            log("Winter")

            game.from(game.first).foreach { f =>
                val (food, wood) = Winter.cost(f.units)

                if (food + wood == 0)
                    f.log("owed nothing for", f.units.hl, "units")
                else
                if (f.food >= food && f.wood >= wood) {
                    f.food -= food
                    f.wood -= wood

                    f.log("paid", food.hl, Food, (wood > 0).?("and " ~ wood.hl ~ " " ~ Wood.elem).|(Empty), "for", f.units.hl, "units")
                }
                else {
                    f.food = math.max(0, f.food - food)
                    f.wood = math.max(0, f.wood - wood)
                    f.draw = UnrestCard +: f.draw

                    f.log("could not pay for", f.units.hl, "units and took", UnrestCard)
                }
            }

            Then(EndOfYearAction)

        // 5. END OF YEAR
        case EndOfYearAction =>
            val rulers = factions.%(f => game.strongholds(f).num >= 3)

            if (rulers.any)
                Then(DominationAction(rulers))
            else
            if (game.year >= game.lastYear)
                Then(GameEndAction)
            else
                Then(game.from(game.first).foldRight(NewYearAction : ForcedAction)((f, then) => ReturnUnitsAction(f, then)))

        case NewYearAction =>
            Milestone(StartYearAction)

        case DominationAction(rulers) =>
            log(DoubleLine)

            rulers.foreach(f => f.log("controls three closed territories with large buildings"))

            // Ties: fame, then territories controlled, then units, then buildings
            val winners = rulers.%(f => f.fame == rulers./(_.fame).max) @@ { l =>
                val t = l.%(f => game.controlled(f).num == l./(game.controlled(_).num).max)
                val u = t.%(f => f.units == t./(_.units).max)
                u.%(f => game.controlled(f)./~(game.buildingsIn).num == u./(game.controlled(_)./~(game.buildingsIn).num).max)
            }

            game.isOver = true
            game.highlight.current = winners.single

            winners.foreach(f => f.log("won"))

            Debug.summary(game)

            GameOver(winners, "Game Over" ~ Break ~ winners./(_.elem).join(Break) ~ Break ~ "won", winners./(f => GameOverWonAction(null, f)))

        case GameEndAction =>
            log(DoubleLine)
            log("End of the game")

            val totals = factions./{ f =>
                val territories = game.controlled(f)
                val built = territories./~(game.buildingsIn).map(_._2)
                val spaces = territories./~(_.areas)./~(a => game.board.spec(a).spaces.indices./(i => SpaceRef(a, i))).%(s => game.buildings.contains(s).not)
                val (food, wood, lore) = territories./(game.produce).foldLeft((0, 0, 0))((a, b) => (a._1 + b._1, a._2 + b._2, a._3 + b._3))

                val cards = f.deck./{
                    case Achievement("builder") => built.count(_.large.not) + 3 * built.count(_.large)
                    case Achievement("explorer") => spaces.count(s => game.board.spec(s.area).spaces(s.index).kind != LargeSpace) + 3 * spaces.count(s => game.board.spec(s.area).spaces(s.index).kind == LargeSpace)
                    case Achievement("food-trader") => 2 * food
                    case Achievement("wood-trader") => 2 * wood
                    case Achievement("scholar") => 2 * lore
                    case Achievement("trapper") => 3 * territories./~(_.areas).count(a => game.board.spec(a).lair)
                    case Achievement("warlord") => f.units
                    case c => c.fame
                }.sum
                val sets = f.resources / 3
                val total = f.fame + cards + sets - 5 * f.unrest

                f.log("scored", total.hlb, "fame:", f.fame.hl, "from tokens,", cards.hl, "from cards,", sets.hl, "from resources,", (-5 * f.unrest).hl, "from", UnrestCard)

                f -> total
            }.toMap

            // Ties: territories controlled, then units, then buildings
            val best = factions.%(f => totals(f) == totals.values.max)
            val t = best.%(f => game.controlled(f).num == best./(game.controlled(_).num).max)
            val u = t.%(f => f.units == t./(_.units).max)
            val winners = u.%(f => game.controlled(f)./~(game.buildingsIn).num == u./(game.controlled(_)./~(game.buildingsIn).num).max)

            game.isOver = true
            game.highlight.current = winners.single

            winners.foreach(f => f.log("won"))

            Debug.summary(game)

            GameOver(winners, "Game Over" ~ Break ~ winners./(_.elem).join(Break) ~ Break ~ "won", winners./(f => GameOverWonAction(null, f)))

        case _ => UnknownContinue
    }
}
