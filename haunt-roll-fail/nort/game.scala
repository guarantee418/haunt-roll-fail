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


trait Resource extends NamedToString with Styling with Elementary with Record {
    override def elem : Elem = name.styled(this)
}

case object Food extends Resource
case object Wood extends Resource
case object Lore extends Resource

object Resource {
    val all : $[Resource] = $(Food, Wood, Lore)
}


trait Card extends Elementary with Record {
    def name : String
    // Fame printed on the card, counted at the end of the game
    def fame : Int = 0
    def removable : Boolean = true
    def elem : Elem = name.hl
}

// Starting cards (six per player)
case object RecruitCard extends Card { val name = "Recruit" }
case object ExploreCard extends Card { val name = "Explore" }
case object MoveCard extends Card { val name = "Move" }
case object BuildCard extends Card { val name = "Build" }
case object FeastCard extends Card { val name = "Feast" }

// Clan cards: n = 0 is the initial card, 1 and 2 the upgrades
case class ClanCard(clan : Faction, n : Int) extends Card {
    def name = (n == 0).?(clan.name + " Clan").|(clan.name + " Clan Upgrade " + n)
    override def elem = name.styled(clan)
}

// Placeholders until the card list is entered
case class EarlyDevelopment(n : Int) extends Card { def name = "Early Development " + n }
case class AdvancedDevelopment(n : Int) extends Card { def name = "Advanced Development " + n }
case class Achievement(n : Int) extends Card { def name = "Achievement " + n }

case object UnrestCard extends Card {
    val name = "Unrest"
    override val removable = false
    override def elem = name.styled(xstyles.error)
}

object Cards {
    // Provisional: the rulebook names Recruit, Explore, Move and Build as the
    // starting card actions, with Feast as a wild card; the exact six still
    // have to be checked against the cards
    val starting : $[Card] = $(RecruitCard, RecruitCard, ExploreCard, MoveCard, BuildCard, FeastCard)

    val early : $[Card] = 1.to(16)./(EarlyDevelopment(_))
    val advanced : $[Card] = 1.to(36)./(AdvancedDevelopment(_))
    val achievements : $[Card] = 1.to(7)./(Achievement(_))
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

    // Units on the map; placement comes with the map
    var units = 0

    var draw : $[Card] = $
    var hand : $[Card] = $
    var active : $[Card] = $
    var discard : $[Card] = $

    var upgrades : $[Card] = $

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
case class ShuffledDiscardAction(f : Faction, shuffled : $[Card], then : ForcedAction) extends ShuffledAction[Card]

case object StartYearAction extends ForcedAction
case object RevealDevelopmentsAction extends ForcedAction
case object ActionsPhaseAction extends ForcedAction
case class TurnAction(f : Faction) extends ForcedAction
case class NextTurnAction(f : Faction) extends ForcedAction

case class WaitCardAction(self : Faction, card : Card) extends BaseAction(card)("Wait")
case class ReplaceCardAction(self : Faction, card : Card) extends BaseAction(card)("Replace", "(" ~ 1.hl ~ " " ~ Lore.elem ~ ")")
case class RemoveCardAction(self : Faction, card : Card) extends BaseAction(card)("Remove", "(" ~ 2.hl ~ " " ~ Lore.elem ~ ")")
case class UpgradeCardAction(self : Faction, card : Card, upgrade : Card, remove : Boolean) extends BaseAction(card)("Upgrade to", upgrade, remove.?("and remove").|("and wait"), "(" ~ 3.hl ~ " " ~ Lore.elem ~ ")")
case class PassAction(self : Faction) extends BaseAction("Actions")("Pass")
case class PickCardAction(self : Faction, card : Card) extends BaseAction("Take a card")(card)

case object HarvestAction extends ForcedAction
case class TradeAction(f : Faction, then : ForcedAction) extends ForcedAction
case class TradeForAction(self : Faction, pay : $[Resource], gain : Resource, then : ForcedAction) extends BaseAction("Trade", "three resources for one")("Pay", pay./(_.elem).join(" "), "for", gain)
case object WinterAction extends ForcedAction
case object EndOfYearAction extends ForcedAction
case object GameEndAction extends ForcedAction

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

    val expansions : $[Expansion] = $(CommonExpansion)

    var seating : $[Faction] = setup

    var factions : $[Faction] = setup
    var states = Map[Faction, FactionState]()

    val lastYear = 7

    var year = 0
    var first : Faction = setup.first

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
        display.any.$(Info((year == lastYear).?("Achievements").|("Developments"), display./(_.elem).join(", "))) ++
        self.%(states.contains)./~(f =>
            $(Info("Hand", f.hand.any.?(f.hand./(_.elem).join(", ")).|("empty".txt))) ++
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

object CommonExpansion extends Expansion {
    // All ways to pay three resources out of what f has
    def payments(f : Faction)(implicit game : Game) : $[$[Resource]] =
        Resource.all./~(a => Resource.all./~(b => Resource.all./(c => $(a, b, c))))
            ./(_.sortBy(Resource.all.indexOf(_))).distinct
            .%(p => Resource.all.forall(r => p.count(_ == r) <= f.has(r)))

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

            Shuffle[Card](Cards.early, ShuffledEarlyAction(_))

        case ShuffledEarlyAction(l) =>
            game.developments = l.take(2 * factions.num)

            Shuffle[Card](Cards.advanced, ShuffledAdvancedAction(_))

        case ShuffledAdvancedAction(l) =>
            game.developments ++= l.take(4 * factions.num)

            Shuffle[Card](Cards.achievements, ShuffledAchievementsAction(_))

        case ShuffledAchievementsAction(l) =>
            game.achievements = l.take(factions.num)

            Then(ShuffleStartingDecksAction(factions))

        case ShuffleStartingDecksAction(Nil) =>
            Random[Faction](factions, FirstPlayerAction(_))

        case ShuffleStartingDecksAction(f :: rest) =>
            f.upgrades = $(ClanCard(f, 1), ClanCard(f, 2))

            Shuffle[Card](Cards.starting :+ ClanCard(f, 0), ShuffledStartingDeckAction(f, _, rest))

        case ShuffledStartingDeckAction(f, l, rest) =>
            f.draw = l

            Then(ShuffleStartingDecksAction(rest))

        case FirstPlayerAction(f) =>
            game.first = f

            log(f, "goes first")

            game.from(f).zipWithIndex.foreach { case (f, i) =>
                f.food = (i < 3).?(2).|(3)
                f.wood = 2

                // Two groups of three units; placing them comes with the map
                f.units = 6

                f.log("starts with", f.food.hl, Food, "and", f.wood.hl, Wood)
            }

            Then(StartYearAction)

        // DRAWING
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

            // Forges add one card each, once there is a map
            Then(game.from(game.first).foldRight(RevealDevelopmentsAction : ForcedAction)((f, then) => DrawCardsAction(f, 4, then)))

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
            Then(TurnAction(game.first))

        case TurnAction(f) =>
            game.highlight.current = |(f)

            val cards = f.hand.distinct

            Ask(f)
                .some(cards)(c =>
                    $(WaitCardAction(f, c)) ++
                    (f.lore >= 1).$(ReplaceCardAction(f, c)) ++
                    (f.lore >= 2 && c.removable).$(RemoveCardAction(f, c)) ++
                    (f.lore >= 3).??(f.upgrades./~(u => $(UpgradeCardAction(f, c, u, false)) ++ c.removable.$(UpgradeCardAction(f, c, u, true))))
                )
                .add(PassAction(f))

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
                case Some(n) => Then(TurnAction(n))
                case None =>
                    game.highlight.current = None
                    game.display = $
                    Then(HarvestAction)
            }

        // 3. HARVEST
        case HarvestAction =>
            log(SingleLine)
            log("Harvest")

            // Fame and resources from territories and buildings come with the map
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
            // Three closed territories with large buildings come with the map
            if (game.year >= game.lastYear)
                Then(GameEndAction)
            else
                Milestone(StartYearAction)

        case GameEndAction =>
            log(DoubleLine)
            log("End of the game")

            val totals = factions./{ f =>
                val cards = f.deck./(_.fame).sum
                val sets = f.resources / 3
                val total = f.fame + cards + sets - 5 * f.unrest

                f.log("scored", total.hlb, "fame:", f.fame.hl, "from tokens,", cards.hl, "from cards,", sets.hl, "from resources,", (-5 * f.unrest).hl, "from", UnrestCard)

                f -> total
            }.toMap

            // Ties: territories and buildings come with the map
            val best = factions.%(f => totals(f) == totals.values.max)
            val winners = best.%(f => f.units == best./(_.units).max)

            game.isOver = true
            game.highlight.current = winners.single

            winners.foreach(f => f.log("won"))

            GameOver(winners, "Game Over" ~ Break ~ winners./(_.elem).join(Break) ~ Break ~ "won", winners./(f => GameOverWonAction(null, f)))
    }
}
