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


// A clan's name shows in the color its player picked
trait Faction extends NamedToString with Styling with GameElementary with BasePlayer with Record {
    def short = name
    def style = name.toLowerCase
    def elem(implicit game : Game) : Elem = (name + " Clan").styled(game.colors.get(this)./(c => c : Styling).|(this))(styles.title)(xstyles.bold)
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

    // Units on the map, with the warchief (Warchiefs module)
    def units = game.onMap(faction) + game.chiefs.contains(faction).??(1)

    var draw : $[Card] = $
    var hand : $[Card] = $
    var active : $[Card] = $
    var discard : $[Card] = $

    var upgrades : $[Card] = $

    // Cards drawn by a card effect, before choosing what to keep
    var drawn : $[Card] = $

    var passed = false

    // Cards played this year (Legendary Heroes)
    var played : $[Card] = $
    // Future Sight was played this year: no card when passing
    var foresaw = false
    // Conqueror was played this year
    var conqueror = false

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
// A card in hand, shown as its image; clicking it selects it and offers what can be done with it
case class CardMenuAction(self : Faction, card : Card, stage : Int) extends BaseAction("Your hand")(card.handImg) with Soft with ViewObject[Card] { def obj = card }
// Another card in hand while one is selected; not exploded, so bots and checks don't walk from card to card
case class CardSwitchAction(self : Faction, card : Card, stage : Int) extends BaseAction("Your hand")(card.handImg) with Soft with NoExplode with ViewObject[Card] { def obj = card }
// The selected card in hand, as in Root's card choices; clicking it again opens it full screen
case class CardSelectedAction(self : Faction, card : Card) extends BaseInfo("Your hand")(card.handImg) with ViewObject[Card] with Selected with OnClickInfo { def obj = card ; def param = card }
// Cards shown while there is nothing to do with them; clicking one opens it full screen
case class CardInfoAction(self : Faction, title : Elem, card : Card) extends BaseInfo(title)(card.handImg) with ViewObject[Card] with OnClickInfo { def obj = card ; def param = card }
case class PassAction(self : Faction) extends BaseAction("Actions")("Pass")
case class EndTurnAction(self : Faction) extends BaseAction("Actions")("End turn")
// Resolve a played card that nobody cancelled
case class PlayResolveAction(f : Faction, card : Card, stage : Int) extends ForcedAction
// Resolve a card's effect (also for effects copied from other cards)
case class ResolveEffectAction(f : Faction, e : Effect, then : ForcedAction) extends ForcedAction

case class ResolveDrawnAction(f : Faction, keep : Int, discard : Int, back : Int, then : ForcedAction) extends ForcedAction
case class KeepDrawnAction(self : Faction, card : Card, keep : Int, discard : Int, back : Int, then : ForcedAction) extends BaseAction("Keep", (keep > 1).?(keep.hl ~ " cards").|("a card"))(card)
case class DiscardDrawnAction(self : Faction, card : Card, keep : Int, discard : Int, back : Int, then : ForcedAction) extends BaseAction("Discard", (discard > 1).?(discard.hl ~ " cards").|("a card"))(card)
case class ReturnDrawnAction(self : Faction, card : Card, keep : Int, discard : Int, back : Int, then : ForcedAction) extends BaseAction("Return to the top of the draw pile", "(the last one ends on top)")(card)
case class NegotiationTakeAction(self : Faction, card : Card, then : ForcedAction) extends BaseAction("Negociation", "take a card from the discard pile")(card)
case class NegotiationDrawAction(self : Faction, then : ForcedAction) extends BaseAction("Negociation")("Draw a card")
case class ResourcefulAction(f : Faction, then : ForcedAction) extends ForcedAction
case class ResourcefulPayAction(self : Faction, pay : $[Resource], then : ForcedAction) extends BaseAction("Resourceful People", "pay any 3 resources to draw 1 more card")("Pay", pay./(_.elem).join(" "))
case class PickCardAction(self : Faction, card : Card) extends BaseAction("Take a card")(card.img, Break, card)

// Snake Clan may take one resource from the territory with its Scorched Earth token
case object ScorchedHarvestAction extends ForcedAction
case object CreaturePhaseAction extends ForcedAction
case class PassedAction(f : Faction) extends ForcedAction
case class ScorchedTakeAction(self : Faction, r : |[Resource]) extends BaseAction("Scorched Earth".hl, "take one resource from", ScorchedEarthPlace)(r./(_.elem).|("Take nothing".txt))
case class HarvestAction(take : |[Resource]) extends ForcedAction
case class TradeAction(f : Faction, then : ForcedAction) extends ForcedAction
case class TradeForAction(self : Faction, pay : $[Resource], gain : Resource, then : ForcedAction) extends BaseAction("Trade", "three resources for one")("Pay", pay./(_.elem).join(" "), "for", gain)
case object WinterAction extends ForcedAction
case object EndOfYearAction extends ForcedAction
case object GameEndAction extends ForcedAction
case object NewYearAction extends ForcedAction
case class DominationAction(rulers : $[Faction]) extends ForcedAction

case class GameOverWonAction(self : Faction, f : Faction) extends BaseInfo("Game Over")(f, "won")


case object ScorchedEarthPlace extends GameElementary {
    def elem(implicit game : Game) = game.scorched./(_.elem).|(Empty)
}


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

    // Modules and expansions turned on in the options
    val modules : $[Module] = Module.all.%(m => options.has(ModuleOption(m)))

    def has(m : Module) = modules.has(m)

    // Module expansions come first, so they can take over any core action
    val expansions : $[Expansion] = modules./~(_.expansion) ++ $(MapExpansion, CardsExpansion, CommonExpansion)

    var seating : $[Faction] = setup

    var factions : $[Faction] = setup
    var states = Map[Faction, FactionState]()

    // Colors chosen on the setup screen; clans without one get the free colors in seating order
    val colors : Map[Faction, PlayerColor] = {
        val chosen = options.of[ColorOption].%(o => setup.has(o.clan)).groupBy(_.clan)./{ case (f, l) => f -> l.head.color }.toMap
        val free = PlayerColor.all.diff(chosen.values.$)
        chosen ++ setup.%(f => chosen.contains(f).not).zip(free).toMap
    }

    val lastYear = options.of[YearsOption].single./(_.years).|(YearsOption.standard.years)

    // One Development card per player each year but the last; about a third are Early cards (2 + 4 in the standard 7-year game, 3 + 6 in the 10-year one)
    val earlyPerPlayer = (lastYear - 1 + 2) / 3
    val advancedPerPlayer = lastYear - 1 - earlyPerPlayer

    // The Warchiefs box's extra clan upgrade cards
    val warchiefCards = has(Warchiefs) || options.has(WarchiefCards)

    // Three closed territories with large buildings win at the end of a year
    val domination = options.has(FameOnly).not

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

    // Warchief upgrade cards: the areas the mover controlled before the Move action (Halvard's Craft),
    // its casualties waiting to come back (Svarn's Menders), and who picks the loser's retreat (Brand's Bravery)
    var heldBefore : $[AreaRef] = $
    var mended = 0
    var retreatBy : |[Faction] = None

    // Where the last Recruit action placed units (Raven Mercenaries)
    var recruited : $[AreaRef] = $

    // Fights so far, for statistics
    var fights = 0

    // Rule events for the headless host's summary (Kaija recruited, cards resolved, ...)
    var events : Map[String, Int] = Map()

    def note(k : String) { events += k -> (events.getOrElse(k, 0) + 1) }

    // Units per player
    val unitLimit = 14

    // Bear Clan's Kaija: where it is (None while in the reserve), and whether it may enter enemy territories this year
    var kaija : |[AreaRef] = None
    var awakened = false

    // Snake Clan's Scorched Earth token
    var scorched : |[AreaRef] = None

    // Creatures module: the draw and discard piles, the creature line (the creatures on the map, in activation order),
    // where each one is, and the creatures attacked by the current Move action
    var creatureDeck : $[Creature] = $
    var creatureDiscard : $[Creature] = $
    var creatureLine : $[Creature] = $
    var creatureAt : Map[Creature, AreaRef] = Map()
    var creatureFights : $[CreatureFight] = $
    var creaturesShuffled = false

    def creaturesIn(t : Territory) : $[Creature] = creatureLine.%(c => t.areas.contains(creatureAt(c)))

    // A Fallen Valkyrie: units can't stay there without fighting it
    def hostileIn(t : Territory) : Boolean = creaturesIn(t).exists(_.kind.shares.not)

    // A Brown Bear: no building, recruiting, exploring or moving out
    def bearIn(t : Territory) : Boolean = creaturesIn(t).exists(_.kind == BrownBear)

    // A Wolf: no fame or resources at harvest except from buildings
    def wolfIn(t : Territory) : Boolean = creaturesIn(t).exists(_.kind == CreatureWolf)

    def unitsAt(a : AreaRef) : Map[Faction, Int] = units.getOrElse(a, Map())

    // Units only; Kaija is counted separately
    def count(t : Territory, f : Faction) : Int = t.areas./(a => unitsAt(a).getOrElse(f, 0)).sum

    def kaijaIn(t : Territory, f : Faction) : Boolean = f == Bear && kaija.exists(t.areas.contains)

    // Warchiefs module: where each clan's warchief is (none while in the reserve)
    var chiefs : Map[Faction, AreaRef] = Map()

    def chiefIn(t : Territory, f : Faction) : Boolean = chiefs.get(f).exists(t.areas.contains)

    // The warchief is in the reserve and can be recruited
    def chiefReady(f : Faction) : Boolean = has(Warchiefs) && chiefs.contains(f).not && setup.has(f)

    // Units, Kaija and the warchief
    def figures(t : Territory, f : Faction) : Int = count(t, f) + kaijaIn(t, f).??(1) + chiefIn(t, f).??(1)

    // Combat points of the figures: Kaija is worth 2, a warchief 2 or 3 depending on its power
    def strength(t : Territory, f : Faction, attacking : Boolean) : Int = count(t, f) + kaijaIn(t, f).??(2) + Warchief.strength(t, f, attacking)

    // Kaija is in Bear Clan's reserve and can be recruited
    def kaijaReady(f : Faction) : Boolean = f == Bear && kaija.none && setup.has(Bear)

    def scorchedIn(t : Territory) : Boolean = scorched.exists(t.areas.contains)

    def present(t : Territory) : $[Faction] = seating.%(f => figures(t, f) > 0)

    // Units or Kaija anywhere on the map
    def anyOnMap(f : Faction) : Boolean = onMap(f) > 0 || (f == Bear && kaija.any) || chiefs.contains(f)

    def controlled(f : Faction) : $[Territory] = board.territories.%(t => present(t) == $(f))

    def onMap(f : Faction) : Int = units.values./(_.getOrElse(f, 0)).sum

    def reserve(f : Faction) : Int = unitLimit - onMap(f)

    def addUnits(a : AreaRef, f : Faction, n : Int) {
        val m = unitsAt(a)
        units += a -> (m + (f -> (m.getOrElse(f, 0) + n)))
    }

    // Casualties: units first, then the warchief, Kaija last
    def removeFigures(t : Territory, f : Faction, n : Int) {
        val k = math.min(n, count(t, f))
        removeUnits(t, f, k)
        var left = n - k
        if (left > 0 && chiefIn(t, f)) {
            chiefs -= f
            left -= 1
        }
        if (left > 0 && kaijaIn(t, f))
            kaija = None
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

    // What a territory gives at harvest (and when collecting as at harvest): a Wolf leaves only the buildings' resources
    def harvest(t : Territory) : (Int, Int, Int) = {
        if (wolfIn(t).not)
            return produce(t)

        val here = buildingsIn(t).map(_._2)
        (here.count(_ == FoodSilo), here.count(_ == WoodcutterLodge), here.count(_ == CarvedStone))
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
        // The developments and achievements are shown in the court pane (UI.drawCards);
        // your hand is in the action pane, as choices on your turn and as pictures otherwise
        val choosing = actions.exists(a => a.unwrap.is[CardMenuAction] || a.unwrap.is[CardSelectedAction])

        (year > 0).$(Info("Year", year.hlb, "of", lastYear.hl)) ++
        self.%(states.contains)./~(f =>
            choosing.not.??(f.hand./(c => CardInfoAction(f, "Your hand".styled(colors(f)), c))) ++
            f.active./(c => CardInfoAction(f, "Played".styled(colors(f)), c)) ++
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

    // Headless testing only: starting decks also hold both clan upgrades
    var upgradesInDeck = false

    def summary(g : Game) {
        if (stats)
            println("  year " + g.year + ", " + g.board.placements.size + " tiles, " + g.buildings.size + " buildings, " + g.fights + " fights, " +
                g.factions.map(f => f.name + " " + g.onMap(f) + "u " + g.states(f).fame + "f " + g.strongholds(f).size + "s").mkString(", ") +
                g.events.any.??("\n    " + g.events.toList.sortBy(_._1).map { case (k, n) => k + " " + n }.mkString(", ")))
    }
}

object CommonExpansion extends Expansion {
    // All ways to pay n (3 by default) resources out of what f has
    def payments(f : Faction, n : Int = 3)(implicit game : Game) : $[$[Resource]] =
        1.to(n).foldLeft($($[Resource]()))((l, _) => l./~(p => Resource.all./(r => p :+ r)))
            ./(_.sortBy(Resource.all.indexOf(_))).distinct
            .%(p => Resource.all.forall(r => p.count(_ == r) <= f.has(r)))

    def available(f : Faction)(implicit game : Game) = f.draw.num + f.discard.num

    // Only cards whose effect is implemented can be played
    def playable(f : Faction, c : Card)(implicit game : Game) : Boolean = playableEffect(f, c.effect)

    // A card's choices, below the hand with that card selected; another card selects that one instead
    def cardMenu(f : Faction, c : Card, stage : Int)(implicit game : Game) : Continue = {
        val hand = handChoices(f, stage)./(d => (d == c).?(CardSelectedAction(f, d) : UserAction).|(CardSwitchAction(f, d, stage)))

        if (stage == 0)
            Ask(f)
                .add(hand)
                .add(playable(f, c).$(PlayCardAction(f, c, stage)))
                .add(WaitCardAction(f, c))
                .when(f.lore >= 1)(ReplaceCardAction(f, c))
                .when(f.lore >= 2 && c.removable)(RemoveCardAction(f, c))
                .add((f.lore >= 3).??(f.upgrades./~(u => $(UpgradeCardAction(f, c, u, false)) ++ c.removable.$(UpgradeCardAction(f, c, u, true)))))
                .cancel
        else
            Ask(f)
                .add(hand)
                .add(PlayCardAction(f, c, stage))
                .cancel
    }

    // The cards in hand that can be chosen on a turn: any at the start, then only playable ones (only Flash cards after the first)
    def handChoices(f : Faction, stage : Int)(implicit game : Game) : $[Card] =
        f.hand.distinct.%(c => stage == 0 || (playable(f, c) && (stage == 1 || c.flash)))

    def playableEffect(f : Faction, e : Effect)(implicit game : Game) : Boolean = e match {
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

            options.%(_.is[ColorOption].not).foreach { o =>
                log(o.group, o.valueOn)
            }

            game.setup.foreach { f =>
                game.states += f -> new FactionState(f)
            }

            Shuffle[Card](Cards.earlyCards, ShuffledEarlyAction(_))

        case ShuffledEarlyAction(l) =>
            game.developments = l.take(game.earlyPerPlayer * factions.num)

            Shuffle[Card](Cards.advancedCards, ShuffledAdvancedAction(_))

        case ShuffledAdvancedAction(l) =>
            game.developments ++= l.take(game.advancedPerPlayer * factions.num)

            Shuffle[Card](Cards.achievementCards, ShuffledAchievementsAction(_))

        case ShuffledAchievementsAction(l) =>
            game.achievements = l.take(factions.num)

            Then(ShuffleStartingDecksAction(factions))

        case ShuffleStartingDecksAction(Nil) =>
            if (options.has(FirstSeatStarts))
                Then(FirstPlayerAction(game.seating.first))
            else
                Random[Faction](factions, FirstPlayerAction(_))

        case ShuffleStartingDecksAction(f :: rest) =>
            f.upgrades = $(ClanCard(f, 1), ClanCard(f, 2)) ++ game.warchiefCards.$(ClanCard(f, 3))

            val upgrades = f.upgrades

            if (Debug.upgradesInDeck)
                f.upgrades = $

            Shuffle[Card]((Cards.starting(game.colors(f)) :+ ClanCard(f, 0)) ++ Debug.upgradesInDeck.??(upgrades), ShuffledStartingDeckAction(f, _, rest))

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
                f.played = $
                f.foresaw = false
                f.conqueror = false
            }

            game.awakened = false

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

            if (stage == 0)
                Ask(f)
                    .each(handChoices(f, stage))(c => CardMenuAction(f, c, stage))
                    .add(PassAction(f))
            else
                Ask(f)
                    .each(handChoices(f, stage))(c => CardMenuAction(f, c, stage))
                    .add(EndTurnAction(f))

        // The hand stays shown with the card selected and its choices below; another card selects that one instead
        case CardSwitchAction(f, c, stage) =>
            cardMenu(f, c, stage)

        case CardMenuAction(f, c, stage) =>
            cardMenu(f, c, stage)

        case PlayCardAction(f, c, stage) =>
            f.hand = f.hand.diff($(c))
            f.active :+= c

            f.played :+= c

            f.log("played", c)

            game.note(c.name.replace(" ", "-"))

            // Opponents holding Defensive Strategy may cancel the card
            Then(DefensiveAskAction(f, c, stage, game.from(f).drop(1)))

        case PlayResolveAction(f, c, stage) =>
            val after = TurnAction(f, (c.flash && stage < 2).?(1).|(2))

            // Snake Clan may move the Scorched Earth token before resolving a clan card
            if (f == Snake && c.is[ClanCard] && MapExpansion.scorchable(f).any)
                Then(ScorchedAction(f, ResolveEffectAction(f, c.effect, after)))
            else
                Then(ResolveEffectAction(f, c.effect, after))

        case ResolveEffectAction(f, e, after) =>
            e match {
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

            // After Future Sight there is no card to take
            if (game.display.any && f.foresaw.not)
                Ask(f).each(game.display)(c => PickCardAction(f, c))
            else
                Then(PassedAction(f))

        case PickCardAction(f, c) =>
            game.display = game.display.diff($(c))
            f.draw = c +: f.draw

            f.log("took", c, "and placed it on top of their draw pile")

            Then(PassedAction(f))

        // After passing (the More Creatures variant may add a creature here)
        case PassedAction(f) =>
            Then(NextTurnAction(f))

        case NextTurnAction(f) =>
            game.from(f).drop(1).:+(f).%(_.passed.not).headOption match {
                case Some(n) => Then(TurnAction(n, 0))
                case None =>
                    game.highlight.current = None
                    game.display = $
                    Then(CreaturePhaseAction)
            }

        // 2.5 CREATURE PHASE (Creatures module, CreaturesExpansion)
        case CreaturePhaseAction =>
            Then(ScorchedHarvestAction)

        // 3. HARVEST
        case ScorchedHarvestAction =>
            val owner = game.scorched./(game.board.territory)./(game.present).|($).single.%(_ != Snake)
            val (food, wood, lore) = game.scorched./(game.board.territory)./(game.harvest).|((0, 0, 0))
            val l = $[(Resource, Int)](Food -> food, Wood -> wood, Lore -> lore).filter(_._2 > 0).map(_._1)

            if (factions.has(Snake) && owner.any && l.any)
                Ask(Snake).each(l)(r => ScorchedTakeAction(Snake, |(r))).add(ScorchedTakeAction(Snake, None))
            else
                Then(HarvestAction(None))

        case ScorchedTakeAction(f, r) =>
            Then(HarvestAction(r))

        case HarvestAction(take) =>
            log(SingleLine)
            log("Harvest")

            val victim = take.any.??(game.scorched./(game.board.territory)./(game.present).|($).single)

            game.from(game.first).foreach { f =>
                val territories = game.controlled(f)

                // A Wolf creature leaves only the buildings' fame and resources
                val fame = territories.%(game.board.closed).%(t => game.wolfIn(t).not)./(t => (game.board.tiles(t) >= 3).?(2).|(1)).sum
                if (fame > 0) {
                    f.fame += fame
                    f.log("gained", fame.hl, "fame from closed territories")
                }

                val altars = territories./~(game.buildingsIn).map(_._2).count(_ == AltarOfKings)
                if (altars > 0) {
                    f.fame += 3 * altars
                    f.log("gained", (3 * altars).hl, "fame from", AltarOfKings)
                }

                val (food, wood, lore) = territories./(game.harvest).foldLeft((0, 0, 0))((a, b) => (a._1 + b._1, a._2 + b._2, a._3 + b._3))
                f.food += food
                f.wood += wood
                f.lore += lore
                f.log("collected", food.hl, Food, Comma, wood.hl, Wood, "and", lore.hl, Lore)

                // The Scorched Earth resource goes to Snake Clan instead
                if (victim.has(f))
                    take.foreach { r =>
                        game.note("scorched-harvest")
                        f.gain(r, -1)
                        Snake.gain(r, 1)
                        Snake.log("took", 1.hl, r, "from", f, "with the", "Scorched Earth".hl, "token")
                    }
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

                    // Ten Unrest cards; with none left, lose 5 fame and discard the top card of the draw pile
                    if (factions./(_.unrest).sum < UnrestCard.supply) {
                        f.draw = UnrestCard +: f.draw

                        f.log("could not pay for", f.units.hl, "units and took", UnrestCard)
                    }
                    else {
                        f.fame -= 5
                        f.discard ++= f.draw.take(1)
                        f.draw = f.draw.drop(1)

                        f.log("could not pay for", f.units.hl, "units; with no", UnrestCard, "left, lost", 5.hl, "fame and discarded the top card of their draw pile")
                    }
                }
            }

            Then(EndOfYearAction)

        // 5. END OF YEAR
        case EndOfYearAction =>
            val rulers = game.domination.??(factions.%(f => game.strongholds(f).num >= 3))

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
