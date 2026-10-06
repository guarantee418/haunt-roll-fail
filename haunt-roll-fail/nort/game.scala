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
    def elem(implicit game : Game) : Elem = (name + (this != Automa).??(" Clan")).styled(game.colors.get(this)./(c => c : Styling).|(this))(styles.title)(xstyles.bold)
}

// The seven clans of the core game
case object Bear extends Faction
case object Boar extends Faction
case object Goat extends Faction
case object Raven extends Faction
case object Snake extends Faction
case object Stag extends Faction
case object Wolf extends Faction

// The solo opponent (Uncharted Horizons' Solo module, automa.scala): a neutral clan with two Leaders, played by its cards
case object Automa extends Faction

// The seven clans of the New Blood expansion (newblood.scala)
case object Dragon extends Faction
case object Horse extends Faction
case object Kraken extends Faction
case object Lynx extends Faction
case object Ox extends Faction
case object Rat extends Faction
case object Squirrel extends Faction


// Player colors mark a player's units and starting cards; they are not tied to the clan
trait PlayerColor extends NamedToString with Styling with Elementary with Record {
    def id = name.toLowerCase
    // The banner on the starting cards: orange has no cards of its own and uses yellow's
    def cards = this match {
        case Orange => "yellow"
        case c => c.id
    }
    override def elem : Elem = name.styled(this)
}

case object Blue extends PlayerColor
case object Red extends PlayerColor
case object Yellow extends PlayerColor
case object Purple extends PlayerColor
case object Green extends PlayerColor
// The sixth player's color; there are no orange starting cards, so orange uses the yellow ones
case object Orange extends PlayerColor

object PlayerColor {
    val all : $[PlayerColor] = $(Blue, Red, Yellow, Purple, Green, Orange)
}


// Shown as its icon (ui-food, ui-wood, ui-lore), with the name as the text alternative
trait Resource extends NamedToString with Styling with Elementary with Record {
    override def elem : Elem = Image("ui-" + name.toLowerCase, styles.inlineIcon).alt(name)
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

    // Units on the map, with the warchief (Warchiefs module); Horse Clan's second warchief, Brok, counts too
    def units = game.onMap(faction) + game.chiefs.contains(faction).??(1) + ((faction == Horse && game.brok.any) || (faction == Automa && game.leader2.any)).??(1) + game.blainnOf(faction).any.??(1) + SeaExpansion.raiders(faction)

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

// What a clan is set to collect at the next harvest, as things stand (shown in the player panels)
case class HarvestForecast(food : Int, wood : Int, lore : Int, fame : Int)

// Harvest amounts, shared by HarvestAction and the forecast in the player panels
object Harvest {
    // A Wolf creature leaves only the buildings' fame and resources;
    // the Wyvern's Den (Wilderness) gives its own fame instead;
    // Events: Frozen Sea gives 1 fame per territory instead; Bountiful Year's territory gives none
    // Wastelands: a Kobold's territory gives no fame, its buildings' included
    def territoryFame(f : Faction)(implicit game : Game) : Int = {
        val territories = game.controlled(f).%(t => game.koboldIn(t).not)

        if (game.eventIs("frozen-sea"))
            territories.%(t => game.wolfIn(t).not).%(t => Wild.denIn(t).not).num
        else
            territories.%(game.board.closed).%(t => game.wolfIn(t).not).%(t => Wild.denIn(t).not).%(EventsExpansion.fameFrom)./(t => (game.board.tiles(t) >= 3).?(2).|(1)).sum
    }

    def altarFame(f : Faction)(implicit game : Game) : Int = 3 * game.controlled(f).%(t => game.koboldIn(t).not)./~(game.working).count(_ == AltarOfKings)

    def resources(f : Faction)(implicit game : Game) : (Int, Int, Int) =
        game.controlled(f)./(EventsExpansion.harvest).foldLeft((0, 0, 0))((a, b) => (a._1 + b._1, a._2 + b._2, a._3 + b._3))

    // Leaves out the choices made during the harvest: Snake's Scorched Earth and the New Blood clan powers' extra resource
    def forecast(f : Faction)(implicit game : Game) : HarvestForecast = {
        // Dragon Clan harvests only after a sacrifice on its Pyre: nothing if it chose not to, or has nothing to sacrifice or place
        if (f == Dragon && (game.dragonHarvest.has(false) || (game.dragonHarvest.none && NewBloodExpansion.sacrificeOptions(f).not)))
            return HarvestForecast(0, 0, 0, 0)

        val (food, wood, lore) = resources(f)
        val (wildFame, wildFood) = game.has(Wilderness).?(WildernessExpansion.forecast(f)).|((0, 0))
        val (wasteFame, wasteFood, wasteLore) = game.has(Wastelands).?(WastelandsExpansion.forecast(f)).|((0, 0, 0))

        HarvestForecast(food + wildFood + wasteFood, wood, lore + wasteLore, territoryFame(f) + altarFame(f) + wildFame + wasteFame)
    }
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

// The six choices at the start of a turn; all but Pass then show the hand, where tapping a card does it
trait TurnMode extends NamedToString with Record
case object PlayMode extends TurnMode
case object WaitMode extends TurnMode
case object ReplaceMode extends TurnMode
case object RemoveMode extends TurnMode
case object UpgradeMode extends TurnMode

object LoreIcon {
    def apply() : Elem = Lore.elem
}

// Fame, shown as the crown of the fame tokens
object FameIcon {
    def apply() : Elem = Image("ui-fame", styles.inlineIcon).alt("fame")
}

object TurnModeLabel {
    def apply(m : TurnMode) : Elem = m match {
        case PlayMode => "Play cards".txt ~ " (" ~ 1.hl ~ " + any " ~ "⚡".hl ~ ")"
        case WaitMode => "Wait".txt
        case ReplaceMode => "Replace card for".txt ~ " " ~ 1.hl ~ " " ~ LoreIcon()
        case RemoveMode => "Remove card for".txt ~ " " ~ 2.hl ~ " " ~ LoreIcon()
        case UpgradeMode => "Upgrade for".txt ~ " " ~ 3.hl ~ " " ~ LoreIcon()
    }
}

case class TurnModeAction(self : Faction, mode : TurnMode) extends BaseAction("Your turn")(TurnModeLabel(mode)) with Soft

// Cards in hand once a choice is made, shown as their images: tapping one does it (no full screen view until the turn ends)
trait HandChoice

// stage 0: the first card of the turn, 1: after a Flash card, 2: after the main card (only Flash cards left)
case class PlayCardAction(self : Faction, card : Card, stage : Int) extends BaseAction((stage == 2).?("Play a Flash card").|("Play a card"))(card.handImg) with ViewObject[Card] with HandChoice { def obj = card }
case class WaitCardAction(self : Faction, card : Card) extends BaseAction("Wait with a card")(card.handImg) with ViewObject[Card] with HandChoice { def obj = card }
case class ReplaceCardAction(self : Faction, card : Card) extends BaseAction("Replace a card for", 1.hl, LoreIcon())(card.handImg) with ViewObject[Card] with HandChoice { def obj = card }
case class RemoveCardAction(self : Faction, card : Card) extends BaseAction("Remove a card from the game for", 2.hl, LoreIcon())(card.handImg) with ViewObject[Card] with HandChoice { def obj = card }
// Upgrade: first the clan upgrade to take, then the card from hand (one row), then Remove or Wait with it
case class UpgradePickAction(self : Faction, upgrade : Card) extends BaseAction("Upgrade for", 3.hl, LoreIcon(), Comma, "take")(upgrade.handImg) with Soft with ViewObject[Card] with HandChoice { def obj = upgrade }
case class UpgradeSacrificeAction(self : Faction, card : Card, upgrade : Card) extends BaseAction("Upgrade to", upgrade, Comma, "choose a card to remove or wait with")(card.handImg) with Soft with ViewObject[Card] with HandChoice { def obj = card }
case class UpgradeCardAction(self : Faction, card : Card, upgrade : Card, remove : Boolean) extends BaseAction("Upgrade to", upgrade, Comma, "with", card)(remove.?("Remove".txt ~ " (out of the game)").|("Wait".txt ~ " (to the played cards)"))
// Cards in hand while they can't be tapped: during your turn once a choice is made, they don't open full screen
case class HandInfoAction(self : Faction, title : Elem, card : Card) extends BaseInfo(title)(card.handImg) with ViewObject[Card] { def obj = card }
// The player's clan board below the Lore Tree, to look up the clan's power and its warchief; clicking it opens it full screen
case class ClanBoard(f : Faction)
// The Winter cost chart, opened from a player panel
case class WinterChart(f : Faction)
// A player's discard pile, opened from the action pane
case class DiscardPile(f : Faction)
// Tapping Dragon Clan's Sacrificial Pyre in its panel shows it full size
case object PyreView
case class DiscardPileInfoAction(self : Faction, title : Elem, n : Int) extends BaseInfo(title)((n == 0).?("empty".txt).|(n.hl ~ (n == 1).?(" card").|(" cards")) ~ " (tap to see)".spn(xstyles.smaller85)) with ViewObject[Faction] with OnClickInfo { def obj = self ; def param = DiscardPile(self) }
case class ClanBoardInfoAction(self : Faction, title : Elem) extends BaseInfo(title)(Image(Warchief.board(self), styles.boardInfo)) with ViewObject[Faction] with OnClickInfo { def obj = self ; def param = ClanBoard(self) }
// Cards shown while there is nothing to do with them; clicking one opens it full screen
case class CardInfoAction(self : Faction, title : Elem, card : Card) extends BaseInfo(title)(card.handImg) with ViewObject[Card] with OnClickInfo { def obj = card ; def param = card }
case class PassAction(self : Faction) extends BaseAction("Your turn")("Pass")
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
case class PickCardAction(self : Faction, card : Card) extends BaseAction("Take a card")(card.handImg) with ViewObject[Card] { def obj = card }

// Snake Clan may take one resource from the territory with its Scorched Earth token
case object ScorchedHarvestAction extends ForcedAction
case object CreaturePhaseAction extends ForcedAction
case class PassedAction(f : Faction) extends ForcedAction
case class ScorchedTakeAction(self : Faction, r : |[Resource]) extends BaseAction("Scorched Earth".hl, "take one resource from", ScorchedEarthPlace)(r./(_.elem).|("Take nothing".txt))
case class HarvestAction(take : |[Resource]) extends ForcedAction
case class AfterHarvestAction(then : ForcedAction) extends ForcedAction
case class TradeAction(f : Faction, then : ForcedAction) extends ForcedAction
case class TradeForAction(self : Faction, pay : $[Resource], gain : Resource, then : ForcedAction) extends BaseAction("Trade", "three resources for one")("Pay", pay./(_.elem).join(" "), "for", gain)
// Team play: one resource for one of a teammate's
case class TeamTradeAction(self : Faction, mate : Faction, give : Resource, take : Resource, then : ForcedAction) extends BaseAction("Trade with", mate, "one for one")("Give", give, "for", take)
case object WinterAction extends ForcedAction
case object EndOfYearAction extends ForcedAction
case object GameEndAction extends ForcedAction
case object NewYearAction extends ForcedAction
case class DominationAction(rulers : $[Faction]) extends ForcedAction

// Fame at the end of the game, by where it came from
case class FinalFame(tokens : Int, cards : Int, sets : Int, unrest : Int) {
    def total = tokens + cards + sets + unrest
}

// The end screen: the winners' clan cards (and warchief cards), the Jarl line and how they won
case class GameOverWonAction(self : Faction, winners : $[Faction], message : Elem) extends BaseInfo("Game Over")(message)


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

    // Uncharted Horizons' Training Fields duel (training.scala), played instead of the usual game
    val training = options.has(TrainingFieldsOption)

    // Modules and expansions turned on in the options
    // New Blood is on whenever one of its clans plays; Training Fields leaves every other module out
    val modules : $[Module] =
        if (training)
            $(TrainingFields)
        else
            Module.all.%(m => Meta.has(options, m) || (m == NewBlood && setup.exists(NewBlood.clans.has)) || (m == Solo && setup.has(Automa)))

    def has(m : Module) = modules.has(m)

    // Module expansions come first, so they can take over any core action; before them the Uncharted Horizons
    // Development cards, which only act on their own actions and specials
    val expansions : $[Expansion] = $(HorizonDevsExpansion) ++ modules.sortBy(_.priority)./~(_.expansion) ++ $(MapExpansion, CardsExpansion, CommonExpansion)

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

    // Three closed territories with large buildings win at the end of a year (not with the Alternative victory module)
    val domination = options.has(FameOnly).not && has(VictoryModule).not && (setup.has(Automa).not || options.has(AutomaLevelOption(1)))

    // Team play (2v2 with four players, 3v3 or 2v2v2 with six): seats go round the teams in turn,
    // so teammates sit opposite each other
    val teamModule : |[Module] = Module.teams.toList.%{ case (m, n) => has(m) && setup.num == n }.map(_._1).headOption

    val teams : Boolean = teamModule.any

    // The number of teams (each player alone without team play)
    val teamCount : Int = teamModule./(Module.sides).|(setup.num)

    def team(f : Faction) : Int = setup.indexOf(f) % teamCount

    def allied(f : Faction, g : Faction) : Boolean = f == g || (teams && team(f) == team(g))

    def enemy(f : Faction, g : Faction) : Boolean = allied(f, g).not

    // f's teammates, in seating order
    def mates(f : Faction) : $[Faction] = setup.%(g => g != f && allied(f, g))

    // The teams in seating order of their first player (each player alone without team play)
    def sides : $[$[Faction]] = setup./(team).distinct./(n => setup.%(team(_) == n))

    def teamName(f : Faction) : Elem = ("Team " + "ABC".charAt(team(f))).hl

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

    // The territory of the fight being resolved, drawn pink on the map
    var battle : |[AreaRef] = None

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

    // Units per player (ten in the Training Fields)
    val unitLimit = training.?(Training.units).|(14)

    // TRAINING FIELDS (training.scala): the tiles still face down, and each player's face-up Action cards
    var hiddenTiles : $[Placement] = $
    var drills : Map[Faction, $[Drill]] = Map()

    // Bear Clan's Kaija: where it is (None while in the reserve), and whether it may enter enemy territories this year
    var kaija : |[AreaRef] = None
    var awakened = false

    // Snake Clan's Scorched Earth token
    var scorched : |[AreaRef] = None

    // NEW BLOOD (newblood.scala)
    // Dragon Clan's Sacrificial Pyre: the owner of each unit on it (at most 2)
    var pyre : $[Faction] = $
    // Dragon Clan may harvest this year (it sacrificed or placed a unit on the Pyre); None until it has chosen
    var dragonHarvest : |[Boolean] = None
    // Kraken Clan's High Tide tokens (2 at most), each in a territory with Kraken figures
    var tides : $[AreaRef] = $
    // Ox Clan's Ancestral Equipment tokens: on the map (face up), the face-down pile, face up in the reserve, and used this year
    var gear : Map[SpaceRef, Int] = Map()
    var gearPile : $[Int] = 1.to(7).$
    var gearReady : $[Int] = $
    var gearUsed : $[Int] = $
    // The tokens Ox Clan uses in the fight being resolved
    var gearFight : $[Int] = $
    // The tile placed by the last Explore action (Warcraft)
    var explored : |[String] = None
    // Howl from the Sea: Kraken's first retreating group adds a unit where it goes
    var howl = false

    def tideIn(t : Territory) : Boolean = tides.exists(t.areas.contains)

    // SOLO MODULE (automa.scala): the Automa's pile and discard pile, the cards drawn this year (its Actions deck)
    // and those played, whether it drew this year, and the spot of its first tile
    var automaDeck : $[AutomaCard] = $
    var automaDiscard : $[AutomaCard] = $
    var automaActions : $[AutomaCard] = $
    var automaPlayed : $[AutomaCard] = $
    var automaDrawn = false
    var automaStart : |[Spot] = None

    // EVENTS MODULE (horizons.scala): the face-up deck, the event of this year, the steps already resolved this year,
    // and the Harvest changes the players chose (territories by their first area at the time)
    var eventDeck : $[EventCard] = $
    var event : |[EventCard] = None
    var eventsShuffled = false
    var eventSteps : $[String] = $
    var harvestSkip : $[AreaRef] = $
    var harvestDouble : $[AreaRef] = $
    var harvestLessFood : $[AreaRef] = $

    def eventIs(id : String) = event.exists(_.id == id)

    // Uncharted Horizons' Sea module (sea.scala): the Ports (the Beach tiles' land areas), their Raids, the Raid deck
    var raidDeck : $[RaidCard] = $
    var raidsShuffled = false
    var ports : $[AreaRef] = $
    var raids : Map[AreaRef, Raid] = Map()
    var beached : $[Faction] = $
    var raidKept : $[RaidKept] = $
    var raidSteps : $[String] = $
    // Uncharted Journey: exploring from open neutral territories too
    var raidExplore = false

    // ALTERNATIVE VICTORY MODULE: the cards in play, the validation counts (they never go down) and who validated each card
    var victory : $[VictoryCard] = $
    var progress : Map[Faction, Map[String, Int]] = Map()

    def progressOf(f : Faction, k : String) : Int = progress.getOrElse(f, Map()).getOrElse(k, 0)

    def advance(f : Faction, k : String, n : Int = 1) {
        if (n > 0 && has(VictoryModule))
            progress += f -> (progress.getOrElse(f, Map()) + (k -> (progressOf(f, k) + n)))
    }

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

    // Wastelands: a territory with a Kobold gives no fame at the Harvest
    def koboldIn(t : Territory) : Boolean = creaturesIn(t).exists(_.kind == Kobold)

    // A Spectral Warrior (Wilderness): the buildings in its territory have no effect
    def ghostIn(t : Territory) : Boolean = creaturesIn(t).exists(_.kind == SpectralWarrior)

    // Wilderness: the Spectral Warriors not yet placed by the Ancestral Graveyard
    var spectrals : $[Creature] = $

    // WASTELANDS (wastelands.scala): the central tile; Jötunn Blainn's owner and area once recruited, and the Jötnar Camp
    // he waits on; the Naströnd tiles whose 2 wood nobody has taken yet; the year the Start of Year steps were made and the
    // steps done this year; the Wyvern placed on the central Wyvern's Den
    var central = "start"
    var blainn : |[(Faction, AreaRef)] = None
    var jotnarCamp : |[Spot] = None
    var nastrond : $[Spot] = $
    var wasteYear = 0
    var wasteSteps : $[String] = $
    var denWyvern = false

    // Wilderness and Wastelands together: the twelve Environment tiles drawn from both
    var environment : |[$[String]] = None

    def blainnOf(f : Faction) : |[AreaRef] = blainn.%(_._1 == f)./(_._2)

    def blainnIn(t : Territory, f : Faction) : Boolean = blainnOf(f).exists(t.areas.contains)

    // The Swamp (Wilderness): units may pass through it but not stay
    def swampIn(t : Territory) : Boolean = t.areas.exists(Wild.swamp)

    // Units of f may only pass through t: a teammate's territory or the Swamp
    def passOnly(f : Faction, t : Territory) : Boolean = mateHeld(f, t) || swampIn(t)

    // Buildings that work: none where a Spectral Warrior is
    def working(t : Territory) : $[Building] = ghostIn(t).not.??(buildingsIn(t).map(_._2))

    def unitsAt(a : AreaRef) : Map[Faction, Int] = units.getOrElse(a, Map())

    // Units only; Kaija is counted separately
    def count(t : Territory, f : Faction) : Int = t.areas./(a => unitsAt(a).getOrElse(f, 0)).sum

    def kaijaIn(t : Territory, f : Faction) : Boolean = companion(f).exists(t.areas.contains)

    // New Blood: Lynx Clan's Brundr and Kaelinn token, and Horse Clan's second warchief, Brok (Warchiefs module),
    // follow Kaija's rules as each clan's companion figure (moved, recruited and removed like it)
    var lynx : |[AreaRef] = None
    var brok : |[AreaRef] = None
    // The Automa's Leader 2 (Leader 1 is its warchief, in chiefs)
    var leader2 : |[AreaRef] = None

    def companion(f : Faction) : |[AreaRef] = f match {
        case Bear => kaija
        case Lynx => lynx
        case Horse => brok
        case Automa => leader2
        case _ => None
    }

    def setCompanion(f : Faction, a : |[AreaRef]) = f match {
        case Bear => kaija = a
        case Lynx => lynx = a
        case Horse => brok = a
        case Automa => leader2 = a
        case _ =>
    }

    // Every companion on the map, with its clan
    def companions : $[(Faction, AreaRef)] = $(Bear, Lynx, Horse, Automa)./~(f => companion(f)./(f -> _))

    // Combat points of the companion: Kaija 2, Brundr and Kaelinn 1, Brok 2 (Eitria and Brok together are worth 3)
    def companionStrength(t : Territory, f : Faction) : Int =
        if (kaijaIn(t, f).not) 0
        else f match {
            case Lynx => 1
            case Horse => chiefIn(t, f).?(1).|(2)
            case Automa => AutomaExpansion.leaderStrength
            case _ => 2
        }

    // Kaija's rule: no entering enemy territories unless awakened (the other companions may)
    def restrained(f : Faction) : Boolean = f == Bear && awakened.not

    // Warchiefs module: where each clan's warchief is (none while in the reserve)
    var chiefs : Map[Faction, AreaRef] = Map()

    // Legal tile placements by game state (Rules.placements)
    val placementCache = scala.collection.mutable.Map[Any, $[(Spot, Int)]]()

    def chiefIn(t : Territory, f : Faction) : Boolean = chiefs.get(f).exists(t.areas.contains)

    // The warchief is in the reserve and can be recruited
    // The Automa's Leader 1 plays as its warchief, with or without the module
    def chiefReady(f : Faction) : Boolean = (has(Warchiefs) || f == Automa) && chiefs.contains(f).not && setup.has(f)

    // Units, Kaija and the warchief
    def figures(t : Territory, f : Faction) : Int = count(t, f) + kaijaIn(t, f).??(1) + chiefIn(t, f).??(1) + blainnIn(t, f).??(1)

    // Combat points of the figures: Kaija is worth 2, a warchief 2 or 3 depending on its power
    // Jötunn Blainn (Wastelands) is worth 2
    // Sea module: +1 defending a Port
    def strength(t : Territory, f : Faction, attacking : Boolean) : Int = count(t, f) + companionStrength(t, f) + Warchief.strength(t, f, attacking) + blainnIn(t, f).??(2) + SeaExpansion.defense(t, f, attacking)

    // Kaija is in Bear Clan's reserve and can be recruited
    // Brundr and Kaelinn likewise; Brok only with the Warchiefs module
    // The Automa's Leader 2 likewise, always
    def kaijaReady(f : Faction) : Boolean = (f == Bear || f == Lynx || f == Automa || f == Horse && has(Warchiefs)) && companion(f).none && setup.has(f)

    def scorchedIn(t : Territory) : Boolean = scorched.exists(t.areas.contains)

    def present(t : Territory) : $[Faction] = seating.%(f => figures(t, f) > 0)

    // Units or Kaija anywhere on the map
    def anyOnMap(f : Faction) : Boolean = onMap(f) > 0 || companion(f).any || chiefs.contains(f)

    def controlled(f : Faction) : $[Territory] = board.territories.%(t => present(t) == $(f))

    // Team play: a territory held by a teammate of f (f's units can only pass through it)
    def mateHeld(f : Faction, t : Territory) : Boolean = present(t).exists(g => g != f && allied(f, g))

    // Teammates' territories and the Swamp with f's figures passing through during a Move
    def passing(f : Faction) : $[Territory] = board.territories.%(t => figures(t, f) > 0 && passOnly(f, t))

    def onMap(f : Faction) : Int = units.values./(_.getOrElse(f, 0)).sum

    // Units on Dragon Clan's Sacrificial Pyre are neither on the map nor in the reserve
    // Sea module: units away on Raids neither
    def reserve(f : Faction) : Int = unitLimit - onMap(f) - pyre.count(f) - SeaExpansion.raiders(f)

    def addUnits(a : AreaRef, f : Faction, n : Int) {
        val m = unitsAt(a)
        units += a -> (m + (f -> (m.getOrElse(f, 0) + n)))
    }

    // Casualties: units first, then the warchief, Jötunn Blainn (back to his camp), Kaija last
    def removeFigures(t : Territory, f : Faction, n : Int) {
        val k = math.min(n, count(t, f))
        removeUnits(t, f, k)
        var left = n - k
        if (left > 0 && chiefIn(t, f)) {
            chiefs -= f
            left -= 1
        }
        if (left > 0 && blainnIn(t, f)) {
            blainn = None
            left -= 1
        }
        if (left > 0 && kaijaIn(t, f))
            setCompanion(f, None)
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
        val here = working(t)
        (specs./(_.food).sum + here.count(_ == FoodSilo), specs./(_.wood).sum + here.count(_ == WoodcutterLodge), specs./(_.lore).sum + here.count(_ == CarvedStone))
    }

    // What a territory gives at harvest (and when collecting as at harvest): a Wolf leaves only the buildings' resources
    def harvest(t : Territory) : (Int, Int, Int) = {
        if (wolfIn(t).not)
            return produce(t)

        val here = working(t)
        (here.count(_ == FoodSilo), here.count(_ == WoodcutterLodge), here.count(_ == CarvedStone))
    }

    // Against the Automa, level 1 is won with four such territories, and other levels have no sudden win
    val strongholdsToWin = setup.has(Automa).?(4).|(3)

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
        def shown(a : UserAction) : Action = a.as[UnavailableReasonAction]./(_.action : Action).|(a.unwrap)
        // The hand is shown as choices once a turn's choice is made
        val choosing = actions.exists(a => shown(a).is[HandChoice] && shown(a).is[UpgradePickAction].not)
        // The clan upgrades are the choices after picking Upgrade
        val inLoreTree = actions.exists(a => shown(a).is[UpgradePickAction])
        // During your turn, past the six choices, cards in hand don't open full screen
        val turn = game.highlight.current == self && actions.exists(_.unwrap.is[PassAction]).not

        // Training Fields: the Action cards, face up and face down (on your turn they are the choices),
        // and what a Refresh would score the opponent
        if (training)
            return self.%(states.contains)./~(f =>
                $(Info("Victory points".styled(colors(f)), f.fame.hlb, "of", Training.goal.hl)) ++
                $(Info("A Refresh now scores", Training.opponent(f)(this), Training.score(Training.opponent(f)(this))(this).hlb, "VP")) ++
                actions.exists(a => shown(a).is[DrillPickAction] || shown(a).is[DrillPlayAction]).not.??(
                    Drill.all./(d => DrillInfoAction(f, "Action cards".styled(colors(f)) ~ " (" ~ drills(f).num.hl ~ " face up)", d)))
            )

        (year > 0).$(Info("Year", year.hlb, "of", lastYear.hl)) ++
        self.%(states.contains)./~(f =>
            (choosing || inLoreTree).not.??(f.hand./(c => turn.?(HandInfoAction(f, "Your hand".styled(colors(f)), c) : Info).|(CardInfoAction(f, "Your hand".styled(colors(f)), c)))) ++
            f.active./(c => CardInfoAction(f, "Played".styled(colors(f)), c)) ++
            inLoreTree.not.??(f.upgrades./(u => CardInfoAction(f, "Lore Tree".styled(colors(f)) ~ " (" ~ f.lore.hl ~ " " ~ LoreIcon() ~ ")", u))) ++
            $(ClanBoardInfoAction(f, "Clan board".styled(colors(f)))) ++
            $(DiscardPileInfoAction(f, "Discard pile".styled(colors(f)), f.discard.num)) ++
            $(Info("Fame", f.fame.hlb))
        )
    }

    def convertForLog(s : $[Any]) : $[Any] = s./~{
        case Empty => None
        case NotInLog(_) => None
        case AltInLog(_, m) => |(m)
        // Cards (played developments, clan and warchief cards, events, creatures...) can be tapped in the log to see them
        case c : Card => |(OnClick(c, c.elem.spn(styles.tappable)(xlo.pointer)))
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

    // The sides (teams, or players alone) with the best sums of the first key, then of the next ones for ties
    def best(sides : $[$[Faction]], keys : $[Faction => Int]) : $[$[Faction]] =
        keys.foldLeft(sides)((l, k) => l.%(s => s./(k).sum == l./(_./(k).sum).max))

    def buildings(f : Faction)(implicit game : Game) = game.controlled(f)./~(game.buildingsIn).num

    // Final fame: (fame tokens, cards, resources, Unrest)
    def finalFame(f : Faction)(implicit game : Game) : FinalFame =
        FinalFame(f.fame, f.deck./(c => cardFame(f, c)).sum, f.resources / 3, -5 * f.unrest)

    // The tie-breaks after fame, as said on the end screen
    val tieBreaks = $("territories controlled", "units", "buildings")

    // How a clan's fame adds up, for the end screen
    def fameWhy(f : Faction)(implicit game : Game) : Elem = {
        val FinalFame(tokens, cards, sets, unrest) = finalFame(f)
        $(
            Some(tokens.hl ~ " from fame tokens"),
            (cards != 0).?(cards.hl ~ " from cards"),
            (sets != 0).?(sets.hl ~ " from resources (" ~ 1.hl ~ " per " ~ 3.hl ~ ")"),
            (unrest != 0).?(unrest.hl ~ " from " ~ f.unrest.hl ~ " " ~ UnrestCard.elem ~ " card" ~ (f.unrest > 1).??("s"))
        ).flatten.join(", ")
    }

    // Clans as "A, B and C"
    def and(l : $[Faction])(implicit game : Game) : Elem = (l.num > 1).?(l.dropRight(1)./(_.elem).join(", ") ~ " and " ~ l.last.elem).|(l.head.elem)

    // The end of the game: the winners' clan cards (and warchief cards), the Jarl line, then how they won
    def victory(winners : $[Faction], how : $[Elem])(implicit game : Game) : Continue = {
        val cards = winners.but(Automa)./~(f => $(ClanCard(f, 0)) ++ game.warchiefCards.$(ClanCard(f, 3)))./(c => Image(c.info.image, styles.winnerCard))
        val who = and(winners)
        val line = (winners.num > 1).?(who ~ " tame these lands and triumph as the supreme " ~ "Jarls".hl).|(who ~ " tames these lands and triumphs as the supreme " ~ "Jarl".hl)

        val message = cards.join(" ").div ~ line.div(styles.winnerLine) ~ how./(_.div).merge

        GameOver(winners, "Game Over" ~ Break ~ winners./(_.elem).join(Break) ~ Break ~ "won", $(GameOverWonAction(null, winners, message)))
    }

    // A card's fame at the end of the game (the Achievements count what f has then)
    def cardFame(f : Faction, c : Card)(implicit game : Game) : Int = {
        val territories = game.controlled(f)
        val built = territories./~(game.buildingsIn).map(_._2)
        lazy val spaces = territories./~(_.areas)./~(a => game.board.spec(a).spaces.indices./(i => SpaceRef(a, i))).%(s => game.buildings.contains(s).not)
        lazy val (food, wood, lore) = territories./(game.produce).foldLeft((0, 0, 0))((a, b) => (a._1 + b._1, a._2 + b._2, a._3 + b._3))

        c match {
            case Achievement("builder") => built.count(_.large.not) + 3 * built.count(_.large)
            case Achievement("explorer") => spaces.count(s => game.board.spec(s.area).spaces(s.index).kind != LargeSpace) + 3 * spaces.count(s => game.board.spec(s.area).spaces(s.index).kind == LargeSpace)
            case Achievement("food-trader") => 2 * food
            case Achievement("wood-trader") => 2 * wood
            case Achievement("scholar") => 2 * lore
            case Achievement("trapper") => 3 * territories./~(_.areas).count(a => game.board.spec(a).lair)
            case Achievement("warlord") => game.states(f).units
            case Achievement("mountaineer") => 2 * territories.count(HorizonDevsExpansion.rough)
            case Achievement("sailor") => 2 * territories.count(game.board.open)
            case c => c.fame
        }
    }

    // Each team, or each player without team play
    def sides(l : $[Faction])(implicit game : Game) : $[$[Faction]] = game.teams.?(game.sides./(_.%(l.has)).%(_.any)).|(l./(f => $(f)))

    // Only cards whose effect is implemented can be played
    def playable(f : Faction, c : Card)(implicit game : Game) : Boolean = playableEffect(f, c.effect)

    // The start of a turn: play cards, wait, replace, remove, upgrade or pass
    def turnModes(f : Faction)(implicit game : Game) : Continue =
        Ask(f)
            .add(TurnModeAction(f, PlayMode).!(f.hand.exists(playable(f, _)).not, "no card can be played"))
            .add(TurnModeAction(f, WaitMode).!(f.hand.none, "no cards"))
            .add(TurnModeAction(f, ReplaceMode).!(f.hand.none || f.lore < 1, "needs 1 lore"))
            .add(TurnModeAction(f, RemoveMode).!(f.hand.exists(_.removable).not || f.lore < 2, "needs 2 lore"))
            .add(TurnModeAction(f, UpgradeMode).!(f.hand.none || f.upgrades.none || f.lore < 3, "needs 3 lore"))
            .add(PassAction(f))

    // The hand after a choice: tapping a card does it; Cancel goes back to the six choices
    def modeChoices(f : Faction, m : TurnMode)(implicit game : Game) : Continue = m match {
        case PlayMode => playChoices(f, 0).cancel
        case WaitMode => Ask(f).each(f.hand.distinct)(c => WaitCardAction(f, c)).cancel
        case ReplaceMode => Ask(f).each(f.hand.distinct)(c => ReplaceCardAction(f, c)).cancel
        case RemoveMode => Ask(f).each(f.hand.distinct)(c => RemoveCardAction(f, c).!(c.removable.not, "can't be removed")).cancel
        case UpgradeMode => Ask(f).each(f.upgrades)(u => UpgradePickAction(f, u)).cancel
    }

    // The cards in hand to play, the ones that can't be played now dimmed
    def playChoices(f : Faction, stage : Int)(implicit game : Game) : Ask = {
        val l = handChoices(f, stage).%(playable(f, _))
        Ask(f).each(f.hand.distinct)(c => PlayCardAction(f, c, stage).!(l.has(c).not, (stage == 2 && c.flash.not).?("only Flash cards now").|("can't be played now")))
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

            if (version != gaming.version)
                log("Saved game version", version.hlb)

            options.%(_.is[ColorOption].not).foreach { o =>
                log(o.group, o.valueOn)
            }

            game.setup.foreach { f =>
                game.states += f -> new FactionState(f)
            }

            Shuffle[Card](Cards.earlyCards.diff(options.has(NoDrawDevelopments).??(NoDrawDevelopments.cards)) ++ options.has(HorizonsDevelopments).??(Cards.horizonsEarlyCards), ShuffledEarlyAction(_))

        case ShuffledEarlyAction(l) =>
            game.developments = l.take(game.earlyPerPlayer * factions.num)

            Shuffle[Card](Cards.advancedCards.diff(options.has(NoDrawDevelopments).??(NoDrawDevelopments.cards)) ++ options.has(HorizonsDevelopments).??(Cards.horizonsAdvancedCards), ShuffledAdvancedAction(_))

        case ShuffledAdvancedAction(l) =>
            // With six players in a long game there aren't enough Early cards: Advanced ones make up the difference
            val short = game.earlyPerPlayer * factions.num - game.developments.num
            game.developments ++= l.take(game.advancedPerPlayer * factions.num + short)

            Shuffle[Card](Cards.achievementCards ++ options.has(HorizonsDevelopments).??(Cards.horizonsAchievementCards), ShuffledAchievementsAction(_))

        case ShuffledAchievementsAction(l) =>
            game.achievements = l.take(factions.num)

            Then(ShuffleStartingDecksAction(factions))

        case ShuffleStartingDecksAction(Nil) =>
            // A random action must come from Random, even with one choice: the client can't perform it directly
            if (options.has(FirstSeatStarts))
                Random[Faction]($(game.seating.first), FirstPlayerAction(_))
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

            // Players 4 to 6 start with one more food
            game.from(f).zipWithIndex.foreach { case (f, i) =>
                f.food = (i < 3).?(2).|(3)
                f.wood = 2

                f.log("plays", game.colors(f), game.teams.?("in " ~ game.teamName(f)).|(Empty), "and starts with", f.food.hl, Food, "and", f.wood.hl, Wood)
            }

            Shuffle[String](Tiles.regular./(_.id) ++ options.has(HorizonsTiles).??(Tiles.horizons./(_.id)), ShuffledTilesAction(_))

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

            // Each controlled Forge draws one more card; the Automa draws its own cards (automa.scala)
            Then(game.from(game.first).but(Automa).foldRight(RevealDevelopmentsAction : ForcedAction)((f, then) => DrawCardsAction(f, 4 + game.controlled(f)./~(game.working).count(_ == Forge) + game.has(Wastelands).??(WastelandsExpansion.extraDraw(f)), then)))

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
                turnModes(f)
            else
                playChoices(f, stage).add(EndTurnAction(f))

        case TurnModeAction(f, m) =>
            modeChoices(f, m)

        case UpgradePickAction(f, u) =>
            Ask(f).each(f.hand.distinct)(c => UpgradeSacrificeAction(f, c, u)).cancel

        case UpgradeSacrificeAction(f, c, u) =>
            Ask(f)
                .add(UpgradeCardAction(f, c, u, true).!(c.removable.not, "can't be removed from the game"))
                .add(UpgradeCardAction(f, c, u, false))
                .cancel

        case PlayCardAction(f, c, stage) =>
            f.hand = f.hand.diff($(c))
            f.active :+= c

            f.played :+= c

            f.log("played", c)

            game.note(c.name.replace(" ", "-"))

            // Opponents holding Defensive Strategy may cancel the card
            Then(DefensiveAskAction(f, c, stage, game.from(f).drop(1).%(game.enemy(f, _))))

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

            // After Future Sight there is no card to take; the last card left is still offered, so the player sees what they take
            if (game.display.any && f.foresaw.not)
                Ask(f).each(game.display)(c => PickCardAction(f, c)).when(game.display.num == 1)(HiddenOkAction)
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
            val owner = game.scorched./(game.board.territory)./(game.present).|($).single.%(game.enemy(Snake, _))
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

            // Dragon Clan harvests nothing without a sacrifice on its Pyre
            game.from(game.first).%(f => f != Dragon || game.dragonHarvest.has(false).not).foreach { f =>
                val fame = Harvest.territoryFame(f)
                if (fame > 0) {
                    f.fame += fame
                    f.log("gained", fame.hl, FameIcon(), "from closed territories")
                }

                val altars = Harvest.altarFame(f)
                if (altars > 0) {
                    f.fame += altars
                    f.log("gained", altars.hl, FameIcon(), "from", AltarOfKings)
                }

                val (food, wood, lore) = Harvest.resources(f)
                f.food += food
                f.wood += wood
                f.lore += lore
                f.log("collected", food.hl, Food, Comma, wood.hl, Wood, "and", lore.hl, Lore)

                // Environment tiles (Wilderness): Ruins and the Wyvern's Den give fame, the Great Lake food
                if (game.has(Wilderness))
                    WildernessExpansion.harvest(f)

                // Wastelands: Yggdrasil and the central Wyvern's Den give fame, the Great Lake food, the Relic of the Gods lore
                if (game.has(Wastelands))
                    WastelandsExpansion.harvest(f)

                // The Scorched Earth resource goes to Snake Clan instead
                if (victim.has(f))
                    take.foreach { r =>
                        game.note("scorched-harvest")
                        f.gain(r, -1)
                        Snake.gain(r, 1)
                        Snake.log("took", 1.hl, r, "from", f, "with the", "Scorched Earth".hl, "token")
                    }
            }

            // New Blood's powers after harvesting (Dragon, Squirrel), then the trades
            Then(AfterHarvestAction(game.from(game.first).foldRight(WinterAction : ForcedAction)((f, then) => TradeAction(f, then))))

        case AfterHarvestAction(then) =>
            Then(then)

        case TradeAction(f, then) =>
            val pp = payments(f)
            // Team play: 1:1 with a teammate
            val swaps = game.mates(f)./~(g => Resource.all.%(f.has(_) > 0)./~(r => Resource.all.%(_ != r).%(g.has(_) > 0)./(x => TeamTradeAction(f, g, r, x, then))))

            if (pp.none && swaps.none)
                Then(then)
            else
                Ask(f)
                    .some(pp)(p => Resource.all./(r => TradeForAction(f, p, r, then)))
                    .add(swaps)
                    .done(then)

        case TeamTradeAction(f, g, r, x, then) =>
            f.gain(r, -1)
            g.gain(r, 1)
            g.gain(x, -1)
            f.gain(x, 1)

            f.log("gave", 1.hl, r, "to", g, "for", 1.hl, x)

            game.note("team-trade")

            Then(TradeAction(f, then))

        case TradeForAction(f, p, r, then) =>
            p.foreach(x => f.gain(x, -1))
            f.gain(r, 1)

            f.log("traded", p./(_.elem).join(" "), "for", r)

            Then(TradeAction(f, then))

        // 4. WINTER
        case WinterAction =>
            log(SingleLine)
            log("Winter")

            // The Automa pays no Winter costs
            game.from(game.first).but(Automa).foreach { f =>
                // Events: Harsh Winter and Blizzard
                val (food, wood) = EventsExpansion.winterCost(f)

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

                        f.log("could not pay for", f.units.hl, "units; with no", UnrestCard, "left, lost", 5.hl, FameIcon(), "and discarded the top card of their draw pile")
                    }
                }
            }

            Then(EndOfYearAction)

        // 5. END OF YEAR
        case EndOfYearAction =>
            val rulers = game.domination.??(factions.%(f => game.strongholds(f).num >= game.strongholdsToWin))

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

            // Team play: the rulers' teams win, with their teammates
            val contenders = game.teams.?(sides(factions).%(_.exists(rulers.has))).|(sides(rulers))

            // Ties: fame, then territories controlled, then units, then buildings (added up for teams)
            val winners = best(contenders, $(f => f.fame, f => game.controlled(f).num, f => f.units, f => buildings(f))).flatten

            game.isOver = true
            game.highlight.current = winners.single

            winners.foreach(f => f.log("won"))

            Debug.summary(game)

            val held = winners.%(rulers.has)./(f => f.elem ~ " held " ~ game.strongholds(f)./(_.areas.head.elem).join(", "))
            val tied = (contenders.num > 1).?(("Several clans did, so the most fame decided" + game.teams.??(" (added up for teams)") + ".").txt)

            victory(winners, $("Won in year " ~ game.year.hl ~ " by controlling " ~ game.strongholdsToWin.hl ~ " closed territories with large buildings.") ++ held ++ tied)

        case GameEndAction =>
            log(DoubleLine)
            log("End of the game")

            val totals = factions./{ f =>
                val FinalFame(tokens, cards, sets, unrest) = finalFame(f)
                val total = finalFame(f).total

                f.log("scored", total.hlb, FameIcon() ~ ":", tokens.hl, "from tokens,", cards.hl, "from cards,", sets.hl, "from resources,", unrest.hl, "from", UnrestCard)

                f -> total
            }.toMap

            // Team play: teammates add their scores together
            if (game.teams)
                sides(factions).foreach { l =>
                    log(game.teamName(l.head), l./(_.elem).join(", "), "scored", l./(totals).sum.hlb, FameIcon(), "together")
                }

            // Ties: territories controlled, then units, then buildings (added up for teams)
            val keys = $[Faction => Int](f => totals(f), f => game.controlled(f).num, f => f.units, f => buildings(f))
            val winners = best(sides(factions), keys).flatten

            game.isOver = true
            game.highlight.current = winners.single

            winners.foreach(f => f.log("won"))

            Debug.summary(game)

            // How they won: their fame and where it came from, the runner-up, and the tie-break that decided it
            val side = sides(factions).%(_.exists(winners.has))
            val others = sides(factions).diff(side)
            val score = winners./(totals).sum
            val runner = others.any.?(others.maxBy(_./(totals).sum))
            val decider = 1.until(keys.num).find(i => best(sides(factions), keys.take(i)).num > 1 && best(sides(factions), keys.take(i + 1)).num < best(sides(factions), keys.take(i)).num)

            val how =
                (winners.num > 1 || game.teams).?(
                    $("Won with the most fame: " ~ score.hlb ~ " together.") ++ winners./(f => f.elem ~ ": " ~ totals(f).hlb ~ " fame, " ~ fameWhy(f) ~ ".")
                ).|(
                    $("Won with the most fame, " ~ score.hlb ~ ": " ~ fameWhy(winners.head) ~ ".")
                ) ++
                runner./(l => decider./(i => "Tied with " ~ and(l) ~ ", and won on " ~ tieBreaks(i - 1).hl ~ ".").|("Next: " ~ and(l) ~ " with " ~ l./(totals).sum.hl ~ " fame.")).$

            victory(winners, how)

        case _ => UnknownContinue
    }
}
