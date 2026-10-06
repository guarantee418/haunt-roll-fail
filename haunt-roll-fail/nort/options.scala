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

import hrf.meta._
import hrf.options._
import hrf.elem._

import nort.elem._


// Game setup options, chosen on the setup screen after the clans

// Each clan's player color; picking a color takes it away from any other clan
case class ColorOption(clan : Faction, color : PlayerColor) extends GameOption {
    val group = (clan.name + " Clan color").txt
    def valueOn = color.elem
    override def forcedOff(all : $[BaseOption]) = all.of[ColorOption].%(o => o != this && (o.clan == clan || o.color == color))
}

object ColorOption {
    val all : $[ColorOption] = Meta.factions./~(f => PlayerColor.all./(c => ColorOption(f, c)))
}

// A clan played by the "Robotos" bot, which cheats (robotos.scala): no Winter costs, creatures ignore it, one more
// unit with every Recruit. Not on the setup screen: the game gets it for each clan set to "Bot / Robotos"
// (Meta.botOptions, called by startGame in hrf.scala), so every client and every replay plays by the same rules
case class RobotosOption(clan : Faction) extends GameOption {
    val group = "Robotos".txt
    def valueOn = (clan.name + " Clan").txt
}

object RobotosOption {
    val all : $[RobotosOption] = Meta.factions./(RobotosOption(_))
}

// The Automa's difficulty (Solo module): 1, the player also wins with four closed territories with large buildings;
// 3 and up need the Creatures module; from 4, the Automa draws one more card each year per level above 3
case class AutomaLevelOption(level : Int) extends GameOption with OneOfGroup with ImportantOption {
    val group = "Automa difficulty".txt
    def valueOn = ("Level " + level).hlb
    override def decorate(e : Elem) = e ~ (level match {
        case 1 => " (or four closed territories with large buildings)"
        case 3 => " (Creatures module)"
        case n if n >= 4 => " (Creatures module, " + (n - 3) + " more card" + (n > 4).??("s") + ")"
        case _ => ""
    }).spn
    override val explain = $(
        "Level 1: win with four closed territories each with a large building, or with more fame than the Automa after the last year.",
        "Level 2: win with more fame after the last year. Level 3: the same with the " ~ "Creatures".hl ~ " module. Level 4 and up: the Automa draws one more card each year per level above 3.",
    )
}

object AutomaLevelOption {
    val all = 1.to(6).$./(AutomaLevelOption(_))
}

// Number of years; developments revealed scale with it (Game.earlyPerPlayer, Game.advancedPerPlayer)
case class YearsOption(years : Int) extends GameOption with OneOfGroup with ImportantOption {
    val group = "Game length".txt
    def valueOn = years.hlb ~ " years"
    override def decorate(e : Elem) = e ~ (years == 7).?(" (standard)".spn).|(Empty) ~ (years == 10).?(" (long variant)".spn).|(Empty)
    override val explain = $(
        "The standard game lasts " ~ 7.hl ~ " years: " ~ 2.hl ~ " Early and " ~ 4.hl ~ " Advanced Development cards per player, the Achievement cards in the last year.",
        "The rulebook's long variant lasts " ~ 10.hl ~ " years, with " ~ 3.hl ~ " Early and " ~ 6.hl ~ " Advanced cards per player, and is usually played with " ~ "Fame victory only".hl ~ ".",
        "Other lengths are not in the rulebook. One Development card per player is revealed each year except the last; about a third of them are Early cards.",
    )
}

object YearsOption {
    val all : $[YearsOption] = 5.to(10).$./(YearsOption(_))
    val standard = YearsOption(7)
}

// VICTORY CONDITIONS: one of the four ways to win, then (Alternative victory) the mode and, if chosen by hand, the cards
trait VictoryChoice extends GameOption with OneOfGroup with ImportantOption {
    val group = "Victory conditions".txt
}

// The core rules: three closed territories each with a large building at the end of a year, otherwise the most fame
case object StandardVictory extends VictoryChoice {
    def valueOn = "Three territories with large buildings, or fame".txt
    override val explain = $(
        "The core rules: a player who controls three closed territories, each with a large building, wins at the end of a year.",
        "Otherwise the player with the most fame wins at the end of the last year.",
    )
}

case object FameOnly extends VictoryChoice {
    def valueOn = "Fame victory only".txt
    override val explain = $(
        "Only fame counts, at the end of the last year; three closed territories with large buildings don't win.",
        "The rulebook suggests it for the " ~ 10.hl ~ "-year game.",
    )
}

// Uncharted Horizons' Alternative victory conditions with the cards drawn at random, as the rulebook says
case object AltVictoryRandom extends VictoryChoice {
    def valueOn = "Alternative victory, random cards".txt
    override def decorate(e : Elem) = e ~ " " ~ "(Uncharted Horizons)".spn(xstyles.smaller85)
    override val explain = VictoryModule.about ++ $("One Map Control card and two Wealth cards (three with teams) are drawn at random at setup, as in the rulebook.".txt)
}

// The same with the cards chosen below (VictoryCardOption); Meta.validateFactionSeatingOptions checks their number
case object AltVictoryChosen extends VictoryChoice {
    def valueOn = "Alternative victory, chosen cards".txt
    override def decorate(e : Elem) = e ~ " " ~ "(Uncharted Horizons)".spn(xstyles.smaller85)
    override val explain = VictoryModule.about ++ $("Choose exactly one Map Control card and two Wealth cards (three with teams) below.".txt)
}

object VictoryChoice {
    val alternative : $[VictoryChoice] = $(AltVictoryRandom, AltVictoryChosen)
}

// The Alternative victory mode
case class VictoryModeOption(jarl : Boolean) extends GameOption with ImportantOption {
    val group = "Victory conditions".txt
    def valueOn = jarl.?("Jarl: all three cards").|("Thane: Map Control and one Wealth card").txt
    override val explain = $("Needs " ~ "Alternative victory".hl ~ ". Thane is the default.")
    override def required(all : $[BaseOption]) = VictoryChoice.alternative./(o => $[BaseOption](o))
    override def forcedOff(all : $[BaseOption]) = $(VictoryModeOption(jarl.not))
}

// An Alternative victory card chosen by hand
case class VictoryCardOption(card : VictoryCard) extends GameOption with ToggleOption with ImportantOption {
    val group = "Victory conditions".txt
    def valueOn = (card.mapControl.?("Map Control: ").|("Wealth: ").spn(xstyles.smaller85) ~ card.name.txt)
    override def decorate(e : Elem) = e ~ VictoryExpansion.needsCreatures(card).?(" (Creatures module)".spn(xstyles.smaller85)).|(Empty)
    override val explain = $(card.name.hl ~ ": " ~ card.info.text, "Needs " ~ "Alternative victory, chosen cards".hl ~ ".")
    override def required(all : $[BaseOption]) = $($[BaseOption](AltVictoryChosen) ++ VictoryExpansion.needsCreatures(card).$(ModuleOption(Creatures)))
}

object VictoryCardOption {
    val all : $[VictoryCardOption] = VictoryCard.all./(VictoryCardOption(_))
}

// The Warchiefs box's 7 extra clan upgrade cards, which the rulebook allows without the Warchiefs module
case object WarchiefCards extends GameOption with ToggleOption {
    val group = "Warchief upgrade cards".txt
    def valueOn = "Warchief upgrade cards without the module".txt
    override val explain = $(
        "The Warchiefs box adds a third clan upgrade card for each clan (Egil's Fury, Brand's Bravery, Borgild's Shield, Signy's Celerity, Liv's Cunning, Halvard's Craft, Svarn's Menders).",
        "They are always in the game with the " ~ "Warchiefs".hl ~ " module. With this option they are also used without it; Borgild's Shield then ignores its Kaija and Borgild part.",
    )
}

// Leaves the Development cards whose effect is to draw cards out of the decks (Game: ShuffledEarlyAction, ShuffledAdvancedAction).
// Cards that only draw 1 besides another effect (Negociation, Spy, Ancestral Curse, Veiled Threats) stay
case object NoDrawDevelopments extends GameOption with ToggleOption {
    val group = "Development cards".txt
    def valueOn = "Ban card draw developments".txt
    override val explain = $(
        "Removes the " ~ NoDrawDevelopments.cards.num.hl ~ " Development cards that draw cards: " ~ NoDrawDevelopments.cards./(_.info.name).mkString(", ").hl ~ ".",
        "Cards that draw 1 besides another effect (Negociation, Spy, Ancestral Curse, Veiled Threats) stay in. With six players in a long game fewer Development cards may be revealed.",
    )

    lazy val cards : $[Card] = (Cards.early ++ Cards.advanced).%((_, info) => info.effect.is[DrawEffect]).lefts./(Development(_))
}

// Uncharted Horizons' 5 Early and 8 Advanced Development cards and 2 Achievement cards, shuffled into their decks
// (Game: ShuffledEarlyAction, ShuffledAdvancedAction, ShuffledAchievementsAction; effects in horizon-devs.scala)
case object HorizonsDevelopments extends GameOption with ToggleOption {
    val group = "Development cards".txt
    def valueOn = "Uncharted Horizons Development cards".txt
    override val explain = $(
        "Shuffles the " ~ 13.hl ~ " Development cards of Uncharted Horizons into the decks: Early " ~ Cards.horizonsEarly.rights./(_.name).mkString(", ").hl ~ "; Advanced " ~ Cards.horizonsAdvanced.rights./(_.name).mkString(", ").hl ~ ".",
        "The Achievement cards " ~ "Mountaineer".hl ~ " (2 fame per territory with a Rough border) and " ~ "Sailor".hl ~ " (2 fame per open territory) join the Achievements.",
        "As many Development cards are revealed as usual; there are just more to draw from.",
    )
}

case object FirstSeatStarts extends GameOption with ToggleOption {
    val group = "First player".txt
    def valueOn = "First seat goes first".txt
    override val explain = $(
        "By the rules the first player is chosen at random (most axes on the dice).",
        "With this option the player in the first seat goes first.",
    )
}


// Modules and expansions. Each one is a game option; until a module is implemented its option
// is shown but can't be turned on. To implement one: set `ready`, give it an `expansion`
// (tried before the core ones in Game.expansions, so it can take over any action), and add its
// assets to Meta.assets with `Meta.has(options, module)` as the condition.
abstract class Module(val label : String, val box : String) extends NamedToString with Record {
    def ready : Boolean = false
    def about : $[Elem]
    def expansion : |[Expansion] = None
    // Lower goes first among the module expansions
    def priority : Int = 0
}

case object Creatures extends Module("Creatures", "core box") {
    override def ready = true
    override def expansion = |(CreaturesExpansion)
    def about = $("Creature cards (Wolves, Brown Bears, Draugr, Fallen Valkyries) and the creature lairs printed on the map tiles.")
}

case object Warchiefs extends Module("Warchiefs", "Warchiefs expansion") {
    override def ready = true
    override def expansion = |(WarchiefsExpansion)
    def about = $(
        "Each clan gets its warchief: a unit worth 2 combat points with a power of its own (see the Warchief button when picking clans).",
        "It can be placed at setup instead of one unit, or recruited instead of a unit, and goes back to the reserve when it dies. Card effects on enemy units can't target it.",
    )
}

case object Wilderness extends Module("Wilderness", "expansion") {
    override def ready = true
    override def expansion = |(WildernessExpansion)
    // Before the Creatures module, so its Environment tiles can act first in the Creature phase
    override def priority = -1
    def about = $(
        "Environment tiles: the Great Lake, two Geysers, two Ruins, the Swamp, the Poisonous Swamp and two High Peaks are shuffled into the map tiles after setup. Impassable borders (orange lines) can't be crossed.",
        "With the " ~ "Creatures".hl ~ " module: the Draugr Jötnar, Eldthursar and Hvedrung join the creature deck, and the Ancestral Graveyard (Spectral Warriors) and the Wyvern's Den (the Wyvern) are added to the map tiles.",
    )
}

case object Wastelands extends Module("Wastelands", "expansion") {
    override def ready = true
    override def expansion = |(WastelandsExpansion)
    // Before Events, New Blood and Wilderness, so its Start of Year steps and setup come first
    override def priority = -4
    def about = $(
        "Seven Environment tiles (the Kobold Camp, the Jötnar Camp with Jötunn Blainn, Naströnd, Landvidi, Thor's Wrath, Urdarbrunn and Vedrfolnir) are shuffled into the map tiles after setup; with " ~ "Wilderness".hl ~ " twelve Environment tiles of both are drawn.",
        "A Central tile can replace the starting tile (" ~ "Central tile".hl ~ " below, also offered without this module).",
        "With the " ~ "Creatures".hl ~ " module: Rock Golems, Myrkalfar, Giant Boars, Kobolds and Valdemar join the creature deck.",
    )
}

// Seven more clans; on whenever one of them plays (Game.modules), so its option isn't listed
case object NewBlood extends Module("New Blood", "expansion") {
    override def ready = true
    override def expansion = |(NewBloodExpansion)
    // Before Wilderness, Creatures and Warchiefs, so the clans' powers can act first
    override def priority = -2
    def about = $("Seven more clans: Dragon, Horse, Kraken, Lynx, Ox, Rat and Squirrel.")

    val clans : $[Faction] = $(Dragon, Horse, Kraken, Lynx, Ox, Rat, Squirrel)
}

case object UnchartedHorizons extends Module("Uncharted Horizons", "expansion") {
    def about = $("The rest of the expansion: the drafting setup. Its Development cards and map tiles are options of their own (Development cards, Map tiles), and its Training Fields duel is Training Grounds on the main menu.")
}

// Uncharted Horizons' Training Fields module (training.scala): a two-player duel of its own, played instead of the usual game.
// On only with TrainingFieldsOption, which the main menu's Training Grounds turns on (Meta.modes), so it has no module option
case object TrainingFields extends Module("Training Fields", "Uncharted Horizons") {
    override def ready = true
    override def expansion = |(TrainingExpansion)
    // Before everything: it takes over the start of the game
    override def priority = -10
    def about = $("A duel for two players on twelve tiles of the base game. Players take turns playing one of their seven Action cards; the first to " ~ 5.hl ~ " victory points wins.")
}

case object TrainingFieldsOption extends GameOption with ToggleOption {
    val group = "Training Fields".txt
    def valueOn = "Training Fields duel".txt
}

// Uncharted Horizons' Solo module: on whenever the Automa plays (Game.modules), so its option isn't listed
case object Solo extends Module("Solo", "Uncharted Horizons") {
    override def ready = true
    override def expansion = |(AutomaExpansion)
    override def priority = -5
    def about = $("Play alone against the Automa: pick it and one clan.")
}

// Uncharted Horizons' Events module (horizons.scala)
case object EventsModule extends Module("Events", "Uncharted Horizons") {
    override def ready = true
    override def expansion = |(EventsExpansion)
    override def priority = -3
    def about = $(
        "A face-up deck of Event cards, one fewer than the years played. From the second year on, one Event applies each year: at the start of the year, during the actions and combats, at the Harvest or in Winter.",
        "The next Event is always visible, so players can prepare for it.",
    )
}

// Uncharted Horizons' Sea module (sea.scala)
case object Sea extends Module("Sea", "Uncharted Horizons") {
    override def ready = true
    override def expansion = |(SeaExpansion)
    // First: the Raid phase comes before the Creature phase, and the Beach tiles before other setup steps
    override def priority = -6
    def about = $(
        "Each player places a Beach tile after their first map tile, leaving an empty space between them. Its territory has a Port: units can move from one Port to another with a Move of 2 or more, and its defender gets +1 in combat.",
        "At the end of the Actions phase, the controller of a Port may draw 2 Raid cards, keep one and send 1 or 2 units from the Port on the Raid. A year later they come back with resources or the card's action, or 2 units stay away a second year for a bigger reward.",
    )
}

// Uncharted Horizons' Alternative victory conditions module (horizons.scala); turned on by the Victory conditions
// (AltVictoryRandom, AltVictoryChosen), so its option isn't listed (games made before keep ModuleOption(VictoryModule))
case object VictoryModule extends Module("Alternative victory", "Uncharted Horizons") {
    override def ready = true
    override def expansion = |(VictoryExpansion)
    override def priority = -3
    def about = $(
        "One Map Control card and two Wealth cards (three with teams) are drawn at setup. At the end of each year, a player who fulfils the chosen mode's conditions wins at once: " ~ "Thane".hl ~ " needs the Map Control card and one Wealth card, " ~ "Jarl".hl ~ " all three.",
        "The three closed territories with large buildings no longer win. If nobody fulfils the conditions by the end of the last year, the most fame wins.",
    )
}

// Team play (Game.teams): seats alternate between the two teams, so teammates sit opposite each other.
// Teammates add their scores together, may move through each other's territories but not stop there,
// never fight or target each other, and trade resources with each other 1:1 at harvest
case object TeamsVariant extends Module("2v2 Teams", "core box variant") {
    override def ready = true
    def about = $(
        "Four players in two teams; teammates sit opposite each other (seats 1 and 3 against seats 2 and 4) and add their scores together.",
        "Units may move through a teammate's territory but not stop there. During the harvest teammates may trade resources with each other 1:1.",
    )
}

// The same rules with six players
case object Teams3v3 extends Module("3v3 Teams", "variant, 2v2 rules") {
    override def ready = true
    def about = $(
        "Six players in two teams of three, with the 2v2 rules; seats alternate between the teams (seats 1, 3 and 5 against seats 2, 4 and 6).",
        "Teammates add their scores together, may move through each other's territories but not stop there, and may trade resources with each other 1:1 during the harvest.",
        "Not in the rulebook.".styled(xstyles.warning),
    )
}

// Three teams of two with six players; teammates sit opposite each other
case object Teams2v2v2 extends Module("2v2v2 Teams", "variant, 2v2 rules") {
    override def ready = true
    def about = $(
        "Six players in three teams of two, with the 2v2 rules; teammates sit opposite each other (seats 1 and 4, 2 and 5, 3 and 6).",
        "Teammates add their scores together, may move through each other's territories but not stop there, and may trade resources with each other 1:1 during the harvest.",
        "Not in the rulebook.".styled(xstyles.warning),
    )
}

object Module {
    val all : $[Module] = $(Creatures, Warchiefs, Wilderness, Wastelands, NewBlood, EventsModule, VictoryModule, Sea, Solo, UnchartedHorizons, TeamsVariant, Teams3v3, Teams2v2v2, TrainingFields)

    // The team variants and the number of players each one needs
    val teams : Map[Module, Int] = Map(TeamsVariant -> 4, Teams3v3 -> 6, Teams2v2v2 -> 6)

    // How many teams each variant has
    val sides : Map[Module, Int] = Map(TeamsVariant -> 2, Teams3v3 -> 2, Teams2v2v2 -> 3)
}

case class ModuleOption(module : Module) extends GameOption with ToggleOption with ImportantOption {
    val group = "Modules and expansions".txt
    def valueOn = module.label.txt
    override val explain = module.about ++ module.ready.not.$("Not implemented yet.".styled(xstyles.warning))
    // A module that isn't ready requires itself, so it can never be turned on
    override def required(all : $[BaseOption]) = module.ready.?($($[BaseOption]())).|($($[BaseOption](this)))
}
