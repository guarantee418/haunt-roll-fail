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

case object FameOnly extends GameOption with ToggleOption with ImportantOption {
    val group = "Victory".txt
    def valueOn = "Fame victory only".txt
    override val explain = $(
        "Normally a player who controls three closed territories, each with a large building, wins at the end of a year.",
        "With this option only fame counts, at the end of the last year. The rulebook suggests it for the " ~ 10.hl ~ "-year game.",
    )
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
    def about = $("Creatures, Environment tiles and Central tiles.")
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
    def about = $("The rest of the expansion: Development and Raid cards, Training Fields, solo play, drafting setup and more map tiles.")
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

// Uncharted Horizons' Alternative victory conditions module (horizons.scala)
case object VictoryModule extends Module("Alternative victory", "Uncharted Horizons") {
    override def ready = true
    override def expansion = |(VictoryExpansion)
    override def priority = -3
    def about = $(
        "One Map Control card and two Wealth cards (three with teams) are drawn at setup. At the end of each year, a player who fulfils the chosen mode's conditions wins at once: " ~ "Thane".hl ~ " needs the Map Control card and one Wealth card, " ~ "Jarl".hl ~ " all three.",
        "The three closed territories with large buildings no longer win. If nobody fulfils the conditions by the end of the last year, the most fame wins.",
    )
}

// The Alternative victory module's mode
case class VictoryModeOption(jarl : Boolean) extends GameOption with OneOfGroup {
    val group = "Alternative victory mode".txt
    def valueOn = jarl.?("Jarl: all three cards").|("Thane: Map Control and one Wealth card").txt
    override val explain = $("Needs the " ~ "Alternative victory".hl ~ " module. Thane is the default.")
    override def required(all : $[BaseOption]) = $($(ModuleOption(VictoryModule)))
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

object Module {
    val all : $[Module] = $(Creatures, Warchiefs, Wilderness, Wastelands, NewBlood, EventsModule, VictoryModule, UnchartedHorizons, TeamsVariant, Teams3v3)

    // The team variants and the number of players each one needs
    val teams : Map[Module, Int] = Map(TeamsVariant -> 4, Teams3v3 -> 6)
}

case class ModuleOption(module : Module) extends GameOption with ToggleOption with ImportantOption {
    val group = "Modules and expansions".txt
    def valueOn = module.label.txt
    override def decorate(e : Elem) = e ~ " " ~ ("(" + module.box + module.ready.not.??(", coming later") + ")").spn(xstyles.smaller85)
    override val explain = module.about ++ module.ready.not.$("Not implemented yet.".styled(xstyles.warning))
    // A module that isn't ready requires itself, so it can never be turned on
    override def required(all : $[BaseOption]) = module.ready.?($($[BaseOption]())).|($($[BaseOption](this)))
}
