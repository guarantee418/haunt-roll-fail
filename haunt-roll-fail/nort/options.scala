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
    override def buttonStyles = $(styles.colorButton)
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
}

case object Creatures extends Module("Creatures", "core box") {
    def about = $("Creature cards (Wolves, Brown Bears, Draugr, Fallen Valkyries) and the creature lairs printed on the map tiles.")
}

case object Warchiefs extends Module("Warchiefs", "New Blood") {
    def about = $("A Warchief figure for each player, in their color.")
}

case object Wilderness extends Module("Wilderness", "expansion") {
    def about = $("More creatures and Environment tiles.")
}

case object Wastelands extends Module("Wastelands", "expansion") {
    def about = $("Creatures, Environment tiles and Central tiles.")
}

case object NewBlood extends Module("New Blood", "expansion") {
    def about = $("Seven more clans: Dragon, Horse, Kraken, Lynx, Ox, Rat and Squirrel.")
}

case object UnchartedHorizons extends Module("Uncharted Horizons", "expansion") {
    def about = $("Development, Event and Raid cards, other victory conditions, Training Fields, solo play, drafting setup and more map tiles.")
}

case object TeamsVariant extends Module("2v2 Teams", "core box variant") {
    def about = $("Four players in two teams.")
}

object Module {
    val all : $[Module] = $(Creatures, Warchiefs, Wilderness, Wastelands, NewBlood, UnchartedHorizons, TeamsVariant)
}

case class ModuleOption(module : Module) extends GameOption with ToggleOption with ImportantOption {
    val group = "Modules and expansions".txt
    def valueOn = module.label.txt
    override def decorate(e : Elem) = e ~ " " ~ ("(" + module.box + module.ready.not.??(", coming later") + ")").spn(xstyles.smaller85)
    override val explain = module.about ++ module.ready.not.$("Not implemented yet.".styled(xstyles.warning))
    // A module that isn't ready requires itself, so it can never be turned on
    override def required(all : $[BaseOption]) = module.ready.?($($[BaseOption]())).|($($[BaseOption](this)))
}
