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

// Team play, picked above the clan picker (Meta.pickerModes): each player is on the team of its TeamOption, chosen on its row of
// the setup screen; any split of the players into two or more teams. The team modules below are what games made before used
case object TeamPlay extends GameOption {
    val group = "Game type".txt
    def valueOn = "Teams".hlb
}

// A clan's team in team play: 0 is Team A, 1 Team B, and so on; picking a team takes the clan off its old one
case class TeamOption(clan : Faction, team : Int) extends GameOption {
    val group = (clan.name + " Clan team").txt
    def valueOn = ("Team " + TeamOption.letter(team)).hl
    override def forcedOff(all : $[BaseOption]) = all.of[TeamOption].%(o => o != this && o.clan == clan)
}

object TeamOption {
    def letter(team : Int) = "ABCDEF".charAt(team).toString
    val all : $[TeamOption] = Meta.factions./~(f => $(0, 1, 2, 3, 4, 5)./(TeamOption(f, _)))
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

// The same for a seat of an Adset game (adset.scala), whose clan is known only once it is drafted:
// the clan the seat picks plays by the Robotos rules (game.cheaters)
case class RobotosSeatOption(seat : Seat) extends GameOption {
    val group = "Robotos".txt
    def valueOn = seat.name.txt
}

object RobotosSeatOption {
    val all : $[RobotosSeatOption] = MetaAdset.factions./(RobotosSeatOption(_))
}

// A clan (or Adset seat) played by any bot, Easy, Hard or Robotos. Hidden, added by Meta.botOptions like Robotos, so the
// Hard bots can tell the human players apart (game.human): they break creature movement ties towards them
case class BotClanOption(clan : Faction) extends GameOption {
    val group = "Bots".txt
    def valueOn = (clan.name + " Clan").txt
}

object BotClanOption {
    val all : $[BotClanOption] = Meta.factions./(BotClanOption(_))
}

case class BotSeatOption(seat : Seat) extends GameOption {
    val group = "Bots".txt
    def valueOn = seat.name.txt
}

object BotSeatOption {
    val all : $[BotSeatOption] = MetaAdset.factions./(BotSeatOption(_))
}

// The Automa's difficulty (Solo module): 1, the player also wins with four closed territories with large buildings;
// 3 and up need the Creatures module; from 4, the Automa draws one more card each year per level above 3
case class AutomaLevelOption(level : Int) extends GameOption with OneOfGroup with ImportantOption {
    val group = "Automa difficulty".txt
    override def fold = |("Automa difficulty")
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
    override def fold = |("Game length")
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
    override def fold = |("Victory conditions")
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
    override def fold = |("Victory conditions")
    def valueOn = jarl.?("Jarl: all three cards").|("Thane: Map Control and one Wealth card").txt
    override val explain = $("Needs " ~ "Alternative victory".hl ~ ". Thane is the default.")
    override def required(all : $[BaseOption]) = VictoryChoice.alternative./(o => $[BaseOption](o))
    override def forcedOff(all : $[BaseOption]) = $(VictoryModeOption(jarl.not))
}

// An Alternative victory card chosen by hand
case class VictoryCardOption(card : VictoryCard) extends GameOption with ToggleOption with ImportantOption {
    val group = "Victory conditions".txt
    override def fold = |("Victory conditions")
    def valueOn = (card.mapControl.?("Map Control: ").|("Wealth: ").spn(xstyles.smaller85) ~ card.name.txt)
    override def decorate(e : Elem) = e ~ VictoryExpansion.needsCreatures(card).?(" (Creatures module)".spn(xstyles.smaller85)).|(Empty)
    override val explain = $(card.name.hl ~ ": " ~ card.info.textElem, "Needs " ~ "Alternative victory, chosen cards".hl ~ ".")
    override def required(all : $[BaseOption]) = $($[BaseOption](AltVictoryChosen) ++ VictoryExpansion.needsCreatures(card).$(ModuleOption(Creatures)))
}

object VictoryCardOption {
    val all : $[VictoryCardOption] = VictoryCard.all./(VictoryCardOption(_))
}

// WARCHIEF EXPANSION: the whole expansion (ModuleOption(Warchiefs)), only its third clan upgrade cards (WarchiefCards),
// or only its warchiefs (WarchiefPawns); at most one of the three. "Warchiefs module" until Robotos asked for its name (2026-10-26)
object WarchiefsChoice {
    val group = "Warchief expansion".txt
    def all : $[GameOption] = $(ModuleOption(Warchiefs), WarchiefCards, WarchiefPawns)
    def others(o : BaseOption) : $[BaseOption] = all.%(_ != o)
}

// Each clan's warchief upgrade card (its third clan upgrade card, all 14 clans) without the warchiefs, which the rulebook allows
case object WarchiefCards extends GameOption with ToggleOption {
    val group = WarchiefsChoice.group
    def valueOn = "Warchief upgrade cards only".txt
    override val explain = $(
        "Each clan gets its warchief upgrade card, a third clan upgrade card (Egil's Fury, Brand's Bravery, Borgild's Shield, ...), but no warchief.",
        "Borgild's Shield then ignores its Kaija and Borgild part.",
    )
    override def forcedOff(all : $[BaseOption]) = WarchiefsChoice.others(this)
}

// The warchiefs (pawns and powers) without the warchief upgrade cards; Meta.has counts it as the Warchiefs module
case object WarchiefPawns extends GameOption with ToggleOption {
    val group = WarchiefsChoice.group
    def valueOn = "Warchief pawns only".txt
    override val explain = $(
        CombatText("Each clan gets its warchief: a unit worth 2 combat points with a power of its own (see the Warchief button when picking clans)."),
        "No warchief upgrade cards: every clan has its usual two clan upgrades.",
    )
    override def forcedOff(all : $[BaseOption]) = WarchiefsChoice.others(this)
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

// Leaves Defensive Strategy (an Advanced Development card that cancels another player's card) out of the decks
// (Game: ShuffledEarlyAction, ShuffledAdvancedAction)
case object NoDefensiveStrategy extends GameOption with ToggleOption {
    val group = "Development cards".txt
    def valueOn = "Ban Defensive Strategy".txt
    override val explain = $(
        "Removes " ~ "Defensive Strategy".hl ~ " from the Development cards, so no card played can be cancelled.",
    )

    val cards : $[Card] = $(Development("defensive-strategy"))
}

// Development cards left out of the decks one by one (asked for by Marzaus and Robotos on 2026-10-21: Future Sight, Market Place,
// "any cards you want"); Game: ShuffledEarlyAction, ShuffledAdvancedAction
case class BanDevelopment(id : String) extends GameOption with ToggleOption {
    val group = "Ban Development cards".txt
    override def fold = |("Ban Development cards")
    def info = Cards.developments(id)
    def valueOn = info.name.txt
    override def decorate(e : Elem) = e ~ " " ~ BanDevelopment.deck(id).spn(xstyles.smaller85)
    override val explain = $(info.name.hl ~ ": " ~ info.textElem)
}

object BanDevelopment {
    val all : $[BanDevelopment] = (Cards.early ++ Cards.advanced ++ Cards.horizonsEarly ++ Cards.horizonsAdvanced).lefts./(BanDevelopment(_))

    def horizons(id : String) = (Cards.horizonsEarly ++ Cards.horizonsAdvanced).lefts.has(id)

    def deck(id : String) = (Cards.early ++ Cards.horizonsEarly).lefts.has(id).?("(Early").|("(Advanced") + horizons(id).??(", Uncharted Horizons") + ")"

    // Every card the setup leaves out: the two ban options and the cards banned one by one
    def cards(options : $[BaseOption]) : $[Card] =
        options.has(NoDrawDevelopments).??(NoDrawDevelopments.cards) ++ options.has(NoDefensiveStrategy).??(NoDefensiveStrategy.cards) ++ options.of[BanDevelopment]./(o => Development(o.id))
}

// DEVELOPMENT CARDS EACH YEAR: one per player (the rules), one more (suggested by Marzaus for two players on 2026-10-19, for every
// player count by Gman on 2026-10-20), or one fewer from year 2 on, "hard mode" (Marzaus, 2026-10-22). Game.devsIn; not offered
// against the Automa (optionsFor in meta.scala). Games made before the choice have none of these, or ExtraDevelopment.
// Marzaus's two variants are listed under Fan content as toggles, each turning the other off (Robotos, 2026-10-26); the rules'
// NormalDevelopments, once the third choice of a "Development cards each year" section, is hidden and kept for the games that have it
trait DevelopmentsChoice extends GameOption with ToggleOption {
    val group = "Fan content".txt
    override def fold = |("Fan content")
    override def forcedOff(all : $[BaseOption]) = all.of[DevelopmentsChoice].but(this)
}

case object NormalDevelopments extends DevelopmentsChoice {
    def valueOn = "Development cards: one per player".txt
    override val explain = $(
        "The rules: each year as many Development cards are revealed as there are players.",
    )
}

// One more Development card revealed each year than there are players; the one left after everyone passes is discarded
case object ExtraDevelopment extends DevelopmentsChoice {
    def valueOn = "Development cards: one extra each year".txt
    override val explain = $(
        "Fan-made variant by Marzaus. Each year one more Development card is revealed than there are players (" ~ 3.hl ~ " in a two-player game), so the last player to pass still has a choice. The card left over is discarded.",
        "When the decks can't last every year (six players in a long game), the " ~ "Uncharted Horizons Development cards".hl ~ " are added.",
    )

    // The core Development cards left after the bans can't give one more card than the players every year but the last:
    // the Uncharted Horizons ones join in (Game.horizonsDevelopments; told on the setup screen, validateFactionSeatingOptions)
    def needsHorizons(options : $[BaseOption], players : Int, years : Int) : Boolean =
        options.has(ExtraDevelopment) && options.has(HorizonsDevelopments).not &&
            (players + 1) * (years - 1) > (Cards.earlyCards ++ Cards.advancedCards).diff(BanDevelopment.cards(options)).num
}

// One fewer Development card than there are players from the second year on: the last player to pass gets none
case object FewerDevelopments extends DevelopmentsChoice {
    def valueOn = "Development cards: one fewer after year 1 (hard mode)".txt
    override val explain = $(
        "Fan-made variant by Marzaus. Year " ~ 1.hl ~ " reveals one Development card per player. From year " ~ 2.hl ~ " on, one fewer is revealed than there are players, so the last player to pass gets none.",
    )
}

object DevelopmentsChoice {
    val all : $[DevelopmentsChoice] = $(NormalDevelopments, ExtraDevelopment, FewerDevelopments)

    // The two offered under Fan content
    val fan : $[DevelopmentsChoice] = $(ExtraDevelopment, FewerDevelopments)

    // Development cards revealed in a year (not the last, which reveals the Achievements)
    def in(options : $[BaseOption], players : Int, automa : Boolean, year : Int) : Int =
        if (automa) players
        else if (options.has(ExtraDevelopment)) players + 1
        else if (options.has(FewerDevelopments) && year > 1) players - 1
        else players

    // Development cards revealed in the years from..until (until excluded)
    def total(options : $[BaseOption], players : Int, automa : Boolean, from : Int, until : Int) : Int =
        (from until until)./(in(options, players, automa, _)).sum
}

// One more Achievement card than there are players is revealed in the last year (Game: ShuffledAchievementsAction);
// the one left over after everyone has passed is set aside
case object ExtraAchievement extends GameOption with ToggleOption {
    val group = "Development cards".txt
    def valueOn = "One more Achievement card".txt
    override val explain = $(
        "Reveals one more " ~ "Achievement".hl ~ " card than there are players in the last year, so the last player to pass still has a choice. The card nobody takes is set aside.",
    )
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

// Each player's three starting tiles are one from a food, a wood and a lore pile (Marzaus's group's house rule, asked for
// by Gman on 2026-10-19, who picked three piles over redrawing; MapExpansion.dealStart)
case object BalancedStartTiles extends GameOption with ToggleOption {
    val group = "Map tiles".txt
    def valueOn = "Balanced starting tiles".txt
    override val explain = $(
        "Each player's " ~ 3.hl ~ " starting tiles show at least one " ~ Food.elem ~ ", one " ~ Wood.elem ~ " and one " ~ Lore.elem ~ ".",
        "The map tiles are sorted into a food, a wood and a lore pile, and each player gets one tile from each. A tile with two resources is in the pile of the rarer one; tiles with none aren't dealt.",
    )
}

case object FirstSeatStarts extends GameOption with ToggleOption {
    val group = "First player".txt
    def valueOn = "First seat goes first".txt
    override val explain = $(
        CombatText("By the rules the first player is chosen at random (most axes on the dice)."),
        "With this option the player in the first seat goes first.",
    )
}

// FAME: whether the player panels show every clan's fame (nort/ui.scala, fameHidden). Private is the rule (the fame tokens are
// kept face down) and the default; games made before the choice have neither option and stay private
trait FameChoice extends GameOption with OneOfGroup {
    val group = "Fame".txt
    override def fold = |("Fame")
}

case object PrivateFame extends FameChoice {
    def valueOn = "Private fame".txt
    override val explain = $(
        "The rules: fame tokens are kept face down. Each player sees only their own fame (and their teammates'), everyone's once the game is over.",
    )
}

case object PublicFame extends FameChoice {
    def valueOn = "Public fame".txt
    override val explain = $(
        "Every player's fame is shown in the player panels all game.",
    )
}

object FameChoice {
    val all : $[FameChoice] = $(PrivateFame, PublicFame)
}

// UNDO: how far "Undo to here" in the log may go back (undoBlocked in nort/ui.scala; asked for by Robotos on 2026-10-20).
// Display only, so recorded games replay the same; games made before the choice have none of these and keep unlimited undo
trait UndoChoice extends GameOption with OneOfGroup {
    val group = "Undo".txt
    override def fold = |("Undo")
}

case object NoUndo extends UndoChoice {
    def valueOn = "No undo".txt
    override val explain = $(
        "Nothing can be undone. The log still replays the game.",
    )
}

case object LimitedUndo extends UndoChoice {
    def valueOn = "Limited undo".txt
    override val explain = $(
        "Undo can't go back past a combat die roll, a shuffle, a card or tile drawn, or Developments, creatures or events revealed.",
        "After an Explore, the tile can be placed again, but not drawn again.",
    )
}

case object UnlimitedUndo extends UndoChoice {
    def valueOn = "Unlimited undo".txt
    override val explain = $(
        "Anything can be undone, back to any line of the log.",
    )
}

object UndoChoice {
    val all : $[UndoChoice] = $(UnlimitedUndo, LimitedUndo, NoUndo)
}


// Casualties are chosen by their player: units, the warchief, Kaija (or Brundr and Kaelinn, or Brok) and Jötunn Blainn,
// in any mix (CasualtyPickAction in map.scala; asked for by Robotos on 2026-10-11). Hidden, and given to every new game
// (Meta.mandatoryFor): games started before it took units first, then the warchief, Blainn and the companion, and their
// replays must not stop on a choice they never recorded
case object ChooseCasualties extends GameOption with ToggleOption {
    val group = "Casualties".txt
    def valueOn = "Chosen by their player".txt
}

// Ox Clan's Ancestral Equipment token after an Explore goes on a free small building space or Carved Stone space of the tile
// that Ox picks (GearPlaceAction in newblood.scala; asked for by Robotos on 2026-10-12). Hidden, and given to every new game
// (Meta.mandatoryFor): games started before it put the token on the tile's first free small building space, never on a
// Carved Stone space, and their replays must not stop on a choice they never recorded
case object OxPicksSpace extends GameOption with ToggleOption {
    val group = "Ancestral Equipment".txt
    def valueOn = "Ox picks the space".txt
}

// A tile placement is refused only when it joins two players' units (or a player's units and a Fallen Valkyrie) into one
// territory: the territories it is part of are checked, not every territory on the map (MapExpansion.placements). Without it,
// teammates sharing a territory, or the Robotos bot's units beside a Fallen Valkyrie, anywhere, stopped every Explore.
// Hidden, and given to every new game (Meta.mandatoryFor): older games drew and set aside other tiles, and their replays must not change
case object JoinedTilesOnly extends GameOption with ToggleOption {
    val group = "Tile placement".txt
    def valueOn = "Checks only the territories joined".txt
}

// Svarn's Menders brings back the warchief, the companion (Kaija, Brundr and Kaelinn, Brok) and Jötunn Blainn among the
// casualties too, not only units (the owner's ruling of 2026-10-12; figures lost for having nowhere to retreat still stay lost).
// Hidden, and given to every new game (Meta.mandatoryFor): older games brought back units only, and their replays must not stop
// on a choice they never recorded
case object MendFigures extends GameOption with ToggleOption {
    val group = "Svarn's Menders".txt
    def valueOn = "Brings back every figure".txt
}

// Ancestral Curse's opponents pick the card they discard all at once, each on their own screen (CurseAllAction in effects.scala;
// asked for by the owner on 2026-10-16), instead of one after another in turn order. Hidden, and given to every new game
// (Meta.mandatoryFor): older games recorded the discards one by one, and their replays must not change
case object SimultaneousCurse extends GameOption with ToggleOption {
    val group = "Ancestral Curse".txt
    def valueOn = "Opponents discard at once".txt
}

// Dragon Clan's extra food or wood after harvesting comes "from one of your territories": only a resource one of them gave at
// this harvest, less what Snake's Scorched Earth took from it (NewBloodExpansion.dragonExtra; reported on Discord on 2026-10-19,
// food was collected from a food territory whose food Snake had taken). Hidden, and given to every new game (Meta.mandatoryFor):
// older games could take either, and their replays must not lose a recorded choice
case object DragonExtraFromTerritory extends GameOption with ToggleOption {
    val group = "Dragon Clan".txt
    def valueOn = "Extra resource from a territory".txt
}

// The Kraken Clan card collects 1 food or 1 wood only if a territory Kraken controls with a High Tide token gives it (reported
// by Marzaus on 2026-10-19: wood was offered from a tile without any). Hidden, and given to every new game (Meta.mandatoryFor):
// older games may have recorded a resource no such territory gives, and their replays must not change
case object KrakenFromTerritory extends GameOption with ToggleOption {
    val group = "Kraken Clan".txt
    def valueOn = "Collects from the territory".txt
}

// Knowledge from Beyond as on the print card Marzaus posted on 2026-10-20: for each High Tide territory, any of Recruit 1
// (with its Training Camps), Collect 1 resource that territory gives, or Draw 1, the same one again if wanted. Hidden, and
// given to every new game (Meta.mandatoryFor): older games offered each option once and any resource, and replay that way
case object KnowledgeEachTerritory extends GameOption with ToggleOption {
    val group = "Kraken Clan".txt
    def valueOn = "Knowledge from Beyond for each territory".txt
}

// The Kraken clan cards, with both High Tide tokens out, may move one to another territory Kraken controls (asked for by the
// owner on 2026-10-21; the clan board only says "place"). Hidden, and given to every new game (Meta.mandatoryFor): older
// games' replays must not stop on a step they never recorded
case object KrakenMovesTide extends GameOption with ToggleOption {
    val group = "Kraken Clan".txt
    def valueOn = "Clan cards move a High Tide token".txt
}

// The combat report and the clan power report are notifications that don't stop the game (Robotos, 2026-10-21: nobody
// should wait for another player to tap OK on a box only they read): each player sees them over their screen the next time
// it updates (ReportNote in map.scala). Hidden, and given to every new game (Meta.mandatoryFor): older games recorded the OKs
case object ReportsDontWait extends GameOption with ToggleOption {
    val group = "Clan powers".txt
    def valueOn = "Reports don't wait".txt
}

// Future Sight's "or Achievement" only in the last year (Gman, 2026-10-21): the Achievements are the cards on offer then, so
// before it Future Sight takes only a revealed Development card. Hidden, and given to every new game (Meta.mandatoryFor):
// older games could take an Achievement early, and replay that way
case object FutureSightLastYear extends GameOption with ToggleOption {
    val group = "Development cards".txt
    def valueOn = "Future Sight takes Achievements only in the last year".txt
}

// An Achievement taken by Future Sight or at Mimirsbrunn (the last year) is kept aside, face down, to score at the end, not put on
// top of the draw pile, where a Replace, Remove or draw effect brought it into the hand in place of a card (Marzaus and Robotos,
// relayed by Gman on 2026-10-28). Hidden, and given to every new game (Meta.mandatoryFor): older games replay the old way, and a
// running one can be switched over with AchievementsAsideAction(X)
case object AchievementsAside extends GameOption with ToggleOption {
    val group = "Development cards".txt
    def valueOn = "Achievements taken early are kept aside".txt
}

// Printed Lynx Clan (PrintedNewBlood): each starting placement may take a lynx, so Lynx can start with both Brundr and Kaelinn
// (Gman, 2026-10-31: "Lynx is supposed to have 2 cat tokens"). Before it, a lynx placed in the second placement moved the first
// one there and Lynx started with one. Hidden, and given to every new game (Meta.mandatoryFor); older games replay the old way
case object LynxBothAtSetup extends GameOption with ToggleOption {
    val group = "New Blood".txt
    def valueOn = "Lynx may start with both lynxes".txt
}

// New Blood as printed (Gman sent the printed cards and boards on 2026-10-23): the 19 clan cards in CardVersions (editions.scala)
// and the clan board rules (Lynx's bonus Move and Mielikki, Kraken's High Tide and Kàra, Dragon's harvest, Squirrel after Winter).
// The site had followed the TTS mod's prototype cards and boards. Hidden, and given to every new game (Meta.mandatoryFor), in either
// card edition: older games keep the old cards and rules, and replay that way
case object PrintedNewBlood extends GameOption with ToggleOption {
    val group = "New Blood".txt
    def valueOn = "Printed clan cards and boards".txt
}

// The clan power report (ClanPowers in map.scala; asked for by Robotos on 2026-10-12). Hidden, and given to every new game
// (Meta.mandatoryFor), like ChooseCasualties: older games' replays must not stop on a step they never recorded
case object ClanPowerReports extends GameOption with ToggleOption {
    val group = "Clan powers".txt
    def valueOn = "Reported".txt
}

// A player who can't pay for Winter takes the Unrest card themselves, by tapping it (TakeUnrestAction in game.scala; asked
// for by Robotos on 2026-10-12, as it was easy to miss). Hidden, and given to every new game (Meta.mandatoryFor): older games
// gave it without asking, and their replays must not stop on a choice they never recorded
case object TakeUnrestCard extends GameOption with ToggleOption {
    val group = "Unrest".txt
    def valueOn = "Taken by the player".txt
}

// The game options of 2026-10-07 for who saw the combat report. Since 2026-10-08 the report is asked of both sides and
// each player's "Combat Report" setting (ShowCombatReport, SkipCombatReport in meta.scala) shows or skips it, so these are
// hidden and do nothing; they stay so the games that have them still load
abstract class CombatReportChoice(label : String, side : String) extends GameOption with ToggleOption {
    val group = "Combat report".txt
    def valueOn = label.txt
}

case object CombatReportAttackers extends CombatReportChoice("Combat report for attackers", "attacking player")
case object CombatReportDefenders extends CombatReportChoice("Combat report for defenders", "defending player")


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
    // Fan-made, listed under "Fan content" on the setup screen
    def fan : Boolean = false
    // The options it needs, any one of the lists (BaseOption.required)
    def requires : $[$[BaseOption]] = $($())
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
        CombatText("Each clan gets its warchief: a unit worth 2 combat points with a power of its own (see the Warchief button when picking clans)."),
        "It can be placed at setup instead of one unit, or recruited instead of a unit, and goes back to the reserve when it dies. Card effects on enemy units can't target it.",
        "Each clan also gets its warchief upgrade card, a third clan upgrade card.",
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

// The fixed team variants, from before the teams were chosen per player (TeamPlay); no longer offered, kept for the games made with them.
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

// FAN CONTENT
// The Minerals module (minerals.scala), fan-made by Maciej "Niedźwiedź" Drewing (BoardGameGeek file 255809, 2023; asked for by Gman
// on 2026-10-21): Mines on territories with a creature lair dig up Stone, Iron and Gem at the Harvest
case object MineralsModule extends Module("Minerals", "fan-made module by Maciej Drewing") {
    override def ready = true
    override def fan = true
    override def expansion = |(MineralsExpansion)
    def about = $(
        "A new small building, the " ~ "Mine".hl ~ ": " ~ Amount(1.hl, Wood.elem) ~ ", only in a territory with a creature lair (" ~ 7.hl ~ " in the supply).",
        CombatText("At the Harvest each player rolls the combat die, times their Mines: 2 axes give a Stone, an axe and a skull an Iron, 2 skulls a Gem."),
        "Minerals are limited. Each is worth two resources of your choice for trading, Winter and the final fame; spent minerals leave the game.",
        "Stone".hl ~ ": during a Build, upgrade one of your buildings (" ~ 1.hl ~ " for a small one, " ~ 2.hl ~ " for a large one); it then counts as two.",
        "Iron".hl ~ ": in a fight, roll two combat dice instead of one.",
        "Gem".hl ~ ": when playing cards, gain the fame of a card in your discard pile and put it on top of your draw pile.",
        "Fan-made, not part of the published game.".styled(xstyles.warning),
    )
}

// The Tower PC version (towers.scala), fan-made by Marzaus (Discord, 2026-10-22): Defense Towers as in the video game.
// First of all the module expansions, so it steps in before the Creatures module and the fights
case object TowersModule extends Module("Tower PC version", "fan-made by Marzaus") {
    override def ready = true
    override def fan = true
    override def expansion = |(TowersExpansion)
    override def priority = -20
    def about = $(
        CombatText("Defense Towers work as in the Northgard video game:"),
        "A " ~ "Defense Tower".hl ~ " costs " ~ Amount(2.hl, Wood.elem) ~ ".",
        "It is destroyed when its territory is captured or abandoned (the clan holding it has no figure left there).",
        "It kills 1 attacking unit before the fight, not after it; with no attacker left, the defender wins without a roll.",
        CombatText("A clan fighting a creature in a territory with a Defense Tower gets 1 more combat point; when a creature appears or moves into such a territory, the clan holding it may fight it first, before the creature's ability."),
        "Fan-made, not part of the published game.".styled(xstyles.warning),
    )
}

// Warchiefs PC version (game.scala, map.scala), fan-made by Marzaus (Discord, northgard-fanmade-content; asked for by Gman on
// 2026-10-22): the warchiefs play more like the video game's. Needs the warchiefs (the Warchiefs module or its pawns)
case object WarchiefsPC extends Module("Warchiefs PC version", "fan-made variant by Marzaus") {
    override def ready = true
    override def fan = true
    override def requires = $($(ModuleOption(Warchiefs)), $(WarchiefPawns))
    def about = $(
        "The warchiefs (not Kaija, Brok, Brundr and Kaelinn) play more like the video game's:",
        "Recruiting a warchief takes " ~ 2.hl ~ " of a Recruit's units, into a territory with your " ~ TrainingCamp.elem ~ ". No warchief is placed at setup.",
        CombatText("It takes 2 casualties to kill a warchief; a single one left over does nothing to it. Effects that remove a unit outside a fight never take it."),
        "Warchiefs don't count for Winter costs.",
        "Needs the warchiefs (" ~ "Warchiefs module".hl ~ ").",
        "Fan-made, not part of the published game.".styled(xstyles.warning),
    )
}

// The Military paths module (military.scala), fan-made by Marzaus (Discord, northgard-fanmade-content; asked for by Gman on
// 2026-10-22): MXP from fights, and at 5 MXP one of three paths, each with three levels
case object MilitaryModule extends Module("Military paths", "fan-made module by Marzaus") {
    override def ready = true
    override def fan = true
    override def expansion = |(MilitaryExpansion)
    def about = $(
        "Clans gain military experience (MXP): twice the fame of each creature they defeat; for winning a fight against a clan, 1 for each enemy figure in it; in a fight against a clan, won or lost, 2 for each enemy unit and 10 for an enemy warchief killed; 3 for destroying an enemy Defense Tower.",
        "At 5 MXP (level 1) a clan picks a path for the rest of the game; levels 2 and 3 come at 50 and 100 MXP.",
        "Leadership".hl ~ " (with the Warchiefs module): a bodyguard for the warchief, rerolls, an extra move and point, captures, a kill before attacking.",
        "Guardian".hl ~ ": free Defense Towers that kill before the fight, Militia, a free Fortress each year.",
        "Legions".hl ~ ": free Training Camps that recruit more and train units, more points for big armies, rerolls.",
        "Fan-made, not part of the published game.".styled(xstyles.warning),
    )
}

// Robotos's "Lower skill floor Dragon variant" (Discord, northgard-fanmade-content, 2026-10-21; asked for by Gman on 2026-10-24):
// Dragon Clan's power and clan cards (CardVersions.dragonVariant in editions.scala, the rules in newblood.scala)
case object DragonVariant extends Module("Lower skill floor Dragon", "fan-made variant by Robotos") {
    override def ready = true
    override def fan = true
    def about = $(
        "Dragon Clan".hl ~ " plays by Robotos's variant: it never sacrifices its own units, and makes fewer resources out of thin air.",
        CombatText("The Sacrificial Pyre starts with 1 Dragon unit, and enemy casualties go on it after each combat, as before."),
        "Dragon harvests without a sacrifice and gets no extra food or wood; at the start of each Harvest it may put 1 unit from its reserve on an empty Pyre spot.",
        "Dragon Clan".hl ~ " (Flash): Recruit 1; with the Pyre full, may empty it for " ~ Amount(1.hl, Food.elem) ~ ".",
        "Reluctant Workforce".hl ~ ": draw 2; with the Pyre full, may empty it to draw a third card.",
        "Enthrall".hl ~ " (Flash): with the Pyre full, empty it for " ~ Amount(2.hl, FameIcon()) ~ "; then put an enemy unit from a territory adjacent to yours on it.",
        "Emptying the Pyre sends its units back to their owners' reserves. Surtr and his card don't change.",
        "Fan-made, not part of the published game.".styled(xstyles.warning),
    )
}

// Robotos's "Special Unit Hater's Bear, Rough Border's Edition" (Discord, northgard-fanmade-content, 2026-10-22; asked for by Gman on
// 2026-10-31): Kaija as a token on a border, picked as its own clan in the clan picker (factionVariants in meta.scala), the rules in
// bear-border.scala, the cards in CardVersions.bearBorder (editions.scala)
case object BearBorder extends Module("Rough border Bear", "fan-made variant by Robotos") {
    override def ready = true
    override def fan = true
    override def expansion = |(BearBorderExpansion)
    // With New Blood's and the Lion's, before Wilderness, Creatures and Warchiefs
    override def priority = -2
    def about = $(
        "Bear Clan".hl ~ " plays by Robotos's variant: " ~ "Kaija".hl ~ " is no unit but a token that lies on a border, never in a territory.",
        "Kaija".hl ~ " starts off the map. Before resolving one of its clan cards (the Bear Clan card or an upgrade, not Borgild's Shield), Bear may place it on a regular border between a territory it controls and an adjacent territory.",
        "Every clan, Bear too, treats the border with " ~ "Kaija".hl ~ " as a Rough border; a clan may discard any 1 resource to cross it as a regular border for the rest of a Move action.",
        "Bear Clan".hl ~ " (Flash): Move 1. After placing " ~ "Kaija".hl ~ " on a new border, you may collect 1 " ~ Food.elem ~ " or " ~ Wood.elem ~ " from the territories it connects.",
        "The Bear Awakens".hl ~ " (Flash): Recruit 2. For the rest of the year, Bear crosses the border with " ~ "Kaija".hl ~ " as a regular border.",
        "Protector of the Land".hl ~ ": draw 1 card for each of your territories with a Rough border (the border with " ~ "Kaija".hl ~ " counts too), at most 3.",
        "Borgild and Borgild's Shield don't change; the Shield's part about " ~ "Kaija".hl ~ " does nothing.",
        "Fan-made, not part of the published game.".styled(xstyles.warning),
    )
}

// Marzaus's "Fan creatures" (Discord, northgard-fanmade-content, 2026-10-23; asked for by Gman on 2026-10-24), a fun mode
// (creatures.scala, AggressiveCreaturesExpansion): no creature has its ability, and every creature is aggressive like the Fallen Valkyrie
case object AggressiveCreatures extends Module("Aggressive creatures", "fan-made variant by Marzaus") {
    override def ready = true
    override def fan = true
    override def expansion = |(AggressiveCreaturesExpansion)
    // After the Tower PC version's challenge, before the creatures' own abilities (Wilderness, Wastelands, Creatures)
    override def priority = -15
    override def requires = $($(ModuleOption(Creatures)))
    def about = $(
        "Creatures have no abilities, but every one of them is aggressive, like the " ~ "Fallen Valkyrie".hl ~ ":",
        "No clan shares a territory with a creature. A creature that appears or moves into a clan's territory attacks it, and units that move into a creature's territory must fight it.",
        "Creatures still move by their priorities, and their strength and fame don't change. Hrimgandr still never moves, but no longer raises Winter costs.",
        "Needs the " ~ "Creatures".hl ~ " module.",
        "Fan-made, not part of the published game.".styled(xstyles.warning),
    )
}

// The Lion Kingdom (lion.scala), a fan-made clan: the video game's Kingdom of Neustria (Gman, 2026-10-21, with Robotos's Chapel and
// alternative starting deck). On whenever the Lion plays (Game.modules), like New Blood, so its option isn't listed
case object LionKingdom extends Module("Lion Kingdom", "fan-made clan") {
    override def ready = true
    override def fan = true
    override def expansion = |(LionExpansion)
    // With New Blood's, before Wilderness, Creatures and Warchiefs
    override def priority = -2
    def about = $("The video game's Kingdom of Neustria as a clan: Lords on its Defense Towers, Chapels instead of Carved Stones, and Hildegard as its warchief.")
}

// Robotos's alternative starting deck for the Lion Kingdom (Discord, 2026-10-21): 2 Adventure (Explore or Move 1), 2 Toil (Build, or
// 1 food or 1 wood) and 2 Prey (Recruit 1, or 1 fame) instead of the usual six cards. It was a setup option at first; it is the Lion's
// own deck in every new game (LionOwnDeck, Gman, 2026-10-24), and this option, now hidden, stays for the games that picked it
case object LionStartingDeck extends GameOption with ToggleOption {
    val group = "Fan content".txt
    override def fold = |("Fan content")
    def valueOn = "Lion Kingdom: alternative starting deck".txt
}

// The Lion always starts with its own deck (LionStartingDeck's), in every new game (mandatoryFor); games started before it keep the
// usual starting cards unless they picked the old option, so their replays don't change
case object LionOwnDeck extends GameOption with ToggleOption {
    val group = "Fan content".txt
    override def fold = |("Fan content")
    def valueOn = "Lion Kingdom plays its own starting deck".txt
}

// A Lord stays on its Defense Tower while the Lion has figures in the territory, so it defends it in a fight (bug report, 2026-10-25):
// it needed the Lion to control the territory alone, which an attacker moving in ends, so Lords never added their combat points and
// went back to the reserve at the next move. Hidden, and given to every new game (mandatoryFor); older games replay the old way
case object LordsWhileContested extends GameOption with ToggleOption {
    val group = "Fan content".txt
    override def fold = |("Fan content")
    def valueOn = "Lion Kingdom's Lords stay while the Lion is in the territory".txt
}

// A Lord adds its 2 combat points to every fight in its territory, the Lion attacking or defending, against a clan or a creature
// (bug report, 2026-10-25: the Lion attacked a Draugr beside its own Lord, which added nothing; Gman chose this). Hidden, and given
// to every new game (mandatoryFor); older games count the Lords only defending against a clan, and replay that way
case object LordsInEveryFight extends GameOption with ToggleOption {
    val group = "Fan content".txt
    override def fold = |("Fan content")
    def valueOn = "Lion Kingdom's Lords fight in every combat in their territory".txt
}

// The Lion's upgrade card Ost (after the video game's Faith tree entry) in place of Abundance (the owner, 2026-10-25): a free Defense
// Tower, and for the rest of the year its Lords join the combats it starts next to them. Hidden, and given to every new game
// (mandatoryFor); older games keep Abundance (CardVersions.lionOst in editions.scala) and replay that way
case object LionOst extends GameOption with ToggleOption {
    val group = "Fan content".txt
    override def fold = |("Fan content")
    def valueOn = "Lion Kingdom plays Ost instead of Abundance".txt
}

// The Lion's Rise of the Twelve Peers recruits 1 unit in each territory it controls with a Chapel (the owner, 2026-10-25), instead
// of in up to 2 with a Defense Tower or Training Camp. Hidden, and given to every new game (mandatoryFor); older games keep the old
// card (CardVersions.lionOst in editions.scala) and replay that way
case object LionChapelPeers extends GameOption with ToggleOption {
    val group = "Fan content".txt
    override def fold = |("Fan content")
    def valueOn = "Lion Kingdom's Rise of the Twelve Peers recruits by its Chapels".txt
}

// The Lion's Hildegard's Vow gives 2 combat points for each of its Chapels next to the fight (the owner, 2026-10-25), instead of
// for each Lord (Ost brings the Lords). Hidden, and given to every new game (mandatoryFor); older games keep the old card
// (CardVersions.lionOst in editions.scala) and replay that way
case object LionChapelVow extends GameOption with ToggleOption {
    val group = "Fan content".txt
    override def fold = |("Fan content")
    def valueOn = "Lion Kingdom's Hildegard's Vow counts its Chapels".txt
}

// The Lion's Chapels give 1 lore each at harvest instead of 1 fame (the owner, 2026-10-25; the planned Stoat Kingdom's give fame).
// Hidden, and given to every new game (mandatoryFor); older games keep the fame and replay that way
case object LionChapelLore extends GameOption with ToggleOption {
    val group = "Fan content".txt
    override def fold = |("Fan content")
    def valueOn = "Lion Kingdom's Chapels give lore".txt
}

// The Lion's Faith (Gman, 2026-10-28): a fourth resource, 1 for each casualty its Lords or Hildegard deal in a fight and 1 for each
// Chapel at harvest (instead of lore); its clan upgrades cost Faith; Paladins (Recruit 1, and 1 more for each Faith
// paid, up to 4) in Ost's place, Rise of the Twelve Peers builds a free Defense Tower and Ost, only bringing the Lords, is the clan card. Hidden, and given to every new
// game (mandatoryFor); older games keep the lore and the earlier cards (CardVersions.lionFaith in editions.scala) and replay that way
case object LionFaith extends GameOption with ToggleOption {
    val group = "Fan content".txt
    override def fold = |("Fan content")
    def valueOn = "Lion Kingdom collects and spends Faith".txt
}

object Module {
    // The setup screen's section of a module that isn't fan-made (Robotos, 2026-10-26: one section per box)
    def section(m : Module) : String = m match {
        case Creatures => "Base game"
        case Wilderness => "Wilderness expansion"
        case Wastelands => "Wastelands expansion"
        case EventsModule | Sea | VictoryModule | Solo | TrainingFields | UnchartedHorizons => "Uncharted Horizons"
        case _ => "Modules and expansions"
    }

    val all : $[Module] = $(Creatures, Warchiefs, Wilderness, Wastelands, NewBlood, EventsModule, VictoryModule, Sea, Solo, UnchartedHorizons, TeamsVariant, Teams3v3, Teams2v2v2, TrainingFields, MineralsModule, TowersModule, WarchiefsPC, MilitaryModule, DragonVariant, AggressiveCreatures, LionKingdom, BearBorder)

    // The team variants and the number of players each one needs
    val teams : Map[Module, Int] = Map(TeamsVariant -> 4, Teams3v3 -> 6, Teams2v2v2 -> 6)

    // How many teams each variant has
    val sides : Map[Module, Int] = Map(TeamsVariant -> 2, Teams3v3 -> 2, Teams2v2v2 -> 3)
}

// WILDERNESS AND WASTELANDS: each expansion is two modules, its Environment tiles and its creatures, and each has bans (asked for by
// Robotos on 2026-10-26: Wilderness tiles with the base game's creatures only, no Ruins or Graveyard, no Hvedrung). Any part turns
// the expansion's code on (Meta.has); Wild.tilesOn, Wild.creaturesOn, Waste.tilesOn and Waste.creaturesOn say which. Games made
// before have ModuleOption(Wilderness) or ModuleOption(Wastelands), both parts, which are no longer offered (Meta.hiddenOptions)
case object WildernessTiles extends GameOption with ToggleOption with ImportantOption {
    val group = "Wilderness expansion".txt
    def valueOn = "Environment tiles".txt
    override val explain = $(
        "The Great Lake, two Geysers, two Ruins, the Swamp, the Poisonous Swamp and two High Peaks are shuffled into the map tiles after setup. Impassable borders (orange lines) can't be crossed.",
        "With the " ~ "Creatures".hl ~ " module, the Ancestral Graveyard (Spectral Warriors) and the Wyvern's Den (the Wyvern) are added too.",
        "Single tiles can be left out under " ~ "Ban Environment tiles".hl ~ ".",
    )
}

case object WildernessCreatures extends GameOption with ToggleOption with ImportantOption {
    val group = "Wilderness expansion".txt
    def valueOn = "Wilderness creatures".txt
    override val explain = $(
        "The Draugr Jötnar, the Eldthursar and Hvedrung join the creature deck.",
        "Needs the " ~ "Creatures".hl ~ " module. The Spectral Warriors and the Wyvern come with their tiles, the Ancestral Graveyard and the Wyvern's Den (" ~ "Environment tiles".hl ~ "). Single creatures can be left out under " ~ "Ban creatures".hl ~ ".",
    )
    override def required(all : $[BaseOption]) = $($(ModuleOption(Creatures)))
}

case object WastelandsTiles extends GameOption with ToggleOption with ImportantOption {
    val group = "Wastelands expansion".txt
    def valueOn = "Environment tiles".txt
    override val explain = $(
        "Seven Environment tiles (the Kobold Camp, the Jötnar Camp with Jötunn Blainn, Naströnd, Landvidi, Thor's Wrath, Urdarbrunn and Vedrfolnir) are shuffled into the map tiles after setup; with the Wilderness Environment tiles, twelve of both are drawn.",
        "A Central tile can replace the starting tile (" ~ "Central tile".hl ~ " below, also offered without this module). Single tiles can be left out under " ~ "Ban Environment tiles".hl ~ ".",
    )
}

case object WastelandsCreatures extends GameOption with ToggleOption with ImportantOption {
    val group = "Wastelands expansion".txt
    def valueOn = "Wastelands creatures".txt
    override val explain = $(
        "Rock Golems, Myrkalfar, Giant Boars, Kobolds and Valdemar join the creature deck.",
        "Needs the " ~ "Creatures".hl ~ " module. Single creatures can be left out under " ~ "Ban creatures".hl ~ ".",
    )
    override def required(all : $[BaseOption]) = $($(ModuleOption(Creatures)))
}

// An Environment tile left out of the pile; the two Geysers, Ruins and High Peaks are banned together (Wild.tiles, Waste.tiles)
case class BanEnvironment(id : String) extends GameOption with ToggleOption {
    val group = "Ban Environment tiles".txt
    override def fold = |("Ban Environment tiles")
    def wild = id.startsWith("wild-")
    def valueOn = BanEnvironment.name(id).txt
    override def decorate(e : Elem) = e ~ " " ~ ("(" + wild.?("Wilderness").|("Wastelands") + BanEnvironment.creatureOnly.has(id).??(", Creatures module") + ")").spn(xstyles.smaller85)
    override val explain = $(("Leaves the " ~ BanEnvironment.name(id).hl ~ " out of the Environment tiles."))
    override def required(all : $[BaseOption]) = $($(wild.?(WildernessTiles).|(WastelandsTiles)))
}

object BanEnvironment {
    val creatureOnly = $("wild-graveyard", "wild-den")
    val all : $[BanEnvironment] = ($("wild-lake", "wild-geyser", "wild-ruins", "wild-swamp", "wild-poison", "wild-peaks") ++ creatureOnly ++ Tiles.wastelands./(_.id))./(BanEnvironment(_))

    def name(id : String) : String = id match {
        case "wild-lake" => "Great Lake"
        case "wild-geyser" => "Geysers (both)"
        case "wild-ruins" => "Ruins (both)"
        case "wild-swamp" => "Swamp"
        case "wild-poison" => "Poisonous Swamp"
        case "wild-peaks" => "High Peaks (both)"
        case "wild-graveyard" => "Ancestral Graveyard (Spectral Warriors)"
        case "wild-den" => "Wyvern's Den (the Wyvern)"
        case _ => Waste.name(id)
    }

    def allowed(tile : String, options : $[BaseOption]) = options.of[BanEnvironment].exists(b => tile == b.id || tile.startsWith(b.id + "-")).not
}

// A Wilderness or Wastelands creature left out of the creature deck (Creature.deck)
case class BanCreature(id : String) extends GameOption with ToggleOption {
    val group = "Ban creatures".txt
    override def fold = |("Ban creatures")
    def kind = BanCreature.kinds.%(_.id == id).head
    def wild = Creature.wild.exists(_.kind == kind)
    def valueOn = kind.title.txt
    override def decorate(e : Elem) = e ~ " " ~ ("(" + wild.?("Wilderness").|("Wastelands") + ")").spn(xstyles.smaller85)
    override val explain = $((kind.title.hl ~ ": " ~ CombatText(kind.text)))
    override def required(all : $[BaseOption]) = $($(wild.?(WildernessCreatures).|(WastelandsCreatures)))
}

object BanCreature {
    val kinds : $[CreatureKind] = (Creature.wild ++ Creature.waste)./(_.kind).distinct
    val all : $[BanCreature] = kinds./(k => BanCreature(k.id))

    def allowed(kind : CreatureKind, options : $[BaseOption]) = options.of[BanCreature].exists(_.id == kind.id).not
}

case class ModuleOption(module : Module) extends GameOption with ToggleOption with ImportantOption {
    // The Warchiefs module has its own section, with its parts (WarchiefsChoice); fan-made modules have theirs, Fan content;
    // the core box's Creatures is under Base game, with More Creatures, and Events and Sea under Uncharted Horizons, with its map tiles
    val group = (module == Warchiefs).?(WarchiefsChoice.group).|(module.fan.?("Fan content").|(Module.section(module)).txt)
    // Fan content folds like the single-choice sections (Gman, 2026-10-29)
    override def fold = module.fan.?("Fan content")
    def valueOn = (module == Warchiefs).?("Full expansion: warchief pawns and upgrade cards").|((module == Creatures).?("Creatures module").|(module.label)).txt
    override def forcedOff(all : $[BaseOption]) = (module == Warchiefs).??(WarchiefsChoice.others(this))
    override val explain = module.about ++ module.ready.not.$("Not implemented yet.".styled(xstyles.warning))
    // A module that isn't ready requires itself, so it can never be turned on
    override def required(all : $[BaseOption]) = module.ready.?(module.requires).|($($[BaseOption](this)))
}
