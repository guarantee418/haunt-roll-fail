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

import root.elem._

import hrf.meta._
import hrf.elem._
import hrf.reflect._
import hrf.options._

import helper.&


// Root's setup sections fold like Northgard's (BaseOption.fold, hrf.ui.Folds): folded, a section shows only what is picked.
// Display only, so games and their records are unchanged; "root." keeps the open sections apart from other games' ones
trait FoldedOption { self : BaseOption =>
    override def fold = |("root." + group.text)
}

case class UnknownOption(o : String) extends GameOption {
    val group = "Unknown"
    val valueOn = "Unknown Option " ~ o
}

// RANKED OR UNRANKED (asked for on 2026-11-22): an online game picks whether it counts toward the rankings. A ranked game can't be
// started with anything not approved for ranked play (Meta.rankedCheck), and those options are marked "unranked" on the setup screen
// (Meta.rankingUnapproved). An unranked game counts in the stats, not in the ratings or win rates. Not offered in local games or Mirror
trait RankingOption extends RankingChoice with OneOfGroup with FoldedOption {
    val group : Elem = "Ranking"
}

case object RankedGame extends RankingOption {
    val ranked = true
    val valueOn = "Ranked".hlb
    override val explain = $(
        "Counts toward the " ~ "Elo ratings".hh ~ " and the faction and turn order stats, when every seat is a person (a game with a bot never counts).",
        "Approved for ranked play: the official factions (base, expansions and " ~ "Homeland".hh ~ ") and the " ~ "Negabond".styled(NB) ~ " as a second " ~ "Vagabond".styled(VB) ~ ", the official maps and the fan maps " ~ "Tidal Flats".styled(ED) ~ ", " ~ "Tundra".styled(RF) ~ " and " ~ "Gloom".styled(styles.forest) ~ ", the decks " ~ "Capitalists and Anarchists".hh ~ ", " ~ "Exiles and Partisans".hh ~ ", " ~ "Squires and Disciples".hh ~ ", " ~ "Might and Right".hh ~ ", " ~ "Mixed".hh ~ " and " ~ "Dawn and Dusk".hh ~ ", the official landmarks and the " ~ "Simple Landmarks".hh ~ ", and the official hirelings.",
        "Not approved: fan, playtest and the other mirror factions, the other fan maps and decks, the " ~ "Action! Deck Booster".hh ~ ", " ~ "Turnlings".hh ~ ", " ~ "Doomed Vagabonds".hh ~ " and " ~ "Knave Captains".hh ~ ", and the " ~ "Legends Scenarios".hh ~ ". The setup screen marks each of them " ~ "unranked".styled(xstyles.warning) ~ ".",
    )
}

case object UnrankedGame extends RankingOption {
    val ranked = false
    val valueOn = "Unranked".hlb
    override val explain = $(
        "Doesn't count toward the " ~ "Elo ratings".hh ~ " or win rates, whatever is played; the faction and turn order stats still count it.",
        "Anything may be played, approved for ranked play or not.",
    )
}

trait MapOption extends GameOption with OneOfGroup with ImportantOption with FoldedOption {
    val group : Elem = "Map"
    // the Fan Maps section picks the Map too
    override def oneOf : Any = "Map"
}

// Fan content gets sections of its own, after the official ones, each folded like the rest (2026-11-17): Fan Maps, Fan Decks,
// the Fan Hirelings folder, the Scenarios folder and, in the faction picker and Advanced Setup's pool, Fan Factions. A new fan map or
// deck extends FanMap or FanDeck; a new fan hireling option or scenario option sets its folder (Meta.fanHirelings, Meta.scenarios).
// Display only: the options and what games record are unchanged
trait FanMap extends MapOption {
    override val group : Elem = "Fan Maps"
}

case object AutumnMap extends MapOption {
    val valueOn = "Autumn".styled(Mouse)
    override val explain = $(
        "The " ~ "Root".hh ~ " base game map.",
    )
}

case object WinterMap extends MapOption {
    val valueOn = "Winter".styled(Bird)
    override val explain = $(
        "The back of the base game board.",
    )
}

case object LakeMap extends MapOption {
    val valueOn = "Lake".styled(Rabbit)
    override val explain = $(
        "The " ~ "Underworld".hh ~ " expansion map: a lake in the middle, crossed by the " ~ "Ferry".hh ~ ".",
    )
}

case object MountainMap extends MapOption {
    val valueOn = "Mountain".styled(Fox)
    override val explain = $(
        "The " ~ "Underworld".hh ~ " expansion map: some paths are closed until cleared, and the " ~ "Tower".hh ~ " stands in the middle.",
    )
}

case object TidalMap extends FanMap {
    val valueOn = "Tidal Flats".styled(ED)
    override val explain = $(
        "A beautiful map by " ~ "Endgamer1331".hl ~ "!",
        "At the start of the game a randomly chosen central clearing is " ~ "Flooded".styled(ED) ~ " and all neighbouring clearings become " ~ "coastal clearings connected by water.",
        &("The " ~ "Boardwalk".styled(UD) ~ " paths are just regular paths,") ~ &(" but they don't divide forests.")
    )
}

case object TundraMap extends FanMap {
    val valueOn = "Tundra".styled(RF)
    override val explain = $(
        "A winter map by " ~ "Endgamer1331".hl ~ "!",
        "Each time all players make a turn, a die is rolled and the " ~ "Blizzard".hl ~ "moves, blocking another path between clearings, or the river."
    )
}

case object GloomMap extends FanMap {
    val valueOn = "Gloom".styled(styles.forest)
    override val explain = $(
        "A spooky map by " ~ "totgeboren".hl ~ "!",
        "The river divides forests.",
        "The clearing in the " ~ "River Mouth".hh ~ " are connected by river to each other.",
        "The " ~ "Jaw Clearings".hl ~ " are connected to a single nearby forest each."
    )
}

case object GorgeMap extends MapOption {
    val valueOn = "Gorge".styled(Fox)
    override val explain = $(
        "The " ~ "Dam".hl ~ " path between " ~ "Forge".hh ~ " and " ~ "Saloon".hh ~ " divides forests.",
        "The " ~ "Bridge".hl ~ " path between " ~ "Homestead".hh ~ " and " ~ "Lookout".hh ~ " does not divide forests,",
        "and neither do the gorge sides."
    )
}

case object MarshMap extends MapOption {
    val valueOn = "Marsh".styled(LDvE)
    override val explain = $(
        "The " ~ "Homeland".hh ~ " marsh map, with 15 clearings.",
        "With up to 4 players, one clearing of each colored pair is " ~ "Flooded".styled(ED) ~ ": it is no longer a clearing, and the paths on its flood marker link its neighbours.",
        "With 5 or more players, no clearing is flooded, and one of each pair has no suit and gets " ~ "Mousehold".styled(Mouse) ~ ", " ~ "Foxburrow".styled(Fox) ~ " or " ~ "Rabbittown".styled(Rabbit) ~ ".",
        "Two ruins go in the unnumbered ruin slots, two in the lowest numbered slots that are not flooded."
    )
}

// The Doomed Expansion's Urban Legends maps (fan-made, UrbanBoard and LegendsBoard in root/maps.scala)
case object UrbanMap extends FanMap {
    val valueOn = "Urban".styled(Fox)
    override val explain = $(
        "Urban: Districts & Denizens".hh ~ ", a city map from slugfacekillah's " ~ "Doomed Expansion".hl ~ ".",
        "The city blocks between the roads are the forests.",
        "Two rivers meet at the " ~ "Wharf".hh ~ ".",
        FanInfo.doomedLink
    )
}

case object LegendsMap extends FanMap {
    val valueOn = "Legends".styled(styles.forest)
    override val explain = $(
        "Legends".hh ~ ", a forest map with a castle in each corner, from slugfacekillah's " ~ "Doomed Expansion".hl ~ ".",
        "The ruins are in the four castles.",
        "With a " ~ "Legends Scenario".hh ~ ", its space goes in the stone circle, in the forest south of the " ~ "Watchtower".hh ~ ".",
        FanInfo.doomedLink
    )
}

case object DeepWoodsMap extends FanMap {
    val valueOn = "Deep Woods".styled(styles.forest)
    override val explain = $(
        "Deep Woods".hh ~ ", the past, a map from slugfacekillah's fan-made " ~ "Outsiders Expansion".hl ~ ".",
        "With " ~ "Settling".hh ~ " (the toggle under the maps, on by default) the clearings start unsettled and are settled during the game; without it each clearing tile is on the clearing of its number from the start.",
        "A river runs across the middle, with a bridge on each path over it. The ruins are in the four inner clearings.",
        FanInfo.outsidersLink
    )
}

case object WastelandsMap extends FanMap {
    val valueOn = "Wastelands".styled(Rabbit)
    override val explain = $(
        "Wastelands".hh ~ ", the future, a map from slugfacekillah's fan-made " ~ "Outsiders Expansion".hl ~ ".",
        "A walled city of three clearings in the middle, where two rivers meet at the " ~ "Cistern".hh ~ ".",
        "With " ~ "Water and Barren Trails".hh ~ " (the toggle under the maps, on by default) Water is the currency and moving along a barren trail costs something; without it the barren trails are plain paths.",
        FanInfo.outsidersLink
    )
}

// The Outsiders maps' own rules (Law of Slug S.1 and S.2, root/outsiders-maps.scala), only with their map, shown unfolded right under
// the maps so they are seen when the map is picked (2026-11-18); both are on by default (Water and Barren Trails since 2026-11-19)
case object DeepWoodsSettling extends GameOption with ToggleOption {
    val group = "Fan Map Rules"
    val valueOn = "Deep Woods".styled(styles.forest) ~ SpacedDash ~ "Settling".hh
    override def required(all : $[BaseOption]) = $($(DeepWoodsMap))
    override val explain = OutsidersMaps.settlingRules
}

case object WastelandsWater extends GameOption with ToggleOption {
    val group = "Fan Map Rules"
    val valueOn = "Wastelands".styled(Rabbit) ~ SpacedDash ~ "Water and Barren Trails".hh
    override def required(all : $[BaseOption]) = $($(WastelandsMap))
    override val explain = OutsidersMaps.waterRules
}

// Drawn once when the game is created (Meta.startOptions), so the game and its log only ever have the drawn map.
// No longer offered (2026-11-18): only Random Official is; a setup a browser saved with it gets Random Official (completeSaved)
case object RandomMap extends MapOption {
    val valueOn = "Random".hh
    override val explain = $(
        "One of the maps above is picked at random when the game starts.",
        "With " ~ "Default".hh ~ " clearings, a map other than " ~ "Autumn".styled(Mouse) ~ " gets " ~ "No Clusters".hh ~ "."
    )
}

case object RandomOfficialMap extends MapOption {
    val valueOn = "Random Official".hh
    override val explain = $(
        "One of the official maps is picked at random when the game starts:",
        "Autumn".styled(Mouse) ~ ", " ~ "Winter".styled(Bird) ~ ", " ~ "Lake".styled(Rabbit) ~ ", " ~ "Mountain".styled(Fox) ~ ", " ~ "Gorge".styled(Fox) ~ " or " ~ "Marsh".styled(LDvE) ~ ".",
        "With " ~ "Default".hh ~ " clearings, a map other than " ~ "Autumn".styled(Mouse) ~ " gets " ~ "No Clusters".hh ~ "."
    )
}


trait ClearingsOption extends GameOption with OneOfGroup with FoldedOption {
    val group = "Clearings"
}

case object DefaultClearings extends ClearingsOption {
    val valueOn = "Default".hh
    override def required(all : $[BaseOption]) = $($(AutumnMap), $(RandomMap), $(RandomOfficialMap))
    override val explain = $("As printed on the " ~ "Autumn".styled(Mouse) ~ " map," ~ HorizontalBreak ~ "tree leaves color matches the clearing suit.")
}

case object AllRandomClearings extends ClearingsOption {
    val valueOn = "All Random".hh
    override val explain = $("All suits are assigned randomly.")
}

case object NoClustersClearings extends ClearingsOption {
    val valueOn = "No Clusters".hh
    override val explain = $("All suits are assigned randomly," ~ HorizontalBreak ~ "no two connected clearings share suit.")
}

case object SuitPairsClearings extends ClearingsOption {
    val valueOn = "Suit Pairs".hh
    override val explain = $("All suits are assigned randomly," ~ HorizontalBreak ~ "each clearing is connected to exactly one another clearing of the same suit.")
}

case object ConnectedClearings extends ClearingsOption {
    val valueOn = "Connected".hh
    override val explain = $("All suits are assigned randomly," ~ HorizontalBreak ~ "clearings of the same suit are all connected.")
}

case object ThreeFourFiveClearings extends ClearingsOption {
    val valueOn = "3".hh ~ "/" ~ "4".hh ~ "/" ~ "5".hh
    override val explain = $(
        "A random suit " ~ "is assigned" ~ " to the " ~ "three".hh ~ " clearings " ~ "with the most building slots.",
        "Another random suit " ~ "is assigned" ~ " to the " ~ "five".hh ~ " clearings " ~ "with the least building slots.",
        "Remaining suit " ~ "is assigned" ~ " to the remaining " ~ "four".hh ~ " clearings."
    )
}


trait DeckOption extends GameOption with OneOfGroup with ImportantOption with FoldedOption {
    val group : Elem = "Deck"
    // the Fan Decks section picks the Deck too
    override def oneOf : Any = "Deck"
}

trait FanDeck extends DeckOption {
    override val group : Elem = "Fan Decks"
}

// under Fan Decks since 2026-11-21 (owner's ask), Capitalists and Anarchists taking its place among the Decks; still official for the rankings
case object StandardDeck extends FanDeck {
    val valueOn = "Might and Right".hh
    override val explain = $(
        "The deck from the " ~ "Root".hh ~ " base game.",
    )
}

case object ExilesDeck extends DeckOption {
    val valueOn = "Exiles and Partisans".hh
    override val explain = $(
        "The official alternative deck, with " ~ "Partisans".hh ~ ", " ~ "Coffin Makers".hh ~ ", " ~ "Tunnels".hh ~ " and other new cards.",
    )
}

case object MixedDeck extends FanDeck {
    val valueOn = "Mixed".hh
    override val explain = $(
        "Both official decks mixed to take one instance" ~ &("of the most " ~ "effect cards".hh ~ "."),
        "Scouting Party".hh ~ " and " ~ "Coffin Makers".hh ~ " are excluded.",
        "Soup Kitchens".hh ~ " are replaced with " ~ "Borscht Kitchens".hh ~ ".",
        "Boat Builders".hh ~ " are excluded if " ~ RF.name.styled(RF) ~ " is in the game.",
        "Corvid Planners".hh ~ " are excluded if " ~ CC.name.styled(CC) ~ " is in the game.",
        "Ambushes, dominances and item craft cards same as in all decks.",
        Image("borscht-kitchens", styles.card)
    )
}

case object DuskDeck extends FanDeck {
    val valueOn = "Dawn and Dusk".hh
    override val explain = $(
        "An alternative deck by " ~ "its_rudimentary".hl ~ " and " ~ "Endgamer1331".hl ~ "!",
        "For " ~ "Service".styled(styles.FoxRabbitMouse) ~ " and " ~ "Phase".styled(styles.phase) ~ " cards optional rules for " ~ VB.elem ~ " interaction are available.",
    )
}

case object SquiresDeck extends DeckOption {
    val valueOn = "Squires and Disciples".hh
    override val explain = $(
        "The " ~ "Homeland".hh ~ " deck, with new cards such as " ~ "Friend of the ___".hh ~ ", " ~ "Silver-Tongue".hh ~ " and " ~ "Feather Rufflers".hh ~ ".",
    )
}

case object FrankenDeck extends FanDeck {
    val valueOn = "Frankendeck".hh
    override val explain = $(
        "A deck builder from " ~ Link("https://boardgamegeek.com/boardgameexpansion/482234/the-doomed-expansion-fan-expansion-for-root", "The Doomed Expansion", $(xstyles.link)) ~ ", fan-made by " ~ "slugfacekillah".hl ~ " with " ~ "Endgamer1331".hl ~ ".",
        "A core of " ~ "30".hh ~ " cards in its own art: the " ~ "20".hh ~ " items, " ~ "5".hh ~ " ambushes, " ~ "4".hh ~ " dominances and a " ~ "Sabotage!".hh ~ " card.",
        "Then " ~ "30".hh ~ " persistent effects are drawn at random from the " ~ "Might and Right".hh ~ ", " ~ "Exiles and Partisans".hh ~ ", " ~ "Dawn and Dusk".hh ~ " and " ~ "Squires and Disciples".hh ~ " decks, by suit and crafting cost:",
        "one crafting activation or none: " ~ "3".hh ~ " bird, " ~ "3".hh ~ " fox, " ~ "3".hh ~ " mouse and " ~ "4".hh ~ " rabbit;",
        "two or more: " ~ "4".hh ~ " bird, " ~ "4".hh ~ " fox, " ~ "4".hh ~ " mouse and " ~ "5".hh ~ " rabbit.",
        "Favors".hh ~ " are left out. " ~ "Boat Builders".hh ~ " and " ~ "Corvid Planners".hh ~ " leave the deck with " ~ RF.name.styled(RF) ~ " or " ~ CC.name.styled(CC) ~ ", as in the " ~ "Mixed".hh ~ " deck.",
        FanInfo.doomedLink,
        Image("franken-bird-ambush", styles.card)
    )
}

case object DarkDeck extends FanDeck {
    val valueOn = "Dark Deck".hh
    override val explain = $(
        "A 66 card deck from " ~ Link("https://boardgamegeek.com/boardgameexpansion/482234/the-doomed-expansion-fan-expansion-for-root", "The Doomed Expansion", $(xstyles.link)) ~ ", fan-made by " ~ "slugfacekillah".hl ~ ".",
        "Its six " ~ "Abomination".styled(Abomination) ~ " cards count as fox, rabbit or mouse, but not bird.",
        "The " ~ "Cursed!".hh ~ " card has no suit: it can't be spent or discarded, only given, taken or swapped by effects, or crafted with four of any suit for " ~ 3.vp ~ " and the " ~ "Cursed Idol".hh ~ ".",
        "A fifth " ~ "Dark Dominance".hh ~ " needs three adjacent ruled clearings and the " ~ "Cursed Idol".hh ~ ".",
        "Its shared supply adds a " ~ "Torch".hh ~ ", the " ~ "Book of Names".hh ~ ", the " ~ "Rabbit's Paw".hh ~ " and the " ~ "Cursed Idol".hh ~ " to the craftable items.",
        FanInfo.doomedLink,
        Image("dark-dark-dominance", styles.card)
    )
}

case object UpstartsDeck extends FanDeck {
    val valueOn = "Upstarts & Renegades".hh
    override val explain = $(
        "A 54 card deck from " ~ "The Outsiders Expansion".hh ~ ", fan-made by " ~ "slugfacekillah".hl ~ ".",
        "Its ambushes, items and dominances are the base deck's; its " ~ "25".hh ~ " persistent effects are new: " ~ "Escorts".hh ~ ", " ~ "Field Medic".hh ~ ", " ~ "Murder Hawk".hh ~ ", " ~ "Trailblazers".hh ~ ", " ~ "Baron Von Costume".hh ~ ", " ~ "Goon for Hire".hh ~ " and more.",
        "Its three " ~ "Sabotage!".hh ~ " cards are the " ~ "Exiles and Partisans".hh ~ "' " ~ "Saboteurs".hh ~ ".",
        FanInfo.outsidersLink,
        Image("upstarts-murder-hawk", styles.card)
    )
}

case object SorceryDeck extends FanDeck {
    val valueOn = "Sorcery of the Enchanted Woods".hh
    override val explain = $(
        "A 55 card fan deck from the " ~ Link("https://steamcommunity.com/sharedfiles/filedetails/?id=2413287097", "Tabletop Simulator mod", $(xstyles.link)) ~ " uploaded by " ~ "slugfacekillah".hl ~ " (not his own; its designer isn't named).",
        "Its ambushes, items and four dominances are the base deck's; its " ~ "25".hh ~ " persistent effects are spells: " ~ "The Wizards Tower".hh ~ ", " ~ "Garden of Glory".hh ~ ", " ~ "Hailstorm".hh ~ ", " ~ "Dispel".hh ~ ", " ~ "Spell of Flight".hh ~ ", " ~ "Treewalking".hh ~ " and more.",
        "A fifth " ~ "Dominance".hh ~ " wins with six crafted improvements.",
        "There is no rulebook: the card text is the rules.",
        Image("sorcery-wizards-tower", styles.card)
    )
}

// among the Decks since 2026-11-21 (owner's ask): the site's playable version of the base deck
case object CapitalistsDeck extends DeckOption {
    val valueOn = "Capitalists and Anarchists".hh
    override val explain = $(
        "A playable version of the " ~ "Root".hh ~ " base game's deck, " ~ "Might and Right".hh ~ ", by the site's owner. Its items, ambushes and dominances are the base deck's, and some of its effect cards are changed:",
        "Command Warren".hh ~ ", " ~ "Cobbler".hh ~ ", " ~ "Better Burrow Bank".hh ~ ", " ~ "Brutal Tactics".hh ~ " and " ~ "Scouting Party".hh ~ " cost one card; they (but " ~ "Better Burrow Bank".hh ~ ") and " ~ "Sappers".hh ~ " are paid for each use with a card matching the clearing, or by discarding them, and " ~ "Brutal Tactics".hh ~ " gives the defender no point.",
        "Better Burrow Bank".hh ~ " goes back to hand when used. " ~ "Stand and Deliver!".hh ~ " is used once, at the next Birdsong. " ~ "Tax Collector".hh ~ " and " ~ "Codebreakers".hh ~ " are used once, when crafted.",
        "Scouting Party".hh ~ " cancels an ambush. " ~ "Royal Claim".hh ~ " must be used at the next Birdsong.",
        "The " ~ "Dawn and Dusk".hh ~ " deck's three " ~ "Service".styled(styles.FoxRabbitMouse) ~ " cards replace the " ~ "Favors".hh ~ ".",
        Image("ca-command-warren", styles.card)
    )
}

case object ActionBooster extends GameOption with ToggleOption with FoldedOption {
    val group = "Fan Decks"
    val valueOn = "Action! Deck Booster".hh
    override val explain = $(
        "Ten more cards for big games from " ~ Link("https://boardgamegeek.com/boardgameexpansion/482234/the-doomed-expansion-fan-expansion-for-root", "The Doomed Expansion", $(xstyles.link)) ~ ", fan-made by " ~ "slugfacekillah".hl ~ ".",
        "One booster is shuffled into the deck for each player above " ~ "5".hh ~ " (two at most), or one with " ~ "5".hh ~ " players.",
        "An " ~ "Action!".hh ~ " card is spent from hand during its owner's turn for its effect (the sword's extra hit also in a battle they are in); otherwise it is a card of its suit.",
        "The two " ~ "Secret!".hh ~ " cards are birds: either can be discarded to draw the top card of the deck, one cancels an ambush played against its owner, the other scores " ~ 3.vp ~ " at the start of Birdsong with three crafted improvements or a fulfilled Dominance.",
        "Using an " ~ "Action!".hh ~ " card as the item it shows is not done yet.",
        FanInfo.doomedLink,
        Image("action-sword", styles.card)
    )
}

case object NonBirdPartisans extends GameOption with ToggleOption with FoldedOption {
    val group = "Cards"
    val valueOn = "Partisans".styled(styles.get(Bird)) ~ SpacedDash ~ "Discard".hh ~ " " ~ "Birds".styled(Bird)
    override def required(all : $[BaseOption]) = $($(ExilesDeck), $(MixedDeck), $(FrankenDeck))
    override val explain = $(
        "As printed, " ~ "Partisan".hh ~ " cards don't imply that the " ~ Bird.elem ~ " cards are discarded.",
        "This option forces the discard of the " ~ Bird.elem ~ " cards.",
    )
}

case object UnthematicCoffinMakers extends GameOption with ToggleOption with FoldedOption {
    val group = "Cards"
    val valueOn = "Coffin Makers".styled(styles.get(Rabbit)) ~ SpacedDash ~ "Any Warrior Recycle".hh
    override def required(all : $[BaseOption]) = $($(ExilesDeck), $(FrankenDeck))
    override def forcedOff(all : $[BaseOption]) = $(ErrataCoffinMakers)
    override val explain = $(
        "Without this option, only " ~ "forcibly".styled(styles.hit) ~ " removed warriors go to " ~ (FigureSpace + "Coffin Makers" + FigureSpace).styled(xlo.pre, xstyles.outlined, styles.get(Rabbit)) ~ ".",
        "This option expands this to all instances of warriors returned to the supply."
    )
}

case object ErrataCoffinMakers extends GameOption with ToggleOption with FoldedOption {
    val group = "Cards"
    val valueOn = "Coffin Makers".styled(styles.get(Rabbit)) ~ SpacedDash ~ "Homeland Errata".hh
    override def required(all : $[BaseOption]) = $($(ExilesDeck), $(FrankenDeck))
    override def forcedOff(all : $[BaseOption]) = $(UnthematicCoffinMakers)
    override val explain = $(
        "As reprinted in " ~ "Homeland".hh ~ ", warriors removed from the map to their supply by any effect go to " ~ (FigureSpace + "Coffin Makers" + FigureSpace).styled(xlo.pre, xstyles.outlined, styles.get(Rabbit)) ~ ", unless they are replaced.",
        "Warriors returned from anywhere else (officers, acolytes, payments, captives) go to their supply."
    )
}

case object UnthematicPropagandaBureau extends GameOption with ToggleOption with FoldedOption {
    val group = "Cards"
    val valueOn = "Propaganda Bureau".styled(styles.get(Fox)) ~ SpacedDash ~ "Optional Placement".hh
    override def required(all : $[BaseOption]) = $($(ExilesDeck), $(MixedDeck), $(FrankenDeck))
    override val explain = $(
        "With this option, " ~ "Propaganda Bureau".styled(styles.get(Fox)) ~ " can remove an enemy " ~ "warrior".hh ~ " even if own " ~ "warrior".hh ~ " can't be placed" ~ " afterwards."
    )
}

case object TunnelsIgnoreRaid extends GameOption with ToggleOption with FoldedOption {
    val group = "Cards"
    val valueOn = "Tunnels".styled(styles.get(Rabbit)) ~ SpacedDash ~ "Ignore ".hh ~ "Raid".styled(CC)
    override def required(all : $[BaseOption]) = $($(ExilesDeck), $(MixedDeck), $(FrankenDeck))./~(d => $(d ++ $(IncludeFaction(CC)), d ++ $(IncludeFaction(RI))))
    override val explain = $(
        "With this option, triggered " ~ "Raid".styled(CC) ~ " doesn't benefit from " ~ (FigureSpace + "Tunnels" + FigureSpace).styled(xlo.pre, xstyles.outlined, styles.get(Rabbit)) ~ "."
    )
}

case object TunnelsIgnoreTradePosts extends GameOption with ToggleOption with FoldedOption {
    val group = "Cards"
    val valueOn = "Tunnels".styled(styles.get(Rabbit)) ~ SpacedDash ~ "Ignore ".hh ~ "Trade Posts".styled(RF)
    override def required(all : $[BaseOption]) = $($(ExilesDeck), $(MixedDeck), $(FrankenDeck))./~(d => $(d ++ $(IncludeFaction(RF))))
    override val explain = $(
        "With this option, " ~ "TradePosts".styled(RF) ~ " aren't considered crafting pieces for " ~ (FigureSpace + "Tunnels" + FigureSpace).styled(xlo.pre, xstyles.outlined, styles.get(Rabbit)) ~ "."
    )
}


trait SeatingOption extends GameOption with OneOfGroup {
    val group = "Seating Order"
}

case object SeatingGiven extends SeatingOption {
    val valueOn = "Given".hh
    override val explain = $(
        "Players are seated in the order entered.",
    )
}

case object SeatingRandom extends SeatingOption {
    val valueOn = "Random".hh
    override val explain = $(
        "Players' seating order is randomized.",
    )
}


trait FactionSeatingOption extends GameOption with OneOfGroup {
    val group = "Faction Seating Order"
}

case object FactionSeatingGiven extends FactionSeatingOption {
    val valueOn = "Given".hh
}

case object FactionSeatingRandom extends FactionSeatingOption {
    val valueOn = "Random".hh
}


trait SetupOrderOption extends GameOption with OneOfGroup {
    val group = "Setup Order"
}

case object SetupOrderPriority extends SetupOrderOption {
    val valueOn = "Faction Priorities".hh
}

case object SetupOrderReverse extends SetupOrderOption {
    val valueOn = "Reverse Turn Order".hh
}


trait SetupTypeOption extends GameOption with OneOfGroup with FoldedOption {
    val group = "Setup Type"
}

case object SetupTypeCorners extends SetupTypeOption {
    val valueOn = "Corners".hh
    override val explain = $(
        "Most militant factions setup in the map corners.",
    )
}

case object SetupTypeHomelands extends SetupTypeOption {
    val valueOn = "Homelands".hh
    override val explain = $(
        "Factions setup by claiming " ~ &("clearings as " ~ "Homelands".hh ~ "."),
    )
}


trait CardDraftOption extends GameOption with OneOfGroup {
    val group = "Card Draft"
}

case object CardDraftStandard extends CardDraftOption {
    val valueOn = "Draw Three".hh
}

case object CardDraftFive extends CardDraftOption {
    val valueOn = "Draw Five".hh ~ " / " ~ "Keep Three".hh
}

// Lilypad Diaspora's Frog cards go into the shared deck once every faction is set up, so no starting hand has
// any (as in Advanced Setup). Hidden and on by default for new games; games started without it keep the old
// timing (shuffled in during Lilypad's setup) so their replays don't change.
case object FrogCardsAfterStartingHands extends GameOption with ToggleOption {
    val group = "Card Draft"
    val valueOn = "Frog".hh ~ " cards after starting hands"
}

// With the five-card draft (Advanced Setup's A.7 to A.10) the starting hands are drawn before any faction sets up, so the
// Frog cards go into the shared deck during Lilypad Diaspora's own setup, as its board says (3rd step), and a faction set up
// after it may draw them in its setup (the Woodland Alliance's supporters). Without the draft FrogCardsAfterStartingHands still
// waits until every faction is set up, as the hands are drawn then. Hidden and on by default for new games (2026-11-11);
// older games keep waiting so their replays don't change.
case object FrogCardsAtDiasporaSetup extends GameOption with ToggleOption {
    val group = "Card Draft"
    val valueOn = "Frog".hh ~ " cards at the Diaspora's setup"
}

// Discarded Frog cards skip the Lizard Cult's Lost Souls and go straight to the Pond (or the other Lilypad
// versions' piles). Hidden and on by default for new games; older games keep sending them to the Lost Souls
// so their replays don't change.
case object FrogCardsSkipLostSouls extends GameOption with ToggleOption {
    val group = "Card Draft"
    val valueOn = "Frog".hh ~ " cards skip the " ~ "Lost Souls".hh
}

// Lilypad Diaspora picks which clearings' enclaves it crafts with, and an enclave used this turn stays used
// when another is removed. Hidden and on by default for new games; older games keep crafting by suit only
// so their replays don't change.
case object FrogsCraftByClearing extends GameOption with ToggleOption {
    val group = "Card Draft"
    val valueOn = "Lilypad Diaspora".hh ~ " crafts by clearing"
}

// Companions stays out until the frog card turned into another suit with it is discarded (spent), and can't turn another
// one meanwhile. Hidden and on by default for new games; older games discard Companions as soon as the card is turned
// so their replays don't change.
case object CompanionsUntilSpent extends GameOption with ToggleOption {
    val group = "Card Draft"
    val valueOn = "Companions".hh ~ " discarded when its card is spent"
}

// Governors (17.2.3): Lilypad Diaspora can't flip its enclaves at an enemy Council's Governing assemblies (Provoke, Reconcile,
// Fears Come to Pass, Negotiations, Militias). Hidden and on by default for new games; older games keep flipping there so their
// replays don't change.
case object GovernorsStopEnclaveFlips extends GameOption with ToggleOption {
    val group = "Card Draft"
    val valueOn = "Governors".hh ~ " stop enclave flips"
}

// Outrage (and the playtest Pacifism) with no card to give: the hand is shown to the Alliance in its action pane, and the game
// waits for its OK instead of going on with only a notification. Hidden; new games got it from 2026-10-17 to 2026-10-31, when
// The owner asked for the notification back (it now shows the next time the player opens the game), so those games keep waiting.
case object OutragePeekNeedsOk extends GameOption with ToggleOption {
    val group = "Card Draft"
    val valueOn = "Outrage".hh ~ " waits until the hand is seen"
}

// Mice-in-a-Bush counts as an ambush card, so the attacker can cancel it with a counter-ambush, as the Root rules channel
// confirmed (2026-10-18). Hidden and on by default for new games; older games deal its hit straight away so their replays
// don't change.
case object CounterMiceInABush extends GameOption with ToggleOption {
    val group = "Card Draft"
    val valueOn = "Mice-in-a-Bush".hh ~ " can be counter-ambushed"
}

// Withdrawn: Governors (17.2.3) were taken to stop the Keepers' Delve at an enemy Council's Governing assemblies (2026-10-21),
// but delving moves a relic from the forest to the clearing, it places nothing (owner, 2026-10-21). New games no longer get it; the
// games started with it keep its extra Entreat choice in the Delve, so their recorded moves still load (RetinueDelveAction)
case object GovernorsStopDelve extends GameOption with ToggleOption {
    val group = "Card Draft"
    val valueOn = "Governors".hh ~ " stop the Keepers' Delve"
}

// Lilypad Diaspora's Rally or Reconcile is skipped when neither would do anything: no Militant Enclave to flip, and none to place
// a warrior at (asked for on 2026-10-21). Hidden and on by default for new games; older games keep asking so their
// replays don't change.
case object SkipIdleRallyReconcile extends GameOption with ToggleOption {
    val group = "Card Draft"
    val valueOn = "Rally".hh ~ " or " ~ "Reconcile".hh ~ " skipped when idle"
}

// A Twilight Council move or battle reveals only the card it was revealed with: a Ferry move draws a card, and when that was
// another copy of the revealed card it was revealed too (Season against Land, 2026-10-26), and a battle at an assembly
// discarded such a copy from the hand instead of the revealed card. Hidden and on by default for new games; older games
// keep the old way so their replays don't change (CouncilRevealOnceAction switches a running one over).
case object CouncilRevealOnce extends GameOption with ToggleOption {
    val group = "Card Draft"
    val valueOn = "Twilight Council".hh ~ " reveals one card per action"
}

// An Entreat gives the Twilight Council its Loyalist without asking when it has none it could place in the clearing instead, and
// asks nothing when it can't gain one either (2026-10-29: the Council was asked only to place 0). Hidden and on by default
// for new games; older games keep asking so their replays don't change.
case object EntreatGainUnasked extends GameOption with ToggleOption {
    val group = "Card Draft"
    val valueOn = "Entreat".hh ~ " gains a Loyalist unasked when none could be placed"
}

// Banish is offered only against an enemy with warriors it could move in the clearing, or a Lilypad Diaspora Peaceful enclave
// there, whose fears a battle sets off (2026-10-30: he could still banish a lone Duchy tunnel). Hidden and on by
// default for new games; older games keep offering it so their recorded moves load.
case object BanishNeedsWarriors extends GameOption with ToggleOption {
    val group = "Card Draft"
    val valueOn = "Banish".hh ~ " only against warriors or a Peaceful enclave"
}

// A defender banished by the Twilight Council ignores the first hit as in any other battle (Hundreds' Stubborn, Armored, Devout Knights,
// Badger Bodyguards, Dazzle): the Banish moved the warriors before those effects (2026-11-04, Season for Revolution: the Hundreds
// were Stubborn with the Warlord there). Hidden and on by default for new games; older games keep the old way so their replays don't
// change (BanishAfterIgnoredHitsAction switches a running one over).
case object BanishAfterIgnoredHits extends GameOption with ToggleOption {
    val group = "Card Draft"
    val valueOn = "Banish".hh ~ " after ignored hits"
}

// The Twilight Council's Banish moves the Knaves' Captains only once no other Knave warrior there is left to move, as hits reach a
// Captain only then (Rearguard): it took the warriors in the order they came, so a Captain could go before a Skunk (2026-11-18,
// Control as Struggle: the Captain was banished from Pit to Trench, the Skunk stayed). Hidden, in RulesVersion2; older games keep
// the old way so their replays don't change (BanishCaptainsLastAction switches a running one over).
case object BanishCaptainsLast extends GameOption with ToggleOption {
    val group = "Card Draft"
    val valueOn = "Banish".hh ~ " Captains last"
}

// When the Knaves' Nab battle sets off the Lilypad Diaspora's Fears Come to Pass, the Fears and the Acting Captain's move after the Nab
// have the same timing, and the Knaves (the active player) pick which resolves first (Root rules channel, 2026-11-20): moving first,
// the Fears flip the Peaceful enclaves where the Knaves are after the move. The Fears always came first, during the battle's removal.
// Hidden, in RulesVersion4; older games keep the old way so their replays don't change (NabFearsOrderAction switches a running one over).
case object NabFearsOrder extends GameOption with ToggleOption {
    val group = "Card Draft"
    val valueOn = "Nab".hh ~ " move or " ~ "Fears Come to Pass".hh ~ " first"
}

// The Eyrie's Charismatic leader recruiting with one warrior left in the supply places it, then goes into Turmoil if the card can't
// be done (Law of Root 7.7, 7.8.3, the Leder FAQ: "you must place as many as possible"); it went into Turmoil placing none.
// Hidden, in RulesVersion5; older games keep the old way so their replays don't change (CharismaticPlacesWhatItCanAction switches
// a running one over).
case object CharismaticPlacesWhatItCan extends GameOption with ToggleOption {
    val group = "Card Draft"
    val valueOn = "Charismatic".hh ~ " places what it can"
}

// With MountainSuitedLandmark, the landmark drawn for the Mountain's Tower clearing may be of that clearing's own suit (owner,
// 2026-11-19): older games left those out. Hidden, in RulesVersion3; it only changes a setup draw, so running games need no switch.
case object SuitedLandmarkAnySuit extends GameOption with ToggleOption {
    val group = "Card Draft"
    val valueOn = "Suited landmark".hh ~ " of any suit"
}

// On the Mountain map the Twilight Council may clear a path in Evening, after Convene Woodfolk, if it didn't in Daylight: its cards
// are revealed in Birdsong and come back to hand only in Evening, so its Daylight had none to discard (Discord,
// 2026-11-04). Hidden and on by default for new games; older games keep their Evening so their replays don't change
// (CouncilEveningClearPathAction switches a running one over).
case object CouncilEveningClearPath extends GameOption with ToggleOption {
    val group = "Card Draft"
    val valueOn = "Twilight Council".hh ~ " may clear a path in Evening"
}

// Lilypad Diaspora's Reconcile flips its Militant Enclaves one at a time: each flip draws the clearing's ruler a card before the
// next is picked, so the Diaspora sees what it drew before deciding on the next (2026-11-05: all were picked
// at once). Hidden and on by default for new games; older games keep picking them all at once so their replays don't change.
case object ReconcileOneAtATime extends GameOption with ToggleOption {
    val group = "Card Draft"
    val valueOn = "Reconcile".hh ~ " one enclave at a time"
}

// Lilypad Diaspora's Fears Come to Pass places its warriors once the warriors the battle or removal took are back in the supply,
// and with too few for every flipped clearing the Diaspora picks which get one (2026-11-08: in Movement as Betrayal a battle
// at Effigy flipped Effigy and Lower Jaw, but the two dispersed frogs were still off the map, so only Effigy, listed first, got a
// warrior). Hidden and on by default for new games; older games keep the old way so their replays don't change
// (FearsAfterRemovalAction switches a running one over).
case object FearsAfterRemoval extends GameOption with ToggleOption {
    val group = "Card Draft"
    val valueOn = "Fears Come to Pass".hh ~ " after the removal"
}

// Feather Rufflers is resolved when crafted, as its parchment text box says (like the Favor cards): it places two warriors in
// a clearing with the crafter's pieces and is discarded (2026-10-30: the Knaves crafted it with Apprentice and it stayed
// among their crafted cards). Hidden and on by default for new games; older games keep it as a crafted card used later
// (FeatherRufflersOnCraftAction switches a running one over).
case object FeatherRufflersOnCraft extends GameOption with ToggleOption {
    val group = "Card Draft"
    val valueOn = "Feather Rufflers".hh ~ " resolved when crafted"
}

// Silver-Tongue follows its card: "During any one effect on your turn ... treat a matching clearing with your pieces as one you
// rule". The clearing counts as ruled for the next effect only (the next recorded move and what follows it until someone is asked
// again), not to the end of the phase, and a Negotiations there asks no card for the clearing's other ruler (2026-10-30).
// Hidden and on by default for new games; older games keep the phase-long ruling so their replays don't change
// (SilverTongueOneEffectAction switches a running one over).
case object SilverTongueOneEffect extends GameOption with ToggleOption {
    val group = "Card Draft"
    val valueOn = "Silver-Tongue".hh ~ " lasts for one effect"
}

// Governors (17.2.3): an enemy of the Twilight Council can't craft with its pieces at the Council's Governing assemblies unless it
// entreats first. Lilypad Diaspora's enclaves and the Corvids' plots there still crafted (2026-10-30: the Diaspora crafted with
// its Bayou enclave in Season in Control). Hidden and on by default for new games; older games keep crafting there so their replays
// don't change (GovernorsStopCraftingAction switches a running one over).
case object GovernorsStopCrafting extends GameOption with ToggleOption {
    val group = "Card Draft"
    val valueOn = "Governors".hh ~ " stop crafting"
}

// Mousehold places only the removed warriors that went to the supply: a cat the Marquise's Field Hospitals put at the Keep was offered
// at Mousehold too, so one removed cat came back twice (2026-11-02, Control versus Power). Hidden and on by default for new
// games; older games keep offering it so their replays don't change (MouseholdFromSupplyAction switches a running one over).
case object MouseholdFromSupply extends GameOption with ToggleOption {
    val group = "Card Draft"
    val valueOn = "Mousehold".hh ~ " places only warriors in the supply"
}

// The Riverfolk Company crafts with the crafting icons its placed trade posts uncover on its faction board (Law of Root 8.2), not with
// pieces in a clearing, so the Twilight Council's Governors (17.2.3) don't stop it and it never needs to entreat for it: a trade post
// at a Governing assembly left the Riverfolk one icon short until it entreated (2026-11-07, Destiny and Right).
// Hidden and on by default for new games; older games keep it so their replays don't change (RiverfolkCraftUngovernedAction switches
// a running one over).
case object RiverfolkCraftUngoverned extends GameOption with ToggleOption {
    val group = "Card Draft"
    val valueOn = "Governors".hh ~ " don't stop the Riverfolk crafting"
}

// Governors (17.2.3): an enemy of the Twilight Council can't remove pieces at its Governing assemblies except in battle, so a correct
// guess exposing a Corvid plot there doesn't remove it: the plot stays face down, nothing is scored and the card goes back to the hand
// (a wrong guess still gives it to the Corvids). The exposure removed the plot and scored (Discord,
// 2026-11-10, Destiny and Right; the owner picked guessing with no removal over not offering it). Hidden and on by default for new games;
// older games keep it so their replays don't change (GovernorsStopExposureAction switches a running one over).
case object GovernorsStopExposure extends GameOption with ToggleOption {
    val group = "Card Draft"
    val valueOn = "Governors".hh ~ " stop exposed plots being removed"
}

// The Knaves' Have at Thee! takes prisoners from every enemy warrior they removed, the co-defender's too (the Twilight Council's Bats
// hit when the Knaves battle another faction at a Council assembly; Root rules channel, 2026-11-11): only the defender's were offered,
// so in Betrayal versus Glory the Knaves took no prisoner when the Woodland Alliance gave both hits to Bats. Hidden and on by default
// for new games; older games keep the old way so their replays don't change (PrisonersFromCodefendersAction switches a running one over).
case object PrisonersFromCodefenders extends GameOption with ToggleOption {
    val group = "Card Draft"
    val valueOn = "Knaves".hh ~ " take co-defenders prisoner"
}

// The Dawn and Dusk deck's fixes after a check against its cards (2026-11-13): Fowl Play in the deck, Sly Dog and Old Friend
// asked only when they can be used (and Sly Dog again in every battle of the turn), see root/deck-dusk.scala. Hidden and on by default
// for new games (the Frankendeck draws from the deck too); older games keep the old way so their replays don't change.
// ReplayCheck's "upstartsall": every Upstarts & Renegades effect is dealt out crafted at the first Birdsong, to test them (not offered)
case object UpstartsTestDeal extends GameOption with ToggleOption {
    val group = "Card Draft"
    val valueOn = "Upstarts & Renegades".hh ~ " effects dealt crafted"
}

// ReplayCheck's "sorceryall": every Sorcery of the Enchanted Woods effect but Hailstorm dealt out crafted at the first Birdsong (not offered)
case object SorceryTestDeal extends GameOption with ToggleOption {
    val group = "Card Draft"
    val valueOn = "Sorcery of the Enchanted Woods".hh ~ " effects dealt crafted"
}

// ReplayCheck's "capitalistsall": the Capitalists and Anarchists effects that stay dealt out crafted at the first Birdsong, the ones
// used when crafted and the Services dealt into hands (not offered)
case object CapitalistsTestDeal extends GameOption with ToggleOption {
    val group = "Card Draft"
    val valueOn = "Capitalists and Anarchists".hh ~ " effects dealt"
}

case object DuskDeckFixes extends GameOption with ToggleOption {
    val group = "Card Draft"
    val valueOn = "Dawn and Dusk".hh ~ " deck fixes"
}

// A player buying fewer Riverfolk services than their limit (1, plus 1 for each trade post in a clearing where they have pieces) is
// offered the rest right after, in the same Birdsong: the first purchase used up the whole limit, so the Underground Duchy, which
// could buy 2, bought one card and couldn't buy another (2026-11-03, Ground in Conquest). Hidden and on by default for new
// games; older games keep the single purchase so their replays don't change.
case object BuyServicesAgain extends GameOption with ToggleOption {
    val group = "Card Draft"
    val valueOn = "Riverfolk".hh ~ " services can be bought in more than one purchase"
}

// The Homeland Frog Ambush goes to the bottom of the Pond whenever it is discarded ("Discard this to the Pond bottom."), not only
// when spent as an ambush: discarded at the hand limit it went on top (2026-10-22). Hidden and on by default for new
// games; older games keep putting it on top so their replays don't change.
case object FrogAmbushToPondBottom extends GameOption with ToggleOption {
    val group = "Card Draft"
    val valueOn = "Frog Ambush".hh ~ " always discarded to the Pond bottom"
}

// The Frog Ambush goes to the bottom of the Pond only when played as an ambush (or counter-ambush); any other discard puts it
// on top like every Frog card (the ruling, relayed on 2026-10-23). Hidden and on by default for new games
// in place of FrogAmbushToPondBottom, and it turns that one off; games started with FrogAmbushToPondBottom alone keep it.
case object FrogAmbushBottomOnlyAsAmbush extends GameOption with ToggleOption {
    val group = "Card Draft"
    val valueOn = "Frog Ambush".hh ~ " to the Pond bottom only as an ambush"
}

// Advanced Setup deals the Knaves their four Captain cards when the factions are drawn, so the draft shows them
// (asked for on 2026-10-20). Hidden and on by default for new games; older games deal them when the Knaves
// set up, so their replays don't change.
case object KnavesCaptainsInDraft extends GameOption with ToggleOption {
    val group = "Card Draft"
    val valueOn = "Knaves".hh ~ " Captains shown in the draft"
}

// Hirelings as their cards say (the TTS mod's cards, Law of Root appendix H): the Band blocks only those who moved into
// its clearing that turn, the Forest Patrol saves one warrior of each removal, the Spring Uprising has 4 warriors and the
// Warm Sun Prophets start in every ruin. Hidden and on by default for new games; older games keep the earlier rules so
// their replays don't change.
case object HirelingsByTheCard extends GameOption with ToggleOption with FoldedOption {
    val group = "Hirelings"
    val valueOn = "Hirelings".hh ~ " as their cards say"
}

// Lizard Cult gardens of the Outcast suit pay for their clearing's suits (frog at enclaves, the Lost City's three,
// double suited landmarks), not their printed suit (Homeland Lilypad Diaspora rules). Hidden and on by default for
// new games; older games keep the printed suit so their replays don't change.
case object LizardsCraftByClearing extends GameOption with ToggleOption {
    val group = "Card Draft"
    val valueOn = "Lizard Cult".hh ~ " gardens craft by clearing"
}


// Advanced Setup's insurgent lock (2026-11-04: Destiny and Right drafted four insurgents): the last faction drawn,
// if it is insurgent, can't be picked until a militant faction has been (the draft in SetupNextFactionAction). Adset only;
// games without either option (started before it) draft as before, so their replays don't change.
trait InsurgentLockOption extends GameOption with OneOfGroup {
    val group = "Insurgent Lock"
}

case object InsurgentLockOn extends InsurgentLockOption {
    val valueOn = "Last Insurgent Locked".hh
    override val explain = $(
        &("If the " ~ "last".hl ~ " faction drawn is " ~ "insurgent".hh ~ ",") ~ " " ~ &("nobody can pick it until a " ~ "militant".hh ~ " faction has been picked,") ~ " " ~ &("so the game always has a " ~ "militant".hh ~ " faction, as the rules say."),
    )
}

case object InsurgentLockOff extends InsurgentLockOption {
    val valueOn = "All Insurgents Allowed".hh
    override val explain = $(
        &("Any faction drawn can be picked at any time,") ~ " " ~ &("so a game with " ~ "no militant".hh ~ " faction is possible."),
    )
}

// The setup's "Hits and Ambushes - Offline Players" section (2026-11-12) replaced the Async one:
// four toggles for what the game does by itself while a player is away, or Live Game, where it does none of it and each player's
// own "Hits and Ambushes when Online" settings apply. Every new game gets the hidden HitsAndAmbushesSetup, which tells these games
// apart from older ones (game.offlinePlayers); older games keep their Async options (Forced Async Mode, Players Choose Hits,
// Auto Skip Ambushes, now retired) and play as before.
object HitsAndAmbushes {
    val group = "Hits and Ambushes" ~ " – " ~ "Offline Players"
}

case object HitsAndAmbushesSetup extends GameOption with ToggleOption {
    val group = "Hits and Ambushes"
    val valueOn = "Hits and Ambushes".hh ~ " setup"
}

case object LiveGameMode extends GameOption with ToggleOption with FoldedOption {
    val group = HitsAndAmbushes.group
    val valueOn = "Live Game".hh
    override val explain = $(
        &("Everyone plays " ~ "at the same time".hl ~ ", online.") ~ " " ~ &("The game does " ~ "nothing by itself".hl ~ ":") ~ " " ~ &("the other choices here are off."),
        &("Each player picks for themselves under " ~ "Interface".hh ~ ", " ~ "Hits and Ambushes when Online".hh ~ ":") ~ " " ~ &("whether to be asked about " ~ "hits".styled(styles.hit) ~ " with only one way to take them,") ~ " " ~ &("and about an " ~ "ambush".styled(Bird) ~ " with none to play."),
        &("Those settings only act while that player has the game " ~ "open".hl ~ ";") ~ " " ~ &("their " ~ "Off Turn Actions".hh ~ " switches in the game act " ~ "from any open screen".hl ~ "."),
    )
}

case object BluffMode extends GameOption with ToggleOption with FoldedOption {
    val group = HitsAndAmbushes.group
    val valueOn = "Ambush Bluff Mode".hh
    override def blocked(all : $[BaseOption]) = $($(LiveGameMode))
    override val explain = $(
        &("On".hh ~ ":") ~ " " ~ &("every player who is " ~ "attacked".styled(styles.hit) ~ " is asked whether to " ~ "Ambush".styled(Bird) ~ ",") ~ " " ~ &("even with no " ~ "ambush".styled(Bird) ~ " card,") ~ " " ~ &("so nobody learns who holds one.") ~ " " ~ &("The battle " ~ "waits".hl ~ " for the answer."),
        &("A player is still skipped when that gives nothing away:") ~ " " ~ &("no cards in hand, ") ~ &("Scouting Party".hh ~ ", ") ~ &("a " ~ "Riverfolk".hh ~ " hand (it's open), ") ~ &("or every " ~ "ambush".styled(Bird) ~ " card already seen."),
        &("Off".hh ~ ":") ~ " " ~ &("an attacked player " ~ "with no ambush".hl ~ " card they can play is skipped,") ~ " " ~ &("for " ~ "every".hl ~ " player:") ~ " " ~ &("quicker, but everyone can tell who holds none."),
        &("The attacker's " ~ "counter-ambush".styled(Bird) ~ " works the same way."),
    )
}

case object AutoHitsAssignmentMode extends GameOption with ToggleOption with FoldedOption {
    val group = HitsAndAmbushes.group
    val valueOn = "Auto Assign Hits".hh ~ " (If No Choice)"
    override def blocked(all : $[BaseOption]) = $($(LiveGameMode), $(OffTurnHitsPreset))
    override val explain = $(
        &("On".hh ~ ":") ~ " " ~ &("when there is only one way to take a battle's " ~ "hits".styled(styles.hit) ~ ",") ~ " " ~ &("the game takes them " ~ "without asking".hl ~ "."),
        &("Off".hh ~ ":") ~ " " ~ &("every player assigns " ~ "every".hl ~ " hit by hand, and the battle waits for them."),
        &("With " ~ "Off Turn Hits Preset".hh ~ " the preset order decides instead.")
    )
}

case object OffTurnHitsPreset extends GameOption with ToggleOption with FoldedOption {
    val group = HitsAndAmbushes.group
    val valueOn = "Off Turn Hits Preset".hh
    override def blocked(all : $[BaseOption]) = $($(LiveGameMode))
    override val explain = $(
        &("Hits".styled(styles.hit) ~ " are " ~ "never asked".hl ~ " for:") ~ " " ~ &("they are taken in an order each player sets beforehand."),
        &("At the end of their turn (" ~ "Adjust priorities".hh ~ "),") ~ " " ~ &("a player with more than one kind of " ~ "building".hh ~ " or " ~ "token".hh ~ " in a clearing") ~ " " ~ &("sets which goes first,") ~ " " ~ &("and " ~ VB.elem ~ " sets the order of their items."),
        &("Auto Assign Hits".hh ~ " is greyed out with it, as the order covers every hit."),
    )
}

case object OffTurnFieldHospitals extends GameOption with ToggleOption with FoldedOption {
    val group = HitsAndAmbushes.group
    val valueOn = "Off Turn".hh ~ " " ~ "Field Hospitals".styled(MC)
    override def blocked(all : $[BaseOption]) = $($(LiveGameMode))
    override val explain = $(
        &("At the end of each of its turns (" ~ "Adjust Hospital Plan".hh ~ "),") ~ " " ~ &(MC.elem ~ " marks each " ~ "card".hh ~ " in hand as used or skipped for " ~ "Field Hospitals".styled(MC) ~ ","),
        &("so a battle on another player's turn " ~ "doesn't wait".hl ~ " for " ~ MC.elem ~ " to decide."),
    )
}

// Retired (2026-11-12): Forced Async Mode was the four choices above at once, and Players Choose Hits took its preset order away.
// Still read, so the games that have them load and play as before
case object AutoAmbushSkippingMode extends GameOption with ToggleOption {
    val group = "Async"
    val valueOn = "Auto Skip Ambushes".hh
}

case object ForcedAsyncMode extends GameOption with ToggleOption {
    val group = "Async"
    val valueOn = "Forced Async Mode".styled(xstyles.warning)
    override def decorate(elem : Elem) = Image("swap")(styles.token) ~ " " ~ elem ~ " " ~ Image("swap")(styles.token)
}

// Forced Async Mode without the preset damage order (Season in Control, 2026-10-28). Games started without it can be switched over
// with ForcedAsyncChooseHitsAction.
case object ForcedAsyncChooseHits extends GameOption with ToggleOption {
    val group = "Async"
    val valueOn = "Players Choose Hits".hh
    override def required(all : $[BaseOption]) = $($(ForcedAsyncMode))
}

trait AdSetBuffOption extends GameOption with OneOfGroup with FoldedOption {
    val group = "AdSet " ~ "MC".styled(MC) ~ ", " ~ "LC".styled(LC) ~ ", " ~ "CC".styled(CC) ~ " Setup Buffs"
}

object AdSetBuffOption {
    val explain : $[Elem] = $(
        "The setup bonuses of the " ~ "Advanced Setup".hh ~ " rules (Law of Root), always played in " ~ "Root: Advanced Setup".hh ~ ":",
        MC.elem ~ " places a " ~ "warrior".hh ~ " with each starting " ~ "building".hh ~ ".",
        LC.elem ~ " starts with " ~ 2.hl ~ " " ~ "Acolytes".hh ~ ".",
        CC.elem ~ " places a " ~ "plot".hh ~ " at setup.",
        "The standard rules don't have them, so they are " ~ "Off".hh ~ " by default here.",
    )
}

case object AdSetBuffOn extends AdSetBuffOption {
    val valueOn = "On".hh
    override val explain = AdSetBuffOption.explain
}

case object AdSetBuffOff extends AdSetBuffOption {
    val valueOn = "Off".hh
    override val explain = AdSetBuffOption.explain
}

trait MirrorOption extends GameOption with ToggleOption with FoldedOption {
    val group = "Mirror Factions"
}

case object TheWoodMustFlow extends MirrorOption {
    val valueOn = "The Wood Must Flow".hh

    override val explain = $(
        "Cats".styled(MC) ~ " capture other " ~ "Cats'".styled(BK) ~ " " ~ "buildings".hh ~ ", " ~ "wood".hh ~ " and " ~ "keep".hh ~ " instead of destroying them, " ~ &("if they have the same available in their supply."),
        "They don't score points capturing.",
    )
}

case object OneTrueEmpire extends MirrorOption {
    val valueOn = "One True Empire".hh

    override val explain = $(
        "When " ~ "Birds".styled(ED) ~ " " ~ "turmoil".hh ~ ", the " ~ "bird".styled(Bird) ~ " cards from the " ~ "Decree".hh ~ " are not discarded, they are added to other " ~ "Birds'".styled(PE) ~ " " ~ "Decree".hh ~ " in the same columns.",
        "With each " ~ "turmoil".hh ~ " " ~ "Birds".styled(ED) ~ " increase their turmoil " ~ "tolerance".hh ~ " that allows them to skip one more unfulfillable order before turmoiling."
    )
}

case object Imprinting extends MirrorOption {
    val valueOn = "Imprinting".hh

    override val explain = $(
        "Enemy " ~ "Sympathy".styled(WA) ~ " " ~ "increases the cost of spreading " ~ "Sympathy".styled(FU) ~ " by " ~ "one".hh ~ ".",
        "Stacks with the " ~ "Martial Law".hh ~ "."
    )
}

case object RootingForTheUnderdog extends MirrorOption {
    val valueOn = "Rooting for the Underdog".hh

    override val explain = $(
        "Vagabond".styled(VB) ~ " refreshes " ~ "one more".hh ~ " item in the " ~ Birdsong.elem ~ " " ~ &("if they have less " ~ "VP".styled(styles.vp) ~ " than other " ~ "Vagabonds".styled(NB) ~ "."),
        "Vagabond".styled(VB) ~ " refreshes " ~ "one less".hh ~ " item in the " ~ Birdsong.elem ~ " " ~ &("if they have more " ~ "VP".styled(styles.vp) ~ " than other " ~ "Vagabonds".styled(NB) ~ ".")
    )
}

case object DeathToTheInfidels extends MirrorOption {
    val valueOn = "Death to the Infidels".hh

    override val explain = $(
        "Lizards".styled(LC) ~ " " ~ "deal an extra " ~ "hit".styled(styles.hit) ~ " attacking other " ~ "Lizards".styled(CM) ~ ".",
        "Discarded " ~ "cards".hh ~ " go to the " ~ "Lost Souls".hh ~ " " ~ "of the player" ~ " next in the turn order."
    )
}

case object HostileTakeover extends MirrorOption {
    val valueOn = "Hostile Takeover".hh
}

case object DoubleAgents extends MirrorOption {
    val valueOn = "Double Agents".hh

    override val explain = $(
        "Hidden " ~ "Plots".styled(CC) ~ " " ~ "deal double extra " ~ "hits".styled(styles.hit) ~ " to other " ~ "Crows".styled(RI) ~ "."
    )
}

case object OldBoys extends MirrorOption {
    val valueOn = "Old Boys".hh

    override val explain = $(
        "A " ~ "Minister".hh ~ " " ~ "can only be swayed by one " ~ "Underground".styled(UD) ~ " faction at a time.",
        "Swaying a " ~ "Minister".hh ~ " of another " ~ "Underground".styled(DR) ~ " faction costs one more " ~ "card".hh ~ ", " ~ "and doesn't provide victory points.",
        "Losing a " ~ "Minister".hh ~ " to enemy " ~ "swaying".hh ~ " has no negative consequences and leaves the " ~ "crown".hh ~ " available."
    )
}

case object PeerPressure extends MirrorOption {
    val valueOn = "Peer Pressure".hh

    override val explain = $(
        "Mob".styled(LH) ~ " immediately " ~ "converts".hh ~ " enemy " ~ "Rats".styled(LK) ~ " to their own if they are without " ~ "Warlord".styled(LK) ~ " or " ~ "Stronghold".styled(LK) ~ ".",
        "If " ~ "Mob".styled(LH) ~ " is placed in a clearing with another "  ~ "Mob".styled(LK) ~  "," ~ " both disperse, nobody scores anything."
    )
}

case object SharedHistory extends MirrorOption {
    val valueOn = "Shared History".hh
}






case object UntargetableKeep extends GameOption with ToggleOption with FoldedOption {
    val group = MC.title
    val valueOn = "Keep".styled(MC) ~ SpacedDash ~ "Untargetable".hh
    override def required(all : $[BaseOption]) = $($(IncludeFaction(MC)), $(IncludeFaction(BK)))
    override def blocked(all : $[BaseOption]) = $($(IndestructibleKeep), $(ThreeHitKeep), $(InstantRebuildKeep))
    override val explain = $(
        "The " ~ Keep.of(MC) ~ " doesn't count as token," ~ HorizontalBreak ~ "it can't be removed from the map," ~ HorizontalBreak ~ " and it can't be assigned hits."
    )
}

case object IndestructibleKeep extends GameOption with ToggleOption with FoldedOption {
    val group = MC.title
    val valueOn = "Keep".styled(MC) ~ SpacedDash ~ "Indestructible".hh
    override def required(all : $[BaseOption]) = $($(IncludeFaction(MC)), $(IncludeFaction(BK)))
    override def blocked(all : $[BaseOption]) = $($(UntargetableKeep), $(ThreeHitKeep), $(InstantRebuildKeep))
    override val explain = $(
        "The " ~ Keep.of(MC) ~ " can't be removed from the map," ~ HorizontalBreak ~ " although a " ~ "victory point".styled(styles.vp) ~ " is scored" ~ HorizontalBreak ~ "each time it would be removed."
    )
}

case object ThreeHitKeep extends GameOption with ToggleOption with FoldedOption {
    val group = MC.title
    val valueOn = "Keep".styled(MC) ~ SpacedDash ~ "Takes ".hh ~ 3.hit
    override def required(all : $[BaseOption]) = $($(IncludeFaction(MC)), $(IncludeFaction(BK)))
    override def blocked(all : $[BaseOption]) = $($(UntargetableKeep), $(IndestructibleKeep))
    override val explain = $(
        "The first "~ "two".hh ~ " times the " ~ Keep.of(MC) ~ " is " ~ "removed".styled(styles.hit) ~ " it " ~ "isn't".hl ~ "," ~ HorizontalBreak ~ "although a " ~ "victory point".styled(styles.vp) ~ " is scored" ~ HorizontalBreak ~ "each time it would be removed."
    )
}

case object InstantRebuildKeep extends GameOption with ToggleOption with FoldedOption {
    val group = MC.title
    val valueOn = "Keep".styled(MC) ~ SpacedDash ~ "Instant Rebuild".hh
    override def required(all : $[BaseOption]) = $($(IncludeFaction(MC)), $(IncludeFaction(BK)))
    override def blocked(all : $[BaseOption]) = $($(UntargetableKeep), $(IndestructibleKeep))
    override val explain = $(
        "After the " ~ Keep.of(MC) ~ " is removed," ~ HorizontalBreak ~ &(&(MC.elem ~ " can immediately rebuild it ") ~ &("in another clearing she rules,") ~ " " ~ &("discarding a " ~ "matching card".hh ~ "."))
    )
}

case object SawmillMassOverwork extends GameOption with ToggleOption with FoldedOption {
    val group = MC.title
    val valueOn = Sawmill.sof(MC) ~ SpacedDash ~ "Mass Overwork".hh
    override def required(all : $[BaseOption]) = $($(IncludeFaction(MC)), $(IncludeFaction(BK)))
    override val explain = $(
        "After an " ~ "Overwork".hh ~ ", " ~ MC.elem ~ " may " ~ "Overwork".hh ~ " again in another clearing, " ~ "as long as there is " ~ Wood.of(MC) ~ " left in supply.",
    )
}

case object WorkshopEveningCraft extends GameOption with ToggleOption with FoldedOption {
    val group = MC.title
    val valueOn = Workshop.sof(MC) ~ SpacedDash ~ "Evening".styled(styles.phase) ~ " Craft".hh
    override def required(all : $[BaseOption]) = $($(IncludeFaction(MC)), $(IncludeFaction(BK)))
    override val explain = $(
        MC.elem ~ " crafts with " ~ Workshop.sof(MC) ~ " in the " ~ "Evening".styled(styles.phase) ~ " instead of in " ~ "Daylight".styled(styles.phase) ~ ".",
    )
}

case object WorkshopActionBonuses extends GameOption with ToggleOption with FoldedOption {
    val group = MC.title
    val valueOn = Workshop.sof(MC) ~ SpacedDash ~ "Action Bonuses".hh
    override def required(all : $[BaseOption]) = $($(IncludeFaction(MC)), $(IncludeFaction(BK)))
    override val explain = $(
        "Each " ~ Workshop.of(MC) ~ " after the first gives a lasting bonus: " ~ "an extra hit in battle, an extra battle, an extra card, an extra move, another battle, another move.",
        Workshop.sof(MC) ~ " score fewer " ~ "victory points".styled(styles.vp) ~ " to make up for it.",
    )
}

case object KeepExtraBuildingSlot extends GameOption with ToggleOption with FoldedOption {
    val group = MC.title
    val valueOn = "Keep".styled(MC) ~ SpacedDash ~ "Extra Building Slot".hh
    override def required(all : $[BaseOption]) = $($(IncludeFaction(MC)), $(IncludeFaction(BK)))
    override val explain = $(
        "The clearing with the " ~ Keep.of(MC) ~ " has an extra building slot as long as it stands."
    )
}

case object Catapults extends GameOption with ToggleOption with FoldedOption {
    val group = MC.title
    val valueOn = "Catapults".styled(MC) ~ SpacedDash ~ "Siege Tokens".hh
    override def required(all : $[BaseOption]) = $($(IncludeFaction(MC)), $(IncludeFaction(BK)))
    override val explain = $(
        "Spend " ~ 1.hl ~ " " ~ Wood.of(MC) ~ " to place a catapult token at a " ~ Workshop.of(MC) ~ ".",
        "When moving, can move " ~ 1.hl ~ " " ~ Catapult.of(MC) ~ " for every " ~ 4.hl ~ " " ~ Cat.sof(MC) ~ " that move.",
        "At the start of battle as the attacker, " ~ MC.elem ~ " may declare that they want to siege the defender.",
        "In that case, the number of rolled hit is limited by the number of " ~ Catapult.sof(MC) ~ "," ~ " and rolled hits can only be assigned to " ~ "tokens".hh ~ " and " ~ "buildings".hh ~ "."
    )
}


case object WarriorsOnlyInfamy extends GameOption with ToggleOption with FoldedOption {
    val group = VB.title
    val valueOn = "Infamy".styled(VB) ~ SpacedDash ~ "Warriors Only".hh
    override def required(all : $[BaseOption]) = $($(IncludeFaction(VB)), $(IncludeFaction(NB)), $(IncludeFaction(MB)))
    override val explain = $(
        VB.elem ~ " scores " ~ "Infamy".styled(VB) ~ " only for killing " ~ "warriors".hh ~ "."
    )
}

case object LimitedInfamy extends GameOption with ToggleOption with FoldedOption {
    val group = VB.title
    val valueOn = "Infamy".styled(VB) ~ SpacedDash ~ "Max".hh ~ " " ~ (1 + " VP").styled(styles.vp) ~ " per Battle".hh
    override def required(all : $[BaseOption]) = $($(IncludeFaction(VB)), $(IncludeFaction(NB)), $(IncludeFaction(MB)))
    override val explain = $(
        VB.elem ~ " scores " ~ "Infamy".styled(VB) ~ " only once per " ~ "enemy faction".hh ~ " per " ~ "battle".hh ~ "."
    )
}

case object RevisedQuests extends GameOption with ToggleOption with FoldedOption {
    val group = VB.title
    val valueOn = "Quests".styled(VB) ~ SpacedDash ~ "Revised Quest Deck".hh
    override def required(all : $[BaseOption]) = $($(IncludeFaction(VB)), $(IncludeFaction(NB)), $(IncludeFaction(MB)))
    override val explain = $(
        VB.elem ~ " uses the " ~ Link("https://boardgamegeek.com/thread/2810553/revised-quest-deck", "Revised Quest Deck", $(xstyles.link)) ~ " by " ~ "Phoenix1147".hl ~ ".",
        "Six of the original quests have their item requirements changed to make questing more viable.",
        Image("quest:" + "errand-fox", styles.quest) ~ Image("quest:" + "errand-rabbit", styles.quest) ~ Image("quest:" + "escort-mouse", styles.quest) ~ Break ~
        Image("quest:" + "repair-a-shed-rabbit", styles.quest) ~ Image("quest:" + "fundraising-fox", styles.quest) ~ Image("quest:" + "escort-fox", styles.quest),
    )
    override val grow = xlo.grow4
}


trait VagabondCharOption {
    def charImage(character : Character, options : $[GameOption]) =
        convertDesc($(
            Image(character.title)(styles.chartitle),
            Break,
            Image(character.img)(styles.quest)((character == Ranger).?(styles.ranger))((character == Adventurer).?(styles.adventurer))((character == FolkHero).?(styles.folk)).div(styles.charblock) ~ character.starting(options)./(_.img).merge,
            Break,
            character.special./(_.of(VB)
        )))(null, null)(null).div(xstyles.xx)(xstyles.chp)(xstyles.chm)(xstyles.info)(styles.inline)(styles.charback)
}

case object TinkerNoBag extends GameOption with ToggleOption with VagabondCharOption with FoldedOption {
    val group = VB.title
    val valueOn = Tinker.name.styled(VB) ~ SpacedDash ~ "No Starting ".hh ~ Bag.name.spn(styles.itemInfo)
    override def required(all : $[BaseOption]) = $($(IncludeFaction(VB)), $(IncludeFaction(NB)), $(IncludeFaction(MB)))
    override val explain = $(
        Tinker.name.styled(VB) ~ " starts without a " ~ Bag.elem ~ ".",
        charImage(Tinker, $(this))
    )
    override val grow = xlo.grow16
}

case object HarrierNoCoins extends GameOption with ToggleOption with VagabondCharOption with FoldedOption {
    val group = VB.title
    val valueOn = Harrier.name.styled(VB) ~ SpacedDash ~ "No Starting ".hh ~ Coins.name.spn(styles.itemInfo)
    override def required(all : $[BaseOption]) = $($(IncludeFaction(VB)), $(IncludeFaction(NB)), $(IncludeFaction(MB)))
    override val explain = $(
        Harrier.name.styled(VB) ~ " starts without " ~ Coins.elem ~ ".",
        charImage(Harrier, $(this))
    )
}

case object ScoundrelTeapotNoBoot extends GameOption with ToggleOption with VagabondCharOption with FoldedOption {
    val group = VB.title
    val valueOn = Scoundrel.name.styled(VB) ~ SpacedDash ~ Teapot.name.spn(styles.itemInfo) ~ " instead of ".hh ~ Boots.name.spn(styles.itemInfo)
    override def required(all : $[BaseOption]) = $($(IncludeFaction(VB)), $(IncludeFaction(NB)), $(IncludeFaction(MB)))
    override val explain = $(
        Scoundrel.name.styled(VB) ~ " starts with " ~ Teapot.elem ~ " instead of " ~ Boots.elem ~ ".",
        charImage(Scoundrel, $(this))
    )
}

case object ArbiterScoreCard extends GameOption with ToggleOption with VagabondCharOption with FoldedOption {
    val group = VB.title
    val valueOn = Arbiter.name.styled(VB) ~ SpacedDash ~ Protector.of(VB) ~ " grants ".hh ~ "1 Card".styled(Bird)
    override def required(all : $[BaseOption]) = $($(IncludeFaction(VB)), $(IncludeFaction(NB)), $(IncludeFaction(MB)))
    override val explain = $(
        Arbiter.name.styled(VB) ~ " doesn't get a " ~ "victory point".styled(styles.vp) ~ " from his " ~ Protector.of(VB) ~ " abilitiy, draws a " ~ "card".hh ~ " instead.",
        charImage(Arbiter, $(this))
    )
}

case object RoninSwiftTorch extends GameOption with ToggleOption with VagabondCharOption with FoldedOption {
    val group = VB.title
    val valueOn = Ronin.name.styled(VB) ~ SpacedDash ~ SwiftStrike.of(VB) ~ " with ".hh ~ Torch.name.spn(styles.itemInfo)
    override def required(all : $[BaseOption]) = $($(IncludeFaction(VB)), $(IncludeFaction(NB)), $(IncludeFaction(MB)))
    override val explain = $(
        Ronin.name.styled(VB) ~ " uses " ~ Torch.elem ~ " instead of the " ~ Sword.elem ~ " to trigger " ~ SwiftStrike.of(VB) ~ ".",
        charImage(Ronin, $(this))
    )
}

case object FolkHeroCharacter extends GameOption with ToggleOption with VagabondCharOption with FoldedOption {
    val group = VB.title
    val valueOn = FolkHero.name.styled(VB) ~ " " ~ "character"
    override def required(all : $[BaseOption]) = $($(IncludeFaction(VB)), $(IncludeFaction(NB)), $(IncludeFaction(MB)))
    override val explain = $(
        FolkHero.name.styled(VB) ~ SpacedDash ~ "character drawn by " ~ "Kyle Ferrin".hl ~ " and designed by " ~ "S.P. Shaman".hl ~ " in the loving memory of " ~ "t1mothy".styled(styles.get(MC))(xstyles.bold)(xstyles.larger125) ~ ".",
        "When aiding a " ~ "hostile".styled(styles.hit) ~ " faction," ~ " " ~ &("repairs".hh ~ " an item and changes their attitude to " ~ "indifferent".hh ~ "."),
        charImage(FolkHero, $(this))
    )
}


// the Doomed Expansion's seven Vagabond characters (fan-made by slugfacekillah; Law of Doom D7; `root/doomed-vagabonds.scala`)
case object DoomedVagabonds extends GameOption with ToggleOption with VagabondCharOption with FoldedOption {
    val group = VB.title
    val valueOn = "Doomed Vagabonds".styled(VB) ~ " " ~ "(fan-made)"
    override def required(all : $[BaseOption]) = $($(IncludeFaction(VB)), $(IncludeFaction(NB)), $(IncludeFaction(MB)))
    override val explain = $(
        "Doomed Vagabonds".styled(VB) ~ SpacedDash ~ "seven more characters from " ~ "The Doomed Expansion".hl ~ " by " ~ "slugfacekillah".hl ~ ": " ~
        DoomedCharacters.characters./(_.name.styled(VB)).comma ~ ".",
        FanInfo.doomedLink
    ) ++ DoomedCharacters.characters./(c => charImage(c, $(this)))
}


// the Doomed Expansion's six Knave Captains (fan-made by slugfacekillah; Law of Doom D6; `Knaves.doomed` in `root/faction-knaves.scala`)
case object DoomedCaptains extends GameOption with ToggleOption with FoldedOption {
    val group = KD.title
    val valueOn = "Doomed Knave Captains".styled(KD) ~ " " ~ "(fan-made)"
    override def required(all : $[BaseOption]) = $($(IncludeFaction(KD)))
    override val explain = $(
        "Doomed Knave Captains".styled(KD) ~ SpacedDash ~ "six more Captain cards from " ~ "The Doomed Expansion".hl ~ " by " ~ "slugfacekillah".hl ~ ", dealt with the others.",
        FanInfo.doomedLink
    ) ++ Knaves.doomed./(k => k.name.styled(KD).styled(xstyles.bold) ~ " " ~ k.items./(_.img).merge ~ SpacedDash ~ k.text)
}


case object LimitedTrampleDiscard extends GameOption with ToggleOption with FoldedOption {
    val group = LC.title
    val valueOn = "Trampled ".hh ~ "Gardens".styled(LC) ~ SpacedDash ~ "Discard Only ".hh ~ 1.styled(styles.hit) ~ " Card".hh
    override def required(all : $[BaseOption]) = $($(IncludeFaction(LC)))
    override val explain = $(
        "If several ".hh ~ "Gardens".styled(LC) ~ " are removed at once, " ~ LC.elem ~ " discards only " ~ 1.hl ~ " card."
    )
}

case object RevengeFromAnyRemoval extends GameOption with ToggleOption with FoldedOption {
    val group = LC.title
    val valueOn = "Revenge".styled(LC) ~ SpacedDash ~ "Warriors Removed Out of Turn".hh
    override def required(all : $[BaseOption]) = $($(IncludeFaction(LC)))
    override val explain = $(
        "All " ~ "Lizards".styled(LC) ~ " removed out of turn become " ~ "Acolytes".hl ~ "," ~ HorizontalBreak ~ "not only those killed defending in battle."
    )
}

case object LizardHandSizeSix extends GameOption with ToggleOption with FoldedOption {
    val group = LC.title
    val valueOn = "Hand".styled(LC) ~ SpacedDash ~ "Hand Size ".hh ~ "Six".styled(Bird) ~ " Cards".hh
    override def required(all : $[BaseOption]) = $($(IncludeFaction(LC)))
    override val explain = $(
        LC.elem ~ " discards down to " ~ 6.styled(Bird) ~ " cards in the evening," ~ HorizontalBreak ~ "not to " ~ 5.styled(Bird) ~ " as all factions."
    )
}

case object ExportToCommitments extends GameOption with ToggleOption {
    val group = RF.title
    val valueOn = "Export".styled(RF) ~ SpacedDash ~ "Add to Commitments".hh
    override def required(all : $[BaseOption]) = $($(IncludeFaction(RF)))
    override val explain = $(
        "Exported " ~ "funds".styled(RF) ~ " go straight to " ~ "Commitments".styled(RF) ~ ", not to " ~ "Payments".styled(RF) ~ "."
    )
}

case object MayorAssemblyAction extends GameOption with ToggleOption {
    val group = UD.title
    val valueOn = "Mayor".styled(UD) ~ SpacedDash ~ "Performs ".hh ~ "Assembly".styled(UD) ~ " Action".hh
    override def required(all : $[BaseOption]) = $($(IncludeFaction(UD)))
    override val explain = $(
        "Mayor".styled(UD) ~ " " ~ "digs".hh ~ ", " ~ "builds".hh ~ ", " ~ "moves".hh ~ ", " ~ "battles".hh ~ " or " ~ "recruits".hh ~ Break ~ "instead of copying other ministers."
    )
}

case object ThreeOfEachPlot extends GameOption with ToggleOption with FoldedOption {
    val group = CC.title
    val valueOn = "More Plots".styled(CC) ~ SpacedDash ~ "Three of each Plot".hh
    override def required(all : $[BaseOption]) = $($(IncludeFaction(CC)), $(IncludeFaction(RI)))
    override val explain = $(
        CC.elem ~ " get " ~ "three".hh ~ " instances of each plot in supply," ~ HorizontalBreak ~ "not " ~ "two".hh ~ " as default."
    )
}

case object EmbeddedAgentsBeforeBattle extends GameOption with ToggleOption with FoldedOption {
    val group = CC.title
    val valueOn = "Embedded Agents".styled(CC) ~ SpacedDash ~ 1.hit ~ " before Battle".hh
    override def required(all : $[BaseOption]) = $($(IncludeFaction(CC)), $(IncludeFaction(RI)))
    override val explain = $(
        "Hidden plots deal a " ~ 1.hit ~ " before the roll in the battle," ~ HorizontalBreak ~ "like a half-" ~ "ambush".hh ~ " that can't be countered.",
        "Plots don't deal an extra " ~ 1.hit ~ " after the roll."
    )
}

case object DiversionPlot extends GameOption with ToggleOption with FoldedOption {
    val group = CC.title
    val valueOn = "Diversion".styled(CC) ~ SpacedDash ~ "New Plot Type".hh
    override def required(all : $[BaseOption]) = $($(IncludeFaction(CC)), $(IncludeFaction(RI)))
    override val explain = $(
        "Diversion".styled(CC) ~ SpacedDash ~ "a new type of plot available to " ~ CC.elem ~ ".",
        "This plot can't ever be " ~ "exposed".hh ~ "," ~ HorizontalBreak ~ "exposing always fails as if the plot was guessed wrong.",
        "But it can never be " ~ "flipped".hh ~ " and scored either."
    )
}

case object BrutalHonesty extends GameOption with ToggleOption with FoldedOption {
    val group = CC.title
    val valueOn = "Brutal Honesty".hh ~ SpacedDash ~ "Better " ~ "Bombs".styled(CC)
    override def required(all : $[BaseOption]) = $($(IncludeFaction(CC)), $(IncludeFaction(RI)))
    override val explain = $(
        "Bombs".styled(CC) ~ " can be plotted revealed.",
        ("In battle,").& ~ " " ~ (CC.elem ~ " can remove").& ~ " " ~ ("a hidden or revealed " ~ "Bomb".styled(CC)).& ~ " " ~ ("in lieu of playing an " ~ "Ambush".styled(Bird) ~ ",").& ~ " " ~ "both offensively and defensively."
    )
}

case object AssassinPlot extends GameOption with ToggleOption with FoldedOption {
    val group = CC.title
    val valueOn = "New Plot Type".hh ~ SpacedDash ~ "Assassin".styled(CC)
    override def required(all : $[BaseOption]) = $($(IncludeFaction(CC)), $(IncludeFaction(RI)))
    override val explain = $(
        "Assassin".styled(CC) ~ SpacedDash ~ "a new type of plot available to " ~ CC.elem,
        "On flip, either " ~ "place".hh ~ " a warrior in a matching clearing, or " ~ "remove".hh ~ " an enemy warrior in a matching clearing",
        "Afterwards, remove the plot token",
    )
}

case object ColumnBonusCardVP extends GameOption with ToggleOption {
    val group = KI.title
    val valueOn = "Completed Column".styled(KI) ~ SpacedDash ~ (1 + " VP").styled(styles.vp) ~ " and ".hh ~ "1 Card".styled(Bird)
    override def required(all : $[BaseOption]) = $($(IncludeFaction(KI)))
    override val explain = $(
        "Instead of " ~ &("scoring " ~ 2.vp) ~ " for completing" ~ &(" a set of " ~ "Relics".hh ~ ",") ~ &("score " ~ 1.vp ~ " and " ~ &("draw " ~ "1 Card".styled(Bird) ~ "."))
    )
}

case object SeparateItemsInRuins extends GameOption with ToggleOption {
    val group = LH.title
    val valueOn = "Items in Ruins".styled(LH) ~ SpacedDash ~ "Own Set".hh
    override def required(all : $[BaseOption]) = $($(IncludeFaction(VB)), $(IncludeFaction(NB)), $(IncludeFaction(MB)))./(_ ++ $(IncludeFaction(LH)))
    override val explain = $(
        "In games with " ~ LH.elem ~ " and " ~ VB.elem ~ " there are items in ruins for " ~ "both".hh ~ " factions."
    )
}

case object RandomCharacter extends GameOption with ToggleOption with FoldedOption {
    val group = VB.title
    val valueOn = "Random Character".hh
    override def required(all : $[BaseOption]) = $($(IncludeFaction(VB)), $(IncludeFaction(NB)), $(IncludeFaction(MB)))
    override val explain = $(
        VB.elem ~ " gets a character drawn at random instead of choosing one.",
    )
}


case object TotalWarDominance extends GameOption with ToggleOption {
    val group = "Dominance"
    val valueOn = "Total War".styled(styles.hit)
    override def decorate(elem : Elem) = Image("battle")(styles.token) ~ " " ~ elem ~ " " ~ Image("battle")(styles.token)
    override val explain = $(
        &("Any " ~ "faction".hh ~ " with activated " ~ "Dominance".hl) ~ &(" continues" ~ " to score " ~ &("victory points".styled(styles.vp) ~ ", ")) ~ &("only they" ~ " are immediately used."),
        &(&("After " ~ "any action".hh ~ " is complete") ~ " " ~ "during which that faction scored points,") ~ " " ~ &("even not on their turn,") ~ " " ~ &(&("that faction places a " ~ "warrior".hl) ~ " " ~  &("for each " ~ "point".styled(styles.vp)) ~ " " ~ &("anywhere with their pieces.")),
        &("Each time that faction " ~ "doesn't have " ~ &("a warrior in supply, ")) ~ &("it " ~ "battles".hl ~ " anywhere instead."),
        &("And when the faction " ~ &("recruits warriors " ~ &("through other means, "))) ~ &("but the supply runs out, ") ~ &(&("it scores " ~ 1.vp) ~ &(" for each warrior not recruited."))
    )
}


trait LandmarksOption extends GameOption with FoldedOption {
    val group = "Landmarks"
}

trait Landmark extends LandmarksOption with ToggleOption with Record with Elementary {
    val name : String
    def valueOn = name.hh
    def elem = name.hl
}

case object MapDefaultLandmarks extends LandmarksOption {
    val valueOn = "Map Default".hh
    override def forcedOff(all : $[BaseOption]) = all.of[LandmarksOption].but(this).%(_.group == this.group)
}

// Map Default, but on the Mountain map a suited landmark drawn at setup goes where the Tower would (SuitedLandmarkAtTowerAction)
case object MountainSuitedLandmark extends LandmarksOption {
    val valueOn = "Map Default".hh ~ ", or on " ~ "Mountain".hh ~ " a random " ~ "suited".hh ~ " landmark"
    override def forcedOff(all : $[BaseOption]) = all.of[LandmarksOption].but(this).%(_.group == this.group)
    override val explain = $(
        "On the " ~ "Mountain".hh ~ " map, instead of its " ~ "Tower".hh ~ ", one of " ~ "Lost City".hl ~ ", " ~ "Mousehold".styled(Mouse) ~ ", " ~ "Foxburrow".styled(Fox) ~ " and " ~ "Rabbittown".styled(Rabbit) ~ " is drawn at setup and placed in the " ~ "Tower".hh ~ "'s clearing.",
        "The landmark may be of that clearing's own suit, which then keeps its one suit; otherwise it gets a second suit (or all three with the " ~ "Lost City".hl ~ ").",
        "On any other map, the map's own landmarks."
    )
}

case object NoLandmarks extends LandmarksOption {
    val valueOn = "None".hh
    override def forcedOff(all : $[BaseOption]) = all.of[LandmarksOption].but(this).%(_.group == this.group)
}

case object FerryLandmark extends Landmark {
    val name = "Ferry"
    override def forcedOff(all : $[BaseOption]) = $(MapDefaultLandmarks, MountainSuitedLandmark, NoLandmarks)
}

case object LostCityLandmark extends Landmark {
    val name = "Lost City"
    override def forcedOff(all : $[BaseOption]) = $(MapDefaultLandmarks, MountainSuitedLandmark, NoLandmarks)
}

case object TowerLandmark extends Landmark {
    val name = "Tower"
    override def forcedOff(all : $[BaseOption]) = $(MapDefaultLandmarks, MountainSuitedLandmark, NoLandmarks)
}

case object MouseholdLandmark extends Landmark {
    val name = "Mousehold"
    override def elem = name.styled(Mouse)
    override def forcedOff(all : $[BaseOption]) = $(MapDefaultLandmarks, MountainSuitedLandmark, NoLandmarks)
    override val explain = $(
        "Homeland".hh ~ " landmark, placed in a " ~ "mouse".styled(Mouse) ~ " clearing.",
        "Adds the " ~ "mouse".styled(Mouse) ~ " suit to its clearing.",
        "In battle, when warriors are removed from a different " ~ "mouse".styled(Mouse) ~ " clearing, their player may place them at " ~ "Mousehold".styled(Mouse) ~ "."
    )
}

case object FoxburrowLandmark extends Landmark {
    val name = "Foxburrow"
    override def elem = name.styled(Fox)
    override def forcedOff(all : $[BaseOption]) = $(MapDefaultLandmarks, MountainSuitedLandmark, NoLandmarks)
    override val explain = $(
        "Homeland".hh ~ " landmark, placed in a " ~ "fox".styled(Fox) ~ " clearing.",
        "Adds the " ~ "fox".styled(Fox) ~ " suit to its clearing.",
        "While moving, " ~ "fox".styled(Fox) ~ " clearings count as adjacent to " ~ "Foxburrow".styled(Fox) ~ ", ignoring paths."
    )
}

// Simple Landmarks (the owner's own rules, 2026-11-20; root/simple-landmarks.scala): the Landmarks Pack's Legendary Forge, Black
// Market and Elder Treetop pieces and the Tower, with simpler effects; a section of their own, so they go with any Landmarks choice
trait SimpleLandmark extends Landmark {
    override val group = "Simple Landmarks"
}

case object SimpleForgeLandmark extends SimpleLandmark {
    val name = "Forge"
    override val explain = $(
        "Simple Landmarks".hh ~ ", with the " ~ "Legendary Forge".hh ~ " piece; placed in a clearing without a landmark, not next to one.",
        "If your faction pieces are in its clearing, treat it as one of your crafting pieces."
    )
}

case object SimpleMarketLandmark extends SimpleLandmark {
    val name = "Market"
    override val explain = $(
        "Simple Landmarks".hh ~ ", with the " ~ "Black Market".hh ~ " piece; placed in a clearing with exactly one building slot and no ruin, without a landmark and not next to one.",
        "Once per turn, if your faction pieces are in its clearing, you may discard a card to draw a card."
    )
}

// official for the site (the owner, 2026-11-20): it counts toward the rankings, and in the tournament it is one of the Mountain's
// choices for the Tower's clearing (root/tournament.scala); on the setup screen it stays with the Simple Landmarks
case object BirdsongTowerLandmark extends SimpleLandmark {
    val name = "Birdsong Tower"
    override val explain = $(
        "Simple Landmarks".hh ~ ", with the " ~ "Tower".hh ~ " piece; placed in a clearing with a ruin and no landmark.",
        "If you rule its clearing at the start of your " ~ "Birdsong".hh ~ ", score a point."
    )
}

case object RoyalTreeLandmark extends SimpleLandmark {
    val name = "Royal Tree"
    override val explain = $(
        "Simple Landmarks".hh ~ ", with the " ~ "Elder Treetop".hh ~ " piece; placed in a corner clearing without a landmark, not next to one.",
        "Adds a building slot. While it's your turn, if your faction pieces are in its clearing, you rule its clearing."
    )
}

case object RabbittownLandmark extends Landmark {
    val name = "Rabbittown"
    override def elem = name.styled(Rabbit)
    override def forcedOff(all : $[BaseOption]) = $(MapDefaultLandmarks, MountainSuitedLandmark, NoLandmarks)
    override val explain = $(
        "Homeland".hh ~ " landmark, placed in a " ~ "rabbit".styled(Rabbit) ~ " clearing.",
        "Adds the " ~ "rabbit".styled(Rabbit) ~ " suit to its clearing.",
        "Once in Daylight, spend a " ~ "rabbit".styled(Rabbit) ~ " card to place a warrior in " ~ "Rabbittown".styled(Rabbit) ~ " for each other " ~ "rabbit".styled(Rabbit) ~ " clearing with your pieces, then battle there."
    )
}



trait HirelingsOption extends GameOption with OneOfGroup with FoldedOption {
    val group = "Hirelings"
}

case object NoHirelings extends HirelingsOption {
    val valueOn = "None".hh
    override val explain = $(
        "No hirelings in the game.",
    )
}

case object ThreeDemotedHirelings extends HirelingsOption {
    val valueOn = "Demoted".hh ~ ", " ~ "Demoted".hh ~ ", " ~ "Demoted".hh
    override val explain = $(
        "Three " ~ "Demoted".hh ~ " hirelings, drawn at random from those selected below.", "The Law of Root count for " ~ "5".hl ~ " or more players.",
    )
}

case object TwoDemotedOneNormalHirelings extends HirelingsOption {
    val valueOn = "Demoted".hh ~ ", " ~ "Demoted".hh ~ ", " ~ "Staunch".styled(styles.hit)
    override val explain = $(
        "Two " ~ "Demoted".hh ~ " and one " ~ "Staunch".styled(styles.hit) ~ " hireling, drawn at random from those selected below.", "The Law of Root count for " ~ "4".hl ~ " players.",
    )
}

case object OneDemotedTwoNormalHirelings extends HirelingsOption {
    val valueOn = "Demoted".hh ~ ", " ~ "Staunch".styled(styles.hit) ~ ", " ~ "Staunch".styled(styles.hit)
    override val explain = $(
        "One " ~ "Demoted".hh ~ " and two " ~ "Staunch".styled(styles.hit) ~ " hirelings, drawn at random from those selected below.", "The Law of Root count for " ~ "3".hl ~ " players.",
    )
}

case object ThreeNormalHirelings extends HirelingsOption {
    val valueOn = "Staunch".styled(styles.hit) ~ ", " ~ "Staunch".styled(styles.hit) ~ ", " ~ "Staunch".styled(styles.hit)
    override val explain = $(
        "Three " ~ "Staunch".styled(styles.hit) ~ " hirelings, drawn at random from those selected below.", "The Law of Root count for " ~ "2".hl ~ " players.",
    )
}

case object ThreeRandomHirelings extends HirelingsOption {
    val valueOn = "Random".styled(StreetBand) ~ ", " ~ "Random".styled(StoicProtector) ~ ", " ~ "Random".styled(BanditGangs)
    override val explain = $(
        "Three hirelings, each " ~ "Demoted".hh ~ " or " ~ "Staunch".styled(styles.hit) ~ ", drawn at random from those selected below.",
    )
}

case object SelectedHirelings extends HirelingsOption {
    val valueOn = "All Selected".hh
    override val explain = $(
        &("Any number of " ~ &("Hirelings".hh ~ " selected")) ~ " will be available in the game," ~ Break ~ &("with hiring threshold " ~ &("every " ~ 4.vp ~ ".")),
        &("Some " ~ "Hirelings".hh ~ " can be dropped") ~ " if " ~ &("Faction".hh ~ "/" ~ "Hireling".hh ~ " clashing") ~ " is disallowed if that would leave" ~ &(" too few factions to draft.")
    )
}


trait HirelingsClashOption extends GameOption with ToggleOption with FoldedOption {
    val group = "Hirelings Clash"
}

case object HirelingHirelingClash extends HirelingsClashOption {
    val valueOn = "Allow Hireling".hh ~ "/" ~ "Hireling Clash".hh
    override def required(all : $[BaseOption]) = all.of[IncludeHireling]./~(o => all.of[IncludeHireling].%(_.hireling.clashKey == o.hireling.clashKey)./(h => $(o, h)))
    override val explain = $(
        "Both " ~ "Staunch".styled(styles.hit) ~ " and " ~ "Demoted".hh ~ " versions" ~ &(" of the same Hireling" ~ &(" can be drawn."))
    )
}

case object FactionHirelingClash extends HirelingsClashOption {
    val valueOn = "Allow Faction".hh ~ "/" ~ "Hireling Clash".hh
    override def required(all : $[BaseOption]) = all.of[IncludeFaction]./~(o => all.of[IncludeHireling].%(_.hireling.clashKey == o.faction.clashKey)./(h => $(o, h)))
    override val explain = $(
        "Both a faction and" ~ &(" a matching hireling ") ~ "can be drawn."
    )
}


// The Doomed Expansion's Turnlings (fan-made by slugfacekillah; Law of Doom D8; `root/turnlings.scala`): taking a hireling
// marker, the player turns the hireling over to another player, who rolls for its control
case object TurnOverHirelings extends GameOption with ToggleOption with FoldedOption {
    val group = "Turnlings".hh
    override def folder = Meta.fanHirelings
    val valueOn = "Turn Over Hirelings".hh
    override def required(all : $[BaseOption]) = $($(TwoDemotedOneNormalHirelings), $(OneDemotedTwoNormalHirelings), $(ThreeNormalHirelings), $(ThreeDemotedHirelings), $(ThreeRandomHirelings), $(SelectedHirelings))
    override val explain = $(
        "Turn Over Hirelings".hh ~ SpacedDash ~ "the " ~ "Turnlings".hh ~ " rule from " ~ "The Doomed Expansion".hl ~ " by " ~ "slugfacekillah".hl ~ ":",
        "a player whose score reaches a hireling marker" ~ &(" turns the hireling over to " ~ &("another player,")) ~ &(" who rolls for its control."),
        "The " ~ "Turnlings".hh ~ " cards below can be drawn" ~ &(" with or without it."),
        FanInfo.doomedLink
    )
}

case class IncludeHireling(hireling : Hireling) extends GameOption with ToggleOption with FoldedOption {
    val group = hireling.demoted.?("Demoted".hh).|("Staunch".styled(styles.hit)) ~ hireling.is[Turnling].?(" Turnlings").|(" Hirelings")
    override def folder = hireling.is[Turnling].??(Meta.fanHirelings)
    val valueOn = hireling.name.styled(hireling)
    override def required(all : $[BaseOption]) =
        if (hireling.demoted)
            $($(ThreeDemotedHirelings), $(TwoDemotedOneNormalHirelings), $(OneDemotedTwoNormalHirelings), $(ThreeRandomHirelings), $(SelectedHirelings))
        else
            $($(TwoDemotedOneNormalHirelings), $(OneDemotedTwoNormalHirelings), $(ThreeNormalHirelings), $(ThreeRandomHirelings), $(SelectedHirelings))
}


// One folded section, "Hits and Ambushes when Online" (2026-11-12): they act only while the player has the game open,
// and only where the game's setup leaves the choice to the players (a Live Game, or a game started before the Hits and Ambushes
// section with neither Bluff Mode nor Auto Skip Ambushes)
object HitsAndAmbushesOnline {
    val fold = "Hits and Ambushes when Online (Live Games)"
}

trait AssignHitsOption extends hrf.Setting with OneOfGroup {
    val group = "Assign Hits"
    override def fold = Some(HitsAndAmbushesOnline.fold)
}

case object PromptAssignHits extends AssignHitsOption {
    val valueOn = "Always Prompt".hlb
}

case object AutoAssignHits extends AssignHitsOption {
    val valueOn = "Auto Assign If No Choice".hlb
}


trait AmbushOption extends hrf.Setting with OneOfGroup {
    val group = "Ambush!"
    override def fold = Some(HitsAndAmbushesOnline.fold)
}

case object PromptAmbush extends AmbushOption {
    val valueOn = "Always Prompt".hlb
}

case object AutoAmbush extends AmbushOption {
    val valueOn = "Auto Skip If None".hlb
}


trait OutOfTurnOption extends hrf.Setting with OneOfGroup {
    val group = "Out of Turn Actions"
}

case object NoOutOfTurn extends OutOfTurnOption {
    val valueOn = "None".hlb
}

case object BasicOutOfTurn extends OutOfTurnOption {
    val valueOn = "Basic".hlb ~ " (Only Shortcuts That Cost You Nothing)".hl
}

case object DetailedOutOfTurn extends OutOfTurnOption {
    val valueOn = "Detailed".hlb ~ " (Every Shortcut)".hl
}


trait RuleOption extends hrf.Setting with OneOfGroup {
    val group = "Clearing Rule"
}

case object ShowRule extends RuleOption {
    val valueOn = "Show".hlb
}

case object FlagRule extends RuleOption {
    val valueOn = "Flag".hlb
}

case object HideRule extends RuleOption {
    val valueOn = "Hide".hlb
}


trait WarriorsOption extends hrf.Setting with OneOfGroup {
    val group = "Warriors"
}

case object StackWarriors extends WarriorsOption {
    val valueOn = "Stacked".hlb ~ " (One Figure With a Count)".hl
}

case object SeparateWarriors extends WarriorsOption {
    val valueOn = "Separate".hlb ~ " (Every Figure)".hl
}


// Local games with more than one human player: a "Pass to <faction>" screen hides the action pane (hand and
// choices) whenever the turn goes to another player on the device, until that player taps it (`ask` in root/ui.scala)
trait PassScreenOption extends hrf.Setting with OneOfGroup {
    val group = "Pass and Play"
}

case object ShowPassScreen extends PassScreenOption {
    val valueOn = "Pass Screen".hlb ~ " (Local Games: Hand Hidden Until the Next Player Taps)".hl
}

case object NoPassScreen extends PassScreenOption {
    val valueOn = "Off".hlb
}


trait TrackersOption extends hrf.Setting with OneOfGroup {
    val group = "Board Trackers"
}

case object ShowBoardTrackers extends TrackersOption {
    val valueOn = "Shown".hlb ~ " (Score and Items on the Map)".hl
}

case object HideBoardTrackers extends TrackersOption {
    val valueOn = "Hidden".hlb
}


trait CardPilesOption extends hrf.Setting with OneOfGroup {
    val group = "Card Piles"
}

case object CardPilesInPanel extends CardPilesOption {
    val valueOn = "Side Panel".hlb
}

case object CardPilesOnMap extends CardPilesOption {
    val valueOn = "On the Map".hlb ~ " (Draw, Discard, Pond, Lost Souls and Dominances)".hl
}


trait LanguageOption extends hrf.Setting with OneOfGroup {
    val group = "Language"
}

case object EnglishLanguage extends LanguageOption {
    val valueOn = "English".hlb
}

// display only (root/french.scala), so players in one game can each pick their language
case object FrenchLanguage extends LanguageOption {
    val valueOn = "Français".hlb
}


trait HighlightsOption extends hrf.Setting with OneOfGroup {
    val group = "Highlights"
}

case object HighlightNone extends HighlightsOption {
    val valueOn = "None".hlb
}

case object HighlightClearings extends HighlightsOption {
    val valueOn = "Clearings".hlb
}

trait Switch extends GameOption {
    val group = ""
}

case object UseAutoAssignHits extends Switch {
    val valueOn = "Auto " ~ "Assign Hits".hl ~ " in " ~ "Battle".hl ~ " (If No Choice)"
}

// Shown whatever the Out of Turn Actions setting, so a player away from the game doesn't hold up battles, but only in games
// where the setup leaves it to the players (switchesFor in root/ui.scala): not with Bluff Mode, where every attacked player is
// asked, nor where the game already skips them
case object AutoSkipAmbushIfNone extends Switch {
    val valueOn = "Auto Skip " ~ "Ambush".hl ~ " (If None)"
}

case class SkipAmbushSuit(suit : BaseSuit) extends Switch {
    val valueOn = "Skip " ~ "Ambush".hl ~ " in " ~ suit.elem
}

case class SkipFieldHospitalsSuit(suit : BaseSuit) extends Switch {
    val valueOn = "Skip " ~ "Field Hospitals".hl ~ " in " ~ suit.elem
}

case class SkipPartisansSuit(suit : BaseSuit) extends Switch {
    val valueOn = "Skip " ~ "Partisans".hl ~ " in " ~ suit.elem
}

case class UsePartisansSuit(suit : BaseSuit) extends Switch {
    val valueOn = "Use " ~ "Partisans".hl ~ " in " ~ suit.elem
}

case class SkipFieldHospitalsCount(n : Int) extends Switch {
    val valueOn = "Skip " ~ "Field Hospitals".hl ~ " for " ~ 1.hlb ~ " Warrior"
}

case class PickFactionIfAvailable(f : PlayChoice) extends Switch {
    val valueOn = "Pick " ~ f.elem ~ " If Available"
}




object Meta extends MetaGame {
    val gaming = root.gaming

    type F = Faction

    def tagF = implicitly

    val name = "root"
    val label = "Root"

    override def settingsList = super.settingsList ++ $(PromptAssignHits, AutoAssignHits, PromptAmbush, AutoAmbush, ShowRule, FlagRule, HideRule, StackWarriors, SeparateWarriors, ShowBoardTrackers, HideBoardTrackers, CardPilesInPanel, CardPilesOnMap, NoOutOfTurn, BasicOutOfTurn, DetailedOutOfTurn, HighlightClearings, HighlightNone, ShowPassScreen, NoPassScreen, EnglishLanguage, FrenchLanguage)
    // the card piles on the map are the default (2026-11-24, owner), so the side panel saved while it was the default is moved there once
    override def settingsMoves = $(("card-piles-on-map", CardPilesInPanel.toString, CardPilesOnMap.toString))

    override def settingsDefaults = super.settingsDefaults ++ $(AutoAssignHits, AutoAmbush, FlagRule, StackWarriors, ShowBoardTrackers, CardPilesOnMap, NoOutOfTurn, HighlightNone, ShowPassScreen, EnglishLanguage)

    override def translation(settings : $[hrf.Setting]) =
        if (settings.has(FrenchLanguage))
            ("fr", French.text _, French.image _)
        else
            super.translation(settings)

    override val about = $(
        "A quaint and quirky adaptation of the " ~ "R".styled(Fox).larger ~ "O".styled(Rabbit).larger ~ "O".styled(Mouse).larger ~ "T".styled(Bird).larger ~ " board game.",
        Gap,
        Gap,
        Gap,
        Gap,
        Gap,
        "It was a dark and stormy night in " ~ "2019".styled(Bird) ~ ".",
        "Just having finished my first play of the game, I knew", "something was wrong.",
        "It was obvious the game needed a thousand plays to shine.",
        "No way my friends were ready for such an ordeal.",
        "There was only one way...",
        Gap,
        Gap,
        Gap,
        Gap,
        Gap,
        "Three weeks later, the first version of the app with bots was ready.",
        "Written in " ~ "Scala".styled(Fox) ~ ", with too much implicits.",
        "Made public in " ~ "2020".styled(Bird) ~ " during the lockdown.",
        "Some of my friends were up for a ride after all.",
        "We played most every " ~ "Sunday".styled(Rabbit) ~ " ever since.",
        Gap,
        Gap,
        Gap,
        Gap,
        Gap,
        "Thanks to " ~ "Cole Wehrle".styled(Bird) ~ " and " ~ "Leder Games".styled(Bird) ~ " for developing",
        "such an immersive game.",
        "Thanks to " ~ "my friends".styled(Fox) ~ " for putting up with so many",
        "gaming sessions turned debuggїng sessions.",
        "Thanks to " ~ "the people".styled(Mouse) ~ " on " ~ &(Link("https://discord.gg/[REDACTED]", "Root Jeu en Ligne", $(xstyles.link))) ~ ", " ~
        &(Link("https://discord.gg/woodland-warriors-476234833572397056", "Woodland Warriors", $(xstyles.link))) ~ " and " ~
        &(Link("https://boardgamegeek.com/boardgame/237182/root/forums", "Root BGG forum", $(xstyles.link))).hh ~ " for support, suggestions and ideas.",
        "Thanks to everyone making fan content.",
        Gap,
        Gap,
        Gap,
        Gap,
        Gap,
        "Sincerely yours " ~ "Haunt Roll Fail".hl,
    )

    val official = $[Faction](MC, ED, WA, VB, LC, RF, UD, CC, LH, KI)
    val homeland = $[Faction](LDvE, TC, KD)
    val clones = $[Faction](BK, PE, FU, NB, CM, /*WC,*/ DR, RI, LK)
    val fun = $[Faction](KDvA, TCvA, FH, WW, DU, WG, EE, DM, DV, NW, XC, BP, WM, OF, TW, OMT, NX, UDC, AA, CUv2, CU, OK, AF, SF, MB)

    val factions = official ++ homeland ++ clones ++ fun

    // The playtest Lilypad Diaspora versions and the Tidepool Diaspora, no longer offered (2026-11-17), still read so older games load
    val retiredFactions = $[Faction](LDvD, LDvC, LDvB, TD)

    // the official Homeland factions are listed with the other official ones, after the Keepers (2026-11-20)
    override def factionGroup(f : F) : |[Elem] = clones.has(f).?("Mirror Factions".txt) || fun.has(f).?("Fan Factions".txt)

    // the picker's Fan Factions fold, like the setup's fan sections
    override def foldedFactionGroups : $[String] = $("Fan Factions")

    // the setup's folders of fan content (BaseOption.folder)
    val fanHirelings = |("Fan Hirelings")
    val scenarios = |("Scenarios")

    val minPlayers = 2

    // demoted ones first, then staunch, each in the cards' order (Marauder, Riverfolk and Underworld packs, then Homeland)
    val hirelings = $(
        FelinePhysicians, BluebirdNobles, RabbitScouts, Brigand, LizardEnvoys, OtterDivers, MoleArtisians, RavenSentries, RatSmugglers, BadgerBodyguards, StoicProtector, BanditGangs, StreetBand, BatMessengers, FrogTinkers, StrugglingFarmers,
        ForestPatrol, LastDynasty, SpringUprising, TheExile, WarmSunProphets, RiverfolkFlotilla, SunwardExpedition, CorvidSpies, FlameBearers, VaultKeepers, FuriousProtector, HighwayBandits, PopularBand, SunnyAdvocates, RiverRoamers, ProsperousFarmers)

    // the Doomed Expansion's Turnlings (fan-made), offered apart and not picked by default
    val turnlings = $[Hireling](
        ForestConnections, InfectiousSpirits, GoldenPig, CardinalCarts, CorporateOverreach, FeederTurnling, MiracleCureAll, FineSwine,
        MidnightMurderers, TheInfected, KnightTurnling, RougeAgents, UnionMembers, ConvergingCoup, BuzzlingBuggers, GristlyGrunts)

    override def optionPages(n : Int, l : $[F]) : $[$[GameOption]] = $(
        hiddenOptions ++
        rankingOptions ++
        hitsAndAmbushes(l) ++
        $(SetupTypeCorners, SetupTypeHomelands) ++
        (n > 2).?(TotalWarDominance) ++
        $,
        $(AutumnMap, WinterMap, LakeMap, MountainMap, GorgeMap, MarshMap, RandomOfficialMap, TidalMap, TundraMap, GloomMap, UrbanMap, LegendsMap, DeepWoodsMap, WastelandsMap) ++
        $(DeepWoodsSettling, WastelandsWater) ++
        $(DefaultClearings, NoClustersClearings, AllRandomClearings, SuitPairsClearings, ConnectedClearings, ThreeFourFiveClearings) ++
        $(MapDefaultLandmarks, MountainSuitedLandmark, NoLandmarks, FerryLandmark, LostCityLandmark, TowerLandmark, MouseholdLandmark, FoxburrowLandmark, RabbittownLandmark) ++
        $(SimpleForgeLandmark, SimpleMarketLandmark, BirdsongTowerLandmark, RoyalTreeLandmark) ++
        legendsOptions ++
        $,
        $(CapitalistsDeck, ExilesDeck, SquiresDeck, StandardDeck, MixedDeck, DuskDeck, FrankenDeck, DarkDeck, UpstartsDeck, SorceryDeck) ++
        (n >= 5).$(ActionBooster) ++
        $,
        (l.of[Feline].num > 1).$(TheWoodMustFlow) ++
        (l.of[Aviary].num > 1).$(OneTrueEmpire) ++
        (l.of[Insurgent].num > 1).$(Imprinting) ++
        (l.of[Hero].num > 1).$(RootingForTheUnderdog) ++
        (l.of[Fanatic].num > 1).$(DeathToTheInfidels) ++
        (l.of[Mischief].num > 1).$(DoubleAgents) ++
        (l.of[Underground].num > 1).$(OldBoys) ++
        (l.of[Horde].num > 1).$(PeerPressure) ++
        (l.of[Feline].any || l.of[Fanatic].any || l.of[Mischief].any).$(AdSetBuffOn, AdSetBuffOff) ++
        l.of[Feline].any.$(UntargetableKeep, IndestructibleKeep, ThreeHitKeep, InstantRebuildKeep, KeepExtraBuildingSlot, SawmillMassOverwork, WorkshopEveningCraft, WorkshopActionBonuses, Catapults) ++
        l.of[Hero].any.$(RandomCharacter, FolkHeroCharacter, DoomedVagabonds) ++
        l.of[Hero].any.$(WarriorsOnlyInfamy, LimitedInfamy, RevisedQuests) ++
        l.of[Hero].any.$(TinkerNoBag, HarrierNoCoins, ScoundrelTeapotNoBoot, ArbiterScoreCard, RoninSwiftTorch) ++
        l.of[Fanatic].any.$(LimitedTrampleDiscard, RevengeFromAnyRemoval, LizardHandSizeSix) ++
        l.of[Trader].any.$(ExportToCommitments) ++
        l.of[Underground].any.$(MayorAssemblyAction) ++
        l.of[Mischief].any.$(ThreeOfEachPlot, EmbeddedAgentsBeforeBattle, DiversionPlot, BrutalHonesty) ++
        l.of[Expedition].any.$(ColumnBonusCardVP) ++
        l.of[Knaves].any.$(DoomedCaptains) ++
        l.of[Horde].any.$(SeparateItemsInRuins) ++
        $,
        $(NoHirelings, ThreeDemotedHirelings, TwoDemotedOneNormalHirelings, OneDemotedTwoNormalHirelings, ThreeNormalHirelings, ThreeRandomHirelings, SelectedHirelings) ++
        $(HirelingHirelingClash, FactionHirelingClash) ++
        hirelings./(IncludeHireling) ++
        $(TurnOverHirelings) ++
        turnlings./(IncludeHireling) ++
        $,
    )

    // Off Turn Field Hospitals only with the Marquise (in Advanced Setup always, as the factions are drafted later)
    def hitsAndAmbushes(l : $[F]) : $[GameOption] = $(LiveGameMode, BluffMode, AutoHitsAssignmentMode, OffTurnHitsPreset) ++ l.of[Feline].any.$(OffTurnFieldHospitals)

    val maps : $[MapOption] = $(AutumnMap, WinterMap, LakeMap, MountainMap, TidalMap, TundraMap, GloomMap, GorgeMap, MarshMap, UrbanMap, LegendsMap, DeepWoodsMap, WastelandsMap)

    val officialMaps : $[MapOption] = $(AutumnMap, WinterMap, LakeMap, MountainMap, GorgeMap, MarshMap)

    // Random map: one of the maps (or of the official ones) is drawn; Default clearings, printed only on Autumn, become No Clusters elsewhere
    override def startOptions(options : $[O]) : $[O] = (
        if (options.has(RandomMap).not && options.has(RandomOfficialMap).not)
            options
        else {
            val map = options.has(RandomOfficialMap).?(officialMaps).|(maps).shuffle.head
            val o = options./{
                case RandomMap | RandomOfficialMap => map
                case o => o
            }

            if (map != AutumnMap && o.has(DefaultClearings))
                o.diff($(DefaultClearings)).but(NoClustersClearings) :+ NoClustersClearings
            else
                o
        }
    ).distinct

    val rankingOptions : $[GameOption] = $(RankedGame, UnrankedGame)

    override def optionsFor(n : Int, l : $[F]) =
        hiddenOptions ++
        rankingOptions ++
        $(AutumnMap, WinterMap, LakeMap, MountainMap, GorgeMap, MarshMap, RandomOfficialMap, TidalMap, TundraMap, GloomMap, UrbanMap, LegendsMap, DeepWoodsMap, WastelandsMap) ++
        $(DeepWoodsSettling, WastelandsWater) ++
        $(DefaultClearings, NoClustersClearings, AllRandomClearings, SuitPairsClearings, ConnectedClearings, ThreeFourFiveClearings) ++
        $(SetupTypeCorners, SetupTypeHomelands) ++
        $(CapitalistsDeck, ExilesDeck, SquiresDeck, StandardDeck, MixedDeck, DuskDeck, FrankenDeck, DarkDeck, UpstartsDeck, SorceryDeck) ++
        (n >= 5).$(ActionBooster) ++
        hitsAndAmbushes(l) ++
        (n > 2).?(TotalWarDominance) ++
        (l.of[Feline].num > 1).$(TheWoodMustFlow) ++
        (l.of[Aviary].num > 1).$(OneTrueEmpire) ++
        (l.of[Insurgent].num > 1).$(Imprinting) ++
        (l.of[Hero].num > 1).$(RootingForTheUnderdog) ++
        (l.of[Fanatic].num > 1).$(DeathToTheInfidels) ++
        (l.of[Mischief].num > 1).$(DoubleAgents) ++
        (l.of[Underground].num > 1).$(OldBoys) ++
        (l.of[Horde].num > 1).$(PeerPressure) ++
        (l.of[Feline].any || l.of[Fanatic].any || l.of[Mischief].any).$(AdSetBuffOn, AdSetBuffOff) ++
        l.of[Feline].any.$(UntargetableKeep, IndestructibleKeep, ThreeHitKeep, InstantRebuildKeep, KeepExtraBuildingSlot, SawmillMassOverwork, WorkshopEveningCraft, WorkshopActionBonuses, Catapults) ++
        l.of[Hero].any.$(RandomCharacter, FolkHeroCharacter, DoomedVagabonds) ++
        l.of[Hero].any.$(WarriorsOnlyInfamy, LimitedInfamy, RevisedQuests) ++
        l.of[Hero].any.$(TinkerNoBag, HarrierNoCoins, ScoundrelTeapotNoBoot, ArbiterScoreCard, RoninSwiftTorch) ++
        l.of[Fanatic].any.$(LimitedTrampleDiscard, RevengeFromAnyRemoval, LizardHandSizeSix) ++
        l.of[Trader].any.$(ExportToCommitments) ++
        l.of[Underground].any.$(MayorAssemblyAction) ++
        l.of[Mischief].any.$(ThreeOfEachPlot, EmbeddedAgentsBeforeBattle, DiversionPlot, BrutalHonesty) ++
        l.of[Expedition].any.$(ColumnBonusCardVP) ++
        l.of[Knaves].any.$(DoomedCaptains) ++
        l.of[Horde].any.$(SeparateItemsInRuins) ++
        $(MapDefaultLandmarks, MountainSuitedLandmark, NoLandmarks, FerryLandmark, LostCityLandmark, TowerLandmark, MouseholdLandmark, FoxburrowLandmark, RabbittownLandmark) ++
        $(SimpleForgeLandmark, SimpleMarketLandmark, BirdsongTowerLandmark, RoyalTreeLandmark) ++
        $(NoHirelings, ThreeDemotedHirelings, TwoDemotedOneNormalHirelings, OneDemotedTwoNormalHirelings, ThreeNormalHirelings, ThreeRandomHirelings, SelectedHirelings) ++
        $(HirelingHirelingClash, FactionHirelingClash) ++
        hirelings./(IncludeHireling) ++
        $(TurnOverHirelings) ++
        turnlings./(IncludeHireling) ++
        legendsOptions

    // the Doomed Expansion's Legends Scenarios (root/legends.scala)
    def legendsOptions : $[GameOption] =
        $(CastlesAndCatacombs, TheFifthRuin, DarkRituals, HuntTheLegend) ++
        $(PickShadowmaw, PickWhiteWolf, PickBeaverbot, PickRedKnight, PickDemonBear) ++
        $(RallyRound, FriendshipRing, BonafiedPrince, Merchandising, ThePrecious, RingOfRage, ReturnOfTheKing, PowerOfTheSchwartz) ++
        $(FatFingers, LilLooters, FiveFingerFilch, SeizureAndSurrender, Pickpockets)

    override val hiddenOptions =
        (factions ++ retiredFactions)./(IncludeFaction) ++
        $(SeatingGiven, SeatingRandom) ++
        $(FactionSeatingGiven, FactionSeatingRandom) ++
        $(SetupOrderPriority, SetupOrderReverse) ++
        $(CardDraftStandard, CardDraftFive) ++
        fixes ++
        settled ++
        retired ++
        RulesVersion.all ++
        $(TournamentDraft, TournamentCapitalists)

    // The hidden options of rules fixes, each gating a fix so the games started before it replay the same (most new games get through
    // their RulesVersion); not listed in a game's log (OptionsAction)
    lazy val fixes : $[O] = $(FrogCardsAfterStartingHands, FrogCardsSkipLostSouls, FrogsCraftByClearing, LizardsCraftByClearing, CompanionsUntilSpent, HirelingsByTheCard, GovernorsStopEnclaveFlips, OutragePeekNeedsOk, CounterMiceInABush, KnavesCaptainsInDraft, SkipIdleRallyReconcile, GovernorsStopDelve, FrogAmbushToPondBottom, FrogAmbushBottomOnlyAsAmbush, CouncilRevealOnce, EntreatGainUnasked, BanishNeedsWarriors, FeatherRufflersOnCraft, SilverTongueOneEffect, GovernorsStopCrafting, MouseholdFromSupply, BuyServicesAgain, BanishAfterIgnoredHits, CouncilEveningClearPath, ReconcileOneAtATime, FearsAfterRemoval, RiverfolkCraftUngoverned, PrisonersFromCodefenders, GovernorsStopExposure, FrogCardsAtDiasporaSetup, HitsAndAmbushesSetup, DuskDeckFixes, BanishCaptainsLast, SuitedLandmarkAnySuit, NabFearsOrder, CharismaticPlacesWhatItCan)

    def unlisted : $[O] = fixes ++ settled ++ RulesVersion.all

    // No longer choices (2026-10-31): the card rulings Official Rules always picked. Every new game gets them
    // (mandatoryFor); a game started without them keeps its rules, as its options are recorded with it. Auto Hits Assignment was one
    // too, until the Hits and Ambushes section made it a choice again (2026-11-12)
    lazy val settled : $[O] = $(NonBirdPartisans, ErrataCoffinMakers, UnthematicPropagandaBureau, TunnelsIgnoreRaid, TunnelsIgnoreTradePosts)

    // No longer offered: Hostile Takeover and Shared History did nothing, Coffin Makers' "any warrior" house rule gave way to the errata,
    // the Async options gave way to the Hits and Ambushes section (2026-11-12), and Random map to Random Official (2026-11-18).
    // Still read, so the games that have them load; dropped from the options a setup screen saved (completeSaved)
    lazy val retired : $[O] = $(HostileTakeover, SharedHistory, UnthematicCoffinMakers, AutoAmbushSkippingMode, ForcedAsyncMode, ForcedAsyncChooseHits, RandomMap)

    // The hidden options every new game gets: one rules version (RulesVersion), which stands for every rules fix made since the
    // fixes were first gated (RulesVersion.expand, used by Game), so a new game records one option instead of each fix
    lazy val newGame : $[O] = $(RulesVersion.latest)

    override def completeSaved(n : Int, l : $[F], options : $[O]) = settlingOn(autoHitsOn(randomOfficial(options).diff(retired).diff(RulesVersion.implied)))

    // Deep Woods Settling is on by default (2026-11-18): a setup a browser saved without the Deep Woods map picked starts with it on
    // the Outsiders maps' own rules are on by default (2026-11-18 and 19), also in a setup a browser saved without that map picked
    def settlingOn(options : $[O]) : $[O] = {
        val a = options.has(DeepWoodsMap).?(options).|((options :+ DeepWoodsSettling).distinct)
        a.has(WastelandsMap).?(a).|((a :+ WastelandsWater).distinct)
    }

    // A setup saved with the retired Random map gets Random Official instead
    def randomOfficial(options : $[O]) : $[O] = options./{
        case RandomMap => RandomOfficialMap
        case o => o
    }.distinct

    // Auto Assign Hits is on by default and Ambush Bluff Mode off (2026-11-17): a setup a browser saved with none of Auto Assign
    // Hits, Live Game or the hits preset (from before the Hits and Ambushes section, when Auto Assign Hits was always on and so never
    // saved, and Bluff Mode was the default) starts with Auto Assign Hits on and Bluff Mode off
    def autoHitsOn(options : $[O]) : $[O] = options.intersect($(AutoHitsAssignmentMode, LiveGameMode, OffTurnHitsPreset)).any.?(options).|(options.but(BluffMode) :+ AutoHitsAssignmentMode)

    // offered only in Advanced Setup (MetaAdset), listed here so every game reads them
    lazy val adsetOnly : $[O] = $(InsurgentLockOn, InsurgentLockOff)

    val options = optionsFor(4, factions) ++ hiddenOptions ++ (factions ++ retiredFactions)./(IncludeFaction) ++ adsetOnly

    override def mandatoryFor(n : Int, l : $[F]) : $[O] = l./(IncludeFaction) ++ newGame

    override def defaultsFor(n : Int, l : $[F]) = $(
        RankedGame,
        SeatingGiven,
        FactionSeatingGiven,
        RandomOfficialMap,
        AllRandomClearings,
        SquiresDeck,
        AutoHitsAssignmentMode,
        FolkHeroCharacter,
        AdSetBuffOff,
        SetupTypeCorners,
        SetupOrderPriority,
        CardDraftStandard,
        MountainSuitedLandmark,
        DeepWoodsSettling,
        WastelandsWater,
        NoHirelings) ++ hirelings./(IncludeHireling)

    // Two presets (2026-10-31): the hirelings one repeated the Hirelings section, Sunday Games was JT's group's house rules
    override def presetsFor(n : Int, l : $[F]) = $(
        ("Site Defaults".spn, hirelings./(IncludeHireling), $),
        ("Official".hl ~ " Rules", $(
            AllRandomClearings,
            SquiresDeck,
            AdSetBuffOff
        ), $),
    )

    override def presetsInfo(n : Int, l : $[F]) = $(
        $(
            "Puts every option back to what the site starts with:",
            &("Random Official".hh ~ " map, ") ~ &("All Random".hh ~ " clearings, ") ~ &("Map Default".hh ~ " landmarks (on " ~ "Mountain".styled(Fox) ~ " a random suited one), ") ~ &("Squires and Disciples".hh ~ " deck, ") ~ &("Corners".hh ~ " setup, ") ~ &("Auto Assign Hits".hh ~ " (no " ~ "Ambush Bluff Mode".hh ~ "), ") ~ &("no hirelings."),
        ),
        $(
            "The rules as Leder Games prints them:",
            &("All Random".hh ~ " clearings, ") ~ &("the " ~ "Squires and Disciples".hh ~ " deck, ") ~ &("and the " ~ "AdSet".hh ~ " setup buffs " ~ "Off".hh ~ "."),
            "Nothing else changes.",
        ),
    )

    // In the Scenarios folder, a scenario's own choices (the Ring's rules, the capture rule, the starting monster) show only once it is picked
    override def optionShown(o : O, selected : $[O]) : Boolean = o match {
        case _ : RingRule | _ : CaptureRule | _ : MonsterChoice | DeepWoodsSettling | WastelandsWater => o.required(options).exists(_.forall(r => selected.exists(_ == r)))
        case _ => true
    }

    override def groupInfo(o : O) = o.group.text match {
        case "Hits and Ambushes – Offline Players" => $(
            &("What the game does " ~ "by itself".hl ~ " in a battle,") ~ " " ~ &("so it doesn't wait for a player who isn't there."),
            &("What is picked here applies to " ~ "every player".hl ~ " and comes first:") ~ " " ~ &("each player's own " ~ "Hits and Ambushes when Online".hh ~ " settings (under " ~ "Interface".hh ~ ")") ~ " " ~ &("and " ~ "Off Turn Actions".hh ~ " switches in the game") ~ " " ~ &("only act where it leaves the choice to them,") ~ " " ~ &("and the switches show only there."),
            &("Ambush Bluff Mode".hh ~ ":") ~ " " ~ &("every attacked player is asked about an " ~ "ambush".styled(Bird) ~ ", even without one.") ~ " " ~ &("Off, a player with none is skipped.")
                ~ " " ~ &("Auto Assign Hits".hh ~ ":") ~ " " ~ &("hits".styled(styles.hit) ~ " with only one way to take them are taken without asking."),
            &("Off Turn Hits Preset".hh ~ ":") ~ " " ~ &("hits are never asked for;") ~ " " ~ &("each player sets their order at the end of their turn.")
                ~ " " ~ &("Off Turn".hh ~ " " ~ "Field Hospitals".styled(MC) ~ ":") ~ " " ~ &(MC.elem ~ " plans it at the end of its turn."),
            &("Par".styled(Fox) ~ "tis".styled(Rabbit) ~ "ans".styled(Mouse) ~ " are used off turn without asking") ~ " " ~ &("when no cards have to be discarded,") ~ " " ~ &("except in a " ~ "Live Game".hh ~ "."),
            &("Live Game".hh ~ ":") ~ " " ~ &("everyone is online;") ~ " " ~ &("the game does none of this,") ~ " " ~ &("and each player's own settings decide."),
        )
        case _ => $
    }

    // Quick Game is the site's defaults with random factions (as Northgard's): each option at random, retired ones too, took up to
    // a minute to settle and made games that couldn't start
    override def quickOptions = Map()

    val quickMin = 4
    val quickMax = 4

    override val quickFactions = official ++ $(XC) ++ $(CUv2) ++ $(FH)

    def randomGameName() = {
        val n = $("Power", "Victory", "Glory", "Destiny", "Might", "Fight", "Right", "Betrayal", "Wood", "Land", "Air", "Ground").shuffle
        val c = $("for", "against", "versus", "through", "and", "of", "in", "as").shuffle
        n.head + " " + c.head + " " + n.last
    }

    def reach(factions : $[Faction]) =
        factions.of[Feline].num * 10 +
        factions.of[Aviary].num * 7 +
        factions.of[Insurgent].num * 3 +
        factions.of[Trader].num * 5 +
        factions.of[Fanatic].num * 2 +
        factions.of[Underground].num * 8 +
        factions.of[Mischief].num * 3 +
        factions.of[Expedition].num * 8 +
        factions.of[Horde].num * 9 +
        factions.of[Knaves].num * 4 +
        factions.of[WarriorsWake].num * 5 +
        factions.of[BonePatrol].num * 5 +
        factions.of[WingedMenace].num * 6 +
        factions.of[OrderOfTheForest].num * 5 +
        factions.of[TwelveColonies].num * 5 +
        factions.of[OldManTinker].num * 6 +
        factions.of[NoxiousBattery].num * 5 +
        factions.of[UnitedDoveCorps].num * 6 +
        factions.of[ArachnidAssociation].num * 5 +
        factions.of[Doomsayers].num * 6 +
        factions.of[Whisperers].num * 2 +
        factions.of[Marquistadors].num * 9 +
        (0 :: 5 :: 7 :: 9)(factions.of[Hero].num)

    def validateFactionCombination(factions : $[Faction]) = {
        if (factions.num < 2)
            ErrorResult("Select at least two factions")
        else
        if (factions.num > 8)
            ErrorResult("Max eight factions")
        else
        if (factions.of[Knaves].any && factions.of[Hero].any)
            ErrorResult("Knaves of the Deepwood can't play with the Vagabond")
        else
        if (factions.of[Whisperers].any && factions.num < 5)
            ErrorResult("The Whisperers Guild only plays with 5 or more players")
        else
        if (factions.has(DV) && factions.num < 4)
            ErrorResult("The Doomed Vagabond only plays with 4 or more players")
        else
        if (factions.has(DV) && factions.of[Hero].num > 1)
            ErrorResult("The Doomed Vagabond can't play with another Vagabond")
        else
        if (factions.has(EE) && factions.but(EE).of[Aviary].any)
            ErrorResult("Eyrie's End can't play with the Eyrie Dynasties")
        else
        if (factions.has(DM) && factions.of[Feline].any)
            ErrorResult("Dawn of the Marquistadors can't play with the Marquise de Cat")
        else
        if (factions.has(NW) && factions.but(NW).of[Knaves].any)
            ErrorResult("The Naifs can't play with the Knaves of the Deepwood")
        else
        if (reach(factions) < factions.num.of(0 :: 0 :: 17 :: 18 :: 21 :: 25 :: 28 :: 28 :: 28))
            WarningResult("Reach " + reach(factions))
        else
            InfoResult("Reach " + reach(factions))
    }

    def validateFactionSeatingOptions(factions : $[Faction], options : $[O]) = {
        val ff = factions.of[Hero]./(h => (factions.dropWhile(_ != h) ++ factions.takeWhile(_ != h)).drop(1).takeWhile(_.is[Hero].not).num)

        validateFactionCombination(factions) && rankedCheck(factions, options) && (
            if (factions.of[Feline].num + factions.of[Aviary].num + factions.of[Fanatic].num + factions.of[Underground].num + factions.of[Horde].num > 4 && options.has(SetupTypeCorners))
                ErrorResult("Too many corner factions")
            else
            if (ff.maxOr(0) - ff.minOr(0) > 1)
                WarningResult("Vagabonds seated unevenly")
            else
                InfoResult("")
        )
    }

    def factionName(f : Faction) = f.name
    def factionElem(f : Faction) = f.elem
    override def factionNote(f : Faction) : Elem = f.note

    override def setupDesign = true
    private lazy val glyphs : Map[String, String] = assets./~(_.get).%(_.name.endsWith("-glyph"))./(a => a.name -> ("/hrf/webp2/" + path + "/images/" + a.copy(ext = "webp").src)).toMap
    override def menuImages = glyphs
    private def icon(f : Faction, s : Style) : Elem = glyphs.contains(f.style.toLowerCase + "-glyph").?(Image(f.style.toLowerCase + "-glyph", s, "") : Elem).|(Empty)
    override def factionTile(f : Faction) = |(Div(icon(f, elem.styles.pickIcon), elem.styles.pickIconBox) ~ Div(f.name.hlb) ~ homeland.has(f).?(Div("Homeland".spn(xstyles.smaller85))).|(Empty))
    override def factionRowIcon(f : Faction) : Elem = icon(f, elem.styles.menuIcon)
    override def factionChosenElem(f : Faction) : Elem = factionRowIcon(f) ~ f.name.hlb

    // The picker's ? for the fan, playtest and mirror factions: who made it, what it does and how it plays (FanInfo)
    override def factionInfo(f : Faction) = FanInfo.about(f).some./(l => ("?".txt, f.elem, l))

    override def glyph(g : G) : |[String] = g.current.but(Neutral)./(_.style + "-glyph")
    override def glyph(f : F) : |[String] = |(f.style + "-glyph")
    // The rankings' tiers, after the rootelo ladder's (asked for on 2026-11-19), 200 lower as ratings here start at 1000
    // not 1200; the suits' card icons for the four suited tiers
    override def rankingTiers = $(
        RankingTier("Stag", |(1400), "Legend of the woods", None),
        RankingTier("Bird", |(1300), "Rules the canopy", |("card-suit-bird-gray")),
        RankingTier("Fox", |(1200), "Sly and seasoned", |("card-suit-fox-gray")),
        RankingTier("Rabbit", |(1100), "Quick on their feet", |("card-suit-rabbit-gray")),
        RankingTier("Mouse", |(1000), "Holding their ground", |("card-suit-mouse-gray")),
        RankingTier("Squirrel", None, "Still gathering acorns", None))
    override def glyph(g : G, f : F) : |[String] = glyph(f).%!(_ => g.highlightFaction.has(f) && hrf.HRF.uptime() / 1000 % 2 == 1)

    def createGame(factions : $[Faction], options : $[O]) = new Game(factions, factions, options)

    // Rankings (asked for on 2026-11-18): every Root game (the standard setup, Advanced Setup and Mirror) goes to one ladder; each
    // faction is a side, so a Vagabond in a coalition wins beside its ally
    override def ranking = |("root")

    // the tournament's tables are Advanced Setup games with the tournament draft (root/tournament.scala)
    override def tournament = |("root-adset")
    // the games list (overview in meta.scala): the map, then the factions in their seating order
    override def overviewIcons = $(AutumnMap -> "rm_autumn", WinterMap -> "rm_winter", LakeMap -> "rm_lake", MountainMap -> "rm_mountain",
        GorgeMap -> "rm_gorge", MarshMap -> "rm_marsh", TidalMap -> "rm_tidal", TundraMap -> "rm_tundra", GloomMap -> "rm_gloom",
        UrbanMap -> "rm_urban", LegendsMap -> "rm_legendsmap", DeepWoodsMap -> "rm_deepwoods", WastelandsMap -> "rm_wastelands")./((o, i) => writeOption(o) -> i)
    override def overview(g : G) = overviewOf(g)
    // the tournament's status posts (MetaGame.status): the round and each faction's VP ("D" for a dominance), by its seat
    def statusOf(g : Game, seat : Faction => String) : $[String] = {
        implicit val game = g
        // seat order: game.factions turns with each turn, game.ordering keeps the order set up
        val fs = g.ordering.toList.sortBy(_._1).map(_._2).some.|(g.factions).%(g.states.contains)
        $("r" + (if (g.turn > 0 && fs.any) (g.turn - 1) / fs.num + 1 else 0)) ++ fs./(f => seat(f) + ":" + f.name.replace(' ', '_') + ":" + (if (f.vp < 0) "D" else f.vp.toString))
    }
    override def status(g : G) = statusOf(g, f => writeFaction(f))

    // and in Advanced Setup the drawn factions no seat took, grayed out ("off:" before their glyph, hrf.scala; 2026-11-22, asked for)
    def overviewOf(g : Game) : $[String] = {
        val picked = g.ordering.toList.sortBy(_._1).map(_._2).some.|(g.factions)
        overviewOptions(g.options./(writeOption)) ++ picked./~(f => glyph(f)).distinct ++ undrafted(g)./~(f => glyph(f))./("off:" + _).distinct
    }

    // Advanced Setup's drawn factions no seat has taken (the one left once the draft is done)
    def undrafted(g : Game) : $[Faction] = g.drawn.diff(g.ordering.values.$).diff(g.factions)

    // the draft stats (MetaGame.rankingDraft): each faction left, named as rankSeat names it (a Vagabond with its character)
    def rankDraft(g : Game) : $[(String, String)] = undrafted(g)./(f => "U" -> f.as[Hero]./~(h => g.chars.get(h))./(c => f.name + " (" + c.name + ")").|(f.name))
    // the landmark and hireling stats (MetaGame.rankingSetup; 2026-11-23, asked for): the landmarks on the map and the hirelings in the game
    // and the map (2026-11-23, asked for): the tournament's picked one, else the game's map option (Random Official is drawn before the game)
    def rankSetup(g : Game) : $[(String, String)] =
        (g.tournamentMap.$ ++ g.options.of[MapOption]).take(1).map(m => "M" -> m.valueOn.text) ++
        $("Ferry" -> g.ferry, "Tower" -> g.tower, "Lost City" -> g.lostCity, "Mousehold" -> g.mousehold, "Foxburrow" -> g.foxburrow, "Rabbittown" -> g.rabbittown,
            "Forge" -> g.simpleForge, "Market" -> g.simpleMarket, "Birdsong Tower" -> g.birdsongTower, "Royal Tree" -> g.royalTree).filter(_._2.nonEmpty).map(x => "K" -> x._1) ++
        g.hirelings./(h => "H" -> h.name)
    override def rankingSetup(g : G) = rankSetup(g)
    override def rankingSides(g : G, winners : $[Any]) = rankSides(g, winners)
    override def rankingSeat(g : G, f : F) = rankSeat(g, f)
    override def rankingFan(g : G) = rankFan(g)

    // fan content (rankingFan): a faction that isn't an official one (fan, playtest and mirror factions), a fan map or deck, the
    // (Capitalists and Anarchists is among the official Decks since 2026-11-21, so it counts as official)
    // Doomed Vagabonds, Knave Captains, Turnlings or Legends Scenarios, or the Action! Deck Booster (the Simple Landmarks are rated since 2026-11-23)
    // and a game picked as unranked (UnrankedGame, 2026-11-22)
    def rankFan(game : Game) : Boolean = game.options.has(UnrankedGame) || game.factions.exists(approved(_).not) || game.options.exists(unapproved)

    // approved for ranked play: the official factions, Homeland's too, and the Negabond, which plays as a second Vagabond (owner, 2026-11-23)
    def approved(f : Faction) = (official ++ homeland ++ $(NB)).has(f)

    // an option not approved for ranked play
    def unapproved(o : GameOption) : Boolean = o match {
        // Might and Right, Mixed and Dawn and Dusk sit under Fan Decks on the setup screen; approved (Dawn and Dusk since 2026-11-22)
        case StandardDeck | MixedDeck | DuskDeck => false
        // the fan maps Tidal Flats, Tundra and Gloom are approved for ranked play (owner, 2026-11-22)
        case TidalMap | TundraMap | GloomMap => false
        case _ : FanMap | _ : FanDeck => true
        // the Simple Landmarks are approved for ranked play (the Birdsong Tower since 2026-11-20, the others since 2026-11-23, owner)
        case _ : SimpleLandmark => false
        case IncludeHireling(h) => turnlings.has(h)
        case o => $[GameOption](ActionBooster, DoomedVagabonds, DoomedCaptains, TurnOverHirelings, DeepWoodsSettling, WastelandsWater).has(o) || legendsOptions.has(o)
    }

    // the setup screen and the Discord setup panel mark these: the options above, and the fan factions in Advanced Setup's pool
    def unapprovedPick(o : GameOption) : Boolean = o match {
        case IncludeFaction(f) => approved(f).not
        case o => unapproved(o)
    }
    override def rankingUnapproved(o : O) = unapprovedPick(o)

    // a ranked game with something not approved for ranked play is refused (the setup screen's Start), naming them
    def rankedCheck(factions : $[Faction], options : $[GameOption]) : |[ValidationResult] = options.has(RankedGame).??{
        val why = (factions.%(approved(_).not)./(_.name) ++ options.%(unapprovedPick)./{
            case IncludeFaction(f) => f.name
            case o => o.valueOn.text
        }).distinct
        why.any.?(ErrorResult("Not approved for ranked play: " + why.mkString(", ") + ". Pick Unranked to play with " + (why.num > 1).?("them").|("it")))
    }

    def rankSides(game : Game, winners : $[Any]) : $[($[Faction], Boolean)] = game.factions./(f => $(f) -> winners.contains(f))

    // the faction (a Vagabond with its character), its place in the turn order set up and its VP ("D" for a dominance)
    def rankSeat(implicit game : Game, f : Faction) : |[RankingSeat] = game.factions.has(f).?{
        val name = f.as[Hero]./~(h => game.states.get(h)./~(_.as[HeroPlayer])./~(_.character))./(c => f.name + " (" + c.name + ")").|(f.name)
        // game.factions turns with each turn; game.ordering keeps the order set up
        val seat = game.ordering.find(_._2 == f)./(_._1).|(game.factions.indexOf(f))
        RankingSeat(name, seat + 1, game.factions.num, |((f.vp < 0).?("D").|(f.vp.toString)))
    }

    def getBots(f : Faction) = f match {
        case _ : Feline => $("Easy")
        case _ : Aviary => $("Easy")
        case _ : Insurgent => $("Easy")
        case _ : Hero => $("Easy")
        case _ => $("None")
    }

    def defaultBot(f : Faction) = "Easy"

    def getBot(f : Faction, b : String) = (f, b) match {
        case (f : Faction, "Easy") => new BotXX(f)
        case (f : Faction, "None") => new BotXX(f)
        case (f : Faction, _) => new BotXX(f)
        case (f : Faction, "Normal") => new BotTT(f, 1, 6, 3, o => new BotXX(o))
    }

    override def turnReason(g : G, f : Any, actions : $[Any]) : String = TurnReasons.of(g, f, actions)
    override def jests(g : G) = Jests.report(g, f => writeFaction(f))

    def writeFaction(f : Faction) = f.short
    def parseFaction(s : String) = (factions ++ retiredFactions ++ hirelings ++ turnlings).%(_.short == s).single || (s == "XCMDM").?(Mudmen(XC)) || $[Faction](InfectedHorde, TrapLayer, LegendBeast, GuanoMess).%(_.short == s).single || hirelings.%(_.toString == s).single

    def writeOption(o : O) = Serialize.write(o)
    def parseOption(s : String) = $(options.find(o => writeOption(o) == s) || options.find(o => o.toString == s) | (s @@ {
        case "AllHirelings" => SelectedHirelings
        case _ => UnknownOption(s)
    }))

    def parseAction(s : String) : Action = Serialize.parseAction(s)
    def writeAction(a : Action) : String = Serialize.write(a)

    val start = StartAction(gaming.version)

    override def bodyFont = Some("luminari")

    val assets =
    ConditionalAssetsList((factions, options) => true, "transport")(
        ImageAsset("riverboat"            ) ::
        ImageAsset("swimmers"             ) ::
        ImageAsset("tunnel"               ) ::
        ImageAsset("balloon"              ) ::
        ImageAsset("forest"               ) ::
    $) ::
    ConditionalAssetsList((factions, options) => true, "feature")(
        ImageAsset("ruins"                ) ::
        ImageAsset("ferry"                ) ::
        ImageAsset("tower"                ) ::
        ImageAsset("lost-city"            ) ::
        ImageAsset("simple-forge"         ) ::
        ImageAsset("simple-market"        ) ::
        ImageAsset("royal-tree"           ) ::
        ImageAsset("mousehold"            ) ::
        ImageAsset("foxburrow"            ) ::
        ImageAsset("rabbittown"           ) ::


        ImageAsset("clearing-highlight-placement" ) ::
        ImageAsset("clearing-highlight-battle"    ) ::
        ImageAsset("clearing-highlight-nuke"      ) ::

        ImageAsset("flag-cloth"           ) ::
        ImageAsset("flag-cloth-detail"    ) ::
        ImageAsset("flag-pole"            ) ::
        ImageAsset("flag-bg-alliance"  ) ::
        ImageAsset("flag-bg-badgers"   ) ::
        ImageAsset("flag-bg-bats"      ) ::
        ImageAsset("flag-bg-birds"     ) ::
        ImageAsset("flag-bg-cats"      ) ::
        ImageAsset("flag-bg-crows"     ) ::
        ImageAsset("flag-bg-frogs"     ) ::
        ImageAsset("flag-bg-knaves"    ) ::
        ImageAsset("flag-bg-lizards"   ) ::
        ImageAsset("flag-bg-moles"     ) ::
        ImageAsset("flag-bg-otters"    ) ::
        ImageAsset("flag-bg-rats"      ) ::
        ImageAsset("flag-bg-vagabond"  ) ::

        ImageAsset("clearing-suit-fox"    ) ::
        ImageAsset("clearing-suit-rabbit" ) ::
        ImageAsset("clearing-suit-mouse"  ) ::
        ImageAsset("clearing-suit-frog"   ) ::
        ImageAsset("clearing-suit-frog-hl") ::

        ImageAsset("clearing-suit-fox-off"    ) ::
        ImageAsset("clearing-suit-rabbit-off" ) ::
        ImageAsset("clearing-suit-mouse-off"  ) ::

        ImageAsset("dark-trap-token"   ) ::
        ImageAsset("dark-knight-token" ) ::
        ImageAsset("scorched-earth-1"  ) ::
        ImageAsset("scorched-earth-2"  ) ::
        ImageAsset("scorched-earth-3"  ) ::
        ImageAsset("scorched-earth-4"  ) ::
        ImageAsset("scorched-earth-5"  ) ::
        ImageAsset("scorched-earth-6"  ) ::
    $) ::
    ConditionalAssetsList((factions, options) => true, "feature/building")(
        ImageAsset("cross"                ) ::
        ImageAsset("empty-building"       ) ::
        ImageAsset("empty-building-card"  ) ::
        ImageAsset("empty-building-1"     ) ::
        ImageAsset("empty-building-2"     ) ::
        ImageAsset("empty-building-3"     ) ::
        ImageAsset("empty-building-4"     ) ::
        ImageAsset("empty-building-5"     ) ::
        ImageAsset("empty-building-1-card") ::
        ImageAsset("empty-building-2-card") ::
        ImageAsset("empty-building-3-card") ::
        ImageAsset("empty-building-4-card") ::
        ImageAsset("empty-building-5-card") ::
        ImageAsset("empty-building-1p"    ) ::
        ImageAsset("empty-building-2p"    ) ::
        ImageAsset("empty-building-cost-0") ::
        ImageAsset("empty-building-cost-1") ::
        ImageAsset("empty-building-cost-2") ::
        ImageAsset("empty-building-cost-3") ::
        ImageAsset("empty-building-cost-4") ::
    $) ::
    ConditionalAssetsList((factions, options) => true, "feature/token")(
        ImageAsset("empty-token"          ) ::
        ImageAsset("empty-token-card"     ) ::
        ImageAsset("empty-token-1"        ) ::
        ImageAsset("empty-token-2"        ) ::
        ImageAsset("empty-token-3"        ) ::
        ImageAsset("empty-token-4"        ) ::
        ImageAsset("empty-token-5"        ) ::
        ImageAsset("empty-token-2-card"   ) ::
        ImageAsset("empty-token-cost-1"   ) ::
        ImageAsset("empty-token-cost-2"   ) ::
        ImageAsset("empty-token-cost-3"   ) ::
        ImageAsset("empty-token-fox"      ) ::
        ImageAsset("empty-token-rabbit"   ) ::
        ImageAsset("empty-token-mouse"    ) ::
        ImageAsset("empty-token-bird"     ) ::
        ImageAsset("empty-token-anysuit"  ) ::
        ImageAsset("empty-token-militant"      ) ::
        ImageAsset("empty-token-militant-card" ) ::
    $) ::
    ConditionalAssetsList((factions, options) => true, "feature/text", "text-tint:")(
        ImageAsset("fox") ::
        ImageAsset("rabbit") ::
        ImageAsset("mouse") ::
        ImageAsset("frog") ::
        ImageAsset("fox-rabbit") ::
        ImageAsset("fox-mouse") ::
        ImageAsset("rabbit-mouse") ::
        ImageAsset("fox-frog") ::
        ImageAsset("rabbit-frog") ::
        ImageAsset("mouse-frog") ::
        ImageAsset("fox-rabbit-mouse") ::
        ImageAsset("fox-rabbit-frog") ::
        ImageAsset("fox-mouse-frog") ::
        ImageAsset("rabbit-mouse-frog") ::
        ImageAsset("fox-rabbit-mouse-frog") ::
        ImageAsset("frog-hl") ::
        ImageAsset("fox-frog-hl") ::
        ImageAsset("rabbit-frog-hl") ::
        ImageAsset("mouse-frog-hl") ::
        ImageAsset("fox-rabbit-frog-hl") ::
        ImageAsset("fox-mouse-frog-hl") ::
        ImageAsset("rabbit-mouse-frog-hl") ::
        ImageAsset("fox-rabbit-mouse-frog-hl") ::
    $) ::
    ConditionalAssetsList((factions, options) => true, "icon/craft")(
        ImageAsset("craft-suit-anysuit"  ) ::
        ImageAsset("craft-suit-fox"      ) ::
        ImageAsset("craft-suit-rabbit"   ) ::
        ImageAsset("craft-suit-mouse"    ) ::
        ImageAsset("craft-suit-frog"     ) ::
        ImageAsset("craft-suit-fox-frog"      ) ::
        ImageAsset("craft-suit-rabbit-frog"   ) ::
        ImageAsset("craft-suit-mouse-frog"    ) ::
        ImageAsset("craft-suit-fox-rabbit"    ) ::
        ImageAsset("craft-suit-fox-mouse"     ) ::
        ImageAsset("craft-suit-rabbit-mouse"  ) ::
        ImageAsset("craft-suit-fox-rabbit-frog"    ) ::
        ImageAsset("craft-suit-fox-mouse-frog"     ) ::
        ImageAsset("craft-suit-rabbit-mouse-frog"  ) ::
        ImageAsset("craft-suit-fox-rabbit-mouse"       ) ::
        ImageAsset("craft-suit-fox-rabbit-mouse-frog"  ) ::
    $) ::
    ConditionalAssetsList((factions, options) => true, "icon/card")(
        ImageAsset("card-back"             ) ::
        ImageAsset("card-back-frog"        ) ::
        ImageAsset("card-back-frog-hl"     ) ::
        ImageAsset("card-empty"            ) ::
        ImageAsset("card-separator"        ) ::

        ImageAsset("card-suit-bird-gray"    ) ::
        ImageAsset("card-suit-fox-gray"     ) ::
        ImageAsset("card-suit-rabbit-gray"  ) ::
        ImageAsset("card-suit-mouse-gray"   ) ::
        ImageAsset("card-suit-frog-gray"    ) ::
        ImageAsset("card-suit-anysuit-gray" ) ::
        ImageAsset("card-suit-abomination-gray" ) ::
        ImageAsset("card-suit-cursed-gray" ) ::

        ImageAsset("deck" ) ::
        ImageAsset("deck-frog" ) ::
        ImageAsset("deck-frog-hl" ) ::

        ImageAsset("pile-empty"  ) ::
        ImageAsset("pile-bird"   ) ::
        ImageAsset("pile-fox"    ) ::
        ImageAsset("pile-rabbit" ) ::
        ImageAsset("pile-mouse"  ) ::
        ImageAsset("pile-frog"   ) ::
        ImageAsset("pile-abomination" ) ::
        ImageAsset("pile-cursed" ) ::
    $) ::
    ConditionalAssetsList((factions, options) => true, "faction")(
        ImageAsset("action-black" ) ::
        ImageAsset("action-bird" ) ::
    $) ::
    // VP markers: the Homeland ones from the component sheet (root-factions/, page 17); the others are
    // that sheet's laurel frame on the faction colour with the faction head from the Root Database
    // (therootdatabase.com, /media/small_component_icons/custom/<animal>100.webp)
    ConditionalAssetsList((factions, options) => true, "vp")(
        ImageAsset("vp-mc") ::
        ImageAsset("vp-ed") ::
        ImageAsset("vp-wa") ::
        ImageAsset("vp-vb") ::
        ImageAsset("vp-rf") ::
        ImageAsset("vp-lc") ::
        ImageAsset("vp-ud") ::
        ImageAsset("vp-cc") ::
        ImageAsset("vp-lh") ::
        ImageAsset("vp-ki") ::
        // Mirror factions: the same heads recoloured (Negabond's in negative)
        ImageAsset("vp-bk") ::
        ImageAsset("vp-pe") ::
        ImageAsset("vp-fu") ::
        ImageAsset("vp-nb") ::
        ImageAsset("vp-cm") ::
        ImageAsset("vp-dr") ::
        ImageAsset("vp-ri") ::
        ImageAsset("vp-lk") ::
        ImageAsset("vp-tc") ::
        ImageAsset("vp-kd") ::
        ImageAsset("vp-ld") ::
    $) ::
    // The Legends Scenarios (root/legends.scala, art by root/tools/legends-art.py)
    ConditionalAssetsList((factions, options) => LegendsExpansion.on(options), "legends")(
        ImageAsset("space-suited") ::
        ImageAsset("space-unsuited") ::
        ImageAsset("monster-shadowmaw") ::
        ImageAsset("marker-shadowmaw") ::
        ImageAsset("monster-card-shadowmaw") ::
        ImageAsset("monster-card-shadowmaw-weapon") ::
        ImageAsset("monster-white-wolf") ::
        ImageAsset("marker-white-wolf") ::
        ImageAsset("monster-card-white-wolf") ::
        ImageAsset("monster-card-white-wolf-weapon") ::
        ImageAsset("monster-beaverbot") ::
        ImageAsset("marker-beaverbot") ::
        ImageAsset("monster-card-beaverbot") ::
        ImageAsset("monster-card-beaverbot-weapon") ::
        ImageAsset("monster-red-knight") ::
        ImageAsset("marker-red-knight") ::
        ImageAsset("monster-card-red-knight") ::
        ImageAsset("monster-card-red-knight-weapon") ::
        ImageAsset("monster-demon-bear") ::
        ImageAsset("marker-demon-bear") ::
        ImageAsset("monster-card-demon-bear") ::
        ImageAsset("monster-card-demon-bear-weapon") ::
        ImageAsset("marker-back") ::
        ImageAsset("die-1") ::
        ImageAsset("die-2") ::
        ImageAsset("die-3") ::
        ImageAsset("die-4") ::
        ImageAsset("die-5") ::
        ImageAsset("die-6") ::
    $) ::
    // The score and item trackers (the Board Trackers setting)
    ConditionalAssetsList((factions, options) => true, "tracker")(
        ImageAsset("score-track") ::
        ImageAsset("item-track") ::
    $) ::
    ConditionalAssetsList((factions, options) => true, "item")(
        ImageAsset("item-x-spacer"     ) ::
        ImageAsset("item-x-placeholder" ) ::
        ImageAsset("item-x-placeholder-0p" ) ::
        ImageAsset("item-x-placeholder-1p" ) ::
        ImageAsset("item-x-placeholder-2p" ) ::
        ImageAsset("item-x-placeholder-3p" ) ::
        ImageAsset("item-x-placeholder-4p" ) ::

        ImageAsset("item-boots"         ) ::
        ImageAsset("item-coins"         ) ::
        ImageAsset("item-sword"         ) ::
        ImageAsset("item-torch"         ) ::
        ImageAsset("item-bag"           ) ::
        ImageAsset("item-crossbow"      ) ::
        ImageAsset("item-hammer"        ) ::
        ImageAsset("item-teapot"        ) ::

        ImageAsset("item-boots-exhausted"     ) ::
        ImageAsset("item-coins-exhausted"     ) ::
        ImageAsset("item-sword-exhausted"     ) ::
        ImageAsset("item-torch-exhausted"     ) ::
        ImageAsset("item-bag-exhausted"       ) ::
        ImageAsset("item-crossbow-exhausted"  ) ::
        ImageAsset("item-hammer-exhausted"    ) ::
        ImageAsset("item-teapot-exhausted"    ) ::

        ImageAsset("item-boots-damaged"       ) ::
        ImageAsset("item-coins-damaged"       ) ::
        ImageAsset("item-sword-damaged"       ) ::
        ImageAsset("item-torch-damaged"       ) ::
        ImageAsset("item-bag-damaged"         ) ::
        ImageAsset("item-crossbow-damaged"    ) ::
        ImageAsset("item-hammer-damaged"      ) ::
        ImageAsset("item-teapot-damaged"      ) ::

        ImageAsset("item-boots-damaged-exhausted"     ) ::
        ImageAsset("item-coins-damaged-exhausted"     ) ::
        ImageAsset("item-sword-damaged-exhausted"     ) ::
        ImageAsset("item-torch-damaged-exhausted"     ) ::
        ImageAsset("item-bag-damaged-exhausted"       ) ::
        ImageAsset("item-crossbow-damaged-exhausted"  ) ::
        ImageAsset("item-hammer-damaged-exhausted"    ) ::
        ImageAsset("item-teapot-damaged-exhausted"    ) ::

        ImageAsset("item-boots-empty"     ) ::
        ImageAsset("item-coins-empty"     ) ::
        ImageAsset("item-sword-empty"     ) ::
        ImageAsset("item-torch-empty"     ) ::
        ImageAsset("item-bag-empty"       ) ::
        ImageAsset("item-crossbow-empty"  ) ::
        ImageAsset("item-hammer-empty"    ) ::
        ImageAsset("item-teapot-empty"    ) ::

        ImageAsset("item-club"                      ) ::
        ImageAsset("item-club-exhausted"            ) ::
        ImageAsset("item-club-damaged"              ) ::
        ImageAsset("item-club-damaged-exhausted"    ) ::
        ImageAsset("item-club-empty"    ) ::

        ImageAsset("item-bookofnames"                   ) ::
        ImageAsset("item-bookofnames-exhausted"         ) ::
        ImageAsset("item-bookofnames-damaged"           ) ::
        ImageAsset("item-bookofnames-damaged-exhausted" ) ::
        ImageAsset("item-bookofnames-empty"             ) ::
        ImageAsset("item-rabbitspaw"                    ) ::
        ImageAsset("item-rabbitspaw-exhausted"          ) ::
        ImageAsset("item-rabbitspaw-damaged"            ) ::
        ImageAsset("item-rabbitspaw-damaged-exhausted"  ) ::
        ImageAsset("item-rabbitspaw-empty"              ) ::
        ImageAsset("item-cursedidol"                    ) ::
        ImageAsset("item-cursedidol-exhausted"          ) ::
        ImageAsset("item-cursedidol-damaged"            ) ::
        ImageAsset("item-cursedidol-damaged-exhausted"  ) ::
        ImageAsset("item-cursedidol-empty"              ) ::

        ImageAsset("item-ring"                          ) ::
        ImageAsset("item-ring-exhausted"                ) ::
        ImageAsset("item-ring-damaged"                  ) ::
        ImageAsset("item-ring-damaged-exhausted"        ) ::
        ImageAsset("item-ring-empty"                    ) ::

        ImageAsset("item-mauler") ::
        ImageAsset("item-mauler-exhausted") ::
        ImageAsset("item-mauler-damaged") ::
        ImageAsset("item-mauler-damaged-exhausted") ::
        ImageAsset("item-mauler-empty") ::

        ImageAsset("item-spiritsword") ::
        ImageAsset("item-spiritsword-exhausted") ::
        ImageAsset("item-spiritsword-damaged") ::
        ImageAsset("item-spiritsword-damaged-exhausted") ::
        ImageAsset("item-spiritsword-empty") ::

        ImageAsset("item-chainsaw") ::
        ImageAsset("item-chainsaw-exhausted") ::
        ImageAsset("item-chainsaw-damaged") ::
        ImageAsset("item-chainsaw-damaged-exhausted") ::
        ImageAsset("item-chainsaw-empty") ::

        ImageAsset("item-slaymore") ::
        ImageAsset("item-slaymore-exhausted") ::
        ImageAsset("item-slaymore-damaged") ::
        ImageAsset("item-slaymore-damaged-exhausted") ::
        ImageAsset("item-slaymore-empty") ::

        ImageAsset("item-bushblazer") ::
        ImageAsset("item-bushblazer-exhausted") ::
        ImageAsset("item-bushblazer-damaged") ::
        ImageAsset("item-bushblazer-damaged-exhausted") ::
        ImageAsset("item-bushblazer-empty") ::

        ImageAsset("item-any"         ) ::
        ImageAsset("item-any-damaged" ) ::

    $) ::
    ConditionalAssetsList((factions, options) => true, "icon")(
        ImageAsset("swap"    ) ::
        ImageAsset("move"    ) ::
        ImageAsset("battle"  ) ::
        ImageAsset("remove"  ) ::
        ImageAsset("income"  ) ::
        ImageAsset("current"  ) ::

        ImageAsset("cog") ::

        ImageAsset("move-deg-0"    ) ::
        ImageAsset("move-deg-15"   ) ::
        ImageAsset("move-deg-30"   ) ::
        ImageAsset("move-deg-45"   ) ::
        ImageAsset("move-deg-60"   ) ::
        ImageAsset("move-deg-75"   ) ::
        ImageAsset("move-deg-90"   ) ::
        ImageAsset("move-deg-105"  ) ::
        ImageAsset("move-deg-120"  ) ::
        ImageAsset("move-deg-135"  ) ::
        ImageAsset("move-deg-150"  ) ::
        ImageAsset("move-deg-165"  ) ::
        ImageAsset("move-deg-180"  ) ::
        ImageAsset("move-deg-195"  ) ::
        ImageAsset("move-deg-210"  ) ::
        ImageAsset("move-deg-225"  ) ::
        ImageAsset("move-deg-240"  ) ::
        ImageAsset("move-deg-255"  ) ::
        ImageAsset("move-deg-270"  ) ::
        ImageAsset("move-deg-285"  ) ::
        ImageAsset("move-deg-300"  ) ::
        ImageAsset("move-deg-315"  ) ::
        ImageAsset("move-deg-330"  ) ::
        ImageAsset("move-deg-345"  ) ::
    $) ::
    ConditionalAssetsList((factions, options) => true, "card")(
        ImageAsset("card-back-art") ::
        ImageAsset("card-back-art-ep") ::
        ImageAsset("card-back-art-sd") ::
        ImageAsset("card-back-art-fd") ::
        ImageAsset("card-back-art-dd") ::
        ImageAsset("card-back-art-ur") ::
        ImageAsset("card-back-art-se") ::

    $) ::
    ConditionalAssetsList((factions, options) => true, "faction/hireling")(
        ImageAsset("ddi-infected"          ) ::
        ImageAsset("ddi-infected-empty"    ) ::

        // the Doomed Expansion's Turnlings, cut from its stickers by `root/tools/turnlings-art.py`
        ImageAsset("turnling-whisperer"    ) ::
        ImageAsset("turnling-cardinal"     ) ::
        ImageAsset("turnling-caravan"      ) ::
        ImageAsset("turnling-doomsayer"    ) ::
        ImageAsset("turnling-flame"        ) ::
        ImageAsset("turnling-vulture"      ) ::
        ImageAsset("turnling-bug"          ) ::
        ImageAsset("turnling-grunt"        ) ::
        ImageAsset("turnling-swine"        ) ::

        ImageAsset("stp-deer"              ) ::
        ImageAsset("stp-deer-empty"        ) ::

        ImageAsset("stb-weasel"            ) ::
        ImageAsset("stb-weasel-playing"    ) ::
        ImageAsset("stb-weasel-empty"      ) ::

        ImageAsset("bdg-porcupine"         ) ::
        ImageAsset("bdg-porcupine-empty"   ) ::

        ImageAsset("plb-rock-star"         ) ::
        ImageAsset("plb-rock-star-playing"   ) ::
        ImageAsset("plb-rock-star-empty"  , "stb-weasel-empty"     ) ::

        ImageAsset("fpr-elk"               ) ::

        ImageAsset("hwb-bandit"            ) ::

        ImageAsset("frp-stray-cat"         ) ::
        ImageAsset("frp-stray-cat-empty"   ) ::

        ImageAsset("ldn-vulture"           ) ::
        ImageAsset("ldn-vulture-empty"     ) ::

        ImageAsset("sup-animal"            ) ::
        ImageAsset("sup-animal-empty"      ) ::

        ImageAsset("exl-bear"              ) ::

        ImageAsset("rff-flotilla"          ) ::

        ImageAsset("wsp-chameleon"         ) ::
        ImageAsset("wsp-chameleon-empty"   ) ::

        ImageAsset("csp-magpie"            ) ::
        ImageAsset("csp-magpie-empty"      ) ::

        ImageAsset("swe-mouldwarp"         ) ::
        ImageAsset("swe-mouldwarp-empty"   ) ::
        ImageAsset("swe-foothold"          ) ::

        ImageAsset("vlk-vole"              ) ::
        ImageAsset("vlk-vole-empty"        ) ::
        ImageAsset("vlk-vault"             ) ::

        ImageAsset("fmb-varmint"           ) ::
        ImageAsset("fmb-varmint-empty"     ) ::

        ImageAsset("sad-advocate"          ) ::
        ImageAsset("sad-advocate-empty"    ) ::

        ImageAsset("rvr-roamer"            ) ::
        ImageAsset("rvr-roamer-empty"      ) ::
        ImageAsset("rvr-lilypad"           ) ::

        ImageAsset("pfr-farmer"            ) ::
        ImageAsset("pfr-farmer-empty"      ) ::
        ImageAsset("pfr-farm"              ) ::
        ImageAsset("sfr-farmer"            ) ::
        ImageAsset("sfr-farmer-empty"      ) ::
        ImageAsset("sfr-farm"              ) ::

        // the hireling markers on the score track (4, 8, 12; the back once taken), cut from the TTS mod's textures
        ImageAsset("hireling-tab-4"        ) ::
        ImageAsset("hireling-tab-8"        ) ::
        ImageAsset("hireling-tab-12"       ) ::
        ImageAsset("hireling-tab-back"     ) ::
        ImageAsset("hireling-marker-4"     ) ::
        ImageAsset("hireling-marker-8"     ) ::
        ImageAsset("hireling-marker-12"    ) ::
        ImageAsset("hireling-marker-back"  ) ::
    $) ::
    // the hireling cards, both sides (staunch on the front, demoted on the back), from the TTS mod "Root - Ultimate Collection"
    ConditionalAssetsList((factions, options) => true, "faction/hireling/card", "hireling-card:", lzy = Laziness.OnDemand)(
        (Meta.hirelings ++ Meta.turnlings)./(h => ImageAsset(h.style + "-card"))
    ) ::
    // the landmark cards, from the Leder Card Library (https://cards.ledergames.com/), shown when a landmark on the map is tapped
    ConditionalAssetsList((factions, options) => true, "card/landmark", "landmark-card:", lzy = Laziness.OnDemand)(
        $("ferry", "lost-city", "tower", "mousehold", "foxburrow", "rabbittown")./(ImageAsset(_))
    ) ::
    ConditionalAssetsList((factions, options) => options.has(AutumnMap) || options.has(TournamentDraft), "autumn", "autumn:")(
        ImageAsset("map"          ,    "map-bright-new-expand" ) ::
        ImageAsset("map-regions"  ).makeLossless ::
        ImageAsset("map-water"    ).makeLossless ::
        ImageAsset("map-woods"  ) ::

        ImageAsset("building-slot", "empty-building-white") ::

        ImageAsset("clearing-name-beach"           ) ::
        ImageAsset("clearing-name-creek"           ) ::
        ImageAsset("clearing-name-dune"            ) ::
        ImageAsset("clearing-name-glade"           ) ::
        ImageAsset("clearing-name-haven"           ) ::
        ImageAsset("clearing-name-hill"            ) ::
        ImageAsset("clearing-name-meadow"          ) ::
        ImageAsset("clearing-name-mountain"        ) ::
        ImageAsset("clearing-name-pond"            ) ::
        ImageAsset("clearing-name-quarry"          ) ::
        ImageAsset("clearing-name-waterfall"       ) ::
        ImageAsset("clearing-name-weald"           ) ::

        ImageAsset("path-beach-quarry"          ) ::
        ImageAsset("path-creek-quarry"          ) ::
        ImageAsset("path-dune-hill"             ) ::
        ImageAsset("path-glade-beach"           ) ::
        ImageAsset("path-glade-dune"            ) ::
        ImageAsset("path-glade-pond"            ) ::
        ImageAsset("path-glade-waterfall"       ) ::
        ImageAsset("path-haven-dune"            ) ::
        ImageAsset("path-haven-glade"           ) ::
        ImageAsset("path-haven-meadow"          ) ::
        ImageAsset("path-hill-beach"            ) ::
        ImageAsset("path-hill-creek"            ) ::
        ImageAsset("path-meadow-pond"           ) ::
        ImageAsset("path-mountain-quarry"       ) ::
        ImageAsset("path-pond-weald"            ) ::
        ImageAsset("path-waterfall-mountain"    ) ::
        ImageAsset("path-weald-mountain"        ) ::
        ImageAsset("path-weald-waterfall"       ) ::
    $) ::
    ConditionalAssetsList((factions, options) => options.has(WinterMap) || options.has(TournamentDraft), "winter", "winter:")(
        ImageAsset("map"          ,    "map-bright" ) ::
        ImageAsset("map-regions"  ).makeLossless ::
        ImageAsset("map-water"    ).makeLossless ::
        ImageAsset("map-woods"  ) ::

        ImageAsset("building-slot", "empty-building-black") ::

        ImageAsset("clearing-name-bend"         ) ::
        ImageAsset("clearing-name-dale"         ) ::
        ImageAsset("clearing-name-drift"        ) ::
        ImageAsset("clearing-name-ford"         ) ::
        ImageAsset("clearing-name-hedge"        ) ::
        ImageAsset("clearing-name-moor"         ) ::
        ImageAsset("clearing-name-mound"        ) ::
        ImageAsset("clearing-name-pit"          ) ::
        ImageAsset("clearing-name-rock"         ) ::
        ImageAsset("clearing-name-spire"        ) ::
        ImageAsset("clearing-name-trench"       ) ::
        ImageAsset("clearing-name-wade"         ) ::
    $) ::
    ConditionalAssetsList((factions, options) => options.has(LakeMap) || options.has(TournamentDraft), "lake", "lake:")(
        ImageAsset("map"          ,    "map-bright" ) ::
        ImageAsset("map-regions"  ).makeLossless ::
        ImageAsset("map-water"    ).makeLossless ::
        ImageAsset("map-woods"  ) ::

        ImageAsset("building-slot", "empty-building-white") ::

        ImageAsset("clearing-name-alley"   ) ::
        ImageAsset("clearing-name-bay"     ) ::
        ImageAsset("clearing-name-den"     ) ::
        ImageAsset("clearing-name-grove"   ) ::
        ImageAsset("clearing-name-gulf"    ) ::
        ImageAsset("clearing-name-lawn"    ) ::
        ImageAsset("clearing-name-marsh"   ) ::
        ImageAsset("clearing-name-prairie" ) ::
        ImageAsset("clearing-name-shade"   ) ::
        ImageAsset("clearing-name-shoal"   ) ::
        ImageAsset("clearing-name-vert"    ) ::
        ImageAsset("clearing-name-yard"    ) ::
    $) ::
    ConditionalAssetsList((factions, options) => options.has(MountainMap) || options.has(TournamentDraft), "mountain", "mountain:")(
        ImageAsset("map"          ,    "map-bright" ) ::
        ImageAsset("map-regions"  ).makeLossless ::
        ImageAsset("map-water"    ).makeLossless ::
        ImageAsset("map-woods"  ) ::

        ImageAsset("building-slot", "empty-building-white") ::

        ImageAsset("clearing-name-brim"       ) ::
        ImageAsset("clearing-name-cliff"      ) ::
        ImageAsset("clearing-name-crest"      ) ::
        ImageAsset("clearing-name-drain"      ) ::
        ImageAsset("clearing-name-ledge"      ) ::
        ImageAsset("clearing-name-mine"       ) ::
        ImageAsset("clearing-name-pass"       ) ::
        ImageAsset("clearing-name-peak"       ) ::
        ImageAsset("clearing-name-ramp"       ) ::
        ImageAsset("clearing-name-ridge"      ) ::
        ImageAsset("clearing-name-slope"      ) ::
        ImageAsset("clearing-name-valley"     ) ::

        ImageAsset("rubble-mine-peak"  ) ::
        ImageAsset("rubble-ledge-mine"  ) ::
        ImageAsset("rubble-brim-ledge"  ) ::
        ImageAsset("rubble-ramp-valley"  ) ::
        ImageAsset("rubble-valley-ridge"  ) ::
        ImageAsset("rubble-cliff-crest"  ) ::
    $) ::
    ConditionalAssetsList((factions, options) => options.has(TidalMap), "tidal", "tidal:")(
        ImageAsset("map"          ,    "map-bright" ) ::
        ImageAsset("map-regions"  ).makeLossless ::
        ImageAsset("map-woods"  ) ::

        ImageAsset("flood"  ) ::
        ImageAsset("flood-north"  ) ::
        ImageAsset("flood-south-east"  ) ::
        ImageAsset("flood-south-west"  ) ::

        ImageAsset("building-slot", "empty-building-white") ::

        ImageAsset("clearing-name-barn" ) ::
        ImageAsset("clearing-name-big-tree" ) ::
        ImageAsset("clearing-name-cottage" ) ::
        ImageAsset("clearing-name-house-boat" ) ::
        ImageAsset("clearing-name-inn" ) ::
        ImageAsset("clearing-name-north-glen" ) ::
        ImageAsset("clearing-name-old-dock" ) ::
        ImageAsset("clearing-name-shanty-town" ) ::
        ImageAsset("clearing-name-stilts" ) ::
        ImageAsset("clearing-name-tree-house" ) ::
        ImageAsset("clearing-name-twin-pines" ) ::
        ImageAsset("clearing-name-underpass" ) ::
        ImageAsset("clearing-name-wetlands" ) ::
    $) ::
    ConditionalAssetsList((factions, options) => options.has(TundraMap), "tundra", "tundra:")(
        ImageAsset("map"          ,    "map-bright" ) ::
        ImageAsset("map-regions"  ).makeLossless ::
        ImageAsset("map-water"    ).makeLossless ::
        ImageAsset("map-woods"  ) ::

        ImageAsset("blizzard"  ) ::
        ImageAsset("blizzard-mark"  ) ::

        ImageAsset("building-slot", "empty-building-black") ::

        ImageAsset("clearing-name-cliffside" ) ::
        ImageAsset("clearing-name-glacial-vale" ) ::
        ImageAsset("clearing-name-mountain-pass" ) ::
        ImageAsset("clearing-name-frozen-hollow" ) ::
        ImageAsset("clearing-name-cold-creek" ) ::
        ImageAsset("clearing-name-lone-cabin" ) ::
        ImageAsset("clearing-name-mansion" ) ::
        ImageAsset("clearing-name-river-bend" ) ::
        ImageAsset("clearing-name-old-farm" ) ::
        ImageAsset("clearing-name-seaside" ) ::
        ImageAsset("clearing-name-hovel" ) ::
        ImageAsset("clearing-name-deep-woods" ) ::
    $) ::
    ConditionalAssetsList((factions, options) => options.has(MarshMap) || options.has(TournamentDraft), "marsh", "marsh:")(
        // new file names for the taller map (the new art shows more of the bottom), so browsers don't keep the cached ones
        ImageAsset("map"          , "map-art" ) ::
        ImageAsset("map-regions"  , "map-regions-2250").makeLossless ::
        ImageAsset("map-water"    ).makeLossless ::
        // no "map-woods": the Marsh's was empty (fully transparent), and iPhones failed to load it however it was saved

        ImageAsset("building-slot", "empty-building-white") ::

        ImageAsset("flood-weir" ) ::
        ImageAsset("flood-mudflat" ) ::
        ImageAsset("flood-mire" ) ::
        ImageAsset("flood-fen" ) ::
        ImageAsset("flood-bulrush" ) ::
        ImageAsset("flood-peat" ) ::

        ImageAsset("clearing-name-thatch" ) ::
        ImageAsset("clearing-name-jetty" ) ::
        ImageAsset("clearing-name-bayou" ) ::
        ImageAsset("clearing-name-delta" ) ::
        ImageAsset("clearing-name-sedge" ) ::
        ImageAsset("clearing-name-weir" ) ::
        ImageAsset("clearing-name-heron" ) ::
        ImageAsset("clearing-name-mudflat" ) ::
        ImageAsset("clearing-name-cattail" ) ::
        ImageAsset("clearing-name-bulrush" ) ::
        ImageAsset("clearing-name-peat" ) ::
        ImageAsset("clearing-name-mire" ) ::
        ImageAsset("clearing-name-fen" ) ::
        ImageAsset("clearing-name-confluence" ) ::
        ImageAsset("clearing-name-crossing" ) ::
    $) ::
    ConditionalAssetsList((factions, options) => options.has(GloomMap), "gloom", "gloom:")(
        ImageAsset("map"          , "map-bright" ) ::
        ImageAsset("map-regions"  ).makeLossless ::
        ImageAsset("map-water"    ).makeLossless ::
        ImageAsset("map-woods"    ) ::

        ImageAsset("building-slot", "empty-building-white") ::

        ImageAsset("clearing-name-lower-jaw" ) ::
        ImageAsset("clearing-name-upper-jaw" ) ::
        ImageAsset("clearing-name-cork-isle" ) ::
        ImageAsset("clearing-name-auberge" ) ::
        ImageAsset("clearing-name-deadwing" ) ::
        ImageAsset("clearing-name-cemetery" ) ::
        ImageAsset("clearing-name-runestone" ) ::
        ImageAsset("clearing-name-haunted" ) ::
        ImageAsset("clearing-name-depot" ) ::
        ImageAsset("clearing-name-bog" ) ::
        ImageAsset("clearing-name-tailbone" ) ::
        ImageAsset("clearing-name-effigy" ) ::
    $) ::
    ConditionalAssetsList((factions, options) => options.has(UrbanMap), "urban", "urban:")(
        ImageAsset("map"          , "map-art" ) ::
        ImageAsset("map-regions"  ).makeLossless ::
        ImageAsset("map-water"    ).makeLossless ::

        ImageAsset("building-slot", "empty-building-white") ::

        ImageAsset("clearing-name-tollgate" ) ::
        ImageAsset("clearing-name-mill" ) ::
        ImageAsset("clearing-name-guildhall" ) ::
        ImageAsset("clearing-name-belfry" ) ::
        ImageAsset("clearing-name-tannery" ) ::
        ImageAsset("clearing-name-bazaar" ) ::
        ImageAsset("clearing-name-barracks" ) ::
        ImageAsset("clearing-name-wharf" ) ::
        ImageAsset("clearing-name-abbey" ) ::
        ImageAsset("clearing-name-sluice" ) ::
        ImageAsset("clearing-name-plaza" ) ::
        ImageAsset("clearing-name-arcade" ) ::
    $) ::
    ConditionalAssetsList((factions, options) => options.has(LegendsMap), "legends-map", "legends-map:")(
        ImageAsset("map"          , "map-art" ) ::
        ImageAsset("map-regions"  ).makeLossless ::
        ImageAsset("map-water"    ).makeLossless ::

        ImageAsset("building-slot", "empty-building-white") ::

        ImageAsset("clearing-name-thornkeep" ) ::
        ImageAsset("clearing-name-lumberyard" ) ::
        ImageAsset("clearing-name-stormkeep" ) ::
        ImageAsset("clearing-name-woodcutter" ) ::
        ImageAsset("clearing-name-lodge" ) ::
        ImageAsset("clearing-name-hollow" ) ::
        ImageAsset("clearing-name-watchtower" ) ::
        ImageAsset("clearing-name-smithy" ) ::
        ImageAsset("clearing-name-hamlet" ) ::
        ImageAsset("clearing-name-oakkeep" ) ::
        ImageAsset("clearing-name-hermitage" ) ::
        ImageAsset("clearing-name-redkeep" ) ::
    $) ::
    ConditionalAssetsList((factions, options) => options.has(DeepWoodsMap), "deep-woods", "deep-woods:")(
        ImageAsset("map"          , "map-art" ) ::
        ImageAsset("map-regions"  ).makeLossless ::
        ImageAsset("map-water"    ).makeLossless ::

        ImageAsset("building-slot", "empty-building-white") ::

        ImageAsset("clearing-name-thicket" ) ::
        ImageAsset("clearing-name-bramble" ) ::
        ImageAsset("clearing-name-fernbank" ) ::
        ImageAsset("clearing-name-mossgrove" ) ::
        ImageAsset("clearing-name-pinecrest" ) ::
        ImageAsset("clearing-name-briarfold" ) ::
        ImageAsset("clearing-name-hazeldell" ) ::
        ImageAsset("clearing-name-wildmere" ) ::
        ImageAsset("clearing-name-oldroot" ) ::
        ImageAsset("clearing-name-deepden" ) ::
        ImageAsset("clearing-name-shadowvale" ) ::
        ImageAsset("clearing-name-elderglen" ) ::
    $) ::
    // the Settling rules: each clearing as printed (unsettled) and the twelve clearing tiles
    ConditionalAssetsList((factions, options) => options.has(DeepWoodsMap) && options.has(DeepWoodsSettling), "deep-woods", "deep-woods:")(
        1.to(12).$./~(n => $(ImageAsset("tile-" + n), ImageAsset("unsettled-" + n)))
    ) ::
    ConditionalAssetsList((factions, options) => options.has(WastelandsMap), "wastelands", "wastelands:")(
        ImageAsset("map"          , "map-art" ) ::
        ImageAsset("map-regions"  ).makeLossless ::
        ImageAsset("map-water"    ).makeLossless ::

        ImageAsset("building-slot", "empty-building-white") ::

        ImageAsset("clearing-name-driftcamp" ) ::
        ImageAsset("clearing-name-scrapyard" ) ::
        ImageAsset("clearing-name-ashpit" ) ::
        ImageAsset("clearing-name-tollpost" ) ::
        ImageAsset("clearing-name-farmstead" ) ::
        ImageAsset("clearing-name-manor" ) ::
        ImageAsset("clearing-name-ferrytown" ) ::
        ImageAsset("clearing-name-dugout" ) ::
        ImageAsset("clearing-name-junkshop" ) ::
        ImageAsset("clearing-name-cistern" ) ::
        ImageAsset("clearing-name-gatehouse" ) ::
        ImageAsset("clearing-name-granary" ) ::
    $) ::
    ConditionalAssetsList((factions, options) => options.has(GorgeMap) || options.has(TournamentDraft), "gorge", "gorge:")(
        ImageAsset("map"          , "map-art" ) ::
        ImageAsset("map-regions"  ).makeLossless ::
        ImageAsset("map-water"    ).makeLossless ::
        // a new file name, so browsers don't keep the cached image with the lowercase "e" label
        ImageAsset("map-woods"    , "map-woods-e") ::

        ImageAsset("building-slot", "empty-building-white") ::

        ImageAsset("clearing-name-ranch" ) ::
        ImageAsset("clearing-name-mesa" ) ::
        ImageAsset("clearing-name-camp" ) ::
        ImageAsset("clearing-name-chapel" ) ::
        ImageAsset("clearing-name-bluff" ) ::
        ImageAsset("clearing-name-saloon" ) ::
        ImageAsset("clearing-name-lookout" ) ::
        ImageAsset("clearing-name-rapids" ) ::
        ImageAsset("clearing-name-homestead" ) ::
        ImageAsset("clearing-name-forge" ) ::
        ImageAsset("clearing-name-fork" ) ::
        ImageAsset("clearing-name-pueblo" ) ::
    $) ::
    ConditionalAssetsList((factions, options) => factions.of[Feline].any, "faction/feline")(
        ImageAsset("castle"            ) ::
        ImageAsset("castle-m"          ) ::

        ImageAsset("mc-glyph"          ) ::
        ImageAsset("mc-title"          ) ::
        ImageAsset("mc-char"           ) ::
        ImageAsset("mc-cat"            ) ::
        ImageAsset("mc-cat-empty"      ) ::
        ImageAsset("mc-cat-x5"         ) ::
        ImageAsset("mc-cat-x5-empty"   ) ::
        ImageAsset("mc-sawmill"        ) ::
        ImageAsset("mc-workshop"       ) ::
        ImageAsset("mc-recruiter"      , "mc-recruiter-new") ::
        ImageAsset("mc-wood"           ) ::
        ImageAsset("mc-keep"           ) ::
        ImageAsset("mc-catapult"       ) ::
        ImageAsset("mc-action"         ) ::

        ImageAsset("mc-skip"           ) ::
        ImageAsset("mc-skip-done"      ) ::
        ImageAsset("mc-ask"            ) ::
        ImageAsset("mc-ask-done"       ) ::
        ImageAsset("mc-one-plus"       ) ::
        ImageAsset("mc-one-plus-done"  ) ::
        ImageAsset("mc-two-plus"       ) ::
        ImageAsset("mc-two-plus-done"  ) ::

        ImageAsset("bk-glyph"          ) ::
        ImageAsset("bk-title"          ) ::
        ImageAsset("bk-char"           , "mc-char") ::
        ImageAsset("bk-cat"            ) ::
        ImageAsset("bk-cat-x5"         ) ::
        ImageAsset("bk-cat-empty"      , "mc-cat-empty") ::
        ImageAsset("bk-cat-x5-empty"   , "mc-cat-x5-empty") ::
        ImageAsset("bk-sawmill"        ) ::
        ImageAsset("bk-workshop"       ) ::
        ImageAsset("bk-recruiter"      , "bk-recruiter-new") ::
        ImageAsset("bk-wood"           ) ::
        ImageAsset("bk-keep"           , "mc-keep") ::
        ImageAsset("bk-catapult"       ) ::
        ImageAsset("bk-action"         ) ::
    $) ::
    ConditionalAssetsList((factions, options) => factions.of[Aviary].any, "card", scale = 50)(
        ImageAsset("loyal-vizier") ::
    $) ::
    ConditionalAssetsList((factions, options) => factions.of[Aviary].any, "faction/aviary")(
        ImageAsset("ed-glyph"          ) ::
        ImageAsset("ed-title"          ) ::
        ImageAsset("ed-char"           ) ::
        ImageAsset("ed-hawk"           ) ::
        ImageAsset("ed-hawk-x5"        ) ::
        ImageAsset("ed-hawk-empty"     ) ::
        ImageAsset("ed-hawk-x5-empty"  ) ::
        ImageAsset("ed-roost"          ) ::

        ImageAsset("ed-recruit-bird"           ) ::
        ImageAsset("ed-recruit-fox"            ) ::
        ImageAsset("ed-recruit-rabbit"         ) ::
        ImageAsset("ed-recruit-mouse"          ) ::
        ImageAsset("ed-recruit-frog"           ) ::

        ImageAsset("ed-double-recruit-bird"      ) ::
        ImageAsset("ed-double-recruit-fox"       ) ::
        ImageAsset("ed-double-recruit-rabbit"    ) ::
        ImageAsset("ed-double-recruit-mouse"     ) ::
        ImageAsset("ed-double-recruit-frog"      ) ::

        ImageAsset("ed-move-bird"              ) ::
        ImageAsset("ed-move-fox"               ) ::
        ImageAsset("ed-move-rabbit"            ) ::
        ImageAsset("ed-move-mouse"             ) ::
        ImageAsset("ed-move-frog"              ) ::

        ImageAsset("ed-battle-bird"            ) ::
        ImageAsset("ed-battle-fox"             ) ::
        ImageAsset("ed-battle-rabbit"          ) ::
        ImageAsset("ed-battle-mouse"           ) ::
        ImageAsset("ed-battle-frog"            ) ::

        ImageAsset("ed-build-bird"             ) ::
        ImageAsset("ed-build-fox"              ) ::
        ImageAsset("ed-build-rabbit"           ) ::
        ImageAsset("ed-build-mouse"            ) ::
        ImageAsset("ed-build-frog"            ) ::

        ImageAsset("ed-recruit-bird-done"      ) ::
        ImageAsset("ed-recruit-fox-done"       ) ::
        ImageAsset("ed-recruit-rabbit-done"    ) ::
        ImageAsset("ed-recruit-mouse-done"     ) ::
        ImageAsset("ed-recruit-frog-done"      ) ::

        ImageAsset("ed-double-recruit-bird-done"      ) ::
        ImageAsset("ed-double-recruit-fox-done"       ) ::
        ImageAsset("ed-double-recruit-rabbit-done"    ) ::
        ImageAsset("ed-double-recruit-mouse-done"     ) ::
        ImageAsset("ed-double-recruit-frog-done"      ) ::

        ImageAsset("ed-move-bird-done"         ) ::
        ImageAsset("ed-move-fox-done"          ) ::
        ImageAsset("ed-move-rabbit-done"       ) ::
        ImageAsset("ed-move-mouse-done"        ) ::
        ImageAsset("ed-move-frog-done"         ) ::

        ImageAsset("ed-battle-bird-done"       ) ::
        ImageAsset("ed-battle-fox-done"        ) ::
        ImageAsset("ed-battle-rabbit-done"     ) ::
        ImageAsset("ed-battle-mouse-done"      ) ::
        ImageAsset("ed-battle-frog-done"       ) ::

        ImageAsset("ed-build-bird-done"        ) ::
        ImageAsset("ed-build-fox-done"         ) ::
        ImageAsset("ed-build-rabbit-done"      ) ::
        ImageAsset("ed-build-mouse-done"       ) ::
        ImageAsset("ed-build-frog-done"        ) ::


        ImageAsset("ed-builder"        ) ::
        ImageAsset("ed-charismatic"    ) ::
        ImageAsset("ed-commander"      ) ::
        ImageAsset("ed-despot"         ) ::

        ImageAsset("pe-glyph"          ) ::
        ImageAsset("pe-title"          ) ::
        ImageAsset("pe-char"           , "ed-char"          ) ::
        ImageAsset("pe-hawk"           ) ::
        ImageAsset("pe-hawk-x5"        ) ::
        ImageAsset("pe-hawk-empty"     , "ed-hawk-empty"    ) ::
        ImageAsset("pe-hawk-x5-empty"  , "ed-hawk-x5-empty" ) ::
        ImageAsset("pe-roost"          ) ::

        ImageAsset("pe-recruit-bird"               , "ed-recruit-bird"               ) ::
        ImageAsset("pe-recruit-fox"                , "ed-recruit-fox"                ) ::
        ImageAsset("pe-recruit-rabbit"             , "ed-recruit-rabbit"             ) ::
        ImageAsset("pe-recruit-mouse"              , "ed-recruit-mouse"              ) ::
        ImageAsset("pe-double-recruit-bird"        , "ed-double-recruit-bird"        ) ::
        ImageAsset("pe-double-recruit-fox"         , "ed-double-recruit-fox"         ) ::
        ImageAsset("pe-double-recruit-rabbit"      , "ed-double-recruit-rabbit"      ) ::
        ImageAsset("pe-double-recruit-mouse"       , "ed-double-recruit-mouse"       ) ::
        ImageAsset("pe-move-bird"                  , "ed-move-bird"                  ) ::
        ImageAsset("pe-move-fox"                   , "ed-move-fox"                   ) ::
        ImageAsset("pe-move-rabbit"                , "ed-move-rabbit"                ) ::
        ImageAsset("pe-move-mouse"                 , "ed-move-mouse"                 ) ::
        ImageAsset("pe-battle-bird"                , "ed-battle-bird"                ) ::
        ImageAsset("pe-battle-fox"                 , "ed-battle-fox"                 ) ::
        ImageAsset("pe-battle-rabbit"              , "ed-battle-rabbit"              ) ::
        ImageAsset("pe-battle-mouse"               , "ed-battle-mouse"               ) ::
        ImageAsset("pe-build-bird"                 , "ed-build-bird"                 ) ::
        ImageAsset("pe-build-fox"                  , "ed-build-fox"                  ) ::
        ImageAsset("pe-build-rabbit"               , "ed-build-rabbit"               ) ::
        ImageAsset("pe-build-mouse"                , "ed-build-mouse"                ) ::

        ImageAsset("pe-recruit-bird-done"          , "ed-recruit-bird-done"          ) ::
        ImageAsset("pe-recruit-fox-done"           , "ed-recruit-fox-done"           ) ::
        ImageAsset("pe-recruit-rabbit-done"        , "ed-recruit-rabbit-done"        ) ::
        ImageAsset("pe-recruit-mouse-done"         , "ed-recruit-mouse-done"         ) ::
        ImageAsset("pe-recruit-frog-done"          , "ed-recruit-frog-done"          ) ::
        ImageAsset("pe-double-recruit-bird-done"   , "ed-double-recruit-bird-done"   ) ::
        ImageAsset("pe-double-recruit-fox-done"    , "ed-double-recruit-fox-done"    ) ::
        ImageAsset("pe-double-recruit-rabbit-done" , "ed-double-recruit-rabbit-done" ) ::
        ImageAsset("pe-double-recruit-mouse-done"  , "ed-double-recruit-mouse-done"  ) ::
        ImageAsset("pe-double-recruit-frog-done"   , "ed-double-recruit-frog-done"   ) ::
        ImageAsset("pe-move-bird-done"             , "ed-move-bird-done"             ) ::
        ImageAsset("pe-move-fox-done"              , "ed-move-fox-done"              ) ::
        ImageAsset("pe-move-rabbit-done"           , "ed-move-rabbit-done"           ) ::
        ImageAsset("pe-move-mouse-done"            , "ed-move-mouse-done"            ) ::
        ImageAsset("pe-move-frog-done"             , "ed-move-frog-done"             ) ::
        ImageAsset("pe-battle-bird-done"           , "ed-battle-bird-done"           ) ::
        ImageAsset("pe-battle-fox-done"            , "ed-battle-fox-done"            ) ::
        ImageAsset("pe-battle-rabbit-done"         , "ed-battle-rabbit-done"         ) ::
        ImageAsset("pe-battle-mouse-done"          , "ed-battle-mouse-done"          ) ::
        ImageAsset("pe-battle-frog-done"           , "ed-battle-frog-done"           ) ::
        ImageAsset("pe-build-bird-done"            , "ed-build-bird-done"            ) ::
        ImageAsset("pe-build-fox-done"             , "ed-build-fox-done"             ) ::
        ImageAsset("pe-build-rabbit-done"          , "ed-build-rabbit-done"          ) ::
        ImageAsset("pe-build-mouse-done"           , "ed-build-mouse-done"           ) ::
        ImageAsset("pe-build-frog-done"            , "ed-build-frog-done"            ) ::

        ImageAsset("pe-builder"       , "ed-builder"        ) ::
        ImageAsset("pe-charismatic"   , "ed-charismatic"    ) ::
        ImageAsset("pe-commander"     , "ed-commander"      ) ::
        ImageAsset("pe-despot"        , "ed-despot"         ) ::
    $) ::
    ConditionalAssetsList((factions, options) => factions.of[Insurgent].any, "faction/insurgent")(
        ImageAsset("wa-glyph"         ) ::
        ImageAsset("wa-title"         ) ::
        ImageAsset("wa-char"          ) ::
        ImageAsset("wa-critter"       ) ::
        ImageAsset("wa-critter-empty" ) ::
        ImageAsset("wa-officer"       ) ::
        ImageAsset("wa-supporter"     ) ::
        ImageAsset("wa-sympathy"      ) ::
        ImageAsset("wa-base-fox"      ) ::
        ImageAsset("wa-base-rabbit"   ) ::
        ImageAsset("wa-base-mouse"    ) ::
        ImageAsset("wa-action"        ) ::

        ImageAsset("af-glyph"         ) ::
        ImageAsset("af-title"         ) ::
        ImageAsset("af-char"          , "wa-char"          ) ::
        ImageAsset("af-critter"       ) ::
        ImageAsset("af-critter-empty" , "wa-critter-empty" ) ::
        ImageAsset("af-officer"       , "wa-officer"       ) ::
        ImageAsset("af-supporter"     ) ::
        ImageAsset("af-sympathy"      ) ::
        ImageAsset("af-base-fox"      ) ::
        ImageAsset("af-base-rabbit"   ) ::
        ImageAsset("af-base-mouse"    ) ::
        ImageAsset("af-action"        ) ::

        ImageAsset("fu-glyph"         , "af-glyph"         ) ::
        ImageAsset("fu-title"         , "af-title"         ) ::
        ImageAsset("fu-char"          , "wa-char"          ) ::
        ImageAsset("fu-critter"       , "af-critter"       ) ::
        ImageAsset("fu-critter-empty" , "wa-critter-empty" ) ::
        ImageAsset("fu-officer"       , "wa-officer"       ) ::
        ImageAsset("fu-supporter"     , "af-supporter"     ) ::
        ImageAsset("fu-sympathy"      , "af-sympathy"      ) ::
        ImageAsset("fu-base-fox"      , "af-base-fox"      ) ::
        ImageAsset("fu-base-rabbit"   , "af-base-rabbit"   ) ::
        ImageAsset("fu-base-mouse"    , "af-base-mouse"    ) ::
        ImageAsset("fu-action"        , "af-action"        ) ::
    $) ::
    ConditionalAssetsList((factions, options) => factions.of[Trader].any, "faction/trader")(
        ImageAsset("rf-glyph"           ) ::
        ImageAsset("rf-title"           ) ::
        ImageAsset("rf-char"            ) ::
        ImageAsset("rf-otter"           ) ::
        ImageAsset("rf-otter-empty"     ) ::
        ImageAsset("rf-otter-x5"        ) ::
        ImageAsset("rf-otter-x5-empty"  ) ::

        ImageAsset("rf-trade-post-fox"    ) ::
        ImageAsset("rf-trade-post-rabbit" ) ::
        ImageAsset("rf-trade-post-mouse"  ) ::
        ImageAsset("rf-trade-post-fox-destroyed"    ) ::
        ImageAsset("rf-trade-post-rabbit-destroyed" ) ::
        ImageAsset("rf-trade-post-mouse-destroyed"  ) ::

        ImageAsset("rf-price-1") ::
        ImageAsset("rf-price-2") ::
        ImageAsset("rf-price-3") ::
        ImageAsset("rf-price-4") ::
        ImageAsset("rf-price-4-selected") ::
        ImageAsset("rf-price-3-selected") ::
        ImageAsset("rf-price-2-selected") ::
        ImageAsset("rf-price-1-selected") ::
        ImageAsset("rf-service-mercs") ::
        ImageAsset("rf-service-boats") ::
        ImageAsset("rf-service-cards") ::
    $) ::
    ConditionalAssetsList((factions, options) => factions.has(SF), "faction/trader")(
        ImageAsset("sf-glyph") ::
        ImageAsset("sf-title") ::
        ImageAsset("sf-char", "../hero/vb-char-harrier") ::

        ImageAsset("sf-squirrel"           ) ::
        ImageAsset("sf-squirrel-x5"        ) ::
        ImageAsset("sf-squirrel-empty"     ) ::
        ImageAsset("sf-squirrel-x5-empty"  ) ::

        ImageAsset("sf-trade-post-fox"    ) ::
        ImageAsset("sf-trade-post-rabbit" ) ::
        ImageAsset("sf-trade-post-mouse"  ) ::
        ImageAsset("sf-trade-post-fox-destroyed"    , "rf-trade-post-fox-destroyed"    ) ::
        ImageAsset("sf-trade-post-rabbit-destroyed" , "rf-trade-post-rabbit-destroyed" ) ::
        ImageAsset("sf-trade-post-mouse-destroyed"  , "rf-trade-post-mouse-destroyed"  ) ::

        ImageAsset("sf-price-1") ::
        ImageAsset("sf-price-2") ::
        ImageAsset("sf-price-3") ::
        ImageAsset("sf-price-4") ::
        ImageAsset("sf-price-4-selected") ::
        ImageAsset("sf-price-3-selected") ::
        ImageAsset("sf-price-2-selected") ::
        ImageAsset("sf-price-1-selected") ::
        ImageAsset("sf-service-cards") ::
        ImageAsset("sf-service-balloon") ::
        ImageAsset("sf-service-outsourcing") ::
        ImageAsset("sf-service-peacekeepers") ::
        ImageAsset("sf-service-shares") ::
        ImageAsset("sf-service-talent-scout") ::
    $) ::
    ConditionalAssetsList((factions, options) => factions.of[Fanatic].any, "faction/fanatic")(
        ImageAsset("lc-glyph"            ) ::
        ImageAsset("lc-title"            ) ::
        ImageAsset("lc-char"             ) ::
        ImageAsset("lc-lizard"           ) ::
        ImageAsset("lc-lizard-x5"        ) ::
        ImageAsset("lc-lizard-empty"     ) ::
        ImageAsset("lc-lizard-x5-empty"  ) ::
        ImageAsset("lc-acolyte"          ) ::
        ImageAsset("lc-acolyte-x5"       ) ::
        ImageAsset("lc-garden-fox"       ) ::
        ImageAsset("lc-garden-rabbit"    ) ::
        ImageAsset("lc-garden-mouse"     ) ::

        ImageAsset("cm-glyph"            ) ::
        ImageAsset("cm-title"            ) ::
        ImageAsset("cm-char"             , "lc-char"             ) ::
        ImageAsset("cm-lizard"           ) ::
        ImageAsset("cm-lizard-x5"        ) ::
        ImageAsset("cm-lizard-empty"     , "lc-lizard-empty"     ) ::
        ImageAsset("cm-lizard-x5-empty"  , "lc-lizard-x5-empty"  ) ::
        ImageAsset("cm-acolyte"          , "lc-acolyte"          ) ::
        ImageAsset("cm-acolyte-x5"       , "lc-acolyte-x5"       ) ::
        ImageAsset("cm-garden-fox"       ) ::
        ImageAsset("cm-garden-rabbit"    ) ::
        ImageAsset("cm-garden-mouse"     ) ::

        ImageAsset("outcast-fox"         ) ::
        ImageAsset("outcast-fox-hated"   ) ::
        ImageAsset("outcast-rabbit"      ) ::
        ImageAsset("outcast-rabbit-hated") ::
        ImageAsset("outcast-mouse"       ) ::
        ImageAsset("outcast-mouse-hated" ) ::
        ImageAsset("building-any"        ) ::
    $) ::
    ConditionalAssetsList((factions, options) => factions.of[Underground].any, "faction/underground")(
        ImageAsset("ud-glyph"          ) ::
        ImageAsset("ud-title"          ) ::
        ImageAsset("ud-char"           ) ::
        ImageAsset("ud-mole"           ) ::
        ImageAsset("ud-mole-x5"        ) ::
        ImageAsset("ud-mole-empty"     ) ::
        ImageAsset("ud-mole-x5-empty"  ) ::
        ImageAsset("ud-young"          ) ::
        ImageAsset("ud-young-x5"       ) ::
        ImageAsset("ud-citadel"        ) ::
        ImageAsset("ud-market"         ) ::
        ImageAsset("ud-tunnel"         ) ::
        ImageAsset("ud-crown"          ) ::
        ImageAsset("ud-crown-2"        ) ::
        ImageAsset("ud-crown-3"        ) ::
        ImageAsset("ud-crown-4"        ) ::
        ImageAsset("ud-crown-empty"    ) ::
        ImageAsset("ud-action"         ) ::
        ImageAsset("ud-building"       ) ::

        ImageAsset("dr-glyph"          ) ::
        ImageAsset("dr-title"          ) ::
        ImageAsset("dr-char"           , "ud-char"           ) ::
        ImageAsset("dr-mole"           ) ::
        ImageAsset("dr-mole-x5"        ) ::
        ImageAsset("dr-mole-empty"     , "ud-mole-empty"     ) ::
        ImageAsset("dr-mole-x5-empty"  , "ud-mole-x5-empty"  ) ::
        ImageAsset("dr-young"          , "ud-young"          ) ::
        ImageAsset("dr-young-x5"       , "ud-young-x5"       ) ::
        ImageAsset("dr-citadel"        ) ::
        ImageAsset("dr-market"         ) ::
        ImageAsset("dr-tunnel"         ) ::
        ImageAsset("dr-crown"          , "ud-crown"          ) ::
        ImageAsset("dr-crown-2"        , "ud-crown-2"        ) ::
        ImageAsset("dr-crown-3"        , "ud-crown-3"        ) ::
        ImageAsset("dr-crown-4"        , "ud-crown-4"        ) ::
        ImageAsset("dr-crown-empty"    , "ud-crown-empty"    ) ::
        ImageAsset("dr-action"         ) ::
        ImageAsset("dr-building"       ) ::

        ImageAsset("empty-building-mole-1") ::
        ImageAsset("empty-building-mole-2") ::
    $) ::
    ConditionalAssetsList((factions, options) => factions.of[Underground].any, "info", "info:", lzy = Laziness.OnDemand)(
        ImageAsset("minister-foremole"        ) ::
        ImageAsset("minister-marshal"         ) ::
        ImageAsset("minister-captain"         ) ::
        ImageAsset("minister-brigadier"       ) ::
        ImageAsset("minister-mayor"           ) ::
        ImageAsset("minister-banker"          ) ::
        ImageAsset("minister-duchess"   , "minister-duchess-of-mud"  ) ::
        ImageAsset("minister-baron"     , "minister-baron-of-dirt"   ) ::
        ImageAsset("minister-earl"      , "minister-earl-of-stone"   ) ::
    $) ::
    ConditionalAssetsList((factions, options) => factions.of[Mischief].any, "faction/mischief")(
        ImageAsset("cc-glyph"          ) ::
        ImageAsset("cc-title"          ) ::
        ImageAsset("cc-char"           ) ::
        ImageAsset("cc-raven"          ) ::
        ImageAsset("cc-raven-empty"    ) ::
        ImageAsset("cc-raven-x5"       ) ::
        ImageAsset("cc-raven-x5-empty" ) ::
        ImageAsset("cc-bomb"           ) ::
        ImageAsset("cc-snare"          ) ::
        ImageAsset("cc-extortion"      ) ::
        ImageAsset("cc-raid"           ) ::
        ImageAsset("cc-plot"           ) ::
        ImageAsset("cc-diversion"      , "cc-plot" ) ::
        ImageAsset("cc-action"         ) ::
        ImageAsset("ri-glyph"           ) ::
        ImageAsset("ri-title"           ) ::
        ImageAsset("ri-char"            , "cc-char"           ) ::
        ImageAsset("ri-raven"           ) ::
        ImageAsset("ri-raven-x5"        ) ::
        ImageAsset("ri-raven-empty"     , "cc-raven-empty"    ) ::
        ImageAsset("ri-raven-x5-empty"  , "cc-raven-x5-empty" ) ::
        ImageAsset("ri-bomb"            ) ::
        ImageAsset("ri-snare"           ) ::
        ImageAsset("ri-extortion"       ) ::
        ImageAsset("ri-raid"            ) ::
        ImageAsset("ri-plot"            ) ::
        ImageAsset("ri-diversion"       , "ri-plot"           ) ::
        ImageAsset("ri-action"          ) ::
    $) ::
    ConditionalAssetsList((factions, options) => factions.of[Horde].any, "faction/horde")(
        ImageAsset("lh-glyph"          ) ::
        ImageAsset("lh-title"          ) ::
        ImageAsset("lh-char"           ) ::
        ImageAsset("lh-rat"            ) ::
        ImageAsset("lh-rat-empty"      ) ::
        ImageAsset("lh-rat-x5"         ) ::
        ImageAsset("lh-rat-x5-empty"   ) ::
        ImageAsset("lh-warlord"        ) ::
        ImageAsset("lh-warlord-empty"  ) ::
        ImageAsset("lh-stronghold"     ) ::
        ImageAsset("lh-mob"            ) ::
        ImageAsset("lh-mob-q"          ) ::
        ImageAsset("lh-move"           ) ::
        ImageAsset("lh-move-done"      ) ::
        ImageAsset("lh-battle"         ) ::
        ImageAsset("lh-battle-done"    ) ::
        ImageAsset("lh-action"         ) ::

        ImageAsset("lk-glyph"          ) ::
        ImageAsset("lk-title"          ) ::
        ImageAsset("lk-char"           , "lh-char"           ) ::
        ImageAsset("lk-rat"            ) ::
        ImageAsset("lk-rat-x5"         ) ::
        ImageAsset("lk-rat-empty"      , "lh-rat-empty"      ) ::
        ImageAsset("lk-rat-x5-empty"   , "lh-rat-x5-empty"   ) ::
        ImageAsset("lk-warlord"        ) ::
        ImageAsset("lk-warlord-empty"  , "lh-warlord-empty"  ) ::
        ImageAsset("lk-stronghold"     ) ::
        ImageAsset("lk-mob"            ) ::
        ImageAsset("lk-mob-q"          ) ::
        ImageAsset("lk-move"           , "lh-move"           ) ::
        ImageAsset("lk-move-done"      , "lh-move-done"      ) ::
        ImageAsset("lk-battle"         , "lh-battle"         ) ::
        ImageAsset("lk-battle-done"    , "lh-battle-done"    ) ::
        ImageAsset("lk-action"         , "lh-action"         ) ::

        ImageAsset("satchel-top"        ) ::
        ImageAsset("satchel-top-2"      ) ::
        ImageAsset("satchel-top-3"      ) ::
        ImageAsset("satchel-top-4"      ) ::
        ImageAsset("satchel-bottom"     ) ::
        ImageAsset("satchel-bottom-2"   ) ::
        ImageAsset("satchel-bottom-3"   ) ::
        ImageAsset("satchel-bottom-4"   ) ::

        ImageAsset("satchel-middle"       ) ::
        ImageAsset("satchel-middle-start" ) ::
        ImageAsset("satchel-middle-cont"  ) ::

        ImageAsset("satchel-left-1"        ) ::
        ImageAsset("satchel-right-empty"   ) ::

        ImageAsset("satchel-thin-top-3" ) ::
        ImageAsset("satchel-thin-top-4" ) ::
        ImageAsset("satchel-thin-top-5" ) ::
        ImageAsset("satchel-thin-top-6" ) ::

        ImageAsset("satchel-thin-bottom-3" ) ::
        ImageAsset("satchel-thin-bottom-4" ) ::
        ImageAsset("satchel-thin-bottom-5" ) ::
        ImageAsset("satchel-thin-bottom-6" ) ::

        ImageAsset("satchel-left-1-lh"     ) ::
        ImageAsset("satchel-top-2-lh"      ) ::
        ImageAsset("satchel-top-3-lh"      ) ::
        ImageAsset("satchel-top-4-lh"      ) ::
        ImageAsset("satchel-bottom-2-lh"   ) ::
        ImageAsset("satchel-bottom-3-lh"   ) ::
        ImageAsset("satchel-bottom-4-lh"   ) ::

        ImageAsset("satchel-left-1-lk"     , "satchel-left-1-lh"   ) ::
        ImageAsset("satchel-top-2-lk"      , "satchel-top-2-lh"    ) ::
        ImageAsset("satchel-top-3-lk"      , "satchel-top-3-lh"    ) ::
        ImageAsset("satchel-top-4-lk"      , "satchel-top-4-lh"    ) ::
        ImageAsset("satchel-bottom-2-lk"   , "satchel-bottom-2-lh" ) ::
        ImageAsset("satchel-bottom-3-lk"   , "satchel-bottom-3-lh" ) ::
        ImageAsset("satchel-bottom-4-lk"   , "satchel-bottom-4-lh" ) ::
    $) ::
    ConditionalAssetsList((factions, options) => factions.of[Horde].any, "info", "info:", lzy = Laziness.OnDemand)(
        ImageAsset("lord-of-the-hundreds-1" ) ::
        ImageAsset("lord-of-the-hundreds-2" ) ::
        ImageAsset("lord-of-the-hundreds-3" ) ::
        ImageAsset("mood-bitter"            ) ::
        ImageAsset("mood-grandiose"         ) ::
        ImageAsset("mood-jubilant"          ) ::
        ImageAsset("mood-lavish"            ) ::
        ImageAsset("mood-relentless"        ) ::
        ImageAsset("mood-rowdy"             ) ::
        ImageAsset("mood-stubborn"          ) ::
        ImageAsset("mood-wrathful"          ) ::
    $) ::
    ConditionalAssetsList((factions, options) => factions.of[Expedition].any, "card", scale = 50)(
        ImageAsset("faithful-retainer") ::
    $) ::
    ConditionalAssetsList((factions, options) => factions.of[Expedition].any, "faction/aviary")(
        ImageAsset("ki-move-bird-done"     ,  "ed-move-bird-done"     ) ::
        ImageAsset("ki-move-fox-done"      ,  "ed-move-fox-done"      ) ::
        ImageAsset("ki-move-rabbit-done"   ,  "ed-move-rabbit-done"   ) ::
        ImageAsset("ki-move-mouse-done"    ,  "ed-move-mouse-done"    ) ::
        ImageAsset("ki-move-frog-done"     ,  "ed-move-frog-done"     ) ::

        ImageAsset("ki-battle-bird-done"   ,  "ed-battle-bird-done"   ) ::
        ImageAsset("ki-battle-fox-done"    ,  "ed-battle-fox-done"    ) ::
        ImageAsset("ki-battle-rabbit-done" ,  "ed-battle-rabbit-done" ) ::
        ImageAsset("ki-battle-mouse-done"  ,  "ed-battle-mouse-done"  ) ::
        ImageAsset("ki-battle-frog-done"   ,  "ed-battle-frog-done"   ) ::

        ImageAsset("ki-move-bird"          ,  "ed-move-bird"          ) ::
        ImageAsset("ki-move-fox"           ,  "ed-move-fox"           ) ::
        ImageAsset("ki-move-rabbit"        ,  "ed-move-rabbit"        ) ::
        ImageAsset("ki-move-mouse"         ,  "ed-move-mouse"         ) ::
        ImageAsset("ki-move-frog"          ,  "ed-move-frog"          ) ::

        ImageAsset("ki-battle-bird"        ,  "ed-battle-bird"        ) ::
        ImageAsset("ki-battle-fox"         ,  "ed-battle-fox"         ) ::
        ImageAsset("ki-battle-rabbit"      ,  "ed-battle-rabbit"      ) ::
        ImageAsset("ki-battle-mouse"       ,  "ed-battle-mouse"       ) ::
        ImageAsset("ki-battle-frog"        ,  "ed-battle-frog"        ) ::
    $) ::
    ConditionalAssetsList((factions, options) => factions.of[Expedition].any, "faction/expedition")(
        ImageAsset("ki-glyph"          ) ::
        ImageAsset("ki-title"          ) ::
        ImageAsset("ki-char"           ) ::
        ImageAsset("ki-badger-empty") ::
        ImageAsset("ki-badger-x5-empty") ::
        ImageAsset("ki-badger-x5") ::
        ImageAsset("ki-badger") ::
        ImageAsset("ki-idol-1") ::
        ImageAsset("ki-idol-1-map") ::
        ImageAsset("ki-idol-2") ::
        ImageAsset("ki-idol-2-map") ::
        ImageAsset("ki-idol-3") ::
        ImageAsset("ki-idol-3-map") ::
        ImageAsset("ki-idol-hidden") ::
        ImageAsset("ki-idol-hidden-map") ::
        ImageAsset("ki-idol-empty") ::
        ImageAsset("ki-jewel-1") ::
        ImageAsset("ki-jewel-1-map") ::
        ImageAsset("ki-jewel-2") ::
        ImageAsset("ki-jewel-2-map") ::
        ImageAsset("ki-jewel-3") ::
        ImageAsset("ki-jewel-3-map") ::
        ImageAsset("ki-jewel-hidden") ::
        ImageAsset("ki-jewel-hidden-map") ::
        ImageAsset("ki-jewel-empty") ::
        ImageAsset("ki-tablet-1") ::
        ImageAsset("ki-tablet-1-map") ::
        ImageAsset("ki-tablet-2") ::
        ImageAsset("ki-tablet-2-map") ::
        ImageAsset("ki-tablet-3") ::
        ImageAsset("ki-tablet-3-map") ::
        ImageAsset("ki-tablet-hidden") ::
        ImageAsset("ki-tablet-hidden-map") ::
        ImageAsset("ki-tablet-empty") ::

        ImageAsset("ki-waystation-idol-jewel") ::
        ImageAsset("ki-waystation-idol-tablet") ::
        ImageAsset("ki-waystation-jewel-idol") ::
        ImageAsset("ki-waystation-jewel-tablet") ::
        ImageAsset("ki-waystation-tablet-idol") ::
        ImageAsset("ki-waystation-tablet-jewel") ::

        ImageAsset("ki-recover-bird-done"    ) ::
        ImageAsset("ki-recover-fox-done"     ) ::
        ImageAsset("ki-recover-rabbit-done"  ) ::
        ImageAsset("ki-recover-mouse-done"   ) ::
        ImageAsset("ki-recover-frog-done"    ) ::

        ImageAsset("ki-recover-bird"    ) ::
        ImageAsset("ki-recover-fox"     ) ::
        ImageAsset("ki-recover-rabbit"  ) ::
        ImageAsset("ki-recover-mouse"   ) ::
        ImageAsset("ki-recover-frog"    ) ::
    $) ::
    ConditionalAssetsList((factions, options) => factions.of[OldExpedition].any, "faction/old-expedition")(
        ImageAsset("ok-glyph"           ) ::
        ImageAsset("ok-title"           ) ::
        ImageAsset("ok-char"            , "../expedition/ki-char") ::
        ImageAsset("ok-board"           ).scaled(50) ::
        ImageAsset("ok-badger"          ) ::
        ImageAsset("ok-badger-empty"    ) ::
        ImageAsset("ok-badger-x5"       ) ::
        ImageAsset("ok-badger-x5-empty" ) ::
        ImageAsset("ok-caravan"         ) ::
        ImageAsset("ok-station"         ) ::
        ImageAsset("ok-tablet"          ) ::
        ImageAsset("ok-tablet-map") ::
        ImageAsset("ok-jewel"           ) ::
        ImageAsset("ok-jewel-map") ::
        ImageAsset("ok-idol"            ) ::
        ImageAsset("ok-idol-map") ::
        ImageAsset("ok-tablet-hidden"   ) ::
        ImageAsset("ok-tablet-hidden-map") ::
        ImageAsset("ok-jewel-hidden"    ) ::
        ImageAsset("ok-jewel-hidden-map") ::
        ImageAsset("ok-idol-hidden"     ) ::
        ImageAsset("ok-idol-hidden-map") ::
    $) ::
    ConditionalAssetsList((factions, options) => factions.of[Hero].any, "faction/hero")(
        ImageAsset("vb-glyph"          ) ::
        ImageAsset("nb-glyph"          ) ::
        ImageAsset("mb-glyph"          ) ::
        ImageAsset("vb-vagabond"       ) ::
        ImageAsset("nb-vagabond"       ) ::
        ImageAsset("mb-vagabond"       ) ::
        ImageAsset("vagabond-harrier"  ) ::
        ImageAsset("vagabond-ronin"     ) ::
        ImageAsset("vagabond-adventurer") ::
        ImageAsset("vagabond-scoundrel" ) ::
        ImageAsset("vagabond-arbiter"   ) ::
        ImageAsset("vagabond-vagrant"   ) ::
        ImageAsset("vagabond-ranger"    ) ::
        ImageAsset("vagabond-tinker"    ) ::
        ImageAsset("vagabond-thief"     ) ::

        ImageAsset("quest-fox"         ) ::
        ImageAsset("quest-rabbit"      ) ::
        ImageAsset("quest-mouse"       ) ::

        ImageAsset("attitude-heart-empty" ) ::
        ImageAsset("attitude-heart-full"  ) ::
        ImageAsset("attitude-hostile"     ) ::

        ImageAsset("satchel-top",    "../horde/satchel-top"        ) ::
        ImageAsset("satchel-bottom", "../horde/satchel-bottom"     ) ::

        ImageAsset("satchel-thin-top-3", "../horde/satchel-thin-top-3" ) ::
        ImageAsset("satchel-thin-top-4", "../horde/satchel-thin-top-4" ) ::
        ImageAsset("satchel-thin-top-5", "../horde/satchel-thin-top-5" ) ::
        ImageAsset("satchel-thin-top-6", "../horde/satchel-thin-top-6" ) ::

        ImageAsset("satchel-thin-bottom-3", "../horde/satchel-thin-bottom-3" ) ::
        ImageAsset("satchel-thin-bottom-4", "../horde/satchel-thin-bottom-4" ) ::
        ImageAsset("satchel-thin-bottom-5", "../horde/satchel-thin-bottom-5" ) ::
        ImageAsset("satchel-thin-bottom-6", "../horde/satchel-thin-bottom-6" ) ::

        ImageAsset("vb-char-thief") ::
        ImageAsset("vb-char-tinker") ::
        ImageAsset("vb-char-ranger") ::
        ImageAsset("vb-char-arbiter") ::
        ImageAsset("vb-char-vagrant") ::
        ImageAsset("vb-char-scoundrel") ::
        ImageAsset("vb-char-adventurer") ::
        ImageAsset("vb-char-ronin") ::
        ImageAsset("vb-char-harrier") ::

        ImageAsset("vb-char-folk-hero").scaled(50) ::
        ImageAsset("vb-char-bard").scaled(50) ::
        ImageAsset("vb-char-barkeep").scaled(50) ::
        ImageAsset("vb-char-berserker").scaled(50) ::
        ImageAsset("vb-char-blacksmith").scaled(50) ::
        ImageAsset("vb-char-swindler").scaled(50) ::
        ImageAsset("vb-char-zealot").scaled(50) ::
        ImageAsset("vb-char-grouch").scaled(50) ::

        ImageAsset("vb-char", "vb-char-thief") ::
        ImageAsset("nb-char", "vb-char-thief") ::
        ImageAsset("mb-char", "vb-char-thief") ::

        ImageAsset("vb-title-thief") ::
        ImageAsset("vb-title-tinker") ::
        ImageAsset("vb-title-ranger") ::
        ImageAsset("vb-title-arbiter") ::
        ImageAsset("vb-title-vagrant") ::
        ImageAsset("vb-title-scoundrel") ::
        ImageAsset("vb-title-adventurer") ::
        ImageAsset("vb-title-ronin") ::
        ImageAsset("vb-title-harrier") ::

        ImageAsset("vb-title-folk-hero") ::
        ImageAsset("vb-title-bard") ::
        ImageAsset("vb-title-barkeep") ::
        ImageAsset("vb-title-berserker") ::
        ImageAsset("vb-title-blacksmith") ::
        ImageAsset("vb-title-swindler") ::
        ImageAsset("vb-title-zealot") ::
        ImageAsset("vb-title-grouch") ::

        ImageAsset("vb-title") ::
        ImageAsset("nb-title") ::
        ImageAsset("mb-title") ::
    $) ::
    ConditionalAssetsList((factions, options) => factions.of[Hero].any && options.has(RevisedQuests).not, "card/quest", "quest:", lzy = Laziness.OnDemand)(
        ImageAsset("errand-fox"             ) ::
        ImageAsset("errand-rabbit"          ) ::
        ImageAsset("escort-mouse"           ) ::
        ImageAsset("expel-bandits-mouse"    ) ::
        ImageAsset("expel-bandits-rabbit"   ) ::
        ImageAsset("fend-off-a-bear-mouse"  ) ::
        ImageAsset("fend-off-a-bear-rabbit" ) ::
        ImageAsset("fundraising-fox"        ) ::
        ImageAsset("give-a-speech-fox"      ) ::
        ImageAsset("give-a-speech-rabbit"   ) ::
        ImageAsset("guard-duty-mouse"       ) ::
        ImageAsset("guard-duty-rabbit"      ) ::
        ImageAsset("logistic-help-fox"      ) ::
        ImageAsset("logistic-help-mouse"    ) ::
        ImageAsset("repair-a-shed-fox"      ) ::
    $) ::
    ConditionalAssetsList((factions, options) => factions.of[Hero].any && options.has(RevisedQuests), "card/quest", "quest:", lzy = Laziness.OnDemand)(
        ImageAsset("revised-errand-fox"            , "revised/errand-fox"           ) ::
        ImageAsset("revised-errand-rabbit"         , "revised/errand-rabbit"        ) ::
        ImageAsset("revised-escort-mouse"          , "revised/escort-mouse"         ) ::
        ImageAsset("revised-expel-bandits-mouse"   , "expel-bandits-mouse"          ) ::
        ImageAsset("revised-expel-bandits-rabbit"  , "expel-bandits-rabbit"         ) ::
        ImageAsset("revised-fend-off-a-bear-mouse" , "fend-off-a-bear-mouse"        ) ::
        ImageAsset("revised-repair-a-shed-rabbit"  , "revised/repair-a-shed-rabbit" ) ::
        ImageAsset("revised-fundraising-fox"       , "revised/fundraising-fox"      ) ::
        ImageAsset("revised-give-a-speech-fox"     , "give-a-speech-fox"            ) ::
        ImageAsset("revised-give-a-speech-rabbit"  , "give-a-speech-rabbit"         ) ::
        ImageAsset("revised-guard-duty-mouse"      , "guard-duty-mouse"             ) ::
        ImageAsset("revised-guard-duty-rabbit"     , "guard-duty-rabbit"            ) ::
        ImageAsset("revised-logistic-help-fox"     , "logistic-help-fox"            ) ::
        ImageAsset("revised-logistic-help-mouse"   , "logistic-help-mouse"          ) ::
        ImageAsset("revised-escort-fox"            , "revised/escort-fox"           ) ::
    $) ::
    ConditionalAssetsList((factions, options) => options.has(ActionBooster), "card/action", lzy = Laziness.Later, scale = 50)(
        ImageAsset("action-hammer") ::
        ImageAsset("action-sword") ::
        ImageAsset("action-bag") ::
        ImageAsset("action-tea") ::
        ImageAsset("action-coins") ::
        ImageAsset("action-boots") ::
        ImageAsset("action-crossbow") ::
        ImageAsset("action-key") ::
        ImageAsset("action-secret-cancel") ::
        ImageAsset("action-secret-score") ::
    $) ::
    ConditionalAssetsList((factions, options) => options.has(CapitalistsDeck) || options.has(TournamentCapitalists), "card/capitalists", lzy = Laziness.Later, scale = 50)(
        ImageAsset("ca-command-warren") ::
        ImageAsset("ca-cobbler") ::
        ImageAsset("ca-better-burrow-bank") ::
        ImageAsset("ca-brutal-tactics") ::
        ImageAsset("ca-stand-and-deliver") ::
        ImageAsset("ca-tax-collector") ::
        ImageAsset("ca-scouting-party") ::
        ImageAsset("ca-sappers") ::
        ImageAsset("ca-royal-claim") ::
        ImageAsset("ca-codebreakers") ::
    $) ::
    ConditionalAssetsList((factions, options) => options.has(SorceryDeck), "card/sorcery", lzy = Laziness.Later, scale = 50)(
        ImageAsset("sorcery-dominance") ::
        ImageAsset("sorcery-garden-of-glory") ::
        ImageAsset("sorcery-enchanted-objects") ::
        ImageAsset("sorcery-living-buildings") ::
        ImageAsset("sorcery-mantra-of-frenzy") ::
        ImageAsset("sorcery-favor-of-the-phoenix") ::
        ImageAsset("sorcery-foresight") ::
        ImageAsset("sorcery-knowledge-of-the-fallen") ::
        ImageAsset("sorcery-enchanted-gifts") ::
        ImageAsset("sorcery-spell-of-flight") ::
        ImageAsset("sorcery-onyx-cauldron") ::
        ImageAsset("sorcery-misdirection") ::
        ImageAsset("sorcery-hailstorm") ::
        ImageAsset("sorcery-scepter-of-majesty") ::
        ImageAsset("sorcery-visions-from-beyond") ::
        ImageAsset("sorcery-treewalking") ::
        ImageAsset("sorcery-wizards-tower") ::
        ImageAsset("sorcery-fire-and-lightning") ::
        ImageAsset("sorcery-dispel") ::
        ImageAsset("sorcery-summon-river-beast") ::
        ImageAsset("sorcery-tower") ::
        ImageAsset("sorcery-garden") ::
    $) ::
    ConditionalAssetsList((factions, options) => options.has(UpstartsDeck), "card/upstarts", lzy = Laziness.Later, scale = 50)(
        ImageAsset("upstarts-sabotage") ::
        ImageAsset("upstarts-prophetic-sight") ::
        ImageAsset("upstarts-cooperative-housing") ::
        ImageAsset("upstarts-birdy-boom-boom") ::
        ImageAsset("upstarts-murder-hawk") ::
        ImageAsset("upstarts-fox-escort") ::
        ImageAsset("upstarts-mouse-escort") ::
        ImageAsset("upstarts-rabbit-escort") ::
        ImageAsset("upstarts-field-medic") ::
        ImageAsset("upstarts-coordinated-movement") ::
        ImageAsset("upstarts-burglar-extraordinaire") ::
        ImageAsset("upstarts-rabbit-hood") ::
        ImageAsset("upstarts-cobbler-commander") ::
        ImageAsset("upstarts-salt-factory") ::
        ImageAsset("upstarts-peace-offering") ::
        ImageAsset("upstarts-trailblazers") ::
        ImageAsset("upstarts-salvagers") ::
        ImageAsset("upstarts-tagalongs") ::
        ImageAsset("upstarts-baron-von-costume") ::
        ImageAsset("upstarts-goon-for-hire") ::
        ImageAsset("upstarts-hawk") ::
        ImageAsset("upstarts-housing") ::
        ImageAsset("upstarts-trail") ::
    $) ::
    ConditionalAssetsList((factions, options) => options.has(DarkDeck), "card/dark", lzy = Laziness.Later, scale = 50)(
        ImageAsset("dark-a-visit-to-friends") ::
        ImageAsset("dark-abomination-ambush") ::
        ImageAsset("dark-anvil") ::
        ImageAsset("dark-arms-trader") ::
        ImageAsset("dark-bear-traps") ::
        ImageAsset("dark-bird-ambush") ::
        ImageAsset("dark-bird-crossbow") ::
        ImageAsset("dark-bird-dominance") ::
        ImageAsset("dark-birdy-bindle") ::
        ImageAsset("dark-blood-gate") ::
        ImageAsset("dark-book-of-names") ::
        ImageAsset("dark-bottom-feeder") ::
        ImageAsset("dark-bunny-bandits") ::
        ImageAsset("dark-cleansing-fire") ::
        ImageAsset("dark-cursed") ::
        ImageAsset("dark-dark-dominance") ::
        ImageAsset("dark-deadly-deadly-poison") ::
        ImageAsset("dark-devotion-to-the-deep") ::
        ImageAsset("dark-doppleganger") ::
        ImageAsset("dark-felonious-fox") ::
        ImageAsset("dark-foaming-fury") ::
        ImageAsset("dark-forbidden-science") ::
        ImageAsset("dark-fox-ambush") ::
        ImageAsset("dark-fox-dominance") ::
        ImageAsset("dark-fox-root-tea") ::
        ImageAsset("dark-fox-transmutations") ::
        ImageAsset("dark-fox-travel-gear") ::
        ImageAsset("dark-foxes-of-fortune") ::
        ImageAsset("dark-foxfolk-steel") ::
        ImageAsset("dark-gently-used-knapsack") ::
        ImageAsset("dark-golden-knight") ::
        ImageAsset("dark-headhunters-trail") ::
        ImageAsset("dark-infernal-designs") ::
        ImageAsset("dark-investments") ::
        ImageAsset("dark-meat-for-the-masses") ::
        ImageAsset("dark-midnight-runners") ::
        ImageAsset("dark-mouse-ambush") ::
        ImageAsset("dark-mouse-crossbow") ::
        ImageAsset("dark-mouse-dominance") ::
        ImageAsset("dark-mouse-in-a-sack") ::
        ImageAsset("dark-mouse-root-tea") ::
        ImageAsset("dark-mouse-transmutations") ::
        ImageAsset("dark-mouse-travel-gear") ::
        ImageAsset("dark-mouster-criminal") ::
        ImageAsset("dark-murderous-mice") ::
        ImageAsset("dark-protection-racket") ::
        ImageAsset("dark-rabbit-ambush") ::
        ImageAsset("dark-rabbit-dominance") ::
        ImageAsset("dark-rabbit-root-tea") ::
        ImageAsset("dark-rabbit-roulette") ::
        ImageAsset("dark-rabbit-transmutations") ::
        ImageAsset("dark-rabbits-paw") ::
        ImageAsset("dark-rapacious-rabbit") ::
        ImageAsset("dark-rodent-zero") ::
        ImageAsset("dark-sabotage") ::
        ImageAsset("dark-sword") ::
        ImageAsset("dark-torch") ::
        ImageAsset("dark-wake-sale") ::
        ImageAsset("dark-words-of-undoing") ::
    $) ::
    ConditionalAssetsList((factions, options) => options.has(FrankenDeck), "card/franken", lzy = Laziness.Later, scale = 50)(
        ImageAsset("franken-a-visit-to-friends") ::
        ImageAsset("franken-anvil") ::
        ImageAsset("franken-arms-trader") ::
        ImageAsset("franken-bird-ambush") ::
        ImageAsset("franken-bird-crossbow") ::
        ImageAsset("franken-bird-dominance") ::
        ImageAsset("franken-birdy-bindle") ::
        ImageAsset("franken-fox-ambush") ::
        ImageAsset("franken-fox-dominance") ::
        ImageAsset("franken-fox-root-tea") ::
        ImageAsset("franken-fox-travel-gear") ::
        ImageAsset("franken-foxfolk-steel") ::
        ImageAsset("franken-gently-used-knapsack") ::
        ImageAsset("franken-headhunters-trail") ::
        ImageAsset("franken-investments") ::
        ImageAsset("franken-midnight-runners") ::
        ImageAsset("franken-mouse-ambush") ::
        ImageAsset("franken-mouse-crossbow") ::
        ImageAsset("franken-mouse-dominance") ::
        ImageAsset("franken-mouse-in-a-sack") ::
        ImageAsset("franken-mouse-root-tea") ::
        ImageAsset("franken-mouse-travel-gear") ::
        ImageAsset("franken-protection-racket") ::
        ImageAsset("franken-rabbit-ambush") ::
        ImageAsset("franken-rabbit-dominance") ::
        ImageAsset("franken-rabbit-root-tea") ::
        ImageAsset("franken-sabotage") ::
        ImageAsset("franken-sword") ::
        ImageAsset("franken-wake-sale") ::
    $) ::
    ConditionalAssetsList((factions, options) => true, "card/deck", lzy = Laziness.Later, scale = 50)(
        ImageAsset("bird-ambush"                ) ::
        ImageAsset("fox-ambush"                 ) ::
        ImageAsset("mouse-ambush"               ) ::
        ImageAsset("rabbit-ambush"              ) ::

        ImageAsset("bird-dominance"             ) ::
        ImageAsset("fox-dominance"              ) ::
        ImageAsset("mouse-dominance"            ) ::
        ImageAsset("rabbit-dominance"           ) ::

        ImageAsset("a-visit-to-friends"         ) ::
        ImageAsset("anvil"                      ) ::
        ImageAsset("arms-trader"                ) ::
        ImageAsset("bake-sale"                  ) ::
        ImageAsset("bird-crossbow"              ) ::
        ImageAsset("birdy-bindle"               ) ::
        ImageAsset("fox-root-tea"               ) ::
        ImageAsset("fox-travel-gear"            ) ::
        ImageAsset("foxfolk-steel"              ) ::
        ImageAsset("gently-used-knapsack"       ) ::
        ImageAsset("investments"                ) ::
        ImageAsset("mouse-crossbow"             ) ::
        ImageAsset("mouse-in-a-sack"            ) ::
        ImageAsset("mouse-root-tea"             ) ::
        ImageAsset("mouse-travel-gear"          ) ::
        ImageAsset("protection-racket"          ) ::
        ImageAsset("rabbit-root-tea"            ) ::
        ImageAsset("smugglers-trail"            ) ::
        ImageAsset("sword"                      ) ::
        ImageAsset("woodland-runners"           ) ::

        ImageAsset("armorers"                   ) ::
        ImageAsset("better-burrow-bank"         ) ::
        ImageAsset("brutal-tactics"             ) ::
        ImageAsset("cobbler"                    ) ::
        ImageAsset("codebreakers"               ) ::
        ImageAsset("command-warren"             ) ::
        ImageAsset("favor-of-the-foxes"         ) ::
        ImageAsset("favor-of-the-mice"          ) ::
        ImageAsset("favor-of-the-rabbits"       ) ::
        ImageAsset("royal-claim"                ) ::
        ImageAsset("sappers"                    ) ::
        ImageAsset("scouting-party"             ) ::
        ImageAsset("stand-and-deliver"          ) ::
        ImageAsset("tax-collector"              ) ::

        ImageAsset("boat-builders"              ) ::
        ImageAsset("charm-offensive"            ) ::
        ImageAsset("coffin-makers"              ) ::
        ImageAsset("corvid-planners"            ) ::
        ImageAsset("eyrie-emigre"               ) ::
        ImageAsset("false-orders"               ) ::
        ImageAsset("fox-partisans"              ) ::
        ImageAsset("informants"                 ) ::
        ImageAsset("league-of-adventurous-mice" ) ::
        ImageAsset("master-engravers"           ) ::
        ImageAsset("mouse-partisans"            ) ::
        ImageAsset("murine-broker"              ) ::
        ImageAsset("propaganda-bureau"          ) ::
        ImageAsset("rabbit-partisans"           ) ::
        ImageAsset("saboteurs"                  ) ::
        ImageAsset("soup-kitchens"              ) ::
        ImageAsset("borscht-kitchens"           ) ::
        ImageAsset("swap-meet"                  ) ::
        ImageAsset("tunnels"                    ) ::

        ImageAsset("apprentice") ::
        ImageAsset("bold-leadership") ::
        ImageAsset("brazen-demagogue") ::
        ImageAsset("feather-rufflers") ::
        ImageAsset("fox-squires") ::
        ImageAsset("friend-of-the-foxes") ::
        ImageAsset("friend-of-the-mice") ::
        ImageAsset("friend-of-the-rabbits") ::
        ImageAsset("hidden-warrens") ::
        ImageAsset("lookouts") ::
        ImageAsset("mice-in-a-bush") ::
        ImageAsset("mouse-squires") ::
        ImageAsset("rabbit-squires") ::
        ImageAsset("raiding-party") ::
        ImageAsset("riversteads") ::
        ImageAsset("shadow-council") ::
        ImageAsset("silver-tongue") ::
        ImageAsset("sky-couriers") ::
        ImageAsset("spy-network") ::
        ImageAsset("standard-bearer") ::
        ImageAsset("supply-train") ::
        ImageAsset("tactician") ::
        ImageAsset("the-faithful") ::
    $) ::
    ConditionalAssetsList((factions, options) => true, "card/deck/fr", "fr:", lzy = Laziness.OnDemand, scale = 50)(French.cards./(ImageAsset(_))) ::
    ConditionalAssetsList((factions, options) => true, "faction/fr", "fr:", lzy = Laziness.OnDemand)(French.boards./(ImageAsset(_))) ::
    ConditionalAssetsList((factions, options) => options.has(DuskDeck) || options.has(FrankenDeck) || options.has(CapitalistsDeck) || options.has(TournamentCapitalists), "card/dusk", lzy = Laziness.Later)(
        ImageAsset("adventurers"                ) ::
        ImageAsset("breaking-dawn"              ) ::
        ImageAsset("dusk-awakening"             ) ::
        ImageAsset("forest-vigil"               ) ::
        ImageAsset("fowl-play"                  ) ::
        ImageAsset("fox-guard"                  ) ::
        ImageAsset("high-noon"                  ) ::
        ImageAsset("hoarders"                   ) ::
        ImageAsset("merchants"                  ) ::
        ImageAsset("military-supplies"          ) ::
        ImageAsset("mouse-guard"                ) ::
        ImageAsset("night-terrors"              ) ::
        ImageAsset("off-trail"                  ) ::
        ImageAsset("old-friend"                 ) ::
        ImageAsset("public-relations"           ) ::
        ImageAsset("rabbit-guard"               ) ::
        ImageAsset("service-of-the-foxes"       ) ::
        ImageAsset("service-of-the-mice"        ) ::
        ImageAsset("service-of-the-rabbits"     ) ::
        ImageAsset("sly-dog"                    ) ::
        ImageAsset("spoil"                      ) ::
        ImageAsset("tricky-claws"               ) ::
        ImageAsset("tricky-ears"                ) ::
        ImageAsset("tricky-tails"               ) ::
    $) ::
    ConditionalAssetsList((factions, options) => true, "info", "info:", lzy = Laziness.OnDemand)(
        ImageAsset("street-band-1"          ) ::
        ImageAsset("street-band-2"          ) ::
        ImageAsset("bandit-gangs-1"         ) ::
        ImageAsset("bandit-gangs-2"         ) ::
        ImageAsset("rat-smugglers"          ) ::
        ImageAsset("forest-patrol"          ) ::
        ImageAsset("last-dynasty"           ) ::
        ImageAsset("spring-uprising"        ) ::
        ImageAsset("the-exile"              ) ::
        ImageAsset("mole-artisians"         ) ::
        ImageAsset("flotilla"               ) ::
    $) ::
    ConditionalAssetsList((factions, options) => true, "artwork", "artwork:", lzy = Laziness.OnDemand)(
        ImageAsset("bird-ambush"                ) ::
        ImageAsset("fox-ambush"                 ) ::
        ImageAsset("mouse-ambush"               ) ::
        ImageAsset("rabbit-ambush"              ) ::

        ImageAsset("bird-dominance"             ) ::
        ImageAsset("fox-dominance"              ) ::
        ImageAsset("mouse-dominance"            ) ::
        ImageAsset("rabbit-dominance"           ) ::

        ImageAsset("a-visit-to-friends"         ) ::
        ImageAsset("anvil"                      ) ::
        ImageAsset("arms-trader"                ) ::
        ImageAsset("bake-sale"                  ) ::
        ImageAsset("bird-crossbow"              ) ::
        ImageAsset("birdy-bindle"               ) ::
        ImageAsset("fox-root-tea"               ) ::
        ImageAsset("fox-travel-gear"            ) ::
        ImageAsset("foxfolk-steel"              ) ::
        ImageAsset("gently-used-knapsack"       ) ::
        ImageAsset("investments"                ) ::
        ImageAsset("mouse-crossbow"             ) ::
        ImageAsset("mouse-in-a-sack"            ) ::
        ImageAsset("mouse-root-tea"             ) ::
        ImageAsset("mouse-travel-gear"          ) ::
        ImageAsset("protection-racket"          ) ::
        ImageAsset("rabbit-root-tea"            ) ::
        ImageAsset("smugglers-trail"            ) ::
        ImageAsset("sword"                      ) ::
        ImageAsset("woodland-runners"           ) ::

        ImageAsset("armorers"                   ) ::
        ImageAsset("better-burrow-bank"         ) ::
        ImageAsset("brutal-tactics"             ) ::
        ImageAsset("cobbler"                    ) ::
        ImageAsset("codebreakers"               ) ::
        ImageAsset("command-warren"             ) ::
        ImageAsset("favor-of-the-foxes"         ) ::
        ImageAsset("favor-of-the-mice"          ) ::
        ImageAsset("favor-of-the-rabbits"       ) ::
        ImageAsset("royal-claim"                ) ::
        ImageAsset("sappers"                    ) ::
        ImageAsset("scouting-party"             ) ::
        ImageAsset("stand-and-deliver"          ) ::
        ImageAsset("tax-collector"              ) ::

        ImageAsset("boat-builders"              ) ::
        ImageAsset("charm-offensive"            ) ::
        ImageAsset("coffin-makers"              ) ::
        ImageAsset("corvid-planners"            ) ::
        ImageAsset("eyrie-emigre"               ) ::
        ImageAsset("false-orders"               ) ::
        ImageAsset("fox-partisans"              ) ::
        ImageAsset("informants"                 ) ::
        ImageAsset("league-of-adventurous-mice" ) ::
        ImageAsset("master-engravers"           ) ::
        ImageAsset("mouse-partisans"            ) ::
        ImageAsset("murine-broker"              ) ::
        ImageAsset("propaganda-bureau"          ) ::
        ImageAsset("rabbit-partisans"           ) ::
        ImageAsset("saboteurs"                  ) ::
        ImageAsset("soup-kitchens"              ) ::
        ImageAsset("borscht-kitchens"           ) ::
        ImageAsset("swap-meet"                  ) ::
        ImageAsset("tunnels"                    ) ::

        ImageAsset("faithful-retainer"          ) ::
    $) ::
    ConditionalAssetsList((factions, options) => factions.of[Hero].any && options.has(RevisedQuests).not, "artwork/quest", "artwork:", lzy = Laziness.OnDemand)(
        ImageAsset("errand-fox"             ) ::
        ImageAsset("errand-rabbit"          ) ::
        ImageAsset("escort-mouse"           ) ::
        ImageAsset("expel-bandits-mouse"    ) ::
        ImageAsset("expel-bandits-rabbit"   ) ::
        ImageAsset("fend-off-a-bear-mouse"  ) ::
        ImageAsset("fend-off-a-bear-rabbit" ) ::
        ImageAsset("fundraising-fox"        ) ::
        ImageAsset("give-a-speech-fox"      ) ::
        ImageAsset("give-a-speech-rabbit"   ) ::
        ImageAsset("guard-duty-mouse"       ) ::
        ImageAsset("guard-duty-rabbit"      ) ::
        ImageAsset("logistic-help-fox"      ) ::
        ImageAsset("logistic-help-mouse"    ) ::
        ImageAsset("repair-a-shed-fox"      ) ::
    $) ::
    ConditionalAssetsList((factions, options) => factions.of[Hero].any && options.has(RevisedQuests), "artwork/quest", "artwork:", lzy = Laziness.OnDemand)(
        ImageAsset("errand-fox"             , "revised/errand-fox"            ) ::
        ImageAsset("errand-rabbit"          , "revised/errand-rabbit"         ) ::
        ImageAsset("escort-mouse"           , "revised/escort-mouse"          ) ::
        ImageAsset("expel-bandits-mouse"    ) ::
        ImageAsset("expel-bandits-rabbit"   ) ::
        ImageAsset("fend-off-a-bear-mouse"  ) ::
        ImageAsset("repair-a-shed-rabbit"   , "revised/repair-a-shed-rabbit"  ) ::
        ImageAsset("fundraising-fox"        , "revised/fundraising-fox"       ) ::
        ImageAsset("give-a-speech-fox"      ) ::
        ImageAsset("give-a-speech-rabbit"   ) ::
        ImageAsset("guard-duty-mouse"       ) ::
        ImageAsset("guard-duty-rabbit"      ) ::
        ImageAsset("logistic-help-fox"      ) ::
        ImageAsset("logistic-help-mouse"    ) ::
        ImageAsset("escort-fox"             , "revised/escort-fox"            ) ::
    $) ::
    ConditionalAssetsList((factions, options) => factions.of[Utopia].any, "faction/utopia")(
        ImageAsset("cu-glyph"          ) ::
        ImageAsset("cu-title"          ) ::
        ImageAsset("cu-char"           ) ::
        ImageAsset("cu-foxy"           ) ::
        ImageAsset("cu-foxy-empty"     ) ::
        ImageAsset("cu-foxy-x5"        ) ::
        ImageAsset("cu-foxy-x5-empty"  ) ::
        ImageAsset("cu-palace"         ) ::
        ImageAsset("cu-living-shield"  ) ::
        ImageAsset("cu-action"         ) ::

        ImageAsset("cu-board-v1"       ) ::
        ImageAsset("cu-board-v2"       ) ::
        ImageAsset("cu-board-v3"       ) ::

        ImageAsset("cu-track-v1-1") ::
        ImageAsset("cu-track-v1-1-m") ::
        ImageAsset("cu-track-v1-2") ::
        ImageAsset("cu-track-v1-2-m") ::
        ImageAsset("cu-track-v1-3") ::
        ImageAsset("cu-track-v1-3-m") ::
        ImageAsset("cu-track-v1-4") ::
        ImageAsset("cu-track-v1-4-m") ::
        ImageAsset("cu-track-v1-5") ::
        ImageAsset("cu-track-v1-5-m") ::
        ImageAsset("cu-track-v1-6") ::
        ImageAsset("cu-track-v1-6-m") ::
        ImageAsset("cu-track-v1-7") ::
        ImageAsset("cu-track-v1-7-m") ::
        ImageAsset("cu-track-v3-1") ::
        ImageAsset("cu-track-v3-1-m") ::
        ImageAsset("cu-track-v3-2") ::
        ImageAsset("cu-track-v3-2-m") ::
        ImageAsset("cu-track-v3-3") ::
        ImageAsset("cu-track-v3-3-m") ::
        ImageAsset("cu-track-v3-4") ::
        ImageAsset("cu-track-v3-4-m") ::
        ImageAsset("cu-track-v3-5") ::
        ImageAsset("cu-track-v3-5-m") ::
        ImageAsset("cu-track-v3-6") ::
        ImageAsset("cu-track-v3-6-m") ::
        ImageAsset("cu-track-v3-7") ::
        ImageAsset("cu-track-v3-7-m") ::
    $) ::
    ConditionalAssetsList((factions, options) => factions.of[Caster].any, "faction/caster")(
        ImageAsset("xc-glyph"          ) ::
        ImageAsset("xc-title"          ) ::
        ImageAsset("xc-char"           ) ::
        ImageAsset("xc-frog"           ) ::
        ImageAsset("xc-frog-empty"     ) ::
        ImageAsset("xc-frog-x5"        ) ::
        ImageAsset("xc-frog-x5-empty"  ) ::
        ImageAsset("xc-school"         ) ::
        ImageAsset("xc-statue"         ) ::
        ImageAsset("xc-growth"         ) ::
        ImageAsset("xc-mudman"         ) ::
        ImageAsset("xc-action"         ) ::
        ImageAsset("xc-magic"          ) ::
        ImageAsset("xc-magic-q"        ) ::
        ImageAsset("xc-board"          ) ::

        ImageAsset("xc-spell-petrify"      ) ::
        ImageAsset("xc-spell-dazzle"       ) ::
        ImageAsset("xc-spell-regrowth"     ) ::
        ImageAsset("xc-spell-fireball"     ) ::
        ImageAsset("xc-spell-summon"       ) ::
        ImageAsset("xc-spell-resurrection" ) ::
        ImageAsset("xc-spell-transform"    ) ::
        ImageAsset("xc-spell-teleport"     ) ::
    $) ::
    // the Doomed Expansion's Re-Imaginings (art by root/tools/reimaginings-art.py)
    ConditionalAssetsList((factions, options) => factions.has(EE), "faction/aviary")(
        ImageAsset("ee-glyph"             ) ::
        ImageAsset("ee-title"             ) ::
        ImageAsset("ee-char"              ) ::
        ImageAsset("ee-hawk"              , "ed-hawk"          ) ::
        ImageAsset("ee-hawk-x5"           , "ed-hawk-x5"       ) ::
        ImageAsset("ee-hawk-empty"        , "ed-hawk-empty"    ) ::
        ImageAsset("ee-hawk-x5-empty"     , "ed-hawk-x5-empty" ) ::
        ImageAsset("ee-cardinal"          ) ::
        ImageAsset("ee-cardinal-empty"    ) ::
        ImageAsset("ee-roost"             , "ed-roost"         ) ::
        ImageAsset("ee-caravan"           ) ::
        ImageAsset("ee-caravandown"       ) ::
        // leader cards are drawn as "ee-<leader>" (ShowLeaderAction); without them the leader pick threw and showed no choices
        ImageAsset("ee-patron"            , "ed-patron"        ) ::
        ImageAsset("ee-megalomaniac"      , "ed-megalomaniac"  ) ::
        ImageAsset("ee-hedonistic"        , "ed-hedonistic"    ) ::
        ImageAsset("ee-charismatic"       , "ed-charismatic"   ) ::
        ImageAsset("ee-commander"         , "ed-commander"     ) ::
        ImageAsset("ee-despot"            , "ed-despot"        ) ::
        ImageAsset("ee-board"             ) ::
        $("recruit", "double-recruit", "move", "battle", "build")./~(a => $("bird", "fox", "rabbit", "mouse", "frog")./~(s => $("", "-done")./(d => ImageAsset("ee-" + a + "-" + s + d, "ed-" + a + "-" + s + d))))
    ) ::
    ConditionalAssetsList((factions, options) => factions.of[Marquistadors].any, "faction/marquistadors")(
        ImageAsset("dm-glyph"                 ) ::
        ImageAsset("dm-title"                 ) ::
        ImageAsset("dm-char"                  ) ::
        ImageAsset("dm-marquistador"          ) ::
        ImageAsset("dm-marquistador-empty"    ) ::
        ImageAsset("dm-marquistador-x5"       ) ::
        ImageAsset("dm-marquistador-x5-empty" ) ::
        ImageAsset("dm-outpost"               ) ::
        ImageAsset("dm-ship"                  ) ::
        ImageAsset("dm-wood"                  ) ::
        ImageAsset("dm-board"                 ) ::
    $) ::
    ConditionalAssetsList((factions, options) => factions.has(DV), "faction/hero")(
        ImageAsset("dv-glyph"          , "vb-glyph" ) ::
        ImageAsset("dv-vagabond"       , "vb-vagabond" ) ::
        ImageAsset("dv-char"           , "vb-char-thief" ) ::
        ImageAsset("dv-title"          ) ::
        ImageAsset("dv-board"          ) ::
    $) ::
    ConditionalAssetsList((factions, options) => factions.has(DV), "card/quest", "quest:", lzy = Laziness.OnDemand)(
        1.to(5)./(i => ImageAsset("collect-bounty-" + i, "collect-bounty"))
    ) ::
    ConditionalAssetsList((factions, options) => factions.has(DV), "artwork/quest", "artwork:", lzy = Laziness.OnDemand)(
        1.to(5)./(i => ImageAsset("collect-bounty-" + i, "collect-bounty"))
    ) ::
    ConditionalAssetsList((factions, options) => factions.has(NW), "faction/abduct")(
        ImageAsset("nw-glyph"            ) ::
        ImageAsset("nw-title"            ) ::
        ImageAsset("nw-char"             , "kd-char" ).scaled(20) ::
        ImageAsset("nw-board"            ) ::
        ImageAsset("nw-skunk"            ) ::
        ImageAsset("nw-skunk-empty"      ) ::
        ImageAsset("nw-skunk-x5"         ) ::
        ImageAsset("nw-skunk-x5-empty"   ) ::
        ImageAsset("nw-acclaim"          ) ::
        ImageAsset("nw-captain-empty"    , "kd-captain-empty" ) ::
        $("birdsong", "daylight", "evening", "gladiator", "cheat", "jailor", "harrier", "ronin", "adventurer", "scoundrel", "arbiter", "vagrant", "ranger", "tinker", "thief", "bard", "barkeep", "berserker", "blacksmith", "swindler", "zealot")./(k => ImageAsset("nw-captain-" + k, "kd-captain-" + k))
    ) ::
    ConditionalAssetsList((factions, options) => factions.of[BonePatrol].any, "faction/bone")(
        ImageAsset("bp-glyph"                 ) ::
        ImageAsset("bp-title"                 ) ::
        ImageAsset("bp-char"                  ) ::
        ImageAsset("bp-action"                ) ::
        ImageAsset("bp-pug"                   ) ::
        ImageAsset("bp-pug-empty"             ) ::
        ImageAsset("bp-pug-x5"                ) ::
        ImageAsset("bp-pug-x5-empty"          ) ::
        ImageAsset("bp-alpha"                 ) ::
        ImageAsset("bp-alpha-empty"           ) ::
        ImageAsset("bp-stash"                 ) ::
        ImageAsset("bp-bone-fox"              ) ::
        ImageAsset("bp-bone-rabbit"           ) ::
        ImageAsset("bp-bone-mouse"            ) ::
        ImageAsset("bp-bone-buried"           ) ::
        ImageAsset("bp-board"                 ) ::
    $) ::
    ConditionalAssetsList((factions, options) => factions.of[OrderOfTheForest].any, "faction/order")(
        ImageAsset("of-glyph"                 ) ::
        ImageAsset("of-title"                 ) ::
        ImageAsset("of-char"                  ) ::
        ImageAsset("of-action"                ) ::
        ImageAsset("of-devotee"               ) ::
        ImageAsset("of-devotee-empty"         ) ::
        ImageAsset("of-devotee-x5"            ) ::
        ImageAsset("of-devotee-x5-empty"      ) ::
        ImageAsset("of-priest-fox"            ) ::
        ImageAsset("of-priest-rabbit"         ) ::
        ImageAsset("of-priest-mouse"          ) ::
        ImageAsset("of-priest-bird"           ) ::
        ImageAsset("of-priest-fox-empty"      ) ::
        ImageAsset("of-priest-rabbit-empty"   ) ::
        ImageAsset("of-priest-mouse-empty"    ) ::
        ImageAsset("of-priest-bird-empty"     ) ::
        ImageAsset("of-abbey-fox"             ) ::
        ImageAsset("of-abbey-rabbit"          ) ::
        ImageAsset("of-abbey-mouse"           ) ::
        ImageAsset("of-abbey-bird"            ) ::
        ImageAsset("of-soup"                  ) ::
        ImageAsset("of-coins"                 ) ::
        ImageAsset("of-board"                 ) ::
    $) ::
    ConditionalAssetsList((factions, options) => factions.of[OldManTinker].any, "faction/tinker")(
        ImageAsset("omt-glyph"                ) ::
        ImageAsset("omt-title"                ) ::
        ImageAsset("omt-char"                 ) ::
        ImageAsset("omt-action"               ) ::
        ImageAsset("omt-drone"                ) ::
        ImageAsset("omt-gear"                 ) ::
        ImageAsset("omt-factory"              ) ::
        ImageAsset("omt-old-man"              ) ::
        ImageAsset("omt-wolfbot"              ) ::
        ImageAsset("omt-birdbot"              ) ::
        ImageAsset("omt-owlbot"               ) ::
        ImageAsset("omt-wolfbot-broken"       ) ::
        ImageAsset("omt-birdbot-broken"       ) ::
        ImageAsset("omt-owlbot-broken"        ) ::
        ImageAsset("omt-board"                ) ::
    $) ::
    ConditionalAssetsList((factions, options) => factions.of[NoxiousBattery].any, "faction/battery")(
        ImageAsset("nx-glyph"                 ) ::
        ImageAsset("nx-title"                 ) ::
        ImageAsset("nx-char"                  ) ::
        ImageAsset("nx-action"                ) ::
        ImageAsset("nx-skunk"                 ) ::
        ImageAsset("nx-skunk-empty"           ) ::
        ImageAsset("nx-skunk-x5"              ) ::
        ImageAsset("nx-skunk-x5-empty"        ) ::
        ImageAsset("nx-cannon"                ) ::
        ImageAsset("nx-cannon-damaged"        ) ::
        ImageAsset("nx-cannonball"            ) ::
        ImageAsset("nx-scent"                 ) ::
        ImageAsset("nx-board"                 ) ::
    $) ::
    ConditionalAssetsList((factions, options) => factions.of[UnitedDoveCorps].any, "faction/dove")(
        ImageAsset("udc-glyph"                ) ::
        ImageAsset("udc-title"                ) ::
        ImageAsset("udc-char"                 ) ::
        ImageAsset("udc-action"               ) ::
        ImageAsset("udc-dove"                 ) ::
        ImageAsset("udc-dove-empty"           ) ::
        ImageAsset("udc-dove-x5"              ) ::
        ImageAsset("udc-dove-x5-empty"        ) ::
        ImageAsset("udc-clinic"               ) ::
        ImageAsset("udc-school"               ) ::
        ImageAsset("udc-ceasefire"            ) ::
        ImageAsset("udc-peace"                ) ::
        ImageAsset("udc-board"                ) ::
    $) ::
    ConditionalAssetsList((factions, options) => factions.of[ArachnidAssociation].any, "faction/arachnid")(
        ImageAsset("aa-glyph"                 ) ::
        ImageAsset("aa-title"                 ) ::
        ImageAsset("aa-char"                  ) ::
        ImageAsset("aa-action"                ) ::
        ImageAsset("aa-adult"                 ) ::
        ImageAsset("aa-adult-empty"           ) ::
        ImageAsset("aa-spiderling"            ) ::
        ImageAsset("aa-spiderling-empty"      ) ::
        ImageAsset("aa-spiderling-x5"         ) ::
        ImageAsset("aa-spiderling-x5-empty"   ) ::
        ImageAsset("aa-web"                   ) ::
        ImageAsset("aa-web-full"              ) ::
        ImageAsset("aa-egg"                   ) ::
        ImageAsset("aa-hatched"               ) ::
        ImageAsset("aa-mutation"              ) ::
        ImageAsset("aa-board"                 ) ::
    $) ::
    ConditionalAssetsList((factions, options) => factions.of[TwelveColonies].any, "faction/colonies")(
        ImageAsset("tw-glyph"                 ) ::
        ImageAsset("tw-title"                 ) ::
        ImageAsset("tw-char"                  ) ::
        ImageAsset("tw-action"                ) ::
        ImageAsset("tw-snake"                 ) ::
        ImageAsset("tw-snake-empty"           ) ::
        ImageAsset("tw-snake-x5"              ) ::
        ImageAsset("tw-snake-x5-empty"        ) ::
        ImageAsset("tw-turtle"                ) ::
        ImageAsset("tw-turtle-empty"          ) ::
        ImageAsset("tw-turtle-x5"             ) ::
        ImageAsset("tw-turtle-x5-empty"       ) ::
        ImageAsset("tw-capitol"               ) ::
        ImageAsset("tw-common"                ) ::
        ImageAsset("tw-historical-site"       ) ::
        ImageAsset("tw-ballot"                ) ::
        ImageAsset("tw-board"                 ) ::
        ImageAsset("tw-law-amplius-actum"     ) ::
        ImageAsset("tw-law-arcitectus-corvidae") ::
        ImageAsset("tw-law-artifex-contentio" ) ::
        ImageAsset("tw-law-communia-divitiae" ) ::
        ImageAsset("tw-law-defensio-optimum"  ) ::
        ImageAsset("tw-law-feris-ratio"       ) ::
        ImageAsset("tw-law-insidae-tutela"    ) ::
        ImageAsset("tw-law-militaris-subsidium") ::
        ImageAsset("tw-law-non-variablis"     ) ::
        ImageAsset("tw-law-omnius-exitus"     ) ::
        ImageAsset("tw-law-pax-lepus"         ) ::
        ImageAsset("tw-law-pax-mus"           ) ::
        ImageAsset("tw-law-pax-vulpes"        ) ::
        ImageAsset("tw-law-public-trust"      ) ::
        ImageAsset("tw-law-publico-auxilio"   ) ::
        ImageAsset("tw-law-status-aequalis"   ) ::
        ImageAsset("tw-law-valorem-historiae" ) ::
    $) ::
    ConditionalAssetsList((factions, options) => factions.of[WingedMenace].any, "faction/menace")(
        ImageAsset("wm-glyph"                 ) ::
        ImageAsset("wm-title"                 ) ::
        ImageAsset("wm-char"                  ) ::
        ImageAsset("wm-action"                ) ::
        ImageAsset("wm-bat"                   ) ::
        ImageAsset("wm-bat-empty"             ) ::
        ImageAsset("wm-bat-x5"                ) ::
        ImageAsset("wm-bat-x5-empty"          ) ::
        ImageAsset("wm-infestation"           ) ::
        ImageAsset("wm-cave"                  ) ::
        ImageAsset("wm-guano"                 ) ::
        ImageAsset("wm-board"                 ) ::
    $) ::
    ConditionalAssetsList((factions, options) => factions.of[WarriorsWake].any, "faction/wake")(
        ImageAsset("ww-glyph"                 ) ::
        ImageAsset("ww-title"                 ) ::
        ImageAsset("ww-char"                  ) ::
        ImageAsset("ww-wakevulture"           ) ::
        ImageAsset("ww-wakevulture-empty"     ) ::
        ImageAsset("ww-wakevulture-x5"        ) ::
        ImageAsset("ww-wakevulture-x5-empty"  ) ::
        ImageAsset("ww-despair"               ) ::
        ImageAsset("ww-board"                 ) ::
    $) ::
    ConditionalAssetsList((factions, options) => factions.of[Whisperers].any, "faction/whisperers")(
        ImageAsset("wg-glyph"                 ) ::
        ImageAsset("wg-title"                 ) ::
        ImageAsset("wg-char"                  ) ::
        ImageAsset("wg-guildwhisperer"        ) ::
        ImageAsset("wg-guildwhisperer-empty"  ) ::
        ImageAsset("wg-guildwhisperer-x5"     ) ::
        ImageAsset("wg-guildwhisperer-x5-empty") ::
        ImageAsset("wg-wglair"                ) ::
        ImageAsset("wg-insight"               ) ::
        ImageAsset("wg-board"                 ) ::
        Whisperers.kinds./(k => ImageAsset("wg-insight-" + k.id))
    ) ::
    ConditionalAssetsList((factions, options) => factions.of[Doomsayers].any, "faction/doomsayers")(
        ImageAsset("du-glyph"                 ) ::
        ImageAsset("du-title"                 ) ::
        ImageAsset("du-char"                  ) ::
        ImageAsset("du-unioncat"              ) ::
        ImageAsset("du-unioncat-empty"        ) ::
        ImageAsset("du-unioncat-x5"           ) ::
        ImageAsset("du-unioncat-x5-empty"     ) ::
        ImageAsset("du-church"                ) ::
        ImageAsset("du-summoningcircle"       ) ::
        ImageAsset("du-board"                 ) ::
        $(BlackDemand, OrangeDemand, YellowDemand)./~(c => c.faces.distinct./(x => ImageAsset("du-die-" + c.id + "-" + x.name.toLowerCase)))
    ) ::
    ConditionalAssetsList((factions, options) => factions.of[Farmer].any, "faction/farmer")(
        ImageAsset("fh-glyph"            ) ::
        ImageAsset("fh-title"            ) ::
        ImageAsset("fh-char"             ) ::
        ImageAsset("fh-hamster"          ) ::
        ImageAsset("fh-hamster-empty"    ) ::
        ImageAsset("fh-hamster-x5"       ) ::
        ImageAsset("fh-hamster-x5-empty" ) ::
        ImageAsset("fh-sheriff"          ) ::
        ImageAsset("fh-sheriff-empty"    ) ::
        ImageAsset("fh-farm"             ) ::
        ImageAsset("fh-windmill"         ) ::
        ImageAsset("fh-grain"            ) ::
        ImageAsset("fh-action"           ) ::
        ImageAsset("fh-board-v2"         ) ::

        ImageAsset("attitude-heart-empty" , "../hero/attitude-heart-empty" ) ::
        ImageAsset("attitude-heart-full"  , "../hero/attitude-heart-full"  ) ::
        ImageAsset("attitude-hostile"     , "../hero/attitude-hostile"     ) ::
    $) ::
    ConditionalAssetsList((factions, options) => factions.of[InvasiveAAA].any, "faction/invasive/card", lzy = Laziness.Later, scale = 50)(
        ImageAsset("frog-ambush") ::
        ImageAsset("frog-engineers") ::
        ImageAsset("favor-of-the-frogs") ::
        ImageAsset("frog-dominance") ::
    $) ::
    ConditionalAssetsList((factions, options) => factions.of[InvasiveAAA].any, "faction/invasive")(
        ImageAsset("td-glyph"            ) ::
        ImageAsset("td-title"            ) ::
        ImageAsset("td-char"             ) ::
        ImageAsset("td-board"            ) ::

        ImageAsset("td-frog"          ) ::
        ImageAsset("td-frog-empty"    ) ::
        ImageAsset("td-frog-x5"       ) ::
        ImageAsset("td-frog-x5-empty" ) ::
        ImageAsset("td-peaceful"      ) ::
        ImageAsset("td-militant"      ) ::

        ImageAsset("td-action"           ) ::
        ImageAsset("td-action-done"      ) ::

        ImageAsset("outcast-fox"         , "../fanatic/outcast-fox") ::
        ImageAsset("outcast-rabbit"      , "../fanatic/outcast-rabbit") ::
        ImageAsset("outcast-mouse"       , "../fanatic/outcast-mouse") ::
    $) ::
    ConditionalAssetsList((factions, options) => factions.of[InvasiveBBB].any, "faction/invasive/card", lzy = Laziness.Later, scale = 50)(
        ImageAsset("frog-ambush") ::
        ImageAsset("favor-of-the-frogs-half") ::
        ImageAsset("frog-dominance-half") ::
        ImageAsset("imitation") ::
        ImageAsset("rabbit-integration") ::
        ImageAsset("mouse-integration") ::
        ImageAsset("fox-integration") ::
        ImageAsset("card-frog-back-art") ::
    $) ::
    ConditionalAssetsList((factions, options) => factions.of[InvasiveBBB].any, "faction/invasive")(
        ImageAsset("ld-glyph"            , "td-glyph"            ) ::
        ImageAsset("ld-title"                                    ) ::
        ImageAsset("ld-char"             , "td-char"             ) ::
        ImageAsset("ld-board"                                    ).scaled(50) ::

        ImageAsset("ld-frog"          , "td-frog"          ) ::
        ImageAsset("ld-frog-empty"    , "td-frog-empty"    ) ::
        ImageAsset("ld-frog-x5"       , "td-frog-x5"       ) ::
        ImageAsset("ld-frog-x5-empty" , "td-frog-x5-empty" ) ::
        ImageAsset("ld-peaceful"      , "td-peaceful"      ) ::
        ImageAsset("ld-militant"      , "td-militant"      ) ::

        ImageAsset("ld-action"        , "td-action"        ) ::
        ImageAsset("ld-action-done"   , "td-action-done"   ) ::

        ImageAsset("outcast-fox"         , "../fanatic/outcast-fox") ::
        ImageAsset("outcast-rabbit"      , "../fanatic/outcast-rabbit") ::
        ImageAsset("outcast-mouse"       , "../fanatic/outcast-mouse") ::
    $) ::
    ConditionalAssetsList((factions, options) => factions.of[InvasiveCCC].any, "faction/invasive/card", lzy = Laziness.Later, scale = 50)(
        ImageAsset("frog-ambush") ::
        ImageAsset("favor-of-the-frogs-half") ::
        ImageAsset("frog-dominance-half") ::
        ImageAsset("imitation") ::
        ImageAsset("laborers") ::
        ImageAsset("peace-talks") ::
        ImageAsset("card-frog-back-art") ::
    $) ::
    ConditionalAssetsList((factions, options) => factions.of[InvasiveCCC].any, "faction/invasive")(
        ImageAsset("ld-glyph"            , "td-glyph"            ) ::
        ImageAsset("ld-title"                                    ) ::
        ImageAsset("ld-char"             , "td-char"             ) ::
        ImageAsset("ld-board-c"                                  ) ::

        ImageAsset("ld-frog"          , "td-frog"          ) ::
        ImageAsset("ld-frog-empty"    , "td-frog-empty"    ) ::
        ImageAsset("ld-frog-x5"       , "td-frog-x5"       ) ::
        ImageAsset("ld-frog-x5-empty" , "td-frog-x5-empty" ) ::
        ImageAsset("ld-peaceful"      , "td-peaceful"      ) ::
        ImageAsset("ld-militant"      , "td-militant"      ) ::

        ImageAsset("ld-action"        , "td-action"        ) ::
        ImageAsset("ld-action-done"   , "td-action-done"   ) ::

        ImageAsset("outcast-fox"         , "../fanatic/outcast-fox") ::
        ImageAsset("outcast-rabbit"      , "../fanatic/outcast-rabbit") ::
        ImageAsset("outcast-mouse"       , "../fanatic/outcast-mouse") ::
    $) ::
    ConditionalAssetsList((factions, options) => factions.of[InvasiveDDD].any, "faction/invasive/card", lzy = Laziness.Later, scale = 50)(
        ImageAsset("card-frog-back-art") ::
        ImageAsset("frog-ambush") ::
        ImageAsset("frog-dominance-half") ::
        ImageAsset("fox-laborers") ::
        ImageAsset("rabbit-laborers") ::
        ImageAsset("mouse-laborers") ::
        ImageAsset("peace-talks-flip") ::
        ImageAsset("incite-conflict") ::
    $) ::
    ConditionalAssetsList((factions, options) => factions.of[InvasiveDDD].any, "faction/invasive")(
        ImageAsset("ld-glyph"            , "td-glyph"            ) ::
        ImageAsset("ld-title"                                    ) ::
        ImageAsset("ld-char"             , "td-char"             ) ::
        ImageAsset("ld-board-d"                                  ) ::

        ImageAsset("ld-frog"          , "td-frog"          ) ::
        ImageAsset("ld-frog-empty"    , "td-frog-empty"    ) ::
        ImageAsset("ld-frog-x5"       , "td-frog-x5"       ) ::
        ImageAsset("ld-frog-x5-empty" , "td-frog-x5-empty" ) ::
        ImageAsset("ld-peaceful"      , "td-peaceful"      ) ::
        ImageAsset("ld-militant"      , "td-militant"      ) ::

        ImageAsset("ld-action"        , "td-action"        ) ::
        ImageAsset("ld-action-done"   , "td-action-done"   ) ::

        ImageAsset("outcast-fox"         , "../fanatic/outcast-fox") ::
        ImageAsset("outcast-rabbit"      , "../fanatic/outcast-rabbit") ::
        ImageAsset("outcast-mouse"       , "../fanatic/outcast-mouse") ::
    $) ::
    ConditionalAssetsList((factions, options) => factions.of[InvasiveEEE].any, "faction/invasive/card", lzy = Laziness.Later, scale = 50)(
        ImageAsset("card-frog-back-art") ::
        ImageAsset("hl-card-frog-back-art") ::
        ImageAsset("frog-ambush", "hl-frog-ambush") ::
        ImageAsset("hl-frog-dominance") ::
        ImageAsset("hl-rabbit-laborers") ::
        ImageAsset("hl-mouse-laborers") ::
        ImageAsset("hl-fox-laborers") ::
        ImageAsset("hl-settlers") ::
        ImageAsset("hl-stewards") ::
        ImageAsset("hl-companions") ::
        ImageAsset("hl-agitators") ::
        ImageAsset("hl-advocates") ::
        ImageAsset("hl-militias") ::
        ImageAsset("hl-assimilationists") ::
        ImageAsset("hl-frog-partisans") ::
    $) ::
    ConditionalAssetsList((factions, options) => factions.of[InvasiveEEE].any, "faction/invasive")(
        ImageAsset("hld-glyph"            , "hld-glyph-meeple"  ) ::
        ImageAsset("hld-title"                                   ) ::
        ImageAsset("hld-char"                                    ) ::
        ImageAsset("hld-board"                                   ) ::

        ImageAsset("hld-frog"                              ) ::
        ImageAsset("hld-frog-empty"                        ) ::
        ImageAsset("hld-frog-x5"                           ) ::
        ImageAsset("hld-frog-x5-empty"                     ) ::
        ImageAsset("hld-peaceful"                          ) ::
        ImageAsset("hld-militant"                          ) ::

        ImageAsset("hld-action"                            ) ::
        ImageAsset("hld-action-done"                       ) ::
    $) ::
    ConditionalAssetsList((factions, options) => factions.of[LegalAAA].any, "faction/legal")(
        ImageAsset("tc-glyph"            ) ::
        ImageAsset("tc-title"            ) ::
        ImageAsset("tc-char"             ) ::
        ImageAsset("tc-board-a"          ).scaled(50) ::
        ImageAsset("tc-board-a-back"     ).scaled(50) ::

        ImageAsset("tc-bat"          ) ::
        ImageAsset("tc-bat-empty"    ) ::
        ImageAsset("tc-bat-x5"       ) ::
        ImageAsset("tc-bat-x5-empty" ) ::

        ImageAsset("tc-commune"      ) ::
        ImageAsset("tc-assembly"     ) ::
        ImageAsset("tc-convened"     ) ::
    $) ::
    ConditionalAssetsList((factions, options) => factions.of[LegalAAA].any, "faction/legal/card", lzy = Laziness.Later)(
        ImageAsset("rouse-loyalists") ::
        ImageAsset("canvass-support") ::
        ImageAsset("overtime-debates") ::
        ImageAsset("pacificy-threats") ::
        ImageAsset("awaken-the-people") ::
        ImageAsset("might-makes-right") ::
        ImageAsset("intensify-pacifism") ::
    $) ::
    ConditionalAssetsList((factions, options) => factions.of[Council].any, "faction/legal")(
        ImageAsset("tc-glyph"            ) ::
        ImageAsset("tc-title"            ) ::
        ImageAsset("tc-char"             ) ::
        ImageAsset("tc-board-a"          ).scaled(50) ::
        ImageAsset("tc-board-a-back"     ).scaled(50) ::

        ImageAsset("tc-bat"          ) ::
        ImageAsset("tc-bat-empty"    ) ::
        ImageAsset("tc-bat-x5"       ) ::
        ImageAsset("tc-bat-x5-empty" ) ::

        ImageAsset("tc-assembly"     ) ::
        ImageAsset("tc-convened"     ) ::
    $) ::
    ConditionalAssetsList((factions, options) => factions.of[CommonAbduct].any, "faction/abduct")(
        ImageAsset("kd-glyph"            ) ::
        ImageAsset("kd-title"            ) ::
        ImageAsset("kd-char"             ).scaled(20) ::
        ImageAsset("kd-board-a"          ) ::

        ImageAsset("kd-skunk"          ) ::
        ImageAsset("kd-skunk-empty"    ) ::
        ImageAsset("kd-skunk-x5"       ) ::
        ImageAsset("kd-skunk-x5-empty" ) ::

        ImageAsset("kd-captain-birdsong" ) ::
        ImageAsset("kd-captain-daylight" ) ::
        ImageAsset("kd-captain-evening"  ) ::

        ImageAsset("kd-captain-birdsong-empty" , "kd-captain-empty" ) ::
        ImageAsset("kd-captain-daylight-empty" , "kd-captain-empty" ) ::
        ImageAsset("kd-captain-evening-empty"  , "kd-captain-empty" ) ::

        // Knaves of the Deepwood: one piece per Captain
        ImageAsset("kd-captain-gladiator"  ) ::
        ImageAsset("kd-captain-cheat"      ) ::
        ImageAsset("kd-captain-jailor"     ) ::
        ImageAsset("kd-captain-harrier"    ) ::
        ImageAsset("kd-captain-ronin"      ) ::
        ImageAsset("kd-captain-adventurer" ) ::
        ImageAsset("kd-captain-scoundrel"  ) ::
        ImageAsset("kd-captain-arbiter"    ) ::
        ImageAsset("kd-captain-vagrant"    ) ::
        ImageAsset("kd-captain-ranger"     ) ::
        ImageAsset("kd-captain-tinker"     ) ::
        ImageAsset("kd-captain-thief"      ) ::
        // the Doomed Expansion's Captains, cut from its stickers by root/tools/doomed-captains.py
        ImageAsset("kd-captain-bard"       ) ::
        ImageAsset("kd-captain-barkeep"    ) ::
        ImageAsset("kd-captain-berserker"  ) ::
        ImageAsset("kd-captain-blacksmith" ) ::
        ImageAsset("kd-captain-swindler"   ) ::
        ImageAsset("kd-captain-zealot"     ) ::

        ImageAsset("kd-acclaim"          ) ::
    $) ::
    // Faction boards shown when a faction's status is tapped (boardImage in ui.scala),
    // from the Tabletop Simulator mod "Root - Ultimate Collection" (Steam workshop 2516434159)
    ConditionalAssetsList((factions, options) => factions.of[Feline].any, "faction/feline", lzy = Laziness.OnDemand)(ImageAsset("mc-board") :: $) ::
    ConditionalAssetsList((factions, options) => factions.of[Aviary].any, "faction/aviary", lzy = Laziness.OnDemand)(ImageAsset("ed-board") :: $) ::
    ConditionalAssetsList((factions, options) => factions.of[Insurgent].any, "faction/insurgent", lzy = Laziness.OnDemand)(ImageAsset("wa-board") :: $) ::
    ConditionalAssetsList((factions, options) => factions.of[Hero].any, "faction/hero", lzy = Laziness.OnDemand)(ImageAsset("vb-board") :: $) ::
    ConditionalAssetsList((factions, options) => factions.of[Trader].any, "faction/trader", lzy = Laziness.OnDemand)(ImageAsset("rf-board") :: $) ::
    ConditionalAssetsList((factions, options) => factions.of[Fanatic].any, "faction/fanatic", lzy = Laziness.OnDemand)(ImageAsset("lc-board") :: $) ::
    ConditionalAssetsList((factions, options) => factions.of[Mischief].any, "faction/mischief", lzy = Laziness.OnDemand)(ImageAsset("cc-board") :: $) ::
    ConditionalAssetsList((factions, options) => factions.of[Underground].any, "faction/underground", lzy = Laziness.OnDemand)(ImageAsset("ud-board") :: $) ::
    ConditionalAssetsList((factions, options) => factions.of[Horde].any, "faction/horde", lzy = Laziness.OnDemand)(ImageAsset("lh-board") :: $) ::
    ConditionalAssetsList((factions, options) => factions.of[Expedition].any, "faction/expedition", lzy = Laziness.OnDemand)(ImageAsset("ki-board") :: $) ::
    ConditionalAssetsList((factions, options) => factions.of[Council].any, "faction/legal", lzy = Laziness.OnDemand)(ImageAsset("tc-board-h") :: $) ::
    ConditionalAssetsList((factions, options) => factions.of[Knaves].any, "faction/abduct", lzy = Laziness.OnDemand)(ImageAsset("kd-board-h") :: $)

    val mapPaths = $("autumn", "winter", "lake", "mountain", "tidal", "tundra", "marsh", "gloom", "urban", "legends-map", "deep-woods", "wastelands", "gorge")
    override def deferrable(a : ConditionalAssetsList) = mapPaths.has(a.path)

    override def intLinks = $(("Root: Advanced Setup".spn -> "root-adset"), ("Root: Mirror".spn -> "root-mirror"))
}


// A rules version stands for the rules fixes every new game played with when it was made. Each fix is still a hidden option of its own
// (the games started before it don't have it, and the switch actions, `...Action(X)`, turn it on in a running game), but a new game
// records only its rules version, which Game.options expands to the fixes: before 2026-11-15 every new game recorded each of them,
// and its log opened with a line for each. A fix made later goes into a new version (RulesVersion2 = RulesVersion1's fixes and it),
// made `latest`; the games that recorded an earlier version keep its fixes, so they replay the same.
abstract class RulesVersion(val n : Int) extends GameOption with ToggleOption {
    val group = "Rules"
    val valueOn = ("Rules version " + n).hh
    def fixes : $[Meta.O]
}

case object RulesVersion1 extends RulesVersion(1) {
    def fixes = Meta.settled ++ $[Meta.O](FrogCardsAfterStartingHands, FrogCardsSkipLostSouls, FrogsCraftByClearing, LizardsCraftByClearing, CompanionsUntilSpent, HirelingsByTheCard, GovernorsStopEnclaveFlips, CounterMiceInABush, SkipIdleRallyReconcile, FrogAmbushBottomOnlyAsAmbush, CouncilRevealOnce, EntreatGainUnasked, BanishNeedsWarriors, FeatherRufflersOnCraft, SilverTongueOneEffect, GovernorsStopCrafting, MouseholdFromSupply, BuyServicesAgain, BanishAfterIgnoredHits, CouncilEveningClearPath, ReconcileOneAtATime, FearsAfterRemoval, RiverfolkCraftUngoverned, PrisonersFromCodefenders, GovernorsStopExposure, FrogCardsAtDiasporaSetup, HitsAndAmbushesSetup, DuskDeckFixes)
}

// Banish moves the Knaves' Captains last (2026-11-18)
case object RulesVersion2 extends RulesVersion(2) {
    def fixes = RulesVersion1.fixes ++ $[Meta.O](BanishCaptainsLast)
}

// The Mountain's suited landmark may be of the Tower clearing's own suit (2026-11-19)
case object RulesVersion3 extends RulesVersion(3) {
    def fixes = RulesVersion2.fixes ++ $[Meta.O](SuitedLandmarkAnySuit)
}

// The Knaves pick whether the Nab move or the Diaspora's Fears Come to Pass resolves first (2026-11-20)
case object RulesVersion4 extends RulesVersion(4) {
    def fixes = RulesVersion3.fixes ++ $[Meta.O](NabFearsOrder)
}

// A Charismatic Recruit with one warrior left places it (2026-11-25)
case object RulesVersion5 extends RulesVersion(5) {
    def fixes = RulesVersion4.fixes ++ $[Meta.O](CharismaticPlacesWhatItCan)
}

object RulesVersion {
    val all : $[RulesVersion] = $(RulesVersion1, RulesVersion2, RulesVersion3, RulesVersion4, RulesVersion5)
    def latest : RulesVersion = RulesVersion5
    def implied : $[Meta.O] = all./~(_.fixes).distinct

    def expand(l : $[Meta.O]) : $[Meta.O] = l ++ l.of[RulesVersion]./~(_.fixes).distinct.diff(l)
}
