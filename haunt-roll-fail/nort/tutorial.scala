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


// The main menu's Tutorial: lessons on the rules and on this site, then a practice game as the Stag Clan against an Easy bot,
// with a tip at the top of the action pane for every choice (Tutorial.tip, shown by preinfo in ui.scala)
case object TutorialOption extends GameOption with ToggleOption {
    val group = "Tutorial".txt
    def valueOn = "Tutorial tips".txt
}

// A lesson opened from the in-game "Tutorial Lessons" button (UI.onClick)
case class TutorialLessonView(n : Int)

object Tutorial {
    // The clan the tutorial teaches with, and the practice game's opponent
    val clan : Faction = Stag
    val opponent : Faction = Wolf

    // The practice game: Stag goes first, against the Easy bot, with the standard rules
    def seating : $[Faction] = $(clan, opponent)
    def bots : Map[Faction, String] = Map(opponent -> "Easy")
    def options : $[GameOption] = Meta.defaultsFor(2, seating).%(_.is[VictoryModeOption].not) ++ $(TutorialOption, FirstSeatStarts)

    // Images in the lessons (Meta.menuImages has their URLs for the menus; in the game they are assets)
    def card(n : String) = Image("card-start-blue-" + n, styles.menuCard)
    def clanCard(n : Int) = Image(ClanCard(clan, n).info.image, styles.menuCard)
    def building(b : String) = Image("building-" + b, styles.inlineIcon)
    def icon(s : String) = Image("ui-" + s, styles.inlineIcon)

    val food = icon("food")
    val wood = icon("wood")
    val lore = icon("lore")
    val fame = icon("fame")

    val stag = "Stag Clan".styled(Stag)(xstyles.bold)

    def p(e : Elem) : Elem = e.div(styles.tutorialText)
    def site(e : Elem) : Elem = ("On this site: ".hl ~ e).div(styles.tutorialText)(styles.tutorialSite)

    // Each lesson: its title and its paragraphs
    val lessons : $[(Elem, $[Elem])] = $(
        ("Welcome".txt, $(
            p("Northgard: Uncharted Lands".hl ~ " is a board game for 2 to 6 players. Each player leads a Viking clan that lands on an unknown island: you explore it tile by tile, settle territories, build, fight and gather " ~ fame ~ " fame."),
            p("The game lasts " ~ "7 years".hl ~ " (rounds). After the seventh year the player with the most fame wins. A clan can also win early: at the end of any year, " ~ "control three closed territories that each have a large building".hl ~ "."),
            p("In this tutorial you play the " ~ stag ~ ". These lessons cover the rules and how to play them on this site; the practice game at the end gives a tip for every choice you make."),
            site("Northgard runs in your browser at " ~ "games.clean5110.com".hl ~ ", on a computer, tablet or phone. There is nothing to install, and you don't need an account."),
        )),
        ("The Stag Clan".txt, $(
            Div(clanCard(0) ~ clanCard(1) ~ clanCard(2), styles.menuText),
            p("Every clan has a " ~ "clan ability".hl ~ ", a clan card that starts in its deck and two clan upgrades it can buy during the game."),
            p("Stag's ability: " ~ "+1 fame".hl ~ " for every territory you " ~ "conquer in combat".hl ~ " and every territory you " ~ "close by exploring".hl ~ ". The Stag rewards bold play: explore to close territories, and take land from your neighbours."),
            p("Stag Clan".hl ~ " card (a Flash card): Recruit 1, then you may move 1 of your units from an adjacent territory into the one you recruited in (even across a Rough border)."),
            p("Upgrades: " ~ "Glory of the Clan".hl ~ " (Build, and collect that territory's resources as if it were the Harvest) and " ~ "Annexation".hl ~ " (Move 1 and Explore, a Flash card). With the Warchiefs module, " ~ "Brand's Bravery".hl ~ " is a third one."),
            site("tap the " ~ "i".hl ~ " beside a clan in the clan picker, or a clan's name in its player panel during a game, to see its cards."),
        )),
        ("The map".txt, $(
            p("The island is made of square " ~ "tiles".hl ~ ". The dotted lines on them are " ~ "borders".hl ~ " that split the land into " ~ "territories".hl ~ ". A territory can stretch over several tiles: its borders continue from one tile to the next."),
            p("A territory with your units in it is " ~ "yours".hl ~ " (you control it). One with no units is " ~ "neutral".hl ~ ", even if it has buildings."),
            p("A territory is " ~ "closed".hl ~ " once tiles surround it on every side, and " ~ "open".hl ~ " while one of its edges is still at the edge of the map. Closed territories give fame."),
            p("White dotted borders are regular. Yellow dashed borders are " ~ "Rough".hl ~ ": crossing one costs 2 moves."),
            p("Territories show resources (" ~ food ~ " food, " ~ wood ~ " wood, " ~ lore ~ " lore) and building spaces: small round ones, large ones, and Carved Stone spaces."),
            site("each territory is tinted with its owner's color, and has a number for the log and the menus. When a choice is a territory, tap it on the map (or in the list). Tap a creature, warchief or token to see its card."),
        )),
        ("Your cards".txt, $(
            Div(card("recruit") ~ card("move") ~ card("explore") ~ card("build") ~ card("feast"), styles.menuText),
            p("Your actions come from " ~ "cards".hl ~ ". Your deck starts with six cards, " ~ "Recruit, Move, Explore, Build".hl ~ " and two " ~ "Feasts".hl ~ ", plus your " ~ "Stag Clan".hl ~ " card."),
            p("At the start of each year you draw " ~ "4 cards".hl ~ ". When your draw pile runs out, your discard pile is shuffled into a new one."),
            p("Cards with a lightning bolt are " ~ "Flash".hl ~ " cards: you may play any number of them before or after your main card."),
            p("Development cards".hl ~ " are new, stronger cards you add to your deck during the game (see the next lessons)."),
            site("your hand is at the bottom of the action pane. The icons at the top of your player panel count the cards " ~ "to draw".hl ~ ", " ~ "in hand".hl ~ ", " ~ "played this year".hl ~ " and " ~ "discarded".hl ~ "; tap the last two to see those cards."),
        )),
        ("A year".txt, $(
            Image("card-aid-phases", styles.menuBoard),
            p("1. Start of year".hl ~ ": everyone draws 4 cards, and one " ~ "Development card".hl ~ " per player is revealed."),
            p("2. Actions".hl ~ ": starting with the first player, everyone takes one action in turn, again and again, until everyone has passed."),
            p("3. Harvest".hl ~ ": you score fame for your closed territories and collect resources; then you may trade."),
            p("4. Winter".hl ~ ": you pay food (and wood) for your units."),
            p("5. End of year".hl ~ ": a clan with three closed territories that each hold a large building wins. Otherwise a new year starts, until the seventh is over."),
            site("the folded bar above the panels starts with " ~ "Year X of 7".hl ~ ", and the log on the right says what everyone did."),
        )),
        ("Your turn".txt, $(
            p("On your turn you do " ~ "one".hl ~ " of these:"),
            p("Play cards".hl ~ ": play one card from your hand and do what it says (plus any Flash cards). It goes to your played cards."),
            p("Wait".hl ~ ": put a card with your played cards without using it. Useful to let others act first."),
            p("Replace".hl ~ " for 1 " ~ lore ~ ": put a card with your played cards and draw 1."),
            p("Remove".hl ~ " for 2 " ~ lore ~ ": remove a weak card from the game for good and draw 2."),
            p("Upgrade".hl ~ " for 3 " ~ lore ~ ": take one of your clan upgrades into your hand."),
            p("Pass".hl ~ ": you are done for the year. Your hand and played cards are discarded, and you pick one of the revealed " ~ "Development cards".hl ~ ": it goes on top of your draw pile, so you draw it next year. The first to pass picks first and is the first player next year."),
            site("your turn starts with six buttons at the top of the action pane. Tap one, then tap a card in your hand to do it. A dimmed choice says why it isn't possible; " ~ "Cancel".hl ~ " goes back."),
        )),
        ("The actions".txt, $(
            p("Recruit N".hl ~ ": place N new units in your territories (in any neutral one if you have none). You have 14 units."),
            p("Move N".hl ~ ": N times, move any number of units from one of your territories across one border. Entering an enemy territory stops them and starts a fight."),
            p("Explore".hl ~ ": draw a tile and add it to the map next to one of your open territories. Its borders must continue the ones it touches, and it can't join two players' territories. Closing your territories this way gives fame (the " ~ stag ~ " gets 1 more)."),
            p("Build".hl ~ ": in your territory, on a free space. Small buildings cost 1 " ~ wood ~ ": " ~ building("food-silo") ~ " Food Silo (+1 food), " ~ building("woodcutter-lodge") ~ " Woodcutter Lodge (+1 wood), " ~ building("defense-tower") ~ " Defense Tower, " ~ building("training-camp") ~ " Training Camp, " ~ building("carved-stone") ~ " Carved Stone (+1 lore). Large ones cost 3 " ~ wood ~ ": " ~ building("fortress") ~ " Fortress, " ~ building("forge") ~ " Forge (+1 card a year), " ~ building("altar-of-kings") ~ " Altar of Kings (+3 fame each Harvest)."),
            p("Feast".hl ~ ": Recruit 1, Move 1, Explore or Build, as you choose."),
            site(Div(
                "Recruit and Move: tap the territories on the map. " ~
                "Explore: you see the tile first; tap a spot, turn it with the blue arrows, then tap the green " ~ Image("ui-confirm", styles.inlineIcon) ~ " check mark (the red " ~ Image("ui-cancel", styles.inlineIcon) ~ " cross cancels). " ~
                "Build: tap a free space, pick a building from the menu (it shows the cost and how many are left), then confirm with the check mark.")),
        )),
        ("Combat".txt, $(
            p("Moving into an enemy territory starts a fight. Each side counts " ~ CombatIcon.axe ~ " combat points: 1 per unit, plus the Move card's bonus for the attacker and the buildings for the defender (a Fortress gives 2)."),
            p("Then each side may spend " ~ food ~ " food, +1 point each (at most one per unit), and rolls the battle die. The die gives points " ~ CombatIcon.axe ~ " and casualties " ~ CombatIcon.skull ~ ": each " ~ CombatIcon.skull ~ " removes one enemy unit."),
            p("The higher total wins; " ~ "ties go to the defender".hl ~ ". A side that loses all its units loses. The loser retreats to a neighbouring territory of its own or a neutral one."),
            p("The " ~ stag ~ " gains " ~ "+1 fame".hl ~ " for each territory it conquers in combat."),
            site("after the roll, a " ~ "combat report".hl ~ " shows both sides' points and losses; tap " ~ "OK".hl ~ ". To retreat, tap where to go, then who goes. You can turn the reports off under " ~ "Interface".hl ~ " (Combat Report), and the word for how a fight ended in the log (\"won\", \"defeated\") opens its report again."),
        )),
        ("Harvest and Winter".txt, $(
            p("Harvest".hl ~ ": each closed territory you control gives " ~ fame ~ " 1 fame (2 if it covers 3 tiles or more), each Altar of Kings 3 fame. You collect a resource for each icon in your territories and for your Food Silos, Woodcutter Lodges and Carved Stones."),
            p("Then you may " ~ "trade".hl ~ " any 3 resources for 1 of your choice, as often as you like."),
            p("Winter".hl ~ ": your units must eat. 1 to 3 units are free, 4 to 6 cost 1 " ~ food ~ ", 7 to 9 cost 2 " ~ food ~ ", 10 to 12 cost 3 " ~ food ~ " and 1 " ~ wood ~ ", 13 or more 4 " ~ food ~ " and 2 " ~ wood ~ ". If you can't pay, you take an " ~ "Unrest".hl ~ " card: it clogs your hand and costs 5 fame at the end."),
            site("the small table in your player panel shows your resources and fame, then your next harvest " ~ SeasonIcon.harvest ~ " (green) and your winter cost " ~ SeasonIcon.winter ~ " (red), as things stand. Tap it for the whole winter chart. Other players' fame shows as " ~ "?".hl ~ ": fame tokens are kept face down. While trading, tap the trades you want, then " ~ "Done trading".hl ~ "."),
        )),
        ("Winning".txt, $(
            p("At the end of each year, a clan controlling " ~ "three closed territories each with a large building".hl ~ " (Fortress, Forge or Altar of Kings) wins at once."),
            p("Otherwise, after the 7th year, the most " ~ fame ~ " fame wins: your fame tokens, plus the fame printed on your Development and Achievement cards, plus 1 for every 3 resources left, minus 5 for each Unrest card. In the last year the revealed cards are " ~ "Achievements".hl ~ " instead of Developments."),
            p("Tips for the " ~ stag ~ ": recruit early so you can spread out; explore to close territories (2 fame each for you, more for big ones); keep food for winter; and watch for weak neighbours to conquer."),
        )),
        ("Using this site".txt, $(
            p("The main menu".hl ~ ": " ~ "Quick Game".hl ~ " (you and two bots), " ~ "Local Game".hl ~ " (pick the clans, people and bots; people take turns on one device), " ~ "Solo vs Automa".hl ~ ", " ~ "Training Grounds".hl ~ " (a short two-player duel), " ~ "Adset".hl ~ " (clans drafted during setup) and " ~ "Play Online".hl ~ "."),
            p("Play Online".hl ~ ": set up the game, then send each player " ~ "their own link".hl ~ ". The link is your seat: keep it private, and use it to come back at any time; the game waits on the server. The " ~ "Spectator".hl ~ " link lets others watch."),
            p("Local games".hl ~ " are saved in your browser as you play; the menu offers to continue them."),
            p("The screen".hl ~ ": the map on the left; on the right the player panels, the log and the action pane. Drag the round handles on the dividers to resize them (double-tap to reset). The strip of shared cards folds with the tab on its left."),
            p("Looking back and undo".hl ~ ": tap a line in the log to see the game at that point; " ~ "Undo to here".hl ~ " takes the game back to it, and " ~ "Back".hl ~ " returns to the present."),
            p("Settings".hl ~ ": " ~ "Interface".hl ~ ", under the action pane, sets the territory tints, the panel layout, combat reports and the font size. " ~ "Report a Bug".hl ~ " is next to it."),
        )),
        ("The practice game".txt, $(
            p("You play the " ~ stag ~ " against the " ~ "Wolf Clan".styled(Wolf)(xstyles.bold) ~ ", played by the Easy bot. You go first. It is a full game of 7 years with the standard rules."),
            p("A " ~ "tip".hl ~ " at the top of the action pane explains each choice as it comes up. " ~ "Hide tips".hl ~ " turns them off; " ~ "Tutorial Lessons".hl ~ " under the action pane opens these lessons again."),
            p("The practice game is saved in your browser, so you can stop and continue it later from the Tutorial menu."),
        )),
    )

    def about : Elem =
        ("Learn to play " ~ "Northgard: Uncharted Lands".hl ~ " as the " ~ stag ~ ", and how to play it on this site. Read the lessons, then try a practice game against a bot, with a tip for every choice.").div

    // The tip for the choices f is offered; None when there is nothing to say
    def tip(game : Game, f : Faction, actions : $[UserAction]) : |[Elem] = {
        val l = actions./(_.unwrap)

        def has[T : scala.reflect.ClassTag] = l.exists(_.is[T])

        val st = game.states.get(f)

        if (has[SetupTileAction])
            |("Setup: you drew 3 map tiles. Pick one to add next to the starting tile. Look at what it offers: " ~ food ~ " food and " ~ wood ~ " wood to start with, building spaces, and borders that will be easy to close. Tap a tile to pick it.")
        else
        if (has[SetupSpotAction] || has[ExploreSpotAction] || has[SecondChanceSpotAction])
            |("Tap one of the marked spots on the map (or in the list) to place the tile there. Its borders must continue the ones they touch, so only some spots and turns are allowed.")
        else
        if (has[SetupTurnAction] || has[ExploreTurnAction] || has[SecondChanceTurnAction])
            |("The tile is shown in place. Turn it with the blue arrows above it, then tap the green check mark below it to place it (or the red cross to pick another spot).")
        else
        if (has[SetupAreaAction] || has[SetupUnitsAction])
            |("Place your 3 starting units: tap a territory of your new tile. A territory with " ~ food ~ " food or " ~ wood ~ " wood icons gives them at every Harvest, and one that is nearly closed is easy to close for fame.")
        else
        if (has[TurnModeAction]) {
            val n = st./(_.hand.num).|(0)
            val lr = st./(_.lore).|(0)
            |(("Your turn, year " ~ game.year.hl ~ ". You have " ~ n.hl ~ " card" ~ (n != 1).??("s") ~ " in hand. ").div ~
                "Tap " ~ "Play cards".hl ~ " to use a card, " ~ "Wait".hl ~ " to set one aside, or " ~ "Pass".hl ~ " when you're done for the year (the first to pass picks a Development card first). " ~
                (lr >= 3).?("You have " ~ lr.hl ~ " " ~ lore ~ " lore: " ~ "Upgrade".hl ~ " would get you a Stag clan upgrade. ").|(Empty) ~
                (n == 0).?("Your hand is empty, so Pass is your only choice.".hl).|(Empty))
        }
        else
        if (has[PlayCardAction] && has[EndTurnAction])
            |("You may play Flash cards (the ones with a lightning bolt) now, or tap " ~ "End turn".hl ~ ".")
        else
        if (has[PlayCardAction])
            |("Tap a card in your hand to play it. A dimmed card can't be played now; it says why. Tap a card's picture after your turn to see it full size.")
        else
        if (has[WaitCardAction])
            |("Tap the card to wait with: it goes with your played cards, unused, and comes back after the next shuffle.")
        else
        if (has[ReplaceCardAction])
            |("Tap the card to replace: it goes with your played cards and you draw a new one, for 1 " ~ lore ~ ".")
        else
        if (has[RemoveCardAction])
            |("Tap the card to remove from the game for good, for 2 " ~ lore ~ ". A thinner deck draws your best cards more often.")
        else
        if (has[UpgradePickAction])
            |("Pick the Stag clan upgrade to take into your hand. " ~ "Annexation".hl ~ " moves and explores in one Flash card; " ~ "Glory of the Clan".hl ~ " builds and harvests that territory at once.")
        else
        if (has[UpgradeSacrificeAction] || has[UpgradeCardAction])
            |("An upgrade also costs a card from your hand: it either waits (goes with your played cards) or is removed from the game.")
        else
        if (has[StagPullFromAction])
            |("Stag Clan".hl ~ ": you may now move 1 of your units from an adjacent territory into the one you recruited in. Tap where it comes from, or skip.")
        else
        if (has[RecruitAreaAction] || has[RecruitPlaceAction])
            |("Recruit: tap one of your territories to place a new unit there. More units hold more land and win fights, but cost more food in Winter (4 or more units cost food).")
        else
        if (has[FeastChoiceAction])
            |("Feast: choose one action. Early on, Recruit or Explore help most; Build when you have " ~ wood ~ " wood and a good space.")
        else
        if (has[MoveFromAction])
            |("Move: tap the territory your units leave from. Each move crosses one border; a Rough (yellow) border costs 2. " ~ "Done".hl ~ " ends the card's moves.")
        else
        if (has[MoveToAction])
            |("Tap where they go. A neutral territory becomes yours; an enemy one starts a fight once all moves are made (the Stag gets +1 fame for each conquest).")
        else
        if (has[MoveUnitsAction])
            |("Pick how many units go. Leave at least one behind to keep the territory they leave.")
        else
        if (has[ExploreTileAction] || has[ExploreTileInfoAction])
            |("Explore: here is the tile you drew. It must go next to one of your open territories. Closing a territory of yours gives fame for each of its tiles, and the Stag gets 1 more.")
        else
        if (has[BuildSpotAction])
            |("Build: tap a free building space in one of your territories. Small spaces take small buildings (1 " ~ wood ~ "), large spaces large ones (3 " ~ wood ~ ").")
        else
        if (has[BuildPickAction])
            |("Pick a building: dimmed ones can't go there and say why. A Food Silo or Woodcutter Lodge pays every Harvest; a large building in a closed territory counts toward the early win.")
        else
        if (has[BuildConfirmAction])
            |("The building is shown on its space: tap the green check mark above it to build it, or the red cross to choose again.")
        else
        if (has[FightAction])
            |("Several fights: tap the territory to fight in first.")
        else
        if (has[CombatFoodAction])
            |("Spend " ~ food ~ " food for this fight? Each one gives +1 combat point (at most one per unit). Remember Winter: your units must eat.")
        else
        if (has[CombatChooseAction])
            |("Your die shows a choice: 1 combat point " ~ CombatIcon.axe ~ " or 1 casualty " ~ CombatIcon.skull ~ " (one enemy unit removed). Pick what helps more.")
        else
        if (has[CombatReportDoneAction])
            |("The combat report: who won, each side's points and losses. Tap " ~ "OK".hl ~ " to go on. (Interface > Combat Report can skip these.)")
        else
        if (has[RetreatPickAction])
            |("You lost this fight: tap where your units retreat to (your territory or a neutral one), then who goes.")
        else
        if (has[BrandRetreatPickAction])
            |("Brand's Bravery".hl ~ ": you choose where the beaten enemy retreats. Pick the place that helps you most.")
        else
        if (has[TradeForAction] || has[TradeDoneAction])
            |("Harvest trading: pay any 3 resources for 1 of your choice, as often as you like. Check your Winter cost in your panel (the red row) first. Tap " ~ "Done trading".hl ~ " when finished.")
        else
        if (has[PickCardAction])
            |("You passed: pick a Development card. It goes on top of your draw pile, so you draw it at the start of next year. Tap a card to see it full size first.")
        else
        if (has[KeepDrawnAction] || has[DiscardDrawnAction] || has[ReturnDrawnAction])
            |("Choose what to do with each card you drew, as the card says.")
        else
        if (has[ReturnUnitsToAction])
            |("You have no units left on the map: place 3 new ones in a neutral territory.")
        else
            None
    }

    // While the bot plays
    def waiting(game : Game) : Elem =
        ("The " ~ "Wolf Clan".styled(Wolf)(xstyles.bold) ~ " (the Easy bot) is playing. The log on the right says what it does; tap a line in it to look back.")
}
