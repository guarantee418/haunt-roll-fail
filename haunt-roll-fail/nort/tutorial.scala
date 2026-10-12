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
    def options : $[GameOption] = Meta.defaultsFor(2, seating).%(_.is[VictoryModeOption].not) ++ $(TutorialOption, FirstSeatStarts, ChooseCasualties, TakeUnrestCard, ClanPowerReports, MendFigures, OxPicksSpace)

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

    // Each lesson: its title and its paragraphs. The order is the owner's teaching plan (2026-10-10): the goal, the map
    // (territories, open and closed, the part new players find hardest), the deck and the year, the actions, harvest and
    // winter, combat (also explained in the practice game when the first fight comes), and the clan last, as a twist on the rules
    val lessons : $[(Elem, $[Elem])] = $(
        ("How you win".txt, $(
            p("Northgard: Uncharted Lands".hl ~ " is a board game for 2 to 6 players. Each player leads a Viking clan settling an unknown island: you explore it tile by tile, take land, build, fight and gather " ~ fame ~ " fame."),
            p("Start with the goal, so every other rule has a reason. There are " ~ "two ways to win".hl ~ ":"),
            p("1. Sudden death".hl ~ ": at the end of " ~ "any".hl ~ " year, a clan that controls " ~ "three closed territories that each have a large building".hl ~ " (Fortress, Forge or Altar of Kings) wins at once. This really happens: keep an eye on your opponents' closed territories and large buildings, every year."),
            p("2. Fame".hl ~ ": otherwise, after the " ~ "seventh year".hl ~ ", the clan with the most fame wins."),
            p("In this tutorial you play the " ~ stag ~ ". The lessons go in the order a good teacher would use; the practice game at the end gives a tip for every choice you make."),
            site("Northgard runs in your browser at " ~ "games.clean5110.com".hl ~ ", on a computer, tablet or phone. There is nothing to install, and you don't need an account."),
        )),
        ("The map: territories".txt, $(
            p("This is the idea to take slowly. The island is built from square " ~ "tiles".hl ~ ". The dotted lines are " ~ "borders".hl ~ ", and the land inside them is a " ~ "territory".hl ~ ". A territory can stretch over several tiles: its borders carry on from one tile to the next."),
            Image("tile-start", styles.menuWarchiefCard),
            p("The starting tile, above, has " ~ "four territories".hl ~ ". In the game every territory has a number on the map."),
            p("Control".hl ~ ": a territory is yours when you have " ~ "at least one unit".hl ~ " in it. With no units it is " ~ "neutral".hl ~ ", even if it has buildings."),
            p("Open or closed".hl ~ ": a territory is " ~ "open".hl ~ " while one of its edges still runs to the edge of the map, where a new tile could extend it. Once tiles surround it on every side it is " ~ "closed".hl ~ ": fully bounded, it can't grow any more. All four territories of the starting tile are open at first."),
            p("Closed territories are what matter".hl ~ ": they give you fame at every Harvest, and the sudden-death win needs three of them. Exploring next to a territory is how it gets closed."),
            p("White dotted borders are regular. Yellow dashed borders are " ~ "Rough".hl ~ ": crossing one costs 2 moves. Territories also show resources (" ~ food ~ " food, " ~ wood ~ " wood, " ~ lore ~ " lore) and building spaces."),
            site("each territory is tinted with its owner's color and has a number. Closed territories that give fame get their border drawn in their owner's color. When a choice is a territory, tap it on the map (or in the list)."),
        )),
        ("Your deck and the year".txt, $(
            Div(card("recruit") ~ card("explore") ~ card("move") ~ card("build") ~ card("feast"), styles.menuText),
            p("Your deck starts with six cards, " ~ "Recruit, Explore, Move, Build".hl ~ " and two " ~ "Feasts".hl ~ ", plus your clan card."),
            p("If you know other deck-builders, note the difference: you " ~ "don't play your whole hand".hl ~ " on your turn. At the start of each year everyone draws " ~ "4 cards".hl ~ ". Then players take turns doing " ~ "one thing at a time".hl ~ ", around and around, until everyone has passed."),
            p("Passing".hl ~ " matters. When you pass, your hand and played cards are discarded and you pick one of the revealed " ~ "Development cards".hl ~ ", new and stronger cards. It goes " ~ "on top of your draw pile".hl ~ ", so you draw it next year. The first to pass picks first and takes the " ~ "First Player marker".hl ~ "."),
            p("That's the tension of every year: " ~ "pass early".hl ~ " for the best card and to go first, or " ~ "stay in".hl ~ " for more actions."),
            Image("card-aid-phases", styles.menuBoard),
            p("A year: " ~ "1. Start of year".hl ~ " (draw 4, Developments revealed), " ~ "2. Actions".hl ~ " (turns until everyone passes), " ~ "3. Harvest".hl ~ ", " ~ "4. Winter".hl ~ ", " ~ "5. End of year".hl ~ " (check the sudden-death win)."),
            site("the counts at the top of your player panel are the cards " ~ "to draw".hl ~ ", " ~ "in hand".hl ~ ", " ~ "played this year".hl ~ " and " ~ "discarded".hl ~ " (tap the last two to see those cards). The revealed Development cards are in the strip at the top; the bar above the panels shows " ~ "Year X of 7".hl ~ "."),
        )),
        ("The four actions".txt, $(
            p("Your Starting cards show the four core actions:"),
            p("Recruit N".hl ~ ": place N new units in your territories (in any neutral one if you have none). You have 14 units."),
            p("Explore".hl ~ ": draw a tile and add it to the map next to one of your open territories. Its borders must continue the ones it touches, and it can't join two players' territories. Closing your own territories this way gives fame."),
            p("Move N".hl ~ ": N times, move any number of units from one of your territories across one border. Entering an enemy territory starts a fight."),
            p("Build".hl ~ ": in your territory, on a free space. Small buildings cost 1 " ~ wood ~ ": " ~ building("food-silo") ~ " Food Silo (+1 food), " ~ building("woodcutter-lodge") ~ " Woodcutter Lodge (+1 wood), " ~ building("defense-tower") ~ " Defense Tower, " ~ building("training-camp") ~ " Training Camp, " ~ building("carved-stone") ~ " Carved Stone (+1 lore). Large ones cost 3 " ~ wood ~ ": " ~ building("fortress") ~ " Fortress, " ~ building("forge") ~ " Forge (+1 card a year), " ~ building("altar-of-kings") ~ " Altar of Kings (+3 fame each Harvest)."),
            p("Feast".hl ~ " is wild: Recruit 1, Move 1, Explore or Build."),
            p("Flash".hl ~ " cards (a lightning bolt) can be played in any number, before and after your main card."),
            p("Lore".hl ~ " " ~ lore ~ " lets you replace a card (1), remove a card from your deck (2) or take a clan upgrade (3). That's for after your first year: don't worry about it yet."),
            site(Div(
                "your turn starts with six buttons: " ~ "Play cards".hl ~ ", " ~ "Wait".hl ~ " (set a card aside unused), the three lore choices, and " ~ "Pass".hl ~ ". Tap one, then tap a card in your hand. " ~
                "Recruit and Move: tap the territories on the map. Explore: you see the tile first; tap a spot, turn it with the blue arrows, then tap the green " ~ Image("ui-confirm", styles.inlineIcon) ~ " check mark (the red " ~ Image("ui-cancel", styles.inlineIcon) ~ " cross cancels). " ~
                "Build: tap a free space, pick a building from the menu, then confirm with the check mark.")),
        )),
        ("Harvest and Winter".txt, $(
            p("Harvest".hl ~ " is the payoff. Each closed territory you control gives " ~ fame ~ " 1 fame (2 if it covers 3 tiles or more), each Altar of Kings 3 fame. You collect a resource for every icon in your territories and on your buildings. Then you may trade any 3 resources for 1."),
            p("Winter".hl ~ " is the bill: your units must eat."),
            p("1 to 3 units: free. 4 to 6: 1 " ~ food ~ ". 7 to 9: 2 " ~ food ~ ". 10 to 12: 3 " ~ food ~ " and 1 " ~ wood ~ ". 13 or more: 4 " ~ food ~ " and 2 " ~ wood ~ "."),
            p("Watch out: this is where new players get punished.".styled(xstyles.warning) ~ " If you can't pay, you take an " ~ "Unrest".hl ~ " card: it clogs your deck (a dead card in your hand) and costs " ~ "5 fame".hl ~ " at the end. Don't recruit more units than your food can feed."),
            site("the small table in your player panel shows your resources and fame, then your next harvest " ~ SeasonIcon.harvest ~ " (green) and your winter cost " ~ SeasonIcon.winter ~ " (red), as things stand right now. Check it before you recruit. Tap it for the whole winter chart. Other players' fame shows as " ~ "?".hl ~ ": fame is kept face down."),
        )),
        ("Combat".txt, $(
            p("You can leave this until your first fight: the practice game explains it then, step by step."),
            p("Moving into an enemy territory starts a fight. Each side counts " ~ CombatIcon.axe ~ " combat points: " ~ "1 per unit".hl ~ ", plus the " ~ "Move card's bonus".hl ~ " for the attacker or the " ~ "buildings".hl ~ " for the defender (a Fortress gives 2)."),
            p("Then each side may spend " ~ food ~ " food, +1 point each (at most one per unit), and rolls " ~ "one die".hl ~ ". The die gives points " ~ CombatIcon.axe ~ " and casualties " ~ CombatIcon.skull ~ ": each " ~ CombatIcon.skull ~ " removes one enemy unit."),
            p("Two things to remember: " ~ "ties go to the defender".hl ~ ", and a side that " ~ "loses all its units".hl ~ " to casualties loses the fight, whatever the score. The loser retreats to a neighbouring territory of its own or a neutral one."),
            site("after the roll, a " ~ "combat report".hl ~ " shows both sides' points and losses; tap " ~ "OK".hl ~ ". To retreat, tap where to go, then who goes. Interface > Combat Report can skip the reports, and the word for how a fight ended in the log (\"won\", \"defeated\") opens its report again."),
        )),
        ("Your clan: the Stag".txt, $(
            Div(clanCard(0) ~ clanCard(1) ~ clanCard(2), styles.menuText),
            p("Once you know the base game, your clan is " ~ "your twist on the rules".hl ~ ". Each player only needs to read their own."),
            p("Stag's ability: " ~ "+1 fame".hl ~ " for every territory you " ~ "conquer in combat".hl ~ " and every territory you " ~ "close by exploring".hl ~ ". The Stag rewards bold play: explore to close territories, and take land from your neighbours."),
            p("Stag Clan".hl ~ " card (a Flash card): Recruit 1, then you may move 1 of your units from an adjacent territory into the one you recruited in (even across a Rough border)."),
            p("Upgrades, for 3 " ~ lore ~ ": " ~ "Glory of the Clan".hl ~ " (Build, and collect that territory's resources as if it were the Harvest) and " ~ "Annexation".hl ~ " (Move 1 and Explore, a Flash card). With the Warchiefs module, " ~ "Brand's Bravery".hl ~ " is a third one."),
            site("tap the " ~ "i".hl ~ " beside a clan in the clan picker, or a clan's name in its player panel during a game, to see its cards."),
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
            p("You play the " ~ stag ~ " against the " ~ "Wolf Clan".styled(Wolf)(xstyles.bold) ~ ", played by the Easy bot. You go first. It is a full game of 7 years with the core rules only: no Creatures or other modules, as the rulebook recommends for your first games."),
            p("A " ~ "tip".hl ~ " at the top of the action pane talks you through each choice, as a teacher would in your first year: what it does, what to watch for (winter costs, the sudden-death win), and combat when your first fight comes."),
            p("Read the cards".hl ~ ": card text overrides the rulebook. Tap a card to see it full size (in your hand, between your turns)."),
            p(("Hide tips".hl ~ " turns the tips off; " ~ "Tutorial Lessons".hl ~ " under the action pane opens these lessons again. The practice game is saved in your browser, so you can stop and continue it later from the Tutorial menu.")),
        )),
    )

    def about : Elem =
        ("Learn to play " ~ "Northgard: Uncharted Lands".hl ~ " as the " ~ stag ~ ", and how to play it on this site. The lessons go from the goal to the map, the cards and the year, then combat and your clan; then try a practice game against a bot, with a tip for every choice.").div

    // The map as it is: which territories are closed (the map lesson's point, shown on the real map)
    def closedNote(game : Game) : Elem = {
        val closed = game.board.territories.zipWithIndex.%{ case (t, _) => game.board.closed(t) }.map(_._2 + 1)
        if (closed.none)
            "No territory is closed yet: every one still has an edge at the edge of the map.".txt
        else
            ("Closed now: territor" + (closed.num > 1).?("ies ").|("y ")).txt ~ closed.map(_.toString).mkString(", ").hl ~ ". The others are still open.".txt
    }

    // What Winter will cost f as things stand, and a warning when the food won't cover it
    def winterNote(game : Game, f : Faction) : Elem = {
        implicit val g = game
        val st = game.states(f)
        val n = st.units
        val (cf, cw) = Winter.cost(n)
        val h = Harvest.forecast(f)
        val short = st.food + h.food < cf || st.wood + h.wood < cw
        ("You have " ~ n.hl ~ " unit" ~ (n != 1).??("s") ~ " on the map: Winter will cost " ~
            ((cf + cw) == 0).?("nothing".hl).|(Amount(cf, food) ~ (cw > 0).?(" and " ~ Amount(cw, wood)).|(Empty)) ~ ". " ~
            short.?("Your food after the Harvest won't cover it: an Unrest card is coming unless you get more.".styled(xstyles.warning)).|(Empty))
    }

    // The sudden-death win: closed territories with a large building, f's and its opponents'
    def suddenDeathNote(game : Game, f : Faction) : Elem = {
        val need = game.strongholdsToWin
        val mine = game.strongholds(f).num
        val danger = game.factions.but(f).%(o => game.strongholds(o).num >= need - 1)
        danger./(o => (o.name + " Clan").styled(o)(xstyles.bold) ~ " has " ~ game.strongholds(o).num.hl ~ " of the " ~ need.hl ~ " closed territories with a large building it needs to win at the end of a year. Stop it!".styled(xstyles.warning) ~ " ").merge ~
            (mine > 0).?("You have " ~ mine.hl ~ " of the " ~ need.hl ~ " you need for the sudden-death win. ").|(Empty)
    }

    // The tip for the choices f is offered; None when there is nothing to say
    def tip(game : Game, f : Faction, actions : $[UserAction]) : |[Elem] = {
        val l = actions./(_.unwrap)

        def has[T : scala.reflect.ClassTag] = l.exists(_.is[T])

        val st = game.states.get(f)

        if (has[SetupTileAction])
            |(("Setup: you drew 3 map tiles. Pick one to add to the map. Look at what it offers: " ~ food ~ " food and " ~ wood ~ " wood, building spaces, and borders that will be easy to close. Tap a tile to pick it.").div ~
                ("Look at the map: the dotted lines are borders, and each numbered area between them is a territory. " ~ closedNote(game)).div)
        else
        if (has[SetupSpotAction] || has[ExploreSpotAction] || has[SecondChanceSpotAction])
            |("Tap one of the marked spots on the map (or in the list) to place the tile there. Its borders must continue the ones they touch, so only some spots and turns are allowed.")
        else
        if (has[SetupTurnAction] || has[ExploreTurnAction] || has[SecondChanceTurnAction])
            |("The tile is shown in place. Turn it with the blue arrows above it, then tap the green check mark below it to place it (or the red cross to pick another spot).")
        else
        if (has[SetupAreaAction] || has[SetupUnitsAction])
            |(("Place your 3 starting units: tap a territory of your new tile. One unit is enough to control a territory. A territory with " ~ food ~ " food or " ~ wood ~ " wood icons gives them at every Harvest, and one that is nearly surrounded is easy to close for fame.").div ~ closedNote(game).div)
        else
        if (has[TurnModeAction]) {
            val n = st./(_.hand.num).|(0)
            val lr = st./(_.lore).|(0)
            |(("Your turn, year " ~ game.year.hl ~ ". You have " ~ n.hl ~ " card" ~ (n != 1).??("s") ~ " in hand. ").div ~
                (n == 0).?("Your hand is empty, so Pass is your only choice.".hl.div).|(Empty) ~
                (n > 0).?(("You do " ~ "one".hl ~ " thing per turn, not your whole hand: tap " ~ "Play cards".hl ~ " to use a card, " ~ "Wait".hl ~ " to set one aside, or " ~ "Pass".hl ~ " to stop for the year. Pass early to pick the best Development card and go first next year; stay in for more actions.").div).|(Empty) ~
                (game.year <= 1).?(("The three " ~ lore ~ " lore choices are for later years; ignore them for now.").div).|((lr >= 3).?(("You have " ~ lr.hl ~ " " ~ lore ~ " lore: " ~ "Upgrade".hl ~ " would get you a Stag clan upgrade.").div).|(Empty)) ~
                winterNote(game, f).div ~
                suddenDeathNote(game, f).div)
        }
        else
        if (has[PlayCardAction] && has[EndTurnAction])
            |("You may play Flash cards (the ones with a lightning bolt) now, or tap " ~ "End turn".hl ~ ".")
        else
        if (has[PlayCardAction])
            |("Tap a card in your hand to play it. A dimmed card can't be played now; it says why. Read the card: its text overrides the rules. (Between your turns, tap a card to see it full size.)")
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
            |(("Recruit: tap one of your territories to place a new unit there. More units hold more land and win fights, but every unit must eat in Winter, and new players often recruit more than they can feed.".txt).div ~ winterNote(game, f).div)
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
            |(("Explore: here is the tile you drew. It must go next to one of your open territories. Closing a territory of yours gives fame for each of its tiles, and the Stag gets 1 more.".txt).div ~ closedNote(game).div)
        else
        if (has[BuildSpotAction])
            |("Build: tap a free building space in one of your territories. Small spaces take small buildings (1 " ~ wood ~ "), large spaces large ones (3 " ~ wood ~ ").")
        else
        if (has[BuildPickAction])
            |("Pick a building: dimmed ones can't go there and say why. A Food Silo or Woodcutter Lodge pays every Harvest; a large building in a closed territory counts toward the sudden-death win (three of them).")
        else
        if (has[BuildConfirmAction])
            |("The building is shown on its space: tap the green check mark above it to build it, or the red cross to choose again.")
        else
        if (has[FightAction])
            |("Several fights: tap the territory to fight in first.")
        else
        if (has[CombatFoodAction])
            |(("A fight! Here's how it works:".hl).div ~
                ("1. Each side counts " ~ CombatIcon.axe ~ " combat points: 1 per unit, plus the Move card's bonus for the attacker, or the buildings for the defender. The totals so far are shown below.").div ~
                ("2. Now each side may spend " ~ food ~ " food: +1 point each, at most one per unit. Food spent now can't feed your units in Winter.").div ~
                ("3. Then each side rolls one die, for more points " ~ CombatIcon.axe ~ " and casualties " ~ CombatIcon.skull ~ " (each removes an enemy unit).").div ~
                ("Remember: " ~ "ties go to the defender".hl ~ ", and a side that " ~ "loses all its units".hl ~ " loses, whatever the score.").div)
        else
        if (has[CombatChooseAction])
            |("Your die shows a choice: 1 combat point " ~ CombatIcon.axe ~ " or 1 casualty " ~ CombatIcon.skull ~ " (one enemy unit removed). Pick what helps more.")
        else
        if (has[CasualtyPickAction])
            |("You take casualties " ~ CombatIcon.skull ~ " in this fight: pick who is lost, your units or a special figure (it goes back to your reserve). The fight's result doesn't change.")
        else
        if (has[CombatReportDoneAction])
            |("The combat report: who won, each side's points and losses (ties go to the defender; losing every unit loses). Tap " ~ "OK".hl ~ " to go on. The Stag gets +1 fame for each territory it conquers. (Interface > Combat Report can skip these reports.)")
        else
        if (has[ClanPowerReportDoneAction])
            |("Your clan power went off by itself: this box says where and what it gave you (already added to your stockpile). Tap the button to go on. (Interface > Clan Power Report can skip these reports.)")
        else
        if (has[TakeUnrestAction])
            |("Winter: you couldn't feed your units (or heat their homes), so you take an " ~ "Unrest".hl ~ " card. Tap it to add it to your draw pile. It is a dead card in your hand and costs " ~ "5 fame".hl ~ " at the end.")
        else
        if (has[RetreatPickAction])
            |("You lost this fight: tap where your units retreat to (your territory or a neutral one), then who goes.")
        else
        if (has[BrandRetreatPickAction])
            |("Brand's Bravery".hl ~ ": you choose where the beaten enemy retreats. Pick the place that helps you most.")
        else
        if (has[TradeForAction] || has[TradeDoneAction])
            |(("Harvest done: you scored fame for your closed territories and collected your resources. Now you may trade any 3 resources for 1 of your choice, as often as you like. Tap " ~ "Done trading".hl ~ " when finished.").div ~ winterNote(game, f).div)
        else
        if (has[PickCardAction])
            |("You passed: pick a Development card. It goes on top of your draw pile, so you draw it at the start of next year. Read it first: tap a card to see it full size.")
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
