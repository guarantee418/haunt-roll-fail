package pincer
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

import pincer.elem._


// The main menu's Tutorial (asked for on 2026-11-27, after Company of Heroes'): lessons on the rules as this site plays them (RULES.md,
// from the second draft of the rulebook) and on the site, then a practice game as Red against the Green Easy bot, with a tip at the top of
// the action pane for every choice (Tutorial.tip, shown by preinfo in ui.scala)
case object TutorialOption extends GameOption with ToggleOption {
    val group = "Tutorial".txt
    def valueOn = "Tutorial tips".txt
}

// A lesson opened from the in-game "Tutorial Lessons" button (UI.onClick)
case class TutorialLessonView(n : Int)

object Tutorial {
    // The player the tutorial teaches with, and the practice game's opponent
    val side : Faction = Red
    val opponent : Faction = Green

    def seating : $[Faction] = $(side, opponent)
    def bots : Map[Faction, String] = Map(opponent -> "Easy")
    def options : $[GameOption] = $(TutorialOption)

    val red = "Red".styled(Red)(xstyles.bold)
    val green = "Green".styled(Green)(xstyles.bold)

    def p(e : Elem) : Elem = e.div(styles.tutorialText)
    def site(e : Elem) : Elem = ("On this site: ".hl ~ e).div(styles.tutorialText)(styles.tutorialSite)

    // Each lesson: its title and its paragraphs. The goal first, then the land and its resources, the turn, building and trade, the
    // units, war, the Maneuver cards, the seals in detail, and the site
    def lessons : $[(Elem, $[Elem])] = $(
        ("How you win".txt, $(
            p("Pincer: A Gentleman's War".hl ~ " is Julian Birke's game of colonies, trade and war for 2 to 4 players. It is a work in progress: this site plays the " ~ "second draft".hl ~ " of its rulebook, and the playtesters' readings of what the draft leaves open."),
            p("There are " ~ "14 Royal Seals".hl ~ ": four Secret Societies, six Trading Companies and four Military Badges. The first player to hold " ~ "8 Royal Seals".hl ~ " wins."),
            p("Seals are won by being first (to found colonies, build ports or forts, finish road networks), by founding a company for a resource, or by making the most captures with a kind of unit. Most of them can be " ~ "taken away".hl ~ " again, so a lead is never safe."),
            p("In this tutorial you play " ~ red ~ " against " ~ green ~ ", an Easy bot."),
            site("Pincer runs in your browser, on a computer, tablet or phone, with nothing to install and no account needed."),
        )),
        ("The land and its resources".txt, $(
            p("The board is made of " ~ "hexes".hl ~ ". Each land hex is a terrain that gives a resource: wheat fields " ~ Wheat.elem ~ ", water " ~ Fish.elem ~ ", hills " ~ Mustang.elem ~ ", forest " ~ Lumber.elem ~ ", stone " ~ Stone.elem ~ " and mountains " ~ Iron.elem ~ "."),
            p("Your " ~ "colonies".hl ~ " gather them. A colony harvests a resource when one of the six hexes around it has that terrain and no other player's unit in it."),
            p("Setup".hl ~ ": everyone starts with one card of each resource, then places a colony and a musketeer together in one hex: on land, not a mountain or an edge hex, at least 3 hexes from every other colony. The last player to place takes the first turn."),
            p("Place your first colony where many different terrains touch it: every terrain around it is a resource you can harvest."),
        )),
        ("Your turn".txt, $(
            p("Harvest".hl ~ " first: you roll the two resource dice and " ~ "pick one".hl ~ " for yourself; everyone else takes the other. Each of your colonies next to that terrain gives you one card of it."),
            p("Pick with care: the die you leave is the one your opponents harvest."),
            p("Then you may do anything, as often as you can pay for it, in any order: move and attack with your units, declare war or make peace, build, recruit, revive, trade, and found or buy out trading companies."),
            p("Tap " ~ "End turn".hl ~ " when you are done; play goes on in seating order."),
        )),
        ("Building and trading".txt, $(
            p("Colony".hl ~ ": 2 Lumber, 2 Stone, Iron, Wheat. " ~ "Road".hl ~ ": Stone. " ~ "Bridge".hl ~ ": Stone, Lumber. " ~ "Port".hl ~ ": 2 Lumber, Iron. " ~ "Fort".hl ~ ": 3 Stone, Lumber."),
            p("Roads".hl ~ " join your colonies into " ~ "road networks".hl ~ ": a land unit moves along a network from colony to colony for one movement point. A network can be cut off by enemy units standing at both its ends."),
            p("Trade".hl ~ ": give any cards for one card of the resource you want, at " ~ "5 to 1".hl ~ ", one less for each of your road networks and each of your ports (never below 1 to 1). That is the only way cards come from the bank outside the harvest."),
            p("Trading Companies".hl ~ " are seals: found one with 5 cards of its resource while you hold a colony on that terrain (a port for Fish). Then the others' trades for that resource pay " ~ "you".hl ~ ". Another player may buy it out with more matching cards than its stockpile."),
            site("the " ~ "Build".hh ~ " choice lists every building's cost above the spots you can build on. Tap a player panel for the Player Reference card."),
        )),
        ("Units".txt, $(
            p("Each unit has " ~ "one action".hl ~ " a turn: move, attack, or be revived. New and revived units wait until their owner's next turn. One unit a hex: units pass through their own player's units but never stop on them."),
            p("Musketeer".hl ~ " (2 Wheat or Fish): move 1 or attack 1, land units only."),
            p("Cavalry".hl ~ " (2 Wheat, Mustang): move up to 2, or move 1 then attack 1."),
            p("Cannon".hl ~ " (4 Iron): move 1 or attack up to 2 hexes away, land units, frigates and forts."),
            p("Frigate".hl ~ " (2 Iron, 2 Lumber): moves up to 2 water hexes or attacks up to 2 away, and carries one land unit. Land units never enter water or mountains."),
            p("Units are " ~ "recruited".hl ~ " at your colonies (frigates at your ports)."),
        )),
        ("War and combat".txt, $(
            p("Only players " ~ "at war".hl ~ " attack each other. You declare war with one of your three Declare War cards: a " ~ "Gentleman's War".hl ~ " lets you attack from your next turn (your enemy may attack at once); a " ~ "Boar's War".hl ~ " attacks at once."),
            p("An attack " ~ "injures".hl ~ " its target, which then can't attack. Moving into a hex where every enemy unit is injured " ~ "captures".hl ~ " them: they become yours. Captures count toward the Military Badges."),
            p("Counter-Action".hl ~ ": after a unit of the player whose turn it is attacks or captures, each player it hit may answer with one of their unused units: a move, or an attack on that player."),
            p("Forts".hl ~ " keep enemies out of a colony until cannons or frigates damage them. An enemy land unit standing in your colony " ~ "annexes".hl ~ " it: its harvest goes to them while they stay."),
            p("Peace".hl ~ " is offered on your turn, with terms (cards and captured units given or asked); the other player accepts or declines."),
        )),
        ("Maneuver cards".txt, $(
            p("Each resource deck hides one " ~ "Maneuver card".hl ~ ", drawn like a resource. They are played from your hand, then discarded, and need no war:"),
            p("Ambush".hl ~ " (Lumber): on another player's turn, a unit in a forest moves or attacks. " ~ "Outflank".hl ~ " (Stone) and " ~ "Broadside".hl ~ " (Fish, a frigate): a unit gets one more action."),
            p("Retreat".hl ~ " (Wheat): a unit attacked on a wheat field moves away instead of being hit. " ~ "Fortify".hl ~ " (Mustang): an attack on a unit on hills is deflected. " ~ "Hideout".hl ~ " (Iron): a unit next to a mountain enters it."),
            p("Hands are hidden, so every attack on a wheat field or hills asks the defender, whether or not they hold the card: the question gives nothing away."),
        )),
        ("The Royal Seals".txt, $(
            p("Secret Societies".hl ~ ": first to found 2 more colonies (Founder's Oath: pick either die at every harvest), to build 2 ports (Maritime Exchange), 2 forts (Stonewall), or to complete 2 road networks (Crossroads). Each is stolen by an enemy who takes away what earned it."),
            p("Trading Companies".hl ~ ": six, one a resource (lesson 4)."),
            p("Military Badges".hl ~ ": the most captures by musketeers, cavalry, cannons or frigates; a tie keeps the holder. Each makes that kind of unit stronger."),
            p("Eight seals win: a peaceful player can get there with societies and companies, a warlike one with badges and by stealing."),
            site("Royal Seals".hh ~ " under the action pane lists every seal and who holds it; tap one to read it."),
        )),
        ("Playing on this site".txt, $(
            p("Your choices are listed in the " ~ "action pane".hl ~ " on the right, and the hexes and units they concern are outlined on the map. Tap a choice in the list, or tap an outlined hex or unit."),
            p("Tap one of your units to see what it can do this turn. Tap a card or a seal to see it large; tap again to close it. " ~ "Icons".hl ~ " under the action pane says what each panel icon means."),
            p("Zoom the map with the mouse wheel or by pinching, and drag it to look around. Tap a line in the " ~ "log".hl ~ " to see the game as it was then, and undo back to it."),
            p("In the practice game a " ~ "Tutorial tip".hl ~ " above the choices explains each one. " ~ "Hide tips".hl ~ " turns them off; " ~ "Tutorial Lessons".hl ~ " under the action pane opens these lessons again. The practice game is saved in your browser, so you can stop and continue it later from the Tutorial menu."),
        )),
    )

    def about : Elem =
        ("Learn to play " ~ "Pincer: A Gentleman's War".hl ~ " as this site plays it, from the second draft of its rulebook, and how to play it here: the goal, the land, your turn, building and trade, units, war, the Maneuver cards and the Royal Seals; then try a practice game against a bot, with a tip for every choice.").div

    // Where f stands: seals, cards, wars
    def standing(game : Game, f : Faction) : Elem = {
        val st = game.states(f)
        val seals = game.sealsOf(f).num
        val best = game.factions.but(f)./(e => e -> game.sealsOf(e).num).sortBy(-_._2).headOption
        val wars = game.factions.but(f).%(e => game.isAtWar(f, e))
        ("You hold " ~ seals.hl ~ " of the 8 Royal Seals you need" ~ best./{ case (e, n) => (", " ~ e.elem ~ " " ~ n.hl) : Elem }.|(Empty) ~ ". " ~
            "You have " ~ st.hand.num.hl ~ " card" ~ (st.hand.num != 1).??("s") ~ " and trade at " ~ game.rate(f).hl ~ ":1. " ~
            wars.any.?(("At war with " ~ wars./(_.elem).join(", ") ~ ".") : Elem).|("At peace.".txt))
    }

    // The tip for the choices f is offered; None when there is nothing to say
    def tip(game : Game, f : Faction, actions : $[UserAction]) : |[Elem] = {
        val l = actions./(_.unwrap)

        def has[T : scala.reflect.ClassTag] = l.exists(_.is[T])

        if (has[PlaceStartAction])
            |(("Setup: place your starting colony and musketeer. Tap a hex outlined on the map. A colony harvests from the six hexes around it: pick one where many different terrains meet, and water nearby for a port later.").txt.div)
        else
        if (has[HarvestChooseAction] || has[FoundersChooseAction])
            |(("Harvest: pick the die you take; " ~ "everyone else takes the other".hl ~ ". Each of your colonies next to that terrain gives you a card. Look at what the other die gives your opponents too.").div)
        else
        if (has[CounterUnitAction] || has[AmbushMenuAction] || has[CounterPassAction])
            |(("Counter-Action: one of your units was hit, so you may answer with one unused unit: a move, or an attack on the player whose turn it is." ~ has[AmbushMenuAction].?(" You may also play " ~ "Ambush".hl ~ " for a unit in a forest.").|(Empty) ~ " Or " ~ "Pass".hl ~ ".").div)
        else
        if (has[RetreatAction] || has[FortifyAction] || has[TakeHitAction])
            |(("One of your units is attacked. " ~ has[RetreatAction].?("Retreat".hl ~ " (Wheat) moves it away instead. ").|(Empty) ~ has[FortifyAction].?("Fortify".hl ~ " (Mustang) deflects the attack. ").|(Empty) ~
                "Every attack on a wheat field or hills asks this, card or not, so the question tells your opponent nothing. Otherwise " ~ "take the hit".hl ~ ": the unit is injured, not lost, unless an enemy then moves in to capture it.").div)
        else
        if (has[StonewallRepairAction])
            |("Stonewall Society: you may repair your damaged fort at once, as your Counter-Action.")
        else
        if (has[FrigateReviveNowAction] || has[FrigateReviveSkipAction])
            |("Naval Supremacy: you may revive your injured frigate at once, paying for it.")
        else
        if (has[AcceptPeaceAction] || has[DeclinePeaceAction])
            |("Peace is offered, with its terms shown. Accepting ends the war and the cards and units change hands at once; declining keeps the war on.")
        else
        if (has[PlacePrisonerAction] || has[SupplyPrisonerAction])
            |("A unit of yours was returned in the peace: place it in one of your empty colonies (a port, for a frigate), or keep it in your supply.")
        else
        if (has[UnitMenuAction] && has[EndTurnAction])
            |(("Your turn. Tap a unit to move or attack with it, or a choice below: " ~ "Build".hl ~ ", " ~ "Recruit".hl ~ ", " ~ "Trade".hl ~ ", " ~ "Trading Companies".hl ~ " and " ~ "Diplomacy".hl ~ " (war and peace). " ~ "End turn".hl ~ " when you are done.").div ~
                standing(game, f).div)
        else
        if (has[EndTurnAction])
            |(("Your turn: " ~ "Build".hl ~ ", " ~ "Recruit".hl ~ ", " ~ "Trade".hl ~ " or " ~ "Diplomacy".hl ~ ", or " ~ "End turn".hl ~ ".").div ~ standing(game, f).div)
        else
        if (has[StepAction] || has[AttackAction] || has[AttackFortAction] || has[ReviveAction] || has[DoneUnitAction] || has[HideoutAction] || has[ExtraActionAction])
            |(("Pick what this unit does: each step moves one hex (outlined on the map). ".txt ~
                has[AttackAction].?("Attack".hl ~ " injures an enemy unit; then move into its hex to capture it. ").|(Empty) ~
                has[AttackFortAction].?("Only cannons and frigates can damage a fort. ".txt).|(Empty) ~
                has[ReviveAction].?("Revive".hl ~ " heals an injured unit, using its action this turn. ").|(Empty) ~
                has[HideoutAction].?("Hideout".hl ~ " takes it into the mountain. ").|(Empty) ~
                has[ExtraActionAction].?("Outflank".hl ~ " or " ~ "Broadside".hl ~ " gives it one more action. ").|(Empty) ~
                has[DoneUnitAction].?("Done".hl ~ " ends its action here.").|(Empty)).div)
        else
        if (has[BuildColonyAction] || has[BuildRoadAction] || has[BuildBridgeAction] || has[BuildPortAction] || has[BuildFortAction] || has[RepairFortAction] || has[BuildCostInfoAction])
            |(("Build: the costs are listed at the top. A new " ~ "colony".hl ~ " harvests more and counts toward the Founder's Oath (2 more colonies); " ~ "roads".hl ~ " joining two colonies make a network (cheaper trade, faster moves); a " ~ "port".hl ~ " trades cheaper and launches frigates; a " ~ "fort".hl ~ " keeps a colony safe.").div)
        else
        if (has[RecruitAction])
            |(("Recruit: pick a unit and the colony it appears in (a port, for a frigate). New units act from your next turn. Units keep enemies off your harvest hexes, and you need them for war.").txt.div)
        else
        if (has[TradeForAction])
            |(("Trade: pick the resource you want; you pay " ~ game.rate(f).hl ~ " cards for it. Each road network and each port lowers that by one. If another player owns that resource's Trading Company, your cards go to them.").div)
        else
        if (has[TradePayAction] || has[TradeAction])
            |("Pick the cards you pay with, one by one, then confirm.")
        else
        if (has[FoundCompanyAction] || has[BuyoutAction] || has[DepositAction] || has[WithdrawAction])
            |(("Trading Companies: found one with 5 cards of its resource (you need a colony on that terrain, a port for Fish): it is a Royal Seal, and the others' trades for that resource then pay you. You may put cards on its stockpile, or take its top card. A buyout takes another player's company with more matching cards than its stockpile.").txt.div)
        else
        if (has[DeclareWarAction])
            |(("Declare War: you have " ~ game.states(f).warCards.hl ~ " Declare War cards. A " ~ "Gentleman's War".hl ~ " lets you attack from your next turn, while your enemy may attack at once; a " ~ "Boar's War".hl ~ " attacks at once. War lets you capture units (Military Badges) and annex colonies, and opens you to the same.").div)
        else
        if (has[PeaceTermsAction] || has[OfferPeaceAction])
            |("Peace terms: add the cards and captured units you give or ask for, then offer them. The other player accepts or declines.")
        else
        if (has[SurrenderAction])
            |("Every one of your colonies is annexed. You may surrender, leaving the game, or play on and try to win them back.")
        else
        if (has[KeepColonyAction])
            |("You annexed a surrendering player's colonies but have too few colony pieces to replace them all: pick which to keep.")
        else
            None
    }

    // While the bot plays
    def waiting(game : Game) : Elem =
        ("The " ~ green ~ " Easy bot is playing. The log says what it does; tap a line in it to look back.")
}
