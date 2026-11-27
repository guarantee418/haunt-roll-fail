package cyclades
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

import cyclades.elem._


// The main menu's Tutorial (asked for on 2026-11-27, after Pincer's and Company of Heroes'): lessons on the rules as this site plays
// them (RULES.md) and on the site, then a practice game as Blue against the Red and Green Easy bots, with a tip at the top of the action
// pane for every choice (Tutorial.tip, shown by preinfo in ui.scala)
case object TutorialOption extends GameOption with ToggleOption {
    val group = "Tutorial".txt
    def valueOn = "Tutorial tips".txt
}

// A lesson opened from the in-game "Tutorial Lessons" button (UI.onClick)
case class TutorialLessonView(n : Int)

object Tutorial {
    // The player the tutorial teaches with, and the practice game's opponents: three players, so two Metropolises win and each
    // player makes one offering a cycle
    val side : Faction = Blue
    val opponents : $[Faction] = $(Red, Green)

    def seating : $[Faction] = side +: opponents
    def bots : Map[Faction, String] = opponents./(_ -> "Easy").toMap
    def options : $[GameOption] = $(TutorialOption)

    val blue = "Blue".styled(Blue)(xstyles.bold)

    def p(e : Elem) : Elem = e.div(styles.tutorialText)
    def site(e : Elem) : Elem = ("On this site: ".hl ~ e).div(styles.tutorialText)(styles.tutorialSite)

    def head(g : God) : Elem = Image("god:head-" + g.id, styles.godHead)
    def building(b : Building) : Elem = Image("building:" + b.id, styles.buildingIcon)
    def metropolis : Elem = Image("building:metropolis", styles.buildingIcon)

    // Each lesson: its title and its paragraphs, in the order a cycle runs: the goal, the board, the cycle, offerings, the gods,
    // war, the Creatures, the Metropolises, and the site
    def lessons : $[(Elem, $[Elem])] = $(
        ("How you win".txt, $(
            p("Cyclades".hl ~ " is a game for 2 to 5 players: Greek cities on the isles of the Aegean, bidding for the gods' favor. Each god grants its own actions, and gold pays for everything."),
            p("The first player with " ~ "2 Metropolises".hl ~ " " ~ metropolis ~ " at the end of a cycle wins (" ~ "3".hl ~ " with two players). With several, the one with the most gold wins."),
            p("A Metropolis is founded either with " ~ "4 Philosophers".hl ~ " or with " ~ "one building of each kind".hl ~ ": a Port " ~ building(Port) ~ ", a Fortress " ~ building(Fortress) ~ ", a Temple " ~ building(Temple) ~ " and a University " ~ building(University) ~ ". Or you can take one by force, with the isle it stands on."),
            p("In this tutorial you play " ~ blue ~ " against the " ~ "Red".styled(Red)(xstyles.bold) ~ " and " ~ "Green".styled(Green)(xstyles.bold) ~ " Easy bots."),
            site("Cyclades runs in your browser, on a computer, tablet or phone, with nothing to install and no account needed."),
        )),
        ("The isles and the sea".txt, $(
            p("The board is " ~ "isles".hl ~ " and " ~ "sea spaces".hl ~ ". Your " ~ "Troops".hl ~ " stand on isles and hold them; your " ~ "Fleets".hl ~ " sail the sea, and carry Troops across it."),
            p("An isle is yours while your Troops are on it, and stays yours, with a territory marker, when they leave, until someone else lands there."),
            p("Gold (GP)".hl ~ " comes from " ~ "prosperity".hl ~ ": the cornucopias printed on your isles, the prosperity markers Apollo adds, and the " ~ "commerce spaces".hl ~ " (the arrows at sea) where you have a Fleet."),
            p("Each isle has a few " ~ "building spaces".hl ~ " (the white squares): your buildings go there. A Metropolis takes up some of them."),
            p("You start with 5 GP, 2 Troops on two isles and a Fleet next to each."),
        )),
        ("A cycle".txt, $(
            p("The game is played in " ~ "cycles".hl ~ ". Each cycle:"),
            p("1. ".hl ~ "Creatures".hl ~ " come onto the track: new ones arrive, the cheapest no one called leaves."),
            p("2. ".hl ~ "The gods".hl ~ " are shuffled: their order is the order they act in. With three players two are face up and two face down: the face-down pair comes first next cycle. " ~ "Apollo".hl ~ " " ~ head(Apollo) ~ " is always there, and always last."),
            p("3. ".hl ~ "Revenue".hl ~ ": everyone collects 1 GP for each prosperity they control."),
            p("4. ".hl ~ "Offerings".hl ~ ": each player wins one god (next lesson)."),
            p("5. ".hl ~ "Actions".hl ~ ": the gods act in order, each played by the player who won it."),
            p("6. ".hl ~ "End".hl ~ ": a player with 2 Metropolises wins; otherwise the next cycle starts. Next cycle's offering order is this cycle's action order, reversed."),
        )),
        ("Offerings".txt, $(
            p("In turn, each player puts their marker on a god and offers gold, " ~ "more than the god's current offering".hl ~ "."),
            p("If you are " ~ "outbid".hl ~ ", you must at once offer to another god, so a god can change hands several times. You can never offer to the god you were just pushed off."),
            p("Apollo".hl ~ " " ~ head(Apollo) ~ " costs nothing and any number of players can pick him: a cheap way to sit out a cycle and save gold."),
            p("Each " ~ "Priest".hl ~ " takes 1 GP off your offering (it always costs at least 1). The offerings are paid once everyone has a god."),
            p("A god is worth what it lets you do this cycle: look at what you need for your Metropolis, and what your opponents need."),
            site("tap a god, then the GP to offer; the list says what each god is offered already."),
        )),
        ("The gods".txt, $(
            p("Every god but Apollo lets you " ~ "recruit".hl ~ " its unit (the first free, the next ones dearer), " ~ "build".hl ~ " its building for 2 GP, and " ~ "call Creatures".hl ~ ", all as often as you can pay, in any order."),
            p(head(Poseidon) ~ " " ~ "Poseidon".hl ~ ": Fleets, the " ~ "Port".hl ~ " " ~ building(Port) ~ ", and for 1 GP a time moves the Fleets of a space up to 3 spaces."),
            p(head(Ares) ~ " " ~ "Ares".hl ~ ": Troops, the " ~ "Fortress".hl ~ " " ~ building(Fortress) ~ ", and for 1 GP a time moves Troops from an isle to another joined to it by a chain of your Fleets."),
            p(head(Zeus) ~ " " ~ "Zeus".hl ~ ": Priests (cheaper offerings), the " ~ "Temple".hl ~ " " ~ building(Temple) ~ " (cheaper Creatures), and for 1 GP swaps a Creature on offer."),
            p(head(Athena) ~ " " ~ "Athena".hl ~ ": Philosophers (four make a Metropolis at once) and the " ~ "University".hl ~ " " ~ building(University) ~ "."),
            p(head(Apollo) ~ " " ~ "Apollo".hl ~ ": 1 GP (4 if you hold a single isle), and the first player on him adds a prosperity marker to one of their isles: more revenue every cycle after."),
        )),
        ("War".txt, $(
            p("Ares".hl ~ " moves Troops onto an enemy isle: a " ~ "battle".hl ~ ". " ~ "Poseidon".hl ~ " moves Fleets into an enemy's Fleets: a sea battle."),
            p("Each round, both sides roll the battle die (0, 1, 1, 2, 2 or 3) and add their units. The defender adds 1 for each " ~ "Fortress".hl ~ " on the isle (land) or " ~ "Port".hl ~ " on the isles around (sea); a Metropolis counts as both. The lower total loses a unit; a tie, both do."),
            p("Between rounds, the defender may " ~ "retreat".hl ~ ", then the attacker. Win, and the isle is yours with everything on it: its buildings and its Metropolis."),
            p("A player's last isle can only be attacked to take a winning Metropolis."),
        )),
        ("Creatures".txt, $(
            p("The " ~ "Creatures".hl ~ " on the track can be called by the player of any god but Apollo, for 2, 3 or 4 GP by their space. Each " ~ "Temple".hl ~ " takes 1 GP off once a cycle."),
            p("A Creature acts at once and is discarded: the " ~ "Griffon".hl ~ " takes half a player's gold, the " ~ "Harpy".hl ~ " removes a Troop, the " ~ "Pegasus".hl ~ " flies Troops anywhere, the " ~ "Satyr".hl ~ " steals a Philosopher..."),
            p("Five stay on the board as figures until they are called again: the " ~ "Kraken".hl ~ " sinks Fleets, the " ~ "Minotaur".hl ~ " guards an isle, " ~ "Medusa".hl ~ " freezes Troops, " ~ "Polyphemus".hl ~ " drives Fleets off, " ~ "Chiron".hl ~ " protects an isle."),
            site("tap a Creature anywhere to read its card. " ~ "Creatures".hh ~ " under the action pane lists all seventeen."),
        )),
        ("Metropolises".txt, $(
            p("As soon as you have " ~ "4 Philosophers".hl ~ ", or a Port, a Fortress, a Temple and a University on your isles, they become a " ~ "Metropolis".hl ~ " " ~ metropolis ~ ": you pick one of your isles for it, and the Philosophers or the four buildings are discarded."),
            p("A Metropolis counts as every building: in battle, for discounts, and toward the next Metropolis. And it is taken with its isle: guard it."),
            p("Philosophers are the slow, steady road, and Athena gives one each time; buildings are faster but spread over four gods. Watch the others: when someone is one step away, it is time to bid them off the god they need, or to attack."),
        )),
        ("Playing on this site".txt, $(
            p("Your choices are listed in the " ~ "action pane".hl ~ " on the right, and the isles and sea spaces they concern are " ~ "ringed".hl ~ " on the map. Tap a choice in the list, or a ringed isle or space."),
            p("The panel under the player panels shows the cycle, the gods in their order with each one's offering, and the Creatures on offer. " ~ "The Gods".hh ~ " under the action pane shows the gods' tiles."),
            p("Zoom the map with the mouse wheel or by pinching, and drag it to look around. Tap a line in the " ~ "log".hl ~ " to see the game as it was then, and undo back to it."),
            p("In the practice game a " ~ "Tutorial tip".hl ~ " above the choices explains each one. " ~ "Hide tips".hl ~ " turns them off; " ~ "Tutorial Lessons".hl ~ " under the action pane opens these lessons again. The practice game is saved in your browser, so you can stop and continue it later from the Tutorial menu."),
        )),
    )

    def about : Elem =
        ("Learn to play " ~ "Cyclades".hl ~ " and how to play it here: the goal, the isles, the cycle, offerings, the gods, war, the Creatures and the Metropolises; then try a practice game against two bots, with a tip for every choice.").div

    // Where f stands: Metropolises, gold, revenue, the god it plays this cycle
    def standing(game : Game, f : Faction) : Elem = {
        val st = game.states(f)
        val best = game.factions.but(f)./(e => e -> game.metropolisesOf(e)).sortBy(-_._2).headOption
        ("You have " ~ game.metropolisesOf(f).hl ~ " of the " ~ game.goal.hl ~ " Metropolises you need" ~ best./{ case (e, n) => (", " ~ e.elem ~ " " ~ n.hl) : Elem }.|(Empty) ~ ". " ~
            st.gold.hl ~ " GP, revenue " ~ game.revenue(f).hl ~ ", " ~ st.priests.hl ~ " Priest" ~ (st.priests != 1).??("s") ~ ", " ~ st.philosophers.hl ~ " Philosopher" ~ (st.philosophers != 1).??("s") ~ ".")
    }

    def godTip(game : Game, f : Faction) : Elem = game.currentGod match {
        case Poseidon => "Poseidon".hl ~ ": recruit Fleets (the first free), build a " ~ "Port".hl ~ ", or move Fleets for 1 GP. Fleets on commerce spaces bring gold, and a chain of them lets Ares' Troops cross the sea."
        case Ares => "Ares".hl ~ ": recruit Troops (the first free), build a " ~ "Fortress".hl ~ ", or move Troops for 1 GP to an isle joined by your Fleets: an empty isle is taken at once, an enemy one is a battle."
        case Zeus => "Zeus".hl ~ ": a free " ~ "Priest".hl ~ " (each takes 1 GP off your offerings), build a " ~ "Temple".hl ~ " (cheaper Creatures), or for 1 GP swap a Creature on offer."
        case Athena => "Athena".hl ~ ": a free " ~ "Philosopher".hl ~ " (four make a Metropolis) and a " ~ "University".hl ~ ". Philosophers are the surest road to a Metropolis."
        case _ => "Your god's turn."
    }

    // The tip for the choices f is offered; None when there is nothing to say
    def tip(game : Game, f : Faction, actions : $[UserAction]) : |[Elem] = {
        val l = actions./(_.unwrap)

        def has[T : scala.reflect.ClassTag] = l.exists(_.is[T])

        if (has[BidMenuAction] || has[ApolloBidAction])
            |(("Offerings: pick a god for this cycle, then what you offer. To take a god already offered you must offer more, and the player you outbid moves to another god. " ~
                "Apollo".hl ~ " is free: a cheap cycle that saves gold. Each Priest takes 1 GP off what you pay.").div ~ standing(game, f).div)
        else
        if (has[BidAction])
            |(("Pick your offering. You pay it once everyone has a god, so keep enough gold for the god's actions: the first unit is free, a building costs 2 GP.").txt.div)
        else
        if (has[EndGodTurnAction])
            |((godTip(game, f) ~ " " ~ has[BuyCreatureAction].?("You may also " ~ "call a Creature".hl ~ " from the track. ").|(Empty) ~ "Done".hl ~ " ends your god's turn.").div ~ standing(game, f).div)
        else
        if (has[TroopsFromAction] || has[FleetsFromAction])
            |(("Pick where the units start from: a ringed isle or sea space.").txt.div)
        else
        if (has[FleetsTakeAction])
            |("Pick how many of the Fleets in that space move together.")
        else
        if (has[FleetsStepAction] || has[FleetsPickUpAction] || has[FleetsDropAction] || has[FleetsStopAction])
            |(("Moving Fleets: step to a ringed space, up to 3 in all. On the way you may take along or leave Fleets. Moving into another player's Fleets is a sea battle and ends the move. " ~ "Stop here".hl ~ " ends it.").div)
        else
        if (has[TroopsMoveAction])
            |(("Pick how many Troops go. An isle you leave empty stays yours, with a territory marker, until someone lands there; an enemy isle means a battle.").txt.div)
        else
        if (has[TroopsToAction])
            |(("Pick where the Troops go: a ringed isle joined to this one by your Fleets.").txt.div)
        else
        if (has[RetreatAction] || has[FightOnAction])
            |(("Battle: the lower total (die plus units, plus Fortresses or Ports for the defender) loses a unit each round. " ~ "Fight on".hl ~ ", or " ~ "retreat".hl ~ " to save your units: on land to one of your isles joined by your Fleets, at sea to a space next to it.").div)
        else
        if (has[PlaceMetropolisAction])
            |(("A Metropolis! Pick one of your isles for it. It counts as every building, but it is taken with its isle: put it where your Troops can hold it.").txt.div)
        else
        if (has[ProsperityAction])
            |(("Apollo: you were first on him, so you place a prosperity marker on one of your isles: 1 GP more every cycle while you hold it.").txt.div)
        else
        if (has[PlaceFigureAction])
            |(("Place the Creature's figure: it stays on the board until someone calls it again.").txt.div)
        else
        if (has[PushAction])
            |("Polyphemus: pick where each Fleet next to his isle is pushed; where it can't go, it is destroyed.")
        else
        if (has[KrakenStepAction] || has[KrakenDoneAction])
            |("The Kraken: for 1 GP a space it moves on, sinking the Fleets it meets; or it stays.")
        else
        if (has[SylphDoneAction] || has[SphinxDoneAction] || has[SirenAction] || has[SirenFromAction] || has[CyclopsAction] || has[ChimeraAction] ||
            has[HarpyAction] || has[GiantAction] || has[GriffonAction] || has[StealPriestAction] || has[StealPhilosopherAction] || has[SphinxSellFleetAction] || has[SphinxSellTroopAction])
            |(("Your Creature acts now: pick its target. Tap the Creature's name to read its card again.").txt.div)
        else
            None
    }

    // While the bots play
    def waiting(game : Game) : Elem =
        ("The Easy bots are playing. The log says what they do; tap a line in it to look back.").txt
}
