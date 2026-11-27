package tactics
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

import tactics.elem._



// The main menu's Tutorial (asked for on 2026-11-25, "some new players need a tutorial"): lessons on the First Game Rules and on
// this site, then a practice game as the US Forces against the Wehrmacht's Easy bot on Moro River, with a tip at the top of the
// action pane for every choice (Tutorial.tip, shown by preinfo in ui.scala); the last lesson points to the Full Game Rules
case object TutorialOption extends GameOption with ToggleOption {
    val group = "Tutorial".txt
    def valueOn = "Tutorial tips".txt
}

// A lesson opened from the in-game "Tutorial Lessons" button (UI.onClick)
case class TutorialLessonView(n : Int)

object Tutorial {
    // The side the tutorial teaches with, and the practice game's opponent
    val side : Faction = US
    val opponent : Faction = Wehrmacht

    // The practice game: the First Game Rules on Moro River, a Short game (18 VP), against the Easy bot
    def seating : $[Faction] = $(side, opponent)
    def bots : Map[Faction, String] = Map(opponent -> "Easy")
    def options : $[GameOption] = $(FirstGameRules, NoCommanders, MoroRiverMap, ShortGame, TutorialOption) ++ Meta.mandatoryFor(2, seating)

    // The second practice game (2026-11-25, asked for): the same sides and map with the Full Game Rules and the Commanders
    def fullOptions : $[GameOption] = $(FullGameRules, DrawCommanders, MoroRiverMap, ShortGame, TutorialOption) ++ Meta.mandatoryFor(2, seating)

    // The two parts of the lessons, each ending with its practice game (Meta.tutorialGames, tutorialLessonGroup)
    val firstPart = "First Game Rules"
    val fullPart = "Full Game Rules"

    def games : $[(String, Elem, String, $[Faction], Map[Faction, String], $[Meta.O])] = $(
        (firstPart, "Practice Game: First Game Rules".hlb, "tutorial", seating, bots, options),
        (fullPart, "Practice Game: Full Game Rules".hlb, "tutorial-full", seating, bots, fullOptions),
    )

    // where the Full Game Rules lessons start
    def fullFrom = lessons.indexWhere(_._1 == fullTitle)

    def groupOf(i : Int) = (i < fullFrom).?(firstPart).|(fullPart)

    // the lesson the in-game "Tutorial Lessons" button opens: the first of the game's part
    def firstLesson(game : Game) = game.full.?(fullFrom).|(0)

    val fullTitle = "Full Game Rules: the round".txt

    val us = "US Forces".styled(US)(xstyles.bold)
    val wm = "Wehrmacht".styled(Wehrmacht)(xstyles.bold)

    def mp = Manpower.icon
    def mu = Munitions.icon
    def fu = Fuel.icon
    def vp = VP.icon
    def xp = XP.icon

    def p(e : Elem) : Elem = e.div(styles.tutorialText)
    def site(e : Elem) : Elem = ("On this site: ".hl ~ e).div(styles.tutorialText)(styles.tutorialSite)

    // Each lesson: its title and its paragraphs. The goal first, then the map, the round and its three phases in order, then the
    // site, and the Full Game Rules last, for after the first game
    def lessons : $[(Elem, $[Elem])] = $(
        ("How you win".txt, $(
            p("Company of Heroes".hl ~ " is a tactical board game of the Second World War for two players (or teams). Each side has a company of infantry, weapon teams and vehicles, and a Building Board of what it can buy."),
            p("You win on " ~ "Victory Points".hl ~ " (VP). In a Short game, the first side to reach " ~ "18 VP".hl ~ " at the end of a round ends the game. You score VP two ways:"),
            p("1. Control Points".hl ~ ": hold the Control Points marked with VP, and each gives you VP at the end of every round. This is where most points come from."),
            p("2. Kills".hl ~ ": every enemy unit you destroy gives you 1 VP."),
            p("When the game ends, the side holding the most VP Control Points gets 3 more VP, and the most VP wins."),
            p("In this tutorial you command the " ~ us ~ " against the " ~ wm ~ ", with the rulebook's " ~ "First Game Rules".hl ~ ": the core of the game, without the extras the full rules add later."),
            site("Company of Heroes runs in your browser, on a computer, tablet or phone, with nothing to install and no account needed."),
        )),
        ("The map and Control Points".txt, $(
            p("The map is made of " ~ "hexes".hl ~ ", and each holds at most one unit. Your units come on at your " ~ "Spawn Area".hl ~ ", the back row on your side; the enemy's is the other edge."),
            p("Control Points".hl ~ " are the round tokens on the map. Each gives you resources: " ~ mp ~ " Manpower, " ~ mu ~ " Munitions, " ~ fu ~ " Fuel, and some " ~ vp ~ " VP. They are the reason to fight over the middle of the map."),
            p("Capturing".hl ~ ": at the end of a round, an Infantry unit standing on a neutral Control Point captures it, and it stays yours until the enemy takes it away. An enemy standing on your Control Point first " ~ "neutralizes".hl ~ " it (it becomes neutral), and can capture it the round after."),
            p("Cover".hl ~ ": lines on the map protect Infantry. A yellow line is Light Cover (1 Defense Die), a green one Heavy Cover (2), when the attack crosses it. A Building gives 3. Red lines and water can't be crossed, and Buildings block sight."),
            site("each side's Control Points carry its flag. Tap " ~ "Icons".hh ~ " under the action pane to see what the lines and icons mean."),
        )),
        ("A round".txt, $(
            p("The game is played in " ~ "rounds".hl ~ ", each in three phases:"),
            p("1. Maneuver".hl ~ ": the sides take turns moving their units."),
            p("2. Damage".hl ~ ": every unit fires at once, and the dice decide what is hit."),
            p("3. Supply".hl ~ ": capture Control Points, collect resources, and buy new units and upgrades."),
            p("The side that goes first changes every round. Before the first round, each side places its " ~ "three starting Infantry".hl ~ " in its Spawn Area: the side going second places first."),
            site("the panel under the player panels shows the round, the phase and the last dice rolled."),
        )),
        ("Maneuver: Command Points".txt, $(
            p("In the Maneuver phase each side has " ~ "four turns".hl ~ ", alternating. Each turn gives you " ~ "3 Command Points".hl ~ " (CP), to spend on any of your units. CP left at the end of a turn are lost."),
            p("One CP".hl ~ " moves a unit one hex, or sets up or packs up a weapon team. A unit can take at most 3 CP a round (2 if it is " ~ "Slow".hl ~ "). Some units have a Speed Die: one free CP each round."),
            p("Weapon Teams".hl ~ " (the Mortar Team, machine guns, anti-tank guns) move only " ~ "packed up".hl ~ " and fire only " ~ "set up".hl ~ ": plan a CP to set them up where they should fire."),
            p("Move to win the Damage phase: get in range of enemies, behind cover, and onto Control Points. Spread your CP: three units moved one hex each often do more than one unit moved three."),
        )),
        ("Damage: firing".txt, $(
            p("In the Damage phase every unit that can fire picks " ~ "one target".hl ~ " it can see, in its range, and puts all its damage on it."),
            p("Machine guns first".hl ~ ": set-up machine guns fire before everyone else and " ~ "pin".hl ~ " the Infantry they hit. A pinned unit can't fire, and can't take or neutralize Control Points this round."),
            p("Damage dice".hl ~ ": each weapon shows its dice. " ~ "AI".hl ~ " (anti-infantry) is good against Infantry, " ~ "AP".hl ~ " (armor piercing) against vehicles. " ~ "HE".hl ~ " (high explosive) is rolled: a red face misses, black hits, green hits twice."),
            p("Defense Dice".hl ~ ": the target then rolls one Defense Die for each die its type resists, plus cover. Every black or green face saves one damage. Damage that gets through takes health off; a unit with none left is destroyed, giving the other side 1 " ~ vp ~ " VP."),
            p("Experience".hl ~ ": you gain 1 " ~ xp ~ " XP each round, 1 for each enemy unit you damage, and 1 for each of your own units lost. XP buys upgrades."),
        )),
        ("Supply: buying".txt, $(
            p("At the end of the round, Infantry on neutral Control Points capture them, and you collect your " ~ "income".hl ~ ": 1 " ~ mp ~ " Manpower, 1 " ~ mu ~ " Munitions and 1 " ~ fu ~ " Fuel, plus what your Control Points give, and their " ~ vp ~ " VP."),
            p("Then the sides take turns buying, one purchase at a time, until both pass:"),
            p("• Spawn".hl ~ " a unit from your Building Board into an empty hex of your Spawn Area. Infantry costs mostly " ~ mp ~ " Manpower, vehicles " ~ fu ~ " Fuel."),
            p("• Upgrade".hl ~ " a unit, with " ~ xp ~ " XP or " ~ mu ~ " Munitions: more health, more dice, more defense."),
            p("• Unlock".hl ~ " the next " ~ "Tier".hl ~ " of your Building Board (Tier 2, then Tier 3), for stronger units like the Sherman tank."),
            p("At the end of the Supply phase, Infantry in your Spawn Area heal."),
            site("tap " ~ "Building Boards".hh ~ " under the action pane to see every unit you can buy, its cost and its dice."),
        )),
        ("Playing on this site".txt, $(
            p("Your choices are listed in the " ~ "action pane".hl ~ " on the right, and the units and hexes they concern are outlined on the map. Tap a choice in the list, or tap an outlined unit or hex on the map."),
            p("In the Maneuver phase, tap one of your units to see what it can do: the hexes it can move to light up. Tap one to move there, or " ~ "End turn".hl ~ " when you are done."),
            p("Tap any unit that has nothing to do to see its card: its health, dice, range and upgrades."),
            p("Zoom the map with the mouse wheel or by pinching, and drag it to look around. Tap a line in the " ~ "log".hl ~ " to see the game as it was then, and undo back to it if you like."),
            p("In the practice game a " ~ "Tutorial tip".hl ~ " above the choices explains each one. " ~ "Hide tips".hl ~ " turns them off; " ~ "Tutorial Lessons".hl ~ " under the action pane opens these lessons again. The practice game is saved in your browser, so you can stop and continue it later from the Tutorial menu."),
        )),
        ("After your first game".txt, $(
            p("Play the First Game Rules practice game until they feel easy. Then the next lessons teach the " ~ "Full Game Rules".hl ~ ", the default for every new game on this site. They add:"),
            p("• the " ~ "Operations deck".hl ~ ": 2 to 5 turns a round instead of 4;"),
            p("• " ~ "Retreats".hl ~ " to save a battered Infantry unit, and " ~ "Assaults".hl ~ " into an enemy's hex;"),
            p("• " ~ "facing".hl ~ " and " ~ "firing arcs".hl ~ ", Frontal Armor, and machine gun " ~ "Overwatch".hl ~ ";"),
            p("• " ~ "HE Splash".hl ~ ", destructible Buildings, barbed wire and tank traps;"),
            p("• " ~ "Commanders".hl ~ ": each side picks one of three, and unlocks its abilities with XP."),
            p("Each part ends with its own practice game, saved apart in your browser."),
        )),
        (fullTitle, $(
            p("The Full Game Rules keep everything you have learned and add to it. The first change is the length of the Maneuver phase."),
            p("Operations".hl ~ ": at the start of each round, the side going first draws an " ~ "Operations card".hl ~ " and picks its top (2 to 4 turns) or its bottom (3 to 5). Both sides then get that many turns of 3 CP."),
            p("The other side only learns whether the top or the bottom was picked, not the number, until the turns are done: plan for the shortest case, and don't count on a last turn that may not come."),
            site("the panel under the player panels shows the Operation picked (\"top (2-4)\" or \"bottom (3-5)\") and, once revealed, the card."),
        )),
        ("Retreats".txt, $(
            p("On your turn, any of your Infantry (not in a transport, not assaulting) may " ~ "Retreat".hl ~ ", free, whatever CP it already spent: it moves up to six hexes toward a " ~ "Retreat Point".hl ~ " you pick, a hex of your Spawn Area (or next to your Major, once you have one)."),
            p("A retreating unit spends no CP and can't attack, spot, capture or neutralize; it keeps its Cover. After the Maneuver turns, each side may start more Retreats and moves its retreating units on (" ~ "final Retreats".hl ~ ")."),
            p("The status ends in the Supply phase for units on a Retreat Point, where Infantry in the Spawn Area heal as usual."),
            p("Use it to save a squad that would die: a destroyed unit gives the enemy 1 VP, a retreated one comes back healed."),
        )),
        ("Assaults".txt, $(
            p("A unit may move (1 CP) into an adjacent hex held by an enemy: an " ~ "Assault".hl ~ ". Both units stand in the hex, and the assaulting unit spends no more CP that round."),
            p("In the Damage phase the two may fire at each other at range 1, with " ~ "no Cover".hl ~ " and no Frontal Armor. Other units' attacks on the assaulted unit use Cover as usual."),
            p("If the assaulted unit is destroyed and the attacker survives, the attacker holds the hex. If both survive, the attacker falls back to the nearest empty hex toward a Retreat Point."),
            p("Assaults dig an enemy out of a Building or off a Control Point; they are risky against a unit with more dice."),
        )),
        ("Facing, arcs and Overwatch".txt, $(
            p("Weapon Teams".hl ~ " pick a side to face when they set up, and fire only into the 120° " ~ "arc".hl ~ " in front of it. A fixed gun (a straight arrow on the board) does the same."),
            p("Heavy Vehicles".hl ~ " always face a side: each CP moves one hex forward or back (turning one side before or after), or turns one side in place. Their front is " ~ "Frontal Armor".hl ~ ": AP from a unit in front of it rolls 2 more Defense Dice."),
            p("Overwatch".hl ~ ": a set-up machine gun deals 1 damage at once, with no save, to enemy Infantry moving onto a hex in its range, sight and arc, every hex counting. The Wehrmacht's " ~ "MG42 Team".hl ~ " starts with it: don't walk your Riflemen across its arc."),
            site("a unit's facing is the ▲ on its token; " ~ "Icons".hh ~ " under the action pane explains the marks."),
        )),
        ("Splash, Buildings and defenses".txt, $(
            p("HE Splash".hl ~ ": some HE (the US Mortar's XP upgrade, the Major's) targets a hex instead of a unit and hits every unit in it and the six hexes around it that you see, " ~ "your own included".hl ~ "."),
            p("Buildings".hl ~ " have 2 health: HE and Flame on Infantry inside also damage the Building, and units may fire HE and Flame at an empty one. A destroyed Building stays a Destroyed Building (1 Defense Die instead of 3), and the units in it are pushed out."),
            p("Defenses".hl ~ ": a unit with a Wrench builds, for 1 CP on its own hex, Sandbags, " ~ "Barbed Wire".hl ~ " (no Infantry may enter) or a " ~ "Tank Trap".hl ~ " (no Vehicle may enter). Two damage on the hex destroys them. Moro River's printed tank traps can't be destroyed."),
        )),
        ("Commanders".txt, $(
            p("At the start each side draws three of its faction's " ~ "Commander".hl ~ " cards. In the Supply phase you buy their tiers with " ~ xp ~ " XP, left to right, each a purchase; the first tier you buy picks your Commander for the game."),
            p("Tiers give abilities: new units to call in, upgrades, strikes, abilities at the start of your Maneuver phase. Some need a Tier Building first. With all four tiers bought, 2 XP trade for 1 resource."),
            p("XP now has two uses, upgrades and Commanders: early tiers are cheap and often worth more than an upgrade."),
            site("tap your player panel's Commander line to see the cards drawn, and the tiers bought."),
        )),
        ("Where to go next".txt, $(
            p("You now know the whole core game. On the setup screen you can try:"),
            p("• the other factions: " ~ "British Forces".hl ~ ", " ~ "Soviet Forces".hl ~ " and " ~ "Oberkommando West".hl ~ ", each with its own Building Boards and Commanders;"),
            p("• the " ~ "Map".hl ~ " choice: every mission of the Mission Booklet and the Large Map Pack, with team games of three to five players and free-for-alls;"),
            p("• " ~ "Expansion Commanders".hl ~ " and the " ~ "Lost Brigades".hl ~ ", for more Commanders and Building Extensions."),
            site(("How to Play".hh ~ " on the Company of Heroes menu links Bad Crow Games' rulebooks and tutorial videos.")),
        )),
    )

    def about : Elem =
        ("Learn to play " ~ "Company of Heroes".hl ~ " as the " ~ us ~ ", and how to play it on this site. The first lessons teach the rulebook's First Game Rules, from the goal to the map, the round's three phases and the site, with a practice game against a bot; the next ones add the Full Game Rules and the Commanders, with a second practice game. Both games give a tip for every choice.").div

    // Where f stands: VP, and the Control Points held
    def standing(game : Game, f : Faction) : Elem = {
        val e = game.other(f)
        val mine = game.pointsOf(f)
        val vps = mine.%(h => Board.points(h).has(VP)).num
        ("You have " ~ game.states(f).stock(VP).hl ~ " VP, the " ~ e.elem ~ " " ~ game.states(e).stock(VP).hl ~ " (" ~ game.endVP.hl ~ " ends the game). You hold " ~
            mine.num.hl ~ " Control Point" ~ (mine.num != 1).??("s") ~ (vps > 0).?((", " + vps + " of them with VP").txt).|(Empty) ~ ".")
    }

    // The tip for the choices f is offered; None when there is nothing to say
    def tip(game : Game, f : Faction, actions : $[UserAction]) : |[Elem] = {
        val l = actions./(_.unwrap)

        def has[T : scala.reflect.ClassTag] = l.exists(_.is[T])

        if (has[StartPickAction])
            |(("Setup: pick your three starting Infantry. " ~ "Riflemen".hl ~ " are the backbone: 4 health, they move, fire, and capture Control Points. The " ~ "Mortar Team".hl ~ " fires HE far away, over walls, but has only 3 health and must set up before it fires.").div ~
                "For a first game, three Riflemen are the simplest start.".txt.div)
        else
        if (has[StartPlaceAction])
            |("Tap a hex of your Spawn Area, outlined on the map, to place the unit. Put them across from the Control Points you want to take first.")
        else
        if (has[OpPickAction])
            |(("Operations: pick the top or the bottom of this card. Its number is how many Maneuver turns each side gets this round. The enemy only learns whether you took the top or the bottom.").txt.div ~
                "More turns help the side that has more to do: far from the Control Points, or with more units to move.".txt.div)
        else
        if (has[SpawnFaceAction])
            |("Your Heavy Vehicle needs a facing: pick the side its front points to. Point it at the enemy: its Frontal Armor (2 more Defense Dice against AP) only works from the front.")
        else
        if (has[FinalRetreatAction] || has[RetreatDoneAction])
            |("Final Retreats: you may start more Retreats now, and move your retreating units on toward their Retreat Point. A retreating unit you don't move goes toward the nearest one. Tap " ~ "Done retreating".hl ~ " when finished.")
        else
        if (has[UnitMenuAction] && has[EndTurnAction])
            |(("Maneuver, turn with " ~ game.cpLeft.hl ~ " CP left. Tap one of your units (on the map or in the list) to move it or set it up. Head for the Control Points, and end next to cover: a yellow or green line between you and the enemy.").div ~
                ("When you are done, tap " ~ "End turn".hl ~ ": CP left over are lost. " ~ game.full.?("The Operation picked this round sets how many turns each side has.").|("Each side has four turns a round.")).div ~
                (has[OpeningAction] || has[OpeningHexAction] || has[OpeningLineStartAction]).?(("Your Commander's start-of-Maneuver abilities are listed too: they can be used only now, before you spend any CP this round.").txt.div).|(Empty) ~
                standing(game, f).div)
        else
        if (has[MoveAction] || has[SetUpAction] || has[PackUpAction] || has[EmbarkAction] || has[DisembarkAction] || has[HeavyMoveAction] || has[SetUpFacingAction] || has[AssaultAction] || has[RetreatAction] || has[DefensesAction])
            |(("Pick what this unit does. Each hex moved costs 1 CP (a move through another unit costs both at once). " ~
                has[SetUpAction].?("Set up".hl ~ " lets a weapon team fire in the Damage phase; it can't move until it packs up again. ").|(Empty) ~
                has[SetUpFacingAction].?("Set up".hl ~ " picks the side it faces: it fires only into the arc in front of it, so point it where the enemy will come. ").|(Empty) ~
                has[PackUpAction].?("Pack up".hl ~ " lets it move, but it won't fire this round unless it sets up again. ").|(Empty) ~
                has[HeavyMoveAction].?("A Heavy Vehicle moves forward or back and turns as it goes; keep its front, with its Frontal Armor, toward the enemy. ".txt).|(Empty) ~
                has[AssaultAction].?("Assault".hl ~ " moves into an enemy's hex: the two fight at range 1 with no Cover, and the assaulting unit spends no more CP this round. ").|(Empty) ~
                has[RetreatAction].?("Retreat".hl ~ " (free) pulls the unit back toward a Retreat Point, out of the fight until the round ends: use it for a squad that would otherwise die. ").|(Empty) ~
                has[DefensesAction].?("Barbed Wire".hl ~ " keeps enemy Infantry out of this hex, a " ~ "Tank Trap".hl ~ " keeps Vehicles out (1 CP). ").|(Empty) ~
                "Cancel goes back to your units.").div)
        else
        if (has[SandbagsAction])
            |("Sandbags: pick the two sides of the hex they cover. Infantry in the hex gets their cover against attacks crossing those sides. Two or more damage dice on the hex destroy them.")
        else
        if (has[EndTurnAction])
            |("This unit can't do more now. Tap " ~ "End turn".hl ~ ".")
        else
        if (has[SplashAction] && has[TargetAction].not)
            |("HE Splash: pick the hex to hit. It hits every unit there and in the six hexes around it that you see, yours too: check none of your units is next to it.")
        else
        if (has[TargetAction] || has[HoldFireAction] || has[SplashAction] || has[ShotAction])
            |(("Damage: pick this unit's target. It puts all its dice on that one unit. AI dice are best against Infantry, AP against vehicles. Defense Dice and cover will save some damage, so finishing off a hurt unit is often worth more: a destroyed unit gives you 1 VP.").txt.div ~
                ("Hold fire".hl ~ " only if no target is worth it.").div)
        else
        if (has[CommanderTierAction] && has[SpawnMenuAction].not && has[PassAction].not)
            |("Pick the Commander tier to buy. The first tier you buy picks your Commander for the rest of the game: read the three cards first (tap your panel's Commander line).")
        else
        if (has[SpawnMenuAction] || has[UpgradesMenuAction] || has[UnlockAction] || has[PassAction]) {
            val s = game.states(f).stock
            |(("Supply: one purchase at a time, the sides taking turns. You have " ~ s(Manpower).hl ~ " " ~ mp ~ " " ~ s(Munitions).hl ~ " " ~ mu ~ " " ~ s(Fuel).hl ~ " " ~ fu ~ " and " ~ s(XP).hl ~ " " ~ xp ~ ".").div ~
                ("More Riflemen hold more Control Points; an upgrade makes a unit you have last longer; unlocking Tier 2 or 3 opens the stronger units. " ~ "Pass".hl ~ " when you are done for this round: what you don't spend is kept.").div ~
                has[CommanderTierAction].?(("Commander".hl ~ " tiers cost XP too: the first one you buy picks your Commander for the game.").div).|(Empty) ~
                standing(game, f).div)
        }
        else
        if (has[FreeSpawnAction])
            |("Your Commander tier gives you a free unit: tap the hex for it.")
        else
        if (has[DropAction] || has[BuildAction] || has[SmokeAction] || has[BraceAction] || has[OpeningHexAction] || has[OpeningLineAction] || has[DemolitionAction])
            |("A Commander ability: pick where it goes, or skip it. Its card says what it does (tap your panel's Commander line).")
        else
        if (has[SpawnAction])
            |("Tap an empty hex of your Spawn Area for the new unit.")
        else
        if (has[UpgradeAction])
            |("Pick the unit and its upgrade. Each upgrade can be bought once per unit, with XP or Munitions as shown.")
        else
            None
    }

    // While the bot plays
    def waiting(game : Game) : Elem =
        ("The " ~ wm ~ " (the Easy bot) is playing. The log says what it does; tap a line in it to look back.")
}
