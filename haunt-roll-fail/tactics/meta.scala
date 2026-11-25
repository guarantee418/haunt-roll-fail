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
import hrf.elem._

object Meta extends MetaGame { mmm =>
    val gaming = tactics.gaming

    type F = Faction

    def tagF = implicitly

    val name = "tactics"
    val label = "Company of Heroes"

    val factions = Faction.all

    val minPlayers = 2

    val options = $[O]() ++ RulesChoice.all ++ CommandersChoice.all ++ $(ExpansionCommanders) ++ $(LostBrigades) ++ MapChoice.all ++ Length.all ++ hiddenOptions

    // hidden rules fixes every new game gets, so games made before keep their rules
    override def hiddenOptions = $[O](TeamRetreatPoints)

    override def mandatoryFor(n : Int, l : $[F]) = $[O](TeamRetreatPoints)

    // new games play the Full Game Rules (2026-11-24); a game with neither choice (made before) plays the First Game Rules
    override def defaultsFor(n : Int, l : $[F]) = $(FullGameRules, DrawCommanders, MoroRiverMap, ShortGame)

    // a setup saved in the browser before the rules choice gets the Full Game Rules
    override def completeSaved(n : Int, l : $[F], options : $[O]) : $[O] = {
        val o = options.of[RulesChoice].any.?(options).|(options :+ FullGameRules)
        val p = o.of[CommandersChoice].any.?(o).|(o :+ DrawCommanders)
        p.of[MapChoice].any.?(p).|(p :+ MoroRiverMap)
    }

    val quickMin = 2
    val quickMax = 2

    override val about = $[Elem](
        "Company of Heroes: The Board Game".hlb ~ ", 2nd Edition, by Bad Crow Games, licensed from SEGA and Relic Entertainment: a work in progress played here by a team helping to test it.",
        "The rules are the core rulebook's Full Game Rules with its thirty core Commanders, the five factions' 44 expansion Commanders and their Lost Brigade Building Extensions as setup options (or its First Game Rules, a setup choice), the five factions' Building Boards (US Forces, Wehrmacht, British Forces, Soviet Forces, Oberkommando West) and the Mission Booklet's ten core missions (six on 2 panels, two skirmishes on 1, two team maps on 3), two players or teams of up to five; the art is from the Tabletop Simulator mod \"Company of Heroes - 2nd Edition\".",
        "© SEGA. Developed by Relic Entertainment Inc. SEGA, the SEGA logo, Relic Entertainment and Company of Heroes are either registered trademarks or trademarks of SEGA Corporation or its affiliates.",
    )

    // How to Play on the menu (2026-11-24): the publisher's pages, and how the site is used, never the rules themselves
    override def howToPlayLinks = $[(Elem, String)](
        ("Rulebooks and manuals".hh ~ ", from Bad Crow Games (Rulebook, Faction Guide, Mission Booklet)") -> "https://badcrowgames.com/rulebooks-and-manuals/",
        ("Tutorial videos".hh ~ ", from Bad Crow Games") -> "https://badcrowgames.com/tutorial-videos/",
    )

    override def howToPlay = $[(Elem, $[Elem])](
        ("The rules".txt, $[Elem](
            "This site doesn't teach the rules: they are Bad Crow Games', in the Rulebook linked above (the First Game Rules first, then the Full Game Rules). The site keeps to them for you, offering only the moves the rules allow.",
            "Played here: any two of the five factions against each other, or team games of three to five players, Allies (US, British, Soviet) against Axis (Wehrmacht, OKW), on the ten core missions (two of them 1-panel skirmishes for two players, two 3-panel maps for teams). The setup's " ~ "Rules".hh ~ " choice picks the Full Game Rules or the First Game Rules, " ~ "Commanders".hh ~ " whether they are drawn, " ~ "Map".hh ~ " the mission.",
            "In a team game teammates share their VPs (the game ends at 3 more), their sight and their Control Points; each starts with 2 Infantry, the missions being 2-panel ones. Teams alternate turns and purchases; teammates, who play at the same time in the rulebook, take theirs one after another.",
        )),
        ("Starting a game".txt, $[Elem](
            "Quick Game".hh ~ " starts at once against an Easy bot.",
            "Local Game".hh ~ " lets you make each side a person or a bot; people take turns on this device.",
            "Play Online".hh ~ " makes a game with a link for each player, so each plays from their own device, whenever they like.",
            "Local games are kept in this browser, so you can leave and pick them up again from the same button.",
        )),
        ("Making your moves".txt, $[Elem](
            "When it's your turn, your choices are listed in the action pane on the right, and the hexes and units they concern are outlined on the map.",
            "Tap a choice in the list, or tap an outlined unit or hex on the map. If it has several choices, they open over the map: pick one, or Cancel.",
            "With many hexes to choose from, the list folds into \"Tap a highlighted hex on the map\"; \"Show the choices as a list\" unfolds it.",
            "Zoom the map with the mouse wheel or by pinching, and drag it to look around.",
        )),
        ("Seeing what's going on".txt, $[Elem](
            "Tap any unit on the map that has nothing to do to see its card: health, weapons and upgrades.",
            "The player panels show each side's points, resources and tier; the panel under them shows the round, the phase and the last dice.",
            "Building Boards".hh ~ " (in that panel, and under the action pane) shows both sides' boards, and " ~ "Icons".hh ~ " explains the status icons and the map's cover lines.",
        )),
        ("Undo, replays and help".txt, $[Elem](
            "Tap a line in the log to see the game as it was then: from there you can replay the game from that point, or undo back to it. In an online game, undo only what the other player agrees to.",
            "Interface".hh ~ " under the action pane has the display settings, " ~ "Report a Bug".hh ~ " sends a report to the site, and " ~ "Main Menu".hh ~ " goes back to this menu.",
        )),
    )

    def randomGameName() = {
        val n = $("River", "Ridge", "Crossing", "Hedgerow", "Bridge", "Farm", "Mill", "Ford", "Orchard", "Chapel").shuffle
        val c = $("at", "by", "over", "past", "beyond", "under").shuffle
        "The " + n.head + " " + c.head + " the " + n.last
    }

    // two players, any two factions; three to five play Allies (US, British, Soviet) against Axis (Wehrmacht, OKW), or three a
    // free-for-all (2026-11-24)
    def validateFactionCombination(factions : $[Faction]) =
        if (factions.num < 2)
            ErrorResult("Select two players or more")
        else
            InfoResult("Company of Heroes")

    // the free-for-all mission takes three players; three or more on another map play Allies against Axis
    def validateFactionSeatingOptions(factions : $[Faction], options : $[O]) = {
        val ffa = options.has(MoroRiverFFAMap)
        val players = options.of[MapChoice].single./~(_.map.players)
        if (players.exists(_ != factions.num))
            ErrorResult(options.of[MapChoice].single.get.map.name + " takes " + players.get + " players")
        else
        if (ffa && factions.num != 3)
            ErrorResult("The 3 Way Free-for-all takes three players")
        else
        if (ffa)
            InfoResult("Free-for-all: " + factions./(_.short).mkString(", "))
        else
        if (factions.num > 2 && (factions.exists(_.axis).not || factions.exists(_.axis.not).not))
            ErrorResult("Team games: Allies (US, British, Soviet) against Axis (Wehrmacht, OKW), or three players on the 3 Way Free-for-all")
        else
        if (factions.num > 2)
            InfoResult("Team game: " + factions.%(_.axis.not)./(_.short).mkString(" + ") + " against " + factions.%(_.axis)./(_.short).mkString(" + "))
        else
            InfoResult("Company of Heroes")
    }

    def factionName(f : Faction) = f.name
    def factionElem(f : Faction) = f.name.styled(f)

    // (its own plain face, not the storybook one)
    override def bodyFont = |("tactics-ui")

    // the reworked setup screens, with the factions' emblems
    override def setupDesign = true
    override def menuImages = Faction.all./(f => ("menu-" + f.id) -> ("/hrf/webp2/tactics/images/emblem/" + f.id + ".webp")).toMap
    override def factionTile(f : Faction) = |(Div(Image("menu-" + f.id, ExternalStyle("setup-emblem"), "")) ~ Div(f.name.hlb))
    override def factionRowIcon(f : Faction) : Elem = Image("menu-" + f.id, ExternalStyle("setup-row-emblem"), "")
    override def factionChosenElem(f : Faction) = factionRowIcon(f) ~ factionElem(f).spn(xstyles.bold)

    override def glyph(f : F) : |[String] = |("emblem:" + f.id)

    def createGame(factions : $[Faction], options : $[O]) = new Game(factions, options)

    def getBots(f : Faction) = $("Easy")

    def getBot(f : Faction, b : String) = new BotXX(f)

    def defaultBot(f : Faction) = "Easy"

    def writeFaction(f : Faction) = f.short
    def parseFaction(s : String) : |[Faction] = factions.%(_.short == s).single

    def writeOption(o : O) = o.toString
    def parseOption(s : String) = options.%(o => o.toString == s)

    def parseAction(s : String) : Action = Serialize.parseAction(s)
    def writeAction(a : Action) : String = Serialize.write(a)

    val start = StartAction(gaming.version)

    val assets =
    ConditionalAssetsList((factions : $[F], options : $[O]) => true, "", "", "webp")(
        ImageAsset("hex-select") ::
        ImageAsset("hex-target") ::
    $) ::
    ConditionalAssetsList((factions : $[F], options : $[O]) => true, "", "", "webp")(
        Faction.all./(f => ImageAsset("hex-spawn-" + f.id))
    ) ::
    // each map's picture, loaded only for its own games
    (("map" +: MapChoice.all./(_.map.image)).distinct./(m =>
        ConditionalAssetsList((factions : $[F], options : $[O]) => (options.of[MapChoice].single./(_.map.image).|("map") == m), "", "", "webp")(
            ImageAsset(m) ::
        $))) ++ (
    ConditionalAssetsList((factions : $[F], options : $[O]) => true, "token", "token:", "webp")(
        UnitType.all./(t => ImageAsset(t.id + "-" + t.faction.id)) ++ UnitType.shared./{ case (t, f) => ImageAsset(t.id + "-" + f.id) }
    ) ::
    ConditionalAssetsList((factions : $[F], options : $[O]) => true, "emblem", "emblem:", "webp")(
        Faction.all./(f => ImageAsset(f.id))
    ) ::
    ConditionalAssetsList((factions : $[F], options : $[O]) => true, "res", "res:", "webp")(
        $(Manpower, Munitions, Fuel, VP, XP)./(r => ImageAsset(r.id))
    ) ::
    ConditionalAssetsList((factions : $[F], options : $[O]) => true, "dmg", "dmg:", "webp")(
        $(AI, AP, HE, Flame)./(d => ImageAsset(d.id))
    ) ::
    ConditionalAssetsList((factions : $[F], options : $[O]) => true, "face", "face:", "webp")(
        Face.all./(f => ImageAsset(f.toString.toLowerCase))
    ) ::
    ConditionalAssetsList((factions : $[F], options : $[O]) => true, "status", "status:", "webp")(
        ($("pinned", "setup", "nofire", "slow", "retreat", "splash") ++ Faction.all./("flag-" + _.id))./(ImageAsset(_))
    ) ::
    ConditionalAssetsList((factions : $[F], options : $[O]) => true, "ops", "ops:", "webp")(
        (0.until(Operations.designs.num).$./("ops-" + _) :+ "ops-back")./(ImageAsset(_))
    ) ::
    ConditionalAssetsList((factions : $[F], options : $[O]) => true, "commander", "commander:", "webp", lzy = Laziness.OnDemand)(
        Commander.all./(c => ImageAsset(c.id))
    ) ::
    ConditionalAssetsList((factions : $[F], options : $[O]) => true, "brigade", "brigade:", "webp", lzy = Laziness.OnDemand)(
        Brigade.all./(b => ImageAsset(b.id))
    ) ::
    ConditionalAssetsList((factions : $[F], options : $[O]) => true, "board", "board:", "webp", lzy = Laziness.OnDemand)(
        Faction.all./~(f => 1.to(3)./(t => ImageAsset(Tiers.board(f, t))))
    ) ::
    $)
}
