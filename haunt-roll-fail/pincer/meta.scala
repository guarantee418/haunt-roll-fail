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
import hrf.elem._

object Meta extends MetaGame { mmm =>
    val gaming = pincer.gaming

    type F = Faction

    def tagF = implicitly

    val name = "pincer"
    val label = "Pincer: A Gentleman's War"

    val factions = Faction.all

    val minPlayers = 2

    val options = $[O]()

    val quickMin = 2
    val quickMax = 4

    override val about = $[Elem](
        "Pincer: A Gentleman's War".hlb ~ " by Julian Birke (Birke Publishing), a work in progress played here from the second draft of its rulebook.",
        "The art is from the Tabletop Simulator mod \"Pincer: A Gentleman's War (Updated for First Draft)\".",
    )

    // How to Play on the menu (2026-11-24): the designer's page, and how the site is used, never the rules themselves
    override def howToPlayLinks = $[(Elem, String)](
        ("Pincer: A Gentleman's War".hh ~ " on Kickstarter, by Julian Birke") -> "https://www.kickstarter.com/projects/juilanbirke/pincer-a-gentlemans-war",
    )

    override def howToPlay = $[(Elem, $[Elem])](
        ("The rules".txt, $[Elem](
            "This site doesn't teach the rules: they are Julian Birke's, and the rulebook is still a draft. Learn them from the game's own rulebook or from someone who plays it; the site keeps to them for you, offering only the moves the rules allow.",
        )),
        ("Starting a game".txt, $[Elem](
            "Quick Game".hh ~ " starts at once against Easy bots.",
            "Local Game".hh ~ " lets you pick 2 to 4 players and make each one a person or a bot; people take turns on this device.",
            "Play Online".hh ~ " makes a game with a link for each player, so everyone plays from their own device, whenever they like.",
            "Local games are kept in this browser, so you can leave and pick them up again from the same button.",
        )),
        ("Making your moves".txt, $[Elem](
            "When it's your turn, your choices are listed in the action pane on the right, and the hexes and units they concern are outlined on the map.",
            "Tap a choice in the list, or tap an outlined hex or unit on the map. If that hex or unit has several choices, they open over the map: pick one, or Cancel.",
            "With many hexes to choose from, the list folds into \"Tap a highlighted hex on the map\"; \"Show the choices as a list\" unfolds it.",
            "Zoom the map with the mouse wheel or by pinching, and drag it to look around.",
        )),
        ("Seeing what's going on".txt, $[Elem](
            "The player panels show each player's cards, Royal Seals, wars and pieces; the panel under them shows the turn and the last dice.",
            "Under the action pane, " ~ "Royal Seals".hh ~ " lists every seal and who holds it (tap one to read it), and " ~ "Icons".hh ~ " says what each icon on the panels means.",
            "Tap a card or a seal anywhere to see it large; tap again to close it. Tap a player panel for the Player Reference: what everything costs, and how far each unit moves and attacks.",
            "Tapping " ~ "Build".hh ~ " lists what each building costs above the choices.",
        )),
        ("Undo, replays and help".txt, $[Elem](
            "Tap a line in the log to see the game as it was then: from there you can replay the game from that point, or undo back to it. In an online game, undo only what the other players agree to.",
            "Interface".hh ~ " under the action pane has the display settings, " ~ "Report a Bug".hh ~ " sends a report to the site, and " ~ "Main Menu".hh ~ " goes back to this menu.",
        )),
    )

    // a plain face for the controls, the log and the numbers; Luminari for the title only (index.html)
    override def bodyFont = |("pincer-ui")
    override def titleFont = |("luminari")

    def randomGameName() = {
        val n = $("Empire", "Colonies", "Muskets", "Frigates", "Forts", "Trade", "Crowns", "Seals", "Honour", "Powder").shuffle
        val c = $("for", "against", "versus", "through", "and", "of", "in", "as").shuffle
        n.head + " " + c.head + " " + n.last
    }

    def validateFactionCombination(factions : $[Faction]) = None ||
        (factions.num < 2).?(ErrorResult("Select at least two players")) ||
        (factions.num > 4).?(ErrorResult("Max four players")) |
        InfoResult("Pincer")

    def validateFactionSeatingOptions(factions : $[Faction], options : $[O]) = InfoResult("Pincer")

    def factionName(f : Faction) = f.name
    def factionElem(f : Faction) = f.name.styled(f)

    // the reworked setup screens, with each color's colony
    override def setupDesign = true
    override def factionsWord = "players"
    override def setupRowsLabel = "Players"
    override def menuImages = Faction.all./(f => ("menu-" + f.id) -> ("/hrf/webp2/pincer/images/piece/colony-" + f.id + ".webp")).toMap
    override def factionTile(f : Faction) = |(Div(Image("menu-" + f.id, ExternalStyle("setup-emblem"), "")) ~ Div(f.name.hlb))
    override def factionRowIcon(f : Faction) : Elem = Image("menu-" + f.id, ExternalStyle("setup-row-emblem"), "")
    override def factionChosenElem(f : Faction) = factionRowIcon(f) ~ factionElem(f).spn(xstyles.bold)

    override def glyph(f : F) : |[String] = |("piece:colony-" + f.id)

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
        ImageAsset("map") ::
        ImageAsset("hex-select") ::
        ImageAsset("hex-target") ::
    $) ::
    ConditionalAssetsList((factions : $[F], options : $[O]) => true, "piece", "piece:", "webp")(
        Faction.all./~(f =>
            $("colony", "fort", "port", "musketeer", "cavalry", "cannon", "frigate")./(n => ImageAsset(n + "-" + f.id)) ++
            $("road", "bridge")./~(n => $("e", "ne", "se")./(d => ImageAsset(n + "-" + f.id + "-" + d)))
        )
    ) ::
    ConditionalAssetsList((factions : $[F], options : $[O]) => true, "card", "card:", "webp")(
        (Resource.all./(_.image) ++ Maneuver.all./(_.image) :+ "card-back" :+ "reference")./(ImageAsset(_))
    ) ::
    ConditionalAssetsList((factions : $[F], options : $[O]) => true, "seal", "seal:", "webp")(
        Seal.all./(_.image)./(ImageAsset(_))
    ) ::
    ConditionalAssetsList((factions : $[F], options : $[O]) => true, "die", "die:", "webp")(
        Resource.all./(_.id)./(ImageAsset(_))
    ) ::
    ConditionalAssetsList((factions : $[F], options : $[O]) => true, "icon", "icon:", "webp")(
        Icons.all./(ImageAsset(_))
    ) ::
    ConditionalAssetsList((factions : $[F], options : $[O]) => true, "war", "war:", "webp")(
        Faction.all./~(f => $("gentleman", "boar")./(n => ImageAsset(n + "-" + f.id)))
    ) ::
    $
}
