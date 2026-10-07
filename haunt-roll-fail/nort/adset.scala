package nort
//
//
//
//
import hrf.colmat._
import hrf.compute._
import hrf.logger._
//
//
//
//

import hrf.meta._
import hrf.options._
import hrf.elem._

import nort.elem._


// ADSET: a free-for-all with a clan draft. The players are seats (Seat) in a random order; the clans drafted are
// laid out, the first seat bans one, the second picks Wilderness or Wastelands, the third the central tile; then from
// the last seat to the first each picks a clan, places a tile and three units, and all do it again with a second tile.
// Always with the Creatures module and the Uncharted Horizons Development and Achievement cards.

// Who decides between Wilderness and Wastelands (or which one was agreed at setup)
trait AdsetLands extends GameOption with OneOfGroup with ImportantOption {
    val group = "Wilderness or Wastelands".txt
}

case object LandsSeatChooses extends AdsetLands {
    def valueOn = "The second seat chooses".txt
    override val explain = $("During setup the second seat chooses to play with " ~ "Wilderness".hl ~ " or " ~ "Wastelands".hl ~ " (not with two players).")
}

case object LandsWilderness extends AdsetLands {
    def valueOn = "Wilderness".txt
    override val explain = $("Agreed at setup: the game is played with " ~ "Wilderness".hl ~ ".")
}

case object LandsWastelands extends AdsetLands {
    def valueOn = "Wastelands".txt
    override val explain = $("Agreed at setup: the game is played with " ~ "Wastelands".hl ~ ".")
}

case object LandsNeither extends AdsetLands {
    def valueOn = "Neither".txt
    override val explain = $("Neither expansion (the two-player default).".txt)
}

object AdsetLands {
    val all : $[AdsetLands] = $(LandsSeatChooses, LandsWilderness, LandsWastelands, LandsNeither)
}


// The random seat order, the clans drafted, the map tiles
case class AdsetSeatingAction(shuffled : $[Player]) extends ShuffledAction[Player]
case class AdsetClansAction(shuffled : $[Faction]) extends ShuffledAction[Faction]
case class AdsetTilesAction(shuffled : $[String]) extends ShuffledAction[String]

// The ban and the pick show the clans as the clan picker does: emblem, name and the "i" button (AdsetExpansion.clanTile)
trait AdsetDraftChoice { def clan : Faction }

case class AdsetBanAction(self : Player, clan : Faction, then : ForcedAction) extends BaseAction(self, "bans a clan from the draft")(AdsetExpansion.clanTile(clan, true)) with AdsetDraftChoice
case object AdsetLandsStepAction extends ForcedAction
case class AdsetLandsAction(self : Player, lands : String) extends BaseAction(self, "chooses the expansion")(AdsetExpansion.landsName(lands).hl)
case object AdsetCentralStepAction extends ForcedAction
case class AdsetCentralAction(self : Player, tile : String) extends BaseAction(self, "chooses the central tile")(AdsetExpansion.centralLabel(tile))
// round 1: each player in l picks a clan, then places a tile and three units; round 2: a second tile and three units
case class AdsetTurnAction(round : Int, l : $[Player]) extends ForcedAction
case class AdsetPickAction(self : Player, clan : Faction, rest : $[Player]) extends BaseAction(self, "chooses a clan")(AdsetExpansion.clanTile(clan, true)) with AdsetDraftChoice

// The draft, shown to everyone while it lasts; tapping a clan shows its info
case class AdsetClanInfo(clan : Faction)
case class AdsetClanInfoAction(title : Elem, clan : Faction) extends BaseInfo(title)(AdsetExpansion.clanTile(clan, false)) with AdsetDraftChoice with OnClickInfo { def param = AdsetClanInfo(clan) }
// The three map tiles a seat drew, shown to it until it picks its clan
case class AdsetTileInfoAction(title : Elem, tile : String) extends BaseInfo(title)(Image("tile-" + tile, styles.seatTile))


object AdsetExpansion extends Expansion {
    def landsName(lands : String) = (lands == "wilderness").?("Wilderness").|("Wastelands")

    // A central tile and what it does
    def centralLabel(tile : String) : Elem = CentralChoice.tiles.find(_.tile.has(tile)) match {
        case Some(o) => o.explain.take(1)./(_.div).merge
        case None => (Waste.elem(tile) ~ ": the core game's starting tile.").div
    }

    // A clan as in the clan picker (Meta.factionTile): its emblem and name, and the "i" button for its info (Meta.factionInfo)
    def clanTile(clan : Faction, button : Boolean) : Elem =
        Div(Image("clan-" + clan.style, styles.pickIcon)) ~ Div(clan.name.hlb) ~
        button.?(Div(Parameter(AdsetClanInfo(clan), OnClick(Span("i".txt, $(xstyles.outlined, xstyles.tileButton)))))).|(Empty)

    // Clans are still to be picked
    def drafting(implicit game : Game) = game.setup.num < game.arity

    // The clans that can still be picked
    def remaining(implicit game : Game) = game.draft.diff(game.banned).diff(game.setup)

    def seatElem(p : Player) : Elem = p match {
        case p : Seat => p.elem
        case f : Faction => f.name.hl
    }

    // Each player's color, by player number
    def color(p : Player)(implicit game : Game) : PlayerColor = PlayerColor.all(game.players.indexOf(p))

    // The player who picks the central tile: the third seat, the second with two players
    def centralSeat(implicit game : Game) = game.seats((game.arity >= 3).?(2).|(1))

    // p plays c from now on
    def assign(p : Player, c : Faction)(implicit game : Game) {
        game.ptf += p -> c
        game.ftp += c -> p
        game.setup = game.seats./~(game.ptf.get)
        game.factions = game.setup
        game.seating = game.setup
        game.colors += c -> color(p)
        game.states += c -> new FactionState(c)
        game.tileHand += c -> game.seatTiles.get(p).|($)
        game.seatTiles -= p

        // New Blood: Dragon Clan's first unit on the Pyre
        if (c == Dragon)
            game.pyre = $(Dragon)
    }

    // The clans aren't shown again to a player choosing among them
    def info(player : |[Player], actions : $[UserAction])(implicit game : Game) : $[Info] =
        game.seats.any.??(
            $(Info("Seats:", game.seats.zipWithIndex./{ case (p, i) => ((i + 1) + ". ").hl ~ seatElem(p).styled(color(p)) ~ game.ptf.get(p)./(c => " " ~ c.name.hlb).|(Empty) }.join(", "))) ++
            game.banned.any.$(Info("Banned:", game.banned./(_.name.hl).join(", "))) ++
            actions.exists(a => a.unwrap.is[AdsetBanAction] || a.unwrap.is[AdsetPickAction]).not.??(remaining./(c => AdsetClanInfoAction("Clans in the draft".hl, c))) ++
            player./~(game.seatTiles.get).|($)./(t => AdsetTileInfoAction("Your map tiles".hl, t))
        )

    def perform(action : Action, soft : Void)(implicit game : Game) = action @@ {
        // After the decks are shuffled: the seats in a random order, then the clans drafted
        case ShuffleStartingDecksAction(Nil) if game.seats.none =>
            Shuffle[Player](game.players, AdsetSeatingAction(_))

        case AdsetSeatingAction(l) =>
            game.seats = l

            log("Adset".hlb, "seat order:", l.zipWithIndex./{ case (p, i) => ((i + 1) + ". ").hl ~ seatElem(p) }.join(", "))

            Shuffle[Faction](Meta.clans, AdsetClansAction(_))

        case AdsetClansAction(l) =>
            // The number of players plus two, one more with two players
            game.draft = l.take(game.arity + 2 + (game.arity == 2).??(1))

            log("Clans in the draft:", game.draft./(_.name.hl).join(", "))

            Shuffle[String](Tiles.regular./(_.id), AdsetTilesAction(_))

        case AdsetTilesAction(l) =>
            game.pile = l

            game.seats.foreach { p =>
                game.seatTiles += p -> game.pile.take(3)
                game.pile = game.pile.drop(3)
            }

            log("Each player drew three map tiles")

            val p = game.seats.first

            Ask(p).each(game.draft)(c => AdsetBanAction(p, c, AdsetLandsStepAction))

        case AdsetBanAction(p, c, then) =>
            game.banned :+= c

            log(p, "banned", c.name.hl, "from the draft")

            Then(then)

        // Wilderness or Wastelands: the second seat chooses, or it was agreed at setup (with two players only then)
        case AdsetLandsStepAction =>
            options.of[AdsetLands].single match {
                case Some(LandsSeatChooses) if game.arity > 2 =>
                    val p = game.seats(1)
                    Ask(p).add(AdsetLandsAction(p, "wilderness")).add(AdsetLandsAction(p, "wastelands"))
                case Some(LandsWilderness) =>
                    game.addOption(ModuleOption(Wilderness))
                    log("The game is played with", "Wilderness".hl)
                    Then(AdsetCentralStepAction)
                case Some(LandsWastelands) =>
                    game.addOption(ModuleOption(Wastelands))
                    log("The game is played with", "Wastelands".hl)
                    Then(AdsetCentralStepAction)
                case _ =>
                    Then(AdsetCentralStepAction)
            }

        case AdsetLandsAction(p, lands) =>
            game.addOption(ModuleOption((lands == "wilderness").?(Wilderness : Module).|(Wastelands)))

            log(p, "chose to play with", landsName(lands).hl)

            Then(AdsetCentralStepAction)

        // The central tile: the standard one, any Wastelands central tile (but its Wyvern's Den with Wilderness), the Wilderness Great Lake
        case AdsetCentralStepAction =>
            val p = centralSeat
            val wild = game.has(Wilderness)
            val tiles = "start" +: Waste.centrals.%(t => (wild && t == Waste.den).not) :+ Wild.lake

            Ask(p).each(tiles)(t => AdsetCentralAction(p, t))

        case AdsetCentralAction(p, tile) =>
            game.addOption(CentralChoice.all.find(_.tile.has(tile)).|(StandardCentral))

            log(p, "chose", Waste.elem(tile), "as the central tile")

            // The Creatures deck and the central tile, as at the usual setup (MapExpansion then goes on with AdsetTurnAction)
            game.internalPerform(ShuffledTilesAction(game.pile), soft)

        // From the last seat to the first: pick a clan, place a tile and three units; then a second tile and three units
        case AdsetTurnAction(1, Nil) =>
            Then(AdsetTurnAction(2, game.seats.reverse))

        case AdsetTurnAction(1, p :: rest) =>
            Ask(p).each(remaining)(c => AdsetPickAction(p, c, rest))

        case AdsetPickAction(p, c, rest) =>
            assign(p, c)

            log(p, "plays", c, "as", color(p))

            game.adsetNext = |((1, rest))

            // Two players: the second seat, who picks first, then bans a clan, leaving two for the first seat
            if (game.arity == 2 && rest.any)
                Ask(p).each(remaining)(b => AdsetBanAction(p, b, SetupPlaceAction(1, $(c))))
            else
                Then(SetupPlaceAction(1, $(c)))

        case AdsetTurnAction(2, p :: rest) =>
            game.adsetNext = |((2, rest))

            Then(SetupPlaceAction(2, $(game.ptf(p))))

        case SetupPlaceAction(_, Nil) if game.adsetNext.any =>
            val (round, rest) = game.adsetNext.get
            game.adsetNext = None

            Then(AdsetTurnAction(round, rest))

        // The tiles left over go to the bottom of the pile, after the Environment tiles are shuffled in (StartYearAction)
        case AdsetTurnAction(_, Nil) =>
            game.leftovers = game.setup./~(f => game.tileHand.get(f).|($))
            game.setup.foreach(f => game.tileHand += f -> $)

            Then(ShuffleStartingDecksAction(game.setup))

        // The first seat begins
        case ShuffleStartingDecksAction(Nil) =>
            Random[Faction]($(game.setup.first), FirstPlayerAction(_))

        // No reshuffle: the pile stays as it is (Wilderness and Wastelands still shuffle their Environment tiles in)
        case ShuffledTilesBackAction(l) if game.has(Wilderness).not && Waste.module.not =>
            game.pile = l

            Then(StartYearAction)

        case StartYearAction if game.leftovers.any =>
            game.pile ++= game.leftovers

            log("The", game.leftovers.num.hl, "leftover tiles went to the bottom of the pile")

            game.leftovers = $

            UnknownContinue

        case _ => UnknownContinue
    }
}


// A seat's bot: random during the draft, then the clan's Easy or Hard bot
class BotAdset(p : Player, hard : Boolean) extends EvalBot {
    def draft(a : Action) = a.unwrap match {
        case _ : AdsetBanAction | _ : AdsetLandsAction | _ : AdsetCentralAction | _ : AdsetPickAction => true
        case _ => false
    }

    def eval(actions : $[UserAction])(implicit game : Game) : Compute[$[ActionEval]] =
        game.ptf.get(p) match {
            case Some(f) if actions.exists(draft).not => hard.?(new BotHard(f) : EvalBot).|(new BotXX(f)).eval(actions)
            // At random: the sort keeps the order of equal choices, so unshuffled it would always take the first
            case _ => actions.shuffle./(a => ActionEval(a, $))
        }
}


// The Adset game: on the Northgard main menu (Meta.linkedModes); seats instead of clans, drafted in the game
object MetaAdset extends MetaGame {
    val gaming = nort.gaming

    type F = Seat

    def tagF = implicitly

    val name = "nort-adset"

    // Northgard's images
    override def path = "nort"
    val label = "Northgard: Adset"

    val factions = 1.to(6).$./(Seat)

    val minPlayers = 2

    override val indistinguishableFactions : Boolean = true
    override val gradualFactions : Boolean = true

    override def settingsKey = Meta.settingsKey
    override def settingsList = Meta.settingsList
    override def settingsDefaults = Meta.settingsDefaults

    override val showAbout = false

    override def menuAbout = |(
        "A free-for-all with a clan draft, always with the " ~ "Creatures".hl ~ " module and the Uncharted Horizons Development and Achievement cards." ~ Break ~
        "The seat order is random. The number of players plus two clans are drafted (five with two players). The first seat bans a clan, the second chooses " ~ "Wilderness".hl ~ " or " ~ "Wastelands".hl ~ ", the third the central tile." ~ Break ~
        "From the last seat to the first, each player picks a clan, places a tile and three units; then each places a second tile and three units. The first seat begins."
    )

    override def intLinks = $(("Northgard: Uncharted Lands".spn -> "nort"))

    // Always on: the Creatures module and the Uncharted Horizons Development and Achievement cards
    val always : $[O] = $(ModuleOption(Creatures), HorizonsDevelopments)

    val options : $[O] = YearsOption.all ++ $(StandardVictory, FameOnly, AltVictoryRandom, AltVictoryChosen, VictoryModeOption(false), VictoryModeOption(true)) ++ VictoryCardOption.all ++ $(CombatReportAttackers, CombatReportDefenders) ++ WarchiefsChoice.all ++ $(ModuleOption(Sea)) ++ AdsetLands.all

    // With two players the second seat doesn't choose: an expansion is agreed on here, or none
    override def optionsFor(n : Int, l : $[F]) = options.%{
        case LandsSeatChooses => n > 2
        case _ => true
    }

    override def optionShown(o : O, selected : $[O]) = Meta.optionShown(o, selected)

    override def defaultsFor(n : Int, l : $[F]) = $(YearsOption.standard, StandardVictory, VictoryModeOption(false), (n > 2).?(LandsSeatChooses).|(LandsNeither))

    override def quickOptions = Map()

    val quickMin = 3
    val quickMax = 3

    override def quickFactions = factions.take(3)

    def randomGameName() = Meta.randomGameName()

    def validateFactionCombination(factions : $[F]) = InfoResult("Northgard: Adset")

    def validateFactionSeatingOptions(factions : $[F], options : $[O]) : ValidationResult =
        Meta.validateVictoryCards($, options) | InfoResult("Northgard: Adset")

    def factionName(f : F) = f.name
    def factionElem(f : F) = f.name.hh

    def createGame(factions : $[F], options : $[O]) = new Game(factions, options.diff(always) ++ always)

    def getBots(f : F) = $("Easy", "Hard")

    def getBot(f : F, b : String) = new BotAdset(f, b == "Hard")

    def defaultBot(f : F) = "Easy"

    def writeFaction(f : F) = f.short
    def parseFaction(s : String) : |[F] = factions.%(_.short == s).single

    def writeOption(o : O) = Meta.writeOption(o)
    def parseOption(s : String) = $(options.find(o => writeOption(o) == s) || options.find(o => o.toString == s) | (UnknownOption(s)))

    def parseAction(s : String) : Action = Serialize.parseAction(s)
    def writeAction(a : Action) : String = Serialize.write(a)

    val start = StartAction(gaming.version)

    // Any clan can be drafted, and Wilderness, Wastelands and every central tile can be chosen in the game
    val assets = Meta.assets./(a => ConditionalAssetsList(
        (factions : $[F], options : $[O]) => a.condition(Meta.clans, options ++ always ++ $(ModuleOption(Wilderness), ModuleOption(Wastelands), WildLakeCentral)),
        a.path, a.prefix, a.ext, a.lzy, a.scale, a.lossless)(a.list)) :+
    // The clan emblems, for the draft
    ConditionalAssetsList((factions : $[F], options : $[O]) => true, "clan", "clan-", "webp")(Meta.clans./(f => ImageAsset(f.style)))
}
