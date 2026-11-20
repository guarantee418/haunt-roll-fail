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

import hrf.meta._
import hrf.elem._
import hrf.options._

import root.elem._

// The site's Root tournament (asked for on 2026-11-20; the sign-ups, tables and final are on the server, good-game/Tournament.scala):
// each table is an Advanced Setup game with this hidden option, whose draft is
//   1. the flop: the factions drawn for the draft (players + 1), shown to everyone,
//   2. the first seat picks the deck (one of the official ones),
//   3. the second seat picks the map (one of the official ones),
//   4. on the Mountain, the third seat picks what goes in the Tower's clearing (the Tower, or a suited landmark),
//   5. the factions are picked as in Advanced Setup, last seat first.
// The game is created with no map or deck; the picks are added to game.options, and the board, the deck and the expansions
// are made again from them. Until the map is picked the game stands on the Autumn map, which the UI doesn't draw (mapPending).
case object TournamentDraft extends GameOption with ToggleOption {
    val group = "Tournament"
    val valueOn = "Tournament".hh ~ " draft"
    override val explain = $(
        "The site's tournament games: after the factions are drawn, the first seat picks the deck, the second the map, and on the " ~ "Mountain".hh ~ " the third what goes in the " ~ "Tower".hh ~ "'s clearing; then the factions are picked, last seat first.",
    )
}

case object TournamentFlopAction extends ForcedAction
case object TournamentDeckAskAction extends ForcedAction
case object TournamentMapAskAction extends ForcedAction
case object TournamentLandmarkAskAction extends ForcedAction

case class TournamentDeckAction(self : Player, d : DeckOption) extends BaseAction(self, "picks the deck")(d.valueOn)
case class TournamentMapAction(self : Player, m : MapOption) extends BaseAction(self, "picks the map")(m.valueOn)
case class TournamentLandmarkAction(self : Player, l : Landmark) extends BaseAction(self, "picks what goes in the", TowerLandmark, "clearing")(l)

object TournamentExpansion extends Expansion {
    def active(factions : $[Faction], options : $[Meta.O]) = options.has(TournamentDraft)

    val decks : $[DeckOption] = $(StandardDeck, ExilesDeck, SquiresDeck)
    val maps : $[MapOption] = $(AutumnMap, WinterMap, LakeMap, MountainMap, GorgeMap, MarshMap)
    val landmarks : $[Landmark] = $(TowerLandmark, LostCityLandmark, MouseholdLandmark, FoxburrowLandmark, RabbittownLandmark)

    def mapBoard(m : MapOption) : Board = m match {
        case AutumnMap => AutumnBoard
        case WinterMap => WinterBoard
        case LakeMap => LakeBoard
        case MountainMap => MountainBoard
        case GorgeMap => GorgeBoard
        case MarshMap => MarshBoard
        case _ => AutumnBoard
    }

    // the stand-in board of a tournament game whose map isn't picked yet
    def boardOf(options : $[Meta.O]) : |[Board] = (options.has(TournamentDraft) && options.of[MapOption].none).?(AutumnBoard)

    // the faction draw has dealt the Knaves' Captain cards (KnavesCaptainsInDraft)
    def captainsDealt(implicit game : Game) = game.options.has(KnavesCaptainsInDraft).not || game.drawn.of[Knaves].forall(game.draftCaptains.contains)

    def seat(n : Int)(implicit game : Game) = game.seating(n % game.seating.num)

    def flop(implicit game : Game) : Elem = game.playChoices./(_.elem).join(Text(", "))

    def perform(action : Action, soft : Void)(implicit game : Game) = action @@ {
        // the factions are drawn first, before the map is set up
        case SetupMapAction if game.drawn.none =>
            DraftFactionsAction

        case DraftCaptainsAction if game.tournamentDeck.none && captainsDealt =>
            TournamentFlopAction

        case TournamentFlopAction =>
            log("The flop".hl ~ ":", game.playChoices./(_.elem).comma)

            Milestone(TournamentDeckAskAction)

        case TournamentDeckAskAction =>
            val p = seat(0)

            Ask(p).group("The flop".hl ~ ": " ~ flop).each(decks)(d => TournamentDeckAction(p, d))

        case TournamentDeckAction(p, d) =>
            game.tournamentDeck = Some(d)
            game.options = game.options.notOf[DeckOption] :+ d

            game.cards.replace(game.pile, Deck.fromOptions(game.options, game.players.num, game.players.of[Faction]))
            game.expansions = game.expansionsFor(game.candidates ++ game.options.of[IncludeFaction]./(_.faction))

            log(p, "picked the", d.valueOn, "deck")

            TournamentMapAskAction

        case TournamentMapAskAction =>
            val p = seat(1)

            Ask(p).group("The flop".hl ~ ": " ~ flop ~ Break ~ "Deck".hl ~ ": " ~ game.tournamentDeck./(_.valueOn).|(Empty)).each(maps)(m => TournamentMapAction(p, m))

        case TournamentMapAction(p, m) =>
            game.tournamentMap = Some(m)
            // every map with All Random clearings and its own landmarks (the Mountain's Tower clearing is picked next), keeping the
            // Simple Landmarks the game has (the Birdsong Tower, 2026-11-20), placed after the map's own
            game.options = game.options.notOf[MapOption].notOf[ClearingsOption].%(o => o.is[LandmarksOption].not || o.is[SimpleLandmark]) :+ m :+ AllRandomClearings :+ MountainSuitedLandmark

            game.board = mapBoard(m)
            game.rubble = game.board.rubble
            game.graveyard = game.board.clearings./(_ -> $[Figure]()).toMap

            log(p, "picked the", m.valueOn, "map")

            if (game.board == MountainBoard)
                TournamentLandmarkAskAction
            else
                Milestone(SetupMapAction)

        case TournamentLandmarkAskAction =>
            val p = seat(2)

            Ask(p).group("The flop".hl ~ ": " ~ flop ~ Break ~ p.elem ~ " picks what goes in the " ~ TowerLandmark.elem ~ "'s clearing (" ~ game.board.tower./(_.elem).join(Text(", ")) ~ ")").each(landmarks)(l => TournamentLandmarkAction(p, l))

        case TournamentLandmarkAction(p, l) =>
            game.tournamentLandmark = Some(l)

            log(p, "picked", l, "for the", TowerLandmark, "clearing")

            Milestone(SetupMapAction)

        // the Mountain's Tower clearing gets the landmark the third seat picked (the Tower itself, or a suited one)
        case SetupLandmarksAction if game.board == MountainBoard && game.tournamentLandmark.any && game.board.tower.any && game.tower.none && game.lostCity.none && game.mousehold.none && game.foxburrow.none && game.rabbittown.none =>
            val c = game.board.tower.head

            val next : ForcedAction = game.tournamentLandmark.get match {
                case TowerLandmark =>
                    game.ferry = game.board.ferry
                    game.tower = game.board.tower

                    // the Simple Landmarks are placed next (SetupLandmarksAction would draw a suited landmark for the Tower)
                    if (game.options.of[SimpleLandmark].any) {
                        game.landmarkDefaultsSet = true
                        SetupNextLandmarkAction
                    }
                    else
                        SetupHirelingsAction

                case LostCityLandmark =>
                    game.lostCity :+= c
                    game.mapping += c -> FoxRabbitMouse
                    game.original = game.mapping

                    log(LostCityLandmark, "was placed in", c, "instead of the", TowerLandmark)

                    SetupLandmarksAction

                case l =>
                    SetupExpansion.placeHomelandLandmark(l, c)

                    log(l, "was placed in", c, "instead of the", TowerLandmark)

                    SetupLandmarksAction
            }

            next

        // the factions were drawn before the map: on to the picks
        case DraftFactionsAction if game.drawn.any =>
            Milestone(SetupFactionsAction)

        case _ => UnknownContinue
    }
}
