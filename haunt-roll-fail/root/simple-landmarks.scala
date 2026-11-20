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

import hrf.elem._
import root.elem._


// Simple Landmarks (the owner's own rules, asked for on 2026-11-20), four toggles in the setup's "Simple Landmarks" section
// (`SimpleLandmark` in root/meta.scala), with the pieces of the Landmarks Pack's Legendary Forge, Black Market and Elder Treetop and
// of the Tower:
//   Forge - If your faction pieces are in its clearing, treat it as one of your crafting pieces.
//   Market - Once per turn, if your faction pieces are in its clearing, you may discard a card to draw a card.
//   Birdsong Tower - If you rule its clearing at the start of your Birdsong, score a point.
//   Royal Tree - Adds a building slot. While it's your turn, if your faction pieces are in its clearing, you rule its clearing.
// Readings:
//   - They are placed at setup like the other landmark toggles (the players in reverse seating order, one landmark each in turn),
//     where the Landmarks Pack's setup cards put their pieces: the Forge in a clearing without a landmark and not next to one, the
//     Market in one with exactly one building slot and no ruin, not a landmark's nor next to one, the Birdsong Tower in a clearing
//     with a ruin and no landmark, the Royal Tree in a corner clearing, not a landmark's nor next to one; when no clearing fits, the
//     next rule is dropped (not next to one, then the slot, ruin or corner). The map's own landmarks (Landmarks section) are placed
//     first, and none of these share a clearing with another landmark. Like every landmark, they can't be battled, moved or removed.
//   - The Forge is a crafting piece of its clearing's suits (`c.asset`), for each faction with a piece there, used once a turn like
//     any crafting piece.
//   - The Market is offered on its user's own turn, in the Birdsong, Daylight and Evening menus that list such extras (like
//     Rabbittown), once a turn; the card discarded may be any card.
//   - The Birdsong Tower scores at the start of Birdsong, before anything else is done then (`BirdsongStartAction`).
//   - The Royal Tree's rule is on top of the usual rule: on your turn you rule there with any piece there, others may still rule it
//     by the usual count (on their own turns that is all that counts).
object SimpleLandmarksExpansion extends MandatoryExpansion {
    def placed(l : SimpleLandmark)(implicit game : Game) = l @@ {
        case SimpleForgeLandmark => game.simpleForge.any
        case SimpleMarketLandmark => game.simpleMarket.any
        case BirdsongTowerLandmark => game.birdsongTower.any
        case RoyalTreeLandmark => game.royalTree.any
    }

    def forgeCraft(f : Faction)(implicit game : Game) : $[SuitAsset] = game.simpleForge.%(f.present)./(_.asset)

    def treeRules(f : Player, c : Region)(implicit game : Game) : Boolean = f @@ {
        case f : Faction => c.as[Clearing].?(game.royalTree.has) && game.current == f && f.present(c)
        case _ => false
    }

    // the clearings a Simple Landmark may go in at setup, each rule dropped in turn when nothing is left
    def spots(l : SimpleLandmark)(implicit game : Game) : $[Clearing] = {
        val open = clearings.but(LegendSpace).diff(game.landmarks)
        val apart = open.diff(game.landmarks./~(game.connected))
        def fit(c : Clearing) : Boolean = l match {
            case SimpleForgeLandmark => true
            case SimpleMarketLandmark => game.slots(c) == 1 && game.ruins.contains(c).not
            case BirdsongTowerLandmark => game.ruins.contains(c)
            case RoyalTreeLandmark => board.corners.has(c)
        }
        val near = l != BirdsongTowerLandmark

        near.?(apart.%(fit)).|($).some.|(open.%(c => fit(c))).some.|(apart).some.|(open)
    }

    def place(l : SimpleLandmark, c : Clearing)(implicit game : Game) = l @@ {
        case SimpleForgeLandmark => game.simpleForge :+= c
        case SimpleMarketLandmark => game.simpleMarket :+= c
        case BirdsongTowerLandmark => game.birdsongTower :+= c
        case RoyalTreeLandmark => game.royalTree :+= c
    }

    def market(f : Faction)(implicit game : Game, ask : ActionCollector) {
        if (game.current == f && f.used.has(SimpleMarketUsed).not)
            game.simpleMarket.%(f.present).foreach { c =>
                + SimpleMarketMainAction(f, c, Repeat).!(f.hand.none, "no cards")
            }
    }

    override def birdsong(f : Faction)(implicit game : Game, ask : ActionCollector) { market(f) }
    override def daylight(f : Faction)(implicit game : Game, ask : ActionCollector) { market(f) }
    override def evening(f : Faction)(implicit game : Game, ask : ActionCollector) { market(f) }

    def perform(action : Action, soft : Void)(implicit game : Game) = action @@ {
        // SETUP
        case SetupLandmarkAction(f, l : SimpleLandmark, more) =>
            Ask(f).each(spots(l))(c => SetupLandmarkClearingAction(f, l, c)).cancelIf(more)

        case SetupLandmarkClearingAction(f, l : SimpleLandmark, c) =>
            place(l, c)

            log(f, "placed", l, "in", c)

            SetupNextLandmarkAction

        // BIRDSONG TOWER
        case BirdsongStartAction(f) if game.birdsongTower.any && game.birdsongTowerTurn != game.turn =>
            game.birdsongTowerTurn = game.turn

            game.birdsongTower.foreach { c =>
                if (f.rules(c))
                    f.oscore(1)("ruling", BirdsongTowerLandmark.elem, "at the start of", Birdsong)
            }

            BirdsongStartAction(f)

        // MARKET
        case SimpleMarketMainAction(f, c, then) =>
            OptionalDiscardCardAction(f, ToSimpleMarket(c), AnySuit, SimpleMarketAction(f, c, then))

        case SimpleMarketAction(f, c, then) =>
            val card = f.drawn.get

            f.drawn --> discard.quiet

            f.used :+= SimpleMarketUsed

            f.log("discarded", card, "at the", SimpleMarketLandmark.elem, "in", c, "to draw a card")

            DrawCardsAction(f, 1, SuppressLog, AddCardsAction(f, then))

        case _ => UnknownContinue
    }
}

case object SimpleMarketUsed extends DisplayEffect {
    override val name = "Market"
}

case class ToSimpleMarket(c : Clearing) extends Message {
    def elem(implicit game : Game) = "at the " ~ SimpleMarketLandmark.elem ~ " to draw a card"
}

case class SimpleMarketMainAction(self : Faction, c : Clearing, then : ForcedAction) extends BaseAction(None)(SimpleMarketLandmark.elem, "(discard a card to draw a card)") with Soft
case class SimpleMarketAction(self : Faction, c : Clearing, then : ForcedAction) extends ForcedAction
