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

import hrf.elem._

import nort.elem._

// Picking who goes, figure by figure (asked for on Discord on 2026-11-01, relayed by Gman): after the destination,
// one panel with the units as a counter ("−" and "+"), a Yes/No switch for the warchief and for each companion
// (Kaija, Brok, the Automa's Leader 2, Brundr and Kaelinn each), and the button that does it, naming who goes.
// The switches and the counter are soft (nothing is recorded); the button is the same recorded action as before
// (MoveUnitsAction, RetreatPartyAction, LynxGoAction...), so games recorded before it load and replay unchanged.
// FiguresExplodeAction lists every button the panel can lead to, for bots and move checks.

// Who is picked: units, the first companion (Kaija, Brok, Leader 2, or Brundr), the second lynx (Kaelinn), the warchief
case class FigurePick(units : Int, first : Boolean, second : Boolean, chief : Boolean) {
    def companions = first.??(1) + second.??(1)
    def any = units > 0 || first || second || chief
}

// What the figures are picked for: where they are, who is there, and the recorded action a pick leads to
trait FigureGoal extends GameElementary {
    def self : Faction
    def owner : Faction
    def from : AreaRef
    def territory(implicit game : Game) = game.board.territory(from)
    def maxUnits(implicit game : Game) : Int = game.count(territory, owner)
    // How many of the clan's companions may go: 2 for both lynxes
    def maxCompanions(implicit game : Game) : Int = (owner == Lynx).?(game.lynxesIn(territory)).|(game.kaijaIn(territory, owner).??(1))
    def chiefMay(implicit game : Game) : Boolean = game.chiefIn(territory, owner)
    // The pick the panel starts with
    def start(implicit game : Game) : FigurePick
    def finish(p : FigurePick)(implicit game : Game) : |[UserAction]
    def cancel(a : Ask)(implicit game : Game) : Ask = a.cancel

    def picks(implicit game : Game) : $[FigurePick] = {
        def may(b : Boolean) = b.?($(false, true)).|($(false))
        val c = maxCompanions
        for (n <- 0.to(maxUnits).$ ; a <- may(c > 0) ; b <- may(c > 1) ; w <- may(chiefMay)) yield FigurePick(n, a, b, w)
    }

    def ask(p : FigurePick)(implicit game : Game) : Ask = {
        val max = maxUnits
        val c = maxCompanions

        val names : $[Elem] = (owner == Lynx && c == 2).?($("Brundr".hl, "Kaelinn".hl)).|((owner == Lynx).?($("Brundr or Kaelinn".hl)).|($(Companion(owner).elem)))

        cancel(Ask(self)
            .add(FiguresExplodeAction(self, picks./~(finish).distinct))
            .when(max > 0)(FigureCountAction(self, this, p, -1).!(p.units <= 0))
            .when(max > 0)(FigureCountInfoAction(self, this, p.units, max))
            .when(max > 0)(FigureCountAction(self, this, p, +1).!(p.units >= max))
            .when(c > 0)(FigureSwitchAction(self, this, p, 1, names(0), p.first))
            .when(c > 1)(FigureSwitchAction(self, this, p, 2, names(1), p.second))
            .when(chiefMay)(FigureSwitchAction(self, this, p, 0, WarchiefElem(owner).elem, p.chief))
            .add(finish(p).|(FiguresNoneAction(self, this))))
    }
}

case class FigureCountAction(self : Faction, goal : FigureGoal, p : FigurePick, d : Int) extends BaseAction(goal)((d < 0).?("−").|("+")) with Soft with NoExplode with NoClear with SkipValidate with SelfPerform {
    def perform(soft : Void)(implicit game : Game) = goal.ask(p.copy(units = p.units + d))
}

case class FigureCountInfoAction(self : Faction, goal : FigureGoal, n : Int, max : Int) extends BaseInfo(goal)(n.hl ~ " of " ~ max.hl ~ (max == 1).?(" unit").|(" units"))

case class FigureSwitchAction(self : Faction, goal : FigureGoal, p : FigurePick, which : Int, name : Elem, on : Boolean) extends BaseAction(goal)(name ~ ":", on.?("Yes".hl).|("No".txt)) with Soft with NoExplode with NoClear with SkipValidate with SelfPerform with Selectable {
    def selected = on
    def perform(soft : Void)(implicit game : Game) = goal.ask(which match {
        case 0 => p.copy(chief = p.chief.not)
        case 1 => p.copy(first = p.first.not)
        case _ => p.copy(second = p.second.not)
    })
}

case class FiguresNoneAction(self : Faction, goal : FigureGoal) extends BaseInfo(goal)("Pick who goes")

case class FiguresExplodeAction(self : Faction, all : $[UserAction]) extends HiddenChoice with HalfExplode {
    def expand(target : |[Action]) = all
}

// The panel opened right away (no step of its own), by the soft step that picked the destination
object FigurePicker {
    def apply(goal : FigureGoal)(implicit game : Game) : Ask = goal.ask(goal.start)
}

// A Move card's Move (MoveToAction)
case class MoveGoal(self : Faction, from : AreaRef, to : AreaRef, cost : Int, left : Int, e : MoveEffect, kaija : Boolean, then : ForcedAction) extends FigureGoal {
    def owner = self
    def elem(implicit game : Game) = "Move from " ~ from.elem ~ " to " ~ to.elem
    override def maxCompanions(implicit game : Game) = kaija.??(super.maxCompanions)
    def start(implicit game : Game) = FigurePick(maxUnits, false, false, false)
    def finish(p : FigurePick)(implicit game : Game) =
        p.any.?((owner == Lynx && p.companions == 1 && maxCompanions == 2).?(MoveOneLynxAction(self, from, to, p.units, p.chief, cost, left, e, then) : UserAction).|(MoveUnitsAction(self, from, to, p.units, p.companions > 0, p.chief, cost, left, e, then)))
}

// A retreat after a lost fight (RetreatPickAction); everyone left goes somewhere, so a pick only splits them
case class RetreatGoal(self : Faction, from : AreaRef, to : AreaRef, rough : Boolean, then : ForcedAction) extends FigureGoal {
    def owner = self
    def elem(implicit game : Game) = "Retreat from " ~ from.elem ~ " to " ~ to.elem
    def start(implicit game : Game) = FigurePick(maxUnits, maxCompanions > 0, maxCompanions > 1, chiefMay)
    def finish(p : FigurePick)(implicit game : Game) =
        p.any.?((owner == Lynx && p.companions == 1 && maxCompanions == 2).?(RetreatOneLynxAction(self, from, to, p.units, p.chief, rough, then) : UserAction).|(RetreatPartyAction(self, from, to, p.units, p.companions > 0, p.chief, rough, then)))
    override def cancel(a : Ask)(implicit game : Game) = MapExpansion.retreatCount(a, owner, from, rough)
}

// Brand's Bravery: another clan picks who retreats where
case class BrandRetreatGoal(self : Faction, owner : Faction, from : AreaRef, to : AreaRef, rough : Boolean, then : ForcedAction) extends FigureGoal {
    def elem(implicit game : Game) = "Brand's Bravery".hl ~ ": " ~ owner.elem ~ " retreats from " ~ from.elem ~ " to " ~ to.elem
    def start(implicit game : Game) = FigurePick(maxUnits, maxCompanions > 0, maxCompanions > 1, chiefMay)
    def finish(p : FigurePick)(implicit game : Game) =
        p.any.?((owner == Lynx && p.companions == 1 && maxCompanions == 2).?(BrandRetreatOneLynxAction(self, owner, from, to, p.units, p.chief, rough, then) : UserAction).|(BrandRetreatPartyAction(self, owner, from, to, p.units, p.companions > 0, p.chief, rough, then)))
    override def cancel(a : Ask)(implicit game : Game) = MapExpansion.retreatCount(a, owner, from, rough)
}

// Lynx Clan's bonus Move: one or both lynxes, and up to `units` of the units just recruited there
case class LynxBonusGoal(self : Faction, from : AreaRef, to : AreaRef, units : Int, then : ForcedAction) extends FigureGoal {
    def owner = self
    def elem(implicit game : Game) = "Brundr and Kaelinn".hl ~ " bonus Move from " ~ from.elem ~ " to " ~ to.elem
    override def maxUnits(implicit game : Game) = units
    override def chiefMay(implicit game : Game) = false
    def start(implicit game : Game) = FigurePick(0, true, maxCompanions > 1, false)
    def finish(p : FigurePick)(implicit game : Game) = (p.companions > 0).?(LynxGoAction(self, from, to, p.companions, p.units, then))
}
