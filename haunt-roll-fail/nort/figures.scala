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
// (and, among the casualties, Jötunn Blainn and the Military paths' bodyguard)
case class FigurePick(units : Int, first : Boolean, second : Boolean, chief : Boolean, blainn : Boolean = false, guard : Boolean = false) {
    def companions = first.??(1) + second.??(1)
    def any = units > 0 || first || second || chief || blainn || guard
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
    // Jötunn Blainn (Wastelands), only among the casualties
    def blainnMay(implicit game : Game) : Boolean = false
    // The Military paths' bodyguard (Leadership), only among the casualties
    def guardMay(implicit game : Game) : Boolean = false
    // What the last button says while the pick leads nowhere
    def none(p : FigurePick)(implicit game : Game) : Elem = "Pick who goes".txt
    // The pick the panel starts with
    def start(implicit game : Game) : FigurePick
    def finish(p : FigurePick)(implicit game : Game) : |[UserAction]
    def cancel(a : Ask)(implicit game : Game) : Ask = a.cancel

    // One of the two lynxes picked: the second slot's goes (or is lost), so the first slot's needs the names swapped first
    def named(p : FigurePick, a : UserAction)(implicit game : Game) : UserAction =
        (owner == Lynx && maxCompanions == 2 && p.first && p.second.not).?(LynxFirstAction(self, a)).|(a)

    def picks(implicit game : Game) : $[FigurePick] = {
        def may(b : Boolean) = b.?($(false, true)).|($(false))
        val c = maxCompanions
        for (n <- 0.to(maxUnits).$ ; a <- may(c > 0) ; b <- may(c > 1) ; w <- may(chiefMay) ; j <- may(blainnMay) ; g <- may(guardMay)) yield FigurePick(n, a, b, w, j, g)
    }

    def ask(p : FigurePick)(implicit game : Game) : Ask = {
        val max = maxUnits
        val c = maxCompanions

        // Lynx: the first switch is the first slot's lynx, the second the second's (game.lynxName)
        val names : $[Elem] = (owner == Lynx && c == 2).?($(game.lynxName(false).hl, game.lynxName(true).hl)).|((owner == Lynx && game.options.has(PrintedNewBlood)).?($(game.lynxNameIn(territory).hl)).|($(Companion(owner).elem)))

        cancel(Ask(self)
            .add(FiguresExplodeAction(self, picks./~(finish).distinct))
            .when(max > 0)(FigureCountAction(self, this, p, -1).!(p.units <= 0))
            .when(max > 0)(FigureCountInfoAction(self, this, p.units, max))
            .when(max > 0)(FigureCountAction(self, this, p, +1).!(p.units >= max))
            .when(c > 0)(FigureSwitchAction(self, this, p, 1, names(0), p.first))
            .when(c > 1)(FigureSwitchAction(self, this, p, 2, names(1), p.second))
            .when(chiefMay)(FigureSwitchAction(self, this, p, 0, WarchiefElem(owner).elem, p.chief))
            .when(blainnMay)(FigureSwitchAction(self, this, p, 3, "Jötunn Blainn".hl, p.blainn))
            .when(guardMay)(FigureSwitchAction(self, this, p, 4, "The bodyguard".hl, p.guard))
            .add(finish(p).|(FiguresNoneAction(self, this, none(p)))))
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
        case 3 => p.copy(blainn = p.blainn.not)
        case 4 => p.copy(guard = p.guard.not)
        case _ => p.copy(second = p.second.not)
    })
}

case class FiguresNoneAction(self : Faction, goal : FigureGoal, label : Elem) extends BaseInfo(goal)(label)

// The first slot's lynx picked alone out of a pair: the names are swapped (game.lynxSwapped), then the same recorded action as before
// does it with the second slot's figure, now bearing the picked name; the label is shown with the names swapped
case class LynxFirstAction(self : Faction, a : UserAction) extends BaseAction((g : Game) => LynxFirstAction.swapped(a.question(g))(g))((g : Game) => LynxFirstAction.swapped(a.option(g))(g))

object LynxFirstAction {
    def swapped(e : => Elem)(implicit game : Game) : Elem = {
        game.lynxSwapped = game.lynxSwapped.not
        try e finally { game.lynxSwapped = game.lynxSwapped.not }
    }
}

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
        p.any.?((owner == Lynx && p.companions == 1 && maxCompanions == 2).?(named(p, MoveOneLynxAction(self, from, to, p.units, p.chief, cost, left, e, then))).|(MoveUnitsAction(self, from, to, p.units, p.companions > 0, p.chief, cost, left, e, then)))
}

// A retreat after a lost fight (RetreatPickAction); everyone left goes somewhere, so a pick only splits them
case class RetreatGoal(self : Faction, from : AreaRef, to : AreaRef, rough : Boolean, then : ForcedAction) extends FigureGoal {
    def owner = self
    def elem(implicit game : Game) = "Retreat from " ~ from.elem ~ " to " ~ to.elem
    def start(implicit game : Game) = FigurePick(maxUnits, maxCompanions > 0, maxCompanions > 1, chiefMay)
    def finish(p : FigurePick)(implicit game : Game) =
        p.any.?((owner == Lynx && p.companions == 1 && maxCompanions == 2).?(named(p, RetreatOneLynxAction(self, from, to, p.units, p.chief, rough, then))).|(RetreatPartyAction(self, from, to, p.units, p.companions > 0, p.chief, rough, then)))
    override def cancel(a : Ask)(implicit game : Game) = MapExpansion.retreatCount(a, owner, from, rough)
}

// Brand's Bravery: another clan picks who retreats where
case class BrandRetreatGoal(self : Faction, owner : Faction, from : AreaRef, to : AreaRef, rough : Boolean, then : ForcedAction) extends FigureGoal {
    def elem(implicit game : Game) = "Brand's Bravery".hl ~ ": " ~ owner.elem ~ " retreats from " ~ from.elem ~ " to " ~ to.elem
    def start(implicit game : Game) = FigurePick(maxUnits, maxCompanions > 0, maxCompanions > 1, chiefMay)
    def finish(p : FigurePick)(implicit game : Game) =
        p.any.?((owner == Lynx && p.companions == 1 && maxCompanions == 2).?(named(p, BrandRetreatOneLynxAction(self, owner, from, to, p.units, p.chief, rough, then))).|(BrandRetreatPartyAction(self, owner, from, to, p.units, p.companions > 0, p.chief, rough, then)))
    override def cancel(a : Ask)(implicit game : Game) = MapExpansion.retreatCount(a, owner, from, rough)
}

// Hidden Ways: any figures from one territory to an open one (HiddenToAction)
case class HiddenGoal(self : Faction, from : AreaRef, to : AreaRef, then : ForcedAction) extends FigureGoal {
    def owner = self
    def elem(implicit game : Game) = "Hidden Ways".hl ~ ": move from " ~ from.elem ~ " to " ~ to.elem
    // Kaija can't enter enemy territories unless awakened (open territories hold none, but as before)
    override def maxCompanions(implicit game : Game) = (game.restrained(owner).not || game.present(game.board.territory(to)).forall(game.allied(owner, _))).??(super.maxCompanions)
    def start(implicit game : Game) = FigurePick(maxUnits, false, false, false)
    def finish(p : FigurePick)(implicit game : Game) =
        p.any.?((owner == Lynx && p.companions == 1 && maxCompanions == 2).?(named(p, HiddenOneLynxAction(self, from, to, p.units, p.chief, then))).|(HiddenUnitsAction(self, from, to, p.units, p.companions > 0, p.chief, then)))
}

// Lynx Clan's bonus Move: one or both lynxes, and up to `units` of the units just recruited there
case class LynxBonusGoal(self : Faction, from : AreaRef, to : AreaRef, units : Int, then : ForcedAction) extends FigureGoal {
    def owner = self
    def elem(implicit game : Game) = "Brundr and Kaelinn".hl ~ " bonus Move from " ~ from.elem ~ " to " ~ to.elem
    override def maxUnits(implicit game : Game) = units
    override def chiefMay(implicit game : Game) = false
    def start(implicit game : Game) = FigurePick(0, true, maxCompanions > 1, false)
    def finish(p : FigurePick)(implicit game : Game) = (p.companions > 0).?(named(p, LynxGoAction(self, from, to, p.companions, p.units, then)))
}

// Casualties picked by their player (ChooseCasualties): which k figures are lost. The units counter counts the units, then any
// Militia and bodyguard (they go after the units); a pick leads to the same CasualtyPickAction as before, except losing both
// lynxes while keeping some units, which the old picks couldn't say (CasualtyBothLynxAction)
case class CasualtyGoal(self : Faction, from : AreaRef, k : Int, then : ForcedAction) extends FigureGoal {
    def owner = self
    def elem(implicit game : Game) = self.elem ~ " takes " ~ CombatIcon.skulls(k) ~ " in " ~ from.elem ~ ", who is lost"
    // The bodyguard has its own switch, so it can be lost before the units (Marzaus, 2026-11-02: it comes back each year)
    // (with Horse's two, HorseTwoGuards, the switch is one of them, the warchief's first, and the other goes with the units)
    def guard(implicit game : Game) : Int = (MilitaryExpansion.guardsIn(territory, owner) > 0).??(1)
    override def guardMay(implicit game : Game) = guard > 0
    // the soldiers but the bodyguard and the second lynx (MilitaryExpansion.soldiers counts them last)
    override def maxUnits(implicit game : Game) = MilitaryExpansion.soldiers(territory, owner) - game.extraLynx(territory, owner) - guard
    override def blainnMay(implicit game : Game) = game.blainnIn(territory, owner)
    def start(implicit game : Game) = FigurePick(math.min(k, maxUnits), false, false, false, false, k > maxUnits && guardMay)
    override def cancel(a : Ask)(implicit game : Game) = a

    def taken(p : FigurePick)(implicit game : Game) = p.units + p.guard.??(1) + p.companions + p.blainn.??(1) + p.chief.??(game.chiefCost(owner))

    override def none(p : FigurePick)(implicit game : Game) =
        if (taken(p) == k && guardsKept(p))
            "The bodyguard goes first".txt
        else
            "Pick " ~ CombatIcon.skulls(k) ~ " (" ~ taken(p).hl ~ " picked)"

    // GuardBeforeChief: the warchief is lost only with its bodyguard, and Horse's Brok only with his (HorseTwoGuards), as the bodyguard
    // goes before them and is wherever they are (a warchief lost took its living bodyguard to the reserve). The bodyguard switch is the
    // warchief's when it is there, else Brok's; Brok's is otherwise the last of the units counter
    def guardsKept(p : FigurePick)(implicit game : Game) : Boolean = game.options.has(GuardBeforeChief) && {
        val chiefGuard = MilitaryExpansion.guardIn(territory, owner)
        val brokGuard = MilitaryExpansion.brokGuardIn(territory, owner)
        val chiefGuardLost = chiefGuard.not || p.guard
        val brokGuardLost = brokGuard.not || chiefGuard.?(p.guard && p.units == maxUnits).|(p.guard)
        (p.chief && chiefGuardLost.not) || (owner == Horse && p.first && brokGuardLost.not)
    }

    def finish(p : FigurePick)(implicit game : Game) = {
        val parties = MapExpansion.casualtyParties(owner, territory, k)
        val max = maxUnits
        // the recorded n of CasualtyPickAction counts the units, the Militia, then the bodyguard, then the second lynx
        val n = p.units + p.guard.??(1)
        // everything before the second lynx in that order is lost, so n can go on to it
        val all = p.units == max && p.guard == guardMay
        def old(n : Int, kaija : Boolean) = parties.has((n, kaija, p.chief, p.blainn)).?(CasualtyPickAction(owner, from, k, n, kaija, p.chief, p.blainn, then) : UserAction)
        // the bodyguard and fewer than all the units, which CasualtyPickAction can't say (it takes the bodyguard after every unit)
        def first(kaija : Boolean, both : Boolean) = (taken(p) == k).?(CasualtyGuardAction(owner, from, k, p.units, kaija, both, p.chief, p.blainn, then) : UserAction)

        if (guardsKept(p))
            None
        else
        if (p.guard && p.units < max)
            first(p.companions > 0, p.companions == 2)./(a => (p.companions == 1).?(named(p, a)).|(a))
        else
        p.companions match {
            case 2 if all.not => (taken(p) == k).?(CasualtyBothLynxAction(owner, from, k, n, p.chief, p.blainn, then))
            case 2 => old(n + 1, true)
            case 1 => old(n, true).orElse(all.??(old(n + 1, false)))./(named(p, _))
            case _ => old(n, false)
        }
    }
}
