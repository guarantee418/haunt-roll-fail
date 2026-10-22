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


// The Military paths module (option ModuleOption(MilitaryModule), under Fan content), fan-made by Marzaus (Discord,
// northgard-fanmade-content, 2026-10-22; asked for by Gman the same day): military experience (MXP) from fights, and at
// 5 MXP a path, Leadership, Guardian or Legions, with three levels (5, 50 and 100 MXP).
// RULES.md (Military paths) lists the choices made where the post is short

trait MilitaryPath extends NamedToString with Elementary with Record {
    override def elem : Elem = name.hl
    // What each level gives
    def levels : $[$[String]]
}

case object Leadership extends MilitaryPath {
    def levels = $(
        $("At the end of the year, 1 MXP for each unit in your warchief's territory (not the warchief or the bodyguard).",
            "Your warchief has a bodyguard: a warrior that goes wherever the warchief goes. It isn't one of your units and costs no Winter food; dead, it comes back at the end of the year."),
        $("1 reroll of the combat die in each fight with your warchief.",
            "With a Move card, your warchief may move once more across a regular border.",
            "Your warchief gains 1 combat point."),
        $("Your warchief works as a Training Camp: 1 more unit when you recruit in its territory.",
            "Your warchief captures: after a fight it wins, each enemy unit killed there is replaced by one of yours from your supply.",
            "Attacking a clan, your warchief kills 1 enemy unit before the fight."))
}

case object Guardian extends MilitaryPath {
    def levels = $(
        $("Defense Towers cost no wood and need no building space.",
            "Your Defense Towers kill before the fight: each kills an attacking unit before the dice; when there is nothing to kill, it adds its casualty at the end as usual.",
            "At the end of the year, 3 MXP for each Defense Tower you control."),
        $("Militia: 4 more warriors, placed in your territories. They fight and hold territories like units, aren't units for the supply or Winter, and only move between your territories. At the end of each year, 1 dead Militia comes back."),
        $("Defense Towers also work as Training Camps.",
            "Once a year, a free Fortress (no wood) when you build one."))
}

case object Legions extends MilitaryPath {
    def levels = $(
        $("Training Camps cost no wood and need no building space.",
            "Each Training Camp recruits 2 more units instead of 1.",
            "At the end of the year, 3 MXP for each Training Camp you control."),
        $("At the end of the year, each of your Training Camps places 1 unit from your supply in its territory."),
        $("1 more combat point for every 2 of your units in a fight.",
            "1 reroll of the combat die in each fight."))
}

object MilitaryPath {
    val all : $[MilitaryPath] = $(Leadership, Guardian, Legions)
}

// Guardian path, level 3: a small building that takes no space, 2 to a territory; not in Building.all, so games without the
// module never draw or offer it

// "12 MXP"
case class MXP(n : Int) extends Elementary {
    def elem : Elem = n.hl ~ " MXP"
}

// A clan's MXP, level and path, as in its panel
case class MilitaryLabel(f : Faction) extends GameElementary {
    def elem(implicit game : Game) = {
        val l = MilitaryExpansion.level(f)
        MXP(MilitaryExpansion.mxp(f)).elem ~ game.paths.get(f)./(p => ", " ~ p.elem ~ " " ~ l.hl).|(Empty)
    }
}

// Reaching 5 MXP: the path for the rest of the game
case class MilitaryPathAction(self : Faction, path : MilitaryPath, then : ForcedAction) extends BaseAction(self, "reached level 1: choose a military path")(MilitaryPathText(path))

case class MilitaryPathText(p : MilitaryPath) extends Elementary {
    def elem : Elem = (p.elem.div ~ p.levels.head./(s => CombatText(s).spn(xstyles.smaller85).div).merge).div
}

// After a fight, a Move, the year end: paths to choose and Militia to place
case class MilitaryCheckAction(then : ForcedAction) extends ForcedAction

// Guardian path, level 2: the Militia, one by one
case class MilitiaAskAction(f : Faction, left : Int, then : ForcedAction) extends ForcedAction
case class MilitiaPlaceAction(self : Faction, area : AreaRef, left : Int, then : ForcedAction) extends BaseAction("Place a", "Militia".hl, "warrior", "(" ~ left.hl ~ " left)", "in")(area) with MapTarget { def target = area }

// Militia moving with a Move card, only into the clan's own territories: how many first (then the rest of the party)
case class MilitiaPickAction(self : Faction, from : AreaRef, to : AreaRef, m : Int, cost : Int, left : Int, e : MoveEffect, then : ForcedAction) extends BaseAction("Move from", from, "to", to)((m == 0).?("No Militia".txt).|(m.hl ~ " Militia")) with Soft
case class MilitiaMoveAction(self : Faction, from : AreaRef, to : AreaRef, n : Int, kaija : Boolean, chief : Boolean, m : Int, cost : Int, left : Int, e : MoveEffect, then : ForcedAction) extends BaseAction("Move from", from, "to", to)(MilitiaParty(self, n, kaija, chief, m))

case class MilitiaParty(f : Faction, n : Int, kaija : Boolean, chief : Boolean, m : Int) extends GameElementary {
    def elem(implicit game : Game) = (((n > 0 || kaija || chief).$(Party(f, n, kaija, chief).elem) ++ (m > 0).$(m.hl ~ " Militia")) : $[Elem]).join(" and ")
}

// Militia retreating: only into the clan's own territories
case class MilitiaRetreatAction(self : Faction, from : AreaRef, to : AreaRef, rough : Boolean, then : ForcedAction) extends BaseAction("Militia".hl, "retreat from", from, "to")(to) with MapTarget { def target = to }

// Leadership path, level 2: the warchief's extra move after the card's moves
case class ChiefMoveAction(self : Faction, from : AreaRef, to : AreaRef, then : ForcedAction) extends BaseAction("Leadership".hl, "move", WarchiefElem(self), "once more, from", from, "to")(to) with MapTarget { def target = to }
case class ChiefMoveSkipAction(self : Faction, then : ForcedAction) extends BaseAction("Leadership".hl)("Don't move the warchief again")

// Leadership 2 and Legions 3: the reroll, like Liv's
case class MilitaryRerollInfoAction(self : Faction, keep : ForcedAction) extends BaseInfo((g : Game) => MilitaryExpansion.rerollSource(self)(g))(RerollInfo(keep))
case class MilitaryRerollAction(self : Faction, face : DieFace, again : ForcedAction) extends BaseAction((g : Game) => MilitaryExpansion.rerollSource(self)(g))("Reroll the combat die")
case class MilitaryKeepAction(self : Faction, keep : ForcedAction) extends BaseAction((g : Game) => MilitaryExpansion.rerollSource(self)(g))("Keep the result")

// The die kept: on as CombatRolledAction would (Liv's and Ox's rerolls still offered), or after a reroll (again) as CombatRerolledAction
case class MilitaryKeptAction(attacker : Faction, defender : Faction, area : AreaRef, e : MoveEffect, food : $[Int], faces : $[DieFace], face : DieFace, again : Boolean, then : ForcedAction) extends ForcedAction


// The panel's line (tapped: MilitaryView)
case class MilitaryView(f : Faction)


object MilitaryExpansion extends Expansion {
    // MXP for each level
    val thresholds = $(5, 50, 100)

    def on(implicit game : Game) = game.has(MilitaryModule)

    def mxp(f : Faction)(implicit game : Game) : Int = game.mxp.getOrElse(f, 0)

    def level(f : Faction)(implicit game : Game) : Int = thresholds.count(_ <= mxp(f))

    // f is on path p at level n or higher
    def at(f : Faction, p : MilitaryPath, n : Int)(implicit game : Game) : Boolean = game.paths.get(f).has(p) && level(f) >= n

    // The paths f may choose: Leadership only with warchiefs
    def choices(f : Faction)(implicit game : Game) : $[MilitaryPath] = MilitaryPath.all.%(p => p != Leadership || game.has(Warchiefs))

    def gain(f : Faction, n : Int, why : Any*)(implicit game : Game) {
        if (on && f != Automa && n > 0 && game.states.contains(f)) {
            val before = level(f)
            val k = n * Debug.mxpTimes
            game.mxp += f -> (mxp(f) + k)

            f.log(("gained" +: MXP(k) +: why.toList) : _*)

            game.note("mxp")

            if (level(f) > before && game.paths.contains(f)) {
                f.log("reached level", level(f).hl, "of the", game.paths(f), "path")
                game.note("level-" + game.paths(f).name.toLowerCase + "-" + level(f))
            }
        }
    }

    def militiaIn(t : Territory, f : Faction)(implicit game : Game) : Int = t.areas./(a => game.militia.getOrElse(a, Map()).getOrElse(f, 0)).sum

    def militiaOnMap(f : Faction)(implicit game : Game) : Int = game.militia.values./(_.getOrElse(f, 0)).sum

    def guardIn(t : Territory, f : Faction)(implicit game : Game) : Boolean = game.guards.has(f) && game.chiefIn(t, f)

    // The Militia and the bodyguard: figures beside the units, the warchief and the companions
    def figures(t : Territory, f : Faction)(implicit game : Game) : Int = militiaIn(t, f) + guardIn(t, f).??(1)

    // What can be lost as a unit: the units, then the Militia, then the bodyguard
    def soldiers(t : Territory, f : Faction)(implicit game : Game) : Int = game.count(t, f) + figures(t, f)

    def addMilitia(a : AreaRef, f : Faction, n : Int)(implicit game : Game) {
        val m = game.militia.getOrElse(a, Map())
        val v = m.getOrElse(f, 0) + n
        game.militia += a -> ((v > 0).?(m + (f -> v)).|(m - f))
        if (game.militia(a).isEmpty)
            game.militia -= a
    }

    def removeMilitia(t : Territory, f : Faction, n : Int)(implicit game : Game) {
        var left = n
        t.areas.foreach { a =>
            val k = math.min(left, militiaIn(Territory($(a)), f))
            if (k > 0) {
                addMilitia(a, f, -k)
                left -= k
            }
        }
    }

    // Casualties past the units: Militia first, then the bodyguard; how many were taken
    def removeExtra(t : Territory, f : Faction, n : Int)(implicit game : Game) : Int = {
        val m = math.min(n, militiaIn(t, f))
        removeMilitia(t, f, m)
        val g = (n > m && guardIn(t, f)).??(1)
        if (g > 0)
            game.guards = game.guards.but(f)
        m + g
    }

    // Soldiers killed by a card or a building (units first)
    def kill(t : Territory, f : Faction, n : Int)(implicit game : Game) : Int = {
        val k = math.min(n, game.count(t, f))
        game.removeUnits(t, f, k)
        k + removeExtra(t, f, n - k)
    }

    // Combat points beside the units, the warchief and the companions (they add up to game.strength)
    def parts(t : Territory, f : Faction, attacking : Boolean)(implicit game : Game) : $[(Int, Elem)] = on.??($(
        militiaIn(t, f) -> "Militia".hl,
        guardIn(t, f).??(1) -> "the bodyguard".txt,
        (at(f, Leadership, 2) && game.chiefIn(t, f)).??(1) -> "Leadership".hl,
        at(f, Legions, 3).??(game.count(t, f) / 2) -> "Legions".hl
    ).filter(_._1 > 0))

    def strength(t : Territory, f : Faction, attacking : Boolean)(implicit game : Game) : Int = parts(t, f, attacking).map(_._1).sum

    // More units when recruiting in t: Legions' Training Camps recruit two, Guardian's Defense Towers and Leadership's warchief one
    def camps(f : Faction, t : Territory)(implicit game : Game) : Int = on.??(
        at(f, Legions, 1).??(game.working(t).count(_ == TrainingCamp)) +
        at(f, Guardian, 3).??(game.working(t).count(_ == DefenseTower)) +
        (at(f, Leadership, 3) && game.chiefIn(t, f)).??(1))

    // The buildings a path builds with no space
    val buildings : $[Building] = $(DefenseTower, TrainingCamp)

    // Buildings f builds for no wood and with no space (whatever they cost otherwise, as with the Tower PC version): Guardian's Defense Towers, Legions' Training Camps
    def freeBuild(f : Faction, b : Building)(implicit game : Game) : Boolean = on && (b == DefenseTower && at(f, Guardian, 1) || b == TrainingCamp && at(f, Legions, 1))

    def cost(f : Faction, b : Building, base : Int)(implicit game : Game) : Int = (freeBuild(f, b) || b == Fortress && freeFortress(f)).?(0).|(base)

    // Guardian 3: once a year, a Fortress for no wood (Marzaus's Palisade is the Fortress, 2026-10-22); the first Fortress built that year uses it
    def freeFortress(f : Faction)(implicit game : Game) : Boolean = on && at(f, Guardian, 3) && game.fortressYear.get(f).has(game.year).not

    def built(f : Faction, b : Building)(implicit game : Game) {
        if (b == Fortress && freeFortress(f)) {
            game.fortressYear += f -> game.year
            f.log("built it for free", "(" ~ "Guardian".hl ~ ")")
            game.note("guardian-fortress")
        }
    }

    // A territory's next space for a building that takes none
    def extraSpace(t : Territory)(implicit game : Game) : SpaceRef = SpaceRef(t.anchor, SpaceRef.extra + game.buildings.keys.count(s => s.area == t.anchor && s.index >= SpaceRef.extra))


    // Militia move only into the clan's own territories; other figures may go anywhere they could
    def mayEnter(f : Faction, from : Territory, to : Territory)(implicit game : Game) : Boolean =
        militiaIn(from, f) == 0 || game.count(from, f) + game.kaijaIn(from, f).??(1) + game.chiefIn(from, f).??(1) > 0 || game.controlled(f).has(to)

    // Leadership 2: the reroll is in a fight with the warchief; Legions 3: in every fight
    def reroll(t : Territory, f : Faction)(implicit game : Game) : Boolean =
        on && game.rerolled.has(f).not && (at(f, Leadership, 2) && game.chiefIn(t, f) || at(f, Legions, 3))

    def rerollSource(f : Faction)(implicit game : Game) : Elem = game.paths.get(f)./(_.elem).|(Empty)

    // Destroying a Defense Tower in another clan's territory gives 3 MXP
    def razed(f : Faction, s : SpaceRef, b : Building)(implicit game : Game) {
        if (b == DefenseTower && game.controlled(f).has(game.board.territory(s.area)).not)
            gain(f, 3, "for destroying a", DefenseTower)
    }

    // A creature defeated: twice its fame
    def defeated(f : Faction, c : Creature)(implicit game : Game) {
        gain(f, 2 * c.kind.fame, "for defeating", c)
    }

    // Each side's soldiers and warchief before the casualties are taken
    def before(t : Territory, l : $[Faction])(implicit game : Game) : Map[Faction, (Int, Boolean)] = l./(f => f -> (soldiers(t, f), game.chiefIn(t, f))).toMap

    // After a fight: each side, the loser too (Marzaus, 2026-10-22), gains 2 MXP for each enemy unit it killed (Militia and
    // the bodyguard too, and those killed before the fight) and 10 for the enemy warchief; Leadership 3's winner captures
    // the killed units
    def afterCombat(attacker : Faction, defender : Faction, t : Territory, before : Map[Faction, (Int, Boolean)], winner : |[Faction])(implicit game : Game) {
        if (on)
            $(attacker -> defender, defender -> attacker).foreach { case (w, l) =>
                val (n, chief) = before(l)
                val units = math.max(0, n - soldiers(t, l)) + game.fightKills.getOrElse(w, 0)
                val chiefKilled = chief && game.chiefIn(t, l).not

                if (units + chiefKilled.??(1) > 0)
                    gain(w, 2 * units + chiefKilled.??(10), "for killing", ((units > 0).$((units == 1).?("1 unit").|(units.toString + " units").txt) ++ chiefKilled.$(Warchief.elem(l))).join(" and "), "of", l)

                if (winner.has(w))
                    capture(w, t, units)
            }
    }

    // Leadership 3: the warchief that won captures the killed enemy units, as far as the supply goes
    def capture(w : Faction, t : Territory, killed : Int)(implicit game : Game) {
        if (at(w, Leadership, 3) && game.chiefIn(t, w)) {
            val n = math.min(killed, game.reserve(w))

            if (n > 0) {
                game.addUnits(t.anchor, w, n)
                w.log("captured", n.hl, (n == 1).?("unit").|("units"), "with", Warchief.elem(w), "in", t.anchor)
                game.note("capture")
            }
        }
    }

    // A fight is over
    def fightOver()(implicit game : Game) {
        game.fightStarted = false
        game.towersUsed = 0
        game.fightKills = Map()
        game.rerolled = $
    }

    // Choices still to make: a path at level 1, the Militia at Guardian 2, a dead Militia back at the end of the year
    def pending(implicit game : Game) : Boolean = on && game.factions.exists(f => f != Automa && (pathDue(f) || militiaDue(f) || game.militiaBack.has(f)))

    def pathDue(f : Faction)(implicit game : Game) = level(f) >= 1 && game.paths.contains(f).not

    def militiaDue(f : Faction)(implicit game : Game) = at(f, Guardian, 2) && game.militiaGiven.has(f).not

    def perform(action : Action, soft : Void)(implicit game : Game) : Continue = action @@ {
        case FightOverAction(then) if pending =>
            Then(MilitaryCheckAction(FightOverAction(then)))

        // THE END OF THE YEAR (after Winter): MXP from units, towers and camps, Legions' new units, the bodyguards back
        case EndOfYearAction if on && game.militaryYear != game.year =>
            game.militaryYear = game.year

            game.from(game.first).%(game.paths.contains).foreach { f =>
                if (at(f, Leadership, 1))
                    game.chiefs.get(f)./(game.board.territory).foreach { t =>
                        gain(f, game.count(t, f) + militiaIn(t, f), "for the units with", Warchief.elem(f))
                    }

                if (at(f, Guardian, 1))
                    gain(f, 3 * game.controlled(f)./~(game.working).count(_ == DefenseTower), "for their", DefenseTower.elem ~ "s")

                if (at(f, Legions, 1))
                    gain(f, 3 * game.controlled(f)./~(game.working).count(_ == TrainingCamp), "for their", TrainingCamp.elem ~ "s")

                if (at(f, Legions, 2))
                    game.controlled(f).foreach { t =>
                        val n = math.min(game.working(t).count(_ == TrainingCamp), game.reserve(f))
                        if (n > 0) {
                            game.addUnits(t.anchor, f, n)
                            f.log("placed", n.hl, (n == 1).?("unit").|("units"), "with", TrainingCamp, "in", t.anchor, "(" ~ "Legions".hl ~ ")")
                            game.note("legions-unit")
                        }
                    }

                // Guardian 2: one dead Militia comes back each year (Marzaus, 2026-10-22)
                if (at(f, Guardian, 2) && game.militiaGiven.has(f) && militiaOnMap(f) < 4 && game.controlled(f).any)
                    game.militiaBack :+= f

                if (at(f, Leadership, 1) && game.guards.has(f).not) {
                    game.guards :+= f
                    f.log("got the bodyguard of", Warchief.elem(f), "back")
                    game.note("guard-back")
                }
            }

            Then(MilitaryCheckAction(EndOfYearAction))

        case MilitaryCheckAction(then) =>
            val l = game.from(game.first).but(Automa)

            l.find(pathDue) match {
                case Some(f) =>
                    Ask(f).each(choices(f))(p => MilitaryPathAction(f, p, MilitaryCheckAction(then)))

                case None =>
                    l.find(militiaDue) match {
                        case Some(f) =>
                            game.militiaGiven :+= f
                            Then(MilitiaAskAction(f, 4, MilitaryCheckAction(then)))
                        case None =>
                            l.find(game.militiaBack.has) match {
                                case Some(f) =>
                                    game.militiaBack = game.militiaBack.but(f)
                                    f.log("got a", "Militia".hl, "warrior back")
                                    game.note("militia-back")
                                    Then(MilitiaAskAction(f, 1, MilitaryCheckAction(then)))
                                case None =>
                                    Then(then)
                            }
                    }
            }

        case MilitaryPathAction(f, p, then) =>
            game.paths += f -> p
            if (p == Leadership)
                game.guards :+= f

            f.log("chose the", p, "path")

            if (level(f) > 1)
                f.log("is at level", level(f).hl, "of it")

            game.note("path-" + p.name.toLowerCase)

            Then(then)

        case MilitiaAskAction(f, left, then) =>
            val l = game.controlled(f)

            if (left <= 0 || l.none)
                Then(then)
            else
                Ask(f).each(l)(t => MilitiaPlaceAction(f, t.anchor, left, then))

        case MilitiaPlaceAction(f, a, left, then) =>
            addMilitia(a, f, 1)

            f.log("placed a", "Militia".hl, "warrior in", a)

            game.note("militia-place")

            Then(MilitiaAskAction(f, left - 1, then))

        // MOVE: Militia going into one of the clan's territories: how many, then who goes with them
        case MoveToAction(f, from, to, cost, left, e, then) if on && militiaIn(game.board.territory(from), f) > 0 && game.controlled(f).has(game.board.territory(to)) && game.passOnly(f, game.board.territory(from)).not =>
            val m = militiaIn(game.board.territory(from), f)

            val t = game.board.territory(from)
            // Without other figures, some Militia must go
            val others = game.count(t, f) > 0 || game.kaijaIn(t, f) || game.chiefIn(t, f)

            Ask(f).each(m.to(others.?(0).|(1), -1).$)(k => MilitiaPickAction(f, from, to, k, cost, left, e, then)).cancel

        case MilitiaPickAction(f, from, to, m, cost, left, e, then) =>
            val t = game.board.territory(from)
            val n = game.count(t, f)
            val kaija = game.kaijaIn(t, f)
            val chief = game.chiefIn(t, f)
            // With Militia, the units may stay behind
            val ns = (m > 0).?(n.to(0, -1).$).|(n.to(1, -1).$)

            Ask(f)
                .each(ns)(k => MilitiaMoveAction(f, from, to, k, false, false, m, cost, left, e, then))
                .some(kaija.$(n.to(0, -1).$).flatten)(k => $(MilitiaMoveAction(f, from, to, k, true, false, m, cost, left, e, then)))
                .some(chief.$(n.to(0, -1).$).flatten)(k => $(MilitiaMoveAction(f, from, to, k, false, true, m, cost, left, e, then)))
                .some((chief && kaija).$(n.to(0, -1).$).flatten)(k => $(MilitiaMoveAction(f, from, to, k, true, true, m, cost, left, e, then)))
                .cancel

        case MilitiaMoveAction(f, from, to, n, kaija, chief, m, cost, left, e, then) =>
            val src = game.board.territory(from)

            if (m > 0) {
                removeMilitia(src, f, m)
                addMilitia(game.board.territory(to).anchor, f, m)

                f.log("moved", m.hl, "Militia".hl, "from", from, "to", to)
                game.note("militia-move")
            }

            if (n > 0 || kaija || chief)
                Then(MoveUnitsAction(f, from, to, n, kaija, chief, cost, left, e, then))
            else
                Then(MoveAction(f, left - cost, e, then))

        // Leadership 2: after the card's moves, the warchief may move once more
        case MovesMadeAction(f, e, then) if on && game.extraMoved.not && at(f, Leadership, 2) && game.chiefs.contains(f) =>
            game.extraMoved = true

            val from = game.board.territory(game.chiefs(f))
            val l = (game.passOnly(f, from).not && game.bearIn(from).not).??(game.board.adjacent(from).filter(_._2).map(_._1)
                .%(o => MapExpansion.canEnter(f, o, 0, e, false)).%(o => game.passOnly(f, o).not && game.swampIn(o).not && game.hostileIn(o).not && game.bearIn(o).not))

            if (l.none)
                Then(MovesMadeAction(f, e, then))
            else
                Ask(f).each(l)(o => ChiefMoveAction(f, from.anchor, o.anchor, MovesMadeAction(f, e, then))).add(ChiefMoveSkipAction(f, MovesMadeAction(f, e, then)))

        case ChiefMoveAction(f, from, to, then) =>
            val dst = game.board.territory(to)

            game.chiefs += f -> dst.anchor

            val enemy = game.present(dst).%(game.enemy(f, _))

            f.log("moved", Warchief.elem(f), "from", from, "to", to, "with", "Leadership".hl, enemy.any.?("and attacked " ~ enemy./(_.elem).join(", ")).|(Empty))

            if (enemy.any && game.combats.has(dst.anchor).not)
                game.combats :+= dst.anchor

            game.note("chief-extra-move")

            Then(then)

        case ChiefMoveSkipAction(f, then) =>
            Then(then)

        // BEFORE THE FIGHT: Leadership 3's warchief attacking, then Guardian's Defense Towers, kill
        case CombatFoodStartAction(f, defender, a, e, then) if on && game.fightStarted.not =>
            game.fightStarted = true

            val t = game.board.territory(a)

            if (at(f, Leadership, 3) && game.chiefIn(t, f) && soldiers(t, defender) > 0) {
                kill(t, defender, 1)
                game.fightKills += f -> 1
                f.log(Warchief.elem(f), "killed a unit of", defender, "before the fight", "(" ~ "Leadership".hl ~ ")")
                game.note("leadership-kill")
            }

            // With the Tower PC version every tower already fired before the fight (TowerVolleyAction)
            val towers = (at(defender, Guardian, 1) && TowersExpansion.killFirst.not && f.conqueror.not && e.special != ShieldMove && e.special != BorgildMove).??(game.working(t).count(_ == DefenseTower))
            val killed = (game.figures(t, defender) > 0).??(kill(t, f, math.min(towers, soldiers(t, f))))

            if (killed > 0) {
                game.towersUsed = killed
                game.fightKills += defender -> killed
                defender.log("killed", killed.hl, (killed == 1).?("unit").|("units"), "of", f, "with", DefenseTower.elem ~ (towers > 1).??("s"), "before the fight", "(" ~ "Guardian".hl ~ ")")
                game.note("guardian-kill")
            }

            // The warchief killed the last defender: the attacker wins without rolling, as with Intimidate
            if (game.figures(t, defender) == 0) {
                f.log("won the fight in", a, "before it started")
                EventsExpansion.afterCombat(f, defender, killed, game.fightKills.getOrElse(f, 0), |(f))
                game.note("attack-won")
                MapExpansion.attackWon(f, a)
                afterCombat(f, defender, t, Map(defender -> (0, false), f -> (0, false)), |(f))

                Then(ClanPowers.report(then))
            }
            else
            // The towers killed every attacker
            if (game.figures(t, f) == 0) {
                defender.log("won the fight in", a, "before it started")
                EventsExpansion.afterCombat(f, defender, killed, game.fightKills.getOrElse(f, 0), |(defender))
                game.note("attack-lost")
                afterCombat(f, defender, t, Map(defender -> (0, false), f -> (0, false)), |(defender))

                Then(ClanPowers.report(then))
            }
            else
                Then(CombatFoodStartAction(f, defender, a, e, then))

        // THE DIE: Leadership 2 (with the warchief) and Legions 3 reroll once per fight
        case CombatRolledAction(attacker, defender, a, e, food, faces, face, then) if reroll(game.board.territory(a), (faces.num == 0).?(attacker).|(defender)) =>
            val self = (faces.num == 0).?(attacker).|(defender)
            val keep = MilitaryKeptAction(attacker, defender, a, e, food, faces, face, false, then)

            Ask(self)
                .add(MilitaryRerollInfoAction(self, CombatFaceAction(attacker, defender, a, e, food, faces, face, then)))
                .add(MilitaryRerollAction(self, face, CombatRerollAction(attacker, defender, a, e, food, faces, then)))
                .add(MilitaryKeepAction(self, keep))

        // After Liv's or Ancestral Equipment's reroll, the path's
        case CombatRerolledAction(attacker, defender, a, e, food, faces, face, then) if reroll(game.board.territory(a), (faces.num == 0).?(attacker).|(defender)) =>
            val self = (faces.num == 0).?(attacker).|(defender)
            val keep = MilitaryKeptAction(attacker, defender, a, e, food, faces, face, true, then)

            Ask(self)
                .add(MilitaryRerollInfoAction(self, CombatFaceAction(attacker, defender, a, e, food, faces, face, then)))
                .add(MilitaryRerollAction(self, face, CombatRerollAction(attacker, defender, a, e, food, faces, then)))
                .add(MilitaryKeepAction(self, keep))

        case MilitaryRerollAction(f, face, again) =>
            game.rerolled :+= f

            f.log("rolled", face, "and rerolled with", rerollSource(f))

            game.note("military-reroll")

            Then(again)

        case MilitaryKeepAction(f, keep) =>
            game.rerolled :+= f

            Then(keep)

        // The same as CombatRolledAction and CombatRerolledAction (a RandomAction can't be performed again with Then)
        case MilitaryKeptAction(attacker, defender, a, e, food, faces, face, again, then) =>
            val self = (faces.num == 0).?(attacker).|(defender)
            val next = CombatFaceAction(attacker, defender, a, e, food, faces, face, then)

            self.log("rolled", face)

            if (again.not && Warchief.reroll(game.board.territory(a), self))
                Ask(self)
                    .add(RerollInfoAction(self, true, next))
                    .add(LivRerollAction(self, CombatRerollAction(attacker, defender, a, e, food, faces, then)))
                    .add(LivKeepAction(self, next))
            else
            if (again.not && self == Ox && game.gearFight.has(3))
                Ask(self)
                    .add(RerollInfoAction(self, false, next))
                    .add(GearRerollAction(self, CombatRerollAction(attacker, defender, a, e, food, faces, then)))
                    .add(GearKeepAction(self, next))
            else
                Then(next)

        // RETREAT: the Militia go only into the clan's own territories, or are lost
        case RetreatAction(f, a, rough, then) if on && militiaIn(game.board.territory(a), f) > 0 =>
            val t = game.board.territory(a)
            val to = MapExpansion.retreats(f, t, rough).%(o => game.controlled(f).has(o))

            if (to.none) {
                val n = militiaIn(t, f)
                removeMilitia(t, f, n)
                f.log("had nowhere to retreat", n.hl, "Militia".hl, "and lost them")
                game.note("militia-lost")

                Then(RetreatAction(f, a, rough, then))
            }
            else
                Ask(f).each(to)(o => MilitiaRetreatAction(f, a, o.anchor, rough, then))

        case MilitiaRetreatAction(f, from, to, rough, then) =>
            val t = game.board.territory(from)
            val n = militiaIn(t, f)

            removeMilitia(t, f, n)
            addMilitia(to, f, n)

            f.log(n.hl, "Militia".hl, "retreated to", to)
            game.note("militia-retreat")

            Then(RetreatAction(f, from, rough, then))


        case _ => UnknownContinue
    }
}
