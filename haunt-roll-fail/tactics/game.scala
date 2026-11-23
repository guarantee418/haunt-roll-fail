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

import hrf.elem._
import tactics.elem._

// The HRF game around the engine (state.scala, turns.scala): the engine's GameState is the game, and every
// recorded action goes through Turns.applyAction, so replaying the actions rebuilds the same game.
// Attack dice are rolled by a Roll (a recorded random action) before the engine's AttackUnit gets them.

trait Faction extends BasePlayer with Named with Styling with Record {
    def name = toString
    def short = name.take(1)
    def id = name.toLowerCase
}

case object Red extends Faction
case object Blue extends Faction

object Faction {
    val all = $[Faction](Red, Blue)
}

// A hex in an action (the engine's Hex isn't a Record, so it can't be written to a saved game), named like C4
case class Cell(q : Int, r : Int) extends Record with Elementary {
    def hex = Hex(q, r)
    def name = Skirmish.name(hex)
    def elem = name.styled(styles.hex)
}

object Cell {
    def apply(h : Hex) : Cell = Cell(h.q, h.r)
}

// choices about a hex (tapped on the map) or a unit (its token tapped)
trait HexKey { self : UserAction =>
    def cell : Cell
}

trait UnitKey { self : UserAction =>
    def unit : String
}


case class StartAction(version : String) extends StartGameAction with GameVersion
case class TurnAction() extends ForcedAction
case class PlayerTurnAction(f : Faction) extends ForcedAction

// Income phase
case class DeployAction(self : Faction, unitType : String, cell : Cell) extends BaseAction("Deploy")((g : Game) => g.typeElem(unitType), "in", cell, "for", (g : Game) => g.costElem(unitType)) with HexKey
case class ToActionPhaseAction(self : Faction) extends BaseAction(None)("Go to the", "Action".hl, "phase")

// Action phase
case class UnitMenuAction(self : Faction, unit : String) extends BaseAction("Units")((g : Game) => g.unitElem(unit), "in", (g : Game) => g.cellOf(unit), (g : Game) => g.unitNote(unit)) with Soft with UnitKey
case class MoveAction(self : Faction, unit : String, cell : Cell) extends BaseAction((g : Game) => g.unitElem(unit), "in", (g : Game) => g.cellOf(unit))("Move to", cell, (g : Game) => g.moveNote(unit, cell)) with HexKey
case class AttackAction(self : Faction, attacker : String, target : String) extends BaseAction((g : Game) => g.unitElem(attacker), "in", (g : Game) => g.cellOf(attacker))("Attack", (g : Game) => g.unitElem(target), "in", (g : Game) => g.cellOf(target), (g : Game) => g.attackNote(attacker, target)) with UnitKey { def unit = target }
case class AttackRolledAction(self : Faction, attacker : String, target : String, rolled : $[Int]) extends RolledAction[Int]
case class EndActionsAction(self : Faction) extends BaseAction(None)("End the", "Action".hl, "phase")

// End phase
case class EndTurnAction(self : Faction) extends BaseAction(None)("End turn")

case class GameOverAction(winner : Faction) extends ForcedAction
case class GameOverWonAction(self : Faction, f : Faction) extends BaseInfo("Game Over")(f, "won")


class Game(val setup : $[Faction], val options : $[Meta.O]) extends BaseGame with ContinueGame with LoggedGame { game =>
    var isOver = false

    val factions : $[Faction] = setup

    // the engine's state: board, units, players' stock and VP, sectors, turn and phase
    var state : GameState = null

    // each deployed unit's type, for its name on the screen (the engine keeps only its numbers)
    var types = Map[String, String]()

    var highlightFaction = $[Faction]()

    def faction(p : String) : Faction = factions.find(_.name == p).get

    def current : |[Faction] = (state != null).??(state.current./(faction))

    // NAMES

    def unitType(id : String) : String = types.getOrElse(id, Roster.riflemen.name)

    def typeElem(name : String) = name.styled(styles.unitType)

    def costElem(name : String) : Elem = state.rules.unitType(name)./(t => t.cost.toString.hl).|(Empty)

    def unitElem(id : String) : Elem = state.unit(id) match {
        case Some(u) => (faction(u.owner).name + " " + unitType(id)).styled(faction(u.owner))
        case None => unitType(id).txt
    }

    def cellOf(id : String) : Elem = state.unit(id)./(u => Cell(u.hex).elem).|(Empty)

    def unitNote(id : String) : Elem = state.unit(id)./{ u =>
        ("(" + u.health + " health, " + u.movesLeft + " moves left" + u.suppressed.??(", suppressed") + u.attacked.??(", attacked") + ")").txt
    }.|(Empty)

    def moveNote(id : String, c : Cell) : Elem = {
        val cost = state.reachable(id).get(c.hex)
        val sector = Skirmish.sectorName(c.hex)
        (cost./(n => "(" + n + (n == 1).?(" move").|(" moves") + ")").|("") + sector./(s => " " + s).|("")).txt
    }

    def attackNote(attacker : String, target : String) : Elem = (for {
        a <- state.unit(attacker)
        t <- state.unit(target)
        w <- a.weapon
    } yield ("(" + w.dice + " dice, hit on " + Combat.needed(state, a, w, t) + "+, " + Combat.damagePerHit(w, t) + " damage a hit)").txt).|(Empty)

    // CHOICES

    // the hexes of the player's zone a unit of the type could be deployed on now
    def deploySites(p : String, t : UnitType) : $[Hex] =
        state.zones.getOrElse(p, Set()).toList.sortBy(h => (h.r, h.q)).%(h => state.occupied(h).not && state.board.get(h).exists(_.terrain.passable(t.kind)))

    def deployActions(f : Faction) : $[UserAction] =
        state.rules.roster.%(t => state.player(f.name).resources.covers(t.cost))./~(t => deploySites(f.name, t)./(h => DeployAction(f, t.name, Cell(h))))

    // a unit that has attacked can't move any more
    def moves(id : String) : $[Hex] = state.unit(id).%(_.attacked.not)./~(_ => state.reachable(id).keys.toList.sortBy(h => (h.r, h.q)))

    def targets(id : String) : $[CombatUnit] = state.unit(id).%(_.attacked.not)./~(_ => Combat.targets(state, id))

    def unitActions(f : Faction, id : String) : $[UserAction] =
        moves(id)./(h => MoveAction(f, id, Cell(h))) ++ targets(id)./(t => AttackAction(f, id, t.id))

    def ready(f : Faction) : $[CombatUnit] = state.unitsOf(f.name).%(u => unitActions(f, u.id).any)

    // INFO

    def preinfo(waiting : $[Faction], self : |[Faction], actions : $[UserAction]) : $[Info] = $

    def info(waiting : $[Faction], self : |[Faction], actions : $[UserAction]) : $[Info] = $

    // ENGINE

    // every recorded action goes through the engine; a refused one is a bug in the choices offered
    def apply(a : TacticsAction) {
        state.applyAction(a) match {
            case Right(s) => state = s
            case Left(e) => throw new Error("refused " + a + ": " + e)
        }
    }

    def loggedPerform(action : Action, soft : Void) : Continue = {
        val c = performInternal(action, soft)

        highlightFaction = c match {
            case Ask(f, _) => $(f)
            case MultiAsk(a, _) => a./(_.faction)
            case _ => $
        }

        c
    }

    // the attack's log; `target` is the target's name, worked out before the attack, as it may be destroyed
    def report(r : CombatReport, target : Elem) {
        val hexes = (r.distance == 1).?("1 hex").|(r.distance + " hexes")
        val cover = (r.cover == NoCover).?("no cover").|(r.cover.name.toLowerCase + " cover")
        log(unitElem(r.attacker), "fired", r.weapon.hl, "at", target, "(" + hexes + ", " + cover + "):",
            "rolled", r.rolls.mkString(" ").hl, "needing", (r.needed + "+").hl)

        val hits = r.hits.hl ~ " " ~ (r.hits == 1).?("hit").|("hits").txt

        if (r.hits == 0)
            log("No hits")
        else
        if (r.destroyed)
            log(hits, "for", r.damage.hl, "damage,", target, "destroyed")
        else
            log(hits, "for", r.damage.hl, "damage,", target, "has", r.health.hl, "health left", r.suppressed.?("and is suppressed"))
    }

    def performInternal(a : Action, soft : Void) : Continue = {
        implicit val action = a

        action match {
            case StartAction(version) =>
                log("HRF".hl, "version", gaming.version.hlb)
                log("Hex Tactics".hlb, "(working name)")

                if (version != gaming.version)
                    log("Saved game version", version.hlb)

                log("Players", setup./(_.elem).comma)

                state = Skirmish.start(setup(0).name, setup(1).name) match {
                    case Right(s) => s
                    case Left(e) => throw new Error(e)
                }

                state.units.values.toList.sortBy(_.id).foreach { u =>
                    types += u.id -> Roster.riflemen.name
                    log(faction(u.owner), "starts with", typeElem(Roster.riflemen.name), "in", Cell(u.hex))
                }

                log(setup(0), "deploys in the west,", setup(1), "in the east;", state.rules.vpTarget.hl, "VP win")

                Milestone(PlayerTurnAction(setup(0)))

            // the start of a player's turn: the engine has already given them their income and VP
            case PlayerTurnAction(f) =>
                log(DoubleLine)
                log("Turn", ("#" + state.turn).hl, f)

                val p = state.player(f.name)
                val (gain, vp) = Turns.income(state, f.name)
                log(f, "got", gain.toString.hl, vp.>(0).??(" and " + vp + " VP"), "and has", p.resources.toString.hl, "and", p.vp.hl, "VP")

                TurnAction()

            // what the current player may do in this phase
            case TurnAction() =>
                state.winner match {
                    case Some(w) => GameOverAction(faction(w))
                    case None =>
                        val f = current.get

                        state.phase match {
                            case IncomePhase =>
                                Ask(f).add(deployActions(f)).add(ToActionPhaseAction(f))

                            case ActionPhase =>
                                Ask(f).each(ready(f))(u => UnitMenuAction(f, u.id)).add(EndActionsAction(f))

                            case EndPhase =>
                                Ask(f).add(EndTurnAction(f))
                        }
                }

            case DeployAction(f, t, c) =>
                val id = f.name + "-" + (state.deployed + 1)
                val held = state.control.get(c.hex)
                apply(DeployUnit(f.name, t, c.hex))
                types += id -> t
                log(f, "deployed", typeElem(t), "in", c, "for", state.rules.unitType(t).get.cost.toString.hl)
                if (held.has(f.name).not)
                    captured(f, c)
                TurnAction()

            case ToActionPhaseAction(f) =>
                apply(EndPhaseAction(f.name))
                TurnAction()

            case UnitMenuAction(f, id) =>
                Ask(f).add(unitActions(f, id)).cancel

            case MoveAction(f, id, c) =>
                val from = state.unit(id).get.hex
                val held = state.control.get(c.hex)
                apply(MoveUnit(f.name, id, c.hex))
                log(unitElem(id), "moved from", Cell(from), "to", c)
                if (held.has(f.name).not)
                    captured(f, c)
                TurnAction()

            case AttackAction(f, a, t) =>
                val dice = state.unit(a).get.weapon.get.dice
                Roll[Int](List.fill(dice)(Die.range((1, Combat.sides))), r => AttackRolledAction(f, a, t, r))

            case AttackRolledAction(f, a, t, rolls) =>
                val target = unitElem(t)
                apply(AttackUnit(f.name, a, t, rolls))
                state.lastReport.foreach(report(_, target))
                TurnAction()

            case EndActionsAction(f) =>
                apply(EndPhaseAction(f.name))
                TurnAction()

            case EndTurnAction(f) =>
                log(f, "ended the turn")
                apply(EndPhaseAction(f.name))
                if (state.winner.any)
                    TurnAction()
                else
                    Milestone(PlayerTurnAction(current.get))

            case GameOverAction(w) =>
                isOver = true
                log(w, "won with", state.player(w.name).vp.hl, "VP")
                GameOver($(w), "Game Over", $(GameOverWonAction(null, w)))
        }
    }

    // a sector the player now holds
    def captured(f : Faction, c : Cell) {
        if (state.control.get(c.hex).has(f.name))
            Skirmish.sectorName(c.hex).foreach(s => log(f, "captured the", s.hl, "in", c))
    }
}
