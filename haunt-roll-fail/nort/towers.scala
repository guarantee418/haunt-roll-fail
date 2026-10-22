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


// The Tower PC version (option ModuleOption(TowersModule), under Fan content), fan-made by Marzaus (Discord, #northgard-fanmade-content,
// asked for by Gman on 2026-10-22): Defense Towers work as in the Northgard video game.
// - A Defense Tower is destroyed (back to the supply) once the clan that held its territory no longer has a figure there:
//   captured or abandoned (sweep, run by Game.loggedPerform before and after each action).
// - It costs 2 wood (discounts still apply; free builds stay free).
// - Each Defense Tower kills 1 attacking unit before the fight (TowerVolleyAction), not after it; the attacker picks
//   the figure (ChooseCasualties), Shieldbearers can cancel it, Conqueror ignores it; with no attacker left, the defender
//   wins without a roll.
// - A clan fighting a creature gets 1 more combat point per Defense Tower in the territory (towerPoints, in creatures.scala),
//   and when a creature appears or moves into a territory with a Defense Tower, the clan holding it may fight it first,
//   as the attacker, before the creature's ability (TowerChallengeAction).
// RULES.md (Tower PC version) lists the choices made where the post is short.

// BEFORE THE FIGHT: the Defense Towers' kills, then the fight as usual (FightStartAction)
case class TowerVolleyAction(f : Faction, area : AreaRef, e : MoveEffect, then : ForcedAction) extends ForcedAction

// A CREATURE ENTERING: fight it with the Defense Tower before its ability, or let it act
case class TowerChallengeAction(self : Faction, area : AreaRef, c : Creature, then : ForcedAction) extends BaseAction(c, "entered", area, "with your", DefenseTower)("Fight", c, "before its ability") with MapTarget { def target = area }
case class TowerChallengeSkipAction(self : Faction, area : AreaRef, c : Creature, then : ForcedAction) extends BaseAction(c, "entered", area, "with your", DefenseTower)("Let it be")
// After that fight: the ability, if the creature is still there
case class TowerChallengeDoneAction(area : AreaRef, c : Creature, then : ForcedAction) extends ForcedAction


object TowersExpansion extends Expansion {
    def on(implicit game : Game) = game.has(TowersModule)

    // A Defense Tower's cost in wood
    def cost(b : Building)(implicit game : Game) : Int = (b == DefenseTower && on).?(2).|(b.cost)

    // Whether Defense Towers kill before the fight rather than adding casualties after it
    def killFirst(implicit game : Game) : Boolean = on

    // The Defense Towers in t
    def towers(t : Territory)(implicit game : Game) : Int = game.working(t).count(_ == DefenseTower)

    // Combat points against a creature in t
    def towerPoints(t : Territory)(implicit game : Game) : Int = on.??(towers(t))

    // Captured or abandoned: a tower whose holder has no figure left in its territory is destroyed;
    // a new tower's holder is the clan alone in its territory
    def sweep(implicit game : Game) {
        val spaces = game.buildings.filter(_._2 == DefenseTower).keys.toList.sortBy(s => (s.area.x, s.area.y, s.area.id, s.index))

        game.towerHolder = game.towerHolder.view.filterKeys(spaces.has).toMap

        spaces.foreach { s =>
            val t = game.board.territory(s.area)
            val present = game.present(t)

            game.towerHolder.get(s) match {
                case Some(f) if present.has(f) =>
                case Some(f) =>
                    destroy(s, t, present.any.?("captured").|("abandoned"))
                case None =>
                    present.single match {
                        case Some(f) => game.towerHolder += s -> f
                        case None if present.none => destroy(s, t, "abandoned")
                        case None =>
                    }
            }
        }
    }

    def destroy(s : SpaceRef, t : Territory, why : String)(implicit game : Game) {
        game.buildings -= s
        game.towerHolder -= s
        game.note("tower-" + why)
        game.log(DefenseTower, "in", t.anchor, "was destroyed", "(" + why + ")")
    }

    def perform(action : Action, soft : Void)(implicit game : Game) = action @@ {
        // The towers fire once per fight, before anything else in it
        case FightStartAction(f, a, e, then) if killFirst && game.volleyed.has(a).not =>
            Then(TowerVolleyAction(f, a, e, then))

        case FightStartAction(f, a, e, then) if killFirst =>
            game.volleyed = None
            UnknownContinue

        case TowerVolleyAction(f, a, e, then) =>
            val t = game.board.territory(a)
            val defender = game.present(t).but(f).headOption
            val n = f.conqueror.?(0).|(towers(t))
            // Shieldbearers cancel 1 casualty inflicted by the defender: this one, if there is one
            val shield = (n > 0 && (e.special == ShieldMove || e.special == BorgildMove)).??(1)
            val k = math.min(n - shield, game.figures(t, f))

            lazy val pick = MapExpansion.pickCasualties($(f -> k), t, TowerVolleyAction(f, a, e, then))

            if (defender.none || k == 0) {
                game.towerShield = shield > 0
                if (shield > 0)
                    f.log("lost no unit to", DefenseTower, "in", a, "(" ~ 1.hl ~ " cancelled by " ~ "Shieldbearers".hl ~ ")")
                game.volleyed = |(a)
                Then(FightStartAction(f, a, e, then))
            }
            else
            if (pick.any)
                pick.get
            else {
                game.towerShield = shield > 0
                game.removeCasualties(t, f, k)
                game.advance(f, "valhalla", k)
                game.note("tower-volley")

                f.log("lost", k.hl, (k == 1).?("unit").|("units"), "to", (n == 1).?(DefenseTower.elem).|("the Defense Towers".hl), "in", a, "before the fight", (shield > 0).?("(" ~ 1.hl ~ " cancelled by " ~ "Shieldbearers".hl ~ ")").|(Empty))

                if (game.figures(t, f) == 0) {
                    val d = defender.get
                    game.fights += 1
                    game.note("attack-lost")
                    d.log("won the fight in", a, "with", DefenseTower)
                    EventsExpansion.afterCombat(f, d, 0, 0, |(d))
                    Then(then)
                }
                else {
                    game.volleyed = |(a)
                    Then(FightStartAction(f, a, e, then))
                }
            }

        // A creature entering a territory with a Defense Tower: its holder may fight it first
        case CreatureEffectAction(c, then) if on && game.towerAsked.has(c).not && game.creatureAt.contains(c) =>
            val a = game.creatureAt(c)
            val t = game.board.territory(a)
            val holder = game.present(t).single.%(_ != Automa)

            game.towerAsked = |(c)

            holder.%(_ => towers(t) > 0) match {
                case Some(f) => Ask(f).add(TowerChallengeAction(f, t.anchor, c, then)).add(TowerChallengeSkipAction(f, t.anchor, c, then))
                case None => Then(CreatureEffectAction(c, then))
            }

        case CreatureEffectAction(c, then) if on =>
            game.towerAsked = None
            UnknownContinue

        case TowerChallengeAction(f, a, c, then) =>
            f.log("fights", c, "in", a, "with", DefenseTower)
            game.note("tower-challenge")

            Then(CreatureCombatAction(f, a, c, MoveEffect(0), true, TowerChallengeDoneAction(game.creatureAt(c), c, then)))

        case TowerChallengeSkipAction(f, a, c, then) =>
            game.note("tower-challenge-skip")
            Then(CreatureEffectAction(c, then))

        // Defeated or driven back to its Den, it doesn't act
        case TowerChallengeDoneAction(a, c, then) =>
            if (game.creatureAt.get(c).has(a))
                Then(CreatureEffectAction(c, then))
            else {
                game.towerAsked = None
                Then(then)
            }

        case _ => UnknownContinue
    }
}
