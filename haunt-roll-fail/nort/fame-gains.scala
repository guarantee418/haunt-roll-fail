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


// Marzaus's "Alternative fame gains" (Discord, northgard-fanmade-content, 2026-10-22; asked for by Gman on 2026-11-01), option
// ModuleOption(FameGainsModule) under Fan content. Fame comes from what a clan does, as it is done, and a clan with 120 fame that
// controls an Altar of Kings wins at once. RULES.md (Alternative fame gains) lists the readings of the short post:
// - "Altar of Fame" is the Altar of Kings, the board game's only altar
// - 120 fame on the fame tokens (f.fame); checked after every action (Game.loggedPerform), it replaces the three closed territories
//   with large buildings and Alternative victory (the option needs Standard or Fame only victory); it is the only way to win: nobody
//   there after the last year, the game goes on, year after year, until a clan wins (Gman, 2026-11-02)
// - the Altar of Kings still gives its 3 fame each harvest; when built it gives 2 like any large building and 3 more (5)
// - kills and wins only in fights against a clan (not creatures, not card effects outside a fight); Kaija and the other companions
//   are no units; the warchief is (5); before-the-fight kills (Defense Towers, Leadership) count
// - every building placed counts (free ones too, Mines as small ones); a replaced building doesn't
// - an upgrade bought with lore (or the Lion's Faith) gives 5
// - a Development card gives its fame when taken and nothing at the end
// - Achievements: at setup, in turn order, each clan draws 2 at random, keeps 1 face down and shuffles the other back; once, on its
//   turn, when the card would give fame, it may resolve it for that fame (not an action); unresolved, it gives nothing. The last year
//   reveals no Achievement cards, as they were all dealt at setup

// SETUP: the Achievements dealt in turn order
case class FameDealAction(l : $[Faction], then : ForcedAction) extends ForcedAction
case class FameDealtAction(f : Faction, shuffled : $[Card], l : $[Faction], then : ForcedAction) extends ShuffledAction[Card]
case class FameKeepAction(self : Faction, card : Card, other : Card, l : $[Faction], then : ForcedAction) extends BaseAction("Alternative fame gains".hl, Break, "Keep one Achievement card face down, to resolve on a turn")(card.handImg) with ViewObject[Card] { def obj = card }

// TURN: resolving the kept Achievement, then back to the same choices
case class FameResolveAction(self : Faction, card : Card, then : ForcedAction) extends BaseAction("Your turn")((g : Game) => FameGainsExpansion.resolveText(self, card)(g)) with ViewObject[Card] { def obj = card }

// 120 fame and an Altar of Kings
case class FameWinAction(l : $[Faction]) extends ForcedAction

object FameGainsExpansion extends Expansion {
    def goal = Debug.fameGoal

    def on(implicit game : Game) = game.has(FameGainsModule)

    def gain(f : Faction, n : Int, why : Any*)(implicit game : Game) {
        if (on && f != Automa && n > 0 && game.states.contains(f)) {
            f.fame += n
            f.log(("gained" +: Amount(n.hl, FameIcon()) +: why.toList) : _*)
            game.note("fame-gains")
        }
    }

    // A building placed: 1 fame for a small one, 2 for a large one, 3 more for an Altar of Kings
    def built(f : Faction, b : Building)(implicit game : Game) {
        gain(f, b.large.?(2).|(1) + (b == AltarOfKings).??(3), "for building", b)
    }

    // An upgrade bought: 5 fame
    def upgraded(f : Faction, u : Card)(implicit game : Game) {
        gain(f, 5, "for the upgrade", u)
    }

    // A Development card taken: its fame now (and none at the end, CommonExpansion.cardFame)
    def taken(f : Faction, c : Card)(implicit game : Game) {
        if (c.is[Development])
            gain(f, c.fame, "for taking", c)
    }

    def scoresLater(c : Card)(implicit game : Game) : Boolean = on.not || c.is[Development].not

    // Winning a fight against a clan: 1 fame
    def won(w : Faction, l : Faction)(implicit game : Game) {
        gain(w, 1, "for winning the fight against", l)
    }

    // Killing a clan's units: 1 fame each, 5 for its warchief
    def killed(w : Faction, l : Faction, units : Int, chief : Boolean)(implicit game : Game) {
        if (units + chief.??(1) > 0)
            gain(w, units + chief.??(5), "for killing", ((units > 0).$((units == 1).?("1 unit").|(units.toString + " units").txt) ++ chief.$(Warchief.elem(l))).join(" and "), "of", l)
    }

    // After a fight against a clan, from each side's soldiers and warchief before the casualties (MilitaryExpansion.before), with the
    // units killed before the fight (game.fightKills); the same sums as the Military paths' MXP
    def afterCombat(attacker : Faction, defender : Faction, t : Territory, before : Map[Faction, (Int, Boolean)], winner : |[Faction])(implicit game : Game) {
        if (on)
            $(attacker -> defender, defender -> attacker).foreach { case (w, l) =>
                if (winner.has(w))
                    won(w, l)

                val (n, chief) = before(l)
                val units = math.max(0, n - MilitaryExpansion.soldiers(t, l)) + game.fightKills.getOrElse(w, 0)

                killed(w, l, units, chief && game.chiefIn(t, l).not)
            }
    }

    // ACHIEVEMENTS: the kept card's fame now, as at the end of the game
    def resolveText(f : Faction, c : Card)(implicit game : Game) : Elem = {
        val n = CommonExpansion.cardFame(f, c)
        "Resolve".txt ~ " " ~ c.elem ~ " for " ~ Amount(n.hl, FameIcon()) ~ " (not an action)".spn(xstyles.smaller85)
    }

    def resolve(f : Faction, then : ForcedAction)(implicit game : Game) : $[UserAction] =
        (on && f != Automa).??(game.fameKept.get(f).$./(c => FameResolveAction(f, c, then).!(CommonExpansion.cardFame(f, c) <= 0, "would give no fame now")))

    // Clans with 120 fame or more that control an Altar of Kings
    def winners(implicit game : Game) : $[Faction] =
        game.factions.%(f => game.states.contains(f) && f.fame >= goal && game.controlled(f).exists(t => game.buildingsIn(t).exists(_._2 == AltarOfKings)))

    def perform(action : Action, soft : Void)(implicit game : Game) : Continue = action @@ {
        // SETUP: once the map is set up, before the first year, the Achievements in turn order; none are left for the last year
        case StartYearAction if game.fameDealt.not =>
            game.fameDealt = true
            game.achievements = $

            Then(FameDealAction(game.from(game.first).but(Automa), StartYearAction))

        case FameDealAction(Nil, then) =>
            Then(then)

        case FameDealAction(f :: rest, then) =>
            val pool = (Cards.achievementCards ++ game.horizonsDevelopments.??(Cards.horizonsAchievementCards)).diff(game.fameKept.values.$)

            Shuffle[Card](pool, FameDealtAction(f, _, rest, then))

        case FameDealtAction(f, l, rest, then) =>
            l.take(2) match {
                case a :: b :: _ => Ask(f).add(FameKeepAction(f, a, b, rest, then)).add(FameKeepAction(f, b, a, rest, then))
                case a :: _ => Then(FameKeepAction(f, a, a, rest, then))
                case _ => Then(FameDealAction(rest, then))
            }

        case FameKeepAction(f, c, _, rest, then) =>
            game.fameKept += f -> c

            f.log("kept an Achievement card face down")

            Then(FameDealAction(rest, then))

        // TURN: once a game, on f's turn
        case FameResolveAction(f, c, then) =>
            val n = CommonExpansion.cardFame(f, c)

            game.fameKept -= f
            game.fameResolved :+= f -> c

            f.log("resolved", c)
            gain(f, n, "for", c)

            game.note("fame-achievement")

            Then(then)

        // After the last year nobody wins on fame: the game goes on until a clan wins with 120 fame and an Altar of Kings
        // (Gman, 2026-11-02); the bot games stop after Debug.fameExtraYears more years and score as usual
        case GameEndAction if game.year < game.lastYear + Debug.fameExtraYears =>
            log(DoubleLine)
            log("Nobody has", Amount(goal.hl, FameIcon()), "with an", AltarOfKings, Comma, "so the game goes on")

            game.note("fame-extra-year")

            Then(game.from(game.first).foldRight(NewYearAction : ForcedAction)((f, then) => ReturnUnitsAction(f, then)))

        // THE END: 120 fame and an Altar of Kings
        case FameWinAction(rulers) =>
            log(DoubleLine)

            rulers.foreach(f => f.log("reached", Amount(goal.hl, FameIcon()), "and controls an", AltarOfKings))

            // Team play: their teams win; ties: fame, then territories controlled, units and buildings (added up for teams)
            val contenders = game.teams.?(CommonExpansion.sides(game.factions).%(_.exists(rulers.has))).|(CommonExpansion.sides(rulers))
            val winners = CommonExpansion.best(contenders, $(f => f.fame, f => game.controlled(f).num, f => f.units, f => CommonExpansion.buildings(f))).flatten

            game.isOver = true
            game.highlight.current = winners.single

            winners.foreach(f => f.log("won"))

            game.note("fame-win")

            Debug.summary(game)

            CommonExpansion.victory(winners, $("Won in year " ~ game.year.hl ~ " by reaching " ~ Amount(goal.hl, FameIcon()) ~ " with an " ~ AltarOfKings.elem ~ " (Alternative fame gains)."))

        case _ => UnknownContinue
    }
}
