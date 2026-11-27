package root
//
//
//
//
import hrf.colmat._
//
//
//
//

// THE COURT CROW'S JESTS (2026-11-25, asked for: "have the crow do off turn things, like laugh at you being attacked and your opponent
// rolled 3-0, or laugh that a mob razed your buildings, a revolt wiped you out, or lizards converting"): the Roast Me role's heckler
// (good-game/Heckler.scala) also mocks what happens to a player on anyone's turn. The server can't read a game, and the log names no
// one for a Raze or a Revolt, so the game notes each such event here as it plays out, with its victim, numbered in order, and the
// clients send the last few with their turn reports (Meta.jests, Journal.turn); the server takes each number once.
//
// Noted when the action is performed (internalPerform), before it changes the board, so a Raze's or a Revolt's victims are the
// factions whose pieces it is about to remove. Display only: nothing here changes the game, and every client, replaying the same log,
// notes the same events with the same numbers. An event: "<tag>/<victim>/<other faction>/<clearing>".
//   rolled30   the attacker rolled 3 and the victim, defending, 0          whiffed   the victim attacked and rolled 0 to the defender's 3
//   ambushed   the victim's attack was ambushed                            raze      the Hundreds razed the victim's buildings or tokens
//   revolt     a Woodland Alliance Revolt removed the victim's pieces      convert   the Lizard Cult converted the victim's warrior
//   sanctify   the Lizard Cult sanctified the victim's building            turmoil   the Eyrie fell into Turmoil (the victim is the Eyrie)
//   bomb       a Corvid bomb went off on the victim's warriors
//   wiped      a battle left the victim with nothing in the clearing    zerovp    the victim starts a turn of the third round on 0 VP
//   closing    a faction reached 25 VP, the victim alone in last place   hostile   the Vagabond turned Hostile toward the victim
//   prisoner   the Knaves took the victim's warrior prisoner              dominance the victim played a dominance card
//   (the six above asked for on 2026-11-25: "add all of them")
//   faithful   the Faithful's extra hit against the victim drew a card matching the clearing, so it stays (2026-11-25, asked for: "drawing
//              the matching card is hilariously lucky/unlucky")
object Jests {
    def see(action : Action)(implicit game : Game) : Unit = try {
        def note(tag : String, victim : Faction, other : |[Faction], c : |[Clearing]) = {
            val e = tag + "/" + victim.short + "/" + other./(_.short).|("") + "/" + c./(_.name).|("")
            // the same event twice in a row (a battle finished again by a card's wrapper, a second prisoner of the same battle) is noted once
            if (game.jests.lastOption.has(e).not)
                game.jests :+= e
        }

        action @@ {
            case BattleFinishRollAction(b, f, o, _, _, fr, or, _, _) =>
                if (fr == 3 && or == 0) note("rolled30", o, |(f), |(b.clearing))
                if (fr == 0 && or == 3) note("whiffed", f, |(o), |(b.clearing))

            case BattleAmbushHitsAction(b, n, _) if n > 0 && b.defender.canRemove(b.clearing)(b.attacker) =>
                note("ambushed", b.attacker, |(b.defender), |(b.clearing))

            case RazeClearingAction(f, c, _) =>
                f.enemies.%(e => f.canRemove(c)(e) && e.at(c).exists { case _ : Building | _ : Token => true ; case _ => false }).foreach(e => note("raze", e, |(f), |(c)))

            case RevoltAction(f, c, _, _) =>
                f.enemies.%(e => f.canRemove(c)(e) && e.at(c).any).foreach(e => note("revolt", e, |(f), |(c)))

            case FanaticConvertAction(f, _, c, e, _) => note("convert", e, |(f), |(c))

            case FanaticSanctifyAction(f, _, c, _, e, _) => note("sanctify", e, |(f), |(c))

            case TheFaithfulRevealAction(f, c, BattlePostRollAction(b, _, _, _, _, _)) if f.drawn.$.exists(_.matches(c.cost)) =>
                note("faithful", if (f == b.attacker) b.defender else b.attacker, |(f), |(c))

            // the battle left a side with nothing in the clearing (2026-11-25: "wiped out of a clearing")
            // (only a real presence lost: 4 pieces or more, or a building, when the battle started; most battles clear a lone warrior)
            case BattleStartAction(_, _, a, _, c, o, _, _) =>
                def weight(e : Faction) = e.at(c).num + e.at(c).exists { case _ : Building => true ; case _ => false }.?(4).|(0)
                game.jestBattle = Map(a -> weight(a), o -> weight(o))

            case BattleFinishedAction(b) =>
                $(b.defender -> b.attacker, b.attacker -> b.defender).foreach { case (e, o) =>
                    if (game.factions.has(e) && game.jestBattle.get(e).exists(_ >= 4) && e.at(b.clearing).none)
                        note("wiped", e, |(o), |(b.clearing))
                }

            // a faction starting a turn of the third round still on 0 VP (once each, as the third round comes once), and the first time
            // a faction stands on 25 VP or more at a turn's start, the faction alone in last place
            case StartPlayerTurnAction(f) =>
                val n = game.factions.num
                if (n > 0 && game.turn / n + 1 == 3 && f.vp == 0 && f.dominance.none)
                    note("zerovp", f, None, None)

                if (game.factions.num > 1 && game.jests.exists(_.startsWith("closing/")).not) {
                    val leader = game.factions.maxBy(_.vp)
                    val low = game.factions.minBy(_.vp)
                    if (leader.vp >= 25 && low != leader && game.factions.count(_.vp == low.vp) == 1)
                        note("closing", low, |(leader), None)
                }

            // the Vagabond turning Hostile toward the victim
            case ForcedRemoveSourceEffectAction(e : Hero, c, f, p : Warrior, _) if f.is[Hireling].not && e.hostile(f).not =>
                note("hostile", f, |(e), |(c))

            case BattleCleanupAttackerAction(b, f : Hero) if b.balance < 0 && b.ally.any && f.named.has(b.ally.get).not =>
                note("hostile", b.ally.get, |(f), |(b.clearing))

            // the Knaves taking the victim's warriors prisoner
            case KnavesTakePrisonerAction(f, b, _, _, _) => note("prisoner", b.defender, |(f), |(b.clearing))

            case KnavesTakeCodefPrisonerAction(f, b, e, _, _, _) => note("prisoner", e, |(f), |(b.clearing))

            // playing a dominance card (the victim is the player)
            case ActivateDominanceAction(f, _, _) => note("dominance", f, None, None)

            case TurmoilAction(f) => note("turmoil", f, None, None)

            case FlipAction(f, Bomb, c) =>
                f.enemies.%(e => f.canRemove(c)(e) && e.at(c).of[Warrior].any).foreach(e => note("bomb", e, |(f), |(c)))

            case _ =>
        }
    }
    catch {
        case e : Throwable =>
    }

    // the last events, numbered from the game's first, the victim by its seat ("<n>:<tag>/<seat>/<other>/<clearing>")
    def report(game : Game, seat : Faction => String) : $[String] = {
        val all = game.jests
        all.zipWithIndex.drop(all.num - 12)./~ { case (e, i) =>
            e.split('/').toList match {
                case tag :: v :: rest => game.factions.find(_.short == v)./(f => seat(f)).%(_.nonEmpty)./(s => (i + 1) + ":" + (tag :: s :: rest).mkString("/"))
                case _ => None
            }
        }
    }
}
