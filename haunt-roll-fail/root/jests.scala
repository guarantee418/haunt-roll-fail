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
object Jests {
    def see(action : Action)(implicit game : Game) : Unit = try {
        def note(tag : String, victim : Faction, other : |[Faction], c : |[Clearing]) =
            game.jests :+= tag + "/" + victim.short + "/" + other./(_.short).|("") + "/" + c./(_.name).|("")

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
