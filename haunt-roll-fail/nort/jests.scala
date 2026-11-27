package nort
//
//
//
//
import hrf.colmat._
//
//
//
//

// RATATOSKR'S JESTS (2026-11-26, asked for: "build out a troll library for Northgard that's equal to Root's"): as the Court Crow does in
// Root (root/jests.scala), the Roast Me role's Northgard heckler (good-game/Heckler.scala) mocks what happens to a player on anyone's
// turn. The game notes each event with its victim as it plays out, numbered in order, and the clients send the last few with their turn
// reports (Meta.jests); the server takes each number once. Display only, and from the public log alone: nothing about hidden fame.
// An event: "<tag>/<victim>/<other>/<territory>", the other a clan's short name or "~<creature>".
//   lostland   the victim, defending, lost the fight and the territory to a clan        repelled  the victim attacked a clan and lost
//   wiped      a fight killed all the victim's units there                                creature  a creature beat the victim
//   mutual     both sides were wiped out (noted for each clan)                            unrest    the victim couldn't pay for Winter
//   chief      the victim picked their warchief as a casualty                             lair      a creature appeared in the victim's territory
//   prowl      a creature moved into the victim's territory
object Jests {
    def see(action : Action)(implicit game : Game) : Unit = try {
        def note(tag : String, victim : Faction, other : String, where : String) = {
            val e = tag + "/" + victim.short + "/" + other + "/" + where.replace('/', ' ')
            if (game.jests.lastOption.has(e).not)
                game.jests :+= e
        }

        def place(a : AreaRef) : String = scala.util.Try("Territory " + game.board.label(a)).getOrElse("")
        def side(s : FightSide) : String = s.f match { case Some(f) => f.short ; case None => "~" + s.who.text.takeWhile(_ != '(').trim }
        def owner(a : AreaRef) : |[Faction] = scala.util.Try(game.present(game.board.territory(a)) match { case List(f) => Some(f) ; case _ => None }).getOrElse(None)

        action @@ {
            // a fight decided (its report is kept until everyone shown it has tapped OK, so it is noted once)
            case CombatReportAction(_, _) if game.fightReport.any && game.jestFight != game.fightReport =>
                game.jestFight = game.fightReport
                val r = game.fightReport.get
                val where = place(r.area)
                r.winner match {
                    case None =>
                        r.attacker.f.foreach(f => note("mutual", f, side(r.defender), where))
                        r.defender.f.foreach(f => note("mutual", f, side(r.attacker), where))
                    case Some(attackerWon) =>
                        val w : FightSide = if (attackerWon) r.attacker else r.defender
                        val l : FightSide = if (attackerWon) r.defender else r.attacker
                        l.f.foreach { f =>
                            val tag =
                                if (r.wipe) "wiped"
                                else if (w.f.none) "creature"
                                else if (l.attacking) "repelled"
                                else "lostland"
                            note(tag, f, side(w), where)
                        }
                }

            case TakeUnrestAction(f, _, _) => note("unrest", f, "", "")

            case UnrestGoneAction(f, _, _) => note("unrest", f, "", "")

            case CasualtyPickAction(f, a, _, _, _, true, _, _) => note("chief", f, "", place(a))

            // (not the clan whose own Explore brought it)
            case CreatureAppearAction(e, a, _, _) => owner(a).%(_ != e).foreach(f => note("lair", f, "", place(a)))

            case CreatureMoveAction(c, a, _) => owner(a).foreach(f => note("prowl", f, "~" + c.kind.title, place(a)))

            case _ =>
        }
    }
    catch {
        case e : Throwable =>
    }

    // the last events, numbered from the game's first, the victim by its seat ("<n>:<tag>/<seat>/<other>/<territory>")
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
