package hrf
//
// Loads every online game from a copy of guarantee418/hrf-games the way the browser does
// (runner.scala: performContinue on each recorded action, validating the last 40,
// UndoAction rebuilding through the generateGameVoid steps) and prints "ok" or what stopped it.
// Run it with the code about to be deployed, in the JVM host project (see nort/HANDOFF.md):
//   sbt "runMain hrf.LiveCheck <hrf-games checkout> [full] [id:<game id>]"
// "full" also counts the moves anywhere in a game that fail validation (only the last 40 are
// validated on load, so those don't stop it); "id:" checks one game.
import hrf.colmat._
import hrf.meta._

object LiveCheck {
    val metas : $[MetaGame] = $(root.Meta, root.MetaAdset, root.MetaMirror, nort.Meta, nort.MetaAdset)

    def main(args : Array[String]) {
        val dir = args(0)
        val full = args.contains("full")
        val only = args.find(_.startsWith("id:")).map(_.drop(3))
        val src = scala.io.Source.fromFile(dir + "/journals.tsv")
        val journals = src.getLines().drop(1).map(_.split("\t", -1).toList).toList
        src.close()

        def read(id : String) : $[(String, String)] = {
            val f = new java.io.File(dir + "/games/" + id + ".tsv")
            if (!f.exists) return $
            val s = scala.io.Source.fromFile(f)
            val r = s.getLines().drop(1).map(_.split("\t", 3).toList).collect { case $(_, u, t) => u -> t.replace("\\n", "\n").replace("\\t", "\t").replace("\\\\", "\\") }.toList
            s.close()
            r
        }

        var bad = 0
        var total = 0

        journals.filter(_(1).endsWith(" LOBBY")).foreach { j =>
            val lobby = read(j(0)).map(_._2)
            val metaName = lobby.find(_.startsWith("meta ")).map(_.drop(5))
            val server = lobby.find(_.startsWith("server ")).map(_.drop(7))
            metas.find(m => metaName.contains(m.name)).foreach { meta =>
                server.filter(s => only.forall(_ == s)).foreach { gid =>
                    val title = j(1).dropRight(6)
                    total += 1
                    val r = try { check(meta, lobby, read(gid).map(_._2), full) } catch { case e : Throwable => "EXCEPTION " + e.toString + "\n" + e.getStackTrace.take(12).mkString("\n") }
                    if (r.startsWith("ok").not) bad += 1
                    println(meta.name + " " + gid + " [" + title + "] " + r)
                }
            }
        }

        println("games: " + total + " bad: " + bad)
    }

    def check(meta : MetaGame, lobby : $[String], recorded : $[String], full : Boolean) : String = {
        import meta.gaming._

        var seating : $[meta.F] = $
        var bots = Map[meta.F, String]()
        var options : $[meta.O] = $

        lobby.foreach { line =>
            val l = line.split(' ').toList
            l(0) match {
                case "bot" => bots += meta.parseFaction(l(1)).get -> l(2)
                case "seating" => seating = l.drop(1).map(f => meta.parseFaction(f).get)
                case "options" => options = l.drop(1).flatMap(o => meta.parseOption(o))
                case _ =>
            }
        }

        if (seating.isEmpty)
            return "no seating"

        if (recorded.isEmpty)
            return "ok"

        if (options.isEmpty)
            options = meta.defaultsFor(seating.num, seating)

        val unknown = options.filter(_.toString.contains("Unknown"))

        // In the order of meta.options, as the browser makes the game (OptionsState in hrf.scala), not the lobby's
        val opts = (meta.options.intersect(options) ++ meta.botOptions(seating, bots)).distinct
        val active = seating.filter(f => bots.get(f).has("Off").not)

        def newGame() : G = meta.createGame(active, opts).asInstanceOf[G]

        val parsed = recorded.map(s => meta.parseActionExternal(s).asInstanceOf[ExternalAction])

        val cant = parsed.zipWithIndex.filter(_._1.isInstanceOf[CantParseAction])
        if (cant.any)
            return "CANT PARSE at " + cant.head._2 + ": " + cant.head._1

        var game = newGame()
        var actions : $[ExternalAction] = $

        def generateGameVoid(n : Int) : G = {
            var lll : $[ExternalAction] = $
            var skip = 0
            actions.take(n).reverse.foreach { a =>
                if (skip > 0) skip -= 1
                else a match {
                    case UndoAction(_, n, _) => skip = n
                    case a => lll +:= a
                }
            }
            val g = newGame()
            g.asInstanceOf[Any] match { case lg : LoggedGame => lg.logging = false ; case _ => }
            lll.dropRight(1).foreach(a => g.performVoid(a))
            lll.lastOption.foreach { a =>
                var c = g.performContinue(|(g.continue), a, false).nest
                while (c match {
                    case Log(_, _, x) => c = x; true
                    case DelayedContinue(_, Force(t)) => c = g.performContinue(|(g.continue), t, false).nest; true
                    case _ => false
                }) {}
            }
            g
        }

        def unlog(c : Continue) : Continue = c match {
            case Log(_, _, x) => unlog(x)
            case x => x
        }

        var last : Continue = null
        var invalid : $[Int] = $

        parsed.zipWithIndex.foreach { case (a, i) =>
            actions :+= a
            val remaining = parsed.num - i - 1
            a match {
                case UndoAction(_, _, t) =>
                    game = generateGameVoid(t)
                    last = game.continue
                case a =>
                    val validate = full || remaining < 40
                    if (validate && i > 0 && a.isSoft.not && a.unwrap.isInstanceOf[SkipValidate].not && a.unwrap.isInstanceOf[OutOfTurn].not && game.validate(game.continue, a).not)
                        invalid :+= i
                    var c = unlog(game.performContinue(|(game.continue), a, remaining < 40).nest)
                    // the browser performs a forced action left waiting (a DelayedContinue, like the Eyrie's Turmoil)
                    // before the next recorded one (UIContinue in runner.scala)
                    while (c match {
                        case Force(t) => c = unlog(game.performContinue(|(game.continue), t, false).nest); true
                        case DelayedContinue(_, x) => c = unlog(x); true
                        case _ => false
                    }) {}
                    c match {
                        case ErrorContinue(e, m) =>
                            return "ERROR at " + i + "/" + parsed.num + " (" + recorded(i).take(200) + "): " + m + " " + e + "\n" + e.getStackTrace.take(10).mkString("\n")
                        case _ =>
                    }
                    last = c
            }
        }

        // what the browser does after the last recorded action
        var c = unlog(last)
        var steps = 0
        while (steps < 1000 && (c match {
            case Force(a) => c = unlog(game.performContinue(|(game.continue), a, false).nest); true
            case DelayedContinue(_, x) => c = unlog(x); true
            case Ask(_, List(a)) if a.isSoft => c = unlog(game.performContinue(|(game.continue), a, false).nest); true
            case _ => false
        })) steps += 1

        val end = c match {
            case ErrorContinue(e, m) => "ERROR after last action: " + m + " " + e
            case Ask(f, l) if l.isEmpty => "EMPTY ASK for " + f
            case MultiAsk(l, _) if l.isEmpty && game.isOver.not => "EMPTY MULTIASK"
            case _ => ""
        }

        val inv = invalid.any.??("INVALID (fails validation, " + invalid.num + " actions; " + invalid.filter(i => parsed.num - i - 1 < 40).num + " in the last 40) first at " + invalid.head + "/" + parsed.num + ": " + recorded(invalid.head).take(160))

        val res = $(end, inv, unknown.any.??("unknown options " + unknown.mkString(" "))).filter(_.nonEmpty)

        res.any.?(res.mkString("; ")).|("ok" + game.isOver.??(" (over)"))
    }
}
