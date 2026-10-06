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

// Plays bot games, records the actions the way the browser journal does, and
// checks that rebuilding the game from a prefix of them (what undo and loading
// do, via performVoid, after a write and parse like the server's copy) gives
// the same state as the live game at that point. JVM only, like host.scala:
//   sbt "runMain nort.ReplayCheck [games] [dense] [hard]"
// Each game gets random clans, colors and options from Host.batch (so the
// NORT_* variables of the host apply too). "dense" checks every prefix of the
// first 400 actions instead of about 80 spread over the game; "hard" seats the
// Hard bot first.
object ReplayCheck {
    // Fields that are bookkeeping of the framework or the UI, or caches, not game state
    val skipped = Set("logs", "actionsReverseList", "notifications", "continue", "voiding", "logging", "highlight", "game")

    def clean(s : String) = s.replaceAll("\\$\\$Lambda[^,) ]*", "Lambda").replaceAll("@[0-9a-f]+", "")

    // Every field of the game and the objects it owns (faction states, the board), one line each,
    // with maps and sets sorted so only their contents matter
    def fingerprint(g : Game) : $[String] = {
        var lines : $[String] = $

        def value(x : Any, depth : Int) : String = x match {
            case null => "null"
            case m : scala.collection.Map[_, _] => m.toList.map { case (k, v) => value(k, depth) + " -> " + value(v, depth) }.sorted.mkString("Map(", ", ", ")")
            case s : scala.collection.Set[_] => s.toList.map(value(_, depth)).sorted.mkString("Set(", ", ", ")")
            case l : scala.collection.Seq[_] => l.map(value(_, depth)).mkString("List(", ", ", ")")
            case Some(v) => "Some(" + value(v, depth) + ")"
            case _ : Game => "game"
            case p : Product => clean(p.toString)
            case _ : Number | _ : Boolean | _ : String | _ : Character => x.toString
            case o : AnyRef if o.getClass.getName.startsWith("nort.") && depth < 3 => fields(o, depth + 1).mkString("{", "; ", "}")
            case o : AnyRef => o.getClass.getName.takeWhile(_ != '$')
        }

        def fields(o : AnyRef, depth : Int) : $[String] = {
            var cl : Class[_] = o.getClass
            var r = $[String]()
            while (cl != null && cl.getName.startsWith("nort.")) {
                cl.getDeclaredFields.foreach { fd =>
                    val name = fd.getName.split("\\$").last
                    if (java.lang.reflect.Modifier.isStatic(fd.getModifiers).not && skipped.contains(name).not && name.endsWith("Cache").not && name.contains("bitmap").not) {
                        fd.setAccessible(true)
                        r :+= name + " = " + value(fd.get(o), depth)
                    }
                }
                cl = cl.getSuperclass
            }
            r
        }

        lines ++= fields(g, 0)
        g.states.toList.sortBy(_._1.toString).foreach { case (f, s) => lines ++= fields(s, 1)./(f.toString + "." + _) }
        lines :+ ("placements = " + value(g.board.placements, 1))
    }

    def show(c : Continue) : String = clean(show0(c))

    def show0(c : Continue) : String = c match {
        case Log(_, _, c) => show0(c)
        case DelayedContinue(_, c) => show0(c)
        case Ask(f, l) => "Ask(" + f + ", " + l./(_.unwrap.toString).mkString(" | ") + ")"
        case MultiAsk(l, _) => "MultiAsk(" + l./(show0).mkString(" || ") + ")"
        case c => c.toString.takeWhile(_ != '(')
    }

    // Follows the continues the browser performs without recording (forced and
    // soft actions) until it reaches one whose answer gets recorded
    def settle(g : Game, start : Continue) : Continue = {
        var c = start
        while (c match {
            case Log(_, _, x) => c = x; true
            case DelayedContinue(_, x) => c = x; true
            case Force(a) => c = g.performContinue(|(g.continue), a, false).nest; true
            case Then(a) if a.isSoft => c = g.performContinue(|(g.continue), a, false).nest; true
            case Ask(_, $(a)) if a.isSoft => c = g.performContinue(|(g.continue), a, false).nest; true
            case _ => false
        }) {}
        c
    }

    // What the client does on undo or load (generateGameVoid in runner.scala), from actions read back like the server's copy
    def rebuild(setup : $[Faction], options : $[Meta.O], recorded : $[ExternalAction]) : (Game, Continue) = {
        val g = new Game(setup, options)
        g.logging = false
        val parsed = recorded./(a => Meta.parseActionExternal(Meta.writeActionExternal(a)))
        parsed.dropRight(1).foreach(g.performVoid)
        val c = settle(g, g.performContinue(|(g.continue), parsed.last, false).nest)
        (g, c)
    }

    def main(args : Array[String]) {
        val games = args.lift(0).%(_.forall(_.isDigit))./(_.toInt).|(10)
        val dense = args.contains("dense")
        val hard = args.contains("hard")
        val batch = Host.batch
        Debug.stats = false
        var failures = 0

        0.until(games).foreach { gi =>
            val game = batch(gi % batch.num)()
            game.logging = false
            val setup = game.setup
            val options = game.options

            var recorded : $[ExternalAction] = $
            var snapshots : Map[Int, ($[String], String)] = Map()

            def record(a : Action) = a match {
                case a : ExternalAction => recorded :+= a
                case a => throw new Error("not external " + a)
            }

            def bot(f : Faction) : Bot = (hard && f == setup.first).?(new BotHard(f) : Bot).|(new BotXX(f))

            var next : Action = StartAction(version)
            record(next)
            var steps = 0
            var over = false

            try {
                while (over.not && steps < 12000) {
                    steps += 1
                    var c = settle(game, game.performContinue(|(game.continue), next, true).nest)
                    if (snapshots.contains(recorded.num).not)
                        snapshots += recorded.num -> (fingerprint(game), show(c))

                    var chosen : |[Action] = None
                    while (chosen.none && over.not) c match {
                        case ErrorContinue(e, m) => throw new Error("error continue " + m, e)
                        case Log(_, _, x) => c = x
                        case DelayedContinue(_, x) => c = x
                        case Force(a) => chosen = |(a)
                        case Milestone(_, a) => chosen = |(a.wrap); record(a.wrap)
                        case Then(a) => chosen = |(a); if (a.isSoft.not) record(a.wrap)
                        case Roll(d, r, _) => val a = r(d./(_.roll())); chosen = |(a); record(a)
                        case Roll2(d1, d2, r, _) => val a = r(d1./(_.roll()), d2./(_.roll())); chosen = |(a); record(a)
                        case Shuffle(l, s, _) => val a = s(l.shuffle); chosen = |(a); record(a)
                        case Shuffle2(l1, l2, s, _) => val a = s(l1.shuffle, l2.shuffle); chosen = |(a); record(a)
                        case ShuffleUntil(l, cond, s, _) => var r = l.shuffle; while (!cond(r)) r = l.shuffle; val a = s(r); chosen = |(a); record(a)
                        case ShuffleTake(l, n, s, _) => val a = s(l.shuffle.take(n)); chosen = |(a); record(a)
                        case Random(l, x, _) => val a = x(l.shuffle(0)); chosen = |(a); record(a)
                        case GameOver(_, _, _) => over = true
                        case MultiAsk(l, _) => c = l.shuffle.head
                        case Ask(f : Faction, l) =>
                            val a = if (l.num == 1) l(0) else bot(f).ask(l, 0)(game).immediate
                            chosen = |(a)
                            a match {
                                case _ if a.isSoft =>
                                case _ : DontRecord =>
                                case _ => record(a)
                            }
                        case x => throw new Error("unhandled continue " + x)
                    }

                    chosen.foreach(next = _)
                }
            }
            catch {
                case e : Throwable => +++("game", gi, "live play failed:", e); e.getStackTrace.take(12).foreach(s => +++("     ", s)); failures += 1
            }

            +++("game", gi, setup./(_.name).mkString(", "), "-", options./(Meta.writeOption).mkString(" "), "-", recorded.num, "recorded actions", over.?("finished").|("stopped"))

            var bad = 0
            val points = (if (dense) 1.until(math.min(recorded.num, 400)).$ else (1.until(recorded.num).by(math.max(1, recorded.num / 80)).$ :+ (recorded.num - 1))).distinct.%(snapshots.contains)
            points.foreach { k =>
                if (bad < 3) {
                    try {
                        val (g2, c2) = rebuild(setup, options, recorded.take(k))
                        val (fp, sc) = snapshots(k)
                        val fp2 = fingerprint(g2)
                        if (fp2 != fp || show(c2) != sc) {
                            bad += 1
                            failures += 1
                            +++("  MISMATCH after", k, "recorded actions")
                            (k - 4).until(k).%(_ >= 0).foreach(i => +++("    recorded", i, ":", recorded(i).unwrap.toString.take(300)))
                            fp.zipAll(fp2, "", "").filter(x => x._1 != x._2).take(8).foreach { case (a, b) => +++("    live:   ", a.take(400)); +++("    rebuilt:", b.take(400)) }
                            if (show(c2) != sc) { +++("    live continue:   ", sc.take(400)); +++("    rebuilt continue:", show(c2).take(400)) }
                        }
                    }
                    catch {
                        case e : Throwable =>
                            bad += 1
                            failures += 1
                            +++("  rebuild of first", k, "actions threw:", e)
                            e.getStackTrace.take(12).foreach(s => +++("     ", s))
                    }
                }
            }
            +++("  checked", points.num, "prefixes", (bad == 0).?("ok").|(""))
        }

        +++("failures:", failures)
    }
}
