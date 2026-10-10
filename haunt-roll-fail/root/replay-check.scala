package root
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
// checks that rebuilding the game from a prefix of them (what undo does, via
// performVoid) gives the same state as the live game at that point.
object ReplayCheck {
    var marsh = false
    var exiles = false
    var oldFrogs = false

    def options(seating : $[Faction]) : $[Meta.O] = marsh.?($[Meta.O](
        MarshMap, AllRandomClearings, SetupTypeCorners, MixedDeck, AdSetBuffOn, NoHirelings,
        MouseholdLandmark, FoxburrowLandmark, RabbittownLandmark, SeatingGiven, FactionSeatingGiven, SetupOrderPriority, CardDraftStandard,
    )).|($[Meta.O](
        AutumnMap, DefaultClearings, SetupTypeCorners, MixedDeck, AdSetBuffOn, NoHirelings,
        FerryLandmark, LostCityLandmark, SeatingGiven, FactionSeatingGiven, SetupOrderPriority, CardDraftStandard,
    )).map(o => (exiles && o == MixedDeck).?[Meta.O](ExilesDeck).|(o)) ++ exiles.$(ErrataCoffinMakers) ++ oldFrogs.not.$(FrogCardsAfterStartingHands) ++ seating./(IncludeFaction)

    def privateMap(o : AnyRef, suffix : String) : Map[Any, Any] = {
        val f = o.getClass.getDeclaredFields.filter(_.getName.endsWith(suffix)).head
        f.setAccessible(true)
        f.get(o).asInstanceOf[Map[Any, Any]]
    }

    def fingerprint(g : Game) : String = {
        val keys : $[(Faction, Region)] = g.pieces.keys
        val p = keys.map((k : (Faction, Region)) => k.toString + " = " + g.pieces.get(k)./(_.toString).sorted.mkString(",")).sorted
        val c = privateMap(g.cards, "l2e").toList./{ case (k, v) => k.toString + " = " + v.asInstanceOf[$[Any]]./(_.toString).mkString(",") }.sorted
        val v = g.states.toList./{ case (f, s) => f.toString + " vp " + s.vp }.sorted
        def fields(o : AnyRef) : $[String] = {
            var cl : Class[_] = o.getClass
            var r = $[String]()
            while (cl != null && cl.getName.startsWith("root")) {
                cl.getDeclaredFields.foreach { fd =>
                    if (java.lang.reflect.Modifier.isStatic(fd.getModifiers).not) {
                        fd.setAccessible(true)
                        val x = fd.get(o)
                        x match {
                            case _ : Int | _ : Boolean | _ : Option[_] | _ : List[_] | _ : String => r :+= cl.getSimpleName + "." + fd.getName + " = " + x.toString.replaceAll("@[0-9a-f]+", "").take(300)
                            case _ =>
                        }
                    }
                }
                cl = cl.getSuperclass
            }
            r
        }
        val st = g.states.toList.sortBy(_._1.toString)./~{ case (f, s) => fields(s.asInstanceOf[AnyRef])./(f.toString + " " + _) }
        (p ++ c ++ v ++ st :+ ("repeat = " + g.repeat)).mkString("\n")
    }

    def show(c : Continue) : String = show0(c).replaceAll("Lambda[^,) ]*", "Lambda").replaceAll("@[0-9a-f]+", "")

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

    def rebuild(seating : $[Faction], recorded : $[ExternalAction]) : (Game, Continue) = {
        val g = new Game(seating, seating, options(seating))
        g.logging = false
        recorded.dropRight(1).foreach(g.performVoid)
        val c = settle(g, g.performContinue(|(g.continue), recorded.last, false).nest)
        (g, c)
    }

    def main(args : Array[String]) {
        val games = args.lift(0)./(_.toInt).|(6)
        marsh = args.exists(_.startsWith("marsh"))
        exiles = args.contains("exiles")
        oldFrogs = args.contains("oldfrogs")
        val pool : $[$[Faction]] = args.lift(1)./(_ match {
            case "base" => $($(MC, ED, WA, VB), $(MC, ED, WA, LC), $(MC, ED, RF, UD))
            case "ld" => $($(MC, ED, WA, LDvE))
            case "tc" => $($(MC, ED, WA, TC))
            case "kd" => $($(MC, ED, WA, KD))
            case "ldkd" => $($(MC, ED, LDvE, KD), $(MC, WA, LDvE, KD))
            case "marsh5" => $($(MC, ED, WA, VB, LDvE), $(MC, ED, WA, KD, TC))
            case "marsh" => $($(MC, ED, WA, VB), $(MC, ED, WA, LDvE), $(MC, WA, KD, TC))
        }).|($($(MC, WA, LDvE, KD), $(MC, ED, LDvE, KD), $(MC, WA, KD, TC), $(MC, WA, LDvE, TC)))
        var failures = 0

        0.until(games).foreach { gi =>
            val seating = pool(gi % pool.num).shuffle
            val game = new Game(seating, seating, options(seating))
            game.logging = false

            var recorded : $[ExternalAction] = $
            var snapshots : Map[Int, (String, String)] = Map()

            def record(a : Action) = a match {
                case a : ExternalAction => recorded :+= a
                case a => throw new Error("not external " + a)
            }

            var next : Action = StartAction(root.version)
            record(next)
            var steps = 0
            var over = false
            var handsChecked = false

            try {
                while (over.not && steps < 6000) {
                    steps += 1
                    val before = recorded.num
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
                            val a = if (l.num == 1) l(0) else new BotXX(f).ask(l, 0)(game).immediate
                            chosen = |(a)
                            a match {
                                case _ if a.isSoft =>
                                case _ : DontRecord =>
                                case _ => record(a)
                            }
                        case x => throw new Error("unhandled continue " + x)
                    }

                    chosen.foreach(next = _)

                    // no starting hand may hold Lilypad Diaspora's Frog cards
                    if (handsChecked.not && oldFrogs.not && game.turn > 0) {
                        handsChecked = true
                        implicit val g : Game = game
                        game.factions.foreach { f =>
                            if (game.states.get(f).exists(_ => f.hand.exists(Deck.frogEEE.has)))
                                throw new Error(f.name + " began with a Frog card: " + f.hand.$)
                        }
                    }
                }
            }
            catch {
                case e : Throwable => +++("game", gi, "live play failed:", e); failures += 1
            }

            +++("game", gi, seating./(_.name).mkString(", "), "-", recorded.num, "recorded actions", over.?("finished").|("stopped"))

            var bad = 0
            val points = (if (args.lift(2).has("dense")) 1.until(math.min(recorded.num, 400)).$ else (1.until(recorded.num).by(math.max(1, recorded.num / 80)).$ :+ (recorded.num - 1))).distinct.%(snapshots.contains)
            points.foreach { k =>
                if (bad < 3) {
                    try {
                        val (g2, c2) = rebuild(seating, recorded.take(k))
                        val (fp, sc) = snapshots(k)
                        val fp2 = fingerprint(g2)
                        if (fp2 != fp || show(c2) != sc) {
                            bad += 1
                            failures += 1
                            +++("  MISMATCH after", k, "recorded actions")
                            (k - 4).until(k).%(_ >= 0).foreach(i => +++("    recorded", i, ":", recorded(i).unwrap.toString.take(300)))
                            fp.split("\n").toList.zipAll(fp2.split("\n").toList, "", "").filter(x => x._1 != x._2).take(8).foreach { case (a, b) => +++("    live:   ", a); +++("    rebuilt:", b) }
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
        }

        +++("failures:", failures)
    }
}
