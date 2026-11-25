package hrf
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

import hrf.meta._
import hrf.web._


trait Journal[T] {
    val meta : MetaGame

    def known : Int

    def read(n : Int)(then : $[T] => Unit) : Unit
    def write(n : Int, message : T)(then : => Unit)(fail : => Unit) : Unit = write(n, message, None)(then)(fail)
    def write(n : Int, message : T, comment : |[String])(then : => Unit)(fail : => Unit) : Unit
    def append(message : T, comment : |[String] = None, retries : Int = 9)(then : => Unit)(fail : => Unit) : Unit = write(known, message, comment)(then) {
        if (retries > 0)
            read(known) { l =>
                append(message, comment, retries - 1)(then)(fail)
            }
        else
            fail
    }

    // tells the server whose turn it is, once the whole log is read (good-game/Turns.scala); only online games have a server
    // (and the game's overview, the icons Play Online's games list shows, Meta.overview);
    // fan: the game has fan content and so counts toward no rating (MetaGame.rankingFan), for the Unranked Games folder
    def turn(factions : $[String], reasons : $[String] = $, overview : $[String] = $, fan : Boolean = false, status : $[String] = $, jests : $[String] = $) {}
}


// A page loaded before a deploy kept running the old client, and its turn reports went on saying what the old code said (2026-11-22:
// a ping for an attack named no attacker, from the attacker's page left open since before the deploy). An online game's page asks the
// server for the client build's fingerprint (good-game's /client-version) when it starts, and again before each turn report and each
// time it comes back into view; when the fingerprint has changed it reloads itself, which brings it back to the same game with the new
// client, and sends no report from the old one. Nothing is lost: an online game's moves are on the server.
object ClientVersion {
    private var server = ""
    private var loaded : |[String] = None
    private var reloading = false

    def start(s : String) {
        if (server == "") {
            server = s
            getF(server + "/client-version") { v => if (v.trim.nonEmpty) loaded = |(v.trim) } { }
            scala.util.Try {
                val d = scalajs.js.Dynamic.global.document
                d.addEventListener("visibilitychange", { (e : scalajs.js.Any) =>
                    if (!d.hidden.asInstanceOf[Boolean])
                        check { }
                } : scalajs.js.Function1[scalajs.js.Any, Unit])
            }
        }
    }

    // then: the client is current (or the server can't say)
    def check(then : => Unit) {
        if (reloading)
            ()
        else
        if (server == "" || loaded.none)
            then
        else
            getF(server + "/client-version?" + scala.util.Random.nextInt(1000000)) { v =>
                if (v.trim.nonEmpty && !loaded.has(v.trim)) {
                    reloading = true
                    scala.util.Try(scalajs.js.Dynamic.global.location.reload())
                }
                else
                    then
            } { then }
    }
}


class ServerJournal[T](val meta : MetaGame, server : String, user : String, secret : String, val journal : String, parse : String => T, write : T => String, readLimit : Int = 999999) extends Journal[T] {
    var size : Int = 0

    def known = size

    ClientVersion.start(server)

    // the page hidden or not in front: the server doesn't count this read as the player having the game open, so their turn
    // pings still come (good-game/TurnPings.scala; 2026-11-22: a tab left open in the background read the log every minute or so,
    // and its player was never pinged)
    private def away : Boolean = scala.util.Try {
        val d = scalajs.js.Dynamic.global.document
        d.hidden.asInstanceOf[Boolean] || !d.hasFocus().asInstanceOf[Boolean]
    }.getOrElse(false)

    def read(n : Int)(then : $[T] => Unit) {
        get(server + "/read/" + user + "/" + secret + "/" + journal + "/" + n + away.??("?away=1")) { l =>
            val messages = l.split('\n').$.but("").take(readLimit - n)./(parse)
            size = n + messages.num
            then(messages)
        }
    }

    def write(n : Int, message : T, comment : |[String])(then : => Unit)(fail : => Unit) {
        val prefix = comment./("// " + _ + "\n").|("")
        postF(server + "/append/" + user + "/" + secret + "/" + journal + "/" + n, prefix + write(message))(_ => { size += 1 ; then})(fail)
    }

    private var lastTurn = ""

    // (the third line, "<seat>=<reason> ...": why each is asked, Meta.turnReason)
    // (the fourth, the game's overview: icon names, Meta.overview; 2026-11-22; the fifth, "fan", a game with fan content; the sixth, its
    // progress, Meta.status; empty lines at the end left out)
    override def turn(factions : $[String], reasons : $[String] = $, overview : $[String] = $, fan : Boolean = false, status : $[String] = $, jests : $[String] = $) {
        val report = $(size.toString, factions.join(" "), reasons.join(" "), overview.join(" "), fan.??("fan"), status.join(" "), jests.join(" ")).reverse.dropWhile(_.isEmpty).reverse.join("\n")
        if (report != lastTurn) {
            lastTurn = report
            ClientVersion.check {
                postF(server + "/turn/" + user + "/" + secret + "/" + journal, report)(_ => ()) { lastTurn = "" }
            }
        }
    }
}


class ServerPhantomJournal[T](val meta : MetaGame, val server : String, user : String, secret : String, journal : String, parse : String => T, write : T => String, readLimit : Int = 999999) extends Journal[T] {
    var actions = $[T]()

    def known = actions.num

    def read(n : Int)(then : $[T] => Unit) {
        if (actions.any)
            then(actions.drop(n))
        else
            get(server + "/read/" + user + "/" + secret + "/" + journal + "/" + 0) { l =>
                actions = l.split('\n').$.but("").take(readLimit - n)./(parse)
                then(actions.drop(n))
            }
    }

    def write(n : Int, message : T, comment : |[String])(then : => Unit)(fail : => Unit) {
        if (actions.none)
            fail
        else
        if (actions.num == n) {
            actions :+= message
            then
        }
        else
            fail
    }
}


class MemoryJournal[T](val meta : MetaGame) extends Journal[T] {
    var actions = $[T]()

    def known = actions.num

    def read(n : Int)(then : $[T] => Unit) {
        then(actions.drop(n))
    }

    def write(n : Int, message : T, comment : |[String])(then : => Unit)(fail : => Unit) {
        if (actions.num == n) {
            actions :+= message
            then
        }
        else
            fail
    }
}


// A local game saved in the browser (Northgard's solo games): the moves are kept under one key, a line each, so the game can be continued later
// kind: the kind of local game (solo, quick, hotseat), so Play Again saves the next one the same way
class LocalStorageJournal[T](val meta : MetaGame, key : String, parse : String => T, serialize : T => String, val kind : String) extends Journal[T] {
    private var text = Local.get(key, "")
    var actions = text.split('\n').$.but("")./(parse)

    def known = actions.num

    def read(n : Int)(then : $[T] => Unit) {
        then(actions.drop(n))
    }

    def write(n : Int, message : T, comment : |[String])(then : => Unit)(fail : => Unit) {
        if (actions.num == n) {
            actions :+= message
            text = text.some./(_ + "\n").|("") + serialize(message)

            // Storage full or unavailable: the game goes on, it just can't be continued later
            try {
                Local.set(key, text)
            }
            catch {
                case e : Throwable => println("local game not saved: " + e)
            }

            then
        }
        else
            fail
    }
}


class ReplayJournal[T](val meta : MetaGame, data : String, parse : String => T, readLimit : Int = 999999) extends Journal[T] {
    val actions = data.split('\n').$./(_.trim).but("")./(parse)

    def known = actions.num

    def read(n : Int)(then : $[T] => Unit) {
        then(actions.drop(n).take(readLimit - n))
    }

    def write(n : Int, message : T, comment : |[String])(then : => Unit)(fail : => Unit) {
        fail
    }
}


class ReplayPhantomJournal[T](val meta : MetaGame, data : String, parse : String => T, readLimit : Int = 999999) extends Journal[T] {
    var actions = data.split('\n').$./(_.trim).but("")./(parse)

    def known = actions.num

    def read(n : Int)(then : $[T] => Unit) {
        then(actions.drop(n).take(readLimit - n))
    }

    def write(n : Int, message : T, comment : |[String])(then : => Unit)(fail : => Unit) {
        if (actions.none)
            fail
        else
        if (actions.num == n) {
            actions :+= message
            then
        }
        else
            fail
    }
}
