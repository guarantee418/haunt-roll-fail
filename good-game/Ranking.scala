package hrf.gg

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path, Paths}

// Player rankings (Northgard, asked for on 2026-11-18). There are no accounts: each seat of an online game is a user of its own,
// and the player types a name on joining. A name is claimed by the first browser that uses it: the browser keeps a random key
// (its "player code") and the server keeps the key's hash with the name, so only that browser (or one given the code) can play
// under it again. Results are reported by the clients when a game ends; the rating itself is worked out by the client
// from the list of results (GET /rankings/<meta>).
//
// Files, in ranking/ in the server's working directory (copied to the games backup by Backup):
//   names.tsv    name in lower case, name as claimed, SHA-256 of the key, time
//   seats.tsv    user (a seat of a game), name in lower case: the seat played under that claimed name
//   results.tsv  game journal, meta, time, title, sides
// ranking-seed.tsv in the working directory (committed in the repository) holds the results of games that ended before
// rankings existed, in the format of results.tsv; a seat "*" is no one (Root's games before rankings, 2026-11-18: their seats
// count in the faction and turn order stats, not in the ratings). A side is "W" or "L" and the users on it, "W:user,user;L:user".
// A seat with no claimed name counts under the name typed for it in the lobby ("name <user> <name>"). Since 2026-11-18 a user may
// be followed by what the seat played, "user/faction/seat/players/score", for the faction and turn order stats.
//
// Names are released or moved by hand through the games backup (hrf-games): each file ranking-changes/<name>.txt there is applied
// once (by its hash, like the game fixes; Backup.scala), its outcome written to ranking-changes/<name>.result. One change a line:
//   release <name>         the name is free again (its games stay under it, for whoever claims it next)
//   rename <name> -> <new> the claim and the games its seats played move to the new name, which must be free; the old name
//                          is free again (seats that played under the name typed, never claimed, stay: "seat" moves them)
//   seat <user> <name>     a seat counts under that name (claimed or not) from now on
//   merge <name> -> <into> one player under two names (a second device that couldn't take the name, 2026-11-22): <into>, which
//                          must be claimed, gets every key of <name> (so those devices play under it), the seats that played
//                          under <name> move to it, and <name> is free again; <name> needn't be claimed (its seats still move)
// Names may have spaces; "#" starts a comment line. The files are only appended to: a release is a row with no key.
object Ranking {
    import Backup.{esc, unesc, rows, row}

    val dir = Paths.get("ranking")
    val names = dir.resolve("names.tsv")
    val seats = dir.resolve("seats.tsv")
    val results = dir.resolve("results.tsv")
    val codes = dir.resolve("codes.tsv")
    val requests = dir.resolve("requests.tsv")
    val seed = Paths.get("ranking-seed.tsv")

    def sha(s : String) = java.security.MessageDigest.getInstance("SHA-256").digest(s.getBytes(UTF_8)).map("%02x".format(_)).mkString

    def append(path : Path, line : String) = {
        Files.createDirectories(path.getParent)
        Files.write(path, (line + "\n").getBytes(UTF_8), java.nio.file.StandardOpenOption.CREATE, java.nio.file.StandardOpenOption.APPEND)
    }

    def now() = java.time.Instant.now().toString.take(19)

    def clean(name : String) = name.split(' ').filter(_ != "").mkString(" ").take(32)

    // a claimed name and the hashes of the keys (browsers) that may play under it: the one that claimed it, and any other a sign-in
    // with the name's own code added (2026-11-19)
    case class Claim(key : String, display : String, hashes : List[String])

    // each row with a key adds it to the name's keys; a row with no key is a release, which drops them all
    def claims() : Map[String, Claim] = rows(names).foldLeft(Map[String, Claim]()) {
        case (m, k :: d :: h :: _) if h.isEmpty => m - k
        case (m, k :: d :: h :: _) => m + (k -> m.get(k).map(c => Claim(k, d, (c.hashes :+ h).distinct)).getOrElse(Claim(k, d, List(h))))
        case (m, _) => m
    }

    def seatNames() : Map[String, String] = rows(seats).collect { case u :: k :: _ => u -> k }.toMap

    // the seats (users) under the given names, newest first, each with the name it plays under now
    def seatsOf(kk : Set[String]) : List[(String, String)] = {
        val now = seatNames()
        rows(seats).collect { case u :: _ => u }.reverse.distinct.flatMap(u => now.get(u).filter(kk.contains).map(u -> _))
    }

    // "ok\n<name as claimed>" when the name is free (now claimed with this key) or already claimed with it, else "taken";
    // the seat then plays under it
    def claim(user : String, name : String, key : String) : String = synchronized {
        val n = clean(name)
        val k = n.toLowerCase

        if (n.isEmpty || key.length < 16)
            return "invalid"

        val c = claims().get(k) match {
            case Some(c) if c.hashes.contains(sha(key)) => c
            case Some(_) => return "taken"
            case None =>
                val c = Claim(k, n, List(sha(key)))
                append(names, row(c.key, c.display, sha(key), now()))
                c
        }

        if (!seatNames().get(user).contains(k))
            append(seats, row(user, k))

        "ok\n" + c.display
    }

    // a name claimed with this key without a seat (the tournament's sign-up): "ok\n<name as claimed>", "taken" or "invalid"
    def claimName(name : String, key : String) : String = synchronized {
        val n = clean(name)
        val k = n.toLowerCase

        if (n.isEmpty || key.length < 16)
            return "invalid"

        claims().get(k) match {
            case Some(c) if c.hashes.contains(sha(key)) => "ok\n" + c.display
            case Some(_) => "taken"
            case None =>
                append(names, row(k, n, sha(key), now()))
                "ok\n" + n
        }
    }

    // the names claimed with this key
    def owned(key : String) : List[String] = synchronized {
        val h = sha(key)
        claims().values.filter(_.hashes.contains(h)).map(_.display).toList.sorted
    }

    // OWN CODES (2026-11-19). A player may pick a code of their own for the names their browser holds, to sign in with a name
    // and its code on another device (the browser's random key is still the player code). codes.tsv: name in lower case, a random
    // salt, SHA-256 of the salt and the code, time; the last row of a name wins, and an empty one (a release) clears it. A sign-in
    // adds the new browser's key to the name's keys. Five wrong codes for a name keep it from signing in for an hour.

    val minCode = 8

    def codeOf(k : String) : Option[(String, String)] = rows(codes).collect { case n :: s :: h :: _ if n == k => (s, h) }.lastOption.filter(_._2.nonEmpty)

    // "ok\n<names>" once the code is set for every name the key holds
    def setCode(key : String, code : String) : String = synchronized {
        val h = sha(key)
        val mine = claims().values.filter(_.hashes.contains(h)).toList.sortBy(_.key)
        if (code.length < minCode || code.length > 64) "short"
        else
        if (mine.isEmpty) "none"
        else {
            mine.foreach { c =>
                val salt = java.util.UUID.randomUUID().toString
                append(codes, row(c.key, salt, sha(salt + "\n" + code), now()))
            }
            "ok\n" + mine.map(_.display).mkString("\n")
        }
    }

    // whether a name has a code of its own
    def hasCode(name : String) : Boolean = synchronized { claims().contains(clean(name).toLowerCase) && codeOf(clean(name).toLowerCase).nonEmpty }

    val wrong = scala.collection.mutable.Map[String, List[Long]]()

    // "ok\n<name>" and the key may play under the name; "wrong", "locked" (five wrong codes in the last hour), "none" (no code set)
    def signIn(name : String, code : String, key : String) : String = synchronized {
        val k = clean(name).toLowerCase
        val now0 = System.currentTimeMillis()
        val tries = wrong.getOrElse(k, Nil).filter(_ > now0 - 3600 * 1000)
        wrong(k) = tries
        (claims().get(k), codeOf(k)) match {
            case _ if key.length < 16 => "invalid"
            case _ if tries.size >= 5 => "locked"
            case (Some(c), Some((salt, hash))) =>
                if (sha(salt + "\n" + code) == hash) {
                    wrong(k) = Nil
                    if (!c.hashes.contains(sha(key)))
                        append(names, row(k, c.display, sha(key), now()))
                    "ok\n" + c.display
                }
                else {
                    wrong(k) = now0 :: tries
                    "wrong"
                }
            case _ => "none"
        }
    }

    // RESET REQUESTS (2026-11-19): a player who lost their code asks for their name to be released. The request is kept in
    // requests.tsv (copied to the private games backup, as it names the player) and the owner decides; the name is released
    // through ranking-changes. What the owner needs to judge it: when the name was claimed, the games played under it, the last one
    def nameFacts(name : String) : String = synchronized {
        val k = clean(name).toLowerCase
        val claimed = rows(names).collect { case n :: _ :: h :: t :: _ if n == k && h.nonEmpty => t }
        val users = seatNames().filter(_._2 == k).keySet
        val played = recorded().filter(r => r.size >= 5 && r(4).split(';').exists(_.drop(2).split(',').exists(t => users.contains(t.takeWhile(_ != '/')))))
        if (!claims().contains(k)) "not claimed"
        else "claimed " + claimed.headOption.getOrElse("?") + ", " + claimed.size + " device(s), " + users.size + " seat(s), " + played.size + " ranked game(s)" + played.map(_(2)).sorted.lastOption.map(", last " + _).getOrElse("") + (if (codeOf(k).nonEmpty) ", own code set" else "")
    }

    def request(name : String, message : String) : String = synchronized {
        val n = clean(name)
        if (n.isEmpty) "invalid"
        else {
            val id = java.util.UUID.randomUUID().toString.take(8)
            append(requests, row(id, n, message.take(1000), nameFacts(n), now()))
            id
        }
    }

    // a seat's user and what it played, as reported (Elo.token): letters, digits and a few signs
    def cleanToken(s : String) = s.filter(c => c.isLetterOrDigit || "/_'().-!&".contains(c)).take(96)

    // the changes of a ranking-changes file, one a line; the outcome of each
    def change(text : String) : List[String] = synchronized {
        text.split('\n').toList.map(_.trim).filter(l => l.nonEmpty && !l.startsWith("#")).map { line =>
            def n(s : String) = clean(s)
            val (word, rest) = (line.takeWhile(_ != ' '), line.dropWhile(_ != ' ').trim)
            val result = (word, rest.split(" -> ").toList.map(_.trim)) match {
                case ("release", name :: Nil) =>
                    val k = n(name).toLowerCase
                    claims().get(k) match {
                        case Some(c) =>
                            append(names, row(k, c.display, "", now()))
                            append(codes, row(k, "", "", now()))
                            "released " + c.display
                        case None => "not done: " + n(name) + " is not claimed"
                    }
                case ("rename", from :: to :: Nil) =>
                    val (k, t) = (n(from).toLowerCase, n(to))
                    val cc = claims()
                    if (t.isEmpty) "not done: no new name"
                    else
                    if (t.toLowerCase != k && cc.contains(t.toLowerCase)) "not done: " + t + " is claimed"
                    else
                    cc.get(k) match {
                        case Some(c) =>
                            c.hashes.foreach(h => append(names, row(t.toLowerCase, t, h, now())))
                            if (t.toLowerCase != k) {
                                codeOf(k).foreach { case (salt, hash) => append(codes, row(t.toLowerCase, salt, hash, now())) }
                                append(names, row(k, c.display, "", now()))
                                append(codes, row(k, "", "", now()))
                                // only the seats that played under the claim: a seat that never claimed its typed name may be another
                                // player's (the one the name was taken from), and is moved with "seat"
                                seatNames().filter(_._2 == k).keys.toList.sorted.foreach(u => append(seats, row(u, t.toLowerCase)))
                            }
                            "renamed " + c.display + " to " + t
                        case None => "not done: " + n(from) + " is not claimed"
                    }
                case ("merge", from :: into :: Nil) =>
                    val (k, t) = (n(from).toLowerCase, n(into).toLowerCase)
                    val cc = claims()
                    if (k == t) "not done: the same name"
                    else
                    cc.get(t) match {
                        case None => "not done: " + n(into) + " is not claimed"
                        case Some(target) =>
                            val keys = cc.get(k).toList.flatMap(_.hashes).filterNot(target.hashes.contains)
                            keys.foreach(h => append(names, row(t, target.display, h, now())))
                            val moved = seatNames().filter(_._2 == k).keys.toList.sorted
                            moved.foreach(u => append(seats, row(u, t)))
                            cc.get(k).foreach { c =>
                                append(names, row(k, c.display, "", now()))
                                append(codes, row(k, "", "", now()))
                            }
                            "merged " + cc.get(k).map(_.display).getOrElse(n(from) + " (not claimed)") + " into " + target.display + ": " + keys.size + " device(s), " + moved.size + " seat(s)"
                    }
                case ("seat", u :: Nil) if u.contains(' ') && n(u.dropWhile(_ != ' ')).nonEmpty =>
                    val (user, name) = (u.takeWhile(_ != ' '), n(u.dropWhile(_ != ' ')))
                    append(seats, row(user, name.toLowerCase))
                    "seat " + user + " counts as " + name
                case _ => "not done: not a change (release, rename, merge or seat)"
            }
            line + ": " + result
        }
    }

    def recorded() : List[List[String]] = rows(seed) ++ rows(results)

    // every name as claimed, released ones too
    def displays() : Map[String, String] = rows(names).collect { case k :: d :: _ => k -> d }.toMap

    // the first report of a game is kept
    def report(game : String, meta : String, title : String, sides : String) : Boolean = synchronized {
        if (recorded().exists(_.headOption.contains(game)))
            false
        else {
            append(results, row(game, meta, now(), title, sides))
            true
        }
    }

    // game, time, title, sides with names: "W:Marzaus,gg;L:GCZ"; typed: user -> the name typed for it in its lobby
    def list(metas : Set[String], typed : Set[String] => Map[String, String]) : String = synchronized {
        val rr = recorded().filter(r => r.size >= 5 && metas.contains(r(1)))
        val users = rr.flatMap(r => r(4).split(';').toList.flatMap(_.drop(2).split(',').toList.map(_.takeWhile(_ != '/')))).filter(_.nonEmpty).toSet
        val seated = seatNames()
        val cc = displays()
        val typedNames = typed(users.filter(u => !seated.contains(u)))

        def nameOf(u : String) = seated.get(u).orElse(typedNames.get(u).map(clean(_).toLowerCase)).map(k => cc.get(k).getOrElse(typedNames.get(u).map(clean).filter(_.toLowerCase == k).getOrElse(k)))

        rr.flatMap { r =>
            val sides = r(4).split(';').toList.filter(_.length > 2).map { s =>
                // "X:fan", a game with fan content, as it is
                if (s.startsWith("X:")) s else
                s.take(2) + s.drop(2).split(',').toList.flatMap { t =>
                    val u = t.takeWhile(_ != '/')
                    (if (u == "*") Some("*") else nameOf(u)).map(_.replace(",", " ").replace(";", " ").replace("/", " ") + t.drop(u.length))
                }.mkString(",")
            }.filter(_.length > 2)

            // a game with a bot in it counts nowhere, ratings or stats (owner, 2026-11-21): its lobby has a "bot <seat> <level>" line
            // (games before this were rated between their people, leaving the bots out); TurnPings.lobbyOf keeps the lobbies read
            val bot = scala.util.Try(TurnPings.lobbyOf(r(0)).exists(_.lines.exists(_.startsWith("bot ")))).getOrElse(false)

            if (!bot && sides.size >= 2 && sides.exists(_.startsWith("W:")) && sides.exists(_.startsWith("L:")))
                Some(List(r(0), r(2), r(3), sides.mkString(";")).map(esc).mkString("\t"))
            else
                None
        }.mkString("\n")
    }
}
