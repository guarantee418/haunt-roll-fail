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
// rankings existed, in the format of results.tsv. A side is "W" or "L" and the users on it, "W:user,user;L:user".
// A seat with no claimed name counts under the name typed for it in the lobby ("name <user> <name>").
object Ranking {
    import Backup.{esc, unesc, rows, row}

    val dir = Paths.get("ranking")
    val names = dir.resolve("names.tsv")
    val seats = dir.resolve("seats.tsv")
    val results = dir.resolve("results.tsv")
    val seed = Paths.get("ranking-seed.tsv")

    def sha(s : String) = java.security.MessageDigest.getInstance("SHA-256").digest(s.getBytes(UTF_8)).map("%02x".format(_)).mkString

    def append(path : Path, line : String) = {
        Files.createDirectories(path.getParent)
        Files.write(path, (line + "\n").getBytes(UTF_8), java.nio.file.StandardOpenOption.CREATE, java.nio.file.StandardOpenOption.APPEND)
    }

    def now() = java.time.Instant.now().toString.take(19)

    def clean(name : String) = name.split(' ').filter(_ != "").mkString(" ").take(32)

    case class Claim(key : String, display : String, hash : String)

    def claims() : Map[String, Claim] = rows(names).collect { case k :: d :: h :: _ => k -> Claim(k, d, h) }.toMap

    def seatNames() : Map[String, String] = rows(seats).collect { case u :: k :: _ => u -> k }.toMap

    // "ok\n<name as claimed>" when the name is free (now claimed with this key) or already claimed with it, else "taken";
    // the seat then plays under it
    def claim(user : String, name : String, key : String) : String = synchronized {
        val n = clean(name)
        val k = n.toLowerCase

        if (n.isEmpty || key.length < 16)
            return "invalid"

        val c = claims().get(k) match {
            case Some(c) if c.hash == sha(key) => c
            case Some(_) => return "taken"
            case None =>
                val c = Claim(k, n, sha(key))
                append(names, row(c.key, c.display, c.hash, now()))
                c
        }

        if (!seatNames().get(user).contains(k))
            append(seats, row(user, k))

        "ok\n" + c.display
    }

    // the names claimed with this key
    def owned(key : String) : List[String] = synchronized {
        val h = sha(key)
        claims().values.filter(_.hash == h).map(_.display).toList.sorted
    }

    def recorded() : List[List[String]] = rows(seed) ++ rows(results)

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
        val users = rr.flatMap(r => r(4).split(';').toList.flatMap(_.drop(2).split(',').toList)).filter(_.nonEmpty).toSet
        val seated = seatNames()
        val cc = claims()
        val typedNames = typed(users.filter(u => !seated.contains(u)))

        def nameOf(u : String) = seated.get(u).orElse(typedNames.get(u).map(clean(_).toLowerCase)).map(k => cc.get(k).map(_.display).getOrElse(typedNames.get(u).map(clean).filter(_.toLowerCase == k).getOrElse(k)))

        rr.flatMap { r =>
            val sides = r(4).split(';').toList.filter(_.length > 2).map { s =>
                s.take(2) + s.drop(2).split(',').toList.flatMap(nameOf).map(_.replace(",", " ").replace(";", " ")).mkString(",")
            }.filter(_.length > 2)

            if (sides.size >= 2 && sides.exists(_.startsWith("W:")) && sides.exists(_.startsWith("L:")))
                Some(List(r(0), r(2), r(3), sides.mkString(";")).map(esc).mkString("\t"))
            else
                None
        }.mkString("\n")
    }
}
