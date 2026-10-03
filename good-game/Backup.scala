package hrf.gg

import slick.jdbc.HsqldbProfile.api._

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path, Paths, StandardCopyOption}

import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.jdk.CollectionConverters._

// Copies every game to a directory (a git clone that live-server.sh pushes to
// a private GitHub repository) and applies fixes committed there.
//
//   journals.tsv     id, name, public, status, message, entries
//   users.tsv        id, name, secret
//   access.tsv       journal, user, right
//   plays.tsv        journal, user, secret (the player's link is /play/<meta>/<secret>)
//   games/<id>.tsv   "# game <id> entries <n>", then index, user, text
//   fixes/<name>.tsv a corrected copy of games/<id>.tsv with its first line unchanged;
//                    applied if the game still has <n> entries. The server writes
//                    the outcome to fixes/<name>.result and the replaced log to
//                    fixes/<name>.before.tsv, and never changes or deletes the
//                    files people commit, so git merges stay simple.
//   fixes/applied.log the fixes seen so far (by hash), so none is tried twice
//
// Fields are separated by tabs; tabs, newlines and backslashes in them are escaped.
// "restore" rebuilds a database from such a directory.
object Backup {
    import GoodGame._

    def esc(s : String) = s.flatMap {
        case '\\' => "\\\\"
        case '\t' => "\\t"
        case '\n' => "\\n"
        case '\r' => "\\r"
        case c => c.toString
    }

    def unesc(s : String) = {
        val b = new StringBuilder
        var i = 0
        while (i < s.length) {
            if (s(i) == '\\' && i + 1 < s.length) {
                b += (s(i + 1) match {
                    case 't' => '\t'
                    case 'n' => '\n'
                    case 'r' => '\r'
                    case c => c
                })
                i += 2
            }
            else {
                b += s(i)
                i += 1
            }
        }
        b.toString
    }

    def row(fields : Any*) = fields.map(f => esc(f.toString)).mkString("\t")

    def rows(path : Path) : List[List[String]] =
        if (Files.exists(path))
            Files.readAllLines(path, UTF_8).asScala.toList.filter(l => l.nonEmpty && !l.startsWith("#")).map(_.split("\t", -1).toList.map(unesc))
        else
            Nil

    def header(id : String, n : Int) = "# game " + id + " entries " + n

    val Header = "# game ([a-z]+) entries ([0-9]+)".r

    def read(path : Path) = new String(Files.readAllBytes(path), UTF_8)

    // Writes only when the content changed, through a temporary file
    def write(path : Path, content : String) {
        if (!Files.exists(path) || read(path) != content) {
            Files.createDirectories(path.getParent)
            val tmp = path.resolveSibling(path.getFileName.toString + ".tmp")
            Files.write(tmp, content.getBytes(UTF_8))
            Files.move(tmp, path, StandardCopyOption.REPLACE_EXISTING, StandardCopyOption.ATOMIC_MOVE)
        }
    }

    def lines(l : Iterable[String]) = l.map(_ + "\n").mkString

    def sha256(s : String) = java.security.MessageDigest.getInstance("SHA-256").digest(s.getBytes(UTF_8)).map("%02x".format(_)).mkString

    def now() = java.time.LocalDateTime.now().withNano(0).toString


    class Exporter(db : Database, val dir : Path) {
        implicit val ec : scala.concurrent.ExecutionContext = scala.concurrent.ExecutionContext.global

        def run[R](a : DBIOAction[R, NoStream, Nothing]) : R = Await.result(db.run(a), Duration.Inf)

        // Entry count and last index of each exported game
        var exported = Map[String, (Int, Int)]()

        def gameLog(id : String) : (Int, String) = {
            val l = run(entries.filter(_.journalId === id).sortBy(_.index).result)
            (l.size, lines(header(id, l.size) +: l.map(e => row(e.index, e.userId, e.text))))
        }

        def export() {
            val js = run(journals.result)
            val stats = run(entries.groupBy(_.journalId).map { case (j, q) => (j, q.length, q.map(_.index).max) }.result).map {
                case (j, n, last) => j -> (n, last.getOrElse(-1))
            }.toMap

            write(dir.resolve("journals.tsv"), lines(row("# id", "name", "public", "status", "message", "entries") +:
                js.map(j => row(j.id, j.name, j.public, j.status, j.message, stats.get(j.id).map(_._1).getOrElse(0)))))
            write(dir.resolve("users.tsv"), lines(row("# id", "name", "secret") +:
                run(users.sortBy(_.id).result).map(u => row(u.id, u.name, u.secret))))
            write(dir.resolve("access.tsv"), lines(row("# journal", "user", "right") +:
                run(accessRights.sortBy(a => (a.journalId, a.userId, a.right)).result).map(a => row(a.journalId, a.userId, a.right))))
            write(dir.resolve("plays.tsv"), lines(row("# journal", "user", "secret") +:
                run(plays.sortBy(p => (p.journalId, p.userId)).result).map(p => row(p.journalId, p.userId, p.secret))))

            js.foreach { j =>
                val s = stats.getOrElse(j.id, (0, -1))
                val path = dir.resolve("games").resolve(j.id + ".tsv")

                if (exported.get(j.id) != Some(s) || !Files.exists(path)) {
                    write(path, gameLog(j.id)._2)
                    exported += j.id -> s
                }
            }
        }

        // Every fix seen, applied or not, so none is tried twice
        def ledger = dir.resolve("fixes").resolve("applied.log")

        def seen = if (Files.exists(ledger)) Files.readAllLines(ledger, UTF_8).asScala.map(_.takeWhile(_ != ' ')).toSet else Set[String]()

        def fix(path : Path) {
            val content = read(path)
            val hash = sha256(content)

            if (seen.contains(hash))
                return

            val name = path.getFileName.toString.stripSuffix(".tsv")
            val before = path.resolveSibling(name + ".before.tsv")

            val result = try {
                val (id, base) = content.linesIterator.nextOption() match {
                    case Some(Header(id, n)) => (id, n.toInt)
                    case _ => throw new Exception("the first line must be the first line of games/<id>.tsv, \"# game <id> entries <n>\"")
                }

                val replacement = rows(path).zipWithIndex.map {
                    case (List(index, user, text), _) if index.toIntOption.exists(_ >= 0) => Entry(id, index.toInt, user, text)
                    case (r, i) => throw new Exception("line " + (i + 2) + " is not index, user and text separated by tabs: " + r.mkString(" | ").take(200))
                }

                if (replacement.map(_.index).distinct.size != replacement.size)
                    throw new Exception("an index appears twice")

                if (run(journals.filter(_.id === id).exists.result) == false)
                    throw new Exception("there is no game " + id)

                val (n, old) = gameLog(id)

                if (n != base)
                    throw new Exception("the game has " + n + " entries now, not " + base + "; copy games/" + id + ".tsv again")

                run((for {
                    m <- entries.filter(_.journalId === id).length.result
                    _ <- if (m == base) DBIO.successful(()) else DBIO.failed(new Exception("the game changed while the fix was applied"))
                    _ <- entries.filter(_.journalId === id).delete
                    _ <- entries ++= replacement
                } yield ()).transactionally)

                write(before, old)
                exported -= id

                "Applied: " + base + " entries replaced with " + replacement.size + ". The old log is in " + before.getFileName + ". Players should reload the game."
            }
            catch {
                case e : Exception => "Not applied: " + e.getMessage
            }

            write(path.resolveSibling(name + ".result"), now() + " " + result + "\n")
            Files.write(ledger, (hash + " " + now() + " " + name + " " + result.takeWhile(_ != ':') + "\n").getBytes(UTF_8), java.nio.file.StandardOpenOption.CREATE, java.nio.file.StandardOpenOption.APPEND)
            println("Game fix " + name + ": " + result)
        }

        def fixes() {
            val d = dir.resolve("fixes")
            if (Files.isDirectory(d))
                Files.list(d).iterator.asScala.toList.filter(p => Files.isRegularFile(p) && p.toString.endsWith(".tsv") && !p.toString.endsWith(".before.tsv")).sorted.foreach(fix)
        }

        def tick() {
            List[(String, () => Unit)]("fixes" -> (() => fixes()), "export" -> (() => export())).foreach { case (what, f) =>
                try f()
                catch {
                    case e : Exception => println("Backup " + what + " failed: " + e)
                }
            }
        }
    }

    // The directory comes from HRF_BACKUP_DIR or the file backup-dir in the working directory
    def directory : Option[Path] =
        sys.env.get("HRF_BACKUP_DIR").map(_.trim).filter(_.nonEmpty).orElse {
            val f = Paths.get("backup-dir")
            if (Files.exists(f)) Some(read(f).trim).filter(_.nonEmpty) else None
        }.map(Paths.get(_))

    // A full database copy (tar.gz) once a day in db-backups/, kept 14 days
    def daily(db : Database) {
        def run[R](a : DBIOAction[R, NoStream, Nothing]) : R = Await.result(db.run(a), Duration.Inf)

        val d = Paths.get("db-backups").toAbsolutePath
        Files.createDirectories(d)
        val today = java.time.LocalDate.now().toString
        val marker = d.resolve("last")

        if (!Files.exists(marker) || read(marker).trim != today) {
            run(sqlu"#${"BACKUP DATABASE TO '" + d + "/' BLOCKING"}")
            Files.write(marker, today.getBytes(UTF_8))
            println("Database backed up to " + d)
        }

        val old = System.currentTimeMillis() - 14L * 24 * 3600 * 1000
        Files.list(d).iterator.asScala.toList.filter(_.toString.endsWith(".tar.gz")).filter(Files.getLastModifiedTime(_).toMillis < old).foreach(Files.delete)
    }

    // Every minute: the daily database copy, and the games copy if a directory is set
    // (read each time, so setting one needs no restart)
    def start(db : Database) {
        var exporter : Option[Exporter] = None

        val thread = new Thread(() => {
            while (true) {
                try daily(db)
                catch {
                    case e : Exception => println("Database backup failed: " + e)
                }

                directory.filter(Files.isDirectory(_)) match {
                    case Some(dir) =>
                        if (exporter.map(_.dir) != Some(dir)) {
                            exporter = Some(new Exporter(db, dir))
                            println("Copying games to " + dir)
                        }
                        exporter.foreach(_.tick())
                    case None =>
                        exporter = None
                }
                Thread.sleep(60 * 1000)
            }
        })
        thread.setDaemon(true)
        thread.start()
    }

    // Fill a new, empty database from an exported directory
    def restore(db : Database, dir : Path) {
        def run[R](a : DBIOAction[R, NoStream, Nothing]) : R = Await.result(db.run(a), Duration.Inf)

        val us = rows(dir.resolve("users.tsv")).map { case List(id, name, secret) => User(name, secret, id) }
        val js = rows(dir.resolve("journals.tsv")).map { case List(id, name, public, status, message, _) => Journal(name, public.toBoolean, status, message, id) }
        val as = rows(dir.resolve("access.tsv")).map { case List(j, u, r) => AccessRight(j, u, r) }
        val ps = rows(dir.resolve("plays.tsv")).map { case List(j, u, s) => Play(j, u, s) }

        run(DBIO.seq(users.schema.create, journals.schema.create, entries.schema.create, accessRights.schema.create, plays.schema.create))
        run(DBIO.seq(users ++= us, journals ++= js, accessRights ++= as, plays ++= ps).transactionally)

        var n = 0
        js.foreach { j =>
            val es = rows(dir.resolve("games").resolve(j.id + ".tsv")).map { case List(i, u, t) => Entry(j.id, i.toInt, u, t) }
            run(entries ++= es)
            n += es.size
        }

        println("Restored " + us.size + " users, " + js.size + " games, " + n + " entries.")
    }
}
