package hrf.gg

import java.nio.file.{Path, Paths}

// Whose turn it is in each online game (2026-11-20, so the player's active games list can highlight those waiting on them).
// The server can't work it out, as it doesn't run the games: each client, once it has read the whole log and reached a
// question, posts which seats (factions as the lobby's "user" lines write them) are asked, with the number of log entries it
// has read (POST /turn/<user>/<secret>/<game journal>). A report for fewer entries than the last one kept is dropped, unless
// the log has since been shortened by a fix.
//
// File turns.tsv in the server's working directory: game journal, entries, the factions asked (space-separated).
object Turns {
    import Backup.{rows, row, write, lines}

    val file : Path = Paths.get("turns.tsv").toAbsolutePath

    private var turns : Map[String, (Int, List[String])] =
        rows(file).collect { case j :: n :: ff :: _ if n.forall(_.isDigit) && n.nonEmpty => j -> (n.toInt, ff.split(" ").toList.filter(_.nonEmpty)) }.toMap

    // entries: the number of entries the game's log has now
    def report(journal : String, n : Int, factions : List[String], entries : => Int) : Unit = synchronized {
        val keep = turns.get(journal) match {
            case Some((m, _)) if m > n => m > entries
            case _ => true
        }

        if (keep && !turns.get(journal).contains((n, factions))) {
            turns += journal -> (n, factions)
            TurnPings.report(journal, n, factions)
            write(file, lines(turns.toList.sortBy(_._1).map { case (j, (n, ff)) => row(j, n, ff.mkString(" ")) }))
        }
    }

    def asked(journal : String) : List[String] = synchronized { turns.get(journal).map(_._2).getOrElse(Nil) }
}
