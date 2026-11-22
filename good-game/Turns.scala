package hrf.gg

import java.nio.file.{Path, Paths}

// Whose turn it is in each online game (2026-11-20, so the player's active games list can highlight those waiting on them).
// The server can't work it out, as it doesn't run the games: each client, once it has read the whole log and reached a
// question, posts which seats (factions as the lobby's "user" lines write them) are asked, with the number of log entries it
// has read (POST /turn/<user>/<secret>/<game journal>). A report for fewer entries than the last one kept is dropped, unless
// the log has since been shortened by a fix.
//
// File turns.tsv in the server's working directory: game journal, entries, the factions asked (space-separated), and why each is
// asked ("<faction>=<reason>", space-separated; 2026-11-21, the Discord turn pings' reasons, TurnPings.reasonOf), and "fan" for a
// game with fan content, which counts toward no rating (MetaGame.rankingFan; the report's fifth line), for the Unranked Games folder
// of Play Online (2026-11-21).
object Turns {
    import Backup.{rows, row, write, lines}

    val file : Path = Paths.get("turns.tsv").toAbsolutePath

    private var turns : Map[String, (Int, List[String])] =
        rows(file).collect { case j :: n :: ff :: _ if n.forall(_.isDigit) && n.nonEmpty => j -> (n.toInt, ff.split(" ").toList.filter(_.nonEmpty)) }.toMap

    private var reasons : Map[String, Map[String, String]] =
        rows(file).collect { case j :: _ :: _ :: rr :: _ => j -> rr.split(" ").toList.map(_.split("=", 2).toList).collect { case s :: r :: Nil => s -> r }.toMap }.toMap

    private var fans : Set[String] =
        rows(file).collect { case j :: _ :: _ :: _ :: "fan" :: _ => j }.toSet

    // entries: the number of entries the game's log has now
    def report(journal : String, n : Int, factions : List[String], entries : => Int, why : Map[String, String] = Map(), fan : Boolean = false) : Unit = synchronized {
        val keep = turns.get(journal) match {
            case Some((m, _)) if m > n => m > entries
            case _ => true
        }

        // (the reasons kept with the report they came with; a report of the same turn with reasons, from a newer client, adds them)
        val again = turns.get(journal).contains((n, factions))
        if (keep && (!again || (why.nonEmpty && !reasons.get(journal).contains(why)) || fans.contains(journal) != fan)) {
            turns += journal -> (n, factions)
            reasons += journal -> why
            fans = if (fan) fans + journal else fans - journal
            if (!again)
                TurnPings.report(journal, n, factions)
            write(file, lines(turns.toList.sortBy(_._1).map { case (j, (n, ff)) => row(j, n, ff.mkString(" "), reasons.getOrElse(j, Map()).toList.sorted.map { case (s, r) => s + "=" + r }.mkString(" "), if (fans.contains(j)) "fan" else "") }))
        }
    }

    def asked(journal : String) : List[String] = synchronized { turns.get(journal).map(_._2).getOrElse(Nil) }

    // why a seat is asked in the last report kept ("defend/MC", ...), if its client said
    def reason(journal : String, seat : String) : Option[String] = synchronized { reasons.get(journal).flatMap(_.get(seat)) }

    // the last report said the game has fan content
    def fan(journal : String) : Boolean = synchronized { fans.contains(journal) }

    // the last report kept: the entries read and the factions asked
    def last(journal : String) : Option[(Int, List[String])] = synchronized { turns.get(journal) }

    def all : Map[String, (Int, List[String])] = synchronized { turns }

    // Each game's overview (2026-11-22): the icons Play Online's games list shows for it, its map and factions (Meta.overview in the
    // client), as the last client reporting a turn gave them. File overviews.tsv: game journal, the icon names (space-separated)
    val overviewFile : Path = Paths.get("overviews.tsv").toAbsolutePath

    private var overviews : Map[String, List[String]] =
        rows(overviewFile).collect { case j :: ii :: _ => j -> ii.split(" ").toList.filter(_.nonEmpty) }.toMap

    def setOverview(journal : String, icons : List[String]) : Unit = synchronized {
        if (icons.nonEmpty && !overviews.get(journal).contains(icons)) {
            overviews += journal -> icons
            write(overviewFile, lines(overviews.toList.sortBy(_._1).map { case (j, ii) => row(j, ii.mkString(" ")) }))
        }
    }

    def overview(journal : String) : List[String] = synchronized { overviews.getOrElse(journal, Nil) }

    // Each game's progress (2026-11-22), for the tournament's status posts (Tournament.status): "r<round>" and "<seat>:<faction>:<score>"
    // for each faction (Meta.status in the client), as the last client reporting a turn gave it. File statuses.tsv: game journal, the tokens
    val statusFile : Path = Paths.get("statuses.tsv").toAbsolutePath

    private var statuses : Map[String, List[String]] =
        rows(statusFile).collect { case j :: ss :: _ => j -> ss.split(" ").toList.filter(_.nonEmpty) }.toMap

    def setStatus(journal : String, tokens : List[String]) : Unit = synchronized {
        if (tokens.nonEmpty && !statuses.get(journal).contains(tokens)) {
            statuses += journal -> tokens
            write(statusFile, lines(statuses.toList.sortBy(_._1).map { case (j, ss) => row(j, ss.mkString(" ")) }))
        }
    }

    def status(journal : String) : List[String] = synchronized { statuses.getOrElse(journal, Nil) }
}
