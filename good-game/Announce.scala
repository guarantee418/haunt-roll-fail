package hrf.gg

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Paths}

// GENERAL ANNOUNCEMENTS (asked for on 2026-11-26): a webhook that posts only to the Discord server's #announcements channel, made by the
// bot as "General Announcements" with a scroll for its picture (make-webhook General Announcements | in announcements | avatar
// general-announcements.png | for general). Each announcement goes out under the next of four names from Samson's riddle (Judges 14),
// each with its own picture, in turn: Eater to Eat, Sweeter than Honey, Strength became Sweet, Stronger than a Lion, a sun (pictures in
// webp2/lfg/webhooks/, drawn by haunt-roll-fail/tools/announce-avatars.py, served by /art/). The webhook's own name and scroll show
// wherever Discord shows the webhook itself.
//
// The texts are files in hrf-games, announcements/<name>.md (Backup.directory), posted by the tournament changes:
//   announce <name>        posts announcements/<name>.md; a line of only "---" starts a new message, and a message longer than Discord's
//                          2000 characters is split at blank lines. Every message of one announcement takes the same name; a message
//                          pings only when its text says @everyone or @here (both allowed then, 2026-11-26, asked for); nobody else
//                          is ever pinged.
//   announce-voices        says which name the next announcement takes
// The webhook's URL is kept in the file discord-general-webhook (or HRF_DISCORD_GENERAL_WEBHOOK), set by make-webhook's "for general"; the
// next name's number in tournament/announce-voice.tsv.
//
// NORTHGARD ANNOUNCEMENTS (2026-11-26, asked for): a second desk, a webhook in the Northgard announcements channel that always posts as
// Vedrfolnir, the hawk at the top of Yggdrasil that sees all nine worlds (the Wastelands tile waste-vedrfolnir; the Northgard heckler,
// Ratatoskr, carries the eagle's insults down that tree, so the two are a pair). Avatar webp2/lfg/webhooks/vedrfolnir.png, the Clan of the
// Eagle's painted eagle, made by haunt-roll-fail/tools/announce-avatars.py. Made by make-webhook Vedrfolnir | in northgard-announcements | avatar vedrfolnir.png |
// for nort-announcements (its URL in discord-nort-announce-webhook, or HRF_DISCORD_NORT_ANNOUNCE_WEBHOOK); the texts are
// announcements/<name>.md in hrf-games too, posted by
//   nort-announce <name>   as announce does, through Vedrfolnir
//
// GAME UPDATES (2026-11-26, asked for): each game's updates go to its own announcements channel, each through a webhook posting under one
// name: Northgard's Vedrfolnir above; Root's through Royal Claim, the tournament's webhook in #root-announcements (Tournament.webhook),
// as Informants (root-announce <name>); Pincer's Dispatch Rider in #pincer-announcements (pincer-announce <name>, make-webhook ... | for
// pincer-announcements); Company of Heroes' Signal Corps in #coh-announcements (coh-announce <name>, make-webhook ... | for
// coh-announcements). Avatars in webp2/lfg/webhooks/: the Informants card's art, Pincer's Mustang card medallion, the Mechanized
// Company commander card's crewman in his radio headset (announce-avatars.py).
object Announce {
    case class Voice(name : String, avatar : String)

    // an announcements webhook: the change that posts through it, where its URL is kept, the names its posts take in turn, and where the
    // next one's number is kept
    case class Desk(name : String, word : String, title : String, env : String, file : String, voices : List[Voice], counter : String) {
        def hook = Tournament.hook(env, file)
        def next = Tournament.dir.resolve(counter)
    }

    val general = Desk("general", "announce", "general announcements", "HRF_DISCORD_GENERAL_WEBHOOK", "discord-general-webhook", List(
        Voice("Eater to Eat", "eater-to-eat.png"),
        Voice("Sweeter than Honey", "sweeter-than-honey.png"),
        Voice("Strength became Sweet", "strength-became-sweet.png"),
        Voice("Stronger than a Lion", "stronger-than-a-lion-sun.png")), "announce-voice.tsv")

    val nort = Desk("nort-announcements", "nort-announce", "Northgard announcements", "HRF_DISCORD_NORT_ANNOUNCE_WEBHOOK", "discord-nort-announce-webhook", List(
        Voice("Vedrfolnir", "vedrfolnir.png")), "nort-announce-voice.tsv")

    // Root's game updates go through the tournament's announcements webhook (Royal Claim, in #root-announcements: Tournament.webhook),
    // posting as Informants; set it with the tournament change "webhook <url>", not make-webhook
    val root = Desk("root-announcements", "root-announce", "Root announcements", "HRF_DISCORD_WEBHOOK", "discord-webhook", List(
        Voice("Informants", "informants.png")), "root-announce-voice.tsv")

    val pincer = Desk("pincer-announcements", "pincer-announce", "Pincer announcements", "HRF_DISCORD_PINCER_ANNOUNCE_WEBHOOK", "discord-pincer-announce-webhook", List(
        Voice("Dispatch Rider", "dispatch-rider.png")), "pincer-announce-voice.tsv")

    val tactics = Desk("coh-announcements", "coh-announce", "Company of Heroes announcements", "HRF_DISCORD_COH_ANNOUNCE_WEBHOOK", "discord-coh-announce-webhook", List(
        Voice("Signal Corps", "signal-corps.png")), "coh-announce-voice.tsv")

    // Cyclades' (2026-11-27): the Oracle of Delphi, in #cyclades-announcements (cyclades-announce <name>)
    val cyclades = Desk("cyclades-announcements", "cyclades-announce", "Cyclades announcements", "HRF_DISCORD_CYCLADES_ANNOUNCE_WEBHOOK", "discord-cyclades-announce-webhook", List(
        Voice("Oracle of Delphi", "oracle-of-delphi.png")), "cyclades-announce-voice.tsv")

    val desks = List(general, nort, root, pincer, tactics, cyclades)

    // make-webhook's "for": a desk's name, or nort, root, pincer, coh (root only through the tournament change "webhook")
    def desk(s : String) : Option[Desk] = s.trim.toLowerCase match {
        case "nort" | "northgard" => Some(nort)
        case "pincer" => Some(pincer)
        case "coh" | "tactics" | "tactics-announcements" => Some(tactics)
        case "cyclades" => Some(cyclades)
        case n => desks.find(_.name == n).filter(_ != root)
    }

    // the desk a webhook is set as, by the ID in the URL kept
    def using(id : String) : List[String] = desks.filter(_.hook.exists(_.split('/').contains(id))).map(_.name)

    def setHook(k : Desk, url : String) : String =
        if (url.startsWith("https://discord.com/api/webhooks/") || url.startsWith("https://discordapp.com/api/webhooks/")) {
            Files.write(Paths.get(k.file), (url + "\n").getBytes(UTF_8))
            "set as the " + k.title + " webhook"
        }
        else "not set: not a Discord webhook URL"

    def nextIndex(k : Desk) : Int = try {
        if (Files.exists(k.next)) new String(Files.readAllBytes(k.next), UTF_8).trim.toIntOption.getOrElse(0) else 0
    } catch { case _ : Exception => 0 }

    def advance(k : Desk, i : Int) {
        Files.createDirectories(k.next.getParent)
        Files.write(k.next, ((i + 1) % k.voices.size).toString.getBytes(UTF_8))
    }

    // the messages of a text: split at "---" lines, then at blank lines to keep each under Discord's limit
    def messages(text : String, limit : Int = 1900) : List[String] = {
        val parts = text.replace("\r", "").split("\n---[ \t]*\n|^---[ \t]*\n|\n---[ \t]*$").toList.map(_.trim).filter(_.nonEmpty)
        parts.flatMap { p =>
            if (p.length <= limit) List(p)
            else {
                val out = scala.collection.mutable.ListBuffer[String]()
                var cur = ""
                p.split("\n\n").foreach { para =>
                    val add = if (cur.isEmpty) para else cur + "\n\n" + para
                    if (add.length <= limit) cur = add
                    else {
                        if (cur.nonEmpty) out += cur
                        cur = para
                        while (cur.length > limit) {
                            val cut = cur.lastIndexOf('\n', limit) match { case k if k > limit / 2 => k ; case _ => limit }
                            out += cur.take(cut).trim
                            cur = cur.drop(cut).trim
                        }
                    }
                }
                if (cur.nonEmpty) out += cur
                out.toList
            }
        }
    }

    def persona(v : Voice) =
        ",\"username\":" + Tournament.json(v.name) +
            (if (Tournament.site.nonEmpty) ",\"avatar_url\":" + Tournament.json(Tournament.site + "/art/lfg/webhooks/" + v.avatar) else "")

    def send(url : String, text : String, v : Voice) : Either[String, Unit] = {
        val everyone = text.contains("@everyone") || text.contains("@here")
        val body = "{\"content\":" + Tournament.json(text) + ",\"allowed_mentions\":{\"parse\":[" + (if (everyone) "\"everyone\"" else "") + "]}" + persona(v) + "}"
        val request = java.net.http.HttpRequest.newBuilder(java.net.URI.create(url + "?wait=true")).timeout(java.time.Duration.ofSeconds(15))
            .header("Content-Type", "application/json").POST(java.net.http.HttpRequest.BodyPublishers.ofString(body, UTF_8)).build()
        try {
            var r = Tournament.client.send(request, java.net.http.HttpResponse.BodyHandlers.ofString(UTF_8))
            var tries = 0
            while (r.statusCode == 429 && tries < 5) {
                val wait = "\"retry_after\"\\s*:\\s*([0-9.]+)".r.findFirstMatchIn(r.body).flatMap(_.group(1).toDoubleOption).getOrElse(2.0)
                Thread.sleep((wait.min(30.0) * 1000).toLong + 250)
                r = Tournament.client.send(request, java.net.http.HttpResponse.BodyHandlers.ofString(UTF_8))
                tries += 1
            }
            if (r.statusCode / 100 == 2) Right(()) else Left("Discord answered " + r.statusCode + ": " + r.body.take(200))
        }
        catch { case e : Exception => Left("Discord couldn't be asked: " + e.getMessage) }
    }

    def announce(k : Desk, name : String) : String = {
        val clean = name.trim.stripSuffix(".md")
        if (!clean.matches("[A-Za-z0-9._-]+")) "not done: an announcement is a file name in announcements/ (letters, digits, - and _)"
        else k.hook match {
            case None => "not done: no " + k.title + " webhook (make-webhook ... | for " + k.name + ")"
            case Some(url) => Backup.directory.map(_.resolve("announcements").resolve(clean + ".md")) match {
                case None => "not done: no hrf-games copy (backup-dir)"
                case Some(p) if !Files.isRegularFile(p) => "not done: no announcements/" + clean + ".md"
                case Some(p) =>
                    val mm = messages(new String(Files.readAllBytes(p), UTF_8))
                    val i = nextIndex(k)
                    val v = k.voices(i % k.voices.size)
                    var sent = 0
                    var failed : Option[String] = None
                    mm.foreach { m => if (failed.isEmpty) send(url, m, v) match { case Right(_) => sent += 1 ; case Left(e) => failed = Some(e) } }
                    if (sent > 0) advance(k, i)
                    failed match {
                        case None => "posted " + clean + " as " + v.name + " (" + sent + " message" + (if (sent == 1) "" else "s") + ")"
                        case Some(e) => "posted " + sent + " of " + mm.size + " messages of " + clean + " as " + v.name + ", then " + e
                    }
            }
        }
    }

    def change(word : String, rest : String) : String = word match {
        case w if desks.exists(_.word == w) => announce(desks.find(_.word == w).get, rest)
        case "announce-voices" => desks.map { k =>
            k.title + ": next " + k.voices(nextIndex(k) % k.voices.size).name +
                (if (k.voices.size > 1) "; in turn: " + k.voices.map(_.name).mkString(", ") else "") +
                "; webhook " + (if (k.hook.nonEmpty) "set" else "not set")
        }.mkString("\n")
        case _ => "not done"
    }

    val words = desks.map(_.word).toSet + "announce-voices"
}
