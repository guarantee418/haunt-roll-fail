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
//                          2000 characters is split at blank lines. Every message of one announcement takes the same name; no one is
//                          pinged (allowed_mentions is empty).
//   announce-voices        says which name the next announcement takes
// The webhook's URL is kept in the file discord-general-webhook (or HRF_DISCORD_GENERAL_WEBHOOK), set by make-webhook's "for general"; the
// next name's number in tournament/announce-voice.tsv.
object Announce {
    case class Voice(name : String, avatar : String)

    val voices = List(
        Voice("Eater to Eat", "eater-to-eat.png"),
        Voice("Sweeter than Honey", "sweeter-than-honey.png"),
        Voice("Strength became Sweet", "strength-became-sweet.png"),
        Voice("Stronger than a Lion", "stronger-than-a-lion-sun.png"))

    val env = "HRF_DISCORD_GENERAL_WEBHOOK"
    val file = "discord-general-webhook"
    val next = Tournament.dir.resolve("announce-voice.tsv")

    def hook = Tournament.hook(env, file)

    def setHook(url : String) : String =
        if (url.startsWith("https://discord.com/api/webhooks/") || url.startsWith("https://discordapp.com/api/webhooks/")) {
            Files.write(Paths.get(file), (url + "\n").getBytes(UTF_8))
            "set as the general announcements webhook"
        }
        else "not set: not a Discord webhook URL"

    def nextIndex : Int = try {
        if (Files.exists(next)) new String(Files.readAllBytes(next), UTF_8).trim.toIntOption.getOrElse(0) else 0
    } catch { case _ : Exception => 0 }

    def advance(i : Int) {
        Files.createDirectories(next.getParent)
        Files.write(next, ((i + 1) % voices.size).toString.getBytes(UTF_8))
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
        val body = "{\"content\":" + Tournament.json(text) + ",\"allowed_mentions\":{\"parse\":[]}" + persona(v) + "}"
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

    def announce(name : String) : String = {
        val clean = name.trim.stripSuffix(".md")
        if (!clean.matches("[A-Za-z0-9._-]+")) "not done: an announcement is a file name in announcements/ (letters, digits, - and _)"
        else hook match {
            case None => "not done: no general announcements webhook (make-webhook ... | for general)"
            case Some(url) => Backup.directory.map(_.resolve("announcements").resolve(clean + ".md")) match {
                case None => "not done: no hrf-games copy (backup-dir)"
                case Some(p) if !Files.isRegularFile(p) => "not done: no announcements/" + clean + ".md"
                case Some(p) =>
                    val mm = messages(new String(Files.readAllBytes(p), UTF_8))
                    val i = nextIndex
                    val v = voices(i % voices.size)
                    var sent = 0
                    var failed : Option[String] = None
                    mm.foreach { m => if (failed.isEmpty) send(url, m, v) match { case Right(_) => sent += 1 ; case Left(e) => failed = Some(e) } }
                    if (sent > 0) advance(i)
                    failed match {
                        case None => "posted " + clean + " as " + v.name + " (" + sent + " message" + (if (sent == 1) "" else "s") + ")"
                        case Some(e) => "posted " + sent + " of " + mm.size + " messages of " + clean + " as " + v.name + ", then " + e
                    }
            }
        }
    }

    def change(word : String, rest : String) : String = word match {
        case "announce" => announce(rest)
        case "announce-voices" => "next: " + voices(nextIndex % voices.size).name + "; in turn: " + voices.map(_.name).mkString(", ") +
            "; webhook " + (if (hook.nonEmpty) "set" else "not set")
        case _ => "not done"
    }

    val words = Set("announce", "announce-voices")
}
