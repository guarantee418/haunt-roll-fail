package hrf.gg

import java.nio.charset.StandardCharsets.UTF_8

// DISCORD BOT (asked for on 2026-11-20): a webhook can only post, so a Discord username given on the Tournament page pinged no one.
// With a bot's token (HRF_DISCORD_BOT_TOKEN, or the file discord-bot-token, set by the tournament change "bot <token>"), the server
// looks a username up among the members of the Discord server (Search Guild Members) and pings its user ID. The Discord server is
// HRF_DISCORD_GUILD_ID (or the file discord-guild, the change "guild <id>"), else the one of the announcements' webhook, which a GET
// of the webhook gives with no token. The bot needs no permissions in the channels, only to be a member of the server, with the
// Server Members Intent turned on in the Developer Portal. The token is never logged or written to a .result.
object DiscordBot {
    def token = Tournament.hook("HRF_DISCORD_BOT_TOKEN", "discord-bot-token")

    def configured = token.nonEmpty

    lazy val client = java.net.http.HttpClient.newBuilder().connectTimeout(java.time.Duration.ofSeconds(10)).build()

    // a GET, with the bot's token when auth is given; the body when Discord answers 2xx
    def get(url : String, auth : Option[String]) : Option[String] = {
        val b = java.net.http.HttpRequest.newBuilder(java.net.URI.create(url)).timeout(java.time.Duration.ofSeconds(10)).GET()
        auth.foreach(t => b.header("Authorization", "Bot " + t))
        val request = b.build()

        var r = client.send(request, java.net.http.HttpResponse.BodyHandlers.ofString(UTF_8))
        var tries = 0
        while (r.statusCode == 429 && tries < 3) {
            val wait = "\"retry_after\"\\s*:\\s*([0-9.]+)".r.findFirstMatchIn(r.body).flatMap(_.group(1).toDoubleOption).getOrElse(2.0)
            Thread.sleep((wait.min(10.0) * 1000).toLong + 250)
            r = client.send(request, java.net.http.HttpResponse.BodyHandlers.ofString(UTF_8))
            tries += 1
        }

        if (r.statusCode / 100 == 2)
            Some(r.body)
        else {
            println("Discord bot: " + url.replaceAll("/webhooks/.*", "/webhooks/...") + " answered " + r.statusCode + " " + r.body.take(200))
            None
        }
    }

    // a POST, PUT or PATCH of JSON (Lfg.scala: the slash command, the follow-ups), with the bot's token when auth is given; the body when 2xx
    def send(method : String, url : String, body : String, auth : Option[String]) : Option[String] = {
        val b = java.net.http.HttpRequest.newBuilder(java.net.URI.create(url)).timeout(java.time.Duration.ofSeconds(10))
            .header("Content-Type", "application/json").method(method, java.net.http.HttpRequest.BodyPublishers.ofString(body, UTF_8))
        auth.foreach(t => b.header("Authorization", "Bot " + t))
        val request = b.build()

        var r = client.send(request, java.net.http.HttpResponse.BodyHandlers.ofString(UTF_8))
        var tries = 0
        while (r.statusCode == 429 && tries < 3) {
            val wait = "\"retry_after\"\\s*:\\s*([0-9.]+)".r.findFirstMatchIn(r.body).flatMap(_.group(1).toDoubleOption).getOrElse(2.0)
            Thread.sleep((wait.min(10.0) * 1000).toLong + 250)
            r = client.send(request, java.net.http.HttpResponse.BodyHandlers.ofString(UTF_8))
            tries += 1
        }

        if (r.statusCode / 100 == 2)
            Some(r.body)
        else {
            println("Discord bot: " + method + " " + url.replaceAll("/webhooks/.*", "/webhooks/...") + " answered " + r.statusCode + " " + r.body.take(200))
            None
        }
    }

    // a direct message from the bot (Lfg.scala: a new player's seat link): its DM channel with the user, then the message; false if
    // Discord refused (no token, or the user takes no DMs from the server's members)
    def dm(user : String, text : String) : Boolean = token.exists { t =>
        send("POST", "https://discord.com/api/v10/users/@me/channels", "{\"recipient_id\":" + Tournament.json(user) + "}", Some(t))
            .flatMap(b => "\"id\"\\s*:\\s*\"(\\d+)\"".r.findFirstMatchIn(b).map(_.group(1)))
            .flatMap(c => send("POST", "https://discord.com/api/v10/channels/" + c + "/messages", "{\"content\":" + Tournament.json(text.take(1900)) + ",\"allowed_mentions\":{\"parse\":[]}}", Some(t)))
            .nonEmpty
    }

    // the webhook whose server was looked up, and that server
    @volatile var webhookGuild : Option[(String, String)] = None

    def guild : Option[String] = Tournament.hook("HRF_DISCORD_GUILD_ID", "discord-guild").orElse {
        Tournament.webhook.orElse(Tournament.forumWebhook).orElse(TurnPings.webhook).flatMap { url =>
            webhookGuild.filter(_._1 == url).map(_._2).orElse {
                val g = get(url, None).flatMap(b => Json.parse(b) match {
                    case m : Map[_, _] => m.asInstanceOf[Map[String, Any]].get("guild_id").collect { case s : String => s }
                    case _ => None
                })
                g.foreach(x => webhookGuild = Some((url, x)))
                g
            }
        }
    }

    sealed trait Lookup
    case class Found(id : String) extends Lookup
    case object NotFound extends Lookup
    case object Unavailable extends Lookup

    // names already looked up: found ones for the server's lifetime, missing ones for two minutes (they may join the server)
    val found = new java.util.concurrent.ConcurrentHashMap[String, String]()
    val missing = new java.util.concurrent.ConcurrentHashMap[String, java.lang.Long]()

    def cleanName(v : String) = v.trim.stripPrefix("@").toLowerCase

    // a username: Discord's own (lower case letters, digits, _ and .), or the display or server name a member goes by
    def lookup(name : String) : Lookup = {
        val q = cleanName(name)

        Option(found.get(q)).map(Found(_)).getOrElse {
            if (Option(missing.get(q)).exists(t => System.currentTimeMillis - t < 2 * 60 * 1000))
                NotFound
            else
            (token, guild) match {
                case (Some(t), Some(g)) =>
                    try {
                        get("https://discord.com/api/v10/guilds/" + g + "/members/search?limit=100&query=" + java.net.URLEncoder.encode(q, UTF_8), Some(t)) match {
                            case Some(body) =>
                                val result = pick(q, members(body))
                                result match {
                                    case Found(id) => found.put(q, id) ; missing.remove(q)
                                    case _ => missing.put(q, System.currentTimeMillis)
                                }
                                result
                            case None => Unavailable
                        }
                    }
                    catch {
                        case e : Exception => println("Discord bot: lookup failed: " + e) ; Unavailable
                    }
                case _ => Unavailable
            }
        }
    }

    def idOf(name : String) : Option[String] = lookup(name) match {
        case Found(id) => Some(id)
        case _ => None
    }

    // a member found by a search: user ID, username, display name, server nickname
    case class Member(id : String, username : String, global : String, nick : String)

    def members(body : String) : List[Member] = Json.parse(body) match {
        case l : List[_] => l.collect { case m : Map[_, _] =>
            val mm = m.asInstanceOf[Map[String, Any]]
            val u = mm.get("user").collect { case x : Map[_, _] => x.asInstanceOf[Map[String, Any]] }.getOrElse(Map[String, Any]())
            def s(x : Option[Any]) = x.collect { case v : String => v }.getOrElse("")
            Member(s(u.get("id")), s(u.get("username")), s(u.get("global_name")), s(mm.get("nick")))
        }.filter(_.id.nonEmpty)
        case _ => Nil
    }

    // the username itself first; else a display or server name only one member has
    def pick(q : String, l : List[Member]) : Lookup =
        l.find(_.username.toLowerCase == q).map(m => Found(m.id)).getOrElse {
            l.filter(m => m.global.toLowerCase == q || m.nick.toLowerCase == q).map(_.id).distinct match {
                case id :: Nil => Found(id)
                case _ => NotFound
            }
        }
}

// just enough JSON for Discord's answers: objects (Map), arrays (List), strings, numbers (as their text), true, false, null
object Json {
    def parse(s : String) : Any = {
        var i = 0

        def ws() { while (i < s.length && s(i).isWhitespace) i += 1 }

        def fail() = throw new IllegalArgumentException("bad JSON at " + i)

        def value() : Any = {
            ws()
            if (i >= s.length) fail()
            s(i) match {
                case '{' =>
                    i += 1
                    val m = scala.collection.mutable.LinkedHashMap[String, Any]()
                    ws()
                    if (i < s.length && s(i) == '}') i += 1
                    else {
                        var more = true
                        while (more) {
                            ws()
                            val k = string()
                            ws()
                            if (i >= s.length || s(i) != ':') fail()
                            i += 1
                            m(k) = value()
                            ws()
                            if (i < s.length && s(i) == ',') i += 1
                            else if (i < s.length && s(i) == '}') { i += 1 ; more = false }
                            else fail()
                        }
                    }
                    m.toMap
                case '[' =>
                    i += 1
                    val l = scala.collection.mutable.ListBuffer[Any]()
                    ws()
                    if (i < s.length && s(i) == ']') i += 1
                    else {
                        var more = true
                        while (more) {
                            l += value()
                            ws()
                            if (i < s.length && s(i) == ',') i += 1
                            else if (i < s.length && s(i) == ']') { i += 1 ; more = false }
                            else fail()
                        }
                    }
                    l.toList
                case '"' => string()
                case 't' if s.startsWith("true", i) => i += 4 ; true
                case 'f' if s.startsWith("false", i) => i += 5 ; false
                case 'n' if s.startsWith("null", i) => i += 4 ; null
                case c if c == '-' || c.isDigit =>
                    val start = i
                    while (i < s.length && "+-.eE0123456789".contains(s(i))) i += 1
                    s.substring(start, i)
                case _ => fail()
            }
        }

        def string() : String = {
            if (i >= s.length || s(i) != '"') fail()
            i += 1
            val b = new StringBuilder
            while (i < s.length && s(i) != '"') {
                if (s(i) == '\\') {
                    i += 1
                    if (i >= s.length) fail()
                    s(i) match {
                        case 'n' => b += '\n'
                        case 't' => b += '\t'
                        case 'r' => b += '\r'
                        case 'b' => b += '\b'
                        case 'f' => b += '\f'
                        case 'u' if i + 4 < s.length => b += Integer.parseInt(s.substring(i + 1, i + 5), 16).toChar ; i += 4
                        case c => b += c
                    }
                }
                else
                    b += s(i)
                i += 1
            }
            if (i >= s.length) fail()
            i += 1
            b.toString
        }

        val v = value()
        ws()
        if (i != s.length) fail()
        v
    }
}
