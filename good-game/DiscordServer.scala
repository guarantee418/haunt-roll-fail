package hrf.gg

import java.nio.charset.StandardCharsets.UTF_8

// DISCORD SERVER ROLES AND CHANNELS (asked for on 2026-11-24): the owner manages the Discord server's roles and channels through the
// bot, by committing tournament changes (tournament-changes/<name>.txt in hrf-games, Tournament.change, applied by Backup.scala), so a
// Claude session, which can't reach Discord, can do it for them. The bot needs Manage Roles and Manage Channels; a role it creates or
// gives must sit below the bot's own role. On purpose there is no delete, no kick or ban, and a role is never given any server
// permission (created with none): only names, colors, who sees a channel and who may write in it. Every request carries an audit log
// reason, so the server's audit log shows them. A name is matched without regard to case (a text channel's also with its spaces as
// dashes, as Discord writes them); a user or channel ID, or a <@...>, <#...> or <@&...> mention, works in place of any name.
//
//   roles                                     lists the roles, highest first: name, ID, color, members shown apart, mentionable
//   role <name> [| color #rrggbb] [| hoist] [| mentionable] [| plain]
//                                             creates the role, or changes one of that name (hoist: its members are shown apart;
//                                             plain: neither hoisted nor mentionable)
//   rename-role <name> -> <new name>
//   give-role <user> -> <role>                a user ID, a mention, a username or display name the bot finds, or a ranking name
//   take-role <user> -> <role>                with a Discord (Tournament.discordId)
//   merge-role <role> -> <role>               gives everyone holding the first role the second and takes the first off them; the first
//                                             role stays, to be deleted in Discord (2026-11-25)
//   channels                                  lists the categories and their channels: type, name, ID, who may see or write
//   category <name> [| private] [| see <role>, ...] [| read-only <role>, ...]
//   channel <name> [| in <category>] [| text | voice | forum | announcement | stage] [| topic <text>] [| private]
//           [| see <role>, ...] [| read-only <role>, ...] [| synced]
//                                             creates the channel (text by default), or changes the one of that name (in that category,
//                                             if given): private hides it from @everyone, see lets those roles see it (and write),
//                                             read-only lets them see it but not write; a new channel with none of those takes its
//                                             category's permissions, as does one changed with "synced". The bot always keeps seeing
//                                             and writing in a channel it makes private. Given any of private, see or read-only, the
//                                             channel's permissions become exactly those (and the bot's), else they are left as they are
//   rename-channel <name> -> <new name>       a category too
//   move-channel <name> -> <category>         "-> none" takes it out of its category
//   order <category> -> <channel>, <channel>, ...
//                                             puts the category's channels in that order, top first; those left out follow, as they were
//
// WEBHOOKS (2026-11-24, asked for as "thematic webhooks for Pincer and CoH that the Cobbler bot can manage", like Northgard's Huginn
// and Muninn): the bot makes a channel's webhook with a name and avatar of its own, and can hand its URL straight to a turns channel
// (TurnPings.slots), so nobody copies a webhook URL by hand. Needs Manage Webhooks. A webhook's URL holds its token: no result shows it.
//   webhooks                                  lists the server's webhooks: name, channel, ID, whether the bot made it, the turns
//                                             channel it is set as
//   make-webhook <name> | in <channel> [| avatar <file>] [| for <turns channel>]
//                                             makes the channel's webhook, or renames and gives the avatar to the one of that name
//                                             there; avatar is a picture in webp2/lfg/webhooks/ (made by tools/webhook-avatars.py:
//                                             drum-and-fife.png, field-wireless.png); for sets it as turns, nort-turns, pincer-turns or
//                                             tactics-turns, as that change would (only a webhook Discord gives the bot the token of)
//   rename-webhook <name> -> <new name>       (in any channel; its ID works in place of its name)
//   test-ping <turns channel> [| role <role>] [| text <text>]
//                                             posts a new thread, "Test ping", through that turns channel's webhook, mentioning the
//                                             role (2026-11-24, asked for to try the new webhooks); a forum's thread, else the channel
object DiscordServer {
    def api = sys.props.getOrElse("hrf.discord.api", "https://discord.com/api/v10")

    // permission bits
    val View = 1L << 10
    val Send = 1L << 11
    val Connect = 1L << 20
    val Speak = 1L << 21
    val PublicThreads = 1L << 35
    val PrivateThreads = 1L << 36
    val SendInThreads = 1L << 38
    val Write = Send | Speak | PublicThreads | PrivateThreads | SendInThreads
    val ReadOnlyDeny = Write
    val SeeAllow = View | Connect | Write

    val types = Map("text" -> 0, "voice" -> 2, "category" -> 4, "announcement" -> 5, "stage" -> 13, "forum" -> 15)
    def typeName(t : Int) = types.find(_._2 == t).map(_._1).getOrElse("type " + t)

    // a request with the bot's token and an audit log reason: Right(body), or Left(what Discord said)
    def call(method : String, path : String, body : String = "") : Either[String, String] = DiscordBot.token match {
        case None => Left("no Discord bot (the bot change)")
        case Some(t) =>
            val b = java.net.http.HttpRequest.newBuilder(java.net.URI.create(api + path)).timeout(java.time.Duration.ofSeconds(15))
                .header("Authorization", "Bot " + t)
                .header("X-Audit-Log-Reason", "HRF server change")
            if (body.nonEmpty)
                b.header("Content-Type", "application/json").method(method, java.net.http.HttpRequest.BodyPublishers.ofString(body, UTF_8))
            else
                b.method(method, java.net.http.HttpRequest.BodyPublishers.noBody())
            val request = b.build()
            try {
                var r = DiscordBot.client.send(request, java.net.http.HttpResponse.BodyHandlers.ofString(UTF_8))
                var tries = 0
                while (r.statusCode == 429 && tries < 3) {
                    val wait = "\"retry_after\"\\s*:\\s*([0-9.]+)".r.findFirstMatchIn(r.body).flatMap(_.group(1).toDoubleOption).getOrElse(2.0)
                    Thread.sleep((wait.min(10.0) * 1000).toLong + 250)
                    r = DiscordBot.client.send(request, java.net.http.HttpResponse.BodyHandlers.ofString(UTF_8))
                    tries += 1
                }
                if (r.statusCode / 100 == 2)
                    Right(r.body)
                else {
                    val message = obj(r.body).get("message").collect { case s : String => s }.getOrElse(r.body.take(200))
                    val hint = r.statusCode match {
                        case 403 => " (the bot lacks a permission, Manage Webhooks for a webhook, or the role is above the bot's own role)"
                        case _ => ""
                    }
                    Left("Discord answered " + r.statusCode + ": " + message + hint)
                }
            }
            catch {
                case e : Exception => Left("Discord couldn't be asked: " + e.getMessage)
            }
    }

    def obj(body : String) : Map[String, Any] = try Json.parse(body) match {
        case m : Map[_, _] => m.asInstanceOf[Map[String, Any]]
        case _ => Map()
    } catch { case _ : Exception => Map() }

    def list(body : String) : List[Map[String, Any]] = try Json.parse(body) match {
        case l : List[_] => l.collect { case m : Map[_, _] => m.asInstanceOf[Map[String, Any]] }
        case _ => Nil
    } catch { case _ : Exception => Nil }

    def str(m : Map[String, Any], k : String) = m.get(k).collect { case s : String => s }.getOrElse("")
    def num(m : Map[String, Any], k : String) = m.get(k).collect { case s : String => s.toLongOption.getOrElse(0L) }.getOrElse(0L)
    def bool(m : Map[String, Any], k : String) = m.get(k).contains(true)

    case class Role(id : String, name : String, color : Long, position : Long, hoist : Boolean, mentionable : Boolean, managed : Boolean, permissions : Long)
    case class Overwrite(id : String, kind : Int, allow : Long, deny : Long)
    case class Channel(id : String, name : String, kind : Int, parent : String, position : Long, overwrites : List[Overwrite])

    def roles(g : String) : Either[String, List[Role]] = call("GET", "/guilds/" + g + "/roles").map(b => list(b).map(m =>
        Role(str(m, "id"), str(m, "name"), num(m, "color"), num(m, "position"), bool(m, "hoist"), bool(m, "mentionable"), bool(m, "managed"), num(m, "permissions"))))

    def channels(g : String) : Either[String, List[Channel]] = call("GET", "/guilds/" + g + "/channels").map(b => list(b).map { m =>
        val ow = m.get("permission_overwrites").collect { case l : List[_] => l.collect { case o : Map[_, _] =>
            val oo = o.asInstanceOf[Map[String, Any]]
            Overwrite(str(oo, "id"), num(oo, "type").toInt, num(oo, "allow"), num(oo, "deny"))
        } }.getOrElse(Nil)
        Channel(str(m, "id"), str(m, "name"), num(m, "type").toInt, str(m, "parent_id"), num(m, "position"), ow)
    })

    def me : Either[String, String] = call("GET", "/users/@me").map(b => str(obj(b), "id"))

    def idIn(v : String) : Option[String] = {
        val s = v.trim.stripPrefix("<").stripSuffix(">").stripPrefix("@&").stripPrefix("@!").stripPrefix("@").stripPrefix("#")
        Some(s).filter(Tournament.isDiscordId)
    }

    def norm(s : String) = s.trim.toLowerCase
    def dashed(s : String) = norm(s).replaceAll("\\s+", "-")

    def findRole(rr : List[Role], v : String) : Either[String, Role] =
        idIn(v).flatMap(id => rr.find(_.id == id)).map(Right(_)).getOrElse {
            val n = norm(v.stripPrefix("@"))
            rr.filter(r => norm(r.name) == n || (n == "everyone" && r.name == "@everyone")) match {
                case r :: Nil => Right(r)
                case Nil => Left("no role called " + v)
                case l => Left(l.size + " roles are called " + v + "; give its ID (roles lists them)")
            }
        }

    def findChannel(cc : List[Channel], v : String, kinds : Int => Boolean = _ => true) : Either[String, Channel] =
        idIn(v).flatMap(id => cc.find(_.id == id)).map(Right(_)).getOrElse {
            val n = norm(v.stripPrefix("#"))
            cc.filter(c => kinds(c.kind) && (norm(c.name) == n || norm(c.name) == dashed(n))) match {
                case c :: Nil => Right(c)
                case Nil => Left("no channel called " + v)
                case l => Left(l.size + " channels are called " + v + "; give its ID (channels lists them)")
            }
        }

    def color(c : Long) = if (c == 0) "no color" else "#%06x".format(c)

    // a change's options, after its name: "name | key value | flag"
    def split(rest : String) : (String, List[(String, String)]) = rest.split("\\|").toList.map(_.trim) match {
        case name :: opts => (name, opts.filter(_.nonEmpty).map(o => (o.takeWhile(_ != ' ').toLowerCase, o.dropWhile(_ != ' ').trim)))
        case Nil => ("", Nil)
    }

    def names(v : String) = v.split(",").toList.map(_.trim).filter(_.nonEmpty)

    def change(word : String, rest : String) : String = DiscordBot.guild match {
        case None => "not done: no Discord server known (a webhook, or the guild change)"
        case Some(g) => (word match {
            case "roles" => listRoles(g)
            case "role" => role(g, rest)
            case "rename-role" => arrow(rest).flatMap { case (a, b) => roles(g).flatMap(findRole(_, a)).flatMap(r =>
                call("PATCH", "/guilds/" + g + "/roles/" + r.id, "{\"name\":" + Tournament.json(b) + "}").map(_ => "renamed")) }
            case "give-role" => member(g, rest, "PUT").map(_ => "given")
            case "take-role" => member(g, rest, "DELETE").map(_ => "taken")
            case "merge-role" => mergeRole(g, rest)
            case "channels" => listChannels(g)
            case "category" => channel(g, rest, true)
            case "channel" => channel(g, rest, false)
            case "rename-channel" => arrow(rest).flatMap { case (a, b) => channels(g).flatMap(findChannel(_, a)).flatMap(c =>
                call("PATCH", "/channels/" + c.id, "{\"name\":" + Tournament.json(b) + "}").map(_ => "renamed")) }
            case "order" => order(g, rest)
            case "webhooks" => listWebhooks(g)
            case "make-webhook" => makeWebhook(g, rest)
            case "test-ping" => testPing(g, rest)
            case "rename-webhook" => arrow(rest).flatMap { case (a, b) => webhooks(g).flatMap(findWebhook(_, a)).flatMap(w =>
                call("PATCH", "/webhooks/" + w.id, "{\"name\":" + Tournament.json(b) + "}").map(_ => "renamed")) }
            case "move-channel" => arrow(rest).flatMap { case (a, b) => channels(g).flatMap { cc =>
                findChannel(cc, a, _ != 4).flatMap { c =>
                    (if (norm(b) == "none") Right(None) else findChannel(cc, b, _ == 4).map(Some(_))).flatMap { p =>
                        call("PATCH", "/channels/" + c.id, "{\"parent_id\":" + p.map(x => Tournament.json(x.id)).getOrElse("null") + "}")
                            .map(_ => p.map("moved to " + _.name).getOrElse("taken out of its category"))
                    }
                }
            } }
        }).fold("not done: " + _, x => x)
    }

    // Discord sorts a category's channels by position (voice channels after the others), so the ones named get 0, 1, 2, ... and the
    // others the positions after them, all sent at once (Modify Guild Channel Positions)
    def order(g : String, rest : String) : Either[String, String] = arrow(rest).flatMap { case (k, l) =>
        channels(g).flatMap { cc =>
            findChannel(cc, k, _ == 4).flatMap { cat =>
                val in = cc.filter(_.parent == cat.id)
                names(l).map(n => findChannel(in, n)).collectFirst { case Left(e) => Left(e + " in " + cat.name) }.getOrElse {
                    val named = names(l).map(n => findChannel(in, n).toOption.get).distinctBy(_.id)
                    val all = named ++ in.filterNot(c => named.exists(_.id == c.id)).sortBy(_.position)
                    call("PATCH", "/guilds/" + g + "/channels", all.zipWithIndex.map { case (c, i) => "{\"id\":" + Tournament.json(c.id) + ",\"position\":" + i + "}" }.mkString("[", ",", "]"))
                        .map(_ => "ordered: " + all.map(_.name).mkString(", "))
                }
            }
        }
    }

    def arrow(rest : String) : Either[String, (String, String)] = rest.split(" -> ").toList.map(_.trim) match {
        case a :: b :: Nil if a.nonEmpty && b.nonEmpty => Right((a, b))
        case _ => Left("<name> -> <name>")
    }

    def listRoles(g : String) : Either[String, String] = roles(g).map(_.sortBy(-_.position).map { r =>
        r.name + " (" + r.id + "): " + color(r.color) + (if (r.hoist) ", shown apart" else "") + (if (r.mentionable) ", mentionable" else "") + (if (r.managed) ", a bot's or integration's" else "")
    }.mkString("\n", "\n", ""))

    def role(g : String, rest : String) : Either[String, String] = {
        val (name, opts) = split(rest)
        if (name.isEmpty) return Left("role <name> [| color #rrggbb] [| hoist] [| mentionable] [| plain]")
        val bad = opts.map(_._1).filterNot(Set("color", "colour", "hoist", "mentionable", "plain"))
        if (bad.nonEmpty) return Left("not an option: " + bad.mkString(", "))
        val c = opts.find(o => o._1 == "color" || o._1 == "colour").map(_._2.stripPrefix("#"))
        if (c.exists(x => x.length != 6 || !x.forall(ch => "0123456789abcdefABCDEF".contains(ch)))) return Left("a color is #rrggbb")
        val fields =
            c.map(x => "\"color\":" + java.lang.Long.parseLong(x, 16)).toList ++
            (if (opts.exists(_._1 == "plain")) List("\"hoist\":false", "\"mentionable\":false") else Nil) ++
            (if (opts.exists(_._1 == "hoist")) List("\"hoist\":true") else Nil) ++
            (if (opts.exists(_._1 == "mentionable")) List("\"mentionable\":true") else Nil)
        roles(g).flatMap { rr =>
            rr.filter(r => norm(r.name) == norm(name)) match {
                case Nil =>
                    call("POST", "/guilds/" + g + "/roles", ("\"name\":" + Tournament.json(name) :: "\"permissions\":\"0\"" :: fields).mkString("{", ",", "}"))
                        .map(b => "created (" + str(obj(b), "id") + ")")
                case r :: Nil if r.managed => Left("that role belongs to a bot or integration")
                case r :: Nil if fields.isEmpty => Right("already there (" + r.id + ")")
                case r :: Nil => call("PATCH", "/guilds/" + g + "/roles/" + r.id, fields.mkString("{", ",", "}")).map(_ => "updated (" + r.id + ")")
                case l => Left(l.size + " roles are called " + name)
            }
        }
    }

    def user(v : String) : Either[String, String] = idIn(v).map(Right(_)).getOrElse {
        Tournament.discordId(Ranking.clean(v).toLowerCase).map(Right(_)).getOrElse {
            DiscordBot.lookup(v) match {
                case DiscordBot.Found(id) => Right(id)
                case DiscordBot.NotFound => Left("no member of the server is called " + v)
                case DiscordBot.Unavailable => Left("Discord couldn't be asked about " + v)
            }
        }
    }

    // MERGE-ROLE (2026-11-25, asked for: "merge the ng spectator and the root spectator into a single spectator role"): every member
    // holding the first role is given the second and the first is taken off them; the first role itself stays (the bot deletes
    // nothing), for the owner to delete in Discord
    def mergeRole(g : String, rest : String) : Either[String, String] = arrow(rest).flatMap { case (a, b) =>
        roles(g).flatMap { rr =>
            findRole(rr, a).flatMap(from => findRole(rr, b).flatMap { into =>
                if (Seq(from, into).exists(r => r.managed || r.name == "@everyone")) Left("those roles can't be given or taken")
                else if (from.id == into.id) Left("that's the same role")
                else holders(g, from.id).map { ids =>
                    val failed = ids.filter(id => call("PUT", "/guilds/" + g + "/members/" + id + "/roles/" + into.id).flatMap(_ =>
                        call("DELETE", "/guilds/" + g + "/members/" + id + "/roles/" + from.id)).isLeft)
                    "moved " + (ids.size - failed.size) + " of " + ids.size + " members from " + from.name + " to " + into.name +
                        (if (failed.nonEmpty) " (not moved: " + failed.mkString(", ") + ")" else "") + "; delete " + from.name + " in Discord's server settings"
                }
            })
        }
    }

    // the user IDs of the members holding a role (the server's members, 1000 a page; needs the Server Members Intent)
    def holders(g : String, role : String) : Either[String, List[String]] = {
        def page(after : String, acc : List[String]) : Either[String, List[String]] =
            call("GET", "/guilds/" + g + "/members?limit=1000&after=" + after).flatMap { body =>
                Json.parse(body) match {
                    case l : List[_] =>
                        val mm = l.collect { case m : Map[_, _] => m.asInstanceOf[Map[String, Any]] }
                        val ids = mm.flatMap { m =>
                            val id = m.get("user").collect { case u : Map[_, _] => u.asInstanceOf[Map[String, Any]].get("id") }.flatten.collect { case v : String => v }
                            val has = m.get("roles").collect { case r : List[_] => r.contains(role) }.getOrElse(false)
                            id.map(_ -> has)
                        }
                        val now = acc ++ ids.filter(_._2).map(_._1)
                        if (mm.size == 1000 && ids.nonEmpty) page(ids.last._1, now) else Right(now)
                    case _ => Left("Discord didn't list the members")
                }
            }
        page("0", Nil)
    }

    def member(g : String, rest : String, method : String) : Either[String, String] = arrow(rest).flatMap { case (u, r) =>
        user(u).flatMap(id => roles(g).flatMap(findRole(_, r)).flatMap { role =>
            if (role.managed || role.name == "@everyone") Left("that role can't be given or taken")
            else call(method, "/guilds/" + g + "/members/" + id + "/roles/" + role.id)
        })
    }

    def listChannels(g : String) : Either[String, String] = for (rr <- roles(g) ; cc <- channels(g)) yield {
        def roleName(id : String) = rr.find(_.id == id).map(_.name).getOrElse(id)
        def access(c : Channel) = {
            val everyone = c.overwrites.find(o => o.id == g)
            val hidden = everyone.exists(o => (o.deny & View) != 0)
            val see = c.overwrites.filter(o => o.kind == 0 && o.id != g && (o.allow & View) != 0 && (o.deny & Send) == 0)
            val ro = c.overwrites.filter(o => o.kind == 0 && (o.deny & Send) != 0)
            (if (hidden) List("private") else Nil) ++
            (if (see.nonEmpty) List("seen by " + see.map(o => roleName(o.id)).mkString(", ")) else Nil) ++
            (if (ro.nonEmpty) List("read-only for " + ro.map(o => roleName(o.id)).mkString(", ")) else Nil)
        }
        def line(c : Channel, indent : String) = indent + typeName(c.kind) + " " + c.name + " (" + c.id + ")" + (access(c) match { case Nil => "" ; case l => ": " + l.mkString("; ") })
        val cats = cc.filter(_.kind == 4).sortBy(_.position)
        val loose = cc.filter(c => c.kind != 4 && c.parent.isEmpty).sortBy(c => (c.kind == 2, c.position))
        (loose.map(line(_, "")) ++ cats.flatMap(k => line(k, "") :: cc.filter(_.parent == k.id).sortBy(c => (c.kind == 2, c.position)).map(line(_, "  ")))).mkString("\n", "\n", "")
    }

    def channel(g : String, rest : String, category : Boolean) : Either[String, String] = {
        val (name, opts) = split(rest)
        if (name.isEmpty) return Left(if (category) "category <name> [| private] [| see <role>, ...] [| read-only <role>, ...]" else "channel <name> [| in <category>] [| text] [| topic <text>] [| private] [| see <role>, ...] [| read-only <role>, ...]")
        val allowed = Set("private", "see", "read-only") ++ (if (category) Nil else Set("in", "topic", "synced") ++ types.keySet - "category")
        val bad = opts.map(_._1).filterNot(allowed)
        if (bad.nonEmpty) return Left("not an option: " + bad.mkString(", "))
        val kinds = opts.map(_._1).filter(types.contains)
        if (kinds.size > 1) return Left("one type at most")
        val kind = if (category) 4 else kinds.headOption.map(types).getOrElse(0)
        val private_ = opts.exists(_._1 == "private")
        val see = opts.filter(_._1 == "see").flatMap(o => names(o._2))
        val ro = opts.filter(_._1 == "read-only").flatMap(o => names(o._2))
        val perms = private_ || see.nonEmpty || ro.nonEmpty
        val synced = opts.exists(_._1 == "synced")
        if (synced && perms) return Left("synced takes the category's permissions; give no private, see or read-only with it")
        val topic = opts.find(_._1 == "topic").map(_._2)

        for {
            rr <- roles(g)
            cc <- channels(g)
            parent <- opts.find(_._1 == "in").map(o => findChannel(cc, o._2, _ == 4).map(Some(_))).getOrElse(Right(None))
            seeRoles <- (see.map(findRole(rr, _)) ++ ro.map(findRole(rr, _))).collectFirst { case Left(e) => Left(e) }.getOrElse(Right(see.map(findRole(rr, _).toOption.get)))
            roRoles = ro.map(findRole(rr, _).toOption.get)
            _ <- if (private_ && (seeRoles ++ roRoles).exists(_.id == g)) Left("private hides it from @everyone; leave @everyone out of see and read-only") else Right(())
            bot <- if (perms) me.map(Some(_)) else Right(None)
            result <- {
                // read-only wins over see for a role given both
                val overwrites =
                    (if (private_) List(Overwrite(g, 0, 0, View)) else Nil) ++
                    seeRoles.filterNot(r => roRoles.exists(_.id == r.id)).distinctBy(_.id).map(r => Overwrite(r.id, 0, SeeAllow, 0)) ++
                    roRoles.distinctBy(_.id).map(r => Overwrite(r.id, 0, View | Connect, ReadOnlyDeny)) ++
                    bot.filter(_ => private_).map(b => Overwrite(b, 1, View | Connect | Send | SendInThreads, 0)).toList
                def owJson(l : List[Overwrite]) = l.map(o => "{\"id\":" + Tournament.json(o.id) + ",\"type\":" + o.kind + ",\"allow\":\"" + o.allow + "\",\"deny\":\"" + o.deny + "\"}").mkString("[", ",", "]")

                val existing = cc.filter(c => (if (category) c.kind == 4 else c.kind != 4) && (norm(c.name) == norm(name) || norm(c.name) == dashed(name)) && (category || parent.forall(_.id == c.parent)))
                existing match {
                    case Nil =>
                        val ow = if (perms) Some(overwrites) else parent.map(_.overwrites)
                        val fields = List("\"name\":" + Tournament.json(name), "\"type\":" + kind) ++
                            parent.map(p => "\"parent_id\":" + Tournament.json(p.id)) ++
                            topic.map(t => "\"topic\":" + Tournament.json(t)) ++
                            ow.map(l => "\"permission_overwrites\":" + owJson(l))
                        call("POST", "/guilds/" + g + "/channels", fields.mkString("{", ",", "}")).map(b => "created " + typeName(kind) + " " + str(obj(b), "name") + " (" + str(obj(b), "id") + ")" + parent.map(" in " + _.name).getOrElse(""))
                    case c :: Nil =>
                        if (kinds.nonEmpty && kind != c.kind) Left("it is a " + typeName(c.kind) + " channel; Discord changes a type only between text and announcement in the app")
                        else {
                            val fields =
                                topic.map(t => "\"topic\":" + Tournament.json(t)).toList ++
                                (if (perms) List("\"permission_overwrites\":" + owJson(overwrites)) else Nil) ++
                                (if (synced) cc.find(_.id == c.parent).map(p => "\"permission_overwrites\":" + owJson(p.overwrites)).toList else Nil)
                            if (synced && c.parent.isEmpty) Left("it is in no category to take permissions from")
                            else if (fields.isEmpty) Right("already there (" + c.id + ")")
                            else call("PATCH", "/channels/" + c.id, fields.mkString("{", ",", "}")).map(_ => "updated " + c.name + " (" + c.id + ")")
                        }
                    case l => Left(l.size + " channels are called " + name + "; add | in <category>, or use its ID with rename-channel and move-channel")
                }
            }
        } yield result
    }

    case class Webhook(id : String, name : String, channel : String, token : String, application : String, kind : Int) {
        def url = "https://discord.com/api/webhooks/" + id + "/" + token
    }

    def webhook(m : Map[String, Any]) = Webhook(str(m, "id"), str(m, "name"), str(m, "channel_id"), str(m, "token"), str(m, "application_id"), num(m, "type").toInt)

    def webhooks(g : String) : Either[String, List[Webhook]] = call("GET", "/guilds/" + g + "/webhooks").map(b => list(b).map(webhook))

    def findWebhook(ww : List[Webhook], v : String) : Either[String, Webhook] =
        ww.find(_.id == v.trim).map(Right(_)).getOrElse {
            ww.filter(w => norm(w.name) == norm(v)) match {
                case w :: Nil => Right(w)
                case Nil => Left("no webhook called " + v)
                case l => Left(l.size + " webhooks are called " + v + "; give its ID (webhooks lists them)")
            }
        }

    // the turns channel a webhook is set as, by the ID in the URL kept
    def slotUsing(id : String) = TurnPings.slots.filter(s => TurnPings.hookOf(s).exists(_.split('/').contains(id))).map(_.name)

    def listWebhooks(g : String) : Either[String, String] = for {
        ww <- webhooks(g)
        cc <- channels(g)
        bot <- me
    } yield ww.sortBy(w => (cc.find(_.id == w.channel).map(_.name).getOrElse(""), w.name)).map { w =>
        w.name + " (" + w.id + ") in " + cc.find(_.id == w.channel).map(_.name).getOrElse(w.channel) +
            (if (w.application == bot) ", made by the bot" else "") +
            slotUsing(w.id).map(", set as " + _).mkString
    }.mkString("\n") match { case "" => "no webhooks" ; case t => t }

    // the picture a webhook's avatar is made from, as Discord takes it
    def avatar(file : String) : Either[String, String] =
        if (!file.matches("[a-z0-9-]+\\.png")) Left("an avatar is a .png file name in webp2/lfg/webhooks/")
        else {
            val f = java.nio.file.Paths.get(Lfg.assets, "webp2", "lfg", "webhooks", file)
            if (!java.nio.file.Files.isRegularFile(f)) Left("no " + file + " in webp2/lfg/webhooks/")
            else Right("data:image/png;base64," + java.util.Base64.getEncoder.encodeToString(java.nio.file.Files.readAllBytes(f)))
        }

    def makeWebhook(g : String, rest : String) : Either[String, String] = {
        val (name, opts) = split(rest)
        def opt(k : String) = opts.find(_._1 == k).map(_._2)
        for {
            _ <- if (name.isEmpty || name.length > 80 || norm(name).contains("discord") || norm(name).contains("clyde")) Left("a webhook's name is 1 to 80 characters, without \"discord\" or \"clyde\"") else Right(())
            in <- opt("in").toRight("which channel? make-webhook <name> | in <channel>")
            target <- opt("for") match {
                case None => Right(None)
                case Some(s) => TurnPings.slot(s.trim).map(Some(_)).toRight("for takes " + TurnPings.slots.map(_.name).mkString(", "))
            }
            picture <- opt("avatar") match { case None => Right(None) ; case Some(f) => avatar(f.trim).map(Some(_)) }
            cc <- channels(g)
            c <- findChannel(cc, in, k => k == 0 || k == 5 || k == 15)
            there <- call("GET", "/channels/" + c.id + "/webhooks").map(b => list(b).map(webhook))
            fields = ("\"name\":" + Tournament.json(name)) :: picture.map(p => "\"avatar\":" + Tournament.json(p)).toList
            made <- there.filter(w => w.kind == 1 && norm(w.name) == norm(name)) match {
                case Nil => call("POST", "/channels/" + c.id + "/webhooks", fields.mkString("{", ",", "}")).map(b => ("made", webhook(obj(b))))
                case w :: Nil => call("PATCH", "/webhooks/" + w.id, fields.mkString("{", ",", "}")).map(b => ("updated", webhook(obj(b)).copy(token = w.token)))
                case l => Left(l.size + " webhooks in " + c.name + " are called " + name + "; rename one (rename-webhook <its ID> -> ...)")
            }
        } yield {
            val (what, w) = made
            what + " " + w.name + " (" + w.id + ") in " + c.name + (if (picture.nonEmpty) ", with its avatar" else "") + target.map { s =>
                if (w.token.isEmpty) "; not set as " + s.name + ": Discord didn't give the bot its URL (make one with another name)"
                else "; " + s.name + ": " + TurnPings.setHook(s, w.url)
            }.getOrElse("")
        }
    }

    def testPing(g : String, rest : String) : Either[String, String] = {
        val (name, opts) = split(rest)
        def opt(k : String) = opts.find(_._1 == k).map(_._2)
        for {
            s <- TurnPings.slot(name).toRight("test-ping takes " + TurnPings.slots.map(_.name).mkString(", "))
            url <- TurnPings.hookOf(s).toRight("no webhook set as " + s.name)
            role <- opt("role") match { case None => Right(None) ; case Some(r) => roles(g).flatMap(findRole(_, r)).map(Some(_)) }
            r <- {
                val text = role.map(r => "<@&" + r.id + "> ").getOrElse("") + opt("text").getOrElse("This is a test ping from the " + s.name + " webhook. Turn pings for these games will be posted in threads like this one.")
                def send(thread : Boolean) = {
                    val body = "{\"content\":" + Tournament.json(text.take(1900)) + ",\"allowed_mentions\":{\"parse\":[],\"roles\":[" + role.map(r => Tournament.json(r.id)).mkString + "]}" +
                        (if (thread) ",\"thread_name\":\"Test ping\"" else "") + "}"
                    val request = java.net.http.HttpRequest.newBuilder(java.net.URI.create(url + "?wait=true")).timeout(java.time.Duration.ofSeconds(15))
                        .header("Content-Type", "application/json").POST(java.net.http.HttpRequest.BodyPublishers.ofString(body, UTF_8)).build()
                    DiscordBot.client.send(request, java.net.http.HttpResponse.BodyHandlers.ofString(UTF_8))
                }
                try {
                    val a = send(true)
                    val b = if (a.statusCode / 100 == 4) send(false) else a
                    if (b.statusCode / 100 == 2) Right("posted" + (if (b eq a) " in a new thread" else " in the channel") + role.map(" mentioning @" + _.name).getOrElse(""))
                    else Left("Discord answered " + b.statusCode + ": " + obj(b.body).get("message").getOrElse(b.body.take(200)))
                }
                catch { case e : Exception => Left("Discord couldn't be asked: " + e.getMessage) }
            }
        } yield r
    }

    val words = Set("roles", "role", "rename-role", "give-role", "take-role", "channels", "category", "channel", "rename-channel", "move-channel", "order",
        "webhooks", "make-webhook", "rename-webhook", "test-ping")
}
