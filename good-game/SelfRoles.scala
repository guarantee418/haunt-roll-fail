package hrf.gg

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.Files

// ROLES MEMBERS PICK FOR THEMSELVES (asked for on 2026-11-24: "users on the server can assign and unassign roles to themselves"): the
// game and ping roles (root-lfg, ng-lfg, pincer, coh, the spectator roles) that the owner lists, and only those. A member types /roles,
// or taps "Pick my roles" under the bot's role picker post, and gets a menu only they see, their roles already ticked; ticking and
// unticking gives and takes them. The list is tournament/self-roles.tsv (role ID, name), set by tournament changes:
//
//   self-roles                                lists the roles members may pick, and the roles that could be added
//   self-role <role>, <role>, ...             adds roles to the list
//   self-role-off <role>, ...                 takes roles off it (members keep them)
//   role-picker <channel> [| text <text>]     posts a message there with a "Pick my roles" button (the bot needs to post there)
//
// A role goes on the list only if it gives no server permission at all, isn't a bot's or integration's, isn't @everyone and sits under
// the bot's own role, and every one is checked again each time a member picks: a role given a permission since is never handed out.
// So Manager, or any role that moderates, can't be self-assigned even by mistake.
// The site's My Account page offers the same roles (2026-11-26, asked for): /discord/roles lists them for a name the device's player code
// holds, with that name's Discord ticked or not (site), and /discord/role gives or takes one (siteSet), with the same checks. Discord's menus hold 25 options, so the list does too.
object SelfRoles extends BotCommand {
    val name = "roles"
    val file = Tournament.dir.resolve("self-roles.tsv")
    val max = 25

    def registration = "{\"name\":\"roles\",\"type\":1,\"description\":\"Pick the game and ping roles you want (only you see the menu)\",\"options\":[]}"

    def listed : List[String] = if (!Files.exists(file)) Nil else
        new String(Files.readAllBytes(file), UTF_8).split('\n').toList.map(_.split('\t').head.trim).filter(Tournament.isDiscordId).distinct

    def save(l : List[DiscordServer.Role]) = synchronized {
        Files.createDirectories(file.getParent)
        Files.write(file, l.map(r => r.id + "\t" + r.name.replace('\t', ' ') + "\n").mkString.getBytes(UTF_8))
    }

    // the highest position among the bot's own roles: Discord lets it give only roles under that
    def botTop(g : String, rr : List[DiscordServer.Role]) : Either[String, Long] = for {
        me <- DiscordServer.me
        m <- DiscordServer.call("GET", "/guilds/" + g + "/members/" + me)
    } yield {
        val mine = DiscordServer.obj(m).get("roles").collect { case l : List[_] => l.collect { case s : String => s } }.getOrElse(Nil)
        rr.filter(r => mine.contains(r.id)).map(_.position).maxOption.getOrElse(0L)
    }

    // why a role can't be self-assigned, if it can't
    def unsafe(r : DiscordServer.Role, top : Long) : Option[String] =
        if (r.name == "@everyone") Some("@everyone is everyone's")
        else if (r.managed) Some("it belongs to a bot or integration")
        else if (r.permissions != 0) Some("it gives server permissions")
        else if (r.position >= top) Some("it isn't under the bot's role")
        else None

    // the listed roles that are safe to hand out right now
    def offered(g : String) : Either[String, List[DiscordServer.Role]] = for {
        rr <- DiscordServer.roles(g)
        top <- botTop(g, rr)
    } yield {
        val ids = listed
        rr.filter(r => ids.contains(r.id) && unsafe(r, top).isEmpty).sortBy(-_.position).take(max)
    }

    def memberRoles(g : String, uid : String) : Either[String, List[String]] =
        DiscordServer.call("GET", "/guilds/" + g + "/members/" + uid).map(b =>
            DiscordServer.obj(b).get("roles").collect { case l : List[_] => l.collect { case s : String => s } }.getOrElse(Nil))

    // the menu, only the member sees it: every role offered, theirs ticked; kind 4 a new reply, 7 an edit of it
    def menu(g : String, have : List[String], kind : Int, note : String = "") : String = offered(g) match {
        case Left(e) =>
            println("Roles: " + e)
            Lfg.privately("The roles can't be shown right now.")
        case Right(Nil) => Lfg.privately("There are no roles to pick yet.")
        case Right(rr) =>
            val text = (if (note.nonEmpty) note + "\n\n" else "") + "Tick the roles you want and untick the ones you don't. A role pings you when a game of it is looking for players or waiting on you."
            val select = LfgSetup.select("roles:pick", "Your roles", 0, rr.size, rr.map(r => ("@" + r.name, r.id, have.contains(r.id), "", "")))
            Lfg.reply(kind, "{\"content\":" + Lfg.json(text) + ",\"flags\":64,\"components\":[" + select + "],\"allowed_mentions\":{\"parse\":[]}}")
    }

    // /roles typed: the member's roles asked from Discord, as a command's run isn't given them
    def run(uid : String, user : String, data : Lfg.J) : String = DiscordBot.guild match {
        case None => Lfg.privately("The roles can't be shown right now.")
        case Some(g) => memberRoles(g, uid) match {
            case Left(e) => println("Roles: " + e) ; Lfg.privately("The roles can't be shown right now.")
            case Right(have) => menu(g, have, 4)
        }
    }

    // the button under the role picker post, and the menu's picks; have is the member's roles as the interaction gave them
    def press(uid : String, have : List[String], custom : String, values : List[String]) : String = DiscordBot.guild match {
        case None => Lfg.privately("The roles can't be changed right now.")
        case Some(g) => custom match {
            case "roles:open" => menu(g, have, 4)
            case "roles:pick" => offered(g) match {
                case Left(e) => println("Roles: " + e) ; Lfg.privately("The roles can't be changed right now.")
                case Right(rr) =>
                    // only roles on offer are ever given or taken, whatever the menu sent
                    val add = rr.filter(r => values.contains(r.id) && !have.contains(r.id))
                    val take = rr.filter(r => !values.contains(r.id) && have.contains(r.id))
                    val done = add.map(r => r -> DiscordServer.call("PUT", "/guilds/" + g + "/members/" + uid + "/roles/" + r.id)) ++
                        take.map(r => r -> DiscordServer.call("DELETE", "/guilds/" + g + "/members/" + uid + "/roles/" + r.id))
                    done.collect { case (r, Left(e)) => println("Roles: @" + r.name + " for " + uid + ": " + e) }
                    def names(l : List[DiscordServer.Role]) = l.map("@" + _.name).mkString(", ")
                    val added = add.filter(r => done.exists(d => d._1 == r && d._2.isRight))
                    val taken = take.filter(r => done.exists(d => d._1 == r && d._2.isRight))
                    val failed = done.filter(_._2.isLeft).map(_._1)
                    val note = List(
                        Some(added).filter(_.nonEmpty).map("Added " + names(_) + "."),
                        Some(taken).filter(_.nonEmpty).map("Removed " + names(_) + "."),
                        Some(failed).filter(_.nonEmpty).map("Couldn't change " + names(_) + "; try again later."),
                    ).flatten match { case Nil => "No change." ; case l => l.mkString(" ") }
                    val now = (have.filterNot(id => taken.exists(_.id == id)) ++ added.map(_.id)).distinct
                    menu(g, now, 7, note)
            }
            case _ => Lfg.privately("That isn't something I know.")
        }
    }

    // what each role is for, on the site
    def about(r : DiscordServer.Role) : String = r.name.toLowerCase match {
        case "root-lfg" => "Pinged when a Root game looks for players"
        case "ng-lfg" => "Pinged when a Northgard game looks for players"
        case "pincer" => "Pinged when a Pincer game looks for players"
        case "coh" => "Pinged when a Company of Heroes game looks for players"
        case "spectator" => "See the game channels without being pinged for games"
        case n if Heckler.roleName.exists(_.equalsIgnoreCase(n)) => "Your turn pings come from the game's heckler, who mocks your play"
        case _ => ""
    }

    // the user ID behind a name the code holds, or why there is none
    def userOf(key : String, name : String) : Either[String, String] = {
        val k = Ranking.clean(name).toLowerCase
        if (!Tournament.ownedKeys(key).contains(k)) Left("no")
        else Tournament.storedId(k).toRight("nodiscord")
    }

    // the roles for the site: "id\tname\ton|off\tabout" lines, or one word: no, nodiscord, notmember, unavailable
    def site(key : String, name : String) : String = (for {
        uid <- userOf(key, name)
        g <- DiscordBot.guild.toRight("unavailable")
        rr <- offered(g).left.map(_ => "unavailable")
        have <- DiscordServer.call("GET", "/guilds/" + g + "/members/" + uid) match {
            case Left(e) if e.contains("404") || e.contains("Unknown Member") => Left("notmember")
            case Left(e) => println("Roles: " + e) ; Left("unavailable")
            case Right(b) => Right(DiscordServer.obj(b).get("roles").collect { case l : List[_] => l.collect { case s : String => s } }.getOrElse(Nil))
        }
    } yield rr.map(r => List(r.id, r.name, if (have.contains(r.id)) "on" else "off", about(r)).map(_.replace('\t', ' ').replace('\n', ' ')).mkString("\t")).mkString("\n")).fold(x => x, x => if (x.isEmpty) "none" else x)

    // give or take one role on offer: "ok", or why not
    def siteSet(key : String, name : String, role : String, on : Boolean) : String = (for {
        uid <- userOf(key, name)
        g <- DiscordBot.guild.toRight("unavailable")
        rr <- offered(g).left.map(_ => "unavailable")
        r <- rr.find(_.id == role).toRight("notoffered")
        _ <- DiscordServer.call(if (on) "PUT" else "DELETE", "/guilds/" + g + "/members/" + uid + "/roles/" + r.id).left.map { e => println("Roles: @" + r.name + " for " + uid + ": " + e) ; if (e.contains("404") || e.contains("Unknown Member")) "notmember" else "unavailable" }
    } yield "ok").fold(x => x, x => x)

    // the tournament changes
    def change(word : String, rest : String) : String = (DiscordBot.guild match {
        case None => Left("no Discord server known (a webhook, or the guild change)")
        case Some(g) => for {
            rr <- DiscordServer.roles(g)
            top <- botTop(g, rr)
            r <- word match {
                case "self-roles" =>
                    val ids = listed
                    def line(r : DiscordServer.Role) = "@" + r.name + " (" + r.id + ")" + unsafe(r, top).map(": not offered, " + _).getOrElse("")
                    Right("\nmembers may pick: " + (rr.filter(r => ids.contains(r.id)).sortBy(-_.position).map(line) match { case Nil => "none" ; case l => l.mkString("\n  ", "\n  ", "") }) +
                        "\ncould be added: " + (rr.filter(r => !ids.contains(r.id) && unsafe(r, top).isEmpty).sortBy(-_.position).map(line) match { case Nil => "none" ; case l => l.mkString(", ") }) +
                        (ids.filterNot(id => rr.exists(_.id == id)) match { case Nil => "" ; case l => "\nno longer on the server: " + l.mkString(", ") }))
                case "self-role" =>
                    val found = DiscordServer.names(rest).map(DiscordServer.findRole(rr, _))
                    found.collectFirst { case Left(e) => Left(e) }.getOrElse {
                        val add = found.collect { case Right(r) => r }
                        add.map(r => unsafe(r, top).map("@" + r.name + " can't be self-assigned: " + _)).flatten match {
                            case e :: _ => Left(e)
                            case Nil =>
                                val kept = rr.filter(r => listed.contains(r.id))
                                val all = (kept ++ add).distinctBy(_.id)
                                if (all.size > max) Left("at most " + max + " roles (Discord's menus hold no more)")
                                else { save(all) ; Right("members may pick " + all.map("@" + _.name).mkString(", ")) }
                        }
                    }
                case "self-role-off" =>
                    val found = DiscordServer.names(rest).map(DiscordServer.findRole(rr, _))
                    found.collectFirst { case Left(e) => Left(e) }.getOrElse {
                        val off = found.collect { case Right(r) => r.id }
                        val all = rr.filter(r => listed.contains(r.id) && !off.contains(r.id))
                        save(all)
                        Right("members may pick " + (if (all.isEmpty) "no roles" else all.map("@" + _.name).mkString(", ")))
                    }
                case "role-picker" =>
                    val (ch, opts) = DiscordServer.split(rest)
                    val text = opts.find(_._1 == "text").map(_._2).getOrElse("**Pick your roles**\nTap the button to get a menu only you see, tick the game and ping roles you want and untick the ones you don't. You can also type `/roles`.")
                    DiscordServer.channels(g).flatMap(DiscordServer.findChannel(_, ch, k => k == 0 || k == 5)).flatMap { c =>
                        DiscordServer.call("POST", "/channels/" + c.id + "/messages", "{\"content\":" + Lfg.json(text.take(1900)) +
                            ",\"allowed_mentions\":{\"parse\":[]},\"components\":[{\"type\":1,\"components\":[" + LfgSetup.button("Pick my roles", 1, "roles:open") + "]}]}")
                            .map(_ => "posted in #" + c.name)
                    }
            }
        } yield r
    }).fold("not done: " + _, x => x)

    val words = Set("self-roles", "self-role", "self-role-off", "role-picker")
}
