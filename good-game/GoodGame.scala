package hrf.gg

import slick.jdbc.HsqldbProfile.api._
import slick.jdbc.HsqldbProfile.api.DBIO.seq

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import akka.http.scaladsl.model.headers._
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.settings.ServerSettings
import akka.stream.ActorMaterializer

import ch.megard.akka.http.cors.scaladsl.CorsDirectives._

object GoodGame {
    case class User(name : String, secret : String, id : String)

    class Users(tag : Tag) extends Table[User](tag, "Users") {
        def name = column[String]("name")
        def secret = column[String]("secret")
        def id = column[String]("id", O.PrimaryKey)
        def * = (name, secret, id).mapTo[User]
    }

    val users = TableQuery[Users]


    case class Journal(name : String, public : Boolean, status : String, message : String, id : String)

    class Journals(tag : Tag) extends Table[Journal](tag, "Journals") {
        def name = column[String]("name")
        def public = column[Boolean]("public")
        def status = column[String]("status")
        def message = column[String]("message")
        def id = column[String]("id", O.PrimaryKey)
        def * = (name, public, status, message, id).mapTo[Journal]
    }

    val journals = TableQuery[Journals]


    case class Entry(journalId : String, index : Int, userId : String, text : String)

    class Entries(tag : Tag) extends Table[Entry](tag, "Entries") {
        def journalId = column[String]("journalId")
        def index = column[Int]("index")
        def userId = column[String]("userId")
        def text = column[String]("text")
        def * = (journalId, index, userId, text).mapTo[Entry]
        def pk = primaryKey("Entries" + "Key", (journalId, index))
        def journal = foreignKey("Entries" + "Journals", journalId, journals)(_.id)
        def user = foreignKey("Entries" + "Users", userId, users)(_.id)
    }

    val entries = TableQuery[Entries]


    case class AccessRight(journalId : String, userId : String, right : String)

    class AccessRights(tag : Tag) extends Table[AccessRight](tag, "AccessRights") {
        def journalId = column[String]("journalId")
        def userId = column[String]("userId")
        def right = column[String]("right")
        def * = (journalId, userId, right).mapTo[AccessRight]
        def pk = primaryKey("AccessRights" + "Key", (journalId, userId, right))
        def journal = foreignKey("AccessRights" + "Journals", journalId, journals)(_.id)
        def user = foreignKey("AccessRights" + "Users", userId, users)(_.id)
    }

    val accessRights = TableQuery[AccessRights]


    case class Play(journalId : String, userId : String, secret : String)

    class Plays(tag : Tag) extends Table[Play](tag, "Plays") {
        def journalId = column[String]("journalId")
        def userId = column[String]("userId")
        def secret = column[String]("secret")
        def * = (journalId, userId, secret).mapTo[Play]
        def journal = foreignKey("Play" + "Journals", journalId, journals)(_.id)
        def user = foreignKey("Play" + "Users", userId, users)(_.id)
    }

    val plays = TableQuery[Plays]


    def main(args : Array[String]) {
        // gg restore <database> <directory>: a new database from a copy made by Backup
        if (args.size == 3 && args(0) == "restore") {
            if (new java.io.File(args(1) + ".script").exists() || new java.io.File(args(1) + ".properties").exists()) {
                println("Database " + args(1) + " already exists.")
                return
            }

            val db = Database.forURL("jdbc:hsqldb:file:" + args(1) + ";hsqldb.cache_rows=10000;hsqldb.nio_data_file=false;shutdown=true", driver="org.hsqldb.jdbcDriver")
            Backup.restore(db, java.nio.file.Paths.get(args(2)))
            db.close()
            return
        }

        if (args.size != 6) {
            println("gg <create|run> <directory> <database> <url> <cdn> <port>")
            return
        }

        val mode = args(0)
        val database = args(1)
        val directory = args(2)
        val url = args(3)
        val cdn = args(4)
        val port = args(5).toInt

        def readFile(path : String) = {
            import java.nio.charset.StandardCharsets._
            import java.nio.file.{Files, Paths}

            new String(Files.readAllBytes(Paths.get(path)), UTF_8)
        }

        implicit class Ascii(val s : String) {
            def ascii = s.filter(c => c >= 32 && c < 128)
            def asciiplus = s.filter(c => (c >= 32 && c < 128) || (c > 158 && c < 256 && c.isLetter))
            def safe = ascii.filter(_ != '<').filter(_ != '>').filter(_ != '"').filter(_ != '\\')
            def safeplus = asciiplus.filter(_ != '<').filter(_ != '>').filter(_ != '"').filter(_ != '\\')
        }

        def newSecret(n : Int) = {
            val random = new scala.util.Random()

            0.until(n).map(_ => "abcdefghijklmnopqrstuvwxyz".charAt(random.nextInt(26))).mkString("")
        }

        val db = Database.forURL("jdbc:hsqldb:file:" + database + ";hsqldb.cache_rows=10000;hsqldb.nio_data_file=false", driver="org.hsqldb.jdbcDriver")

        object execute {
            import scala.concurrent.Await
            import scala.concurrent.duration.Duration

            def apply[E <: Effect](actions : DBIOAction[_, NoStream, E]*) = Await.result(db.run(DBIO.seq(actions : _*).withPinnedSession), Duration.Inf)
            def apply[R](action : DBIOAction[R, NoStream, Effect.Read]) : R = Await.result(db.run(action.withPinnedSession), Duration.Inf)
        }

        if (mode == "create") {
            execute(users.schema.create, journals.schema.create, entries.schema.create, accessRights.schema.create, plays.schema.create)
            println("Created database.")
            return
        }

        if (mode != "run") {
            println("Unknown mode.")
            return
        }

        implicit val system = ActorSystem()
        implicit val executionContext = system.dispatcher

        def hasRight[R, E <: Effect with Effect.Read](userId : String, userSecret : String, journalId : String, right : String)(then : => DBIOAction[R, NoStream, E]) : DBIOAction[R, NoStream, E] = {
            users.filter(_.id === userId).filter(_.secret === userSecret).result.head.flatMap { _ =>
                accessRights.filter(_.journalId === journalId).filter(_.userId === userId).filter(_.right === right).result.head.flatMap { _ =>
                    then
                }
            }
        }

        def index = readFile(directory + "/index.html")

        def html(s : String) = complete(HttpEntity(ContentTypes.`text/html(UTF-8)`, s))
        def plain(s : String) = complete(HttpEntity(ContentTypes.`text/plain(UTF-8)`, s))
        def redir(s : String) = redirect(s, StatusCodes.SeeOther)

        // Bug reports from the in-game "Report a Bug" button become GitHub issues.
        // The token (fine-grained, Issues: read and write on the repository) is read
        // from HRF_GITHUB_TOKEN or the file github-token in the working directory.
        // Without one, reports are only saved in bug-reports/.
        object bugs {
            import java.nio.charset.StandardCharsets.UTF_8
            import java.nio.file.{Files, Paths}

            def setting(env : String, file : String) : Option[String] =
                sys.env.get(env).map(_.trim).filter(_.nonEmpty).orElse {
                    val f = Paths.get(file)
                    if (Files.exists(f)) Some(new String(Files.readAllBytes(f), UTF_8).trim).filter(_.nonEmpty) else None
                }

            def token = setting("HRF_GITHUB_TOKEN", "github-token")
            def repo = setting("HRF_GITHUB_REPO", "github-repo").getOrElse("guarantee418/haunt-roll-fail")

            val recent = scala.collection.mutable.Map[String, List[Long]]()

            // at most 10 reports per address per hour
            def allow(address : String) : Boolean = synchronized {
                val now = System.currentTimeMillis()
                val times = recent.getOrElse(address, Nil).filter(_ > now - 3600 * 1000)
                recent(address) = now :: times
                times.size < 10
            }

            def json(s : String) = "\"" + s.flatMap {
                case '"' => "\\\""
                case '\\' => "\\\\"
                case '\n' => "\\n"
                case '\r' => ""
                case '\t' => "\\t"
                case c if c < ' ' => ""
                case c => c.toString
            } + "\""

            def save(title : String, body : String) {
                val dir = Paths.get("bug-reports")
                Files.createDirectories(dir)
                val name = java.time.LocalDateTime.now().toString.replace(":", "-").take(19) + "-" + newSecret(4) + ".md"
                Files.write(dir.resolve(name), ("# " + title + "\n\n" + body).getBytes(UTF_8))
            }

            val client = java.net.http.HttpClient.newHttpClient()

            def send(token : String, title : String, body : String, labels : Boolean) = {
                val request = java.net.http.HttpRequest.newBuilder(java.net.URI.create("https://api.github.com/repos/" + repo + "/issues"))
                    .header("Accept", "application/vnd.github+json")
                    .header("Authorization", "Bearer " + token)
                    .header("X-GitHub-Api-Version", "2022-11-28")
                    .header("Content-Type", "application/json")
                    .POST(java.net.http.HttpRequest.BodyPublishers.ofString("{\"title\":" + json(title) + ",\"body\":" + json(body) + (if (labels) ",\"labels\":[\"bug report\"]" else "") + "}", UTF_8))
                    .build()

                import scala.jdk.FutureConverters._

                client.sendAsync(request, java.net.http.HttpResponse.BodyHandlers.ofString(UTF_8)).asScala
            }

            // the issue URL, or "" if no token is set up
            def post(title : String, body : String) : scala.concurrent.Future[String] = token match {
                case None => scala.concurrent.Future.successful("")
                case Some(token) =>
                    send(token, title, body, true).flatMap { response =>
                        // a token that can't create the label: post without it
                        if (response.statusCode() == 422) send(token, title, body, false) else scala.concurrent.Future.successful(response)
                    }.map { response =>
                        if (response.statusCode() != 201)
                            throw new Exception("GitHub answered " + response.statusCode() + ": " + response.body().take(500))

                        "\"html_url\"\\s*:\\s*\"([^\"]+/issues/[0-9]+)\"".r.findFirstMatchIn(response.body()).map(_.group(1)).getOrElse("")
                    }
            }
        }

        val route = cors() {
            (pathPrefix("hrf")) {
                optionalHeaderValueByName("Referer") { referer =>
                    if (referer.exists(_.startsWith(url)))
                        encodeResponse {
                            getFromDirectory(directory)
                        }
                    else
                        complete(StatusCodes.NotFound, "")
                }
            } ~
            (get & path("")) {
                redir("/play")
            } ~
            (get & path("play" / "")) {
                redir("/play")
            } ~
            (get & path("play")) {
                html(index
                    .replace("<base href=\"\" />", "<base href=\"" + cdn + "\"/>")
                    .replace("data-server=\"" + "\"", "data-server=\"" + url + "\"")
                    .replace("data-meta=\"" + "\"", "data-meta=\"" + "" + "\"")
                )
            } ~
            (get & path("play" / Segment / "")) { meta =>
                redir("/play/" + meta)
            } ~
            (get & path("play" / Segment)) { meta =>
                html(index
                    .replace("<base href=\"\" />", "<base href=\"" + cdn + "\"/>")
                    .replace("data-server=\"" + "\"", "data-server=\"" + url + "\"")
                    .replace("data-meta=\"" + "\"", "data-meta=\"" + meta + "\"")
                )
            } ~
            (get & path("play" / Segment / Segments)) { (meta, secret) =>
                if (secret.length == 1 && secret(0).length == 16) {
                    val (user, play) = execute(plays.filter(_.secret === secret(0)).flatMap { play =>
                        users.filter(_.id === play.userId).map((_, play))
                    }.result.head)

                    html(index
                        .replace("<base href=\"\" />", "<base href=\"" + cdn + "\"/>")
                        .replace("data-server=\"" + "\"", "data-server=\"" + url + "\"")
                        .replace("data-meta=\"" + "\"", "data-meta=\"" + meta + "\"")
                        .replace("data-user=\"" + "\"", "data-user=\"" + user.id + "\"")
                        .replace("data-secret=\"" + "\"", "data-secret=\"" + user.secret + "\"")
                        .replace("data-lobby=\"" + "\"", "data-lobby=\"" + play.journalId + "\"")
                    )
                }
                else
                    html(index
                        .replace("<base href=\"\" />", "<base href=\"" + cdn + "\"/>")
                        .replace("data-server=\"" + "\"", "data-server=\"" + url + "\"")
                        .replace("data-meta=\"" + "\"", "data-meta=\"" + meta + "\"")
                    )
            } ~
            (post & path("report-bug")) {
                (optionalHeaderValueByName("Referer") & extractClientIP) { (referer, ip) =>
                    if (!referer.exists(_.startsWith(url)))
                        complete(StatusCodes.Forbidden, "")
                    else
                    if (!bugs.allow(ip.toOption.map(_.getHostAddress).getOrElse("unknown")))
                        complete(StatusCodes.TooManyRequests, "Too many reports, try again later.")
                    else
                        decodeRequest {
                            entity(as[String]) { text =>
                                val title = text.takeWhile(_ != '\n').take(120).trim.filter(_ >= ' ') match {
                                    case "" => "Bug report"
                                    case t => t
                                }
                                val body = text.dropWhile(_ != '\n').drop(1).take(60000) + "\n\n---\n_Sent with the in-game Report a Bug button._\n"

                                bugs.save(title, body)

                                onComplete(bugs.post(title, body)) {
                                    case scala.util.Success(issue) => plain(issue)
                                    case scala.util.Failure(e) =>
                                        println("Bug report not posted: " + e.getMessage)
                                        complete(StatusCodes.BadGateway, "Saved on the server, but posting to GitHub failed.")
                                }
                            }
                        }
                }
            } ~
            (post & path("new-user")) {
                decodeRequest {
                    entity(as[String]) { body =>
                        val name = body.take(32).trim.safeplus
                        val user = User(name, newSecret(16), newSecret(16))
                        execute(users += user)
                        plain(user.id + "\n" + user.secret)
                    }
                }
            } ~
            (post & path("new-journal" / Segment / Segment)) { case (userId, userSecret) =>
                decodeRequest {
                    entity(as[String]) { body =>
                        val name = body.take(128).trim.safeplus
                        val id = newSecret(16)
                        execute(users.filter(_.id === userId).filter(_.secret === userSecret).map(_.id).result.head.flatMap { userId =>
                            seq(
                                journals += Journal(name, false, "", "", id),
                                accessRights += AccessRight(id, userId, "full"),
                                accessRights += AccessRight(id, userId, "read"),
                                accessRights += AccessRight(id, userId, "append")
                            )
                        })
                        plain(id)
                    }
                }
            } ~
            (post & path("grant-read" / Segment / Segment / Segment / Segment)) { case (userId, userSecret, journalId, anotherUser) =>
                execute(hasRight(userId, userSecret, journalId, "full") {
                    users.filter(_.id === anotherUser).result.head.flatMap { _ =>
                        accessRights += AccessRight(journalId, anotherUser, "read")
                    }
                })
                plain("")
            } ~
            (post & path("grant-read-append" / Segment / Segment / Segment / Segment)) { case (userId, userSecret, journalId, anotherUser) =>
                execute(hasRight(userId, userSecret, journalId, "full") {
                    users.filter(_.id === anotherUser).result.head.flatMap { _ =>
                        accessRights ++= List(AccessRight(journalId, anotherUser, "read"), AccessRight(journalId, anotherUser, "append"))
                    }
                })
                plain("")
            } ~
            (post & path("new-play" / Segment / Segment / Segment)) { case (userId, userSecret, journalId) =>
                decodeRequest {
                    entity(as[String]) { body =>
                        val name = body.take(32).trim.safeplus
                        val secret = newSecret(16)
                        val user = User(name, newSecret(16), newSecret(16))
                        execute(hasRight(userId, userSecret, journalId, "full") {
                            seq(
                                users += user,
                                accessRights += AccessRight(journalId, user.id, "read"),
                                accessRights += AccessRight(journalId, user.id, "append"),
                                plays += Play(journalId, user.id, secret)
                            )
                        })
                        plain(user.id + "\n" + secret)
                    }
                }
            } ~
            (get & path("read" / Segment / Segment / Segment / IntNumber)) { (userId, userSecret, journalId, from) =>
                val log = execute(hasRight(userId, userSecret, journalId, "read") {
                    entries.filter(_.journalId === journalId).filter(_.index >= from).map(_.text).result
                })
                plain(log.mkString("\n"))
            } ~
            (post & path("append" / Segment / Segment / Segment / IntNumber)) { (userId, userSecret, journalId, from) =>
                decodeRequest {
                    entity(as[String]) { body =>
                        val ss = body.split('\n').toList.map(_.asciiplus)

                        try {
                            // No gaps: a player who has not reloaded since a game fix shortened the log gets a conflict
                            val next = execute(entries.filter(_.journalId === journalId).map(_.index).max.result).map(_ + 1).getOrElse(0)

                            if (from > next)
                                complete(StatusCodes.Conflict)
                            else {
                                execute(hasRight(userId, userSecret, journalId, "append") {
                                    entries ++= 0.until(ss.size).map(n => Entry(journalId, from + n, userId, ss(n)))
                                })
                                complete(StatusCodes.Accepted)
                            }
                        }
                        catch {
                            case e : java.sql.SQLIntegrityConstraintViolationException => complete(StatusCodes.Conflict)
                        }
                    }
                }
            }
        }
// create a user database and a journal database
        implicit val materializer = ActorMaterializer()
        import akka.stream.scaladsl._  
        import akka.http.scaladsl.util._
        import akka.http.scaladsl.model.StatusCodes
        import akka.http.scaladsl.server.Directives._
        import akka.http.scaladsl.server.Route
        import akka.http.scaladsl.settings.ServerSettings
        import akka.http.scaladsl.Http
        import hrf.gg.Ssl
        import scala.util.Using
        import scala.concurrent.Future
        import akka.http.scaladsl.model.headers.Location
    






        val settings = ServerSettings("").withRemoteAddressAttribute(true)

        var server = Http().newServerAt("0.0.0.0", port).withSettings(settings)

        val keyFile = new java.io.File("certificate.pkcs12")

        if (keyFile.exists()) {
            val hcc = Ssl.serverHttpsContext(keyFile, "")

            server = server.enableHttps(hcc)
        }

        val bindingFuture = server.bind(route)

        println("Started server.")

        Backup.start(db)

        // Webroot for Let's Encrypt HTTP-01 challenges (certbot --webroot -w good-game/acme)
        val acmeDir = new java.io.File("acme")

        if (port != 80 && (keyFile.exists() || acmeDir.isDirectory())) {
            val base = Uri(url)

            val redirroute = get {
                pathPrefix(".well-known" / "acme-challenge") {
                    getFromDirectory("acme/.well-known/acme-challenge")
                } ~
                extractUri { uri =>
                    redirect(uri.withScheme(base.scheme).withAuthority(base.authority), StatusCodes.MovedPermanently)
                }
            }

            Http().newServerAt("0.0.0.0", 80).bind(redirroute).onComplete {
                case scala.util.Success(_) => println("Started redirect server.")
                case scala.util.Failure(e) => println("Failed to start redirect server: " + e)
            }
        }

        while (true)
            Thread.sleep(1000)

        bindingFuture.flatMap(_.unbind()).onComplete(_ => system.terminate())
    }
}
