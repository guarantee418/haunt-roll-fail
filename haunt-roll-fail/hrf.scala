package hrf
//
//
//
//
import hrf.colmat._
import hrf.compute._
// import hrf.logger._
//
//
//
//

import hrf.elem._
import hrf.html._
import hrf.meta._
import hrf.web._
import hrf.base._
import hrf.ui._
import hrf.loader._
import hrf.options._
import hrf.quine._

import org.scalajs.dom

import scalajs.js.timers.setTimeout
import scalajs.js.Dynamic

import scala.collection.mutable


object HRF {
    val version = BuildInfo.version

    // Images are kept in Cache Storage under this name and never refetched, so bump it whenever an image
    // changes in place (same file name), or browsers keep showing the old one
    val imageDataVersion = "as-of-2026-11-15-ee-new-roost"

    def now() = new scalajs.js.Date()

    val startAt = now()

    def uptime() : Int = (now().getTime() - startAt.getTime()).toInt

    val defaultGlyph = getElem("icon").asInstanceOf[dom.html.Link].href

    def glyph(s : String) = getElem("icon").asInstanceOf[dom.html.Link].href = s

    val imageCache = new CachedBlobImageLoader("hrf-image-cache-" + imageDataVersion)

    // Drop the image caches of older versions (Cache Storage is only there in secure contexts)
    dom.window.caches.toOption.foreach { caches =>
        caches.keys().`then`[Null]({ names : scalajs.js.Array[String] =>
            names.filter(n => n.startsWith("hrf-image-cache-") && n != "hrf-image-cache-" + imageDataVersion).foreach(n => caches.delete(n))
            null
        } : scalajs.js.Function1[scalajs.js.Array[String], scalajs.js.|[Null, scalajs.js.Thenable[Null]]])
    }
    val stringCache = new CachedStringLoader("hrf-page-cache")
    val stringLoader = StringLoader

    private val settings = getElem("settings").?

    def hash = dom.window.location.hash.drop(1)
    val search = dom.window.location.search.drop(1)

    private def cookieParam(p : String) = web.getCookie("hrf-param-" + p)
    private def settingsParam(p : String) = settings./~(_.getAttribute("data-" + p).?).but("")
    private def hashParam(p : String)  = hash.split('|').$./(_.split('=')).%(_(0) == p).single./(_.drop(1).join("=")).map(java.net.URLDecoder.decode(_, "UTF-8"))
    private def urlParam(p : String) = search.split('&').$./(_.split('=')).%(_(0) == p).single./(_.drop(1).join("=")).map(java.net.URLDecoder.decode(_, "UTF-8"))

    private val params = mutable.Map[String, |[String]]()

    dom.window.onhashchange = e => params.clear()

    if (cookieParam("cache-html").any)
        HRF.stringCache.queue(dom.window.location.origin + dom.window.location.pathname)

    def param(p : String) = params.getOrElseUpdate(p, hashParam(p) || urlParam(p) || cookieParam(p) || settingsParam(p))

    def flag(p : String) = param(p).but("-").but("false").but("no").any

    def paramInt(p : String) = param(p)./~(_.toIntOption)

    def paramList(p : String) = param(p)./~(_.split(' ').$)

    val versionOverride = hashParam("version") || urlParam("version")
    var lfgSent = false

    var segments = dom.window.location.pathname.split('/').$.drop(3)

    var speed = paramInt("speed").|(640)

    var server = param("server").map(url => url.endsWith("/").?(url.dropRight(1)).|(url))
    var lobby = param("lobby")
    var user = param("user")
    var secret = param("secret")

    val offline = flag("offline") || dom.window.location.protocol == "file:"

    var offsite = server.has(dom.window.location.origin).not.?(dom.window.location.origin + dom.window.location.pathname)

    val embedded = flag("embedded-assets")
    // Lazy, as a small replay link (Quine.openSmall) sets it on the page before the game starts
    lazy val replay = flag("replay")

    // An online game opened through its Spectator link: this browser plays no clan, so the waiting pane says so
    var spectator = false

    val metaUIs : $[(MetaGame, BaseUI)] = $(
        root.Meta -> root.UI,
        root.MetaAdset -> root.UI,
        root.MetaMirror -> root.UI,
        cthw.Meta -> cthw.UI,
        dwam.Meta -> dwam.UI,
        vast.Meta -> vast.UI,
        arcs.Meta -> arcs.UI,
        arcs.MetaBR -> arcs.UI,
        doms.Meta -> doms.UI,
        inis.Meta -> inis.UI,
        coup.Meta -> coup.UI,
        sehi.Meta -> sehi.UI,
        suok.Meta -> suok.UI,
        yarg.Meta -> yarg.UI,
        nort.Meta -> nort.UI,
        nort.MetaAdset -> nort.UI,
    )

    val metas = metaUIs.lefts

    val rootMetas = $[MetaGame](root.Meta, root.MetaAdset, root.MetaMirror)

    val menuMetas = $[MetaGame](root.Meta, nort.Meta)

    val html = dom.window.location.origin + "/play/"
    val script = dom.document.getElementById("script").asInstanceOf[dom.html.Script].src

    var originalOuterHtml : String = ""

    def onKey(filter : dom.KeyboardEvent => Boolean)(process : => Unit) {
        val old = dom.document.onkeyup
        dom.document.onkeyup = (e) => {
            if (filter(e)) {
                process
            }
            else
            if (scalajs.js.isUndefined(old).not && old != null) {
                old(e)
            }
        }
    }

    def writeOnlineGame(og : OnlineGame) : String = (
        $("version " + og.version) ++
        $("meta " + og.meta) ++
        $("title " + og.title) ++
        og.options./("option " + _) ++
        og.links./(l => "link " + l.faction + " " + l.key + " " + l.note) ++
        $("spectate " + og.spectate) ++
        og.status./("status " + _)
    ).join("\n")

    def parseOnlineGame(lobby : String, time : Double, body : String) : OnlineGame = {
        val lines = body.split('\n').$
        def p(prefix : String) = lines.%(_.startsWith(prefix))./(_.substring(prefix.length))
        def pp(prefix : String) = p(prefix)./(_.split(' ').$)

        OnlineGame(time, p("version ").single.|("unknown.version"), p("meta ").single.|("unknown-meta"), lobby, p("title ").single.|("Unnamed Game"), p("option "), pp("link ")./(l => PlayerLink(l(0), l(1), l.drop(2).join(" "))), p("spectate ").single.|("no/spectator/link"), p("status "))
    }

    def writeLocalGame(lg : LocalGame) : String = (
        $("version " + lg.version) ++
        $("title " + lg.title) ++
        $("seating " + lg.seating.join(" ")) ++
        lg.bots./(b => "bot " + b.faction + " " + b.bot) ++
        $("options " + lg.options.join(" "))
    ).join("\n")

    def parseLocalGame(kind : String, time : Double, body : String) : LocalGame = {
        val lines = body.split('\n').$
        def p(prefix : String) = lines.%(_.startsWith(prefix))./(_.substring(prefix.length))

        LocalGame(kind, time, p("version ").single.|("unknown.version"), p("title ").single.|("Unnamed Game"), p("seating ").single./(_.split(' ').$.but("")).|($), p("bot ")./(_.split(' ').$)./~(l => (l.num >= 2).?(BotAssigned(l(0), l.drop(1).join(" ")))), p("options ").single./(_.split(' ').$.but("")).|($))
    }

    def main(args : Array[String]) {
        val fonts = dom.document.fonts

        fonts.forEach { c =>
            fonts.load("20px " + c.family)
        }

        if (uptime() < 3000)
            fonts.forEach { c =>
                if (c.status != "loaded") {
                    setTimeout(20) { main(args) }
                    return
                }
            }

        if (offsite.any && lobby.none && param("play").any) {
            get(server.get + "/get-play/" + param("play").get) { r =>
                val l = r.splt("\n")

                user = l.lift(0)
                secret = l.lift(1)
                lobby = l.lift(2)

                main(args)
            }
        }
        else
        if (fonts.status == "loading" && uptime() < 3000)
            setTimeout(20) { main(args) }
        else
        if (dom.document.readyState == dom.DocumentReadyState.complete)
            onDocumentLoad()
        else
            dom.window.onload = (e) => onDocumentLoad()
    }

    private var smallReplayOpened = false

    def onDocumentLoad() {
        if (smallReplayOpened.not && getElem("lobby") == null && hashParam(Quine.smallReplayKey).any) {
            smallReplayOpened = true
            Quine.openSmall(hashParam(Quine.smallReplayKey).get) {
                params.clear()
                onDocumentLoad()
            }
            return
        }

        originalOuterHtml = "<!doctype html>\n<html>\n    " + dom.document.documentElement.innerHTML + "\n<html>"

        HRFR.load(resources => new HRFUI()(resources))
    }
}

class Quants(duration : Int, count : Int, onQuant : () => Boolean) {
    private var last = HRF.uptime()
    private var n = 0

    def continue(f : () => Unit) {
        val now = HRF.uptime()

        if (now - last < duration && n < count) {
            n += 1
            f()
        }
        else {
            last = now
            n = 0

            // println("quanting...")

            if (onQuant())
                setTimeout(0)(continue(f))
        }
    }
}


trait Switches {
    def canSwitches : Boolean
    def getSwitches : $[String]
    def enableSwitch(s : String)
    def disableSwitch(s : String)
}

object NoSwitches extends Switches {
    def canSwitches = false
    def getSwitches = $
    def enableSwitch(s : String) {}
    def disableSwitch(s : String) {}
}

trait Callbacks {
    def switches : Switches
    def canPlayAgain : Boolean
    def playAgain() : Unit
    def saveReplay(onSave : => Unit) : Unit
    def saveSmallReplay(onSave : => Unit) : Unit
    def saveReplayOnline(replaceBots : Boolean, mergeHumans : Boolean)(onSave : String => Unit) : Unit
    def settings : $[Setting]
    def editSettings(onEdit : => Unit) : Unit
    def canReportBug : Boolean
    def reportBug() : Unit
    def canReturnToTitle : Boolean
    def returnToTitle() : Unit
    // A local game (hotseat) with more than one human player on this device
    def hotseat : Boolean
}

sealed trait Difficulty

case object Off extends Difficulty
case object Recorded extends Difficulty
case object Human extends Difficulty
case class BotDebug(name : String) extends Difficulty
case class Bot(name : String) extends Difficulty
case object AllVsHuman extends Difficulty

case class OnlineGame(time : Double, version : String, meta : String, lobby : String, title : String, options : $[String], links : $[PlayerLink], spectate : String, status : $[String])

case class PlayerLink(faction : String, key : String, note : String)

case class BotAssigned(faction : String, bot : String)

// A local game saved in the browser, continued from its menu; kind is "solo", "quick", "hotseat" or "tutorial"
case class LocalGame(kind : String, time : Double, version : String, title : String, seating : $[String], bots : $[BotAssigned], options : $[String])

case class HotseatGame(time : Double, version : String, meta : String, title : String, options : $[String], bots : $[BotAssigned], status : $[String])

object HRFR {
    val original = Resources(ImageResources(Map(), Map(
        "question-mark" -> "/hrf/question-mark.png",
        "external-link" -> "/hrf/external-link.png",

        "battle" -> "/hrf/webp2/root/images/icon/battle.webp",
        "swap" -> "/hrf/webp2/root/images/icon/swap.webp",

        "vb-title-tinker" -> "/hrf/webp2/root/images/faction/hero/vb-title-tinker.webp",
        "vb-char-tinker"  -> "/hrf/webp2/root/images/faction/hero/vb-char-tinker.webp",
        "vb-title-harrier" -> "/hrf/webp2/root/images/faction/hero/vb-title-harrier.webp",
        "vb-char-harrier"  -> "/hrf/webp2/root/images/faction/hero/vb-char-harrier.webp",
        "vb-title-scoundrel" -> "/hrf/webp2/root/images/faction/hero/vb-title-scoundrel.webp",
        "vb-char-scoundrel"  -> "/hrf/webp2/root/images/faction/hero/vb-char-scoundrel.webp",
        "vb-title-arbiter" -> "/hrf/webp2/root/images/faction/hero/vb-title-arbiter.webp",
        "vb-char-arbiter"  -> "/hrf/webp2/root/images/faction/hero/vb-char-arbiter.webp",
        "vb-title-ronin" -> "/hrf/webp2/root/images/faction/hero/vb-title-ronin.webp",
        "vb-char-ronin"  -> "/hrf/webp2/root/images/faction/hero/vb-char-ronin.webp",
        "vb-title-folk-hero" -> "/hrf/webp2/root/images/faction/hero/vb-title-folk-hero.webp",
        "vb-char-folk-hero"  -> "/hrf/webp2/root/images/faction/hero/vb-char-folk-hero.webp",

        "item-boots"     -> "/hrf/webp2/root/images/item/item-boots.webp",
        "item-coins"     -> "/hrf/webp2/root/images/item/item-coins.webp",
        "item-sword"     -> "/hrf/webp2/root/images/item/item-sword.webp",
        "item-torch"     -> "/hrf/webp2/root/images/item/item-torch.webp",
        "item-bag"       -> "/hrf/webp2/root/images/item/item-bag.webp",
        "item-crossbow"  -> "/hrf/webp2/root/images/item/item-crossbow.webp",
        "item-hammer"    -> "/hrf/webp2/root/images/item/item-hammer.webp",
        "item-teapot"    -> "/hrf/webp2/root/images/item/item-teapot.webp",

        "quest:errand-fox"             -> "/hrf/webp2/root/images/card/quest/revised/errand-fox.webp",
        "quest:errand-rabbit"          -> "/hrf/webp2/root/images/card/quest/revised/errand-rabbit.webp",
        "quest:escort-mouse"           -> "/hrf/webp2/root/images/card/quest/revised/escort-mouse.webp",
        "quest:repair-a-shed-rabbit"   -> "/hrf/webp2/root/images/card/quest/revised/repair-a-shed-rabbit.webp",
        "quest:fundraising-fox"        -> "/hrf/webp2/root/images/card/quest/revised/fundraising-fox.webp",
        "quest:escort-fox"             -> "/hrf/webp2/root/images/card/quest/revised/escort-fox.webp",

        // pictured by the fan decks' ? on the setup screen, before any game's assets are loaded
        "action-sword"         -> "/hrf/webp2/root/images/card/action/action-sword.webp",
        "dark-dark-dominance"  -> "/hrf/webp2/root/images/card/dark/dark-dark-dominance.webp",
        "franken-bird-ambush"  -> "/hrf/webp2/root/images/card/franken/franken-bird-ambush.webp",
        "upstarts-murder-hawk" -> "/hrf/webp2/root/images/card/upstarts/upstarts-murder-hawk.webp",
        "sorcery-wizards-tower" -> "/hrf/webp2/root/images/card/sorcery/sorcery-wizards-tower.webp",

        "borscht-kitchens"        -> "/hrf/webp2/root/images/card/deck/borscht-kitchens.webp",
    ), HRF.imageCache), () => Map())

    val loader = HRF.embedded.?(new WrappedEmbeddedImageLoader(s => "asset-" + s)).|(HRF.imageCache)

    def load(onLoad : Resources => Unit) {
        if (HRF.replay) {
            onLoad(Resources(ImageResources(Map(), Map(), HRF.imageCache), () => Map()))
        }
        else
        loader.wait(HRF.embedded.?(original.images.sources.keys.$).|(original.images.sources.values.$)) {
            val loaded = original.images.sources.$./((key, url) => key -> loader.get(HRF.embedded.?(key).|(url))).toMap

            onLoad(Resources(ImageResources(loaded, original.images.sources, HRF.imageCache), () => Map()))
        }
    }
}

// Who made the site, opened by About at the bottom of the main page and in every game's About menu (asked for on 2026-10-30)
object Credits {
    def options : $[ZOption] =
        $(ZOption("About this site", Div(
            "This site is a fan-run copy of HRF (Haunt Roll Fail), the board game engine JT created at hrf.im. " +
            "The engine, its interface and the original Root adaptation are JT's work, and none of this would exist without it. Thank you, JT.",
            ZBasic.info))) ++
        $(ZOption("About this site", Div(
            "Made with the help of AI: Claude Code, Anthropic's coding assistant, wrote most of the code added here " +
            "(Northgard: Uncharted Lands and the changes to Root), directed by geffrey. " +
            "The rules checks, playtesting and feedback come from geffrey and the players of this community, who have put a lot of work into it.",
            ZBasic.info))) ++
        $(ZOption("About this site", Div(
            "Root belongs to Leder Games, and Northgard and its board game Uncharted Lands to Shiro Games and the board game's publishers. This is an unofficial fan site.",
            ZBasic.info))) ++
        $(ZOption("HRF", Link("https://hrf.im", "hrf.im".spn.div, ZBasic.choice ++ $(xstyles.link)))) ++
        $(ZOption("HRF", Link("https://github.com/haunt-roll-fail/haunt-roll-fail", "Source Code".spn.div, ZBasic.choice ++ $(xstyles.link)))) ++
        $(ZOption("HRF", Link("https://ko-fi.com/hauntrollfail", "Support JT on Ko-Fi".styled(xstyles.error).div, ZBasic.choice ++ $(xstyles.link))))
}

// Rankings (Northgard, asked for on 2026-11-18; the server side is good-game/Ranking.scala). There are no accounts: a name typed on
// joining an online game is claimed with this browser's key, which only this browser (or one given the key as its "player code")
// can play under again. The rating is Elo, worked out here from every finished online game with people on two sides or more.
object PlayerKey {
    val key = "hrf.player-key"

    def get : String = {
        val k = try { Local.get(key, "") } catch { case _ : Throwable => "" }
        if (k.length >= 16) k
        else {
            val n = 0.until(24).map(_ => "abcdefghijklmnopqrstuvwxyz".charAt(scala.util.Random.nextInt(26))).mkString
            try { Local.set(key, n) } catch { case _ : Throwable => }
            n
        }
    }

    def set(k : String) = try { Local.set(key, k) } catch { case _ : Throwable => }

    // signing out: a new key, so this browser holds no name (Play Online's Sign out and Switch account; 2026-11-20)
    def fresh() = set(0.until(24).map(_ => "abcdefghijklmnopqrstuvwxyz".charAt(scala.util.Random.nextInt(26))).mkString)
}

object Elo {
    // a seat of a finished game: the player's name and, in games reported since 2026-11-18 (MetaGame.rankingSeat), what it played
    case class Entry(name : String, seat : |[RankingSeat])
    // a side: the seats on it and whether it won
    case class Side(entries : $[Entry], won : Boolean) {
        def names = entries./(_.name)
    }
    // fan: played with fan-made content (MetaGame.rankingFan), counted only in the stats
    case class Result(game : String, time : String, title : String, sides : $[Side], fan : Boolean = false) {
        // the tournament score of a winning side: the win shared between the winning sides (a coalition, ...)
        def share = 1.0 / sides.count(_.won)
        def score(s : Side) = if (s.won) share else 0.0
        // a seat "*" is no one: a game before rankings, counted only in the stats
        def rated = fan.not && sides.forall(_.names.has("*").not)
        def players = sides./~(_.entries)./~(_.seat)./(_.players).maxOption.|(sides./(_.entries.num).sum)
    }
    case class Change(result : Result, side : Side, entry : Entry, before : Double, after : Double, won : Boolean, opponents : $[String])
    case class Player(name : String, rating : Double, games : Int, wins : Int, last : String, history : $[Change]) {
        def provisional = games < Elo.provisional
        def peak = history./(_.after).max
        def lastChange = history.last.after - history.last.before
    }

    val start = 1000.0
    val K = 32.0
    val provisional = 5

    def unesc(s : String) = s.replace("\\t", "\t").replace("\\n", "\n").replace("\\\\", "\\")

    def entry(s : String) : Entry = s.split('/').$ match {
        case n :: f :: seat :: players :: rest if seat.toIntOption.any && players.toIntOption.any => Entry(n, |(RankingSeat(f.replace('_', ' '), seat.toInt, players.toInt, rest.headOption.filter(_.nonEmpty))))
        case n :: _ => Entry(n, None)
        case _ => Entry(s, None)
    }

    // the server's lines: game, time, title, "W:name,name;L:name", each name maybe followed by its seat (token), and ";X:fan" for a
    // game with fan content
    def parse(text : String) : $[Result] = text.split('\n').$.%(_.nonEmpty)./~{ line =>
        line.split('\t').$./(unesc) match {
            case game :: time :: title :: sides :: _ =>
                val ss = sides.split(';').$.%(_.length > 2).%(s => s.startsWith("W:") || s.startsWith("L:"))./(s => Side(s.drop(2).split(',').$.%(_.nonEmpty)./(entry).distinctBy(e => (e.name == "*").?(e.seat.toString).|(e.name.toLowerCase)), s.startsWith("W:")))
                val names = ss.flatMap(_.names).map(_.toLowerCase).but("*")
                (names.distinct.num == names.num).?(Result(game, time, title, ss, sides.split(';').$.has("X:fan")))
            case _ => None
        }
    }.sortBy(_.time)

    def compute(results : $[Result]) : $[Player] = {
        var rating = Map[String, Double]()
        var display = Map[String, String]()
        var history = Map[String, $[Change]]()

        results.%(_.rated).foreach { r =>
            r.sides.flatMap(_.names).foreach(n => display += n.toLowerCase -> n)

            def of(n : String) = rating.get(n.toLowerCase).|(start)
            def avg(l : $[String]) = l./(of).sum / l.num

            val n = r.sides.num
            var delta = Map[String, Double]()

            for (w <- r.sides.%(_.won); l <- r.sides.%(_.won.not)) {
                val e = 1 / (1 + math.pow(10, (avg(l.names) - avg(w.names)) / 400))
                val d = K / (n - 1) * (1 - e)
                w.names.foreach(x => delta += x.toLowerCase -> (delta.get(x.toLowerCase).|(0.0) + d))
                l.names.foreach(x => delta += x.toLowerCase -> (delta.get(x.toLowerCase).|(0.0) - d))
            }

            r.sides.foreach { s =>
                s.entries.foreach { e =>
                    val k = e.name.toLowerCase
                    val before = of(e.name)
                    val after = before + delta.get(k).|(0.0)
                    history += k -> (history.get(k).|($) :+ Change(r, s, e, before, after, s.won, r.sides.filter(_ != s).flatMap(_.names)))
                }
            }

            delta.foreach { case (k, d) => rating += k -> (rating.get(k).|(start) + d) }
        }

        history.keys.$./{ k =>
            val h = history(k)
            Player(display(k), rating.get(k).|(start), h.num, h.count(_.won), h.last.result.time, h)
        }.sortBy(p => (p.provisional, -p.rating))
    }

    // The faction and turn order stats, like the Root leagues': games, tournament score (a win, shared between the winning
    // sides), relative score (score per game) and the average game score
    case class Stat(key : String, games : Int, score : Double, scores : $[Int]) {
        def relative = score / games
        def average = scores.any.?(scores.sum.toDouble / scores.num)
    }

    def seats(results : $[Result]) : $[(Result, Side, RankingSeat)] = results./~(r => r.sides./~(s => s.entries./~(_.seat)./(e => (r, s, e))))

    def stats(l : $[(Result, Side, RankingSeat)], key : RankingSeat => String) : $[Stat] =
        l.groupBy(x => key(x._3)).toList.map { case (k, xx) => Stat(k, xx.num, xx./{ case (r, s, _) => r.score(s) }.sum, xx./~(_._3.score)./~(_.toIntOption)) }.sortBy(s => (-s.relative, -s.games))
}

class HRFUI(implicit resources : Resources) {
    StyleRegister.add($(xstyles.pane))
    StyleRegister.add($(xstyles.outer))
    StyleRegister.add($(xstyles.inner, xstyles.pane.log))
    StyleRegister.add($(xstyles.choice, xstyles.chm, xstyles.chp, xstyles.thu, xstyles.xx, xlo.pointer))
    StyleRegister.add($(xlo.fullwidth, xlo.pre))

    val uir = new ElementAttachmentPoint(getElem("root-attachment-point"))

    val guir = new ElementAttachmentPoint(getElem("game-attachment-point"))

    val logger = new NewLogger(new ElementAttachmentPoint(getElem("log")), true)

    dom.window.onerror = (_, _, _, _, error) => {
        logger.alog(error.toString.spn.div(xstyles.error))
    }

    object action {
        val hook = getElem("action")
        val pane = new ElementAttachmentPoint(hook.parentElement.parentElement)
        val asker = new Asker(new ElementAttachmentPoint(hook.parentElement).appendContainer(
            Div(
                Div(
                    Div(Content,
                    xstyles.middleScrollIn),
                xstyles.middleScrollOut),
            xstyles.unselectable, xstyles.inner, xstyles.pane.action
            ), resources).attach, Map())
        val scroll = hook.parentElement.children(1)
        hook.remove()
    }

    object overlay {
        val hook = getElem("overlay")
        val pane = new ElementAttachmentPoint(hook.parentElement.parentElement)
        val asker = new Asker(new ElementAttachmentPoint(hook.parentElement).appendContainer(
            Div(
                Div(
                    Div(Content,
                    xstyles.middleScrollIn),
                xstyles.middleScrollOut),
            xstyles.unselectable, xstyles.inner, xstyles.pane.action
            ), resources).attach, Map())
        val scroll = hook.parentElement.children(1)
        hook.remove()
    }

    logger.alog(Empty ~ "John 3:16")

    def topMenu() {
        hrf.elem.Translation.set("", identity, identity)

        def entry(m : MetaGame, label : Elem, onClick : () => Unit) = ZBasic("Kings and Chronicles".hh(xstyles.larger110)(ExternalStyle("consolas")), label, onClick, ZBasic.choice.but(xstyles.thumargin))

        action.asker.zask(HRF.menuMetas./~(m =>
            if (m == root.Meta)
                $(
                    entry(m, metaLabel(m), () => { rootLanguage(root.EnglishLanguage) ; rootMenu() }),
                    entry(m, (m.label + " en français").spn(xstyles.larger110)(ExternalStyle(m.titleFont.|(""))), () => { rootLanguage(root.FrenchLanguage) ; rootMenu() })
                )
            else
                $(entry(m, metaLabel(m), () => new HRFMetaUI(this, m, 0)(resources).withMeta()))
        ) :+
            ZBasic(" ", "About", () => creditsMenu())
        )
    }

    def creditsMenu() {
        action.asker.zask(Credits.options :+ ZBasic(" ", "Back", () => topMenu()))
    }

    // Root's language is one of its settings (shared by all the Root metas), so the French entry
    // only saves it and every Root game, online or local, is the same game in either language
    def rootLanguage(language : root.LanguageOption) {
        val key = root.Meta.settingsKey + ".settings"
        val saved = Local.get(key, "").split(' ').$.%(s => s != "" && s != root.EnglishLanguage.toString && s != root.FrenchLanguage.toString)
        Local.set(key, (saved :+ language.toString).mkString(" "))
        root.Meta.translation($(language)) @@ { case (language, text, image) => hrf.elem.Translation.set(language, text, image) }
    }

    def rootMenu() {
        action.asker.zask(HRF.rootMetas./(m => ZBasic("Root".hh(xstyles.larger110)(ExternalStyle(root.Meta.titleFont.|(""))), rootLabel(m), () => new HRFMetaUI(this, m, 0)(resources).withMeta(), ZBasic.choice.but(xstyles.thumargin))) :+
            ZBasic(" ", "Back", () => topMenu()))
    }

    def rootLabel(m : MetaGame) : Elem = (m match {
        case root.MetaAdset => "Advanced Setup"
        case root.MetaMirror => "Mirror"
        case _ => "Standard"
    }).spn(xstyles.larger110)(ExternalStyle(m.titleFont.|("")))

    def topInfo() {
        action.asker.zask(HRF.menuMetas./(m => ZBasic("Kings and Chronicles".hh(xstyles.larger110)(ExternalStyle("consolas")), metaLabel(m), null, ZBasic.info.but(xstyles.thumargin))))
    }

    def metaLabel(m : MetaGame) : Elem =
        if (m.underConstruction)
            m.label.spn(xstyles.larger110)(ExternalStyle(m.titleFont.|(""))) ~ Break ~ "Under Construction".styled(xstyles.warning)
        else
            m.label.spn(xstyles.larger110)(ExternalStyle(m.titleFont.|("")))

    HRF.param("meta")./~(mn => HRF.metas.%(_.name == mn).single)./{m =>
        new HRFMetaUI(this, m, 800)(resources).withMeta()
    }.|{
        topMenu()
    }

}

class HRFMetaUI(val ui : HRFUI, val meta : MetaGame, delayMainMenu : Int)(baseResources : Resources) {
    implicit val resources : Resources = meta.menuImages.any.?(baseResources.copy(images = baseResources.images.copy(sources = baseResources.images.sources ++ meta.menuImages.map { case (k, v) => k.toLowerCase -> v }))).|(baseResources)

    var settings : $[Setting] = $

    var history = new web.History("/play/" + meta.name, dom.window.location.search, dom.window.location.hash)

    if (HRF.offsite.any)
        history.disable()
    else
        history.init()

    def loadSettings() : $[Setting] = {
        val saved = Local.get(meta.settingsKey + ".settings", "").split(' ').$

        val preset = HRF.paramList("settings")

        OptionsState[Setting](meta.settingsList, $, preset.some.|(saved)./~(s => meta.settingsList.find(_.toString == s)), meta.settingsDefaults).checkDimmed().selected
    }

    def applySettings(settings : $[Setting]) {
        def setBodyStyle(style : String, italic : Boolean) {
            while (dom.document.body.classList.length > 0)
                dom.document.body.classList.remove(dom.document.body.classList.item(0))

            dom.document.body.classList.add(style)
            dom.document.styleSheets(0).as[dom.CSSStyleSheet].get.cssRules.iterator.$.of[dom.CSSStyleRule].filter(_.selectorText == ".italic").foreach(_.style.fontStyle = italic.??("italic"))
        }

        if (settings.has(GameFontFace))
            setBodyStyle(meta.bodyFont.|("luminari"), meta.bodyFontSupportsItalic)
        else
        if (settings.has(CodeFontFace))
            setBodyStyle("consolas", true)
        else
        if (settings.has(SystemFontFace))
            setBodyStyle("system-ui", true)

        val (margin, padding) = settings.of[ButtonSpacingSetting].lastOption @@ {
            case Some(CondensedButtonSpacing) => ("0.4ex", "0.2ex")
            case Some(NormalSpacing) | None => ("0.8ex", "0.4ex")
            case Some(ExpandedButtonSpacing) => ("1.2ex", "1.2ex")
        }

        HRF.speed = settings.of[ScrollSpeedSetting].lastOption @@ {
            case Some(SlowestScrollSpeed) => 1920
            case Some(SlowerScrollSpeed) => 1440
            case Some(SlowScrollSpeed) => 960
            case Some(NormalScrollSpeed) | None => 640
            case Some(FastScrollSpeed) => 480
            case Some(FasterScrollSpeed) => 320
            case Some(FastestScrollSpeed) => 240
        }

        getElem("font-size-adjust").style.fontSize = settings.of[FontSizeSetting].lastOption @@ {
            case Some(SmallerFontSize) => "84%"
            case Some(SmallFontSize) => "92%"
            case Some(NormalFontSize) | None => "100%"
            case Some(LargeFontSize) => "108%"
            case Some(LargerFontSize) => "120%"
        }

        dom.document.styleSheets(0).as[dom.CSSStyleSheet].get.cssRules.iterator.$.of[dom.CSSStyleRule].filter(_.selectorText == ".thumargin").foreach { r =>
            r.style = (
                "margin-top: " + margin + " !important;" +
                "margin-bottom: " + margin + " !important;" +
                "padding-top: " + padding + " !important;" +
                "padding-bottom: " + padding + " !important;").asInstanceOf[dom.CSSStyleDeclaration]
        }

        meta.translation(settings) @@ { case (language, text, image) =>
            val changed = language != hrf.elem.Translation.language
            hrf.elem.Translation.set(language, text, image)
            if (changed)
                hrf.html.retranslate()
        }

        dom.window.asInstanceOf[Dynamic].clicksToSwitchFullScreen = settings.of[FullScreenSetting].lastOption @@? {
            case Some(AlwaysFullScreen) => dom.document.documentElement.requestFullscreen(); 0
            case Some(TripleClickFullScreen) | None => 3
            case Some(NeverFullScreen) => try { if (dom.document.fullscreenElement != null) dom.document.exitFullscreen(); } catch { case e : Throwable => }; 999999
        }
    }

    def saveSettings(l : $[Setting]) {
        val ll = OptionsState[Setting](meta.settingsList, $, l, meta.settingsDefaults).checkDimmed().selected

        Local.set(meta.settingsKey + ".settings", ll./(_.toString).mkString(" "))
    }

    dom.window.onhashchange = {
        val old = dom.window.onhashchange

        e => {
            println("onhashchange")
            println("settings before "+ settings)

        old(e) ; settings = loadSettings() ; applySettings(settings)

            println("settings after "+ settings)
        }
    }

    def editSettings(then : => Unit) {
        ui.overlay.pane.vis()

        var ostate = OptionsState[Setting](meta.settingsList, $, settings, meta.settingsDefaults).checkDimmed()

        def ask() {
            def pick(o : Setting) {
                if (ostate.enabled(o)) {
                    ostate = ostate.click(o)
                    settings = ostate.selected
                    saveSettings(settings)
                    applySettings(settings)
                    ask()
                }
            }

            // Consecutive compact settings of the same group share one row
            val rows = meta.settingsList.filter(_.visible(ostate.selected)).foldLeft($[$[Setting]]()) { (rows, o) =>
                (o, rows.lastOption./(_.last)) match {
                    case (c : CompactSetting, Some(p : CompactSetting)) if p.group == c.group => rows.dropRight(1) :+ (rows.last :+ o)
                    case _ => rows :+ $(o)
                }
            }

            // A foldable section (Folds) under its own group's heading shows only what is picked until the heading is tapped;
            // one made of several groups gets a heading of its own, with a summary of what is picked while folded
            def fold(s : String) = (_ : Any) => { Folds.toggle(s) ; ask() }

            ui.overlay.asker.zask(
                rows.zipWithIndex./~ { case (l, i) =>
                    val f = l.head.fold
                    val open = f.forall(Folds.isOpen)
                    val headed = f.exists(_ == l.head.group.text)
                    val group = f.%(_ => headed)./(s => Folds.heading(l.head.group, open)).|(l.head.group)
                    val first = f.any && headed.not && (i == 0 || rows(i - 1).head.fold != f)
                    val section = rows.drop(i).takeWhile(_.head.fold == f).flatten

                    first.$(ZOption(Folds.heading(f.get.txt, open), open.?(Empty).|(OnClick(Div(section.%(ostate.selected.has)./(_.foldSummary).reduceOption((a, b) => a ~ " · " ~ b).|(Empty), ZBasic.choice ++ $(xlo.fullwidth)))), fold(f.get), clear = false, groupClick = fold(f.get))) ++
                    (open || headed).??(l match {
                        case (h : CompactSetting) :: _ =>
                            val items = l./(o => OnClick(o, Div(ostate.selected.has(o).?(o.valueOn).|(o.valueOff), $(xstyles.choice, xstyles.xx, xstyles.chp, xstyles.compactChoice, xlo.pointer))))
                            $(ZOption(group, Div(items.merge, $(xlo.flexhcenter, xlo.fullwidth)), p => pick(p.asInstanceOf[Setting]), clear = false, groupClick = f./(fold).orNull))
                        case l => l.%(o => open || ostate.selected.has(o))./({ o =>
                            val state = ostate.selected.has(o)
                            val active = ostate.enabled(o)
                            ZBasic(group, state.?(o.valueOn).|(o.valueOff), (active || state).??(() => pick(o)), ZBasic.choice ++ $(xlo.fullwidth)).copy(clear = false, groupClick = f./(fold).orNull)
                        })
                    })
                } ++
                $(ZBasic("", "Reset All Settings", () => {
                    ostate = OptionsState[Setting](meta.settingsList, $, $, meta.settingsDefaults).checkDimmed()
                    settings = ostate.selected
                    saveSettings(settings)
                    applySettings(settings)
                    ask()
                }, ZBasic.choice ++ $(xlo.fullwidth))) ++
                $(ZBasic(" ", "Done".hh, () => {
                    ui.overlay.pane.invis()
                    then
                }, ZBasic.choice ++ $(xlo.fullwidth)))
            )
        }

        ask()

        val background = dom.document.getElementById("blurred-background").asInstanceOf[dom.html.Div]

        background.onclick = (e) => {
            background.onclick = null
            ui.overlay.pane.invis()
            then
        }
    }

    def withMeta() {
        ui.logger.alog(meta.label.spn.div)

        if (meta.name == "cthw" && false.not) {
            dom.document.location.assign("https://cwo.im/")
            return
        }

        settings = loadSettings()
        saveSettings(settings)
        applySettings(settings.notOf[FontSizeSetting])

        // ui.topInfo()

        dom.document.title = meta.label

        if (HRF.lobby.any || HRF.replay) {
            val (user, lj) = if (HRF.replay) {
                ("", new ReplayPhantomJournal[String](meta, getElem("lobby").textContent, identity))
            }
            else {
                val server = HRF.server.get
                val lobby = HRF.lobby.get
                val user = HRF.user.get
                val secret = HRF.secret.get

                (user, new ServerJournal[String](meta, server, user, secret, lobby, identity, identity))
            }

            var started = false

            var enteredNames = Map[String, String]()
            var preNames = Map[String, String]()
            var users = Map[meta.F, String]()
            var switches = $[String]()
            var bots = Map[meta.F, String]()
            var seating = $[meta.F]()
            var options = $[meta.O]()
            var server : |[String] = None
            var title : |[String] = None
            var version : |[String] = None

            var position = 0
            var cycle = false

            def reread() { lj.read(position)(readLobby) }

            def readLobby(lines : $[String]) {
                position += lines.num

                lines.foreach { line =>
                    val l = line.split(' ').$
                    l(0) match {
                        case "meta" =>
                            val m = l(1).sanitize(32)
                            if (m != meta.name) {
                                val l = dom.document.location
                                val url = l.origin + "/play/" + m + "/" + l.pathname.split('/').drop(3).join("/") + l.search + l.hash
                                ui.logger.alog(" Reload " + url)
                                dom.document.location.assign(url)
                                throw new Error("incorrect meta")
                            }
                        case "version" =>
                            version = Some(l(1))./(v => if (v == "2") "0.8.73" else v)
                        case "user" =>
                            users += meta.parseFaction(l(1)).get -> l(2)
                        case "bot" =>
                            bots += meta.parseFaction(l(1)).get -> l(2)
                        case "seating" =>
                            seating = l.drop(1)./(f => meta.parseFaction(f).get)
                        case "options" =>
                            options = l.drop(1)./~(o => meta.parseOption(o))
                        case "server" =>
                            server = Some(l(1))
                        case "title" =>
                            title = Some(l.drop(1).join(" ").sanitize(32))
                        case "name" =>
                            enteredNames += l(1) -> (l.drop(2).join(" ").sanitize(32))
                        case "prename" =>
                            preNames += l(1) -> (l.drop(2).join(" ").sanitize(32))
                        case "enable" =>
                            switches :+= l.drop(1).join(" ")
                        case "disable" =>
                            switches --= l.drop(1).join(" ")
                        case _ =>
                    }
                }

                if (options.none)
                    options = meta.defaultsFor(seating.num, seating)

                val self = users.keys.%(f => users(f) == user).$

                self.single.foreach { f =>
                    val assetsources = meta.assets.%(_.condition(seating, options))./~(_.get)./(a => a.name -> (meta.path + "/images/" + a.src)).toMap

                    meta.glyph(f).foreach { s =>
                        assetsources.get(s.toLowerCase).foreach { src =>
                            HRF.glyph(src)
                        }
                    }
                }

                if (version.any && version.has(HRF.version).not && HRF.flag("override").not) {
                    val q = "This game was created on " ~ "HRF".hl ~ " version " ~ version.get.hl ~ (version.get == "0.8.73").??(" or earlier")

                    ui.action.asker.zask(
                        ZBasic(q, ("Load version ".hh ~ version.get.hlb), () => {
                            val l = dom.document.location
                            val search = ("version=" + version.get) +: l.search.drop(1).split('&').$.%(_.startsWith("version").not).but("")
                            val url = l.origin + l.pathname + "?" + search.join("&") + l.hash
                            ui.logger.alog(" Reload " + url)
                            dom.document.location.assign(url)
                        }) ::
                        ZBasic(q, "Try loading version " ~ HRF.version.hh, () => {
                            version = Some(HRF.version)
                            setTimeout(0) { reread() }
                            ()
                        })
                    )
                }
                else
                if (self.any && enteredNames.contains(user).not) {
                    def randomName() {
                        val adjective = $(
                            "Ardent", "Adventurous", "Agile", "Alert", // "Ambitious",
                            "Brave", "Bold",
                            "Clever", "Cunning", "Caring", "Calm", "Curious", "Cheerful", "Compassionate", // "Confident", "Considerate", "Creative",
                            "Daring", "Diligent", "Determined", // "Dedicated",
                            "Eager", "Eloquent", "Enthusiastic",
                            "Fair", "Fearless", // "Frank",
                            "Good", "Generous", "Gentle", "Gracious", "Grateful",
                            "Honorable", "Honest", "Humble",
                            "Ingenious",
                            "Jolly", "Just",
                            "Kind",
                            "Loyal",
                            "Merciful", "Modest",
                            "Noble", "Nice",
                            "Observant",
                            "Proud", "Patient", "Prudent", "Principled", "Passionate", // "Persistent",
                            "Quick",
                            "Righteous", "Resolute", "Resilient",
                            "Selfless", "Sincere",
                            "Tough", "Tenacious",
                            "Unyielding",
                            "Vigorous", "Valiant",
                            "Wholesome", "Wise",
                            "Xenial",
                            "Yearning",
                            "Zealous")
                        val animal = $(
                            "Armadillo", "Albatross", "Ant", "Ape", "Alligator", "Antelope",
                            "Badger", "Barracuda", "Bear", "Beaver", "Bat", "Bee", "Boar", "Butterfly", "Baboon",
                            "Camel", "Cat", "Cow", "Cheetah", "Crab", "Crane", "Crocodile", "Crow",
                            "Dog", "Deer", "Duck", "Dolphin", "Donkey", "Dragonfly",
                            "Eagle", "Elephant", "Elk", "Eel", "Emu",
                            "Fox", "Fish", "Ferret", "Falcon", "Flamingo", "Frog",
                            "Giraffe", "Gazelle", "Gecko", "Goat", "Goose", "Gopher",
                            "Hare", "Horse", "Hamster", "Hawk", "Hedgehog", "Hyena",
                            "Iguana",
                            "Jellyfish", "Jaguar",
                            "Kangaroo", "Koala",
                            "Lion", "Llama", "Lemur", "Leopard", "Lizard", "Lark",
                            "Monkey", "Mouse", "Mongoose", "Moose",
                            "Newt", "Narwhal", "Nightingale",
                            "Otter", "Octopus", "Opossum", "Ostrich", "Owl",
                            "Panda", "Pig", "Panther", "Parrot", "Pelican", "Penguin", "Porcupine",
                            "Quail",
                            "Raccoon", "Rat", "Raven", "Rhinoceros", "Rook",
                            "Sheep", "Shark", "Sloth", "Snake", "Scorpion", "Skunk", "Squirrel", "Salamander", "Seal", "Snail", "Swan", "Seagull", "Squid",
                            "Turtle", "Tiger", "Tapir", "Toad",
                            "Urchin",
                            "Vulture",
                            "Wolf", "Walrus", "Whale", "Weasel",
                            "Zebra")
                        askName(adjective.shuffle.head + " " + animal.shuffle.head)
                    }

                    def postName(value : String, retries : Int) {
                        val name = value.split(' ').filter(_ != "").join(" ").sanitize(32)
                        if (name == value && retries < 3) {
                            setCookie("name", name, Some(3650))
                            lj.append($("name", user, name).join(" "))(reread())(postName(name, retries + 1))
                        }
                        else
                            askName(name)
                    }

                    // Ranked games (Meta.ranking) claim the name with this browser's key first (Rankings); a name another
                    // player holds is refused. When the server can't be reached the name is used unclaimed.
                    def claimName(value : String) {
                        val name = value.split(' ').filter(_ != "").join(" ").sanitize(32)
                        if (meta.ranking.none || HRF.server.none || name != value)
                            postName(value, 0)
                        else
                            postF(HRF.server.get + "/claim-name/" + user + "/" + HRF.secret.get, name + "\n" + PlayerKey.get) { r =>
                                if (r.startsWith("ok"))
                                    postName(value, 0)
                                else
                                if (r.startsWith("taken"))
                                    askName(name, |("Taken by another player: pick another name, or sign in if " + name + " is you"), |(name))
                                else
                                    askName(name, |("Pick another name."))
                            } {
                                postName(value, 0)
                            }
                    }

                    // taken: the name is held by another device, which may be the player's own: they sign in to it here (2026-11-20)
                    def askName(name : String, problem : |[String] = None, taken : |[String] = None) {
                        ui.action.asker.iask(InputOption(problem.|(|("Name").|("What's your name, punk?")), name, Nil, v => claimName(v)) ::
                            taken./(t => BasicOption(" ", "It's me: sign in as " + t, Nil, () => signInAs(|(t), n => claimName(n), () => askName(name, problem, taken)))).$ ++
                            $(BasicOption(" ", "Generate Random", Nil, () => randomName())))
                    }

                    askName(getCookie("name").|(""))
                }
                else
                if (started.not) {
                    started = true

                    dom.document.title = (self./(meta.factionName) :+ title.|("%untitled%") :+ meta.label).join(" - ")

                    var difficulties = seating./(_ -> HRF.param("debug")./(bot => BotDebug(bot)).|(Human)).toMap ++ bots./{ case (f, d) => f -> HRF.flag("debug-bot").?(BotDebug(d)).|(Bot(d)) } ++ seating.%(meta.botOnly)./(f => f -> Bot(meta.defaultBot(f)))
                    val state = OptionsState(meta.options, meta.mandatoryFor(seating.num, seating), options, $)

                    def names : Map[meta.F, String] = users.flatMap {
                        case (f, i) => preNames.get(i)./(f -> _)
                    } ++ users.flatMap {
                        case (f, i) => enteredNames.get(i)./(f -> _)
                    }

                    object ServerSwitches extends Switches {
                        def canSwitches = true
                        def getSwitches = switches
                        def enableSwitch(s : String) {
                            switches :+= s
                            lj.append($("enable", s).join(" ")){reread()}{reread()}
                        }
                        def disableSwitch(s : String) {
                            switches --= s
                            lj.append($("disable", s).join(" ")){reread()}{reread()}
                        }
                    }

                    val journal =
                        if (HRF.replay)
                            new ReplayPhantomJournal[meta.gaming.ExternalAction](meta, getElem("replay").textContent, s => meta.parseActionExternal(s), HRF.paramInt("at") | 999999)
                        else
                        if (HRF.flag("phantom"))
                            new ServerPhantomJournal[meta.gaming.ExternalAction](meta, HRF.server.get, HRF.user.get, HRF.secret.get, server.get, s => meta.parseActionExternal(s), s => meta.writeActionExternal(s), HRF.paramInt("at") | 999999)
                        else
                            new ServerJournal[meta.gaming.ExternalAction](meta, HRF.server.get, HRF.user.get, HRF.secret.get, server.get, s => meta.parseActionExternal(s), s => meta.writeActionExternal(s), HRF.paramInt("at") | 999999)

                    HRF.spectator = self.none && HRF.replay.not

                    // A seat named before rankings (or offline then) claims its name now, if no one holds it
                    if (meta.ranking.any && HRF.replay.not)
                        self.single.foreach { f =>
                            enteredNames.get(user).foreach { n =>
                                postF(HRF.server.get + "/claim-name/" + user + "/" + HRF.secret.get, n + "\n" + PlayerKey.get)(_ => ())(())
                            }
                        }

                    // Rankings: the first client to reach the end reports the sides, by seat, people only; the server keeps one report
                    var reported = false

                    // the rating changes the game made, as a line at the end of its log (asked for on 2026-11-18)
                    def showRatings(note : Elem => Unit) {
                        val metas = HRF.metas.%(m => m.ranking.any && m.ranking == meta.ranking)./(_.name).distinct.join(",")
                        getF(HRF.server.get + "/rankings/" + metas) { text =>
                            val changes = Elo.compute(Elo.parse(text))./~(_.history).%(_.result.game == server.get)
                            def signed(d : Double) = { val r = d.round ; (r >= 0).?("+" + r).|(r.toString) }
                            if (changes.any)
                                note("Rankings: ".hl ~ changes.sortBy(c => -c.after)./(c => c.entry.name.hlb ~ " " ~ c.after.round.toString.hl ~ " (" ~ signed(c.after - c.before) ~ ")").join(", "))
                        }(())
                    }

                    def reportResult(g : meta.G, winners : $[Any], note : Elem => Unit) {
                        if (reported.not && HRF.replay.not && meta.ranking.any && server.any) {
                            reported = true
                            val sides = meta.rankingSides(g, winners)./{ case (l, won) => (l./~(f => users.get(f)./(_ + meta.rankingSeat(g, f)./(_.token).|(""))), won) }.filter(_._1.any)
                            if (sides.num >= 2 && sides.exists(_._2) && sides.exists(_._2.not))
                                post(HRF.server.get + "/report-result/" + user + "/" + HRF.secret.get + "/" + server.get, (meta.name +: title.|("%untitled%") +: (sides./{ case (l, won) => (won.?("W").|("L") +: l).join(" ") } ++ meta.rankingFan(g).$("X fan"))).join("\n"))(_ => showRatings(note))
                        }
                    }

                    startGame(seating, difficulties, state.selected, self, journal, title.|("%untitled%"), () => names, ServerSwitches, reportResult)

                    reread()

                    if (HRF.replay.not) {
                        val lobby = HRF.lobby.get
                        val user = HRF.user.get

                        val link = HRF.offsite.any.?(HRF.param("play").get).||(dom.document.location.pathname.split('/').$.drop(3).single).|("unknown/link")
                        val faction = users.keys.$.%(k => users(k) == user).single
                        val name = preNames.get(user).|("")

                        val prefix = meta.settingsKey + ".online.game." + lobby + "."

                        val rg = OnlineGame(HRF.now().getTime(), version.|("unknown.version"), meta.name, lobby, title.|("Unknown Title"), options.of[ImportantOption]./(meta.writeOption), faction.$./(f => PlayerLink(meta.writeFaction(f), link, name)), faction.none.??(link), $)

                        val time = Local.list(prefix)./(_.drop(prefix.length).toDouble).maxOr(0)

                        val og = (time > 0).?(HRF.parseOnlineGame(lobby, time, Local.get(prefix + time, ""))).|(rg)

                        val ng = og.copy(links = (og.links ++ rg.links).distinctBy(_.key), spectate = og.spectate.some || rg.spectate.some | "")

                        Local.set(prefix + ng.time, HRF.writeOnlineGame(ng))
                    }
                }
                else {
                    if (cycle.not) {
                        cycle = true
                        setTimeout(15000) {
                            cycle = false
                            reread()
                        }
                    }
                }
            }

            reread()
        }
        else
        if (HRF.flag("quick")) {
            def generateFactions() : $[meta.F] = {
                var i = 0

                while (i < 1000) {
                    var l = HRF.paramList("factions")./(meta.parseFaction(_).get).some.|({
                        val included = HRF.paramList("include")./(meta.parseFaction(_).get)
                        val excluded = HRF.paramList("exclude")./(meta.parseFaction(_).get)
                        val total = meta.quickMin.to(meta.quickMax).$.shuffle.head
                        included ++ meta.factions.diff(excluded).diff(included).shuffle.take(total).drop(included.num)
                    })

                    if (meta.validateFactionCombination(l).is[ErrorResult].not)
                        return l

                    i += 1
                }

                throw new Error("could not generate a valid quick faction combination")
            }

            val factions = generateFactions()

            var difficulties = factions./(f => f -> Bot(meta.defaultBot(f))).toMap[meta.F, Difficulty]

            var options = OptionsState(meta.optionsFor(factions.num, factions), meta.mandatoryFor(factions.num, factions), $, meta.defaultsFor(factions.num, factions)).checkDimmed()

            HRF.paramList("human")./(meta.parseFaction(_).get).some.|($(factions.shuffle.head)).foreach { h =>
                difficulties += h -> HRF.flag("debug").?(BotDebug(meta.defaultBot(h))).|(Human)
            }

            HRF.paramList("options")./~(meta.parseOption).use { o =>
                options = OptionsState(meta.optionsFor(factions.num, factions), meta.mandatoryFor(factions.num, factions), o, meta.defaultsFor(factions.num, factions)).checkDimmed()
            }

            val journal = new MemoryJournal[meta.gaming.ExternalAction](meta)

            startGame(factions, difficulties, meta.startOptions(options.actual), $, journal, meta.randomGameName(), () => Map(), NoSwitches)
        }
        else
            if (hrf.HRF.flag("fastsetup"))
                startSetup(meta.indistinguishableFactions.?(meta.factions.take(4)).|(meta.factions.take(10).combinations(4).$.shuffle(0)), false)
            else {
                history.pushState("/play/" + meta.name, () => {})

                if (delayMainMenu > 0)
                    setTimeout(delayMainMenu) {
                        metaMenu()
                    }
                else {
                    metaMenu()
                }
            }
    }

    def metaMenu() {
        val title = meta.label.hl.spn(xstyles.larger110)(ExternalStyle(meta.titleFont.|("")))

        def goQuickGame() {
            history.pushState("/play/" + meta.name + "/quick", () => metaMenu())
            savedGamesMenu("quick", "New Quick Game", () => quickGame())
        }

        def goHotseat() {
            history.pushState("/play/" + meta.name + "/hotseat", () => metaMenu())
            savedGamesMenu("hotseat", "New Local Game", () => customGame(false))
        }

        def goSolo() {
            history.pushState("/play/" + meta.name + "/solo", () => metaMenu())
            savedGamesMenu("solo", "New Solo Game", () => soloGame())
        }

        def goOnline() {
            history.pushState("/play/" + meta.name + "/online", () => metaMenu())
            onlineGame()
        }

        def goTutorial() {
            history.pushState("/play/" + meta.name + "/tutorial", () => metaMenu())
            tutorialMenu()
        }

        def goMode(mode : String) {
            history.pushState("/play/" + meta.name + "/" + mode, () => metaMenu())
            modeMenu(mode)
        }

        def goRankings() {
            history.pushState("/play/" + meta.name + "/rankings", () => metaMenu())
            rankingsMenu()
        }

        def goTournament() {
            history.pushState("/play/" + meta.name + "/tournament", () => metaMenu())
            tournamentMenu()
        }

        ui.action.asker.zask(
            meta.underConstruction.$(ZOption(title, Div("This game is very much " ~ "under construction".styled(xstyles.warning) ~ "." ~ Break ~ "Expect missing rules, bugs and placeholder art.", ZBasic.info))) ++
            meta.menuAbout./(e => ZOption(title, Div(e, ZBasic.info))).$ ++
            (
                meta.tutorialLessons.any.$(ZBasic(title, "Tutorial".hlb, () => goTutorial())) ++
                $(ZBasic(title, "Quick Game".hlb, meta.factions.%(f => meta.getBots(f).has(meta.defaultBot(f))).any.??(() => goQuickGame()))) ++
                $(ZBasic(title, "Local Game".hhb, () => goHotseat())) ++
                meta.soloFaction./(s => ZBasic(title, ("Solo vs " + meta.factionName(s).split(' ').head).hhb, () => goSolo())).$ ++
                meta.modes./{ case (label, mode) => ZBasic(title, label.hhb, () => goMode(mode)) } ++
                meta.linkedModes./{ case (label, l) => ZBasic(title, label.hhb, () => {
                    HRF.metas.%(_.name == l).single./{ m =>
                        new HRFMetaUI(ui, m, 800)(baseResources).withMeta()
                    }.|(throw new Error("meta not found " + l))
                }) } ++
                $(ZBasic(title, "Play Online".hlb, (HRF.server.any && HRF.offline.not).??(() => goOnline()))) ++
                (meta.ranking.any && HRF.server.any && HRF.offline.not).$(ZBasic(title, "Rankings".hhb, () => goRankings())) ++
                // the site's tournament, on the menu of the game whose tournament it is (Root's, played in Advanced Setup), and on
                // the menu of the meta its games use (Root's Adset, 2026-11-20), where it opens that game's Tournament page
                (meta.tournament.exists(_ != meta.name) && HRF.server.any && HRF.offline.not).$(ZBasic(title, "Tournament".hhb, () => goTournament())) ++
                (meta.tournament.has(meta.name) && HRF.server.any && HRF.offline.not).??(
                    HRF.metas.%(m => m.name != meta.name && m.tournament.has(meta.name)).take(1)./(m => ZBasic(title, "Tournament".hhb, () => dom.document.location.assign("/play/" + m.name + "/tournament"))))
            ) ++
            meta.intLinks./((t, l) => ZBasic("Other", t, () => {
                HRF.metas.%(_.name == l).single./{ m =>
                    new HRFMetaUI(ui, m, 800)(baseResources).withMeta()
                }.|(throw new Error("meta not found " + l))
            })) ++
            meta.extLinks./((t, l) => ZOption(Div(meta.extLinksName), Link(l, t.div(xstyles.divint) ~ Image("external-link")(xstyles.explain)(xstyles.clickThrough), ZBasic.infoch ++ $(xstyles.link)))) ++
            // $(ZOption(Div(Empty, xlo.grow6), Empty)) ++
            $(ZBasic(" ", Div("Settings"), () => miscellaneous())) ++
            meta.showAbout.$(ZBasic(" ", Div("About"), () => aboutMenu())) ++
            // Back to the site's main screen (/play), or to the game whose menu links here (Northgard for its Adset);
            // loaded afresh, so nothing of this game's menus is left over
            HRF.offline.not.$(ZBasic(" ", Div("Back"), () => dom.document.location.assign("/play" + HRF.metas.%(_.linkedModes.exists(_._2 == meta.name)).single./("/" + _.name).|(""))))
        )

        if (HRF.segments.startsWith($("tutorial")) && meta.tutorialLessons.any)
            goTutorial()
        else
        // (the segments used up, so that their pages' Back, which shows this menu again, doesn't open them again: a page opened
        // from its link, as the Tournament's from Discord, couldn't be left, 2026-11-20)
        if (HRF.segments.startsWith($("rankings")) && meta.ranking.any && HRF.server.any && HRF.offline.not) {
            HRF.segments = $
            goRankings()
        }
        else
        if (HRF.segments.startsWith($("tournament")) && meta.tournament.exists(_ != meta.name) && HRF.server.any && HRF.offline.not) {
            HRF.segments = $
            goTournament()
        }
        else
        if (HRF.segments.startsWith($("quick")))
            goQuickGame()
        else
        if (HRF.segments.startsWith($("hotseat")))
            goHotseat()
        else
        if (HRF.segments.startsWith($("solo")) && meta.soloFaction.any)
            goSolo()
        else
        if (HRF.segments.startsWith($("online")))
            goOnline()
        else
        if (meta.modes.exists(m => HRF.segments.startsWith($(m._2))))
            goMode(HRF.segments.head)
        else
            HRF.segments = $
    }

    // The main menu's Tutorial (Northgard's): its lessons, each on its own page with the next one a tap away, and a practice game
    // against bots with the meta's options, saved in the browser like the other local games
    def tutorialMenu() {
        val label = "Tutorial".hlb
        val lessons = meta.tutorialLessons

        def goLesson(i : Int) {
            history.pushState("/play/" + meta.name + "/tutorial/lesson", () => tutorialMenu())
            lesson(i)
        }

        def lesson(i : Int) {
            val (t, body) = lessons(i)

            ui.action.asker.zask(
                $(ZOption(("Lesson " + (i + 1) + " of " + lessons.num).txt.div ~ t.hlb.div, Div(body./(e => Div(e)).merge, ZBasic.info))) ++
                (i + 1 < lessons.num).$(ZBasic(" ", "Next: ".txt ~ lessons(i + 1)._1.hh, () => lesson(i + 1))) ++
                (i + 1 == lessons.num).??(meta.tutorialGame.$./(_ => ZBasic(" ", "Start the Practice Game".hlb, () => {
                    history.popState()
                    goPractice()
                }))) ++
                (i > 0).$(ZBasic(" ", "Previous: ".txt ~ lessons(i - 1)._1, () => lesson(i - 1))) ++
                $(ZBasic(" ", "All Lessons", () => history.popState()))
            )

            ui.action.scroll.scrollTop = 0
        }

        def practice() {
            HRF.segments = $

            meta.tutorialGame.foreach { case (seating, bots, options) =>
                val difficulties = seating./(f => f -> bots.get(f)./(b => Bot(b) : Difficulty).|(Human)).toMap

                startGame(seating, difficulties, options, $, newLocalGame("tutorial", seating, difficulties, options, "Tutorial"), "Tutorial", () => Map(), NoSwitches)
            }
        }

        def goPractice() {
            history.pushState("/play/" + meta.name + "/tutorial/practice", () => tutorialMenu())
            savedGamesMenu("tutorial", "New Practice Game", () => practice())
        }

        ui.action.asker.zask(
            $(ZOption(label, Div(meta.tutorialAbout, ZBasic.info))) ++
            lessons.zipWithIndex./{ case ((t, _), i) => ZBasic("Lessons", ((i + 1) + ". ").txt ~ t, () => goLesson(i)) } ++
            meta.tutorialGame.$./(_ => ZBasic("Practice", "Practice Game".hlb, () => goPractice())) ++
            ZBasic(" ", "Back", () => {
                history.popState()
            }).?
        )

        // savedGamesMenu's own address for a new game is .../tutorial/new
        if (HRF.segments.startsWith($("tutorial", "new")) && meta.tutorialGame.any)
            goPractice()
        else
        if (HRF.segments.startsWith($("tutorial", "practice")) && meta.tutorialGame.any) {
            HRF.segments = $
            goPractice()
        }
        else
            HRF.segments = $
    }

    // A mode from the main menu (Northgard's Training Grounds): a local or an online game, set up with the mode's factions and options
    def modeMenu(mode : String) {
        val label = meta.modes.find(_._2 == mode)./(_._1).|(mode).hlb

        def goLocal() {
            history.pushState("/play/" + meta.name + "/" + mode + "/local", () => modeMenu(mode))
            HRF.segments = $
            startSetup(meta.modeFactions(mode), false, mode = |(mode))
        }

        def goOnline() {
            history.pushState("/play/" + meta.name + "/" + mode + "/online", () => modeMenu(mode))
            HRF.segments = $
            startSetup(meta.modeFactions(mode), true, mode = |(mode))
        }

        ui.action.asker.zask(
            $(ZOption(label, Div(meta.modeAbout(mode), ZBasic.info))) ++
            $(ZBasic(label, "Local Game".hhb, () => goLocal())) ++
            $(ZBasic(label, "Play Online".hlb, (HRF.server.any && HRF.offline.not).??(() => goOnline()))) ++
            ZBasic(" ", "Back", () => {
                history.popState()
            }).?
        )

        if (HRF.segments.startsWith($(mode, "local")))
            goLocal()
        else
        if (HRF.segments.startsWith($(mode, "online")) && HRF.server.any && HRF.offline.not)
            goOnline()
        else
            HRF.segments = $
    }

    def miscellaneous(download : Boolean = false) {
        val versions = $("0.8.125")

        ui.action.asker.zask(
            versions./(v => ZBasic("Previous Stable Versions", "HRF " + v, () => {
                val l = dom.document.location
                val search = ("version=" + v) +: l.search.drop(1).split('&').$.%(_.startsWith("version").not).but("")
                val url = l.origin + l.pathname + "?" + search.join("&") + l.hash
                ui.logger.alog(("Reload " + url).spn.div, _ => {})
                dom.document.location.assign(url)
            }, ZBasic.choice ++ $(xlo.fullwidth) ++ $(xstyles.link))) ++
            $(ZBasic("UI", "Interface", () => {
                editSettings {
                    miscellaneous()
                }
            }, ZBasic.choice ++ $(xlo.fullwidth))) ++
            $(ZBasic("Offline Version", download.not.?("Download".styled(xstyles.warning)).|("Preparing Download".hh ~ " (may take a few minutes)"), download.not.??(() => {
                val assets = meta.assets./~(_.get)

                val sources = assets./(a => a.name -> ("webp2/" + meta.path + "/images/" + a.copy(ext = "webp").src)).toMap
                val preload = HRF.embedded.?(assets./(a => a.name -> a.name)).|(assets.%(_.lzy == Laziness.Immediate)./(a => a.name -> sources(a.name)))
                val loader = HRF.embedded.?(new WrappedEmbeddedImageLoader(s => "asset-" + s)).|(HRF.imageCache)

                ui.logger.alog("Loading assets".spn.div)

                var i = 0

                loader.onLoad = (url, result) => {
                    i += 1

                    if (i % (preload.num / 23 + 1) == 0)
                        ui.logger.alog("*".spn)
                }

                loader.wait(preload.rights) {
                    val loaded = preload./((key, url) => key -> loader.get(url)).toMap

                    val combined = Resources(ImageResources(loaded ++ resources.images.loaded, sources ++ resources.images.sources, HRF.imageCache), () => Map())

                    val filename = "hrf--" + meta.name + "--" + HRF.version + "--offline"

                    Quine.save(meta)("HRF", $, $, combined, new MemoryJournal[meta.gaming.ExternalAction](meta), filename, false, HRF.server.|(""), miscellaneous())
                }

                miscellaneous(true)
            }), ZBasic.choice ++ $(xlo.fullwidth))) ++
            $(ZBasic("Local Storage", "Clear All Data".styled(xstyles.error), () => {
                hrf.web.Local.clear()
                settings = $
                applySettings(settings)
                miscellaneous()
            }, ZBasic.choice ++ $(xlo.fullwidth))) ++
            $(ZBasic(" ", "Done".hh, () => {
                metaMenu()
            }, ZBasic.choice ++ $(xlo.fullwidth)))
        )
    }

    // Rankings (Elo, PlayerKey): every rated player, tapping one shows their games; the names this browser holds and its player code.
    // The look (2026-11-18) is the rk- classes in index.html: the faction glyphs and colours, rating badges, bars and the ladder rows.
    // the server's answers to a Discord username (DiscordBot in good-game; 2026-11-20)
    def discordNotFound = "No one on the Discord server has that username. Check the spelling (it's the name under your display name in your Discord profile), and join the server first if you haven't."

    def discordNoBot = "Usernames can't be looked up yet, so give your Discord user ID for now. " + "In Discord, open Settings, then Advanced (on a phone, it's under App Settings), and turn on Developer Mode. Then tap or right-click your own name and pick Copy User ID: a long number like 123456789012345678. Your username can't ping you."

    def rankingsMenu() {
        val server = HRF.server.get
        val title = (meta.label + " Rankings").hl.spn(xstyles.larger110)
        val metas = HRF.metas.%(m => m.ranking.any && m.ranking == meta.ranking)./(_.name).distinct.join(",")

        // the faction glyphs, before the game's assets load (the same paths as the game's)
        // the metas on this ladder: Advanced Setup's factions are seats, so a seat's faction is looked up in all of them (Root's)
        val ladderMetas = (meta +: HRF.metas.%(m => m.ranking.any && m.ranking == meta.ranking)).distinct
        // (each meta names its own: Root's "<faction>-glyph", Northgard's "clan-<clan>")
        val glyphSources = ladderMetas./~{ m =>
            val names = (m.factions./~(f => m.glyph(f)) ++ m.rankingTiers./~(_.icon))./(_.toLowerCase).toSet
            m.assets./~(_.get).%(a => a.name.endsWith("-glyph") || names.contains(a.name.toLowerCase))./(a => a.name.toLowerCase -> ("webp2/" + m.path + "/images/" + a.copy(ext = "webp").src))
        }.toMap

        implicit val resources : Resources = this.resources.copy(images = this.resources.images.copy(sources = glyphSources ++ this.resources.images.sources))

        def css(names : String*) : $[Style] = names.$./(n => ExternalStyle(n))
        def box(e : Elem, names : String*) = Div(e, css(names : _*))
        def tag(e : Elem, names : String*) = Span(e, css(names : _*))
        def num(s : String) = tag(Text(s), "rk-num")
        def heading(s : String) : Elem = tag(Text(s), "rk-h")
        // the tournament champion's name is golden on the ladder until the next champion (good-game/Tournament.scala)
        var golden : |[String] = None
        def rkName(n : String) : Elem = tag(Text(n), (golden.exists(_.toLowerCase == n.toLowerCase)).?($("rk-name", "hrf-golden")).|($("rk-name")) : _*)

        def back = ZBasic(" ", Div("Back"), () => metaMenu())

        def rating(p : Elo.Player) = p.rating.round.toString + p.provisional.??("?")

        def signed(d : Double) = { val r = d.round ; (r >= 0).?("+" + r).|(r.toString) }

        var mine = $[String]()

        // the tiers (Root's, after rootelo): a player's tier once they're past the provisional games
        val tiers = ladderMetas./(_.rankingTiers).find(_.any).|($)

        def tierOf(p : Elo.Player) : |[RankingTier] = if (p.provisional) None else tiers.find(t => t.min.forall(p.rating >= _))

        def tierRange(t : RankingTier) = t.min./(m => m.round + "+").|("under " + tiers.flatMap(_.min).minOption./(_.round).|(0))

        // the ladder hides the provisional players until asked, as rootelo hides its tierless ones
        var showProvisional = false

        val tierWord = ladderMetas./(_.rankingTierWord).find(_ != "Tier").|("Tier")
        val titles = ladderMetas./(_.rankingTitles).find(_.any).|($)
        val modes = ladderMetas.exists(_.rankingModes)


        // a game's mode: one player a side, two sides (1v1) or more (free-for-all); two sides of two (2v2); other teams
        def modeOf(r : Elo.Result) : String = {
            val sizes = r.sides./(_.entries.num)
            if (sizes.forall(_ == 1)) (sizes.num == 2).?("1v1").|("FFA")
            else if (sizes == $(2, 2)) "2v2"
            else "Teams"
        }

        val modeNames = $("1v1", "2v2", "FFA", "Teams")

        // the mode the ladder and the clan stats show (None: every mode)
        var mode : |[String] = None

        var results = $[Elo.Result]()

        // by wins in every mode, as the game's ranks are, whatever mode the ladder shows
        def winsOf(name : String) = results.%(_.rated).count(r => r.sides.exists(s => s.won && s.names.exists(_.toLowerCase == name.toLowerCase)))
        def titleOf(p : Elo.Player) : |[String] = titles.filter(_._2 <= winsOf(p.name)).lastOption./(_._1)

        def percent(x : Double) = (math.round(x * 1000) / 10.0).toString + "%"
        def percent0(x : Double) = math.round(x * 100) + "%"
        def plural(n : Int, s : String) = n + " " + s + (n != 1).??("s")

        // a faction as a seat records it ("Vagabond (Thief)"): its glyph and its name as the game styles it (its colour)
        case class Ranked(glyph : |[String], elem : Elem)

        val rankedCache = mutable.Map[String, |[Ranked]]()

        def factionOf(name : String) : |[Ranked] = rankedCache.getOrElseUpdate(name, {
            val base = name.takeWhile(_ != '(').trim
            ladderMetas.view.flatMap(m => m.factions.find(f => m.factionName(f) == base)./(f => Ranked(m.glyph(f), m.rankingElem(f)))).headOption
        })

        def colourStyles(e : Elem) : $[Style] = e match {
            case Span(Text(_), ss) => ss
            case Span(e, _) => colourStyles(e)
            case Div(e, _) => colourStyles(e)
            case Concat(a, _) => colourStyles(a)
            case _ => $
        }

        def colour(name : String) : $[Style] = factionOf(name)./(r => colourStyles(r.elem)).|($)

        def glyph(name : String, size : String = "rk-glyph") : Elem =
            factionOf(name).flatMap(_.glyph).filter(g => glyphSources.contains(g.toLowerCase)).map(g => Image(g, css(size), |(name)) : Elem).getOrElse(tag(Text(name.take(1)), size, "rk-glyph-none"))

        def factionName(name : String) : Elem = Span(Text(name), colour(name) ++ css("rk-fname"))

        // a bar: the share (0 to 1) filled in the faction's colour, a tick at what a seat would expect
        def width(x : Double) = math.max(0, math.min(100, math.round(x * 100).toInt))

        def bar(x : Double, expected : |[Double], colours : $[Style]) : Elem =
            Span(tag(Empty, "rk-fill", "rk-w" + width(x)) ~ expected./(e => tag(Empty, "rk-exp", "rk-x" + width(e)) : Elem).|(Empty), colours ++ css("rk-track") ++ colours.any.$(ExternalStyle("rk-has-colour")))

        // a rating as a badge, its ring by band: 1100 and up, 1000 and up, under 1000; a provisional rating has a dashed ring
        def badge(p : Elo.Player, large : Boolean = false) =
            tag(num(p.rating.round.toString) ~ p.provisional.?(tag(Text("?"), "rk-q")).|(Empty), "rk-badge", large.?("rk-badge-large").|("rk-badge-small"), (p.rating >= 1100).?("rk-t3").|((p.rating >= 1000).?("rk-t2").|("rk-t1")), p.provisional.?("rk-prov").|("rk-firm"))

        def delta(d : Double) = { val r = d.round ; tag(Text(signed(d)), "rk-num", "rk-delta", (r > 0).?("rk-up").|((r < 0).?("rk-down").|("rk-even"))) }

        // the factions a player played most, for their row
        def favourites(p : Elo.Player) : $[String] = p.history.flatMap(_.entry.seat).map(_.faction.takeWhile(_ != '(').trim).groupBy(identity).toList.sortBy(x => (-x._2.size, x._1)).map(_._1).take(3)

        // one stat (a faction, a seat): its glyph or seat number, its name, games and average score, and a bar of its relative score
        def statRow(st : Elo.Stat, icon : Elem, label : Elem, colours : $[Style], expected : |[Double]) =
            box(
                icon ~
                tag(label ~ tag(Text(plural(st.games, "game") + " · score " + (math.round(st.score * 100) / 100.0) + st.average./(a => " · avg " + (math.round(a * 10) / 10.0) + " " + unit).|("") + expected./(e => " · " + percent0(e) + " expected").|("")), "rk-sub"), "rk-who") ~
                bar(st.relative, expected, colours) ~
                num(percent(st.relative))(css("rk-pct")),
            "rk-stat")

        def factionRows(group : Elem, l : $[Elo.Stat], expected : String => |[Double]) =
            l./(st => ZOption(group, statRow(st, glyph(st.key), factionName(st.key), colour(st.key), expected(st.key))))

        def seatRows(group : Elem, l : $[Elo.Stat], expected : String => |[Double]) =
            l./(st => ZOption(group, statRow(st, tag(num(st.key.takeWhile(_ != ' ')), "rk-seat"), tag(Text("Seat " + st.key), "rk-fname"), $, expected(st.key))))

        def note(group : Elem, e : Elem) = ZOption(group, box(e, "rk-note"))

        def player(p : Elo.Player, all : $[Elo.Player]) {
            val seats = p.history./~(c => c.entry.seat./(e => (c.result, c.side, e)))
            val rank = all.indexOf(p) + 1

            def figure(n : String, label : String) = tag(num(n)(css("rk-figure-n")) ~ tag(Text(label), "rk-figure-l"), "rk-figure")

            ui.action.asker.zask(
                tabs("Player's Journey", all) ++
                $(ZOption(Empty, box(
                    box(badge(p, true) ~ tag(Text(p.name), "rk-profile-name") ~ tierOf(p)./(t => tierPill(t)).|(Empty) ~ titleOf(p)./(t => tag(Text(t), "rk-title")).|(Empty) ~ tag(Text(p.provisional.?("Provisional · " + p.games + " of " + Elo.provisional + " games").|("Rank " + rank)), "rk-profile-rank"), "rk-profile-head") ~
                    box(figure(p.games.toString, "games") ~ figure(p.wins.toString, "won") ~ figure(percent0(p.wins.toDouble / p.games), "win rate") ~ figure(p.peak.round.toString, "peak"), "rk-figures"),
                "rk-profile"))) ++
                tiers.any.??(journey(p, all)) ++
                factionRows(heading("By faction"), Elo.stats(seats, _.faction), _ => None) ++
                seatRows(heading("By turn order"), Elo.stats(seats, e => e.seat + " of " + e.players).sortBy(_.key), k => k.split(' ').last.toIntOption./(1.0 / _)) ++
                p.history.reverse./{ c =>
                    ZOption(heading("Games"), box(
                        tag(Text(c.won.?("Won").|("Lost")), "rk-result", c.won.?("rk-won").|("rk-lost")) ~
                        c.entry.seat./(e => glyph(e.faction)).|(tag(Empty, "rk-glyph", "rk-glyph-none")) ~
                        tag(
                            tag(Text(c.result.title), "rk-game-title") ~
                            tag(Text(c.result.time.take(10) + c.entry.seat./(e => " · " + e.faction + ", seat " + e.seat + e.score./(x => " · " + (x == "D").?("dominance").|(x + " " + unit)).|("")).|("") + " · against " + c.opponents.join(", ")), "rk-sub"),
                        "rk-who") ~
                        tag(delta(c.after - c.before) ~ tag(num(c.after.round.toString), "rk-after"), "rk-change"),
                    "rk-game"))
                } ++
                $(ZBasic(" ", Div("Back"), () => show(all)))
            )
        }

        // the players counts the games had, for the stats' filter (None: every game)
        def playerCounts = results./(_.players).distinct.sorted

        def filterOptions(group : Elem, n : |[Int], go : |[Int] => Unit) =
            (playerCounts.num > 1).$(ZBasic(group, tag(Text("All games"), "rk-chip-text"), () => go(None), css("rk-chip") ++ n.none.$(ExternalStyle("rk-chip-on")) ++ $(xlo.pointer))) ++
            (playerCounts.num > 1).??(playerCounts./(k => ZBasic(group, tag(Text(k + " players"), "rk-chip-text"), () => go(|(k)), css("rk-chip") ++ n.has(k).$(ExternalStyle("rk-chip-on")) ++ $(xlo.pointer))))

        def unit = meta.ranking.has("root").?("VP").|("fame")

        def explainStats = "From the finished online games" ~ meta.ranking.has("root").?(" (bots too, before 2026-11-18)").|(" with people on two sides or more") ~ ". Score: " ~ "1".hl ~ " for a win, shared between the winning sides (a coalition, a shared win); relative score: score per game; average score: the " ~ unit ~ " at the end" ~ meta.ranking.has("root").??(" (a dominance not counted)") ~ "."

        def factions(all : $[Elo.Player], n : |[Int]) {
            val seats = Elo.seats(results.%(r => n.forall(_ == r.players) && (modes.not || mode.forall(_ == modeOf(r)))))
            ui.action.asker.zask(
                $(ZOption((meta.label + " Faction Stats").hl.spn(xstyles.larger110), box(explainStats ~ n./(k => " The tick on each bar is what a seat would expect in a " + k + " player game (" + percent0(1.0 / k) + ").").|(""), "rk-note"))) ++
                tabs(modes.?("Clans").|("Factions"), all) ++
                modeChips(heading("Mode"), () => factions(all, n)) ++
                filterOptions(heading("Players"), n, factions(all, _)) ++
                factionRows(heading("Factions"), Elo.stats(seats, _.faction), _ => n./(1.0 / _)) ++
                seats.none.$(note(heading("Factions"), "No games yet.".txt)) ++
                $(ZBasic(" ", Div("Back"), () => show(all)))
            )
        }

        def turns(all : $[Elo.Player]) {
            val seats = Elo.seats(results)
            ui.action.asker.zask(
                $(ZOption((meta.label + " Turn Order Stats").hl.spn(xstyles.larger110), box(explainStats ~ " Seat " ~ "1".hl ~ " is the first player; the tick on each bar is what a seat would expect.", "rk-note"))) ++
                tabs("Turn Order", all) ++
                seats./(_._3.players).distinct.sorted./~{ k =>
                    seatRows(heading(k + " players"), Elo.stats(seats.%(_._3.players == k), _.seat.toString).sortBy(_.key.toInt), _ => |(1.0 / k))
                } ++
                seats.none.$(note(heading("Seats"), "No games yet.".txt)) ++
                $(ZBasic(" ", Div("Back"), () => show(all)))
            )
        }

        // (the text in one span: the info box lays out its children apart, so each highlighted word was on a line of its own)
        def says(e : Elem, again : |[() => Unit] = None) = ui.action.asker.zask($(ZOption(title, Div(e.spn, ZBasic.info))) ++ again./(f => ZBasic(" ", Div("Try again"), f)) ++ $(ZBasic(" ", Div("Back"), () => load())))
        def unreachable() = says("The server can't be reached.".txt)

        // the player code from another device, or a name and its own code (Ranking.signIn; 2026-11-19)
        def useCode() {
            // (a code the player picked, typed here, was taken for a name and refused: it now asks which name the code is for (2026-11-20))
            // (the prompt's two choices on lines of their own, the choice itself bright: "or your name" was missed in one line (2026-11-20))
            val b = "<span style='color:#f2f2f2;font-weight:bold'>"
            val d = "<span style='font-size:85%;opacity:0.8'>"
            ui.action.asker.iask(InputOption(b + "Enter either code" + "</span>\n\n" +
                b + "Your player code</span>\n" + d + "the 24 letters under Show my player code on your other device</span>\n\n" +
                b + "or a code you picked yourself</span>\n" + d + "your name comes next</span>\n", "", Nil, v => {
                val code = v.trim
                postF(server + "/player-code", code) { r =>
                    val names = r.split('\n').$.%(_.nonEmpty)
                    if (names.any && code.length >= 16) {
                        PlayerKey.set(code)
                        mine = names
                        load()
                    }
                    else
                        postF(server + "/has-code", code) { r =>
                            if (r.trim == "yes")
                                signIn(code)
                            else
                                codeFor(code)
                        } { unreachable() }
                } { unreachable() }
            }) :: BasicOption(" ", "Cancel", Nil, () => load()))
        }

        // a code the player picked: which name it was picked for
        def codeFor(code : String) {
            ui.action.asker.iask(InputOption("<span style='color:#f2f2f2;font-weight:bold'>Which name did you pick this code for?</span>\n", "", Nil, n => {
                val name = n.trim
                if (name.isEmpty) useCode()
                else
                postF(server + "/sign-in", name + "\n" + code + "\n" + PlayerKey.get) { r =>
                    r.split('\n').$ match {
                        case "ok" :: n :: _ => mine = (mine :+ n).distinct ; load()
                        case "locked" :: _ => says("Too many wrong codes for " ~ name.hlb ~ ". Try again in an hour, or ask for a reset with " ~ "Lost my code".hl ~ ".")
                        case "wrong" :: _ => says("That isn't the code for " ~ name.hlb ~ ". Check the name and the code: the code is case-sensitive.", |(() => useCode()))
                        case _ => says(name.hlb ~ " has no code of its own yet, and " ~ code.hlb ~ " isn't a player code. On the device that holds " ~ name.hlb ~ ", tap " ~ "Set or change my own code".hl ~ " first.", |(() => useCode()))
                    }
                } { unreachable() }
            }) :: BasicOption(" ", "Cancel", Nil, () => load()))
        }

        def signIn(name : String) {
            ui.action.asker.iask(InputOption("Enter the code you picked for " + name, "", Nil, v => {
                postF(server + "/sign-in", name + "\n" + v.trim + "\n" + PlayerKey.get) { r =>
                    r.split('\n').$ match {
                        case "ok" :: n :: _ => mine = (mine :+ n).distinct ; load()
                        case "locked" :: _ => says("Too many wrong codes for " ~ name.hlb ~ ". Try again in an hour, or ask for a reset with " ~ "Lost my code".hl ~ ".")
                        case _ => says("That isn't the code for " ~ name.hlb ~ ".", |(() => signIn(name)))
                    }
                } { unreachable() }
            }) :: BasicOption(" ", "Cancel", Nil, () => load()))
        }

        // a code of the player's own for the names this browser holds (Ranking.setCode)
        // the player code, in the menu itself: the overlay it was shown in left the menu hidden once closed (2026-11-19)
        // a code of the player's own is kept only scrambled (Ranking.setCode), so it can't be shown: the screen says which names
        // have one, as players who set one took the device's code shown here for their old one (2026-11-20)
        // Discord turn pings (TurnPings in good-game; 2026-11-20): for each name this device holds, its Discord user ID and
        // whether it is mentioned in the game's Discord post when it becomes its turn
        def discordPings() {
            def setId(name : String, value : String) {
                postF(server + "/tournament/discord", PlayerKey.get + "\n" + name + "\n" + value.trim) { r =>
                    r.trim match {
                        case "ok" if value.trim.nonEmpty => setPings(name, true)
                        case "ok" => discordPings()
                        case "invalid" => says("That isn't a Discord username or user ID.".txt, |(() => askId(name)))
                        case "notfound" => says(discordNotFound.txt, |(() => askId(name)))
                        case "nobot" => says(discordNoBot.txt, |(() => askId(name)))
                        case _ => says("Only the device that holds the name can set its Discord.".txt)
                    }
                } { unreachable() }
            }

            def setPings(name : String, on : Boolean) {
                postF(server + "/discord/pings", PlayerKey.get + "\n" + name + "\n" + on.?("on").|("off")) { _ => discordPings() } { unreachable() }
            }

            def askId(name : String) {
                val head = "Discord username for " + name
                ui.action.asker.iask(
                    BasicOption(head, "Your Discord username (or your user ID). You need to be a member of the Discord server. Leave it empty to forget it.", Nil, null) ::
                    InputOption(head, "", Nil, v => setId(name, v), _ => true) ::
                    BasicOption(" ", "Cancel", Nil, () => discordPings()))
            }

            postF(server + "/discord/mine", PlayerKey.get) { text =>
                val rows = text.split('\n').$.%(_.nonEmpty)./(_.split('\t').$ ++ $("", "", ""))
                val group = heading("Discord turn pings")
                ui.action.asker.zask(
                    $(note(group, "Each online Root game gets its own post in the Discord server's games forum. When it becomes your turn, you are mentioned (pinged) there, unless you had the game open in the last minute or two. It needs your Discord username, and you need to be a member of the Discord server. Tournament games ping you in your table's thread whenever you gave it, unless you turn pings off here.".txt)) ++
                    rows.none.$(note(group, "This device holds no name yet. Join an online game, or use your code from another device.".txt)) ++
                    rows./~{ r =>
                        // the fourth field: "ping" when the server has a user ID to ping (given, or a username the Discord bot found)
                        val (n, id, on) = (r(0), r(1), r(2) == "on")
                        val digits = id.any && (id.forall(_.isDigit) || r(3) == "ping")
                        $(ZBasic(heading(n), "Discord: " ~ id.any.?(id.hh).|("not set".txt), () => askId(n))) ++
                        digits.$(ZBasic(heading(n), "Turn pings: " ~ on.?("On".hlb).|("Off".hh) ~ " (tap to turn " ~ on.?("off").|("on") ~ ")", () => setPings(n, on.not))) ++
                        (id.any && digits.not).$(note(heading(n), id.hlb ~ " wasn't found on the Discord server yet, so it can't be pinged: tap it to give it again."))
                    } ++
                    $(ZBasic(" ", Div("Back"), () => load()))
                )
            } { unreachable() }
        }

        def showCode() {
            def shown(own : $[String]) = ui.action.asker.zask(
                $(ZOption(heading("This device's player code"), Div("Enter it under Rankings on another device to play there under your names. Keep it to yourself: anyone with it can.".txt ~ Break ~ PlayerKey.get.hlb.spn(xstyles.larger110), ZBasic.info))) ++
                own.any.$(ZOption(heading("Your own code"), Div(("You set a code of your own for " ~ own./(_.hlb).join(", ") ~ ". It isn't shown here: the site keeps it only scrambled, so no one can read it, the site included. It still works, and the code above does too. On another device, tap " ~ "Use a code from another device".hl ~ ", enter your name, then your own code. Forgot it? Pick a new one with " ~ "Set or change my own code".hl ~ ".").spn, ZBasic.info))) ++
                $(ZBasic(" ", Div("Back"), () => load())))

            def check(rest : $[String], own : $[String]) : Unit = rest match {
                case n :: more => postF(server + "/has-code", n) { r => check(more, (r.trim == "yes").?(own :+ n).|(own)) } { check(more, own) }
                case Nil => shown(own)
            }

            check(mine, $)
        }

        // a code is set, or changed (the last one set wins), for the names this device holds; with none, say how to get one
        def setCode() {
            if (mine.none)
                says("This device holds no name yet. Type a name when you join an online game: it is claimed for this device, and then you can set a code for it here.".txt)
            else
            ui.action.asker.iask(InputOption("Pick a code of your own for " + mine.join(", ") + " (8 to 64 characters). If you set one before, this one replaces it.", "", Nil, v => {
                val code = v.trim
                if (code.length < 8 || code.length > 64)
                    says("A code needs 8 to 64 characters.".txt, |(() => setCode()))
                else
                    postF(server + "/set-code", PlayerKey.get + "\n" + code) { r =>
                        r.split('\n').$ match {
                            case "ok" :: names => says("Your code is set for " ~ names./(_.hlb).join(", ") ~ ". On another device, open Rankings, tap " ~ "Use a code from another device".hl ~ ", enter your name, then this code. Keep it to yourself: anyone with your name and code can play as you. " ~ "Show my player code".hl ~ " still shows this device's code, which works too: your own code is kept scrambled and never shown.")
                            case _ => says("The code wasn't set: this device holds no name.".txt)
                        }
                    } { unreachable() }
            }) :: BasicOption(" ", "Cancel", Nil, () => load()))
        }

        // a lost code: ask the site's owner to release the name (Ranking.request)
        def lostCode() {
            ui.action.asker.iask(InputOption("Your name", "", Nil, n => {
                val name = n.trim
                if (name.isEmpty) load()
                else
                ui.action.asker.iask(InputOption("Anything that shows " + name + " is yours, such as the games you played or who you played against (optional)", "", Nil, m => {
                    postF(server + "/name-reset", name + "\n" + m.trim) { _ =>
                        says("Your request was sent. Once the site's owner releases " ~ name.hlb ~ ", type it again when you join a game, or open one of your games, to claim it on this device. Your past games stay under it.")
                    } { unreachable() }
                }, _ => true) :: BasicOption(" ", "Cancel", Nil, () => load()))
            }) :: BasicOption(" ", "Cancel", Nil, () => load()))
        }

        // a player's row on the ladder: place (a medal for the first three), name, the factions played most, record, last change, rating
        def ladderRow(p : Elo.Player, i : Int, all : $[Elo.Player], group : Elem) = {
            val place = p.provisional.?(tag(Empty, "rk-place", "rk-place-none")).|(Span(num((i + 1).toString), css((i < 3).?($("rk-place", "rk-medal", "rk-medal-" + (i + 1))).|($("rk-place", "rk-place-plain")) : _*)))
            val yours = mine.exists(_.toLowerCase == p.name.toLowerCase)

            ZBasic(group,
                place ~
                tag(
                    rkName(p.name) ~ yours.?(tag(Text("you"), "rk-you")).|(Empty) ~
                    tag(favourites(p)./(f => glyph(f, "rk-glyph-mini")).merge ~ tag(Text(p.wins + "–" + (p.games - p.wins) + " · " + percent0(p.wins.toDouble / p.games) + " won · peak " + p.peak.round + " · " + p.last.take(10)), "rk-num-inline"), "rk-sub"),
                "rk-who") ~
                tag(delta(p.lastChange), "rk-last") ~
                badge(p),
            () => player(p, all), css("rk-row", (i < 3 && p.provisional.not).?("rk-row-top").|("rk-row-plain")) ++ yours.$(ExternalStyle("rk-row-yours")) ++ $(xlo.pointer))
        }

        def explainRatings : Elem = "Elo ratings from finished online games with people on two sides or more (teams count as one side; bots and local games don't count). Everyone starts at " ~ Elo.start.round.toString.hl ~ "; a " ~ "?".hl ~ " marks fewer than " ~ Elo.provisional.hl ~ " games." ~ meta.ranking.has("root").??(" Root ratings count the games played from 2026-11-18.") ~ " Games with fan content (" ~ meta.ranking.has("root").?("fan, playtest or mirror factions, fan maps, decks, hirelings or scenarios").|("fan clans, modules or variants") ~ ") don't count in the ratings. The stats count every game."

        // Root's page, after rootelo: Leaderboard, Top Tables, Player's Journey, the stats and the Codex, as tabs
        def tabs(current : String, all : $[Elo.Player]) =
            tiers.any.??(modes.?($[(String, () => Unit)](
                "Leaderboard" -> (() => show(all)),
                "Clans" -> (() => factions(all, None)),
                "Turn Order" -> (() => turns(all)),
                "Leagues" -> (() => codex(all))
            )).|($[(String, () => Unit)](
                "Leaderboard" -> (() => show(all)),
                "Top Tables" -> (() => topTables(all)),
                "Player's Journey" -> (() => journeys(all)),
                "Factions" -> (() => factions(all, None)),
                "Turn Order" -> (() => turns(all)),
                "Codex" -> (() => codex(all))
            ))./{ case (l, f) => ZBasic(Empty, tag(Text(l), "rk-chip-text"), f, css("rk-chip", "rk-tab") ++ (l == current).$(ExternalStyle("rk-chip-on")) ++ $(xlo.pointer)) })

        def tierIcon(t : RankingTier) : Elem =
            t.icon.filter(i => glyphSources.contains(i.toLowerCase)).map(i => Image(i, css("rk-tier-icon"), |(t.name)) : Elem).getOrElse(tag(Text(t.name.take(1)), "rk-tier-icon", "rk-tier-letter"))

        def tierPill(t : RankingTier) : Elem = tag(tierIcon(t) ~ tag(Text(t.name), "rk-tier-name"), "rk-tier", "rk-tier-" + t.name.toLowerCase)

        def tierStrip = ZOption(Empty, box(tiers./(t => tag(tierPill(t) ~ tag(Text(tierRange(t)), "rk-tier-min"), "rk-tier-key")).merge, "rk-tiers"))

        // a leaderboard row: rank, tier, player, rating, games, wins, win rate, peak, last change (phones: the figures go under the name)
        def lbRow(p : Elo.Player, i : Int, all : $[Elo.Player], group : Elem) = {
            val place = p.provisional.?(tag(Text("–"), "rk-place", "rk-place-dash")).|(Span(num((i + 1).toString), css((i < 3).?($("rk-place", "rk-medal", "rk-medal-" + (i + 1))).|($("rk-place", "rk-place-plain")) : _*)))
            val yours = mine.exists(_.toLowerCase == p.name.toLowerCase)
            def cell(s : String) = num(s)(css("rk-lb-c"))

            ZBasic(group,
                place ~
                tierOf(p)./(t => tierPill(t)).|(tag(Text("–"), "rk-tier", "rk-tier-none")) ~
                tag(
                    rkName(p.name) ~ yours.?(tag(Text("you"), "rk-you")).|(Empty) ~ titleOf(p)./(t => tag(Text(t), "rk-title")).|(Empty) ~
                    tag(favourites(p)./(f => glyph(f, "rk-glyph-mini")).merge ~ tag(Text(plural(p.games, "game") + " · " + p.wins + " won · " + percent0(p.wins.toDouble / p.games) + " · peak " + p.peak.round), "rk-num-inline", "rk-lb-phone"), "rk-sub"),
                "rk-who") ~
                badge(p) ~
                cell(p.games.toString) ~ cell(p.wins.toString) ~ cell(percent0(p.wins.toDouble / p.games)) ~ cell(p.peak.round.toString) ~
                tag(delta(p.lastChange), "rk-last"),
            () => player(p, all), css("rk-row", "rk-lb", (i < 3 && p.provisional.not).?("rk-row-top").|("rk-row-plain")) ++ yours.$(ExternalStyle("rk-row-yours")) ++ $(xlo.pointer))
        }

        def lbHead(group : Elem) = ZOption(group, box($("Rank", tierWord, "Player", "Rating", "Games", "Wins", "Win %", "Peak", "Last")./(l => tag(Text(l), "rk-lb-h")).merge, "rk-lb", "rk-lb-head"))

        def counts(all : $[Elo.Player]) = ZOption(title, box(
            tag(num(all.num.toString) ~ " rated player" ~ (all.num != 1).??("s"), "rk-count") ~
            tag(num(results.num.toString) ~ " game" ~ (results.num != 1).??("s") ~ " recorded", "rk-count") ~
            tag(num(results.%(_.rated).num.toString) ~ " rated", "rk-count"),
        "rk-counts"))

        // the mode chips (Northgard): every mode, then each mode its games have
        def modeChips(group : Elem, again : () => Unit) = modes.??({
            val counts = results.groupBy(modeOf)./{ case (k, l) => k -> l.num }.toMap
            ZBasic(group, tag(Text("All modes"), "rk-chip-text"), () => { mode = None ; again() }, css("rk-chip") ++ mode.none.$(ExternalStyle("rk-chip-on")) ++ $(xlo.pointer)) +:
            modeNames.%(counts.contains)./(k => ZBasic(group, tag(Text(k), "rk-chip-text") ~ tag(Text(counts(k).toString), "rk-num", "rk-chip-n"), () => { mode = |(k) ; again() }, css("rk-chip") ++ mode.has(k).$(ExternalStyle("rk-chip-on")) ++ $(xlo.pointer)))
        })

        def showTiered(everyone : $[Elo.Player]) {
            val all = mode.filter(_ => modes)./(m => Elo.compute(results.%(r => modeOf(r) == m))).|(everyone)
            val you = mine.any.?("Your name" ~ (mine.num > 1).??("s") ~ " on this device: " ~ mine./(_.hlb).join(", ")).|("Names you type when joining an online game are claimed for this device. Play on another device with your player code.".txt)
            val ranked = all.%(_.provisional.not)
            val provisional = all.%(_.provisional)
            val showing = showProvisional || ranked.none
            val group = heading("Leaderboard · only players with a " + tierWord.toLowerCase + " are ranked")

            ui.action.asker.zask(
                $(counts(everyone)) ++
                tabs("Leaderboard", everyone) ++
                $(tierStrip) ++
                modeChips(heading("Mode"), () => show(everyone)) ++
                mode.filter(_ => modes)./(m => note(group, "Ratings from " ~ m.hl ~ " games only.")).$ ++
                (all.any).$(lbHead(group)) ++
                ranked.zipWithIndex./{ case (p, i) => lbRow(p, i, all, group) } ++
                (ranked.none && provisional.any).$(note(group, "No one has a " ~ tierWord.toLowerCase ~ " yet: a " ~ tierWord.toLowerCase ~ " comes after " ~ Elo.provisional.hl ~ " rated games.")) ++
                showing.??(provisional.zipWithIndex./{ case (p, i) => lbRow(p, ranked.num + i, all, group) }) ++
                (provisional.any && ranked.any).$(ZBasic(group, tag(Text(showing.?("Hide").|("Show") + " the " + plural(provisional.num, "provisional player") + " (fewer than " + Elo.provisional + " games)"), "rk-chip-text"), () => { showProvisional = showing.not ; show(everyone) }, css("rk-chip", "rk-toggle") ++ showing.$(ExternalStyle("rk-chip-on")) ++ $(xlo.pointer))) ++
                all.none.$(note(group, tag(Text("No rated games yet."), "rk-empty-title") ~ Break ~ "The ladder starts with the first finished online game with people on two sides or more. Bots, local games and games with fan content don't count.".txt)) ++
                $(ZOption(heading("Your player code"), box(you, "rk-note"))) ++
                $(ZBasic(heading("Your player code"), Div("Show my player code"), () => showCode())) ++
                $(ZBasic(heading("Your player code"), Div("Use a code from another device"), () => useCode())) ++
                $(ZBasic(heading("Your player code"), Div("Set or change my own code"), () => setCode())) ++
                $(ZBasic(heading("Your player code"), Div("Lost my code"), () => lostCode())) ++
                $(ZBasic(heading("Your player code"), Div("Discord turn pings"), () => discordPings())) ++
                $(back)
            )
        }

        // each rated game's changes, by game
        def changesByGame(all : $[Elo.Player]) = all./~(_.history).groupBy(_.result.game)

        // Top Tables: the rated games by the sum of their players' ratings at the table
        def topTables(all : $[Elo.Player]) {
            val games = changesByGame(all).values.$./(cs => (cs.head.result, cs.sortBy(c => (c.won.not, -(c.after - c.before))), cs./(_.before).sum)).sortBy(x => (-x._3, x._1.time))
            val group = heading("Top Tables · rated games by total rating")

            ui.action.asker.zask(
                $(counts(all)) ++
                tabs("Top Tables", all) ++
                $(note(group, "The sum of the players' ratings when the game started; the strongest tables first, each player's change after it.".txt)) ++
                games.take(50).zipWithIndex./{ case ((r, cs, sum), i) =>
                    ZOption(group, box(
                        num((i + 1).toString)(css("rk-tt-rank")) ~
                        tag(num(sum.round.toString)(css("rk-tt-sum")) ~ tag(Text("total"), "rk-tt-l"), "rk-tt-sumbox") ~
                        tag(
                            tag(Text(r.title), "rk-game-title") ~
                            tag(Text(r.time.take(10) + " · " + plural(r.players, "player")), "rk-sub") ~
                            tag(cs./(c => tag(c.entry.seat./(e => glyph(e.faction, "rk-glyph-mini")).|(Empty) ~ tag(Text(c.entry.name), "rk-lu-name") ~ delta(c.after - c.before), "rk-lu", c.won.?("rk-lu-won").|("rk-lu-lost"))).merge, "rk-lineup"),
                        "rk-who"),
                    "rk-tt"))
                } ++
                games.none.$(note(group, "No rated games yet.".txt)) ++
                $(back)
            )
        }

        // Player's Journey: pick a player
        def journeys(all : $[Elo.Player]) {
            val group = heading("Player's Journey")
            ui.action.asker.zask(
                $(counts(all)) ++
                tabs("Player's Journey", all) ++
                $(note(group, "Pick a player to see their rating game by game, their best win and their toughest loss.".txt)) ++
                all./(p => ZBasic(group, tierOf(p)./(t => tierIcon(t)).|(Empty) ~ tag(Text(p.name), "rk-chip-text") ~ tag(Text(rating(p)), "rk-num", "rk-chip-n"), () => player(p, all), css("rk-chip", "rk-pick") ++ $(xlo.pointer))) ++
                all.none.$(note(group, "No rated games yet.".txt)) ++
                $(back)
            )
        }

        // a player's journey: the rating after each game, the strongest opponent beaten and the weakest lost to (their rating then)
        def journey(p : Elo.Player, all : $[Elo.Player]) = {
            val byGame = changesByGame(all)
            def opponents(c : Elo.Change, won : Boolean) = byGame.get(c.result.game).|($).%(o => o.side != c.side && (won || o.won))
            val best = p.history.%(_.won)./~(c => opponents(c, true)).sortBy(o => -o.before).headOption
            val worst = p.history.%(_.won.not)./~(c => opponents(c, false)).sortBy(o => o.before).headOption

            val points = p.history./(_.after)
            val lo = (points :+ Elo.start).min
            val hi = (points :+ Elo.start).max
            def height(v : Double) = (hi - lo < 1).?(50).|(math.round(10 + 90 * (v - lo) / (hi - lo)).toInt)
            val chart = box(
                box(tag(num(hi.round.toString), "rk-chart-max") ~ tag(num(lo.round.toString), "rk-chart-min"), "rk-chart-axis") ~
                box(p.history./(c => tag(Empty, "rk-bar", "rk-ht" + height(c.after), c.won.?("rk-bar-won").|("rk-bar-lost"))).merge, "rk-chart-bars") ~
                box(tag(Text(p.history.head.result.time.take(10)), "rk-sub") ~ tag(Text(p.history.last.result.time.take(10)), "rk-sub"), "rk-chart-dates"),
            "rk-chart")

            def highlight(label : String, o : |[Elo.Change], none : String) = o./(o => ZBasic(heading("Highlights"),
                tag(tag(Text(label), "rk-hl-l") ~ rkName(o.entry.name) ~ tag(Text("rated " + o.before.round + " then · " + o.result.time.take(10) + " · see their journey"), "rk-sub"), "rk-who"),
                () => all.find(_.name.toLowerCase == o.entry.name.toLowerCase).foreach(q => player(q, all)), css("rk-row", "rk-hl") ++ $(xlo.pointer))).|(note(heading("Highlights"), (label + ": " + none).txt))

            $(ZOption(heading("Rating game by game"), chart)) ++
            $(highlight("Best win · the highest-rated opponent beaten", best, "no win yet")) ++
            $(highlight("Toughest loss · the lowest-rated opponent lost to", worst, "no loss yet"))
        }

        // Codex: the tiers and how the ratings work
        def codex(all : $[Elo.Player]) {
            ui.action.asker.zask(
                $(counts(all)) ++
                tabs("Codex", all) ++
                tiers./(t => ZOption(heading(tierWord + "s"), box(tierPill(t) ~ tag(num(tierRange(t)), "rk-tier-min") ~ tag(Text(t.note), "rk-sub"), "rk-tier-row"))) ++
                $(note(heading(tierWord + "s"), modes.?("A league comes after " ~ Elo.provisional.hl ~ " rated games; until then a rating is provisional and shows a " ~ "?".hl ~ ". The leagues are the game's ranked leagues; here they follow the rating, not Glory Points, 100 apart, with everyone starting in Stone at " ~ Elo.start.round.toString.hl ~ ", as the game starts everyone in Stone.").|("A tier comes after " ~ Elo.provisional.hl ~ " rated games; until then a rating is provisional and shows a " ~ "?".hl ~ ". The tiers follow the rootelo ladder's, each 200 lower, as ratings here start at " ~ Elo.start.round.toString.hl ~ " (rootelo's start at 1200)."))) ++
                titles./{ case (t, w) => ZOption(heading("Ranks · by wins"), box(tag(Text(t), "rk-title", "rk-title-big") ~ tag(num((w == 0).?("start").|(w + "+")), "rk-tier-min") ~ tag(Text((w == 0).?("Where everyone starts").|(plural(w, "win") + " in any mode, with any clan")), "rk-sub"), "rk-tier-row")) } ++
                modes.$(note(heading("Modes"), "1v1".hl ~ ": two players. " ~ "2v2".hl ~ ": two teams of two. " ~ "FFA".hl ~ ": three players or more, each on their own. " ~ "Teams".hl ~ ": any other teams. The ladder counts every mode together; pick a mode to see a ladder of its games only.")) ++
                $(note(heading("How the rankings work"), explainRatings)) ++
                $(note(heading("Not tracked here"), modes.?("Glory Points and seasons: one rating ladder counts every rated game.").|("Seasons and leagues: every rated game since 2026-11-18 counts in one ladder. Live or async play: games don't record it.").txt)) ++
                $(back)
            )
        }


        def show(all : $[Elo.Player]) = if (tiers.any) showTiered(all) else showPlain(all)

        def showPlain(all : $[Elo.Player]) {
            val explain = explainRatings
            val you = mine.any.?("Your name" ~ (mine.num > 1).??("s") ~ " on this device: " ~ mine./(_.hlb).join(", ")).|("Names you type when joining an online game are claimed for this device. Play on another device with your player code.".txt)

            val ranked = all.%(_.provisional.not)
            val provisional = all.%(_.provisional)
            val seats = Elo.seats(results)
            val factionStats = Elo.stats(seats, _.faction)
            val top = 6

            ui.action.asker.zask(
                $(ZOption(title, box(
                    tag(num(all.num.toString) ~ " rated player" ~ (all.num != 1).??("s"), "rk-count") ~
                    tag(num(results.num.toString) ~ " game" ~ (results.num != 1).??("s") ~ " recorded", "rk-count") ~
                    tag(num(results.%(_.rated).num.toString) ~ " rated", "rk-count"),
                "rk-counts"))) ++
                ranked.zipWithIndex./{ case (p, i) => ladderRow(p, i, all, heading("Ladder")) } ++
                (ranked.none && provisional.any).$(note(heading("Ladder"), "No one has " ~ Elo.provisional.hl ~ " rated games yet. Ranks start at " ~ Elo.provisional.hl ~ " games; until then a rating is provisional.")) ++
                provisional.zipWithIndex./{ case (p, i) => ladderRow(p, ranked.num + i, all, heading("Provisional · fewer than " + Elo.provisional + " games")) } ++
                all.none.$(note(heading("Ladder"), tag(Text("No rated games yet."), "rk-empty-title") ~ Break ~ "The ladder starts with the first finished online game with people on two sides or more. Bots, local games and games with fan content don't count.".txt)) ++
                factionRows(heading("Factions · relative score, every game"), factionStats.take(top), _ => None) ++
                seats.any.$(ZBasic(" ", Div((factionStats.num > top).?("All " + factionStats.num + " factions, by player count").|("Faction stats by player count")), () => factions(all, None))) ++
                seats.any.$(ZBasic(" ", Div("Turn order stats"), () => turns(all))) ++
                $(ZOption(heading("Your player code"), box(you, "rk-note"))) ++
                $(ZBasic(heading("Your player code"), Div("Show my player code"), () => showCode())) ++
                $(ZBasic(heading("Your player code"), Div("Use a code from another device"), () => useCode())) ++
                $(ZBasic(heading("Your player code"), Div("Set or change my own code"), () => setCode())) ++
                $(ZBasic(heading("Your player code"), Div("Lost my code"), () => lostCode())) ++
                $(ZBasic(heading("Your player code"), Div("Discord turn pings"), () => discordPings())) ++
                $(note(heading("How the rankings work"), explain)) ++
                $(back)
            )
        }

        def load() {
            ui.action.asker.zask($(ZOption(title, box(tag(Empty, "rk-skeleton") ~ tag(Empty, "rk-skeleton") ~ tag(Empty, "rk-skeleton"), "rk-loading")), back))

            def rankings() =
                getF(server + "/rankings/" + metas) { text =>
                    results = Elo.parse(text)
                    show(Elo.compute(results))
                } {
                    ui.action.asker.zask($(ZOption(title, Div("The server can't be reached.", ZBasic.info)), back))
                }

            def code() = postF(server + "/player-code", PlayerKey.get) { r => mine = r.split('\n').$.%(_.nonEmpty) ; rankings() } { rankings() }

            if (HRF.metas.exists(m => meta.ranking.any && m.ranking == meta.ranking && m.tournament.any))
                getF(server + "/tournament") { text =>
                    golden = text.split('\n').$./(_.split('\t').$).%(_.headOption.has("golden")).headOption./~(_.lift(1))
                    code()
                } { code() }
            else
                code()
        }

        load()
    }

    // THE TOURNAMENT (good-game/Tournament.scala; Root's, asked for on 2026-11-20): sign-ups with a ranking name, the tables with
    // their winners and spectator links, the player's own games and the champion, whose name is golden until the next one's
    def tournamentMenu() {
        val server = HRF.server.get
        val tmeta = HRF.metas.%(m => meta.tournament.has(m.name)).single.|(meta)
        val title = (meta.label.split(':').head + " Tournament").hl.spn(xstyles.larger110)

        def back = ZBasic(" ", Div("Back"), () => metaMenu())
        // (the text in one span: the info box lays out its children apart, so each highlighted word was on a line of its own)
        def note(group : Elem, e : Elem) = ZOption(group, Div(e.spn, ZBasic.info))
        def golden(n : String) : Elem = Span(Text(n), $(ExternalStyle("hrf-golden")))
        def url(secret : String) = server + "/play/" + tmeta.name + "/" + secret
        def linked(u : String, e : Elem) = (Link(u, e, $(xstyles.link, xlo.fullwidth, xlo.fullheight)) ~ Image("external-link")(xstyles.explain)(xstyles.clickThrough)).div(ZBasic.choice)

        def says(e : Elem) = ui.action.asker.zask($(note(title, e), ZBasic(" ", Div("Back"), () => load())))
        def unreachable() = says("The server can't be reached.".txt)

        def signUp(name : String) {
            val n = name.split(' ').filter(_ != "").join(" ").sanitize(32)
            val options = tmeta.tournamentOptions./(o => tmeta.writeOption(o)).join(" ")
            postF(server + "/tournament/sign-up", PlayerKey.get + "\n" + n + "\n" + HRF.version + "\n" + options) { r =>
                r.trim match {
                    case "ok" => askDiscord(n, true)
                    case "taken" => says(n.hlb ~ " is another player's name. Pick another, or use your code from another device under " ~ "Rankings".hl ~ ".")
                    case "already" => says(n.hlb ~ " has already signed up.")
                    case "one" => says("This device has already signed up a name for this tournament.".txt)
                    case "playing" => says(n.hlb ~ " is still playing at a table of the tournament.")
                    case "closed" => says("Sign-ups are closed until the final of the tournament being played starts.".txt)
                    case _ => says("That name can't be used.".txt)
                }
            } { unreachable() }
        }

        def withdraw(name : String) {
            postF(server + "/tournament/withdraw", PlayerKey.get + "\n" + name) { _ => load() } { unreachable() }
        }

        def askName() {
            ui.action.asker.iask(InputOption("Your name for the rankings and the tournament", "", Nil, v => signUp(v)) :: BasicOption(" ", "Cancel", Nil, () => load()))
        }

        // the Discord a name's player gives, so its table's thread on the Discord server can mention them: a username, which the
        // server's Discord bot looks up (DiscordBot in good-game; 2026-11-20), or a user ID
        def setDiscord(name : String, value : String) {
            postF(server + "/tournament/discord", PlayerKey.get + "\n" + name + "\n" + value.trim) { r =>
                r.trim match {
                    case "ok" => load()
                    case "invalid" => says("That isn't a Discord username or user ID.".txt)
                    case "notfound" => says(discordNotFound.txt)
                    case "nobot" => says(discordNoBot.txt)
                    case _ => says("Only the device that holds the name can set its Discord.".txt)
                }
            } { unreachable() }
        }

        def askDiscord(name : String, first : Boolean) {
            val head = (if (first) "Signed up as " + name + ". " else "") + "Discord for " + name + " (optional)"
            ui.action.asker.iask(
                BasicOption(head, "Each table gets its own thread on the Discord server. Give your Discord username to be mentioned (pinged) there. You need to be a member of the Discord server.", Nil, null) ::
                InputOption(head, "", Nil, v => setDiscord(name, v), _ => true) ::
                BasicOption(" ", first.?("Skip").|("Cancel"), Nil, () => load()))
        }

        def show(text : String, owned : $[String], mine : $[$[String]], discord : $[$[String]] = $) {
            val rows = text.split('\n').$.%(_.nonEmpty)./(_.split('\t').$)
            def lines(k : String) = rows.%(r => r.headOption.has(k))
            val size = lines("size").headOption./~(_.lift(1))./~(_.toIntOption).|(16)
            val champion = lines("golden").headOption./~(_.lift(1))
            val open = lines("open").headOption
            val signups = lines("signup")./~(_.lift(2))
            val tables = lines("table").%(_.num >= 6)
            val champions = lines("champion").%(_.num >= 3)
            val cancelled = lines("cancelled")./~(_.lift(1))
            def isGolden(n : String) = champion.exists(_.toLowerCase == n.toLowerCase)
            def name(n : String) : Elem = isGolden(n).?(golden(n)).|(n.hh)
            def names(l : $[String]) : Elem = l./(name).join(", ")

            val signGroup : Elem = open./(o => ("Tournament " + o(1) + " · sign-ups " + o(2) + " of " + size).hl).|("Sign-ups".hl)
            val signed = owned.%(n => signups.exists(_.toLowerCase == n.toLowerCase))

            ui.action.asker.zask(
                $(note(title, "Sign up with your ranking name. The tournament starts by itself when " ~ size.hl ~ " players have signed up: four tables of four, each an " ~ "Advanced Setup".hh ~ " game. Each table's winner goes to the final, and the final's winner is the champion, whose name is golden until the next champion." ~ Break ~ "The draft: the flop of five factions, then the first seat picks the deck and the second the map. On the " ~ "Mountain".hh ~ ", the third seat picks the one landmark for " ~ "Pass".hh ~ ", where the " ~ "Tower".hh ~ " usually stands: the " ~ "Tower".hh ~ ", or instead " ~ "Lost City".hh ~ ", " ~ "Mousehold".hh ~ ", " ~ "Foxburrow".hh ~ ", " ~ "Rabbittown".hh ~ " or the " ~ "Birdsong Tower".hh ~ " (rule its clearing at the start of your Birdsong to score 1). Then the factions are picked, last seat first. Official factions, decks and maps, no " ~ "Vagabond".hh ~ ".")) ++
                champion./(c => note("Champion".hl, golden(c) ~ " won the last tournament.")).$ ++
                mine./(r => ZOption("Your Tournament Game".hl, linked(url(r(3)), r(2).hh ~ Break ~ ("as " + r(4) + " against " + r(5)).spn(xstyles.smaller75)))) ++
                open./(_ => note(signGroup, signups.any.?(names(signups)).|("No one has signed up yet.".txt))).$ ++
                open.any.??(owned.diff(signed)./(n => ZBasic(signGroup, "Sign up as " ~ n.hh, () => signUp(n)))) ++
                open.any.$(ZBasic(signGroup, owned.any.?("Sign up with another name").|("Sign up"), () => askName())) ++
                open.any.??(signed./(n => ZBasic(signGroup, "Withdraw " ~ n.hh, () => withdraw(n)))) ++
                owned./(n => ZBasic("Discord".hl, "Discord for " ~ n.hh ~ ": " ~ discord.find(_.lift(1).exists(_.toLowerCase == n.toLowerCase))./~(_.lift(2))./(v => (v.forall(_.isDigit) || discord.exists(d => d.lift(2).has(v) && d.lift(3).has("ping"))).?(v.hh).|(v.hh ~ " (not found on the Discord server yet: tap to give it again)".txt)).|("not set".txt), () => askDiscord(n, false))) ++
                open.none.$(note("Sign-ups".hl, "Sign-ups for the next tournament open when the final of this one starts.".txt)) ++
                tables.groupBy(_(1)).toList.sortBy(-_._1.toIntOption.|(0))./~{ case (n, l) =>
                    val group = ("Tournament " + n + champions.find(_(1) == n)./(c => " · champion " + c(2)).|("") + cancelled.has(n).??(" · cancelled")).hl
                    l.sortBy(_(2).replace("F", "Z"))./{ t =>
                        val label = (t(2) == "F").?("Final").|("Table " + t(2))
                        val winner = t(4)
                        ZOption(group, linked(url(t(3)), label.hh ~ ": " ~ names(t.drop(5)) ~ Break ~ (winner.nonEmpty.?("Won by " ~ name(winner) ~ " · ").|(Empty) ~ "Spectate").spn(xstyles.smaller75)))
                    }
                } ++
                $(note("Your player code".hl, owned.any.?("Your names on this device: " ~ owned./(_.hlb).join(", ") ~ ". On another device, use your player code under " ~ "Rankings".hl ~ " to play there under the same names.").|(Empty ~ "No name is claimed on this device yet. Signing up claims one; if you have one on another device, use its player code under " ~ "Rankings".hl ~ " first."))) ++
                $(back)
            )
        }

        def load() {
            ui.action.asker.zask($(note(title, "Loading...".txt), back))

            getF(server + "/tournament") { text =>
                postF(server + "/player-code", PlayerKey.get) { owned =>
                    val names = owned.split('\n').$.%(_.nonEmpty)
                    postF(server + "/tournament/mine", PlayerKey.get) { mine =>
                        val rr = mine.split('\n').$.%(_.nonEmpty)./(_.split('\t').$)
                        show(text, names, rr.%(_.num >= 6), rr.%(_.headOption.has("discord")))
                    } { show(text, names, $) }
                } { show(text, $, $) }
            } { unreachable() }
        }

        load()
    }

    def aboutMenu() {
        ui.action.asker.zask(
            Credits.options ++
            $(ZOption("HRF", Link("https://boardgamegeek.com/user/hauntrollfail", "Contact JT".spn.div, ZBasic.choice ++ $(xstyles.link)))) ++
            meta.about.any.$(ZOption("HRF", OnClick(Div("History", ZBasic.choice ++ $(xstyles.link))), _ => showOverlayRaw(meta.label.styled(xstyles.bright)(xstyles.larger125), meta.about)).copy(clear = false)) ++
            $(ZBasic(" ", "Done".hh, () => {
                metaMenu()
            }, ZBasic.choice ++ $(xlo.fullwidth)))
        )
    }

    def quickGame() {
        HRF.segments = $

        val (f, l, d, o) = meta.randomQuickGame()

        val bots = d.view.mapValues(s => Bot(s)).toMap + (f -> Human)

        val title = meta.randomGameName()

        startGame(l, bots, o, $, newLocalGame("quick", l, bots, o, title), title, () => Map(), NoSwitches)
    }

    def quickGameOld() {
        HRF.segments = $

        ui.action.asker.zask(meta.factions./(faction => ZBasic(meta.factionGroup(faction).|("Play as".txt), meta.factionElem(faction).spn(xstyles.bold) ~ meta.factionNote(faction), () => {
            ui.action.scroll.scrollTop = 0

            val ff = meta.factions.but(faction).%(f => meta.getBots(f).has(meta.defaultBot(f)))
            def randomOpponents = meta.quickFactions.but(faction).shuffle.take(meta.quickMin.to(meta.quickMax).$.shuffle.head - 1)

            var factions = (faction +: randomOpponents).shuffle

            while (meta.validateFactionCombination(factions).ok.not && random() > 0.0001) {
                factions = (faction +: randomOpponents).shuffle

                while (meta.validateFactionCombination(factions).ok.not && random() > 0.0001) {
                    factions = factions.shuffle
                }
            }

            def askAdd() {
                val options = OptionsState(meta.optionsFor(factions.num, factions), meta.mandatoryFor(factions.num, factions), $, meta.defaultsFor(factions.num, factions)).checkDimmed().selected

                while (meta.validateFactionCombination(factions).ok.not && random() > 0.0001) {
                    factions = factions.shuffle
                }

                val v = meta.validateFactionSeatingOptions(factions, options)

                ui.action.asker.zask(
                    $(ZBasic("Play as", meta.factionElem(faction) ~ meta.factionNote(faction))) ++
                    factions.but(faction)./(o => ZBasic("Against", meta.factionElem(o).spn(xstyles.bold) ~ meta.factionNote(o), () => {
                        factions :-= o
                        askAdd()
                    }, ZBasic.infoch)) ++
                    ZBasic("", v.ok.?("Start Game".hl ~ v.message.any.??(" | ")).|(Empty) ~ v.message.styled(v.style), v.ok.??(() => {
                        val seating = factions

                        val difficulties = seating./(f => f -> Bot(meta.defaultBot(f))).toMap ++ meta.botOnly(faction).not.$(faction -> Human)

                        val journal = new MemoryJournal[meta.gaming.ExternalAction](meta)

                        startGame(seating, difficulties, options, $, journal, meta.randomGameName(), () => Map(), NoSwitches)
                    })).? ++
                    ff.diff(factions)./(f => ZOption(Div(meta.factionGroup(f).|("Factions".txt)), OnClick(Div(Text(meta.factionName(f)), ZBasic.infoch)), _ => {
                        factions :+= f
                        askAdd()
                    })) ++
                    ZBasic(" ", "Cancel", () => {
                        // metaMenu()
                        history.popState()
                    }).?
                )
            }

            askAdd()
        })) :+ ZBasic(" ", "Cancel", () => {
            // metaMenu()
            history.popState()
        }))
    }

    // Accounts on Play Online (asked for on 2026-11-20): a player signed in on one device couldn't sign in on another, as typing
    // their name there said it was taken. A name is held by the devices whose keys claimed it (PlayerKey, good-game/Ranking.scala);
    // another device signs in with the name's own code (Ranking.signIn) or a held device's player code, which it then uses as its key.
    def accountSays(e : Elem, back : () => Unit, again : |[() => Unit] = None) =
        ui.action.asker.zask($(ZOption("Account".hl, Div(e.spn, ZBasic.info))) ++ again./(f => ZBasic(" ", Div("Try again"), f)) ++ $(ZBasic(" ", Div("Back"), back)))

    val accountBright = "<span style='color:#f2f2f2;font-weight:bold'>"
    val accountDim = "<span style='font-size:85%;opacity:0.8'>"

    // sign in as a name (asked for when none is given); done gets the name as the server spells it
    def signInAs(name0 : |[String], done : String => Unit, back : () => Unit) {
        val server = HRF.server.get
        val b = accountBright
        val d = accountDim
        def unreachable() = accountSays("The server can't be reached.".txt, back)

        def askCode(name : String) {
            ui.action.asker.iask(InputOption(b + "Sign in as " + name + "</span>\n\n" +
                b + "Your code</span>\n" + d + "the code you picked for " + name + ",\nor the 24-letter player code\nfrom a device signed in as " + name + "</span>\n", "", Nil, v => {
                val code = v.trim

                def own() = postF(server + "/sign-in", name + "\n" + code + "\n" + PlayerKey.get) { r =>
                    r.split('\n').$ match {
                        case "ok" :: n :: _ => done(n)
                        case "locked" :: _ => accountSays("Too many wrong codes for " ~ name.hlb ~ ". Try again in an hour.", back)
                        case "wrong" :: _ => accountSays("That isn't the code for " ~ name.hlb ~ ". Check the name and the code: the code is case-sensitive.", back, |(() => askCode(name)))
                        case _ => accountSays(name.hlb ~ " has no code of its own yet, and that isn't a player code for " ~ name.hlb ~ ". On a device signed in as " ~ name.hlb ~ ", open " ~ "Play Online".hl ~ " and tap " ~ "Sign in on another device".hl ~ ": it shows the code to enter here.", back, |(() => askCode(name)))
                    }
                } { unreachable() }

                if (code.isEmpty)
                    askCode(name)
                else
                // a player code is 24 small letters: its names, if this is one (a code picked may look the same, so then it's tried as one)
                if (code.length >= 16 && code.forall(c => c >= 'a' && c <= 'z'))
                    postF(server + "/player-code", code) { r =>
                        r.split('\n').$.%(_.nonEmpty).find(_.toLowerCase == name.toLowerCase) match {
                            case Some(n) => PlayerKey.set(code) ; done(n)
                            case None => own()
                        }
                    } { unreachable() }
                else
                    own()
            }) :: BasicOption(" ", "Cancel", Nil, back))
        }

        name0 match {
            case Some(n) => askCode(n)
            case None =>
                ui.action.asker.iask(InputOption(b + "Sign in</span>\n" + d + "the name you play under\non your other device</span>\n", "", Nil, v => {
                    val n = v.trim
                    if (n.isEmpty) back() else askCode(n)
                }) :: BasicOption(" ", "Cancel", Nil, back))
        }
    }

    // the names this device holds, then whether each has a code of its own
    def heldNames(then : $[String] => Unit) = postF(HRF.server.get + "/player-code", PlayerKey.get) { r => then(r.split('\n').$.%(_.nonEmpty)) } { then($) }

    def ownCodes(names : $[String], then : $[String] => Unit) {
        def check(rest : $[String], own : $[String]) : Unit = rest match {
            case n :: more => postF(HRF.server.get + "/has-code", n) { r => check(more, (r.trim == "yes").?(own :+ n).|(own)) } { check(more, own) }
            case Nil => then(own)
        }
        check(names, $)
    }

    // a code of the player's own for the names this device holds (Ranking.setCode)
    def setOwnCode(names : $[String], done : () => Unit, back : () => Unit) {
        ui.action.asker.iask(InputOption(accountBright + "Pick a code of your own for " + names.join(", ") + "</span>\n" + accountDim + "8 to 64 characters;\nit replaces one set before</span>\n", "", Nil, v => {
            val code = v.trim
            if (code.length < 8 || code.length > 64)
                accountSays("A code needs 8 to 64 characters.".txt, back, |(() => setOwnCode(names, done, back)))
            else
                postF(HRF.server.get + "/set-code", PlayerKey.get + "\n" + code) { r =>
                    if (r.startsWith("ok")) done() else accountSays("The code wasn't set: this device holds no name.".txt, back)
                } { accountSays("The server can't be reached.".txt, back) }
        }) :: BasicOption(" ", "Cancel", Nil, back))
    }

    // on the device signed in: what to enter on the other one
    def otherDevice(names : $[String], back : () => Unit) {
        val again = () => otherDevice(names, back)
        ownCodes(names, own => ui.action.asker.zask(
            $(ZOption("Sign in on another device".hl, Div(("On the other device, open " ~ "Play Online".hl ~ ", tap " ~ "Sign in".hl ~ ", type " ~ names./(_.hlb).join(" or ") ~ ", then one of these codes. Keep them to yourself: anyone with them can play as you.").spn, ZBasic.info))) ++
            $(ZOption("This device's player code".hl, Div(PlayerKey.get.hlb.spn(xstyles.larger110), ZBasic.info))) ++
            $(ZOption("Your own code".hl, Div((own.any.?("You set a code of your own for " ~ own./(_.hlb).join(", ") ~ ". It's kept scrambled, so it can't be shown, but it works. Forgot it? Pick a new one below.").|("Easier to type than the player code: pick one below.".txt)).spn, ZBasic.info))) ++
            $(ZBasic(" ", Div(own.any.?("Change my own code").|("Pick my own code")), () => setOwnCode(names, () => accountSays("Your code is set for " ~ names./(_.hlb).join(", ") ~ ". On the other device, sign in with your name and this code.", again), again))) ++
            $(ZBasic(" ", Div("Back"), back))))
    }

    // sign out (a new key), then, for Switch account, sign in as another name; a name with no code of its own is warned of first,
    // as this device couldn't sign back in to it without one
    def signOut(names : $[String], switch : Boolean, back : () => Unit) {
        def go() {
            PlayerKey.fresh()
            if (switch) signInAs(None, _ => onlineGame(), () => onlineGame()) else onlineGame()
        }

        ownCodes(names, own => {
            val none = names.%(n => own.contains(n).not)
            if (none.none)
                go()
            else
                ui.action.asker.zask(
                    $(ZOption("Account".hl, Div((none./(_.hlb).join(", ") ~ " has no code of its own. Once you sign out, this device can't sign back in as " ~ none./(_.hlb).join(" or ") ~ " without one, and if no other device is signed in, the name is lost. Pick a code first.").spn, ZBasic.info))) ++
                    $(ZBasic(" ", Div("Pick a code, then sign out"), () => setOwnCode(names, () => go(), back))) ++
                    $(ZBasic(" ", Div("Sign out anyway"), () => go())) ++
                    $(ZBasic(" ", Div("Cancel"), back)))
        })
    }

    // tour: the player's tournament games (good-game/Tournament.scala), shown at the top once the server has said which
    // mine: the player's own games from the server (good-game "my-games"), shown while their names are claimed on this device;
    // the completed ones in a folder, open when doneOpen (2026-11-20)
    // held: the names this device holds (Account; 2026-11-20); fetch: ask the server for all three (once: the answers re-render)
    def onlineGame(tour : |[$[$[String]]] = None, mine : |[$[$[String]]] = None, doneOpen : Boolean = false, held : |[$[String]] = None, fetch : Boolean = true) {
        val prefix = meta.settingsKey + ".online.game."
        val l = Local.list(prefix)./(_.drop(prefix.length).split('.'))./(l => l(0) -> l(1).toDouble).sortBy(-_._2)
        val games = l./{ case (lobby, time) => HRF.parseOnlineGame(lobby, time, Local.get(prefix + lobby + "." + time, "")) }

        // (each answer re-renders with the answers so far: one re-render used to drop the other's, and ask again)
        if (fetch && HRF.server.any && HRF.offline.not) {
            val at = dom.document.location.pathname
            var t = tour
            var m = mine
            var h = held
            def again() = if (dom.document.location.pathname == at) onlineGame(t, m, doneOpen, h, false)

            if (meta.tournament.any)
                postF(HRF.server.get + "/tournament/mine", PlayerKey.get) { r =>
                    val l = r.split('\n').$.%(_.nonEmpty)./(_.split('\t').$).%(_.num >= 6)
                    if (l.any) { t = |(l) ; again() }
                } { }

            postF(HRF.server.get + "/my-games", PlayerKey.get) { r =>
                val l = r.split('\n').$.%(_.nonEmpty)./(_.split('\t').$).%(_.num >= 5)
                if (l.any) { m = |(l) ; again() }
            } { }

            heldNames { n => h = |(n) ; again() }

            // Discord LFG games (good-game/Lfg.scala): the options this version of the site gives them, once a page load
            if (HRF.lfgSent.not) {
                HRF.lfgSent = true
                HRF.metas.%(_.lfgOptions.any).foreach { m =>
                    postF(HRF.server.get + "/lfg/template", m.name + "\n" + HRF.version + "\n" + m.lfgOptions./(o => m.writeOption(o)).join(" "))(_ => ())(())
                }
            }
        }

        // the game waits on the player (the server's sixth field, good-game/Turns.scala; 2026-11-20)
        def myTurn(r : $[String]) = r.lift(5).has("1")

        // a game of the player's own: its title, the game when it isn't this one, the name played under; it opens the seat;
        // highlighted in gold, with "Your turn", while it waits on the player
        def myGame(group : Elem, r : $[String]) = {
            val label = HRF.metas.find(_.name == r(0))./(_.label).|(r(0))
            val other = (r(0) != meta.name).??(label + " · ")
            val turn = myTurn(r)
            val sub = turn.?(Span(Text("Your turn"), $(ExternalStyle("hrf-my-turn-tag"))) ~ " · ".txt).|(Empty) ~ (other + "as " + r(3)).txt
            ZOption(group, (Link(HRF.server.get + "/play/" + r(0) + "/" + r(2), r(1).but("")./(_.hh).|(label.hh) ~ Break ~ sub.spn(xstyles.smaller75), $(xstyles.link, xlo.fullwidth, xlo.fullheight)) ~ Image("external-link")(xstyles.explain)(xstyles.clickThrough)).div(ZBasic.choice ++ turn.$(ExternalStyle("hrf-my-turn"))))
        }

        val tourSecrets = tour.|($)./(_(3)).toSet
        val myGames = mine.|($).%(r => tourSecrets.contains(r(2)).not)
        // the games waiting on the player first, each part in the server's order (newest first)
        val active = myGames.%(_(4) != "1").sortBy(r => myTurn(r).?(0).|(1))
        val done = myGames.%(_(4) == "1")
        val names = myGames./(_(3)).distinct

        val myGameRows =
            active./(r => myGame(("Your Active Games" + (names.num == 1).??(" · " + names.head)).hl, r)) ++
            (myGames.any && active.none).$(ZOption("Your Active Games".hl, Div("None right now.".txt, ZBasic.info))) ++
            done.any.$(ZBasic(" ", ((doneOpen.?("▾ ").|("▸ ") + "Completed Games (" + done.num + ")").hh).div($(xlo.fullwidth, xlo.fullheight)), () => onlineGame(tour, mine, doneOpen.not, held, false))) ++
            doneOpen.??(done./(r => myGame(" ".spn, r)))

        val tournamentGames = tour.|($)./(r => ZOption("Your Tournament Game".hl, (Link(HRF.server.get + "/play/" + meta.tournament.get + "/" + r(3), r(2).hh ~ Break ~ ("as " + r(4) + " against " + r(5)).spn(xstyles.smaller75), $(xstyles.link, xlo.fullwidth, xlo.fullheight)) ~ Image("external-link")(xstyles.explain)(xstyles.clickThrough)).div(ZBasic.choice)))

        def accountMenu(n : $[String]) {
            val back = () => onlineGame()
            ui.action.asker.zask(
                $(ZOption("Account".hl, Div(("Signed in as " ~ n./(_.hlb).join(", ")).spn, ZBasic.info))) ++
                $(ZBasic(" ", Div("Sign in on another device"), () => otherDevice(n, () => accountMenu(n)))) ++
                $(ZBasic(" ", Div("Switch account"), () => signOut(n, true, () => accountMenu(n)))) ++
                $(ZBasic(" ", Div("Sign out"), () => signOut(n, false, () => accountMenu(n)))) ++
                $(ZBasic(" ", Div("Back"), back)))
        }

        // signed out: Sign in; signed in: the names, opening the Account screen
        val accountRows : $[ZOption] = held match {
            case Some(Nil) => $(ZBasic("Account".hl, ("Sign in".hh ~ Break ~ "Played on another device? Sign in to play as the same name here.".spn(xstyles.smaller75)).div($(xlo.fullwidth, xlo.fullheight)), () => signInAs(None, _ => onlineGame(), () => onlineGame())))
            case Some(n) => $(ZBasic("Account".hl, (("Signed in as " + n.join(", ")).hh ~ Break ~ "Sign in on another device, switch account or sign out".spn(xstyles.smaller75)).div($(xlo.fullwidth, xlo.fullheight)), () => accountMenu(n)))
            case None => $
        }

        def goOnlineNew() {
            history.pushState("/play/" + meta.name + "/online/new", () => onlineGame())
            customGame(true)
        }

        def goOnlineArchive() {
            history.pushState("/play/" + meta.name + "/online/archive", () => onlineGame())
            onlineGameArchive()
        }

        def goOnlineLobby(lobby : String) {
            history.pushState("/play/" + meta.name + "/online/lobby/" + lobby, () => onlineGame())
            val g = games.%(_.lobby == lobby)
            if (g.any)
                showLinksMenu(g.shuffle(0))
            else {
                HRF.segments = $

                ui.logger.alog("Lobby info not found locally".spn.div(xstyles.error))
            }
        }

        ui.action.asker.zask(
            meta.recommendedVersion.but("")./(v => ZBasic("This version is currently under development or testing.".styled(xstyles.error) ~ Break ~ "If you are not feeling adventurous, please use the following version instead:".styled(xstyles.warning), v.styled(xstyles.link).styled(xstyles.larger125), () => {
                val l = dom.document.location
                val search = ("version=" + v) +: l.search.drop(1).split('&').$.%(_.startsWith("version").not).but("")
                val url = l.origin + l.pathname + "?" + search.join("&") + l.hash
                ui.logger.alog(" Reload " + url)
                dom.document.location.assign(url)
            })).$ ++
            accountRows ++
            tournamentGames ++
            myGameRows ++
            ZBasic(meta.label.hl, "New Online Game".hl.styled(xstyles.larger110), () => {
                goOnlineNew()
            }).?.$ ++
            games.%(_.status.has("archived").not)./(g => ZBasic("Recent Games", (new scalajs.js.Date(g.time).asInstanceOf[Dynamic].toLocaleString("sv-SE").toString.spn(xstyles.smaller75) ~ Break ~ g.title.hh).div($(xlo.fullwidth, xlo.fullheight)), () => {
                goOnlineLobby(g.lobby)
            })) ++
            ZBasic("  ", "Archived Games".hh, () => {
                goOnlineArchive()
            }).?.$.%(_ => games.exists(_.status.has("archived"))) ++
            ZBasic(" ", "Back", () => {
                // metaMenu()
                history.popState()
            }).?
        )

        if (HRF.segments.startsWith($("online", "new")))
            goOnlineNew()
        else
        if (HRF.segments.startsWith($("online", "archive")))
            goOnlineArchive()
        else
        if (HRF.segments.startsWith($("online", "lobby")) && HRF.segments.num >= 3)
            goOnlineLobby(HRF.segments(2))
        else
            HRF.segments = $

        HRF.segments = $
    }

    def onlineGameArchive() {
        val prefix = meta.settingsKey + ".online.game."
        val l = Local.list(prefix)./(_.drop(prefix.length).split('.'))./(l => l(0) -> l(1).toDouble).sortBy(-_._2)
        val games = l./{ case (lobby, time) => HRF.parseOnlineGame(lobby, time, Local.get(prefix + lobby + "." + time, "")) }

        ui.action.asker.zask(
            games.%(_.status.has("archived"))./(g => ZBasic("Archived Games", (new scalajs.js.Date(g.time).asInstanceOf[Dynamic].toLocaleString("sv-SE").toString.spn(xstyles.smaller75) ~ Break ~ g.title.hh).div($(xlo.fullwidth, xlo.fullheight)), () => {
                showLinksMenu(g)
            })) ++
            ZBasic(" ", "Back", () => {
                // onlineGame()
                history.popState()
            }).?
        )
    }

    // Local games (solo, quick and hotseat) are saved in the browser: the newest ones of each kind are kept,
    // the rest deleted when a new one starts
    val localGamesKept = 12

    def localGameKey(kind : String, time : Double) = meta.settingsKey + "." + kind + ".game." + time.toLong
    def localMovesKey(kind : String, time : Double) = meta.settingsKey + "." + kind + ".moves." + time.toLong

    def localGames(kind : String) : $[LocalGame] = {
        val prefix = meta.settingsKey + "." + kind + ".game."
        Local.list(prefix)./~(k => k.drop(prefix.length).toLongOption./(t => HRF.parseLocalGame(kind, t.toDouble, Local.get(k, "")))).sortBy(-_.time)
    }

    def deleteLocalGame(lg : LocalGame) {
        Local.remove(localGameKey(lg.kind, lg.time))
        Local.remove(localMovesKey(lg.kind, lg.time))
    }

    def localGameMoves(lg : LocalGame) : Int = Local.get(localMovesKey(lg.kind, lg.time), "").split('\n').$.but("").num

    def localJournal(kind : String, time : Double) = new LocalStorageJournal[meta.gaming.ExternalAction](meta, localMovesKey(kind, time), s => meta.parseActionExternal(s), a => meta.writeActionExternal(a), kind)

    // Saves a new local game's setup and returns the journal its moves go to;
    // when the browser's storage is full, the oldest saved games of any kind make room
    def newLocalGame(kind : String, seating : $[meta.F], difficulties : Map[meta.F, Difficulty], options : $[meta.O], title : String) : Journal[meta.gaming.ExternalAction] = {
        localGames(kind).drop(localGamesKept - 1).foreach(deleteLocalGame)

        val time = HRF.now().getTime()
        val bots = seating./~(f => difficulties(f) match {
            case Bot(name) => |(BotAssigned(meta.writeFaction(f), name))
            case _ => None
        })
        val lg = LocalGame(kind, time, HRF.version, title, seating./(meta.writeFaction), bots, options./(meta.writeOption))

        def save(tries : Int) {
            try {
                Local.set(localGameKey(kind, time), HRF.writeLocalGame(lg))
                Local.set(localMovesKey(kind, time), "")
            }
            catch {
                case e : Throwable =>
                    val oldest = $("solo", "quick", "hotseat", "tutorial")./~(localGames).%(_.time != time).sortBy(_.time)
                    if (tries > 0 && oldest.any) {
                        deleteLocalGame(oldest.head)
                        save(tries - 1)
                    }
                    else
                        println("local game not saved: " + e)
            }
        }

        save(6)

        localJournal(kind, time)
    }

    def continueLocalGame(lg : LocalGame) {
        HRF.segments = $

        val seating = lg.seating./~(meta.parseFaction)

        if (seating.num != lg.seating.num || seating.none) {
            ui.logger.alog("Saved game not readable".spn.div(xstyles.error))
        }
        else {
            val difficulties = seating./(f => f -> lg.bots.%(_.faction == meta.writeFaction(f)).single./(b => Bot(b.bot) : Difficulty).|(Human)).toMap
            val options = lg.options./~(meta.parseOption)

            startGame(seating, difficulties, options, $, localJournal(lg.kind, lg.time), lg.title, () => Map(), NoSwitches)
        }
    }

    // The menu of a kind of local game, like the online one: start a new game or continue a saved one
    def savedGamesMenu(kind : String, newLabel : String, startNew : () => Unit) {
        val games = localGames(kind)

        def goNew() {
            history.pushState("/play/" + meta.name + "/" + kind + "/new", () => savedGamesMenu(kind, newLabel, startNew))
            startNew()
        }

        var deleting : |[Double] = None

        def ask() {
            ui.action.asker.zask(
                ZBasic(meta.label.hl, newLabel.hl.styled(xstyles.larger110), () => {
                    goNew()
                }).?.$ ++
                localGames(kind)./(g => ZOption("Continue", OnClick(Div(
                    (new scalajs.js.Date(g.time).asInstanceOf[Dynamic].toLocaleString("sv-SE").toString.spn(xstyles.smaller75) ~ Break ~
                    g.title.hh ~ Break ~
                    g.seating./~(meta.parseFaction)./(f => meta.factionElem(f)).reduceLeftOption((a, b) => a ~ " vs " ~ b).|(Empty) ~ " " ~ ("(" + localGameMoves(g) + " moves)").spn(xstyles.smaller75) ~ " " ~
                    Parameter("delete", OnClick(Span(deleting.has(g.time).?("Confirm Delete".styled(xstyles.error)).|("Delete".txt), xstyles.outlined)))).div($(xlo.fullwidth, xlo.fullheight)), ZBasic.choice)), {
                        case "delete" =>
                            if (deleting.has(g.time)) {
                                deleteLocalGame(g)
                                deleting = None
                            }
                            else
                                deleting = |(g.time)
                            setTimeout(0) { ask() }
                        case _ =>
                            continueLocalGame(g)
                    })) ++
                ZBasic(" ", "Back", () => {
                    history.popState()
                }).?
            )
        }

        if (HRF.segments.startsWith($(kind, "new")) || games.none)
            if (games.none) {
                HRF.segments = $
                startNew()
            }
            else
                goNew()
        else {
            HRF.segments = $
            ask()
        }
    }

    // A faction in the picker: a tile in a grid when the game has them (Meta.factionTile), else a row with its note;
    // either way with the factionInfo button
    def factionPick(f : meta.F) : Elem = pickElem(meta.factionTile(f), meta.factionElem(f), meta.factionNote(f), meta.factionInfo(f))

    // A faction's variant in the picker (Meta.factionVariants), shown like the factions
    def variantPick(v : meta.FactionVariant) : Elem = pickElem(meta.factions.exists(meta.factionTile(_).any).?(v.tile), v.tile, Empty, v.info)

    def pickElem(tile : |[Elem], elem : Elem, note : Elem, about : |[(Elem, Elem, $[Elem])]) : Elem = {
        def info(styles : Style*) = about./{ case (label, _, _) => Parameter("faction-info", OnClick(Span(label, (xstyles.outlined +: styles).toList))) }

        tile match {
            case Some(tile) => Div(Div(tile) ~ info(xstyles.tileButton)./(i => Div(i)).|(Empty), ZBasic.tile)
            case None => Div(elem ~ info(xstyles.infoButton)./(" " ~ _).|(Empty) ~ note, ZBasic.choice)
        }
    }

    // The picker's entries in their groups (Meta.factionGroup, FactionVariant.group): those without one first, as a group of
    // entries is headed only where it starts. A group in Meta.foldedFactionGroups folds under its heading (Folds): folded, it is
    // one line saying how many it holds; tapping the heading or that line opens it, and again (the heading) closes it
    def grouped(l : $[(|[Elem], Elem => ZOption)], reask : () => Unit) : $[ZOption] = {
        val ordered = l.filter(_._1.none) ++ l.filter(_._1.any)
        def key(g : Elem) = "picker." + meta.name + "." + g.text
        def foldable(g : |[Elem]) = g.exists(g => meta.foldedFactionGroups.has(g.text))
        def toggle(g : Elem) = (_ : Any) => { Folds.toggle(key(g)) ; reask() }

        ordered.zipWithIndex./~ { case ((g, z), i) =>
            if (foldable(g)) {
                val h = g.get
                val open = Folds.isOpen(key(h))
                val first = ordered.indexWhere(_._1 == g) == i
                val heading = Folds.heading(h, open)
                if (open)
                    $(z(heading).copy(groupClick = toggle(h)))
                else if (first)
                    $(ZOption(heading, OnClick(Div(ordered.count(_._1 == g).hl ~ " " ~ h, ZBasic.choice)), toggle(h), groupClick = toggle(h)))
                else
                    $
            }
            else
                $(z(g.|("Play as".txt)))
        }
    }

    def randomPick(label : Elem) : Elem = Div(label, meta.factions.exists(meta.factionTile(_).any).?(ZBasic.tile).|(ZBasic.choice))

    // A local game against the solo opponent: pick a faction, then the usual setup screen
    def soloGame() {
        HRF.segments = $

        val solo = meta.soloFaction.get
        val t = "Play " ~ meta.label.hl ~ " " ~ "Solo".hl

        // Each faction with its info button (Northgard's Info), as in the custom game's picker
        def ask() {
            ui.action.asker.zask(
                $(ZOption(t, Div("Against the " ~ meta.factionName(solo).hl, ZBasic.info))) ++
                meta.randomFactions.map { case (label, pool) => ZOption(Div("Play as".txt), OnClick(randomPick(label)), _ => {
                    val f = pool.but(solo).shuffle.head
                    history.pushState(null, () => ask())
                    startSetup($(f, solo), false, |("solo"), hidden = $(f), picked = true)
                }) } ++
                grouped(meta.factions.but(solo)./(f => meta.factionGroup(f) -> ((g : Elem) => ZOption(Div(g), OnClick(factionPick(f)), {
                    case "faction-info" =>
                        meta.factionInfo(f).foreach { case (_, title, l) => showOverlayRaw(title, l) }
                        setTimeout(0) { ask() }
                    case _ =>
                        history.pushState(null, () => ask())
                        startSetup($(f, solo), false, |("solo"), picked = true)
                }))) ++
                meta.factionVariants.%(_.faction != solo)./(v => v.group -> ((g : Elem) => ZOption(Div(g), OnClick(variantPick(v)), {
                    case "faction-info" =>
                        v.info.foreach { case (_, title, l) => showOverlayRaw(title, l) }
                        setTimeout(0) { ask() }
                    case _ =>
                        history.pushState(null, () => ask())
                        startSetup($(v.faction, solo), false, |("solo"), extra = v.options, picked = true, names = Map(v.faction -> v.name))
                }))), () => ask()) ++
                ZBasic(" ", "Cancel", () => {
                    history.popState()
                }).?
            )
        }

        ask()
    }

    def customGame(online : Boolean) {
        HRF.segments = $

        val ff = meta.factions

        if (meta.gradualFactions) {
            def askCount() : Unit = ui.action.asker.zask(
                ff.drop(meta.minPlayers - 1)./(f => ZOption(meta.label.hl, OnClick(Div((ff.indexOf(f) + 1).hlb(xstyles.larger125) ~ " Players", ZBasic.choice)), _ => {
                    val n = ff.indexOf(f) + 1
                    val fff =
                        if (meta.indistinguishableFactions)
                            ff.take(n).shuffle
                        else
                            ff.combinations(n)
                                .flatMap(_.permutations)
                                .filter(c => meta.validateFactionCombination(c).is[ErrorResult].not)
                                .$.shuffle
                                .sortBy(c => meta.validateFactionCombination(c).is[WarningResult])
                                .first

                    history.pushState(null, () => askCount())
                    startSetup(fff, online, online.not.?("hotseat"), picked = true)
                })) ++
                ZBasic(" " ~ HorizontalBreak, "                                              Cancel                                              ", () => {
                    // metaMenu()
                    history.popState()
                }).?
            )

            askCount()
        }
        else {
            var opponents = $[meta.F]()
            // Random picks: the faction drawn, kept hidden until the game starts, and the pool it was drawn from
            var hidden = Map[meta.F, $[meta.F]]()

            // Factions picked as one of their variants (Meta.factionVariants)
            var variants = Map[meta.F, meta.FactionVariant]()

            def chosenElem(f : meta.F) = hidden.contains(f).?(meta.randomName.hlb).|(variants.get(f)./(_.chosen).|(meta.factionChosenElem(f)))

            // A random pick that drew this faction draws again, so picks never give away what was drawn;
            // with nothing left to draw, the random pick becomes this pick
            def pick(f : meta.F) {
                variants -= f
                hidden.get(f) match {
                    case Some(pool) =>
                        hidden -= f
                        pool.diff(opponents).shuffle.take(1).foreach { g =>
                            opponents = opponents./(o => (o == f).?(g).|(o))
                            hidden += g -> pool
                        }
                        if (opponents.has(f).not)
                            opponents :+= f
                    case None =>
                        opponents :+= f
                }
            }

            // The choice above the picker (Meta.pickerModes, Northgard's free-for-all or teams), remembered for the next game
            val modeKey = meta.name + ".picker-mode"
            var mode = meta.pickerModes.indices.find(_.toString == hrf.web.Local.get(modeKey, "")).|(0)

            def modeOptions = meta.pickerModes.lift(mode)./(_._2).|($)

            def askAdd() {
                val t = "Play " ~ meta.label.hl ~ " " ~ (online).?("Online").|("Hotseat")
                val v = meta.validateFactionCombination(opponents)

                ui.action.asker.zask(
                    $(ZOption(t, Empty)) ++
                    opponents./(f => ZBasic(t, chosenElem(f), () => {
                        opponents :-= f
                        hidden -= f
                        variants -= f
                        askAdd()
                    }, ZBasic.infoch ++ $(xstyles.optionOn))) ++
                    ZBasic(t, v.ok.?("Start Setup".hl ~ v.message.any.??(" | ")).|(Empty) ~ v.message.styled(v.style), (v.ok).??(() => {
                        // The setup screen's Back comes back here with the picks kept
                        history.pushState(null, () => askAdd())
                        startSetup(opponents, online, online.not.?("hotseat"), hidden = hidden.keys.$, extra = modeOptions ++ opponents./~(variants.get(_)./~(_.options)), picked = true, names = variants.view.mapValues(_.name).toMap)
                    })).? ++
                    meta.pickerModes.zipWithIndex./{ case ((label, _), i) =>
                        ZOption("Game type".styled(xstyles.larger125), OnClick(Div(label, ZBasic.choice ++ (i == mode).$(xstyles.optionOn, xstyles.bold) ++ $(xstyles.optionE, xstyles.halfbutton))), _ => {
                            mode = i
                            hrf.web.Local.set(modeKey, i.toString)
                            askAdd()
                        })
                    } ++
                    meta.randomFactions.filter { case (_, pool) => pool.diff(opponents).any }.map { case (label, pool) => ZOption(Div("Play as".txt), OnClick(randomPick(label)), _ => {
                        val f = pool.diff(opponents).shuffle.head
                        opponents :+= f
                        hidden += f -> pool
                        askAdd()
                    }) } ++
                    grouped(ff.diff(opponents.diff(hidden.keys.$))./(f => meta.factionGroup(f) -> ((g : Elem) => ZOption(Div(g), OnClick(factionPick(f)), {
                        case "faction-info" =>
                            meta.factionInfo(f).foreach { case (_, title, l) => showOverlayRaw(title, l) }
                            setTimeout(0) { askAdd() }
                        case _ =>
                            pick(f)
                            askAdd()
                    }))) ++
                    // A variant is offered while its faction isn't picked (or only drawn at random)
                    meta.factionVariants.%(v => opponents.diff(hidden.keys.$).has(v.faction).not)./(v => v.group -> ((g : Elem) => ZOption(Div(g), OnClick(variantPick(v)), {
                        case "faction-info" =>
                            v.info.foreach { case (_, title, l) => showOverlayRaw(title, l) }
                            setTimeout(0) { askAdd() }
                        case _ =>
                            pick(v.faction)
                            variants += v.faction -> v
                            askAdd()
                    }))), () => askAdd()) ++
                    ZBasic(" ", "Cancel", () => {
                        // metaMenu()
                        history.popState()
                    }).?
                )
            }

            askAdd()
        }
    }

    // mode: a mode from the main menu (Meta.modes), whose options are always on; its choices are saved apart from the usual ones
    // keep: the kind of local game to save it as (savedGamesMenu), if it is saved
    // extra: options picked before the setup screen (Meta.pickerModes), always on like the mode's
    // picked: the factions were picked on the screen before, which the first page's Back returns to (the caller pushed it)
    def startSetup(factions : $[meta.F], online : Boolean, keep : |[String] = None, mode : |[String] = None, hidden : $[meta.F] = $, extra : $[meta.O] = $, picked : Boolean = false, names : Map[meta.F, String] = Map()) {
        val optionsSaveKey = meta.name + mode./("." + _).|("") + "." + online.?("online").|("offline") + ".options." + factions.num + "p"
        val mandatory = meta.mandatoryFor(factions.num, factions) ++ mode./~(meta.modeOptions) ++ extra
        val saved = hrf.web.Local.get(optionsSaveKey, "").split(' ').$./~(_.some)
        val provided = HRF.paramList("options")
        val preset = provided.some.|(saved)./~(meta.parseOption(_)).intersect(meta.options)
        val matching = meta.optionsFor(factions.num, factions)
        val unneeded = preset.diff(matching)
        val needed = preset.intersect(matching)

        val defaults = meta.defaultsFor(factions.num, factions)
        var options = OptionsState(meta.optionsFor(factions.num, factions), mandatory, $, needed.some./(o => meta.completeSaved(factions.num, factions, o ++ defaults.%(_.toggle.not))).|(defaults)).checkDimmed()

        val pages = {
            val pages = meta.optionPages(factions.num, factions)./(_.intersect(options.all))

            pages.take(1) ++ pages.drop(1).%(_.any)
        }

        val presets = meta.presetsFor(factions.num, factions)./{ case (title, included, excluded) =>
            val options = OptionsState(meta.optionsFor(factions.num, factions), mandatory, included ++ meta.defaultsFor(factions.num, factions).diff(excluded), $).checkDimmed()
            (title, included, excluded, options)
        }

        var seating = factions
        var difficulties : Map[meta.F, Difficulty] = factions.map(f => f -> meta.botOnly(f).?(Bot(meta.defaultBot(f)) : Difficulty).|(Human)).toMap

        var notes = Map[meta.F, String]()

        def setupQuestions(page : Int) {
            val v = meta.validateFactionSeatingOptions(seating, options.actual)
            // Factions drawn at random are shown only as random until the game starts
            def shownName(f : meta.F) = hidden.has(f).?(meta.randomName).|(names.get(f).|(meta.factionName(f)))
            val maxname = factions./(shownName(_).length).max

            def padding(f : meta.F) = "".padTo(maxname - shownName(f).length, ' ').pre.spn(xstyles.compressed)

            val vm = v.message.styled(v.style)

            def foldOpen(s : String) = Folds.isOpen(s) || meta.optionFoldOpen(s, seating, options.actual)

            // Back (or Cancel) and Start (or Next), at the top of the page and again at the bottom
            def buttons =
                (page == 0).$(
                    ZOption(vm, picked.?("Back").|("Cancel").txt.div(ZBasic.choice)(xstyles.halfbutton).onClick, _ => {
                        ui.action.scroll.scrollTop = 0
                        // metaMenu()
                        history.popState()
                    })
                ) ++
                (page > 0).$(
                    ZOption(vm, "Back".txt.div(ZBasic.choice)(xstyles.halfbutton).onClick, _ => {
                        ui.action.scroll.scrollTop = 0
                        // setupQuestions(page - 1)
                        history.popState()
                    })
                ) ++
                v.ok.??(
                    (page == pages.length - 1).$(
                        ZOption(vm, "Start".hlb.div(ZBasic.choice)(xstyles.halfbutton).onClick, _ => {
                            history.destroy(pages.length - 1 + picked.??(1))

                            val started = meta.startOptions(options.actual)

                        if (online)
                            arrangeOnlineGame(seating, difficulties, started, Map(), notes, false, meta.randomGameName(), HRF.now(), $(meta.start), None, g => {})
                        else {
                            val title = meta.randomGameName()
                            val journal = keep./(newLocalGame(_, seating, difficulties, started, title)).|(new MemoryJournal[meta.gaming.ExternalAction](meta))

                            startGame(seating, difficulties, started, $, journal, title, () => Map(), NoSwitches)
                        }
                    })
                ) ++
                (page < pages.length - 1).$(
                    ZOption(vm, "Next".hh.div(ZBasic.choice)(xstyles.halfbutton).onClick, _ => {
                        ui.action.scroll.scrollTop = 0
                        history.pushState(null, () => setupQuestions(page))
                        setupQuestions(page + 1)
                    })
                )
            ) ++
            v.ok.not.??(
                (page == pages.length - 1).$(
                    ZOption(vm, "Start".hlb.div(ZBasic.info)(xstyles.halfbutton))
                ) ++
                (page < pages.length - 1).$(
                    ZOption(vm, "Next".hh.div(ZBasic.info)(xstyles.halfbutton))
                )
            )

            ui.action.asker.zask(
                buttons ++
                (page == 0).?? {
                    seating./(f => ZOption("Setup Factions".styled(xstyles.larger125), Div(Empty ~ online.?(Input(notes.get(f).|(""), "Player #" + (seating.indexOf(f) + 1), s => notes += f -> s.sanitize(32), 9, 16, ZBasic.inputT, ZBasic.inputD)) ~ " " ~ (padding(f) ~ hidden.has(f).?(meta.randomName.txt).|(names.get(f)./(_.txt).|(meta.factionElem(f))) ~ padding(f)).div(xstyles.width18ch) ~ " " ~
                        Parameter("difficulty", OnClick(Span((difficulties(f) match {
                            case Human  => " Human ".pre.hl
                            case Bot(s) => " Bot / ".pre ~ (s + " ").pre.hl
                        }).div(xstyles.width14ex), xstyles.outlined))) ~
                        meta.factionRowOptions(f, seating).some./(l => " " ~ Parameter("row-option", OnClick(Span(l.find(options.selected.has)./(o => o.valueOn).|(meta.factionRowNone).div(xstyles.width14ex), xstyles.outlined)))).|(Empty) ~
                        meta.factionRowMore(f, seating, options.actual).zipWithIndex./{ case (l, i) => " " ~ Parameter("row-more-" + i, OnClick(Span(l.find(options.selected.has)./(o => o.valueOn).|(meta.factionRowNone).div(xstyles.width10ch), xstyles.outlined))) }.merge ~
                        Div(
                            (seating.head == f).?("   ".pre.spn(xstyles.larger125)).|(OnClick("up",   Span(" ▲ ".pre.spn(xstyles.larger125), xstyles.outlined))) ~
                            (seating.last == f).?("   ".pre.spn(xstyles.larger125)).|(OnClick("down", Span(" ▼ ".pre.spn(xstyles.larger125), xstyles.outlined))),
                        xstyles.updown)
                        , ZBasic.choice :+ xstyles.player), {
                        case "difficulty" =>
                            if (meta.botOnly(f).not)
                                difficulties += f -> ($(Human) ++ meta.getBots(f)./(Bot) ++ $(Human)).dropWhile(_ != difficulties(f)).drop(1).head
                            setupQuestions(page)
                        case "row-option" =>
                            meta.factionRowClick(f, seating, options.selected).foreach { o =>
                                if (options.selected.has(o).not)
                                    options = options.click(o)
                            }
                            hrf.web.Local.set(optionsSaveKey, (options.selected ++ options.dimmed ++ unneeded)./(meta.writeOption).join(" "))
                            setupQuestions(page)
                        case s : String if s.startsWith("row-more-") =>
                            meta.factionRowMore(f, seating, options.actual).lift(s.drop("row-more-".length).toInt).foreach { l =>
                                (l ++ l).dropWhile(o => options.selected.has(o).not).drop(1).take(1).some.|(l.take(1)).foreach { o =>
                                    if (options.selected.has(o).not)
                                        options = options.click(o)
                                }
                            }
                            hrf.web.Local.set(optionsSaveKey, (options.selected ++ options.dimmed ++ unneeded)./(meta.writeOption).join(" "))
                            setupQuestions(page)
                        case "up" =>
                            val a = seating.takeWhile(_ != f)
                            val b = seating.dropWhile(_ != f)
                            seating = a.dropRight(1) ++ b.take(1) ++ a.takeRight(1) ++ b.drop(1)
                            setupQuestions(page)
                        case "down" =>
                            val a = seating.takeWhile(_ != f)
                            val b = seating.dropWhile(_ != f)
                            seating = a ++ b.drop(1).take(1) ++ b.take(1) ++ b.drop(2)
                            setupQuestions(page)
                    })) ++
                    presets.zipWithIndex./{ case ((title, included, excluded, neu), i) =>
                        val matches = meta.options.intersect(neu.selected) == meta.options.intersect(options.selected)
                        val info = meta.presetsInfo(seating.num, seating).lift(i).|($)
                        ZOption("Presets".styled(xstyles.larger125), OnClick(Div(title ~ info.any.?(Parameter("preset-info", OnClick(Image("question-mark")(xstyles.explain)))), ZBasic.choice ++ matches.$(xstyles.optionOn, xstyles.bold) ++ $(xstyles.optionE))), {
                            case "preset-info" =>
                                showOverlay(info./(ZBasic("Presets".txt ~ " " ~ MDash ~ " " ~ title, _)), xlo.grow4)
                                setTimeout(0) { setupQuestions(page) }
                            case _ =>
                                options = neu
                                setupQuestions(page)
                        })
                    }
                } ++
                // A foldable section shows only what is picked until its heading is tapped (Folds)
                // and a folded section with nothing picked shows its heading over "None", so it can still be opened
                {
                val shown = meta.optionsFor(seating.num, seating).diff(meta.hiddenOptions).intersect(pages(page)).%(meta.optionShown(_, options.actual))
                val empty = shown./~(_.fold).distinct.%(s => foldOpen(s).not && shown.exists(o => o.fold.has(s) && options.selected.has(o)).not)
                // A folder (BaseOption.folder) is one heading over several sections; folded, it shows only a line of what is picked in it
                def folderKey(d : String) = "folder." + d
                def folderOpen(d : String) = Folds.isOpen(folderKey(d))
                def toggleFolder(d : String) = (_ : Any) => { Folds.toggle(folderKey(d)) ; setupQuestions(page) }
                // a section's heading, smaller inside a folder, so the folder's heading stands over it
                def heading(o : meta.O) = o.folder.any.?(o.group).|(o.group.styled(xstyles.larger125))
                // a folded section with nothing picked: "None", or what is picked in a later section that shares its choice (OneOfGroup.oneOf),
                // as the Map with a pick from the Fan Maps under it
                def noneOf(o : meta.O) : Elem = o match {
                    case g : OneOfGroup => shown.drop(shown.indexOf(o) + 1).%(options.selected.has).%{ case q : OneOfGroup => q.oneOf == g.oneOf ; case _ => false }.headOption./(q => q.valueOn ~ " (" ~ q.group ~ ")").|("None".txt)
                    case _ => "None".txt
                }
                val entries : $[Either[ZOption, meta.O]] = shown./~{ o =>
                  o.folder.%(d => shown.find(_.folder.has(d)).has(o))./{ d =>
                    val open = folderOpen(d)
                    val picked = shown.%(_.folder.has(d)).%(options.selected.has)
                    Left(ZOption(Folds.heading(d.txt.styled(xstyles.larger125), open), open.?(Empty).|(OnClick(Div(picked./(_.valueOn).reduceOption((a, b) => a ~ " · " ~ b).|("None".txt), ZBasic.choice ++ $(xstyles.optionE)))),
                        toggleFolder(d), clear = false, groupClick = toggleFolder(d))) : Either[ZOption, meta.O]
                  }.$ ++
                  o.folder.forall(folderOpen).$(Right(o) : Either[ZOption, meta.O])
                }
                entries.%{
                    case Left(_) => true
                    case Right(o) => o.fold.forall(foldOpen) || options.selected.has(o) || o.fold.exists(s => empty.has(s) && shown.find(_.fold.has(s)).has(o))
                }./({
                  case Left(z) => z
                  case Right(o) =>
                  if (o.fold.exists(empty.has))
                    ZOption(Folds.heading(heading(o), false), Div(noneOf(o), ZBasic.info ++ $(xstyles.optionE)), _ => {},
                        groupClick = o.fold./(s => (_ : Any) => { Folds.toggle(s) ; setupQuestions(page) }).orNull)
                  else {
                    val state = options.selected.has(o)
                    val enabled = options.enabled(o)
                    val active = enabled
                    // A section with groupInfo has a ? on its heading; tapping the ? shows it instead of folding or unfolding the section
                    val groupInfo = meta.groupInfo(o)
                    val title = heading(o) ~ groupInfo.any.?(" ".txt ~ Parameter("group-info", OnClick(Image("question-mark")(xstyles.explain))))
                    ZOption(o.fold./(s => Folds.heading(title, foldOpen(s))).|(groupInfo.any.?(OnClick(title)).|(title)),
                        OnClick(Div(state.?(o.decorate(o.valueOn).styled(xstyles.bold)).|(o.decorate(o.valueOff))
                        ~ (o.explain.any).?(Parameter(o, OnClick(Image("question-mark")(xstyles.explain)))), (active || state).?(ZBasic.choice).|(ZBasic.info) ++ state.$(xstyles.optionOn) ++ $(xstyles.optionE))), {
                            case o : GameOption if o.explain.any =>
                                showOverlay(o.explain./(ZBasic(o.group ~ " " ~ MDash ~ " " ~ o.valueOn, _)), o.grow)
                                setTimeout(0) { setupQuestions(page) }
                            case Nil =>
                                if (active || state) {
                                    if (active) {
                                        val next = options.click(o)
                                        options = next
                                        hrf.web.Local.set(optionsSaveKey, (next.selected ++ next.dimmed ++ unneeded)./(meta.writeOption).join(" "))
                                    }
                                }
                                setupQuestions(page)
                        },
                        // the heading's own parameter comes first when the ? inside it is tapped
                        groupClick = {
                            case p if p == "group-info" || (p match { case l : Seq[_] => l.contains("group-info") ; case _ => false }) =>
                                showOverlay(groupInfo./(ZBasic(o.group, _)), xlo.grow4)
                                setTimeout(0) { setupQuestions(page) }
                            case _ =>
                                o.fold.foreach(s => if (meta.optionFoldOpen(s, seating, options.actual).not) Folds.toggle(s))
                                setupQuestions(page)
                        }
                    )
                  }
                })
                } ++
                buttons ++
                $

            )
        }

        setupQuestions(0)

        ui.action.scroll.scrollTop = 0
    }

    // One game per page: a second start (say a Start button tapped twice) would build a second game UI over the
    // first, its drag handles and all. Play Again clears the page and resets it
    var gameStarted = false

    def startGame(seatingX : $[meta.F], difficulties : Map[meta.F, Difficulty], optionsX : $[meta.O], self : $[meta.F], journal : Journal[meta.gaming.ExternalAction], title : String, names : () => Map[meta.F, String], swt : Switches, onOver : (meta.G, $[Any], Elem => Unit) => Unit = (_ : meta.G, _ : $[Any], _ : Elem => Unit) => ()) {
        if (gameStarted) {
            dom.console.warn("game already started, second start ignored")
            return
        }
        gameStarted = true

        history.nuke()

        // Bots that play by other rules (Northgard's Robotos) add their options; every client derives them the same way
        val options = (optionsX ++ meta.botOptions(seatingX, difficulties.toList.collect { case (f, Bot(d)) => f -> d ; case (f, BotDebug(d)) => f -> d }.toMap)).distinct

        val seating = seatingX.%(f => difficulties(f) != Off)

        val assets = meta.assets.%(_.condition(seating, options) || true)./~(_.get)

        val sources = assets./(a => a.name -> ("webp2/" + meta.path + "/images/" + a.copy(ext = "webp").src)).toMap
        val preload = HRF.embedded.?(assets./(a => a.name -> a.name)).|(assets.%(_.lzy == Laziness.Immediate)./(a => a.name -> sources(a.name)))
        val loader = HRF.embedded.?(new WrappedEmbeddedImageLoader(s => "asset-" + s)).|(HRF.imageCache)

        ui.logger.alog("Loading assets".spn.div)

        var i = 0

        loader.onLoad = (url, result) => {
            i += 1

            if (i % (preload.num / 23 + 1) == 0)
                ui.logger.alog("*".spn)
        }

        loader.wait(preload.rights) {
            val loaded = preload./((key, url) => key -> loader.get(url)).toMap

            val resources = Resources(ImageResources(loaded, sources, HRF.imageCache), () => names().toMap)

            import meta.gaming._

            val main = this

            val callbacks = new Callbacks {
                def switches = swt
                def saveReplay(onSave : => Unit) = {
                    val filename = "hrf--" + meta.name + "--" + HRF.version + "--replay--" + HRF.startAt.toISOString().take(16).replace("T", "--")

                    Quine.save(meta)(title, seating, options, resources, journal, filename, true, "", onSave)
                }
                def saveSmallReplay(onSave : => Unit) = {
                    val filename = "hrf--" + meta.name + "--" + HRF.version + "--small-replay--" + HRF.startAt.toISOString().take(16).replace("T", "--")

                    Quine.saveSmall(meta)(title, seating, options, resources, journal, filename, onSave)
                }
                def saveReplayOnline(replaceBots : Boolean, mergeHumans : Boolean)(onSave : String => Unit) = {
                    journal.read(0) { actions =>
                        val factions = seating.%(f => difficulties(f) != Off)

                        val ss =
                            if (replaceBots)
                                factions./(_ -> Human).toMap
                            else
                                difficulties

                        val humans = factions.%(f => ss(f) == Human)

                        arrangeOnlineGame(factions, ss, options, Map(), humans./(_ -> "Player").toMap, mergeHumans, title, HRF.startAt, actions./(_.unwrap), |("SLG"), g => {
                            val l = dom.document.location
                            val url = "/play/" + meta.name + "/online/lobby/" + g.lobby + l.search + l.hash
                            println("reload " + url)
                            dom.document.location.assign(url)
                        })
                    }
                }
                def canPlayAgain = journal.is[MemoryJournal[_]] || journal.is[LocalStorageJournal[_]]
                def playAgain() = {
                    ui.guir.clear()
                    gameStarted = false
                    val again = journal.as[LocalStorageJournal[_]]./(j => newLocalGame(j.kind, seating, difficulties, options, title)).|(new MemoryJournal[meta.gaming.ExternalAction](meta))
                    startGame(seating, difficulties, options : $[meta.O], self : $[meta.F], again, title : String, names : () => Map[meta.F, String], swt : Switches)
                }
                def editSettings(onEdit : => Unit) = {
                    ui.uir.show()
                    main.editSettings {
                        ui.uir.hide()
                        onEdit
                    }
                }
                def settings = main.settings
                // "Main Menu" under "Report a Bug": loads the game's main menu afresh, so nothing of this game's UI
                // is left over. Online and saved local games can be picked up again from there; a game kept only
                // in memory (Training Grounds, URL-started games) asks first, as it would be lost
                def canReturnToTitle = HRF.offline.not
                def returnToTitle() = {
                    val unsaved = journal.is[MemoryJournal[_]] && HRF.lobby.none
                    if (unsaved.not || dom.window.confirm("This game isn't saved and will be lost. Leave it for the main menu?"))
                        dom.document.location.assign("/play/" + meta.name)
                }
                def hotseat = (journal.is[MemoryJournal[_]] || journal.is[LocalStorageJournal[_]]) && seating.count(f => difficulties(f) == Human) > 1
                def canReportBug = HRF.server.any && HRF.offline.not
                def reportBug() = {
                    journal.read(0) { actions =>
                        val details = () =>
                            "### Game\n\n" +
                            "- Game: " + meta.label + " (" + meta.name + "), HRF " + HRF.version + "\n" +
                            "- Title: " + title + "\n" +
                            "- Factions: " + seating./(f => meta.factionName(f) + " (" + (difficulties.get(f) match {
                                case Some(Bot(d)) => "bot " + d
                                case Some(d) => d.toString.toLowerCase
                                case None => "?"
                            }) + ")").join(", ") + "\n" +
                            self.any.??("- Reporter plays: " + self./(meta.factionName).join(", ") + "\n") +
                            "- Options: " + options./(meta.writeOption).join(" ").some.|("none") + "\n" +
                            "- Online game: " + HRF.lobby.|("no (local game)") + "\n" +
                            "- Moves: " + actions.num + "\n\n" +
                            "<details><summary>Moves so far (last 300)</summary>\n\n```\n" + actions.takeRight(300)./(meta.writeActionExternal).join("\n").takeRight(30000) + "\n```\n\n</details>"

                        BugReport.show(HRF.server.get, details, () => {})
                    }
                }
            }

            val gui = HRF.metaUIs.toMap.apply(meta).asInstanceOf[BaseUI { val mmeta : meta.type }]

            val renderer = gui.create(ui.guir, seating.num, options, resources, title, callbacks)

            ui.uir.hide()

            applySettings(settings)

            def ask(game : G, faction : meta.F) : AskResult = {
                if (faction == null)
                    AskHuman
                else
                difficulties(faction) match {
                    case _ if journal.is[ServerPhantomJournal[_]] =>
                        AskHuman
                    case Human if journal.is[ServerJournal[_]].not =>
                        AskHuman
                    case Human if self.contains(faction) =>
                        AskHuman
                    case Human =>
                        WaitRemote
                    case BotDebug(botname) =>
                        val bot = meta.getBot(faction, botname)
                        if (bot.is[EvalBot]) {
                            val ebot = bot.as[EvalBot].get
                            DebugBot((actions : $[UserAction]) => {
                                val aa = game.explode(actions, false, None)
                                ebot.eval(aa)(game).map(_.sortWith(EvalBot.compare))
                            })
                        }
                        else
                            AskBot((actions : $[UserAction]) => meta.getBot(faction, botname).ask(actions, 0)(game))

                    case Bot(botname) =>
                        AskBot((actions : $[UserAction]) => meta.getBot(faction, botname).ask(actions, 0.01)(game))
                }
            }

            import meta.tagF

            Runner.run(meta)(seating, options, resources, renderer, (g, f) => f.as[meta.F]./(f => ask(g, f)).|!("unsuitable ask"), journal, onOver)
        }

    }

    def arrangeOnlineGame(seating : $[meta.F], difficulties : Map[meta.F, Difficulty], options : $[meta.O], names : Map[meta.F, String], notes : Map[meta.F, String], merge : Boolean, title : String, time : scalajs.js.Date, actions : $[meta.gaming.Action], comment : |[String], onCreate : OnlineGame => Unit) {
        val server = HRF.server.get

        val factions = seating.%(f => difficulties(f) != Off)

        val humans = factions.%(f => difficulties(f) == Human)
        val bots = factions.%(f => difficulties(f) != Human)

        var plays = Map[String, String]()
        var users = Map[meta.F, String]()
        var spectate = ""
        val version = HRF.versionOverride./("?version=" + _).|("")

        post(server + "/new-user", title + " HOST") { mm =>
            val master = mm.split('\n').join("/")

            post(server + "/new-journal/" + master, title + " LOBBY") { lobby =>
                post(server + "/new-journal/" + master, title) { game =>
                    def writeLobby() {
                        var intro = $("meta " + meta.name, "version " + HRF.version, "title " + title) ++
                            humans./(f => "user " + meta.writeFaction(f) + " " + users(f)) ++
                            humans./~(f => notes.get(f)./(n => "prename " + users(f) + " " + n)) ++
                            humans./~(f => names.get(f)./(n => "name " + users(f) + " " + n)) ++
                            bots./(f => "bot " + meta.writeFaction(f) + " " + difficulties(f).asInstanceOf[Bot].name) ++
                            comment./(c => "comment " + c).$ ++
                            $(
                                "seating " + factions./(meta.writeFaction).join(" "),
                                "options " + options./(meta.writeOption).join(" "),
                                "server " + game
                            )

                        post(server + "/append/" + master + "/" + lobby + "/0", intro.join("\n")) { _ =>
                            post(server + "/append/" + master + "/" + game + "/0", actions./(meta.writeAction).join(comment.?(" // " + _) + "\n") + comment.?(" // " + _)) { _ =>
                                val og = OnlineGame(time.getTime(), HRF.version, meta.name, lobby, title, options.of[ImportantOption]./(meta.writeOption), humans./(f => PlayerLink(meta.writeFaction(f), plays(users(f)), notes.get(f).|(""))), plays(spectate), $)

                                showLinksMenu(og)

                                Local.set(meta.settingsKey + ".online.game." + og.lobby + "." + og.time, HRF.writeOnlineGame(og))

                                onCreate(og)
                            }
                        }
                    }

                    post(server + "/new-play/" + master + "/" + lobby, "Spectator") { sp =>
                        spectate = sp.split('\n')(0)
                        plays += spectate -> sp.split('\n')(1)
                        post(server + "/grant-read/" + master + "/" + game + "/" + spectate, "") { _ =>
                            var n = humans.num
                            humans.take(merge.?(1).|(n)).foreach { f =>
                                post(server + "/new-play/" + master + "/" + lobby, meta.factionName(f)) { sc =>
                                    users += f -> sc.split('\n')(0)
                                    plays += users(f) -> sc.split('\n')(1)
                                    post(server + "/grant-read-append/" + master + "/" + game + "/" + users(f), "") { _ =>
                                        n -= 1
                                        if (merge) {
                                            humans.drop(1).foreach { f =>
                                                users += f -> sc.split('\n')(0)
                                                plays += users(f) -> sc.split('\n')(1)
                                            }
                                            writeLobby()
                                        }
                                        else
                                        if (n == 0)
                                            writeLobby()
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    def showLinksMenu(og : OnlineGame) {
        HRF.segments = $

        val timestamp = new scalajs.js.Date(og.time).asInstanceOf[Dynamic].toLocaleString("sv-SE").toString

        val meta = HRF.metas.%(_.name == og.meta).single.|(this.meta)

        def url(key : String) = HRF.offsite./(_ + "?play=" + key).|(HRF.server.get + "/play/" + meta.name + "/" + key + (og.version != HRF.version || HRF.versionOverride.any).??("?version=" + og.version))

        var ca : Elem = "Copy all links".hl

        def linkMenu() {
            ui.action.asker.zask(
                $(ZBasic((meta.label.hl ~ " Online Game").spn(xstyles.larger110), Div(timestamp.spn(xstyles.smaller75) ~ Break ~ og.title.hl ~ og.options./~(meta.parseOption)./(o => Break ~ o.group ~ " " ~ o.valueOn).merge, $(xlo.fullwidth, xlo.fullheight)))) ++
                og.spectate.some./(s => ZOption("Spectator Link", (Link(url(s), "Spectate".txt, $(xstyles.link, xlo.fullwidth, xlo.fullheight)) ~ Image("external-link")(xstyles.explain)(xstyles.clickThrough)).div(ZBasic.choice))) ++
                og.links./(l => ZOption("Player Links", (Link(url(l.key), l.note.some./(_.hl).|("Play".txt) ~ " as ".spn(xstyles.normal) ~ meta.parseFaction(l.faction)./(meta.factionElem).|(l.faction.txt), $(xstyles.link, xlo.fullwidth, xlo.fullheight)) ~ Image("external-link")(xstyles.explain)(xstyles.clickThrough)).div(ZBasic.choice))) ++
                $(ZBasic("", ca, () => {
                    ca = clipboard(
                        ($(meta.label) ++
                            $(og.title) ++
                            og.options ++
                            og.spectate.some./(s => "Spectate " + url(s)) ++
                            og.links./(l => l.note.some./(_ + " ").|("") + l.faction + " " + url(l.key))
                        ).join("\n")
                    ).?("Copied links to clipboard").|("Error copying to clipboard").hl

                    linkMenu()
                })).%(_ => og.links.num > 1) ++
                og.status.has("archived").$(ZBasic("   ", "Move to " ~ "Recent".hh, () => {
                    Local.set(meta.settingsKey + ".online.game." + og.lobby + "." + og.time, HRF.writeOnlineGame(og.copy(status = og.status.but("archived"))))
                    // onlineGame()
                    history.popState()
                })) ++
                og.status.has("archived").not.$(ZBasic("  ", "Move to " ~ "Archive".hh, () => {
                    Local.set(meta.settingsKey + ".online.game." + og.lobby + "." + og.time, HRF.writeOnlineGame(og.copy(status = og.status :+ "archived")))
                    // onlineGame()
                    history.popState()
                })) ++
                $(ZBasic(" ", "Back", () => {
                    // if (og.status.has("archived").not)
                    //     onlineGame()
                    // else
                    //     onlineGameArchive()
                    history.popState()
                }))
            )
        }

        linkMenu()
    }

    def showOverlay(l : $[ZOption], grow : Style) {
        ui.overlay.pane.vis()

        val ok = $(ZBasic(Empty, "Ok", () => {
            ui.overlay.pane.invis()
        }))

        // a picture missing from the loaded assets throws; the Ok must still come, or the player is stuck behind the overlay
        try {
            ui.overlay.asker.zask(l ++ $(ZOption(Div(Empty, grow), Empty)) ++ ok)
        }
        catch {
            case e : Throwable =>
                println("overlay failed: " + e)
                ui.overlay.asker.zask(ok)
        }
    }

    def showOverlayRaw(title : Elem, l : $[Elem]) {
        ui.overlay.pane.vis()

        ui.overlay.asker.zask(
            $(ZBasic(title, l./(e => Div(e)).merge.div(xlo.flexvcenter))) ++
            $(ZBasic(Empty, "Ok", () => {
                ui.overlay.pane.invis()
            }))
        )
    }

}
