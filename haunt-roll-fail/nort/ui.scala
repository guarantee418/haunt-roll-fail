package nort
//
//
//
//
import hrf.colmat._
import hrf.logger._
//
//
//
//

import org.scalajs.dom

import hrf.canvas._

import hrf.web._
import hrf.ui._

import hrf.elem._
import hrf.html._

import nort.elem._

import hrf.ui.again._
import hrf.ui.sprites._

import hrf.tracker4.implicits._

import scalajs.js.timers.setTimeout


object UI extends BaseUI {
    val mmeta = Meta

    def create(uir : ElementAttachmentPoint, arity : Int, options : $[hrf.meta.GameOption], resources : Resources, title : String, callbacks : hrf.Callbacks) = new UI(uir, arity, options, resources, callbacks)
}

class UI(val uir : ElementAttachmentPoint, arity : Int, val options : $[hrf.meta.GameOption], val resources : Resources, callbacks : hrf.Callbacks) extends MapGUI {
    def factionElem(f : Faction) = f.name.styled(f)

    val statuses = 1.to(arity)./(i => newPane("status-" + i, Content, styles.status, styles.fstatus, ExternalStyle("hide-scrollbar")))

    val background = new OrderedLayer
    val pieces = new OrderedLayer

    // Scene units per map tile
    val T = 948.0
    val margins = Margins(60, 60, 60, 60)
    zoomBase = 0

    // Scene size, updated with the map
    var sceneWidth = 3 * T
    var sceneHeight = 3 * T

    override def adjustCenterZoomX() {
        zoomBase = zoomBase.clamp(-990, 990*2)

        val qX = (sceneWidth + margins.left + margins.right) * (1 - 1 / zoom) / 2
        val minX = -qX + margins.right - zoomBase / 5
        val maxX = qX - margins.left + zoomBase / 5
        dX = dX.clamp(minX, maxX)

        val qY = (sceneHeight + margins.top + margins.bottom) * (1 - 1 / zoom) / 2
        val minY = -qY + margins.bottom - zoomBase / 5
        val maxY = qY - margins.top + zoomBase / 5
        dY = dY.clamp(minY, maxY)
    }

    class Highlights {
        var coordinates : |[XY] = None
        var target : $[Any] = $
    }

    var highlight = new Highlights

    def processRightClick(target : $[Any], xy : XY) {
        lastActions.of[Cancel].single.foreach(onClick)
    }

    def processHighlight(target : $[Any], xy : XY) {
        highlight.coordinates = |(xy)
        highlight.target = target

        mapSmall.attach.parent.style.cursor = clickable(target).any.?("pointer").|("default")
    }

    // The single offered action a click on the map stands for
    def clickable(target : $[Any]) : |[UserAction] = {
        target.foreach { t =>
            val l = lastActions.%(a => a.unwrap.as[MapTarget].exists(_.target == t))
            if (l.num == 1)
                return l.headOption
        }
        None
    }

    def processTargetClick(target : $[Any], xy : XY) {
        clickable(target).foreach(onClick)
    }

    // Rotated tile images, made once
    val rotated = scala.collection.mutable.Map[(String, Int), hrf.ui.sprites.Image]()

    def tileImage(tile : String, r : Int) : hrf.ui.sprites.Image = rotated.getOrElseUpdate((tile, r % 4), {
        val i = img("tile-" + tile)
        (r % 4) match {
            case 0 => new RawImage(i)
            case 1 => new RawImageRotated90(i)
            case 2 => new RawImageRotated180(i)
            case 3 => new RawImageRotated270(i)
        }
    })

    def at(image : String, size : Double, alpha : Double = 1.0) : ImageRect = ImageRect(new RawImage(img(image)), Rectangle(-size / 2, -size / 2, size, size), alpha)

    def makeScene() : |[Scene] = {
        if (game.states.none || game.board.placements.none)
            return None

        val board = game.board

        val (x0, y0, x1, y1) = board.bounds

        // One spare row and column around the map for new tiles
        def sx(x : Double) = (x - x0 + 1) * T
        def sy(y : Double) = (y - y0 + 1) * T

        sceneWidth = (x1 - x0 + 3) * T
        sceneHeight = (y1 - y0 + 3) * T

        background.clear()
        pieces.clear()

        board.placements.foreach { p =>
            background.add(Sprite($(ImageRect(tileImage(p.tile, p.r), Rectangle(0, 0, T, T), 1.0)), $))(sx(p.x), sy(p.y))
        }

        val targets = lastActions./~(_.unwrap.as[MapTarget])./(_.target)

        // Empty spots offered for a new tile
        targets.of[Spot].distinct.foreach { s =>
            val n = board.spotLabel(s.x, s.y)
            if (n > 0 && n <= 40)
                pieces.add(Sprite($(at("ui-spot-" + n, T * 0.9)), $(Rectangle(-T * 0.45, -T * 0.45, T * 0.9, T * 0.9)), $(s)))(sx(s.x + 0.5), sy(s.y + 0.5))
        }

        // Buildings on their spaces
        game.buildings.foreach { case (s, b) =>
            val (x, y) = board.point(s)
            pieces.add(Sprite($(at(b.image, 190)), $))(sx(x), sy(y))
        }

        // Territory numbers, highlighted when they can be chosen, and units next to them
        board.territories.zipWithIndex.foreach { case (t, i) =>
            val (x, y) = board.point(t.anchor)
            val tag = $(t.anchor)
            val box = $(Rectangle(-70, -70, 140, 140))

            if (targets.has(t.anchor))
                pieces.add(Sprite($(at("ui-target", 230)), box, tag))(sx(x), sy(y))

            if (i < 99)
                pieces.add(Sprite($(at("ui-label-" + (i + 1), 110)), box, tag))(sx(x), sy(y))

            game.present(t).zipWithIndex.foreach { case (f, k) =>
                val n = game.count(t, f)
                val ux = sx(x) + 150 + k * 190
                val uy = sy(y)
                pieces.add(Sprite($(at("unit-" + game.colors(f).id, 200)), $(Rectangle(-100, -100, 200, 200)), tag))(ux, uy)
                if (n <= 15)
                    pieces.add(Sprite($(at("ui-count-" + n, 90)), $))(ux + 60, uy + 70)
            }
        }

        |(new Scene($(background, pieces), sceneWidth, sceneHeight, margins))
    }

    // The player this client shows the hand of (none for spectators)
    var viewer : |[Faction] = None

    // Like the Arcs court: the cards everyone can see, always on top
    val court = newPane("court", Content, styles.strip)

    // Like the Arcs hand: your cards, always at the bottom
    val hand = newPane("hand", Content, styles.strip)

    def strip(groups : $[(Elem, $[Card])]) =
        Div(groups./{ case (title, cards) =>
            Div(Div(title, styles.stripTitle) ~ Div(cards.any.?(cards./(c => Image(c.info.image, styles.stripCard)).merge).|(Div("none".txt, styles.stripEmpty)), styles.stripCards), styles.stripGroup)
        }.merge, styles.stripRow)

    def drawCards() {
        if (game.year == 0)
            return

        val last = game.year == game.lastYear

        val developments = last.not.$(("Developments, year " ~ game.year.hlb ~ " of " ~ game.lastYear.hl) -> game.display)

        val achievements = $(("Achievements" ~ last.not.?(", year " ~ game.lastYear.hl).|(Empty)) -> last.?(game.display).|(game.achievements))

        court.replaceCached((game.year, game.display, game.achievements).toString, strip(developments ++ achievements), resources)

        val self = viewer.%(game.states.contains)

        val cards = self./~(f => $(("Your hand".styled(f) : Elem) -> f.hand) ++ f.active.any.$(("Played".styled(f) : Elem) -> f.active))

        hand.replaceCached((self, self./(_.hand), self./(_.active)).toString, strip(cards), resources)
    }

    def factionStatus(f : Faction) {
        val container = statuses(game.setup.indexOf(f))

        val name = resources.getName(f).|(f.name)

        if (!game.states.contains(f)) {
            container.replace(Div(Div(name).styled(f)(styles.title), styles.smallname, xlo.pointer), resources)
            return
        }

        val title = Div(Div(name.styled(f)(styles.title) ~ " " ~ game.colors(f).elem), styles.smallname, styles.titleLine, xlo.pointer)

        val state = game.states(f)

        val res = Resource.all./(r => state.has(r).hl ~ " " ~ r.elem).join(" ").div

        val units = (state.units.hl ~ " units, " ~ state.fame.hl ~ " fame").div

        val cards = (state.hand.num.hl ~ " in hand, " ~ state.draw.num.hl ~ " to draw").div

        val marks = ((game.first == f).?("First player".hh).|(Empty) ~ (state.passed && game.isOver.not).?(" Passed".txt).|(Empty)).div

        val content = (title.div ~ res ~ units ~ cards ~ marks).div(styles.statusUpper)(xlo.flexVX)(ExternalStyle("hide-scrollbar")).pointer.onClick.param(f)

        container.replace(content, resources, {
            case x => onClick(x)
        })

        if (game.highlight.current.has(f))
            container.attach.parent.style.outline = "2px solid #aaaaaa"
        else
        if (game.highlight.faction.has(f))
            container.attach.parent.style.outline = "2px dashed #aaaaaa"
        else
            container.attach.parent.style.outline = ""
    }

    def updateStatus() {
        0.until(arity).foreach { n =>
            factionStatus(game.setup(n))
        }

        if (overlayPane.visible)
            overlayPane.vis()
        else
            overlayPane.invis()

        drawMap()

        drawCards()
    }

    val layoutZoom = 0.49 * 0.88

    val kkk = 1.18 / 30

    val layouts = $(Layout("base",
        $(
            BasicPane("status", 15, 8.5, Priorities(top = 3, left = 2, maxXscale = 1.8, maxYscale = 1.8, grow = 1)),
            BasicPane("court", 80, 20, Priorities(top = 3, right = 3, maxXscale = 1.5, maxYscale = 1.5, grow = -2)),
            BasicPane("hand", 60, 22, Priorities(bottom = 2, left = 1, maxXscale = 1.5, maxYscale = 1.5, grow = -2)),
            BasicPane("log", 32, 16, Priorities(right = 1)),
            BasicPane("map-small", 73, 70, Priorities(top = 2, left = 1, grow = 3)),
            BasicPane("action-a", 64/1.5, 36, Priorities(bottom = 1, right = 3, grow = 2)),
            BasicPane("action-b", 55/1.5, 47, Priorities(bottom = 1, right = 3, grow = 2, maxXscale = 1.2)),
        )
       ./(p => p.copy(kX = p.kX * layoutZoom, kY = p.kY * layoutZoom))
    ))./~(l =>
        l.copy(name = l.name + "-fulldim", panes = l.panes./{
            case p : BasicPane if p.name == "map-small" => FullDimPane(p.name, p.kX, p.kY, p.pr)
            case p => p
        }, boost = 1.2) ::
        l.copy(name = l.name + "-plus20", panes = l.panes./{
            case p : BasicPane if p.name == "map-small" => BasicPane(p.name, p.kX * 1.2, p.kY * 1.2, p.pr)
            case p => p
        }, boost = 1.1) ::
        l.copy(name = l.name + "-normal")
    )./~(l =>
        l.copy(name = l.name + "-horizontal", boost = l.boost * 1.02, panes = l.panes./{
            case p : BasicPane if p.name == "status" => p.copy(name = "status-horizontal", kX = p.kX * arity)
            case p => p
        }) ::
        l.copy(name = l.name + "-vertical", panes = l.panes./{
            case p : BasicPane if p.name == "status" => p.copy(name = "status-vertical", kY = p.kY * arity)
            case p => p
        })
    )./~(l =>
        l.copy(name = l.name + "-actionA", panes = l.panes./~{
            case p : BasicPane if p.name == "action-a" => Some(p.copy(name = "action"))
            case p : BasicPane if p.name == "action-b" => None
            case p => Some(p)
        }) ::
        l.copy(name = l.name + "-actionB", panes = l.panes./~{
            case p : BasicPane if p.name == "action-a" => None
            case p : BasicPane if p.name == "action-b" => Some(p.copy(name = "action"))
            case p => Some(p)
        }) ::
        Nil
    )

    val layouter = Layouter(layouts, _./~{
        case f if f.name == "action" => $(f, f.copy(name = "undo"), f.copy(name = "settings"))
        case f if f.name == "map-small" => $(f, f.copy(name = "map-small-overlay"))
        case f if f.name == "status-horizontal" => 1.to(arity)./(n => f.copy(name = "status-" + n, x = f.x + ((n - 1) * f.width  /~/ arity), width  = (n * f.width  /~/ arity) - ((n - 1) * f.width  /~/ arity)))
        case f if f.name == "status-vertical"   => 1.to(arity)./(n => f.copy(name = "status-" + n, y = f.y + ((n - 1) * f.height /~/ arity), height = (n * f.height /~/ arity) - ((n - 1) * f.height /~/ arity)))
        case f => $(f)
    })

    val settingsKey = Meta.settingsKey

    val layoutKey = "v" + 3 + "." + "arity-" + arity

    def overlayScrollX(e : Elem) = overlayScroll(e)(styles.seeThroughInner).onClick
    def overlayFitX(e : Elem) = overlayFit(e)(styles.seeThroughInner).onClick

    def showOverlay(e : Elem, onClick : Any => Unit) {
        overlayPane.vis()
        overlayPane.replace(e, resources, onClick, _ => {}, _ => {})
    }

    override def onClick(a : Any) = a @@ {
        case Some(x) => onClick(x)

        case ("notifications", Some(f : Faction)) =>
            shown = $
            showNotifications($(f))

        case ("notifications", None) =>
            shown = $
            showNotifications(game.factions)

        case $(f : Faction, x) =>
            onClick(x)

        case action : Action if lastThen != null =>
            clearOverlay()

            highlight = new Highlights

            val then = lastThen
            lastThen = null
            lastActions = $
            keysDirect = $
            keysExplode = $

            asker.clear()

            then(action.as[UserAction].||(action.as[ForcedAction]./(_.as("Do Action On Click"))).|(throw new Error("non-user non-forced action in on click handler")))


        case Nil =>
            clearOverlay()

        case Left(x) => onClick(x)
        case Right(x) => onClick(x)

        case x =>
            println("unknown onClick: " + x)
    }

    def clearOverlay() {
        overlayPane.invis()
        overlayPane.clear()
    }

    override def info(self : |[Faction], aa : $[UserAction]) = {
        val ii = currentGame.info($, self, aa)
        ii.any.??($(ZOption(Empty, Break)) ++ convertActions(self.of[Faction], ii)) ++
            (currentGame.isOver && hrf.HRF.flag("replay").not).$(
                ZBasic(Break ~ Break ~ Break, "Save Replay As File".hh, () => {
                    showOverlay(overlayScrollX("Saving Replay...".hl.div).onClick, null)

                    callbacks.saveReplay {
                        overlayPane.invis()
                        overlayPane.clear()
                    }
                }).copy(clear = false)
            ) ++
            (hrf.HRF.param("lobby").none && hrf.HRF.offline.not).$(
                ZBasic(Break ~ Break ~ Break, "Save Game Online".hh, () => {
                    showOverlay(overlayScrollX("Save Game Online".hlb(xstyles.larger125) ~
                        ("Save".hlb).div.div(xstyles.choice)(xstyles.xx)(xstyles.chm)(xstyles.chp)(xstyles.thu)(xlo.fullwidth)(xstyles.width60ex).pointer.onClick.param("***") ~
                        ("Save and replace bots with humans".hh).div.div(xstyles.choice)(xstyles.xx)(xstyles.chm)(xstyles.chp)(xstyles.thu)(xlo.fullwidth)(xstyles.width60ex).pointer.onClick.param("///") ~
                        ("Save as a single-player multi-handed game".hh).div.div(xstyles.choice)(xstyles.xx)(xstyles.chm)(xstyles.chp)(xstyles.thu)(xlo.fullwidth)(xstyles.width60ex).pointer.onClick.param("###") ~
                        ("Cancel".txt).div.div(xstyles.choice)(xstyles.xx)(xstyles.chm)(xstyles.chp)(xstyles.thu)(xlo.fullwidth)(xstyles.width60ex).pointer.onClick.param("???")
                    ).onClick, {
                        case "***" => callbacks.saveReplayOnline(false, false) { url => onClick(Nil) }
                        case "///" => callbacks.saveReplayOnline(true , false) { url => onClick(Nil) }
                        case "###" => callbacks.saveReplayOnline(true , true ) { url => onClick(Nil) }
                        case _ => onClick(Nil)
                    })
                }).copy(clear = false)
            ) ++
            $(ZBasic(Break ~ Break, "Interface".spn, () => { callbacks.editSettings { updateStatus() } }).copy(clear = false)) ++
            $(ZBasic(Break ~ Break, "Report a Bug".spn, () => { callbacks.reportBug() }).copy(clear = false)).%(_ => callbacks.canReportBug)
    }

    var shown : $[Notification] = $

    override def showNotifications(self : $[F]) : Unit = {
        val newer = game.notifications
        val older = shown

        shown = game.notifications

        val display = newer.diff(older).%(_.factions.intersect(self).any)./~(n => convertActions(self.single, n.infos)).some./~(_ :+ ZOption(Empty, Break))

        if (display.none)
            return

        overlayPane.vis()

        overlayPane.attach.clear()

        val ol = overlayPane.attach.appendContainer(overlayScrollX(Content), resources, onClick)

        val asker = new NewAsker(ol, s => img(s))

        asker.zask(display)(resources)
    }

    override def wait(self : $[F], factions : $[F], message : Elem) {
        lastActions = $
        lastThen = null

        viewer = self.single || self.intersect(game.highlight.current.$).single || viewer.%(self.has)

        drawCards()

        showNotifications(self)

        super.wait(self, factions, message)
    }

    var lastActions : $[UserAction] = $
    var lastThen : UserAction => Unit = null

    var keysDirect = $[Key]()

    var keysExplode = $[Key]()

    def keys = keysDirect ++ keysExplode

    override def ask(faction : |[F], actions : $[UserAction], then : UserAction => Unit) {
        lastActions = actions
        lastThen = then

        viewer = faction

        showNotifications(faction.$)

        keysDirect = actions./~(a => a.as[Key] || a.unwrap.as[Key]).distinct

        keysExplode ++= actions.of[Choice].some./~(l => game.explode(actions, false, None)./~(a => a.as[Key] || a.unwrap.as[Key]).distinct)

        updateStatus()

        super.ask(faction, actions, a => {
            clearOverlay()
            keysDirect = $
            keysExplode = $
            then(a)
        })
    }

    override def styleAction(faction : |[F], actions : $[UserAction], a : UserAction, unavailable : Boolean, view : |[Any]) : $[Style] =
        view @@ {
            case _ if unavailable.not => $()
            case Some(_) => $(xstyles.unavailableCard)
            case _ => $(xstyles.unavailableText)
        } ++
        a @@ {
            case _ : Info => $(xstyles.info)
            case _ if unavailable => $(xstyles.info)
            case _ => $(xstyles.choice)
        } ++
        a @@ {
            case _ => $(xstyles.xx, xstyles.chp, xstyles.chm)
        } ++
        faction @@ {
            case Some(f : Faction) => $(elem.borders.get(f))
            case _ => $()
        } ++
        a @@ {
            case a : Selectable if a.selected => $(styles.selected)
            case _ => $()
        } ++
        view @@ {
            case Some(_) => $(styles.inline)
            case _ => $(xstyles.thu, xstyles.thumargin, xlo.fullwidth)
        } ++
        a @@ {
            case _ if unavailable => $()
            case _ : Extra[_] => $()
            case _ : Choice | _ : Cancel | _ : Back | _ : OnClickInfo => $(xlo.pointer)
            case _ => $()
        }

}
