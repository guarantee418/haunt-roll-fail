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
    def factionElem(f : Faction) = f.name.styled(colorOf(f))

    // The style of the color a clan's player picked
    def colorOf(f : Faction) : Style = elem.styles.get(game.colors.get(f)./(c => c : Styling).|(f))

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

    // Zoom from half size to about six times; the map can be dragged until its middle reaches the edge
    override def adjustCenterZoomX() {
        zoomBase = zoomBase.clamp(-990, 2560)

        dX = dX.clamp(-sceneWidth / 2, sceneWidth / 2)
        dY = dY.clamp(-sceneHeight / 2, sceneHeight / 2)
    }

    // Pan and zoom, replacing the MapGUI handlers: drag (mouse or one finger) pans at any zoom,
    // the mouse wheel zooms around the cursor, and two fingers pinch-zoom and pan together
    object mapControl {
        val node = mapSmall.attach.parent

        // Screen point (client coordinates) to scene coordinates, for a zoom and offset
        def scene(c : XY, zoom : Double, dx : Double, dy : Double) : |[XY] = lastScene./{ scene =>
            val r = node.getBoundingClientRect()
            val k = dom.window.devicePixelRatio
            scene.toSceneCoordinates((c.x - r.left) * k, (c.y - r.top) * k, (node.clientWidth * k * upscale).~, (node.clientHeight * k * upscale).~, zoom, dx, dy)
        }

        // Change the zoom keeping the scene point under `from` under `to`
        def zoomAt(from : XY, to : XY, base : Double) {
            scene(from, zoom, dX, dY).foreach { p =>
                zoomBase = base.clamp(-990, 2560)
                scene(to, zoom, 0, 0).foreach { q =>
                    dX = q.x - p.x
                    dY = q.y - p.y
                }
            }
            drawMap()
        }

        val pointers = scala.collection.mutable.Map[Double, XY]()

        var down : |[XY] = None
        var grabbed : |[XY] = None
        var dragged = false

        def center = XY(pointers.values.map(_.x).sum / pointers.size, pointers.values.map(_.y).sum / pointers.size)
        def spread = { val c = center ; pointers.values.map(p => math.sqrt((p.x - c.x) * (p.x - c.x) + (p.y - c.y) * (p.y - c.y))).sum / pointers.size }

        def regrab() {
            grabbed = (pointers.size == 1).??(scene(pointers.values.head, zoom, dX, dY))
        }

        node.style.touchAction = "none"

        node.onpointerdown = (e : dom.PointerEvent) => {
            val c = XY(e.clientX, e.clientY)
            pointers(e.pointerId) = c

            // Keep getting the moves and the release when the pointer leaves the map
            node.setPointerCapture(e.pointerId)

            if (pointers.size == 1) {
                down = |(c)
                dragged = false

                scene(c, zoom, dX, dY).foreach { xy =>
                    lastScene.foreach(s => processHighlight(s.pick(xy), xy))
                }
            }
            else
                dragged = true

            regrab()
        }

        node.onpointermove = (e : dom.PointerEvent) => {
            val c = XY(e.clientX, e.clientY)

            if (pointers.contains(e.pointerId)) {
                val before = (center, spread)

                pointers(e.pointerId) = c

                if (pointers.size == 1) {
                    if (down.exists(d => math.abs(d.x - c.x) + math.abs(d.y - c.y) > 6))
                        dragged = true

                    if (dragged) {
                        grabbed.foreach { g =>
                            scene(c, zoom, 0, 0).foreach { q =>
                                dX = q.x - g.x
                                dY = q.y - g.y
                            }
                        }

                        node.style.cursor = "grabbing"

                        drawMap()
                    }
                }
                else {
                    val (c0, s0) = before
                    val s1 = spread

                    if (s0 > 8 && s1 > 8)
                        zoomAt(c0, center, zoomBase + math.log(s1 / s0) / math.log(1.0007))
                }
            }
            else
            if (e.pointerType == "mouse")
                scene(c, zoom, dX, dY).foreach { xy =>
                    lastScene.foreach(s => processHighlight(s.pick(xy), xy))
                }
        }

        def release(e : dom.PointerEvent) {
            pointers.remove(e.pointerId)

            if (pointers.isEmpty) {
                down = None
                node.style.cursor = "default"
            }

            regrab()
        }

        node.onpointerup = (e : dom.PointerEvent) => release(e)
        node.onpointercancel = (e : dom.PointerEvent) => release(e)
        node.onpointerout = null

        // A click that ended a drag or a pinch doesn't pick anything
        node.onclick = (e : dom.MouseEvent) => {
            if (dragged.not)
                scene(XY(e.clientX, e.clientY), zoom, dX, dY).foreach { xy =>
                    lastScene.foreach(s => processTargetClick(s.pick(xy), xy))
                }

            dragged = false
        }

        node.ontouchstart = null
        node.ontouchmove = (e : dom.TouchEvent) => e.preventDefault()

        // Wheel up zooms in; pinching a trackpad comes as a wheel event with Ctrl held
        node.onwheel = (e : dom.WheelEvent) => {
            e.preventDefault()

            val lines = (e.deltaMode == 1).?(40.0).|((e.deltaMode == 2).?(800.0).|(1.0))
            val k = e.ctrlKey.?(8.0).|(1.0)
            val c = XY(e.clientX, e.clientY)

            zoomAt(c, c, zoomBase - (e.deltaY * lines * k).clamp(-300.0, 300.0))
        }
    }

    mapControl

    // Buttons over the map's bottom right corner: zoom in and out (magnifying glasses),
    // arrows to move the map, and the middle one to reset the view
    object mapButtons {
        val node = mapSmall.attach.parent

        def svg(body : String) = "<svg viewBox='0 0 24 24' width='70%' height='70%' fill='none' stroke='#dddddd' stroke-width='2.4' stroke-linecap='round' stroke-linejoin='round'>" + body + "</svg>"

        val glass = "<circle cx='10' cy='10' r='6.5'/><line x1='15' y1='15' x2='21' y2='21'/>"
        val plus = glass + "<line x1='7' y1='10' x2='13' y2='10'/><line x1='10' y1='7' x2='10' y2='13'/>"
        val minus = glass + "<line x1='7' y1='10' x2='13' y2='10'/>"
        def arrow(r : Int) = "<g transform='rotate(" + r + " 12 12)'><polyline points='6,14 12,8 18,14'/></g>"
        val reset = "<rect x='6' y='6' width='12' height='12' rx='1.5'/><line x1='12' y1='9' x2='12' y2='15'/><line x1='9' y1='12' x2='15' y2='12'/>"

        val box = dom.document.createElement("div").asInstanceOf[dom.html.Div]
        box.style.position = "absolute"
        box.style.right = "1.2vmin"
        box.style.bottom = "1.2vmin"
        box.style.zIndex = "10"
        box.style.display = "grid"
        box.style.setProperty("grid-template-columns", "repeat(3, max(4.6vmin, 36px))")
        box.style.setProperty("grid-auto-rows", "max(4.6vmin, 36px)")
        box.style.setProperty("gap", "0.5vmin")
        box.style.setProperty("touch-action", "manipulation")

        // An arrow shows more of the map on its side, by a quarter of the pane
        def pan(fx : Double, fy : Double) {
            val r = node.getBoundingClientRect()
            val c = XY(r.left + r.width / 2, r.top + r.height / 2)
            val t = XY(c.x + fx * r.width / 4, c.y + fy * r.height / 4)

            for (a <- mapControl.scene(c, zoom, 0, 0) ; b <- mapControl.scene(t, zoom, 0, 0)) {
                dX -= b.x - a.x
                dY -= b.y - a.y
            }

            drawMap()
        }

        def zoomBy(d : Double) {
            val r = node.getBoundingClientRect()
            val c = XY(r.left + r.width / 2, r.top + r.height / 2)
            mapControl.zoomAt(c, c, zoomBase + d)
        }

        def button(icon : String, title : String, col : Int, row : Int)(action : => Unit) {
            val b = dom.document.createElement("div").asInstanceOf[dom.html.Div]
            b.innerHTML = svg(icon)
            b.title = title
            b.style.setProperty("grid-column", col.toString)
            b.style.setProperty("grid-row", row.toString)
            b.style.display = "flex"
            b.style.setProperty("align-items", "center")
            b.style.setProperty("justify-content", "center")
            b.style.background = "#222222c0"
            b.style.border = "1px solid #aaaaaa80"
            b.style.borderRadius = "0.8vmin"
            b.style.cursor = "pointer"
            b.style.setProperty("user-select", "none")

            // Don't let the map take these as drags or clicks on it
            b.onpointerdown = (e : dom.PointerEvent) => e.stopPropagation()
            b.onpointerup = (e : dom.PointerEvent) => e.stopPropagation()
            b.onwheel = (e : dom.WheelEvent) => e.stopPropagation()
            b.ontouchmove = (e : dom.TouchEvent) => e.stopPropagation()
            b.onclick = (e : dom.MouseEvent) => {
                e.stopPropagation()
                action
            }

            box.appendChild(b)
        }

        button(plus, "Zoom in", 1, 1)(zoomBy(350))
        button(minus, "Zoom out", 3, 1)(zoomBy(-350))
        button(arrow(0), "Move up", 2, 2)(pan(0, -1))
        button(arrow(270), "Move left", 1, 3)(pan(-1, 0))
        button(reset, "Reset the view", 2, 3) { zoomBase = 0 ; dX = 0 ; dY = 0 ; drawMap() }
        button(arrow(90), "Move right", 3, 3)(pan(1, 0))
        button(arrow(180), "Move down", 2, 4)(pan(0, 1))

        node.appendChild(box)
    }

    mapButtons

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

        // The tile being placed, previewed at its spot
        lastActions./~(_.unwrap.as[TilePreview]).take(1).foreach { p =>
            background.add(Sprite($(ImageRect(tileImage(p.tile, p.r), Rectangle(0, 0, T, T), 0.85)), $))(sx(p.spot.x), sy(p.spot.y))
        }

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

        // Territory numbers on every area of the territory, highlighted when it can be chosen; units at its first area
        board.territories.zipWithIndex.foreach { case (t, i) =>
            val tag = $(t.anchor)
            val box = $(Rectangle(-70, -70, 140, 140))

            t.areas.foreach { a =>
                val (x, y) = board.point(a)

                if (targets.has(t.anchor))
                    pieces.add(Sprite($(at("ui-target", 230)), box, tag))(sx(x), sy(y))

                if (i < 99)
                    pieces.add(Sprite($(at("ui-label-" + (i + 1), 110)), box, tag))(sx(x), sy(y))
            }

            // The unit point is clear of resources and buildings for one full size figure, with its count and Kaija
            // inside its outline; during a fight the clans' figures share that space at a smaller size
            val (px, py) = board.unitPoint(t.anchor)
            val present = game.present(t)
            val k = present.num
            val scale = k match { case 0 | 1 => 1.0 ; case 2 => 0.6 ; case 3 => 0.45 ; case _ => 0.36 }
            val step = 160 * scale

            present.zipWithIndex.foreach { case (f, i) =>
                val n = game.count(t, f)
                val ux = sx(px) + (k > 1).?(9 + (i - (k - 1) / 2.0) * step).|(0.0)
                val uy = sy(py) + (k > 1).?(20.0).|(0.0)
                val z = 300 * scale
                if (n > 0) {
                    pieces.add(Sprite($(at("unit-" + game.colors(f).id, z)), $(Rectangle(-z / 2, -z / 2, z, z)), tag))(ux, uy)
                    if (n <= 15)
                        pieces.add(Sprite($(at("ui-count-" + n, 96 * scale)), $))(ux + 62 * scale, uy + 62 * scale)
                }
                // Kaija in front of Bear Clan's figure, or in its place
                if (game.kaijaIn(t, f)) {
                    val kz = (n > 0).?(110).|(180) * scale
                    pieces.add(Sprite($(at("token-kaija", kz)), $(Rectangle(-kz / 2, -kz / 2, kz, kz)), tag))((n > 0).?(ux - 40 * scale).|(ux), (n > 0).?(uy + 55 * scale).|(uy))
                }
            }

            // Snake Clan's Scorched Earth token, by the territory number
            if (game.scorchedIn(t)) {
                val (x, y) = board.point(t.anchor)
                pieces.add(Sprite($(at("token-scorched-earth", 150)), $(Rectangle(-75, -75, 150, 150)), tag))(sx(x) - 40, sy(y) + 110)
            }

            // Creatures, above the territory number, side by side
            val creatures = game.creaturesIn(t)
            creatures.zipWithIndex.foreach { case (c, i) =>
                val (x, y) = board.point(t.anchor)
                val z = 220
                pieces.add(Sprite($(at(c.token, z)), $(Rectangle(-z / 2, -z / 2, z, z)), tag))(sx(x) + (i - (creatures.num - 1) / 2.0) * 190, sy(y) - 160)
            }
        }

        |(new Scene($(background, pieces), sceneWidth, sceneHeight, margins))
    }

    // Like the Arcs court: the cards everyone can see, always on top
    val court = newPane("court", Content, styles.strip)

    def stripCard(c : Card) : Elem = OnClick(c, Image(c.info.image, styles.stripCard, xlo.pointer))

    def strip(groups : $[(Elem, $[Elem])]) =
        Div(groups./{ case (title, items) =>
            Div(Div(title, styles.stripTitle) ~ Div(items.any.?(items.merge).|(Div("none".txt, styles.stripEmpty)), styles.stripCards), styles.stripGroup)
        }.merge, styles.stripRow)

    // Your hand and played cards are in the action pane, like in Arcs and Root (Game.info, CardMenuAction)
    def drawCards() {
        if (game.year == 0)
            return

        val last = game.year == game.lastYear

        val developments = last.not.$(("Developments, year " ~ game.year.hlb ~ " of " ~ game.lastYear.hl) -> game.display)

        val achievements = $(("Achievements" ~ last.not.?(", year " ~ game.lastYear.hl).|(Empty)) -> last.?(game.display).|(game.achievements))

        // Creatures module: the creature line, in activation order
        val creatures = game.has(Creatures).$(("Creatures" ~ " (left to right)".spn(xstyles.smaller85)) -> game.creatureLine)

        court.replaceCached((game.year, game.display, game.achievements, game.creatureLine).toString, strip((developments ++ achievements ++ creatures)./{ case (t, l) => t -> l./(stripCard) }), resources, onClick)
    }

    def factionStatus(f : Faction) {
        val container = statuses(game.setup.indexOf(f))

        // The player's name (human players in online games), then the clan's, in the player's color
        val name = resources.getName(f)./(n => n.styled(colorOf(f))(styles.title) ~ " " ~ f.name.txt).|(f.name.styled(colorOf(f))(styles.title))

        if (!game.states.contains(f)) {
            container.replace(Div(Div(name), styles.smallname, xlo.pointer), resources)
            return
        }

        val title = Div(Div(name), styles.smallname, styles.titleLine, xlo.pointer)

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
            BasicPane("status", 15, (arity >= 4).?(24).|(18), Priorities(top = 3, left = 2, maxXscale = 1.8, maxYscale = 1.8, grow = 1)),
            BasicPane("court", 80, 20, Priorities(top = 3, right = 3, maxXscale = 1.5, maxYscale = 1.5, grow = -2)),
            BasicPane("log", 32, 16, Priorities(right = 1)),
            BasicPane("map-small", 73, 64, Priorities(top = 2, left = 1, grow = 3)),
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
        // The player panels always go in a row along the top
        l.copy(name = l.name + "-horizontal", panes = l.panes./{
            case p : BasicPane if p.name == "status" => p.copy(name = "status-horizontal", kX = p.kX * arity)
            case p => p
        }) :: Nil
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
        case f if f.name == "status-horizontal" => 1.to(arity)./(n => f.copy(name = "status-" + n, x = f.x + ((n - 1) * f.width  /~/ arity), width  = (n * f.width  /~/ arity) - ((n - 1) * f.width  /~/ arity)))
        case f if f.name == "status-vertical"   => 1.to(arity)./(n => f.copy(name = "status-" + n, y = f.y + ((n - 1) * f.height /~/ arity), height = (n * f.height /~/ arity) - ((n - 1) * f.height /~/ arity)))
        case f => $(f)
    },
    x => x,
    // The overlay (zoomed cards, notifications) covers the whole screen
    ff => ff :+ Fit("map-small-overlay", ff./(_.x).min, ff./(_.y).min, ff./(_.right).max - ff./(_.x).min, ff./(_.bottom).max - ff./(_.y).min))

    val settingsKey = Meta.settingsKey

    val layoutKey = "v" + 8 + "." + "arity-" + arity

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


        // Any card on the table opens full screen; clicking again closes it
        case c : Card =>
            showOverlay(overlayFitX(Image(c.info.image, styles.zoomCard)).onClick, onClick)

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
