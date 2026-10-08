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
    // Adset: a player shows as their clan once drafted
    def factionElem(p : Player) : Elem = p match {
        case f : Faction => f.name.styled(colorOf(f))
        case p => currentGame.?./~(_.ptf.get(p))./(f => f.name.styled(colorOf(f))).|(p.as[Seat]./(_.elem).|(Empty))
    }

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

    // The single offered action a click on the map stands for (the cross on a building or tile being confirmed cancels)
    def clickable(target : $[Any]) : |[UserAction] = {
        target.foreach { t =>
            if (t == CancelMark)
                return lastActions.of[Cancel].single
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

    // How big a building is drawn: its space's printed octagon, so a large building fills its large space
    val smallBuilding = 190.0
    val largeBuilding = 240.0
    def buildingSize(b : Building) = b.large.?(largeBuilding).|(smallBuilding)

    def at(image : String, size : Double, alpha : Double = 1.0) : ImageRect = ImageRect(new RawImage(img(image)), Rectangle(-size / 2, -size / 2, size, size), alpha)

    // Territory tints: an area's mask (webp2/nort/images/tile/mask/) filled with a colour and turned with its tile, made once
    val tints = scala.collection.mutable.Map[(String, String, Int, String), hrf.ui.sprites.Image]()

    def tint(tile : String, area : String, r : Int, color : String) : |[hrf.ui.sprites.Image] = {
        val key = (tile, area, r % 4, color)

        if (tints.contains(key).not) {
            val m = img("mask-" + tile + "-" + area)

            // Not loaded yet; try again on the next drawing
            if (m.complete.not || m.width == 0)
                return None

            val canvas = dom.document.createElement("canvas").asInstanceOf[dom.html.Canvas]
            canvas.width = m.width
            canvas.height = m.height

            val c = new CanvasImage(canvas)
            val g = c.context
            g.translate(m.width / 2, m.height / 2)
            g.rotate(math.Pi / 2 * (r % 4))
            g.translate(-m.width / 2, -m.height / 2)
            g.drawImage(m, 0, 0)
            g.setTransform(1, 0, 0, 1, 0, 0)
            g.globalCompositeOperation = "source-in"
            g.fillStyle = color
            g.fillRect(0, 0, m.width, m.height)

            // The resource icons and the printed building space frames stay untinted
            // (some masks only partly leave the icons out)
            g.translate(m.width / 2, m.height / 2)
            g.rotate(math.Pi / 2 * (r % 4))
            g.translate(-m.width / 2, -m.height / 2)
            g.scale(m.width, m.height)
            g.globalCompositeOperation = "destination-out"
            g.fillStyle = "black"
            g.beginPath()
            TileIcons.icons.get(tile).|($).foreach { i =>
                g.moveTo(i.x + i.r, i.y)
                g.arc(i.x, i.y, i.r, 0, 2 * math.Pi)
            }
            Tiles.byId(tile).areas./~(_.spaces).foreach { s =>
                if (s.kind == LargeSpace) {
                    val (h, c) = (0.145, 0.065)
                    g.moveTo(s.x - h + c, s.y - h)
                    g.lineTo(s.x + h - c, s.y - h)
                    g.lineTo(s.x + h, s.y - h + c)
                    g.lineTo(s.x + h, s.y + h - c)
                    g.lineTo(s.x + h - c, s.y + h)
                    g.lineTo(s.x - h + c, s.y + h)
                    g.lineTo(s.x - h, s.y + h - c)
                    g.lineTo(s.x - h, s.y - h + c)
                    g.closePath()
                }
                else
                    g.rect(s.x - 0.117, s.y - 0.117, 0.234, 0.234)
            }
            g.fill()
            g.setTransform(1, 0, 0, 1, 0, 0)

            tints(key) = c
        }

        tints.get(key)
    }

    // Tint per player colour: the colour of the player's name, as opaque as the player's Territories setting
    def tintOf(c : PlayerColor) : (String, Double) =
        (c.hex, callbacks.settings.has(CustomTerritoryColor).??(callbacks.settings.collectFirst { case TerritoryOpacity(x, p) if x == c => p }).|(50) / 100.0)

    // Border dashes of the closed territories a player controls, bright enough to show on the dark roads
    def lineOf(c : PlayerColor) : String = c match {
        case Blue => "#3d7bff"
        case Red => "#c8140f"
        case Yellow => "#ffc400"
        case Purple => "#b54ae6"
        case Green => "#2fb52f"
        case Orange => "#ff8a1f"
    }

    // The two lines beside a Rough border's dashes
    val roughRail = "#ffd21f"
    val roughRailOnYellow = "#ffffff"

    // A border's dashes in its sides' colours (alternating when both sides have one), rails beside a Rough one,
    // turned with its tile; in scene units from the tile's top left corner, drawn at half size, made once
    val lineImages = scala.collection.mutable.Map[(String, Int, Int, String, String, |[String]), (hrf.ui.sprites.Image, Rectangle)]()

    def lineImage(tile : String, r : Int, n : Int, ca : String, cb : String, rail : |[String]) : (hrf.ui.sprites.Image, Rectangle) = {
        val key = (tile, r % 4, n, ca, cb, rail)

        if (lineImages.size > 400)
            lineImages.clear()

        lineImages.getOrElseUpdate(key, {
            val line = BorderLines.lines(tile)(n)

            def turn(x : Double, y : Double) = {
                val (rx, ry) = game.board.rotate(x, y, r)
                (rx * T, ry * T)
            }

            // Each dash's centre line, from the end that comes first along the border
            val bars = line.dashes.map(d => ($(turn(d.x0, d.y0), turn(d.x1, d.y1), turn(d.x2, d.y2)), d.width * T))

            def dist(a : (Double, Double), b : (Double, Double)) = math.sqrt((a._1 - b._1) * (a._1 - b._1) + (a._2 - b._2) * (a._2 - b._2))

            val ordered = bars.zipWithIndex.map { case ((ps, w), i) =>
                val prev = (i > 0).?(bars(i - 1)._1(1))
                val next = (i < bars.num - 1).?(bars(i + 1)._1(1))
                val flip = prev./(p => dist(ps(0), p) > dist(ps(2), p)).|(next./(p => dist(ps(2), p) > dist(ps(0), p)).|(false))
                (flip.?(ps.reverse).|(ps), w)
            }

            // The rails follow the dashes, broken where the border has a gap (a lake, a junction)
            val runs = ordered.foldLeft(List[List[(Double, Double)]]()) { case (acc, (ps, _)) =>
                if (acc.none || dist(acc.last.last, ps.head) > 0.06 * T) acc :+ ps
                else acc.dropRight(1) :+ (acc.last ++ ps)
            }

            val offset = 26.0
            val margin = 40.0
            val all = ordered.flatMap(_._1)
            val x0 = all.map(_._1).min - margin
            val y0 = all.map(_._2).min - margin
            val x1 = all.map(_._1).max + margin
            val y1 = all.map(_._2).max + margin
            val k = 0.5

            val canvas = dom.document.createElement("canvas").asInstanceOf[dom.html.Canvas]
            canvas.width = ((x1 - x0) * k).ceil.toInt.max(1)
            canvas.height = ((y1 - y0) * k).ceil.toInt.max(1)

            val c = new CanvasImage(canvas)
            val g = c.context
            g.scale(k, k)
            g.translate(-x0, -y0)
            g.lineJoin = "round"

            // Dotted, with a dark rim so they show on the light art
            rail.foreach { color =>
                g.lineCap = "round"
                g.setLineDash(scalajs.js.Array[Double](0.01, 28))

                runs.foreach { ps =>
                    // Each point moved along the normal of the way through it
                    def side(s : Int) = ps.indices.toList.map { i =>
                        val (ax, ay) = ps((i - 1).max(0))
                        val (bx, by) = ps((i + 1).min(ps.num - 1))
                        val l = math.sqrt((bx - ax) * (bx - ax) + (by - ay) * (by - ay)).max(0.001)
                        (ps(i)._1 - (by - ay) / l * offset * s, ps(i)._2 + (bx - ax) / l * offset * s)
                    }

                    $(1, -1).foreach { s =>
                        val q = side(s)
                        $(("rgba(40,25,0,0.8)", 22.0), (color, 15.0)).foreach { case (stroke, width) =>
                            g.strokeStyle = stroke
                            g.lineWidth = width
                            g.beginPath()
                            g.moveTo(q.head._1, q.head._2)
                            q.drop(1).foreach { case (x, y) => g.lineTo(x, y) }
                            g.stroke()
                        }
                    }
                }

                g.setLineDash(scalajs.js.Array[Double]())
            }

            g.lineCap = "butt"

            ordered.zipWithIndex.foreach { case ((ps, w), i) =>
                // A pixel longer and wider, to cover the art's dash
                val (ax, ay) = ps(0)
                val (bx, by) = ps(2)
                val l = math.sqrt((bx - ax) * (bx - ax) + (by - ay) * (by - ay)).max(0.001)
                val ex = (bx - ax) / l * 1.5
                val ey = (by - ay) / l * 1.5

                g.strokeStyle = (i % 2 == 0).?(ca).|(cb)
                g.lineWidth = w + 2
                g.beginPath()
                g.moveTo(ax - ex, ay - ey)
                g.lineTo(ps(1)._1, ps(1)._2)
                g.lineTo(bx + ex, by + ey)
                g.stroke()
            }

            (c, Rectangle(x0, y0, x1 - x0, y1 - y0))
        })
    }

    // Solid rails beside a wall (an impassable border's orange line), turned with its tile, like lineImage
    def wallImage(tile : String, r : Int, n : Int, rail : String) : (hrf.ui.sprites.Image, Rectangle) = {
        val key = (tile + "/wall", r % 4, n, rail, rail, |(rail))

        if (lineImages.size > 400)
            lineImages.clear()

        lineImages.getOrElseUpdate(key, {
            val runs = BorderLines.walls(tile)(n).runs./(_./ { case (x, y) =>
                val (rx, ry) = game.board.rotate(x, y, r)
                (rx * T, ry * T)
            })

            val offset = 33.0
            val margin = 50.0
            val all = runs.flatten
            val x0 = all.map(_._1).min - margin
            val y0 = all.map(_._2).min - margin
            val x1 = all.map(_._1).max + margin
            val y1 = all.map(_._2).max + margin
            val k = 0.5

            val canvas = dom.document.createElement("canvas").asInstanceOf[dom.html.Canvas]
            canvas.width = ((x1 - x0) * k).ceil.toInt.max(1)
            canvas.height = ((y1 - y0) * k).ceil.toInt.max(1)

            val c = new CanvasImage(canvas)
            val g = c.context
            g.scale(k, k)
            g.translate(-x0, -y0)
            g.lineJoin = "round"
            g.lineCap = "round"

            runs.foreach { ps =>
                // Each point moved along the normal of the way through it
                def side(s : Int) = ps.indices.toList.map { i =>
                    val (ax, ay) = ps((i - 1).max(0))
                    val (bx, by) = ps((i + 1).min(ps.num - 1))
                    val l = math.sqrt((bx - ax) * (bx - ax) + (by - ay) * (by - ay)).max(0.001)
                    (ps(i)._1 - (by - ay) / l * offset * s, ps(i)._2 + (bx - ax) / l * offset * s)
                }

                $(1, -1).foreach { s =>
                    val q = side(s)
                    $(("rgba(40,25,0,0.8)", 16.0), (rail, 10.0)).foreach { case (stroke, width) =>
                        g.strokeStyle = stroke
                        g.lineWidth = width
                        g.beginPath()
                        g.moveTo(q.head._1, q.head._2)
                        q.drop(1).foreach { case (x, y) => g.lineTo(x, y) }
                        g.stroke()
                    }
                }
            }

            (c, Rectangle(x0, y0, x1 - x0, y1 - y0))
        })
    }

    // Invaded, waiting for its fight
    val contestedTint = ("#8c8c8c", 0.45)

    // The fight being resolved, like Root's red cloud
    val battleTint = ("#ff3f9f", 0.42)

    // Free ground for tokens, from TileGrid: the area and clutter (0-9) at a map point in tile units
    def cellAt(fx : Double, fy : Double) : |[(AreaRef, Int)] = {
        val x = math.floor(fx).toInt
        val y = math.floor(fy).toInt

        game.board.at(x, y).flatMap { p =>
            TileGrid.cells.get(p.tile).map { rows =>
                val n = TileGrid.size
                // Back to the unturned tile
                val (lx, ly) = game.board.rotate(fx - x, fy - y, 4 - p.r % 4)
                val ch = rows((ly * n).toInt.clamp(0, n - 1))((lx * n).toInt.clamp(0, n - 1))
                val g = TileGrid.groups.indexWhere(_.contains(ch))
                (AreaRef(x, y, p.spec.areas(g).id), TileGrid.groups(g).indexOf(ch))
            }
        }
    }

    // The centres of a territory's grid cells, in tile units
    def cells(t : Territory) : $[(Double, Double)] = t.areas./~{ a =>
        val p = game.board.at(a.x, a.y).get
        val g = p.spec.areas.indexWhere(_.id == a.id)
        val n = TileGrid.size
        val rows = TileGrid.cells(p.tile)

        0.until(n).$./~(cy => 0.until(n).$.%(cx => TileGrid.groups(g).contains(rows(cy)(cx)))./{ cx =>
            val (x, y) = game.board.rotate((cx + 0.5) / n, (cy + 0.5) / n, p.r)
            (a.x + x, a.y + y)
        })
    }

    // The resource icons on the map (nort/icons.scala): centre and radius, in tile units
    def mapIcons : $[(Double, Double, Double)] = game.board.placements./~{ p =>
        TileIcons.icons.get(p.tile).|($)./{ i =>
            val (x, y) = game.board.rotate(i.x, i.y, p.r)
            (p.x + x, p.y + y, i.r)
        }
    }

    // A resource icon cut from its tile's art, turned with the tile, made once: the part of the circle around it
    // that no area's mask covers (the masks leave the icons out), so it matches the untinted icon exactly;
    // drawn over the pieces so a figure never hides it
    val iconImages = scala.collection.mutable.Map[(String, Int, Int), hrf.ui.sprites.Image]()

    def iconImage(tile : String, r : Int, n : Int) : |[hrf.ui.sprites.Image] = {
        val key = (tile, r % 4, n)

        if (iconImages.contains(key).not) {
            val i = img("tile-" + tile)
            val masks = Tiles.byId(tile).areas./(a => img("mask-" + tile + "-" + a.id))

            // Not loaded yet; try again on the next drawing
            if ((i +: masks).exists(m => m.complete.not || m.width == 0))
                return None

            val icon = TileIcons.icons(tile)(n)
            val (cx, cy) = game.board.rotate(icon.x, icon.y, r)
            val size = (icon.r * T * 2).ceil.toInt

            val canvas = dom.document.createElement("canvas").asInstanceOf[dom.html.Canvas]
            canvas.width = size
            canvas.height = size

            val c = new CanvasImage(canvas)
            val g = c.context
            // The tile's art, or its masks added up, under the circle
            def draw(g : dom.CanvasRenderingContext2D, images : $[dom.html.Image]) {
                g.translate(size / 2 - cx * T, size / 2 - cy * T)
                g.translate(T / 2, T / 2)
                g.rotate(math.Pi / 2 * (r % 4))
                g.translate(-T / 2, -T / 2)
                images.foreach(m => g.drawImage(m, 0, 0, T, T))
                g.setTransform(1, 0, 0, 1, 0, 0)
            }

            draw(g, $(i))

            val covered = dom.document.createElement("canvas").asInstanceOf[dom.html.Canvas]
            covered.width = size
            covered.height = size
            val h = covered.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]
            h.globalCompositeOperation = "lighter"
            draw(h, masks)

            g.globalCompositeOperation = "destination-out"
            g.drawImage(covered, 0, 0)

            val fade = g.createRadialGradient(size / 2, size / 2, 0, size / 2, size / 2, size / 2)
            fade.addColorStop(0, "rgba(0,0,0,1)")
            fade.addColorStop(0.9, "rgba(0,0,0,1)")
            fade.addColorStop(1, "rgba(0,0,0,0)")
            g.globalCompositeOperation = "destination-in"
            g.fillStyle = fade
            g.fillRect(0, 0, size, size)

            iconImages(key) = c
        }

        iconImages.get(key)
    }

    val spots = scala.collection.mutable.Map[Any, (Double, Double)]()

    // Where a round token of radius r (tile units) best fits in a territory: on open ground of that territory,
    // clear of the circles already taken, and, with a pull, not far from a point (a warchief by its clan's units)
    def freeSpot(t : Territory, r : Double, taken : $[(Double, Double, Double)], near : |[(Double, Double)], pull : Double) : (Double, Double) = spots.getOrElseUpdate((t.areas, r, taken, near, pull, game.board.placements.num), {
        val gap = 0.03

        val icons = mapIcons

        // Sample points over the token: its centre, a ring halfway out and a ring near its edge
        val ring = (0.0, 0.0) +: (0.until(6).$./(i => (math.cos(i * math.Pi / 3) * r * 0.5, math.sin(i * math.Pi / 3) * r * 0.5)) ++
            0.until(12).$./(i => (math.cos(i * math.Pi / 6) * r * 0.95, math.sin(i * math.Pi / 6) * r * 0.95)))

        def score(x : Double, y : Double) : Double = {
            // Clutter counts cubed, so a resource icon, building space or number (9) costs far more than busy art;
            // off the map or over another territory costs by far the most, more than covering a piece
            val ground = ring.map { case (dx, dy) =>
                cellAt(x + dx, y + dy) match {
                    case Some((a, c)) if t.areas.contains(a) => (c * c * c).toDouble
                    case _ => 20000.0
                }
            }.sum / ring.num

            // Covering anything already drawn costs the most, with a little gap kept around it
            val overlap = taken.map { case (ox, oy, or) =>
                val d = math.sqrt((x - ox) * (x - ox) + (y - oy) * (y - oy))
                (d < r + or + gap).?(3000 * (1 - d / (r + or + gap))).|(0.0)
            }.sum

            // Covering a resource icon costs more than hanging over another territory
            val covered = icons.map { case (ix, iy, ir) =>
                val d = math.sqrt((x - ix) * (x - ix) + (y - iy) * (y - iy))
                (d < r + ir).?(20000 * (1 - d / (r + ir))).|(0.0)
            }.sum

            val distance = near.map { case (nx, ny) => pull * math.max(0, math.sqrt((x - nx) * (x - nx) + (y - ny) * (y - ny)) - r) }.|(0.0)

            ground + overlap + covered + distance
        }

        cells(t).some./(_.minBy { case (x, y) => score(x, y) }).|(game.board.unitPoint(t.anchor))
    })

    def makeScene() : |[Scene] = {
        // Adset: the map shows from the central tile on, before any clan is drafted
        if ((game.states.none && game.adset.not) || game.board.placements.none)
            return None

        val board = game.board

        // Training Fields: the whole grid, with the tiles still face down
        val (x0, y0, x1, y1) = game.training.?((0, 0, Training.width - 1, Training.height - 1)).|(board.bounds)

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

        game.hiddenTiles.foreach { p =>
            background.add(Sprite($(ImageRect(new RawImage(img("tile-back")), Rectangle(0, 0, T, T), 1.0)), $))(sx(p.x), sy(p.y))
        }

        // Territories in the colour of the player who controls them, gray once invaded, pink while the fight is resolved;
        // the masks leave the resource icons clear
        // (the Territory Color setting can hide the controllers' colours, or all tints)
        val territoryColor = callbacks.settings.has(HideTerritoryColor).not
        val controllerColor = territoryColor && callbacks.settings.has(FightsTerritoryColor).not

        if (territoryColor)
        board.placements.foreach { p =>
            p.spec.areas.foreach { s =>
                val t = board.territory(AreaRef(p.x, p.y, s.id))
                val color =
                    if (game.battle.exists(t.areas.contains)) battleTint
                    else
                    if (game.present(t).num > 1 || game.creatureFights.exists(x => t.areas.contains(x.area))) contestedTint
                    else
                    if (controllerColor)
                        game.present(t).single./(f => tintOf(game.colors(f))).orNull
                    else
                        null

                if (color != null)
                    tint(p.tile, s.id, p.r, color._1).foreach { i =>
                        background.add(Sprite($(ImageRect(i, Rectangle(0, 0, T, T), color._2)), $))(sx(p.x), sy(p.y))
                    }
            }
        }

        // The borders of closed territories that give fame, in their controllers' colours: alternating between two players
        // where two such territories meet, with rails beside a Rough border (white ones when a side is yellow)
        def fameColor(t : Territory) : |[PlayerColor] =
            if (board.closed(t) && game.wolfIn(t).not) game.present(t).single./(game.colors) else None

        // (hidden with the Fame Borders setting)
        if (callbacks.settings.has(HideBorderColor).not)
        board.placements.foreach { p =>
            BorderLines.lines.get(p.tile).|($).zipWithIndex.foreach { case (line, n) =>
                val ca = fameColor(board.territory(AreaRef(p.x, p.y, line.a)))
                // A Beach's shore: the land's colour only ("sea" is no area)
                val cb = if (line.b == "sea") None else fameColor(board.territory(AreaRef(p.x, p.y, line.b)))

                if (line.dashes.any && (ca.any || cb.any)) {
                    val colors = (ca ++ cb).toList
                    val rough = p.spec.borders.exists(b => b.rough && $(b.a, b.b).toSet == $(line.a, line.b).toSet)
                    val rail = rough.?(colors.has(Yellow).?(roughRailOnYellow).|(roughRail))
                    val (i, rect) = lineImage(p.tile, p.r, n, lineOf(colors.head), lineOf(colors.last), rail)
                    background.add(Sprite($(ImageRect(i, rect, 1.0)), $))(sx(p.x), sy(p.y))
                }
            }

            // Solid rails on both sides of a wall when a side is controlled (white ones when a side is yellow)
            BorderLines.walls.get(p.tile).|($).zipWithIndex.foreach { case (wall, n) =>
                val colors = (fameColor(board.territory(AreaRef(p.x, p.y, wall.a))) ++ fameColor(board.territory(AreaRef(p.x, p.y, wall.b)))).toList

                if (wall.runs.any && colors.any) {
                    val (i, rect) = wallImage(p.tile, p.r, n, colors.has(Yellow).?(roughRailOnYellow).|(roughRail))
                    background.add(Sprite($(ImageRect(i, rect, 1.0)), $))(sx(p.x), sy(p.y))
                }
            }
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

        // Buildings on their spaces: a large building fills its (bigger) space
        game.buildings.foreach { case (s, b) =>
            val (x, y) = board.point(s)
            pieces.add(Sprite($(at(b.image, buildingSize(b))), $))(sx(x), sy(y))
        }

        // Free building spaces offered for a building, clickable
        targets.of[SpaceRef].distinct.foreach { s =>
            val (x, y) = board.point(s)
            val n = (s.index < SpaceRef.extra && MapExpansion.spaceKind(s) == LargeSpace).?(largeBuilding).|(smallBuilding)
            pieces.add(Sprite($(at("ui-target", n)), $(Rectangle(-n / 2, -n / 2, n, n)), $(s)))(sx(x), sy(y))
        }

        // Ox Clan's Ancestral Equipment tokens on their spaces, face up
        game.gear.foreach { case (s, n) =>
            val (x, y) = board.point(s)
            pieces.add(Sprite($(at("token-ox-" + n, 170)), $))(sx(x), sy(y))
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

            // Circles (centre and radius, tile units) the warchiefs, Kaija and creatures keep clear of,
            // starting with the territory's numbers and buildings
            var taken : $[(Double, Double, Double)] = t.areas./{ a =>
                val (x, y) = board.point(a)
                (x, y, 0.06)
            } ++ game.buildings.keys.$.%(s => t.areas.contains(s.area))./{ s =>
                val (x, y) = board.point(s)
                (x, y, 0.1)
            }

            def mx(x : Double) = x / T + x0 - 1
            def my(y : Double) = y / T + y0 - 1

            present.zipWithIndex.foreach { case (f, i) =>
                val n = game.count(t, f)
                val ux = sx(px) + (k > 1).?(9 + (i - (k - 1) / 2.0) * step).|(0.0)
                val uy = sy(py) + (k > 1).?(20.0).|(0.0)
                val z = 300 * scale
                if (n > 0) {
                    pieces.add(Sprite($(at(Warchief.warrior(f, t.toString, n), z)), $(Rectangle(-z / 2, -z / 2, z, z)), tag))(ux, uy)
                    if (n <= 15)
                        pieces.add(Sprite($(at("ui-count-" + n, 96 * scale)), $))(ux + 62 * scale, uy + 62 * scale)
                    taken :+= ((mx(ux), my(uy), 0.13 * scale))
                }
            }

            present.zipWithIndex.foreach { case (f, i) =>
                val n = game.count(t, f)
                val ux = sx(px) + (k > 1).?(9 + (i - (k - 1) / 2.0) * step).|(0.0)
                val uy = sy(py) + (k > 1).?(20.0).|(0.0)

                // Whether the figure's spot is used: without units, the first of the warchief and Kaija takes it
                var used = n > 0

                // On open ground a little apart from the clan's figure, or in its place when free
                def beside(r : Double) : (Double, Double) =
                    if (used.not) {
                        used = true
                        (ux, uy)
                    }
                    else {
                        val (x, y) = freeSpot(t, r, taken, |((mx(ux), my(uy))), 8)
                        (sx(x), sy(y))
                    }

                // The warchief (Warchiefs module), the size of a warrior
                if (game.chiefIn(t, f)) {
                    // A figure fills about three quarters of its image, like a warrior's; a round token fills its box, so 230 matches a warrior's height
                    val round = Warchief.round(f)
                    val cz = round.?(230.0).|(300.0) * scale
                    val (cx, cy) = beside(round.?(cz / T / 2).|(0.13 * scale))
                    pieces.add(Sprite($(at(Warchief.figure(f), cz)), $(Rectangle(-cz / 2, -cz / 2, cz, cz)), tag))(cx, cy)
                    taken :+= ((mx(cx), my(cy), round.?(cz / T / 2).|(0.13 * scale)))
                }

                // Bear Clan's Kaija and Lynx Clan's Brundr and Kaelinn; the round token fills its box, so 230 matches a warrior's height
                // Horse Clan's second warchief, Brok, is a round token like Eitria's; the Automa's Leader 2 is its black miniature
                if (game.kaijaIn(t, f)) {
                    val kz = (f == Automa).?(300.0).|(230.0) * scale
                    val image = f match {
                        case Automa => Warchief.leader2
                        case Horse => Warchief.brok
                        case Lynx => "token-lynx"
                        case _ => "token-kaija"
                    }
                    val (kx, ky) = beside((f == Automa).?(0.13 * scale).|(kz / T / 2))
                    pieces.add(Sprite($(at(image, kz)), $(Rectangle(-kz / 2, -kz / 2, kz, kz)), tag))(kx, ky)
                    taken :+= ((mx(kx), my(ky), kz / T / 2))
                }

                // Jötunn Blainn (Wastelands), a round token like Kaija's
                if (game.blainnIn(t, f)) {
                    val bz = 230.0 * scale
                    val (bx, by) = beside(bz / T / 2)
                    pieces.add(Sprite($(at("token-blainn", bz)), $(Rectangle(-bz / 2, -bz / 2, bz, bz)), tag))(bx, by)
                    taken :+= ((mx(bx), my(by), bz / T / 2))
                }
            }

            // Kraken Clan's High Tide token, by the territory number
            if (game.tideIn(t)) {
                val (x, y) = board.point(t.anchor)
                pieces.add(Sprite($(at("token-high-tide", 150)), $(Rectangle(-75, -75, 150, 150)), tag))(sx(x) + 40, sy(y) - 110)
                taken :+= ((x + 40 / T, y - 110 / T, 75 / T))
            }

            // Snake Clan's Scorched Earth token, by the territory number
            if (game.scorchedIn(t)) {
                val (x, y) = board.point(t.anchor)
                pieces.add(Sprite($(at("token-scorched-earth", 150)), $(Rectangle(-75, -75, 150, 150)), tag))(sx(x) - 40, sy(y) + 110)
                taken :+= ((x - 40 / T, y + 110 / T, 75 / T))
            }

            // Creatures on the territory's most open ground, clear of everything else, a little drawn to its first number
            game.creaturesIn(t).foreach { c =>
                val z = 220
                val (x, y) = freeSpot(t, z / T / 2, taken, |(board.point(t.anchor)), 0.5)
                pieces.add(Sprite($(at(c.token, z)), $(Rectangle(-z / 2, -z / 2, z, z)), tag))(sx(x), sy(y))
                taken :+= ((x, y, z / T / 2))
            }
        }

        // Wastelands: Jötunn Blainn waiting at his camp and Naströnd's wood not yet taken, in the impassable middle of their tiles
        if (game.has(Wastelands)) {
            game.jotnarCamp.%(_ => game.blainn.none).foreach { s =>
                pieces.add(Sprite($(at("token-blainn", 230)), $(Rectangle(-115, -115, 230, 230))))(sx(s.x + 0.5), sy(s.y + 0.5))
            }

            game.nastrond.foreach { s =>
                $(-55, 55).foreach { d =>
                    pieces.add(Sprite($(at("token-wood", 140)), $(Rectangle(-70, -70, 140, 140))))(sx(s.x + 0.5) + d, sy(s.y + 0.5))
                }
            }
        }

        // Sea module: each Port's Raid card on its sea, with the raiders on its left (first year) or right (second year)
        game.ports.foreach { p =>
            val r = game.raids(p)
            r.card.foreach { c =>
                val d = South.rotate(board.at(p.x, p.y).get.r)
                val (cx, cy) = (sx(p.x + d.dx + 0.5), sy(p.y + d.dy + 0.5))
                val (w, h) = (680.0, 445.0)
                pieces.add(Sprite($(ImageRect(new RawImage(img(c.info.image)), Rectangle(-w / 2, -h / 2, w, h), 1.0)), $))(cx, cy)

                r.owner.%(_ => r.units > 0).foreach { f =>
                    val z = 230
                    val ux = cx + (r.years == 1).?(-w / 2 - 120).|(w / 2 + 120)
                    pieces.add(Sprite($(at(Warchief.warrior(f, p.toString, r.units), z)), $))(ux, cy)
                    pieces.add(Sprite($(at("ui-count-" + r.units, 80)), $))(ux + 50, cy + 50)
                }
            }
        }

        // The resource icons over everything, so a piece that has to overlap one never hides it
        board.placements.foreach { p =>
            TileIcons.icons.get(p.tile).|($).indices.foreach { n =>
                val icon = TileIcons.icons(p.tile)(n)
                val (x, y) = board.rotate(icon.x, icon.y, p.r)
                val z = (icon.r * T * 2).ceil
                iconImage(p.tile, p.r, n).foreach { i =>
                    pieces.add(Sprite($(ImageRect(i, Rectangle(-z / 2, -z / 2, z, z), 1.0)), $))(sx(p.x + x), sy(p.y + y))
                }
            }
        }

        // The tile being placed: one button just outside each corner, the rotate arrows at the top, the check mark (confirm) and the cross (cancel) at the bottom, on top of everything
        var margins = this.margins
        lastActions./~(_.unwrap.as[TilePreview]).take(1).foreach { p =>
            val (x, y) = (sx(p.spot.x), sy(p.spot.y))
            val d = 120
            def mark(image : String, tag : Any, dx : Double, dy : Double) = pieces.add(Sprite($(at(image, 220)), $(Rectangle(-110, -110, 220, 220)), $(tag)))(x + dx, y + dy)
            if (targets.has(RotateMark(-1)))
                mark("ui-rotate-left", RotateMark(-1), -d, -d)
            if (targets.has(RotateMark(1)))
                mark("ui-rotate-right", RotateMark(1), T + d, -d)
            mark("ui-confirm", ConfirmMark, -d, T + d)
            if (lastActions.of[Cancel].any)
                mark("ui-cancel", CancelMark, T + d, T + d)

            // A tile on the spare row or column at the edge of the scene: widen the margin there so its buttons stay in view
            val out = d + 110 + 10
            margins = Margins(
                margins.left max (out - x),
                margins.top max (out - y),
                margins.right max (x + T + out - sceneWidth),
                margins.bottom max (y + T + out - sceneHeight))
        }

        // The building being confirmed, on its space, with the check mark (confirm) and the cross (cancel) above it, on top of everything
        lastActions./~(_.unwrap.as[BuildPreview]).take(1).foreach { p =>
            val (x, y) = board.point(p.space)
            pieces.add(Sprite($(at(p.building.image, buildingSize(p.building), 0.9)), $))(sx(x), sy(y))
            pieces.add(Sprite($(at("ui-confirm", 220)), $(Rectangle(-110, -110, 220, 220)), $(ConfirmMark)))(sx(x) - 120, sy(y) - 225)
            pieces.add(Sprite($(at("ui-cancel", 220)), $(Rectangle(-110, -110, 220, 220)), $(CancelMark)))(sx(x) + 120, sy(y) - 225)
        }

        |(new Scene($(background, pieces), sceneWidth, sceneHeight, margins))
    }

    // Like the Arcs court: the cards everyone can see, always on top
    val court = newPane("court", Content, styles.strip)

    def stripCard(c : Card) : Elem = OnClick(c, Image(c.info.image, styles.stripCard, xlo.pointer))

    // The tab on the left of the court folds it into a one-line bar (and back), remembered in the browser;
    // the pane under it gets the room (foldCourt)
    def courtKey = Meta.settingsKey + ".court-folded"

    var courtFolded : Boolean = try { hrf.web.Local.get(courtKey, "") == "yes" } catch { case _ : Throwable => false }

    def strip(groups : $[(Elem, $[Elem])]) =
        Div(OnClick(CourtToggle, Div(courtSideways.?("◂").|("▴").txt, styles.stripTab, xlo.pointer)) ~ groups./{ case (title, items) =>
            Div(Div(title, styles.stripTitle) ~ Div(items.any.?(items.merge).|(Div("none".txt, styles.stripEmpty)), styles.stripCards), styles.stripGroup)
        }.merge, styles.stripRow)

    // Folded: the year, then the groups' titles with their card counts, all of it tappable to unfold
    // (sideways: a narrow tab on the left, its text running down)
    def foldedStrip(titles : $[(String, Int)]) = {
        val item = courtSideways.?(styles.stripFoldedSideItem).|(styles.stripFoldedItem)
        // the sideways tab is short: shorter names
        def short(t : String) = t match {
            case "Developments" => "Dev."
            case "Achievements" => "Ach."
            case "Creatures" => "Creat."
            case "Jötunn Blainn" => "Blainn"
            case t => t
        }
        OnClick(CourtToggle, Div(Div(courtSideways.?("▸").|("▾").txt, styles.stripTab) ~ Div(courtSideways.?(("Year " ~ game.year.hlb ~ "/" ~ game.lastYear.hl).spn(item)).|(("Year " ~ game.year.hlb ~ " of " ~ game.lastYear.hl).spn(item)) ~ titles./{ case (t, n) => (courtSideways.?(short(t)).|(t) ~ " " ~ ("(" + n + ")").hl).spn(item) }.merge, courtSideways.?(styles.stripFoldedSideText).|(styles.stripFoldedText)), courtSideways.?(styles.stripFoldedSide).|(styles.stripFolded), xlo.pointer))
    }

    // Your hand and played cards are in the action pane, like in Arcs and Root (Game.info, TurnModeAction)
    def drawCards() {
        if (game.year == 0)
            return

        val last = game.year == game.lastYear

        val developments = last.not.$(("Developments, year " ~ game.year.hlb ~ " of " ~ game.lastYear.hl) -> game.display)

        val achievements = $(("Achievements" ~ last.not.?(", year " ~ game.lastYear.hl).|(Empty)) -> last.?(game.display).|(game.achievements))

        // Creatures module: the creature line, in activation order
        val creatures = (game.has(Creatures) || game.creatureLine.any).$(("Creatures" ~ " (left to right)".spn(xstyles.smaller85)) -> game.creatureLine)

        // Wastelands: Jötunn Blainn's card once the Jötnar Camp is on the map, with where he is
        val blainn = game.jotnarCamp.any.$(("Jötunn Blainn" ~ game.blainn./{ case (f, _) => " (" ~ f.elem ~ ")" }.|(" (at his camp)".txt).spn(xstyles.smaller85)) -> $(BlainnCard))

        // Uncharted Horizons: this year's Event and the next one, and the Alternative victory cards
        val events = game.has(EventsModule).$(("Event" ~ " (this year, next)".spn(xstyles.smaller85)) -> (game.event.$ ++ game.eventDeck.take(1)))

        val victory = game.has(VictoryModule).$(("Victory: " ~ options.has(VictoryModeOption(true)).?("Jarl").|("Thane")) -> game.victory)

        // Solo: the Automa's cards played this year, the last on the right
        val automa = game.setup.has(Automa).$(("Automa" ~ " (played this year)".spn(xstyles.smaller85)) -> game.automaPlayed)

        // Sea module: the Raid cards on the Ports, then the ones kept for the next Harvest or Start of Year
        val raids = (game.has(Sea) && (game.raids.values.exists(_.card.any) || game.raidKept.any)).$(("Raids" ~ " (on the Ports, then kept)".spn(xstyles.smaller85)) -> (game.ports./~(p => game.raids(p).card) ++ game.raidKept./(_.card)))

        val groups = automa ++ events ++ victory ++ developments ++ achievements ++ creatures ++ blainn ++ raids

        if (courtFolded) {
            val short = $(automa -> "Automa", events -> "Event", victory -> "Victory", developments -> "Developments", achievements -> "Achievements", creatures -> "Creatures", blainn -> "Jötunn Blainn", raids -> "Raids")
            val titles = short.flatMap { case (g, t) => g.map(x => t -> x._2.num) }
            court.replaceCached("folded " + courtSideways + " " + game.year + " " + titles, foldedStrip(titles), resources, onClick)
        }
        else
            court.replaceCached((courtSideways, game.year, game.display, game.achievements, game.creatureLine, game.event, game.eventDeck.num, game.victory, game.automaPlayed, game.raids, game.raidKept, game.jotnarCamp, game.blainn./(_._1)).toString, strip(groups./{ case (t, l) => t -> l./(stripCard) }), resources, onClick)
    }

    // The Winter cost chart, with each clan on its row and what f has to pay with
    def winterChart(f : Faction) : Elem = {
        val payers = game.setup.but(Automa).%(game.states.contains)

        // Harsh Winter counts 1 more unit per closed territory
        def counted(g : Faction) = game.states(g).units + game.eventIs("harsh-winter").??(game.controlled(g).%(game.board.closed).num)

        val rows = $((0, 3), (4, 6), (7, 9), (10, 12), (13, 99))

        val cells = rows./{ case (lo, hi) =>
            val (food, wood) = Winter.cost(lo)
            val here = payers.%(g => counted(g) >= lo && counted(g) <= hi)
            val ss = here.has(f).$(styles.winterHere)
            val label = (hi == 99).?((lo + "+ units").txt).|((lo + "–" + hi + " units").txt)
            val cost = (food + wood == 0).?("nothing".txt).|($(food -> Food.elem, wood -> Wood.elem).filter(_._1 > 0).map { case (n, e) => Amount(n.hl, e) }.join(" "))
            val clans = here./(g => g.name.styled(colorOf(g))).join(", ")

            Div(label, styles.winterCell +: ss) ~ Div(cost, styles.winterCell +: ss) ~ Div(clans, styles.winterCell +: ss)
        }

        val state = game.states(f)
        val (food, wood) = EventsExpansion.winterCost(f)
        val next = Harvest.forecast(f)
        val after = (state.food + next.food, state.wood + next.wood)

        val events =
            game.eventIs("harsh-winter").?(("Harsh Winter".hl ~ ": each closed territory counts as 1 more unit").div).|(Empty) ~
            game.eventIs("blizzard").?(("Blizzard".hl ~ ": everyone pays 1 more " ~ Food.elem ~ " and 1 more " ~ Wood.elem).div).|(Empty)

        val short = after._1 < food || after._2 < wood

        ("Winter costs".hlb.div ~
            Div(cells.merge, styles.winterChart) ~
            events ~
            ("Has " ~ Amount(state.food.hl, Food.elem) ~ " " ~ Amount(state.wood.hl, Wood.elem) ~ ", after the harvest " ~ Amount(after._1.hl, Food.elem) ~ " " ~ Amount(after._2.hl, Wood.elem)).div ~
            (f.name.styled(colorOf(f)) ~ " pays " ~ (food + wood == 0).?("nothing".txt).|($(food -> Food.elem, wood -> Wood.elem).filter(_._1 > 0).map { case (n, e) => Amount(n.hl, e) }.join(" ")) ~ " for " ~ counted(f).hl ~ " units during winter").div ~
            short.?(("Not enough without trading: an unpaid Winter gives an " ~ UnrestCard.elem ~ " card").div).|(Empty) ~
            HorizontalBreak ~
            "(tap to close)".spn(xstyles.smaller85).div
        ).div
    }

    // Dragon Clan's Sacrificial Pyre: the token with the units on it drawn on its two circles, in their owners' colors
    def pyreElem(size : Style) : Elem = {
        val slots = $(styles.pyreSlot0, styles.pyreSlot1).zipWithIndex./{ case (s, i) =>
            game.pyre.lift(i)./(g => Image("unit-" + game.colors(g).id, $(s, styles.pyreUnit), |(g.name))).|(Div(Empty, s, styles.pyreEmpty))
        }
        val names = game.pyre.none.?("empty").|(game.pyre./(_.name).mkString(", "))
        Div(Image("token-pyre", styles.pyreImage, "Sacrificial Pyre: " + names) ~ slots.merge, styles.pyre, size)
    }

    def pyreView : Elem = {
        val units = game.pyre.none.?("none".txt).|(game.pyre./(g => g.name.styled(colorOf(g))).join(", "))

        ((Dragon.name.styled(colorOf(Dragon)) ~ " " ~ "Sacrificial Pyre".hl).hlb.div ~
            pyreElem(styles.pyreLarge).div ~
            ("Units on it: " ~ units ~ " (room for " ~ NewBloodExpansion.pyreSize.hl ~ ")").div ~
            CombatText("Enemy casualties go on it after each combat. To harvest, Dragon sacrifices a unit from it (back to its owner) or places one of its deployed units on it.").spn(xstyles.smaller85).div ~
            HorizontalBreak ~
            "(tap to close)".spn(xstyles.smaller85).div
        ).div
    }

    // A player's discard pile, newest card last
    def discardPile(f : Faction) : Elem = cardPile(f, " discard pile", game.states(f).discard)

    def activeCards(f : Faction) : Elem = cardPile(f, " active cards (played this year)", game.states(f).active)

    def cardPile(f : Faction, what : String, l : $[Card]) : Elem = {
        ((f.name.styled(colorOf(f)) ~ what).hlb.div ~
            l.none.?("No cards".txt.div).|(Div(l./(c => Image(c.info.image, styles.discardCard)).merge, styles.discardCards)) ~
            "(tap to close)".spn(xstyles.smaller85).div
        ).div
    }

    // Adset: a player who hasn't drafted a clan yet, in their color, with their seat
    def seatStatus(p : Player) {
        val container = statuses(game.players.indexOf(p))
        val color = elem.styles.get(AdsetExpansion.color(p))
        val label = p.as[Seat]./(_.name).|("")
        val name = resources.getName(p)./(n => n.styled(color)(styles.title) ~ " " ~ label.txt).|(label.styled(color)(styles.title))
        val seat = game.seats.indexOf(p)
        val note = (seat >= 0).?(("Seat " + (seat + 1)).hl ~ ", choosing a clan".txt).|("Seating...".txt)

        container.replace(Div(Div(name), styles.smallname) ~ note.div, resources)

        container.attach.parent.style.outline = game.highlight.faction.has(p).?("2px solid #aaaaaa").|("")
    }

    def factionStatus(f : Faction) {
        val container = statuses(game.players.indexOf(game.ftp(f)))

        // The player's name (human players in online games), then the clan's, in the player's color
        val name = resources.getName(game.ftp(f))./(n => n.styled(colorOf(f))(styles.title) ~ " " ~ f.name.txt).|(f.name.styled(colorOf(f))(styles.title))

        if (!game.states.contains(f)) {
            container.replace(Div(Div(name), styles.smallname, xlo.pointer), resources)
            return
        }

        // Robotos: a tag beside the clan name; tapping it lists the cheats
        val robotos = game.robotos(f).?(" ".txt ~ OnClick(RobotosInfo(f), "(Robotos)".spn(styles.tappable)(xlo.pointer))).|(Empty)

        // The first player marker beside the name
        val first = (game.first == f).?(" ".txt ~ FirstPlayerIcon()).|(Empty)

        val title = Div(Div(name ~ first ~ robotos), styles.smallname, styles.titleLine, xlo.pointer)

        val state = game.states(f)

        // Training Fields: victory points, units, the resources and Monopolies that score when the opponent refreshes, the Action cards
        if (game.training) {
            val (food, wood, lore) = Training.resources(f)
            val vp = (state.fame.hlb ~ " of " ~ Training.goal.hl ~ " VP").div
            val units = (Amount(game.onMap(f).hl, UnitIcon()) ~ " on the map, " ~ game.reserve(f).hl ~ " in reserve").div
            val res = (Amount(food.hl, Food.elem) ~ " " ~ Amount(wood.hl, Wood.elem) ~ " " ~ Amount(lore.hl, Lore.elem) ~ " controlled").div
            val scores = ("Scores " ~ Training.score(f).hl ~ " VP on a Refresh" ~ (Training.monopolies(f) > 0).?(" (" ~ Training.monopolies(f).hl ~ " Monopol" ~ (Training.monopolies(f) > 1).?("ies").|("y") ~ ")").|(Empty)).div
            val cards = ("Cards: ".txt ~ game.drills.get(f)./(_.num).|(0).hl ~ " face up, " ~ (Drill.all.num - game.drills.get(f)./(_.num).|(0)).hl ~ " face down").div
            container.replace((title.div ~ vp ~ units ~ res ~ scores ~ cards).div(styles.statusUpper)(xlo.flexVX)(ExternalStyle("hide-scrollbar")).pointer.onClick.param(f), resources, {
                case x => onClick(x)
            })

            container.attach.parent.style.outline = game.highlight.current.has(f).?("2px solid #aaaaaa").|(game.highlight.faction.has(f).?("2px dashed #aaaaaa").|(""))

            return
        }

        // Each number with its icon: "2 [food]" in a row, with the Stacked setting the icon above the number, with Reverse Stacked the number above the icon
        val reverse = callbacks.settings.has(ReverseStackedPanels)
        val stacked = reverse || callbacks.settings.has(StackedPanels)
        def item(n : Elem, e : Elem) : Elem = stacked.?(reverse.?(n.div ~ e.div).|(e.div ~ n.div).spn(styles.stackedItem)).|(Amount(n, e))
        def row(l : $[Elem]) : Elem = stacked.?(l.merge).|(l.join(" "))

        // Sea module: the units away on Raids
        val raiding = SeaExpansion.raiders(f)
        // Units on the map, the warchief (its token and name, grayed out while in the reserve) and the units left in the supply
        val onMap = item(game.onMap(f).hl ~ (raiding > 0).?(" (" ~ raiding.hl ~ " raiding)").|(Empty), UnitIcon())
        val chief = game.has(Warchiefs).$ {
            val here = game.chiefs.contains(f)
            val token = Image(Warchief.figure(f), styles.inlineIcon).alt(Warchief.name(f))
            val name = here.?(Warchief.elem(f)).|(Warchief.name(f).txt)
            stacked.?(item(name, token)).|(token ~ name).spn(here.?(styles.onBoard).|(styles.offBoard))
        }
        val units = row(onMap +: chief :+ item(game.reserve(f).hl, SupplyIcon())).div

        // Cards to draw, in hand, played this year and discarded; tapping the white card shows the cards played this year, the red one the discards
        val cardRow = row($(
            item(state.draw.num.hl, CardIcon.draw),
            item(state.hand.num.hl, CardIcon.hand),
            OnClick(ActiveCards(f), item(state.active.num.hl, CardIcon.active).spn(xlo.pointer)),
            OnClick(DiscardPile(f), item(state.discard.num.hl, CardIcon.discard).spn(xlo.pointer))
        ))
        val cards = stacked.?(cardRow.div(styles.panelLine)).|(cardRow.div(styles.panelLine)(styles.cardLine))

        // New Blood: Kraken's High Tide tokens, Ox's Ancestral Equipment tokens
        val nb = f match {
            case Kraken => ("High Tide: ".txt ~ (2 - game.tides.num).hl ~ " in reserve").div
            case Ox => ("Equipment: ".txt ~ game.gearReady.num.hl ~ " ready, " ~ game.gearUsed.num.hl ~ " used").div
            case Automa => ("Cards: ".txt ~ game.automaActions.num.hl ~ " to play, " ~ game.automaDeck.num.hl ~ " in its pile").div
            case _ => Empty
        }

        // Team play: the player's team
        // Dragon's Sacrificial Pyre, small, at the start of this line (tapping it shows it full size)
        val pyreMark = (f == Dragon).?(OnClick(PyreView, pyreElem(styles.pyreSmall).spn(xlo.pointer)) ~ " ").|(Empty)

        val marks = (pyreMark ~ game.teams.?(game.teamName(f) ~ " ").|(Empty) ~ (state.passed && game.isOver.not).?(" Passed".txt).|(Empty)).div

        // Alternative victory: one mark per card, in the strip's order: ✓ when fulfilled, the validation count, or ✗
        val goals = game.has(VictoryModule).?(("Victory: ".txt ~ game.victory./{ c =>
            if (VictoryExpansion.fulfilled(f, c)) "✓".styled(styles.fame)
            else c.target./(n => game.progressOf(f, c.id).hl ~ "/" ~ n.toString).|("✗".txt)
        }.join(" ")).div).|(Empty)

        // The resources and fame, and under them the next harvest as things stand and the Winter costs, one column each (tapping them shows
        // the whole Winter chart). Compact: "2 [food]" in each cell; Stacked: the icons head the columns; Reverse Stacked: they go under the stockpile
        val next = Harvest.forecast(f)
        val (winterFood, winterWood) = EventsExpansion.winterCost(f)
        // The columns: the icon, the stockpile, the harvest's gain and the Winter cost
        case class Column(icon : Elem, has : Int, gain : Int, cost : Int)
        val columns = $(
            Column(Food.elem, state.has(Food), next.food, winterFood),
            Column(Wood.elem, state.has(Wood), next.wood, winterWood),
            Column(Lore.elem, state.has(Lore), next.lore, 0),
            Column(FameIcon(), state.fame, next.fame, 0))
        // Dragon Clan: 1 more food or wood for the sacrifice, on a row of its own under the harvest's
        val pyre = f == Dragon && game.dragonHarvest.has(false).not && (game.dragonHarvest.any || NewBloodExpansion.sacrificeOptions(f)) && game.controlled(f).any

        def cell(e : Elem) : Elem = e.div(styles.ledgerCell)
        def none : Elem = cell("–".spn(styles.ledgerNone))
        // The stockpile with its icons in the compact panels; the harvest and Winter numbers alone, under them, green and red
        def signed(n : Int, sign : String, s : Style) : Elem = (n > 0).?(cell((sign + n).styled(s))).|(none)

        val icons = cell(Empty) ~ columns./(c => cell(c.icon)).merge
        val stock = cell(Empty) ~ columns./(c => cell(stacked.?(c.has.hl).|(Amount(c.has.hl, c.icon)))).merge
        val harvest = cell(SeasonIcon.harvest) ~ columns./(c => signed(c.gain, "+", styles.ledgerGain)).merge ~
            pyre.?(Amount("+1".styled(styles.ledgerGain), Food.elem ~ "/" ~ Wood.elem).div(styles.ledgerExtra)).|(Empty)
        val winter = (f != Automa).?(cell(SeasonIcon.winter) ~ columns./(c => signed(c.cost, "-", styles.ledgerLoss)).merge).|(Empty)

        val rows = stacked.?(reverse.?(stock ~ icons ~ harvest ~ winter).|(icons ~ stock ~ harvest ~ winter)).|(stock ~ harvest ~ winter)
        val grid = stacked.?(rows.div(styles.ledger)).|(rows.div(styles.ledger)(styles.ledgerCompact))
        val ledger = (f != Automa).?(OnClick(WinterChart(f), grid.div(xlo.pointer))).|(grid)

        val content = (title.div ~ cards ~ units ~ ledger ~ nb ~ goals ~ marks).div(styles.statusUpper)(xlo.flexVX)(ExternalStyle("hide-scrollbar")).pointer.onClick.param(f)

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
            game.ptf.get(game.players(n))./(factionStatus).|(seatStatus(game.players(n)))
        }

        if (overlayPane.visible)
            overlayPane.vis()
        else
            overlayPane.invis()

        drawMap()

        drawCards()

        refitFolded()
    }

    val layoutZoom = 0.49 * 0.88

    val kkk = 1.18 / 30

    val layouts = $(Layout("base",
        $(
            BasicPane("status", 15, (arity >= 4).?(24).|(18), Priorities(top = 3, right = 2, maxXscale = 1.8, maxYscale = 1.8, grow = 1)),
            BasicPane("court", 80, 20, Priorities(top = 3, left = 3, maxXscale = 1.5, maxYscale = 1.5, grow = -2)),
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

    // With the shared cards on the top left and the player panels on the top right, the log and the
    // action pane start where the player panels do, so the right column lines up with them; the map
    // below the shared cards gives up the width.
    def alignRightColumn(ff : $[Fit]) : $[Fit] = {
        def get(name : String) = ff.find(_.name == name)

        (get("status-horizontal"), get("court"), get("map-small"), get("log"), get("action")) match {
            case (Some(s), Some(c), Some(m), Some(l), Some(a))
                if s.y < c.bottom && c.right <= s.x && m.y >= c.bottom - 2 && m.x < s.x &&
                    l.y >= s.bottom - 2 && a.y >= s.bottom - 2 && l.x >= m.right - 2 && a.x >= m.right - 2 &&
                    s.x < min(l.x, a.x) =>
                ff./{
                    case f if f.name == "log" || f.name == "action" => f.copy(x = s.x, width = f.right - s.x)
                    case f if f.name == "map-small" => f.copy(width = s.x - f.x)
                    case f => f
                }
            case _ => ff
        }
    }

    val layouter = Layouter(layouts,
    // The map on the left; the player panels, the log and the action pane (choices and hand) on the right
    alignRightColumn,
    _./~{
        case f if f.name == "action" => $(f, f.copy(name = "undo"), f.copy(name = "settings"))
        case f if f.name == "status-horizontal" => 1.to(arity)./(n => f.copy(name = "status-" + n, x = f.x + ((n - 1) * f.width  /~/ arity), width  = (n * f.width  /~/ arity) - ((n - 1) * f.width  /~/ arity)))
        case f if f.name == "status-vertical"   => 1.to(arity)./(n => f.copy(name = "status-" + n, y = f.y + ((n - 1) * f.height /~/ arity), height = (n * f.height /~/ arity) - ((n - 1) * f.height /~/ arity)))
        case f => $(f)
    },
    // The overlay (zoomed cards, notifications, dialogs) covers the map, as in Root
    ff => ff ++ ff.%(_.name == "map-small")./(_.copy(name = "map-small-overlay")))

    val settingsKey = Meta.settingsKey

    val layoutKey = "v" + 12 + "." + "arity-" + arity

    // What the player panels' contents need, in em: the widest row and the rows' total height, measured on the
    // panels as they are drawn now; None when they aren't drawn yet, or their icons are still loading (an icon
    // takes no room until it has loaded)
    def panelNeed(names : $[String]) : Option[(Double, Double)] = {
        val uppers = names./~(n => panes.get(n)./~(c => Option(c.node.asInstanceOf[dom.html.Element].querySelector("[class^=nort-status-upper]").asInstanceOf[dom.html.Element])))

        def loading(upper : dom.html.Element) = {
            val images = upper.querySelectorAll("img")
            0.until(images.length).exists(i => images(i).asInstanceOf[dom.html.Image].complete.not)
        }

        val sizes = uppers./~ { upper =>
            try {
                val em = dom.window.getComputedStyle(upper).fontSize.replace("px", "").toDouble
                val rows = 0.until(upper.children.length)./(i => upper.children(i).asInstanceOf[dom.html.Element])
                val widths = rows./ { e =>
                    val range = scalajs.js.Dynamic.global.document.createRange()
                    range.selectNodeContents(e)
                    range.getBoundingClientRect().width.asInstanceOf[Double]
                }
                (em > 0 && rows.any).?(widths.max / em -> rows./(_.offsetHeight.toDouble).sum / em)
            }
            catch { case _ : Throwable => None }
        }

        (sizes.any && uppers.exists(loading).not).?((sizes.map(_._1).max, sizes.map(_._2).max))
    }

    // A pane's font size is a percentage of the font of the element the panes are in (not always 16px, and
    // when a game loads it can change after the first layout)
    def paneBasePx() : Double = panes.get("court")./~(c => try {
        Some(dom.window.getComputedStyle(c.node.asInstanceOf[dom.html.Element].parentElement).fontSize.replace("px", "").toDouble).filter(_ > 0)
    } catch { case _ : Throwable => None }).|(16.0)

    def statusNames = 1.to(arity)./("status-" + _)

    // The needs and font the folded layout was made for. The panels are often drawn after it (when a game loads,
    // or when a warchief or a goal adds a row), so updateStatus lays them out again when either changes
    var foldBasePx = 0.0
    var foldNeed : Option[(Double, Double)] = None
    var refitting = false

    // The icons load after the panels are drawn: once they have, the panels are measured again
    var refitTimer : Option[Int] = None

    dom.document.addEventListener("load", (e : dom.Event) => {
        if (courtFolded && e.target.isInstanceOf[dom.html.Image] && statusNames.exists(n => panes.get(n).exists(_.node.contains(e.target.asInstanceOf[dom.Node])))) {
            refitTimer.foreach(dom.window.clearTimeout)
            refitTimer = Some(dom.window.setTimeout(() => { refitTimer = None; refitFolded() }, 50))
        }
    }, true)

    def refitFolded() {
        if (courtFolded && foldBasePx > 0 && refitting.not) {
            val now = panelNeed(statusNames)
            val stale = (now, foldNeed) match {
                case (Some((w1, h1)), Some((w0, h0))) => (w1 - w0).abs > 0.3 || (h1 - h0).abs > 0.3
                case (Some(_), None) => true
                case _ => false
            }
            if (stale || (paneBasePx() - foldBasePx).abs > 0.5) {
                refitting = true
                try resize() finally refitting = false
            }
        }
    }

    // Folded, the court shrinks to a one-line bar. When the player panels are right above it (ultrawide and
    // phone layouts), they take the room it gave, the bar goes under them, and whatever room they don't need
    // goes to the pane below (the map or the action pane); otherwise the pane below grows up into the room.
    // Either way the panels are laid out again in their area (plus that room): in one row or several (say 2x2
    // for four narrow phone panels), whichever lets their font grow the most while their contents (measured)
    // still fit, each as tall as its contents need. When even their present font doesn't fit (four panels in
    // a 16:9 or tablet layout), the arrangement that needs the least shrinking is used. Nothing else moves
    def foldCourt(height : Int)(l : $[PanePlacement]) : $[PanePlacement] = {
        courtSideways = sideways(l)
        foldCourt0(height)(l)
    }

    // Whether the court is at the top with the player panels in a row right of it (16:9 and 16:10 layouts)
    def sideways(l : $[PanePlacement]) : Boolean = l.find(_.name == "court").exists { c =>
        val pp = l.filter(_.name.startsWith("status-"))
        pp.any && pp.forall(p => (p.rect.y - c.rect.y).abs <= 3) && (pp./(_.rect.x).min - (c.rect.x + c.rect.width)).abs <= 3
    }

    var courtSideways = false

    def foldCourt0(height : Int)(l : $[PanePlacement]) : $[PanePlacement] = l.find(_.name == "court") match {
        case Some(c) if courtFolded =>
            val r = c.rect
            val basePx = paneBasePx()
            foldBasePx = basePx
            def px(p : PanePlacement) = p.fontSize./(f => basePx * f * 3612 / height / 100).|(basePx)
            val bar = min(r.height, (px(c) * 1.9).round.toInt)
            val gain = r.height - bar

            def within(p : PanePlacement) = p.rect.x >= r.x - 3 && p.rect.x + p.rect.width <= r.x + r.width + 3
            def below(p : PanePlacement) = (p.rect.y - (r.y + r.height)).abs <= 3 && within(p)

            // The player panels, when they are in one row
            val panels : $[PanePlacement] = l.filter(_.name.startsWith("status-")).sortBy(_.name.drop(7).toIntOption.getOrElse(0)) match {
                case pp if pp.any && pp.forall(p => p.rect.y == pp.head.rect.y && p.rect.height == pp.head.rect.height) => pp
                case _ => $
            }
            val top = panels./(_.rect.y).minOption.|(0)
            val left = panels./(_.rect.x).minOption.|(0)
            val w = panels./(p => p.rect.x + p.rect.width).maxOption.|(0) - left
            val h0 = panels./(_.rect.height).maxOption.|(0)
            val above = panels.any && (top + h0 - r.y).abs <= 3 && left >= r.x - 3 && left + w <= r.x + r.width + 3

            // What the panels' contents need, in em (about 12 by 8 when they aren't drawn yet)
            foldNeed = panelNeed(statusNames).orElse(foldNeed)
            val (needW, needH) = foldNeed.|((12.0, 8.0))

            // The panels laid out again in `room` pixels of height (and `w` of width from `left`); returns them and the height they use
            def regrid(room : Int, left : Int = left, w : Int = w) : (Map[String, PanePlacement], Int) = {
                val f0 = px(panels.head)
                val n = panels.num
                val cellH = f0 * (needH * 1.15 + 0.6)

                // For each number of rows: how much the font can grow (k), limited by the width of a column and by the height
                def fit(rows : Int) = {
                    val cols = (n + rows - 1) / rows
                    min(w * 1.0 / cols / (f0 * (needW * 1.1 + 0.6)), room * 1.0 / rows / cellH)
                }
                val (rows, k) = 1.to(n)./(rows => rows -> fit(rows)).maxBy { case (rows, k) => (k, -rows) }

                // Never smaller than now, unless they don't fit now either
                val (rr, kk) = (k < 1 && fit(1) >= 1).?((1, 1.0)).|((rows, k))
                val cols = (n + rr - 1) / rr
                val ch = (kk < 1).?(room / rr).|(min(room / rr, max((cellH * kk).ceil.toInt, (rr == 1).?(h0).|(0))))

                panels.zipWithIndex./{ case (p, i) =>
                    val (row, col) = (i / cols, i % cols)
                    // a short last row is centered
                    val inRow = (row == rr - 1).?(n - row * cols).|(cols)
                    val shift = (cols - inRow) * w / cols / 2
                    val x0 = left + shift + col * w / cols
                    val x1 = left + shift + (col + 1) * w / cols
                    p.name -> p.copy(rect = Rect(x0, top + row * ch, x1 - x0, ch), fontSize = p.fontSize./(_ * kk))
                }.toMap -> ch * rr
            }

            if (courtSideways) {
                // The court is left of the panels at the top (16:9 and 16:10): folded, it is a narrow tab on the
                // left, the panels run across the top from it, and the panes under both start under them
                // wide enough for two columns of the tab's text, running down
                val tab = (bar * 1.7).round.toInt
                val (placed, used) = regrid(h0, r.x + tab, left + w - r.x - tab)
                val bandY = r.y + used
                def under(p : PanePlacement) = ((p.rect.y - (r.y + r.height)).abs <= 3 || (p.rect.y - (top + h0)).abs <= 3) &&
                    p.rect.x >= r.x - 3 && p.rect.x + p.rect.width <= left + w + 3
                l./{
                    case p if p.name == "court" => p.copy(rect = Rect(r.x, r.y, tab, used))
                    case p if placed.contains(p.name) => placed(p.name)
                    case p if under(p) => p.copy(rect = Rect(p.rect.x, bandY, p.rect.width, p.rect.y + p.rect.height - bandY))
                    case p => p
                }
            }
            else if (above) {
                val (placed, used) = regrid(h0 + gain)
                val barY = top + used
                l./{
                    case p if p.name == "court" => p.copy(rect = Rect(r.x, barY, r.width, bar))
                    case p if placed.contains(p.name) => placed(p.name)
                    case p if below(p) => p.copy(rect = Rect(p.rect.x, barY + bar, p.rect.width, p.rect.y + p.rect.height - barY - bar))
                    case p => p
                }
            }
            else {
                val placed = panels.any.?(regrid(h0)._1).|(Map[String, PanePlacement]())
                l./{
                    case p if p.name == "court" => p.copy(rect = Rect(r.x, r.y, r.width, bar))
                    case p if placed.contains(p.name) => placed(p.name)
                    case p if below(p) => p.copy(rect = Rect(p.rect.x, r.y + bar, p.rect.width, p.rect.height + gain))
                    case p => p
                }
            }
        case _ => l
    }

    // Ultrawide screens (21:9 and wider): the layouter stretches the player panels to the full height
    // and leaves empty space around the map, so the hand gets a narrow column. Instead: the map on the
    // left, then short player panels in a row, the shared cards below them and the hand filling the
    // rest, wide enough to show the whole hand, and the log on the right.
    override def layout(width : Int, height : Int)(onLayout0 : $[PanePlacement] => Unit) {
        val onLayout = onLayout0.compose(foldCourt(height))

        val root = dom.document.documentElement.asInstanceOf[dom.html.Element].style

        if (width < height * 2.1) {
            root.removeProperty("--nort-hand-card")
            return super.layout(width, height)(onLayout)
        }

        val fontSize = height / 34.0

        val logW = (width * 0.16).round.toInt
        val mapW = min(width * 0.40, height * 1.1).round.toInt
        val rightX = mapW
        val rightW = width - mapW - logW

        // Room for six lines in the player panels, in a smaller font (their text lines are short)
        val statusH = (height * 0.19).round.toInt
        val courtH = min(height * 0.22, rightW / 5.5).round.toInt
        val actionY = statusH + courtH

        def place(name : String, x : Int, y : Int, w : Int, h : Int) = PanePlacement(name, Rect(x, y, w, h), Some(fontSize))

        // Hand cards (styles.handCard) sized so five or six fit in a row
        root.setProperty("--nort-hand-card", (rightW / 5.8).round + "px")

        val action = place("action", rightX, actionY, rightW, height - actionY)

        onLayout(
            1.to(arity)./(n => place("status-" + n, rightX + (n - 1) * rightW / arity, 0, n * rightW / arity - (n - 1) * rightW / arity, statusH).copy(fontSize = Some(fontSize * 0.8))) ++
            $(
                place("court", rightX, statusH, rightW, courtH),
                action, action.copy(name = "undo"), action.copy(name = "settings"),
                place("log", rightX + rightW, 0, logW, height),
                place("map-small", 0, 0, mapW, height),
                place("map-small-overlay", 0, 0, mapW, height)
            )
        )
    }

    def overlayScrollX(e : Elem) = overlayScroll(e)(styles.seeThroughInner).onClick
    def overlayFitX(e : Elem) = overlayFit(e)(styles.seeThroughInner).onClick

    def showOverlay(e : Elem, onClick : Any => Unit) {
        overlayPane.vis()
        overlayPane.replace(e, resources, onClick, _ => {}, _ => {})
    }

    override def onClick(a : Any) = a @@ {
        case Some(x) => onClick(x)

        case CourtToggle =>
            courtFolded = courtFolded.not
            try { hrf.web.Local.set(courtKey, courtFolded.?("yes").|("")) } catch { case _ : Throwable => }
            resize()

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

        case ClanBoard(f) =>
            showOverlay(overlayFitX(Image(Warchief.board(f), styles.zoomCard)).onClick, onClick)

        case WinterChart(f) =>
            showOverlay(overlayScrollX(winterChart(f)).onClick, onClick)

        case RobotosInfo(f) =>
            showOverlay(overlayScrollX((f.elem ~ " is played by " ~ "Robotos".hl ~ ", the Hard bot with cheats:").div ~ RobotosExpansion.cheats./(_.txt.div).merge).onClick, onClick)

        case DiscardPile(f) =>
            showOverlay(overlayScrollX(discardPile(f)).onClick, onClick)

        case ActiveCards(f) =>
            showOverlay(overlayScrollX(activeCards(f)).onClick, onClick)

        case PyreView =>
            showOverlay(overlayScrollX(pyreView).onClick, onClick)

        // A fight's result tapped in the log
        case FightReportView(r) =>
            showOverlay(overlayScrollX(("Combat report".hlb.div ~ CombatReport.text(r, None).div)).onClick, onClick)

        // Adset: a drafted clan's info, as behind the clan picker's i button
        case AdsetClanInfo(f) =>
            Meta.factionInfo(f).foreach { case (_, title, l) => showOverlay(overlayScrollX((title.div ~ l./(e => Div(e)).merge).div(xlo.flexvcenter)).onClick, onClick) }

        case Nil =>
            clearOverlay()

        case Left(x) => onClick(x)
        case Right(x) => onClick(x)

        // A card tapped in the log comes with the log line's own parameter stripped off
        case List(x) => onClick(x)

        case x =>
            println("unknown onClick: " + x)
    }

    def clearOverlay() {
        overlayPane.invis()
        overlayPane.clear()
    }

    override def info(self : |[F], aa : $[UserAction]) = {
        val ii = currentGame.info($, self, aa)
        ii.any.??($(ZOption(Empty, Break)) ++ convertActions(self, ii)) ++
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

        // The "Combat Report" setting on Skip: the report's only choice, OK, is taken for the player
        if (callbacks.settings.has(SkipCombatReport) && actions.exists(_.unwrap.is[CombatReportDoneAction])) {
            scalajs.js.timers.setTimeout(0) { then(actions.find(_.unwrap.is[CombatReportDoneAction]).get) }
            return
        }

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

    // Adset: a clan's "i" button opens its info instead of choosing it
    override def convertActions(faction : |[F], actions : $[UserAction], then : UserAction => Unit = null) : $[ZOption] =
        if (actions.exists(_.is[AdsetDraftChoice]).not || then == null)
            super.convertActions(faction, actions, then)
        else
            actions./~(a => super.convertActions(faction, $(a), then)./(z => a @@ {
                case d : AdsetDraftChoice => z.copy(clear = false, click = {
                    case AdsetClanInfo(f) => onClick(AdsetClanInfo(f))
                    case x => z.click(x)
                })
                case _ => z
            }))

    override def styleAction(faction : |[F], actions : $[UserAction], a : UserAction, unavailable : Boolean, view : |[Any]) : $[Style] =
        if (a.is[AdsetDraftChoice])
            $(xstyles.choice, xstyles.xx, xstyles.chp, xstyles.factionTile, styles.draftTile, xlo.pointer)
        else
        // Adset: the seat's three map tiles side by side
        if (a.is[AdsetTileInfoAction])
            $(xstyles.info, xstyles.xx, xstyles.chp, styles.draftTile)
        else
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
            case Some(p) if game.ptf.contains(p) => $(elem.borders.get(game.ptf(p)))
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
