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

            tints(key) = c
        }

        tints.get(key)
    }

    // Tint per player colour: yellow and green strong, red and blue medium to light
    def tintOf(c : PlayerColor) : (String, Double) = c match {
        case Blue => ("#1f5fe0", 0.24)
        case Red => ("#a3100c", 0.34)
        case Yellow => ("#ffc400", 0.55)
        case Purple => ("#9b30c8", 0.3)
        case Green => ("#147a14", 0.55)
        case Orange => ("#ff7a1a", 0.4)
    }

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

            val offset = 15.0
            val margin = 30.0
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

            rail.foreach { color =>
                g.strokeStyle = color
                g.lineWidth = 4
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
                        g.beginPath()
                        g.moveTo(q.head._1, q.head._2)
                        q.drop(1).foreach { case (x, y) => g.lineTo(x, y) }
                        g.stroke()
                    }
                }
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

    val spots = scala.collection.mutable.Map[Any, (Double, Double)]()

    // Where a round token of radius r (tile units) best fits in a territory: on open ground of that territory,
    // clear of the circles already taken, and, with a pull, not far from a point (a warchief by its clan's units)
    def freeSpot(t : Territory, r : Double, taken : $[(Double, Double, Double)], near : |[(Double, Double)], pull : Double) : (Double, Double) = spots.getOrElseUpdate((t.areas, r, taken, near, pull, game.board.placements.num), {
        val gap = 0.03

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

            val distance = near.map { case (nx, ny) => pull * math.max(0, math.sqrt((x - nx) * (x - nx) + (y - ny) * (y - ny)) - r) }.|(0.0)

            ground + overlap + distance
        }

        cells(t).some./(_.minBy { case (x, y) => score(x, y) }).|(game.board.unitPoint(t.anchor))
    })

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

        // Territories in the colour of the player who controls them, gray once invaded, pink while the fight is resolved;
        // the masks leave the resource icons clear
        board.placements.foreach { p =>
            p.spec.areas.foreach { s =>
                val t = board.territory(AreaRef(p.x, p.y, s.id))
                val color =
                    if (game.battle.exists(t.areas.contains)) battleTint
                    else
                    if (game.present(t).num > 1 || game.creatureFights.exists(x => t.areas.contains(x.area))) contestedTint
                    else
                        game.present(t).single./(f => tintOf(game.colors(f))).orNull

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

        board.placements.foreach { p =>
            BorderLines.lines.get(p.tile).|($).zipWithIndex.foreach { case (line, n) =>
                val ca = fameColor(board.territory(AreaRef(p.x, p.y, line.a)))
                val cb = fameColor(board.territory(AreaRef(p.x, p.y, line.b)))

                if (line.dashes.any && (ca.any || cb.any)) {
                    val colors = (ca ++ cb).toList
                    val rough = p.spec.borders.exists(b => b.rough && $(b.a, b.b).toSet == $(line.a, line.b).toSet)
                    val rail = rough.?(colors.has(Yellow).?(roughRailOnYellow).|(roughRail))
                    val (i, rect) = lineImage(p.tile, p.r, n, lineOf(colors.head), lineOf(colors.last), rail)
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

        // Buildings on their spaces
        game.buildings.foreach { case (s, b) =>
            val (x, y) = board.point(s)
            pieces.add(Sprite($(at(b.image, 190)), $))(sx(x), sy(y))
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
                    pieces.add(Sprite($(at("unit-" + game.colors(f).id, z)), $(Rectangle(-z / 2, -z / 2, z, z)), tag))(ux, uy)
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
                    // The figure fills about three quarters of its image, like a warrior's
                    val cz = 300 * scale
                    val (cx, cy) = beside(0.13 * scale)
                    pieces.add(Sprite($(at("warchief-" + game.colors(f).id, cz)), $(Rectangle(-cz / 2, -cz / 2, cz, cz)), tag))(cx, cy)
                    taken :+= ((mx(cx), my(cy), 0.13 * scale))
                }

                // Bear Clan's Kaija and Lynx Clan's Brundr and Kaelinn; the round token fills its box, so 230 matches a warrior's height
                // Horse Clan's second warchief, Brok, is a warchief figure
                if (game.kaijaIn(t, f)) {
                    val kz = (f == Horse).?(300.0).|(230.0) * scale
                    val image = f match {
                        case Horse => "warchief-" + game.colors(f).id
                        case Lynx => "token-lynx"
                        case _ => "token-kaija"
                    }
                    val (kx, ky) = beside((f == Horse).?(0.13 * scale).|(kz / T / 2))
                    pieces.add(Sprite($(at(image, kz)), $(Rectangle(-kz / 2, -kz / 2, kz, kz)), tag))(kx, ky)
                    taken :+= ((mx(kx), my(ky), kz / T / 2))
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

        // Warchiefs module: the warchief's name, dimmed while in the reserve
        val chief = game.has(Warchiefs).?(game.chiefs.contains(f).?(Warchief.elem(f)).|(Warchief.name(f).txt ~ " (reserve)".spn(xstyles.smaller85)).div).|(Empty)

        val cards = (state.hand.num.hl ~ " in hand, " ~ state.draw.num.hl ~ " to draw").div

        // New Blood: Dragon's Sacrificial Pyre, Kraken's High Tide tokens, Ox's Ancestral Equipment tokens
        val nb = f match {
            case Dragon => ("Pyre: ".txt ~ game.pyre.none.?("empty".txt).|(game.pyre./(g => (g == Dragon).?("own".txt).|(g.name.styled(colorOf(g)))).join(", "))).div
            case Kraken => ("High Tide: ".txt ~ (2 - game.tides.num).hl ~ " in reserve").div
            case Ox => ("Equipment: ".txt ~ game.gearReady.num.hl ~ " ready, " ~ game.gearUsed.num.hl ~ " used").div
            case _ => Empty
        }

        // Team play: the player's team
        val marks = (game.teams.?(game.teamName(f) ~ " ").|(Empty) ~ (game.first == f).?("First player".hh).|(Empty) ~ (state.passed && game.isOver.not).?(" Passed".txt).|(Empty)).div

        val content = (title.div ~ res ~ units ~ chief ~ cards ~ nb ~ marks).div(styles.statusUpper)(xlo.flexVX)(ExternalStyle("hide-scrollbar")).pointer.onClick.param(f)

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
    // The overlay (zoomed cards, notifications, dialogs) covers the map, as in Root
    ff => ff ++ ff.%(_.name == "map-small")./(_.copy(name = "map-small-overlay")))

    val settingsKey = Meta.settingsKey

    val layoutKey = "v" + 9 + "." + "arity-" + arity

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
