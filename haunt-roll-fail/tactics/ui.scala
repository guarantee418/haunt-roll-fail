package tactics
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

import tactics.elem._

import hrf.ui.again._
import hrf.ui.sprites._


object UI extends BaseUI {
    val mmeta = Meta

    def create(uir : ElementAttachmentPoint, arity : Int, options : $[mmeta.O], resources : hrf.html.Resources, title : String, callbacks : hrf.Callbacks) = new UI(uir, arity, resources, callbacks)
}

// a unit's token on the map, tapped
case class UnitTag(cell : Cell, unit : String)

// The map is drawn in code, no images: the board once into a canvas (terrain, cover, sectors, zones, hex names),
// then every redraw the highlights, who holds each sector, and the units as tokens (canvases kept by their look).
class UI(val uir : ElementAttachmentPoint, arity : Int, val resources : hrf.html.Resources, callbacks : hrf.Callbacks) extends MapGUI {
    def factionElem(f : Faction) = f.name.styled(f)

    lazy val narrow = dom.window.innerWidth < 700

    val statuses = 1.to(arity)./(i => newPane("status-" + i, Content, styles.status, styles.fstatus, ExternalStyle("hide-scrollbar")))
    val statusGame = newOuterPane("status-game", Content)

    // GEOMETRY: pointy-top hexes of radius S, odd rows shifted half a hex right

    val S = 60.0
    val W = math.sqrt(3) * S
    val margin = 24.0

    def colOf(c : Cell) = Skirmish.column(c.hex)

    def x(c : Cell) = margin + W / 2 + W * (colOf(c) + 0.5 * (c.r & 1))
    def y(c : Cell) = margin + S + 1.5 * S * c.r

    val width = (margin * 2 + W * 13).ceil
    val height = (margin * 2 + 2 * S + 1.5 * S * (Skirmish.rows - 1)).ceil
    val margins = Margins(0, 0, 0, 0)

    lazy val cells : $[Cell] = Skirmish.board.all./(t => Cell(t.hex)).sortBy(c => (c.r, c.q))

    object regions extends Regions[Cell] {
        def center(c : Cell) = XY(x(c), y(c))

        def regionAt(p : XY) : |[Cell] = {
            val c = cells.minBy(c => (x(c) - p.x) * (x(c) - p.x) + (y(c) - p.y) * (y(c) - p.y))
            val d = math.sqrt((x(c) - p.x) * (x(c) - p.x) + (y(c) - p.y) * (y(c) - p.y))
            (d < S * 0.95).?(c)
        }

        def random(c : Cell) : XY = XY(x(c), y(c))
    }

    def color(f : Faction) = f match {
        case Red => "#e0453c"
        case Blue => "#6f8dff"
    }

    def terrainColor(t : tactics.Terrain) = t match {
        case tactics.Plains => "#a3b46c"
        case tactics.Forest => "#4e7a3c"
        case tactics.Hills => "#b79c66"
        case tactics.Water => "#4a7fa8"
        case tactics.Mountain => "#78706a"
        case tactics.Building => "#a9a49a"
    }

    def newCanvas(w : Int, h : Int) = {
        val canvas = dom.document.createElement("canvas").asInstanceOf[dom.html.Canvas]
        canvas.width = w
        canvas.height = h
        (canvas, canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D])
    }

    def hexPath(g : dom.CanvasRenderingContext2D, cx : Double, cy : Double, r : Double) {
        g.beginPath()
        0.until(6).foreach { i =>
            val a = math.Pi / 180 * (60 * i - 30)
            val px = cx + r * math.cos(a)
            val py = cy + r * math.sin(a)
            if (i == 0) g.moveTo(px, py) else g.lineTo(px, py)
        }
        g.closePath()
    }

    val font = "'Avenir Next', 'Segoe UI', Roboto, Arial, sans-serif"

    // THE BOARD, drawn once
    lazy val boardImage : CanvasImage = {
        val (canvas, g) = newCanvas(width.toInt, height.toInt)

        g.fillStyle = "#1d2126"
        g.fillRect(0, 0, width, height)

        val west = game.setup(0)
        val east = game.setup(1)

        Skirmish.board.all.foreach { t =>
            val c = Cell(t.hex)
            val cx = x(c)
            val cy = y(c)

            hexPath(g, cx, cy, S - 1)
            g.fillStyle = terrainColor(t.terrain)
            g.fill()

            // the terrain's marks
            g.strokeStyle = "rgba(20, 24, 31, 0.55)"
            g.fillStyle = "rgba(20, 24, 31, 0.35)"
            g.lineWidth = 3
            t.terrain match {
                case tactics.Forest =>
                    $((-18.0, -8.0), (6.0, -14.0), (-4.0, 10.0), (18.0, 6.0)).foreach { case (dx, dy) =>
                        g.beginPath()
                        g.arc(cx + dx, cy + dy, 9, 0, 2 * math.Pi)
                        g.fill()
                    }
                case tactics.Hills =>
                    $((-14.0, 0.0), (12.0, 8.0)).foreach { case (dx, dy) =>
                        g.beginPath()
                        g.arc(cx + dx, cy + dy + 6, 14, math.Pi, 2 * math.Pi)
                        g.stroke()
                    }
                case tactics.Mountain =>
                    g.beginPath()
                    g.moveTo(cx - 26, cy + 16)
                    g.lineTo(cx - 4, cy - 22)
                    g.lineTo(cx + 8, cy - 2)
                    g.lineTo(cx + 16, cy - 12)
                    g.lineTo(cx + 30, cy + 16)
                    g.closePath()
                    g.fill()
                case tactics.Water =>
                    g.strokeStyle = "rgba(230, 240, 250, 0.45)"
                    $(-10.0, 4.0, 18.0).foreach { dy =>
                        g.beginPath()
                        g.moveTo(cx - 24, cy + dy)
                        g.quadraticCurveTo(cx - 12, cy + dy - 7, cx, cy + dy)
                        g.quadraticCurveTo(cx + 12, cy + dy + 7, cx + 24, cy + dy)
                        g.stroke()
                    }
                case tactics.Building =>
                    $((-20.0, -14.0, 18.0, 16.0), (2.0, -18.0, 20.0, 20.0), (-10.0, 6.0, 22.0, 16.0)).foreach { case (dx, dy, w, h) =>
                        g.fillRect(cx + dx, cy + dy, w, h)
                    }
                case _ =>
            }

            // deployment zones: an inner border in the side's color
            $(west -> Skirmish.westZone, east -> Skirmish.eastZone).foreach { case (f, z) =>
                if (z.contains(t.hex)) {
                    hexPath(g, cx, cy, S - 7)
                    g.strokeStyle = color(f)
                    g.globalAlpha = 0.55
                    g.lineWidth = 4
                    g.setLineDash(scalajs.js.Array(8.0, 6.0))
                    g.stroke()
                    g.setLineDash(scalajs.js.Array[Double]())
                    g.globalAlpha = 1.0
                }
            }

            hexPath(g, cx, cy, S - 1)
            g.strokeStyle = "#14181f"
            g.lineWidth = 2
            g.stroke()

            // the hex's name at its top
            g.font = "bold 15px " + font
            g.textAlign = "center"
            g.textBaseline = "middle"
            g.fillStyle = "rgba(20, 24, 31, 0.7)"
            g.fillText(Skirmish.name(t.hex), cx, cy - S + 16)

            // cover: a shield with a bar for each level, at the left
            if (t.cover.level > 0) {
                val sx = cx - 34
                val sy = cy + 14
                g.beginPath()
                g.moveTo(sx - 9, sy - 10)
                g.lineTo(sx + 9, sy - 10)
                g.lineTo(sx + 9, sy + 2)
                g.lineTo(sx, sy + 11)
                g.lineTo(sx - 9, sy + 2)
                g.closePath()
                g.fillStyle = "#f3eee2"
                g.fill()
                g.fillStyle = "#14181f"
                1.to(t.cover.level).foreach(i => g.fillRect(sx - 5, sy - 8 + i * 5, 10, 2.5))
            }

            // sectors: their name, then their VP and income, under the hex name, above where the unit stands
            Skirmish.sectorName(t.hex).foreach { name =>
                val gives = t.vp./(v => "★" + v).$ ++
                    List((t.income.manpower, "MP"), (t.income.munitions, "MU"), (t.income.fuel, "FU")).filter(_._1 > 0).map { case (n, s) => "+" + n + " " + s }

                g.font = "bold 13px " + font
                val l1 = name
                val l2 = gives.mkString(" ")
                val w = max(g.measureText(l1).width, g.measureText(l2).width) + 12
                g.fillStyle = "rgba(20, 24, 31, 0.82)"
                g.fillRect(cx - w / 2, cy - 36, w, 32)
                g.fillStyle = "#f0c040"
                g.fillText(l1, cx, cy - 28)
                g.fillStyle = "#f3eee2"
                g.fillText(l2, cx, cy - 13)
            }
        }

        new CanvasImage(canvas)
    }

    // an outline of a hex, kept by color and width
    var outlines = Map[String, CanvasImage]()

    def outline(c : String, w : Int, inset : Int = 0) : ImageRect = {
        val key = c + "/" + w + "/" + inset
        val i = outlines.getOrElse(key, {
            val size = (2 * S + 8).toInt
            val (canvas, g) = newCanvas(size, size)
            hexPath(g, size / 2, size / 2, S - 2 - inset)
            g.strokeStyle = c
            g.lineWidth = w
            g.stroke()
            val i = new CanvasImage(canvas)
            outlines += key -> i
            i
        })
        ImageRect(i, Rectangle(-(S + 4), -(S + 4), 2 * S + 8, 2 * S + 8), 1.0)
    }

    // THE UNITS: a disc (a rounded square for vehicles) in the owner's color, the type's letters, a health bar
    def short(t : String) = t match {
        case "Riflemen" => "RF"
        case "Machine gun team" => "MG"
        case "Anti-tank gun" => "AT"
        case "Mortar team" => "MT"
        case "Tank" => "TK"
        case "Scouts" => "SC"
        case s => s.split(' ').map(_.take(1)).mkString.take(2).toUpperCase
    }

    var tokens = Map[String, CanvasImage]()

    def token(u : CombatUnit, f : Faction, full : Int, done : Boolean) : CanvasImage = {
        val label = short(game.unitType(u.id))
        val key = f.name + "/" + label + "/" + u.health + "/" + full + "/" + u.suppressed + "/" + done + "/" + u.kind.name + "/" + u.camouflaged
        tokens.getOrElse(key, {
            val (canvas, g) = newCanvas(96, 96)
            g.globalAlpha = done.?(0.6).|(1.0)

            g.beginPath()
            if (u.kind == tactics.Vehicle) {
                val r = 10.0
                g.moveTo(16 + r, 10)
                g.arcTo(80, 10, 80, 70, r)
                g.arcTo(80, 70, 16, 70, r)
                g.arcTo(16, 70, 16, 10, r)
                g.arcTo(16, 10, 80, 10, r)
            }
            else
                g.arc(48, 40, 31, 0, 2 * math.Pi)
            g.closePath()
            g.fillStyle = color(f)
            g.fill()
            g.strokeStyle = "#f3eee2"
            g.lineWidth = 4
            g.stroke()

            g.font = "bold 26px " + font
            g.textAlign = "center"
            g.textBaseline = "middle"
            g.fillStyle = "#14181f"
            g.fillText(label, 48, 41)

            // health
            g.fillStyle = "#14181f"
            g.fillRect(14, 78, 68, 12)
            g.fillStyle = (u.health * 3 > full).?("#7ed36a").|("#f0a030")
            g.fillRect(16, 80, 64.0 * u.health / max(1, full), 8)

            // camouflaged: a green badge at the top left
            if (u.camouflaged) {
                g.beginPath()
                g.arc(18, 16, 13, 0, 2 * math.Pi)
                g.fillStyle = "#5f8f3a"
                g.fill()
                g.font = "bold 17px " + font
                g.fillStyle = "#f3eee2"
                g.fillText("C", 18, 17)
            }

            // suppressed: an orange badge
            if (u.suppressed) {
                g.beginPath()
                g.arc(78, 16, 13, 0, 2 * math.Pi)
                g.fillStyle = "#f0a030"
                g.fill()
                g.font = "bold 17px " + font
                g.fillStyle = "#14181f"
                g.fillText("S", 78, 17)
            }

            val i = new CanvasImage(canvas)
            tokens += key -> i
            i
        })
    }

    def maxHealth(id : String) = game.state.rules.unitType(game.unitType(id))./(_.health).|(game.state.unit(id)./(_.health).|(1))

    lazy val units = new OrderedLayer
    lazy val highlighted = new OrderedLayer
    lazy val control = new OrderedLayer
    lazy val fogLayer = new OrderedLayer

    // a hex the viewer doesn't see: darkened
    lazy val fogImage : CanvasImage = {
        val size = (2 * S + 8).toInt
        val (canvas, g) = newCanvas(size, size)
        hexPath(g, size / 2, size / 2, S - 1)
        g.fillStyle = "rgba(10, 12, 16, 0.55)"
        g.fill()
        new CanvasImage(canvas)
    }

    // whose eyes the map is drawn through: with the fog of war, the one player this screen is for, until the game
    // is over; None shows everything (no fog, a spectator, or several players on one screen)
    def fogFor : |[Faction] = (game.state != null && game.state.rules.fog && game.isOver.not).??(viewers.single)

    lazy val scene = {
        val background = new OrderedLayer
        background.add(Sprite($(ImageRect(boardImage, Rectangle(0, 0, width, height), 1.0)), $))(0, 0)

        val areas = new HitLayer(regions)

        new Scene($(background, fogLayer, control, highlighted, areas, units), width, height, margins)
    }

    override def adjustCenterZoomX() {
        zoomBase = zoomBase.clamp(-990, 990)

        val qX = (width + margins.left + margins.right) * (1 - 1 / zoom) / 2
        dX = dX.clamp(-qX + margins.right, qX - margins.left)

        val qY = (height + margins.top + margins.bottom) * (1 - 1 / zoom) / 2
        dY = dY.clamp(-qY + margins.bottom, qY - margins.top)
    }

    var lastActions : $[UserAction] = $
    var lastThen : UserAction => Unit = null

    var highlightCell : |[Cell] = None

    // the players this screen is for: the one asked, or those waiting here; only they see their exact orders
    var viewers : $[Faction] = $
    var listedCells = $[Cell]()

    def cellsOf(a : UserAction) : $[Cell] = a.unwrap match {
        case a : HexKey => $(a.cell)
        case a : UnitKey => game.state.unit(a.unit)./(u => Cell(u.hex)).$
        case _ => $
    }

    override def updateHighlight(a : |[UserAction]) {
        listedCells = a./~(cellsOf)
        drawMap()
    }

    def hexActions(c : Cell) = lastActions.%(_.unwrap @@ {
        case a : HexKey => a.cell == c
        case _ => false
    })

    def unitActions(id : String) = lastActions.%(_.unwrap @@ {
        case a : UnitKey => a.unit == id
        case _ => false
    })

    def processRightClick(target : $[Any], xy : XY) {
    }

    def processHighlight(target : $[Any], xy : XY) {
        highlightCell = target.of[Cell].single
        val any = target.exists {
            case UnitTag(_, u) => unitActions(u).any
            case c : Cell => hexActions(c).any
            case _ => false
        }
        mapSmall.attach.parent.style.cursor = any.?("pointer").|("default")
    }

    def processTargetClick(target : $[Any], xy : XY) {
        target.collect { case t : UnitTag => t }.foreach { t =>
            val l = unitActions(t.unit)
            if (l.any)
                return choose(l)
        }

        (target.collect { case t : UnitTag => t.cell } ++ target.of[Cell]).distinct.foreach { c =>
            val l = hexActions(c)
            if (l.any)
                return choose(l)
        }
    }

    // one action is taken at once, several are offered over the map
    def choose(l : $[UserAction]) {
        if (l.num == 1)
            onClick(l.only)
        else
            showOverlay(overlayScrollX(l./(a => (a.question(game) ~ " " ~ a.option(game)).div(xstyles.choice)(xstyles.xx)(xstyles.chm)(xstyles.chp)(xstyles.thu)(xlo.fullwidth).pointer.onClick.param(a)).merge ~
                "Cancel".txt.div(xstyles.choice)(xstyles.xx)(xstyles.chm)(xstyles.chp)(xstyles.thu)(xlo.fullwidth).pointer.onClick.param(Nil)), onClick)
    }

    def makeScene() : |[Scene] = {
        if (game.state == null)
            return None

        val s = game.state

        units.clear()
        highlighted.clear()
        control.clear()
        fogLayer.clear()

        // the fog of war: the hexes the viewer's side doesn't see are darkened, and the enemy units it hasn't spotted aren't drawn
        val seen = fogFor./(f => s.visibleHexes(f.name))
        val hidden = fogFor./(f => s.units.values.%(_.owner != f.name).map(_.id).toSet -- s.spottedBy(f.name).map(_.id)).|(Set[String]())

        seen.foreach { v =>
            cells.%(c => v.contains(c.hex).not).foreach { c =>
                fogLayer.add(Sprite($(ImageRect(fogImage, Rectangle(-(S + 4), -(S + 4), 2 * S + 8, 2 * S + 8), 1.0)), $))(x(c), y(c))
            }
        }

        // who holds each sector: a ring in their color
        s.control.foreach { case (h, p) =>
            control.add(Sprite($(outline(color(game.faction(p)), 7, 3)), $))(x(Cell(h)), y(Cell(h)))
        }

        // the current choices: a gold outline, stronger where the mouse is (on the map or in the list)
        val choiceCells = lastActions./~(_.unwrap @@ {
            case a : HexKey => $(a.cell)
            case _ => $()
        }).distinct

        val targetUnits = lastActions./~(_.unwrap @@ {
            case a : AttackAction => $(a.target)
            case _ => $()
        }).distinct

        // the unit whose moves and attacks are offered
        val selected = lastActions./~(_.unwrap @@ {
            case a : MoveAction => $(a.unit)
            case a : AttackAction => $(a.attacker)
            case a : RetreatAction => $(a.unit)
            case _ => $()
        }).distinct.single

        choiceCells.foreach { c =>
            val strong = highlightCell.has(c) || listedCells.has(c)
            highlighted.add(Sprite($(outline(strong.?("#ffe070").|("rgba(255, 224, 112, 0.75)"), strong.?(7).|(4), 2)), $))(x(c), y(c))
        }

        targetUnits./~(s.unit).foreach { u =>
            val c = Cell(u.hex)
            val strong = highlightCell.has(c) || listedCells.has(c)
            highlighted.add(Sprite($(outline("#ff4040", strong.?(8).|(5), 2)), $))(x(c), y(c))
        }

        lastActions./~(_.unwrap @@ {
            case a : UnitMenuAction => $(a.unit)
            case a : ReinforceAction => $(a.unit)
            case _ => $()
        })./~(s.unit).foreach { u =>
            val c = Cell(u.hex)
            val strong = highlightCell.has(c) || listedCells.has(c)
            highlighted.add(Sprite($(outline(strong.?("#ffe070").|("rgba(255, 224, 112, 0.75)"), strong.?(7).|(4), 2)), $))(x(c), y(c))
        }

        selected./~(s.unit).foreach { u =>
            highlighted.add(Sprite($(outline("#ffffff", 6, 1)), $))(x(Cell(u.hex)), y(Cell(u.hex)))
        }

        // units that have nothing left to do this turn are dimmed
        s.units.values.toList.sortBy(_.id).%(u => hidden.contains(u.id).not).foreach { u =>
            val f = game.faction(u.owner)
            val c = Cell(u.hex)
            val done = u.attacked || (u.movesLeft == 0 && s.current.has(u.owner))
            val i = ImageRect(token(u, f, maxHealth(u.id), done), Rectangle(-30, -30, 60, 60), 1.0)
            val box = Rectangle(-30, -30, 60, 60)
            units.add(Sprite($(i), $(box), $(UnitTag(c, u.id))))(x(c), y(c) + 22)
        }

        |(scene)
    }

    // PANELS

    def factionStatus(f : Faction) {
        val container = statuses(game.setup.indexOf(f))

        if (game.state == null) {
            container.replace(Div(Span(resources.getName(f).|(f.name)).styled(f), styles.title), resources)
            return
        }

        val s = game.state
        val name = resources.getName(f).|(f.name)
        val turn = game.current.has(f)

        val badge = turn.?((s.phase.name + " phase").styled(styles.badge)).|(game.highlightFaction.has(f).?("Choosing".styled(styles.badge)).|(Empty))

        val title = Div(Span(name).styled(f) ~ badge, styles.name, styles.title)

        val p = s.player(f.name)

        val vp = Div("VP ".txt ~ p.vp.hlb ~ ("/" + s.rules.vpTarget).txt)

        val stock = Div(
            Span("Manpower ".txt ~ p.resources.manpower.hlb, styles.stat) ~
            Span("Munitions ".txt ~ p.resources.munitions.hlb, styles.stat) ~
            Span("Fuel ".txt ~ p.resources.fuel.hlb, styles.stat)
        )(styles.row)

        // orders: the exact number left only for the player whose turn it is, on their own screen; the others see the range
        val orders = game.ordersRange./{ range =>
            val mine = viewers.has(f) || game.isOver
            val line =
                if (turn.not)
                    "Orders " + range + " a turn"
                else
                if (s.exactOrderLimit == 0)
                    "Orders " + range + ", not drawn yet"
                else
                if (mine)
                    "Orders left " + s.ordersLeft.|(0) + " of " + s.exactOrderLimit
                else
                    "Orders " + range + ", " + s.ordersUsed + " used"
            Div(line.txt)(styles.row)
        }.|(Empty)

        val (gain, gvp) = Turns.income(s, f.name)
        val income = Div(("Each turn " + gain.toString + (gvp > 0).??(", " + gvp + " VP")).txt)(styles.small)

        val l = s.unitsOf(f.name)
        // with the fog of war the other side's panel says only how many units it has
        val secret = fogFor.exists(_ != f)
        val unitRow = Div(("Units " + l.num + (l.any && secret.not).??(": " + l.groupBy(u => game.unitType(u.id)).toList.sortBy(_._1)./{ case (t, us) => us.num + " " + short(t) }.mkString(", "))).txt)(styles.small)

        val sectors = s.sectorsOf(f.name)./(t => Skirmish.sectorName(t.hex).|("?") + " " + Skirmish.name(t.hex))
        val sectorRow = Div(("Sectors " + sectors.some./(_.mkString(", ")).|("none")).txt)(styles.small)

        container.replace((title ~ vp ~ orders ~ stock ~ income ~ unitRow ~ sectorRow).div(xstyles.fillHeight), resources, onClick)

        if (turn || game.highlightFaction.has(f))
            container.attach.parent.style.outline = "3px " + turn.?("solid").|("dashed") + " " + color(f)
        else
            container.attach.parent.style.outline = ""
    }

    def gameStatus() {
        val e = (game.state == null).?(Div("Hex Tactics".hl)).|(Div("Turn " ~ game.state.turn.hl ~ " " ~ game.current./(f => f.elem ~ " " ~ (game.state.phase.name + " phase").txt).|(Empty)))
        statusGame.replace(overlayFit(Div(e ~ Div(("Rules and units".hh).div(xlo.pointer).onClick.param("rules")), styles.gstatus)), resources, onClick)
    }

    def updateStatus() {
        0.until(arity).foreach { n =>
            factionStatus(game.setup(n))
        }

        gameStatus()

        if (overlayPane.visible)
            overlayPane.vis()
        else
            overlayPane.invis()

        drawMap()
    }

    val layoutZoom = 0.49

    val layouts = $(Layout("base",
        $(
            BasicPane("status", 14, narrow.?(22.0).|(13.0), Priorities(top = 3, right = 2, maxXscale = 1.8, maxYscale = 1.8)),
            BasicPane("status-game", 16, 10, Priorities(maxXscale = 1.2, maxYscale = 1.2)),
            BasicPane("log", 30, 13, Priorities(right = 1)),
            BasicPane("map-small", 60, 40, Priorities(top = 2, left = 1, grow = -1)),
            BasicPane("action-a", 64, 26, Priorities(bottom = 1, right = 3, grow = 1)),
            BasicPane("action-b", 52, 47, Priorities(bottom = 1, right = 3, grow = 1, maxXscale = 1.2))
        )./(p => p.copy(kX = p.kX * layoutZoom, kY = p.kY * layoutZoom))
    ))./~(l =>
        l.copy(name = l.name + "-fulldim", panes = l.panes./{
            case p : BasicPane if p.name == "map-small" => FullDimPane(p.name, p.kX, p.kY, p.pr)
            case p => p
        }, boost = 1.2) ::
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
            case p : BasicPane if p.name == "action-a" => |(p.copy(name = "action"))
            case p : BasicPane if p.name == "action-b" => None
            case p => |(p)
        }) ::
        l.copy(name = l.name + "-actionB", panes = l.panes./~{
            case p : BasicPane if p.name == "action-a" => None
            case p : BasicPane if p.name == "action-b" => |(p.copy(name = "action"))
            case p => |(p)
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

    val layoutKey = "v" + 1 + "." + "arity-" + arity + narrow.??(".narrow")

    def overlayScrollX(e : Elem) = overlayScroll(e)(styles.seeThroughInner).onClick
    def overlayFitX(e : Elem) = overlayFit(e)(styles.seeThroughInner).onClick

    def showOverlay(e : Elem, onClick : Any => Unit) {
        overlayPane.vis()
        overlayPane.replace(e, resources, onClick, _ => {}, _ => {})
    }

    def clearOverlay() {
        overlayPane.invis()
        overlayPane.clear()
    }

    // the rules as played and every unit type's numbers
    def rulesView() : Elem = {
        val r = Skirmish.rules

        def line(s : String) = Div(s.txt)

        val roster = r.roster./{ t =>
            val w = t.weapon./(w => w.name + ": range " + (w.minRange > 1).??(w.minRange + "-") + w.range + ", " + w.dice + " dice hitting on " + w.accuracy + "+, " + w.infantryDamage + " damage to infantry, " + w.vehicleDamage + " to vehicles" + (w.penetration > 0).??(", penetration " + w.penetration)).|("no weapon")
            Div(t.name.hlb ~ (" (" + short(t.name) + ", " + t.kind.name.toLowerCase + "): costs " + t.cost + "; health " + t.health + ", movement " + t.movement + ", armor " + t.armor + ", sight " + t.sightRange + t.camouflaged.??(", camouflaged")).txt ~ Break ~ w.txt)(styles.small)
        }.merge

        val terrain = tactics.Terrain.all./{ t =>
            def c(k : MoveKind) = t.cost(k)./(_.toString).|("can't")
            line(t.name + ": infantry " + c(tactics.Infantry) + ", vehicles " + c(tactics.Vehicle) + t.blocking.??(", blocks line of sight"))
        }.merge

        Div("Hex Tactics".hlb) ~
        Div("Each player's turn has three phases.", styles.legend) ~
        Div(
            line("Income: you get " + r.base + " and what your sectors give, and score their VP; deploy units on the free hexes of your zone (dashed border), paying their cost.") ~
            line("Orders: at the start of your turn you draw your order limit, " + game.ordersRange.|("") + " (only you see the number). Every move, attack and retreat spends an order; the Action phase ends when they run out.") ~
            line("Action: each unit may move, in as many moves as its movement points allow, and then attack once; it can't move after attacking.") ~
            line("Retreat: hurt infantry may fall back up to " + r.retreatHexes + " hexes toward your zone, whatever the terrain costs (not through water, mountains or enemy units); it can't move or attack again that turn. In your Income phase, hurt infantry in your zone can be reinforced: " + r.reinforceCost + " for " + r.reinforceHealth + " health back.") ~
            line("End: your suppressed units recover.") ~
            line("A unit ending a move on a sector (named on the map) captures it; it stays yours until an enemy unit ends a move there. Destroying an enemy unit scores " + r.killVP + " VP. The first player at " + r.vpTarget + " VP wins.") ~
            line("Fog of war: you see what your units see, " + "their sight range in hexes and in their line of sight; the rest of the map is darkened, and enemy units there are hidden. A camouflaged unit (C) is spotted only from next to it, or by a unit that sees 4 hexes or more (scouts). A unit may fire at any enemy your side has spotted, beyond its own sight, but still within its range and its own line of sight. The log doesn't say where units go.") ~
            line("Attacks need range and line of sight (mountains and buildings between block it). Each die hits on the weapon's number or more: +1 for light cover (one bar on the shield), +2 for heavy, +1 if the attacker is suppressed; a 1 always misses and a 6 always hits. Each hit deals the weapon's damage less the armor beyond its penetration. Infantry hit and left standing is suppressed (S) until the end of its side's next turn."),
            styles.legend) ~
        Div("Units".hlb) ~ Div(roster, styles.legend) ~
        Div("Movement cost".hlb) ~ Div(terrain, styles.legend)
    }

    override def onClick(a : Any) = a @@ {
        case ("notifications", Some(f : Faction)) =>
            shown = $
            showNotifications($(f))

        case ("notifications", None) =>
            shown = $
            showNotifications(game.factions)

        case "rules" =>
            showOverlay(overlayScrollX(rulesView()), onClick)

        case action : UserAction if lastThen != null =>
            clearOverlay()

            val then = lastThen
            lastThen = null
            lastActions = $

            asker.clear()

            then(action)

        case Nil =>
            clearOverlay()

        case x =>
            clearOverlay()
    }

    override def info(self : |[Faction], aa : $[UserAction]) = {
        val ii = currentGame.info($, self, aa)
        ii.any.??($(ZOption(Empty, Break)) ++ convertActions(self.of[Faction], ii)) ++
            $(ZBasic(Break ~ Break, "Rules and units".hh, () => { onClick("rules") }).copy(clear = false)) ++
            (currentGame.isOver && hrf.HRF.flag("replay").not).$(
                ZBasic(Break ~ Break ~ Break, "Save Replay As File".hh, () => {
                    showOverlay(overlayScrollX("Saving Replay...".hl.div).onClick, null)

                    callbacks.saveReplay {
                        overlayPane.invis()
                        overlayPane.clear()
                    }
                }).copy(clear = false)
            ) ++
            $(ZBasic(Break ~ Break, "Notifications".spn, () => { onClick("notifications", self) }).copy(clear = false)).%(_ => self.any || game.isOver) ++
            $(ZBasic(Break ~ Break, "Interface".spn, () => { callbacks.editSettings { updateStatus() } }).copy(clear = false)) ++
            $(ZBasic(Break ~ Break, "Report a Bug".spn, () => { callbacks.reportBug() }).copy(clear = false)).%(_ => callbacks.canReportBug) ++
            $(ZBasic(Break ~ Break, "Main Menu".spn, () => { callbacks.returnToTitle() }).copy(clear = false)).%(_ => callbacks.canReturnToTitle)
    }

    var shown : $[Notification] = $
    var primed = false

    override def showNotifications(self : $[F]) : Unit = {
        if (primed.not) {
            primed = true
            shown = game.notifications
        }

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
        viewers = self

        showNotifications(self)

        super.wait(self, factions, message)
    }

    // a long list of hexes is folded, as they are all highlighted on the map, until it is asked for
    val foldFrom = 9
    var unfolded = false
    var unfold : () => Unit = () => {}

    override def convertActions(faction : |[F], actions : $[UserAction], then : UserAction => Unit = null) : $[ZOption] = {
        val l = actions.%(a => a.unwrap.is[HexKey] && a.is[Unavailable].not && a.is[Hidden].not)

        val fold = then != null && unfolded.not && l./(_.unwrap.as[HexKey].get.cell).distinct.num >= foldFrom

        if (fold.not)
            super.convertActions(faction, actions, then)
        else
            ZOption(Div(l.head.question(currentGame))(xlo.fullwidth), Div("Tap a highlighted hex on the map", xstyles.info, xstyles.xx, xstyles.thu, xlo.fullwidth)) ::
            ZOption(Div(l.head.question(currentGame))(xlo.fullwidth), OnClick(Div("Show the " ~ l.num.hl ~ " choices as a list", $(xstyles.choice, xstyles.xx, xstyles.thu, xlo.pointer, xlo.fullwidth) ++ faction./(f => elem.borders.get(f)).$)), _ => {
                unfolded = true
                unfold()
            }, false) ::
            super.convertActions(faction, actions.diff(l), then)
    }

    override def ask(faction : |[F], actions : $[UserAction], then : UserAction => Unit) {
        lastActions = actions
        lastThen = then
        viewers = faction.$
        listedCells = $
        highlightCell = None
        unfolded = false
        unfold = () => ask2(faction, actions, then)

        showNotifications(faction.$)

        updateStatus()

        ask2(faction, actions, then)
    }

    def ask2(faction : |[F], actions : $[UserAction], then : UserAction => Unit) {
        super.ask(faction, actions, a => {
            clearOverlay()
            lastActions = $
            listedCells = $
            then(a)
        })
    }

    override def styleAction(faction : |[Faction], actions : $[UserAction], a : UserAction, unavailable : Boolean, view : |[Any]) = {
        unavailable.$(xstyles.unavailableText) ++
        (a @@ {
            case _ : Info => $(xstyles.info, xstyles.xx, xstyles.thu)
            case _ => $(xstyles.choice, xstyles.xx, xstyles.thu)
        }) ++
        (a @@ {
            case _ : Choice | _ : Cancel | _ : OnClickInfo => $(xlo.pointer)
            case _ => $()
        }) ++
        $(xlo.fullwidth) ++
        faction./(f => elem.borders.get(f)).$
    }
}
