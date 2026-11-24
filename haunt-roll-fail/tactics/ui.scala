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

    def create(uir : ElementAttachmentPoint, arity : Int, options : $[mmeta.O], resources : Resources, title : String, callbacks : hrf.Callbacks) = new UI(uir, arity, resources, callbacks)
}

class UI(val uir : ElementAttachmentPoint, arity : Int, val resources : Resources, callbacks : hrf.Callbacks) extends MapGUI {
    def factionElem(f : Faction) = f.name.styled(f)

    lazy val narrow = dom.window.innerWidth < 700

    val statuses = 1.to(arity)./(i => newPane("status-" + i, Content, styles.status, styles.fstatus, ExternalStyle("hide-scrollbar")))
    val statusGame = newOuterPane("status-game", Content)

    object regions extends Regions[Hex] {
        def center(h : Hex) = XY(Board.x(h), Board.y(h))

        def regionAt(p : XY) : |[Hex] = {
            val h = Board.hexes.minBy(h => (Board.x(h) - p.x) * (Board.x(h) - p.x) + (Board.y(h) - p.y) * (Board.y(h) - p.y))
            val d = math.sqrt((Board.x(h) - p.x) * (Board.x(h) - p.x) + (Board.y(h) - p.y) * (Board.y(h) - p.y))
            (d < 95).?(h)
        }

        def random(h : Hex) : XY = XY(Board.x(h), Board.y(h))
    }

    lazy val units = new OrderedLayer
    lazy val structures = new OrderedLayer
    lazy val highlighted = new OrderedLayer
    lazy val labels = new OrderedLayer

    val width = Board.width
    val height = Board.height
    val margins = Margins(0, 0, 0, 0)

    lazy val scene = {
        val mp = img(game.map.image)

        val background = new OrderedLayer
        background.add(Sprite($(ImageRect(new RawImage(mp), Rectangle(0, 0, mp.width, mp.height), 1.0)), $))(0, 0)

        // the Spawn Areas, tinted in their team's color (a layer of their own, as a layer draws its sprites by their height)
        val spawns = new OrderedLayer
        game.setup.foreach { f =>
            Board.spawn(game.team(f)).foreach { h =>
                spawns.add(Sprite($(ImageRect(new RawImage(img("hex-spawn-" + f.id)), Rectangle(-93, -101, 186, 202), 1.0)), $))(Board.x(h), Board.y(h))
            }
        }

        val areas = new HitLayer(regions)

        new Scene($(background, spawns, highlighted, structures, areas, units, labels), mp.width, mp.height, margins)
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

    var highlightHex : |[Hex] = None

    var listedHexes = $[Hex]()
    var loggedHex : |[Hex] = None

    def hexesOf(a : UserAction) : $[Hex] = a.unwrap match {
        case a : HexKey => $(a.hex)
        case a : SquadKey => game.pos.get(a.squad).$
        case a : TargetAction => game.pos.get(a.unit).$ ++ game.pos.get(a.target).$
        case _ => $
    }

    var textImages = Map[String, CanvasImage]()

    // a label in a dark box: the hex codes, and the units' health
    def textImage(s : String, fg : String = "#f3eee2", bg : String = "#14181f") = textImages.getOrElse(s + fg + bg, {
        val canvas = dom.document.createElement("canvas").asInstanceOf[dom.html.Canvas]
        canvas.width = 128
        canvas.height = 72
        val g = canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]
        g.font = "bold 46px 'Avenir Next', 'Segoe UI', Roboto, Arial, sans-serif"
        g.textAlign = "center"
        g.textBaseline = "middle"
        g.fillStyle = bg
        val w = g.measureText(s).width + 32
        g.fillRect(64 - w / 2, 6, w, 60)
        g.fillStyle = fg
        g.fillText(s, 64, 38)
        val i = new CanvasImage(canvas)
        textImages += (s + fg + bg) -> i
        i
    })

    // sandbags along an edge of a hex
    var bagImages = Map[Int, CanvasImage]()

    def bagImage(e : Int) = bagImages.getOrElse(e, {
        val canvas = dom.document.createElement("canvas").asInstanceOf[dom.html.Canvas]
        canvas.width = 200
        canvas.height = 210
        val g = canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]
        val w = Board.dx / 2
        val s = Board.side
        val corners = $((0.0, -s), (w, -s / 2), (w, s / 2), (0.0, s), (-w, s / 2), (-w, -s / 2))
        val (ax, ay) = corners(e)
        val (bx, by) = corners((e + 1) % 6)
        g.strokeStyle = "#c8a46a"
        g.lineWidth = 14
        g.lineCap = "round"
        g.beginPath()
        g.moveTo(100 + ax * 0.82, 105 + ay * 0.82)
        g.lineTo(100 + bx * 0.82, 105 + by * 0.82)
        g.stroke()
        g.strokeStyle = "#5a4020"
        g.lineWidth = 3
        g.stroke()
        val i = new CanvasImage(canvas)
        bagImages += e -> i
        i
    })

    // the side a Heavy Vehicle or set-up Weapon Team faces: a triangle on the token's edge, in its faction's color
    var arrowImages = Map[(Int, String), CanvasImage]()

    def arrowImage(e : Int, color : String) = arrowImages.getOrElse((e, color), {
        val canvas = dom.document.createElement("canvas").asInstanceOf[dom.html.Canvas]
        canvas.width = 160
        canvas.height = 160
        val g = canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]
        val a = math.toRadians(Board.normals(e))
        def at(r : Double, d : Double) = (80 + r * math.cos(a + math.toRadians(d)), 80 + r * math.sin(a + math.toRadians(d)))
        val (x0, y0) = at(78, 0)
        val (x1, y1) = at(58, -14)
        val (x2, y2) = at(58, 14)
        g.fillStyle = color
        g.strokeStyle = "#14181f"
        g.lineWidth = 4
        g.beginPath()
        g.moveTo(x0, y0)
        g.lineTo(x1, y1)
        g.lineTo(x2, y2)
        g.closePath()
        g.fill()
        g.stroke()
        val i = new CanvasImage(canvas)
        arrowImages += (e, color) -> i
        i
    })

    // a hex filled with a color (Smoke gray, a strike red, a Recon Sweep blue)
    var fillImages = Map[String, CanvasImage]()

    def fillImage(fill : String, stroke : String) = fillImages.getOrElse(fill + stroke, {
        val canvas = dom.document.createElement("canvas").asInstanceOf[dom.html.Canvas]
        canvas.width = 200
        canvas.height = 210
        val g = canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]
        val w = Board.dx / 2
        val sd = Board.side
        val corners = $((0.0, -sd), (w, -sd / 2), (w, sd / 2), (0.0, sd), (-w, sd / 2), (-w, -sd / 2))
        g.fillStyle = fill
        g.strokeStyle = stroke
        g.lineWidth = 5
        g.beginPath()
        corners.zipWithIndex.foreach { case ((x, y), i) => if (i == 0) g.moveTo(100 + x * 0.92, 105 + y * 0.92) else g.lineTo(100 + x * 0.92, 105 + y * 0.92) }
        g.closePath()
        g.fill()
        g.stroke()
        val i = new CanvasImage(canvas)
        fillImages += (fill + stroke) -> i
        i
    })

    // a destroyed Building: the hex darkened
    lazy val ruinImage = {
        val canvas = dom.document.createElement("canvas").asInstanceOf[dom.html.Canvas]
        canvas.width = 200
        canvas.height = 210
        val g = canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]
        val w = Board.dx / 2
        val sd = Board.side
        val corners = $((0.0, -sd), (w, -sd / 2), (w, sd / 2), (0.0, sd), (-w, sd / 2), (-w, -sd / 2))
        g.fillStyle = "rgba(40, 30, 30, 0.45)"
        g.strokeStyle = "#8a5fb0"
        g.lineWidth = 6
        g.beginPath()
        corners.zipWithIndex.foreach { case ((x, y), i) => if (i == 0) g.moveTo(100 + x * 0.96, 105 + y * 0.96) else g.lineTo(100 + x * 0.96, 105 + y * 0.96) }
        g.closePath()
        g.fill()
        g.stroke()
        new CanvasImage(canvas)
    }

    def locate(h : |[Hex]) {
        if (h != loggedHex) {
            loggedHex = h
            drawMap()
        }
    }

    dom.document.addEventListener("pointerover", (e : dom.PointerEvent) => {
        val hex = e.target @@ {
            case t : dom.Element =>
                val code = t.asInstanceOf[scalajs.js.Dynamic].closest("[class*=hrf-hanging] .tactics-hex").asInstanceOf[dom.Element]
                if (code != null) Board.hexes.find(_.name == code.textContent.trim) else None
            case _ => None
        }
        locate(hex)
    })

    override def updateHighlight(a : |[UserAction]) {
        listedHexes = a./~(hexesOf)
        drawMap()
    }

    def hexActions(h : Hex) = lastActions.%(_.unwrap @@ {
        case a : HexKey => a.hex == h
        case _ => false
    })

    def squadActions(u : Squad) = lastActions.%(_.unwrap @@ {
        case a : SquadKey => a.squad == u
        case _ => false
    })

    def processRightClick(target : $[Any], xy : XY) {
    }

    def processHighlight(target : $[Any], xy : XY) {
        highlightHex = target.of[Hex].single
        val any = target.exists {
            case (_ : Hex, u : Squad) => squadActions(u).any
            case h : Hex => hexActions(h).any
            case _ => false
        }
        mapSmall.attach.parent.style.cursor = any.?("pointer").|("default")
    }

    def processTargetClick(target : $[Any], xy : XY) {
        val squads = target.collect { case (_ : Hex, u : Squad) => u }

        squads.foreach { u =>
            val l = squadActions(u)
            if (l.any)
                return choose(l)
        }

        target.of[Hex].foreach { h =>
            val l = hexActions(h)
            if (l.any)
                return choose(l)
        }

        // a unit with nothing to do: what it is
        squads.starting.foreach { u =>
            onClick(u)
        }
    }

    def choose(l : $[UserAction]) {
        if (l.num == 1)
            onClick(l.only)
        else
            showOverlay(overlayScrollX(l./(a => (a.question(game) ~ " " ~ a.option(game)).div(xstyles.choice)(xstyles.xx)(xstyles.chm)(xstyles.chp)(xstyles.thu)(xlo.fullwidth).pointer.onClick.param(a)).merge ~
                "Cancel".txt.div(xstyles.choice)(xstyles.xx)(xstyles.chm)(xstyles.chp)(xstyles.thu)(xlo.fullwidth).pointer.onClick.param(Nil)), onClick)
    }

    def scaled(name : String, size : Double, alpha : Double = 1.0) : ImageRect = {
        val i = img(name)
        val k = size / max(i.width, i.height)
        val w = i.width * k
        val h = i.height * k
        ImageRect(new RawImage(i), Rectangle(-w / 2, -h / 2, w, h), alpha)
    }

    def makeScene() : |[Scene] = {
        if (img(game.map.image).complete.not)
            return None

        units.clear()
        structures.clear()
        highlighted.clear()
        labels.clear()

        val hexes = lastActions./~(_.unwrap @@ {
            case a : HexKey => $(a.hex)
            case _ => $()
        }).distinct

        val many = hexes.num > 8
        hexes.foreach { h =>
            val strong = highlightHex.has(h) || listedHexes.has(h)
            val select = ImageRect(new RawImage(img("hex-select")), Rectangle(-90, -98, 180, 196), (strong || many.not).?(1.0).|(0.75))
            highlighted.add(Sprite(strong.?($(select, select)).|($(select)), $))(Board.x(h), Board.y(h))
        }

        // the targets on offer, and the damage already assigned (an HE Splash hits the hexes around its own too)
        val splashed = game.splashes./~(s => game.area(s.hex)) ++ game.shots./(_.hex)
        splashed.distinct.foreach { h =>
            highlighted.add(Sprite($(ImageRect(new RawImage(img("hex-target")), Rectangle(-90, -98, 180, 196), 0.55)), $))(Board.x(h), Board.y(h))
        }

        val targeted = (lastActions.collect { case a : TargetAction => a.target } ++ game.assignments./(_.target)).distinct
        targeted.foreach { u =>
            game.pos.get(u).foreach { h =>
                highlighted.add(Sprite($(ImageRect(new RawImage(img("hex-target")), Rectangle(-90, -98, 180, 196), 0.85)), $))(Board.x(h), Board.y(h))
            }
        }

        // the Control Points: their resources, and the flag of the team holding them
        Board.points.foreach { case (h, l) =>
            l.zipWithIndex.foreach { case (r, i) =>
                structures.add(Sprite($(scaled("res:" + r.id, 50)), $))(Board.x(h) - 36 + (l.num == 1).?(36).|(i * 36 * 2 / (l.num - 1)) - (l.num == 1).?(0).|(0), Board.y(h) - 70)
            }
            game.control.get(h).foreach { f =>
                structures.add(Sprite($(scaled("status:flag-" + f.id, 92)), $))(Board.x(h) + 70, Board.y(h) - 30)
            }
        }

        // destroyed Buildings, and the damage of damaged ones
        game.ruined.foreach { h =>
            structures.add(Sprite($(ImageRect(ruinImage, Rectangle(-100, -105, 200, 210), 1.0)), $))(Board.x(h), Board.y(h))
        }

        game.buildingDamage.foreach { case (h, n) =>
            if (game.ruined.contains(h).not)
                structures.add(Sprite($(ImageRect(textImage("Building " + (2 - n) + "/2", "#f3eee2", "#5a4a3a"), Rectangle(-50, -15, 100, 30), 0.95)), $))(Board.x(h), Board.y(h) - 84)
        }

        // Commanders: Smoke, the strikes to come, the hexes a Recon Sweep shows
        game.smoke.foreach { h =>
            structures.add(Sprite($(ImageRect(fillImage("rgba(220, 220, 220, 0.55)", "#f0f0f0"), Rectangle(-100, -105, 200, 210), 1.0)), $))(Board.x(h), Board.y(h))
        }
        game.strikes./~(_.hexes).distinct.foreach { h =>
            structures.add(Sprite($(ImageRect(fillImage("rgba(220, 40, 30, 0.25)", "#ff5030"), Rectangle(-100, -105, 200, 210), 1.0)), $))(Board.x(h), Board.y(h))
        }
        game.eyes.values.toList.flatMap(_.toList).distinct.foreach { h =>
            structures.add(Sprite($(ImageRect(fillImage("rgba(60, 120, 230, 0.18)", "#5a8cff"), Rectangle(-100, -105, 200, 210), 1.0)), $))(Board.x(h), Board.y(h))
        }

        game.sandbags.foreach { case (h, l) =>
            l.foreach { e =>
                structures.add(Sprite($(ImageRect(bagImage(e), Rectangle(-100, -105, 200, 210), 1.0)), $))(Board.x(h), Board.y(h))
            }
        }

        // the units: a token, its health, and its status (set up, packed up, pinned); a passenger small on its vehicle
        game.pos.foreach { case (u, h) =>
            val passenger = game.isPassenger(u)
            // an assaulting unit is drawn smaller at the edge of the hex it assaults
            val assaulting = game.assaults.contains(u)
            val size = passenger.?(66).|(assaulting.?(84).|(116))
            val x = Board.x(h) + passenger.?(46).|(assaulting.?(-46).|(0))
            val y = Board.y(h) + passenger.?(-36).|(assaulting.?(40).|(6))
            val token = scaled("token:" + u.utype.id + "-" + u.faction.id, size)
            val box = Rectangle(token.rect.x, token.rect.y, token.rect.width, token.rect.height)
            val hp = ImageRect(textImage(game.health(u) + "/" + game.maxHealth(u), "#f3eee2", u.faction.color), Rectangle(-size * 0.28, size * 0.22, size * 0.56, size * 0.31), 0.95)
            val st = $[ImageRect]() ++
                game.pinned.contains(u).$(ImageRect(new RawImage(img("status:pinned")), Rectangle(-size / 2, -size / 2, size * 0.32, size * 0.32), 1.0)) ++
                (u.utype.setsUp && passenger.not).$(ImageRect(new RawImage(img(game.setUp.contains(u).?("status:setup").|("status:nofire"))), Rectangle(size * 0.18, -size / 2, size * 0.32, size * 0.32), 1.0)) ++
                game.noFire.contains(u).$(ImageRect(new RawImage(img("status:nofire")), Rectangle(size * 0.18, -size / 2, size * 0.32, size * 0.32), 1.0)) ++
                game.slowed.get(u).has(game.round).$(ImageRect(new RawImage(img("status:slow")), Rectangle(-size / 2, size * 0.18, size * 0.32, size * 0.32), 1.0)) ++
                game.retreating.contains(u).$(ImageRect(new RawImage(img("status:retreat")), Rectangle(size * 0.18, size * 0.18, size * 0.32, size * 0.32), 1.0)) ++
                game.facing.get(u).toList.map(e => ImageRect(arrowImage(e, u.faction.color), Rectangle(-size * 0.8, -size * 0.8, size * 1.6, size * 1.6), 1.0))
            units.add(Sprite($(token, hp) ++ st, $(box), $((h, u))), 1.0, passenger.?(2).|(assaulting.?(3).|(1)))(x, y)
        }

        val listed = folded.?(lastActions.%(_.unwrap.is[HexKey].not)).|(lastActions)

        (highlightHex.$ ++ listedHexes ++ loggedHex.$ ++ listed./~(hexesOf)).distinct.foreach { h =>
            labels.add(Sprite($(ImageRect(textImage(h.name), Rectangle(-32, -18, 64, 36), 0.92)), $))(Board.x(h), Board.y(h) + 78)
        }

        loggedHex.foreach { h =>
            val select = ImageRect(new RawImage(img("hex-select")), Rectangle(-90, -98, 180, 196), 1.0)
            highlighted.add(Sprite($(select, select), $))(Board.x(h), Board.y(h))
        }

        |(scene)
    }

    def factionStatus(f : Faction) {
        val container = statuses(game.setup.indexOf(f))

        val name = resources.getName(f).|(f.name)

        val badge = (f == game.current).?("Turn".styled(styles.badge)).|((game.highlightFaction.has(f)).?("Choosing".styled(styles.badge)).|(Empty))

        val title = Div(Image("emblem:" + f.id, styles.token) ~ " " ~ Span(name).styled(f) ~ badge, styles.name, styles.title)

        val p = game.states(f)

        def stat(r : Res) = Span(r.icon ~ p.stock(r).hlb ~ (r != XP).?(("+" + game.income(f, r)).spn(styles.small) : Elem).|(Empty), styles.stat)

        // in team games, the team's VPs together after the player's own (2026-11-24)
        val vp = Div(stat(VP) ~ game.teamGame.?((" (team " + game.teamVP(game.team(f)) + ")").spn(styles.small) : Elem).|(Empty) ~ " / " ~ game.endVP.hl ~ " " ~ stat(XP))

        val stock = Div(Res.spent./(stat).merge)(styles.row)

        val tier = Div(OnClick(("board", f), Span(("Tier " + p.tier).hl ~ " " ~ Tiers.name(f, p.tier).spn(styles.small) ~ " " ~ "(boards)".spn(styles.small), xlo.pointer)))(styles.small)

        val units = game.units(f)./(u => Hint(u.utype.name + " " + game.health(u) + "/" + game.maxHealth(u), Image("token:" + u.utype.id + "-" + f.id, styles.token, u.utype.name)) : Elem)

        val unitRow = Div(units.merge)(styles.row)

        val points = Div(("Control Points " + game.pointsOf(f).num).txt)(styles.small)

        // the Commander: its name and tiers unlocked (or how many are drawn), tapping shows the cards
        val commanderSpan = (game.commanders && Commander.all.exists(_.faction == f)).?(OnClick(("commander", f), Span(game.commanderOf.get(f)./(c => c.elem ~ " " ~ (game.tiersOf.getOrElse(f, 0) + "/4").hl).|(("Commander: " + game.drawn.getOrElse(f, $[Commander]()).num + " drawn").txt), xlo.pointer)) : Elem).|(Empty)
        // the Lost Brigade, on the same line: its name once picked (or how many are drawn), tapping shows the cards
        val brigadeSpan = game.brigades.?(OnClick(("brigade", f), Span(game.brigadeOf.get(f)./(b => b.elem).|(("Lost Brigades: " + game.brigadesDrawn.getOrElse(f, $[Brigade]()).num + " drawn").txt), xlo.pointer)) : Elem).|(Empty)
        val commander = (game.commanders || game.brigades).?(Div(commanderSpan ~ (game.commanders && game.brigades).?(" · ".txt : Elem).|(Empty) ~ brigadeSpan)(styles.small)).|(Empty)

        container.replace((title ~ vp ~ stock ~ tier ~ commander ~ points ~ unitRow).div(xstyles.fillHeight), resources, onClick)

        if (f == game.current || game.highlightFaction.has(f))
            container.attach.parent.style.outline = "3px " + (f == game.current).?("solid").|("dashed") + " " + f.color
        else
            container.attach.parent.style.outline = ""
    }

    def gameStatus() {
        val dice = game.lastRolls.takeRight(12)./(_.elem).merge

        val phase = game.phase @@ {
            case "Maneuver" => "Maneuver".hl ~ " " ~ Option(game.current)./(f => f.elem ~ " " ~ game.cpLeft.hlb ~ " CP").|(Empty)
            case p => p.hl
        }

        // the Operation picked: the other team knows only its side
        val operation = (game.full && game.phase == "Maneuver" && game.opsCard.any).?(Div("Operation " ~ Operations.side(game.opsUpper).hl ~ " turns")).|(Empty)

        statusGame.replace(overlayFit(Div(Div("Round " ~ game.round.hl) ~ Div(phase) ~ operation ~ Div(dice) ~
            ("Building Boards".hh).div(xlo.pointer).onClick.param(("board", game.setup.head)), styles.gstatus)), resources, onClick)
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
            BasicPane("status", 14, narrow.?(18.0).|(9.6), Priorities(top = 3, right = 2, maxXscale = 1.8, maxYscale = 1.8)),
            BasicPane("status-game", 16, 10, Priorities(maxXscale = 1.2, maxYscale = 1.2)),
            BasicPane("log", 30, 13, Priorities(right = 1)),
            BasicPane("map-small", 80, 60, Priorities(top = 2, left = 1, grow = -1)),
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

    // what a unit is: its stats from its Building Board, its upgrades and its state
    def squadInfo(u : Squad) : Elem = {
        val t = u.utype
        val ups = game.bought.getOrElse(u, $[Bought]())./(_.effect)
        Div(Image("token:" + t.id + "-" + u.faction.id, styles.icon) ~ u.elem) ~
        Div(t.kind.elem ~ ", health " ~ game.health.get(u)./(_.hl).|(t.health.hl) ~ "/" ~ game.maxHealth(u).hl ~ ", range " ~ ((t.minRange > 1).??(t.minRange + "-") + game.rangeOf(u)).hl ~ ", sight " ~ game.sightOf(u).hl) ~
        Div("Attack " ~ game.attacksOf(u)./(a => a.icon ~ (game.full && game.fixedGun(u, a)).?("(fixed)".spn(styles.small)).|(Empty)).merge) ~
        ups.any.?(Div("Upgrades: " ~ ups./(_.elem).comma)).|(Empty) ~
        game.facing.get(u)./(e => Div("Facing " ~ Facing.elem(e))).|(Empty) ~
        game.hasSplash(u).?(Div("HE Splash".hl ~ ": its HE hits the target hex and every hex around it")).|(Empty) ~
        (game.full && t.retreatPoint).?(Div("Retreat Point".hl ~ ": Infantry may Retreat to the hexes next to it")).|(Empty) ~
        game.retreating.contains(u).?(Div("Retreating".hl)).|(Empty) ~
        game.stance.contains(u).?(Div("Defensive Stance".hl ~ ": Heavy Cover this round")).|(Empty) ~
        game.noFire.contains(u).?(Div("No-Fire Capture mode".hl ~ ": captures, doesn't fire (1 CP to switch back)")).|(Empty) ~
        u.utype.strafing.?(Div("Strafing Fire".hl ~ ": its damage may also go, in full, to a second unit next to its target")).|(Empty) ~
        (u.utype.tierBuilding > 0).?(Div("Tier Building".hl ~ ": its Tier's units may spawn next to it, with Slow (Double Slow if already Slow) their first round")).|(Empty) ~
        game.hullDown.contains(u).?(Div("Hull Down".hl ~ ": 2 Defense Dice until it moves")).|(Empty) ~
        game.assaults.get(u)./(d => Div("Assaulting " ~ d.elem)).|(Empty) ~
        Div(("Board: Tier " + t.tier + ", " + Tiers.name(u.faction, t.tier)).spn(styles.small))
    }

    override def onClick(a : Any) = a @@ {
        case ("notifications", Some(f : Faction)) =>
            shown = $
            showNotifications($(f))

        case ("notifications", None) =>
            shown = $
            showNotifications(game.setup)

        case ("board", f : Faction) =>
            val e = (game.setup.dropWhile(_ != f).drop(1) ++ game.setup).head
            showOverlay(overlayScrollX(Div(f.elem ~ " Building Boards " ~ OnClick(("board", e), Span(("(" + e.name + "'s)").txt, styles.small, xlo.pointer))) ~
                1.to(3)./(t => Div(Image("board:" + Tiers.board(f, t), styles.boardImg) ~ Div(("Tier " + t + ", " + Tiers.name(f, t) + (t > 1).??(" (unlock: " + Tiers.cost(t).mp + " Manpower, " + Tiers.cost(t).fuel + " Fuel)") + (t <= game.states(f).tier).?(" - unlocked").|("")).txt))).merge), onClick)

        case u : Squad =>
            showOverlay(overlayFitX(squadInfo(u)), onClick)

        // a player's Commander card (or the three drawn), its tiers marked as unlocked
        case ("commander", f : Faction) =>
            val l = game.commanderOf.get(f).$ ++ game.commanderOf.get(f).none.??(game.drawn.getOrElse(f, $[Commander]()))
            def card(c : Commander) = Div(Image(c.image, styles.boardImg)) ~ Div(c.elem) ~ c.tiers.zipWithIndex./{ case (t, i) =>
                val on = game.commanderOf.get(f).has(c) && i < game.tiersOf.getOrElse(f, 0)
                Div((on.?("✔ ").|("") + t.xp + " XP" + (t.needs > 0).??(", Tier " + t.needs)).spn(styles.small) ~ " " ~ t.ability.elem ~ ": " ~ t.ability.text.txt)
            }.merge
            showOverlay(overlayScrollX(Div(f.elem ~ " Commander" ~ (l.num > 1).?(" (three drawn, one to take)".txt).|(Empty)) ~ l./(card).merge ~
                (game.tiersOf.getOrElse(f, 0) >= 4).?(Div("All tiers unlocked: 2 XP trade for 1 Manpower, Munitions or Fuel in the purchases.".txt)).|(Empty)), onClick)

        // a player's Lost Brigade (or the three drawn)
        case ("brigade", f : Faction) =>
            val l = game.brigadeOf.get(f).$ ++ game.brigadeOf.get(f).none.??(game.brigadesDrawn.getOrElse(f, $[Brigade]()))
            def card(b : Brigade) = Div(Image(b.image, styles.boardImg)) ~ Div(b.elem ~ " " ~ ("(" + b.xp + " XP)").spn(styles.small) ~ ": " ~ b.text.txt)
            showOverlay(overlayScrollX(Div(f.elem ~ " Lost Brigade" ~ (l.num > 1).?(" (three drawn, one to pick)".txt).|(Empty)) ~ l./(card).merge), onClick)

        case "legend" =>
            def line(i : Elem, s : String) = Div(i ~ " " ~ s.txt)
            showOverlay(overlayScrollX(Div("Icons".hlb) ~ Div(
                Res.points./(r => line(r.icon, r.name + ": stockpile, and income each round")).merge ~
                line(XP.icon, "Experience: 1 a round, 1 for each enemy unit you hurt, 1 for each of your units lost") ~
                $(AI, AP, HE, Flame)./(d => line(d.icon, d.name + " damage")).merge ~
                line(Image("status:pinned", styles.icon), "Pinned (by a machine gun): can't attack, neutralize or capture this round") ~
                line(Image("status:setup", styles.icon), "Set up: the weapon team can fire, not move") ~
                line(Image("status:nofire", styles.icon), "Packed up: the weapon team can move, not fire") ~
                line(Image("status:slow", styles.icon), "Slow: one CP less this round") ~
                game.full.?(
                    line(Image("status:retreat", styles.icon), "Retreating: moves up to six hexes toward a Retreat Point (a Spawn Area hex, or next to the Major), and can't attack, spot, capture or spend CP until the round ends there") ~
                    line("▲".txt, "Facing: the side a Heavy Vehicle or set-up Weapon Team faces; a Weapon Team, and a fixed gun, fires only into the 120° in front, which is also a Heavy Vehicle's Frontal Armor (2 Defense Dice against AP from there)") ~
                    line(Image("status:splash", styles.icon), "HE Splash: the HE dice hit every unit the attacker's team sees in the target hex and around it, allies too")
                ).|(Empty) ~
                line(Image("status:flag-us", styles.icon), "Control Point held") ~
                Div("Yellow lines are Light Cover (1 Defense Die), green ones Heavy Cover (2), for Infantry in the hex when an attack crosses them. Infantry in a Building gets 3 Defense Dice, in a Destroyed Building 1. Red lines and water ringed in red can't be crossed. Buildings block sight and fire, except indirect fire.".txt) ~
                game.full.?(Div("Full Game Rules: tank traps keep Vehicles out of their hex; a Building has 2 health against HE and Flame and becomes a Destroyed Building (darkened); a unit moving into an enemy's hex Assaults it; a set-up machine gun deals 1 Overwatch damage to enemy Infantry moving onto a hex in its arc.".txt)).|(Empty), styles.legend)), onClick)

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
            $(ZBasic(Break ~ Break, "Building Boards".hh, () => { onClick(("board", self.|(game.setup.head))) }).copy(clear = false)) ++
            $(ZBasic(Break ~ Break, "Icons".hh, () => { onClick("legend") }).copy(clear = false)) ++
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
            $(ZBasic(Break ~ Break, "Main Menu".spn, () => { callbacks.returnToTitle() }).copy(clear = false)).%(_ => callbacks.canReturnToTitle) ++
            $(ZBasic(Break ~ Break, "My Online Games".spn, () => { callbacks.myGames() }).copy(clear = false)).%(_ => callbacks.canMyGames)
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

        showNotifications(self)

        super.wait(self, factions, message)
    }

    val foldFrom = 9
    var unfolded = false
    var folded = false
    var unfold : () => Unit = () => {}

    override def convertActions(faction : |[F], actions : $[UserAction], then : UserAction => Unit = null) : $[ZOption] = {
        val l = actions.%(a => a.unwrap.is[HexKey] && a.is[Unavailable].not && a.is[Hidden].not)

        val fold = then != null && unfolded.not && l./(_.unwrap.as[HexKey].get.hex).distinct.num >= foldFrom

        if (then != null && fold != folded) {
            folded = fold
            drawMap()
        }

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
        listedHexes = $
        highlightHex = None
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
            listedHexes = $
            then(a)
        })
    }

    override def styleAction(faction : |[Faction], actions : $[UserAction], a : UserAction, unavailable : Boolean, view : |[Any]) = {
        (unavailable).$(xstyles.unavailableText) ++
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
