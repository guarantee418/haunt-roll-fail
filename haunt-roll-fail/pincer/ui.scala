package pincer
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

import pincer.elem._

import hrf.ui.again._
import hrf.ui.sprites._


object UI extends BaseUI {
    val mmeta = Meta

    def create(uir : ElementAttachmentPoint, arity : Int, options : $[mmeta.O], resources : Resources, title : String, callbacks : hrf.Callbacks) = new UI(uir, arity, resources, callbacks)
}

class UI(val uir : ElementAttachmentPoint, arity : Int, val resources : Resources, callbacks : hrf.Callbacks) extends MapGUI {
    def factionElem(f : Faction) = f.name.styled(f)

    // on a phone the player panels are taller, for text of a readable size (index.html)
    lazy val narrow = dom.window.innerWidth < 700

    val statuses = 1.to(arity)./(i => newPane("status-" + i, Content, styles.status, styles.fstatus, ExternalStyle("hide-scrollbar")))
    val statusGame = newOuterPane("status-game", Content)

    // the hexes of the board, found by their centers
    object regions extends Regions[Hex] {
        def center(h : Hex) = XY(Board.x(h), Board.y(h))

        def regionAt(p : XY) : |[Hex] = {
            val h = Board.hexes.minBy(h => (Board.x(h) - p.x) * (Board.x(h) - p.x) + (Board.y(h) - p.y) * (Board.y(h) - p.y))
            val d = math.sqrt((Board.x(h) - p.x) * (Board.x(h) - p.x) + (Board.y(h) - p.y) * (Board.y(h) - p.y))
            (d < 64 && p.x > 30 && p.y > 30 && p.x < Board.width - 30 && p.y < Board.height - 30).?(h)
        }

        def random(h : Hex) : XY = {
            val a = math.random() * 2 * math.Pi
            val r = math.sqrt(math.random()) * 34
            XY(Board.x(h) + r * math.cos(a), Board.y(h) + 8 + r * math.sin(a))
        }
    }

    lazy val units = new OrderedLayer
    lazy val structures = new OrderedLayer
    lazy val highlighted = new OrderedLayer
    lazy val labels = new OrderedLayer

    val width = Board.width
    val height = Board.height
    val margins = Margins(0, 0, 0, 0)

    lazy val scene = {
        val mp = img("map")

        val background = new OrderedLayer
        background.add(Sprite($(ImageRect(new RawImage(mp), Rectangle(0, 0, mp.width, mp.height), 1.0)), $))(0, 0)

        val areas = new HitLayer(regions)

        new Scene($(background, highlighted, structures, areas, units, labels), mp.width, mp.height, margins)
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

    // the hexes of the choice the mouse is over in the list, and of the code it is over in the log
    var listedHexes = $[Hex]()
    var loggedHex : |[Hex] = None

    // the hexes a choice names
    def hexesOf(a : UserAction) : $[Hex] = a.unwrap match {
        case a : BuildRoadAction => $(a.a, a.b)
        case a : BuildBridgeAction => $(a.a, a.w, a.b)
        case a : HexKey => $(a.hex)
        case a : FigureKey => game.pos.get(a.figure).$
        case _ => $
    }

    // the codes of the hexes (D4) aren't on the board: they show on the hex the mouse is over, on those the listed choices name,
    // and on one whose code the mouse is over (or a tap is on) in the log
    var labelImages = Map[String, CanvasImage]()

    def labelImage(s : String) = labelImages.getOrElse(s, {
        val canvas = dom.document.createElement("canvas").asInstanceOf[dom.html.Canvas]
        canvas.width = 128
        canvas.height = 72
        val g = canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]
        g.font = "bold 46px 'Avenir Next', 'Segoe UI', Roboto, Arial, sans-serif"
        g.textAlign = "center"
        g.textBaseline = "middle"
        g.fillStyle = "#14181f"
        val w = g.measureText(s).width + 32
        g.fillRect(64 - w / 2, 6, w, 60)
        g.fillStyle = "#f3eee2"
        g.fillText(s, 64, 38)
        val i = new CanvasImage(canvas)
        labelImages += s -> i
        i
    })

    def locate(h : |[Hex]) {
        if (h != loggedHex) {
            loggedHex = h
            drawMap()
        }
    }

    dom.document.addEventListener("pointerover", (e : dom.PointerEvent) => {
        val hex = e.target @@ {
            case t : dom.Element =>
                // (the code is in a span of its own in the styled one)
                val code = t.asInstanceOf[scalajs.js.Dynamic].closest("[class*=hrf-hanging] .pincer-hex").asInstanceOf[dom.Element]
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

    def figureActions(u : Figure) = lastActions.%(_.unwrap @@ {
        case a : FigureKey => a.figure == u
        case _ => false
    })

    def processRightClick(target : $[Any], xy : XY) {
    }

    def processHighlight(target : $[Any], xy : XY) {
        highlightHex = target.of[Hex].single
        val any = target.exists {
            case (_ : Hex, u : Figure) => figureActions(u).any
            case h : Hex => hexActions(h).any
            case _ => false
        }
        mapSmall.attach.parent.style.cursor = any.?("pointer").|("default")
    }

    def processTargetClick(target : $[Any], xy : XY) {
        val figures = target.collect { case (_ : Hex, u : Figure) => u }

        figures.foreach { u =>
            val l = figureActions(u)
            if (l.any)
                return choose(l)
        }

        target.of[Hex].foreach { h =>
            val l = hexActions(h)
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

    def scaled(name : String, size : Double, rotate : Boolean = false, flip : Boolean = false, alpha : Double = 1.0) : ImageRect = {
        val i = img(name)
        val k = size / max(i.width, i.height)
        val w = i.width * k
        val h = i.height * k
        if (rotate)
            ImageRect(new RawImageTurned(i, 1), Rectangle(-h / 2, -w / 2, h, w), alpha)
        else
        if (flip)
            ImageRect(new RawImageRotated180(i), Rectangle(-w / 2, -h / 2, w, h), alpha)
        else
            ImageRect(new RawImage(i), Rectangle(-w / 2, -h / 2, w, h), alpha)
    }

    def makeScene() : |[Scene] = {
        if (img("map").complete.not)
            return None

        units.clear()
        structures.clear()
        highlighted.clear()
        labels.clear()

        def at(h : Hex, i : ImageRect, dx : Double = 0, dy : Double = 0) = structures.add(Sprite($(i), $))(Board.x(h) + dx, Board.y(h) + dy)

        // where the current choices are
        val hexes = lastActions./~(_.unwrap @@ {
            case a : HexKey => $(a.hex)
            case _ => $()
        }).distinct

        val targets = lastActions./~(_.unwrap @@ {
            case a : AttackAction => $(a.target)
            case a : FigureKey => $(a.figure)
            case _ => $()
        }).distinct

        // a quiet outline on every choice, a strong one where the mouse is (on the map or in the list)
        val many = hexes.num > 8
        hexes.foreach { h =>
            val strong = highlightHex.has(h) || listedHexes.has(h)
            val select = ImageRect(new RawImage(img("hex-select")), Rectangle(-60, -69, 120, 138), (strong || many.not).?(1.0).|(0.8))
            // drawn over itself it is brighter and fills the hex
            highlighted.add(Sprite(strong.?($(select, select, select)).|($(select)), $))(Board.x(h), Board.y(h))
        }

        lastActions.collect { case a : AttackAction => a.target }.distinct.foreach { u =>
            game.pos.get(u).foreach { h =>
                highlighted.add(Sprite($(ImageRect(new RawImage(img("hex-target")), Rectangle(-60, -69, 120, 138), 0.8)), $))(Board.x(h), Board.y(h))
            }
        }

        game.bridges.foreach { b =>
            val x = (Board.x(b.a) + Board.x(b.b)) / 2
            val y = (Board.y(b.a) + Board.y(b.b)) / 2
            structures.add(Sprite($(scaled("piece:bridge-" + b.owner.id + "-" + Board.direction(b.a, b.b), 180)), $))(x, y)
        }

        game.ports.foreach { case (h, p) =>
            at(h, scaled("piece:port-" + p.owner.id, 92))
        }

        game.roads.foreach { r =>
            val x = (Board.x(r.a) + Board.x(r.b)) / 2
            val y = (Board.y(r.a) + Board.y(r.b)) / 2
            val name = "piece:road-" + r.owner.id + "-" + Board.direction(r.a, r.b)
            structures.add(Sprite($(scaled(name, (Board.direction(r.a, r.b) == "e").?(74).|(72))), $))(x, y)
        }

        game.colonies.foreach { case (h, f) =>
            game.forts.get(h).foreach { t =>
                at(h, scaled("piece:fort-" + t.owner.id, 124, flip = t.damaged, alpha = t.damaged.?(0.75).|(1.0)), 0, 4)
            }
            at(h, scaled("piece:colony-" + f.id, 84), -12, -16)
        }

        // the units of a hex in up to two rows, the lower one in front, beside the colony if there is one
        Board.hexes.foreach { h =>
            val l = game.on(h)
            val colony = game.colonies.contains(h) || game.ports.contains(h)
            val perRow = 3
            l.zipWithIndex.foreach { case (u, k) =>
                val injured = game.isInjured(u)
                val size = (u.kind == Frigate).?(66).|(54) * (l.num > 3).?(0.8).|(1.0)
                val i = scaled("piece:" + u.kind.id + "-" + u.faction.id, size, rotate = injured)
                val box = Rectangle(i.rect.x, i.rect.y, i.rect.width, i.rect.height)
                val mark = if (targets.has(u)) $(ImageRect(new RawImage(img("hex-target")), Rectangle(-24, -27, 48, 54), 0.7)) else $[ImageRect]()
                val row = k / perRow
                val inRow = min(perRow, l.num - row * perRow)
                val dx = ((k % perRow) - (inRow - 1) / 2.0) * 34 + colony.?(26).|(0)
                val dy = (row == 0).?(18).|(-20) + colony.?(8).|(0) + game.embarked.contains(u).?(-10).|(0)
                units.add(Sprite(mark :+ i, $(box), $((h, u))), 1.0, 3 - row)(Board.x(h) + dx, Board.y(h) + dy)
            }
        }

        val listed = folded.?(lastActions.%(_.unwrap.is[HexKey].not)).|(lastActions)

        (highlightHex.$ ++ listedHexes ++ loggedHex.$ ++ listed./~(hexesOf)).distinct.foreach { h =>
            labels.add(Sprite($(ImageRect(labelImage(h.name), Rectangle(-32, -18, 64, 36), 0.92)), $))(Board.x(h), Board.y(h) + 40)
        }

        // a hex found from the log, marked too
        loggedHex.foreach { h =>
            val select = ImageRect(new RawImage(img("hex-select")), Rectangle(-60, -69, 120, 138), 1.0)
            highlighted.add(Sprite($(select, select, select), $))(Board.x(h), Board.y(h))
        }

        |(scene)
    }

    def factionColor(f : Faction) = f match {
        case Red => "#e0453c"
        case Green => "#3fae4f"
        case Blue => "#6f8dff"
        case Yellow => "#e8c42e"
    }

    def factionStatus(f : Faction) {
        val container = statuses(game.setup.indexOf(f))

        val name = resources.getName(f).|(f.name)

        val badge = (f == game.current).?("Turn".styled(styles.badge)).|((game.highlightFaction.has(f)).?("Choosing".styled(styles.badge)).|(Empty))

        val title = Div(Span(name).styled(f) ~ badge, styles.name, styles.title)

        if (game.factions.has(f).not && game.current != null) {
            container.replace(Div(title ~ Div("Surrendered".hl)).div(xstyles.fillHeight).pointer.onClick.param("reference"), resources, onClick)
            return
        }

        val p = game.states(f)

        val seals = game.sealsOf(f)

        val sealRow = Div(Icons.only("seal", "Royal Seals (8 win)") ~ (seals.num.hlb ~ "/8".txt) ~ " " ~ seals./(s => OnClick(s, Image("seal:" + s.image, styles.sealSmall, xlo.pointer))).merge)

        val hand = Div(Span(Icons.only("cards", "Cards in hand") ~ p.hand.num.hlb, styles.stat) ~ Span(Icons.only("trade", "Trade rate") ~ (game.rate(f) + ":1").hlb, styles.stat) ~ Span(Icons.only("war-card", "War cards left") ~ p.warCards.hlb, styles.stat))(styles.small)

        // swords, then the enemies in Gentleman's Wars; a flame, then those in Boar's Wars
        val wars = game.wars.%(w => w.declarer == f || w.target == f)

        val warGroups = $(false, true)./~{ boar =>
            wars.%(_.boar == boar)./{ w =>
                val e = (w.declarer == f).?(w.target).|(w.declarer)
                val later = game.gentleman.has((f, e))
                Hint("At war with " + e.name + boar.?(" (Boar's War)").|(" (Gentleman's War)") + later.?(" (from the next turn)").|(""),
                    Image("piece:colony-" + e.id, styles.iconOnly, e.name) ~ e.short.styled(e) ~ later.?(Image("icon:end-turn", styles.piece, "from the next turn")).|(Empty), xlo.nowrap) : Elem
            }.some./(l => Icons.only(boar.?("boar").|("war"), boar.?("Boar's War").|("Gentleman's War")) ~ l.merge)
        }

        val warRow = Div(warGroups.some./(_.join(" ")).|(Icons.only("peace", "At peace")))

        val units = game.units(f)

        val pieceRow = Div(
            Span(Image("piece:colony-" + f.id, styles.piece) ~ game.colonyOf(f).num.hlb, styles.stat) ~
            Kind.all./{ k =>
                val l = units.%(_.kind == k)
                val inj = l.count(game.isInjured)
                Span(Image("piece:" + k.id + "-" + f.id, styles.piece) ~ l.num.hlb ~ (inj > 0).?((" (" + inj + " injured)").txt).|(Empty), styles.stat)
            }.merge
        )(styles.row)

        val captured = p.captured.num
        // every prisoner, in its own color
        val capRow = (captured > 0).?(Div(Span("Captured ") ~ p.captured.map(_._2).map(u => Hint(u.faction.name + " " + u.kind.name, Image("piece:" + u.kind.id + "-" + u.faction.id, styles.piece, u.faction.name + " " + u.kind.name)) : Elem).merge)(styles.small)).|(Empty)

        // tapping the panel (but not a seal on it) opens the Player Reference
        container.replace((title ~ sealRow ~ warRow ~ hand ~ pieceRow ~ capRow).div(xstyles.fillHeight).pointer.onClick.param("reference"), resources, onClick)

        if (f == game.current || game.highlightFaction.has(f)) {
            container.attach.parent.style.outline = "3px " + (f == game.current).?("solid").|("dashed") + " " + factionColor(f)
        }
        else
            container.attach.parent.style.outline = ""
    }

    def gameStatus() {
        val dice = game.rolled./(r => Image("die:" + r.id, styles.die)).merge

        val decks = Div(Resource.all./(r => Span(Image("die:" + r.id, styles.die) ~ " " ~ game.decks(r).num.hlb, styles.deck)).merge)(styles.small)

        val companies = Seal.companies.%(c => game.seals.contains(c))./(c => Div(c.elem ~ " " ~ game.seals(c).elem ~ " (" ~ game.stockpiles(c.resource).num.hl ~ ")")(styles.small))

        statusGame.replace(overlayFit(Div(Div("Turn " ~ game.turn.hl ~ " " ~ dice) ~ decks ~ companies.merge ~
            (Icons("seal") ~ "Royal Seals".hh).div(xlo.pointer).onClick.param("seals"), styles.gstatus)), resources, onClick)
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
            BasicPane("status", 14, narrow.?(21.0).|(10.6), Priorities(top = 3, right = 2, maxXscale = 1.8, maxYscale = 1.8)),
            BasicPane("status-game", 16, 12, Priorities(maxXscale = 1.2, maxYscale = 1.2)),
            BasicPane("log", 30, 13, Priorities(right = 1)),
            BasicPane("map-small", 60, 60, Priorities(top = 2, left = 1, grow = -1)),
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

    val layoutKey = "v" + 2 + "." + "arity-" + arity + narrow.??(".narrow")

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

    override def onClick(a : Any) = a @@ {
        case ("notifications", Some(f : Faction)) =>
            shown = $
            showNotifications($(f))

        case ("notifications", None) =>
            shown = $
            showNotifications(game.factions)

        case "seals" =>
            showOverlay(overlayScrollX(Div("Royal Seals".hlb) ~ Div("Eight of the fourteen win the game.") ~
                Seal.all./{ s =>
                    Div(OnClick(s, Image("seal:" + s.image, styles.sealImg, xlo.pointer)) ~ " " ~ s.elem ~ " " ~ game.seals.get(s)./(f => "held by " ~ f.elem).|("unclaimed".txt) ~ Break ~ SealText.of(s).spn(styles.small))
                }.merge), onClick)

        // touch screens have no tooltips: what every icon on the panels means (the Icons button)
        case "legend" =>
            def line(i : Elem, s : String) = Div(i ~ " " ~ s.txt)
            showOverlay(overlayScrollX(Div("Icons".hlb) ~ Div(
                line(Icons("seal"), "Royal Seals held; eight win") ~
                line(Icons("cards"), "Cards in hand") ~
                line(Icons("trade"), "Cards paid for one in trade") ~
                line(Icons("war-card"), "War cards left") ~
                line(Icons("peace"), "At peace") ~
                line(Icons("war"), "Gentleman's War, with the player shown after it") ~
                line(Icons("boar"), "Boar's War, with the player shown after it") ~
                line(Icons("end-turn"), "Beside a war: attacks from the next turn") ~
                line(Icons.piece("colony", game.setup.head), "Colonies") ~
                Kind.all./(k => line(Icons.piece(k.id, game.setup.head), k.name)).merge ~
                Resource.all./(r => line(Image("die:" + r.id, styles.icon), r.name)).merge, styles.legend)), onClick)

        case "reference" =>
            showOverlay(overlayFitX(Image("card:reference", styles.artwork)), onClick)

        case s : Seal =>
            showOverlay(overlayFitX(Image("seal:" + s.image, styles.artwork) ~ Div(s.elem ~ Break ~ SealText.of(s))), onClick)

        case c : Card =>
            showOverlay(overlayFitX(Image("card:" + c.image, styles.artwork)), onClick)

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

    override def preinfo(self : |[Faction], aa : $[UserAction]) = {
        val ii = currentGame.preinfo($, self, aa)
        ii.any.??(convertActions(self, ii))
    }

    override def info(self : |[Faction], aa : $[UserAction]) = {
        val ii = currentGame.info($, self, aa)
        ii.any.??($(ZOption(Empty, Break)) ++ convertActions(self.of[Faction], ii)) ++
            $(ZBasic(Break ~ Break, Icons("seal") ~ "Royal Seals".hh, () => { onClick("seals") }).copy(clear = false)) ++
            $(ZBasic(Break ~ Break, Icons("list") ~ "Icons".hh, () => { onClick("legend") }).copy(clear = false)) ++
            (currentGame.isOver && hrf.HRF.flag("replay").not).$(
                ZBasic(Break ~ Break ~ Break, "Save Replay As File".hh, () => {
                    showOverlay(overlayScrollX("Saving Replay...".hl.div).onClick, null)

                    callbacks.saveReplay {
                        overlayPane.invis()
                        overlayPane.clear()
                    }
                }).copy(clear = false)
            ) ++
            $(ZBasic(Break ~ Break, Icons("notifications") ~ "Notifications".spn, () => { onClick("notifications", self) }).copy(clear = false)).%(_ => self.any || game.isOver) ++
            $(ZBasic(Break ~ Break, Icons("interface") ~ "Interface".spn, () => { callbacks.editSettings { updateStatus() } }).copy(clear = false)) ++
            $(ZBasic(Break ~ Break, Icons("bug") ~ "Report a Bug".spn, () => { callbacks.reportBug() }).copy(clear = false)).%(_ => callbacks.canReportBug) ++
            $(ZBasic(Break ~ Break, Icons("home") ~ "Main Menu".spn, () => { callbacks.returnToTitle() }).copy(clear = false)).%(_ => callbacks.canReturnToTitle) ++
            $(ZBasic(Break ~ Break, "My Online Games".spn, () => { callbacks.myGames() }).copy(clear = false)).%(_ => callbacks.canMyGames)
    }

    var shown : $[Notification] = $
    var primed = false

    override def showNotifications(self : $[F]) : Unit = {
        // a game loaded or replayed shows only what comes after
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

    // a long list of hexes is folded, as they are all highlighted on the map, until it is asked for
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
            ZOption(Div(l.head.question(currentGame))(xlo.fullwidth), Div(Icons("hex") ~ "Tap a highlighted hex on the map", xstyles.info, xstyles.xx, xstyles.thu, xlo.fullwidth)) ::
            ZOption(Div(l.head.question(currentGame))(xlo.fullwidth), OnClick(Div(Icons("list") ~ "Show the " ~ l.num.hl ~ " choices as a list", $(xstyles.choice, xstyles.xx, xstyles.thu, xlo.pointer, xlo.fullwidth) ++ faction./(f => elem.borders.get(f)).$)), _ => {
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
        val card = view.exists(_.is[Card]) || view.exists(_.is[Seal])

        (unavailable && card).$(xstyles.unavailableCard) ++
        (unavailable && card.not).$(xstyles.unavailableText) ++
        (card.not).??(a @@ {
            case _ : Info => $(xstyles.info, xstyles.xx, xstyles.thu)
            case _ => $(xstyles.choice, xstyles.xx, xstyles.thu)
        }) ++
        (a @@ {
            case _ : Choice | _ : Cancel | _ : OnClickInfo => $(xlo.pointer)
            case _ => $()
        }) ++
        card.?(styles.inline).$ ++
        card.not.$(xlo.fullwidth) ++
        faction./(f => elem.borders.get(f)).$
    }
}

object SealText {
    def of(s : Seal) : String = s match {
        case FoundersOath => "First to found 2 colonies after the starting one. Choose either die at every harvest. Stolen by an enemy occupying all your colonies."
        case MaritimeExchange => "First to build 2 ports. Your ports keep their trade bonus while blockaded. Stolen by an enemy blockading all your ports."
        case StonewallSociety => "First to build 2 forts. Repair a damaged fort at once, even on another player's turn (as your Counter-Action). Stolen when all your forts are destroyed."
        case CrossroadsGuild => "First to complete 2 road networks. Revive units with any resource cards. Stolen when all your road networks are cut off."
        case c : Company => "Trading company for " + c.resource.name + ": founded with 5 " + c.resource.name + " from a colony on its terrain (a port for fish). Others trading for " + c.resource.name + " pay you instead of the bank. Forcible buyout with more cards than its stockpile."
        case RoyalMusket => "Most captures by Musketeers. Your musketeers may attack while injured."
        case MountedVanguard => "Most captures by Cavalry. Your cavalry moves up to 3."
        case BlackPowderVolley => "Most captures by Cannons. Your cannons may attack twice a turn."
        case NavalSupremacy => "Most captures by Frigates. Your frigates may be revived at once, at the normal cost."
    }
}
