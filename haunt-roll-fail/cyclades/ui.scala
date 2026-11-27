package cyclades
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

import cyclades.elem._

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

    lazy val layout = Layouts.of(arity)

    // the sea spaces and isles, found by their cells' centers
    object regions extends Regions[Area] {
        def center(a : Area) = a match {
            case Sea(c) => XY(layout.px(c), layout.py(c))
            case i : Isle => val (x, y) = layout.center(i) ; XY(x, y)
        }

        def regionAt(p : XY) : |[Area] = {
            val c = layout.cells.minBy(c => (layout.px(c) - p.x) * (layout.px(c) - p.x) + (layout.py(c) - p.y) * (layout.py(c) - p.y))
            val d = math.sqrt((layout.px(c) - p.x) * (layout.px(c) - p.x) + (layout.py(c) - p.y) * (layout.py(c) - p.y))
            (d < layout.dx * 0.55).?(layout.areaOf(c))
        }

        def random(a : Area) : XY = center(a)
    }

    lazy val units = new OrderedLayer
    lazy val structures = new OrderedLayer
    lazy val highlighted = new OrderedLayer
    lazy val labels = new OrderedLayer

    lazy val width = layout.width
    lazy val height = layout.height
    val margins = Margins(0, 0, 0, 0)

    lazy val scene = {
        val mp = img(layout.image)

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

    var highlightArea : |[Area] = None
    var listedAreas = $[Area]()

    def areasOf(a : UserAction) : $[Area] = a.unwrap match {
        case a : AreaKey => $(a.area)
        case _ => $
    }

    override def updateHighlight(a : |[UserAction]) {
        listedAreas = a./~(areasOf)
        drawMap()
    }

    def areaActions(x : Area) = lastActions.%(_.unwrap @@ {
        case a : AreaKey => a.area == x
        case _ => false
    })

    def processRightClick(target : $[Any], xy : XY) {
    }

    def processHighlight(target : $[Any], xy : XY) {
        highlightArea = target.of[Area].single
        mapSmall.attach.parent.style.cursor = target.of[Area].exists(a => areaActions(a).any).?("pointer").|("default")
    }

    def processTargetClick(target : $[Any], xy : XY) {
        target.of[Area].foreach { x =>
            val l = areaActions(x)
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

    // a number on the map: white on a dark disc
    var labelImages = Map[String, CanvasImage]()

    def labelImage(s : String) = labelImages.getOrElse(s, {
        val canvas = dom.document.createElement("canvas").asInstanceOf[dom.html.Canvas]
        canvas.width = 72
        canvas.height = 72
        val g = canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]
        g.font = "bold 46px 'Avenir Next', 'Segoe UI', Roboto, Arial, sans-serif"
        g.textAlign = "center"
        g.textBaseline = "middle"
        g.fillStyle = "#14181f"
        g.beginPath()
        g.arc(36, 36, 32, 0, 2 * math.Pi)
        g.fill()
        g.fillStyle = "#f3eee2"
        g.fillText(s, 36, 39)
        val i = new CanvasImage(canvas)
        labelImages += s -> i
        i
    })

    def scaled(name : String, size : Double, alpha : Double = 1.0) : ImageRect = {
        val i = img(name)
        val k = size / max(i.width, i.height)
        val w = i.width * k
        val h = i.height * k
        ImageRect(new RawImage(i), Rectangle(-w / 2, -h / 2, w, h), alpha)
    }

    def count(n : Int, size : Double) = ImageRect(labelImage(n.toString), Rectangle(size * 0.15, size * 0.05, size * 0.42, size * 0.42), 0.95)

    def makeScene() : |[Scene] = {
        if (img(layout.image).complete.not)
            return None

        units.clear()
        structures.clear()
        highlighted.clear()
        labels.clear()

        val d = layout.dx

        def cellsOf(a : Area) : $[Cell] = a match {
            case Sea(c) => $(c)
            case i : Isle => layout.data(i).cells
        }

        // a ring on every choice's cells, brighter where the mouse is
        val chosen = lastActions./~(areasOf).distinct
        chosen.foreach { a =>
            val strong = highlightArea.has(a) || listedAreas.has(a)
            cellsOf(a).foreach { c =>
                val r = ImageRect(new RawImage(img("select")), Rectangle(-d * 0.5, -d * 0.5, d, d), strong.?(1.0).|(0.75))
                highlighted.add(Sprite(strong.?($(r, r)).|($(r)), $))(layout.px(c), layout.py(c))
            }
        }

        game.battle.foreach { b =>
            cellsOf(b.area).foreach { c =>
                highlighted.add(Sprite($(ImageRect(new RawImage(img("target")), Rectangle(-d * 0.5, -d * 0.5, d, d), 1.0)), $))(layout.px(c), layout.py(c))
            }
        }

        // isles: the buildings and Metropolis in a row above the middle, the Troops or the owner's marker below it
        layout.islands.foreach { i =>
            val (x, y) = layout.center(i)
            val bb = game.buildingsOn(i)./(b => "building:" + b.id) ++ game.metropolises.contains(i).$("building:metropolis")
            bb.zipWithIndex.foreach { case (b, k) =>
                val size = b.endsWith("metropolis").?(d * 0.6).|(d * 0.36)
                structures.add(Sprite($(scaled(b, size)), $))(x + (k - (bb.num - 1) / 2.0) * d * 0.4, y - d * 0.3)
            }
            val p = game.prosperity.getOrElse(i, 0)
            if (p > 0) {
                val pr = scaled("prosperity", d * 0.32)
                structures.add(Sprite((p > 1).?($(pr, count(p, d * 0.32))).|($(pr)), $))(x - d * 0.38, y + d * 0.2)
            }
            game.owner.get(i).foreach { f =>
                val n = game.troopsAt(i)
                if (n > 0) {
                    val t = scaled("piece:troop-" + f.id, d * 0.5)
                    units.add(Sprite((n > 1).?($(t, count(n, d * 0.5))).|($(t)), $))(x + d * 0.05, y + d * 0.18)
                }
                else
                    units.add(Sprite($(scaled("piece:territory-" + f.id, d * 0.32)), $))(x + d * 0.05, y + d * 0.18)
            }
        }

        layout.seas.foreach { s =>
            game.fleets.get(s).foreach { st =>
                val t = scaled("piece:fleet-" + st.f.id, d * 0.55)
                units.add(Sprite((st.n > 1).?($(t, count(st.n, d * 0.55))).|($(t)), $))(layout.px(s.cell), layout.py(s.cell))
            }
        }

        game.moving.foreach { m =>
            val t = scaled("piece:fleet-" + m.f.id, d * 0.55, 0.85)
            units.add(Sprite((m.n > 1).?($(t, count(m.n, d * 0.55))).|($(t)), $))(layout.px(m.at.cell) + d * 0.12, layout.py(m.at.cell) - d * 0.12)
        }

        game.figures.foreach { case (c, a) =>
            val (x, y) = a match {
                case Sea(cl) => (layout.px(cl), layout.py(cl))
                case i : Isle => val (x, y) = layout.center(i) ; (x + d * 0.35, y + d * 0.05)
            }
            units.add(Sprite($(scaled("figure:" + c.id, d * 0.42)), $))(x, y)
        }

        |(scene)
    }

    def factionColor(f : Faction) = f match {
        case Red => "#f0473f"
        case Yellow => "#f2cb2a"
        case Blue => "#5a9cff"
        case Black => "#c8c8d0"
        case Green => "#46c25a"
    }

    def factionStatus(f : Faction) {
        val container = statuses(game.setup.indexOf(f))

        val name = resources.getName(f).|(f.name)

        val badge = (f == game.current).?("Turn".styled(styles.badge)).|((game.highlightFaction.has(f)).?("Choosing".styled(styles.badge)).|(Empty))

        val title = Div(Span(name).styled(f) ~ badge, styles.name, styles.title)

        val p = game.states(f)

        val gold = Div(Span(p.gold.hlb ~ " GP", styles.stat) ~ Span(Image("card:priest", styles.piece) ~ " " ~ p.priests.hlb, styles.stat) ~ Span(Image("card:philosopher", styles.piece) ~ " " ~ p.philosophers.hlb, styles.stat))

        val pieces = Div(
            Span(Image("piece:fleet-" + f.id, styles.piece) ~ " " ~ game.fleetsOn(f).hlb ~ "/8", styles.stat) ~
            Span(Image("piece:troop-" + f.id, styles.piece) ~ " " ~ game.troopsOn(f).hlb ~ "/8", styles.stat) ~
            Span(("Isles " ~ game.isles(f).num.hlb), styles.stat)
        )(styles.row)

        val built = Div(
            Building.all./(b => Span(Image("building:" + b.id, styles.buildingIcon) ~ game.isles(f)./(i => game.buildingsOn(i).count(_ == b)).sum.hlb, styles.stat)).merge ~
            Span(Image("building:metropolis", styles.buildingIcon) ~ game.metropolisesOf(f).hlb ~ ("/" + game.goal).txt, styles.stat)
        )(styles.row)

        val income = Div("Revenue ".txt ~ game.revenue(f).hl ~ " GP")(styles.small)

        container.replace((title ~ gold ~ pieces ~ built ~ income).div(xstyles.fillHeight), resources, onClick)

        if (f == game.current || game.highlightFaction.has(f))
            container.attach.parent.style.outline = "3px " + (f == game.current).?("solid").|("dashed") + " " + factionColor(f)
        else
            container.attach.parent.style.outline = ""
    }

    def gameStatus() {
        def offer(g : God) : Elem = g match {
            case Apollo => game.apollo./(m => m.f.elem).some./(l => " " ~ l.comma).|(Empty)
            case _ => game.bids.get(g)./{ case (m, n) => " " ~ m.f.elem ~ " " ~ n.hl }.|(Empty)
        }

        val gods = (game.gods :+ Apollo)./(g => Div(Image("god:head-" + g.id, styles.godHead) ~ " " ~ g.elem ~ offer(g))).merge

        val hidden = game.hidden.any.?(Div(("Face down: ".txt ~ game.hidden./(_.elem).comma))(styles.small)).|(Empty)

        val track = Div(game.track.zipWithIndex./{ case (c, i) =>
            Span(c./(c => OnClick(c, Image("creature:" + c.id, styles.trackCard, xlo.pointer)) : Elem).|(Span(" — ".txt)) ~ Div(Creature.prices(i).hl ~ " GP")(styles.small), styles.inline)
        }.merge)

        val dice = game.battle.any.?(Div("Dice ".txt ~ game.lastRoll./(_.hl).comma)).|(Empty)

        statusGame.replace(overlayFit(Div(Div("Cycle " ~ game.cycle.hl) ~ gods ~ hidden ~ track ~ dice ~
            Div(("Prosperity markers left " ~ game.prosperityLeft.hl))(styles.small), styles.gstatus)), resources, onClick)
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
            BasicPane("status", 14, narrow.?(22.0).|(11.5), Priorities(top = 3, right = 2, maxXscale = 1.8, maxYscale = 1.8)),
            BasicPane("status-game", 16, 26, Priorities(maxXscale = 1.2, maxYscale = 1.2)),
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

    override def onClick(a : Any) = a @@ {
        case ("notifications", Some(f : Faction)) =>
            shown = $
            showNotifications($(f))

        case ("notifications", None) =>
            shown = $
            showNotifications(game.factions)

        case "creatures" =>
            showOverlay(overlayScrollX(Div("Creatures".hlb) ~
                Creature.all./{ c =>
                    Div(OnClick(c, Image("creature:" + c.id, styles.trackCard, xlo.pointer)) ~ " " ~ c.elem ~ " " ~ game.figures.get(c)./(x => "on " ~ x.elem).|(Empty) ~ Break ~ c.text.spn(styles.small))
                }.merge), onClick)

        case "gods" =>
            showOverlay(overlayScrollX(Div("The Gods".hlb) ~ (God.main :+ Apollo)./(g => Div(Image("god:" + g.id, styles.godTile))).merge), onClick)

        case c : Creature =>
            showOverlay(overlayFitX(Image("creature:" + c.id, styles.artwork) ~ Div(c.elem ~ Break ~ c.text)), onClick)

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
        $(ZBasic(Break ~ Break, "The Gods".hh, () => { onClick("gods") }).copy(clear = false)) ++
            $(ZBasic(Break ~ Break, "Creatures".hh, () => { onClick("creatures") }).copy(clear = false)) ++
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

    // a long list of places is folded, as they are all ringed on the map, until it is asked for
    val foldFrom = 9
    var unfolded = false
    var folded = false
    var unfold : () => Unit = () => {}

    override def convertActions(faction : |[F], actions : $[UserAction], then : UserAction => Unit = null) : $[ZOption] = {
        val l = actions.%(a => a.unwrap.is[AreaKey] && a.is[Unavailable].not && a.is[Hidden].not)

        val fold = then != null && unfolded.not && l./(_.unwrap.as[AreaKey].get.area).distinct.num >= foldFrom

        if (then != null && fold != folded) {
            folded = fold
            drawMap()
        }

        if (fold.not)
            super.convertActions(faction, actions, then)
        else
            ZOption(Div(l.head.question(currentGame))(xlo.fullwidth), Div("Tap a ringed isle or sea space on the map", xstyles.info, xstyles.xx, xstyles.thu, xlo.fullwidth)) ::
            ZOption(Div(l.head.question(currentGame))(xlo.fullwidth), OnClick(Div("Show the " ~ l.num.hl ~ " choices as a list", $(xstyles.choice, xstyles.xx, xstyles.thu, xlo.pointer, xlo.fullwidth) ++ faction./(f => elem.borders.get(f)).$)), _ => {
                unfolded = true
                unfold()
            }, false) ::
            super.convertActions(faction, actions.diff(l), then)
    }

    override def ask(faction : |[F], actions : $[UserAction], then : UserAction => Unit) {
        lastActions = actions
        lastThen = then
        listedAreas = $
        highlightArea = None
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
            listedAreas = $
            then(a)
        })
    }

    override def styleAction(faction : |[Faction], actions : $[UserAction], a : UserAction, unavailable : Boolean, view : |[Any]) = {
        val card = view.exists(_.is[Creature]) && a.is[Info]

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
