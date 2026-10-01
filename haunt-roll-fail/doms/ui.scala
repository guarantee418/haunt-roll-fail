package doms
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

import doms.elem._

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

    val market : CanvasPane = newCanvasPane("market", 2) { bitmap =>
        val n = 5
        val d = 12

        val cards = new OrderedLayer

        if (game.market.any) {
            0.until(n).foreach { i =>
                if (i < game.market.num) {
                    val c = game.market(i)

                    cards.add(Sprite($(ImageRect(new RawImage(img(c.id)), Rectangle(0, 0, 392, 548), 1.0)), $))(392*i + d*i, 548*0)
                }
            }
        }

        val scene = new Scene($(cards), 392*n + d*n - d, 548, Margins(d, d, d, d))

        if (resources.images.incomplete.none)
            scene.render(bitmap.context, bitmap.width, bitmap.height, 1, 0, 0, Inside)

        if (resources.images.incomplete.any)
            setTimeout(min(25, resources.images.incomplete.num) * 20)(market.draw())

        resources.images.incomplete = $
    }

    market.container.attach.parent.onclick = (e) => {
        val offsetX = e.offsetX * dom.window.devicePixelRatio
        val offsetY = e.offsetY * dom.window.devicePixelRatio

        val width = market.container.attach.parent.clientWidth * dom.window.devicePixelRatio * upscale
        val height = market.container.attach.parent.clientHeight * dom.window.devicePixelRatio * upscale

        onClick(game.market((offsetX.~ * 5 /↓ width.~).clamp(0, 4)))
    }

    var phase : |[(Faction, Phase)] = None

    val chart : CanvasPane = new CanvasPaneX(newPane("chart", Content), 2/2, Inside)(resources) {
        def makeScene(reposition : Boolean) = {
            val dims = img("chart")
            val bgimg = img("chart-2")

            val background = new OrderedLayer
            background.add(Sprite($(ImageRect(new RawImage(bgimg), Rectangle(0, 0, dims.width, dims.height), 1.0)), $))(0, 0)

            val tokens = new OrderedLayer

            factions.indexed.foreach { (f, i) =>
                tokens.add(Sprite($(ImageRect(new RawImage(img(color(f) + "-cube")), Rectangle(-48/2, -48/2, 48, 48), 1.0)), $))(162 + i * 71, 100)
            }

            game.adaptation.indexed.foreach { (r, i) =>
                tokens.add(Sprite($(ImageRect(new RawImage(img(r.name)), Rectangle(-54/2, -54/2, 54, 54), 1.0)), $))(180 + i * 56, 200)
            }

            game.regression.indexed.foreach { (r, i) =>
                tokens.add(Sprite($(ImageRect(new RawImage(img(r.name)), Rectangle(-54/2, -54/2, 54, 54), 1.0)), $))(180 + i * 56, 200+72)
            }

            game.abundance.indexed.foreach { (r, i) =>
                tokens.add(Sprite($(ImageRect(new RawImage(img(r.name)), Rectangle(-54/2, -54/2, 54, 54), 1.0)), $))(180 + i * 56, 358)
            }

            game.wasteland.indexed.foreach { (r, i) =>
                tokens.add(Sprite($(ImageRect(new RawImage(img(r.name)), Rectangle(-54/2, -54/2, 54, 54), 1.0)), $))(180 + i * 56, 358+72)
            }

            game.depletion.indexed.foreach { (r, i) =>
                tokens.add(Sprite($(ImageRect(new RawImage(img(r.name)), Rectangle(-54/2, -54/2, 54, 54), 1.0)), $))(180 + i * 56, 358+72+72)
            }

            game.wanderlust.indexed.foreach { (r, i) =>
                tokens.add(Sprite($(ImageRect(new RawImage(img(r.name)), Rectangle(-54/2, -54/2, 54, 54), 1.0)), $))(183 + i * 56, 846)
            }

            def center(p : Phase) : (Int, Int) = p @@ {
                case Initiative => (96, 102)
                case Adaptation(1) => (419, 204)
                case Adaptation(2) => (474, 204)
                case Adaptation(3) => (529, 204)
                case Regression(1) => (419, 275)
                case Regression(2) => (474, 275)
                case ReptileRegression => (533, 275)
                case Abundance(1) => (419, 361)
                case Abundance(2) => (474, 361)
                case Wasteland => (419, 432)
                case Depletion => (419, 503)
                case Glaciation(0) => (187+1, 590)
                case Glaciation(1) => (298, 590)
                case Glaciation(2) => (409, 590)
                case Glaciation(3) => (520, 590)
                case Speciation(Meat) =>  (187, 670)
                case Speciation(Sun) =>   (242, 670)
                case Speciation(Seed) =>  (298, 670)
                case Speciation(Water) => (354, 670)
                case Speciation(Grub) =>  (409, 670)
                case Speciation(Grass) => (465, 670)
                case InsectSpeciation => (524, 670)
                case Wanderlust(1) => (411-1, 848)
                case Wanderlust(2) => (467-1, 848)
                case Wanderlust(3) => (522-1, 848)
                case Migration(7) => (187, 930)
                case Migration(6) => (243, 930)
                case Migration(5) => (299, 930)
                case Migration(4) => (355, 930)
                case Migration(3) => (410, 930)
                case Migration(2) => (466, 930)
                case BirdMigration => (530, 930)
                case ArachnidCompetition => (104, 1052)
                case Competition(Jungle, Wetland) =>    (187, 1052)
                case Competition(Wetland, Desert) =>    (243, 1052)
                case Competition(Desert, Forest) =>     (299, 1052)
                case Competition(Forest, Savannah) =>   (355, 1052)
                case Competition(Savannah, Mountain) => (410, 1052)
                case Competition(Mountain, Sea) =>      (466, 1052)
                case Competition(Sea, Jungle) =>        (522, 1052)
                case Domination(1) => (188, 1162)
                case Domination(2) => (244, 1162)
                case Domination(3) => (299, 1162)
                case Domination(4) => (355, 1162)
                case Domination(5) => (410, 1162)
                case MammalExtinction => (163, 1216)
                case Extinction => (207, 1258)
                case Survival => (316, 1258)
                case Reseed => (427, 1258)
                case _ => (0x11, 0x11)
            }

            if (game.states.any)
            Phases.flow.notOf[Invisible].foreach { p =>
                val (x, y) = center(p)

                val hb = $(Rectangle(-20, -20, 40, 40), Rectangle(-26, -10, 52, 20), Rectangle(-10, -26, 20, 52))

                tokens.add(Sprite($, hb, $(p)))(x, y)
                tokens.add(Sprite($, hb./(_.scale(1.4)), $(p)))(x, y)
                tokens.add(Sprite($, hb./(_.scale(1.8)), $(p)))(x, y)

                p./(_.faction).single./ { f =>
                    tokens.add(Sprite($(ImageRect(new RawImage(img(color(f) + "-" + "cylinder")), Rectangle(-20, -60+4+2, 40, 68), 1.0)), $))(x, y)
                }

                phase.foreach { case (faction, phase) =>
                    if (phase == p)
                        tokens.add(Sprite($(ImageRect(new RawImage(img(color(faction) + "-" + "cylinder")), Rectangle(-20, -60+4+2, 40, 68).scale(1.3), 0.8)), $))(x, y)
                }

                if (game.highlight.cylinder.has(p)) {
                    tokens.add(Sprite($(ImageRect(new RawImage(img(game.highlight.current./(color).?(_ + "-") + "snowflake")), Rectangle(-20*2, -20*2, 40*2, 40*2), 1.0)), $))(x, y)
                }
            }

            val scene = new Scene($(background, tokens), dims.width, dims.height, Margins(0, 0, 0, 0))

            |(scene)
        }

        def processHighlight(target : $[Any], xy : XY) {
            val l = target.of[Phase]
            val ll = l.%(p => l.count(p) == l./(p => l.count(p)).max)
            phase = ll.distinct.single./~(p => keysDirect.of[PlanningAction].%(_.p == p)./(k => k.f -> p)).single
            if (phase.any)
                chart.container.attach.parent.style.cursor = "pointer"
        }

        def processTargetClick(target : $[Any], xy : XY) {
            phase = None
            val l = target.of[Phase]
            val ll = l.%(p => l.count(p) == l./(p => l.count(p)).max)
            onClick(ll.distinct.single./~(p => keysDirect.of[PlanningAction].%(_.p == p)).single)
        }

        def adjustCenterZoom() {
            val width = 594
            val height = 1285
            val margins = Margins(0, 0, 0, 0)

            zoomBase = zoomBase.clamp(0, 990*2)

            val qX = (width + margins.left + margins.right) * (1 - 1 / zoom) / 2
            val minX = -qX + margins.right - zoomBase / 5
            val maxX = qX - margins.left + zoomBase / 5
            dX = dX.clamp(minX, maxX)

            val qY = (height + margins.top + margins.bottom) * (1 - 1 / zoom) / 2
            val minY = -qY + margins.bottom - zoomBase / 5
            val maxY = qY - margins.top + zoomBase / 5
            dY = dY.clamp(minY, maxY)
        }
    }

    val background = new OrderedLayer

    val width = 2528
    val height = 1776
    val margins = Margins(0, 0, 0, 0)
    zoomBase = 800

    lazy val scene = {
        val mp = img("map")

        background.add(Sprite($(ImageRect(new RawImage(mp), Rectangle(0, 0, mp.width, mp.height), 1.0)), $))(0, 0)

        new Scene($(background), mp.width, mp.height, margins)
    }

    override def adjustCenterZoomX() {
        zoomBase = zoomBase.clamp(-990, 990*2)

        val qX = (width + margins.left + margins.right) * (1 - 1 / zoom) / 2
        val minX = -qX + margins.right - zoomBase / 5
        val maxX = qX - margins.left + zoomBase / 5
        dX = dX.clamp(minX, maxX)

        val qY = (height + margins.top + margins.bottom) * (1 - 1 / zoom) / 2
        val minY = -qY + margins.bottom - zoomBase / 5
        val maxY = qY - margins.top + zoomBase / 5
        dY = dY.clamp(minY, maxY)
    }

    class Highlights {
        var coordinates : |[XY] = None
        var target : $[Any] = $

        var placeResource : $[(Corner, Resource)] = $
        var removeResource : $[Corner] = $
        var tileFaction : $[(Tile, Faction)] = $
        var unit : $[Figure] = $
        var tile : $[Tile] = $
        var tileTerrain : $[(Tile, Terrain)] = $
        var move : $[(Faction, Tile, Tile)] = $
    }

    var highlight = new Highlights

    def processRightClick(target : $[Any], xy : XY) {
        lastActions.of[Cancel].single.foreach(onClick)
    }

    def processHighlight(target : $[Any], xy : XY) {
        highlight.coordinates = |(xy)
        highlight.target = target

        highlight.removeResource =
            target.of[Corner].distinct.single./~{ c =>
                keys.of[RemoveResourceKey].exists(_.corner == c).$(c)
            }

        highlight.placeResource =
            target.of[Corner].single./~{ c =>
                keysDirect.of[PlaceResourceKey].%(_.corner == c)./(k => (c, k.resource)).single.$
            }

        highlight.tileFaction =
            target.of[Tile].single./~{ t =>
                target.of[Faction].distinct.single./~{ f =>
                    (keys.of[TileFactionKey].exists(k => k.tile == t && k.faction == f)).$((t, f))
                }
            }

        highlight.unit =
            target.of[Figure].starting./~{ u =>
                (keys.of[UnitKey].exists(k => k.unit == u)).$(u)
            }

        highlight.tile =
            target.of[Tile].single./~{ t =>
                (keys.of[TileFactionKey].none && keys.of[TileKey].exists(_.tile == t)).$(t)
            }

        highlight.move =
            target.of[Tile].single./~{ t =>
                keys.of[TileFactionKey].none.??(
                    (keysDirect.of[DirectionToKey].%(_.to == t) ++ keysDirect.of[DirectionFromKey].%(_.from == t))./(k => (k.faction, k.from, k.to)).distinct.single
                ).$
            }

        highlight.tileTerrain =
            target.of[Tile].single./~{ t =>
                keysDirect.of[TileTerrainKey].%(_.tile == t)./(k => (t, k.terrain)).single.$
            }

        if (highlight.removeResource.any)
            mapSmall.attach.parent.style.cursor = "pointer"

        if (highlight.placeResource.any)
            mapSmall.attach.parent.style.cursor = "pointer"

        if (highlight.unit.any)
            mapSmall.attach.parent.style.cursor = "pointer"

        if (highlight.tile.any)
            mapSmall.attach.parent.style.cursor = "pointer"

        if (highlight.tileFaction.any)
            mapSmall.attach.parent.style.cursor = "pointer"

        if (highlight.tileTerrain.any)
            mapSmall.attach.parent.style.cursor = "pointer"
    }

    def processTargetClick(target : $[Any], xy : XY) {
        target.of[Corner].distinct.single.foreach { case c =>
            keys.of[RemoveResourceKey].%(_.corner == c).distinct.single.foreach { k =>
                return onClick(k)
            }
        }

        target.of[Corner].single.foreach { case c =>
            keysDirect.of[PlaceResourceKey].%(_.corner == c).single.foreach { k =>
                return onClick(k)
            }
        }

        target.of[Tile].single./~(t => target.of[Faction].single./(t -> _)).foreach { case (t, f) =>
            keys.of[TileFactionKey].%(_.tile == t).%(_.faction == f).single.foreach { k =>
                return onClick(k)
            }
        }

        target.of[Tile].single.foreach { case t =>
            keysDirect.of[TileKey].%(_.tile == t).single.foreach { k =>
                return onClick(k)
            }
        }

        target.of[Tile].single.foreach { case t =>
            keys.of[TileKey].%(_.tile == t).single.foreach { k =>
                return onClick(k)
            }
        }

        target.of[Tile].single.foreach { case x =>
            keysDirect.of[TileTerrainKey].%(_.tile == x).single.foreach { k =>
                return onClick(k)
            }
        }

        target.of[Figure].distinctBy(_.faction).single.foreach { case u =>
            keysDirect.of[UnitKey].%(_.unit == u).single.foreach { k =>
                return onClick(k)
            }
        }

        target.of[Key].single.foreach { case k =>
            return onClick(k)
        }

        lastActions.of[Cancel].single.foreach { a =>
            return onClick(a)
        }
    }

    def makeScene() : |[Scene] = {
        if (game.states.none)
            return None

        val rtw = 390
        val rth = 338

        val tw = rtw
        val th = rth

        val fw = tw*5.5
        val fh = th*6

        background.clear()

        game.board.tiles.foreach { x =>
            val r = game.terrain.get(x)./(_.id)|("empty")

            val hitboxes = $(
                Rectangle(-64, -166, 128, 38),
                Rectangle(-108, -128, 216, 38),
                Rectangle(-144, -90, 288, 180),
                Rectangle(-108, 90, 216, 38),
                Rectangle(-64, 128, 128, 38),
            )

            val empty = r == "empty"
            val a = highlight.tile.has(x).?(empty.?(1.0).|(0.8)).|(empty.?(0.2).|(1.0))

            background.add(Sprite($(ImageRect(new RawImage(img(r)), Rectangle(-rtw/2, -rth/2, rtw, rth), a)), hitboxes, $(x)))(x.x*tw/4 + fw/2, x.y*th/4 + fh/2)

            game.letters.get(x).foreach { l =>
                background.add(Sprite($(ImageRect(new RawImage(img(r + "-" + l)), Rectangle(-rtw/2, -rth/2, rtw, rth), 1.0)), $))(x.x*tw/4 + fw/2, x.y*th/4 + fh/2)
            }

            highlight.tileTerrain.%<(_ == x)./{ (x, t) =>
                background.add(Sprite($(ImageRect(new RawImage(img(t.id)), Rectangle(-rtw/2, -rth/2, rtw, rth), 0.8)), $))(x.x*tw/4 + fw/2, x.y*th/4 + fh/2)
            }
        }

        game.board.tiles.foreach { t =>
            val all = t.$
            val l = all./(_.faction).distinct
            var d = game.order.reverse.intersect(l).sortBy(f => all./(_.faction).count(f)).reverse

            val domination = highlight.tile.has(t) && keys.of[DominationKey].%(_.tile == t).any
            val points = t.terrain./~(_.points)

            val extinct = l.%(_.claim(t) <= 0)

            val pain = keysDirect.of[UnitCompetitionKey].any

            def cube(u : Figure, tag : $[Any], alpha : Double, scale : Double, x : Double, y : Double) {
                val f = u.faction
                val c = color(f)
                val a = highlight.tileFaction.has((t, f)).?(0.8).|(1.0)*alpha

                val b = Rectangle(-48/2, -48/2, 48, 48).scale(scale)

                background.add(Sprite($(ImageRect(new RawImage(img(c + "-cube")), b, a)), $(b), tag))(x, y)

                if (highlight.unit.has(u))
                    if (pain)
                        background.add(Sprite($(ImageRect(new RawImage(img("pain")), b, alpha)), $, $), z = 1)(x, y)
                    else
                        background.add(Sprite($(ImageRect(new RawImage(img(c + "-cube")), b.scale(1.4), alpha)), $, $))(x, y)

                if (game.saved.has(u))
                    background.add(Sprite($(ImageRect(new RawImage(img("snowflake")), b, alpha)), $, $))(x, y)
                else
                if (extinct.has(f))
                    background.add(Sprite($(ImageRect(new RawImage(img("kill")), b, alpha)), $, $))(x, y)

            }

            if (d.any) {
                val f = d.first
                val l = t.of(f)
                val s = (l.num <= 5).?(50).|(5*48/(l.num - 1))

                l.indexed.foreach { (u, i) =>
                    cube(u, $(f, u), 1.0, (domination && points.num >= 1).?(1.4).|(1.0), t.x*tw/4 + fw/2 + (i - (l.num - 1) * 0.5) * s, t.y*th/4 + fh/2 + 88)
                }

                d = d.drop(1)
            }

            if (d.any) {
                val f = d.first
                val l = t.of(f)
                val s = (l.num <= 3).?(50).|(3*48/(l.num - 1))

                l.indexed.foreach { (u, i) =>
                    cube(u, $(f, u), 1.0, (domination && points.num >= 2).?(1.3).|(1.0), t.x*tw/4 + fw/2 + 65, t.y*th/4 + fh/2 + (i - (l.num - 1) * 0.5) * s - 40)
                }

                d = d.drop(1)
            }

            if (d.any) {
                val f = d.first
                val l = t.of(f)
                val s = (l.num <= 3).?(50).|(3*48/(l.num - 1))

                l.indexed.foreach { (u, i) =>
                    cube(u, $(f, u), 1.0, (domination && points.num >= 3).?(1.2).|(1.0), t.x*tw/4 + fw/2 - 65, t.y*th/4 + fh/2 + (i - (l.num - 1) * 0.5) * s - 40)
                }

                d = d.drop(1)
            }

            if (d.any) {
                val f = d.first
                val l = t.of(f)
                val s = (l.num <= 2).?(50).|(2*48/(l.num - 1))

                l.indexed.foreach { (u, i) =>
                    cube(u, $(f, u), 1.0, (domination && points.num >= 4).?(1.1).|(1.0), t.x*tw/4 + fw/2 + 120, t.y*th/4 + fh/2 + (i - (l.num - 1) * 0.5) * s - 16)
                }

                d = d.drop(1)
            }

            if (d.any) {
                val f = d.first
                val l = t.of(f)
                val s = (l.num <= 2).?(50).|(2*48/(l.num - 1))

                l.indexed.foreach { (u, i) =>
                    cube(u, $(f, u), 1.0, 1.0, t.x*tw/4 + fw/2 - 120, t.y*th/4 + fh/2 + (i - (l.num - 1) * 0.5) * s - 16)
                }

                d = d.drop(1)
            }

            if (d.any) {
                val f = d.first
                val l = t.of(f)
                val s = (l.num <= 1).?(50).|(36/(l.num - 1))

                l.indexed.foreach { (u, i) =>
                    cube(u, $(f, u), 1.0, 1.0, t.x*tw/4 + fw/2 + (i - (l.num - 1) * 0.5) * s, t.y*th/4 + fh/2 - 88)
                }

                d = d.drop(1)
            }

            l.%(f => f.claim(t) > l.but(f)./(_.claim(t)).maxOr(0)).single.foreach { f =>
                val k = 1.44
                background.add(Sprite($(ImageRect(new RawImage(img(color(f) + "-cone")), Rectangle(-48/2*k, -48*k, 48*k, 64*k).scale(domination.?(1.4).|(1.0)), 1.0)), $))(t.x*tw/4 + fw/2, t.y*th/4 + fh/2)
            }

            val speciation = keysDirect.of[SpeciationKey].%(_.tile == t)

            if (speciation.any)
                background.add(Sprite($(ImageRect(new RawImage(img("cover")), Rectangle(-rtw/2, -rth/2, rtw, rth), 0.5)), $))(t.x*tw/4 + fw/2, t.y*th/4 + fh/2)

            speciation.foreach { k =>
                val s = 50
                0.until(k.count).foreach { i =>
                    cube(new Figure(k.faction, -k.count), $(k), highlight.target.has(k).?(1.0).|(0.8), highlight.target.has(k).?(1.2).|(1.0), t.x*tw/4 + fw/2 + (i - (k.count - 1) * 0.5) * s, t.y*th/4 + fh/2 + 25 * (speciation.num + 1) - 50 * k.count)
                }
            }
        }

        game.board.corners.foreach { c =>
            val resource = game.resources.get(c)./(_.id)

            val r = Rectangle(-72/2 + ((c.x - 1) % 3 == 0).??(8) - ((c.x + 1) % 3 == 0).??(8), -72/2, 72, 72)

            if (resource.any) {
                if (highlight.removeResource.has(c))
                    background.add(Sprite($(ImageRect(new RawImage(img(resource.get)), r, 0.5)), $(r), $(c)))(c.x*tw/4 + fw/2, c.y*th/4 + fh/2)
                else
                if (highlight.placeResource.%<(_ == c).any)
                    background.add(Sprite($(ImageRect(new RawImage(img(resource.get)), r.scale(1.4), 1.0)), $(r), $(c)))(c.x*tw/4 + fw/2, c.y*th/4 + fh/2)
                else
                    background.add(Sprite($(ImageRect(new RawImage(img(resource.get)), r, 1.0)), $(r), $(c)))(c.x*tw/4 + fw/2, c.y*th/4 + fh/2)
            }
            else {
                background.add(Sprite($, $(r), $(c)))(c.x*tw/4 + fw/2, c.y*th/4 + fh/2)

                highlight.placeResource.%<(_ == c).single./>.foreach { resource =>
                    val r = Rectangle(-72/2 + ((c.x - 1) % 3 == 0).??(8) - ((c.x + 1) % 3 == 0).??(8), -72/2, 72, 72).scale(1.4)

                    background.add(Sprite($(ImageRect(new RawImage(img(resource.id)), r, 0.7)), $(r)))(c.x*tw/4 + fw/2, c.y*th/4 + fh/2)
                }
            }
        }

        highlight.move.foreach { case (faction, from, to) =>
            val x = (from.x + to.x) /:/ 2
            val y = (from.y + to.y) /:/ 2
            val k = 1.44

            val (dir, rect) = (from.x - to.x, from.y - to.y) match {
                case (0, 4) => ("n", Rectangle(-33 /:/ 2, -43, 33, 63))
                case (0, -4) => ("s", Rectangle(-33 /:/ 2, -20, 33, 63))
                case (3, -2) => ("ws", Rectangle(-38, -14, 56, 37))
                case (3, 2) => ("wn", Rectangle(-38, -23, 56, 37))
                case (-3, 2) => ("en", Rectangle(-18, -23, 56, 37))
                case (-3, -2) => ("es", Rectangle(-18, -14, 56, 37))

                case (0, 8) => ("n-n", Rectangle(-33 /:/ 2, -43 - 160, 33, 63*6))
                case (0, -8) => ("s-s", Rectangle(-33 /:/ 2, -20 - 160 + 4, 33, 63*6))
                case (6, -4) => ("ws-ws", Rectangle(-38 - 56*2.5, -14 - 37*2.5, 56*6, 37*6))
                case (6, 4) => ("wn-wn", Rectangle(-38 - 56*2.5, -23 - 37*2.5, 56*6, 37*6))
                case (-6, 4) => ("en-en", Rectangle(-18 - 56*2.5, -23 - 37*2.5, 56*6, 37*6))
                case (-6, -4) => ("es-es", Rectangle(-18 - 56*2.5, -14 - 37*2.5, 56*6, 37*6))

                case (6, 0) => ("w", Rectangle(-43 - 33*5, -33 /:/ 2, 63*6, 33))
                case (-6, 0) => ("e", Rectangle(-20 - 33*5, -33 /:/ 2, 63*6, 33))
                case (3, 6) => ("nw", Rectangle(-23 - 80 - 12, -38 - 140, 37*6, 56*6))
                case (-3, 6) => ("ne", Rectangle(-23 - 80 - 4, -38 - 140, 37*6, 56*6))
                case (3, -6) => ("sw", Rectangle(-23 - 80 - 12, -38 - 140 + 18, 37*6, 56*6))
                case (-3, -6) => ("se", Rectangle(-23 - 80 - 4, -38 - 140 + 18, 37*6, 56*6))

                case (xxx, yyy) => println(xxx + "//" + yyy); ("cone", Rectangle(-48/2*k, -48*k, 48*k, 64*k))
            }

            background.add(Sprite($(ImageRect(new RawImage(img(color(faction) + "-arrow-" + dir)), rect, 1.0)), $))(x*tw/4 + fw/2, y*th/4 + fh/2)
        }

        |(new Scene($(background), fw, fh, Margins(100, 100, 100, 100)))
    }

    def factionStatus(f : Faction) {
        val container = statuses(game.order.indexOf(f))

        val name = f.name // resources.getName(f).|(f.name)

        if (!game.states.contains(f)) {
            container.replace(Div(Div(name).styled(f)(styles.title), styles.smallname, xlo.pointer), resources)
            return
        }

        val survival = (f.survival() > factions.but(f)./(_.survival()).max).?(Image("snowflake", styles.token).onClick.param("survival") ~ " ")

        val title = Div(Div(survival ~ name.styled(f)(styles.title) ~ " " ~ f.vp.vp.styled(styles.title)), styles.smallname, styles.titleLine, xlo.pointer)
        val res = (f.fixed ++ f.adaptation)./(r => Image(r.name, styles.token)).merge.div(styles.resLine)(xlo.nowrap)

        val cc = f.pawns.$
        val cyls = (cc.num <= 5).?(cc.num.times(Image(color(f) + "-" + "cylinder", styles.cylinder)).merge).|(cc.num.hh ~ "×" ~ Image(color(f) + "-" + "cylinder", styles.cylinder))
        val figs = (cyls ~ " " ~ f.reserve.num.hh ~ "×" ~ Image(color(f) + "-" + "cube", styles.cube)).div(styles.resLine)(xlo.nowrap)

        val content = ((title ~ res ~ figs).div).div(styles.statusUpper)(xlo.flexVX)(ExternalStyle("hide-scrollbar")).pointer.onClick.param(f)

        container.replace(content, resources, {
            case x => onClick(x)
        })

        // if (f == game.current && game.isOver)
        //     container.attach.parent.style.background = f @@ {
        //         case Red => "#680016"
        //         case Yellow => "#684f19"
        //         case Blue => "#05274c"
        //         case White => "#666666"
        //     }
        // else
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
            factionStatus(game.order(n))
        }

        if (overlayPane.visible)
            overlayPane.vis()
        else
            overlayPane.invis()

        drawMap()
        market.draw()
        chart.draw()
    }

    val layoutZoom = 0.49 * 0.88

    val kkk = 1.18 / 30

    val layouts = $(Layout("base",
        $(
            BasicPane("status", 15, 8.5, Priorities(top = 3, left = 2, maxXscale = 1.8, maxYscale = 1.8, grow = 1)),
            BasicPane("market", (2032-60)*kkk, 572*kkk, Priorities(top = 3, right = 3, maxXscale = 1.0, maxYscale = 1.0, grow = 0)),
            BasicPane("log", 32, 16, Priorities(right = 1)),
            BasicPane("map-small", 73, 70, Priorities(top = 2, left = 1, grow = 3)),
            BasicPane("action-a", 64/1.5, 36, Priorities(bottom = 1, right = 3, grow = 2)),
            BasicPane("action-b", 55/1.5, 47, Priorities(bottom = 1, right = 3, grow = 2, maxXscale = 1.2)),
            BasicPane("chart", 33, 70, Priorities(grow = -4))
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

    val layoutKey = "v" + 2 + "." + "arity-" + arity

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

        case "survival" =>
            showOverlay(overlayFitX(Image("survival", styles.artwork, xlo.flexBasis100)).onClick, onClick)

        case card : DominanceCard =>
            showOverlay(overlayFitX(Image(card.id, styles.artwork, xlo.flexBasis100)).onClick, onClick)

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
            $(ZBasic(Break ~ Break, "Interface".spn, () => { callbacks.editSettings { updateStatus() } }).copy(clear = false))
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
            case _ if a.unwrap.is[PlanningAction] => $(xstyles.unavailableCard)
            case _ => $(xstyles.unavailableText)
        } ++
        a @@ {
            case _ if view.any && view.get.is[Resource] => $(styles.card0, styles.circle)
            case _ if view.any && view.get.is[(Resource, Int)] => $(styles.card0, styles.circle)
            case _ : Info => $(xstyles.info)
            case _ if unavailable => $(xstyles.info)
            case _ => $(xstyles.choice)
        } ++
        a @@ {
            case _ if view.any && view.get.is[Resource] => $()
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
