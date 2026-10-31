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

import hrf.elem._

import nort.elem._


// Robotos's "Special Unit Hater's Bear, Rough Border's Edition" (Discord, northgard-fanmade-content, 2026-10-22; asked for by Gman
// on 2026-10-31), picked as its own clan in the clan picker's "Fan-made clans" section (BearBorder in options.scala, factionVariants
// in meta.scala). Kaija is no figure but a token on a border: before each of Bear's clan cards it may be placed on a regular border
// between a territory Bear controls and an adjacent one, and every clan, Bear too, treats that border as Rough (Game.links), unless it
// discards a resource to cross it as a regular one for one Move action (KaijaTollAction), or it is Bear after The Bear Awakens.

// The Bear Clan card: Move 1, and 1 food or wood from the two territories when Kaija goes on a new border (KaijaCollectAction)
// The Bear Awakens: Recruit 2, and Bear crosses Kaija's border as a regular one for the rest of the year
case object BorderAwakenEffect extends Effect
// Protector of the Land: draw 1 card for each of Bear's territories with a Rough border (Kaija's too), at most 3
case object BorderProtectorEffect extends Effect

case class KaijaBorderAction(f : Faction, collect : Boolean, then : ForcedAction) extends ForcedAction
case class KaijaPlaceAction(self : Faction, a : AreaRef, b : AreaRef, collect : Boolean, then : ForcedAction) extends BaseAction("Kaija".hl, "place on the border between")(a, "and", b) with MapTarget { def target = b }
case class KaijaCollectAction(self : Faction, r : Resource, then : ForcedAction) extends BaseAction("Kaija".hl, "collect from the territories it connects")(Amount(1.hl, r.elem))
case class KaijaTollAction(self : Faction, r : Resource, left : Int, e : MoveEffect, then : ForcedAction) extends BaseAction("Kaija's border".hl, "discard to cross it as a regular border this Move")(Amount(1.hl, r.elem))

object BearBorderExpansion extends Expansion {
    def on(implicit game : Game) = game.has(BearBorder)

    // Regular borders between a territory Bear controls and an adjacent one, as an area of each, but not where Kaija is
    def targets(f : Faction)(implicit game : Game) : $[(AreaRef, AreaRef)] = {
        val l : $[(Territory, Territory)] = game.controlled(f)./~(t => game.board.adjacent(t).%((_, regular) => regular)./((o, _) => (t, o))).%((t, o) => game.kaijaBetween(t, o).not)
        // Each border once, when both territories are Bear's
        l.indices.$.%(i => l.take(i).has((l(i)._2, l(i)._1)).not)./(i => (l(i)._1.anchor, l(i)._2.anchor))
    }

    // The territories Kaija's border connects
    def ends(implicit game : Game) : $[Territory] = game.kaijaBorder./~{ case (a, b) => $(game.board.territory(a), game.board.territory(b)) }

    // The food and wood the territories on Kaija's border give
    def collectable(implicit game : Game) : $[Resource] = {
        val l = ends./(game.harvest)
        $[(Resource, Int)](Food -> l./(_._1).sum, Wood -> l./(_._2).sum).%((_, n) => n > 0)./((r, _) => r)
    }

    // Bear's territories with a Rough border, Kaija's counting too (Protector of the Land)
    def roughTerritories(f : Faction)(implicit game : Game) : $[Territory] =
        game.controlled(f).%(t => HorizonDevsExpansion.rough(t) || ends.has(t))

    // The resources f may discard to cross Kaija's border as a regular one this Move: only when that opens a move it couldn't make
    def tolls(f : Faction, left : Int, e : MoveEffect)(implicit game : Game) : $[Resource] =
        if (on.not || game.kaijaBorder.none || game.kaijaToll.has(f) || (f == Bear && game.awakened) || e.ignoreRough || f.resources == 0) $
        else {
            val near = ends.%(t => MapExpansion.moveSources(f, e).has(t) || game.passing(f).has(t))
            def moves = near./~(t => MapExpansion.destinations(f, t, left, e)./((o, _) => (t, o)))
            val before = moves
            game.kaijaToll :+= f
            val after = moves
            game.kaijaToll = game.kaijaToll.but(f)
            if (after.diff(before).any) Resource.of(f).%(f.has(_) > 0) else $
        }

    def playable(f : Faction, e : Effect)(implicit game : Game) : Boolean = e match {
        case BorderAwakenEffect => true
        case BorderProtectorEffect => (roughTerritories(f).any || targets(f).any) && CommonExpansion.available(f) > 0
        case _ => false
    }

    def resolve(f : Faction, e : Effect, then : ForcedAction)(implicit game : Game) : Continue = e match {
        case BorderAwakenEffect =>
            game.awakened = true
            f.log("can cross", "Kaija".hl, "'s border as a regular one this year")
            Then(RecruitAction(f, 2, RecruitNormal, $, then))

        case BorderProtectorEffect =>
            val n = math.min(3, roughTerritories(f).num)
            f.log("drew", n.hl, (n == 1).?("card").|("cards"), "for territories with a Rough border")
            Then(DrawCardsAction(f, n, then))

        case _ => Then(then)
    }

    def perform(action : Action, soft : Void)(implicit game : Game) = action @@ {
        // Before each of Bear's clan cards (not the warchief's), Kaija may go on a border
        case PlayResolveAction(f, c : ClanCard, stage) if on && f == Bear && c.clan == Bear && c.n < 3 =>
            val after = TurnAction(f, (c.flash && stage < 2).?(1).|(2))
            Then(KaijaBorderAction(f, c.n == 0, ResolveEffectAction(f, c.effect, after)))

        case KaijaBorderAction(f, collect, then) =>
            val l = targets(f)
            if (l.none)
                Then(then)
            else
                Ask(f).each(l)((a, b) => KaijaPlaceAction(f, a, b, collect, then))
                    .add(then.as(game.kaijaBorder.any.?("Leave Kaija where it is").|("Place no token"))("Kaija".hl))

        case KaijaPlaceAction(f, a, b, collect, then) =>
            game.kaijaBorder = |((a, b))
            f.log("placed", "Kaija".hl, "on the border between", a, "and", b)
            game.note("kaija-border")

            // The Bear Clan card: 1 food or wood from the territories it connects
            if (collect && collectable.any)
                Ask(f).each(collectable)(r => KaijaCollectAction(f, r, then)).add(then.as("Collect nothing")("Kaija".hl))
            else
                Then(then)

        case KaijaCollectAction(f, r, then) =>
            f.gain(r, 1)
            game.note("kaija-collect")
            f.log("collected", 1.hl, r, "with", "Kaija".hl)
            Then(then)

        case KaijaTollAction(f, r, left, e, then) =>
            f.gain(r, -1)
            game.kaijaToll :+= f
            game.note("kaija-toll")
            f.log("discarded", 1.hl, r, "to cross", "Kaija".hl, "'s border this Move")
            Then(MoveAction(f, left, e, then))

        case _ => UnknownContinue
    }
}
