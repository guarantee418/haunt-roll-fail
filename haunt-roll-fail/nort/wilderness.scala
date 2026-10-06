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


// Wilderness expansion (8-page rulebook): the Environment tiles module, and five creatures for the Creatures
// module (two Draugr Jötnar, two Eldthursar, Hvedrung). The Ancestral Graveyard (two Spectral Warriors) and the
// Wyvern's Den (the Wyvern) need the Creatures module. The tiles are in Tiles.environment, the creatures in creatures.scala.


// Where each Environment tile's feature is: tile and area
object Wild {
    val lake = "wild-lake"
    val poison = "wild-poison"
    val den = "wild-den"
    val graveyard = "wild-graveyard"
    // Without the Creatures module the Ancestral Graveyard and the Wyvern's Den stay out
    val creatureTiles = $(graveyard, den)

    // Not the Great Lake when it is the central tile
    def tiles(implicit game : Game) : $[String] = Tiles.environment./(_.id).%(t => game.has(Creatures) || creatureTiles.has(t).not).but(den).but(game.central)

    // Areas holding a feature: a geyser, ruins and their fame, the Swamp, the graveyard, the Den itself
    val geysers : $[(String, String)] = $("wild-geyser-1" -> "s", "wild-geyser-2" -> "s")
    val ruins : Map[(String, String), Int] = Map(("wild-ruins-1", "s") -> 1, ("wild-ruins-2", "s") -> 2)

    def tile(a : AreaRef)(implicit game : Game) : String = game.board.at(a.x, a.y)./(_.tile).|("")

    def key(a : AreaRef)(implicit game : Game) = (tile(a), a.id)

    def swamp(a : AreaRef)(implicit game : Game) = key(a) == ("wild-swamp", "m")

    def denArea(a : AreaRef)(implicit game : Game) = key(a) == (den, "d")

    def denIn(t : Territory)(implicit game : Game) = t.areas.exists(denArea)

    // The Den's area, once explored
    def dens(implicit game : Game) : $[AreaRef] = game.board.placements.%(_.tile == den)./(p => AreaRef(p.x, p.y, "d"))

    // Where a Wyvern lives: this Den or the Wastelands central Wyvern's Den
    def homes(implicit game : Game) : $[AreaRef] = dens ++ game.board.placements.%(_.tile == Waste.den)./(p => AreaRef(p.x, p.y, "d"))

    def homeIn(t : Territory)(implicit game : Game) = homes.exists(t.areas.contains)

    def geysersIn(t : Territory)(implicit game : Game) = t.areas.count(a => geysers.has(key(a)))

    def ruinsFame(t : Territory)(implicit game : Game) = t.areas./(a => ruins.getOrElse(key(a), 0)).sum

    def graveyardIn(t : Territory)(implicit game : Game) = t.areas.exists(a => key(a) == (graveyard, "s"))

    // The territories next to each Great Lake or Poisonous Swamp on the map (its four shores)
    def around(tile : String)(implicit game : Game) : $[$[Territory]] =
        game.board.placements.%(_.tile == tile)./(p => p.spec.areas./(a => game.board.territory(AreaRef(p.x, p.y, a.id))).distinct)
}


// SETUP
case class ShuffledEnvironmentAction(shuffled : $[String]) extends ShuffledAction[String]
case class ShuffledDenAction(top : $[String], shuffled : $[String]) extends ShuffledAction[String]

// CREATURES
case class JotunnPayAction(self : Faction, c : Creature, pay : $[Resource], then : ForcedAction) extends BaseAction(c, "demands two resources")("Pay", pay./(_.elem).join(" "))
case class EldthursAction(self : Faction, c : Creature, space : SpaceRef, building : Building, then : ForcedAction) extends BaseAction(c, "destroys a small building in", space.area)(building) with MapTarget { def target = space.area }
case class HvedrungAction(c : Creature, then : ForcedAction) extends ForcedAction
case class ShuffledHvedrungAction(shuffled : $[Creature], c : Creature, then : ForcedAction) extends ShuffledAction[Creature]
case class WyvernAppearAction(spot : Spot, then : ForcedAction) extends ForcedAction

// ANCESTRAL GRAVEYARD
case class GraveyardAction(f : Faction) extends ForcedAction
case class SpectralPlaceAction(self : Faction, area : AreaRef) extends BaseAction("Ancestral Graveyard".hl, "a Spectral Warrior rises in")(area) with MapTarget { def target = area }
case class SpectralSkipAction(self : Faction) extends BaseAction("Ancestral Graveyard".hl)("No Spectral Warrior")

// GEYSERS
case class GeyserSpot(f : Faction, area : AreaRef) extends Record
case class GeysersAction(l : $[GeyserSpot]) extends ForcedAction
case class GeyserPlaceAction(self : Faction, area : AreaRef, rest : $[GeyserSpot]) extends BaseAction("Geyser".hl, "place a unit for free in")(area) with MapTarget { def target = area }
case class GeyserSkipAction(self : Faction, rest : $[GeyserSpot]) extends BaseAction("Geyser".hl)("Place no unit")


object WildernessExpansion extends Expansion {
    // Harvest: the Ruins' fame (closed or not) and the Den's, unless a Wolf creature is there; the Great Lake's food
    def ruinsFame(f : Faction)(implicit game : Game) = game.controlled(f).%(t => game.wolfIn(t).not)./(Wild.ruinsFame).sum

    def denFame(f : Faction)(implicit game : Game) = 2 * game.controlled(f).%(t => game.wolfIn(t).not).count(Wild.denIn)

    // The most units around each lake: 2 food; tied players get 1 each
    def lakeFood(f : Faction)(implicit game : Game) : $[Int] = Wild.around(Wild.lake)./~{ l =>
        def units(g : Faction) = l./(t => game.count(t, g) + game.chiefIn(t, g).??(1)).sum
        val most = factions./(units).max
        val tied = factions.%(g => units(g) == most)

        (most > 0 && tied.has(f)).?((tied.num == 1).?(2).|(1))
    }

    // Fame and food at the next harvest, for the player panels
    def forecast(f : Faction)(implicit game : Game) : (Int, Int) = (ruinsFame(f) + denFame(f), lakeFood(f).sum)

    def harvest(f : Faction)(implicit game : Game) {
        val ruins = ruinsFame(f)
        if (ruins > 0) {
            f.fame += ruins
            f.log("gained", ruins.hl, FameIcon(), "from", "Ruins".hl)
        }

        val dens = denFame(f)
        if (dens > 0) {
            f.fame += dens
            f.log("gained", dens.hl, FameIcon(), "from the", "Wyvern's Den".hl)
        }

        lakeFood(f).foreach { n =>
            f.food += n
            f.log("collected", n.hl, Food, "from the", "Great Lake".hl)
        }
    }

    def perform(action : Action, soft : Void)(implicit game : Game) = action @@ {
        // SETUP: after setup, the Environment tiles go into the map tile pile; the Wyvern's Den below the first 3 tiles per player
        case ShuffledTilesBackAction(l) =>
            game.pile = l

            log("The unused tiles went back into the pile")

            if (game.has(Creatures))
                game.spectrals = Creature.spectral

            // With Wastelands, the twelve Environment tiles drawn from both expansions
            Shuffle[String](l ++ game.environment.|(Wild.tiles), ShuffledEnvironmentAction(_))

        case ShuffledEnvironmentAction(l) =>
            game.pile = l

            log("The Environment tiles were shuffled into the map tiles")

            if (game.has(Creatures)) {
                val top = l.take(3 * game.arity)
                Shuffle[String]((l.drop(top.num) :+ Wild.den), ShuffledDenAction(top, _))
            }
            else
                Then(StartYearAction)

        case ShuffledDenAction(top, rest) =>
            game.pile = top ++ rest

            log("The", "Wyvern's Den".hl, "was shuffled in below the first", top.num.hl, "tiles")

            Then(StartYearAction)

        // The Wyvern appears when its Den is explored
        case TilePlacedAction(f, Wild.den, spot, setup, then) if game.has(Creatures) =>
            CreaturesExpansion.perform(TilePlacedAction(f, Wild.den, spot, setup, WyvernAppearAction(spot, then)), soft)

        case WyvernAppearAction(spot, then) =>
            val c = Creature.wyvern

            if (game.creatureLine.has(c).not) {
                game.creatureLine :+= c
                game.creatureAt += c -> AreaRef(spot.x, spot.y, "d")
                game.note("wyvern")

                log(c, "appeared in its Den")
            }

            Then(then)

        // CREATURE POWERS
        case CreatureEffectAction(c, then) if c.kind == Wyvern =>
            val t = game.board.territory(game.creatureAt(c))

            game.present(t).single match {
                case Some(o) =>
                    log(c, "attacks", o, "in", t.anchor)
                    Then(CreatureCombatAction(o, t.anchor, c, MoveEffect(0), false, then))
                case None => Then(then)
            }

        // Pay 2 resources, or as many as there are and be attacked
        case CreatureEffectAction(c, then) if c.kind == DraugrJotunn =>
            val t = game.board.territory(game.creatureAt(c))

            game.present(t).single match {
                case Some(o) if o.resources >= 2 =>
                    Ask(o).each(CommonExpansion.payments(o, 2))(p => JotunnPayAction(o, c, p, then))

                case Some(o) =>
                    val lost = Resource.all.%(o.has(_) > 0)
                    lost.foreach(r => o.gain(r, -1))
                    game.note("jotunn-attack")

                    o.log("could not pay", c, lost.any.?("and lost " ~ lost./(_.elem).join(" ")).|(Empty))
                    log(c, "attacks", o, "in", t.anchor)

                    Then(CreatureCombatAction(o, t.anchor, c, MoveEffect(0), false, then))

                case None => Then(then)
            }

        case JotunnPayAction(f, c, p, then) =>
            p.foreach(r => f.gain(r, -1))
            game.note("jotunn-pay")

            f.log("paid", p./(_.elem).join(" "), "to", c)

            Then(then)

        // The owner removes one of their small buildings there
        case CreatureEffectAction(c, then) if c.kind == Eldthurs =>
            val t = game.board.territory(game.creatureAt(c))
            val small = game.buildingsIn(t).filter(_._2.large.not)

            game.present(t).single match {
                case Some(o) if small.num == 1 =>
                    Then(EldthursAction(o, c, small.head._1, small.head._2, then))
                case Some(o) if small.any =>
                    Ask(o).each(small.distinctBy(_._2))((s, b) => EldthursAction(o, c, s, b, then))
                case _ =>
                    Then(then)
            }

        case EldthursAction(f, c, s, b, then) =>
            game.buildings -= s
            game.note("eldthurs")

            log(c, "destroyed", b, "of", f, "in", s.area)

            Then(then)

        // Hvedrung draws a creature into its territory; it comes right after Hvedrung in the line, so it acts next
        case CreatureEffectAction(c, then) if c.kind == Hvedrung =>
            Then(HvedrungAction(c, then))

        // Hvedrung staying where it is draws nothing
        case CreatureActivateAction(c :: rest) if c.kind == Hvedrung && game.creatureLine.has(c) && CreaturesExpansion.destinations(c).none =>
            log(c, "could not move")
            Then(CreatureActivateAction(rest))

        case HvedrungAction(h, then) =>
            if (game.creatureDeck.none && game.creatureDiscard.any)
                Shuffle[Creature](game.creatureDiscard, ShuffledHvedrungAction(_, h, then))
            else
            if (game.creatureDeck.none || game.creatureLine.has(h).not) {
                log(h, "found no creature to call")
                Then(then)
            }
            else {
                val c = game.creatureDeck.head
                game.creatureDeck = game.creatureDeck.drop(1)

                val i = game.creatureLine.indexOf(h)
                game.creatureLine = game.creatureLine.take(i + 1) ++ $(c) ++ game.creatureLine.drop(i + 1)
                game.creatureAt += c -> game.creatureAt(h)
                game.note("hvedrung")

                log(h, "called", c, "into", game.board.territory(game.creatureAt(h)).anchor)

                // In the Creature phase the new creature is activated right after Hvedrung
                val next = then match {
                    case CreatureActivateAction(rest) => CreatureActivateAction(c :: rest)
                    case x => x
                }

                Then(CreatureEffectAction(c, next))
            }

        case ShuffledHvedrungAction(l, h, then) =>
            game.creatureDeck = l
            game.creatureDiscard = $

            log("The creature discard pile was shuffled into a new draw pile")

            Then(HvedrungAction(h, then))

        // ANCESTRAL GRAVEYARD: at the start of the Creature phase its owner may raise a Spectral Warrior on a lair
        case CreaturePhaseAction if game.has(Creatures) =>
            val owner = game.board.territories.%(Wild.graveyardIn)./~(t => game.present(t).single).single

            owner match {
                case Some(f) if game.spectrals.any => Then(GraveyardAction(f))
                case _ => CreaturesExpansion.perform(CreaturePhaseAction, soft)
            }

        case GraveyardAction(f) =>
            val lairs = game.board.territories.%(t => t.areas.exists(a => game.board.spec(a).lair))

            if (lairs.none)
                CreaturesExpansion.perform(CreaturePhaseAction, soft)
            else
                Ask(f).each(lairs)(t => SpectralPlaceAction(f, t.anchor)).add(SpectralSkipAction(f))

        case SpectralPlaceAction(f, a) =>
            val t = game.board.territory(a)
            val c = game.spectrals.head
            game.spectrals = game.spectrals.drop(1)

            game.creatureLine :+= c
            game.creatureAt += c -> t.areas.find(x => game.board.spec(x).lair).|(a)
            game.note("spectral")

            f.log("raised", c, "in", a, "with the", "Ancestral Graveyard".hl)

            CreaturesExpansion.perform(CreaturePhaseAction, soft)

        case SpectralSkipAction(f) =>
            CreaturesExpansion.perform(CreaturePhaseAction, soft)

        // POISONOUS SWAMP: at the end of the Harvest, one unit dies in each territory next to it
        case WinterAction =>
            Wild.around(Wild.poison).flatten.distinct.foreach { t =>
                game.present(t).single.%(g => game.count(t, g) > 0 || game.chiefIn(t, g)).foreach { g =>
                    if (game.count(t, g) > 0)
                        game.removeUnits(t, g, 1)
                    else
                        game.chiefs -= g

                    game.note("poison")

                    g.log("lost a unit in", t.anchor, "to the", "Poisonous Swamp".hl)
                }
            }

            CommonExpansion.perform(WinterAction, soft)

        // GEYSERS: at the End of the Year, a free unit in each controlled territory per geyser
        case EndOfYearAction =>
            Then(GeysersAction(game.from(game.first)./~(f => game.controlled(f)./~(t => 1.to(Wild.geysersIn(t)).$./(_ => GeyserSpot(f, t.anchor))))))

        case GeysersAction(Nil) =>
            CommonExpansion.perform(EndOfYearAction, soft)

        case GeysersAction(GeyserSpot(f, a) :: rest) =>
            if (game.reserve(f) > 0 && game.controlled(f).has(game.board.territory(a)))
                Ask(f).add(GeyserPlaceAction(f, a, rest)).add(GeyserSkipAction(f, rest))
            else
                Then(GeysersAction(rest))

        case GeyserPlaceAction(f, a, rest) =>
            game.addUnits(a, f, 1)
            game.note("geyser")

            f.log("placed a unit in", a, "with a", "Geyser".hl)

            Then(GeysersAction(rest))

        case GeyserSkipAction(f, rest) =>
            Then(GeysersAction(rest))

        case _ => UnknownContinue
    }
}
