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
import hrf.meta._
import hrf.options._

import nort.elem._


// Creatures module (core box, rulebook pages 18-24): nine creatures appear on the lairs printed on the map tiles,
// move and act in the Creature phase between the Actions and the Harvest, and can be fought for fame


// How a creature picks between territories it could move to, in order
trait CreaturePriority extends NamedToString with Record
// Building points: 1 per small building, 3 per large one
case object MostBuildings extends CreaturePriority
case object MostUnits extends CreaturePriority
// Resources on the tiles and buildings
case object MostResources extends CreaturePriority

trait CreatureKind extends NamedToString with Record {
    def id : String
    def title : String
    // Combat value
    def value : Int
    // Fame for defeating it
    def fame : Int
    // Whether it can be in a territory with units without fighting them
    def shares : Boolean
    def priorities : $[CreaturePriority]
    def text : String
    // Removed from the game, not discarded, when defeated (Wilderness: Spectral Warriors and the Wyvern)
    def leaves : Boolean = false
}

case object BrownBear extends CreatureKind {
    val id = "brown-bear"
    val title = "Brown Bear"
    val value = 6
    val fame = 3
    val shares = true
    val priorities = $(MostBuildings, MostResources, MostUnits)
    val text = "A player cannot build or recruit in a territory with this creature, and they cannot explore or move units from it."
}

case object Draugr extends CreatureKind {
    val id = "draugr"
    val title = "Draugr"
    val value = 5
    val fame = 2
    val shares = true
    val priorities = $(MostUnits, MostBuildings, MostResources)
    val text = "When this creature appears or moves in a controlled territory, that player removes 1 unit from it."
}

case object FallenValkyrie extends CreatureKind {
    val id = "fallen-valkyrie"
    val title = "Fallen Valkyrie"
    val value = 7
    val fame = 4
    val shares = false
    val priorities = $(MostResources, MostBuildings, MostUnits)
    val text = "This creature does not share its territory with a player and attacks them when it appears or moves into a territory."
}

// Not to be confused with the Wolf Clan
case object CreatureWolf extends CreatureKind {
    val id = "wolf"
    val title = "Wolf"
    val value = 4
    val fame = 1
    val shares = true
    val priorities = $(MostResources, MostBuildings, MostUnits)
    val text = "A territory with this creature on it does not generate any fame or resources during the Harvest phase (except from buildings)."
}

// Wilderness expansion creatures (their rules are in wilderness.scala)
case object DraugrJotunn extends CreatureKind {
    val id = "draugr-jotunn"
    val title = "Draugr Jötunn"
    val value = 8
    val fame = 5
    val shares = true
    val priorities = $(MostResources, MostBuildings, MostUnits)
    val text = "When this creature appears or moves into a controlled territory, its owner must pay 2 resources of any type. Otherwise, it attacks."
}

case object Eldthurs extends CreatureKind {
    val id = "eldthurs"
    val title = "Eldthurs"
    val value = 8
    val fame = 5
    val shares = true
    val priorities = $(MostBuildings, MostUnits, MostResources)
    val text = "When this creature appears or moves into a controlled territory, its owner must remove 1 small building on the territory."
}

case object Hvedrung extends CreatureKind {
    val id = "hvedrung"
    val title = "Hvedrung"
    val value = 7
    val fame = 4
    val shares = true
    val priorities = $(MostBuildings, MostResources, MostUnits)
    val text = "When this creature appears or moves into a territory, draw a new creature card; place the card next in the creature line and the miniature in its territory."
}

// Only placed by the Ancestral Graveyard tile
case object SpectralWarrior extends CreatureKind {
    val id = "spectral-warrior"
    val title = "Spectral Warrior"
    val value = 4
    val fame = 1
    val shares = true
    val priorities = $(MostBuildings, MostResources, MostUnits)
    val text = "Buildings in the same territory as this creature have no effect. When it is eliminated, remove it from the game."
    override def leaves = true
}

// Only appears on the Wyvern's Den tile
case object Wyvern extends CreatureKind {
    val id = "wyvern"
    val title = "Wyvern"
    val value = 9
    val fame = 6
    val shares = false
    val priorities = $(MostUnits, MostBuildings, MostResources)
    val text = "This creature does not share a territory with a player and attacks them when it moves into a territory. All territories are considered to be adjacent to it for purposes of movement. Before combat, the player removes 1 unit. If it loses a combat while outside its Den, instead of it being defeated, place it back on its Den. If it loses a combat while inside its Den, remove it from the game."
    override def leaves = true
}

// Wastelands expansion creatures (their rules are in wastelands.scala)
case object RockGolem extends CreatureKind {
    val id = "rock-golem"
    val title = "Rock Golem"
    val value = 7
    val fame = 4
    val shares = true
    val priorities = $(MostUnits, MostBuildings, MostResources)
    val text = "When spawning or moving, this creature attacks if there are buildings in the territory. During combat, both player and this creature add 1 axe for each skull rolled. If the creature rolls a skull/axe it always chooses the skull."
}

case object Myrkalf extends CreatureKind {
    val id = "myrkalf"
    val title = "Myrkalf"
    val value = 6
    val fame = 3
    val shares = true
    val priorities = $(MostBuildings, MostResources, MostUnits)
    val text = "Only when moving, this creature moves twice, without returning to its starting point. The owner of the territory in which this creature ends its movement must pay 1 wood or lose 2 fame."
}

case object GiantBoar extends CreatureKind {
    val id = "giant-boar"
    val title = "Giant Boar"
    val value = 5
    val fame = 2
    val shares = true
    val priorities = $(MostResources, MostBuildings, MostUnits)
    val text = "When spawning or moving into a territory with wood (including buildings), this creature attacks. When attacking, add 1 skull to its die, and if the defender wins, increase their reward by 2 fame."
}

case object Kobold extends CreatureKind {
    val id = "kobold"
    val title = "Kobold"
    val value = 3
    val fame = 0
    val shares = true
    val priorities = $(MostBuildings, MostUnits, MostResources)
    val text = "The territory containing this creature doesn't generate fame during Harvest (including buildings). However, once per Harvest, the owner of that territory may exchange 1 food for 1 wood (or vice-versa)."
}

case object Valdemar extends CreatureKind {
    val id = "valdemar"
    val title = "Valdemar"
    val value = 7
    val fame = 4
    val shares = true
    val priorities = $(MostResources, MostBuildings, MostUnits)
    val text = "When spawning or after moving, this creature removes 1 unit from its territory. While Valdemar is alive, all other creatures gain 1 axe bonus in all combats."
}

// Only on the Hrimgandr's Lair central tile; never moves
case object Hrimgandr extends CreatureKind {
    val id = "hrimgandr"
    val title = "Hrimgandr"
    val value = 8
    val fame = 8
    val shares = false
    val priorities = $
    val text = "Hrimgandr doesn't move during the creature phase and doesn't share its territory: it only defends itself. While Hrimgandr is alive, Winter costs are increased by one step for every player. If it is defeated, remove it from the game."
    override def leaves = true
}

// A creature card and its miniature; n is the color: 1 beige, 2 brown, 3 dark brown
case class Creature(kind : CreatureKind, n : Int) extends Card {
    def info = CardInfo(kind.title, "card-creature-" + kind.id + "-" + n, kind.fame, false, MapEffect, kind.text)
    def color = n match { case 1 => "beige" ; case 2 => "brown" ; case _ => "dark brown" }
    def token = "creature-" + kind.id + "-" + n
    override def removable = false
    override def elem : Elem = kind.title.styled(styles.creature) ~ " (" ~ color.txt ~ ")"
}

object Creature {
    val all : $[Creature] = $(Creature(CreatureWolf, 1), Creature(CreatureWolf, 2), Creature(CreatureWolf, 3), Creature(BrownBear, 1), Creature(BrownBear, 2), Creature(Draugr, 1), Creature(Draugr, 2), Creature(FallenValkyrie, 1), Creature(FallenValkyrie, 2))

    // Wilderness: added to the creature deck
    val wild : $[Creature] = $(Creature(DraugrJotunn, 1), Creature(DraugrJotunn, 2), Creature(Eldthurs, 1), Creature(Eldthurs, 2), Creature(Hvedrung, 1))

    // Wilderness: set aside for the Ancestral Graveyard and the Wyvern's Den
    val spectral : $[Creature] = $(Creature(SpectralWarrior, 1), Creature(SpectralWarrior, 2))
    val wyvern = Creature(Wyvern, 1)

    val expansion : $[Creature] = wild ++ spectral :+ wyvern

    // Wastelands: added to the creature deck; Hrimgandr only on its Lair
    val waste : $[Creature] = $(Creature(RockGolem, 1), Creature(RockGolem, 2), Creature(Myrkalf, 1), Creature(Myrkalf, 2), Creature(GiantBoar, 1), Creature(GiantBoar, 2), Creature(Kobold, 1), Creature(Kobold, 2), Creature(Valdemar, 1))
    val hrimgandr = Creature(Hrimgandr, 1)

    // The creature deck of a game
    def deck(implicit game : Game) : $[Creature] = all ++ game.has(Wilderness).??(wild) ++ game.has(Wastelands).??(waste)
}

// A creature attacked by the current Move action, in the territory with this anchor
case class CreatureFight(area : AreaRef, creature : Creature) extends Record


// The More Creatures variant: after passing, a player may make a creature appear
case object MoreCreatures extends GameOption with ToggleOption {
    val group = "Creatures".txt
    def valueOn = "More creatures!".txt
    override val explain = $(
        "Rulebook variant: after taking their Development card, a player may make a creature appear on a free lair (or in any territory without a creature), unless there are already as many creatures on the map as players.",
        "Needs the " ~ "Creatures".hl ~ " module.",
    )
    override def required(all : $[BaseOption]) = $($(ModuleOption(Creatures)))
}


// SETUP
case class ShuffledCreaturesAction(shuffled : $[Creature], tiles : $[String]) extends ShuffledAction[Creature]
case class ShuffledCreaturesRestAction(top : $[Creature], shuffled : $[Creature], tiles : $[String]) extends ShuffledAction[Creature]

// APPARITION
case class CreatureAppearAction(f : Faction, area : AreaRef, activate : Boolean, then : ForcedAction) extends ForcedAction
case class ShuffledCreatureDiscardAction(shuffled : $[Creature], f : Faction, area : AreaRef, activate : Boolean, then : ForcedAction) extends ShuffledAction[Creature]
case class CreatureEffectAction(c : Creature, then : ForcedAction) extends ForcedAction

// CREATURE PHASE
case class CreatureActivateAction(l : $[Creature]) extends ForcedAction
case class CreatureMoveChoiceAction(self : Faction, c : Creature, to : AreaRef, l : $[Creature]) extends BaseAction("Creature phase:", c, "is tied between territories; move it to")(to) with MapTarget { def target = to }
case class CreatureMoveAction(c : Creature, to : AreaRef, l : $[Creature]) extends ForcedAction

// COMBAT
case class CreatureDeclareAction(f : Faction, e : MoveEffect, left : $[AreaRef], then : ForcedAction) extends ForcedAction
case class CreatureAttackAction(self : Faction, area : AreaRef, c : Creature, e : MoveEffect, left : $[AreaRef], then : ForcedAction) extends BaseAction("Attack a creature")(c, "in", area) with MapTarget { def target = area }
case class CreatureDeclareDoneAction(self : Faction, e : MoveEffect, then : ForcedAction) extends BaseAction("Attack a creature")("Attack no more creatures")
case class CreatureFightAction(self : Faction, area : AreaRef, c : Creature, e : MoveEffect, then : ForcedAction) extends BaseAction("Fight in")(area, "against", c) with MapTarget { def target = area }
// attacking: the player attacks the creature; otherwise the creature attacks the player
case class CreatureCombatAction(f : Faction, area : AreaRef, c : Creature, e : MoveEffect, attacking : Boolean, then : ForcedAction) extends ForcedAction
case class CreatureFoodAction(self : Faction, area : AreaRef, c : Creature, e : MoveEffect, attacking : Boolean, food : Int, then : ForcedAction) extends BaseAction(self, "spends food for the fight against", c)((food == 0).?("No food".txt).|(food.hl ~ " " ~ Food.elem))
case class CreaturePlayerRolledAction(f : Faction, area : AreaRef, c : Creature, e : MoveEffect, attacking : Boolean, food : Int, random : DieFace, then : ForcedAction) extends RandomAction[DieFace]
// Liv's reroll (Warchiefs module), and going on with the player's face
case class CreatureRerollAction(f : Faction, area : AreaRef, c : Creature, e : MoveEffect, attacking : Boolean, food : Int, then : ForcedAction) extends ForcedAction
case class CreatureRerolledAction(f : Faction, area : AreaRef, c : Creature, e : MoveEffect, attacking : Boolean, food : Int, random : DieFace, then : ForcedAction) extends RandomAction[DieFace]
case class CreatureFaceAction(f : Faction, area : AreaRef, c : Creature, e : MoveEffect, attacking : Boolean, food : Int, face : DieFace, then : ForcedAction) extends ForcedAction
case class CreatureCunningAskAction(f : Faction, area : AreaRef, c : Creature, e : MoveEffect, spent : $[Resource], then : ForcedAction) extends ForcedAction
case class CreatureCunningAction(self : Faction, area : AreaRef, c : Creature, e : MoveEffect, r : Resource, spent : $[Resource], then : ForcedAction) extends BaseAction("Liv's Cunning".hl, "spend for the fight", spent.any.?("(" ~ spent./(_.elem).join(" ") ~ " so far)").|(Empty))("1", r)
case class CreatureCunningDoneAction(self : Faction, area : AreaRef, c : Creature, e : MoveEffect, spent : $[Resource], then : ForcedAction) extends BaseAction("Liv's Cunning".hl)(spent.none.?("Spend nothing").|("Done"))
case class CreatureFoodPaidAction(f : Faction, area : AreaRef, c : Creature, e : MoveEffect, attacking : Boolean, food : Int, then : ForcedAction) extends ForcedAction
case class CreatureFoodStartAction(f : Faction, area : AreaRef, c : Creature, e : MoveEffect, attacking : Boolean, then : ForcedAction) extends ForcedAction
case class CreatureRolledAction(f : Faction, area : AreaRef, c : Creature, e : MoveEffect, attacking : Boolean, food : Int, face : DieFace, random : DieFace, then : ForcedAction) extends RandomAction[DieFace]

// MORE CREATURES VARIANT
case class MoreCreaturesAction(self : Faction, area : AreaRef, then : ForcedAction) extends BaseAction("More creatures!", "a new creature appears in")(area) with MapTarget { def target = area }
case class MoreCreaturesSkipAction(self : Faction, then : ForcedAction) extends BaseAction("More creatures!")("No new creature")


object CreaturesExpansion extends Expansion {
    // Territories f controls that hold creatures it could attack with a Move action
    def attackable(f : Faction)(implicit game : Game) : $[Territory] =
        game.has(Creatures).??(game.controlled(f).%(t => game.creaturesIn(t).any))

    def buildingPoints(t : Territory)(implicit game : Game) = game.buildingsIn(t).map(x => x._2.large.?(3).|(1)).sum

    def score(p : CreaturePriority, t : Territory)(implicit game : Game) : Int = p match {
        case MostBuildings => buildingPoints(t)
        case MostUnits => game.seating./(game.figures(t, _)).sum
        case MostResources =>
            val (food, wood, lore) = game.produce(t)
            food + wood + lore
    }

    // Where a creature moves: an adjacent territory (Rough borders don't matter) without a creature,
    // with units if possible, then by its priorities; the first player breaks the remaining ties.
    // Every territory is adjacent to the Wyvern
    def destinations(c : Creature)(implicit game : Game) : $[Territory] = {
        val t = game.board.territory(game.creatureAt(c))
        val near = (c.kind == Wyvern).?(game.board.territories.but(t)).|(game.board.adjacent(t).map(_._1))
        val free = near.%(o => game.creaturesIn(o).none)
        val peopled = free.%(o => game.present(o).any)

        c.kind.priorities.foldLeft(peopled.any.?(peopled).|(free)) { (l, p) =>
            if (l.none) l else { val m = l./(score(p, _)).max ; l.%(score(p, _) == m) }
        }
    }

    def removeCreature(c : Creature)(implicit game : Game) {
        game.creatureLine = game.creatureLine.but(c)
        game.creatureAt -= c
        if (c.kind.leaves.not)
            game.creatureDiscard :+= c
    }

    def perform(action : Action, soft : Void)(implicit game : Game) = action @@ {
        // SETUP: N+1 creatures of value 6 or less on top, the rest shuffled below
        case ShuffledTilesAction(tiles) =>
            Shuffle[Creature](Creature.deck.%(_.kind.value <= 6), ShuffledCreaturesAction(_, tiles))

        case ShuffledCreaturesAction(low, tiles) =>
            val top = low.take(factions.num + 1)
            Shuffle[Creature](low.drop(top.num) ++ Creature.deck.%(_.kind.value > 6), ShuffledCreaturesRestAction(top, _, tiles))

        case ShuffledCreaturesRestAction(top, rest, tiles) =>
            game.creatureDeck = top ++ rest

            log("The creature cards were shuffled")

            MapExpansion.perform(ShuffledTilesAction(tiles), soft)

        // A tile with a lair: a creature appears there; during setup it does nothing else
        case TilePlacedAction(f, tile, spot, setup, then) =>
            Then(Tiles(tile).areas.%(_.lair).foldRight(then)((a, next) => CreatureAppearAction(f, AreaRef(spot.x, spot.y, a.id), setup.not, next)))

        // APPARITION
        case CreatureAppearAction(f, area, activate, then) =>
            if (game.creatureDeck.none && game.creatureDiscard.any)
                Shuffle[Creature](game.creatureDiscard, ShuffledCreatureDiscardAction(_, f, area, activate, then))
            else
            if (game.creatureDeck.none) {
                log("No creature card was left to appear")
                Then(then)
            }
            else {
                val c = game.creatureDeck.head
                game.creatureDeck = game.creatureDeck.drop(1)
                game.creatureLine :+= c
                game.creatureAt += c -> area

                log(c, "appeared in", game.board.territory(area).anchor)

                if (activate)
                    Then(CreatureEffectAction(c, then))
                else
                    Then(then)
            }

        case ShuffledCreatureDiscardAction(l, f, area, activate, then) =>
            game.creatureDeck = l
            game.creatureDiscard = $

            log("The creature discard pile was shuffled into a new draw pile")

            Then(CreatureAppearAction(f, area, activate, then))

        // What a creature does when it appears or moves
        case CreatureEffectAction(c, then) =>
            val t = game.board.territory(game.creatureAt(c))
            val owner = game.present(t).single

            (c.kind, owner) match {
                case (Draugr, Some(o)) =>
                    game.removeFigures(t, o, 1)
                    game.note("draugr")
                    o.log("lost a unit to", c, "in", t.anchor)
                    Then(then)

                case (FallenValkyrie, Some(o)) =>
                    log(c, "attacks", o, "in", t.anchor)
                    Then(CreatureCombatAction(o, t.anchor, c, MoveEffect(0), false, then))

                case _ =>
                    Then(then)
            }

        // 2.5 CREATURE PHASE: from left to right, each creature moves, then acts
        case CreaturePhaseAction =>
            if (game.creatureLine.any) {
                log(SingleLine)
                log("Creature phase")
            }

            Then(CreatureActivateAction(game.creatureLine))

        case CreatureActivateAction(Nil) =>
            Then(ScorchedHarvestAction)

        case CreatureActivateAction(c :: rest) =>
            if (game.creatureLine.has(c).not)
                Then(CreatureActivateAction(rest))
            else {
                val l = destinations(c)

                if (l.none) {
                    log(c, "could not move")
                    Then(CreatureEffectAction(c, CreatureActivateAction(rest)))
                }
                else
                if (l.num == 1)
                    Then(CreatureMoveAction(c, l.head.anchor, rest))
                else
                    Ask(game.first).each(l)(t => CreatureMoveChoiceAction(game.first, c, t.anchor, rest))
            }

        case CreatureMoveChoiceAction(f, c, to, rest) =>
            Then(CreatureMoveAction(c, to, rest))

        case CreatureMoveAction(c, to, rest) =>
            game.creatureAt += c -> to
            game.note("creature-move")

            log(c, "moved to", to)

            Then(CreatureEffectAction(c, CreatureActivateAction(rest)))

        // COMBAT: once the moves are made, a Fallen Valkyrie with the player's units must be fought,
        // and the player may attack one other creature in each territory they share with creatures
        case MoveEndAction(f, e, then) =>
            val l = game.board.territories.%(t => game.present(t) == $(f) && game.creaturesIn(t).any)
            val forced = l.%(game.hostileIn)

            game.creatureFights = forced./(t => CreatureFight(t.anchor, game.creaturesIn(t).%(_.kind.shares.not).head))

            game.creatureFights.foreach(x => f.log("must fight", x.creature, "in", x.area))

            Then(CreatureDeclareAction(f, e, l.diff(forced)./(_.anchor), then))

        case CreatureDeclareAction(f, e, left, then) =>
            if (left.none)
                Then(CombatsAction(f, e, then))
            else
                Ask(f)
                    .some(left)(a => game.creaturesIn(game.board.territory(a))./(c => CreatureAttackAction(f, a, c, e, left, then)))
                    .add(CreatureDeclareDoneAction(f, e, then))

        case CreatureAttackAction(f, a, c, e, left, then) =>
            game.creatureFights :+= CreatureFight(a, c)

            f.log("will attack", c, "in", a)

            Then(CreatureDeclareAction(f, e, left.but(a), then))

        case CreatureDeclareDoneAction(f, e, then) =>
            Then(CombatsAction(f, e, then))

        case CreatureFightAction(f, a, c, e, then) =>
            game.creatureFights = game.creatureFights.%(_.area != a)

            log(f, "attacks", c, "in", a)

            Then(CreatureCombatAction(f, a, c, e, true, then))

        // The player may spend food; the creature can't
        case CreatureCombatAction(f, a, c, e, attacking, then) =>
            game.fights += 1
            game.note("creature-fight")
            game.battle = |(a)

            // The Wyvern (Wilderness): the player first removes 1 unit, and may have none left
            val t = game.board.territory(a)

            if (c.kind == Wyvern) {
                game.removeFigures(t, f, 1)
                f.log("removed a unit before fighting", c)
            }

            if (game.figures(t, f) == 0) {
                log(c, "won the fight in", a)
                Then(FightOverAction(then))
            }
            else
            // Step 1: Signy and Brand (Warchiefs module)
            if (game.has(Warchiefs))
                Then(ChiefStepOneAction($(f), a, CreatureFoodStartAction(f, a, c, e, attacking, FightOverAction(then))))
            else
                Then(CreatureFoodStartAction(f, a, c, e, attacking, FightOverAction(then)))

        // Liv's Cunning: wood or lore may be spent like food, one at a time
        case CreatureFoodStartAction(f, a, c, e, attacking, then) if attacking && e.special == LivMove =>
            Then(CreatureCunningAskAction(f, a, c, e, $, then))

        case CreatureCunningAskAction(f, a, c, e, spent, then) =>
            val max = game.figures(game.board.territory(a), f)

            Ask(f)
                .some(Resource.all.%(r => f.has(r) > 0 && spent.num < max))(r => $(CreatureCunningAction(f, a, c, e, r, spent, then)))
                .add(CreatureCunningDoneAction(f, a, c, e, spent, then))

        case CreatureCunningAction(f, a, c, e, r, spent, then) =>
            f.gain(r, -1)

            Then(CreatureCunningAskAction(f, a, c, e, spent :+ r, then))

        case CreatureCunningDoneAction(f, a, c, e, spent, then) =>
            if (spent.any)
                f.log("spent", spent./(_.elem).join(" "), "with", "Liv's Cunning".hl)

            Then(CreatureFoodPaidAction(f, a, c, e, true, spent.num, then))

        case CreatureFoodStartAction(f, a, c, e, attacking, then) =>
            val t = game.board.territory(a)
            val max = math.min(f.food, game.figures(t, f))

            if (max == 0)
                Then(CreatureFoodAction(f, a, c, e, attacking, 0, then))
            else
                Ask(f).each(0.to(max).$)(k => CreatureFoodAction(f, a, c, e, attacking, k, then))

        case CreatureFoodAction(f, a, c, e, attacking, food, then) =>
            f.food -= food

            if (food > 0)
                f.log("spent", food.hl, Food)

            Then(CreatureFoodPaidAction(f, a, c, e, attacking, food, then))

        case CreatureFoodPaidAction(f, a, c, e, attacking, food, then) =>
            Random[DieFace](NorthgardDie.faces, CreaturePlayerRolledAction(f, a, c, e, attacking, food, _, then))

        // Casualties don't hurt creatures, so a player always takes the point
        case CreaturePlayerRolledAction(f, a, c, e, attacking, food, face, then) =>
            f.log("rolled", face)

            // Liv may reroll once (Warchiefs module)
            if (Warchief.reroll(game.board.territory(a), f))
                Ask(f)
                    .add(LivRerollAction(f, CreatureRerollAction(f, a, c, e, attacking, food, then)))
                    .add(LivKeepAction(f, CreatureFaceAction(f, a, c, e, attacking, food, face, then)))
            else
                Then(CreatureFaceAction(f, a, c, e, attacking, food, face, then))

        case CreatureRerollAction(f, a, c, e, attacking, food, then) =>
            Random[DieFace](NorthgardDie.faces, CreatureRerolledAction(f, a, c, e, attacking, food, _, then))

        case CreatureRerolledAction(f, a, c, e, attacking, food, face, then) =>
            f.log("rolled", face)

            Then(CreatureFaceAction(f, a, c, e, attacking, food, face, then))

        case CreatureFaceAction(f, a, c, e, attacking, food, face, then) =>
            Random[DieFace](NorthgardDie.faces, CreatureRolledAction(f, a, c, e, attacking, food, face.choice.?(NorthgardDie.point).|(face), _, then))

        // A creature's die is rolled by another player; it always takes the point
        case CreatureRolledAction(f, a, c, e, attacking, food, face, random, then) =>
            log(c, "rolled", random)

            // Wastelands: a Rock Golem takes the skull, a Giant Boar attacking adds one
            val cface = game.has(Wastelands).?(WastelandsExpansion.creatureFace(c, attacking, random)).|(random.choice.?(NorthgardDie.point).|(random))

            val t = game.board.territory(a)
            val here = game.working(t)
            val units = game.figures(t, f)

            // The attacker's card bonus or the defender's Fortresses; Defense Towers only add casualties, which creatures ignore
            val bonus = attacking.?(e.bonus).|(0)
            val axe = (attacking && e.special == AxeMove).??(1)
            val fortress = attacking.not.??(2 * here.count(_ == Fortress))
            val snake = (f == Snake && game.scorchedIn(t)).??(1)
            // Wastelands: Rock Golem, Valdemar, Thor's Wrath, Landvidi, the Gate of Helheim and Urdarbrunn
            val (pw, cw) = game.has(Wastelands).?(WastelandsExpansion.creaturePoints(f, t, c, attacking, face, cface)).|((0, 0))
            val urdar = game.has(Wastelands).??(WastelandsExpansion.creatureIgnored(f, t, attacking))
            val ps = game.strength(t, f, attacking) + bonus + axe + fortress + snake + food + face.points + pw
            // Shieldbearers cancel 1 casualty; Halvard defending ignores 1 inflicted by the attacking creature
            val shield = math.min(cface.casualties, (attacking && (e.special == ShieldMove || e.special == BorgildMove)).??(1) + Warchief.shield(t, f, attacking) + urdar)
            val pc = cface.casualties - shield
            val cs = c.kind.value + cface.points + cw

            def extra(l : (Int, Elem)*) : Elem = l.toList.filter(_._1 > 0).map { case (n, what) => "(" ~ n.hl ~ " from " ~ what ~ ")" }.join(" ")

            f.log("scored", ps.hl, extra(bonus -> "the card".txt, axe -> "Axe Throwers".hl, fortress -> Fortress.elem, snake -> "Scorched Earth".hl, pw -> "Wastelands".hl))
            log(c, "scored", cs.hl, extra(cw -> "Wastelands".hl), "and inflicted", pc.hl, (pc == 1).?("casualty").|("casualties"), (shield > 0).?("(" ~ shield.hl ~ " cancelled)").|(Empty))

            // Losing all units loses the fight; otherwise ties go to the defender
            val won = pc < units && attacking.?(ps > cs).|(ps >= cs)

            val before = game.count(t, f)
            game.removeFigures(t, f, math.min(pc, units))

            // Alternative victory: Valhalla counts units lost to creatures too
            game.advance(f, "valhalla", math.min(pc, units))

            // Svarn's Menders: the attacker's casualties wait on the card
            if (attacking && e.special == SvarnMove)
                game.mended += before - game.count(t, f)

            // The Wyvern goes back to its Den unless it lost there (Wilderness)
            val den = (c.kind == Wyvern && Wild.homeIn(t).not).??(Wild.homes)

            if (won && den.any) {
                game.creatureAt += c -> den.head
                game.note("wyvern-back")

                f.log("drove", c, "back to its Den")

                Then(then)
            }
            else
            if (won) {
                removeCreature(c)
                f.fame += c.kind.fame
                game.note("creature-defeated")
                game.advance(f, "hunting")

                f.log("defeated", c, "and gained", c.kind.fame.hl, "fame", c.kind.leaves.?("(it leaves the game)".txt).|(Empty))

                // Wastelands: a Giant Boar that attacked gives 2 more fame
                if (c.kind == GiantBoar && attacking.not) {
                    f.fame += 2
                    f.log("gained", 2.hl, "more fame for defeating", c, "as the defender")
                }

                if (attacking && f == Wolf) {
                    f.food += 1
                    f.log("collected", 1.hl, Food, "for winning as the attacker")
                }

                // Beating a Fallen Valkyrie takes its territory
                if (attacking && f == Stag && c.kind.shares.not) {
                    f.fame += 1
                    f.log("gained", 1.hl, "fame for conquering a territory")
                }

                Then(then)
            }
            else {
                log(c, "won the fight in", a)

                // Attacking a creature that shares its territory, the units simply stay
                if (game.figures(t, f) > 0 && (attacking.not || c.kind.shares.not))
                    Then(RetreatAction(f, a, false, then))
                else
                    Then(then)
            }

        // MORE CREATURES VARIANT
        case PassedAction(f) if options.has(MoreCreatures) =>
            val free = game.board.territories.%(t => game.creaturesIn(t).none)
            val lairs = free.%(t => t.areas.exists(a => game.board.spec(a).lair))

            if (game.creatureLine.num < factions.num && (game.creatureDeck.any || game.creatureDiscard.any) && free.any)
                Ask(f).each(lairs.any.?(lairs).|(free))(t => MoreCreaturesAction(f, t.anchor, NextTurnAction(f))).add(MoreCreaturesSkipAction(f, NextTurnAction(f)))
            else
                Then(NextTurnAction(f))

        // On the lair if there is one; the creature doesn't move but acts
        case MoreCreaturesAction(f, a, then) =>
            val t = game.board.territory(a)
            val area = t.areas.find(x => game.board.spec(x).lair).|(a)

            game.note("more-creatures")

            Then(CreatureAppearAction(f, area, true, then))

        case MoreCreaturesSkipAction(f, then) =>
            Then(then)

        case _ => UnknownContinue
    }
}
