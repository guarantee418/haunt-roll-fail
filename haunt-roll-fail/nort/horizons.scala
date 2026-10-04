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


// Uncharted Horizons: the Events module and the Alternative victory conditions module.
// Rules from the Uncharted Horizons rulebook on Tabletopia (work in progress); card texts and images from the
// Tabletop Simulator mod 3597126237 (20 Event cards, 8 Map Control and 13 Wealth cards). RULES.md has the summary

// EVENTS

// phase: when the Event acts
case class EventCard(id : String) extends Card {
    def info = EventCard.info(id)
    override def elem = name.styled(styles.fame)
    override def img = Image(info.image, styles.card)
    def phase = EventCard.phases(id)
}

object EventCard {
    private def card(id : String, name : String, phase : String, text : String) = id -> (CardInfo(name, "card-event-" + id, 0, false, MapEffect, text), phase)

    val list : $[(String, (CardInfo, String))] = $(
        card("gods-favor", "God's Favor", "1. Start of the year", "After drawing cards, each player chooses either to: collect 1 lore OR gain 3 fame."),
        card("myrkalfars-levy", "Myrkálfar's Levy", "3. Harvest", "Before Harvesting, each player chooses one of the territories they control that is generating 2 or more resources. They must choose either not to collect the resources of that territory OR remove 1 unit from that territory."),
        card("new-horizons", "New Horizons", "2. Actions", "Explore action: each enemy or neutral territory players close grants them 2 fame. If the territory has 4 or more tiles, they collect 1 additional lore."),
        card("bountiful-year", "Bountiful Year", "3. Harvest", "Before Harvesting, players must choose to not collect any fame for one territory they control. Instead, collect double resources (including buildings) from it."),
        card("happy-people", "Happy People", "1. Start of the year", "Before drawing cards, each player can either: remove 1 Unrest card (from their draw or discard pile) OR gain 3 fame and add 1 unit to a territory they control."),
        card("offerings", "Offerings", "1. Start of the year", "After drawing cards, each player may discard 1 lore to either: draw 1 card OR choose and take any 1 card from their discard pile."),
        card("draugr-invasion", "Draugr Invasion", "3. Harvest", "After Harvesting, each player receives 2 fame and removes 2 units from each territory they control with at least 3 units. They remove 1 less unit for each Defense Tower and 2 less units for each Fortress in that territory."),
        card("infestation", "Infestation", "3. Harvest", "Before Harvesting, for each territory they control generating 2 or more resources, players must either: collect 1 food less and gain 1 fame OR remove 1 unit from the territory."),
        card("earthquake", "Earthquake", "3. Harvest", "After Harvesting, each player pays 1 wood for each territory they control with at least 1 building. For each wood paid, collect 1 fame. For each unpaid wood, remove a building (maximum one removed building per territory)."),
        card("early-spring", "Early Spring", "1. Start of the year", "After drawing cards, each player may choose to collect 1 food OR to collect 1 wood OR to draw 1 extra card."),
        card("sailor-ghosts", "Sailor Ghosts", "3. Harvest", "Before Harvesting, each player selects the open territory they control with the most units (they can choose between a tie) and removes 2 units from it. They collect 2 fame for each unit removed."),
        card("volcano-eruption", "Volcano Eruption", "1. Start of the year", "After drawing cards, players must remove 1 building from a territory they control (if they have any). If a small building is removed this way, they draw 1 card. If it was a large building, they draw 2 cards instead."),
        card("blood-moon", "Blood Moon", "All combats", "During each combat, add 1 casualty to both Defender and Attacker die rolls. Players collect 1 fame for each unit lost in combat."),
        card("blizzard", "Blizzard", "4. Winter", "Players pay 1 additional food and 1 additional wood on top of normal Winter costs."),
        card("krakens-attack", "Kraken's Attack", "3. Harvest", "Before Harvesting, players who control any open territory must remove 1 unit from one of them and discard 1 wood if available. Otherwise, they gain 2 fame."),
        card("ceremonial-bonfire", "Ceremonial Bonfire", "3. Harvest", "Earn 1 fame for each resource trade you make this year. Exceptionally, players can trade 2 wood for 1 lore."),
        card("frozen-sea", "Frozen Sea", "3. Harvest", "Instead of earning fame for each closed territory, players earn 1 fame per territory controlled, regardless of their size and whether it is closed or open."),
        card("conquests", "Conquests", "All combats", "During combat, the attacker adds 1 point to his die result. If the defender wins the battle, he gains 2 fame."),
        card("harsh-winter", "Harsh Winter", "4. Winter", "During Winter Costs, consider that you have 1 more unit for each of the closed territories you control."),
        card("supply-from-homeland", "Supply from Homeland", "1. Start of the year", "After drawing cards, each player must choose: collect 1 resource OR draw 1 more card."),
    )

    val info : Map[String, CardInfo] = list.map { case (id, (c, _)) => id -> c }.toMap
    val phases : Map[String, String] = list.map { case (id, (_, p)) => id -> p }.toMap
    val all : $[EventCard] = list.map(x => EventCard(x._1))

    // When each kind acts
    val start = $("gods-favor", "offerings", "early-spring", "supply-from-homeland", "volcano-eruption", "happy-people")
    val before = $("myrkalfars-levy", "bountiful-year", "infestation", "sailor-ghosts", "krakens-attack")
    val after = $("draugr-invasion", "earthquake")
}

case class ShuffledEventsAction(shuffled : $[EventCard], achievements : $[Card]) extends ShuffledAction[EventCard]
case class EventStepAction(step : String, l : $[Faction], then : ForcedAction) extends ForcedAction
case class EventGainAction(self : Faction, card : EventCard, r : |[Resource], fame : Int, draw : Int, step : String, l : $[Faction], then : ForcedAction) extends BaseAction(card)(EventGainLabel(r, fame, draw))
case class OfferingsTakeAction(self : Faction, card : Card, step : String, l : $[Faction], then : ForcedAction) extends BaseAction(EventCard("offerings"), "discard", 1.hl, Lore, "to take from the discard pile")(card.img, Break, card)
case class VolcanoAction(self : Faction, space : SpaceRef, building : Building, step : String, l : $[Faction], then : ForcedAction) extends BaseAction(EventCard("volcano-eruption"), "remove a building in", space.area)(building) with MapTarget { def target = space.area }
case class HappyUnrestAction(self : Faction, step : String, l : $[Faction], then : ForcedAction) extends BaseAction(EventCard("happy-people"))("Remove 1", UnrestCard)
case class HappyFameAction(self : Faction, area : |[AreaRef], step : String, l : $[Faction], then : ForcedAction) extends BaseAction(EventCard("happy-people"), "gain", 3.hl, "fame and add a unit")(area./(a => a : Any).|("Gain 3 fame (no unit)".txt))
case class LevyAction(self : Faction, area : AreaRef, remove : Boolean, step : String, l : $[Faction], then : ForcedAction) extends BaseAction(EventCard("myrkalfars-levy"), "in", area)(remove.?("Remove 1 unit").|("Don't collect its resources")) with MapTarget { def target = area }
case class BountifulAction(self : Faction, area : AreaRef, step : String, l : $[Faction], then : ForcedAction) extends BaseAction(EventCard("bountiful-year"), "no fame, double resources from")(area) with MapTarget { def target = area }
case class InfestationAction(f : Faction, areas : $[AreaRef], step : String, l : $[Faction], then : ForcedAction) extends ForcedAction
case class InfestationChoiceAction(self : Faction, area : AreaRef, remove : Boolean, rest : $[AreaRef], step : String, l : $[Faction], then : ForcedAction) extends BaseAction(EventCard("infestation"), "in", area)(remove.?("Remove 1 unit").|("1 food less, gain 1 fame")) with MapTarget { def target = area }
case class SailorAction(self : Faction, area : AreaRef, step : String, l : $[Faction], then : ForcedAction) extends BaseAction(EventCard("sailor-ghosts"), "remove 2 units from")(area) with MapTarget { def target = area }
case class KrakenAttackAction(self : Faction, area : AreaRef, step : String, l : $[Faction], then : ForcedAction) extends BaseAction(EventCard("krakens-attack"), "remove 1 unit from")(area) with MapTarget { def target = area }
case class EarthquakeAction(f : Faction, areas : $[AreaRef], step : String, l : $[Faction], then : ForcedAction) extends ForcedAction
case class EarthquakeRemoveAction(self : Faction, space : SpaceRef, building : Building, rest : $[AreaRef], step : String, l : $[Faction], then : ForcedAction) extends BaseAction(EventCard("earthquake"), "remove a building in", space.area)(building) with MapTarget { def target = space.area }
case class BonfireTradeAction(self : Faction, then : ForcedAction) extends BaseAction("Trade", "with", EventCard("ceremonial-bonfire"))("Pay", Wood, Wood, "for", Lore)

case class EventGainLabel(r : |[Resource], fame : Int, draw : Int) extends Elementary {
    def elem = {
        val l : $[Elem] = r.$./(r => "Collect " ~ 1.hl ~ " " ~ r.elem) ++ (fame > 0).$("Gain " ~ fame.hl ~ " fame") ++ (draw > 0).$(("Draw " + draw + " card").txt)
        l.any.?(l.join(" and ")).|("Nothing".txt)
    }
}


object EventsExpansion extends Expansion {
    // Harvest changes chosen before harvesting
    def fameFrom(t : Territory)(implicit game : Game) : Boolean = game.harvestDouble.exists(t.areas.contains).not

    def harvest(t : Territory)(implicit game : Game) : (Int, Int, Int) = {
        val (food, wood, lore) = game.harvest(t)
        if (game.harvestSkip.exists(t.areas.contains))
            (0, 0, 0)
        else {
            val k = game.harvestDouble.exists(t.areas.contains).?(2).|(1)
            (math.max(0, k * food - game.harvestLessFood.count(t.areas.contains)), k * wood, k * lore)
        }
    }

    // Winter costs: Harsh Winter counts 1 more unit per closed territory, Blizzard adds 1 food and 1 wood
    def winterCost(f : Faction)(implicit game : Game) : (Int, Int) = {
        val harsh = game.eventIs("harsh-winter").??(game.controlled(f).%(game.board.closed).num)
        val (food, wood) = Winter.cost(game.states(f).units + harsh)
        val blizzard = game.eventIs("blizzard").??(1)
        (food + blizzard, wood + blizzard)
    }

    // After an Explore: New Horizons pays for closing enemy or neutral territories; Alternative victory counts f's closed territories
    def explored(f : Faction, closing : $[Territory], closed : $[Territory])(implicit game : Game) {
        game.advance(f, "exploration", closed.num)

        if (game.eventIs("new-horizons"))
            closing.%(t => game.present(t).forall(game.enemy(f, _))).foreach { t =>
                game.states(f).fame += 2
                val lore = (game.board.tiles(t) >= 4).??(1)
                game.states(f).lore += lore
                f.log("gained", 2.hl, "fame", (lore > 0).?("and " ~ 1.hl ~ " " ~ Lore.elem).|(Empty), "for closing", t.anchor, "with", EventCard("new-horizons"))
            }
    }

    // After a fight between players: Blood Moon's and Conquests' fame, and the Alternative victory counts
    def afterCombat(attacker : Faction, defender : Faction, aLost : Int, dLost : Int, winner : |[Faction])(implicit game : Game) {
        if (game.eventIs("blood-moon"))
            $(attacker -> aLost, defender -> dLost).foreach { case (f, n) =>
                if (n > 0) {
                    game.states(f).fame += n
                    f.log("gained", n.hl, "fame with", EventCard("blood-moon"))
                }
            }

        if (game.eventIs("conquests") && winner.has(defender)) {
            game.states(defender).fame += 2
            defender.log("gained", 2.hl, "fame with", EventCard("conquests"))
        }

        winner.foreach(w => game.advance(w, "conquest"))
        game.advance(attacker, "valhalla", aLost)
        game.advance(defender, "valhalla", dLost)
    }

    // Territories producing 2 or more resources
    def rich(f : Faction)(implicit game : Game) = game.controlled(f).%{ t => val (a, b, c) = game.harvest(t) ; a + b + c >= 2 }

    def removable(f : Faction)(implicit game : Game) = game.controlled(f).%(t => game.figures(t, f) > 0)

    def next(step : String, l : $[Faction], then : ForcedAction) = EventStepAction(step, l, then)

    def perform(action : Action, soft : Void)(implicit game : Game) = action @@ {
        // SETUP: the face-up Event deck, one card fewer than the years
        case ShuffledAchievementsAction(l) if game.eventsShuffled.not =>
            game.eventsShuffled = true
            Shuffle[EventCard](EventCard.all, ShuffledEventsAction(_, l))

        // Back to the setup (an oracle action can't go through Then)
        case ShuffledEventsAction(l, achievements) =>
            game.eventDeck = l.take(game.lastYear - 1)
            log("Event deck:", game.eventDeck.take(1)./(_.elem), "on top")
            game.internalPerform(ShuffledAchievementsAction(achievements), soft)

        // A new year: from the second year on, the next Event applies
        case StartYearAction =>
            game.eventSteps = $
            game.harvestSkip = $
            game.harvestDouble = $
            game.harvestLessFood = $
            game.event = None

            if (game.year >= 1 && game.eventDeck.any) {
                game.event = |(game.eventDeck.head)
                game.eventDeck = game.eventDeck.drop(1)
                game.note("event-" + game.event.get.id)
            }

            UnknownContinue

        // 1. START OF THE YEAR, after drawing
        case RevealDevelopmentsAction if game.event.exists(e => EventCard.start.has(e.id)) && game.eventSteps.has("start").not =>
            game.eventSteps :+= "start"
            log(game.event.get, "applies this year:", game.event.get.info.text.txt)
            Then(EventStepAction("start", game.from(game.first), RevealDevelopmentsAction))

        case RevealDevelopmentsAction if game.event.any && game.eventSteps.has("shown").not =>
            game.eventSteps :+= "shown"
            if (EventCard.start.has(game.event.get.id).not)
                log(game.event.get, "applies this year:", game.event.get.info.text.txt)
            UnknownContinue

        // 3. HARVEST: before and after
        case ScorchedHarvestAction if game.event.exists(e => EventCard.before.has(e.id)) && game.eventSteps.has("before").not =>
            game.eventSteps :+= "before"
            Then(EventStepAction("before", game.from(game.first), ScorchedHarvestAction))

        case AfterHarvestAction(then) if game.event.exists(e => EventCard.after.has(e.id)) && game.eventSteps.has("after").not =>
            game.eventSteps :+= "after"
            Then(EventStepAction("after", game.from(game.first), AfterHarvestAction(then)))

        case EventStepAction(_, Nil, then) =>
            Then(then)

        case EventStepAction(step, f :: rest, then) =>
            val e = game.event.get
            val n = next(step, rest, then)

            e.id match {
                case "gods-favor" =>
                    Ask(f).add(EventGainAction(f, e, |(Lore), 0, 0, step, rest, then)).add(EventGainAction(f, e, None, 3, 0, step, rest, then))

                case "early-spring" =>
                    Ask(f).add(EventGainAction(f, e, |(Food), 0, 0, step, rest, then)).add(EventGainAction(f, e, |(Wood), 0, 0, step, rest, then))
                        .when(CommonExpansion.available(f) > 0)(EventGainAction(f, e, None, 0, 1, step, rest, then))
                        .add(EventGainAction(f, e, None, 0, 0, step, rest, then))

                case "supply-from-homeland" =>
                    Ask(f).each(Resource.all)(r => EventGainAction(f, e, |(r), 0, 0, step, rest, then))
                        .when(CommonExpansion.available(f) > 0)(EventGainAction(f, e, None, 0, 1, step, rest, then))

                case "offerings" =>
                    if (f.lore > 0)
                        Ask(f).when(CommonExpansion.available(f) > 0)(EventGainAction(f, e, |(Lore), 0, 1, step, rest, then))
                            .each(f.discard.distinct)(c => OfferingsTakeAction(f, c, step, rest, then))
                            .add(n.as("Discard no lore")(e))
                    else
                        Then(n)

                case "volcano-eruption" =>
                    val l = game.controlled(f)./~(game.buildingsIn)
                    if (l.none)
                        Then(n)
                    else
                        Ask(f).each(l)((s, b) => VolcanoAction(f, s, b, step, rest, then))

                case "happy-people" =>
                    val units = (game.reserve(f) > 0).??(game.controlled(f).%(t => game.bearIn(t).not && game.hostileIn(t).not))
                    Ask(f).when(f.unrest > 0)(HappyUnrestAction(f, step, rest, then))
                        .each(units)(t => HappyFameAction(f, |(t.anchor), step, rest, then))
                        .when(units.none)(HappyFameAction(f, None, step, rest, then))

                case "myrkalfars-levy" =>
                    val l = rich(f)
                    if (l.none)
                        Then(n)
                    else
                        Ask(f).each(l)(t => LevyAction(f, t.anchor, false, step, rest, then))
                            .each(l.%(t => game.figures(t, f) > 0))(t => LevyAction(f, t.anchor, true, step, rest, then))

                case "bountiful-year" =>
                    val l = game.controlled(f)
                    if (l.none)
                        Then(n)
                    else
                        Ask(f).each(l)(t => BountifulAction(f, t.anchor, step, rest, then))

                case "infestation" =>
                    Then(InfestationAction(f, rich(f)./(_.anchor), step, rest, then))

                case "sailor-ghosts" =>
                    val open = game.controlled(f).%(game.board.open)
                    val most = open./(t => game.count(t, f)).maxOption.|(0)
                    val l = open.%(t => game.count(t, f) == most && most > 0)
                    if (l.none)
                        Then(n)
                    else
                        Ask(f).each(l)(t => SailorAction(f, t.anchor, step, rest, then))

                case "krakens-attack" =>
                    val l = game.controlled(f).%(game.board.open).%(t => game.figures(t, f) > 0)
                    if (l.none) {
                        f.fame += 2
                        f.log("gained", 2.hl, "fame with", e)
                        Then(n)
                    }
                    else
                        Ask(f).each(l)(t => KrakenAttackAction(f, t.anchor, step, rest, then))

                case "draugr-invasion" =>
                    f.fame += 2
                    f.log("gained", 2.hl, "fame with", e)
                    game.controlled(f).%(t => game.count(t, f) >= 3).foreach { t =>
                        val here = game.working(t)
                        val k = math.max(0, 2 - here.count(_ == DefenseTower) - 2 * here.count(_ == Fortress))
                        if (k > 0) {
                            game.removeUnits(t, f, k)
                            f.log("lost", k.hl, "units in", t.anchor, "to the Draugr")
                        }
                    }
                    Then(n)

                case "earthquake" =>
                    val l = game.controlled(f).%(t => game.buildingsIn(t).any)
                    val paid = math.min(f.wood, l.num)
                    if (paid > 0) {
                        f.wood -= paid
                        f.fame += paid
                        f.log("paid", paid.hl, Wood, "and gained", paid.hl, "fame with", e)
                    }
                    // The territories left unpaid lose a building: the ones with the fewest large buildings first
                    val unpaid = l.sortBy(t => (game.buildingsIn(t).count(_._2.large), game.buildingsIn(t).num)).take(l.num - paid)
                    Then(EarthquakeAction(f, unpaid./(_.anchor), step, rest, then))

                case _ => Then(n)
            }

        case EventGainAction(f, e, r, fame, draw, step, l, then) =>
            val offering = e.id == "offerings"

            if (offering) {
                f.lore -= 1
                f.log("discarded", 1.hl, Lore, "and drew a card")
            }
            else {
                r.foreach { r =>
                    f.gain(r, 1)
                    f.log("collected", 1.hl, r)
                }
                if (fame > 0) {
                    f.fame += fame
                    f.log("gained", fame.hl, "fame")
                }
                if (draw > 0)
                    f.log("drew", draw.cards)
            }

            Then(DrawCardsAction(f, draw, next(step, l, then)))

        case OfferingsTakeAction(f, c, step, l, then) =>
            f.lore -= 1
            f.discard = f.discard.diff($(c))
            f.hand :+= c
            f.log("discarded", 1.hl, Lore, "and took", c, "from their discard pile")
            Then(next(step, l, then))

        case VolcanoAction(f, s, b, step, l, then) =>
            game.buildings -= s
            val n = b.large.?(2).|(1)
            f.log("lost", b, "in", s.area, "and drew", n.cards)
            Then(DrawCardsAction(f, n, next(step, l, then)))

        case HappyUnrestAction(f, step, l, then) =>
            if (f.discard.has(UnrestCard))
                f.discard = f.discard.diff($(UnrestCard))
            else
            if (f.draw.has(UnrestCard))
                f.draw = f.draw.diff($(UnrestCard))
            else
                f.hand = f.hand.diff($(UnrestCard))

            f.log("removed an", UnrestCard, "card")
            Then(next(step, l, then))

        case HappyFameAction(f, a, step, l, then) =>
            f.fame += 3
            a.foreach(a => game.addUnits(a, f, 1))
            f.log("gained", 3.hl, "fame", a./(a => "and added a unit in " ~ a.elem).|(Empty))
            Then(next(step, l, then))

        case LevyAction(f, a, remove, step, l, then) =>
            if (remove) {
                game.removeFigures(game.board.territory(a), f, 1)
                f.log("removed a unit from", a, "for the", EventCard("myrkalfars-levy"))
            }
            else {
                game.harvestSkip :+= a
                f.log("won't collect the resources of", a)
            }
            Then(next(step, l, then))

        case BountifulAction(f, a, step, l, then) =>
            game.harvestDouble :+= a
            f.log("will collect double resources and no fame from", a)
            Then(next(step, l, then))

        case InfestationAction(f, Nil, step, l, then) =>
            Then(next(step, l, then))

        case InfestationAction(f, a :: rest, step, l, then) =>
            Ask(f).add(InfestationChoiceAction(f, a, false, rest, step, l, then))
                .when(game.figures(game.board.territory(a), f) > 0)(InfestationChoiceAction(f, a, true, rest, step, l, then))

        case InfestationChoiceAction(f, a, remove, rest, step, l, then) =>
            if (remove) {
                game.removeFigures(game.board.territory(a), f, 1)
                f.log("removed a unit from", a)
            }
            else {
                game.harvestLessFood :+= a
                f.fame += 1
                f.log("will collect 1 food less from", a, "and gained", 1.hl, "fame")
            }
            Then(InfestationAction(f, rest, step, l, then))

        case SailorAction(f, a, step, l, then) =>
            val t = game.board.territory(a)
            val n = math.min(2, game.count(t, f))
            game.removeUnits(t, f, n)
            f.fame += 2 * n
            f.log("lost", n.hl, "units in", a, "to the", EventCard("sailor-ghosts"), "and gained", (2 * n).hl, "fame")
            Then(next(step, l, then))

        case KrakenAttackAction(f, a, step, l, then) =>
            game.removeFigures(game.board.territory(a), f, 1)
            val wood = math.min(1, f.wood)
            f.wood -= wood
            f.log("lost a unit in", a, (wood > 0).?("and discarded " ~ 1.hl ~ " " ~ Wood.elem).|(Empty))
            Then(next(step, l, then))

        case EarthquakeAction(f, Nil, step, l, then) =>
            Then(next(step, l, then))

        case EarthquakeAction(f, a :: rest, step, l, then) =>
            Ask(f).each(game.buildingsIn(game.board.territory(a)))((s, b) => EarthquakeRemoveAction(f, s, b, rest, step, l, then))

        case EarthquakeRemoveAction(f, s, b, rest, step, l, then) =>
            game.buildings -= s
            f.log("lost", b, "in", s.area, "to the", EventCard("earthquake"))
            Then(EarthquakeAction(f, rest, step, l, then))

        // Ceremonial Bonfire: 2 wood for 1 lore, and 1 fame per trade
        case TradeAction(f, then) if game.eventIs("ceremonial-bonfire") =>
            val pp = CommonExpansion.payments(f)
            val swaps = game.mates(f)./~(g => Resource.all.%(f.has(_) > 0)./~(r => Resource.all.%(_ != r).%(g.has(_) > 0)./(x => TeamTradeAction(f, g, r, x, then))))

            if (pp.none && swaps.none && f.wood < 2)
                Then(then)
            else
                Ask(f)
                    .some(pp)(p => Resource.all./(r => TradeForAction(f, p, r, then)))
                    .when(f.wood >= 2)(BonfireTradeAction(f, then))
                    .add(swaps)
                    .done(then)

        case BonfireTradeAction(f, then) =>
            f.wood -= 2
            f.lore += 1
            f.fame += 1
            f.log("traded", Wood, Wood, "for", Lore, "and gained", 1.hl, "fame")
            game.advance(f, "trading")
            Then(TradeAction(f, then))

        case TradeForAction(f, _, _, _) =>
            if (game.eventIs("ceremonial-bonfire")) {
                f.fame += 1
                f.log("gained", 1.hl, "fame with", EventCard("ceremonial-bonfire"))
            }
            UnknownContinue

        case TeamTradeAction(f, _, _, _, _) =>
            if (game.eventIs("ceremonial-bonfire")) {
                f.fame += 1
                f.log("gained", 1.hl, "fame with", EventCard("ceremonial-bonfire"))
            }
            UnknownContinue

        case _ => UnknownContinue
    }
}


// ALTERNATIVE VICTORY CONDITIONS

// mapControl: a Map Control card (otherwise Wealth); counter: the validation count it needs (None: checked on the map)
case class VictoryCard(id : String) extends Card {
    def info = VictoryCard.info(id)
    override def elem = name.styled(styles.fame)
    def mapControl = VictoryCard.mapControl.has(id)
    def target = VictoryCard.targets.get(id)
}

object VictoryCard {
    private def card(id : String, name : String, text : String) = id -> CardInfo(name, "card-victory-" + id, 0, false, MapEffect, text)

    val maps = $(
        card("many-territories", "Many Territories", "Control at least 6 closed territories of any size."),
        card("large-buildings", "Large Buildings", "Have at least 4 large buildings across all territories you control."),
        card("spreading", "Spreading", "Control at least 8 closed or open territories of any size."),
        card("creature-territories", "Creature Territories", "Control at least 3 closed territories with at least 1 Creature lair in each."),
        card("vast-territory", "Vast Territory", "Control one closed territory of six or more tiles, containing at least one large building in it."),
        card("large-territories", "Large Territories", "Have at least one large building in each of 3 closed territories you control."),
        card("two-larger-territories", "Two Larger Territories", "Control at least 2 closed territories made of 5 or more tiles, containing at least 1 large building in each."),
        card("mountains", "Mountains", "Control at least 6 closed territories, each bordered by at least one rough or impassable border."),
    )

    val wealth = $(
        card("exploration", "Exploration", "Close at least 6 territories you control."),
        card("architecture", "Architecture", "Build at least 5 buildings of any size."),
        card("conquest", "Conquest", "Win at least 5 combats against other players."),
        card("hunting", "Hunting", "Defeat at least 3 creatures (Creatures module)."),
        card("knowledge", "Knowledge", "Have 3 Upgrades in play. (If you play without Warchiefs, have 2 Upgrades AND at least 3 lore in your reserve.)"),
        card("prosperity", "Prosperity", "Have at least 50 fame (as Fame tokens only)."),
        card("population", "Population", "Have maximum 1 unit left in your reserve AND have no Unrest cards remaining in your deck."),
        card("valhalla", "Valhalla", "Lose at least 6 units in combat versus players and/or creatures."),
        card("development", "Development", "Have at least 6 fame from Development cards only (without Achievement cards)."),
        card("production", "Production", "Have at least 5 of each resource type in their reserve (food, wood, lore)."),
        card("building-ownership", "Building Ownership", "Control at least 9 buildings in your territories."),
        card("trading", "Trading", "Trade resources at least 6 times during the Harvest phase."),
        card("refinement", "Refinement", "Improve or remove at least 4 cards from your deck."),
    )

    val info : Map[String, CardInfo] = (maps ++ wealth).toMap
    val mapControl : $[String] = maps.map(_._1)
    val mapCards : $[VictoryCard] = maps.map(x => VictoryCard(x._1))
    val wealthCards : $[VictoryCard] = wealth.map(x => VictoryCard(x._1))
    val all = mapCards ++ wealthCards

    // The validation cards and their counts (Development keeps the best total reached)
    val targets : Map[String, Int] = Map("exploration" -> 6, "architecture" -> 5, "conquest" -> 5, "hunting" -> 3, "valhalla" -> 6, "development" -> 6, "trading" -> 6, "refinement" -> 4)
}

case class ShuffledMapControlAction(shuffled : $[VictoryCard], achievements : $[Card]) extends ShuffledAction[VictoryCard]
case class ShuffledWealthAction(shuffled : $[VictoryCard], achievements : $[Card]) extends ShuffledAction[VictoryCard]
case class AltVictoryAction(winners : $[Faction]) extends ForcedAction


object VictoryExpansion extends Expansion {
    def needsCreatures(c : VictoryCard) = $("creature-territories", "hunting").has(c.id)

    def rough(t : Territory)(implicit game : Game) : Boolean = t.areas.exists { a =>
        game.board.at(a.x, a.y).exists(p => p.spec.borders.exists(b => (b.a == a.id || b.b == a.id) && (b.rough || b.impassable)))
    }

    def large(t : Territory)(implicit game : Game) = game.buildingsIn(t).exists(_._2.large)

    // Upgrade cards f owns
    def upgrades(f : Faction)(implicit game : Game) = game.states(f).deck.count {
        case ClanCard(_, n) => n > 0
        case _ => false
    }

    def developmentFame(f : Faction)(implicit game : Game) = game.states(f).deck.of[Development]./(_.fame).sum

    def fulfilled(f : Faction, c : VictoryCard)(implicit game : Game) : Boolean = {
        val mine = game.controlled(f)
        val closed = mine.%(game.board.closed)
        val s = game.states(f)

        c.target match {
            case Some(n) => game.progressOf(f, c.id) >= n
            case None => c.id match {
                case "many-territories" => closed.num >= 6
                case "large-buildings" => mine./~(game.buildingsIn).count(_._2.large) >= 4
                case "spreading" => mine.num >= 8
                case "creature-territories" => closed.count(t => t.areas.exists(a => game.board.spec(a).lair)) >= 3
                case "vast-territory" => closed.exists(t => game.board.tiles(t) >= 6 && large(t))
                case "large-territories" => closed.count(large) >= 3
                case "two-larger-territories" => closed.count(t => game.board.tiles(t) >= 5 && large(t)) >= 2
                case "mountains" => closed.count(rough) >= 6
                case "knowledge" => if (game.has(Warchiefs)) upgrades(f) >= 3 else upgrades(f) >= 2 && s.lore >= 3
                case "prosperity" => s.fame >= 50
                case "population" => game.reserve(f) <= 1 && s.unrest == 0
                case "production" => s.food >= 5 && s.wood >= 5 && s.lore >= 5
                case "building-ownership" => mine./~(game.buildingsIn).num >= 9
                case _ => false
            }
        }
    }

    // The cards a side (a team, or one player) fulfils
    def done(side : $[Faction])(implicit game : Game) : $[VictoryCard] = game.victory.%(c => side.exists(fulfilled(_, c)))

    def wins(side : $[Faction])(implicit game : Game) : Boolean = {
        val d = done(side)
        if (options.has(VictoryModeOption(true)))
            d.num == game.victory.num
        else
            d.exists(_.mapControl) && d.exists(_.mapControl.not)
    }

    def perform(action : Action, soft : Void)(implicit game : Game) = action @@ {
        // SETUP: 1 Map Control card and 2 Wealth cards (3 with teams); cards for modules not played are skipped
        case ShuffledAchievementsAction(l) if game.victory.none =>
            Shuffle[VictoryCard](VictoryCard.mapCards, ShuffledMapControlAction(_, l))

        case ShuffledMapControlAction(l, achievements) =>
            game.victory = l.%(c => needsCreatures(c).not || game.has(Creatures)).take(1)
            Shuffle[VictoryCard](VictoryCard.wealthCards, ShuffledWealthAction(_, achievements))

        case ShuffledWealthAction(l, achievements) =>
            game.victory ++= l.%(c => needsCreatures(c).not || game.has(Creatures)).take(game.teams.?(3).|(2))
            log("Victory conditions", "(" ~ options.has(VictoryModeOption(true)).?("Jarl").|("Thane").hl ~ "):", game.victory./(_.elem).join(", "))
            game.internalPerform(ShuffledAchievementsAction(achievements), soft)

        // Validation counts
        case BuildPlaceAction(f, _, _, _, _, _, _, _, _) =>
            game.advance(f, "architecture")
            UnknownContinue

        case TradeForAction(f, _, _, _) =>
            game.advance(f, "trading")
            UnknownContinue

        case TeamTradeAction(f, _, _, _, _) =>
            game.advance(f, "trading")
            UnknownContinue

        case RemoveCardAction(f, _) =>
            game.advance(f, "refinement")
            UnknownContinue

        case UpgradeCardAction(f, _, _, _) =>
            game.advance(f, "refinement")
            UnknownContinue

        // 5. END OF YEAR: a side fulfilling the mode's conditions wins
        case EndOfYearAction if game.isOver.not =>
            factions.foreach { f =>
                val d = developmentFame(f)
                if (d > game.progressOf(f, "development"))
                    game.advance(f, "development", d - game.progressOf(f, "development"))
            }

            val sides = CommonExpansion.sides(factions)
            val winners = sides.%(wins)

            if (winners.any)
                Then(AltVictoryAction(winners.flatten))
            else
                UnknownContinue

        case AltVictoryAction(l) =>
            log(DoubleLine)

            val sides = CommonExpansion.sides(l)

            sides.foreach(s => log(s./(_.elem).join(", "), "fulfilled", done(s)./(_.elem).join(", ")))

            // Ties: the most conditions fulfilled, then fame
            val best = sides./(s => done(s).num).max
            val most = sides.%(s => done(s).num == best)
            val winners = CommonExpansion.best(most, $(f => game.states(f).fame)).flatten

            game.isOver = true
            game.highlight.current = winners.single

            winners.foreach(f => f.log("won with the", "Alternative victory".hl, "conditions"))
            game.note("alt-victory-" + options.has(VictoryModeOption(true)).?("jarl").|("thane"))

            Debug.summary(game)

            GameOver(winners, "Game Over" ~ Break ~ winners./(_.elem).join(Break) ~ Break ~ "won", winners./(f => GameOverWonAction(null, f)))

        case _ => UnknownContinue
    }
}
