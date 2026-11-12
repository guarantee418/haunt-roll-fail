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


// Marzaus's "Clan Fame upgrades (PC)" (Discord, northgard-fanmade-content, 2026-10-22; asked for by Gman on 2026-11-11), option
// ModuleOption(FameUpgradesModule) under Fan content, and part of the PC version mode. Each of the seven core clans unlocks abilities
// when its fame first reaches 20, 50 or 100 (sweep, after every action, Game.loggedPerform). RULES.md (Clan Fame upgrades) lists the
// readings of the short post:
// - a clan has an ability while its fame is at its threshold or more: it loses it when its fame drops below, and gets it back
//   with the fame (Gman, 2026-11-12); fame is public in this mode (ui.scala, fameHidden)
// - the cards' fame (Development cards, Achievements, Unrest) is counted when each card is taken, and none at the end (Gman,
//   2026-11-12): Game.finalFame, taken, unrest; the resources left still give their fame at the end
// - Bear, Shield Mastery: 1 more combat point in every fight, against a clan or a creature, attacking or defending
// - Bear, Hibernation: after Winter, the food and wood the Harvest gave (as it would now: Harvest.resources) once more
// - Boar, Legacy: an Altar of Kings costs 1 wood less, and each one Boar controls gives 1 more food, wood and lore at the Harvest
// - Goat, Feasting, and Snake, Under Pressure: a turn choice once a year, beside Wait: a card from hand goes among the played cards, as
//   with Wait, and Goat harvests (no fame), or Snake moves its Scorched Earth token to an enemy territory next to one of its own and
//   collects what it gives
// - Raven, Rangers: once a year, after an Explore, one more Explore whose tile may go anywhere; and every territory Raven closes by
//   exploring gives it its resources, its own (the clan power) or not (fame only for its own, as before)
// - Raven, Wartime Economy: on its turn, as often as it likes and not as an action, 1 food for 1 unit in a territory it controls with
//   a Training Camp; harvest trades are 2 for 1
// - Snake, Guerilla Tactics (with the Warchiefs module): before a fight against a clan in Signy's territory, attacking or defending,
//   one enemy unit is killed
// - Stag, Supplies: 5 food, 3 wood and 2 lore, once, when unlocked
// - Stag, Glory of the Clan: each Food Silo gives 1 more food at the Harvest; 1 combat point for every 2 units in a fight, at most 1
//   for every 20 fame the clan has
// - Stag, Valorous Leader (with the Warchiefs module): 1 more casualty in a fight against a clan where Brand is, and Brand takes one
//   more casualty to kill (Game.chiefCost)
// - Wolf, Spoils of War: winning a fight as the attacker gives what the territory gives at the Harvest, its fame too
// - Wolf, Field Rations: Winter costs Wolf at most 1 food (the wood is still owed)
// - Boar's Taming and Goat's Shepherd aren't in yet (FameUpgrade.ready), waiting on answers about their rules

case class FameUpgrade(clan : Faction, fame : Int, name : String, text : String, ready : Boolean = true, warchief : Boolean = false)

object FameUpgrade {
    val all : $[FameUpgrade] = $(
        FameUpgrade(Bear, 20, "Shield Mastery", "1 more combat point in every fight."),
        FameUpgrade(Bear, 50, "Hibernation", "After Winter, collect the food and wood of the Harvest again."),
        FameUpgrade(Boar, 20, "Taming", "Creature abilities don't affect you; once a year your warchief can tame a creature, which then works as a warchief.", ready = false),
        FameUpgrade(Boar, 50, "Legacy", "An Altar of Kings costs 1 wood less and gives 1 more food, wood and lore at each Harvest."),
        FameUpgrade(Goat, 20, "Shepherd", "Sheep that move with your Move cards, give 1 food at the Harvest and can be killed for 2 food.", ready = false),
        FameUpgrade(Goat, 50, "Feasting", "Once a year, instead of playing a card: put a card from your hand among your played cards and harvest (no fame)."),
        FameUpgrade(Raven, 20, "Rangers", "Once a year, after an Explore, explore once more and place the tile anywhere. Every territory you close gives you its resources (fame only for your own)."),
        FameUpgrade(Raven, 50, "Wartime Economy", "On your turn, pay 1 food for 1 unit in a territory with your Training Camp, as often as you like. Harvest trades are 2 for 1."),
        FameUpgrade(Snake, 20, "Under Pressure", "Once a year, instead of playing a card: put a card from your hand among your played cards, move the Scorched Earth token to an enemy territory next to yours and collect what it gives."),
        FameUpgrade(Snake, 50, "Guerilla Tactics", "Before a fight against a clan in Signy's territory, Signy kills 1 enemy unit.", warchief = true),
        FameUpgrade(Stag, 20, "Supplies", "Collect 5 food, 3 wood and 2 lore, the first time only."),
        FameUpgrade(Stag, 50, "Glory of the Clan", "Each Food Silo gives 1 more food. 1 combat point for every 2 units in a fight, at most 1 for every 20 fame you have."),
        FameUpgrade(Stag, 100, "Valorous Leader", "Brand inflicts 1 more casualty and takes 1 more casualty to kill.", warchief = true),
        FameUpgrade(Wolf, 20, "Spoils of War", "Winning a fight as the attacker gives you what the territory gives at the Harvest, its fame too."),
        FameUpgrade(Wolf, 50, "Field Rations", "Winter costs you at most 1 food."),
    )

    def of(f : Faction) : $[FameUpgrade] = all.%(_.clan == f)
}

// The panel's line (tapped: FameUpgradesView)
case class FameUpgradesLabel(f : Faction) extends GameElementary {
    def elem(implicit game : Game) = {
        val l = game.fameUnlocked.%(_.clan == f)
        FameIcon() ~ " " ~ l.any.?(l./(_.name.hl).join(", ")).|("no fame upgrade yet".spn(xstyles.smaller85))
    }
}

case class FameUpgradesView(f : Faction)

// Goat's Feasting and Snake's Under Pressure: a turn choice, then the card from hand (and Snake's territory)
case class FeastingAction(self : Faction) extends BaseAction("Your turn")("Feasting".hl ~ " (harvest, once a year)") with Soft
case class FeastingCardAction(self : Faction, card : Card) extends BaseAction("Feasting".hl, "put a card among your played cards")(InHand(self, card)) with ViewObject[Card] with HandChoice { def obj = card }

case class PressureAction(self : Faction) extends BaseAction("Your turn")("Under Pressure".hl ~ " (Scorched Earth, once a year)") with Soft
case class PressureTargetAction(self : Faction, area : AreaRef) extends BaseAction("Under Pressure".hl, "move the", "Scorched Earth".hl, "token to")(area) with Soft with MapTarget { def target = area }
case class PressureCardAction(self : Faction, area : AreaRef, card : Card) extends BaseAction("Under Pressure".hl, "in", area, Comma, "put a card among your played cards")(InHand(self, card)) with ViewObject[Card] with HandChoice { def obj = card }

// Raven's Wartime Economy: 1 food for 1 unit, not an action
case class WartimeAction(self : Faction, area : AreaRef, stage : Int) extends BaseAction("Wartime Economy".hl, "pay", Amount(1.hl, Food.elem), "for 1 unit in")(area) with MapTarget { def target = area }

// Raven's Rangers: one more Explore, anywhere
case class RangersAction(self : Faction, then : ForcedAction) extends BaseAction("Rangers".hl, "(once a year)")("Explore once more, anywhere on the map")
case class RangersSkipAction(self : Faction, then : ForcedAction) extends BaseAction("Rangers".hl, "(once a year)")("Don't explore again")


object FameUpgradesExpansion extends Expansion {
    def on(implicit game : Game) = game.has(FameUpgradesModule)

    def has(f : Faction, name : String)(implicit game : Game) : Boolean = on && game.fameUnlocked.exists(u => u.clan == f && u.name == name)

    // Once a year: used this year already
    def used(f : Faction, name : String)(implicit game : Game) : Boolean = game.fameUsed.get(f -> name).has(game.year)

    def use(f : Faction, name : String)(implicit game : Game) { game.fameUsed += (f -> name) -> game.year }

    // The abilities held: those whose fame the clan has now; one drops when the fame falls below it, and comes back with the fame
    // (Gman, 2026-11-12). Supplies is given only the first time
    def sweep(implicit game : Game) {
        if (on && game.isOver.not) {
            val held = FameUpgrade.all.%(_.ready).%(u => game.states.contains(u.clan)).%(u => u.clan.fame >= u.fame / Debug.fameUpgradeDiv).%(u => u.warchief.not || game.has(Warchiefs))

            game.fameUnlocked.diff(held).foreach { u =>
                game.fameUnlocked = game.fameUnlocked.but(u)
                game.note("fame-upgrade-lost")
                u.clan.log("fell below", Amount(u.fame.hl, FameIcon()), "and lost", u.name.hl)
            }

            held.diff(game.fameUnlocked).foreach { u =>
                val f = u.clan
                game.fameUnlocked :+= u
                game.note("fame-upgrade")

                f.log("reached", Amount(u.fame.hl, FameIcon()), "and unlocked", u.name.hl)

                if (u.name == "Supplies" && game.fameUsed.contains(f -> "Supplies").not) {
                    use(f, "Supplies")
                    f.food += 5
                    f.wood += 3
                    f.lore += 2
                    f.log("collected", Amount(5.hl, Food.elem), Comma, Amount(3.hl, Wood.elem), "and", Amount(2.hl, Lore.elem), "with", "Supplies".hl)
                }
            }
        }
    }

    // CARDS: their fame when taken, as nothing is counted at the end (Gman, 2026-11-12); with Alternative fame gains, which gives a
    // Development card's fame when taken already, only the other cards
    def taken(f : Faction, c : Card)(implicit game : Game) {
        if (on && f != Automa && (c.is[Development] && FameGainsExpansion.on).not) {
            val n = CommonExpansion.cardFame(f, c)
            if (n != 0) {
                f.fame += n
                f.log((n > 0).?("gained").|("lost"), Amount(n.abs.hl, FameIcon()), "for", c)
            }
        }
    }

    // An Unrest card's 5 fame, lost when it is taken
    def unrest(f : Faction)(implicit game : Game) {
        if (on) {
            f.fame -= 5
            f.log("lost", Amount(5.hl, FameIcon()), "for", UnrestCard)
        }
    }

    // COMBAT: Bear's Shield Mastery and Stag's Glory of the Clan (they add up to game.strength)
    def parts(t : Territory, f : Faction, attacking : Boolean)(implicit game : Game) : $[(Int, Elem)] = on.??($(
        has(f, "Shield Mastery").??(1) -> "Shield Mastery".hl,
        has(f, "Glory of the Clan").??(math.min(game.count(t, f) / 2, math.max(0, f.fame) / 20)) -> "Glory of the Clan".hl
    ).filter(_._1 > 0))

    def strength(t : Territory, f : Faction, attacking : Boolean)(implicit game : Game) : Int = parts(t, f, attacking).map(_._1).sum

    // Stag's Valorous Leader: Brand's casualty, and the one more it takes to kill him
    def valorous(f : Faction)(implicit game : Game) : Boolean = f == Stag && has(f, "Valorous Leader")

    def casualties(f : Faction, t : Territory)(implicit game : Game) : Int = (valorous(f) && game.chiefIn(t, f)).??(1)

    // HARVEST: Boar's Legacy (1 food, wood and lore for each Altar of Kings) and Stag's Glory of the Clan (1 food for each Food Silo)
    def harvest(f : Faction)(implicit game : Game) : (Int, Int, Int) = {
        if (on.not)
            return (0, 0, 0)

        val here = game.controlled(f)./~(game.working)
        val altars = has(f, "Legacy").??(here.count(_ == AltarOfKings))
        val silos = has(f, "Glory of the Clan").??(here.count(_ == FoodSilo))

        (altars + silos, altars, altars)
    }

    // Boar's Legacy: an Altar of Kings for 1 wood less
    def discount(f : Faction, b : Building)(implicit game : Game) : Int = (b == AltarOfKings && has(f, "Legacy")).??(1)

    // Raven's Wartime Economy: harvest trades are 2 for 1
    def tradeRate(f : Faction)(implicit game : Game) : Int = has(f, "Wartime Economy").?(2).|(3)

    // Wolf's Field Rations: at most 1 food for Winter
    def winterFood(f : Faction, food : Int)(implicit game : Game) : Int = has(f, "Field Rations").?(math.min(food, 1)).|(food)

    // Wolf's Spoils of War: the territory won gives what it gives at the Harvest, fame too
    def attackWon(f : Faction, a : AreaRef)(implicit game : Game) {
        if (has(f, "Spoils of War")) {
            val t = game.board.territory(a)
            MapExpansion.collect(f, t, "with " ~ "Spoils of War".hl)
            val fame = game.board.closed(t).??((game.board.tiles(t) >= 3).?(2).|(1)) + 3 * game.working(t).count(_ == AltarOfKings)
            if (fame > 0) {
                f.fame += fame
                f.log("gained", Amount(fame.hl, FameIcon()), "with", "Spoils of War".hl)
            }
            game.note("fame-spoils")
        }
    }

    // Raven's Rangers: the territories it closed that aren't its own give their resources too
    def closed(f : Faction, l : $[Territory])(implicit game : Game) {
        if (has(f, "Rangers"))
            l.foreach { t =>
                MapExpansion.collect(f, t, "from the closed territory with " ~ "Rangers".hl)
                game.note("fame-rangers-close")
            }
    }

    // The turn's extra choices: Feasting and Under Pressure at the start (they are the turn), Wartime Economy at any point
    def actions(f : Faction, stage : Int)(implicit game : Game) : $[UserAction] = on.??(
        (stage == 0 && f.hand.any && has(f, "Feasting") && used(f, "Feasting").not).$(FeastingAction(f)) ++
        (stage == 0 && f.hand.any && has(f, "Under Pressure") && used(f, "Under Pressure").not && pressureTargets(f).any).$(PressureAction(f)) ++
        (has(f, "Wartime Economy") && f.food >= 1 && game.reserve(f) > 0).??(game.controlled(f).%(t => game.working(t).has(TrainingCamp))./(t => WartimeAction(f, t.anchor, stage)))
    )

    // Under Pressure: enemy territories next to one of Snake's, other than the one the token is on
    def pressureTargets(f : Faction)(implicit game : Game) : $[Territory] = {
        val own = game.controlled(f)
        game.board.territories.%(t => game.present(t).single.exists(g => game.enemy(f, g))).%(t => own.exists(o => game.board.adjacent(o).exists(_._1 == t))).%(t => game.scorchedIn(t).not)
    }

    // Raven's Rangers, once a year after an Explore
    def rangers(f : Faction)(implicit game : Game) : Boolean = has(f, "Rangers") && used(f, "Rangers").not && game.pile.any && MapExpansion.explorable(f, true).any

    // Snake's Guerilla Tactics: Signy kills before this fight
    def guerilla(f : Faction, a : AreaRef)(implicit game : Game) : |[Faction] = {
        val t = game.board.territory(a)
        val defender = game.present(t).but(f).headOption
        defender.flatMap { d =>
            val snake = $(f, d).%(g => g == Snake && has(g, "Guerilla Tactics") && game.chiefIn(t, g)).headOption
            snake.map(s => (s == f).?(d).|(f)).filter(e => game.count(t, e) > 0)
        }
    }

    def perform(action : Action, soft : Void)(implicit game : Game) = action @@ {
        // GOAT: Feasting
        case FeastingAction(f) =>
            Ask(f).each(f.hand.distinct)(c => FeastingCardAction(f, c)).cancel

        case FeastingCardAction(f, c) =>
            f.hand = f.hand.diff($(c))
            f.active :+= c
            use(f, "Feasting")
            game.note("fame-feasting")

            val (food, wood, lore) = Harvest.resources(f)
            f.food += food
            f.wood += wood
            f.lore += lore
            f.log("used", "Feasting".hl, "and collected", Amount(food.hl, Food.elem), Comma, Amount(wood.hl, Wood.elem), "and", Amount(lore.hl, Lore.elem))

            Then(NextTurnAction(f))

        // SNAKE: Under Pressure
        case PressureAction(f) =>
            Ask(f).each(pressureTargets(f))(t => PressureTargetAction(f, t.anchor)).cancel

        case PressureTargetAction(f, a) =>
            Ask(f).each(f.hand.distinct)(c => PressureCardAction(f, a, c)).cancel

        case PressureCardAction(f, a, c) =>
            f.hand = f.hand.diff($(c))
            f.active :+= c
            use(f, "Under Pressure")
            game.note("fame-pressure")

            game.scorched = |(a)
            f.log("used", "Under Pressure".hl, "and moved the", "Scorched Earth".hl, "token to", a)

            MapExpansion.collect(f, game.board.territory(a), "from it")

            Then(NextTurnAction(f))

        // RAVEN: Wartime Economy
        case WartimeAction(f, a, stage) =>
            f.food -= 1
            game.addUnits(a, f, 1)
            game.note("fame-wartime")

            f.log("paid", Amount(1.hl, Food.elem), "for a unit in", a, "with", "Wartime Economy".hl)

            Then(TurnAction(f, stage))

        // RAVEN: Rangers, once the Explore's tiles are placed
        case ExploreAction(f, _, times, _, _, then) if times <= 0 && rangers(f) =>
            Ask(f).add(RangersAction(f, then)).add(RangersSkipAction(f, then))

        case RangersAction(f, then) =>
            use(f, "Rangers")
            game.note("fame-rangers")

            f.log("explored once more with", "Rangers".hl)

            Then(ExploreAction(f, 1, 1, false, ExploreEffect(anywhere = true), then))

        case RangersSkipAction(f, then) =>
            use(f, "Rangers")

            Then(then)

        // SNAKE: Guerilla Tactics, once before the fight (and before the Defense Towers of the Tower PC version)
        case FightStartAction(f, a, e, then) if game.guerillaAt.has((a, game.fights)).not && guerilla(f, a).any =>
            val t = game.board.territory(a)
            val victim = guerilla(f, a).get

            game.guerillaAt = |((a, game.fights))
            game.removeUnits(t, victim, 1)
            game.note("fame-guerilla")

            Snake.log("killed a unit of", victim, "in", a, "with", WarchiefElem(Snake), "before the fight", "(" ~ "Guerilla Tactics".hl ~ ")")

            // The attacker lost its last figure: the fight is over before it starts
            if (victim == f && game.figures(t, f) == 0) {
                game.fights += 1
                Snake.log("won the fight in", a, "with", "Guerilla Tactics".hl)
                Then(then)
            }
            else
                Then(FightStartAction(f, a, e, then))

        // BEAR: Hibernation, after Winter
        case EndOfYearAction if game.harvested && has(Bear, "Hibernation") && used(Bear, "Hibernation").not =>
            use(Bear, "Hibernation")
            game.note("fame-hibernation")

            val (food, wood, _) = Harvest.resources(Bear)
            Bear.food += food
            Bear.wood += wood
            Bear.log("collected", Amount(food.hl, Food.elem), "and", Amount(wood.hl, Wood.elem), "again with", "Hibernation".hl)

            Then(EndOfYearAction)

        case _ => UnknownContinue
    }
}
