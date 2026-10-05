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


// Uncharted Horizons' Sea module: a Beach tile with a Port for each player, the Raid deck and the Raid phase.
// Rules from the Uncharted Horizons rulebook (the PDF in the Tabletop Simulator mod 3597126237); the 24 Raid cards
// from the same mod. RULES.md has the summary and the choices made where the rulebook is unclear.

// What a Raid gives instead of its action: per unit for a 1-year Raid, once for a 2-year Raid; any: resources of the player's choice
case class RaidGain(food : Int = 0, wood : Int = 0, lore : Int = 0, fame : Int = 0, any : Int = 0) {
    def times(n : Int) = RaidGain(food * n, wood * n, lore * n, fame * n, any * n)
    def elem : Elem = $(food -> Food.elem, wood -> Wood.elem, lore -> Lore.elem, fame -> FameIcon(), any -> "any resource".txt).filter(_._1 > 0)./{ case (n, e) => n.hl ~ " " ~ e }.join(", ")
    def text : String = $(food -> "food", wood -> "wood", lore -> "lore", fame -> "fame", any -> "any resource").filter(_._1 > 0)./{ case (n, e) => n.toString + " " + e }.mkString(", ")
}

// one, two: the actions of a 1-year and a 2-year Raid (None: the 2-year Raid only gives resources)
case class RaidInfo(name : String, gain1 : RaidGain, one : String, gain2 : RaidGain, two : |[String])

case class RaidCard(id : String) extends Card {
    def raid = RaidCard.raids(id)
    def info = CardInfo(raid.name, "card-raid-" + id, 0, false, MapEffect,
        "1-year Raid: " + "per unit, " + raid.gain1.text + " OR " + raid.one + ". 2-year Raid (2 units): " + raid.gain2.text + raid.two./(" OR " + _).|("") + ".")
    override def elem = name.styled(styles.fame)
    def gain(years : Int, n : Int) = (years == 1).?(raid.gain1.times(n)).|(raid.gain2)
    def action(years : Int) : |[String] = (years == 1).?(|(raid.one)).|(raid.two)
    // The action happens at the next Harvest or the next Start of Year: the card is kept until then
    def later(years : Int) = RaidCard.harvest.has(id) || (id == "elders-wisdom" && years == 1)
}

object RaidCard {
    private def raid(id : String, name : String, gain1 : RaidGain, one : String, gain2 : RaidGain, two : String = null) = id -> RaidInfo(name, gain1, one, gain2, Option(two))

    val list : $[(String, RaidInfo)] = $(
        raid("calm-the-storm", "Calm the Storm", RaidGain(wood = 1), "Remove 1 card other than Unrest from your discard pile", RaidGain(wood = 4), "Remove 1 Unrest card from your draw or discard pile, then shuffle both into a new draw pile"),
        raid("raiders-reward", "Raider's Reward", RaidGain(fame = 1), "At the next Harvest, take 1 resource from an enemy territory next to yours instead of its owner", RaidGain(fame = 4), "At the next Harvest, take all the resources and fame of one enemy territory"),
        raid("outpost-establishment", "Outpost Establishment", RaidGain(any = 1), "Place 1 unit and a free small building in a neutral territory", RaidGain(food = 1, wood = 1, lore = 1, fame = 1), "Place 1 unit in a neutral territory and fill its spaces with free small buildings of different types"),
        raid("uncharted-journey", "Uncharted Journey", RaidGain(fame = 1), "Explore once from an open neutral territory or one you control", RaidGain(lore = 1, fame = 4), "Explore 3 times from open neutral territories or ones you control"),
        raid("bold-maneuver", "Bold Maneuver", RaidGain(wood = 1), "Move 1 ignoring Rough borders, gaining 2 fame per combat won", RaidGain(wood = 3, fame = 3), "Move 3 ignoring Rough borders, gaining 2 fame per combat won"),
        raid("clear-the-frontlines", "Clear the Frontlines", RaidGain(fame = 1), "Remove 1 enemy unit from an open territory", RaidGain(fame = 4), "Remove 2 enemy units from open territories"),
        raid("trade-and-triumph", "Trade and Triumph", RaidGain(food = 1), "At the next Harvest, exchange 1 resource for another", RaidGain(food = 2, wood = 1, fame = 2), "At the next Harvest, exchange resources 1 for 1, gaining 1 fame for each"),
        raid("conquerors-tribute", "Conqueror's Tribute", RaidGain(any = 1), "At the next Harvest, gain 1 fame per open enemy territory", RaidGain(food = 1, wood = 1, lore = 1, fame = 1), "At the next Harvest, gain 2 fame per open enemy territory (10 at most)"),
        raid("frontline-reinforcements", "Frontline Reinforcements", RaidGain(food = 1), "Add 1 unit to a territory you control", RaidGain(food = 2, fame = 2), "Add 1 unit to each of up to 5 open territories you control"),
        raid("elders-wisdom", "Elder's Wisdom", RaidGain(lore = 1), "Draw 1 more card at the next Start of Year", RaidGain(lore = 2, fame = 3), "Remove 1 card from your discard pile and put 1 clan upgrade card on top of your draw pile for free"),
        raid("infiltrate-and-conquer", "Infiltrate and Conquer", RaidGain(wood = 1), "Move 1 of your units to a neutral territory", RaidGain(wood = 2, fame = 2), "Move any number of units from one of your territories to a neutral territory"),
        raid("renewal-ritual", "Renewal Ritual", RaidGain(lore = 1), "Remove 1 card other than Unrest from your discard pile", RaidGain(lore = 2, fame = 2), "Remove 2 cards other than Unrest from your draw or discard pile, then shuffle both into a new draw pile"),
        raid("heroic-homestead", "Heroic Homestead", RaidGain(wood = 1), "Place 1 small building for free in a territory you control", RaidGain(wood = 2, fame = 2), "Place 1 large building for free in a territory you control"),
        raid("predators-pride", "Predator's Pride", RaidGain(fame = 1), "Gain 1 fame per creature lair in your territories", RaidGain(fame = 4), "Gain 2 fame per creature lair in your territories"),
        raid("raze-and-conquer", "Raze and Conquer", RaidGain(lore = 1), "Destroy 1 enemy small building in an open territory", RaidGain(lore = 2, fame = 2), "Destroy 1 enemy building, small or large, anywhere"),
        raid("warlords-tribute", "Warlord's Tribute", RaidGain(any = 1), "At the next Harvest, take 1 resource from one open enemy territory", RaidGain(food = 1, wood = 1, lore = 1, fame = 1), "At the next Harvest, take 1 resource from each open enemy territory"),
        raid("glory-of-the-homeland", "Glory of the Homeland", RaidGain(food = 1), "Gain 1 fame per territory of 1 or 2 tiles you control", RaidGain(food = 2, fame = 3), "Gain fame for your territories as at Harvest, then move any of your units between your territories"),
        raid("brotherhood-of-arms", "Brotherhood of Arms", RaidGain(fame = 1), "Place 1 unit in a neutral territory", RaidGain(fame = 4), "Place 3 units in up to 3 neutral territories"),
        raid("fame-for-fortune", "Fame for Fortune", RaidGain(food = 1), "At the next Harvest, exchange 1 resource for another", RaidGain(food = 4), "At the next Harvest, turn resources into fame, 1 for 1"),
        raid("sacred-stones", "Sacred Stones", RaidGain(lore = 1), "Place a Carved Stone on a small building space in a territory you control", RaidGain(lore = 3), "Place 2 Carved Stones in 2 different territories you control, needing no space"),
        raid("woodland-bounty", "Woodland Bounty", RaidGain(wood = 2), "Build a Woodcutter Lodge for free", RaidGain(wood = 7)),
        raid("ancient-wisdom", "Ancient Wisdom", RaidGain(lore = 2), "Build a Carved Stone for free", RaidGain(lore = 5)),
        raid("valors-reward", "Valor's Reward", RaidGain(fame = 2), "Gain 2 fame per large building you have", RaidGain(fame = 6)),
        raid("bounty-of-the-harvest", "Bounty of the Harvest", RaidGain(food = 2), "Build a Food Silo for free", RaidGain(food = 7)),
    )

    val raids : Map[String, RaidInfo] = list.toMap
    val all : $[RaidCard] = list.map(x => RaidCard(x._1))

    // Acting at the next Harvest
    val harvest = $("raiders-reward", "trade-and-triumph", "conquerors-tribute", "warlords-tribute", "fame-for-fortune")
}

// A Port's Raid: its card, and the units of its owner on the Raid slots (years: 1 on the left slots, 2 on the right)
case class Raid(card : |[RaidCard], owner : |[Faction], units : Int, years : Int)

// A completed Raid whose action waits for the next Harvest or Start of Year
case class RaidKept(f : Faction, card : RaidCard, years : Int) extends Record

// Bold Maneuver: Move ignoring Rough borders, 2 fame per combat won
case object BoldMove extends MoveSpecial

case class RaidGainLabel(card : RaidCard, years : Int, n : Int) extends Elementary {
    def elem = "Collect " ~ card.gain(years, n).elem
}

case class RaidActionLabel(card : RaidCard, years : Int) extends Elementary {
    def elem = card.action(years).|("").txt
}

case class RaidQuestion(port : AreaRef, card : RaidCard, years : Int, n : Int) extends GameElementary {
    def elem(implicit game : Game) = "Complete the " ~ (years == 1).?("1-year").|("2-year") ~ " Raid " ~ card.elem ~ " from the Port in " ~ port.elem ~ " with " ~ n.hl ~ (n == 1).?(" unit").|(" units")
}

case class ShuffledRaidsAction(shuffled : $[RaidCard], achievements : $[Card]) extends ShuffledAction[RaidCard]
case class RaidPortsAction(l : $[AreaRef]) extends ForcedAction
case class RaidCompleteAction(self : Faction, port : AreaRef, card : RaidCard, years : Int, n : Int, act : Boolean, rest : $[AreaRef]) extends BaseAction(RaidQuestion(port, card, years, n))(act.?(RaidActionLabel(card, years) : Elementary).|(RaidGainLabel(card, years, n)))
case class RaidContinueAction(self : Faction, port : AreaRef, card : RaidCard, rest : $[AreaRef]) extends BaseAction(RaidQuestion(port, card, 1, 2))("Continue for a second year")
case class RaidStartAction(f : Faction, port : AreaRef, rest : $[AreaRef]) extends ForcedAction
case class RaidDrawAction(self : Faction, port : AreaRef, rest : $[AreaRef]) extends BaseAction("Raid from the Port in", port)("Draw 2 Raid cards")
case class RaidPickAction(self : Faction, port : AreaRef, card : RaidCard, others : $[RaidCard], rest : $[AreaRef]) extends BaseAction("Raid from the Port in", port, Comma, "choose a Raid card")(card.handImg) with ViewObject[Card] { def obj = card }
case class RaidSendAskAction(f : Faction, port : AreaRef, rest : $[AreaRef]) extends ForcedAction
case class RaidSendAction(self : Faction, port : AreaRef, card : RaidCard, n : Int, rest : $[AreaRef]) extends BaseAction("Raid", card, "from the Port in", port)("Send", n.hl, (n == 1).?("unit").|("units"))
case class RaidAnyAction(f : Faction, left : Int, then : ForcedAction) extends ForcedAction
case class RaidAnyPickAction(self : Faction, r : Resource, left : Int, then : ForcedAction) extends BaseAction("Raid", "collect any resource", "(" ~ left.hl ~ " left)")(r)
case class RaidEffectAction(f : Faction, card : RaidCard, years : Int, then : ForcedAction) extends ForcedAction

// Card actions
case class RaidRemoveAction(f : Faction, card : RaidCard, n : Int, unrest : Boolean, draw : Boolean, shuffle : Boolean, then : ForcedAction) extends ForcedAction
case class RaidRemoveCardAction(self : Faction, card : RaidCard, c : Card, n : Int, unrest : Boolean, draw : Boolean, shuffle : Boolean, then : ForcedAction) extends BaseAction(card, "remove from the game", (n > 1).?("(" ~ n.hl ~ " left)").|(Empty))(c.handImg) with ViewObject[Card] { def obj = c }
case class RaidRemoveDoneAction(self : Faction, card : RaidCard, shuffle : Boolean, then : ForcedAction) extends BaseAction(card)("Remove nothing more")
case class RaidShuffledAction(f : Faction, shuffled : $[Card], then : ForcedAction) extends ShuffledAction[Card]
case class RaidUpgradeAskAction(f : Faction, card : RaidCard, then : ForcedAction) extends ForcedAction
case class RaidUpgradeAction(self : Faction, card : RaidCard, upgrade : Card, then : ForcedAction) extends BaseAction(card, "put a clan upgrade card on top of the draw pile")(upgrade.handImg) with ViewObject[Card] { def obj = upgrade }
case class RaidOutpostAction(self : Faction, card : RaidCard, area : AreaRef, years : Int, then : ForcedAction) extends BaseAction(card, "place 1 unit in")(area) with MapTarget { def target = area }
case class RaidBuildAction(f : Faction, card : RaidCard, only : $[Building], small : Boolean, area : |[AreaRef], left : Int, then : ForcedAction) extends ForcedAction
case class RaidBuildPlaceAction(self : Faction, card : RaidCard, space : SpaceRef, building : Building, only : $[Building], small : Boolean, area : |[AreaRef], left : Int, then : ForcedAction) extends BaseAction(card, "build for free in", space.area)(building) with MapTarget { def target = space }
case class RaidStonesAction(f : Faction, card : RaidCard, left : Int, then : ForcedAction) extends ForcedAction
case class RaidStoneAction(self : Faction, card : RaidCard, area : AreaRef, left : Int, then : ForcedAction) extends BaseAction(card, "place a", CarvedStone, "in")(area) with MapTarget { def target = area }
case class RaidClearAction(f : Faction, card : RaidCard, left : Int, then : ForcedAction) extends ForcedAction
case class RaidClearUnitAction(self : Faction, card : RaidCard, area : AreaRef, enemy : Faction, left : Int, then : ForcedAction) extends BaseAction(card, "remove an enemy unit from")(area, "of", enemy) with MapTarget { def target = area }
case class RaidReinforceAction(f : Faction, card : RaidCard, left : Int, placed : $[AreaRef], then : ForcedAction) extends ForcedAction
case class RaidReinforceToAction(self : Faction, card : RaidCard, area : AreaRef, left : Int, placed : $[AreaRef], then : ForcedAction) extends BaseAction(card, "add a unit to")(area) with MapTarget { def target = area }
// Infiltrate and Conquer (to a neutral territory) and Glory of the Homeland (between f's territories)
case class RaidShiftAction(f : Faction, card : RaidCard, neutral : Boolean, all : Boolean, then : ForcedAction) extends ForcedAction
case class RaidShiftFromAction(self : Faction, card : RaidCard, from : AreaRef, neutral : Boolean, all : Boolean, then : ForcedAction) extends BaseAction(card, "move units from")(from) with Soft with MapTarget { def target = from }
case class RaidShiftToAction(self : Faction, card : RaidCard, from : AreaRef, to : AreaRef, neutral : Boolean, all : Boolean, then : ForcedAction) extends BaseAction(card, "move units from", from, "to")(to) with Soft with MapTarget { def target = to }
case class RaidShiftMoveAction(self : Faction, card : RaidCard, from : AreaRef, to : AreaRef, n : Int, neutral : Boolean, all : Boolean, then : ForcedAction) extends BaseAction(card, "move from", from, "to", to)(n.hl, (n == 1).?("unit").|("units"))
case class RaidRazeAction(self : Faction, card : RaidCard, space : SpaceRef, building : Building, then : ForcedAction) extends BaseAction(card, "destroy an enemy building in", space.area)(building) with MapTarget { def target = space.area }
case class RaidExploredAction(then : ForcedAction) extends ForcedAction

// At the next Harvest and Start of Year
case class RaidHarvestAction(l : $[RaidKept], then : ForcedAction) extends ForcedAction
case class RaidKeptDoneAction(k : RaidKept, l : $[RaidKept], then : ForcedAction) extends ForcedAction
case class RaidStealAction(self : Faction, card : RaidCard, area : AreaRef, r : |[Resource], k : RaidKept, l : $[RaidKept], then : ForcedAction) extends BaseAction(card, r./(_ => "take 1 resource from").|("take all the resources and fame of"))(area, r./(r => "(" ~ r.elem ~ ")").|(Empty)) with MapTarget { def target = area }
case class RaidExchangeAction(self : Faction, card : RaidCard, give : Resource, take : Resource, fame : Boolean, given : $[Resource], taken : $[Resource], k : RaidKept, l : $[RaidKept], then : ForcedAction) extends BaseAction(card, "exchange a resource", fame.?("for 1 fame each").|(""))("Pay", give, "for", take)
case class RaidTriumphAction(f : Faction, card : RaidCard, given : $[Resource], taken : $[Resource], k : RaidKept, l : $[RaidKept], then : ForcedAction) extends ForcedAction
case class RaidFortuneAction(self : Faction, card : RaidCard, r : Resource, n : Int, k : RaidKept, l : $[RaidKept], then : ForcedAction) extends BaseAction(card, "turn resources into fame")("Pay", n.hl, r, "for", n.hl, FameIcon())
case class RaidSkipKeptAction(self : Faction, card : RaidCard, k : RaidKept, l : $[RaidKept], then : ForcedAction) extends BaseAction(card)("Done")


object SeaExpansion extends Expansion {
    def port(t : Territory)(implicit game : Game) : |[AreaRef] = game.ports.find(t.areas.contains)

    def portIn(t : Territory)(implicit game : Game) : Boolean = port(t).any

    // Units away on Raids still count for Winter, but aren't in the reserve
    def raiders(f : Faction)(implicit game : Game) : Int = game.raids.values.$.%(_.owner.has(f))./(_.units).sum

    // The Port's defender: +1 combat point
    def defense(t : Territory, f : Faction, attacking : Boolean)(implicit game : Game) : Int = (attacking.not && portIn(t)).??(1)

    // Port to Port with a Move of 2 or more, using up the moves left
    def sailing(f : Faction, t : Territory, left : Int)(implicit game : Game) : $[(Territory, Int)] =
        (left >= 2 && portIn(t)).??(game.ports./(game.board.territory).distinct.%(_ != t)./(o => o -> left))

    // Who acts for a Port in the Raid phase: the owner of a Raid under way while no other player is there, else its controller
    def actor(p : AreaRef)(implicit game : Game) : |[Faction] = {
        val t = game.board.territory(p)
        val r = game.raids(p)
        val present = game.present(t)
        r.owner.%(o => r.units > 0 && present.forall(_ == o)).orElse(present.single)
    }

    def neutral(implicit game : Game) = game.board.territories.%(t => game.present(t).none).%(t => game.hostileIn(t).not && game.bearIn(t).not && game.swampIn(t).not)

    def enemyTerritories(f : Faction)(implicit game : Game) = game.board.territories.%(t => game.present(t).single.exists(g => game.enemy(f, g)))

    def produced(t : Territory)(implicit game : Game) : $[Resource] = {
        val (food, wood, lore) = game.harvest(t)
        $[(Resource, Int)](Food -> food, Wood -> wood, Lore -> lore).filter(_._2 > 0).map(_._1)
    }

    def gain(f : Faction, g : RaidGain)(implicit game : Game) {
        f.food += g.food
        f.wood += g.wood
        f.lore += g.lore
        f.fame += g.fame
    }

    // Free spaces in t a raid building fits: Carved Stones also on small spaces when `small`
    def spaces(f : Faction, t : Territory, b : Building, small : Boolean)(implicit game : Game) : $[SpaceRef] = {
        val kinds : $[SpaceKind] = b match {
            case CarvedStone => small.?($[SpaceKind](SmallSpace, CarvedSpace)).|($[SpaceKind](CarvedSpace))
            case b if b.large => $(LargeSpace)
            case _ => $(SmallSpace, CarvedSpace)
        }
        val ok = game.buildingsIn(t).exists(_._2 == b).not && game.buildings.values.count(_ == b) < Building.tokens && game.bearIn(t).not
        ok.??(t.areas./~(a => game.board.spec(a).spaces.indices./(i => SpaceRef(a, i))).%(s => game.buildings.contains(s).not && game.gear.contains(s).not).%(s => kinds.has(MapExpansion.spaceKind(s))))
    }

    def toBottom(c : RaidCard)(implicit game : Game) {
        game.raidDeck :+= c
    }

    def perform(action : Action, soft : Void)(implicit game : Game) = action @@ {
        // SETUP: the Raid deck
        case ShuffledAchievementsAction(l) if game.raidsShuffled.not =>
            game.raidsShuffled = true
            Shuffle[RaidCard](RaidCard.all, ShuffledRaidsAction(_, l))

        case ShuffledRaidsAction(l, achievements) =>
            game.raidDeck = l
            game.internalPerform(ShuffledAchievementsAction(achievements), soft)

        // After placing their first tile, each player leaves an empty space and places a Beach tile, its Port facing the map
        case TilePlacedAction(f, tile, spot, true, SetupUnitsAskAction(_, 1, _, _, _)) if game.beached.has(f).not && game.ports.num < 5 =>
            game.beached :+= f

            val centre = $((0, 0)) ++ (factions.num >= 5).$((1, 0))
            Side.all.find(s => centre.has((spot.x + s.dx, spot.y + s.dy)))./(_.opposite).foreach { d =>
                val r = d match {
                    case South => 0
                    case West => 1
                    case North => 2
                    case East => 3
                }
                val ws = West.rotate(r)
                val es = East.rotate(r)

                def free(x : Int, y : Int) = game.board.empty(x, y)

                2.to(4).map(k => (spot.x + k * d.dx, spot.y + k * d.dy)).find { case (px, py) =>
                    val (sx, sy) = (px + d.dx, py + d.dy)
                    free(px, py) && free(sx, sy) && free(sx + ws.dx, sy + ws.dy) && free(sx + es.dx, sy + es.dy) &&
                    free(px + ws.dx, py + ws.dy) && free(px + es.dx, py + es.dy)
                }.foreach { case (px, py) =>
                    val (sx, sy) = (px + d.dx, py + d.dy)
                    game.board.place(Placement("beach-port", px, py, r))
                    game.board.place(Placement("beach-sea", sx, sy, r))
                    game.board.place(Placement("beach-wing-w", sx + ws.dx, sy + ws.dy, r))
                    game.board.place(Placement("beach-wing-e", sx + es.dx, sy + es.dy, r))

                    val p = AreaRef(px, py, "p")
                    game.board.join(AreaRef(sx + ws.dx, sy + ws.dy, "p"), p)
                    game.board.join(AreaRef(sx + es.dx, sy + es.dy, "p"), p)

                    game.ports :+= p
                    game.raids += p -> Raid(None, None, 0, 0)

                    f.log("placed a Beach tile with a Port in", p)
                }
            }

            UnknownContinue

        // 1. START OF THE YEAR: Elder's Wisdom draws one more card
        case RevealDevelopmentsAction if game.raidKept.exists(k => k.card.id == "elders-wisdom") =>
            val l = game.raidKept.%(_.card.id == "elders-wisdom")
            game.raidKept = game.raidKept.diff(l)
            l.foreach { k =>
                toBottom(k.card)
                k.f.log("drew", 1.hl, "more card with", k.card)
            }
            Then(l.foldRight(RevealDevelopmentsAction : ForcedAction)((k, then) => DrawCardsAction(k.f, 1, then)))

        // 2.5 THE RAID PHASE, at the end of the Actions phase (before the Creature phase)
        case CreaturePhaseAction if game.raidSteps.has("raid-" + game.year).not =>
            game.raidSteps :+= "raid-" + game.year

            // A player who lost control of a Port loses the units on its Raid
            game.ports.foreach { p =>
                val r = game.raids(p)
                r.owner.foreach { o =>
                    if (r.units > 0 && game.present(game.board.territory(p)).exists(_ != o)) {
                        game.raids += p -> r.copy(owner = None, units = 0, years = 0)
                        o.log("lost the", r.units.hl, "units raiding from", p)
                    }
                }
            }

            val order = game.from(game.first)
            val l = game.ports.%(p => actor(p).exists(_ != Automa)).sortBy(p => order.indexOf(actor(p).get))

            if (l.any) {
                log(SingleLine)
                log("Raids")
            }

            Then(RaidPortsAction(l))

        case RaidPortsAction(Nil) =>
            Then(CreaturePhaseAction)

        case RaidPortsAction(p :: rest) =>
            actor(p) match {
                case Some(f) if f != Automa =>
                    val r = game.raids(p)
                    if (r.units > 0 && r.owner.has(f)) {
                        val c = r.card.get
                        Ask(f)
                            .add(RaidCompleteAction(f, p, c, r.years, r.units, false, rest))
                            .when(c.action(r.years).any)(RaidCompleteAction(f, p, c, r.years, r.units, true, rest))
                            .when(r.years == 1 && r.units == 2)(RaidContinueAction(f, p, c, rest))
                    }
                    else
                        Then(RaidStartAction(f, p, rest))
                case _ =>
                    Then(RaidPortsAction(rest))
            }

        case RaidContinueAction(f, p, c, rest) =>
            game.raids += p -> game.raids(p).copy(years = 2)
            f.log("continued the Raid", c, "for a second year")
            Then(RaidPortsAction(rest))

        case RaidCompleteAction(f, p, c, years, n, act, rest) =>
            // The units come back to the Port; after two years, one of them to the reserve
            game.raids += p -> Raid(None, None, 0, 0)
            game.addUnits(p, f, (years == 1).?(n).|(1))
            game.note("raid-" + years)

            val then = RaidStartAction(f, p, rest)

            if (act) {
                f.log("completed the Raid", c, Colon, c.action(years).get.txt)
                if (c.later(years)) {
                    game.raidKept :+= RaidKept(f, c, years)
                    Then(then)
                }
                else {
                    toBottom(c)
                    Then(RaidEffectAction(f, c, years, then))
                }
            }
            else {
                toBottom(c)
                val g = c.gain(years, n)
                gain(f, g)
                f.log("completed the Raid", c, "and collected", g.elem)
                if (g.any > 0)
                    Then(RaidAnyAction(f, g.any, then))
                else
                    Then(then)
            }

        case RaidAnyAction(f, left, then) =>
            if (left <= 0)
                Then(then)
            else
                Ask(f).each(Resource.all)(r => RaidAnyPickAction(f, r, left, then))

        case RaidAnyPickAction(f, r, left, then) =>
            f.gain(r, 1)
            f.log("chose", 1.hl, r)
            Then(RaidAnyAction(f, left - 1, then))

        // A new Raid: draw two cards and keep one, or take over the card left on the Port, then send 1 or 2 units
        case RaidStartAction(f, p, rest) =>
            val r = game.raids(p)
            val here = game.count(game.board.territory(p), f)

            if (here == 0 || (r.card.none && game.raidDeck.none))
                Then(RaidPortsAction(rest))
            else
                Ask(f)
                    .when(game.raidDeck.any)(RaidDrawAction(f, p, rest))
                    .some(r.card.$)(c => 1.to(math.min(2, here)).$./(n => RaidSendAction(f, p, c, n, rest)))
                    .add(RaidPortsAction(rest).as("No Raid")("Raid from the Port in", p))

        case RaidDrawAction(f, p, rest) =>
            // A card left by the Port's last controller goes to the bottom of the deck
            game.raids(p).card.foreach(toBottom)
            game.raids += p -> Raid(None, None, 0, 0)

            val two = game.raidDeck.take(2)
            game.raidDeck = game.raidDeck.drop(2)

            f.log("drew", two.num.hl, "Raid cards")

            Ask(f).each(two)(c => RaidPickAction(f, p, c, two.diff($(c)), rest))

        case RaidPickAction(f, p, c, others, rest) =>
            others.foreach(toBottom)
            game.raids += p -> Raid(|(c), None, 0, 0)
            f.log("chose the Raid", c)
            Then(RaidSendAskAction(f, p, rest))

        case RaidSendAskAction(f, p, rest) =>
            val here = game.count(game.board.territory(p), f)
            val c = game.raids(p).card.get
            Ask(f).each(1.to(math.min(2, here)).$)(n => RaidSendAction(f, p, c, n, rest))
                .bailHard(RaidPortsAction(rest))

        case RaidSendAction(f, p, c, n, rest) =>
            game.removeUnits(game.board.territory(p), f, n)
            game.raids += p -> Raid(|(c), |(f), n, 1)
            f.log("sent", n.hl, (n == 1).?("unit").|("units"), "on the Raid", c)
            Then(RaidPortsAction(rest))

        // THE RAID CARDS' ACTIONS
        case RaidEffectAction(f, c, years, then) =>
            val two = years == 2
            c.id match {
                case "calm-the-storm" => Then(two.?(RaidRemoveAction(f, c, 1, true, true, true, then)).|(RaidRemoveAction(f, c, 1, false, false, false, then)))
                case "renewal-ritual" => Then(two.?(RaidRemoveAction(f, c, 2, false, true, true, then)).|(RaidRemoveAction(f, c, 1, false, false, false, then)))
                case "elders-wisdom" => Then(RaidRemoveAction(f, c, 1, false, false, false, RaidUpgradeAskAction(f, c, then)))
                case "outpost-establishment" =>
                    val l = (game.reserve(f) > 0).??(neutral)
                    Ask(f).each(l)(t => RaidOutpostAction(f, c, t.anchor, years, then)).bailHard(then)
                case "uncharted-journey" =>
                    game.raidExplore = true
                    MapExpansion.resolve(f, ExploreEffect(times = two.?(3).|(1)), RaidExploredAction(then))
                case "bold-maneuver" =>
                    MapExpansion.resolve(f, MoveEffect(two.?(3).|(1), 0, true, BoldMove), then)
                case "clear-the-frontlines" => Then(RaidClearAction(f, c, two.?(2).|(1), then))
                case "frontline-reinforcements" =>
                    if (two)
                        Then(RaidReinforceAction(f, c, 5, $, then))
                    else
                        MapExpansion.resolve(f, RecruitEffect(1), then)
                case "infiltrate-and-conquer" => Then(RaidShiftAction(f, c, true, two, then))
                case "heroic-homestead" => Then(RaidBuildAction(f, c, Building.all.%(_.large == two), false, None, 1, then))
                case "predators-pride" =>
                    val lairs = game.controlled(f)./~(_.areas).%(a => game.board.spec(a).lair).num
                    val n = lairs * two.?(2).|(1)
                    f.fame += n
                    f.log("gained", n.hl, FameIcon(), "for", lairs.hl, (lairs == 1).?("lair").|("lairs"))
                    Then(then)
                case "raze-and-conquer" =>
                    val l = enemyTerritories(f).%(t => two || game.board.open(t))./~(game.buildingsIn).%{ case (_, b) => two || b.large.not }
                    Ask(f).each(l)((s, b) => RaidRazeAction(f, c, s, b, then)).add(then.as("Destroy nothing")(c)).bailHard(then)
                case "glory-of-the-homeland" =>
                    if (two) {
                        val n = Harvest.territoryFame(f)
                        f.fame += n
                        f.log("gained", n.hl, FameIcon(), "for closed territories")
                        Then(RaidShiftAction(f, c, false, true, then))
                    }
                    else {
                        val n = game.controlled(f).%(t => game.board.tiles(t) <= 2).num
                        f.fame += n
                        f.log("gained", n.hl, FameIcon(), "for territories of 1 or 2 tiles")
                        Then(then)
                    }
                case "brotherhood-of-arms" => MapExpansion.resolve(f, RecruitEffect(two.?(3).|(1), RecruitNeutralOnly), then)
                case "sacred-stones" =>
                    if (two)
                        Then(RaidStonesAction(f, c, 2, then))
                    else
                        Then(RaidBuildAction(f, c, $(CarvedStone), true, None, 1, then))
                case "woodland-bounty" => Then(RaidBuildAction(f, c, $(WoodcutterLodge), false, None, 1, then))
                case "ancient-wisdom" => Then(RaidBuildAction(f, c, $(CarvedStone), true, None, 1, then))
                case "bounty-of-the-harvest" => Then(RaidBuildAction(f, c, $(FoodSilo), false, None, 1, then))
                case "valors-reward" =>
                    val large = game.controlled(f)./~(game.buildingsIn).count(_._2.large)
                    f.fame += 2 * large
                    f.log("gained", (2 * large).hl, FameIcon(), "for", large.hl, (large == 1).?("large building").|("large buildings"))
                    Then(then)
                case _ => Then(then)
            }

        case RaidExploredAction(then) =>
            game.raidExplore = false
            Then(then)

        // Removing cards from the deck
        case RaidRemoveAction(f, c, n, unrest, draw, shuffle, then) =>
            val l = (n > 0).??((f.discard ++ draw.??(f.draw)).%(x => unrest.?(x == UnrestCard).|(x != UnrestCard && x.removable)).distinct)
            if (l.none && shuffle)
                Shuffle[Card](f.draw ++ f.discard, RaidShuffledAction(f, _, then))
            else
            if (l.none)
                Then(then)
            else
                Ask(f).each(l)(x => RaidRemoveCardAction(f, c, x, n, unrest, draw, shuffle, then)).add(RaidRemoveDoneAction(f, c, shuffle, then))

        case RaidRemoveCardAction(f, c, x, n, unrest, draw, shuffle, then) =>
            if (f.discard.has(x))
                f.discard = f.discard.diff($(x))
            else
                f.draw = f.draw.diff($(x))
            f.log("removed", x, "from the game with", c)
            Then(RaidRemoveAction(f, c, n - 1, unrest, draw, shuffle, then))

        case RaidRemoveDoneAction(f, c, shuffle, then) =>
            Then(RaidRemoveAction(f, c, 0, false, false, shuffle, then))

        case RaidShuffledAction(f, l, then) =>
            f.draw = l
            f.discard = $
            f.log("shuffled their draw and discard piles into a new draw pile")
            Then(then)

        case RaidUpgradeAskAction(f, c, then) =>
            Ask(f).each(f.upgrades)(u => RaidUpgradeAction(f, c, u, then)).add(then.as("Take none")(c)).bailHard(then)

        case RaidUpgradeAction(f, c, u, then) =>
            f.upgrades = f.upgrades.diff($(u))
            f.draw = u +: f.draw
            f.log("put", u, "on top of their draw pile with", c)
            Then(then)

        // Outpost Establishment: a unit, then free small buildings in that territory
        case RaidOutpostAction(f, c, a, years, then) =>
            game.addUnits(a, f, 1)
            f.log("placed a unit in", a, "with", c)
            Then(RaidBuildAction(f, c, Building.all.%(_.large.not), false, |(a), (years == 1).?(1).|(99), then))

        case RaidBuildAction(f, c, only, small, area, left, then) =>
            val ts = area./(a => $(game.board.territory(a))).|(game.controlled(f))
            val l = (left > 0).??(ts./~(t => only./~(b => spaces(f, t, b, small)./(s => s -> b))))
            Ask(f).each(l)((s, b) => RaidBuildPlaceAction(f, c, s, b, only, small, area, left, then)).add(then.as("Build nothing more")(c)).bailHard(then)

        case RaidBuildPlaceAction(f, c, s, b, only, small, area, left, then) =>
            game.buildings += s -> b
            f.log("built", b, "in", s.area, "for free with", c)
            Then(RaidBuildAction(f, c, only, small, area, left - 1, then))

        // Sacred Stones: Carved Stones needing no space, in different territories
        case RaidStonesAction(f, c, left, then) =>
            val l = (left > 0 && game.buildings.values.count(_ == CarvedStone) < Building.tokens).??(game.controlled(f).%(t => game.buildingsIn(t).exists(_._2 == CarvedStone).not))
            Ask(f).each(l)(t => RaidStoneAction(f, c, t.anchor, left, then)).add(then.as("Place no more")(c)).bailHard(then)

        case RaidStoneAction(f, c, a, left, then) =>
            game.buildings += SpaceRef(a, SpaceRef.extra + game.buildings.keys.count(s => s.area == a && s.index >= SpaceRef.extra)) -> CarvedStone
            f.log("placed a", CarvedStone, "in", a, "with", c)
            Then(RaidStonesAction(f, c, left - 1, then))

        // Clear the Frontlines: enemy units (not warchiefs) in open territories
        case RaidClearAction(f, c, left, then) =>
            val l = (left > 0).??(game.board.territories.%(game.board.open)./~(t => game.present(t).%(g => game.enemy(f, g) && game.count(t, g) > 0)./(g => t -> g)))
            Ask(f).each(l)((t, g) => RaidClearUnitAction(f, c, t.anchor, g, left, then)).add(then.as("Remove no more")(c)).bailHard(then)

        case RaidClearUnitAction(f, c, a, g, left, then) =>
            game.removeUnits(game.board.territory(a), g, 1)
            f.log("removed a unit of", g, "from", a, "with", c)
            Then(RaidClearAction(f, c, left - 1, then))

        // Frontline Reinforcements: 1 unit in each of up to 5 open territories
        case RaidReinforceAction(f, c, left, placed, then) =>
            val l = (left > 0 && game.reserve(f) > 0).??(game.controlled(f).%(game.board.open).%(t => placed.exists(t.areas.contains).not).%(t => game.bearIn(t).not && game.hostileIn(t).not))
            Ask(f).each(l)(t => RaidReinforceToAction(f, c, t.anchor, left, placed, then)).add(then.as("Add no more")(c)).bailHard(then)

        case RaidReinforceToAction(f, c, a, left, placed, then) =>
            game.addUnits(a, f, 1)
            f.log("added a unit to", a, "with", c)
            Then(RaidReinforceAction(f, c, left - 1, placed :+ a, then))

        // Moving units without a Move action: to a neutral territory (Infiltrate and Conquer) or between f's territories (Glory of the Homeland)
        case RaidShiftAction(f, c, neutral, all, then) =>
            val l = game.controlled(f).%(t => game.count(t, f) > 0 && game.bearIn(t).not)
            val ask = Ask(f).each(l)(t => RaidShiftFromAction(f, c, t.anchor, neutral, all, then))
            if (neutral)
                ask.add(then.as("Move nothing")(c)).bailHard(then)
            else
                ask.add(then.as("Done")(c)).bailHard(then)

        case RaidShiftFromAction(f, c, from, neutral, all, then) =>
            val t = game.board.territory(from)
            val l = neutral.?(this.neutral).|(game.controlled(f).%(_ != t).%(o => game.bearIn(o).not && game.hostileIn(o).not))
            Ask(f).each(l)(o => RaidShiftToAction(f, c, from, o.anchor, neutral, all, then)).cancel

        case RaidShiftToAction(f, c, from, to, neutral, all, then) =>
            val n = game.count(game.board.territory(from), f)
            Ask(f).each(1.to(all.?(n).|(1)).$.reverse)(k => RaidShiftMoveAction(f, c, from, to, k, neutral, all, then)).cancel

        case RaidShiftMoveAction(f, c, from, to, n, neutral, all, then) =>
            game.removeUnits(game.board.territory(from), f, n)
            game.addUnits(to, f, n)
            f.log("moved", n.hl, (n == 1).?("unit").|("units"), "from", from, "to", to, "with", c)
            // Glory of the Homeland goes on until Done; Infiltrate and Conquer is one move
            Then(neutral.?(then).|(RaidShiftAction(f, c, false, true, then)))

        case RaidRazeAction(f, c, s, b, then) =>
            game.buildings -= s
            f.log("destroyed", b, "in", s.area, "with", c)
            Then(then)

        // 3. HARVEST: the Raid cards kept for it, after harvesting
        case AfterHarvestAction(then) if game.raidKept.exists(k => RaidCard.harvest.has(k.card.id)) && game.raidSteps.has("harvest-" + game.year).not =>
            game.raidSteps :+= "harvest-" + game.year
            val l = game.raidKept.%(k => RaidCard.harvest.has(k.card.id)).sortBy(k => game.from(game.first).indexOf(k.f))
            Then(RaidHarvestAction(l, AfterHarvestAction(then)))

        case RaidHarvestAction(Nil, then) =>
            Then(then)

        case RaidHarvestAction(k :: rest, then) =>
            val f = k.f
            val c = k.card
            val two = k.years == 2
            val done = RaidKeptDoneAction(k, rest, then)
            val skip = RaidSkipKeptAction(f, c, k, rest, then)

            c.id match {
                case "raiders-reward" =>
                    if (two)
                        Ask(f).each(enemyTerritories(f))(t => RaidStealAction(f, c, t.anchor, None, k, rest, then)).add(skip)
                    else {
                        val near = game.controlled(f).flatMap(game.board.adjacent).map(_._1).distinct
                        Ask(f).some(enemyTerritories(f).%(near.has))(t => produced(t)./(r => RaidStealAction(f, c, t.anchor, |(r), k, rest, then))).add(skip)
                    }

                case "warlords-tribute" =>
                    val open = enemyTerritories(f).%(game.board.open)
                    if (two) {
                        open.foreach { t =>
                            val g = game.present(t).head
                            produced(t).%(r => g.has(r) > 0).sortBy(r => -g.has(r)).take(1).foreach { r =>
                                g.gain(r, -1)
                                f.gain(r, 1)
                                f.log("took", 1.hl, r, "from", g, "in", t.anchor, "with", c)
                            }
                        }
                        Then(done)
                    }
                    else
                        Ask(f).some(open)(t => produced(t)./(r => RaidStealAction(f, c, t.anchor, |(r), k, rest, then))).add(skip)

                case "conquerors-tribute" =>
                    val open = enemyTerritories(f).%(game.board.open).num
                    val n = two.?(math.min(10, 2 * open)).|(open)
                    f.fame += n
                    f.log("gained", n.hl, FameIcon(), "for", open.hl, "open enemy territories with", c)
                    Then(done)

                case "trade-and-triumph" if two =>
                    Then(RaidTriumphAction(f, c, $, $, k, rest, then))

                case "trade-and-triumph" | "fame-for-fortune" if two.not =>
                    Ask(f).some(Resource.all.%(f.has(_) > 0))(r => Resource.all.%(_ != r)./(x => RaidExchangeAction(f, c, r, x, false, $, $, k, rest, then))).add(skip)

                case "fame-for-fortune" =>
                    Ask(f).some(Resource.all.%(f.has(_) > 0))(r => $(1, f.has(r)).distinct./(n => RaidFortuneAction(f, c, r, n, k, rest, then))).add(skip)

                case _ => Then(done)
            }

        case RaidKeptDoneAction(k, rest, then) =>
            game.raidKept = game.raidKept.diff($(k))
            toBottom(k.card)
            Then(RaidHarvestAction(rest, then))

        case RaidSkipKeptAction(f, c, k, rest, then) =>
            Then(RaidKeptDoneAction(k, rest, then))

        case RaidStealAction(f, c, a, r, k, rest, then) =>
            val t = game.board.territory(a)
            val g = game.present(t).head
            r match {
                case Some(r) =>
                    val n = math.min(1, g.has(r))
                    g.gain(r, -n)
                    f.gain(r, n)
                    f.log("took", n.hl, r, "from", g, "in", a, "with", c)
                case None =>
                    produced(t).foreach { r =>
                        val (food, wood, lore) = game.harvest(t)
                        val n = math.min(r match { case Food => food ; case Wood => wood ; case _ => lore }, g.has(r))
                        g.gain(r, -n)
                        f.gain(r, n)
                        f.log("took", n.hl, r, "from", g, "in", a, "with", c)
                    }
                    val fame = math.min(g.fame, (game.board.closed(t) && game.wolfIn(t).not).??((game.board.tiles(t) >= 3).?(2).|(1)))
                    if (fame > 0) {
                        g.fame -= fame
                        f.fame += fame
                        f.log("took", fame.hl, FameIcon(), "from", g)
                    }
            }
            Then(RaidKeptDoneAction(k, rest, then))

        // Exchanges: once, or (Trade and Triumph, 2 years) as many as wanted for 1 fame each, never taking back a resource given
        case RaidTriumphAction(f, c, given, taken, k, rest, then) =>
            Ask(f).some(Resource.all.%(f.has(_) > 0).diff(taken))(r => Resource.all.%(_ != r).diff(given)./(x => RaidExchangeAction(f, c, r, x, true, given, taken, k, rest, then)))
                .add(RaidSkipKeptAction(f, c, k, rest, then))

        case RaidExchangeAction(f, c, r, x, fame, given, taken, k, rest, then) =>
            f.gain(r, -1)
            f.gain(x, 1)
            if (fame)
                f.fame += 1
            f.log("exchanged", 1.hl, r, "for", 1.hl, x, fame.?("and gained " ~ 1.hl ~ " " ~ FameIcon()).|(Empty), "with", c)
            if (fame)
                Then(RaidTriumphAction(f, c, (given :+ r).distinct, (taken :+ x).distinct, k, rest, then))
            else
                Then(RaidKeptDoneAction(k, rest, then))

        case RaidFortuneAction(f, c, r, n, k, rest, then) =>
            f.gain(r, -n)
            f.fame += n
            f.log("turned", n.hl, r, "into", n.hl, FameIcon(), "with", c)
            Then(RaidHarvestAction(k :: rest, then))

        case _ => UnknownContinue
    }
}
