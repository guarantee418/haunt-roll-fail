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


// UNCHARTED HORIZONS: TRAINING FIELDS (the main menu's Training Grounds, Meta.modes)
// A standalone duel on twelve of thirteen given base game tiles in a 4x3 grid, face down but for two opposing corners,
// the players' base tiles. Each player has seven Action cards; a turn plays one face-up card, which is then turned face
// down (Refresh turns them all face up again and scores the opponent). Combat: units plus the Combat Bonus of a random
// face-up card, one casualty each, the higher total wins (ties to the defender). The first to 5 victory points wins.
// The clans only name the players: no clan powers, decks, years, harvests or Winter.


// The seven Action cards, the same for both players: the action and the Combat Bonus printed below it
trait Drill extends NamedToString with Elementary with Record {
    def label : String
    def bonus : Int
    // The card's image name, after the player's color
    def id = label.toLowerCase.replace(" ", "")
    override def elem : Elem = label.hl
}

case object DrillRecruit extends Drill { val label = "Recruit" ; val bonus = 3 }
case object DrillExplore extends Drill { val label = "Explore" ; val bonus = 3 }
case object DrillSpecial extends Drill { val label = "Special" ; val bonus = 2 }
case object DrillMove2 extends Drill { val label = "Move 2" ; val bonus = 2 }
case object DrillMove1 extends Drill { val label = "Move 1" ; val bonus = 1 }
case object DrillBuild extends Drill { val label = "Build" ; val bonus = 1 }
case object DrillRefresh extends Drill { val label = "Refresh" ; val bonus = 0 }

object Drill {
    val all : $[Drill] = $(DrillRecruit, DrillExplore, DrillSpecial, DrillMove2, DrillMove1, DrillBuild, DrillRefresh)

    // What the Special card can do instead
    val special : $[Drill] = $(DrillRecruit, DrillBuild, DrillExplore, DrillMove1)
}


object Training {
    // The thirteen base game tiles the rulebook shows (only small building spaces), identified from its picture
    val tiles : $[String] = $("tile-01", "tile-06", "tile-08", "tile-10", "tile-12", "tile-14", "tile-16", "tile-19", "tile-20", "tile-23", "tile-26", "tile-30", "tile-32")

    val width = 4
    val height = 3

    val goal = 5

    val units = 10

    // The building tokens: two Defense Towers, three Training Camps and one of each Monopoly
    val supply : $[(Building, Int)] = $(DefenseTower -> 2, TrainingCamp -> 3, WoodcutterLodge -> 1, FoodSilo -> 1, CarvedStone -> 1)

    // The resources a player must control (they are not spent): food, wood, lore
    def cost(b : Building) : (Int, Int, Int) = b match {
        case WoodcutterLodge => (0, 3, 0)
        case FoodSilo => (3, 1, 0)
        case CarvedStone => (0, 1, 3)
        case _ => (0, 1, 0)
    }

    def monopoly(b : Building) = b == WoodcutterLodge || b == FoodSilo || b == CarvedStone

    def name(b : Building) : String = b match {
        case WoodcutterLodge => "Wood Monopoly"
        case FoodSilo => "Food Monopoly"
        case CarvedStone => "Lore Monopoly"
        case b => b.title
    }

    def costElem(b : Building) : Elem = {
        val (food, wood, lore) = cost(b)
        (List.fill(wood)(Wood.elem) ++ List.fill(food)(Food.elem) ++ List.fill(lore)(Lore.elem)).merge
    }

    // The base tile of each player: the first seat's in the top left corner, the second's in the bottom right one
    def base(f : Faction)(implicit game : Game) : (Int, Int) = (game.setup.indexOf(f) == 0).?((0, 0)).|((width - 1, height - 1))

    def opponent(f : Faction)(implicit game : Game) : Faction = game.setup.but(f).head

    def enemyIn(f : Faction, t : Territory)(implicit game : Game) = game.present(t).exists(_ != f)

    // The territories with an area on f's base tile
    def baseTerritories(f : Faction)(implicit game : Game) : $[Territory] = {
        val (x, y) = base(f)
        game.board.at(x, y).$./~(p => p.spec.areas./(a => game.board.territory(AreaRef(x, y, a.id)))).distinct
    }

    // Food, wood and lore printed in the territories f controls (buildings add none)
    def resources(f : Faction)(implicit game : Game) : (Int, Int, Int) = {
        val specs = game.controlled(f)./~(_.areas)./(game.board.spec)
        (specs./(_.food).sum, specs./(_.wood).sum, specs./(_.lore).sum)
    }

    def sets(f : Faction)(implicit game : Game) : Int = {
        val (food, wood, lore) = resources(f)
        $(food, wood, lore).min
    }

    def monopolies(f : Faction)(implicit game : Game) : Int = game.controlled(f)./~(game.buildingsIn).count(x => monopoly(x._2))

    // What f scores when the opponent plays Refresh
    def score(f : Faction)(implicit game : Game) : Int = sets(f) + monopolies(f)

    // A card's image: its face while face up, its back while face down
    def cardImage(f : Faction, d : Drill)(implicit game : Game) =
        "training-" + game.colors(f).id + "-" + game.drills.get(f).exists(_.has(d)).?(d.id).|("back")

    def left(b : Building)(implicit game : Game) : Int = supply.toMap.apply(b) - game.buildings.values.count(_ == b)

    def affordable(f : Faction, b : Building)(implicit game : Game) : Boolean = {
        val (food, wood, lore) = resources(f)
        val (cf, cw, cl) = cost(b)
        food >= cf && wood >= cw && lore >= cl
    }

    def buildable(f : Faction, b : Building)(implicit game : Game) : Boolean = left(b) > 0 && affordable(f, b)

    // Recruiting: any territory of the base tile without enemy units, or one f controls with a Training Camp
    def recruitTargets(f : Faction)(implicit game : Game) : $[Territory] =
        (game.reserve(f) > 0).??((baseTerritories(f).%(t => enemyIn(f, t).not) ++ game.controlled(f).%(t => game.buildingsIn(t).exists(_._2 == TrainingCamp))).distinct)

    def spaces(f : Faction)(implicit game : Game) : $[SpaceRef] =
        game.controlled(f)./~(_.areas)./~(a => game.board.spec(a).spaces.indices./(i => SpaceRef(a, i))).%(s => game.buildings.contains(s).not)

    def canBuild(f : Faction)(implicit game : Game) : Boolean = spaces(f).any && supply.exists(x => buildable(f, x._1))

    // Face-down tiles next to a territory f controls
    def explorable(f : Faction)(implicit game : Game) : $[Placement] = {
        val held = game.controlled(f)
        game.hiddenTiles.%(p => Side.all.exists(d => game.board.areaAt(p.x - d.dx, p.y - d.dy, d.opposite).exists(a => held.has(game.board.territory(a)))))
    }

    def canMove(f : Faction, n : Int)(implicit game : Game) : Boolean = {
        val e = MoveEffect(n)
        MapExpansion.moveSources(f, e).exists(t => MapExpansion.destinations(f, t, n, e).any)
    }

    def possible(f : Faction, d : Drill)(implicit game : Game) : Boolean = d match {
        case DrillRecruit => recruitTargets(f).any
        case DrillExplore => explorable(f).any
        case DrillBuild => canBuild(f)
        case DrillMove1 => canMove(f, 1)
        case DrillMove2 => canMove(f, 2)
        case DrillSpecial => Drill.special.exists(possible(f, _))
        case DrillRefresh => true
    }

    def reason(d : Drill) : String = d match {
        case DrillRecruit => "nowhere to recruit"
        case DrillExplore => "no face-down tile next to your territories"
        case DrillBuild => "nothing to build"
        case DrillMove1 | DrillMove2 => "no units can move"
        case _ => "nothing to do"
    }
}


// SETUP
case class TrainingTilesAction(shuffled : $[String]) extends ShuffledAction[String]
case class TrainingTurnsAction(tiles : $[String], shuffled : $[Int]) extends ShuffledAction[Int]
case class TrainingFirstAction(random : Faction) extends RandomAction[Faction]
case class TrainingSetupAction(f : Faction, left : Int, rest : $[Faction]) extends ForcedAction
case class TrainingPlaceAction(self : Faction, area : AreaRef, left : Int, rest : $[Faction]) extends BaseAction("Place a unit on your base tile", "(" ~ left.hl ~ " left)", "in")(area) with MapTarget { def target = area }

// TURNS
case class TrainingTurnAction(f : Faction) extends ForcedAction
case class TrainingEndTurnAction(f : Faction) extends ForcedAction

// An Action card, face up or face down, in its player's color (images in card/training/, cut from the Training Fields rulebook;
// the yellow ones are as printed, the others recolored)
case class DrillCard(f : Faction, d : Drill) extends GameElementary {
    def elem(implicit game : Game) = Image(Training.cardImage(f, d), styles.drillCard)
}

// Cards with a choice to make first (nothing happens until it is made), and cards played at once
case class DrillPickAction(self : Faction, d : Drill) extends BaseAction("Play an Action card")(DrillCard(self, d)) with Soft with ViewObject[Drill] { def obj = d }
case class DrillPlayAction(self : Faction, d : Drill) extends BaseAction("Play an Action card")(DrillCard(self, d)) with ViewObject[Drill] { def obj = d }
// Your Action cards while it isn't your turn
case class DrillInfoAction(self : Faction, title : Elem, d : Drill) extends BaseInfo(title)(DrillCard(self, d)) with ViewObject[Drill] { def obj = d }
case class SpecialPickAction(self : Faction, d : Drill) extends BaseAction("Special".hl, "do one of")(d) with Soft
case class SpecialMoveAction(self : Faction) extends BaseAction("Special".hl, "do one of")(DrillMove1)

case class TrainingRecruitAction(self : Faction, card : Drill, area : AreaRef) extends BaseAction(card, Comma, "recruit in")(area, TrainingRecruitCount(area)) with MapTarget { def target = area }

case class TrainingRecruitCount(area : AreaRef) extends GameElementary {
    def elem(implicit game : Game) = {
        val camps = game.working(game.board.territory(area)).count(_ == TrainingCamp)
        (camps > 0).?(("(" + (1 + camps) + " units, Training Camp)").spn(xstyles.smaller85)).|(Empty)
    }
}

case class TrainingSpaceAction(self : Faction, card : Drill, space : SpaceRef) extends BaseAction(card, Comma, "tap a building space")("A building space in", space.area) with Soft with MapTarget { def target = space }
case class TrainingBuildAction(self : Faction, card : Drill, space : SpaceRef, building : Building) extends BaseAction(card, Comma, "build in", space.area)(TrainingBuildingLabel(building))

case class TrainingBuildingLabel(b : Building) extends GameElementary {
    def elem(implicit game : Game) = Image(b.image, styles.buildIcon) ~ Training.name(b).hl ~ " (" ~ Training.costElem(b) ~ ", " ~ Training.left(b).hl ~ " left)"
}

case class TrainingExploreAction(self : Faction, card : Drill, spot : Spot) extends BaseAction(card, Comma, "turn a tile face up at")(spot) with MapTarget { def target = spot }
case class TrainingExploreFightsAction(f : Faction, then : ForcedAction) extends ForcedAction

// COMBAT: the attacker's then the defender's bonus card drawn at random from their face-up cards
case class TrainingFightAction(attacker : Faction, defender : Faction, area : AreaRef, then : ForcedAction) extends ForcedAction
case class TrainingBonusAction(attacker : Faction, defender : Faction, area : AreaRef, bonuses : $[Int], then : ForcedAction) extends ForcedAction
case class TrainingBonusDrawnAction(attacker : Faction, defender : Faction, area : AreaRef, bonuses : $[Int], random : Drill, then : ForcedAction) extends RandomAction[Drill]
case class TrainingResolveAction(attacker : Faction, defender : Faction, area : AreaRef, bonuses : $[Int], then : ForcedAction) extends ForcedAction


object TrainingExpansion extends Expansion {
    def opponent(f : Faction)(implicit game : Game) = Training.opponent(f)

    // Turn the card face down and say what it is used for
    def play(f : Faction, card : Drill, what : Any*)(implicit game : Game) {
        game.drills += f -> game.drills(f).but(card)
        f.log(("played" +: card +: what.$) : _*)
    }

    def choices(f : Faction, card : Drill, d : Drill)(implicit game : Game) : Continue = d match {
        case DrillRecruit =>
            Ask(f).each(Training.recruitTargets(f))(t => TrainingRecruitAction(f, card, t.anchor)).cancel
        case DrillBuild =>
            Ask(f).each(Training.spaces(f))(s => TrainingSpaceAction(f, card, s)).cancel
        case DrillExplore =>
            Ask(f).each(Training.explorable(f))(p => TrainingExploreAction(f, card, Spot(p.x, p.y))).cancel
        case DrillSpecial =>
            Ask(f)
                .each(Drill.special.but(DrillMove1))(d => SpecialPickAction(f, d).!(Training.possible(f, d).not, Training.reason(d)))
                .add(SpecialMoveAction(f).!(Training.possible(f, DrillMove1).not, Training.reason(DrillMove1)))
                .cancel
        case _ =>
            throw new Error("no choices for " + d)
    }

    def win(f : Faction)(implicit game : Game) : Continue = {
        game.isOver = true
        game.highlight.current = |(f)

        f.log("won with", f.fame.hl, "victory points")

        Debug.summary(game)

        CommonExpansion.victory($(f), $(f.elem ~ " reached " ~ f.fame.hl ~ " victory points first, in the " ~ "Training Fields".hl ~ "."))
    }

    def perform(action : Action, soft : Void)(implicit game : Game) = action @@ {
        case _ if game.training.not => UnknownContinue

        // SETUP
        case StartAction(version) =>
            log("HRF".hl, "version", gaming.version.hlb)
            log("Northgard: Uncharted Lands".hlb.styled(styles.title), "|", "Training Fields".hlb)

            if (version != gaming.version)
                log("Saved game version", version.hlb)

            game.setup.foreach { f =>
                game.states += f -> new FactionState(f)
                game.drills += f -> Drill.all
            }

            Shuffle[String](Training.tiles, TrainingTilesAction(_))

        case TrainingTilesAction(l) =>
            Shuffle[Int](0.until(Training.width * Training.height).$./(_ % 4), TrainingTurnsAction(l, _))

        // Twelve tiles face down in a 4x3 grid, turned as they come; the two opposing corners face up as the base tiles
        case TrainingTurnsAction(tiles, turns) =>
            val placed = tiles.take(Training.width * Training.height).zip(turns).zipWithIndex./{ case ((t, r), i) => Placement(t, i % Training.width, i / Training.width, r) }

            val bases = game.setup./(Training.base)

            placed.%(p => bases.has((p.x, p.y))).foreach(game.board.place)
            game.hiddenTiles = placed.%(p => bases.has((p.x, p.y)).not)

            game.setup.foreach { f =>
                f.log("plays", game.colors(f), "from the base tile in the", (Training.base(f) == (0, 0)).?("top left").|("bottom right"), "corner")
            }

            if (options.has(FirstSeatStarts))
                Random[Faction]($(game.setup.first), TrainingFirstAction(_))
            else
                Random[Faction](game.setup, TrainingFirstAction(_))

        case TrainingFirstAction(f) =>
            game.first = f

            log(f, "goes first")

            Then(TrainingSetupAction(f, 3, $(opponent(f))))

        // Each player places three units on their base tile, split as they like
        case TrainingSetupAction(f, left, rest) =>
            if (left <= 0)
                rest match {
                    case g :: l => Then(TrainingSetupAction(g, 3, l))
                    case Nil => Then(TrainingTurnAction(game.first))
                }
            else
                Ask(f).each(Training.baseTerritories(f))(t => TrainingPlaceAction(f, t.anchor, left, rest))

        case TrainingPlaceAction(f, a, left, rest) =>
            game.addUnits(a, f, 1)

            f.log("placed a unit in", a)

            Then(TrainingSetupAction(f, left - 1, rest))

        // A TURN: one face-up Action card
        case TrainingTurnAction(f) =>
            game.highlight.current = |(f)

            Ask(f).each(Drill.all) { d =>
                val a = (d == DrillMove1 || d == DrillMove2 || d == DrillRefresh).?(DrillPlayAction(f, d) : UserAction).|(DrillPickAction(f, d))
                if (game.drills(f).has(d).not)
                    a.!(true, "face down")
                else
                    a.!(Training.possible(f, d).not, Training.reason(d))
            }

        case DrillPickAction(f, d) =>
            choices(f, d, d)

        case SpecialPickAction(f, d) =>
            choices(f, DrillSpecial, d)

        case SpecialMoveAction(f) =>
            play(f, DrillSpecial, "to", "Move 1".hl)

            Then(MoveStartAction(f, MoveEffect(1), TrainingEndTurnAction(f)))

        case DrillPlayAction(f, DrillRefresh) =>
            val g = opponent(f)
            val sets = Training.sets(g)
            val monopolies = Training.monopolies(g)

            game.drills += f -> Drill.all
            g.fame += sets + monopolies

            f.log("played", DrillRefresh, "and turned all their Action cards face up")
            g.log("scored", (sets + monopolies).hl, "VP", "(" ~ sets.hl ~ " for sets of three resources, " ~ monopolies.hl ~ " for Monopolies)", "and has", g.fame.hl)

            if (g.fame >= Training.goal)
                win(g)
            else
                Then(TrainingEndTurnAction(f))

        case DrillPlayAction(f, d) =>
            play(f, d)

            Then(MoveStartAction(f, MoveEffect((d == DrillMove2).?(2).|(1)), TrainingEndTurnAction(f)))

        case TrainingRecruitAction(f, card, a) =>
            val t = game.board.territory(a)
            val n = math.min(1 + game.working(t).count(_ == TrainingCamp), game.reserve(f))

            game.addUnits(t.anchor, f, n)

            play(f, card, "and recruited", (n == 1).?("a unit".txt).|(n.hl ~ " units"), "in", t.anchor)

            Then(TrainingEndTurnAction(f))

        case TrainingSpaceAction(f, card, s) =>
            Ask(f).each(Training.supply.map(_._1))(b => TrainingBuildAction(f, card, s, b).!(Training.left(b) <= 0, "none left").!(Training.affordable(f, b).not, "not enough resources")).cancel

        case TrainingBuildAction(f, card, s, b) =>
            game.buildings += s -> b

            play(f, card, "and built a", Training.name(b).hl, "in", s.area)

            Then(TrainingEndTurnAction(f))

        case TrainingExploreAction(f, card, spot) =>
            val p = game.hiddenTiles.find(p => p.x == spot.x && p.y == spot.y).get

            game.hiddenTiles = game.hiddenTiles.but(p)
            game.board.place(p)

            play(f, card, "and turned a face-down tile face up")

            Then(TrainingExploreFightsAction(f, TrainingEndTurnAction(f)))

        // The new tile may join territories with units of both players: the explorer attacks
        case TrainingExploreFightsAction(f, then) =>
            game.board.territories.%(t => game.present(t).num > 1).headOption match {
                case Some(t) => Then(TrainingFightAction(f, opponent(f), t.anchor, TrainingExploreFightsAction(f, then)))
                case None => Then(then)
            }

        case TrainingEndTurnAction(f) =>
            Then(TrainingTurnAction(opponent(f)))

        // COMBAT
        case FightStartAction(f, a, e, then) =>
            val t = game.board.territory(a)

            if (game.present(t).but(f).none) {
                f.log("took", a, "without a fight")
                Then(then)
            }
            else
                Then(TrainingFightAction(f, game.present(t).but(f).head, a, then))

        case TrainingFightAction(f, d, a, then) =>
            game.fights += 1
            game.battle = |(a)

            log(f, "attacks", d, "in", a)

            Then(TrainingBonusAction(f, d, a, $, then))

        case TrainingBonusAction(f, d, a, bonuses, then) =>
            if (bonuses.num >= 2)
                Then(TrainingResolveAction(f, d, a, bonuses, then))
            else {
                val g = (bonuses.num == 0).?(f).|(d)

                if (game.drills(g).none) {
                    g.log("has no face-up Action card for a Combat Bonus")
                    Then(TrainingBonusAction(f, d, a, bonuses :+ 0, then))
                }
                else
                    Random[Drill](game.drills(g), TrainingBonusDrawnAction(f, d, a, bonuses, _, then))
            }

        // A Refresh card drawn gives no bonus: its owner turns their cards face up, and the opponent scores nothing
        case TrainingBonusDrawnAction(f, d, a, bonuses, c, then) =>
            val g = (bonuses.num == 0).?(f).|(d)

            if (c == DrillRefresh) {
                game.drills += g -> Drill.all
                g.log("drew", c, "for the Combat Bonus: no bonus, but all their Action cards are face up again")
            }
            else {
                game.drills += g -> game.drills(g).but(c)
                g.log("drew", c, "for the Combat Bonus:", ("+" + c.bonus).hl)
            }

            Then(TrainingBonusAction(f, d, a, bonuses :+ (c == DrillRefresh).?(0).|(c.bonus), then))

        // One casualty each (the attacker one more per Defense Tower), the higher total wins, ties to the defender;
        // a side with no units left loses, and with none on either side the territory is left empty
        case TrainingResolveAction(f, d, a, bonuses, then) =>
            val t = game.board.territory(a)
            val sf = game.count(t, f) + bonuses(0)
            val sd = game.count(t, d) + bonuses(1)
            val towers = game.working(t).count(_ == DefenseTower)

            log(f, "has", sf.hl, "against", sd.hl, "for", d)

            game.removeUnits(t, d, 1)
            game.removeUnits(t, f, 1 + towers)

            if (towers > 0)
                f.log("lost", (1 + towers).hl, "units with", (towers > 1).?("the Defense Towers").|("the Defense Tower"))

            val nf = game.count(t, f)
            val nd = game.count(t, d)

            if (nf == 0 && nd == 0) {
                log("Both sides were wiped out;", a, "is empty")
                Then(FightOverAction(then))
            }
            else
            if (nf == 0) {
                d.log("won the fight in", a)
                Then(FightOverAction(then))
            }
            else
            if (nd == 0) {
                f.log("won the fight in", a)
                Then(FightOverAction(then))
            }
            else {
                val (winner, loser) = if (sf > sd) (f, d) else (d, f)
                winner.log("won the fight in", a)
                Then(RetreatAction(loser, a, false, FightOverAction(then)))
            }

        case _ => UnknownContinue
    }
}


// The bots in the Training Fields: Easy plays with much more chance in its choices than Hard
class TrainingEvaluation(val self : Faction, noise : Int)(implicit val game : Game) {
    def eval(a : Action) : $[Evaluation] = {
        var result : $[Evaluation] = Nil

        implicit class condToEval(val bool : Boolean) {
            def |=> (e : (Int, String)) { if (bool) result +:= Evaluation(e._1, e._2) }
        }

        val other = Training.opponent(self)
        val up = game.drills(self).but(DrillRefresh).num

        // A territory's printed resources (more for the kind f has least of), and whether it is next to the opponent
        def worth(t : Territory) : Int = {
            val (food, wood, lore) = Training.resources(self)
            val least = $(food, wood, lore).min
            t.areas./(game.board.spec)./(s => (s.food + s.wood + s.lore) + (food == least).??(s.food) + (wood == least).??(s.wood) + (lore == least).??(s.lore)).sum
        }
        def threatened(t : Territory) : Boolean = game.board.adjacent(t).exists { case (o, _) => game.count(o, other) > 0 }

        // The best a Move of n can do: take a neutral territory with resources, or attack with the odds on our side
        def moveValue(n : Int) : Int = {
            val e = MoveEffect(n)
            val l = MapExpansion.moveSources(self, e)./~(t => MapExpansion.destinations(self, t, n, e)./ { case (o, _) =>
                val enemy = game.count(o, other)
                val mine = game.count(t, self)
                if (enemy > 0)
                    (mine + 1 > enemy + game.working(o).count(_ == DefenseTower)).?(35 + 10 * worth(o)).|(-10)
                else
                if (game.present(o).none)
                    12 + 8 * worth(o)
                else
                    0
            })
            l.maxOption.|(0)
        }

        a.unwrap @@ {
            case CancelAction => true |=> -1000 -> "cancel"

            case DrillPlayAction(_, DrillRefresh) =>
                val gain = Training.score(other)
                (other.fame + gain >= Training.goal) |=> -900 -> "the opponent would win"
                true |=> (12 * (Drill.all.num - 1 - up) - 30 * gain) -> "refresh"

            // Choices made later (where to recruit, build or explore) are scored on the final action, where the card is known
            case DrillPlayAction(_, d) =>
                true |=> (moveValue((d == DrillMove2).?(2).|(1)) - d.bonus) -> "move"

            case SpecialMoveAction(_) =>
                true |=> (moveValue(1) - 8) -> "move with Special"

            case TrainingExploreAction(_, card, _) =>
                true |=> (20 + 4 * game.hiddenTiles.num - card.bonus - (card == DrillSpecial).??(5)) -> "explore"

            case TrainingRecruitAction(_, card, area) =>
                val t = game.board.territory(area)
                true |=> (32 - 3 * game.onMap(self) + 8 * game.working(t).count(_ == TrainingCamp) + threatened(t).??(8) - card.bonus - (card == DrillSpecial).??(5)) -> "recruit"

            case TrainingBuildAction(_, card, s, b) =>
                val t = game.board.territory(s.area)
                Training.monopoly(b) |=> 70 -> "monopoly"
                (b == TrainingCamp) |=> (25 + threatened(t).??(10)) -> "camp"
                (b == DefenseTower) |=> (10 + threatened(t).??(20)) -> "tower"
                true |=> -(card.bonus + (card == DrillSpecial).??(5)) -> "card"

            case MoveUnitsAction(_, from, to, n, _, _, _, _, _, _) =>
                val src = game.board.territory(from)
                val dst = game.board.territory(to)
                val enemy = game.count(dst, other)
                val left = game.count(src, self) - n
                (enemy > 0 && n + 2 > enemy + game.working(dst).count(_ == DefenseTower)) |=> (30 + 10 * worth(dst)) -> "attack"
                (enemy > 0 && n + 2 <= enemy) |=> -40 -> "weak attack"
                (enemy == 0 && game.present(dst).none) |=> (10 + 10 * worth(dst)) -> "take a territory"
                (left == 0 && worth(src) > 0) |=> -10 -> "leave a territory"
                (enemy == 0 && game.present(dst).has(self)) |=> (threatened(dst) && threatened(src).not).?(3).|(-8) -> "move between own territories"

            case MoveDoneAction(_, _, _) => true |=> -5 -> "stop moving"

            case _ =>
        }

        result.none |=> 0 -> "none"

        true |=> -((1 + math.random() * noise).round.toInt) -> "random"

        result.sortBy(v => -v.weight.abs)
    }
}
