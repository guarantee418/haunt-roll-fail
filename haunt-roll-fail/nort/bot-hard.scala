package nort
//
//
//
//
import hrf.colmat._
import hrf.compute._
import hrf.logger._
//
//
//
//

// The "Hard" bot. Every choice is scored by trying it on the game for a moment (units, buildings, Kaija, the warchief,
// resources and placed tiles are put back right after) and valuing the position in hundredths of a fame point:
// what the clan's territories will give at the remaining harvests, closed territories and Altars, progress towards
// three closed territories with large buildings (the sudden win), units and resources, the Winter bill, and the risk
// of losing territories to adjacent enemies. Fights use the exact odds of the two dice. Opponents' positions count
// against the bot, the leader's more, so it attacks a clan that is about to win.
//
// Playing priorities follow strategy discussions on BoardGameGeek (Northgard: Uncharted Lands forums):
// - most games end with three closed territories holding large buildings, so large spaces and closing territories
//   matter, and an opponent close to that is attacked;
// - a Forge early (one more card each year) and Training Camps where units are recruited;
// - an attacker with as much strength as the defender wins only about a third of the fights: attack with +2 or +3,
//   or spend food to get there;
// - clan upgrades are nearly always worth their 3 lore;
// - a clan short of resources builds a Food Silo or Woodcutter's Lodge first;
// - attacks are worth more once the defender has passed (no counterattack this year);
// - pass early when the hand has little left, to take the best Development card.

class BotHard(f : Faction) extends EvalBot {
    def eval(actions : $[UserAction])(implicit game : Game) : Compute[$[ActionEval]] = {
        if (f == Automa || game.states.contains(f).not)
            return new BotXX(f).eval(actions)

        val ev = new HardEvaluation(f)
        actions./{ a => ActionEval(a, ev.eval(a)) }
    }
}

// Fight odds: one Northgard die each; the attacker rolls first and picks "1 point or 1 casualty" before the defender rolls
object HardCombat {
    val faces = NorthgardDie.faces

    // as, ds: combat points before the dice (units, companions, warchiefs, card bonus, food, buildings);
    // au, du: figures that can be lost; towers: casualties added to the defender's roll; extra: casualties added to the attacker's
    def attackWin(as : Int, ds : Int, au : Int, du : Int, towers : Int = 0, extra : Int = 0) : Double = {
        if (du <= 0)
            return 1.0
        if (au <= 0)
            return 0.0

        faces.map { fa =>
            val options = if (fa.choice) $(NorthgardDie.point, NorthgardDie.casualty) else $(fa)
            options.map(a => faces.map(fd => outcome(as, ds, au, du, towers, extra, a, fd)).sum / 6.0).max
        }.sum / 6.0
    }

    // The attacker's face is known (fa, chosen already); the defender picks for their own good
    def outcome(as : Int, ds : Int, au : Int, du : Int, towers : Int, extra : Int, fa : DieFace, fd : DieFace) : Double = {
        if (fd.choice)
            math.min(result(as, ds, au, du, towers, extra, fa, NorthgardDie.point), result(as, ds, au, du, towers, extra, fa, NorthgardDie.casualty))
        else
            result(as, ds, au, du, towers, extra, fa, fd)
    }

    def result(as : Int, ds : Int, au : Int, du : Int, towers : Int, extra : Int, fa : DieFace, fd : DieFace) : Double = {
        val ac = fa.casualties + extra
        val dc = fd.casualties + towers
        if (ac >= du && dc >= au) 0.0
        else if (ac >= du) 1.0
        else if (dc >= au) 0.0
        else if (as + fa.points > ds + fd.points) 1.0
        else 0.0
    }

    // A fight against a creature: the player takes the point on a choice face, and so does the creature;
    // only the creature's casualties count; ties go to the defender
    def creatureWin(points : Int, value : Int, units : Int, attacking : Boolean) : (Double, Double) = {
        var win = 0.0
        var lost = 0.0
        faces.foreach { fp =>
            faces.foreach { fc =>
                val ps = points + fp.choice.?(1).|(fp.points)
                val cs = value + fc.choice.?(1).|(fc.points)
                val pc = fc.choice.?(0).|(fc.casualties)
                if (pc < units && (attacking.?(ps > cs).|(ps >= cs)))
                    win += 1
                lost += math.min(pc, units)
            }
        }
        (win / 36, lost / 36)
    }

    // The odds once the attacker's die is rolled (and maybe the defender's): for choosing a face
    def attackWinGiven(as : Int, ds : Int, au : Int, du : Int, towers : Int, extra : Int, fa : DieFace, fd : |[DieFace]) : Double =
        fd match {
            case Some(d) => result(as, ds, au, du, towers, extra, fa, d)
            case None => faces.map(d => outcome(as, ds, au, du, towers, extra, fa, d)).sum / 6.0
        }
}

object HardEvaluation {
    // An action the bot fails to value is valued like the Easy bot does; the first few failures are logged
    var failures = 0

    def failed(a : Action, e : Throwable) {
        failures += 1
        if (failures <= 5)
            warn("hard bot failed to value " + a + ": " + e)
    }
}

class HardEvaluation(val self : Faction)(implicit val game : Game) {
    val board = game.board

    // Harvests still to come, this year's included; values over more than three years are too uncertain to count
    val harvests : Int = math.max(0, game.lastYear - game.year + 1)
    val horizon : Double = math.min(harvests, 3).toDouble

    // The actions phase is over once everyone has passed: the harvest is then in the resources already
    def harvestAhead : Boolean = game.factions.exists(f => game.states.contains(f) && game.states(f).passed.not)

    val players = game.factions.%(f => game.states.contains(f))
    val enemies = players.%(f => game.enemy(self, f))
    val mates = players.%(f => f != self && game.allied(self, f))

    def score(f : Faction) : Double = f.fame + harvestFame(f) * math.max(1, harvests - 1)

    def harvestFame(f : Faction) : Double = try { Harvest.forecast(f).fame } catch { case e : Throwable => 0 }

    // The enemy closest to winning gets more weight
    lazy val leader : |[Faction] = enemies.sortBy(f => -(score(f) + 15 * game.strongholds(f).num)).headOption

    def enemyWeight(f : Faction) : Double = (enemies.num > 0).?(0.6 / enemies.num).|(0.0) + leader.has(f).?(0.3).|(0.0)

    // ADJACENCY: changes only with the tiles
    private var adjacencyKey : $[Placement] = null
    private val adjacencyCache = scala.collection.mutable.Map[Territory, $[(Territory, Boolean)]]()

    def adjacent(t : Territory) : $[(Territory, Boolean)] = {
        if (adjacencyKey ne board.placements) {
            adjacencyCache.clear()
            adjacencyKey = board.placements
        }
        adjacencyCache.getOrElseUpdate(t, board.adjacent(t))
    }

    // RESOURCES: what one is worth, in hundredths of fame
    def resourceValue(r : Resource) : Double = {
        if (harvests <= 1 && harvestAhead.not)
            return 34
        r match {
            case Food => 40
            case Wood => 50
            case Lore => (self.upgrades.any && game.year < game.lastYear).?(55).|(42)
        }
    }

    // TERRITORIES: what controlling one is worth to f over the rest of the game
    def territoryWorth(t : Territory, f : Faction) : Double = {
        val (food, wood, lore) = game.produce(t)
        val here = game.working(t)
        val closed = board.closed(t)

        var v = 20.0
        v += horizon * (food * 45 + wood * 55 + lore * 50)

        if (closed)
            v += horizon * 100 * (board.tiles(t) >= 3).?(2).|(1)

        v += horizon * 300 * here.count(_ == AltarOfKings)
        v += horizon * 110 * here.count(_ == Forge)
        v += 35 * here.count(_ == Fortress)
        v += 25 * here.count(_ == DefenseTower)
        v += (game.year <= 4).?(45).|(20) * here.count(_ == TrainingCamp)

        val spaces = t.areas./~(a => board.spec(a).spaces.indices./(i => SpaceRef(a, i)))
        val free = spaces.%(s => game.buildings.contains(s).not)
        val large = free.count(s => MapExpansion.spaceKind(s) == LargeSpace)
        v += large * (game.domination.?(70).|(45)) * (here.exists(_.large).?(0.5).|(1.0))
        v += (free.num - large) * 15

        if (game.has(Creatures))
            v -= 40 * game.creaturesIn(t).num

        v
    }

    // Progress towards the sudden win: closed territories with a large building
    def strongholdValue(f : Faction) : Double = {
        if (game.domination.not)
            return 0

        val need = game.strongholdsToWin
        val s = game.strongholds(f).num

        if (s >= need)
            return 6000

        val controlled = game.controlled(f)
        // Closed with a free large space, or open with a large building already
        val near = controlled.%(t => game.buildingsIn(t).exists(_._2.large).not && board.closed(t) &&
            t.areas.exists(a => board.spec(a).spaces.indices.exists(i => board.spec(a).spaces(i).kind == LargeSpace && game.buildings.contains(SpaceRef(a, i)).not))).num
        val openBig = controlled.%(t => board.open(t) && game.buildingsIn(t).exists(_._2.large)).num

        val base = $(0, 150, 550, 1500, 3000)(math.min(s, 4))
        val potential = math.min(near, need - s) * (s + 1) * 50 + math.min(openBig, need - s) * (s + 1) * 30

        base + potential
    }

    // Food and wood owed at the next Winter, against what f will have; an Unrest card costs 5 fame
    def winterValue(f : Faction) : Double = {
        val (needFood, needWood) = Winter.cost(f.units)
        val fc = if (harvestAhead) (try { Harvest.forecast(f) } catch { case e : Throwable => HarvestForecast(0, 0, 0, 0) }) else HarvestForecast(0, 0, 0, 0)
        val food = f.food + fc.food
        val wood = f.wood + fc.wood
        val lore = f.lore + fc.lore
        val short = math.max(0, needFood - food) + math.max(0, needWood - wood)
        if (short == 0)
            return 0
        val spare = math.max(0, food - needFood) + math.max(0, wood - needWood) + lore
        // Before the harvest, the harvest trade can still cover it; at the trade itself, only trading now does
        if (harvestAhead && spare / 3 >= short)
            -90.0 * short
        else
            -550.0 - 60 * short
    }

    // The points an enemy could bring against t from the territories next to it
    def threat(t : Territory, f : Faction) : (Faction, Int) = {
        var best : (Faction, Int) = (f, 0)
        enemiesOf(f).foreach { e =>
            val groups = adjacent(t)./{ case (o, _) => game.figures(o, e) }.%(_ > 0).sortBy(-_)
            val str = groups.take(1).sum + groups.drop(1).sum / 2
            if (str > best._2)
                best = (e, str)
        }
        best
    }

    def enemiesOf(f : Faction) : $[Faction] = players.%(g => game.enemy(f, g))

    def defense(t : Territory, f : Faction) : (Int, Int) = {
        val here = game.working(t)
        (game.strength(t, f, false) + 2 * here.count(_ == Fortress) + (f == Snake && game.scorchedIn(t)).??(1), here.count(_ == DefenseTower))
    }

    def risk(t : Territory, f : Faction, worth : Double) : Double = {
        val (e, str) = threat(t, f)
        if (str == 0)
            return 0
        val (ds, towers) = defense(t, f)
        val du = game.figures(t, f)
        // A card bonus or food is likely; units must stay behind in the territories they come from
        val p = HardCombat.attackWin(str + 1, ds + math.min(du, f.food), str, du, towers)
        val likely = (e.passed).?(0.12).|(0.4) * (harvests <= 1).?(1.2).|(1.0)
        likely * p * (worth + 30 * du)
    }

    // POSITION: the whole value of f's position
    def value(f : Faction) : Double = {
        var v = 0.0

        game.controlled(f).foreach { t =>
            val w = territoryWorth(t, f)
            v += w
            v -= risk(t, f, w)
        }

        // Territories with a fight pending: the attacker moved in this turn
        board.territories.foreach { t =>
            val present = game.present(t)
            if (present.has(f) && present.num > 1) {
                val other = present.but(f).head
                val attacking = game.current.has(f) || (game.current.has(other).not && f == self)
                val w = territoryWorth(t, f)
                val p = fightOdds(t, attacking.?(f).|(other), attacking.?(other).|(f), 0)
                val mine = attacking.?(p).|(1 - p)
                v += mine * w - (1 - mine) * 35 * game.figures(t, f)
            }
        }

        v += strongholdValue(f)

        // Exploring needs an open territory: closing territories by exploring gives fame, and finds new spaces
        if (game.pile.any && harvests > 1) {
            val open = game.controlled(f).count(board.open)
            v += (open == 0).?(-25.0 * horizon).|(math.min(open, 2) * 8.0 * horizon)
        }

        v += 38 * game.onMap(f)
        v += (game.companion(f).any).??(60)
        v += (game.chiefs.contains(f)).??(70)

        v += f.food * resourceValueFor(f, Food) + f.wood * resourceValueFor(f, Wood) + f.lore * resourceValueFor(f, Lore)

        v += winterValue(f)

        v
    }

    def resourceValueFor(f : Faction, r : Resource) : Double = (f == self).?(resourceValue(r)).|(42)

    // Total: own position, plus teammates', less opponents'
    def total : Double = value(self) + mates./(value(_) * 0.7).sum - enemies./(e => value(e) * enemyWeight(e)).sum

    // The game current player, if any
    implicit class GameCurrent(g : Game) {
        def current : |[Faction] = g.highlight.current
    }

    // Odds of the attacker winning a fight in t as things stand
    def fightOdds(t : Territory, attacker : Faction, defender : Faction, bonus : Int) : Double = {
        val here = game.working(t)
        val conq = attacker.conqueror
        val fortress = conq.?(0).|(2 * here.count(_ == Fortress))
        val towers = conq.?(0).|(here.count(_ == DefenseTower))
        val au = game.figures(t, attacker)
        val du = game.figures(t, defender)
        val as = game.strength(t, attacker, true) + bonus + (attacker == Snake && game.scorchedIn(t)).??(1) + math.min(au, spendable(attacker))
        val ds = game.strength(t, defender, false) + fortress + (defender == Snake && game.scorchedIn(t)).??(1) + math.min(du, defender.food)
        HardCombat.attackWin(as, ds, au, du, towers)
    }

    // Food f would spend in a fight, keeping enough for Winter
    def spendable(f : Faction) : Int = {
        val (needFood, _) = Winter.cost(f.units)
        val fc = harvestAhead.??(try { Harvest.forecast(f).food } catch { case e : Throwable => 0 })
        math.max(0, math.min(f.food, f.food + fc - needFood))
    }

    // TRYING THINGS: change the game, value it, put everything back
    def trying(change : => Unit) : Double = {
        val units = game.units
        val buildings = game.buildings
        val chiefs = game.chiefs
        val companion = game.companion(self)
        val (food, wood, lore, fame) = (self.food, self.wood, self.lore, self.fame)
        try {
            change
            total
        }
        finally {
            game.units = units
            game.buildings = buildings
            game.chiefs = chiefs
            game.setCompanion(self, companion)
            self.food = food
            self.wood = wood
            self.lore = lore
            self.fame = fame
        }
    }

    lazy val now : Double = total

    def gain(change : => Unit) : Double = trying(change) - now

    def moveFigures(from : Territory, to : AreaRef, n : Int, kaija : Boolean, chief : Boolean) {
        game.removeUnits(from, self, n)
        game.addUnits(to, self, n)
        if (kaija)
            game.setCompanion(self, |(to))
        if (chief)
            game.chiefs += self -> to
    }

    // CARDS: what playing an effect now is likely worth
    def recruitGain(n : Int, mode : RecruitMode) : Double = {
        var placed : $[AreaRef] = $
        var sum = 0.0
        val units = game.units
        try {
            1.to(math.min(n, game.reserve(self))).foreach { _ =>
                val targets = MapExpansion.recruitTargets(self, mode, placed)
                if (targets.any) {
                    val base = total
                    val (t, g) = targets./(t => t -> (trying(game.addUnits(t.anchor, self, 1)) - base)).maxBy(_._2)
                    if (g > 0) {
                        sum += g
                        game.addUnits(t.anchor, self, 1)
                        placed :+= t.anchor
                    }
                }
            }
        }
        finally {
            game.units = units
        }
        sum
    }

    def moveOptions(e : MoveEffect) : $[Double] = {
        MapExpansion.moveSources(self, e).distinct./~ { t =>
            val n = game.count(t, self)
            val kaija = game.kaijaIn(t, self)
            val chief = game.chiefIn(t, self)
            MapExpansion.destinations(self, t, e.n, e)./~ { case (o, _) =>
                val sizes = $(n, n - 1, 1).%(_ >= 0).distinct
                sizes./(k => gain(moveFigures(t, o.anchor, k, kaija && k == n, chief && k == n)))
            }
        }
    }

    def moveGain(e : MoveEffect) : Double = {
        val l = moveOptions(e).%(_ > 0).sortBy(-_)
        l.take(1).sum + (e.n > 1).?(l.drop(1).take(e.n - 1).sum * 0.35).|(0.0) + e.bonus * (l.any).??(25)
    }

    def buildGain(e : BuildEffect) : Double = {
        val l = MapExpansion.buildOptions(self, e, false)./ { case (a, b, spaces, cost) =>
            if (cost > self.wood && e.special != GloryBuild) -1.0
            else spaces./(s => gain {
                game.buildings += s -> b
                self.wood -= cost
                if (self == Goat)
                    self.food += b.large.?(2).|(1)
            }).maxOr(-1.0) + buildingBonus(b, board.territory(a))
        }.%(_ > 0).sortBy(-_)
        l.take(1).sum + (e.times > 1).?(l.drop(1).take(1).sum * 0.6).|(0.0)
    }

    // What a building does beyond the territory's value: cards, Training Camps where units will be recruited
    def buildingBonus(b : Building, t : Territory) : Double = b match {
        case Forge => (harvests > 1).??(60 + 25 * math.min(harvests - 1, 4))
        case TrainingCamp => (game.year <= game.lastYear - 2).??(30)
        case _ => 0
    }

    def exploreGain(e : ExploreEffect) : Double = {
        if (game.pile.none || MapExpansion.explorable(self, e.anywhere).none)
            return 0
        val sample = game.pile.distinct.shuffle.take(3)
        val near = MapExpansion.explorable(self, e.anywhere)
        val values = sample./(tile => MapExpansion.placements(tile, Some(near), false).take(40)./ { case (spot, r) => explorePlacement(tile, spot, r) }.maxOr(0.0))
        if (values.none)
            return 0
        val v = (e.draw > 1).?(values.max).|(values.sum / values.num)
        v * e.times + (self == Boar).??(30)
    }

    // Placing a tile: the territories it joins, closing own territories (fame now), and the Raven's and Stag's powers
    // Only the bot's own position is valued again: a new tile hardly changes the others' (closing theirs is counted below)
    lazy val ownNow : Double = value(self)

    def explorePlacement(tile : String, spot : Spot, r : Int) : Double = {
        val base = ownNow
        val closedBefore = board.territories.%(board.closed)
        val enemyClosed = enemies./(e => game.controlled(e).%(board.closed).num).sum
        board.withPlaced(Placement(tile, spot.x, spot.y, r)) {
            val after = game.controlled(self)
            val closedNow = after.%(board.closed).%(t => closedBefore.has(t).not)
            val fame = closedNow./(board.tiles).sum
            val raven = (self == Raven).??(closedNow./(t => { val (f, w, l) = game.produce(t) ; f + w + l }).sum * 45)
            val stag = (self == Stag).??(closedNow.num * 100)
            val boar = (self == Boar && closedNow.none).??(45)
            // Closing an enemy's territory gives them fame every harvest
            val gift = enemies./(e => game.controlled(e).%(board.closed).num).sum - enemyClosed
            val lairs = game.has(Creatures).??(Tiles(tile).areas.count(_.lair) * 50)
            value(self) - base + fame * 100 + raven + stag + boar - lairs - gift * 40
        }
    }

    def effectGain(e : Effect) : Double = e match {
        case RecruitEffect(n, mode) => recruitGain(n, mode)
        case AwakenEffect => recruitGain(2, RecruitNormal)
        case e : MoveEffect => moveGain(e)
        case e : ExploreEffect => exploreGain(e)
        case e : BuildEffect => buildGain(e)
        case FeastEffect => $(RecruitEffect(1), MoveEffect(1), ExploreEffect(), BuildEffect()).%(MapExpansion.playable(self, _))./(effectGain).maxOr(0.0)
        case DrawEffect(n, keep, _, back) => 70 * keep + 15 * (n - keep - back) + 10 * back
        case CollectEffect(r, n) => n * resourceValue(r)
        case NegotiationEffect => 80
        case ResourcefulEffect => 170
        case ProtectorEffect => 75 * MapExpansion.bigClosed(self).num
        case MapEffect => 0
        case _ => 110
    }

    lazy val cardValues = scala.collection.mutable.Map[Card, Double]()

    def cardValue(c : Card) : Double = cardValues.getOrElseUpdate(c, if (CommonExpansion.playable(self, c)) effectGain(c.effect) + c.flash.??(15) else 0.0)

    // Cards worth thinning out of the deck
    def thin(c : Card) : Double = c match {
        case UnrestCard => 0
        case StartCard(_, "move") => 40
        case StartCard(_, "explore") => (game.pile.num < 8).?(50).|(10)
        case Development("market-place") | Development("feast") => 10
        case _ => -40
    }

    // A card to take into the deck: fame on it, and what it does each year it is drawn
    def utility(c : Card) : Double = {
        val e = c.effect
        val use = e match {
            case RecruitEffect(n, _) => 55 * n
            case RecruitPerEffect(_) => 110
            case MoveEffect(n, bonus, _, _) => 40 + 22 * n + 35 * bonus
            case e : ExploreEffect => (game.pile.num > 4).?(75 + 25 * (e.draw - 1) + 60 * (e.times - 1)).|(10)
            case e : BuildEffect => 85 + 30 * e.discount + 70 * (e.times - 1) + (e.special != PlainBuild).??(20)
            case DrawEffect(n, keep, _, _) => 45 * keep + 10 * n
            case CollectEffect(_, n) => 42 * n
            case FeastEffect => 80
            case MapEffect => 0
            case _ => 80
        }
        use + c.flash.??(30)
    }

    def pickValue(c : Card) : Double = c match {
        case a : Achievement => 100.0 * CommonExpansion.cardFame(self, a)
        case _ => 100.0 * c.fame + utility(c) * math.min(math.max(harvests - 1, 0), 3)
    }

    def upgradeValue(u : Card) : Double = 120 + 45 * math.min(math.max(harvests - 1, 0), 4) + utility(u) * 0.5

    // TURN CHOICES
    def turnValue(a : Action) : Double = a match {
        case PlayCardAction(_, c, stage) =>
            val v = cardValue(c)
            // An attack is better after the defender can't answer this year
            if (stage == 0) v else v - 10

        case EndTurnAction(_) => 0

        case WaitCardAction(_, c) => -cardValue(c) - 20 + (c == UnrestCard).??(15)

        case ReplaceCardAction(_, c) => 75 - resourceValue(Lore) - cardValue(c)

        case RemoveCardAction(_, c) => 150 - 2 * resourceValue(Lore) - cardValue(c) + thin(c)

        case UpgradeCardAction(_, c, u, remove) => upgradeValue(u) - 3 * resourceValue(Lore) - cardValue(c) + remove.?(thin(c)).|(0.0)

        // Passing takes the best Development card now, but the cards left in hand do nothing this year
        case PassAction(_) =>
            val best = game.display./(pickValue).maxOr(0.0)
            0.3 * best + 10 - self.hand.distinct./(c => math.max(0.0, cardValue(c)) * self.hand.count(c)).sum * 0.9
    }

    // FIGHTS
    def stake(t : Territory) : Double = 150 + territoryWorth(t, self)

    def foodChoice(attacker : Faction, defender : Faction, area : AreaRef, e : MoveEffect, food : $[Int]) : Double = {
        val t = board.territory(area)
        val k = food.last
        val here = game.working(t)
        val conq = attacker.conqueror
        val fortress = conq.?(0).|(2 * here.count(_ == Fortress))
        val towers = conq.?(0).|(here.count(_ == DefenseTower))
        val au = game.figures(t, attacker)
        val du = game.figures(t, defender)
        val attacking = self == attacker
        val af = attacking.?(k).|(food(0))
        val df = attacking.?(math.min(du, defender.food)).|(k)
        val as = game.strength(t, attacker, true) + e.bonus + (attacker == Snake && game.scorchedIn(t)).??(1) + af
        val ds = game.strength(t, defender, false) + fortress + (defender == Snake && game.scorchedIn(t)).??(1) + df
        val extra = (e.special == EgilMove).??(1)
        val p = HardCombat.attackWin(as, ds, au, du, towers, extra)
        val mine = attacking.?(p).|(1 - p)
        // Food spent eats into Winter
        val (needFood, _) = Winter.cost(self.units)
        val left = self.food - k
        val cost = k * resourceValue(Food) + (left < needFood && harvestAhead.not).??((needFood - left) * 120)
        mine * stake(t) - cost
    }

    def faceChoice(self0 : Faction, attacker : Faction, defender : Faction, area : AreaRef, e : MoveEffect, food : $[Int], faces : $[DieFace], choice : DieFace) : Double = {
        val t = board.territory(area)
        val here = game.working(t)
        val conq = attacker.conqueror
        val fortress = conq.?(0).|(2 * here.count(_ == Fortress))
        val towers = conq.?(0).|(here.count(_ == DefenseTower))
        val au = game.figures(t, attacker)
        val du = game.figures(t, defender)
        val as = game.strength(t, attacker, true) + e.bonus + (attacker == Snake && game.scorchedIn(t)).??(1) + food.lift(0).|(0)
        val ds = game.strength(t, defender, false) + fortress + (defender == Snake && game.scorchedIn(t)).??(1) + food.lift(1).|(0)
        val extra = (e.special == EgilMove).??(1)
        val p =
            if (faces.none) HardCombat.attackWinGiven(as, ds, au, du, towers, extra, choice, None)
            else HardCombat.attackWinGiven(as, ds, au, du, towers, extra, faces(0), |(choice))
        (self == attacker).?(p).|(1 - p) * 1000
    }

    // EVALUATION of one action
    def eval(a : Action) : $[Evaluation] = {
        val v : |[Double] = try { value(a) } catch { case e : Throwable => HardEvaluation.failed(a, e) ; None }

        v match {
            case Some(x) => $(Evaluation((x + math.random() * 4).round.toInt, "hard"))
            case None => new GameEvaluation(self).eval(a)
        }
    }

    def value(action : Action) : |[Double] = action.unwrap match {
        case _ : Unavailable => |(-1000000)
        case CancelAction => |(-100000)

        case a : PlayCardAction => |(turnValue(a))
        case a : EndTurnAction => |(turnValue(a))
        case a : WaitCardAction => |(turnValue(a))
        case a : ReplaceCardAction => |(turnValue(a))
        case a : RemoveCardAction => |(turnValue(a))
        case a : UpgradeCardAction => |(turnValue(a))
        case a : PassAction => |(turnValue(a))

        case PickCardAction(_, c) => |(pickValue(c))
        case FutureSightAction(_, c, _) => |(pickValue(c))

        case KeepDrawnAction(_, c, _, _, _, _) => |(utility(c) + cardValueSafe(c))
        case DiscardDrawnAction(_, c, _, _, _, _) => |(-utility(c) - cardValueSafe(c))
        case ReturnDrawnAction(_, c, _, _, _, _) => |(utility(c))
        case NegotiationTakeAction(_, c, _) => |(utility(c) + cardValueSafe(c))
        case NegotiationDrawAction(_, _) => |(90)

        case FeastChoiceAction(_, e, _) => |(effectGain(e))

        // SETUP
        case SetupTurnAction(_, _, _, tile, spot, r) => |(setupPlacement(tile, spot, r))
        case SetupUnitsAction(_, _, _, area) => |(gain(game.addUnits(area, self, 3)))
        case SetupKaijaAction(_, _, _, area) => |(gain { game.addUnits(area, self, 2) ; game.setCompanion(self, |(area)) })
        case SetupChiefAction(_, _, _, area, kaija) => |(gain { game.addUnits(area, self, kaija.?(1).|(2)) ; game.chiefs += self -> area ; if (kaija) game.setCompanion(self, |(area)) })
        case ReturnUnitsToAction(_, area, _) => |(gain(game.addUnits(area, self, 3)))
        case SecondChanceTurnAction(_, tile, spot, r, _) => |(setupPlacement(tile, spot, r))

        // RECRUIT
        case RecruitPlaceAction(_, area, _, _, placed, _) =>
            val t = board.territory(area)
            val camp = (game.working(t).has(TrainingCamp) && placed.exists(p => t.areas.has(p)).not).??(45)
            |(gain(game.addUnits(area, self, 1)) + camp)
        case RecruitKaijaAction(_, area, _, _, _, _) => |(gain(game.setCompanion(self, |(area))) + 20)
        case RecruitChiefAction(_, area, _, _, _, _) => |(gain(game.chiefs += self -> area) + 20)
        case RecruitDoneAction(_, _, _) => |(0)
        case RecruitCapAction(_, area, _, _, _) => |(gain(game.addUnits(area, self, 1)))

        // MOVE
        case MoveUnitsAction(_, from, to, n, kaija, chief, cost, left, e, _) =>
            val t = board.territory(from)
            val o = board.territory(to)
            val attack = game.present(o).exists(game.enemy(self, _))
            val g = gain {
                moveFigures(t, to, n, kaija, chief)
            }
            // The card's bonus; more moves left can bring more units in
            val bonus = attack.??(e.bonus * 40 + (left - cost > 0).??(30))
            // Hold an attack against a clan that can still answer, unless it's worth a lot
            val wait = (attack && game.present(o).exists(f => game.enemy(self, f) && f.passed.not) && g < 250).??(20)
            |(g + bonus - wait)
        case MoveDoneAction(_, _, _) => |(5)

        // FIGHTS
        case CombatFoodAction(_, attacker, defender, area, e, food, _) => |(foodChoice(attacker, defender, area, e, food))
        case CombatChooseAction(s, attacker, defender, area, e, food, faces, choice, _) => |(faceChoice(s, attacker, defender, area, e, food, faces, choice))
        case AxeAction(_, defender, area, e, food, face, _) => |(faceChoice(self, self, defender, area, e, food, $, face))
        case IntimidateAction(_, area, to, _, _) => |(60)
        case IntimidateSkipAction(_, _, _, _) => |(0)
        case RetreatToAction(_, from, to, n, kaija, _, _) =>
            val t = board.territory(from)
            |(gain {
                game.removeUnits(t, self, n)
                game.addUnits(to, self, n)
                if (kaija)
                    game.setCompanion(self, |(to))
            } + n * 10)

        // BUILD
        case BuildConfirmAction(_, area, b, space, cost, _, e, _, _) => |(build(area, b, space, cost))
        case BuildPlaceAction(_, area, b, space, cost, _, e, _, _) => |(build(area, b, space, cost))
        case BuildSpaceAction(_, area, b, space, _, cost, _, e, _, _) => |(build(area, b, space, cost))
        case BuildDoneAction(_, _, _) => |(0)

        // EXPLORE
        case ExploreTurnAction(_, tile, spot, r, _, _, _) => |(explorePlacement(tile, spot, r))
        case ExploreRedrawAction(_, _, _, _, _) => |(70)

        // HARVEST
        case TradeForAction(_, pay, r, _) => |(gain { pay.foreach(x => self.gain(x, -1)) ; self.gain(r, 1) })
        case DoneAction(_) => |(0)

        // SNAKE
        case ScorchedPlaceAction(_, area, _) =>
            val t = board.territory(area)
            val (f, w, l) = game.produce(t)
            |(20 + (f + w + l) * 25 + game.present(t).exists(game.enemy(self, _)).??(30))
        case ScorchedSkipAction(_, _) => |(0)
        case ScorchedTakeAction(_, r) => |(r./(resourceValue).|(0.0))

        // CARD EFFECTS
        case PlunderAction(_, area, enemy, _) => |(removeEnemy(area, enemy))
        case CaptureAction(_, area, enemy, _) => |(removeEnemy(area, enemy))
        case RaidAction(_, area, enemy, _) => |(removeEnemy(area, enemy))
        case CaptureAddAction(_, area, _) => |(gain(game.addUnits(area, self, 1)))

        case DefensiveCancelAction(_, f, card, _) =>
            val lead = leader.has(f).??(40)
            |(utility(card) - 90 + lead + card.effect.is[MoveEffect].??(40))
        case DefensiveAllowAction(_, _, _, _, _) => |(0)

        // CREATURES
        case CreatureAttackAction(_, area, c, e, _, _) =>
            val t = board.territory(area)
            val (p, lost) = HardCombat.creatureWin(game.strength(t, self, true) + e.bonus + spendable(self) / 2, c.kind.value, game.figures(t, self), true)
            |(p * (100 * c.kind.fame + 60 + 0.3 * territoryWorth(t, self)) - lost * 45 - 20)
        case CreatureDeclareDoneAction(_, _, _) => |(0)
        case CreatureFoodAction(_, area, c, e, attacking, food, _) =>
            val t = board.territory(area)
            val here = game.working(t)
            val base = game.strength(t, self, attacking) + attacking.??(e.bonus) + attacking.not.??(2 * here.count(_ == Fortress)) + (self == Snake && game.scorchedIn(t)).??(1)
            val (p, _) = HardCombat.creatureWin(base + food, c.kind.value, game.figures(t, self), attacking)
            val stake = 100 * c.kind.fame + 80 + attacking.not.?(territoryWorth(t, self)).|(0.0)
            val (needFood, _) = Winter.cost(self.units)
            val left = self.food - food
            |(p * stake - food * resourceValue(Food) - (left < needFood).??((needFood - left) * 60))
        // A creature tied between territories, or a new one: away from the bot, towards its opponents
        case CreatureMoveChoiceAction(_, _, to, _) => |(creatureSpot(to))
        case MoreCreaturesAction(_, area, _) => |(creatureSpot(area))
        case MoreCreaturesSkipAction(_, _) => |(0)
        case SpectralPlaceAction(_, area) => |(creatureSpot(area))
        case SpectralSkipAction(_) => |(0)

        // WARCHIEFS
        case BrandAction(_, _, _, _, _) => |(30)
        case BrandSkipAction(_, _, _, _) => |(0)
        case SignyAction(_, _, _, _) => |(25)
        case SignySkipAction(_, _, _, _) => |(0)

        // OTHER CARDS AND EXPANSIONS
        case TeamworkChoiceAction(_, e, _, _) => |(effectGain(e))
        case CopyEffectAction(_, _, card, _) => |(effectGain(card.effect))
        case HiddenUnitsAction(_, from, to, n, kaija, chief, _) => |(gain(moveFigures(board.territory(from), to, n, kaija, chief)))
        case MercenariesPayAction(_, pay, area, _) => |(gain { game.addUnits(area, self, 1) ; pay.foreach(r => self.gain(r, -1)) })
        case RaidCollectAction(_, r, _) => |(resourceValue(r))
        case HarvestExtraAction(_, r, _) => |(resourceValue(r))
        case KrakenCollectAction(_, r, _) => |(resourceValue(r))
        case QualityTakeAction(_, _, r, _, _) => |(resourceValue(r))
        case SquirrelFameAction(_, n, _) => |(100.0 * n)
        case EconomicsPayAction(_, n, _) => |(n * (75 - resourceValue(Food)))
        case JotunnPayAction(_, _, pay, _) => |(-pay./(resourceValue).sum)
        case EventGainAction(_, _, r, fame, draw, _, _, _) => |(r./(resourceValue).|(0.0) + 100.0 * fame + 70.0 * draw)
        case SpyAction(_, enemy, _) => |(100 * enemyWeight(enemy))
        case RapaciousAction(_, enemy, _) => |(100 * enemyWeight(enemy))
        case VeiledDiscardAction(_, enemy, _) => |(80 * enemyWeight(enemy) + 20)
        case VeiledDrawAction(_, _) => |(60)
        case SpyDiscardAction(_, _, card, _) => |(utility(card))
        case RapaciousPickAction(_, _, card, _) => |(utility(card))
        case CurseDiscardAction(_, _, card, _, _) => |(-cardValueSafe(card) - 0.3 * utility(card))
        case RapaciousDiscardAction(_, _, card, _) => |(-cardValueSafe(card) - 0.3 * utility(card))
        case RapaciousGiveAction(_, _, _, pay, _) => |(-pay./(resourceValue).sum)
        case EruptTargetAction(_, target, _) => |(100 * enemyWeight(target) - game.allied(self, target).??(1000))

        case _ => None
    }

    // Where a creature does the most harm to the bot's opponents and the least to the bot
    def creatureSpot(area : AreaRef) : Double = {
        val t = board.territory(area)
        val near = t +: adjacent(t).map(_._1)
        near./(o => game.present(o)./(f => (f == self).?(-1.0).|(game.allied(self, f).?(-0.7).|(enemyWeight(f) * 1.5)) * (game.figures(o, f) * 10 + territoryWorth(o, f) * 0.2)).sum).sum
    }

    def cardValueSafe(c : Card) : Double = try { cardValue(c) } catch { case e : Throwable => 0 }

    def removeEnemy(area : AreaRef, enemy : Faction) : Double = {
        val t = board.territory(area)
        val units = game.units
        try {
            val before = total
            game.removeUnits(t, enemy, 1)
            total - before + 10
        }
        finally {
            game.units = units
        }
    }

    def build(area : AreaRef, b : Building, space : SpaceRef, cost : Int) : Double = {
        val t = board.territory(area)
        gain {
            game.buildings += space -> b
            self.wood -= cost
            if (self == Goat)
                self.food += b.large.?(2).|(1)
        } + buildingBonus(b, t)
    }

    // Setup: the tile, and the best empty territory of it for the three units; valued cheaply (there are many
    // spots and turns): the territory's worth, next to the clan's other group (one Move joins them, BGG advice),
    // and open, to explore from
    def setupPlacement(tile : String, spot : Spot, r : Int) : Double = {
        board.withPlaced(Placement(tile, spot.x, spot.y, r)) {
            val l = board.territories.%(t => t.areas.exists(a => a.x == spot.x && a.y == spot.y)).%(t => game.present(t).none)
            val mine = game.controlled(self)
            l./(t => territoryWorth(t, self) + adjacent(t).exists(x => mine.has(x._1)).??(60) + board.open(t).??(30) - adjacent(t).exists(x => game.present(x._1).exists(game.enemy(self, _))).??(40)).maxOr(-50.0)
        }
    }
}
