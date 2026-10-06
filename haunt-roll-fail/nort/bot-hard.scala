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
// against the bot, the leader's more, so it attacks a clan that is about to win. With Uncharted Horizons' Alternative
// victory, progress towards the cards in play counts the same way (victoryValue).
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
// - pass early when the hand has little left, to take the best Development card;
// - send creatures towards the other players and away from your own units ("Dealing with Creatures" thread):
//   used when the bot breaks a tie in the Creature phase or places a new creature or Spectral Warrior.

class BotHard(f : Faction) extends EvalBot {
    def eval(actions : $[UserAction])(implicit game : Game) : Compute[$[ActionEval]] = {
        if (f == Automa || game.states.contains(f).not)
            return new BotXX(f).eval(actions)

        if (game.training) {
            val ev = new TrainingEvaluation(f, 7)
            return actions./{ a => ActionEval(a, ev.eval(a)) }
        }

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

    // Actions valued the Easy way because the Hard bot has no score for them, by class (host.scala prints them)
    val unvalued = scala.collection.mutable.Map[String, Int]()

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

        // Creatures ignore a Robotos clan
        if (game.has(Creatures) && game.robotos(f).not)
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

    // ALTERNATIVE VICTORY (Uncharted Horizons): how far f is towards each card in play, from 0 to 1;
    // extra: validation counts f would add (a building, a trade, a closed territory, ...)
    def victoryProgress(f : Faction, c : VictoryCard, extra : Map[String, Int]) : Double = {
        def frac(x : Double, n : Double) = math.max(0.0, math.min(1.0, x / n))
        val mine = game.controlled(f)
        val closed = mine.%(board.closed)
        def large(t : Territory) = game.buildingsIn(t).exists(_._2.large)
        def lair(t : Territory) = t.areas.exists(a => board.spec(a).lair)

        c.target match {
            case Some(n) => frac(game.progressOf(f, c.id) + extra.getOrElse(c.id, 0), n)
            case None => c.id match {
                case "many-territories" => frac(closed.num + 0.3 * (mine.num - closed.num), 6)
                case "large-buildings" => frac(mine./~(game.buildingsIn).count(_._2.large), 4)
                case "spreading" => frac(mine.num, 8)
                case "creature-territories" => frac(closed.count(lair) + 0.3 * mine.%(board.open).count(lair), 3)
                case "vast-territory" => mine./(t => math.min(1.0, board.tiles(t) / 6.0) * board.closed(t).?(1.0).|(0.7) * large(t).?(1.0).|(0.7)).maxOr(0.0)
                case "large-territories" => frac(closed.count(large) + 0.4 * mine.count(t => board.closed(t) != large(t)), 3)
                case "two-larger-territories" => frac(mine./(t => math.min(1.0, board.tiles(t) / 5.0) * board.closed(t).?(1.0).|(0.7) * large(t).?(1.0).|(0.6)).sortBy(-_).take(2).sum, 2)
                case "mountains" => frac(closed.count(VictoryExpansion.rough) + 0.3 * mine.%(board.open).count(VictoryExpansion.rough), 6)
                case "knowledge" =>
                    val u = VictoryExpansion.upgrades(f)
                    if (game.has(Warchiefs)) frac(u, 3) else 0.7 * frac(u, 2) + 0.3 * frac(f.lore, 3)
                case "prosperity" => frac(f.fame, 50)
                case "population" => frac(game.onMap(f), game.unitLimit - 1) * (f.unrest == 0).?(1.0).|(0.5)
                case "production" => (math.min(f.food, 5) + math.min(f.wood, 5) + math.min(f.lore, 5)) / 15.0
                case "building-ownership" => frac(mine./~(game.buildingsIn).num, 9)
                case _ => 0.0
            }
        }
    }

    // Thane: one Map Control card and one Wealth card; Jarl: all of them. Winning is worth as much as the three territories
    def victoryValue(f : Faction, extra : Map[String, Int] = Map()) : Double = {
        if (game.has(VictoryModule).not || game.victory.none)
            return 0

        val all = game.victory./(c => victoryProgress(f, c, extra))
        def of(map : Boolean) = game.victory.zip(all).filter(_._1.mapControl == map).map(_._2)

        if (options.has(VictoryModeOption(true))) {
            if (all.forall(_ >= 1)) 6000.0
            else all./(p => 500 * p * p).sum + 1500 * all.product
        }
        else {
            val m = of(true).maxOr(0.0)
            val w = of(false).maxOr(0.0)
            if (m >= 1 && w >= 1) 6000.0
            else 600 * (m * m + w * w) + 2000 * m * w
        }
    }

    lazy val victorySteps = scala.collection.mutable.Map[String, Double]()

    // What one more validation count (a building, a trade, ...) is worth to the bot
    def victoryStep(id : String, n : Int = 1) : Double =
        if (game.has(VictoryModule).not || game.victory.exists(_.id == id).not) 0.0
        else victorySteps.getOrElseUpdate(id + n, victoryValue(self, Map(id -> n)) - victoryValue(self))

    // Food and wood f owes at Winter: nothing for a Robotos clan (robotos.scala)
    def winterNeed(f : Faction) : (Int, Int) = game.robotos(f).?((0, 0)).|(Winter.cost(f.units))

    // Food and wood owed at the next Winter, against what f will have; an Unrest card costs 5 fame
    def winterValue(f : Faction) : Double = {
        val (needFood, needWood) = winterNeed(f)
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

    // The points enemy e could bring against t: its biggest group next to t, and half of the rest; a group that must
    // cross a Rough border (2 moves) counts half. The bot's own units attacking somewhere (sharing a territory, while
    // its moves are tried) count for nothing: they are about to fight; the defenders there still count
    // Who is in each territory, worked out once per position tried (the threats look at every territory's neighbours)
    // (the game replaces these maps and lists when they change, so comparing references is enough)
    private var occupantsKey : $[AnyRef] = null
    private var occupants : Map[Territory, $[(Faction, Int)]] = Map()

    def figuresIn(t : Territory) : $[(Faction, Int)] = {
        val key : $[AnyRef] = $(game.units, game.chiefs, game.kaija, game.lynx, game.brok, game.leader2, board.placements, game.blainn)
        if (occupantsKey == null || occupantsKey.zip(key).exists { case (a, b) => a ne b }) {
            occupants = board.territories./(o => o -> game.present(o)./(f => f -> game.figures(o, f))).toMap
            occupantsKey = key
        }
        occupants.getOrElse(t, $)
    }

    // Groups two territories away count 0.4: they can take the territory in between first, and the bot should not wait
    // for them to arrive before reinforcing (an Altar of Kings behind a weak border, for one)
    def threat(t : Territory, e : Faction) : Int = {
        def here(o : Territory) : Double = {
            val l = figuresIn(o)
            if (e == self && l.num > 1) 0.0 else l.find(_._1 == e)./(_._2.toDouble).|(0.0)
        }
        val near = adjacent(t)
        val groups = near./{ case (o, regular) => regular.?(1.0).|(0.5) * here(o) }
        val far = near./~{ case (o, _) => adjacent(o).map(_._1).filter(x => x != t && near.exists(_._1 == x).not) }.distinct./(x => 0.4 * here(x))
        val all = (groups ++ far).%(_ > 0).sortBy(-_)
        (all.take(1).sum + all.drop(1).sum / 2).round.toInt
    }

    def enemiesOf(f : Faction) : $[Faction] = players.%(g => game.enemy(f, g))

    def defense(t : Territory, f : Faction) : (Int, Int) = {
        val here = game.working(t)
        (game.strength(t, f, false) + 2 * here.count(_ == Fortress) + (f == Snake && game.scorchedIn(t)).??(1), here.count(_ == DefenseTower))
    }

    // What f stands to lose in t: each enemy near it may attack (much less likely once that enemy has passed), with
    // the exact odds of the fight; the chances of holding against each are combined, so reinforcing against the
    // nearest danger counts even when a bigger army is also next door. (Making attacks likelier on valuable
    // territories as well made the bot too defensive: it lost 61 of 100 two-player games to the version without it.)
    def risk(t : Territory, f : Faction, worth : Double) : Double = {
        val (ds, towers) = defense(t, f)
        val du = game.figures(t, f)
        var hold = 1.0
        enemiesOf(f).foreach { e =>
            val str = threat(t, e)
            if (str > 0) {
                // A card bonus or food is likely
                val p = HardCombat.attackWin(str + 1, ds + math.min(du, f.food), str, du, towers)
                val likely = e.passed.?(0.12).|(0.4) * (harvests <= 1).?(1.2).|(1.0)
                hold *= 1 - math.min(1.0, likely * p)
            }
        }
        (1 - hold) * (worth + 30 * du)
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

        v += 100.0 * f.fame

        v += victoryValue(f)

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
        val (needFood, _) = winterNeed(f)
        val fc = harvestAhead.??(try { Harvest.forecast(f).food } catch { case e : Throwable => 0 })
        math.max(0, math.min(f.food, f.food + fc - needFood))
    }

    // TRYING THINGS: change the game, value it, put everything back
    def trying(change : => Unit) : Double = {
        val units = game.units
        val buildings = game.buildings
        val chiefs = game.chiefs
        val companion = game.companion(self)
        // Every player's resources and fame (team trades and some effects change another player's)
        val stocks = players./(f => (f, f.food, f.wood, f.lore, f.fame))
        try {
            change
            total
        }
        finally {
            game.units = units
            game.buildings = buildings
            game.chiefs = chiefs
            game.setCompanion(self, companion)
            stocks.foreach { case (f, food, wood, lore, fame) =>
                f.food = food
                f.wood = wood
                f.lore = lore
                f.fame = fame
            }
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
                        // Robotos: one more unit with the Recruit
                        sum += g + (placed.none && game.robotos(self)).??(38)
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
            val lairs = (game.has(Creatures) && game.robotos(self).not).??(Tiles(tile).areas.count(_.lair) * 50)
            value(self) - base + fame * 100 + raven + stag + boar - lairs - gift * 40 + closedNow.any.?(victoryStep("exploration", closedNow.num)).|(0.0)
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

        case RemoveCardAction(_, c) => 150 - 2 * resourceValue(Lore) - cardValue(c) + thin(c) + victoryStep("refinement")

        case UpgradeCardAction(_, c, u, remove) => upgradeValue(u) - 3 * resourceValue(Lore) - cardValue(c) + remove.?(thin(c)).|(0.0) + victoryStep("refinement")

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
        val (needFood, _) = winterNeed(self)
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
            case None =>
                val k = a.unwrap.getClass.getSimpleName
                HardEvaluation.unvalued(k) = HardEvaluation.unvalued.getOrElse(k, 0) + 1
                new GameEvaluation(self).eval(a)
        }
    }

    // A choice that only goes on to the next step (Done, Skip, "Take no token", "No Raid", ...) is worth nothing in itself
    def value(action : Action) : |[Double] = action.unwrap match {
        case _ : UserAction => valueOf(action)
        case _ if action.is[UserAction] && action.unwrap.is[ForcedAction] => |(0)
        case _ => valueOf(action)
    }

    def valueOf(action : Action) : |[Double] = action.unwrap match {
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
        case RecruitUnitHereAction(f, area, left, mode, placed, then) => value(RecruitPlaceAction(f, area, left, mode, placed, then))
        case RecruitKaijaHereAction(f, area, left, mode, placed, then) => value(RecruitKaijaAction(f, area, left, mode, placed, then))
        case RecruitChiefHereAction(f, area, left, mode, placed, then) => value(RecruitChiefAction(f, area, left, mode, placed, then))
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
            val conquest = attack.?(0.6 * victoryStep("conquest")).|(0.0)
            |(g + bonus - wait + conquest)
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
        case BuildConfirmAction(_, area, b, space, cost, _, e, _, _) => |(build(area, b, space, cost) + victoryStep("architecture"))
        case BuildPlaceAction(_, area, b, space, cost, _, e, _, _) => |(build(area, b, space, cost) + victoryStep("architecture"))
        case BuildSpaceAction(_, area, b, space, _, cost, _, e, _, _) => |(build(area, b, space, cost) + victoryStep("architecture"))
        case BuildDoneAction(_, _, _) => |(0)
        // Industrious Villagers: swap a building for another of the same size
        case ReplaceBuildingAction(_, _, space, b, _) => |(gain(game.buildings += space -> b))
        case ReplaceSkipAction(_, _) => |(0)

        // EXPLORE
        case ExploreTurnAction(_, tile, spot, r, _, _, _) => |(explorePlacement(tile, spot, r))
        case ExploreRedrawAction(_, _, _, _, _) => |(70)

        // HARVEST
        case TradeForAction(_, pay, r, _) => |(gain { pay.foreach(x => self.gain(x, -1)) ; self.gain(r, 1) } + victoryStep("trading"))
        // A teammate's resources count too; a small cost keeps trades from going back and forth
        case TeamTradeAction(_, mate, give, take, _) => |(gain { self.gain(give, -1) ; mate.gain(give, 1) ; mate.gain(take, -1) ; self.gain(take, 1) } + victoryStep("trading") - 15)
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
            |(p * (100 * c.kind.fame + 60 + 0.3 * territoryWorth(t, self) + victoryStep("hunting")) - lost * 45 - 20)
        case CreatureDeclareDoneAction(_, _, _) => |(0)
        case CreatureFoodAction(_, area, c, e, attacking, food, _) =>
            val t = board.territory(area)
            val here = game.working(t)
            val base = game.strength(t, self, attacking) + attacking.??(e.bonus) + attacking.not.??(2 * here.count(_ == Fortress)) + (self == Snake && game.scorchedIn(t)).??(1)
            val (p, _) = HardCombat.creatureWin(base + food, c.kind.value, game.figures(t, self), attacking)
            val stake = 100 * c.kind.fame + 80 + attacking.not.?(territoryWorth(t, self)).|(0.0)
            val (needFood, _) = winterNeed(self)
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
        case BriberyUnitsAction(_, from, enemy, to, n, _) => |(gain { game.removeUnits(board.territory(from), enemy, n) ; game.addUnits(to, enemy, n) })
        case AnnexationOrderAction(_, exploreFirst, _) => |(exploreFirst.??(5))
        case AnnexationExploreYesAction(_, _) => |(exploreGain(ExploreEffect()))

        // REROLLS: Liv (Raven's warchief) and Ox's token 3, from the exact odds of the face kept against a new roll
        case LivKeepAction(_, keep) => |(keepOdds(keep))
        case LivRerollAction(_, again) => |(rerollOdds(again))
        case GearKeepAction(_, keep) => |(keepOdds(keep))
        case GearRerollAction(_, again) => |(rerollOdds(again))
        // Liv's Cunning: wood or lore spent like food, while it still helps
        case LivCunningAction(_, defender, area, e, r, spent, _) => |((spent.num < game.figures(board.territory(area), self)).?(cunning(area, spent.num)).|(-100.0) - resourceValue(r))
        case CreatureCunningAction(_, area, c, e, r, spent, _) => |((spent.num < game.figures(board.territory(area), self)).?(cunning(area, spent.num)).|(-100.0) - resourceValue(r))

        // The order of the fights: the best odds first
        case FightAction(_, area, e, _) =>
            val t = board.territory(area)
            |(game.present(t).but(self).headOption./(o => 10 * fightOdds(t, self, o, e.bonus)).|(0.0))
        case CreatureFightAction(_, _, _, _, _) => |(0)
        case LivCunningDoneAction(_, _, _, _, _, _) => |(0)
        case CreatureCunningDoneAction(_, _, _, _, _, _) => |(0)
        case HalvardCraftSkipAction(_, _) => |(0)
        // Brand's Bravery: the beaten enemy retreats where it does them the least good
        case BrandRetreatToAction(_, loser, from, to, n, kaija, _, _) => |(gain { game.removeUnits(board.territory(from), loser, n) ; game.addUnits(to, loser, n) })

        // OX (New Blood): Ancestral Equipment tokens
        case GearTakeAction(_, _, n, _) => |(gearValue(n) + 10)
        case GearUseAction(_, n, area, _, _, _) => |(gearUse(n, area))
        case TrueHeroAction(_, space, _, _) => |(gain(game.buildings -= space))

        // OTHER NEW BLOOD CLANS
        case SacrificeAction(_, owner, _) => |((owner == self).?(-50.0).|(60.0 + 40 * enemyWeight(owner)))
        case PyrePlaceAction(_, area, _) => |(gain(game.removeUnits(board.territory(area), self, 1)) + 50)
        case PyreReturnAction(_, area, _) => |(gain(game.addUnits(area, self, 1)))
        case SacrificeCaptureAction(_, area, enemy, _) => |(removeEnemy(area, enemy))
        case TidePlaceAction(_, area, _) =>
            val t = board.territory(area)
            val (f, w, l) = game.produce(t)
            |(20 + (f + w + l) * 20 + game.controlled(self).has(t).??(30))
        case KaraTideAction(_, _, _, _) => |(10)
        case EndlessTideAction(_, area, enemy, _) => |(removeEnemy(area, enemy) + resourceValue(Lore))
        case KnowledgeChoiceAction(_, _, _, r, _, _, _) => |(r./(resourceValue).|(70.0))
        case RatRemoveAction(_, area, _, _) => |(gain { game.removeUnits(board.territory(area), self, 1) ; self.wood += 1 })
        case OverworkRemoveAction(_, area, _) => |(gain(game.removeUnits(board.territory(area), self, 1)))
        case OverworkCollectAction(_, area, _) => |(produceValue(board.territory(area)))
        case CraftsmenReplaceAction(_, _, space, b, _) => |(gain(game.buildings += space -> b))
        case HorseWoodAction(_, _, _, _) => |(resourceValue(Wood))
        case QualityBuildAction(_, _, _) => |(40)
        case PrecisionPayAction(_, r, _, _) => |(70 - resourceValue(r))
        case LynxUnitsAction(_, _, n, _) => |(5.0 * n)

        // WASTELANDS: Kobold and Kobold Camp exchanges, Jötunn Blainn, the volcano, Mimirsbrunn, the Gate of Helheim, Vedrfolnir
        case KoboldSwapAction(_, give, _, _, _, _) => |(gain { self.gain(give, -1) ; self.gain((give == Food).?(Wood).|(Food), 1) })
        case CampTradeAction(_, give, g, _, _, _, _) => |(gain { self.gain(give, -1) ; g match { case Some(x) => self.gain(x, 1) ; case None => self.fame += 1 } })
        case WasteTradeDoneAction(_, _, _) => |(0)
        case BlainnRecruitAction(_, area, _) => |(gain(self.food -= 1) + 180)
        case BlainnSkipAction(_, _, _) => |(0)
        case EruptUnitAction(_, area, _, _) => |(gain(game.removeUnits(board.territory(area), self, 1)))
        case MimirTakeAction(_, c) => |(pickValue(c) - 0.4 * game.display./(pickValue).maxOr(0.0))
        case MimirSkipAction(_) => |(0)
        case GatePickAction(_, c, _) => |(-20.0 * c.kind.value)
        case MyrkalfChoiceAction(_, _, to, _, _, _) => |(creatureSpot(to))
        case VedrfolnirAction(_) => |(40)
        case VedrfolnirSkipAction(_) => |(0)
        case VedrfolnirTurnAction(_, tile, spot, r) => |(explorePlacement(tile, spot, r))

        // WILDERNESS
        case GeyserPlaceAction(_, area, _) => |(gain(game.addUnits(area, self, 1)))
        case GeyserSkipAction(_, _) => |(0)
        case EldthursAction(_, _, space, _, _) => |(gain(game.buildings -= space))

        // EVENTS (Uncharted Horizons)
        case OfferingsTakeAction(_, card, _, _, _) => |(utility(card) + cardValueSafe(card) - resourceValue(Lore))
        case BonfireTradeAction(_, _) => |(gain { self.wood -= 2 ; self.lore += 1 ; self.fame += 1 } + victoryStep("trading"))
        case VolcanoAction(_, space, b, _, _, _) => |(gain(game.buildings -= space) + 70 * b.large.?(2).|(1))
        case HappyUnrestAction(_, _, _, _) => |(500)
        case HappyFameAction(_, area, _, _, _) => |(300 + area./(a => gain(game.addUnits(a, self, 1))).|(0.0))
        case LevyAction(_, area, remove, _, _, _) =>
            val t = board.territory(area)
            |(remove.?(gain(game.removeFigures(t, self, 1))).|(-produceValue(t)))
        case BountifulAction(_, area, _, _, _) =>
            val t = board.territory(area)
            |(produceValue(t) - board.closed(t).??((board.tiles(t) >= 3).?(200).|(100)))
        case InfestationChoiceAction(_, area, remove, _, _, _, _) =>
            |(remove.?(gain(game.removeFigures(board.territory(area), self, 1))).|(100 - resourceValue(Food)))
        case SailorAction(_, area, _, _, _) =>
            val t = board.territory(area)
            val n = math.min(2, game.count(t, self))
            |(gain { game.removeUnits(t, self, n) ; self.fame += 2 * n })
        case KrakenAttackAction(_, area, _, _, _) => |(gain { game.removeFigures(board.territory(area), self, 1) ; self.wood -= math.min(1, self.wood) })
        case EarthquakeRemoveAction(_, space, _, _, _, _, _) => |(gain(game.buildings -= space))

        // SEA (Uncharted Horizons): Raids from the Ports
        case RaidDrawAction(_, _, _) => |(90)
        case RaidPickAction(_, _, c, _, _) => |(raidValue(c, 2))
        case RaidSendAction(_, port, c, n, _) => |(gain(game.removeUnits(board.territory(port), self, n)) + 0.85 * raidValue(c, n))
        case RaidCompleteAction(_, _, c, years, n, act, _) => |(act.?(raidAction(c, years)).|(raidGain(c.gain(years, n))))
        case RaidContinueAction(_, _, c, _) => |(0.75 * math.max(raidGain(c.gain(2, 2)), raidAction(c, 2)) - 60)
        case RaidAnyPickAction(_, r, _, _) => |(gain(self.gain(r, 1)))
        case RaidRemoveCardAction(_, _, c, _, _, _, _, _) => |((c == UnrestCard).?(500.0).|(thin(c) + 20))
        case RaidRemoveDoneAction(_, _, _, _) => |(0)
        case RaidUpgradeAction(_, _, u, _) => |(upgradeValue(u))
        case RaidOutpostAction(_, _, area, _, _) => |(gain(game.addUnits(area, self, 1)))
        case RaidBuildPlaceAction(_, _, space, b, _, _, _, _, _) => |(build(space.area, b, space, 0))
        case RaidStoneAction(_, _, area, _, _) => |(horizon * 50)
        case RaidClearUnitAction(_, _, area, enemy, _, _) => |(removeEnemy(area, enemy))
        case RaidReinforceToAction(_, _, area, _, _, _) => |(gain(game.addUnits(area, self, 1)))
        case RaidShiftMoveAction(_, _, from, to, n, _, _, _) => |(gain(moveFigures(board.territory(from), to, n, false, false)))
        case RaidRazeAction(_, _, space, _, _) => |(gain(game.buildings -= space))
        case RaidStealAction(_, _, area, r, _, _, _) => |(r./(resourceValue).|(produceValue(board.territory(area)) + 150.0))
        case RaidExchangeAction(_, _, give, take, fame, _, _, _, _, _) => |(gain { self.gain(give, -1) ; self.gain(take, 1) ; if (fame) self.fame += 1 })
        case RaidFortuneAction(_, _, r, n, _, _, _) => |(gain { self.gain(r, -n) ; self.fame += n })
        case RaidSkipKeptAction(_, _, _, _, _) => |(0)

        case _ => None
    }

    // Where a creature does the most harm to the bot's opponents and the least to the bot
    // (BGG "Dealing with Creatures": players steer creatures towards each other)
    def creatureSpot(area : AreaRef) : Double = {
        val t = board.territory(area)
        val near = t +: adjacent(t).map(_._1)
        near./(o => game.present(o)./(f => (f == self).?(-1.0).|(game.allied(self, f).?(-0.7).|(enemyWeight(f) * 1.5)) * (game.figures(o, f) * 10 + territoryWorth(o, f) * 0.2)).sum).sum
    }

    // What a territory's resources are worth once
    def produceValue(t : Territory) : Double = {
        val (f, w, l) = game.produce(t)
        f * resourceValue(Food) + w * resourceValue(Wood) + l * resourceValue(Lore)
    }

    // RAIDS: what a Raid card's resources are worth, and a rough worth of its actions
    def raidGain(g : RaidGain) : Double = g.food * resourceValue(Food) + g.wood * resourceValue(Wood) + g.lore * resourceValue(Lore) + 100.0 * g.fame + 50.0 * g.any

    def raidAction(c : RaidCard, years : Int) : Double = (c.action(years).none).?(0.0).|((c.id, years) match {
        case ("raiders-reward", 1) => 50
        case ("raiders-reward", 2) => 350
        case ("heroic-homestead", 2) => 3 * resourceValue(Wood) + 150
        case ("elders-wisdom", 2) => 300
        case ("clear-the-frontlines", y) => 90.0 * y
        case ("conquerors-tribute", y) => 100.0 * y * math.min(5, enemies./~(game.controlled).count(board.open))
        case (_, 1) => 90
        case _ => 250
    })

    // The best a Raid with n units can give, with a second year discounted
    def raidValue(c : RaidCard, n : Int) : Double = {
        val one = math.max(raidGain(c.gain(1, n)), raidAction(c, 1))
        val two = (n >= 2).?(0.7 * math.max(raidGain(c.gain(2, 2)), raidAction(c, 2))).|(0.0)
        math.max(one, two)
    }

    // OX: an Ancestral Equipment token kept, and one spent in a fight (the points it adds, against the odds)
    def gearValue(n : Int) : Double = n match {
        case 4 => 90
        case 5 | 6 => 70
        case 7 => 55
        case 2 => 50
        case 3 => 45
        case _ => 40
    }

    def gearUse(n : Int, area : AreaRef) : Double = {
        val t = board.territory(area)
        val other = game.present(t).but(self).headOption
        other match {
            case None => -20
            case Some(o) =>
                val points = n match {
                    case 1 => (self.food > 0).?(1).|(0)
                    case 2 | 6 | 7 => 1
                    case 3 => 1
                    case 4 | 5 => 2
                    case _ => 1
                }
                val attacking = game.current.has(self)
                val p0 = attacking.?(fightOdds(t, self, o, 0)).|(1 - fightOdds(t, o, self, 0))
                val p1 = attacking.?(fightOdds(t, self, o, points)).|(1 - fightOdds(t, o, self, -points))
                (p1 - p0) * stake(t) - 0.5 * gearValue(n)
        }
    }

    // Liv's Cunning: what one more point is worth in the fight in area
    def cunning(area : AreaRef, spent : Int) : Double = {
        val t = board.territory(area)
        game.present(t).but(self).headOption match {
            case Some(o) =>
                val p0 = fightOdds(t, self, o, spent)
                val p1 = fightOdds(t, self, o, spent + 1)
                (p1 - p0) * stake(t)
            case None => 40
        }
    }

    // The bot's odds in a fight once its die is known (keep) or before a new roll (reroll)
    def keepOdds(keep : ForcedAction) : Double = keep match {
        case CombatFaceAction(attacker, defender, area, e, food, faces, face, _) => 1000 * rolledOdds(attacker, defender, area, e, food, faces, |(face))
        case CreatureFaceAction(_, area, c, e, attacking, food, face, _) => 1000 * creatureRolledOdds(area, c, e, attacking, food, |(face))
        case _ => 0
    }

    def rerollOdds(again : ForcedAction) : Double = again match {
        case CombatRerollAction(attacker, defender, area, e, food, faces, _) => 1000 * rolledOdds(attacker, defender, area, e, food, faces, None)
        case CreatureRerollAction(_, area, c, e, attacking, food, _) => 1000 * creatureRolledOdds(area, c, e, attacking, food, None)
        case _ => 0
    }

    // The attacker rolls first (faces empty), then the defender (faces holds the attacker's die); a "point or casualty" face is taken the better way
    def rolledOdds(attacker : Faction, defender : Faction, area : AreaRef, e : MoveEffect, food : $[Int], faces : $[DieFace], face : |[DieFace]) : Double = {
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
        val attacking = faces.none
        def choices(f : DieFace) = f.choice.?($(NorthgardDie.point, NorthgardDie.casualty)).|($(f))
        def mine(p : Double) = attacking.?(p).|(1 - p)
        def odds(f : DieFace) : Double =
            if (attacking) choices(f)./(x => HardCombat.attackWinGiven(as, ds, au, du, towers, extra, x, None)).max
            else choices(f)./(x => 1 - HardCombat.attackWinGiven(as, ds, au, du, towers, extra, faces(0), |(x))).max
        face match {
            case Some(f) => odds(f)
            case None => HardCombat.faces./(odds).sum / 6
        }
    }

    def creatureRolledOdds(area : AreaRef, c : Creature, e : MoveEffect, attacking : Boolean, food : Int, face : |[DieFace]) : Double = {
        val t = board.territory(area)
        val here = game.working(t)
        val base = game.strength(t, self, attacking) + attacking.??(e.bonus) + attacking.not.??(2 * here.count(_ == Fortress)) + (self == Snake && game.scorchedIn(t)).??(1) + food
        val units = game.figures(t, self)
        def odds(f : DieFace) : Double = {
            val ps = base + f.choice.?(1).|(f.points)
            HardCombat.faces./ { fc =>
                val cs = c.kind.value + fc.choice.?(1).|(fc.points)
                val pc = fc.choice.?(0).|(fc.casualties)
                (pc < units && attacking.?(ps > cs).|(ps >= cs)).?(1.0).|(0.0)
            }.sum / 6
        }
        face match {
            case Some(f) => odds(f)
            case None => HardCombat.faces./(odds).sum / 6
        }
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
