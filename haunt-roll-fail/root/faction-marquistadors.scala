package root
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
import root.elem._

// The Doomed Expansion's Dawn of the Marquistadors (slugfacekillah, v1.4; Law of Doom DM and its faction board), a re-imagining of
// the Marquise de Cat: invaders landing by ship, deploying only at the ship and their outposts, making treaties of cards from hand
// (to craft, to place outposts, and as warriors in battle), and hoarding the eight wood tokens of the forests back to the ship, each
// one scoring the number already in their Keep; all eight there wins the game.
//
// Readings (the rules leave these open):
// - Setup: always last in the standard setup (its priority); in Advanced Setup it sets up when drafted. The ship goes in the edge
//   clearing by the river with the fewest enemy pieces (a tie is the Marquistadors' pick); the wood is dealt round the forests not
//   adjacent to the ship's clearing, as evenly as can be, the Marquistadors picking which forests get one more. Enemies in the
//   ship's clearing move all their pieces there to one adjacent clearing they pick, in turn order (a building goes even if the
//   clearing has no free slot)
// - The track: the ship's space shows 2 warriors and each outpost space 1; the 1st, 3rd and 5th outpost spaces show a card. So
//   Deploy is 2 (with the ship on the map) plus 1 for each outpost on the map, and Evening draws 1 for each of the 1st, 3rd and
//   5th outposts on the map (none with no outpost)
// - Treaties are open, kept on the board. A bird treaty matches any clearing and pays for any crafting cost, as a bird card does
// - Treaties as warriors: the Marquistadors decide after the roll, in a battle they attack or defend in, in a clearing without an
//   outpost, with treaties matching it. Then every matching treaty counts as a warrior there (more rolled hits, and no Defenseless
//   hit), and when they take hits they discard treaties first, as many as they pick, before the rest of the hits go to pieces
// - Wood of the Woods: an enemy that removes a wood token scores for it as for any token, then puts it in a forest with the
//   fewest wood tokens (picking among the tied ones)
// - Hoard moves a wood token once: from a forest to an adjacent clearing they rule, or between adjacent clearings they rule. A
//   wood token moved into the ship's clearing scores the wood already in the Keep and goes there; with all eight there they win
// - Conquer's move is a normal move (rule of the origin or destination), and the battle after it is against a faction of their
//   choice in the destination, if there is one they can battle

trait Marquistadors extends WarriorFaction {
    val clashKey = DM

    val warrior = Marquistador

    def abilities(options : $[Meta.O]) = $(TheShip, TreatiesAsWarriors, WoodOfTheWoods, DaylightCraft)

    def pieces(options : $[Meta.O]) = Marquistador *** 25 ++ DMOutpost *** 6 ++ DMShip *** 1 ++ DMWood *** 8

    override def note : Elem = HorizontalBreak ~ "Doomed Expansion (fan-made)"

    def advertising = DMShip.img(this) ~ Marquistador.img(this) ~ DMOutpost.img(this) ~ DMWood.img(this)
    def motto = "Hoard".styled(this)
}

case object DM extends Marquistadors {
    val name = "Dawn of the Marquistadors"
    override def funName = "Dawn of the " ~ NameReference(name, this)
    val short = "DM"
    val style = "dm"
    val priority = "ZZ"
}

case object Marquistador extends Warrior {
    override def id = "Marquistador"
    override def name = "Marquistador"
}

case object DMOutpost extends Building {
    override def id = "Outpost"
    override def name = "Outpost"
}

case object DMShip extends Token {
    override def id = "Ship"
    override def name = "Ship"
}

case object DMWood extends Token {
    override def id = "Wood"
    override def name = "Wood"
    override def plural = "Wood"
}

case object DMKeep extends SpecialRegion {
    override def name = "Keep"
}

case object TheShip extends FactionEffect {
    val name = "The Ship"
}

case object TreatiesAsWarriors extends FactionEffect {
    val name = "Treaties"
}

case object WoodOfTheWoods extends FactionEffect {
    val name = "Wood of the Woods"
}

class MarquistadorsPlayer(val faction : Marquistadors)(implicit val game : Game) extends FactionState {
    board.forests.foreach(r => location(r))

    val keep = location(DMKeep, u => u.faction == faction && u.piece == DMWood)

    val treaties = cards("treaties")

    var acted = 0
    var extra = 0

    // treaties as warriors in the current battle
    var treatyDecided = false
    var treatyIn : |[Clearing] = None
    var adjusted : Action = null
    var hitsAsked : Action = null

    def ship = all(DMShip).starting
    def outposts = all(DMOutpost).num

    def deploy = ship.any.??(2) + outposts
    def draws = (outposts >= 1).??(1) + (outposts >= 3).??(1) + (outposts >= 5).??(1)

    // a bird treaty crafts as any suit
    def asset(d : DeckCard) : SuitAsset = d.suit @@ {
        case s : BaseSuit => s
        case _ => OneOf(FoxRabbitMouse)
    }

    def craft = treaties./(asset)

    def matching(c : Clearing) = treaties.%(_.matches(c.cost))
}

case class DMShipPlaceAction(self : Marquistadors, c : Clearing) extends BaseAction("Place", DMShip.of(self), DMShip.imgd(self), "in")(c)
case class DMLandAction(self : Marquistadors, c : Clearing) extends BaseAction("Land", DMShip.of(self), DMShip.imgd(self), "and six warriors in")(c)
case class DMWoodShuffledAction(self : Marquistadors, l : $[Forest], shuffled : $[Forest]) extends ShuffledAction[Forest]
case class DMClearShipAction(self : Marquistadors, c : Clearing, l : $[Faction]) extends ForcedAction
case class DMClearToAction(self : Faction, f : Marquistadors, c : Clearing, d : Clearing, l : $[Faction]) extends BaseAction(self, "moves all its pieces from the", DMShip.of(f), "clearing", c, "to")(d)

case class DMDeployAction(self : Marquistadors, c : Clearing, n : Int) extends BaseAction("Deploy".styled(self), (n + 1).hl, "more")(c)
case class DMDeployAskAction(self : Marquistadors, n : Int) extends ForcedAction

trait DMDaylightQuestion extends FactionAction {
    override def self : Marquistadors
    def question(implicit game : Game) = self.elem ~ SpacedDash ~ Daylight.elem
}

case class DMOutpostMainAction(self : Marquistadors, l : $[Clearing]) extends OptionAction("Place", DMOutpost.of(self), DMOutpost.imgd(self)) with DMDaylightQuestion with Soft
case class DMOutpostAction(self : Marquistadors, c : Clearing, a : DeckCard, b : DeckCard) extends BaseAction("Place", DMOutpost.of(self), "with two treaties")(c, a.img, b.img) with ViewCard { val d = a }

case class DMConquerBattleAction(self : Marquistadors, l : $[Clearing]) extends OptionAction("Conquer".styled(self), "battle", dt.Battle) with DMDaylightQuestion with Soft
case class DMConquerMoveAction(self : Marquistadors) extends OptionAction("Conquer".styled(self), "move, then battle", dt.Move) with DMDaylightQuestion with Soft
case class DMConquerMovedAction(self : Marquistadors) extends ForcedAction
case class DMHoardMainAction(self : Marquistadors, l : $[(Region, Clearing)]) extends OptionAction("Hoard".styled(self), DMWood.imgd(self)) with DMDaylightQuestion with Soft
case class DMHoardAction(self : Marquistadors, from : Region, to : Clearing) extends BaseAction("Hoard".styled(self), DMWood.of(self), "from", from, "to")(to)
case class DMBirdAction(self : Marquistadors, d : DeckCard) extends BaseAction("Spend a bird card for an extra action")(d.img) with ViewCard
case class DMActedAction(self : Marquistadors) extends ForcedAction

case class DMTradeMainAction(self : Marquistadors, used : $[Clearing], n : Int) extends ForcedAction with Soft
case class DMTradeAction(self : Marquistadors, c : Clearing, d : DeckCard, used : $[Clearing], n : Int) extends BaseAction("Trade".styled(self), "a card to the treaties for", c)(d.img) with ViewCard
case class DMTradeDoneAction(self : Marquistadors, n : Int) extends ForcedAction

case class DMTreatiesAction(self : Marquistadors, b : Battle, fr : Int, or : Int, use : Boolean) extends BaseAction("Treat treaties as warriors in", b.clearing)(use.?("Yes").|("No"))
case class DMTreatyHitAction(self : Marquistadors, b : Battle, n : Int, s : Int, d : DeckCard, then : ForcedAction) extends BaseAction("Discard a treaty to take a hit", "(" ~ n.hits ~ " to take)")(d.img) with ViewCard
case class DMTreatyHitsDoneAction(self : Marquistadors, b : Battle, n : Int, s : Int, then : ForcedAction) extends BaseAction(None)("Take the rest with pieces")

case class DMWoodForestAction(self : Faction, f : Marquistadors, r : Forest, then : ForcedAction) extends BaseAction(self, "puts", DMWood.of(f), "in a forest with the fewest")(implicit g => g.board.forestName(r))
case class DMKeepWinAction(self : Marquistadors) extends ForcedAction

object MarquistadorsExpansion extends FactionExpansion[Marquistadors] {
    def edges(f : Marquistadors)(implicit game : Game) = game.riverside.%(c => board.inner.has(c).not).%(f.canPlace)

    // the ship's clearing: no other faction may place pieces there
    def blocks(f : Faction, c : Clearing)(implicit game : Game) = factions.but(f).of[Marquistadors].%(game.states.contains).exists(_.at(c).has(DMShip))

    def fewestWood(f : Marquistadors)(implicit game : Game) = {
        val l = board.forests./(r => r -> f.at(r).count(DMWood))
        val m = l.rights.minOr(0)
        l.%((_, n) => n == m).lefts
    }

    // Hoard: a wood token from a forest to an adjacent clearing they rule, or between adjacent clearings they rule
    def hoards(f : Marquistadors)(implicit game : Game) : $[(Region, Clearing)] =
        board.forests.%(r => f.at(r).has(DMWood))./~(r => board.fromForest(r).%(f.rules)./(c => (r : Region) -> c)) ++
        clearings.%(c => f.at(c).has(DMWood)).%(f.rules)./~(c => game.connected(c).%(f.rules)./(d => (c : Region) -> d))

    def perform(action : Action, soft : Void)(implicit game : Game) = action @@ {
        // SETUP
        case CreatePlayerAction(f : Marquistadors) =>
            game.states += f -> new MarquistadorsPlayer(f)

            FactionInitAction(f)

        case FactionSetupAction(f : Marquistadors) =>
            val l = edges(f)./(c => c -> factions.but(f).%(game.states.contains)./(_.at(c).num).sum)
            val m = l.rights.minOr(0)

            Ask(f).each(l.%((_, n) => n == m).lefts)(DMLandAction(f, _)).needOk

        case DMLandAction(f, c) =>
            game.homelands :+= c

            f.reserve --> DMShip --> c
            f.reserve --> 6.times(Marquistador) --> c

            f.log("landed", DMShip.of(f), "and six", Marquistador.sof(f), "in", c)

            val l = board.forests.%(r => board.fromForest(r).has(c).not)

            Shuffle[Forest](l, DMWoodShuffledAction(f, l, _))

        case DMWoodShuffledAction(f, l, s) =>
            0.until(8).foreach { i =>
                f.reserve --> DMWood --> s(i % s.num)
            }

            f.log("placed", 8.hl, DMWood.of(f), "in the forests")

            DMClearShipAction(f, f.ship.get, factions.but(f).%(game.states.contains).%(_.at(f.ship.get).any))

        case DMClearShipAction(f, c, Nil) =>
            SetupFactionsAction

        case DMClearShipAction(f, c, e :: rest) =>
            val l = game.connected(c)

            Ask(e).each(l)(d => DMClearToAction(e, f, c, d, rest)).needOk

        case DMClearToAction(e, f, c, d, rest) =>
            val l = e.from(c).$./(_.piece)

            e.from(c) --> l --> d

            e.log("moved", l./(_.of(e)).comma, "from", c, "to", d)

            DMClearShipAction(f, c, rest)

        // A New Dawn
        case BirdsongNAction(30, f : Marquistadors) if f.all(DMShip).none =>
            val l = edges(f)./(c => c -> (factions ++ game.unhired).%(game.states.contains)./(_.at(c).num).sum)
            val m = l.rights.minOr(0)

            Ask(f).each(l.%((_, n) => n == m).lefts)(DMShipPlaceAction(f, _)).needOk.bailw(Next) { f.log("found no clearing for", DMShip.of(f)) }

        case DMShipPlaceAction(f, c) =>
            f.reserve --> DMShip --> c

            val n = min(2, f.pooled(Marquistador))
            f.reserve --> n.times(Marquistador) --> c

            f.log("landed", DMShip.of(f), (n > 0).?("and"), (n > 0).?(n.times(Marquistador.of(f)).comma), "in", c)

            Next

        // Deploy
        case BirdsongNAction(40, f : Marquistadors) =>
            DMDeployAskAction(f, f.deploy)

        case DMDeployAction(f, c, n) =>
            f.reserve --> Marquistador --> c

            f.log("deployed", Marquistador.of(f), "in", c)

            DMDeployAskAction(f, n)

        case DMDeployAskAction(f, n) =>
            val l = (f.ship.$ ++ f.all(DMOutpost)).distinct.%(f.canPlace)

            if (n <= 0 || f.pool(Marquistador).not || l.none)
                Next
            else
                Ask(f).each(l)(DMDeployAction(f, _, n - 1)).done(Next).birdsong(f)

        // Exploit
        case DaylightNAction(30, f : Marquistadors) =>
            val c = f.hand.%(f.craftable)
            val o = clearings.%(f.rules).%(f.canBuild).%(f.canPlace).%(c => f.matching(c).num >= 2)

            Ask(f)
                .add(NiceCraftMainAction(f, f.craft, f.crafted, f.elem ~ " crafts with treaties").!(f.hand.none).!(c.none, "nothing craftable"))
                .add(DMOutpostMainAction(f, o).!(f.pool(DMOutpost).not).!(o.none))
                .add(Next.as("End Exploit"))
                .daylight(f)

        case CraftAssignAction(f : Marquistadors, d, a, used, m, then) =>
            var l = f.treaties.$

            used.foreach { s =>
                val t = l.%(d => f.asset(d) == s).starting
                t.foreach { t =>
                    l :-= t
                    f.treaties --> t --> discard.quiet
                }
            }

            f.log("discarded", used.num.hl, "treaties")

            then

        case DMOutpostMainAction(f, l) =>
            Ask(f)
                .some(l) { c =>
                    val t = f.matching(c)
                    t.indices.combinations(2).$./(i => (t(i(0)), t(i(1)))).distinctBy { case (a, b) => (a.suit, b.suit) }./ { case (a, b) => DMOutpostAction(f, c, a, b) }
                }
                .cancel

        case DMOutpostAction(f, c, a, b) =>
            game.highlights :+= PlaceHighlight($(c))

            f.treaties --> a --> discard.quiet
            f.treaties --> b --> discard.quiet

            f.reserve --> DMOutpost --> c
            f.reserve --?> Marquistador --> c

            f.log("placed", DMOutpost.of(f), "in", c, "with", a, "and", b)

            Repeat

        // Actions
        case DaylightNAction(40, f : Marquistadors) =>
            val left = 3 + f.extra - f.acted
            val battles = clearings.%(f.canAttackIn)
            val moves = f.moveFrom
            val h = hoards(f)

            Ask(f)
                .when(left > 0)(DMConquerBattleAction(f, battles).!(battles.none))
                .when(left > 0)(DMConquerMoveAction(f).!(moves.none))
                .when(left > 0)(DMHoardMainAction(f, h).!(h.none))
                .each(f.hand.%(_.suit == Bird))(DMBirdAction(f, _))
                .done(Next)
                .daylight(f)

        case DMBirdAction(f, d) =>
            f.hand --> d --> discard.quiet

            f.extra += 1

            f.log("spent", d, "for an extra action")

            Repeat

        case DMConquerBattleAction(f, l) =>
            BattleInitAction(f, f, NoMessage, l, $(CancelAction), DMActedAction(f))

        case DMConquerMoveAction(f) =>
            MoveInitAction(f, f, $, NoMessage, f.moveFrom, f.movable, $(CancelAction), DMConquerMovedAction(f))

        case MoveFinishedAction(f : Marquistadors, from, to : Clearing, DMConquerMovedAction(ff)) =>
            if (f.canAttackIn(to))
                BattleInitAction(f, f, NoMessage, $(to), $, DMActedAction(f))
            else
                DMActedAction(f)

        case DMConquerMovedAction(f) =>
            DMActedAction(f)

        case DMActedAction(f) =>
            f.acted += 1

            Repeat

        case DMHoardMainAction(f, l) =>
            Ask(f).each(l) { case (r, c) => DMHoardAction(f, r, c) }.cancel

        case DMHoardAction(f, r, c) =>
            game.highlights :+= PlaceHighlight($(c))

            f.from(r) --> DMWood --> c

            f.log("hoarded", DMWood.of(f), "from", r, "to", c)

            f.acted += 1

            if (f.ship.has(c)) {
                val n = f.keep.num

                f.oscore(n)("bringing", DMWood.of(f), "to the", DMKeep.name.styled(f))

                f.from(c) --> DMWood --> f.keep

                if (f.keep.num >= 8)
                    DMKeepWinAction(f)
                else
                    Repeat
            }
            else
                Repeat

        case DMKeepWinAction(f) =>
            f.log("built the", DMKeep.name.styled(f), "with all eight", DMWood.of(f))

            val winners = $[Faction](f) ++ factions.%(_.coalition.has(f))

            Milestone(GameOverAction(winners, factions./(e => e -> e.vp).to(scala.collection.immutable.ListMap), game.seating./(game.ptf), game.recorded))

        // Trade
        case EveningNAction(30, f : Marquistadors) =>
            DMTradeMainAction(f, $, 0)

        case DMTradeMainAction(f, used, n) =>
            val l = clearings.%(f.rules).diff(used)

            Ask(f)
                .some(l)(c => f.hand.%(_.matches(c.cost))./(d => DMTradeAction(f, c, d, used, n)))
                .done(DMTradeDoneAction(f, n))
                .evening(f)

        case DMTradeAction(f, c, d, used, n) =>
            f.hand --> d --> f.treaties

            f.log("made a treaty in", c, "with", d)

            DMTradeMainAction(f, used :+ c, n + 1)

        case DMTradeDoneAction(f, n) =>
            if (n > 0)
                DrawCardsAction(f, n, NoMessage, AddCardsAction(f, Next))
            else
                Next

        case NightStartAction(f : Marquistadors) =>
            EveningDrawAction(f, f.draws)

        case FactionCleanUpAction(f : Marquistadors) =>
            f.acted = 0
            f.extra = 0

            CleanUpAction(f)

        // Wood of the Woods
        case ForcedRemoveTargetEffectAction(e, c, f : Marquistadors, DMWood, then) if e != f && f.pool(DMWood) =>
            Ask(e).each(fewestWood(f))(r => DMWoodForestAction(e, f, r, then)).needOk

        case DMWoodForestAction(e, f, r, then) =>
            f.reserve --> DMWood --> r

            e.log("put", DMWood.of(f), "in", board.forestName(r))

            then

        // Treaties as warriors
        case BattleAttackerPreRollAction(b) if b.parties.of[Marquistadors].exists(f => game.states(f).as[MarquistadorsPlayer].get.use(s => s.treatyDecided || s.treatyIn.any)) =>
            b.parties.of[Marquistadors].foreach { f =>
                val s = game.states(f).as[MarquistadorsPlayer].get
                s.treatyDecided = false
                s.treatyIn = None
            }

            TryAgain

        case BattleRolledDoneAction(b, fr, or) if $(b.attacker, b.defender).of[Marquistadors].exists(f => f.at(b.clearing).has(DMOutpost).not && f.matching(b.clearing).any && game.states(f).as[MarquistadorsPlayer].get.treatyDecided.not) =>
            val f = $(b.attacker, b.defender).of[Marquistadors].%(f => f.at(b.clearing).has(DMOutpost).not && f.matching(b.clearing).any).first

            Ask(f).group(f, "rolled", (f == b.attacker).?(fr).|(or).roll, "in", b.clearing)
                .add(DMTreatiesAction(f, b, fr, or, true))
                .add(DMTreatiesAction(f, b, fr, or, false))
                .needOk

        case DMTreatiesAction(f, b, fr, or, use) =>
            f.treatyDecided = true

            if (use) {
                f.treatyIn = Some(b.clearing)

                f.log("treated", f.matching(b.clearing).num.hl, "treaties as warriors in", b.clearing)
            }

            BattleRolledDoneAction(b, fr, or)

        case a @ BattleAdjustRollAction(b, f, o, fs, os, fr, or) if $(f, o).of[Marquistadors].exists(e => e.treatyIn.has(b.clearing)) && (b.parties.of[Marquistadors].exists(e => e.adjusted eq a).not) =>
            val n = (e : Faction) => e.as[Marquistadors].%(_.treatyIn.has(b.clearing))./(_.matching(b.clearing).num).|(0)

            val x = BattleAdjustRollAction(b, f, o, fs + n(f), os + n(o), fr, or)

            b.parties.of[Marquistadors].foreach(_.adjusted = x)

            x

        case a @ BattleAssignHitsAction(f : Marquistadors, b, n, s, then) if n > 0 && f.treatyIn.has(b.clearing) && f.matching(b.clearing).any && (f.hitsAsked ne a) =>
            Ask(f)
                .each(f.matching(b.clearing))(d => DMTreatyHitAction(f, b, n, s, d, then))
                .add(DMTreatyHitsDoneAction(f, b, n, s, then))
                .needOk

        case DMTreatyHitAction(f, b, n, s, d, then) =>
            f.treaties --> d --> discard.quiet

            f.log("discarded", d, "from the treaties to take a hit")

            if (n > 1)
                BattleAssignHitsAction(f, b, n - 1, s, then)
            else
                then

        case DMTreatyHitsDoneAction(f, b, n, s, then) =>
            val a = BattleAssignHitsAction(f, b, n, s, then)

            f.hitsAsked = a

            a

        case _ => UnknownContinue
    }
}
