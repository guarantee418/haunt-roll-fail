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

// The Outsiders Expansion's Order of the Forest (slugfacekillah, v2.1; Law of Slug OF and its faction board): altruists whose High
// Priests herald from clearing to clearing, minister soup and coins to the poor, and Witness: their hits turn enemy warriors to
// their side, and the converted fill the Ranks for points.
//
// Readings (the rules leave these open):
// - The Order Card: the Foreseen, revealed at the start of Birdsong; a Daylight action's clearing must match it (any clearing for a
//   bird card, none for a card of another suit, like a Frog card). Anoint places the High Priest of the card's suit, the bird one
//   for a bird card
// - High Priests are warriors in battle (they count for hits and are removed after the warriors, like the Knaves' Captains) but
//   never for rule; they move alone ignoring rule, and with warriors where the Order rules
// - Minister: once (twice with the mouse High Priest on the Altar), a soup or coins token in a clearing with a High Priest
// - Witness: the spent cards are of any suit, the clearing matches the Order Card; the cost is 1 card, plus 1 for each Witness
//   already taken against that faction this turn. A hit on an enemy warrior swaps it with a warrior from the Order's supply: the
//   enemy's own warrior goes to the Ranks (back to its owner's supply when the Ranks are full), any other warrior (a Captain, a
//   hireling's) goes back to its owner's supply. With no warrior left in the Order's supply, the hit does nothing. Never removed, the
//   swapped warriors set off nothing (no Field Hospitals, no points)
// - Build: an abbey of the clearing's suit, or the bird abbey anywhere, where the Order rules
// - Congregate: each card is crafted with pieces in one clearing matching one of the card's costs; each pawn and abbey crafts once
//   a turn
// - Rest: the Ranks score after the hand limit; the bird High Priest on the Altar adds 1 whenever they score
// - Reclamation: an enemy removing Order warriors on its turn may, after the removal, send back to the supply as many warriors of
//   one player in the Ranks (any player, its own or another's)
// - Power of Faith: the Order's tokens in the clearing add to its maximum hits, attacking or defending, so with a token it is
//   never defenseless

case object Devotee extends Warrior {
    override def id = "devotee"
    override def name = "Devotee"
}

case class HighPriest(suit : Suit) extends Warrior with Rearguard {
    override def id = "priest-" + suit.name.toLowerCase
    override def name = suit.name + " High Priest"
    override def plural = suit.name + " High Priests"
}

case class OrderAbbey(suit : Suit) extends Building {
    override def id = "abbey-" + suit.name.toLowerCase
    override def name = suit.name + " Abbey"
    override def plural = suit.name + " Abbeys"
}

case object OrderSoup extends Token {
    override def id = "soup"
    override def name = "Soup"
    override def plural = "Soup"
}

case object OrderCoins extends Token {
    override def id = "coins"
    override def name = "Coins"
    override def plural = "Coins"
}

case object TheAltar extends SpecialRegion {
    override def name = "The Altar"
    override def elem(implicit game : Game) : Elem = "The Altar".hh
}

case object TheRanks extends SpecialRegion {
    override def name = "The Ranks"
    override def elem(implicit game : Game) : Elem = "The Ranks".hh
}

case object HighPriests extends FactionEffect {
    val name = "High Priests"
}

case object Confessions extends FactionEffect {
    val name = "Confessions"
}

case object PowerOfFaith extends FactionEffect {
    val name = "Power of Faith"
}

case object Martyrs extends FactionEffect {
    val name = "Martyrs"
}

case object Reclamation extends FactionEffect {
    val name = "Reclamation"
}

case object OrderFaith extends BattleEffect {
    val name = "Power of Faith"
}

case object OrderFoxHit extends BattleEffect {
    val name = "Fox High Priest"
}

// the High Priests move alone ignoring rule
case object PriestsMove extends Transport {
    override def allows(f : Faction, o : Region, d : Region, l : $[Movable])(implicit game : Game) = l.any && l.forall(_.is[HighPriest]) && allows(f, o, d)
}

trait OrderOfTheForest extends WarriorFaction {
    val clashKey = OF

    val warrior = Devotee

    def abilities(options : $[Meta.O]) = $(HighPriests, Confessions, PowerOfFaith, Martyrs, Reclamation)

    def pieces(options : $[Meta.O]) = Devotee *** 15 ++ OrderForest.suits./(HighPriest) ++ OrderForest.suits./(OrderAbbey) ++ OrderSoup *** 3 ++ OrderCoins *** 3

    override val transports : $[$[Transport]] = $($(RuledMove), $(PriestsMove))

    override def note : Elem = SpacedDash ~ "V" ~ "2.1".hh ~ " by " ~ "slugfacekillah".hl ~ HorizontalBreak ~ "Outsiders Expansion (fan-made)"

    def advertising = HighPriest(Fox).img(this) ~ OrderAbbey(Bird).img(this) ~ OrderSoup.img(this)

    def motto = "Witness".styled(this)

    override def initialDamagePriorities(options : $[Meta.O]) = $(Devotee, OrderSoup, OrderCoins) ++ OrderForest.suits./(HighPriest) ++ OrderForest.suits./(OrderAbbey)
}

case object OF extends OrderOfTheForest {
    val name = "Order of the Forest"
    val short = "OF"
    val style = "of"
    val priority = "V''''"
}

case class OrderAppearAction(self : OrderOfTheForest, c : Clearing) extends BaseAction("Appear".styled(self), "with the", HighPriests.of(self), "in")(c)
case class OrderSetupAppearAction(self : OrderOfTheForest) extends ForcedAction
case class OrderForeseeAction(self : OrderOfTheForest, then : ForcedAction) extends ForcedAction
case class OrderAnointAction(self : OrderOfTheForest, p : HighPriest, c : Clearing) extends BaseAction("Anoint".styled(self), p.of(self), "in")(c)
case class OrderConfessAction(self : OrderOfTheForest, e : Faction) extends BaseAction(Confessions.of(self), "look at the hand of")(e)
case class OrderHeraldedAction(self : OrderOfTheForest) extends ForcedAction

case class OrderMinisterAction(self : OrderOfTheForest, c : Clearing, t : Token) extends BaseAction("Minister".styled(self), "in")(t.img(self), t.of(self), "in", c)

case class OrderWitnessTargetAction(self : OrderOfTheForest, c : Clearing, e : Faction, n : Int) extends BaseAction("Witness".styled(self), "(spend cards to battle)")(e, "in", c, "(" ~ n.cards ~ ")") with Soft
case class OrderWitnessAction(self : OrderOfTheForest, c : Clearing, e : Faction, l : $[DeckCard]) extends ForcedAction
case class OrderWitnessDoneAction(self : OrderOfTheForest, then : ForcedAction) extends ForcedAction

case class OrderBuildAction(self : OrderOfTheForest, c : Clearing, p : OrderAbbey) extends BaseAction("Build".styled(self), "an abbey")(p.img(self), p.of(self), "in", c)

case class OrderCongregateAction(self : OrderOfTheForest, c : Clearing, d : DeckCard) extends BaseAction("Congregate".styled(self), "(craft with pawns and abbeys)")("Craft", d, "in", c) with ViewCard

case class OrderReclaimAskAction(self : Faction, t : OrderOfTheForest, n : Int, then : ForcedAction) extends ForcedAction
case class OrderReclaimAction(self : Faction, t : OrderOfTheForest, e : Faction, n : Int, then : ForcedAction) extends BaseAction(Reclamation.of(t), "returns", n.hl, "warriors from", "The Ranks".hh, "of")(e)


class OrderForestPlayer(val faction : OrderOfTheForest)(implicit val game : Game) extends FactionState {
    val altar = location(TheAltar, u => u.faction == faction && u.piece.is[HighPriest])
    val ranks = location(TheRanks, u => u.faction != faction && u.piece.is[Warrior])

    val foreseen = cards("foreseen")
    val order = cards("order")

    var ministered = 0
    var witnessed : $[Faction] = $
    var witnessing = false
    var built = false
    var confessed = false
    var heralded = 0
    var rested = false

    // the pieces that crafted this turn
    var congregated : $[Piece] = $

    // the enemies that removed Order warriors on their turn, once for each warrior
    var reclaim : $[Faction] = $

    def craft : $[SuitAsset] = $

    def martyr(s : Suit) = altar.$.exists(_.piece == HighPriest(s))

    def card = order.$.starting
}


object OrderForest {
    val suits = $[Suit](Fox, Rabbit, Mouse, Bird)
}

object OrderForestExpansion extends FactionExpansion[OrderOfTheForest] {
    def orders(implicit game : Game) = factions.of[OrderOfTheForest].%(game.states.contains)

    def matching(f : OrderOfTheForest, c : Clearing)(implicit game : Game) = f.card.?(_.matches(c.cost))

    def tokens(f : Faction, c : Clearing)(implicit game : Game) = f.as[OrderOfTheForest].?(f => game.states.contains(f)).??(f.at(c).of[Token].num)

    // the pieces' suits crafting in a clearing
    def assets(f : OrderOfTheForest, c : Clearing)(implicit game : Game) : $[(Piece, Suit)] = f.at(c).diff(f.congregated)./~{
        case p @ HighPriest(s) => $((p, s))
        case p @ OrderAbbey(s) => $((p, s))
        case _ => $
    }

    // the pieces to craft d with in c, the fewest of them bird, if any
    def pick(f : OrderOfTheForest, c : Clearing, d : DeckCard)(implicit game : Game) : |[$[Piece]] = d @@ {
        case d : CraftCard if d.cost.exists(c.asset.matches) && f.craftableWith(assets(f, c).map(x => x._2 : SuitAsset), $)(d) =>
            val a = assets(f, c)
            val costs = d.cost

            a.combinations(costs.num).toList.%(l => costs.permutations.exists(_.lazyZip(l).forall((x, y) => y._2.matches(x)))).sortBy(_.count(_._2 == Bird)).starting.map(_.map(x => x._1))
        case _ => None
    }

    def congregations(f : OrderOfTheForest)(implicit game : Game) = clearings.%(c => assets(f, c).any)./~(c => f.hand.%(d => pick(f, c, d).any)./(d => (c, d)))

    def ministrable(f : OrderOfTheForest)(implicit game : Game) = clearings.%(c => f.at(c).of[HighPriest].any).%(matching(f, _)).%(f.canPlace)

    def witnessable(f : OrderOfTheForest)(implicit game : Game) = clearings.%(matching(f, _)).%(f.canAttackIn)./~(c => f.enemies.%(f.canAttack(c))./(e => (c, e)))

    def cost(f : OrderOfTheForest, e : Faction)(implicit game : Game) = 1 + f.witnessed.count(e)

    def buildable(f : OrderOfTheForest)(implicit game : Game) = clearings.%(matching(f, _)).%(f.rules).%(f.canBuild)./~(c => OrderForest.suits./(OrderAbbey).%(f.pool).%(p => p.suit == Bird || p.suit.matches(c.cost))./(p => (c, p)))

    def confessable(f : OrderOfTheForest)(implicit game : Game) = f.enemies.%(_.hand.any).%(e => clearings.exists(c => f.at(c).of[OrderAbbey].any && e.at(c).any))

    // an enemy warrior a Witness hit swaps out
    def swappable(x : Figure)(implicit game : Game) = x.piece.is[Warrior] && x.piece.is[Tenacious].not

    def ranksScore(n : Int) = (n >= 9).?(3).|((n >= 5).?(2).|((n >= 2).?(1).|(0)))

    def orderIn(b : Battle)(implicit game : Game) = $(b.attacker, b.defender).of[OrderOfTheForest].%(game.states.contains)

    def perform(action : Action, soft : Void)(implicit game : Game) = action @@ {
        // SETUP
        case CreatePlayerAction(f : OrderOfTheForest) =>
            game.states += f -> new OrderForestPlayer(f)

            FactionInitAction(f)

        case FactionSetupAction(f : OrderOfTheForest) =>
            DrawCardsAction(f, 1, NoMessage, OrderForeseeAction(f, OrderSetupAppearAction(f)))

        case OrderSetupAppearAction(f) =>
            val l = clearings.%(f.canPlace)
            val n = l./(c => factions./(_.at(c).num).sum).minOr(0)

            Ask(f).group(f, "appears in a clearing with the fewest pieces").each(l.%(c => factions./(_.at(c).num).sum == n))(c => OrderAppearAction(f, c))

        case OrderAppearAction(f, c) =>
            game.highlights :+= PlaceHighlight($(c))

            f.reserve --> OrderForest.suits./(HighPriest) --> c

            f.log("placed the", HighPriests.of(f), "in", c)

            SetupFactionsAction

        case OrderForeseeAction(f, then) =>
            f.drawn --> f.foreseen

            then

        // HIGH PRIESTS: pawns don't rule
        // MARTYRS
        case ForcedRemoveTargetEffectAction(e, c, f : OrderOfTheForest, p : HighPriest, then) =>
            f.limbo(c) --> p --> f.altar

            f.log("placed", p.of(f), "on", TheAltar.elem, "with", Martyrs.of(f))

            then

        // RECLAMATION
        case ForcedRemoveTargetEffectAction(e, c, f : OrderOfTheForest, Devotee, then) if e != f && game.current == e && f.ranks.any =>
            f.reclaim :+= e

            then

        case ForcedRemoveCleanupAction(x, then) if orders.exists(_.reclaim.any) =>
            val f = orders.%(_.reclaim.any).head
            val e = f.reclaim.head
            val n = f.reclaim.count(e)

            f.reclaim = f.reclaim.filterNot(_ == e)

            OrderReclaimAskAction(e, f, n, ForcedRemoveCleanupAction(x, then))

        case OrderReclaimAskAction(e, f, n, then) =>
            val l = f.ranks.$./(_.faction).distinct

            if (n <= 0 || l.none)
                then
            else
                Ask(e).group(Reclamation.of(f), "returns", n.hl, "warriors of one player from", TheRanks.elem)
                    .each(l)(o => OrderReclaimAction(e, f, o, min(n, f.ranks.$.count(_.faction == o)), then))
                    .skip(then)

        case OrderReclaimAction(e, f, o, n, then) =>
            val l = f.ranks.$.%(_.faction == o).take(n)

            l.foreach(_ --> game.pool)

            e.log("returned", l./(_.elem).comma, "from", TheRanks.elem, "with", Reclamation.of(f))

            then

        // POWER OF FAITH
        case BattleAdjustRollAction(b, f, o, fs, os, fr, or) if orderIn(b).exists(x => x.used.has(OrderFaith).not && tokens(x, b.clearing) > 0) =>
            orderIn(b).foreach(_.used :+= OrderFaith)

            orderIn(b).%(x => tokens(x, b.clearing) > 0).foreach(x => x.log("can deal", tokens(x, b.clearing).hits, "more with", PowerOfFaith.of(x)))

            BattleAdjustRollAction(b, f, o, fs + tokens(f, b.clearing), os + tokens(o, b.clearing), fr, or)

        // THE ALTAR: fox, an extra hit as defender
        case BattleBonusAction(b, f, o : OrderOfTheForest, fs, os, fr, or, fh, oh, fe, oe) if o.martyr(Fox) && o.used.has(OrderFoxHit).not =>
            o.used :+= OrderFoxHit

            o.log("dealt an extra", 1.hit, "with", HighPriest(Fox).of(o), "on", TheAltar.elem)

            BattleBonusAction(b, f, o, fs, os, fr, or, fh, oh, fe, oe + 1)

        // WITNESS: the hits swap enemy warriors
        case BattleDealHitAction(e, b, x, _, then) if b.attacker.as[OrderOfTheForest].?(f => game.states.contains(f) && f.witnessing) && x.faction != b.attacker && swappable(x) =>
            val f = b.attacker.as[OrderOfTheForest].get
            val c = b.clearing
            val a = x.faction

            if (f.pool(Devotee).not) {
                f.log("had no", Devotee.of(f), "left to swap", x.piece.of(a))

                then
            }
            else {
                f.reserve --> Devotee --> c

                if (a.as[WarriorFaction].?(_.warrior == x.piece) && a.is[Hireling].not && f.ranks.num < 10) {
                    a.from(c) --> x.piece --> f.ranks

                    f.log("swapped", x.piece.of(a), "for", Devotee.of(f), "and placed it in", TheRanks.elem)
                }
                else {
                    x --> game.pool

                    f.log("swapped", x.piece.of(a), "for", Devotee.of(f), "and returned it to", a)
                }

                then
            }

        // BIRDSONG
        case BirdsongNAction(20, f : OrderOfTheForest) =>
            if (f.foreseen.any) {
                f.foreseen --> f.order

                f.log("proclaimed", f.card.get, "the", "Order Card".styled(f))
            }

            Next

        case BirdsongNAction(30, f : OrderOfTheForest) =>
            val p = f.card./~(d => f.altar.$./(_.piece).of[HighPriest].%(_.suit == d.suit).starting)

            val l = clearings.%(matching(f, _)).%(f.canPlace)

            if (p.none || l.none)
                Next
            else
                Ask(f).group("Anoint".styled(f), p.get.of(f)).each(l)(c => OrderAnointAction(f, p.get, c))

        case OrderAnointAction(f, p, c) =>
            game.highlights :+= PlaceHighlight($(c))

            f.altar --> p --> c

            f.log("anointed", p.of(f), "in", c)

            Next

        case BirdsongNAction(40, f : OrderOfTheForest) =>
            DrawCardsAction(f, 1, NotInLog(OnTurn(game.turn)), OrderForeseeAction(f, Next))

        case BirdsongNAction(45, f : OrderOfTheForest) if f.confessed.not =>
            val l = confessable(f)

            if (l.none)
                Next
            else
                Ask(f).group(Confessions.of(f)).each(l)(e => OrderConfessAction(f, e)).skip(Next).birdsong(f)

        case OrderConfessAction(f, e) =>
            f.confessed = true

            f.log("heard the", Confessions.of(f), "of", e)

            PeekCardsAsk(f, FactionHand(e), e.hand.get, Next)

        case BirdsongNAction(50, f : OrderOfTheForest) if f.heralded < 3 =>
            soft()

            val l = f.moveFrom

            if (l.none)
                NoAsk(f)(Next)
            else
                Ask(f).group("Herald".styled(f), "(" ~ (3 - f.heralded).hl ~ " moves left)")
                    .add(MoveInitAction(f, f, $, NoMessage, l, f.movable, $(CancelAction), OrderHeraldedAction(f)).as("Move".styled(f)))
                    .done(Next)
                    .birdsong(f)

        case OrderHeraldedAction(f) =>
            f.heralded += 1

            Repeat

        // DAYLIGHT
        case DaylightNAction(30, f : OrderOfTheForest) if f.ministered < 1 + f.martyr(Mouse).??(1) =>
            soft()

            val l = ministrable(f)

            if (l.none || (f.pool(OrderSoup) || f.pool(OrderCoins)).not)
                NoAsk(f)(Next)
            else
                Ask(f).group("Minister".styled(f), "(" ~ "Order Card".styled(f) ~ " " ~ f.card./(_.elem).|("none".txt) ~ ")")
                    .some(l)(c => $[Token](OrderSoup, OrderCoins).%(f.pool)./(t => OrderMinisterAction(f, c, t)))
                    .skip(Next)
                    .daylight(f)

        case OrderMinisterAction(f, c, t) =>
            game.highlights :+= PlaceHighlight($(c))

            f.reserve --> t --> c

            f.ministered += 1

            if (t == OrderSoup) {
                f.log("ministered", OrderSoup.of(f), "in", c)

                DrawCardsAction(f, 2, NoMessage, AddCardsAction(f, Repeat))
            }
            else {
                val n = min(2, f.pooled(Devotee))

                f.reserve --> n.times(Devotee) --> c

                f.log("ministered", OrderCoins.of(f), "in", c, "and placed", n.of(Devotee.name).styled(f))

                Repeat
            }

        case DaylightNAction(40, f : OrderOfTheForest) =>
            soft()

            val l = witnessable(f).%{ case (c, e) => cost(f, e) <= f.hand.num }

            if (l.none)
                NoAsk(f)(Next)
            else
                Ask(f).group("Witness".styled(f), "(" ~ "Order Card".styled(f) ~ " " ~ f.card./(_.elem).|("none".txt) ~ ")")
                    .each(l) { case (c, e) => OrderWitnessTargetAction(f, c, e, cost(f, e)) }
                    .done(Next)
                    .daylight(f)

        case OrderWitnessTargetAction(f, c, e, n) =>
            XXSelectObjectsAction(f, f.hand)
                .withGroup("Witness".styled(f) ~ " " ~ e.elem ~ " in " ~ c.elem ~ ", spending " ~ n.cards)
                .withRule(_.num(n))
                .withThen(l => OrderWitnessAction(f, c, e, l))(l => "Spend".hl ~ l./(" " ~ _.elem))
                .withExtra($(NoHand, CancelAction))

        case OrderWitnessAction(f, c, e, l) =>
            f.hand --> l --> discard.quiet

            f.witnessed :+= e
            f.witnessing = true

            f.log("witnessed in", c, "spending", l./(_.elem).comma)

            ForceAction(BattleConfirmAction(f, f, f, NoMessage, c, e, None, OrderWitnessDoneAction(f, Repeat)))

        case OrderWitnessDoneAction(f, then) =>
            f.witnessing = false

            then

        case DaylightNAction(50, f : OrderOfTheForest) if f.built.not =>
            soft()

            val l = buildable(f)

            if (l.none)
                NoAsk(f)(Next)
            else
                Ask(f).group("Build".styled(f), "an abbey").each(l) { case (c, p) => OrderBuildAction(f, c, p) }.skip(Next).daylight(f)

        case OrderBuildAction(f, c, p) =>
            game.highlights :+= PlaceHighlight($(c))

            f.reserve --> p --> c

            f.built = true

            f.log("built", p.of(f), "in", c)

            Next

        // EVENING
        case EveningNAction(20, f : OrderOfTheForest) if f.martyr(Rabbit) =>
            DrawCardsAction(f, 1, NoMessage, AddCardsAction(f, Next))

        case EveningNAction(30, f : OrderOfTheForest) =>
            soft()

            val l = congregations(f)

            if (l.none)
                NoAsk(f)(Next)
            else
                Ask(f).group("Congregate".styled(f)).each(l) { case (c, d) => OrderCongregateAction(f, c, d) }.add(Next.as("End Craft")).evening(f)

        case OrderCongregateAction(f, c, d) =>
            val l = pick(f, c, d).get

            f.congregated ++= l

            f.log("congregated in", c, "with", l./(_.of(f)).comma)

            CraftPerformAction(f, d, NoMessage)

        case NightStartAction(f : OrderOfTheForest) =>
            if (f.order.any) {
                f.log("took the", "Order Card".styled(f), "into hand")

                f.order --> f.hand
            }

            EveningDrawAction(f, 0)

        case EveningAfterHandLimitAction(f : OrderOfTheForest) if f.rested.not =>
            f.rested = true

            val n = ranksScore(f.ranks.num)

            if (n > 0)
                f.oscore(n + f.martyr(Bird).??(1))("for", f.ranks.num.hl, "warriors in", TheRanks.elem)

            EveningAfterHandLimitAction(f)

        case FactionCleanUpAction(f : OrderOfTheForest) =>
            f.ministered = 0
            f.witnessed = $
            f.witnessing = false
            f.built = false
            f.confessed = false
            f.heralded = 0
            f.congregated = $
            f.rested = false

            CleanUpAction(f)

        case _ => UnknownContinue
    }

}
