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
import hrf.meta._
import hrf.options._
import root.elem._

// The Outsiders Expansion's maps' own rules (slugfacekillah, Law of Slug S.1 and S.2), each a setup toggle shown with its map:
// the Deep Woods' Settling (`DeepWoodsSettling`) and the Wastelands' Water and barren trails (`WastelandsWater`). Without them the
// maps play settled (each clearing tile on the clearing of its priority number) and the barren trails are plain paths.
//
// Readings (the rules leave these open, or the site does them its own way):
// - Deep Woods: an unsettled clearing is wild (any card matches it) but its crafting pieces pay for nothing; it has one building slot
//   and no player may have more than two pieces there. A move that would bring a third is offered only to a player who can settle it,
//   and settles it first. A placement that brings a third (a recruit, a build) is checked right after, before the next step: that
//   player settles it then, and with no card to spend the extra pieces go back to the supply (warriors first, then tokens, then
//   buildings). Required settling ignores the once-per-turn limit; voluntary settling (once per turn, on the player's own turn, a
//   clearing with or beside their pieces) is offered in each phase's menu.
// - Settling spends a card: its suit picks the settler and so the clearing's suit; a bird (or any card of no single suit) picks any
//   suit with settlers left. Ruin clearings get the 'R' tiles. The clearing then has its tile's building slots.
// - Setup draws 5 cards, and the required settling comes after every faction has set up, in reverse turn order, before each player
//   keeps 3. Advanced Setup's 6 cards and Corvids' Maximum Corvige aren't done.
// - Wastelands: the Water supply holds one Water per player; each player takes one (when there is one) at the start of their turn.
//   Moving along a barren trail asks for its cost when the move is made: a Water, a warrior that stays in the clearing moved from
//   (back to the supply), or for the Vagabond an undamaged item. The Water actions (2 Water each) are offered in each phase's menu on
//   the player's own turn. Surrender is asked when a building or token scoring a point is removed; Seizure counts the enemy pieces the
//   current player removes, a Water at the third, sixth and so on in a turn.

case object SettleEffect extends HiddenEffect
case object WaterAdvance extends DisplayEffect { val name = "Advance" }

case class SettleTilesAction(shuffled : $[Int], then : ForcedAction) extends ShuffledAction[Int]
case class SettleRuinTilesAction(shuffled : $[Int], then : ForcedAction) extends ShuffledAction[Int]
case object SettleMapAction extends ForcedAction

case class SettleMainAction(self : Faction, l : $[Clearing], then : ForcedAction) extends BaseAction("Deep Woods".styled(styles.forest))("Settle a clearing") with Soft
case class SettleClearingAction(self : Faction, c : Clearing, then : ForcedAction) extends BaseAction("Settle".hl)(c) with Soft
case class SettleAction(self : Faction, c : Clearing, d : DeckCard, s : BaseSuit, required : Boolean, then : ForcedAction) extends BaseAction(g => "Settle".hl ~ " " ~ c.elem(g) ~ " as")(d.img, s.elem) with ViewCard
case class SettleExcessAction(f : Faction, c : Clearing, then : ForcedAction) extends ForcedAction

case class BarrenWaterAction(self : Faction, t : $[Transport], m : Message, from : Region, to : Region, l : $[Piece], then : ForcedAction) extends BaseAction("Barren trail".hl, "from", from, "to", to)("Spend", "Water".hl)
case class BarrenWarriorAction(self : Faction, t : $[Transport], m : Message, from : Region, to : Region, l : $[Piece], w : Piece, then : ForcedAction) extends BaseAction("Barren trail".hl, "from", from, "to", to)("Remove", w.of(self), "from", from)
case class BarrenItemAction(self : Faction, t : $[Transport], m : Message, from : Region, to : Region, l : $[Piece], i : ItemRef, then : ForcedAction) extends BaseAction("Barren trail".hl, "from", from, "to", to)("Damage", i)

case class WaterMainAction(self : Faction, then : ForcedAction) extends BaseAction("Wastelands".styled(Rabbit))("Spend 2", "Water".hl) with Soft
case class WaterDrawAction(self : Faction, then : ForcedAction) extends BaseAction("Spend 2", "Water".hl)("Draw, Discard, Destroy".hl)
case class WaterDrawnAction(self : Faction, then : ForcedAction) extends ForcedAction
case class WaterDiscardAction(self : Faction, d : DeckCard, then : ForcedAction) extends BaseAction("Discard".hl, "one of the cards drawn")(d.img) with ViewCard
case class WaterDestroyAction(self : Faction, d : DeckCard, then : ForcedAction) extends BaseAction("Remove from the game".hl, "one of the cards left")(d.img) with ViewCard
case class WaterRecruitAction(self : Faction, c : Clearing, then : ForcedAction) extends BaseAction("Spend 2", "Water".hl, "to", "Recruit".hl, "up to 2 warriors in")(c)
case class WaterAdvanceAction(self : Faction, then : ForcedAction) extends BaseAction("Spend 2", "Water".hl)("Advance".hl, "(move, then battle)")
case class WaterBattleAction(self : Faction, then : ForcedAction) extends ForcedAction
case class WaterSkipAction(self : Faction, then : ForcedAction) extends BaseAction(None)("Skip")
case class WaterTakeAction(f : Faction, then : ForcedAction) extends ForcedAction
case class SurrenderVPAction(self : Faction, c : Clearing, e : Faction, p : Piece, n : Int, m : Message, then : ForcedAction) extends BaseAction("Removed", p.of(e), "in", c)("Score", n.vp)
case class SurrenderWaterAction(self : Faction, c : Clearing, e : Faction, p : Piece, n : Int, m : Message, then : ForcedAction) extends BaseAction("Removed", p.of(e), "in", c)("Take", "Water".hl, "from", e, "(" ~ "Surrender".hl ~ ")")
case class SeizureAction(f : Faction, e : Faction, then : ForcedAction) extends ForcedAction

// a barren trail: a path for adjacency, but a move along it needs its cost paid (asked once the move is picked)
case object BarrenTrail extends Transport {
    override def allows(f : Faction)(implicit game : Game) = OutsidersMaps.water

    // some party can pay: a Water, an item, or a warrior left behind
    override def allows(f : Faction, o : Region)(implicit game : Game) = allows(f) && (OutsidersMaps.waterOf(f) > 0 || OutsidersMaps.items(f).any || OutsidersMaps.staying(f, o, $).num >= 2)

    override def allows(f : Faction, o : Region, d : Region)(implicit game : Game) = allows(f, o) && OutsidersMaps.barren(o, d)

    override def allows(f : Faction, o : Region, d : Region, l : $[Movable])(implicit game : Game) = allows(f, o, d) && OutsidersMaps.canPay(f, o, l)
}

object OutsidersMaps extends Expansion {
    def active(setup : $[Faction], options : $[Meta.O]) = options.has(DeepWoodsSettling) || options.has(WastelandsWater)

    def settling(implicit game : Game) = game.board == DeepWoodsBoard && game.options.has(DeepWoodsSettling)

    def water(implicit game : Game) = game.board == WastelandsBoard && game.options.has(WastelandsWater)

    // DEEP WOODS
    def unsettled(c : Suitable)(implicit game : Game) : Boolean = settling && (c @@ {
        case c : Clearing => DeepWoodsBoard.number.contains(c) && game.settled.contains(c).not
        case _ => false
    })

    def slots(c : Clearing)(implicit game : Game) : |[Int] =
        if (settling && DeepWoodsBoard.number.contains(c))
            Some(game.settled.get(c)./(t => DeepWoodsBoard.tileSlots(t).num).|(1))
        else
            None

    // the building slots' positions on the map: the unsettled clearing's one in its centre, or the tile's
    def gates(r : Region)(implicit game : Game) : |[$[(Int, Int)]] = r @@ {
        case c : Clearing if settling && DeepWoodsBoard.number.contains(c) =>
            val (x, y) = DeepWoodsBoard.center(c)
            Some(game.settled.get(c)./(t => DeepWoodsBoard.tileSlots(t)./ { case (dx, dy) => (x + dx, y + dy) }).|($((x, y))))
        case _ => None
    }

    def settlersLeft(s : BaseSuit)(implicit game : Game) = 4 - game.settlers.get(s).|(0)

    // the suits a card can settle as
    def settleSuits(d : DeckCard)(implicit game : Game) : $[BaseSuit] =
        if (d.matches(AnySuit).not)
            $
        else
            (d.suit @@ {
                case s : BaseSuit if FoxRabbitMouse.has(s) => $(s)
                case _ => FoxRabbitMouse
            }).%(settlersLeft(_) > 0)

    def settleOptions(f : Faction)(implicit game : Game) : $[(DeckCard, BaseSuit)] = f.hand./~(d => settleSuits(d)./(s => (d, s)))

    def canSettle(f : Faction)(implicit game : Game) = settleOptions(f).any

    def players(implicit game : Game) = factions.%(game.states.contains)

    def mayEnter(f : Faction, d : Region, l : $[Movable])(implicit game : Game) : Boolean = d @@ {
        case d : Clearing if unsettled(d) && players.has(f) => f.at(d).num + l.num <= 2 || canSettle(f)
        case _ => true
    }

    def overLimit(implicit game : Game) : $[(Faction, Clearing)] =
        if (settling)
            players./~(f => clearings.%(unsettled(_)).%(c => f.at(c).num > 2)./(c => (f, c)))
        else
            $

    def voluntary(f : Faction)(implicit game : Game) : $[Clearing] =
        clearings.%(unsettled(_)).%(c => f.present(c) || game.connected(c).exists(f.present))

    def settleAsk(f : Faction, c : Clearing, then : ForcedAction, required : Boolean)(implicit game : Game) =
        Ask(f)
            .group(required.?("Must settle".hl ~ " " ~ c.elem ~ " (" ~ "no more than two pieces".hh ~ ")").|("Settle".hl ~ " " ~ c.elem))
            .each(settleOptions(f)) { case (d, s) => SettleAction(f, c, d, s, required, then) }
            .cancelIf(required.not)

    def settleMenu(f : Faction)(implicit game : Game, ask : ActionCollector) {
        if (settling && game.current == f && f.used.has(SettleEffect).not && canSettle(f)) {
            val l = voluntary(f)

            if (l.any)
                + SettleMainAction(f, l, Repeat)
        }
    }

    // WASTELANDS
    def barren(o : Region, d : Region)(implicit game : Game) : Boolean = water && WastelandsBoard.barren.exists(p => p == (o, d) || p == (d, o))

    def waterOf(f : Faction)(implicit game : Game) = game.water.get(f).|(0)

    def staying(f : Faction, o : Region, l : $[Piece])(implicit game : Game) : $[Piece] =
        if (f.is[Hero]) $ else f.at(o).of[Warrior].notOf[Tenacious].diff(l)

    def items(f : Faction)(implicit game : Game) : $[ItemRef] = f.as[Hero]./~(_.inv.%(_.damaged.not).%(_.item != CursedIdol))

    def canPay(f : Faction, o : Region, l : $[Piece])(implicit game : Game) = waterOf(f) > 0 || staying(f, o, l).any || items(f).any

    def give(f : Faction, n : Int)(implicit game : Game) {
        game.water += f -> (waterOf(f) + n)
    }

    def waterMenu(f : Faction)(implicit game : Game, ask : ActionCollector) {
        if (water && game.current == f && waterOf(f) >= 2)
            + WaterMainAction(f, Repeat)
    }

    override def birdsong(f : Faction)(implicit game : Game, ask : ActionCollector) = { settleMenu(f) ; waterMenu(f) }
    override def daylight(f : Faction)(implicit game : Game, ask : ActionCollector) = { settleMenu(f) ; waterMenu(f) }
    override def evening(f : Faction)(implicit game : Game, ask : ActionCollector) = { settleMenu(f) ; waterMenu(f) }

    // a sentence as separate words, so it wraps between them
    def t(s : String) : Elem = s.split(" ", -1).$./(w => w.txt : Elem).reduce(_ ~ " " ~ _)

    val settlingRules : $[Elem] = $(
        t("The past: clearings start") ~ " " ~ "unsettled".hh ~ t(", without a clearing tile."),
        t("An unsettled clearing is wild (any card matches it), but crafting pieces there can't be used; it has") ~ " " ~ "one building slot".hh ~ " " ~ t("(a ruin takes it), and no player may have more than") ~ " " ~ "two pieces".hh ~ " " ~ t("there."),
        "Settle".hh ~ t(": once per turn, spend a card to settle an unsettled clearing with or next to your pieces. Its suit picks the") ~ " " ~ "settler".hh ~ " " ~ t("(a bird picks any suit with settlers left, four of each), and so the clearing's suit; a face-down clearing tile is drawn for it (an 'R' tile where there is a ruin), with its own building slots."),
        t("A player who would have a third piece in an unsettled clearing must settle it first; with no card, the extra pieces go back to the supply."),
        t("Setup: everyone draws") ~ " " ~ "five cards".hh ~ t(", settles the clearings their setup put more than two pieces in (in reverse turn order), then keeps 3.")
    )

    val waterRules : $[Elem] = $(
        t("The future:") ~ " " ~ "Water".hh ~ " " ~ t("is the currency. The supply holds one Water per player; at the start of your turn, take one if any is left."),
        "Barren trails".hh ~ " " ~ t("(the pale paths with bones) link clearings, but a move along one costs a") ~ " " ~ "Water".hh ~ t(", or a warrior removed from the clearing moved from (the Vagabond damages an undamaged item instead)."),
        t("On your turn, spend") ~ " " ~ "2 Water".hh ~ " " ~ t("to:") ~ " " ~ "Draw, Discard, Destroy".hh ~ " " ~ t("(draw 3, discard one, remove one from the game, keep one),") ~ " " ~ "Recruit".hh ~ " " ~ t("(up to 2 warriors in a clearing you rule) or") ~ " " ~ "Advance".hh ~ " " ~ t("(move, then battle)."),
        "Surrender".hh ~ t(": removing an enemy's building or token, take a Water from that player instead of the point.") ~ " " ~ "Seizure".hh ~ t(": for every 3 pieces of one enemy you remove in your turn, take a Water from them."),
        t("The river reaches only the top city clearing, and runs out of the city's drain to the clearing below it.")
    )

    def perform(action : Action, soft : Void)(implicit game : Game) = action @@ {
        // DEEP WOODS SETUP
        case SetupMapAction if settling && game.settleTiles.none && game.settleRuinTiles.none && game.settled.none && (options.has(FrankenDeck).not || game.frankenDealt) =>
            Shuffle[Int]($(9, 10, 11, 12), SettleRuinTilesAction(_, SettleMapAction))

        case SettleRuinTilesAction(l, then) =>
            game.settleRuinTiles = l

            Shuffle[Int]($(1, 2, 3, 4, 5, 6, 7, 8), SettleTilesAction(_, then))

        case SettleTilesAction(l, then) =>
            game.settleTiles = l

            then

        case SettleMapAction =>
            game.original = clearings./(c => (c : Suitable) -> FoxRabbitMouse).toMap
            game.mapping = game.original

            game.ruins = board.ruinsIn(clearings)./(_ -> Ruins($)).toMap

            log("Deep Woods".styled(styles.forest), "clearings were", "unsettled".hl)

            MappingDoneAction

        case DrawCardsAction(f, 3, NotInLog(OnSetup), then) if settling =>
            DrawCardsAction(f, 5, NotInLog(OnSetup), then)

        case SetupDoneAction if settling && overLimit.any =>
            val o = overLimit
            val f = factions.reverse.%(f => o.lefts.has(f)).first
            val c = o.%{ case (g, _) => g == f }.first._2

            if (canSettle(f)) settleAsk(f, c, SetupDoneAction, true) else SettleExcessAction(f, c, SetupDoneAction)

        // SETTLING
        case a : TopLevelAction if settling && overLimit.any =>
            val fc : (Faction, Clearing) = overLimit.head
            val f = fc._1
            val c = fc._2

            if (canSettle(f)) settleAsk(f, c, a, true) else SettleExcessAction(f, c, a)

        case MoveListAction(self, f, t, m, from, to : Clearing, l, then) if settling && unsettled(to) && players.has(f) && f.at(to).num + l.num > 2 && canSettle(f) =>
            settleAsk(f, to, ForceAction(MoveListAction(self, f, t, m, from, to, l, then)), true)

        case SettleMainAction(f, l, then) =>
            Ask(f).each(l)(c => SettleClearingAction(f, c, then)).cancel

        case SettleClearingAction(f, c, then) =>
            settleAsk(f, c, then, false)

        case SettleAction(f, c, d, s, required, then) =>
            f.hand --> d --> discard(f)

            if (required.not)
                f.used :+= SettleEffect

            val ruin = board.ruins.has(c)
            val pile = ruin.?(game.settleRuinTiles).|(game.settleTiles)
            val t = pile.starting.|(DeepWoodsBoard.number(c))

            if (ruin)
                game.settleRuinTiles = game.settleRuinTiles.drop(1)
            else
                game.settleTiles = game.settleTiles.drop(1)

            game.settled += c -> t
            game.settlers += s -> (game.settlers.get(s).|(0) + 1)
            game.mapping += c -> $(s)
            game.original += c -> $(s)

            f.log("spent", d, "and settled", c, "as", s, "with a", (ruin.?("ruin tile").|("clearing tile")).hl, "of", DeepWoodsBoard.tileSlots(t).num.hl, "building", (DeepWoodsBoard.tileSlots(t).num == 1).?("slot").|("slots"))

            then

        case SettleExcessAction(f, c, then) =>
            val l = f.at(c)
            val n = l.num - 2
            val order = l.of[Warrior] ++ l.of[Token] ++ l.diff(l.of[Warrior]).diff(l.of[Token]).%!(_.is[Building]) ++ l.of[Building]
            val back = order.take(n)

            back.foreach(p => f.from(c) --> p --> f.reserve)

            f.log("had no card to settle", c, "and returned", back./(_.of(f)).comma, "to the supply")

            then

        // WASTELANDS WATER
        case BirdsongStartAction(f) if water && game.waterTurn != game.turn =>
            game.waterTurn = game.turn
            game.seized = Map()

            if (game.waterSupply < 0)
                game.waterSupply = players.num

            if (game.waterSupply > 0 && players.has(f)) {
                game.waterSupply -= 1
                give(f, 1)

                f.log("took a", "Water".hl, "from the supply")
            }

            BirdsongStartAction(f)

        // after the Vagabond's boots are exhausted for the move, so they can be the item damaged
        case MoveListAction(self, f, t, m, from, to, l, then) if t.has(BarrenTrail) && t.has(MoveBoots).not && t.has(MoveDoubleBoots).not =>
            val tt = t.but(BarrenTrail)
            val ll = l.of[Piece]

            Ask(f)
                .group("Barren trail".hl, "from", from, "to", to)
                .when(waterOf(f) > 0)(BarrenWaterAction(f, tt, m, from, to, ll, then))
                .each(staying(f, from, ll).distinct)(w => BarrenWarriorAction(f, tt, m, from, to, ll, w, then))
                .each(items(f).distinct)(i => BarrenItemAction(f, tt, m, from, to, ll, i, then))
                .bailout(CantMoveAction(f, f, $, m, from, to, then))

        case BarrenWaterAction(f, t, m, from, to, l, then) =>
            give(f, -1)
            game.waterSupply += 1

            f.log("spent a", "Water".hl, "to cross the barren trail")

            Force(MoveListAction(f, f, t.some.|($(FreeMove)), m, from, to, l, then))

        case BarrenWarriorAction(f, t, m, from, to, l, w, then) =>
            f.from(from) --> w --> f.reserve

            f.log("left", w.of(f), "behind in", from, "to cross the barren trail")

            Force(MoveListAction(f, f, t.some.|($(FreeMove)), m, from, to, l, then))

        case BarrenItemAction(f : Hero, t, m, from, to, l, i, then) =>
            f.inv = f.inv.diff($(i)) :+ i.damage

            f.log("damaged", i, "to cross the barren trail")

            Force(MoveListAction(f, f, t.some.|($(FreeMove)), m, from, to, l, then))

        case WaterMainAction(f, then) =>
            val rc = f.as[WarriorFaction]./~(w => clearings.%(w.rules).%(w.canPlace))
            val canRecruit = f.as[WarriorFaction].exists(w => w.pooled(w.warrior) > 0)

            Ask(f)
                .group("Spend 2", "Water".hl, "(" ~ waterOf(f).hl ~ " left)")
                .add(WaterDrawAction(f, then))
                .when(canRecruit)(Info("Recruit in a clearing you rule:"))
                .some(canRecruit.??(rc))(c => $(WaterRecruitAction(f, c, then)))
                .add(WaterAdvanceAction(f, then).!(f.moveFrom.none && clearings.%(f.canAttackIn).none))
                .cancel

        case WaterDrawAction(f, then) =>
            give(f, -2)
            game.waterSupply += 2

            f.log("spent 2", "Water".hl, "to", "Draw, Discard, Destroy".hl)

            DrawCardsAction(f, 3, NotInLog(NoMessage), WaterDrawnAction(f, then))

        case WaterDrawnAction(f, then) =>
            val l = f.drawn.$

            if (l.num >= 3)
                Ask(f).each(l)(d => WaterDiscardAction(f, d, WaterDrawnAction(f, then)))
            else
            if (l.num == 2)
                Ask(f).each(l)(d => WaterDestroyAction(f, d, WaterDrawnAction(f, then)))
            else
                AddCardsAction(f, then)

        case WaterDiscardAction(f, d, then) =>
            f.drawn --> d --> discard(f)

            f.log("discarded", d)

            then

        case WaterDestroyAction(f, d, then) =>
            f.drawn --> d --> game.outOfGameCards

            f.log("removed a card from the game")

            then

        case WaterRecruitAction(f : WarriorFaction, c, then) =>
            give(f, -2)
            game.waterSupply += 2

            val n = min(2, f.pooled(f.warrior))

            n.times(()).foreach(_ => f.reserve --> f.warrior --> c)

            f.log("spent 2", "Water".hl, "and recruited", n.times(f.warrior.of(f)).comma, "in", c)

            then

        case WaterAdvanceAction(f, then) =>
            give(f, -2)
            game.waterSupply += 2

            f.log("spent 2", "Water".hl, "to", "Advance".hl)

            MoveInitAction(f, f, $, WithEffect(WaterAdvance), f.moveFrom, f.movable, $(WaterSkipAction(f, WaterBattleAction(f, then))), WaterBattleAction(f, then))

        case WaterSkipAction(f, then) =>
            then

        case WaterBattleAction(f, then) =>
            val l = clearings.%(f.canAttackIn)

            if (l.none)
                then
            else
                BattleInitAction(f, f, WithEffect(WaterAdvance), l, $(WaterSkipAction(f, then)), then)

        // SURRENDER, SEIZURE
        case ForcedRemoveScoreAction(e, c, f, p, n, m, then) if water && e == game.current && e != f && players.has(e) && players.has(f) && then.is[SeizureAction].not =>
            if ((p.is[Building] || p.is[Token]) && n > 0 && waterOf(f) > 0)
                Ask(e)
                    .add(SurrenderVPAction(e, c, f, p, n, m, SeizureAction(e, f, then)))
                    .add(SurrenderWaterAction(e, c, f, p, n, m, SeizureAction(e, f, then)))
            else
                ForcedRemoveScoreAction(e, c, f, p, n, m, SeizureAction(e, f, then))

        case SurrenderVPAction(e, c, f, p, n, m, then) =>
            ForcedRemoveScoreAction(e, c, f, p, n, m, then)

        case SurrenderWaterAction(e, c, f, p, n, m, then) =>
            give(f, -1)
            give(e, 1)

            e.log("took a", "Water".hl, "from", f, "instead of scoring")

            ForcedRemoveScoreAction(e, c, f, p, 0, m, then)

        case SeizureAction(e, f, then) =>
            val k = game.seized.get(f).|(0) + 1

            game.seized += f -> k

            if (k % 3 == 0 && waterOf(f) > 0) {
                give(f, -1)
                give(e, 1)

                e.log("seized a", "Water".hl, "from", f)
            }

            then

        case _ => UnknownContinue
    }
}
