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


// Lilypad Diaspora, official Root: Homeland version (Law of Root section 16, 2026-06-05)

case class EnclaveLaborers(faction : InvasiveEEE, suit : BaseSuit) extends CardEffect {
    override val name = suit.name + " Laborers"
}

case class FrogSettlers(faction : InvasiveEEE) extends CardEffect {
    override val name = "Settlers"
}

case class FrogStewards(faction : InvasiveEEE) extends CardEffect {
    override val name = "Stewards"
}

case class FrogCompanions(faction : InvasiveEEE) extends CardEffect {
    override val name = "Companions"
}

case class FrogAgitators(faction : InvasiveEEE) extends CardEffect {
    override val name = "Agitators"
}

case class FrogAdvocates(faction : InvasiveEEE) extends CardEffect {
    override val name = "Advocates"
}

case class FrogMilitias(faction : InvasiveEEE) extends CardEffect {
    override val name = "Militias"
}

case class FrogAssimilationists(faction : InvasiveEEE) extends CardEffect {
    override val name = "Assimilationists"
}

case object EnclaveDefense extends FactionEffect {
    val name = "Enclave Defense"
}

case object FearsComeToPass extends FactionEffect {
    val name = "Fears Come to Pass"
}

case object Negotiations extends FactionEffect {
    val name = "Negotiations"
}


trait InvasiveEEE extends WarriorFaction with CommonInvasive {
    val clashKey = LDvD

    val warrior = FrogEEE

    def abilities(options : $[Meta.O]) = $(MorningCraft, Swimmers)

    def pieces(options : $[Meta.O]) = FrogEEE *** 20 ++ PeacefulEEE *** 12 ++ MilitantEEE *** 12

    override val transports : $[$[Transport]] = $($(RuledMove), $(Waterway))

    override def note : Elem = HorizontalBreak ~ "Root: Homeland version, Law of Root from " ~ "2026-06-05".hh

    def advertising = PeacefulEEE.img(this) ~ MilitantEEE.img(this) ~ PeacefulEEE.img(this) ~ MilitantEEE.img(this) ~ PeacefulEEE.img(this)

    def motto = "Settle".styled(this)
}

case object FrogEEE extends Warrior with CommonFrogWarrior {
    override def id = "Frog"
    override def name = "Frog"
}

trait EnclaveEEE extends Token with CommonFrogToken

case object PeacefulEEE extends EnclaveEEE with CommonPeaceful {
    override def id = "Peaceful"
    override def name = "Peaceful Enclave"
    override def imgid(f : Faction) = f.style + "-" + id
}

case object MilitantEEE extends EnclaveEEE with CommonMilitant {
    override def id = "Militant"
    override def name = "Militant Enclave"
    override def imgid(f : Faction) = f.style + "-" + id
}


case object LDvE extends InvasiveEEE {
    val name = "Lilypad Diaspora"
    override def funName = NameReference(name, this) ~ " Diaspora"
    val short = "LDvE"
    val style = "hld"
    val priority = "T"
}


// A frog card spent or revealed as another suit with Companions
case class DisguisedCard(card : DeckCard, suit : Suit) extends DeckCard {
    def name = card.name
    def id = card.id
    override def altS = card.name + " as " + suit.name
    def altL = card.name + " - frog card used as a " + suit.name + " card with Companions"
    override def toString = "DisguisedCard(" + card.id + ", " + suit + ")"
}


class InvasiveEEEPlayer(val faction : InvasiveEEE)(implicit val game : Game) extends FactionState {
    var acted = 0

    // frog cards before they are shuffled into the shared deck
    val supply = cards("frog-supply", Deck.frogEEE)

    // face up, last card is the top
    val pond = cards("pond")

    // frog cards turned into another suit with Companions, and their stand-ins
    val companions = cards("companions")
    val standins = cards("standins", Deck.frogEEE./~(d => InvasiveEEEExpansion.disguiseSuits./(DisguisedCard(d, _))))

    var retaliated : $[Clearing] = $
    var integrated = false

    // Fears Come to Pass
    var watched : |[Battle] = None
    var fears : $[Faction] = $
    var calm = false

    // Negotiations, once per turn for each enemy
    var negotiated : $[(Int, Faction)] = $

    // Laborers, once at the start of the Evening of the current turn
    var drawing = false

    var laborers : |[Int] = None
    var laborersUsed : $[SuitAsset] = $

    def enclaves = clearings.%(c => at(c).of[EnclaveEEE].any)
    def peaceful = clearings.%(c => at(c).has(PeacefulEEE))
    def militant = clearings.%(c => at(c).has(MilitantEEE))
    def available = 12 - enclaves.num

    def craft = enclaves./(_.asset)
}


case class InvasiveEEESetupClearingAction(self : InvasiveEEE, c : Clearing) extends BaseAction(self, "starts in")(c)
case class InvasiveEEEShuffleFrogCardsAction(f : InvasiveEEE, shuffled : $[DeckCard], then : ForcedAction) extends ShuffledAction[DeckCard]

trait InvasiveEEEDaylightQuestion extends FactionAction {
    override def self : InvasiveEEE

    def question(implicit game : Game) = self.elem ~ SpacedDash ~ Daylight.elem ~ Break ~
        Div(
            self.acted.times(Image("action-black", styles.action, "")).take(self.acted) ~
            (3 - self.acted).times(Image(self.style + "-action", styles.action)),
        styles.margined)
}

case class InvasiveEEESettleMainAction(self : InvasiveEEE, l : $[Clearing]) extends OptionAction("Settle".styled(self)) with InvasiveEEEDaylightQuestion with Soft
case class InvasiveEEESettleClearingAction(self : InvasiveEEE, c : Clearing) extends ForcedAction
case class InvasiveEEESettleMoveAction(self : InvasiveEEE, c : Clearing, used : $[Clearing]) extends ForcedAction with Soft
case class InvasiveEEESettleEndAction(self : InvasiveEEE, c : Clearing) extends ForcedAction with Soft
case class InvasiveEEESettlePlaceAction(self : InvasiveEEE, c : Clearing) extends ForcedAction

case class InvasiveEEEProvokeMainAction(self : InvasiveEEE) extends OptionAction("Provoke".styled(self)) with InvasiveEEEDaylightQuestion with Soft
case class InvasiveEEEProvokeFlipAction(self : InvasiveEEE, c : Clearing) extends ForcedAction
case class InvasiveEEEProvokePlaceAction(self : InvasiveEEE, c : Clearing) extends ForcedAction

case class InvasiveEEEDoneAction(f : InvasiveEEE, then : ForcedAction) extends ForcedAction

case class InvasiveEEEMusterAction(f : InvasiveEEE, m : Message, then : ForcedAction) extends ForcedAction with Soft
case class InvasiveEEEMusterClearingsAction(self : InvasiveEEE, m : Message, l : $[Clearing], then : ForcedAction) extends ForcedAction

case class InvasiveEEERallyAction(self : InvasiveEEE) extends BaseAction("Rally".styled(self), "or", "Reconcile".styled(self))("Rally".styled(self))
case class InvasiveEEEReconcileMainAction(self : InvasiveEEE) extends BaseAction("Rally".styled(self), "or", "Reconcile".styled(self))("Reconcile".styled(self)) with Soft
case class InvasiveEEEReconcileSelectAction(self : InvasiveEEE, l : $[Clearing]) extends ForcedAction with Soft
case class InvasiveEEEReconcileAction(self : InvasiveEEE, l : $[Clearing]) extends ForcedAction
case class InvasiveEEEReconcileDrawAction(self : InvasiveEEE, l : $[Clearing]) extends ForcedAction

case class InvasiveEEERetaliateAction(f : InvasiveEEE, c : Clearing) extends ForcedAction

case class InvasiveEEEIntegrateAction(self : InvasiveEEE, d : DeckCard, s : BaseSuit) extends ForcedAction

case class InvasiveEEEFearsAction(f : InvasiveEEE, e : Faction, then : ForcedAction) extends ForcedAction

case class InvasiveEEEPondDrawAction(f : Faction, e : InvasiveEEE, n : Int, m : Message, then : ForcedAction) extends ForcedAction
case class InvasiveEEEDeckDrawAction(f : Faction, e : InvasiveEEE, n : Int, m : Message, then : ForcedAction) extends ForcedAction
case class InvasiveEEEDrawnAction(e : InvasiveEEE, then : ForcedAction) extends ForcedAction
case class InvasiveEEEPondTopAction(f : Faction, e : InvasiveEEE, m : Message, then : ForcedAction) extends ForcedAction

case class InvasiveEEENegotiateMainAction(self : Faction, e : InvasiveEEE, then : ForcedAction) extends BaseAction(None)(Negotiations.of(e)) with Soft
case class InvasiveEEENegotiateAction(self : Faction, e : InvasiveEEE, c : Clearing, then : ForcedAction) extends ForcedAction with Soft
case class InvasiveEEENegotiateGiveAction(self : Faction, e : InvasiveEEE, c : Clearing, r : Faction, d : DeckCard, then : ForcedAction) extends ForcedAction
case class InvasiveEEENegotiateFlipAction(self : Faction, e : InvasiveEEE, c : Clearing, then : ForcedAction) extends ForcedAction

case class InvasiveEEELaborersMainAction(self : Faction, e : InvasiveEEE) extends ForcedAction with Soft
case class InvasiveEEELaborersCraftAction(self : Faction, e : InvasiveEEE, d : CraftCard) extends ForcedAction with Soft
case class InvasiveEEELaborersAssignAction(self : Faction, e : InvasiveEEE, d : CraftCard, used : $[SuitAsset]) extends ForcedAction
case class InvasiveEEELaborersDoneAction(self : Faction, e : InvasiveEEE) extends ForcedAction

case class InvasiveEEEAdvocatesAction(self : Faction, e : InvasiveEEE, s : BaseSuit) extends ForcedAction

case class InvasiveEEECompanionsMainAction(self : Faction, e : InvasiveEEE, then : ForcedAction) extends BaseAction(None)(FrogCompanions(e)) with Soft
case class InvasiveEEECompanionsAction(self : Faction, e : InvasiveEEE, d : DeckCard, s : Suit, then : ForcedAction) extends ForcedAction

case class InvasiveEEESettlersMoveAction(self : Faction, e : InvasiveEEE) extends ForcedAction
case class InvasiveEEEAgitatorsAction(self : Faction, e : InvasiveEEE, c : Clearing, t : Faction) extends ForcedAction
case class InvasiveEEEMilitiasFlipAction(self : Faction, e : InvasiveEEE, c : Clearing) extends ForcedAction
case class InvasiveEEEMilitiasPlaceAction(self : Faction, e : InvasiveEEE, c : Clearing) extends ForcedAction
case class InvasiveEEEAssimilateAction(self : Faction, e : InvasiveEEE, c : Clearing) extends ForcedAction


object InvasiveEEEExpansion extends FactionExpansion[InvasiveEEE] {
    val disguiseSuits = $[Suit](Bird, Fox, Rabbit, Mouse)

    // 16.2.1.I: Peaceful adds the frog suit, Militant covers the clearing suit (landmark suits stay)
    def refresh(f : InvasiveEEE, c : Clearing)(implicit game : Game) {
        val original = game.original(c)

        if (f.at(c).has(MilitantEEE))
            game.mapping += c -> (game.lostCity.has(c).??(original) ++ $(Frog))
        else
        if (f.at(c).has(PeacefulEEE))
            game.mapping += c -> (original ++ $(Frog))
        else
            game.mapping += c -> original
    }

    def flip(f : InvasiveEEE, c : Clearing, from : EnclaveEEE, to : EnclaveEEE)(implicit game : Game) {
        if (f.at(c).has(from)) {
            f.from(c) --> from --> f.reserve
            f.reserve --> to --> c

            refresh(f, c)
        }
    }

    def place(f : InvasiveEEE, c : Clearing, p : EnclaveEEE)(implicit game : Game) {
        f.reserve --> p --> c

        refresh(f, c)
    }

    def bonus(n : Int) = (n >= 8).?(3).|((n >= 4).?(2).|((n >= 2).?(1).|(0)))

    def pondTop(e : InvasiveEEE)(implicit game : Game) = e.pond.get.lastOption

    def toPondBottom(e : InvasiveEEE, f : Faction, d : DeckCard)(implicit game : Game) {
        val rest = e.pond.get

        f.hand --> d --> e.pond

        e.pond --> rest --> e.pond
    }

    // Negotiations (16.2.5): offered to enemies during their turn
    def negotiations(f : Faction)(implicit game : Game, ask : ActionCollector) {
        factions.but(f).of[InvasiveEEE].%(game.states.contains).%(_.friends(f).not).foreach { e =>
            if (e.negotiated.has((game.turn, f)).not)
                if (e.militant.exists(f.present))
                    + InvasiveEEENegotiateMainAction(f, e, Repeat)
        }
    }

    // Companions: turn a frog card into another suit
    def companions(f : Faction)(implicit game : Game, ask : ActionCollector) {
        factions.of[InvasiveEEE].%(game.states.contains).foreach { e =>
            if (f.has(FrogCompanions(e)) && f.hand.exists(_.suit == Frog))
                + InvasiveEEECompanionsMainAction(f, e, Repeat)
        }
    }

    override def birdsong(f : Faction)(implicit game : Game, ask : ActionCollector) = { negotiations(f) ; companions(f) }
    override def daylight(f : Faction)(implicit game : Game, ask : ActionCollector) = { negotiations(f) ; companions(f) }
    override def evening(f : Faction)(implicit game : Game, ask : ActionCollector) = { negotiations(f) ; companions(f) }

    def laborersAssets(f : Faction, e : InvasiveEEE)(implicit game : Game) : $[SuitAsset] =
        FoxRabbitMouse.%(s => f.has(EnclaveLaborers(e, s)))./~(s => e.peaceful.%(c => game.original(c).has(s)))./(_.asset)

    def perform(action : Action, soft : Void)(implicit game : Game) = action @@ {
        // SETUP
        case CreatePlayerAction(f : InvasiveEEE) =>
            game.states += f -> new InvasiveEEEPlayer(f)

            FactionInitAction(f)

        case FactionSetupAction(f : InvasiveEEE) =>
            val river = game.riverside.diff(game.homelands).%(f.canPlace)
            val l = river.some.|(clearings.diff(game.homelands).%(c => game.connected(c).exists(game.riverside.has)).%(f.canPlace))

            Ask(f).each(l)(c => InvasiveEEESetupClearingAction(f, c)).needOk.bail(SetupFactionsAction)

        case InvasiveEEESetupClearingAction(f, c) =>
            if (options.has(SetupTypeHomelands))
                game.homelands :+= c

            f.reserve --> 5.times(f.warrior) --> c

            place(f, c, PeacefulEEE)

            f.log("placed", 5.hl, f.warrior.sof(f), "and", PeacefulEEE.of(f), "in", c)

            if (game.arity == 2) {
                f.supply --> EnclaveDominance --> game.outOfGameCards

                f.log("removed", EnclaveDominance, "from the game")
            }

            f.supply --> deck

            Shuffle[DeckCard](deck, InvasiveEEEShuffleFrogCardsAction(f, _, SetupFactionsAction))

        case InvasiveEEEShuffleFrogCardsAction(f, l, then) =>
            deck --> l --> deck

            f.log("shuffled", (game.arity == 2).?(13).|(14).hl, Frog, "cards into the shared deck")

            then

        // POND
        case ShufflePileAction(l, then) if factions.of[InvasiveEEE].exists(_.pond.any) =>
            factions.of[InvasiveEEE].foreach { e =>
                e.pond --> pile

                log("The", "Pond".styled(e), "was shuffled into the discard pile")
            }

            Shuffle[DeckCard](pile, ShufflePileAction(_, then))

        case DrawCardsAction(f, n, m, then) if n > 0 && factions.of[InvasiveEEE].exists(e => e.pond.any && e.drawing.not) =>
            val e = factions.of[InvasiveEEE].%(e => e.pond.any && e.drawing.not).first
            val d = pondTop(e).get

            Ask(f)
                .group(f, "draws", n.cards)
                .add(InvasiveEEEPondDrawAction(f, e, n, m, then).as("Draw", d, "from the", "Pond".styled(e)))
                .add(InvasiveEEEDeckDrawAction(f, e, n, m, then).as("Draw from the deck"))

        case InvasiveEEEDeckDrawAction(f, e, n, m, then) =>
            e.drawing = true

            DrawCardsAction(f, n, m, InvasiveEEEDrawnAction(e, then))

        case InvasiveEEEPondDrawAction(f, e, n, m, then) =>
            e.drawing = true

            if (n > 1)
                DrawCardsAction(f, n - 1, m, InvasiveEEEPondTopAction(f, e, m, then))
            else
                InvasiveEEEPondTopAction(f, e, m, then)

        case InvasiveEEEPondTopAction(f, e, m, then) =>
            pondTop(e).foreach { d =>
                e.pond --> d --> f.drawn

                f.log("drew", d, "from the", "Pond".styled(e), m)
            }

            InvasiveEEEDrawnAction(e, then)

        case InvasiveEEEDrawnAction(e, then) =>
            e.drawing = false

            then

        // Frog Ambush: discard to the Pond bottom
        case BattleAmbushAction(f, b, d @ Ambush(Frog), true, true, then) if factions.of[InvasiveEEE].any =>
            toPondBottom(factions.of[InvasiveEEE].first, f, d)

            f.log("played", d)

            BattleAskCounterAmbushAction(b, then)

        case BattleCounterAmbushAction(f, b, d @ Ambush(Frog), true, true, then) if factions.of[InvasiveEEE].any =>
            toPondBottom(factions.of[InvasiveEEE].first, f, d)

            f.log("counter-played", d)

            then

        // ENCLAVES
        case BattlePostHitInAction(b, e, f : InvasiveEEE, FrogEEE, then) =>
            e.log("dispersed", FrogEEE.of(f))

            then

        case BattlePostHitInAction(b, e, f : InvasiveEEE, p : EnclaveEEE, then) =>
            e.log("broke up", p.of(f))

            then

        case ForcedRemoveTargetEffectAction(e, c, f : InvasiveEEE, p : EnclaveEEE, then) =>
            refresh(f, c)

            if (p == PeacefulEEE && e != f && f.watched.none && f.calm.not)
                f.fears :+= e

            then

        // Enclave Defense (16.2.2.Ia)
        case BattleDefenderPreRollAction(b) if b.codef.none && b.defender.is[InvasiveEEE] && b.defender.at(b.clearing).of[EnclaveEEE].any &&
            factions.of[WarriorFaction].%(d => d != b.attacker && d != b.defender && b.ally.has(d).not && d.dominance.has(EnclaveDominance)).exists(_.at(b.clearing).of[Warrior].any) =>
            val d = factions.of[WarriorFaction].%(d => d != b.attacker && d != b.defender && b.ally.has(d).not && d.dominance.has(EnclaveDominance)).%(_.at(b.clearing).of[Warrior].any).first

            d.log("joined the defense of", b.defender, "in", b.clearing, "with", EnclaveDefense.of(b.defender))

            BattleDefenderPreRollAction(b.copy(codef = Some(d)))

        // Fears Come to Pass (16.2.4)
        case BattleStartedAction(b) if factions.of[InvasiveEEE].exists(f => b.parties.has(f) && f.watched.has(b).not) =>
            factions.of[InvasiveEEE].%(f => b.parties.has(f) && f.watched.has(b).not).foreach { f =>
                f.watched = Some(b)

                if (b.defender == f && b.attacker != f && f.at(b.clearing).has(PeacefulEEE))
                    f.fears :+= b.attacker
            }

            BattleStartedAction(b)

        case ForcedRemoveProcessAction(f : InvasiveEEE, then) =>
            val l = f.fears.distinct

            f.fears = $
            f.watched = None
            f.calm = false

            l.foldRight(then)((e, q) => InvasiveEEEFearsAction(f, e, q))

        case InvasiveEEEFearsAction(f, e, then) =>
            val l = f.peaceful.%(e.present)

            if (l.any) {
                l.foreach(c => flip(f, c, PeacefulEEE, MilitantEEE))

                f.log("feared", e, "and flipped", MilitantEEE.sof(f), "in", l./(_.elem).comma, "with", FearsComeToPass.of(f))

                InvasiveEEEMusterClearingsAction(f, NoMessage, l.%(f.canPlace).take(f.pooled(f.warrior)), then)
            }
            else
                then

        // HELPER
        case InvasiveEEEMusterAction(f, m, then) =>
            val t = f.pooled(f.warrior)
            val r = f.militant.%(f.canPlace)

            if (r.none)
                then
            else
            if (t >= r.num)
                InvasiveEEEMusterClearingsAction(f, m, r, then)
            else
            if (t == 0) {
                f.log("had no warriors to place")

                then
            }
            else
                Ask(f).group("Place", 1.hl, "warrior at each", MilitantEEE.of(f))
                    .each(r.combinations(t).$)(l => InvasiveEEEMusterClearingsAction(f, m, l, then).as(l.comma))

        case InvasiveEEEMusterClearingsAction(f, m, l, then) =>
            if (l.any) {
                game.highlights :+= PlaceHighlight(l)

                l.foreach(c => f.reserve --> f.warrior --> c)

                f.log("placed", f.warrior.of(f), "in", l./(_.elem).comma, m)
            }

            then

        // TURN
        case BirdsongNAction(30, f : InvasiveEEE) =>
            XCraftMainAction(f)

        case BirdsongNAction(50, f : InvasiveEEE) =>
            Ask(f)
                .add(InvasiveEEERallyAction(f))
                .add(InvasiveEEEReconcileMainAction(f))
                .birdsong(f)

        case InvasiveEEERallyAction(f) =>
            f.log("rallied")

            InvasiveEEEMusterAction(f, NoMessage, Next)

        case InvasiveEEEReconcileMainAction(f) =>
            InvasiveEEEReconcileSelectAction(f, $)

        case InvasiveEEEReconcileSelectAction(f, l) =>
            Ask(f).group("Reconcile".styled(f), "- flip", MilitantEEE.sof(f), "to", PeacefulEEE.of(f), l.any.?("in"), l.any.?(l.comma))
                .each(f.militant.diff(l))(c => InvasiveEEEReconcileSelectAction(f, l :+ c).as("Add", c))
                .add(InvasiveEEEReconcileAction(f, l).as("Reconcile".styled(f), l.none.?("without flipping")))
                .cancel

        case InvasiveEEEReconcileAction(f, l) =>
            l.foreach(c => flip(f, c, MilitantEEE, PeacefulEEE))

            if (l.any)
                f.log("reconciled in", l./(_.elem).comma)
            else
                f.log("reconciled")

            InvasiveEEEReconcileDrawAction(f, l)

        case InvasiveEEEReconcileDrawAction(f, Nil) =>
            Next

        case InvasiveEEEReconcileDrawAction(f, c :: l) =>
            val r = factions.%(_.rules(c)).single

            if (r.any)
                DrawCardsAction(r.get, 1, NotInLog(NoMessage), AddCardsAction(r.get, InvasiveEEEReconcileDrawAction(f, l)))
            else
                InvasiveEEEReconcileDrawAction(f, l)

        // DAYLIGHT
        case DaylightNAction(50, f : InvasiveEEE) =>
            implicit val ask = builder

            if (f.acted < 3) {
                val l = clearings.%(c => settleable(f, c))

                + InvasiveEEESettleMainAction(f, l).!(l.none)

                + InvasiveEEEProvokeMainAction(f).!(provokeFlip(f).none && provokePlace(f).none)
            }

            + EndTurnSoftAction(f, "Turn", ForfeitActions(3 - f.acted))

            ask(f).daylight(f)

        case InvasiveEEEDoneAction(f, then) =>
            f.acted += 1

            then

        // Settle (16.5.1)
        case InvasiveEEESettleMainAction(f, l) =>
            Ask(f).group("Settle".styled(f), "in")
                .each(l)(c => InvasiveEEESettleClearingAction(f, c).as(c))
                .cancel

        case InvasiveEEESettleClearingAction(f, c) =>
            f.log("settled", c)

            InvasiveEEESettleMoveAction(f, c, $)

        case InvasiveEEESettleMoveAction(f, c, used) =>
            val plans = settleMoves(f, c, used)

            Ask(f).group("Settle".styled(f), "in", c, "- move in from")
                .add(plans.map { case (o, tt) => MoveToAction(f, f, tt, NoMessage, o, c, InvasiveEEESettleMoveAction(f, c, used :+ o)) })
                .add(InvasiveEEESettleEndAction(f, c).as("Done moving"))

        case InvasiveEEESettleEndAction(f, c) =>
            val battle = f.canAttackIn(c)
            val enclave = f.at(c).of[EnclaveEEE].none && f.rules(c) && f.available > 0 && f.canPlace(c)

            Ask(f).group("Settle".styled(f), "in", c, "- then")
                .add(BattleInitAction(f, f, NoMessage, $(c), $, InvasiveEEEDoneAction(f, Repeat)).as("Battle".styled(f), dt.Battle).!(battle.not))
                .add(InvasiveEEESettlePlaceAction(f, c).as("Place", PeacefulEEE.of(f), PeacefulEEE.imgd(f)).!(enclave.not))
                .add(InvasiveEEEDoneAction(f, Repeat).as("Done"))

        case InvasiveEEESettlePlaceAction(f, c) =>
            place(f, c, PeacefulEEE)

            f.log("placed", PeacefulEEE.of(f), "in", c)

            InvasiveEEEDoneAction(f, Repeat)

        // Provoke (16.5.2)
        case InvasiveEEEProvokeMainAction(f) =>
            Ask(f).group("Provoke".styled(f))
                .each(provokeFlip(f))(c => InvasiveEEEProvokeFlipAction(f, c).as("Flip", PeacefulEEE.of(f), "in", c))
                .each(provokePlace(f))(c => InvasiveEEEProvokePlaceAction(f, c).as("Place", MilitantEEE.of(f), MilitantEEE.imgd(f), "in", c))
                .cancel

        case InvasiveEEEProvokeFlipAction(f, c) =>
            flip(f, c, PeacefulEEE, MilitantEEE)

            f.log("provoked and flipped", MilitantEEE.of(f), "in", c)

            InvasiveEEEMusterAction(f, NoMessage, DiscardRandomCardAction(f, InvasiveEEEDoneAction(f, Repeat)))

        case InvasiveEEEProvokePlaceAction(f, c) =>
            place(f, c, MilitantEEE)

            f.log("provoked and placed", MilitantEEE.of(f), "in", c)

            InvasiveEEEMusterAction(f, NoMessage, DiscardRandomCardAction(f, InvasiveEEEDoneAction(f, Repeat)))

        // EVENING
        // Retaliate (16.6.1)
        case EveningNAction(20, f : InvasiveEEE) =>
            val l = f.militant.diff(f.retaliated).%(f.canAttackIn)

            if (l.any)
                Ask(f).group("Retaliate".styled(f), "at")
                    .each(l)(c => InvasiveEEERetaliateAction(f, c).as(c))
                    .evening(f)
            else
                Next

        case InvasiveEEERetaliateAction(f, c) =>
            f.retaliated :+= c

            BattleInitAction(f, f, NoMessage, $(c), $, Repeat)

        // Integrate (16.6.2)
        case EveningNAction(40, f : InvasiveEEE) =>
            if (f.integrated || f.hand.none)
                Next
            else
                YYSelectObjectsAction(f, f.hand)
                    .withGroup("Integrate".styled(f) ~ " - score " ~ 1.vp ~ " for each matching " ~ PeacefulEEE.of(f))
                    .withRule(d => FoxRabbitMouse.exists(d.matches))
                    .withThensInfo(d => FoxRabbitMouse.%(d.matches)./(s => InvasiveEEEIntegrateAction(f, d, s).as("Integrate", s, "with", d, "for", integration(f, s).vp)))(FoxRabbitMouse./(s => Info("Integrate", s, "for", integration(f, s).vp)))
                    .withExtra($(NoHand, Next.as("Skip")))
                    .ask
                    .evening(f)

        case InvasiveEEEIntegrateAction(f, d, s) =>
            f.integrated = true

            f.hand --> d --> discard.quiet

            f.nscore(integration(f, s))("integrating", s)(f, "integrated", s, "with", d, ForVP)

            Next

        case EveningNAction(60, f : InvasiveEEE) =>
            soft()

            Ask(f).evening(f).done(Next)

        // Draw and Discard (16.6.3)
        case NightStartAction(f : InvasiveEEE) =>
            EveningDrawAction(f, 1 + bonus(f.peaceful.num))

        case FactionCleanUpAction(f : InvasiveEEE) =>
            f.acted = 0
            f.retaliated = $
            f.integrated = false

            CleanUpAction(f)

        // NEGOTIATIONS
        case InvasiveEEENegotiateMainAction(f, e, then) =>
            Ask(f).group(Negotiations.of(e), "- flip", MilitantEEE.of(e), "to", PeacefulEEE.of(e))
                .each(e.militant.%(f.present))(c => InvasiveEEENegotiateAction(f, e, c, then).as(c))
                .cancel

        case InvasiveEEENegotiateAction(f, e, c, then) =>
            val r = factions.but(f).%(_.rules(c)).single

            if (r.any)
                YYSelectObjectsAction(f, f.hand)
                    .withGroup(Negotiations.of(e) ~ " in " ~ c.elem ~ " - give a card to " ~ r.get.elem)
                    .withThen(d => InvasiveEEENegotiateGiveAction(f, e, c, r.get, d, then))(d => "Give " ~ d.elem)("Give a card")
                    .withExtra($(NoHand, CancelAction))
            else
                Ask(f).add(InvasiveEEENegotiateFlipAction(f, e, c, then).as("Flip", MilitantEEE.of(e), "in", c)(Negotiations.of(e))).cancel

        case InvasiveEEENegotiateGiveAction(f, e, c, r, d, then) =>
            f.hand --> d --> r.hand

            f.log("gave a card to", r)

            r.notify($(ViewCardInfoAction(r, r.elem ~ " got from " ~ f.elem, d)))

            InvasiveEEENegotiateFlipAction(f, e, c, then)

        case InvasiveEEENegotiateFlipAction(f, e, c, then) =>
            e.negotiated :+= (game.turn, f)

            flip(e, c, MilitantEEE, PeacefulEEE)

            f.log("negotiated with", e, "and flipped", PeacefulEEE.of(e), "in", c)

            then

        // FROG CARDS
        // Laborers: at start of Evening, craft with Peaceful enclaves in matching clearings
        case EveningNAction(0, f) if factions.of[InvasiveEEE].exists(e => e.laborers.has(game.turn).not && laborersAssets(f, e).any) =>
            val e = factions.of[InvasiveEEE].%(e => e.laborers.has(game.turn).not && laborersAssets(f, e).any).first

            e.laborers = Some(game.turn)
            e.laborersUsed = $

            InvasiveEEELaborersMainAction(f, e)

        case InvasiveEEELaborersMainAction(f, e) =>
            val assets = laborersAssets(f, e)

            YYSelectObjectsAction(f, f.hand)
                .withGroup("Laborers".hl ~ " - craft with " ~ assets.diff(e.laborersUsed)./(dt.CraftSuit).merge)
                .withRule(d => d.is[CraftCard] && f.craftableWith(assets, e.laborersUsed)(d))
                .withThen(d => InvasiveEEELaborersCraftAction(f, e, d.as[CraftCard].get))(d => "Craft " ~ d.elem)("Craft")
                .withExtra($(NoHand, InvasiveEEELaborersDoneAction(f, e).as("Done")))
                .ask
                .evening(f)

        case InvasiveEEELaborersCraftAction(f, e, d) =>
            val assets = laborersAssets(f, e).diff(e.laborersUsed)
            val costs : $[SuitCost] = d.cost

            if (costs.diff(assets).none)
                Ask(f).done(InvasiveEEELaborersAssignAction(f, e, d, assets.intersect(costs)))
            else
            if (assets.distinct.num == 1)
                Ask(f).done(InvasiveEEELaborersAssignAction(f, e, d, assets.take(costs.num)))
            else
                XXSelectObjectsAction(f, assets./(ToCraft))
                    .withGroup("Craft " ~ d.elem ~ " using")
                    .withRule(_.num(costs.num).all(l => costs.permutations.exists(_.lazyZip(l).forall((c, a) => a.ref.matches(c)))))
                    .withThenElem(l => InvasiveEEELaborersAssignAction(f, e, d, l./(_.ref)))("Craft".hh)
                    .withExtra($(CancelAction))

        case InvasiveEEELaborersAssignAction(f, e, d, used) =>
            e.laborersUsed ++= used

            f.log("crafted with", "Laborers".hl)

            CraftPerformAction(f, d, NoMessage)

        case InvasiveEEELaborersDoneAction(f, e) =>
            Repeat

        // Advocates: at start of Birdsong, score for a suit
        case BirdsongNAction(10, f) if factions.of[InvasiveEEE].exists(e => f.has(FrogAdvocates(e))) =>
            val e = factions.of[InvasiveEEE].%(e => f.has(FrogAdvocates(e))).first

            Ask(f).group(FrogAdvocates(e), "- choose a clearing suit")
                .each(FoxRabbitMouse)(s => InvasiveEEEAdvocatesAction(f, e, s).as(s, "for", advocates(f, e, s).vp))

        case InvasiveEEEAdvocatesAction(f, e, s) =>
            f.nscore(advocates(f, e, s))("advocating", s)(f, "advocated", s, "with", FrogAdvocates(e), ForVP)

            f.removeStuckEffect(FrogAdvocates(e))

            Repeat

        // Companions: spend or reveal a frog card as another suit
        case InvasiveEEECompanionsMainAction(f, e, then) =>
            YYSelectObjectsAction(f, f.hand)
                .withGroup(FrogCompanions(e).elem ~ " - use a " ~ Frog.elem ~ " card as another suit")
                .withRule(_.suit == Frog)
                .withThens(d => disguiseSuits./(s => InvasiveEEECompanionsAction(f, e, d, s, then).as("As", s, "card")))
                .withExtra($(NoHand, CancelAction))

        case InvasiveEEECompanionsAction(f, e, d, s, then) =>
            f.hand --> d --> e.companions
            e.standins --> DisguisedCard(d, s) --> f.hand

            f.log("turned", d, "into a", s, "card with", FrogCompanions(e))

            f.removeStuckEffect(FrogCompanions(e))

            then

        // Settlers: force the Diaspora to move once
        case CraftPerformAction(f, d @ CraftEffectCard(_, _, _, FrogSettlers(e)), m) =>
            f.hand --> d --> discard.quiet

            f.log("sent", d, "(" ~ d.cost.ss ~ ")", m)

            val l = e.moveFrom.of[Clearing]

            if (l.any)
                MoveInitAction(f, e, $, WithEffect(FrogSettlers(e)), l, e.movable, $, Repeat)
            else {
                e.log("could not move")

                Repeat
            }

        // Stewards: draw and craft the top Pond card at no cost
        case CraftPerformAction(f, d @ CraftEffectCard(_, _, _, FrogStewards(e)), m) =>
            val top = pondTop(e).%(stewardable(f))

            f.hand --> d --> discard.quiet

            if (top.any) {
                val x = top.get

                e.pond --> x --> f.hand

                f.log("employed", d, "(" ~ d.cost.ss ~ ")", m, "and drew", x, "from the", "Pond".styled(e))

                CraftPerformAction(f, x, WithEffect(FrogStewards(e)))
            }
            else {
                f.log("employed", d, "(" ~ d.cost.ss ~ ")", m, "with no effect")

                Repeat
            }

        // Agitators: force the Diaspora to battle
        case CraftPerformAction(f, d @ CraftEffectCard(_, _, _, FrogAgitators(e)), m) =>
            f.hand --> d --> discard.quiet

            val l = clearings.%(e.canAttackIn)

            if (l.any) {
                f.log("sent", d, "(" ~ d.cost.ss ~ ")", m)

                Ask(f).group(FrogAgitators(e), "- force", e, "to battle")
                    .some(l)(c => e.canAttackList(c)./(t => InvasiveEEEAgitatorsAction(f, e, c, t).as(t, "in", c)))
                    .needOk
            }
            else {
                f.log("sent", d, "(" ~ d.cost.ss ~ ")", m, "with no effect")

                Repeat
            }

        case InvasiveEEEAgitatorsAction(f, e, c, t) =>
            Force(BattleStartAction(e, e, e, WithEffect(FrogAgitators(e)), c, t, None, Repeat))

        // Militias: flip a Peaceful enclave or place a Militant one
        case CraftPerformAction(f, d @ CraftEffectCard(_, _, _, FrogMilitias(e)), m) =>
            f.hand --> d --> discard.quiet

            val lf = e.peaceful
            val lp = (e.available > 0).??(clearings.diff(game.scorched).diff(game.flooded).%(c => e.at(c).of[EnclaveEEE].none))

            if (lf.any || lp.any) {
                f.log("raised", d, "(" ~ d.cost.ss ~ ")", m)

                Ask(f).group(FrogMilitias(e))
                    .each(lf)(c => InvasiveEEEMilitiasFlipAction(f, e, c).as("Flip", PeacefulEEE.of(e), "in", c))
                    .each(lp)(c => InvasiveEEEMilitiasPlaceAction(f, e, c).as("Place", MilitantEEE.of(e), MilitantEEE.imgd(e), "in", c))
                    .needOk
            }
            else {
                f.log("raised", d, "(" ~ d.cost.ss ~ ")", m, "with no effect")

                Repeat
            }

        case InvasiveEEEMilitiasFlipAction(f, e, c) =>
            flip(e, c, PeacefulEEE, MilitantEEE)

            f.log("flipped", MilitantEEE.of(e), "in", c)

            Repeat

        case InvasiveEEEMilitiasPlaceAction(f, e, c) =>
            place(e, c, MilitantEEE)

            f.log("placed", MilitantEEE.of(e), "in", c)

            Repeat

        // Assimilationists: remove a Peaceful enclave you rule, both draw, no Fears Come to Pass
        case CraftPerformAction(f, d @ CraftEffectCard(_, _, _, FrogAssimilationists(e)), m) =>
            f.hand --> d --> discard.quiet

            val l = e.peaceful.%(f.rules)

            if (l.any) {
                f.log("welcomed", d, "(" ~ d.cost.ss ~ ")", m)

                Ask(f).group(FrogAssimilationists(e), "- remove", PeacefulEEE.of(e), "in")
                    .each(l)(c => InvasiveEEEAssimilateAction(f, e, c).as(c))
                    .needOk
            }
            else {
                f.log("welcomed", d, "(" ~ d.cost.ss ~ ")", m, "with no effect")

                Repeat
            }

        case InvasiveEEEAssimilateAction(f, e, c) =>
            e.calm = true

            val draw = DrawCardsAction(f, 1, WithEffect(FrogAssimilationists(e)), AddCardsAction(f, (f != e).?(DrawCardsAction(e, 1, WithEffect(FrogAssimilationists(e)), AddCardsAction(e, Repeat)) : ForcedAction).|(Repeat)))

            TryForcedRemoveAction(f, c, e, PeacefulEEE, (f != e).??(1), Removing, ForcedRemoveFinishedAction(f, draw), draw)

        case _ => UnknownContinue
    }

    def settleMoves(f : InvasiveEEE, c : Clearing, used : $[Clearing])(implicit game : Game) : $[(Clearing, $[$[Transport]])] = {
        val from = f.movable.of[Clearing].diff(used).but(c).%(f.canMoveFrom)
        val plans = f.movePlans(from, game.transports./($) ** f.transports)

        from./~{ o =>
            val p = plans.get(o)
            val tt : $[$[Transport]] = p.filter(_._2.has(c)).map(_._1).getOrElse(Nil).filter(_.forall(_.allows(f, o, c)))
            tt.any.?((o : Clearing, tt))
        }
    }

    def settleable(f : InvasiveEEE, c : Clearing)(implicit game : Game) : Boolean =
        f.validDest.has(c) && (f.at(c).of[Warrior].any || settleMoves(f, c, $).any)

    def provokeFlip(f : InvasiveEEE)(implicit game : Game) = f.peaceful

    def provokePlace(f : InvasiveEEE)(implicit game : Game) =
        (f.available > 0).??(clearings.%(c => f.at(c).of[EnclaveEEE].none).%(c => game.riverside.has(c) || f.at(c).of[Warrior].any).%(f.canPlace))

    def integration(f : InvasiveEEE, s : BaseSuit)(implicit game : Game) = f.peaceful.%(c => game.original(c).has(s)).num

    def advocates(f : Faction, e : InvasiveEEE, s : BaseSuit)(implicit game : Game) = e.peaceful.%(c => game.original(c).has(s)).%(f.rules).num

    def stewardable(f : Faction)(d : DeckCard)(implicit game : Game) : Boolean = d @@ {
        case d : CraftItemCard => game.uncrafted.has(d.item)
        case d : CraftEffectCard => f.has(d.effect).not
        case d : Favor => true
        case _ => false
    }
}
