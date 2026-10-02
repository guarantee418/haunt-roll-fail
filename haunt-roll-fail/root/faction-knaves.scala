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

// Knaves of the Deepwood, as published in Root: Homeland (Law of Root, section 18)

trait Knaves extends WarriorFaction with CommonAbduct {
    val clashKey = KDvA

    val warrior = KnavesSkunk

    def abilities(options : $[Meta.O]) = $(KnavesDeepwoodRunners, KnavesFollowMe, KnavesHaveAtThee, KnavesRunAway)

    def pieces(options : $[Meta.O]) = KnavesSkunk *** 10 ++ KnavesCaptain(0) *** 1 ++ KnavesCaptain(1) *** 1 ++ KnavesCaptain(2) *** 1 ++ Acclaim *** Knaves.maxAcclaim

    override val transports : $[$[Transport]] = $($(KnavesDeepwoodMove))

    override def note : Elem = HorizontalBreak ~ "Homeland"

    def advertising = Acclaim.img(this) ~ KnavesSkunk.img(this) ~ Acclaim.img(this) ~ KnavesSkunk.img(this) ~ Acclaim.img(this)

    def motto = "Mock".styled(this)
}

case object KD extends Knaves {
    val name = "Knaves of the Deepwood"
    override def funName = "Knaves of the " ~ NameReference(name, this)
    val short = "KD"
    val style = "KD"
    val priority = "U"
}

case object KnavesSkunk extends Warrior with CommonSkunkWarrior {
    override def id = "Skunk"
    override def name = "Skunk"
}

case class KnavesCaptain(slot : Int) extends Warrior with Rearguard {
    override def ofg(f : Faction)(implicit g : Game) : Elem = f.as[Knaves]./(f => f.captains.lift(slot)./(_.name.styled(f)).|("Captain".styled(f))).|(super.ofg(f))

    override def id = "captain-" + $("Birdsong", "Daylight", "Evening")(slot)
    override def name = "Captain"
}

case object Acclaim extends Token {
    override def id = "Acclaim"
    override def name = "Acclaim"
    override def plural = "Acclaim"
}

case object KnavesDeepwoodRunners extends FactionEffect {
    val name = "Deepwood Runners"
}

case object KnavesFollowMe extends FactionEffect {
    val name = "Follow Me"
}

case object KnavesHaveAtThee extends FactionEffect {
    val name = "Have at Thee"
}

case object KnavesRunAway extends FactionEffect {
    val name = "Run Away"
}

// once per turn
case object KnavesFilched extends HiddenEffect
case object KnavesVagrantFlip extends HiddenEffect
case object KnavesRangerFlip extends HiddenEffect

// during one battle
case object KnavesBattleStarted extends BattleEffect
case object KnavesActingAttack extends BattleEffect
case object KnavesPrisonersTaken extends BattleEffect
case object KnavesAssaulting extends BattleEffect
case object KnavesSkirmishing extends BattleEffect
case object KnavesNabbing extends BattleEffect
case object KnavesBonusAsked extends BattleEffect


abstract class KnaveCaptain(val name : String, val items : $[Item], val text : String) extends Record with Elementary {
    def elem = name.styled(KD).styled(xstyles.bold)
}

case object KnaveHarrier    extends KnaveCaptain("Harrier",    $(Boots, Crossbow), "When you Dash, you may move the Harrier up to three times, ignoring rule.")
case object KnaveTinker     extends KnaveCaptain("Tinker",     $(Bag, Hammer),     "After you Serve, draw 1 card.")
case object KnaveVagrant    extends KnaveCaptain("Vagrant",    $(Teapot, Coins),   "Once per turn, as an action, you may flip any item down to Revel, Gift, or Serve.")
case object KnaveThief      extends KnaveCaptain("Thief",      $(Boots, Bag),      "After you Filch, you may move the Thief.")
case object KnaveScoundrel  extends KnaveCaptain("Scoundrel",  $(Crossbow, Teapot), "When you Skirmish, you may instead move from a clearing before the battle. If you do, do not ignore 1 hit.")
case object KnaveRonin      extends KnaveCaptain("Ronin",      $(Boots, Sword),    "When you Assault, the Ronin may move before the battle.")
case object KnaveJailor     extends KnaveCaptain("Jailor",     $(Crossbow, Bag),   "In Nab battles, you may deal 1 less hit to ignore 1 rolled hit you take.")
case object KnaveRanger     extends KnaveCaptain("Ranger",     $(Sword, Crossbow), "Once per turn, as an action, you may flip any item down to Assault, Skirmish, or Nab.")
case object KnaveArbiter    extends KnaveCaptain("Arbiter",    $(Sword, Coins),    "In Assault battles, you may take 1 extra hit to deal 1 extra hit.")
case object KnaveCheat      extends KnaveCaptain("Cheat",      $(Boots, Teapot),   "As an action, you may flip two items down to take any item action.")
case object KnaveGladiator  extends KnaveCaptain("Gladiator",  $(Sword, Hammer),   "When you Assault, draw 1 card at the start of battle.")
case object KnaveAdventurer extends KnaveCaptain("Adventurer", $(Hammer, Coins),   "After you place acclaim at a ruin, draw 1 card.")


abstract class KnavesItemPower(val name : String, val item : Item) extends Record with Elementary {
    def elem = name.styled(KD).styled(xstyles.bold)
}

case object KnavesDash     extends KnavesItemPower("Dash", Boots)
case object KnavesAssault  extends KnavesItemPower("Assault", Sword)
case object KnavesSkirmish extends KnavesItemPower("Skirmish", Crossbow)
case object KnavesNab      extends KnavesItemPower("Nab", Bag)
case object KnavesRevel    extends KnavesItemPower("Revel", Teapot)
case object KnavesGift     extends KnavesItemPower("Gift", Coins)
case object KnavesServe    extends KnavesItemPower("Serve", Hammer)


trait KnavesPending extends Record
case object KnavesThiefMove extends KnavesPending
case object KnavesNabMove extends KnavesPending
case object KnavesServeCraft extends KnavesPending


case class KnavesWith(a : KnavesItemPower, ignore : Boolean) extends Message {
    def elem(implicit game : Game) = " with " ~ a.elem
}

case object KnavesFilchMessage extends Message {
    def elem(implicit game : Game) = " with " ~ "Filch".styled(KD).styled(xstyles.bold)
}


object Knaves {
    val maxAcclaim = 8
    val maxActions = 4

    val captains = $[KnaveCaptain](KnaveHarrier, KnaveTinker, KnaveVagrant, KnaveThief, KnaveScoundrel, KnaveRonin, KnaveJailor, KnaveRanger, KnaveArbiter, KnaveCheat, KnaveGladiator, KnaveAdventurer)

    val itemActions = $[KnavesItemPower](KnavesDash, KnavesAssault, KnavesSkirmish, KnavesNab, KnavesRevel, KnavesGift, KnavesServe)
}


// Deepwood Runners (18.2.4): moves in, out of and between forests ignore rule
case object KnavesDeepwoodMove extends Transport {
    override def allows(f : Faction, o : Region, d : Region)(implicit game : Game) = (o, d) @@ {
        case (o : Clearing, d : Clearing) => RuledMove.allows(f, o, d)
        case _ => true
    }

    override def allows(f : Faction, o : Region, d : Region, l : $[Movable])(implicit game : Game) = allows(f, o, d) && l.of[KnavesCaptain].num <= 1

    override def sortBy(m : Movable) : Int = m.is[KnavesCaptain].??(-1)
}

// Follow Me (18.2.5): a move with the Acting Captain, Skunks may come along
case class KnavesCaptainMove(slot : Int) extends Transport {
    override def allows(f : Faction, o : Region)(implicit game : Game) = f @@ {
        case f : Knaves => f.at(o).has(KnavesCaptain(slot))
        case _ => false
    }

    override def allows(f : Faction, o : Region, d : Region, l : $[Movable])(implicit game : Game) = allows(f, o, d) && l.has(KnavesCaptain(slot)) && l.of[KnavesCaptain].num == 1

    override def order(l : $[Movable]) : Int = l.num

    override def sortBy(m : Movable) : Int = m.is[KnavesCaptain].??(-1)
}

case object KnavesToClearing extends Transport {
    override def allows(f : Faction, o : Region, d : Region)(implicit game : Game) = d.is[Clearing]
}


class KnavesPlayer(val faction : Knaves)(implicit val game : Game) extends FactionState {
    board.forests.foreach(r => location(r))
    board.forests.foreach(r => location(Cage(r), u => u.piece.is[Warrior] && u.piece.is[Tenacious].not))

    val figures : $[Figure] = 0.until(3).$./(i => reserve.$.%(_.piece == KnavesCaptain(i)).only)

    var captains : $[KnaveCaptain] = $
    var retired : $[KnaveCaptain] = $
    var acting : |[KnaveCaptain] = None

    var actions : Int = 0

    var pending : $[KnavesPending] = $

    var activated : $[Clearing] = $
    var serving : $[Clearing] = $
    var filchCrafting : Boolean = false
    var nabEscaped : Boolean = false

    var protectedAcclaim : $[Clearing] = $

    var pool : $[SuitAsset] = $

    def craft = pool

    def piece(k : KnaveCaptain) = KnavesCaptain(captains.indexOf(k))

    def figure(k : KnaveCaptain) = figures(captains.indexOf(k))

    def where(k : KnaveCaptain) : |[Region] = game.pieces.find(figure(k))./(_._2).%(r => r.is[Clearing] || r.is[Forest])

    def actingWhere : |[Region] = acting./~(where)

    def is(k : KnaveCaptain) = acting.has(k)

    def faceUp : $[KnaveCaptain] = captains.diff(retired)

    def prisoners : $[Figure] = board.forests./~(r => from(Cage(r)).$)

    def prisonersOf(e : Faction) : $[Figure] = prisoners.%(_.faction == e)

    def freeForests(c : Clearing) : $[Forest] = board.forests.%(r => board.fromForest(r).has(c)).%(r => from(Cage(r)).none)

    def stash : $[Item] = forTrade.%(i => i.exhausted.not && i.damaged.not)./(_.item)

    def allr(p : Piece) : $[Region] = (clearings ++ board.forests)./~(c => at(c).count(p).times(c))

    def plog(s : Any*) = game.log((acting./(_.elem).|(faction.elem) +: s.$) : _*)
}


case class KnavesChooseCaptainAction(self : Knaves, k : KnaveCaptain) extends BaseAction("Choose", 3.hl, "Captains")(k.elem, k.items./(_.img).merge, Break, k.text)
case class KnavesStartingForestAction(self : Knaves, k : KnaveCaptain, r : Forest) extends BaseAction(implicit g => k.elem, "and a", "Skunk".styled(self), "start in")(implicit g => board.forestName(r))

case class KnavesReadyMainAction(self : Knaves) extends ForcedAction
case class KnavesReadyAction(self : Knaves, k : KnaveCaptain) extends BaseAction("Ready".styled(self), "a Captain")(k.elem, k.items./(_.img).merge, Break, k.text)
case class KnavesReadyPlaceAction(self : Knaves, k : KnaveCaptain, r : Forest) extends ForcedAction

case class KnavesActsMainAction(self : Knaves) extends ForcedAction with Soft
case class KnavesSpendAction(self : Knaves, then : ForcedAction) extends ForcedAction
case class KnavesCommitAction(self : Knaves, a : KnavesItemPower, flips : $[Item], via : $[Effect], then : ForcedAction) extends ForcedAction
case class KnavesPendingDoneAction(self : Knaves, p : KnavesPending) extends ForcedAction

case class KnavesItemMainAction(self : Knaves, a : KnavesItemPower, l : $[($[Item], $[Effect])]) extends ForcedAction with Soft
case class KnavesItemAction(self : Knaves, a : KnavesItemPower, flips : $[Item], via : $[Effect]) extends ForcedAction with Soft
case class KnavesCheatMainAction(self : Knaves) extends ForcedAction with Soft
case class KnavesCheatItemsAction(self : Knaves, a : KnavesItemPower) extends ForcedAction with Soft

case class KnavesFilchMainAction(self : Knaves) extends ForcedAction with Soft
case class KnavesFilchTakeAction(self : Knaves, e : Faction, i : ItemRef) extends ForcedAction
case class KnavesCraftMenuAction(self : Knaves, assets : $[SuitAsset], serve : Boolean) extends ForcedAction with Soft
case class KnavesCraftAction(self : Knaves, d : DeckCard, assets : $[SuitAsset], serve : Boolean) extends ForcedAction

case class KnavesDashMoreAction(self : Knaves, n : Int) extends ForcedAction with Soft
case class KnavesAssaultAfterMoveAction(self : Knaves) extends ForcedAction with Soft
case class KnavesSkirmishBattleAction(self : Knaves, ignore : Boolean) extends ForcedAction with Soft
case class KnavesNabAfterAction(self : Knaves) extends ForcedAction
case class KnavesRevelAction(self : Knaves) extends ForcedAction
case class KnavesGiftAction(self : Knaves) extends ForcedAction
case class KnavesServeAction(self : Knaves) extends ForcedAction
case class KnavesTinkerAction(self : Knaves, then : ForcedAction) extends ForcedAction

case class KnavesPlaceAcclaimAction(self : Knaves, c : Clearing, then : ForcedAction) extends ForcedAction

case class KnavesBattleBonusAction(self : Knaves, b : Battle, f : Faction, o : Faction, fs : Int, os : Int, fr : Int, or : Int, fh : Int, oh : Int, fe : Int, oe : Int) extends ForcedAction
case class KnavesPrisonersAction(self : Knaves, b : Battle, n : Int) extends ForcedAction
case class KnavesTakePrisonerAction(self : Knaves, b : Battle, p : Piece, r : Forest, n : Int) extends ForcedAction
case class KnavesAfterBattleAcclaimAction(self : Knaves, b : Battle) extends ForcedAction
case class KnavesNabEscapeAction(self : Knaves, c : Clearing, p : KnavesCaptain, r : Forest, then : ForcedAction) extends ForcedAction
case class KnavesRunAwayAction(self : Faction, f : Knaves, c : Clearing, r : Forest, then : ForcedAction) extends ForcedAction

case class KnavesMockAction(self : Knaves) extends ForcedAction
case class KnavesProtectMainAction(self : Knaves) extends ForcedAction with Soft
case class KnavesProtectCardAction(self : Knaves, c : Clearing, d : DeckCard) extends BaseAction("Protect the Weak".styled(self), "in", c, "with")(d.img) with ViewCard
case class KnavesTakeItEasyAction(self : Knaves) extends ForcedAction
case class KnavesFreePrisonersMainAction(self : Knaves, e : Faction) extends ForcedAction
case class KnavesFreePrisonersAction(self : Faction, f : Knaves, c : Clearing) extends ForcedAction


object KnavesExpansion extends FactionExpansion[Knaves] {
    override def extraMoveFrom(f : Faction)(implicit game : Game) = f @@ {
        case f : Knaves => board.forests
        case _ => $()
    }

    def adjacentForests(c : Clearing)(implicit game : Game) : $[Forest] = board.forests.%(r => board.fromForest(r).has(c))

    def nearbyForests(r : Forest)(implicit game : Game) : $[Forest] = board.forests.%(x => x == r || board.forestsConnected(r, x))

    def moves(f : Knaves)(implicit game : Game) : $[$[Transport]] = game.transports./($) ** $($(KnavesDeepwoodMove, KnavesCaptainMove(f.captains.indexOf(f.acting.get))))
    def dashes(f : Knaves)(implicit game : Game) : $[$[Transport]] = game.transports./($) ** $($(KnavesCaptainMove(f.captains.indexOf(f.acting.get))))
    def skirmishes(f : Knaves)(implicit game : Game) : $[$[Transport]] = game.transports./($) ** $($(KnavesDeepwoodMove, KnavesCaptainMove(f.captains.indexOf(f.acting.get)), KnavesToClearing))

    def canMove(f : Knaves, tt : $[$[Transport]])(implicit game : Game) : Boolean = f.actingWhere.?(r => f.canMoveFrom(r) && f.movePlans($(r), tt).contains(r))

    def canAcclaim(f : Knaves, c : Clearing, battle : Boolean)(implicit game : Game) : Boolean = f.pool(Acclaim) && f.at(c).has(Acclaim).not && (f.canPlace(c) || (battle && Council.governs(f, c)))

    def flip(f : Knaves, l : $[Item])(implicit game : Game) {
        l.foreach { i =>
            f.forTrade.%(r => r.item == i && r.exhausted.not && r.damaged.not).starting.foreach { r =>
                f.forTrade = f.forTrade :- r
                f.forTrade :+= r.exhaust
            }
        }
    }

    // the ways to pay for an item action: its own item, or another one with Vagrant, Ranger or Cheat
    def payments(f : Knaves, a : KnavesItemPower)(implicit game : Game) : $[($[Item], $[Effect])] = {
        val up = f.stash

        val own = up.has(a.item).$(($(a.item), $[Effect]()))

        val vagrant = (f.is(KnaveVagrant) && f.used.has(KnavesVagrantFlip).not && $[KnavesItemPower](KnavesRevel, KnavesGift, KnavesServe).has(a)).??(up.distinct.but(a.item)./(i => ($(i), $[Effect](KnavesVagrantFlip))))
        val ranger = (f.is(KnaveRanger) && f.used.has(KnavesRangerFlip).not && $[KnavesItemPower](KnavesAssault, KnavesSkirmish, KnavesNab).has(a)).??(up.distinct.but(a.item)./(i => ($(i), $[Effect](KnavesRangerFlip))))

        own ++ vagrant ++ ranger
    }

    def cheats(f : Knaves)(implicit game : Game) : $[$[Item]] = f.is(KnaveCheat).??(f.stash.combinations(2).$./(_.sortBy(i => Item.order.indexOf(i))).distinct)

    // can the item action do anything right now
    def possible(f : Knaves, a : KnavesItemPower)(implicit game : Game) : Boolean = {
        val r = f.actingWhere
        val c = r./~(_.as[Clearing])

        a @@ {
            case KnavesDash => canMove(f, dashes(f))
            case KnavesAssault => c.?(c => f.canAttackList(c).any) || (f.is(KnaveRonin) && canMove(f, moves(f)))
            case KnavesSkirmish => (r.?(_.is[Forest]) || (f.is(KnaveScoundrel) && c.any)) && canMove(f, skirmishes(f))
            case KnavesNab => c.?(c => f.canAttackList(c).any)
            case KnavesRevel => r.any
            case KnavesGift => r.any
            case KnavesServe => c.any
        }
    }

    // Revel, Gift and Serve happen at once, the others go through a move or battle prompt first
    def entry(f : Knaves, a : KnavesItemPower, flips : $[Item], via : $[Effect])(implicit game : Game) : ForcedAction = a match {
        case KnavesRevel => KnavesCommitAction(f, a, flips, via, KnavesRevelAction(f))
        case KnavesGift => KnavesCommitAction(f, a, flips, via, KnavesGiftAction(f))
        case KnavesServe => KnavesCommitAction(f, a, flips, via, KnavesServeAction(f))
        case _ => KnavesItemAction(f, a, flips, via)
    }

    def drawAfter(f : Knaves, n : Int, then : ForcedAction)(implicit game : Game) : ForcedAction =
        (n > 0).?(DrawCardsAction(f, n, NoMessage, AddCardsAction(f, then))).|(then)

    def perform(action : Action, soft : Void)(implicit game : Game) = action @@ {
        // SETUP
        case CreatePlayerAction(f : Knaves) =>
            game.states += f -> new KnavesPlayer(f)

            FactionInitAction(f)

        case FactionSetupAction(f : Knaves) if f.captains.num < 3 =>
            Ask(f).each(Knaves.captains.diff(f.captains))(k => KnavesChooseCaptainAction(f, k)).needOk

        case KnavesChooseCaptainAction(f, k) =>
            f.captains :+= k

            f.forTrade ++= k.items./(_.pristine)

            f.log("chose", k, "as a Captain, with", k.items./(_.elem).comma)

            FactionSetupAction(f)

        case FactionSetupAction(f : Knaves) if f.captains.exists(k => f.where(k).none) =>
            val k = f.captains.%(k => f.where(k).none).first

            Ask(f).each(board.forests.%(r => f.at(r).none))(r => KnavesStartingForestAction(f, k, r)).needOk

        case FactionSetupAction(f : Knaves) =>
            SetupFactionsAction

        case KnavesStartingForestAction(f, k, r) =>
            f.reserve --> f.piece(k) --> r
            f.reserve --> f.warrior --> r

            f.log("placed", k, "and", f.warrior.of(f), "in", r)

            FactionSetupAction(f)

        // HELPER
        case BattlePostHitInAction(b, e, f : Knaves, KnavesSkunk, then) =>
            e.log("routed", KnavesSkunk.of(f))

            then

        case BattlePostHitInAction(b, e, f : Knaves, p : KnavesCaptain, then) =>
            e.log("routed", p.ofg(f))

            then

        case BattlePostHitInAction(b, e, f : Knaves, Acclaim, then) =>
            e.log("mocked away", Acclaim.of(f))

            then

        // Run Away (18.2.7)
        case ForcedRemoveTargetEffectAction(e, c, f : Knaves, Acclaim, then) if e != f && f.pool(f.warrior) =>
            Ask(e.as[Hireling]./~(_.owner).|(e)).group(e, "makes", f.warrior.of(f), "run away from", c, "to")
                .each(adjacentForests(c))(r => KnavesRunAwayAction(e, f, c, r, then).as(r))
                .needOk

        case KnavesRunAwayAction(e, f, c, r, then) =>
            f.reserve --> f.warrior --> r

            f.log("ran away from", c, "to", r, "with", KnavesRunAway.of(f))

            then

        // Nab (18.5.1.IVd): a hit Acting Captain escapes to an adjacent forest
        case ForcedRemoveTargetEffectAction(e, c, f : Knaves, p : KnavesCaptain, then) if f.used.has(KnavesNabbing) && f.acting.map(f.piece).has(p) && adjacentForests(c).any =>
            Ask(f).group(p.ofg(f), "escapes from", c, "to")
                .each(adjacentForests(c))(r => KnavesNabEscapeAction(f, c, p, r, then).as(r))
                .needOk

        case KnavesNabEscapeAction(f, c, p, r, then) =>
            f.limbo(c) --> p --> r

            f.nabEscaped = true

            f.log("escaped to", r)

            then

        // battle modes, Have at Thee and Gladiator
        case BattleStartAction(self, f : Knaves, a, m, c, o, i, then) if f.used.has(KnavesBattleStarted).not =>
            f.used :+= KnavesBattleStarted

            f.nabEscaped = false

            if (f.actingWhere.has(c))
                f.used :+= KnavesActingAttack

            m @@ {
                case KnavesWith(KnavesAssault, _) => f.used :+= KnavesAssaulting
                case KnavesWith(KnavesSkirmish, true) => f.used :+= KnavesSkirmishing
                case KnavesWith(KnavesNab, _) => f.used :+= KnavesNabbing
                case _ =>
            }

            val q = BattleStartAction(self, f, a, m, c, o, i, then)

            if (f.used.has(KnavesAssaulting) && f.is(KnaveGladiator)) {
                f.plog("drew a card at the start of the battle")

                DrawCardsAction(f, 1, NoMessage, AddCardsAction(f, q))
            }
            else
                q

        // Skirmish (18.5.1.IVc): ignore the first hit
        case BattleAssignHitsAction(f : Knaves, b, n, s, then) if n + s > 0 && f.used.has(IgnoreFirstHit).not && f.used.has(KnavesSkirmishing) =>
            f.used :+= IgnoreFirstHit

            f.log("ignored the first hit while skirmishing")

            BattleAssignHitsAction(f, b, n - (n > 0).??(1), s - (n == 0).??(1), then)

        // Arbiter and Jailor
        case BattleBonusAction(b, f : Knaves, o, fs, os, fr, or, fh, oh, fe, oe) if f.used.has(KnavesBonusAsked).not && (
            (f.used.has(KnavesAssaulting) && f.is(KnaveArbiter)) ||
            (f.used.has(KnavesNabbing) && f.is(KnaveJailor) && oh > 0 && fh + fe > 0)
        ) =>
            f.used :+= KnavesBonusAsked

            val q = BattleBonusAction(b, f, o, fs, os, fr, or, fh, oh, fe, oe)

            if (f.used.has(KnavesAssaulting))
                Ask(f).group(KnaveArbiter, "in", "Assault".styled(f))
                    .add(KnavesBattleBonusAction(f, b, f, o, fs, os, fr, or, fh, oh, fe + 1, oe + 1).as("Take", 1.hit, "to deal", 1.hit))
                    .skip(q)
            else
                Ask(f).group(KnaveJailor, "in", "Nab".styled(f))
                    .add(KnavesBattleBonusAction(f, b, f, o, fs, os, fr, or, fh - (fe == 0).??(1), oh - 1, fe - (fe > 0).??(1), oe).as("Deal", 1.hit, "less to ignore", 1.hit))
                    .skip(q)

        case KnavesBattleBonusAction(f, b, ff, o, fs, os, fr, or, fh, oh, fe, oe) =>
            if (f.is(KnaveArbiter) && f.used.has(KnavesAssaulting))
                f.plog("took an extra", 1.hit, "to deal an extra", 1.hit)
            else
                f.plog("dealt", 1.hit, "less to ignore", 1.hit)

            BattleBonusAction(b, ff, o, fs, os, fr, or, fh, oh, fe, oe)

        // Have at Thee (18.2.6)
        case BattleCleanupAttackerAction(b, f : Knaves) if f.used.has(KnavesActingAttack) && f.used.has(KnavesPrisonersTaken).not =>
            f.used :+= KnavesPrisonersTaken

            KnavesPrisonersAction(f, b, f.used.has(KnavesAssaulting).?(99).|(1))

        case KnavesPrisonersAction(f, b, n) =>
            val c = b.clearing
            val l = b.defender.from(Limbo(c)).$.%(_.piece.is[Warrior]).%(_.piece.is[Tenacious].not)./(_.piece).distinct
            val rr = f.freeForests(c)

            if (n > 0 && l.any && rr.any)
                Ask(f).group("Take a prisoner".styled(f), "with", KnavesHaveAtThee.of(f))
                    .some(l)(p => rr./(r => KnavesTakePrisonerAction(f, b, p, r, n).as(p.of(b.defender), "to", r)))
                    .needOk
            else
                KnavesAfterBattleAcclaimAction(f, b)

        case KnavesTakePrisonerAction(f, b, p, r, n) =>
            b.defender.from(Limbo(b.clearing)) --> $(p) --> (f, Cage(r))

            f.log("took", p.of(b.defender), "prisoner to", r)

            KnavesPrisonersAction(f, b, n - 1)

        case KnavesAfterBattleAcclaimAction(f, b) =>
            val q = BattleCleanupAttackerAction(b, f)

            if (f.actingWhere.has(b.clearing) && canAcclaim(f, b.clearing, true))
                KnavesPlaceAcclaimAction(f, b.clearing, q)
            else
                q

        case KnavesPlaceAcclaimAction(f, c, then) =>
            f.reserve --> Acclaim --> c

            f.log("placed", Acclaim.of(f), "in", c)

            if (f.is(KnaveAdventurer) && game.ruins.contains(c)) {
                f.plog("drew a card for acclaim at a ruin")

                drawAfter(f, 1, then)
            }
            else
                then

        // BIRDSONG
        case BirdsongNAction(40, f : Knaves) =>
            KnavesReadyMainAction(f)

        case KnavesReadyMainAction(f) =>
            if (f.faceUp.any)
                Ask(f).each(f.faceUp)(k => KnavesReadyAction(f, k)).needOk
            else
                NoAsk(f)(Next)

        case KnavesReadyAction(f, k) =>
            f.acting = Some(k)
            f.actions = Knaves.maxActions

            f.log("readied", k)

            if (f.where(k).none)
                Ask(f).group(k, "enters", "any forest")
                    .each(board.forests)(r => KnavesReadyPlaceAction(f, k, r).as(r))
                    .needOk
            else
                Next

        case KnavesReadyPlaceAction(f, k, r) =>
            f.figure(k) --> r

            f.actions = Knaves.maxActions - 1

            f.log("placed", k, "in", r)

            val l = nearbyForests(r)./~(x => f.from(Cage(x)).$)

            if (l.any) {
                l.foreach(_ --> game.pool)

                f.log("released", l./(_.elem).comma)
            }

            Next

        // DAYLIGHT
        case DaylightNAction(40, f : Knaves) =>
            KnavesActsMainAction(f)

        case KnavesActsMainAction(f) if f.acting.none =>
            NoAsk(f)(Next)

        case KnavesActsMainAction(f) if f.pending.any =>
            val k = f.acting.get
            val g = k.elem

            f.pending.first @@ {
                case KnavesThiefMove =>
                    Ask(f).group(k, "may move after", "Filch".styled(f))
                        .add(MoveInitAction(f, f, moves(f), NoMessage, f.actingWhere.$, f.actingWhere.$, $(KnavesPendingDoneAction(f, KnavesThiefMove).as("Skip")), KnavesPendingDoneAction(f, KnavesThiefMove)).as("Move".styled(f))(g).!(canMove(f, moves(f)).not))
                        .add(KnavesPendingDoneAction(f, KnavesThiefMove).as("Skip"))

                case KnavesNabMove =>
                    Ask(f).group(k, "may move after", "Nab".styled(f))
                        .add(MoveInitAction(f, f, moves(f), NoMessage, f.actingWhere.$, f.actingWhere.$, $(KnavesPendingDoneAction(f, KnavesNabMove).as("Skip")), KnavesPendingDoneAction(f, KnavesNabMove)).as("Move".styled(f))(g).!(canMove(f, moves(f)).not))
                        .add(KnavesPendingDoneAction(f, KnavesNabMove).as("Skip"))

                case KnavesServeCraft =>
                    val l = f.serving.diff(f.activated)

                    Ask(f).group(KnavesServe, "activates acclaim to craft")
                        .add(KnavesCraftMenuAction(f, l./(_.asset), true).as("Craft".styled(f), l./(c => dt.CraftSuit(c.asset)).merge)(g).!(f.hand.%(f.craftableWith(l./(_.asset), $)).none, "nothing craftable"))
                        .add(KnavesPendingDoneAction(f, KnavesServeCraft).as("Done"))
            }

        case KnavesActsMainAction(f) =>
            implicit val ask = builder

            val k = f.acting.get
            val g = k.elem ~ " acts " ~ ("(" + f.actions + " action" + (f.actions != 1).??("s") + " left)").hl

            val r = f.actingWhere
            val c = r./~(_.as[Clearing])

            if (f.actions > 0) {
                + MoveInitAction(f, f, moves(f), NoMessage, r.$, r.$, $(CancelAction), KnavesSpendAction(f, Repeat)).as("Move".styled(f))(g).!(canMove(f, moves(f)).not)

                + BattleInitAction(f, f, NoMessage, c.$, $(CancelAction), KnavesSpendAction(f, Repeat)).as("Battle".styled(f))(g).!(c.none, "in a forest").!(c.?(c => f.canAttackList(c).none), "no targets")

                val filch = c.?(c => f.hand.exists(f.craftableWith($(c.asset), $)) || f.enemies.%(_.present(c)).%(_.is[Hero].not).exists(_.forTrade.any))

                + KnavesFilchMainAction(f).as("Filch".styled(f))(g).!(f.used.has(KnavesFilched), "once per turn").!(c.none, "in a forest").!(filch.not, "nothing to filch")

                Knaves.itemActions.foreach { a =>
                    val l = payments(f, a)

                    if (l.num == 1)
                        + entry(f, a, l.only._1, l.only._2).as(a.item.img, dt.Arrow, a)(g).!(possible(f, a).not)
                    else
                        + KnavesItemMainAction(f, a, l).as(a.item.img, dt.Arrow, a)(g).!(l.none, "no item").!(possible(f, a).not)
                }

                if (f.is(KnaveCheat))
                    + KnavesCheatMainAction(f).as(KnaveCheat, "flips two items")(g).!(cheats(f).none, "not enough items").!(Knaves.itemActions.exists(possible(f, _)).not)
            }

            ask(f)
                .add(Next.as("End", k.elem, "actions")(g))
                .daylight(f)

        case KnavesSpendAction(f, then) =>
            f.actions -= 1

            then

        case KnavesCommitAction(f, a, flips, via, then) =>
            flip(f, flips)

            f.used ++= via

            f.actions -= 1

            f.plog("flipped", flips./(_.elem).comma, "to", a)

            then

        case KnavesPendingDoneAction(f, KnavesServeCraft) =>
            f.pending = f.pending :- KnavesServeCraft

            f.serving = $

            KnavesTinkerAction(f, Repeat)

        case KnavesPendingDoneAction(f, p) =>
            f.pending = f.pending :- p

            Repeat

        case KnavesTinkerAction(f, then) =>
            if (f.is(KnaveTinker)) {
                f.plog("drew a card after", KnavesServe)

                drawAfter(f, 1, then)
            }
            else
                then

        // ITEM ACTIONS
        case KnavesItemMainAction(f, a, l) =>
            Ask(f).group(a, "flipping")
                    .each(l)((i, v) => entry(f, a, i, v).as(i./(_.img).merge, i./(_.elem).comma, v.has(KnavesVagrantFlip).?(("with", KnaveVagrant)), v.has(KnavesRangerFlip).?(("with", KnaveRanger))))
                    .cancel

        case KnavesCheatMainAction(f) =>
            Ask(f).group(KnaveCheat, "flips two items")
                .each(Knaves.itemActions.%(possible(f, _)))(a => KnavesCheatItemsAction(f, a).as(a.item.img, dt.Arrow, a))
                .cancel

        case KnavesCheatItemsAction(f, a) =>
            Ask(f).group(KnaveCheat, "flips two items to", a)
                .each(cheats(f))(i => entry(f, a, i, $).as(i./(_.img).merge, i./(_.elem).comma))
                .cancel

        case KnavesItemAction(f, a, flips, via) =>
            val k = f.acting.get
            val r = f.actingWhere.get
            val c = r.as[Clearing]
            val m = KnavesWith(a, true)

            def commit(then : ForcedAction) = KnavesCommitAction(f, a, flips, via, then)

            a match {
                case KnavesDash =>
                    MoveInitAction(f, f, dashes(f), m, $(r), $(r), $(CancelAction), commit(KnavesDashMoreAction(f, 2)))

                case KnavesAssault =>
                    val ronin = f.is(KnaveRonin).?(MoveInitAction(f, f, moves(f), m, $(r), $(r), $(CancelAction), commit(KnavesAssaultAfterMoveAction(f))).as("Move", k, "first")).$

                    if (c.?(c => f.canAttackList(c).any))
                        BattleInitAction(f, f, m, c.$, ronin ++ $(CancelAction), commit(Repeat))
                    else
                        Ask(f).add(ronin).cancel

                case KnavesSkirmish =>
                    if (r.is[Forest])
                        MoveInitAction(f, f, skirmishes(f), m, $(r), $(r), $(CancelAction), commit(KnavesSkirmishBattleAction(f, true)))
                    else
                        MoveInitAction(f, f, skirmishes(f), KnavesWith(a, false), $(r), $(r), $(CancelAction), commit(KnavesSkirmishBattleAction(f, false)))

                case KnavesNab =>
                    BattleInitAction(f, f, m, c.$, $(CancelAction), commit(KnavesNabAfterAction(f)))

                case _ =>
                    NoAsk(f)(entry(f, a, flips, via))
            }

        case KnavesDashMoreAction(f, n) =>
            val limit = f.is(KnaveHarrier).?(3).|(2)

            if (n <= limit && canMove(f, dashes(f)))
                MoveInitAction(f, f, dashes(f), KnavesWith(KnavesDash, true), f.actingWhere.$, f.actingWhere.$, $(Repeat.as("Stop")), KnavesDashMoreAction(f, n + 1))
            else
                NoAsk(f)(Repeat)

        case KnavesAssaultAfterMoveAction(f) =>
            f.actingWhere./~(_.as[Clearing]) match {
                case Some(c) => BattleInitAction(f, f, KnavesWith(KnavesAssault, true), $(c), $, Repeat)
                case None => NoAsk(f)(Repeat)
            }

        case KnavesSkirmishBattleAction(f, ignore) =>
            f.actingWhere./~(_.as[Clearing]) match {
                case Some(c) => BattleInitAction(f, f, KnavesWith(KnavesSkirmish, ignore), $(c), $, Repeat)
                case None => NoAsk(f)(Repeat)
            }

        case KnavesNabAfterAction(f) =>
            if (f.nabEscaped.not && f.actingWhere.any)
                f.pending :+= KnavesNabMove

            Repeat

        case KnavesRevelAction(f) =>
            val r = f.actingWhere.get

            def skunks(n : Int) {
                val k = f.canPlace(r).??(min(n, f.pooled(f.warrior)))

                if (k > 0) {
                    f.reserve --> k.times(f.warrior) --> r

                    f.log("placed", k.times(f.warrior.of(f)).comma, "in", r)
                }
            }

            r match {
                case c : Clearing if f.at(c).has(Acclaim) =>
                    skunks(2)

                    Repeat

                case c : Clearing if canAcclaim(f, c, false) =>
                    skunks(1)

                    KnavesPlaceAcclaimAction(f, c, Repeat)

                case _ =>
                    skunks(1)

                    Repeat
            }

        case KnavesGiftAction(f) =>
            f.actingWhere.get match {
                case c : Clearing if f.at(c).has(Acclaim) =>
                    drawAfter(f, 2, Repeat)

                case c : Clearing if canAcclaim(f, c, false) =>
                    KnavesPlaceAcclaimAction(f, c, drawAfter(f, 1, Repeat))

                case _ =>
                    drawAfter(f, 1, Repeat)
            }

        case KnavesServeAction(f) =>
            f.actingWhere./~(_.as[Clearing]) match {
                case Some(c) if f.at(c).has(Acclaim) =>
                    f.serving = f.all(Acclaim).%(_.asset == c.asset)

                    f.pending :+= KnavesServeCraft

                    Repeat

                case Some(c) if canAcclaim(f, c, false) =>
                    KnavesPlaceAcclaimAction(f, c, KnavesTinkerAction(f, Repeat))

                case _ =>
                    KnavesTinkerAction(f, Repeat)
            }

        // FILCH
        case KnavesFilchMainAction(f) =>
            val c = f.actingWhere./~(_.as[Clearing]).get
            val ee = f.enemies.%(_.present(c)).%(_.is[Hero].not).%(_.forTrade.any)

            Ask(f).group("Filch".styled(f), "in", c)
                .add(KnavesCraftMenuAction(f, $(c.asset), false).as("Craft".styled(f), dt.CraftSuit(c.asset), "(no points)").!(f.hand.%(f.craftableWith($(c.asset), $)).none, "nothing craftable"))
                .some(ee)(e => e.forTrade./(i => KnavesFilchTakeAction(f, e, i).as("Take", i.img, i, "from", e)))
                .cancel

        case KnavesFilchTakeAction(f, e, i) =>
            e.forTrade = e.forTrade :- i
            f.forTrade :+= i.item.pristine

            f.used :+= KnavesFilched
            f.actions -= 1

            f.plog("filched", i.item, "from", e)

            if (f.is(KnaveThief))
                f.pending :+= KnavesThiefMove

            Repeat

        case KnavesCraftMenuAction(f, assets, serve) =>
            YYSelectObjectsAction(f, f.hand)
                .withGroup("Craft".styled(f) ~ " with " ~ assets./(dt.CraftSuit).merge)
                .withRule(f.craftableWith(assets, $))
                .withThen(d => KnavesCraftAction(f, d, assets, serve))(d => game.desc("Craft", d))("Craft")
                .withExtra($(NoHand, CancelAction))

        case KnavesCraftAction(f, d, assets, serve) =>
            f.pool = assets
            f.crafted = $
            f.filchCrafting = serve.not

            if (serve.not) {
                f.used :+= KnavesFilched
                f.actions -= 1

                if (f.is(KnaveThief))
                    f.pending :+= KnavesThiefMove
            }

            val m = serve.?(KnavesWith(KnavesServe, true)).|(KnavesFilchMessage)

            CraftAction(f, d, m, CraftPerformAction(f, d, m))

        case CraftAssignAction(f : Knaves, d, all, used, m, then) =>
            f.crafted ++= used

            if (f.filchCrafting.not)
                used.foreach { a =>
                    f.serving.diff(f.activated).%(_.asset == a).starting.foreach { c => f.activated :+= c }
                }

            then

        case CraftScoreAction(f : Knaves, d, n, m, then) if f.filchCrafting && n != 0 =>
            CraftScoreAction(f, d, 0, m, then)

        // RETIRE
        case DaylightNAction(60, f : Knaves) =>
            f.acting.foreach { k =>
                f.retired :+= k

                f.log("retired", k)
            }

            f.acting = None
            f.actions = 0
            f.pending = $

            Next

        // EVENING
        case EveningNAction(20, f : Knaves) =>
            KnavesMockAction(f)

        case KnavesMockAction(f) =>
            val p = f.prisoners.num
            val a = f.all(Acclaim).num

            if (p / 2 + a / 2 > 0)
                f.oscore(p / 2 + a / 2)("mocking the powerful with", p.hl, "prisoners and", a.hl, "acclaim")

            Next

        case EveningNAction(30, f : Knaves) =>
            KnavesProtectMainAction(f)

        case KnavesProtectMainAction(f) =>
            val l = f.all(Acclaim).diff(f.protectedAcclaim)

            if (l.any && f.pool(f.warrior) && f.hand.any)
                Ask(f).group("Protect the Weak".styled(f), "spend a matching card to place a", f.warrior.of(f))
                    .some(l)(c => f.hand./(d => KnavesProtectCardAction(f, c, d).!(d.matches(c.cost).not).!(f.canPlace(c).not, "can't place")))
                    .done(Next)
                    .evening(f)
            else
                NoAsk(f)(Next)

        case KnavesProtectCardAction(f, c, d) =>
            f.hand --> d --> discard.quiet

            f.protectedAcclaim :+= c

            f.reserve --> f.warrior --> c

            f.log("protected the weak in", c, "with", d)

            Repeat

        case EveningNAction(40, f : Knaves) =>
            KnavesTakeItEasyAction(f)

        case KnavesTakeItEasyAction(f) =>
            if (f.captains.num == 3 && f.faceUp.none) {
                f.retired = $

                f.forTrade = f.forTrade./(_.refresh)

                f.log("took it easy")

                val ee = f.enemies.%(e => f.prisonersOf(e).any)

                if (ee.any) {
                    val n = ee./(e => f.prisonersOf(e).num).max
                    val tied = ee.%(e => f.prisonersOf(e).num == n)

                    if (tied.num == 1)
                        KnavesFreePrisonersMainAction(f, tied.only)
                    else
                        Ask(f).group("Choose who may free prisoners")
                            .each(tied)(e => KnavesFreePrisonersMainAction(f, e).as(e))
                            .needOk
                }
                else
                    Next
            }
            else
                Next

        case KnavesFreePrisonersMainAction(f, e) =>
            val l = clearings.%(c => adjacentForests(c).exists(r => f.from(Cage(r)).$.exists(_.faction == e)))

            Ask(e).group(e, "may free prisoners into a clearing")
                .each(l)(c => KnavesFreePrisonersAction(e, f, c).as(c, "(" ~ adjacentForests(c)./~(r => f.from(Cage(r)).$.%(_.faction == e)).num.hl ~ ")"))
                .skip(Next)

        case KnavesFreePrisonersAction(e, f, c) =>
            val l = adjacentForests(c)./~(r => f.from(Cage(r)).$.%(_.faction == e))

            if (e.canPlace(c)) {
                l.foreach(_ --> c)

                e.log("freed", l./(_.elem).comma, "into", c)
            }
            else {
                l.foreach(_ --> game.pool)

                e.log("freed", l./(_.elem).comma, "to the supply")
            }

            Next

        case NightStartAction(f : Knaves) =>
            EveningDrawAction(f, 1)

        case FactionCleanUpAction(f : Knaves) =>
            f.activated = $
            f.serving = $
            f.pending = $
            f.protectedAcclaim = $
            f.filchCrafting = false
            f.pool = $

            CleanUpAction(f)

        case _ => UnknownContinue
    }

}
