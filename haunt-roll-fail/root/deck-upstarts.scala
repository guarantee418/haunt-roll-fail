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


// The Upstarts & Renegades deck (The Outsiders Expansion, fan-made by slugfacekillah; 2026-11-18): the Deck choice `UpstartsDeck`,
// 54 cards. Cards 1 to 29 are the base deck's ambushes, items and dominances; the other 25 are new persistent effects (its three
// Sabotage! cards are the Exiles and Partisans' Saboteurs).
//
// Readings where the cards leave room:
// - Fox, Mouse and Rabbit Escort: at the start of Daylight ("after Birdsong"); the clearing counts as ruled by its owner until the
//   end of the turn; the hit without battling is assigned by the player hit, as a battle hit
// - Field Medic: once for each clearing where warriors are removed in a battle; the card matches the clearing
// - Coordinated Movement: after each move on its owner's turn between two clearings, more moves out of the same clearing into
//   others next to it, or into the same clearing from others next to it, follow as part of that move, along paths and ignoring rule
// - Burglar Extraordinaire and Rabbit Hood: a crafted item of a faction (not the Vagabond), or a Vagabond item it didn't start with
// - Cobbler Commander: two moves, two battles or one of each, at the start of Daylight; skipping both discards it, stopping after one
//   doesn't
// - Salt Factory: draws three, gives one to another player, keeps one and discards the third
// - Peace Offering: after the roll, as defender, a crafted item (its card's cost: Coins and Sword 2, the others 1) or improvement
//   (its cost) goes to the attacker, still crafted, and that many of the attacker's hits are ignored; the Murder Hawk, Cooperative
//   Housing, Baron Von Costume and Trailblazers aren't given, as their pieces stay with their owner
// - Trailblazers: the trail is placed by a move between two clearings of one forest that aren't adjacent; while it is out, anyone may
//   move along it as along a path (rule still applies)
// - Salvagers: two cards of one suit (birds count as any) for that crafting piece until the end of the turn
// - Tagalongs: after another player's move out of a clearing with its owner's warriors, any number of them may go along
// - Baron Von Costume: up to five of its owner's warriors from the supply; in Evening, once, two warriors (anyone's) in one clearing
//   are swapped with two on the card; with none of its owner's left on it, it is discarded and the warriors on it go to their supplies
// - Goon for Hire: asked before the roll when its owner attacks or defends; the owner takes the higher die, whatever else applies
// - Birdy Boom-Boom!: its owner's warriors there go back to the supply, then every enemy piece there is removed (scoring as usual)
// - Murder Hawk: a warrior of its owner (it counts for rule and battle), dealing an extra hit in each battle it is in; moving with it
//   ignores rule; removed, its card is discarded, and discarded, it leaves the map
// - Cooperative Housing: one more building slot in the clearing; each building an enemy places there scores its owner 1; when the
//   card is discarded, a building over the clearing's slots (the last built there) goes back to its supply
// - Prophetic Sight: the top five cards (fewer if the deck has fewer), put back in the order picked

case object ProphecySight extends CardEffect { override val name = "Prophetic Sight" }
case object CooperativeHousing extends CardEffect { override val name = "Cooperative Housing" }
case object BirdyBoomBoom extends CardEffect { override val name = "Birdy Boom-Boom!" }
case object MurderHawkCard extends CardEffect { override val name = "Murder Hawk" }
case class Escort(suit : BaseSuit) extends CardEffect { override val name = suit.name + " Escort" }
case object FieldMedic extends CardEffect { override val name = "Field Medic" }
case object CoordinatedMovement extends CardEffect { override val name = "Coordinated Movement" }
case class ItemThief(suit : BaseSuit) extends CardEffect { override val name = (suit == Fox).?("Burglar Extraordinaire").|("Rabbit Hood") }
case object CobblerCommander extends CardEffect { override val name = "Cobbler Commander" }
case object SaltFactory extends CardEffect { override val name = "Salt Factory" }
case object PeaceOffering extends CardEffect { override val name = "Peace Offering" }
case object Trailblazers extends CardEffect { override val name = "Trailblazers" }
case object Salvagers extends CardEffect { override val name = "Salvagers" }
case object Tagalongs extends CardEffect { override val name = "Tagalongs" }
case object BaronVonCostume extends CardEffect { override val name = "Baron Von Costume" }
case object GoonForHire extends CardEffect { override val name = "Goon for Hire" }

// the Murder Hawk, a warrior of the card's owner
case object MurderHawk extends Warrior {
    override def name = "Murder Hawk"
    override def imgid(f : Faction) = "upstarts-hawk"
}

// drawn on the map (`displayRegion` in `game.scala`)
case object HousingSlot extends Token {
    override def name = "Cooperative Housing"
}

// where the Murder Hawk waits off the map, and the warriors on Baron Von Costume
case object HawkPerch extends SpecialRegion
case object OnCostume extends SpecialRegion

// per battle (cleared by the removal cleanup each battle ends with)
case object HawkStruck extends RemoveEffect with HiddenEffect
case object GoonAsked extends RemoveEffect with HiddenEffect
case object GoonPaid extends RemoveEffect with HiddenEffect
case class MedicSkip(c : Clearing) extends RemoveEffect with HiddenEffect
case object EscortUsed extends HiddenEffect
case object CommanderStarted extends HiddenEffect

class UpstartsState {
    var escorts : $[(Faction, Clearing)] = $
    var trail : |[(Faction, Clearing, Clearing)] = None
    var housing : |[(Faction, Clearing)] = None
    var housingSeen : $[(Faction, Piece)] = $
    var housingLast : |[(Faction, Piece)] = None
    var baronPending = false
    var battleRemoval = false
    var dealt = false
}

// Trailblazers: through a forest to a clearing of it not adjacent, placing the trail, or along the trail
case object TrailMove extends Transport {
    override def img = Image("upstarts-trail", styles.widepiece)

    override def allows(f : Faction, o : Region, d : Region)(implicit game : Game) = (o, d) @@ {
        case (o : Clearing, d : Clearing) =>
            game.upstarts.trail @@ {
                case Some((_, a, b)) => (a == o && b == d) || (a == d && b == o)
                case None => f.has(Trailblazers) && game.current == f && UpstartsDeckExpansion.trailable(o, d)
            }
        case _ => false
    }

    override def order(l : $[Movable]) = 1
}


case class UpstartsMenuAction(self : Faction, e : CardEffect) extends BaseAction(Birdsong, "start")(e) with Soft

// PROPHETIC SIGHT
case class ProphecyPickAction(self : Faction, picked : $[DeckCard]) extends ForcedAction with Soft
case class ProphecyNextAction(self : Faction, picked : $[DeckCard], d : DeckCard) extends BaseAction(ProphecySight, "puts back on top, from the top down")(d.img) with ViewCard with Soft
case class ProphecyAction(self : Faction, order : $[DeckCard]) extends BaseAction(ProphecySight)("Put the cards back in this order")

// COOPERATIVE HOUSING
case class HousingPlaceAction(self : Faction, c : Clearing) extends BaseAction(CooperativeHousing, "places the extra building slot in")(c)

// BIRDY BOOM-BOOM
case class BoomAction(self : Faction, c : Clearing) extends BaseAction(Birdsong, "start", BirdyBoomBoom, "removes all pieces in")(c)

// MURDER HAWK
case class HawkPlaceAction(self : Faction, c : Clearing) extends BaseAction(MurderHawkCard, "places the", MurderHawk.of(self), "in")(c)

// ESCORT
case class EscortRuleAction(self : Faction, s : BaseSuit, c : Clearing) extends BaseAction(Escort(s), "rules until the end of the turn")(c)
case class EscortHitMainAction(self : Faction, s : BaseSuit, c : Clearing) extends BaseAction(Escort(s), "is discarded to deal", 1.hit, "in")(c) with Soft
case class EscortHitAction(self : Faction, s : BaseSuit, c : Clearing, e : Faction) extends BaseAction(Escort(s), "deals", 1.hit, "in", c, "to")(e)
case class EscortSkipAction(self : Faction) extends BaseAction(None)("Skip")

// FIELD MEDIC
case class MedicAction(self : WarriorFaction, c : Clearing, d : DeckCard, o : Clearing, then : ForcedAction) extends BaseAction(FieldMedic, "spends a card to place the warriors removed in", c, "in")(o, d)
case class MedicSkipAction(self : Faction, c : Clearing, then : ForcedAction) extends BaseAction(None)("Skip", FieldMedic)

// COORDINATED MOVEMENT and TAGALONGS
case class UpstartsAfterMoveAction(f : Faction, from : Clearing, to : Clearing, chained : Boolean, tagged : $[Faction], then : ForcedAction) extends ForcedAction
case class TagalongAction(self : WarriorFaction, e : Faction, from : Clearing, to : Clearing, n : Int, then : ForcedAction) extends BaseAction(Tagalongs, "go along with", e, "from", from, "to", to)(n.of(self.warrior.name).styled(self))
case class TagalongSkipAction(self : Faction, then : ForcedAction) extends BaseAction(None)("Stay")
case class CoordChainAction(f : Faction, from : Clearing, to : Clearing, mode : Int, then : ForcedAction) extends ForcedAction
case class CoordDoneAction(self : Faction, then : ForcedAction) extends BaseAction(None)("Done")

// ITEM THIEVES
case class ThiefMainAction(self : Faction, s : BaseSuit) extends BaseAction(Birdsong, "start")(ItemThief(s)) with Soft
case class ThiefAction(self : Faction, s : BaseSuit, e : Faction, i : ItemRef) extends BaseAction(ItemThief(s), "returns to the supply an item of")(e, i.img)

// COBBLER COMMANDER
case class CommanderAction(self : Faction, n : Int) extends ForcedAction
case class CommanderSkipAction(self : Faction) extends BaseAction(CobblerCommander)("Skip and discard", CobblerCommander)
case class CommanderDoneAction(self : Faction) extends BaseAction(None)("Done")

// SALT FACTORY
case class SaltAction(self : Faction) extends BaseAction(Evening, "start", SaltFactory)("Draw", 3.cards)
case class SaltSkipAction(self : Faction) extends BaseAction(None)("Skip")
case class SaltGiveAskAction(self : Faction) extends ForcedAction
case class SaltGiveAction(self : Faction, d : DeckCard, e : Faction) extends BaseAction(SaltFactory, "gives", d, "to")(e)
case class SaltCardAction(self : Faction, d : DeckCard) extends BaseAction(SaltFactory, "gives")(d.img) with ViewCard with Soft
case class SaltKeepAction(self : Faction, d : DeckCard) extends BaseAction(SaltFactory, "keeps")(d.img) with ViewCard

// PEACE OFFERING
case class PeaceItemAction(self : Faction, e : Faction, i : ItemRef, n : Int, then : BattlePostRollAction) extends BaseAction(self, "in battle")("Give", i, "to", e, "with", PeaceOffering, "to ignore", n.hits)
case class PeaceCardAction(self : Faction, e : Faction, d : CraftEffectCard, n : Int, then : BattlePostRollAction) extends BaseAction(self, "in battle")("Give", d, "to", e, "with", PeaceOffering, "to ignore", n.hits)

// SALVAGERS
case class SalvageMainAction(self : Faction) extends BaseAction(Birdsong, "start")(Salvagers) with Soft
case class SalvagePieceAction(self : Faction, s : BaseSuit, a : DeckCard, b : DeckCard) extends BaseAction(Salvagers, "discards two cards for a crafting piece")(dt.CraftSuit(s), a, b)
case class SalvageTakeAction(self : Faction, d : DeckCard) extends BaseAction(Salvagers, "is discarded to take a card from the discard pile")(d.img) with ViewCard

// BARON VON COSTUME
case class BaronPlaceAction(self : WarriorFaction, n : Int) extends BaseAction(BaronVonCostume, "places warriors on the card")(n.hl)
case class BaronMainAction(self : Faction) extends BaseAction(Evening)(BaronVonCostume) with Soft
case class BaronClearingAction(self : Faction, c : Clearing) extends BaseAction(BaronVonCostume, "swaps two warriors in")(c) with Soft
case class BaronTakeAction(self : Faction, c : Clearing, taken : $[WarriorFaction]) extends ForcedAction with Soft
case class BaronGiveAction(self : Faction, c : Clearing, taken : $[WarriorFaction], given : $[WarriorFaction]) extends ForcedAction with Soft
case class BaronSwapAction(self : Faction, c : Clearing, taken : $[WarriorFaction], given : $[WarriorFaction]) extends BaseAction(BaronVonCostume, "in", c)("Take", taken./(e => e.warrior.of(e)).comma, "and place", given./(e => e.warrior.of(e)).comma)

// GOON FOR HIRE
case class GoonAction(self : Faction, b : Battle, d : DeckCard) extends BaseAction(GoonForHire, "spends a card to take the higher die")(d.img) with ViewCard
case class GoonSkipAction(self : Faction, b : Battle) extends BaseAction(None)("Skip", GoonForHire)

// TRAILBLAZERS
case class TrailPlacedAction(f : Faction, from : Clearing, to : Clearing, then : ForcedAction) extends ForcedAction


object UpstartsDeckExpansion extends Expansion {
    def active(setup : $[Faction], options : $[Meta.O]) = options.has(UpstartsDeck)

    // the effects whose pieces stay with their owner
    val ownPieces = $[CardEffect](MurderHawkCard, CooperativeHousing, BaronVonCostume, Trailblazers)

    def itemCost(i : Item) = i @@ {
        case Coins | Sword => 2
        case _ => 1
    }

    def trailable(o : Clearing, d : Clearing)(implicit game : Game) = o != d && board.connected(o).has(d).not && game.board.forests.exists(x => game.fromForest(x).use(l => l.has(o) && l.has(d)))

    def escortRules(f : Faction, c : Region)(implicit game : Game) = game.options.has(UpstartsDeck) && game.upstarts.escorts.exists { case (e, o) => e == f && o == c }

    def hawkAt(f : Faction, o : Region)(implicit game : Game) = game.options.has(UpstartsDeck) && f.at(o).has(MurderHawk)

    def extraSlot(c : Clearing)(implicit game : Game) = game.options.has(UpstartsDeck) && game.upstarts.housing.exists(_._2 == c)

    def markers(c : Clearing)(implicit game : Game) : $[Figure] = game.options.has(UpstartsDeck).??(game.upstarts.housing.%(_._2 == c)./(_ => Figure(Neutral, HousingSlot, 0)).$)

    def enemyBuildings(implicit game : Game) : $[(Faction, Piece)] = game.upstarts.housing.$./~ { case (f, c) => factions.but(f)./~(e => e.at(c).of[Building]./(p => (e, p : Piece))) }

    def hawkFallen(implicit game : Game) = factions.exists(e => e.reserve.$.exists(_.piece == MurderHawk))

    def baronEmpty(implicit game : Game) = game.upstarts.baronPending.not && factions.exists(e => e.has(BaronVonCostume) && e.from(OnCostume).none)

    def onCostume(implicit game : Game) = (factions ++ hirelings).%(game.states.contains)./~(e => e.from(OnCostume).$)

    def sweepNeeded(implicit game : Game) = (game.upstarts.housing.any && enemyBuildings.sortBy(_.toString) != game.upstarts.housingSeen.sortBy(_.toString)) || hawkFallen || baronEmpty

    // the items another player has crafted: the Vagabond's, beyond those it started with
    def stealable(f : Faction)(implicit game : Game) : $[(Faction, ItemRef)] = factions.but(f)./~ {
        case h : Hero => h.character./(_.starting(options)).|($).foldLeft(h.inv)((l, i) => l.diff(l.%(_.item == i).take(1)))./(i => (h : Faction, i))
        case e => e.forTrade./(i => (e, i))
    }

    def birdsongStart(f : Faction)(implicit game : Game) : $[UserAction] =
        f.can(ProphecySight).$(UpstartsMenuAction(f, ProphecySight).!(game.deck.none, "deck is empty")) ++
        f.can(BirdyBoomBoom).$(UpstartsMenuAction(f, BirdyBoomBoom).!(boomClearings(f).none, "no warriors")) ++
        FoxRabbitMouse.%(s => f.can(ItemThief(s)))./(s => ThiefMainAction(f, s).!(stealable(f).none, "no items to take")) ++
        f.can(Salvagers).$(SalvageMainAction(f))

    def boomClearings(f : Faction)(implicit game : Game) = clearings.%(c => f.at(c).of[Warrior].any)

    def escortClearings(f : Faction, s : BaseSuit)(implicit game : Game) = clearings.%(_.suits.has(s)).%(c => f.present(c) || game.connected(c).exists(f.present))

    def postRoll(f : Faction, b : Battle, fh : Int, oh : Int, fe : Int, oe : Int, full : $[Faction])(implicit game : Game) : $[UserAction] = {
        if (f != b.defender || f.has(PeaceOffering).not || fh + fe <= 0)
            return $

        def ignore(n : Int) = {
            val a = min(fh, n)
            BattlePostRollAction(b, fh - a, oh, max(0, fe - (n - a)), oe, full)
        }

        val e = b.attacker

        f.is[Hero].not.??(f.forTrade.distinct./(i => PeaceItemAction(f, e, i, itemCost(i.item), ignore(itemCost(i.item))))) ++
        f.stuck.$.of[CraftEffectCard].%(d => ownPieces.has(d.effect).not).distinct./(d => PeaceCardAction(f, e, d, max(1, d.cost.num), ignore(max(1, d.cost.num))))
    }

    def discarded(f : Faction, effect : Effect)(implicit game : Game) {
        if (effect == MurderHawkCard)
            factions.foreach { e =>
                (clearings ++ $(Pool))./~(r => e.from(r).$.%(_.piece == MurderHawk)).foreach { u =>
                    f.log("lost the", MurderHawk.of(e))
                    u --> HawkPerch
                }
            }

        if (effect == BaronVonCostume)
            onCostume.foreach { u => u --> Pool }

        if (effect == Trailblazers && game.upstarts.trail.exists(_._1 == f))
            game.upstarts.trail = None

        if (effect == CooperativeHousing)
            game.upstarts.housing.%(_._1 == f).foreach { case (_, c) =>
                game.upstarts.housing = None
                game.upstarts.housingSeen = $

                if (game.freeSlots0(c) < 0)
                    game.upstarts.housingLast.%{ case (e, p) => e.at(c).has(p) }.foreach { case (e, p) =>
                        e.from(c) --> p --> game.recycle
                        e.log("lost", p.of(e), "in", c, "with the slot of", CooperativeHousing)
                    }

                game.upstarts.housingLast = None
            }
    }

    def perform(action : Action, soft : Void)(implicit game : Game) = action @@ {
        // THE PIECES: buildings in the Cooperative Housing clearing, a removed Murder Hawk, an emptied Baron Von Costume
        case a if a.isSoft.not && game.turn > 0 && sweepNeeded =>
            game.upstarts.housing.foreach { case (f, c) =>
                val now = enemyBuildings
                val added = now.diff(game.upstarts.housingSeen)

                if (added.any) {
                    f.oscore(added.num)("from", CooperativeHousing)
                    game.upstarts.housingLast = Some(added.last)
                }

                game.upstarts.housingSeen = now
            }

            factions.foreach { e =>
                e.reserve.$.%(_.piece == MurderHawk).foreach { u =>
                    u --> HawkPerch

                    if (e.has(MurderHawkCard)) {
                        e.log("discarded", MurderHawkCard)
                        e.removeStuckEffect(MurderHawkCard)
                    }
                }
            }

            factions.%(e => e.has(BaronVonCostume) && e.from(OnCostume).none).foreach { e =>
                e.log("had no warriors left on", BaronVonCostume)
                e.removeStuckEffect(BaronVonCostume)
            }

            TryAgain

        // CRAFTED
        case CraftPerformAction(f, d @ CraftEffectCard(_, _, _, MurderHawkCard), m) =>
            f.hand --> d --> f.stuck
            f.effects :+= MurderHawkCard

            CraftEffectAction(f, d, m, HawkAskAction(f))

        case HawkAskAction(f) =>
            val l = clearings.%(f.present)

            if (l.none)
                Repeat
            else
                Ask(f).group(MurderHawkCard, "places the", MurderHawk.of(f)).each(l)(c => HawkPlaceAction(f, c))

        case HawkPlaceAction(f, c) =>
            f.from(HawkPerch) --> c

            f.log("placed the", MurderHawk.of(f), "in", c)

            Repeat

        case CraftPerformAction(f, d @ CraftEffectCard(_, _, _, CooperativeHousing), m) =>
            f.hand --> d --> f.stuck
            f.effects :+= CooperativeHousing

            CraftEffectAction(f, d, m, HousingAskAction(f))

        case HousingAskAction(f) =>
            val l = clearings.%(f.present)

            if (l.none)
                Repeat
            else
                Ask(f).group(CooperativeHousing, "places the extra building slot").each(l)(c => HousingPlaceAction(f, c))

        case HousingPlaceAction(f, c) =>
            game.upstarts.housing = Some((f, c))
            game.upstarts.housingSeen = enemyBuildings

            f.log("placed the extra building slot of", CooperativeHousing, "in", c)

            Repeat

        case CraftPerformAction(f, d @ CraftEffectCard(_, _, _, BaronVonCostume), m) =>
            f.hand --> d --> f.stuck
            f.effects :+= BaronVonCostume

            game.upstarts.baronPending = true

            CraftEffectAction(f, d, m, BaronAskAction(f))

        case BaronAskAction(f) =>
            f.as[WarriorFaction] @@ {
                case Some(w) =>
                    val n = min(5, w.pooled(w.warrior))
                    Ask(f).group(BaronVonCostume, "places warriors on the card").each(0.to(n).reverse.$)(k => BaronPlaceAction(w, k))
                case None =>
                    Ask(f).add(BaronPlaceDoneAction(f))
            }

        case BaronPlaceAction(f, n) =>
            game.upstarts.baronPending = false

            f.reserve --> (f.warrior *** n) --> OnCostume

            f.log("placed", n.of(f.warrior.name).styled(f), "on", BaronVonCostume)

            Repeat

        case BaronPlaceDoneAction(f) =>
            game.upstarts.baronPending = false

            Repeat

        // ReplayCheck: every effect dealt out crafted, in turn, at the first Birdsong
        case BirdsongNAction(5, f) if options.has(UpstartsTestDeal) && game.upstarts.dealt.not =>
            game.upstarts.dealt = true

            val l = (game.deck.$ ++ pile.$).of[CraftEffectCard].%(d => Deck.upstarts.has(d))

            l.indexed.foreach { (d, i) =>
                val e : Faction = factions.apply(i % factions.num)

                if (game.deck.$.has(d)) game.deck --> d --> e.stuck else pile --> d --> e.stuck
                e.effects :+= d.effect

                d.effect @@ {
                    case MurderHawkCard => clearings.%(e.present).starting.foreach(c => e.from(HawkPerch) --> c)
                    case CooperativeHousing => clearings.%(e.present).starting.foreach(c => game.upstarts.housing = Some((e, c)))
                    case BaronVonCostume => e.as[WarriorFaction].foreach(w => w.reserve --> (w.warrior *** min(3, w.pooled(w.warrior))) --> OnCostume)
                    case _ =>
                }
            }

            game.upstarts.housingSeen = enemyBuildings

            BirdsongNAction(5, f)

        // START OF BIRDSONG
        case BirdsongNAction(5, f) if game.upstarts.trail.exists(_._1 == f) =>
            game.upstarts.trail = None

            f.log("removed the trail of", Trailblazers)

            BirdsongNAction(5, f)

        case UpstartsMenuAction(f, ProphecySight) =>
            ProphecyPickAction(f, $)

        case ProphecyPickAction(f, picked) =>
            val top = game.deck.$.take(5)
            val rest = top.diff(picked)

            if (rest.none)
                Ask(f).add(ProphecyAction(f, picked)).add(ProphecyPickAction(f, $).as("Start over")).cancel
            else
                Ask(f).group(ProphecySight, "puts back on top", picked.any.$("after", picked))
                    .each(rest)(d => ProphecyNextAction(f, picked, d))
                    .cancel

        case ProphecyNextAction(f, picked, d) =>
            ProphecyPickAction(f, picked :+ d)

        case ProphecyAction(f, order) =>
            f.used :+= ProphecySight

            val rest = game.deck.$.diff(order)

            game.deck --> (order ++ rest) --> game.deck

            f.log("looked at the top", order.num.cards, "of the deck with", ProphecySight, "and put them back")

            Repeat

        case UpstartsMenuAction(f, BirdyBoomBoom) =>
            Ask(f).group(BirdyBoomBoom).each(boomClearings(f))(c => BoomAction(f, c)).cancel

        case BoomAction(f, c) =>
            f.removeStuckEffect(BirdyBoomBoom)

            val own = f.from(c).$.%(_.piece.is[Warrior])

            own.foreach(_ --> game.removed)

            f.log("removed", own./(_.elem).comma, "in", c, "and discarded", BirdyBoomBoom)

            NukeAction(f, f.enemies, $(c), NukeType.ClearSector, Repeat)

        case ThiefMainAction(f, s) =>
            Ask(f).group(ItemThief(s), "returns an item to the supply")
                .each(stealable(f))((e, i) => ThiefAction(f, s, e, i))
                .cancel

        case ThiefAction(f, s, e, i) =>
            f.removeStuckEffect(ItemThief(s))

            e @@ {
                case h : Hero => h.inv :-= i
                case e => e.forTrade :-= i
            }

            game.uncrafted :+= i.item

            f.log("discarded", ItemThief(s), "and returned", i, "of", e, "to the supply")

            Repeat

        case SalvageMainAction(f) =>
            val pairs = FoxRabbitMouse./~(s => f.hand.%(_.matches(s)).combinations(2).$./(l => (s, l(0), l(1))))

            Ask(f).group(Salvagers)
                .each(pairs)(x => SalvagePieceAction(f, x._1, x._2, x._3))
                .each(pile.$.distinct)(d => SalvageTakeAction(f, d))
                .cancel

        case SalvagePieceAction(f, s, a, b) =>
            f.used :+= Salvagers

            f.hand --> $(a, b) --> discard.quiet

            f.extraCraft :+= s

            f.log("discarded", a, "and", b, "with", Salvagers, "for a", s, "crafting piece until the end of the turn")

            Repeat

        case SalvageTakeAction(f, d) =>
            f.removeStuckEffect(Salvagers)

            pile --> d --> f.hand

            f.log("discarded", Salvagers, "to take", d, "from the discard pile")

            Repeat

        // START OF DAYLIGHT
        case DaylightNAction(0, f) if FoxRabbitMouse.exists(s => f.has(Escort(s))) && f.used.has(EscortUsed).not =>
            val ss = FoxRabbitMouse.%(s => f.has(Escort(s)))

            Ask(f).group("Start of", Daylight)
                .some(ss)(s => escortClearings(f, s)./(c => EscortRuleAction(f, s, c)))
                .some(ss)(s => escortClearings(f, s).%(c => f.enemies.exists(e => e.present(c) && f.canRemove(c)(e)))./(c => EscortHitMainAction(f, s, c)))
                .add(EscortSkipAction(f))
                .needOk

        case EscortRuleAction(f, s, c) =>
            f.used :+= EscortUsed

            game.upstarts.escorts :+= ((f, c))

            f.log("rules", c, "until the end of the turn with", Escort(s))

            DaylightNAction(0, f)

        case EscortHitMainAction(f, s, c) =>
            Ask(f).group(Escort(s), "deals", 1.hit, "in", c, "to")
                .each(f.enemies.%(e => e.present(c) && f.canRemove(c)(e)))(e => EscortHitAction(f, s, c, e))
                .cancel

        case EscortHitAction(f, s, c, e) =>
            f.used :+= EscortUsed

            f.removeStuckEffect(Escort(s))

            f.log("discarded", Escort(s), "to deal", 1.hit, "to", e, "in", c)

            val then = DaylightNAction(0, f)

            BattleAssignHitsAction(e, Battle(c, f, None, e, None, None, None, 0, 0, 0, then), 1, 0, ForcedRemoveFinishedAction(f, then))

        case EscortSkipAction(f) =>
            f.used :+= EscortUsed

            DaylightNAction(0, f)

        case DaylightNAction(0, f) if f.has(CobblerCommander) && f.used.has(CommanderStarted).not =>
            f.used :+= CommanderStarted

            CommanderAction(f, 0)

        case CommanderAction(f, n) if n >= 2 || f.has(CobblerCommander).not =>
            DaylightNAction(0, f)

        case CommanderAction(f, n) =>
            val m = f.moveFrom
            val b = clearings.%(f.canAttackIn)

            Ask(f).group(CobblerCommander, (n == 1).?("takes another move or battle"))
                .add(MoveInitAction(f, f, $, WithEffect(CobblerCommander), m, f.movable, $(CancelAction), CommanderAction(f, n + 1)).as("Move".hl, dt.Move)(CobblerCommander).!(m.none, "can't move"))
                .add(BattleInitAction(f, f, WithEffect(CobblerCommander), b, $(CancelAction), CommanderAction(f, n + 1)).as("Battle".hl, dt.Battle)(CobblerCommander).!(b.none, "can't battle"))
                .when(n == 0)(CommanderSkipAction(f))
                .when(n > 0)(CommanderDoneAction(f))
                .needOk

        case CommanderSkipAction(f) =>
            f.removeStuckEffect(CobblerCommander)

            f.log("discarded", CobblerCommander)

            DaylightNAction(0, f)

        case CommanderDoneAction(f) =>
            DaylightNAction(0, f)

        // START OF EVENING
        case EveningNAction(0, f) if f.can(SaltFactory) =>
            Ask(f).group("Start of", Evening)
                .add(SaltAction(f).!(game.deck.none && pile.none, "no cards"))
                .add(SaltSkipAction(f))
                .needOk

        case SaltSkipAction(f) =>
            f.used :+= SaltFactory

            EveningNAction(0, f)

        case SaltAction(f) =>
            f.used :+= SaltFactory

            DrawCardsAction(f, 3, WithEffect(SaltFactory), SaltGiveAskAction(f))

        case SaltGiveAskAction(f) =>
            val l = f.drawn.$

            if (l.none)
                EveningNAction(0, f)
            else
            if (l.num == 1) {
                f.drawn --> f.hand

                EveningNAction(0, f)
            }
            else
                Ask(f).group(SaltFactory, "gives a card to another player")
                    .each(l)(d => SaltCardAction(f, d))

        case SaltCardAction(f, d) =>
            Ask(f).group(SaltFactory, "gives", d, "to")
                .each(factions.but(f))(e => SaltGiveAction(f, d, e))
                .cancel

        case SaltGiveAction(f, d, e) =>
            f.drawn --> d --> e.hand

            f.log("gave a card to", e, "with", SaltFactory)

            e.notify($(ViewCardInfoAction(e, Gave(f, e).elem(game), d)))

            if (f.drawn.num == 1) {
                f.drawn --> f.hand

                EveningNAction(0, f)
            }
            else
                Ask(f).group(SaltFactory, "keeps a card and discards the other")
                    .each(f.drawn.$)(x => SaltKeepAction(f, x))

        case SaltKeepAction(f, d) =>
            f.drawn --> d --> f.hand
            f.drawn --> discard.quiet

            EveningNAction(0, f)

        // EVENING - BARON VON COSTUME
        case BaronMainAction(f) =>
            val l = clearings.%(c => (factions ++ hirelings).of[WarriorFaction].%(game.states.contains)./(e => e.at(c).count(e.warrior)).sum >= 2)

            Ask(f).group(BaronVonCostume, "swaps two warriors in a clearing with two on the card")
                .each(l)(c => BaronClearingAction(f, c))
                .cancel

        case BaronClearingAction(f, c) =>
            BaronTakeAction(f, c, $)

        case BaronTakeAction(f, c, taken) if taken.num >= 2 =>
            BaronGiveAction(f, c, taken, $)

        case BaronTakeAction(f, c, taken) =>
            val l = (factions ++ hirelings).of[WarriorFaction].%(game.states.contains).%(e => e.at(c).count(e.warrior) > taken.count(e))

            Ask(f).group(BaronVonCostume, "takes from", c, taken.any.$("with", taken./(e => e.warrior.of(e)).comma))
                .each(l)(e => BaronTakeAction(f, c, taken :+ e).as(e.warrior.of(e), e.warrior.imgd(e)))
                .cancel

        case BaronGiveAction(f, c, taken, given) if given.num >= 2 =>
            Ask(f).add(BaronSwapAction(f, c, taken, given)).cancel

        case BaronGiveAction(f, c, taken, given) =>
            val l = (factions ++ hirelings).of[WarriorFaction].%(game.states.contains).%(e => e.from(OnCostume).$.count(_.piece == e.warrior) > given.count(e))

            Ask(f).group(BaronVonCostume, "places in", c, given.any.$("with", given./(e => e.warrior.of(e)).comma))
                .each(l)(e => BaronGiveAction(f, c, taken, given :+ e).as(e.warrior.of(e), e.warrior.imgd(e)))
                .cancel

        case BaronSwapAction(f, c, taken, given) =>
            f.used :+= BaronVonCostume

            given.foreach { e => e.from(OnCostume) --> e.warrior --> c }
            taken.foreach { e => e.from(c) --> e.warrior --> OnCostume }

            f.log("swapped", taken./(e => e.warrior.of(e)).comma, "in", c, "for", given./(e => e.warrior.of(e)).comma, "with", BaronVonCostume)

            Repeat

        // MOVES - TRAILBLAZERS, TAGALONGS, COORDINATED MOVEMENT
        case MoveListAction(self, f, t, m, from : Clearing, to : Clearing, l, then) if t.has(TrailMove) =>
            log(MovementExpansion.describe(self, f, $, l), "from", from, "to", to, m, "along the trail of", Trailblazers)

            Force(MoveListAction(self, f, $, NoMessage, from, to, l, TrailPlacedAction(f, from, to, then)))

        case TrailPlacedAction(f, from, to, then) =>
            if (game.upstarts.trail.none) {
                game.upstarts.trail = Some((f, from, to))

                f.log("placed the trail of", Trailblazers, "between", from, "and", to)
            }

            then

        case MoveFinishedAction(f, from : Clearing, to : Clearing, then) if then.is[UpstartsAfterMoveAction].not && then.is[FerryAction].not && then.is[TrailPlacedAction].not && (
            factions.but(f).exists(e => e.has(Tagalongs) && e.at(from).of[Warrior].any) ||
            (f.has(CoordinatedMovement) && game.current == f)
        ) =>
            MoveFinishedAction(f, from, to, UpstartsAfterMoveAction(f, from, to, then.is[CoordChainAction], $, then))

        case UpstartsAfterMoveAction(f, from, to, chained, tagged, then) =>
            factions.but(f).diff(tagged).of[WarriorFaction].%(e => e.has(Tagalongs) && e.at(from).count(e.warrior) > 0).starting match {
                case Some(e) =>
                    val next = UpstartsAfterMoveAction(f, from, to, chained, tagged :+ e, then)

                    Ask(e).group(Tagalongs, "go along with", f, "from", from, "to", to)
                        .each(1.to(e.at(from).count(e.warrior)).reverse.$)(n => TagalongAction(e, f, from, to, n, next))
                        .add(TagalongSkipAction(e, next))

                case None =>
                    if (f.has(CoordinatedMovement) && game.current == f && chained.not)
                        CoordChainAction(f, from, to, 0, then)
                    else
                        then
            }

        case TagalongAction(e, f, from, to, n, then) =>
            e.from(from) --> (e.warrior *** n) --> to

            e.log("moved", n.of(e.warrior.name).styled(e), "from", from, "to", to, "with", f, "as", Tagalongs)

            then

        case TagalongSkipAction(e, then) =>
            then

        case CoordChainAction(f, from, to, mode, then) =>
            val tt = $($[Transport](Roads))

            val out = (mode != 2 && f.canMoveFrom(from) && f.at(from).of[Movable].any).??(game.connected(from).but(to).%(d => f.validDest.has(d) && Roads.allows(f, from, d)))
            val in = (mode != 1).??(game.connected(to).but(from).%(o => f.canMoveFrom(o) && f.at(o).of[Movable].any && f.validDest.has(to) && Roads.allows(f, o, to)))

            if (out.none && in.none)
                then
            else
                Ask(f).group(CoordinatedMovement, "moves more as part of the same move, ignoring rule")
                    .each(out)(d => MoveToAction(f, f, tt, WithEffect(CoordinatedMovement), from, d, CoordChainAction(f, from, to, 1, then)))
                    .each(in)(o => MoveToAction(f, f, tt, WithEffect(CoordinatedMovement), o, to, CoordChainAction(f, from, to, 2, then)))
                    .add(CoordDoneAction(f, then))

        case CoordDoneAction(f, then) =>
            then

        // BATTLE - GOON FOR HIRE, MURDER HAWK, FIELD MEDIC
        case BattleRollAction(b) if $(b.attacker, b.defender).exists(f => f.has(GoonForHire) && f.used.has(GoonAsked).not && f.hand.exists(_.matches(b.clearing.cost))) =>
            val f = $(b.attacker, b.defender).%(f => f.has(GoonForHire) && f.used.has(GoonAsked).not).first

            f.used :+= GoonAsked

            Ask(f).group(GoonForHire, "before the roll in", b.clearing)
                .each(f.hand.%(_.matches(b.clearing.cost)))(d => GoonAction(f, b, d))
                .add(GoonSkipAction(f, b))

        case GoonAction(f, b, d) =>
            f.hand --> d --> discard.quiet

            f.used :+= GoonPaid

            f.log("spent", d, "with", GoonForHire, "to take the higher die")

            BattleRollAction(b)

        case GoonSkipAction(f, b) =>
            BattleRollAction(b)

        case BattleAdjustRollAction(b, f, o, fs, os, fr, or) if $(b.attacker, b.defender).exists(_.used.has(GoonPaid)) =>
            val hi = max(fr, or)
            val lo = min(fr, or)

            val (afr, aor) = b.attacker.used.has(GoonPaid).?((hi, lo)).|((lo, hi))

            b.attacker.used :-= GoonPaid
            b.defender.used :-= GoonPaid

            val afs = (fs > 0 && b.attacker.used.has(Looters)).?(0).|(fs)

            BattleFinishRollAction(b, f, o, afs, os, afr, aor, min(afr, afs), min(aor, os))

        case BattleBonusAction(b, f, o, fs, os, fr, or, fh, oh, fe, oe) if f.used.has(HawkStruck).not && f.at(b.clearing).has(MurderHawk) =>
            f.used :+= HawkStruck

            f.log("dealt an extra", 1.hit, "with the", MurderHawk.of(f))

            BattleBonusAction(b, f, o, fs, os, fr, or, fh, oh, fe + 1, oe)

        case BattleBonusAction(b, f, o, fs, os, fr, or, fh, oh, fe, oe) if o.used.has(HawkStruck).not && o.at(b.clearing).has(MurderHawk) =>
            o.used :+= HawkStruck

            o.log("dealt an extra", 1.hit, "with the", MurderHawk.of(o))

            BattleBonusAction(b, f, o, fs, os, fr, or, fh, oh, fe, oe + 1)

        case PeaceItemAction(f, e, i, n, then) =>
            f.forTrade :-= i

            e @@ {
                case h : Hero => h.inv :+= i.item.pristine
                case e => e.forTrade :+= i
            }

            f.log("gave", i, "to", e, "with", PeaceOffering, "and ignored", n.hits)

            then

        case PeaceCardAction(f, e, d, n, then) =>
            f.stuck --> d --> e.stuck
            f.effects :-= d.effect
            e.effects :+= d.effect

            f.log("gave", d, "to", e, "with", PeaceOffering, "and ignored", n.hits)

            then

        case BattleForcedRemoveAction(b) if game.upstarts.battleRemoval.not && factions.exists(_.has(FieldMedic)) =>
            game.upstarts.battleRemoval = true

            ForcedRemoveFinishedAction(b.instigator | b.attacker, BattleResolvedAction(b))

        case BattleResolvedAction(b) if game.upstarts.battleRemoval =>
            game.upstarts.battleRemoval = false

            BattleResolvedAction(b)

        case ForcedRemoveProcessAction(f : WarriorFaction, then) if game.upstarts.battleRemoval && f.has(FieldMedic) && game.limboClearings.exists(c => f.ignored.has(MedicSkip(c)).not && f.limbo(c).$.exists(_.piece.is[Warrior])) =>
            val c = game.limboClearings.%(c => f.ignored.has(MedicSkip(c)).not && f.limbo(c).$.exists(_.piece.is[Warrior])).first
            val n = f.limbo(c).$.%(_.piece.is[Warrior]).num
            val l = game.connected(c).%(f.canPlace)
            val q = ForcedRemoveProcessAction(f, then)

            Ask(f).group(FieldMedic, "places", n.of("warrior"), "removed in", c, "in a clearing next to it")
                .some(f.hand.%(_.matches(c.cost)))(d => l./(o => MedicAction(f, c, d, o, q)))
                .add(MedicSkipAction(f, c, q))

        case MedicAction(f, c, d, o, then) =>
            val l = f.limbo(c).$.%(_.piece.is[Warrior])

            f.hand --> d --> discard.quiet

            l.foreach(_ --> o)

            f.ignored :+= MedicSkip(c)

            f.log("spent", d, "with", FieldMedic, "to place", l./(_.elem).comma, "in", o)

            then

        case MedicSkipAction(f, c, then) =>
            f.ignored :+= MedicSkip(c)

            then

        // END OF TURN
        case CleanUpAction(f) if game.upstarts.escorts.exists(_._1 == f) =>
            game.upstarts.escorts = game.upstarts.escorts.filter(_._1 != f)

            CleanUpAction(f)

        case _ => UnknownContinue
    }

    override def evening(f : Faction)(implicit game : Game, ask : ActionCollector) {
        if (game.current == f && f.can(BaronVonCostume))
            + BaronMainAction(f)
    }
}

case class BaronPlaceDoneAction(self : Faction) extends BaseAction(BaronVonCostume)("Ok")
case class HawkAskAction(self : Faction) extends ForcedAction
case class HousingAskAction(self : Faction) extends ForcedAction
case class BaronAskAction(self : Faction) extends ForcedAction
