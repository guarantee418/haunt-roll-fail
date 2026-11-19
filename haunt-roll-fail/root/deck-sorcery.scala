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


// Sorcery of the Enchanted Woods (fan-made; 2026-11-19): the Deck choice `SorceryDeck`, 55 cards from the Tabletop Simulator workshop
// mod 2413287097 (uploaded by slugfacekillah, "Not Mine": its designer isn't named). Its ambushes, items and the four suit dominances
// are the base deck's; the other 26 cards are 25 new persistent effects (Hailstorm is spent when crafted) and a fifth dominance for six
// crafted improvements (`SorceryDominance`). There is no rulebook: the card text is the rules.
//
// Readings where the cards leave room:
// - Garden of Glory: a token of its crafter, placed in any clearing; it can be removed like any token (scoring as usual), and then the
//   card is discarded; whoever rules its clearing at the start of their Evening scores 1 and draws a card, once a turn
// - Enchanted Objects: after an item is crafted, its effect may be taken at once: Sword a battle, Boots a move, Bag a card, Crossbow the
//   removal of any enemy warrior (no points), Teapot two cards drawn then two discarded, Hammer another card from hand crafted without
//   paying its cost, Coins two warriors from the supply in a clearing its crafter rules
// - Living Buildings: one more maximum rolled hit in a battle in a clearing with any of its owner's buildings (not one per building), for a
//   side with warriors there (so a defender with buildings alone stays defenseless), and
//   each of its owner's buildings counts as two pieces for rule
// - Mantra of Frenzy: after the first battle its owner attacked in on its own turn, another battle anywhere
// - Favor of the Phoenix: one card after each battle in which a building or token of its owner was removed, whatever the number
// - Foresight: asked when an ambush card is played against its owner attacking, before the counter-ambush; the ambush deals no hits and
//   its owner takes a card of its choice from the defender's hand
// - Knowledge of the Fallen: the first battle its owner is in each turn, attacking or defending; both sides draw after it ends
// - Enchanted Gifts: a Daylight action on its owner's turn; the item given stays crafted (the Vagabond gives an item it didn't start with)
// - Spell of Flight: a Daylight action on its owner's turn; the move ignores rule and may go along rivers as along paths
// - Onyx Cauldron: the warriors removed in a battle, of its owner's warrior kind, once for each clearing; at the start of its owner's
//   Evening, once, two of them (or one, if one is left) go to a clearing it rules matching the card spent; discarded, they go to the supply
// - Misdirection: asked when its owner is declared defender and the attacker could battle another faction there; that faction (not one
//   already misdirected to in this battle) becomes the defender
// - Hailstorm: spent when crafted; in each clearing its crafter removes one piece (its own too, scoring 1 for each enemy building or
//   token): a warrior where any warrior is, else a building or token; landmarks, ruins and pawns stay; then each Vagabond damages an item
// - Scepter of Majesty: three cards of one suit (birds count as any), once a turn, in Birdsong, Daylight or Evening of its owner's turn
// - Visions From Beyond: every draw of a card or more draws one more, then one card from hand (or drawn) is discarded
// - Treewalking: once a turn, a move between two clearings of one forest, ignoring paths (rule still applies), as Off Trail does
// - The Wizards Tower: a building of its crafter in a clearing it rules with a free slot (none placed without one); while on the map, it
//   is a crafting piece of any suit for its owner; removed, it scores as a building and the card is discarded
// - Fire and Lightning: one more hit as attacker, after the roll; Summon River Beast: once a battle, as attacker or defender, in a
//   clearing on a river, a card matching the clearing spent for one more hit
// - Dispel: at the start of Birdsong, any player's crafted improvement (its owner's too) is discarded with it
// - the sixth-improvement Dominance: a bird card; activated with 10 or more points and fewer than 4 crafted improvements, it wins at the
//   start of its owner's turn with 6 crafted improvements (persistent effect cards, not items)

case object GardenOfGlory extends CardEffect { override val name = "Garden of Glory" }
case object EnchantedObjects extends CardEffect { override val name = "Enchanted Objects" }
case object LivingBuildings extends CardEffect { override val name = "Living Buildings" }
case object MantraOfFrenzy extends CardEffect { override val name = "Mantra of Frenzy" }
case object FavorOfThePhoenix extends CardEffect { override val name = "Favor of the Phoenix" }
case object Foresight extends CardEffect { override val name = "Foresight" }
case object KnowledgeOfTheFallen extends CardEffect { override val name = "Knowledge of the Fallen" }
case object EnchantedGifts extends CardEffect { override val name = "Enchanted Gifts" }
case object SpellOfFlight extends CardEffect { override val name = "Spell of Flight" }
case object OnyxCauldron extends CardEffect { override val name = "Onyx Cauldron" }
case object Misdirection extends CardEffect { override val name = "Misdirection" }
case object Hailstorm extends CardEffect { override val name = "Hailstorm" }
case object ScepterOfMajesty extends CardEffect { override val name = "Scepter of Majesty" }
case object VisionsFromBeyond extends CardEffect { override val name = "Visions From Beyond" }
case object Treewalking extends CardEffect { override val name = "Treewalking" }
case object WizardsTowerCard extends CardEffect { override val name = "The Wizards Tower" }
case object FireAndLightning extends CardEffect { override val name = "Fire and Lightning" }
case object Dispel extends CardEffect { override val name = "Dispel" }
case object SummonRiverBeast extends CardEffect { override val name = "Summon River Beast" }

// The fifth dominance: six crafted improvements
case object SorceryDominance extends Dominance {
    val suit = Bird
    def name = "Sorcery Dominance"
    def id = "sorcery-dominance"
    override def altS = name
    def altL = name + " - change own win condition - now win only if having six crafted improvements at the start of Birdsong"
}

// the pieces of the Garden of Glory and The Wizards Tower, waiting off the map (`SorceryShelf`) for their cards
case object GloryGarden extends Token {
    override def name = "Garden of Glory"
    override def imgid(f : Faction) = "sorcery-garden"
}

case object WizardsTower extends Building {
    override def name = "Wizards Tower"
    override def imgid(f : Faction) = "sorcery-tower"
}

case object SorceryShelf extends SpecialRegion
case object OnCauldron extends SpecialRegion

// per battle (cleared when each battle ends)
case object ForesightAsked extends BattleEffect with HiddenEffect
case object RiverBeastUsed extends BattleEffect with HiddenEffect
// per removal
case class CauldronSkip(c : Clearing) extends RemoveEffect with HiddenEffect
// per turn
case object GardenScored extends HiddenEffect
case object CauldronAsked extends HiddenEffect

class SorceryState {
    var battle = false
    var phoenix : $[Faction] = $
    var misdirected : $[Faction] = $
    var battleRemoval = false
    var visions = false
    var dealt = false
    var hail : $[(Clearing, Figure)] = $
}

// Treewalking: between two clearings of one forest
case object TreeWalk extends Transport {
    override def img = Image("forest", styles.widepiece)

    override def allows(f : Faction)(implicit game : Game) = game.options.has(SorceryDeck) && f.can(Treewalking)

    override def allows(f : Faction, o : Region, d : Region)(implicit game : Game) = allows(f, o) && (o, d) @@ {
        case (o : Clearing, d : Clearing) => o != d && game.board.forests.exists(x => game.board.fromForest(x).use(l => l.has(o) && l.has(d)))
        case _ => false
    }

    override def order(l : $[Movable]) = 1
}

// Spell of Flight: along a river as along a path
case object FlightRiver extends ByWater {
    override def img = Image("Riverboat", styles.widepiece)
}


// CRAFTED
case class GardenAskAction(self : Faction) extends ForcedAction
case class GardenPlaceAction(self : Faction, c : Clearing) extends BaseAction(GardenOfGlory, "places the", GloryGarden.of(self), "in")(c)
case class TowerAskAction(self : Faction) extends ForcedAction
case class TowerPlaceAction(self : Faction, c : Clearing) extends BaseAction(WizardsTowerCard, "places the", WizardsTower.of(self), "in")(c)

// HAILSTORM
// each clearing's piece is its own recorded pick (kept in `game.sorcery.hail`), as the bots would otherwise try every combination
case class HailPickAction(self : Faction, todo : $[Clearing]) extends ForcedAction
case class HailChoiceAction(self : Faction, todo : $[Clearing], u : Figure) extends BaseAction(Hailstorm, "removes in", todo.head)(u.piece.of(u.faction), u.piece.imgd(u.faction))
case class HailstormAction(self : Faction) extends ForcedAction
case class HailDamageAskAction(self : Faction, l : $[Hero], then : ForcedAction) extends ForcedAction
case class HailDamageAction(self : Hero, i : ItemRef, l : $[Hero], then : ForcedAction) extends BaseAction(Hailstorm, "damages an item of", self)(i.img)

// ENCHANTED OBJECTS
case class EnchantedAskAction(self : Faction, i : Item, then : ForcedAction) extends ForcedAction
case class EnchantedSkipAction(self : Faction, then : ForcedAction) extends BaseAction(None)("Skip", EnchantedObjects)
case class EnchantedDrawAction(self : Faction, n : Int, then : ForcedAction) extends BaseAction(EnchantedObjects)("Draw", n.cards, (n > 1).?("then discard " ~ n.cards))
case class EnchantedDiscardAction(self : Faction, n : Int, then : ForcedAction) extends ForcedAction
case class EnchantedDiscardCardAction(self : Faction, d : DeckCard, n : Int, then : ForcedAction) extends BaseAction(EnchantedObjects, "discards", n.cards)(d.img) with ViewCard
case class EnchantedRemoveAction(self : Faction, c : Clearing, e : Faction, p : Piece, then : ForcedAction) extends BaseAction(EnchantedObjects, "removes a warrior")(p.of(e), "in", c)
case class EnchantedCraftAction(self : Faction, d : DeckCard, then : ForcedAction) extends BaseAction(EnchantedObjects, "crafts without paying its cost")(d.img) with ViewCard
case class EnchantedPlaceAction(self : WarriorFaction, c : Clearing, n : Int, then : ForcedAction) extends BaseAction(EnchantedObjects, "places", n.of("warrior"), "in")(c)

// MANTRA OF FRENZY
case class MantraAskAction(self : Faction, then : ForcedAction) extends ForcedAction
case class MantraSkipAction(self : Faction, then : ForcedAction) extends BaseAction(None)("Skip", MantraOfFrenzy)

// FORESIGHT
case class ForesightAction(self : Faction, b : Battle, then : ForcedAction) extends BaseAction(Foresight, "against the ambush")("Discard", Foresight, "to cancel the ambush")
case class ForesightSkipAction(self : Faction, b : Battle, then : ForcedAction) extends BaseAction(Foresight, "against the ambush")("Keep", Foresight)
case class ForesightTakeAction(self : Faction, e : Faction, d : DeckCard, then : ForcedAction) extends BaseAction(Foresight, "takes from the hand of", e)(d.img) with ViewCard

// MISDIRECTION
case class MisdirectAction(self : Faction, e : Faction, then : ForcedAction) extends BaseAction(Misdirection, "turns the battle against")(e)
case class MisdirectSkipAction(self : Faction, then : ForcedAction) extends BaseAction(None)("Keep", Misdirection)

// ENCHANTED GIFTS
case class GiftsMainAction(self : Faction) extends BaseAction(Daylight)(EnchantedGifts) with Soft
case class GiftsGiveAction(self : Faction, e : Faction, i : ItemRef) extends BaseAction(EnchantedGifts, "gives an item to")(e, i.img)
case class GiftsTakeAction(self : Faction, e : Faction, d : DeckCard) extends BaseAction(EnchantedGifts, "takes from the hand of", e)(d.img) with ViewCard

// SPELL OF FLIGHT
case class FlightMainAction(self : Faction) extends BaseAction(Daylight)(SpellOfFlight) with Soft
case class FlightDoneAction(self : Faction, then : ForcedAction) extends ForcedAction

// SCEPTER OF MAJESTY
case class ScepterMainAction(self : Faction) extends BaseAction(None)(ScepterOfMajesty) with Soft
case class ScepterAction(self : Faction, l : $[DeckCard]) extends BaseAction(ScepterOfMajesty, "discards three matching cards for", 3.vp)(l./(_.img).merge)

// VISIONS FROM BEYOND
case class VisionsAskAction(self : Faction, then : ForcedAction) extends ForcedAction
case class VisionsDiscardAction(self : Faction, d : DeckCard, then : ForcedAction) extends BaseAction(VisionsFromBeyond, "discards a card")(d.img) with ViewCard

// ONYX CAULDRON
case class CauldronPlaceAction(self : WarriorFaction, c : Clearing, then : ForcedAction) extends BaseAction(OnyxCauldron, "can take the warriors removed in", c)("Place them on", OnyxCauldron)
case class CauldronSkipAction(self : Faction, c : Clearing, then : ForcedAction) extends BaseAction(OnyxCauldron, "can take the warriors removed in", c)("Skip")
case class CauldronClearingAction(self : Faction, c : Clearing) extends BaseAction("Start of", Evening, OnyxCauldron, "moves warriors from the card to")(c) with Soft
case class CauldronMoveAction(self : WarriorFaction, c : Clearing, d : DeckCard) extends BaseAction(OnyxCauldron, "spends a card to move warriors to", c)(d.img) with ViewCard
case class CauldronDoneAction(self : Faction) extends BaseAction(None)("Skip", OnyxCauldron)

// RIVER BEAST, FIRE AND LIGHTNING
case class FireLightningAction(self : Faction, then : ForcedAction) extends BaseAction(self, "in battle")("Deal an extra", 1.hit, "and discard", FireAndLightning)
case class RiverBeastAction(self : Faction, d : DeckCard, then : ForcedAction) extends BaseAction(self, "in battle", SummonRiverBeast)("Spend", d, "for an extra", 1.hit)

// DISPEL
case class DispelAction(self : Faction, e : Faction, d : DeckCard) extends BaseAction(Dispel, "discard")(d, "of", e)


object SorceryDeckExpansion extends Expansion {
    def active(setup : $[Faction], options : $[Meta.O]) = options.has(SorceryDeck)

    // the pieces of the Garden of Glory and The Wizards Tower
    val shelved = $[Piece](GloryGarden, WizardsTower)

    def cardOf(p : Piece) : CardEffect = (p == GloryGarden).?[CardEffect](GardenOfGlory).|(WizardsTowerCard)

    // The Wizards Tower: a crafting piece of any suit while on the map
    def towerCraft(f : Faction)(implicit game : Game) : $[SuitAsset] = (game.options.has(SorceryDeck) && f.all(WizardsTower).any).$(OneOf($(Fox, Rabbit, Mouse, Frog)))

    // Living Buildings: its owner's buildings count twice for rule, and one more maximum rolled hit where any of them are
    def living(f : Faction)(implicit game : Game) = game.options.has(SorceryDeck) && f.has(LivingBuildings)

    def livingBonus(f : Faction, c : Clearing)(implicit game : Game) = (living(f) && f.at(c).of[Building].any).??(1)

    def improvements(f : Faction)(implicit game : Game) = f.stuck.$.of[CraftEffectCard].num

    def dominanceMet(f : Faction)(implicit game : Game) = improvements(f) >= 6

    def stray(implicit game : Game) = factions.%(game.states.contains).exists(e => e.reserve.$.exists(u => shelved.has(u.piece)))

    // the items a faction could give away: the Vagabond's beyond those it started with
    def giftable(f : Faction)(implicit game : Game) : $[ItemRef] = f @@ {
        case h : Hero => h.character./(_.starting(options)).|($).foldLeft(h.inv)((l, i) => l.diff(l.%(_.item == i).take(1)))
        case e => e.forTrade
    }

    def hailClearings(f : Faction)(implicit game : Game) = clearings.%(c => hailPieces(f, c).any)

    def hailPieces(f : Faction, c : Clearing)(implicit game : Game) : $[Figure] = {
        val all = (factions ++ hirelings).%(game.states.contains)
        val warriors = all.exists(e => e.from(c).$.exists(u => u.piece.is[Warrior] && u.piece.is[Pawn].not && u.piece.is[Tenacious].not))

        all./~(e => e.from(c).$.%(u => u.piece @@ {
            case _ : Tenacious => false
            case _ : Pawn => false
            case _ : Warrior => warriors
            case _ : Building => warriors.not
            case _ : Token => warriors.not
            case _ => false
        }).%(u => u.faction == f || f.canRemove(c)(u.faction)))
    }

    def flightTransports(f : Faction)(implicit game : Game) : $[$[Transport]] = (game.transports :+ FlightRiver)./($) ** f.transports./(_.but(RuledMove))

    def birdsongStart(f : Faction)(implicit game : Game) : $[UserAction] =
        f.can(Dispel).??(factions./~(e => e.stuck.$.of[CraftEffectCard].%(d => e != f || d.effect != Dispel)./(d => DispelAction(f, e, d))))

    def scepterSets(f : Faction)(implicit game : Game) : $[$[DeckCard]] = (FoxRabbitMouse :+ Bird)./~(s => f.hand.%(_.matches(s)).combinations(3).$).distinct

    def offer(f : Faction)(implicit game : Game, ask : ActionCollector) {
        if (game.current == f && f.can(ScepterOfMajesty))
            + ScepterMainAction(f).!(scepterSets(f).none, "no three matching cards")
    }

    def postRoll(f : Faction, b : Battle, fh : Int, oh : Int, fe : Int, oe : Int, full : $[Faction], ft : Int, ot : Int)(implicit game : Game) : $[UserAction] = {
        var actions : $[UserAction] = $

        if (f.has(FireAndLightning) && f == b.attacker && fh + fe < ot)
            actions :+= FireLightningAction(f, BattlePostRollAction(b, fh, oh, fe + 1, oe, full))

        if (f.has(SummonRiverBeast) && f.used.has(RiverBeastUsed).not && game.riverside.has(b.clearing)) {
            val l = f.hand.%(_.matches(b.clearing.cost))

            if (f == b.attacker && fh + fe < ot)
                actions ++= l./(d => RiverBeastAction(f, d, BattlePostRollAction(b, fh, oh, fe + 1, oe, full)))

            if (f == b.defender && oh + oe < ft)
                actions ++= l./(d => RiverBeastAction(f, d, BattlePostRollAction(b, fh, oh, fe, oe + 1, full)))
        }

        actions
    }

    def discarded(f : Faction, effect : Effect)(implicit game : Game) {
        if (effect == GardenOfGlory || effect == WizardsTowerCard) {
            val p = (effect == GardenOfGlory).?[Piece](GloryGarden).|(WizardsTower)

            clearings.foreach { c =>
                f.from(c).$.%(_.piece == p).foreach { u =>
                    u --> SorceryShelf
                    f.log("took back", p.of(f), "from", c)
                }
            }
        }

        if (effect == OnyxCauldron)
            f.from(OnCauldron).$.foreach(_ --> game.pool)
    }

    def perform(action : Action, soft : Void)(implicit game : Game) = action @@ {
        // THE PIECES: a Garden of Glory or Wizards Tower removed from the map goes back off it, and its card is discarded
        case a if a.isSoft.not && game.turn > 0 && stray =>
            factions.%(game.states.contains).foreach { e =>
                e.reserve.$.%(u => shelved.has(u.piece)).foreach { u =>
                    u --> SorceryShelf

                    if (e.has(cardOf(u.piece))) {
                        e.log("discarded", cardOf(u.piece))
                        e.removeStuckEffect(cardOf(u.piece))
                    }
                }
            }

            TryAgain

        // CRAFTED
        case CraftPerformAction(f, d @ CraftEffectCard(_, _, _, GardenOfGlory), m) =>
            f.hand --> d --> f.stuck
            f.effects :+= GardenOfGlory

            CraftEffectAction(f, d, m, GardenAskAction(f))

        case GardenAskAction(f) =>
            Ask(f).group(GardenOfGlory, "places the", GloryGarden.of(f)).each(clearings)(c => GardenPlaceAction(f, c))

        case GardenPlaceAction(f, c) =>
            f.from(SorceryShelf) --> GloryGarden --> c

            f.log("placed the", GloryGarden.of(f), "in", c)

            Repeat

        case CraftPerformAction(f, d @ CraftEffectCard(_, _, _, WizardsTowerCard), m) =>
            f.hand --> d --> f.stuck
            f.effects :+= WizardsTowerCard

            CraftEffectAction(f, d, m, TowerAskAction(f))

        case TowerAskAction(f) =>
            val l = clearings.%(f.rules).%(c => game.freeSlots(c) > 0)

            if (l.none) {
                f.log("had no clearing to place the", WizardsTower.of(f))

                Repeat
            }
            else
                Ask(f).group(WizardsTowerCard, "places the", WizardsTower.of(f)).each(l)(c => TowerPlaceAction(f, c))

        case TowerPlaceAction(f, c) =>
            f.from(SorceryShelf) --> WizardsTower --> c

            f.log("placed the", WizardsTower.of(f), "in", c)

            Repeat

        // HAILSTORM
        case CraftPerformAction(f, d @ CraftEffectCard(_, _, _, Hailstorm), m) =>
            f.hand --> d --> discard.quiet

            f.log("crafted", d, "(" ~ d.cost.ss ~ ")", m)

            game.sorcery.hail = $

            HailPickAction(f, hailClearings(f))

        case HailPickAction(f, todo) =>
            if (todo.none)
                HailstormAction(f)
            else {
                val l = hailPieces(f, todo.head)
                val kinds = l.distinctBy(u => (u.faction, u.piece))

                Ask(f).group(Hailstorm, "removes a piece in", todo.head)
                    .each(kinds)(u => HailChoiceAction(f, todo, u))
            }

        case HailChoiceAction(f, todo, u) =>
            game.sorcery.hail :+= (todo.head, u)

            HailPickAction(f, todo.drop(1))

        case HailstormAction(f) =>
            val (cc, ff) = game.sorcery.hail.unzip

            game.sorcery.hail = $

            game.highlights :+= NukeHighlight(cc)

            val heroes = factions.of[Hero].%(game.states.contains).%(_.inv.notDamaged.any)

            var q : ForcedAction = ForcedRemoveFinishedAction(f, HailDamageAskAction(f, heroes, Repeat))

            cc.lazyZip(ff).foreach { (c, u) =>
                f.log("removed", u.piece.of(u.faction), "in", c, "with", Hailstorm)

                val n = (u.faction != f && u.piece.is[Scoring]).??(1)

                q = TryForcedRemoveAction(f, c, u.faction, u.piece, n, Removing, q, q)
            }

            q

        case HailDamageAskAction(f, l, then) =>
            l.%(_.inv.notDamaged.any) match {
                case h :: rest =>
                    Ask(h).group(Hailstorm, "damages an item of", h).each(h.inv.notDamaged.distinct)(i => HailDamageAction(h, i, rest, then))
                case _ =>
                    then
            }

        case HailDamageAction(h, i, rest, then) =>
            h.inv = h.inv.diff($(i)) :+ i.damage

            h.log("damaged", i, "in the", Hailstorm)

            HailDamageAskAction(h, rest, then)

        // ENCHANTED OBJECTS
        case CraftScoreAction(f, d, n, m, then) if f.has(EnchantedObjects) && then.is[EnchantedAskAction].not =>
            CraftScoreAction(f, d, n, m, EnchantedAskAction(f, d.item, then))

        case EnchantedAskAction(f, i, then) =>
            val skip = EnchantedSkipAction(f, then)

            i match {
                case Sword =>
                    val l = clearings.%(f.canAttackIn)
                    if (l.none)
                        then
                    else
                        BattleInitAction(f, f, WithEffect(EnchantedObjects), l, $(skip), then)

                case Boots =>
                    val l = f.moveFrom
                    if (l.none)
                        then
                    else
                        MoveInitAction(f, f, $, WithEffect(EnchantedObjects), l, f.movable, $(skip), then)

                case Bag =>
                    Ask(f).group(EnchantedObjects, "for", Bag).add(EnchantedDrawAction(f, 1, then)).add(skip)

                case Teapot =>
                    Ask(f).group(EnchantedObjects, "for", Teapot).add(EnchantedDrawAction(f, 2, then)).add(skip)

                case Crossbow =>
                    val l = clearings./~(c => f.enemies.%(game.states.contains).%(f.canRemove(c))./~(e => e.at(c).of[Warrior].%(_.is[Pawn].not).%(_.is[Tenacious].not).distinct./(p => (c, e, p : Piece))))

                    if (l.none)
                        then
                    else
                        Ask(f).group(EnchantedObjects, "for", Crossbow, "removes a warrior").each(l) { case (c, e, p) => EnchantedRemoveAction(f, c, e, p, then) }.add(skip)

                case Hammer =>
                    val free = List.fill(9)(Bird : SuitAsset)
                    val l = f.hand.%(_.is[CraftCard]).%(f.craftableWith(free, $))

                    if (l.none)
                        then
                    else
                        Ask(f).group(EnchantedObjects, "for", Hammer, "crafts another card without paying its cost").each(l)(d => EnchantedCraftAction(f, d, then)).add(skip)

                case Coins =>
                    f.as[WarriorFaction] match {
                        case Some(w) if w.pooled(w.warrior) > 0 =>
                            val l = clearings.%(w.rules).%(w.canPlace)

                            if (l.none)
                                then
                            else
                                Ask(f).group(EnchantedObjects, "for", Coins, "places warriors").each(l)(c => EnchantedPlaceAction(w, c, min(2, w.pooled(w.warrior)), then)).add(skip)

                        case _ =>
                            then
                    }

                case _ =>
                    then
            }

        case EnchantedSkipAction(f, then) =>
            then

        case EnchantedDrawAction(f, n, then) =>
            DrawCardsAction(f, n, WithEffect(EnchantedObjects), AddCardsAction(f, (n > 1).?[ForcedAction](EnchantedDiscardAction(f, n, then)).|(then)))

        case EnchantedDiscardAction(f, n, then) =>
            if (n <= 0 || f.hand.none)
                then
            else
                Ask(f).group(EnchantedObjects, "discards", n.cards).each(f.hand.$)(d => EnchantedDiscardCardAction(f, d, n, then)).add(NoHand)

        case EnchantedDiscardCardAction(f, d, n, then) =>
            f.hand --> d --> discard(f)

            EnchantedDiscardAction(f, n - 1, then)

        case EnchantedRemoveAction(f, c, e, p, then) =>
            f.log("removed", p.of(e), "in", c, "with", EnchantedObjects)

            TryForcedRemoveAction(f, c, e, p, 0, Removing, ForcedRemoveFinishedAction(f, then), ForcedRemoveFinishedAction(f, then))

        case EnchantedCraftAction(f, d, then) =>
            f.log("crafted", d, "without paying its cost with", EnchantedObjects)

            CraftPerformAction(f, d, WithEffect(EnchantedObjects))

        case EnchantedPlaceAction(f, c, n, then) =>
            f.reserve --> (f.warrior *** n) --> c

            f.log("placed", n.of(f.warrior.name).styled(f), "in", c, "with", EnchantedObjects)

            then

        // START OF BIRDSONG - DISPEL
        case DispelAction(f, e, CraftEffectCard(_, _, _, effect)) =>
            f.removeStuckEffect(Dispel)

            e.removeStuckEffect(effect)

            f.log("discarded", Dispel, "to discard", effect, "of", e)

            Repeat

        // ReplayCheck: every effect but Hailstorm dealt out crafted, in turn, at the first Birdsong
        case BirdsongNAction(5, f) if options.has(SorceryTestDeal) && game.sorcery.dealt.not =>
            game.sorcery.dealt = true

            val l = (game.deck.$ ++ pile.$).of[CraftEffectCard].%(d => Deck.sorcery.has(d)).%(_.effect != Hailstorm)

            l.indexed.foreach { (d, i) =>
                val e : Faction = factions.apply(i % factions.num)

                if (e.has(d.effect).not) {
                    if (game.deck.$.has(d)) game.deck --> d --> e.stuck else pile --> d --> e.stuck
                    e.effects :+= d.effect

                    d.effect @@ {
                        case GardenOfGlory => clearings.%(e.present).starting.foreach(c => e.from(SorceryShelf) --> GloryGarden --> c)
                        case WizardsTowerCard => clearings.%(e.rules).%(c => game.freeSlots(c) > 0).starting.foreach(c => e.from(SorceryShelf) --> WizardsTower --> c)
                        case _ =>
                    }
                }
            }

            BirdsongNAction(5, f)

        // START OF DAYLIGHT, DAYLIGHT
        case GiftsMainAction(f) =>
            Ask(f).group(EnchantedGifts, "gives an item to look at a hand and take a card")
                .some(factions.but(f))(e => giftable(f).distinct./(i => GiftsGiveAction(f, e, i)))
                .cancel

        case GiftsGiveAction(f, e, i) =>
            f.used :+= EnchantedGifts

            f @@ {
                case h : Hero => h.inv :-= i
                case f => f.forTrade :-= i
            }

            e @@ {
                case h : Hero => h.inv :+= i.item.pristine
                case e => e.forTrade :+= i
            }

            f.log("gave", i, "to", e, "with", EnchantedGifts)

            if (e.hand.none)
                Repeat
            else
                Ask(f).group(EnchantedGifts, "takes a card from the hand of", e).each(e.hand.$)(d => GiftsTakeAction(f, e, d))

        case GiftsTakeAction(f, e, d) =>
            e.hand --> d --> f.hand

            f.log("took a card from", e, "with", EnchantedGifts)

            e.notify($(ViewCardInfoAction(e, Gave(e, f).elem(game), d)))

            Repeat

        case FlightMainAction(f) =>
            MoveInitAction(f, f, flightTransports(f), WithEffect(SpellOfFlight), f.movable, f.movable, $(CancelAction), FlightDoneAction(f, Repeat))

        case FlightDoneAction(f, then) =>
            f.removeStuckEffect(SpellOfFlight)

            f.log("discarded", SpellOfFlight)

            then

        case ScepterMainAction(f) =>
            Ask(f).group(ScepterOfMajesty, "discards three matching cards for", 3.vp)
                .each(scepterSets(f))(l => ScepterAction(f, l))
                .cancel

        case ScepterAction(f, l) =>
            f.used :+= ScepterOfMajesty

            f.hand --> l --> discard.quiet

            f.oscore(3)("discarding", l./(_.elem).comma, "with", ScepterOfMajesty)

            Repeat

        // START OF EVENING - GARDEN OF GLORY, ONYX CAULDRON
        case EveningNAction(0, f) if f.used.has(GardenScored).not && factions.%(game.states.contains).exists(e => e.all(GloryGarden).exists(f.rules)) =>
            f.used :+= GardenScored

            factions.%(game.states.contains)./~(e => e.all(GloryGarden)).%(f.rules).foldLeft[ForcedAction](EveningNAction(0, f)) { (q, c) =>
                f.oscore(1)("ruling the", GardenOfGlory, "in", c)

                DrawCardsAction(f, 1, WithEffect(GardenOfGlory), AddCardsAction(f, q))
            }

        case EveningNAction(0, f) if f.has(OnyxCauldron) && f.used.has(CauldronAsked).not && f.from(OnCauldron).any && clearings.exists(c => f.rules(c) && f.canPlace(c) && f.hand.exists(_.matches(c.cost))) =>
            Ask(f).group("Start of", Evening, OnyxCauldron)
                .each(clearings.%(c => f.rules(c) && f.canPlace(c) && f.hand.exists(_.matches(c.cost))))(c => CauldronClearingAction(f, c))
                .add(CauldronDoneAction(f))
                .needOk

        case CauldronClearingAction(f, c) =>
            Ask(f).group(OnyxCauldron, "spends a card to move warriors to", c)
                .each(f.hand.%(_.matches(c.cost)))(d => CauldronMoveAction(f.as[WarriorFaction].get, c, d))
                .cancel

        case CauldronMoveAction(f, c, d) =>
            f.used :+= CauldronAsked

            f.hand --> d --> discard.quiet

            val l = f.from(OnCauldron).$.take(2)

            l.foreach(_ --> c)

            f.log("spent", d, "to move", l./(_.elem).comma, "from", OnyxCauldron, "to", c)

            EveningNAction(0, f)

        case CauldronDoneAction(f) =>
            f.used :+= CauldronAsked

            EveningNAction(0, f)

        // MOVES - TREEWALKING
        case MoveListAction(self, f, t, m, from : Clearing, to : Clearing, l, then) if t.has(TreeWalk) =>
            log(MovementExpansion.describe(self, f, $, l), "from", from, "to", to, m, "with", Treewalking)

            Force(MoveListAction(self, f, $, NoMessage, from, to, l, UsedEffectAction(f, Treewalking, then)))

        case MoveListAction(self, f, t, m, from : Clearing, to : Clearing, l, then) if t.has(FlightRiver) =>
            log(MovementExpansion.describe(self, f, $, l), "from", from, "to", to, m, "along the river")

            Force(MoveListAction(self, f, $, NoMessage, from, to, l, then))

        // DRAWS - VISIONS FROM BEYOND
        case DrawCardsAction(f, n, m, then) if n > 0 && game.turn > 0 && f.has(VisionsFromBeyond) && game.sorcery.visions.not =>
            game.sorcery.visions = true

            DrawCardsAction(f, n + 1, m, VisionsAskAction(f, then))

        case VisionsAskAction(f, then) =>
            game.sorcery.visions = false

            val l = f.drawn.$ ++ f.hand.$

            if (l.none)
                then
            else
                Ask(f).group(VisionsFromBeyond, "discards a card").each(l)(d => VisionsDiscardAction(f, d, then)).add(NoHand)

        case VisionsDiscardAction(f, d, then) =>
            if (f.drawn.$.has(d))
                f.drawn --> d --> discard.quiet
            else
                f.hand --> d --> discard.quiet

            f.log("discarded a card with", VisionsFromBeyond)

            then

        // BATTLE - MISDIRECTION
        case BattleStartAction(self, f, a, m, c, o, i, then) if o.has(Misdirection) && game.sorcery.misdirected.has(o).not && f.canAttackList(c).but(o).diff(game.sorcery.misdirected).any =>
            game.sorcery.misdirected :+= o

            val q = (e : Faction) => BattleStartAction(self, f, a, m, c, e, i, then)

            Ask(o).group(o, "is attacked by", f, "in", c, "and can turn the battle with", Misdirection)
                .each(f.canAttackList(c).but(o).diff(game.sorcery.misdirected))(e => MisdirectAction(o, e, q(e)))
                .add(MisdirectSkipAction(o, q(o)))

        case MisdirectAction(o, e, then) =>
            o.removeStuckEffect(Misdirection)

            o.log("discarded", Misdirection, "to turn the battle against", e)

            then

        case MisdirectSkipAction(o, then) =>
            then

        case BattleStartAction(self, f, a, m, c, o, i, then) if game.sorcery.battle.not =>
            game.sorcery.battle = true
            game.sorcery.phoenix = $

            UnknownContinue

        // BATTLE - FORESIGHT
        case BattleAskCounterAmbushAction(b, then) if b.attacker.has(Foresight) && b.attacker.used.has(ForesightAsked).not =>
            b.attacker.used :+= ForesightAsked

            Ask(b.attacker).group(b.defender, "ambushed in", b.clearing)
                .add(ForesightAction(b.attacker, b, then))
                .add(ForesightSkipAction(b.attacker, b, then))

        case ForesightAction(f, b, then) =>
            f.removeStuckEffect(Foresight)

            f.log("discarded", Foresight, "to cancel the ambush")

            val e = b.defender

            if (e.hand.none)
                then
            else
                Ask(f).group(Foresight, "takes a card from the hand of", e).each(e.hand.$)(d => ForesightTakeAction(f, e, d, then))

        case ForesightSkipAction(f, b, then) =>
            BattleAskCounterAmbushAction(b, then)

        case ForesightTakeAction(f, e, d, then) =>
            e.hand --> d --> f.hand

            f.log("took a card from", e, "with", Foresight)

            e.notify($(ViewCardInfoAction(e, Gave(e, f).elem(game), d)))

            then

        // BATTLE - FIRE AND LIGHTNING, RIVER BEAST
        case FireLightningAction(f, then) =>
            f.removeStuckEffect(FireAndLightning)

            f.log("dealt an extra", 1.hit, "and discarded", FireAndLightning)

            then

        case RiverBeastAction(f, d, then) =>
            f.used :+= RiverBeastUsed

            f.hand --> d --> discard.quiet

            f.log("spent", d, "to deal an extra", 1.hit, "with", SummonRiverBeast)

            then

        // BATTLE - FAVOR OF THE PHOENIX: its owner's buildings and tokens removed
        case ForcedRemoveAction(e, c, f, p, n, m, then) if game.sorcery.battle && f.has(FavorOfThePhoenix) && (p.is[Building] || p.is[Token]) && game.sorcery.phoenix.has(f).not =>
            game.sorcery.phoenix :+= f

            UnknownContinue

        // BATTLE - ONYX CAULDRON
        case BattleForcedRemoveAction(b) if game.sorcery.battleRemoval.not && factions.exists(_.has(OnyxCauldron)) =>
            game.sorcery.battleRemoval = true

            ForcedRemoveFinishedAction(b.instigator | b.attacker, BattleResolvedAction(b))

        case BattleResolvedAction(b) if game.sorcery.battleRemoval =>
            game.sorcery.battleRemoval = false

            BattleResolvedAction(b)

        case ForcedRemoveProcessAction(f : WarriorFaction, then) if game.sorcery.battleRemoval && f.has(OnyxCauldron) && game.limboClearings.exists(c => f.ignored.has(CauldronSkip(c)).not && f.limbo(c).$.exists(_.piece == f.warrior)) =>
            val c = game.limboClearings.%(c => f.ignored.has(CauldronSkip(c)).not && f.limbo(c).$.exists(_.piece == f.warrior)).first
            val q = ForcedRemoveProcessAction(f, then)

            Ask(f).group(OnyxCauldron, "can take the warriors removed in", c)
                .add(CauldronPlaceAction(f, c, q))
                .add(CauldronSkipAction(f, c, q))

        case CauldronPlaceAction(f, c, then) =>
            val l = f.limbo(c).$.%(_.piece == f.warrior)

            l.foreach(_ --> OnCauldron)

            f.ignored :+= CauldronSkip(c)

            f.log("placed", l./(_.elem).comma, "removed in", c, "on", OnyxCauldron)

            then

        case CauldronSkipAction(f, c, then) =>
            f.ignored :+= CauldronSkip(c)

            then

        // BATTLE ENDS - PHOENIX, KNOWLEDGE OF THE FALLEN, MANTRA OF FRENZY
        case BattleFinishedAction(b) if game.sorcery.battle =>
            game.sorcery.battle = false
            game.sorcery.misdirected = $

            val f = b.attacker
            val o = b.defender

            var q : ForcedAction =
                if (f == game.current && f.can(MantraOfFrenzy)) {
                    f.used :+= MantraOfFrenzy
                    BattleFinishedAction(b.copy(then = MantraAskAction(f, b.then)))
                }
                else
                    BattleFinishedAction(b)

            $(f, o).distinct.%(_.can(KnowledgeOfTheFallen)).foreach { x =>
                x.used :+= KnowledgeOfTheFallen

                val y = $(f, o).but(x).single

                x.log("and", y./(_.elem).|("no one"), "drew a card with", KnowledgeOfTheFallen)

                y.foreach { y => q = DrawCardsAction(y, 1, WithEffect(KnowledgeOfTheFallen), AddCardsAction(y, q)) }

                q = DrawCardsAction(x, 1, WithEffect(KnowledgeOfTheFallen), AddCardsAction(x, q))
            }

            game.sorcery.phoenix.%(_.has(FavorOfThePhoenix)).foreach { e =>
                q = DrawCardsAction(e, 1, WithEffect(FavorOfThePhoenix), AddCardsAction(e, q))
            }

            game.sorcery.phoenix = $

            q

        case MantraAskAction(f, then) =>
            val l = clearings.%(f.canAttackIn)

            if (l.none)
                then
            else
                BattleInitAction(f, f, WithEffect(MantraOfFrenzy), l, $(MantraSkipAction(f, then)), then)

        case MantraSkipAction(f, then) =>
            then

        case _ => UnknownContinue
    }

    override def birdsong(f : Faction)(implicit game : Game, ask : ActionCollector) {
        offer(f)
    }

    override def daylight(f : Faction)(implicit game : Game, ask : ActionCollector) {
        if (game.current == f && f.can(EnchantedGifts) && giftable(f).any)
            + GiftsMainAction(f)

        if (game.current == f && f.can(SpellOfFlight))
            + FlightMainAction(f).!(f.moveFrom.none && f.movable.none, "can't move")

        offer(f)
    }

    override def evening(f : Faction)(implicit game : Game, ask : ActionCollector) {
        offer(f)
    }
}

