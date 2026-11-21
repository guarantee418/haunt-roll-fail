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


// Capitalists and Anarchists (`CapitalistsDeck`; the site owner's variation of the Might and Right deck, 2026-11-19): the base deck's
// items, ambushes and dominances, Armorers and Sappers as printed, Dawn and Dusk's three Services in place of the Favors, and the
// other effects changed (`Deck.capitalists`, pictures `card/capitalists/` by `root/tools/capitalists-art.py`):
// - Command Warren, Cobbler, Brutal Tactics (no point for the defender) and Scouting Party are paid for each time: a card from hand matching the clearing (the battle's, or the
//   one Cobbler's move starts from; a bird matches any), or the crafted card itself, discarded, which counts as a bird
// - Better Burrow Bank is optional at the start of Birdsong and goes back to its owner's hand when used
// - Stand and Deliver! must be used at the start of the next Birdsong (once an enemy holds a card) and is discarded; the enemy draws a card
//   instead of scoring
// - Tax Collector (draws two) and Codebreakers are used once, right when crafted, and discarded
// - Scouting Party cancels an ambush played against its owner attacking
// - Royal Claim must be used at the next Birdsong


case class CapitalistsPayCardAction(self : Faction, e : CardEffect, d : DeckCard, then : ForcedAction) extends BaseAction(e, "spending")(d.img) with ViewCard
case class CapitalistsPaySelfAction(self : Faction, e : CardEffect, then : ForcedAction) extends BaseAction(e, "spending")("Discard", e, "itself, as a bird")

case class CapitalistsBrutalMainAction(self : Faction, o : Faction, c : Clearing, then : ForcedAction) extends BaseAction(self, "in battle")("Deal an extra", 1.hit, "with", BrutalTactics) with Soft
case class CapitalistsBrutalAction(self : Faction, o : Faction, then : ForcedAction) extends ForcedAction

case class CapitalistsBankAction(self : Faction, o : Faction) extends BaseAction(self, "may return", BetterBurrowBank, "to hand to draw a card and share with")(o)

case class CapitalistsScoutMainAction(self : Faction, b : Battle, then : ForcedAction) extends BaseAction(self, "can cancel the ambush")("Cancel the", "Ambush".hl, "with", ScoutingParty) with Soft
case class CapitalistsScoutedAction(self : Faction, then : ForcedAction) extends ForcedAction
// kept so that a game that recorded it before Scouting Party was paid for at each use still loads
case class CapitalistsScoutingAction(self : Faction, b : Battle, then : ForcedAction) extends BaseAction(self, "can cancel the ambush")("Discard", ScoutingParty, "to cancel the", "Ambush".hl)

case class CapitalistsCraftedAction(self : Faction, d : CraftEffectCard, m : Message) extends ForcedAction
case class CapitalistsStealAction(self : Faction, e : Faction, then : ForcedAction) extends BaseAction(StandAndDeliver, "takes a card from")(e)
case class CapitalistsTaxAction(self : WarriorFaction, c : Clearing, p : Piece) extends BaseAction(TaxCollector, "removes")(p.of(self), "in", c)


object CapitalistsDeckExpansion extends Expansion {
    def active(setup : $[Faction], options : $[Meta.O]) = options.has(CapitalistsDeck)

    val lasting = $[Effect](CommandWarren, Cobbler, BetterBurrowBank, BrutalTactics, ScoutingParty, RoyalClaim, StandAndDeliver)

    def on(implicit game : Game) = options.has(CapitalistsDeck)

    // the cards that can pay for an effect used in a region: matching its clearing, or a bird (a forest has no suit)
    def pays(d : DeckCard, r : Region)(implicit game : Game) : Boolean = r match {
        case c : Clearing => d.matches(c.cost)
        case _ => d.suit == Bird
    }

    // Better Burrow Bank at the start of Birdsong, optional (with the other optional start of Birdsong choices)
    def birdsongStart(f : Faction)(implicit game : Game) : $[UserAction] =
        f.can(BetterBurrowBank).??(factions.but(f)./(CapitalistsBankAction(f, _)))

    // Brutal Tactics in a battle's post-roll menu: paid with a card matching the battle's clearing or the card itself
    def brutal(f : Faction, b : Battle, then : ForcedAction) : UserAction = CapitalistsBrutalMainAction(f, b.defender, b.clearing, then)

    def perform(action : Action, soft : Void)(implicit game : Game) = action @@ {
        // ReplayCheck: the lasting effects dealt out crafted, the others into hands, at the first player's first Birdsong
        case BirdsongNAction(5, f) if options.has(CapitalistsTestDeal) && game.turn == 1 && factions.forall(_.effects.intersect(lasting).none) =>
            val l = (game.deck.$ ++ pile.$).of[CraftEffectCard].%(d => Deck.capitalists.has(d)).distinct

            l.indexed.foreach { (d, i) =>
                val e : Faction = factions.apply(i % factions.num)
                val to = lasting.has(d.effect).?(e.stuck).|(e.hand)

                if (e.has(d.effect).not) {
                    if (game.deck.$.has(d)) game.deck --> d --> to else pile --> d --> to

                    if (lasting.has(d.effect))
                        e.effects :+= d.effect
                }
            }

            BirdsongNAction(5, f)

        // PAYMENTS
        case CapitalistsPayCardAction(f, e, d, then) =>
            f.hand --> d --> discard.quiet

            f.log("spent", d, "for", e)

            then

        case CapitalistsPaySelfAction(f, e, then) =>
            f.removeStuckEffect(e)

            f.log("discarded", e, "to use it")

            then

        // COMMAND WARREN
        case DaylightNAction(0, f) if f.has(CommandWarren) =>
            soft()

            val cw = clearings.%(f.canAttackIn)

            if (cw.none)
                NoAsk(f)(Next)
            else
                Ask(f).group("Start of", Daylight, "-", CommandWarren, "battles, spending")
                    .each(f.hand.$) { d => val l = cw.%(pays(d, _)); CapitalistsPayCardAction(f, CommandWarren, d, BattleInitAction(f, f, WithEffect(CommandWarren), l, $, Next)).!(l.none, "no matching clearing") }
                    .add(CapitalistsPaySelfAction(f, CommandWarren, BattleInitAction(f, f, WithEffect(CommandWarren), cw, $, Next)))
                    .skip(Next)

        // COBBLER
        case EveningNAction(0, f) if f.can(Cobbler) =>
            soft()

            val from = f.moveFrom

            if (from.none)
                NoAsk(f)(Next)
            else
                Ask(f).group("Start of", Evening, "-", Cobbler, "moves, spending")
                    .each(f.hand.$) { d => val l = from.%(pays(d, _)); CapitalistsPayCardAction(f, Cobbler, d, MoveInitAction(f, f, $, WithEffect(Cobbler), l, f.movable, $, UsedEffectAction(f, Cobbler, Repeat))).!(l.none, "no matching clearing") }
                    .add(CapitalistsPaySelfAction(f, Cobbler, MoveInitAction(f, f, $, WithEffect(Cobbler), from, f.movable, $, Repeat)))
                    .skip(Next)
                    .evening(f)

        // BRUTAL TACTICS
        case CapitalistsBrutalMainAction(f, o, c, then) =>
            val next = CapitalistsBrutalAction(f, o, then)

            Ask(f)
                .each(f.hand.$)(d => CapitalistsPayCardAction(f, BrutalTactics, d, next).!(pays(d, c).not))
                .add(CapitalistsPaySelfAction(f, BrutalTactics, next))
                .cancel
                .needOk

        case CapitalistsBrutalAction(f, o, then) =>
            f.used :+= BrutalTactics

            f.log("employed", BrutalTactics, "to deal an extra", 1.hit)

            then

        // BETTER BURROW BANK
        case CapitalistsBankAction(f, o) =>
            f.used :+= BetterBurrowBank

            val d = f.stuck.$.of[CraftEffectCard].%(_.effect == BetterBurrowBank).take(1)

            f.effects :-= BetterBurrowBank

            f.stuck --> d --> f.hand

            f.log("returned", BetterBurrowBank, "to hand and shared it with", o.elem ~ ",", ("each drew " ~ 1.hl ~ " card").spn(xlo.nowrap))

            DrawCardsAction(f, 1, SuppressLog, AddCardsAction(f, DrawCardsAction(o, 1, SuppressLog, AddCardsAction(o, Repeat))))

        // STAND AND DELIVER!: must be used at the start of the next Birdsong (kept while no enemy holds a card)
        case BirdsongNAction(0, f) if f.can(StandAndDeliver) && factions.but(f).exists(_.hand.any) =>
            Ask(f).group("Start of", Birdsong, "-", StandAndDeliver, "takes a card from")
                .each(factions.but(f).%(_.hand.any))(e => CapitalistsStealAction(f, e, BirdsongNAction(0, f)))

        // ROYAL CLAIM: must be used at the next Birdsong
        case BirdsongNAction(0, f) if f.can(RoyalClaim) =>
            Ask(f).add(RoyalClaimAction(f, clearings.%(f.ruleSelf).num))

        // SCOUTING PARTY: the ambush is played, then the attacker may cancel it, spending a card matching the clearing or Scouting Party
        case BattleAskCounterAmbushAction(b, then) if b.attacker.can(ScoutingParty) =>
            BattleExpansion.askCounterAmbush(b, 2, then).prepend(CapitalistsScoutMainAction(b.attacker, b, then)).needOk

        case CapitalistsScoutMainAction(f, b, then) =>
            val next = CapitalistsScoutedAction(f, then)

            Ask(f)
                .each(f.hand.$)(d => CapitalistsPayCardAction(f, ScoutingParty, d, next).!(pays(d, b.clearing).not))
                .add(CapitalistsPaySelfAction(f, ScoutingParty, next))
                .cancel
                .needOk

        case CapitalistsScoutedAction(f, then) =>
            f.log("cancelled the", "Ambush".hl, "with", ScoutingParty)

            then

        case CapitalistsScoutingAction(f, b, then) =>
            f.removeStuckEffect(ScoutingParty)

            f.log("discarded", ScoutingParty, "to cancel the", "Ambush".hl)

            then

        // ONE-TIME EFFECTS, used when crafted
        case CraftPerformAction(f, d @ CraftEffectCard(_, _, _, TaxCollector | Codebreakers), m) =>
            f.hand --> d --> discard.quiet

            f.log("crafted", d, "(" ~ d.cost.ss ~ ")", m)

            CapitalistsCraftedAction(f, d, m)

        case CapitalistsCraftedAction(f, d, m) => d.effect match {
            case TaxCollector =>
                val l = f.as[WarriorFaction]./~(f => clearings./~(c => f.at(c).of[Warrior].notOf[Tenacious].distinct./(p => CapitalistsTaxAction(f, c, p))))

                if (l.none) {
                    f.log("had no warrior to remove for", TaxCollector)

                    Repeat
                }
                else
                    Ask(f).add(l)

            case Codebreakers =>
                val l = factions.but(f).%(_.hand.any)

                if (l.none) {
                    f.log("had no one to look at")

                    Repeat
                }
                else
                    Ask(f).each(l)(e => CodebreakersAction(f, e, Repeat))

            case _ => Repeat
        }

        case CapitalistsStealAction(f, e, then) =>
            f.removeStuckEffect(StandAndDeliver)

            f.log("took a random card from", e, "with", StandAndDeliver, "and", e, "drew a card")

            StealCardAction(f, e, DrawCardsAction(e, 1, SuppressLog, AddCardsAction(e, then)))

        case CapitalistsTaxAction(f, c, p) =>
            f.from(c) --> p --> game.removed

            DrawCardsAction(f, 2, ConcatMessage(WithEffect(TaxCollector), InClearing(c)), AddCardsAction(f, Repeat))

        case _ => UnknownContinue
    }
}
