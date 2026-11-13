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


// Action! Deck Booster (The Doomed Expansion, fan-made by slugfacekillah; Law of Doom D3.2; Gman, 2026-11-13): ten cards shuffled
// into the shared deck, one booster for each player above five (or one with five players, `ActionBooster`). An Action! card is
// spent from hand on its owner's turn (or, for the sword's extra hit, in a battle they are in) for its effect; it is also a card
// of its suit for everything else. The two Secret! cards are birds: either one can be spent to draw the top card of the deck in
// its place, the first cancels an ambush played against its owner (the attacker's counter-ambush) and the second scores 3 points
// at the start of Birdsong for three crafted improvements or a Dominance whose condition its owner fulfills.

trait ActionEffect extends CardEffect {
    val item : String
    val text : String
}

case object ActionHammer extends ActionEffect {
    val name = "Action! Hammer"
    val item = "Hammer"
    val text = "may activate a single crafting piece a second time this turn"
}

case object ActionSword extends ActionEffect {
    val name = "Action! Sword"
    val item = "Sword"
    val text = "deal one extra hit or, if your turn, may initiate a battle"
}

case object ActionBag extends ActionEffect {
    val name = "Action! Bag"
    val item = "Bag"
    val text = "if you would discard cards (during this action) discard two fewer"
}

case object ActionTea extends ActionEffect {
    val name = "Action! Tea"
    val item = "Tea"
    val text = "place two warriors in a clearing where you already have pieces"
}

case object ActionCoins extends ActionEffect {
    val name = "Action! Coins"
    val item = "Coins"
    val text = "immediately draw three cards, discard two of them"
}

case object ActionBoots extends ActionEffect {
    val name = "Action! Boots"
    val item = "Boots"
    val text = "take a move now, as long as no dice have been rolled"
}

case object ActionCrossbow extends ActionEffect {
    val name = "Action! Crossbow"
    val item = "Crossbow"
    val text = "discard an enemy's crafted card by spending a card of the same suit"
}

case object ActionKey extends ActionEffect {
    val name = "Action! Key"
    val item = "Key"
    val text = "remove up to three of your warriors from the map and place them in any clearing"
}

case object SecretCancel extends ActionEffect {
    val name = "Secret! Cancel"
    val item = ""
    val text = "replace with the top card of the deck, or cancel an ambush played against you"
}

case object SecretScore extends ActionEffect {
    val name = "Secret! Score"
    val item = ""
    val text = "replace with the top card of the deck, or at the start of Birdsong score 3 points with three crafted improvements or a fulfilled Dominance"
}

case class ActionCard(suit : Suit, id : String, effect : ActionEffect) extends DeckCard {
    def name = effect.name
    def secret = effect == SecretCancel || effect == SecretScore
    override def altS = name
    def altL = name + " - " + effect.text
}


case class ActionPayAction(self : Faction, d : DeckCard, then : ForcedAction) extends ForcedAction

case class ActionCardMainAction(self : Faction, p : Phase, d : ActionCard) extends BaseAction(p)(d.effect) with Soft
case class ActionReplaceAction(self : Faction, d : ActionCard) extends BaseAction(d.effect)("Discard to draw the top card of the deck")
case class ActionHammerAction(self : Faction, d : ActionCard, a : SuitAsset) extends BaseAction(ActionHammer, "activates a crafting piece again")(dt.CraftSuit(a))
case class ActionBagAction(self : Faction, d : ActionCard) extends BaseAction(ActionBag)("Hand limit", 2.hl, "more this turn")
case class ActionTeaAction(self : WarriorFaction, d : ActionCard, c : Clearing) extends BaseAction(ActionTea, "places warriors in")(c)
case class ActionCoinsAction(self : Faction, d : ActionCard) extends BaseAction(ActionCoins)("Draw", 3.cards, "and discard", 2.cards)
case class ActionCoinsKeepAction(self : Faction, then : ForcedAction) extends ForcedAction
case class ActionCoinsDiscardAction(self : Faction, l : $[DeckCard], then : ForcedAction) extends ForcedAction
case class ActionCrossbowTargetAction(self : Faction, d : ActionCard, e : Faction, x : DeckCard) extends BaseAction(ActionCrossbow, "discards a crafted card of", e)(x.img) with ViewCard with Soft
case class ActionCrossbowAction(self : Faction, d : ActionCard, e : Faction, x : DeckCard, p : DeckCard) extends BaseAction(ActionCrossbow, "discards", x, "of", e, "spending")(p.img) with ViewCard
case class ActionKeyDestinationAction(self : WarriorFaction, d : ActionCard, to : Clearing) extends BaseAction(ActionKey, "places warriors in")(to) with Soft
case class ActionKeyPickAction(self : WarriorFaction, d : ActionCard, to : Clearing, l : $[Clearing]) extends ForcedAction with Soft
case class ActionKeyFromAction(self : WarriorFaction, d : ActionCard, to : Clearing, l : $[Clearing], c : Clearing) extends BaseAction(ActionKey, "takes a warrior from")(c) with Soft
case class ActionKeyAction(self : WarriorFaction, d : ActionCard, to : Clearing, l : $[Clearing]) extends BaseAction(ActionKey)("Place", l.num.hl, (l.num == 1).?("warrior").|("warriors"), "in", to)
case class ActionSwordHitAction(self : Faction, d : ActionCard, then : ForcedAction) extends BaseAction(self, "in battle")("Deal an extra", 1.hit, "with", ActionSword)
case class SecretScoreAction(self : Faction, d : ActionCard, x : Option[Dominance]) extends BaseAction(Birdsong, "start")(SecretScore, "for", 3.vp, x.any.?("with").|("with three crafted improvements"), x)


object ActionDeckExpansion extends Expansion {
    def active(setup : $[Faction], options : $[Meta.O]) = options.has(ActionBooster)

    def held(f : Faction)(implicit game : Game) = f.hand.$.of[ActionCard]

    def holds(f : Faction, e : ActionEffect)(implicit game : Game) = held(f).exists(_.effect == e)

    def cardOf(f : Faction, e : ActionEffect)(implicit game : Game) = held(f).%(_.effect == e).starting

    def dominanceMet(f : Faction, d : Dominance)(implicit game : Game) : Boolean = d @@ {
        case CornersDominance(_) => board.diagonals.%((a, b) => f.rules(a) && f.rules(b)).any
        case BaseSuitDominance(s) => clearings.%(_.asset.matches(s)).%(f.rules).num >= 3
        case EnclaveDominance => factions.of[InvasiveEEE].exists(e => e.enclaves.%(game.riverside.has).%(f.rules).num >= 2)
        case FrogDominance => clearings.%(_.asset.matches(Frog)).use(l => l.%(f.rules).num > f.enemies.notOf[Hireling]./(e => l.%(e.rules).num).maxOr(0))
        case _ => false
    }

    def improvements(f : Faction)(implicit game : Game) = f.stuck.$.of[CraftEffectCard].num

    def crossbowTargets(f : Faction)(implicit game : Game) : $[(Faction, DeckCard)] =
        factions.but(f)./~(e => e.stuck.$.of[CraftEffectCard]./(x => (e, x)))

    def crossbowPayments(f : Faction, d : ActionCard, x : DeckCard)(implicit game : Game) = f.hand.but(d).%(_.matches(x.suit))

    def keyFrom(f : WarriorFaction, to : Clearing, l : $[Clearing])(implicit game : Game) =
        clearings.but(to).%(c => f.at(c).count(f.warrior) > l.count(c))

    def phased(f : Faction, p : Phase)(implicit game : Game, ask : ActionCollector) {
        if (game.current != f)
            return

        held(f).distinct.foreach { d =>
            + ActionCardMainAction(f, p, d)
        }
    }

    override def birdsong(f : Faction)(implicit game : Game, ask : ActionCollector) {
        if (game.current == f)
            cardOf(f, SecretScore).foreach { d =>
                if (improvements(f) >= 3)
                    + SecretScoreAction(f, d, None)

                (game.dominances.$ ++ f.hand.$.of[Dominance]).distinct.%(dominanceMet(f, _)).foreach { x =>
                    + SecretScoreAction(f, d, Some(x))
                }
            }

        phased(f, Birdsong)
    }

    override def daylight(f : Faction)(implicit game : Game, ask : ActionCollector) {
        phased(f, Daylight)
    }

    override def evening(f : Faction)(implicit game : Game, ask : ActionCollector) {
        phased(f, Evening)
    }

    def perform(action : Action, soft : Void)(implicit game : Game) = action @@ {
        // PAYMENT, once the move or battle is done
        case ActionPayAction(f, d, then) =>
            if (f.hand.has(d)) {
                f.hand --> d --> discard.quiet

                f.log("spent", d)
            }

            then

        case MoveListAction(self, f, t, m, from, to, l, ActionPayAction(ff, d, then)) =>
            ActionPayAction(ff, d, ForceAction(MoveListAction(self, f, t, m, from, to, l, then)))

        case MoveFinishedAction(f, from, to, ActionPayAction(ff, d, then)) =>
            ActionPayAction(ff, d, MoveFinishedAction(f, from, to, then))

        case CantMoveAction(self, f, t, m, from, to, ActionPayAction(_, _, then)) =>
            ForceAction(CantMoveAction(self, f, t, m, from, to, then))

        case BattleStartAction(self, f, a, m, c, o, i, ActionPayAction(ff, d, then)) =>
            ActionPayAction(ff, d, BattleStartAction(self, f, a, m, c, o, i, then))

        case BattleImpossibleAction(self, f, l, ActionPayAction(_, _, then)) =>
            ForceAction(BattleImpossibleAction(self, f, l, then))

        // MENU
        case ActionCardMainAction(f, p, d) =>
            val pay = ActionPayAction(f, d, Repeat)

            val ask = Ask(f).group(d.effect, Break ~ d.effect.text.hl)

            (d.effect match {
                case ActionHammer =>
                    ask.each(f.craft.distinct)(a => ActionHammerAction(f, d, a)).when(f.craft.none)(Info("No crafting pieces"))

                case ActionSword =>
                    val l = clearings.%(f.canAttackIn)
                    ask.add(BattleInitAction(f, f, WithEffect(ActionSword), l, $(CancelAction), pay).as("Battle".hl, dt.Battle).!(l.none, "no targets"))

                case ActionBag =>
                    ask.add(ActionBagAction(f, d).!(f.used.has(ActionBag), "already spent one"))

                case ActionTea =>
                    f.as[WarriorFaction] match {
                        case Some(f) =>
                            val l = clearings.%(f.present).%(f.canPlace)
                            ask.each(l)(c => ActionTeaAction(f, d, c).!(f.pool(f.warrior).not, "no warriors"))
                        case None =>
                            ask.add(Info("No warriors"))
                    }

                case ActionCoins =>
                    ask.add(ActionCoinsAction(f, d).!(deck.none && pile.none, "no cards"))

                case ActionBoots =>
                    val l = SquiresDeckExpansion.moveFromWith(f, SquiresDeckExpansion.defaultTransports(f))
                    ask.add(MoveInitAction(f, f, SquiresDeckExpansion.defaultTransports(f), WithEffect(ActionBoots), l, l, $(CancelAction), pay).as("Move".hl, dt.Move).!(l.none, "can't move"))

                case ActionCrossbow =>
                    val l = crossbowTargets(f)
                    ask.each(l)((e, x) => ActionCrossbowTargetAction(f, d, e, x).!(crossbowPayments(f, d, x).none, "no " + x.suit.name + " card"))
                        .when(l.none)(Info("No crafted cards"))

                case ActionKey =>
                    f.as[WarriorFaction] match {
                        case Some(f) =>
                            val l = clearings.%(f.canPlace)
                            ask.each(l)(c => ActionKeyDestinationAction(f, d, c).!(keyFrom(f, c, $).none, "no warriors elsewhere"))
                        case None =>
                            ask.add(Info("No warriors"))
                    }

                case SecretCancel | SecretScore =>
                    ask
            })
            .when(d.secret)(ActionReplaceAction(f, d).!(deck.none && pile.none, "no cards"))
            .cancel

        // SECRET! - REPLACE
        case ActionReplaceAction(f, d) =>
            f.hand --> d --> discard.quiet

            f.log("discarded", d, "to draw the top card of the deck")

            DrawCardsAction(f, 1, WithEffect(d.effect), AddCardsAction(f, Repeat))

        // SECRET! - SCORE
        case SecretScoreAction(f, d, x) =>
            f.hand --> d --> discard.quiet

            x.foreach { x =>
                if (f.hand.has(x))
                    f.hand --> x --> game.outOfGameCards
                else
                if (game.dominances.has(x))
                    game.dominances --> x --> game.outOfGameCards
            }

            f.oscore(3)("with", SecretScore)

            x.foreach { x =>
                f.log("removed", x, "from the game")
            }

            Repeat

        // HAMMER
        case ActionHammerAction(f, d, a) =>
            f.hand --> d --> discard.quiet

            f.extraCraft :+= a

            f.log("spent", d, "to activate", dt.CraftSuit(a), "again")

            Repeat

        // BAG
        case ActionBagAction(f, d) =>
            f.hand --> d --> discard.quiet

            f.used :+= ActionBag

            f.log("spent", d, "to keep", 2.cards, "more at the hand limit this turn")

            Repeat

        // TEA
        case ActionTeaAction(f, d, c) =>
            f.hand --> d --> discard.quiet

            val n = min(2, f.pooled(f.warrior))

            n.times(f.warrior).foreach { w =>
                f.reserve --> w --> c
            }

            f.log("spent", d, "and placed", n.times(f.warrior)./(_.of(f)).comma, "in", c)

            Repeat

        // COINS
        case ActionCoinsAction(f, d) =>
            f.hand --> d --> discard.quiet

            f.log("spent", d)

            DrawCardsAction(f, 3, WithEffect(ActionCoins), AddCardsAction(f, ActionCoinsKeepAction(f, Repeat)))

        case ActionCoinsKeepAction(f, then) =>
            val n = min(2, f.hand.num)

            if (n == 0)
                then
            else
                XXSelectObjectsAction(f, f.hand)
                    .withGroup(ActionCoins.elem ~ " discards " ~ n.cards)
                    .withRule(_.num(n))
                    .withThen(l => ActionCoinsDiscardAction(f, l, then))(l => "Discard".hl ~ l./(" " ~ _.elem))
                    .withExtra($(NoHand))

        case ActionCoinsDiscardAction(f, l, then) =>
            f.hand --> l --> discard.quiet

            f.log("discarded", l)

            then

        // CROSSBOW
        case ActionCrossbowTargetAction(f, d, e, x) =>
            Ask(f).group(ActionCrossbow, "spends a", x.suit.elem, "card to discard", x, "of", e)
                .each(crossbowPayments(f, d, x))(p => ActionCrossbowAction(f, d, e, x, p))
                .cancel

        case ActionCrossbowAction(f, d, e, x, p) =>
            f.hand --> d --> discard.quiet
            f.hand --> p --> discard.quiet

            x @@ {
                case CraftEffectCard(_, _, _, effect) =>
                    e.removeStuckEffect(effect)
                case _ =>
            }

            f.log("spent", d, "and", p, "to discard", x, "of", e)

            Repeat

        // KEY
        case ActionKeyDestinationAction(f, d, to) =>
            ActionKeyPickAction(f, d, to, $)

        case ActionKeyPickAction(f, d, to, l) =>
            Ask(f).group(ActionKey, "takes up to", 3.hl, "warriors to", to)
                .each(keyFrom(f, to, l))(c => ActionKeyFromAction(f, d, to, l, c).!(l.num >= 3))
                .when(l.any)(ActionKeyAction(f, d, to, l))
                .cancel

        case ActionKeyFromAction(f, d, to, l, c) =>
            if (l.num + 1 >= 3)
                Ask(f).group(ActionKey).add(ActionKeyAction(f, d, to, l :+ c)).cancel
            else
                ActionKeyPickAction(f, d, to, l :+ c)

        case ActionKeyAction(f, d, to, l) =>
            f.hand --> d --> discard.quiet

            l.foreach { c =>
                f.from(c) --> f.warrior --> to
            }

            f.log("spent", d, "and moved", l./(c => f.warrior.of(f) ~ " from " ~ c.elem).comma, "to", to)

            Repeat

        // SWORD, in battle
        case ActionSwordHitAction(f, d, then) =>
            f.hand --> d --> discard.quiet

            f.log("spent", d, "to deal an extra", 1.hit)

            then

        case _ => UnknownContinue
    }
}
