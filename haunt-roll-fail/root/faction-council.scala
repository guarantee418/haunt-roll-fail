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

// Twilight Council, as published in Root: Homeland (Law of Root, section 17)

trait Council extends WarriorFaction with CommonLegal {
    val clashKey = TCvA

    val warrior = CouncilBat

    def abilities(options : $[Meta.O]) = $(CouncilGovernors, CouncilEntreating, CouncilPeacekeepers, EveningCraft)

    def pieces(options : $[Meta.O]) = CouncilBat *** 20 ++ ClosedAssembly *** 6 ++ GoverningAssembly *** 6

    override def note : Elem = HorizontalBreak ~ "Homeland"

    def advertising = ClosedAssembly.img(this) ~ GoverningAssembly.img(this) ~ ClosedAssembly.img(this) ~ GoverningAssembly.img(this) ~ ClosedAssembly.img(this)

    def motto = "Govern".styled(this)
}

case object TC extends Council {
    val name = "Twilight Council"
    override def funName = NameReference(name, this) ~ " Council"
    val short = "TC"
    val style = "TC"
    val priority = "S"
}

case object CouncilBat extends Warrior with CommonBatWarrior {
    override def id = "Bat"
    override def name = "Bat"
}

trait CouncilAssembly extends Token with CommonBatToken

case object ClosedAssembly extends CouncilAssembly {
    override def id = "Assembly"
    override def name = "Closed Assembly"
}

case object GoverningAssembly extends CouncilAssembly {
    override def id = "Convened"
    override def name = "Governing Assembly"
}

case object Loyalists extends SpecialRegion {
    def of(f : Faction, n : Int) = (n == 1).?("Loyalist").|("Loyalists").styled(f)
}

case object CouncilGovernors extends FactionEffect {
    val name = "Governors"
}

case object CouncilEntreating extends FactionEffect {
    val name = "Entreating"
}

case object CouncilPeacekeepers extends FactionEffect {
    val name = "Peacekeepers"
}

case object Banishing extends BattleEffect with DisplayEffect {
    val name = "Banish"
}


object Council {
    val maxLoyalists = 4
    val maxAssemblies = 6

    // Governors (17.2.3): an enemy of a Council cannot place, remove, flip or craft at its Governing assemblies, except in battle
    def governs(f : Faction, c : Clearing)(implicit game : Game) : Boolean =
        factions.but(f).of[Council].%(game.states.contains).exists(t => t.at(c).has(GoverningAssembly) && t.friends(f).not)

    def governedFor(f : Faction, l : $[Clearing])(implicit game : Game) : $[Clearing] = l.%!(c => governs(f, c))

    // For bots: entreat only where the faction has something to do, that is, it has pieces there or rules the clearing
    def entreatNeeded(f : Faction, c : Clearing)(implicit game : Game) : Boolean =
        (f.present(c) || f.rules(c)) && game.scorched.has(c).not && game.flooded.has(c).not
}


class CouncilPlayer(val faction : Council)(implicit val game : Game) extends FactionState {
    val revealed = cards("revealed")

    val loyalists = location(Loyalists, u => u.faction == faction && u.piece == faction.warrior)

    def assemblies : $[Clearing] = clearings.%(c => at(c).of[CouncilAssembly].any)

    def placedAssemblies = all(ClosedAssembly).num + all(GoverningAssembly).num

    def craft = assemblies./(_.asset)
}


case class CouncilRevealMainAction(f : Council) extends ForcedAction with Soft
case class CouncilRevealAction(self : Council, d : DeckCard) extends BaseAction("Reveal a card to act")(d.img) with ViewCard with Soft
case class CouncilRevealedAction(f : Council, d : DeckCard, spend : Boolean, then : ForcedAction) extends ForcedAction
case class CouncilRevealCardAction(f : Council, d : DeckCard, then : ForcedAction) extends ForcedAction

case class CouncilRecruitAction(self : Council, d : DeckCard, c : Clearing) extends ForcedAction
case class CouncilBattleAction(self : Council, d : DeckCard, c : Clearing) extends ForcedAction with Soft
case class CouncilAssembleAction(self : Council, d : DeckCard, c : Clearing) extends ForcedAction
case class CouncilPlaceLoyalistsAction(self : Council, c : Clearing, n : Int, then : ForcedAction) extends ForcedAction
case class CouncilAssembleDoneAction(self : Council, d : DeckCard, c : Clearing) extends ForcedAction

case class CouncilConveneMainAction(f : Council) extends ForcedAction with Soft
case class CouncilConveneCardAction(self : Council, d : DeckCard) extends BaseAction("Convene Woodfolk", "return a card")(d.img) with ViewCard
case class CouncilBanishAction(self : Council, c : Clearing, e : Faction) extends ForcedAction
case class CouncilAgitateAction(self : Council, d : DeckCard, c : Clearing) extends ForcedAction
case class CouncilEmpowerAction(self : Council, c : Clearing) extends ForcedAction
case class CouncilEmpowerRolledAction(self : Council, c : Clearing, n : Int) extends RolledAction[Int] { def rolled = $(n) }
case class CouncilEmpowerLoyalistsAction(self : Council, n : Int) extends ForcedAction
case class CouncilEmpowerScoreAction(self : Council, c : Clearing) extends ForcedAction
case class CouncilBanishDestinationAction(self : Council, b : Battle, e : Faction, l : $[Figure], t : Clearing, then : ForcedAction) extends ForcedAction

case class CouncilInspireAction(f : Council) extends ForcedAction
case class CouncilAdjournMainAction(f : Council) extends ForcedAction with Soft
case class CouncilAdjournRemoveAction(self : Council, c : Clearing) extends ForcedAction
case class CouncilAdjournFlipAction(f : Council) extends ForcedAction
case class CouncilOverseeAction(f : Council) extends ForcedAction

case class CouncilSetupSecondAction(f : Council, start : Clearing) extends ForcedAction
case class CouncilSetupPlaceAction(f : Council, c : Clearing, n : Int, then : ForcedAction) extends ForcedAction

case class CouncilEntreatMainAction(self : Faction, t : Council, then : ForcedAction) extends BaseAction(None)("Entreat".styled(t), t) with Soft
case class CouncilEntreatAction(self : Faction, t : Council, c : Clearing, then : ForcedAction) extends ForcedAction
case class CouncilEntreatGainAction(self : Council, then : ForcedAction) extends ForcedAction

case class CouncilRemoveLoyalistAction(self : Council, then : ForcedAction) extends ForcedAction


object CouncilExpansion extends FactionExpansion[Council] {
    def toLoyalists(f : Council, n : Int)(implicit game : Game) : Int = {
        val k = min(n, min(Council.maxLoyalists - f.loyalists.$.num, f.pooled(f.warrior)))

        if (k > 0)
            f.reserve --> k.times(f.warrior) --> f.loyalists

        k
    }

    def flip(f : Council, c : Clearing, from : CouncilAssembly, to : CouncilAssembly)(implicit game : Game) {
        if (f.at(c).has(from)) {
            f.from(c) --> from --> f.reserve
            f.reserve --> to --> c
        }
    }

    // Entreat (17.2.4): offered to enemies during their turn
    def entreat(f : Faction)(implicit game : Game, ask : ActionCollector) {
        factions.but(f).of[Council].%(game.states.contains).%(_.friends(f).not).foreach { t =>
            if (t.all(GoverningAssembly).any)
                + CouncilEntreatMainAction(f, t, Repeat)
        }
    }

    override def birdsong(f : Faction)(implicit game : Game, ask : ActionCollector) = entreat(f)
    override def daylight(f : Faction)(implicit game : Game, ask : ActionCollector) = entreat(f)
    override def evening(f : Faction)(implicit game : Game, ask : ActionCollector) = entreat(f)

    def perform(action : Action, soft : Void)(implicit game : Game) = action @@ {
        // SETUP
        case CreatePlayerAction(f : Council) =>
            game.states += f -> new CouncilPlayer(f)

            FactionInitAction(f)

        case FactionSetupAction(f : Council) if options.has(SetupTypeHomelands) =>
            val l = clearings.diff(game.homelands).%(f.canPlace)

            Ask(f).each(l)(c => StartingClearingAction(f, c).as(c)(f, "starts in")).needOk

        case FactionSetupAction(f : Council) =>
            StartingCornerAction(f)

        case StartingClearingAction(f : Council, c) =>
            if (options.has(SetupTypeHomelands))
                game.homelands :+= c

            f.reserve --> 4.times(f.warrior) --> c

            f.log("placed", 4.hl, f.warrior.sof(f), "in", c)

            CouncilSetupSecondAction(f, c)

        case CouncilSetupSecondAction(f, start) =>
            Ask(f).group(f, "places", 2.hl, f.warrior.sof(f), "in")
                .each(clearings.but(start).%(f.canPlace))(c => CouncilSetupPlaceAction(f, c, 2, SetupFactionsAction).as(c))
                .needOk

        case CouncilSetupPlaceAction(f, c, n, then) =>
            f.reserve --> n.times(f.warrior) --> c

            f.log("placed", n.hl, f.warrior.sof(f), "in", c)

            then

        // HELPER
        case BattlePostHitInAction(b, e, f : Council, CouncilBat, then) =>
            e.log("grounded", CouncilBat.of(f))

            then

        case BattlePostHitInAction(b, e, f : Council, p : CouncilAssembly, then) =>
            e.log("dispersed", p.of(f))

            then

        // Assemblies (17.2.2): when an enemy removes an assembly, also remove 1 Loyalist
        case ForcedRemoveTargetEffectAction(e, c, f : Council, p : CouncilAssembly, then) if e != f =>
            CouncilRemoveLoyalistAction(f, then)

        case CouncilRemoveLoyalistAction(f, then) =>
            if (f.loyalists.$.any) {
                f.loyalists --> f.warrior --> f.reserve

                f.log("lost a", Loyalists.of(f, 1))
            }

            then

        // Governors: no removal outside battle at Governing assemblies
        case NukeAction(e, affects, l, nuke, then) if l.exists(c => Council.governs(e, c)) =>
            val g = l.%(c => Council.governs(e, c))

            g.foreach(c => e.log("could not remove pieces in", c, "due to", CouncilGovernors.of(factions.of[Council].%(_.at(c).has(GoverningAssembly)).first)))

            NukeAction(e, affects, l.diff(g), nuke, then)

        // Entreat
        case CouncilEntreatMainAction(f, t, then) =>
            Ask(f).group("Entreat".styled(t), "to close an", GoverningAssembly.of(t))
                .each(t.all(GoverningAssembly))(c => CouncilEntreatAction(f, t, c, then).as(c))
                .cancel

        case CouncilEntreatAction(f, t, c, then) =>
            flip(t, c, GoverningAssembly, ClosedAssembly)

            f.log("entreated", t, "to close the assembly in", c)

            val l = t.loyalists.$.num
            val gain = t.loyalists.$.num < Council.maxLoyalists && t.pool(t.warrior)

            Ask(t).group(t, "was entreated in", c)
                .add(CouncilEntreatGainAction(t, then).as("Gain", 1.hl, Loyalists.of(t, 1)).!(gain.not))
                .each(1.to(l).toList.reverse)(n => CouncilPlaceLoyalistsAction(t, c, n, then).as("Place", n.hl, Loyalists.of(t, n), "in", c).!(t.canPlace(c).not))
                .skip(then)

        case CouncilEntreatGainAction(t, then) =>
            if (toLoyalists(t, 1) > 0)
                t.log("gained a", Loyalists.of(t, 1))

            then

        case CouncilPlaceLoyalistsAction(f, c, n, then) =>
            f.loyalists --> n.times(f.warrior) --> c

            f.log("placed", n.hl, Loyalists.of(f, n), "in", c)

            then

        // Peacekeepers (17.2.5): Council warriors join the defense at an assembly
        case BattleDefenderPreRollAction(b) if b.codef.none && b.defender.is[Hero].not && b.defender.is[Council].not && b.attacker.is[Council].not &&
            factions.of[Council].%(t => t != b.attacker && t != b.defender && t.friends(b.attacker).not && t.friends(b.defender).not)
                .exists(t => t.at(b.clearing).of[CouncilAssembly].any && t.at(b.clearing).of[Warrior].any) =>
            val t = factions.of[Council].%(t => t.at(b.clearing).of[CouncilAssembly].any && t.at(b.clearing).of[Warrior].any).first

            t.log("joined the defense of", b.defender, "in", b.clearing, "as", CouncilPeacekeepers.of(t))

            BattleDefenderPreRollAction(b.copy(codef = Some(t)))

        // Banish (17.6.1.I): ignore rolled hits taken
        case BattleFinishRollAction(b, f : Council, o, fs, os, fr, or, fh, oh) if oh > 0 && f.used.has(Banishing) =>
            f.log("ignored rolled", "Hits".styled(styles.hit), "while banishing")

            BattleFinishRollAction(b, f, o, fs, os, fr, or, fh, 0)

        // Banish: defending warriors move instead of being removed; buildings and tokens cannot be hit
        case BattleAssignHitsAction(o, b, n, s, then) if n + s > 0 && o == b.defender && b.attacker.as[Council].?(_.used.has(Banishing)) =>
            val t = b.attacker.as[Council].get
            val l = o.from(b.clearing).$.%(_.piece.is[Warrior]).%(_.piece != Warlord).sortBy(_.piece.priority).take(n + s)
            val dest = game.connected(b.clearing)

            if (l.none || dest.none) {
                o.log("had no warriors to banish")

                then
            }
            else
                Ask(t).group("Banish", l./(_.elem).comma, "from", b.clearing, "to")
                    .each(dest)(d => CouncilBanishDestinationAction(t, b, o, l, d, then).as(d))
                    .needOk

        case CouncilBanishDestinationAction(t, b, o, l, d, then) =>
            game.highlights :+= MoveHighlight(b.clearing, d)

            o.from(b.clearing) --> l./(_.piece) --> d

            t.log("banished", l./(_.elem).comma, "from", b.clearing, "to", d)

            // Banished warriors are forced to move, treated as a single move by their owner for other effects (Outrage)
            MoveCompleteAction(o, o, b.clearing, d, l./(_.piece), $, then)

        // BIRDSONG
        case BirdsongNAction(30, f : Council) =>
            CouncilRevealMainAction(f)

        case CouncilRevealMainAction(f) =>
            Ask(f)
                .each(f.hand)(d => CouncilRevealAction(f, d))
                .done(Next)
                .birdsong(f)

        case CouncilRevealAction(f, d) =>
            def matching(c : Clearing) = c.cost.matched(d.suit)

            val mv = f.moveFrom.of[Clearing].%(matching)
            val rc = clearings.%(matching).%(f.canPlace)
            val bt = clearings.%(matching).%(f.canAttackIn)
            val asm = clearings.%(matching).%(c => f.at(c).of[CouncilAssembly].none).%(f.canPlace)

            Ask(f).group("Reveal", d, "to")
                .add(MoveInitAction(f, f, $, NoMessage, mv, f.movable, $(CancelAction), CouncilRevealedAction(f, d, false, Repeat)).as("Move".styled(f), "from", mv.comma).!(mv.none))
                .each(rc)(c => CouncilRecruitAction(f, d, c).as("Recruit".styled(f), f.warrior.imgd(f), "in", c).!(f.pool(f.warrior).not))
                .each(bt)(c => CouncilBattleAction(f, d, c).as("Battle".styled(f), "in", c, f.at(c).of[CouncilAssembly].any.?("(discard)")))
                .each(asm)(c => CouncilAssembleAction(f, d, c).as("Assemble".styled(f), ClosedAssembly.imgd(f), "in", c).!(f.placedAssemblies >= Council.maxAssemblies))
                .cancel

        // Revealed cards go into the play area before the action and cannot be used for any other purposes during Birdsong,
        // so move the card out of the hand once a move or battle is committed (not in the soft steps before it)
        case MoveListAction(self, f : Council, t, m, from, to, l, then @ CouncilRevealedAction(_, d, _, _)) if f.hand.has(d) =>
            CouncilRevealCardAction(f, d, ForceAction(MoveListAction(self, f, t, m, from, to, l, then)))

        case MoveListAlliedAction(self, f : Council, t, m, from, to, l, a, al, then @ CouncilRevealedAction(_, d, _, _)) if f.hand.has(d) =>
            CouncilRevealCardAction(f, d, ForceAction(MoveListAlliedAction(self, f, t, m, from, to, l, a, al, then)))

        case BattleStartAction(self, f : Council, a, m, c, o, i, then @ CouncilRevealedAction(_, d, _, _)) if f.hand.has(d) =>
            CouncilRevealCardAction(f, d, BattleStartAction(self, f, a, m, c, o, i, then))

        case CouncilRevealCardAction(f, d, then) =>
            if (f.hand.has(d)) {
                f.hand --> d --> f.revealed

                f.log("revealed", d)
            }

            then

        case CouncilRevealedAction(f, d, spend, then) =>
            if (spend && f.hand.has(d)) {
                f.hand --> d --> discard.quiet

                f.log("discarded", d)
            }
            else
            if (spend && f.revealed.has(d)) {
                f.revealed --> d --> discard.quiet

                f.log("discarded", d)
            }
            else
            if (spend.not && f.hand.has(d)) {
                f.hand --> d --> f.revealed

                f.log("revealed", d)
            }

            then

        case CouncilRecruitAction(f, d, c) =>
            game.highlights :+= PlaceHighlight($(c))

            f.reserve --> f.warrior --> c

            f.log("recruited in", c, "with", d)

            CouncilRevealedAction(f, d, false, Repeat)

        case CouncilBattleAction(f, d, c) =>
            val spend = f.at(c).of[CouncilAssembly].any

            BattleInitAction(f, f, NoMessage, $(c), $(CancelAction), CouncilRevealedAction(f, d, spend, Repeat))

        case CouncilAssembleAction(f, d, c) =>
            f.reserve --> ClosedAssembly --> c

            f.log("assembled in", c, "with", d)

            val n = f.loyalists.$.num

            if (n > 0)
                Ask(f).group("Place", Loyalists.of(f, 2), "in", c)
                    .each(1.to(n).toList.reverse)(k => CouncilPlaceLoyalistsAction(f, c, k, CouncilAssembleDoneAction(f, d, c)).as(k.hl, Loyalists.of(f, k)))
                    .skip(CouncilAssembleDoneAction(f, d, c))
            else
                CouncilAssembleDoneAction(f, d, c)

        case CouncilAssembleDoneAction(f, d, c) =>
            CouncilRevealedAction(f, d, f.rules(c).not, Repeat)

        // DAYLIGHT
        case DaylightNAction(30, f : Council) =>
            val l = f.all(GoverningAssembly).%(c => f.enemies.exists(_.rules(c)))

            l.foreach { c =>
                flip(f, c, GoverningAssembly, ClosedAssembly)

                f.log("went to", "Sleep".styled(f), "in", c)
            }

            Next

        case DaylightNAction(60, f : Council) =>
            soft()

            Ask(f).daylight(f).done(Next)

        // EVENING
        case EveningNAction(20, f : Council) =>
            CouncilConveneMainAction(f)

        case CouncilConveneMainAction(f) =>
            if (f.revealed.any)
                Ask(f).each(f.revealed.$)(d => CouncilConveneCardAction(f, d))
            else
                NoAsk(f)(Next)

        case CouncilConveneCardAction(f, d) =>
            f.revealed --> d --> f.hand

            f.log("returned", d, "to hand")

            val g = "Convene Woodfolk".styled(f) ~ " with " ~ d.elem

            if (d.suit == Bird) {
                val l = f.assemblies

                Ask(f).group(g)
                    .each(l)(c => CouncilEmpowerAction(f, c).as("Empower".styled(f), "in", c).!(f.at(c).of[Warrior].none, "no warriors"))
                    .skip(CouncilConveneMainAction(f))
            }
            else {
                val l = f.assemblies.%(_.cost.matched(d.suit))

                Ask(f).group(g)
                    .some(l)(c => f.enemies.%(_.present(c)).%(f.canAttack(c)).%(_.is[Hero].not)./(e => CouncilBanishAction(f, c, e).as("Banish".styled(f), e, "in", c)))
                    .each(l)(c => CouncilAgitateAction(f, d, c).as("Agitate".styled(f), "in", c, "(spend card)"))
                    .skip(CouncilConveneMainAction(f))
            }

        case CouncilBanishAction(f, c, e) =>
            f.used :+= Banishing

            BattleStartAction(f, f, f, WithEffect(Banishing), c, e, None, CouncilConveneMainAction(f))

        case CouncilAgitateAction(f, d, c) =>
            f.hand --> d --> discard.quiet

            f.log("agitated in", c, "with", d)

            if (toLoyalists(f, 1) > 0)
                f.log("gained a", Loyalists.of(f, 1))

            if (f.at(c).has(ClosedAssembly)) {
                flip(f, c, ClosedAssembly, GoverningAssembly)

                f.log("opened the assembly in", c)
            }

            CouncilConveneMainAction(f)

        case CouncilEmpowerAction(f, c) =>
            Roll[Int]($(D4), r => CouncilEmpowerRolledAction(f, c, r(0)), f.elem ~ " empowers in " ~ c.elem)

        case CouncilEmpowerRolledAction(f, c, r) =>
            val n = min(r, f.at(c).count(f.warrior))

            f.log("rolled", r.roll, "and removed", n.hl, f.warrior.nof(n)(f), "in", c)

            f.from(c) --> n.times(f.warrior) --> f.reserve

            val canScore = f.rules(c)

            Ask(f).group("Empower".styled(f), "in", c)
                .add(CouncilEmpowerLoyalistsAction(f, n).as("Place", n.hl, f.warrior.nof(n)(f), "in", Loyalists.of(f, 2)).!(n == 0))
                .add(CouncilEmpowerScoreAction(f, c).as("Score", 1.vp).!(canScore.not, "don't rule"))
                .skip(CouncilConveneMainAction(f))

        case CouncilEmpowerLoyalistsAction(f, n) =>
            val k = toLoyalists(f, n)

            f.log("gained", k.hl, Loyalists.of(f, k))

            CouncilConveneMainAction(f)

        case CouncilEmpowerScoreAction(f, c) =>
            f.oscore(1)("empowering in", c)

            CouncilConveneMainAction(f)

        case EveningNAction(40, f : Council) =>
            XCraftMainAction(f)

        // Craft or Inspire (17.6.2): if nothing was crafted, draw a card per card draw icon showing on the Assemblies track
        case EveningNAction(45, f : Council) =>
            CouncilInspireAction(f)

        case CouncilInspireAction(f) =>
            val n = f.placedAssemblies / 2

            if (f.crafted.none && n > 0) {
                f.log("was inspired")

                DrawCardsAction(f, n, NoMessage, AddCardsAction(f, Next))
            }
            else
                Next

        // Adjourn (17.6.3)
        case EveningNAction(50, f : Council) =>
            CouncilAdjournMainAction(f)

        case CouncilAdjournMainAction(f) =>
            Ask(f).group("Adjourn".styled(f), "remove assemblies")
                .each(f.assemblies)(c => CouncilAdjournRemoveAction(f, c).as(f.at(c).of[CouncilAssembly]./(_.imgd(f)).merge, "in", c))
                .done(CouncilAdjournFlipAction(f))
                .evening(f)

        case CouncilAdjournRemoveAction(f, c) =>
            f.at(c).of[CouncilAssembly].foreach(p => f.from(c) --> p --> f.reserve)

            f.log("adjourned the assembly in", c)

            CouncilAdjournMainAction(f)

        case CouncilAdjournFlipAction(f) =>
            f.all(ClosedAssembly).%(f.rules).foreach { c =>
                flip(f, c, ClosedAssembly, GoverningAssembly)

                f.log("began governing in", c)
            }

            Next

        // Oversee (17.6.4)
        case EveningNAction(60, f : Council) =>
            CouncilOverseeAction(f)

        case CouncilOverseeAction(f) =>
            val l = f.all(GoverningAssembly).%(c => f.enemies.exists(e => e.at(c).exists(p => p.is[Building] || p.is[Token])))

            val n = l.num match {
                case 0 => 0
                case 1 => 1
                case 2 | 3 => 2
                case 4 => 3
                case _ => 4
            }

            if (n > 0)
                f.oscore(n)("overseeing", l./(c => c.elem : Elem).comma)

            Next

        case NightStartAction(f : Council) =>
            EveningDrawAction(f, 1)

        case FactionCleanUpAction(f : Council) =>
            CleanUpAction(f)

        case _ => UnknownContinue
    }

}
