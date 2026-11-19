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

// United Dove Corps ii (MarcustheCat, from L'Arbre's United Dove Corps, art by SuPaCatOnE; from its faction board and
// Advanced Setup card in the Root Ultimate Collection TTS mod, as there is no rulebook): doves protecting the woodland's
// inhabitants, declaring ceasefires, turning them into peace and scoring the refugees they rescue.
//
// Readings (the board leaves these open):
// - Tokens: six two-sided tokens, ceasefire on one side and peace on the other; a clearing holds at most one of them
// - Civilian Safety: on another player's own turn (not a friend's), it can't start a battle in a clearing with a ceasefire token,
//   and nothing it does removes the Corps' pieces there
// - Uphold the Peace: the Corps can't start a battle in a clearing with its peace token; a peace token removed by anyone discards one
//   refugee card at random
// - Hostage Rescue: each enemy building or token the Corps' hits remove in a battle, attacking or defending, draws a card into
//   the refugees
// - Refugees: a pile of cards only the Corps sees; a bird card matches any clearing
// - Resettle Refugees: every refugee card matching a clearing with a peace token is discarded, and the number of them scores (1, 3, 5, 7)
// - Expedition: a move, then a battle in its destination if the Corps wants to and can
// - Resupply: a warrior in each clearing with a ceasefire token; with too few warriors, the Corps picks them one at a time
// - Ransom Hostages: the player picked must have a card; it picks which card it gives
// - Build: the cards, all matching the clearing, are as many as the cost of the leftmost building on that track (1, then 2); placing
//   it scores what it uncovers (1, then 2), and the school spaces show a draw each
// - Evacuate Refugees: each peace token and each clinic lets the Corps add a card from its hand matching that clearing
// - Setup: six warriors and the ceasefire token in the board's inner clearings, the token with the first warrior placed; with
//   Advanced Setup, its card: a homeland not adjacent to another, three warriors and the ceasefire there, three warriors in the
//   clearings next to it as evenly as possible

case object Dove extends Warrior {
    override def id = "dove"
    override def name = "Dove"
}

case object DoveClinic extends Building {
    override def id = "clinic"
    override def name = "Clinic"
}

case object DoveSchool extends Building {
    override def id = "school"
    override def name = "School"
}

case object Ceasefire extends Token {
    override def id = "ceasefire"
    override def name = "Ceasefire"
}

case object PeaceToken extends Token {
    override def id = "peace"
    override def name = "Peace"
}

case object CivilianSafety extends FactionEffect {
    val name = "Civilian Safety"
}

case object UpholdThePeace extends FactionEffect {
    val name = "Uphold the Peace"
}

case object HostageRescue extends FactionEffect {
    val name = "Hostage Rescue"
}

case object Refugees extends FactionEffect {
    val name = "Refugees"
}

trait UnitedDoveCorps extends WarriorFaction {
    val clashKey = UDC

    val warrior = Dove

    def abilities(options : $[Meta.O]) = $(CivilianSafety, UpholdThePeace, HostageRescue)

    def pieces(options : $[Meta.O]) = Dove *** 15 ++ DoveClinic *** 2 ++ DoveSchool *** 2 ++ Ceasefire *** 6 ++ PeaceToken *** 6

    override def note : Elem = SpacedDash ~ "by " ~ "MarcustheCat".hl ~ ", from " ~ "L'Arbre".hl ~ HorizontalBreak ~ "fan-made"

    def advertising = Dove.img(this) ~ Ceasefire.img(this) ~ PeaceToken.img(this)

    def motto = "Peace".styled(this)

    override def initialDamagePriorities(options : $[Meta.O]) = $(Dove, Ceasefire, PeaceToken, DoveSchool, DoveClinic)
}

case object UDC extends UnitedDoveCorps {
    val name = "United Dove Corps"
    val short = "UDC"
    val style = "udc"
    val priority = "V''''''''"
}

object UnitedDoveCorps {
    // Resettle Refugees: the points for the cards discarded
    def resettle(n : Int) = n @@ {
        case 0 => 0
        case 1 => 1
        case 2 => 3
        case 3 => 5
        case _ => 7
    }

    val emergency = 1
    val protection = 2
    val stabilization = 3

    def mandate(f : UnitedDoveCorps, m : Int) = m @@ {
        case 1 => "Emergency Intervention".styled(f)
        case 2 => "Civilian Protection".styled(f)
        case 3 => "Stabilization".styled(f)
        case _ => "No Mandate".hl
    }
}

trait UDCDaylightQuestion extends FactionAction {
    override def self : UnitedDoveCorps

    def question(implicit game : Game) = self.elem ~ SpacedDash ~ UnitedDoveCorps.mandate(self, self.mandate) ~ Break ~
        Div(
            1.to(3)./(_ => Image("action-black", styles.action, "")).take(self.acted) ~
            1.to(3)./(_ => Image(self.style + "-action", styles.action)).drop(self.acted),
            styles.margined)
}

case class UDCSetupAction(self : UnitedDoveCorps, c : Clearing) extends BaseAction(self, "places", Ceasefire.of(self), "and", Dove.of(self), "in")(c)
case class UDCSetupWarriorAction(self : UnitedDoveCorps, c : Clearing, n : Int) extends BaseAction(self, "places", Dove.of(self), "(" ~ n.hl ~ "/" ~ 6.hl ~ ")", "in")(c)
case class UDCSetupNextAction(self : UnitedDoveCorps) extends ForcedAction
case class UDCSetupHomelandAction(self : UnitedDoveCorps, c : Clearing, l : $[Clearing]) extends BaseAction(self, "starts with", Ceasefire.of(self), "in")(c)

case class UDCResettleAction(self : UnitedDoveCorps) extends ForcedAction

case class UDCMandateAction(self : UnitedDoveCorps, m : Int) extends BaseAction(self, "chooses a", "Mandate".hl)(UnitedDoveCorps.mandate(self, m))

case class UDCDeclarePeaceAction(self : UnitedDoveCorps, c : Clearing) extends BaseAction("Declare Peace".styled(self), "(" ~ 1.vp ~ ")")(c)

case class UDCRecruitAction(self : UnitedDoveCorps, l : $[Clearing]) extends OptionAction("Recruit".styled(self), Dove.imgd(self)) with UDCDaylightQuestion with Soft
case class UDCRecruitClearingAction(self : UnitedDoveCorps, c : Clearing) extends BaseAction("Recruit".styled(self), "in")(c)
case class UDCExpeditionAction(self : UnitedDoveCorps) extends OptionAction("Expedition".styled(self), dt.Move, dt.Battle) with UDCDaylightQuestion with Soft with Only[MoveListAction] { def tag = implicitly }
case class UDCExpeditionMovedAction(self : UnitedDoveCorps) extends ForcedAction
case class UDCMoveAction(self : UnitedDoveCorps) extends OptionAction("Move".styled(self), dt.Move) with UDCDaylightQuestion with Soft with Only[MoveListAction] { def tag = implicitly }
case class UDCCeasefireAction(self : UnitedDoveCorps, l : $[Clearing]) extends OptionAction("Declare Ceasefire".styled(self), Ceasefire.imgd(self)) with UDCDaylightQuestion with Soft
case class UDCCeasefireClearingAction(self : UnitedDoveCorps, c : Clearing) extends BaseAction("Declare Ceasefire".styled(self), "in")(c)
case class UDCRansomAction(self : UnitedDoveCorps, l : $[Faction]) extends OptionAction("Ransom Hostages".styled(self), dt.CardBack) with UDCDaylightQuestion with Soft
case class UDCRansomPlayerAction(self : UnitedDoveCorps, e : Faction) extends BaseAction("Ransom Hostages".styled(self), "from")(e)
case class UDCRansomGiveAction(self : Faction, t : UnitedDoveCorps, d : DeckCard) extends BaseAction("Give", t, "a card for", "Ransom Hostages".styled(t), "(" ~ 1.vp ~ ")")(d.img) with ViewCard
case class UDCBuildAction(self : UnitedDoveCorps, p : Building, l : $[Clearing]) extends OptionAction("Build".styled(self), p.of(self), p.imgd(self)) with UDCDaylightQuestion with Soft
case class UDCBuildClearingAction(self : UnitedDoveCorps, p : Building, c : Clearing) extends BaseAction("Build", p.of(self), "in")(c) with Soft
case class UDCBuildCardsAction(self : UnitedDoveCorps, p : Building, c : Clearing, l : $[DeckCard]) extends BaseAction("Build", p.of(self), "in", c, "with")(l./(_.img).merge)
case class UDCRepatriateAction(self : UnitedDoveCorps) extends OptionAction("Repatriate Refugees".styled(self), dt.CardBack) with UDCDaylightQuestion with Soft
case class UDCRepatriateCardAction(self : UnitedDoveCorps, d : DeckCard) extends BaseAction("Repatriate Refugees".styled(self), "to hand")(d.img) with ViewCard
case class UDCDoneAction(f : UnitedDoveCorps, then : ForcedAction) extends ForcedAction

case class UDCResupplyAction(self : UnitedDoveCorps) extends ForcedAction
case class UDCResupplyPickAction(self : UnitedDoveCorps, c : Clearing) extends BaseAction("Resupply".styled(self), Dove.of(self), "in")(c)

case class UDCEvacuateAction(self : UnitedDoveCorps) extends ForcedAction
case class UDCEvacuateCardAction(self : UnitedDoveCorps, c : Clearing, d : DeckCard) extends BaseAction("Evacuate Refugees".styled(self), "from", c)(d.img) with ViewCard

case class UDCRescueAction(self : UnitedDoveCorps, then : ForcedAction) extends ForcedAction
case class UDCLoseRefugeeAction(self : UnitedDoveCorps, random : DeckCard, then : ForcedAction) extends RandomAction[DeckCard]


class UnitedDoveCorpsPlayer(val faction : UnitedDoveCorps)(implicit val game : Game) extends FactionState {
    val refugees = cards("refugees")

    var mandate = 0
    var acted = 0
    var ransomed = false
    // the last destination of a move, for an Expedition's battle
    var dest : |[Clearing] = None
    // the clearings left to evacuate refugees from this Evening, one for each peace token and clinic
    var evac : $[Clearing] = $
    var setupLeft = 0
    var resupplied : $[Clearing] = $

    def ceasefires = all(Ceasefire)

    def peace = all(PeaceToken)

    def tokensLeft = 6 - ceasefires.num - peace.num

    def craft = (ceasefires ++ peace ++ all(DoveSchool))./(_.asset)

    // the leftmost building on its track: its cost and the points it uncovers
    def next(p : Building) = 3 - pooled(p)

    def draws = 1 + all(DoveSchool).num
}


object UnitedDoveCorpsExpansion extends FactionExpansion[UnitedDoveCorps] {
    def corps(implicit game : Game) = factions.of[UnitedDoveCorps].%(game.states.contains)

    // Civilian Safety and Uphold the Peace: f may start a battle in c
    def battleAllowed(f : Faction, c : Clearing)(implicit game : Game) : Boolean = corps.none || (f @@ {
        case f : UnitedDoveCorps if game.states.contains(f) && f.at(c).has(PeaceToken) => false
        case _ => true
    }) && (game.current != f || corps.%(_ != f).%(_.friends(f).not).forall(_.at(c).has(Ceasefire).not))

    // Civilian Safety: on its own turn, f can't remove e's pieces in a ceasefire clearing
    def protectedFrom(f : Faction, c : Clearing, e : Faction)(implicit game : Game) = e @@ {
        case e : UnitedDoveCorps => game.states.contains(e) && f != e && game.current == f && e.friends(f).not && e.at(c).has(Ceasefire)
        case _ => false
    }

    def ceasefirable(f : UnitedDoveCorps)(implicit game : Game) =
        clearings.%(f.rules).%(c => f.at(c).of[Token].none).%(c => f.enemies.exists(_.at(c).of[Warrior].any))

    def ransomable(f : UnitedDoveCorps)(implicit game : Game) =
        factions.but(f).%(f.friends(_).not).%(_.hand.any).%(e => f.peace.exists(c => e.at(c).of[Warrior].any))

    def buildable(f : UnitedDoveCorps, p : Building)(implicit game : Game) =
        f.pool(p).??(clearings.%(f.rules).%(f.canBuild).%(c => f.at(c).has(p).not).%(c => f.hand.count(_.matches(c.cost)) >= f.next(p)))

    def evacuable(f : UnitedDoveCorps)(implicit game : Game) = f.evac.distinct.%(c => f.hand.exists(_.matches(c.cost)))

    def perform(action : Action, soft : Void)(implicit game : Game) = action @@ {
        // SETUP
        case CreatePlayerAction(f : UnitedDoveCorps) =>
            game.states += f -> new UnitedDoveCorpsPlayer(f)

            FactionInitAction(f)

        case FactionSetupAction(f : UnitedDoveCorps) if options.has(SetupTypeHomelands) =>
            val all = clearings.%(f.canPlace).diff(game.homelands)
            val l = all.%(c => game.connected(c).intersect(game.homelands).none).some.|(all)

            Ask(f).each(l) { c =>
                val n = game.connected(c).%(f.canPlace)
                val ll = n.any.??(0.until(3).$./(i => n(i % n.num)))
                UDCSetupHomelandAction(f, c, ll)
            }.needOk

        case UDCSetupHomelandAction(f, c, l) =>
            game.homelands :+= c

            f.reserve --> Ceasefire --> c
            f.reserve --> 3.times(Dove) --> c

            l.foreach(x => f.reserve --> Dove --> x)

            f.log("placed", Ceasefire.of(f), "and", 3.hl, Dove.sof(f), "in", c, l.any.?("and" ~ " " ~ Dove.sof(f) ~ " in " ~ l.distinct./(_.elem).comma))

            SetupFactionsAction

        case FactionSetupAction(f : UnitedDoveCorps) =>
            val l = board.inner.%(f.canPlace).some.|(clearings.%(f.canPlace))

            Ask(f).each(l)(c => UDCSetupAction(f, c)).needOk

        case UDCSetupAction(f, c) =>
            game.homelands :+= c

            f.reserve --> Ceasefire --> c
            f.reserve --> Dove --> c

            f.log("placed", Ceasefire.of(f), "and", Dove.of(f), "in", c)

            f.setupLeft = 5

            UDCSetupNextAction(f)

        case UDCSetupWarriorAction(f, c, n) =>
            f.reserve --> Dove --> c

            f.log("placed", Dove.of(f), "in", c)

            f.setupLeft -= 1

            UDCSetupNextAction(f)

        case UDCSetupNextAction(f) =>
            if (f.setupLeft <= 0)
                SetupFactionsAction
            else {
                val l = board.inner.%(f.canPlace).some.|(clearings.%(f.canPlace))

                Ask(f).each(l)(c => UDCSetupWarriorAction(f, c, 7 - f.setupLeft))
            }

        // CIVILIAN SAFETY
        case ForcedRemoveAction(e, c, f : UnitedDoveCorps, p, n, m, then) if protectedFrom(e, c, f) =>
            f.log("kept", p.of(f), "in", c, "with", CivilianSafety.of(f))

            then

        // UPHOLD THE PEACE
        case ForcedRemoveTargetEffectAction(e, c, f : UnitedDoveCorps, PeaceToken, then) if f.refugees.any =>
            Random[DeckCard](f.refugees, r => UDCLoseRefugeeAction(f, r, then))

        case UDCLoseRefugeeAction(f, d, then) =>
            f.refugees --> d --> discard.quiet

            f.log("discarded", d, "from the", Refugees.of(f), "for", UpholdThePeace.of(f))

            then

        // HOSTAGE RESCUE
        case BattlePostHitOutAction(b, e : UnitedDoveCorps, f, p, then) if e != f && (p.is[Building] || p.is[Token]) && f.friends(e).not =>
            DrawCardsAction(e, 1, NoMessage, UDCRescueAction(e, then))

        case UDCRescueAction(f, then) =>
            f.drawn --> f.refugees

            f.log("added a card to the", Refugees.of(f), "with", HostageRescue.of(f))

            then

        // the last destination, for an Expedition's battle
        case MoveFinishedAction(f : UnitedDoveCorps, o, d : Clearing, then) if f.dest.has(d).not =>
            f.dest = Some(d)

            MoveFinishedAction(f, o, d, then)

        // BIRDSONG
        case BirdsongNAction(30, f : UnitedDoveCorps) =>
            UDCResettleAction(f)

        case UDCResettleAction(f) =>
            val l = f.refugees.%(d => f.peace.exists(c => d.matches(c.cost)))

            if (l.any) {
                f.refugees --> l --> discard.quiet

                f.nscore(UnitedDoveCorps.resettle(l.num))("resettling refugees")(f, "resettled", l.num.cards, "of", Refugees.of(f), ForVP)
            }

            Next

        // DAYLIGHT
        case DaylightNAction(20, f : UnitedDoveCorps) =>
            Ask(f).each($(UnitedDoveCorps.emergency, UnitedDoveCorps.protection, UnitedDoveCorps.stabilization))(m => UDCMandateAction(f, m)).daylight(f)

        case UDCMandateAction(f, m) =>
            f.mandate = m

            f.dest = None

            f.log("chose the", UnitedDoveCorps.mandate(f, m), "mandate")

            Next

        case DaylightNAction(40, f : UnitedDoveCorps) if f.mandate == UnitedDoveCorps.protection =>
            soft()

            val l = f.ceasefires

            if (l.none)
                NoAsk(f)(Next)
            else
                Ask(f).group("Declare Peace".styled(f)).each(l)(c => UDCDeclarePeaceAction(f, c)).done(Next).daylight(f)

        case UDCDeclarePeaceAction(f, c) =>
            f.from(c) --> Ceasefire --> f.reserve
            f.reserve --> PeaceToken --> c

            f.nscore(1)("declaring peace")(f, "declared", PeaceToken.of(f), "in", c, ForVP)

            DaylightNAction(40, f)

        case DaylightNAction(60, f : UnitedDoveCorps) =>
            implicit val ask = builder

            if (f.acted < 3) {
                if (f.mandate == UnitedDoveCorps.emergency) {
                    val rr = clearings.%(f.canPlace)
                    + UDCRecruitAction(f, rr).!(f.pool(Dove).not, "max").!(rr.none)

                    + UDCExpeditionAction(f).!(f.moveFrom.none)

                    val cc = ceasefirable(f)
                    + UDCCeasefireAction(f, cc).!(f.tokensLeft <= 0, "max").!(cc.none)
                }

                if (f.mandate == UnitedDoveCorps.protection) {
                    val rr = f.peace.%(f.canPlace)
                    + UDCRecruitAction(f, rr).!(f.pool(Dove).not, "max").!(rr.none, "no peace")

                    + UDCMoveAction(f).!(f.moveFrom.none)

                    val hh = ransomable(f)
                    + UDCRansomAction(f, hh).!(f.ransomed, "once per turn").!(hh.none)
                }

                if (f.mandate == UnitedDoveCorps.stabilization) {
                    + UDCMoveAction(f).!(f.moveFrom.none)

                    $[Building](DoveClinic, DoveSchool).foreach { p =>
                        val bb = buildable(f, p)
                        + UDCBuildAction(f, p, bb).!(f.pool(p).not, "max").!(bb.none)
                    }

                    + UDCRepatriateAction(f).!(f.refugees.none, "no refugees")
                }
            }

            + EndTurnSoftAction(f, "Actions", ForfeitActions(3 - f.acted))

            ask(f).daylight(f)

        case UDCRecruitAction(f, l) =>
            Ask(f).each(l)(c => UDCRecruitClearingAction(f, c)).cancel

        case UDCRecruitClearingAction(f, c) =>
            game.highlights :+= PlaceHighlight($(c))

            f.reserve --> Dove --> c

            f.log("recruited", Dove.of(f), "in", c)

            UDCDoneAction(f, Repeat)

        case UDCMoveAction(f) =>
            MoveInitAction(f, f, $, NoMessage, f.moveFrom, f.movable, $(CancelAction), UDCDoneAction(f, Repeat))

        case UDCExpeditionAction(f) =>
            MoveInitAction(f, f, $, NoMessage, f.moveFrom, f.movable, $(CancelAction), UDCExpeditionMovedAction(f))

        case UDCExpeditionMovedAction(f) =>
            f.dest.filter(d => f.canAttackIn(d)) match {
                case Some(d) =>
                    BattleInitAction(f, f, NoMessage, $(d), $(UDCDoneAction(f, Repeat).as("Skip the battle")), UDCDoneAction(f, Repeat))
                case None =>
                    UDCDoneAction(f, Repeat)
            }

        case UDCCeasefireAction(f, l) =>
            Ask(f).each(l)(c => UDCCeasefireClearingAction(f, c)).cancel

        case UDCCeasefireClearingAction(f, c) =>
            game.highlights :+= PlaceHighlight($(c))

            f.reserve --> Ceasefire --> c

            f.log("declared", Ceasefire.of(f), "in", c)

            UDCDoneAction(f, Repeat)

        case UDCRansomAction(f, l) =>
            Ask(f).each(l)(e => UDCRansomPlayerAction(f, e)).cancel

        case UDCRansomPlayerAction(f, e) =>
            f.ransomed = true

            f.log("ransomed hostages from", e)

            Ask(e).each(e.hand)(d => UDCRansomGiveAction(e, f, d)).needOk

        case UDCRansomGiveAction(e, f, d) =>
            e.hand --> d --> f.hand

            e.nscore(1)("giving a ransom")(e, "gave", f, "a card", ForVP)

            UDCDoneAction(f, Repeat)

        case UDCBuildAction(f, p, l) =>
            Ask(f).each(l)(c => UDCBuildClearingAction(f, p, c)).cancel

        case UDCBuildClearingAction(f, p, c) =>
            val h = f.hand.%(_.matches(c.cost))

            Ask(f).each(h.combinations(f.next(p)).$)(l => UDCBuildCardsAction(f, p, c, l)).cancel

        case UDCBuildCardsAction(f, p, c, l) =>
            game.highlights :+= PlaceHighlight($(c))

            val vp = f.next(p)

            f.hand --> l --> discard.quiet

            f.reserve --> p --> c

            f.nscore(vp)("building", p.of(f))(f, "built", p.of(f), "in", c, "with", l./(_.elem).comma, ForVP)

            UDCDoneAction(f, Repeat)

        case UDCRepatriateAction(f) =>
            Ask(f).each(f.refugees)(d => UDCRepatriateCardAction(f, d)).cancel

        case UDCRepatriateCardAction(f, d) =>
            f.refugees --> d --> f.hand

            f.log("repatriated a card from the", Refugees.of(f))

            UDCDoneAction(f, Repeat)

        case UDCDoneAction(f, then) =>
            f.acted += 1

            f.dest = None

            then

        case DaylightNAction(80, f : UnitedDoveCorps) if f.mandate == UnitedDoveCorps.emergency =>
            UDCResupplyAction(f)

        case UDCResupplyAction(f) =>
            val l = f.ceasefires.%(f.canPlace).diff(f.resupplied)

            if (l.none || f.pool(Dove).not)
                Next
            else
            if (l.num <= f.pooled(Dove)) {
                l.foreach(c => f.reserve --> Dove --> c)

                f.log("resupplied", Dove.sof(f), "in", l./(_.elem).comma)

                Next
            }
            else
                Ask(f).each(l)(c => UDCResupplyPickAction(f, c)).daylight(f)

        case UDCResupplyPickAction(f, c) =>
            f.reserve --> Dove --> c

            f.resupplied :+= c

            f.log("resupplied", Dove.of(f), "in", c)

            UDCResupplyAction(f)

        case DaylightNAction(80, f : UnitedDoveCorps) if f.mandate == UnitedDoveCorps.stabilization =>
            XCraftMainAction(f)

        // EVENING
        case EveningNAction(20, f : UnitedDoveCorps) =>
            f.evac = f.peace ++ f.all(DoveClinic)

            UDCEvacuateAction(f)

        case UDCEvacuateAction(f) =>
            soft()

            val l = evacuable(f)

            if (l.none)
                NoAsk(f)(Next)
            else
                Ask(f).group("Evacuate Refugees".styled(f))
                    .some(l)(c => f.hand.%(_.matches(c.cost))./(d => UDCEvacuateCardAction(f, c, d)))
                    .done(Next)
                    .evening(f)

        case UDCEvacuateCardAction(f, c, d) =>
            f.hand --> d --> f.refugees

            f.evac = f.evac.diff($(c))

            f.log("evacuated refugees from", c)

            UDCEvacuateAction(f)

        case NightStartAction(f : UnitedDoveCorps) =>
            EveningDrawAction(f, f.draws)

        case FactionCleanUpAction(f : UnitedDoveCorps) =>
            f.mandate = 0
            f.acted = 0
            f.ransomed = false
            f.dest = None
            f.evac = $
            f.resupplied = $

            CleanUpAction(f)

        case _ => UnknownContinue
    }

}
