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

// The Doomed Expansion's Eyrie's End (slugfacekillah, v1.4; Law of Doom EE and its faction board), a re-imagining of the Eyrie
// Dynasties: the same Decree and Turmoil, with five Cardinals mixed into its supply and drawn at random, caravans it crafts with in
// Evening, rule on its turn wherever it is tied, and leaders Patron, Megalomaniac and Hedonistic in place of the Builder.
// It is an Aviary, so the Eyrie's Decree, leaders, Turmoil, moves and battles are the Eyrie's code (faction-aviary.scala); this file
// takes over what differs.
//
// Readings (the rules leave these open):
// - Setup: always first in the standard setup (its priority); in Advanced Setup it sets up when drafted. Its caravan corner is any
//   corner; the warrior in "every clearing except the opposite corner" goes in the caravan corner too, which the piece count needs
//   (20 Eyrie warriors: 12 + 3, and 5 left with the Cardinals)
// - A New Roost: 3 warriors, as the rules say (the board says up to 6); they are drawn at random, like every warrior it places
// - The Red Menace: on their own turn the other factions (not the Vagabond) may move Cardinals in place of their own warriors in any
//   move they make, from any clearing with Cardinals, ignoring rule (a move from the Eyrie Dynasties' Decree still needs a matching
//   clearing). They don't battle with them or rule with them. Allied Without Aid (the Vagabond's) isn't done
// - Roving Caravans: a caravan moves only together with at least one warrior (the End's, or Cardinals moved by others), into a
//   clearing without a caravan; the ruler of that clearing before the move draws (the End too)
// - Lords of Corruption: on its turn the End rules where its pieces are tied for the most (with at least one piece there)
// - Patron: the craft adds one more activation of a face-up caravan's suit (any of them), once per turn
// - Hedonistic: at the end of Birdsong one roost goes back to the track and two warriors, drawn at random, take its place
// - Inaugurate: after the new leader, one card from hand may go to any column of the Decree
// - Crafted items score in full: the End has no Disdain for Trade

case object EECardinal extends Warrior {
    override def id = "Cardinal"
    override def name = "Cardinal"
}

trait EECaravanPiece extends Token with Movable

case object EECaravan extends EECaravanPiece {
    override def id = "Caravan"
    override def name = "Caravan"
}

case object EECaravanDown extends EECaravanPiece {
    override def id = "CaravanDown"
    override def name = "Caravan"
}

case object TheRedMenace extends FactionEffect {
    val name = "The Red Menace"
}

case object LordsOfCorruption extends FactionEffect {
    val name = "Lords of Corruption"
}

case object RovingCaravans extends FactionEffect {
    val name = "Roving Caravans"
}

case object RedMenaceMsg extends Message {
    def elem(implicit game : Game) = "Cardinals".styled(EE) ~ " with " ~ TheRedMenace.of(EE)
}

case object EE extends Aviary {
    val name = "Eyrie's End"
    override def funName = NameReference(name, this)
    val short = "EE"
    val style = "ee"
    val priority = "0"

    override def abilities(options : $[Meta.O]) = $(TheRedMenace, LordsOfCorruption, RovingCaravans, EveningCraft)

    override def pieces(options : $[Meta.O]) = Hawk *** 20 ++ EECardinal *** 5 ++ Roost *** 7 ++ EECaravan *** 3 ++ EECaravanDown *** 3

    override val transports : $[$[Transport]] = $($(CaravanRule))

    override def note : Elem = HorizontalBreak ~ "Doomed Expansion (fan-made)"

    def advertising = Hawk.img(this) ~ EECardinal.img(this) ~ Roost.img(this) ~ EECaravan.img(this)
    def motto = "Corrupt".styled(this)
}

// a caravan moves only with a warrior, into a clearing without a caravan, one at a time
case object CaravanRule extends Transport {
    override def allows(f : Faction, o : Region)(implicit game : Game) = f.at(o).of[Warrior].any
    override def allows(f : Faction, o : Region, d : Region, l : $[Movable])(implicit game : Game) = l.of[EECaravanPiece].none || (
        l.of[EECaravanPiece].num == 1 && l.of[Warrior].any && EE.at(d).of[EECaravanPiece].none)
}

// marks a move whose Roving Caravans draw is done
case object CaravanDrawn extends Transport

// The Red Menace: others move only Cardinals (and a caravan with them), ignoring rule
case object CardinalRule extends Transport {
    override def allows(f : Faction, o : Region)(implicit game : Game) = f == EE && EE.at(o).has(EECardinal)
    override def allows(f : Faction, o : Region, d : Region, l : $[Movable])(implicit game : Game) = allows(f, o) && l.of[Warrior].any && l.of[Warrior].forall(_ == EECardinal)
}

class EndPlayer(e : Aviary)(implicit g : Game) extends AviaryPlayer(e)(g) {
    var inaugurating = false

    def caravans = all(EECaravan)

    override def craft = Council.governedFor(faction, caravans)./(_.asset) ++ (leader.has(Patron) && caravans.any).$(OneOf(caravans./~(_.suits).of[BaseSuit].distinct))
}

case class EEPlacedAction(self : Aviary, c : Clearing, n : Int, then : ForcedAction, shuffled : $[Warrior]) extends ShuffledAction[Warrior]
case class EEDecreeCardAction(self : Aviary, h : $[DeckCard], m : scala.collection.immutable.SortedMap[Int, Decree], l : Leader, n : Int, max : Int) extends BaseAction("Add to", Decree.of(self))(h(n).img) with Soft with ViewCard with Selectable with NoClear with NoExplode with LimitedExtra[Decree] with ElemWrap {
    val d = h(n)

    def wrap(g : Game)(e : Elem) = Div(values./ { c =>
        val x = m.contains(n) || (m.num < max && (h(n).suit != Bird || m.keys./(h.apply)./(_.suit).%(_ == Bird).none))
        val s = m.get(n) == Some(c)
        val p = self.style + "-" + (c == Decree.Recruit && l == Charismatic).??("double-") + c.name + "-" + h(n).suit.name + s.not.??("-done")
        x.?(OnClick(c, s.?(Image(p, styles.action, styles.selected, xlo.pointer)).|(Image(p, styles.action, xlo.pointer)))).|(Image(p, styles.action))
    }.merge ~ Break ~ m.get(n).any.?(OnClick(m(n), e.styled(xlo.pointer))).|(e), styles.inline, styles.margined)

    def selected = m.contains(n)

    def fromAny(s : Any) = s match {
        case c : Decree => Some(c)
        case _ => None
    }

    def values = Decree.all

    def update(c : Decree) = this.copy(m = (m.get(n) == Some(c)).?(m - n).|(m + (n -> c)))
}
case class EEDecreeMainAction(f : Aviary, h : $[DeckCard], m : scala.collection.immutable.SortedMap[Int, Decree], max : Int) extends ForcedAction with Soft
case class EEDecreeExplodeAction(self : Aviary, h : $[DeckCard], l : Leader, max : Int) extends HiddenChoice with SelfExplode {
    def explode(withSoft : Boolean)(implicit game : Game) = {
        val iii = 1.to(max)./~(h.indices.combinations).flatMap { l =>
            l.foldLeft($(scala.collection.immutable.SortedMap[Int, Decree]()))((mm, n) => mm./~(m => Decree.all./(c => m + (n -> c))))
        }

        val ii = iii.%(m => h.indices.%(m.contains).%(n => h(n).suit == Bird).num <= 1).%(_.any)

        ii./(m => DecreeAction(self, h, m.keys.$, m.values.$)) ++ withSoft.??(ii./~(m => h.indices./(EEDecreeCardAction(self, h, m, l, _, max))))
    }
}

case class EEBuildMainAction(self : Aviary, roosts : $[Clearing], caravans : $[Clearing], suits : $[Suit]) extends OptionAction(Decree.Build, implicit g => "[" ~ self.desc(Decree.Build) ~ "]") with EDDaylightQuestion with Soft
case class EECaravanBuildAction(self : Aviary, c : Clearing, s : Suit, alts : $[Suit]) extends BaseAction(Decree.Build, "in", implicit g => "[" ~ self.desc(Decree.Build) ~ "]")("Caravan".styled(self), EECaravanDown.imgd(self), "in", c, (alts.num > 1).?("(" ~ s.elem ~ ")"))
case class EENewRoostAction(self : Aviary, c : Clearing) extends BaseAction("Start a new", Roost.of(self), Roost.imgd(self))(c)
case class EEHedonisticAction(self : Aviary, c : Clearing) extends BaseAction(Hedonistic.name.styled(self), "replaces a", Roost.of(self), "with two warriors")(c)
case class EEInaugurateAction(self : Aviary, d : DeckCard, c : Decree) extends BaseAction("Inaugurate".styled(self), "add a card to", Decree.of(self))(d.img, c.name.hl) with ViewCard
case class EECaravanDrawAction(f : |[Faction], then : ForcedAction) extends ForcedAction

object EyriesEndExpansion extends FactionExpansion[EE.type] {
    def end(implicit game : Game) = game.states(EE).as[EndPlayer].get

    def supply(implicit game : Game) : $[Warrior] = EE.pooled(Hawk).times(Hawk) ++ EE.pooled(EECardinal).times(EECardinal)

    // places n warriors drawn at random from the supply (Hawks and Cardinals)
    def place(c : Clearing, n : Int, then : ForcedAction)(implicit game : Game) : Continue = {
        val l = supply
        if (l.none || n <= 0 || EE.canPlace(c).not)
            then
        else
            Shuffle[Warrior](l, EEPlacedAction(EE, c, min(n, l.num), then, _))
    }

    def track(p : Int) = (p >= 7).?(0).|($(0, 1, 1, 2, 2, 3, 4)(6 - p))

    // The Red Menace: the Cardinals another faction may move with a move of its own
    def redMenace(self : Faction, f : Faction, m : Message, then : ForcedAction)(implicit game : Game) : $[UserAction] = {
        if (self != f || f == EE || f.is[Hero] || game.current != f || factions.has(EE).not || game.states.contains(EE).not)
            $
        else {
            val ok : Clearing => Boolean = then match {
                case DecreeDoneAction(_, NoSuit, all, _) => (c : Clearing) => all.exists(c.cost.matched)
                case _ => (c : Clearing) => true
            }

            val tt = $($(Roads, CardinalRule, CaravanRule))
            val plans = EE.movePlans(clearings.%(c => EE.at(c).has(EECardinal)).%(ok).%(EE.canMoveFrom), tt)

            plans.$./{ case (o, (t, l)) => MoveFromAction(f, EE, t, RedMenaceMsg, o, l, then) }
        }
    }

    def perform(action : Action, soft : Void)(implicit game : Game) = action @@ {
        // SETUP
        case CreatePlayerAction(f : EE.type) =>
            val s = new EndPlayer(f)
            s.contenders = $(Patron, Megalomaniac, Hedonistic, Charismatic, Commander, Despot)
            game.states += f -> s

            FactionInitAction(f)

        case FactionSetupAction(f : EE.type) =>
            StartingCornerAction(f)

        case StartingClearingAction(f : EE.type, r) =>
            game.homelands :+= r

            val o = board.opposite(r)
            val e = board.connected(r).%(c => board.corners.has(c).not && board.inner.has(c).not).some.|(board.connected(r))

            f.reserve --> EECaravanDown --> r

            clearings.%(c => o.has(c).not).foreach { c =>
                f.reserve --> Hawk --> c
            }
            f.reserve --> Hawk --> r

            (r +: e).foreach { c =>
                f.reserve --> Roost --> c
                f.reserve --> Hawk --> c
            }

            f.log("placed a", "Caravan".styled(f), "in", r, Comma, Roost.sof(f), "in", (r +: e)./(_.elem).comma, "and", Hawk.sof(f), "everywhere but", o)

            NewLeaderAction(f, None, SetupFactionsAction)

        // HELPERS
        case EEPlacedAction(f, c, n, then, l) =>
            val w = l.take(n)

            game.highlights :+= PlaceHighlight($(c))

            f.reserve --> w --> c

            f.log("placed", w./(_.of(f)).comma, "in", c)

            then

        case EECaravanDrawAction(e, then) =>
            e./(e => DrawCardsAction(e, 1, NoMessage, AddCardsAction(e, then))).|(then)

        // Roving Caravans: the clearing's ruler before the move draws a card
        case MoveListAction(self, f : EE.type, t, m, from : Clearing, to : Clearing, l, then) if t.any && t.has(CaravanDrawn).not && l.of[EECaravanPiece].any =>
            val r = (factions ++ game.unhired).%(game.states.contains).%(_.rules(to)).starting

            EECaravanDrawAction(r, ForceAction(MoveListAction(self, f, t :+ CaravanDrawn, m, from, to, l, then)))

        // TURN
        case BirdsongNAction(40, f : EE.type) =>
            Next

        case BirdsongNAction(50, f : EE.type) =>
            if (f.hand.any)
                EEDecreeMainAction(f, f.hand.get, scala.collection.immutable.SortedMap(), 1 + (f.pooled(Roost) <= 3).??(1))
            else {
                f.log("had no cards for", Decree.of(f))

                Next
            }

        case EEDecreeMainAction(f, h, m, max) =>
            Ask(f)
              .each(0.until(h.num).$)(n => EEDecreeCardAction(f, h, m, f.leader.get, n, max))
              .add(DecreeAction(f, h, m.keys.$, m.values.$).!(m.none))
              .add(EEDecreeExplodeAction(f, h, f.leader.get, max))
              .add(NoHand)
              .birdsong(f)

        case EEDecreeCardAction(f, h, m, _, _, max) =>
            EEDecreeMainAction(f, h, m, max)

        case BirdsongNAction(60, f : EE.type) =>
            if (f.all(Roost).none) {
                val b = clearings.%(f.canPlace).%(f.canBuild)./(c => c -> (factions./(_.at(c).of[Warrior].num).sum))
                val m = b.rights.minOr(0)
                val l = b.%((c : Clearing, n : Int) => n == m).lefts
                Ask(f).each(l)(EENewRoostAction(f, _)).needOk.bailw(Next) { f.log("could not find a place for a new", Roost.of(f)) }
            }
            else
                Next

        case EENewRoostAction(f, c) =>
            if (f.canBuild(c) && f.pool(Roost))
                f.reserve --> Roost --> c

            f.log("started a new", Roost.of(f), "in", c)

            place(c, 3, Next)

        case BirdsongNAction(70, f : EE.type) if f.leader.has(Hedonistic) && f.all(Roost).any =>
            Ask(f).each(f.all(Roost).distinct)(EEHedonisticAction(f, _)).needOk

        case EEHedonisticAction(f, c) =>
            f.from(c) --> Roost --> f.reserve

            f.log("replaced a", Roost.of(f), "in", c, "with two warriors as", Hedonistic.name.hl)

            place(c, 2, Next)

        case DaylightNAction(40, f : EE.type) =>
            Next

        case AviaryMainDecreeAction(f : EE.type, d @ Decree.Recruit, r) =>
            val l = f.all(Roost).distinct.%(f.canPlace).%(c => r.exists(c.cost.matched))

            if (l.any && supply.num > f.has(Charismatic).??(1))
                Ask(f).add(AviaryRecruitMainAction(f, l, r)).daylight(f)
            else
                TurmoilLoomingAction(f, d, r)

        case AviaryRecruitAction(f : EE.type, c, s, r) =>
            val n = 1 + f.has(Charismatic).??(1)

            f.log("recruited in", c)

            place(c, n, DecreeDoneAction(f, s, r, Repeat))

        case AviaryMainDecreeAction(f : EE.type, d @ Decree.Build, r) =>
            val roosts = f.pool(Roost).??(clearings.%(f.rules).%(f.canBuild).%(c => f.leader.has(Megalomaniac) || f.at(c).has(Roost).not).%(f.canPlace).%(c => r.exists(c.cost.matched)))
            val caravans = (f.all(EECaravan).num + f.all(EECaravanDown).num < 3).??(clearings.%(f.rules).%(f.canPlace).%(c => f.at(c).of[EECaravanPiece].none).%(c => r.exists(c.cost.matched)))

            if (roosts.any || caravans.any)
                Ask(f).add(EEBuildMainAction(f, roosts, caravans, r)).daylight(f)
            else
                TurmoilLoomingAction(f, d, r)

        case EEBuildMainAction(f, roosts, caravans, r) =>
            Ask(f)
              .some(roosts)(c => r.distinct.%(_.matches(c.cost))./(s => BuildRoostAction(f, c, s, r.distinct.%(_.matches(c.cost)))))
              .some(caravans)(c => r.distinct.%(_.matches(c.cost))./(s => EECaravanBuildAction(f, c, s, r.distinct.%(_.matches(c.cost)))))
              .needOk
              .daylight(f)

        case EECaravanBuildAction(f, c, s, r) =>
            game.highlights :+= PlaceHighlight($(c))

            f.reserve --> EECaravanDown --> c

            f.log("placed a", "Caravan".styled(f), "in", c)

            DecreeDoneAction(f, s, r, Repeat)

        // Turmoil, then Inaugurate
        case TurmoilAction(f : EE.type) if end.inaugurating.not =>
            end.inaugurating = true

            TryAgain

        case EveningStartAction(f : EE.type) if end.inaugurating =>
            end.inaugurating = false

            if (f.hand.any)
                Ask(f).some(f.hand.get)(d => Decree.all./(c => EEInaugurateAction(f, d, c))).skip(EveningStartAction(f))
            else
                EveningStartAction(f)

        case EEInaugurateAction(f, d, c) =>
            f.hand --> d --> f.decree(c)

            f.log("added", d, "to", c.name.hl, "as", "Inaugurate".styled(f))

            EveningStartAction(f)

        case EveningNAction(20, f : EE.type) =>
            XCraftMainAction(f)

        case EveningNAction(90, f : EE.type) =>
            val r = track(f.pooled(Roost))

            f.oscore(r)("from the", "Roosts".styled(f), "track")

            Next

        case NightStartAction(f : EE.type) =>
            EveningDrawAction(f, 1 + (f.pooled(Roost) <= 2).??(1) + (f.pooled(Roost) == 0).??(1))

        case FactionCleanUpAction(f : EE.type) if f.all(EECaravanDown).any =>
            f.all(EECaravanDown).foreach { c =>
                f.from(c) --> EECaravanDown --> f.reserve
                f.reserve --> EECaravan --> c
            }

            f.log("flipped", "Caravans".styled(f), "face up")

            TryAgain

        case _ => UnknownContinue
    }
}
