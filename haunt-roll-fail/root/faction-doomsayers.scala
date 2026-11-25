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

// The Doomed Expansion's Doomsayers Union (slugfacekillah, v1.2; the rules are its faction board's): cats who roll demand dice for
// their actions, commit dice to the union card to re-roll the rest (and bust it when the strikes outnumber its locks), reveal cards to
// crusade and consecrate, score the cards revealed, and light Flames in the forests they enclose, winning when there's none left to light.
//
// Readings (the board leaves these open):
// - Dice: the Birdsong roll takes 3 dice from the supply, black first, then orange, then yellow; Renegotiate's extra die is the next one
//   in that order; dice not committed go back to the supply at the end of the turn, committed ones (and strikes) stay on the union card
//   until spent or lost; a die spent in Daylight is taken from the rolled ones first, then from the union card
// - The union card: a strike rolled goes onto it (while it shows "The End is Nigh!"), a die can be committed only while it holds fewer
//   dice than the locks showing, and when it holds more it flips to "Cats on Strike!", losing every die on it; while on strike, strikes
//   rolled are lost and nothing can be committed; it flips back at the end of the Evening
// - Locks and cards: the Churches track's spaces are lock, card, lock, lock, card, lock, the leftmost uncovered at setup; a space is
//   uncovered for each church on the map, so a church removed covers one again (Morale Drop, checked as it is removed)
// - Recruit: up to 4 warriors in clearings matching the card spent (a bird card matches every clearing), at most one in each clearing
//   without a church
// - Revealed cards stay revealed until the Evening's Score, so a card is revealed for one action only; DemandCrusade and DemandConsecrate need a
//   card matching the clearing (moved from, battled in or placed in); a DemandCrusade battle reveals its card after the battle, a move after
//   the move; with no matching card left then, the die is spent all the same
// - Craft: a card costing N takes N craft dice and N churches as crafting pieces
// - Apocalypse Meow: a forest is enclosed when each clearing next to it has a church or a summoning circle; the enemy pieces in a forest
//   that gets a Flame are removed (the Doomsayers scoring the buildings and tokens) or, the Vagabond, moved by their owner to a clearing
//   next to it; no enemy moves into a forest with a Flame
// - The End: the Doomsayers win when a forest is to get a Flame and all 6 are on the map

case object UnionCat extends Warrior {
    override def name = "Doomsayer"
}

case object Church extends Building
case object SummoningCircle extends Token {
    override def name = "Summoning Circle"
}

case object ApocalypseMeow extends FactionEffect {
    val name = "Apocalypse Meow"
}

case object TheEnd extends FactionEffect {
    val name = "The End"
}

trait DemandFace extends Record with Elementary {
    def name : String
    def elem = name.styled(styles.hit)
}

case object DemandCrusade extends DemandFace { val name = "Crusade" }
case object DemandConsecrate extends DemandFace { val name = "Consecrate" }
case object DemandCraft extends DemandFace { val name = "Craft" }
case object DemandStrike extends DemandFace { val name = "Strike" }

trait DemandColor extends Record {
    def faces : $[DemandFace]
    def id : String
}

case object BlackDemand extends DemandColor {
    val faces = $(DemandCrusade, DemandCrusade, DemandCrusade, DemandConsecrate, DemandConsecrate, DemandCraft)
    val id = "black"
}

case object OrangeDemand extends DemandColor {
    val faces = $(DemandStrike, DemandCrusade, DemandCrusade, DemandConsecrate, DemandConsecrate, DemandCraft)
    val id = "orange"
}

case object YellowDemand extends DemandColor {
    val faces = $(DemandStrike, DemandStrike, DemandCrusade, DemandConsecrate, DemandCraft, DemandCraft)
    val id = "yellow"
}

case class DemandDie(color : DemandColor, face : DemandFace) extends Record with Elementary {
    def elem = face.name.styled(styles.hit) ~ " (" ~ color.id ~ ")"
    def img = Image("du-die-" + color.id + "-" + face.name.toLowerCase, styles.token)
}

trait Doomsayers extends WarriorFaction {
    val clashKey = DU

    val warrior = UnionCat

    def abilities(options : $[Meta.O]) = $(ApocalypseMeow, TheEnd)

    def pieces(options : $[Meta.O]) = UnionCat *** 25 ++ Church *** 6 ++ SummoningCircle *** 6

    override def note : Elem = SpacedDash ~ "by " ~ "slugfacekillah".hl

    def advertising = 3.times(Image(this.style + "-church", styles.piece)).merge

    def motto = "Doom".styled(this)

    override def initialDamagePriorities(options : $[Meta.O]) = $(UnionCat, SummoningCircle, Church)
}

case object DU extends Doomsayers {
    val name = "Doomsayers Union"
    val short = "DU"
    val style = "du"
    val priority = "V'"
}

object Doomsayers {
    val dice : $[DemandColor] = $(BlackDemand, BlackDemand, BlackDemand, OrangeDemand, OrangeDemand, YellowDemand)

    val flames = 6

    // the Churches track, left to right: true for a lock, false for a card
    val track = $(true, false, true, true, false, true)

    def revealScore(n : Int) = n match {
        case 0 => 0
        case 1 | 2 => 1
        case 3 => 2
        case _ => 4
    }
}

case object DoomCrusadeMsg extends Message {
    def elem(implicit game : Game) = "with " ~ DemandCrusade.elem
}

case object DoomCraftMsg extends Message {
    def elem(implicit game : Game) = "with " ~ DemandCraft.elem
}

case class DoomApocalypseAction(self : Doomsayers, l : $[Forest]) extends ForcedAction
case class DoomTheEndAction(self : Doomsayers) extends ForcedAction
case class DoomFlameMoveAction(self : Faction, f : Doomsayers, r : Forest, c : Clearing, l : $[Forest]) extends BaseAction(ApocalypseMeow.of(f), "moves the", Vagabond.of(self), "out of", r, "to")(c)

case class DoomRollAction(self : Doomsayers, n : Int, then : ForcedAction) extends ForcedAction
case class DoomRolledAction(self : Doomsayers, colors : $[DemandColor], rolled : $[DemandFace], then : ForcedAction) extends RolledAction[DemandFace]

case class DoomRecruitAction(self : Doomsayers, d : DeckCard) extends BaseAction("Recruit".styled(self), "with")(d.img) with ViewCard
case class DoomRecruitNextAction(self : Doomsayers, d : DeckCard, n : Int, done : $[Clearing]) extends ForcedAction
case class DoomRecruitPlaceAction(self : Doomsayers, d : DeckCard, c : Clearing, n : Int, done : $[Clearing]) extends BaseAction("Recruit".styled(self), "(" ~ n.hl ~ " left)")(c)

case class DoomCommitAction(self : Doomsayers, d : DemandDie) extends BaseAction("Renegotiate".styled(self), "committing a die to the union card")(d.img, d) with Soft
case class DoomRenegotiateAction(self : Doomsayers, d : DemandDie, reroll : $[DemandDie]) extends BaseAction("Renegotiate".styled(self), "committing", d, "and re-rolling")(reroll.none.?("only a new die".txt).|(reroll./(_.img).merge))

case class DoomCrusadeBattleAction(self : Doomsayers, l : $[Clearing]) extends BaseAction(DemandCrusade.elem)("Battle".styled(self), dt.Battle) with Soft
case class DoomCrusadeMoveAction(self : Doomsayers, l : $[Clearing]) extends BaseAction(DemandCrusade.elem)("Move".styled(self), dt.Move) with Soft
case class DoomCrusadeBattledAction(self : Doomsayers, c : |[Clearing]) extends ForcedAction
case class DoomCrusadeMovedAction(self : Doomsayers, from : |[Region], to : |[Region]) extends ForcedAction
case class DoomRevealAction(self : Doomsayers, d : DeckCard, face : DemandFace, then : ForcedAction) extends BaseAction(face.elem, "reveal a card")(d.img) with ViewCard
case class DoomSpendAction(self : Doomsayers, face : DemandFace, then : ForcedAction) extends ForcedAction
case class DoomCrusadeBattleAfterAction(self : Doomsayers, c : Clearing) extends ForcedAction

case class DoomConsecrateAction(self : Doomsayers) extends BaseAction(DemandConsecrate.elem)("Place", Church.of(self), "or", SummoningCircle.of(self)) with Soft
case class DoomConsecrateClearingAction(self : Doomsayers, p : Piece, c : Clearing) extends BaseAction(DemandConsecrate.elem, "place")(p.of(self), p.imgd(self), "in", c) with Soft
case class DoomConsecrateCardAction(self : Doomsayers, p : Piece, c : Clearing, d : DeckCard) extends BaseAction(DemandConsecrate.elem, p.of(self), "in", c, "revealing")(d.img) with ViewCard

case class DoomCraftAction(self : Doomsayers, n : Int) extends BaseAction(DemandCraft.elem)("Craft".styled(self), "with", Church.sof(self)) with Soft
case class DoomCraftPayAction(self : Doomsayers, n : Int, then : ForcedAction) extends ForcedAction


class DoomsayersPlayer(val faction : Doomsayers)(implicit val game : Game) extends FactionState {
    var rolled : $[DemandDie] = $
    var card : $[DemandDie] = $
    var onStrike = false
    var recruited = false
    var placed = false
    var flames : $[Forest] = $

    val revealed = cards("revealed")

    def craft = all(Church)./(_.asset)

    def uncovered = Doomsayers.track.take(all(Church).num)
    def locks = uncovered.count(true)
    def bonus = uncovered.count(false)

    def supply : $[DemandColor] = {
        var l = Doomsayers.dice
        (rolled ++ card).foreach(d => l = l :- d.color)
        l
    }

    def usable : $[DemandDie] = rolled ++ card.%(_.face != DemandStrike)

    def count(face : DemandFace) = usable.count(_.face == face)

    def canCommit = onStrike.not && card.num < locks
}


object DoomsayersExpansion extends FactionExpansion[Doomsayers] {
    // the forests with a Flame, where enemies of the Doomsayers can't go
    def burnt(f : Faction)(implicit game : Game) : $[Forest] = factions.of[Doomsayers].%(game.states.contains).%(_ != f)./~(_.flames)

    def markers(r : Region)(implicit game : Game) : $[Figure] = r match {
        case r : Forest => factions.of[Doomsayers].%(game.states.contains).%(_.flames.has(r))./(_ => Figure(Neutral, UnionFlame, 1))
        case _ => $
    }

    def enclosed(f : Doomsayers, r : Forest)(implicit game : Game) = board.fromForest(r).forall(c => f.at(c).exists(p => p == Church || p == SummoningCircle))

    def matching(f : Doomsayers, c : Clearing)(implicit game : Game) = f.hand.%(_.matches(c.cost))

    // breaks the union card when it holds more dice than the locks showing
    def bust(f : Doomsayers)(implicit game : Game) {
        if (f.onStrike.not && f.card.num > f.locks) {
            f.log("busted the union card with", f.card.num.hl, "dice and", f.locks.hl, "lock" + (f.locks != 1).??("s") ~ ",", "the", "Cats on Strike!".styled(f))

            f.card = $
            f.onStrike = true
        }
    }

    def strikes(f : Doomsayers, l : $[DemandDie])(implicit game : Game) {
        val s = l.%(_.face == DemandStrike)

        f.rolled ++= l.%(_.face != DemandStrike)

        if (s.any) {
            if (f.onStrike)
                f.log("lost", s.num.hl, "strike" + (s.num > 1).??("s"), "while on strike")
            else {
                f.card ++= s

                f.log("put", s.num.hl, "strike" + (s.num > 1).??("s"), "on the union card")

                bust(f)
            }
        }
    }

    def spend(f : Doomsayers, face : DemandFace)(implicit game : Game) {
        f.rolled.find(_.face == face) match {
            case Some(d) => f.rolled :-= d
            case None => f.card.find(_.face == face).foreach(d => f.card :-= d)
        }
    }

    def perform(action : Action, soft : Void)(implicit game : Game) = action @@ {
        // SETUP
        case CreatePlayerAction(f : Doomsayers) =>
            game.states += f -> new DoomsayersPlayer(f)

            FactionInitAction(f)

        case FactionSetupAction(f : Doomsayers) if options.has(SetupTypeHomelands) =>
            val l = clearings.diff(game.homelands).%(f.canPlace)
            val ll = l.%(c => factions.but(f)./~(_.at(c)).none).some.|(l)

            Ask(f).each(ll)(c => StartingClearingAction(f, c).as(c)(f, "starts in")).needOk

        case FactionSetupAction(f : Doomsayers) =>
            StartingCornerAction(f)

        case StartingClearingAction(f : Doomsayers, c) =>
            game.homelands :+= c

            f.reserve --> Church --> c
            f.reserve --> 4.times(UnionCat) --> c

            val l = game.connected(c).%(f.canPlace)

            l.foreach(x => f.reserve --> UnionCat --> x)

            f.log("placed", Church.of(f), "and", 4.of(UnionCat.name).styled(f), "in", c, "and", UnionCat.of(f), "in", l.comma)

            SetupFactionsAction

        // MORALE DROP
        case ForcedRemoveTargetEffectAction(e, c, f : Doomsayers, Church, then) if f.onStrike.not && f.card.num > f.locks =>
            f.log("lost a", Church.of(f), "and morale")

            bust(f)

            TryAgain

        // APOCALYPSE MEOW
        case BirdsongNAction(15, f : Doomsayers) =>
            DoomApocalypseAction(f, board.forests.diff(f.flames).%(r => enclosed(f, r)))

        case DoomApocalypseAction(f, Nil) =>
            Next

        case DoomApocalypseAction(f, r :: rest) if f.flames.num >= Doomsayers.flames =>
            DoomTheEndAction(f)

        case DoomApocalypseAction(f, r :: rest) =>
            val heroes = factions.%(_ != f).%(e => game.pieces.has(e -> r)).%(e => e.at(r).has(Vagabond))

            if (heroes.any) {
                val e = heroes.first
                val l = board.fromForest(r).%(e.canPlace).some.|(board.fromForest(r))

                Ask(e).each(l)(c => DoomFlameMoveAction(e, f, r, c, r :: rest))
            }
            else {
            f.flames :+= r

            game.highlights :+= PlaceHighlight(board.fromForest(r))

            f.log("lit a", "Flame".styled(f), "in", r, "with", ApocalypseMeow.of(f))

            (factions ++ hirelings).%(_ != f).%(e => game.pieces.has(e -> r)).foreach { e =>
                e.from(r).$.foreach { u =>
                    u --> game.recycle

                    f.log("burned", u.piece.of(e), "in", r)

                    if (u.piece.is[Scoring])
                        f.oscore(1)("burning", u.piece.of(e))
                }
            }

            factions.of[CommonAbduct].%(_ != f).%(e => game.pieces.has(e -> Cage(r))).foreach { e =>
                e.from(Cage(r)).$.foreach { u =>
                    u --> game.recycle

                    f.log("freed", u.piece.of(u.faction), "from", r)
                }
            }

            DoomApocalypseAction(f, rest)
            }

        case DoomFlameMoveAction(e, f, r, c, l) =>
            e.from(r) --> Vagabond --> c

            e.log("moved out of", r, "to", c)

            DoomApocalypseAction(f, l)

        case DoomTheEndAction(f) =>
            f.log("could light no more", "Flames".styled(f) ~ ",", "and brought about", TheEnd.of(f))

            val winners = $[Faction](f) ++ factions.%(_.coalition.has(f))

            Milestone(GameOverAction(winners, factions./(e => e -> e.vp).to(scala.collection.immutable.ListMap), game.seating./(game.ptf), game.recorded))

        // ROLL
        case BirdsongNAction(20, f : Doomsayers) =>
            DoomRollAction(f, 3, Next)

        case DoomRollAction(f, n, then) =>
            val l = f.supply.take(n)

            if (l.none)
                then
            else
                Roll[DemandFace](l./(c => Die.custom(c.faces)), r => DoomRolledAction(f, l, r, then))

        case DoomRolledAction(f, l, r, then) =>
            val dd = l.lazyZip(r).map((c, x) => DemandDie(c, x))

            f.log("rolled", dd./(_.elem).comma)

            strikes(f, dd)

            then

        // RECRUIT
        case BirdsongNAction(30, f : Doomsayers) =>
            soft()

            if (f.recruited || f.pool(UnionCat).not)
                NoAsk(f)(Next)
            else
                Ask(f)
                    .each(f.hand.get)(d => DoomRecruitAction(f, d).!(clearings.%(c => d.matches(c.cost)).%(f.canPlace).none))
                    .skip(Next)
                    .birdsong(f)

        case DoomRecruitAction(f, d) =>
            f.hand --> d --> discard.quiet

            f.recruited = true

            f.log("recruited with", d)

            DoomRecruitNextAction(f, d, 4, $)

        case DoomRecruitNextAction(f, d, n, done) =>
            val l = clearings.%(c => d.matches(c.cost)).%(f.canPlace).%(c => f.at(c).has(Church) || done.has(c).not)

            if (n <= 0 || l.none || f.pool(UnionCat).not)
                Next
            else
                Ask(f).each(l)(c => DoomRecruitPlaceAction(f, d, c, n, done)).done(Next)

        case DoomRecruitPlaceAction(f, d, c, n, done) =>
            game.highlights :+= PlaceHighlight($(c))

            f.reserve --> UnionCat --> c

            f.placed = true

            f.log("placed", UnionCat.of(f), "in", c)

            DoomRecruitNextAction(f, d, n - 1, done :+ c)

        // RENEGOTIATE
        case BirdsongNAction(40, f : Doomsayers) =>
            soft()

            if (f.placed.not || f.canCommit.not || f.rolled.none)
                NoAsk(f)(Next)
            else
                Ask(f)
                    .group("Renegotiate".styled(f), "(" ~ f.card.num.hl ~ " of " ~ f.locks.hl ~ " locks on the union card)")
                    .each(f.rolled.distinct)(d => DoomCommitAction(f, d))
                    .done(Next)
                    .birdsong(f)

        case DoomCommitAction(f, d) =>
            val rest = f.rolled :- d
            val subsets = 0.to(rest.num)./~(n => rest.combinations(n).$)./(_.sortBy(_.toString)).distinct

            Ask(f).add(subsets./(l => DoomRenegotiateAction(f, d, l))).cancel

        case DoomRenegotiateAction(f, d, l) =>
            f.rolled :-= d
            f.card :+= d

            l.foreach(x => f.rolled :-= x)

            f.log("committed", d, "to the union card")

            val extra = f.supply.diff(l./(_.color)).take(1)

            val dice = l./(_.color) ++ extra

            if (dice.none)
                Repeat
            else
                Roll[DemandFace](dice./(c => Die.custom(c.faces)), r => DoomRolledAction(f, dice, r, Repeat))

        // DAYLIGHT
        case DaylightNAction(40, f : Doomsayers) =>
            soft()

            val att = clearings.%(f.canAttackIn).%(c => matching(f, c).any)
            val mv = f.moveFrom.of[Clearing].%(c => matching(f, c).any)

            val church = clearings.%(f.rules).%(f.canBuild).%(c => f.at(c).has(SummoningCircle).not).%(c => matching(f, c).any)
            val circle = clearings.%(f.present).%(f.canPlace).%(c => f.at(c).has(SummoningCircle).not && f.at(c).has(Church).not).%(c => matching(f, c).any)

            val crafts = f.count(DemandCraft)
            val craftable = f.hand.%(f.craftable).%(_.as[CraftCard].?(_.cost.num <= crafts))

            Ask(f)
                .group("Demands", f.usable./(_.img).merge)
                .add(DoomCrusadeBattleAction(f, att).!(f.count(DemandCrusade) == 0, "no die").!(att.none, "nowhere"))
                .add(DoomCrusadeMoveAction(f, mv).!(f.count(DemandCrusade) == 0, "no die").!(mv.none, "nowhere"))
                .add(DoomConsecrateAction(f).!(f.count(DemandConsecrate) == 0, "no die").!((church.any && f.pool(Church)).not && (circle.any && f.pool(SummoningCircle)).not, "nowhere"))
                .add(DoomCraftAction(f, crafts).!(crafts == 0, "no die").!(craftable.none, "nothing craftable"))
                .add(Next.as("End", Daylight.elem))
                .daylight(f)

        case DoomCrusadeBattleAction(f, l) =>
            BattleInitAction(f, f, DoomCrusadeMsg, l, $(CancelAction), DoomCrusadeBattledAction(f, None))

        case BattleStartAction(self, e, a, m, c, o, i, DoomCrusadeBattledAction(f, None)) =>
            BattleStartAction(self, e, a, m, c, o, i, DoomCrusadeBattledAction(f, Some(c)))

        case DoomCrusadeBattledAction(f, c) =>
            val l = c./~(c => matching(f, c))

            if (l.none)
                DoomSpendAction(f, DemandCrusade, Repeat)
            else
                Ask(f).each(l)(d => DoomRevealAction(f, d, DemandCrusade, Repeat))

        case DoomCrusadeMoveAction(f, l) =>
            MoveInitAction(f, f, $, DoomCrusadeMsg, l, f.movable, $(CancelAction), DoomCrusadeMovedAction(f, None, None))

        case MoveCompleteAction(self, e, from, to, l, n, DoomCrusadeMovedAction(f, None, None)) =>
            MoveCompleteAction(self, e, from, to, l, n, DoomCrusadeMovedAction(f, Some(from), Some(to)))

        case DoomCrusadeMovedAction(f, from, to) =>
            val l = from./~(_.as[Clearing])./~(c => matching(f, c))
            val then = to./~(_.as[Clearing])./(c => DoomCrusadeBattleAfterAction(f, c)).|(Repeat)

            if (l.none)
                DoomSpendAction(f, DemandCrusade, then)
            else
                Ask(f).each(l)(d => DoomRevealAction(f, d, DemandCrusade, then))

        case DoomCrusadeBattleAfterAction(f, c) =>
            if (f.canAttackIn(c))
                BattleInitAction(f, f, DoomCrusadeMsg, $(c), $(Repeat.as("Skip")), Repeat)
            else
                Repeat

        case DoomRevealAction(f, d, face, then) =>
            f.hand --> d --> f.revealed

            f.log("revealed", d)

            DoomSpendAction(f, face, then)

        case DoomSpendAction(f, face, then) =>
            spend(f, face)

            then

        case DoomConsecrateAction(f) =>
            val church = clearings.%(f.rules).%(f.canBuild).%(c => f.at(c).has(SummoningCircle).not).%(c => matching(f, c).any)
            val circle = clearings.%(f.present).%(f.canPlace).%(c => f.at(c).has(SummoningCircle).not && f.at(c).has(Church).not).%(c => matching(f, c).any)

            Ask(f)
                .add(f.pool(Church).??(church./(c => DoomConsecrateClearingAction(f, Church, c))))
                .add(f.pool(SummoningCircle).??(circle./(c => DoomConsecrateClearingAction(f, SummoningCircle, c))))
                .cancel

        case DoomConsecrateClearingAction(f, p, c) =>
            Ask(f).each(matching(f, c))(d => DoomConsecrateCardAction(f, p, c, d)).cancel

        case DoomConsecrateCardAction(f, p, c, d) =>
            game.highlights :+= PlaceHighlight($(c))

            f.hand --> d --> f.revealed

            f.reserve --> p --> c

            f.log("revealed", d, "and placed", p.of(f), "in", c)

            DoomSpendAction(f, DemandConsecrate, Repeat)

        case DoomCraftAction(f, n) =>
            YYSelectObjectsAction(f, f.hand)
                .withGroup("Craft".styled(f) ~ " with " ~ f.craft.diff(f.crafted)./(dt.CraftSuit).merge ~ " and " ~ n.hl ~ " craft dice")
                .withRule(d => f.craftable(d) && d.as[CraftCard].?(_.cost.num <= n))
                .withThen(d => CraftAction(f, d, DoomCraftMsg, DoomCraftPayAction(f, d.as[CraftCard]./(_.cost.num).|(1), CraftPerformAction(f, d, DoomCraftMsg))))(d => game.desc("Craft", d, "with", d.as[CraftCard]./~(_.cost./(dt.CraftSuit)).merge))("Craft")
                .withExtra($(NoHand, CancelAction))

        case DoomCraftPayAction(f, n, then) =>
            n.times(()).foreach(_ => spend(f, DemandCraft))

            then

        // EVENING: SUMMON
        case EveningNAction(20, f : Doomsayers) =>
            val l = f.all(SummoningCircle).%(f.canPlace).take(f.pooled(UnionCat))

            l.foreach(c => f.reserve --> UnionCat --> c)

            if (l.any)
                f.log("summoned", UnionCat.of(f), "in", l.comma)

            Next

        // EVENING: SCORE
        case EveningNAction(30, f : Doomsayers) =>
            val n = f.revealed.num

            if (n > 0) {
                f.nscore(Doomsayers.revealScore(n))("revealing", n.cards)(f, "revealed", n.cards, ForVP)

                f.revealed --> f.hand
            }

            Next

        case NightStartAction(f : Doomsayers) =>
            EveningDrawAction(f, 1 + f.bonus)

        case FactionCleanUpAction(f : Doomsayers) =>
            f.rolled = $
            f.recruited = false
            f.placed = false

            if (f.revealed.any)
                f.revealed --> f.hand

            if (f.onStrike) {
                f.onStrike = false

                f.log("flipped the union card back to", "The End is Nigh!".styled(f))
            }

            CleanUpAction(f)

        case _ => UnknownContinue
    }
}
