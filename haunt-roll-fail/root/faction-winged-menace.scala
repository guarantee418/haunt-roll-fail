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

// The Outsiders Expansion's Winged Menace (slugfacekillah, v2.4; Law of Slug WM and its faction board): bats from the Nexus that
// frighten the other warriors out of their clearings for points, take over the buildings they remove, and leave Guano behind.
//
// Readings (the rules leave these open):
// - The Nexus: a region of the Menace's own (`TheNexus`), not on the map, next to the four caves; only Scatter, Swoop, Spawn and
//   Explore use it, and nothing can battle or remove there
// - Caves: the map's ruins are the caves (`game.caves`); a cave that has given up its last item stays as an empty ruin, and the
//   "R" items are put under them with no Vagabond or Hundreds
// - Fight or Fright: the Menace picks its active card at the start of each battle it is in, before ambushes; only the Menace sees
//   it until it is revealed after the roll
// - Fright: the hits the Menace would deal move that many of the enemy's warriors (not the ones never removed, like the Alpha), all
//   to one adjacent clearing, picked by their owner among the clearings it rules, else among those with the most of its pieces;
//   with none of its pieces next door, the Menace picks; with no adjacent clearing they stay. With none of the enemy's warriors
//   left there, the Menace scores 1 for each moved. Its other hits (on buildings, tokens, the Vagabond's items) are lost
// - Squatters: after any removal of an enemy building by the Menace (in Fight or otherwise), it may place an infestation there
// - Infestations: two rows of four; the first space of each row shows nothing, the others a warrior (top) or a draw (bottom). The
//   Menace picks the row when it places one and, when both rows have some out, when one is removed
// - Guano: one token each time a Scatter, Spawn, Swoop or Dark Passage leaves more than four Menace warriors in a clearing (not in
//   the Keep's clearing, and only while one is left); the 2nd, 3rd and 4th cost 1, 2 and 3 points. Guano is its own player
//   (`GuanoMess`), always defenseless, and removing it scores 1 like any token; it goes back to the track
// - Dark Passage: on their turn, another player gives a card to place any number of their warriors from one cave's clearing in
//   another's (a placement, so not into the Keep's clearing)
// - Swarm: the battle is with a player with fewer warriors there; the second battle, for a matching card, needs no such count

case object Bat extends Warrior

// a cave: a ruin that stays when its last item is taken (WM.2.4); not the Menace's piece
case object Cave extends Building with Tenacious {
    override def name = "Cave"
}

case object Infestation extends Building {
    override def plural = "Infestations"
}

case object Guano extends Token {
    override def plural = "Guano"
}

case object TheNexus extends SpecialRegion {
    override def name = "The Nexus"
    override def elem(implicit game : Game) : Elem = "The Nexus".hh
}

// the Menace's Guano, its own player on the map
case object GuanoMess extends Hireling {
    val name = "Guano"
    val short = "WMG"
    val style = "wmg"

    val demoted = false
    val setup = false

    val clashKey = GuanoMess

    def pieces(options : $[Meta.O]) = Guano *** 4
}

case object FightOrFright extends FactionEffect {
    val name = "Fight or Fright"
}

case object Squatters extends FactionEffect {
    val name = "Squatters"
}

case object DarkPassage extends FactionEffect {
    val name = "Dark Passage"
}

case object MenaceChose extends BattleEffect {
    val name = "Fight or Fright"
}

case object MenaceRevealed extends BattleEffect {
    val name = "Fight or Fright"
}

trait WingedMenace extends WarriorFaction {
    val clashKey = WM

    val warrior = Bat

    def abilities(options : $[Meta.O]) = $(EveningCraft, FightOrFright, Squatters, DarkPassage)

    def pieces(options : $[Meta.O]) = Bat *** 20 ++ Infestation *** 8

    override def note : Elem = SpacedDash ~ "V" ~ "2.4".hh ~ " by " ~ "slugfacekillah".hl ~ HorizontalBreak ~ "Outsiders Expansion (fan-made)"

    def advertising = Bat.img(this) ~ Infestation.img(this)

    def motto = "Frighten".styled(this)

    override def initialDamagePriorities(options : $[Meta.O]) = $(Bat, Infestation)
}

case object WM extends WingedMenace {
    val name = "Winged Menace"
    val short = "WM"
    val style = "wm"
    val priority = "V'''"
}

trait WMEveningQuestion extends FactionAction {
    override def self : WingedMenace

    def question(implicit game : Game) = self.elem ~ SpacedDash ~ Evening.elem ~ Break ~
        Div(
            1.to(3)./(_ => Image("action-black", styles.action, "")).take(self.acted) ~
            1.to(3)./(_ => Image(self.style + "-action", styles.action)).drop(self.acted),
            styles.margined)
}

case class MenaceActiveAction(self : WingedMenace, b : Battle, fright : Boolean) extends BaseAction(self, "picks the active card")(fright.?("Fright!").|("Fight!").styled(self))
case class MenaceFrightAction(self : WingedMenace, b : Battle, e : Faction, n : Int, then : ForcedAction) extends ForcedAction
case class MenaceFrightToAction(self : Faction, t : WingedMenace, b : Battle, e : Faction, n : Int, d : Clearing, then : ForcedAction) extends BaseAction(self, "runs from", b.clearing, "with", n.hl, "warriors to")(d)

case class MenaceSquatAskAction(self : WingedMenace, c : Clearing, then : ForcedAction) extends ForcedAction
case class MenaceSquatAction(self : WingedMenace, c : Clearing, top : Boolean, then : ForcedAction) extends BaseAction(Squatters.of(self), "place", Infestation.of(self), "in", c, "from the")(top.?("top row").|("bottom row"))

case class MenaceReturnAction(self : WingedMenace, top : Boolean, then : ForcedAction) extends BaseAction(self, "returns", Infestation.of(self), "to the")(top.?("top row").|("bottom row"))

case class MenaceScatterFromAction(self : WingedMenace, o : Region) extends BaseAction("Scatter".styled(self), "warriors from")(o) with Soft
case class MenaceScatterAction(self : WingedMenace, o : Region, d : Region) extends BaseAction("Scatter".styled(self), Bat.of(self), "from", o, "to")(d)
case class MenaceScatterDoneAction(self : WingedMenace) extends ForcedAction

case class MenaceNexusSpawnAction(self : WingedMenace, n : Int) extends BaseAction("Spawn".styled(self))("Place", n.hl, Bat.sof(self), "in", TheNexus.name.hh)
case class MenaceSpawnClearingAction(self : WingedMenace, c : Clearing, n : Int) extends BaseAction("Spawn".styled(self), "with a card")(n.hl, Bat.sof(self), "in", c) with Soft
case class MenaceSpawnAction(self : WingedMenace, c : Clearing, n : Int, d : DeckCard) extends BaseAction("Spawn".styled(self), n.hl, Bat.sof(self), "in", c, "with")(d.img) with ViewCard

case class MenaceExploreAction(self : WingedMenace, c : Clearing, i : Item) extends BaseAction("Explore".styled(self), "a cave in", c)("Take", i.img, i)

case class MenaceBiteAction(self : WingedMenace, l : $[(Clearing, Faction, Piece)]) extends OptionAction("Bite".styled(self)) with WMEveningQuestion with Soft
case class MenaceBiteTargetAction(self : WingedMenace, c : Clearing, e : Faction, p : Piece) extends BaseAction("Bite".styled(self), "in", c)(p.of(e), p.imgd(e))

case class MenaceBuildAction(self : WingedMenace, l : $[Clearing]) extends OptionAction("Build".styled(self), Infestation.of(self), Infestation.imgd(self)) with WMEveningQuestion with Soft
case class MenaceBuildClearingAction(self : WingedMenace, c : Clearing) extends BaseAction("Build", Infestation.of(self), "in")(c) with Soft
case class MenaceBuildCardAction(self : WingedMenace, c : Clearing, d : DeckCard) extends BaseAction("Build", Infestation.of(self), "in", c, "with")(d.img) with ViewCard with Soft
case class MenaceBuildRowAction(self : WingedMenace, c : Clearing, d : DeckCard, top : Boolean) extends BaseAction("Build", Infestation.of(self), "in", c, "from the")(top.?("top row (warriors)").|("bottom row (draws)"))

case class MenaceSwarmAction(self : WingedMenace, l : $[(Clearing, Faction)]) extends OptionAction("Swarm".styled(self), dt.Battle) with WMEveningQuestion with Soft
case class MenaceSwarmTargetAction(self : WingedMenace, c : Clearing, e : Faction) extends BaseAction("Swarm".styled(self), "in", c)(e)
case class MenaceSwarmAgainAction(self : WingedMenace, c : Clearing, e : Faction) extends ForcedAction
case class MenaceSwarmCardAction(self : WingedMenace, c : Clearing, e : Faction, d : DeckCard) extends BaseAction("Battle", e, "again in", c, "with")(d.img) with ViewCard

case class MenaceSwoopAction(self : WingedMenace, l : $[Region]) extends OptionAction("Swoop".styled(self), dt.Move) with WMEveningQuestion with Soft
case class MenaceSwoopToAction(self : WingedMenace, d : Region) extends BaseAction("Swoop".styled(self), "to")(d) with Soft
case class MenaceSwoopMoveAction(self : WingedMenace, d : Region, o : Region, n : Int) extends BaseAction("Swoop".styled(self), "to", d)("Move", n.hl, Bat.sof(self), "from", o)
case class MenaceSwoopMoreAction(self : WingedMenace, d : Region) extends ForcedAction
case class MenaceSwoopDoneAction(self : WingedMenace, d : Region) extends ForcedAction

case class MenaceDoneAction(self : WingedMenace, then : ForcedAction) extends ForcedAction

case class DarkPassageMainAction(self : Faction, t : WingedMenace, l : $[Clearing], then : ForcedAction) extends BaseAction(DarkPassage.of(t))("Give", t, "a card to place warriors from one cave to another") with Soft
case class DarkPassageFromAction(self : Faction, t : WingedMenace, o : Clearing, then : ForcedAction) extends BaseAction(DarkPassage.of(t), "from")(o) with Soft
case class DarkPassageToAction(self : Faction, t : WingedMenace, o : Clearing, d : Clearing, then : ForcedAction) extends BaseAction(DarkPassage.of(t), "from", o, "to")(d) with Soft
case class DarkPassageCardAction(self : Faction, t : WingedMenace, o : Clearing, to : Clearing, d : DeckCard, then : ForcedAction) extends BaseAction("Give", t)(d.img) with ViewCard with Soft
case class DarkPassageAction(self : Faction, t : WingedMenace, o : Clearing, d : Clearing, c : DeckCard, n : Int, then : ForcedAction) extends BaseAction(DarkPassage.of(t), "from", o, "to", d)("Place", n.hl, "warriors")


class WingedMenacePlayer(val faction : WingedMenace)(implicit val game : Game) extends FactionState {
    val nexus = location(TheNexus, u => u.faction == faction && u.piece == Bat)

    // the active card: Fright! or Fight!
    var fright = false

    // the infestations placed from each row of the track
    var top = 0
    var bottom = 0

    var acted = 0
    var spawned = false
    var explored = false

    // Scatter's origin and the clearings it placed in
    var scatter : |[Region] = None
    var scattered : $[Region] = $
    var scatterDone = false

    def craft = all(Infestation)./(_.asset)

    def warriorBonus = max(0, top - 1)

    def drawBonus = max(0, bottom - 1)
}


object WingedMenaceExpansion extends FactionExpansion[WingedMenace] {
    def menaces(implicit game : Game) = factions.of[WingedMenace].%(game.states.contains)

    // the regions next to r for the Menace: the Nexus is next to the four caves
    def adjacent(r : Region)(implicit game : Game) : $[Region] = r match {
        case TheNexus => game.caves
        case c : Clearing => game.connected(c) ++ game.caves.has(c).$(TheNexus)
        case _ => $
    }

    def warriors(f : WingedMenace, r : Region)(implicit game : Game) = (r == TheNexus).?(f.nexus.num).|(r.as[Clearing]./(c => f.at(c).count(Bat)).|(0))

    def place(f : WingedMenace, n : Int, r : Region)(implicit game : Game) {
        if (r == TheNexus)
            f.reserve --> n.times(Bat) --> f.nexus
        else
            f.reserve --> n.times(Bat) --> r
    }

    def take(f : WingedMenace, n : Int, r : Region)(implicit game : Game) {
        if (r == TheNexus)
            f.nexus --> n.times(Bat) --> f.reserve
        else
            f.from(r) --> n.times(Bat) --> f.reserve
    }

    // Guano: more than four Menace warriors in a clearing (not the Nexus)
    def guano(f : WingedMenace, r : Region)(implicit game : Game) {
        r.as[Clearing].foreach { c =>
            if (f.at(c).count(Bat) > 4 && game.states.contains(GuanoMess) && GuanoMess.pool(Guano) && f.canPlace(c)) {
                GuanoMess.reserve --> Guano --> c

                val n = 4 - GuanoMess.pooled(Guano) - 1

                if (n > 0)
                {
                    f.log("left", Guano.of(GuanoMess), "in", c)
                    f.oscore(-n)("for", Guano.of(GuanoMess))
                }
                else
                    f.log("left", Guano.of(GuanoMess), "in", c)
            }
        }
    }

    def frightened(e : Faction, c : Clearing)(implicit game : Game) = e.at(c).of[Warrior].%(_.is[Tenacious].not).sortBy(p => e.as[WarriorFaction].?(_.warrior == p).?(0).|(1))

    def items(implicit game : Game) = game.caves.%(game.ruins.contains)./~(c => game.ruins(c).items.distinct./(i => (c, i)))

    def biteable(f : WingedMenace)(implicit game : Game) = clearings.%(c => f.at(c).has(Bat))./~(c => f.enemies.%(f.canRemove(c))./~(e => e.at(c).of[Warrior].%(_.is[Tenacious].not).distinct./(p => (c, e, p))))

    def swarmable(f : WingedMenace)(implicit game : Game) = clearings.%(c => f.at(c).has(Bat)).%(f.hasAttackingForce(_))./~(c => f.enemies.%(f.validAttackTarget(c)).%(e => e.at(c).of[Warrior].num < f.at(c).count(Bat))./(e => (c, e)))

    def origins(f : WingedMenace, d : Region)(implicit game : Game) = adjacent(d).%(o => warriors(f, o) > 0)

    def destinations(f : WingedMenace)(implicit game : Game) = (clearings ++ game.caves.any.$(TheNexus)).%(d => origins(f, d).any)

    def passages(f : Faction)(implicit game : Game) = game.caves.%(c => f.at(c).of[Warrior].any).%(c => game.caves.but(c).exists(f.canPlace))

    def darkPassage(f : Faction)(implicit game : Game, ask : ActionCollector) {
        if (game.current == f && f.hand.any)
            menaces.%(_ != f).foreach { t =>
                val l = passages(f)

                if (l.any)
                    + DarkPassageMainAction(f, t, l, Repeat)
            }
    }

    override def birdsong(f : Faction)(implicit game : Game, ask : ActionCollector) = darkPassage(f)
    override def daylight(f : Faction)(implicit game : Game, ask : ActionCollector) = darkPassage(f)
    override def evening(f : Faction)(implicit game : Game, ask : ActionCollector) = darkPassage(f)

    def menaceIn(b : Battle)(implicit game : Game) = $(b.attacker, b.defender).of[WingedMenace].%(game.states.contains).starting

    def perform(action : Action, soft : Void)(implicit game : Game) = action @@ {
        // SETUP
        case CreatePlayerAction(f : WingedMenace) =>
            game.states += f -> new WingedMenacePlayer(f)

            FactionInitAction(f)

        case FactionSetupAction(f : WingedMenace) =>
            game.caves = (game.caves ++ game.ruins.keys.$).distinct

            f.reserve --> 6.times(Bat) --> f.nexus

            DarkDeckExpansion.ensure(GuanoMess)

            f.log("turned the ruins of", game.caves./(_.elem).comma, "into caves and placed", 6.of(Bat.name).styled(f), "in", TheNexus.elem)

            SetupFactionsAction

        // the "R" items under the caves, unless the Vagabond or the Hundreds put them
        case AfterSetupAction(f : WingedMenace, then) if factions.of[Hero].none && factions.of[Horde].none && game.ruins.values.forall(_.items.none) =>
            AddRuinItemsAction(then)

        // FIGHT OR FRIGHT
        case BattleStartedAction(b) if menaceIn(b).exists(_.used.has(MenaceChose).not) =>
            val f = menaceIn(b).get

            f.used :+= MenaceChose

            Ask(f).group(f, "active card", "(" ~ f.fright.?("Fright!").|("Fight!").hl ~ ")")
                .add(MenaceActiveAction(f, b, false))
                .add(MenaceActiveAction(f, b, true))

        // each battle picks its own active card
        case BattleFinishedAction(b) if menaceIn(b).exists(f => f.used.has(MenaceChose) || f.used.has(MenaceRevealed)) =>
            menaceIn(b).foreach { f =>
                f.used = f.used.but(MenaceChose).but(MenaceRevealed)
            }

            BattleFinishedAction(b)

        case MenaceActiveAction(f, b, fright) =>
            f.fright = fright

            f.log("looked at", FightOrFright.of(f))

            BattleStartedAction(b)

        case BattleResolveHitsAction(b, fh, oh, fsh) if menaceIn(b).exists(_.used.has(MenaceRevealed).not) =>
            val f = menaceIn(b).get

            f.used :+= MenaceRevealed

            f.log("revealed", f.fright.?("Fright!").|("Fight!").styled(f))

            BattleResolveHitsAction(b, fh, oh, fsh)

        case BattleResolveHitsAction(b, fh, oh, fsh) if b.attacker.as[WingedMenace].?(_.fright) && fh + fsh > 0 =>
            val f = b.attacker.as[WingedMenace].get
            val e = b.defender

            MenaceFrightAction(f, b, e, min(fh + fsh, frightened(e, b.clearing).num), BattleResolveHitsAction(b, 0, oh, 0))

        case BattleResolveHitsAction(b, fh, oh, fsh) if b.defender.as[WingedMenace].?(_.fright) && oh > 0 =>
            val f = b.defender.as[WingedMenace].get
            val e = b.attacker

            MenaceFrightAction(f, b, e, min(oh, frightened(e, b.clearing).num), BattleResolveHitsAction(b, fh, 0, fsh))

        case MenaceFrightAction(f, b, e, n, then) =>
            val c = b.clearing
            val adj = game.connected(c)

            if (n <= 0 || adj.none)
                then
            else {
                val ruled = adj.%(e.rules)
                val top = adj./(x => e.at(x).num).maxOr(0)
                val most = adj.%(x => e.at(x).any && e.at(x).num == top)

                if (ruled.any)
                    Ask(e).each(ruled)(d => MenaceFrightToAction(e, f, b, e, n, d, then))
                else
                if (most.any)
                    Ask(e).each(most)(d => MenaceFrightToAction(e, f, b, e, n, d, then))
                else
                    Ask(f).each(adj)(d => MenaceFrightToAction(f, f, b, e, n, d, then))
            }

        case MenaceFrightToAction(_, f, b, e, n, d, then) =>
            val c = b.clearing

            val l = frightened(e, c).take(n)

            e.from(c) --> l --> d

            e.log("ran from", c, "to", d, "with", l./(_.of(e)).comma)

            if (frightened(e, c).none && e.at(c).of[Warrior].none)
                f.nscore(l.num)("frightening")(f, "frightened", e, "out of", c, ForVP)

            then

        // SQUATTERS
        case ForcedRemoveScoreAction(e : WingedMenace, c, f, p : Building, n, m, then) if f != e && then.is[MenaceSquatAskAction].not && p.is[Ruins].not =>
            ForcedRemoveScoreAction(e, c, f, p, n, m, MenaceSquatAskAction(e, c, then))

        case MenaceSquatAskAction(f, c, then) =>
            if (f.pool(Infestation) && f.canBuild(c))
                Ask(f).group(Squatters.of(f))
                    .add(MenaceSquatAction(f, c, true, then).!(f.top >= 4))
                    .add(MenaceSquatAction(f, c, false, then).!(f.bottom >= 4))
                    .skip(then)
            else
                then

        case MenaceSquatAction(f, c, top, then) =>
            f.reserve --> Infestation --> c

            if (top)
                f.top += 1
            else
                f.bottom += 1

            f.log("placed", Infestation.of(f), "in", c, "with", Squatters.of(f))

            then

        // INFESTATIONS: back to a row of the track
        case ForcedRemoveTargetEffectAction(e, c, f : WingedMenace, Infestation, then) =>
            if (f.top > 0 && f.bottom > 0)
                Ask(f).group(f, "returns", Infestation.of(f), "to")
                    .add(MenaceReturnAction(f, true, then))
                    .add(MenaceReturnAction(f, false, then))
            else {
                if (f.top > 0)
                    f.top -= 1
                else
                if (f.bottom > 0)
                    f.bottom -= 1

                then
            }

        case MenaceReturnAction(f, top, then) =>
            if (top)
                f.top -= 1
            else
                f.bottom -= 1

            then

        // DARK PASSAGE
        case DarkPassageMainAction(f, t, l, then) =>
            Ask(f).each(l)(c => DarkPassageFromAction(f, t, c, then)).cancel

        case DarkPassageFromAction(f, t, o, then) =>
            Ask(f).each(game.caves.but(o).%(f.canPlace))(d => DarkPassageToAction(f, t, o, d, then)).cancel

        case DarkPassageToAction(f, t, o, d, then) =>
            Ask(f).each(f.hand)(c => DarkPassageCardAction(f, t, o, d, c, then)).cancel

        case DarkPassageCardAction(f, t, o, d, c, then) =>
            Ask(f).each(1.to(f.at(o).of[Warrior].num).$.reverse)(n => DarkPassageAction(f, t, o, d, c, n, then)).cancel

        case DarkPassageAction(f, t, o, d, c, n, then) =>
            f.hand --> c --> t.hand

            val l = f.at(o).of[Warrior].sortBy(p => f.as[WarriorFaction].?(_.warrior == p).?(0).|(1)).take(n)

            f.from(o) --> l --> d

            f.log("gave", t, "a card and placed", l./(_.of(f)).comma, "from", o, "in", d, "with", DarkPassage.of(t))

            then

        // BIRDSONG
        case BirdsongNAction(40, f : WingedMenace) if f.scatterDone =>
            Next

        case BirdsongNAction(40, f : WingedMenace) if f.scatter.none =>
            soft()

            val l = (f.nexus.any.$(TheNexus) ++ clearings.%(c => f.at(c).has(Bat))).%(o => adjacent(o).exists(d => d == TheNexus || d.as[Clearing].?(f.canPlace)))

            if (l.none)
                NoAsk(f)(Next)
            else
                Ask(f).each(l)(o => MenaceScatterFromAction(f, o)).skip(Next).birdsong(f)

        case BirdsongNAction(40, f : WingedMenace) =>
            val o = f.scatter.get
            val l = adjacent(o).%(d => d == TheNexus || d.as[Clearing].?(f.canPlace))

            if (warriors(f, o) == 0 || l.none)
                MenaceScatterDoneAction(f)
            else
                Ask(f).group("Scatter".styled(f), "from", o.elem, "(" ~ warriors(f, o).hl ~ " left)")
                    .each(l)(d => MenaceScatterAction(f, o, d))
                    .done(MenaceScatterDoneAction(f))
                    .birdsong(f)

        case MenaceScatterFromAction(f, o) =>
            Ask(f).each(adjacent(o).%(d => d == TheNexus || d.as[Clearing].?(f.canPlace)))(d => MenaceScatterAction(f, o, d)).cancel

        case MenaceScatterAction(f, o, d) =>
            if (f.scatter.none)
                f.log("scattered from", o.elem)

            f.scatter = Some(o)

            take(f, 1, o)
            place(f, 1, d)

            f.scattered :+= d

            f.log("placed", Bat.of(f), "in", d.elem)

            Repeat

        case MenaceScatterDoneAction(f) =>
            f.scattered.distinct.foreach(d => guano(f, d))

            f.scattered = $
            f.scatterDone = true

            Next

        // DAYLIGHT
        case DaylightNAction(30, f : WingedMenace) if f.spawned.not =>
            val n = min(f.nexus.num / 2, f.pooled(Bat))

            f.spawned = true

            if (n <= 0)
                Next
            else
                Ask(f).add(MenaceNexusSpawnAction(f, n)).skip(Next).daylight(f)

        case MenaceNexusSpawnAction(f, n) =>
            f.reserve --> n.times(Bat) --> f.nexus

            f.log("spawned", n.of(Bat.name).styled(f), "in", TheNexus.elem)

            Next

        case DaylightNAction(40, f : WingedMenace) =>
            soft()

            val n = min(1 + f.warriorBonus, f.pooled(Bat))

            val l = clearings.%(f.canPlace).%(c => f.hand.exists(_.matches(c.cost)))

            if (n <= 0 || l.none)
                NoAsk(f)(Next)
            else
                Ask(f).each(l)(c => MenaceSpawnClearingAction(f, c, n)).done(Next).daylight(f)

        case MenaceSpawnClearingAction(f, c, n) =>
            Ask(f).each(f.hand)(d => MenaceSpawnAction(f, c, n, d).!(d.matches(c.cost).not)).cancel

        case MenaceSpawnAction(f, c, n, d) =>
            game.highlights :+= PlaceHighlight($(c))

            f.hand --> d --> discard.quiet

            f.reserve --> n.times(Bat) --> c

            f.log("spawned", n.of(Bat.name).styled(f), "in", c, "with", d)

            guano(f, c)

            Repeat

        case DaylightNAction(60, f : WingedMenace) if f.explored.not =>
            soft()

            val l = items

            if (f.nexus.none || l.none)
                NoAsk(f)(Next)
            else
                Ask(f).each(l) { case (c, i) => MenaceExploreAction(f, c, i) }.skip(Next).daylight(f)

        case MenaceExploreAction(f, c, i) =>
            val rest = game.ruins(c).items :- i

            if (rest.any)
                game.ruins += c -> Ruins(rest)
            else
                game.ruins -= c

            f.forTrade :+= i.pristine

            f.explored = true

            f.nscore(1)("exploring")(f, "explored the cave in", c, "and found", i, AndScoredVP)

            Next

        // EVENING
        case EveningNAction(30, f : WingedMenace) =>
            implicit val ask = builder

            if (f.acted < 3) {
                val bb = biteable(f)
                + MenaceBiteAction(f, bb).!(bb.none)

                val b1 = clearings.%(f.canPlace)
                val b2 = b1.%(f.rules)
                val b3 = b2.%(f.canBuild)
                val b4 = b3.%(c => f.hand.exists(_.matches(c.cost)))

                + MenaceBuildAction(f, b4)
                    .!(f.pool(Infestation).not, "max")
                    .!(b2.none, "no rule")
                    .!(b3.none, "no slot")
                    .!(b4.none, "no card")

                val ss = swarmable(f)
                + MenaceSwarmAction(f, ss).!(ss.none)

                val dd = destinations(f)
                + MenaceSwoopAction(f, dd).!(dd.none)
            }

            + EndTurnSoftAction(f, "Actions", ForfeitActions(3 - f.acted))

            ask(f).evening(f)

        case MenaceBiteAction(f, l) =>
            Ask(f).each(l) { case (c, e, p) => MenaceBiteTargetAction(f, c, e, p) }.cancel

        case MenaceBiteTargetAction(f, c, e, p) =>
            game.highlights :+= NukeHighlight($(c))

            f.log("bit", p.of(e), "in", c)

            ForcedRemoveAction(f, c, e, p, 0, Removing, ForcedRemoveFinishedAction(f, MenaceDoneAction(f, Repeat)))

        case MenaceBuildAction(f, l) =>
            Ask(f).each(l)(c => MenaceBuildClearingAction(f, c)).cancel

        case MenaceBuildClearingAction(f, c) =>
            Ask(f).each(f.hand)(d => MenaceBuildCardAction(f, c, d).!(d.matches(c.cost).not)).cancel

        case MenaceBuildCardAction(f, c, d) =>
            Ask(f)
                .add(MenaceBuildRowAction(f, c, d, true).!(f.top >= 4))
                .add(MenaceBuildRowAction(f, c, d, false).!(f.bottom >= 4))
                .cancel

        case MenaceBuildRowAction(f, c, d, top) =>
            game.highlights :+= PlaceHighlight($(c))

            f.hand --> d --> discard.quiet

            f.reserve --> Infestation --> c

            if (top)
                f.top += 1
            else
                f.bottom += 1

            f.log("built", Infestation.of(f), "in", c, "with", d)

            MenaceDoneAction(f, Repeat)

        case MenaceSwarmAction(f, l) =>
            Ask(f).each(l) { case (c, e) => MenaceSwarmTargetAction(f, c, e) }.cancel

        case MenaceSwarmTargetAction(f, c, e) =>
            f.acted += 1

            ForceAction(BattleConfirmAction(f, f, f, NoMessage, c, e, None, MenaceSwarmAgainAction(f, c, e)))

        case MenaceSwarmAgainAction(f, c, e) =>
            val l = f.hand.%(_.matches(c.cost))

            if (l.none || f.canAttack(c)(e).not)
                Repeat
            else
                Ask(f).group("Swarm".styled(f), "again in", c)
                    .each(l)(d => MenaceSwarmCardAction(f, c, e, d))
                    .skip(Repeat)

        case MenaceSwarmCardAction(f, c, e, d) =>
            f.hand --> d --> discard.quiet

            f.log("swarmed again with", d)

            ForceAction(BattleConfirmAction(f, f, f, NoMessage, c, e, None, Repeat))

        case MenaceSwoopAction(f, l) =>
            Ask(f).each(l)(d => MenaceSwoopToAction(f, d)).cancel

        case MenaceSwoopToAction(f, d) =>
            Ask(f)
                .each(origins(f, d))(o => MenaceSwoopMoveAction(f, d, o, warriors(f, o)))
                .each(origins(f, d).%(warriors(f, _) > 1))(o => MenaceSwoopMoveAction(f, d, o, 1))
                .cancel

        case MenaceSwoopMoveAction(f, d, o, n) =>
            take(f, n, o)
            place(f, n, d)

            f.log("swooped", n.of(Bat.name).styled(f), "from", o.elem, "to", d.elem)

            MenaceSwoopMoreAction(f, d)

        case MenaceSwoopMoreAction(f, d) =>
            val l = origins(f, d)

            if (l.none)
                MenaceSwoopDoneAction(f, d)
            else
                Ask(f).group("Swoop".styled(f), "to", d.elem)
                    .each(l)(o => MenaceSwoopMoveAction(f, d, o, warriors(f, o)))
                    .each(l.%(warriors(f, _) > 1))(o => MenaceSwoopMoveAction(f, d, o, 1))
                    .done(MenaceSwoopDoneAction(f, d))

        case MenaceSwoopDoneAction(f, d) =>
            guano(f, d)

            MenaceDoneAction(f, Repeat)

        case MenaceDoneAction(f, then) =>
            f.acted += 1

            then

        case EveningNAction(50, f : WingedMenace) =>
            XCraftMainAction(f)

        case NightStartAction(f : WingedMenace) =>
            EveningDrawAction(f, 1 + f.drawBonus)

        case FactionCleanUpAction(f : WingedMenace) =>
            f.acted = 0
            f.spawned = false
            f.explored = false
            f.scatter = None
            f.scattered = $
            f.scatterDone = false

            CleanUpAction(f)

        case _ => UnknownContinue
    }

}
