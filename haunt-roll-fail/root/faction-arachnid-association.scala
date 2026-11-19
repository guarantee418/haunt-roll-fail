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

// Arachnid Association ii (chemicalreaction, art by SuPaCatOnE; from its faction board and Advanced Setup card in the Root Ultimate
// Collection TTS mod, as there is no rulebook): spiders weaving webs, capturing the woodland's warriors on them and consuming them,
// laying eggs that hatch more spiders, and frightening the others out of the clearings they rule.
//
// Readings (the board leaves these open):
// - Adult spiders and spiderlings are both warriors; a move takes at most one adult spider (and any spiderlings)
// - Webs: 15 tokens, at most 4 in a clearing and 1 on a path; a web in a clearing holding a captive is drawn as a full web. Webs on
//   paths are never hit or removed. With none left in the supply, an empty web already in play is moved (the first one found)
// - Captured warriors are taken off the map (they don't rule and can't act) and go back to their owner's supply when consumed. A full
//   web removed from a clearing puts its warrior back in that clearing
// - Hunters: attacking, each enemy warrior (the basic warrior of a faction, not a hireling's) its hits remove is captured instead,
//   while a free web is in the clearing or on a path next to it
// - Webs on Paths: an enemy moving its warriors along a path with an empty web leaves one of its warriors there, captured
// - Consume (Birdsong): every captive on a web in a clearing with an adult spider, or on a path next to one, is consumed, one at a time,
//   each for 2 VP, 2 spiderlings in that clearing, or a mutation picked from those not yet taken
// - Incubation: hatching flips the egg and places up to 3 spiderlings and 1 adult spider there, scoring 3
// - Daylight: 3 actions, and 1 more for each bird card spent (spent at any time in Daylight)
// - Weave: at most as many times a turn as there are adult spiders on the map; its webs go in a clearing with an adult spider or on a
//   path next to one; the card for its additional web matches that web's clearing, or is any card for a path
// - Craft: one card for each Craft action; each clearing counts its webs or its egg, whichever is more; Weavers: no items
// - Lay: the eggs on the map (hatched or not) say which space of the track comes next: 2 webs and 1 incubation marker, then 3 and 2,
//   then 4 and 3; a removed egg goes back to the track. Each of the first two spaces uncovers one more card drawn in Evening
// - Capture (Evening): its clearing has no adult spider; the warrior goes on a free web in a clearing next to it with an adult spider
// - Predation: a clearing with 4 spiderlings counts as having an adult spider for Capture and Consume
// - Arachnophobia: after the hand limit (and its discard when adults outnumber webs somewhere), each opponent either gives the
//   Association a card or, in each clearing the Association rules, moves 1 of its warriors (half, rounded up, with Terror) to the
//   clearing next to it where it has the most warriors (the first such clearing when tied)
// - Agility: moves from a clearing to another clearing of the same forest, without stopping in the forest
// - Lures: defending, a hit that would remove an adult spider removes a web there instead
// - Setup: 3 adult spiders, 3 spiderlings and 3 webs in a corner clearing without an opponent's piece; with Advanced Setup, its card:
//   a homeland on the map's edge (not one of the inner clearings) not adjacent to another homeland

case object AdultSpider extends Warrior {
    override def id = "adult"
    override def name = "Adult Spider"
}

case object Spiderling extends Warrior {
    override def id = "spiderling"
    override def name = "Spiderling"
}

case object SpiderWeb extends Token {
    override def id = "web"
    override def name = "Web"
}

case object SpiderWebFull extends Token {
    override def id = "web-full"
    override def name = "Web"
}

case object SpiderEgg extends Token {
    override def id = "egg"
    override def name = "Egg"
}

case object SpiderHatched extends Token {
    override def id = "hatched"
    override def name = "Hatched Egg"
}

case object WebPaths extends SpecialRegion {
    override def name = "Webs on Paths"
    override def elem(implicit game : Game) : Elem = "Webs on Paths".hh
}

case object Cocoons extends SpecialRegion {
    override def name = "Webs"
    override def elem(implicit game : Game) : Elem = "Webs".hh
}

case object Arachnophobia extends FactionEffect {
    val name = "Arachnophobia"
}

case object Hunters extends FactionEffect {
    val name = "Hunters"
}

case object Weavers extends FactionEffect {
    val name = "Weavers"
}

trait AAMutation extends FactionEffect {
    def text : String
}

case object AAFrenzy extends AAMutation {
    val name = "Frenzy"
    val text = "2 webs for each Weave"
}

case object AAAggressiveness extends AAMutation {
    val name = "Aggressiveness"
    val text = "Capture twice in Evening"
}

case object AALures extends AAMutation {
    val name = "Lures"
    val text = "defending, webs are removed before adult spiders"
}

case object AATerror extends AAMutation {
    val name = "Terror"
    val text = "half the warriors, rounded up, flee from Arachnophobia"
}

case object AADissolution extends AAMutation {
    val name = "Dissolution"
    val text = "empty webs removed by opponents score nothing"
}

case object AAGregariousness extends AAMutation {
    val name = "Gregariousness"
    val text = "up to 2 moves for each Move"
}

case object AAAgility extends AAMutation {
    val name = "Agility"
    val text = "move through forests"
}

case object AAPredation extends AAMutation {
    val name = "Predation"
    val text = "4 spiderlings Capture and Consume like an adult spider"
}

object AAMutation {
    val all = $[AAMutation](AAFrenzy, AAAggressiveness, AALures, AATerror, AADissolution, AAGregariousness, AAAgility, AAPredation)
}

// a move takes at most one adult spider
case object SpiderMove extends Transport {
    override def allows(f : Faction, o : Region, d : Region, l : $[Movable])(implicit game : Game) = l.count(AdultSpider) <= 1
}

// Agility: from a clearing to another clearing of the same forest
case object SpiderForest extends Transport {
    override def allows(f : Faction)(implicit game : Game) = f @@ {
        case f : ArachnidAssociation => game.states.contains(f) && game.current == f && f.mutations.has(AAAgility)
        case _ => false
    }

    override def allows(f : Faction, o : Region, d : Region)(implicit game : Game) = allows(f, o) && (o, d) @@ {
        case (o : Clearing, d : Clearing) => o != d && game.board.forests.exists(x => game.fromForest(x).use(l => l.has(o) && l.has(d)))
        case _ => false
    }
}

trait ArachnidAssociation extends WarriorFaction {
    val clashKey = AA

    val warrior = Spiderling

    override val transports : $[$[Transport]] = $($(RuledMove, SpiderMove))

    def abilities(options : $[Meta.O]) = $(Arachnophobia, Hunters, Weavers)

    def pieces(options : $[Meta.O]) = AdultSpider *** 5 ++ Spiderling *** 15 ++ SpiderWeb *** 15 ++ SpiderWebFull *** 15 ++ SpiderEgg *** 3 ++ SpiderHatched *** 3

    override def note : Elem = SpacedDash ~ "by " ~ "chemicalreaction".hl ~ HorizontalBreak ~ "fan-made"

    def advertising = AdultSpider.img(this) ~ SpiderWeb.img(this) ~ SpiderEgg.img(this)

    def motto = "Hunt".styled(this)

    override def initialDamagePriorities(options : $[Meta.O]) = $(Spiderling, AdultSpider, SpiderWeb, SpiderWebFull, SpiderEgg, SpiderHatched)
}

case object AA extends ArachnidAssociation {
    val name = "Arachnid Association"
    val short = "AA"
    val style = "aa"
    val priority = "V'''''''''"
}

trait AADaylightQuestion extends FactionAction {
    override def self : ArachnidAssociation

    def question(implicit game : Game) = self.elem ~ SpacedDash ~ Daylight.elem ~ Break ~
        Div(
            1.to(self.actions)./(_ => Image("action-black", styles.action, "")).take(self.acted) ~
            1.to(self.actions)./(_ => Image(self.style + "-action", styles.action)).drop(self.acted),
            styles.margined)
}

case class AASetupAction(self : ArachnidAssociation, c : Clearing) extends BaseAction(self, "starts in")(c)

case class AAConsumeAction(self : ArachnidAssociation) extends ForcedAction
case class AAConsumeVPAction(self : ArachnidAssociation, u : Figure) extends BaseAction("Consume".styled(self), u.piece.of(u.faction))("Score".hh, 2.vp)
case class AAConsumeSpawnAction(self : ArachnidAssociation, u : Figure, c : Clearing) extends BaseAction("Consume".styled(self), u.piece.of(u.faction))("Place", 2.hl, Spiderling.sof(self), "in", c)
case class AAConsumeMutateAction(self : ArachnidAssociation, u : Figure, m : AAMutation) extends BaseAction("Consume".styled(self), u.piece.of(u.faction))("Mutation".hh, m.of(self), "(" ~ m.text ~ ")")
case class AAIncubateAction(self : ArachnidAssociation) extends ForcedAction

case class AABattleAction(self : ArachnidAssociation, l : $[Clearing]) extends OptionAction("Battle".styled(self), dt.Battle) with AADaylightQuestion with Soft
case class AAMoveAction(self : ArachnidAssociation) extends OptionAction("Move".styled(self), dt.Move) with AADaylightQuestion with Soft with Only[MoveListAction] { def tag = implicitly }
case class AAWeaveAction(self : ArachnidAssociation) extends OptionAction("Weave".styled(self), SpiderWeb.imgd(self)) with AADaylightQuestion with Soft
case class AAWeaveStartAction(self : ArachnidAssociation, c : Clearing, o : |[Clearing]) extends BaseAction("Weave".styled(self), "a", SpiderWeb.of(self))((g : Game) => o./(x => c.elem(g) ~ " - " ~ x.elem(g)).|(c.elem(g)))
case class AAWeaveFreeAction(self : ArachnidAssociation, c : Clearing, o : |[Clearing]) extends BaseAction(AAFrenzy.of(self), "weaves another", SpiderWeb.of(self))((g : Game) => o./(x => c.elem(g) ~ " - " ~ x.elem(g)).|(c.elem(g)))
case class AAWeaveMoreAction(self : ArachnidAssociation) extends ForcedAction
case class AAWeaveCardAction(self : ArachnidAssociation, c : Clearing, o : |[Clearing], d : DeckCard) extends BaseAction("Weave".styled(self), "one more", SpiderWeb.of(self), "in", (g : Game) => o./(x => c.elem(g) ~ " - " ~ x.elem(g)).|(c.elem(g)), "with")(d.img) with ViewCard
case class AACraftAction(self : ArachnidAssociation) extends OptionAction("Craft".styled(self), dt.CraftSuit(AnySuit)) with AADaylightQuestion with Soft with Only[CraftAssignAction] { def tag = implicitly }
case class AALayAction(self : ArachnidAssociation, l : $[Clearing]) extends OptionAction("Lay".styled(self), SpiderEgg.imgd(self)) with AADaylightQuestion with Soft
case class AALayCardAction(self : ArachnidAssociation, c : Clearing, d : DeckCard) extends BaseAction("Lay".styled(self), "an", SpiderEgg.of(self), "in", c, "with")(d.img) with ViewCard
case class AABirdAction(self : ArachnidAssociation, d : DeckCard) extends BaseAction("Spend a bird card for one more action")(d.img) with ViewCard
case class AADoneAction(f : ArachnidAssociation, then : ForcedAction) extends ForcedAction

case class AACaptureAction(self : ArachnidAssociation) extends ForcedAction
case class AACaptureCardAction(self : ArachnidAssociation, c : Clearing, e : Faction, x : Clearing, d : DeckCard) extends BaseAction("Capture".styled(self), "a warrior of", e, "from", c, "to", x, "with")(d.img) with ViewCard

case class AAPhobiaAction(self : ArachnidAssociation, l : $[Faction]) extends ForcedAction
case class AAPhobiaPayAction(self : Faction, f : ArachnidAssociation, d : DeckCard, l : $[Faction]) extends BaseAction(Arachnophobia.of(f), "give", f, "a card")(d.img) with ViewCard
case class AAPhobiaFleeAction(self : Faction, f : ArachnidAssociation, l : $[Faction]) extends BaseAction(Arachnophobia.of(f))("Flee".hh, "from the clearings", f, "rules")
case class AADiscardAction(self : ArachnidAssociation, d : DeckCard) extends BaseAction("Discard a card", "(more", AdultSpider.sof(self), "than", SpiderWeb.sof(self), "in a clearing)")(d.img) with ViewCard

case class AATrapAction(f : Faction, t : ArachnidAssociation, o : Clearing, d : Clearing, then : ForcedAction) extends ForcedAction


class ArachnidAssociationPlayer(val faction : ArachnidAssociation)(implicit val game : Game) extends FactionState {
    val onPaths = location(WebPaths, u => u.faction == faction && u.piece == SpiderWeb)
    val cocoon = location(Cocoons, u => u.faction != faction && u.piece.is[Warrior])

    // the webs on paths, by their two clearings in board order
    var paths : $[(Clearing, Clearing)] = $
    // the captives: a clearing's web, or a path's web (its two clearings), and the warrior on it
    var held : $[(Clearing, |[Clearing], Figure)] = $
    // the incubation markers on each egg
    var incubation : Map[Clearing, Int] = Map()

    var mutations : $[AAMutation] = $

    var acted = 0
    var extra = 0
    var weaved = 0
    var captured = 0
    var phobia = false

    def actions = 3 + extra

    def webs(c : Clearing) = at(c).count(SpiderWeb) + at(c).count(SpiderWebFull)

    def eggs = all(SpiderEgg) ++ all(SpiderHatched)

    def websLeft = 15 - all(SpiderWeb).num - all(SpiderWebFull).num - paths.num

    def eggsLeft = 3 - eggs.num

    def hunter(c : Clearing) = at(c).has(AdultSpider) || (mutations.has(AAPredation) && at(c).count(Spiderling) >= 4)

    def craft = clearings./~(c => (max(webs(c), at(c).count(SpiderEgg) + at(c).count(SpiderHatched))).times(c.asset))

    def draws = 1 + min(2, eggs.num)
}


object ArachnidAssociationExpansion extends FactionExpansion[ArachnidAssociation] {
    def spiders(implicit game : Game) = factions.of[ArachnidAssociation].%(game.states.contains)

    def key(a : Clearing, b : Clearing)(implicit game : Game) = (board.clearings.indexOf(a) <= board.clearings.indexOf(b)).?((a, b)).|((b, a))

    def pathFull(f : ArachnidAssociation, k : (Clearing, Clearing))(implicit game : Game) = f.held.exists(h => h._2.any && (h._1, h._2.get) == k)

    // a free web for a captive taken in c: in the clearing, or on a path next to it
    def freeWeb(f : ArachnidAssociation, c : Clearing)(implicit game : Game) : |[(Clearing, |[Clearing])] =
        f.at(c).has(SpiderWeb).?((c, None)) || f.paths.filter(k => k._1 == c || k._2 == c).filterNot(k => pathFull(f, k)).headOption./(k => (k._1, Some(k._2)))

    // the basic warrior of a faction that can be captured
    def capturable(e : Faction, p : Piece) = e.is[Hireling].not && e.as[WarriorFaction].?(_.warrior == p)

    def capture(f : ArachnidAssociation, u : Figure, w : (Clearing, |[Clearing]))(implicit game : Game) {
        w._2 match {
            case None =>
                f.from(w._1) --> SpiderWeb --> f.reserve
                f.reserve --> SpiderWebFull --> w._1
            case Some(_) =>
        }

        u --> f.cocoon

        f.held :+= (w._1, w._2, u)
    }

    // the empty web taken off the map when a new one is needed and none is left in the supply
    def takeEmptyWeb(f : ArachnidAssociation, not : |[(Clearing, |[Clearing])])(implicit game : Game) : Boolean = {
        clearings.%(c => f.at(c).has(SpiderWeb)).%(c => not.has((c, None)).not).headOption match {
            case Some(c) =>
                f.from(c) --> SpiderWeb --> f.reserve
                true
            case None =>
                f.paths.filterNot(k => pathFull(f, k)).filter(k => not.has((k._1, Some(k._2))).not).headOption match {
                    case Some(k) =>
                        f.onPaths --> SpiderWeb --> f.reserve
                        f.paths :-= k
                        true
                    case None =>
                        false
                }
        }
    }

    def placeWeb(f : ArachnidAssociation, c : Clearing, o : |[Clearing])(implicit game : Game) {
        if (f.websLeft <= 0 && takeEmptyWeb(f, Some((c, o))).not)
            return

        o match {
            case None =>
                f.reserve --> SpiderWeb --> c
            case Some(o) =>
                f.reserve --> SpiderWeb --> f.onPaths
                f.paths :+= key(c, o)
        }
    }

    // the places a web can be woven: a clearing with an adult spider and fewer than 4 webs, or an empty path next to one
    def weavable(f : ArachnidAssociation)(implicit game : Game) : $[(Clearing, |[Clearing])] = {
        val aa = clearings.%(c => f.at(c).has(AdultSpider))

        aa.%(c => f.webs(c) < 4)./(c => (c, None : |[Clearing])) ++
        aa./~(c => game.connected(c)./(d => key(c, d))).distinct.filterNot(f.paths.has).map(k => (k._1, Some(k._2) : |[Clearing]))
    }

    def weaveOk(f : ArachnidAssociation)(implicit game : Game) = f.websLeft > 0 || clearings.exists(c => f.at(c).has(SpiderWeb)) || f.paths.exists(k => pathFull(f, k).not)

    def layable(f : ArachnidAssociation)(implicit game : Game) =
        (f.eggsLeft > 0).??(clearings.%(c => f.at(c).has(AdultSpider)).%(c => f.at(c).has(SpiderEgg).not && f.at(c).has(SpiderHatched).not).%(f.canPlace).%(c => f.webs(c) >= 2 + f.eggs.num).%(c => f.hand.exists(_.matches(c.cost))))

    def consumable(f : ArachnidAssociation)(implicit game : Game) = f.held.%{ case (c, o, _) => f.hunter(c) || o.?(f.hunter) }

    def captures(f : ArachnidAssociation)(implicit game : Game) : $[(Clearing, Faction, Clearing)] =
        clearings.%(c => f.at(c).has(AdultSpider).not)./~(c =>
            factions.but(f).%(_.is[WarriorFaction]).%(f.friends(_).not).%(e => capturable(e, e.as[WarriorFaction].get.warrior) && e.at(c).has(e.as[WarriorFaction].get.warrior))./~(e =>
                game.connected(c).%(f.hunter).%(x => f.at(x).has(SpiderWeb))./(x => (c, e, x))))

    // Arachnophobia: the clearings each opponent's warriors flee
    def frightened(f : ArachnidAssociation, e : Faction)(implicit game : Game) = clearings.%(f.rules).%(c => e.at(c).of[Warrior].any)

    def phobic(f : ArachnidAssociation)(implicit game : Game) = factions.but(f).%(f.friends(_).not).%(e => frightened(f, e).any)

    def perform(action : Action, soft : Void)(implicit game : Game) = action @@ {
        // SETUP
        case CreatePlayerAction(f : ArachnidAssociation) =>
            game.states += f -> new ArachnidAssociationPlayer(f)

            FactionInitAction(f)

        case FactionSetupAction(f : ArachnidAssociation) if options.has(SetupTypeHomelands) =>
            val all = clearings.diff(board.inner).%(f.canPlace).diff(game.homelands)
            val l = all.%(c => game.connected(c).intersect(game.homelands).none).some.||(all.some).|(clearings.%(f.canPlace).diff(game.homelands))

            Ask(f).each(l)(c => AASetupAction(f, c)).needOk

        case FactionSetupAction(f : ArachnidAssociation) =>
            val all = board.corners.%(f.canPlace)
            val l = all.%(c => factions.but(f).forall(_.at(c).none)).some.||(all.some).|(clearings.%(f.canPlace))

            Ask(f).each(l)(c => AASetupAction(f, c)).needOk

        case AASetupAction(f, c) =>
            game.homelands :+= c

            f.reserve --> 3.times(AdultSpider) --> c
            f.reserve --> 3.times(Spiderling) --> c
            f.reserve --> 3.times(SpiderWeb) --> c

            f.log("placed", 3.hl, AdultSpider.sof(f) ~ ",", 3.hl, Spiderling.sof(f), "and", 3.hl, SpiderWeb.sof(f), "in", c)

            SetupFactionsAction

        // HUNTERS: an attacker's hits capture enemy warriors on its free webs
        case BattleDealHitAction(e, b, x, _, then) if b.attacker.is[ArachnidAssociation] && game.states.contains(b.attacker) && x.faction == e && e != b.attacker && capturable(e, x.piece) && e.from(b.clearing).$.has(x) && freeWeb(b.attacker.as[ArachnidAssociation].get, b.clearing).any =>
            val f = b.attacker.as[ArachnidAssociation].get
            val w = freeWeb(f, b.clearing).get

            capture(f, x, w)

            f.log("captured", x.piece.of(e), "on a", SpiderWeb.of(f), "with", Hunters.of(f))

            then

        // LURES: defending, a web goes before an adult spider
        case BattleDealHitAction(f : ArachnidAssociation, b, x, _, then) if f == b.defender && x.faction == f && x.piece == AdultSpider && f.mutations.has(AALures) && (f.at(b.clearing).has(SpiderWeb) || f.at(b.clearing).has(SpiderWebFull)) =>
            val p = f.at(b.clearing).has(SpiderWeb).?(SpiderWeb).|(SpiderWebFull)
            val w = f.from(b.clearing).$.%(_.piece == p).head

            f.log("lured the hit to a", SpiderWeb.of(f), "with", AALures.of(f))

            BattleDealHitAction(f, b, w, 0, then)

        // a full web removed puts its warrior back in the clearing
        case ForcedRemoveTargetEffectAction(e, c, f : ArachnidAssociation, SpiderWebFull, then) =>
            f.held.%(h => h._1 == c && h._2.none).headOption.foreach { h =>
                f.held :-= h

                h._3 --> c

                f.log("freed", h._3.piece.of(h._3.faction), "in", c)
            }

            then

        case ForcedRemoveTargetEffectAction(e, c, f : ArachnidAssociation, SpiderEgg, then) =>
            f.incubation -= c

            then

        // EGGS: an opponent removing one scores 3
        case ForcedRemoveScoreAction(e, c, f : ArachnidAssociation, p, n, m, then) if e != f && (p == SpiderEgg || p == SpiderHatched) && n != 3 =>
            ForcedRemoveScoreAction(e, c, f, p, 3, m, then)

        // DISSOLUTION
        case ForcedRemoveScoreAction(e, c, f : ArachnidAssociation, SpiderWeb, n, m, then) if e != f && n > 0 && f.mutations.has(AADissolution) =>
            ForcedRemoveScoreAction(e, c, f, SpiderWeb, 0, m, then)

        // WEBS ON PATHS
        case MoveListAction(self, e, t, m, o : Clearing, d : Clearing, l, then) if t.has(Roads) && e.is[ArachnidAssociation].not && e.is[WarriorFaction] && capturable(e, e.as[WarriorFaction].get.warrior) && l.has(e.as[WarriorFaction].get.warrior) && spiders.%(_.friends(e).not).exists(f => f.paths.has(key(o, d)) && pathFull(f, key(o, d)).not) && then.is[AATrapAction].not =>
            val f = spiders.%(_.friends(e).not).%(f => f.paths.has(key(o, d)) && pathFull(f, key(o, d)).not).head

            Force(MoveListAction(self, e, t, m, o, d, l, AATrapAction(e, f, o, d, then)))

        case AATrapAction(e, f, o, d, then) =>
            val k = key(o, d)

            e.as[WarriorFaction].foreach { w =>
                e.from(d).$.%(_.piece == w.warrior).headOption.foreach { u =>
                    if (f.paths.has(k) && pathFull(f, k).not) {
                        capture(f, u, (k._1, Some(k._2)))

                        f.log("captured", w.warrior.of(e), "on the", SpiderWeb.of(f), "between", o, "and", d)
                    }
                }
            }

            then

        // BIRDSONG
        case BirdsongNAction(30, f : ArachnidAssociation) =>
            AAConsumeAction(f)

        case AAConsumeAction(f) =>
            val l = consumable(f)

            if (l.none)
                Next
            else {
                val (c, o, u) = l.head
                val at : Clearing = if (f.hunter(c)) c else o.get

                Ask(f).group("Consume".styled(f), u.piece.of(u.faction), "on a", SpiderWeb.of(f), o./(o => "between " ~ c.elem ~ " and " ~ o.elem).|("in " ~ c.elem))
                    .add(AAConsumeVPAction(f, u))
                    .add(AAConsumeSpawnAction(f, u, at).!(f.pool(Spiderling).not, "no spiderlings").!(f.canPlace(at).not, "can't place"))
                    .each(AAMutation.all.diff(f.mutations))(m => AAConsumeMutateAction(f, u, m))
                    .birdsong(f)
            }

        case AAConsumeVPAction(f, u) =>
            f.held = f.held.%(_._3 != u)
            u --> game.pool

            f.nscore(2)("consuming", u.piece.of(u.faction))(f, "consumed", u.piece.of(u.faction), ForVP)

            AAConsumeEmptyAction(f, AAConsumeAction(f))

        case AAConsumeSpawnAction(f, u, c) =>
            f.held = f.held.%(_._3 != u)
            u --> game.pool

            val n = min(2, f.pooled(Spiderling))
            f.reserve --> n.times(Spiderling) --> c

            f.log("consumed", u.piece.of(u.faction), "and placed", n.hl, Spiderling.sof(f), "in", c)

            AAConsumeEmptyAction(f, AAConsumeAction(f))

        case AAConsumeMutateAction(f, u, m) =>
            f.held = f.held.%(_._3 != u)
            u --> game.pool

            f.mutations :+= m

            f.log("consumed", u.piece.of(u.faction), "and mutated", m.of(f))

            AAConsumeEmptyAction(f, AAConsumeAction(f))

        // the full webs in clearings with no captive left are emptied
        case AAConsumeEmptyAction(f, then) =>
            clearings.foreach { c =>
                val full = f.at(c).count(SpiderWebFull)
                val held = f.held.count(h => h._1 == c && h._2.none)

                if (full > held) {
                    f.from(c) --> (full - held).times(SpiderWebFull) --> f.reserve
                    f.reserve --> (full - held).times(SpiderWeb) --> c
                }
            }

            then

        case BirdsongNAction(50, f : ArachnidAssociation) =>
            AAIncubateAction(f)

        case AAIncubateAction(f) =>
            f.all(SpiderEgg).foreach { c =>
                val n = f.incubation.getOrElse(c, 1) - 1

                if (n > 0) {
                    f.incubation += c -> n

                    f.log("incubated the", SpiderEgg.of(f), "in", c, "(" ~ n.hl ~ " left)")
                }
                else {
                    f.incubation -= c

                    f.from(c) --> SpiderEgg --> f.reserve
                    f.reserve --> SpiderHatched --> c

                    val s = min(3, f.pooled(Spiderling))
                    f.reserve --> s.times(Spiderling) --> c

                    val a = min(1, f.pooled(AdultSpider))
                    f.reserve --> a.times(AdultSpider) --> c

                    f.nscore(3)("hatching an egg")(f, "hatched the", SpiderEgg.of(f), "in", c, "placing", s.hl, Spiderling.sof(f), (a > 0).?("and " ~ AdultSpider.of(f)), ForVP)
                }
            }

            Next

        // DAYLIGHT
        case DaylightNAction(50, f : ArachnidAssociation) =>
            implicit val ask = builder

            if (f.acted < f.actions) {
                val bb = clearings.%(f.canAttackIn)
                + AABattleAction(f, bb).!(bb.none)

                + AAMoveAction(f).!(f.moveFrom.none)

                + AAWeaveAction(f).!(f.weaved >= f.all(AdultSpider).num, "once per adult").!(weavable(f).none || weaveOk(f).not)

                + AACraftAction(f).!(f.hand.exists(f.craftable).not)

                val ll = layable(f)
                + AALayAction(f, ll).!(f.eggsLeft <= 0, "max").!(ll.none)
            }

            f.hand.%(_.suit == Bird).foreach { d =>
                + AABirdAction(f, d)
            }

            + EndTurnSoftAction(f, "Actions", ForfeitActions(f.actions - f.acted))

            ask(f).daylight(f)

        case AABirdAction(f, d) =>
            f.hand --> d --> discard.quiet

            f.extra += 1

            f.log("spent", d, "for one more action")

            Repeat

        case AABattleAction(f, l) =>
            BattleInitAction(f, f, NoMessage, l, $(CancelAction), AADoneAction(f, Repeat))

        case AAMoveAction(f) =>
            if (f.mutations.has(AAGregariousness))
                MarchAction(f, 1, 2, AADoneAction(f, Repeat))
            else
                MoveInitAction(f, f, $, NoMessage, f.moveFrom, f.movable, $(CancelAction), AADoneAction(f, Repeat))

        case AAWeaveAction(f) =>
            Ask(f).each(weavable(f))((c, o) => AAWeaveStartAction(f, c, o)).cancel

        case AAWeaveStartAction(f, c, o) =>
            game.highlights :+= PlaceHighlight($(c) ++ o.$)

            f.acted += 1
            f.weaved += 1

            placeWeb(f, c, o)

            f.log("wove", SpiderWeb.of(f), o./(o => "between " ~ c.elem ~ " and " ~ o.elem).|("in " ~ c.elem))

            if (f.mutations.has(AAFrenzy) && weavable(f).any && weaveOk(f))
                Ask(f).each(weavable(f))((c, o) => AAWeaveFreeAction(f, c, o)).skip(AAWeaveMoreAction(f))
            else
                AAWeaveMoreAction(f)

        case AAWeaveFreeAction(f, c, o) =>
            placeWeb(f, c, o)

            f.log("wove", SpiderWeb.of(f), o./(o => "between " ~ c.elem ~ " and " ~ o.elem).|("in " ~ c.elem), "with", AAFrenzy.of(f))

            AAWeaveMoreAction(f)

        case AAWeaveMoreAction(f) =>
            if (f.hand.none || weavable(f).none || weaveOk(f).not)
                Repeat
            else
                Ask(f).group("Weave".styled(f), "one more", SpiderWeb.of(f), "with a card")
                    .some(weavable(f))((c, o) => f.hand.%(d => o.any || d.matches(c.cost))./(d => AAWeaveCardAction(f, c, o, d)))
                    .done(Repeat)
                    .daylight(f)

        case AAWeaveCardAction(f, c, o, d) =>
            f.hand --> d --> discard.quiet

            placeWeb(f, c, o)

            f.log("wove another", SpiderWeb.of(f), o./(o => "between " ~ c.elem ~ " and " ~ o.elem).|("in " ~ c.elem), "with", d)

            Repeat

        case AACraftAction(f) =>
            CraftMenuAction(f)

        // a Craft action
        case CraftAssignAction(f : ArachnidAssociation, d, a, used, m, CraftPerformAction(ff, dd, mm)) =>
            f.crafted ++= used

            f.acted += 1

            CraftPerformAction(ff, dd, mm)

        case AALayAction(f, l) =>
            Ask(f).some(l)(c => f.hand.%(_.matches(c.cost))./(d => AALayCardAction(f, c, d))).cancel

        case AALayCardAction(f, c, d) =>
            game.highlights :+= PlaceHighlight($(c))

            val n = 1 + f.eggs.num

            f.hand --> d --> discard.quiet

            f.reserve --> SpiderEgg --> c

            f.incubation += c -> n

            f.log("laid", SpiderEgg.of(f), "in", c, "with", d, "(" ~ n.hl ~ " incubation)")

            AADoneAction(f, Repeat)

        case AADoneAction(f, then) =>
            f.acted += 1

            then

        // EVENING
        case EveningNAction(20, f : ArachnidAssociation) =>
            AACaptureAction(f)

        case AACaptureAction(f) =>
            soft()

            val l = captures(f)
            val n = f.mutations.has(AAAggressiveness).?(2).|(1)

            if (f.captured >= n || l.none || f.hand.none)
                NoAsk(f)(Next)
            else
                Ask(f).group("Capture".styled(f), (n > 1).?("(" ~ (f.captured + 1).hl ~ "/" ~ n.hl ~ ")"))
                    .some(l)(t => f.hand.%(_.matches(t._1.cost))./(d => AACaptureCardAction(f, t._1, t._2, t._3, d)))
                    .done(Next)
                    .evening(f)

        case AACaptureCardAction(f, c, e, x, d) =>
            f.hand --> d --> discard.quiet

            f.captured += 1

            e.as[WarriorFaction].foreach { w =>
                e.from(c).$.%(_.piece == w.warrior).headOption.foreach { u =>
                    capture(f, u, (x, None))
                }

                f.log("captured", w.warrior.of(e), "from", c, "on a", SpiderWeb.of(f), "in", x, "with", d)
            }

            AACaptureAction(f)

        case NightStartAction(f : ArachnidAssociation) =>
            EveningDrawAction(f, f.draws)

        case EveningAfterHandLimitAction(f : ArachnidAssociation) if f.phobia.not =>
            f.phobia = true

            if (f.hand.any && clearings.exists(c => f.at(c).count(AdultSpider) > f.webs(c)))
                Ask(f).each(f.hand)(d => AADiscardAction(f, d)).evening(f)
            else
                AAPhobiaAction(f, phobic(f))

        case AADiscardAction(f, d) =>
            f.hand --> d --> discard.quiet

            f.log("discarded a card, with more", AdultSpider.sof(f), "than", SpiderWeb.sof(f), "in a clearing")

            AAPhobiaAction(f, phobic(f))

        // ARACHNOPHOBIA
        case AAPhobiaAction(f, Nil) =>
            EveningAfterHandLimitAction(f)

        case AAPhobiaAction(f, e :: l) =>
            if (frightened(f, e).none)
                AAPhobiaAction(f, l)
            else
            if (e.hand.none)
                Ask(e).add(AAPhobiaFleeAction(e, f, l))
            else
                Ask(e).group(Arachnophobia.of(f), "give", f, "a card, or flee")
                    .each(e.hand)(d => AAPhobiaPayAction(e, f, d, l))
                    .add(AAPhobiaFleeAction(e, f, l))
                    .needOk

        case AAPhobiaPayAction(e, f, d, l) =>
            e.hand --> d --> f.hand

            e.log("gave", f, "a card for", Arachnophobia.of(f))

            AAPhobiaAction(f, l)

        case AAPhobiaFleeAction(e, f, l) =>
            frightened(f, e).foreach { c =>
                val ww = e.at(c).of[Warrior]
                val w = e.as[WarriorFaction]./(x => ww.sortBy(p => (p != x.warrior).??(1))).|(ww)
                val n = f.mutations.has(AATerror).?((ww.num + 1) / 2).|(1)
                val dd = game.connected(c)

                if (dd.any) {
                    val d = dd.maxBy(x => e.at(x).of[Warrior].num)

                    e.from(c) --> w.take(n) --> d

                    e.log("fled from", c, "to", d, "with", w.take(n)./(_.of(e)).comma, "for", Arachnophobia.of(f))
                }
            }

            AAPhobiaAction(f, l)

        case FactionCleanUpAction(f : ArachnidAssociation) =>
            f.acted = 0
            f.extra = 0
            f.weaved = 0
            f.captured = 0
            f.phobia = false

            CleanUpAction(f)

        case _ => UnknownContinue
    }

}

case class AAConsumeEmptyAction(self : ArachnidAssociation, then : ForcedAction) extends ForcedAction
