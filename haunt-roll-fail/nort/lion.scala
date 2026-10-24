package nort
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

import nort.elem._


// The Lion Kingdom (fan-made, LionKingdom in options.scala): the video game's Kingdom of Neustria as a clan, from the design
// Gman asked for on 2026-10-21 (Claude Doc "Lion Kingdom for Uncharted Lands") with Robotos's Chapel and alternative starting
// deck (Discord, "Generic Kingdom", 2026-10-21). On whenever the Lion plays, like New Blood. RULES.md (Lion Kingdom) has the rules.
//
// Clan power, Forts and Lords: 3 Lord tokens. When the Lion builds a Defense Tower and has a Lord in reserve, the Lord goes on it.
// Defending its territory, each Lord there adds 2 combat points. Lords never move and never take casualties, aren't units and
// don't hold a territory: a Lord goes back to the reserve when its tower is removed or the Lion loses the territory.
// Chapel: the Lion builds Chapels instead of Carved Stones, on Carved Stone spaces; each gives 1 fame at harvest.

// Lion Kingdom: Recruit 1, and 1 more in the same territory for each lore paid (up to 2)
case object LionClanEffect extends Effect
// Rise of the Twelve Peers: Recruit 1 in up to 2 territories with a Defense Tower or Training Camp, then may place a Lord
case object PeersEffect extends Effect
// Abundance: Build, then collect the territory's resources if it has 3 or more buildings
case object AbundanceBuild extends BuildSpecial
// Hildegard's Vow: Move 2, +2 combat points for each Lord next to the fight
case object VowMove extends MoveSpecial

// The Lion's own starting deck (LionOwnDeck, or LionStartingDeck in older games): each card offers two actions
// Adventure: Explore or Move 1
case object AdventureEffect extends Effect
// Toil: Build, or collect 1 food or 1 wood
case object ToilEffect extends Effect
// Prey: Recruit 1, or gain 1 fame
case object PreyEffect extends Effect
// Prey's second choice
case object LionFameEffect extends Effect

// Rise of the Twelve Peers: different territories f controls with a Defense Tower or Training Camp
case object RecruitPeers extends RecruitMode

// The Lion's Chapel: a small building on a Carved Stone space, 1 fame at harvest; not in Building.all, so games without the Lion
// never offer it (MineralsExpansion.buildings adds it)
case object Chapel extends Building { val large = false }


// Lion Kingdom: how much lore to pay for more units
case class LionPayAction(self : Faction, n : Int, then : ForcedAction) extends BaseAction("Lion Kingdom".hl, "recruit 1 unit, and 1 more in the same territory for each", Lore, "paid")(LionPayLabel(n))

case class LionPayLabel(n : Int) extends Elementary {
    def elem = (n == 0).?("Pay nothing, recruit 1".txt).|("Pay " ~ Amount(n.hl, Lore.elem) ~ ", recruit " ~ (n + 1).hl)
}

// Rise of the Twelve Peers: a Lord from the reserve onto a Defense Tower without one
case class PeersLordAction(f : Faction, then : ForcedAction) extends ForcedAction
case class LordPlaceAction(self : Faction, space : SpaceRef, then : ForcedAction) extends BaseAction("Rise of the Twelve Peers".hl, "put a", "Lord".hl, "on the", DefenseTower, "in")(space.area) with MapTarget { def target = space }

// The alternative starting cards' two choices
case class LionChoiceAction(self : Faction, card : String, e : Effect, then : ForcedAction) extends BaseAction(card.hl)(LionChoiceLabel(e))

case class LionChoiceLabel(e : Effect) extends GameElementary {
    def elem(implicit game : Game) = e match {
        case CollectEffect(r, n) => "Collect " ~ Amount(n.hl, r.elem)
        case LionFameEffect => "Gain " ~ Amount(1.hl, FameIcon())
        case e => FeastText(e).elem
    }
}


object LionExpansion extends Expansion {
    val lordTokens = 3

    def on(implicit game : Game) = game.factions.has(Lion)

    // The Lords on the map: the spaces of Defense Towers in the Lion's territories that hold one
    def lords(implicit game : Game) : $[SpaceRef] = game.lordSpaces.%(s => game.buildings.get(s).has(DefenseTower) && game.controlled(Lion).exists(_.areas.contains(s.area)))

    def reserve(implicit game : Game) : Int = lordTokens - lords.num

    def lordsIn(t : Territory)(implicit game : Game) : Int = lords.count(s => t.areas.contains(s.area))

    // The Lion's Defense Towers without a Lord
    def emptyTowers(implicit game : Game) : $[SpaceRef] =
        game.controlled(Lion)./~(game.buildingsIn).%{ case (s, b) => b == DefenseTower && lords.has(s).not }.map(_._1)

    // Lords whose tower is gone or whose territory the Lion lost go back to the reserve
    def cleanup()(implicit game : Game) {
        if (game.lordSpaces.any)
            game.lordSpaces = lords
    }

    // A Defense Tower the Lion just built gets a Lord from the reserve
    def towerBuilt(f : Faction, s : SpaceRef)(implicit game : Game) {
        cleanup()
        if (f == Lion && reserve > 0 && game.lordSpaces.has(s).not) {
            game.lordSpaces :+= s
            f.log("put a", "Lord".hl, "on the", DefenseTower)
            ClanPowers.note(PowerNote(f, |(s.area), f.elem ~ " puts a " ~ "Lord".hl ~ " on each " ~ DefenseTower.elem ~ " it builds."))
            game.note("lion-lord")
        }
    }

    // Abundance: a territory with 3 or more buildings gives its resources as at harvest
    def abundance(f : Faction, t : Territory)(implicit game : Game) {
        if (game.buildingsIn(t).num >= 3) {
            MapExpansion.collect(f, t, "with " ~ "Abundance".hl)
            game.note("lion-abundance")
        }
    }

    // Combat points: each Lord defending its territory, and Hildegard's Vow's Lords next to the fight
    def pointParts(f : Faction, t : Territory, e : MoveEffect, attacking : Boolean)(implicit game : Game) : $[(Int, Elem)] =
        (f == Lion).??($(
            attacking.not.??(2 * lordsIn(t)) -> "Lords".hl,
            (attacking && e.special == VowMove).??(2 * lords.count(s => game.board.adjacent(t).exists(_._1.areas.contains(s.area)))) -> "Hildegard's Vow".hl
        ))

    // Hildegard: 1 lore for each casualty the enemy suffered in a fight she took part in (game.hildegard, set as the fight is resolved)
    def afterCombat(attacker : Faction, defender : Faction, aLost : Int, dLost : Int)(implicit game : Game) {
        if (game.hildegard) {
            val n = (attacker == Lion).?(dLost).|(aLost)
            if (n > 0) {
                Lion.lore += n
                Lion.log("collected", n.hl, Lore, "with", WarchiefElem(Lion))
                game.note("lion-hildegard")
            }
        }
        game.hildegard = false
    }

    // Chapels: 1 fame each at harvest (none where a Kobold is, like the Altar of Kings)
    def chapelFame(f : Faction)(implicit game : Game) : Int = (f == Lion).??(game.controlled(f).%(t => game.koboldIn(t).not)./~(game.working).count(_ == Chapel))

    // The buildings f may build: the Lion builds Chapels instead of Carved Stones
    def allowed(f : Faction, b : Building) : Boolean = (b != Chapel || f == Lion) && (b != CarvedStone || f != Lion)

    // The starting deck: the Lion's own (LionOwnDeck in every new game, LionStartingDeck in older games that picked it)
    def starting(f : Faction)(implicit game : Game) : $[Card] =
        if (f == Lion && (game.options.has(LionOwnDeck) || game.options.has(LionStartingDeck)))
            $("adventure", "adventure", "toil", "toil", "prey", "prey")./(StartCard(game.colors(f), _))
        else
            Cards.starting(game.colors(f))

    def peersTargets(f : Faction, placed : $[AreaRef])(implicit game : Game) : $[Territory] =
        game.controlled(f).%(t => game.working(t).exists(b => b == DefenseTower || b == TrainingCamp)).%(t => placed.forall(p => t.areas.contains(p).not))

    // The choices of an alternative starting card
    def choices(e : Effect) : $[Effect] = e match {
        case AdventureEffect => $(ExploreEffect(), MoveEffect(1))
        case ToilEffect => $(BuildEffect(), CollectEffect(Food, 1), CollectEffect(Wood, 1))
        case PreyEffect => $(RecruitEffect(1), LionFameEffect)
        case _ => $
    }

    def playable(f : Faction, e : Effect)(implicit game : Game) : Boolean = e match {
        case LionClanEffect => MapExpansion.playable(f, RecruitEffect(1))
        case PeersEffect => (MapExpansion.canRecruit(f) && peersTargets(f, $).any) || (reserve > 0 && emptyTowers.any)
        case AdventureEffect | ToilEffect | PreyEffect => choices(e).exists(CommonExpansion.playableEffect(f, _))
        case LionFameEffect => true
        case _ => false
    }

    def resolve(f : Faction, e : Effect, then : ForcedAction)(implicit game : Game) : Continue = e match {
        case LionClanEffect =>
            // Paying for units that can't be placed would be lore lost
            val max = math.min(2, math.min(f.lore, math.max(0, game.reserve(f) - 1)))
            if (max == 0)
                Then(RecruitAction(f, 1, RecruitSame, $, then))
            else
                Ask(f).each(0.to(max).$)(n => LionPayAction(f, n, then))

        case PeersEffect =>
            Then(RecruitAction(f, 2, RecruitPeers, $, PeersLordAction(f, then)))

        case AdventureEffect | ToilEffect | PreyEffect =>
            val name = e match {
                case AdventureEffect => "Adventure"
                case ToilEffect => "Toil"
                case _ => "Prey"
            }
            Ask(f).each(choices(e).%(CommonExpansion.playableEffect(f, _)))(c => LionChoiceAction(f, name, c, then))

        case LionFameEffect =>
            f.fame += 1
            f.log("gained", 1.hl, FameIcon())
            Then(then)

        case _ => Then(then)
    }

    def perform(action : Action, soft : Void)(implicit game : Game) = action @@ {
        // Lords follow the Lion's territories and towers, like Kraken's High Tide tokens follow its figures
        case MoveAction(_, _, _, _) | CombatsAction(_, _, _) | RetreatAction(_, _, _, _) | ReturnUnitsAction(_, _) =>
            cleanup()
            UnknownContinue

        // Whether Hildegard is in the fight being resolved (her lore comes after the casualties, which may take her)
        case CombatResolveAction(attacker, defender, a, _, _, _, _) if attacker == Lion || defender == Lion =>
            game.hildegard = game.chiefIn(game.board.territory(a), Lion)
            UnknownContinue

        case LionPayAction(f, n, then) =>
            if (n > 0) {
                f.lore -= n
                f.log("paid", n.hl, Lore, "to recruit", n.hl, (n == 1).?("more unit").|("more units"))
            }
            Then(RecruitAction(f, n + 1, RecruitSame, $, then))

        case PeersLordAction(f, then) =>
            cleanup()
            if (reserve > 0 && emptyTowers.any)
                Ask(f).each(emptyTowers)(s => LordPlaceAction(f, s, then)).add(then.as("Place no Lord")("Rise of the Twelve Peers".hl))
            else
                Then(then)

        case LordPlaceAction(f, s, then) =>
            game.lordSpaces :+= s
            f.log("put a", "Lord".hl, "on the", DefenseTower, "in", s.area)
            Then(then)

        case LionChoiceAction(f, _, e, then) =>
            Then(ResolveEffectAction(f, e, then))

        case _ => UnknownContinue
    }
}
