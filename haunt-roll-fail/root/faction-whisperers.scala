package root
//
//
//
//
import hrf.colmat._
import hrf.logger._
import hrf.tracker.Location
//
//
//
//

import hrf.elem._
import root.elem._

// The Doomed Expansion's Whisperers Guild (slugfacekillah, v1.2; Law of Doom, section WG, and its faction board), a faction only for
// games of 5 or more players: hidden in the forests, it scripts the story with face-down Insight markers, reveals them on other
// players' turns when they do what the marker foretold (1 point each), deals the cards around the table, gains warriors when enemies
// spend its Favored Suit, and acts between other players' turns (Midnight).
//
// Readings (the rules leave these open, or the site can't do them as written):
// - Insight is revealed after the action that set it off, before the next step of that player's turn (the site can't stop an action
//   halfway), and at most once for each thing that happened; skipping it skips every Insight offered at that moment
// - Build: set off when an enemy building appears in the clearing; the face-up Build marker is a building of the Guild that takes no
//   building slot and is battled like any other building; Deeply Rooted counts each clearing with one as ruled for the hand limit
// - Craft: set off when an enemy crafts with a crafting suit matching the clearing while it has pieces there; the marker becomes Salvage
//   of its printed suit, which goes back to the supply once spent
// - Martyr and Carnage: set off by an enemy removing the Guild's warriors (Martyr) or a third player's pieces (Carnage) there; Martyr
//   takes a card matching the clearing from that enemy's hand, Carnage places a warrior from the supply (or the Favored box) there
// - Favored Suit: counted for cards of exactly that suit an enemy discards from hand (spending included), not for cards only revealed;
//   the warriors go to the Favored box at once, and at Regroup each one may instead remove a face-down Insight marker
// - Midnight: offered at the start of each turn of another player after another player's turn; MidnightInstigate's battle is the first
//   player's against the second, and the points for what it removes are the Guild's; MidnightInfiltrate moves to an adjacent clearing
// - Slink moves any number of the Guild's warriors from one place to an adjacent one each time; forests are adjacent across a path
// - Setup: the first faction set up that has a building puts the Guild's face-up Build marker under one of them (the Guild picks it)

case object GuildWhisperer extends Warrior {
    override def name = "Whisperer"
}

case object WGLair extends Building {
    override def name = "Build Marker"
}

case object HiddenInsight extends Pawn {
    override def name = "Insight"
}

case object WGFavored extends SpecialRegion

case object HiddenInsightEffect extends FactionEffect {
    val name = "Hidden Insight"
}

case object DeeplyRooted extends FactionEffect {
    val name = "Deeply Rooted"
}

case object MidnightMoves extends FactionEffect {
    val name = "Midnight Moves"
}

trait InsightKind extends Record with Elementary {
    def name : String
    def id : String
    def elem = name.styled(styles.hit)
}

case object InsightBuild extends InsightKind { val name = "Build" ; val id = "build" }
case object InsightMartyr extends InsightKind { val name = "Martyr" ; val id = "martyr" }
case object InsightCarnage extends InsightKind { val name = "Carnage" ; val id = "carnage" }
case class InsightCraft(s : BaseSuit) extends InsightKind {
    def name = "Craft (" + s.name + ")"
    def id = "craft-" + s.name.toLowerCase
    override def elem = ("Craft (" + s.name + ")").styled(styles.hit)
}

trait MidnightKind extends Record with Elementary {
    def name : String
    def elem = name.styled(styles.hit)
}

case object MidnightAssassinate extends MidnightKind { val name = "Assassinate" }
case object MidnightInstigate extends MidnightKind { val name = "Instigate" }
case object MidnightInfiltrate extends MidnightKind { val name = "Infiltrate" }
case object MidnightInterrogate extends MidnightKind { val name = "Interrogate" }
case object MidnightManipulate extends MidnightKind { val name = "Manipulate" }

trait Whisperers extends WarriorFaction {
    val clashKey = WG

    val warrior = GuildWhisperer

    def abilities(options : $[Meta.O]) = $(HiddenInsightEffect, DeeplyRooted, MidnightMoves)

    def pieces(options : $[Meta.O]) = GuildWhisperer *** 12 ++ WGLair *** 5

    override def note : Elem = SpacedDash ~ "by " ~ "slugfacekillah".hl

    def advertising = 3.times(Image(this.style + "-insight", styles.piece)).merge

    def motto = "Insight".styled(this)

    override def initialDamagePriorities(options : $[Meta.O]) = $(GuildWhisperer, WGLair)
}

case object WG extends Whisperers {
    val name = "Whisperers Guild"
    val short = "WG"
    val style = "wg"
    val priority = "Z"
}

object Whisperers {
    val kinds : $[InsightKind] = $(InsightBuild, InsightCraft(Fox), InsightCraft(Rabbit), InsightCraft(Mouse), InsightMartyr, InsightCarnage)

    def total(k : InsightKind) = k match {
        case InsightBuild => 5
        case _ => 3
    }

    def guild(implicit game : Game) : |[Whisperers] = factions.of[Whisperers].%(game.states.contains).single
}

case object WGSlinkMsg extends Message {
    def elem(implicit game : Game) = "with " ~ "Slink".hh
}

case object WGStrikeMsg extends Message {
    def elem(implicit game : Game) = "with " ~ "Strike".hh
}

case class WGInterrogateMsg(e : Faction) extends Message {
    def elem(implicit game : Game) = MidnightInterrogate.elem ~ " " ~ e.elem
}

case object WGInstigateMsg extends Message {
    def elem(implicit game : Game) = "instigated by the " ~ WG.elem
}

case class WGStartingForestAction(self : Whisperers, r : Forest) extends BaseAction(self, "starts in")(implicit g => board.forestName(r))
case class WGLairSetupStartAction(self : Whisperers) extends ForcedAction
case class WGLairSetupAction(self : Whisperers, e : Faction, c : Clearing) extends BaseAction("Put a", WGLair.of(self), "under", e, "in")(c)
case class WGFavorSetupAction(self : Whisperers, d : DeckCard) extends BaseAction("Tuck a card as the", "Favored Suit".styled(self))(d.img) with ViewCard

// Insight
case class WGInsightInfoAction(self : Whisperers, k : InsightKind, c : Clearing) extends BaseInfo(Break ~ "Hidden Insight")(Image("wg-insight-" + k.id, styles.token), k, "in", c) with Info
case class WGSalvageInfoAction(self : Whisperers, s : BaseSuit) extends BaseInfo(Break ~ "Salvage")(Image("wg-insight-craft-" + s.name.toLowerCase, styles.token), s) with Info
case class WGOfferAction(self : Whisperers, k : InsightKind, c : Clearing, e : Faction, then : ForcedAction) extends BaseAction("Reveal", "Insight".styled(self), "in", c)(k, "(" ~ e.elem ~ ")")
case class WGPassAction(self : Whisperers, then : ForcedAction) extends BaseAction(None)("Keep hidden")
case class WGMartyrTakeAction(self : Whisperers, e : Faction, d : DeckCard, then : ForcedAction) extends BaseAction("Martyr".hl, "take a card from", e)(d.img) with ViewCard
case class WGCarnagePlaceAction(self : Whisperers, c : Clearing, from : Region, then : ForcedAction) extends BaseAction("Carnage".hl, "place a", GuildWhisperer.of(self), "in", c, "from")(from)

// Midnight
case class WGMidnightAction(self : Whisperers, k : MidnightKind, then : ForcedAction) extends BaseAction(MidnightMoves.of(self))(k) with Soft
case class WGMidnightSkipAction(self : Whisperers, then : ForcedAction) extends BaseAction(None)("Skip", "Midnight".styled(self))
case class WGAssassinateAction(self : Whisperers, c : Clearing, e : WarriorFaction, then : ForcedAction) extends BaseAction(MidnightAssassinate.elem)(e.warrior.of(e), e.warrior.imgd(e), "in", c)
case class WGInstigateClearingAction(self : Whisperers, c : Clearing, then : ForcedAction) extends BaseAction(MidnightInstigate.elem, "in")(c) with Soft
case class WGInstigateAction(self : Whisperers, c : Clearing, a : Faction, d : Faction, then : ForcedAction) extends BaseAction(MidnightInstigate.elem, "in", c)(a, "battles", d)
case class WGInstigatedAction(self : Whisperers, then : ForcedAction) extends ForcedAction
case class WGInfiltrateClearingAction(self : Whisperers, c : Clearing, then : ForcedAction) extends BaseAction(MidnightInfiltrate.elem, "from")(c) with Soft
case class WGInfiltrateAction(self : Whisperers, c : Clearing, e : WarriorFaction, to : Clearing, then : ForcedAction) extends BaseAction(MidnightInfiltrate.elem, "from", c)(e, "to", to)
case class WGInterrogateAction(self : Whisperers, e : Faction, then : ForcedAction) extends BaseAction(MidnightInterrogate.elem)(e)
case class WGManipulateAction(self : Whisperers, rest : $[DeckCard], top : $[DeckCard], then : ForcedAction) extends ForcedAction
case class WGManipulatePickAction(self : Whisperers, d : DeckCard, rest : $[DeckCard], top : $[DeckCard], then : ForcedAction) extends BaseAction(MidnightManipulate.elem, "next card on top")(d.img) with ViewCard

// Birdsong
case class WGCraftAction(self : Whisperers) extends BaseAction("Craft".styled(self))("Craft".styled(self), "with", "Salvage".styled(self)) with Soft
case class WGDistributeAction(self : Whisperers, l : $[Faction]) extends ForcedAction
case class WGGiveAction(self : Whisperers, e : Faction, d : DeckCard, l : $[Faction]) extends BaseAction("Give a card to", e)(d.img) with ViewCard
case class WGFavorAction(self : Whisperers, d : DeckCard) extends BaseAction("Choose the", "Favored Suit".styled(self))(d.img) with ViewCard

// Daylight
case class WGRegroupRemoveAction(self : Whisperers, c : Clearing) extends BaseAction("Regroup".styled(self), "remove a face-down", "Insight".styled(self), "instead of placing a warrior")(c)
case class WGRegroupAction(self : Whisperers) extends BaseAction("Regroup".styled(self))("Place the", "Favored".styled(self), "warriors")
case class WGSlinkNextAction(self : Whisperers, n : Int) extends ForcedAction
case class WGSlinkFromAction(self : Whisperers, from : Region, n : Int) extends BaseAction("Slink".styled(self), "(" ~ n.hl ~ " left)", "from")(implicit g => from @@ { case r : Forest => board.forestName(r).elem ; case c => c.elem }) with Soft
case class WGSlinkToAction(self : Whisperers, from : Region, to : Region, k : Int, n : Int) extends BaseAction("Slink".styled(self), "from", implicit g => from @@ { case r : Forest => board.forestName(r).elem ; case c => c.elem }, "to", implicit g => to @@ { case r : Forest => board.forestName(r).elem ; case c => c.elem })(k.of(GuildWhisperer.name).styled(self))

// Evening
case class WGScriptAction(self : Whisperers, r : Forest) extends BaseAction("Script the Story".styled(self), "removing a warrior from")(implicit g => board.forestName(r))
case class WGScriptNextAction(self : Whisperers, r : Forest, l : $[Clearing]) extends ForcedAction
case class WGScriptPlaceAction(self : Whisperers, r : Forest, c : Clearing, k : InsightKind, l : $[Clearing]) extends BaseAction("Place", "Insight".styled(self), "face down in", c)(k)
case class WGStrikeBattleAction(self : Whisperers, l : $[Clearing]) extends BaseAction("Strike".styled(self))("Battle".styled(self), dt.Battle) with Soft
case class WGStruckAction(self : Whisperers) extends ForcedAction


class WhisperersPlayer(val faction : Whisperers)(implicit val game : Game) extends FactionState {
    board.forests.foreach(r => location(r))

    val favoredBox = location(WGFavored, u => u.faction == faction && u.piece == GuildWhisperer)

    val favored = cards("favored")

    // the face-down Insight markers, and what each one is (only the Guild knows)
    var insight : Map[Clearing, InsightKind] = Map()
    var salvage : $[BaseSuit] = $

    // Insight that was set off and can be revealed now
    var offers : $[(InsightKind, Clearing, Faction)] = $
    var removals : $[(InsightKind, Clearing, Faction)] = $
    var handled : |[Action] = None

    // what was on the map when Insight was last looked at
    var buildingsSeen : Map[(Faction, Clearing), Int] = Map()
    var craftSeen : Map[Faction, Int] = Map()

    var midnightTurn = -1
    var instigated : |[Faction] = None
    var struck = false

    def craft = salvage./(s => s : SuitAsset)

    def supply(k : InsightKind) = Whisperers.total(k) - insight.values.$.count(k) - (k @@ {
        case InsightBuild => all(WGLair).num
        case InsightCraft(s) => salvage.count(s)
        case _ => 0
    })

    def forests = board.forests.%(r => at(r).has(GuildWhisperer))

    def inForests = board.forests./(r => at(r).count(GuildWhisperer)).sum

    def limit = 5 + clearings.%(c => this.faction.rules(c) || at(c).has(WGLair)).num
}


object WhisperersExpansion extends FactionExpansion[Whisperers] {
    // the face-down Insight markers drawn on the map, the same for everyone
    def markers(r : Region)(implicit game : Game) : $[Figure] = r match {
        case c : Clearing => Whisperers.guild.%(_.insight.contains(c))./(_ => Figure(Neutral, HiddenInsight, 1)).$
        case _ => $
    }

    def adjacent(r : Region)(implicit game : Game) : $[Region] = r match {
        case c : Clearing => game.connected(c) ++ board.forests.%(f => game.fromForest(f).has(c))
        case f : Forest => game.fromForest(f) ++ board.forests.but(f).%(o => board.forestsConnected(f, o))
        case _ => $
    }

    // a card of the Favored Suit leaving an enemy's hand to the discard pile
    def discarded(source : Location[Card], d : Card)(implicit game : Game) {
        Whisperers.guild.foreach { f =>
            if (f.favored.any && factions.but(f).exists(e => e.hand == source) && d.as[DeckCard].exists(_.suit == f.favored.first.suit)) {
                if (f.pool(GuildWhisperer)) {
                    f.reserve --> GuildWhisperer --> f.favoredBox

                    f.log("gained a", "Favored".styled(f), "warrior from a", d.as[DeckCard].get.suit, "card")
                }
            }
        }
    }

    // looks at what happened since the last time and lists the Insight it set off
    def watch(f : Whisperers)(implicit game : Game) {
        val buildings = factions.but(f)./~(e => clearings./(c => (e, c) -> e.at(c).of[Building].num)).toMap

        val built = f.insight.filter(_._2 == InsightBuild).keys.$./~(c => factions.but(f).%(e => buildings((e, c)) > f.buildingsSeen.get((e, c)).|(0))./(e => (InsightBuild : InsightKind, c, e)))

        f.buildingsSeen = buildings

        val crafted = factions.but(f)./~{ e =>
            val seen = f.craftSeen.get(e).|(0)
            val now = e.crafted.num
            val used = (now > seen).??(e.crafted.drop(seen))

            f.craftSeen += e -> now

            f.insight.filter(_._2.is[InsightCraft]).keys.$.%(c => e.present(c) && used.has(c.asset))./(c => (f.insight(c), c, e))
        }

        val l = (built ++ crafted ++ f.removals).%(o => f.insight.get(o._2).has(o._1))

        f.removals = $

        if (game.current != f)
            f.offers = (f.offers ++ l).distinct.%(o => f.insight.get(o._2).has(o._1))
    }

    def offerAsk(f : Whisperers, then : ForcedAction)(implicit game : Game) : Continue =
        Ask(f)
            .group(HiddenInsightEffect.of(f))
            .each(f.offers)(o => WGOfferAction(f, o._1, o._2, o._3, then))
            .add(WGPassAction(f, then))

    def midnightChoices(f : Whisperers, then : ForcedAction)(implicit game : Game) : $[UserAction] = {
        val enemies = factions.but(f)
        val warriors = clearings.%(c => enemies.of[WarriorFaction].exists(e => e.at(c).has(e.warrior)))
        val instigate = clearings.%(c => f.at(c).has(GuildWhisperer)).%(c => enemies.exists(a => a.at(c).of[Warrior].any && enemies.but(a).exists(d => d.present(c))))
        val infiltrate = clearings.%(c => f.at(c).has(GuildWhisperer)).%(c => enemies.of[WarriorFaction].exists(e => e.at(c).has(e.warrior)) && game.connected(c).any)

        $(
            WGMidnightAction(f, MidnightAssassinate, then).!(warriors.none),
            WGMidnightAction(f, MidnightInstigate, then).!(instigate.none),
            WGMidnightAction(f, MidnightInfiltrate, then).!(infiltrate.none),
            WGMidnightAction(f, MidnightInterrogate, then).!(enemies.%(_.hand.any).none),
            WGMidnightAction(f, MidnightManipulate, then).!(deck.none)
        )
    }

    def perform(action : Action, soft : Void)(implicit game : Game) = action @@ {
        // SETUP
        case CreatePlayerAction(f : Whisperers) =>
            game.states += f -> new WhisperersPlayer(f)

            FactionInitAction(f)

        case FactionSetupAction(f : Whisperers) =>
            Ask(f).each(board.forests)(r => WGStartingForestAction(f, r)).needOk

        case WGStartingForestAction(f, r) =>
            val n = factions.num min f.pooled(GuildWhisperer)

            f.reserve --> n.times(GuildWhisperer) --> r

            f.log("placed", n.of(GuildWhisperer.name).styled(f), "in", board.forestName(r))

            DrawCardsAction(f, 3, NoMessage, AddCardsAction(f, WGLairSetupStartAction(f)))

        case WGLairSetupStartAction(f) =>
            val e = factions.but(f).%(e => clearings.exists(c => e.at(c).of[Building].any)).starting

            e match {
                case Some(e) => Ask(f).each(clearings.%(c => e.at(c).of[Building].any))(c => WGLairSetupAction(f, e, c)).needOk
                case None =>
                    val r = f.forests.starting
                    r.foreach { r =>
                        val n = factions.num min f.pooled(GuildWhisperer)
                        f.reserve --> n.times(GuildWhisperer) --> r
                        f.log("placed", n.of(GuildWhisperer.name).styled(f), "in", board.forestName(r))
                    }
                    Ask(f).each(f.hand.get)(d => WGFavorSetupAction(f, d)).needOk
            }

        case WGLairSetupAction(f, e, c) =>
            f.reserve --> WGLair --> c

            f.log("put a", WGLair.of(f), "under", e, "in", c)

            Ask(f).each(f.hand.get)(d => WGFavorSetupAction(f, d)).needOk

        case WGFavorSetupAction(f, d) =>
            f.hand --> d --> f.favored

            f.log("tucked", d, "as the", "Favored Suit".styled(f))

            SetupFactionsAction

        // INSIGHT: set off on other players' turns
        case ForcedRemoveTargetEffectAction(e, c, t, p, then) if Whisperers.guild.exists(f => f.handled.forall(_ ne action) && game.current != f && e != f && f.instigated.none && f.insight.contains(c)) =>
            val f = Whisperers.guild.get

            f.handled = Some(action)

            if (t == f && p == GuildWhisperer && f.insight(c) == InsightMartyr)
                f.removals :+= ((InsightMartyr, c, e))

            if (t != f && t != e && f.insight(c) == InsightCarnage)
                f.removals :+= ((InsightCarnage, c, e))

            TryAgain

        case a : TopLevelAction if Whisperers.guild.exists { f => watch(f) ; f.offers.any } =>
            offerAsk(Whisperers.guild.get, a)

        case StartPlayerTurnAction(o) if game.turn > 0 && Whisperers.guild.exists { f => watch(f) ; f.offers.any } =>
            offerAsk(Whisperers.guild.get, action.as[ForcedAction].get)

        case StartPlayerTurnAction(o) if game.turn > 0 && Whisperers.guild.exists(f => o != f && game.current != f && f.midnightTurn != game.turn) =>
            val f = Whisperers.guild.get

            f.midnightTurn = game.turn

            Ask(f)
                .group("Midnight".styled(f))
                .add(midnightChoices(f, StartPlayerTurnAction(o)))
                .add(WGMidnightSkipAction(f, StartPlayerTurnAction(o)))

        case WGPassAction(f, then) =>
            f.offers = $

            then

        case WGOfferAction(f, k, c, e, then) =>
            f.offers = f.offers.%(_._2 != c)
            f.insight -= c

            f.log("revealed", k, "Insight".styled(f), "in", c)

            f.oscore(1)("revealing", "Insight".styled(f))

            k match {
                case InsightBuild =>
                    if (f.pool(WGLair)) {
                        f.reserve --> WGLair --> c

                        f.log("put a", WGLair.of(f), "under", e, "in", c)
                    }

                    then

                case InsightCraft(s) =>
                    f.salvage :+= s

                    f.log("took", s, "Salvage".styled(f))

                    then

                case InsightMartyr =>
                    val l = e.hand.%(_.matches(c.cost))

                    if (l.any)
                        Ask(f).each(l)(d => WGMartyrTakeAction(f, e, d, then)).skip(then)
                    else {
                        f.log("found no matching card in", e, "hand")

                        then
                    }

                case InsightCarnage =>
                    val from = f.pool(GuildWhisperer).?(Pool : Region).$ ++ f.favoredBox.any.?(WGFavored : Region).$

                    if (from.any)
                        Ask(f).each(from)(r => WGCarnagePlaceAction(f, c, r, then)).skip(then)
                    else
                        then
            }

        case WGMartyrTakeAction(f, e, d, then) =>
            e.hand --> d --> f.hand

            f.log("took a card from", e)

            then

        case WGCarnagePlaceAction(f, c, from, then) =>
            if (from == WGFavored)
                f.favoredBox --> GuildWhisperer --> c
            else
                f.reserve --> GuildWhisperer --> c

            f.log("placed", GuildWhisperer.of(f), "in", c)

            then

        // INSTIGATE: the points are the Guild's
        case ForcedRemoveScoreAction(e, c, t, p, n, m, then) if Whisperers.guild.exists(f => f.instigated.has(e)) =>
            ForcedRemoveScoreAction(Whisperers.guild.get, c, t, p, n, m, then)

        // MIDNIGHT
        case WGMidnightSkipAction(f, then) =>
            then

        case WGMidnightAction(f, MidnightAssassinate, then) =>
            val l = clearings./~(c => factions.but(f).of[WarriorFaction].%(e => e.at(c).has(e.warrior))./(e => (c, e)))

            Ask(f).each(l)((c, e) => WGAssassinateAction(f, c, e, then)).cancel

        case WGAssassinateAction(f, c, e, then) =>
            f.log("assassinated", e.warrior.of(e), "in", c)

            ForcedRemoveAction(f, c, e, e.warrior, 0, Removing, ForcedRemoveFinishedAction(f, then))

        case WGMidnightAction(f, MidnightInstigate, then) =>
            val enemies = factions.but(f)
            val l = clearings.%(c => f.at(c).has(GuildWhisperer)).%(c => enemies.exists(a => a.at(c).of[Warrior].any && enemies.but(a).exists(d => d.present(c))))

            Ask(f).each(l)(c => WGInstigateClearingAction(f, c, then)).cancel

        case WGInstigateClearingAction(f, c, then) =>
            val enemies = factions.but(f)

            Ask(f).some(enemies.%(a => a.at(c).of[Warrior].any))(a => enemies.but(a).%(d => d.present(c))./(d => WGInstigateAction(f, c, a, d, then))).cancel

        case WGInstigateAction(f, c, a, d, then) =>
            f.instigated = Some(a)

            f.log("instigated", a, "to battle", d, "in", c)

            BattleStartAction(a, a, a, WGInstigateMsg, c, d, None, WGInstigatedAction(f, then))

        case WGInstigatedAction(f, then) =>
            f.instigated = None

            then

        case WGMidnightAction(f, MidnightInfiltrate, then) =>
            val l = clearings.%(c => f.at(c).has(GuildWhisperer)).%(c => factions.but(f).of[WarriorFaction].exists(e => e.at(c).has(e.warrior)) && game.connected(c).any)

            Ask(f).each(l)(c => WGInfiltrateClearingAction(f, c, then)).cancel

        case WGInfiltrateClearingAction(f, c, then) =>
            val l = factions.but(f).of[WarriorFaction].%(e => e.at(c).has(e.warrior))

            Ask(f).some(l)(e => game.connected(c)./(to => WGInfiltrateAction(f, c, e, to, then))).cancel

        case WGInfiltrateAction(f, c, e, to, then) =>
            f.from(c) --> GuildWhisperer --> f.reserve

            val n = (e.at(c).count(e.warrior) + 1) / 2

            e.from(c) --> n.times(e.warrior) --> to

            f.log("infiltrated", c, "and moved", n.of(e.warrior.name).styled(e), "to", to)

            then

        case WGMidnightAction(f, MidnightInterrogate, then) =>
            Ask(f).each(factions.but(f).%(_.hand.any))(e => WGInterrogateAction(f, e, then)).cancel

        case WGInterrogateAction(f, e, then) =>
            f.log("interrogated", e)

            PeekCardsAsk(f, WGInterrogateMsg(e), e.hand.get, then)

        case WGMidnightAction(f, MidnightManipulate, then) =>
            f.log("looked at the top", (deck.num min 5).hl, "cards of the deck")

            WGManipulateAction(f, deck.get.take(5), $, then)

        case WGManipulateAction(f, rest, top, then) if rest.none =>
            val others = deck.get.diff(top)

            deck --> top --> deck
            deck --> others --> deck

            then

        case WGManipulateAction(f, rest, top, then) =>
            Ask(f).each(rest)(d => WGManipulatePickAction(f, d, rest.but(d), top :+ d, then))

        case WGManipulatePickAction(f, d, rest, top, then) =>
            WGManipulateAction(f, rest, top, then)

        // BIRDSONG: CRAFT
        case BirdsongNAction(30, f : Whisperers) =>
            soft()

            val l = f.hand.%(f.craftable)

            if (f.salvage.none || l.none)
                NoAsk(f)(Next)
            else
                Ask(f)
                    .add(WGCraftAction(f))
                    .done(Next)
                    .birdsong(f)

        case WGCraftAction(f) =>
            YYSelectObjectsAction(f, f.hand)
                .withGroup("Craft".styled(f) ~ " with " ~ f.craft.diff(f.crafted)./(dt.CraftSuit).merge)
                .withRule(f.craftable)
                .withThen(d => CraftAction(f, d, NoMessage, CraftPerformAction(f, d, NoMessage)))(d => game.desc("Craft", d, "with", d.as[CraftCard]./~(_.cost./(dt.CraftSuit)).merge))("Craft")
                .withExtra($(NoHand, CancelAction))

        // BIRDSONG: DRAW AND DISTRIBUTE
        case BirdsongNAction(40, f : Whisperers) =>
            // the Salvage spent goes back to the supply
            f.crafted.foreach {
                case s : BaseSuit if f.salvage.has(s) => f.salvage :-= s
                case _ =>
            }

            f.crafted = $

            DrawCardsAction(f, factions.num, NoMessage, AddCardsAction(f, WGDistributeAction(f, game.seating./(game.ptf).%(factions.has).but(f))))

        case WGDistributeAction(f, Nil) =>
            Next

        case WGDistributeAction(f, e :: rest) =>
            if (f.hand.none)
                Next
            else
                Ask(f).each(f.hand.get)(d => WGGiveAction(f, e, d, rest))

        case WGGiveAction(f, e, d, rest) =>
            f.hand --> d --> e.hand

            f.log("gave a card to", e)

            WGDistributeAction(f, rest)

        // BIRDSONG: CHOOSE THE FAVORED
        case BirdsongNAction(50, f : Whisperers) =>
            if (f.hand.none)
                Next
            else
                Ask(f).each(f.hand.get)(d => WGFavorAction(f, d)).needOk

        case WGFavorAction(f, d) =>
            f.favored --> f.hand
            f.hand --> d --> f.favored

            f.log("chose", d.suit, "as the", "Favored Suit".styled(f))

            Next

        // DAYLIGHT: REGROUP
        case DaylightNAction(30, f : Whisperers) =>
            if (f.favoredBox.none)
                Next
            else
                Ask(f)
                    .group("Regroup".styled(f), "(" ~ f.favoredBox.num.hl ~ " " ~ "Favored".styled(f) ~ ")")
                    .each(f.insight.keys.$)(c => WGRegroupRemoveAction(f, c))
                    .add(WGRegroupAction(f))

        case WGRegroupRemoveAction(f, c) =>
            f.favoredBox --> GuildWhisperer --> f.reserve

            f.insight -= c

            f.log("removed the", "Insight".styled(f), "in", c)

            Repeat

        case WGRegroupAction(f) =>
            val forests = f.forests.some.|(board.forests)

            while (f.favoredBox.any) {
                val r = forests.minBy(r => f.at(r).count(GuildWhisperer))

                f.favoredBox --> GuildWhisperer --> r
            }

            f.log("regrouped in the forests")

            Next

        // DAYLIGHT: SLINK
        case DaylightNAction(40, f : Whisperers) =>
            WGSlinkNextAction(f, factions.num)

        case WGSlinkNextAction(f, n) =>
            val l = (clearings ++ board.forests).%(r => f.at(r).has(GuildWhisperer)).%(r => adjacent(r).any)

            if (n <= 0 || l.none)
                Next
            else
                Ask(f).each(l)(r => WGSlinkFromAction(f, r, n)).done(Next).daylight(f)

        case WGSlinkFromAction(f, from, n) =>
            val k = f.at(from).count(GuildWhisperer)

            Ask(f).some(adjacent(from))(to => 1.to(k)./(i => WGSlinkToAction(f, from, to, i, n))).cancel

        case WGSlinkToAction(f, from, to, k, n) =>
            f.from(from) --> k.times(GuildWhisperer) --> to

            f.log("slinked", k.of(GuildWhisperer.name).styled(f), "from", from @@ { case r : Forest => board.forestName(r).hl ; case c => c.elem(game) }, "to", to @@ { case r : Forest => board.forestName(r).hl ; case c => c.elem(game) })

            WGSlinkNextAction(f, n - 1)

        // EVENING: SCRIPT THE STORY
        case EveningNAction(30, f : Whisperers) =>
            val l = f.forests.%(r => game.fromForest(r).exists(c => f.insight.contains(c).not)).%(_ => Whisperers.kinds.exists(f.supply(_) > 0))

            if (l.none)
                Next
            else
                Ask(f).group("Script the Story".styled(f)).each(l)(r => WGScriptAction(f, r)).done(Next).evening(f)

        case WGScriptAction(f, r) =>
            f.from(r) --> GuildWhisperer --> f.reserve

            WGScriptNextAction(f, r, game.fromForest(r).%(c => f.insight.contains(c).not))

        case WGScriptNextAction(f, r, l) =>
            val kinds = Whisperers.kinds.%(f.supply(_) > 0)

            if (l.none || kinds.none)
                Repeat
            else
                Ask(f).some(l)(c => kinds./(k => WGScriptPlaceAction(f, r, c, k, l.but(c)))).done(Repeat)

        case WGScriptPlaceAction(f, r, c, k, l) =>
            f.insight += c -> k

            f.log("placed", "Insight".styled(f), "face down in", c)

            WGScriptNextAction(f, r, l)

        // EVENING: DISCARD AND STRIKE
        case NightStartAction(f : Whisperers) =>
            EveningHandLimitAction(f)

        case EveningHandLimitAction(f : Whisperers) =>
            HandLimitAction(f, f.limit, EveningAfterHandLimitAction(f))

        case EveningAfterHandLimitAction(f : Whisperers) if f.struck.not =>
            val l = clearings.%(f.canAttackIn)

            val ask = Ask(f)
                .group("Strike".styled(f))
                .add(WGStrikeBattleAction(f, l).!(l.none))
                .add(midnightChoices(f, WGStruckAction(f)))

            if (ask.available)
                ask
            else
                WGStruckAction(f)

        case WGStrikeBattleAction(f, l) =>
            BattleInitAction(f, f, WGStrikeMsg, l, $(CancelAction), WGStruckAction(f))

        case WGStruckAction(f) =>
            f.struck = true

            EveningAfterHandLimitAction(f)

        case FactionCleanUpAction(f : Whisperers) =>
            f.struck = false
            f.crafted = $

            CleanUpAction(f)

        case _ => UnknownContinue
    }
}
