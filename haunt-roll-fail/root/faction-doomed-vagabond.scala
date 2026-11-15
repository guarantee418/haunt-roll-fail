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

// The Doomed Expansion's Doomed Vagabond (slugfacekillah, OGVB v1.67; Law of Doom DV and its faction board), a re-imagining of the
// Vagabond for 4 or more players: Despot Infamy (one point a battle for each Hostile faction it removes pieces of, in faction-hero.scala),
// deals with Hostile factions to make them Indifferent again, quests freshened at the end of each Evening, and five bird "Collect
// Bounty" quests shuffled into the quest deck (and five quests out unseen) that the other factions put their crafted items on, with
// another enemy's relationship marker, for the Vagabond to take by removing that enemy's warriors in battle. Everything else is the
// Vagabond's (faction-hero.scala), the characters too.
//
// Readings (the rules leave these open):
// - Deals: a faction Hostile to the Doomed Vagabond and the Doomed Vagabond may offer each other peace on their own turns, once a
//   turn; if the other accepts, the faction is Indifferent again. What each gets for it is agreed between the players, not by the game
// - Collect Bounty: an enemy puts an item from its Crafted Items box, on its own turn, on an available Collect Bounty quest without one,
//   with the marker of a faction that is neither itself nor a Vagabond. The Doomed Vagabond takes the item (face up in its Satchel)
//   and the quest card when it removes a warrior of that faction in a battle, any battle; a bounty quest freshened away takes its
//   item out of the game. Bounty quests can't be completed the usual way
// - Scoring bounties: a completed fox, mouse or rabbit quest taken for points scores 1 more for each completed bounty quest, which are
//   then out of the game
// - Freshen Quests comes before the Evening draw (the order changes nothing)

case object DV extends Hero {
    val name = "Doomed Vagabond"
    val short = "DV"
    val style = "dv"
    val priority = "E'''"

    override def note : Elem = HorizontalBreak ~ "Doomed Expansion (fan-made)"
}

object DoomedVagabond {
    val bounties = 1.to(5)./(i => Quest(Bird, "Collect Bounty", "collect-bounty-" + i, Torch, Torch))

    def isBounty(q : Quest) = q.suit == Bird && q.id.startsWith("collect-bounty")
}

case class DVQuestsShuffledAction(shuffled : $[Quest], then : ForcedAction) extends ShuffledAction[Quest]

case class DVBountyMainAction(self : Faction, then : ForcedAction) extends BaseAction(None)("Collect Bounty".styled(DV), Image("item-any", styles.piece)) with Soft
case class DVBountyAction(self : Faction, q : Quest, i : ItemRef, e : Faction, then : ForcedAction) extends BaseAction("Put a bounty on", q.elem, "for", DV)(i.item.img, "on", e) with ViewQuest
case class DVBountyClaimAction(self : Hero, q : Quest, e : Faction, then : ForcedAction) extends BaseAction("Collect Bounty".styled(self), "for removing", e)("Take", game => game.bounties.get(q.id)./(_._1.img).|(Empty), "and the quest") with ViewQuest

case class DVPeaceOfferAction(self : Faction, e : Faction, then : ForcedAction) extends BaseAction(None)("Offer peace to", e, "(" ~ Hostile.elem ~ " to " ~ Indifferent.elem ~ ")")
case class DVPeaceAnswerAction(self : Faction, e : Faction, accept : Boolean, then : ForcedAction) extends BaseAction(e, "offers peace", "(" ~ Hostile.elem ~ " to " ~ Indifferent.elem ~ ")")(accept.?("Accept").|("Decline"))

case class DVBountyAskAction(self : Hero, q : Quest, e : Faction, then : ForcedAction) extends ForcedAction

object DoomedVagabondExpansion extends FactionExpansion[DV.type] {
    def bountiesFor(f : Faction)(implicit game : Game) : $[DVBountyAction] = {
        if (f.is[Hero] || f.is[Knaves] || f.forTrade.none)
            $
        else {
            val qq = game.quests.take(3).%(DoomedVagabond.isBounty).%(q => game.bounties.contains(q.id).not)
            val ee = factions.but(f).notOf[Hero].%(game.states.contains)

            qq./~(q => f.forTrade.distinct./~(i => ee./(e => DVBountyAction(f, q, i, e, null))))
        }
    }

    // the Collect Bounty and peace actions a faction may take on its own turn, in any menu
    def anytime(action : Action, c : Continue)(implicit game : Game) : Continue = (action, c) @@ {
        case (then : ForcedAction, ask @ Ask(f : Faction, l)) if factions.has(DV) && game.states.contains(DV) && then.isSoft && f == game.current && game.turn > 0 &&
            game.phase != Night && ask.available && l.exists(a => a.is[DVBountyMainAction] || a.is[DVPeaceOfferAction] || a.is[DVBountyAction] || a.is[DVPeaceAnswerAction]).not && l.exists(_.is[Cancel]).not &&
            (l.num == 1 && l.exists(_.is[WrapAction])).not && InvasiveEEEExpansion.aboutCard(then).not && InvasiveEEEExpansion.unsaved(then).not && InvasiveEEEExpansion.paying(then).not =>

            val b = bountiesFor(f).any.$(DVBountyMainAction(f, then))

            val p = (game.peaceOffered.get(f) != Some(game.turn)).??(
                if (f == DV)
                    factions.but(f).%(DV.hostile)./(e => DVPeaceOfferAction(f, e, then))
                else
                    DV.hostile(f).$(DVPeaceOfferAction(f, DV, then))
            )

            if (b.none && p.none)
                c
            else
                ask.group(DV).add(b).add(p)

        case _ => c
    }

    def perform(action : Action, soft : Void)(implicit game : Game) = action @@ {
        // SETUP: the bounty quests go into the quest deck, and five random quests out
        case StartingForestAction(f : DV.type, r) if game.quests.none =>
            f.pawn --> r

            f.log("started in", board.forestName(r))

            Shuffle[Quest](options.has(RevisedQuests).?(Quest.revised).|(Quest.default) ++ DoomedVagabond.bounties, DVQuestsShuffledAction(_, SetupFactionsAction))

        case DVQuestsShuffledAction(l, then) =>
            game.quests = l.dropRight(5)

            log("Five quests were returned to the box unseen")

            then

        // Collect Bounty
        case DVBountyMainAction(f, then) =>
            Ask(f).each(bountiesFor(f))(_.copy(then = then)).cancel

        case DVBountyAction(f, q, i, e, then) =>
            f.forTrade :-= i
            game.bounties += q.id -> (i.item, e)

            f.log("put", i.item, "on", q, "with the marker of", e)

            then

        case BattlePostHitOutAction(b, x, y, z, TryForcedRemoveAction(e : DV.type, c, f, p : Warrior, n, m, then, fail)) if then.is[DVBountyAskAction].not && game.quests.take(3).exists(q => game.bounties.get(q.id).exists(_._2 == f)) =>
            val q = game.quests.take(3).%(q => game.bounties.get(q.id).exists(_._2 == f)).first

            BattlePostHitOutAction(b, x, y, z, TryForcedRemoveAction(e, c, f, p, n, m, DVBountyAskAction(e, q, f, then), fail))

        case DVBountyAskAction(f, q, e, then) =>
            if (game.quests.has(q).not || game.bounties.contains(q.id).not)
                then
            else
                Ask(f).add(DVBountyClaimAction(f, q, e, then)).skip(then)

        case DVBountyClaimAction(f, q, e, then) =>
            val (i, _) = game.bounties(q.id)

            game.bounties -= q.id
            game.quests :-= q

            f.inv :+= i.pristine
            f.quests :+= Bird

            f.log("collected the bounty", q, "for removing", e, "and took", i)

            if (game.quests.num >= 3)
                log("New quest available", game.quests(2))

            then

        // bounty quests score with a quest taken for points
        case QuestRewardVPAction(f : DV.type, q, n) if f.quests.has(Bird) =>
            val k = f.quests.count(Bird)

            f.quests = f.quests.but(Bird)

            f.oscore(k)("for", k.hl, "Collect Bounty".styled(f), "quests")

            TryAgain

        // peace
        case DVPeaceOfferAction(f, e, then) =>
            game.peaceOffered += f -> game.turn

            f.log("offered peace to", e)

            Ask(e).add(DVPeaceAnswerAction(e, f, true, then)).add(DVPeaceAnswerAction(e, f, false, then)).needOk

        case DVPeaceAnswerAction(e, f, accept, then) =>
            if (accept) {
                val x = (f == DV).?(e).|(f)

                DV.attitude += x -> Indifferent

                e.log("accepted;", x, "became", Indifferent, "to", DV)
            }
            else
                e.log("declined")

            then

        // Freshen Quests
        case EveningNAction(95, f : DV.type) =>
            game.quests.starting.foreach { q =>
                game.quests = game.quests.drop(1)

                log("Quest", q, "was freshened away")

                game.bounties.get(q.id).foreach { case (i, _) =>
                    game.bounties -= q.id

                    log(i, "on it was removed from the game")
                }

                if (game.quests.num >= 3)
                    log("New quest available", game.quests(2))
            }

            Next

        case _ => UnknownContinue
    }
}
