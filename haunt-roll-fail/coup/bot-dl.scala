package coup
//
//
//
//
import hrf.colmat._
import hrf.compute._
import hrf.logger._
//
//
//
//

class BotDL(self : Faction, vengefulness : Double, honesty : Double, skepticism : Double) extends Bot {
    def ask(aaa : $[UserAction], deviation : Double = 0.0)(implicit game : G) : Compute[UserAction] = {
        if (aaa.none)
            throw new Error("empty actions ***")

        val aa = try {
            game.explode(aaa, false, None).notOf[Hidden]
        }
        catch {
            case e : Exception =>
                error(e)

                warn("error on explode")

                aaa.foreach(a => +++(a))

                ---
                ---
                ---
                ---

                aaa.foreach {
                    case a : Cancel => Nil
                    case a : Info => Nil
                    case a : Soft =>
                        +++("soft", a)
                        game.explode($(a), false, None)
                    case _ =>
                }

                Nil
        }

        if (aa.none) {
            var l : $[String] = $
            l :+= ""
            l :+= ""
            l :+= ""
            aaa.foreach { a =>
                l :+= a.toString
                l :+= ""
                l :+= (game.explode($(a), false, None)).toString
                l :+= ""
                l :+= (game.explode($(a), false, None).notOf[Hidden]).toString
            }
            l :+= ""
            l :+= ""
            l :+= ""
            throw new Error("empty actions !!!\n" + aaa./(_.toString).mkString("\n") + l.mkString("\n"))
        }

        val actions = aa


        import game._

        val enemies = factions.but(self)

        def see(r : Role) = setup./(_.lost.%(_.role == r).num).sum + self.hand.%(_.role == r).num

        def have(r : Role) = self.hand.exists(_.role == r)

        def double(r : Role) = self.hand.%(_.role == r).num == 2

        def getProbabilityOfHiddenCardBeingInfluence(r : Role) : Double = (3 - see(r)) /:/ (enemies./(_.hand.num).sum + deck.num)

        def getProbabilityOfPlayersInfluence(r : Role) = enemies./(_.hand.num).sum * getProbabilityOfHiddenCardBeingInfluence(r)

        implicit class FactionEx(f : Faction) {
            def getProbabilityOfPlayerInfluence(r : Role) : Double = f.hand.num * getProbabilityOfHiddenCardBeingInfluence(r)
            def getPlayerDangerFactor : Int = f.hand.num * 10 + f.money
            def grudge : Double = self.grudges(f)
            def claimedInfluences : $[Role] = f.claimed
        }

        def decideCoupTarget() = enemies.maxBy(e => e.getPlayerDangerFactor + e.grudge * vengefulness * 2 + Math.random() * 3)

        def decideAssasinationTarget() = enemies.maxBy(e => e.getPlayerDangerFactor + e.grudge * vengefulness * 2 - e.claimedInfluences.has(Contessa).?(10 + 10 * (1 - skepticism)).|(0.0) + Math.random() * 3)

        def checkEndGameAction() : |[UserAction] = {
            if (enemies.num > 1)
                return None

            val enemy = enemies.only

            if (enemy.hand.num == 1 && self.money >= 7)
                return actions.of[CoupWhomAction].single

            if (self.hand.num == 1 && enemy.money >= 7) {
                val assassinate = actions.of[KillWhomAction].single
                val steal = actions.of[StealFromAction].single

                if (self.money < 3)
                    return steal

                if (enemy.money >= 9)
                    return assassinate

                val chanceOfAssassin = self.getProbabilityOfPlayerInfluence(Assassin)
                val chanceOfCaptain = self.getProbabilityOfPlayerInfluence(Captain)

                return (chanceOfAssassin + random() * 0.1 > chanceOfCaptain + random() * 0.1).?(assassinate).|(steal)
            }

            return None
        }

        def checkEndGameBlockResponse() : |[UserAction] = {
            if (enemies.num > 1)
                return None

            if (self.hand.num > 1)
                return None

            val enemy = enemies.only

            if (enemy.money >= 7)
                return actions.of[ChallengeAction].single

            return None
        }

        // deciceAction
        if (actions.of[IncomeAction].any || actions.of[CoupWhomAction].any) {
            checkEndGameAction().foreach { a =>
                return a
            }

            var willCoup = false
            if (self.money >= 10)
                willCoup = true
            else
            if (self.money >= 7)
                willCoup = random() > 0.5

            if (willCoup) {
                val targetPlayer = decideCoupTarget()
                return actions.of[CoupWhomAction].%(_.f == targetPlayer).only
            }

            val bluffMargin = pow(1 - honesty, 1.5) * 0.5

            val selfEffectiveInfluences = self.hand ++ self.claimedInfluences

            if ((random() > 0.05 && selfEffectiveInfluences.has(Duke))
             || (random() < bluffMargin && getProbabilityOfPlayersInfluence(Duke) > 0))
                return actions.of[TaxAction].only

            if ((random() > 0.05 && selfEffectiveInfluences.has(Captain))
             || (random() < bluffMargin && getProbabilityOfPlayersInfluence(Captain) > 0)) {
                def getProbabilityOfBlockingSteal(e : Faction) = e.getProbabilityOfPlayerInfluence(Captain) + e.getProbabilityOfPlayerInfluence(Ambassador)

                val possibleTargets = enemies.%(_.money > 0)

                var minBlockingAbility = Double.PositiveInfinity

                var bestTargets : $[Faction] = $

                possibleTargets.foreach { possibleTarget =>
                    val blockingAbility =
                        (possibleTarget.claimedInfluences.has(Captain).?(0.5 * (1.5 - skepticism)).|(0)) +
                        (possibleTarget.claimedInfluences.has(Ambassador).?(0.5 * (1.5 - skepticism)).|(0)) +
                        getProbabilityOfBlockingSteal(possibleTarget)

                    if (blockingAbility < minBlockingAbility) {
                        minBlockingAbility = blockingAbility
                        bestTargets = $
                    }

                    if (blockingAbility <= minBlockingAbility) {
                        bestTargets :+= possibleTarget
                    }
                }

                if (bestTargets.any && minBlockingAbility < 0.99) {
                    val chosenTarget = bestTargets.shuffle(0)

                    // println(actions)

                    return actions.of[StealFromAction].%(_.f == chosenTarget).only
                }
            }

            if ((random() > 0.05 && selfEffectiveInfluences.has(Ambassador))
             || (random() < bluffMargin && getProbabilityOfPlayersInfluence(Ambassador) > 0))
                return actions.of[ExchangeAction].only

            if (((random() > 0.05 && selfEffectiveInfluences.has(Assassin))
             || (random() < bluffMargin && getProbabilityOfPlayersInfluence(Assassin) > 0))
             && self.money >= 3) {
                val targetPlayer = decideAssasinationTarget()
                return actions.of[KillWhomAction].%(_.f == targetPlayer).only
            }

            val claimedDukeCount = factions.%(_.claimedInfluences.has(Duke)).num
            if (claimedDukeCount * (0.35 - skepticism * 0.35) + getProbabilityOfPlayersInfluence(Duke) < 0.25 + Math.random() * 0.1)
                return actions.of[ForeignAidAction].only

            return actions.of[IncomeAction].only
        }

        // decideActionResponse | challenge
        if (actions.of[ChallengeAction].any) {
            val action = actions.of[ChallengeAction].only
            val then : Action = action.then

            val isSelfTargetKill = then.as[KillBlockAction].exists(_.self == self)
            val isSelfTargetSteal = then.as[StealBlockAction].exists(_.self == self)
            val isSelfActionKill = then.as[BlockSuccessAction].exists(_.f == self)
            val isSelfActionSteal = then.as[StealBlockedAction].exists(_.f == self)
            val isSelfTarget = isSelfTargetKill || isSelfTargetSteal
            val isSelfAction = isSelfActionKill || isSelfActionSteal
            val isSelfConcerned = isSelfTarget || isSelfAction

            if (isSelfTargetKill && self.hand.num == 1) {
                val probabilityOfAssassin = then.as[KillBlockAction].get.f.getProbabilityOfPlayerInfluence(Assassin)
                val probabilityOfContessa = self.getProbabilityOfPlayerInfluence(Contessa)

                if (probabilityOfAssassin == 0 || probabilityOfContessa == 0
                 || probabilityOfAssassin < 0.4 + random() * 0.2)
                    return action
            }

            checkEndGameBlockResponse().foreach { a => return a }

            val skepticismMargin = pow(skepticism, 2) * (isSelfConcerned.?(0.8).|(0.4) + random() * 0.1)

            if (action.f.getProbabilityOfPlayerInfluence(action.r) <= skepticismMargin
             && (action.f.claimedInfluences.has(action.r).not || random() < skepticismMargin))
                return action

            return actions.of[PassAction].only
        }

        // decideActionResponse | block
        {
            if (actions.of[BlockAction].any) {
                val bluffMargin = pow(1 - honesty, 1.5) * (false.?(0.4).|(0.2) + Math.random() * 0.1)
                val randomForBlockBluff = random()

                if (have(Duke) || (randomForBlockBluff < bluffMargin && getProbabilityOfPlayersInfluence(Duke) > 0))
                    return actions.of[BlockAction].only
            }

            if (actions.of[StealBlockWithAction].any) {
                val bluffMargin = pow(1 - honesty, 1.5) * (true.?(0.4).|(0.2) + Math.random() * 0.1)
                val randomForBlockBluff = random()

                $(Captain, Ambassador).foreach { r =>
                    if (have(r) || (randomForBlockBluff < bluffMargin && getProbabilityOfPlayersInfluence(r) > 0))
                        return actions.of[StealBlockWithAction].%(_.r == r).only
                }
            }

            if (actions.of[KillBlockWithAction].any) {
                val bluffMargin = pow(1 - honesty, 1.5) * (true.?(0.4).|(0.2) + Math.random() * 0.1)
                val randomForBlockBluff = random()

                $(Contessa).foreach { r =>
                    if (have(r) || (randomForBlockBluff < bluffMargin && getProbabilityOfPlayersInfluence(r) > 0))
                        return actions.of[KillBlockWithAction].only
                }
            }

            if (actions.of[KillBlockWithAction].any && self.hand.num == 1) {
                val probabilityOfAssassin = actions.of[KillBlockWithAction].only.f.getProbabilityOfPlayerInfluence(Assassin)
                val probabilityOfContessa = self.getProbabilityOfPlayerInfluence(Contessa)

                if (true /* probabilityOfAssassin > 0.4 + random() * 0.2 */)
                    return actions.of[KillBlockWithAction].only
            }
        }

        // decideActionChallengeResponse
        if (actions.of[RevealInfluenceAction].any) {
            val l = actions.of[RevealInfluenceAction]
            return l.%(a => a.r == a.c.role).some.|(l).shuffle(0)
        }

        // decideInfluencesToLose
        if (actions.of[RevealCardAction].any)
            return actions.of[RevealCardAction].shuffle(0)

        // throw new Error(actions.toString)

        return actions.shuffle(0)
    }
}
